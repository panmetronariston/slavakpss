# Общие функции: подготовка данных (analysis/prepare.R) и веб-приложение (app/app.R).
# Shiny подключает этот файл автоматически, т.к. он лежит в app/R/.

suppressPackageStartupMessages({
  library(stylo)
})

TARGET_LABEL <- "SlavaKPSS"

PERSONA_NAMES <- c(
  SlavaKPSS = "Слава КПСС",
  Gnoyny = "Гнойный",
  SonyaMarmeladova = "Соня Мармеладова",
  ValentinDyadka = "Валентин Дядька"
)

DISTANCES <- c(
  "Classic Delta (Burrows)" = "dist.delta",
  "Cosine Delta (Würzburg)" = "dist.wurzburg",
  "Eder's Delta" = "dist.eder",
  "Argamon's Delta" = "dist.argamon",
  "Cosine" = "dist.cosine",
  "MinMax" = "dist.minmax",
  "Manhattan" = "dist.manhattan",
  "Euclidean" = "dist.euclidean"
)

# Служебные слова — для вкладки «частотные слова» (в стилометрии их как раз НЕ убирают:
# именно частоты служебных слов несут «авторский почерк»).
RU_STOPWORDS <- c(
  "и", "в", "во", "не", "что", "он", "на", "я", "с", "со", "как", "а", "то", "все", "она",
  "так", "его", "но", "да", "ты", "к", "у", "же", "вы", "за", "бы", "по", "только", "ее",
  "мне", "было", "вот", "от", "меня", "еще", "нет", "о", "из", "ему", "теперь", "когда",
  "даже", "ну", "вдруг", "ли", "если", "уже", "или", "ни", "быть", "был", "него", "до",
  "вас", "нибудь", "опять", "уж", "вам", "ведь", "там", "потом", "себя", "ничего", "ей",
  "может", "они", "тут", "где", "есть", "надо", "ней", "для", "мы", "тебя", "их", "чем",
  "была", "сам", "чтоб", "без", "будто", "чего", "раз", "тоже", "себе", "под", "будет",
  "ж", "тогда", "кто", "этот", "того", "потому", "этого", "какой", "совсем", "ним",
  "здесь", "этом", "один", "почти", "мой", "тем", "чтобы", "нее", "сейчас", "были", "куда",
  "зачем", "всех", "никогда", "можно", "при", "наконец", "два", "об", "другой", "хоть",
  "после", "над", "больше", "тот", "через", "эти", "нас", "про", "всего", "них", "какая",
  "много", "разве", "три", "эту", "моя", "впрочем", "хорошо", "свою", "этой", "перед",
  "иногда", "лучше", "чуть", "том", "нельзя", "такой", "им", "более", "всегда", "конечно",
  "всю", "между", "это", "эта", "мои", "твой", "твои", "наш", "свой", "своих", "лишь",
  "тебе", "мной", "собой", "всё", "ещё", "её", "те", "ко", "ой", "эй", "е", "йо", "a",
  "the", "i", "you", "yeah", "е-е", "оу", "ага", "ха", "эх", "ах", "о-о"
)

# ---- Текст -------------------------------------------------------------------

# Та же нормализация, что scraper/lyrics.py::clean_text: убираем [заголовки],
# (эдлибы), ё -> е.
clean_lyrics <- function(text, drop_parentheses = TRUE) {
  text <- paste(text, collapse = "\n")
  text <- gsub("\\[[^]]*\\]", " ", text, perl = TRUE)
  if (drop_parentheses) text <- gsub("\\([^()]*\\)", " ", text, perl = TRUE)
  text <- chartr("ёЁ", "еЕ", text)
  text
}

tokenize <- function(text) {
  toks <- stylo::txt.to.words.ext(clean_lyrics(text), corpus.lang = "Other", preserve.case = FALSE)
  toks[nzchar(toks)]
}

# Класс текста по имени файла stylo: 'SlavaKPSS_chunk01' -> 'SlavaKPSS'
text_class <- function(names) sub("_.*$", "", names)

# ---- Корпус ------------------------------------------------------------------

load_corpus_dir <- function(dir) {
  files <- list.files(dir, pattern = "\\.txt$")
  if (!length(files)) stop("Нет .txt файлов в ", dir)
  corpus <- stylo::load.corpus.and.parse(
    files = files, corpus.dir = dir, markup.type = "plain",
    corpus.lang = "Other", features = "w", ngram.size = 1,
    preserve.case = FALSE, encoding = "UTF-8"
  )
  # ё -> е на случай, если корпус собирали не нашим скриптом
  corpus[] <- lapply(corpus, function(x) chartr("ё", "е", x))
  corpus
}

# Признаки: слова или символьные n-граммы (последние лучше работают на коротких текстах)
corpus_features <- function(corpus, type = c("w", "c"), ngram = 1) {
  type <- match.arg(type)
  if (type == "w" && ngram == 1) return(corpus)
  out <- lapply(corpus, stylo::txt.to.features, features = type, ngram.size = ngram)
  class(out) <- class(corpus)
  out
}

# Таблица относительных частот (тексты x признаки) по n самым частотным признакам
# корпуса; culling — доля текстов, в которых слово должно встречаться (0..100).
freq_table <- function(corpus, mfw = 300, culling = 0, extra = NULL) {
  features <- stylo::make.frequency.list(corpus, head = max(mfw * 3, 2000))
  all_texts <- if (is.null(extra)) corpus else c(corpus, extra)
  tbl <- stylo::make.table.of.frequencies(all_texts, features = features, relative = TRUE)
  if (culling > 0) {
    # culling считаем только по корпусу, проверяемый текст не должен влиять
    keep <- colnames(stylo::perform.culling(tbl[names(corpus), , drop = FALSE], culling.level = culling))
    tbl <- tbl[, keep, drop = FALSE]
  }
  tbl[, seq_len(min(mfw, ncol(tbl))), drop = FALSE]
}

dist_matrix <- function(tbl, measure = "dist.delta") {
  x <- as.matrix(tbl)
  d <- switch(measure,
    dist.manhattan = stats::dist(x, method = "manhattan"),
    dist.euclidean = stats::dist(x, method = "euclidean"),
    get(measure, envir = asNamespace("stylo"))(x)
  )
  as.matrix(d)
}

# Частоты слов (на 1000 слов) для группы текстов
word_freqs <- function(corpus, top = 50, drop_stop = FALSE) {
  toks <- unlist(corpus, use.names = FALSE)
  if (drop_stop) toks <- toks[!toks %in% RU_STOPWORDS]
  tab <- sort(table(toks), decreasing = TRUE)
  tab <- head(tab, top)
  data.frame(word = names(tab), n = as.integer(tab),
             per1000 = as.numeric(tab) / length(unlist(corpus)) * 1000,
             stringsAsFactors = FALSE)
}

# ---- Атрибуция ---------------------------------------------------------------

# Проверка одного текста против корпуса. Возвращает список с результатами трёх
# методов stylo: imposters (верификация), Delta (ближайшие тексты), SVM (классификация).
check_text <- function(corpus, test_tokens, mfw = 300, measure = "dist.delta",
                       iterations = 100, target = TARGET_LABEL) {
  test <- list(UNKNOWN_text = test_tokens)
  tbl <- freq_table(corpus, mfw = mfw, extra = test)
  ref <- tbl[names(corpus), , drop = FALSE]
  test_vec <- tbl["UNKNOWN_text", ]

  # 1. General Imposters (Koppel & Winter; реализация stylo::imposters):
  #    доля итераций, в которых текст ближе к кандидату, чем к «самозванцам».
  #    Каждый автор по очереди — кандидат, остальные — «самозванцы».
  classes <- text_class(rownames(ref))
  imp <- tryCatch({
    s <- vapply(unique(classes), function(cl) {
      as.numeric(quietly(stylo::imposters(
        reference.set = ref[classes != cl, , drop = FALSE],
        test = test_vec,
        candidate.set = ref[classes == cl, , drop = FALSE],
        iterations = iterations,
        distance = sub("^dist\\.", "", measure)
      )))[1]
    }, numeric(1))
    sort(s, decreasing = TRUE)
  }, error = function(e) structure(numeric(0), error = conditionMessage(e)))

  # 2. Delta: расстояние от текста до каждого текста корпуса, усреднённое по авторам
  d <- dist_matrix(tbl, measure)["UNKNOWN_text", names(corpus)]
  by_author <- sort(tapply(d, text_class(names(d)), mean))
  nearest <- head(sort(d), 10)

  # 3. SVM через stylo::classify
  svm <- tryCatch({
    res <- quietly(in_tempdir(stylo::classify(
      gui = FALSE, training.frequencies = ref,
      test.frequencies = tbl["UNKNOWN_text", , drop = FALSE],
      mfw.min = ncol(tbl), mfw.max = ncol(tbl), culling.min = 0, culling.max = 0,
      classification.method = "svm", use.existing.freq.tables = TRUE
    )))
    as.character(res$predicted)[1]
  }, error = function(e) NA_character_)

  list(imposters = imp, delta_by_author = by_author, nearest = nearest,
       svm = svm, table = tbl, n_tokens = length(test_tokens), target = target)
}

verdict <- function(score, p1 = 0.4, p2 = 0.6) {
  if (is.null(score) || is.na(score)) return(list(level = "unknown", text = "Не удалось посчитать"))
  if (score >= p2) list(level = "yes", text = "Стиль похож на Славу КПСС")
  else if (score <= p1) list(level = "no", text = "Стиль не похож на Славу КПСС")
  else list(level = "maybe", text = "Неопределённо: серая зона")
}

# ---- Утилиты -----------------------------------------------------------------

# stylo пишет служебные файлы (stylo_config.txt, final_results.txt) в рабочую папку
in_tempdir <- function(expr) {
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  force(expr)
}

quietly <- function(expr) {
  out <- NULL
  utils::capture.output(out <- suppressMessages(suppressWarnings(force(expr))))
  out
}
