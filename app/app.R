# Стилометрия Славы КПСС — веб-приложение (Shiny + stylo).
#
# Запуск локально из корня проекта:  shiny::runApp("app")
# Данные: app/data/corpus.rds (создаётся analysis/prepare.R).

library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(ape)
# helpers.R (stylo и общие функции) Shiny подгружает автоматически из app/R/

DATA_PATH <- "data/corpus.rds"
if (!file.exists(DATA_PATH)) {
  stop("Нет ", normalizePath("data", mustWork = FALSE), "/corpus.rds. ",
       "Соберите корпус: python -m scraper.build_corpus && Rscript analysis/prepare.R")
}
DATA <- readRDS(DATA_PATH)
CALIB <- if (!is.null(DATA$calibration)) DATA$calibration else c(p1 = 0.4, p2 = 0.6)

COL_TARGET <- "#2a78d6"
COL_TEST <- "#eb6834"
COL_OTHER <- "#8a8a85"
COL_INK <- "#5f5e5a"  # подписи «прочих» авторов на деревьях
COL_PERSONA <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100")

AUTHORS <- sort(unique(text_class(names(DATA$chunks))))
LEVELS <- c("Куски по ~1500 слов (рекомендуется)" = "chunks",
            "Отдельные песни" = "songs",
            "Альтер-эго Славы (песни)" = "personas")

class_label <- function(cl) {
  unname(ifelse(cl %in% names(PERSONA_NAMES), PERSONA_NAMES[cl],
                ifelse(cl == "UNKNOWN", "Проверяемый текст", cl)))
}

class_colors <- function(classes, level) {
  u <- sort(unique(classes))
  if (level == "personas") return(setNames(rep_len(COL_PERSONA, length(u)), u))
  cols <- setNames(rep(COL_OTHER, length(u)), u)
  cols[names(cols) == TARGET_LABEL] <- COL_TARGET
  cols[names(cols) == "UNKNOWN"] <- COL_TEST
  cols
}

short_name <- function(x, n = 28) {
  x <- sub("^[^_]+_", "", x)
  ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "…"), x)
}

theme_viz <- function() {
  theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#e6e6e1", linewidth = 0.4),
          axis.text = element_text(colour = "#5f5e5a"),
          legend.position = "top", plot.title.position = "plot")
}

# ---- Графики -----------------------------------------------------------------

plot_tree <- function(phylo, classes, level, main) {
  par(mar = c(1, 1, 3, 1))
  plot(phylo, tip.color = tip_colors(phylo$tip.label, classes, level),
       cex = tip_cex(length(classes)), font = 1, label.offset = 0.01,
       no.margin = FALSE, main = main)
}

tip_colors <- function(tips, classes, level) {
  cols <- class_colors(classes, level)
  cols[cols == COL_OTHER] <- COL_INK
  unname(cols[classes[tips]])
}

tip_cex <- function(n) if (n > 80) 0.55 else if (n > 40) 0.75 else 1

plot_map <- function(coords, classes, level, title, xlab, ylab, label_points = TRUE) {
  df <- data.frame(x = coords[, 1], y = coords[, 2], name = rownames(coords),
                   cls = classes[rownames(coords)], stringsAsFactors = FALSE)
  df$author <- class_label(df$cls)
  df$focus <- df$cls %in% c(TARGET_LABEL, "UNKNOWN") | level == "personas"
  cols <- class_colors(df$cls, level)
  names(cols) <- class_label(names(cols))
  df$label <- if (level == "chunks") df$author else short_name(df$name)
  df$label[df$cls == "UNKNOWN"] <- "ПРОВЕРЯЕМЫЙ ТЕКСТ"

  p <- ggplot(df, aes(x, y)) +
    geom_point(data = df[!df$focus, ], aes(colour = author), size = 2.6, alpha = 0.7) +
    geom_point(data = df[df$focus, ], aes(colour = author), size = 3.2,
               stroke = 0.6, shape = 21, fill = NA) +
    geom_point(data = df[df$focus, ], aes(colour = author), size = 2.6) +
    scale_colour_manual(values = cols, name = NULL,
                        # у «прочих» авторов цвет общий — их различают подписи, легенда не нужна
                        guide = if (level == "personas") "legend" else "none") +
    labs(title = title, x = xlab, y = ylab) +
    theme_viz()
  if (label_points) {
    p <- p + geom_text(aes(label = label), size = 3, colour = "#3d3d3a",
                       vjust = -0.9, check_overlap = TRUE)
  }
  if ("UNKNOWN" %in% df$cls) {
    p <- p + geom_point(data = df[df$cls == "UNKNOWN", ], colour = COL_TEST, size = 6, shape = 18)
  }
  p
}

# ---- UI ----------------------------------------------------------------------

about_md <- "
### Что такое стилометрия

Стилометрия — определение автора по количественным особенностям стиля
([подробнее на «Системном блоке»](https://sysblok.ru/knowhow/stilometrija-kak-v-raznoe-vremja-ljudi-iskali-avtorov-tekstov/)).
Главная идея (Мостеллер и Уоллес, Берроуз): у каждого автора устойчивые частоты
самых **частотных слов** — союзов, предлогов, местоимений. Их человек почти не
контролирует, поэтому они выдают автора лучше, чем «яркая» лексика.

### Как это устроено здесь

1. **Сбор** — тексты Славы КПСС и его альтер-эго (Гнойный, Соня Мармеладова,
   Валентин Дядька) и других рэперов скачиваются с Genius (`lyricsgenius`).
2. **Фильтрация** — по заголовкам секций Genius (`[Куплет 1: Слава КПСС]`)
   оставляются только партии нужного исполнителя; повторы припевов и эдлибы
   в скобках убираются.
3. **Анализ** — R-пакет [`stylo`](https://github.com/computationalstylistics/stylo):
   таблица частот N самых частотных слов (MFW) → расстояние Delta → кластеризация,
   MDS, консенсусное дерево; для проверки текста — метод **General Imposters**
   (`stylo::imposters`), Delta и SVM (`stylo::classify`).

### Параметры

* **MFW** — сколько самых частотных слов брать. 100–500 — «служебный» уровень,
  больше — подключается тематическая лексика.
* **Culling** — слово учитывается, только если встречается в заданной доле текстов.
* **Delta** — расстояние между текстами по z-оценкам частот. Cosine Delta (Würzburg)
  обычно надёжнее классической.
* **Imposters** — текст много раз сравнивается со случайными подмножествами слов
  и «самозванцами» (текстами других авторов). Оценка 0…1 — доля раундов,
  когда кандидат оказался ближе всех. Ниже `p1` — скорее не автор, выше `p2` — скорее автор.

### Ограничения

* Песня — 200–400 слов, а для надёжной атрибуции нужны тысячи. Поэтому корпус
  склеивается в куски по ~1500 слов, а проверка одной песни — ориентир, а не экспертиза.
* Рэп часто пишется в соавторстве и с гострайтерами; Genius-разметка партий
  делается пользователями и бывает неточной.
* Результат зависит от набора «конкурентов»: imposters отвечает на вопрос
  «похоже ли на Славу больше, чем на этих рэперов».
"

ui <- page_navbar(
  title = "Слава КПСС · стилометрия",
  theme = bs_theme(version = 5, primary = COL_TARGET, base_font = font_collection("system-ui", "sans-serif")),
  fillable = FALSE,

  nav_panel("Корпус",
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Песен Славы", textOutput("vb_songs")),
      value_box("Слов Славы", textOutput("vb_words")),
      value_box("Авторов для сравнения", length(AUTHORS) - 1),
      value_box("Всего песен", nrow(DATA$meta))
    ),
    layout_columns(
      col_widths = c(5, 7),
      card(card_header("Объём корпуса по авторам"), tableOutput("authors_table")),
      card(card_header("Альтер-эго Славы"), plotOutput("persona_plot", height = 260))
    ),
    card(card_header("Песни в корпусе"), DTOutput("songs_table"))
  ),

  nav_panel("Частые слова",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("fw_who", "Автор / альтер-эго",
                    choices = c(setNames(AUTHORS, class_label(AUTHORS)),
                                setNames(paste0("persona:", names(PERSONA_NAMES)),
                                         paste("Слава как", PERSONA_NAMES))),
                    selected = TARGET_LABEL),
        sliderInput("fw_top", "Сколько слов", 10, 100, 30, step = 5),
        checkboxInput("fw_stop", "Скрыть служебные слова", FALSE),
        helpText("Для стилометрии важны именно служебные слова — их частоты",
                 "автор почти не контролирует.")
      ),
      card(card_header(textOutput("fw_title")), plotOutput("fw_plot", height = 640)),
      card(card_header("Характерные слова Славы КПСС на фоне других рэперов"),
           helpText(textOutput("key_method", inline = TRUE)),
           uiOutput("key_out"))
    )
  ),

  nav_panel("Стилометрия",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectInput("st_level", "Единица анализа", LEVELS),
        selectInput("st_method", "Метод",
                    c("Кластерный анализ (дендрограмма)" = "CA",
                      "Многомерное шкалирование (MDS)" = "MDS",
                      "Метод главных компонент (PCA)" = "PCA",
                      "Консенсусное дерево (BCT)" = "BCT")),
        selectInput("st_feat", "Признаки",
                    c("Слова" = "w1", "Биграммы слов" = "w2", "Символьные 3-граммы" = "c3", "Символьные 4-граммы" = "c4")),
        sliderInput("st_mfw", "MFW (самых частотных признаков)", 50, 2000, 300, step = 50),
        sliderInput("st_cull", "Culling, %", 0, 100, 0, step = 10),
        selectInput("st_dist", "Расстояние", DISTANCES, selected = "dist.wurzburg"),
        conditionalPanel("input.st_level != 'personas'",
          checkboxGroupInput("st_authors", "Авторы", choices = setNames(AUTHORS, class_label(AUTHORS)),
                             selected = AUTHORS)),
        conditionalPanel("input.st_method == 'BCT'",
          helpText("BCT строит деревья для MFW от 100 до выбранного значения с шагом 100",
                   "и оставляет ветви, которые повторяются в ≥50% деревьев."))
      ),
      card(full_screen = TRUE, card_header(textOutput("st_title")),
           plotOutput("st_plot", height = 720)),
      card(card_header("Ближайшие соседи текстов Славы (доля соседей-Слав)"),
           helpText("Для каждого текста Славы — ближайший текст в выбранной метрике.",
                    "Если стиль устойчив, ближайшим почти всегда будет тоже Слава."),
           tableOutput("st_neighbors"))
    )
  ),

  nav_panel("Проверка текста",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        textAreaInput("ck_text", "Текст песни", rows = 12,
                      placeholder = "Вставьте текст (можно прямо с Genius — заголовки [Куплет] уберутся)"),
        fileInput("ck_file", "…или загрузите .txt", accept = c(".txt", "text/plain")),
        selectInput("ck_level", "С чем сравнивать", LEVELS[1:2]),
        sliderInput("ck_mfw", "MFW", 50, 1000, 200, step = 50),
        selectInput("ck_dist", "Расстояние", DISTANCES[c(1, 2, 3, 5)], selected = "dist.wurzburg"),
        sliderInput("ck_iter", "Итераций imposters", 50, 500, 100, step = 50),
        checkboxInput("ck_exclude", "Исключить из корпуса совпадающие тексты (если проверяете песню из корпуса)", TRUE),
        actionButton("ck_go", "Проверить", class = "btn-primary w-100")
      ),
      uiOutput("ck_verdict"),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("General Imposters: оценка для каждого автора"),
             plotOutput("ck_imp_plot", height = 380)),
        card(card_header("Delta: среднее расстояние до текстов автора"),
             plotOutput("ck_delta_plot", height = 380))
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Место текста на карте корпуса (MDS)"), plotOutput("ck_map", height = 480)),
        card(card_header("10 ближайших текстов корпуса"), tableOutput("ck_nearest"))
      )
    )
  ),

  nav_panel("О методе", card(markdown(about_md))),
  nav_spacer(),
  nav_item(tags$small(class = "text-muted", "stylo + Shiny"))
)

# ---- Server ------------------------------------------------------------------

server <- function(input, output, session) {

  # Корпус ---------------------------------------------------------------------
  meta <- DATA$meta
  slava <- meta[meta$author == TARGET_LABEL, ]
  output$vb_songs <- renderText(format(nrow(slava), big.mark = " "))
  output$vb_words <- renderText(format(sum(slava$words), big.mark = " "))

  output$authors_table <- renderTable({
    agg <- aggregate(words ~ author, meta, function(x) c(n = length(x), w = sum(x)))
    agg <- data.frame(Автор = class_label(agg$author), Песен = agg$words[, "n"],
                      Слов = agg$words[, "w"], check.names = FALSE)
    agg[order(-agg$Слов), ]
  }, digits = 0)

  output$persona_plot <- renderPlot({
    pm <- aggregate(words ~ persona, slava, sum)
    pm$label <- class_label(pm$persona)
    ggplot(pm, aes(reorder(label, words), words)) +
      geom_col(fill = COL_TARGET, width = 0.6) +
      geom_text(aes(label = format(words, big.mark = " ")), hjust = -0.15, size = 3.6, colour = "#3d3d3a") +
      coord_flip(clip = "off") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = "слов") + theme_viz()
  })

  output$songs_table <- renderDT({
    d <- meta[, c("author", "persona", "title", "album", "release_date", "words", "url")]
    d$author <- class_label(d$author)
    d$persona <- class_label(d$persona)
    d$url <- ifelse(nzchar(d$url), sprintf('<a href="%s" target="_blank">Genius</a>', d$url), "")
    names(d) <- c("Автор", "Персона", "Песня", "Альбом", "Дата", "Слов", "")
    datatable(d, escape = FALSE, rownames = FALSE,
              options = list(pageLength = 15, language = list(search = "Поиск:")))
  }, server = FALSE)

  # Частые слова ---------------------------------------------------------------
  fw_texts <- reactive({
    who <- input$fw_who
    if (startsWith(who, "persona:")) {
      p <- sub("^persona:", "", who)
      DATA$personas[text_class(names(DATA$personas)) == p]
    } else {
      DATA$songs[text_class(names(DATA$songs)) == who]
    }
  })

  output$fw_title <- renderText({
    who <- sub("^persona:", "", input$fw_who)
    sprintf("Самые частотные слова: %s (%d текстов)", class_label(who), length(fw_texts()))
  })

  output$fw_plot <- renderPlot({
    validate(need(length(fw_texts()) > 0, "Нет текстов для этого автора"))
    wf <- word_freqs(fw_texts(), top = input$fw_top, drop_stop = input$fw_stop)
    ggplot(wf, aes(reorder(word, per1000), per1000)) +
      geom_col(fill = COL_TARGET, width = 0.7) +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(x = NULL, y = "на 1000 слов") + theme_viz()
  })

  keywords <- reactive({
    zeta <- DATA$zeta
    if (!is.null(zeta) && length(zeta$words.preferred)) {
      n <- 30
      pref <- head(zeta$words.preferred, n); avoid <- head(zeta$words.avoided, n)
      len <- max(length(pref), length(avoid))
      return(list(method = "Craig's Zeta (stylo::oppose): слова, которые встречаются у Славы в заметно большей (меньшей) доле фрагментов, чем у других рэперов. Списки упорядочены по силе эффекта.",
                  table = data.frame(`№` = seq_len(len),
                                     `Предпочитает` = c(pref, rep("", len - length(pref))),
                                     `Избегает` = c(avoid, rep("", len - length(avoid))),
                                     check.names = FALSE)))
    }
    # Без предрасчитанной Zeta: логарифм отношения частот со сглаживанием
    is_t <- text_class(names(DATA$songs)) == TARGET_LABEL
    t1 <- table(unlist(DATA$songs[is_t])); t2 <- table(unlist(DATA$songs[!is_t]))
    words <- union(names(t1), names(t2))
    f1 <- as.numeric(t1[words]); f1[is.na(f1)] <- 0
    f2 <- as.numeric(t2[words]); f2[is.na(f2)] <- 0
    ok <- (f1 + f2) >= 10
    lr <- log2(((f1 + 0.5) / sum(f1)) / ((f2 + 0.5) / sum(f2)))[ok]
    names(lr) <- words[ok]
    lr <- sort(lr)
    list(method = "Log-ratio частот (слова, встречающиеся ≥10 раз). Запустите analysis/run_stylo.R, чтобы получить Craig's Zeta.",
         df = rbind(data.frame(word = names(tail(lr, 20)), score = tail(lr, 20), side = "Предпочитает"),
                    data.frame(word = names(head(lr, 20)), score = head(lr, 20), side = "Избегает")))
  })

  output$key_method <- renderText(keywords()$method)
  output$key_out <- renderUI({
    if (!is.null(keywords()$table)) tableOutput("key_table") else plotOutput("key_plot", height = 520)
  })
  output$key_table <- renderTable(keywords()$table, digits = 0)
  output$key_plot <- renderPlot({
    df <- keywords()$df
    ggplot(df, aes(reorder(word, score), score, fill = side)) +
      geom_col(width = 0.7) +
      coord_flip() +
      scale_fill_manual(values = c("Предпочитает" = COL_TARGET, "Избегает" = COL_OTHER), name = NULL) +
      labs(x = NULL, y = "log2 отношения частот: Слава / другие") + theme_viz()
  })

  # Стилометрия ----------------------------------------------------------------
  st_corpus <- reactive({
    corpus <- DATA[[input$st_level]]
    if (input$st_level != "personas") {
      req(length(input$st_authors) >= 2)
      corpus <- corpus[text_class(names(corpus)) %in% input$st_authors]
    }
    feat <- input$st_feat
    corpus_features(corpus, type = substr(feat, 1, 1), ngram = as.integer(substr(feat, 2, 2)))
  })

  st_table <- reactive({
    freq_table(st_corpus(), mfw = input$st_mfw, culling = input$st_cull)
  })

  st_dist <- reactive(dist_matrix(st_table(), input$st_dist))

  output$st_title <- renderText({
    sprintf("%s · %d текстов · %d MFW · %s", names(which(c(
      "Кластерный анализ" = "CA", "MDS" = "MDS", "PCA" = "PCA", "Консенсусное дерево" = "BCT") == input$st_method)),
      nrow(st_table()), ncol(st_table()), names(which(DISTANCES == input$st_dist)))
  })

  output$st_plot <- renderPlot({
    tbl <- st_table()
    validate(need(nrow(tbl) >= 3, "Нужно хотя бы 3 текста"))
    classes <- setNames(text_class(rownames(tbl)), rownames(tbl))
    level <- input$st_level
    lab <- function(x) if (level == "chunks") x else rownames(tbl)

    if (input$st_method == "CA") {
      hc <- hclust(as.dist(st_dist()), method = "ward.D2")
      plot_tree(as.phylo(hc), classes, level, "")
    } else if (input$st_method == "MDS") {
      xy <- cmdscale(as.dist(st_dist()), k = 2)
      plot_map(xy, classes, level, NULL, "Измерение 1", "Измерение 2", label_points = nrow(tbl) <= 80)
    } else if (input$st_method == "PCA") {
      x <- tbl[, apply(tbl, 2, sd) > 0, drop = FALSE]
      pc <- prcomp(x, scale. = TRUE)
      ve <- round(100 * pc$sdev^2 / sum(pc$sdev^2), 1)
      plot_map(pc$x[, 1:2], classes, level, NULL, sprintf("PC1 (%.1f%%)", ve[1]),
               sprintf("PC2 (%.1f%%)", ve[2]), label_points = nrow(tbl) <= 80)
    } else {
      steps <- seq(100, max(100, input$st_mfw), by = 100)
      corpus <- st_corpus()
      trees <- lapply(steps, function(m) {
        t <- freq_table(corpus, mfw = m, culling = input$st_cull)
        as.phylo(hclust(as.dist(dist_matrix(t, input$st_dist)), method = "ward.D2"))
      })
      cons <- if (length(trees) > 1) ape::consensus(trees, p = 0.5) else trees[[1]]
      par(mar = c(1, 1, 1, 1))
      plot(cons, type = "fan", tip.color = tip_colors(cons$tip.label, classes, level),
           cex = tip_cex(nrow(tbl)), no.margin = TRUE)
    }
  })

  output$st_neighbors <- renderTable({
    d <- st_dist()
    diag(d) <- Inf
    cls <- text_class(rownames(d))
    validate(need(input$st_level != "personas", "Для альтер-эго смотрите график: все тексты здесь — Славы."))
    idx <- which(cls == TARGET_LABEL)
    validate(need(length(idx) > 0, "Слава не выбран в списке авторов"))
    nn <- apply(d[idx, , drop = FALSE], 1, which.min)
    nn_cls <- cls[nn]
    tab <- sort(table(class_label(nn_cls)), decreasing = TRUE)
    data.frame(`Ближайший сосед — автор` = names(tab), Текстов = as.integer(tab),
               Доля = sprintf("%.0f%%", 100 * as.numeric(tab) / length(idx)), check.names = FALSE)
  })

  # Проверка текста ------------------------------------------------------------
  observeEvent(input$ck_file, {
    txt <- readLines(input$ck_file$datapath, encoding = "UTF-8", warn = FALSE)
    updateTextAreaInput(session, "ck_text", value = paste(txt, collapse = "\n"))
  })

  check <- eventReactive(input$ck_go, {
    toks <- tokenize(input$ck_text)
    validate(need(length(toks) >= 50, "Слишком короткий текст: нужно хотя бы 50 слов (лучше 300+)."))
    corpus <- DATA[[input$ck_level]]
    if (isTRUE(input$ck_exclude)) {
      # Убираем тексты корпуса, почти целиком совпадающие с проверяемым (иначе тривиально «узнаем» песню)
      # (критерий: >60% биграмм слов проверяемого текста встречаются в тексте корпуса).
      bigrams <- function(x) paste(head(x, -1), x[-1])
      test_bg <- unique(bigrams(toks))
      overlap <- vapply(corpus, function(x) mean(test_bg %in% bigrams(x)) > 0.6, logical(1))
      corpus <- corpus[!overlap]
    }
    withProgress(message = "Считаю imposters, Delta и SVM…", value = 0.3, {
      res <- check_text(corpus, toks, mfw = input$ck_mfw, measure = input$ck_dist,
                        iterations = input$ck_iter)
      incProgress(0.6)
    })
    res$corpus <- corpus
    res$excluded <- if (isTRUE(input$ck_exclude)) sum(!names(DATA[[input$ck_level]]) %in% names(corpus)) else 0
    res
  })

  output$ck_verdict <- renderUI({
    req(input$ck_go > 0)
    r <- check()
    score <- r$imposters[TARGET_LABEL]
    v <- verdict(score, CALIB[["p1"]], CALIB[["p2"]])
    theme <- switch(v$level, yes = "success", no = "danger", maybe = "warning", "secondary")
    best <- names(r$imposters)[1]
    notes <- c(
      sprintf("Imposters: %.2f для Славы КПСС (серая зона %.2f–%.2f).", score, CALIB[["p1"]], CALIB[["p2"]]),
      sprintf("Ближе всего по Delta: %s. SVM относит к: %s.",
              class_label(names(r$delta_by_author)[1]), ifelse(is.na(r$svm), "—", class_label(r$svm))),
      if (length(best) && best != TARGET_LABEL) sprintf("Максимальная оценка imposters — у автора %s (%.2f).", class_label(best), r$imposters[1]),
      if (r$excluded > 0) sprintf("Из корпуса исключено совпадающих текстов: %d.", r$excluded),
      if (r$n_tokens < 1000) sprintf("Внимание: в тексте %d слов — для надёжной атрибуции нужно 1500+, результат ориентировочный.", r$n_tokens)
    )
    value_box(title = "Вердикт", value = v$text, theme = theme,
              tags$p(paste(notes, collapse = " ")))
  })

  output$ck_imp_plot <- renderPlot({
    r <- check()
    validate(need(length(r$imposters) > 0, paste("imposters не посчитался:", attr(r$imposters, "error"))))
    df <- data.frame(author = class_label(names(r$imposters)), score = as.numeric(r$imposters),
                     target = names(r$imposters) == TARGET_LABEL)
    ggplot(df, aes(reorder(author, score), score, fill = target)) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = CALIB[["p1"]], ymax = CALIB[["p2"]],
               fill = "#eda100", alpha = 0.12) +
      geom_col(width = 0.65) +
      geom_text(aes(label = sprintf("%.2f", score)), hjust = -0.2, size = 3.6, colour = "#3d3d3a") +
      coord_flip(clip = "off") +
      scale_fill_manual(values = c(`TRUE` = COL_TARGET, `FALSE` = COL_OTHER), guide = "none") +
      scale_y_continuous(limits = c(0, 1.08), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
      labs(x = NULL, y = "оценка (жёлтая полоса — серая зона)") + theme_viz()
  })

  output$ck_delta_plot <- renderPlot({
    r <- check()
    d <- r$delta_by_author
    df <- data.frame(author = class_label(names(d)), dist = as.numeric(d), target = names(d) == TARGET_LABEL)
    ggplot(df, aes(reorder(author, -dist), dist, fill = target)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = sprintf("%.2f", dist)), hjust = -0.2, size = 3.6, colour = "#3d3d3a") +
      coord_flip(clip = "off") +
      scale_fill_manual(values = c(`TRUE` = COL_TARGET, `FALSE` = COL_OTHER), guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = "расстояние (меньше — ближе)") + theme_viz()
  })

  output$ck_map <- renderPlot({
    r <- check()
    d <- dist_matrix(r$table, input$ck_dist)
    xy <- cmdscale(as.dist(d), k = 2)
    classes <- setNames(text_class(rownames(xy)), rownames(xy))
    plot_map(xy, classes, input$ck_level, NULL, "Измерение 1", "Измерение 2",
             label_points = input$ck_level == "chunks")
  })

  output$ck_nearest <- renderTable({
    r <- check()
    data.frame(Автор = class_label(text_class(names(r$nearest))),
               Текст = short_name(names(r$nearest), 40),
               Расстояние = sprintf("%.3f", r$nearest), check.names = FALSE)
  })
}

shinyApp(ui, server)
