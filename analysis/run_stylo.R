# Классический стилометрический анализ функцией stylo() + проверка качества.
# Результаты (картинки и таблицы) — в results/.
#
#   Rscript analysis/run_stylo.R
#
# Запускать из корня проекта после `python -m scraper.build_corpus`.
# Это «отчётная» часть: те же графики, что в приложении, но канонической
# функцией stylo(), плюс кросс-валидация и калибровка порогов imposters
# (последняя может идти десятки минут).

source("app/R/helpers.R", encoding = "UTF-8")

dir.create("results", showWarnings = FALSE)
dir.create("app/data", showWarnings = FALSE, recursive = TRUE)
results_dir <- normalizePath("results")

run_stylo <- function(corpus, title, ...) {
  in_tempdir({
    res <- stylo::stylo(
      gui = FALSE, parsed.corpus = corpus, corpus.lang = "Other",
      distance.measure = "dist.wurzburg", culling.min = 20, culling.max = 20,
      display.on.screen = FALSE, write.png.file = TRUE,
      custom.graph.title = title, ...
    )
    pngs <- list.files(pattern = "\\.png$")
    file.copy(pngs, results_dir, overwrite = TRUE)
    file.remove(pngs)
    res
  })
}

# Шаги независимы: ошибка в одном не останавливает остальные
step <- function(title, expr) {
  cat("\n==", title, "==\n")
  tryCatch(invisible(force(expr)), error = function(e) cat("  ОШИБКА:", conditionMessage(e), "\n"))
}

chunks <- load_corpus_dir("corpus/chunks")
personas <- load_corpus_dir("corpus/personas")

step("1. Кластерный анализ и MDS по кускам текстов", {
  run_stylo(chunks, "Рэперы: кластерный анализ", analysis.type = "CA", mfw.min = 300, mfw.max = 300)
  run_stylo(chunks, "Рэперы: MDS", analysis.type = "MDS", mfw.min = 300, mfw.max = 300)
})

step("2. Bootstrap consensus tree (100–1000 MFW)", {
  run_stylo(chunks, "Рэперы: консенсусное дерево", analysis.type = "BCT",
            mfw.min = 100, mfw.max = 1000, mfw.incr = 100, consensus.strength = 0.5)
})

step("3. Альтер-эго Славы: различаются ли персоны стилистически?", {
  if (length(unique(text_class(names(personas)))) > 1) {
    run_stylo(personas, "Альтер-эго Славы КПСС", analysis.type = "MDS", mfw.min = 200, mfw.max = 200)
  }
})

step("4. Надёжность: leave-one-out, ближайший сосед по Delta", {
  # Для каждого куска ищем ближайший другой кусок; доля совпадений автора = точность.
  for (m in c(100, 300, 500, 1000)) {
    for (measure in c("dist.delta", "dist.wurzburg")) {
      d <- dist_matrix(freq_table(chunks, mfw = m), measure)
      diag(d) <- Inf
      cls <- text_class(rownames(d))
      acc <- mean(cls[apply(d, 1, which.min)] == cls)
      cat(sprintf("  MFW %4d  %-14s точность %.3f\n", m, measure, acc))
    }
  }
})

step("4b. Кросс-валидация stylo::classify (SVM, 10 фолдов)", {
  cv <- in_tempdir(stylo::classify(
    gui = FALSE, training.frequencies = freq_table(chunks, mfw = 300),
    test.frequencies = freq_table(chunks, mfw = 300)[1, , drop = FALSE],
    cv.folds = 10, classification.method = "svm", mfw.min = 300, mfw.max = 300,
    culling.min = 0, culling.max = 0
  ))
  capture.output(print(cv), file = file.path(results_dir, "classify_cv.txt"))
  if (!is.null(cv$cross.validation.summary))
    cat(sprintf("  Средняя точность SVM: %.3f\n", mean(unlist(cv$cross.validation.summary))))
})

step("5. Характерные слова Славы КПСС (Craig's Zeta, stylo::oppose)", {
  is_target <- text_class(names(chunks)) == TARGET_LABEL
  zeta <- in_tempdir(stylo::oppose(
    gui = FALSE, primary.corpus = chunks[is_target], secondary.corpus = chunks[!is_target],
    text.slice.length = 500, display.on.screen = FALSE, write.png.file = TRUE
  ))
  writeLines(c("# Предпочитаемые", head(zeta$words.preferred, 100), "",
               "# Избегаемые", head(zeta$words.avoided, 100)),
             file.path(results_dir, "zeta_words.txt"))
  saveRDS(zeta[c("words.preferred", "words.avoided")], "app/data/zeta.rds")
  cat("  Предпочитает:", head(zeta$words.preferred, 15), "\n")
  cat("  Избегает:   ", head(zeta$words.avoided, 15), "\n")
})

step("6. Калибровка порогов imposters (imposters.optimize) — долго", {
  calib <- quietly(stylo::imposters.optimize(freq_table(chunks, mfw = 300)))
  calib <- setNames(as.numeric(calib)[1:2], c("p1", "p2"))
  cat(sprintf("  Серая зона: p1 = %.2f, p2 = %.2f\n", calib[["p1"]], calib[["p2"]]))
  saveRDS(calib, "app/data/calibration.rds")
})

cat("Готово. Картинки и таблицы в results/. Перезапустите analysis/prepare.R,\n",
    "чтобы приложение подхватило калибровку и Zeta-слова.\n")
