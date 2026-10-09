# Готовит данные для веб-приложения: токенизирует corpus/ через stylo
# и сохраняет всё в app/data/corpus.rds.
#
#   Rscript analysis/prepare.R
#
# Запускать из корня проекта после `python -m scraper.build_corpus`.

source("app/R/helpers.R", encoding = "UTF-8")

dir.create("app/data", showWarnings = FALSE, recursive = TRUE)

meta <- read.csv("corpus/metadata.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

data <- list(
  chunks   = load_corpus_dir("corpus/chunks"),
  songs    = load_corpus_dir("corpus/songs"),
  personas = load_corpus_dir("corpus/personas"),
  meta     = meta,
  created  = Sys.time()
)

# Пороги «серой зоны» для imposters, если их посчитал analysis/run_stylo.R
if (file.exists("app/data/calibration.rds")) data$calibration <- readRDS("app/data/calibration.rds")
# Craig's Zeta (характерные слова), тоже из analysis/run_stylo.R
if (file.exists("app/data/zeta.rds")) data$zeta <- readRDS("app/data/zeta.rds")

saveRDS(data, "app/data/corpus.rds")

cat("Сохранено app/data/corpus.rds\n")
for (lvl in c("chunks", "songs", "personas")) {
  cls <- table(text_class(names(data[[lvl]])))
  cat(sprintf("  %-9s %s\n", lvl, paste(names(cls), cls, sep = ":", collapse = "  ")))
}
