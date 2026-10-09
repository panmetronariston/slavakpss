# Контейнер с веб-приложением. Перед сборкой подготовьте данные:
#   python -m scraper.build_corpus && Rscript analysis/prepare.R
#   docker build -t slavakpss . && docker run -p 3838:3838 slavakpss
FROM rocker/r-ver:4.4.1

RUN apt-get update && apt-get install -y --no-install-recommends tcl tk libtcl8.6 libtk8.6 \
    && rm -rf /var/lib/apt/lists/*
RUN install2.r --error --skipinstalled stylo shiny bslib DT ggplot2 ape e1071

WORKDIR /app
COPY app/ /app/

ENV PORT=3838
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = as.integer(Sys.getenv('PORT')))"]
