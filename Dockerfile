FROM rocker/r-ver:4.5.2

RUN apt-get update && apt-get install -y \
    libsodium-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    libcairo2-dev \
    libfontconfig1-dev \
    libpng-dev \
    pandoc \
    git \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny','renv','BiocManager'), repos='https://cloud.r-project.org')"

WORKDIR /srv/shiny-server/haplodb

COPY renv.lock renv.lock

RUN R -e "\
  options(repos = c( \
    BiocManager::repositories(), \
    CRAN = 'https://packagemanager.posit.co/cran/latest' \
  )); \
  renv::restore(prompt = FALSE) \
"

COPY . .

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]
