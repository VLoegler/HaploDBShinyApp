FROM --platform=linux/amd64 rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libsodium-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server/haplodb

# Use Posit Package Manager binaries to avoid compiling under QEMU emulation
ENV RENV_CONFIG_PPM_ENABLED=TRUE
ENV RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/latest"

COPY renv.lock renv.lock
RUN R -e "\
  install.packages(c('renv', 'BiocManager')); \
  options(repos = c(BiocManager::repositories(), \
    CRAN = 'https://packagemanager.posit.co/cran/latest')); \
  renv::restore(prompt = FALSE) \
"

COPY . .

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]
