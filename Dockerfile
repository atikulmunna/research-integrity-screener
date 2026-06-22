# Self-contained image for the Research Integrity Screener.
# Build:  docker build -t ris .
# Run:    docker run -p 3838:3838 ris   ->  http://localhost:3838
#
# rocker/r-ver pulls Linux binary packages from Posit Package Manager by default,
# so installs are fast and reproducible for this R version.
FROM rocker/r-ver:4.4.2

# System libraries:
#  - libpoppler-cpp-dev  -> pdftools (PDF text extraction)
#  - curl/ssl/xml2       -> golem/httr stack
#  - font/image libs     -> ggplot2 / ragg / systemfonts rendering
RUN apt-get update && apt-get install -y --no-install-recommends \
      libpoppler-cpp-dev \
      libcurl4-openssl-dev libssl-dev libxml2-dev \
      libfontconfig1-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
      libharfbuzz-dev libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . /app

# Install the package and all its (CRAN) runtime dependencies. dmetar is a
# test-only reference and is NOT in DESCRIPTION, so it is not installed here.
RUN R -e "install.packages('remotes'); \
          remotes::install_local('.', dependencies = TRUE, upgrade = 'never')"

EXPOSE 3838
CMD ["R", "-e", "options(shiny.port = 3838, shiny.host = '0.0.0.0'); RIS::run_app()"]
