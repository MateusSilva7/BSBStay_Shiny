FROM rocker/r-ver:4.3.3

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    libxt-dev \
    libsqlite3-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    zlib1g-dev \
    pandoc \
    make \
    g++ \
    && rm -rf /var/lib/apt/lists/*

RUN R -q -e "install.packages(c( \
    'shiny','dplyr','tidyr','lubridate','readxl','janitor', \
    'plotly','DT','DBI','RSQLite','shinycssloaders','stringr', \
    'htmlwidgets','bslib','digest','later' \
  ), repos='https://cloud.r-project.org', Ncpus=parallel::detectCores())"

WORKDIR /opt/render/project/src

COPY . .

RUN mkdir -p data/cache data/raw

ENV APP_ROOT=/opt/render/project/src \
    APP_MODE=public \
    MAX_CACHE_AGE_H=6 \
    PORT=3838

EXPOSE 3838

CMD ["R", "-q", "-f", "/opt/render/project/src/run.R"]