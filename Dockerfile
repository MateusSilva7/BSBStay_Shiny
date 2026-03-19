# ============================================================
# Dockerfile — BSBStay Shiny App
# Otimizado para Render.com (Docker runtime)
# ============================================================

FROM rocker/r-ver:4.3.3

ENV DEBIAN_FRONTEND=noninteractive

# ── Dependências do sistema ───────────────────────────────────
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
    curl \
    && rm -rf /var/lib/apt/lists/*

# ── Pacotes R ─────────────────────────────────────────────────
# Instalados em camada separada para melhor cache de build.
RUN R -q -e "install.packages(c( \
    'shiny','dplyr','tidyr','lubridate','readxl','janitor', \
    'plotly','DT','DBI','RSQLite','shinycssloaders','stringr', \
    'htmlwidgets','bslib','digest','later','htmltools' \
  ), repos='https://cloud.r-project.org', Ncpus=parallel::detectCores())"

# ── Diretório de trabalho ─────────────────────────────────────
WORKDIR /opt/render/project/src

# ── Copia o código-fonte ──────────────────────────────────────
# .dockerignore exclui: *.sqlite, *.xlsx, .Rproj.user, *.zip
COPY . .

# ── Diretórios de dados no código (somente para data/raw) ─────
# O cache mutável (SQLite + xlsx) vai para /tmp em runtime,
# conforme APP_CACHE_DIR. data/raw pode conter xlsx de fallback.
RUN mkdir -p data/raw

# ── Permissões: /tmp já é gravável; cria subdir do cache ─────
RUN mkdir -p /tmp/bsbstay_cache && chmod 777 /tmp/bsbstay_cache

# ── Variáveis de ambiente padrão ─────────────────────────────
# Sobrescritas pelas envVars do render.yaml / painel do Render.
ENV APP_ROOT=/opt/render/project/src \
    APP_CACHE_DIR=/tmp/bsbstay_cache \
    APP_MODE=public \
    MAX_CACHE_AGE_H=6 \
    PORT=3838

# ── Healthcheck ───────────────────────────────────────────────
# Render verifica se a porta está respondendo.
# O app pode demorar ~60s no cold start (download do Drive).
HEALTHCHECK --interval=30s --timeout=10s --start-period=120s --retries=3 \
  CMD curl -sf http://localhost:${PORT} || exit 1

EXPOSE 3838

CMD ["R", "-q", "-f", "/opt/render/project/src/run.R"]
