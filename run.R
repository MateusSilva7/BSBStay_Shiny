# run.R — Ponto de entrada único para Docker / Render.com
#
# Responsabilidades EXCLUSIVAS deste arquivo:
#   1. Configurar shiny.host e shiny.port (UMA VEZ)
#   2. Apontar APP_CACHE_DIR para /tmp (gravável no Render)
#   3. Pré-aquecer o cache SQLite (UMA VEZ, antes do primeiro usuário)
#   4. Iniciar o app

# ── 1. Host / Port ────────────────────────────────────────────
options(
  shiny.host            = "0.0.0.0",
  shiny.port            = as.integer(Sys.getenv("PORT", "3838")),
  shiny.maxRequestSize  = 50 * 1024^2,
  shiny.sanitize.errors = FALSE
)

# ── 2. Paths ──────────────────────────────────────────────────
# /opt/render/project/src é read-only após o build.
# Dados mutáveis (SQLite, xlsx cache) devem ir para /tmp.
app_root  <- normalizePath(Sys.getenv("APP_ROOT", "."), winslash = "/", mustWork = FALSE)
cache_dir <- Sys.getenv("APP_CACHE_DIR", "/tmp/bsbstay_cache")
raw_dir   <- Sys.getenv("APP_RAW_DIR",   file.path(app_root, "data", "raw"))

dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(raw_dir,   recursive = TRUE, showWarnings = FALSE)

Sys.setenv(APP_CACHE_DIR = cache_dir)
Sys.setenv(APP_RAW_DIR   = raw_dir)
Sys.setenv(APP_ROOT      = app_root)

message(sprintf("[run.R] APP_ROOT      = %s", app_root))
message(sprintf("[run.R] APP_CACHE_DIR = %s", cache_dir))

# ── 3. Pacotes mínimos ────────────────────────────────────────
suppressPackageStartupMessages({ library(shiny); library(later) })

# ── 4. gdrive_public.R — carrega UMA VEZ no ambiente global ──
# Todos os módulos filhos verificam exists("carregar_dados_app")
# antes de fazer source novamente, evitando cargas duplas.
source(file.path(app_root, "R", "gdrive_public.R"), local = FALSE)

# ── 5. Pré-aquecimento do cache (boot-time) ───────────────────
# forcar_dl=FALSE: só baixa se o cache for inexistente ou velho.
# O resultado é guardado em APP_DATA_GLOBAL para os módulos filhos.
message("[run.R] Iniciando pré-aquecimento do cache...")
APP_DATA_GLOBAL <<- tryCatch(
  carregar_dados_app(
    file_id    = DRIVE_FILE_ID,
    folder_id  = DRIVE_FOLDER_ID,
    forcar_dl  = FALSE,
    forcar_etl = FALSE
  ),
  error = function(e) {
    message("[run.R] AVISO cache: ", e$message,
            " — app tentará na primeira sessão.")
    structure(list(), erro_msg = e$message)
  }
)
message(sprintf("[run.R] Cache pronto: %d proprietário(s).", length(APP_DATA_GLOBAL)))

# ── 6. Inicia o app ───────────────────────────────────────────
app <- source(file.path(app_root, "app.R"), local = new.env())$value
print(app)
