# ============================================================
# app_public.R — Extrato do Proprietário autenticado por sessão
# v3.1-render: adaptado para Render.com / Docker
#
# ATENÇÃO: options(shiny.host/port) e source(gdrive_public.R)
# são feitos EXCLUSIVAMENTE em run.R. Este arquivo é um módulo
# filho carregado via sys.source() pelo app.R roteador.
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(lubridate)
  library(plotly)
  library(DT)
  library(htmltools)
})

APP_ROOT <- normalizePath(Sys.getenv("APP_ROOT", "."), winslash = "/", mustWork = FALSE)

# gdrive_public.R já foi carregado por run.R; só faz source se
# este módulo for rodado de forma standalone (ex: testes locais).
if (!exists("carregar_dados_app", inherits = TRUE)) {
  source(file.path(APP_ROOT, "R", "gdrive_public.R"), local = FALSE)
}

# APP_DATA_GLOBAL é pré-aquecido por run.R (boot-time).
# Se não existir (standalone), carrega agora com forcar_dl=FALSE.
if (!exists("APP_DATA_GLOBAL", inherits = TRUE)) {
  APP_DATA_GLOBAL <<- tryCatch(
    carregar_dados_app(
      file_id    = DRIVE_FILE_ID,
      folder_id  = DRIVE_FOLDER_ID,
      forcar_dl  = FALSE,
      forcar_etl = FALSE
    ),
    error = function(e) structure(list(), erro_msg = e$message)
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

TOKEN_TODOS <- "__todos__"

MESES_PT_FULL <- c(
  "janeiro", "fevereiro", "março", "abril", "maio", "junho",
  "julho", "agosto", "setembro", "outubro", "novembro", "dezembro"
)

MESES_PT_ABBR <- c(
  "jan", "fev", "mar", "abr", "mai", "jun",
  "jul", "ago", "set", "out", "nov", "dez"
)

fmt_mes_pt <- function(x, abreviado = FALSE) {
  x <- as.character(x)
  d <- suppressWarnings(as.Date(paste0(substr(x, 1, 7), "-01")))
  if (all(is.na(d))) return(x)
  mes <- as.integer(format(d, "%m"))
  ano <- format(d, "%Y")
  lab <- if (abreviado) MESES_PT_ABBR[mes] else MESES_PT_FULL[mes]
  paste0(tools::toTitleCase(lab), "/", ano)
}

fmt_currency <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (length(v) == 0) return("R$ —")
  ifelse(
    is.na(v),
    "R$ —",
    paste0("R$ ", formatC(v, format = "f", digits = 2, big.mark = ".", decimal.mark = ","))
  )
}

# brl vetorizada — segura dentro de dplyr::transmute
brl <- function(x) {
  sapply(x, function(v) {
    v <- suppressWarnings(as.numeric(v))
    if (is.na(v)) return("R$ \u2014")
    paste0("R$ ", formatC(v, format = "f", digits = 2, big.mark = ".", decimal.mark = ","))
  }, USE.NAMES = FALSE)
}


safe_num <- function(x, default = 0) {
  y <- suppressWarnings(as.numeric(x))
  y[is.na(y)] <- default
  y
}

safe_date_month <- function(x) {
  if (inherits(x, "Date")) return(as.Date(format(x, "%Y-%m-01")))
  suppressWarnings(as.Date(paste0(substr(as.character(x), 1, 7), "-01")))
}

kcard <- function(lbl, val, delta = "", dn = FALSE, vg = FALSE, icon = "", extra_class = "") {
  div(
    class = paste("kcard", extra_class),
    div(class = "klbl", if (nzchar(icon)) paste(icon, lbl) else lbl),
    div(class = if (vg) "kval g" else "kval", val),
    div(class = if (dn) "kdelta dn" else "kdelta up", delta)
  )
}

frow <- function(lbl, val, neg = FALSE) {
  div(
    class = "fr",
    span(class = "fl", lbl),
    span(class = if (neg) "fv r" else "fv", val)
  )
}

kcard_sm <- function(lbl, val, cor = "blue") {
  cor_map <- c(blue="#1a6ef7", green="#00b388", red="#e03e3e", orange="#d97706", purple="#7c3aed", teal="#0891b2")
  clr <- cor_map[[cor]] %||% "#1a6ef7"
  div(class = "kcard-sm",
      div(class = "ksm-lbl", lbl),
      div(class = "ksm-val", style = paste0("color:", clr), val))
}

ui <- fluidPage(
  tags$head(
    tags$title("BSB.STAY — Extrato do Proprietário"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap"),
    tags$style(HTML("
/* ══ RESET ══════════════════════════════════════════════════ */
*{box-sizing:border-box;margin:0;padding:0;}
body{font-family:'Inter',sans-serif;background:#f0f2f5;color:#1e2d3d;font-size:14px;}
a{color:inherit;text-decoration:none;}

/* ══ HEADER ═════════════════════════════════════════════════ */
.hdr{background:#0f1c2e;padding:14px 32px;display:flex;align-items:center;justify-content:space-between;position:sticky;top:0;z-index:100;box-shadow:0 2px 8px rgba(0,0,0,.25);}
.hdr-left{display:flex;align-items:center;gap:14px;}
.hdr-logo-img{height:52px;width:auto;display:block;border-radius:10px;box-shadow:0 4px 14px rgba(0,0,0,.25);background:#fff;}
.hdr-title{color:#fff;font-size:17px;font-weight:700;}
.hdr-sub{color:#5a7a96;font-size:11px;margin-top:2px;}
.hdr-prop{color:#7a9ab5;font-size:12px;text-align:right;line-height:1.5;}
.hdr-prop b{color:#e2f0ff;}
.hdr-badge{background:#1a3350;color:#5ab4ff;border-radius:20px;padding:3px 10px;font-size:10px;font-weight:700;letter-spacing:.6px;display:inline-block;margin-top:4px;}

/* ══ SYNC BAR ════════════════════════════════════════════════ */
.sync-bar{background:#f8fafc;border-bottom:1px solid #e2e8f0;padding:6px 32px;display:flex;align-items:center;gap:10px;font-size:11px;color:#6b7280;}
.sync-dot{width:7px;height:7px;border-radius:50%;background:#00b388;flex-shrink:0;}
.sync-dot.old{background:#d97706;} .sync-dot.err{background:#e03e3e;}
.sync-btn{background:none;border:1px solid #d1d5db;border-radius:6px;padding:3px 10px;font-size:11px;color:#374151;cursor:pointer;font-family:inherit;transition:all .15s;}
.sync-btn:hover{background:#f3f4f6;}

/* ══ CNPJ BAR ════════════════════════════════════════════════ */
.cnpj-bar{background:#fff;padding:18px 32px;border-bottom:1px solid #e2e8f0;}
.cnpj-bar-inner{max-width:680px;margin:0 auto;}
.cnpj-bar-title{font-weight:800;color:#0f1c2e;font-size:15px;margin-bottom:4px;}
.cnpj-bar-sub{font-size:12px;color:#6b7280;margin-bottom:14px;}
.cnpj-input-wrap{display:flex;gap:10px;align-items:flex-end;}
.cnpj-input-wrap .form-group{flex:1;margin-bottom:0!important;}
.cnpj-input-wrap input{border:2px solid #e2e8f0!important;border-radius:8px!important;padding:10px 14px!important;font-size:14px!important;font-family:'Inter',sans-serif!important;color:#0f1c2e!important;transition:border .15s!important;}
.cnpj-input-wrap input:focus{border-color:#1a6ef7!important;outline:none!important;box-shadow:0 0 0 3px rgba(26,110,247,.12)!important;}
.cnpj-btn{background:#0f1c2e;color:#fff;border:none;border-radius:8px;padding:10px 20px;font-size:13px;font-weight:700;cursor:pointer;font-family:'Inter',sans-serif;white-space:nowrap;}
.cnpj-btn:hover{background:#1a3350;}
.cnpj-erro{background:#fff1f0;border:1px solid #fca5a5;color:#991b1b;padding:10px 14px;border-radius:8px;font-size:13px;margin-top:10px;}
.cnpj-ok{background:#f0fdf4;border:1px solid #86efac;color:#166534;padding:10px 14px;border-radius:8px;font-size:13px;margin-top:10px;}
.cnpj-hint{font-size:11px;color:#9ca3af;margin-top:8px;}

/* ══ FILTER BAR ══════════════════════════════════════════════ */
.fbar{background:#fff;padding:10px 32px;border-bottom:2px solid #e8edf3;display:flex;gap:14px;align-items:center;flex-wrap:wrap;}
.fbar-lbl{font-size:11px;color:#6b7280;font-weight:700;letter-spacing:.6px;}

/* ══ CONTENT ═════════════════════════════════════════════════ */
.content{padding:20px 32px 56px;max-width:1340px;margin:0 auto;}
.sec{font-size:10px;font-weight:800;color:#6b7280;letter-spacing:1.5px;text-transform:uppercase;margin:26px 0 10px;padding-bottom:6px;border-bottom:2px solid #e5e9ef;}

/* ══ KPI CARDS ══════════════════════════════════════════════ */
.kgrid{display:grid;grid-template-columns:1.6fr repeat(5,1fr);gap:14px;margin-bottom:6px;align-items:stretch;}
@media(max-width:1200px){.kgrid{grid-template-columns:repeat(3,1fr);}}
@media(max-width:600px){.kgrid{grid-template-columns:repeat(2,1fr);}}
.kcard{background:#fff;border-radius:12px;padding:18px 20px;border:1px solid #e5e9ef;box-shadow:0 1px 4px rgba(0,0,0,.04);transition:box-shadow .15s;}
.kcard:hover{box-shadow:0 3px 12px rgba(0,0,0,.08);}
.kcard.hero{background:linear-gradient(145deg,#0a1e36 0%,#0f2d4a 100%);border:none;
  box-shadow:0 8px 32px rgba(0,30,60,.28);padding:22px 24px;}
.kcard.hero .klbl{color:rgba(255,255,255,.55);letter-spacing:1.2px;}
.kcard.hero .kval{color:#ffffff;font-size:30px;}
.kcard.hero .kval.g{color:#34d99e;}
.kcard.hero .kdelta{color:rgba(255,255,255,.45);}
.kcard.hero .kdelta.up{color:#34d99e;}
.klbl{font-size:10px;font-weight:700;color:#6b7280;letter-spacing:.9px;text-transform:uppercase;margin-bottom:7px;}
.kval{font-size:24px;font-weight:800;color:#0f1c2e;line-height:1.1;}
.kval.g{color:#00b388;}
.kdelta{font-size:11px;margin-top:6px;font-weight:600;}
.kdelta.up{color:#00b388;} .kdelta.dn{color:#e03e3e;}

/* KPI small — para diária e métricas operacionais */
.kgrid-sm{display:grid;grid-template-columns:repeat(6,1fr);gap:10px;margin-bottom:14px;}
@media(max-width:1100px){.kgrid-sm{grid-template-columns:repeat(3,1fr);}}
@media(max-width:600px){.kgrid-sm{grid-template-columns:repeat(2,1fr);}}
.kcard-sm{background:#fff;border-radius:10px;padding:14px 16px;border:1px solid #e5e9ef;box-shadow:0 1px 3px rgba(0,0,0,.04);}
.ksm-lbl{font-size:9px;font-weight:700;color:#9aa5b4;letter-spacing:.9px;text-transform:uppercase;margin-bottom:5px;}
.ksm-val{font-size:20px;font-weight:800;line-height:1.1;}

/* ══ CARDS ══════════════════════════════════════════════════ */
.card{background:#fff;border-radius:12px;padding:20px;border:1px solid #e5e9ef;box-shadow:0 1px 5px rgba(0,0,0,.04);}
.cgrid{display:grid;grid-template-columns:1fr 1fr;gap:14px;margin-bottom:6px;}
.cgrid-3{display:grid;grid-template-columns:1fr 1fr 1fr;gap:14px;margin-bottom:6px;}
@media(max-width:900px){.cgrid{grid-template-columns:1fr;}.cgrid-3{grid-template-columns:1fr 1fr;}}
@media(max-width:600px){.cgrid-3{grid-template-columns:1fr;}}
.card-hdr{display:flex;justify-content:space-between;align-items:center;margin-bottom:14px;}
.card-ttl{font-size:13px;font-weight:700;color:#1e2d3d;}
.badge{background:#f0f4f8;color:#6b7280;font-size:10px;padding:3px 9px;border-radius:12px;font-weight:700;}
.badge-blue{background:#eff6ff;color:#2563eb;}
.badge-green{background:#f0fdf4;color:#16a34a;}
.badge-orange{background:#fff7ed;color:#d97706;}
.badge-red{background:#fff1f0;color:#e03e3e;}
.badge-purple{background:#faf5ff;color:#7c3aed;}

/* ══ IMÓVEL CARDS ═══════════════════════════════════════════ */
.imovel-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(220px,1fr));gap:12px;margin-bottom:6px;}
.icard{background:#fff;border-radius:10px;padding:14px 16px;border:1px solid #e5e9ef;box-shadow:0 1px 3px rgba(0,0,0,.04);transition:box-shadow .15s;}
.icard:hover{box-shadow:0 4px 14px rgba(0,0,0,.08);}
.icard-nome{font-size:12px;font-weight:700;color:#0f1c2e;margin-bottom:2px;}
.icard-end{font-size:11px;color:#6b7280;margin-bottom:7px;}
.icard-tipo{font-size:10px;background:#f0f4f8;color:#374151;padding:2px 8px;border-radius:10px;display:inline-block;font-weight:600;margin-bottom:8px;}
.icard-plat{display:flex;gap:4px;flex-wrap:wrap;}
.plat-tag{font-size:9px;font-weight:700;padding:2px 7px;border-radius:8px;text-transform:uppercase;letter-spacing:.4px;}
.p-air{background:#fff1f0;color:#e03e3e;} .p-bk{background:#e8f4ff;color:#1a6ef7;}
.p-dc{background:#fff7e6;color:#d97706;} .p-vr{background:#f0fdf4;color:#16a34a;}
.p-ex{background:#faf5ff;color:#7c3aed;} .p-out{background:#f3f4f6;color:#6b7280;}

/* ══ CALENDÁRIO ═════════════════════════════════════════════ */
.cg{display:grid;grid-template-columns:repeat(7,1fr);gap:4px;text-align:center;}
.ch{font-size:9px;font-weight:800;color:#9aa5b4;padding:6px 0;text-transform:uppercase;}
.cd{background:#d1fae5;border-radius:8px;padding:6px 2px;border:1px solid #a7f3d0;line-height:1.5;}
.cd.v{background:#f7f9fb;border-color:#e2e8f0;}
.cd.e{background:transparent;border:none;}
.cd-n{font-size:11px;font-weight:800;color:#0f1c2e;}
.cd-v{font-size:9px;color:#059669;font-weight:700;}
.cd.v .cd-n{color:#9aa5b4;} .cd.v .cd-v{color:#c8d4de;}

/* ══ FINANCEIRO ═════════════════════════════════════════════ */
.fr{display:flex;justify-content:space-between;padding:9px 0;border-bottom:1px solid #f3f6f9;font-size:13px;}
.fr:last-child{border:none;}
.fl{color:#374151;} .fv{font-weight:700;} .fv.r{color:#e03e3e;} .fv.g{color:#00b388;font-size:15px;}
.ftotal{display:flex;justify-content:space-between;padding:12px 0 4px;font-weight:800;font-size:14px;border-top:2px solid #e5e9ef;margin-top:4px;}

/* ══ RANKING ════════════════════════════════════════════════ */
.ri{display:flex;align-items:center;gap:10px;padding:8px 0;border-bottom:1px solid #f3f6f9;}
.ri:last-child{border:none;}
.rn{width:22px;height:22px;background:#f0f4f8;border-radius:50%;display:flex;align-items:center;justify-content:center;font-size:10px;font-weight:800;color:#6b7280;flex-shrink:0;}
.rname{font-size:12px;font-weight:700;min-width:80px;flex:1;}
.rbw{flex:2;} .rb{height:9px;border-radius:3px;transition:width .4s ease;}
.b1{background:#0052cc;} .b2{background:#2684ff;} .b3{background:#79b8ff;} .b4{background:#bcd6f8;}
.rval{font-size:12px;font-weight:700;white-space:nowrap;}

/* ══ ACUMULADO ══════════════════════════════════════════════ */
.acg{display:grid;grid-template-columns:1fr 1fr;gap:14px;margin-bottom:8px;}
.al{font-size:9px;font-weight:800;color:#6b7280;letter-spacing:.9px;text-transform:uppercase;margin-bottom:3px;}
.av{font-size:22px;font-weight:800;color:#0f1c2e;} .av.g{color:#00b388;}
.ad{font-size:11px;color:#00b388;font-weight:700;}

/* ══ ANÁLISE DE DIÁRIA — intervalo entre check-ins ══════════ */
.diaria-row{display:flex;align-items:center;padding:10px 12px;border-radius:8px;background:#fafbfc;border:1px solid #e5e9ef;margin-bottom:6px;gap:12px;transition:background .1s;}
.diaria-row:hover{background:#f0f7ff;}
.diaria-datas{font-size:11px;color:#6b7280;min-width:140px;}
.diaria-datas b{color:#0f1c2e;font-size:12px;}
.diaria-noites{font-size:11px;color:#6b7280;min-width:60px;}
.diaria-val{font-size:16px;font-weight:800;color:#1a6ef7;min-width:90px;text-align:right;}
.diaria-bar-wrap{flex:1;}
.diaria-bar{height:8px;border-radius:4px;background:#1a6ef7;opacity:.7;}
.diaria-badge{font-size:9px;font-weight:700;padding:2px 8px;border-radius:10px;}
.db-alto{background:#fef3c7;color:#d97706;}
.db-medio{background:#eff6ff;color:#2563eb;}
.db-baixo{background:#f3f4f6;color:#6b7280;}

/* ══ TABELAS OPERACIONAIS ═══════════════════════════════════ */
.tab-wrap{overflow-x:auto;}
.cat-pill{display:inline-block;padding:2px 9px;border-radius:10px;font-size:10px;font-weight:700;}
.cat-energia{background:#fef9c3;color:#854d0e;}
.cat-agua{background:#dbeafe;color:#1d4ed8;}
.cat-condominio{background:#f3f4f6;color:#374151;}
.cat-internet{background:#fdf4ff;color:#7c3aed;}
.cat-limpeza{background:#d1fae5;color:#065f46;}
.cat-manutencao{background:#fff7ed;color:#c2410c;}
.cat-reposicao{background:#fce7f3;color:#9d174d;}
.cat-outros{background:#f3f4f6;color:#6b7280;}

/* status OS */
.os-status{display:inline-block;padding:2px 8px;border-radius:8px;font-size:10px;font-weight:700;}
.os-concluida{background:#d1fae5;color:#065f46;}
.os-pendente{background:#fef3c7;color:#854d0e;}
.os-em-andamento{background:#dbeafe;color:#1d4ed8;}

/* ══ EMPTY / ERROR ══════════════════════════════════════════ */
.empty-state{text-align:center;padding:80px 20px;color:#6b7280;}
.empty-state h3{font-size:20px;color:#374151;margin-bottom:8px;}
.empty-state p{font-size:14px;max-width:440px;margin:0 auto;}
.erro-dados{background:#fff7ed;border:1px solid #fdba74;color:#9a3412;padding:14px 16px;border-radius:10px;margin-bottom:16px;font-size:13px;}
.sem-dados{color:#9aa5b4;font-size:13px;padding:20px 0;text-align:center;}

/* ══ TABS (seções operacionais) ════════════════════════════ */
.op-tabs{display:flex;gap:4px;margin-bottom:16px;background:#f3f4f6;border-radius:10px;padding:4px;}
.op-tab{flex:1;text-align:center;padding:8px 12px;border-radius:7px;font-size:12px;font-weight:700;color:#6b7280;cursor:pointer;border:none;background:none;font-family:'Inter',sans-serif;transition:all .15s;}
.op-tab.active{background:#fff;color:#0f1c2e;box-shadow:0 1px 4px rgba(0,0,0,.1);}
.op-tab:hover:not(.active){background:rgba(255,255,255,.5);}

/* ══ DT OVERRIDES ════════════════════════════════════════════ */
.dataTables_wrapper .dataTables_filter input{border:1px solid #e2e8f0;border-radius:6px;padding:4px 10px;font-size:12px;}
.dataTables_wrapper .dataTables_info,.dataTables_wrapper .dataTables_paginate{font-size:12px;color:#6b7280;}
table.dataTable thead th{font-size:11px;font-weight:700;color:#6b7280;letter-spacing:.5px;text-transform:uppercase;}
table.dataTable tbody td{font-size:12px;}

/* ══ SHINY OVERRIDES ════════════════════════════════════════ */
.form-group{margin-bottom:0!important;}
label{font-size:11px!important;font-weight:700!important;color:#6b7280!important;}
.shiny-spinner-output-container{min-height:60px;}

/* ══ RECV / DET WRAPS ════════════════════════════════════════ */
.recv-wrap,.det-wrap{background:#fff;border-radius:12px;padding:20px;border:1px solid #e5e9ef;box-shadow:0 1px 5px rgba(0,0,0,.04);margin-bottom:14px;}
.recv-hdr,.det-hdr{display:flex;justify-content:space-between;align-items:center;margin-bottom:16px;}
.recv-ttl,.det-ttl{font-size:14px;font-weight:700;color:#1e2d3d;}
    "))
  ),
  
  # ── Header ──────────────────────────────────────────────────
  div(class = "hdr",
      div(class = "hdr-left",
          tags$img(
            src   = "assets/marca_BSB_STAY_RS_10.jpg",
            alt   = "BSB Stay",
            class = "hdr-logo-img"
          ),
          div(div(class = "hdr-title", "Extrato do Proprietário"),
              div(class = "hdr-sub",   "Painel de acompanhamento de resultados"))
      ),
      uiOutput("hdr_prop")
  ),
  uiOutput("sync_bar"),
  
  uiOutput("filter_bar"),
  
  div(class = "content",
      uiOutput("alerta_erro"),
      uiOutput("body")
  )
)

# ═══════════════════════════════════════════════════════════════
# SERVER
# ═══════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  # APP_DATA_GLOBAL foi carregado uma vez em run.R (boot-time).
  # Cada sessão começa com o mesmo snapshot — atualizado pelo botão Sync.
  rv <- reactiveValues(
    app_data    = APP_DATA_GLOBAL,
    syncing     = FALSE,
    sync_status = "ok",
    last_sync   = {
      st <- tryCatch(status_cache(), error = function(e) list(last_sync = NA))
      st$last_sync
    },
    op_aba = "despesas"
  )
  
  observeEvent(input$btn_aba_op, { rv$op_aba <- input$btn_aba_op }, ignoreInit = TRUE)
  
  # ── Reactives base ──────────────────────────────────────────
  dados <- reactive({
    req(identical(session$userData$auth_role, "owner"))
    doc   <- as.character(session$userData$auth_doc)
    chaves <- gsub("[^0-9]", "", names(rv$app_data))
    idx    <- which(chaves == gsub("[^0-9]", "", doc))
    req(length(idx) > 0)
    rv$app_data[[idx[1]]]
  })
  
  meses_disponiveis <- reactive({
    d <- dados(); req(d)
    if (is.null(d$receitas) || nrow(d$receitas) == 0) return(character(0))
    sort(unique(d$receitas$competencia[!is.na(d$receitas$competencia)]), decreasing = TRUE)
  })
  
  rec_fil <- reactive({
    d <- dados(); req(d)
    df <- d$receitas
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- df |> dplyr::filter(imovel == input$imovel)
    df
  })
  
  rm <- reactive({
    req(input$mes_sel)
    df_mes <- rec_fil() |> dplyr::filter(competencia == input$mes_sel)
    n_im <- dplyr::n_distinct(df_mes$imovel)
    out  <- df_mes |>
      dplyr::summarise(
        receita_bruta = sum(receita_bruta, na.rm = TRUE),
        taxa_adm      = sum(taxa_adm,      na.rm = TRUE),
        outros_custos = sum(outros_custos, na.rm = TRUE),
        resultado_liq = sum(resultado_liq, na.rm = TRUE),
        ocupacao      = mean(ocupacao,     na.rm = TRUE),
        diaria_media  = mean(diaria_media, na.rm = TRUE),
        n_diarias     = sum(n_diarias,     na.rm = TRUE),
        .groups = "drop"
      )
    out$n_imoveis <- n_im
    out$revpar    <- if (!is.na(n_im) && n_im > 0) out$receita_bruta / n_im else NA_real_
    out
  })
  
  # Filtra reservas por mês e imóvel
  reservas_fil <- reactive({
    d <- dados(); req(d, input$mes_sel)
    if (is.null(d$reservas) || nrow(d$reservas) == 0) return(data.frame())
    df <- d$reservas |> dplyr::filter(competencia == input$mes_sel)
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- df |> dplyr::filter(imovel_nome == input$imovel)
    df |> dplyr::arrange(checkin)
  })
  
  # Filtra manutenção
  manutencao_fil <- reactive({
    d <- dados(); req(d, input$mes_sel)
    if (is.null(d$manutencao) || nrow(d$manutencao) == 0) return(data.frame())
    df <- d$manutencao |> dplyr::filter(competencia == input$mes_sel)
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- df |> dplyr::filter(imovel_nome == input$imovel)
    df
  })
  
  # Filtra despesas
  despesas_fil <- reactive({
    d <- dados(); req(d, input$mes_sel)
    if (is.null(d$despesas) || nrow(d$despesas) == 0) return(data.frame())
    df <- d$despesas |> dplyr::filter(competencia == input$mes_sel)
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- df |> dplyr::filter(imovel_nome == input$imovel)
    df
  })
  
  # Filtra reposição
  reposicao_fil <- reactive({
    d <- dados(); req(d, input$mes_sel)
    if (is.null(d$reposicao) || nrow(d$reposicao) == 0) return(data.frame())
    df <- d$reposicao |> dplyr::filter(competencia == input$mes_sel)
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- df |> dplyr::filter(imovel_nome == input$imovel)
    df
  })
  
  # ── Sync ──────────────────────────────────────────────────────
  
  # ── Sync ──────────────────────────────────────────────────────
  output$sync_bar <- renderUI({
    dot_class <- paste("sync-dot", rv$sync_status)
    msg_txt <- if (rv$syncing) "\u23f3 Sincronizando..." else if (!is.na(rv$last_sync))
      paste0("\u2713 \u00daltima sincroniza\u00e7\u00e3o: ", rv$last_sync) else "\u26a0 Dados n\u00e3o sincronizados"
    div(class = "sync-bar",
        div(class = dot_class), span(msg_txt),
        tags$button(class = "sync-btn",
                    onclick = "Shiny.setInputValue(\'btn_sync\', Math.random())",
                    if (rv$syncing) "\u23f3 Aguarde..." else "\u21bb Atualizar dados"))
  })
  
  observeEvent(input$btn_sync, {
    rv$syncing <- TRUE
    tryCatch({
      nd <- carregar_dados_app(
        file_id    = DRIVE_FILE_ID,
        folder_id  = DRIVE_FOLDER_ID,
        forcar_dl  = TRUE,
        forcar_etl = TRUE
      )
      rv$app_data    <- nd
      rv$last_sync   <- format(Sys.time(), "%d/%m/%Y %H:%M")
      rv$sync_status <- "ok"
      # Atualiza o cache global para que novas sessões também se beneficiem
      APP_DATA_GLOBAL <<- nd
      showNotification("✓ Dados atualizados!", type = "message", duration = 4)
    }, error = function(e) {
      rv$sync_status <- "err"
      showNotification(paste("⚠ Falha:", e$message), type = "error", duration = 6)
    })
    rv$syncing <- FALSE
  }, ignoreInit = TRUE)
  
  # ── Header prop ───────────────────────────────────────────────
  
  # ── Header ────────────────────────────────────────────────────
  output$hdr_prop <- renderUI({
    d <- dados(); req(d)
    nome <- session$userData$auth_owner_name %||% d$proprietario %||% "Proprietário"
    tipo <- session$userData$auth_doc_type   %||% "CPF/CNPJ"
    div(style = "display:flex;align-items:center;gap:14px;",
        div(class = "hdr-prop", tags$b(nome), br(), div(class = "hdr-badge", tipo)),
        tags$button(
          style = paste0("background:rgba(255,255,255,.08);border:1px solid rgba(255,255,255,.18);",
                         "color:#c9dff2;border-radius:8px;padding:6px 12px;font-size:11px;",
                         "font-weight:700;cursor:pointer;font-family:'Inter',sans-serif;",
                         "white-space:nowrap;transition:background .15s;"),
          onclick = "Shiny.setInputValue('nav_alterar_senha', Math.random())",
          "🔑 Alterar Senha")
    )
  })
  
  # ── Filter bar ────────────────────────────────────────────────
  output$filter_bar <- renderUI({
    d <- dados(); req(d, length(meses_disponiveis()) > 0)
    meses <- meses_disponiveis()
    meses_lbl <- setNames(meses, {
      datas <- suppressWarnings(as.Date(paste0(meses, "-01")))
      ifelse(is.na(datas), meses, format(datas, "%B/%Y"))
    })
    imoveis <- c("Todos os im\u00f3veis" = "all", setNames(d$imoveis_ids, d$imoveis_ids))
    div(class = "fbar",
        div(class = "fbar-lbl", "M\u00cAS:"),
        selectInput("mes_sel", NULL, choices = meses_lbl, selected = meses[1], width = "180px"),
        div(class = "fbar-lbl", "IM\u00d3VEL:"),
        selectInput("imovel",  NULL, choices = imoveis,   selected = "all",    width = "260px"))
  })
  
  output$alerta_erro <- renderUI({
    msg <- attr(rv$app_data, "erro_msg")
    if (!is.null(msg)) div(class = "erro-dados", tags$b("\u26a0 Aten\u00e7\u00e3o: "), msg)
  })
  
  output$body <- renderUI({
    d <- dados()
    if (is.null(d)) {
      return(div(class = "empty-state",
                 h3("⏳ Carregando dados..."),
                 p("Aguarde enquanto os dados são preparados.")))
    }
    req(input$mes_sel)
    m <- isolate(rm())
    mes_label_sel <- {
      dt <- suppressWarnings(as.Date(paste0(input$mes_sel, "-01")))
      if (!is.na(dt)) format(dt, "%B/%Y") else input$mes_sel
    }
    mes_badge_sm <- {
      dt <- suppressWarnings(as.Date(paste0(input$mes_sel, "-01")))
      if (!is.na(dt)) format(dt, "%b %Y") else input$mes_sel
    }
    
    tagList(
      # ════════════════════════════════════════
      # 1. RESULTADOS DO MÊS — KPIs principais
      # ════════════════════════════════════════
      div(class = "sec", "RESULTADOS DO MÊS"),
      div(class = "kgrid",
          # Resultado Líquido — card principal em destaque
          kcard("Resultado Líquido", brl(m$resultado_liq),
                paste0("receita: ", brl(m$receita_bruta)),
                vg = TRUE, extra_class = "hero"),
          kcard("Receita Bruta",  brl(m$receita_bruta),  "receita do período"),
          kcard("RevPAR",
                brl(if (!is.na(m$revpar)) m$revpar else 0),
                paste0(m$n_imoveis, " imóvel(is) no filtro"), icon = "📈"),
          kcard("Taxa Adm.",      brl(m$taxa_adm),       "20% da receita",   dn = TRUE),
          kcard("Outros Custos",  brl(m$outros_custos),  "fixa + variável",  dn = TRUE),
          kcard("Ocupação",       paste0(round(m$ocupacao), "%"),
                paste0("Diária média: ", brl(m$diaria_media)))
      ),
      
      # ════════════════════════════════════════
      # 2. PORTFÓLIO
      # ════════════════════════════════════════
      div(class = "sec", "PORTFÓLIO DE IMÓVEIS"),
      {
        cfg_lst <- d$imoveis_cfg
        if (length(cfg_lst) == 0) p("Nenhum imóvel cadastrado.")
        else div(class = "imovel-grid",
                 lapply(cfg_lst, function(im) {
                   plats <- strsplit(as.character(im$plataformas %||% ""), "/|,| ")[[1]]
                   plats <- trimws(plats[nzchar(trimws(plats))])
                   plat_tags <- lapply(plats, function(p) {
                     cl <- switch(tolower(p),
                                  "airbnb"="plat-tag p-air","booking"=,"bookingcom"="plat-tag p-bk",
                                  "decolar"="plat-tag p-dc","vrbo"="plat-tag p-vr",
                                  "expedia"="plat-tag p-ex","plat-tag p-out")
                     div(class = cl, p)
                   })
                   div(class = "icard",
                       div(class = "icard-nome", as.character(im$nome %||% im$id)),
                       div(class = "icard-end",  as.character(im$bairro %||% "")),
                       div(class = "icard-tipo", as.character(im$tipo %||% "")),
                       div(class = "icard-plat", !!!plat_tags))
                 }))
      },
      
      # ════════════════════════════════════════
      # 3. ANÁLISE DE RECEITA (gráfico diária/dia)
      # ════════════════════════════════════════
      uiOutput("sec_analise_receita"),
      
      # ════════════════════════════════════════
      # 4. DETALHAMENTO DO MÊS — calendário
      # ════════════════════════════════════════
      uiOutput("sec_detalhamento_mes"),
      
      # Resultado + Custos + Ranking
      div(class = "cgrid",
          div(class = "card",
              div(class = "card-hdr",
                  div(class = "card-ttl", "Resultado Financeiro"),
                  span(class = "badge", "Após deduções")),
              uiOutput("resultado"),
              div(style = "margin-top:16px;",
                  div(class = "card-hdr",
                      div(class = "card-ttl", "Custos do Mês"),
                      span(class = "badge", "Discriminado")),
                  shinycssloaders::withSpinner(DTOutput("t_custos"), type = 4, color = "#00c49a"))
          ),
          div(class = "card",
              div(class = "card-hdr",
                  div(class = "card-ttl", "Ranking de Imóveis"),
                  span(class = "badge", "Receita no mês")),
              uiOutput("ranking"))
      ),
      
      # ════════════════════════════════════════
      # 5. ANÁLISE DA DIÁRIA ENTRE CHECK-INS
      # ════════════════════════════════════════
      div(class = "sec", "ANÁLISE DA DIÁRIA ENTRE CHECK-INS"),
      
      # KPIs da diária
      shinycssloaders::withSpinner(uiOutput("kpis_diaria"), type = 4, color = "#1a6ef7"),
      
      # Gráfico + lista de intervalos
      div(class = "cgrid",
          div(class = "card",
              div(class = "card-hdr",
                  div(class = "card-ttl", "Evolução da Diária por Reserva"),
                  span(class = "badge badge-blue", "Nível de reserva")),
              shinycssloaders::withSpinner(
                plotlyOutput("g_diaria_reservas", height = "240px"), type = 4, color = "#1a6ef7")
          ),
          div(class = "card",
              div(class = "card-hdr",
                  div(class = "card-ttl", "Intervalos entre Check-ins"),
                  span(class = "badge badge-blue", mes_badge_sm)),
              uiOutput("lista_diarias"))
      ),
      
      # ════════════════════════════════════════
      # 6. OPERACIONAL: Despesas | Custos | OS
      # ════════════════════════════════════════
      div(class = "sec", "OPERACIONAL"),
      
      div(class = "card",
          # Tabs de navegação
          div(class = "op-tabs",
              tags$button(
                class = paste("op-tab", if (rv$op_aba == "despesas") "active" else ""),
                onclick = "Shiny.setInputValue('btn_aba_op', 'despesas', {priority: 'event'})",
                "💰 Despesas"
              ),
              tags$button(
                class = paste("op-tab", if (rv$op_aba == "custos") "active" else ""),
                onclick = "Shiny.setInputValue('btn_aba_op', 'custos', {priority: 'event'})",
                "🏠 Custos por Apartamento"
              ),
              tags$button(
                class = paste("op-tab", if (rv$op_aba == "os") "active" else ""),
                onclick = "Shiny.setInputValue('btn_aba_op', 'os', {priority: 'event'})",
                "🔧 Ordens de Serviço"
              ),
              tags$button(
                class = paste("op-tab", if (rv$op_aba == "reposicao") "active" else ""),
                onclick = "Shiny.setInputValue('btn_aba_op', 'reposicao', {priority: 'event'})",
                "📦 Reposição"
              )
          ),
          uiOutput("painel_operacional")
      ),
      
      # ════════════════════════════════════════
      # 7. ANÁLISE TEMPORAL
      # ════════════════════════════════════════
      div(class = "sec", "ANÁLISE TEMPORAL"),
      div(class = "cgrid",
          div(class = "card",
              div(class = "card-hdr",
                  div(class = "card-ttl", "Diárias por Dia"),
                  span(class = "badge", "Valores realizados")),
              shinycssloaders::withSpinner(plotlyOutput("g_diarias", height = "200px"), type = 4, color = "#00c49a")
          ),
          div(class = "card",
              div(class = "card-hdr",
                  div(class = "card-ttl", "Evolução 12 Meses"),
                  span(class = "badge", "Receita + Resultado")),
              shinycssloaders::withSpinner(plotlyOutput("g_evolucao", height = "200px"), type = 4, color = "#00c49a")
          )
      ),
      
      # ════════════════════════════════════════
      # 8. VISÃO GERAL + HISTÓRICO
      # ════════════════════════════════════════
      div(class = "sec", "VISÃO GERAL DA CARTEIRA"),
      div(class = "card",
          div(class = "card-hdr", div(class = "card-ttl", "Acumulado do Ano"),
              span(class = "badge", "Acumulado até o período")),
          uiOutput("acumulado")),
      
      div(class = "sec", "HISTÓRICO DETALHADO"),
      div(class = "card",
          shinycssloaders::withSpinner(DTOutput("t_historico"), type = 4, color = "#00c49a"))
    )
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Gráfico diária por dia (calendário)
  # ═══════════════════════════════════════════════════════════
  output$g_diaria_dia <- renderPlotly({
    d <- dados(); req(d, input$mes_sel)
    iid <- if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      input$imovel else if (length(d$imoveis_ids) > 0) d$imoveis_ids[[1]] else return(NULL)
    cal <- d$calendario
    if (is.null(cal) || nrow(cal) == 0) validate(need(FALSE, "Sem dados diários."))
    cal <- cal |>
      dplyr::filter(apto_original == iid | property_id == iid,
                    substr(as.character(data), 1, 7) == input$mes_sel) |>
      dplyr::arrange(data)
    validate(need(nrow(cal) > 0, "Sem dados para o período."))
    plot_ly(cal, x = ~as.Date(data), y = ~valor, type = "scatter", mode = "lines+markers",
            line   = list(color = "#1a6ef7", width = 2.5),
            marker = list(color = "#1a6ef7", size = 7),
            hovertemplate = "Dia %{x|%d/%m}<br>R$ %{y:,.0f}<extra></extra>") |>
      layout(paper_bgcolor="transparent", plot_bgcolor="transparent",
             xaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,
                        tickformat="%d/%m",tickfont=list(size=10,color="#6b7280"),title=""),
             yaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,
                        tickprefix="R$ ",tickfont=list(size=10,color="#6b7280"),title=""),
             margin=list(l=55,r=12,t=8,b=32),showlegend=FALSE) |>
      config(displayModeBar = FALSE)
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Calendário de ocupação v2
  # ═══════════════════════════════════════════════════════════
  output$calendario_v2 <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    iid <- if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      input$imovel else if (length(d$imoveis_ids) > 0) d$imoveis_ids[[1]] else return(p("Sem dados."))
    cal <- d$calendario
    if (is.null(cal) || nrow(cal) == 0) return(p(class = "sem-dados", "Sem dados de ocupação."))
    cal <- cal |>
      dplyr::filter(apto_original == iid | property_id == iid,
                    substr(as.character(data), 1, 7) == input$mes_sel) |>
      dplyr::arrange(data)
    if (nrow(cal) == 0) return(p(class = "sem-dados", "Sem dados para o período."))
    mes_inicio <- as.Date(paste0(input$mes_sel, "-01"))
    mes_fim    <- lubridate::ceiling_date(mes_inicio, "month") - 1
    cal_full   <- data.frame(data = seq.Date(mes_inicio, mes_fim, by = "day")) |>
      dplyr::left_join(cal |> dplyr::select(data, valor, ocupado), by = "data") |>
      dplyr::mutate(ocupado = dplyr::coalesce(ocupado, FALSE), valor = dplyr::coalesce(valor, 0))
    hdrs   <- lapply(c("DOM","SEG","TER","QUA","QUI","SEX","SÁB"), function(x) div(class="ch", x))
    prm    <- as.integer(format(mes_inicio, "%w"))
    vazios <- if (prm > 0) lapply(seq_len(prm), function(i) div(class="cd e")) else list()
    dias   <- lapply(seq_len(nrow(cal_full)), function(i) {
      r <- cal_full[i,]
      div(class = if (isTRUE(r$ocupado)) "cd" else "cd v",
          div(class="cd-n", as.integer(format(as.Date(r$data),"%d"))),
          div(class="cd-v", if (r$valor>0) paste0("R$",format(round(r$valor),big.mark=".")) else "—"))
    })
    div(class="cg", !!!c(hdrs, vazios, dias))
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Seção Análise de Receita (condicional — oculta se sem dados)
  # ═══════════════════════════════════════════════════════════
  output$sec_analise_receita <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    iid <- if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      input$imovel else if (length(d$imoveis_ids) > 0) d$imoveis_ids[[1]] else return(NULL)
    cal <- d$calendario
    if (is.null(cal) || nrow(cal) == 0) return(NULL)
    cal_fil <- cal |>
      dplyr::filter(apto_original == iid | property_id == iid,
                    substr(as.character(data), 1, 7) == input$mes_sel)
    if (nrow(cal_fil) == 0) return(NULL)
    mes_label_sel <- {
      dt <- suppressWarnings(as.Date(paste0(input$mes_sel, "-01")))
      if (!is.na(dt)) format(dt, "%B/%Y") else input$mes_sel
    }
    tagList(
      div(class = "sec", "ANÁLISE DE RECEITA"),
      div(class = "recv-wrap",
          div(class = "recv-hdr",
              div(class = "recv-ttl", "Valor da Diária por Dia"),
              span(class = "badge badge-blue", mes_label_sel)
          ),
          shinycssloaders::withSpinner(plotlyOutput("g_diaria_dia", height = "220px"), type = 4, color = "#1a6ef7")
      )
    )
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Seção Detalhamento do Mês (condicional — oculta se sem dados)
  # ═══════════════════════════════════════════════════════════
  output$sec_detalhamento_mes <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    iid <- if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      input$imovel else if (length(d$imoveis_ids) > 0) d$imoveis_ids[[1]] else return(NULL)
    cal <- d$calendario
    if (is.null(cal) || nrow(cal) == 0) return(NULL)
    cal_fil <- cal |>
      dplyr::filter(apto_original == iid | property_id == iid,
                    substr(as.character(data), 1, 7) == input$mes_sel)
    if (nrow(cal_fil) == 0) return(NULL)
    mes_badge_sm <- {
      dt <- suppressWarnings(as.Date(paste0(input$mes_sel, "-01")))
      if (!is.na(dt)) format(dt, "%b %Y") else input$mes_sel
    }
    tagList(
      div(class = "sec", "DETALHAMENTO DO MÊS"),
      div(class = "det-wrap",
          div(class = "det-hdr",
              div(class = "det-ttl", "Calendário de Ocupação"),
              span(class = "badge badge-green", mes_badge_sm)
          ),
          shinycssloaders::withSpinner(uiOutput("calendario_v2"), type = 4, color = "#00c49a")
      )
    )
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: KPIs da Diária entre Check-ins
  # ═══════════════════════════════════════════════════════════
  output$kpis_diaria <- renderUI({
    df <- reservas_fil()
    if (nrow(df) == 0) return(p(class = "sem-dados", "Sem reservas para o período selecionado."))
    
    d_media  <- mean(df$diaria_liquida, na.rm = TRUE)
    d_max    <- max(df$diaria_liquida,  na.rm = TRUE)
    d_min    <- min(df$diaria_liquida[df$diaria_liquida > 0], na.rm = TRUE)
    d_var    <- if (!is.na(d_max) && !is.na(d_min) && d_min > 0)
      paste0("+", round((d_max/d_min - 1)*100), "%") else "—"
    n_int    <- nrow(df)                       # nº de intervalos/reservas
    ticket   <- mean(df$receita_total, na.rm = TRUE)  # ticket médio por reserva
    
    div(class = "kgrid-sm",
        kcard_sm("Diária Média",      brl(d_media), "blue"),
        kcard_sm("Maior Diária",      brl(d_max),   "green"),
        kcard_sm("Menor Diária",      brl(d_min),   "orange"),
        kcard_sm("Variação no Mês",   d_var,         "purple"),
        kcard_sm("Ticket Médio/Res.", brl(ticket),  "blue"),
        kcard_sm("Nº de Intervalos",  as.character(n_int), "orange")
    )
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Gráfico de diária por reserva (scatter/line)
  # Lógica: cada ponto = 1 reserva; x = checkin; y = diária_liquida
  # Permite ver como a diária varia de reserva para reserva no mês
  # ═══════════════════════════════════════════════════════════
  output$g_diaria_reservas <- renderPlotly({
    df <- reservas_fil()
    validate(need(nrow(df) > 0, "Sem reservas para o período."))
    
    df <- df |>
      dplyr::mutate(
        label = paste0(format(checkin, "%d/%m"), " → ", format(checkout, "%d/%m"),
                       "\n", as.integer(checkout - checkin), " noites",
                       "\nDiária: R$ ", round(diaria_liquida))
      ) |>
      dplyr::arrange(checkin)
    
    d_media <- mean(df$diaria_liquida, na.rm = TRUE)
    
    plot_ly() |>
      add_bars(data = df,
               x = ~checkin, y = ~diaria_liquida,
               marker = list(
                 color = ~diaria_liquida,
                 colorscale = list(c(0,"#bcd6f8"), c(0.5,"#2684ff"), c(1,"#0052cc")),
                 showscale = FALSE,
                 line = list(color = "transparent")
               ),
               text = ~label,
               hovertemplate = "%{text}<extra></extra>",
               name = "Diária"
      ) |>
      add_lines(x = range(df$checkin, na.rm = TRUE),
                y = c(d_media, d_media),
                line = list(color = "#e03e3e", dash = "dash", width = 1.5),
                name = paste0("Média: R$ ", round(d_media)),
                hoverinfo = "skip"
      ) |>
      layout(
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        xaxis = list(showgrid=FALSE, zeroline=FALSE, tickformat="%d/%m",
                     tickfont=list(size=10,color="#6b7280"), title=""),
        yaxis = list(showgrid=TRUE, gridcolor="#f3f6f9", zeroline=FALSE,
                     tickprefix="R$ ", tickfont=list(size=10,color="#6b7280"), title=""),
        margin = list(l=55, r=12, t=8, b=32),
        showlegend = TRUE,
        legend = list(x=0, y=1.12, orientation="h", font=list(size=10))
      ) |>
      config(displayModeBar = FALSE)
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Lista visual de intervalos entre check-ins
  # Mostra cada reserva como um card horizontal com barra de valor
  # ═══════════════════════════════════════════════════════════
  output$lista_diarias <- renderUI({
    df <- reservas_fil()
    if (nrow(df) == 0) return(p(class = "sem-dados", "Sem reservas para o período."))
    
    df <- df |> dplyr::arrange(checkin)
    d_max <- max(df$diaria_liquida, na.rm = TRUE)
    # Percentil 75 para classificar como "alto"
    p75 <- quantile(df$diaria_liquida, 0.75, na.rm = TRUE)
    p25 <- quantile(df$diaria_liquida, 0.25, na.rm = TRUE)
    
    items <- lapply(seq_len(min(nrow(df), 15)), function(i) {
      r      <- df[i,]
      noites <- as.integer(r$checkout - r$checkin)
      pct    <- round(r$diaria_liquida / d_max * 100)
      badge_cl <- if (r$diaria_liquida >= p75) "diaria-badge db-alto"
      else if (r$diaria_liquida >= p25) "diaria-badge db-medio"
      else "diaria-badge db-baixo"
      badge_lbl <- if (r$diaria_liquida >= p75) "Alta" else if (r$diaria_liquida >= p25) "Média" else "Baixa"
      
      div(class = "diaria-row",
          div(class = "diaria-datas",
              tags$b(paste0(format(r$checkin, "%d/%m"), " → ", format(r$checkout, "%d/%m"))),
              br(), span(class = "diaria-noites", paste0(noites, " noite", if(noites>1)"s" else ""))
          ),
          div(class = "diaria-bar-wrap",
              div(class = "diaria-bar", style = paste0("width:", pct, "%;"))
          ),
          div(class = "diaria-val", brl(r$diaria_liquida)),
          div(class = badge_cl, badge_lbl)
      )
    })
    
    note <- if (nrow(df) > 15) p(style="font-size:11px;color:#9aa5b4;margin-top:8px;text-align:center;",
                                 paste0("Exibindo 15 de ", nrow(df), " reservas")) else NULL
    tagList(!!!items, note)
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Painel operacional (tabs: despesas | custos | OS)
  # ═══════════════════════════════════════════════════════════
  output$painel_operacional <- renderUI({
    aba <- rv$op_aba %||% "despesas"
    
    if (aba == "despesas") {
      # ── ABA: Despesas ──────────────────────────────────────
      tagList(
        # KPIs de despesas
        shinycssloaders::withSpinner(uiOutput("kpis_despesas"), type = 4, color = "#d97706"),
        # Gráfico por categoria + tabela
        div(class = "cgrid",
            div(class = "card",
                div(class = "card-hdr",
                    div(class = "card-ttl", "Despesas por Categoria"),
                    span(class = "badge badge-orange", "Distribuição")),
                shinycssloaders::withSpinner(
                  plotlyOutput("g_despesas_cat", height = "240px"), type = 4, color = "#d97706")
            ),
            div(class = "card",
                div(class = "card-hdr",
                    div(class = "card-ttl", "Despesas por Apartamento"),
                    span(class = "badge badge-orange", "Comparativo")),
                shinycssloaders::withSpinner(
                  plotlyOutput("g_despesas_apto", height = "240px"), type = 4, color = "#d97706")
            )
        ),
        div(class = "card", style = "margin-top:14px;",
            div(class = "card-hdr",
                div(class = "card-ttl", "Tabela de Despesas"),
                span(class = "badge badge-orange", "Detalhada")),
            div(class = "tab-wrap",
                shinycssloaders::withSpinner(DTOutput("t_despesas"), type = 4, color = "#d97706"))
        )
      )
      
    } else if (aba == "custos") {
      # ── ABA: Custos por Apartamento ────────────────────────
      tagList(
        shinycssloaders::withSpinner(uiOutput("kpis_custos"), type = 4, color = "#7c3aed"),
        div(class = "card", style = "margin-top:0;",
            div(class = "card-hdr",
                div(class = "card-ttl", "Evolução de Custos por Apartamento"),
                span(class = "badge badge-purple", "Histórico")),
            shinycssloaders::withSpinner(
              plotlyOutput("g_custos_apto", height = "260px"), type = 4, color = "#7c3aed")
        ),
        div(class = "card", style = "margin-top:14px;",
            div(class = "card-hdr",
                div(class = "card-ttl", "Custos Detalhados por Apartamento"),
                span(class = "badge badge-purple", "Mês selecionado")),
            div(class = "tab-wrap",
                shinycssloaders::withSpinner(DTOutput("t_custos_apto"), type = 4, color = "#7c3aed"))
        )
      )
      
    } else if (aba == "reposicao") {
      # ── ABA: Reposição ────────────────────────────────────
      tagList(
        shinycssloaders::withSpinner(uiOutput("kpis_reposicao"), type = 4, color = "#0891b2"),
        div(class = "card", style = "margin-top:0;",
            div(class = "card-hdr",
                div(class = "card-ttl", "Itens de Reposição"),
                span(class = "badge badge-teal", "Mês selecionado")),
            div(class = "tab-wrap",
                shinycssloaders::withSpinner(DTOutput("t_reposicao"), type = 4, color = "#0891b2"))
        )
      )
    } else {
      # ── ABA: Ordens de Serviço ────────────────────────────
      tagList(
        shinycssloaders::withSpinner(uiOutput("kpis_os"), type = 4, color = "#0052cc"),
        div(class = "card", style = "margin-top:0;",
            div(class = "card-hdr",
                div(class = "card-ttl", "Ordens de Serviço"),
                span(class = "badge badge-blue", "Mês selecionado")),
            div(class = "tab-wrap",
                shinycssloaders::withSpinner(DTOutput("t_os"), type = 4, color = "#0052cc"))
        )
      )
    }
  })
  
  
  # ── KPIs Despesas ────────────────────────────────────────────
  output$kpis_despesas <- renderUI({
    des <- despesas_fil(); rep <- reposicao_fil()
    total_des <- if (nrow(des) > 0) sum(des$valor, na.rm = TRUE) else 0
    total_rep <- if (nrow(rep) > 0) sum(rep$valor_unitario_ou_total, na.rm = TRUE) else 0
    n_cat     <- if (nrow(des) > 0 && "categoria" %in% names(des)) length(unique(des$categoria)) else 0
    n_apto    <- if (nrow(des) > 0 && "imovel_nome" %in% names(des)) length(unique(des$imovel_nome)) else 0
    maior_cat <- if (nrow(des) > 0 && "categoria" %in% names(des)) {
      tc <- des |> dplyr::group_by(categoria) |> dplyr::summarise(v=sum(valor,na.rm=TRUE),.groups="drop") |> dplyr::arrange(dplyr::desc(v))
      if (nrow(tc) > 0) tc$categoria[1] else "—"
    } else "—"
    
    div(class = "kgrid-sm",
        kcard_sm("Total Despesas",  brl(total_des),   "orange"),
        kcard_sm("Total Reposição", brl(total_rep),   "purple"),
        kcard_sm("Nº Categorias",   as.character(n_cat), "orange"),
        kcard_sm("Aptos Afetados",  as.character(n_apto), "blue"),
        kcard_sm("Maior Categoria", maior_cat,         "red"),
        kcard_sm("Total Operac.",   brl(total_des + total_rep), "green")
    )
  })
  
  # ── Gráfico despesas por categoria (pizza) ───────────────────
  output$g_despesas_cat <- renderPlotly({
    des <- despesas_fil()
    validate(need(nrow(des) > 0 && "categoria" %in% names(des), "Sem dados de despesas."))
    df <- des |>
      dplyr::group_by(categoria) |>
      dplyr::summarise(total = sum(valor, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(total))
    cores <- c("#0052cc","#2684ff","#d97706","#7c3aed","#e03e3e","#00b388","#f59e0b","#9ca3af")
    plot_ly(df, labels = ~categoria, values = ~total, type = "pie",
            marker = list(colors = cores, line = list(color = "#fff", width = 2)),
            textinfo = "percent", textposition = "inside",
            hovertemplate = "%{label}<br>R$ %{value:,.0f}<extra></extra>") |>
      layout(paper_bgcolor="transparent", plot_bgcolor="transparent",
             showlegend=TRUE,
             legend=list(font=list(size=10), orientation="v", x=1.02, y=0.5),
             margin=list(l=0,r=120,t=10,b=10),
             autosize=TRUE) |>
      config(displayModeBar = FALSE)
  })
  
  # ── Gráfico despesas por apartamento (barras) ─────────────────
  output$g_despesas_apto <- renderPlotly({
    des <- despesas_fil()
    validate(need(nrow(des) > 0 && "imovel_nome" %in% names(des), "Sem dados de despesas."))
    df <- des |>
      dplyr::group_by(imovel_nome) |>
      dplyr::summarise(total = sum(valor, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(total)
    plot_ly(df, x = ~total, y = ~imovel_nome, type = "bar", orientation = "h",
            marker = list(color = "#d97706",
                          line = list(color = "transparent")),
            hovertemplate = "%{y}<br>R$ %{x:,.0f}<extra></extra>") |>
      layout(paper_bgcolor="transparent", plot_bgcolor="transparent",
             xaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,
                        tickprefix="R$ ",tickfont=list(size=10),title=""),
             yaxis=list(showgrid=FALSE,zeroline=FALSE,tickfont=list(size=10),title=""),
             margin=list(l=10,r=12,t=8,b=20),showlegend=FALSE) |>
      config(displayModeBar = FALSE)
  })
  
  # ── Tabela despesas ───────────────────────────────────────────
  output$t_despesas <- renderDT({
    des <- despesas_fil()
    validate(need(nrow(des) > 0, "Sem despesas para o período/imóvel selecionado."))
    
    # Colunas disponíveis com fallback
    cols_base <- c("imovel_nome", "categoria", "descricao", "data", "competencia", "valor")
    cols_ok   <- cols_base[cols_base %in% names(des)]
    df <- des |>
      dplyr::select(dplyr::all_of(cols_ok)) |>
      dplyr::rename_with(~ dplyr::recode(.,
                                         imovel_nome = "Imóvel", categoria = "Categoria", descricao = "Descrição",
                                         data = "Data", competencia = "Competência", valor = "Valor (R$)")) |>
      dplyr::mutate(dplyr::across(dplyr::any_of("Valor (R$)"), ~ brl(.x)))
    
    datatable(df,
              options = list(pageLength = 10, dom = "ftip",
                             language = list(search = "Buscar:", paginate = list(previous="Ant.", `next`="Próx."))),
              rownames = FALSE, class = "compact stripe hover",
              escape = FALSE)
  }, server = FALSE)
  
  # ── KPIs Custos por Apartamento ──────────────────────────────
  output$kpis_custos <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    rec <- rec_fil() |> dplyr::filter(competencia == input$mes_sel)
    if (nrow(rec) == 0) return(p(class="sem-dados","Sem dados."))
    
    total_custo <- sum(rec$outros_custos, na.rm = TRUE)
    apto_maior  <- rec |> dplyr::arrange(dplyr::desc(outros_custos)) |> dplyr::slice(1)
    n_apto      <- length(unique(rec$imovel))
    custo_med   <- mean(rec$outros_custos, na.rm = TRUE)
    custo_r_rec <- if (sum(rec$receita_bruta,na.rm=TRUE) > 0)
      paste0(round(total_custo/sum(rec$receita_bruta,na.rm=TRUE)*100), "%") else "—"
    
    div(class = "kgrid-sm",
        kcard_sm("Total Custos",       brl(total_custo),          "purple"),
        kcard_sm("Custo/Apto Médio",   brl(custo_med),            "blue"),
        kcard_sm("Aptos com Custo",    as.character(n_apto),      "orange"),
        kcard_sm("Custo/Receita",      custo_r_rec,               "red"),
        kcard_sm("Apto c/ + Custo",    as.character(apto_maior$imovel[1] %||% "—"), "purple"),
        kcard_sm("Custo Maior Apto",   brl(apto_maior$outros_custos[1]), "red")
    )
  })
  
  # ── Gráfico custos por apartamento (barras agrupadas) ────────
  output$g_custos_apto <- renderPlotly({
    d <- dados(); req(d)
    # Usa todos os meses disponíveis para mostrar evolução histórica
    df <- d$receitas |>
      dplyr::group_by(imovel, mes_label, mes) |>
      dplyr::summarise(
        custo = sum(outros_custos, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(mes)
    
    validate(need(nrow(df) > 0, "Sem dados de custos."))
    
    aptos  <- unique(df$imovel)
    cores  <- c("#0052cc","#2684ff","#d97706","#7c3aed","#e03e3e","#00b388","#f59e0b","#9ca3af")
    traces <- lapply(seq_along(aptos), function(i) {
      sub <- df |> dplyr::filter(imovel == aptos[i])
      list(x = sub$mes_label, y = sub$custo, name = aptos[i],
           type = "bar",
           marker = list(color = cores[((i-1)%%length(cores))+1], line=list(color="transparent")),
           hovertemplate = paste0(aptos[i], "<br>%{x}<br>R$ %{y:,.0f}<extra></extra>"))
    })
    
    p <- plot_ly()
    for (tr in traces) p <- do.call(add_trace, c(list(p), tr))
    p |>
      layout(barmode = "group",
             paper_bgcolor="transparent", plot_bgcolor="transparent",
             xaxis=list(showgrid=FALSE,zeroline=FALSE,tickfont=list(size=10),title=""),
             yaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,
                        tickprefix="R$ ",tickfont=list(size=10),title=""),
             margin=list(l=55,r=12,t=8,b=30),
             legend=list(x=0,y=1.15,orientation="h",font=list(size=10))) |>
      config(displayModeBar = FALSE)
  })
  
  # ── Tabela custos por apartamento ────────────────────────────
  output$t_custos_apto <- renderDT({
    d <- dados(); req(d, input$mes_sel)
    rec <- rec_fil() |>
      dplyr::filter(competencia == input$mes_sel) |>
      dplyr::transmute(
        `Imóvel`          = imovel,
        `Receita Bruta`   = brl(receita_bruta),
        `Taxa Adm`        = paste0("- ", brl(taxa_adm)),
        `Manutenção`      = paste0("- ", brl(dplyr::coalesce(as.numeric(manutencao_total), 0))),
        `Reposição`       = paste0("- ", brl(dplyr::coalesce(as.numeric(reposicao_total), 0))),
        `Despesas`        = paste0("- ", brl(dplyr::coalesce(as.numeric(despesas_total), 0))),
        `Custos Totais`   = paste0("- ", brl(outros_custos)),
        `Resultado Líq`   = brl(resultado_liq),
        `% Custo/Receita` = paste0(round(ifelse(receita_bruta > 0, outros_custos/receita_bruta*100, 0)), "%")
      )
    validate(need(nrow(rec) > 0, "Sem dados para o período."))
    datatable(rec, rownames = FALSE, class = "compact stripe hover",
              options = list(dom = "ft", paging = FALSE, ordering = TRUE,
                             language = list(search = "Buscar:"))) |>
      formatStyle("% Custo/Receita",
                  background = styleColorBar(c(0, 100), "#fde68a"),
                  backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  }, server = FALSE)
  
  # ── KPIs Ordens de Serviço ────────────────────────────────────
  output$kpis_os <- renderUI({
    man <- manutencao_fil()
    if (nrow(man) == 0) return(p(class="sem-dados","Sem ordens de serviço no período."))
    
    total_val <- sum(man$valor_total, na.rm = TRUE)
    n_os      <- nrow(man)
    n_apto    <- if ("imovel_nome" %in% names(man)) length(unique(man$imovel_nome)) else 0
    val_med   <- mean(man$valor_total, na.rm = TRUE)
    # tipo mais frequente
    tipo_freq <- if ("tipo_servico" %in% names(man)) {
      tc <- sort(table(man$tipo_servico), decreasing = TRUE)
      if (length(tc) > 0) names(tc)[1] else "—"
    } else "—"
    n_pend <- if ("status" %in% names(man)) sum(grepl("pendente", tolower(man$status)), na.rm=TRUE) else 0
    
    div(class = "kgrid-sm",
        kcard_sm("Nº de OS",       as.character(n_os),   "blue"),
        kcard_sm("Custo Total OS", brl(total_val),        "red"),
        kcard_sm("Custo Médio/OS", brl(val_med),          "orange"),
        kcard_sm("Aptos Afetados", as.character(n_apto),  "blue"),
        kcard_sm("OS Pendentes",   as.character(n_pend),  "red"),
        kcard_sm("Tipo Mais Freq.", tipo_freq,             "purple")
    )
  })
  
  # ── Tabela Ordens de Serviço ──────────────────────────────────
  output$t_os <- renderDT({
    man <- manutencao_fil()
    validate(need(nrow(man) > 0, "Sem ordens de serviço para o período/imóvel selecionado."))
    
    # Monta colunas disponíveis com fallback robusto
    df <- man |>
      dplyr::transmute(
        `OS ID`           = if ("os_id"          %in% names(man)) as.character(os_id) else "—",
        `Imóvel`          = dplyr::coalesce(as.character(imovel_nome %||% property_id), "—"),
        `Competência`     = as.character(competencia),
        `Data`            = if ("data" %in% names(man)) as.character(data) else as.character(competencia),
        `Tipo Serviço`    = if ("tipo_servico"    %in% names(man)) as.character(tipo_servico) else "—",
        `Produto/Serviço` = if ("produto_servico" %in% names(man)) as.character(produto_servico) else "—",
        `Descrição`       = if ("descricao"       %in% names(man)) as.character(descricao) else "—",
        `Prestador`       = if ("prestador"       %in% names(man)) as.character(prestador) else "—",
        `Status`          = if ("status"          %in% names(man)) as.character(status) else "Concluída",
        `Valor (R$)`      = brl(valor_total)
      )
    
    datatable(df, rownames = FALSE, class = "compact stripe hover", escape = FALSE,
              options = list(pageLength = 10, dom = "ftip",
                             language = list(search="Buscar:",
                                             paginate=list(previous="Ant.",`next`="Próx.")))) |>
      formatStyle("Status",
                  color = styleEqual(
                    c("Concluída","concluída","Pendente","pendente","Em andamento","em andamento"),
                    c("#065f46","#065f46","#854d0e","#854d0e","#1d4ed8","#1d4ed8")
                  ),
                  backgroundColor = styleEqual(
                    c("Concluída","concluída","Pendente","pendente","Em andamento","em andamento"),
                    c("#d1fae5","#d1fae5","#fef3c7","#fef3c7","#dbeafe","#dbeafe")
                  ))
  }, server = FALSE)
  
  # ═══════════════════════════════════════════════════════════
  # ── KPIs Reposição ───────────────────────────────────────────
  output$kpis_reposicao <- renderUI({
    rep <- reposicao_fil()
    if (nrow(rep) == 0) return(p(class = "sem-dados", "Sem itens de reposição para o período."))
    
    total_val  <- sum(suppressWarnings(as.numeric(rep$valor_unitario_ou_total)), na.rm = TRUE)
    total_qtd  <- sum(suppressWarnings(as.numeric(rep$quantidade)), na.rm = TRUE)
    n_itens    <- nrow(rep)
    n_aptos    <- if ("apto_original" %in% names(rep)) length(unique(rep$apto_original)) else
      if ("imovel_nome"   %in% names(rep)) length(unique(rep$imovel_nome))   else "—"
    item_freq  <- if ("item_limpo" %in% names(rep)) {
      tc <- sort(table(rep$item_limpo), decreasing = TRUE)
      if (length(tc) > 0) names(tc)[1] else "—"
    } else if ("item_raw" %in% names(rep)) {
      tc <- sort(table(rep$item_raw), decreasing = TRUE)
      if (length(tc) > 0) names(tc)[1] else "—"
    } else "—"
    ticket_med <- if (n_itens > 0) total_val / n_itens else 0
    
    div(class = "kgrid-sm",
        kcard_sm("Total Reposição",   brl(total_val),               "teal"),
        kcard_sm("Qtd. Total",        as.character(round(total_qtd)), "blue"),
        kcard_sm("Nº de Itens",       as.character(n_itens),        "purple"),
        kcard_sm("Aptos Afetados",    as.character(n_aptos),        "orange"),
        kcard_sm("Item Mais Reposto", item_freq,                    "red"),
        kcard_sm("Ticket Médio/Item", brl(ticket_med),              "teal"))
  })
  
  # ── Tabela Reposição ──────────────────────────────────────────
  output$t_reposicao <- renderDT({
    rep <- reposicao_fil()
    validate(need(nrow(rep) > 0, "Sem itens de reposição para o período/imóvel selecionado."))
    
    # Resolver coluna de imóvel
    apto_col <- if ("apto_original" %in% names(rep)) rep$apto_original
    else if ("imovel_nome" %in% names(rep)) rep$imovel_nome
    else if ("property_id" %in% names(rep)) rep$property_id
    else rep("—", nrow(rep))
    
    # Resolver coluna de item
    item_col <- if ("item_limpo" %in% names(rep)) rep$item_limpo
    else if ("item_raw" %in% names(rep)) rep$item_raw
    else rep("—", nrow(rep))
    
    # Quantidade e valores
    qtd_col   <- suppressWarnings(as.numeric(if ("quantidade" %in% names(rep)) rep$quantidade else NA))
    val_col   <- suppressWarnings(as.numeric(rep$valor_unitario_ou_total))
    val_total <- ifelse(is.na(qtd_col) | qtd_col <= 0, val_col, qtd_col * val_col)
    max_val   <- max(val_total, na.rm = TRUE)
    
    df <- data.frame(
      `Imóvel`      = as.character(apto_col),
      `Item`        = as.character(item_col),
      `Qtd.`        = ifelse(is.na(qtd_col), "—", as.character(round(qtd_col))),
      `Valor Unit.` = brl(val_col),
      `Valor Total` = brl(val_total),
      check.names      = FALSE,
      stringsAsFactors = FALSE
    )
    
    # Se imóvel filtrado, remover coluna redundante
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- df[, names(df) != "Imóvel", drop = FALSE]
    
    datatable(
      df,
      rownames = FALSE,
      class    = "compact stripe hover",
      options  = list(
        pageLength = 15,
        dom        = "ftip",
        order      = list(list(0, "asc"), list(1, "asc")),
        language   = list(
          search   = "Buscar:",
          info     = "Mostrando _START_ a _END_ de _TOTAL_ itens",
          paginate = list(previous = "Ant.", `next` = "Próx.")
        )
      )
    ) |>
      formatStyle(
        "Valor Total",
        background         = styleColorBar(c(0, if (is.finite(max_val) && max_val > 0) max_val * 1.1 else 1), "#c7f2ed"),
        backgroundSize     = "100% 75%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      )
  }, server = FALSE)
  
  
  # OUTPUT: Resultado financeiro
  # ═══════════════════════════════════════════════════════════
  output$resultado <- renderUI({
    m <- rm()
    div(
      frow("Receita Bruta",        brl(m$receita_bruta), FALSE),
      frow("Taxa Administrativa",  paste0("- ", brl(m$taxa_adm)),      TRUE),
      frow("Outros custos fixos",  paste0("- ", brl(m$outros_custos)), TRUE),
      div(class = "ftotal",
          span("RESULTADO LÍQUIDO"),
          span(class = "fv g", brl(m$resultado_liq)))
    )
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Tabela custos (summary do mês — seção financeiro)
  # ═══════════════════════════════════════════════════════════
  output$t_custos <- renderDT({
    d <- dados(); req(d, input$mes_sel)
    rec_m <- d$receitas |> dplyr::filter(competencia == input$mes_sel)
    linhas <- rec_m |>
      dplyr::transmute(`Imóvel` = imovel, Item = "Custos do mês", Valor = brl(outros_custos))
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      linhas <- dplyr::select(linhas, -`Imóvel`)
    datatable(linhas, options=list(dom="t",paging=FALSE,ordering=FALSE),
              rownames=FALSE, class="compact stripe")
  }, server = FALSE)
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Ranking
  # ═══════════════════════════════════════════════════════════
  output$ranking <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    rank_df <- d$receitas |>
      dplyr::filter(competencia == input$mes_sel) |>
      dplyr::group_by(imovel) |>
      dplyr::summarise(receita = sum(receita_bruta, na.rm=TRUE), .groups="drop") |>
      dplyr::arrange(dplyr::desc(receita))
    if (nrow(rank_df) == 0) return(p(class="sem-dados","Sem dados."))
    mx  <- max(rank_df$receita, 1)
    cls <- c("b1","b2","b3","b4")
    items <- lapply(seq_len(nrow(rank_df)), function(i) {
      r <- rank_df[i,]
      div(class="ri",
          div(class="rn", i),
          div(class="rname", r$imovel),
          div(class="rbw", div(class=paste("rb",cls[min(i,4)]),
                               style=paste0("width:",round(r$receita/mx*100),"%;"))),
          div(class="rval", brl(r$receita)))
    })
    div(!!!items)
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Acumulado
  # ═══════════════════════════════════════════════════════════
  output$acumulado <- renderUI({
    acum <- rec_fil() |>
      dplyr::summarise(rec=sum(receita_bruta,na.rm=TRUE), res=sum(resultado_liq,na.rm=TRUE))
    div(
      div(class="acg",
          div(div(class="al","RECEITA ACUMULADA"), div(class="av",brl(acum$rec)), div(class="ad","▲ base consolidada")),
          div(div(class="al","RESULTADO ACUMULADO"), div(class="av g",brl(acum$res)), div(class="ad","▲ base consolidada"))
      ),
      shinycssloaders::withSpinner(plotlyOutput("g_acum",height="90px"),type=4,color="#00c49a")
    )
  })
  output$g_acum <- renderPlotly({
    df <- rec_fil() |>
      dplyr::group_by(mes) |>
      dplyr::summarise(rec=sum(receita_bruta,na.rm=TRUE),.groups="drop") |>
      dplyr::arrange(mes) |>
      dplyr::mutate(acum=cumsum(rec))
    validate(need(nrow(df)>0,""))
    plot_ly(df,x=~mes,y=~acum,type="scatter",mode="lines",fill="tozeroy",
            line=list(color="#00b388",width=2),fillcolor="rgba(0,179,136,0.12)",
            hovertemplate="%{x|%b/%Y}<br>Acum: R$ %{y:,.0f}<extra></extra>") |>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=F,zeroline=F,showticklabels=F,title=""),
             yaxis=list(showgrid=F,zeroline=F,showticklabels=F,title=""),
             margin=list(l=0,r=0,t=0,b=0),showlegend=FALSE) |>
      config(displayModeBar=FALSE)
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Gráfico Diárias por dia (análise temporal)
  # ═══════════════════════════════════════════════════════════
  output$g_diarias <- renderPlotly({
    d <- dados(); req(d, input$mes_sel)
    iid <- if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      input$imovel else if (length(d$imoveis_ids)>0) d$imoveis_ids[[1]] else return(NULL)
    cal <- d$calendario
    if (is.null(cal) || nrow(cal)==0) validate(need(FALSE,"Sem dados."))
    cal <- cal |>
      dplyr::filter(apto_original==iid,format(as.Date(data),"%Y-%m")==input$mes_sel) |>
      dplyr::arrange(data)
    validate(need(nrow(cal)>0,"Sem dados para o período."))
    plot_ly(cal,x=~as.Date(data),y=~valor,type="scatter",mode="lines+markers",
            line=list(color="#1a6ef7",width=2),marker=list(color=ifelse(cal$ocupado,"#1a6ef7","#d1d9e0"),size=6),
            hovertemplate="Dia %{x|%d/%m}<br>R$ %{y:,.0f}<extra></extra>") |>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=F,zeroline=F,tickformat="%d/%m",tickfont=list(size=10),title=""),
             yaxis=list(showgrid=T,gridcolor="#f0f4f8",zeroline=F,tickprefix="R$ ",tickfont=list(size=10),title=""),
             margin=list(l=52,r=10,t=8,b=30),showlegend=FALSE) |>
      config(displayModeBar=FALSE)
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Gráfico Evolução 6 Meses
  # ═══════════════════════════════════════════════════════════
  output$g_evolucao <- renderPlotly({
    df <- rec_fil() |>
      dplyr::group_by(mes,mes_label) |>
      dplyr::summarise(receita=sum(receita_bruta,na.rm=TRUE),resultado=sum(resultado_liq,na.rm=TRUE),.groups="drop") |>
      dplyr::arrange(mes) |> tail(6)
    validate(need(nrow(df)>0,"Sem dados."))
    n <- nrow(df); cores <- c(rep("#c5d8f7",max(n-1,0)),"#1a6ef7")[1:n]
    plot_ly(df,x=~mes_label,y=~receita,type="bar",
            marker=list(color=cores,line=list(color="transparent")),name="Receita Bruta",
            hovertemplate="%{x}<br>Receita: R$ %{y:,.0f}<extra></extra>") |>
      add_trace(y=~resultado,type="scatter",mode="lines+markers",
                line=list(color="#00b388",width=2),marker=list(color="#00b388",size=6),
                name="Resultado Líq.",yaxis="y2",
                hovertemplate="%{x}<br>Resultado: R$ %{y:,.0f}<extra></extra>") |>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=F,zeroline=F,tickfont=list(size=10),title=""),
             yaxis=list(showgrid=T,gridcolor="#f0f4f8",zeroline=F,tickprefix="R$ ",tickfont=list(size=10),title=""),
             yaxis2=list(overlaying="y",side="right",showgrid=F,zeroline=F,tickprefix="R$ ",tickfont=list(size=9),title=""),
             margin=list(l=52,r=52,t=8,b=30),
             legend=list(x=0,y=1.15,orientation="h",font=list(size=10))) |>
      config(displayModeBar=FALSE)
  })
  
  # ═══════════════════════════════════════════════════════════
  # OUTPUT: Tabela histórica
  # ═══════════════════════════════════════════════════════════
  output$t_historico <- renderDT({
    df <- rec_fil() |>
      dplyr::arrange(dplyr::desc(mes)) |>
      dplyr::transmute(
        `Imóvel`        = imovel,
        `Mês`           = mes_label,
        `Receita Bruta` = brl(receita_bruta),
        `Taxa Adm`      = paste0("- ", brl(taxa_adm)),
        `Outros Custos` = paste0("- ", brl(outros_custos)),
        `Resultado Líq` = brl(resultado_liq),
        `Ocupação`      = paste0(round(ocupacao), "%"),
        `Diária Média`  = brl(diaria_media),
        `Nº Diárias`    = n_diarias
      )
    datatable(df,
              options=list(pageLength=12,dom="frtip",
                           language=list(search="Buscar:",info="Mostrando _START_ a _END_ de _TOTAL_ registros",
                                         paginate=list(previous="Anterior",`next`="Próximo"))),
              rownames=FALSE,class="compact stripe hover")
  }, server=FALSE)
  
  
  
} # fim server

# app_public.R pode ser rodado de forma standalone (dev local) ou
# carregado como módulo filho por app.R (produção).
# Em produção, app.R acessa e$ui e chama e$server() manualmente.
# A linha abaixo garante compatibilidade com ambos os modos.
app <- shinyApp(ui, server)