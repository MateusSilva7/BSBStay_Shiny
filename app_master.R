# ============================================================
# BSB.STAY — Painel Mestre (Versão Interna) v2.0
# app_master.R
#
# - Sem busca por CNPJ — acesso direto
# - Navegação por Proprietário + Imóvel
# - Visão consolidada da carteira
# - Insights e proposta de valor
# ============================================================

# ── Bootstrap Render/Docker ───────────────────────────────────
options(
  shiny.host = "0.0.0.0",
  shiny.port = as.integer(Sys.getenv("PORT", "3838"))
)

APP_ROOT <- normalizePath(Sys.getenv("APP_ROOT", "."), winslash = "/", mustWork = FALSE)
dir.create(file.path(APP_ROOT, "data", "cache"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(APP_ROOT, "data", "raw"), recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(readxl)
  library(janitor)
  library(plotly)
  library(DT)
  library(DBI)
  library(RSQLite)
  library(shinycssloaders)
  library(stringr)
})

source(file.path(APP_ROOT, "R", "gdrive_public.R"), local = FALSE)

# ── Helpers ────────────────────────────────────────────────────
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

# Formatação VETORIZADA — seguras dentro de dplyr::transmute/mutate
brl <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(is.na(v), "R$ —",
         paste0("R$ ", formatC(round(v), format = "d", big.mark = ".")))
}
brl_compact <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(is.na(v), "R$ —",
         ifelse(abs(v) >= 1e6,
                paste0("R$ ", formatC(round(v / 1e6, 2), format = "f", digits = 2), "M"),
                ifelse(abs(v) >= 1e3,
                       paste0("R$ ", formatC(round(v / 1e3, 1), format = "f", digits = 1), "k"),
                       paste0("R$ ", formatC(round(v), format = "d", big.mark = ".")))))
}
fmt_pct <- function(x, decimals = 1) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(is.na(v), "—",
         paste0(formatC(round(v, decimals), format = "f", digits = decimals), "%"))
}

# Wrappers escalares — para renderUI / kcard / kcard_sm (recebem 1 valor)
s_brl <- function(x) {
  v <- suppressWarnings(as.numeric(x[1]))
  if (is.na(v)) return("R$ —")
  paste0("R$ ", formatC(round(v), format = "d", big.mark = "."))
}
s_brl_compact <- function(x) {
  v <- suppressWarnings(as.numeric(x[1]))
  if (is.na(v)) return("R$ —")
  if (abs(v) >= 1e6) paste0("R$ ", format(round(v / 1e6, 2), nsmall = 2), "M")
  else if (abs(v) >= 1e3) paste0("R$ ", format(round(v / 1e3, 1), nsmall = 1), "k")
  else paste0("R$ ", formatC(round(v), format = "d", big.mark = "."))
}
s_pct <- function(x, decimals = 1) {
  v <- suppressWarnings(as.numeric(x[1]))
  if (is.na(v)) return("—")
  paste0(format(round(v, decimals), nsmall = decimals), "%")
}

# UI components
kcard <- function(lbl, val, sub, dn = FALSE, vg = FALSE) {
  div(class = "kcard",
      div(class = "klbl", lbl),
      div(class = if (vg) "kval g" else "kval", val),
      div(class = if (dn) "kdelta dn" else "kdelta up", sub))
}
kcard_sm <- function(lbl, val, cor = "blue") {
  clr <- c(blue = "#1a6ef7", green = "#00b388", red = "#e03e3e",
           orange = "#d97706", purple = "#7c3aed", teal = "#0891b2")[[cor]] %||% "#1a6ef7"
  div(class = "kcard-sm",
      div(class = "ksm-lbl", lbl),
      div(class = "ksm-val", style = paste0("color:", clr), val))
}
frow <- function(lbl, val, neg = FALSE) {
  div(class = "fr",
      span(class = "fl", lbl),
      span(class = if (neg) "fv r" else "fv", val))
}
insight_card <- function(ico, titulo, corpo, cor = "blue") {
  bg_clr <- list(
    blue   = c("#eff6ff", "#1d4ed8"),
    green  = c("#f0fdf4", "#15803d"),
    orange = c("#fff7ed", "#c2410c"),
    purple = c("#faf5ff", "#6d28d9"),
    red    = c("#fff1f0", "#b91c1c")
  )[[cor]] %||% c("#eff6ff", "#1d4ed8")
  div(class = "insight-card",
      style = paste0("background:", bg_clr[1], ";border-left:4px solid ", bg_clr[2], ";"),
      div(class = "insight-icon", ico),
      div(class = "insight-body",
          div(class = "insight-title", style = paste0("color:", bg_clr[2]), titulo),
          div(class = "insight-text", corpo)))
}

# ── Carregamento inicial ────────────────────────────────────────
APP_DATA <- tryCatch(
  carregar_dados_app(folder_id = DRIVE_FOLDER_ID, forcar_dl = FALSE, forcar_etl = FALSE),
  error = function(e) { message("[Master] Erro: ", e$message); structure(list(), erro_msg = e$message) }
)

# Extrai dados flat de todos os proprietários (para aba Carteira)
build_carteira_flat <- function(app_data) {
  if (length(app_data) == 0) return(data.frame())
  res <- lapply(names(app_data), function(cpf) {
    d <- app_data[[cpf]]
    if (is.null(d$receitas) || nrow(d$receitas) == 0) return(NULL)
    d$receitas |> dplyr::mutate(
      cpf_cnpj     = cpf,
      proprietario = d$proprietario %||% cpf,
      n_imoveis    = length(d$imoveis_ids)
    )
  })
  do.call(rbind, Filter(Negate(is.null), res))
}

# ═══════════════════════════════════════════════════════════════
# UI
# ═══════════════════════════════════════════════════════════════
ui <- fluidPage(
  tags$head(
    tags$title("BSB.STAY — Painel Mestre"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggle_fbar', function(msg) {
        var prop = document.getElementById('fbar_prop');
        var cart = document.getElementById('fbar_cart');
        if (prop) prop.style.display = (msg.aba === 'proprietario') ? '' : 'none';
        if (cart) cart.style.display = (msg.aba === 'carteira')     ? '' : 'none';
      });
    ")),
    tags$style(HTML("
*{box-sizing:border-box;margin:0;padding:0;}
body{font-family:'Inter',sans-serif;background:#f0f2f5;color:#1e2d3d;font-size:14px;}
a{color:inherit;text-decoration:none;}

.hdr{background:#0a1628;padding:14px 32px;display:flex;align-items:center;
     justify-content:space-between;position:sticky;top:0;z-index:200;
     box-shadow:0 2px 12px rgba(0,0,0,.35);}
.hdr-left{display:flex;align-items:center;gap:14px;}
.logo{background:linear-gradient(135deg,#00c49a,#1a6ef7);color:#fff;font-weight:800;
      font-size:12px;padding:7px 11px;border-radius:7px;line-height:1.3;}
.hdr-title{color:#fff;font-size:17px;font-weight:700;}
.hdr-sub{color:#4a6a86;font-size:11px;margin-top:2px;}
.master-badge{background:linear-gradient(90deg,#f59e0b,#ef4444);color:#fff;
              border-radius:20px;padding:4px 12px;font-size:10px;font-weight:800;
              letter-spacing:.8px;text-transform:uppercase;}
.hdr-stats{display:flex;gap:20px;align-items:center;}
.hdr-stat{text-align:right;}
.hdr-stat-val{color:#e2f0ff;font-size:16px;font-weight:800;}
.hdr-stat-lbl{color:#4a6a86;font-size:9px;font-weight:700;letter-spacing:.6px;text-transform:uppercase;}

.sync-bar{background:#f8fafc;border-bottom:1px solid #e2e8f0;padding:6px 32px;
          display:flex;align-items:center;gap:10px;font-size:11px;color:#6b7280;}
.sync-dot{width:7px;height:7px;border-radius:50%;background:#00b388;flex-shrink:0;}
.sync-dot.old{background:#d97706;} .sync-dot.err{background:#e03e3e;}
.sync-btn{background:none;border:1px solid #d1d5db;border-radius:6px;padding:3px 10px;
          font-size:11px;color:#374151;cursor:pointer;font-family:inherit;transition:all .15s;}
.sync-btn:hover{background:#f3f4f6;}

.nav-tabs-master{background:#fff;border-bottom:2px solid #e2e8f0;padding:0 32px;display:flex;gap:0;}
.nav-tab-master{padding:14px 22px;font-size:13px;font-weight:700;color:#6b7280;
                cursor:pointer;border:none;background:none;font-family:'Inter',sans-serif;
                border-bottom:3px solid transparent;margin-bottom:-2px;transition:all .15s;}
.nav-tab-master.active{color:#1a6ef7;border-bottom-color:#1a6ef7;}
.nav-tab-master:hover:not(.active){color:#374151;}

.fbar{background:#fff;padding:10px 32px;border-bottom:2px solid #e8edf3;
      display:flex;gap:14px;align-items:center;flex-wrap:wrap;}
.fbar-lbl{font-size:11px;color:#6b7280;font-weight:700;letter-spacing:.6px;}
.fbar-sep{width:1px;height:24px;background:#e2e8f0;margin:0 4px;}

.content{padding:20px 32px 56px;max-width:1440px;margin:0 auto;}
.sec{font-size:10px;font-weight:800;color:#6b7280;letter-spacing:1.5px;text-transform:uppercase;
     margin:26px 0 10px;padding-bottom:6px;border-bottom:2px solid #e5e9ef;
     display:flex;align-items:center;gap:8px;}
.sec-badge{background:#0f1c2e;color:#5ab4ff;border-radius:10px;padding:2px 8px;
           font-size:9px;font-weight:800;letter-spacing:.5px;}

.kgrid{display:grid;grid-template-columns:repeat(5,1fr);gap:14px;margin-bottom:6px;}
.kgrid-4{display:grid;grid-template-columns:repeat(4,1fr);gap:14px;margin-bottom:6px;}
.kgrid-sm{display:grid;grid-template-columns:repeat(6,1fr);gap:10px;margin-bottom:14px;}
@media(max-width:1200px){.kgrid{grid-template-columns:repeat(3,1fr);}
  .kgrid-4{grid-template-columns:repeat(2,1fr);}
  .kgrid-sm{grid-template-columns:repeat(3,1fr);}}
@media(max-width:600px){.kgrid,.kgrid-4,.kgrid-sm{grid-template-columns:repeat(2,1fr);}}
.kcard{background:#fff;border-radius:12px;padding:18px 20px;border:1px solid #e5e9ef;
       box-shadow:0 1px 4px rgba(0,0,0,.04);transition:box-shadow .15s;}
.kcard:hover{box-shadow:0 3px 12px rgba(0,0,0,.08);}
.klbl{font-size:10px;font-weight:700;color:#6b7280;letter-spacing:.9px;
      text-transform:uppercase;margin-bottom:7px;}
.kval{font-size:24px;font-weight:800;color:#0f1c2e;line-height:1.1;}
.kval.g{color:#00b388;} .kval.r{color:#e03e3e;} .kval.b{color:#1a6ef7;}
.kdelta{font-size:11px;margin-top:6px;font-weight:600;}
.kdelta.up{color:#00b388;} .kdelta.dn{color:#e03e3e;}
.kcard-sm{background:#fff;border-radius:10px;padding:14px 16px;border:1px solid #e5e9ef;
          box-shadow:0 1px 3px rgba(0,0,0,.04);}
.ksm-lbl{font-size:9px;font-weight:700;color:#9aa5b4;letter-spacing:.9px;
         text-transform:uppercase;margin-bottom:5px;}
.ksm-val{font-size:20px;font-weight:800;line-height:1.1;}

.card{background:#fff;border-radius:12px;padding:20px;border:1px solid #e5e9ef;
      box-shadow:0 1px 5px rgba(0,0,0,.04);}
.cgrid{display:grid;grid-template-columns:1fr 1fr;gap:14px;margin-bottom:6px;}
.cgrid-3{display:grid;grid-template-columns:1fr 1fr 1fr;gap:14px;margin-bottom:6px;}
@media(max-width:900px){.cgrid,.cgrid-3{grid-template-columns:1fr;}}
.card-hdr{display:flex;justify-content:space-between;align-items:center;margin-bottom:14px;}
.card-ttl{font-size:13px;font-weight:700;color:#1e2d3d;}
.badge{background:#f0f4f8;color:#6b7280;font-size:10px;padding:3px 9px;border-radius:12px;font-weight:700;}
.badge-blue{background:#eff6ff;color:#2563eb;} .badge-green{background:#f0fdf4;color:#16a34a;}
.badge-orange{background:#fff7ed;color:#d97706;} .badge-red{background:#fff1f0;color:#e03e3e;}
.badge-purple{background:#faf5ff;color:#7c3aed;} .badge-teal{background:#f0fdfa;color:#0f766e;}

.imovel-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(200px,1fr));gap:12px;}
.icard{background:#fff;border-radius:10px;padding:14px 16px;border:1px solid #e5e9ef;
       box-shadow:0 1px 3px rgba(0,0,0,.04);transition:all .15s;}
.icard:hover{box-shadow:0 4px 14px rgba(0,0,0,.1);border-color:#1a6ef7;}
.icard-nome{font-size:12px;font-weight:700;color:#0f1c2e;margin-bottom:2px;}
.icard-end{font-size:11px;color:#6b7280;margin-bottom:6px;}
.icard-metrics{display:flex;gap:8px;flex-wrap:wrap;margin-top:6px;}
.icard-m{font-size:10px;background:#f0f4f8;color:#374151;padding:2px 7px;border-radius:8px;font-weight:600;}
.icard-m.g{background:#d1fae5;color:#065f46;} .icard-m.b{background:#dbeafe;color:#1d4ed8;}

.cg{display:grid;grid-template-columns:repeat(7,1fr);gap:4px;text-align:center;}
.ch{font-size:9px;font-weight:800;color:#9aa5b4;padding:6px 0;text-transform:uppercase;}
.cd{background:#d1fae5;border-radius:8px;padding:6px 2px;border:1px solid #a7f3d0;line-height:1.5;}
.cd.v{background:#f7f9fb;border-color:#e2e8f0;} .cd.e{background:transparent;border:none;}
.cd-n{font-size:11px;font-weight:800;color:#0f1c2e;} .cd-v{font-size:9px;color:#059669;font-weight:700;}
.cd.v .cd-n{color:#9aa5b4;} .cd.v .cd-v{color:#c8d4de;}

.fr{display:flex;justify-content:space-between;padding:9px 0;border-bottom:1px solid #f3f6f9;font-size:13px;}
.fr:last-child{border:none;}
.fl{color:#374151;} .fv{font-weight:700;} .fv.r{color:#e03e3e;} .fv.g{color:#00b388;font-size:15px;}
.ftotal{display:flex;justify-content:space-between;padding:12px 0 4px;font-weight:800;
        font-size:14px;border-top:2px solid #e5e9ef;margin-top:4px;}

.ri{display:flex;align-items:center;gap:10px;padding:8px 0;border-bottom:1px solid #f3f6f9;}
.ri:last-child{border:none;}
.rn{width:22px;height:22px;background:#f0f4f8;border-radius:50%;display:flex;
    align-items:center;justify-content:center;font-size:10px;font-weight:800;color:#6b7280;flex-shrink:0;}
.rname{font-size:12px;font-weight:700;min-width:80px;flex:1;}
.rsub{font-size:10px;color:#9aa5b4;font-weight:500;}
.rbw{flex:2;} .rb{height:9px;border-radius:3px;transition:width .4s ease;}
.b1{background:#0052cc;} .b2{background:#2684ff;} .b3{background:#79b8ff;}
.b4{background:#bcd6f8;} .b5{background:#dbeafe;}
.rval{font-size:12px;font-weight:700;white-space:nowrap;}

.acg{display:grid;grid-template-columns:1fr 1fr;gap:14px;margin-bottom:8px;}
.al{font-size:9px;font-weight:800;color:#6b7280;letter-spacing:.9px;text-transform:uppercase;margin-bottom:3px;}
.av{font-size:22px;font-weight:800;color:#0f1c2e;} .av.g{color:#00b388;}
.ad{font-size:11px;color:#00b388;font-weight:700;}

.insights-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(320px,1fr));gap:12px;margin-bottom:6px;}
.insight-card{display:flex;gap:12px;padding:14px 16px;border-radius:10px;border:1px solid transparent;}
.insight-icon{font-size:22px;flex-shrink:0;line-height:1.4;}
.insight-title{font-size:12px;font-weight:800;margin-bottom:4px;}
.insight-text{font-size:12px;line-height:1.5;color:#374151;}

.pv-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:14px;margin-bottom:6px;}
@media(max-width:900px){.pv-grid{grid-template-columns:1fr;}}
.pv-card{background:#fff;border-radius:12px;padding:20px;border:1px solid #e5e9ef;
         box-shadow:0 1px 5px rgba(0,0,0,.04);}
.pv-icon{font-size:28px;margin-bottom:10px;}
.pv-title{font-size:13px;font-weight:800;color:#0f1c2e;margin-bottom:6px;}
.pv-body{font-size:12px;color:#4b5563;line-height:1.6;}
.pv-metric{margin-top:10px;padding:8px 12px;background:#f8fafc;border-radius:8px;
           font-size:13px;font-weight:700;color:#0f1c2e;}

.diaria-row{display:flex;align-items:center;padding:10px 12px;border-radius:8px;
            background:#fafbfc;border:1px solid #e5e9ef;margin-bottom:6px;gap:12px;}
.diaria-row:hover{background:#f0f7ff;}
.diaria-datas{font-size:11px;color:#6b7280;min-width:140px;}
.diaria-datas b{color:#0f1c2e;font-size:12px;}
.diaria-noites{font-size:11px;color:#6b7280;min-width:60px;}
.diaria-val{font-size:16px;font-weight:800;color:#1a6ef7;min-width:90px;text-align:right;}
.diaria-bar-wrap{flex:1;}
.diaria-bar{height:8px;border-radius:4px;background:#1a6ef7;opacity:.7;}
.diaria-badge{font-size:9px;font-weight:700;padding:2px 8px;border-radius:10px;}
.db-alto{background:#fef3c7;color:#d97706;} .db-medio{background:#eff6ff;color:#2563eb;}
.db-baixo{background:#f3f4f6;color:#6b7280;}

.op-tabs{display:flex;gap:4px;margin-bottom:16px;background:#f3f4f6;border-radius:10px;padding:4px;}
.op-tab{flex:1;text-align:center;padding:8px 12px;border-radius:7px;font-size:12px;font-weight:700;
        color:#6b7280;cursor:pointer;border:none;background:none;font-family:'Inter',sans-serif;transition:all .15s;}
.op-tab.active{background:#fff;color:#0f1c2e;box-shadow:0 1px 4px rgba(0,0,0,.1);}
.op-tab:hover:not(.active){background:rgba(255,255,255,.5);}

.alert-row{display:flex;align-items:flex-start;gap:10px;padding:10px 14px;
           border-radius:8px;margin-bottom:8px;font-size:12px;}
.alert-warn{background:#fff7ed;border:1px solid #fde68a;}
.alert-ok{background:#f0fdf4;border:1px solid #bbf7d0;}
.alert-info{background:#eff6ff;border:1px solid #bfdbfe;}
.alert-icon{font-size:16px;flex-shrink:0;}

.dataTables_wrapper .dataTables_filter input{border:1px solid #e2e8f0;border-radius:6px;padding:4px 10px;font-size:12px;}
.dataTables_wrapper .dataTables_info,.dataTables_wrapper .dataTables_paginate{font-size:12px;color:#6b7280;}
table.dataTable thead th{font-size:11px;font-weight:700;color:#6b7280;letter-spacing:.5px;text-transform:uppercase;}
table.dataTable tbody td{font-size:12px;}

.sem-dados{color:#9aa5b4;font-size:13px;padding:20px 0;text-align:center;}
.erro-dados{background:#fff7ed;border:1px solid #fdba74;color:#9a3412;
            padding:14px 16px;border-radius:10px;margin-bottom:16px;font-size:13px;}
.empty-state{text-align:center;padding:80px 20px;color:#6b7280;}
.empty-state h3{font-size:20px;color:#374151;margin-bottom:8px;}
.form-group{margin-bottom:0!important;}
label{font-size:11px!important;font-weight:700!important;color:#6b7280!important;}
.shiny-spinner-output-container{min-height:60px;}
.recv-wrap,.det-wrap{background:#fff;border-radius:12px;padding:20px;border:1px solid #e5e9ef;
  box-shadow:0 1px 5px rgba(0,0,0,.04);margin-bottom:14px;}
.recv-hdr,.det-hdr{display:flex;justify-content:space-between;align-items:center;margin-bottom:16px;}
.recv-ttl,.det-ttl{font-size:14px;font-weight:700;color:#1e2d3d;}
.tab-wrap{overflow-x:auto;}
    "))
  ),
  
  div(class = "hdr",
      div(class = "hdr-left",
          div(class = "logo", HTML("bsb.<br>STAY")),
          div(div(class = "hdr-title", "Painel Mestre"),
              div(class = "hdr-sub",   "Controle e Validação Interno — BSBStay")),
          div(class = "master-badge", "🔐 USO INTERNO")
      ),
      uiOutput("hdr_stats")
  ),
  uiOutput("sync_bar"),
  uiOutput("nav_tabs"),
  uiOutput("filter_bar"),
  div(class = "content",
      uiOutput("alerta_erro"),
      uiOutput("body_master"))
)

# ═══════════════════════════════════════════════════════════════
# SERVER
# ═══════════════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    app_data    = APP_DATA,
    aba         = "proprietario",
    op_aba      = "despesas",
    syncing     = FALSE,
    sync_status = "ok",
    last_sync   = tryCatch(status_cache(), error = function(e) list(last_sync = NA))$last_sync
  )
  
  observeEvent(input$btn_aba,    { rv$aba    <- input$btn_aba    }, ignoreInit = TRUE)
  observeEvent(input$btn_aba_op, { rv$op_aba <- input$btn_aba_op }, ignoreInit = TRUE)
  
  # ── Header stats ─────────────────────────────────────────────
  output$hdr_stats <- renderUI({
    d <- rv$app_data; if (length(d) == 0) return(NULL)
    n_prop <- length(d)
    n_imov <- sum(sapply(d, function(x) length(x$imoveis_ids)))
    flat   <- build_carteira_flat(d)
    rec    <- if (nrow(flat) > 0) sum(flat$receita_bruta, na.rm = TRUE) else 0
    div(class = "hdr-stats",
        div(class = "hdr-stat", div(class = "hdr-stat-val", n_prop),
            div(class = "hdr-stat-lbl", "Proprietários")),
        div(class = "hdr-stat", div(class = "hdr-stat-val", n_imov),
            div(class = "hdr-stat-lbl", "Imóveis")),
        div(class = "hdr-stat", div(class = "hdr-stat-val", s_brl_compact(rec)),
            div(class = "hdr-stat-lbl", "Receita Total")))
  })
  
  # ── Sync bar ─────────────────────────────────────────────────
  output$sync_bar <- renderUI({
    dot <- paste("sync-dot", rv$sync_status)
    msg <- if (rv$syncing) "⏳ Sincronizando..." else if (!is.na(rv$last_sync))
      paste0("✓ Última sincronização: ", rv$last_sync) else "⚠ Dados não sincronizados"
    div(class = "sync-bar", div(class = dot), span(msg),
        tags$button(class = "sync-btn",
                    onclick = "Shiny.setInputValue('btn_sync', Math.random())",
                    if (rv$syncing) "⏳ Aguarde..." else "↻ Atualizar dados"))
  })
  observeEvent(input$btn_sync, {
    rv$syncing <- TRUE
    tryCatch({
      nd <- carregar_dados_app(folder_id = DRIVE_FOLDER_ID, forcar_dl = TRUE, forcar_etl = TRUE)
      rv$app_data <- nd; rv$last_sync <- format(Sys.time(), "%d/%m/%Y %H:%M"); rv$sync_status <- "ok"
      showNotification("✓ Dados atualizados!", type = "message", duration = 4)
    }, error = function(e) {
      rv$sync_status <- "err"
      showNotification(paste("⚠ Falha:", e$message), type = "error", duration = 6)
    })
    rv$syncing <- FALSE
  }, ignoreInit = TRUE)
  
  # ── Nav tabs ─────────────────────────────────────────────────
  output$nav_tabs <- renderUI({
    abas <- list(
      list(id = "proprietario", lbl = "👤 Por Proprietário"),
      list(id = "carteira",     lbl = "🏘 Visão da Carteira"),
      list(id = "insights",     lbl = "💡 Insights & Proposta de Valor")
    )
    div(class = "nav-tabs-master",
        lapply(abas, function(a) tags$button(
          class   = paste("nav-tab-master", if (rv$aba == a$id) "active" else ""),
          onclick = sprintf("Shiny.setInputValue('btn_aba','%s',{priority:'event'})", a$id),
          a$lbl)))
  })
  
  # ── Filter bar ───────────────────────────────────────────────
  # Renderizado UMA vez; visibilidade controlada por JS + updateSelectInput.
  output$filter_bar <- renderUI({
    d <- rv$app_data; if (length(d) == 0) return(NULL)
    props      <- names(d)
    prop_nomes <- setNames(props, sapply(props, function(cpf) d[[cpf]]$proprietario %||% cpf))
    flat       <- build_carteira_flat(d)
    meses_cart <- if (nrow(flat) > 0) sort(unique(flat$competencia), decreasing = TRUE) else character(0)
    meses_cart_lbl <- setNames(meses_cart, {
      dt <- suppressWarnings(as.Date(paste0(meses_cart, "-01")))
      ifelse(is.na(dt), meses_cart, format(dt, "%B/%Y"))
    })
    tagList(
      div(id = "fbar_prop", class = "fbar",
          style = if (isolate(rv$aba) != "proprietario") "display:none" else "",
          div(class = "fbar-lbl", "PROPRIETÁRIO:"),
          selectInput("prop_sel", NULL, choices = prop_nomes, selected = props[1], width = "260px"),
          div(class = "fbar-sep"),
          div(class = "fbar-lbl", "MÊS:"),
          selectInput("mes_sel", NULL, choices = character(0), width = "160px"),
          div(class = "fbar-sep"),
          div(class = "fbar-lbl", "IMÓVEL:"),
          selectInput("imovel", NULL, choices = c("Todos os imóveis" = "all"), width = "220px")),
      div(id = "fbar_cart", class = "fbar",
          style = if (isolate(rv$aba) != "carteira") "display:none" else "",
          div(class = "fbar-lbl", "MÊS:"),
          selectInput("mes_cart", NULL, choices = meses_cart_lbl,
                      selected = meses_cart[1], width = "160px"))
    )
  })
  
  # Mostrar/ocultar fbar via JS sem recriar inputs
  observeEvent(rv$aba, {
    session$sendCustomMessage("toggle_fbar", list(aba = rv$aba))
  }, ignoreInit = TRUE)
  
  # Atualizar mes e imovel quando proprietario muda
  observeEvent(input$prop_sel, {
    req(input$prop_sel)
    d_sel <- rv$app_data[[input$prop_sel]]
    if (is.null(d_sel)) return()
    meses <- if (!is.null(d_sel$receitas) && nrow(d_sel$receitas) > 0)
      sort(unique(d_sel$receitas$competencia), decreasing = TRUE) else character(0)
    meses_lbl <- setNames(meses, {
      dt <- suppressWarnings(as.Date(paste0(meses, "-01")))
      ifelse(is.na(dt), meses, format(dt, "%B/%Y"))
    })
    imoveis <- c("Todos os imóveis" = "all", setNames(d_sel$imoveis_ids, d_sel$imoveis_ids))
    updateSelectInput(session, "mes_sel", choices = meses_lbl, selected = meses[1])
    updateSelectInput(session, "imovel",  choices = imoveis,   selected = "all")
  }, ignoreInit = FALSE)
  
  output$alerta_erro <- renderUI({
    msg <- attr(rv$app_data, "erro_msg")
    if (!is.null(msg)) div(class = "erro-dados", tags$b("⚠ Atenção: "), msg)
  })
  
  # ── Body roteamento ──────────────────────────────────────────
  output$body_master <- renderUI({
    if (length(rv$app_data) == 0)
      return(div(class = "empty-state", h3("⏳ Carregando dados..."),
                 p("Aguarde a sincronização com o Google Drive.")))
    switch(rv$aba,
           "proprietario" = uiOutput("body_proprietario"),
           "carteira"     = uiOutput("body_carteira"),
           "insights"     = uiOutput("body_insights"))
  })
  
  # ═══════════════════════════════════════════════════════════
  # REACTIVES — ABA PROPRIETÁRIO
  # ═══════════════════════════════════════════════════════════
  dados <- reactive({
    req(input$prop_sel, rv$aba == "proprietario")
    rv$app_data[[input$prop_sel]]
  })
  
  rec_fil <- reactive({
    d <- dados(); req(d)
    df <- d$receitas
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- dplyr::filter(df, imovel == input$imovel)
    df
  })
  
  rm_mes <- reactive({
    req(input$mes_sel)
    rec_fil() |> dplyr::filter(competencia == input$mes_sel) |>
      dplyr::summarise(
        receita_bruta = sum(receita_bruta, na.rm = TRUE),
        taxa_adm      = sum(taxa_adm,      na.rm = TRUE),
        outros_custos = sum(outros_custos, na.rm = TRUE),
        resultado_liq = sum(resultado_liq, na.rm = TRUE),
        ocupacao      = mean(ocupacao,     na.rm = TRUE),
        diaria_media  = mean(diaria_media, na.rm = TRUE),
        n_diarias     = sum(n_diarias,     na.rm = TRUE))
  })
  
  reservas_fil <- reactive({
    d <- dados(); req(d, input$mes_sel)
    if (is.null(d$reservas) || nrow(d$reservas) == 0) return(data.frame())
    df <- dplyr::filter(d$reservas, competencia == input$mes_sel)
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- dplyr::filter(df, imovel_nome == input$imovel)
    dplyr::arrange(df, checkin)
  })
  
  despesas_fil <- reactive({
    d <- dados(); req(d, input$mes_sel)
    if (is.null(d$despesas) || nrow(d$despesas) == 0) return(data.frame())
    df <- dplyr::filter(d$despesas, competencia == input$mes_sel)
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- dplyr::filter(df, imovel_nome == input$imovel)
    df
  })
  
  reposicao_fil <- reactive({
    d <- dados(); req(d, input$mes_sel)
    if (is.null(d$reposicao) || nrow(d$reposicao) == 0) return(data.frame())
    df <- dplyr::filter(d$reposicao, competencia == input$mes_sel)
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- dplyr::filter(df, imovel_nome == input$imovel)
    df
  })
  
  manutencao_fil <- reactive({
    d <- dados(); req(d, input$mes_sel)
    if (is.null(d$manutencao) || nrow(d$manutencao) == 0) return(data.frame())
    df <- dplyr::filter(d$manutencao, competencia == input$mes_sel)
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      df <- dplyr::filter(df, imovel_nome == input$imovel)
    df
  })
  
  # ═══════════════════════════════════════════════════════════
  # ABA 1 — POR PROPRIETÁRIO
  # ═══════════════════════════════════════════════════════════
  output$body_proprietario <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    m  <- isolate(rm_mes())
    mes_lbl    <- { dt <- suppressWarnings(as.Date(paste0(input$mes_sel,"-01")))
    if (!is.na(dt)) format(dt, "%B/%Y") else input$mes_sel }
    mes_badge  <- { dt <- suppressWarnings(as.Date(paste0(input$mes_sel,"-01")))
    if (!is.na(dt)) format(dt, "%b %Y") else input$mes_sel }
    tagList(
      div(class = "sec", "RESULTADOS DO MÊS", div(class = "sec-badge", mes_lbl)),
      div(class = "kgrid",
          kcard("Receita Bruta",  s_brl(m$receita_bruta), "receita do período"),
          kcard("Taxa Adm.",      s_brl(m$taxa_adm),      "comissão BSBStay", dn = TRUE),
          kcard("Outros Custos",  s_brl(m$outros_custos), "manutenção + despesas", dn = TRUE),
          kcard("Resultado Líq.", s_brl(m$resultado_liq), "após todas as deduções", vg = TRUE),
          kcard("Ocupação", paste0(round(m$ocupacao), "%"),
                paste0("Diária média: ", s_brl(m$diaria_media)))),
      
      div(class = "sec", "PORTFÓLIO DE IMÓVEIS",
          div(class = "sec-badge", paste(length(d$imoveis_ids), "imóveis"))),
      uiOutput("portfolio_cards"),
      
      uiOutput("sec_analise_receita"),
      uiOutput("sec_detalhamento_mes"),
      
      div(class = "sec", "RESULTADO FINANCEIRO"),
      div(class = "cgrid",
          div(class = "card",
              div(class = "card-hdr", div(class = "card-ttl", "Resultado Financeiro"),
                  span(class = "badge", "Após deduções")),
              uiOutput("resultado"),
              div(style = "margin-top:16px;",
                  div(class = "card-hdr",
                      div(class = "card-ttl", "Custos Discriminados"),
                      span(class = "badge badge-orange", mes_badge)),
                  shinycssloaders::withSpinner(DTOutput("t_custos"), type=4, color="#00c49a"))),
          div(class = "card",
              div(class = "card-hdr", div(class = "card-ttl", "Ranking de Imóveis"),
                  span(class = "badge", "Receita no mês")),
              uiOutput("ranking"))),
      
      div(class = "sec", "ANÁLISE DA DIÁRIA ENTRE CHECK-INS"),
      shinycssloaders::withSpinner(uiOutput("kpis_diaria"), type=4, color="#1a6ef7"),
      div(class = "cgrid",
          div(class = "card",
              div(class = "card-hdr", div(class = "card-ttl", "Evolução da Diária por Reserva"),
                  span(class = "badge badge-blue", "Nível de reserva")),
              shinycssloaders::withSpinner(
                plotlyOutput("g_diaria_reservas", height = "240px"), type=4, color="#1a6ef7")),
          div(class = "card",
              div(class = "card-hdr", div(class = "card-ttl", "Intervalos entre Check-ins"),
                  span(class = "badge badge-blue", mes_badge)),
              uiOutput("lista_diarias"))),
      
      div(class = "sec", "OPERACIONAL"),
      div(class = "card",
          div(class = "op-tabs",
              tags$button(class = paste("op-tab", if (rv$op_aba=="despesas") "active" else ""),
                          onclick="Shiny.setInputValue('btn_aba_op','despesas',{priority:'event'})", "💰 Despesas"),
              tags$button(class = paste("op-tab", if (rv$op_aba=="custos") "active" else ""),
                          onclick="Shiny.setInputValue('btn_aba_op','custos',{priority:'event'})", "🏠 Custos por Apartamento"),
              tags$button(class = paste("op-tab", if (rv$op_aba=="os") "active" else ""),
                          onclick="Shiny.setInputValue('btn_aba_op','os',{priority:'event'})", "🔧 Ordens de Serviço")),
          uiOutput("painel_operacional")),
      
      div(class = "sec", "ANÁLISE TEMPORAL"),
      div(class = "cgrid",
          div(class = "card",
              div(class = "card-hdr", div(class = "card-ttl", "Diárias por Dia"),
                  span(class = "badge", mes_lbl)),
              shinycssloaders::withSpinner(plotlyOutput("g_diarias", height="200px"), type=4, color="#00c49a")),
          div(class = "card",
              div(class = "card-hdr", div(class = "card-ttl", "Evolução de Receita e Resultado"),
                  span(class = "badge", "Todos os meses")),
              shinycssloaders::withSpinner(plotlyOutput("g_evolucao", height="200px"), type=4, color="#00c49a"))),
      
      div(class = "sec", "VISÃO GERAL DA CARTEIRA"),
      div(class = "card",
          div(class = "card-hdr", div(class = "card-ttl", "Acumulado do Ano"),
              span(class = "badge", "Acumulado até o período")),
          uiOutput("acumulado")),
      
      div(class = "sec", "HISTÓRICO DETALHADO"),
      div(class = "card",
          shinycssloaders::withSpinner(DTOutput("t_historico"), type=4, color="#00c49a"))
    )
  })
  
  # ── Portfolio cards ──────────────────────────────────────────
  output$portfolio_cards <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    if (length(d$imoveis_cfg) == 0) return(p(class="sem-dados","Nenhum imóvel cadastrado."))
    rec_mes <- rec_fil() |> dplyr::filter(competencia == input$mes_sel) |>
      dplyr::select(imovel, receita_bruta, ocupacao, diaria_media)
    div(class = "imovel-grid",
        lapply(d$imoveis_cfg, function(im) {
          nm  <- as.character(im$nome %||% im$id)
          rec <- dplyr::filter(rec_mes, imovel == nm)
          r   <- if (nrow(rec) > 0) rec[1,] else NULL
          div(class = "icard",
              div(class = "icard-nome", nm),
              div(class = "icard-end",  as.character(im$bairro %||% "")),
              div(class = "icard-metrics",
                  if (!is.null(r)) div(class="icard-m g", s_brl_compact(r$receita_bruta)) else NULL,
                  if (!is.null(r)) div(class="icard-m b", paste0(round(r$ocupacao),"% ocup.")) else NULL,
                  if (!is.null(r)) div(class="icard-m",   paste0(s_brl_compact(r$diaria_media),"/noite")) else NULL))
        }))
  })
  
  # ── Análise receita (gráfico diária/dia) ────────────────────
  output$sec_analise_receita <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    iid <- if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      input$imovel else if (length(d$imoveis_ids) > 0) d$imoveis_ids[[1]] else return(NULL)
    cal <- d$calendario
    if (is.null(cal) || nrow(cal) == 0) return(NULL)
    if (nrow(dplyr::filter(cal, (apto_original==iid | property_id==iid),
                           substr(as.character(data),1,7)==input$mes_sel)) == 0) return(NULL)
    mes_lbl <- format(suppressWarnings(as.Date(paste0(input$mes_sel,"-01"))), "%B/%Y")
    tagList(
      div(class="sec","ANÁLISE DE RECEITA"),
      div(class="recv-wrap",
          div(class="recv-hdr", div(class="recv-ttl","Valor da Diária por Dia"),
              span(class="badge badge-blue", mes_lbl)),
          shinycssloaders::withSpinner(plotlyOutput("g_diaria_dia",height="220px"),type=4,color="#1a6ef7")))
  })
  output$g_diaria_dia <- renderPlotly({
    d <- dados(); req(d, input$mes_sel)
    iid <- if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      input$imovel else if (length(d$imoveis_ids)>0) d$imoveis_ids[[1]] else return(NULL)
    cal <- d$calendario; if (is.null(cal)||nrow(cal)==0) validate(need(FALSE,"Sem dados."))
    cal <- cal |> dplyr::filter(apto_original==iid|property_id==iid,
                                substr(as.character(data),1,7)==input$mes_sel) |> dplyr::arrange(data)
    validate(need(nrow(cal)>0,"Sem dados para o período."))
    plot_ly(cal,x=~as.Date(data),y=~valor,type="scatter",mode="lines+markers",
            line=list(color="#1a6ef7",width=2.5),marker=list(color="#1a6ef7",size=7),
            hovertemplate="Dia %{x|%d/%m}<br>R$ %{y:,.0f}<extra></extra>") |>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,tickformat="%d/%m",tickfont=list(size=10),title=""),
             yaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,tickprefix="R$ ",tickfont=list(size=10),title=""),
             margin=list(l=55,r=12,t=8,b=32),showlegend=FALSE) |> config(displayModeBar=FALSE)
  })
  
  # ── Detalhamento do mês (calendário) ────────────────────────
  output$sec_detalhamento_mes <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    iid <- if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      input$imovel else if (length(d$imoveis_ids)>0) d$imoveis_ids[[1]] else return(NULL)
    cal <- d$calendario; if (is.null(cal)||nrow(cal)==0) return(NULL)
    if (nrow(dplyr::filter(cal,apto_original==iid|property_id==iid,
                           substr(as.character(data),1,7)==input$mes_sel))==0) return(NULL)
    mes_badge <- format(suppressWarnings(as.Date(paste0(input$mes_sel,"-01"))),"%b %Y")
    tagList(
      div(class="sec","DETALHAMENTO DO MÊS"),
      div(class="det-wrap",
          div(class="det-hdr", div(class="det-ttl","Calendário de Ocupação"),
              span(class="badge badge-green", mes_badge)),
          shinycssloaders::withSpinner(uiOutput("calendario_v2"),type=4,color="#00c49a")))
  })
  output$calendario_v2 <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    iid <- if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      input$imovel else if (length(d$imoveis_ids)>0) d$imoveis_ids[[1]] else return(p("Sem dados."))
    cal <- d$calendario
    if (is.null(cal)||nrow(cal)==0) return(p(class="sem-dados","Sem dados de ocupação."))
    cal <- cal |> dplyr::filter(apto_original==iid|property_id==iid,
                                substr(as.character(data),1,7)==input$mes_sel) |>
      dplyr::arrange(data) |> dplyr::mutate(data=as.Date(data))
    if (nrow(cal)==0) return(p(class="sem-dados","Sem dados para o período."))
    ini  <- as.Date(paste0(input$mes_sel,"-01"))
    fim  <- lubridate::ceiling_date(ini,"month")-1
    full <- data.frame(data=seq.Date(ini,fim,by="day")) |>
      dplyr::left_join(dplyr::select(cal,data,valor,ocupado),by="data") |>
      dplyr::mutate(ocupado=dplyr::coalesce(ocupado,FALSE),valor=dplyr::coalesce(valor,0))
    hdrs   <- lapply(c("DOM","SEG","TER","QUA","QUI","SEX","SÁB"),function(x) div(class="ch",x))
    prm    <- as.integer(format(ini,"%w"))
    vazios <- if(prm>0) lapply(seq_len(prm),function(i) div(class="cd e")) else list()
    dias   <- lapply(seq_len(nrow(full)),function(i){
      r <- full[i,]
      div(class=if(isTRUE(r$ocupado))"cd" else "cd v",
          div(class="cd-n",as.integer(format(r$data,"%d"))),
          div(class="cd-v",if(r$valor>0) paste0("R$",format(round(r$valor),big.mark=".")) else "—"))
    })
    div(class="cg",!!!c(hdrs,vazios,dias))
  })
  
  # ── Resultado financeiro ────────────────────────────────────
  output$resultado <- renderUI({
    m <- rm_mes()
    div(
      frow("Receita Bruta",       s_brl(m$receita_bruta), FALSE),
      frow("Taxa Administrativa", paste0("- ", s_brl(m$taxa_adm)),      TRUE),
      frow("Outros custos",       paste0("- ", s_brl(m$outros_custos)), TRUE),
      div(class="ftotal",
          span("RESULTADO LÍQUIDO"),
          span(class="fv g", s_brl(m$resultado_liq))))
  })
  
  # ── Tabela custos ─────────────────────────────────────────────
  output$t_custos <- renderDT({
    d <- dados(); req(d, input$mes_sel)
    linhas <- d$receitas |> dplyr::filter(competencia == input$mes_sel) |>
      dplyr::transmute(`Imóvel` = imovel, `Item` = "Custos do mês", `Valor` = brl(outros_custos))
    if (!is.null(input$imovel) && nzchar(input$imovel) && input$imovel != "all")
      linhas <- dplyr::select(linhas, -`Imóvel`)
    datatable(linhas, options=list(dom="t",paging=FALSE,ordering=FALSE),
              rownames=FALSE, class="compact stripe")
  }, server=FALSE)
  
  # ── Ranking ──────────────────────────────────────────────────
  output$ranking <- renderUI({
    d <- dados(); req(d, input$mes_sel)
    df <- d$receitas |> dplyr::filter(competencia==input$mes_sel) |>
      dplyr::group_by(imovel) |>
      dplyr::summarise(receita=sum(receita_bruta,na.rm=TRUE),.groups="drop") |>
      dplyr::arrange(dplyr::desc(receita))
    if (nrow(df)==0) return(p(class="sem-dados","Sem dados."))
    mx  <- max(df$receita,1)
    cls <- c("b1","b2","b3","b4","b5")
    items <- lapply(seq_len(nrow(df)),function(i){
      r <- df[i,]
      div(class="ri",div(class="rn",i),div(class="rname",r$imovel),
          div(class="rbw",div(class=paste("rb",cls[min(i,5)]),
                              style=paste0("width:",round(r$receita/mx*100),"%;"))),
          div(class="rval",s_brl_compact(r$receita)))
    })
    div(!!!items)
  })
  
  # ── KPIs diária ───────────────────────────────────────────────
  output$kpis_diaria <- renderUI({
    df <- reservas_fil()
    if (nrow(df)==0) return(p(class="sem-dados","Sem reservas para o período selecionado."))
    d_media <- mean(df$diaria_liquida,na.rm=TRUE)
    d_max   <- max(df$diaria_liquida, na.rm=TRUE)
    d_min   <- suppressWarnings(min(df$diaria_liquida[df$diaria_liquida>0],na.rm=TRUE))
    d_var   <- if (!is.infinite(d_max)&&!is.infinite(d_min)&&d_min>0)
      paste0("+",round((d_max/d_min-1)*100),"%") else "—"
    ticket  <- mean(df$receita_total,na.rm=TRUE)
    div(class="kgrid-sm",
        kcard_sm("Diária Média",      s_brl(d_media),        "blue"),
        kcard_sm("Maior Diária",      s_brl(d_max),          "green"),
        kcard_sm("Menor Diária",      s_brl(d_min),          "orange"),
        kcard_sm("Variação no Mês",   d_var,                  "purple"),
        kcard_sm("Ticket Médio/Res.", s_brl(ticket),         "blue"),
        kcard_sm("Nº de Reservas",    as.character(nrow(df)), "orange"))
  })
  
  # ── Gráfico diária por reserva ────────────────────────────────
  output$g_diaria_reservas <- renderPlotly({
    df <- reservas_fil()
    validate(need(nrow(df)>0,"Sem reservas para o período."))
    df <- df |> dplyr::mutate(
      checkin  = as.Date(checkin), checkout = as.Date(checkout),
      label = paste0(format(as.Date(checkin),"%d/%m")," → ",format(as.Date(checkout),"%d/%m"),
                     "\n",as.integer(as.Date(checkout)-as.Date(checkin))," noites",
                     "\nDiária: R$ ",round(diaria_liquida))) |> dplyr::arrange(checkin)
    d_med <- mean(df$diaria_liquida,na.rm=TRUE)
    plot_ly() |>
      add_bars(data=df,x=~checkin,y=~diaria_liquida,
               marker=list(color=~diaria_liquida,
                           colorscale=list(c(0,"#bcd6f8"),c(.5,"#2684ff"),c(1,"#0052cc")),
                           showscale=FALSE,line=list(color="transparent")),
               text=~label,hovertemplate="%{text}<extra></extra>",name="Diária") |>
      add_lines(x=range(df$checkin,na.rm=TRUE),y=c(d_med,d_med),
                line=list(color="#e03e3e",dash="dash",width=1.5),
                name=paste0("Média: R$ ",round(d_med)),hoverinfo="skip") |>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=FALSE,zeroline=FALSE,tickformat="%d/%m",tickfont=list(size=10),title=""),
             yaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,tickprefix="R$ ",tickfont=list(size=10),title=""),
             margin=list(l=55,r=12,t=8,b=32),
             legend=list(x=0,y=1.12,orientation="h",font=list(size=10))) |>
      config(displayModeBar=FALSE)
  })
  
  # ── Lista diárias ─────────────────────────────────────────────
  output$lista_diarias <- renderUI({
    df <- reservas_fil()
    if (nrow(df)==0) return(p(class="sem-dados","Sem reservas para o período."))
    df <- dplyr::arrange(df,checkin)
    d_max <- max(df$diaria_liquida,na.rm=TRUE)
    p75 <- quantile(df$diaria_liquida,.75,na.rm=TRUE)
    p25 <- quantile(df$diaria_liquida,.25,na.rm=TRUE)
    items <- lapply(seq_len(min(nrow(df),15)),function(i){
      r      <- df[i,]
      noites <- as.integer(as.Date(r$checkout)-as.Date(r$checkin))
      pct_w  <- round(r$diaria_liquida/d_max*100)
      bcls   <- if(r$diaria_liquida>=p75) "diaria-badge db-alto"
      else if(r$diaria_liquida>=p25) "diaria-badge db-medio" else "diaria-badge db-baixo"
      btxt   <- if(r$diaria_liquida>=p75) "ALTO" else if(r$diaria_liquida>=p25) "MED" else "BAIXO"
      div(class="diaria-row",
          div(class="diaria-datas",tags$b(format(as.Date(r$checkin),"%d/%m"))," → ",
              format(as.Date(r$checkout),"%d/%m")),
          div(class="diaria-noites",paste0(noites," noites")),
          div(class="diaria-bar-wrap",div(class="diaria-bar",style=paste0("width:",pct_w,"%;"))),
          div(class="diaria-val",s_brl(r$diaria_liquida)),
          div(class=bcls,btxt))
    })
    div(!!!items)
  })
  
  # ── Painel operacional ────────────────────────────────────────
  output$painel_operacional <- renderUI({
    if (rv$op_aba=="despesas") {
      tagList(
        shinycssloaders::withSpinner(uiOutput("kpis_despesas"),type=4,color="#d97706"),
        div(class="cgrid",
            div(class="card",
                div(class="card-hdr",div(class="card-ttl","Despesas por Categoria"),
                    span(class="badge badge-orange","Pizza")),
                shinycssloaders::withSpinner(plotlyOutput("g_despesas_cat",height="220px"),type=4,color="#d97706")),
            div(class="card",
                div(class="card-hdr",div(class="card-ttl","Despesas por Apartamento"),
                    span(class="badge badge-orange","Barras")),
                shinycssloaders::withSpinner(plotlyOutput("g_despesas_apto",height="220px"),type=4,color="#d97706"))),
        div(class="card",
            div(class="card-hdr",div(class="card-ttl","Tabela de Despesas"),
                span(class="badge badge-orange","Mês selecionado")),
            div(class="tab-wrap",
                shinycssloaders::withSpinner(DTOutput("t_despesas"),type=4,color="#d97706"))))
      
    } else if (rv$op_aba=="custos") {
      tagList(
        shinycssloaders::withSpinner(uiOutput("kpis_custos"),type=4,color="#7c3aed"),
        div(class="card",
            div(class="card-hdr",div(class="card-ttl","Evolução de Custos por Apartamento"),
                span(class="badge badge-purple","Histórico")),
            shinycssloaders::withSpinner(plotlyOutput("g_custos_apto",height="240px"),type=4,color="#7c3aed")),
        div(class="card",
            div(class="card-hdr",div(class="card-ttl","Custos por Apartamento"),
                span(class="badge badge-purple","Mês selecionado")),
            div(class="tab-wrap",
                shinycssloaders::withSpinner(DTOutput("t_custos_apto"),type=4,color="#7c3aed"))))
      
    } else {
      tagList(
        shinycssloaders::withSpinner(uiOutput("kpis_os"),type=4,color="#0052cc"),
        div(class="card",
            div(class="card-hdr",div(class="card-ttl","Ordens de Serviço"),
                span(class="badge badge-blue","Mês selecionado")),
            div(class="tab-wrap",
                shinycssloaders::withSpinner(DTOutput("t_os"),type=4,color="#0052cc"))))
    }
  })
  
  output$kpis_despesas <- renderUI({
    des <- despesas_fil(); rep <- reposicao_fil()
    total_des <- if(nrow(des)>0) sum(des$valor,na.rm=TRUE) else 0
    total_rep <- if(nrow(rep)>0) sum(rep$valor_unitario_ou_total,na.rm=TRUE) else 0
    n_cat   <- if(nrow(des)>0&&"categoria"%in%names(des)) length(unique(des$categoria)) else 0
    n_apto  <- if(nrow(des)>0&&"imovel_nome"%in%names(des)) length(unique(des$imovel_nome)) else 0
    mai_cat <- if(nrow(des)>0&&"categoria"%in%names(des)){
      tc <- des|>dplyr::group_by(categoria)|>dplyr::summarise(v=sum(valor,na.rm=TRUE),.groups="drop")|>dplyr::arrange(dplyr::desc(v))
      if(nrow(tc)>0) tc$categoria[1] else "—"} else "—"
    div(class="kgrid-sm",
        kcard_sm("Total Despesas",  s_brl(total_des),           "orange"),
        kcard_sm("Total Reposição", s_brl(total_rep),           "purple"),
        kcard_sm("Nº Categorias",   as.character(n_cat),        "orange"),
        kcard_sm("Aptos Afetados",  as.character(n_apto),       "blue"),
        kcard_sm("Maior Categoria", mai_cat,                    "red"),
        kcard_sm("Total Operac.",   s_brl(total_des+total_rep), "green"))
  })
  output$g_despesas_cat <- renderPlotly({
    des <- despesas_fil()
    validate(need(nrow(des)>0&&"categoria"%in%names(des),"Sem dados de despesas."))
    df <- des|>dplyr::group_by(categoria)|>dplyr::summarise(total=sum(valor,na.rm=TRUE),.groups="drop")|>dplyr::arrange(dplyr::desc(total))
    cores <- c("#0052cc","#2684ff","#d97706","#7c3aed","#e03e3e","#00b388","#f59e0b","#9ca3af")
    plot_ly(df,labels=~categoria,values=~total,type="pie",
            marker=list(colors=cores,line=list(color="#fff",width=2)),
            textinfo="percent",textposition="inside",
            hovertemplate="%{label}<br>R$ %{value:,.0f}<extra></extra>")|>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",showlegend=TRUE,
             legend=list(font=list(size=10),orientation="v",x=1.02,y=.5),
             margin=list(l=0,r=120,t=10,b=10))|>config(displayModeBar=FALSE)
  })
  output$g_despesas_apto <- renderPlotly({
    des <- despesas_fil()
    validate(need(nrow(des)>0&&"imovel_nome"%in%names(des),"Sem dados de despesas."))
    df <- des|>dplyr::group_by(imovel_nome)|>dplyr::summarise(total=sum(valor,na.rm=TRUE),.groups="drop")|>dplyr::arrange(total)
    plot_ly(df,x=~total,y=~imovel_nome,type="bar",orientation="h",
            marker=list(color="#d97706",line=list(color="transparent")),
            hovertemplate="%{y}<br>R$ %{x:,.0f}<extra></extra>")|>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,tickprefix="R$ ",tickfont=list(size=10),title=""),
             yaxis=list(showgrid=FALSE,zeroline=FALSE,tickfont=list(size=10),title=""),
             margin=list(l=10,r=12,t=8,b=20),showlegend=FALSE)|>config(displayModeBar=FALSE)
  })
  output$t_despesas <- renderDT({
    des <- despesas_fil()
    validate(need(nrow(des)>0,"Sem despesas para o período/imóvel selecionado."))
    cols_base <- c("imovel_nome","categoria","descricao","data","competencia","valor")
    cols_ok   <- cols_base[cols_base%in%names(des)]
    df <- des|>dplyr::select(dplyr::all_of(cols_ok))|>
      dplyr::rename_with(~dplyr::recode(.,imovel_nome="Imóvel",categoria="Categoria",
                                        descricao="Descrição",data="Data",competencia="Competência",valor="Valor (R$)"))|>
      dplyr::mutate(dplyr::across(dplyr::any_of("Valor (R$)"),~brl(.x)))
    datatable(df,options=list(pageLength=10,dom="ftip",
                              language=list(search="Buscar:",paginate=list(previous="Ant.",`next`="Próx."))),
              rownames=FALSE,class="compact stripe hover",escape=FALSE)
  },server=FALSE)
  
  output$kpis_custos <- renderUI({
    d <- dados(); req(d,input$mes_sel)
    rec <- rec_fil()|>dplyr::filter(competencia==input$mes_sel)
    if(nrow(rec)==0) return(p(class="sem-dados","Sem dados."))
    total  <- sum(rec$outros_custos,na.rm=TRUE)
    maior  <- rec|>dplyr::arrange(dplyr::desc(outros_custos))|>dplyr::slice(1)
    n_ap   <- length(unique(rec$imovel))
    med    <- mean(rec$outros_custos,na.rm=TRUE)
    ratio  <- if(sum(rec$receita_bruta,na.rm=TRUE)>0)
      paste0(round(total/sum(rec$receita_bruta,na.rm=TRUE)*100),"%") else "—"
    div(class="kgrid-sm",
        kcard_sm("Total Custos",     s_brl(total),                  "purple"),
        kcard_sm("Custo/Apto Médio", s_brl(med),                    "blue"),
        kcard_sm("Aptos com Custo",  as.character(n_ap),            "orange"),
        kcard_sm("Custo/Receita",    ratio,                         "red"),
        kcard_sm("Apto c/ + Custo",  as.character(maior$imovel[1]%||%"—"), "purple"),
        kcard_sm("Custo Maior Apto", s_brl(maior$outros_custos[1]), "red"))
  })
  output$g_custos_apto <- renderPlotly({
    d <- dados(); req(d)
    df <- d$receitas|>dplyr::group_by(imovel,mes_label,mes)|>
      dplyr::summarise(custo=sum(outros_custos,na.rm=TRUE),.groups="drop")|>dplyr::arrange(mes)
    validate(need(nrow(df)>0,"Sem dados de custos."))
    aptos <- unique(df$imovel)
    cores <- c("#0052cc","#2684ff","#d97706","#7c3aed","#e03e3e","#00b388","#f59e0b","#9ca3af")
    traces <- lapply(seq_along(aptos),function(i){
      sub <- dplyr::filter(df,imovel==aptos[i])
      list(x=sub$mes_label,y=sub$custo,name=aptos[i],type="bar",
           marker=list(color=cores[((i-1)%%length(cores))+1],line=list(color="transparent")),
           hovertemplate=paste0(aptos[i],"<br>%{x}<br>R$ %{y:,.0f}<extra></extra>"))
    })
    p <- plot_ly()
    for(tr in traces) p <- do.call(add_trace,c(list(p),tr))
    p|>layout(barmode="group",paper_bgcolor="transparent",plot_bgcolor="transparent",
              xaxis=list(showgrid=FALSE,zeroline=FALSE,tickfont=list(size=10),title=""),
              yaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,tickprefix="R$ ",tickfont=list(size=10),title=""),
              margin=list(l=55,r=12,t=8,b=30),
              legend=list(x=0,y=1.15,orientation="h",font=list(size=10)))|>config(displayModeBar=FALSE)
  })
  output$t_custos_apto <- renderDT({
    d <- dados(); req(d,input$mes_sel)
    rec <- rec_fil()|>dplyr::filter(competencia==input$mes_sel)|>
      dplyr::transmute(
        `Imóvel`          = imovel,
        `Receita Bruta`   = brl(receita_bruta),
        `Outros Custos`   = brl(outros_custos),
        `% Custo/Receita` = paste0(round(ifelse(receita_bruta>0,outros_custos/receita_bruta*100,0)),"%"),
        `Resultado`       = brl(resultado_liq))
    datatable(rec,options=list(dom="t",paging=FALSE,ordering=FALSE),rownames=FALSE,class="compact stripe")
  },server=FALSE)
  
  output$kpis_os <- renderUI({
    man <- manutencao_fil()
    if(nrow(man)==0) return(p(class="sem-dados","Sem ordens de serviço para o período."))
    total <- sum(man$valor_total,na.rm=TRUE)
    n_os  <- nrow(man)
    n_ap  <- if("imovel_nome"%in%names(man)) length(unique(man$imovel_nome)) else "—"
    div(class="kgrid-4",
        kcard_sm("Total OS",    s_brl(total),               "orange"),
        kcard_sm("Nº de OS",    as.character(n_os),         "blue"),
        kcard_sm("Aptos",       as.character(n_ap),         "purple"),
        kcard_sm("Ticket Médio",s_brl(if(n_os>0) total/n_os else 0),"red"))
  })
  output$t_os <- renderDT({
    man <- manutencao_fil()
    validate(need(nrow(man)>0,"Sem ordens de serviço para o período."))
    cols_base <- c("imovel_nome","os_id","produto_servico","valor_total","competencia","data")
    cols_ok   <- cols_base[cols_base%in%names(man)]
    df <- man|>dplyr::select(dplyr::all_of(cols_ok))|>
      dplyr::rename_with(~dplyr::recode(.,imovel_nome="Imóvel",os_id="OS ID",
                                        produto_servico="Serviço",valor_total="Valor (R$)",competencia="Competência",data="Data"))|>
      dplyr::mutate(dplyr::across(dplyr::any_of("Valor (R$)"),~brl(.x)))
    datatable(df,options=list(pageLength=10,dom="ftip",
                              language=list(search="Buscar:",paginate=list(previous="Ant.",`next`="Próx."))),
              rownames=FALSE,class="compact stripe hover")
  },server=FALSE)
  
  # ── Gráficos temporais ────────────────────────────────────────
  output$g_diarias <- renderPlotly({
    d <- dados(); req(d,input$mes_sel)
    iid <- if(!is.null(input$imovel)&&nzchar(input$imovel)&&input$imovel!="all")
      input$imovel else if(length(d$imoveis_ids)>0) d$imoveis_ids[[1]] else return(NULL)
    cal <- d$calendario
    if(is.null(cal)||nrow(cal)==0) validate(need(FALSE,"Sem dados."))
    cal <- cal|>dplyr::filter(apto_original==iid|property_id==iid,
                              substr(as.character(data),1,7)==input$mes_sel)|>dplyr::arrange(data)
    validate(need(nrow(cal)>0,"Sem dados para o período."))
    plot_ly(cal,x=~as.Date(data),y=~valor,type="scatter",mode="lines+markers",
            line=list(color="#1a6ef7",width=2),
            marker=list(color=ifelse(cal$ocupado,"#1a6ef7","#d1d9e0"),size=6),
            hovertemplate="Dia %{x|%d/%m}<br>R$ %{y:,.0f}<extra></extra>")|>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=FALSE,zeroline=FALSE,tickformat="%d/%m",tickfont=list(size=10),title=""),
             yaxis=list(showgrid=TRUE,gridcolor="#f0f4f8",zeroline=FALSE,tickprefix="R$ ",tickfont=list(size=10),title=""),
             margin=list(l=52,r=10,t=8,b=30),showlegend=FALSE)|>config(displayModeBar=FALSE)
  })
  output$g_evolucao <- renderPlotly({
    df <- rec_fil()|>dplyr::group_by(mes,mes_label)|>
      dplyr::summarise(receita=sum(receita_bruta,na.rm=TRUE),resultado=sum(resultado_liq,na.rm=TRUE),.groups="drop")|>
      dplyr::arrange(mes)
    validate(need(nrow(df)>0,"Sem dados."))
    n <- nrow(df); cores <- c(rep("#c5d8f7",max(n-1,0)),"#1a6ef7")[1:n]
    plot_ly(df,x=~mes_label,y=~receita,type="bar",
            marker=list(color=cores,line=list(color="transparent")),name="Receita Bruta",
            hovertemplate="%{x}<br>Receita: R$ %{y:,.0f}<extra></extra>")|>
      add_trace(y=~resultado,type="scatter",mode="lines+markers",
                line=list(color="#00b388",width=2),marker=list(color="#00b388",size=6),
                name="Resultado Líq.",yaxis="y2",
                hovertemplate="%{x}<br>Resultado: R$ %{y:,.0f}<extra></extra>")|>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=FALSE,zeroline=FALSE,tickfont=list(size=10),title=""),
             yaxis=list(showgrid=TRUE,gridcolor="#f0f4f8",zeroline=FALSE,tickprefix="R$ ",tickfont=list(size=10),title=""),
             yaxis2=list(overlaying="y",side="right",showgrid=FALSE,zeroline=FALSE,tickprefix="R$ ",tickfont=list(size=9),title=""),
             margin=list(l=52,r=52,t=8,b=30),
             legend=list(x=0,y=1.15,orientation="h",font=list(size=10)))|>config(displayModeBar=FALSE)
  })
  
  # ── Acumulado ─────────────────────────────────────────────────
  output$acumulado <- renderUI({
    acum <- rec_fil()|>dplyr::summarise(rec=sum(receita_bruta,na.rm=TRUE),res=sum(resultado_liq,na.rm=TRUE))
    div(
      div(class="acg",
          div(div(class="al","RECEITA ACUMULADA"),div(class="av",s_brl(acum$rec)),div(class="ad","▲ base consolidada")),
          div(div(class="al","RESULTADO ACUMULADO"),div(class="av g",s_brl(acum$res)),div(class="ad","▲ base consolidada"))),
      shinycssloaders::withSpinner(plotlyOutput("g_acum",height="90px"),type=4,color="#00c49a"))
  })
  output$g_acum <- renderPlotly({
    df <- rec_fil()|>dplyr::group_by(mes)|>dplyr::summarise(rec=sum(receita_bruta,na.rm=TRUE),.groups="drop")|>
      dplyr::arrange(mes)|>dplyr::mutate(acum=cumsum(rec))
    validate(need(nrow(df)>0,""))
    plot_ly(df,x=~mes,y=~acum,type="scatter",mode="lines",fill="tozeroy",
            line=list(color="#00b388",width=2),fillcolor="rgba(0,179,136,0.12)",
            hovertemplate="%{x|%b/%Y}<br>Acum: R$ %{y:,.0f}<extra></extra>")|>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE,title=""),
             yaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE,title=""),
             margin=list(l=0,r=0,t=0,b=0),showlegend=FALSE)|>config(displayModeBar=FALSE)
  })
  
  # ── Histórico ─────────────────────────────────────────────────
  output$t_historico <- renderDT({
    df <- rec_fil()|>dplyr::arrange(dplyr::desc(mes))|>
      dplyr::transmute(
        `Imóvel`        = imovel,
        `Mês`           = mes_label,
        `Receita Bruta` = brl(receita_bruta),
        `Taxa Adm`      = paste0("- ",brl(taxa_adm)),
        `Outros Custos` = paste0("- ",brl(outros_custos)),
        `Resultado Líq` = brl(resultado_liq),
        `Ocupação`      = paste0(round(ocupacao),"%"),
        `Diária Média`  = brl(diaria_media),
        `Nº Diárias`    = n_diarias)
    datatable(df,options=list(pageLength=12,dom="frtip",
                              language=list(search="Buscar:",info="Mostrando _START_ a _END_ de _TOTAL_",
                                            paginate=list(previous="Anterior",`next`="Próximo"))),
              rownames=FALSE,class="compact stripe hover")
  },server=FALSE)
  
  # ═══════════════════════════════════════════════════════════
  # ABA 2 — VISÃO DA CARTEIRA
  # ═══════════════════════════════════════════════════════════
  output$body_carteira <- renderUI({
    req(input$mes_cart)
    mes_lbl <- { dt <- suppressWarnings(as.Date(paste0(input$mes_cart,"-01")))
    if(!is.na(dt)) format(dt,"%B/%Y") else input$mes_cart }
    tagList(
      div(class="sec","VISÃO CONSOLIDADA DA CARTEIRA",div(class="sec-badge",mes_lbl)),
      shinycssloaders::withSpinner(uiOutput("kpis_carteira"),type=4,color="#1a6ef7"),
      
      div(class="sec","RANKING DE PROPRIETÁRIOS"),
      div(class="cgrid",
          div(class="card",
              div(class="card-hdr",div(class="card-ttl","Receita por Proprietário"),
                  span(class="badge badge-blue","Mês selecionado")),
              uiOutput("ranking_proprietarios")),
          div(class="card",
              div(class="card-hdr",div(class="card-ttl","Ocupação por Proprietário"),
                  span(class="badge badge-green","Taxa média")),
              shinycssloaders::withSpinner(plotlyOutput("g_ocupacao_prop",height="300px"),type=4,color="#00b388"))),
      
      div(class="sec","RANKING DE IMÓVEIS",div(class="sec-badge","Top performers")),
      div(class="cgrid",
          div(class="card",
              div(class="card-hdr",div(class="card-ttl","Top 10 — Maior Receita"),
                  span(class="badge badge-blue","No mês")),
              uiOutput("ranking_imoveis_receita")),
          div(class="card",
              div(class="card-hdr",div(class="card-ttl","Top 10 — Maior Diária Média"),
                  span(class="badge badge-purple","No mês")),
              uiOutput("ranking_imoveis_diaria"))),
      
      div(class="sec","EVOLUÇÃO DA CARTEIRA"),
      div(class="card",
          div(class="card-hdr",div(class="card-ttl","Receita Total por Mês — Todos os Proprietários"),
              span(class="badge","Visão consolidada")),
          shinycssloaders::withSpinner(plotlyOutput("g_evolucao_carteira",height="260px"),type=4,color="#1a6ef7")),
      
      div(class="sec","COMPARATIVO DE OCUPAÇÃO"),
      div(class="cgrid",
          div(class="card",
              div(class="card-hdr",div(class="card-ttl","Distribuição de Ocupação"),
                  span(class="badge badge-teal","Todos os imóveis")),
              shinycssloaders::withSpinner(plotlyOutput("g_dist_ocupacao",height="240px"),type=4,color="#0891b2")),
          div(class="card",
              div(class="card-hdr",div(class="card-ttl","Receita × Resultado"),
                  span(class="badge badge-green","Por imóvel")),
              shinycssloaders::withSpinner(plotlyOutput("g_scatter_rec_res",height="240px"),type=4,color="#00b388"))),
      
      div(class="sec","TABELA COMPLETA DA CARTEIRA"),
      div(class="card",
          shinycssloaders::withSpinner(DTOutput("t_carteira"),type=4,color="#1a6ef7"))
    )
  })
  
  output$kpis_carteira <- renderUI({
    req(input$mes_cart)
    flat <- build_carteira_flat(rv$app_data)
    if(nrow(flat)==0) return(p(class="sem-dados","Sem dados."))
    mes <- dplyr::filter(flat,competencia==input$mes_cart)
    if(nrow(mes)==0) return(p(class="sem-dados","Sem dados para o mês selecionado."))
    div(class="kgrid",
        kcard("Receita Total",   s_brl_compact(sum(mes$receita_bruta,na.rm=TRUE)),
              paste(length(unique(mes$imovel)),"imóveis ativos")),
        kcard("Resultado Total", s_brl_compact(sum(mes$resultado_liq,na.rm=TRUE)),
              "após deduções",vg=TRUE),
        kcard("Ocupação Média",  s_pct(mean(mes$ocupacao,na.rm=TRUE)),  "carteira inteira"),
        kcard("Diária Média",    s_brl(mean(mes$diaria_media,na.rm=TRUE)), "carteira inteira"),
        kcard("Proprietários",   as.character(length(unique(mes$proprietario))),
              paste(length(unique(mes$imovel)),"imóveis ativos")))
  })
  
  output$ranking_proprietarios <- renderUI({
    req(input$mes_cart)
    flat <- build_carteira_flat(rv$app_data)
    df <- flat|>dplyr::filter(competencia==input$mes_cart)|>
      dplyr::group_by(proprietario)|>
      dplyr::summarise(receita=sum(receita_bruta,na.rm=TRUE),n_im=dplyr::first(n_imoveis),.groups="drop")|>
      dplyr::arrange(dplyr::desc(receita))
    if(nrow(df)==0) return(p(class="sem-dados","Sem dados."))
    mx <- max(df$receita,1); cls <- c("b1","b2","b3","b4","b5")
    items <- lapply(seq_len(nrow(df)),function(i){
      r <- df[i,]
      div(class="ri",div(class="rn",i),
          div(div(class="rname",r$proprietario),div(class="rsub",paste(r$n_im,"imóvel(is)"))),
          div(class="rbw",div(class=paste("rb",cls[min(i,5)]),style=paste0("width:",round(r$receita/mx*100),"%;"))),
          div(class="rval",s_brl_compact(r$receita)))
    })
    div(!!!items)
  })
  
  output$g_ocupacao_prop <- renderPlotly({
    req(input$mes_cart)
    flat <- build_carteira_flat(rv$app_data)
    df <- flat|>dplyr::filter(competencia==input$mes_cart)|>
      dplyr::group_by(proprietario)|>dplyr::summarise(ocp=mean(ocupacao,na.rm=TRUE),.groups="drop")|>
      dplyr::arrange(ocp)|>dplyr::mutate(lbl=substr(proprietario,1,22))
    validate(need(nrow(df)>0,"Sem dados."))
    cores <- ifelse(df$ocp>=70,"#00b388",ifelse(df$ocp>=50,"#1a6ef7","#e03e3e"))
    plot_ly(df,x=~ocp,y=~lbl,type="bar",orientation="h",
            marker=list(color=cores,line=list(color="transparent")),
            hovertemplate="%{y}<br>Ocupação: %{x:.1f}%<extra></extra>")|>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,ticksuffix="%",tickfont=list(size=10),title="",range=c(0,105)),
             yaxis=list(showgrid=FALSE,zeroline=FALSE,tickfont=list(size=10),title=""),
             margin=list(l=10,r=12,t=8,b=20),showlegend=FALSE)|>config(displayModeBar=FALSE)
  })
  
  output$ranking_imoveis_receita <- renderUI({
    req(input$mes_cart)
    flat <- build_carteira_flat(rv$app_data)
    df <- flat|>dplyr::filter(competencia==input$mes_cart)|>
      dplyr::arrange(dplyr::desc(receita_bruta))|>dplyr::slice(1:10)
    if(nrow(df)==0) return(p(class="sem-dados","Sem dados."))
    mx <- max(df$receita_bruta,1); cls <- c("b1","b2","b3","b4","b5")
    items <- lapply(seq_len(nrow(df)),function(i){
      r <- df[i,]
      div(class="ri",div(class="rn",i),
          div(div(class="rname",r$imovel),div(class="rsub",r$proprietario)),
          div(class="rbw",div(class=paste("rb",cls[min(i,5)]),style=paste0("width:",round(r$receita_bruta/mx*100),"%;"))),
          div(class="rval",s_brl_compact(r$receita_bruta)))
    })
    div(!!!items)
  })
  
  output$ranking_imoveis_diaria <- renderUI({
    req(input$mes_cart)
    flat <- build_carteira_flat(rv$app_data)
    df <- flat|>dplyr::filter(competencia==input$mes_cart,diaria_media>0)|>
      dplyr::arrange(dplyr::desc(diaria_media))|>dplyr::slice(1:10)
    if(nrow(df)==0) return(p(class="sem-dados","Sem dados."))
    mx <- max(df$diaria_media,1); cls <- c("b1","b2","b3","b4","b5")
    items <- lapply(seq_len(nrow(df)),function(i){
      r <- df[i,]
      div(class="ri",div(class="rn",i),
          div(div(class="rname",r$imovel),div(class="rsub",r$proprietario)),
          div(class="rbw",div(class=paste("rb",cls[min(i,5)]),style=paste0("width:",round(r$diaria_media/mx*100),"%;"))),
          div(class="rval",s_brl(r$diaria_media)))
    })
    div(!!!items)
  })
  
  output$g_evolucao_carteira <- renderPlotly({
    flat <- build_carteira_flat(rv$app_data)
    if(nrow(flat)==0) validate(need(FALSE,"Sem dados."))
    df <- flat|>dplyr::group_by(mes,mes_label)|>
      dplyr::summarise(receita=sum(receita_bruta,na.rm=TRUE),resultado=sum(resultado_liq,na.rm=TRUE),.groups="drop")|>
      dplyr::arrange(mes)
    validate(need(nrow(df)>0,"Sem dados."))
    n <- nrow(df)
    cores <- grDevices::colorRampPalette(c("#c5d8f7","#1a6ef7"))(n)
    plot_ly(df,x=~mes_label,y=~receita,type="bar",
            marker=list(color=cores,line=list(color="transparent")),name="Receita",
            hovertemplate="%{x}<br>R$ %{y:,.0f}<extra></extra>")|>
      add_trace(y=~resultado,type="scatter",mode="lines+markers",
                line=list(color="#00b388",width=2.5),marker=list(color="#00b388",size=7),
                name="Resultado",yaxis="y2",hovertemplate="%{x}<br>R$ %{y:,.0f}<extra></extra>")|>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=FALSE,zeroline=FALSE,tickfont=list(size=10),title=""),
             yaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,tickprefix="R$ ",tickfont=list(size=10),title=""),
             yaxis2=list(overlaying="y",side="right",showgrid=FALSE,zeroline=FALSE,tickprefix="R$ ",tickfont=list(size=9),title=""),
             margin=list(l=55,r=55,t=8,b=30),
             legend=list(x=0,y=1.12,orientation="h",font=list(size=10)))|>config(displayModeBar=FALSE)
  })
  
  output$g_dist_ocupacao <- renderPlotly({
    req(input$mes_cart)
    flat <- build_carteira_flat(rv$app_data)
    df <- dplyr::filter(flat,competencia==input$mes_cart)
    validate(need(nrow(df)>0,"Sem dados."))
    plot_ly(df,x=~ocupacao,type="histogram",nbinsx=15,
            marker=list(color="#0891b2",line=list(color="#fff",width=1)),
            hovertemplate="Ocupação: %{x:.0f}%<br>Imóveis: %{y}<extra></extra>")|>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=FALSE,zeroline=FALSE,ticksuffix="%",tickfont=list(size=10),title="Taxa de Ocupação (%)"),
             yaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,tickfont=list(size=10),title="Nº de imóveis"),
             margin=list(l=45,r=12,t=8,b=40),showlegend=FALSE)|>config(displayModeBar=FALSE)
  })
  
  output$g_scatter_rec_res <- renderPlotly({
    req(input$mes_cart)
    flat <- build_carteira_flat(rv$app_data)
    df <- dplyr::filter(flat,competencia==input$mes_cart)
    validate(need(nrow(df)>0,"Sem dados."))
    plot_ly(df,x=~receita_bruta,y=~resultado_liq,type="scatter",mode="markers",
            marker=list(size=10,color=~ocupacao,
                        colorscale=list(c(0,"#bcd6f8"),c(.5,"#1a6ef7"),c(1,"#0052cc")),
                        showscale=TRUE,colorbar=list(title="Ocup.%",len=0.6)),
            text=~imovel,hovertemplate="%{text}<br>Receita: R$ %{x:,.0f}<br>Resultado: R$ %{y:,.0f}<extra></extra>")|>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=TRUE,zerolinecolor="#e5e9ef",tickprefix="R$ ",tickfont=list(size=10),title="Receita Bruta"),
             yaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=TRUE,zerolinecolor="#e5e9ef",tickprefix="R$ ",tickfont=list(size=10),title="Resultado Líq."),
             margin=list(l=55,r=20,t=8,b=45),showlegend=FALSE)|>config(displayModeBar=FALSE)
  })
  
  output$t_carteira <- renderDT({
    req(input$mes_cart)
    flat <- build_carteira_flat(rv$app_data)
    df <- flat|>dplyr::filter(competencia==input$mes_cart)|>
      dplyr::arrange(proprietario,imovel)|>
      dplyr::transmute(
        `Proprietário` = proprietario,
        `Imóvel`       = imovel,
        `Receita`      = brl(receita_bruta),
        `Taxa Adm`     = brl(taxa_adm),
        `Custos`       = brl(outros_custos),
        `Resultado`    = brl(resultado_liq),
        `Ocupação`     = paste0(round(ocupacao),"%"),
        `Diária Média` = brl(diaria_media),
        `Nº Diárias`   = n_diarias)
    datatable(df,options=list(pageLength=20,dom="frtip",
                              language=list(search="Buscar:",info="Mostrando _START_ a _END_ de _TOTAL_",
                                            paginate=list(previous="Anterior",`next`="Próximo"))),
              rownames=FALSE,class="compact stripe hover")
  },server=FALSE)
  
  # ═══════════════════════════════════════════════════════════
  # ABA 3 — INSIGHTS & PROPOSTA DE VALOR
  # ═══════════════════════════════════════════════════════════
  output$body_insights <- renderUI({
    tagList(
      div(class="sec","DIAGNÓSTICO AUTOMÁTICO DA CARTEIRA"),
      shinycssloaders::withSpinner(uiOutput("insights_auto"),type=4,color="#1a6ef7"),
      
      div(class="sec","OPORTUNIDADES IDENTIFICADAS"),
      shinycssloaders::withSpinner(uiOutput("oportunidades"),type=4,color="#d97706"),
      
      div(class="sec","PROPOSTA DE VALOR BSBStay",div(class="sec-badge","Para apresentar ao proprietário")),
      div(class="pv-grid",
          div(class="pv-card",div(class="pv-icon","📈"),div(class="pv-title","Maximização de Receita"),
              div(class="pv-body","Gestão profissional de preços dinâmicos baseada em sazonalidade, eventos e demanda em tempo real."),
              shinycssloaders::withSpinner(uiOutput("pv_metric_receita"),type=4,color="#1a6ef7")),
          div(class="pv-card",div(class="pv-icon","🔧"),div(class="pv-title","Manutenção Preventiva"),
              div(class="pv-body","Acompanhamento de todas as ordens de serviço com histórico. Reduzimos o custo total com ação preventiva."),
              shinycssloaders::withSpinner(uiOutput("pv_metric_manutencao"),type=4,color="#d97706")),
          div(class="pv-card",div(class="pv-icon","🏆"),div(class="pv-title","Performance Comparativa"),
              div(class="pv-body","O proprietário recebe benchmarks contra a carteira completa. Mostramos onde o imóvel está e o que pode melhorar."),
              shinycssloaders::withSpinner(uiOutput("pv_metric_bench"),type=4,color="#00b388")),
          div(class="pv-card",div(class="pv-icon","📊"),div(class="pv-title","Transparência Total"),
              div(class="pv-body","Extrato detalhado com cada receita, despesa e reserva. O proprietário acessa em tempo real."),
              div(class="pv-metric","✓ Relatório automático disponível")),
          div(class="pv-card",div(class="pv-icon","💰"),div(class="pv-title","Controle de Custos"),
              div(class="pv-body","Monitoramos cada despesa por categoria e imóvel, agindo antes que impactem o resultado."),
              shinycssloaders::withSpinner(uiOutput("pv_metric_custos"),type=4,color="#7c3aed")),
          div(class="pv-card",div(class="pv-icon","🌟"),div(class="pv-title","Resultado Líquido"),
              div(class="pv-body","Foco em resultado líquido — não apenas receita. Gerenciamos a equação completa."),
              shinycssloaders::withSpinner(uiOutput("pv_metric_resultado"),type=4,color="#00b388"))),
      
      div(class="sec","BENCHMARK — IMÓVEL VS. MÉDIA DA CARTEIRA"),
      div(class="card",
          div(class="card-hdr",div(class="card-ttl","Selecione um imóvel para comparar"),
              span(class="badge badge-blue","Versus média da carteira")),
          selectInput("bench_imovel",NULL,
                      choices=c("Selecione..."="",
                                setNames(unlist(lapply(rv$app_data,function(d) d$imoveis_ids)),
                                         unlist(lapply(rv$app_data,function(d) d$imoveis_ids)))),
                      width="300px"),
          shinycssloaders::withSpinner(uiOutput("benchmark_imovel"),type=4,color="#1a6ef7")),
      shinycssloaders::withSpinner(uiOutput("sec_benchmark_graficos"),type=4,color="#7c3aed")
    )
  })
  
  output$insights_auto <- renderUI({
    flat <- build_carteira_flat(rv$app_data)
    if(nrow(flat)==0) return(p(class="sem-dados","Sem dados."))
    ultimo <- max(flat$competencia,na.rm=TRUE)
    mes    <- dplyr::filter(flat,competencia==ultimo)
    ocp    <- mean(mes$ocupacao,na.rm=TRUE)
    dar    <- mean(mes$diaria_media,na.rm=TRUE)
    rec    <- sum(mes$receita_bruta,na.rm=TRUE)
    res    <- sum(mes$resultado_liq,na.rm=TRUE)
    cus    <- sum(mes$outros_custos,na.rm=TRUE)
    custo_r <- if(rec>0) cus/rec*100 else 0
    top    <- mes|>dplyr::slice_max(receita_bruta,n=1,with_ties=FALSE)
    bot    <- mes|>dplyr::slice_min(ocupacao,n=1,with_ties=FALSE)
    prem   <- mes|>dplyr::slice_max(diaria_media,n=1,with_ties=FALSE)
    ab50   <- sum(mes$ocupacao<50,na.rm=TRUE)
    margem <- if(rec>0) res/rec*100 else 0
    div(class="insights-grid",
        insight_card("📊",paste0("Carteira ativa: ",nrow(mes)," imóveis"),
                     paste0("Ocupação média de ",round(ocp,1),"% e diária média de ",s_brl(dar)," no último período."),
                     if(ocp>=65)"green" else if(ocp>=45)"blue" else "orange"),
        insight_card("🏆",paste0("Top performer: ",top$imovel),
                     paste0("Maior receita: ",s_brl_compact(top$receita_bruta)," com ",round(top$ocupacao),"% ocupação."),"blue"),
        insight_card(if(custo_r>30)"⚠️" else "✅",paste0("Custo/Receita: ",round(custo_r,1),"%"),
                     paste0("Custos representam ",round(custo_r,1),"% da receita bruta. ",
                            if(custo_r>30)"Avaliar oportunidades de redução." else "Índice saudável."),
                     if(custo_r>30)"orange" else "green"),
        insight_card(if(ab50>0)"⚠️" else "✅",paste0(ab50," imóvel(is) com ocupação < 50%"),
                     if(ab50>0) paste0("'",bot$imovel,"' teve apenas ",round(bot$ocupacao),"% de ocupação. Revisar precificação.")
                     else "Todos os imóveis acima de 50% de ocupação no período.",
                     if(ab50>0)"orange" else "green"),
        insight_card("💎",paste0("Diária premium: ",prem$imovel),
                     paste0("Maior diária: ",s_brl(prem$diaria_media)," — ",round(prem$ocupacao),"% de ocupação."),"purple"),
        insight_card("💵",paste0("Resultado total: ",s_brl_compact(res)),
                     paste0("Receita de ",s_brl_compact(rec)," com margem líquida de ",round(margem,1),"%."),
                     if(margem>60)"green" else if(margem>40)"blue" else "red"))
  })
  
  output$oportunidades <- renderUI({
    flat <- build_carteira_flat(rv$app_data)
    if(nrow(flat)==0) return(p(class="sem-dados","Sem dados."))
    ultimo <- max(flat$competencia,na.rm=TRUE)
    mes    <- dplyr::filter(flat,competencia==ultimo)
    ocp_m  <- mean(mes$ocupacao,na.rm=TRUE)
    dar_m  <- mean(mes$diaria_media,na.rm=TRUE)
    alertas <- list()
    abaixo <- mes|>dplyr::filter(ocupacao<ocp_m*.8)|>dplyr::arrange(ocupacao)
    if(nrow(abaixo)>0) for(i in seq_len(min(nrow(abaixo),3))){
      r <- abaixo[i,]
      alertas <- c(alertas,list(div(class="alert-row alert-warn",div(class="alert-icon","⚠️"),
                                    div(tags$b(r$imovel)," — Ocupação abaixo da média: ",tags$b(paste0(round(r$ocupacao),"%")),
                                        " vs. média ",tags$b(paste0(round(ocp_m),"%")),". Sugestão: revisar preço ou ampliar canais."))))
    }
    precif <- mes|>dplyr::filter(diaria_media>dar_m*1.3,ocupacao<ocp_m*.7)
    if(nrow(precif)>0) for(i in seq_len(min(nrow(precif),2))){
      r <- precif[i,]
      alertas <- c(alertas,list(div(class="alert-row alert-warn",div(class="alert-icon","💡"),
                                    div(tags$b(r$imovel)," — Diária alta (",s_brl(r$diaria_media),") mas ocupação baixa (",
                                        round(r$ocupacao),"%). Possível precificação fora do mercado."))))
    }
    tops <- mes|>dplyr::filter(ocupacao>ocp_m*1.2,diaria_media>dar_m)|>dplyr::arrange(dplyr::desc(receita_bruta))
    if(nrow(tops)>0) for(i in seq_len(min(nrow(tops),2))){
      r <- tops[i,]
      alertas <- c(alertas,list(div(class="alert-row alert-ok",div(class="alert-icon","🌟"),
                                    div(tags$b(r$imovel)," — Performance excelente: ",round(r$ocupacao),"% ocupação + diária ",
                                        s_brl(r$diaria_media),". Bom caso para apresentar a novos proprietários."))))
    }
    if(length(alertas)==0)
      alertas <- list(div(class="alert-row alert-ok",div(class="alert-icon","✅"),
                          div("Nenhuma oportunidade crítica identificada. Carteira com bom desempenho geral.")))
    div(!!!alertas)
  })
  
  output$pv_metric_receita <- renderUI({
    flat <- build_carteira_flat(rv$app_data); if(nrow(flat)==0) return(NULL)
    div(class="pv-metric",paste0("📈 Total gerenciado: ",s_brl_compact(sum(flat$receita_bruta,na.rm=TRUE))))
  })
  output$pv_metric_manutencao <- renderUI({
    flat <- build_carteira_flat(rv$app_data)
    div(class="pv-metric",paste0("🔧 ",length(unique(flat$imovel))," imóveis monitorados"))
  })
  output$pv_metric_bench <- renderUI({
    flat <- build_carteira_flat(rv$app_data); if(nrow(flat)==0) return(NULL)
    div(class="pv-metric",paste0("🏆 Ocupação média: ",round(mean(flat$ocupacao,na.rm=TRUE),1),"%"))
  })
  output$pv_metric_custos <- renderUI({
    flat <- build_carteira_flat(rv$app_data); if(nrow(flat)==0) return(NULL)
    rec <- sum(flat$receita_bruta,na.rm=TRUE); cus <- sum(flat$outros_custos,na.rm=TRUE)
    div(class="pv-metric",paste0("💰 Custo/Receita: ",if(rec>0) round(cus/rec*100,1) else 0,"%"))
  })
  output$pv_metric_resultado <- renderUI({
    flat <- build_carteira_flat(rv$app_data); if(nrow(flat)==0) return(NULL)
    rec <- sum(flat$receita_bruta,na.rm=TRUE); res <- sum(flat$resultado_liq,na.rm=TRUE)
    div(class="pv-metric",paste0("🌟 Margem líquida: ",if(rec>0) round(res/rec*100,1) else 0,"%"))
  })
  
  output$benchmark_imovel <- renderUI({
    req(input$bench_imovel,nzchar(input$bench_imovel))
    flat <- build_carteira_flat(rv$app_data); if(nrow(flat)==0) return(NULL)
    ultimo <- max(flat$competencia,na.rm=TRUE)
    mes_df <- dplyr::filter(flat,competencia==ultimo)
    im_df  <- dplyr::filter(mes_df,imovel==input$bench_imovel)
    if(nrow(im_df)==0) return(p(class="sem-dados","Imóvel sem dados no período mais recente."))
    im  <- im_df[1,]
    med <- dplyr::summarise(mes_df,
                            rec=mean(receita_bruta,na.rm=TRUE),res=mean(resultado_liq,na.rm=TRUE),
                            ocp=mean(ocupacao,na.rm=TRUE),dar=mean(diaria_media,na.rm=TRUE))
    mk <- function(val,mval){
      diff <- val-mval; pct_d <- if(mval>0) diff/mval*100 else 0
      list(sinal=if(diff>=0)"▲ +" else "▼ ",pct=abs(round(pct_d,1)),cls=if(diff>=0)"fv g" else "fv r")
    }
    metricas <- list(
      list(lbl="Receita Bruta",  val=s_brl(im$receita_bruta),med=s_brl(med$rec), cp=mk(im$receita_bruta,med$rec)),
      list(lbl="Resultado Líq.", val=s_brl(im$resultado_liq),med=s_brl(med$res), cp=mk(im$resultado_liq,med$res)),
      list(lbl="Ocupação",       val=s_pct(im$ocupacao),      med=s_pct(med$ocp), cp=mk(im$ocupacao,med$ocp)),
      list(lbl="Diária Média",   val=s_brl(im$diaria_media),  med=s_brl(med$dar), cp=mk(im$diaria_media,med$dar)))
    div(style="display:grid;grid-template-columns:repeat(4,1fr);gap:12px;margin-top:14px;",
        lapply(metricas,function(mt){
          div(class="kcard",
              div(class="klbl",mt$lbl),
              div(class="kval",mt$val),
              div(style="font-size:11px;color:#6b7280;margin-top:4px;",paste0("Média: ",mt$med)),
              div(class=mt$cp$cls,style="font-size:11px;margin-top:2px;font-weight:700;",
                  paste0(mt$cp$sinal,mt$cp$pct,"% vs. média")))
        }))
  })
  
  output$sec_benchmark_graficos <- renderUI({
    req(input$bench_imovel,nzchar(input$bench_imovel))
    div(class="cgrid",
        div(class="card",
            div(class="card-hdr",div(class="card-ttl","Receita: Imóvel vs. Média"),span(class="badge badge-blue","Evolução")),
            shinycssloaders::withSpinner(plotlyOutput("g_bench_receita",height="220px"),type=4,color="#1a6ef7")),
        div(class="card",
            div(class="card-hdr",div(class="card-ttl","Ocupação: Imóvel vs. Média"),span(class="badge badge-green","Evolução")),
            shinycssloaders::withSpinner(plotlyOutput("g_bench_ocupacao",height="220px"),type=4,color="#00b388")))
  })
  
  output$g_bench_receita <- renderPlotly({
    req(input$bench_imovel,nzchar(input$bench_imovel))
    flat <- build_carteira_flat(rv$app_data); if(nrow(flat)==0) validate(need(FALSE,""))
    im_df  <- flat|>dplyr::filter(imovel==input$bench_imovel)|>dplyr::arrange(mes)
    med_df <- flat|>dplyr::group_by(mes,mes_label)|>
      dplyr::summarise(rec_med=mean(receita_bruta,na.rm=TRUE),.groups="drop")|>dplyr::arrange(mes)
    validate(need(nrow(im_df)>0,"Sem dados para este imóvel."))
    plot_ly()|>
      add_lines(data=med_df,x=~mes_label,y=~rec_med,
                line=list(color="#d1d9e0",width=2,dash="dash"),name="Média Carteira",
                hovertemplate="%{x}<br>Média: R$ %{y:,.0f}<extra></extra>")|>
      add_lines(data=im_df,x=~mes_label,y=~receita_bruta,
                line=list(color="#1a6ef7",width=2.5),marker=list(color="#1a6ef7",size=7),
                name=input$bench_imovel,hovertemplate="%{x}<br>R$ %{y:,.0f}<extra></extra>")|>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=FALSE,zeroline=FALSE,tickfont=list(size=10),title=""),
             yaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,tickprefix="R$ ",tickfont=list(size=10),title=""),
             margin=list(l=55,r=12,t=8,b=30),
             legend=list(x=0,y=1.15,orientation="h",font=list(size=10)))|>config(displayModeBar=FALSE)
  })
  
  output$g_bench_ocupacao <- renderPlotly({
    req(input$bench_imovel,nzchar(input$bench_imovel))
    flat <- build_carteira_flat(rv$app_data); if(nrow(flat)==0) validate(need(FALSE,""))
    im_df  <- flat|>dplyr::filter(imovel==input$bench_imovel)|>dplyr::arrange(mes)
    med_df <- flat|>dplyr::group_by(mes,mes_label)|>
      dplyr::summarise(ocp_med=mean(ocupacao,na.rm=TRUE),.groups="drop")|>dplyr::arrange(mes)
    validate(need(nrow(im_df)>0,"Sem dados."))
    plot_ly()|>
      add_lines(data=med_df,x=~mes_label,y=~ocp_med,
                line=list(color="#d1d9e0",width=2,dash="dash"),name="Média Carteira",
                hovertemplate="%{x}<br>Média: %{y:.1f}%<extra></extra>")|>
      add_lines(data=im_df,x=~mes_label,y=~ocupacao,
                line=list(color="#00b388",width=2.5),marker=list(color="#00b388",size=7),
                name=input$bench_imovel,hovertemplate="%{x}<br>%{y:.1f}%<extra></extra>")|>
      layout(paper_bgcolor="transparent",plot_bgcolor="transparent",
             xaxis=list(showgrid=FALSE,zeroline=FALSE,tickfont=list(size=10),title=""),
             yaxis=list(showgrid=TRUE,gridcolor="#f3f6f9",zeroline=FALSE,ticksuffix="%",tickfont=list(size=10),title=""),
             margin=list(l=45,r=12,t=8,b=30),
             legend=list(x=0,y=1.15,orientation="h",font=list(size=10)))|>config(displayModeBar=FALSE)
  })
  
} # fim server

app <- shinyApp(ui, server)
