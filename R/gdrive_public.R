# ============================================================
# gdrive_public.R  —  BSBStay Shiny integração Google Drive
# v3.0 — Inclui fact_manutencao, fact_reposicao, fact_despesas
#         no objeto final por proprietário
# Download base R + SQLite cache. Sem httr2, sem Google Cloud.
# ============================================================

# -- Ambiente / paths ------------------------------------------
APP_ROOT <- get0("APP_ROOT", ifnotfound = normalizePath(Sys.getenv("APP_ROOT", "."), winslash = "/", mustWork = FALSE))
DATA_DIR <- normalizePath(Sys.getenv("APP_DATA_DIR", file.path(APP_ROOT, "data")), winslash = "/", mustWork = FALSE)
CACHE_DIR <- normalizePath(Sys.getenv("APP_CACHE_DIR", file.path(DATA_DIR, "cache")), winslash = "/", mustWork = FALSE)
RAW_DIR <- normalizePath(Sys.getenv("APP_RAW_DIR", file.path(DATA_DIR, "raw")), winslash = "/", mustWork = FALSE)
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)

# -- Constantes ------------------------------------------------
DRIVE_FOLDER_ID <- Sys.getenv("DRIVE_FOLDER_ID", unset = "1753AZxwmyyWYS2oYQPLeMHIz5gM8bscb")
DRIVE_FILE_URL  <- Sys.getenv("DRIVE_FILE_URL", unset = "")
DRIVE_FILE_ID   <- Sys.getenv("DRIVE_FILE_ID", unset = "1fnereY6JOrAbSl1yw_o_U94Fb0KTuHGJU85GEUrCBiU")

CACHE_XLSX      <- file.path(CACHE_DIR, "db_master_drive.xlsx")
SQLITE_PATH     <- file.path(CACHE_DIR, "bsbstay.sqlite")
CACHE_META_KEY  <- "last_drive_sync"
MAX_CACHE_AGE_H <- suppressWarnings(as.numeric(Sys.getenv("MAX_CACHE_AGE_H", "6")))
if (is.na(MAX_CACHE_AGE_H) || MAX_CACHE_AGE_H <= 0) MAX_CACHE_AGE_H <- 6

# -- Pacotes ---------------------------------------------------
.ensure_pkgs <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    stop(
      "Pacotes ausentes no ambiente do container: ",
      paste(miss, collapse = ", "),
      ". Refaça o build da imagem Docker."
    )
  }
  invisible(TRUE)
}
.ensure_pkgs(c("readxl", "DBI", "RSQLite", "dplyr", "lubridate", "tidyr", "janitor"))

# -- Utilitários -----------------------------------------------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

extrair_file_id <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) return("")
  if (!grepl("https?://", x, ignore.case = TRUE)) return(x)
  pats <- c("/spreadsheets/d/([a-zA-Z0-9_-]+)",
            "[?&]id=([a-zA-Z0-9_-]+)")
  for (pt in pats) {
    m <- regexec(pt, x, perl = TRUE)
    r <- regmatches(x, m)[[1]]
    if (length(r) >= 2 && nzchar(r[2])) return(r[2])
  }
  ""
}

if (!nzchar(trimws(DRIVE_FILE_ID %||% "")) && nzchar(trimws(DRIVE_FILE_URL %||% ""))) {
  DRIVE_FILE_ID <- extrair_file_id(DRIVE_FILE_URL)
}


parse_date_safe <- function(x) {
  if (is.null(x) || all(is.na(x))) return(as.Date(NA))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXt"))) return(as.Date(x))
  if (is.numeric(x)) return(suppressWarnings(as.Date(as.numeric(x), origin = "1899-12-30")))
  if (is.character(x)) {
    num_try  <- suppressWarnings(as.numeric(x))
    is_serial <- !is.na(num_try) & num_try > 40000 & num_try < 60000
    out <- as.Date(rep(NA, length(x)))
    if (any(is_serial, na.rm = TRUE))
      out[is_serial] <- as.Date(num_try[is_serial], origin = "1899-12-30")
    if (any(!is_serial & !is.na(x), na.rm = TRUE))
      out[!is_serial & !is.na(x)] <- suppressWarnings(as.Date(x[!is_serial & !is.na(x)]))
    return(out)
  }
  suppressWarnings(as.Date(as.character(x)))
}

normalizar_cpf_cnpj <- function(x) trimws(as.character(x))

# -- URLs de download ------------------------------------------
urls_para_file_id <- function(file_id) {
  c(
    paste0("https://docs.google.com/spreadsheets/d/", file_id, "/export?format=xlsx"),
    paste0("https://docs.google.com/spreadsheets/d/", file_id, "/export?format=xlsx&id=", file_id),
    paste0("https://drive.google.com/uc?export=download&id=", file_id, "&confirm=t"),
    paste0("https://drive.google.com/uc?export=download&id=", file_id),
    paste0("https://drive.usercontent.google.com/download?id=", file_id, "&export=download&confirm=t")
  )
}

# -- Download binário com retry e validação por readxl ---------
baixar_url_base <- function(urls, destino, timeout_s = 120) {
  metodos <- unique(c("libcurl", "auto", "curl", if (.Platform$OS.type == "windows") "wininet"))
  tentativas <- character(0)
  validar_xlsx <- function(path) {
    ok <- tryCatch({
      sh <- readxl::excel_sheets(path)
      length(sh) > 0
    }, error = function(e) FALSE)
    isTRUE(ok)
  }
  for (url in urls) {
    for (met in metodos) {
      tentativas <- c(tentativas, sprintf("%s [%s]", url, met))
      ok <- tryCatch({
        tmp <- tempfile(fileext = ".xlsx")
        old_to <- getOption("timeout"); options(timeout = timeout_s)
        on.exit(options(timeout = old_to), add = TRUE)
        st <- utils::download.file(url, tmp, mode = "wb", quiet = TRUE, method = met)
        if (!identical(st, 0L) || !file.exists(tmp) || file.info(tmp)$size <= 0) { unlink(tmp); return(FALSE) }
        dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
        file.copy(tmp, destino, overwrite = TRUE)
        unlink(tmp)
        if (!validar_xlsx(destino)) { unlink(destino); return(FALSE) }
        TRUE
      }, error = function(e) FALSE)
      if (isTRUE(ok)) return(list(ok = TRUE, tentativas = tentativas))
    }
  }
  list(ok = FALSE, tentativas = tentativas)
}

# -- Download principal -----------------------------------------
baixar_db_master_publico <- function(file_id = DRIVE_FILE_ID, destino = CACHE_XLSX,
                                     forcar = FALSE, timeout_s = 120) {
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  if (!forcar && file.exists(destino)) {
    idade_h <- as.numeric(difftime(Sys.time(), file.mtime(destino), units = "hours"))
    if (idade_h < MAX_CACHE_AGE_H) return(list(ok = TRUE, path = destino, source = "cache"))
  }
  fid <- extrair_file_id(file_id %||% DRIVE_FILE_URL)
  if (!nzchar(fid)) {
    candidatos <- if (dir.exists(RAW_DIR)) list.files(RAW_DIR, pattern = "\.(xlsx|xls)$", full.names = TRUE) else character(0)
    if (length(candidatos) > 0) {
      file.copy(candidatos[1], destino, overwrite = TRUE)
      return(list(ok = TRUE, path = destino, source = "local_raw"))
    }
    return(list(ok = FALSE, path = NULL, source = "erro", msg = "DRIVE_FILE_ID/DRIVE_FILE_URL nao configurado e nenhum xlsx encontrado em data/raw/"))
  }
  dl <- baixar_url_base(urls_para_file_id(fid), destino, timeout_s)
  if (isTRUE(dl$ok)) return(list(ok = TRUE, path = destino, source = "drive"))
  candidatos <- if (dir.exists(RAW_DIR)) list.files(RAW_DIR, pattern = "\.(xlsx|xls)$", full.names = TRUE) else character(0)
  if (length(candidatos) > 0) {
    file.copy(candidatos[1], destino, overwrite = TRUE)
    return(list(ok = TRUE, path = destino, source = "local_raw"))
  }
  if (file.exists(destino)) return(list(ok = TRUE, path = destino, source = "cache_old"))
  list(
    ok = FALSE,
    path = NULL,
    source = "erro",
    msg = paste0(
      "Nao foi possivel baixar/validar o xlsx do Google Drive. ",
      "Verifique se a planilha esta acessivel pelo link publico de exportacao ou defina DRIVE_FILE_URL. ",
      "Tentativas: ", paste(unique(dl$tentativas), collapse = " | ")
    )
  )
}

# -- SQLite helpers ---------------------------------------------
sqlite_connect     <- function(path = SQLITE_PATH) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  DBI::dbConnect(RSQLite::SQLite(), path)
}
sqlite_get_meta    <- function(key, con = NULL) {
  own <- is.null(con); if (own) con <- sqlite_connect(); on.exit(if (own) DBI::dbDisconnect(con))
  if (!DBI::dbExistsTable(con, "meta")) return(NA_character_)
  res <- DBI::dbGetQuery(con, "SELECT value FROM meta WHERE key=?", params = list(key))
  if (nrow(res) == 0) NA_character_ else res$value[[1]]
}
sqlite_set_meta    <- function(key, value, con = NULL) {
  own <- is.null(con); if (own) con <- sqlite_connect(); on.exit(if (own) DBI::dbDisconnect(con))
  if (!DBI::dbExistsTable(con, "meta"))
    DBI::dbExecute(con, "CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT)")
  DBI::dbExecute(con, "INSERT OR REPLACE INTO meta(key,value) VALUES(?,?)", params = list(key, as.character(value)))
  invisible(TRUE)
}
sqlite_write_table <- function(df, table_name, con = NULL) {
  own <- is.null(con); if (own) con <- sqlite_connect(); on.exit(if (own) DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, table_name, df, overwrite = TRUE); invisible(TRUE)
}
sqlite_read_table  <- function(table_name, con = NULL) {
  own <- is.null(con); if (own) con <- sqlite_connect(); on.exit(if (own) DBI::dbDisconnect(con))
  if (!DBI::dbExistsTable(con, table_name)) return(NULL)
  DBI::dbReadTable(con, table_name)
}
sqlite_tables_exist <- function(tables, con = NULL) {
  own <- is.null(con); if (own) con <- sqlite_connect(); on.exit(if (own) DBI::dbDisconnect(con))
  all(vapply(tables, DBI::dbExistsTable, logical(1), conn = con))
}

# -- Leitura e normalização do xlsx ----------------------------
#
# ESTRUTURA REAL confirmada do [DB] BSBStay_VF.xlsx:
#
#   dim_proprietario : owner_id, cpf_cnpj, nome_proprietario
#   dim_imovel       : property_id, owner_id, nome_canonico, empreendimento, unidade
#   agg_prestacao_contas : competencia, cpf_cnpj, nome_proprietario, owner_id, property_id,
#                          nome_canonico, empreendimento, unidade, noites_no_mes, taxa_ocupacao,
#                          receita_liquida, diaria_media, tx_adm, manutencao_total,
#                          reposicao_total, despesas_total, custos_total, resultado
#   fact_reservas    : competencia, property_id, checkin, checkout, noites_no_mes,
#                      diaria_liquida, receita_liquida_mes
#   fact_manutencao  : competencia, property_id, tipo_servico, descricao, data, valor_total,
#                      prestador (se disponível)
#   fact_reposicao   : competencia, property_id, item, quantidade, valor_unitario_ou_total
#   fact_despesas    : competencia, property_id, categoria, descricao, data, valor
#   fact_repasse     : competencia, property_id, valor, comissao
#
ler_e_processar_db_master <- function(path_xlsx) {
  stopifnot(file.exists(path_xlsx))
  sheets_ok <- readxl::excel_sheets(path_xlsx)
  
  obrig <- c("dim_proprietario", "dim_imovel", "fact_reservas",
             "fact_manutencao", "fact_reposicao", "fact_repasse",
             "fact_despesas", "agg_prestacao_contas")
  falt <- setdiff(obrig, sheets_ok)
  if (length(falt)) stop("Abas faltantes no xlsx: ", paste(falt, collapse = ", "))
  
  out <- setNames(lapply(obrig, function(sh) {
    df <- readxl::read_excel(path_xlsx, sheet = sh, guess_max = 5000)
    janitor::clean_names(df)
  }), obrig)
  
  # -- dim_proprietario --
  out$dim_proprietario <- out$dim_proprietario |>
    dplyr::mutate(
      owner_id          = format(as.numeric(owner_id), scientific = FALSE, trim = TRUE),
      cpf_cnpj          = normalizar_cpf_cnpj(cpf_cnpj),
      nome_proprietario = as.character(nome_proprietario)
    ) |>
    dplyr::filter(!is.na(owner_id), !is.na(cpf_cnpj), nzchar(cpf_cnpj))
  
  # -- dim_imovel --
  out$dim_imovel <- out$dim_imovel |>
    dplyr::mutate(
      property_id    = as.character(property_id),
      owner_id       = format(as.numeric(owner_id), scientific = FALSE, trim = TRUE),
      nome_canonico  = as.character(nome_canonico),
      empreendimento = as.character(empreendimento),
      unidade        = ifelse(is.na(unidade), NA_character_, gsub("\\.0$", "", as.character(unidade)))
    ) |>
    dplyr::filter(!is.na(property_id), nzchar(property_id))
  
  # -- agg_prestacao_contas --
  out$agg_prestacao_contas <- out$agg_prestacao_contas |>
    dplyr::mutate(
      competencia       = format(parse_date_safe(competencia), "%Y-%m"),
      cpf_cnpj          = normalizar_cpf_cnpj(cpf_cnpj),
      nome_proprietario = as.character(nome_proprietario),
      owner_id          = format(as.numeric(owner_id), scientific = FALSE, trim = TRUE),
      property_id       = as.character(property_id),
      nome_canonico     = as.character(nome_canonico),
      empreendimento    = as.character(empreendimento),
      unidade           = ifelse(is.na(unidade), NA_character_, gsub("\\.0$", "", as.character(unidade))),
      dplyr::across(c(noites_no_mes, dias_no_mes, taxa_ocupacao, reservas,
                      receita_liquida, diaria_media, comissao_pct, tx_adm,
                      manutencao_total, reposicao_total, despesas_total,
                      custos_total, resultado, itens_reposicao, qtd_itens),
                    ~ suppressWarnings(as.numeric(.x)))
    ) |>
    dplyr::filter(!is.na(cpf_cnpj), nzchar(cpf_cnpj), !is.na(competencia))
  
  # -- fact_reservas --
  out$fact_reservas <- out$fact_reservas |>
    dplyr::mutate(
      competencia         = format(parse_date_safe(competencia), "%Y-%m"),
      property_id         = as.character(property_id),
      checkin             = as.character(parse_date_safe(checkin)),
      checkout            = as.character(parse_date_safe(checkout)),
      noites_total        = suppressWarnings(as.numeric(noites_total)),
      noites_no_mes       = suppressWarnings(as.numeric(noites_no_mes)),
      diaria_liquida      = suppressWarnings(as.numeric(diaria_liquida)),
      receita_liquida_mes = suppressWarnings(as.numeric(receita_liquida_mes))
    ) |>
    dplyr::filter(!is.na(property_id), !is.na(checkin), !is.na(checkout))
  
  # -- fact_manutencao --
  manut_data_src <- if ("data" %in% names(out$fact_manutencao)) {
    out$fact_manutencao[["data"]]
  } else {
    out$fact_manutencao[["competencia"]]
  }

  out$fact_manutencao <- out$fact_manutencao |>
    dplyr::mutate(
      competencia     = format(parse_date_safe(competencia), "%Y-%m"),
      property_id     = as.character(property_id),
      valor_total     = suppressWarnings(as.numeric(valor_total)),
      data            = as.character(parse_date_safe(manut_data_src)),
      os_id           = if ("os_id"          %in% names(out$fact_manutencao)) as.character(os_id) else NA_character_,
      produto_servico = if ("produto_servico" %in% names(out$fact_manutencao)) as.character(produto_servico) else NA_character_
    )
  
  # -- fact_reposicao --
  out$fact_reposicao <- out$fact_reposicao |>
    dplyr::mutate(
      competencia             = format(parse_date_safe(competencia), "%Y-%m"),
      property_id             = as.character(property_id),
      quantidade              = suppressWarnings(as.numeric(quantidade)),
      valor_unitario_ou_total = suppressWarnings(as.numeric(valor_unitario_ou_total))
    )
  
  # -- fact_repasse --
  out$fact_repasse <- out$fact_repasse |>
    dplyr::mutate(
      competencia = format(parse_date_safe(competencia), "%Y-%m"),
      property_id = as.character(property_id),
      valor       = suppressWarnings(as.numeric(valor)),
      comissao    = suppressWarnings(as.numeric(comissao))
    )
  
  # -- fact_despesas --
  desp_data_src <- if ("data" %in% names(out$fact_despesas)) {
    out$fact_despesas[["data"]]
  } else {
    out$fact_despesas[["competencia"]]
  }

  out$fact_despesas <- out$fact_despesas |>
    dplyr::mutate(
      competencia = format(parse_date_safe(competencia), "%Y-%m"),
      property_id = as.character(property_id),
      valor       = suppressWarnings(as.numeric(valor)),
      data        = as.character(parse_date_safe(desp_data_src))
    )
  
  out
}

# -- Pipeline principal -----------------------------------------
carregar_dados_app <- function(
    file_id    = DRIVE_FILE_ID,
    folder_id  = DRIVE_FOLDER_ID,
    forcar_dl  = FALSE,
    forcar_etl = FALSE
) {
  con <- sqlite_connect(); on.exit(DBI::dbDisconnect(con))
  
  tabs_ok   <- sqlite_tables_exist(c("agg_prestacao_contas", "dim_imovel", "meta"), con)
  last_sync <- sqlite_get_meta(CACHE_META_KEY, con)
  age_h     <- if (!is.na(last_sync))
    as.numeric(difftime(Sys.time(), as.POSIXct(last_sync), units = "hours")) else Inf
  
  if (!forcar_dl && !forcar_etl && tabs_ok && age_h < MAX_CACHE_AGE_H) {
    message(sprintf("[Cache] SQLite fresco (%.1fh). Carregando.", age_h))
    return(montar_objeto_app_sqlite(con))
  }
  
  dl <- baixar_db_master_publico(file_id = file_id, forcar = forcar_dl)
  if (!dl$ok) stop(dl$msg)
  
  message(sprintf("[Download] Fonte de dados: %s | arquivo: %s", dl$source, dl$path))
  message("[ETL] Processando planilha...")
  db <- tryCatch(
    ler_e_processar_db_master(dl$path),
    error = function(e) stop("Falha ao processar a planilha baixada: ", e$message)
  )
  for (nm in names(db)) if (is.data.frame(db[[nm]])) sqlite_write_table(db[[nm]], nm, con)
  sqlite_set_meta(CACHE_META_KEY, format(Sys.time()), con)
  
  message("[ETL] Concluido.")
  montar_objeto_app_sqlite(con)
}

# -- Monta objeto app a partir do SQLite -----------------------
#
# FLUXO DE DADOS (v3):
#   agg_prestacao_contas  → owners + receitas (métricas mensais)
#   dim_imovel            → portfolio (lista de imóveis com cpf_cnpj)
#   fact_reservas         → calendario (diárias diárias expandidas) + reservas (nível reserva)
#   fact_manutencao       → manutencao (ordens de serviço) — JOIN via property_id -> cpf_cnpj
#   fact_reposicao        → reposicao  (itens de reposição)
#   fact_despesas         → despesas   (despesas operacionais categorizadas)
#
montar_objeto_app_sqlite <- function(con) {
  
  message("[Montar] Lendo tabelas do SQLite...")
  agg        <- sqlite_read_table("agg_prestacao_contas", con)
  dim_prop   <- sqlite_read_table("dim_proprietario",     con)
  dim_imovel <- sqlite_read_table("dim_imovel",           con)
  reservas   <- sqlite_read_table("fact_reservas",        con)
  manutencao <- sqlite_read_table("fact_manutencao",      con)
  reposicao  <- sqlite_read_table("fact_reposicao",       con)
  despesas   <- sqlite_read_table("fact_despesas",        con)
  
  if (is.null(agg) || nrow(agg) == 0)
    stop("Tabela agg_prestacao_contas vazia. Apague o SQLite e reinicie.")
  
  # -- 1. Normaliza agg ------------------------------------------
  agg <- tryCatch({
    agg |>
      dplyr::mutate(
        cpf_cnpj      = normalizar_cpf_cnpj(cpf_cnpj),
        competencia   = as.character(competencia),
        mes           = suppressWarnings(as.Date(paste0(substr(competencia, 1, 7), "-01"))),
        mes_label     = format(mes, "%b/%Y"),
        imovel        = as.character(nome_canonico),
        receita_bruta = dplyr::coalesce(as.numeric(receita_liquida),  0),
        taxa_adm      = dplyr::coalesce(as.numeric(tx_adm),           0),
        outros_custos = dplyr::coalesce(as.numeric(custos_total),     0) +
          dplyr::coalesce(as.numeric(manutencao_total), 0) +
          dplyr::coalesce(as.numeric(reposicao_total),  0) +
          dplyr::coalesce(as.numeric(despesas_total),   0),
        resultado_liq = dplyr::coalesce(as.numeric(resultado),        0),
        ocupacao      = round(dplyr::coalesce(as.numeric(taxa_ocupacao), 0) * 100),
        diaria_media  = dplyr::coalesce(as.numeric(diaria_media),     0),
        n_diarias     = dplyr::coalesce(suppressWarnings(as.integer(as.numeric(noites_no_mes))), 0L)
      ) |>
      dplyr::filter(!is.na(cpf_cnpj), nzchar(cpf_cnpj), !is.na(mes))
  }, error = function(e) stop("Erro ao normalizar agg: ", e$message))
  
  # -- 2. Portfolio ----------------------------------------------
  portfolio <- tryCatch({
    if (!is.null(dim_imovel) && nrow(dim_imovel) > 0 &&
        !is.null(dim_prop)   && nrow(dim_prop)   > 0) {
      prop_cpf <- dim_prop |>
        dplyr::transmute(owner_id = as.character(owner_id),
                         cpf_cnpj = normalizar_cpf_cnpj(cpf_cnpj)) |>
        dplyr::filter(!is.na(owner_id), !is.na(cpf_cnpj))
      
      dim_imovel |>
        dplyr::mutate(owner_id = as.character(owner_id)) |>
        dplyr::left_join(prop_cpf, by = "owner_id") |>
        dplyr::filter(!is.na(cpf_cnpj)) |>
        dplyr::transmute(
          cpf_cnpj, owner_id, property_id,
          id          = as.character(nome_canonico),
          nome        = as.character(nome_canonico),
          bairro      = as.character(dplyr::coalesce(empreendimento, nome_canonico)),
          tipo        = as.character(dplyr::coalesce(unidade, empreendimento, nome_canonico)),
          plataformas = "Airbnb / Booking / Direta"
        )
    } else data.frame()
  }, error = function(e) { message("AVISO portfolio: ", e$message); data.frame() })
  
  # -- Mapa property_id → cpf_cnpj (usado pelos fatos) ----------
  pid_map <- if (nrow(portfolio) > 0)
    portfolio |>
    dplyr::select(property_id, cpf_cnpj, imovel_nome = nome) |>
    dplyr::distinct(property_id, .keep_all = TRUE)
  else
    data.frame(property_id = character(), cpf_cnpj = character(), imovel_nome = character())
  
  # -- 3. Calendario (expansão diária das reservas) --------------
  calendario <- tryCatch({
    if (!is.null(reservas) && nrow(reservas) > 0 &&
        all(c("checkin", "checkout", "property_id") %in% names(reservas))) {
      
      res_clean <- reservas |>
        dplyr::mutate(
          checkin  = parse_date_safe(checkin),
          checkout = parse_date_safe(checkout),
          valor    = dplyr::coalesce(as.numeric(diaria_liquida), 0)
        ) |>
        dplyr::filter(!is.na(checkin), !is.na(checkout), checkout > checkin) |>
        dplyr::left_join(pid_map, by = "property_id")
      
      if (nrow(res_clean) == 0) return(data.frame())
      
      dias_list <- lapply(seq_len(nrow(res_clean)), function(i) {
        r     <- res_clean[i, ]
        datas <- tryCatch(seq.Date(r$checkin, r$checkout - 1, by = "day"),
                          error = function(e) as.Date(character(0)))
        if (length(datas) == 0) return(NULL)
        data.frame(
          cpf_cnpj      = r$cpf_cnpj      %||% NA_character_,
          property_id   = r$property_id,
          apto_original = r$imovel_nome   %||% NA_character_,
          data          = datas,
          valor         = r$valor,
          ocupado       = TRUE,
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, Filter(Negate(is.null), dias_list))
    } else data.frame()
  }, error = function(e) { message("AVISO calendario: ", e$message); data.frame() })
  
  # -- 4. Reservas nível reserva (para análise de diária entre check-ins) --
  # Cada linha = 1 reserva; usado para calcular intervalos entre check-ins
  reservas_clean <- tryCatch({
    if (!is.null(reservas) && nrow(reservas) > 0) {
      reservas |>
        dplyr::mutate(
          checkin        = parse_date_safe(checkin),
          checkout       = parse_date_safe(checkout),
          diaria_liquida = dplyr::coalesce(as.numeric(diaria_liquida), 0),
          noites_total   = dplyr::coalesce(as.numeric(noites_total),   0),
          receita_total  = dplyr::coalesce(as.numeric(receita_liquida_mes), 0)
        ) |>
        dplyr::filter(!is.na(checkin), !is.na(checkout), checkout > checkin) |>
        dplyr::left_join(pid_map, by = "property_id") |>
        dplyr::filter(!is.na(cpf_cnpj))
    } else data.frame()
  }, error = function(e) { message("AVISO reservas_clean: ", e$message); data.frame() })
  
  # -- 5. Manutenção / Ordens de serviço -------------------------
  # JOIN: fact_manutencao.property_id → pid_map.property_id → cpf_cnpj
  manutencao_clean <- tryCatch({
    if (!is.null(manutencao) && nrow(manutencao) > 0) {
      df <- manutencao |>
        dplyr::mutate(
          property_id     = as.character(property_id),
          valor_total     = dplyr::coalesce(as.numeric(valor_total), 0),
          competencia     = as.character(competencia),
          os_id           = if ("os_id"          %in% names(manutencao)) as.character(os_id) else NA_character_,
          produto_servico = if ("produto_servico" %in% names(manutencao)) as.character(produto_servico) else NA_character_
        ) |>
        dplyr::left_join(pid_map, by = "property_id") |>
        dplyr::filter(!is.na(cpf_cnpj))
      df
    } else data.frame()
  }, error = function(e) { message("AVISO manutencao: ", e$message); data.frame() })
  
  # -- 6. Reposição ----------------------------------------------
  reposicao_clean <- tryCatch({
    if (!is.null(reposicao) && nrow(reposicao) > 0) {
      reposicao |>
        dplyr::mutate(
          property_id             = as.character(property_id),
          quantidade              = dplyr::coalesce(as.numeric(quantidade), 0),
          valor_unitario_ou_total = dplyr::coalesce(as.numeric(valor_unitario_ou_total), 0),
          competencia             = as.character(competencia)
        ) |>
        dplyr::left_join(pid_map, by = "property_id") |>
        dplyr::filter(!is.na(cpf_cnpj))
    } else data.frame()
  }, error = function(e) { message("AVISO reposicao: ", e$message); data.frame() })
  
  # -- 7. Despesas operacionais ----------------------------------
  # Inferência de categoria: se a coluna "categoria" não existe na base,
  # tentamos derivá-la de "tipo", "descricao" ou similar; caso contrário, "Outras"
  despesas_clean <- tryCatch({
    if (!is.null(despesas) && nrow(despesas) > 0) {
      df <- despesas |>
        dplyr::mutate(
          property_id = as.character(property_id),
          valor       = dplyr::coalesce(as.numeric(valor), 0),
          competencia = as.character(competencia)
        ) |>
        dplyr::left_join(pid_map, by = "property_id") |>
        dplyr::filter(!is.na(cpf_cnpj))
      
      # Garantir coluna "categoria" — usa "tipo" se existir, senão "Outras"
      if (!"categoria" %in% names(df)) {
        if ("tipo" %in% names(df)) {
          df <- dplyr::rename(df, categoria = tipo)
        } else {
          df <- dplyr::mutate(df, categoria = "Outras")
        }
      }
      df <- dplyr::mutate(df, categoria = dplyr::coalesce(as.character(categoria), "Outras"))
      df
    } else data.frame()
  }, error = function(e) { message("AVISO despesas: ", e$message); data.frame() })
  
  # -- 8. Owners -------------------------------------------------
  owners <- agg |>
    dplyr::distinct(cpf_cnpj, nome_proprietario) |>
    dplyr::filter(!is.na(cpf_cnpj), nzchar(cpf_cnpj))
  
  if (nrow(portfolio) > 0) {
    n_im  <- portfolio |> dplyr::count(cpf_cnpj, name = "n_imoveis")
    owners <- dplyr::left_join(owners, n_im, by = "cpf_cnpj")
  }
  owners <- owners |>
    dplyr::mutate(
      n_imoveis = dplyr::coalesce(suppressWarnings(as.integer(as.numeric(n_imoveis))), 1L),
      perfil    = dplyr::case_when(
        n_imoveis >= 4 ~ "Expansao Acelerada",
        n_imoveis == 3 ~ "Carteira Diversificada",
        TRUE           ~ "Portfolio Concentrado"
      )
    )
  
  # -- 9. Lista final por cpf_cnpj -------------------------------
  obj <- lapply(owners$cpf_cnpj, function(cpf) {
    orow <- owners |> dplyr::filter(cpf_cnpj == cpf) |> dplyr::slice(1)
    port <- if (nrow(portfolio) > 0) portfolio |> dplyr::filter(cpf_cnpj == cpf) else data.frame()
    recs <- agg |> dplyr::filter(cpf_cnpj == cpf)
    cal  <- if (!is.null(calendario) && nrow(calendario) > 0)
      calendario |> dplyr::filter(cpf_cnpj == cpf) else data.frame()
    resv <- if (nrow(reservas_clean) > 0) reservas_clean |> dplyr::filter(cpf_cnpj == cpf) else data.frame()
    man  <- if (nrow(manutencao_clean) > 0) manutencao_clean |> dplyr::filter(cpf_cnpj == cpf) else data.frame()
    rep  <- if (nrow(reposicao_clean) > 0)  reposicao_clean  |> dplyr::filter(cpf_cnpj == cpf) else data.frame()
    des  <- if (nrow(despesas_clean) > 0)   despesas_clean   |> dplyr::filter(cpf_cnpj == cpf) else data.frame()
    
    cfg <- if (nrow(port) > 0)
      lapply(seq_len(nrow(port)), function(i) as.list(port[i, c("id", "nome", "bairro", "tipo", "plataformas")]))
    else list()
    
    list(
      proprietario = orow$nome_proprietario[[1]],
      email        = NA_character_,
      perfil       = orow$perfil[[1]],
      cnpj         = cpf,
      imoveis_ids  = if (nrow(port) > 0) port$id else character(0),
      imoveis_cfg  = cfg,
      receitas     = recs,        # métricas mensais (agg)
      calendario   = cal,         # diárias diárias expandidas
      reservas     = resv,        # nível de reserva (check-in/out + diária)
      manutencao   = man,         # ordens de serviço
      reposicao    = rep,         # itens de reposição
      despesas     = des          # despesas operacionais categorizadas
    )
  })
  
  message(sprintf("[App] %d proprietario(s) carregado(s).", length(obj)))
  stats::setNames(obj, owners$cpf_cnpj)
}

# Status ---------------------------------------------------------
status_cache <- function() {
  con <- sqlite_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  last <- sqlite_get_meta(CACHE_META_KEY, con)
  tabelas <- tryCatch(DBI::dbListTables(con), error = function(e) character(0))

  cache_age_h <- NA_real_
  if (!is.null(last) && length(last) > 0 && !all(is.na(last))) {
    cache_age_h <- tryCatch(
      round(as.numeric(difftime(Sys.time(), as.POSIXct(last), units = "hours")), 1),
      error = function(e) NA_real_
    )
  }

  list(
    last_sync = last,
    cache_age_h = cache_age_h,
    xlsx_cache = file.exists(CACHE_XLSX),
    sqlite_path = SQLITE_PATH,
    tabelas = tabelas
  )
}

# Diagnostico ----------------------------------------------------
diagnostico_drive <- function(file_id = DRIVE_FILE_ID) {
  cat("\n=============================================\n")
  cat("  BSBStay - Diagnostico v4\n")
  cat("=============================================\n\n")
  cat("1. Rede... ")

  ok_net <- tryCatch({
    tmp <- tempfile(fileext = ".html")
    on.exit(unlink(tmp), add = TRUE)
    old_to <- getOption("timeout")
    options(timeout = 10)
    on.exit(options(timeout = old_to), add = TRUE)
    utils::download.file("https://www.google.com", tmp, quiet = TRUE, method = "libcurl")
    TRUE
  }, error = function(e) FALSE)

  cat(if (isTRUE(ok_net)) "OK\n" else "SEM REDE\n")

  fid <- trimws(file_id %||% "")
  fid_msg <- if (nzchar(fid)) paste("OK:", fid) else "NAO CONFIGURADO"
  cat(sprintf("2. DRIVE_FILE_ID... %s\n", fid_msg))

  st <- tryCatch(status_cache(), error = function(e) NULL)
  sqlite_msg <- "Vazio"
  if (!is.null(st) && length(st$tabelas) > 0) {
    sqlite_msg <- sprintf("%d tabelas, sync: %s", length(st$tabelas), st$last_sync %||% "nunca")
  }
  cat(sprintf("3. SQLite... %s\n", sqlite_msg))

  invisible(NULL)
}

# Fallback manual ------------------------------------------------
carregar_xlsx_local <- function(path_xlsx) {

  if (!file.exists(path_xlsx)) stop("Arquivo nao encontrado: ", path_xlsx)
  dir.create(dirname(CACHE_XLSX), recursive = TRUE, showWarnings = FALSE)
  file.copy(path_xlsx, CACHE_XLSX, overwrite = TRUE)
  con <- sqlite_connect(); on.exit(DBI::dbDisconnect(con))
  db <- ler_e_processar_db_master(CACHE_XLSX)
  for (nm in names(db)) {
    if (is.data.frame(db[[nm]])) sqlite_write_table(db[[nm]], nm, con)
  }
  sqlite_set_meta(CACHE_META_KEY, format(Sys.time()), con)
  montar_objeto_app_sqlite(con)
}
