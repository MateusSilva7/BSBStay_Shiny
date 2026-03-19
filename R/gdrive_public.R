# ============================================================
# gdrive_public.R  —  BSBStay Shiny integração Google Drive
# v3.1 — Correções Render:
#   - parse_date_safe em todas as datas críticas
#   - correção de ambiguidade no mutate() de fact_manutencao
#   - correção de ambiguidade no mutate() de fact_despesas
#   - loops com chaves explícitas para evitar erro de parse
# ============================================================

# ── Ambiente / paths ──────────────────────────────────────────
APP_ROOT <- get0(
  "APP_ROOT",
  ifnotfound = normalizePath(Sys.getenv("APP_ROOT", "."), winslash = "/", mustWork = FALSE)
)

DATA_DIR <- normalizePath(
  Sys.getenv("APP_DATA_DIR", file.path(APP_ROOT, "data")),
  winslash = "/",
  mustWork = FALSE
)

CACHE_DIR <- normalizePath(
  Sys.getenv("APP_CACHE_DIR", file.path(DATA_DIR, "cache")),
  winslash = "/",
  mustWork = FALSE
)

RAW_DIR <- normalizePath(
  Sys.getenv("APP_RAW_DIR", file.path(DATA_DIR, "raw")),
  winslash = "/",
  mustWork = FALSE
)

dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)

# ── Constantes ────────────────────────────────────────────────
DRIVE_FOLDER_ID <- Sys.getenv("DRIVE_FOLDER_ID", unset = "1753AZxwmyyWYS2oYQPLeMHIz5gM8bscb")
DRIVE_FILE_ID   <- Sys.getenv("DRIVE_FILE_ID",   unset = "1fnereY6JOrAbSl1yw_o_U94Fb0KTuHGJU85GEUrCBiU")

CACHE_XLSX      <- file.path(CACHE_DIR, "db_master_drive.xlsx")
SQLITE_PATH     <- file.path(CACHE_DIR, "bsbstay.sqlite")
CACHE_META_KEY  <- "last_drive_sync"

MAX_CACHE_AGE_H <- suppressWarnings(as.numeric(Sys.getenv("MAX_CACHE_AGE_H", "6")))
if (is.na(MAX_CACHE_AGE_H) || MAX_CACHE_AGE_H <= 0) MAX_CACHE_AGE_H <- 6

# ── Pacotes ───────────────────────────────────────────────────
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

# ── Utilitários ───────────────────────────────────────────────
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

parse_date_safe <- function(x) {
  if (is.null(x) || all(is.na(x))) return(as.Date(NA))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXt"))) return(as.Date(x))
  
  if (is.numeric(x)) {
    return(suppressWarnings(as.Date(as.numeric(x), origin = "1899-12-30")))
  }
  
  if (is.character(x)) {
    num_try   <- suppressWarnings(as.numeric(x))
    is_serial <- !is.na(num_try) & num_try > 40000 & num_try < 60000
    out       <- as.Date(rep(NA, length(x)))
    
    if (any(is_serial, na.rm = TRUE)) {
      out[is_serial] <- as.Date(num_try[is_serial], origin = "1899-12-30")
    }
    
    if (any(!is_serial & !is.na(x), na.rm = TRUE)) {
      out[!is_serial & !is.na(x)] <- suppressWarnings(as.Date(x[!is_serial & !is.na(x)]))
    }
    
    return(out)
  }
  
  suppressWarnings(as.Date(as.character(x)))
}

normalizar_cpf_cnpj <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[^0-9]", "", x)
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  x
}

# ── URLs de download ──────────────────────────────────────────
urls_para_file_id <- function(file_id) {
  c(
    paste0("https://docs.google.com/spreadsheets/d/", file_id, "/export?format=xlsx"),
    paste0("https://docs.google.com/spreadsheets/d/", file_id, "/export?format=xlsx&id=", file_id),
    paste0("https://drive.google.com/uc?export=download&id=", file_id, "&confirm=t"),
    paste0("https://drive.google.com/uc?export=download&id=", file_id),
    paste0("https://drive.usercontent.google.com/download?id=", file_id, "&export=download&confirm=t")
  )
}

# ── Download binário base R com retry ─────────────────────────
baixar_url_base <- function(urls, destino, timeout_s = 120) {
  metodos <- unique(c("libcurl", "auto", "curl", if (.Platform$OS.type == "windows") "wininet"))
  
  for (url in urls) {
    for (met in metodos) {
      ok <- tryCatch({
        tmp <- tempfile(fileext = ".xlsx")
        
        old_to <- getOption("timeout")
        options(timeout = timeout_s)
        on.exit(options(timeout = old_to), add = TRUE)
        
        st <- utils::download.file(url, tmp, mode = "wb", quiet = TRUE, method = met)
        if (st != 0) {
          unlink(tmp)
          return(NULL)
        }
        
        sig <- readBin(tmp, raw(), n = 4)
        is_zip  <- identical(sig, as.raw(c(0x50, 0x4B, 0x03, 0x04)))
        is_ole2 <- identical(sig, as.raw(c(0xD0, 0xCF, 0x11, 0xE0)))
        
        if (!is_zip && !is_ole2) {
          unlink(tmp)
          return(NULL)
        }
        
        dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
        file.copy(tmp, destino, overwrite = TRUE)
        unlink(tmp)
        TRUE
      }, error = function(e) NULL)
      
      if (isTRUE(ok)) return(TRUE)
    }
  }
  
  FALSE
}

# ── Download principal ─────────────────────────────────────────
baixar_db_master_publico <- function(
    file_id  = DRIVE_FILE_ID,
    destino  = CACHE_XLSX,
    forcar   = FALSE,
    timeout_s = 120
) {
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  
  if (!forcar && file.exists(destino)) {
    idade_h <- as.numeric(difftime(Sys.time(), file.mtime(destino), units = "hours"))
    if (idade_h < MAX_CACHE_AGE_H) {
      return(list(ok = TRUE, path = destino, source = "cache"))
    }
  }
  
  fid <- trimws(file_id %||% "")
  if (!nzchar(fid)) {
    raw_dir <- RAW_DIR
    candidatos <- if (dir.exists(raw_dir)) {
      list.files(raw_dir, pattern = "\\.(xlsx|xls)$", full.names = TRUE)
    } else {
      character(0)
    }
    
    if (length(candidatos) > 0) {
      file.copy(candidatos[1], destino, overwrite = TRUE)
      return(list(ok = TRUE, path = destino, source = "local_raw"))
    }
    
    return(list(ok = FALSE, path = NULL, source = "erro", msg = "DRIVE_FILE_ID nao configurado"))
  }
  
  ok <- baixar_url_base(urls_para_file_id(fid), destino, timeout_s)
  if (ok) {
    return(list(ok = TRUE, path = destino, source = "drive"))
  }
  
  raw_dir <- RAW_DIR
  candidatos <- if (dir.exists(raw_dir)) {
    list.files(raw_dir, pattern = "\\.(xlsx|xls)$", full.names = TRUE)
  } else {
    character(0)
  }
  
  if (length(candidatos) > 0) {
    file.copy(candidatos[1], destino, overwrite = TRUE)
    return(list(ok = TRUE, path = destino, source = "local_raw"))
  }
  
  if (file.exists(destino)) {
    return(list(ok = TRUE, path = destino, source = "cache_old"))
  }
  
  list(ok = FALSE, path = NULL, source = "erro", msg = "Nao foi possivel baixar. Coloque o xlsx em data/raw/")
}

# ── SQLite helpers ─────────────────────────────────────────────
sqlite_connect <- function(path = SQLITE_PATH) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  DBI::dbConnect(RSQLite::SQLite(), path)
}

sqlite_get_meta <- function(key, con = NULL) {
  own <- is.null(con)
  if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  
  if (!DBI::dbExistsTable(con, "meta")) return(NA_character_)
  res <- DBI::dbGetQuery(con, "SELECT value FROM meta WHERE key=?", params = list(key))
  if (nrow(res) == 0) NA_character_ else res$value[[1]]
}

sqlite_set_meta <- function(key, value, con = NULL) {
  own <- is.null(con)
  if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  
  if (!DBI::dbExistsTable(con, "meta")) {
    DBI::dbExecute(con, "CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT)")
  }
  
  DBI::dbExecute(
    con,
    "INSERT OR REPLACE INTO meta(key,value) VALUES(?,?)",
    params = list(key, as.character(value))
  )
  
  invisible(TRUE)
}

sqlite_write_table <- function(df, table_name, con = NULL) {
  own <- is.null(con)
  if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  
  DBI::dbWriteTable(con, table_name, df, overwrite = TRUE)
  invisible(TRUE)
}

sqlite_read_table <- function(table_name, con = NULL) {
  own <- is.null(con)
  if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  
  if (!DBI::dbExistsTable(con, table_name)) return(NULL)
  DBI::dbReadTable(con, table_name)
}

# ── Excel helpers ──────────────────────────────────────────────
listar_abas_excel <- function(path_xlsx) {
  readxl::excel_sheets(path_xlsx)
}

ler_aba_segura <- function(path_xlsx, sheet) {
  tryCatch(
    readxl::read_excel(path_xlsx, sheet = sheet) |>
      janitor::clean_names(),
    error = function(e) NULL
  )
}

achar_aba <- function(sheets, candidatos) {
  sheets_l <- tolower(trimws(sheets))
  cand_l   <- tolower(trimws(candidatos))
  
  idx_exato <- match(cand_l, sheets_l, nomatch = 0)
  if (any(idx_exato > 0)) return(sheets[idx_exato[idx_exato > 0][1]])
  
  for (c in cand_l) {
    idx <- grep(c, sheets_l, fixed = TRUE)
    if (length(idx)) return(sheets[idx[1]])
  }
  
  NA_character_
}

coluna_mais_proxima <- function(df, candidatos) {
  nms <- names(df)
  nms_l <- tolower(nms)
  cand_l <- tolower(candidatos)
  
  idx_exato <- match(cand_l, nms_l, nomatch = 0)
  if (any(idx_exato > 0)) return(nms[idx_exato[idx_exato > 0][1]])
  
  for (c in cand_l) {
    idx <- grep(c, nms_l, fixed = TRUE)
    if (length(idx)) return(nms[idx[1]])
  }
  
  NA_character_
}

# ── Normalização de dataframes ─────────────────────────────────
padronizar_dim_imovel <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(
      imovel_id = character(0),
      nome_imovel = character(0),
      cnpj = character(0),
      proprietario = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  col_imovel_id   <- coluna_mais_proxima(df, c("imovel_id", "id_imovel", "id"))
  col_nome_imovel <- coluna_mais_proxima(df, c("nome_imovel", "imovel", "nome"))
  col_cnpj        <- coluna_mais_proxima(df, c("cnpj"))
  col_prop        <- coluna_mais_proxima(df, c("proprietario", "nome_proprietario", "owner"))
  
  out <- data.frame(
    imovel_id     = as.character(df[[col_imovel_id %||% 1]]),
    nome_imovel   = as.character(df[[col_nome_imovel %||% 1]]),
    cnpj          = if (!is.na(col_cnpj)) normalizar_cpf_cnpj(df[[col_cnpj]]) else NA_character_,
    proprietario  = if (!is.na(col_prop)) as.character(df[[col_prop]]) else NA_character_,
    stringsAsFactors = FALSE
  )
  
  out$imovel_id   <- trimws(out$imovel_id)
  out$nome_imovel <- trimws(out$nome_imovel)
  out$proprietario <- trimws(out$proprietario)
  out
}

padronizar_dim_proprietario <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(
      owner_id = character(0),
      nome_proprietario = character(0),
      cpf_cnpj = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  col_owner_id <- coluna_mais_proxima(df, c("owner_id", "id_proprietario", "id"))
  col_nome     <- coluna_mais_proxima(df, c("nome_proprietario", "proprietario", "nome"))
  col_doc      <- coluna_mais_proxima(df, c("cpf_cnpj", "cpf", "cnpj", "documento"))
  
  out <- data.frame(
    owner_id          = as.character(df[[col_owner_id %||% 1]]),
    nome_proprietario = as.character(df[[col_nome %||% 1]]),
    cpf_cnpj          = if (!is.na(col_doc)) normalizar_cpf_cnpj(df[[col_doc]]) else NA_character_,
    stringsAsFactors = FALSE
  )
  
  out$owner_id          <- trimws(out$owner_id)
  out$nome_proprietario <- trimws(out$nome_proprietario)
  out
}

padronizar_fact_receita <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(
      data = as.Date(character(0)),
      imovel_id = character(0),
      receita = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  col_data   <- coluna_mais_proxima(df, c("data", "dt", "check_in", "competencia"))
  col_imovel <- coluna_mais_proxima(df, c("imovel_id", "id_imovel", "imovel"))
  col_valor  <- coluna_mais_proxima(df, c("receita", "valor_receita", "valor", "total"))
  
  data.frame(
    data      = parse_date_safe(df[[col_data %||% 1]]),
    imovel_id = as.character(df[[col_imovel %||% 1]]),
    receita   = suppressWarnings(as.numeric(df[[col_valor %||% 1]])),
    stringsAsFactors = FALSE
  )
}

padronizar_fact_manutencao <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(
      data = as.Date(character(0)),
      imovel_id = character(0),
      descricao = character(0),
      valor = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  col_data   <- coluna_mais_proxima(df, c("data", "dt", "data_servico"))
  col_imovel <- coluna_mais_proxima(df, c("imovel_id", "id_imovel", "imovel"))
  col_desc   <- coluna_mais_proxima(df, c("descricao", "servico", "ordem_servico", "os"))
  col_valor  <- coluna_mais_proxima(df, c("valor", "custo", "valor_total"))
  
  data.frame(
    data      = parse_date_safe(df[[col_data %||% 1]]),
    imovel_id = as.character(df[[col_imovel %||% 1]]),
    descricao = if (!is.na(col_desc)) as.character(df[[col_desc]]) else NA_character_,
    valor     = suppressWarnings(as.numeric(df[[col_valor %||% 1]])),
    stringsAsFactors = FALSE
  )
}

padronizar_fact_despesas <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(
      data = as.Date(character(0)),
      imovel_id = character(0),
      categoria = character(0),
      valor = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  col_data   <- coluna_mais_proxima(df, c("data", "dt", "competencia"))
  col_imovel <- coluna_mais_proxima(df, c("imovel_id", "id_imovel", "imovel"))
  col_cat    <- coluna_mais_proxima(df, c("categoria", "tipo", "despesa"))
  col_valor  <- coluna_mais_proxima(df, c("valor", "valor_despesa", "total"))
  
  data.frame(
    data      = parse_date_safe(df[[col_data %||% 1]]),
    imovel_id = as.character(df[[col_imovel %||% 1]]),
    categoria = if (!is.na(col_cat)) as.character(df[[col_cat]]) else NA_character_,
    valor     = suppressWarnings(as.numeric(df[[col_valor %||% 1]])),
    stringsAsFactors = FALSE
  )
}

# ── ETL Excel -> SQLite ───────────────────────────────────────
processar_xlsx_para_sqlite <- function(path_xlsx, sqlite_path = SQLITE_PATH) {
  sheets <- listar_abas_excel(path_xlsx)
  
  aba_dim_imovel <- achar_aba(sheets, c("dim_imovel", "imoveis", "imovel"))
  aba_dim_prop   <- achar_aba(sheets, c("dim_proprietario", "proprietarios", "proprietario"))
  aba_receita    <- achar_aba(sheets, c("fact_receita", "receita", "receitas"))
  aba_manut      <- achar_aba(sheets, c("fact_manutencao", "manutencao", "ordens_servico"))
  aba_desp       <- achar_aba(sheets, c("fact_despesas", "despesas", "custos"))
  
  df_dim_imovel <- if (!is.na(aba_dim_imovel)) ler_aba_segura(path_xlsx, aba_dim_imovel) else NULL
  df_dim_prop   <- if (!is.na(aba_dim_prop))   ler_aba_segura(path_xlsx, aba_dim_prop) else NULL
  df_receita    <- if (!is.na(aba_receita))    ler_aba_segura(path_xlsx, aba_receita) else NULL
  df_manut      <- if (!is.na(aba_manut))      ler_aba_segura(path_xlsx, aba_manut) else NULL
  df_desp       <- if (!is.na(aba_desp))       ler_aba_segura(path_xlsx, aba_desp) else NULL
  
  dim_imovel       <- padronizar_dim_imovel(df_dim_imovel)
  dim_proprietario <- padronizar_dim_proprietario(df_dim_prop)
  fact_receita     <- padronizar_fact_receita(df_receita)
  fact_manutencao  <- padronizar_fact_manutencao(df_manut)
  fact_despesas    <- padronizar_fact_despesas(df_desp)
  
  con <- sqlite_connect(sqlite_path)
  on.exit(if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con), add = TRUE)
  
  sqlite_write_table(dim_imovel, "dim_imovel", con)
  sqlite_write_table(dim_proprietario, "dim_proprietario", con)
  sqlite_write_table(fact_receita, "fact_receita", con)
  sqlite_write_table(fact_manutencao, "fact_manutencao", con)
  sqlite_write_table(fact_despesas, "fact_despesas", con)
  
  sqlite_set_meta(CACHE_META_KEY, as.character(Sys.time()), con)
  
  list(
    ok = TRUE,
    sqlite_path = sqlite_path,
    sheets = sheets,
    n_dim_imovel = nrow(dim_imovel),
    n_dim_proprietario = nrow(dim_proprietario),
    n_fact_receita = nrow(fact_receita),
    n_fact_manutencao = nrow(fact_manutencao),
    n_fact_despesas = nrow(fact_despesas)
  )
}

# ── Pipeline principal ────────────────────────────────────────
carregar_dados_app <- function(
    file_id = DRIVE_FILE_ID,
    folder_id = DRIVE_FOLDER_ID,
    forcar_dl = FALSE,
    forcar_etl = FALSE
) {
  dl <- baixar_db_master_publico(
    file_id = file_id,
    destino = CACHE_XLSX,
    forcar = forcar_dl
  )
  
  if (!isTRUE(dl$ok)) {
    stop(dl$msg %||% "Falha ao obter DB_MASTER.")
  }
  
  precisa_etl <- forcar_etl || !file.exists(SQLITE_PATH)
  
  if (!precisa_etl) {
    xlsx_mtime <- if (file.exists(dl$path)) file.mtime(dl$path) else as.POSIXct(NA)
    sql_mtime  <- if (file.exists(SQLITE_PATH)) file.mtime(SQLITE_PATH) else as.POSIXct(NA)
    precisa_etl <- is.na(sql_mtime) || is.na(xlsx_mtime) || xlsx_mtime > sql_mtime
  }
  
  etl <- NULL
  if (isTRUE(precisa_etl)) {
    etl <- processar_xlsx_para_sqlite(dl$path, SQLITE_PATH)
  }
  
  list(
    ok = TRUE,
    download = dl,
    etl = etl,
    sqlite_path = SQLITE_PATH
  )
}

# ── Auth helpers ──────────────────────────────────────────────
auth_hash <- function(senha, salt) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Pacote 'digest' ausente no ambiente do container.")
  }
  digest::digest(paste0(trimws(senha %||% ""), "::", trimws(salt %||% "")), algo = "sha256")
}

auth_init_table <- function(con = NULL) {
  own <- is.null(con)
  if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  
  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS auth_owners (
      cpf_cnpj TEXT PRIMARY KEY,
      senha_hash TEXT NOT NULL,
      updated_at TEXT
    )"
  )
  
  invisible(TRUE)
}

auth_set_password <- function(cpf_cnpj, senha, con = NULL) {
  own <- is.null(con)
  if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  
  auth_init_table(con)
  
  cpf_norm <- normalizar_cpf_cnpj(cpf_cnpj)
  if (is.na(cpf_norm) || !nzchar(cpf_norm)) stop("CPF/CNPJ inválido para cadastro de senha.")
  if (is.null(senha) || nchar(trimws(senha)) < 6) stop("Senha deve ter no mínimo 6 caracteres.")
  
  hash <- auth_hash(senha, cpf_norm)
  
  DBI::dbExecute(
    con,
    "INSERT OR REPLACE INTO auth_owners (cpf_cnpj, senha_hash, updated_at) VALUES (?, ?, ?)",
    params = list(cpf_norm, hash, as.character(Sys.time()))
  )
  
  invisible(TRUE)
}

auth_has_password <- function(cpf_cnpj, con = NULL) {
  own <- is.null(con)
  if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  
  auth_init_table(con)
  
  cpf_norm <- normalizar_cpf_cnpj(cpf_cnpj)
  if (is.na(cpf_norm) || !nzchar(cpf_norm)) return(FALSE)
  
  res <- DBI::dbGetQuery(
    con,
    "SELECT cpf_cnpj FROM auth_owners WHERE cpf_cnpj = ? LIMIT 1",
    params = list(cpf_norm)
  )
  
  nrow(res) > 0
}

auth_check_password <- function(cpf_cnpj, senha, con = NULL) {
  own <- is.null(con)
  if (own) con <- sqlite_connect()
  on.exit(if (own) DBI::dbDisconnect(con), add = TRUE)
  
  auth_init_table(con)
  
  cpf_norm <- normalizar_cpf_cnpj(cpf_cnpj)
  if (is.na(cpf_norm) || !nzchar(cpf_norm)) return(FALSE)
  
  res <- tryCatch(
    DBI::dbGetQuery(
      con,
      "SELECT senha_hash FROM auth_owners WHERE cpf_cnpj = ? LIMIT 1",
      params = list(cpf_norm)
    ),
    error = function(e) data.frame(senha_hash = character(0))
  )
  if (nrow(res) == 0) return(FALSE)
  identical(res$senha_hash[1], auth_hash(senha, cpf_norm))
}