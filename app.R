# ============================================================
# app.R — Launcher BSBStay com autenticação por senha
#
# Fluxos:
#   1. Proprietário sem senha → tela de cadastro de senha
#   2. Proprietário com senha → login normal (CPF/CNPJ + senha)
#   3. Admin → login com credenciais de variável de ambiente
#   4. Proprietário logado → opção de alterar senha
# ============================================================

APP_ROOT <- normalizePath(Sys.getenv("APP_ROOT", "."), winslash = "/", mustWork = FALSE)

suppressPackageStartupMessages({ library(shiny) })

addResourcePath("assets", APP_ROOT)

source(file.path(APP_ROOT, "R", "gdrive_public.R"), local = FALSE)

# ── Helpers ──────────────────────────────────────────────────
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}
trim_na <- function(x) {
  x <- trimws(as.character(x %||% ""))
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  x
}
normalizar_doc <- function(x) {
  x <- trim_na(x)
  if (all(is.na(x))) return(NA_character_)
  x <- gsub("[^0-9]", "", x)
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  x
}
is_valid_cpf_mask  <- function(x) grepl("^\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2}$",    trimws(x %||% ""))
is_valid_cnpj_mask <- function(x) grepl("^\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}$", trimws(x %||% ""))
is_valid_doc <- function(x) {
  n <- nchar(normalizar_doc(x) %||% "")
  n %in% c(11, 14)
}
doc_tipo <- function(x) {
  n <- nchar(normalizar_doc(x) %||% "")
  if (n == 11) return("CPF")
  if (n == 14) return("CNPJ")
  NA_character_
}

ADMIN_USER <- Sys.getenv("BSBSTAY_ADMIN_USER", "admin")
ADMIN_PASS <- Sys.getenv("BSBSTAY_ADMIN_PASS", "bsbstay123")

# ── Bootstrap de dados + registro de usuários ──────────────────
boot_data_status <- tryCatch({
  carregar_dados_app(
    file_id = DRIVE_FILE_ID,
    folder_id = DRIVE_FOLDER_ID,
    forcar_dl = FALSE,
    forcar_etl = FALSE
  )
  list(ok = TRUE, msg = NULL)
}, error = function(e) {
  message("[App] Falha no bootstrap inicial de dados: ", e$message)
  list(ok = FALSE, msg = e$message)
})

get_auth_registry <- function() {
  tryCatch({
    con <- sqlite_connect()
    on.exit(if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con), add = TRUE)
    
    dim_prop <- sqlite_read_table("dim_proprietario", con)
    if (is.null(dim_prop) || nrow(dim_prop) == 0) {
      carregar_dados_app(
        file_id = DRIVE_FILE_ID,
        folder_id = DRIVE_FOLDER_ID,
        forcar_dl = isFALSE(boot_data_status$ok),
        forcar_etl = isFALSE(boot_data_status$ok)
      )
      DBI::dbDisconnect(con)
      con <- sqlite_connect()
      dim_prop <- sqlite_read_table("dim_proprietario", con)
    }
    
    if (is.null(dim_prop) || nrow(dim_prop) == 0) {
      data.frame(owner_id=character(), cpf_cnpj=character(),
                 nome_proprietario=character(), tipo_documento=character(),
                 stringsAsFactors=FALSE)
    } else {
      dim_prop |>
        dplyr::transmute(
          owner_id          = as.character(owner_id),
          cpf_cnpj          = normalizar_doc(cpf_cnpj),
          nome_proprietario = trim_na(nome_proprietario),
          tipo_documento    = vapply(cpf_cnpj, doc_tipo, character(1))
        ) |>
        dplyr::filter(!is.na(cpf_cnpj), nzchar(cpf_cnpj)) |>
        dplyr::distinct(cpf_cnpj, .keep_all = TRUE)
    }
  }, error = function(e) {
    message("[App] Erro ao carregar auth_registry: ", e$message)
    data.frame(owner_id=character(), cpf_cnpj=character(),
               nome_proprietario=character(), tipo_documento=character(),
               stringsAsFactors=FALSE)
  })
}

# ── CSS compartilhado ─────────────────────────────────────────
auth_css <- "
  *{box-sizing:border-box;}
  body{margin:0;font-family:'Inter',sans-serif;background:#08111f;}
  .auth-shell{min-height:100vh;display:grid;grid-template-columns:400px 1fr;}
  .auth-side{background:linear-gradient(180deg,#07101d 0%,#0f1c2e 100%);padding:40px 32px;
    border-right:1px solid rgba(255,255,255,.06);display:flex;flex-direction:column;justify-content:space-between;}
  .auth-brand-top{display:flex;flex-direction:column;gap:18px;}
  .auth-logo-wrap{width:200px;max-width:100%;}
  .auth-logo{width:100%;height:auto;display:block;border-radius:12px;box-shadow:0 8px 28px rgba(0,0,0,.25);}
  .auth-badge{display:inline-flex;align-items:center;gap:8px;background:rgba(0,196,154,.12);
    color:#8df0d1;border:1px solid rgba(0,196,154,.28);padding:7px 12px;border-radius:999px;
    font-size:12px;font-weight:700;width:max-content;}
  .auth-title{color:#fff;font-size:26px;line-height:1.1;font-weight:800;}
  .auth-sub{color:#8ea7c2;font-size:13px;line-height:1.6;max-width:300px;}
  .auth-foot{color:#6f87a3;font-size:11px;}
  .auth-main{display:flex;align-items:center;justify-content:center;padding:32px;
    background:radial-gradient(circle at top right,rgba(26,110,247,.18),transparent 28%),
               radial-gradient(circle at bottom left,rgba(0,196,154,.12),transparent 28%),#0b1322;}
  .auth-card{width:100%;max-width:420px;background:#fff;border-radius:18px;padding:28px 26px;
    box-shadow:0 18px 56px rgba(0,0,0,.28);border:1px solid #e5e9ef;}
  .auth-card h2{margin:0 0 6px 0;color:#0f1c2e;font-size:22px;font-weight:800;}
  .auth-card p{margin:0 0 16px 0;color:#6b7280;font-size:13px;line-height:1.5;}
  .auth-form .form-group{margin-bottom:12px !important;}
  .auth-form label{font-size:12px !important;font-weight:700 !important;color:#475569 !important;margin-bottom:5px;}
  .auth-form input{border:2px solid #e2e8f0 !important;border-radius:10px !important;
    height:44px !important;font-size:14px !important;padding:10px 12px !important;}
  .auth-form input:focus{border-color:#1a6ef7 !important;box-shadow:0 0 0 4px rgba(26,110,247,.10) !important;outline:none !important;}
  .auth-hint{margin-bottom:12px;color:#64748b;font-size:12px;line-height:1.5;
    background:#f8fafc;border:1px solid #e2e8f0;border-radius:10px;padding:10px 12px;}
  .auth-hint b{color:#374151;}
  .auth-btn{width:100%;height:44px;border:none;border-radius:10px;
    background:linear-gradient(90deg,#0f1c2e 0%,#1a3350 100%);color:#fff;
    font-size:14px;font-weight:800;cursor:pointer;transition:opacity .15s;}
  .auth-btn:hover{opacity:.88;}
  .auth-btn-sec{width:100%;height:40px;border:2px solid #e2e8f0;border-radius:10px;
    background:#fff;color:#374151;font-size:13px;font-weight:700;cursor:pointer;
    margin-top:10px;transition:all .15s;}
  .auth-btn-sec:hover{background:#f8fafc;border-color:#cbd5e1;}
  .auth-err{margin-top:12px;background:#fff1f0;color:#991b1b;border:1px solid #fecaca;
    border-radius:10px;padding:10px 12px;font-size:13px;font-weight:600;}
  .auth-ok{margin-top:12px;background:#f0fdf4;color:#166534;border:1px solid #86efac;
    border-radius:10px;padding:10px 12px;font-size:13px;font-weight:600;}
  .auth-req{font-size:11px;color:#9ca3af;margin-top:4px;}
  .pw-rules{font-size:11px;color:#6b7280;background:#f8fafc;border:1px solid #e2e8f0;
    border-radius:8px;padding:8px 12px;margin-bottom:10px;line-height:1.8;}
  .pw-rules li{list-style:none;padding-left:4px;}
  .pw-ok{color:#16a34a;font-weight:700;} .pw-fail{color:#dc2626;}
  .divider{display:flex;align-items:center;gap:10px;margin:14px 0;color:#9ca3af;font-size:12px;}
  .divider::before,.divider::after{content:'';flex:1;border-top:1px solid #e5e9ef;}
  @media(max-width:860px){.auth-shell{grid-template-columns:1fr;}.auth-side{display:none;}}
"

# ── Função auxiliar: painel lateral ──────────────────────────
auth_side <- function(subtitulo = NULL) {
  div(class = "auth-side",
      div(class = "auth-brand-top",
          div(class = "auth-logo-wrap",
              tags$img(src = "assets/marca_BSB_STAY_RS_10.jpg",
                       class = "auth-logo", alt = "BSB Stay")),
          div(class = "auth-badge", "🔐 Acesso seguro"),
          div(class = "auth-title", "BSB Stay"),
          div(class = "auth-sub",
              subtitulo %||% HTML(paste0(
                "Acesse seu painel de resultados com seu ",
                "<b>CPF ou CNPJ</b> e a senha que você cadastrou.<br><br>",
                "Primeiro acesso? Cadastre sua senha usando seu documento."
              )))
      ),
      div(class = "auth-foot", "Dashboard protegido por senha pessoal."))
}

# ════════════════════════════════════════════════════════════
# UIs das telas
# ════════════════════════════════════════════════════════════

# ── Tela 1: Login ────────────────────────────────────────────
tela_login <- fluidPage(
  tags$head(
    tags$title("BSB Stay | Entrar"),
    tags$link(rel="stylesheet",
              href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap"),
    tags$style(HTML(auth_css))
  ),
  div(class = "auth-shell",
      auth_side(),
      div(class = "auth-main",
          div(class = "auth-card auth-form",
              h2("Entrar"),
              p("Informe seu documento e senha para acessar."),
              textInput("login_doc",  "CPF / CNPJ",
                        placeholder = "000.000.000-00  ou  00.000.000/0000-00"),
              passwordInput("login_pass", "Senha"),
              actionButton("btn_login", "Acessar", class = "auth-btn"),
              div(class = "divider", "ou"),
              tags$button(class = "auth-btn-sec",
                          onclick = "Shiny.setInputValue('nav_cadastro', Math.random())",
                          "Primeiro acesso / Esqueci minha senha"),
              uiOutput("login_msg"))))
)

# ── Tela 2: Cadastro / Redefinição de senha ──────────────────
tela_cadastro <- fluidPage(
  tags$head(
    tags$title("BSB Stay | Criar senha"),
    tags$link(rel="stylesheet",
              href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap"),
    tags$style(HTML(auth_css)),
    tags$script(HTML("
      function checkPw(id) {
        var v = document.getElementById(id)?.value || '';
        var okLen = v.length >= 6;
        var okNum = /[0-9]/.test(v);
        var okLet = /[A-Za-zÀ-ÿ]/.test(v);

        var a = document.getElementById('pw_len');
        var b = document.getElementById('pw_num');
        var c = document.getElementById('pw_let');

        if (a) { a.className = okLen ? 'pw-ok' : 'pw-fail'; a.innerHTML = (okLen ? '✓' : '✗') + '  Mínimo 6 caracteres'; }
        if (b) { b.className = okNum ? 'pw-ok' : 'pw-fail'; b.innerHTML = (okNum ? '✓' : '✗') + '  Pelo menos 1 número'; }
        if (c) { c.className = okLet ? 'pw-ok' : 'pw-fail'; c.innerHTML = (okLet ? '✓' : '✗') + '  Pelo menos 1 letra'; }
      }
      document.addEventListener('input', function(e){
        if(e.target && e.target.id === 'cad_senha1'){ checkPw('cad_senha1'); }
        if(e.target && e.target.id === 'alt_senha1'){ checkPw('alt_senha1'); }
      });
    "))
  ),
  div(class = "auth-shell",
      auth_side("Seu documento precisa já estar cadastrado na base da BSB Stay. Depois disso, você cria sua senha de acesso."),
      div(class = "auth-main",
          div(class = "auth-card auth-form",
              h2("Criar ou redefinir senha"),
              p("Informe seu documento cadastrado e escolha uma senha pessoal."),
              textInput("cad_doc", "CPF / CNPJ",
                        placeholder = "000.000.000-00  ou  00.000.000/0000-00"),
              div(class = "auth-hint",
                  HTML("<b>Como funciona?</b> Verificamos que seu documento está cadastrado no sistema e você escolhe sua senha. Somente você saberá.")),
              passwordInput("cad_senha1", "Nova Senha",
                            placeholder = "Mínimo 6 caracteres"),
              tags$ul(class = "pw-rules",
                      tags$li(id = "pw_len", class = "pw-fail", "✗  Mínimo 6 caracteres"),
                      tags$li(id = "pw_num", class = "pw-fail", "✗  Pelo menos 1 número"),
                      tags$li(id = "pw_let", class = "pw-fail", "✗  Pelo menos 1 letra")),
              passwordInput("cad_senha2", "Confirmar Senha"),
              actionButton("btn_cadastrar", "Salvar Senha", class = "auth-btn"),
              div(class = "divider", "ou"),
              tags$button(class = "auth-btn-sec",
                          onclick = "Shiny.setInputValue('nav_login', Math.random())",
                          "← Voltar para o login"),
              uiOutput("cadastro_msg"))))
)

# ── Tela 3: Alterar senha (pós-login) ────────────────────────
tela_alterar_senha <- function(nome_prop = "") {
  fluidPage(
    tags$head(
      tags$title("BSB Stay | Alterar Senha"),
      tags$link(rel="stylesheet",
                href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap"),
      tags$style(HTML(auth_css)),
      tags$script(HTML("
        function checkPw2(id) {
          var v = document.getElementById(id)?.value || '';
          var okLen = v.length >= 6;
          var okNum = /[0-9]/.test(v);
          var okLet = /[A-Za-zÀ-ÿ]/.test(v);

          var a = document.getElementById('pw2_len');
          var b = document.getElementById('pw2_num');
          var c = document.getElementById('pw2_let');

          if (a) { a.className = okLen ? 'pw-ok' : 'pw-fail'; a.innerHTML = (okLen ? '✓' : '✗') + '  Mínimo 6 caracteres'; }
          if (b) { b.className = okNum ? 'pw-ok' : 'pw-fail'; b.innerHTML = (okNum ? '✓' : '✗') + '  Pelo menos 1 número'; }
          if (c) { c.className = okLet ? 'pw-ok' : 'pw-fail'; c.innerHTML = (okLet ? '✓' : '✗') + '  Pelo menos 1 letra'; }
        }
        document.addEventListener('input', function(e){
          if(e.target && e.target.id === 'alt_senha1'){ checkPw2('alt_senha1'); }
        });
      "))
    ),
    div(class = "auth-shell",
        auth_side(HTML(paste0(
          "Você está logado como <b>", htmltools::htmlEscape(nome_prop %||% ""), "</b>.<br><br>",
          "Para sua segurança, você pode atualizar sua senha a qualquer momento."
        ))),
        div(class = "auth-main",
            div(class = "auth-card auth-form",
                h2("Alterar senha"),
                p("Defina uma nova senha para o seu acesso."),
                passwordInput("alt_senha1", "Nova Senha",
                              placeholder = "Mínimo 6 caracteres"),
                tags$ul(class = "pw-rules",
                        tags$li(id = "pw2_len", class = "pw-fail", "✗  Mínimo 6 caracteres"),
                        tags$li(id = "pw2_num", class = "pw-fail", "✗  Pelo menos 1 número"),
                        tags$li(id = "pw2_let", class = "pw-fail", "✗  Pelo menos 1 letra")),
                passwordInput("alt_senha2", "Confirmar Nova Senha"),
                actionButton("btn_alterar_senha", "Salvar nova senha", class = "auth-btn"),
                div(class = "divider", "ou"),
                tags$button(class = "auth-btn-sec",
                            onclick = "Shiny.setInputValue('nav_app', Math.random())",
                            "← Voltar ao painel"),
                uiOutput("alterar_msg"))))
  )
}

# ── App principal ─────────────────────────────────────────────
ui_app_principal <- function() {
  if (file.exists(file.path(APP_ROOT, "app_public.R"))) {
    source(file.path(APP_ROOT, "app_public.R"), local = TRUE)
    if (exists("app_ui", inherits = FALSE)) return(app_ui)
  }
  fluidPage(
    tags$head(tags$title("BSB Stay | Painel")),
    h2("Painel carregado"),
    p("O front-end principal deve ser definido em app_public.R como objeto `app_ui`."),
    p("Se desejar, eu também ajusto essa camada.")
  )
}

ui <- uiOutput("root_ui")

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    tela = "login",
    owner_id = NULL,
    owner_name = NULL,
    owner_doc = NULL,
    is_admin = FALSE,
    auth_error = NULL
  )
  
  # ── Navegação entre telas ──────────────────────────────────
  observeEvent(input$nav_cadastro, {
    rv$tela <- "cadastro"
  })
  
  observeEvent(input$nav_login, {
    rv$tela <- "login"
  })
  
  observeEvent(input$nav_app, {
    rv$tela <- "app"
  })
  
  output$root_ui <- renderUI({
    if (rv$tela == "login") return(tela_login)
    if (rv$tela == "cadastro") return(tela_cadastro)
    if (rv$tela == "alterar") return(tela_alterar_senha(rv$owner_name))
    ui_app_principal()
  })
  
  # ── Mensagens bootstrap ────────────────────────────────────
  output$login_msg <- renderUI({
    msgs <- list()
    
    if (!isTRUE(boot_data_status$ok)) {
      msgs <- c(msgs, list(div(class = "auth-err",
                               paste0("Atenção: houve falha na carga inicial dos dados. ",
                                      "O sistema tentará recarregar automaticamente. Detalhe: ",
                                      boot_data_status$msg %||% "erro desconhecido"))))
    }
    
    if (!is.null(rv$auth_error) && nzchar(rv$auth_error)) {
      msgs <- c(msgs, list(div(class = "auth-err", rv$auth_error)))
    }
    
    if (length(msgs) == 0) return(NULL)
    tagList(msgs)
  })
  
  output$cadastro_msg <- renderUI(NULL)
  output$alterar_msg  <- renderUI(NULL)
  
  # ── Login ──────────────────────────────────────────────────
  observeEvent(input$btn_login, {
    rv$auth_error <- NULL
    
    doc_raw  <- trim_na(input$login_doc)
    senha_in <- trim_na(input$login_pass)
    
    if (is.na(doc_raw) || !nzchar(doc_raw) || is.na(senha_in) || !nzchar(senha_in)) {
      rv$auth_error <- "Preencha CPF/CNPJ e senha."
      return()
    }
    
    doc_norm <- normalizar_doc(doc_raw)
    if (is.na(doc_norm) || !is_valid_doc(doc_norm)) {
      rv$auth_error <- "Informe um CPF ou CNPJ válido."
      return()
    }
    
    # Admin
    if (identical(trimws(doc_raw), ADMIN_USER) && identical(senha_in, ADMIN_PASS)) {
      rv$is_admin   <- TRUE
      rv$owner_id   <- NA_character_
      rv$owner_name <- "Administrador"
      rv$owner_doc  <- NA_character_
      rv$tela       <- "app"
      return()
    }
    
    auth_registry <- get_auth_registry()
    
    prop <- auth_registry[auth_registry$cpf_cnpj == doc_norm, , drop = FALSE]
    if (nrow(prop) == 0) {
      rv$auth_error <- "Documento não encontrado na base. Verifique o CPF/CNPJ cadastrado."
      return()
    }
    
    owner_id <- prop$owner_id[1]
    nome     <- prop$nome_proprietario[1] %||% "Proprietário"
    
    senha_ok <- tryCatch({
      auth_check_password(doc_norm, senha_in)
    }, error = function(e) {
      rv$auth_error <- paste0("Erro ao validar a senha: ", e$message)
      FALSE
    })
    
    if (!isTRUE(senha_ok)) {
      rv$auth_error <- "Senha inválida. Se for seu primeiro acesso, clique em “Primeiro acesso / Esqueci minha senha”."
      return()
    }
    
    rv$is_admin   <- FALSE
    rv$owner_id   <- owner_id
    rv$owner_name <- nome
    rv$owner_doc  <- doc_norm
    rv$tela       <- "app"
  })
  
  # ── Cadastro / redefinição de senha ────────────────────────
  observeEvent(input$btn_cadastrar, {
    doc_raw <- trim_na(input$cad_doc)
    s1      <- trim_na(input$cad_senha1)
    s2      <- trim_na(input$cad_senha2)
    
    output$cadastro_msg <- renderUI(NULL)
    
    if (is.na(doc_raw) || !nzchar(doc_raw)) {
      output$cadastro_msg <- renderUI(div(class = "auth-err", "Informe seu CPF/CNPJ."))
      return()
    }
    
    doc_norm <- normalizar_doc(doc_raw)
    if (is.na(doc_norm) || !is_valid_doc(doc_norm)) {
      output$cadastro_msg <- renderUI(div(class = "auth-err", "Informe um CPF ou CNPJ válido."))
      return()
    }
    
    if (is.na(s1) || nchar(s1) < 6 || !grepl("[0-9]", s1) || !grepl("[A-Za-zÀ-ÿ]", s1)) {
      output$cadastro_msg <- renderUI(div(class = "auth-err",
                                          "Sua senha precisa ter no mínimo 6 caracteres, incluindo pelo menos 1 letra e 1 número."))
      return()
    }
    
    if (!identical(s1, s2)) {
      output$cadastro_msg <- renderUI(div(class = "auth-err", "As senhas não coincidem."))
      return()
    }
    
    auth_registry <- get_auth_registry()
    prop <- auth_registry[auth_registry$cpf_cnpj == doc_norm, , drop = FALSE]
    
    if (nrow(prop) == 0) {
      output$cadastro_msg <- renderUI(div(class = "auth-err",
                                          "Documento não encontrado na base da BSB Stay."))
      return()
    }
    
    ok <- tryCatch({
      auth_set_password(doc_norm, s1)
      TRUE
    }, error = function(e) {
      output$cadastro_msg <- renderUI(div(class = "auth-err",
                                          paste0("Erro ao salvar a senha: ", e$message)))
      FALSE
    })
    
    if (!isTRUE(ok)) return()
    
    output$cadastro_msg <- renderUI(div(class = "auth-ok",
                                        "Senha salva com sucesso. Agora você já pode entrar no sistema."))
  })
  
  # ── Alterar senha ──────────────────────────────────────────
  observeEvent(input$btn_alterar_senha, {
    s1 <- trim_na(input$alt_senha1)
    s2 <- trim_na(input$alt_senha2)
    
    output$alterar_msg <- renderUI(NULL)
    
    if (isTRUE(rv$is_admin)) {
      output$alterar_msg <- renderUI(div(class = "auth-err",
                                         "O usuário administrador não utiliza este fluxo de alteração."))
      return()
    }
    
    if (is.null(rv$owner_doc) || !nzchar(rv$owner_doc)) {
      output$alterar_msg <- renderUI(div(class = "auth-err",
                                         "Não foi possível identificar o documento do usuário logado."))
      return()
    }
    
    if (is.na(s1) || nchar(s1) < 6 || !grepl("[0-9]", s1) || !grepl("[A-Za-zÀ-ÿ]", s1)) {
      output$alterar_msg <- renderUI(div(class = "auth-err",
                                         "Sua nova senha precisa ter no mínimo 6 caracteres, incluindo pelo menos 1 letra e 1 número."))
      return()
    }
    
    if (!identical(s1, s2)) {
      output$alterar_msg <- renderUI(div(class = "auth-err", "As senhas não coincidem."))
      return()
    }
    
    ok <- tryCatch({
      auth_set_password(rv$owner_doc, s1)
      TRUE
    }, error = function(e) {
      output$alterar_msg <- renderUI(div(class = "auth-err",
                                         paste0("Erro ao alterar a senha: ", e$message)))
      FALSE
    })
    
    if (!isTRUE(ok)) return()
    
    output$alterar_msg <- renderUI(div(class = "auth-ok",
                                       "Senha alterada com sucesso."))
  })
  
  # ── Exposição de sessão ao app principal ───────────────────
  outputOptions(output, "root_ui", suspendWhenHidden = FALSE)
  
  observe({
    session$userData$auth <- list(
      is_authenticated = identical(rv$tela, "app"),
      is_admin         = isTRUE(rv$is_admin),
      owner_id         = rv$owner_id,
      owner_name       = rv$owner_name,
      owner_doc        = rv$owner_doc
    )
  })
  
  # ── Gancho opcional do app principal ───────────────────────
  observe({
    if (identical(rv$tela, "app")) {
      if (file.exists(file.path(APP_ROOT, "app_public.R"))) {
        env_app <- new.env(parent = globalenv())
        env_app$session_auth <- reactive({
          list(
            is_authenticated = TRUE,
            is_admin         = isTRUE(rv$is_admin),
            owner_id         = rv$owner_id,
            owner_name       = rv$owner_name,
            owner_doc        = rv$owner_doc
          )
        })
        
        try(source(file.path(APP_ROOT, "app_public.R"), local = env_app), silent = TRUE)
        
        if (exists("app_server", envir = env_app, inherits = FALSE)) {
          try(env_app$app_server(input, output, session), silent = TRUE)
        }
      }
    }
  })
  
  # ── Logout opcional ────────────────────────────────────────
  observeEvent(input$logout, {
    rv$is_admin   <- FALSE
    rv$owner_id   <- NULL
    rv$owner_name <- NULL
    rv$owner_doc  <- NULL
    rv$auth_error <- NULL
    rv$tela       <- "login"
  })
  
  # ── Compatibilidade com app principal ──────────────────────
  observe({
    session$userData$owner_id   <- rv$owner_id
    session$userData$owner_name <- rv$owner_name
    session$userData$is_admin   <- rv$is_admin
    session$userData$owner_doc  <- rv$owner_doc
  })
  
  # ── Atalho: trocar senha a partir do painel ────────────────
  observeEvent(input$go_change_password, {
    if (!isTRUE(rv$is_admin) && !is.null(rv$owner_doc)) {
      rv$tela <- "alterar"
    }
  })
}

app <- shinyApp(ui, server)