# run.R — execução explícita para Docker/Render
options(
  shiny.host = "0.0.0.0",
  shiny.port = as.integer(Sys.getenv("PORT", "3838"))
)

app <- source("app.R", local = new.env())$value
print(app)
