# Deploy do app Shiny no Render via Docker

## Estrutura
- `app.R`: seletor principal do modo do app (`public` ou `master`)
- `app_public.R`: painel do proprietário
- `app_master.R`: painel mestre interno
- `R/gdrive_public.R`: ingestão do Google Drive + cache SQLite
- `Dockerfile`: build da imagem
- `render.yaml`: blueprint opcional do Render

## Variáveis de ambiente
- `APP_MODE=public` ou `master`
- `DRIVE_FILE_ID`: ID da planilha Google Sheets publicada/compartilhada
- `DRIVE_FOLDER_ID`: opcional, mantido por compatibilidade
- `MAX_CACHE_AGE_H`: janela de validade do cache local

## Deploy manual no Render
1. Suba esta pasta para um repositório GitHub.
2. No Render, clique em **New +** > **Web Service**.
3. Conecte o repositório.
4. O Render deve detectar o `Dockerfile` automaticamente.
5. No painel do serviço, configure:
   - `APP_MODE=public` para o app do proprietário
   - ou `APP_MODE=master` para o painel interno
6. Faça o deploy.

## Observações
- O plano free do Render entra em sleep após inatividade.
- O cache SQLite/XLSX é efêmero: em restart/redeploy ele pode ser recriado.
- O app recompõe o cache baixando novamente a planilha do Google Drive.
