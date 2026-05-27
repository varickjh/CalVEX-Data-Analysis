# deploy.R — publish the CalVEX dashboard to shinyapps.io
#
# ONE-TIME SETUP (run once per machine, then you're done):
#   1. Install rsconnect if you don't have it:
#        install.packages("rsconnect")
#   2. Connect your shinyapps.io account — get your token & secret from
#      https://www.shinyapps.io/ → Account → Tokens → Show Secret, then run:
#        rsconnect::setAccountInfo(
#          name   = "<YOUR_SHINYAPPS_USERNAME>",
#          token  = "<TOKEN>",
#          secret = "<SECRET>"
#        )
#
# SUPABASE SETUP (do this before the first deploy):
#   1. Create a Supabase project at https://supabase.com.
#   2. Table Editor → Import data from CSV → upload data/calvex_data.csv.
#      Name the table  calvex_data  in the public schema.
#   3. Go to Settings → API and copy:
#        - Project URL  →  SUPABASE_URL
#        - service_role secret key  →  SUPABASE_KEY
#      Paste them below. NEVER commit these values to git.
#
# DEPLOY:
#   Source this file or run it line-by-line in RStudio.

library(rsconnect)

# ── Fill these in ────────────────────────────────────────────────────────────

SHINYAPPS_ACCOUNT <- "<YOUR_SHINYAPPS_USERNAME>"   # your shinyapps.io username
APP_NAME          <- "calvex-dashboard"             # URL slug on shinyapps.io

# Supabase → Settings → API → Project URL (no trailing slash)
SUPABASE_URL <- "https://<project-ref>.supabase.co"

# Supabase → Settings → API → service_role secret key
SUPABASE_KEY <- "<YOUR_SERVICE_ROLE_KEY>"

# ─────────────────────────────────────────────────────────────────────────────

stopifnot(
  "Fill in SHINYAPPS_ACCOUNT" = SHINYAPPS_ACCOUNT != "<YOUR_SHINYAPPS_USERNAME>",
  "Fill in SUPABASE_URL"      = SUPABASE_URL       != "https://<project-ref>.supabase.co",
  "Fill in SUPABASE_KEY"      = SUPABASE_KEY       != "<YOUR_SERVICE_ROLE_KEY>"
)

# Push env vars to shinyapps.io (stored encrypted; NOT baked into the bundle).
rsconnect::setEnvironmentVars(
  appName      = APP_NAME,
  account      = SHINYAPPS_ACCOUNT,
  SUPABASE_URL = SUPABASE_URL,
  SUPABASE_KEY = SUPABASE_KEY
)

# Deploy — data/ is excluded; the app fetches from the Supabase Data API.
rsconnect::deployApp(
  appDir      = ".",
  appName     = APP_NAME,
  account     = SHINYAPPS_ACCOUNT,
  appFiles    = c("app.R", "server.R", "ui.R"),
  forceUpdate = TRUE
)
