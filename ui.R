library(shiny)
library(bslib)
library(markdown) # for shinyapps.io deployment
library(ggiraph)

ui <- page_sidebar(
  fillable = TRUE,
  fillable_mobile = TRUE,
  # trigger a resize once Shiny finishes its first render so plot fills the container height
  tags$head(
    tags$script(HTML(
      "$(document).one('shiny:idle', function() {
        setTimeout(function() { $(window).trigger('resize'); }, 10);
      });"
    )),
    tags$style(HTML("
      :root {
        --calvex-purple-light: #E8D4F5;
        --calvex-purple-mid: #CEA9EA;
        --calvex-purple-hover: #B08FD4;
        --calvex-purple-active: #8E6FB0;
        --calvex-purple-border: rgba(107, 85, 142, 0.35);
      }
      .bslib-page-sidebar > .navbar,
      .bslib-page-sidebar > header,
      .bslib-page-sidebar .navbar {
        background-color: var(--calvex-purple-light) !important;
        border-bottom: 1px solid var(--calvex-purple-border);
      }
      .calvex-sidebar,
      .calvex-sidebar.accordion,
      .bslib-page-sidebar .calvex-sidebar {
        background-color: var(--calvex-purple-light) !important;
        border-right: 1px solid var(--calvex-purple-border);
      }
      .calvex-sidebar .accordion {
        --bs-accordion-border-radius: 0;
        --bs-accordion-inner-border-radius: 0;
        border-radius: 0.375rem;
        overflow: hidden;
        border: 1px solid var(--calvex-purple-border);
      }
      .calvex-sidebar .accordion-item {
        border: none;
        border-bottom: 1px solid var(--calvex-purple-border);
        border-radius: 0;
        overflow: hidden;
        margin-bottom: 0;
        background-color: var(--calvex-purple-light);
      }
      .calvex-sidebar .accordion-item:last-child {
        border-bottom: none;
      }
      .calvex-sidebar .accordion-button {
        background-color: var(--calvex-purple-light) !important;
        color: #000 !important;
        font-weight: 600;
        box-shadow: none !important;
      }
      .calvex-sidebar .accordion-button:hover {
        background-color: #DDD0EF !important;
        color: #000 !important;
      }
      .calvex-sidebar .accordion-button:not(.collapsed) {
        background-color: var(--calvex-purple-mid) !important;
        color: #000 !important;
      }
      .calvex-sidebar .accordion-button:not(.collapsed):hover {
        background-color: var(--calvex-purple-hover) !important;
        color: #000 !important;
      }
      .calvex-sidebar .accordion-item:has(.accordion-button:not(.collapsed)) {
        background-color: var(--calvex-purple-mid);
      }
      .calvex-sidebar .accordion-item:has(.accordion-button:not(.collapsed)) .accordion-body {
        background-color: var(--calvex-purple-light);
      }
      .calvex-sidebar .accordion-body {
        background-color: var(--calvex-purple-light);
        color: #000;
      }
      .calvex-app-title-link {
        display: inline-flex;
        align-items: center;
        gap: 0.65rem;
        text-decoration: none;
        color: #000;
        font-weight: 600;
        font-size: 1.1rem;
        cursor: pointer;
      }
      .calvex-app-title-link:hover {
        color: #000;
        text-decoration: none;
        opacity: 0.85;
      }
      .calvex-logo {
        height: 2.25rem;
        width: auto;
        display: block;
      }
      .bslib-page-sidebar > .main { display: flex; flex-direction: column; min-height: 0; }
      .calvex-plot-wrap {
        min-height: 280px;
        height: calc(100dvh - 80px);
        height: calc(100vh - 80px);
        display: flex;
        flex-direction: column;
        position: relative;
      }
      .calvex-plot-inner {
        position: relative;
        flex: 1 1 auto;
        min-height: 0;
        display: flex;
        flex-direction: column;
      }
      .calvex-plot-wrap .shiny-plot-output {
        flex: 1 1 auto;
        min-height: 0;
      }
      @media (max-width: 768px) {
        .calvex-plot-wrap {
          max-height: min(520px, calc(100dvh - 120px));
          max-height: min(520px, calc(100vh - 120px));
        }
      }
      @media (max-width: 576px) {
        .calvex-plot-wrap {
          max-height: min(420px, calc(100dvh - 140px));
          max-height: min(420px, calc(100vh - 140px));
        }
      }
      label.control-label { font-weight: 600; }
      .accordion-button { font-weight: 600; }
    "))
  ),

  # page title (logo + link resets app to defaults)
  title = tags$a(
    id = "calvex_home_reset",
    class = "calvex-app-title-link",
    href = "#",
    onclick = "Shiny.setInputValue('reset_to_defaults', Date.now()); return false;",
    tags$img(src = "images/logo.png", alt = "CalVEX logo", class = "calvex-logo"),
    tags$span(class = "calvex-app-title-text", "Online Data Visualization Tool")
  ),

  # sidebar Layout
  sidebar = sidebar(
    open = list(desktop = "open", mobile = "closed"),
    collapsible = TRUE,
    class = "calvex-sidebar",

    # graph time selection: Lifetime or Past Year
    selectInput("time_period", "Time Period:",
      choices = list("Lifetime" = "lifetime",
                  "Past Year" = "past_year"),
      # choices = list("Past Year" = "past_year"),
      selected = "past_year"
    ),

    # graph select violence type
    selectInput("violence", "Violence Type:",
      choices = list("Physical Violence" = "physical",
                  "Sexual Violence" = "sexual",
                  "Intimate Partner Violence" = "ipv",
                  "Sexual Violence Perpetration" = "sexual_perp",
                  "Physical Violence Perpetration" = "physical_perp"),
      selected = "physical"
    ),

    # graph select demographic
    selectInput("demographic", "Display by Demographic:",
      choices = list("Gender" = "GENDER",
        "Age" = "AGE_6",
        "Race/Ethnicity" = "RACE_5",
        "Sexuality" = "LGB_3",
        "Income Quintile" = "INCOME_QUINTILE",
        "Education Level" = "EDUC5",
        "Employment Status" = "EMPLOY_2",
        "Disability Status" = "DISABILITY"),
      selected = "GENDER"
    ),

    selectInput(
      "chart_type",
      "Chart Type:",
      choices = list("Bar chart" = "bar", "Line chart" = "line"),
      selected = "bar"
    ),

    # graph select percentage vs. count
    selectInput("statistics", "Statistics Display:",
      choices = list("Percent Experiencing Violence" = "percent",
                  "Raw Number Experiencing Violence" = "count"),
      selected = "percent"
    ),

    # overall toggle (applies to all demographic types)
    checkboxInput(
      "overall",
      "Overall (all respondents)",
      value = TRUE
    ),

    # show past-year subcategory plots (only when Past Year and not IPV)
    conditionalPanel(
      condition = "input.time_period == 'past_year' && input.violence != 'ipv'",
      checkboxInput(
        "show_subcategories",
        "Show subcategories",
        value = FALSE
      )
    ),

    accordion(
      open = FALSE,
      # accordion panel: Demographic Information
      accordion_panel(
        "Demographic Specifics",

        # checkbox selection for gender identity; GENDER
        checkboxGroupInput(
          "GENDER", "Gender Identity:",
          choices = list(
            "Female" = 1,
            "Male" = 2,
            "Gender non-conforming" = 3
          ),
          selected = list(1, 2, 3)
        ),

        # checkbox selection for self-described sexuality; LGB_3
        checkboxGroupInput(
          "LGB_3", "Self-described sexuality:",
          choices = list(
            "Lesbian / Gay" = 1,
            "Straight" = 2,
            "Bisexual / other identity" = 3
          ),
          selected = list(1, 2, 3)
        ),

        # checkbox selection for age; AGE_6
        checkboxGroupInput(
          "AGE_6", "Age:",
          choices = list(
            "18-24" = 1,
            "25-34" = 2,
            "35-44" = 3,
            "45-54" = 4,
            "55-64" = 5,
            "65+" = 6
          ),
          selected = list(1, 2, 3, 4, 5, 6)
        ),

        # checkbox selection for race/ethnicity; RACE_5
        checkboxGroupInput(
          "RACE_5", "Race / Ethnicity:",
          choices = list(
            "White, Non-Hispanic" = 1,
            "Black, Non-Hispanic" = 2,
            "Asian, Non-Hispanic" = 3,
            "Hispanic" = 4,
            "Other/multiple races, Non-Hispanic" = 5
          ),
          selected = list(1, 2, 3, 4, 5)
        ),

        # checkbox selection for income quintile; INCOME_QUINTILE
        checkboxGroupInput(
          "INCOME_QUINTILE", "Income Quintile:",
          choices = list(
            "Lowest Quintile" = 1,
            "Second Quintile" = 2,
            "Middle Quintile" = 3,
            "Fourth Quintile" = 4,
            "Highest Quintile" = 5
          ),
          selected = list(1, 2, 3, 4, 5)
        ),

        # checkbox selection for education level; EDUC5
        checkboxGroupInput(
          "EDUC5", "Education Level:",
          choices = list(
            "Less than High School" = 1,
            "High School Graduate / Some College" = 2,
            "Bachelor's Degree" = 3,
            "Master's Degree" = 4,
            "Post-Graduate/Professional Degree" = 5
          ), 
          selected = list(1, 2, 3, 4, 5)
        ),

        # checkbox selection for employment status; EMPLOY_2
        checkboxGroupInput(
          "EMPLOY_2", "Employment Status:",
          choices = list(
            "Employed" = 1,
            "Unemployed / Not in Labor Force" = 2
          ),
          selected = list(1, 2)
        ),

        # checkbox selection for disability status; DISABILITY
        checkboxGroupInput(
          "DISABILITY", "Disability Status:",
          choices = list(
            "No Disability" = 0,
            "Has Disability" = 1
          ),
          selected = list(0, 1)
        ),
      ),

      # accordion panel: time & location
      accordion_panel(
        "Time & Location",

        checkboxGroupInput(
          "YEAR", "Survey Year:", 
          choices  = list("2025" = 2025, "2023" = 2023, "2022" = 2022, "2021" = 2021, "2020" = 2020),
          selected = c(2025, 2023, 2022, 2021, 2020)
        ),

        checkboxGroupInput(
          "CA_REGION",
          "California Region:",
          choices = list(
            "Bay Region" = 1,
            "Central Valley" = 2,
            "Mountain Valley" = 3,
            "Northern" = 4,
            "Southern" = 5
          ),
          selected = list(1, 2, 3, 4, 5)
        ),
        helpText(
          style = "font-size: 0.82rem;",
          "Region filter applies to years with region data; respondents with missing region are excluded when filtering."
        )
      ),

      accordion_panel(
        "Notes",
        tags$ul(
          style = "font-size: 0.92rem; padding-left: 1.1rem;",
          tags$li("All data are weighted."),
          tags$li(
            "Interpret proportions and percentages with caution when the cell size is less than 50."
          ),
          tags$li(
            "We cannot assume that differences are statistically significant; see reports for significance levels."
          ),
          tags$li("Raw data can be accessed in full datasets on OpenICPSR."),
          tags$li("Charts can be saved by pressing Ctrl+S or can be opened in a new tab."),
          tags$li("On default, the app will show data for all years and regions."),
        )
      ),
      tags$footer(
        style = paste(
          "padding: clamp(6px, 1.5vw, 10px) clamp(10px, 3vw, 16px); font-size: 0.75rem; color: #444;"
        ),
        HTML(
          paste0(
            "Thomas J, Johns NE, Kully G, Raj A. California Violence Experiences (CalVEX) Online Data Visualization Tool. ",
            "2026. University of California San Diego &amp; Newcomb Institute, Tulane University. ",
            "<a href=\"https://www.vexdata.org/data/caldashboard\" target=\"_blank\" rel=\"noopener noreferrer\">",
            "www.vexdata.org/data/caldashboard</a>."
          )
        )
      )
    ),
  ),


  # main Panel: single plot or side-by-side subcategory plots
  conditionalPanel(
    condition = "!input.show_subcategories || input.time_period != 'past_year' || input.violence == 'ipv'",
    div(
      class = "calvex-plot-wrap",
      div(
        class = "calvex-plot-inner",
        ggiraph::girafeOutput(
          "histogram",
          width  = "100%",
          height = "100%"
        )
      ),
      uiOutput("footnotes_html")
    )
  ),
  conditionalPanel(
    condition = "input.show_subcategories && input.time_period == 'past_year' && input.violence != 'ipv'",
    tagList(
      uiOutput("subcategory_plots_ui"),
      uiOutput("footnotes_html_sub")
    )
  ),

  # tags$footer(
  #   style = paste(
  #     "padding: 10px 16px; font-size: 0.88rem; color: #444;",
  #     "border-top: 1px solid #e0e0e0; margin-top: auto; line-height: 1.45;"
  #   ),
  #   HTML(
  #     paste0(
  #       "Thomas J, Johns NE, Kully G, Raj A. California Violence Experiences (CalVEX) Online Data Visualization Tool. ",
  #       "2026. University of California San Diego &amp; Newcomb Institute, Tulane University. ",
  #       "<a href=\"https://www.vexdata.org/data/caldashboard\" target=\"_blank\" rel=\"noopener noreferrer\">",
  #       "www.vexdata.org/data/caldashboard</a>."
  #     )
  #   )
  # )
)
