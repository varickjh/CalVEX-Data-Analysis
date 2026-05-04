library(shiny)
library(bslib)
library(markdown) # for shinyapps.io deployment

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
    "))
  ),

  # page title
  title = "CalVEX Analysis",

  # sidebar Layout 
  sidebar = sidebar(
    open = list(desktop = "open", mobile = "closed"),
    collapsible = TRUE,
    
    # graph time selection: Lifetime or Past Year
    selectInput("time_period", "Time Period:",
      choices = list("Lifetime" = "lifetime",
                  "Past Year" = "past_year"),
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
        "Education Level" = "EDUC_4",
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
      # accordion panel: Demographic Information
      accordion_panel(
        "Demographic Specifics",

        # checkbox selection for gender identity; GENDER
        checkboxGroupInput(
          "GENDER", "Gender Identity:",
          choices = list(
            "Female" = 1,
            "Male" = 2,
            "Gender non-conforming" = 3,
            "Prefer not to say" = 98
          ),
          selected = list(1, 2, 3, 98)
        ),

        # checkbox selection for self-described sexuality; LGB_3
        checkboxGroupInput(
          "LGB_3", "Self-described sexuality:",
          choices = list(
            "Lesbian / Gay" = 1,
            "Straight" = 2,
            "Bisexual / other identity" = 3,
            "Prefer not to say" = 98
          ),
          selected = list(1, 2, 3, 98)
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
            "White, NH" = 1,
            "Black, NH" = 2,
            "Asian, NH" = 3,
            "Hispanic" = 4,
            "Other/multiple races, NH" = 5
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

        # checkbox selection for education level; EDUC_4
        checkboxGroupInput(
          "EDUC_4", "Education Level:",
          choices = list(
            "Less than High School" = 1,
            "High School Graduate / Some College" = 2,
            "Bachelor's Degree" = 3,
            "Master's Degree" = 4
          ), 
          selected = list(1, 2, 3, 4)
        ),
        #checkbox selection for employment status; EMPLOY_2
        checkboxGroupInput(
          "EMPLOY_2", "Employment Status:",
          choices = list(
            "Employed" = 1,
            "Unemployed / Not in Labor Force" = 2
          ),
          selected = list(1, 2)
        ),
        #checkbox selection for disability status; DISABILITY
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

        uiOutput("year_ui"),

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
        "Notes & Citations",
        tags$ul(
          style = "font-size: 0.92rem; padding-left: 1.1rem;",
          tags$li("All data are weighted."),
          tags$li(
            "Interpret proportions and percentages with caution when the cell size is less than 50."
          ),
          tags$li(
            "We cannot assume that differences are statistically significant; see reports for significance levels."
          ),
          tags$li("Raw data can be accessed in full datasets on OpenICPSR.")
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
        plotOutput(
          "histogram",
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

  tags$footer(
    style = paste(
      "padding: 10px 16px; font-size: 0.88rem; color: #444;",
      "border-top: 1px solid #e0e0e0; margin-top: auto; line-height: 1.45;"
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
)

