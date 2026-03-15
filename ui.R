library(shiny)
library(bslib)
library(markdown) # for shinyapps.io deployment

ui <- page_sidebar(
  # trigger a resize once Shiny finishes its first render so plot fills the container height
  tags$head(
    tags$script(HTML(
      "$(document).one('shiny:idle', function() {
        setTimeout(function() { $(window).trigger('resize'); }, 10);
      });"
    ))
  ),

  # page title
  title = "CalVEX Analysis",

  # sidebar Layout 
  sidebar = sidebar(
    
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
        "Disability Status" = "DISABILITY",
        "Transgender" = "TRANSGENDER"),
      selected = "GENDER"
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
            "Non-binary / Genderqueer / Gender fluid person (2023 only)" = 3,
            "Prefer to self describe (2023 only)" = 4,
            "Prefer not to say" = 98
          ),
          selected = list(1, 2, 3, 4, 98)
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
        #checkbox selection for transgender status; TRANSGENDER
        checkboxGroupInput(
          "TRANSGENDER", "Transgender Status:",
          choices = list(
            "Not Transgender" = 0,
            "Transgender" = 1
          ), 
          selected = list(0, 1)
        ),
      ),

      # accordion panel: time & location
      accordion_panel(
        "Time & Location",

        # checkbox selection for year data was recorded; YEAR
        conditionalPanel(
          condition = "input.violence != 'ipv'",
          checkboxGroupInput(
            "YEAR", "YEAR:",
            choices = list(
              "2025" = 2025,
              "2023" = 2023,
              "2022" = 2022,
              "2021" = 2021,
              "2020" = 2020
            ),
            selected = list(2023, 2022, 2021, 2020)
          )
        ),
        
        conditionalPanel(
          condition = "input.violence == 'ipv'",
          checkboxGroupInput(
            "YEAR", "YEAR:",
            choices = list(
              "2025" = 2025,
              "2023" = 2023,
              "2020" = 2020
            ),
            selected = list(2023, 2020)
          )
        )
      )
    ),
  ),
  

  # main Panel: single plot or side-by-side subcategory plots
  conditionalPanel(
    condition = "!input.show_subcategories || input.time_period != 'past_year' || input.violence == 'ipv'",
    div(
      style = "height: calc(100vh - 80px);",
      plotOutput("histogram", height = "100%")
    )
  ),
  conditionalPanel(
    condition = "input.show_subcategories && input.time_period == 'past_year' && input.violence != 'ipv'",
    uiOutput("subcategory_plots_ui")
  )

)

