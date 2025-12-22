library(shiny)
library(bslib)
library(markdown) # for shinyapps.io deployment

ui <- page_sidebar(
  # page title
  title = "CalVEX Analysis",

  # sidebar Layout 
  sidebar = sidebar(
    
    # graph select violence type
    selectInput("violence", "Violence Type:",
      choices = list("Physical Violence" = "physical",
                  "Sexual Violence" = "sexual"),
      selected = "physical"
    ),

    # graph select demographic
    selectInput("demographic", "Display by Demographic:",
      choices = list("Gender" = "GENDER_2",
        "Age" = "AGE_6",
        "Race/Ethnicity" = "RACE_5",
        "Sexuality" = "LGB_2"),
      selected = "GENDER_2"
    ),

    # graph select percentage vs. count
    selectInput("statistics", "Statistics Display:",
      choices = list("Percent Experiencing Violence" = "percent",
                  "Raw Number Experiencing Violence" = "count"),
      selected = "percent"
    ),

    accordion(
      # accordion panel: Demographic Information
      accordion_panel(
        "Demographic Specifics",

        # checkbox selection for 2-level gender identity; GENDER_2
        checkboxGroupInput(
          "GENDER_2", "2-level Gender Identity:",
          choices = list(
            "Woman" = 1, 
            "Man" = 2
          ),
          selected = list(1, 2)
        ),

        # checkbox selection for self-described sexuality; LGB_2
        checkboxGroupInput(
          "LGB_2", "Self-described sexuality:",
          choices = list(
            "LGB/other identity" = 1, 
            "Straight" = 2
          ),
          selected = list(1, 2)
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
        )
      ),

      # accordion panel: time & location
      accordion_panel(
        "Time & Location",

        # checkbox selection for year data was recorded; YEAR
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
      )
    ),
  ),
  

  # main Panel
  plotOutput("histogram")

)

