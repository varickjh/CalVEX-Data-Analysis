library(shiny)
library(dplyr)
library(ggplot2)

server <- function(input, output) {
  
  # data organization
  calvex2023 <- read.csv("data/CalVEX2023.csv")
  calvex2022 <- read.csv("data/CalVEX2022.csv")
  calvex2021 <- read.csv("data/CalVEX2021.csv")
  calvex2020 <- read.csv("data/CalVEX2020.csv")

  # standardize IPV column names across years
  if ("IPV22_EVER" %in% names(calvex2023)) names(calvex2023)[names(calvex2023) == "IPV22_EVER"] <- "ipv_ever" # nolint: line_length_linter.
  if ("ipv_12mo" %in% names(calvex2022)) names(calvex2022)[names(calvex2022) == "ipv_12mo"] <- "ipv_ever" # nolint: line_length_linter.
  if ("ipv_12mo" %in% names(calvex2021)) names(calvex2021)[names(calvex2021) == "ipv_12mo"] <- "ipv_ever" # nolint: line_length_linter.
  if ("ipv_ever" %in% names(calvex2020)) names(calvex2020)[names(calvex2020) == "ipv_ever"] <- "ipv_ever" # nolint: line_length_linter.

  # standardize gender column names across years
  if ("Q47_T4" %in% names(calvex2023)) names(calvex2023)[names(calvex2023) == "Q47_T4"] <- "GENDER" # nolint: line_length_linter.
  if ("GENDER2" %in% names(calvex2022)) names(calvex2022)[names(calvex2022) == "GENDER2"] <- "GENDER" # nolint: line_length_linter.
  if ("GENDER2" %in% names(calvex2021)) names(calvex2021)[names(calvex2021) == "GENDER2"] <- "GENDER" # nolint: line_length_linter.
  if ("GENDER2" %in% names(calvex2020)) names(calvex2020)[names(calvex2020) == "GENDER2"] <- "GENDER" # nolint: line_length_linter.
  
  # new transgender column
  # if ("TRANSGENDER" %in% names(calvex2023)) names(calvex2023)[names(calvex2023) == "TRANSGENDER"] <- "TRANSGENDER" # nolint: line_length_linter.

  # isolate variables we are comparing (can change later) & combine into dataset
  cols_needed <- c(
    "GENDER",
    "LGB_3",
    "AGE_6",
    "RACE_5",
    "pv_ever",
    "sv_ever",
    "ipv_ever",  # new column for intimate partner violence
    "data_year",
    "INCOME_QUINTILE",
    "EDUC_4",
    "EMPLOY_2",
    "DISABILITY"
    # "TRANSGENDER"  # new column for transgender
  )
  calvex2020 <- calvex2020[, cols_needed]
  calvex2021 <- calvex2021[, cols_needed]
  calvex2022 <- calvex2022[, cols_needed]
  calvex2023 <- calvex2023[, cols_needed]

  # Standardize gender codes for 2020, 2021, and 2022 data
  standardize_gender <- function(df) {
    df$GENDER <- as.character(df$GENDER)
    
    # Map 2020-2022 GENDER2 codes to 2023 Q47_T4 structure
    # 2020-2022: 1=Male, 2=Female, 3=Transgender, 4=Do not identify, 77=Don't Know, 98=Skipped, 99=Refused
    # 2023: 1=Woman, 2=Man, 3=Non-binary, 4=Prefer to self describe, 98=Prefer not to say
    
    # Create mapping for 2020-2022 to 2023 structure
    df$GENDER[df$GENDER == "1"] <- "2"  # Male -> Man
    df$GENDER[df$GENDER == "2"] <- "1"  # Female -> Woman
    df$GENDER[df$GENDER == "3"] <- "3"  # Transgender -> Non-binary (closest match)
    df$GENDER[df$GENDER == "4"] <- "3"  # Do not identify -> Non-binary (closest match)
    df$GENDER[df$GENDER == "77"] <- "98" # Don't Know -> Prefer not to say
    df$GENDER[df$GENDER == "98"] <- "98" # Skipped -> Prefer not to say
    df$GENDER[df$GENDER == "99"] <- "98" # Refused -> Prefer not to say
    
    # Remove any invalid entries
    df <- df[!is.na(df$GENDER) & df$GENDER %in% c("1", "2", "3", "4", "98"), ]
    df$GENDER <- as.numeric(df$GENDER)
    df
  }

  calvex2020 <- standardize_gender(calvex2020)
  calvex2021 <- standardize_gender(calvex2021)
  calvex2022 <- standardize_gender(calvex2022)

  calvex_data <- rbind(
    calvex2023,
    calvex2022,
    calvex2021,
    calvex2020
  )

  # lookup tables / labels for graph & legend
  # gender labels (graph) - updated to match 2023 structure
  gender_labels <- c(
    "1" = "Woman",
    "2" = "Man",
    "3" = "Non-binary / Genderqueer / Gender fluid person",
    "4" = "Prefer to self describe", # Only available in 2023 data
    "98" = "Prefer not to say"
  )
  # sexuality labels (graph)
  sexuality_labels <- c(
    "1" = "Lesbian / Gay",
    "2" = "Straight",
    "3" = "Bisexual / other identity",
    "98" = "Prefer not to say"
  )
  # income labels (graph) 
  income_labels <- c(
    "1" = "Lowest Quintile",
    "2" = "Second Quintile",
    "3" = "Middle Quintile",
    "4" = "Fourth Quintile",
    "5" = "Highest Quintile"
  )
  # education labels (graph)
  education_labels <- c(
    "1" = "Less than High School",
    "2" = "High School Graduate / Some College",
    "3" = "Bachelor's Degree",
    "4" = "Master's Degree"
  )
  # employment labels (graph)
  employment_labels <- c(
    "1" = "Employed",
    "2" = "Unemployed / Not in Labor Force"
  )
  # disability labels (graph)
  disability_labels <- c(
    "0" = "No Disability",
    "1" = "Has Disability"
  )
  # age labels (graph)
  age_labels <- c(
    "1" = "18–24",
    "2" = "25–34",
    "3" = "35–44",
    "4" = "45–54",
    "5" = "55–64",
    "6" = "65+"
  )
  # race labels (graph)
  race_labels <- c(
    "1" = "White, NH",
    "2" = "Black, NH",
    "3" = "Asian, NH",
    "4" = "Hispanic",
    "5" = "Other/multiple races, NH"
  )
  # legend labels
  graph_labels <- c(
  "GENDER" = "GENDER (Gender)",
  "AGE_6" = "AGE_6 (Age)",
  "RACE_5" = "RACE_5 (Race/Ethnicity)",
  "LGB_3" = "LGB_3 (Sexuality)",
  "INCOME_QUINTILE" = "INCOME_QUINTILE (Income Quintile)",
  "EDUC_4" = "EDUC_4 (Education Level)",
  "EMPLOY_2" = "EMPLOY_2 (Employment Status)",
  "DISABILITY" = "DISABILITY (Disability Status)"
  )

    # reactive subset
  filtered_data <- reactive({
    df <- calvex_data

    # filter out 2021 and 2022 for IPV
    if (!is.null(input$violence) && input$violence == "ipv") {
      df <- df[!df$data_year %in% c(2021, 2022), ]
    }

    # filter by demographics
    if (!is.null(input$GENDER) && length(input$GENDER) > 0)
      df <- df[df$GENDER %in% suppressWarnings(as.numeric(input$GENDER)), ]
    if (!is.null(input$LGB_3) && length(input$LGB_3) > 0)
      df <- df[df$LGB_3 %in% suppressWarnings(as.numeric(input$LGB_3)), ]
    if (!is.null(input$AGE_6) && length(input$AGE_6) > 0)
      df <- df[df$AGE_6 %in% suppressWarnings(as.numeric(input$AGE_6)), ]
    if (!is.null(input$RACE_5) && length(input$RACE_5) > 0)
      df <- df[df$RACE_5 %in% suppressWarnings(as.numeric(input$RACE_5)), ]
    if (!is.null(input$INCOME_QUINTILE) && length(input$INCOME_QUINTILE) > 0)
      df <- df[df$INCOME_QUINTILE %in% suppressWarnings(as.numeric(input$INCOME_QUINTILE)), ]
    if (!is.null(input$EDUC_4) && length(input$EDUC_4) > 0)
      df <- df[df$EDUC_4 %in% suppressWarnings(as.numeric(input$EDUC_4)), ]
    if (!is.null(input$EMPLOY_2) && length(input$EMPLOY_2) > 0)
      df <- df[df$EMPLOY_2 %in% suppressWarnings(as.numeric(input$EMPLOY_2)), ]
    if (!is.null(input$DISABILITY) && length(input$DISABILITY) > 0)
      df <- df[df$DISABILITY %in% suppressWarnings(as.numeric(input$DISABILITY)), ]

    # filter by year
    if (!is.null(input$YEAR) && length(input$YEAR) > 0)
      df <- df[df$data_year %in% suppressWarnings(as.numeric(input$YEAR)), ]

    # convert codes to labels for plotting
    df$GENDER <- factor(
      df$GENDER,
      levels = names(gender_labels),
      labels = gender_labels,
    )
    df$AGE_6 <- factor(
      df$AGE_6, 
      levels = names(age_labels), 
      labels = age_labels
    )
    df$RACE_5 <- factor(
      df$RACE_5, 
      levels = names(race_labels), 
      labels = race_labels
    )
    df$LGB_3 <- factor(
      df$LGB_3, 
      levels = names(sexuality_labels), 
      labels = sexuality_labels
    )
    df$INCOME_QUINTILE <- factor(
      df$INCOME_QUINTILE,
      levels = names(income_labels),
      labels = income_labels
    )
    df$EDUC_4 <- factor(
      df$EDUC_4,
      levels = names(education_labels),
      labels = education_labels
    )
    df$EMPLOY_2 <- factor(
      df$EMPLOY_2,
      levels = names(employment_labels),
      labels = employment_labels
    )
    df$DISABILITY <- factor(
      df$DISABILITY,
      levels = names(disability_labels),
      labels = disability_labels
    )
    df
  })

# output: plot comparing violence across demographics and years
  output$histogram <- renderPlot({
    df <- filtered_data()
    
    # use selected demographic & statistics type
    demographic <- input$demographic
    stat_type <- input$statistics
    violence_type <- input$violence

    # choose between physical, sexual, and intimate partner violence
    violence_col <- if (violence_type == "physical") "pv_ever" else if (violence_type == "sexual") "sv_ever" else "ipv_ever" # nolint: line_length_linter.

    # summarize by year, demographic, & statistic (count & percent)
    summary_df <- df %>%
    group_by(data_year, .data[[demographic]]) %>%
    summarise(violence_count = sum(.data[[violence_col]] == 1,
      na.rm = TRUE), 
      total = n(), 
      .groups = "drop"
    ) %>%
    mutate(violence_percent = (violence_count / total) * 100)

    # choose between count & percent
    if (stat_type == "percent") {
    summary_df <- summary_df %>%
      mutate(
        value = violence_percent,
        label = paste0(round(violence_percent, 1), "%")
      )
    y_lab <- "Percent Experiencing Violence"
    ylim_max <- 100
    } 
    if (stat_type == "count") { 
      summary_df <- summary_df %>%
        mutate(
          value = violence_count,
          label = violence_count
        )
      y_lab <- "Number Experiencing Violence"
      ylim_max <- max(summary_df$value, na.rm = TRUE) * 1.1
    }

    # update the title to reflect the type of violence
    v_title <- if (violence_type == "physical") "Physical Violence" else if (violence_type == "sexual") "Sexual Violence" else "Intimate Partner Violence" # nolint: line_length_linter.

    # build plot
    ggplot(summary_df, aes(
      x = factor(data_year), 
      y = value, 
      fill = .data[[demographic]])) +
      # add labels
      geom_col(position = position_dodge()) +
      geom_text(
        aes(label = label), 
        vjust = -0.5, 
        position = position_dodge(width = 0.9)) +
      # add labels
      labs(
        x = "Year",
        y = y_lab,
        fill = graph_labels[demographic],
        title = paste(v_title, "Experience by", graph_labels[demographic], "–", paste(sort(unique(df$data_year)), collapse = ", "))      ) +
      ylim(0, ylim_max) +
      theme_minimal()
    }
  )


}

