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
  
  # Rename gender columns to standard GENDER name
  if ("Q47_T4" %in% names(calvex2023)) {
    names(calvex2023)[names(calvex2023) == "Q47_T4"] <- "GENDER"
  }
  if ("GENDER2" %in% names(calvex2022)) {
    names(calvex2022)[names(calvex2022) == "GENDER_2"] <- "GENDER"
  } else if ("GENDER2" %in% names(calvex2022)) {
    names(calvex2022)[names(calvex2022) == "GENDER_2"] <- "GENDER"
  }
  if ("GENDER2" %in% names(calvex2021)) {
    names(calvex2021)[names(calvex2021) == "GENDER_2"] <- "GENDER"
  } else if ("GENDER2" %in% names(calvex2021)) {
    names(calvex2021)[names(calvex2021) == "GENDER_2"] <- "GENDER"
  }
  if ("GENDER2" %in% names(calvex2020)) {
    names(calvex2020)[names(calvex2020) == "GENDER_2"] <- "GENDER"
  } else if ("GENDER2" %in% names(calvex2020)) {
    names(calvex2020)[names(calvex2020) == "GENDER_2"] <- "GENDER"
  }
  
  # standardize transgender column
  if ("TRANSGENDER" %in% names(calvex2023)) {
    names(calvex2023)[names(calvex2023) == "TRANSGENDER"] <- "TRANSGENDER"
  }
  
  # For 2020-2022, create TRANSGENDER column based on GENDER2 == 3
  calvex2022$TRANSGENDER <- ifelse(calvex2022$GENDER == 3, 1, 0)
  calvex2021$TRANSGENDER <- ifelse(calvex2021$GENDER == 3, 1, 0)
  calvex2020$TRANSGENDER <- ifelse(calvex2020$GENDER == 3, 1, 0)

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
    "DISABILITY",
    "TRANSGENDER",  # new column for transgender
    "sv_perp_ever", # new column for sexual violence perpetration
    "pv_perp_ever", # new column for physical violence perpetration
    "WEIGHT"  # survey weight for denominator = all surveyed in demographic
  )
  calvex2020 <- calvex2020[, cols_needed]
  calvex2021 <- calvex2021[, cols_needed]
  calvex2022 <- calvex2022[, cols_needed]
  calvex2023 <- calvex2023[, cols_needed]

  # Standardize gender codes for all years to match 2023 structure
  standardize_gender <- function(df, year) {
    # Debug: print original gender values distribution
    print(paste("Year", year, "- Original GENDER values:"))
    print(table(df$GENDER, useNA = "ifany"))
    
    df$GENDER <- as.character(df$GENDER)
    
    if (year == 2023) {
      # 2023: Map code 5 (Prefer not to answer) to 98 (Prefer not to say)
      df$GENDER[df$GENDER == "5"] <- "98"  # Prefer not to answer -> Prefer not to say
    } else {
      # 2023: 1=Woman, 2=Man, 3=Non-binary, 4=Prefer to self describe, 98=Prefer not to say
      
      # Create a new column to store mapped values
      df$GENDER_MAPPED <- df$GENDER
      df$GENDER_MAPPED[df$GENDER == "98"] <- "98" # Skipped -> Prefer not to say
      df$GENDER_MAPPED[df$GENDER == "99"] <- "98" # Refused -> Prefer not to say
      
      # Replace original GENDER column with mapped values
      df$GENDER <- df$GENDER_MAPPED
      df$GENDER_MAPPED <- NULL
    }
 
    # Debug: print mapped gender values distribution
    print(paste("Year", year, "- After mapping:"))
    print(table(df$GENDER, useNA = "ifany"))
    
    # Remove any invalid entries and convert to numeric
    valid_categories <- c("1", "2", "3", "4", "98")
    df <- df[!is.na(df$GENDER) & df$GENDER %in% valid_categories, ]
    df$GENDER <- as.numeric(df$GENDER)
    
    # Debug: print final gender values distribution
    print(paste("Year", year, "- Final values:"))
    print(table(df$GENDER, useNA = "ifany"))
    
    df
  }

  calvex2020 <- standardize_gender(calvex2020, 2020)
  calvex2021 <- standardize_gender(calvex2021, 2021)
  calvex2022 <- standardize_gender(calvex2022, 2022)
  calvex2023 <- standardize_gender(calvex2023, 2023)

  calvex_data <- rbind(
    calvex2023,
    calvex2022,
    calvex2021,
    calvex2020
  )

  # lookup tables / labels for graph & legend
  # gender labels (graph) - updated to match 2023 structure
  gender_labels <- c(
    "1" = "Female",
    "2" = "Male",
    "3" = "Non-binary / Genderqueer / Gender fluid person", # Only available in 2023 data
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
  # transgender labels (graph)
  transgender_labels <- c(
    "0" = "Not Transgender",
    "1" = "Transgender"
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
  "DISABILITY" = "DISABILITY (Disability Status)",
  "TRANSGENDER" = "TRANSGENDER (Transgender Identity)"
  )

  # map demographic variable name -> code-to-label vector (for filtering which bars to show)
  demographic_labels <- list(
    GENDER = gender_labels,
    AGE_6 = age_labels,
    RACE_5 = race_labels,
    LGB_3 = sexuality_labels,
    INCOME_QUINTILE = income_labels,
    EDUC_4 = education_labels,
    EMPLOY_2 = employment_labels,
    DISABILITY = disability_labels,
    TRANSGENDER = transgender_labels
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
    if (!is.null(input$TRANSGENDER) && length(input$TRANSGENDER) > 0)
      df <- df[df$TRANSGENDER %in% suppressWarnings(as.numeric(input$TRANSGENDER)), ]

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
    df$TRANSGENDER <- factor(
      df$TRANSGENDER,
      levels = names(transgender_labels),
      labels = transgender_labels
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

    # violence outcomes
    violence_col <- if (violence_type == "physical") "pv_ever" else if (violence_type == "sexual") "sv_ever" else if (violence_type == "ipv" ) "ipv_ever" else if (violence_type == "sexual_perp") "sv_perp_ever" else "pv_perp_ever" # nolint: line_length_linter.

    # summarize by year, demographic, & statistic (count & percent)
    # Percent = weighted (people in demographic who experienced violence) / weighted (all in that demographic surveyed in that year)
    # Use survey WEIGHT so denominator = all surveyed in demographic (weights represent full sample); consistent for all years
    # compute by selected demographic
    summary_df <- df %>%
      group_by(data_year, .data[[demographic]]) %>%
      summarise(
        violence_count = sum(.data[[violence_col]] == 1, na.rm = TRUE),
        violence_count_weighted = sum(WEIGHT * (.data[[violence_col]] == 1), na.rm = TRUE),
        total_surveyed_in_demographic = sum(WEIGHT, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(violence_percent = (violence_count_weighted / total_surveyed_in_demographic) * 100)

    # optionally add an overall bar for each year (all respondents, regardless of demographic category)
    show_overall <- is.null(input$overall) || isTRUE(input$overall)
    if (show_overall) {
      overall_df <- df %>%
        group_by(data_year) %>%
        summarise(
          violence_count = sum(.data[[violence_col]] == 1, na.rm = TRUE),
          violence_count_weighted = sum(WEIGHT * (.data[[violence_col]] == 1), na.rm = TRUE),
          total_surveyed_in_demographic = sum(WEIGHT, na.rm = TRUE),
          .groups = "drop"
        )
      overall_df[[demographic]] <- "Overall"
      overall_df$violence_percent <- (overall_df$violence_count_weighted /
        overall_df$total_surveyed_in_demographic) * 100

      summary_df <- dplyr::bind_rows(summary_df, overall_df)

      # ensure 'Overall' is the leftmost bar within each year (when present)
      if (any(summary_df[[demographic]] == "Overall", na.rm = TRUE)) {
        demo_vals <- as.character(summary_df[[demographic]])
        other_levels <- sort(unique(demo_vals[demo_vals != "Overall"]))
        summary_df[[demographic]] <- factor(
          demo_vals,
          levels = c("Overall", other_levels)
        )
      }
    }

    # show only bars for selected demographic labels (or only Overall if none selected)
    selected_codes <- input[[demographic]]
    labels_vec <- demographic_labels[[demographic]]
    if (!is.null(labels_vec)) {
      if (is.null(selected_codes) || length(selected_codes) == 0) {
        # no labels selected -> show only Overall bar(s)
        summary_df <- dplyr::filter(summary_df, .data[[demographic]] == "Overall")
      } else {
        # map selected checkbox codes to label names
        selected_labels <- unname(labels_vec[names(labels_vec) %in% as.character(selected_codes)])
        to_keep <- c(selected_labels, if (show_overall) "Overall" else character(0))
        to_keep <- to_keep[!is.na(to_keep)]
        if (length(to_keep) > 0) {
          summary_df <- dplyr::filter(summary_df, .data[[demographic]] %in% to_keep)
        }
      }
    }

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
    v_title <- if (violence_type == "physical") "Physical Violence" else if (violence_type == "sexual") "Sexual Violence" else if (violence_type == "ipv") "Intimate Partner Violence" else if (violence_type == "sexual_perp") "Sexual Violence Perpetration" else "Physical Violence Perpetration" # nolint: line_length_linter.

    # fill colors: Overall = dark grey, others from default palette
    fill_levels <- unique(as.character(summary_df[[demographic]]))
    n_other <- length(fill_levels) - sum(fill_levels == "Overall", na.rm = TRUE)
    fill_values <- c("Overall" = "darkgrey")
    if (n_other > 0) {
      other_levels <- fill_levels[fill_levels != "Overall"]
      fill_values <- c(fill_values, setNames(scales::hue_pal()(n_other), other_levels))
    }

    # build plot
    ggplot(summary_df, aes(
      x = factor(data_year), 
      y = value, 
      fill = .data[[demographic]])) +
      geom_col(position = position_dodge()) +
      geom_text(
        aes(label = label), 
        vjust = -0.5, 
        position = position_dodge(width = 0.9)) +
      scale_fill_manual(values = fill_values) +
      labs(
        x = "Year",
        y = y_lab,
        fill = graph_labels[demographic],
        title = paste(v_title, "Experience by", graph_labels[demographic], "–", paste(sort(unique(df$data_year)), collapse = ", "))) +
      ylim(0, ylim_max) +
      theme_minimal()
  })

}

