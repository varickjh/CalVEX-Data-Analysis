library(shiny)
library(dplyr)
library(ggplot2)
library(grid)

server <- function(input, output) {
  
  # data organization
  calvex2023 <- read.csv("data/CalVEX2023.csv")
  calvex2022 <- read.csv("data/CalVEX2022.csv")
  calvex2021 <- read.csv("data/CalVEX2021.csv")
  calvex2020 <- read.csv("data/CalVEX2020.csv")

  # standardization of column names (for violence outcomes) across years
  # IPV
  if ("IPV22_EVER" %in% names(calvex2023)) names(calvex2023)[names(calvex2023) == "IPV22_EVER"] <- "ipv_ever" # nolint: line_length_linter.
  if ("ipv_ever" %in% names(calvex2020)) names(calvex2020)[names(calvex2020) == "ipv_ever"] <- "ipv_ever" # nolint: line_length_linter.

  # SV, PV, SV Perp, PV Perp (no standardization needed)
  
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
    "pv_12mo",
    "sv_ever",
    "sv_12mo",
    "ipv_ever",
    "ipv_12mo",
    "data_year",
    "INCOME_QUINTILE",
    "EDUC_4",
    "EMPLOY_2",
    "DISABILITY",
    "TRANSGENDER",
    "sv_perp_ever",
    "sv_perp_12mo",
    "pv_perp_ever",
    "pv_perp_12mo",
    "WEIGHT",
    # past-year subcategories (physical violence 12mo)
    "pastyearpv1",
    "pastyearpv2",
    "pastyearpv3",
    # past-year subcategories (sexual violence 12mo)
    "pastyearsv1",
    "pastyearsv2",
    "pastyearsv3",
    "pastyearsv4",
    "pastyearsv5",
    "pastyearsv6",
    # past-year subcategories (SV perpetration)
    "pastyearperpsv1",
    "pastyearperpsv2",
    "pastyearperpsv3",
    "pastyearperpsv4",
    "pastyearperpsv5",
    "pastyearperpsv6",
    # past-year subcategories (PV perpetration)
    "pastyearperppv1",
    "pastyearperppv2",
    "pastyearperppv3"
  )

  # ensure all placeholder columns exist for each year (create as NA if missing)
  for (col in cols_needed) {
    if (!col %in% names(calvex2020)) calvex2020[[col]] <- NA
    if (!col %in% names(calvex2021)) calvex2021[[col]] <- NA
    if (!col %in% names(calvex2022)) calvex2022[[col]] <- NA
    if (!col %in% names(calvex2023)) calvex2023[[col]] <- NA
  }

  # now subset to the standardized set of columns
  calvex2020 <- calvex2020[, cols_needed]
  calvex2021 <- calvex2021[, cols_needed]
  calvex2022 <- calvex2022[, cols_needed]
  calvex2023 <- calvex2023[, cols_needed]

  # Standardize gender codes for all years to match 2023 structure
  standardize_gender <- function(df, year) {
    
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
    
    # Remove any invalid entries and convert to numeric
    valid_categories <- c("1", "2", "3", "4", "98")
    df <- df[!is.na(df$GENDER) & df$GENDER %in% valid_categories, ]
    df$GENDER <- as.numeric(df$GENDER)
    
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
    time_period <- if (!is.null(input$time_period)) input$time_period else "lifetime"

    # time-aware filtering based on which violence variables actually exist
    if (!is.null(input$violence)) {
      # IPV: specific years for lifetime vs 12mo
      if (input$violence == "ipv") {
        if (time_period == "lifetime") {
          # only years with lifetime IPV: 2020, 2023
          df <- df[df$data_year %in% c(2020, 2023), ]
        } else {
          # only years with 12mo IPV: 2021, 2022
          df <- df[df$data_year %in% c(2021, 2022), ]
        }
      }

      # Sexual violence perpetration – past year: drop years with no 12mo data (e.g., 2020)
      if (input$violence == "sexual_perp" && time_period == "past_year") {
        df <- df[!is.na(df$sv_perp_12mo), ]
      }

      # Physical violence perpetration – past year: drop years with no 12mo data (e.g., 2020)
      if (input$violence == "physical_perp" && time_period == "past_year") {
        df <- df[!is.na(df$pv_perp_12mo), ]
      }
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

  # Extract legend grob from a ggplot (for shared legend in subcategory grid)
  get_legend_grob <- function(a_plot) {
    if (!inherits(a_plot, "ggplot")) return(NULL)
    tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a_plot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    if (length(leg) > 0) tmp$grobs[[leg]] else NULL
  }

  # Build footnote lines based on current violence type
  build_footnote_lines <- function(violence_type) {
    lines <- "* Background bars represent the total\n  number of people surveyed in that\n  demographic"
    if (violence_type == "ipv") {
      lines <- c(lines,
        "* 2021 & 2022 have no lifetime IPV\n  data; 2020 & 2023 have no past-year\n  (12-month) IPV data")
    } else if (violence_type == "sexual_perp") {
      lines <- c(lines, "* sv_perp_12mo was not asked in 2020")
    } else if (violence_type == "physical_perp") {
      lines <- c(lines, "* pv_perp_12mo was not asked in 2020")
    }
    lines
  }

  # Draw legend grob + footnotes stacked vertically inside the current viewport
  draw_legend_with_footnotes <- function(legend_grob, footnote_lines) {
    note_text <- paste(footnote_lines, collapse = "\n\n")
    fn_grob <- grid::textGrob(
      note_text,
      x = 0.05, y = 0.98,
      just = c("left", "top"),
      gp = grid::gpar(fontsize = 8, col = "gray50")
    )
    right_layout <- grid::grid.layout(2, 1, heights = grid::unit(c(0.55, 0.45), "npc"))
    grid::pushViewport(grid::viewport(layout = right_layout))
    if (!is.null(legend_grob)) {
      grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
      grid::grid.draw(legend_grob)
      grid::popViewport()
    }
    grid::pushViewport(grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
    grid::grid.draw(fn_grob)
    grid::popViewport()
    grid::popViewport()
  }

  # Build one bar chart (same format as main plot) for a given violence column
  make_one_plot <- function(df, violence_col, plot_title, demographic, stat_type,
                            show_overall, demographic_labels, graph_labels, show_legend = TRUE) {
    if (!violence_col %in% names(df)) return(NULL)
    summary_df <- df %>%
      group_by(data_year, .data[[demographic]]) %>%
      summarise(
        n_total = n(),
        violence_count = sum(.data[[violence_col]] == 1, na.rm = TRUE),
        violence_count_weighted = sum(WEIGHT * (.data[[violence_col]] == 1), na.rm = TRUE),
        total_surveyed_in_demographic = sum(WEIGHT, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(violence_percent = (violence_count_weighted / total_surveyed_in_demographic) * 100)

    if (show_overall) {
      overall_df <- df %>%
        group_by(data_year) %>%
        summarise(
          n_total = n(),
          violence_count = sum(.data[[violence_col]] == 1, na.rm = TRUE),
          violence_count_weighted = sum(WEIGHT * (.data[[violence_col]] == 1), na.rm = TRUE),
          total_surveyed_in_demographic = sum(WEIGHT, na.rm = TRUE),
          .groups = "drop"
        )
      overall_df[[demographic]] <- "Overall"
      overall_df$violence_percent <- (overall_df$violence_count_weighted /
        overall_df$total_surveyed_in_demographic) * 100
      summary_df <- dplyr::bind_rows(summary_df, overall_df)
      if (any(summary_df[[demographic]] == "Overall", na.rm = TRUE)) {
        demo_vals <- as.character(summary_df[[demographic]])
        other_levels <- sort(unique(demo_vals[demo_vals != "Overall"]))
        summary_df[[demographic]] <- factor(demo_vals, levels = c("Overall", other_levels))
      }
    }

    selected_codes <- input[[demographic]]
    labels_vec <- demographic_labels[[demographic]]
    if (!is.null(labels_vec)) {
      if (is.null(selected_codes) || length(selected_codes) == 0) {
        summary_df <- dplyr::filter(summary_df, .data[[demographic]] == "Overall")
      } else {
        selected_labels <- unname(labels_vec[names(labels_vec) %in% as.character(selected_codes)])
        to_keep <- c(selected_labels, if (show_overall) "Overall" else character(0))
        to_keep <- to_keep[!is.na(to_keep)]
        if (length(to_keep) > 0) {
          summary_df <- dplyr::filter(summary_df, .data[[demographic]] %in% to_keep)
        }
      }
    }

    if (stat_type == "percent") {
      summary_df <- summary_df %>%
        mutate(
          value = violence_percent,
          label = paste0(round(violence_percent, 1), "%"),
          denom_value = 100
        )
      y_lab <- "Percent Experiencing Violence"
      ylim_max <- 107
    } else {
      summary_df <- summary_df %>%
        mutate(
          value = violence_count,
          label = violence_count,
          denom_value = n_total
        )
      y_lab <- "Number Experiencing Violence"
      ylim_max <- max(summary_df$n_total, na.rm = TRUE) * 1.15
    }

    fill_levels <- unique(as.character(summary_df[[demographic]]))
    n_other <- length(fill_levels) - sum(fill_levels == "Overall", na.rm = TRUE)
    fill_values <- c("Overall" = "darkgrey")
    if (n_other > 0) {
      other_levels <- fill_levels[fill_levels != "Overall"]
      fill_values <- c(fill_values, setNames(scales::hue_pal()(n_other), other_levels))
    }

    p <- ggplot(summary_df, aes(x = factor(data_year), fill = .data[[demographic]])) +
      geom_col(aes(y = denom_value), position = position_dodge(), alpha = 0.3, show.legend = FALSE) +
      geom_text(
        aes(y = denom_value, label = paste0(n_total)),
        vjust = -0.5, position = position_dodge(width = 0.9), size = 3, color = "gray40"
      ) +
      geom_col(aes(y = value), position = position_dodge()) +
      geom_text(aes(y = value, label = label), vjust = -0.5, position = position_dodge(width = 0.9)) +
      scale_fill_manual(values = fill_values) +
      labs(
        x = "Year",
        y = y_lab,
        fill = graph_labels[demographic],
        title = plot_title
      ) +
      ylim(0, ylim_max) +
      theme_minimal()
    if (!show_legend) {
      p <- p + theme(legend.position = "none")
    }
    p
  }

  # Past-year subcategory config: list of (column, title) per violence type
  subcategory_config <- list(
    physical = list(
      list(col = "pastyearpv1", title = "Physical abuse (past year)"),
      list(col = "pastyearpv2", title = "Knife violence (past year)"),
      list(col = "pastyearpv3", title = "Gun violence (past year)")
    ),
    sexual = list(
      list(col = "pastyearsv1", title = "Verbal SH Trans- or Homophobic SH (past year)"),
      list(col = "pastyearsv2", title = "Cyber SH (past year)"),
      list(col = "pastyearsv3", title = "Physically aggressive SH (past year)"),
      list(col = "pastyearsv4", title = "Quid pro quo SH (past year)"),
      list(col = "pastyearsv5", title = "Sexual coercion (past year)"),
      list(col = "pastyearsv6", title = "Forced sex (past year)")
    ),
    sexual_perp = list(
      list(col = "pastyearperpsv1", title = "Perpetrated Verbal SH Trans- or Homophobic SH (past year)"),
      list(col = "pastyearperpsv2", title = "Perpetrated Cyber SH (past year)"),
      list(col = "pastyearperpsv3", title = "Perpetrated Physically aggressive SH (past year)"),
      list(col = "pastyearperpsv4", title = "Perpetrated Quid pro quo SH (past year)"),
      list(col = "pastyearperpsv5", title = "Perpetrated Sexual coercion (past year)"),
      list(col = "pastyearperpsv6", title = "Perpetrated Forced sex (past year)")
    ),
    physical_perp = list(
      list(col = "pastyearperppv1", title = "Perpetrated physical abuse (past year)"),
      list(col = "pastyearperppv2", title = "Perpetrated Knife violence (past year)"),
      list(col = "pastyearperppv3", title = "Perpetrated Gun violence (past year)")
    )
  )

  # output: single plot (when not showing subcategories)
  output$histogram <- renderPlot({
    if (isTRUE(input$show_subcategories) &&
        identical(input$time_period, "past_year") &&
        input$violence != "ipv") {
      return(NULL)
    }
    df <- filtered_data()
    demographic <- input$demographic
    stat_type <- input$statistics
    violence_type <- input$violence
    time_period <- if (!is.null(input$time_period)) input$time_period else "lifetime"

    violence_col <- if (violence_type == "physical") {
      if (time_period == "lifetime") "pv_ever" else "pv_12mo"
    } else if (violence_type == "sexual") {
      if (time_period == "lifetime") "sv_ever" else "sv_12mo"
    } else if (violence_type == "ipv") {
      if (time_period == "lifetime") "ipv_ever" else "ipv_12mo"
    } else if (violence_type == "sexual_perp") {
      if (time_period == "lifetime") "sv_perp_ever" else "sv_perp_12mo"
    } else {
      if (time_period == "lifetime") "pv_perp_ever" else "pv_perp_12mo"
    }

    summary_df <- df %>%
      group_by(data_year, .data[[demographic]]) %>%
      summarise(
        n_total = n(),
        violence_count = sum(.data[[violence_col]] == 1, na.rm = TRUE),
        violence_count_weighted = sum(WEIGHT * (.data[[violence_col]] == 1), na.rm = TRUE),
        total_surveyed_in_demographic = sum(WEIGHT, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(violence_percent = (violence_count_weighted / total_surveyed_in_demographic) * 100)

    show_overall <- is.null(input$overall) || isTRUE(input$overall)
    if (show_overall) {
      overall_df <- df %>%
        group_by(data_year) %>%
        summarise(
          n_total = n(),
          violence_count = sum(.data[[violence_col]] == 1, na.rm = TRUE),
          violence_count_weighted = sum(WEIGHT * (.data[[violence_col]] == 1), na.rm = TRUE),
          total_surveyed_in_demographic = sum(WEIGHT, na.rm = TRUE),
          .groups = "drop"
        )
      overall_df[[demographic]] <- "Overall"
      overall_df$violence_percent <- (overall_df$violence_count_weighted /
        overall_df$total_surveyed_in_demographic) * 100
      summary_df <- dplyr::bind_rows(summary_df, overall_df)
      if (any(summary_df[[demographic]] == "Overall", na.rm = TRUE)) {
        demo_vals <- as.character(summary_df[[demographic]])
        other_levels <- sort(unique(demo_vals[demo_vals != "Overall"]))
        summary_df[[demographic]] <- factor(demo_vals, levels = c("Overall", other_levels))
      }
    }

    selected_codes <- input[[demographic]]
    labels_vec <- demographic_labels[[demographic]]
    if (!is.null(labels_vec)) {
      if (is.null(selected_codes) || length(selected_codes) == 0) {
        summary_df <- dplyr::filter(summary_df, .data[[demographic]] == "Overall")
      } else {
        selected_labels <- unname(labels_vec[names(labels_vec) %in% as.character(selected_codes)])
        to_keep <- c(selected_labels, if (show_overall) "Overall" else character(0))
        to_keep <- to_keep[!is.na(to_keep)]
        if (length(to_keep) > 0) {
          summary_df <- dplyr::filter(summary_df, .data[[demographic]] %in% to_keep)
        }
      }
    }

    if (stat_type == "percent") {
      summary_df <- summary_df %>%
        mutate(
          value = violence_percent,
          label = paste0(round(violence_percent, 1), "%"),
          denom_value = 100
        )
      y_lab <- "Percent Experiencing Violence"
      ylim_max <- 107
    } else {
      summary_df <- summary_df %>%
        mutate(
          value = violence_count,
          label = violence_count,
          denom_value = n_total
        )
      y_lab <- "Number Experiencing Violence"
      ylim_max <- max(summary_df$n_total, na.rm = TRUE) * 1.15
    }

    v_title <- if (violence_type == "physical") "Physical Violence" else if (violence_type == "sexual") "Sexual Violence" else if (violence_type == "ipv") "Intimate Partner Violence" else if (violence_type == "sexual_perp") "Sexual Violence Perpetration" else "Physical Violence Perpetration"

    fill_levels <- unique(as.character(summary_df[[demographic]]))
    n_other <- length(fill_levels) - sum(fill_levels == "Overall", na.rm = TRUE)
    fill_values <- c("Overall" = "darkgrey")
    if (n_other > 0) {
      other_levels <- fill_levels[fill_levels != "Overall"]
      fill_values <- c(fill_values, setNames(scales::hue_pal()(n_other), other_levels))
    }

    p <- ggplot(summary_df, aes(x = factor(data_year), fill = .data[[demographic]])) +
      geom_col(aes(y = denom_value), position = position_dodge(), alpha = 0.3, show.legend = FALSE) +
      geom_text(
        aes(y = denom_value, label = paste0(n_total)),
        vjust = -0.5, position = position_dodge(width = 0.9), size = 3, color = "gray40"
      ) +
      geom_col(aes(y = value), position = position_dodge()) +
      geom_text(aes(y = value, label = label), vjust = -0.5, position = position_dodge(width = 0.9)) +
      scale_fill_manual(values = fill_values) +
      labs(
        x = "Year",
        y = y_lab,
        fill = graph_labels[demographic],
        title = paste(v_title, "Experience by", graph_labels[demographic], "–", paste(sort(unique(df$data_year)), collapse = ", "))) +
      ylim(0, ylim_max) +
      theme_minimal()

    legend_grob <- get_legend_grob(p)
    p_no_legend <- p + theme(legend.position = "none")
    footnote_lines <- build_footnote_lines(violence_type)

    grid::grid.newpage()
    layout_mat <- grid::grid.layout(1, 2, widths = grid::unit(c(85, 15), "null"))
    grid::pushViewport(grid::viewport(layout = layout_mat))
    print(p_no_legend, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
    grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
    draw_legend_with_footnotes(legend_grob, footnote_lines)
    grid::popViewport()
    grid::popViewport()
  })

  # dynamic height for subcategory plot container
  output$subcategory_plots_ui <- renderUI({
    violence_type <- input$violence
    config <- subcategory_config[[violence_type]]
    n <- if (is.null(config)) 0L else length(config)
    ncol_val <- if (n <= 3) 1L else 3L
    nrow_val <- ceiling(n / ncol_val)
    # stacked (1 col): more height per row; grid (3 col): slightly less per row
    px_per_row <- if (ncol_val == 1L) 340L else 400L
    total_height <- nrow_val * px_per_row
    plotOutput("subcategory_plots", height = paste0(total_height, "px"))
  })

  # output: side-by-side subcategory plots (past year only, not IPV)
  output$subcategory_plots <- renderPlot({
    if (!isTRUE(input$show_subcategories) ||
        !identical(input$time_period, "past_year") ||
        input$violence == "ipv") {
      return(NULL)
    }
    df <- filtered_data()
    violence_type <- input$violence
    config <- subcategory_config[[violence_type]]
    if (is.null(config) || length(config) == 0) return(NULL)

    demographic <- input$demographic
    stat_type <- input$statistics
    show_overall <- is.null(input$overall) || isTRUE(input$overall)

    # Build one plot with legend to extract shared legend
    legend_plot <- make_one_plot(
      df,
      violence_col = config[[1]]$col,
      plot_title = config[[1]]$title,
      demographic = demographic,
      stat_type = stat_type,
      show_overall = show_overall,
      demographic_labels = demographic_labels,
      graph_labels = graph_labels,
      show_legend = TRUE
    )
    legend_grob <- get_legend_grob(legend_plot)

    plot_list <- list()
    for (i in seq_along(config)) {
      p <- make_one_plot(
        df,
        violence_col = config[[i]]$col,
        plot_title = config[[i]]$title,
        demographic = demographic,
        stat_type = stat_type,
        show_overall = show_overall,
        demographic_labels = demographic_labels,
        graph_labels = graph_labels,
        show_legend = FALSE
      )
      if (!is.null(p)) plot_list[[length(plot_list) + 1L]] <- p
    }
    if (length(plot_list) == 0) return(NULL)
    n <- length(plot_list)
    # <= 3 plots: stack vertically (1 column); > 3 plots: 3-column grid
    ncol <- if (n <= 3) 1L else 3L
    nrow <- ceiling(n / ncol)
    grid::grid.newpage()
    # Layout: left = plot grid (85%), right = single legend (15%) using null units for proper proportions
    layout_mat <- grid::grid.layout(1, 2, widths = grid::unit(c(85, 15), "null"))
    grid::pushViewport(grid::viewport(layout = layout_mat))
    # Left: grid of plots
    grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1,
      layout = grid::grid.layout(nrow, ncol)))
    for (i in seq_along(plot_list)) {
      row <- (i - 1L) %/% ncol + 1L
      col <- (i - 1L) %% ncol + 1L
      print(plot_list[[i]], vp = grid::viewport(layout.pos.row = row, layout.pos.col = col))
    }
    grid::popViewport()
    # Right: legend + footnotes
    grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
    draw_legend_with_footnotes(legend_grob, build_footnote_lines(violence_type))
    grid::popViewport()
    grid::popViewport()
  })

}

