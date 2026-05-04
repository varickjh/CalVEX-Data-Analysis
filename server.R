library(shiny)
library(dplyr)
library(ggplot2)
library(grid)

server <- function(input, output, session) {
  
  # data organization
  calvex2025 <- read.csv("data/CalVEX2025.csv")
  calvex2023 <- read.csv("data/CalVEX2023.csv")
  calvex2022 <- read.csv("data/CalVEX2022.csv")
  calvex2021 <- read.csv("data/CalVEX2021.csv")
  calvex2020 <- read.csv("data/CalVEX2020.csv")

  # normalize year-specific IPV columns into shared schema
  if ("IPV22_EVER" %in% names(calvex2023)) {
    names(calvex2023)[names(calvex2023) == "IPV22_EVER"] <- "ipv_ever"
  }
  if ("IPV22_YEAR" %in% names(calvex2023)) {
    names(calvex2023)[names(calvex2023) == "IPV22_YEAR"] <- "ipv_year"
  }
  if ("IPV25_EVER" %in% names(calvex2025)) {
    names(calvex2025)[names(calvex2025) == "IPV25_EVER"] <- "ipv_ever"
  }
  if ("IPV25_YEAR" %in% names(calvex2025)) {
    names(calvex2025)[names(calvex2025) == "IPV25_YEAR"] <- "ipv_year"
  }

  # normalize 2025-specific column names into app schema
  if (!("data_year" %in% names(calvex2025))) {
    calvex2025$data_year <- 2025
  }
  if ("WEIGHT_CA" %in% names(calvex2025)) {
    names(calvex2025)[names(calvex2025) == "WEIGHT_CA"] <- "WEIGHT"
  }
  if (!("INCOME_QUINTILE" %in% names(calvex2025)) && "INCOME_QUINTILE1" %in% names(calvex2025)) {
    names(calvex2025)[names(calvex2025) == "INCOME_QUINTILE1"] <- "INCOME_QUINTILE"
  }
  if (!("EDUC_4" %in% names(calvex2025)) && "EDUC5" %in% names(calvex2025)) {
    educ5_num <- suppressWarnings(as.numeric(calvex2025$EDUC5))
    # Collapse 5-category education coding into the app's 4-category scheme.
    # 5th category is merged into 4 (highest education).
    calvex2025$EDUC_4 <- dplyr::case_when(
      is.na(educ5_num) ~ NA_real_,
      educ5_num >= 5 ~ 4,
      TRUE ~ educ5_num
    )
  }
  if ("DISABILITY" %in% names(calvex2025)) {
    disability_num <- suppressWarnings(as.numeric(calvex2025$DISABILITY))
    # 2025 coding uses 1/2/98; app expects 1 = Has Disability, 0 = No Disability.
    calvex2025$DISABILITY <- dplyr::case_when(
      is.na(disability_num) ~ NA_real_,
      disability_num == 2 ~ 0,
      disability_num == 1 ~ 1,
      TRUE ~ disability_num
    )
  }

  # SV, PV, SV Perp, PV Perp (no standardization needed)
  
  # Rename gender columns to standard GENDER name
  if ("GENDER_NEW" %in% names(calvex2025)) {
    names(calvex2025)[names(calvex2025) == "GENDER_NEW"] <- "GENDER"
  }
  if ("GENDER_NEW" %in% names(calvex2023)) {
    names(calvex2023)[names(calvex2023) == "GENDER_NEW"] <- "GENDER"
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
    "ipv_year",
    "data_year",
    "INCOME_QUINTILE",
    "EDUC_4",
    "EMPLOY_2",
    "DISABILITY",
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
    if (!col %in% names(calvex2025)) calvex2025[[col]] <- NA
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
  calvex2025 <- calvex2025[, cols_needed]
  
  calvex_data <- rbind(
    calvex2025,
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
    "3" = "Gender non-conforming",
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

  # map demographic variable name -> code-to-label vector (for filtering which bars to show)
  demographic_labels <- list(
    GENDER = gender_labels,
    AGE_6 = age_labels,
    RACE_5 = race_labels,
    LGB_3 = sexuality_labels,
    INCOME_QUINTILE = income_labels,
    EDUC_4 = education_labels,
    EMPLOY_2 = employment_labels,
    DISABILITY = disability_labels
  )

  # fixed purple palette (stable order; mapped to canonical demographic order)
  purple_fill_palette <- c("#CEA9EA", "#B08FD4", "#8E6FB0", "#6b558e", "#4A3D66", "#3d2d52")

  # ordered non-Overall levels: definition order in demographic_labels, intersected with present values
  ordered_demographic_levels <- function(demographic, present_chars) {
    labs <- demographic_labels[[demographic]]
    others <- present_chars[present_chars != "Overall"]
    if (is.null(labs)) {
      return(sort(unique(others)))
    }
    intersect(unname(labs), unique(others))
  }

  # named fill colors for Overall + categories (same label -> same color across violence types)
  demographic_fill_values <- function(demographic, levels_present, overall_color = "#5c5c5c") {
    lv <- levels_present[!is.na(levels_present)]
    others <- lv[lv != "Overall"]
    if (length(others) == 0L) {
      return(c("Overall" = overall_color)[lv %in% "Overall"])
    }
    cols <- purple_fill_palette[seq_along(others)]
    names(cols) <- others
    out <- c(if ("Overall" %in% lv) c("Overall" = overall_color), cols)
    idx <- lv[lv %in% names(out)]
    out[idx]
  }

    # reactive subset
  filtered_data <- reactive({
    df <- calvex_data
    time_period <- if (!is.null(input$time_period)) input$time_period else "lifetime"

    # time-aware filtering based on which violence variables actually exist
    if (!is.null(input$violence)) {
      # IPV is only available in 2023 and 2025
      if (input$violence == "ipv") {
        df <- df[df$data_year %in% c(2023, 2025), ]
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

  # Extract legend grob from a ggplot (for shared legend in subcategory grid).
  # ggplot2 >= 3.5 renamed "guide-box" to "guide-box-right" / "guide-box-inside"
  # etc., so match on the prefix rather than an exact string.
  get_legend_grob <- function(a_plot) {
    if (!inherits(a_plot, "ggplot")) return(NULL)
    tmp  <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a_plot))
    nms  <- sapply(tmp$grobs, function(x) x$name)
    leg  <- which(grepl("^guide-box", nms))
    if (length(leg) > 0) tmp$grobs[[leg[1]]] else NULL
  }

  # Build footnote lines based on current violence type
  build_footnote_lines <- function(violence_type, time_period = NULL, stat_type = NULL) {
    lines <- "* Background bars represent the total\n  number of people surveyed in that\n  demographic"
    if (violence_type == "ipv") {
      lines <- c(lines, "* IPV is only available in 2023 and 2025")
    } else if (violence_type == "sexual_perp") {
      lines <- c(lines, "* sv_perp_12mo was not asked in 2020")
    } else if (violence_type == "physical_perp") {
      lines <- c(lines, "* pv_perp_12mo was not asked in 2020")
    }
    if (!is.null(time_period) && !is.null(stat_type) &&
        identical(time_period, "past_year") && identical(stat_type, "percent")) {
      lines <- c(
        lines,
        "* Past-year percent axis is truncated at 60% for readability; percentages are out of 100%"
      )
    }
    lines
  }

  # Draw legend grob + footnotes stacked vertically inside the current viewport.
  # clip = "on" prevents a wide legend grob from bleeding into the plot area.
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
      grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1, clip = "on"))
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
        other_levels <- ordered_demographic_levels(demographic, demo_vals)
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

    if (nrow(summary_df) > 0) {
      dch <- as.character(summary_df[[demographic]])
      olv <- ordered_demographic_levels(demographic, dch)
      final_levels <- if (any(dch == "Overall", na.rm = TRUE)) c("Overall", olv) else olv
      final_levels <- final_levels[vapply(final_levels, function(L) any(dch == L, na.rm = TRUE), logical(1L))]
      summary_df[[demographic]] <- factor(dch, levels = final_levels)
    }

    # subcategory plots are always past-year, so cap percent scale at 50%
    if (stat_type == "percent") {
      summary_df <- summary_df %>%
        mutate(
          value = violence_percent,
          label = paste0(round(violence_percent, 1), "%"),
          denom_value = 50
        )
      y_lab <- "Percent Experiencing Violence"
      ylim_max <- 53
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

    fill_levels <- levels(summary_df[[demographic]])
    fill_values <- demographic_fill_values(demographic, fill_levels)

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
      if (time_period == "lifetime") "ipv_ever" else "ipv_year"
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
        other_levels <- ordered_demographic_levels(demographic, demo_vals)
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

    if (nrow(summary_df) > 0) {
      dch <- as.character(summary_df[[demographic]])
      olv <- ordered_demographic_levels(demographic, dch)
      final_levels <- if (any(dch == "Overall", na.rm = TRUE)) c("Overall", olv) else olv
      final_levels <- final_levels[vapply(final_levels, function(L) any(dch == L, na.rm = TRUE), logical(1L))]
      summary_df[[demographic]] <- factor(dch, levels = final_levels)
    }

    # message("\n--- Percent Calculation: numerator / denominator ---")
    # print(as.data.frame(summary_df[, c("data_year", demographic,
    #   "n_total", "violence_count",
    #   "violence_count_weighted", "total_surveyed_in_demographic", "violence_percent")]))

    if (stat_type == "percent") {
      scale_max  <- if (time_period == "past_year") 60 else 100
      ylim_max   <- if (time_period == "past_year") 70 else 107
      summary_df <- summary_df %>%
        mutate(
          value = violence_percent,
          label = paste0(round(violence_percent, 1), "%"),
          denom_value = scale_max
        )
      y_lab <- "Percent Experiencing Violence"
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

    fill_levels <- levels(summary_df[[demographic]])
    fill_values <- demographic_fill_values(demographic, fill_levels)

    p <- ggplot(summary_df, aes(x = factor(data_year), fill = .data[[demographic]]))
    if (stat_type == "percent" && identical(time_period, "past_year")) {
      p <- p + geom_hline(yintercept = 60, color = "gray80", linewidth = 0.35)
    }
    p <- p +
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
      theme_minimal()

    if (stat_type == "percent" && identical(time_period, "past_year")) {
      ny <- length(unique(summary_df$data_year))
      x_annot <- if (ny >= 1L) mean(seq_len(ny)) else 1
      p <- p +
        annotate(
          "text",
          x = x_annot,
          y = 62.5,
          label = "\u2192 100%",
          size = 3.2,
          color = "gray35",
          fontface = "italic"
        ) +
        scale_y_continuous(
          limits = c(0, ylim_max),
          breaks = c(0, 20, 40, 60),
          expand = ggplot2::expansion(mult = c(0, 0))
        ) +
        coord_cartesian(clip = "off") +
        theme(plot.margin = ggplot2::margin(6, 6, 14, 6))
    } else if (stat_type == "percent") {
      p <- p +
        scale_y_continuous(
          limits = c(0, ylim_max),
          breaks = c(0, 20, 40, 60, 80, 100),
          expand = ggplot2::expansion(mult = c(0, 0))
        )
    } else {
      p <- p +
        scale_y_continuous(
          limits = c(0, ylim_max),
          expand = ggplot2::expansion(mult = c(0, 0.02))
        )
    }

    legend_grob <- get_legend_grob(p)
    p_no_legend <- p + theme(legend.position = "none")
    footnote_lines <- build_footnote_lines(violence_type, time_period, stat_type)

    pw <- session$clientData[["output_histogram_width"]]
    narrow <- !is.null(pw) && !is.na(pw) && pw < 700

    grid::grid.newpage()
    if (narrow) {
      layout_mat <- grid::grid.layout(2, 1, heights = grid::unit(c(72, 28), "null"))
      grid::pushViewport(grid::viewport(layout = layout_mat))
      print(p_no_legend, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
      grid::pushViewport(grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
      draw_legend_with_footnotes(legend_grob, footnote_lines)
      grid::popViewport()
      grid::popViewport()
    } else {
      layout_mat <- grid::grid.layout(1, 2, widths = grid::unit(c(80, 20), "null"))
      grid::pushViewport(grid::viewport(layout = layout_mat))
      print(p_no_legend, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
      grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
      draw_legend_with_footnotes(legend_grob, footnote_lines)
      grid::popViewport()
      grid::popViewport()
    }
  })

  # dynamic height for subcategory plot container
  # req() ensures this only creates the plotOutput while the panel is actually
  # visible, preventing a 0px container that would cause device-size errors.
  output$subcategory_plots_ui <- renderUI({
    req(isTRUE(input$show_subcategories),
        identical(input$time_period, "past_year"),
        input$violence != "ipv")
    violence_type <- input$violence
    config <- subcategory_config[[violence_type]]
    n <- if (is.null(config)) 0L else length(config)
    req(n > 0)
    div(
      class = "calvex-plot-wrap",
      plotOutput("subcategory_plots", height = "100%")
    )
  })

  # output: side-by-side subcategory plots (past year only, not IPV)
  output$subcategory_plots <- renderPlot({
    req(isTRUE(input$show_subcategories),
        identical(input$time_period, "past_year"),
        input$violence != "ipv")
    df <- filtered_data()
    violence_type <- input$violence
    config <- subcategory_config[[violence_type]]
    req(!is.null(config), length(config) > 0)

    demographic <- input$demographic
    stat_type <- input$statistics
    show_overall <- is.null(input$overall) || isTRUE(input$overall)

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
    req(length(plot_list) > 0)
    n <- length(plot_list)
    pw_sub <- session$clientData[["output_subcategory_plots_width"]]
    narrow_sub <- !is.null(pw_sub) && !is.na(pw_sub) && pw_sub < 700
    ncol <- if (narrow_sub) 1L else if (n <= 3L) 1L else 3L
    nrow <- ceiling(n / ncol)

    # tryCatch guards against the brief moment when the container is resized
    # during a violence-type switch and the device dimensions are invalid.
    tryCatch({
      grid::grid.newpage()
      if (narrow_sub) {
        layout_mat <- grid::grid.layout(2, 1, heights = grid::unit(c(72, 28), "null"))
        grid::pushViewport(grid::viewport(layout = layout_mat))
        grid::pushViewport(grid::viewport(
          layout.pos.row = 1, layout.pos.col = 1,
          layout = grid::grid.layout(nrow, ncol)
        ))
        for (i in seq_along(plot_list)) {
          row <- (i - 1L) %/% ncol + 1L
          col <- (i - 1L) %% ncol + 1L
          print(plot_list[[i]], vp = grid::viewport(layout.pos.row = row, layout.pos.col = col))
        }
        grid::popViewport()
        grid::pushViewport(grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
        draw_legend_with_footnotes(legend_grob, build_footnote_lines(violence_type, "past_year", stat_type))
        grid::popViewport()
        grid::popViewport()
      } else {
        layout_mat <- grid::grid.layout(1, 2, widths = grid::unit(c(80, 20), "null"))
        grid::pushViewport(grid::viewport(layout = layout_mat))
        grid::pushViewport(grid::viewport(
          layout.pos.row = 1, layout.pos.col = 1,
          layout = grid::grid.layout(nrow, ncol)
        ))
        for (i in seq_along(plot_list)) {
          row <- (i - 1L) %/% ncol + 1L
          col <- (i - 1L) %% ncol + 1L
          print(plot_list[[i]], vp = grid::viewport(layout.pos.row = row, layout.pos.col = col))
        }
        grid::popViewport()
        grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
        draw_legend_with_footnotes(legend_grob, build_footnote_lines(violence_type, "past_year", stat_type))
        grid::popViewport()
        grid::popViewport()
      }
    }, error = function(e) {
      # Suppress transient device-size errors; Shiny will re-render once the
      # container has settled at its correct dimensions.
      NULL
    })
  })

}

