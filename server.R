library(shiny)
library(dplyr)
library(ggplot2)
library(grid)
library(httr)
library(jsonlite)

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Data loading via Supabase Data API (PostgREST)
  # PostgREST caps each response at 1000 rows, so we paginate in batches
  # until the returned page is shorter than the batch size.
  # Requires two env vars (set in .Renviron or the host environment):
  #   SUPABASE_URL — Project URL, e.g. https://<project-ref>.supabase.co
  #   SUPABASE_KEY — service_role secret key  (NEVER commit this to git)
  # ---------------------------------------------------------------------------
  .sb_base  <- paste0(Sys.getenv("SUPABASE_URL"), "/rest/v1/calvex_data")
  .sb_hdrs  <- httr::add_headers(
    apikey        = Sys.getenv("SUPABASE_KEY"),
    Authorization = paste("Bearer", Sys.getenv("SUPABASE_KEY"))
  )
  .batch_size <- 1000L
  .offset     <- 0L
  .pages      <- list()
  repeat {
    resp <- httr::GET(
      paste0(.sb_base, "?select=*&limit=", .batch_size, "&offset=", .offset),
      .sb_hdrs
    )
    httr::stop_for_status(resp)
    page <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
    .pages[[length(.pages) + 1L]] <- page
    if (nrow(page) < .batch_size) break
    .offset <- .offset + .batch_size
  }
  calvex_data <- dplyr::bind_rows(.pages)

  # Normalize "NA" strings (CSV→Supabase roundtrip artifact) to real R NA
  # across all character columns so filters and factor conversions work cleanly.
  calvex_data <- dplyr::mutate(calvex_data, dplyr::across(
    where(is.character),
    ~ dplyr::na_if(.x, "NA")
  ))

  # lookup tables / labels for graph & legend
  gender_labels <- c(
    "1" = "Female",
    "2" = "Male",
    "3" = "Gender non-conforming"
  )
  # sexuality labels (graph)
  sexuality_labels <- c(
    "1" = "Lesbian / Gay",
    "2" = "Straight",
    "3" = "Bisexual / other identity"
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
    "4" = "Master's Degree",
    "5" = "Post-Graduate/Professional Degree"
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
    "1" = "White, Non-Hispanic",
    "2" = "Black, Non-Hispanic",
    "3" = "Asian, Non-Hispanic",
    "4" = "Hispanic",
    "5" = "Other/multiple races, Non-Hispanic"
  )
  # legend labels
  graph_labels <- c(
  "GENDER" = "Gender",
  "AGE_6" = "Age",
  "RACE_5" = "Race/Ethnicity",
  "LGB_3" = "Sexuality",
  "INCOME_QUINTILE" = "Income Quintile",
  "EDUC5" = "Education Level",
  "EMPLOY_2" = "Employment Status",
  "DISABILITY" = "Disability Status"
  )

  # map demographic variable name -> code-to-label vector (for filtering which bars to show)
  demographic_labels <- list(
    GENDER = gender_labels,
    AGE_6 = age_labels,
    RACE_5 = race_labels,
    LGB_3 = sexuality_labels,
    INCOME_QUINTILE = income_labels,
    EDUC5 = education_labels,
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

  # ggplot theme: larger axis text, legends, titles
  plot_calvex_theme <- function(legend_pos = "right") {
    theme_minimal(base_size = 14) +
      theme(
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"),
        plot.caption = element_text(size = 10, color = "gray40"),
        legend.position = legend_pos
      )
  }

  # Dynamic year selector: choices depend on violence type (IPV = 2023/2025 only)
  observe({
    vt <- input$violence
    tp <- input$time_period
    if (identical(vt, "ipv")) {
      updateCheckboxGroupInput(session, "YEAR",
        choices  = list("2025" = 2025, "2023" = 2023),
        selected = c(2025, 2023)
      )
    } else if (vt %in% c("sexual_perp", "physical_perp") && identical(tp, "past_year")) {
      updateCheckboxGroupInput(session, "YEAR",
        choices  = list("2025" = 2025, "2023" = 2023, "2022" = 2022, "2021" = 2021),
        selected = c(2025, 2023, 2022, 2021)
      )
    } else {
      updateCheckboxGroupInput(session, "YEAR",
        choices  = list("2025" = 2025, "2023" = 2023, "2022" = 2022, "2021" = 2021, "2020" = 2020),
        selected = c(2025, 2023, 2022, 2021, 2020)
      )
    }
  })

  filtered_data <- reactive({
    df <- calvex_data
    time_period <- input$time_period

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
    if (!is.null(input$EDUC5) && length(input$EDUC5) > 0)
      df <- df[df$EDUC5 %in% suppressWarnings(as.numeric(input$EDUC5)), ]
    if (!is.null(input$EMPLOY_2) && length(input$EMPLOY_2) > 0)
      df <- df[df$EMPLOY_2 %in% suppressWarnings(as.numeric(input$EMPLOY_2)), ]
    if (!is.null(input$DISABILITY) && length(input$DISABILITY) > 0)
      df <- df[df$DISABILITY %in% suppressWarnings(as.numeric(input$DISABILITY)), ]

    # filter by year
    if (!is.null(input$YEAR) && length(input$YEAR) > 0)
      df <- df[df$data_year %in% suppressWarnings(as.numeric(input$YEAR)), ]

    # filter by California region (numeric codes 1–5)
    if (!is.null(input$CA_REGION) && length(input$CA_REGION) > 0) {
      df <- df[!is.na(df$CA_REGION) &
        df$CA_REGION %in% suppressWarnings(as.numeric(input$CA_REGION)), ]
    }

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
    df$EDUC5 <- factor(
      df$EDUC5,
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

  # Build footnote lines based on current violence type
  build_footnote_lines <- function(violence_type, time_period = NULL, stat_type = NULL) {
    lines <- "* Background bars represent the total\n  number of people surveyed in that\n  demographic"
    if (violence_type == "ipv") {
      lines <- c(lines, "* IPV is only available in 2023 and 2025")
    } else if (violence_type == "sexual_perp") {
      lines <- c(lines, "* Past year sexual violence perpetration was not asked in 2020")
    } else if (violence_type == "physical_perp") {
      lines <- c(lines, "* Past year physical violence perpetration was not asked in 2020")
    }
    if (!is.null(time_period) && !is.null(stat_type) &&
        identical(time_period, "past_year") && identical(stat_type, "percent")) {
      lines <- c(
        lines,
        "* Past-year subcategory percent axes are scaled per chart to its highest value; percentages are out of 100%"
      )
    }
    lines
  }

  # Summarise one subcategory column (shared by limit computation and plotting)
  prepare_subcategory_summary <- function(df, violence_col, demographic, stat_type,
                                        show_overall, demographic_labels) {
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

    if (stat_type == "percent") {
      summary_df <- summary_df %>%
        mutate(
          value = violence_percent,
          label = paste0(round(violence_percent, 1), "%")
        )
    } else {
      summary_df <- summary_df %>%
        mutate(
          value = violence_count,
          label = as.character(violence_count)
        )
    }
    summary_df
  }

  # Per-panel y-axis from the tallest bar in that subcategory chart
  # compute_panel_subcategory_limits <- function(panel_max, stat_type) {
  #   if (identical(stat_type, "percent")) {
  #     if (is.na(panel_max) || !is.finite(panel_max) || panel_max <= 0) {
  #       return(list(scale_max = 20, ylim_max = 23, truncated = TRUE))
  #     }
  #     if (panel_max <= 50) {
  #       scale_max <- max(10, ceiling(panel_max / 5) * 5)
  #     } else {
  #       scale_max <- min(100, ceiling(panel_max / 10) * 10)
  #     }
  #     ylim_max <- min(107, scale_max + max(5, round(scale_max * 0.1)))
  #     list(scale_max = scale_max, ylim_max = ylim_max, truncated = scale_max < 100)
  #   } else {
  #     if (is.na(panel_max) || !is.finite(panel_max) || panel_max <= 0) {
  #       return(list(scale_max = NULL, ylim_max = 1, truncated = FALSE))
  #     }
  #     list(scale_max = NULL, ylim_max = max(1, panel_max * 1.15), truncated = FALSE)
  #   }
  # }

  # Uniform y-axis across all subcategory panels (highest bar in any chart).
  # Uncomment this and the UNIFORM block in output$subcategory_plots to restore.
  compute_shared_subcategory_limits <- function(global_max, stat_type) {
    if (identical(stat_type, "percent")) {
      if (is.na(global_max) || !is.finite(global_max) || global_max <= 0) {
        return(list(scale_max = 20, ylim_max = 23, truncated = TRUE))
      }
      if (global_max <= 50) {
        scale_max <- max(10, ceiling(global_max / 5) * 5)
      } else {
        scale_max <- min(100, ceiling(global_max / 10) * 10)
      }
      ylim_max <- min(107, scale_max + max(5, round(scale_max * 0.1)))
      list(scale_max = scale_max, ylim_max = ylim_max, truncated = scale_max < 100)
    } else {
      if (is.na(global_max) || !is.finite(global_max) || global_max <= 0) {
        return(list(scale_max = NULL, ylim_max = 1, truncated = FALSE))
      }
      list(scale_max = NULL, ylim_max = max(1, global_max * 1.15), truncated = FALSE)
    }
  }

  percent_axis_breaks <- function(scale_max) {
    step <- if (scale_max <= 25) 5 else if (scale_max <= 50) 10 else 20
    seq(0, scale_max, by = step)
  }

  # Build one chart (bar or line) for a given violence column (subcategory panel)
  make_one_plot <- function(df, violence_col, plot_title, demographic, stat_type,
                            show_overall, demographic_labels, graph_labels,
                            show_legend = TRUE, chart_type = "bar",
                            ylim_max = NULL, scale_max = NULL,
                            summary_df = NULL) {
    if (is.null(summary_df)) {
      summary_df <- prepare_subcategory_summary(
        df, violence_col, demographic, stat_type, show_overall, demographic_labels
      )
    }
    if (is.null(summary_df) || nrow(summary_df) == 0) return(NULL)

    if (stat_type == "percent") {
      denom_scale <- if (!is.null(scale_max)) scale_max else 50
      summary_df <- summary_df %>%
        mutate(denom_value = denom_scale)
      y_lab <- "Percent Experiencing Violence (%)"
      if (is.null(ylim_max)) ylim_max <- denom_scale + 3
      truncated_percent <- !is.null(scale_max) && scale_max < 100
    } else {
      summary_df <- summary_df %>%
        mutate(denom_value = n_total)
      y_lab <- "Number Experiencing Violence"
      if (is.null(ylim_max)) {
        ylim_max <- max(summary_df$value, na.rm = TRUE) * 1.15
        if (!is.finite(ylim_max) || ylim_max <= 0) ylim_max <- 1
      }
      truncated_percent <- FALSE
    }

    fill_levels <- levels(summary_df[[demographic]])
    fill_values <- demographic_fill_values(demographic, fill_levels)
    x_breaks <- sort(unique(as.integer(summary_df$data_year)))

    apply_percent_scale <- function(p, is_bar = FALSE) {
      if (!truncated_percent) {
        return(p + scale_y_continuous(limits = c(0, ylim_max), expand = ggplot2::expansion(mult = c(0, 0.02))))
      }
      denom_scale <- scale_max
      breaks <- percent_axis_breaks(denom_scale)
      annotate_x <- mean(seq_along(x_breaks))
      p <- p +
        {if (is_bar) geom_hline(yintercept = denom_scale, color = "gray80", linewidth = 0.35) else NULL} +
        annotate(
          "text",
          x = annotate_x,
          y = denom_scale + ylim_max * 0.04,
          label = "\u2192 100%",
          size = 3.5,
          color = "gray35",
          fontface = "italic"
        ) +
        scale_y_continuous(
          limits = c(0, ylim_max),
          breaks = breaks,
          expand = ggplot2::expansion(mult = c(0, 0))
        ) +
        coord_cartesian(clip = "off") +
        theme(plot.margin = ggplot2::margin(8, 8, 16, 8))
      p
    }

    if (identical(chart_type, "line")) {
      summary_df <- summary_df %>%
        mutate(data_year = factor(as.character(.data$data_year), levels = as.character(x_breaks))) %>%
        arrange(.data$data_year, .data[[demographic]])
      p <- ggplot(summary_df, aes(
        x = .data$data_year,
        y = .data$value,
        color = .data[[demographic]],
        group = .data[[demographic]]
      )) +
        geom_line(linewidth = 1.1) +
        geom_point(size = 3) +
        geom_text(aes(label = .data$label), vjust = -0.75, size = 4, show.legend = FALSE) +
        scale_color_manual(values = fill_values, name = graph_labels[demographic]) +
        labs(x = "Year", y = y_lab, color = graph_labels[demographic], title = plot_title) +
        plot_calvex_theme(if (show_legend) "bottom" else "none") +
        theme(legend.direction = "horizontal")
      if (truncated_percent) {
        p <- apply_percent_scale(p, is_bar = FALSE)
      } else {
        p <- p + scale_y_continuous(limits = c(0, ylim_max), expand = ggplot2::expansion(mult = c(0, 0.02)))
      }
    } else {
      p <- ggplot(summary_df, aes(x = factor(.data$data_year), fill = .data[[demographic]])) +
        # geom_col(
        #   aes(y = .data$denom_value),
        #   position = position_dodge(width = 0.85),
        #   alpha = 0.3,
        #   width = 0.72,
        #   show.legend = FALSE
        # ) +
        # geom_text(
        #   aes(y = .data$denom_value, label = paste0(.data$n_total)),
        #   vjust = -0.35,
        #   position = position_dodge(width = 0.85),
        #   size = 4,
        #   color = "gray40",
        #   show.legend = FALSE
        # ) +
        geom_col(aes(y = .data$value), position = position_dodge(width = 0.85), width = 0.72) +
        geom_text(
          aes(y = .data$value, label = .data$label),
          vjust = -0.35,
          position = position_dodge(width = 0.85),
          size = 4,
          show.legend = FALSE
        ) +
        scale_fill_manual(values = fill_values, name = graph_labels[demographic]) +
        labs(x = "Year", y = y_lab, fill = graph_labels[demographic], title = plot_title) +
        plot_calvex_theme()
      if (truncated_percent) {
        p <- apply_percent_scale(p, is_bar = TRUE)
      } else {
        p <- p + scale_y_continuous(limits = c(0, ylim_max), expand = ggplot2::expansion(mult = c(0, 0.02)))
      }
    }

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

  # Footnotes below main plot (ggplot renders its own legend)
  output$footnotes_html <- renderUI({
    vt <- input$violence
    tp <- input$time_period
    st <- input$statistics
    if (isTRUE(input$show_subcategories) &&
        identical(tp, "past_year") &&
        !identical(vt, "ipv")) {
      lines <- build_footnote_lines(vt, "past_year", st)
    } else {
      lines <- build_footnote_lines(vt, tp, st)
    }
    note_html <- lapply(lines, function(ln) tags$p(style = "margin: 0.35rem 0; font-size: 0.9rem; color: #555;", ln))
    tags$div(
      style = "padding: 0.5rem 0.25rem 0.75rem; max-width: 100%;",
      note_html
    )
  })

  # Footnotes below subcategory grid (separate output id so both panels can exist)
  output$footnotes_html_sub <- renderUI({
    req(
      isTRUE(input$show_subcategories),
      identical(input$time_period, "past_year"),
      !identical(input$violence, "ipv")
    )
    vt <- input$violence
    st <- input$statistics
    lines <- build_footnote_lines(vt, "past_year", st)
    note_html <- lapply(lines, function(ln) tags$p(style = "margin: 0.35rem 0; font-size: 0.9rem; color: #555;", ln))
    tags$div(
      style = "padding: 0.5rem 0.25rem 0.75rem; max-width: 100%;",
      note_html
    )
  })


  # output: single plot (when not showing subcategories)
  output$histogram <- renderPlot({
    if (isTRUE(input$show_subcategories) &&
        identical(input$time_period, "past_year") &&
        input$violence != "ipv") {
      return(invisible(NULL))
    }
    shiny::validate(
      need(length(input$YEAR) > 0, "Please select at least one survey year."),
      need(length(input$CA_REGION) > 0, "Please select at least one California region.")
    )
    df <- filtered_data()
    demographic <- input$demographic
    stat_type <- input$statistics
    violence_type <- input$violence
    time_period <- input$time_period
    chart_type <- input$chart_type

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

    if (stat_type == "percent") {
      scale_max  <- if (time_period == "past_year") 60 else 100
      ylim_max   <- if (time_period == "past_year") 70 else 107
      summary_df <- summary_df %>%
        mutate(
          value = violence_percent,
          label = paste0(round(violence_percent, 1), "%"),
          denom_value = scale_max
        )
      y_lab <- "Percent Experiencing Violence (%)"
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

    summary_df$data_year <- as.integer(summary_df$data_year)
    x_breaks <- sort(unique(summary_df$data_year))

    v_title <- if (violence_type == "physical") "Physical Violence" else if (violence_type == "sexual") "Sexual Violence" else if (violence_type == "ipv") "Intimate Partner Violence" else if (violence_type == "sexual_perp") "Sexual Violence Perpetration" else "Physical Violence Perpetration"
    main_title <- paste(v_title, "Experience by", graph_labels[demographic], "–", paste(sort(unique(df$data_year)), collapse = ", "))

    fill_levels <- levels(summary_df[[demographic]])
    fill_values <- demographic_fill_values(demographic, fill_levels)

    pw <- session$clientData[["output_histogram_width"]]
    narrow <- !is.null(pw) && !is.na(pw) && pw < 700
    leg_pos_main <- if (narrow) "bottom" else "right"

    if (identical(chart_type, "line")) {
      summary_df <- summary_df %>%
        mutate(data_year = factor(as.character(.data$data_year), levels = as.character(x_breaks))) %>%
        arrange(.data$data_year, .data[[demographic]])
      p <- ggplot(summary_df, aes(
        x = .data$data_year,
        y = .data$value,
        color = .data[[demographic]],
        group = .data[[demographic]]
      )) +
        geom_line(linewidth = 1.15) +
        geom_point(size = 3.2) +
        geom_text(aes(label = .data$label), vjust = -0.8, size = 4.2, show.legend = FALSE) +
        scale_color_manual(values = fill_values, name = graph_labels[demographic]) +
        labs(x = "Year", y = y_lab, color = graph_labels[demographic], title = main_title) +
        plot_calvex_theme(leg_pos_main) +
        theme(legend.direction = if (narrow) "horizontal" else "vertical")

      if (stat_type == "percent" && identical(time_period, "past_year")) {
        p <- p + geom_hline(yintercept = 60, color = "gray80", linewidth = 0.35) +
          annotate(
            "text",
            x = mean(seq_along(x_breaks)),
            y = 62.5,
            label = "\u2192 100%",
            size = 3.8,
            color = "gray35",
            fontface = "italic"
          ) +
          scale_y_continuous(
            limits = c(0, ylim_max),
            breaks = c(0, 20, 40, 60),
            expand = ggplot2::expansion(mult = c(0, 0))
          ) +
          coord_cartesian(clip = "off") +
          theme(plot.margin = ggplot2::margin(8, 8, 16, 8))
      } else if (stat_type == "percent") {
        p <- p +
          scale_y_continuous(
            limits = c(0, ylim_max),
            breaks = c(0, 20, 40, 60, 80, 100),
            expand = ggplot2::expansion(mult = c(0, 0))
          )
      } else {
        p <- p +
          scale_y_continuous(limits = c(0, ylim_max), expand = ggplot2::expansion(mult = c(0, 0.02)))
      }
      print(p)
      return(invisible(NULL))
    }

    p <- ggplot(summary_df, aes(x = factor(.data$data_year), fill = .data[[demographic]]))
    if (stat_type == "percent" && identical(time_period, "past_year")) {
      p <- p + geom_hline(yintercept = 60, color = "gray80", linewidth = 0.35)
    }
    p <- p +
      geom_col(
        aes(y = .data$denom_value),
        position = position_dodge(width = 0.85),
        alpha = 0.3,
        width = 0.72,
        show.legend = FALSE
      ) +
      geom_text(
        aes(y = .data$denom_value, label = paste0(.data$n_total)),
        vjust = -0.35,
        position = position_dodge(width = 0.85),
        size = 4,
        color = "gray40",
        show.legend = FALSE
      ) +
      geom_col(aes(y = .data$value), position = position_dodge(width = 0.85), width = 0.72) +
      geom_text(
        aes(y = .data$value, label = .data$label),
        vjust = -0.35,
        position = position_dodge(width = 0.85),
        size = 4,
        show.legend = FALSE
      ) +
      scale_fill_manual(values = fill_values, name = graph_labels[demographic]) +
      labs(x = "Year", y = y_lab, fill = graph_labels[demographic], title = main_title) +
      plot_calvex_theme(leg_pos_main) +
      theme(legend.direction = if (narrow) "horizontal" else "vertical")

    if (stat_type == "percent" && identical(time_period, "past_year")) {
      p <- p +
        annotate(
          "text",
          x = mean(seq_along(x_breaks)),
          y = 62.5,
          label = "\u2192 100%",
          size = 3.8,
          color = "gray35",
          fontface = "italic"
        ) +
        scale_y_continuous(
          limits = c(0, ylim_max),
          breaks = c(0, 20, 40, 60),
          expand = ggplot2::expansion(mult = c(0, 0))
        ) +
        coord_cartesian(clip = "off") +
        theme(plot.margin = ggplot2::margin(8, 8, 16, 8))
    } else if (stat_type == "percent") {
      p <- p +
        scale_y_continuous(
          limits = c(0, ylim_max),
          breaks = c(0, 20, 40, 60, 80, 100),
          expand = ggplot2::expansion(mult = c(0, 0))
        )
    } else {
      p <- p +
        scale_y_continuous(limits = c(0, ylim_max), expand = ggplot2::expansion(mult = c(0, 0.02)))
    }

    print(p)
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
    shiny::validate(
      need(length(input$YEAR) > 0, "Please select at least one survey year."),
      need(length(input$CA_REGION) > 0, "Please select at least one California region.")
    )
    df <- filtered_data()
    violence_type <- input$violence
    config <- subcategory_config[[violence_type]]
    req(!is.null(config), length(config) > 0)

    demographic <- input$demographic
    stat_type <- input$statistics
    show_overall <- is.null(input$overall) || isTRUE(input$overall)

    chart_type <- input$chart_type

    summaries <- lapply(config, function(item) {
      prepare_subcategory_summary(
        df, item$col, demographic, stat_type, show_overall, demographic_labels
      )
    })
    valid_summaries <- summaries[!vapply(summaries, is.null, logical(1L))]
    req(length(valid_summaries) > 0)

    # UNIFORM: one y-axis for all panels from the highest bar in any subcategory chart.
    global_max <- max(
      vapply(valid_summaries, function(s) max(s$value, na.rm = TRUE), numeric(1)),
      na.rm = TRUE
    )
    limits <- compute_shared_subcategory_limits(global_max, stat_type)

    plot_list <- list()
    plot_idx <- 0L
    for (i in seq_along(config)) {
      s <- summaries[[i]]
      if (is.null(s)) next
      plot_idx <- plot_idx + 1L
      # PER-PANEL: scale each chart to its own tallest bar
      # panel_max <- max(s$value, na.rm = TRUE)
      # limits <- compute_panel_subcategory_limits(panel_max, stat_type)
      # UNIFORM: use shared limits instead — ylim_max = limits$ylim_max, scale_max = limits$scale_max
      p <- make_one_plot(
        df,
        violence_col = config[[i]]$col,
        plot_title = config[[i]]$title,
        demographic = demographic,
        stat_type = stat_type,
        show_overall = show_overall,
        demographic_labels = demographic_labels,
        graph_labels = graph_labels,
        show_legend = identical(plot_idx, 1L),
        chart_type = chart_type,
        ylim_max = limits$ylim_max,
        scale_max = limits$scale_max,
        summary_df = s
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
      grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow, ncol)))
      for (i in seq_along(plot_list)) {
        row <- (i - 1L) %/% ncol + 1L
        col <- (i - 1L) %% ncol + 1L
        print(plot_list[[i]], vp = grid::viewport(layout.pos.row = row, layout.pos.col = col))
      }
      grid::popViewport()
    }, error = function(e) {
      # Suppress transient device-size errors; Shiny will re-render once the
      # container has settled at its correct dimensions.
      NULL
    })
  })

}
