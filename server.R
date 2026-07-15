library(shiny)
library(dplyr)
library(ggplot2)
library(grid)
library(httr)
library(jsonlite)
library(ggiraph)
 
# Register at source time (not per-session) so the logo resolves even on the
# very first page load, before any Shiny session has connected.
addResourcePath("images", file.path(getwd(), "images"))

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
  # California region labels (graph)
  region_labels <- c(
    "1" = "Bay Region",
    "2" = "Central Valley",
    "3" = "Mountain Valley",
    "4" = "Northern",
    "5" = "Southern"
  )
  # legend labels
  graph_labels <- c(
  "GENDER" = "Gender",
  "CA_REGION" = "California Region",
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
    CA_REGION = region_labels,
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

  # thousands-separated labels for raw-count values and axes (e.g. 2,083)
  comma_label <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

  # Non-Overall levels in UI / demographic_labels order (sidebar checkbox order).
  ordered_demographic_levels <- function(demographic, present_chars) {
    labs <- demographic_labels[[demographic]]
    others <- present_chars[present_chars != "Overall"]
    if (is.null(labs)) {
      return(sort(unique(others)))
    }
    intersect(unname(labs), unique(others))
  }
 
  # Full bar/legend order: Overall (leftmost bar) then sidebar demographic order.
  build_demographic_factor_levels <- function(demographic, present_chars, show_overall = TRUE) {
    present_chars <- as.character(present_chars)
    present_chars <- present_chars[!is.na(present_chars)]
    olv <- ordered_demographic_levels(demographic, present_chars)
    has_overall <- any(present_chars == "Overall", na.rm = TRUE)
    lv <- if (has_overall && show_overall) c("Overall", olv) else olv
    lv[vapply(lv, function(L) any(present_chars == L, na.rm = TRUE), logical(1L))]
  }
 
  apply_demographic_factor <- function(summary_df, demographic, levels_vec) {
    if (nrow(summary_df) == 0L || length(levels_vec) == 0L) {
      return(summary_df)
    }
    dch <- as.character(summary_df[[demographic]])
    summary_df[[demographic]] <- factor(dch, levels = levels_vec)
    summary_df
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
 
  # Map Overall + up to 6 demographic slots to a fixed pool (line charts).
  demographic_slot_values <- function(levels_present, pool) {
    lv <- levels_present[!is.na(levels_present)]
    out <- stats::setNames(vector(typeof(pool[[1L]]), length(lv)), lv)
    cat_idx <- 0L
    for (label in lv) {
      if (identical(label, "Overall")) {
        out[[label]] <- pool[[1L]]
      } else {
        cat_idx <- cat_idx + 1L
        idx <- min(cat_idx + 1L, length(pool))
        out[[label]] <- pool[[idx]]
      }
    }
    out
  }
 
  # Colour + linetype + shape for line charts (triple-encoded legend).
  demographic_line_aesthetics <- function(demographic, levels_present, fill_values = NULL) {
    if (is.null(fill_values)) {
      fill_values <- demographic_fill_values(demographic, levels_present)
    }
    list(
      color = fill_values,
      linetype = demographic_slot_values(
        levels_present,
        c("solid", "solid", "dashed", "dotdash", "longdash", "twodash", "dotted")
      ),
      shape = demographic_slot_values(
        levels_present,
        c(19, 16, 17, 15, 18, 8, 4)
      )
    )
  }
 
  add_line_demographic_scales <- function(p, demo_levels, demographic, graph_labels, fill_values = NULL) {
    aes_vals <- demographic_line_aesthetics(demographic, demo_levels, fill_values)
    leg_name <- graph_labels[demographic]
    p +
      scale_color_manual(
        values = aes_vals$color,
        breaks = demo_levels,
        limits = demo_levels,
        name = leg_name
      ) +
      scale_linetype_manual(
        values = aes_vals$linetype,
        breaks = demo_levels,
        limits = demo_levels,
        name = leg_name
      ) +
      scale_shape_manual(
        values = aes_vals$shape,
        breaks = demo_levels,
        limits = demo_levels,
        name = leg_name
      )
  }
 
  # ---------------------------------------------------------------------------
  # Responsive font-size helpers
  #
  # px_w     : actual container pixel width from session$clientData
  # ref_w    : "full-size" reference width (default 960 px)
  # min_scale: floor — text never shrinks below this fraction of max size
  # px_h/ref_h/min_scale_h : same idea, but for available height. The y-axis
  #   title is rotated 90°, so its rendered length runs the full height of
  #   the panel — when vertical space is tight (short window, browser zoom,
  #   or footnotes eating into the plot's height), it needs to shrink
  #   independently of the width-based scale so it's never clipped.
  #
  # Returns a named list of pt sizes to plug into theme() elements.
  # ---------------------------------------------------------------------------
  responsive_text_sizes <- function(px_w, ref_w = 960, min_scale = 0.55,
                                     px_h = NULL, ref_h = 420, min_scale_h = 0.45) {
    s <- if (!is.null(px_w) && !is.na(px_w) && px_w > 0) {
      max(min_scale, min(1, px_w / ref_w))
    } else 1

    s_h <- if (!is.null(px_h) && !is.na(px_h) && px_h > 0) {
      max(min_scale_h, min(1, px_h / ref_h))
    } else 1

    list(
      scale        = s,
      base         = 14 * s,
      axis_title   = 14 * s,
      axis_title_y = 14 * s * s_h,
      axis_text    = 12 * s,
      plot_title   = 18 * s,
      legend_title = 11 * s,
      legend_text  = 10 * s,
      caption      =  9 * s,
      # geom_text sizes (ggplot "mm" units, ~1/2.835 of pt)
      label_n      =  3 * s,        # N count above background bars
      label_val    =  3 * s,        # value / percentage label
      label_dense_n   = 2.5 * s,   # same but for dense demographics
      label_dense_val = 2   * s,
      # legend key physical size
      legend_key   = max(0.55, s) * 1   # in "lines" units
    )
  }
 
  # ggplot theme: bold title + muted subtitle, light horizontal-only gridlines,
  # legend inline at top-right (legend_pos = "top") or hidden ("none").
  # sz = output of responsive_text_sizes(); falls back to fixed defaults.
  plot_calvex_theme <- function(legend_pos = "top", sz = NULL) {
    if (is.null(sz)) {
      sz <- list(
        base = 14, axis_title = 14, axis_title_y = 14, axis_text = 12,
        plot_title = 18, legend_title = 11, legend_text = 10,
        caption = 9, legend_key = 1
      )
    }
    axis_title_y_size <- if (!is.null(sz$axis_title_y)) sz$axis_title_y else sz$axis_title
    th <- theme_minimal(base_size = sz$base, base_family = "Inter") +
      theme(
        axis.title         = element_text(size = sz$axis_title, face = "bold"),
        # Rotated 90° — its rendered length spans the full panel height, so
        # it gets its own (potentially smaller) size to avoid clipping when
        # vertical space is tight; overrides the general axis.title above.
        axis.title.y       = element_text(size = axis_title_y_size, face = "bold"),
        axis.text          = element_text(size = sz$axis_text),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#e8eaee", linewidth = 0.4),
        legend.title       = element_blank(),
        legend.text        = element_text(size = sz$legend_text),
        legend.key.size    = unit(sz$legend_key, "lines"),
        plot.title         = element_text(size = sz$plot_title, face = "bold"),
        plot.subtitle      = element_text(size = sz$axis_text, color = "gray45",
                                          margin = ggplot2::margin(t = 2, b = 10)),
        plot.caption       = element_text(size = sz$caption, color = "gray40"),
        legend.position    = legend_pos
      )
    if (identical(legend_pos, "top")) {
      th <- th + theme(
        legend.direction     = "horizontal",
        legend.justification = "right",
        legend.box.just      = "right",
        legend.margin        = ggplot2::margin(t = 0, b = 6)
      )
    }
    th
  }
 
  # Valid survey-year choices for the current violence type / time period
  # (IPV = 2023/2025 only; past-year perpetration was not asked in 2020)
  year_choices_for <- function(violence, time_period) {
    if (identical(violence, "ipv")) {
      c(2025, 2023)
    } else if (violence %in% c("sexual_perp", "physical_perp") &&
               identical(time_period, "past_year")) {
      c(2025, 2023, 2022, 2021)
    } else {
      c(2025, 2023, 2022, 2021, 2020)
    }
  }

  # Dynamic year selector: choices depend on violence type / time period
  observe({
    yrs <- year_choices_for(input$violence, input$time_period)
    updateCheckboxGroupInput(session, "YEAR",
      choices  = as.list(stats::setNames(yrs, yrs)),
      selected = yrs
    )
  })

  # All/None shortcut links in the sidebar filter groups (see calvex_filter_group in ui.R)
  bulk_choice_values <- list(
    GENDER = c(1, 2, 3), LGB_3 = c(1, 2, 3), AGE_6 = 1:6, RACE_5 = 1:5,
    INCOME_QUINTILE = 1:5, EDUC5 = 1:5, EMPLOY_2 = c(1, 2), DISABILITY = c(0, 1),
    CA_REGION = 1:5
  )
  observeEvent(input$calvex_bulk, {
    info <- input$calvex_bulk
    vals <- if (identical(info$id, "YEAR")) {
      year_choices_for(input$violence, input$time_period)
    } else {
      bulk_choice_values[[info$id]]
    }
    req(!is.null(vals))
    updateCheckboxGroupInput(
      session, info$id,
      selected = if (identical(info$action, "all")) vals else character(0)
    )
  })
 
  # Click logo / title to restore default sidebar and chart settings
  observeEvent(input$reset_to_defaults, {
    updateSelectInput(session, "time_period", selected = "past_year")
    updateSelectInput(session, "violence", selected = "physical")
    updateSelectInput(session, "demographic", selected = "GENDER")
    updateSelectInput(session, "chart_type", selected = "bar")
    updateSelectInput(session, "statistics", selected = "percent")
    updateCheckboxInput(session, "overall", value = TRUE)
    updateCheckboxInput(session, "show_subcategories", value = FALSE)
    updateCheckboxGroupInput(session, "GENDER", selected = c(1, 2, 3))
    updateCheckboxGroupInput(session, "LGB_3", selected = c(1, 2, 3))
    updateCheckboxGroupInput(session, "AGE_6", selected = 1:6)
    updateCheckboxGroupInput(session, "RACE_5", selected = 1:5)
    updateCheckboxGroupInput(session, "INCOME_QUINTILE", selected = 1:5)
    updateCheckboxGroupInput(session, "EDUC5", selected = 1:5)
    updateCheckboxGroupInput(session, "EMPLOY_2", selected = c(1, 2))
    updateCheckboxGroupInput(session, "DISABILITY", selected = c(0, 1))
    updateCheckboxGroupInput(session, "YEAR", selected = c(2025, 2023, 2022, 2021, 2020))
    updateCheckboxGroupInput(session, "CA_REGION", selected = 1:5)
    updateSliderInput(session, "scale_max_override", value = 40)
  }, ignoreInit = TRUE)

  # Demographic display selection: validation (Overall or category) or title-only mode.
  demographic_display_selection <- function(demographic, categories_only = FALSE) {
    categories_unselected <- is.null(input[[demographic]]) || length(input[[demographic]]) == 0L
    if (categories_only) return(categories_unselected)
    (is.null(input$overall) || isTRUE(input$overall)) || !categories_unselected
  }

  overall_or_demographic_validation_msg <- function(demographic) {
    sprintf(
      'Please select at least one of "Overall" or %s labels',
      graph_labels[demographic]
    )
  }

  # Experiences: "Past-Year Experiences of Physical Violence by Gender"
  # Perpetration: "Past-Year Sexual Violence Perpetrated by Gender"
  # No categories selected: group label becomes "all Californians"
  build_main_chart_title <- function(
    violence_type, time_period, demographic, graph_labels, only_overall = FALSE
  ) {
    tp_prefix <- if (identical(time_period, "lifetime")) "Lifetime" else "Past-Year"
    v_base <- switch(violence_type,
      physical      = "Physical Violence",
      sexual        = "Sexual Violence",
      ipv           = "Intimate Partner Violence",
      sexual_perp   = "Sexual Violence",
      physical_perp = "Physical Violence"
    )
    stem <- if (violence_type %in% c("sexual_perp", "physical_perp")) {
      paste(tp_prefix, v_base, "Perpetrated by")
    } else {
      paste(tp_prefix, "Experiences of", v_base, "by")
    }
    group_label <- if (isTRUE(only_overall)) "all Californians" else graph_labels[demographic]
    paste(stem, group_label)
  }

  # Muted subtitle under the title: statistic + displayed year range,
  # e.g. "Number experiencing violence \u00b7 2020\u20132025"
  build_chart_subtitle <- function(stat_type, violence_type, data_years) {
    verb <- if (violence_type %in% c("sexual_perp", "physical_perp")) "perpetrating" else "experiencing"
    stat_label <- if (identical(stat_type, "percent")) {
      paste("Percent", verb, "violence")
    } else {
      paste("Number", verb, "violence")
    }
    yrs <- sort(unique(data_years))
    if (length(yrs) == 0) return(stat_label)
    yr_label <- if (length(yrs) == 1) {
      as.character(yrs)
    } else {
      paste0(min(yrs), "\u2013", max(yrs))
    }
    paste0(stat_label, " \u00b7 ", yr_label)
  }

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
    df$CA_REGION <- factor(
      df$CA_REGION,
      levels = names(region_labels),
      labels = region_labels
    )
    df
  })
 
  # Build footnote lines based on current violence type
  build_footnote_lines <- function(violence_type, time_period = NULL, stat_type = NULL,
                                   demographic = NULL) {
    lines <- "* Hover over or click a bar to see the raw count, shown as the number experiencing violence out of the total number of people surveyed in that group"
    lines <- c(
      lines,
      "* We asked respondents about their experiences of violence across their lifetime (“ever”) and also about their experiences in the past year"
    )
    if (violence_type == "ipv") {
      lines <- c(lines, "* IPV is only available in 2023 and 2025")
    } else if (violence_type == "sexual_perp") {
      lines <- c(lines, "* Past year sexual violence perpetration was not asked in 2020")
    } else if (violence_type == "physical_perp") {
      lines <- c(lines, "* Past year physical violence perpetration was not asked in 2020")
    }
    if (identical(demographic, "CA_REGION")) {
      lines <- c(
        lines,
        "* Region comparisons include only years with region data; respondents with missing region are excluded"
      )
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
 
    demo_levels <- build_demographic_factor_levels(
      demographic, summary_df[[demographic]], show_overall
    )
    summary_df <- apply_demographic_factor(summary_df, demographic, demo_levels)
 
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
          label = comma_label(violence_count)
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
 
  # Main (non-subcategory) percent chart: past-year axis cap; 60 for sexual + gender/age.
  main_histogram_percent_limits <- function(time_period, violence_type, demographic) {
    if (!identical(time_period, "past_year")) {
      return(list(
        scale_max = 100,
        ylim_max = 107,
        breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
      ))
    }
    if (identical(violence_type, "sexual") && demographic %in% c("GENDER", "AGE_6")) {
      scale_max <- 60
    } else {
      scale_max <- 40
    }
    list(
      scale_max = scale_max,
      ylim_max = scale_max + 5,
      breaks = percent_axis_breaks(scale_max)
    )
  }
 
  # Sync slider to auto-computed default whenever the three inputs that drive
  # it change. bindEvent means dragging the slider does NOT retrigger this —
  # only switching violence/demographic/time_period resets to the new default.
  observe({
    req(identical(input$time_period, "past_year"), input$statistics == "percent")
    auto <- main_histogram_percent_limits(
      input$time_period, input$violence, input$demographic
    )$scale_max
    updateSliderInput(session, "scale_max_override", value = auto)
  }) |> bindEvent(input$violence, input$demographic, input$time_period, ignoreInit = FALSE)

  girafe_default_options <- function() {
    list(
      ggiraph::opts_sizing(rescale = TRUE, width = 1),
      ggiraph::opts_tooltip(
        css = paste0(
          "background:transparent;border:none;padding:0;",
          "box-shadow:none;font-family:Inter,sans-serif;"
        ),
        use_fill = FALSE,
        use_stroke = FALSE,
        delay_mouseout = 200
      ),
      ggiraph::opts_hover(css = "opacity:0.85;cursor:crosshair;"),
      # Clicked bars/points highlight blue (ggiraph's default selected style is red)
      ggiraph::opts_selection(
        type = "multiple",
        css = "fill:#4682B4;stroke:#4682B4;"
      ),
      # Keep the "download as png" button. Hide the lasso select/deselect
      # icons (unused — click-to-select still works via opts_selection
      # above) and the zoom/pan icons. Zoom is never configured (no
      # opts_zoom(max > 1) anywhere), and per ggiraph's own docs there is no
      # way to keep axis titles/labels pinned in view while panned/zoomed —
      # they can scroll out of the viewport and get clipped. Since we don't
      # use in-chart zoom, removing the tool entirely is the only way to
      # guarantee that never happens.
      ggiraph::opts_toolbar(
        saveaspng = TRUE,
        hidden = c("lasso_select", "lasso_deselect", "zoom")
      )
    )
  }
 
  # Build one chart (bar or line) for a given violence column (subcategory panel)
  make_one_plot <- function(df, violence_col, plot_title, demographic, stat_type,
                            show_overall, demographic_labels, graph_labels,
                            violence_type,
                            show_legend = TRUE, chart_type = "bar",
                            ylim_max = NULL, scale_max = NULL,
                            summary_df = NULL,
                            sz = NULL) {
    if (is.null(sz)) sz <- responsive_text_sizes(NULL)
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
 
    demo_levels <- build_demographic_factor_levels(
      demographic, summary_df[[demographic]], show_overall
    )
    summary_df <- apply_demographic_factor(summary_df, demographic, demo_levels)
    fill_values <- demographic_fill_values(demographic, demo_levels)
    x_breaks <- sort(unique(as.integer(summary_df$data_year)))
 
    dense_demo <- demographic %in% c("AGE_6", "RACE_5", "INCOME_QUINTILE", "EDUC5", "CA_REGION")
    label_size_val <- if (dense_demo) sz$label_dense_val else sz$label_val
 
    summary_df <- summary_df %>%
      mutate(
        .tooltip = mapply(
          make_tooltip_html,
          demo_label = as.character(.data[[demographic]]),
          year = .data$data_year,
          pct = .data$violence_percent,
          raw_count = .data$violence_count,
          n_total = .data$n_total,
          MoreArgs = list(violence_type = violence_type, v_label = plot_title),
          SIMPLIFY = TRUE
        ),
        .data_id = paste0(
          violence_col, "_",
          as.character(.data[[demographic]]), "_",
          .data$data_year
        )
      )
    summary_df <- apply_demographic_factor(summary_df, demographic, demo_levels)
    summary_df$.demo_fill <- factor(
      as.character(summary_df[[demographic]]),
      levels = demo_levels
    )
    summary_df <- summary_df[order(summary_df$data_year, summary_df$.demo_fill), ]
 
    subcat_legend_theme <- function() {
      if (!show_legend) {
        return(theme(legend.position = "none"))
      }
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.box.just = "left",
        legend.margin = ggplot2::margin(t = 8, b = 0),
        plot.margin = ggplot2::margin(8, 8, 20, 8)
      )
    }
 
    apply_percent_scale <- function(p) {
      if (!truncated_percent) {
        return(p + scale_y_continuous(limits = c(0, ylim_max), labels = comma_label, expand = ggplot2::expansion(mult = c(0, 0.02))))
      }
      breaks <- percent_axis_breaks(scale_max)
      p +
        scale_y_continuous(
          limits = c(0, ylim_max),
          breaks = breaks,
          expand = ggplot2::expansion(mult = c(0, 0))
        ) +
        coord_cartesian(clip = "off") +
        theme(plot.margin = ggplot2::margin(8, 8, 16, 8))
    }
 
    educ5_override <- if (identical(demographic, "EDUC5") && show_legend) {
      theme(
        legend.title    = element_text(size = sz$legend_title * 0.9),
        legend.text     = element_text(size = sz$legend_text  * 0.9),
        legend.key.size = unit(sz$legend_key * 0.75, "lines")
      )
    } else NULL
 
    if (identical(chart_type, "line")) {
      summary_df <- summary_df %>%
        mutate(data_year = factor(as.character(.data$data_year), levels = as.character(x_breaks))) %>%
        arrange(.data$data_year, .data$.demo_fill)
      p <- ggplot(summary_df, aes(
        x = .data$data_year,
        y = .data$value,
        color = .data$.demo_fill,
        linetype = .data$.demo_fill,
        shape = .data$.demo_fill,
        group = .data$.demo_fill
      ))
      if (truncated_percent) {
        p <- p + geom_hline(yintercept = scale_max, color = "gray80", linewidth = 0.35)
      }
      p <- p +
        geom_line(linewidth = 1.1) +
        ggiraph::geom_point_interactive(
          aes(tooltip = .data$.tooltip, data_id = .data$.data_id),
          size = 3
        ) +
        geom_text(aes(label = .data$label), vjust = -0.75, size = label_size_val, show.legend = FALSE)
      p <- add_line_demographic_scales(p, demo_levels, demographic, graph_labels, fill_values)
      p <- p +
        labs(x = "Year", y = y_lab, color = graph_labels[demographic], title = plot_title) +
        plot_calvex_theme(if (show_legend) "bottom" else "none", sz) +
        subcat_legend_theme()
      if (!is.null(educ5_override)) p <- p + educ5_override
      if (truncated_percent) {
        p <- apply_percent_scale(p)
      } else {
        p <- p + scale_y_continuous(limits = c(0, ylim_max), labels = comma_label, expand = ggplot2::expansion(mult = c(0, 0.02)))
      }
    } else {
      p <- ggplot(summary_df, aes(
        x = factor(.data$data_year),
        fill = .data$.demo_fill,
        group = .data$.demo_fill
      ))
      if (truncated_percent) {
        p <- p + geom_hline(yintercept = scale_max, color = "gray80", linewidth = 0.35)
      }
      p <- p +
        ggiraph::geom_col_interactive(
          aes(
            y = .data$value,
            tooltip = .data$.tooltip,
            data_id = .data$.data_id,
            group = .data$.demo_fill
          ),
          position = position_dodge(width = 0.85),
          width = 0.72
        ) +
        geom_text(
          aes(
            y = .data$value,
            label = .data$label,
            group = .data$.demo_fill
          ),
          vjust = -0.35,
          position = position_dodge(width = 0.85),
          size = label_size_val,
          show.legend = FALSE
        ) +
        scale_fill_manual(
          values = fill_values,
          breaks = demo_levels,
          limits = demo_levels,
          name = graph_labels[demographic]
        ) +
        labs(x = "Year", y = y_lab, fill = graph_labels[demographic], title = plot_title) +
        plot_calvex_theme(if (show_legend) "bottom" else "none", sz) +
        subcat_legend_theme()
      if (!is.null(educ5_override)) p <- p + educ5_override
      if (truncated_percent) {
        p <- apply_percent_scale(p)
      } else {
        p <- p + scale_y_continuous(limits = c(0, ylim_max), labels = comma_label, expand = ggplot2::expansion(mult = c(0, 0.02)))
      }
    }
 
    p
  }
 
  # Single faceted ggiraph plot: vertical stack, one legend below the last panel.
  build_subcategory_combined_plot <- function(
    config, summaries, demographic, stat_type, violence_type,
    show_overall, graph_labels, limits, chart_type,
    sz = NULL, show_legend = TRUE
  ) {
    if (is.null(sz)) sz <- responsive_text_sizes(NULL)
    chunks <- list()
    panel_levels <- character()
    for (i in seq_along(config)) {
      s <- summaries[[i]]
      if (is.null(s)) next
      s$panel_title <- config[[i]]$title
      s$subcat_col <- config[[i]]$col
      panel_levels <- c(panel_levels, config[[i]]$title)
      chunks[[length(chunks) + 1L]] <- s
    }
    if (length(chunks) == 0L) return(NULL)
 
    combined <- dplyr::bind_rows(chunks)
    combined$panel_title <- factor(combined$panel_title, levels = panel_levels)
 
    demo_levels <- build_demographic_factor_levels(
      demographic, combined[[demographic]], show_overall
    )
    combined <- apply_demographic_factor(combined, demographic, demo_levels)
    fill_values <- demographic_fill_values(demographic, demo_levels)
 
    dense_demo <- demographic %in% c("AGE_6", "RACE_5", "INCOME_QUINTILE", "EDUC5", "CA_REGION")
    label_size_val <- if (dense_demo) sz$label_dense_val else sz$label_val
 
    if (stat_type == "percent") {
      scale_max <- limits$scale_max
      ylim_max <- limits$ylim_max
      y_lab <- "Percent Experiencing Violence (%)"
      truncated_percent <- !is.null(scale_max) && scale_max < 100
      combined <- combined %>% mutate(denom_value = scale_max)
    } else {
      ylim_max <- limits$ylim_max
      y_lab <- "Number Experiencing Violence"
      truncated_percent <- FALSE
      combined <- combined %>% mutate(denom_value = n_total)
    }
 
    combined <- combined %>%
      mutate(
        .tooltip = mapply(
          make_tooltip_html,
          demo_label = as.character(.data[[demographic]]),
          year = .data$data_year,
          pct = .data$violence_percent,
          raw_count = .data$violence_count,
          n_total = .data$n_total,
          v_label = as.character(.data$panel_title),
          MoreArgs = list(violence_type = violence_type),
          SIMPLIFY = TRUE
        ),
        .data_id = paste0(
          .data$subcat_col, "_",
          as.character(.data[[demographic]]), "_",
          .data$data_year
        )
      )
    combined <- apply_demographic_factor(combined, demographic, demo_levels)
    combined$.demo_fill <- factor(
      as.character(combined[[demographic]]),
      levels = demo_levels
    )
    combined <- combined[order(combined$panel_title, combined$data_year, combined$.demo_fill), ]
 
    subcat_combined_theme <- function() {
      theme(
        legend.position  = if (show_legend) "top" else "none",
        strip.text       = element_text(face = "bold", size = sz$axis_text, hjust = 0),
        strip.placement  = "outside",
        panel.spacing.y  = grid::unit(1.1, "lines"),
        plot.margin      = ggplot2::margin(10, 12, 14, 10)
      )
    }
 
    if (identical(chart_type, "line")) {
      x_breaks <- sort(unique(as.integer(combined$data_year)))
      combined <- combined %>%
        mutate(data_year = factor(as.character(.data$data_year), levels = as.character(x_breaks)))
      p <- ggplot(combined, aes(
        x = .data$data_year,
        y = .data$value,
        color = .data$.demo_fill,
        linetype = .data$.demo_fill,
        shape = .data$.demo_fill,
        group = .data$.demo_fill
      ))
      if (truncated_percent) {
        p <- p + geom_hline(yintercept = scale_max, color = "gray80", linewidth = 0.35)
      }
      p <- p +
        geom_line(linewidth = 1.1) +
        ggiraph::geom_point_interactive(
          aes(tooltip = .data$.tooltip, data_id = .data$.data_id),
          size = 2.8
        ) +
        geom_text(aes(label = .data$label), vjust = -0.75, size = label_size_val, show.legend = FALSE) +
        facet_wrap(~panel_title, ncol = 1, scales = "fixed", strip.position = "top")
      p <- add_line_demographic_scales(p, demo_levels, demographic, graph_labels, fill_values)
      p <- p +
        labs(x = "Year", y = y_lab, color = graph_labels[demographic]) +
        plot_calvex_theme("top", sz) +
        subcat_combined_theme()
    } else {
      p <- ggplot(combined, aes(
        x = factor(.data$data_year),
        y = .data$value,
        fill = .data$.demo_fill,
        group = .data$.demo_fill
      ))
      if (truncated_percent) {
        p <- p + geom_hline(yintercept = scale_max, color = "gray80", linewidth = 0.35)
      }
      p <- p +
        ggiraph::geom_col_interactive(
          aes(tooltip = .data$.tooltip, data_id = .data$.data_id),
          position = position_dodge(width = 0.85),
          width = 0.72
        ) +
        geom_text(
          aes(label = .data$label, group = .data$.demo_fill),
          vjust = -0.35,
          position = position_dodge(width = 0.85),
          size = label_size_val,
          show.legend = FALSE
        ) +
        facet_wrap(~panel_title, ncol = 1, scales = "fixed", strip.position = "top") +
        scale_fill_manual(
          values = fill_values,
          breaks = demo_levels,
          limits = demo_levels,
          name = graph_labels[demographic]
        ) +
        labs(x = "Year", y = y_lab, fill = graph_labels[demographic]) +
        plot_calvex_theme("top", sz) +
        subcat_combined_theme()
    }
 
    if (identical(demographic, "EDUC5")) {
      p <- p + theme(
        legend.text     = element_text(size = sz$legend_text  * 0.9),
        legend.key.size = unit(sz$legend_key * 0.75, "lines")
      )
    }
 
    if (truncated_percent) {
      breaks <- percent_axis_breaks(scale_max)
      p <- p +
        scale_y_continuous(
          limits = c(0, ylim_max),
          breaks = breaks,
          expand = ggplot2::expansion(mult = c(0, 0))
        ) +
        coord_cartesian(clip = "off")
    } else {
      p <- p +
        scale_y_continuous(
          limits = c(0, ylim_max),
          labels = comma_label,
          expand = ggplot2::expansion(mult = c(0, 0.02))
        )
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
      lines <- build_footnote_lines(vt, "past_year", st, input$demographic)
    } else {
      lines <- build_footnote_lines(vt, tp, st, input$demographic)
    }
    note_html <- lapply(lines, function(ln) {
      tags$p(
        style = "margin: 0.25rem 0; font-size: 0.78rem; line-height: 1.45; color: #6b7280;",
        ln
      )
    })
    tags$div(
      style = "padding: 0.6rem 0.25rem 0.5rem; max-width: 100%; border-top: 1px solid #eef0f3;",
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
    lines <- build_footnote_lines(vt, "past_year", st, input$demographic)
    note_html <- lapply(lines, function(ln) {
      tags$p(
        style = "margin: 0.25rem 0; font-size: 0.78rem; line-height: 1.45; color: #6b7280;",
        ln
      )
    })
    tags$div(
      style = "padding: 0.6rem 0.25rem 0.5rem; max-width: 100%; border-top: 1px solid #eef0f3;",
      note_html
    )
  })
 
 
  # ---------------------------------------------------------------------------
  # Helper: build ggiraph tooltip HTML for one summary_df row
  # ---------------------------------------------------------------------------
  make_tooltip_html <- function(demo_label, year, pct, raw_count, n_total, violence_type, v_label) {
    verb <- if (violence_type %in% c("sexual_perp", "physical_perp")) "perpetrating" else "experiencing"
    interp <- paste0(demo_label, " ", verb, " ", v_label, " in ", year)
    paste0(
      "<div style='font-family:Inter,sans-serif;font-size:13px;line-height:1.6;",
      "background:#fff;border:1px solid #ccc;border-radius:6px;",
      "padding:8px 12px;box-shadow:2px 2px 6px rgba(0,0,0,.15);min-width:180px;'>",
      "<b>", interp, "</b><br/>",
      "<span style='color:#555;'>Year:</span> <b>", year, "</b><br/>",
      "<span style='color:#555;'>Percentage:</span> <b>", round(pct, 1), "%</b><br/>",
      "<span style='color:#555;'>Raw count:</span> <b>", raw_count, "/", n_total, "</b>",
      "</div>"
    )
  }
 
  # output: single plot (when not showing subcategories) — rendered via ggiraph
  output$histogram <- ggiraph::renderGirafe({
    if (isTRUE(input$show_subcategories) &&
        identical(input$time_period, "past_year") &&
        input$violence != "ipv") {
      return(ggiraph::girafe(ggobj = ggplot() + theme_void()))
    }
    shiny::validate(
      need(length(input$YEAR) > 0, "Please select at least one survey year."),
      need(length(input$CA_REGION) > 0, "Please select at least one California region."),
      need(
        demographic_display_selection(input$demographic),
        overall_or_demographic_validation_msg(input$demographic)
      )
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
 
    demo_levels <- build_demographic_factor_levels(
      demographic, summary_df[[demographic]], show_overall
    )
    summary_df <- apply_demographic_factor(summary_df, demographic, demo_levels)
 
    if (stat_type == "percent") {
      pct_limits <- main_histogram_percent_limits(time_period, violence_type, demographic)
      # Past-year: slider drives scale_max. Lifetime: always 100 (slider hidden).
      scale_max <- if (identical(time_period, "past_year") && !is.null(input$scale_max_override)) {
        as.integer(input$scale_max_override)
      } else {
        pct_limits$scale_max
      }
      ylim_max       <- scale_max + 5
      percent_breaks <- percent_axis_breaks(scale_max)
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
          label = comma_label(violence_count),
          denom_value = n_total
        )
      y_lab <- "Number Experiencing Violence"
      ylim_max <- max(summary_df$n_total, na.rm = TRUE) * 1.15
    }
 
    summary_df$data_year <- as.integer(summary_df$data_year)
    x_breaks <- sort(unique(summary_df$data_year))
 
    v_title <- if (violence_type == "physical") "Physical Violence" else if (violence_type == "sexual") "Sexual Violence" else if (violence_type == "ipv") "Intimate Partner Violence" else if (violence_type == "sexual_perp") "Sexual Violence Perpetration" else "Physical Violence Perpetration"

    pw <- session$clientData[["output_histogram_width"]]
    ph <- session$clientData[["output_histogram_height"]]
    only_overall <- demographic_display_selection(demographic, categories_only = TRUE)

    main_title <- build_main_chart_title(
      violence_type, time_period, demographic, graph_labels,
      only_overall = only_overall
    )
    sub_title <- build_chart_subtitle(stat_type, violence_type, df$data_year)

    fill_values <- demographic_fill_values(demographic, demo_levels)

    # Responsive font / label sizes based on actual container width/height
    sz <- responsive_text_sizes(pw, px_h = ph)
 
    # Convert container pixel dimensions to SVG inches (96 px/in).
    svg_w <- if (!is.null(pw) && !is.na(pw) && pw > 0) pw / 96 else 8
    svg_h <- if (!is.null(ph) && !is.na(ph) && ph > 0) ph / 96 else 5
    svg_w <- max(4, min(svg_w, 20))
    svg_h <- max(3, min(svg_h, 14))
 
    # Legend sits inline at top-right. No demographic categories selected ->
    # only Overall shows; a one-entry legend is redundant with the
    # "all Californians" title, so drop it.
    leg_pos_main <- if (only_overall) "none" else "top"
 
    dense_demo <- demographic %in% c("AGE_6", "RACE_5", "INCOME_QUINTILE", "EDUC5", "CA_REGION")
    label_size_val <- if (dense_demo) sz$label_dense_val else sz$label_val

    # Build per-row tooltip HTML and a unique data_id for ggiraph hit-testing
    summary_df <- summary_df %>%
      mutate(
        .tooltip = mapply(
          make_tooltip_html,
          demo_label  = as.character(.data[[demographic]]),
          year        = .data$data_year,
          pct         = .data$violence_percent,
          raw_count   = .data$violence_count,
          n_total     = .data$n_total,
          MoreArgs    = list(violence_type = violence_type, v_label = v_title),
          SIMPLIFY    = TRUE
        ),
        .data_id = paste0(as.character(.data[[demographic]]), "_", .data$data_year)
      )
    summary_df <- apply_demographic_factor(summary_df, demographic, demo_levels)
    # ggiraph geom_col_interactive + tooltip/data_id ignores fill level order unless
    # group matches fill (otherwise bars dodge in row order).
    summary_df$.demo_fill <- factor(
      as.character(summary_df[[demographic]]),
      levels = demo_levels
    )
    summary_df <- summary_df[order(summary_df$data_year, summary_df$.demo_fill), ]
 
    if (identical(chart_type, "line")) {
      summary_df <- summary_df %>%
        mutate(data_year = factor(as.character(.data$data_year), levels = as.character(x_breaks))) %>%
        arrange(.data$data_year, .data[[demographic]])
 
      p <- ggplot(summary_df, aes(
        x     = .data$data_year,
        y     = .data$value,
        color = .data$.demo_fill,
        linetype = .data$.demo_fill,
        shape = .data$.demo_fill,
        group = .data$.demo_fill
      ))
      if (stat_type == "percent" && identical(time_period, "past_year")) {
        p <- p + geom_hline(yintercept = scale_max, color = "gray80", linewidth = 0.35)
      }
      p <- p +
        geom_line(linewidth = 1.15) +
        # Interactive points — tooltip only appears on the plotted dot, not along the line
        ggiraph::geom_point_interactive(
          aes(tooltip = .data$.tooltip, data_id = .data$.data_id),
          size = 3.2
        ) +
        geom_text(aes(label = .data$label), vjust = -0.8, size = 3, show.legend = FALSE)
      p <- add_line_demographic_scales(p, demo_levels, demographic, graph_labels, fill_values)
      p <- p +
        labs(x = "Year", y = y_lab, color = graph_labels[demographic],
             title = main_title, subtitle = sub_title) +
        plot_calvex_theme(leg_pos_main, sz)

      if (stat_type == "percent" && identical(time_period, "past_year")) {
        p <- p +
          scale_y_continuous(
            limits = c(0, ylim_max),
            breaks = percent_breaks,
            expand = ggplot2::expansion(mult = c(0, 0))
          ) +
          coord_cartesian(clip = "off") +
          theme(plot.margin = ggplot2::margin(8, 8, 16, 8))
      } else if (stat_type == "percent") {
        p <- p +
          scale_y_continuous(
            limits = c(0, ylim_max),
            breaks = percent_breaks,
            expand = ggplot2::expansion(mult = c(0, 0))
          )
      } else {
        p <- p +
          scale_y_continuous(limits = c(0, ylim_max), labels = comma_label, expand = ggplot2::expansion(mult = c(0, 0.02)))
      }

      return(ggiraph::girafe(
        ggobj = p,
        width_svg  = svg_w,
        height_svg = svg_h,
        options = girafe_default_options()
      ))
    }
 
    # ---- Bar chart ----
    p <- ggplot(summary_df, aes(
      x = factor(.data$data_year),
      fill = .data$.demo_fill,
      group = .data$.demo_fill
    ))
    if (stat_type == "percent" && identical(time_period, "past_year")) {
      p <- p + geom_hline(yintercept = scale_max, color = "gray80", linewidth = 0.35)
    }
    p <- p +
      # Value bars — interactive: tooltip + hover highlight
      # (denominator n is shown in the tooltip as raw_count/n_total)
      ggiraph::geom_col_interactive(
        aes(
          y        = .data$value,
          tooltip  = .data$.tooltip,
          data_id  = .data$.data_id,
          group    = .data$.demo_fill
        ),
        position = position_dodge(width = 0.85),
        width    = 0.72
      ) +
      geom_text(
        aes(
          y = .data$value,
          label = .data$label,
          group = .data$.demo_fill
        ),
        vjust = -0.35,
        position = position_dodge(width = 0.85),
        size = label_size_val,
        show.legend = FALSE
      ) +
      scale_fill_manual(
        values = fill_values,
        breaks = demo_levels,
        limits = demo_levels,
        name = graph_labels[demographic]
      ) +
      labs(x = "Year", y = y_lab, fill = graph_labels[demographic],
           title = main_title, subtitle = sub_title) +
      plot_calvex_theme(leg_pos_main, sz)
 
    if (identical(demographic, "EDUC5")) {
      p <- p + theme(
        legend.text     = element_text(size = sz$legend_text  * 0.9),
        legend.key.size = unit(sz$legend_key * 0.75, "lines")
      )
    }
 
    if (stat_type == "percent" && identical(time_period, "past_year")) {
      p <- p +
        scale_y_continuous(
          limits = c(0, ylim_max),
          breaks = percent_breaks,
          expand = ggplot2::expansion(mult = c(0, 0))
        ) +
        coord_cartesian(clip = "off") +
        theme(plot.margin = ggplot2::margin(8, 8, 16, 8))
    } else if (stat_type == "percent") {
      p <- p +
        scale_y_continuous(
          limits = c(0, ylim_max),
          breaks = percent_breaks,
          expand = ggplot2::expansion(mult = c(0, 0))
        )
    } else {
      p <- p +
        scale_y_continuous(limits = c(0, ylim_max), labels = comma_label, expand = ggplot2::expansion(mult = c(0, 0.02)))
    }
 
    ggiraph::girafe(
      ggobj = p,
      width_svg  = svg_w,
      height_svg = svg_h,
      options = girafe_default_options()
    )
  })
 
  # Subcategory panel container (single faceted ggiraph plot)
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
      div(
        class = "calvex-plot-inner",
        ggiraph::girafeOutput("subcategory_plots", width = "100%", height = "100%")
      )
    )
  })
 
  # output: stacked subcategory facets (past year only, not IPV) — ggiraph
  output$subcategory_plots <- ggiraph::renderGirafe({
    req(isTRUE(input$show_subcategories),
        identical(input$time_period, "past_year"),
        input$violence != "ipv")
    shiny::validate(
      need(length(input$YEAR) > 0, "Please select at least one survey year."),
      need(length(input$CA_REGION) > 0, "Please select at least one California region."),
      need(
        demographic_display_selection(input$demographic),
        overall_or_demographic_validation_msg(input$demographic)
      )
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

    # Apply slider override to all subcategory panels (percent mode only).
    if (identical(stat_type, "percent") && !is.null(input$scale_max_override)) {
      sm <- as.integer(input$scale_max_override)
      limits$scale_max <- sm
      limits$ylim_max  <- sm + max(5, round(sm * 0.1))
      limits$truncated <- sm < 100
    }
 
    n_panels <- sum(!vapply(summaries, is.null, logical(1L)))
    pw_sub <- session$clientData[["output_subcategory_plots_width"]]
    ph_sub <- session$clientData[["output_subcategory_plots_height"]]
 
    p <- build_subcategory_combined_plot(
      config = config,
      summaries = summaries,
      demographic = demographic,
      stat_type = stat_type,
      violence_type = violence_type,
      show_overall = show_overall,
      graph_labels = graph_labels,
      limits = limits,
      chart_type = chart_type,
      sz = responsive_text_sizes(pw_sub, px_h = ph_sub),
      show_legend = !demographic_display_selection(demographic, categories_only = TRUE)
    )
    req(!is.null(p))
 
    svg_w <- if (!is.null(pw_sub) && !is.na(pw_sub) && pw_sub > 0) pw_sub / 96 else 8
    svg_h <- if (!is.null(ph_sub) && !is.na(ph_sub) && ph_sub > 0) {
      ph_sub / 96
    } else {
      3.5 + n_panels * 1.6
    }
    svg_w <- max(5, min(svg_w, 20))
    svg_h <- max(3.5 + n_panels * 1.4, min(svg_h, 28))
 
    ggiraph::girafe(
      ggobj = p,
      width_svg = svg_w,
      height_svg = svg_h,
      options = girafe_default_options()
    )
  })
 
}