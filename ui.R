library(shiny)
library(bslib)
library(markdown) # for shinyapps.io deployment
library(ggiraph)

# Checkbox filter group with All/None bulk-select shortcuts in its header row.
# Clicks are handled server-side via the 'calvex_bulk' input (see server.R).
calvex_filter_group <- function(inputId, label, choices, selected, help = NULL) {
  bulk_link <- function(action, text) {
    tags$a(
      href = "#",
      onclick = sprintf(
        "Shiny.setInputValue('calvex_bulk', {id: '%s', action: '%s', nonce: Math.random()}); return false;",
        inputId, action
      ),
      text
    )
  }
  div(
    class = "calvex-filter-group",
    div(
      class = "calvex-filter-head",
      tags$span(class = "calvex-filter-label", label),
      div(class = "calvex-filter-links", bulk_link("all", "All"), bulk_link("none", "None"))
    ),
    checkboxGroupInput(inputId, NULL, choices = choices, selected = selected),
    if (!is.null(help)) helpText(help)
  )
}

ui <- page_fillable(
  theme = bslib::bs_theme(
    primary = "#8E6FB0",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter")
  ),
  padding = 0,
  gap = 0,
  fillable_mobile = TRUE,
  # Keep the ggiraph plot sized to its *actual* available space at all times.
  # Shiny/ggiraph only remeasure container size on a window 'resize' event —
  # they don't notice purely content-driven reflows (e.g. the footnotes below
  # the plot rendering after the plot's first paint, or wrapping to an extra
  # line at narrow/zoomed widths). Without this, the plot keeps the taller
  # pre-footnote size baked in, overflows past its own box, and overlaps the
  # footnotes text. A ResizeObserver on the plot container(s) catches every
  # such reflow (not just the first one) and re-triggers a 'resize' so the
  # plot always re-renders at the correct, current size.
  tags$head(
    tags$link(rel = "preload", href = "images/logo-black.png", as = "image"),
    tags$script(HTML(
      "$(document).one('shiny:idle', function() {
        if (typeof ResizeObserver === 'undefined') {
          setTimeout(function() { $(window).trigger('resize'); }, 10);
          return;
        }
        var resizeTimer = null;
        var lastSizes = new WeakMap();
        function scheduleResize() {
          clearTimeout(resizeTimer);
          resizeTimer = setTimeout(function() { $(window).trigger('resize'); }, 60);
        }
        var observer = new ResizeObserver(function(entries) {
          entries.forEach(function(entry) {
            var w = Math.round(entry.contentRect.width);
            var h = Math.round(entry.contentRect.height);
            var prev = lastSizes.get(entry.target);
            if (!prev || prev.w !== w || prev.h !== h) {
              lastSizes.set(entry.target, { w: w, h: h });
              scheduleResize();
            }
          });
        });
        function observeAll(root) {
          if (!root.querySelectorAll) return;
          root.querySelectorAll('.calvex-plot-inner').forEach(function(el) {
            observer.observe(el);
          });
        }
        observeAll(document);
        // The subcategory grid's '.calvex-plot-inner' is only inserted into
        // the DOM later (via renderUI, when the user toggles it on), so also
        // watch for newly-added plot containers and observe those too.
        new MutationObserver(function(mutations) {
          mutations.forEach(function(m) {
            m.addedNodes.forEach(function(node) {
              if (node.nodeType !== 1) return;
              if (node.classList && node.classList.contains('calvex-plot-inner')) {
                observer.observe(node);
              }
              observeAll(node);
            });
          });
        }).observe(document.body, { childList: true, subtree: true });
        scheduleResize();
      });"
    )),
    tags$style(HTML("
      :root {
        --calvex-purple-light: #c8b8e2;
        --calvex-purple-mid: #CEA9EA;
        --calvex-purple-hover: #B08FD4;
        --calvex-purple-active: #8E6FB0;
        --calvex-purple-deep: #6b558e;
        --calvex-purple-border: rgba(107, 85, 142, 0.35);
        --calvex-border: #e5e7eb;
        --calvex-muted: #6b7280;
        --calvex-font: Inter, sans-serif;
      }
      body { font-family: var(--calvex-font); }
      input[type='checkbox'] { accent-color: var(--calvex-purple-active); }
      .form-check-input:checked {
        background-color: var(--calvex-purple-active);
        border-color: var(--calvex-purple-active);
      }
      /* ionRangeSlider (Y-axis max) in deep purple so it reads against the
         purple top bar */
      .irs--shiny .irs-bar,
      .irs--shiny .irs-single,
      .irs--shiny .irs-handle {
        background-color: var(--calvex-purple-deep);
        border-color: var(--calvex-purple-deep);
      }
      .irs--shiny .irs-handle { box-shadow: none; }
      .irs--shiny .irs-line { background: rgba(255, 255, 255, 0.55); }

      /* ---- header (logo + reset link) ---- */
      .calvex-header {
        background-color: var(--calvex-purple-light);
        border-bottom: 1px solid var(--calvex-purple-border);
        padding: 0.4rem 1.1rem;
        flex-shrink: 0;
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
        height: 3.575rem;
        width: auto;
        display: block;
      }

      /* ---- top control bar (primary controls, separate from filters) ---- */
      .calvex-topbar {
        display: flex;
        flex-wrap: wrap;
        align-items: flex-end;
        gap: 0.8rem 1.35rem;
        padding: 0.8rem 1.25rem;
        background: var(--calvex-purple-light);
        border-bottom: 1px solid var(--calvex-purple-border);
        flex-shrink: 0;
      }
      .calvex-topbar .form-group,
      .calvex-topbar .shiny-input-container {
        margin-bottom: 0;
      }
      .calvex-topbar label.control-label {
        text-transform: uppercase;
        font-size: 0.7rem;
        letter-spacing: 0.07em;
        color: #000;
        font-weight: 700;
        margin-bottom: 0.4rem;
      }
      .calvex-control-divided {
        border-left: 1px solid var(--calvex-purple-border);
        padding-left: 1.35rem;
      }
      /* ---- sidebar: display option checkboxes ---- */
      .calvex-sidebar-checks {
        padding: 0.4rem 0 0.5rem;
      }
      .calvex-sidebar-checks .shiny-input-container {
        margin-bottom: 0.15rem;
      }

      /* ---- sidebar: filters ---- */
      .bslib-sidebar-layout {
        border: none !important;
        border-radius: 0 !important;
      }
      .calvex-sidebar,
      .bslib-sidebar-layout > .sidebar.calvex-sidebar {
        background-color: var(--calvex-purple-light) !important;
        border-right: 1px solid var(--calvex-purple-border);
        /* single scroll container; translateZ fixes iOS Safari scroll freeze
           after accordion DOM changes */
        overflow-y: auto !important;
        -webkit-overflow-scrolling: touch;
        overscroll-behavior-y: contain;
        position: relative;
        transform: translateZ(0);
      }
      .calvex-sidebar .sidebar-content {
        height: auto !important;
        max-height: none !important;
        overflow-y: visible !important;
        gap: 0.5rem;
      }
      .calvex-sidebar .sidebar-title {
        text-transform: uppercase;
        font-size: 0.74rem;
        letter-spacing: 0.09em;
        color: #4A3D66;
        font-weight: 700;
        margin-bottom: 0.25rem;
      }
      .calvex-sidebar .accordion {
        --bs-accordion-border-radius: 0;
        --bs-accordion-inner-border-radius: 0;
        border-radius: 0.375rem;
        overflow: hidden;
        border: 1px solid var(--calvex-purple-border);
        background-color: var(--calvex-purple-light);
      }
      .calvex-sidebar .accordion-item {
        border: none;
        border-bottom: 1px solid var(--calvex-purple-border);
        border-radius: 0;
        background-color: var(--calvex-purple-light);
      }
      .calvex-sidebar .accordion-item:last-child { border-bottom: none; }
      .calvex-sidebar .accordion-button {
        background-color: var(--calvex-purple-light) !important;
        color: #000 !important;
        font-weight: 700;
        font-size: 0.75rem;
        text-transform: uppercase;
        letter-spacing: 0.07em;
        padding: 0.7rem 0.9rem;
        box-shadow: none !important;
      }
      .calvex-sidebar .accordion-button:hover { background-color: #DDD0EF !important; }
      .calvex-sidebar .accordion-button:not(.collapsed) {
        background-color: var(--calvex-purple-mid) !important;
      }
      .calvex-sidebar .accordion-button:not(.collapsed):hover {
        background-color: var(--calvex-purple-hover) !important;
      }
      .calvex-sidebar .accordion-body {
        background-color: var(--calvex-purple-light);
        color: #000;
        padding: 0.25rem 0.9rem 0.9rem;
      }

      /* filter groups: label row with All/None links, tight checkbox rhythm */
      .calvex-filter-head {
        display: flex;
        justify-content: space-between;
        align-items: baseline;
        margin: 1rem 0 0.35rem;
      }
      .calvex-filter-group:first-child .calvex-filter-head { margin-top: 0.6rem; }
      .calvex-filter-label {
        font-weight: 600;
        font-size: 0.92rem;
      }
      .calvex-filter-links a {
        font-size: 0.76rem;
        font-weight: 600;
        color: var(--calvex-purple-deep);
        text-decoration: none;
        margin-left: 0.6rem;
      }
      .calvex-filter-links a:hover { text-decoration: underline; }
      .calvex-filter-group .form-group { margin-bottom: 0; }
      .calvex-filter-group .shiny-options-group {
        display: flex;
        flex-direction: column;
        gap: 0.35rem;
      }
      .calvex-filter-group .checkbox,
      .calvex-filter-group .form-check {
        margin: 0;
        min-height: 0;
      }
      .calvex-filter-group .checkbox label,
      .calvex-filter-group .form-check-label {
        font-size: 0.9rem;
        line-height: 1.45;
      }
      .calvex-filter-group .help-block {
        display: block;
        font-size: 0.78rem;
        color: #3f3a4d;
        margin-top: 0.45rem;
      }
      .calvex-notes-list {
        font-size: 0.85rem;
        padding-left: 1.1rem;
        color: #222;
      }

      /* ---- main area: plain white, chart sits directly on it ---- */
      .bslib-sidebar-layout > .main {
        background: #fff;
        overflow-y: auto;
      }
      .calvex-plot-wrap {
        background: #fff;
        border: none;
        padding: 8px 10px 4px;
        min-height: 360px;
        height: calc(100dvh - 255px);
        height: calc(100vh - 255px);
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

      /* long selected values ellipsize instead of overflowing their control */
      .calvex-topbar .selectize-input { white-space: nowrap; overflow: hidden; }
      .calvex-topbar .selectize-input > .item {
        max-width: 100%;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }

      /* mobile-only collapse toggle for the top bar */
      .calvex-topbar-toggle { display: none; }
      .calvex-topbar-toggle-btn {
        width: 100%;
        display: flex;
        align-items: center;
        justify-content: space-between;
        background: var(--calvex-purple-light);
        border: none;
        border-bottom: 1px solid var(--calvex-purple-border);
        padding: 0.55rem 1rem;
        font-family: var(--calvex-font);
        font-weight: 600;
        font-size: 0.9rem;
        color: #000;
      }
      .calvex-toggle-caret { transition: transform 0.15s ease; }
      .calvex-topbar-toggle-btn.open .calvex-toggle-caret { transform: rotate(180deg); }

      /* ---- desktop: single-line top bar (controls shrink, never wrap) ----
         Note: conditionalPanel wrappers render display:contents, so their
         inner .shiny-input-container is the actual flex item. */
      @media (min-width: 1200px) {
        /* one uniform gap between every control; divider gets the same space
           on both sides so rhythm stays even across the whole bar */
        .calvex-topbar { flex-wrap: nowrap; gap: 1rem; padding: 0.7rem 1rem; }
        .calvex-topbar .calvex-control .shiny-input-container { width: 100% !important; }
        /* selects sized to their content (shrink only when tight) so gaps
           stay visually even instead of growing at different rates */
        .calvex-control--time     { flex: 0 1 165px; min-width: 105px; }
        .calvex-control--violence { flex: 0 1 235px; min-width: 125px; }
        .calvex-control--compare  { flex: 0 1 195px; min-width: 105px; }
        .calvex-control--chart    { flex: 0 1 155px; min-width: 105px; }
        .calvex-control--stats    { flex: 0 1 265px; min-width: 128px; }
        /* slider grows to fill remaining space after the fixed-width select controls */
        .calvex-topbar .calvex-control--slider > .shiny-input-container {
          flex: 1 1 150px;
          min-width: 140px;
          width: 100% !important;
          margin-left: auto;
        }
        .calvex-topbar label.control-label { white-space: nowrap; font-size: 0.66rem; }
        .calvex-topbar .selectize-input { font-size: 0.875rem; }
        .calvex-control-divided { padding-left: 1rem; }
      }

      /* ---- tablet: two controls per row in the top bar ---- */
      @media (max-width: 900px) {
        .calvex-topbar {
          gap: 0.6rem 0.9rem;
          padding: 0.7rem 1rem;
        }
        .calvex-control,
        .calvex-topbar .shiny-panel-conditional > .shiny-input-container {
          flex: 1 1 calc(50% - 0.9rem);
          min-width: 0;
        }
        .calvex-control .shiny-input-container,
        .calvex-topbar .shiny-panel-conditional > .shiny-input-container {
          width: 100% !important;
        }
        .calvex-control-divided { border-left: none; padding-left: 0; }
      }
      /* ---- phone: shorter chart ---- */
      @media (max-width: 768px) {
        .calvex-plot-wrap {
          height: auto;
          min-height: 420px;
          max-height: min(560px, calc(100dvh - 160px));
          max-height: min(560px, calc(100vh - 160px));
        }
      }
      /* ---- phone: collapsible top bar, compact two-per-row controls ---- */
      @media (max-width: 576px) {
        .calvex-topbar-toggle { display: block; }
        .calvex-topbar.calvex-collapsed { display: none; }
        .calvex-topbar { gap: 0.5rem 0.6rem; padding: 0.6rem 0.75rem; }
        .calvex-control { flex: 1 1 calc(50% - 0.6rem); min-width: 0; }
        .calvex-topbar label.control-label {
          font-size: 0.62rem;
          letter-spacing: 0.05em;
          margin-bottom: 0.25rem;
        }
        .calvex-topbar .selectize-input { font-size: 0.85rem; }
        .calvex-control--slider > .shiny-input-container { flex: 1 1 100%; }
        .calvex-logo { height: 2.75rem; }
        .calvex-app-title-link { font-size: 1rem; }
        .calvex-plot-wrap { padding: 6px 4px 4px; min-height: 360px; }
      }
      label.control-label { font-weight: 600; }
      .accordion-button { font-weight: 600; }
    "))
  ),

  # header (logo + link resets app to defaults)
  div(
    class = "calvex-header",
    tags$a(
      id = "calvex_home_reset",
      class = "calvex-app-title-link",
      href = "#",
      onclick = "Shiny.setInputValue('reset_to_defaults', Date.now()); return false;",
      tags$img(src = "images/logo-black.png", alt = "CalVEX logo", class = "calvex-logo"),
      tags$span(class = "calvex-app-title-text", "VEX Data Visualization Tool")
    )
  ),

  # mobile-only toggle to collapse/expand the chart controls bar
  div(
    class = "calvex-topbar-toggle",
    tags$button(
      type = "button",
      class = "calvex-topbar-toggle-btn",
      onclick = paste0(
        "document.querySelector('.calvex-topbar').classList.toggle('calvex-collapsed');",
        "this.classList.toggle('open');",
        "$(window).trigger('resize');"
      ),
      "Chart controls",
      tags$span(class = "calvex-toggle-caret", HTML("&#9662;"))
    )
  ),

  # top control bar: primary chart controls, separated from the data filters
  div(
    class = "calvex-topbar",
    div(
      class = "calvex-control calvex-control--time",
      selectInput("time_period", "Time period",
        choices = list("Lifetime" = "lifetime", "Past Year" = "past_year"),
        selected = "past_year",
        width = "170px"
      )
    ),
    div(
      class = "calvex-control calvex-control--violence",
      selectInput("violence", "Violence type",
        choices = list("Physical Violence" = "physical",
                    "Sexual Violence" = "sexual",
                    "Intimate Partner Violence" = "ipv",
                    "Sexual Violence Perpetration" = "sexual_perp",
                    "Physical Violence Perpetration" = "physical_perp"),
        selected = "physical",
        width = "240px"
      )
    ),
    div(
      class = "calvex-control calvex-control--compare",
      selectInput("demographic", "Compare by",
        choices = list("Gender" = "GENDER",
          "California Region" = "CA_REGION",
          "Age" = "AGE_6",
          "Race/Ethnicity" = "RACE_5",
          "Sexuality" = "LGB_3",
          "Income Quintile" = "INCOME_QUINTILE",
          "Education Level" = "EDUC5",
          "Employment Status" = "EMPLOY_2",
          "Disability Status" = "DISABILITY"),
        selected = "GENDER",
        width = "205px"
      )
    ),
    div(
      class = "calvex-control calvex-control-divided calvex-control--chart",
      selectInput("chart_type", "Chart type",
        choices = list("Bar chart" = "bar", "Line chart" = "line"),
        selected = "bar",
        width = "160px"
      )
    ),
    div(
      class = "calvex-control calvex-control--stats",
      selectInput("statistics", "Statistics display",
        choices = list("Percent (%)" = "percent",
                    "Raw number (n)" = "count"),
        selected = "percent",
        width = "180px"
      )
    ),
    # Y-axis max slider — only shown for percent + past-year (the only mode with truncation)
    conditionalPanel(
      condition = "input.statistics == 'percent' && input.time_period == 'past_year'",
      class = "calvex-control calvex-control--slider",
      sliderInput(
        "scale_max_override",
        "Y-axis max (%)",
        min   = 5,
        max   = 100,
        value = 40,
        step  = 5,
        ticks = FALSE,
        width = "210px"
      )
    )
  ),

  # sidebar (filters only) + main chart area
  layout_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      open = list(desktop = "open", mobile = "closed"),
      collapsible = TRUE,
      width = 285,
      class = "calvex-sidebar",

      div(
        class = "calvex-sidebar-checks",
        checkboxInput("overall", "Overall (all respondents)", value = TRUE),
        conditionalPanel(
          condition = "input.time_period == 'past_year' && input.violence != 'ipv'",
          checkboxInput("show_subcategories", "Show subcategories", value = FALSE)
        )
      ),

      accordion(
        open = FALSE,

        # accordion panel: Demographic Information
        accordion_panel(
          "Demographic Specifics",
          calvex_filter_group(
            "GENDER", "Gender identity",
            choices = list(
              "Female" = 1,
              "Male" = 2,
              "Gender non-conforming" = 3
            ),
            selected = list(1, 2, 3)
          ),
          calvex_filter_group(
            "LGB_3", "Self-described sexuality",
            choices = list(
              "Lesbian / Gay" = 1,
              "Straight" = 2,
              "Bisexual / other identity" = 3
            ),
            selected = list(1, 2, 3)
          ),
          calvex_filter_group(
            "AGE_6", "Age",
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
          calvex_filter_group(
            "RACE_5", "Race / ethnicity",
            choices = list(
              "White, Non-Hispanic" = 1,
              "Black, Non-Hispanic" = 2,
              "Asian, Non-Hispanic" = 3,
              "Hispanic" = 4,
              "Other/multiple races, Non-Hispanic" = 5
            ),
            selected = list(1, 2, 3, 4, 5)
          ),
          calvex_filter_group(
            "INCOME_QUINTILE", "Income quintile",
            choices = list(
              "Lowest Quintile" = 1,
              "Second Quintile" = 2,
              "Middle Quintile" = 3,
              "Fourth Quintile" = 4,
              "Highest Quintile" = 5
            ),
            selected = list(1, 2, 3, 4, 5)
          ),
          calvex_filter_group(
            "EDUC5", "Education level",
            choices = list(
              "Less than High School" = 1,
              "High School Graduate / Some College" = 2,
              "Bachelor's Degree" = 3,
              "Master's Degree" = 4,
              "Post-Graduate/Professional Degree" = 5
            ),
            selected = list(1, 2, 3, 4, 5)
          ),
          calvex_filter_group(
            "EMPLOY_2", "Employment status",
            choices = list(
              "Employed" = 1,
              "Unemployed / Not in Labor Force" = 2
            ),
            selected = list(1, 2)
          ),
          calvex_filter_group(
            "DISABILITY", "Disability status",
            choices = list(
              "No Disability" = 0,
              "Has Disability" = 1
            ),
            selected = list(0, 1)
          )
        ),

        # accordion panel: time & location
        accordion_panel(
          "Time & Location",
          calvex_filter_group(
            "YEAR", "Survey year",
            choices  = list("2025" = 2025, "2023" = 2023, "2022" = 2022, "2021" = 2021, "2020" = 2020),
            selected = c(2025, 2023, 2022, 2021, 2020)
          ),
          calvex_filter_group(
            "CA_REGION", "California region",
            choices = list(
              "Bay Region" = 1,
              "Central Valley" = 2,
              "Mountain Valley" = 3,
              "Northern" = 4,
              "Southern" = 5
            ),
            selected = list(1, 2, 3, 4, 5),
            help = paste(
              "Region filter applies to years with region data;",
              "respondents with missing region are excluded when filtering."
            )
          )
        ),

        accordion_panel(
          "Notes",
          tags$ul(
            class = "calvex-notes-list",
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
        )
      ),
      tags$footer(
        style = "padding: 0.3rem 0.1rem 0.5rem; font-size: 0.75rem; color: #333; line-height: 1.5;",
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

    # main panel: single plot or stacked subcategory plots
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
    )
  )
)
