library(shiny)
library(dplyr)
library(ggplot2)

server <- function(input, output) {
  
  # data organization
  calvex2023 <- read.csv("data/CalVEX2023.csv")
  calvex2022 <- read.csv("data/CalVEX2022.csv")
  calvex2021 <- read.csv("data/CalVEX2021.csv")
  calvex2020 <- read.csv("data/CalVEX2020.csv")

  # isolate variables we are comparing (can change later) & combine into dataset
  cols_needed <- c(
    "GENDER_2", 
    "LGB_2", 
    "AGE_6", 
    "RACE_5", 
    "pv_ever",
    "sv_ever", 
    "data_year"
  )
  calvex2020 <- calvex2020[, cols_needed]
  calvex2021 <- calvex2021[, cols_needed]
  calvex2022 <- calvex2022[, cols_needed]
  calvex2023 <- calvex2023[, cols_needed]
  calvex_data <- rbind(
    calvex2023, 
    calvex2022, 
    calvex2021, 
    calvex2020
  )

  # lookup tables / labels for graph & legend
  # gender labels (graph)
  gender_labels <- c(
    "1" = "Woman", 
    "2" = "Man"
  )
  # sexuality labels (graph)
  sexuality_labels <- c(
    "1" = "LGB/other identity",
    "2" = "Straight"
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
  "GENDER_2" = "GENDER_2 (Gender)",
  "AGE_6" = "AGE_6 (Age)",
  "RACE_5" = "RACE_5 (Race/Ethnicity)",
  "LGB_2" = "LGB_2 (Sexuality)"
  )

  # reactive subset
filtered_data <- reactive({
  df <- calvex_data

  # filter by demographics
  if (!is.null(input$GENDER_2))
    df <- df[df$GENDER_2 %in% as.numeric(input$GENDER_2), ]
  if (!is.null(input$LGB_2))
    df <- df[df$LGB_2 %in% as.numeric(input$LGB_2), ]
  if (!is.null(input$AGE_6))
    df <- df[df$AGE_6 %in% as.numeric(input$AGE_6), ]
  if (!is.null(input$RACE_5))
    df <- df[df$RACE_5 %in% as.numeric(input$RACE_5), ]

  # filter by year
  if (!is.null(input$YEAR))
    df <- df[df$data_year %in% as.numeric(input$YEAR), ]

  # convert codes to labels for plotting
    df$GENDER_2 <- factor(
      df$GENDER_2, 
      levels = names(gender_labels), 
      labels = gender_labels
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
    df$LGB_2 <- factor(
      df$LGB_2, 
      levels = names(sexuality_labels), 
      labels = sexuality_labels
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

    # choose between physical & sexual violence
    violence_col <- if (violence_type == "physical") "pv_ever" else "sv_ever"

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
    v_title <- if (violence_type == "physical") "Physical Violence" else "Sexual Violence"

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
        title = paste(v_title, "Experience by Year and Demographic")
      ) +
      ylim(0, ylim_max) +
      theme_minimal()
    }
  )


}  

