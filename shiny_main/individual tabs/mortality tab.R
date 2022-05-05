

# Libraries ---------------------------------------------------------------
library(shiny)
library(ggplot2)
library(dplyr)
library(demogmx)
library(shinythemes)
library(shinycssloaders)

# User Interface ----------------------------------------------------------
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "mort_options_A",
                   label = "Select an option",
                   choices = list("One year graphs" = "one_year_mort",
                                  "Year range graphs" = "year_range_mort",
                                  "Download data" = "download_mort_data"),
                   selected = character(0),
                   inline = FALSE),
      uiOutput("ui_mort_options_B")
    ),
    mainPanel(
      uiOutput("ui_mort_outputs")
    )
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {

  ## Reactive objects --------------------------------------------------------
  # Filtered dataset
  df_mort_server <- reactive({
    if (input$mort_options_A == "one_year_mort") {
      get_deaths(v_state = input$sel_state_mort,
                 v_year = input$sel_year_mort,
                 v_sex = input$sel_sex_mort,
                 v_age = seq(input$sel_age_mort[1],
                             input$sel_age_mort[2]),
                 age_groups = FALSE)
    }
    else if (input$mort_options_A == "year_range_mort") {
      get_deaths(v_state = input$sel_state_mort,
                 v_year = seq(input$sel_year_mort[1],
                              input$sel_year_mort[2]),
                 v_sex = input$sel_sex_mort,
                 v_age = c(0, 109),
                 age_groups = TRUE)
    }
    else if (input$mort_options_A == "download_mort_data") {
      get_deaths(v_state = input$sel_state_mort,
                 v_year = seq(input$sel_year_mort[1],
                              input$sel_year_mort[2]),
                 v_sex = input$sel_sex_mort,
                 v_age = seq(input$sel_age_mort[1],
                             input$sel_age_mort[2]),
                 age_groups = FALSE)
    }
  })
  # To establish minor and major breaks in graphs's x-axis
  v_minor_breaks_mort <- reactive({
    if (is.null(input$mort_options_A)) {
      return(NULL)
    }
    else if (input$mort_options_A == "one_year_mort") {
      seq(input$sel_age_mort[1], input$sel_age_mort[2])
    }
    else if (input$mort_options_A == "year_range_mort") {
      seq(input$sel_year_mort[1], input$sel_year_mort[2])
    }
  })
  v_major_breaks_mort <- reactive({
    if (is.null(input$mort_options_A)) {
      return(NULL)
    }
    else if (input$mort_options_A == "one_year_mort") {
      c(input$sel_age_mort[1],
        v_minor_breaks_mort()[which(v_minor_breaks_mort() %% 5 == 0)],
        input$sel_age_mort[2])
    }
    else if (input$mort_options_A == "year_range_mort") {
      c(input$sel_year_mort[1],
        v_minor_breaks_mort()[which(v_minor_breaks_mort() %% 5 == 0)],
        input$sel_year_mort[2])
    }
  })

  ## Render UI's -------------------------------------------------------------
  # Inputs set based on User selection
  output$ui_mort_options_B <- renderUI({
    if (is.null(input$mort_options_A)) {
      return(NULL)
    }
    else if (input$mort_options_A == "download_mort_data") {
      tagList(
        selectInput(inputId = "sel_state_mort",
                    label = "State(s)",
                    choices = unique(df_mortrate_state_age_sex$state),
                    selected = "National",
                    multiple = TRUE,
                    selectize = TRUE),
        sliderInput(inputId = "sel_year_mort",
                    label = "Year range",
                    value = c(1990, 2010),
                    min = 1970,
                    max = 2050,
                    step = 1,
                    ticks = T,
                    sep = ""),
        selectInput(inputId = "sel_sex_mort",
                    label = "Sex",
                    choices = c("Female", "Male", "Total"),
                    selected = "Total",
                    multiple = TRUE,
                    selectize = TRUE),
        sliderInput(inputId = "sel_age_mort",
                    label = "Age range",
                    min = 0,
                    max = 100,
                    value = c(25,55),
                    step = 1,
                    ticks = TRUE,
                    dragRange = TRUE),
        br(),
        h3(downloadButton(outputId = "download_mort_data",
                          label = "Download data"),
           align = "center")
      )
    }
    else if (input$mort_options_A == "one_year_mort") {
      # State, 1 Year, Sex, Age (Range)
      tagList(
        selectInput(inputId = "sel_state_mort",
                    label = "State(s)",
                    choices = unique(df_mortrate_state_age_sex$state),
                    selected = "National",
                    multiple = TRUE,
                    selectize = TRUE),
        sliderInput(inputId = "sel_year_mort",
                    label = "Year",
                    min = 1970, max = 2050,
                    value = 2005,
                    step = 1,
                    ticks = TRUE,
                    sep = ""),
        selectInput(inputId = "sel_sex_mort",
                    label = "Sex",
                    choices = c("Female", "Male", "Total"),
                    selected = "Total",
                    multiple = TRUE,
                    selectize = TRUE),
        sliderInput(inputId = "sel_age_mort",
                    label = "Age range",
                    min = 0,
                    max = 100,
                    value = c(25,55),
                    step = 1,
                    ticks = TRUE,
                    dragRange = TRUE),
        h3(actionButton(inputId = "button_mort_age",
                        label = "Get mortality by age",
                        class = "btn-primary"),
           align = "center"),
        h3(actionButton(inputId = "button_mort_prop_age",
                        label = "Get mortality proportion by age",
                        class = "btn-primary"),
           align = "center"),
        h3(actionButton(inputId = "button_mort_rate_age",
                        label = "Get mortality rate by age",
                        class = "btn-primary"),
           align = "center")
      )
    }
    else if (input$mort_options_A == "year_range_mort") {
      tagList(
        selectInput(inputId = "sel_state_mort",
                    label = "State(s)",
                    choices = unique(df_mortrate_state_age_sex$state),
                    selected = "National",
                    multiple = TRUE,
                    selectize = TRUE),
        sliderInput(inputId = "sel_year_mort",
                    label = "Year range",
                    value = c(1990, 2010),
                    min = 1970,
                    max = 2050,
                    step = 1,
                    ticks = T,
                    sep = ""),
        selectInput(inputId = "sel_sex_mort",
                    label = "Sex",
                    choices = c("Female", "Male", "Total"),
                    selected = "Total",
                    multiple = TRUE,
                    selectize = TRUE),
        h3(actionButton(inputId = "button_mort_years",
                        label = "Get mortality by years",
                        class = "btn-primary"),
           align = "center"),
        h3(actionButton(inputId = "button_mort_prop_year",
                        label = "Get mortality proportion by year",
                        class = "btn-primary"),
           align = "center")
      )
    }
  })
  # Download data, Plot and text outputs
  output$ui_mort_outputs <- renderUI({

    if (is.null(input$mort_options_A)) {
      return(NULL)
    }
    else if (input$mort_options_A == "one_year_mort") {
      tagList(
        h2(textOutput("txt_mort_age"),
           align = "center"),
        plotOutput(outputId = "plt_mort_age",
                   width = "120vh",
                   height = "40vw"),
        h2(textOutput("txt_mort_prop_age"),
           align = "center"),
        plotOutput(outputId = "plt_mort_prop_age",
                   width = "120vh",
                   height = "55vw"),
        h2(textOutput("txt_mort_rate_age"),
           align = "center"),
        plotOutput(outputId = "plt_mort_rate_age",
                   width = "120vh",
                   height = "55vw")
      )
    }
    else if (input$mort_options_A == "year_range_mort") {
      tagList(
        h2(textOutput("txt_mort_years"),
           align = "center"),
        plotOutput(outputId = "plt_mort_years",
                   width = "120vh",
                   height = "45vw"),
        h2(textOutput("txt_mort_prop_year"),
           align = "center"),
        plotOutput(outputId = "plt_mort_prop_year",
                   width = "120vh",
                   height = "45vw")
      )
    }
  })

  ## Event reactive elements -------------------------------------------------
  # Mortality by age
  txt_mort_age_server <- eventReactive(input$button_mort_age, {
    paste("Mortality from", input$sel_age_mort[1], " to ", input$sel_age_mort[2],
          " years of age in ", input$sel_year_mort)
  })
  plt_mort_age_server <- eventReactive(input$button_mort_age, {
    # Create plot
    ggplot(data = df_mort_server(), aes(x = age, y = deaths,
                                        color = state, linetype = sex)) +
      theme_bw(base_size = 11) +
      geom_line(size = 1) +
      theme(legend.box = "vertical",
            legend.position = "bottom",
            legend.key.width = unit(x = 1.5, units = "cm"),
            text = element_text(size = 22),
            panel.grid.major.x = element_line(size = 1.5)) +
      scale_y_continuous(labels = scales::comma,
                         name = "Number of deaths") +
      scale_x_continuous(name = "Age (Years)",
                         breaks = v_major_breaks_mort(),
                         minor_breaks = v_minor_breaks_mort()) +
      guides(color = guide_legend(title = "State"),
             linetype = guide_legend(title = "Sex"))
  })
  # Sex proportion by age
  txt_mort_prop_age_server <- eventReactive(input$button_mort_prop_age, {
    paste("Mortality proportion by sex from ", input$sel_age_mort[1], " to ",
          input$sel_age_mort[2], " years of age in ", input$sel_year_mort)
  })
  plt_mort_prop_age_server <- eventReactive(input$button_mort_prop_age, {
    # Filter dataset
    df_prop_age <- get_deaths(v_state = input$sel_state_mort,
                              v_year = input$sel_year_mort,
                              v_sex = c("Male", "Female"),
                              v_age = seq(input$sel_age_mort[1],
                                          input$sel_age_mort[2]),
                              age_groups = FALSE)

    # Create plot
    ggplot(data = df_prop_age, aes(x = age, y = deaths, fill = sex)) +
      geom_col(position = "fill") +
      geom_hline(yintercept = 0.5, size = 1.5, color = "black",
                 linetype = "dashed", alpha = 0.4) +
      theme_bw(base_size = 11) +
      theme(legend.box = "vertical",
            legend.position = "bottom",
            legend.key.width = unit(x = 1.5, units = "cm"),
            text = element_text(size = 22)) +
      scale_y_continuous(labels = scales::comma,
                         name = "Proportion") +
      scale_x_continuous(name = "Age (Years)") +
      guides(fill = guide_legend(title = "Sex")) +
      coord_cartesian(ylim = c(0, 1)) +
      facet_wrap(~ state)
  })
  # Mortality rate by age
  txt_mort_rate_age_server <- eventReactive(input$button_mort_rate_age, {
    paste("Mortality rate from", input$sel_age_mort[1], " to ", input$sel_age_mort[2],
          " years of age in ", input$sel_year_mort)
  })
  plt_mort_rate_age_server <- eventReactive(input$button_mort_rate_age, {
    # Create plot
    ggplot(data = df_mort_server(),
           aes(x = age, y = death_rate, color = state, linetype = sex)) +
      theme_bw(base_size = 11) +
      geom_line(size = 1) +
      theme(legend.box = "vertical",
            legend.position = "bottom",
            legend.key.width = unit(x = 1.5, units = "cm"),
            text = element_text(size = 22),
            panel.grid.major.x = element_line(size = 1.5)) +
      scale_y_continuous(labels = scales::comma,
                         name = "Mortality rate") +
      scale_x_continuous(name = "Age (Years)",
                         breaks = v_major_breaks_mort(),
                         minor_breaks = v_minor_breaks_mort()) +
      guides(color = guide_legend(title = "State"),
             linetype = guide_legend(title = "Sex"))
  })

  # Mortality by year
  txt_mort_years_server <- eventReactive(input$button_mort_years, {
    paste("Number of deaths from ", input$sel_year_mort[1], " to ", input$sel_year_mort[2])
  })
  plt_mort_years_server <- eventReactive(input$button_mort_years, {
    # Crete plot
    ggplot(data = df_mort_server(), aes(x = year, y = deaths,
                                        color = state, linetype = sex)) +
      theme_bw(base_size = 11) +
      geom_line(size = 1) +
      theme(legend.box = "vertical",
            legend.position = "bottom",
            legend.key.width = unit(x = 1.5, units = "cm"),
            text = element_text(size = 22),
            panel.grid.major.x = element_line(size = 1.5)) +
      scale_y_continuous(labels = scales::comma,
                         name = "Number of deaths") +
      scale_x_continuous(name = "Year",
                         breaks = v_major_breaks_mort(),
                         minor_breaks = v_minor_breaks_mort()) +
      guides(color = guide_legend(title = "State"),
             linetype = guide_legend(title = "Sex"))
  })
  # Sex proportion by year
  txt_mort_prop_year_server <- eventReactive(input$button_mort_prop_year, {
    paste("Mortality proportion by sex from ", input$sel_year_mort[1], " to ", input$sel_year_mort[2])
  })
  plt_mort_prop_year_server <- eventReactive(input$button_mort_prop_year, {
    # Filter dataset (to have both sexes)
    df_prop_mort <- get_deaths(v_state = input$sel_state_mort,
                               v_year = seq(input$sel_year_mort[1],
                                            input$sel_year_mort[2]),
                               v_sex = c("Male", "Female"),
                               v_age = c(0, 109),
                               age_groups = TRUE)

    # Create plot
    ggplot(data = df_prop_mort,
           mapping = aes(x = year, y = deaths, fill = sex)) +
      theme_bw(base_size = 11) +
      geom_col(position = "fill") +
      geom_hline(yintercept = 0.5, size = 1.5, color = "black",
                 linetype = "dashed", alpha = 0.4) +
      theme(legend.box = "vertical",
            legend.position = "bottom",
            legend.key.width = unit(x = 1.5, units = "cm"),
            text = element_text(size = 22)) +
      scale_y_continuous(labels = scales::comma,
                         name = "Proportion") +
      scale_x_continuous(name = "Year") +
      guides(fill = guide_legend(title = "Sex")) +
      coord_cartesian(ylim = c(0, 1)) +
      facet_wrap(~ state)
  })

  ## Render outputs ----------------------------------------------------------
  # Mortality by age
  output$txt_mort_age <- renderText({txt_mort_age_server()})
  output$plt_mort_age <- renderPlot({plt_mort_age_server()})
  # Sex proportion by age
  output$txt_mort_prop_age <- renderText({txt_mort_prop_age_server()})
  output$plt_mort_prop_age <- renderPlot({plt_mort_prop_age_server()})
  # Mortality rate by sex
  output$txt_mort_rate_age <- renderText({txt_mort_rate_age_server()})
  output$plt_mort_rate_age <- renderPlot({plt_mort_rate_age_server()})

  # Mortality by year
  output$txt_mort_years <- renderText({txt_mort_years_server()})
  output$plt_mort_years <- renderPlot({plt_mort_years_server()})
  # Sex proportion by year
  output$txt_mort_prop_year <- renderText({txt_mort_prop_year_server()})
  output$plt_mort_prop_year <- renderPlot({plt_mort_prop_year_server()})

  # Mortality data for download
  output$download_mort_data <- downloadHandler(
    filename = "mortality_data.csv",
    content = function(file) {
      write.csv(x = df_mort_server(), file = file, row.names = FALSE)
    }
  )

}

shinyApp(ui, server)


