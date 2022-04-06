

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
      radioButtons(inputId = "pop_options_A",
                   label = "Select an option",
                   choices = list("One year graphs" = "one_year_pop",
                                  "Year range graphs" = "year_range_pop",
                                  "Download data" = "download_pop_data"),
                   selected = character(0),
                   inline = FALSE),
      uiOutput("ui_pop_options_B")
      ),
    mainPanel(
      uiOutput("ui_outputs")
    )
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {

  ## Reactive objects --------------------------------------------------------
  # Filtered dataset
  df_pop_server <- reactive({
    if (input$pop_options_A == "one_year_pop") {
      get_population(v_state = input$sel_state_pop,
                     v_year = input$sel_year_pop,
                     v_sex = input$sel_sex_pop,
                     v_age = seq(input$sel_age_pop[1],
                                 input$sel_age_pop[2]),
                     age_groups = FALSE)
    }
    else if (input$pop_options_A == "year_range_pop") {
      get_population(v_state = input$sel_state_pop,
                     v_year = seq(input$sel_year_pop[1],
                                  input$sel_year_pop[2]),
                     v_sex = input$sel_sex_pop,
                     v_age = c(0, 109),
                     age_groups = TRUE)
    }
    else if (input$pop_options_A == "download_pop_data") {
      get_population(v_state = input$sel_state_pop,
                     v_year = seq(input$sel_year_pop[1],
                                  input$sel_year_pop[2]),
                     v_sex = input$sel_sex_pop,
                     v_age = seq(input$sel_age_pop[1],
                                 input$sel_age_pop[2]),
                     age_groups = FALSE)
    }
  })
  # To establish minor and major breaks in graphs's x-axis
  v_minor_breaks_pop <- reactive({
    if (is.null(input$pop_options_A)) {
      return(NULL)
    }
    else if (input$pop_options_A == "one_year_pop") {
      seq(input$sel_age_pop[1], input$sel_age_pop[2])
    }
    else if (input$pop_options_A == "year_range_pop") {
      seq(input$sel_year_pop[1], input$sel_year_pop[2])
    }
  })
  v_major_breaks_pop <- reactive({
    if (is.null(input$pop_options_A)) {
      return(NULL)
    }
    else if (input$pop_options_A == "one_year_pop") {
      c(input$sel_age_pop[1],
        v_minor_breaks_pop()[which(v_minor_breaks_pop() %% 5 == 0)],
        input$sel_age_pop[2])
    }
    else if (input$pop_options_A == "year_range_pop") {
      c(input$sel_year_pop[1],
        v_minor_breaks_pop()[which(v_minor_breaks_pop() %% 5 == 0)],
        input$sel_year_pop[2])
    }
  })

  ## Render UI's -------------------------------------------------------------
  # Inputs set based on User selection
  output$ui_pop_options_B <- renderUI({
    if (is.null(input$pop_options_A)) {
      return(NULL)
    }
    else if (input$pop_options_A == "download_pop_data") {
      tagList(
        selectInput(inputId = "sel_state_pop",
                    label = "State(s)",
                    choices = unique(df_mortrate_state_age_sex$state),
                    selected = "National",
                    multiple = TRUE,
                    selectize = TRUE),
        sliderInput(inputId = "sel_year_pop",
                    label = "Year range",
                    value = c(1990, 2010),
                    min = 1970,
                    max = 2050,
                    step = 1,
                    ticks = T,
                    sep = ""),
        selectInput(inputId = "sel_sex_pop",
                    label = "Sex",
                    choices = c("Female", "Male", "Total"),
                    selected = "Total",
                    multiple = TRUE,
                    selectize = TRUE),
        sliderInput(inputId = "sel_age_pop",
                    label = "Age range",
                    min = 0,
                    max = 100,
                    value = c(25,55),
                    step = 1,
                    ticks = TRUE,
                    dragRange = TRUE),
        br(),
        h3(downloadButton(outputId = "download_pop_data",
                          label = "Download data"),
           align = "center")
      )
    }
    else if (input$pop_options_A == "one_year_pop") {
      # State, 1 Year, Sex, Age (Range)
      tagList(
        selectInput(inputId = "sel_state_pop",
                    label = "State(s)",
                    choices = unique(df_mortrate_state_age_sex$state),
                    selected = "National",
                    multiple = TRUE,
                    selectize = TRUE),
        sliderInput(inputId = "sel_year_pop",
                    label = "Year",
                    min = 1970, max = 2050,
                    value = 2005,
                    step = 1,
                    ticks = TRUE,
                    sep = ""),
        selectInput(inputId = "sel_sex_pop",
                    label = "Sex",
                    choices = c("Female", "Male", "Total"),
                    selected = "Total",
                    multiple = TRUE,
                    selectize = TRUE),
        sliderInput(inputId = "sel_age_pop",
                    label = "Age range",
                    min = 0,
                    max = 100,
                    value = c(25,55),
                    step = 1,
                    ticks = TRUE,
                    dragRange = TRUE),
        h3(actionButton(inputId = "button_pop_age",
                        label = "Get population by age",
                        class = "btn-primary"),
           align = "center"),
        h3(actionButton(inputId = "button_pyramid",
                        label = "Get population pyramid",
                        class = "btn-primary"),
           align = "center"),
        h3(actionButton(inputId = "button_pop_prop_age",
                        label = "Get sex proportion by age",
                        class = "btn-primary"),
           align = "center")

      )
    }
    else if (input$pop_options_A == "year_range_pop") {
      # State, Year (Range), Sex, Age (all ages)
      tagList(
        selectInput(inputId = "sel_state_pop",
                    label = "State(s)",
                    choices = unique(df_mortrate_state_age_sex$state),
                    selected = "National",
                    multiple = TRUE,
                    selectize = TRUE),
        sliderInput(inputId = "sel_year_pop",
                    label = "Year range",
                    value = c(1990, 2010),
                    min = 1970,
                    max = 2050,
                    step = 1,
                    ticks = T,
                    sep = ""),
        selectInput(inputId = "sel_sex_pop",
                    label = "Sex",
                    choices = c("Female", "Male", "Total"),
                    selected = "Total",
                    multiple = TRUE,
                    selectize = TRUE),
        h3(actionButton(inputId = "button_pop_years",
                        label = "Get population by years",
                        class = "btn-primary"),
           align = "center"),
        h3(actionButton(inputId = "button_pop_prop_year",
                        label = "Get sex proportion by years",
                        class = "btn-primary"),
           align = "center"),
        h6("The proportion bar grapg takes up to 4 STATES...")
      )

    }
  })
  # Download data, Plot and text outputs
  output$ui_outputs <- renderUI({

    if (is.null(input$pop_options_A)) {
      return(NULL)
    }
    else if (input$pop_options_A == "one_year_pop") {
      tagList(
        h2(textOutput("txt_pop_age"),
           align = "center"),
        plotOutput(outputId = "plt_pop_age",
                   width = "120vh",
                   height = "40vw"),
        h2(textOutput("txt_pyramid"),
           align = "center"),
        plotOutput(outputId = "plt_pyramid",
                   width = "120vh",
                   height = "55vw"),
        h2(textOutput("txt_pop_prop_age"),
           align = "center"),
        plotOutput(outputId = "plt_pop_prop_age",
                   width = "120vh",
                   height = "55vw")
      )
    }
    else if (input$pop_options_A == "year_range_pop") {
      tagList(
        h2(textOutput("txt_pop_years"),
           align = "center"),
        plotOutput(outputId = "plt_pop_years",
                   width = "120vh",
                   height = "45vw"),
        h2(textOutput("txt_pop_prop_year"),
           align = "center"),
        plotOutput(outputId = "plt_pop_prop_year",
                   width = "120vh",
                   height = "45vw")
      )
    }
  })


  ## Event reactive elements -------------------------------------------------
  # Population by age
  txt_pop_age_server <- eventReactive(input$button_pop_age, {
    paste("Population from", input$sel_age_pop[1], " to ", input$sel_age_pop[2],
          " years of age in ", input$sel_year_pop)
  })
  plt_pop_age_server <- eventReactive(input$button_pop_age, {
    ggplot(data = df_pop_server(), aes(x = age, y = population,
                                color = state, linetype = sex)) +
      theme_bw(base_size = 11) +
      geom_line(size = 1) +
      theme(legend.box = "vertical",
            legend.position = "bottom",
            legend.key.width = unit(x = 1.5, units = "cm"),
            text = element_text(size = 22),
            panel.grid.major.x = element_line(size = 1.5)) +
      scale_y_continuous(labels = scales::comma,
                         name = "Population") +
      scale_x_continuous(name = "Age (Years)",
                         breaks = v_major_breaks_pop(),
                         minor_breaks = v_minor_breaks_pop()) +
      guides(color = guide_legend(title = "State"),
             linetype = guide_legend(title = "Sex"))
  })
  # Population pyramid
  txt_pyramid_server <- eventReactive(input$button_pyramid, {
    paste("Population pyramid of people between ", input$sel_age_pop[1],
          " and ", input$sel_age_pop[1], " years of age in ", input$sel_year_pop)
  })
  plt_pyramid_server <- eventReactive(input$button_pyramid, {
    # Filter dataset
    df_pyramid <- get_population(v_state = input$sel_state_pop,
                                    v_year = input$sel_year_pop,
                                    v_sex = c("Male", "Female"),
                                    v_age = seq(input$sel_age_pop[1],
                                                input$sel_age_pop[2]),
                                    age_groups = FALSE)

    # Create pyramid plot
    ggplot(df_pyramid, aes(x = ifelse(test = (sex == "Male"),
                                           yes = -population, no = population),
                       y = factor(age), fill = sex)) +
      theme_bw(base_size = 11) +
      theme(legend.box = "vertical",
            legend.position = "bottom",
            legend.key.width = unit(x = 1.5, units = "cm"),
            text = element_text(size = 22)) +
      geom_col() +
      scale_x_continuous(name = "Population (thousands)",
                         labels = function(x) format(abs(x/1000), big.mark = ",", scientific = FALSE),
                         breaks = scales::breaks_pretty(n = 10)) +
      scale_y_discrete(name = "Age (Years)",
                       breaks = seq(from = input$sel_age_pop[1],
                                    to = input$sel_age_pop[2],
                                    by = 5)) +
      guides(fill = guide_legend(title = "Sex")) +
      theme(legend.position = "bottom") +
      facet_wrap(~ state)
  })
  # Sex proportion by age
  txt_pop_prop_age_server <- eventReactive(input$button_pop_prop_age, {
    paste("Sex proportion of population from ", input$sel_age_pop[1], " to ",
          input$sel_age_pop[2], " years of age in ", input$sel_year_pop)
  })
  plt_pop_prop_age_server <- eventReactive(input$button_pop_prop_age, {
    # Filter dataset
    df_prop_age <- get_population(v_state = input$sel_state_pop,
                                  v_year = input$sel_year_pop,
                                  v_sex = c("Male", "Female"),
                                  v_age = seq(input$sel_age_pop[1],
                                              input$sel_age_pop[2]),
                                  age_groups = FALSE)

    # Create plot
    ggplot(data = df_prop_age, aes(x = age, y = population, fill = sex)) +
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
      coord_cartesian(ylim = c(0.45, 0.55)) +
      facet_wrap(~ state)
  })

  # Population by year
  txt_pop_years_server <- eventReactive(input$button_pop_years, {
    paste("Total population from ", input$sel_year_pop[1], " to ", input$sel_year_pop[2])
  })
  plt_pop_years_server <- eventReactive(input$button_pop_years, {
    # Crete plot
    ggplot(data = df_pop_server(), aes(x = year, y = population,
                                       color = state, linetype = sex)) +
      theme_bw(base_size = 11) +
      geom_line(size = 1) +
      theme(legend.box = "vertical",
            legend.position = "bottom",
            legend.key.width = unit(x = 1.5, units = "cm"),
            text = element_text(size = 22),
            panel.grid.major.x = element_line(size = 1.5)) +
      scale_y_continuous(labels = scales::comma,
                         name = "Population") +
      scale_x_continuous(name = "Year",
                         breaks = v_major_breaks_pop(),
                         minor_breaks = v_minor_breaks_pop()) +
      guides(color = guide_legend(title = "State"),
             linetype = guide_legend(title = "Sex"))
  })
  # Sex proportion by year
  txt_pop_prop_year_server <- eventReactive(input$button_pop_prop_year, {
    paste("Sex proportion within total population from ", input$sel_year_pop[1], " to ", input$sel_year_pop[2])
  })
  plt_pop_prop_year_server <- eventReactive(input$button_pop_prop_year, {
    # Filter dataset (to have both sexes)
    df_prop_pop <- get_population(v_state = input$sel_state_pop,
                                  v_year = seq(input$sel_year_pop[1],
                                               input$sel_year_pop[2]),
                                  v_sex = c("Male", "Female"),
                                  v_age = c(0, 109),
                                  age_groups = TRUE)

    # Create plot
    ggplot(data = df_prop_pop,
           mapping = aes(x = year, y = population, fill = sex)) +
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
      coord_cartesian(ylim = c(0.475, 0.525)) +
      facet_wrap(~ state)
  })


  ## Render outputs ----------------------------------------------------------
  # Population by age
  output$txt_pop_age <- renderText({txt_pop_age_server()})
  output$plt_pop_age <- renderPlot({plt_pop_age_server()})
  # Population pyramid
  output$txt_pyramid <- renderText({txt_pyramid_server()})
  output$plt_pyramid <- renderPlot({plt_pyramid_server()})
  # Sex proportion by age
  output$txt_pop_prop_age <- renderText({txt_pop_prop_age_server()})
  output$plt_pop_prop_age <- renderPlot({plt_pop_prop_age_server()})

  # Population by year
  output$txt_pop_years <- renderText({txt_pop_years_server()})
  output$plt_pop_years <- renderPlot({plt_pop_years_server()})
  # Sex proportion by year
  output$txt_pop_prop_year <- renderText({txt_pop_prop_year_server()})
  output$plt_pop_prop_year <- renderPlot({plt_pop_prop_year_server()})

  # Population data for download
  output$download_pop_data <- downloadHandler(
    filename = "population_data.csv",
    content = function(file) {
      write.csv(x = df_pop_server(), file = file, row.names = FALSE)
    }
  )

}

shinyApp(ui, server)


