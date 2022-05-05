

library(demogmx)
library(shiny)
library(dplyr)
library(ggplot2)


# User interface ----------------------------------------------------------
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "sel_state_aging",
                  label = "State(s)",
                  choices = unique(df_births_INEGI$state),
                  selected = "National",
                  multiple = TRUE,
                  selectize = TRUE),
      sliderInput(inputId = "sel_year_aging",
                  label = "Year",
                  min = 1985, max = 2020,
                  value = 2005,
                  step = 1,
                  ticks = TRUE,
                  sep = ""),
      selectInput(inputId = "sel_sex_aging",
                  label = "Sex",
                  choices = c("Female", "Male", "Total"),
                  selected = "Total",
                  multiple = TRUE,
                  selectize = TRUE),
      sliderInput(inputId = "sel_age_aging",
                  label = "Age range",
                  min = 0,
                  max = 89,
                  value = c(25,55),
                  step = 1,
                  ticks = TRUE,
                  dragRange = TRUE),
      h3(actionButton(inputId = "button_aging_age",
                      label = "Get aging population by age",
                      class = "btn-primary"),
         align = "center"),
      h3(actionButton(inputId = "button_aging_rate",
                      label = "Get aging rate by age",
                      class = "btn-primary"),
         align = "center"),
      h3(downloadButton(outputId = "download_aging_data",
                        label = "Download data"),
         align = "center")
    ),
    mainPanel(
      h2(textOutput("txt_aging_age"),
         align = "center"),
      plotOutput(outputId = "plt_aging_age",
                 width = "120vh",
                 height = "40vw"),
      h2(textOutput("txt_aging_rate"),
         align = "center"),
      plotOutput(outputId = "plt_aging_rate",
                 width = "120vh",
                 height = "55vw")
    )
  )

)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {

  ## Render UI ------------------------------------------------------------

  ## Reactive objects -----------------------------------------------------
  # Filtered dataset
  df_aging_server <- reactive({
    get_aging_rate(v_state = input$sel_state_aging,
                   v_year = input$sel_year_aging,
                   v_sex = input$sel_sex_aging,
                   v_age = seq(input$sel_age_aging[1],
                               input$sel_age_aging[2]),
                   age_groups = FALSE)
  })
  # To establish minor and major breaks in graphs's x-axis
  v_minor_breaks_aging <- reactive({
    seq(input$sel_age_aging[1], input$sel_age_aging[2])
  })
  v_major_breaks_aging <- reactive({
    c(input$sel_age_aging[1],
      v_minor_breaks_aging()[which(v_minor_breaks_aging() %% 5 == 0)],
      input$sel_age_aging[2])
  })

  ## Event reactive elements ----------------------------------------------
  # Aging population by age
  txt_aging_age_server <- eventReactive(input$button_aging_age, {
    paste("Aging population from", input$sel_age_aging[1], " to ", input$sel_age_aging[2],
          " years of age in ", input$sel_year_aging)
  })
  plt_aging_age_server <- eventReactive(input$button_aging_age, {
    # Create plot
    ggplot(data = df_aging_server(),
           aes(x = age, y = aging_pop, color = state, linetype = sex)) +
      theme_bw(base_size = 11) +
      geom_line(size = 1) +
      theme(legend.box = "vertical",
            legend.position = "bottom",
            legend.key.width = unit(x = 1.5, units = "cm"),
            text = element_text(size = 22),
            panel.grid.major.x = element_line(size = 1.5)) +
      scale_y_continuous(labels = scales::comma,
                         name = "Number of people") +
      scale_x_continuous(name = "Age (Years)",
                         breaks = v_major_breaks_aging(),
                         minor_breaks = v_minor_breaks_aging()) +
      guides(color = guide_legend(title = "State"),
             linetype = guide_legend(title = "Sex"))
  })

  # Aging rate by age
  txt_aging_rate_server <- eventReactive(input$button_aging_rate, {
    paste("Aging rate from", input$sel_age_aging[1], " to ", input$sel_age_aging[2],
          " years of age in ", input$sel_year_aging)
  })
  plt_aging_rate_server <- eventReactive(input$button_aging_rate, {
    # Create plot
    ggplot(data = df_aging_server(),
           aes(x = age, y = aging_rate, color = state, linetype = sex)) +
      theme_bw(base_size = 11) +
      geom_line(size = 1) +
      theme(legend.box = "vertical",
            legend.position = "bottom",
            legend.key.width = unit(x = 1.5, units = "cm"),
            text = element_text(size = 22),
            panel.grid.major.x = element_line(size = 1.5)) +
      scale_y_continuous(labels = scales::comma,
                         name = "Aging rate") +
      scale_x_continuous(name = "Age (Years)",
                         breaks = v_major_breaks_aging(),
                         minor_breaks = v_minor_breaks_aging()) +
      guides(color = guide_legend(title = "State"),
             linetype = guide_legend(title = "Sex"))
  })

  ## Render outputs -------------------------------------------------------
  # Aging population by age
  output$txt_aging_age <- renderText({txt_aging_age_server()})
  output$plt_aging_age <- renderPlot({plt_aging_age_server()})
  # Aging rate by age
  output$txt_aging_rate <- renderText({txt_aging_rate_server()})
  output$plt_aging_rate <- renderPlot({plt_aging_rate_server()})

  # Aging population data for download
  output$download_aging_data <- downloadHandler(
    filename = "aging_pop_data.csv",
    content = function(file) {
      write.csv(x = df_aging_server(), file = file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
