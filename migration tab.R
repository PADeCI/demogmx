


library(shiny)
library(demogmx)
library(dplyr)
library(ggplot2)


# User Interface ----------------------------------------------------------
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "mig_options_A",
                   label = "Select an option",
                   choices = list("International migration" = "International",
                                  "Interstate migration" = "Interstate",
                                  "Total migration" = "Total",
                                  "Download data" = "download_mig_data"),
                   selected = character(0),
                   inline = FALSE),
      uiOutput("ui_mig_options_B"),
      uiOutput("ui_mig_options_C")
      ),
    mainPanel(
      h2(textOutput("txt_n_emig"),
         align = "center"),
      plotOutput(outputId = "plt_n_emig",
                 width = "120vh",
                 height = "40vw"),
      h2(textOutput("txt_n_immig"),
         align = "center"),
      plotOutput(outputId = "plt_n_immig",
                 width = "120vh",
                 height = "40vw"),
      h2(textOutput("txt_r_emig"),
         align = "center"),
      plotOutput(outputId = "plt_r_emig",
                 width = "120vh",
                 height = "40vw"),
      h2(textOutput("txt_r_immig"),
         align = "center"),
      plotOutput(outputId = "plt_r_immig",
                 width = "120vh",
                 height = "40vw")
    )
  )
)

server <- function(input, output, session) {


  # Reactive objects -------------------------------------------------------
  df_mig <- reactive({
    get_migration(v_state = input$sel_state_mig,
                  v_year = seq(input$sel_year_mig[1],
                               input$sel_year_mig[2]),
                  v_sex = input$sel_sex_mig,
                  v_age = seq(input$sel_age_mig[1],
                              input$sel_age_mig[2]),
                  v_type = input$sel_type_mig,
                  age_groups = FALSE)
  })

  v_minor_breaks <- reactive({
    if (is.null(input$mig_options_B)) {
      return(NULL)
    }
    else if (input$mig_options_B == "By year") {
      seq(input$sel_year_mig[1], input$sel_year_mig[2])
    }
    else if (input$mig_options_B == "By age") {
      seq(input$sel_age_mig[1], input$sel_age_mig[2])
    }
  })
  v_major_breaks <- reactive({
    if (is.null(input$mig_options_B)) {
      return(NULL)
    }
    else if (input$mig_options_B == "By year") {
      c(input$sel_year_mig[1],
        v_minor_breaks()[which(v_minor_breaks() %% 5 == 0)],
        input$sel_year_mig[2])
    }
    else if (input$mig_options_B == "By age") {
      c(input$sel_age_mig[1],
        v_minor_breaks()[which(v_minor_breaks() %% 5 == 0)],
        input$sel_age_mig[2])
    }
  })



  # Render UI's -------------------------------------------------------------
  # By year, age, and download data
  output$ui_mig_options_B <- renderUI({

    if (is.null(input$mig_options_A)) {
      return(NULL)
    }
    else if (input$mig_options_A == "download_mig_data") {

      tagList(

        selectInput(inputId = "sel_type_mig",
                    label = "Type of migration",
                    choices = unique(df_migration_expanded$type),
                    selected = "Total",
                    multiple = TRUE,
                    selectize = TRUE),

        selectInput(inputId = "sel_state_mig",
                    label = "State(s)",
                    choices = unique(df_migration_expanded$state),
                    selected = "National",
                    multiple = TRUE,
                    selectize = TRUE),

        sliderInput(inputId = "sel_year_mig",
                    label = "Year range",
                    value = c(1990, 2010),
                    min = 1970,
                    max = 2050,
                    step = 1,
                    ticks = T,
                    sep = ""),

        selectInput(inputId = "sel_sex_mig",
                    label = "Sex",
                    choices = c("Female", "Male", "Total"),
                    selected = "Total",
                    multiple = TRUE,
                    selectize = TRUE),

        sliderInput(inputId = "sel_age_mig",
                    label = "Age range",
                    min = 0,
                    max = 89,
                    value = c(25,55),
                    step = 1,
                    ticks = TRUE,
                    dragRange = TRUE),

        h3(downloadButton(outputId = "download_mig_data",
                          label = "Download data"),
           align = "center")
        )
      }
    else {
      radioButtons(inputId = "mig_options_B",
                   label = "Select a graph",
                   choices = c("By year", "By age"),
                   selected = character(0),
                   inline = TRUE)
    }
  })

  # Inputs
  output$ui_mig_options_C <- renderUI({
    if (is.null(input$mig_options_B)) {
      return(NULL)
    }
    else if (input$mig_options_A == "download_mig_data") {
      return(NULL)
    }
    else if (input$mig_options_B == "By year") {

      tagList(
        selectInput(inputId = "sel_state_mig",
                    label = "State(s)",
                    choices = unique(df_migration_expanded$state),
                    selected = "National",
                    multiple = TRUE,
                    selectize = TRUE),

        sliderInput(inputId = "sel_year_mig",
                    label = "Year range",
                    value = c(1990, 2010),
                    min = 1970,
                    max = 2050,
                    step = 1,
                    ticks = T,
                    sep = ""),

        selectInput(inputId = "sel_sex_mig",
                    label = "Sex",
                    choices = c("Female", "Male", "Total"),
                    selected = "Total",
                    multiple = TRUE,
                    selectize = TRUE),

        h3(actionButton(inputId = "button_n_emigrants",
                        label = "Number of emigrants",
                        class = "btn-primary"),
           align = "center"),
        h3(actionButton(inputId = "button_n_immigrants",
                        label = "Number of immigrants",
                        class = "btn-primary"),
           align = "center"),
        h3(actionButton(inputId = "button_r_emigrants",
                        label = "Emigration rate",
                        class = "btn-primary"),
           align = "center"),
        h3(actionButton(inputId = "button_r_immigrants",
                        label = "Immigration rate",
                        class = "btn-primary"),
           align = "center"),

        br(),
        h6("NOTE: In this option, the graphs show the migration data of all the ages.")
      )

    }
    else if (input$mig_options_B == "By age") {

      tagList(
        selectInput(inputId = "sel_state_mig",
                    label = "State(s)",
                    choices = unique(df_migration_expanded$state),
                    selected = "National",
                    multiple = TRUE,
                    selectize = TRUE),

        sliderInput(inputId = "sel_year_mig",
                    label = "Year",
                    value = 2005,
                    min = 1970, max = 2050,
                    step = 1,
                    ticks = TRUE),

        selectInput(inputId = "sel_sex_mig",
                    label = "Sex",
                    choices = c("Female", "Male", "Total"),
                    selected = "Total",
                    multiple = TRUE,
                    selectize = TRUE),

        sliderInput(inputId = "sel_age_mig",
                    label = "Age",
                    min = 0,
                    max = 89,
                    value = c(25,55),
                    step = 1,
                    ticks = TRUE,
                    dragRange = TRUE),

        h3(actionButton(inputId = "button_n_emigrants",
                        label = "Number of emigrants",
                        class = "btn-primary"),
           align = "center"),
        h3(actionButton(inputId = "button_n_immigrants",
                        label = "Number of immigrants",
                        class = "btn-primary"),
           align = "center"),
        h3(actionButton(inputId = "button_r_emigrants",
                        label = "Emigration rate",
                        class = "btn-primary"),
           align = "center"),
        h3(actionButton(inputId = "button_r_immigrants",
                        label = "Immigration rate",
                        class = "btn-primary"),
           align = "center"),

        br(),
        h6("NOTE:In this option, the graphs show the migration data of selected age range in only one year.")
      )
    }
  })


  # Event reactive elements -------------------------------------------------

  # emigration number
  txt_n_emig_server <- eventReactive(input$button_n_emigrants, {
    if (input$mig_options_A == "International" & input$mig_options_B == "By year") {
      paste("International emigration from ", input$sel_year_mig[1], " to ", input$sel_year_mig[2])
    }
    else if (input$mig_options_A == "International" & input$mig_options_B == "By age") {
      paste("International emigration of people between ", input$sel_age_mig[1], " and ", input$sel_age_mig[2], " years of age")
    }
    else if (input$mig_options_A == "Interstate" & input$mig_options_B == "By year") {
      paste("Interstate emigration from ", input$sel_year_mig[1], " to ", input$sel_year_mig[2])
    }
    else if (input$mig_options_A == "Interstate" & input$mig_options_B == "By age") {
      paste("Interstate emigration of people between ", input$sel_age_mig[1], " and ", input$sel_age_mig[2], " years of age")
    }
    else if (input$mig_options_A == "Total" & input$mig_options_B == "By year") {
      paste("Total emigration from ", input$sel_year_mig[1], " to ", input$sel_year_mig[2])
    }
    else if (input$mig_options_A == "Total" & input$mig_options_B == "By age") {
      paste("Total emigration of people between ", input$sel_age_mig[1], " and ", input$sel_age_mig[2], " years of age")
    }

  })
  plt_n_emig_server <- eventReactive(input$button_n_emigrants, {
    if (input$mig_options_A == "International" & input$mig_options_B == "By year") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = seq(input$sel_year_mig[1],
                                           input$sel_year_mig[2]),
                              v_sex = input$sel_sex_mig,
                              v_age = c(0, 89),
                              v_type = "International",
                              age_groups = TRUE)

      ggplot(data = df_mig,
             mapping = aes(x = year, y = emigrants, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Number of emigrants") +
        scale_x_continuous(name = "Year",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))
    }
    else if (input$mig_options_A == "International" & input$mig_options_B == "By age") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = input$sel_year_mig,
                              v_sex = input$sel_sex_mig,
                              v_age = c(input$sel_age_mig[1]:input$sel_age_mig[2]),
                              v_type = "International",
                              age_groups = FALSE)

      ggplot(data = df_mig,
             mapping = aes(x = age, y = emigrants, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Number of emigrants") +
        scale_x_continuous(name = "Age (Years)",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))

    }
    else if (input$mig_options_A == "Interstate" & input$mig_options_B == "By year") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = seq(input$sel_year_mig[1],
                                           input$sel_year_mig[2]),
                              v_sex = input$sel_sex_mig,
                              v_age = c(0, 89),
                              v_type = "Interstate",
                              age_groups = TRUE)

      ggplot(data = df_mig,
             mapping = aes(x = year, y = emigrants, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Number of emigrants") +
        scale_x_continuous(name = "Year",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))
    }
    else if (input$mig_options_A == "Interstate" & input$mig_options_B == "By age") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = input$sel_year_mig,
                              v_sex = input$sel_sex_mig,
                              v_age = c(input$sel_age_mig[1]:input$sel_age_mig[2]),
                              v_type = "Interstate",
                              age_groups = FALSE)

      ggplot(data = df_mig,
             mapping = aes(x = age, y = emigrants, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Number of emigrants") +
        scale_x_continuous(name = "Age (Years)",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))

    }
    else if (input$mig_options_A == "Total" & input$mig_options_B == "By year") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = seq(input$sel_year_mig[1],
                                           input$sel_year_mig[2]),
                              v_sex = input$sel_sex_mig,
                              v_age = c(0, 89),
                              v_type = "Total",
                              age_groups = TRUE)

      ggplot(data = df_mig,
             mapping = aes(x = year, y = emigrants, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Number of emigrants") +
        scale_x_continuous(name = "Year",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))
    }
    else if (input$mig_options_A == "Total" & input$mig_options_B == "By age") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = input$sel_year_mig,
                              v_sex = input$sel_sex_mig,
                              v_age = c(input$sel_age_mig[1]:input$sel_age_mig[2]),
                              v_type = "Total",
                              age_groups = FALSE)

      ggplot(data = df_mig,
             mapping = aes(x = age, y = emigrants, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Number of emigrants") +
        scale_x_continuous(name = "Age (Years)",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))

    }
  })

  # immigration number
  txt_n_immig_server <- eventReactive(input$button_n_immigrants, {
    if (input$mig_options_A == "International" & input$mig_options_B == "By year") {
      paste("International immigration from ", input$sel_year_mig[1], " to ", input$sel_year_mig[2])
    }
    else if (input$mig_options_A == "International" & input$mig_options_B == "By age") {
      paste("International immigration of people between ", input$sel_age_mig[1], " and ", input$sel_age_mig[2], " years of age")
    }
    else if (input$mig_options_A == "Interstate" & input$mig_options_B == "By year") {
      paste("Interstate immigration from ", input$sel_year_mig[1], " to ", input$sel_year_mig[2])
    }
    else if (input$mig_options_A == "Interstate" & input$mig_options_B == "By age") {
      paste("Interstate immigration of people between ", input$sel_age_mig[1], " and ", input$sel_age_mig[2], " years of age")
    }
    else if (input$mig_options_A == "Total" & input$mig_options_B == "By year") {
      paste("Total immigration from ", input$sel_year_mig[1], " to ", input$sel_year_mig[2])
    }
    else if (input$mig_options_A == "Total" & input$mig_options_B == "By age") {
      paste("Total immigration of people between ", input$sel_age_mig[1], " and ", input$sel_age_mig[2], " years of age")
    }

  })
  plt_n_immig_server <- eventReactive(input$button_n_immigrants, {
    if (input$mig_options_A == "International" & input$mig_options_B == "By year") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = seq(input$sel_year_mig[1],
                                           input$sel_year_mig[2]),
                              v_sex = input$sel_sex_mig,
                              v_age = c(0, 89),
                              v_type = "International",
                              age_groups = TRUE)

      ggplot(data = df_mig,
             mapping = aes(x = year, y = immigrants, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Number of immigrants") +
        scale_x_continuous(name = "Year",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))
    }
    else if (input$mig_options_A == "International" & input$mig_options_B == "By age") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = input$sel_year_mig,
                              v_sex = input$sel_sex_mig,
                              v_age = c(input$sel_age_mig[1]:input$sel_age_mig[2]),
                              v_type = "International",
                              age_groups = FALSE)

      ggplot(data = df_mig,
             mapping = aes(x = age, y = immigrants, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Number of emigrants") +
        scale_x_continuous(name = "Age (Years)",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))

    }
    else if (input$mig_options_A == "Interstate" & input$mig_options_B == "By year") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = seq(input$sel_year_mig[1],
                                           input$sel_year_mig[2]),
                              v_sex = input$sel_sex_mig,
                              v_age = c(0, 89),
                              v_type = "Interstate",
                              age_groups = TRUE)

      ggplot(data = df_mig,
             mapping = aes(x = year, y = immigrants, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Number of immigrants") +
        scale_x_continuous(name = "Year",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))
    }
    else if (input$mig_options_A == "Interstate" & input$mig_options_B == "By age") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = input$sel_year_mig,
                              v_sex = input$sel_sex_mig,
                              v_age = c(input$sel_age_mig[1]:input$sel_age_mig[2]),
                              v_type = "Interstate",
                              age_groups = FALSE)

      ggplot(data = df_mig,
             mapping = aes(x = age, y = immigrants, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Number of immigrants") +
        scale_x_continuous(name = "Age (Years)",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))

    }
    else if (input$mig_options_A == "Total" & input$mig_options_B == "By year") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = seq(input$sel_year_mig[1],
                                           input$sel_year_mig[2]),
                              v_sex = input$sel_sex_mig,
                              v_age = c(0, 89),
                              v_type = "Total",
                              age_groups = TRUE)

      ggplot(data = df_mig,
             mapping = aes(x = year, y = immigrants, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Number of immigrants") +
        scale_x_continuous(name = "Year",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))
    }
    else if (input$mig_options_A == "Total" & input$mig_options_B == "By age") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = input$sel_year_mig,
                              v_sex = input$sel_sex_mig,
                              v_age = c(input$sel_age_mig[1]:input$sel_age_mig[2]),
                              v_type = "Total",
                              age_groups = FALSE)

      ggplot(data = df_mig,
             mapping = aes(x = age, y = immigrants, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Number of immigrants") +
        scale_x_continuous(name = "Age (Years)",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))

    }
  })

  # emigration rate
  txt_r_emig_server <- eventReactive(input$button_r_emigrants, {
    if (input$mig_options_A == "International" & input$mig_options_B == "By year") {
      paste("International emigration rate from ", input$sel_year_mig[1], " to ", input$sel_year_mig[2])
    }
    else if (input$mig_options_A == "International" & input$mig_options_B == "By age") {
      paste("International emigration rate of people between ", input$sel_age_mig[1], " and ", input$sel_age_mig[2], " years of age")
    }
    else if (input$mig_options_A == "Interstate" & input$mig_options_B == "By year") {
      paste("Interstate emigration rate from ", input$sel_year_mig[1], " to ", input$sel_year_mig[2])
    }
    else if (input$mig_options_A == "Interstate" & input$mig_options_B == "By age") {
      paste("Interstate emigration rate of people between ", input$sel_age_mig[1], " and ", input$sel_age_mig[2], " years of age")
    }
    else if (input$mig_options_A == "Total" & input$mig_options_B == "By year") {
      paste("Total emigration rate from ", input$sel_year_mig[1], " to ", input$sel_year_mig[2])
    }
    else if (input$mig_options_A == "Total" & input$mig_options_B == "By age") {
      paste("Total emigration rate of people between ", input$sel_age_mig[1], " and ", input$sel_age_mig[2], " years of age")
    }

  })
  plt_r_emig_server <- eventReactive(input$button_r_emigrants, {
    if (input$mig_options_A == "International" & input$mig_options_B == "By year") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = seq(input$sel_year_mig[1],
                                           input$sel_year_mig[2]),
                              v_sex = input$sel_sex_mig,
                              v_age = c(0, 89),
                              v_type = "International",
                              age_groups = TRUE)

      ggplot(data = df_mig,
             mapping = aes(x = year, y = em_rate, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Emigration rate") +
        scale_x_continuous(name = "Year",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))
    }
    else if (input$mig_options_A == "International" & input$mig_options_B == "By age") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = input$sel_year_mig,
                              v_sex = input$sel_sex_mig,
                              v_age = c(input$sel_age_mig[1]:input$sel_age_mig[2]),
                              v_type = "International",
                              age_groups = FALSE)

      ggplot(data = df_mig,
             mapping = aes(x = age, y = em_rate, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Emigration rate") +
        scale_x_continuous(name = "Age (Years)",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))

    }
    else if (input$mig_options_A == "Interstate" & input$mig_options_B == "By year") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = seq(input$sel_year_mig[1],
                                           input$sel_year_mig[2]),
                              v_sex = input$sel_sex_mig,
                              v_age = c(0, 89),
                              v_type = "Interstate",
                              age_groups = TRUE)

      ggplot(data = df_mig,
             mapping = aes(x = year, y = em_rate, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Emigration rate") +
        scale_x_continuous(name = "Year",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))
    }
    else if (input$mig_options_A == "Interstate" & input$mig_options_B == "By age") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = input$sel_year_mig,
                              v_sex = input$sel_sex_mig,
                              v_age = c(input$sel_age_mig[1]:input$sel_age_mig[2]),
                              v_type = "Interstate",
                              age_groups = FALSE)

      ggplot(data = df_mig,
             mapping = aes(x = age, y = em_rate, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Emigration rate") +
        scale_x_continuous(name = "Age (Years)",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))

    }
    else if (input$mig_options_A == "Total" & input$mig_options_B == "By year") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = seq(input$sel_year_mig[1],
                                           input$sel_year_mig[2]),
                              v_sex = input$sel_sex_mig,
                              v_age = c(0, 89),
                              v_type = "Total",
                              age_groups = TRUE)

      ggplot(data = df_mig,
             mapping = aes(x = year, y = em_rate, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Emigration rate") +
        scale_x_continuous(name = "Year",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))
    }
    else if (input$mig_options_A == "Total" & input$mig_options_B == "By age") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = input$sel_year_mig,
                              v_sex = input$sel_sex_mig,
                              v_age = c(input$sel_age_mig[1]:input$sel_age_mig[2]),
                              v_type = "Total",
                              age_groups = FALSE)

      ggplot(data = df_mig,
             mapping = aes(x = age, y = em_rate, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Emigration rate") +
        scale_x_continuous(name = "Age (Years)",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))

    }
  })

  # immigration rate
  txt_r_immig_server <- eventReactive(input$button_r_immigrants, {
    if (input$mig_options_A == "International" & input$mig_options_B == "By year") {
      paste("International immigration rate from ", input$sel_year_mig[1], " to ", input$sel_year_mig[2])
    }
    else if (input$mig_options_A == "International" & input$mig_options_B == "By age") {
      paste("International immigration rate of people between ", input$sel_age_mig[1], " and ", input$sel_age_mig[2], " years of age")
    }
    else if (input$mig_options_A == "Interstate" & input$mig_options_B == "By year") {
      paste("Interstate immigration rate from ", input$sel_year_mig[1], " to ", input$sel_year_mig[2])
    }
    else if (input$mig_options_A == "Interstate" & input$mig_options_B == "By age") {
      paste("Interstate immigration rate of people between ", input$sel_age_mig[1], " and ", input$sel_age_mig[2], " years of age")
    }
    else if (input$mig_options_A == "Total" & input$mig_options_B == "By year") {
      paste("Total immigration rate from ", input$sel_year_mig[1], " to ", input$sel_year_mig[2])
    }
    else if (input$mig_options_A == "Total" & input$mig_options_B == "By age") {
      paste("Total immigration rate of people between ", input$sel_age_mig[1], " and ", input$sel_age_mig[2], " years of age")
    }

  })
  plt_r_immig_server <- eventReactive(input$button_r_immigrants, {
    if (input$mig_options_A == "International" & input$mig_options_B == "By year") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = seq(input$sel_year_mig[1],
                                           input$sel_year_mig[2]),
                              v_sex = input$sel_sex_mig,
                              v_age = c(0, 89),
                              v_type = "International",
                              age_groups = TRUE)

      ggplot(data = df_mig,
             mapping = aes(x = year, y = im_rate, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Immigration rate") +
        scale_x_continuous(name = "Year",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))
    }
    else if (input$mig_options_A == "International" & input$mig_options_B == "By age") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = input$sel_year_mig,
                              v_sex = input$sel_sex_mig,
                              v_age = c(input$sel_age_mig[1]:input$sel_age_mig[2]),
                              v_type = "International",
                              age_groups = FALSE)

      ggplot(data = df_mig,
             mapping = aes(x = age, y = im_rate, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Immigration rate") +
        scale_x_continuous(name = "Age (Years)",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))

    }
    else if (input$mig_options_A == "Interstate" & input$mig_options_B == "By year") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = seq(input$sel_year_mig[1],
                                           input$sel_year_mig[2]),
                              v_sex = input$sel_sex_mig,
                              v_age = c(0, 89),
                              v_type = "Interstate",
                              age_groups = TRUE)

      ggplot(data = df_mig,
             mapping = aes(x = year, y = im_rate, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Immigration rate") +
        scale_x_continuous(name = "Year",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))
    }
    else if (input$mig_options_A == "Interstate" & input$mig_options_B == "By age") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = input$sel_year_mig,
                              v_sex = input$sel_sex_mig,
                              v_age = c(input$sel_age_mig[1]:input$sel_age_mig[2]),
                              v_type = "Interstate",
                              age_groups = FALSE)

      ggplot(data = df_mig,
             mapping = aes(x = age, y = im_rate, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Immigration rate") +
        scale_x_continuous(name = "Age (Years)",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))

    }
    else if (input$mig_options_A == "Total" & input$mig_options_B == "By year") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = seq(input$sel_year_mig[1],
                                           input$sel_year_mig[2]),
                              v_sex = input$sel_sex_mig,
                              v_age = c(0, 89),
                              v_type = "Total",
                              age_groups = TRUE)

      ggplot(data = df_mig,
             mapping = aes(x = year, y = im_rate, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Immigration rate") +
        scale_x_continuous(name = "Year",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))
    }
    else if (input$mig_options_A == "Total" & input$mig_options_B == "By age") {

      df_mig <- get_migration(v_state = input$sel_state_mig,
                              v_year = input$sel_year_mig,
                              v_sex = input$sel_sex_mig,
                              v_age = c(input$sel_age_mig[1]:input$sel_age_mig[2]),
                              v_type = "Total",
                              age_groups = FALSE)

      ggplot(data = df_mig,
             mapping = aes(x = age, y = im_rate, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Immigration rate") +
        scale_x_continuous(name = "Age (Years)",
                           breaks = v_major_breaks(),
                           minor_breaks = v_minor_breaks()) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))

    }
  })

  # Render outputs ----------------------------------------------------------

  # emigration number
  output$txt_n_emig <- renderText({txt_n_emig_server()})
  output$plt_n_emig <- renderPlot({plt_n_emig_server()})
  # immigration number
  output$txt_n_immig <- renderText({txt_n_immig_server()})
  output$plt_n_immig <- renderPlot({plt_n_immig_server()})
  # emigration rate
  output$txt_r_emig <- renderText({txt_r_emig_server()})
  output$plt_r_emig <- renderPlot({plt_r_emig_server()})
  # immigration rate
  output$txt_r_immig <- renderText({txt_r_immig_server()})
  output$plt_r_immig <- renderPlot({plt_r_immig_server()})

  # Migration data for download
  output$download_mig_data <- downloadHandler(
    filename = "migration_data.csv",
    content = function(file) {
      write.csv(x = df_mig(), file = file, row.names = FALSE)
    }
  )

}

shinyApp(ui, server)
