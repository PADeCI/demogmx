
# Libraries ---------------------------------------------------------------
library(shiny)
library(ggplot2)
library(dplyr)
library(demogmx)
library(shinythemes)
library(shinycssloaders)

# User Interface ----------------------------------------------------------
ui <- navbarPage(
  title = "Prueba inicial",

  ## Birthts ----------------------------------------------------------------
  tabPanel("Births",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "sel_state_births",
                           label = "State(s)",
                           choices = unique(df_birth_pop_states$state),
                           selected = "National",
                           multiple = TRUE,
                           selectize = TRUE),
               sliderInput(inputId = "sel_year_births",
                           label = "Year range",
                           value = c(1990, 2010),
                           min = 1970,
                           max = 2050,
                           step = 1,
                           ticks = T,
                           sep = ""),
               selectInput(inputId = "sel_sex_births",
                           label = "Sex",
                           choices = c("Female", "Male", "Total"),
                           selected = "Total",
                           multiple = TRUE,
                           selectize = TRUE),
               h3(actionButton(inputId = "button_n_births",
                               label = "Number of births",
                               class = "btn-primary"),
                  align = "center"),
               h3(actionButton(inputId = "button_r_births",
                               label = "Birth rate",
                               class = "btn-primary"),
                  align = "center"),
               h3(actionButton(inputId = "button_prop_births",
                               label = "Sex proportion",
                               class = "btn-primary"),
                  align = "center"),
               h3(downloadButton(outputId = "download_birth_data",
                              label = "Download data"),
                  align = "center"),
               br(),
               h6("NOTE 1: The graphs that have Female and Male sex are taken from
                  INEGI. These graphs only have information from 1985 to 2020."),
               h6("NOTE 2: If sex = Total, and no other sex has been selected,
                  the graphs can show information from 1970 to 2050.")
             ),
             mainPanel(
               h2(textOutput("caption_n_births"), align = "center"),
               plotOutput(outputId = "plt_n_births_UI",
                          width  = "120vh",
                          height = "40vw") %>%
                 withSpinner(type = 3,
                             color.background = getOption("spinner.color.background",
                                                          default = "#FFFFFF")),
               h2(textOutput("caption_r_births"), align = "center"),
               plotOutput(outputId = "plt_r_births_UI",
                          width  = "120vh",
                          height = "40vw") %>%
                 withSpinner(type = 3,
                             color.background = getOption("spinner.color.background",
                                                          default = "#FFFFFF")),
               h2(textOutput("caption_prop_births"), align = "center"),
               plotOutput(outputId = "plt_prop_births_UI",
                          width  = "120vh",
                          height = "40vw") %>%
                 withSpinner(type = 3,
                             color.background = getOption("spinner.color.background",
                                                          default = "#FFFFFF"))
             )
           )
           ),

  ## Migration --------------------------------------------------------------
  tabPanel("Migration",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "sel_state_mig",
                           label = "State(s)",
                           choices = unique(df_birth_pop_states$state),
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
               fluidRow(
                 column(width = 4,
                        radioButtons(inputId = "sel_age_type_mig",
                                     label = "Type of age selection",
                                     choices = c("All ages", "Range"),
                                     selected = "All ages")
                        ),
                 column(width = 8,
                        uiOutput(outputId = "ui_age_range")
                 )
               ),
               selectInput(inputId = "sel_sex_mig",
                           label = "Sex",
                           choices = c("Female", "Male", "Total"),
                           selected = "Total",
                           multiple = TRUE,
                           selectize = TRUE),
               selectInput(inputId = "sel_type_mig",
                           label = "Type of migration",
                           choices = c("Interstate", "International", "Total"),
                           selected = "Total",
                           multiple = TRUE,
                           selectize = TRUE),
               radioButtons(inputId = "sel_class_mig",
                            label = NULL,
                            choices = list("Emigration", "Immigration", "Total"),
                            selected = "Emigration",
                            inline = TRUE),
               h3(actionButton(inputId = "button_n_mig",
                               label = "Get migration",
                               class = "btn-primary"),
                  align = "center"),
               h3(actionButton(inputId = "button_r_mig",
                               label = "Migration rate",
                               class = "btn-primary"),
                  align = "center"),
               h3(actionButton(inputId = "button_prop_mig",
                               label = "Migration proportion",
                               class = "btn-primary"),
                  align = "center")
             ),
             mainPanel(
               h2("Emigration"),
               h2("Immigration"),
               h2("Emigration rate"),
               h2("Immigration rate"),
               h2(textOutput("caption_n_mig"), align = "center"),
               # plotOutput(outputId = "plt_n_births_UI",
               #            width  = "120vh",
               #            height = "40vw") %>%
               #   withSpinner(type = 3,
               #               color.background = getOption("spinner.color.background",
               #                                            default = "#FFFFFF"))


             )
           )),

  ## Population -------------------------------------------------------------
  tabPanel("Population"),

  ## Mortality --------------------------------------------------------------
  tabPanel("Mortality")
)



# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  ## Births ---------------------------------------------------------------
  ### Reactive objects ----------------------------------------------------
  # For birth plots
  df_births <- reactive({

    get_births(v_state = input$sel_state_births,
               v_year  = seq(input$sel_year_births[1], input$sel_year_births[2]),
               year_groups = FALSE)
  })

  df_births_sex <- reactive({
    get_births_INEGI(v_state = input$sel_state_births,
                     v_year  = seq(input$sel_year_births[1], input$sel_year_births[2]),
                     v_sex = input$sel_sex_births,
                     year_groups = FALSE)
  })

  ### Event reactive elements ---------------------------------------------
  # Title - number of births plot
  txt_n_births <- eventReactive(input$button_n_births, {

    if (all(length(input$sel_sex_births) == 1 & input$sel_sex_births == "Total")) {
      paste("Total number of births between ", input$sel_year_births[1], " and ", input$sel_year_births[2])
    } else if (all(length(input$sel_sex_births) == 1 & input$sel_sex_births == "Males")) {
      paste("Number of males born between ", input$sel_year_births[1], " and ", input$sel_year_births[2])
    } else if (all(length(input$sel_sex_births) == 1 & input$sel_sex_births == "Females")){
      paste("Number of females born between ", input$sel_year_births[1], " and ", input$sel_year_births[2])
    } else {
      paste("Number of births between ", input$sel_year_births[1], " and ", input$sel_year_births[2])
    }
  })
  # Plot - number of births
  plt_n_births <- eventReactive(input$button_n_births, {
    # Define breaks
    v_sel_year_births <- seq(input$sel_year_births[1], input$sel_year_births[2])

    v_mult_5_births <- c(input$sel_year_births[1],
                         v_sel_year_births[which(v_sel_year_births %% 5 == 0)],
                         input$sel_year_births[2])

    if ("Male" %in% input$sel_sex_births | "Female" %in% input$sel_sex_births) {
      ggplot(data = df_births_sex(),
             mapping = aes(x = year, y = births, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Number of births") +
        scale_x_continuous(name = "Year",
                           breaks = v_mult_5_births,
                           minor_breaks = v_sel_year_births) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))

      # ggplot(data = df_births_sex(),
      #        mapping = aes(x = year, y = births, color = state, linetype = sex)) +
      #   scale_y_continuous(name = "Number of births") +
      #   scale_x_continuous(name = "Year") +
      #   guides(color = guide_legend(title = "State"),
      #          linetype = guide_legend(title = "Sex")) +
      #   plt_base(maj_breaks = v_mult_5_births,
      #            min_breaks = v_sel_year_births)


    } else {
      ggplot(data = df_births(),
             mapping = aes(x = year, y = births, color = state)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Number of births") +
        scale_x_continuous(name = "Year",
                           breaks = v_mult_5_births,
                           minor_breaks = v_sel_year_births) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))
    }

  })
  # Title - birth rate plot
  txt_r_births <- eventReactive(input$button_r_births, {

    if (all(length(input$sel_sex_births) == 1 & input$sel_sex_births == "Total")) {
      paste("Total birth rate between ", input$sel_year_births[1], " and ", input$sel_year_births[2])
    } else if (all(length(input$sel_sex_births) == 1 & input$sel_sex_births == "Male")) {
      paste("Male birth rate between ", input$sel_year_births[1], " and ", input$sel_year_births[2])
    } else if (all(length(input$sel_sex_births) == 1 & input$sel_sex_births == "Female")){
      paste("Female birth rate between ", input$sel_year_births[1], " and ", input$sel_year_births[2])
    } else {
      paste("Birth rates between ", input$sel_year_births[1], " and ", input$sel_year_births[2])
    }
  })
  # Plot - birth rate plot
  plt_r_births <- eventReactive(input$button_r_births, {
    # Define breaks
    v_sel_year_births <- seq(input$sel_year_births[1], input$sel_year_births[2])
    v_mult_5_births <- c(input$sel_year_births[1],
                         v_sel_year_births[which(v_sel_year_births %% 5 == 0)],
                         input$sel_year_births[2])

    if ("Male" %in% input$sel_sex_births | "Female" %in% input$sel_sex_births) {
      ggplot(data = df_births_sex(),
             mapping = aes(x = year, y = birth_rate, color = state, linetype = sex)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Birth rate") +
        scale_x_continuous(name = "Year",
                           breaks = v_mult_5_births,
                           minor_breaks = v_sel_year_births) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))
    } else {
      ggplot(data = df_births(),
             mapping = aes(x = year, y = birth_rate, color = state)) +
        theme_bw(base_size = 11) +
        geom_line(size = 1) +
        theme(legend.box = "vertical",
              legend.position = "bottom",
              legend.key.width = unit(x = 1.5, units = "cm"),
              text = element_text(size = 22),
              panel.grid.major.x = element_line(size = 1.5)) +
        scale_y_continuous(labels = scales::comma,
                           name = "Birth rate") +
        scale_x_continuous(name = "Year",
                           breaks = v_mult_5_births,
                           minor_breaks = v_sel_year_births) +
        guides(color = guide_legend(title = "State"),
               linetype = guide_legend(title = "Sex"))
    }

  })
  # Title - proportion of births plot
  txt_prop_births <- eventReactive(input$button_prop_births, {
    paste("Proportion of births that are female between ", input$sel_year_births[1], " and ", input$sel_year_births[2])
  })
  # Plot -proportion of births plot
  plt_prop_births <- eventReactive(input$button_prop_births, {
    # Define breaks
    v_sel_year_births <- seq(input$sel_year_births[1], input$sel_year_births[2])
    v_mult_5_births <- c(input$sel_year_births[1],
                         v_sel_year_births[which(v_sel_year_births %% 5 == 0)],
                         input$sel_year_births[2])

    df_births_fem  <- get_births_INEGI(v_state = input$sel_state_births,
                                       v_year  = seq(input$sel_year_births[1], input$sel_year_births[2]),
                                       v_sex = "Female",
                                       year_groups = FALSE)

    ggplot(data = df_births_fem,
           mapping = aes(x = year, y = birth_prop, color = state)) +
      theme_bw(base_size = 11) +
      geom_line(size = 1) +
      geom_hline(yintercept = 0.5, size = 1.5, color = "black",
                 linetype = "dashed", alpha = 0.4) +
      theme(legend.box = "vertical",
            legend.position = "bottom",
            legend.key.width = unit(x = 1.5, units = "cm"),
            text = element_text(size = 22),
            panel.grid.major.x = element_line(size = 1.5)) +
      scale_y_continuous(labels = scales::comma,
                         name = "Proportion") +
      scale_x_continuous(name = "Year",
                         breaks = v_mult_5_births,
                         minor_breaks = v_sel_year_births) +
      guides(color = guide_legend(title = "State"))
  })

  ### Render outputs ------------------------------------------------------
  # Title - number of births plot
  output$caption_n_births <- renderText({txt_n_births()})
  # Plot - number of births
  output$plt_n_births_UI <- renderPlot({plt_n_births()})
  # Title - birth rate plot
  output$caption_r_births <- renderText({txt_r_births()})
  # Plot - birth rate
  output$plt_r_births_UI <- renderPlot({plt_r_births()})
  # Title - birth rate plot
  output$caption_prop_births <- renderText({txt_prop_births()})
  # Plot - birth rate
  output$plt_prop_births_UI <- renderPlot({plt_prop_births()})
  # Download - birth data
  output$download_birth_data <- downloadHandler(
    filename = "birth_data.zip",
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))

      fs <- c("df_births.csv", "df_births_sex.csv")
      write.csv(x = df_births(),     file = "df_births.csv", row.names = F)
      write.csv(x = df_births_sex(), file = "df_births_sex.csv", row.names = F)

      zip(zipfile = file, files = fs)
    },
    contentType = "application/zip"
  )

  ## Migration ------------------------------------------------------------
  ### Render UI's  --------------------------------------------------------
  output$ui_age_range <- renderUI({

    if (input$sel_age_type_mig == "Range") {
      sliderInput(inputId = "sel_age_mig",
                  label = "Age range",
                  value = c(25, 55),
                  min = 0,
                  max = 89,
                  step = 1,
                  ticks = T,
                  sep = "")
    } else {
      return(NULL)
    }

  })

  ### Reactive objects ----------------------------------------------------
  # For migration plots
  df_migration <- reactive({

    get_migration(v_state = input$sel_state_mig,
                  v_year  = seq(input$sel_year_mig[1], input$sel_year_mig[2]),
                  v_sex = input$sel_sex_mig,
                  v_age = seq(input$sel_age_mig[1], input$sel_age_mig[2]),
                  v_type = input$sel_type_mig,
                  age_groups = FALSE)
  })

  ### Event reactive elements ---------------------------------------------
  # Title - migration plot
  txt_n_mig <- eventReactive(input$button_n_mig, {

    if (length(input$sel_sex_mig) == 1 & input$sel_sex_mig == "Total") {
      paste("Total migration between ", input$sel_year_mig[1], " and ", input$sel_year_mig[1])
    } else if (length(input$sel_sex_mig) == 1 & input$sel_sex_mig == "Male") {
      paste("Male migration between ", input$sel_year_mig[1], " and ", input$sel_year_mig[1])
    } else if (length(input$sel_sex_mig) == 1 & input$sel_sex_mig == "Female"){
      paste("Female migration between ", input$sel_year_mig[1], " and ", input$sel_year_mig[1])
    } else {
      paste("Migration between ", input$sel_year_mig[1], " and ", input$sel_year_mig[1])
    }
  })
  # Plot - migration
  plt_n_mig <- eventReactive(input$button_n_mig, {
    # Define breaks
    v_sel_year_mig <- seq(input$sel_year_mig[1], input$sel_year_mig[2])
    v_mult_5_mig <- c(input$sel_year_mig[1],
                         v_sel_year_mig[which(v_sel_year_mig %% 5 == 0)],
                         input$sel_year_mig[2])

    ggplot(data = df_migration(),
           mapping = aes(x = year, y = birth_prop, color = state)) +
      theme_bw(base_size = 11) +
      geom_line(size = 1) +
      theme(legend.position = "bottom",
            legend.key.width = unit(x = 1.5, units = "cm"),
            text = element_text(size = 22)) +
      scale_y_continuous(labels = scales::comma)



    # if (length(input$sel_type_mig) == 1 & input$sel_type_mig == "Interstate") {
    #
    # } else if (length(input$sel_type_mig) == 1 & input$sel_type_mig == "International") {
    #
    # } else if (length(input$sel_type_mig) == 1 & input$sel_type_mig == "Total") {
    #
    # }


    # if ("Male" %in% input$sel_sex_mig | "Female" %in% input$sel_sex_mig) {
    #   ggplot(data = df_migration(),
    #          mapping = aes(x = year, y = births, color = state, linetype = sex)) +
    #     theme_bw(base_size = 11) +
    #     geom_line(size = 1) +
    #     theme(legend.position = "bottom",
    #           legend.key.width = unit(x = 1.5, units = "cm"),
    #           text = element_text(size = 22)) +
    #     scale_y_continuous(labels = scales::comma)
    # } else {
    #   ggplot(data = df_migration(),
    #          mapping = aes(x = year, y = births, color = state)) +
    #     theme_bw(base_size = 11) +
    #     geom_line(size = 1) +
    #     theme(legend.position = "bottom",
    #           legend.key.width = unit(x = 1.5, units = "cm"),
    #           text = element_text(size = 22)) +
    #     scale_y_continuous(labels = scales::comma) +
    #     scale_x_continuous(breaks = v_mult_5_births,
    #                        minor_breaks = v_sel_year_mig)
    # }

  })

  ### Render outputs ------------------------------------------------------
  # Title - migration plot
  output$caption_n_mig <- renderText({txt_n_mig()})
  # Plot - migration
  # output$plt_n_births_UI <- renderPlot({plt_n_births()})


  ## Population -----------------------------------------------------------
  ### Reactive objects ----------------------------------------------------

  ### Event reactive elements ---------------------------------------------

  ### Render outputs ------------------------------------------------------


  ## Mortality ------------------------------------------------------------
  ### Reactive objects ----------------------------------------------------

  ### Event reactive elements ---------------------------------------------

  ### Render outputs ------------------------------------------------------
}

shinyApp(ui, server)



## Plotting functions ------------------------------------------------------
# plt_base <- function(maj_breaks, min_breaks) {
#   list(
#     theme_bw(base_size = 11),
#     geom_line(size = 1),
#     theme(legend.box = "vertical",
#           legend.position = "bottom",
#           legend.key.width = unit(x = 1.5, units = "cm"),
#           text = element_text(size = 22),
#           panel.grid.major.x = element_line(size = 1.5)),
#     scale_y_continuous(labels = scales::comma),
#     scale_x_continuous(name = "Year",
#                        breaks = maj_breaks,
#                        minor_breaks = min_breaks)
#   )
# }
