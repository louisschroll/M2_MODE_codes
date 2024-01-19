#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinysky)
library(ggthemes)
library(tidyverse)
library(knitr)
library(visNetwork)
library(plotly)
library(extrafont)
library(markdown) 
library(htmltools)

source(file = "simule_isolated_pop.R")
source(file = "simule_with_migration.R")



# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("cosmo"),
    chooseSliderSkin("Flat"),
    busyIndicator(text = "Calculation in progress ... ", wait = 0),

    navbarPage("Gull Population Dynamic",
      ## Onglet 1 : About the model
      tabPanel("About the model", 
               # Render the Markdown file with MathJax
               withMathJax(),
               uiOutput("markdownOutput")
               ),
      
      ## Onglet 2 : graphic for isolated populations
      tabPanel("Isolated populations",
       
        
        # Sidebar with a slider input
          sidebarPanel(
            sliderInput("nb_pop",
                        "Number of populations:",
                        min = 1,
                        max = 5,
                        value = 3),
            sliderInput("r",
                        "Growth rate:",
                        min = 0,
                        max = 2,
                        value = 1,
                        step = 0.1),
            sliderInput("K",
                        "Carrying capacity:",
                        min = 10,
                        max = 1000,
                        value = 500,
                        step = 10),
            sliderInput("N0",
                        "Initial population size:",
                        min = 0,
                        max = 200,
                        value = 100,
                        step = 10),
            actionButton("reset", "Reset")
          ),
          
          # Plot dynamic for isolated population 
          mainPanel(
            HTML("<h5>Explore populations without migration:</h5>
      <p>You can customize the following parameters for each population:
      <ul>
        <li><strong>Growth Rate:</strong> Choose a value for the growth rate.</li>
        <li><strong>Carrying Capacity:</strong> Set the carrying capacity.</li>
        <li><strong>Initial Size:</strong> Specify the initial size of the population.</li>
      </ul>
      <p>Parameters are consistent across all populations for simplicity.</p>"),
            plotlyOutput("distPlot"),
            downloadButton("saveButton", "Save as HTML"),
            div(
              style = "position: absolute; bottom: 20; right: 0;",
              imageOutput("myImage")
            )
          )
      ),
      
      ## Onglet 3 : graphic for connected population
      tabPanel("Metapopulation", 
               # Sidebar with a slider input
               sidebarPanel(
                 sliderInput("nb_pop_connected",
                             "Number of populations:",
                             min = 2,
                             max = 5,
                             value = 3),
                 sliderInput("N0_metapop", "Initial sizes",
                             min = 0,
                             max = 200,
                             value = 10),
                 selectInput("graph", "Choose a graph:", 
                             choices = c("Demographic curves", "Network"))
               ),
               
               # Plot dynamic for connected population 
               mainPanel(
                 HTML("<h5>Explore the impact of migration with different parameter values using the sliders.</h5>
      <ul>
        <li><strong>ri</strong>: Growth rate of population 'i'</li>
        <li><strong>Ki</strong>: Carrying capacity of population 'i'</li>
      </ul>
      <p>Initial population sizes are the same for all populations for simplicity.</p>")
                 ,
                  uiOutput("sliders"),
                  checkboxInput("show_K", "Show carrying capacity", value = TRUE),
                 conditionalPanel(
                   condition = "input.graph == 'Demographic curves'",
                   plotlyOutput("demographicPlot"),
                   downloadButton("saveButton2", "Save as HTML"),
                 ),
                 conditionalPanel(
                   condition = "input.graph == 'Network'",
                   visNetworkOutput("networkPlot"),
                   sliderInput("simulation_time", "Time", min = 1, max = 30, value = 1, step = 1),
               ),
               div(
                 style = "position: absolute; bottom: 20; right: 0;",
                 imageOutput("myImage2")
               )
               
      )),
      ## Onglet 4 : About the project
      tabPanel("The project", 
               withMathJax(),
               uiOutput("markdowTheProject")
      ),
      
))


# Define server logic required to draw a histogram
server <- function(input, output) {
  # Onglet 1 Markdown
  output$markdownOutput <- renderUI(
  withMathJax({
    includeMarkdown("markdown_file/about.md")
  }))
    # Onglet 2
    ## Generate the plot for isolated populations
  inputPlot1 <- reactive({
    # get parameters input$par from ui.R
    growth_rate    <- input$r
    carrying_cap <- input$K
    nb_of_population <- input$nb_pop
    initial_size <- input$N0
    
    # plot the dynamic for isolated populations
    plot_isolated_pop(parametre = c(growth_rate, carrying_cap, initial_size), nb_of_pop=nb_of_population)
  })
  
  output$distPlot <- renderPlotly({
    print(inputPlot1())
  })
    ## reset button
    observeEvent(input$reset, {
      updateSliderInput(inputId = "nb_pop", value = 3)
      updateSliderInput(inputId = "r", value = 1)
      updateSliderInput(inputId = "K", value = 500)
      updateSliderInput(inputId = "N0", value = 100)
    })
    
    # add gull image
    output$myImage <- renderImage({
      list(src = "images/mouette_rieuse.png",
           alt = "Description of your image",
           width = 200, height = 150)
    }, deleteFile = FALSE)
    
    # Function to save the plot
    output$saveButton <- downloadHandler(
      filename = function() {
        paste("plot_", Sys.Date(), ".html", sep = "")
      },
      content = function(file) {
        htmlwidgets::saveWidget(as_widget(inputPlot1()), file)
      }
    )
    
    
    # Onglet 3
    ## Generate the plot for connected populations
    output$demographicPlot <- renderPlotly({
      # get parameters input$par from ui.R
      nb_of_population <- input$nb_pop_connected
      growth_rate <- c(input$r1, input$r2, input$r3, input$r4, input$r5)
      growth_rate <- growth_rate[1:nb_of_population]
      carrying_cap <- c(input$K1, input$K2, input$K3, input$K4, input$K5)
      carrying_cap <- carrying_cap[1:nb_of_population]
      initial_size <- rep(input$N0_metapop, nb_of_population)
      # initial_size <- initial_size[1:nb_of_population]
      plot_connected_pop(parametre = list(r=growth_rate, K=carrying_cap, N0=initial_size), 
                           input$show_K)
      
    })
    # Define a reactive expression for parameters excluding simulation_time
    # Define a reactive expression for parameters excluding simulation_time
    params_expr <- reactive({
      list(
        nb_of_population = input$nb_pop_connected,
        growth_rate = c(input$r1, input$r2, input$r3, input$r4, input$r5)[1:input$nb_pop_connected],
        carrying_cap = c(input$K1, input$K2, input$K3, input$K4, input$K5)[1:input$nb_pop_connected],
        initial_size = rep(input$N0_metapop, input$nb_pop_connected)
      )
    })
    
    output$networkPlot <- renderVisNetwork({
      params <- params_expr()
      
      # Network
      res <- isolate({
        simulate_n_metapop(parametre = list(r = params$growth_rate, K = params$carrying_cap, N0 = params$initial_size))
      })
      
      plot_network(res, date = input$simulation_time, K = params$carrying_cap)
    })
    

    
    ## Adjust the number of slider to the number of pop
    observeEvent(input$simulation_time, {
      
    })

    ## Adjust the number of slider to the number of pop
    observeEvent(input$nb_pop_connected, {
      output$sliders = renderUI({
        tagList(
          # Sliders for r
          lapply(1:input$nb_pop_connected, function(x) {
            column(
              width = 4,
              sliderInput(paste0("r", x), paste0("r", x),
                          min = 0,
                          max = 2,
                          value = 1,
                          step = 0.1)
            )
          }),
          # Sliders for K
          lapply(1:input$nb_pop_connected, function(x) {
            column(
              width = 4, 
              sliderInput(paste0("K", x), paste0("K", x),
                          min = 10,
                          max = 1000,
                          value = 100)
            )
          }),
          # Sliders for N0
          # lapply(1:input$nb_pop_connected, function(x) {
          #   column(
          #     width = 4, 
          #     sliderInput(paste0("N0", x), paste("Initial pop size", x),
          #                 min = 0,
          #                 max = 200,
          #                 value = 10)
          #   )
          # })
        )
      })
    })
    
    # add gull image
    output$myImage2 <- renderImage({
      list(src = "images/mouette_rieuse.png",
           alt = "Description of your image",
           width = 200, height = 150)
    }, deleteFile = FALSE)
    
    # Function to save the plot
    output$saveButton2 <- downloadHandler(
      filename = function() {
        paste("plot_", Sys.Date(), ".html", sep = "")
      },
      content = function(file) {
        htmlwidgets::saveWidget(as_widget(inputPlot1()), file)
      }
    )
    
    # Onglet 4 Markdown 2
    output$markdowTheProject <- renderUI(
      withMathJax({
        includeMarkdown("markdown_file/about_the_project.md")})
      )
    
   
}

# Run the application 
shinyApp(ui = ui, server = server)
