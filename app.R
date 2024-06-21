library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(magrittr)

library(palmerpenguins)

vars <- c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")

ui <- fluidPage(

  titlePanel("펭귄 데이터 분석"),
  
  sidebarLayout(
    
    sidebarPanel(
      checkboxGroupInput(
        inputId = "species",
        label = "펭귄 종류를 선택하세요",
        choices = list("Adelie", "Gentoo", "Chinstrap"),
        selected = "Adelie"
      ),
      selectInput("x", "x축을 선택하세요.", choices = vars, selected = vars[1]),
      selectInput("y", "y축을 선택하세요.", choices = vars, selected = vars[4]),
      sliderInput("slider", '점 크기를 선택하세요', min = 1, max = 10, value = 5),
    ),
    
    mainPanel(
      dataTableOutput('df'),
      plotOutput('gg'),
    )
  )
  
    
)

server <- function(input, output, session) {
  sel_penguins <- reactive({
    penguins %>%
      filter(species %in% input$species)
  })
  
  output$df <- renderDataTable({
    sel_penguins() %>%
      datatable()
  })
  
  output$gg <- renderPlot({
    if (all(c("Gentoo", "Chinstrap") %in% input$species)) {
      color_order <- 2
      shape_order <- 1
    } else {
      color_order <- 1
      shape_order <- 2
    }

    sel_penguins() %>%
      ggplot(aes_string(x = input$x, y = input$y)) +
        geom_point(aes(color = species, shape = sex), size = input$slider) + 
        guides(color = guide_legend(order = color_order), shape = guide_legend(order = shape_order))
  })
  
}

shinyApp(ui = ui, server = server)
