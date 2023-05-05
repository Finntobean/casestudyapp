library(dplyr)
library(ggplot2)
library(forcats)
library(vroom)
library(shiny)

injPath <- "https://raw.githubusercontent.com/hadley/mastering-shiny/main/neiss/injuries.tsv.gz" 
injuries <- vroom::vroom(injPath) 

popPath <- "https://raw.githubusercontent.com/hadley/mastering-shiny/main/neiss/population.tsv"
population <- vroom::vroom(popPath) 

prodPath <- "https://raw.githubusercontent.com/hadley/mastering-shiny/main/neiss/products.tsv" 
products <- vroom::vroom(prodPath) 

if (!exists("injuries")) {
  injuries <- vroom::vroom("injuries.tsv.gz")
  products <- vroom::vroom("products.tsv")
  population <- vroom::vroom("population.tsv")
}

#<< ui
prod_codes <- setNames(products$prod_code, products$title)
# polish tables function 
count_top <- function(df, var, n_rows = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n_rows)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

ui <- fluidPage(tags$head(
  # Note the wrapping of the string in HTML()
  tags$style(HTML(" 
@import url('https://fonts.googleapis.com/css2?family=Merriweather&display=swap');

      body {
       background-image: url('https://ashtonplaceny.com/wp-content/uploads/Emergency-Room.jpg')!important;
        color: white;
      }  
      h2 {
        font-family: 'Merriweather', sans-serif;
      }
      .shiny-input-container {
        color: #474747;
      }
 "))
),
titlePanel("Emergency Room Injuries"),

  fluidRow(
    # user input to select product
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
  # user input to select variable to plot
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"),
    column(4, numericInput("n_rows", "Number of Rows", value = 5, min = 1, max = 20)))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
fluidRow(
  column(1, actionButton("prev_story", "<-")),
  column(1, actionButton("next_story", "->")),
  column(10, textOutput("narrative"))
  )
)
#>>

#<< server
server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  current_narrative <- reactiveVal(1)
  
  output$diag <- renderTable(count_top(selected(), diag, input$n_rows), width = "100%")
  
output$body_part <- renderTable(count_top(selected(), body_part, input$n_rows), width = "100%")
  
output$location <- renderTable(count_top(selected(), location, input$n_rows), width = "100%")
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)
  
  observeEvent(input$prev_story, {
    if (current_narrative() > 1) {
      current_narrative(current_narrative() - 1)
    }
  })
  
  observeEvent(input$next_story, {
    if (current_narrative() < length(selected()$narrative)) {
      current_narrative(current_narrative() + 1)
    }
  })
  
  narrative_sample <- reactive({
    selected() %>% pull(narrative) %>% .[current_narrative()]
  })
  output$narrative <- renderText(narrative_sample())
}
#>>

shinyApp(ui, server)
