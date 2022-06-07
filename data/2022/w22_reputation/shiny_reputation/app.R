library(shiny)
source("data.R")
##################  USER INTERFACE ##############################


ui <- fluidPage(
  titlePanel("Corporate Reputation Rankings"),
  sidebarLayout(
    sidebarPanel(

      radioButtons("dist", "Reputation category:",
                   c("CITIZENSHIP" = "citizenship",
                     "CULTURE" = "culture",
                     "ETHICS" = "ethics",
                     "GROWTH" = "growth",
                     "P&S"="ps",
                     "TRUST"="trust",
                     "VISION"="vision")),

      selectInput("controller", "Show", "plot",choices = c("plot","summary")),

      # br() element to introduce extra vertical spacing ----
      br(),

      
      actionButton("reset", "Reset")
     ),
    mainPanel(
      tabsetPanel(
        id = "switcher",
        type = "hidden",
        tabPanelBody("plot", "Score",plotOutput("plot1",inline = F,height = "300px"),
                     "Rank vs Score",plotOutput("plot2",inline = F,height = "400px")),
        tabPanelBody("summary", "summary",verbatimTextOutput("summary"))

      )
    )
  )
)
#############################################################################
server <- function(input, output, session) {
  observeEvent(input$controller, {
    updateTabsetPanel(inputId = "switcher", selected = input$controller)

  })

  d <- reactive({
    dist <-switch(input$dist,
                  citizenship = reputation$score[reputation$name=="CITIZENSHIP"],
                  culture = reputation$score[reputation$name=="CULTURE"],
                  ethics = reputation$score[reputation$name=="ETHICS"],
                  growth = reputation$score[reputation$name=="GROWTH"],
                  ps = reputation$score[reputation$name=="P&S"],
                  trust = reputation$score[reputation$name=="TRUST"],
                  vision = reputation$score[reputation$name=="VISION"],
                  rnorm)

   # dist(input$n)
  })

  b <- reactive({
    dist <-switch(input$dist,
                  citizenship = reputation$rank[reputation$name=="CITIZENSHIP"],
                  culture = reputation$rank[reputation$name=="CULTURE"],
                  ethics = reputation$rank[reputation$name=="ETHICS"],
                  growth = reputation$rank[reputation$name=="GROWTH"],
                  ps = reputation$rank[reputation$name=="P&S"],
                  trust = reputation$rank[reputation$name=="TRUST"],
                  vision = reputation$rank[reputation$name=="VISION"],
                  rnorm)

    #dist(input$n)
  })

  output$plot1 <- renderPlot({
    dist <- input$dist
    #n <- input$n

   hist(d(),
         main = paste(toupper(dist), "Score distribution 2022"),
        xlab ="Score",
         col = "#75AADB", border = "white")
  })#, height = 200, width = 300)

  output$plot2 <-renderPlot({
    dist <- input$dist
       n <- input$n
       plot(d(),b(),
            main = paste(toupper(dist), "Rank vs Score 2022"),
            xlab ="Score", ylab="Rank",
            col = "#75AADB", border = "white")
  })


  observeEvent(input$reset,{
    updateSliderInput(inputId = "controller", value="plot")
  })


  output$summary <- renderPrint({
    summary(d())
  })

  observeEvent(input$reset,{
    updateSliderInput(inputId = "controller", value="plot")
  })

}

# Run the application
shinyApp(ui, server)

