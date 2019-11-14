# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# generate a random annual return rate
set.seed(12345)
rate_bonds <- 0.05      #U.S. bonds annual avg return
vol_bonds <- 0.045      #U.S. annual volatility
rbonds <- rnorm(1, mean = rate_bonds, sd = vol_bonds)

set.seed(12345)
rate_stocks <- 0.10
vol_stocks <- 0.15
rstocks <- rnorm(1, mean = rate_stocks, sd = vol_stocks)

set.seed(12345)
rate_yield <- 0.02
vol_yield <- 0.01
ryield <- rnorm(1, mean = rate_yield, sd = vol_yield)

shinyServer(function(input,output) {
     #function that generates scenarios
     getNav <- reactive({
        #inputs
        inital.ammount = input$initial.ammount
        
        annual.contribution = input$annual.contribution
        
        annual.growth.rate = input$annual.growth.rate
        
        high.yield.rate = input$high.yield.rate
        
        us.bonds.rate = input$us.bonds.rate
        
        us.stocks.rate = input$us.stocks.rate
        
        high.yield.vol = input$high.yield.vol
        
        us.bonds.vol = input$us.bonds.vol
        
        us.stocks.vol = input$us.stocks.vol
        
        years = input$years
        
        random.seed = input$random.seed
        
        #simulation
        n = years-1
        for(i in 0:n){
        amount.bonds = (inital.amount*(1+rbonds))+(annual.contribution*((1+annual.growth.rate)^i))
        inital.ammount = amount.bonds
        }
        for(i in 0:n){
            amount.stocks = (inital.amount*(1+rstocks))+(annual.contribution*((1+annual.growth.rate)^i))
            inital.ammount = amount.stocks
        }
        for(i in 0:n){
            amount.yield = (inital.amount*(1+ryield))+(annual.contribution*((1+annual.growth.rate)^i))
            inital.ammount = amount.yield
        }
    })
})


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("How to Invest your Money Wisely"),
    
    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(3, 
               sliderInput("sliderbox", "Initial Amount",
                           min = 0,
                           max = 10,000,
                           value = 1,000,
                           step = 100)
            ),
        column(3,
               sliderInput("sliderbox2", "Annual Contribution",
                           min=0,
                           max=5,000,
                           value=200,
                           step = 100)
            ),
        column(3,
               sliderInput("sliderbox3", "Annual Growth Rate",
                           min = 0,
                           max = 20,
                           value = 2,
                           step = 0.1)
               ),
        column(3,
               sliderInput("sliderbox4", "High Yield Rate",
                           min = 0,
                           max = 20,
                           value = 2,
                           step = 0.1)
               ),
        column(3,
               sliderInput("sliderbox5", "Fixed Income rate (U.S. Bonds)",
                           min = 0,
                           max = 20,
                           value = 5,
                           step = 0.1)
               ),
        column(3,
               sliderInput("sliderbox6", "US Equity rate (U.S. Stocks)",
                           min = 0,
                           max = 20,
                           value = 10,
                           step = 0.1)
               ),
        column(3,
               sliderInput("sliderbox7", "High Yield volatility",
                           min = 0,
                           max = 20,
                           value = 0.1,
                           step = 0.1)
               ),
        column(3,
               sliderInput("sliderbox8", "Fixed Income volatility (U.S. Bonds)",
                           min = 0,
                           max = 20,
                           value = 4.5,
                           step = 0.1)
               ),
        column(3,
               sliderInput("sliderbox9", "US Equity volatility (U.S. Stocks)",
                           min = 0,
                           max = 20,
                           value = 15,
                           step = 0.1)
               ),
        column(3,
               sliderInput("sliderbox10", "Years",
                           min = 0,
                           max = 50,
                           value = 20,
                           step = 1)
               ),
        column(3, 
               numericInput("numericbox", "Random Seed",
                            12345, min=NA,
                            max=NA, step=NA)
        ),
        column(3,
               selectInput("selectedBox", "Facet?",
                           choices = c("Yes", "No"), selected = "Yes",
                           multiple = FALSE, selectize = TRUE)
        ),
        mainPanel(
            plotOutput("chart")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$chart <- renderPlot({
        chart <- nav = getNav()
        ggplot(aes(x = year, y = amount)) +
            geom_line()+
            geom_hline(yintercept = 0)+
            facet_wrap(scales = "free_y")
        chart
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
