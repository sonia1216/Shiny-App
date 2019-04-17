#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Investment App"),
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(4,
         sliderInput("initial",
                     "Initial Amount",
                     min = 1,
                     max = 100000,
                     value = 1000, 
                     step=500,
                     pre= "$", sep=","), 
         sliderInput("annual", 
                     "Annual Contribution", 
                     min=0,
                     max=50000,
                     value=2000,
                     step=500,
                     pre="$", sep=",")
     )
   ,
   column(4, 
      sliderInput("return", 
                  "Return Rate (in %)",
                  min=0,
                  max=20,
                  step=0.1,
                  value=5),
      sliderInput("growth", 
                  "Growth Rate (in %)",
                  min=0,
                  max=20, 
                  step=0.1,
                  value=2)),

   column(4,
          sliderInput("years", 
                      "Years", 
                      min=0, 
                      max=50,
                      step=1,
                      value=20),
          selectInput(inputId="facet", 
                      label="Facet?",
                      choices=c("Yes", "No"))
          )),
   
      # Show a plotof the generated distribution
    fluidRow(
      h4("Timelines"),
        plotOutput(outputId= "plot"),
      h4("Balances"),
        verbatimTextOutput(outputId="table")
        )
      )

# Define server logic required to draw a histogram
server <- function(input, output) {
  future_value<-function(initial,return, years){
    initial<-initial*(1+return)**(years)
    return (initial)
  }
  annuity<-function(annual, return, years){
    initial<-annual*(((1+return)**years) - 1)/return
    return (initial)
  }
  growing_annuity<-function(annual, return, growth, years){
    initial<-annual*(((1+return)**years)-((1+growth)**years))/(return-growth)
    return (initial)
  }
  no_contrib_func<-function(initial, return, years){
    amount<-initial
    for (i in rep(1,years)){
      amount<- future_value(initial=initial, return=return, years=i)
      initial<-amount
    }
    return (amount)
  }
  fixed_contrib_func<-function(initial, return,years, annual){
    amount<-initial
    for (i in rep (1, years)){
      amount<- future_value(initial=initial, return=return, years=i)+annuity(annual=annual, return=return, years=i)
      initial<-amount
    }
    return (amount)
  }
  growing_contrib_func<-function(initial, return, years, annual, growth){
    amount<-initial
    for (i in rep (1, years)){
      amount<- future_value(initial=initial, return=return, years=i)
      initial<-amount
    }
    return (amount+growing_annuity(annual=annual, return=return, growth=growth, years=years))
  }
  output$plot<-renderPlot({
    if (input$facet=="No"){
      result_1<-c()
      for (i in (0:input$years)){
        no_contrib<-c(result_1, no_contrib_func(initial=input$initial, return=input$return/100, years=i))
        result_1<-no_contrib
      }
      
      result_2<-c()
      for (i in (0:input$years)){
        fixed_contrib<-c(result_2, fixed_contrib_func(initial=input$initial, return=input$return/100, years=i, annual=input$annual))
        result_2<-fixed_contrib
      }
      
      result_3<-c()
      for (i in (0:input$years)){
        growing_contrib<-c(result_3, growing_contrib_func(initial=input$initial, return=input$return/100, years=i, annual=input$annual, growth=input$growth/100))
        result_3<-growing_contrib
      }
      modalities<-data.frame(years=(0:input$years), no_contrib, fixed_contrib, growing_contrib)
      ggplot(modalities, aes(x= years, colour=modalities))+ggtitle("Three mofes of investing")+
      geom_line(aes(y= no_contrib, color="mode 1"))+
      geom_point(aes(y= no_contrib, color="mode 1"))+
      geom_line (aes(y= fixed_contrib, color="mode 2"))+
      geom_point(aes(y= fixed_contrib, color="mode 2"))+
      geom_line(aes(y= growing_contrib, color="mode 3"))+
      geom_point(aes(y= growing_contrib, color="mode 3"))+
      ylab("balance")+theme_bw()+scale_color_discrete(labels = c("no contrib","fixed contrib","growing contrib"))
      }
  else {
    result_1<-c()
    for (i in (0:input$years)){
      no_contrib<-c(result_1, no_contrib_func(initial=input$initial, return=input$return/100, years=i))
      result_1<-no_contrib
    }
    
    result_2<-c()
    for (i in (0:input$years)){
      fixed_contrib<-c(result_2, fixed_contrib_func(initial=input$initial, return=input$return/100, years=i, annual=input$annual))
      result_2<-fixed_contrib
    }
    
    result_3<-c()
    for (i in (0:input$years)){
      growing_contrib<-c(result_3, growing_contrib_func(initial=input$initial, return=input$return/100, years=i, annual=input$annual, growth=input$growth/100))
      result_3<-growing_contrib
    }
    type_1<-rep("No contrib", input$years+1)
    type_2<-rep("Fixed contrib", input$years+1)
    type_3<-rep("Growing contrib", input$years+1)
    modalities_no<-data.frame("years"=(0:input$years), "type"=type_1, "contrib"= no_contrib)
    modalities_fixed<-data.frame("years"=(0:input$years), "type"=type_2, "contrib"=fixed_contrib)
    modalities_growing<-data.frame("years"=(0:input$years), "type"=type_3,"contrib"=growing_contrib)
    total<-rbind(modalities_no, modalities_fixed, modalities_growing)
    ggplot(total, aes(x =years, color = type)) +
      geom_line(aes(y = contrib)) +
      geom_point(aes(y = contrib))  + 
      geom_area(aes(y=contrib, fill= type),alpha = 0.4)+
      ggtitle("Facet Timeline Graph") + 
      xlab("Year") +
      ylab("balance") +
      theme_bw() +
      facet_grid(~ type)
    }
    })
  output$table<- renderPrint({
    result_1<-c()
    for (i in (0:input$years)){
      no_contrib<-c(result_1, no_contrib_func(initial=input$initial, return=input$return/100, years=i))
      result_1<-no_contrib
    }
    
    result_2<-c()
    for (i in (0:input$years)){
      fixed_contrib<-c(result_2, fixed_contrib_func(initial=input$initial, return=input$return/100, years=i, annual=input$annual))
      result_2<-fixed_contrib
    }
    
    result_3<-c()
    for (i in (0:input$years)){
      growing_contrib<-c(result_3, growing_contrib_func(initial=input$initial, return=input$return/100, years=i, annual=input$annual, growth=input$growth/100))
      result_3<-growing_contrib
    }
    modalities<-data.frame(years=(0:input$years), no_contrib, fixed_contrib  , growing_contrib)
    modalities
  })
}






# Run the application 
shinyApp(ui = ui, server = server)

