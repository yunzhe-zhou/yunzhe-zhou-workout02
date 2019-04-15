library(shiny)
ui=fluidPage(
  titlePanel("Saving-investing Modalities"),
  
  fluidRow(
    column(4,
           div(class = "option-group",
               sliderInput(inputId="amount",
                           label="Initail Amount",
                           value=1000,min=0,max=100000, pre = "$", sep = ",",step=500)
           )),
    column(4,
           div(class = "option-group",
               sliderInput(inputId="rate",
                           label="Return Rate(in %)",
                           value=5,min=0,max=20,step=0.1)
           )),
    column(4,
           div(class = "option-group",
               sliderInput(inputId="years",
                           label="Years",
                           value=10,min=0,max=50,step=1)
           ))
  ),
  
  fluidRow(
    column(4,
           div(class = "option-group",
               sliderInput(inputId="contrib",
                           label="Annual Contribution",
                           value=2000,min=0,max=50000,pre = "$", sep = ",",step=500)
           )),
    column(4,
           div(class = "option-group",
               sliderInput(inputId="growth",
                           label="Growth Rate(in %)",
                           value=2,min=0,max=20,step=0.1)
           )),
    column(4, 
           selectInput("input_type", "Facet?",
                       c("No","Yes"
                       )
                       
           ))
  ),
  fluidRow(
    HTML('<hr>')
  ),
  h3("Timelines"),
  plotOutput("plot1"),
  
  h3("Balances"),
  verbatimTextOutput("balances")
  
  
  
)
server=function(input,output){
  output$plot1=renderPlot({
    library(ggplot2)
    #' @title function future_value
    #' @description compute the future value of an investment
    #' @param amount (numeric) rate (numeric) years (numeric)
    #' @return value (numeric)
    future_value=function(amount,rate,years)
    {
      value=amount*(1+rate)^years
      return(value)
    }
    #' @title annuity
    #' @description compute the future value of annuity
    #' @param contrib (numeric) rate (numeric) years (numeric)
    #' @return value (numeric)
    annuity=function(contrib,rate,years)
    {
      value=contrib*(((1+rate)^years-1)/rate)
      return(value)
    }
    #' @title growing_annuity
    #' @description compute the future value of growing annuity
    #' @param contrib (numeric) rate (numeric) growth (numeric) years (numeric)
    #' @return value (numeric)
    growing_annuity=function(contrib,rate,growth,years)
    {
      value=contrib*(((1+rate)^years-(1+growth)^years)/(rate-growth))
      return(value)
    }
    
    no_contrib=c()
    fixed_contrib=c()
    growing_contrib=c()
    year=c()
    L=input$years+1
    amount=input$amount
    rate=input$rate/100
    contrib=input$contrib
    growth=input$growth/100
    for(i in 1:L)
    {
      year[i]=i-1
      no_contrib[i]=future_value(amount,rate,i-1)
      fixed_contrib[i]=future_value(amount,rate,i-1)+annuity(contrib,rate,i-1)
      growing_contrib[i]=future_value(amount,rate,i-1)+growing_annuity(contrib,rate,growth,i-1)
    }
    modalities=data.frame(year,no_contrib,fixed_contrib,growing_contrib)
    year2=rep(1:L,3)-1
    value=c(no_contrib,fixed_contrib,growing_contrib)
    variable=gl(3,L,labels=c("no_contrib","fixed_contrib","growing_contrib"))
    modalities2=data.frame(year2,value,variable)
    with(modalities2,levels(variable))
    if(input$input_type=="No")
    {
      ggplot(modalities2)+
        geom_point(aes(year2,value,color=variable),size=1.5)+
        geom_line(aes(year2,value,color=variable),size=0.9)+
        labs(title="Three modes of investing",x="year",y="value")
    } else{
      ggplot(modalities2)+
        geom_point(aes(year2,value,color=variable),size=1.5)+
        geom_line(aes(year2,value,color=variable),size=0.9)+
        geom_area(aes(year2,value,fill=variable),alpha=0.5)+
        labs(title="Three modes of investing",x="year",y="value")+
        facet_grid(~variable)+
        theme_bw()
    }
    
    
  })
  output$balances=renderPrint({
    library(ggplot2)
    #' @title function future_value
    #' @description compute the future value of an investment
    #' @param amount (numeric) rate (numeric) years (numeric)
    #' @return value (numeric)
    future_value=function(amount,rate,years)
    {
      value=amount*(1+rate)^years
      return(value)
    }
    #' @title annuity
    #' @description compute the future value of annuity
    #' @param contrib (numeric) rate (numeric) years (numeric)
    #' @return value (numeric)
    annuity=function(contrib,rate,years)
    {
      value=contrib*(((1+rate)^years-1)/rate)
      return(value)
    }
    #' @title growing_annuity
    #' @description compute the future value of growing annuity
    #' @param contrib (numeric) rate (numeric) growth (numeric) years (numeric)
    #' @return value (numeric)
    growing_annuity=function(contrib,rate,growth,years)
    {
      value=contrib*(((1+rate)^years-(1+growth)^years)/(rate-growth))
      return(value)
    }
    
    no_contrib=c()
    fixed_contrib=c()
    growing_contrib=c()
    year=c()
    L=input$years+1
    amount=input$amount
    rate=input$rate/100
    contrib=input$contrib
    growth=input$growth/100
    for(i in 1:L)
    {
      year[i]=i-1
      no_contrib[i]=future_value(amount,rate,i-1)
      fixed_contrib[i]=future_value(amount,rate,i-1)+annuity(contrib,rate,i-1)
      growing_contrib[i]=future_value(amount,rate,i-1)+growing_annuity(contrib,rate,growth,i-1)
    }
    modalities=data.frame(year,no_contrib,fixed_contrib,growing_contrib)
    
    modalities
    
  }
  )
  
}
shinyApp(ui=ui,server=server)
