
# Load libraries
library(arules)
library(arulesViz)
library(DT)
library(shiny)
library(shinydashboard)
library(plotly)

# Load Data
data("Groceries")
basket = read.csv('basket.csv',header = TRUE)
item = read.csv('single.csv',header = TRUE)
product_info = read.csv('product_info.csv',header = TRUE)

# Design User Interface
ui = shinyUI(navbarPage("Market Basket Analysis",
                      navbarMenu("Data",
                                 tabPanel("Product Info",DTOutput('product_info'),downloadButton('Download','download_info')),
                                 tabPanel('Transactions - Basket Format',DTOutput('basket')),
                                 tabPanel('Transactions - Single Format',DTOutput('item'))
                                 ),
                      navbarMenu('Rule',
                                 tabPanel('Visualization',
                                          sidebarLayout(
                                            sidebarPanel(
                                              sliderInput("support", "Support:", min = 0, max = 1, value = 0.05, step = 1/100),
                                              sliderInput("confidence", "Confidence:", min = 0, max = 1, value = 0.2, step = 1/100),
                                              actionButton("rule_button", "Run New Rules")
                                            ),
                                            
                                            mainPanel(
                                              tabsetPanel(
                                                tabPanel("Rule",DTOutput('rule_table')), 
                                                tabPanel("Scatter Plot",plotlyOutput('rule_scatter')), 
                                                tabPanel("Matrix Plot",plotlyOutput('rule_matrix_confidence')),
                                                tabPanel('Network Graph',plotOutput('rule_graph'))
                                              )))
                                           ))
))

# Server Elements
server = function(input, output) {
  # Data - Basket Format  
  output$basket = renderDT({
    datatable(basket, filter = 'top',rownames = FALSE) 
  })
  
  # Data - Single Format
  output$item = renderDT({
    datatable(item, filter = 'top',rownames = FALSE) 
  })
  
  # Data - Product Info
  output$product_info = renderDT({
    datatable(product_info, filter = 'top',rownames = FALSE) 
  })
 
  # Reactive Object - event triggered when button clicked
  rule = eventReactive(input$rule_button, {
    apriori(Groceries, parameter=list(support=input$support,
                                      confidence=input$confidence))
      })
  # Rule Visualization - Table
  output$rule_table = renderDataTable({
    p=inspectDT(rule())[[1]]$data
    
    ratio_list = c('support','confidence','lift')
    for (i in ratio_list){
      p[i] = round(p[i], digits = 2)
    }
    
    datatable(p,filter='top', caption = 'Association Rules', rownames = FALSE)  
  })
  
 # Rule Visualization - Scatter Plot
  output$rule_scatter = renderPlotly({
    plotly_arules(rule(), measure = c('lift','confidence'), shading = 'support') %>%
      layout(title = 'Scatter Plot')
  })
  
 # Rule Visualization - Matrix Plot
  output$rule_matrix_confidence = renderPlotly({
    plotly_arules(rule(),method  = 'matrix', measure = 'confidence', shading = 'confidence', max = 20) %>%
      layout(title = 'Matrix Plot(Confidence)',
             xaxis = list(title = 'Antecedent'),
             yaxis = list(title = 'Consequent'))
  })
  
 # Rule Visualization - Network Graph
  output$rule_graph = renderPlot({
    top_lift = sort(rule(), decreasing = TRUE, na.last = NA, by = 'lift')
    subrule = head(top_lift,10)
    plot(subrule, method = 'graph', main='Network Graph for Top Ten Rules')
  })
  
}

# Run app
shinyApp(ui, server)
