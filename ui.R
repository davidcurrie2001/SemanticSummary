
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)

shinyUI(
  fluidPage(

      # Application title
  titlePanel("Semantic Summary"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("SpeciesSelect", "Select a species:",choices="(Any)", selected = "(Any)"),
      selectInput("StockSelect", "Select a stock:",choices="(Any)", selected = "(Any)"),
      selectInput("WGSelect", "Select an ICES WG:",choices="(Any)", selected = "(Any)"),
      selectInput("AreaSelect", "Select an area:",choices="(Any)", selected = "(Any)"),
      selectInput("GearSelect", "Select a gear:",choices="(Any)", selected = "(Any)"),
      selectInput("StatusSelect", "Select a conservation status:",choices="(Any)", selected = "(Any)")
      ),

  
    # Show a plot of the generated distribution
    mainPanel(
      htmlOutput("title"),
      htmlOutput("speciesdash"),
      htmlOutput("thumbnail"),
      htmlOutput("abstract"),
      plotlyOutput('SummaryPlot'),
      dataTableOutput('StockList'),
      dataTableOutput('SummaryDataTable')
    )
   )
  )
)
