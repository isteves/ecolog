library(shiny)
library(ggplot2)
library(here)

data <- read.csv(here::here("data", "ecolog_shiny.csv"))[-1]

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Basic DataTable"),
    
    sidebarLayout(
        sidebarPanel(
            # conditionalPanel(
            #     'input.position == "TRUE"',
                checkboxGroupInput("show_vars", "Columns to show:",
                                   names(data)[1:5], selected = names(data)[2])),
            # ),
            # conditionalPanel(
            #     'input.event == "TRUE"',
            #     # helpText("Click the column header to sort a column."),
            #     checkboxGroupInput("show_vars2", "Columns to show:",
            #                        names(data)[2:6], selected = names(data)[2])
            # ),
            # conditionalPanel(
            #     'input.funding == "TRUE"',
            #     checkboxGroupInput("show_vars3", "Columns to show:",
            #                        names(data)[2:6], selected = names(data)[2])
                # helpText("Display 5 records by default.")
    
    # Create a new Row in the UI for selectInputs
    # fluidRow(
    #     column(4,
    #            selectInput("man",
    #                        "Manufacturer:",
    #                        c("All",
    #                          unique(as.character(mpg$manufacturer))))
    #     ),
    #     column(4,
    #            selectInput("trans",
    #                        "Transmission:",
    #                        c("All",
    #                          unique(as.character(mpg$trans))))
    #     ),
    #     column(4,
    #            selectInput("cyl",
    #                        "Cylinders:",
    #                        c("All",
    #                          unique(as.character(mpg$cyl))))
    #     )
    # ),
    # Create a new row for the table.
    mainPanel(
        tabsetPanel(
            id = 'dataset',
            tabPanel("positions", DT::dataTableOutput("mytable1")),
            tabPanel("events", DT::dataTableOutput("mytable2")),
            tabPanel("funding", DT::dataTableOutput("mytable3"))
        )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # choose columns to display
    positions <- data[data$position == TRUE, 1:5]
    output$mytable1 <- DT::renderDataTable({
        DT::datatable(positions[, input$show_vars, drop = FALSE])
    })
    
    events <- data[data$event == TRUE, 1:5]
    output$mytable2 <- DT::renderDataTable({
        DT::datatable(events[, input$show_vars, drop = FALSE])
    })
    
    funding <- data[data$funding == TRUE, 1:5]
    output$mytable3 <- DT::renderDataTable({
        DT::datatable(funding[, input$show_vars, drop = FALSE], 
                      options = list(pageLength = 10))
    })
    
    
    # # Filter data based on selections
    # output$table <- DT::renderDataTable(DT::datatable({
    #     data <- mpg
    #     if (input$man != "All") {
    #         data <- data[data$manufacturer == input$man,]
    #     }
    #     if (input$cyl != "All") {
    #         data <- data[data$cyl == input$cyl,]
    #     }
    #     if (input$trans != "All") {
    #         data <- data[data$trans == input$trans,]
    #     }
    #     data
    # }))
    
}

# Run the application 
shinyApp(ui = ui, server = server)

