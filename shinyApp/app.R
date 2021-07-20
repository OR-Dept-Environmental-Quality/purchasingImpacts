#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            fileInput(
                "file1",
                "Choose XLSX or XLSX file",
                accept = c(".xls", ".xlsx")
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           tableOutput("contents")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })

    myData <- reactive({
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      data <-
        read_excel(
          path=inFile$datapath,
#          path="source_data/users_spending_data_sample.xlsx",
          skip=11,
          col_names = TRUE
        ) %>%
        select(beaCode, userSpending, userCategory) %>%
        filter(userSpending > 0) %>%
        arrange(beaCode, desc(userSpending))
      data
    })

    output$contents <- renderTable({
        file <- input$file1
        ext <- tools::file_ext(file$datapath)

        req(file)
        validate(
          need(
            ext == "xls" | ext=="xlsx",
            "Please upload an Excel file"
          )
        )

        myData()
    })

}

# Run the application
shinyApp(ui = ui, server = server)
