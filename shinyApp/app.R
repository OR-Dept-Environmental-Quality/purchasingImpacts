# SETUP
# load packages
library(shiny)
library(tidyverse)
library(viridis)
library(ggthemes)

# graphic conventions
# create a custom theme for ggplots (assumes ggthemes is loaded)
theme_539 <- function() {
  theme_fivethirtyeight() +
    theme(
      rect=element_rect(fill="transparent"),
      panel.grid.major = element_blank(),
      axis.ticks = element_line(),
      plot.title.position = "plot",
      plot.title = element_text(hjust=0)
    )
}

# load datasets
impactIntensitiesWithGWPversions <-
  readRDS("impactIntensitiesWithGWPversions.RData")
householdSpending <-
  readRDS("householdSpending.RData")
governmentProcurement <-
  readRDS("governmentProcurement.RData")

# USER INTERFACE
ui <- navbarPage(
  title="SUPPLY CHAIN IMPACTS OF PURCHASING",

  # lay out the introduction page (no drop-down choices)
  tabPanel(
    title="Introduction",
    sidebarLayout(
      sidebarPanel(
        width=3,
        "sidebar text",
        fileInput(
          "file1",
          "Choose XLSX or XLSX file",
          accept = c(".xls", ".xlsx")
        )
      ),
      mainPanel(
        width=9,
        title = "main panel title",
        fluidRow(
          align="center",
          "the uploadedData",
          tableOutput("theUploadedData"),
          "the impactsInDetail",
          tableOutput("theImpactsInDetail")
        )
      )
    ) # close sidebarlayout for introduction page
  ), #close introduction page

  # lay out weights and impacts section
  navbarMenu(
    title="By user category",

    # lay out totals by material page
    tabPanel(
      title="blah",
      sidebarLayout(
        sidebarPanel(
          width=3,
          h3("blah2")
        ), # close sidebar panel
        mainPanel(
          width=9,
          fluidRow(
            column(
              width=6,
              "blah x",
              plotOutput("theUserCategorySpendingChart")
            ),
            column(
              width=6,
              "blah3"
            )
          ) # close fluidrow
        ) #close mainpanel for sidebar layout
      ) # close sidebarlayout
    ) #close tabPanel for  page
  ) #close navbarmenu for  section
) # close navbarPage
# end user interface definition

# SERVER LOGIC
# Define server logic required to draw a histogram
server <- function(input, output) {

  myUploadedData <- reactive({
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

  output$theUploadedData <- renderTable({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(
      need(
        ext == "xls" | ext=="xlsx",
        "Please upload an Excel file in the format suggested"
      )
    )
    myUploadedData()
  })

  impactsInDetail <- reactive({
    req(myUploadedData)
    # making the basic merge.
    left_join(
      myUploadedData(),
      impactIntensitiesWithGWPversions,
      by="beaCode"
    ) %>%
    mutate(
      impact=userSpending*impFactor*gwp,
      actualSpending=ifelse(substance=="carbon dioxide", userSpending, 0)
    )
  })

  output$theImpactsInDetail <- renderTable({
    req(impactsInDetail())
    impactsInDetail()
  })

  # making some lists of categories by abundance.
  impactCategoryNames <- reactive({
    req(impactsInDetail())
    impactsInDetail() %>%
      select(impactCategory) %>%
      distinct() %>%
      pull(impactCategory)
  })

  userCategoryNames <- reactive({
    impactsInDetail() %>%
    group_by(userCategory) %>%
    summarise(actualSpending=sum(actualSpending)) %>%
    ungroup() %>%
    arrange(desc(actualSpending)) %>%
    pull(userCategory)
  })

  beaCategoryNames <- reactive({
    impactsInDetail() %>%
    group_by(beaCode, beaName) %>%
    summarise(actualSpending=sum(actualSpending)) %>%
    ungroup() %>%
    arrange(desc(actualSpending)) %>%
    pull(beaName)
  })

  gwpVersionNames <- reactive({
    impactsInDetail() %>%
    group_by(gwpVersion) %>%
    summarise(
      impact=sum(ifelse(impactCategory=="GHG emissions", impact, 0))
    ) %>%
    ungroup() %>%
    arrange(desc(impact)) %>%
    pull(gwpVersion)
  })

  # data for summary charts by user category
  userCategoryChartData <- reactive({
    impactsInDetail() %>%
    filter(
      # later to be a selection in app...
      impactCategory=="GHG emissions",
      # later to be a multi checkbox selection in app...
      gwpVersion %in% c("IPCC AR5 100", "IPCC AR5 20")
    ) %>%
    group_by(impactCategory, impactUnits, gwpVersion, userCategory) %>%
    summarise(
      impact=sum(impact),
      actualSpending=sum(actualSpending)
    ) %>%
    ungroup() %>%
    mutate(
      userCategory=
        factor(userCategory, levels=rev(userCategoryNames())),
      gwpVersion=
        factor(gwpVersion, levels=rev(gwpVersionNames()))
    )
  })

  # a title for the impact category.. to be reactive
  userCategoryImpactChartTitle <- reactive({
    paste(
      userCategoryChartData() %>%
        select(impactCategory) %>%
        pull(impactCategory) %>%
        unique(),
      " (",
      userCategoryChartData() %>%
        select(impactUnits) %>%
        pull(impactUnits) %>%
        unique(),
      ")",
      sep=""
    )
  })

  # a chart object that is later going to be reactive
  userCategorySpendingChartObject <- ({
    ggplot()+
    theme_539()+
    ggtitle("Spending ($)")+
    geom_bar(
      data=userCategoryChartData(),
      aes(x=userCategory, y=actualSpending, fill=gwpVersion),
      position="dodge",
      stat="identity"
    )+
    coord_flip()+
    scale_fill_viridis(
      begin=0.32, end=1, discrete=T, direction=-1,
      option=sample(LETTERS[1:8],1),
      guide=guide_legend(reverse=T)
    )
  })

  # output that chart
  output$theUserCategorySpendingChart =
    renderPlot(userCategorySpendingChartObject(), height=500)

}

# Run the application
shinyApp(ui = ui, server = server)
