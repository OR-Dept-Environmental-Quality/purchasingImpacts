# SETUP
# load packages
library(shiny)
library(tidyverse)
library(viridis)
library(ggthemes)
library(readxl)

# graphic conventions
# create a custom theme for ggplots (assumes ggthemes is loaded)
theme_539 <- function() {
  theme_fivethirtyeight() +
    theme(
      rect=element_rect(fill="transparent"),
      panel.grid.major = element_blank(),
      axis.ticks = element_line(),
      plot.title.position = "plot",
      plot.title = element_text(hjust=0),
      axis.text = element_text(size=14)
    )
}

# load datasets
impactIntensitiesWithGWPversions <-
  readRDS("impactIntensitiesWithGWPversions.RData")
householdSpending <-
  readRDS("householdSpending.RData")
governmentProcurement <-
  readRDS("governmentProcurement.RData")

# produce some lists
# making some lists of categories by abundance.
impactCategoryNames <-
  impactIntensitiesWithGWPversions %>%
    select(impactCategory) %>%
    distinct() %>%
    pull(impactCategory)

gwpVersionNames <-
  impactIntensitiesWithGWPversions %>%
    select(gwpVersion) %>%
    distinct() %>%
    pull(gwpVersion)

# USER INTERFACE
ui <- navbarPage(
  title="SUPPLY CHAIN IMPACTS OF PURCHASING",

  # lay out the introduction page (no drop-down choices)
  tabPanel(
    title="Choose or upload spending data",
    sidebarLayout(
      sidebarPanel(
        width=3,
        "First, enter your spending in a file like this:",
        downloadButton(
          "statFile",
          "download Excel template"
        ),
        "Then, upload your file",
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
          tableOutput("theImpactsInDetail"),
          "the user category names",
          textOutput("theUserCategoryNames"),
          "the BEA category names",
          textOutput("theBeaCategoryNames"),
          tableOutput("theUserCategoryChartData"),
          "the user category impact chart title",
          textOutput("theUserCategoryImpactChartTitle")
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
          radioButtons(
            inputId = "impactCategoryChoice",
            label = "choose an impact category",
            choices=impactCategoryNames,
            selected=impactCategoryNames[1]
          ),
          checkboxGroupInput(
            inputId="gwpVersionChoice",
            label="choose GWP version(s)",
            choices=gwpVersionNames,
            selected=gwpVersionNames[1]
          )
        ), # close sidebar panel
        mainPanel(
          width=9,
          fluidRow(
            column(
              width=6,
              plotOutput("theUserCategorySpendingChart")
            ),
            column(
              width=6,
              plotOutput("theUserCategoryImpactChart")
            )
          ) # close fluidrow
        ) #close mainpanel for sidebar layout
      ) # close sidebarlayout
    ) #close tabPanel for  page
  ) #close navbarmenu for  section
) # close navbarPage
# end user interface definition

# SERVER LOGIC
server <- function(input, output) {

  output$statFile <- downloadHandler(
    filename="spending_file_template.xlsx",  # desired file name on client
    content=function(con) {
      file.copy("users_spending_data_blank.xlsx", con)
    }
  )

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
    req(myUploadedData())
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
    impactsInDetail()
  })

  userCategoryNames <- reactive({
    impactsInDetail() %>%
    group_by(userCategory) %>%
    summarise(actualSpending=sum(actualSpending)) %>%
    ungroup() %>%
    arrange(desc(actualSpending)) %>%
    pull(userCategory)
  })

  output$theUserCategoryNames <- renderText(userCategoryNames())

  beaCategoryNames <- reactive({
    impactsInDetail() %>%
    group_by(beaCode, beaName) %>%
    summarise(actualSpending=sum(actualSpending)) %>%
    ungroup() %>%
    arrange(desc(actualSpending)) %>%
    pull(beaName)
  })

  output$theBeaCategoryNames <- renderText(beaCategoryNames())

  # data for summary charts by user category
  userCategoryChartData <- reactive({
    impactsInDetail() %>%
    filter(
      # later to be a selection in app...
      impactCategory==input$impactCategoryChoice,
      # later to be a multi checkbox selection in app...
      gwpVersion %in% input$gwpVersionChoice
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
        factor(gwpVersion, levels=rev(gwpVersionNames))
    )
  })

  output$theUserCategoryChartData <-
    renderTable(userCategoryChartData())

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

  output$theUserCategoryImpactChartTitle <-
    renderText(userCategoryImpactChartTitle())

  # a chart object that is later going to be reactive
  userCategorySpendingChartObject <- reactive({
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
    renderPlot(userCategorySpendingChartObject(), height=600)

  # a chart object that is later going to be reactive
  userCategoryImpactChartObject <- reactive({
    ggplot()+
    theme_539()+
    ggtitle(userCategoryImpactChartTitle())+
    geom_bar(
      data=userCategoryChartData(),
      aes(x=userCategory, y=impact, fill=gwpVersion),
      position="dodge",
      stat="identity"
    )+
    coord_flip()+
    scale_fill_viridis(
      begin=0.32, end=1, discrete=T, direction=-1,
      option=sample(LETTERS[1:8],1),
      guide = guide_legend(reverse = T)
    )
  })

  output$theUserCategoryImpactChart <-
    renderPlot(userCategoryImpactChartObject(), height=600)

}

# Run the application
shinyApp(ui = ui, server = server)
