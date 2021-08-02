# SETUP
# load packages
library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(viridis)
library(ggthemes)
library(readxl)
library(openxlsx)

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
    title="Upload spending data",
    sidebarLayout(
      sidebarPanel(
        width=3,
        h3("Instructions"),
        fluidRow("1. enter your spending in a file like this:"),
        fluidRow(" "),
        fluidRow(
          downloadButton(
            "statFile",
            "download Excel template"
          ),
        ),
        fluidRow(" "),
        fluidRow("2. upload your spending file"),
        fluidRow(" "),
        fileInput(
          "file1",
          "Choose XLSX or XLSX file",
          accept = c(".xls", ".xlsx")
        ),
        fluidRow(" "),
        fluidRow("3. check for noncompatible codes and sum of spending at right"),
        fluidRow(" "),
        fluidRow("4. correct spending data file if necessary"),
        fluidRow(" "),
        fluidRow("5. when satisfied with spending file, move on to results")
      ),
      mainPanel(
        width=9,
        title = "Views of your uploaded data",
        fluidRow(
          align="center",
          h3("a sample of your uploadedData:"),
          tableOutput("theUploadedData"),
          h3("the sum of spending with compatible codes:"),
          textOutput("theTotalUploadedSpending"),
          h3("lines that have noncompatible codes:"),
          tableOutput("theSuspectData")
        )
      )
    ) # close sidebarlayout for introduction page
  ), #close introduction page

  # lay out weights and impacts section
  navbarMenu(
    title="Spending and impact results by user category",

    # lay out totals by material page
    tabPanel(
      title="Total spending and impacts",
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
          ),
          fluidRow(
            downloadButton(
              outputId = "theUserCategorySpendingChartDL",
              label = "download spending chart",
              width = "100%"
            )
          ),
          fluidRow(
            downloadButton(
              outputId = "theUserCategoryImpactChartDL",
              label = "download impact chart",
              width = "100%"
            )
          ),
          fluidRow(
            downloadButton(
              outputId = "theDataDownload",
              label = "download chart and source data"
            )
          )
        ), # close sidebar panel
        mainPanel(
          width=9,
          fluidRow(
            column(
              width=6,
              fluidRow(plotOutput("theUserCategorySpendingChart"))
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

  # allow user to download a static Excel file that they
  # can use as a template for their own data entry
  output$statFile <- downloadHandler(
    filename="spending_file_template.xlsx",  # desired file name on client
    content=function(con) {
      file.copy("users_spending_data_blank.xlsx", con)
    }
  )

  # allow user to upload an excel file with their own
  # spending data
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

  # create a screen display of the user's uploaded data
  output$theUploadedData <- renderTable({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(
      need(
        ext == "xls" | ext=="xlsx",
        "Please upload an Excel file in the format of the template"
      )
    )
    myUploadedData() %>% head(5)
  })

  suspectRecords <- reactive({
    req(myUploadedData())
    anti_join(
      myUploadedData(),
      impactIntensitiesWithGWPversions,
      by="beaCode"
    )
  })

  output$theSuspectData <- renderTable({
    suspectRecords()
  })

  totalUploadedSpending <- reactive({
    req(myUploadedData())
    semi_join(
      myUploadedData(),
      impactIntensitiesWithGWPversions,
      by="beaCode"
    ) %>%
    summarise(userSpending=sum(userSpending)) %>%
    pull()
  })

  output$theTotalUploadedSpending <-
    renderText(totalUploadedSpending())

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

  # make an image of the chart downloadable
  output$theUserCategorySpendingChartDL <-
    downloadHandler(
      filename = "userCategorySpendingChartDL.png",
      content = function(file) {
        ggsave(
          file,
          plot = userCategorySpendingChartObject(),
          device="png"
        )
      }
    )


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

  # output that chart
  output$theUserCategoryImpactChart <-
    renderPlot(userCategoryImpactChartObject(), height=600)

  # make an image of the chart downloadable
  output$theUserCategoryImpactChartDL <-
    downloadHandler(
      filename = "userCategoryImpactChartDL.png",
      content = function(file) {
        ggsave(
          file,
          plot = userCategoryImpactChartObject(),
          device="png"
        )
      }
    )

  output$theDataDownload <-
    downloadHandler(
      filename = function() {
        paste("spendingAndImpacts", ".xlsx", sep = "")
      },
      content = function(file) {
        myWorkbook <-
          createWorkbook()
        addWorksheet(
          wb = myWorkbook, sheetName = "submitted spending"
        )
        writeDataTable(
          wb = myWorkbook,
          sheet = "submitted spending",
          x = myUploadedData()
        )
        addWorksheet(
          wb=myWorkbook, sheetName = "noncompatible records"
        )
        writeDataTable(
          wb = myWorkbook,
          sheet = "noncompatible records",
          x = suspectRecords()
        )
        addWorksheet(
          wb = myWorkbook, sheetName = "totals by user category"
        )
        writeDataTable(
          wb = myWorkbook,
          sheet = "totals by user category",
          x = userCategoryChartData()
        )
        addWorksheet(
          wb=myWorkbook, sheetName = "complete detail"
        )
        writeDataTable(
          wb= myWorkbook,
          sheet = "complete detail",
          x = impactsInDetail()
        )
        saveWorkbook(
          wb = myWorkbook,
          file = file
        )
      }
    )

}

# Run the application
shinyApp(ui = ui, server = server)
