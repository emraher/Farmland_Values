## ui.R ##
library(shinydashboard)
library(leaflet)
library(RColorBrewer)


dashboardPage(
  title = "Estimating Farmland Values Based on Historic Index Numbers",
  skin = "purple",
  # HEADER ---------------------------------------------------------------------
  dashboardHeader(title = "K-State / AgManager",
                  titleWidth = 250,
                  disable = FALSE,
                  # DROPDOWN MENU ----------------------------------------------
                  dropdownMenu(type = "messages",
                               messageItem("Support", "eremrah@ksu.edu")
                               )
                  ),
  # SIDEBAR --------------------------------------------------------------------
  # dashboardSidebar(
  #   width = 250,
  #   sidebarMenu(
  #     # MENU ITEM (PLOT)--------------------------------------------------------
  #     menuItem("Plot", tabName = "plot", icon = icon("line-chart")),
  #     # MENU ITEM (CALCULATE) --------------------------------------------------
  #     menuItem("Calculate", tabName = "calculate", icon = icon("calculator")),
  #     # MENU ITEM (MAP) --------------------------------------------------------
  #     menuItem("Map", tabName = "map", icon = icon("map")),
  #     # MENU ITEM (DOWNLOAD) ---------------------------------------------------
  #     menuItem("Download", tabName = "download", icon = icon("download"))
  #   )
  # ),
  dashboardSidebar(
  tags$head(
    tags$script(
      HTML(
        "
        $(document).ready(function(){
        // Bind classes to menu items, easiet to fill in manually
        var ids = ['subItemOne','subItemTwo','subItemThree','subItemFour'];
        for(i=0; i<ids.length; i++){
        $('a[data-value='+ids[i]+']').addClass('my_subitem_class');
        }

        // Register click handeler
        $('.my_subitem_class').on('click',function(){
        // Unactive menuSubItems
        $('.my_subitem_class').parent().removeClass('active');
        })
        })
        "
      )
    )
    ),
  width = 300,
  sidebarMenu(
    HTML('<h5 style="color:#625DA3;"><strong>About this page</strong>'),
    h6("This is a", a("Shiny", href = "http://shiny.rstudio.com/"), "app.", "Use sidebar panels to plot,"),
    h6("calculate, map, and download farmland values."),
    HTML('<h6 style="color:#625DA3;"><strong>Created by <a href="mailto:eremrah@ksu.edu?Subject=About%20Estimating%20Farmland%20Values%20Based%20on%20Historic%20Index%20Numbers" target="_top" style="color:#58FAF4;">Emrah Er (Kansas State University)</a></h6></strong>'),
    # MENU ITEM (DESCRIPTION)--------------------------------------------------------
    menuItem("Description", tabName = "description", icon = icon("info-circle")),
    # MENU ITEM (PLOT)--------------------------------------------------------
    menuItem("Plot Farmland Values", tabName = "plot", icon = icon("line-chart")),
    # MENU ITEM (CALCULATE) --------------------------------------------------
    menuItem('Calculate Estimated Farmland Values', tabName = 'calculate', icon = icon('calculator'),
             collapsible =
               menuSubItem(HTML('<br>Estimate farmland value for a past year'), tabName = 'CalcsubItemOne'),
               menuSubItem(HTML('<br>Estimate current farmland value based <br> on the value in a past year'), tabName = 'CalcsubItemTwo')
    ),
    # MENU ITEM (MAP) --------------------------------------------------------
    menuItem("Map Farmland Values", tabName = "map", icon = icon("map")),
    # MENU ITEM (DOWNLOAD) ---------------------------------------------------
    menuItem("Download Data", tabName = "download", icon = icon("download"))
    )
),
  # DASHBOARD BODY -------------------------------------------------------------
dashboardBody(
  tabItems(
    # TAB CONTENT (Description) ---------------------------------------------------
    tabItem(tabName = "description",
h2("Estimating Farmland Values Based on Historic Index Numbers", align = "center"),
h3("August 2015", align = "center"),
br(),
HTML('<h4 align="center"><strong><a href="mailto:twgriffin@ksu.edu?Subject=About%20Estimating%20Farmland%20Values%20Based%20on%20Historic%20Index%20Numbers" target="_top">Terry W. Griffin</a> (Kansas State University)</h4></strong>'),
HTML('<h4 align="center"><strong><a href="mailto:mtaylor@ksu.edu?Subject=About%20Estimating%20Farmland%20Values%20Based%20on%20Historic%20Index%20Numbers" target="_top">Mykel R. Taylor</a> (Kansas State University)</h4></strong>'),
hr(),
h4("Kansas farmland values have generally increased over time, with similar growth patterns occurring in surrounding states. A notable exception to the upward trend occurred during the 1980’s when farmland values decreased by 40% between 1982 and 1987. However, this decrease during the 1980’s followed a more than 200% increase in farmland values during the 1970’s.  Land values in Kansas did not return to the highest level observed in 1982 until twenty years later in 2001. While farmland values generally increased over time, the rate of change varies from year to year.   Kansas farmland values have increased an average of 7% each year since 1992. Annual growth rates have ranged from a low of -1% in 2009 to a high of nearly 22% in 2012."),
h4("Historic farmland values are of interest to farmers, landowners, lenders, and policy makers.  The index of Kansas farmland values in Table 1 can be used to estimate farmland value for a given year using a known value for the farmland from another year.  In other words, if the value of farmland is known in any year, a value can be estimated for any other year since 1950 with the farmland value index.  A guide describing how to use the Kansas farmland value index numbers accompanies Table 1. Index numbers were calculated based upon the year 1982 (index value for 1982 equals 100), which was the year of highest farmland values in the 1980’s."),
h4("Index numbers are based on nominal values and are not adjusted for inflation or purchasing power. Although index numbers are useful for estimating the value of Kansas farmland relative to another point in time, the index provides only a single piece of information to include in any decision making process."),
h4("Estimated farmland values do not substitute for land appraisals and may deviate from true market value for any number of reasons including land improvements, buildings and facilities, pressure from development and urban sprawl, mineral rights, and previous farm production management practices."),
h4(strong("Resources:")),
h4("USDA NASS. QuickStats.", a("http://www.nass.usda.gov/Quick_Stats/", href = "http://www.nass.usda.gov/Quick_Stats/")),
h4("USDA NASS. Land Values 2015 Summary, August 2015 ISSN: 1949-1867"),
h4(strong("See Also:")),
h4(a("Estimating Arkansas Farmland Values Based on Historic Index Numbers", href = "https://www.uaex.edu/publications/PDF/FSA-35.pdf"))
    ),
    # TAB CONTENT (PLOT) ---------------------------------------------------
    tabItem(tabName = "plot",
            # CHOOSE YEAR - CHOOSE STATE
            fluidRow(
              column(6, align = "center",
                     sliderInput("dateRange",
                                 label = strong("Choose a Date Range"),
                                 min = 1950, max = 2015,
                                 value = c(1950, 2015), sep = "")),
              column(6, align = "center",
                     selectInput("stateInput",
                                 label = strong("Choose a State"),
                                 choices = c("ALABAMA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "FLORIDA", "GEORGIA", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING"),
                                 selected = "KANSAS")
              )
            ),
            # PLOT DATA
            plotOutput('indexPlot')
    ),

    # TAB CONTENT (CALCULATE) --------------------------------------------------
    tabItem(tabName = "CalcsubItemOne",
            fluidRow(
              box(title = "Description", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                  "The index for farmland values can be used to estimate land value for a past year, or estimate the value of land in the current year, depending upon available information. If the value of farmland is known in any year, the value can be estimated for any other year since 1950 using the farmland value index."
              ),
              box(title = "Example", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                  HTML("The farmland value for a past year can be estimated using the following formula:<br>"),
                  withMathJax("$$Farmland~V._{Past~Year} = Farmland~V._{Current~Year} * \\frac{Index~V._{Past~Year}}{Index~V._{Current~Year}}$$"),
                  HTML("<strong>Example:<br>
Current land values are $2,000 per acre in KS. What was the approximate value of this land in 1987?</strong><br>
Using the results above, the index values for <strong>1987</strong> and <strong>2014</strong> are <strong>59.39</strong> and <strong>326.43</strong>, respectively."),
                  withMathJax("$$Estimated~1987~Farmland~Value = 2,000 * \\frac{59.39}{326.43}$$"),
                  HTML("The estimated value of the farmland in 1987 is <strong>$364 per acre</strong> ")
              )
            ),
            fluidRow(
                box(title = "Enter Values", status = "warning", solidHeader = TRUE, height = 425,
                  # USER INPUTS 1
                  numericInput("indexYear1", label = ("Base Year (Used for Index Calculation)"), value = 1982, min = 1950, max = 2015, step = 1),
                  selectInput("stateInput1",
                              label = strong("Choose a State"),
                              choices = c("ALABAMA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "FLORIDA", "GEORGIA", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING"),
                              selected = "KANSAS"),
                  numericInput("past1", label = ("Past Year"), value = 1987, min = 1950, max = 2015, step = 1),
                  numericInput("current1", label = ("Current Year"), value = 2014, min = 1950, max = 2015, step = 1),
                  numericInput("valuecurrent1", label = ("Current Farmland Value"), value = 2000)
                ),
                box(title = "Results", status = "success", solidHeader = TRUE, height = 425,
                  # CALCULATIONS 1
                  strong("Index Value from Past Year"), verbatimTextOutput("IndexPast1"),
                  strong("Index Value from Current Year"),verbatimTextOutput("IndexCurrent1"),
                  strong("Estimated Farmland Value from Past Year"),verbatimTextOutput("estpast")
                )
            )

    ),
    tabItem(tabName = "CalcsubItemTwo",
            fluidRow(
              box(title = "Description", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                  "The index for farmland values can be used to estimate land value for a past year, or estimate the value of land in the current year, depending upon available information. If the value of farmland is known in any year, the value can be estimated for any other year since 1950 using the farmland value index."
              ),
              box(title = "Example", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                  HTML("Current farmland values can be estimated using past values with the following formula:<br>"),
                  withMathJax("$$Farmland~V._{Current~Year} = Farmland~V._{Past~Year} * \\frac{Index~V._{Current~Year}}{Index~V._{Past~Year}}$$"),
                  HTML("<strong>Example:<br>
                      My relative paid $225 per acre for land in 1969 in KS. What is the estimated value of this land today?</strong><br>
                       Using the results above, the index values for <strong>1969</strong> and <strong>2014</strong> are <strong>25.80</strong> and <strong>326.43</strong>, respectively."),
                  withMathJax("$$Estimated~2014~Farmland~Value = 225 * \\frac{326.43}{25.80}$$"),
                  HTML("The estimated value of the farmland in 1987 is <strong>$2,847 per acre</strong> ")
                  )
              ),
            fluidRow(
              box(title = "Enter Values", status = "warning", solidHeader = TRUE, height = 425,
                  # USER INPUTS 2
                  numericInput("indexYear2", label = ("Base Year (Used for Index Calculation)"), value = 1982, min = 1950, max = 2015, step = 1),
                  selectInput("stateInput2",
                              label = strong("Choose a State"),
                              choices = c("ALABAMA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "FLORIDA", "GEORGIA", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING"),
                              selected = "KANSAS"),
                  numericInput("past2", label = ("Past Year"), value = 1969, min = 1950, max = 2015, step = 1),
                  numericInput("current2", label = ("Current Year"), value = 2014, min = 1950, max = 2015, step = 1),
                  numericInput("valuepast", label = ("Farmland Value from Past Year"), value = 225)
              ),
              box(title = "Results", status = "success", solidHeader = TRUE, height = 425,
                  # CALCULATIONS 2
                  strong("Index Value from Past Year"), verbatimTextOutput("IndexPast2"),
                  strong("Index Value from Current Year"),verbatimTextOutput("IndexCurrent2"),
                  strong("Estimated Current Farmland Value"),verbatimTextOutput("estcurrent")
              )
            )
    ),

    # TAB CONTENT (MAP) --------------------------------------------------------
    tabItem(tabName = "map",
            tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
            leafletOutput("map"),
            absolutePanel(top = 480, right = 10,
            sliderInput("mapyear", "Choose Year to Plot", min = 1950, max = 2015, value = 2015, step = 1, sep = ""),
            selectInput("colors", "Color Scheme", c("Purples", "Reds", "Blues", "Greens", "Greys")),
            checkboxInput("legend", "Show legend", TRUE)),
            br(),
            box(title = "Description", status = "info", solidHeader = TRUE,
"The map plots the Farmland Values in Dollars per Acre for a given year. Select a year from the right hand side. You can also change colors of the map. To view or hide the map legend use the 'Show Legend' checkbox."
            )

    ),

    # TAB CONTENT (DOWNLOAD) ---------------------------------------------------
    tabItem(tabName = "download",
           fluidRow(
             box(
               title = "Choose State for Data and Index", status = "info", solidHeader = TRUE,
               column(12, align = "center",
                      selectInput("stateInputDown",
                                  label = strong("Choose a State"),
                                  choices = c("ALABAMA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "FLORIDA", "GEORGIA", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING"),
                                  selected = "KANSAS")
               )
             )
           ),
           fluidRow(
             box(
               title = "Choose Base Year for Index", status = "info", solidHeader = TRUE,
               column(12, align = "center",
                      numericInput("Index", label = ("Base Year"), value = 1982, min = 1950, max = 2015, step = 1)
               )
             )
           ),
           fluidRow(
             box(
               title = "Download Data", status = "success", solidHeader = TRUE,
               column(12, align = "center",
                      downloadButton('downloadData', 'Download Data (All States)'),
                      hr(),
                      downloadButton('downloadState', 'Download Data (Selected State)'))
             )
           ),
            fluidRow(
              box(
                title = "Download Index Data", status = "warning", solidHeader = TRUE,
                column(12, align = "center",
                       downloadButton('downloadIndex', 'Download Index (Selected State)'))
              )
            )
    )
  )
)
)
