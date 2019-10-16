#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)

# Define UI for application that draws a histogram
dashboardPage(
    dashboardHeader(title = h4(HTML("Visualizing NYC <br/>Gentrification"))),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Visualizing Development", tabName ="develop", icon = icon("map")),
        menuItem(HTML("Taxpayer Data <br/>by ZIP Code"), tabName ="taxer", icon = icon("chart-line")),
        menuItem(HTML("Business Data <br/>by ZIP Code"), tabName ="buss", icon = icon("chart-line")),
        menuItem(HTML("Employee Data <br/>by ZIP Code"), tabName ="employee", icon = icon("chart-line")),
        
        menuItem("Variable Definitions", tabName ="variable_def", icon = icon("book")),
        menuItem(HTML("About the Author <br/>(Kyle D. Weber)"), tabName ="author_bio", icon = icon("book"))
        )
      ),
    dashboardBody(
      tabItems(tabItem(tabName ="develop",
                         fluidRow(
                           leafletOutput("my_item"), height = "100%"),
                         box(
                           status = "primary", solidHeader = F,
                           collapsible = F,
                           splitLayout(
                             selectInput("geography", "Geographies to Use", c("Neighborhoods" = "neighbor", "Census Tracts" = "tracts", "ZIP Codes" = "zips"), selectize = FALSE),
                             selectInput("data_type", "Data to Show", c("New Structures" = "ns", "Structures Demolished" = "dem",
                                                                        "Structures Converted" = "cs"), selectize = FALSE),
                             selectInput("lrg_developers", "Construction Projects to Include", c("All Projects" = "ALL", "Residential" = "NO", "Non-Residential" = "YES"), selectize = FALSE),
                             selectInput("project_assignment", "Assign Projects to Year...", c("Submitted" = "filed_year", "Approved" = "issued_year", "Construction Began" = "start_year"), selectize = FALSE),                             
                             selectInput("typpe", "Shade Areas Based On", c("Quantiles" = "quant", "Raw Values" = "vals"), selectize = FALSE)
                           ),
                           splitLayout(
                             sliderInput("slider", "Year", 2004, min = 2004, max = 2019, step = 1, sep = "")
                             )
                         
                         , width = "100%", height = "25%")),        
               tabItem(tabName ="taxer",
                       fluidRow(
                         splitLayout(cellWidths = c("100%"), plotOutput("population"))
                         ),
                       box(
                         status = "primary", solidHeader = F,
                         collapsible = F,
                         splitLayout(
                           selectInput("specific_zip_taxer", "Zip Code:", sort(unique(income_data$zipcode)), selectize = FALSE)
                         ), width = 12)),
               tabItem(tabName ="buss",
                       fluidRow(
                         splitLayout(cellWidths = c("50%", "50%"), plotOutput("industry_breakdown"), plotOutput("large_industry_breakdown"))
                       ),
                       box(
                         status = "primary", solidHeader = F,
                         collapsible = F,
                         splitLayout(
                           selectInput("specific_zip_buss", "Zip Code:", sort(unique(income_data$zipcode)), selectize = FALSE),
                           selectInput("bus_display", "Business to Display", c("All Businesses" = "total", "Construction" = "construction", "Manufacturing" = "manufacturing", "Wholesale" = "wholesale", "Retail" = "retail", "Logistics" = "logistics", "White Collar" = "professional", "Education" = "education", "Health Care" = "health_care", "Other Services" = "services", "Government" = "government", "Entertainment" = "entertainment"), selectize = FALSE)
                         ), width = 12)),
               tabItem(tabName ="employee",
                       fluidRow(
                         splitLayout(cellWidths = c("50%", "50%"), plotOutput("total_employed"), plotOutput("total_payroll"))
                       ),
                       box(
                         status = "primary", solidHeader = F,
                         collapsible = F,
                         splitLayout(
                           selectInput("specific_zip_employee", "Zip Code:", sort(unique(income_data$zipcode)), selectize = FALSE)
                         ), width = 12)),
               tabItem(tabName ="variable_def",
                       fluidRow(
                         h2("Variable Information", style="text-align: center;"),
                         HTML('<p style="padding:20px;"><strong>Permit Information</strong>: Permit information was provided by the NYC Department of Building permit issuance dataset. It was cleaned by getting rid of extraneous categorical information, eliminating permits that were issued for minor construction jobs or for construction jobs that did not seem to be done to increase the value of the building. While this latter category is obviously subjective, I focused on eliminating jobs that involved elements of a building that most occupants would not notice (e.g., elevator shaft maintenance, re-doing electrical wiring, and other tasks of this nature). I also excluded permits for work that was never completed owing to the relevant permit not being issued or being revoked. The files used in this portion of the analysis were downloaded on 10/14/19 from <a href="https://data.cityofnewyork.us/Housing-Development/DOB-Permit-Issuance/ipu4-2q9a">this link</a>.</p>'),
                         HTML('<p style="padding:20px;"><strong>ZIP-Code Level Income:&nbsp; </strong>Income information comes from aggregated information about tax returns by ZIP code provided through the National Bureau of Economic Research. I primarily modified these files by adjusting the relevant figures for inflation and aggregating certain information across income groups. There are two different income measurements I used in my analysis, each with their own benefits and costs. The measure that includes wages and salary income excludes business income, investment income, and a number of other measurements that may reflect the well-being of the individuals living in an area. On the other hand, my aggregate income measurement is based on taxable income and includes several adjustments for businesses losses that may lead individuals to have low reported income even if their standard of living is high. One caveat that is absolutely crucial to note is that the IRS data is based on reported mailing address, which means that tax return data may not reflect the local standard of living if there is variation across areas in the prevalence of PO boxes. The files used in this analysis were downloaded on 10/14/19 from <a href="https://www.nber.org/data/soi-tax-stats-individual-income-tax-statistics-zip-code-data-soi.html">this link</a>.</p>'),
                         HTML('<p style="padding:20px;"><strong>Industry-Level Data:&nbsp; </strong>Information about employment in specific ZIP codes by industry is provided from the County Business Patterns data set constructed by the Census Bureau. This data set includes information about employment in different industries, the number of establishments in each industry, and total yearly payroll amounts across ZIP codes for different NAICS industry categories. I aggregated all my figures to 2-digit NAICS categories to make sure that I include information from the broadest array of ZIP codes possible, as certain ZIP codes do not report information about smaller collections of industries owing to data anonymization techniques used by the Census. The files used in this analysis were downloaded on 10/14/19 from <a href="https://www.census.gov/programs-surveys/cbp/data/datasets.html">this link</a>.<strong><br /></strong></p>')
                       )
               ),
               tabItem(tabName ="author_bio",
                       fluidRow(
                         h2("About Me", style="text-align: center;"),
                         p("I am a former Columbia Economics PhD candidate (with two Masters degrees) with extensive quantitative research experience in statistics, data cleaning and manipulation, and analytics.", style="padding:20px;"),
                         p("I have co-authored four published papers and one NBER working paper on financial forecasting and quantitative criminology, which use both time-series analysis and panel data techniques.", style="padding:20px;"),
                         p("I have extensive and detailed knowledge of econometric analysis, statistics, and various data management solutions and statistical software packages (R, SQL, Stata, Matlab, and Excel).", style="padding:20px;"),
                         p("I am currenly attending a 12-week data science boot camp at the NYC Data Science Academy to gain additional experience with Python, machine learning techniques, and database management software such as Spark, Hadoop, and Hive.", style="padding:20px;"),
                         HTML('<p style="padding:20px;">You can find additional information about my background and current projects on my <a href="https://www.linkedin.com/in/kyle-david-weber/">LinkedIn page</a> and on my <a href = "https://github.com/kdw2126">Github profile page</a>.</p>.')
                       )
               )
      )
))