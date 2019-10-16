#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(htmltools)

# Define server logic required to draw a histogram
shinyServer(function(input, output){
  selected <- reactive({
    if (input$geography == "tracts") {
      tmp = permit_information_tract %>%
        filter(Year == input$slider) %>%
        filter(Residential != input$lrg_developers) %>%
        filter(year_type == input$project_assignment) %>%
        filter(outcome_variable == input$data_type) %>%
        mutate(NAME = as.character(NAME)) %>%
        group_by(NAME, COUNTYFP) %>%
        select(NAME, COUNTYFP, outcome, Residential) %>%
        distinct %>%
        summarise(outcome = sum(outcome)) %>%
        ungroup() %>%
        mutate(outcome = ifelse(outcome == 0, NA, outcome)) %>%
        mutate(NAME = as.character(NAME))
      tmp
    } else if (input$geography == "zips") {
      tmp = permit_information_zips %>%
        filter(Year == input$slider) %>%
        filter(Residential != input$lrg_developers) %>%
        filter(year_type == input$project_assignment) %>%
        filter(outcome_variable == input$data_type) %>%
        mutate(ZCTA5CE10 = as.character(ZCTA5CE10)) %>%
        group_by(ZCTA5CE10) %>%
        select(ZCTA5CE10, outcome, Residential) %>%
        distinct %>%
        summarise(outcome = sum(outcome)) %>%
        ungroup() %>%
        mutate(outcome = ifelse(outcome == 0, NA, outcome)) %>%
        mutate(ZCTA5CE10 = as.character(ZCTA5CE10))
      tmp
    } else {
      tmp = permit_information_ntas %>%
        filter(Year == input$slider) %>%
        filter(Residential != input$lrg_developers) %>%
        filter(year_type == input$project_assignment) %>%
        filter(outcome_variable == input$data_type) %>%
        mutate(ntaname = as.character(ntaname)) %>%
        group_by(ntaname) %>%
        select(ntaname, outcome, Residential) %>%
        distinct %>%
        summarise(outcome = sum(outcome)) %>%
        ungroup() %>%
        mutate(outcome = ifelse(outcome == 0, NA, outcome)) %>%
        mutate(ntaname = as.character(ntaname))
      tmp
    }
  })
  
  data_set <- reactive({
    if (input$geography == "tracts") {
      nyc_tracts
    } else if (input$geography == "zips") {
      nyc_zipcodes
      } else {
      nyc_neighborhoods
    }
  })
  
  type_to_use <- reactive({
    if (input$typpe == "quant") {
      "quants"
    } else if (input$typpe == "vals") {
      "valls"
    }
  })
  
  output$my_item <- renderLeaflet({
    leaflet() %>%
      setView(lat = 40.752819, lng = -73.992370, zoom = 15) %>%
      addProviderTiles(providers$CartoDB.VoyagerNoLabels)
    })
  
  
  observe({
    nyc_tracts2 = data_set()
    selected_data = selected()
    type_to_use2 = type_to_use()
    
    if (!(is.null(nyc_tracts2@data$NAME))) {
      nyc_tracts2@data = nyc_tracts2@data %>%
        mutate(NAME = as.character(NAME)) %>%
        mutate(COUNTYFP = as.character(COUNTYFP))
    } else if (!(is.null(nyc_tracts2@data$ZCTA5CE10))) {
      nyc_tracts2@data = nyc_tracts2@data %>%
        mutate(ZCTA5CE10 = as.character(ZCTA5CE10))
    } else if (!(is.null(nyc_tracts2@data$ntaname))) {
      nyc_tracts2@data = nyc_tracts2@data %>%
        mutate(ntaname = as.character(ntaname))
    }
    nyc_tracts2@data <- left_join(nyc_tracts2@data, selected_data)

    if (type_to_use2 == "valls") {
      pal <- colorNumeric("Reds", nyc_tracts2@data$outcome)
    } else if (type_to_use2 == "quants") {
      pal <- colorQuantile("Reds", nyc_tracts2@data$outcome)
    }
    
    leafletProxy("my_item", data = nyc_tracts2) %>%
      addTiles() %>%
      clearShapes() %>%
      clearControls()  %>%
      addPolygons(data = nyc_tracts2, fillColor = ~pal(outcome),
                  fillOpacity = 0.85,
                  color = "black", weight = 2) %>%
      addLegend(position = "bottomright",
                pal = pal, values = ~outcome, na.label = "0", title = "Legend")

  })
  
  
  output$population <-renderPlot(
    income_data %>%
      filter(zipcode == as.numeric(input$specific_zip_taxer)) %>%
      select(year, starts_with("n1")) %>%
      pivot_longer(-year, values_to = "value", names_to = "value_label") %>%
      filter(!(is.na(value_label))) %>%
      ggplot(aes(x = year, y = value, color = value_label)) + geom_line() + scale_color_discrete(name = "Groups", labels = c("Under $25K", "$25K to Under $50K", "$50K to Under $75K", "$75K to Under $100K", "$100K to Under $200K", "Over $200K")) + ggtitle("Number of Tax-Paying Households in Each Income Group")  + xlab("Year") +  ylab("Number of Households by Group") + theme_few() + theme(plot.title = element_text(hjust = 0.5))
  )
  

  output$industry_breakdown <-renderPlot(
    industry_data %>%
      filter(zip == as.numeric(input$specific_zip_buss)) %>%
      select(year, starts_with(input$bus_display)) %>%
      select(year, contains("est")) %>%
      pivot_longer(-year, values_to = "value", names_to = "value_label") %>%
      ggplot(aes(x = year, y = value, color = value_label)) + geom_line() + ggtitle("Businesses in Given Zip Code and Industry") + xlab("Year") + ylab("Number of Businesses") + scale_color_discrete(guide=FALSE) + theme_few() + scale_fill_few() + theme(plot.title = element_text(hjust = 0.5))
  )
  
  output$large_industry_breakdown <-renderPlot(
    industry_data %>%
      filter(zip == as.numeric(input$specific_zip_buss)) %>%
      select(year, starts_with(input$bus_display)) %>%
      select(year, contains("large")) %>%
      pivot_longer(-year, values_to = "value", names_to = "value_label") %>%
      ggplot(aes(x = year, y = value, color = value_label)) + geom_line() + ggtitle("Large Businesses (>500 Employees) in Given Zip Code and Industry") + xlab("Year") + ylab("Number of Businesses") + scale_color_discrete(guide=FALSE) + theme_few() + scale_fill_few() + theme(plot.title = element_text(hjust = 0.5))
  )
  
  output$total_employed <-renderPlot(
    employment_info %>%
      filter(zip == as.numeric(input$specific_zip_employee)) %>%
      select(year, emp) %>%
      ggplot(aes(x = year, y = emp)) + geom_line() + scale_color_discrete(name = "Groups", labels = c("Total Employed By Local Businesses in Thousands")) + ggtitle("Variation in Number Employed Across Years")  + xlab("Year") +  ylab("Number Employed in Thousands") + theme_few() + theme(plot.title = element_text(hjust = 0.5))
  )
  
  output$total_payroll <-renderPlot(
    employment_info %>%
      filter(zip == as.numeric(input$specific_zip_employee)) %>%
      select(year, ap) %>%
      ggplot(aes(x = year, y = ap)) + geom_line() + scale_color_discrete(name = "Groups", labels = c("Total Payroll in 2015 Dollars")) + ggtitle("Total Income Earned by Employees of Companies Located in a Given ZIP")  + xlab("Year") +  ylab("Total Payroll in 2015 Dollars") + theme_few() + theme(plot.title = element_text(hjust = 0.5))
   )
  

})