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

# Define server logic required to draw a histogram
shinyServer(function(input, output){


  selected <- reactive({
    tmp = permit_information_tract %>%
      filter(Year == input$slider) %>%
      filter(Residential == input$lrg_developers) %>%
      filter(year_type == input$project_assignment) %>%
      filter(outcome_variable == input$data_type) %>%
      mutate(NAME = as.character(NAME)) %>%
      group_by(NAME) %>%
      mutate(outcome = sum(outcome)) %>%
      select(NAME, Year, Residential, year_type, outcome_variable, outcome) %>%
      ungroup()
    tmp
  })

    output$my_item <- renderLeaflet({
        leaflet() %>%
        setView(lat = 40.752819, lng = -73.992370, zoom = 15) %>%
        addProviderTiles(providers$CartoDB.Positron)
    })
    

    observe({
        nyc_tracts@data <- left_join(nyc_tracts@data, selected(), by="NAME")
        pal <- colorNumeric("Blues", nyc_tracts@data$outcome)
        leafletProxy("my_item", data = nyc_tracts) %>%
          addTiles() %>% 
          clearShapes() %>% 
          clearControls() %>% 
          addPolygons(data = nyc_tracts, fillColor = ~pal(outcome), popup = ~outcome, fillOpacity = 0.7, 
                      color = "white", weight = 2) %>%
          addLegend(position = "bottomright",
                    pal = pal, values = ~outcome)
      })

    
    output$population <-renderPlot(
        income_data %>%
                filter(zipcode == as.numeric(input$specific_zip)) %>%
                select(year, starts_with("n1")) %>%
                pivot_longer(-year, values_to = "value", names_to = "value_label") %>%
                ggplot(aes(x = year, y = value, color = value_label)) + geom_line() + scale_color_discrete(name = "Groups", labels = c("Under $25K", "$25K to Under $50K", "$50K to Under $75K", "$75K to Under $100K", "$100K to Under $200K", "Over $200K")) + ggtitle("Number of Households in Each Income Group")  + xlab("Year") +  ylab("Number of Households by Group") + theme_few() + theme(plot.title = element_text(hjust = 0.5))
        )
    
    output$industry_breakdown <-renderPlot(
        industry_data %>%
                filter(zip == as.numeric(input$specific_zip)) %>%
                select(year, starts_with(input$bus_display)) %>%
                select(year, contains("est")) %>%
                pivot_longer(-year, values_to = "value", names_to = "value_label") %>%
                ggplot(aes(x = year, y = value, color = value_label)) + geom_line() + ggtitle("Businesses in Given Zip Code and Industry") + xlab("Year") + ylab("Number of Businesses") + scale_color_discrete(guide=FALSE) + theme_few() + scale_fill_few() + theme(plot.title = element_text(hjust = 0.5))
    )
})