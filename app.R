
source("00-Libraries.R")
source("01-FDA API.R")
source("02-Data Processing.R")

## Outline ##

# use tabsets

# 1st tab: Overall Breakdowns, Big Categories summarized
# 2nd tab: Foreign objects
# 3rd tab: Contaminants
# 4th tab: Food allergy
# 5th tab: Gluten

# Define UI

ui <- fluidPage(theme = shinytheme("sandstone"),
  h1(id="big-heading",tags$strong(HTML(paste0("FDA Food Recalls"))), align="center"),
  tags$style(HTML("#big-heading{color: #3C4142; background: #FAF7F1;}")), br(),
  
  sidebarLayout(
    sidebarPanel(hr(),
                 
                 useShinyjs(),
                 actionButton("Reset Filters", inputId = "Refresh"), hr(), br(),
                 
                 # dateRangeInput('report_date',
                 #                label = 'Select Date',
                 #                start = min(all_fda_data_clean$report_date),
                 #                end = max(all_fda_data_clean$report_date)),
                                
                 # sliderInput(inputId = "date",
                 #             label = "Select Date",
                 #             value = c(min(all_fda_data_clean$report_date),max(all_fda_data_clean$report_date)),
                 #             min = min(all_fda_data_clean$report_date),
                 #             max = max(all_fda_data_clean$report_date)),
                             
                 pickerInput(inputId = "report_year",
                             label = "Select Year",
                             multiple = TRUE,
                             choices = sort(unique(all_fda_data_clean$report_year)),
                             selected= sort(unique(all_fda_data_clean$report_year)),
                             options = list('actions-box'=TRUE)), hr(),
                 
                 # selectInput(inputId = "recalling_firm",
                 #             label="Select Recalling Firm",
                 #             choices=sort(unique(all_fda_data_clean$recalling_firm)),
                 #             selected= sort(unique(all_fda_data_clean$recalling_firm)), hr(),
                 
                 # selectInput(inputId = "recalling_firm",
                 #             label = "Select Recalling Firm",
                 #             multiple = TRUE,
                 #             choices = sort(unique(all_fda_data_clean$recalling_firm)),
                 #             selected= sort(unique(all_fda_data_clean$recalling_firm)),
                 #             selectize = FALSE
                 #             #size = 5,
                 #             #options = list('actions-box'=TRUE)
                 #             ), hr(),
                 
                 pickerInput(inputId = "broad_category",
                             label = "Select Reason for Recall",
                             multiple = TRUE,
                             choices = sort(unique(all_fda_data_clean$broad_category)),
                             selected= sort(unique(all_fda_data_clean$broad_category)),
                             options = list('actions-box'=TRUE)),
                 
                 pickerInput(inputId = "state",
                             label = "Select State",
                             multiple = TRUE,
                             choices = sort(unique(all_fda_data_clean$state)),
                             selected= sort(unique(all_fda_data_clean$state)),
                             options = list('actions-box'=TRUE)),
                 
                 pickerInput(inputId = "classification",
                             label = "Select Classification",
                             multiple = TRUE,
                             choices = sort(unique(all_fda_data_clean$classification)),
                             selected= sort(unique(all_fda_data_clean$classification)),
                             options = list('actions-box'=TRUE)),
                 
                 pickerInput(inputId = "initial_firm_notification",
                             label = "Select Notification",
                             multiple = TRUE,
                             choices = sort(unique(all_fda_data_clean$initial_firm_notification)),
                             selected= sort(unique(all_fda_data_clean$initial_firm_notification)),
                             options = list('actions-box'=TRUE)),
                 
                 
                 pickerInput(inputId = "status",
                             label = "Select Status",
                             multiple = TRUE,
                             choices = sort(unique(all_fda_data_clean$status)),
                             selected= sort(unique(all_fda_data_clean$status)),
                             options = list('actions-box'=TRUE)),

                 
                 br(), br(), br(),
                 h6("Created with data from OpenFDA"),
                 width = 3),
    
    mainPanel(
      tabsetPanel(
        
        
        tabPanel("Overall Recall Summary", br(),
                 
                 tabsetPanel(
                   
                   tabPanel("Reason for Recall", br(), h4("Breakdown of Recalls by Reason"),
                            billboarderOutput("recall_plot"),
                            ),
                   
                   tabPanel("Classification of Recall", br(), h4("Breakdown of Recall Classes"),
                            billboarderOutput("class_plot"),
                   ),
                   
                   tabPanel("Status of Recall", br(), h4("Breakdown of Recall Status"),
                            billboarderOutput("status_plot"),
                   ),
                   
                   tabPanel("Notification of Recall", br(), h4("Recall by Initial Firm Notification"),
                            billboarderOutput("notif_plot"),
                   ),
                   
                   tabPanel("Recalls Over Time", br(), h4("Recalls in the Last Five Years by Month"),
                            billboarderOutput("time_plot1"), br(),
                            h4("Recalls in the Last Five Years by Reason"),billboarderOutput("time_plot2"),
                            h4("Recalls in the Last Five Years by Classification"),billboarderOutput("time_plot3"),
                            h4("Recalls in the Last Five Years by Initial Firm Notification"),billboarderOutput("time_plot4")
                            ),
                   tabPanel("Firms with Most Recalls", br(), h4("Top 10 Recalling Firms"),
                            DT::dataTableOutput("table1")),
                   
                   tabPanel("Recalls by State",br(),
                            leafletOutput("map_1"))
                 )
            
            ),
        
        ## Allergens
        
        tabPanel("Undeclared Ingredients Summary", br(),
                 
                 tabsetPanel(
                   
                   tabPanel("Undeclared Ingredients", br(), h4("Breakdown of Ingredients"),
                            billboarderOutput("ingredients_plot1"), br(),
                            h4("Undeclared Ingredients Over Time"),
                            billboarderOutput("time_plot_ing")
                   ),
                 )
        )
        
        
        )
      )
    )
  )

# Define server logic
server <- function(input, output, session){
  
  observeEvent(input$Refresh, {
    refresh() })

  # Reactive Data Frame
  full_data <- reactive({
    
    shiny::validate(need(input$state != "" , "No state selected"))
    shiny::validate(need(input$classification != "" , "No classification selected"))
    #shiny::validate(need(input$recalling_firm != "" , "No recalling firm selected"))
    shiny::validate(need(input$status != "" , "No sample status selected"))
    shiny::validate(need(input$report_year != "" , "No year selected"))
    shiny::validate(need(input$initial_firm_notification != "" , "No notication selected"))
    #shiny::validate(need(input$report_date != "" , "No date selected"))
    shiny::validate(need(input$broad_category != "" , "No reason for recall selected"))
    
    all_fda_data_clean %>% filter(state %in% input$state
                                  & classification %in% input$classification
                                  #& recalling_firm %in% input$recalling_firm
                                  & status %in% input$status
                                  & initial_firm_notification %in% input$initial_firm_notification
                                  & broad_category %in% input$broad_category
                                  & report_year %in% input$report_year) #%>%
                         #filter(report_date %in% seq(input$report_date[1],input$report_date[2],by="day"))
    
    })
  
  
  ## Overall Recall Summary ##

  # Reason for Recall plot
  output$recall_plot <- renderBillboarder({
    donut_data = full_data() %>% group_by(broad_category) %>% tally()
    
    billboarder() %>% bb_donutchart(data = donut_data) %>% bb_legend(position = 'right') %>% 
      bb_color(palette = c('#E58606','#5D69B1','#52BCA3','#99C945','#CC61B0','#669900','#9970ab'))
  })
  
  # Classification plot
  output$class_plot <- renderBillboarder({
    donut_data = full_data() %>% group_by(classification) %>% tally()
    
    billboarder() %>% bb_donutchart(data = donut_data) %>% bb_legend(position = 'right') %>% 
      bb_color(palette = c('#E58606','#5D69B1','#52BCA3'))
  })
  
  # Status plot
  output$status_plot <- renderBillboarder({
    donut_data = full_data() %>% group_by(status) %>% tally()
    
    billboarder() %>% bb_donutchart(data = donut_data) %>% bb_legend(position = 'right') %>% 
      bb_color(palette = c('#E58606','#5D69B1','#52BCA3'))
  })
  
  # Status plot
  output$notif_plot <- renderBillboarder({
    donut_data = full_data() %>% group_by(initial_firm_notification) %>% tally()
    
    billboarder() %>% bb_donutchart(data = donut_data) %>% bb_legend(position = 'right')
  })
  
  output$time_plot1 <- renderBillboarder({
    time_data = full_data () %>% group_by(report_date) %>% tally()
    billboarder() %>% bb_linechart(data = time_data) %>% 
      bb_x_axis(tick = list(format = '%b %Y', fit = TRUE)) %>% 
      bb_subchart(show = T, size = list(height = 30), axis = list(x = list(show = F))) %>% 
      bb_legend(show = FALSE)
  })
  
  output$time_plot2 <- renderBillboarder({
    time_data = full_data() %>%
      group_by(report_year, broad_category) %>%
      summarise(CNT = n()) %>% reshape2::dcast(report_year~broad_category)
    billboarder() %>% bb_barchart(data = time_data, stacked = T) %>% 
      bb_color(palette = c('#E58606','#5D69B1','#52BCA3','#99C945','#CC61B0','#669900','#9970ab'))
  })
  
  output$time_plot3 <- renderBillboarder({
    time_data = full_data() %>%
      group_by(report_year, classification) %>%
      summarise(CNT = n()) %>% reshape2::dcast(report_year~classification)
    billboarder() %>% bb_barchart(data = time_data, stacked = T) %>% 
      bb_color(palette = c('#E58606','#5D69B1','#52BCA3'))
  })
  
  output$time_plot4 <- renderBillboarder({
    time_data = full_data() %>%
      group_by(report_year, initial_firm_notification) %>%
      summarise(CNT = n()) %>% reshape2::dcast(report_year~initial_firm_notification)
    billboarder() %>% bb_barchart(data = time_data, stacked = T) %>% 
      bb_color(palette = c('#E58606','#5D69B1','#52BCA3','#99C945','#CC61B0','#669900','#9970ab','#887045'))
  })
  
  output$table1 <- DT::renderDataTable(DT::datatable({
    data <- full_data() %>% 
      group_by(recalling_firm) %>%
      summarise('Total Number of Recalls' = n(),
                'Recalls due to Contamination' = sum(contamination),
                'Recalls due to Foreign Material' = sum(foreign_objects),
                'Recalls due to Undeclared Ingrediant' = sum(undeclared),
                'Recalls due to Mechanical Malfunction' = sum(temperature),
                'Recalls due to Mold' = sum(mold),
                'Recalls due to Gluten' = sum(gluten),
                'Recalls in 2019' = sum(report_year == 2019),
                'Recalls in 2020' = sum(report_year == 2020),
                'Recalls in 2021' = sum(report_year == 2021),
                'Recalls in 2022' = sum(report_year == 2022),
                'Recalls in 2023' = sum(report_year == 2023),
                'Recalls in Class I' = sum(classification == 'Class I'),
                'Recalls in Class II' = sum(classification == 'Class II'),
                'Recalls in Class III' = sum(classification == 'Class III')) %>%
      arrange(desc(`Total Number of Recalls`)) 
      #%>% head(10)
    data
  }))
  
  
  #Create map
  output$map_1 <- renderLeaflet({
    
    map_data <- full_data() %>% 
      group_by(state) %>% 
      summarise(recalls = n())
    
    map_data <- merge(map_data, states, by.x = "state",by.y = "STUSPS",all.x=TRUE)
    
    map_data <- sf::st_as_sf(map_data)
    
    paletteNum <- colorNumeric('Blues', domain = map_data$recalls)
    
    popup_sb <- with(map_data,
                     paste(state, '<br>',
                           "Number of Recalls: ", recalls))
    
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels)  %>% 
      setView(lng = -96.25, lat = 39.50, zoom = 4) %>% 
      addPolygons(data = map_data,
                  
                  # state border stroke color
                  color = 'white',
                  
                  # soften the weight of the state borders
                  weight = 1,
                  
                  # values >1 simplify the polygons' lines for less detail but faster loading
                  smoothFactor = .3,
                  
                  # set opacity of polygons
                  fillOpacity = .75,
                  
                  # specify that the each state should be colored per paletteNum()
                  fillColor = ~paletteNum(map_data$recalls)
      ) %>%
      addPolygons(data = map_data,
                  fillColor = ~paletteNum(map_data$recalls),
                  fillOpacity = 0.7,
                  weight = 0.2,
                  smoothFactor = 0.2,
                  popup = ~popup_sb) %>%
      addLegend(pal = paletteNum, 
                values = map_data$recalls, #reference map_data() 
                position = "bottomright", 
                title = "Number of Recalls")
    m
    
  })
  
  ## Allergens page
  # Reason for Recall plot
  output$ingredients_plot1 <- renderBillboarder({
    ingredient_data = full_data() %>% group_by(undeclared_allergen) %>% tally() %>% na.omit()
    
    billboarder() %>% bb_donutchart(data = ingredient_data) %>% bb_legend(position = 'right') %>% 
      bb_color(palette = c('#E58606','#5D69B1','#52BCA3','#99C945','#CC61B0',
                           '#669900','#9970ab',"#E495A5", "#ABB065","#39BEB1"))
  })
  
  
  output$time_plot_ing <- renderBillboarder({
    time_data_ing = full_data() %>%
      group_by(report_year, undeclared_allergen) %>%
      summarise(CNT = n()) %>% na.omit() %>% reshape2::dcast(report_year~undeclared_allergen)
    billboarder() %>% bb_barchart(data = time_data_ing, stacked = T) %>% 
      bb_color(palette = c('#E58606','#5D69B1','#52BCA3','#99C945','#CC61B0',
                           '#669900','#9970ab',"#E495A5", "#ABB065","#39BEB1"))
  })
  
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
