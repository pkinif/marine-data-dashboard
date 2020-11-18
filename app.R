
# 0.1 PACKAGES ------------------------------------------------------------

if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("shinydashboardPlus")) devtools::install_github("RinteRface/shinydashboardPlus")

if (!require("shinyjs")) install.packages("shinyjs")
if (!require("shinyalert")) install.packages("shinyalert")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("shinycssloaders")) install.packages("shinycssloaders")
if (!require("shinymanager")) install.packages("shinymanager")

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("data.table")) install.packages("data.table")
if (!require("stringr")) install.packages("stringr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("plotly")) install.packages("plotly")
if (!require("forcats")) install.packages("forcats")
if (!require("tsibble")) install.packages("tsibble")
if (!require("DT")) install.packages("DT")

if (!require("leaflet")) install.packages("leaflet")
if (!require("leaflet.extras")) install.packages("leaflet.extras")

source('src/function.R')

# 0.2 GET DATA ------------------------------------------------------------

vessel_dt <- fread('data/vessel_dt.csv')


# 0.3 SET UP --------------------------------------------------------------

credentials <- data.frame(
  user = c('user1', 'user2'), # mandatory
  password = c('pass1', 'pass2'), # mandatory
  stringsAsFactors = FALSE
)

# 1.0 UI ------------------------------------------------------------------


ui <- dashboardPagePlus(
  skin = 'blue',

  ## 1.1 Header ----
  dashboardHeaderPlus(
    title = strong("Marine App"),
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "id-card"
  ),
  ## 1.2 Left Sidebar ----
  dashboardSidebar(
    ### Input 1 - Vessel Type
    pickerInput(
      inputId = "vessel_type",
      label = "Vessel Type", 
      choices = c(unique(vessel_dt$ship_type[order(vessel_dt$ship_type)])),
      selected = c(unique(vessel_dt$ship_type[order(vessel_dt$ship_type)]))[[1]],
      multiple = F,
      options = list(
        `live-search` = TRUE,
        `actions-box` = TRUE,
        size = 7)
    ), 
    
    ### Input 2 - Vessel Name
    pickerInput(
      inputId = "vessel_name",
      label = "Vessel Name", 
      choices = NULL,
      multiple = F,
      options = list(
        `live-search` = TRUE,
        `actions-box` = TRUE,
        size = 7)
    ), 
    
    br(), 
    
    ### Analyze button
    div(class = 'container-fluid pull-right', 
        actionButton(inputId = "collect_data", label = "Analyse", icon = icon("check"))
    ),
    
    br(), br(), br(), br(),
    
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Raw data", tabName = "rawdata", icon = icon("th"))
    )
  ),
  
  ## 1.3 Right Sidebar ----
  rightsidebar = rightSidebar(
    background = "light",
    flipBox(
      id = 1,
      header_img = "https://media-exp1.licdn.com/dms/image/C5616AQF6w2pS48I4ZQ/profile-displaybackgroundimage-shrink_350_1400/0?e=1611187200&v=beta&t=0MzAdERpCcX2MFZzFIEbK5forR5KfnuyETbTiknDT_k",
      main_img = "https://media-exp1.licdn.com/dms/image/C4E03AQH1w85EfrqrJQ/profile-displayphoto-shrink_400_400/0/1600444778624?e=1611187200&v=beta&t=c0dT6DxluT1mmIidRK4MtAP8I0IgWC_-1Wk-euyA3WM",
      front_title = "Pierrick Kinif",
      back_title = strong("Contact Pierrick"),
      p("MBA graduate, Ex-Credit Risk Data Analyst, Ex-Assistant Sustainability Practice, Ex-Department Manager, 
      I am currently working as both a Data Scientist at Decathlon Belgium and an R Shiny Developer Freelancer at Upwork."),
      br(),
      p("I am an R language lover with particularly strong feelings for the R Shiny Community."),
      back_content = tagList(
        column(
          width = 12,
          align = "center",
          a(href = "https://www.pierrickkinif.com/Cv.html", target = "_blank", class = "btn btn-danger", "Check my CV"),
          br(),
          a(href = "https://www.pierrickkinif.com/contact.html", target = "_blank", class = "btn btn-info", "Write me"),
          br(),
          socialButton(
            url = "https://github.com/pkinif",
            type = "github"
          ),
          socialButton(
            url = "https://www.linkedin.com/in/pierrickkinif/",
            type = "linkedin"
          )
        )
      )
    )
  ),
  
  ## 1.4 Body ----
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css.css")
    ),
    useShinyalert(),
    useShinyjs(),
    
    tabItems(
      # 1.4.1 Dashboard ----
      tabItem(
        "dashboard", 
        
        # i// valuebox ----

        fluidRow(
          class = 'container-fluid',
          boxPlus(
            closable = TRUE, 
            width = 12,
            status = "warning", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            shinydashboard::valueBoxOutput("duration", width = 6) %>% withSpinner(),
            shinydashboard::valueBoxOutput("distance", width = 6) %>% withSpinner()
          )
        ),
        
        # ii// leaflet ----
        fluidRow(
          class = 'container-fluid',
          boxPlus(
            closable = FALSE, 
            width = 12,
            status = "warning", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            leafletOutput("map") %>% withSpinner()
          )
        ),
      ),
      
      # 1.4.2 Raw Data ----
      tabItem(
        "rawdata",
        fluidRow(
          class = 'container-fluid pull-right', 
          downloadButton('dwn', 'Download Data', class = " btn btn-info bmd-btn-fab text-center")
        ),
        br(), br(),
        fluidRow(
          boxPlus(
            closable = FALSE, 
            width = 12,
            status = "warning", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            DT::dataTableOutput(outputId = "datatable") %>% withSpinner()
          )
        )
      )
    )
  ),
  
  # 1.5 Footer ----
  
  footer = dashboardFooter(
    left_text = "By Pierrick Kinif",
    right_text = "Belgium, 2020"
  )
)



# 2.0 SERVER ---------------------------------------------------------------



server <- function(input, output, session) {
  
  # 2.0 collect data ----
  db <- reactiveValues(data = NULL)
  
  observeEvent(input$collect_data, {

    # loading data
    
    shinyalert(text = HTML("<div class='cssload-loader'>Loading</div>"),
               timer = 0,
               animation = "pop",
               showConfirmButton = FALSE,
               html = TRUE)
    
    db$data <- get_data(input$vessel_name)

    shinyalert(text = HTML(
      "
      <p> Data collected </p>
      "),
      type = "info",
      timer = 0,
      animation = "pop",
      showConfirmButton = TRUE,
      html = TRUE)
      
  })
  
  # 2.1 Update Ship Name ----
  observeEvent(input$vessel_type, {
      db$vessel_name <- vessel_dt %>% 
        filter(ship_type == input$vessel_type) %>%  
        select(shipname) %>% 
        unique() %>% 
        pull(1)
      
      updatePickerInput(session, "vessel_name", choices = sort(db$vessel_name))  

  })
  
  # 2.2 Leaflet Map ----
  
  leaflet <- eventReactive(c(input$collect_data, db$data), {
    if (!is.null(db$data)) db$data %>%  
      get_longest_distance_between_two_points() %>% 
      make_leaflet_map()
  })
  
  output$map <- renderLeaflet(leaflet())
  
  # 2.3 valuebox ----
  
  # 2.3.1 duration ----
  value_box_duration <- eventReactive(input$collect_data, {
    if(!is.null(db$data)) {
      db$data %>% 
        get_longest_distance_between_two_points() %>% 
        render_value_box('duration', icn = 'clock')
    } 
  })
  output$duration <- renderInfoBox(
    value_box_duration()
  )
  
  # 2.3.2 speed ----
  value_box_speed <- eventReactive(input$collect_data, {
    if(!is.null(db$data)) {
      db$data %>% 
        get_longest_distance_between_two_points() %>% 
        render_value_box('avg_speed', icn = 'tachometer-alt')
    } 
  })
  output$avg_speed <- renderInfoBox(
    value_box_speed()
  )
  
  # 2.3.3 distance ----
  value_box_distance <- eventReactive(input$collect_data, {
    if(!is.null(db$data)) {
      db$data %>% 
        get_longest_distance_between_two_points() %>% 
        render_value_box('distance', icn = 'ship')
    } 
  })
  output$distance <- renderInfoBox(
    value_box_distance()
  )
  
  # 2.4 datatable Raw Data ----
  
  datatable <- eventReactive(c(input$collect_data, db$data), {
    if (!is.null(db$data)) DT::datatable(db$data,
                                         rownames = FALSE,
                                         fillContainer = TRUE,
                                         autoHideNavigation = TRUE,
                                         filter = 'top',
                                         options = list(search = list(regex = TRUE, caseInsensitive = FALSE), pageLength = 15, scrollY = '500px')
    )
  })
  
  
  output$datatable <- DT::renderDataTable(datatable())
  
  # download the filtered data
  output$dwn = downloadHandler(paste("data-", today(), ".csv", sep = ""), content = function(file) {
    s = input$datatable_rows_all
    fwrite(db$data[s,], file, sep = ";",row.names = F)
  }) 
  
  # 2.5 Welcoming Message ----
  shinyalert("Welcome", 
             
             "The Marine App Dashboard, the platform that allows you to analyze the longest distances traveled by vessels.
                
              Choose your vessel type and the name of the vessel you want to analyze, then click on 'Analyze'.
                
                 ", 
             type = "info",
             closeOnClickOutside = TRUE, 
             confirmButtonText = "Enjoy")    
  
  # 2.6 Shiny Manager ----
  
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

}

# 3.0 RUN APP -------------------------------------------------------------

ui <- secure_app(ui)
shinyApp(ui = ui, server = server)
