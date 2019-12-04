

# Read Data ---------------------------------------------------------------

# "V:/metro_data_warehouse/data_spatial/shapefiles/2018/insets/states/low_definition/states51_inset_ld.shp"
# "V:/metro_data_warehouse/data_spatial/shapefiles/2018/insets/counties/low_definition/counties51_inset_ld.shp"
# "V:/metro_data_warehouse/data_spatial/shapefiles/2018/insets/cbsas/low_definition/cbsas51_inset_ld.shp"

library("leaflet")

library("RColorBrewer")

library("shiny")

library("colourpicker")

library("sf")

library("tmap")

library("rmapshaper")

library("tidyverse")

library("leaflet")

library("DT")

library("shinydashboard")

options(shiny.sanitize.errors = TRUE)

# setwd("C:/Users/DWhyman/Documents/shiny_backup")

cbsa50 <- read_sf("shapefiles/cbsas51_inset_ld.shp") %>%
  dplyr::select(name = NAME, geoid = GEOID, geometry = geometry) %>%
  ms_simplify()
co50 <- read_sf("shapefiles/counties51_inset_ld.shp") %>%
  dplyr::select(name = NAME, geoid = GEOID, geometry = geometry, st_code = STATEFP) %>%
  ms_simplify()
st50 <- read_sf("shapefiles/states51_inset_ld.shp") %>%
  ms_simplify()

# cbsa50<-read_csv("data/cbsa50.csv", col_types = cols(geoid = col_character()))
# st50<-read_csv("data/st50.csv", col_types = cols(GEOID = col_character()))
# co50<-read_csv("data/co50.csv", col_types = cols(geoid = col_character()))

# co48 <- filter(co50, !grepl("02|15", st_code))
# cbsa48 <- filter(cbsa50, !grepl(", HI|, AK", name))
# st48 <- filter(st50, !grepl("Alaska|Hawaii", NAME))
# 
# st_borders50 <- tm_shape(st50, projection = 2163) + tm_borders(lwd = 0.5) + tm_layout(frame = FALSE)
# borders48 <- tm_shape(st48, projection = 2163) + tm_borders(lwd = 0.5) + tm_layout(frame = FALSE)

load("data/cbsa_all.rda")
load("data/co_all.rda")
load("data/county_cbsa_st.rda")
load("data/list_all_cbsa.rda")
load("data/list_all_co.rda")


basic_cbsa <- names(county_cbsa_st %>% dplyr::select(contains("cbsa_")) %>% unique())
basic_co <- names(county_cbsa_st %>% dplyr::select(contains("co_"), "cbsa_code", "cbsa_name") %>% unique())



# Shiny R -----------------------------------------------------------------



ui <- fluidPage(
  tabsetPanel(
    type = "tabs",

    tabPanel(

      dashboardPage(
        dashboardHeader(title = "Metro Mapper"),
        dashboardSidebar(
          tags$head(
            tags$style(HTML("
                      .sidebar { height: 99vh; overflow-y: auto; }
                      .label { color: #000000}
                      "))
          ),


          div(style = "text-align:center", helpText("Contact: whyman@uchicago.edu")),

          h3(div(style = "text-align:center", "1. Choose Your Dataset!")),


          radioButtons("wharehouse0", "Where is your dataset?", choices = c("Data warehouse" = "TRUE", "I'll upload my own .csv file" = "FALSE"), selected = character(0)),


          conditionalPanel(
            "input.wharehouse0 == 'TRUE'",

            selectizeInput("wharehouse", "Choose data warehouse dataset", choices = c(names(list_all_cbsa), names(list_all_co)), selected = NULL)
          ),

          conditionalPanel(
            "input.wharehouse0 == 'FALSE'",

            fileInput("file1", "Choose CSV File", accept = c(".csv"))
          ),

          radioButtons("glevel", "The dataset you chose is on what geography level?", choices = c("metro", "county"), selected = character(0)),


          conditionalPanel(
            condition = "input.glevel == 'metro' | input.glevel == 'county'",
            checkboxInput("custom", "Filter dataset by jurisdiction", FALSE)
          ),



          conditionalPanel(
            condition = "input.custom == true",

            selectizeInput(

              "cbsa_choose", "Filter by metro Area:",
              choices = county_cbsa_st$cbsa_name, multiple = TRUE
            ),

            selectizeInput(

              "states", "Filter by state:",
              choices = county_cbsa_st$st_name, multiple = TRUE
            )
          ),

          conditionalPanel(
            condition = "input.custom == true & input.glevel == 'county'",

            selectizeInput(

              "co_choose", "Filter by county",
              choices = county_cbsa_st$stco_name, multiple = TRUE
            )
          ),

          conditionalPanel(
            condition = "input.glevel == 'metro' | input.glevel == 'county'",

            actionButton("choice", "Show Data"),

            tags$hr(),

            h3(div(style = "text-align:center", "2. Choose Column from Dataset!")),


            selectInput("var", "Select variable to map",
              choices = NULL
            ),

            conditionalPanel(
              condition = "input.bubbs == 'low'",

              selectInput("var2", "Choose a variable to map (bubble size)",
                choices = NULL
              )
            ),



            tags$hr(),

            h3(div(style = "text-align:center", "3. Customize Your Map!")),


            checkboxInput("customize", "I'm Ready to customize!", FALSE),

            conditionalPanel(
              condition = "input.customize == true",

              radioButtons("bubbs", "Map resolution (Decrease for faster load)", choices = c("High" = "high", "Medium" = "medium", "Low (bubbles)" = "low")),

              checkboxInput("bord", "Draw jurisdiction border lines", TRUE),

              checkboxInput("scale", "Custom scale breaks", FALSE),

              conditionalPanel(
                condition = "input.scale == true",

                textInput("breaks", "Enter scale breaks (separate by commas)", NULL)
              ),

              # conditionalPanel(
              #   "input.scale != true",
              # 
              #   radioButtons("style", "Scaling", choices = c("Continuous" = "cont", "Categorical" = "pretty"))
              # ),


              colourInput("low", "Choose a color for low value", "#deebf7"),

              colourInput("high", "Choose a color for high value", "#08519c"),


              conditionalPanel(
                "input.scale == true",
                plotOutput("histo", width = "75%", height = "200px")
              )
            ),

            tags$hr(),

            h3("  4. Download Map!"),

            checkboxInput("export", "I'm Ready to Download!", FALSE),

            conditionalPanel(
              condition = "input.export == true",
              radioButtons("filetype", "File type:", choices = c("png", "pdf")),
              # textInput("legen", label = "Legend title"),
              textInput("title", label = "Map title"),
              downloadButton("plot", label = "Download the map!")

            )
          ),
          width = "20em"
        ),



        dashboardBody(
          div(style = "overflow-x: scroll", DT::dataTableOutput("contents")),

          leafletOutput("map")
        ),

        # h5("Note: Basemap will *not* show on downloaded maps. Custom labels will *only* show on on downloaded maps.")
        tags$head(tags$style(HTML("
                              /* labels */
                              .skin-blue .main-sidebar .sidebar .label {
                              color: #000000;
                              text-color: #000000;
          
                              }

                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #10457a;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #10457a;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #10457a;
                              }        
                              
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #000000;
                              text-color: #000000;
                              }
                              
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #000000;

                              }
                              
                              /* other links in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #000000;
                              color: #000000;
                              }
                              
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #000000;
                              }
                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #000000;
                              }

                              ")))
      ),
      title = "Metro Mapper"
    ),

    tabPanel(
      "Instructions",

      includeMarkdown("README.md")
    )
  )
)



# Server logic ----

server <- function(input, output, session) {
  secs <- reactive({
    if (input$glevel == "county") {
      2800
    } else {
      1800
    }
  })

  breaks <- reactive({
    if (is.null(input$breaks)) {
      list(x = NA, y = NA)
    } else {
      input$breaks
    }
  })




  low <- reactive({
    if (is.null(input$low)) {
      list(x = NA, y = NA)
    } else {
      input$low
    }
  })


  high <- reactive({
    if (is.null(input$high)) {
      list(x = NA, y = NA)
    }
    else {
      input$high
    }
  })



  titlee <- reactive({
    if (is.null(input$title)) {
      list(x = NA, y = NA)
    } else {
      input$title
    }
  })


  dbreaks <- breaks %>% debounce(2500)

  blow <- low %>% debounce(secs)

  bhigh <- high %>% debounce(secs)

  btitle <- titlee %>% debounce(secs)

















  info <- eventReactive(input$choice, {
    if (input$custom == TRUE) {
      validate(need(!is.null(input$states) | !is.null(input$cbsa_choose) | !is.null(input$co_choose), "Please select the jurisdictions you wish to map (Step 1)"))
    }

    if (input$wharehouse0 == "FALSE") {
      req(input$file1)



      df <- read_csv(input$file1$datapath, col_types = cols(cbsa_code = col_character(), stco_code = col_character()))

      if (!is.null(df$cbsa_code)) {
        validate(need(input$glevel == "metro", "Please select *metro* as your dataset's geography level (Step 1)"))
      }
      if (!is.null(df$stco_code)) {
        validate(need(input$glevel == "county", "Please select *county* as your dataset's geography level (Step 1)"))
      }

      if (is.null(df$cbsa_code) & !is.null(df$geoid)) {
        df <- dplyr::rename(df, geocode = geoid)
      }
      if (is.null(df$cbsa_code) & !is.null(df$stco_code)) {
        df <- dplyr::rename(df, geocode = stco_code)
      }
      if (!is.null(df$cbsa_code)) {
        df <- dplyr::rename(df, geocode = cbsa_code)
      }


      df$cbsa_code <- str_pad(df$geocode, width = 5, side = "left", pad = "0")

      vars <- names(df[-c(1, 2)])
      updateSelectInput(session, "var", "Choose a variable to map", choices = vars)
      updateSelectInput(session, "var2", "Choose a variable to map (bubble size)", choices = vars)

      (if (input$glevel == "county") {
        (if (input$custom == FALSE) {
          co_codes <- county_cbsa_st$stco_code
        } else {
          co_codes <- (filter(county_cbsa_st, st_name %in% input$states | stco_name %in% input$co_choose | cbsa_name %in% input$cbsa_choose)$stco_code)
        })
      } else {
        (if (input$custom == FALSE) {
          cbsa_codes <- county_cbsa_st$cbsa_code
        } else {
          cbsa_codes <- (filter(county_cbsa_st, st_name %in% input$states | cbsa_name %in% input$cbsa_choose)$cbsa_code)
        })
      })


      df %>% filter(geocode %in% (if (input$glevel == "county") {
        co_codes
      } else {
        cbsa_codes
      }))
    } else {
      if (grepl("cbsa", input$wharehouse) == TRUE) {
        validate(need(input$glevel == "metro", "Please select *metro* as your dataset's geography level (Step 1)"))

        (if (input$custom == FALSE) {
          cbsa_codes <- county_cbsa_st$cbsa_code
        } else {
          cbsa_codes <- (filter(county_cbsa_st, st_name %in% input$states | cbsa_name %in% input$cbsa_choose)$cbsa_code)
        })

        cbsa_columns <- unlist(list_all_cbsa[input$wharehouse], use.names = F)


        updateSelectInput(session, "var", "Choose a variable to map", choices = c(basic_cbsa[-c(1, 2)], cbsa_columns[-c(1, 2)]))
        updateSelectInput(session, "var2", "Choose a variable to map (bubble size)", choices = c(basic_cbsa[-c(1, 2)], cbsa_columns[-c(1, 2)]))
        # updateSelectInput(session,"glevel",'Geography Level of Data', choices = c("metro","county"), selected = "metro")

        cbsa_all %>%
          filter(cbsa_code %in% cbsa_codes) %>%
          dplyr::select(cbsa_columns) %>%
          filter_all(all_vars(!is.na(.)))%>%
          unique() %>%
          left_join(county_cbsa_st %>% dplyr::select(contains("cbsa_")) %>% unique(), by = "cbsa_code") %>%
          mutate_if(is.numeric, ~ round(., 2)) %>%
          dplyr::rename(geocode = cbsa_code)
      }

      else {
        validate(need(input$glevel == "county", "Please select *county* as your dataset's geography level (Step 1)"))

        (if (input$custom == FALSE) {
          co_codes <- county_cbsa_st$stco_code
        } else {
          co_codes <- (filter(county_cbsa_st, st_name %in% input$states | stco_name %in% input$co_choose | cbsa_name %in% input$cbsa_choose)$stco_code)
        })

        co_columns <- unlist(list_all_co[input$wharehouse], use.names = F)

        updateSelectInput(session, "var", "Choose a variable to map", choices = c(basic_co[-c(1, 2, 7, 8)], co_columns[-c(1, 2)]))
        updateSelectInput(session, "var2", "Choose a variable to map (bubble size)", choices = c(basic_co[-c(1, 2, 7, 8)], co_columns[-c(1, 2)]))
        # updateSelectInput(session,"glevel",'Geography Level of Data', choices = c("metro","county"), selected = "county")


        co_all %>%
          filter(stco_code %in% co_codes) %>%
          dplyr::select(co_columns) %>%
          unique() %>%
          left_join(county_cbsa_st %>% dplyr::select(contains("co_"), "cbsa_code", "cbsa_name") %>% unique(), by = "stco_code") %>%
          mutate_if(is.numeric, ~ round(., 2)) %>%
          dplyr::rename(geocode = stco_code)
      }
    }
  })



  info2 <- reactive({
    info1 <- info()

    info1 %>% dplyr::select(geocode, dplyr::everything())
  })

  input_data1 <- reactive({
    input_data <- info2()

    # join shapefile with input_data to plug into map

      inner_join((if (input$glevel == "county") {
        co50
      } else {
        cbsa50
      }), input_data, by = c("geoid" = "geocode"))
    
  })



  # fborders <- reactive({
  #   req(input$var, cancelOutput = TRUE)
  #   if (input$glevel == "county") {
  #     (if (input$custom == FALSE) {
  #       st_names <- county_cbsa_st$st_name
  #     } else {
  #       st_names <- (filter(county_cbsa_st, st_name %in% input$states | stco_name %in% input$co_choose | cbsa_name %in% input$cbsa_choose)$st_name)
  #     })
  #   } else {
  #     (if (input$custom == FALSE) {
  #       st_names <- county_cbsa_st$st_name
  #     } else {
  #       st_names <- (filter(county_cbsa_st, st_name %in% input$states | cbsa_name %in% input$cbsa_choose)$st_name)
  #     })
  #   }
  # 
  #   st_final <- st50
  # 
  #   tm_shape(filter(st_final, NAME %in% st_names), projection = 2163) + tm_borders(lwd = 0.5) + tm_layout(
  #     legend.format = (big.num.abbr <- NA),
  #     frame = FALSE
  #   )
  # })





  the_map <- reactive({
   # fborders() + 
      tm_basemap(NULL)+
      tm_shape(st50, projection = 2163) +
      tm_borders(lwd = 0.5) + 
      
      tm_shape(input_data1(), projection = 2163) + 
      # tm_borders(lwd = 0.5) + 
      tmapper() + tm_layout(
      legend.position = c("LEFT", "BOTTOM"),
      legend.outside = FALSE,
      legend.title.size = .0001,
      title.position = c("LEFT", "BOTTOM"),
      title = input$title,
      title.size = 1.5,
      fontfamily = "serif"
    )
  })



  output$contents <- renderTable({
    input_data <- info()


    head(input_data, 4L)
  })

  output$contents <- DT::renderDataTable({
    info3 <- info2()

    DT::datatable(
      info3,
      options = list(
        lengthMenu = list(c(3, 5, 15, -1), c("3", "5", "15", "All")),
        pageLength = 3
      )
    )
  })


  output$histo <- renderPlot({
    req(input$var)


    input_data <- info()

    if (is.numeric(input_data[[input$var]])) {
      hist(input_data[[input$var]],
        main = input$vars,
        xlab = "",
        freq = TRUE,
        breaks = 100
      )
    }
  })

  output$map <- renderLeaflet({
    req(input$var)


    the_map <- the_map()

    req(!is.null(the_map), cancelOutput = TRUE)

    tmap_leaflet(the_map)
  })





  tmapper <- reactive({
    req(input$var, cancelOutput = TRUE)
    req(input$bubbs, cancelOutput = TRUE)


    input_data2 <- input_data1()

    req(!is.null(input_data2))

    if (input$bubbs == "low") {
      # req(input$var2)
      validate(need(is.numeric(input_data2[[input$var2]]), "Please set bubble size (Step 2) to a *numeric* variable"))

      if (input$scale == TRUE) {
        req(input$breaks, cancelOutput = TRUE)
        req(dbreaks() != list(x = NA, y = NA), cancelOutput = TRUE)

        tm_bubbles(
          col = input$var,
          size = input$var2,
          palette = c(blow(), bhigh()),
          breaks = as.numeric(unlist(strsplit(dbreaks(), ","))),
          popup.vars = c("name", input$var2, input$var),
          popup.format = list(text.align = "left", format = "f", digits = 3),
          colorNA = NULL,
          showNA = NULL
        )
      } else {
        tm_bubbles(
          col = input$var,
          size = input$var2,
          palette = c(blow(), bhigh()),
          # style = input$style,
          popup.vars = c("name", input$var2, input$var),
          popup.format = list(text.align = "left", format = "f", digits = 3),
          colorNA = NULL,
          showNA = NULL
        )
      }
    } else {
      if (input$scale == TRUE) {
        req(input$breaks, cancelOutput = TRUE)
        req(dbreaks() != list(x = NA, y = NA), cancelOutput = TRUE)


        tm_polygons(input$var,
          palette = c(blow(), bhigh()),
          breaks = as.numeric(unlist(strsplit(dbreaks(), ","))),
          popup.vars = c(input$var, "name"),
          popup.format = list(text.align = "left", format = "f", digits = 3),
          colorNA = NULL,
          showNA = NULL,
          border.col = if (input$bord == FALSE) {
            NULL
          } else {
            "#636363"
          }
        )
      } else {
        tm_polygons(input$var,
          palette = c(blow(), bhigh()),
          # style = input$style,
          popup.vars = c(input$var, "name"),
          popup.format = list(text.align = "left", format = "f", digits = 3),
          colorNA = NULL,
          showNA = NULL,
          border.col = if (input$bord == FALSE) {
            NULL
          } else {
            "#636363"
          }
        )
      }
    }
  })



  output$plot <- downloadHandler(
    filename = function() {
      paste("~/plot", input$filetype, sep = ".")
    },

    content = function(file) {
      tmap_save(
        # fborders() + 
        tm_shape(st50, projection = 2163) +
          tm_borders(lwd = 1) + 
          tm_shape(input_data1(), projection = 2163)+ 
          # tm_borders(lwd = 0.5) +
          tmapper()+
          tm_layout(
            legend.position = c("RIGHT", "BOTTOM"),
            legend.outside = FALSE,
            legend.title.size = .0001,
            title = btitle(),
            title.size = 3,
            legend.text.size = 1.5,
            fontfamily = "serif",
            frame = FALSE,
            bg.color =
              "transparent"
          ) ,
      file,
      width = 16,
      height = 10.4, bg = "transparent"
      )
    }
  )
}

# Run app ----

shinyApp(ui, server)
