# README ------------------------------------------------------------------

# KEY FUNCTIONS to be developed

#https://whyman.shinyapps.io/metro_mapper/

# https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/



# 1. (/) File upload: https://shiny.rstudio.com/gallery/file-upload.html

# 2. (/) Choose the variable to map for state color: https://github.com/daattali/colourpicker

# 3. (/) Choose from the color template: https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

# 4. (/) Save the plot to local

# 5. ( ) MSA bubble maps

# 6. ( ) County maps

# 7. (/) Select donwload option (pdf/png)

# Author: David Whyman

# Date: Thu Jul 26 09:13:31 2018

# --------------

# pkgs <- c('dplyr','maps','mapproj','ggplot2','scales','ggthemes','RColorBrewer','plotly','fiftystater',

# 'shiny','colourpicker','plyr')


# check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)

# if(any(!check)){

#     pkgs.missing <- pkgs[!check]

#     install.packages(pkgs.missing)

#     check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)

#   }



# sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)

# shapefile source: 
#https://evergreen.data.socrata.com/dataset/Cartographic-Boundary-Shapefiles-Metropolitan-And-/w6x3-m3ia
# Read Data ---------------------------------------------------------------

#"V:/metro_data_warehouse/data_spatial/shapefiles/2018/insets/states/low_definition/states51_inset_ld.shp"
#"V:/metro_data_warehouse/data_spatial/shapefiles/2018/insets/counties/low_definition/counties51_inset_ld.shp"
#"V:/metro_data_warehouse/data_spatial/shapefiles/2018/insets/cbsas/low_definition/cbsas51_inset_ld.shp"

library('RColorBrewer')

library('plotly')

library('shiny')

library('colourpicker')

library("sf")

library("readr")

library("leaflet")

library("tmap")

library("rmapshaper")

library("tidyverse")

library("DT")

options(shiny.sanitize.errors = TRUE)

#setwd("C:/Users/DWhyman/Documents/shiny_backup")

cbsa50<-read_sf("shapefiles/cbsas51_inset_ld.shp") %>%
  dplyr::select(name = NAME, geoid = GEOID, geometry = geometry) %>%
  ms_simplify()
co50<-read_sf("shapefiles/counties51_inset_ld.shp") %>%
dplyr::select(name = NAME, geoid = GEOID, geometry = geometry, st_code = STATEFP) %>%
  ms_simplify()
st50<-read_sf("shapefiles/states51_inset_ld.shp") %>%
   ms_simplify()

# cbsa50<-read_csv("data/cbsa50.csv", col_types = cols(geoid = col_character()))
# st50<-read_csv("data/st50.csv", col_types = cols(GEOID = col_character()))
# co50<-read_csv("data/co50.csv", col_types = cols(geoid = col_character()))

co48<-filter(co50, !grepl("02|15",st_code))
cbsa48<-filter(cbsa50, !grepl(", HI|, AK",name))
st48<-filter(st50, !grepl("Alaska|Hawaii",NAME))

borders50<-tm_shape(st50, projection = 2163)+tm_borders(lwd = 0.5)+tm_layout(frame=FALSE)
borders48<-tm_shape(st48, projection = 2163)+tm_borders(lwd = 0.5)+tm_layout(frame=FALSE)


co_all<-read_csv("data/co_all.csv", col_types = cols(stco_code = col_character()))
cbsa_all<-read_csv("data/cbsa_all.csv", col_types = cols(cbsa_code = col_character()))
county_cbsa_st<-read_csv("data/county_cbsa_st.csv", col_types = cols(cbsa_code = col_character(), stco_code = col_character()))

list_all_cbsa<-mget(load("data/list_all_cbsa.rda"))$list_all_cbsa
list_all_co<-mget(load("data/list_all_co.rda"))$list_all_co

basic_cbsa<-names(county_cbsa_st %>% dplyr::select(contains("cbsa_")) %>% unique())
basic_co<-names(county_cbsa_st %>% dplyr::select(contains("co_"),"cbsa_code","cbsa_name") %>% unique())

# Shiny R -----------------------------------------------------------------



ui <- fluidPage(
  
  titlePanel("Metro Mapper"),
  
  tabsetPanel(type = "tabs",
              
  tabPanel("Mapper",
  
  sidebarLayout(
    
    
    
    sidebarPanel(
      
      
      
      helpText("Contact: dwhyman@brookings.edu"),
      
      h3("1. Choose Your Dataset!"),
      
      radioButtons("wharehouse0","Where is your dataset?", choices = c("Data warehouse" = "TRUE", "I'll upload my own .csv file" = "FALSE"), selected = character(0)),
 
           
      conditionalPanel("input.wharehouse0 == 'TRUE'",
        
                       selectizeInput("wharehouse","Choose data warehouse dataset", choices = c(names(list_all_co),names(list_all_cbsa)), selected = NULL)
      ),
      
      conditionalPanel("input.wharehouse0 == 'FALSE'",
      
                       fileInput('file1',"Choose CSV File", accept = c(".csv"))
      ),
      
      radioButtons("glevel","The dataset you chose is on what geography level?", choices = c("metro","county"), selected = character(0)),
      
      conditionalPanel(condition = "input.wharehouse0 == 'TRUE'",      
      
                       checkboxInput("custom","Filter dataset by jurisdiction", FALSE)
      ),


      
      conditionalPanel(condition = "input.custom == true",
                       
          selectizeInput(
            
            "cbsa_choose", "Filter by metro Area:",
            
            choices = county_cbsa_st$cbsa_name, multiple = TRUE
            
          ),
          
          selectizeInput(
            
            "states", "Filter by state:",
            
            choices = county_cbsa_st$st_name, multiple = TRUE      
          )
        ),
      
      conditionalPanel(condition = "input.custom == true & input.glevel == 'county'",
                       
      selectizeInput(
        
        "co_choose", "Filter by county",
        
        choices = county_cbsa_st$co_name, multiple = TRUE
        
        )
      ),
      
      conditionalPanel(condition = "input.glevel == 'metro' | input.glevel == 'county'", 
                       
      actionButton("choice", "Show Data"),
      
      tags$hr(),
      
      h3("2. Choose a Column From Your Dataset!"),
      
      
      selectInput("var", "Select variable to map",
                  
                  choices = NULL),
      
      conditionalPanel(condition = "input.bubbs == 'low'",
                       
                       selectInput("var2", "Choose a variable to map (bubble size)",
                                   
                                   choices = NULL)
      ),
      
      
      
      tags$hr(),
      
      h3("3. Customize Your Map!"),
      
      
      checkboxInput("customize", "I'm Ready to customize!", FALSE),
      
      conditionalPanel(condition = "input.customize == true",
                       
            radioButtons("bubbs", "Map resolution (Decrease for faster load)", choices = c("High" = "high", "Medium" = "medium","Low (bubbles)" = "low")),
            
            checkboxInput("bord","Draw jurisdiction border lines", FALSE),
            
            checkboxInput("sbord","Draw state border lines", FALSE),
            
            checkboxInput("scale", "Custom scale breaks", FALSE),
            
            conditionalPanel(condition = "input.scale == true",
                             
                             textInput("breaks","Enter scale breaks (separate by commas)", NULL)
            ),
            
            conditionalPanel("input.scale == false",
                             
                             radioButtons("style", "Scaling", choices = c("Continuous" = "cont", "Categorical" = "pretty"))
            ),
            
            

            checkboxInput("hiak", "Don't leave out Hawaii & Alaska!", FALSE),
            
            
            colourInput("low", "Choose a color for low value","#deebf7"),
            
            colourInput("high", "Choose a color for high value", "#08519c"),
            
            
            plotOutput("histo",width = "75%", height = "200px")
      
      ),
      
      tags$hr(),
      
      h3("4. Download Map & Code!"),
      
      checkboxInput("export","I'm Ready to Download!", FALSE),
      
      conditionalPanel(condition = "input.export == true",
                       
        downloadButton("plot", label = "Download the map!"),
        
        radioButtons("filetype", "File type:", choices = c("png", "pdf", "html")),      
        
        textInput("legen", label = "Legend title"), 
        
        downloadButton("report", label = "Download map! (publication-ready!)"),
        
        textInput("title", label = "Report title"), 
        
        textInput("subtitle", label = "Report subtitle"), 
        
        textInput("source", label = "Report source"), 
        
        textInput("notes", label = "Report notes"),
        
        sliderInput("asp", label = "Publication map height", min = 20, max = 35, value = 30),
        
        downloadButton("code", label = "Download the code!"),
        
        downloadButton("basic", label = "Download csv!")
        
        
      )
      )
    ),
    
    
    
    mainPanel(
      
      dataTableOutput("contents"),
      
      leafletOutput("map"))
    
    
  )
  
  
  ),
  
  tabPanel("Instructions",
           includeMarkdown("README.md"))
  )
  
)



# Server logic ----

server <- function(input, output,session) {
  
  
  
  info <- eventReactive(input$choice,{
  
    if (input$wharehouse0 == "FALSE"){
  
        req(input$file1)
    
    
    
    df <- read_csv(input$file1$datapath, col_types = cols(cbsa_code = col_character(), stco_code = col_character()))
    
    if(is.null(df$cbsa_code) & !is.null(df$geoid)) {df<-dplyr::rename(df,geocode = geoid)}
    if(is.null(df$cbsa_code) & !is.null(df$stco_code)) {df<-dplyr::rename(df,geocode = stco_code)}
    if(!is.null(df$cbsa_code)) {df<-dplyr::rename(df,geocode = cbsa_code)}
    
    
    df$cbsa_code <-str_pad(df$geocode, width=5, side="left", pad="0")  
    
    vars <- names(df[-c(1,2)])
    updateSelectInput(session,"var",'Choose a variable to map', choices = vars)
    updateSelectInput(session,"var2",'Choose a variable to map (bubble size)', choices = vars)
    
    
    df
    } else {
    
      if (grepl("cbsa",input$wharehouse) == TRUE){
      
      
            (if(input$custom == FALSE) {cbsa_codes <- county_cbsa_st$cbsa_code} else {cbsa_codes <- (filter(county_cbsa_st, st_name %in% input$states | cbsa_name %in% input$cbsa_choose)$cbsa_code)})
        
      cbsa_columns <- unlist(list_all_cbsa[input$wharehouse], use.names = F)
      
      
      updateSelectInput(session,"var",'Choose a variable to map', choices = c(basic_cbsa[-c(1,2)],cbsa_columns[-c(1,2)]))  
      updateSelectInput(session,"var2",'Choose a variable to map (bubble size)', choices = c(basic_cbsa[-c(1,2)],cbsa_columns[-c(1,2)]))
      #updateSelectInput(session,"glevel",'Geography Level of Data', choices = c("metro","county"), selected = "metro")  
      
      cbsa_all %>%
        
        filter(cbsa_code %in% cbsa_codes) %>%
        
        dplyr::select(cbsa_columns) %>%
        
        unique() %>%
        
        left_join(county_cbsa_st %>% dplyr::select(contains("cbsa_")) %>% unique(), by = "cbsa_code") %>%
        
        mutate_if(is.numeric, ~ round(., 2)) %>%
      
        dplyr::rename(geocode = cbsa_code)
      }
      
      else { 
        
      
        (if(input$custom == FALSE) {co_codes <- county_cbsa_st$stco_code} else {co_codes <- (filter(county_cbsa_st, st_name %in% input$states | co_name %in% input$co_choose | cbsa_name %in% input$cbsa_choose)$stco_code)})
        
        co_columns <- unlist(list_all_co[input$wharehouse], use.names = F)
        
        updateSelectInput(session,"var",'Choose a variable to map', choices = c(basic_co[-c(1,2,7,8)],co_columns[-c(1,2)]))  
        updateSelectInput(session,"var2",'Choose a variable to map (bubble size)', choices = c(basic_co[-c(1,2,7,8)],co_columns[-c(1,2)]))
        #updateSelectInput(session,"glevel",'Geography Level of Data', choices = c("metro","county"), selected = "county")  
        
        
        co_all %>%
          
          filter(stco_code %in% co_codes) %>%
          
          dplyr::select(co_columns) %>%
          
          unique() %>%          
          
          left_join(county_cbsa_st %>% dplyr::select(contains("co_"),"cbsa_code","cbsa_name") %>% unique(), by = "stco_code")%>%
          
          mutate_if(is.numeric, ~ round(., 2)) %>%
        
          dplyr::rename(geocode = stco_code)
        
      }
    }
      })
  
  
        
        
        

        
     
  
  

        
  info2 <- reactive({
    info1<-info()
    
    info1 %>% dplyr::select(geocode, dplyr::everything())
    
    
    
    
  }) 
  
  input_data1 <- reactive({
    
    input_data <- info2()
    
    #join shapefile with input_data to plug into map
    
    if (input$hiak == FALSE)
    {inner_join((if(input$glevel=="county") {co48} else {cbsa48}),input_data, by = c("geoid" = "geocode"))}
    else
    {inner_join((if(input$glevel=="county") {co50} else {cbsa50}),input_data, by = c("geoid" = "geocode"))}
    
  })
  
  
  
  fborders <- reactive({
    
    req(input$var)
    if(input$glevel == "county"){
      
      (if(input$custom == FALSE) {st_names <- county_cbsa_st$st_name} else {st_names <- (filter(county_cbsa_st, st_name %in% input$states | co_name %in% input$co_choose | cbsa_name %in% input$cbsa_choose)$st_name)})
      
      
    } else {
      
      (if(input$custom == FALSE) {st_names <- county_cbsa_st$st_name} else {st_names <- (filter(county_cbsa_st, st_name %in% input$states | cbsa_name %in% input$cbsa_choose)$st_name)})
      
      
    }
    
    st_final<-(if(input$hiak == FALSE){st48}else{st50})
    
    tm_shape(filter(st_final, NAME %in% st_names), projection = 2163) + tm_borders(lwd = 0.5) + tm_layout(frame = FALSE)
    
  })
  
  
  bmapper <-function(){
    if (input$sbord == FALSE) {fborders} else {fborders()}
    
  }
  
  
  the_map <- reactive({
      bmapper() + tm_shape(input_data1(), projection = 2163) + tmapper() + tm_layout(
      legend.position = c("LEFT","BOTTOM"),
      legend.outside = FALSE,
      legend.title.size = .0001,
      title.position = c("LEFT", "BOTTOM"),
      title = input$title,
      title.size = 1.5,
      fontfamily = "serif") 
  })
  
  

  
  
  

  
  output$contents <- DT::renderDataTable({
    
    display_data <- info()
    
    DT::datatable(
      
      display_data,
      
      options = list(
        
        lengthMenu = list(c(3, 5, 10, -1), c("3","5", "15", "All")),
        
        pageLength = 5
        
      )
      
    )
    
  })
  
  
  output$histo = renderPlot({
    req(input$var)
    input_data <- info()
    
    if(is.numeric(input_data[[input$var]])){
      hist(input_data[[input$var]],
           main = input$vars,
           xlab = "",
           freq = TRUE,
           breaks = 100)
    }
    
  })
  
  output$map = renderLeaflet({
    
    req(input$var)    
    
    the_map<-the_map()
    
    
    tmap_leaflet(the_map)
    
    
    
  })
  
  

  
  
  tmapper <- function(...){
    
    req(input$var, cancelOutput = TRUE)
    req(input$bubbs, cancelOutput = TRUE)
    
    input_data2<-input_data1()
    
    if (input$bubbs == "low") 
    {
      req(input$var2)
      
      if(input$scale == TRUE)
      { req(input$breaks, cancelOutput = TRUE)
        
        
        tm_bubbles(col = input$var, 
                   size = input$var2, 
                   palette = c(input$low, input$high),
                   breaks = as.numeric(unlist(strsplit(input$breaks,","))),
                   popup.vars=c("name", input$var2, input$var),
                   popup.format = list(text.align = "left", format = "f", digits = 3),
                   colorNA = NULL, 
                   showNA = NULL, 
                   ...
                   
        )
      } else {
        
        tm_bubbles(col = input$var, 
                   size = input$var2, 
                   palette = c(input$low, input$high),
                   style = input$style,
                   popup.vars=c( "name", input$var2, input$var),
                   popup.format = list(text.align = "left", format = "f", digits = 3),
                   colorNA = NULL, 
                   showNA = NULL, 
                   ...
                   
                  )     
        
        
      }
      
      
    } else {
      
      
      if(input$scale == TRUE)
      { req(input$breaks, cancelOutput = TRUE)
        
        
        tm_polygons(input$var, 
                    palette = c(input$low, input$high),
                    breaks = as.numeric(unlist(strsplit(input$breaks,","))),
                    popup.vars=c(input$var, "name"),
                    popup.format = list(text.align = "left", format = "f", digits = 3),
                    colorNA = NULL, 
                    showNA = NULL, 
                    border.col = if(input$bord == FALSE){NULL} else {"#636363"},
                    ...
                    )
      } else {
        
        tm_polygons(input$var, 
                    
                    palette = c(input$low, input$high),
                    style = input$style,
                    popup.vars=c(input$var, "name"),
                    popup.format = list(text.align = "left", format = "f", digits = 3),
                    colorNA = NULL, 
                    showNA = NULL, 
                    border.col = if(input$bord == FALSE){NULL} else {"#636363"},
                    ...
                    )     
        
        
      }
    }
    
    
    
    
    
    
  }
  
  
  
  output$plot <- downloadHandler(
    
    
    filename = function(){
      
      paste("plot", input$filetype, sep = ".")
      
    },
    
    content = function(file){
      
      tmap_save(borders48 + tm_shape(input_data1(), projection = 2163) +
                tmapper() + 
                tm_layout(
                    legend.position = c("LEFT","BOTTOM"),
                    legend.outside = FALSE,
                    legend.title.size = .0001,
                    title.position = c("LEFT", "BOTTOM"),
                    title = input$title,
                    title.size = 3,
                    legend.text.size = 1.5,
                    fontfamily = "serif"), 
                file, 
                width = 16, 
                height = 10.4)
      
    }
  )
  
  
  
  
  output$report <- downloadHandler(
    filename = "report.html",
    
    content = function(file) {
      src <- normalizePath('report3.Rmd')
      src2 <- normalizePath('logo2.jpeg')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report3.Rmd', overwrite = TRUE)
      file.copy(src2, 'logo2.jpeg', overwrite = TRUE)
      
      
      rmarkdown::render('report3.Rmd', output_file = file)
      
    }
  )
  
  
  output$code <- downloadHandler(
    filename = "metro_coder.html",
    
    content = function(file) {
      sroc <- normalizePath('metro_coder3.Rmd')
      sroc2 <- normalizePath('logo2.jpeg')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(sroc, 'metro_coder3.Rmd', overwrite = TRUE)
      file.copy(sroc2, 'logo2.jpeg', overwrite = TRUE)     
      
      rmarkdown::render('metro_coder3.Rmd', output_file = file)
      
    }
  )
  
  
  output$basic<-downloadHandler(
    filename = "fulldata.csv",
    
    content = function(file) {write_csv(info2(),path = file)}
  ) 
  
  
  
  
}

# Run app ----

shinyApp(ui, server)


