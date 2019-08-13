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

library("tigris")

library("grid")

library("tidycensus")

options(shiny.sanitize.errors = TRUE)

#setwd("C:/Users/DWhyman/Documents/shiny_backup")

cbsa50<-read_sf("V:/metro_data_warehouse/data_spatial/shapefiles/2018/insets/cbsas/low_definition/cbsas51_inset_ld.shp") %>%
  dplyr::select(name = NAME, geoid = GEOID, geometry = geometry) %>%
  ms_simplify()
co50<-read_sf("V:/metro_data_warehouse/data_spatial/shapefiles/2018/insets/counties/low_definition/counties51_inset_ld.shp") %>%
dplyr::select(name = NAME, geoid = GEOID, geometry = geometry, st_code = STATEFP) %>%
  ms_simplify()

st50<-read_sf("V:/metro_data_warehouse/data_spatial/shapefiles/2018/insets/states/low_definition/states51_inset_ld.shp") %>%
   ms_simplify()

# cbsa50<-read_csv("data/cbsa50.csv", col_types = cols(geoid = col_character()))
# st50<-read_csv("data/st50.csv", col_types = cols(GEOID = col_character()))
# co50<-read_csv("data/co50.csv", col_types = cols(geoid = col_character()))

co48<-filter(co50, !grepl("02|15",st_code))
cbsa48<-filter(cbsa50, !grepl(", HI|, AK",name))
st48<-filter(st50, !grepl("Alaska|Hawaii",NAME))

borders50<-tm_shape(st50, projection = 2163)+tm_borders(lwd=0.2, col = "#d4d4d4")+tm_layout(frame=FALSE)
borders48<-tm_shape(st48, projection = 2163)+tm_borders(lwd=0.2, col = "#d4d4d4")+tm_layout(frame=FALSE)


co_all<-read_csv("data/co_all.csv", col_types = cols(stco_code = col_character()))
cbsa_all<-read_csv("data/cbsa_all.csv", col_types = cols(cbsa_code = col_character()))
county_cbsa_st<-read_csv("data/county_cbsa_st.csv", col_types = cols(cbsa_code = col_character(), stco_code = col_character()))

list_all_cbsa<-mget(load("data/list_all_cbsa.rda"))$list_all_cbsa
list_all_co<-mget(load("data/list_all_co.rda"))$list_all_co


# Shiny R -----------------------------------------------------------------





ui <- fluidPage(
  
  titlePanel("Metro Mapper"),
  
  
  sidebarLayout(
    
    
    
    sidebarPanel(
      
      
      
      helpText("Contact: dwhyman@brookings.edu"),
      
      
      checkboxInput("wharehouse0","Use Data Warehouse?", FALSE),
 
           
      conditionalPanel("input.wharehouse0 == true",
      selectInput("wharehouse","Choose data warehosue dataset", choices = c(names(list_all_co),names(list_all_cbsa)))
      ),
      
      
      conditionalPanel("input.wharehouse0 == false",
      fileInput('file1',"Choose CSV File",
                
                accept = c(".csv"))
      ),
      
      radioButtons("glevel","Geography Level of Data", choices = c("metro","county")
      ),
      

      actionButton("choice", "Show Data"),
      

      
      tags$hr(),
      
      
      
      selectInput("var", "Choose a variable to map",
                  
                  choices = NULL),
      
      conditionalPanel(condition = "input.bubbs == 'low'",
                       
                       selectInput("var2", "Choose a variable to map (bubble size)",
                                   
                                   choices = NULL)
      ),
      
      
      
      tags$hr(),
      
      
      
      radioButtons("bubbs", "map resolution (decrease for faster load)", choices = c("high", "medium","low")),
      
      
      
      checkboxInput("scale", "Custom scale breaks", FALSE),
      
      conditionalPanel(condition = "input.scale == true",
                       
                       textInput("breaks","Enter scale breaks (separate by commas)", NULL)
      ),
      
      conditionalPanel("input.scale == false",
                       
                       radioButtons("style", "Scaling", choices = c("Continuous" = "cont", "Categorical" = "pretty"))
      ),
      
      
      checkboxInput("filt", "1 Row is year X metro area", FALSE),
      
      checkboxInput("hiak", "Include Hawaii & Alaska", FALSE),
      
      conditionalPanel("input.filt == true",
                       
                       textInput("year","Year")
      ),      
      
      colourInput("low", "Choose a color for low value","#deebf7"),
      
      colourInput("high", "Choose a color for high value", "#08519c"),
      
      
      plotOutput("histo",width = "75%", height = "200px"),
      
      
      tags$hr(),
      
      
      downloadButton("plot", label = "Download the map"),
      
      radioButtons("filetype", "File type:", choices = c("png", "pdf", "html")),      
      
      textInput("legen", label = "Legend title"), 
      
      downloadButton("report", label = "Generate pdf report"),
      
      textInput("title", label = "Report title"), 
      
      textInput("subtitle", label = "Report subtitle"), 
      
      textInput("source", label = "Report source"), 
      
      textInput("notes", label = "Report notes"),
      
      downloadButton("code", label = "Download the code"),
      
      downloadButton("basic", label = "Download code-running requirements")
    ),
    
    
    
    mainPanel(
      
      tableOutput("contents"),
      
      leafletOutput("map"))
    
    
    
  )
  
)



# Server logic ----

server <- function(input, output,session) {
  
  
  
  info <- eventReactive(input$choice,{
  
    if (input$wharehouse0 == FALSE){
  
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
      
      cbsa_codes <- county_cbsa_st$cbsa_code
      
      cbsa_columns <- unlist(list_all_cbsa[input$wharehouse], use.names = F)
      
      updateSelectInput(session,"var",'Choose a variable to map', choices = cbsa_columns[-c(1,2)])  
      updateSelectInput(session,"var2",'Choose a variable to map (bubble size)', choices = cbsa_columns[-c(1,2)])
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
        
        co_codes <- county_cbsa_st$stco_code
        
        co_columns <- unlist(list_all_co[input$wharehouse], use.names = F)
        
        updateSelectInput(session,"var",'Choose a variable to map', choices = co_columns[-c(1,2)])  
        updateSelectInput(session,"var2",'Choose a variable to map (bubble size)', choices = co_columns[-c(1,2)])
        #updateSelectInput(session,"glevel",'Geography Level of Data', choices = c("metro","county"), selected = "county")  
        
        
        co_all %>%
          
          filter(stco_code %in% co_codes) %>%
          
          dplyr::select(co_columns) %>%
          
          unique() %>%          
          
          #left_join(county_cbsa_st %>% dplyr::select(contains("co_"),"cbsa_code","cbsa_name") %>% unique(), by = "stco_code")%>%
          
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
  
  
  
  lower48 <- reactive({
    (if(input$hiak == TRUE){borders50} else {borders48}) + tm_shape(input_data1(), projection = 2163) + tmapper() + tm_layout(
      legend.position = c("LEFT","BOTTOM"),
      legend.outside = FALSE,
      legend.title.size = .0001,
      title.position = c("LEFT", "BOTTOM"),
      title = input$title,
      title.size = 1.5,
      fontfamily = "serif") 
  })
  
  
  reactive({
    
    req(input$filt == TRUE)
    
    req(input$year)
    
    info2<-filter(info2(), year == input$year)   
    
  })
  
  
  
  output$contents <- renderTable({
    
    
    
    display_data <- info()
    
    
    head(display_data, 4L)
    
    
    
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
    
    lower_48<-lower48()
    
    
    tmap_leaflet(lower_48)
    
    
    
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
                   popup.vars=c(input$var, input$var2, "name"),
                   popup.format = list(text.align = "left", format = "f", digits = 3)
                   
        )
      } else {
        
        tm_bubbles(col = input$var, 
                   size = input$var2, 
                   palette = c(input$low, input$high),
                   style = input$style,
                   popup.vars=c(input$var, input$var2, "name"),
                   popup.format = list(text.align = "left", format = "f", digits = 3)
                   
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
                    ...
        )
      } else {
        
        tm_polygons(input$var, 
                    
                    palette = c(input$low, input$high),
                    style = input$style,
                    popup.vars=c(input$var, "name"),
                    popup.format = list(text.align = "left", format = "f", digits = 3),
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
      
      tmap_save(usborders + tm_shape(input_data1(), projection = 2163) +
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
    
    content = function(file) {
      
      
      write_csv(info2(),path = file)
      
    }
  ) 
  
  
  
  
}

# Run app ----

shinyApp(ui, server)


