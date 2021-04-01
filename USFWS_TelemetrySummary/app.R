# Radio telemetry data summary and visualization shiny app
# Created by Jonah L. Withers
# Created on March 8, 2021
# The purpose of this script is to visualize radio telemetry data pertaining to Atlantic salmon tagged in the Winooski River, VT.
## The dataset include both adults moved upstream during fall spawning runs in 2019 and 2020 and will soon include smolts 
### Released either downriver of essex 19 or Winooski 1 in early april 2021. Data are cleaned via BIOTAS and manually. 




# Clean house! ----
rm(list = ls()) # Clear environment
gc() # Clear ram



# Libraries ----
library(shiny)
library(shinydashboard)
library(DT)
library(xts)
library(dygraphs)
library(leaflet)
library(dplyr)
library(data.table)
library(ggplot2)
library(scales)





# Import Data ----
# > Site coords ####
SiteCoords <- read.csv("../../Shiny_App_Data/SiteCoords.csv", 
                       header = TRUE,
                       stringsAsFactors = FALSE)

# > SumTbl coords ####
SumTbl <- read.csv("../../Shiny_App_Data/SumTbl.csv", 
                       header = TRUE,
                       stringsAsFactors = FALSE)

# > Import movement table ####
MovTbl <- read.csv("../../Shiny_App_Data/MovTbl.csv", 
                   header = TRUE,
                   stringsAsFactors = FALSE)

# > Bio data ####
BioData <- read.csv("../../Shiny_App_Data/BioData.csv", 
                    header = TRUE,
                    stringsAsFactors = FALSE) %>% 
    mutate(DateTimeRecovered = ifelse(is.na(DateTimeRecovered), "Not Recovered", DateTimeRecovered))

# > Fish coords ####
FishCoords <- read.csv("../../Shiny_App_Data/FishCoords.csv", 
                    header = TRUE,
                    stringsAsFactors = FALSE)

# > Beacon data ####
beacon <- read.csv("../../Shiny_App_Data/Beacon.csv", 
                   header = TRUE,
                   stringsAsFactors = FALSE)

# > Site coords ####
Noise <- read.csv("../../Shiny_App_Data/Noise.csv", 
                       header = TRUE,
                       stringsAsFactors = FALSE) %>% 
    mutate(Date = as.Date(Date))

Sites <- beacon %>%
    arrange(Site) %>%
    distinct(Site) %>%
    pull(Site)


TagSitePts <- FishCoords %>% 
    dplyr::select(TagID, Latitude, Longitude, DetDateTime, ColorGrad) %>% 
    rbind(SiteCoords %>% 
              dplyr::select(SiteName, Latitude_DD, Longitude_DD) %>%
              rename(TagID = SiteName, Latitude = Latitude_DD, Longitude = Longitude_DD) %>% 
              mutate(DetDateTime = as.POSIXct(NA),
                     ColorGrad = .5)) %>% 
    mutate(ColorRamp = ifelse(TagID %in% c("Winooski WWTP", "Winooski One", "BWWTP", "Gorge 18 Downstream",
                                    "Gorge 18 upstream", "Essex WWTP", "Essex 19", "Richmond Downstream",
                                    "Richmond Upstream", "Huntington Upstream", "Huntington Downstream",
                                    "Red House", "Grey House", "Bolton Falls"),
                       "orange", "blue"))






# User Interface ----
ui <- dashboardPage(skin = "black",
               
                    
                    
# > UI Header ####
    dashboardHeader(title = "Radio Telemetry"),



# > UI SideBar ####
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "overview", icon = icon("cog"),
                     badgeLabel = "New", badgeColor = "green"),
            menuItem("Summary", tabName = "summary", icon = icon("chart-pie"),
                     badgeLabel = "New", badgeColor = "green"),
            menuItem("Fish", tabName = "fish", icon = icon("fish"),
                     badgeLabel = "New", badgeColor = "green"),
            menuItem("QA / QC", tabName = "qaqc", icon = icon("balance-scale-right"),
                     badgeLabel = "New", badgeColor = "green")
        )
    ),


# > UI Body ####
    dashboardBody(
        tabItems(
            # Overview
            tabItem("overview",
                    fluidPage(
                        h1("Radio Telemetry in the Winooski River"),
                        p("This radio telemetry project was designed to accomplish three objectives: 1) Determine fall back rate of Atlantic salmon lifted above Winooski One hydro-project, 2) elucidate behavioral patterns, movement, and habitat use, and 3) evaluate downstream migration of smolts.
                          Adult salmon are captured at the Winooski One dam and moved upriver to suspected improved spawning habitat each fall. Radio telemetry data is being used to identify movement and habitat use of these salmon between Essex 19 and Bolton Dam.
                          An important objective is to evaluate whether these lifted salmon are able to spawn prior to falling back over the Essex 19 dam. A recent addition to this project is the evaluation of evaluation of downstream passage and survival of smolts
                          released just downstream of the Winooski One Dam and just downstream of the Gorge 18 Dam. We would like to determine the survival between the time of release and the time the smolts enter the lake to see what the overall survival of smolts is to the lake but also to determine: 1)
                          the impact dams are having on survival and 2) the difference in survival between the two release locations."),
                        box(title = "Study Area",
                            footer = "Orange points denote stationary receivers, blue point denotes capture location, and black point denotes release location.",
                            solidHeader = TRUE,
                            status = "primary",
                            height = 500,
                            width = 8,
                            leafletOutput("SiteMap")
                            )
                        )
                    ),
            
            # Summary
            tabItem("summary",
                    fluidPage(
                        plotOutput("OutMigrationTable"),
                        h1("Summary of fish locations"),
                        dataTableOutput("summary_table"),
                        h1("Distance traveled per week"),
                        dataTableOutput("movement_table")
                        )
                    ),
            
            # Fish
            tabItem("fish",
                    fluidRow(
                        # column(width = 3,
                               box(title = "Input", 
                                   solidHeader = TRUE, 
                                   status = "primary", 
                                   width = 3, 
                                   height = 150,
                                   # radioButtons("lifestage", "Year / Life stage:",
                                   #              c("2019 Adult Tags" = "2019 Adult",
                                   #                "2020 Adult Tags" = "2020 Adult",
                                   #                "2021 Smolt Tags" = "2021 Smolt")),
                                   selectInput("TagID", "TagID:",
                                               c("164.380 185", "164.380 186", "164.380 179", "164.380 160", "164.380 175",
                                                 "164.380 167", "164.380 183", "164.380 155", "164.380 165", "164.380 172",
                                                 "164.380 156", "164.380 184", "164.380 178", "164.380 164", "164.380 189",
                                                 "164.380 170", "164.380 161", "164.380 154", "164.380 177", "164.380 163",
                                                 "164.380 151", "164.380 180", "164.380 162", "164.380 171", "164.380 157",
                                                 "164.380 182", "164.380 174", "164.380 153", "164.380 168", "164.380 188",
                                                 "164.380 152", "164.380 166", "164.380 176", "164.380 181", "164.480 147",
                                                 "164.380 173", "164.480 131", "164.480 122", "164.480 149", "164.480 138",
                                                 "164.480 129", "164.380 169", "164.380 158", "164.480 114", "164.480 121",
                                                 "164.480 111", "164.380 159", "164.480 120", "164.480 150", "164.480 152",
                                                 "164.480 123", "164.480 112", "164.480 116", "164.480 151", "164.480 103",
                                                 "164.480 105", "164.480 117", "164.480 110", "164.480 128", "164.480 134",
                                                 "164.480 145", "164.480 136", "164.480 109", "164.480 115", "164.480 118",
                                                 "164.480 140", "164.480 113", "164.480 133", "164.480 143", "164.480 101",
                                                 "164.480 148", "164.480 139", "164.480 124", "164.480 146", "164.480 153",
                                                 "164.480 141", "164.480 132", "164.480 102", "164.480 126", "164.480 119",
                                                 "164.480 125", "164.480 135", "164.480 137", "164.480 144", "164.480 104",
                                                 "164.480 108", "164.480 106", "164.480 142", "164.480 155", "164.480 127",
                                                 "164.480 107", "164.480 130")
                                               )
                                   ),
                               box(title = "Bio Data",
                                   solidHeader = TRUE,
                                   width = 9,
                                   height = 300,
                                   background = "black",
                                   infoBoxOutput("ReleaseGroup"),
                                   infoBoxOutput("Sex"),
                                   infoBoxOutput("DateLastDet"),
                                   valueBoxOutput("ValTL"),
                                   valueBoxOutput("ValWeight"),
                                   valueBoxOutput("RecoveredDat")
                                   )
                               ),
                    
                    fluidRow(
                        box(title = "Map of Detections", 
                                 footer = "Blue points denote detections of fish while orange denote stationary receivers",
                                 solidHeader = TRUE, 
                                 width = 5, 
                                 height = 500,
                                 leafletOutput("mymap")
                                 )
                             ,
                    box(title = "Detection history plot", 
                        solidHeader = TRUE,
                        width = 7,
                        height = 650,
                        imageOutput("myImage")
                        )
                    )),
            
            # QA / QC
            tabItem("qaqc",
                    fluidRow(
                        box(dygraphOutput("beacon_plot1")),
                        box(plotOutput("noise_plot1")),
                        box(dygraphOutput("beacon_plot2")),
                        box(plotOutput("noise_plot2")),
                        box(dygraphOutput("beacon_plot3")),
                        box(plotOutput("noise_plot3")),
                        box(dygraphOutput("beacon_plot4")),
                        box(plotOutput("noise_plot4")),
                        box(dygraphOutput("beacon_plot5")),
                        box(plotOutput("noise_plot5")),
                        box(dygraphOutput("beacon_plot6")),
                        box(plotOutput("noise_plot6")),
                        box(dygraphOutput("beacon_plot7")),
                        box(plotOutput("noise_plot7")),
                        box(dygraphOutput("beacon_plot8")),
                        box(plotOutput("noise_plot8")),
                        box(dygraphOutput("beacon_plot9")),
                        box(plotOutput("noise_plot9")),
                        box(dygraphOutput("beacon_plot10")),
                        box(plotOutput("noise_plot10")),
                        box(dygraphOutput("beacon_plot11")),
                        box(plotOutput("noise_plot11")),
                        box(dygraphOutput("beacon_plot12")),
                        box(plotOutput("noise_plot12")),
                        box(dygraphOutput("beacon_plot13")),
                        box(plotOutput("noise_plot13")),
                        box(dygraphOutput("beacon_plot14")),
                        box(plotOutput("noise_plot14")),
                        box(dygraphOutput("beacon_plot15")),
                        box(plotOutput("noise_plot15")),
                        box(dygraphOutput("beacon_plot16")),
                        box(plotOutput("noise_plot16")),
                        box(dygraphOutput("beacon_plot17")),
                        box(plotOutput("noise_plot17")),
                        box(dygraphOutput("beacon_plot18")),
                        box(plotOutput("noise_plot18")),
                        box(dygraphOutput("beacon_plot19")),
                        box(plotOutput("noise_plot19")),
                        box(dygraphOutput("beacon_plot20")),
                        box(plotOutput("noise_plot20")),
                        box(dygraphOutput("beacon_plot21")),
                        box(plotOutput("noise_plot21")),
                        box(dygraphOutput("beacon_plot22")),
                        box(plotOutput("noise_plot22")),
                        box(dygraphOutput("beacon_plot23")),
                        box(plotOutput("noise_plot23"))
                        )
                    )
            )
        )
)










# Server ----
server <- function(input, output) {
    
    # > Create barplot of % Fish location
    output$OutMigrationTable <- renderPlot({
        SumTbl %>%
            mutate(River_Segment = factor(River_Segment,
                                          levels = c(
                                              "River Mouth",
                                              "Below Winooski One",
                                              "Below Gorge",
                                              "Below Essex 19",
                                              "Essex 19 to Richmond",
                                              "Richmond to Jonesville",
                                              "Above Jonesville"))) %>%
            ggplot() +
            geom_col(aes(x = River_Segment, y = Proportion), fill = "tomato3") +
            facet_wrap(. ~ ReleaseYear,
                       nrow = 1) +
            scale_y_continuous(limits = c(0, 100)) +
            coord_flip() +
            ggtitle("Fish Locations") +
            xlab("River Section") +
            ylab("Proportion of Fish (100%)") +
            theme_bw(base_size = 16)
    })
    
    
    # > Render datatables ####
    output$summary_table <- renderDataTable(SumTbl %>%
                                                mutate(River_Segment = factor(River_Segment, 
                                                                              levels = c( 
                                                                                  "River Mouth",
                                                                                  "Below Winooski One", 
                                                                                  "Below Gorge", 
                                                                                  "Below Essex 19", 
                                                                                  "Essex 19 to Richmond", 
                                                                                  "Richmond to Jonesville",
                                                                                  "Above Jonesville"))) %>%
                                                dplyr::select(River_Segment, ReleaseYear, Salmon) %>% 
                                                as.data.table() %>% 
                                                dcast(River_Segment ~ ReleaseYear, value.var = "Salmon"))
    
    output$movement_table <- renderDataTable(MovTbl #%>% 
                                                 # formatStyle(
                                                 #     columns = c("00", "01", "02", "03", "04", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52"), 
                                                 #     backgroundColor = styleInterval(400, c('red', 'green')),
                                                 #     fontWeight = 'bold')
                                             )
    
    
    
    # > Render Info and Value Boxes ####
    output$ValTL <- renderValueBox({
        valueBox(paste0(BioData %>%
                            filter(TagID == input$TagID) %>%
                            dplyr::select(TL) %>%
                            pull(), "(cm)"), 
                 subtitle = "Total Length (cm)",
                 icon = icon("ruler"),
                 color = "red",
                 width = 1
        )
    })
    
    output$ValWeight <- renderValueBox({
        valueBox(paste0(BioData %>%
                            filter(TagID == input$TagID) %>%
                            dplyr::select(Weight) %>%
                            pull(), "(kg)"),
                 subtitle = "Weight (kg)",
                 icon = icon("weight-hanging"),
                 color = "blue",
                 width = 1
        )
    })
    
    output$RecoveredDat <- renderValueBox({
        valueBox(paste0(BioData %>%
                            filter(TagID == input$TagID) %>%
                            dplyr::select(DateTimeRecovered) %>%
                            pull()),
                 subtitle = "Tag Recovered",
                 icon = icon("tags"),
                 color = "yellow",
                 width = 1
        )
    })
    
    
    output$Sex <- renderInfoBox({
        infoBox(title = "Sex",
                value = BioData %>%
                    filter(TagID == input$TagID) %>%
                    dplyr::select(Sex) %>%
                    pull(),
                icon = icon("venus-mars"),
                color = "purple",
                width = 1,
                fill = TRUE
        )
    })
    
    output$ReleaseGroup <- renderInfoBox({
        infoBox(title = "Fish Group",
                value = BioData %>%
                    filter(TagID == input$TagID) %>%
                    dplyr::select(FishGroup) %>%
                    pull(),
            # subtitle = "Weight (kg)",
            icon = icon("fish"),
            color = "green",
            width = 1,
            fill = TRUE
        )
    })
    
    output$DateLastDet <- renderInfoBox({
        infoBox(title = "Last Detected",
                value = as.character(FishCoords %>%
                    filter(TagID == input$TagID) %>%
                    arrange(desc(DetDateTime)) %>% 
                    distinct(TagID, .keep_all = TRUE) %>% 
                    dplyr::select(DetDateTime) %>%
                    pull()),
                icon = icon("binoculars"),
                color = "maroon",
                width = 1,
                fill = TRUE
        )
    })

    # > Render Files ####
    output$myImage <- renderImage({
        filename <- normalizePath(file.path("../../Figures/PostClean/",
                                                paste("Fish", input$TagID, "_", BioData %>% 
                                                          filter(TagID == input$TagID) %>% 
                                                          dplyr::select(Sex) %>% slice(.,1) ,"_2020.png", sep = "")))

        # Return a list containing the filename
        list(src = filename,
             height = 600, 
             width = 800)
        }, deleteFile = FALSE)

    
    
    
    # > Render Leaflet Site Map ####
    output$SiteMap <- renderLeaflet({
        leaflet(SiteCoords) %>%
            addProviderTiles("Esri.WorldTopoMap", group = "Satellite") %>%
            addCircleMarkers(lng = ~ Longitude_DD,
                             lat = ~ Latitude_DD,
                             popup = ~ SiteName,
                             label = ~ SiteName,
                             opacity = 1,
                             radius = 2,
                             color = ~ SiteType
                             ) %>%
            addScaleBar(position = "bottomleft")
    })

    output$mymap <- renderLeaflet({
        leaflet(TagSitePts %>%
                    dplyr::filter(TagID %in% c(input$TagID, "Winooski WWTP", "Winooski One", "BWWTP", "Gorge 18 Downstream",
                                               "Gorge 18 upstream", "Essex WWTP", "Essex 19", "Richmond Downstream",
                                               "Richmond Upstream", "Huntington Upstream", "Huntington Downstream",
                                               "Red House", "Grey House", "Bolton Falls"))) %>%
            addProviderTiles("Esri.WorldTopoMap", group = "Satellite") %>%
            addCircleMarkers(lng = ~ Longitude,
                             lat = ~ Latitude,
                             label = ~ DetDateTime,
                             radius = 2,
                             popup = ~ DetDateTime,
                             color = ~ ColorRamp,
                             opacity = ~ ColorGrad) %>%
            addScaleBar(position = "bottomleft")
    })    
    
    
    
    # > Render Beacon Dygraphs ####
    output$beacon_plot1 <- renderDygraph({xts(x = beacon %>%
                                                  filter(Site == Sites[1]) %>%
                                                  ungroup() %>%
                                                  select(detections),
                                              as.Date(beacon %>%
                                                          filter(Site == Sites[1]) %>%
                                                          ungroup() %>%
                                                          pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[1], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[1]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -95, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00")) 
    })
    
    output$beacon_plot2 <- renderDygraph({xts(x = beacon %>%
                                                  filter(Site == Sites[2]) %>%
                                                  ungroup() %>%
                                                  select(detections),
                                              as.Date(beacon %>%
                                                          filter(Site == Sites[2]) %>%
                                                          ungroup() %>%
                                                          pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[2], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[2]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -75, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })
    
    output$beacon_plot3 <- renderDygraph({xts(x = beacon %>%
                                                  filter(Site == Sites[3]) %>%
                                                  ungroup() %>%
                                                  select(detections),
                                              as.Date(beacon %>%
                                                          filter(Site == Sites[3]) %>%
                                                          ungroup() %>%
                                                          pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[3], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[3]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -83, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })
    
    output$beacon_plot4 <- renderDygraph({xts(x = beacon %>%
                                                  filter(Site == Sites[4]) %>%
                                                  ungroup() %>%
                                                  select(detections),
                                              as.Date(beacon %>%
                                                          filter(Site == Sites[4]) %>%
                                                          ungroup() %>%
                                                          pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[4], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[4]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -77, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })
    
    output$beacon_plot5 <- renderDygraph({xts(x = beacon %>%
                                                  filter(Site == Sites[5]) %>%
                                                  ungroup() %>%
                                                  select(detections),
                                              as.Date(beacon %>%
                                                          filter(Site == Sites[5]) %>%
                                                          ungroup() %>%
                                                          pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[5], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[5]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -98, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })
    
    output$beacon_plot6 <- renderDygraph({xts(x = beacon %>%
                                                  filter(Site == Sites[6]) %>%
                                                  ungroup() %>%
                                                  select(detections),
                                              as.Date(beacon %>%
                                                          filter(Site == Sites[6]) %>%
                                                          ungroup() %>%
                                                          pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[6], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[6]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -100, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })
    
    output$beacon_plot7 <- renderDygraph({xts(x = beacon %>%
                                                  filter(Site == Sites[7]) %>%
                                                  ungroup() %>%
                                                  select(detections),
                                              as.Date(beacon %>%
                                                          filter(Site == Sites[7]) %>%
                                                          ungroup() %>%
                                                          pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[7], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[7]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -63, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })
    
    output$beacon_plot8 <- renderDygraph({xts(x = beacon %>%
                                                  filter(Site == Sites[8]) %>%
                                                  ungroup() %>%
                                                  select(detections),
                                              as.Date(beacon %>%
                                                          filter(Site == Sites[8]) %>%
                                                          ungroup() %>%
                                                          pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[8], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[8]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -60, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })
    
    output$beacon_plot9 <- renderDygraph({xts(x = beacon %>%
                                                  filter(Site == Sites[9]) %>%
                                                  ungroup() %>%
                                                  select(detections),
                                              as.Date(beacon %>%
                                                          filter(Site == Sites[9]) %>%
                                                          ungroup() %>%
                                                          pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[9], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[9]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -68, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })
    
    output$beacon_plot10 <- renderDygraph({xts(x = beacon %>%
                                                  filter(Site == Sites[10]) %>%
                                                  ungroup() %>%
                                                  select(detections),
                                              as.Date(beacon %>%
                                                          filter(Site == Sites[10]) %>%
                                                          ungroup() %>%
                                                          pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[10], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[10]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -90, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })
    
    output$beacon_plot11 <- renderDygraph({xts(x = beacon %>%
                                                  filter(Site == Sites[11]) %>%
                                                  ungroup() %>%
                                                  select(detections),
                                              as.Date(beacon %>%
                                                          filter(Site == Sites[11]) %>%
                                                          ungroup() %>%
                                                          pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[11], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[11]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -87, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })
    
    output$beacon_plot12 <- renderDygraph({xts(x = beacon %>%
                                                  filter(Site == Sites[12]) %>%
                                                  ungroup() %>%
                                                  select(detections),
                                              as.Date(beacon %>%
                                                          filter(Site == Sites[12]) %>%
                                                          ungroup() %>%
                                                          pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[12], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[12]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -77, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })
    
    output$beacon_plot13 <- renderDygraph({xts(x = beacon %>%
                                                  filter(Site == Sites[13]) %>%
                                                  ungroup() %>%
                                                  select(detections),
                                              as.Date(beacon %>%
                                                          filter(Site == Sites[13]) %>%
                                                          ungroup() %>%
                                                          pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[13], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[13]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -80, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })

    output$beacon_plot14 <- renderDygraph({xts(x = beacon %>%
                                                   filter(Site == Sites[14]) %>%
                                                   ungroup() %>%
                                                   select(detections),
                                               as.Date(beacon %>%
                                                           filter(Site == Sites[14]) %>%
                                                           ungroup() %>%
                                                           pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[14], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[14]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -90, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })

    output$beacon_plot15 <- renderDygraph({xts(x = beacon %>%
                                                   filter(Site == Sites[15]) %>%
                                                   ungroup() %>%
                                                   select(detections),
                                               as.Date(beacon %>%
                                                           filter(Site == Sites[15]) %>%
                                                           ungroup() %>%
                                                           pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[15], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[15]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -90, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })

    output$beacon_plot16 <- renderDygraph({xts(x = beacon %>%
                                                   filter(Site == Sites[16]) %>%
                                                   ungroup() %>%
                                                   select(detections),
                                               as.Date(beacon %>%
                                                           filter(Site == Sites[16]) %>%
                                                           ungroup() %>%
                                                           pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[16], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[16]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -90, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })
    
    output$beacon_plot17 <- renderDygraph({xts(x = beacon %>%
                                                   filter(Site == Sites[17]) %>%
                                                   ungroup() %>%
                                                   select(detections),
                                               as.Date(beacon %>%
                                                           filter(Site == Sites[17]) %>%
                                                           ungroup() %>%
                                                           pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[17], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[17]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -90, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })
    
    
    output$beacon_plot18 <- renderDygraph({xts(x = beacon %>%
                                                   filter(Site == Sites[18]) %>%
                                                   ungroup() %>%
                                                   select(detections),
                                               as.Date(beacon %>%
                                                           filter(Site == Sites[18]) %>%
                                                           ungroup() %>%
                                                           pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[18], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[18]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -90, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })
    
    output$beacon_plot19 <- renderDygraph({xts(x = beacon %>%
                                                   filter(Site == Sites[19]) %>%
                                                   ungroup() %>%
                                                   select(detections),
                                               as.Date(beacon %>%
                                                           filter(Site == Sites[19]) %>%
                                                           ungroup() %>%
                                                           pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[19], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[19]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -90, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })
    
    output$beacon_plot20 <- renderDygraph({xts(x = beacon %>%
                                                   filter(Site == Sites[20]) %>%
                                                   ungroup() %>%
                                                   select(detections),
                                               as.Date(beacon %>%
                                                           filter(Site == Sites[20]) %>%
                                                           ungroup() %>%
                                                           pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[20], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[20]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -90, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })
    
    output$beacon_plot21 <- renderDygraph({xts(x = beacon %>%
                                                   filter(Site == Sites[21]) %>%
                                                   ungroup() %>%
                                                   select(detections),
                                               as.Date(beacon %>%
                                                           filter(Site == Sites[21]) %>%
                                                           ungroup() %>%
                                                           pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[21], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[21]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -90, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })
    
    output$beacon_plot22 <- renderDygraph({xts(x = beacon %>%
                                                   filter(Site == Sites[22]) %>%
                                                   ungroup() %>%
                                                   select(detections),
                                               as.Date(beacon %>%
                                                           filter(Site == Sites[22]) %>%
                                                           ungroup() %>%
                                                           pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[22], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[22]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -90, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })
    
    output$beacon_plot23 <- renderDygraph({xts(x = beacon %>%
                                                   filter(Site == Sites[23]) %>%
                                                   ungroup() %>%
                                                   select(detections),
                                               as.Date(beacon %>%
                                                           filter(Site == Sites[23]) %>%
                                                           ungroup() %>%
                                                           pull(Date))) %>% 
            dygraph(main = paste("Beacon at Station:",
                                 Sites[23], sep = " ")) %>%
            dyRangeSelector() %>%
            dyOptions(drawPoints = TRUE, pointSize = 2, fillGraph = FALSE) %>%
            dyAxis("y", label = "Number of Detections") %>%
            dyAxis("x", label = "Date") %>%
            dyLimit(as.numeric(288), color = "black") %>%
            dyShading(from = as.numeric(273), to = as.numeric(302),
                      axis = "y", color = "#CCEBD6") %>% 
            dyRibbon(data = beacon %>%
                         filter(Site == Sites[23]) %>%
                         ungroup() %>%
                         mutate(Power = ifelse(Power < -90, 1, 0)) %>% 
                         pull(Power), top = 0.2, bottom = 0.0,
                     palette = c("#009E73",  "#D55E00"))
    })
    
    
    # Render noise plots
    output$noise_plot1 <- renderPlot({
        Noise %>%
            filter(Site == Sites[1]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
             title = paste("Site:", Sites[1], sep = ""))+
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    output$noise_plot2 <- renderPlot({
        Noise %>%
            filter(Site == Sites[2]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[2], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot3 <- renderPlot({
        Noise %>%
            filter(Site == Sites[3]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[3], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot4 <- renderPlot({
        Noise %>%
            filter(Site == Sites[4]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[4], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot5 <- renderPlot({
        Noise %>%
            filter(Site == Sites[5]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[5], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot6 <- renderPlot({
        Noise %>%
            filter(Site == Sites[6]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[6], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot7 <- renderPlot({
        Noise %>%
            filter(Site == Sites[7]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[7], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot8 <- renderPlot({
        Noise %>%
            filter(Site == Sites[8]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[8], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot9 <- renderPlot({
        Noise %>%
            filter(Site == Sites[9]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[9], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot10 <- renderPlot({
        Noise %>%
            filter(Site == Sites[10]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[10], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot11 <- renderPlot({
        Noise %>%
            filter(Site == Sites[11]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[11], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot12 <- renderPlot({
        Noise %>%
            filter(Site == Sites[12]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[12], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot13 <- renderPlot({
        Noise %>%
            filter(Site == Sites[13]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[13], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot14 <- renderPlot({
        Noise %>%
            filter(Site == Sites[14]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[14], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    output$noise_plot15 <- renderPlot({
        Noise %>%
            filter(Site == Sites[15]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[15], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot16 <- renderPlot({
        Noise %>%
            filter(Site == Sites[16]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[16], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot17 <- renderPlot({
        Noise %>%
            filter(Site == Sites[17]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[17], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot18 <- renderPlot({
        Noise %>%
            filter(Site == Sites[18]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[18], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot19 <- renderPlot({
        Noise %>%
            filter(Site == Sites[19]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[19], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot20 <- renderPlot({
        Noise %>%
            filter(Site == Sites[20]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[20], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot21 <- renderPlot({
        Noise %>%
            filter(Site == Sites[21]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[21], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot22 <- renderPlot({
        Noise %>%
            filter(Site == Sites[22]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[22], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$noise_plot23 <- renderPlot({
        Noise %>%
            filter(Site == Sites[23]) %>%
            ggplot() +
            geom_bar(aes(x = Date, y = NoiseDet, fill = factor(Bin)),
                     color = "black",
                     stat = 'identity') +
            scale_x_date(labels = date_format("%Y-%m-%d"),
                         date_breaks = "2 day") +
            labs(x = "Date",
                 y = "Daily Noise Detections",
                 fill = "Power",
                 title = paste("Site:", Sites[23], sep = ""))+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    
    
    
    
    
    
    
    
    
    
}




# Run the application ----
shinyApp(ui = ui, server = server)
