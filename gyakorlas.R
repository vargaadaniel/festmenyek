library(shiny)
library(readr)
festmenyek <- read.csv("~/rGyakorlas/festmenyek.csv", sep=";")
View(festmenyek)
muzeumok <- read.csv("~/rGyakorlas/muzeumok.csv", sep=";")
View(muzeumok)
festok <- read.csv("~/rGyakorlas/festok.csv", sep=";")
View(festok)
kepek <- read.csv("~/rGyakorlas/kep.csv", sep=";")
View(kepek)
library(tidyverse)
library(grid)
links <- c(festmenyek$link[1],festmenyek$link[2],festmenyek$link[3])


data<-data.frame(
  id = c(festmenyek$id[1],festmenyek$id[2],festmenyek$id[3]),
  content = c(festmenyek$alkoto[1],festmenyek$alkoto[2],festmenyek$alkoto[3]),
  start = c(festmenyek$datum[1],festmenyek$datum[2],festmenyek$datum[3]),
  end = c(NA,NA,NA)
  )

ui<-fluidPage(
  titlePanel("FestmÃ©nyek"),
  sidebarPanel(
    
    timevisOutput("appts"),
    div("Selected items:",textOutput("selected"),inline = TRUE),
    div("visible window:",textOutput("window",inline = TRUE)),
    tableOutput(""),
    
    tags$button(
      id = "alkoto1",
      class = "btn action-button",
      tags$image(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f4/The_Scream.jpg/250px-The_Scream.jpg",height = "150px",width="150px")
    ),
    tags$button(
      id = "alkoto2",
      class = "btn action-button",
      tags$image(src = "https://cdn.nwmgroups.hu/s/img/i/1604/20160429leonardo-da-vinci-mona-lisa.jpg",height = "150px",width="150px")
    ),
    tags$button(
      id = "alkoto3",
      class = "btn action-button",
      tags$image(src = "https://upload.wikimedia.org/wikipedia/commons/6/66/Johannes_Vermeer_%281632-1675%29_-_The_Girl_With_The_Pearl_Earring_%281665%29.jpg",height = "150px",width="150px")
    )
  ),
  mainPanel(
    verbatimTextOutput("alk1_festmeny"),
    verbatimTextOutput("alk1_festo"),
    verbatimTextOutput("alk1_muzeum")
  )
)
server <- function(input, output){
  output$appts <- renderTimevis(
    timevis(
      data,
      option = list(editable = TRUE, multiselect = TRUE, align = "center")
    )
  )
  output$selected <- renderText(
    paste(input$appts_selected, collapse = " ")
  )
  output$window <- renderText(
    paste(input$appts_window[1],"to",input$appts_window[2])
  )
  output$table <- renderTable(
    input$appts_data
  )
  observeEvent(input$alkoto1,{
    output$alk1_festmeny <- renderText({
      paste(festmenyek$cim[1],festmenyek$leiras[1], sep="\n")
    })
    output$alk1_festo <- renderText({
      paste(festmenyek$alkoto[1],festok$leiras[1], sep="\n")
    })
    output$alk1_muzeum <- renderText({
      paste(festmenyek$muzeum[1],muzeumok$leiras[1], sep="\n")
    })
  })
  observeEvent(input$alkoto2,{
    output$alk1_festmeny <- renderText({
      paste(festmenyek$cim[2],festmenyek$leiras[2], sep="\n")
    })
    output$alk1_festo <- renderText({
      paste(festmenyek$alkoto[2],festok$leiras[2], sep="\n")
    })
    output$alk1_muzeum <- renderText({
      paste(festmenyek$muzeum[2],muzeumok$leiras[2], sep="\n")
    })
  })
  observeEvent(input$alkoto3,{
    output$alk1_festmeny <- renderText({
      paste(festmenyek$cim[3],festmenyek$leiras[3], sep="\n")
    })
    output$alk1_festo <- renderText({
      paste(festmenyek$alkoto[3],festok$leiras[3], sep="\n")
    })
    output$alk1_muzeum <- renderText({
      paste(festmenyek$muzeum[3],muzeumok$leiras[3], sep="\n")
    })
  })
}

shinyApp(ui = ui, server = server)

###
library(shiny)
library(readr)
library(ggplot2)
library(png)
library(grid)
library(tidyverse)
library(grid)
setwd("C:/Users/DÃ¡niel/Documents/rGyakorlas")
image_1 <- readPNG("sikoly.png")
image_2 <- readPNG("monalisa.png")
image_3 <- readPNG("gyongy.png")
g1 <- rasterGrob(image_1, interpolate = TRUE)
g2 <- rasterGrob(image_2, interpolate = TRUE)
g3 <- rasterGrob(image_3, interpolate = TRUE)
data<-data.frame(
  id = c(festmenyek$id[1],festmenyek$id[2],festmenyek$id[3]),
  content = c(festmenyek$alkoto[1],festmenyek$alkoto[2],festmenyek$alkoto[3]),
  start = c(festmenyek$datum[1],festmenyek$datum[2],festmenyek$datum[3]),
  end = c(NA,NA,NA)
)
ggplot(data, aes(content,start)) + geom_point() + annotation_custom(g1,xmin=0.5,xmax=1.5,ymin=festmenyek$datum[1]-50,ymax=festmenyek$datum[1]+50) + annotation_custom(g3, xmin = 1.5,xmax=2.5,ymin=festmenyek$datum[3]-50,ymax=festmenyek$datum[3]+50)+annotation_custom(g2,xmin=2.5,xmax=3.5,ymin=festmenyek$datum[2]-50,ymax=festmenyek$datum[2]+50)

###
ui<-dashboardPage(
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Festmények leírással", tabName = "festmenyek", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "festmenyek",
      fluidRow(
    box(valueBoxOutput("value1"),valueBoxOutput("value2"),valueBoxOutput("value3"),width=1500),
    box(plotOutput("myplot",click="myplot_click",width=750),
    tags$button(
      id = "alkoto2",
      class = "btn action-button",
      tags$image(src = "https://cdn.nwmgroups.hu/s/img/i/1604/20160429leonardo-da-vinci-mona-lisa.jpg",height = "150px",width="150px"),
      style="margin-left:40px; margin-top:15px;margin-bottom:20px;"
    ),
    tags$button(
      id = "alkoto3",
      class = "btn action-button",
      tags$image(src = "https://upload.wikimedia.org/wikipedia/commons/6/66/Johannes_Vermeer_%281632-1675%29_-_The_Girl_With_The_Pearl_Earring_%281665%29.jpg",height = "150px",width="150px"),
      style="margin-left:0px; margin-top:15px;margin-bottom:20px;"
    ),
    tags$button(
      id = "alkoto1",
      class = "btn action-button",
      tags$image(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f4/The_Scream.jpg/250px-The_Scream.jpg",height = "150px",width="150px"),
      style="margin-left:100px; margin-top:15px; margin-bottom:20px;"
    )),
      box(height = 620,
      verbatimTextOutput("alk1_festmeny",placeholder=TRUE),tags$head(tags$style("#alk1_festmeny{white-space:pre-wrap;height:190px;overflow-y:scroll;font-style:italic;}")),
      verbatimTextOutput("alk1_festo",placeholder=TRUE),tags$head(tags$style("#alk1_festo{white-space:pre-wrap;height:190px;overflow-y:scroll;font-style:italic;}")),
      verbatimTextOutput("alk1_muzeum",placeholder=TRUE),tags$head(tags$style("#alk1_muzeum{white-space:pre-wrap;height:190px;overflow-y:scroll;font-style:italic;}")),)
      )
      ),
    tabItem(tabName = "dashboard")
      )
    )
  )
server <- function(input,output){
  total_festmeny <- sum(festmenyek$datum_count)
  legfiatalabb_kep <- 0
  for(i in 1:length(festmenyek$datum)){
    if(year(festmenyek$datum[i]) > legfiatalabb_kep)
    {
      legfiatalabb_kep <- year(festmenyek$datum[i])
    }  
  }
  legidosebb_kep <- legfiatalabb_kep
  for(i in 1:length(festmenyek$datum)){
    if(legidosebb_kep > year(festmenyek$datum[i])){
      legidosebb_kep <- year(festmenyek$datum[i])
    }
  }
  output$myplot <- renderPlot({
    timeline
  })
  output$value1 <- renderValueBox({
    valueBox(
      formatC(total_festmeny, format="d", big.mark = ','),
      'Összes Festmény',
      icon = icon("stats", lib = 'glyphicon'),
      color = "blue"
    )
  })
  output$value2 <- renderValueBox({
    valueBox(
      formatC(legfiatalabb_kep, format="d"),
      'Legfiatalabb festmény létrejötte',
      icon = icon("stats", lib = 'glyphicon'),
      color = "green"
    )
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC(legidosebb_kep, format="d"),
      'Legidõsebb festmény létrejötte',
      icon = icon("stats", lib = 'glyphicon'),
      color = "yellow"
    )
  })
  count_input1 = 0;
  observeEvent(input$alkoto1,{
    if((count_input1 %% 2) == 0){
    output$alk1_festmeny <- renderText({
      paste(festmenyek$cim[3],festmenyek$leiras[3], sep="\n")
    })
    output$alk1_festo <- renderText({
      paste(festmenyek$alkoto[3],festok$leiras[3], sep="\n")
    })
    output$alk1_muzeum <- renderText({
      paste(festmenyek$muzeum[3],muzeumok$leiras[3], sep="\n")
    })
    count_input1 <- count_input1 + 1
    }
    else{output$alk1_festmeny <- renderText({
      paste("",sep="\n")
    })
    output$alk1_festo <- renderText({
      paste("",sep="\n")
    })
    output$alk1_muzeum <- renderText({
      paste("",sep="\n")
    })
    count_input1 <- count_input1 + 1
    }
    
    
  })
  observeEvent(input$alkoto2,{
    output$alk1_festmeny <- renderText({
      paste(festmenyek$cim[1],festmenyek$leiras[1], sep="\n")
    })
    output$alk1_festo <- renderText({
      paste(festmenyek$alkoto[1],festok$leiras[1], sep="\n")
    })
    output$alk1_muzeum <- renderText({
      paste(festmenyek$muzeum[1],muzeumok$leiras[1], sep="\n")
    })
  })
  observeEvent(input$alkoto3,{
    output$alk1_festmeny <- renderText({
      paste(festmenyek$cim[2],festmenyek$leiras[2], sep="\n")
    })
    output$alk1_festo <- renderText({
      paste(festmenyek$alkoto[2],festok$leiras[2], sep="\n")
    })
    output$alk1_muzeum <- renderText({
      paste(festmenyek$muzeum[2],muzeumok$leiras[2], sep="\n")
    })
  })
}
shinyApp(ui=ui, server = server)


#### Újrakezdés####
library(shiny)
library(readr)
library(ggplot2)
library(png)
library(grid)
library(tidyverse)
library(grid)
library(shinydashboard)
library(base)
library(ggplot2)
library(scales)
library(lubridate)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(maps)
library(ggplot2)
library(dplyr)
library(maps)
festmenyek <- read.csv("~/rGyakorlas/festmenyek.csv", sep=";")
View(festmenyek)
muzeumok <- read.csv("~/rGyakorlas/muzeumok.csv", sep=";")
View(muzeumok)
festok <- read.csv("~/rGyakorlas/festok.csv", sep=";")
View(festok)
kepek <- read.csv("~/rGyakorlas/kep.csv", sep=";")
View(kepek)
festmenyek$kepek <- c("sikoly.png", "monalisa.png", "gyongy.png")
festmenyek$honap <- sample(1:12,3)
festmenyek$nap <- sample(1:28,3)
festmenyek$datum <- with(festmenyek, ymd(sprintf('%04d%02d%02d', festmenyek$datum, honap, nap)))
festmenyek <- festmenyek[with(festmenyek, order(datum_hosszu)),]
korszak <- c("Reneszánsz","Barokk","Epresszionizmus")
korszak_szin <- c("#0070C0","#00B050","#FFC000")
festmenyek$status <- factor(festmenyek$status, levels = korszak, ordered = TRUE)
festmenyek$korszak[1] <- korszak[1]
festmenyek$korszak[2] <- korszak[2]
festmenyek$korszak[3] <- korszak[3]
positions <- c(-0.7, -0.7, -0.7)
directions <- c(-1, -1)
line_pos <- data.frame(
  "datum"=unique(festmenyek$datum),
  "position"=rep(positions, length.out=length(unique(festmenyek$datum))),
  "direction"=rep(directions, length.out=length(unique(festmenyek$datum)))
)
festmenyek <- merge(x = festmenyek, y = line_pos,by="datum", all = TRUE)
festmenyek <- festmenyek[with(festmenyek, order(datum,korszak)),]
text_offset <- 0.1
festmenyek$datum_count <- ave(festmenyek$datum == festmenyek$datum, festmenyek$datum, FUN=cumsum)
festmenyek$text_position <- (festmenyek$datum_count * text_offset * festmenyek$direction) + festmenyek$position
month_buffer <- 2
month_date_range <- seq(min(festmenyek$datum) - months(month_buffer), max(festmenyek$datum) + months(month_buffer), by='month')
month_format <- format(month_date_range, '%b')
month_festmenyek <- data.frame(month_date_range, month_format)
year_date_range <- seq(min(festmenyek$datum) - months(month_buffer), max(festmenyek$datum) + months(month_buffer), by='year')
year_date_range <- as.Date(
  intersect(
    ceiling_date(year_date_range,unit="year"),
    floor_date(year_date_range, unit="year")
  ),
  origin = "1970-01-01"
)
year_format <- format(year_date_range,'%Y')
year_festmenyek <- data.frame(year_date_range,year_format)

shift_axis <- function(p, xmin, xmax, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(y=y)
  ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))), 
                        ymax=y, ymin=y) +
    annotate("segment", y = 0, yend = 0, x = xmin, xend = xmax, 
             arrow = arrow(length = unit(0.1, "inches"))) +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x=element_blank())
}


image_1 <- readPNG("sikoly.png")
image_2 <- readPNG("monalisa.png")
image_3 <- readPNG("gyongy.png")
g1 <- rasterGrob(image_1, interpolate = TRUE)
g2 <- rasterGrob(image_2, interpolate = TRUE)
g3 <- rasterGrob(image_3, interpolate = TRUE)
timeline_plot <- ggplot(festmenyek,aes(datum,0,col=korszak, label=cim))
timeline_plot <- timeline_plot+labs(col="Korszakok:")
timeline_plot <- timeline_plot + scale_color_manual(values=korszak_szin,labels=korszak,drop=FALSE)
timeline_plot <- timeline_plot + theme_minimal()
timeline_plot <- timeline_plot+geom_hline(yintercept=0,color="black",size=1.0)
timeline_plot <- timeline_plot+geom_segment(data=festmenyek[festmenyek$datum_count == 1], aes(y=position, yend=0,xend=datum),color = korszak_szin, size=1.0) 
timeline_plot <- timeline_plot + geom_point(aes(y=0),size = 4)
timeline_plot <- timeline_plot + theme(axis.line.y=element_blank(),
                                       axis.text.y=element_blank(),
                                       axis.title.x=element_blank(),
                                       axis.title.y=element_blank(),
                                       axis.ticks.y=element_blank(),
                                       axis.ticks.x =element_blank(),
                                       axis.line.x =element_blank(),
                                       axis.text.x = element_text(size = 15),
                                       legend.position = "top"
)
timeline_plot <- timeline_plot+geom_text(data = festmenyek, aes(x = datum, y = 0.1, label = datum, fontface = "bold"), size = 6, color = korszak_szin)
timeline_plot <- timeline_plot + geom_text(aes(y=text_position, label = cim), size = 4.5)
timeline_plot <- timeline_plot + expand_limits(x = c(ymd(14500101),ymd(19500101)),y = 1.2)
timeline <- shift_axis(timeline_plot, ymd(15000101), ymd(20000101))
show(timeline)


##Map##

some.eu.countries <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands",
  "Denmark", "Poland", "Italy", 
  "Croatia", "Slovenia", "Hungary", "Slovakia",
  "Czech republic"
)
some.eu.maps <- map_data("world",region = some.eu.countries)
region.lab.data <- some.eu.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(some.eu.maps, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")



##Elvetett ötlet##

data <- tribble(~start_date, ~event, ~displ,
                festmenyek$datum[1], festmenyek$cim[1], -0.7,
                festmenyek$datum[2], festmenyek$cim[2],-0.7,
                festmenyek$datum[3],festmenyek$cim[3],-0.7
                )

shift_axis <- function(p, xmin, xmax, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(y=y)
  ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))), 
                        ymax=y, ymin=y) +
    annotate("segment", y = 0, yend = 0, x = xmin, xend = xmax, 
             arrow = arrow(length = unit(0.1, "inches"))) +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x=element_blank())
}
library(ggplot2)
library(dplyr)
library(ggalt)
library(cowplot)
library(tibble)
library(lubridate)
p1 <- data %>% 
  ggplot(aes(start_date, displ)) +
  geom_lollipop(point.size = 1) +
  geom_text(aes(x = start_date, y = displ, label = event), data = data,
            hjust = 0, vjust = vjust, size = 6) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10)) +
  expand_limits(x = c(ymd(15000101), ymd(20000101)), y = 1.2) +
  scale_x_date(breaks = scales::pretty_breaks(n = 9))+ scale_color_manual(values=korszak_szin,labels=korszak,drop=FALSE)
timeline <- shift_axis(p1, ymd(15000101), ymd(20000101))