library(shiny)
library(shinydashboard)
library(tidyverse)
library(reshape2)
library(lubridate)
library(knitr)
library(plotly)
library(ggpubr)
library(grid)
library(gridExtra)

# Define functions... Später ersetzt durch Aufruf auf vorverarbeitete Dateien. Extra Prozess auf Server ?
klinik <- function(datei) {
        ## Datei einlesen
        klinik <- read.csv2(file(datei, encoding = "ISO-8859-1"), header = T, stringsAsFactors = FALSE)
        ## Unwichtige Spalten entfernen
        kl <- klinik[, c(4:9, 16, 20, 27, 30, 31, 53:55, 86, 89, 120, 126, 142, 144, 146, 157, 162, 165, 166, 169, 178, 195, 199, 238)]
        ## EntlDat in Datum ?nder
        kl$EntlDat <- as.Date(kl$EntlDat, "%d.%m.%Y")
        kl$AufnDat <- as.Date(kl$AufnDat, "%d.%m.%Y")
        kl <- kl %>% mutate(yd = paste(quarter(EntlDat, with_year = TRUE))) %>% mutate(year = year(EntlDat)) %>% mutate(md = paste(year(EntlDat) - 2010, month(EntlDat), sep = "")) %>% mutate(md = as.numeric(md), yd = as.numeric(yd)) %>% mutate(mon = month(EntlDat))
        kl <- kl %>% mutate(mon_auf = month(AufnDat))
        kl
}

## Funktionen
abteil <- function(abtt) {
        fall %>% filter(abt == abtt) %>% group_by(jahr, form, mon) %>% summarise(drg = sum(effgewicht), faelle = n(), sn = sum(snzeit, na.rm = TRUE), verweil = sum(vwd, na.rm = TRUE)) %>% ungroup()
}

ermaech <- function(kvvar){
        fall %>% filter(kat==kvvar)
}                                
## Lesen der Dateien
f2017 <- klinik("../data/fallexport_2017.csv")
f2018 <- klinik("../data/fallexport.csv")
f2016<-klinik("../data/fallexport_2016_final.csv")
#plan <- read_csv2("../data/plan18.csv")
## Achtung: PLZ in 2018 sind chr
# f2017$PLZ <- as.integer(f2017$PLZ)
fall <- bind_rows(list(f2016,f2017, f2018))

## eliminiere falsche Jahre und externe
fall <- filter(fall, year %in% c(2016, 2017, 2018))
br <- unique(fall$mon) ## für x-Achse

## Ändere Namen
## Achtung händische Eingabe des letzten überschüssigen Quartals !!
colnames(fall) <- c("id", "sex", "alter", "plz", "ort", "form", "aufnahme", "entlassung", "abt", "kat", "einweiser", "ediag", "adiag", "hdiag", "ops1", "ops2", "op", "narkose", "snzeit", "arzt1", "arzt2", "rgaop", "vwd", "drg", "erloes", "ldrg", "effgewicht", "ehdiag", "wahlleist", "bsnr", "quartjahr", "jahr", "md", "mon", "mon_auf")
fall <- fall %>% mutate(einw = gsub("\\,.*", "", einweiser)) %>% mutate(form = ifelse(form == "vollstationär", "voll", form)) %>% filter(quartjahr!="2018.4")


## Funktionen

# plotter_abt <- function(df,xname, yname, ybez) {
#         x_var <- enquo(xname)
#         y_var <- enquo(yname)
#         d_df <- enquo(df)
#         
#         ggplot(!!d_df, aes(x = !!x_var, y = !!y_var)) + geom_point() + geom_line(data = stat[stat$jahr == 2018, ], color = "red") + geom_line(data = stat[stat$jahr == 2017, ], color = "blue") + theme_bw() + guides(colour = FALSE, alpha = FALSE, size = FALSE) + ggtitle(ybez, "Vorjahr (blau)") + labs(x = "Monat", y = ybez) + scale_x_continuous(breaks = br) + scale_y_continuous(limits = c(0, NA))
# }
# 
## ortho <- abteil("ORTH") Auswahl über Button
##stat <- ortho %>% filter(form == "voll")
##tab_s <- stat_tab()
##plotter_abt(mon, faelle, "Fälle")


# Define UI for app that draws a histogram ----
ui <- dashboardPage(
        
        # App title ----
        dashboardHeader(title= "Reporting"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Reporting", tabName = "report", icon = icon("dashboard")),
                        menuItem("Zuweiser", tabName="zuweiser", icon=icon("th"))
                )
        ),
        
        dashboardBody(
                tabItems(
                        tabItem(tabName="report",
                fluidRow(
                        box(plotOutput("pstat")),
                        box(plotOutput("aop"))),
                fluidRow(
                        box(
                                title="Controls",
                                selectInput("inVar",
                                    label = "Abteilung:",
                                    choices = c("ORTH", "H-CHI", "S-CHI"))
                ))),
                
                tabItem(tabName="zuweiser",
                        fluidRow(
                        box(plotOutput("zuw"),width=12, height = 600)        
                        ),
                        fluidRow(
                                box(title="KV_Ermächtigung",width=6,
                                        selectInput("kv",
                                                    label = "Ermächtigung",
                                                    choices = c("kvouch", "kvbest", "kvhch")),
                                    box(title="Rangordnung", width=6,
                                    sliderInput("rg","Rang",min=1,max=70,value=c(1,20),dragRange = FALSE))
                ))))))

# Define server logic  ----
server <- function(input, output) {
        
        selectedAbt <- reactive({
                abteil(input$inVar) 
        })
        
        selectedkv <- reactive(
                ermaech(input$kv)
        )
        
        output$selected_var <- renderText({ 
                y <- 4*4
                xx <- paste("Hallo, dies ist eingegeben",input$inVar,"und die Zahl",y)
                
                xx
        })
        
        output$pstat <- renderPlot({
                dd <- selectedAbt() %>% filter(form == "voll")  %>% filter(jahr!=2016)
                ggplot(dd, aes(x = mon, y = faelle)) + geom_point() + 
                        geom_line(data = dd[dd$jahr == 2018, ], color = "red") + 
                        geom_line(data = dd[dd$jahr == 2017, ], color = "blue") + 
                        theme_bw() + guides(colour = FALSE, alpha = FALSE, size = FALSE) + 
                        ggtitle("Faelle", "Vorjahr (blau)") + labs(x = "Monat", y = "Faelle") + 
                        scale_x_continuous(breaks = br) + scale_y_continuous(limits = c(0, NA))
                
        })
        
        output$aop <- renderPlot({
                dd <- selectedAbt() %>% filter(form == "ambulantes operieren")%>% filter(jahr!=2016)
                ggplot(dd, aes(x = mon, y = faelle)) + geom_point() + 
                        geom_line(data = dd[dd$jahr == 2018, ], color = "red") + 
                        geom_line(data = dd[dd$jahr == 2017, ], color = "blue") + 
                        theme_bw() + guides(colour = FALSE, alpha = FALSE, size = FALSE) + 
                        ggtitle("Ambulante OP", "Vorjahr (blau)") + labs(x = "Monat", y = "Fälle") + 
                        scale_x_continuous(breaks = br) + scale_y_continuous(limits = c(0, NA))
        })
        
        output$zuw <- renderPlot({
                fallm <- selectedkv()
                cc <- dcast(fallm, einweiser ~ quartjahr, length)
                hch <- mutate(cc, sm = rowSums(cc[, c(2:ncol(cc))]))
                hch <- hch[-1, ] %>% arrange(desc(sm))
                hch1 <- hch[input$rg[1]:input$rg[2],]
                #hch1 <- filter(hch, sm > 20)
                hch1 <- hch1[, c(1:ncol(hch1) - 1)]
                hch2 <- melt(hch1, id = c("einweiser"))
                lev <- levels(hch2$variable)
                lev <- substr(lev, 3, 6)
                
                hch2 <-
                        hch2 %>% filter(einweiser != "") %>% mutate(variable = as.numeric(variable))
                br <- unique(hch2$variable)
                ggplot(hch2, aes(x = variable, y = value)) + geom_col() + scale_x_continuous(breaks =br, labels = lev) + geom_smooth() + facet_wrap( ~ einweiser)
                
        })
        
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)