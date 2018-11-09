
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

## read prefabricated file containing query of pubmed

        pm_data <- read_csv("abstracts.csv")

# make bins for publication year
        pm_data$year_cut <- cut(pm_data$year, c(1960, 1970, 1980, 1990, 2000,2010,2018),labels=c("60-69","70-79","80-89","90-99","00-09","10-18"), include.lowest=TRUE)


## remove plural words
        pm_data <-pm_data  %>% mutate(abstract=str_replace_all(abstract,"options","option")) 
# list of words to remove in token
        del_word <-  tibble(word=c("dupuytren","NA","patient","disease","treatment","study","result","hand","disease",
                           "contracture","patients","dupuytren's","clinical","results","report","included","month","treated","found","increased","compared","95","ci","10","20","30","statistically","lt","studies","underwent","diagnosis","12","15","50","reported","de","quot","des","bev","major","dd"))

## unnest title        
        pm_title <- pm_data %>% 
                unnest_tokens(word,title) %>% 
                anti_join(stop_words)
# unnest abstract        
        pm_abstract <- pm_data %>% 
                unnest_tokens(word,abstract) %>% 
                anti_join(stop_words)
        
# remove custom stop_words and remove numbers in abstract
        pm_title <- pm_title %>% 
                anti_join(del_word)
        pm_abstract <- pm_abstract %>% 
                anti_join(del_word)  
        pm_abstract <- pm_abstract %>% 
                filter(!word %in% c(0:9))
        
### make DTM, will be replaced by prefabricated DTM use save and load   
             word_counts <- pm_abstract %>% 
                count(DOI,word,sort=TRUE) %>% ungroup()
        abstr_dtm <- word_counts %>% 
                cast_dtm(DOI,word,n)


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