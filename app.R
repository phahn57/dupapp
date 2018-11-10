
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
        dashboardHeader(title= "Pubmed query Dupuytren processed with LDA"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Rationale", tabName = "ratio", icon = icon("dashboard")),
                        menuItem("Topics", tabName="topic", icon=icon("th")),
                        menuItem("Papers",tabName="paper", icon=icon("th"))
                )
        ),
        
        dashboardBody(
                tabItems(
                        tabItem(tabName="ratio",
                fluidRow(
                        box(title="Why and how",
                            "The idea behind this program is build on techniques of natural language processing (NLP)",br(),
                            "A pubmed query: DUpuytren[MeSH] from 1960 up to now reveals 2230 papers.",br(),
                            "Every document is a mixture of topics. We imagine that each document may contain words from several topics in
particular proportions. For example, in a two-topic model we could say “Document 1 is 90% topic A and 10% topic B, while Document 2 is 30% topic A and 70% topic B.”
Every topic is a mixture of words. For example, we could imagine a two-topic model of American news, with one topic for “politics” and one for “entertainment.” The most common words in the politics topic might be “President”, “Congress”, and “government”, while the entertainment topic may be made up of words such as “movies”, “television”, and “actor”. Importantly, words can be shared between topics; a word like “budget” might appear in both equally.
LDA is a mathematical method for estimating both of these at the same time: finding the mixture of words that is associated with each topic, while also determining the mixture of topics that describes each document.
[https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation]", br(),
                            "This program calculates topic models for all papers from the above mentioned query. The topics and the top 10 words
associated with each topic are displyed in Tab 2 (topics), giving the probability for a word belonging to that topic" , br(),
                            "In Tab 3 the user can choose a topic number and a range of probability, that a paper belongs to a specific topic and all papers are displayed."
                        )
                ))),
                
                tabItem(tabName="topic",
                        fluidRow(
                        box(plotOutput("zuw"),width=12, height = 600))
                ))),
                tabItem(tabName="paper",
                        fluidRow(
                        box(plotOutput("zuw"),width=12, height = 600)        
                        ),
                        fluidRow(
                                box(title="Topic",width=6,
                                        numericInput("top",
                                                    label = "Select topic number",
                                                    min=1,max=12),
                                    box(title="Probability", width=6,
                                    sliderInput("prob","Select probability",min=0,max=1,value=c(0.7,1.0),dragRange = FALSE))
                )))
        
        )

# Define server logic  ----
server <- function(input, output) {
        
        selectedtopic <- reactive({
                input$top 
        })
        selectedkv <- reactive(
                input$prob
        )
        
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