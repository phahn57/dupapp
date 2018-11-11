
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidytext)
library(reshape2)
library(lubridate)
library(knitr)
library(plotly)
library(ggpubr)
library(grid)
library(gridExtra)

### Load predefines model(stm) from file

        load("topic_60.RData")
### tidy it
        tidy_stm <- tidy(topic_model)
## top_terms for each topic
   
        top_terms <- tidy_stm %>% 
                group_by(topic) %>%
                filter(!is.na(term)) %>% 
                top_n(10,beta) %>% 
                ungroup() %>% 
                arrange(topic,-beta)

# Define UI for app that draws a histogram ----
ui <- dashboardPage(
        # App title ----
        dashboardHeader(title= "Pubmed query Dupuytren processed with LDA"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Rationale", tabName = "ratio", icon = icon("comment")),
                        menuItem("Topics", tabName="topic", icon=icon("chart-bar")),
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
                            "In Tab 3 the user can choose a topic number and a range of probability, that a paper belongs to a specific topic and all papers are displayed.",width=12
                        )
                )),
                
                tabItem(tabName="topic",
                        fluidRow(
                        box(plotOutput("topics"),width=12, height = 1200)
                )),
                
                tabItem(tabName="paper",
                        fluidRow(
                        #box(plotOutput("zuw"),width=12, height = 600)        
                        ),
                        fluidRow(
                                box(title="Topic",width=6,
                                        numericInput("top",
                                                    label = "Select topic number",
                                                    min=1,max=12,value=4),
                                    box(title="Probability", width=6,
                                    sliderInput("prob","Select probability",min=0,max=1,value=c(0.7,1.0),dragRange = FALSE))
                ))
                ) ## close body
                ))) ## close page
                
        

# Define server logic  ----
server <- function(input, output) {
        
        selectedtopic <- reactive({
                input$top 
        })
        selectedkv <- reactive(
                input$prob
        )
        ### output of top items for each topic
        output$topics <- renderPlot({
                top_terms %>% #filter(topic>15,topic<35)%>%
                        mutate(term = reorder(term, beta)) %>%
                        group_by(topic, term) %>%    
                        arrange(desc(beta)) %>%  
                        ungroup() %>%
                        mutate(term = factor(paste(term, topic, sep = "__"), 
                                             levels = rev(paste(term, topic, sep = "__")))) %>%
                        ggplot(aes(term, beta, fill = as.factor(topic))) +
                        geom_col(show.legend = FALSE) +
                        coord_flip() +
                        scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
                        labs(title = "Top 10 terms in each STM topic",
                             x = NULL, y = expression(beta)) +
                        facet_wrap(~ topic, ncol = 6, scales = "free")
                
        },height = 1200)
        
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)