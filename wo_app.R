library(shiny)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)
library(DT)


wo <- read.delim("wo_data.csv", stringsAsFactors = FALSE, sep=";") %>%
        mutate(Athlete_Team = str_replace_all(Name.of.Athlete.or.Team, "[^[:alnum:]]", " ")) %>%
        mutate(Type = str_replace_all(Event, "[^[:alnum:]]", " ")) %>%
        select(-Name.of.Athlete.or.Team, -Event, -Medal.Rank)


ui <- navbarPage("The Winter Olympics", theme = shinytheme("flatly"),
                tabPanel("Summary by Country", 
                         sidebarLayout(
                                 sidebarPanel(selectInput(inputId = "country", 
                                                          label = "Country", 
                                                          choices = sort(wo$Country), 
                                                          selected = 'Austria',
                                                          selectize = TRUE),
                                              
                                              selectInput(inputId = "sport", 
                                                          label = "Sport", 
                                                          choices = c("Select all", 
                                                                      sort(levels(as.factor(wo$Sport)))), 
                                                          selected = "Select all",
                                                          multiple = T), 
                                              checkboxGroupInput(inputId = "gender", 
                                                                 label = "Gender",
                                                                 selected = c(levels(as.factor(wo$Gender))),
                                                                 choices = levels(as.factor(wo$Gender))), 
                                              sliderInput("year", "Year",
                                                          min = 1924, max = 2014, step = 4,
                                                          value = c(1924, 2014))
                                             
                                 ),
                                 
                                 mainPanel(plotOutput("plot"),
                                           br(),
                                           br(),
                                           DT::dataTableOutput("table")))),
                # tabPanel("Deep dive", 
                #          sidebarLayout(
                #                  sidebarPanel(selectInput(inputId = "sport2", 
                #                                           label = "Sport", 
                #                                           choices = c("Select all", 
                #                                                       sort(levels(as.factor(wo$Sport)))), 
                #                                           selected = "Select all",
                #                                           multiple = T), 
                #                               checkboxGroupInput(inputId = "gender2", 
                #                                                  label = "Gender",
                #                                                  selected = c(levels(as.factor(wo$Gender))),
                #                                                  choices = levels(as.factor(wo$Gender))),
                #                               checkboxGroupInput(inputId = "medal2", 
                #                                                  label = "Medals",
                #                                                  selected = c(levels(as.factor(wo$Medal))),
                #                                                  choices = levels(as.factor(wo$Medal)))
                #                  ),
                #                  
                #                  mainPanel(plotOutput("plot2",  height="800px")
                #         ))),
                
                tabPanel("Gender distribution", 
                         sidebarLayout(
                                 sidebarPanel(
                                                selectInput(inputId = "country3", 
                                                                       label = "Country", 
                                                                       choices = sort(wo$Country), 
                                                                       selected = 'Austria',
                                                                       selectize = TRUE),
                                                           
                                                selectInput(inputId = "sport3", 
                                                                       label = "Sport", 
                                                                       choices = c("Select all", 
                                                                                   sort(levels(as.factor(wo$Sport)))), 
                                                                       selected = "Select all",
                                                                       multiple = T)), 
                                 mainPanel(plotOutput("plot3",  height="700px"))
                         )),
                
                tabPanel("Age distribution", 
                         sidebarLayout(
                                 sidebarPanel(
                                         selectInput(inputId = "country4", 
                                                                       label = "Country", 
                                                                       choices = sort(wo$Country), 
                                                                       selected = 'Austria',
                                                                       selectize = TRUE),
                                                           
                                        selectInput(inputId = "sport4", 
                                                                       label = "Sport", 
                                                                       choices = c("Select all", 
                                                                                   sort(levels(as.factor(wo$Sport)))), 
                                                                       selected = "Select all",
                                                                       multiple = T),
                                          checkboxGroupInput(inputId = "gender4", 
                                                                 label = "Gender",
                                                                 selected = c(levels(as.factor(wo$Gender))),
                                                                 choices = levels(as.factor(wo$Gender)))), 
                                 mainPanel(plotOutput("plot4",  height="700px"))
                         ))
                
                )
                 
    

server <- function(input, output) {
        
        
        output$plot <- renderPlot({
                
                if(input$sport == 'Select all') 
                        wo_sum <- wo %>%
                        filter(Country == input$country, 
                               Gender %in% input$gender, 
                               Year > input$year[1],
                               Year < input$year[2]) %>% 
                        group_by(Year) %>%
                        tally()
                    else 
                wo_sum <- wo %>%
                    filter(Country == input$country, 
                           Sport %in% input$sport, 
                           Gender %in% input$gender,
                           Year > input$year[1],
                           Year < input$year[2]
                           ) %>% 
                    group_by(Year) %>%
                    tally()
                
                ggplot(wo_sum, 
                       aes(x = Year, y = n, label = n)) + 
                                           geom_bar(stat = "identity", fill="#FF9999") +
                                           geom_text(nudge_y = 0.2, size = 5) +
                                           ggtitle("Number of medals over time") + 
                                           xlab("Years") + ylab("Number of Medals") +
                                           theme_classic(base_size = 15)})
        
        
        output$table <- DT::renderDataTable({
                if(input$sport == 'Select all') 
                wo %>% filter(Country == input$country,
                              Gender %in% input$gender,
                              Year > input$year[1],
                              Year < input$year[2])
                else 
                wo %>% filter(Country == input$country, 
                                Sport %in% input$sport, 
                                Gender %in% input$gender,
                                Year > input$year[1],
                                Year < input$year[2])})
        
        
        
        
        
        # output$plot2 <- renderPlot({
        #         
        #         
        #         if(input$sport2 == 'Select all') 
        #         wo_1 <- wo %>%
        #                 filter(Gender %in% input$gender2,
        #                        Medal %in% input$medal2) %>%
        #                 group_by(Country) %>%
        #                 tally() %>%
        #                 arrange((n))
        #         else 
        #         wo2 <- wo %>%
        #                 filter(Sport %in% input$sport2, 
        #                         Gender %in% input$gender2,
        #                         Medal %in% input$medal2) %>%
        #                 group_by(Country) %>%
        #                         tally() %>%
        #                         arrange((n))
        #         
        #         wo2 <- within(wo_1, 
        #                            Country <- factor(Country, 
        #                                               levels=Country))
        #         
        #         ggplot(wo2, aes(x = Country, y = n, label = n)) + 
        #                 geom_bar(stat = "identity", fill="#FF9999") +
        #                 geom_text(nudge_y = 2, size = 5) +
        #                 ggtitle("Number of medals per country") + 
        #                 xlab("Countries") + ylab("Number of Medals") +
        #                 theme_classic(base_size = 15) +
        #                 coord_flip()
        #         })
        
        output$plot3 <- renderPlot({
                wo3 <- wo %>%
                        filter(Gender != 'Mixed') %>%
                        group_by(Year, Gender) %>%
                        summarise(n = n()) %>%
                        mutate(freq = n / sum(n))  %>%
                        ungroup()
                
                ggplot(wo3, aes(x=Year, y=freq, color=Gender)) +
                        geom_line(size = 3) +
                        ggtitle("Share of male vs female atheletes") +
                        xlab("Years") + ylab("Share of total # or atheletes") +
                        theme_classic(base_size = 15) +
                        scale_color_manual(values=c("blue", "red"))
        })
        
        output$plot4 <- renderPlot({
                wo4 <- wo %>%
                        filter(!is.na(Age.of.Athlete))
                
                ggplot(wo4, aes(x=Sport, y=Age.of.Athlete, fill = Gender)) +
                        geom_boxplot() +
                        ggtitle("Age distribution by sport type") +
                        xlab("Sports") + ylab("Age") +
                        theme_classic(base_size = 15) + 
                        coord_flip() +
                        scale_fill_manual(values=c("blue", "red"))
        })
        
}
shinyApp(ui = ui, server = server)
