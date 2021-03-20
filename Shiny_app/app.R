# Shiny App to Explore FDA Drug Trials Snapshots 2015-2020 Data Set
# Author: Ariel Carmeli (ariel_carmeli@hms.harvard.edu)

library(shiny)
library(tidyverse)
library(lubridate)
library(dplyr)
library(scales) 

fda_approvals <- read.csv('FDA_Drug_Trials_Snapshots_2015-20.csv')

# Change class type of select variables to aid in data processing and visualization
fda_approvals$Enrollment <- as.numeric(as.character(fda_approvals$Enrollment))
fda_approvals$Therapeutic_Area <- as.character(fda_approvals$Therapeutic_Area)
fda_approvals$Brand_Name <- as.character(fda_approvals$Brand_Name)
fda_approvals$United_States <- as.numeric(as.character(fda_approvals$United_States))

# Add a column to group trial size
fda_approvals <- fda_approvals %>% 
    mutate(Enrollment_bucket = case_when(
        Enrollment < 100 ~ "a. 1-99 patients", 
        Enrollment >= 100 & Enrollment < 200 ~ "b. 100-199 patients", 
        Enrollment >= 200 & Enrollment < 300 ~ "c. 200-299 patients",
        Enrollment >= 300 & Enrollment < 500 ~ "d. 300-499 patients",
        Enrollment >= 500 & Enrollment < 1000 ~ "e. 500-999 patients",
        Enrollment >= 1000 & Enrollment < 2000 ~ "f. 1000-1999 patients",
        Enrollment >= 2000  ~ "g. Over 2000 patients"
    ), .after = Enrollment)

# Add a column for non-hispanic 
fda_approvals <- fda_approvals %>% mutate(Non_Hispanic = 100 - Hispanic, .after = Hispanic)

# Create longer version, for plotting
fda_approvals_long <- pivot_longer(fda_approvals, cols = Women:Age_80_or_older, names_to = "Demographic", values_to = "Percentage")

fda_approvals_long <- fda_approvals_long %>% pivot_longer(cols = Sex_comparison:Age_comparison, names_to = "Comparison", values_to = "Compared")

# Change class type of variable in long
fda_approvals_long$Percentage <- as.numeric(as.character(fda_approvals_long$Percentage))

ui <- fluidPage(
    
    titlePanel("FDA Drug Trials Snapshots - Data Explorer"),
    
    tabsetPanel( # Problem 2.1 - add tab set to the window
        tabPanel("All 2015-2020 FDA Approvals", # Problem 2.1 - add Product Details tab
            sidebarLayout(
                sidebarPanel(
                     selectInput(
                         "race",
                         "Race",
                         #choices=str_to_title(unique(fda_approvals_long$Demographic)), 
                         choices= c("Asian", "Black", "White", "Other"),
                         selected = c("Asian", "Black", "White"),
                         multiple=T
                     ),
                     
                     selectInput(
                         "ethnicity",
                         "Ethnicity",
                         #choices=str_to_title(unique(fda_approvals_long$Demographic)), 
                         choices= c("Hispanic", "Non_Hispanic"),
                         selected = c("Hispanic"),
                         multiple=T
                     ),
                     
                     #sliderInput(
                     #    "participation",
                     #    "Filter participation by",
                     #    min=0,
                     #    max=100,
                     #    value=c(0, 100)
                     #),
                     
                     radioButtons( 
                         "is_TA_Stratified", 
                         "Stratify by Therapeutic Area?", 
                         choiceNames=list( "Yes", "No" ), 
                         choiceValues=list(TRUE,FALSE),
                         selected=FALSE
                     ),
                     
                     selectInput(
                         "year",
                         "Year(s)",
                         choices=unique(fda_approvals$Approval_Year),
                         selected = c(2015, 2016, 2017, 2018, 2019, 2020),
                         multiple=T
                     ),
                     
                     radioButtons( 
                         "is_Year_labelled", 
                         "Label by Year?", 
                         choiceNames=list( "Yes", "No" ), 
                         choiceValues=list(TRUE,FALSE),
                         selected=FALSE
                     )#,
                     
                     , width = 3
                     
                     #textOutput("summaryText")
                 ),
                     
                 mainPanel(
                     
                     h2("How consistent is diversity in FDA approvals?"),
                     plotOutput("individualPlot", height=700),
                     
                     h2("Distribution of trial participation"),
                     plotOutput("participationCountPlot", height=700),
                     
                     h2("Has diversity in clinical trials improved from 2015-2020?"),
                     plotOutput("cum_participationCountPlot", height=700),
                     
                     h2("Approval Details"),
                     DT::dataTableOutput("approvalsTable"),
                 )
             )
        ), 
        tabPanel("By Therapeutic Area & Disease",
            sidebarLayout(
                sidebarPanel(
                     selectInput(
                         "race_TA_page",
                         "Race",
                         #choices=str_to_title(unique(fda_approvals_long$Demographic)), 
                         choices= c("Asian", "Black", "White", "Other"),
                         selected = c("Asian", "Black", "White"),
                         multiple=T
                     ), 
                     
                     selectInput(
                         "ethnicity_TA_page",
                         "Ethnicity",
                         #choices=str_to_title(unique(fda_approvals_long$Demographic)), 
                         choices= c("Hispanic", "Non_Hispanic"),
                         selected = c("Hispanic"),
                         multiple=T
                     ),
                     
                     selectInput(
                         "therapeutic_area",
                         "Therapeutic Area",
                         choices=sort(unique(fda_approvals$Therapeutic_Area)),
                         selected = "Oncology",
                         multiple=FALSE
                     ),
                     
                     radioButtons( 
                         "is_Disease_Stratified", 
                         "Stratify by Disease?", 
                         choiceNames=list( "Yes", "No" ), 
                         choiceValues=list(TRUE,FALSE),
                         selected=FALSE
                     ),
                     
                 ), 
                 mainPanel(
                      
                     
                     h2("How consistent is diversity in FDA approvals in selected TA?"),
                     plotOutput("TA_individualPlot", height = 700),
                     
                     h2("Has diversity in clinical trials improved in selected TA from 2015-2020?"),
                     plotOutput("TA_cum_participationCountPlot", height=700),
                     
                     h2("Approval Details"),
                     DT::dataTableOutput("TA_approvalsTable"),
                 )
             )
        )
    )
)

server <- function(input, output) {
    
    print( "Loading data ..." )
    
    # lazy loading, first access will be slow
    #data <- neiss_2008_2018
    fda_approvals <- read.csv('FDA_Drug_Trials_Snapshots_2015-20.csv')
    
    print( "Data loaded!" )
    print( dim(fda_approvals) )
    
    print( "Processing data ..." )
    
    # Change class type of select variables to aid in data processing and visualization
    fda_approvals$Enrollment <- as.numeric(as.character(fda_approvals$Enrollment))
    fda_approvals$Therapeutic_Area <- as.character(fda_approvals$Therapeutic_Area)
    fda_approvals$Brand_Name <- as.character(fda_approvals$Brand_Name)
    fda_approvals$United_States <- as.numeric(as.character(fda_approvals$United_States))
    
    # Add a column to group trial size
    fda_approvals <- fda_approvals %>% 
        mutate( Enrollment_bucket = case_when(
            Enrollment < 100 ~ "a. 1-99 patients", 
            Enrollment >= 100 & Enrollment < 200 ~ "b. 100-199 patients", 
            Enrollment >= 200 & Enrollment < 300 ~ "c. 200-299 patients",
            Enrollment >= 300 & Enrollment < 500 ~ "d. 300-499 patients",
            Enrollment >= 500 & Enrollment < 1000 ~ "e. 500-999 patients",
            Enrollment >= 1000 & Enrollment < 2000 ~ "f. 1000-1999 patients",
            Enrollment >= 2000  ~ "g. Over 2000 patients"
        ), .after = Enrollment)
    
    # Add a column for non-hispanic 
    fda_approvals <- fda_approvals %>% mutate(Non_Hispanic = 100 - Hispanic, .after = Hispanic)
    
    # Create longer version, for plotting
    fda_approvals_long <- pivot_longer(fda_approvals, cols = Women:Age_80_or_older, names_to = "Demographic", values_to = "Percentage")
    
    fda_approvals_long <- fda_approvals_long %>% pivot_longer(cols = Sex_comparison:Age_comparison, names_to = "Comparison", values_to = "Compared")
    
    # Change class type of variable in long
    fda_approvals_long$Percentage <- as.numeric(as.character(fda_approvals_long$Percentage))
    
    print( "Data processed!" )
    #print( dim(data) )
    
    # reactive expression to filter selected approvals
    approvals <- reactive({
        selection <- fda_approvals_long %>%
            select(Brand_Name, Therapeutic_Area, Indication, Enrollment, Demographic, Percentage, Approval_Year, Enrollment_bucket) %>%
            filter(Percentage != "NA") %>% 
            unique()

        if ( !is.null( input$race ) | !is.null( input$ethnicity ) ) {
            selection <- selection %>% filter( Demographic %in% input$race | Demographic %in% input$ethnicity )
        }
        
        if ( !is.null( input$year ) ) {
            selection <- selection %>% filter( Approval_Year %in% input$year )
        }
        
        # Find unique set of drugs that have participation values in the bounds from input
        drugs <- selection %>% 
            #filter(Percentage >= input$participation[1]) %>% 
            #filter(Percentage <= input$participation[2]) %>% 
            group_by(Brand_Name) %>% 
            unique()
        
        # Filter selection to include this set of drugs
        selection <- selection %>% 
            filter( Brand_Name %in% drugs$Brand_Name )
        
        selection
    })
    
    # reactive expression for the count chart
    approvals_count <- reactive({
        selection <- fda_approvals_long %>% 
            select(-Comparison) %>% 
            select(-Compared) %>% 
            filter(Percentage != "NA")
        
        if ( !is.null( input$race ) | !is.null( input$ethnicity ) ) {
            selection <- selection %>% filter( Demographic %in% input$race | Demographic %in% input$ethnicity )
        }
        
        if ( !is.null( input$year ) ) {
            selection <- selection %>% filter( Approval_Year %in% input$year )
        }
        
        #selection$Approval_Year <- as.character(selection$Approval_Year)
        
        selection <- selection %>% 
            group_by(Brand_Name, Demographic, Percentage, Approval_Year) %>% 
            unique() %>% 
            summarize(Count = n()) %>% 
            group_by(Demographic, Percentage, Approval_Year) %>% 
            summarize(Count = sum(Count))
        
        selection
    })
    
    # reactive expression for the count chart
    approvals_cum_count <- reactive({
        selection <- fda_approvals_long %>% 
            select(-Comparison) %>% 
            select(-Compared) %>% 
            filter(Percentage != "NA")
        
        if ( !is.null( input$race ) | !is.null( input$ethnicity ) ) {
            selection <- selection %>% filter( Demographic %in% input$race | Demographic %in% input$ethnicity )
        }
        
        if ( !is.null( input$year ) ) {
            selection <- selection %>% filter( Approval_Year %in% input$year )
        }
        
        #selection$Approval_Year <- as.character(selection$Approval_Year)
        
        selection
    })
    
    
    # Reactive expression to filter selected approvals on the TA/disease tab 
    approvals_TA <- reactive({
        selection <- fda_approvals_long %>%
            select(Brand_Name, Therapeutic_Area, Disease, Indication, Enrollment, Demographic, Percentage, Approval_Year, Enrollment_bucket) %>%
            filter(Percentage != "NA") %>% 
            unique()
        
        if ( !is.null( input$race_TA_page ) | !is.null( input$ethnicity_TA_page ) ) {
            selection <- selection %>% filter( Demographic %in% input$race_TA_page | Demographic %in% input$ethnicity_TA_page )
        }
        
        if ( !is.null( input$therapeutic_area ) ) {
            selection <- selection %>% filter( Therapeutic_Area %in% input$therapeutic_area )
        }
        
        selection
        
    })

    output$individualPlot <- renderPlot({
        
        plot <- approvals() %>% 
            #filter(Percentage >= input$participation[1]) %>% 
            #filter(Percentage <= input$participation[2]) %>% 
            ggplot(aes(Demographic, Percentage)) +
            #geom_jitter(width = 0.2, aes(colour=Demographic)) + 
            theme(legend.position = "top", legend.title = element_blank()) +
            scale_y_continuous(breaks = round(seq(min(0), max(100), by = 5),1)) +
            ggtitle("Distribution of clinical trial participation \n (Red dot = mean. Blue dot = median)")
        
        if ( input$is_TA_Stratified ) {
            plot <- plot + 
                facet_wrap(~Therapeutic_Area) + 
                scale_y_continuous(breaks = round(seq(min(0), max(100), by = 10),1))
        }
        
        if ( input$is_Year_labelled ) {
            plot <- approvals() %>% 
                #filter(Percentage >= input$participation[1]) %>% 
                #filter(Percentage <= input$participation[2]) %>% 
                ggplot(aes(as.factor(Approval_Year), Percentage)) +
                geom_violin(aes(fill=as.factor(Approval_Year))) +
                stat_summary(fun=mean, geom="point", color="red") +
                stat_summary(fun=median, geom="point", color="blue") +
                facet_wrap(~Demographic) +
                xlab("Approval Year") +
                theme(legend.position = "top", legend.title = element_blank()) +
                scale_y_continuous(breaks = round(seq(min(0), max(100), by = 5),1)) +
                ggtitle("Distribution of clinical trial participation \n (Red dot = mean. Blue dot = median)")
            #plot <- plot + geom_jitter(width = 0.2, aes(colour=Approval_Year))
        }
        else{
            plot <- plot + geom_violin(aes(fill=Demographic))
            plot <- plot + stat_summary(fun=mean, geom="point", color="red")
            plot <- plot + stat_summary(fun=median, geom="point", color="blue")
            #plot <- plot + geom_jitter(width = 0.2, aes(colour=Demographic))
        }
        
        plot
    })#, height = 800, width = 1200)
    
    output$participationCountPlot <- renderPlot({
        
        # bar chart - for all years
        plot <- approvals_count() %>% 
            group_by(Demographic, Percentage) %>% 
            summarize(Count = sum(Count)) %>% 
            filter(Percentage <= 25) %>%
            #filter(Percentage >= input$participation[1]) %>% 
            #filter(Percentage <= input$participation[2]) %>% 
            ggplot(aes(Percentage, y=Count, width=1)) +
            geom_bar(stat = "identity", position = 'dodge') +
            geom_text(aes(label=Count), position = position_dodge(0.9), vjust=-0.3, size=3) +
            scale_x_continuous(breaks = round(seq(0, 25, by = 1),1)) +
            xlab("Percent participation in trials for FDA approval") +
            ylab("Number of FDA approvals") +
            ggtitle("Number of FDA approvals with given percent participation in its trials") +
            facet_wrap(~Demographic, ncol=1)
        
        plot
    })
    
    output$cum_participationCountPlot <- renderPlot({
        
        plot <- approvals_cum_count() %>%
            ggplot(aes(Percentage/100, colour = factor(Approval_Year))) + stat_ecdf(geom = "step") +
            scale_x_continuous(breaks = round(seq(min(0), max(1), by = 0.05),2)) +
            scale_y_continuous(breaks = round(seq(min(0), max(1), by = 0.05),2)) +
            theme(axis.title.x = element_text(size=15), 
                  axis.title.y = element_text(size=15),
                  axis.text.x = element_text(size=8),
                  axis.text.y = element_text(size=8),
                  title = element_text(size=15),
                  legend.text=element_text(size=20), 
                  legend.position = "top", 
                  legend.title = element_blank()
            ) +
            facet_wrap(~Demographic, ncol = 2) + 
            ylab("Cumulative percentage of FDA approvals") +
            xlab("Representation in clinical trials") +
            ggtitle("Cumulative distribution of Demographic participation in trials by year")
        
        plot
    })
    
    output$approvalsTable <- DT::renderDataTable(
        approvals() %>% 
            select(-Enrollment_bucket) %>% 
            #filter(Percentage >= input$participation[1]) %>% 
            #filter(Percentage <= input$participation[2]) %>%
            pivot_wider(names_from = Demographic, values_from = Percentage), options=list(pageLength=10)
    )
    
    output$TA_individualPlot <- renderPlot({
        
        plot <- approvals_TA() %>% 
            ggplot(aes(Demographic, Percentage)) + 
            geom_jitter(width = 0.2, aes(colour=Demographic)) + 
            theme(legend.position = "top", legend.title = element_blank()) +
            scale_y_continuous(breaks = round(seq(min(0), max(100), by = 5),1)) +
            ggtitle("Participation in clinical trials by demographic \n 
                    Each dot represents % participation in one trial")
        
        if ( input$is_Disease_Stratified ) {
            plot <- plot + 
                facet_wrap(~Disease) + 
                scale_y_continuous(breaks = round(seq(min(0), max(100), by = 10),1))
        }
        
        plot
    })
    
    output$TA_cum_participationCountPlot <- renderPlot({
        
        plot <- approvals_TA() %>%
            ggplot(aes(Percentage/100, colour = factor(Approval_Year))) + stat_ecdf(geom = "step") +
            scale_x_continuous(breaks = round(seq(min(0), max(1), by = 0.05),2)) +
            scale_y_continuous(breaks = round(seq(min(0), max(1), by = 0.05),2)) +
            theme(axis.title.x = element_text(size=15), 
                  axis.title.y = element_text(size=15),
                  axis.text.x = element_text(size=8),
                  axis.text.y = element_text(size=8),
                  title = element_text(size=15),
                  legend.text=element_text(size=20), 
                  legend.position = "top", 
                  legend.title = element_blank()
            ) +
            facet_wrap(~Demographic, ncol = 2) + 
            ylab("Cumulative percentage of FDA approvals") +
            xlab("Representation in clinical trials") +
            ggtitle("Cumulative distribution of Demographic participation in trials by year")
        
        plot
    })
    
    output$TA_approvalsTable <- DT::renderDataTable(
        approvals_TA() %>% 
            select(-Enrollment_bucket) %>%
            pivot_wider(names_from = Demographic, values_from = Percentage), options=list(pageLength=10)
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
