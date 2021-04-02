# Shiny App to Explore FDA Drug Trials Snapshots 2015-2020 Data Set
# Author: Ariel Carmeli (ariel_carmeli@hms.harvard.edu)

library(shiny)
library(tidyverse)
library(lubridate)
library(dplyr)
library(scales) 
library(RColorBrewer)
library(kableExtra)

# Read in the data
print( "Loading data ..." )
fda_approvals <- read.csv('FDA_Drug_Trials_Snapshots_2015-20.csv')
print( "Data loaded!" )

# Change class type of select variables to aid in data processing and visualization
fda_approvals$Enrollment <- as.numeric(as.character(fda_approvals$Enrollment))
fda_approvals$Therapeutic_Area <- as.character(fda_approvals$Therapeutic_Area)
fda_approvals$Brand_Name <- as.character(fda_approvals$Brand_Name)
fda_approvals$United_States <- as.numeric(as.character(fda_approvals$United_States))

# Add columns for non-hispanic, Men, Age under 65 
fda_approvals <- fda_approvals %>% mutate(Non_Hispanic = 100 - Hispanic, .after = Hispanic)
fda_approvals <- fda_approvals %>% mutate(Men = 100 - Women, .after = Women)
fda_approvals <- fda_approvals %>% mutate(Age_under_65 = 100 - Age_65_or_older, .after = Age_65_or_older)

# Create longer version, for plotting
fda_approvals_long <- pivot_longer(fda_approvals, cols = Women:Age_80_or_older, names_to = "Demographic", values_to = "Percentage")
#fda_approvals_long <- fda_approvals_long %>% pivot_longer(cols = Sex_comparison:Age_comparison, names_to = "Comparison", values_to = "Compared")

# Change class type of variable in long
fda_approvals_long$Percentage <- as.numeric(as.character(fda_approvals_long$Percentage))

ui <- fluidPage(
    
    titlePanel("2015-2020 FDA Drug Trials Snapshots - Data Explorer"),
    
    tabsetPanel( 
        
        tabPanel("Welcome + Instructions",
            
        ),
        
        tabPanel("Validation",
                mainPanel(
             
                h2("Total enrollment by Therapeutic Area"),
                tags$a(href = "https://www.fda.gov/media/143592/download", "Recreating page 30 in 2015-19 DTS report"),
                plotOutput("Validation_Enrollment_by_TA", height=700, width = 1000),
            
                h2("Demographics of Trial Participation"),
                tags$a(href = "https://www.fda.gov/media/143592/download", "Recreating page 9 in 2015-19 DTS report"),
                plotOutput("Validation_Demographics", height=300, width = 1000),
                
                )
                 
        ),
        
        tabPanel("All FDA Approvals",
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
                     
                     selectInput(
                         "age",
                         "Age",
                         #choices=str_to_title(unique(fda_approvals_long$Demographic)), 
                         choices= c("Age_under_65", "Age_65_or_older"),
                         #selected = c("Age_under_65", "Age_65_or_older"),
                         selected = FALSE,
                         multiple=T
                     ),
                     
                     selectInput(
                         "sex",
                         "Sex",
                         #choices=str_to_title(unique(fda_approvals_long$Demographic)), 
                         choices= c("Women", "Men"),
                         #selected = c("Women", "Men"),
                         selected = FALSE,
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
                     DT::dataTableOutput("stats_summary_Table"),
                     #h2("How many FDA approvals have under 25% of a demographic in its trials?"),
                     plotOutput("participationCountPlot", height=700),
                     
                     h2("Has diversity in clinical trials improved from 2015-2020?"),
                     plotOutput("cum_participationCountPlot", height=700),
                     
                     h2("Approval Details"),
                     DT::dataTableOutput("approvalsTable"),
                 )
             )
        ), 
        tabPanel("Detail by Therapeutic Area & Disease",
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
                         "year_TA_page",
                         "Year(s)",
                         choices=unique(fda_approvals$Approval_Year),
                         selected = c(2015, 2016, 2017, 2018, 2019, 2020),
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
                     
                     radioButtons( 
                         "is_Enrollment_Stratified", 
                         "View Enrollment size?", 
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

    # default no adjustment
    approvals_15_19 <- reactive({
        selection <- fda_approvals %>% filter(Approval_Year != 2020)
        selection
    })
    
    # reactive expression to filter selected approvals
    approvals <- reactive({
        selection <- fda_approvals_long %>%
            select(Brand_Name, Therapeutic_Area, Indication, Enrollment, Demographic, Percentage, Approval_Year) %>%
            filter(Percentage != "NA") %>% 
            unique()

        if ( !is.null( input$race ) | !is.null( input$ethnicity ) ) {
            selection <- selection %>% filter( Demographic %in% input$race | Demographic %in% input$ethnicity | Demographic %in% input$age | Demographic %in% input$sex)
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
            #select(-Comparison) %>% 
            #select(-Compared) %>% 
            filter(Percentage != "NA")
        
        if ( !is.null( input$race ) | !is.null( input$ethnicity ) ) {
            selection <- selection %>% filter( Demographic %in% input$race | Demographic %in% input$ethnicity | Demographic %in% input$age | Demographic %in% input$sex )
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
    
    # reactive expression for the cumulative chart
    approvals_cum_count <- reactive({
        selection <- fda_approvals_long %>% 
            #select(-Comparison) %>% 
            #select(-Compared) %>% 
            filter(Percentage != "NA")
        
        if ( !is.null( input$race ) | !is.null( input$ethnicity ) ) {
            selection <- selection %>% filter( Demographic %in% input$race | Demographic %in% input$ethnicity | Demographic %in% input$age | Demographic %in% input$sex)
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
            select(Brand_Name, Therapeutic_Area, Disease, Indication, Enrollment, Demographic, Percentage, Approval_Year) %>%
            filter(Percentage != "NA") %>% 
            unique()
        
        if ( !is.null( input$race_TA_page ) | !is.null( input$ethnicity_TA_page ) ) {
            selection <- selection %>% filter( Demographic %in% input$race_TA_page | Demographic %in% input$ethnicity_TA_page )
        }
        
        if ( !is.null( input$year_TA_page ) ) {
            selection <- selection %>% filter( Approval_Year %in% input$year_TA_page )
        }
        
        if ( !is.null( input$therapeutic_area ) ) {
            selection <- selection %>% filter( Therapeutic_Area %in% input$therapeutic_area )
        }
        
        selection
        
    })

    output$Validation_Enrollment_by_TA <- renderPlot({
        
        df <- approvals_15_19() %>% 
            #filter(Approval_Year != 2020) %>% 
            select(Therapeutic_Area, Enrollment) %>% 
            mutate(Therapeutic_Area = case_when(
                Therapeutic_Area == "Oncology" ~ "Oncology and Hematology", 
                Therapeutic_Area == "Hematology" ~ "Oncology and Hematology",
                Therapeutic_Area == "Hematology (Sickle cell)" ~ "Oncology and Hematology",
                Therapeutic_Area == "Endocrinology and Metabolism" ~ "Endocrinology and Metabolism",
                Therapeutic_Area == "Infectious Disease" ~ "Infectious Disease",
                Therapeutic_Area == "Neurology" ~ "Neurology",
                Therapeutic_Area == "Gynecology" ~ "Gynecology",
                Therapeutic_Area == "Dermatology" ~ "Dermatology",
                Therapeutic_Area == "Pulmonology and Rheumatology" ~ "Pulmonology and Rheumatology",
                Therapeutic_Area == "Gastroenterology" ~ "Gastroenterology",
                Therapeutic_Area == "Psychiatry" ~ "Psychiatry",
                Therapeutic_Area == "Sleep Disorders" ~ "Psychiatry",
                Therapeutic_Area == "Ophthalmology" ~ "Ophthalmology",
                Therapeutic_Area == "Anesthesia and Analgesia" ~ "Anesthesia and Analgesia",
                Therapeutic_Area == "Medical Imaging" ~ "Medical Imaging",
                Therapeutic_Area == "Cardiovascular Diseases" ~ "Cardiovascular Diseases"
            )) %>% 
            group_by(Therapeutic_Area) %>% 
            summarize(Our_database = sum(Enrollment)) %>% 
            arrange(desc(Our_database))
        
        # Cardio, Onc, Endocrin, ID, Neurology, Derm, Gyn, Pulm/Rheum, GI, Psychiatry, Anaesthesia, Ophthal, Medical Imaging
        fda_snapshots <- c(59000, 35000, 41000, 32500, 26000, 20500, 21000, 20000, 15000, 10500, 6500, 4500, 1500)
        
        Total_patients_check <- data.frame(df$Therapeutic_Area, 
                                           df$Our_database, 
                                           fda_snapshots)
        names(Total_patients_check) <- c("Therapeutic_Area", "Our_Database", 
                                         "FDA_Snapshots")
        
        fda_approvals_check_long <- pivot_longer(Total_patients_check, cols = Our_Database:FDA_Snapshots, 
                                                 names_to = "Database", values_to = "Participants")
        
        Patient_participation_graph <- fda_approvals_check_long %>% 
            ggplot(aes(reorder(Therapeutic_Area, Participants), Participants, fill=Database)) +
            geom_bar(stat = "identity", position = 'dodge') +
            geom_text(aes(label=Participants), position = position_dodge(0.9), hjust=-0.15, size=5) +
            coord_flip() + 
            ylim(0,70000) +
            xlab("Therapeutic Area") +
            ylab("Number of Participants") + 
            theme(text = element_text(size=20)) +
            ggtitle("Patient participation by Therapeutic Area [FDA approvals 2015-19]")
        
        Patient_participation_graph
        
    })
    
    output$Validation_Demographics <- renderPlot({
        global_black_participation <- approvals_15_19() %>% 
            select(Brand_Name, Enrollment, Black) %>% 
            filter(Black != "NA") %>% 
            mutate(Black_participants = (Black/100) * Enrollment)
        
        global_white_participation <- approvals_15_19() %>% 
            select(Brand_Name, Enrollment, White) %>% 
            filter(White != "NA") %>% 
            mutate(White_participants = (White/100) * Enrollment)
        
        global_asian_participation <- approvals_15_19() %>% 
            select(Brand_Name, Enrollment, Asian) %>% 
            filter(Asian != "NA") %>% 
            mutate(Asian_participants = (Asian/100) * Enrollment)
        
        demographic_participation <- c(round(sum(100 * global_asian_participation$Asian_participants) /
                                                 sum(global_asian_participation$Enrollment), 0), 
                                       round(sum(100 * global_black_participation$Black_participants) / 
                                                 sum(global_black_participation$Enrollment), 0),
                                       round(sum(100 * global_white_participation$White_participants) / 
                                                 sum(global_white_participation$Enrollment), 0))
        
        fda_snapshots <- c(11, 7, 76)
        races <- c("Asian", "Black or African American", "White")
        
        Race_distribution_comparison <- data.frame(races, fda_snapshots, demographic_participation)
        names(Race_distribution_comparison) <- c("Race", "FDA_Snapshots", "Our_Database")
        
        Race_distribution_comparison_graph <- Race_distribution_comparison %>% 
            pivot_longer(cols = FDA_Snapshots:Our_Database, names_to = "Database", values_to = "Participation") %>% 
            ggplot(aes(Race, Participation, fill = Database)) +
            geom_bar(stat = "identity", position = 'dodge') +
            geom_text(aes(label=Participation), position = position_dodge(0.9), vjust=-0.3, size=6) +
            ylim(0,100) +
            xlab("Race") +
            ylab("Participation") + 
            theme(text = element_text(size=15), axis.text.x = element_text(size=15)) +
            ggtitle("Race distribution of trial participants")
        
        Race_distribution_comparison_graph
    })
    
    output$individualPlot <- renderPlot({
        
        plot <- approvals() %>% 
            ggplot(aes(Demographic, Percentage)) +
            #geom_jitter(width = 0.2, aes(colour=Demographic)) + 
            theme(legend.position = "top", legend.title = element_blank()) +
            scale_y_continuous(breaks = round(seq(min(0), max(100), by = 5),1)) +
            ggtitle("Distribution of clinical trial participation")
        
        if ( input$is_TA_Stratified ) {
            plot <- plot + 
                facet_wrap(~Therapeutic_Area) + 
                scale_y_continuous(breaks = round(seq(min(0), max(100), by = 10),1))
        }
        
        if ( input$is_Year_labelled ) {
            plot <- plot + 
                geom_jitter(width = 0.2, aes(colour=Approval_Year))
                #scale_color_brewer(palette="Greys")
        }
        else{
            plot <- plot + geom_jitter(width = 0.2, aes(colour=Demographic))
        }
        
        plot
    })
    
    output$individualPlot_Violin <- renderPlot({
        
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
    })
    
    output$stats_summary_Table <- DT::renderDataTable(
        approvals() %>% 
            #select(-Enrollment_bucket) %>% 
            #filter(Percentage >= input$participation[1]) %>% 
            #filter(Percentage <= input$participation[2]) %>%
            pivot_wider(names_from = Demographic, values_from = Percentage), options=list(pageLength=10)
    )
    
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
            ylim(0,65) +
            scale_x_continuous(breaks = round(seq(0, 25, by = 1),1)) +
            xlab("Percent participation in trials for FDA approval") +
            ylab("Number of FDA approvals") +
            ggtitle("Number of FDA approvals with under 25 percent participation by demographic in its trials") +
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
            #select(-Enrollment_bucket) %>% 
            #filter(Percentage >= input$participation[1]) %>% 
            #filter(Percentage <= input$participation[2]) %>%
            pivot_wider(names_from = Demographic, values_from = Percentage), options=list(pageLength=10)
    )
    
    output$TA_individualPlot <- renderPlot({
        
        plot <- approvals_TA() %>% 
            ggplot(aes(Demographic, Percentage))
        
        if ( input$is_Enrollment_Stratified ) {
            plot <- plot + geom_jitter(width = 0.2, aes(colour=Demographic, size = Enrollment)) + 
                theme(legend.position = "top", legend.title = element_blank()) +
                scale_y_continuous(breaks = round(seq(min(0), max(100), by = 5),1)) +
                ggtitle("Participation in clinical trials by demographic \n 
                    Each dot represents % participation in one trial")
            
        }   
        else{
            plot <- plot + geom_jitter(width = 0.2, aes(colour=Demographic)) + 
                theme(legend.position = "top", legend.title = element_blank()) +
                scale_y_continuous(breaks = round(seq(min(0), max(100), by = 5),1)) +
                ggtitle("Participation in clinical trials by demographic \n 
                    Each dot represents % participation in one trial")
        }
        
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
            #select(-Enrollment_bucket) %>%
            pivot_wider(names_from = Demographic, values_from = Percentage), options=list(pageLength=10)
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
