# Shiny App to Explore FDA Drug Trials Snapshots 2015-2020 Data Set
# Author: Ariel Carmeli (ariel_carmeli@hms.harvard.edu)

library(shiny)
library(tidyverse)
library(lubridate)
library(dplyr)
library(scales) 
library(RColorBrewer)
library(plotly)
#library(kableExtra)

########################
### READ IN THE DATA ###
########################

print( "Loading data ..." )
fda_approvals <- read.csv('FDA_Drug_Trials_Snapshots_2015-20.csv')
disease_burden <- read.csv('Disease_burden.csv')
print( "Data loaded!" )

############################
### PROCESS FDA DTS DATA ###
############################

# Change class type of select variables to aid in data processing and visualization
fda_approvals$Enrollment <- as.numeric(as.character(fda_approvals$Enrollment))
fda_approvals$Therapeutic_Area <- as.character(fda_approvals$Therapeutic_Area)
fda_approvals$Brand_Name <- as.character(fda_approvals$Brand_Name)
fda_approvals$United_States <- as.numeric(as.character(fda_approvals$United_States))

# Add columns for non-hispanic, Men, Age under 65 
fda_approvals <- fda_approvals %>% mutate(Non_Hispanic = 100 - Hispanic, .after = Hispanic)
fda_approvals <- fda_approvals %>% mutate(Male = 100 - Female, .after = Female)
fda_approvals <- fda_approvals %>% mutate(Age_under_65 = 100 - Age_65_or_older, .after = Age_65_or_older)

# Create longer version, for plotting
fda_approvals_long <- pivot_longer(fda_approvals, cols = Female:Age_80_or_older, names_to = "Demographic", values_to = "Percentage")

# Change class type of variable in long
fda_approvals_long$Percentage <- as.numeric(as.character(fda_approvals_long$Percentage))

#################################
### CREATE SUMMARY STATISTICS ###
#################################

# Calculate mean representation treating weighting each trial by enrollment size
averages <- fda_approvals_long %>% filter(Percentage != "NA") # create new df with non NA participation

averages <- averages %>% # Calculate number of people (% representation * Trial Enrollment)
    mutate(Population = round((Percentage / 100) * Enrollment))

averages_all_years <- averages %>% # Calculate weighted average (Demographic groups' enrollment / Total Enrollment) - all years
    group_by(Demographic) %>% 
    summarize(Demographic_enrollment = sum(Population), Total_enrollment = sum(Enrollment)) %>% 
    mutate(Weighted_average = round(100* Demographic_enrollment / Total_enrollment),1)

averages <- averages %>% # Same as above but by year
    group_by(Approval_Year, Demographic) %>% 
    summarize(Demographic_enrollment = sum(Population), Total_enrollment = sum(Enrollment)) %>% 
    mutate(Weighted_average = round(100* Demographic_enrollment / Total_enrollment),1)

# Calculate median representation
approval_median <- fda_approvals_long %>% filter(Percentage != "NA") # create new df with non NA participation

approval_median_all_years <- approval_median %>% # Calculate median enrollment percentage across all trials
    group_by(Demographic) %>% 
    summarize(Median = round(median(Percentage),1))

approval_median <- approval_median %>% # Same as above but by year
    group_by(Approval_Year, Demographic) %>% 
    summarize(Median = round(median(Percentage),1))

# Calculate mean representation treating each trial equally
approval_mean <- fda_approvals_long %>% filter(Percentage != "NA") # create new df with non NA participation

approval_mean_all_years <- approval_mean %>% # Calculate mean enrollment percentage across all trials (non weighted by enrollment)
    group_by(Demographic) %>% 
    summarize(Average = round(mean(Percentage),1))

approval_mean <- approval_mean %>% # Same as above but by year
    group_by(Approval_Year, Demographic) %>% 
    summarize(Average = round(mean(Percentage),1))

# Create combined dataset by Demographic
summary_statistics_all_years <- data.frame(averages_all_years$Demographic, approval_median_all_years$Median, approval_mean_all_years$Average, averages_all_years$Weighted_average)
names(summary_statistics_all_years) <- c("Demographic", "Median", "Average", "Weighted_average")

# Create combined dataset by Year and Demographic
summary_statistics <- data.frame(averages$Approval_Year, averages$Demographic, approval_median$Median, approval_mean$Average, averages$Weighted_average)
names(summary_statistics) <- c("Approval_Year", "Demographic", "Median", "Average", "Weighted_average")

#######################
### DEFINE SHINY UI ###
#######################

ui <- fluidPage(
    
    titlePanel("2015-2020 FDA Drug Trials Snapshots - Data Explorer"),
    
    tabsetPanel( 
        
        tabPanel("Welcome + Instructions",
            
        ),
        
        tabPanel("Descriptive Statistics",
            sidebarLayout(
                sidebarPanel(
                    
                    #radioButtons( 
                    #    "2020_inclusion", 
                    #    "Include 2020 data in all graphs?", 
                    #    choiceNames=list( "Yes", "No" ), 
                    #    choiceValues=list(TRUE,FALSE),
                    #    selected=FALSE
                    #),
                    
                    radioButtons( 
                        "DS_TA_stratify", 
                        "Stratify by Therapeutic Area?", 
                        choiceNames=list( "Yes", "No" ), 
                        choiceValues=list(TRUE,FALSE),
                        selected=FALSE
                    ),
                    width = 2
                ),
                         
                mainPanel(
                 
                    h2("Total Number of Patients Enrolled in Clinical Trials by Therapeutic Area"),
                    h6("Data: All FDA approvals from 2015-19. Excludes 2020 data in order to validate against FDA's 2015-2019 Drug Trial Snapshot report"),
                    tags$a(href = "https://www.fda.gov/media/143592/download", "This graph closely recreates page 30 in FDA's 2015-19 DTS Drug Trial Snapshot report"),
                    plotOutput("Validation_Enrollment_by_TA", height=300, width = 1000),
                    
                    h2("Participation in Clinical Trials by Demographic"),
                    h6("Data: All FDA approvals from 2015-19. Excludes 2020 data in order to validate against FDA's 2015-2019 Drug Trial Snapshot report"),
                    tags$a(href = "https://www.fda.gov/media/143592/download", "This graph closely recreates page 9 in FDA's 2015-19 DTS Drug Trial Snapshot report"),
                    plotOutput("Validation_Demographics", height=175, width = 1000),
                    
                    h2("Total Approvals by Year"),
                    h6("Data: All FDA approvals from 2015-20"),
                    plotOutput("Approvals_DS", height=450, width = 1000),
                    
                    h2("Total Patients by Year"),
                    h6("Data: All FDA approvals from 2015-20"),
                    plotOutput("Patients_DS", height=450, width = 1000),
                    
                    h2("Enrollment by Therapeutic Area"),
                    h6("Data: All FDA approvals from 2015-20"),
                    plotOutput("Enrollment_TA_boxplot_DS", height=300, width = 1000),
                     
                    h2("Data Quality -- Reporting of Participation by Demographic"),
                    h6("Data: All FDA approvals from 2015-20"),
                    plotOutput("Demographics_reported", height=300, width = 1000),
                    
                )
            )
                
        ),
        
        tabPanel("Explore FDA Approvals",
            sidebarLayout(
                sidebarPanel(
                     selectInput(
                         "race",
                         "Race",
                         choices= c("Asian", "Black", "White", "Other"),
                         selected = c("Asian", "Black", "White"),
                         multiple=T
                     ),
                     
                     selectInput(
                         "ethnicity",
                         "Ethnicity",
                         choices= c("Hispanic", "Non_Hispanic"),
                         selected = c("Hispanic"),
                         multiple=T
                     ),
                     
                     selectInput(
                         "age",
                         "Age",
                         choices= c("Age_under_65", "Age_65_or_older"),
                         selected = FALSE,
                         multiple=T
                     ),
                     
                     selectInput(
                         "sex",
                         "Sex",
                         choices= c("Female", "Male"),
                         selected = FALSE,
                         multiple=T
                     ),
                     
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
                     
                     , width = 2
                     
                     #textOutput("summaryText")
                 ),
                     
                 mainPanel(
                     
                     h2("Distribution of Participation by Demographic in Clinical Trials Across FDA approvals"),
                     h6("Each dot represents a pivotal clinical trial for 1 FDA approval"),
                     plotlyOutput("individualPlot", height=700, width = 1200),

                     h2("Data Table: Median and Average of Clinical Trial Participation"),                     
                     DT::dataTableOutput("stats_summary_Table"),
                                          
                     h2("Trend in Clinical Trial Participation by Demographic Over Time"),
                     h6("Boxplots represents 5 points in the distribution: Middle line is median. 
                        Ends of the box are 1st and 3rd quartile.
                        Ends of the whiskers are 1.5 * inter-quartile range from the closer of 1st or 3rd quartile"),
                     plotOutput("change_over_time", height = 700),

                     h2("Count of FDA Approvals Per Participation in Clinical Trials"),
                     plotOutput("participationCountPlot", height=700),
                     
                     h2("Approval Details"),
                     DT::dataTableOutput("approvalsTable"),
                 )
             )
        ), 
        tabPanel("Detail by Therapeutic Area",
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
                         choices= c("Hispanic", "Non_Hispanic"),
                         selected = c("Hispanic"),
                         multiple=T
                     ),
                     
                     selectInput(
                         "age_TA_page",
                         "Age",
                         choices= c("Age_under_65", "Age_65_or_older"),
                         selected = FALSE,
                         multiple=T
                     ),
                     
                     selectInput(
                         "sex_TA_page",
                         "Sex",
                         choices= c("Female", "Male"),
                         selected = FALSE,
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
                         "is_Enrollment_Stratified", 
                         "View Enrollment size?", 
                         choiceNames=list( "Yes", "No" ), 
                         choiceValues=list(TRUE,FALSE),
                         selected=FALSE
                     ),
                     
                     selectInput(
                         "Stratify_by",
                         "Stratify by (Pharma Sponsor, Therapeutic Area subgroup):",
                         choices = list("None", "Sponsor", "TA_subgroup"),
                         selected = "None",
                         multiple = FALSE
                     ),
                     
                     sliderInput("Sponsor_size",
                                 "(If stratified by Pharma Sponsor) Show sponsors with >= approvals",
                                 min=1,
                                 max=3,
                                 value=1
                     ),
                     width = 2
                     
                 ), 
                 mainPanel(
                     
                     h2("How consistent is diversity in FDA approvals in selected TA?"),
                     plotlyOutput("TA_individualPlot", height = 700),
                     
                     h2("How does clinical trial representation compare with disease burden?"),
                     plotOutput("TA_Disease_Burden_Comparison", height = 700),
                     
                     h2("Has diversity in clinical trials improved in selected TA from 2015-2020?"),
                     plotOutput("TA_cum_participationCountPlot", height=700),
                     
                     h2("Approval Details"),
                     DT::dataTableOutput("TA_approvalsTable"),
                 )
             )
        )
    )
)

################################
### DYNAMIC DATA INTERACTION ###
################################

server <- function(input, output) {

    # reactive for disease burden
    disease_burden <- reactive({
        selection <- disease_burden_df
        
        if ( !is.null( input$therapeutic_area ) ) {
            selection <- selection %>% filter( Therapeutic_Area %in% input$therapeutic_area )
        }
        
        selection
    })
    
    # reactive for descriptive statistics
    approvals_DS <- reactive({
        selection <- fda_approvals
    })
    
    # default no adjustment
    approvals_15_19 <- reactive({
        selection <- fda_approvals %>% filter(Approval_Year != 2020)
        selection
    })
    
    # reactive expression to filter selected approvals
    approvals <- reactive({
        selection <- fda_approvals_long %>%
            select(Brand_Name, Therapeutic_Area, TA_subgroup, Indication, Indication_long, Enrollment, Demographic, Percentage, Approval_Year, Sponsor) %>%
            filter(Percentage != "NA") %>% 
            unique()

        if ( !is.null( input$race ) | !is.null( input$ethnicity ) | !is.null( input$age ) | !is.null( input$sex ) ) {
            selection <- selection %>% filter( Demographic %in% input$race | Demographic %in% input$ethnicity | Demographic %in% input$age | Demographic %in% input$sex)
        }
        
        if ( !is.null( input$year ) ) {
            selection <- selection %>% filter( Approval_Year %in% input$year )
        }
        
        # Find unique set of drugs that have participation values in the bounds from input
        drugs <- selection %>% 
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
            filter(Percentage != "NA")
        
        if ( !is.null( input$race ) | !is.null( input$ethnicity ) | !is.null( input$age ) | !is.null( input$sex ) ) {
            selection <- selection %>% filter( Demographic %in% input$race | Demographic %in% input$ethnicity | Demographic %in% input$age | Demographic %in% input$sex )
        }
        
        if ( !is.null( input$year ) ) {
            selection <- selection %>% filter( Approval_Year %in% input$year )
        }
        
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
            filter(Percentage != "NA")
        
        if ( !is.null( input$race ) | !is.null( input$ethnicity ) | !is.null( input$age ) | !is.null( input$sex ) ) {
            selection <- selection %>% filter( Demographic %in% input$race | Demographic %in% input$ethnicity | Demographic %in% input$age | Demographic %in% input$sex)
        }
        
        if ( !is.null( input$year ) ) {
            selection <- selection %>% filter( Approval_Year %in% input$year )
        }
        
        selection
    })
    
    
    # Reactive expression to filter selected approvals on the TA/disease tab 
    approvals_TA <- reactive({
        selection <- fda_approvals_long %>%
            select(Brand_Name, Therapeutic_Area, TA_subgroup, Indication, Indication_long, Enrollment, Demographic, Percentage, Approval_Year, Sponsor) %>%
            filter(Percentage != "NA") %>% 
            unique()
        
        if ( !is.null( input$race_TA_page ) | !is.null( input$ethnicity_TA_page ) | !is.null( input$age_TA_page ) | !is.null( input$sex_TA_page ) ) {
            selection <- selection %>% filter( Demographic %in% input$race_TA_page | Demographic %in% input$ethnicity_TA_page | Demographic %in% input$age_TA_page | Demographic %in% input$sex_TA_page)
        }
        
        if ( !is.null( input$year_TA_page ) ) {
            selection <- selection %>% filter( Approval_Year %in% input$year_TA_page )
        }
        
        if ( !is.null( input$therapeutic_area ) ) {
            selection <- selection %>% filter( Therapeutic_Area %in% input$therapeutic_area )
        }
        
        if ( "Sponsor" %in% input$Stratify_by ) {
            
            # Identify sponsors with > X drugs
            sponsors <- selection %>% 
                pivot_wider(names_from = Demographic, values_from = Percentage, values_fill = -1) %>% 
                select(Sponsor) %>% 
                group_by(Sponsor) %>% 
                summarize(Count = n()) %>% 
                filter(Count >= input$Sponsor_size) %>% 
                select(Sponsor)
            
            selection <- selection %>% filter( Sponsor %in% sponsors$Sponsor )
            
        }
        
        selection
        
    })
    
    output$Approvals_DS <- renderPlot({
        
        plot <- approvals_DS() %>% 
            ggplot(aes(x=Approval_Year)) + 
            geom_bar(width = 0.7) +
            geom_text(stat='count', aes(label=..count..), vjust=-1) +
            xlab("Year") + 
            ylab("Number of Approvals") +
            theme(axis.title.x=element_text(size=12, face="bold"), axis.title.y=element_text(size=12, face="bold")) + 
            scale_x_continuous(breaks = round(seq(min(2015), max(2020), by = 1),1)) #+ 
            #expand_limits(y=c(0, 70))
        
        if ( input$DS_TA_stratify ){
            plot <- plot + 
                facet_wrap(~Therapeutic_Area) +
                expand_limits(y=c(0, 30))

        }
        
        else{
            plot <- plot + expand_limits(y=c(0, 70))
        }
        
        plot
        
    })
    
    output$Patients_DS <- renderPlot({
        
        plot <- approvals_DS() %>% 
            ggplot(aes(x=Approval_Year, y=Enrollment)) + 
            geom_col(width = 0.7) +
            #geom_text(aes(label=Enrollment), position = position_stack(vjust = 0.5), size = 4) +
            #geom_text(aes(label = Enrollment)) +
            #geom_text(aes(label= sum(Enrollment)), vjust=-1) +
            #geom_text(size = 3, position = position_stack(vjust = 0.5)) +
            xlab("Year") + 
            ylab("Number of patients enrolled in pivotal clinical trial") +
            theme(axis.title.x=element_text(size=12, face="bold"), axis.title.y=element_text(size=12, face="bold")) + 
            scale_x_continuous(breaks = round(seq(min(2015), max(2020), by = 1),1))
        
        if ( input$DS_TA_stratify ){
            plot <- plot + 
                facet_wrap(~Therapeutic_Area) + 
                expand_limits(y=c(0, 75000))
        }
        else{
            plot <- plot + expand_limits(y=c(0, 120000))
        }
        
        plot
        
    })
    
    output$Enrollment_TA_boxplot_DS <- renderPlot({
        plot <- approvals_DS() %>% 
            select(Therapeutic_Area, Enrollment) %>% 
            filter(Enrollment != "NA") %>% 
            mutate(Therapeutic_Area = fct_reorder(Therapeutic_Area, Enrollment, .fun='median')) %>%
            ggplot(aes(Therapeutic_Area, Enrollment)) +
            geom_boxplot() +
            geom_jitter() +
            theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y=element_text(size=12, face="bold")) +
            theme(text = element_text(size=12)) +
            coord_flip() +
            xlab("Therapeutic Area") +
            ylab("Enrollment") +
            ggtitle("Participants per approval")
        
        plot
        
    })
    
    
    output$Validation_Enrollment_by_TA <- renderPlot({
        
        df <- approvals_15_19() %>% 
            #filter(Approval_Year != 2020) %>% 
            select(Therapeutic_Area, Enrollment) %>% 
            mutate(Therapeutic_Area = case_when(
                Therapeutic_Area == "Oncology" ~ "Oncology and Hematology", 
                Therapeutic_Area == "Hematology" ~ "Oncology and Hematology",
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
                                           df$Our_database)#, fda_snapshots)
        names(Total_patients_check) <- c("Therapeutic_Area", "Our_Database")#, "FDA_Snapshots")
        
        #fda_approvals_check_long <- pivot_longer(Total_patients_check, cols = Our_Database:FDA_Snapshots, 
        #                                         names_to = "Database", values_to = "Participants")
        
        fda_approvals_check_long <- pivot_longer(Total_patients_check, cols = Our_Database, 
                                                 names_to = "Database", values_to = "Participants")
        
        Patient_participation_graph <- fda_approvals_check_long %>% 
            ggplot(aes(reorder(Therapeutic_Area, Participants), Participants)) + #, fill=Database)) +
            geom_bar(stat = "identity", position = 'dodge') +
            geom_text(aes(label=Participants), position = position_dodge(0.9), hjust=-0.2, size=4) +
            coord_flip() + 
            expand_limits(y=c(0, 70000)) +
            xlab("Therapeutic Area") +
            ylab("Number of Participants") + 
            scale_y_continuous(breaks = round(seq(min(0), max(70000), by = 5000),1)) +
            #theme(text = element_text(size=20)) +
            theme(axis.title.x=element_text(size=12, face="bold"), axis.title.y=element_text(size=12, face="bold"))# +
            #ggtitle("Patient enrollment by Therapeutic Area")
        
        Patient_participation_graph
        
    })
    
    output$Validation_Demographics <- renderPlot({
        ## To account for missing data, we capture % representation of each demographic only in trials where that demographic is captured
        
        # Race
        
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
        
        race_participation <- c(round(sum(100 * global_asian_participation$Asian_participants) /
                                          sum(global_asian_participation$Enrollment), 0), 
                                round(sum(100 * global_black_participation$Black_participants) / 
                                          sum(global_black_participation$Enrollment), 0),
                                round(sum(100 * global_white_participation$White_participants) / 
                                          sum(global_white_participation$Enrollment), 0))
        
        races <- c("Asian", "Black", "White")
        race_participation <- data.frame("Race", races, race_participation)
        names(race_participation) <- c("Demographic", "Category", "Value")
        
        # Sex
        
        global_female_participation <- approvals_15_19() %>% 
            select(Brand_Name, Enrollment, Female) %>% 
            filter(Female != "NA") %>% 
            mutate(Female_participants = (Female/100) * Enrollment)
        
        global_male_participation <- approvals_15_19() %>%
            select(Brand_Name, Enrollment, Male) %>% 
            filter(Male != "NA") %>% 
            mutate(Male_participants = (Male/100) * Enrollment)
        
        sex_participation <- c(round(sum(100 * global_female_participation$Female_participants) /
                                         sum(global_female_participation$Enrollment), 0), 
                               round(sum(100 * global_male_participation$Male_participants) / 
                                         sum(global_male_participation$Enrollment), 0))
        
        sexes <- c("Female", "Male")
        sex_participation <- data.frame("Sex", sexes, sex_participation)
        names(sex_participation) <- c("Demographic", "Category", "Value")
        
        # Age
        
        global_Age_under_65_participation <- approvals_15_19() %>% 
            select(Brand_Name, Enrollment, Age_under_65) %>% 
            filter(Age_under_65 != "NA") %>% 
            mutate(Age_under_65_participants = (Age_under_65/100) * Enrollment)
        
        global_Age_65_or_older_participation <- approvals_15_19() %>%
            select(Brand_Name, Enrollment, Age_65_or_older) %>% 
            filter(Age_65_or_older != "NA") %>% 
            mutate(Age_65_or_older_participants = (Age_65_or_older/100) * Enrollment)
        
        age_participation <- c(round(sum(100 * global_Age_under_65_participation$Age_under_65_participants) /
                                         sum(global_Age_under_65_participation$Enrollment), 0), 
                               round(sum(100 * global_Age_65_or_older_participation$Age_65_or_older_participants) / 
                                         sum(global_Age_65_or_older_participation$Enrollment), 0))
        
        ages <- c("Age_under_65", "Age_65_or_older")
        age_participation <- data.frame("Age", ages, age_participation)
        names(age_participation) <- c("Demographic", "Category", "Value")
        
        # Combine the tables
        demographic_participation <- rbind(age_participation, sex_participation, race_participation)
        
        
        demographic_participation_graph <- demographic_participation %>% 
            ggplot(aes(x = Category, y = Value)) +
            geom_col(width = 0.5) + 
            geom_text(aes(label = Value), position = position_dodge(0.9), vjust=-0.4, size=4) +
            expand_limits(y=c(0, 100)) +
            theme(axis.title.x=element_text(size=12, face="bold"), axis.title.y=element_text(size=12, face="bold")) + 
            facet_wrap(~Demographic, scales = "free") +
            xlab("") +
            ylab("% Participation") #+
            #ggtitle("Demographics of Trial Participation [FDA approvals 2015-19; excludes 2020 for purposes of dataset validation]")
        
        demographic_participation_graph
        
    })

    output$individualPlot <- renderPlotly({
        
        plot <- approvals() %>% 
            ggplot(aes(Demographic, Percentage, text = paste("Therapeutic Area:", Therapeutic_Area,
                                                             "<br>Indication:", Indication,
                                                             "<br>Drug:", Brand_Name, 
                                                             "<br>Sponsor:", Sponsor
                                                             ))) +
            scale_y_continuous(breaks = round(seq(min(0), max(100), by = 5),1)) +
            theme(axis.text.x = element_text(size=8),
                  axis.text.y = element_text(size=8),
                  axis.title.x=element_text(size=12, face="bold"),
                  axis.title.y=element_text(size=12, face="bold"))
            #geom_jitter(width = 0.2, aes(colour=Demographic)) + 
            #theme(legend.position = "top")+#, legend.title = element_blank()) +
             #+
            #theme(axis.title.x=element_text(size=12, face="bold"), axis.title.y=element_text(size=12, face="bold"))
            #ggtitle("Distribution of clinical trial participation")
        
        if ( input$is_TA_Stratified ) {
            plot <- plot + 
                facet_wrap(~Therapeutic_Area) + 
                scale_y_continuous(breaks = round(seq(min(0), max(100), by = 10),1))
        }
        
        if ( input$is_Year_labelled ) {
            plot <- plot + 
                geom_jitter(width = 0.2, aes(colour=factor(Approval_Year))) +
                scale_color_brewer(palette="PuRd")
        }
        else{
            plot <- plot + 
                geom_jitter(width = 0.2, aes(colour=Demographic))
                
        }
        
        #plot <- plot + theme(legend.position = "top")
    
        ggplotly(plot) %>% 
            layout(legend = list(orientation = "h", y = 1.1, x = 0.03))
        
    })
    
    output$stats_summary_Table <- DT::renderDataTable(
        
        if ( input$is_TA_Stratified ) { 
            if (input$is_Year_labelled) { # Year and TA
                demographics_selected <- approvals() %>% select(Demographic) %>% unique()
                
                summary_statistics_all_years %>% filter(Demographic %in% demographics_selected$Demographic)
                
                #approvals() %>% 
                #    group_by(Demographic, Therapeutic_Area, Approval_Year) %>% 
                #    summarize(Median = round(median(Percentage), 1), 
                #              Average = round(mean(Percentage), 1))
            }
            else{ # TA only
                approvals() %>% 
                    group_by(Demographic, Therapeutic_Area) %>% 
                    summarize(Median = round(median(Percentage), 1), 
                              Average = round(mean(Percentage), 1))
            }
        }
        else if ( input$is_Year_labelled) { # Year only
            demographics_selected <- approvals() %>% select(Demographic) %>% unique()
            
            summary_statistics %>% filter(Demographic %in% demographics_selected$Demographic)
            
            #approvals() %>% 
            #    group_by(Demographic, Approval_Year) %>% 
            #    summarize(Median = round(median(Percentage), 1), 
            #              Average = round(mean(Percentage), 1))
        }
        else{
            
            demographics_selected <- approvals() %>% select(Demographic) %>% unique()
            
            summary_statistics_all_years %>% filter(Demographic %in% demographics_selected$Demographic)
            
            #approvals() %>% 
            #    group_by(Demographic) %>% 
            #    summarize(Median = round(median(Percentage), 1), 
            #              Average = round(mean(Percentage), 1))
            
        }
    )
    
    output$participationCountPlot <- renderPlot({
        
        # bar chart - for all years
        plot <- approvals_count() %>% 
            group_by(Demographic, Percentage) %>% 
            summarize(Count = sum(Count)) %>% 
            filter(Percentage <= 25) %>%
            ggplot(aes(Percentage, y=Count)) +
            geom_bar(stat = "identity", position = 'dodge', width = 0.8) +
            geom_text(aes(label=Count), position = position_dodge(0.9), vjust=-0.3, size=3) +
            #ylim(0,65) +
            expand_limits(y=c(0, 65)) +
            scale_x_continuous(breaks = round(seq(0, 25, by = 1),1)) +
            theme(axis.title.x=element_text(size=12, face="bold"), axis.title.y=element_text(size=12, face="bold")) + 
            xlab("Percent participation") +
            ylab("Count of FDA approvals") +
            #ggtitle("Number of FDA approvals with under 25 percent participation by demographic in its trials") +
            facet_wrap(~Demographic, ncol=1)
        
        plot
    })
    
    output$change_over_time <- renderPlot({
        
        plot <- approvals() %>% 
            #filter(Demographic %in% dems) %>% 
            filter(Percentage != "NA") %>% 
            ggplot(aes(x = Demographic, y = Percentage, fill = factor(Approval_Year))) +
            geom_boxplot(outlier.shape = NA) +
            scale_fill_brewer(palette="PuRd")+
            geom_point(position=position_jitterdodge(),alpha=0.1) +
            scale_y_continuous(breaks = round(seq(0, 100, by = 10),1)) +
            theme(legend.position = "top",
                  legend.title = element_blank(),
                  axis.title.x=element_text(size=12, face="bold"), 
                  axis.title.y=element_text(size=12, face="bold"))
        
        if ( input$is_TA_Stratified ) {
            plot <- plot + 
                facet_wrap(~Therapeutic_Area, scales = "free")
        }
        
        plot
        
    })
    
    output$approvalsTable <- DT::renderDataTable(
        approvals() %>% 
            pivot_wider(names_from = Demographic, values_from = Percentage), options=list(pageLength=10)
    )
    
    output$TA_individualPlot <- renderPlotly({
        
        plot <- approvals_TA() %>% 
            ggplot(aes(Demographic, Percentage, text = paste("Therapeutic Area:", Therapeutic_Area,
                                                             "<br>Indication:", Indication,
                                                             "<br>Drug:", Brand_Name, 
                                                             "<br>Sponsor:", Sponsor)))
        
        if ( input$is_Enrollment_Stratified ) {
            plot <- plot + 
                geom_jitter(width = 0.2, aes(colour=Demographic, size = Enrollment)) + 
                theme(legend.position = "top", legend.title = element_blank()) +
                scale_y_continuous(breaks = round(seq(min(0), max(100), by = 5),1)) +
                ggtitle("Participation in clinical trials by demographic <br> Each dot represents % participation in one trial")
            
        }   
        else{
            plot <- plot + 
                geom_jitter(width = 0.2, aes(colour=Demographic)) + 
                theme(legend.position = "top", legend.title = element_blank()) +
                scale_y_continuous(breaks = round(seq(min(0), max(100), by = 5),1)) +
                ggtitle("Participation in clinical trials by demographic <br> Each dot represents % participation in one trial")
        }
        
        if ("None" %in% input$Stratify_by){
            plot <- plot
        }
        
        if ("Sponsor" %in% input$Stratify_by) {
            plot <- plot + 
                facet_wrap(~Sponsor) + 
                scale_y_continuous(breaks = round(seq(min(0), max(100), by = 10),1))
        }
        
        if ("TA_subgroup" %in% input$Stratify_by) {
            plot <- plot + 
                facet_wrap(~TA_subgroup) + 
                scale_y_continuous(breaks = round(seq(min(0), max(100), by = 10),1))
        }
        
        plot
    })
    
    output$TA_Disease_Burden_Comparison <- renderPlot({
        burden <- disease_burden()# %>% sort(Disease)
        trials <- approvals_TA() %>% 
            pivot_wider(names_from = Demographic, values_from = Percentage) #%>% 
        #    arrange(-Disease)
        
        df <- data.frame(trials$Disease, 
                         burden$Black,
                         burden$Hispanic,
                         trials$Black,
                         trials$Hispanic)
        
        #titles <- c("Disease", "Black_burden", "Hispanic_burden")
        
        #Race_distribution_comparison <- data.frame(races, fda_snapshots, demographic_participation)
        #names(Race_distribution_comparison) <- c("Race", "FDA_Snapshots", "Our_Database")
        
        #df %>% 
        
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
