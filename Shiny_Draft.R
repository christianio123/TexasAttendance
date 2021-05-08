library(tidyverse)
library(shiny)
library(shinythemes)
library(plotly)
library(shinydashboard)
library(colourpicker)
library(leaflet)
library(maps)
library(scales)
library(tableHTML)
#library(shinydashboardPlus)

cleaned <- theme(panel.background = element_blank(),
                 panel.border = element_blank(),
                 axis.ticks = element_blank(),
                 panel.grid = element_blank(),
                 plot.title = element_text(hjust = 0.5),
                 plot.subtitle = element_text(hjust = 0.5),
                 legend.title.align=0.5)

header <- dashboardHeader(title = "Student Attendance")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "menu1",
              menuItem("About", tabName = 'name1', icon = icon('address-card')),
              menuItem("Grade Level", tabName = 'name2', icon = icon('chalkboard-teacher')),
              menuItem("School Level", tabName = 'name3', icon = icon('school')),
              menuItem("District Level", tabName = 'name4', icon = icon('chart-area')),
              menuItem("Sources", tabName = 'name5', icon = icon('globe')))
)







body <- dashboardBody(
  tags$style(make_css(list('.box',
                           c('font-size', 'font-family', 'color'),
                           c('20px', 'times-new-roman', 'black')))),
  tabItems(
    tabItem(tabName = 'name1',
            box(
              title = h1('Purpose'),
              status = 'primary',
              solidHeader = T,
              width = 12,
              collapsible = T,
              fluidRow(
                column(
                  width = 7,
                  "The purpose of this project is to help visualize factors associated with student attendance
                  in the state of Texas. There is no target audience, however, anyone associated 
                  in education may find the data helpful for any interests they have regarding student attendance 
                  in Texas for the first two months of the 2020-2021 academic school year. This topic was targeted
                  specifically due to the abnormalities of the academic school year."),
                column(
                  width = 5,
                  align = 'center',
                  img(
                    src = 'https://cdn.pixabay.com/photo/2017/09/07/20/09/abstract-2726482_960_720.png',
                    width = '200',
                    height = '200')
                  )
                )
              ),
            box(
              title = h1('Data Selection'),
              status = 'success',
              solidHeader = T,
              width = 12,
              collapsible = T,
              collapsed = T,
              tabBox(
                title = 'Data Variables',
                width = 12,
                id = 'about_data',
                tabPanel(
                  'Daily Student Attendance',
                  "To attain student attendance information, an email was sent out to 40 school districts around
                  the state of Texas on November 2nd, 2020 using the Freedom of Information Act. Of those districts,
                  19 responded with the requested data, while some districts required a large fee for the data due to
                  the number of hours associated with labor. Due to ambiguity in the original email sent to districts,
                  varying types of data were collected. The major difference between data received was 'daily records'
                  of student attendance and a 'summary' of student attendance records so far this academic school year.
                  School districts took between 10 to 15 business days to respond, not including the holidays. The focus
                  of this project 'daily student attendance' to find correlations with other variables. 11 of the 19
                  datasets were used for this project. \n The 11 schools districts that sent data were (1) Conroe ISD, (2) Cypress-Fairbanks ISD, (3) Floydala ISD,
                  (4) Forworth ISD, (5) Pasadena ISD, (6) Snook ISD, (7) Socorro ISD, (8) Klein ISD, (9) Garland ISD,
                  (10) Dallas ISD, and (11) Katy ISD. However, discrepancies existed within these datasets as well. One such 
                  discrepancies included three school districts sending daily attendance data with student grade level but 
                  one school district did not include any other information. Also, nine school districts included student
                  attendance data broken down by school while three other school districts only had student attendance alone. 
                  This information is important to explain steps taken to prepare data for analysis later. Variables used include 
                  (a) dates, (b) weekdays, (c) school name, (d) school type, (e) district, and (f) grade level."
                ),
                tabPanel(
                  'School Information, County Description, Metropolitan vs. Non-Metropolitan',
                  'In addition to daily student attendance data, two other datasets were used from the Texas Education Agence
                  with data about each school and school district. One dataset, "Current Schools", information about each school
                  included addressm principal, county name, district number, and more as of May 2020. From this dataset, the variables
                  selected were (a) school name, (b) school zip, (c) district number, (d) and school type. In the second dataset, 
                  "District Type", school districts were labeled "major urban", "independent town", or "rural area". 
                  From "District Type" dataset, selected variables used included (a) district, (b) district number, (c) Texas 
                  Education Agency (TEA) Description, (d) and National Center of Education Statistics (NCES) Description. 
                  To determine if a county is "metropolitan" or "non-metropolitan", a dataset from the Texas Health and Human Services 
                  was used. Selected variables from this dataset included county name and metro area.'
                ),
                tabPanel(
                  'COVID-19',
                  "Student attendance has ben noticeably different this academic school year, therefore COVID-19 data was attained 
                  from the New York Times to examine relationships, if any. This dataset is updated daily with data being available 
                  in three formats (country, state, and county). From this dataset, variables selected were both COVID-19 cases by 
                  state and county."
                ),
                tabPanel(
                  'Demographics',
                  "Each school has a unique student population, therefore census data from 2018 (with best estimate of today's 
                  current population) was used to find the makeup of a population sorrounding a school by zip code. Variables selected 
                  from census data include (a) zip code, (b) race/ethnicity, (c) medium income, (d) unemployment rate, and (e) education. 
                  These variables were selected to view correlations between school attendance based on the makeup of the population 
                  sorrounding a school."
                ),
                tabPanel(
                  'Weather Data',
                  "Weather data collected from National Centers for Environmental Information by county. Of the available variables, those 
                  chosen include daily precipitation, temperature high, and temperature low."
                )
              )),
            box(
              title = h1('Data Preparation'),
              status = 'warning',
              solidHeader = T,
              width = 12,
              collapsible = T,
              collapsed = T,
              markdown(
                "Datasets were received in a variety of mediums, ranging from PDF to excel documents. This lead to a series of steps to make
                             data practical to read and use.
                             
                             1. Extract information from PDF documents - the two PDF documents were SNOOK ISD and FLOYDALA ISD datasets.
                             2. Extract informations from Excel files - All other documents were in excel format.
                             3. Join datasets together - Because of the nature of each dataset, not all datasets could be jointed together immediately.
                             Therefore, dataset joining began with the districts who had grade level information **and** school level information were grouped
                             by both district and date and the joined with the other district datasets having district information only. The final step was grouping
                             each dataset by district and date only and then joining every dataset. At the end of this process there were three different datasets.
                             
                             __*<div align = 'center'> Grade Level Dataset --> School Level Dataset --> District Level Dataset </div>*__
                             
                             4. TEA present school information was joined with the school level dataset using school names. Due to discrepancies
                             between every dataset in school spelling and characters used, cleaning was administered on all datasets.
                             5. TEA district information dataset easily joined with each student attendance dataset by the 'district' 
                             variable found in each dataset.
                             6. The Texas Health abd Human Services dataset joined each grade level dataset using the 'county' variable found in each dataset.
                             7. Each student attendance dataset was then joined to the COVID-19 dataset using the 'county' and 'date' variable found in both
                             datasets.
                             8. U.S. census data joined with the student attendance datasets using 'zip' code variable in each dataset.
                             9. Missing information was filled using the available datasets.
                             10. Weather dataset was joined with the three datasets based on county."
                )
              ),
            box(
              title = h1('Author'),
              status = 'danger',
              solidHeader = T,
              width = 12,
              collapsible = T,
              collapsed = T,
              fluidRow(
                column(
                  width = 7,
                  "My name is Christian Ortiz and I am an aspiring data scientist presently an educator for the Houston Independent School District (HISD)
                  in Houston, Texas. As an educator, I have always noticed trends in student attendance, that is sometimes students
                  would miss school on Fridays more often than any other day of the week or were a little less likely to show up on
                  a rainy day. I was curious to know whether this was actually a trend or just something that I am imagined to be true. 
                  I also wanted to know that if there were a trend, was it just in the type of community I served or across the board in most places.
                  This dataset was created to help visualize these trends, enjoy!"
                ),
                column(
                  width = 5,
                  align = 'center',
                  img(
                    src = 'https://storage.googleapis.com/kaggle-avatars/images/6118987-kg.jpg',
                    width = '200',
                    height = '200')
                  )
                )
              )
            ),
    tabItem(tabName = 'name2',
            h1('Grade Level Data'),
            tabBox(
              title = 'Grade Level Dataset Visualizations',
              width = 12,
              tabPanel("Overall Grade Level Data",
                radioButtons(
                  'average_datatype',
                  "Choose Center of Distribution Type:",
                  c(
                    'mean', 
                    'median')
                  ),
                plotlyOutput(
                  'overall_gradelevel_attend'
                  ),
                checkboxInput(
                  'filter_down_gset',
                  'Filter Data',
                  value = FALSE
                  ),
                conditionalPanel(
                  condition = "input.filter_down_gset == 1",
                  radioButtons(
                    'gset_filter_choice',
                    'Group the Data By:',
                    c(
                      'District',
                      'MetroStatus',
                      'County',
                      'Weekday'
                      )
                    )
                  )
                ),
              tabPanel("Specific Grade Level Data",
                       selectInput(
                         'gradelevel_choice',
                         'Select Grade Level:',
                         unique(
                           CompleteGradeLevelDataset$GradeLevel)
                         ),
                       selectInput(
                         'gradelevel_datatype',
                         "Select Data of Interest:",
                         c('COVID', 'Temperature','Rain'),
                         selected = 'COVID'
                       ),
                       plotlyOutput('specific_grade_graph')
                       )
              )
            ),
    tabItem(tabName = 'name3',
            h1('School Level Data')),
    tabItem(tabName = 'name4',
            h1('District Level Data')),
    tabItem(tabName = 'name5',
            h1('Sources')))
  )
  




server <- function(input, output) {
  
  ######################################################################
  ######################################################################
  # GRADE LEVEL
  
  # Creating Serverside Visualization for Overall Grade Level Attendance
  output$overall_gradelevel_attend <- renderPlotly(
    if ('mean' == input$average_datatype){
      if (1 == input$filter_down_gset){
        if ('District' == input$gset_filter_choice){
          p <- CompleteGradeLevelDataset %>%
            group_by(GradeLevel, District) %>%
            summarize(AbsentPercent = mean(AbsentPercent, na.rm = T)) %>%
            ggplot(aes(GradeLevel, AbsentPercent)) +
            geom_col() +
            facet_wrap(.~ District, ncol = 1, scales = 'free_y')
        }
        else if ('MetroStatus' == input$gset_filter_choice){
          p <- CompleteGradeLevelDataset %>%
            group_by(GradeLevel, MetroStatus) %>%
            summarize(AbsentPercent = mean(AbsentPercent, na.rm = T)) %>%
            ggplot(aes(GradeLevel, AbsentPercent)) +
            geom_col() +
            facet_wrap(~MetroStatus, ncol=1, scale = 'free_y')
        }
        else if ('County' == input$gset_filter_choice){
          p <- CompleteGradeLevelDataset %>%
            group_by(GradeLevel, County) %>%
            summarize(AbsentPercent = mean(AbsentPercent, na.rm = T)) %>%
            ggplot(aes(GradeLevel, AbsentPercent)) +
            geom_col() +
            facet_wrap(~County, ncol=1, scale = 'free_y')
        }
        else if ('Weekday' == input$gset_filter_choice){
          p <- CompleteGradeLevelDataset %>%
            group_by(GradeLevel, Weekday) %>%
            summarize(AbsentPercent = mean(AbsentPercent, na.rm = T)) %>%
            ggplot(aes(GradeLevel, AbsentPercent)) +
            geom_col() +
            facet_wrap(~Weekday, ncol=1, scale = 'free_y')
        }
      }
      else {
        p <- CompleteGradeLevelDataset %>%
          group_by(GradeLevel) %>%
          summarize(AbsentPercent = mean(AbsentPercent, na.rm = T)) %>%
          ggplot(aes(GradeLevel, AbsentPercent)) +
          geom_col()
      }
      p
      }
    
    else if ('median' == input$average_datatype){
      
      if (1 == input$filter_down_gset){
        
        if ('District' == input$gset_filter_choice){
          p <- CompleteGradeLevelDataset %>%
            group_by(GradeLevel, District) %>%
            summarize(AbsentPercent = median(AbsentPercent, na.rm = T)) %>%
            ggplot(aes(GradeLevel, AbsentPercent)) +
            geom_col() +
            facet_wrap(.~ District, ncol = 1, scales = 'free_y')
        }
        else if ('MetroStatus' == input$gset_filter_choice){
          p <- CompleteGradeLevelDataset %>%
            group_by(GradeLevel, MetroStatus) %>%
            summarize(AbsentPercent = median(AbsentPercent, na.rm = T)) %>%
            ggplot(aes(GradeLevel, AbsentPercent)) +
            geom_col() +
            facet_wrap(~MetroStatus, ncol=1, scale = 'free_y')
        }
        else if ('County' == input$gset_filter_choice){
          p <- CompleteGradeLevelDataset %>%
            group_by(GradeLevel, County) %>%
            summarize(AbsentPercent = median(AbsentPercent, na.rm = T)) %>%
            ggplot(aes(GradeLevel, AbsentPercent)) +
            geom_col() +
            facet_wrap(~County, ncol=1, scale = 'free_y')
        }
        else if ('Weekday' == input$gset_filter_choice){
          p <- CompleteGradeLevelDataset %>%
            group_by(GradeLevel, Weekday) %>%
            summarize(AbsentPercent = median(AbsentPercent, na.rm = T)) %>%
            ggplot(aes(GradeLevel, AbsentPercent)) +
            geom_col() +
            facet_wrap(~Weekday, ncol=1, scale = 'free_y')
          }
        }
      else {
        p <- CompleteGradeLevelDataset %>%
          group_by(GradeLevel) %>%
          summarize(AbsentPercent = median(AbsentPercent, na.rm = T)) %>%
          ggplot(aes(GradeLevel, AbsentPercent)) +
          geom_col()
      }
      }
  )
  
  ##############################################################################
  ##############################################################################
  # Specific Grade Level Tab Box
  
  # Clean COVID Outliers
  
  # Remove Covid Outliers
  dailycovid_q <- quantile(CompleteGradeLevelDataset$CovidCountyPercentIncrease, probs = c(.25,.75), na.rm=T)
  dailycovid_iqr <- IQR(CompleteGradeLevelDataset$CovidCountyPercentIncrease, na.rm=T)  
  up <- dailycovid_q[2] + 1.5 * dailycovid_iqr  
  low <- dailycovid_q[1] - 1.5 * dailycovid_iqr 
  dailycovid <- subset(CompleteGradeLevelDataset, CompleteGradeLevelDataset$CovidCountyPercentIncrease > low & CompleteGradeLevelDataset$CovidCountyPercentIncrease < up)
  
  
  dailyabsent_q <- quantile(dailycovid$AbsentPercent, probs = c(.25,.75), na.rm=T)
  dailyabsent_iqr <- IQR(dailycovid$AbsentPercent, na.rm=T)
  up <- dailyabsent_q[2] + 1.5 * dailyabsent_q  
  low <- dailyabsent_q[1] - 1.5 * dailyabsent_iqr
  dailycovid <- subset(dailycovid, dailycovid$AbsentPercent > low & dailycovid$AbsentPercent < up)
  dailycovid <- dailycovid [!is.na(dailycovid$CovidCountyPercentIncrease) & !is.na(dailycovid$AbsentPercent),]
  
  ##########################
  # GRAPHS
  output$specific_grade_graph <- renderPlotly(
    if ('COVID' == input$gradelevel_datatype){
      dailycovid %>%
        filter(GradeLevel == input$gradelevel_choice) %>%
        group_by(Day) %>%
        summarize(StateCovid = mean(CovidStatePercentIncrease, na.rm = T),
                  AbsentPercent = mean(AbsentPercent, na.rm = T)) %>%
        ggplot(aes(x=Day)) +
        geom_line(aes(y=StateCovid, color = "State Covid")) +
        geom_line(aes(y = AbsentPercent, color = "Student Absence"))
    }
    else if ('Temperature' == input$gradelevel_datatype){
      CompleteGradeLevelDataset %>%
        filter(GradeLevel == input$gradelevel_choice & AbsentPercent != 100) %>%
        group_by(County, Day) %>%
        summarize(AbsentPercent = mean(AbsentPercent, na.rm=T),
                  Max_Temp = mean(TMAX, na.rm=T),
                  Min_Temp = mean(TMIN, na.rm = T)) %>%
        ggplot(aes(x=Day)) +
        geom_line(aes(y = Max_Temp, color = "Max Temp")) +
        geom_line(aes(y = Min_Temp, color = "Min Temp")) +
        geom_line(aes(y = AbsentPercent, color = 'Student Absence')) +
        facet_wrap(.~County)
    }
    else if ('Rain' == input$gradelevel_datatype){
      CompleteGradeLevelDataset %>%
        filter(GradeLevel == input$gradelevel_choice & AbsentPercent != 100) %>%
        group_by(County, Day) %>%
        summarize(AbsentPercent = mean(AbsentPercent, na.rm=T),
                  Precepitation = mean(PRCP, na.rm=T)) %>%
        ggplot(aes(x=Day))+
        geom_line(aes(y = Precepitation, color = 'Precepitation')) +
        geom_line(aes(y = AbsentPercent, color = 'Student Absence')) +
        facet_wrap(.~County)
    }
  )
  }


  
    





ui <- dashboardPage(skin = 'red',
                    header = header,
                    sidebar = sidebar,
                    body = body)


shinyApp(ui, server)

