library(tidyverse)
library(readxl)
library(pdftools)
library(scales)
library(reshape2)
library(lubridate)

### Code Lines 31 - 4444 : Cleaning and Joining Dataset(s)
###
###
###
###
###
###
###
###
###
###
###
###
###
###
###
###
###
###
###
###
###
######################################################################################

school_datatype_summary <- read_excel(".\\school_datatype_summary.xlsx")
school_datatype_summary

# Filter for schools with daily data

school_daily_data <- school_datatype_summary %>% filter(DailyData == 1)
school_daily_data

# Filter for PDF files

school_pdf <- school_datatype_summary %>%
  filter(PDF == 1)
school_pdf <- school_pdf[,1]
school_pdf

##########################################################################################

# FLOYDALA PDF #1


# Floydala Grade Early Education PDF 
floydala_early_ed_1 <- pdf_text(".\\floydala_abd_early_education_first_six_weeks.pdf") %>%
  readr::read_lines()

# The PDF was split into august and september
floydala_early_ed_august <- floydala_early_ed_1[-c(1:7, 23:57)] %>%
  str_squish() %>%
  strsplit(split = " ")

# fill missing values
lapply(floydala_early_ed_august, length)
floydala_early_ed_august[[6]] <- c('22', 'NA', 'NA', 'NA', 'NA')
floydala_early_ed_august[[7]] <- c('23', 'NA', 'NA', 'NA', 'NA')
floydala_early_ed_august[[13]] <- c('29', 'NA', 'NA', 'NA', 'NA')
floydala_early_ed_august[[14]] <- c('30', 'NA', 'NA', 'NA', 'NA')

# create dataframe
floydala_early_ed_august <-   plyr::ldply(floydala_early_ed_august)
floydala_early_ed_august <- floydala_early_ed_august[1:3]
colnames(floydala_early_ed_august) <- c('Day', 'Abs', 'Mem')
floydala_early_ed_august <- as_tibble(floydala_early_ed_august)
floydala_early_ed_august[1] <- seq(as.Date("2020/08/17"), by = "day", length.out = 15)
floydala_early_ed_august$Abs <- as.numeric(floydala_early_ed_august$Abs)
floydala_early_ed_august$Mem <- as.numeric(floydala_early_ed_august$Mem)

floydala_early_ed_august

# Same file, page 2
floydala_early_ed_september <- floydala_early_ed_1[-c(1:31, 57)]

floydala_early_ed_september <- floydala_early_ed_september %>%
  str_squish() %>%
  strsplit(split = " ")

# fill missing values
lapply(floydala_early_ed_september, length)
floydala_early_ed_september[[5]] <- c('05', 'NA', 'NA', 'NA', 'NA')
floydala_early_ed_september[[6]] <- c('06', 'NA', 'NA', 'NA', 'NA')
floydala_early_ed_september[[7]] <- c('07', 'NA', 'NA', 'NA', 'NA')
floydala_early_ed_september[[12]] <- c('12', 'NA', 'NA', 'NA', 'NA')
floydala_early_ed_september[[13]] <- c('13', 'NA', 'NA', 'NA', 'NA')
floydala_early_ed_september[[19]] <- c('19', 'NA', 'NA', 'NA', 'NA')
floydala_early_ed_september[[20]] <- c('20', 'NA', 'NA', 'NA', 'NA')

# create dataframe
floydala_early_ed_september <-   plyr::ldply(floydala_early_ed_september)
floydala_early_ed_september <- floydala_early_ed_september[1:3]
colnames(floydala_early_ed_september) <- c('Day', 'Abs', 'Mem')
floydala_early_ed_september <- as_tibble(floydala_early_ed_september)
floydala_early_ed_september[1] <- seq(as.Date("2020/09/01"), by = "day", length.out = 25)
floydala_early_ed_september$Abs <- as.numeric(floydala_early_ed_september$Abs)
floydala_early_ed_september$Mem <- as.numeric(floydala_early_ed_september$Mem)

floydala_early_ed_september


######################################################################################


# FLOYDALA PDF #2


# Floydala Grade Early Education Second Cycle PDF 
floydala_early_ed_2 <- pdf_text(".\\floydala_abd_early_education_second_six_weeks.pdf") %>%
  readr::read_lines()

# The PDF was split into september, october, and november
floydala_early_ed_september2 <- floydala_early_ed_2[-c(1:7, 10:73)] %>%
  str_squish() %>%
  strsplit(split = " ")

# create dataframe
floydala_early_ed_september2 <-   plyr::ldply(floydala_early_ed_september2)
floydala_early_ed_september2 <- floydala_early_ed_september2[1:3]
colnames(floydala_early_ed_september2) <- c('Day', 'Abs', 'Mem')
floydala_early_ed_september2 <- as_tibble(floydala_early_ed_september2)
floydala_early_ed_september2[1] <- seq(as.Date("2020/09/29"), by = "day", length.out = 2)
floydala_early_ed_september2$Abs <- as.numeric(floydala_early_ed_september2$Abs)
floydala_early_ed_september2$Mem <- as.numeric(floydala_early_ed_september2$Mem)


# Bind both septembers together
floydala_early_ed_september <- rbind(floydala_early_ed_september, floydala_early_ed_september2)

floydala_early_ed_september
rm(floydala_early_ed_september2)



# OCTOBER
floydala_early_ed_october <- floydala_early_ed_2[-c(1:18, 49:56, 58:73)] %>%
  str_squish() %>%
  strsplit(split = " ")

# fill missing values
lapply(floydala_early_ed_october, length)
floydala_early_ed_october[[3]] <- c('3', 'NA', 'NA', 'NA', 'NA')
floydala_early_ed_october[[4]] <- c('4', 'NA', 'NA', 'NA', 'NA')
floydala_early_ed_october[[10]] <- c('10', 'NA', 'NA', 'NA', 'NA')
floydala_early_ed_october[[11]] <- c('11', 'NA', 'NA', 'NA', 'NA')
floydala_early_ed_october[[12]] <- c('12', 'NA', 'NA', 'NA', 'NA')
floydala_early_ed_october[[17]] <- c('17', 'NA', 'NA', 'NA', 'NA')
floydala_early_ed_october[[18]] <- c('18', 'NA', 'NA', 'NA', 'NA')
floydala_early_ed_october[[24]] <- c('24', 'NA', 'NA', 'NA', 'NA')
floydala_early_ed_october[[25]] <- c('25', 'NA', 'NA', 'NA', 'NA')
floydala_early_ed_october[[31]] <- c('31', 'NA', 'NA', 'NA', 'NA')

# create dataframe
floydala_early_ed_october <-   plyr::ldply(floydala_early_ed_october)
floydala_early_ed_october <- floydala_early_ed_october[1:3]
colnames(floydala_early_ed_october) <- c('Day', 'Abs', 'Mem')
floydala_early_ed_october <- as_tibble(floydala_early_ed_october)
floydala_early_ed_october[1] <- seq(as.Date("2020/10/01"), by = "day", length.out = 31)
floydala_early_ed_october$Abs <- as.numeric(floydala_early_ed_october$Abs)
floydala_early_ed_october$Mem <- as.numeric(floydala_early_ed_october$Mem)


floydala_early_ed_october



# NOVEMBER
floydala_early_ed_november <- floydala_early_ed_2[-c(1:66,73)] %>%
  str_squish() %>%
  strsplit(split = " ")

# fill missing values
lapply(floydala_early_ed_november, length)
floydala_early_ed_november[[1]] <- c('1', 'NA', 'NA', 'NA', 'NA')

# create dataframe
floydala_early_ed_november <-   plyr::ldply(floydala_early_ed_november)
floydala_early_ed_november <- floydala_early_ed_november[1:3]
colnames(floydala_early_ed_november) <- c('Day', 'Abs', 'Mem')
floydala_early_ed_november <- as_tibble(floydala_early_ed_november)
floydala_early_ed_november[1] <- seq(as.Date("2020/11/01"), by = "day", length.out = 6)
floydala_early_ed_november$Abs <- as.numeric(floydala_early_ed_november$Abs)
floydala_early_ed_november$Mem <- as.numeric(floydala_early_ed_november$Mem)

floydala_early_ed_november


###############################################################################

# BIND ALL EARLY EDUCATION DATASETS

floydala_early_education_all_dates <- rbind(floydala_early_ed_august,
                                            floydala_early_ed_september,
                                            floydala_early_ed_october,
                                            floydala_early_ed_november)

###############################################################################


# FLOYDALA PDF #3


# Floydala Grade A B Duncan Elementary School PDF 1
# Split into months
floydala_abd_1 <- pdf_text(".\\floydala_abd_first_six_weeks.pdf") %>%
  readr::read_lines()

floydala_abd_es_august <- floydala_abd_1[-c(1:7, 23:57)]
floydala_abd_es_september <- floydala_abd_1[-c(1:31,57)]


# Floydala Grade A B Duncan ES August
floydala_abd_es_august <- floydala_abd_es_august %>%
  str_squish() %>%
  strsplit(split = " ")

floydala_abd_es_august

# fill missing values
lapply(floydala_abd_es_august, length)
floydala_abd_es_august[[6]] <- c('22', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA', 'NA')
floydala_abd_es_august[[7]] <- c('23', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA', 'NA')
floydala_abd_es_august[[13]] <- c('29', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA', 'NA', 'NA', 'NA')
floydala_abd_es_august[[14]] <- c('30', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA', 'NA', 'NA', 'NA')

# create dataframe
floydala_abd_es_august <-   plyr::ldply(floydala_abd_es_august)
floydala_abd_es_august <- floydala_abd_es_august[1:13]
colnames(floydala_abd_es_august) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                      'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem')
floydala_abd_es_august[1] <- seq(as.Date("2020/08/17"), by = "day", length.out = 15)

# Separate by grade level

# Kinder
floydala_abd_es_august_kinder <- floydala_abd_es_august[c(1:3)]
floydala_abd_es_august_kinder <- as_tibble(floydala_abd_es_august_kinder)
floydala_abd_es_august_kinder$Abs <- as.numeric(floydala_abd_es_august_kinder$Abs)
floydala_abd_es_august_kinder$Mem <- as.numeric(floydala_abd_es_august_kinder$Mem)

floydala_abd_es_august_kinder

# First Grade
floydala_abd_es_august_first_grade <- floydala_abd_es_august[c(1,4,5)]
floydala_abd_es_august_first_grade <- as_tibble(floydala_abd_es_august_first_grade)
floydala_abd_es_august_first_grade$Abs <- as.numeric(floydala_abd_es_august_first_grade$Abs) 
floydala_abd_es_august_first_grade$Mem <- as.numeric(floydala_abd_es_august_first_grade$Mem)

floydala_abd_es_august_first_grade


# Second Grade
floydala_abd_es_august_second_grade <- floydala_abd_es_august[c(1,6,7)]
floydala_abd_es_august_second_grade <- as_tibble(floydala_abd_es_august_second_grade)
floydala_abd_es_august_second_grade$Abs <- as.numeric(floydala_abd_es_august_second_grade$Abs)
floydala_abd_es_august_second_grade$Mem <- as.numeric(floydala_abd_es_august_second_grade$Mem)

floydala_abd_es_august_second_grade

# Third Grade
floydala_abd_es_august_third_grade <- floydala_abd_es_august[c(1,8,9)] 
floydala_abd_es_august_third_grade <- as_tibble(floydala_abd_es_august_third_grade)
floydala_abd_es_august_third_grade$Abs <- as.numeric(floydala_abd_es_august_third_grade$Abs)
floydala_abd_es_august_third_grade$Mem <- as.numeric(floydala_abd_es_august_third_grade$Mem)

floydala_abd_es_august_third_grade

# Fourth Grade
floydala_abd_es_august_fourth_grade <- floydala_abd_es_august[c(1,10,11)]
floydala_abd_es_august_fourth_grade <- as_tibble(floydala_abd_es_august_fourth_grade)
floydala_abd_es_august_fourth_grade$Abs <- as.numeric(floydala_abd_es_august_fourth_grade$Abs)
floydala_abd_es_august_fourth_grade$Mem <- as.numeric(floydala_abd_es_august_fourth_grade$Mem)

floydala_abd_es_august_fourth_grade

# Fifth Grade
floydala_abd_es_august_fifth_grade <- floydala_abd_es_august[c(1,12,13)]
floydala_abd_es_august_fifth_grade <- as_tibble(floydala_abd_es_august_fifth_grade)
floydala_abd_es_august_fifth_grade$Abs <- as.numeric(floydala_abd_es_august_fifth_grade$Abs)
floydala_abd_es_august_fifth_grade$Mem <- as.numeric(floydala_abd_es_august_fifth_grade$Mem)

floydala_abd_es_august_fifth_grade


# SEPTEMBER

floydala_abd_es_september <- floydala_abd_es_september %>%
  str_squish() %>%
  strsplit(split = " ")

# fill missing values
lapply(floydala_abd_es_september, length)
floydala_abd_es_september[[5]] <- c('05', 'NA', 'NA', 'NA', 'NA',
                                    'NA', 'NA', 'NA', 'NA', 'NA',
                                    'NA', 'NA', 'NA', 'NA', 'NA')
floydala_abd_es_september[[6]] <- c('06', 'NA', 'NA', 'NA', 'NA',
                                    'NA', 'NA', 'NA', 'NA', 'NA',
                                    'NA', 'NA', 'NA', 'NA', 'NA')
floydala_abd_es_september[[7]] <- c('07', 'NA', 'NA', 'NA', 'NA',
                                    'NA', 'NA', 'NA', 'NA', 'NA',
                                    'NA', 'NA', 'NA', 'NA', 'NA')
floydala_abd_es_september[[12]] <- c('12', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA', 'NA', 'NA', 'NA')
floydala_abd_es_september[[13]] <- c('13', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA', 'NA', 'NA', 'NA')
floydala_abd_es_september[[19]] <- c('19', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA', 'NA', 'NA', 'NA')
floydala_abd_es_september[[20]] <- c('20', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA', 'NA', 'NA', 'NA')


# Create Dataframe

# create dataframe
floydala_abd_es_september <-   plyr::ldply(floydala_abd_es_september)
floydala_abd_es_september <- floydala_abd_es_september[1:13]
colnames(floydala_abd_es_september) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                         'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem')
floydala_abd_es_september[1] <- seq(as.Date("2020/09/01"), by = "day", length.out = 25)
floydala_abd_es_september



# Separate by grade level

# Kinder

floydala_abd_es_september_kinder <- floydala_abd_es_september[1:3]
floydala_abd_es_september_kinder <- as_tibble(floydala_abd_es_september_kinder)
floydala_abd_es_september_kinder$Abs <- as.numeric(floydala_abd_es_september_kinder$Abs)
floydala_abd_es_september_kinder$Mem <- as.numeric(floydala_abd_es_september_kinder$Mem)

floydala_abd_es_september_kinder

# First Grade
floydala_abd_es_september_first_grade <- floydala_abd_es_september[c(1,4,5)]
floydala_abd_es_september_first_grade <- as_tibble(floydala_abd_es_september_first_grade)
floydala_abd_es_september_first_grade$Abs <- as.numeric(floydala_abd_es_september_first_grade$Abs) 
floydala_abd_es_september_first_grade$Mem <- as.numeric(floydala_abd_es_september_first_grade$Mem)

floydala_abd_es_september_first_grade


# Second Grade
floydala_abd_es_september_second_grade <- floydala_abd_es_september[c(1,6,7)]
floydala_abd_es_september_second_grade <- as_tibble(floydala_abd_es_september_second_grade)
floydala_abd_es_september_second_grade$Abs <- as.numeric(floydala_abd_es_september_second_grade$Abs)
floydala_abd_es_september_second_grade$Mem <- as.numeric(floydala_abd_es_september_second_grade$Mem)

floydala_abd_es_september_second_grade

# Third Grade
floydala_abd_es_september_third_grade <- floydala_abd_es_september[c(1,8,9)] 
floydala_abd_es_september_third_grade <- as_tibble(floydala_abd_es_september_third_grade)
floydala_abd_es_september_third_grade$Abs <- as.numeric(floydala_abd_es_september_third_grade$Abs)
floydala_abd_es_september_third_grade$Mem <- as.numeric(floydala_abd_es_september_third_grade$Mem)

floydala_abd_es_september_third_grade

# Fourth Grade
floydala_abd_es_september_fourth_grade <- floydala_abd_es_september[c(1,10,11)]
floydala_abd_es_september_fourth_grade <- as_tibble(floydala_abd_es_september_fourth_grade)
floydala_abd_es_september_fourth_grade$Abs <- as.numeric(floydala_abd_es_september_fourth_grade$Abs)
floydala_abd_es_september_fourth_grade$Mem <- as.numeric(floydala_abd_es_september_fourth_grade$Mem)

floydala_abd_es_september_fourth_grade

# Fifth Grade
floydala_abd_es_september_fifth_grade <- floydala_abd_es_september[c(1,12,13)]
floydala_abd_es_september_fifth_grade <- as_tibble(floydala_abd_es_september_fifth_grade)
floydala_abd_es_september_fifth_grade$Abs <- as.numeric(floydala_abd_es_september_fifth_grade$Abs)
floydala_abd_es_september_fifth_grade$Mem <- as.numeric(floydala_abd_es_september_fifth_grade$Mem)

floydala_abd_es_september_fifth_grade

######################################################################################################

# FLOYDALA PDF #4


# Floydala Elementary School Second Cycle PDF 
floydala_abd_2 <- pdf_text(".\\floydala_abd_second_six_weeks.pdf") %>%
  readr::read_lines()

# The PDF was split into september, october, and november
floydala_abd_es_september2 <- floydala_abd_2[-c(1:7, 10:73)] %>%
  str_squish() %>%
  strsplit(split = " ")

floydala_abd_es_september2

# create dataframe
floydala_abd_es_september2 <-   plyr::ldply(floydala_abd_es_september2)
floydala_abd_es_september2 <- floydala_abd_es_september2[1:13]
colnames(floydala_abd_es_september2) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                          'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem')
floydala_abd_es_september2[1] <- seq(as.Date("2020/09/29"), by = "day", length.out = 2)

floydala_abd_es_september2

# Separate by grade level

# Kinder

floydala_abd_es_september2_kinder <- floydala_abd_es_september2[1:3]
floydala_abd_es_september2_kinder <- as_tibble(floydala_abd_es_september2_kinder)
floydala_abd_es_september2_kinder$Abs <- as.numeric(floydala_abd_es_september2_kinder$Abs)
floydala_abd_es_september2_kinder$Mem <- as.numeric(floydala_abd_es_september2_kinder$Mem)

floydala_abd_es_september2_kinder

# First Grade
floydala_abd_es_september2_first_grade <- floydala_abd_es_september2[c(1,4,5)]
floydala_abd_es_september2_first_grade <- as_tibble(floydala_abd_es_september2_first_grade)
floydala_abd_es_september2_first_grade$Abs <- as.numeric(floydala_abd_es_september2_first_grade$Abs) 
floydala_abd_es_september2_first_grade$Mem <- as.numeric(floydala_abd_es_september2_first_grade$Mem)

floydala_abd_es_september2_first_grade


# Second Grade
floydala_abd_es_september2_second_grade <- floydala_abd_es_september2[c(1,6,7)]
floydala_abd_es_september2_second_grade <- as_tibble(floydala_abd_es_september2_second_grade)
floydala_abd_es_september2_second_grade$Abs <- as.numeric(floydala_abd_es_september2_second_grade$Abs)
floydala_abd_es_september2_second_grade$Mem <- as.numeric(floydala_abd_es_september2_second_grade$Mem)

floydala_abd_es_september2_second_grade

# Third Grade
floydala_abd_es_september2_third_grade <- floydala_abd_es_september2[c(1,8,9)] 
floydala_abd_es_september2_third_grade <- as_tibble(floydala_abd_es_september2_third_grade)
floydala_abd_es_september2_third_grade$Abs <- as.numeric(floydala_abd_es_september2_third_grade$Abs)
floydala_abd_es_september2_third_grade$Mem <- as.numeric(floydala_abd_es_september2_third_grade$Mem)

floydala_abd_es_september2_third_grade

# Fourth Grade
floydala_abd_es_september2_fourth_grade <- floydala_abd_es_september2[c(1,10,11)]
floydala_abd_es_september2_fourth_grade <- as_tibble(floydala_abd_es_september2_fourth_grade)
floydala_abd_es_september2_fourth_grade$Abs <- as.numeric(floydala_abd_es_september2_fourth_grade$Abs)
floydala_abd_es_september2_fourth_grade$Mem <- as.numeric(floydala_abd_es_september2_fourth_grade$Mem)

floydala_abd_es_september2_fourth_grade

# Fifth Grade
floydala_abd_es_september2_fifth_grade <- floydala_abd_es_september2[c(1,12,13)]
floydala_abd_es_september2_fifth_grade <- as_tibble(floydala_abd_es_september2_fifth_grade)
floydala_abd_es_september2_fifth_grade$Abs <- as.numeric(floydala_abd_es_september2_fifth_grade$Abs)
floydala_abd_es_september2_fifth_grade$Mem <- as.numeric(floydala_abd_es_september2_fifth_grade$Mem)

floydala_abd_es_september2_fifth_grade


# Bind both septembers for each grade level together

# Kinder
floydala_abd_es_september_kinder <- rbind(floydala_abd_es_september_kinder,
                                          floydala_abd_es_september2_kinder)
rm(floydala_abd_es_september2_kinder)


# First Grade
floydala_abd_es_september_first_grade <- rbind(floydala_abd_es_september_first_grade,
                                               floydala_abd_es_september2_first_grade)

rm(floydala_abd_es_september2_first_grade)

# Second Grade
floydala_abd_es_september_second_grade <- rbind(floydala_abd_es_september_second_grade,
                                                floydala_abd_es_september2_second_grade)

rm(floydala_abd_es_september2_second_grade)

# Third Grade
floydala_abd_es_september_third_grade <- rbind(floydala_abd_es_september_third_grade,
                                               floydala_abd_es_september2_third_grade)

rm(floydala_abd_es_september2_third_grade)


# Fourth Grade
floydala_abd_es_september_fourth_grade <- rbind(floydala_abd_es_september_fourth_grade,
                                                floydala_abd_es_september2_fourth_grade)
rm(floydala_abd_es_september2_fourth_grade)


# Fifth Grade
floydala_abd_es_september_fifth_grade <- rbind(floydala_abd_es_september_fifth_grade,
                                               floydala_abd_es_september2_fifth_grade)

rm(floydala_abd_es_september2_fifth_grade)



# OCTOBER

floydala_abd_es_october <- floydala_abd_2[-c(1:18, 49:56, 58:73)] %>%
  str_squish() %>%
  strsplit(split = " ")

floydala_abd_es_october

# fill missing values
lapply(floydala_abd_es_october, length)
floydala_abd_es_october[[3]] <- c('3', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA', 'NA', 'NA', 'NA')
floydala_abd_es_october[[4]] <- c('4', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA', 'NA', 'NA', 'NA')
floydala_abd_es_october[[10]] <- c('10', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA')
floydala_abd_es_october[[11]] <- c('11', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA')
floydala_abd_es_october[[12]] <- c('12', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA')
floydala_abd_es_october[[17]] <- c('17', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA')
floydala_abd_es_october[[18]] <- c('18', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA')
floydala_abd_es_october[[24]] <- c('24', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA')
floydala_abd_es_october[[25]] <- c('25', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA')
floydala_abd_es_october[[31]] <- c('31', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA')

# create dataframe
floydala_abd_es_october <-   plyr::ldply(floydala_abd_es_october)
floydala_abd_es_october <- floydala_abd_es_october[1:13]
colnames(floydala_abd_es_october) <-  c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                        'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem')
floydala_abd_es_october[1] <- seq(as.Date("2020/10/01"), by = "day", length.out = 31)

# Separate by grade level

# Kinder

floydala_abd_es_october_kinder <- floydala_abd_es_october[1:3]
floydala_abd_es_october_kinder <- as_tibble(floydala_abd_es_october_kinder)
floydala_abd_es_october_kinder$Abs <- as.numeric(floydala_abd_es_october_kinder$Abs)
floydala_abd_es_october_kinder$Mem <- as.numeric(floydala_abd_es_october_kinder$Mem)

floydala_abd_es_october_kinder

# First Grade
floydala_abd_es_october_first_grade <- floydala_abd_es_october[c(1,4,5)]
floydala_abd_es_october_first_grade <- as_tibble(floydala_abd_es_october_first_grade)
floydala_abd_es_october_first_grade$Abs <- as.numeric(floydala_abd_es_october_first_grade$Abs) 
floydala_abd_es_october_first_grade$Mem <- as.numeric(floydala_abd_es_october_first_grade$Mem)

floydala_abd_es_october_first_grade


# Second Grade
floydala_abd_es_october_second_grade <- floydala_abd_es_october[c(1,6,7)]
floydala_abd_es_october_second_grade <- as_tibble(floydala_abd_es_october_second_grade)
floydala_abd_es_october_second_grade$Abs <- as.numeric(floydala_abd_es_october_second_grade$Abs)
floydala_abd_es_october_second_grade$Mem <- as.numeric(floydala_abd_es_october_second_grade$Mem)

floydala_abd_es_october_second_grade

# Third Grade
floydala_abd_es_october_third_grade <- floydala_abd_es_october[c(1,8,9)] 
floydala_abd_es_october_third_grade <- as_tibble(floydala_abd_es_october_third_grade)
floydala_abd_es_october_third_grade$Abs <- as.numeric(floydala_abd_es_october_third_grade$Abs)
floydala_abd_es_october_third_grade$Mem <- as.numeric(floydala_abd_es_october_third_grade$Mem)

floydala_abd_es_october_third_grade

# Fourth Grade
floydala_abd_es_october_fourth_grade <- floydala_abd_es_october[c(1,10,11)]
floydala_abd_es_october_fourth_grade <- as_tibble(floydala_abd_es_october_fourth_grade)
floydala_abd_es_october_fourth_grade$Abs <- as.numeric(floydala_abd_es_october_fourth_grade$Abs)
floydala_abd_es_october_fourth_grade$Mem <- as.numeric(floydala_abd_es_october_fourth_grade$Mem)

floydala_abd_es_october_fourth_grade

# Fifth Grade
floydala_abd_es_october_fifth_grade <- floydala_abd_es_october[c(1,12,13)]
floydala_abd_es_october_fifth_grade <- as_tibble(floydala_abd_es_october_fifth_grade)
floydala_abd_es_october_fifth_grade$Abs <- as.numeric(floydala_abd_es_october_fifth_grade$Abs)
floydala_abd_es_october_fifth_grade$Mem <- as.numeric(floydala_abd_es_october_fifth_grade$Mem)

floydala_abd_es_october_fifth_grade


# NOVEMBER

floydala_abd_es_november <- floydala_abd_2[-c(1:66,73)] %>%
  str_squish() %>%
  strsplit(split = " ")

# fill missing values
lapply(floydala_abd_es_november, length)
floydala_abd_es_november[[1]] <- c('1', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA')

# create dataframe
floydala_abd_es_november <-   plyr::ldply(floydala_abd_es_november)
floydala_abd_es_november <- floydala_abd_es_november[1:13]
colnames(floydala_abd_es_november) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                        'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem')
floydala_abd_es_november[1] <- seq(as.Date("2020/11/01"), by = "day", length.out = 6)

floydala_abd_es_november


# Seperate by grade level

# Kinder
floydala_abd_es_november_kinder <- floydala_abd_es_november[1:3]
floydala_abd_es_november_kinder <- as_tibble(floydala_abd_es_november_kinder)
floydala_abd_es_november_kinder$Abs <- as.numeric(floydala_abd_es_november_kinder$Abs)
floydala_abd_es_november_kinder$Mem <- as.numeric(floydala_abd_es_november_kinder$Mem)

floydala_abd_es_november_kinder

# First Grade
floydala_abd_es_november_first_grade <- floydala_abd_es_november[c(1,4,5)]
floydala_abd_es_november_first_grade <- as_tibble(floydala_abd_es_november_first_grade)
floydala_abd_es_november_first_grade$Abs <- as.numeric(floydala_abd_es_november_first_grade$Abs) 
floydala_abd_es_november_first_grade$Mem <- as.numeric(floydala_abd_es_november_first_grade$Mem)

floydala_abd_es_november_first_grade


# Second Grade
floydala_abd_es_november_second_grade <- floydala_abd_es_november[c(1,6,7)]
floydala_abd_es_november_second_grade <- as_tibble(floydala_abd_es_november_second_grade)
floydala_abd_es_november_second_grade$Abs <- as.numeric(floydala_abd_es_november_second_grade$Abs)
floydala_abd_es_november_second_grade$Mem <- as.numeric(floydala_abd_es_november_second_grade$Mem)

floydala_abd_es_november_second_grade

# Third Grade
floydala_abd_es_november_third_grade <- floydala_abd_es_november[c(1,8,9)] 
floydala_abd_es_november_third_grade <- as_tibble(floydala_abd_es_november_third_grade)
floydala_abd_es_november_third_grade$Abs <- as.numeric(floydala_abd_es_november_third_grade$Abs)
floydala_abd_es_november_third_grade$Mem <- as.numeric(floydala_abd_es_november_third_grade$Mem)

floydala_abd_es_november_third_grade

# Fourth Grade
floydala_abd_es_november_fourth_grade <- floydala_abd_es_november[c(1,10,11)]
floydala_abd_es_november_fourth_grade <- as_tibble(floydala_abd_es_november_fourth_grade)
floydala_abd_es_november_fourth_grade$Abs <- as.numeric(floydala_abd_es_november_fourth_grade$Abs)
floydala_abd_es_november_fourth_grade$Mem <- as.numeric(floydala_abd_es_november_fourth_grade$Mem)

floydala_abd_es_november_fourth_grade

# Fifth Grade
floydala_abd_es_november_fifth_grade <- floydala_abd_es_november[c(1,12,13)]
floydala_abd_es_november_fifth_grade <- as_tibble(floydala_abd_es_november_fifth_grade)
floydala_abd_es_november_fifth_grade$Abs <- as.numeric(floydala_abd_es_november_fifth_grade$Abs)
floydala_abd_es_november_fifth_grade$Mem <- as.numeric(floydala_abd_es_november_fifth_grade$Mem)

floydala_abd_es_november_fifth_grade

###############################################################################

# BIND ALL ELEMENTARY SCHOOL DATASETS BY GRADE LEVEL

# Kinder

floydala_abd_es_kinder_grade_all_dates <- rbind(floydala_abd_es_august_kinder,
                                                floydala_abd_es_september_kinder,
                                                floydala_abd_es_october_kinder,
                                                floydala_abd_es_november_kinder)
floydala_abd_es_kinder_grade_all_dates

# First Grade
floydala_abd_es_first_grade_all_dates <- rbind(floydala_abd_es_august_first_grade,
                                               floydala_abd_es_september_first_grade,
                                               floydala_abd_es_october_first_grade,
                                               floydala_abd_es_november_first_grade)
floydala_abd_es_first_grade_all_dates

# Second Grade
floydala_abd_es_second_grade_all_dates <- rbind(floydala_abd_es_august_second_grade,
                                                floydala_abd_es_september_second_grade,
                                                floydala_abd_es_october_second_grade,
                                                floydala_abd_es_november_second_grade)
floydala_abd_es_second_grade_all_dates

# Third Grade
floydala_abd_es_third_grade_all_dates <- rbind(floydala_abd_es_august_third_grade,
                                               floydala_abd_es_september_third_grade,
                                               floydala_abd_es_october_third_grade,
                                               floydala_abd_es_november_third_grade)
floydala_abd_es_third_grade_all_dates

# Fourth Grade
floydala_abd_es_fourth_grade_all_dates <- rbind(floydala_abd_es_august_fourth_grade,
                                                floydala_abd_es_september_fourth_grade,
                                                floydala_abd_es_october_fourth_grade,
                                                floydala_abd_es_november_fourth_grade)
floydala_abd_es_fourth_grade_all_dates



# Fifth Grade
floydala_abd_es_fifth_grade_all_dates <- rbind(floydala_abd_es_august_fifth_grade,
                                               floydala_abd_es_september_fifth_grade,
                                               floydala_abd_es_october_fifth_grade,
                                               floydala_abd_es_november_fifth_grade)
floydala_abd_es_fifth_grade_all_dates

###############################################################################

# FLOYDALA PDF #5


# Floydala PreK PDF 
floydala_prek <- pdf_text(".\\floydala_abd_prek_first_six_weeks.pdf") %>%
  readr::read_lines()

# The PDF was split into august and september
floydala_prek_august <- floydala_prek[-c(1:7, 23:57)] %>%
  str_squish() %>%
  strsplit(split = " ")

# fill missing values
lapply(floydala_prek_august, length)
floydala_prek_august[[6]] <- c('22', 'NA', 'NA', 'NA', 'NA')
floydala_prek_august[[7]] <- c('23', 'NA', 'NA', 'NA', 'NA')
floydala_prek_august[[13]] <- c('29', 'NA', 'NA', 'NA', 'NA')
floydala_prek_august[[14]] <- c('30', 'NA', 'NA', 'NA', 'NA')

# create dataframe
floydala_prek_august <-   plyr::ldply(floydala_prek_august)
floydala_prek_august <- floydala_prek_august[1:3]
colnames(floydala_prek_august) <- c('Day', 'Abs', 'Mem')
floydala_prek_august <- as_tibble(floydala_prek_august)
floydala_prek_august[1] <- seq(as.Date("2020/08/17"), by = "day", length.out = 15)
floydala_prek_august$Abs <- as.numeric(floydala_prek_august$Abs)
floydala_prek_august$Mem <- as.numeric(floydala_prek_august$Mem)

floydala_prek_august

# Same file, page 2
floydala_prek_1_september <- floydala_prek[-c(1:31, 57)]

floydala_prek_1_september <- floydala_prek_1_september %>%
  str_squish() %>%
  strsplit(split = " ")

# fill missing values
lapply(floydala_prek_1_september, length)
floydala_prek_1_september[[5]] <- c('05', 'NA', 'NA', 'NA', 'NA')
floydala_prek_1_september[[6]] <- c('06', 'NA', 'NA', 'NA', 'NA')
floydala_prek_1_september[[7]] <- c('07', 'NA', 'NA', 'NA', 'NA')
floydala_prek_1_september[[12]] <- c('12', 'NA', 'NA', 'NA', 'NA')
floydala_prek_1_september[[13]] <- c('13', 'NA', 'NA', 'NA', 'NA')
floydala_prek_1_september[[19]] <- c('19', 'NA', 'NA', 'NA', 'NA')
floydala_prek_1_september[[20]] <- c('20', 'NA', 'NA', 'NA', 'NA')

# create dataframe
floydala_prek_1_september <-   plyr::ldply(floydala_prek_1_september)
floydala_prek_1_september <- floydala_prek_1_september[1:3]
colnames(floydala_prek_1_september) <- c('Day', 'Abs', 'Mem')
floydala_prek_1_september <- as_tibble(floydala_prek_1_september)
floydala_prek_1_september[1] <- seq(as.Date("2020/09/01"), by = "day", length.out = 25)
floydala_prek_1_september$Abs <- as.numeric(floydala_prek_1_september$Abs)
floydala_prek_1_september$Mem <- as.numeric(floydala_prek_1_september$Mem)

floydala_prek_1_september

######################################################################################


# FLOYDALA PDF #2


# Floydala Grade Early Education Second Cycle PDF 
floydala_prek_2 <- pdf_text(".\\floydala_abd_prek_second_six_weeks.pdf") %>%
  readr::read_lines()

# The PDF was split into september, october, and november
floydala_prek_2_september <- floydala_prek_2[-c(1:7, 10:73)] %>%
  str_squish() %>%
  strsplit(split = " ")

# create dataframe
floydala_prek_2_september <-   plyr::ldply(floydala_prek_2_september)
floydala_prek_2_september <- floydala_prek_2_september[1:3]
colnames(floydala_prek_2_september) <- c('Day', 'Abs', 'Mem')
floydala_prek_2_september <- as_tibble(floydala_prek_2_september)
floydala_prek_2_september[1] <- seq(as.Date("2020/09/29"), by = "day", length.out = 2)
floydala_prek_2_september$Abs <- as.numeric(floydala_prek_2_september$Abs)
floydala_prek_2_september$Mem <- as.numeric(floydala_prek_2_september$Mem)


# Bind both septembers together
floydala_prek_september <- rbind(floydala_prek_1_september, floydala_prek_2_september)

floydala_prek_september
rm(floydala_prek_1_september, floydala_prek_2_september)



# OCTOBER
floydala_prek_october <- floydala_prek_2[-c(1:18, 49:56, 58:73)] %>%
  str_squish() %>%
  strsplit(split = " ")

# fill missing values
lapply(floydala_prek_october, length)
floydala_prek_october[[3]] <- c('3', 'NA', 'NA', 'NA', 'NA')
floydala_prek_october[[4]] <- c('4', 'NA', 'NA', 'NA', 'NA')
floydala_prek_october[[10]] <- c('10', 'NA', 'NA', 'NA', 'NA')
floydala_prek_october[[11]] <- c('11', 'NA', 'NA', 'NA', 'NA')
floydala_prek_october[[12]] <- c('12', 'NA', 'NA', 'NA', 'NA')
floydala_prek_october[[17]] <- c('17', 'NA', 'NA', 'NA', 'NA')
floydala_prek_october[[18]] <- c('18', 'NA', 'NA', 'NA', 'NA')
floydala_prek_october[[24]] <- c('24', 'NA', 'NA', 'NA', 'NA')
floydala_prek_october[[25]] <- c('25', 'NA', 'NA', 'NA', 'NA')
floydala_prek_october[[31]] <- c('31', 'NA', 'NA', 'NA', 'NA')

# create dataframe
floydala_prek_october <-   plyr::ldply(floydala_prek_october)
floydala_prek_october <- floydala_prek_october[1:3]
colnames(floydala_prek_october) <- c('Day', 'Abs', 'Mem')
floydala_prek_october <- as_tibble(floydala_prek_october)
floydala_prek_october[1] <- seq(as.Date("2020/10/01"), by = "day", length.out = 31)
floydala_prek_october$Abs <- as.numeric(floydala_prek_october$Abs)
floydala_prek_october$Mem <- as.numeric(floydala_prek_october$Mem)


floydala_prek_october



# NOVEMBER
floydala_prek_november <- floydala_early_ed_2[-c(1:66,73)] %>%
  str_squish() %>%
  strsplit(split = " ")

# fill missing values
lapply(floydala_prek_november, length)
floydala_prek_november[[1]] <- c('1', 'NA', 'NA', 'NA', 'NA')

# create dataframe
floydala_prek_november <-   plyr::ldply(floydala_prek_november)
floydala_prek_november <- floydala_prek_november[1:3]
colnames(floydala_prek_november) <- c('Day', 'Abs', 'Mem')
floydala_prek_november <- as_tibble(floydala_prek_november)
floydala_prek_november[1] <- seq(as.Date("2020/11/01"), by = "day", length.out = 6)
floydala_prek_november$Abs <- as.numeric(floydala_prek_november$Abs)
floydala_prek_november$Mem <- as.numeric(floydala_prek_november$Mem)

floydala_prek_november


###############################################################################

# BIND ALL PREK DATASETS

floydala_prek_all_dates <- rbind(floydala_prek_august,
                                 floydala_prek_september,
                                 floydala_prek_october,
                                 floydala_prek_november)

###############################################################################


# FLOYDALA PDF #6


# Floydala daep PDF 
floydala_daep <- pdf_text(".\\floydala_daep_first_six_weeks.pdf") %>%
  readr::read_lines()


# The PDF was split into august and september
floydala_daep <- floydala_daep[-c(1:7, 23:31, 57)] %>%
  str_squish() %>%
  strsplit(split = " ")

floydala_daep_august_10th_grade <-  floydala_daep[8:15]
floydala_daep_september1 <- floydala_daep[16:36]
floydala_daep_september2 <- floydala_daep[37:40]
#### FLOYDALA_DAEP_AUGUST_10TH GRADE

# fill missing values

lapply(floydala_daep_august_10th_grade, length)
floydala_daep_august_10th_grade[[6]] <- c('29', 'NA', 'NA', 'NA', 'NA')
floydala_daep_august_10th_grade[[7]] <- c('30', 'NA', 'NA', 'NA', 'NA')

# create dataframe
floydala_daep_august_10th_grade <-   plyr::ldply(floydala_daep_august_10th_grade)
floydala_daep_august_10th_grade <- floydala_daep_august_10th_grade[1:3]
colnames(floydala_daep_august_10th_grade) <- c('Day', 'Abs', 'Mem')
floydala_daep_august_10th_grade <- as_tibble(floydala_daep_august_10th_grade)
floydala_daep_august_10th_grade[1] <- seq(as.Date("2020/08/24"), by = "day", length.out = 8)
floydala_daep_august_10th_grade$Abs <- as.numeric(floydala_daep_august_10th_grade$Abs)
floydala_daep_august_10th_grade$Mem <- as.numeric(floydala_daep_august_10th_grade$Mem)


floydala_daep_august_10th_grade

# SEPTEMBER
# will split and re bind at the end
floydala_daep_september1

# fill missing values
lapply(floydala_daep_september1, length)
floydala_daep_september1[[5]] <- c('05', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA')
floydala_daep_september1[[6]] <- c('06', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA')
floydala_daep_september1[[7]] <- c('07', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA')
floydala_daep_september1[[12]] <- c('12', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA')
floydala_daep_september1[[13]] <- c('13', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA')
floydala_daep_september1[[19]] <- c('19', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA')
floydala_daep_september1[[20]] <- c('20', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA')

# create dataframe & split into grade levels
floydala_daep_september1 <-   plyr::ldply(floydala_daep_september1)
floydala_daep_september1 <- floydala_daep_september1[1:5]
colnames(floydala_daep_september1) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem')
floydala_early_ed_september <- as_tibble(floydala_early_ed_september)
floydala_daep_september1[1] <- seq(as.Date("2020/09/01"), by = "day", length.out = 21)
floydala_daep_september1$Abs <- as.numeric(floydala_daep_september1$Abs)
floydala_daep_september1$Mem <- as.numeric(floydala_daep_september1$Mem)

floydala_daep_september1

# september part 2

floydala_daep_september2

floydala_daep_september2 <-   plyr::ldply(floydala_daep_september2)
colnames(floydala_daep_september2) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem')
floydala_daep_september2[1] <- seq(as.Date("2020/09/22"), by = "day", length.out = 4)

floydala_daep_september_7th_grade <- floydala_daep_september2[1:3]
floydala_daep_september_10th_grade <- floydala_daep_september2[c(1,4,5)]
floydala_daep_september_11th_grade <- floydala_daep_september2[c(1,6,7)]


floydala_daep_september_7th_grade <- as_tibble(floydala_daep_september_7th_grade)
floydala_daep_september_10th_grade <- as_tibble(floydala_daep_september_10th_grade)
floydala_daep_september_11th_grade <- as_tibble(floydala_daep_september_11th_grade)


floydala_daep_september_7th_grade$Abs <- as.numeric(floydala_daep_september_7th_grade$Abs)
floydala_daep_september_7th_grade$Mem <- as.numeric(floydala_daep_september_7th_grade$Mem)

floydala_daep_september_10th_grade$Abs <- as.numeric(floydala_daep_september_10th_grade$Abs)
floydala_daep_september_10th_grade$Mem <- as.numeric(floydala_daep_september_10th_grade$Mem)

floydala_daep_september_11th_grade$Abs <- as.numeric(floydala_daep_september_11th_grade$Abs)
floydala_daep_september_11th_grade$Mem <- as.numeric(floydala_daep_september_11th_grade$Mem)


# fix the split in septemeber with grades 7 and 10

floydala_daep_september_7th_grade <- rbind(floydala_daep_september1[1:3],
                                           floydala_daep_september_7th_grade)

floydala_daep_september_10th_grade <- rbind(floydala_daep_september1[c(1,4,5)],
                                            floydala_daep_september_10th_grade)

floydala_daep_10th_grade_all_dates <- rbind(floydala_daep_august_10th_grade,
                                            floydala_daep_september_10th_grade)


###############################################################################

# FLOYDALA PDF #8

# Floydala Elementary School Second Cycle PDF 
floydala_daep_2 <- pdf_text(".\\floydala_daep_second_six_weeks.pdf") %>%
  readr::read_lines()

# The PDF was split into september, october, and november
floydala_daep_september2 <- floydala_daep_2[-c(1:7, 10:73)] %>%
  str_squish() %>%
  strsplit(split = " ")

floydala_daep_september2

# create dataframe
floydala_daep_september2 <-   plyr::ldply(floydala_daep_september2)
floydala_daep_september2 <- floydala_daep_september2[1:7]
colnames(floydala_daep_september2) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem')
floydala_daep_september2[1] <- seq(as.Date("2020/09/29"), by = "day", length.out = 2)

floydala_daep_september2

# Separate by grade level

# Grade 7

floydala_daep_september2_7th_grade <- floydala_daep_september2[1:3]
floydala_daep_september2_7th_grade <- as_tibble(floydala_daep_september2_7th_grade)
floydala_daep_september2_7th_grade$Abs <- as.numeric(floydala_daep_september2_7th_grade$Abs)
floydala_daep_september2_7th_grade$Mem <- as.numeric(floydala_daep_september2_7th_grade$Mem)

floydala_daep_september2_7th_grade

# Grade 10
floydala_daep_september2_10th_grade <- floydala_daep_september2[c(1,4,5)]
floydala_daep_september2_10th_grade <- as_tibble(floydala_daep_september2_10th_grade)
floydala_daep_september2_10th_grade$Abs <- as.numeric(floydala_daep_september2_10th_grade$Abs) 
floydala_daep_september2_10th_grade$Mem <- as.numeric(floydala_daep_september2_10th_grade$Mem)

floydala_daep_september2_10th_grade


# Grade 11
floydala_daep_september2_11th_grade <- floydala_daep_september2[c(1,6,7)]
floydala_daep_september2_11th_grade <- as_tibble(floydala_daep_september2_11th_grade)
floydala_daep_september2_11th_grade$Abs <- as.numeric(floydala_daep_september2_11th_grade$Abs)
floydala_daep_september2_11th_grade$Mem <- as.numeric(floydala_daep_september2_11th_grade$Mem)

floydala_daep_september2_11th_grade

# Bind both septembers for each grade level together

# Grade 7
floydala_daep_7th_grade_all_dates <- rbind(floydala_daep_september_7th_grade,
                                           floydala_daep_september2_7th_grade)
rm(floydala_daep_september2_7th_grade)


# Grade 10
floydala_daep_10th_grade_all_dates <- rbind(floydala_daep_10th_grade_all_dates,
                                            floydala_daep_september2_10th_grade)


rm(floydala_daep_september2_10th_grade)

# Grade 11
floydala_daep_september_11th_grade <- rbind(floydala_daep_september_11th_grade,
                                            floydala_daep_september2_11th_grade)

rm(floydala_daep_september2_11th_grade)


# OCTOBER

floydala_daep_october <- floydala_daep_2[-c(1:18, 48:73)] %>%
  str_squish() %>%
  strsplit(split = " ")

floydala_daep_october2 <- floydala_daep_2[48] %>%
  str_squish() %>%
  strsplit(split = " ")

floydala_daep_october

# fill missing values
lapply(floydala_daep_october, length)

floydala_daep_october[[1]] <- c('1', 'NA', 'NA', '0.0', '1.0',
                                '0.0', '2.0', '0.0', '1.0', '0.0',
                                '4.0')
floydala_daep_october[[2]] <- c('2', 'NA', 'NA', '0.0', '1.0',
                                '0.0', '2.0', '0.0', '1.0', '0.0',
                                '4.0')
floydala_daep_october[[3]] <- c('3', 'NA', 'NA', 'NA', 'NA',
                                'NA', 'NA', 'NA', 'NA', 'NA',
                                'NA')
floydala_daep_october[[4]] <- c('4', 'NA', 'NA', 'NA', 'NA',
                                'NA', 'NA', 'NA', 'NA', 'NA',
                                'NA')
floydala_daep_october[[10]] <- c('10', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA', 'NA',
                                 'NA')
floydala_daep_october[[11]] <- c('11', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA', 'NA',
                                 'NA')
floydala_daep_october[[12]] <- c('12', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA', 'NA',
                                 'NA')
floydala_daep_october[[17]] <- c('17', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA', 'NA',
                                 'NA')
floydala_daep_october[[18]] <- c('18', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA', 'NA',
                                 'NA')
floydala_daep_october[[21]] <- c('21', '0.0', '1.0', '0.0', '2.0', 
                                 '0.0', '1.0', 'NA', 'NA', '0.0',
                                 '4.0')
floydala_daep_october[[22]] <- c('22', '0.0', '1.0', '2.0', '2.0', 
                                 '1.0', '1.0', 'NA', 'NA', '3.0',
                                 '4.0')
floydala_daep_october[[23]] <- c('23', '1.0', '1.0', '2.0', '2.0', 
                                 '1.0', '1.0', 'NA', 'NA', '4.0',
                                 '4.0')
floydala_daep_october[[24]] <- c('24', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA', 'NA',
                                 'NA')
floydala_daep_october[[25]] <- c('25', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA', 'NA',
                                 'NA')
floydala_daep_october[[26]] <- c('26', '0.0', '1.0', '1.0', '2.0', 
                                 '1.0', '1.0', 'NA', 'NA', '2.0',
                                 '4.0')
floydala_daep_october[[27]] <- c('27', '0.0', '1.0', '1.0', '2.0', 
                                 '0.0', '1.0', 'NA', 'NA', '0.0',
                                 '4.0')
floydala_daep_october[[28]] <- c('28', '0.0', '1.0', '0.0', '2.0', 
                                 '0.0', '1.0', 'NA', 'NA', '0.0',
                                 '4.0')
floydala_daep_october[[29]] <- c('29', '0.0', '1.0', '0.0', '2.0', 
                                 '0.0', '1.0', 'NA', 'NA', '0.0',
                                 '4.0')

# create dataframe
floydala_daep_october <-   plyr::ldply(floydala_daep_october)
floydala_daep_october <- floydala_daep_october[1:9]
colnames(floydala_daep_october) <-  c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                      'Abs', 'Mem')
floydala_daep_october[1] <- seq(as.Date("2020/10/01"), by = "day", length.out = 29)


floydala_daep_october2 <-   plyr::ldply(floydala_daep_october2)
floydala_daep_october2 <- floydala_daep_october2[1:11]
colnames(floydala_daep_october2) <-  c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                       'Abs', 'Mem','Abs', 'Mem')
floydala_daep_october2[1] <- seq(as.Date("2020/10/30"), by = "day", length.out = 1)
floydala_daep_october2

# Separate by grade level

# Grade 5
floydala_daep_october_5th_grade <- floydala_daep_october2[1:3]
floydala_daep_october_5th_grade <- as_tibble(floydala_daep_october_5th_grade)
floydala_daep_october_5th_grade$Abs <- as.numeric(floydala_daep_october_5th_grade$Abs)
floydala_daep_october_5th_grade$Mem <- as.numeric(floydala_daep_october_5th_grade$Mem)

floydala_daep_october_5th_grade

# 6th Grade
floydala_daep_october_6th_grade <- rbind(floydala_daep_october[1:3],
                                         floydala_daep_october2[c(1,4,5)])
floydala_daep_october_6th_grade <- as_tibble(floydala_daep_october_6th_grade)
floydala_daep_october_6th_grade$Abs <- as.numeric(floydala_daep_october_6th_grade$Abs) 
floydala_daep_october_6th_grade$Mem <- as.numeric(floydala_daep_october_6th_grade$Mem)

floydala_daep_october_6th_grade


# 7th Grade
floydala_daep_october_7th_grade <- rbind(floydala_daep_october[c(1,4,5)],
                                         floydala_daep_october2[c(1,6,7)])
floydala_daep_october_7th_grade <- as_tibble(floydala_daep_october_7th_grade)
floydala_daep_october_7th_grade$Abs <- as.numeric(floydala_daep_october_7th_grade$Abs)
floydala_daep_october_7th_grade$Mem <- as.numeric(floydala_daep_october_7th_grade$Mem)

floydala_daep_october_7th_grade

# 10th Grade
floydala_daep_october_10th_grade <- rbind(floydala_daep_october[c(1,6,7)],
                                          floydala_daep_october2[c(1,8,9)])
floydala_daep_october_10th_grade <- as_tibble(floydala_daep_october_10th_grade)
floydala_daep_october_10th_grade$Abs <- as.numeric(floydala_daep_october_10th_grade$Abs)
floydala_daep_october_10th_grade$Mem <- as.numeric(floydala_daep_october_10th_grade$Mem)

floydala_daep_october_10th_grade

# 11th Grade
floydala_daep_october_11th_grade <- rbind(floydala_daep_october[c(1,8,9)],
                                          floydala_daep_october2[c(1,10,11)])
floydala_daep_october_11th_grade <- as_tibble(floydala_daep_october_11th_grade)
floydala_daep_october_11th_grade$Abs <- as.numeric(floydala_daep_october_11th_grade$Abs)
floydala_daep_october_11th_grade$Mem <- as.numeric(floydala_daep_october_11th_grade$Mem)

floydala_daep_october_11th_grade

# NOVEMBER

floydala_daep_november <- floydala_daep_2[-c(1:67,73)] %>%
  str_squish() %>%
  strsplit(split = " ")


# create dataframe
floydala_daep_november <-   plyr::ldply(floydala_daep_november)
floydala_daep_november <- floydala_daep_november[1:13]
colnames(floydala_daep_november) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                      'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem')
floydala_daep_november[1] <- seq(as.Date("2020/11/02"), by = "day", length.out = 5)

floydala_daep_november


# Seperate by grade level

# 5th Grade
floydala_daep_november_5th_grade <- floydala_daep_november[1:3]
floydala_daep_november_5th_grade <- as_tibble(floydala_daep_november_5th_grade)
floydala_daep_november_5th_grade$Abs <- as.numeric(floydala_daep_november_5th_grade$Abs)
floydala_daep_november_5th_grade$Mem <- as.numeric(floydala_daep_november_5th_grade$Mem)

floydala_daep_november_5th_grade

# 6th Grade
floydala_daep_november_6th_grade <- floydala_daep_november[c(1,4,5)]
floydala_daep_november_6th_grade <- as_tibble(floydala_daep_november_6th_grade)
floydala_daep_november_6th_grade$Abs <- as.numeric(floydala_daep_november_6th_grade$Abs) 
floydala_daep_november_6th_grade$Mem <- as.numeric(floydala_daep_november_6th_grade$Mem)

floydala_daep_november_6th_grade


# 7th Grade
floydala_daep_november_7th_grade <- floydala_daep_november[c(1,6,7)]
floydala_daep_november_7th_grade <- as_tibble(floydala_daep_november_7th_grade)
floydala_daep_november_7th_grade$Abs <- as.numeric(floydala_daep_november_7th_grade$Abs)
floydala_daep_november_7th_grade$Mem <- as.numeric(floydala_daep_november_7th_grade$Mem)

floydala_daep_november_7th_grade

# 10th Grade
floydala_daep_november_10th_grade <- floydala_daep_november[c(1,8,9)] 
floydala_daep_november_10th_grade <- as_tibble(floydala_daep_november_10th_grade)
floydala_daep_november_10th_grade$Abs <- as.numeric(floydala_daep_november_10th_grade$Abs)
floydala_daep_november_10th_grade$Mem <- as.numeric(floydala_daep_november_10th_grade$Mem)

floydala_daep_november_10th_grade

# 11th Grade
floydala_daep_november_11th_grade <- floydala_daep_november[c(1,10,11)]
floydala_daep_november_11th_grade <- as_tibble(floydala_daep_november_11th_grade)
floydala_daep_november_11th_grade$Abs <- as.numeric(floydala_daep_november_11th_grade$Abs)
floydala_daep_november_11th_grade$Mem <- as.numeric(floydala_daep_november_11th_grade$Mem)

floydala_daep_november_11th_grade

###########################################################################################

# BIND ALL DATES BY GRADE LEVEL


floydala_daep_5th_grade_all_dates <- rbind(floydala_daep_october_5th_grade,
                                           floydala_daep_november_5th_grade)

floydala_daep_6th_grade_all_dates <- rbind(floydala_daep_october_6th_grade,
                                           floydala_daep_november_6th_grade)


floydala_daep_7th_grade_all_dates <- as_tibble(rbind(floydala_daep_7th_grade_all_dates,
                                                     floydala_daep_october_7th_grade,
                                                     floydala_daep_november_7th_grade))

floydala_daep_10th_grade_all_dates <- rbind(floydala_daep_10th_grade_all_dates,
                                            floydala_daep_october_10th_grade,
                                            floydala_daep_november_10th_grade)

floydala_daep_11th_grade_all_dates <- rbind(floydala_daep_september_11th_grade,
                                            floydala_daep_october_11th_grade,
                                            floydala_daep_november_11th_grade)



#####################################################################################################

# FLOYDALA PDF #9
# FLOYDALA HIGH SCHOOL


# Split into months
floydala_hs_1 <- pdf_text(".\\floydala_high_school_first_six_weeks.pdf") %>%
  readr::read_lines()


floydala_hs_1_august <- floydala_hs_1[-c(1:7, 23:57)]
floydala_hs_1september <- floydala_hs_1[-c(1:31,57)]


# Floydala HS August
floydala_hs_1_august <- floydala_hs_1_august %>%
  str_squish() %>%
  strsplit(split = " ")

floydala_hs_1_august

# fill missing values
lapply(floydala_hs_1_august, length)
floydala_hs_1_august[[6]] <- c('22', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')
floydala_hs_1_august[[7]] <- c('23', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')
floydala_hs_1_august[[13]] <- c('29', 'NA', 'NA', 'NA', 'NA',
                                'NA', 'NA', 'NA', 'NA', 'NA',
                                'NA')
floydala_hs_1_august[[14]] <- c('30', 'NA', 'NA', 'NA', 'NA',
                                'NA', 'NA', 'NA', 'NA', 'NA',
                                'NA')

# create dataframe
floydala_hs_1_august <-   plyr::ldply(floydala_hs_1_august)
floydala_hs_1_august <- floydala_hs_1_august[1:11]
colnames(floydala_hs_1_august) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                    'Abs', 'Mem')
floydala_hs_1_august[1] <- seq(as.Date("2020/08/17"), by = "day", length.out = 15)

# Separate by grade level

# 9th Grade
floydala_hs_august_9th_grade <- floydala_hs_1_august[c(1:3)]
floydala_hs_august_9th_grade <- as_tibble(floydala_hs_august_9th_grade)
floydala_hs_august_9th_grade$Abs <- as.numeric(floydala_hs_august_9th_grade$Abs)
floydala_hs_august_9th_grade$Mem <- as.numeric(floydala_hs_august_9th_grade$Mem)

floydala_hs_august_9th_grade

# 10th Grade
floydala_hs_august_10th_grade <- floydala_hs_1_august[c(1,4,5)]
floydala_hs_august_10th_grade <- as_tibble(floydala_hs_august_10th_grade)
floydala_hs_august_10th_grade$Abs <- as.numeric(floydala_hs_august_10th_grade$Abs) 
floydala_hs_august_10th_grade$Mem <- as.numeric(floydala_hs_august_10th_grade$Mem)

floydala_hs_august_10th_grade


# 11th Grade
floydala_hs_august_11th_grade <- floydala_hs_1_august[c(1,6,7)]
floydala_hs_august_11th_grade <- as_tibble(floydala_hs_august_11th_grade)
floydala_hs_august_11th_grade$Abs <- as.numeric(floydala_hs_august_11th_grade$Abs)
floydala_hs_august_11th_grade$Mem <- as.numeric(floydala_hs_august_11th_grade$Mem)

floydala_hs_august_11th_grade

# 12th Grade
floydala_hs_august_12th_grade <- floydala_hs_1_august[c(1,8,9)] 
floydala_hs_august_12th_grade <- as_tibble(floydala_hs_august_12th_grade)
floydala_hs_august_12th_grade$Abs <- as.numeric(floydala_hs_august_12th_grade$Abs)
floydala_hs_august_12th_grade$Mem <- as.numeric(floydala_hs_august_12th_grade$Mem)

floydala_hs_august_12th_grade


# SEPTEMBER

floydala_hs_september <- floydala_hs_1september %>%
  str_squish() %>%
  strsplit(split = " ")

# fill missing values
lapply(floydala_hs_september, length)
floydala_hs_september[[5]] <- c('05', 'NA', 'NA', 'NA', 'NA',
                                'NA', 'NA', 'NA', 'NA', 'NA',
                                'NA')
floydala_hs_september[[6]] <- c('06', 'NA', 'NA', 'NA', 'NA',
                                'NA', 'NA', 'NA', 'NA', 'NA',
                                'NA')
floydala_hs_september[[7]] <- c('07', 'NA', 'NA', 'NA', 'NA',
                                'NA', 'NA', 'NA', 'NA', 'NA',
                                'NA')
floydala_hs_september[[12]] <- c('12', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA', 'NA',
                                 'NA')
floydala_hs_september[[13]] <- c('13', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA', 'NA',
                                 'NA')
floydala_hs_september[[19]] <- c('19', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA', 'NA',
                                 'NA')
floydala_hs_september[[20]] <- c('20', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA', 'NA',
                                 'NA')


# Create Dataframe

# create dataframe
floydala_hs_september <-   plyr::ldply(floydala_hs_september)
floydala_hs_september <- floydala_hs_september[1:11]
colnames(floydala_hs_september) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                     'Abs', 'Mem', 'Abs', 'Mem')
floydala_hs_september[1] <- seq(as.Date("2020/09/01"), by = "day", length.out = 25)
floydala_hs_september



# Separate by grade level

# 9th Grade

floydala_hs_september_9th_grade <- floydala_hs_september[1:3]
floydala_hs_september_9th_grade <- as_tibble(floydala_hs_september_9th_grade)
floydala_hs_september_9th_grade$Abs <- as.numeric(floydala_hs_september_9th_grade$Abs)
floydala_hs_september_9th_grade$Mem <- as.numeric(floydala_hs_september_9th_grade$Mem)

floydala_hs_september_9th_grade

# 10th Grade
floydala_hs_september_10th_grade <- floydala_hs_september[c(1,4,5)]
floydala_hs_september_10th_grade <- as_tibble(floydala_hs_september_10th_grade)
floydala_hs_september_10th_grade$Abs <- as.numeric(floydala_hs_september_10th_grade$Abs) 
floydala_hs_september_10th_grade$Mem <- as.numeric(floydala_hs_september_10th_grade$Mem)

floydala_hs_september_10th_grade


# 11th Grade
floydala_hs_september_11th_grade <- floydala_hs_september[c(1,6,7)]
floydala_hs_september_11th_grade <- as_tibble(floydala_hs_september_11th_grade)
floydala_hs_september_11th_grade$Abs <- as.numeric(floydala_hs_september_11th_grade$Abs)
floydala_hs_september_11th_grade$Mem <- as.numeric(floydala_hs_september_11th_grade$Mem)

floydala_hs_september_11th_grade

# 12th Grade
floydala_hs_september_12th_grade <- floydala_hs_september[c(1,8,9)] 
floydala_hs_september_12th_grade <- as_tibble(floydala_hs_september_12th_grade)
floydala_hs_september_12th_grade$Abs <- as.numeric(floydala_hs_september_12th_grade$Abs)
floydala_hs_september_12th_grade$Mem <- as.numeric(floydala_hs_september_12th_grade$Mem)

floydala_hs_september_12th_grade


####################################################################################################

# FLOYDALA PDF #10


# Floydala HS
floydala_hs_2 <- pdf_text(".\\floydala_high_school_second_six_weeks.pdf") %>%
  readr::read_lines()

# The PDF was split into september, october, and november
floydala_hs_2 <- floydala_hs_2[-c(1:7, 10:73)] %>%
  str_squish() %>%
  strsplit(split = " ")

floydala_hs_2

# create dataframe
floydala_hs_2 <-   plyr::ldply(floydala_hs_2)
floydala_hs_september2 <- floydala_hs_2[1:11]
colnames(floydala_hs_september2) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                      'Abs', 'Mem', 'Abs', 'Mem')
floydala_hs_september2[1] <- seq(as.Date("2020/09/29"), by = "day", length.out = 2)

floydala_hs_september2

# Separate by grade level

# 9th Grade

floydala_hs_september2_9th_grade <- floydala_hs_september2[1:3]
floydala_hs_september2_9th_grade <- as_tibble(floydala_hs_september2_9th_grade)
floydala_hs_september2_9th_grade$Abs <- as.numeric(floydala_hs_september2_9th_grade$Abs)
floydala_hs_september2_9th_grade$Mem <- as.numeric(floydala_hs_september2_9th_grade$Mem)

floydala_hs_september2_9th_grade

# 10th Grade
floydala_hs_september2_10th_grade <- floydala_hs_september2[c(1,4,5)]
floydala_hs_september2_10th_grade <- as_tibble(floydala_hs_september2_10th_grade)
floydala_hs_september2_10th_grade$Abs <- as.numeric(floydala_hs_september2_10th_grade$Abs) 
floydala_hs_september2_10th_grade$Mem <- as.numeric(floydala_hs_september2_10th_grade$Mem)

floydala_hs_september2_10th_grade


# 11th
floydala_hs_september2_11th_grade <- floydala_hs_september2[c(1,6,7)]
floydala_hs_september2_11th_grade <- as_tibble(floydala_hs_september2_11th_grade)
floydala_hs_september2_11th_grade$Abs <- as.numeric(floydala_hs_september2_11th_grade$Abs)
floydala_hs_september2_11th_grade$Mem <- as.numeric(floydala_hs_september2_11th_grade$Mem)

floydala_hs_september2_11th_grade

# 12th
floydala_hs_september2_12th_grade <- floydala_hs_september2[c(1,8,9)] 
floydala_hs_september2_12th_grade <- as_tibble(floydala_hs_september2_12th_grade)
floydala_hs_september2_12th_grade$Abs <- as.numeric(floydala_hs_september2_12th_grade$Abs)
floydala_hs_september2_12th_grade$Mem <- as.numeric(floydala_hs_september2_12th_grade$Mem)

floydala_hs_september2_12th_grade



# Bind both septembers for each grade level together

# 9th grade
floydala_hs_september_9th_grade <- rbind(floydala_hs_september_9th_grade,
                                         floydala_hs_september2_9th_grade)
rm(floydala_hs_september2_9th_grade)


# 10th grade
floydala_hs_september_10th_grade <- rbind(floydala_hs_september_10th_grade,
                                          floydala_hs_september2_10th_grade)

rm(floydala_hs_september2_10th_grade)

# 11th grade
floydala_hs_september_11th_grade <- rbind(floydala_hs_september_11th_grade,
                                          floydala_hs_september2_11th_grade)

rm(floydala_hs_september2_11th_grade)

# 12th grade
floydala_hs_september_12th_grade <- rbind(floydala_hs_september_12th_grade,
                                          floydala_hs_september2_12th_grade)

rm(floydala_hs_september2_12th_grade)

# OCTOBER
floydala_hs_2 <- pdf_text(".\\floydala_high_school_second_six_weeks.pdf") %>%
  readr::read_lines()

floydala_hs_october <- floydala_hs_2[-c(1:18, 49:56, 58:73)] %>%
  str_squish() %>%
  strsplit(split = " ")

floydala_hs_october

# fill missing values
lapply(floydala_hs_october, length)
floydala_hs_october[[3]] <- c('3', 'NA', 'NA', 'NA', 'NA',
                              'NA', 'NA', 'NA', 'NA', 'NA',
                              'NA')
floydala_hs_october[[4]] <- c('4', 'NA', 'NA', 'NA', 'NA',
                              'NA', 'NA', 'NA', 'NA', 'NA',
                              'NA')
floydala_hs_october[[10]] <- c('10', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')
floydala_hs_october[[11]] <- c('11', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')
floydala_hs_october[[12]] <- c('12', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')
floydala_hs_october[[17]] <- c('17', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')
floydala_hs_october[[18]] <- c('18', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')
floydala_hs_october[[24]] <- c('24', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')
floydala_hs_october[[25]] <- c('25', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')
floydala_hs_october[[31]] <- c('31', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')

# create dataframe
floydala_hs_october <-   plyr::ldply(floydala_hs_october)
floydala_hs_october <- floydala_hs_october[1:11]
colnames(floydala_hs_october) <-  c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                    'Abs', 'Mem', 'Abs', 'Mem')
floydala_hs_october[1] <- seq(as.Date("2020/10/01"), by = "day", length.out = 31)

# Separate by grade level

# 9th Grade

floydala_hs_october_9th_grade <- floydala_hs_october[1:3]
floydala_hs_october_9th_grade <- as_tibble(floydala_hs_october_9th_grade)
floydala_hs_october_9th_grade$Abs <- as.numeric(floydala_hs_october_9th_grade$Abs)
floydala_hs_october_9th_grade$Mem <- as.numeric(floydala_hs_october_9th_grade$Mem)

floydala_hs_october_9th_grade

# 10th Grade
floydala_hs_october_10th_grade <- floydala_hs_october[c(1,4,5)]
floydala_hs_october_10th_grade <- as_tibble(floydala_hs_october_10th_grade)
floydala_hs_october_10th_grade$Abs <- as.numeric(floydala_hs_october_10th_grade$Abs) 
floydala_hs_october_10th_grade$Mem <- as.numeric(floydala_hs_october_10th_grade$Mem)

floydala_hs_october_10th_grade


# 11th Grade
floydala_hs_october_11th_grade <- floydala_hs_october[c(1,6,7)]
floydala_hs_october_11th_grade <- as_tibble(floydala_hs_october_11th_grade)
floydala_hs_october_11th_grade$Abs <- as.numeric(floydala_hs_october_11th_grade$Abs)
floydala_hs_october_11th_grade$Mem <- as.numeric(floydala_hs_october_11th_grade$Mem)

floydala_hs_october_11th_grade

# 12th Grade
floydala_hs_october_12th_grade <- floydala_hs_october[c(1,8,9)] 
floydala_hs_october_12th_grade <- as_tibble(floydala_hs_october_12th_grade)
floydala_hs_october_12th_grade$Abs <- as.numeric(floydala_hs_october_12th_grade$Abs)
floydala_hs_october_12th_grade$Mem <- as.numeric(floydala_hs_october_12th_grade$Mem)

floydala_hs_october_12th_grade

# NOVEMBER

floydala_hs_november <- floydala_hs_2[-c(1:66,73)] %>%
  str_squish() %>%
  strsplit(split = " ")

# fill missing values
lapply(floydala_hs_november, length)
floydala_hs_november[[1]] <- c('1', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')

# create dataframe
floydala_hs_november <-   plyr::ldply(floydala_hs_november)
floydala_hs_november <- floydala_hs_november[1:11]
colnames(floydala_hs_november) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                    'Abs', 'Mem', 'Abs', 'Mem')
floydala_hs_november[1] <- seq(as.Date("2020/11/01"), by = "day", length.out = 6)

floydala_hs_november


# Seperate by grade level

# 9th Grade
floydala_hs_november_9th_grade <- floydala_hs_november[1:3]
floydala_hs_november_9th_grade <- as_tibble(floydala_hs_november_9th_grade)
floydala_hs_november_9th_grade$Abs <- as.numeric(floydala_hs_november_9th_grade$Abs)
floydala_hs_november_9th_grade$Mem <- as.numeric(floydala_hs_november_9th_grade$Mem)

floydala_hs_november_9th_grade

# 10th Grade
floydala_hs_november_10th_grade <- floydala_hs_november[c(1,4,5)]
floydala_hs_november_10th_grade <- as_tibble(floydala_hs_november_10th_grade)
floydala_hs_november_10th_grade$Abs <- as.numeric(floydala_hs_november_10th_grade$Abs) 
floydala_hs_november_10th_grade$Mem <- as.numeric(floydala_hs_november_10th_grade$Mem)

floydala_hs_november_10th_grade


# 11th Grade
floydala_hs_november_11th_grade <- floydala_hs_november[c(1,6,7)]
floydala_hs_november_11th_grade <- as_tibble(floydala_hs_november_11th_grade)
floydala_hs_november_11th_grade$Abs <- as.numeric(floydala_hs_november_11th_grade$Abs)
floydala_hs_november_11th_grade$Mem <- as.numeric(floydala_hs_november_11th_grade$Mem)

floydala_hs_november_11th_grade

# 12th Grade
floydala_hs_november_12th_grade <- floydala_hs_november[c(1,8,9)] 
floydala_hs_november_12th_grade <- as_tibble(floydala_hs_november_12th_grade)
floydala_hs_november_12th_grade$Abs <- as.numeric(floydala_hs_november_12th_grade$Abs)
floydala_hs_november_12th_grade$Mem <- as.numeric(floydala_hs_november_12th_grade$Mem)

floydala_hs_november_12th_grade

###############################################################################

# BIND ALL HIGH SCHOOL DATASETS BY GRADE LEVEL

# 11th grade

floydala_hs_11th_grade_all_dates <- rbind(floydala_hs_august_11th_grade,
                                          floydala_hs_september_11th_grade,
                                          floydala_hs_october_11th_grade,
                                          floydala_hs_november_11th_grade)
floydala_hs_11th_grade_all_dates

# 12th grade
floydala_hs_12th_grade_all_dates <- rbind(floydala_hs_august_12th_grade,
                                          floydala_hs_september_12th_grade,
                                          floydala_hs_october_12th_grade,
                                          floydala_hs_november_12th_grade)
floydala_hs_12th_grade_all_dates

# 9th grade
floydala_hs_9th_grade_all_dates <- rbind(floydala_hs_august_9th_grade,
                                         floydala_hs_september_9th_grade,
                                         floydala_hs_october_9th_grade,
                                         floydala_hs_november_9th_grade)
floydala_hs_9th_grade_all_dates

# 10th grade
floydala_hs_10th_grade_all_dates <- rbind(floydala_hs_august_10th_grade,
                                          floydala_hs_september_10th_grade,
                                          floydala_hs_october_10th_grade,
                                          floydala_hs_november_10th_grade)
floydala_hs_10th_grade_all_dates


####################################################################################################


# FLOYDALA PDF #11
# FLOYDALA MIDDLE SCHOOL


# Split into months
floydala_ms <- pdf_text(".\\floydala_middle_school_first_six_weeks.pdf") %>%
  readr::read_lines()


floydala_ms_august <- floydala_ms[-c(1:7, 23:57)]
floydala_ms_september <- floydala_ms[-c(1:31,57)]


# Floydala Middle School August
floydala_ms_august <- floydala_ms_august %>%
  str_squish() %>%
  strsplit(split = " ")

floydala_ms_august

# fill missing values
lapply(floydala_ms_august, length)
floydala_ms_august[[6]] <- c('22', 'NA', 'NA', 'NA', 'NA',
                             'NA', 'NA', 'NA', 'NA')
floydala_ms_august[[7]] <- c('23', 'NA', 'NA', 'NA', 'NA',
                             'NA', 'NA', 'NA', 'NA')
floydala_ms_august[[13]] <- c('29', 'NA', 'NA', 'NA', 'NA',
                              'NA', 'NA', 'NA', 'NA')
floydala_ms_august[[14]] <- c('30', 'NA', 'NA', 'NA', 'NA',
                              'NA', 'NA', 'NA', 'NA')

# create dataframe
floydala_ms_august <-   plyr::ldply(floydala_ms_august)
floydala_ms_august <- floydala_ms_august[1:9]
colnames(floydala_ms_august) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                  'Abs', 'Mem')
floydala_ms_august[1] <- seq(as.Date("2020/08/17"), by = "day", length.out = 15)

# Separate by grade level

# 6th Grade
floydala_ms_august_6th_grade <- floydala_ms_august[c(1:3)]
floydala_ms_august_6th_grade <- as_tibble(floydala_ms_august_6th_grade)
floydala_ms_august_6th_grade$Abs <- as.numeric(floydala_ms_august_6th_grade$Abs)
floydala_ms_august_6th_grade$Mem <- as.numeric(floydala_ms_august_6th_grade$Mem)

floydala_ms_august_6th_grade

# 7th Grade
floydala_ms_august_7th_grade <- floydala_ms_august[c(1,4,5)]
floydala_ms_august_7th_grade <- as_tibble(floydala_ms_august_7th_grade)
floydala_ms_august_7th_grade$Abs <- as.numeric(floydala_ms_august_7th_grade$Abs) 
floydala_ms_august_7th_grade$Mem <- as.numeric(floydala_ms_august_7th_grade$Mem)

floydala_ms_august_7th_grade


# 8th Grade
floydala_ms_august_8th_grade <- floydala_ms_august[c(1,6,7)]
floydala_ms_august_8th_grade <- as_tibble(floydala_ms_august_8th_grade)
floydala_ms_august_8th_grade$Abs <- as.numeric(floydala_ms_august_8th_grade$Abs)
floydala_ms_august_8th_grade$Mem <- as.numeric(floydala_ms_august_8th_grade$Mem)

floydala_ms_august_8th_grade


# SEPTEMBER
floydala_ms_september <- floydala_ms[-c(1:31,57)]

floydala_ms_september <- floydala_ms_september %>%
  str_squish() %>%
  strsplit(split = " ")

# fill missing values
lapply(floydala_ms_september, length)
floydala_ms_september[[5]] <- c('05', 'NA', 'NA', 'NA', 'NA',
                                'NA', 'NA', 'NA', 'NA')
floydala_ms_september[[6]] <- c('06', 'NA', 'NA', 'NA', 'NA',
                                'NA', 'NA', 'NA', 'NA')
floydala_ms_september[[7]] <- c('07', 'NA', 'NA', 'NA', 'NA',
                                'NA', 'NA', 'NA', 'NA')
floydala_ms_september[[12]] <- c('12', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA')
floydala_ms_september[[13]] <- c('13', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA')
floydala_ms_september[[19]] <- c('19', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA')
floydala_ms_september[[20]] <- c('20', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA')


# Create Dataframe

# create dataframe
floydala_ms_september <-   plyr::ldply(floydala_ms_september)
floydala_ms_september <- floydala_ms_september[1:9]
colnames(floydala_ms_september) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                     'Abs', 'Mem')
floydala_ms_september[1] <- seq(as.Date("2020/09/01"), by = "day", length.out = 25)
floydala_ms_september



# Separate by grade level

# 6th Grade

floydala_ms_september_6th_grade <- floydala_ms_september[1:3]
floydala_ms_september_6th_grade <- as_tibble(floydala_ms_september_6th_grade)
floydala_ms_september_6th_grade$Abs <- as.numeric(floydala_ms_september_6th_grade$Abs)
floydala_ms_september_6th_grade$Mem <- as.numeric(floydala_ms_september_6th_grade$Mem)

floydala_ms_september_6th_grade

# 7th Grade
floydala_ms_september_7th_grade <- floydala_ms_september[c(1,4,5)]
floydala_ms_september_7th_grade <- as_tibble(floydala_ms_september_7th_grade)
floydala_ms_september_7th_grade$Abs <- as.numeric(floydala_ms_september_7th_grade$Abs) 
floydala_ms_september_7th_grade$Mem <- as.numeric(floydala_ms_september_7th_grade$Mem)

floydala_ms_september_7th_grade


# 8th Grade
floydala_ms_september_8th_grade <- floydala_ms_september[c(1,6,7)]
floydala_ms_september_8th_grade <- as_tibble(floydala_ms_september_8th_grade)
floydala_ms_september_8th_grade$Abs <- as.numeric(floydala_ms_september_8th_grade$Abs)
floydala_ms_september_8th_grade$Mem <- as.numeric(floydala_ms_september_8th_grade$Mem)

floydala_ms_september_8th_grade


############################################################################################################

# FLOYDALA PDF #12


# Floydala Middle School

floydala_ms2 <- pdf_text(".\\floydala_middle_school_second_six_weeks.pdf") %>%
  readr::read_lines()

# The PDF was split into september, october, and november
floydala_ms2 <- floydala_ms2[-c(1:7, 10:73)] %>%
  str_squish() %>%
  strsplit(split = " ")

floydala_ms2

# create dataframe
floydala_ms2_september2 <-   plyr::ldply(floydala_ms2)
floydala_ms2_september2 <- floydala_ms2_september2[1:9]
colnames(floydala_ms2_september2) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                       'Abs', 'Mem')
floydala_ms2_september2[1] <- seq(as.Date("2020/09/29"), by = "day", length.out = 2)

floydala_ms2_september2

# Separate by grade level

# 6th Grade

floydala_ms2_september2_6th_Grade <- floydala_ms2_september2[1:3]
floydala_ms2_september2_6th_Grade <- as_tibble(floydala_ms2_september2_6th_Grade)
floydala_ms2_september2_6th_Grade$Abs <- as.numeric(floydala_ms2_september2_6th_Grade$Abs)
floydala_ms2_september2_6th_Grade$Mem <- as.numeric(floydala_ms2_september2_6th_Grade$Mem)

floydala_ms2_september2_6th_Grade

# 7th Grade
floydala_ms2_september2_7th_Grade <- floydala_ms2_september2[c(1,4,5)]
floydala_ms2_september2_7th_Grade <- as_tibble(floydala_ms2_september2_7th_Grade)
floydala_ms2_september2_7th_Grade$Abs <- as.numeric(floydala_ms2_september2_7th_Grade$Abs) 
floydala_ms2_september2_7th_Grade$Mem <- as.numeric(floydala_ms2_september2_7th_Grade$Mem)

floydala_ms2_september2_7th_Grade


# 8th
floydala_ms2_september2_8th_Grade <- floydala_ms2_september2[c(1,6,7)]
floydala_ms2_september2_8th_Grade <- as_tibble(floydala_ms2_september2_8th_Grade)
floydala_ms2_september2_8th_Grade$Abs <- as.numeric(floydala_ms2_september2_8th_Grade$Abs)
floydala_ms2_september2_8th_Grade$Mem <- as.numeric(floydala_ms2_september2_8th_Grade$Mem)

floydala_ms2_september2_8th_Grade





# Bind both septembers for each grade level together

# 6th grade
floydala_ms_september_6th_grade <- rbind(floydala_ms_september_6th_grade,
                                         floydala_ms2_september2_6th_Grade)
rm(floydala_ms2_september2_6th_Grade)


# 7th grade
floydala_ms_september_7th_grade <- rbind(floydala_ms_september_7th_grade,
                                         floydala_ms2_september2_7th_Grade)

rm(floydala_ms2_september2_7th_Grade)

# 8th grade
floydala_ms_september_8th_grade <- rbind(floydala_ms_september_8th_grade,
                                         floydala_ms2_september2_8th_Grade)

rm(floydala_ms2_september2_8th_Grade)


# OCTOBER
floydala_ms_october <- pdf_text(".\\floydala_high_school_second_six_weeks.pdf") %>%
  readr::read_lines()

floydala_ms_october <- floydala_ms_october[-c(1:18, 49:56, 58:73)] %>%
  str_squish() %>%
  strsplit(split = " ")

floydala_ms_october

# fill missing values
lapply(floydala_ms_october, length)
floydala_ms_october[[3]] <- c('3', 'NA', 'NA', 'NA', 'NA',
                              'NA', 'NA', 'NA', 'NA', 'NA',
                              'NA')
floydala_ms_october[[4]] <- c('4', 'NA', 'NA', 'NA', 'NA',
                              'NA', 'NA', 'NA', 'NA', 'NA',
                              'NA')
floydala_ms_october[[10]] <- c('10', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')
floydala_ms_october[[11]] <- c('11', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')
floydala_ms_october[[12]] <- c('12', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')
floydala_ms_october[[17]] <- c('17', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')
floydala_ms_october[[18]] <- c('18', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')
floydala_ms_october[[24]] <- c('24', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')
floydala_ms_october[[25]] <- c('25', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')
floydala_ms_october[[31]] <- c('31', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')

# create dataframe
floydala_ms_october <-   plyr::ldply(floydala_ms_october)
floydala_ms_october <- floydala_ms_october[1:11]
colnames(floydala_ms_october) <-  c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                    'Abs', 'Mem', 'Abs', 'Mem')
floydala_ms_october[1] <- seq(as.Date("2020/10/01"), by = "day", length.out = 31)

# Separate by grade level

# 6th Grade

floydala_ms_october_6th_grade <- floydala_ms_october[1:3]
floydala_ms_october_6th_grade <- as_tibble(floydala_ms_october_6th_grade)
floydala_ms_october_6th_grade$Abs <- as.numeric(floydala_ms_october_6th_grade$Abs)
floydala_ms_october_6th_grade$Mem <- as.numeric(floydala_ms_october_6th_grade$Mem)

floydala_ms_october_6th_grade

# 7th Grade
floydala_ms_october_7th_grade <- floydala_ms_october[c(1,4,5)]
floydala_ms_october_7th_grade <- as_tibble(floydala_ms_october_7th_grade)
floydala_ms_october_7th_grade$Abs <- as.numeric(floydala_ms_october_7th_grade$Abs) 
floydala_ms_october_7th_grade$Mem <- as.numeric(floydala_ms_october_7th_grade$Mem)

floydala_ms_october_7th_grade


# 8th Grade
floydala_ms_october_8th_grade <- floydala_ms_october[c(1,6,7)]
floydala_ms_october_8th_grade <- as_tibble(floydala_ms_october_8th_grade)
floydala_ms_october_8th_grade$Abs <- as.numeric(floydala_ms_october_8th_grade$Abs)
floydala_ms_october_8th_grade$Mem <- as.numeric(floydala_ms_october_8th_grade$Mem)

floydala_ms_october_8th_grade


# NOVEMBER
floydala_ms_november <- pdf_text(".\\floydala_high_school_second_six_weeks.pdf") %>%
  readr::read_lines()
floydala_ms_november <- floydala_ms_november[-c(1:66,73)] %>%
  str_squish() %>%
  strsplit(split = " ")

# fill missing values
lapply(floydala_ms_november, length)
floydala_ms_november[[1]] <- c('1', 'NA', 'NA', 'NA', 'NA',
                               'NA', 'NA', 'NA', 'NA', 'NA',
                               'NA')

# create dataframe
floydala_ms_november <-   plyr::ldply(floydala_ms_november)
floydala_ms_november <- floydala_ms_november[1:11]
colnames(floydala_ms_november) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                    'Abs', 'Mem', 'Abs', 'Mem')
floydala_ms_november[1] <- seq(as.Date("2020/11/01"), by = "day", length.out = 6)

floydala_ms_november


# Seperate by grade level

# 6th Grade
floydala_ms_november_6th_grade <- floydala_ms_november[1:3]
floydala_ms_november_6th_grade <- as_tibble(floydala_ms_november_6th_grade)
floydala_ms_november_6th_grade$Abs <- as.numeric(floydala_ms_november_6th_grade$Abs)
floydala_ms_november_6th_grade$Mem <- as.numeric(floydala_ms_november_6th_grade$Mem)

floydala_ms_november_6th_grade

# 7th Grade
floydala_ms_november_7th_grade <- floydala_ms_november[c(1,4,5)]
floydala_ms_november_7th_grade <- as_tibble(floydala_ms_november_7th_grade)
floydala_ms_november_7th_grade$Abs <- as.numeric(floydala_ms_november_7th_grade$Abs) 
floydala_ms_november_7th_grade$Mem <- as.numeric(floydala_ms_november_7th_grade$Mem)

floydala_ms_november_7th_grade


# 8th Grade
floydala_ms_november_8th_grade <- floydala_ms_november[c(1,6,7)]
floydala_ms_november_8th_grade <- as_tibble(floydala_ms_november_8th_grade)
floydala_ms_november_8th_grade$Abs <- as.numeric(floydala_ms_november_8th_grade$Abs)
floydala_ms_november_8th_grade$Mem <- as.numeric(floydala_ms_november_8th_grade$Mem)

floydala_ms_november_8th_grade


###############################################################################

# BIND ALL HIGH SCHOOL DATASETS BY GRADE LEVEL

# 6th grade

floydala_ms_6th_grade_all_dates <- rbind(floydala_ms_august_6th_grade,
                                         floydala_ms_september_6th_grade,
                                         floydala_ms_october_6th_grade,
                                         floydala_ms_november_6th_grade)
floydala_ms_6th_grade_all_dates

# 7th grade
floydala_ms_7th_grade_all_dates <- rbind(floydala_ms_august_7th_grade,
                                         floydala_ms_september_7th_grade,
                                         floydala_ms_october_7th_grade,
                                         floydala_ms_november_7th_grade)
floydala_ms_7th_grade_all_dates

# 8th grade
floydala_ms_8th_grade_all_dates <- rbind(floydala_ms_august_8th_grade,
                                         floydala_ms_september_8th_grade,
                                         floydala_ms_october_8th_grade,
                                         floydala_ms_november_8th_grade)
floydala_ms_8th_grade_all_dates



########################################################################################################

# Remove Unneccesary Data

variables <- ls()
all_dates <- grepl('all_dates', ls())
rm(list = variables[!all_dates])

#########################################################################################################
school_datatype_summary <- read_excel(".\\school_datatype_summary.xlsx")
school_datatype_summary

# Filter for schools with daily data

school_daily_data <- school_datatype_summary %>% filter(DailyData == 1)
school_daily_data

# Filter for PDF files

school_pdf <- school_datatype_summary %>%
  filter(PDF == 1)
school_pdf <- school_pdf[,1]
school_pdf


#########################################################################################################


# SNOOK ISD PDF #1


# SNOOK ELEMENTARY SCHOOL PDF 1

# Split into months
snook_es <- pdf_text(".\\snook_es_cycle_1.pdf") %>%
  readr::read_lines()

snook_es_august <- snook_es[-c(1:7, 21:57)]
snook_es_september <- snook_es[-c(1:29,55)]


# SNOOK ELEMENTARY SCHOOL AUGUST 
snook_es_august <- snook_es_august %>%
  str_squish() %>%
  strsplit(split = " ")

snook_es_august

# fill missing values
lapply(snook_es_august, length)
snook_es_august[[4]] <- c('22', 'NA', 'NA', 'NA', 'NA',
                          'NA', 'NA', 'NA', 'NA', 'NA',
                          'NA', 'NA', 'NA', 'NA', 'NA',
                          'NA', 'NA')
snook_es_august[[5]] <- c('23', 'NA', 'NA', 'NA', 'NA',
                          'NA', 'NA', 'NA', 'NA', 'NA',
                          'NA', 'NA', 'NA', 'NA', 'NA',
                          'NA', 'NA')
snook_es_august[[11]] <- c('29', 'NA', 'NA', 'NA', 'NA',
                           'NA', 'NA', 'NA', 'NA', 'NA',
                           'NA', 'NA', 'NA', 'NA', 'NA',
                           'NA', 'NA')
snook_es_august[[12]] <- c('30', 'NA', 'NA', 'NA', 'NA',
                           'NA', 'NA', 'NA', 'NA', 'NA',
                           'NA', 'NA', 'NA', 'NA', 'NA',
                           'NA', 'NA')

# create dataframe
snook_es_august <-   plyr::ldply(snook_es_august)
snook_es_august <- snook_es_august[1:15]
colnames(snook_es_august) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                               'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs',
                               'Mem')
snook_es_august[1] <- seq(as.Date("2020/08/19"), by = "day", length.out = 13)

# Separate by grade level
snook_es_august_early_ed <- snook_es_august[c(1:3)]
snook_es_august_early_ed <- as_tibble(snook_es_august_early_ed)
snook_es_august_early_ed$Abs <- as.numeric(snook_es_august_early_ed$Abs)
snook_es_august_early_ed$Mem <- as.numeric(snook_es_august_early_ed$Mem)

snook_es_august_early_ed


# Kinder
snook_es_august_kinder <- snook_es_august[c(1,4,5)]
snook_es_august_kinder <- as_tibble(snook_es_august_kinder)
snook_es_august_kinder$Abs <- as.numeric(snook_es_august_kinder$Abs)
snook_es_august_kinder$Mem <- as.numeric(snook_es_august_kinder$Mem)

snook_es_august_kinder


# First Grade
snook_es_august_1st_grade <- snook_es_august[c(1,6,7)]
snook_es_august_1st_grade <- as_tibble(snook_es_august_1st_grade)
snook_es_august_1st_grade$Abs <- as.numeric(snook_es_august_1st_grade$Abs)
snook_es_august_1st_grade$Mem <- as.numeric(snook_es_august_1st_grade$Mem)

snook_es_august_1st_grade

# Second Grade
snook_es_august_2nd_grade <- snook_es_august[c(1,8,9)] 
snook_es_august_2nd_grade <- as_tibble(snook_es_august_2nd_grade)
snook_es_august_2nd_grade$Abs <- as.numeric(snook_es_august_2nd_grade$Abs)
snook_es_august_2nd_grade$Mem <- as.numeric(snook_es_august_2nd_grade$Mem)

snook_es_august_2nd_grade

# Third Grade
snook_es_august_3rd_grade <- snook_es_august[c(1,10,11)]
snook_es_august_3rd_grade <- as_tibble(snook_es_august_3rd_grade)
snook_es_august_3rd_grade$Abs <- as.numeric(snook_es_august_3rd_grade$Abs)
snook_es_august_3rd_grade$Mem <- as.numeric(snook_es_august_3rd_grade$Mem)

snook_es_august_3rd_grade

# Fourth Grade
snook_es_august_4th_grade <- snook_es_august[c(1,12,13)]
snook_es_august_4th_grade <- as_tibble(snook_es_august_4th_grade)
snook_es_august_4th_grade$Abs <- as.numeric(snook_es_august_4th_grade$Abs)
snook_es_august_4th_grade$Mem <- as.numeric(snook_es_august_4th_grade$Mem)

snook_es_august_4th_grade

# Fifth Grade
snook_es_august_5th_grade <- snook_es_august[c(1,14,15)]
snook_es_august_5th_grade <- as_tibble(snook_es_august_5th_grade)
snook_es_august_5th_grade$Abs <- as.numeric(snook_es_august_5th_grade$Abs)
snook_es_august_5th_grade$Mem <- as.numeric(snook_es_august_5th_grade$Mem)

snook_es_august_5th_grade


# SEPTEMBER

snook_es_september <- snook_es_september %>%
  str_squish() %>%
  strsplit(split = " ")

# fill missing values
lapply(snook_es_september, length)
snook_es_september[[5]] <- c('05', 'NA', 'NA', 'NA', 'NA',
                             'NA', 'NA', 'NA', 'NA', 'NA',
                             'NA', 'NA', 'NA', 'NA', 'NA',
                             'NA', 'NA')
snook_es_september[[6]] <- c('06', 'NA', 'NA', 'NA', 'NA',
                             'NA', 'NA', 'NA', 'NA', 'NA',
                             'NA', 'NA', 'NA', 'NA', 'NA',
                             'NA', 'NA')
snook_es_september[[7]] <- c('07', 'NA', 'NA', 'NA', 'NA',
                             'NA', 'NA', 'NA', 'NA', 'NA',
                             'NA', 'NA', 'NA', 'NA', 'NA',
                             'NA', 'NA')
snook_es_september[[12]] <- c('12', 'NA', 'NA', 'NA', 'NA',
                              'NA', 'NA', 'NA', 'NA', 'NA',
                              'NA', 'NA', 'NA', 'NA', 'NA',
                              'NA', 'NA')
snook_es_september[[13]] <- c('13', 'NA', 'NA', 'NA', 'NA',
                              'NA', 'NA', 'NA', 'NA', 'NA',
                              'NA', 'NA', 'NA', 'NA', 'NA',
                              'NA', 'NA')
snook_es_september[[19]] <- c('19', 'NA', 'NA', 'NA', 'NA',
                              'NA', 'NA', 'NA', 'NA', 'NA',
                              'NA', 'NA', 'NA', 'NA', 'NA',
                              'NA', 'NA')
snook_es_september[[20]] <- c('20', 'NA', 'NA', 'NA', 'NA',
                              'NA', 'NA', 'NA', 'NA', 'NA',
                              'NA', 'NA', 'NA', 'NA', 'NA',
                              'NA', 'NA')


# Create Dataframe

# create dataframe
snook_es_september <-   plyr::ldply(snook_es_september)
snook_es_september <- snook_es_september[1:15]
colnames(snook_es_september) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                  'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                  'Abs', 'Mem')
snook_es_september[1] <- seq(as.Date("2020/09/01"), by = "day", length.out = 25)
snook_es_september



# Separate by grade level

# Early Education

snook_es_september_early_ed <- snook_es_september[1:3]
snook_es_september_early_ed <- as_tibble(snook_es_september_early_ed)
snook_es_september_early_ed$Abs <- as.numeric(snook_es_september_early_ed$Abs)
snook_es_september_early_ed$Mem <- as.numeric(snook_es_september_early_ed$Mem)

snook_es_september_early_ed

# Kinder
snook_es_september_kinder <- snook_es_september[c(1,4,5)]
snook_es_september_kinder <- as_tibble(snook_es_september_kinder)
snook_es_september_kinder$Abs <- as.numeric(snook_es_september_kinder$Abs) 
snook_es_september_kinder$Mem <- as.numeric(snook_es_september_kinder$Mem)

snook_es_september_kinder


# First Grade
snook_es_september_1st_grade <- snook_es_september[c(1,6,7)]
snook_es_september_1st_grade <- as_tibble(snook_es_september_1st_grade)
snook_es_september_1st_grade$Abs <- as.numeric(snook_es_september_1st_grade$Abs)
snook_es_september_1st_grade$Mem <- as.numeric(snook_es_september_1st_grade$Mem)

snook_es_september_1st_grade

# Second Grade
snook_es_september_2nd_grade <- snook_es_september[c(1,8,9)] 
snook_es_september_2nd_grade <- as_tibble(snook_es_september_2nd_grade)
snook_es_september_2nd_grade$Abs <- as.numeric(snook_es_september_2nd_grade$Abs)
snook_es_september_2nd_grade$Mem <- as.numeric(snook_es_september_2nd_grade$Mem)

snook_es_september_2nd_grade

# Third Grade
snook_es_september_3rd_grade <- snook_es_september[c(1,10,11)]
snook_es_september_3rd_grade <- as_tibble(snook_es_september_3rd_grade)
snook_es_september_3rd_grade$Abs <- as.numeric(snook_es_september_3rd_grade$Abs)
snook_es_september_3rd_grade$Mem <- as.numeric(snook_es_september_3rd_grade$Mem)

snook_es_september_3rd_grade

# Fourth Grade
snook_es_september_4th_grade <- snook_es_september[c(1,12,13)]
snook_es_september_4th_grade <- as_tibble(snook_es_september_4th_grade)
snook_es_september_4th_grade$Abs <- as.numeric(snook_es_september_4th_grade$Abs)
snook_es_september_4th_grade$Mem <- as.numeric(snook_es_september_4th_grade$Mem)

snook_es_september_4th_grade

# Fifth Grade
snook_es_september_5th_grade <- snook_es_september[c(1,14,15)]
snook_es_september_5th_grade <- as_tibble(snook_es_september_5th_grade)
snook_es_september_5th_grade$Abs <- as.numeric(snook_es_september_5th_grade$Abs)
snook_es_september_5th_grade$Mem <- as.numeric(snook_es_september_5th_grade$Mem)

snook_es_september_5th_grade


###############################################################################

# BIND ALL SNOOK ES DATASETS BY GRADE LEVEL

# Early Education

snook_es_early_ed_all_dates <- rbind(snook_es_august_early_ed,
                                     snook_es_september_early_ed)


# Kinder

snook_es_kinder_all_dates <- rbind(snook_es_august_kinder,
                                   snook_es_september_kinder)

# 1st Grade

snook_es_1st_grade_all_dates <- rbind(snook_es_august_1st_grade,
                                      snook_es_september_1st_grade)

# 2nd Grade

snook_es_2nd_grade_all_dates <- rbind(snook_es_august_2nd_grade,
                                      snook_es_september_2nd_grade)

# 3rd Grade

snook_es_3rd_grade_all_dates <- rbind(snook_es_august_3rd_grade,
                                      snook_es_september_3rd_grade)

# 4th Grade

snook_es_4th_grade_all_dates <- rbind(snook_es_august_4th_grade,
                                      snook_es_september_4th_grade)

# 5th Grade

snook_es_5th_grade_all_dates <- rbind(snook_es_august_5th_grade,
                                      snook_es_september_5th_grade)

##################################################################################################


# SNOOK ES PDF 2


# SNOOK Elementary School Second Cycle PDF 
snook_es2 <- pdf_text(".\\snook_es_cycle_2.pdf") %>%
  readr::read_lines()

# The PDF was split into september, october, and november
snook_es_september2 <- snook_es2[-c(1:7, 11:74)] %>%
  str_squish() %>%
  strsplit(split = " ")

snook_es_september2

# create dataframe
snook_es_september2 <-   plyr::ldply(snook_es_september2)
snook_es_september2 <- snook_es_september2[1:15]
colnames(snook_es_september2) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                   'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs',
                                   'Mem')
snook_es_september2[1] <- seq(as.Date("2020/09/28"), by = "day", length.out = 3)

snook_es_september2

# Separate by grade level

# Early Education

snook_es_september2_early_ed <- snook_es_september2[1:3]
snook_es_september2_early_ed <- as_tibble(snook_es_september2_early_ed)
snook_es_september2_early_ed$Abs <- as.numeric(snook_es_september2_early_ed$Abs)
snook_es_september2_early_ed$Mem <- as.numeric(snook_es_september2_early_ed$Mem)

snook_es_september2_early_ed

# Kinder
snook_es_september2_kinder <- snook_es_september2[c(1,4,5)]
snook_es_september2_kinder <- as_tibble(snook_es_september2_kinder)
snook_es_september2_kinder$Abs <- as.numeric(snook_es_september2_kinder$Abs) 
snook_es_september2_kinder$Mem <- as.numeric(snook_es_september2_kinder$Mem)

snook_es_september2_kinder


# 1st Grade
snook_es_september2_1st_grade <- snook_es_september2[c(1,6,7)]
snook_es_september2_1st_grade <- as_tibble(snook_es_september2_1st_grade)
snook_es_september2_1st_grade$Abs <- as.numeric(snook_es_september2_1st_grade$Abs)
snook_es_september2_1st_grade$Mem <- as.numeric(snook_es_september2_1st_grade$Mem)

snook_es_september2_1st_grade

# Second Grade
snook_es_september2_2nd_grade <- snook_es_september2[c(1,8,9)] 
snook_es_september2_2nd_grade <- as_tibble(snook_es_september2_2nd_grade)
snook_es_september2_2nd_grade$Abs <- as.numeric(snook_es_september2_2nd_grade$Abs)
snook_es_september2_2nd_grade$Mem <- as.numeric(snook_es_september2_2nd_grade$Mem)

snook_es_september2_2nd_grade

# Third Grade
snook_es_september2_3rd_grade <- snook_es_september2[c(1,10,11)]
snook_es_september2_3rd_grade <- as_tibble(snook_es_september2_3rd_grade)
snook_es_september2_3rd_grade$Abs <- as.numeric(snook_es_september2_3rd_grade$Abs)
snook_es_september2_3rd_grade$Mem <- as.numeric(snook_es_september2_3rd_grade$Mem)

snook_es_september2_3rd_grade

# Fourth Grade
snook_es_september2_4th_grade <- snook_es_september2[c(1,12,13)]
snook_es_september2_4th_grade <- as_tibble(snook_es_september2_4th_grade)
snook_es_september2_4th_grade$Abs <- as.numeric(snook_es_september2_4th_grade$Abs)
snook_es_september2_4th_grade$Mem <- as.numeric(snook_es_september2_4th_grade$Mem)

snook_es_september2_4th_grade

# Fifth Grade
snook_es_september2_5th_grade <- snook_es_september2[c(1,14,15)]
snook_es_september2_5th_grade <- as_tibble(snook_es_september2_5th_grade)
snook_es_september2_5th_grade$Abs <- as.numeric(snook_es_september2_5th_grade$Abs)
snook_es_september2_5th_grade$Mem <- as.numeric(snook_es_september2_5th_grade$Mem)

snook_es_september2_5th_grade

# Bind both septembers for each grade level together

# Early Education
snook_es_early_ed_all_dates <- rbind(snook_es_early_ed_all_dates,
                                     snook_es_september2_early_ed)

# Kinder
snook_es_kinder_all_dates <- rbind(snook_es_kinder_all_dates,
                                   snook_es_september2_kinder)

# First Grade
snook_es_1st_grade_all_dates <- rbind(snook_es_1st_grade_all_dates,
                                      snook_es_september2_1st_grade)


# Second Grade
snook_es_2nd_grade_all_dates <- rbind(snook_es_2nd_grade_all_dates,
                                      snook_es_september2_2nd_grade)


# Third Grade
snook_es_3rd_grade_all_dates <- rbind(snook_es_3rd_grade_all_dates,
                                      snook_es_september2_3rd_grade)



# Fourth Grade
snook_es_4th_grade_all_dates <- rbind(snook_es_4th_grade_all_dates,
                                      snook_es_september2_4th_grade)


# Fifth Grade
snook_es_5th_grade_all_dates <- rbind(snook_es_5th_grade_all_dates,
                                      snook_es_september2_5th_grade)





# OCTOBER

snook_es_october <- snook_es2[-c(1:19, 50:74)] %>%
  str_squish() %>%
  strsplit(split = " ")

snook_es_october

# fill missing values
lapply(snook_es_october, length)
snook_es_october[[3]] <- c('3', 'NA', 'NA', 'NA', 'NA',
                           'NA', 'NA', 'NA', 'NA', 'NA',
                           'NA', 'NA', 'NA', 'NA', 'NA',
                           'NA', 'NA')
snook_es_october[[4]] <- c('4', 'NA', 'NA', 'NA', 'NA',
                           'NA', 'NA', 'NA', 'NA', 'NA',
                           'NA', 'NA', 'NA', 'NA', 'NA',
                           'NA', 'NA')
snook_es_october[[10]] <- c('10', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA')
snook_es_october[[11]] <- c('11', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA')
snook_es_october[[12]] <- c('12', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA')
snook_es_october[[17]] <- c('17', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA')
snook_es_october[[18]] <- c('18', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA')
snook_es_october[[24]] <- c('24', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA')
snook_es_october[[25]] <- c('25', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA', 'NA', 'NA', 'NA',
                            'NA', 'NA')

# create dataframe
snook_es_october <-   plyr::ldply(snook_es_october)
snook_es_october <- snook_es_october[1:15]
colnames(snook_es_october) <-  c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem')
snook_es_october[1] <- seq(as.Date("2020/10/01"), by = "day", length.out = 30)

# Separate by grade level

# Early Education
snook_es_october_early_ed <- snook_es_october[1:3]
snook_es_october_early_ed <- as_tibble(snook_es_october_early_ed)
snook_es_october_early_ed$Abs <- as.numeric(snook_es_october_early_ed$Abs)
snook_es_october_early_ed$Mem <- as.numeric(snook_es_october_early_ed$Mem)

# Kinder

snook_es_october_kinder <- snook_es_october[c(1,4,5)]
snook_es_october_kinder <- as_tibble(snook_es_october_kinder)
snook_es_october_kinder$Abs <- as.numeric(snook_es_october_kinder$Abs)
snook_es_october_kinder$Mem <- as.numeric(snook_es_october_kinder$Mem)


# First Grade
snook_es_october_1st_grade <- snook_es_october[c(1,6,7)]
snook_es_october_1st_grade <- as_tibble(snook_es_october_1st_grade)
snook_es_october_1st_grade$Abs <- as.numeric(snook_es_october_1st_grade$Abs) 
snook_es_october_1st_grade$Mem <- as.numeric(snook_es_october_1st_grade$Mem)

snook_es_october_1st_grade


# Second Grade
snook_es_october_2nd_grade <- snook_es_october[c(1,8,9)] 
snook_es_october_2nd_grade <- as_tibble(snook_es_october_2nd_grade)
snook_es_october_2nd_grade$Abs <- as.numeric(snook_es_october_2nd_grade$Abs)
snook_es_october_2nd_grade$Mem <- as.numeric(snook_es_october_2nd_grade$Mem)

snook_es_october_2nd_grade

# Third Grade
snook_es_october_3rd_grade <- snook_es_october[c(1,10,11)]
snook_es_october_3rd_grade <- as_tibble(snook_es_october_3rd_grade)
snook_es_october_3rd_grade$Abs <- as.numeric(snook_es_october_3rd_grade$Abs)
snook_es_october_3rd_grade$Mem <- as.numeric(snook_es_october_3rd_grade$Mem)

snook_es_october_3rd_grade

# Fourth Grade
snook_es_october_4th_grade <- snook_es_october[c(1,12,13)]
snook_es_october_4th_grade <- as_tibble(snook_es_october_4th_grade)
snook_es_october_4th_grade$Abs <- as.numeric(snook_es_october_4th_grade$Abs)
snook_es_october_4th_grade$Mem <- as.numeric(snook_es_october_4th_grade$Mem)

snook_es_october_4th_grade

# Fifth Grade
snook_es_october_5th_grade <- snook_es_october[c(1,14,15)]
snook_es_october_5th_grade <- as_tibble(snook_es_october_5th_grade)
snook_es_october_5th_grade$Abs <- as.numeric(snook_es_october_5th_grade$Abs)
snook_es_october_5th_grade$Mem <- as.numeric(snook_es_october_5th_grade$Mem)

snook_es_october_5th_grade


# NOVEMBER

snook_es_november <- snook_es2[69:73] %>%
  str_squish() %>%
  strsplit(split = " ")

# create dataframe
snook_es_november <-   plyr::ldply(snook_es_november)
snook_es_november <- snook_es_november[1:15]
colnames(snook_es_november) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem')
snook_es_november[1] <- seq(as.Date("2020/11/02"), by = "day", length.out = 5)

snook_es_november


# Seperate by grade level

# Early Education
snook_es_november_early_ed <- snook_es_november[1:3]
snook_es_november_early_ed <- as_tibble(snook_es_november_early_ed)
snook_es_november_early_ed$Abs <- as.numeric(snook_es_november_early_ed$Abs)
snook_es_november_early_ed$Mem <- as.numeric(snook_es_november_early_ed$Mem)

snook_es_november_early_ed


# Kinder
snook_es_november_kinder <- snook_es_november[c(1,4,5)]
snook_es_november_kinder <- as_tibble(snook_es_november_kinder)
snook_es_november_kinder$Abs <- as.numeric(snook_es_november_kinder$Abs)
snook_es_november_kinder$Mem <- as.numeric(snook_es_november_kinder$Mem)

snook_es_november_kinder

# First Grade
snook_es_november_1st_grade <- snook_es_november[c(1,6,7)]
snook_es_november_1st_grade <- as_tibble(snook_es_november_1st_grade)
snook_es_november_1st_grade$Abs <- as.numeric(snook_es_november_1st_grade$Abs) 
snook_es_november_1st_grade$Mem <- as.numeric(snook_es_november_1st_grade$Mem)

snook_es_november_1st_grade


# Second Grade
snook_es_november_2nd_grade <- snook_es_november[c(1,8,9)]
snook_es_november_2nd_grade <- as_tibble(snook_es_november_2nd_grade)
snook_es_november_2nd_grade$Abs <- as.numeric(snook_es_november_2nd_grade$Abs)
snook_es_november_2nd_grade$Mem <- as.numeric(snook_es_november_2nd_grade$Mem)

snook_es_november_2nd_grade

# Third Grade
snook_es_november_3rd_grade <- snook_es_november[c(1,10,11)] 
snook_es_november_3rd_grade <- as_tibble(snook_es_november_3rd_grade)
snook_es_november_3rd_grade$Abs <- as.numeric(snook_es_november_3rd_grade$Abs)
snook_es_november_3rd_grade$Mem <- as.numeric(snook_es_november_3rd_grade$Mem)

snook_es_november_3rd_grade

# Fourth Grade
snook_es_november_4th_grade <- snook_es_november[c(1,12,13)]
snook_es_november_4th_grade <- as_tibble(snook_es_november_4th_grade)
snook_es_november_4th_grade$Abs <- as.numeric(snook_es_november_4th_grade$Abs)
snook_es_november_4th_grade$Mem <- as.numeric(snook_es_november_4th_grade$Mem)

snook_es_november_4th_grade

# Fifth Grade
snook_es_november_5th_grade <- snook_es_november[c(1,14,15)]
snook_es_november_5th_grade <- as_tibble(snook_es_november_5th_grade)
snook_es_november_5th_grade$Abs <- as.numeric(snook_es_november_5th_grade$Abs)
snook_es_november_5th_grade$Mem <- as.numeric(snook_es_november_5th_grade$Mem)

snook_es_november_5th_grade

###############################################################################

# BIND ALL ELEMENTARY SCHOOL DATASETS BY GRADE LEVEL

# Early Education

snook_es_early_ed_all_dates <- rbind(snook_es_early_ed_all_dates,
                                     snook_es_october_early_ed,
                                     snook_es_november_early_ed)


# Kinder
snook_es_kinder_all_dates <- rbind(snook_es_kinder_all_dates,
                                   snook_es_october_kinder,
                                   snook_es_november_kinder)

# First Grade
snook_es_1st_grade_all_dates <- rbind(snook_es_1st_grade_all_dates,
                                      snook_es_october_1st_grade,
                                      snook_es_november_1st_grade)


# Second Grade
snook_es_2nd_grade_all_dates <- rbind(snook_es_2nd_grade_all_dates,
                                      snook_es_october_2nd_grade,
                                      snook_es_november_2nd_grade)


# Third Grade
snook_es_3rd_grade_all_dates <- rbind(snook_es_3rd_grade_all_dates,
                                      snook_es_october_3rd_grade,
                                      snook_es_november_3rd_grade)



# Fourth Grade
snook_es_4th_grade_all_dates <- rbind(snook_es_4th_grade_all_dates,
                                      snook_es_october_4th_grade,
                                      snook_es_november_4th_grade)



# Fifth Grade
snook_es_5th_grade_all_dates <- rbind(snook_es_5th_grade_all_dates,
                                      snook_es_october_5th_grade,
                                      snook_es_november_5th_grade)

##############################################################################################


# SNOOK ISD UPPER GRADES PDF #1


# SNOOK SECONDARY PDF 1

# Split into months
snook_secondary <- pdf_text(".\\snook_upper_cycle_1.pdf") %>%
  readr::read_lines()

snook_secondary_august <- snook_secondary[-c(1:7, 21:57)]
snook_secondary_september <- snook_secondary[-c(1:29,55)]


# SNOOK ELEMENTARY SCHOOL AUGUST 
snook_secondary_august <- snook_secondary_august %>%
  str_squish() %>%
  strsplit(split = " ")

snook_secondary_august

# fill missing values
lapply(snook_secondary_august, length)
snook_secondary_august[[4]] <- c('22', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA')
snook_secondary_august[[5]] <- c('23', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA', 'NA', 'NA', 'NA',
                                 'NA', 'NA')
snook_secondary_august[[11]] <- c('29', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA')
snook_secondary_august[[12]] <- c('30', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA')

# create dataframe
snook_secondary_august <-   plyr::ldply(snook_secondary_august)
snook_secondary_august <- snook_secondary_august[1:15]
colnames(snook_secondary_august) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                      'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs',
                                      'Mem')
snook_secondary_august[1] <- seq(as.Date("2020/08/19"), by = "day", length.out = 13)

# Separate by grade level

# 6th Grade
snook_secondary_august_6th_grade <- snook_secondary_august[c(1:3)]
snook_secondary_august_6th_grade <- as_tibble(snook_secondary_august_6th_grade)
snook_secondary_august_6th_grade$Abs <- as.numeric(snook_secondary_august_6th_grade$Abs)
snook_secondary_august_6th_grade$Mem <- as.numeric(snook_secondary_august_6th_grade$Mem)

snook_secondary_august_6th_grade


# 7th Grade
snook_secondary_august_7th_grade <- snook_secondary_august[c(1,4,5)]
snook_secondary_august_7th_grade <- as_tibble(snook_secondary_august_7th_grade)
snook_secondary_august_7th_grade$Abs <- as.numeric(snook_secondary_august_7th_grade$Abs)
snook_secondary_august_7th_grade$Mem <- as.numeric(snook_secondary_august_7th_grade$Mem)

snook_secondary_august_7th_grade


# 8th Grade
snook_secondary_august_8th_grade <- snook_secondary_august[c(1,6,7)]
snook_secondary_august_8th_grade <- as_tibble(snook_secondary_august_8th_grade)
snook_secondary_august_8th_grade$Abs <- as.numeric(snook_secondary_august_8th_grade$Abs)
snook_secondary_august_8th_grade$Mem <- as.numeric(snook_secondary_august_8th_grade$Mem)

snook_secondary_august_8th_grade

# 9th Grade
snook_secondary_august_9th_grade <- snook_secondary_august[c(1,8,9)] 
snook_secondary_august_9th_grade <- as_tibble(snook_secondary_august_9th_grade)
snook_secondary_august_9th_grade$Abs <- as.numeric(snook_secondary_august_9th_grade$Abs)
snook_secondary_august_9th_grade$Mem <- as.numeric(snook_secondary_august_9th_grade$Mem)

snook_secondary_august_9th_grade

# 10th Grade
snook_secondary_august_10th_grade <- snook_secondary_august[c(1,10,11)]
snook_secondary_august_10th_grade <- as_tibble(snook_secondary_august_10th_grade)
snook_secondary_august_10th_grade$Abs <- as.numeric(snook_secondary_august_10th_grade$Abs)
snook_secondary_august_10th_grade$Mem <- as.numeric(snook_secondary_august_10th_grade$Mem)

snook_secondary_august_10th_grade

# 11th Grade
snook_secondary_august_11th_grade <- snook_secondary_august[c(1,12,13)]
snook_secondary_august_11th_grade <- as_tibble(snook_secondary_august_11th_grade)
snook_secondary_august_11th_grade$Abs <- as.numeric(snook_secondary_august_11th_grade$Abs)
snook_secondary_august_11th_grade$Mem <- as.numeric(snook_secondary_august_11th_grade$Mem)

snook_secondary_august_11th_grade

# 12th Grade
snook_secondary_august_12th_grade <- snook_secondary_august[c(1,14,15)]
snook_secondary_august_12th_grade <- as_tibble(snook_secondary_august_12th_grade)
snook_secondary_august_12th_grade$Abs <- as.numeric(snook_secondary_august_12th_grade$Abs)
snook_secondary_august_12th_grade$Mem <- as.numeric(snook_secondary_august_12th_grade$Mem)

snook_secondary_august_12th_grade


# SEPTEMBER

snook_secondary_september <- snook_secondary_september %>%
  str_squish() %>%
  strsplit(split = " ")

# fill missing values
lapply(snook_secondary_september, length)
snook_secondary_september[[5]] <- c('05', 'NA', 'NA', 'NA', 'NA',
                                    'NA', 'NA', 'NA', 'NA', 'NA',
                                    'NA', 'NA', 'NA', 'NA', 'NA',
                                    'NA', 'NA')
snook_secondary_september[[6]] <- c('06', 'NA', 'NA', 'NA', 'NA',
                                    'NA', 'NA', 'NA', 'NA', 'NA',
                                    'NA', 'NA', 'NA', 'NA', 'NA',
                                    'NA', 'NA')
snook_secondary_september[[7]] <- c('07', 'NA', 'NA', 'NA', 'NA',
                                    'NA', 'NA', 'NA', 'NA', 'NA',
                                    'NA', 'NA', 'NA', 'NA', 'NA',
                                    'NA', 'NA')
snook_secondary_september[[12]] <- c('12', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA')
snook_secondary_september[[13]] <- c('13', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA')
snook_secondary_september[[19]] <- c('19', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA')
snook_secondary_september[[20]] <- c('20', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA', 'NA', 'NA', 'NA',
                                     'NA', 'NA')


# Create Dataframe

# create dataframe
snook_secondary_september <-   plyr::ldply(snook_secondary_september)
snook_secondary_september <- snook_secondary_september[1:15]
colnames(snook_secondary_september) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                         'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                         'Abs', 'Mem')
snook_secondary_september[1] <- seq(as.Date("2020/09/01"), by = "day", length.out = 25)
snook_secondary_september



# Separate by grade level

# 6th Grade

snook_secondary_september_6th_grade <- snook_secondary_september[1:3]
snook_secondary_september_6th_grade <- as_tibble(snook_secondary_september_6th_grade)
snook_secondary_september_6th_grade$Abs <- as.numeric(snook_secondary_september_6th_grade$Abs)
snook_secondary_september_6th_grade$Mem <- as.numeric(snook_secondary_september_6th_grade$Mem)

snook_secondary_september_6th_grade

# 7th Grade
snook_secondary_september_7th_grade <- snook_secondary_september[c(1,4,5)]
snook_secondary_september_7th_grade <- as_tibble(snook_secondary_september_7th_grade)
snook_secondary_september_7th_grade$Abs <- as.numeric(snook_secondary_september_7th_grade$Abs) 
snook_secondary_september_7th_grade$Mem <- as.numeric(snook_secondary_september_7th_grade$Mem)

snook_secondary_september_7th_grade


# 8th Grade
snook_secondary_september_8th_grade <- snook_secondary_september[c(1,6,7)]
snook_secondary_september_8th_grade <- as_tibble(snook_secondary_september_8th_grade)
snook_secondary_september_8th_grade$Abs <- as.numeric(snook_secondary_september_8th_grade$Abs)
snook_secondary_september_8th_grade$Mem <- as.numeric(snook_secondary_september_8th_grade$Mem)

snook_secondary_september_8th_grade

# 9th Grade
snook_secondary_september_9th_grade <- snook_secondary_september[c(1,8,9)] 
snook_secondary_september_9th_grade <- as_tibble(snook_secondary_september_9th_grade)
snook_secondary_september_9th_grade$Abs <- as.numeric(snook_secondary_september_9th_grade$Abs)
snook_secondary_september_9th_grade$Mem <- as.numeric(snook_secondary_september_9th_grade$Mem)

snook_secondary_september_9th_grade

# 10th Grade
snook_secondary_september_10th_grade <- snook_secondary_september[c(1,10,11)]
snook_secondary_september_10th_grade <- as_tibble(snook_secondary_september_10th_grade)
snook_secondary_september_10th_grade$Abs <- as.numeric(snook_secondary_september_10th_grade$Abs)
snook_secondary_september_10th_grade$Mem <- as.numeric(snook_secondary_september_10th_grade$Mem)

snook_secondary_september_10th_grade

# 11th Grade
snook_secondary_september_11th_grade <- snook_secondary_september[c(1,12,13)]
snook_secondary_september_11th_grade <- as_tibble(snook_secondary_september_11th_grade)
snook_secondary_september_11th_grade$Abs <- as.numeric(snook_secondary_september_11th_grade$Abs)
snook_secondary_september_11th_grade$Mem <- as.numeric(snook_secondary_september_11th_grade$Mem)

snook_secondary_september_11th_grade

# 12th Grade
snook_secondary_september_12th_grade <- snook_secondary_september[c(1,14,15)]
snook_secondary_september_12th_grade <- as_tibble(snook_secondary_september_12th_grade)
snook_secondary_september_12th_grade$Abs <- as.numeric(snook_secondary_september_12th_grade$Abs)
snook_secondary_september_12th_grade$Mem <- as.numeric(snook_secondary_september_12th_grade$Mem)

snook_secondary_september_12th_grade


###############################################################################

# BIND ALL SNOOK SECONDARY DATASETS BY GRADE LEVEL

# 6th

snook_secondary_6th_grade_all_dates <- rbind(snook_secondary_august_6th_grade,
                                             snook_secondary_september_6th_grade)


# 7th

snook_secondary_7th_grade_all_dates <- rbind(snook_secondary_august_7th_grade,
                                             snook_secondary_september_7th_grade)

# 8th Grade

snook_secondary_8th_grade_all_dates <- rbind(snook_secondary_august_8th_grade,
                                             snook_secondary_september_8th_grade)

# 9th Grade

snook_secondary_9th_grade_all_dates <- rbind(snook_secondary_august_9th_grade,
                                             snook_secondary_september_9th_grade)


# 10th Grade

snook_secondary_10th_grade_all_dates <- rbind(snook_secondary_august_10th_grade,
                                              snook_secondary_september_10th_grade)

# 11th Grade

snook_secondary_11th_grade_all_dates <- rbind(snook_secondary_august_11th_grade,
                                              snook_secondary_september_11th_grade)

# 12th Grade

snook_secondary_12th_grade_all_dates <- rbind(snook_secondary_august_12th_grade,
                                              snook_secondary_september_12th_grade)

#########################################################################################################

# SNOOK SECONDARY PDF 2


# SNOOK Secondary School Second Cycle PDF 
snook_secondary2 <- pdf_text(".\\snook_upper_cycle_2.pdf") %>%
  readr::read_lines()

# The PDF was split into september, october, and november
snook_secondary_september2 <- snook_secondary2[-c(1:7, 11:74)] %>%
  str_squish() %>%
  strsplit(split = " ")
snook_secondary_september2


# create dataframe
snook_secondary_september2 <-   plyr::ldply(snook_secondary_september2)
snook_secondary_september2 <- snook_secondary_september2[1:15]
colnames(snook_secondary_september2) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                          'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs',
                                          'Mem')
snook_secondary_september2[1] <- seq(as.Date("2020/09/28"), by = "day", length.out = 3)

snook_secondary_september2

# Separate by grade level

# 6th Grade

snook_secondary_september2_6th_grade <- snook_secondary_september2[1:3]
snook_secondary_september2_6th_grade <- as_tibble(snook_secondary_september2_6th_grade)
snook_secondary_september2_6th_grade$Abs <- as.numeric(snook_secondary_september2_6th_grade$Abs)
snook_secondary_september2_6th_grade$Mem <- as.numeric(snook_secondary_september2_6th_grade$Mem)

snook_secondary_september2_6th_grade

# 7th Grade
snook_secondary_september2_7th_grade <- snook_secondary_september2[c(1,4,5)]
snook_secondary_september2_7th_grade <- as_tibble(snook_secondary_september2_7th_grade)
snook_secondary_september2_7th_grade$Abs <- as.numeric(snook_secondary_september2_7th_grade$Abs) 
snook_secondary_september2_7th_grade$Mem <- as.numeric(snook_secondary_september2_7th_grade$Mem)

snook_secondary_september2_7th_grade


# 8th Grade
snook_secondary_september2_8th_grade <- snook_secondary_september2[c(1,6,7)]
snook_secondary_september2_8th_grade <- as_tibble(snook_secondary_september2_8th_grade)
snook_secondary_september2_8th_grade$Abs <- as.numeric(snook_secondary_september2_8th_grade$Abs)
snook_secondary_september2_8th_grade$Mem <- as.numeric(snook_secondary_september2_8th_grade$Mem)

snook_secondary_september2_8th_grade

# 9th Grade
snook_secondary_september2_9th_grade <- snook_secondary_september2[c(1,8,9)] 
snook_secondary_september2_9th_grade <- as_tibble(snook_secondary_september2_9th_grade)
snook_secondary_september2_9th_grade$Abs <- as.numeric(snook_secondary_september2_9th_grade$Abs)
snook_secondary_september2_9th_grade$Mem <- as.numeric(snook_secondary_september2_9th_grade$Mem)

snook_secondary_september2_9th_grade

# 10th Grade
snook_secondary_september2_10th_grade <- snook_secondary_september2[c(1,10,11)]
snook_secondary_september2_10th_grade <- as_tibble(snook_secondary_september2_10th_grade)
snook_secondary_september2_10th_grade$Abs <- as.numeric(snook_secondary_september2_10th_grade$Abs)
snook_secondary_september2_10th_grade$Mem <- as.numeric(snook_secondary_september2_10th_grade$Mem)

snook_secondary_september2_10th_grade

# 11th Grade
snook_secondary_september2_11th_grade <- snook_secondary_september2[c(1,12,13)]
snook_secondary_september2_11th_grade <- as_tibble(snook_secondary_september2_11th_grade)
snook_secondary_september2_11th_grade$Abs <- as.numeric(snook_secondary_september2_11th_grade$Abs)
snook_secondary_september2_11th_grade$Mem <- as.numeric(snook_secondary_september2_11th_grade$Mem)

snook_secondary_september2_11th_grade

# 12th Grade
snook_secondary_september2_12th_grade <- snook_secondary_september2[c(1,14,15)]
snook_secondary_september2_12th_grade <- as_tibble(snook_secondary_september2_12th_grade)
snook_secondary_september2_12th_grade$Abs <- as.numeric(snook_secondary_september2_12th_grade$Abs)
snook_secondary_september2_12th_grade$Mem <- as.numeric(snook_secondary_september2_12th_grade$Mem)

snook_secondary_september2_12th_grade

# Bind both septembers for each grade level together

# 6th Grade
snook_secondary_6th_grade_all_dates <- rbind(snook_secondary_6th_grade_all_dates,
                                             snook_secondary_september2_6th_grade)

# 7th Grade
snook_secondary_7th_grade_all_dates <- rbind(snook_secondary_7th_grade_all_dates,
                                             snook_secondary_september2_7th_grade)

# 8th Grade
snook_secondary_8th_grade_all_dates <- rbind(snook_secondary_8th_grade_all_dates,
                                             snook_secondary_september2_8th_grade)


# 9th Grade
snook_secondary_9th_grade_all_dates <- rbind(snook_secondary_9th_grade_all_dates,
                                             snook_secondary_september2_9th_grade)


# 10th Grade
snook_secondary_10th_grade_all_dates <- rbind(snook_secondary_10th_grade_all_dates,
                                              snook_secondary_september2_10th_grade)



# 11th Grade
snook_secondary_11th_grade_all_dates <- rbind(snook_secondary_11th_grade_all_dates,
                                              snook_secondary_september2_11th_grade)


# 12th Grade
snook_secondary_12th_grade_all_dates <- rbind(snook_secondary_12th_grade_all_dates,
                                              snook_secondary_september2_12th_grade)





# OCTOBER

snook_secondary_october <- snook_secondary2[-c(1:19, 50:74)] %>%
  str_squish() %>%
  strsplit(split = " ")

snook_secondary_october

# fill missing values
lapply(snook_secondary_october, length)
snook_secondary_october[[3]] <- c('3', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA')
snook_secondary_october[[4]] <- c('4', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA', 'NA', 'NA', 'NA',
                                  'NA', 'NA')
snook_secondary_october[[10]] <- c('10', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA')
snook_secondary_october[[11]] <- c('11', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA')
snook_secondary_october[[12]] <- c('12', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA')
snook_secondary_october[[17]] <- c('17', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA')
snook_secondary_october[[18]] <- c('18', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA')
snook_secondary_october[[24]] <- c('24', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA')
snook_secondary_october[[25]] <- c('25', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA', 'NA', 'NA', 'NA',
                                   'NA', 'NA')

# create dataframe
snook_secondary_october <-   plyr::ldply(snook_secondary_october)
snook_secondary_october <- snook_secondary_october[1:15]
colnames(snook_secondary_october) <-  c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                        'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem')
snook_secondary_october[1] <- seq(as.Date("2020/10/01"), by = "day", length.out = 30)

# Separate by grade level

# 6th Grade
snook_secondary_october_6th_grade <- snook_secondary_october[1:3]
snook_secondary_october_6th_grade <- as_tibble(snook_secondary_october_6th_grade)
snook_secondary_october_6th_grade$Abs <- as.numeric(snook_secondary_october_6th_grade$Abs)
snook_secondary_october_6th_grade$Mem <- as.numeric(snook_secondary_october_6th_grade$Mem)
snook_secondary_october_6th_grade

# 7th Grade
snook_secondary_october_7th_grade <- snook_secondary_october[c(1,4,5)]
snook_secondary_october_7th_grade <- as_tibble(snook_secondary_october_7th_grade)
snook_secondary_october_7th_grade$Abs <- as.numeric(snook_secondary_october_7th_grade$Abs)
snook_secondary_october_7th_grade$Mem <- as.numeric(snook_secondary_october_7th_grade$Mem)
snook_secondary_october_7th_grade

# 8th Grade
snook_secondary_october_8th_grade <- snook_secondary_october[c(1,6,7)]
snook_secondary_october_8th_grade <- as_tibble(snook_secondary_october_8th_grade)
snook_secondary_october_8th_grade$Abs <- as.numeric(snook_secondary_october_8th_grade$Abs) 
snook_secondary_october_8th_grade$Mem <- as.numeric(snook_secondary_october_8th_grade$Mem)

snook_secondary_october_8th_grade


# 9th Grade
snook_secondary_october_9th_grade <- snook_secondary_october[c(1,8,9)] 
snook_secondary_october_9th_grade <- as_tibble(snook_secondary_october_9th_grade)
snook_secondary_october_9th_grade$Abs <- as.numeric(snook_secondary_october_9th_grade$Abs)
snook_secondary_october_9th_grade$Mem <- as.numeric(snook_secondary_october_9th_grade$Mem)

snook_secondary_october_9th_grade

# 10th Grade
snook_secondary_october_10th_grade <- snook_secondary_october[c(1,10,11)]
snook_secondary_october_10th_grade <- as_tibble(snook_secondary_october_10th_grade)
snook_secondary_october_10th_grade$Abs <- as.numeric(snook_secondary_october_10th_grade$Abs)
snook_secondary_october_10th_grade$Mem <- as.numeric(snook_secondary_october_10th_grade$Mem)

snook_secondary_october_10th_grade

# 11th Grade
snook_secondary_october_11th_grade <- snook_secondary_october[c(1,12,13)]
snook_secondary_october_11th_grade <- as_tibble(snook_secondary_october_11th_grade)
snook_secondary_october_11th_grade$Abs <- as.numeric(snook_secondary_october_11th_grade$Abs)
snook_secondary_october_11th_grade$Mem <- as.numeric(snook_secondary_october_11th_grade$Mem)

snook_secondary_october_11th_grade

# 12th Grade
snook_secondary_october_12th_grade <- snook_secondary_october[c(1,14,15)]
snook_secondary_october_12th_grade <- as_tibble(snook_secondary_october_12th_grade)
snook_secondary_october_12th_grade$Abs <- as.numeric(snook_secondary_october_12th_grade$Abs)
snook_secondary_october_12th_grade$Mem <- as.numeric(snook_secondary_october_12th_grade$Mem)

snook_secondary_october_12th_grade


# NOVEMBER

snook_secondary_november <- snook_secondary2[69:73] %>%
  str_squish() %>%
  strsplit(split = " ")

# create dataframe
snook_secondary_november <-   plyr::ldply(snook_secondary_november)
snook_secondary_november <- snook_secondary_november[1:15]
colnames(snook_secondary_november) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem',
                                        'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem', 'Abs', 'Mem')
snook_secondary_november[1] <- seq(as.Date("2020/11/02"), by = "day", length.out = 5)

snook_secondary_november


# Seperate by grade level

# 6th Grade
snook_secondary_november_6th_grade <- snook_secondary_november[1:3]
snook_secondary_november_6th_grade <- as_tibble(snook_secondary_november_6th_grade)
snook_secondary_november_6th_grade$Abs <- as.numeric(snook_secondary_november_6th_grade$Abs)
snook_secondary_november_6th_grade$Mem <- as.numeric(snook_secondary_november_6th_grade$Mem)

snook_secondary_november_6th_grade


# 7th Grade
snook_secondary_november_7th_grade <- snook_secondary_november[c(1,4,5)]
snook_secondary_november_7th_grade <- as_tibble(snook_secondary_november_7th_grade)
snook_secondary_november_7th_grade$Abs <- as.numeric(snook_secondary_november_7th_grade$Abs)
snook_secondary_november_7th_grade$Mem <- as.numeric(snook_secondary_november_7th_grade$Mem)

snook_secondary_november_7th_grade

# 8th Grade
snook_secondary_november_8th_grade <- snook_secondary_november[c(1,6,7)]
snook_secondary_november_8th_grade <- as_tibble(snook_secondary_november_8th_grade)
snook_secondary_november_8th_grade$Abs <- as.numeric(snook_secondary_november_8th_grade$Abs) 
snook_secondary_november_8th_grade$Mem <- as.numeric(snook_secondary_november_8th_grade$Mem)

snook_secondary_november_8th_grade


# 9th Grade
snook_secondary_november_9th_grade <- snook_secondary_november[c(1,8,9)]
snook_secondary_november_9th_grade <- as_tibble(snook_secondary_november_9th_grade)
snook_secondary_november_9th_grade$Abs <- as.numeric(snook_secondary_november_9th_grade$Abs)
snook_secondary_november_9th_grade$Mem <- as.numeric(snook_secondary_november_9th_grade$Mem)

snook_secondary_november_9th_grade

# 10th Grade
snook_secondary_november_10th_grade <- snook_secondary_november[c(1,10,11)] 
snook_secondary_november_10th_grade <- as_tibble(snook_secondary_november_10th_grade)
snook_secondary_november_10th_grade$Abs <- as.numeric(snook_secondary_november_10th_grade$Abs)
snook_secondary_november_10th_grade$Mem <- as.numeric(snook_secondary_november_10th_grade$Mem)

snook_secondary_november_10th_grade

# 11th Grade
snook_secondary_november_11th_grade <- snook_secondary_november[c(1,12,13)]
snook_secondary_november_11th_grade <- as_tibble(snook_secondary_november_11th_grade)
snook_secondary_november_11th_grade$Abs <- as.numeric(snook_secondary_november_11th_grade$Abs)
snook_secondary_november_11th_grade$Mem <- as.numeric(snook_secondary_november_11th_grade$Mem)

snook_secondary_november_11th_grade

# 12th Grade
snook_secondary_november_12th_grade <- snook_secondary_november[c(1,14,15)]
snook_secondary_november_12th_grade <- as_tibble(snook_secondary_november_12th_grade)
snook_secondary_november_12th_grade$Abs <- as.numeric(snook_secondary_november_12th_grade$Abs)
snook_secondary_november_12th_grade$Mem <- as.numeric(snook_secondary_november_12th_grade$Mem)

snook_secondary_november_12th_grade

###############################################################################

# BIND ALL SECONDARY DATASETS BY GRADE LEVEL

# 6th Grade

snook_secondary_6th_grade_all_dates <- rbind(snook_secondary_6th_grade_all_dates,
                                             snook_secondary_october_6th_grade,
                                             snook_secondary_november_6th_grade)


# 7th Grade
snook_secondary_7th_grade_all_dates <- rbind(snook_secondary_7th_grade_all_dates,
                                             snook_secondary_october_7th_grade,
                                             snook_secondary_november_7th_grade)
# 8th Grade
snook_secondary_8th_grade_all_dates <- rbind(snook_secondary_8th_grade_all_dates,
                                             snook_secondary_october_8th_grade,
                                             snook_secondary_november_8th_grade)

# 9th Grade
snook_secondary_9th_grade_all_dates <- rbind(snook_secondary_9th_grade_all_dates,
                                             snook_secondary_october_9th_grade,
                                             snook_secondary_november_9th_grade)

# 10th Grade
snook_secondary_10th_grade_all_dates <- rbind(snook_secondary_10th_grade_all_dates,
                                              snook_secondary_october_10th_grade,
                                              snook_secondary_november_10th_grade)

# 11th Grade
snook_secondary_11th_grade_all_dates <- rbind(snook_secondary_11th_grade_all_dates,
                                              snook_secondary_october_11th_grade,
                                              snook_secondary_november_11th_grade)

# 12th Grade
snook_secondary_12th_grade_all_dates <- rbind(snook_secondary_12th_grade_all_dates,
                                              snook_secondary_october_12th_grade,
                                              snook_secondary_november_12th_grade)



##############################################################################################


# SNOOK ISD PREK PDF #1


# SNOOK PREK PDF 1

# Split into months
snook_prek <- pdf_text(".\\snook_es_prek_cycle_1.pdf") %>%
  readr::read_lines()

snook_prek_august <- snook_prek[-c(1:7, 21:57)]
snook_prek_september <- snook_prek[-c(1:29,55)]


# SNOOK ELEMENTARY SCHOOL AUGUST 
snook_prek_august <- snook_prek_august %>%
  str_squish() %>%
  strsplit(split = " ")

snook_prek_august

# fill missing values
lapply(snook_prek_august, length)
snook_prek_august[[4]] <- c('22', 'NA', 'NA', 'NA', 'NA')
snook_prek_august[[5]] <- c('23', 'NA', 'NA', 'NA', 'NA')
snook_prek_august[[11]] <- c('29', 'NA', 'NA', 'NA', 'NA')
snook_prek_august[[12]] <- c('30', 'NA', 'NA', 'NA', 'NA')

# create dataframe
snook_prek_august <-   plyr::ldply(snook_prek_august)
snook_prek_august <- snook_prek_august[1:5]
colnames(snook_prek_august) <- c('Day', 'Abs', 'Mem', 'Abs', 'Mem')
snook_prek_august[1] <- seq(as.Date("2020/08/19"), by = "day", length.out = 13)

snook_prek_august <- snook_prek_august[c(1:3)]
snook_prek_august <- as_tibble(snook_prek_august)
snook_prek_august$Abs <- as.numeric(snook_prek_august$Abs)
snook_prek_august$Mem <- as.numeric(snook_prek_august$Mem)

snook_prek_august

# SEPTEMBER
snook_prek_september <- snook_prek[-c(1:29,55)]
snook_prek_september <- snook_prek_september %>%
  str_squish() %>%
  strsplit(split = " ")

# fill missing values
lapply(snook_prek_september, length)
snook_prek_september[[5]] <- c('05', 'NA', 'NA', 'NA', 'NA')
snook_prek_september[[6]] <- c('06', 'NA', 'NA', 'NA', 'NA')
snook_prek_september[[7]] <- c('07', 'NA', 'NA', 'NA', 'NA')
snook_prek_september[[12]] <- c('12', 'NA', 'NA', 'NA', 'NA')
snook_prek_september[[13]] <- c('13', 'NA', 'NA', 'NA', 'NA')
snook_prek_september[[19]] <- c('19', 'NA', 'NA', 'NA', 'NA')
snook_prek_september[[20]] <- c('20', 'NA', 'NA', 'NA', 'NA')


# create dataframe
snook_prek_september <-   plyr::ldply(snook_prek_september)
snook_prek_september <- snook_prek_september[1:3]
colnames(snook_prek_september) <- c('Day', 'Abs', 'Mem')
snook_prek_september[1] <- seq(as.Date("2020/09/01"), by = "day", length.out = 25)
snook_prek_september

###############################################################################

# BIND ALL SNOOK PREK DATASETS BY GRADE LEVEL

snook_prek_all_dates <- rbind(snook_prek_august,
                              snook_prek_september)

#################################################################################


# SNOOK PREK PDF 2


# SNOOK PREK Second Cycle PDF 
snook_prek2 <- pdf_text(".\\snook_es_prek_cycle_2.pdf") %>%
  readr::read_lines()

# The PDF was split into september, october, and november
snook_prek2_september2 <- snook_prek2[-c(1:7, 11:74)] %>%
  str_squish() %>%
  strsplit(split = " ")
snook_prek2_september2


# create dataframe
snook_prek2_september2 <-   plyr::ldply(snook_prek2_september2)
snook_prek2_september2 <- snook_prek2_september2[1:3]
colnames(snook_prek2_september2) <- c('Day', 'Abs', 'Mem')
snook_prek2_september2[1] <- seq(as.Date("2020/09/28"), by = "day", length.out = 3)

snook_prek2_september2

# OCTOBER

snook_prek2_october <- snook_prek2[-c(1:19, 50:74)] %>%
  str_squish() %>%
  strsplit(split = " ")

snook_prek2_october

# fill missing values
lapply(snook_prek2_october, length)
snook_prek2_october[[3]] <- c('3', 'NA', 'NA', 'NA', 'NA')
snook_prek2_october[[4]] <- c('4', 'NA', 'NA', 'NA', 'NA')
snook_prek2_october[[10]] <- c('10', 'NA', 'NA', 'NA', 'NA')
snook_prek2_october[[11]] <- c('11', 'NA', 'NA', 'NA', 'NA')
snook_prek2_october[[12]] <- c('12', 'NA', 'NA', 'NA', 'NA')
snook_prek2_october[[17]] <- c('17', 'NA', 'NA', 'NA', 'NA')
snook_prek2_october[[18]] <- c('18', 'NA', 'NA', 'NA', 'NA')
snook_prek2_october[[24]] <- c('24', 'NA', 'NA', 'NA', 'NA')
snook_prek2_october[[25]] <- c('25', 'NA', 'NA', 'NA', 'NA')

# create dataframe
snook_prek2_october <-   plyr::ldply(snook_prek2_october)
snook_prek2_october <- snook_prek2_october[1:3]
colnames(snook_prek2_october) <-  c('Day', 'Abs', 'Mem')
snook_prek2_october[1] <- seq(as.Date("2020/10/01"), by = "day", length.out = 30)


# NOVEMBER

snook_prek_november <- snook_prek2[69:73] %>%
  str_squish() %>%
  strsplit(split = " ")

# create dataframe
snook_prek_november <-   plyr::ldply(snook_prek_november)
snook_prek_november <- snook_prek_november[1:3]
colnames(snook_prek_november) <- c('Day', 'Abs', 'Mem')
snook_prek_november[1] <- seq(as.Date("2020/11/02"), by = "day", length.out = 5)

snook_prek_november

###############################################################################

# BIND ALL PREK DATASETS BY GRADE LEVEL

# 6th Grade

snook_prek_all_dates <- rbind(snook_prek_all_dates,
                              snook_prek2_september2,
                              snook_prek2_october,
                              snook_prek_november)
########################################################################################################

# Remove Unneccesary Data

variables <- ls()
all_dates <- grepl('all_dates', ls())
rm(list = variables[!all_dates])

######################################################################################################


# Adding Columns

floydala_abd_es_fifth_grade_all_dates <- mutate(floydala_abd_es_fifth_grade_all_dates, GradeLevel = '5')
floydala_abd_es_first_grade_all_dates <- mutate(floydala_abd_es_first_grade_all_dates, GradeLevel = '1')
floydala_abd_es_fourth_grade_all_dates <- mutate(floydala_abd_es_fourth_grade_all_dates, GradeLevel = '5')
floydala_abd_es_kinder_grade_all_dates <- mutate(floydala_abd_es_kinder_grade_all_dates, GradeLevel = 'K') 
floydala_abd_es_second_grade_all_dates <- mutate(floydala_abd_es_second_grade_all_dates, GradeLevel = '2')
floydala_abd_es_third_grade_all_dates <- mutate(floydala_abd_es_third_grade_all_dates, GradeLevel = '3')  
floydala_daep_10th_grade_all_dates <- mutate(floydala_daep_10th_grade_all_dates, GradeLevel = '10')  
floydala_daep_11th_grade_all_dates <- mutate(floydala_daep_11th_grade_all_dates, GradeLevel = '11')     
floydala_daep_5th_grade_all_dates <- mutate(floydala_daep_5th_grade_all_dates, GradeLevel = '5')     
floydala_daep_6th_grade_all_dates <- mutate(floydala_daep_6th_grade_all_dates, GradeLevel = '6')      
floydala_daep_7th_grade_all_dates <- mutate(floydala_daep_7th_grade_all_dates, GradeLevel = '7')     
floydala_early_education_all_dates <- mutate(floydala_early_education_all_dates, GradeLevel = 'EE')     
floydala_hs_10th_grade_all_dates <- mutate(floydala_hs_10th_grade_all_dates, GradeLevel = '10')      
floydala_hs_11th_grade_all_dates <- mutate(floydala_hs_11th_grade_all_dates, GradeLevel = '11')     
floydala_hs_12th_grade_all_dates <- mutate(floydala_hs_12th_grade_all_dates, GradeLevel = '12')      
floydala_hs_9th_grade_all_dates <- mutate(floydala_hs_9th_grade_all_dates, GradeLevel = '9')        
floydala_ms_6th_grade_all_dates <- mutate(floydala_ms_6th_grade_all_dates, GradeLevel = '6')     
floydala_ms_7th_grade_all_dates <- mutate(floydala_ms_7th_grade_all_dates, GradeLevel = '7')        
floydala_ms_8th_grade_all_dates <- mutate(floydala_ms_8th_grade_all_dates, GradeLevel = '8')       
floydala_prek_all_dates <- mutate(floydala_prek_all_dates, GradeLevel = 'PREK')                
snook_es_1st_grade_all_dates <- mutate(snook_es_1st_grade_all_dates, GradeLevel = '1')   
snook_es_2nd_grade_all_dates <- mutate(snook_es_2nd_grade_all_dates, GradeLevel = '2')           
snook_es_3rd_grade_all_dates <- mutate(snook_es_3rd_grade_all_dates, GradeLevel = '3')          
snook_es_4th_grade_all_dates <- mutate(snook_es_4th_grade_all_dates, GradeLevel = '4')           
snook_es_5th_grade_all_dates <- mutate(snook_es_5th_grade_all_dates, GradeLevel = '5')          
snook_es_early_ed_all_dates <- mutate(snook_es_early_ed_all_dates, GradeLevel = 'EE')            
snook_es_kinder_all_dates <- mutate(snook_es_kinder_all_dates, GradeLevel = 'K')             
snook_prek_all_dates <- mutate(snook_prek_all_dates, GradeLevel = 'PREK')                  
snook_secondary_10th_grade_all_dates <- mutate(snook_secondary_10th_grade_all_dates, GradeLevel = '10')  
snook_secondary_11th_grade_all_dates <- mutate(snook_secondary_11th_grade_all_dates, GradeLevel = '11')   
snook_secondary_12th_grade_all_dates <- mutate(snook_secondary_12th_grade_all_dates, GradeLevel = '12')  
snook_secondary_6th_grade_all_dates <- mutate(snook_secondary_6th_grade_all_dates, GradeLevel = '6')    
snook_secondary_7th_grade_all_dates <- mutate(snook_secondary_7th_grade_all_dates, GradeLevel = '7')   
snook_secondary_8th_grade_all_dates <- mutate(snook_secondary_8th_grade_all_dates, GradeLevel = '8')    
snook_secondary_9th_grade_all_dates <- mutate(snook_secondary_9th_grade_all_dates, GradeLevel = '9')


floydala_abd_es_fifth_grade_all_dates <- mutate(floydala_abd_es_fifth_grade_all_dates, School = 'A B Duncan Collegiate Elementary')
floydala_abd_es_first_grade_all_dates <- mutate(floydala_abd_es_first_grade_all_dates, School = 'A B Duncan Collegiate Elementary')
floydala_abd_es_fourth_grade_all_dates <- mutate(floydala_abd_es_fourth_grade_all_dates, School = 'A B Duncan Collegiate Elementary')
floydala_abd_es_kinder_grade_all_dates <- mutate(floydala_abd_es_kinder_grade_all_dates, School = 'A B Duncan Collegiate Elementary') 
floydala_abd_es_second_grade_all_dates <- mutate(floydala_abd_es_second_grade_all_dates, School = 'A B Duncan Collegiate Elementary')
floydala_abd_es_third_grade_all_dates <- mutate(floydala_abd_es_third_grade_all_dates, School = 'A B Duncan Collegiate Elementary')  
floydala_daep_10th_grade_all_dates <- mutate(floydala_daep_10th_grade_all_dates, School = 'D.A.E.P')  
floydala_daep_11th_grade_all_dates <- mutate(floydala_daep_11th_grade_all_dates, School = 'D.A.E.P')     
floydala_daep_5th_grade_all_dates <- mutate(floydala_daep_5th_grade_all_dates, School = 'D.A.E.P')     
floydala_daep_6th_grade_all_dates <- mutate(floydala_daep_6th_grade_all_dates, School = 'D.A.E.P')      
floydala_daep_7th_grade_all_dates <- mutate(floydala_daep_7th_grade_all_dates, School = 'D.A.E.P')     
floydala_early_education_all_dates <- mutate(floydala_early_education_all_dates, School = 'A B Duncan Collegiate Elementary')     
floydala_hs_10th_grade_all_dates <- mutate(floydala_hs_10th_grade_all_dates, School = 'Floydada Collegiate High')      
floydala_hs_11th_grade_all_dates <- mutate(floydala_hs_11th_grade_all_dates, School = 'Floydada Collegiate High')     
floydala_hs_12th_grade_all_dates <- mutate(floydala_hs_12th_grade_all_dates, School = 'Floydada Collegiate High')      
floydala_hs_9th_grade_all_dates <- mutate(floydala_hs_9th_grade_all_dates, School = 'Floydada Collegiate High')        
floydala_ms_6th_grade_all_dates <- mutate(floydala_ms_6th_grade_all_dates, School = 'Floydada Collegiate Junior High')     
floydala_ms_7th_grade_all_dates <- mutate(floydala_ms_7th_grade_all_dates, School = 'Floydada Collegiate Junior High')        
floydala_ms_8th_grade_all_dates <- mutate(floydala_ms_8th_grade_all_dates, School = 'Floydada Collegiate Junior High')       
floydala_prek_all_dates <- mutate(floydala_prek_all_dates, School = 'A B Duncan Collegiate Elementary')                
snook_es_1st_grade_all_dates <- mutate(snook_es_1st_grade_all_dates, School = 'Snook Elementary')   
snook_es_2nd_grade_all_dates <- mutate(snook_es_2nd_grade_all_dates, School = 'Snook Elementary')           
snook_es_3rd_grade_all_dates <- mutate(snook_es_3rd_grade_all_dates, School = 'Snook Elementary')          
snook_es_4th_grade_all_dates <- mutate(snook_es_4th_grade_all_dates, School = 'Snook Elementary')           
snook_es_5th_grade_all_dates <- mutate(snook_es_5th_grade_all_dates, School = 'Snook Elementary')          
snook_es_early_ed_all_dates <- mutate(snook_es_early_ed_all_dates, School = 'Snook Elementary')            
snook_es_kinder_all_dates <- mutate(snook_es_kinder_all_dates, School = 'Snook Elementary')             
snook_prek_all_dates <- mutate(snook_prek_all_dates, School = 'Snook Elementary')                  
snook_secondary_10th_grade_all_dates <- mutate(snook_secondary_10th_grade_all_dates, School = 'Snook Secondary')  
snook_secondary_11th_grade_all_dates <- mutate(snook_secondary_11th_grade_all_dates, School = 'Snook Secondary')   
snook_secondary_12th_grade_all_dates <- mutate(snook_secondary_12th_grade_all_dates, School = 'Snook Secondary')  
snook_secondary_6th_grade_all_dates <- mutate(snook_secondary_6th_grade_all_dates, School = 'Snook Secondary')    
snook_secondary_7th_grade_all_dates <- mutate(snook_secondary_7th_grade_all_dates, School = 'Snook Secondary')   
snook_secondary_8th_grade_all_dates <- mutate(snook_secondary_8th_grade_all_dates, School = 'Snook Secondary')    
snook_secondary_9th_grade_all_dates <- mutate(snook_secondary_9th_grade_all_dates, School = 'Snook Secondary')



ls()


floydada_district <- rbind(floydala_abd_es_fifth_grade_all_dates,
                           floydala_abd_es_first_grade_all_dates,  
                           floydala_abd_es_fourth_grade_all_dates,
                           floydala_abd_es_kinder_grade_all_dates, 
                           floydala_abd_es_second_grade_all_dates,
                           floydala_abd_es_third_grade_all_dates,
                           floydala_daep_10th_grade_all_dates,   
                           floydala_daep_11th_grade_all_dates,     
                           floydala_daep_5th_grade_all_dates,    
                           floydala_daep_6th_grade_all_dates,      
                           floydala_daep_7th_grade_all_dates,     
                           floydala_early_education_all_dates,     
                           floydala_hs_10th_grade_all_dates,      
                           floydala_hs_11th_grade_all_dates,      
                           floydala_hs_12th_grade_all_dates,     
                           floydala_hs_9th_grade_all_dates,       
                           floydala_ms_6th_grade_all_dates,      
                           floydala_ms_7th_grade_all_dates,       
                           floydala_ms_8th_grade_all_dates,       
                           floydala_prek_all_dates)  


snook_district <- rbind(snook_es_1st_grade_all_dates,          
                        snook_es_2nd_grade_all_dates,            
                        snook_es_3rd_grade_all_dates,          
                        snook_es_4th_grade_all_dates,           
                        snook_es_5th_grade_all_dates,          
                        snook_es_early_ed_all_dates,          
                        snook_es_kinder_all_dates,            
                        snook_prek_all_dates,                  
                        snook_secondary_10th_grade_all_dates,  
                        snook_secondary_11th_grade_all_dates,   
                        snook_secondary_12th_grade_all_dates,  
                        snook_secondary_6th_grade_all_dates,    
                        snook_secondary_7th_grade_all_dates,   
                        snook_secondary_8th_grade_all_dates,    
                        snook_secondary_9th_grade_all_dates) 



                                # Done w/ PDF
##############################################################################
##############################################################################
floydada_district$Mem <- as.numeric(floydada_district$Mem)
floydada_district$Abs <- as.numeric(floydada_district$Abs)

snook_district$Mem <- as.numeric(snook_district$Mem)
snook_district$Abs <- as.numeric(snook_district$Abs)

floydada_district <- mutate(floydada_district, District = 'FLOYDADA ISD',
                            County = 'Floyd',
                            AttendancePercent = round((((Mem-Abs)/Mem)*100), digits =1),
                            AbsentPercent = round((Abs/Mem) * 100, digits = 1))
snook_district <- mutate(snook_district, District = 'SNOOK ISD',
                         County = 'Burleson',
                         AttendancePercent = round((((Mem-Abs)/Mem)*100), digits =1),
                         AbsentPercent = round((Abs/Mem) * 100, digits = 1))


FloydadaGradeLevelData <- floydada_district
SnookGradeLevelData <- snook_district

#############################################################################################################

# Remove Unneccesary Data

variables <- ls()
data <- grepl('Grade', ls())
rm(list = variables[!data])

######################################################################################################
##################################################################################

# Datasets to join later by Zip and District

# ZIP CODE
tea_schools <- read.csv('.\\Current_Schools.csv')
tea_schools <-tea_schools %>% select('School_Nam','School_Zip','District_N','Grade_Leve')
tea_schools$School_Nam <- tolower(tea_schools$School_Nam)
as.data.frame(tea_schools)
re_from <- "\\b([[:alpha:]])([[:alpha:]]+)"
gsub(re_from, "\\U\\1\\E\\2" ,tea_schools$School, perl=TRUE)
tea_schools <- tea_schools %>% rename(School = School_Nam, DistrictNumber = District_N, GradeLevel = 'Grade_Leve')
tea_schools$School <- gsub('el$', 'Elementary', tea_schools$School)
tea_schools$School <- gsub('h s', 'High School', tea_schools$School)
tea_schools$School <- gsub('middle$', 'Middle School', tea_schools$School)
tea_schools$School <-gsub('m s$', 'Middle School', tea_schools$School)
tea_schools$School <- gsub('middleschool', 'Middle School', tea_schools$School)
tea_schools$School <- gsub('Middl', 'Middle School', tea_schools$School)
tea_schools$School <- gsub('j h', 'Junior High', tea_schools$School)
tea_schools$School <- gsub('Jh', 'Junior High', tea_schools$School)
tea_schools$School <- gsub('School School$', 'School', tea_schools$School)
tea_schools$School <- gsub('Schoole', 'School', tea_schools$School)
tea_schools$School <- gsub(re_from, "\\U\\1\\E\\2" ,tea_schools$School, perl=TRUE)
tea_schools$DistrictNumber <- gsub("[[:punct:]]", "",tea_schools$DistrictNumber)


# Metro vs Non-Metro
metro_type <- read_excel('.\\PHR_MSA_County_masterlist.xlsx')
metro_type <-metro_type %>% select('County Name','Metro Area (82)') %>%
  rename(County = 'County Name',
         MetroStatus = 'Metro Area (82)')

# District, TEA Description, NCES Description, District Number 
district_type <- read_excel('.\\District_Type1819.xlsx', sheet = 3)
district_type <- district_type %>%
  select('District','TEA Description','NCES Description','District Number') %>%
  rename(NCES_Description = 'NCES Description',
         TEA_Description = 'TEA Description',
         DistrictNumber = 'District Number')

###################################################################################
#                                                                                 #
#         GRADE LEVEL DATA --> SCHOOL LEVEL DATA --> DISTRICT LEVEL DATA          #
#                                                                                 #
#                                                                                 #
###################################################################################

###################################################################################
#                                                                                 #
#                              GRADE LEVEL DATA                                   #
#                                                                                 #
#                               - Floydada                                        #
#                               - Snook                                           #
#                               - Pasadena                                        #
#                                                                                 #
###################################################################################


# Floydada & Snook are complete

# Pasadena :

PasadenaGradeLevelData <- read_excel(".\\pasadena_attendance_data2.xlsx")
PasadenaGradeLevelData$Day <- as.Date(PasadenaGradeLevelData$Day, format = "%m/%d/%Y")
PasadenaGradeLevelData <- mutate(PasadenaGradeLevelData, District = 'PASADENA ISD',
                   County = 'Harris',
                   AbsentPercent = round((Abs/Mem) * 100, digits = 1))

# Grade Level Dataset Final Join:

GradeLevelDataset <- full_join(PasadenaGradeLevelData, FloydadaGradeLevelData[-5]) %>%
  full_join(SnookGradeLevelData[-5]) %>%
  left_join(metro_type, by = 'County') %>%
  left_join(district_type, by='District') %>%
  mutate(Weekday = weekdays(as.Date(Day))) %>%
  relocate(Day,Weekday,District, DistrictNumber, County,
           TEA_Description,NCES_Description, 
           MetroStatus, GradeLevel, GradeLevel,
           Mem, Abs, AttendancePercent, AbsentPercent)


###################################################################################
#                                                                                 #
#                             SCHOOL LEVEL DATA                                   #
#                                                                                 #
#                               - Floydada                                        #
#                               - Snook                                           #
#                               - CyFair                                          #
#                               - Dallas                                          #                                      #
#                               - Klein                                           #
#                                                                                 #
#                                                                                 #
###################################################################################

# Floydada

FloydadaSchoolLevelData <- FloydadaGradeLevelData %>%
  group_by(School, Day) %>%
  summarize(Abs = sum(Abs, na.rm=T), Mem = sum(Mem, na.rm=T)) %>%
  mutate(AttendancePercent = ((Mem - Abs) / Mem) * 100,
         AbsentPercent = (Abs / Mem) * 100,
         District = 'FLOYDADA ISD',
         County = 'Floyd')
?sum
# Snook

SnookSchoolLevelData <- SnookGradeLevelData %>%
  group_by(School, Day) %>%
  summarize(Abs = sum(Abs), Mem = sum(Mem)) %>%
  mutate(AttendancePercent = ((Mem - Abs) / Mem) * 100,
         AbsentPercent = (Abs / Mem) * 100,
         District = 'SNOOK ISD',
         County = 'Burleson')
  
# CyFair


cyfair_enroll <- read_excel('.\\cyfair_enrollment.xlsx')
cyfair_attend <- read_excel('.\\cyfair_attendance.xlsx')
school_names <- colnames(cyfair_enroll)[-1]
colnames(cyfair_attend) <- NULL 
colnames(cyfair_enroll) <- NULL
SchoolCyfair <- list()

for (i in 1:length(cyfair_attend)) {
  SchoolCyfair[[i]] <- cyfair_attend[,c(1,i)]
  
}

SchoolCyfair <- SchoolCyfair[-1]

for (i in 1:length(cyfair_attend)) {
  SchoolCyfair[[i]] <- cbind(SchoolCyfair[[i]],
                             school_names[i])
  
}

for (i in 1:length(cyfair_enroll)) {
  SchoolCyfair[[i]] <- cbind(SchoolCyfair[[i]],
                             cyfair_enroll[,i+1])
  
}

CyfairSchoolLevelData <- do.call("rbind", SchoolCyfair)%>% 
  rename(Day = 1,
         AttendancePercent = 2,
         School = `school_names[i]`,
         Mem = `cyfair_enroll[, i + 1]`) %>%
  mutate(AttendancePercent = AttendancePercent * 100,
         AbsentPercent = 100 - AttendancePercent,
         District = 'CYPRESS-FAIRBANKS ISD',
         County = 'Harris',
         School = gsub("\\s*\\([^\\)]+\\)","",as.character(School)))

# Dallas

DallasSchoolLevelData <- read_excel(".\\dallas_data.xlsx", sheet = 4) %>%
  select(-c(1,5,6)) %>%
  mutate(SchoolType = gsub("\\s*\\([^\\)]+\\)","",as.character(SCHOOLTYPE)),
         AttendancePercent = PCT * 100,
         AbsentPercent = 100 - AttendancePercent,
         Day = as.Date(ATT_DATE),
         District = 'DALLAS ISD',
         County = 'Dallas',) %>%
  rename(School = SCHOOL) %>%
  select(-c(2:4))

dallas_schools <- DallasSchoolLevelData$SchoolType 

# Klein

klein_enroll <- read_excel('.\\klein_enrollment_data.xlsx')
klein_attend <- read_excel('.\\klen_attendance_data.xlsx')
school_names <- colnames(klein_attend)[-1]
colnames(klein_attend) <- NULL 
colnames(klein_enroll) <- NULL 
SchoolKlein <- list()

for (i in 1:length(klein_attend)) {
  SchoolKlein[[i]] <- klein_attend[,c(1,i)]
  
}
SchoolKlein <- SchoolKlein[-1]
for (i in 1:length(klein_attend)) {
  SchoolKlein[[i]] <- cbind(SchoolKlein[[i]],
                            school_names[i])
  
}
for (i in 1:49) {
  SchoolKlein[[i]] <- cbind(SchoolKlein[[i]],
                            t(klein_enroll[i,-1]))
}

KleinSchoolLevelData <- do.call("rbind", SchoolKlein) %>% 
  rename(Day = 1,
         AttendancePercent = 2,
         School = `school_names[i]`,
         Mem = 4) %>%
  mutate(District = 'KLEIN ISD',
         County = 'Harris',
         AbsentPercent = 100 - AttendancePercent,
         School = gsub('[[:digit:]]+', '', School))
  



tea_schools$School <- gsub('HS', 'High School', tea_schools$School)
tea_schools$School <- gsub('MS', 'Intermediate', tea_schools$School)
tea_schools$School <- gsub('ES', 'Elementary', tea_schools$School)
tea_schools$School <- gsub('ES', 'Elementary', tea_schools$School)
tea_schools$School <- gsub('Int$', 'Intermediate', tea_schools$School)
KleinSchoolLevelData$School <-  str_trim(KleinSchoolLevelData$School)
KleinSchoolLevelData <- left_join(KleinSchoolLevelData, district_type, by = 'District') %>% 
  left_join(metro_type, by = 'County') %>%
  left_join(tea_schools[-3], by = 'School') %>%
  rename(SchoolType = GradeLevel) %>%
  mutate(Weekday = weekdays(as.Date(Day)))
KleinSchoolLevelData[KleinSchoolLevelData$School == 'Krimmel Intermediate',]$School_Zip <- '77379'
KleinSchoolLevelData[KleinSchoolLevelData$School == 'McDougle Elementary',]$School_Zip <- '77086'
KleinSchoolLevelData[KleinSchoolLevelData$School == 'The Grace England EC/Pre-K Ctr',]$School_Zip <- '77086'
KleinSchoolLevelData[KleinSchoolLevelData$School == 'Fox Elementary',]$School_Zip <- '77388'
KleinSchoolLevelData[KleinSchoolLevelData$School == 'Klein Annex - Alt Ed Program',]$School_Zip <- '77379'
KleinSchoolLevelData[KleinSchoolLevelData$School == 'Krimmel Intermediate',]$SchoolType <- 'Middle School'
KleinSchoolLevelData[KleinSchoolLevelData$School == 'McDougle Elementary',]$SchoolType <- 'Elementary'
KleinSchoolLevelData[KleinSchoolLevelData$School == 'The Grace England EC/Pre-K Ctr',]$SchoolType <- 'Elementary'
KleinSchoolLevelData[KleinSchoolLevelData$School == 'Fox Elementary',]$SchoolType <- 'Elementary'
KleinSchoolLevelData[KleinSchoolLevelData$School == 'Klein Annex - Alt Ed Program',]$SchoolType <- 'Elementary'
tea_schools$School <- gsub('High School', 'HS', tea_schools$School)
tea_schools$School <- gsub('Intermediate', 'MS', tea_schools$School)
tea_schools$School <- gsub('Elementary', 'ES', tea_schools$School)

# Join School Level Data

SchoolLevelDataset <- full_join(FloydadaSchoolLevelData, SnookSchoolLevelData) %>%
  full_join(CyfairSchoolLevelData[-4]) %>%
  left_join(metro_type, by = 'County') %>%
  left_join(district_type, by = 'District') %>%
  left_join(tea_schools[-3], by = 'School') %>%
  select(-c(3,4)) %>%
  rename(SchoolType = GradeLevel) %>%
  mutate(Weekday = weekdays(as.Date(Day))) %>%
  relocate(Day,Weekday, School, SchoolType, 
           School_Zip, District, DistrictNumber, County,
           TEA_Description,NCES_Description, 
           MetroStatus, AttendancePercent, AbsentPercent) %>%
  full_join(KleinSchoolLevelData[-4])



# Fill in Missing School Data
SchoolLevelDataset[SchoolLevelDataset$School == 'Ault Elementary',]$School_Zip <- '77429-5722'
SchoolLevelDataset[SchoolLevelDataset$School == 'Ault Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Bane Elementary',]$School_Zip <- '77040-5459'
SchoolLevelDataset[SchoolLevelDataset$School == 'Bane Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Bang Elementary',]$School_Zip <- '77064-7137'
SchoolLevelDataset[SchoolLevelDataset$School == 'Bang Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Birkes Elementary',]$School_Zip <- '77095'
SchoolLevelDataset[SchoolLevelDataset$School == 'Birkes Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Black Elementary',]$School_Zip <- '77060-4199'
SchoolLevelDataset[SchoolLevelDataset$School == 'Black Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Bridgeland High School',]$School_Zip <- '77433'
SchoolLevelDataset[SchoolLevelDataset$School == 'Bridgeland High School',]$SchoolType <- 'High School'
SchoolLevelDataset[SchoolLevelDataset$School == 'Copeland Elementary',]$School_Zip <- '77095-4441'
SchoolLevelDataset[SchoolLevelDataset$School == 'Copeland Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cy-Fair High School',]$School_Zip <- '77429'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cy-Fair High School',]$SchoolType <- 'High School'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cypress Creek High School',]$School_Zip <- '77070-4501'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cypress Creek High School',]$SchoolType <- 'High School'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cypress Falls High School',]$School_Zip <- '77095-2307'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cypress Falls High School',]$SchoolType <- 'High School'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cypress Lakes High School',]$School_Zip <- '77449'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cypress Lakes High School',]$SchoolType <- 'High School'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cypress Park High School',]$School_Zip <- '77433-0188'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cypress Park High School',]$SchoolType <- 'High School'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cypress Ranch High School',]$School_Zip <- '77433'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cypress Ranch High School',]$SchoolType <- 'High School'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cypress Springs High School',]$School_Zip <- '77433-3240'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cypress Springs High School',]$SchoolType <- 'High School'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cypress Woods High School',]$School_Zip <- '77429'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cypress Woods High School',]$SchoolType <- 'High School'
SchoolLevelDataset[SchoolLevelDataset$School == 'Danish Elementary',]$School_Zip <- '77065'
SchoolLevelDataset[SchoolLevelDataset$School == 'Danish Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Duryea Elementary',]$School_Zip <- '77449'
SchoolLevelDataset[SchoolLevelDataset$School == 'Duryea Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Emery Elementary',]$School_Zip <- '77449'
SchoolLevelDataset[SchoolLevelDataset$School == 'Emery Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Emmott Elementary',]$School_Zip <- '77065-4366'
SchoolLevelDataset[SchoolLevelDataset$School == 'Emmott Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Farney Elementary',]$School_Zip <- '77429'
SchoolLevelDataset[SchoolLevelDataset$School == 'Farney Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Fiest Elementary',]$School_Zip <- '77095-4618'
SchoolLevelDataset[SchoolLevelDataset$School == 'Fiest Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Francone Elementary',]$School_Zip <- '77064-4551'
SchoolLevelDataset[SchoolLevelDataset$School == 'Francone Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Frazier Elementary',]$School_Zip <- '77064-7904'
SchoolLevelDataset[SchoolLevelDataset$School == 'Frazier Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Gleason Elementary',]$School_Zip <- '77064'
SchoolLevelDataset[SchoolLevelDataset$School == 'Gleason Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Hairgrove Elementary',]$School_Zip <- '77041-1883'
SchoolLevelDataset[SchoolLevelDataset$School == 'Hairgrove Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Hancock Elementary',]$School_Zip <- '77070-2628'
SchoolLevelDataset[SchoolLevelDataset$School == 'Hancock Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Hemmenway Elementary',]$School_Zip <- '77449'
SchoolLevelDataset[SchoolLevelDataset$School == 'Hemmenway Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Holbrook Elementary',]$School_Zip <- '77092-1006'
SchoolLevelDataset[SchoolLevelDataset$School == 'Holbrook Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Holmsley Elementary',]$School_Zip <- '77095-1149'
SchoolLevelDataset[SchoolLevelDataset$School == 'Holmsley Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Hoover Elementary',]$School_Zip <- '77449'
SchoolLevelDataset[SchoolLevelDataset$School == 'Hoover Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Horne Elementary',]$School_Zip <- '77084-1523'
SchoolLevelDataset[SchoolLevelDataset$School == 'Horne Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Jersey Village High School',]$School_Zip <- '77040'
SchoolLevelDataset[SchoolLevelDataset$School == 'Jersey Village High School',]$SchoolType <- 'High School'
SchoolLevelDataset[SchoolLevelDataset$School == 'Jowell Elementary',]$School_Zip <- '77040-2134'
SchoolLevelDataset[SchoolLevelDataset$School == 'Jowell Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Keith Elementary',]$School_Zip <- '77433'
SchoolLevelDataset[SchoolLevelDataset$School == 'Keith Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Kirk Elementary',]$School_Zip <- '77041'
SchoolLevelDataset[SchoolLevelDataset$School == 'Kirk Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Lamkin Elementary',]$School_Zip <- '77429-3386'
SchoolLevelDataset[SchoolLevelDataset$School == 'Lamkin Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Langham Creek High School',]$School_Zip <- '77095-1004'
SchoolLevelDataset[SchoolLevelDataset$School == 'Langham Creek High School',]$SchoolType <- 'High School'
SchoolLevelDataset[SchoolLevelDataset$School == 'Lee Elementary',]$School_Zip <- '77041'
SchoolLevelDataset[SchoolLevelDataset$School == 'Lee Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Lieder Elementary',]$School_Zip <- '77084-2510'
SchoolLevelDataset[SchoolLevelDataset$School == 'Lieder Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Lowery Elementary',]$School_Zip <- '77095-2612'
SchoolLevelDataset[SchoolLevelDataset$School == 'Lowery Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Matzke Elementary',]$School_Zip <- '77070-4438'
SchoolLevelDataset[SchoolLevelDataset$School == 'Matzke Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Metcalf Elementary',]$School_Zip <- '77084-6400'
SchoolLevelDataset[SchoolLevelDataset$School == 'Metcalf Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Millsap Elementary',]$School_Zip <- '77429-3281'
SchoolLevelDataset[SchoolLevelDataset$School == 'Millsap Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Moore Elementary',]$School_Zip <- '77070-2814'
SchoolLevelDataset[SchoolLevelDataset$School == 'Moore Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Owens Elementary',]$School_Zip <- '77095-2901'
SchoolLevelDataset[SchoolLevelDataset$School == 'Owens Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Pope Elementary',]$School_Zip <- '77433'
SchoolLevelDataset[SchoolLevelDataset$School == 'Pope Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Post Elementary',]$School_Zip <- '77040-2121'
SchoolLevelDataset[SchoolLevelDataset$School == 'Post Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Postma Elementary',]$School_Zip <- '77433'
SchoolLevelDataset[SchoolLevelDataset$School == 'Postma Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Reed Elementary',]$School_Zip <- '77040-2400'
SchoolLevelDataset[SchoolLevelDataset$School == 'Reed Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Rennell Elementary',]$School_Zip <- '77433'
SchoolLevelDataset[SchoolLevelDataset$School == 'Rennell Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Robison Elementary',]$School_Zip <- '77449'
SchoolLevelDataset[SchoolLevelDataset$School == 'Robison Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Sampson Elementary',]$School_Zip <- '77429'
SchoolLevelDataset[SchoolLevelDataset$School == 'Sampson Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Sheridan Elementary',]$School_Zip <- '77449'
SchoolLevelDataset[SchoolLevelDataset$School == 'Sheridan Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Swenke Elementary',]$School_Zip <- '77433'
SchoolLevelDataset[SchoolLevelDataset$School == 'Swenke Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Tipps Elementary',]$School_Zip <- '77084'
SchoolLevelDataset[SchoolLevelDataset$School == 'Tipps Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Walker Elementary',]$School_Zip <- '77449'
SchoolLevelDataset[SchoolLevelDataset$School == 'Walker Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Warner Elementary',]$School_Zip <- '77433'
SchoolLevelDataset[SchoolLevelDataset$School == 'Warner Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Wells Elementary',]$School_Zip <- '77433'
SchoolLevelDataset[SchoolLevelDataset$School == 'Wells Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Willbern Elementary',]$School_Zip <- '77064-9419'
SchoolLevelDataset[SchoolLevelDataset$School == 'Willbern Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Wilson Elementary',]$School_Zip <- '77084-5722'
SchoolLevelDataset[SchoolLevelDataset$School == 'Wilson Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Woodard Elementary',]$School_Zip <- '77433'
SchoolLevelDataset[SchoolLevelDataset$School == 'Woodard Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Yeager Elementary',]$School_Zip <- '77069-1801'
SchoolLevelDataset[SchoolLevelDataset$School == 'Yeager Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cypress Ridge High School',]$School_Zip <- '77041'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cypress Ridge High School',]$SchoolType <- 'High School'
SchoolLevelDataset[SchoolLevelDataset$School == 'Hamilton Elementary',]$School_Zip <- '77429-2446'
SchoolLevelDataset[SchoolLevelDataset$School == 'Hamilton Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Blackshear Elementary' & SchoolLevelDataset$DistrictNumber == '101915',]$School_Zip <- '77375'

SchoolLevelDataset[SchoolLevelDataset$School == 'Adam Elementary',]$School_Zip <- '77065'
SchoolLevelDataset[SchoolLevelDataset$School == 'Adam Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Snook Elementary',]$School_Zip <- '77878-0087'
SchoolLevelDataset[SchoolLevelDataset$School == 'Snook Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Andre Elementary',]$School_Zip <- '77433'
SchoolLevelDataset[SchoolLevelDataset$School == 'Andre Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Anthony Middle School',]$School_Zip <- '77433'
SchoolLevelDataset[SchoolLevelDataset$School == 'Aragon Middle School',]$School_Zip <- '77095'
SchoolLevelDataset[SchoolLevelDataset$School == 'Arnold Middle School',]$School_Zip <- '77429'
SchoolLevelDataset[SchoolLevelDataset$School == 'Bleyl Middle School',]$School_Zip <- '77070-4612'
SchoolLevelDataset[SchoolLevelDataset$School == 'Campbell Middle School',]$School_Zip <- '77064-3001'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cook Middle School',]$School_Zip <- '77064-7044'
SchoolLevelDataset[SchoolLevelDataset$School == 'Dean Middle School',]$School_Zip <- '77040-5438'
SchoolLevelDataset[SchoolLevelDataset$School == 'Goodson Middle School',]$School_Zip <- '77429'
SchoolLevelDataset[SchoolLevelDataset$School == 'Hamilton Elementary',]$School_Zip <- '77429-2446'
SchoolLevelDataset[SchoolLevelDataset$School == 'Hopper Middle School',]$School_Zip <- '77433'
SchoolLevelDataset[SchoolLevelDataset$School == 'Kahla Middle School',]$School_Zip <- '77084'
SchoolLevelDataset[SchoolLevelDataset$School == 'Labay Middle School',]$School_Zip <- '77095-1703'
SchoolLevelDataset[SchoolLevelDataset$School == 'Hamilton Middle School',]$School_Zip <- '77429-2452'
SchoolLevelDataset[SchoolLevelDataset$School == 'M Robinson Elementary',]$School_Zip <- '77429'
SchoolLevelDataset[SchoolLevelDataset$School == 'McFee Elementary',]$School_Zip <- '77449'
SchoolLevelDataset[SchoolLevelDataset$School == 'Rowe Middle School',]$School_Zip <- '77433'
SchoolLevelDataset[SchoolLevelDataset$School == 'Salyards Middle School',]$School_Zip <- '77269-2003'
SchoolLevelDataset[SchoolLevelDataset$School == 'Smith Middle School',]$School_Zip <- '77433'
SchoolLevelDataset[SchoolLevelDataset$School == 'Spillane Middle School',]$School_Zip <- '77429'
SchoolLevelDataset[SchoolLevelDataset$School == 'Thornton Middle School',]$School_Zip <- '77449-7004'
SchoolLevelDataset[SchoolLevelDataset$School == 'Truitt Middle School',]$School_Zip <- '77084-1520'
SchoolLevelDataset[SchoolLevelDataset$School == 'Watkins Middle School',]$School_Zip <- '77084-2554'
SchoolLevelDataset[SchoolLevelDataset$School == 'Anthony Middle School',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$School == 'Aragon Middle School',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$School == 'Arnold Middle School',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$School == 'Bleyl Middle School',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$School == 'Campbell Middle School',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$School == 'Cook Middle School',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$School == 'Dean Middle School',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$School == 'Goodson Middle School',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$School == 'Hamilton Middle School',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$School == 'Hopper Middle School',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$School == 'Kahla Middle School',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$School == 'Labay Middle School',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$School == 'M Robinson Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'McFee Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Rowe Middle School',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$School == 'Salyards Middle School',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$School == 'Smith Middle School',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$School == 'Spillane Middle School',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$School == 'Thornton Middle School',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$School == 'Truitt Middle School',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$School == 'Watkins Middle School',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$District=='FLOYDADA ISD',]$School_Zip <- '79235'
SchoolLevelDataset[SchoolLevelDataset$School == 'A B Duncan Collegiate Elementary',]$SchoolType <- 'Elementary'
SchoolLevelDataset[SchoolLevelDataset$School == 'D.A.E.P',]$SchoolType <- 'Elementary/Secondary'
SchoolLevelDataset[SchoolLevelDataset$School == 'Floydada Collegiate High',]$SchoolType <- 'High'
SchoolLevelDataset[SchoolLevelDataset$School == 'Floydada Collegiate Junior High',]$SchoolType <- 'Middle'
SchoolLevelDataset[SchoolLevelDataset$School == 'Leonard MS' & SchoolLevelDataset$DistrictNumber == '220905',]$SchoolType <- '76116'
SchoolLevelDataset[SchoolLevelDataset$School == 'Leonard MS',]$SchoolType <- 'Middle'
tea_schools$School <- gsub('hs$', 'High School', tea_schools$School)
tea_schools$School <- gsub('Elementary', 'Elementary School', tea_schools$School)
tea_schools$School <- gsub('hs', 'High School', tea_schools$School)
tea_schools$School <- gsub('Elementary', 'Elementary School', tea_schools$School)
tea_schools$School <- gsub('School School', 'lol', tea_schools$School)
tea_schools$School <- gsub('lol', 'School', tea_schools$School)
tea_schools$School <- str_to_title(tea_schools$School)
tea_schools$School <- gsub('Daep', 'DAEP', tea_schools$School)
tea_schools$School <- gsub('Of', 'of', tea_schools$School)
tea_schools$School <- gsub('And', 'and', tea_schools$School)
tea_schools$School <- gsub('Hs$', 'High School', tea_schools$School)
tea_schools$School <- gsub('Es$', 'Elementary School', tea_schools$School)
tea_schools$School <- gsub('Ms$', 'Middle School', tea_schools$School)
tea_schools$School <- gsub('High$', 'High School', tea_schools$School)




DallasChunk <- left_join(DallasSchoolLevelData, metro_type, by = 'County') %>%
  left_join(district_type, by = 'District') %>%
  mutate(Weekday = weekdays(as.Date(Day))) %>%
  left_join(tea_schools, by = c("School", "DistrictNumber")) %>%
  select(-14)

SchoolLevelDataset<- full_join(SchoolLevelDataset, DallasChunk)

SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='New Tech High School at B.F. Darrell',]$School_Zip <- '75216'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Lincoln Humanities/Communications Magnet High Sch',]$School_Zip <- '75215-4797' 
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Barbara M. Manns Middle School for the DAEP',]$School_Zip <- '75224'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Townview Science & Engineering',]$School_Zip <- '75203'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Barbara M. Manns High School for the DAEP',]$School_Zip <- '75224'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Townview Business & Management',]$School_Zip <- '75203'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Booker T Washington HS for Performing & Visual Arts',]$School_Zip <- '75201'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Townview Health Professions',]$School_Zip <- '75203'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Rosie Sorrells Education and Social Services HS',]$School_Zip <- '75203-2545'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Judge Barefoot Sanders Law Magnet at Townview',]$School_Zip <- '75203-2545'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Townview Talented & Gifted',]$School_Zip <- '75203'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=="Young Men's Leadership Acad at Fred F Florence MS",]$School_Zip <- '75217-3228'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Benjamin Franklin International Exploratory Academy',]$School_Zip <-'75230-5333'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Thomas C Marsh Preparatory Academy',]$School_Zip <- '75244-7198'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='E D Walker Middle School',]$School_Zip <- '75230'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Raul Quintanilla Sr Middle School Stem Academy',]$School_Zip <- '057905'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Francisco Pancho Medrano Jr High School',]$School_Zip <- '75220'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Trinidad Garza Early College at Mt View',]$School_Zip <- '75211'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Zan Wesley Holmes Jr. Middle School',]$School_Zip <- '75233'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='PreK Partnership Center',]$School_Zip <- '75228'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='William M Anderson Elementary School',]$School_Zip <- '75217-4111'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Jose Joe May Elementary School',]$School_Zip <- '75220'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Chapel Hill Preparatory',]$School_Zip <- '75234-6563'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Martin Luther King Jr Arts Academy',]$School_Zip <- '75215-3475'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='George B. Dealey Montessori Academy',]$School_Zip <- '75230-4174'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Jill Stone Elementary School at Vickery Meadow',]$School_Zip <- '75231'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Hall Personalized Learning Academy at Oak Cliff',]$School_Zip <- '75211'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Hogg New Tech Center',]$School_Zip <- '75208'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Houston Personalized Learning',]$School_Zip <- '75219-3447'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='H.I. Holland Elementary School at Lisbon',]$School_Zip <- '75216-6458'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Thomas L Marsalis Elementary STEAM Academy',]$School_Zip <- '75241-1999'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Mount Auburn STEAM Academy',]$School_Zip <- '75241-1999'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='KB Polk Center for Academically Talented & Gifted',]$School_Zip <- '75209-3640'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Joseph J Rhoads Elementary School',]$School_Zip <- '75210-2712'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='William B. Travis Academy for Talented and Gifted',]$School_Zip <- '75204-2414'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Ronald E McNair Elementary School',]$School_Zip <- '75237-3606'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Maria Moreno STEAM Academy',]$School_Zip <- '75224'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Mary McLeod Bethune Elementary School',]$School_Zip <- '75211-6530'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Leonides Gonzalez Cigarroa MD Elementary School',]$School_Zip <- '75220'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Esperanza Hope Medrano Elementary School',]$School_Zip <- '75219'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Lee A McShan Jr Elementary School',]$School_Zip <- '75231'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Felix G Botello Personalized Learning Elementary',]$School_Zip <- '75203'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Arlington Park Elementary School',]$School_Zip <- '75235'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Solar Prep School for Girls at James B Bonham',]$School_Zip <- '75206'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Eddie Bernice Johnson Elementary School',]$School_Zip <- '75215'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=="Young Women's STEAM Academy at Balch Springs MS",]$School_Zip <- '75180'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Ann Richards STEAM Academy',]$School_Zip <- '75227'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='D A Hulcy STEAM Middle School',]$School_Zip <- '75232'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Ida B. Wells Academy',]$School_Zip <- '75201'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Ignite Middle School at J. W. Ray',]$School_Zip <- '75204'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='School for the Talented and Gifted in Pleasant Grove',]$School_Zip <- '75217'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Barack Obama Male Leadership Academy at A. Maceo Smith',]$School_Zip <- '75241'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='CityLab High School',]$School_Zip <- '75201'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Sudie L Williams TAG Academy',]$School_Zip <- '75209'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Montessori Academy at Onesimo Hernandez',]$School_Zip <- '75235'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Solar Prep for Boys at John F. Kennedy',]$School_Zip <- '75206'
SchoolLevelDataset[SchoolLevelDataset$District == 'DALLAS ISD' & SchoolLevelDataset$School=='Juvenile Justice AEP',]$School_Zip <- '057905'



###################################################################################
#                                                                                 #
#                           DISTRICT LEVEL DATA                                   #
#                                                                                 #
#                               - Conroe                                          #
#                               - CyFair                                          #
#                               - Floydada                                        #
#                               - Pasadena                                        #                                      #
#                               - Snook                                           #
#                               - Socorro                                         #
#                               - Klein                                           #
#                               - Garland                                         #
#                               - Dallas                                          #
#                                                                                 #
#                                                                                 #
###################################################################################


# Conroe

ConroeDistrictLevelData <- read_excel('.\\conroe_attendance_daily2.xlsx') %>%
  mutate(Abs = paste(Mon, Tue, Wed, Thu, Fri)) %>%
  mutate(Abs = as.numeric(gsub("[^0-9.]","", Abs)),
         AbsentPercent = 100 - AttendancePercent) %>%
  rename(Day = 'Absent Date',
         Mem = 'Active Students') %>%
  select(-c(2:6)) %>%
  mutate(County = 'Montgomery',
         District = 'CONROE ISD')

ConroeDistrictLevelData <- ConroeDistrictLevelData[-1,]


# CyFair

CyfairDistrictLevelData <- CyfairSchoolLevelData %>%
  mutate(Abs = Mem * (AttendancePercent * 0.01)) %>%
  group_by(Day) %>%
  summarize(Mem = sum(Mem),
            Abs = Mem - sum(Abs)) %>%
  mutate(AbsentPercent = (Abs / Mem) * 100,
         AttendancePercent = ((Mem - Abs) / Mem) * 100,
         District = 'CYPRESS-FAIRBANKS ISD',
         County = 'Harris')

# Floydada

FloydadaDistrictLevelData <- FloydadaSchoolLevelData %>%
  group_by(Day) %>%
  summarize(Mem = sum(Mem, na.rm = T),
            Abs = sum(Abs, na.rm = T)) %>%
  mutate(AttendancePercent = ((Mem - Abs) / Mem) * 100,
         AbsentPercent = (Abs / Mem) * 100,
         District = 'FLOYDADA ISD',
         County = 'Floyd')

# Pasadena

PasadenaDistrictLevelData <- PasadenaGradeLevelData %>%
  group_by(Day) %>%
  summarize(Mem = sum(Mem),
            Abs = sum(Abs)) %>%
  mutate(AttendancePercent = ((Mem - Abs) / Mem) * 100,
         AbsentPercent = (Abs / Mem) * 100,
         District = 'PASADENA ISD',
         County = 'Harris')

# Snook

SnookDistrictLevelData <- SnookSchoolLevelData %>%
  group_by(Day) %>%
  summarize(Mem = sum(Mem),
            Abs = sum(Abs)) %>%
  mutate(AttendancePercent = ((Mem - Abs) / Mem) * 100,
         AbsentPercent = (Abs / Mem) * 100,
         District = 'SNOOK ISD',
         County = 'Burleson')

# Socorro

SocorroDistrictLevelData <- read_excel(".\\socorro_daily_district_attendance2.xlsx") %>%
  rename(Day = 'Date', AttendancePercent = 'ADA',
         Abs = 'Absent', Mem = 'Enrollment') %>%
  mutate(AttendancePercent = AttendancePercent * 100,
        AbsentPercent = 100 - AttendancePercent,
        County = 'El Paso',
        District = 'SOCORRO ISD')

# Klein

KleinDistrictLevelData <- KleinSchoolLevelData %>%
  mutate(Abs = Mem * (AttendancePercent * 0.01)) %>%
  group_by(Day) %>%
  summarize(Mem = sum(Mem),
            Abs = Mem - sum(Abs)) %>%
  mutate(AttendancePercent = ((Mem - Abs) / Mem) * 100,
         AbsentPercent = (Abs / Mem) * 100,
         District = 'KLEIN ISD',
         County = 'Harris')

# Garland

DistrictGarland <- pdf_text(".\\garland_ada_summary.pdf") %>%
  readr::read_lines()
DistrictGarland <- DistrictGarland[c(6:33,35:68)] %>%
  str_squish() %>%
  strsplit(split = " ") 

GarlandDistrictLevelData <- plyr::ldply(DistrictGarland) %>%
  rename(Day = V2, Mem = V4, AttendancePercent = V6) %>%
  select(-c(1,3,5)) %>%
  mutate(Day = as.Date(Day, '%m/%d'),
         AbsentPercent = 100 - as.numeric(AttendancePercent),
         Abs = as.numeric(Mem) * (as.numeric(AttendancePercent) * 0.01)) %>%
  mutate(Mem = as.numeric(Mem),
         Abs = Mem - Abs,
         AttendancePercent = as.numeric(AttendancePercent),
         District = 'GARLAND ISD',
         County = 'Dallas')



# Dallas

DallasDistrictLevelData <- read_excel(".\\dallas_data.xlsx", sheet = 4) %>%
  rename(School = SCHOOL,
         Mem = STDCNT) %>%
  mutate(SchoolType = gsub("\\s*\\([^\\)]+\\)","",as.character(SCHOOLTYPE)),
         AttendancePercent = PCT * 100,
         AbsentPercent = 100 - AttendancePercent,
         Day = as.Date(ATT_DATE),
         Abs = Mem - PRECENTCNT) %>%
  select(-c(1,3)) %>%
  group_by(Day) %>%
  summarize(Mem = sum(Mem), Abs = sum(Abs)) %>%
  mutate(District = 'DALLAS ISD',
         County = 'Dallas',
         AttendancePercent = ((Mem - Abs) / Mem) * 100,
         AbsentPercent = (Abs / Mem) * 100)
            

# Join District Level Data

DistrictLevelDataset <- full_join(ConroeDistrictLevelData, CyfairDistrictLevelData) %>%
  full_join(FloydadaDistrictLevelData) %>%
  full_join(PasadenaDistrictLevelData) %>%
  full_join(SnookDistrictLevelData) %>%
  full_join(SocorroDistrictLevelData) %>%
  full_join(KleinDistrictLevelData) %>%
  full_join(GarlandDistrictLevelData) %>%
  full_join(DallasDistrictLevelData) %>%
  left_join(metro_type, by = 'County') %>%
  left_join(district_type, by = 'District') %>%
  mutate(Weekday = weekdays(as.Date(Day))) %>%
  relocate(Day ,Weekday, District, DistrictNumber, 
           Mem, Abs, AttendancePercent, AbsentPercent, County,
           TEA_Description,NCES_Description, MetroStatus)



###################################################################################
#                                                                                 #
#                           UPDATE W/ FORTWORTH                                   #                                        #
#                                                                                 #
#                                                                                 #
###################################################################################


# Schools in this dataset came with different number of days. Therefore two sets of dates were generated.


set1 <- ConroeDistrictLevelData[1]
set2 <- as.data.frame(seq(as.Date("2020/11/03"), by = "day", length.out = 4)) %>%
  rename(Day = 1)
dateset2 <- rbind(set1,set2) %>% rename(value = 1) %>% mutate(Day = 1:57)
dateset1 <- SchoolKlein[[1]][1]

fortworth <- read_excel(".\\fort_worth_data.xlsx") %>%
  mutate(AttendancePercent = (round(((Mem-Abs)/Mem)*100, digits = 1)),
         AbsentPercent = round((Abs/Mem) * 100, digits = 1))

School_over_46 <- fortworth %>% filter(Day > 46) %>% distinct(School)
over_46 <- fortworth %>% filter(School == as.character(School_over_46[1,1]) | 
                                  School == as.character(School_over_46[2,1]) |
                                  School == as.character(School_over_46[3,1]))
under_46 <- fortworth %>% filter(School != as.character(School_over_46[1,1]) & 
                                   School != as.character(School_over_46[2,1]) &
                                   School != as.character(School_over_46[3,1]))
over_46 <- left_join(over_46,dateset2, by='Day')

school_dates <- as.data.frame(dateset1) %>%
  mutate(Day = 1:46,) %>%
  rename(value = '1')
under_46 <- left_join(under_46,school_dates, by='Day')
fortworth <- full_join(over_46, under_46)

temp <- as.data.frame(as.character(sort(unique(fortworth$Grade)))) %>%
  mutate(GradeLevel = as.character(c('1','2','3','4','5','6','7','8','9','10','11','12','EE','K','PREK'))) %>%
  rename(Grade = 1, GradeLevel = 2)

FortworthGradeLevelData <- fortworth %>% 
  select(-1) %>%
  rename(Day = value) %>%
  mutate(School = gsub('[[:digit:]]+', '', School %>%
                         str_replace_all("[[:punct:]]", "")),
         County = 'Tarrant',
         District = 'FORT WORTH ISD') %>%
  left_join(temp, by = "Grade") %>%
  select(-2) %>%
  left_join(metro_type, by = 'County') %>%
  left_join(district_type, by='District')

# Finalize Grade Level Dataset

GradeLevelDataset <- GradeLevelDataset %>%
  full_join(FortworthGradeLevelData[-1]) %>%
  mutate(Weekday = weekdays(as.Date(Day)))

# Finalize School Level Dataset

tea_schools$School <- gsub('High School', 'HS', tea_schools$School)
tea_schools$School <- gsub('Middle School', 'MS', tea_schools$School)
tea_schools$School <- gsub('Elementary', 'ES', tea_schools$School)
tea_schools$School <- gsub(' School', '', tea_schools$School)
tea_schools$School <- gsub('Hs', 'HS', tea_schools$School)
tea_schools$School <- gsub('Ms', 'MS', tea_schools$School)
tea_schools$School <- gsub('Es', 'ES', tea_schools$School)
tea_schools$School <- gsub('Jh', 'JH', tea_schools$School)
tea_schools$School <- gsub('H S', 'HS', tea_schools$School)
tea_schools$School <- gsub('n s', "n's", tea_schools$School)
tea_schools$School <- gsub('Junior High', 'JH', tea_schools$School)
tea_schools$School <- gsub('Daep', 'DAEP', tea_schools$School)

FortworthSchoolLevelData <- FortworthGradeLevelData %>%
  group_by(School, Day) %>%
  summarize(Abs = sum(Abs), Mem = sum(Mem)) %>%
  mutate(AttendancePercent = ((Mem - Abs) / Mem) * 100,
         AbsentPercent = (Abs / Mem) * 10,
         District = 'FORT WORTH ISD',
         County = 'Tarrant') %>%
  left_join(tea_schools[-3], by = 'School') %>%
  left_join(metro_type, by = 'County') %>%
  left_join(district_type, by='District')

FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Alice Carlson ALC',]$School_Zip <- '76109-3091'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Alice D Contreras ES',]$School_Zip <- '76115'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Amon CarterRiverside HS',]$School_Zip <- '76111'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Applied Learning Academy MS',]$School_Zip <- '76116-3010'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Assessment Center',]$School_Zip <- '76107-3010'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Atwood McDonald ES',]$School_Zip <- '76112-4299'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Benbrook Middle High School MSHS',]$School_Zip <- '76126-3010'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Bill J  Elliott ES',]$School_Zip <- '76120-6211'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Bonnie Brae ES',]$School_Zip <- '76111'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Boulevard Heights School',]$School_Zip <- '76107-2994'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Cesar Chavez  ES',]$School_Zip <- '76106'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Charles E Nash ES',]$School_Zip <- '76102-2395'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Childrens Medical Center',]$School_Zip <- '76107-3010'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Como Montessori K MS',]$School_Zip <- '76107-6930'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'D McRae ES',]$School_Zip <- '76105'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Daggett Montessori K MS',]$School_Zip <- '76110'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'David K  Sellars ES',]$School_Zip <- '76119-7498'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Detention Center',]$School_Zip <- '76111-3091'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Diamond HillJarvis HS',]$School_Zip <- '76106-4596'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'E M Daggett ES',]$School_Zip <- '76110-2627'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'EM Daggett MS',]$School_Zip <- '76110-1999'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Edward J Briscoe ES',]$School_Zip <- '76104-7052'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'George CClarke ES',]$School_Zip <- '76110-6799'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Glencrest th Grade MS',]$School_Zip <- '76119-4699'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Green BTrimble Technical HS',]$School_Zip <- '76104'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'HV Helbing  ES',]$School_Zip <- '76106-4420'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Hubbard Heights ES',]$School_Zip <- '76115-2355'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'IM Terrell Academy for STEM  VPA',]$School_Zip <- '76102'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'International Newcomer Acad MS',]$School_Zip <- '76116-8837'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'JP Elder MS',]$School_Zip <- '76164'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Jean McClung MS',]$School_Zip <- '76112-3010'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'JJAEP Pathways II',]$School_Zip <- '76107'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Jo Kelly School',]$School_Zip <- '76107-1169'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'John T  White ES',]$School_Zip <- '76120'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Lily B  Clayton ES',]$School_Zip <- '76110-1299'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Lowery Road ES',]$School_Zip <- '76120'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'MG Ellis  Primary PreKK',]$School_Zip <- '76164'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'MH Moore ES',]$School_Zip <- '76106-4699'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Mary Louise Phillips ES',]$School_Zip <- '76116'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Maudrie M Walton ES',]$School_Zip <- '76112'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'McLean th Grade MS',]$School_Zip <- '76109'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Middle Level Learning Center',]$School_Zip <- '76116'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Milton LKirkpatrick ES',]$School_Zip <- '76106'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'North Side HS',]$School_Zip <- '76164'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Oscar Dean Wyatt HS',]$School_Zip <- '76119-5598'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Overton Park ES',]$School_Zip <- '76109'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Paul Laurence Dunbar HS',]$School_Zip <- '76112'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'PK Satellite Centers',]$School_Zip <- '76107'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'RLPaschal HS',]$School_Zip <- '76110'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Richard J  Wilson ES',]$School_Zip <- '76110-6199'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Riverside ALC',]$School_Zip <- '76111'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Rosemont th Grade MS',]$School_Zip <- '76115'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Rufino Mendoza ES',]$School_Zip <- '76164-9098'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'SS Dillow ES',]$School_Zip <- '76105-3536'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Success High School',]$School_Zip <- '76104-2895'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'SunriseMcMillan ES',]$School_Zip <- '76119-1732'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'TA Sims ES',]$School_Zip <- '76105-4019'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Tarrant Co College SouthFort Worth Collegiate H S',]$School_Zip <- '76107'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Transition Ctr ',]$School_Zip <- '76107'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Van ZandtGuinn ES',]$School_Zip <- '76104-1466'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Versia L Williams ES',]$School_Zip <- '76111'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'WA Meacham MS',]$School_Zip <- '76106'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'WC Stripling MS',]$School_Zip <- '76107'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'WJ Turner ES',]$School_Zip <- '76106'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'WM Green ES',]$School_Zip <- '76119'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'WP McLean MS',]$School_Zip <- '76109'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Wedgwood th Grade MS',]$School_Zip <- '76133'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Western Hills ES ',]$School_Zip <- '76116'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Western Hills Primary PreK',]$School_Zip <- '76116'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'William James MS',]$School_Zip <- '76105'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'William Monnig MS',]$School_Zip <- '76116'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Young Mens Leadership Academy MS',]$School_Zip <- '76105'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Young Womens Leadership Academy',]$School_Zip <- '76104'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Morningside',]$School_Zip <- '76104'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Meadowbrook ES',]$School_Zip <- '76103'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'De Zavala ES',]$School_Zip <- '76104'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Benbrook ES',]$School_Zip <- '76126'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'AMPate ES',]$School_Zip <- '76126'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Christene CMoss ES',]$School_Zip <- '76119-2998'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'JTStevens ES',]$School_Zip <- '76133-3599'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Maude ILogan ES',]$School_Zip <- '76105-3898'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Texas Academy Of Biomedical',]$School_Zip <- '76107-3010'

FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Charles E Nash ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Childrens Medical Center',]$GradeLevel <- 'Elementary/Secondary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Como Montessori K MS',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'D McRae ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Daggett Montessori K MS',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Detention Center',]$GradeLevel <- 'Elementary/Secondary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Diamond HillJarvis HS',]$GradeLevel <- 'High School'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'E M Daggett ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'EM Daggett MS',]$GradeLevel <- 'Middle'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Edward J Briscoe ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'George CClarke ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Glencrest th Grade MS',]$GradeLevel <- 'Middle'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Green BTrimble Technical HS',]$GradeLevel <- 'High School'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'HV Helbing  ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Hubbard Heights ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'IM Terrell Academy for STEM  VPA',]$GradeLevel <- 'High School'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'International Newcomer Acad MS',]$GradeLevel <- 'Middle'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'JP Elder MS',]$GradeLevel <- 'Middle'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Jean McClung MS',]$GradeLevel <- 'Middle'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'JJAEP Pathways II',]$GradeLevel <- 'Elementary/Secondary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Jo Kelly School',]$GradeLevel <- 'Elementary/Secondary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Lowery Road ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'MG Ellis  Primary PreKK',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'MH Moore ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Mary Louise Phillips ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Maudrie M Walton ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'McLean th Grade MS',]$GradeLevel <- 'Middle'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Middle Level Learning Center',]$GradeLevel <- 'Middle'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Milton LKirkpatrick ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'North Side HS',]$GradeLevel <- 'High School'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Oscar Dean Wyatt HS',]$GradeLevel <- 'High School'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Overton Park ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Paul Laurence Dunbar HS',]$GradeLevel <- 'High School'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'PK Satellite Centers',]$GradeLevel <- 'Middle'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'RLPaschal HS',]$GradeLevel <- 'High School'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Rosemont th Grade MS',]$GradeLevel <- 'Middle'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Rufino Mendoza ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Success High School',]$GradeLevel <- 'High School'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'SunriseMcMillan ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Van Zandt Guinn ESTarrant Co College South Fort Worth Collegiate H S',]$GradeLevel <- ''
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Versia L Williams ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'WA Meacham MS',]$GradeLevel <- 'Middle'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'WC Stripling MS',]$GradeLevel <- 'Middle'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'WP McLean MS',]$GradeLevel <- 'Middle'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Wedgwood th Grade MS',]$GradeLevel <- 'Middle'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Western Hills Primary PreK',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'William James MS',]$GradeLevel <- 'Middle'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'William Monnig MS',]$GradeLevel <- 'Middle'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Young Mens Leadership Academy MS',]$GradeLevel <- 'Elementary/Secondary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Young Womens Leadership Academy',]$GradeLevel <- 'Elementary/Secondary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Alice Carlson ALC',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Alice D Contreras ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Amon Carter Riverside HS',]$GradeLevel <- 'High School'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Applied Learning Academy MS',]$GradeLevel <- 'Middle'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Assessment Center',]$GradeLevel <- 'Elementary/Secondary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Atwood McDonald ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Benbrook Middle High School MSHS',]$GradeLevel <- 'High School'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Bonnie Brae ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Boulevard Heights School',]$GradeLevel <- 'High School'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Riverside ALC',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Tarrant Co College SouthFort Worth Collegiate H S',]$GradeLevel <- 'High School'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Van ZandtGuinn ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Amon CarterRiverside HS',]$GradeLevel <- 'High School'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'AMPate ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Cesar Chavez  ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Christene CMoss ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'JTStevens ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Maude ILogan ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'SS Dillow ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'TA Sims ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Texas Academy Of Biomedical',]$GradeLevel <- 'High School'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'Western Hills ES ',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'WJ Turner ES',]$GradeLevel <- 'Elementary'
FortworthSchoolLevelData[FortworthSchoolLevelData$School == 'WM Green ES',]$GradeLevel <- 'Elementary'

FortworthSchoolLevelData <- FortworthSchoolLevelData %>%
  rename(SchoolType = GradeLevel)

SchoolLevelDataset <- SchoolLevelDataset %>%
  full_join(FortworthSchoolLevelData[-c(3,4)]) %>%
  mutate(Weekday = weekdays(as.Date(Day)),
         School_Zip = substr(School_Zip, 0, 5))

# Finalize District Level Data


FortworthDistrictLevelData <- FortworthSchoolLevelData %>%
  group_by(Day) %>%
  summarize(Mem = sum(Mem),
            Abs = sum(Abs)) %>%
  mutate(AttendancePercent = ((Mem - Abs) / Mem) * 100,
         AbsentPercent = (Abs / Mem) * 100,
         District = 'FORT WORTH ISD',
         County = 'Tarrant') %>%
  left_join(metro_type, by = 'County') %>%
  left_join(district_type, by = 'District') %>%
  mutate(Weekday = weekdays(as.Date(Day)))


DistrictLevelDataset <- DistrictLevelDataset %>%
  full_join(FortworthDistrictLevelData)

###############################################################################
###############################################################################

##############################################################################
##############################################################################

# Updates


# Katy Dataset 12/07/2020

katy_enroll <- read_excel('.\\katy_data2.xlsx')
katy_attend <- read_excel('.\\katy_data2.xlsx', sheet = 2)
school_names <- colnames(katy_attend)[-1]
colnames(katy_attend) <- NULL 
colnames(katy_enroll) <- NULL 
SchoolKaty <- list()



for (i in 1:length(katy_attend)) {
  SchoolKaty[[i]] <- katy_attend[,c(1,i)]
  
}
SchoolKaty <- SchoolKaty[-1]
for (i in 1:length(katy_attend)) {
  SchoolKaty[[i]] <- cbind(SchoolKaty[[i]],
                           school_names[i])
  
}
for (i in 1:69) {
  SchoolKaty[[i]] <- cbind(SchoolKaty[[i]],
                           t(katy_enroll[i,-1]))
}

KatySchoolLevelData <- do.call("rbind", SchoolKaty) %>% 
  rename(Day = 1,
         AttendancePercent = 2,
         School = `school_names[i]`,
         Mem = 4) %>%
  mutate(District = 'KATY ISD',
         County = 'Harris',
         AttendancePercent = AttendancePercent * 100,
         AbsentPercent = 100 - AttendancePercent,
         School = gsub('[[:digit:]]+', '', School))




tea_schools$School <- gsub('Junior HS$', 'Junior High', tea_schools$School)
tea_schools$School <- gsub('HS$', 'High', tea_schools$School)
tea_schools$School <- gsub('ES$', 'Elementary', tea_schools$School)
tea_schools$School <- trimws(tea_schools$School)

KatySchoolLevelData <- left_join(KatySchoolLevelData, metro_type, by = 'County') %>%
  left_join(district_type, by = 'District') %>%
  mutate(Weekday = weekdays(as.Date(Day))) %>%
  left_join(tea_schools[-3], by = c("School"))

KatySchoolLevelData[KatySchoolLevelData$School == 'Beck Junior High',]$School_Zip <- '77450'
KatySchoolLevelData[KatySchoolLevelData$School == 'Beck Junior High',]$GradeLevel <- 'High'
KatySchoolLevelData[KatySchoolLevelData$School == 'Cinco Ranch HS',]$School_Zip <- '77494'
KatySchoolLevelData[KatySchoolLevelData$School == 'Cinco Ranch HS',]$GradeLevel <- 'High'
KatySchoolLevelData[KatySchoolLevelData$School == 'Creech Elementary',]$School_Zip <- '77450'
KatySchoolLevelData[KatySchoolLevelData$School == 'Creech Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'Davidson Elementary',]$School_Zip <- '77494'
KatySchoolLevelData[KatySchoolLevelData$School == 'Davidson Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'Exley Elementary',]$School_Zip <- '101914'
KatySchoolLevelData[KatySchoolLevelData$School == 'Exley Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'Fielder Elementary',]$School_Zip <- '77494'
KatySchoolLevelData[KatySchoolLevelData$School == 'Fielder Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'Golbow Elementary',]$School_Zip <- '77449'
KatySchoolLevelData[KatySchoolLevelData$School == 'Golbow Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'Hayes Elementary',]$School_Zip <- '77450'
KatySchoolLevelData[KatySchoolLevelData$School == 'Hayes Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'Hutsell Elementary',]$School_Zip <- ''
KatySchoolLevelData[KatySchoolLevelData$School == 'Hutsell Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'Jordan HS',]$School_Zip <- '77493'
KatySchoolLevelData[KatySchoolLevelData$School == 'Jordan HS',]$GradeLevel <- 'High'
KatySchoolLevelData[KatySchoolLevelData$School == 'Katy HS',]$School_Zip <- '77494'
KatySchoolLevelData[KatySchoolLevelData$School == 'Katy HS',]$GradeLevel <- 'High'
KatySchoolLevelData[KatySchoolLevelData$School == 'Kilpatrick Elementary',]$School_Zip <- '77494'
KatySchoolLevelData[KatySchoolLevelData$School == 'Kilpatrick Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'King Elementary',]$School_Zip <- '77493'
KatySchoolLevelData[KatySchoolLevelData$School == 'King Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'Mayde Creek HS',]$School_Zip <- '77084'
KatySchoolLevelData[KatySchoolLevelData$School == 'Mayde Creek HS',]$GradeLevel <- 'High'
KatySchoolLevelData[KatySchoolLevelData$School == 'McDonald Junior High',]$School_Zip <- '77449'
KatySchoolLevelData[KatySchoolLevelData$School == 'McDonald Junior High',]$GradeLevel <- 'High'
KatySchoolLevelData[KatySchoolLevelData$School == 'McElwain Elementary',]$School_Zip <- '77493'
KatySchoolLevelData[KatySchoolLevelData$School == 'McElwain Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'McMeans Junior High',]$School_Zip <- '77450'
KatySchoolLevelData[KatySchoolLevelData$School == 'McMeans Junior High',]$GradeLevel <- 'Middle'
KatySchoolLevelData[KatySchoolLevelData$School == 'McRoberts Elementary',]$School_Zip <- '77449'
KatySchoolLevelData[KatySchoolLevelData$School == 'McRoberts Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'Morton Ranch HS',]$School_Zip <- '77449'
KatySchoolLevelData[KatySchoolLevelData$School == 'Morton Ranch HS',]$GradeLevel <- 'High'
KatySchoolLevelData[KatySchoolLevelData$School == 'Opportunity Awareness Center',]$School_Zip <- '77493'
KatySchoolLevelData[KatySchoolLevelData$School == 'Opportunity Awareness Center',]$GradeLevel <- 'High'
KatySchoolLevelData[KatySchoolLevelData$School == 'Paetow HS',]$School_Zip <- '77493'
KatySchoolLevelData[KatySchoolLevelData$School == 'Paetow HS',]$GradeLevel <- 'High'
KatySchoolLevelData[KatySchoolLevelData$School == 'Pattison Elementary',]$School_Zip <- '77450'
KatySchoolLevelData[KatySchoolLevelData$School == 'Pattison Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'Rhoads Elementary',]$School_Zip <- '77449'
KatySchoolLevelData[KatySchoolLevelData$School == 'Rhoads Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'Rylander Elementary',]$School_Zip <- '77494'
KatySchoolLevelData[KatySchoolLevelData$School == 'Rylander Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'Schmalz Elementary',]$School_Zip <- '77084'
KatySchoolLevelData[KatySchoolLevelData$School == 'Schmalz Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'Seven Lakes HS',]$School_Zip <- '77494'
KatySchoolLevelData[KatySchoolLevelData$School == 'Seven Lakes HS',]$GradeLevel <- 'High'
KatySchoolLevelData[KatySchoolLevelData$School == 'Shafer Elementary',]$School_Zip <- '77494'
KatySchoolLevelData[KatySchoolLevelData$School == 'Shafer Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'Stanley Elementary',]$School_Zip <- ''
KatySchoolLevelData[KatySchoolLevelData$School == 'Stanley Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'Taylor HS',]$School_Zip <- '77494'
KatySchoolLevelData[KatySchoolLevelData$School == 'Taylor HS',]$GradeLevel <- 'High'
KatySchoolLevelData[KatySchoolLevelData$School == 'Tompkins HS',]$School_Zip <- '77494'
KatySchoolLevelData[KatySchoolLevelData$School == 'Tompkins HS',]$GradeLevel <- 'High'
KatySchoolLevelData[KatySchoolLevelData$School == 'Winborn Elementary',]$School_Zip <- '77449'
KatySchoolLevelData[KatySchoolLevelData$School == 'Winborn Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'Wolfe Elementary',]$School_Zip <- '77079'
KatySchoolLevelData[KatySchoolLevelData$School == 'Wolfe Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'Wolman Elementary',]$School_Zip <- '77494'
KatySchoolLevelData[KatySchoolLevelData$School == 'Wolman Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'WoodCreek Elementary',]$School_Zip <- '77494'
KatySchoolLevelData[KatySchoolLevelData$School == 'WoodCreek Elementary',]$GradeLevel <- 'Elementary'
KatySchoolLevelData[KatySchoolLevelData$School == 'WoodCreek Junior High',]$School_Zip <- '77494'
KatySchoolLevelData[KatySchoolLevelData$School == 'WoodCreek Junior High',]$GradeLevel <- 'Middle'

KatySchoolLevelData <- KatySchoolLevelData %>% rename(SchoolType = GradeLevel)

SchoolLevelDataset <- SchoolLevelDataset %>%
  full_join(KatySchoolLevelData[-4])




KatyDistrictLevelData <- KatySchoolLevelData %>%
  mutate(Abs = Mem * (AttendancePercent * 0.01)) %>%
  group_by(Day) %>%
  summarize(Mem = sum(Mem),
            Abs = Mem - sum(Abs)) %>%
  mutate(AttendancePercent = ((Mem - Abs) / Mem) * 100,
         AbsentPercent = (Abs / Mem) * 100,
         District = 'KATY ISD',
         County = 'Harris') %>%
  left_join(metro_type, by = 'County') %>%
  left_join(district_type, by = 'District') %>%
  mutate(Weekday = weekdays(as.Date(Day)))

DistrictLevelDataset <- full_join(DistrictLevelDataset, KatyDistrictLevelData)



###############################################################################
###############################################################################

# Remove Unneccesary Data

variables <- ls()
all_data <- grepl('Level', ls())
rm(list = variables[!all_data])

##############################################################################
##############################################################################


# Converting Cleaning School Type
SchoolLevelDataset$SchoolType <- gsub(' School', '', SchoolLevelDataset$SchoolType)
SchoolLevelDataset$SchoolType <- gsub('Junior High', 'Middle', SchoolLevelDataset$SchoolType)
SchoolLevelDataset$SchoolType <- trimws(SchoolLevelDataset$SchoolType)
unique(SchoolLevelDataset$SchoolType)
SchoolLevelDataset$SchoolType <- as.factor(SchoolLevelDataset$SchoolType)



SchoolLevelDataset$TEA_Description <- as.factor(SchoolLevelDataset$TEA_Description)
SchoolLevelDataset$NCES_Description <- as.factor(SchoolLevelDataset$NCES_Description)
SchoolLevelDataset$MetroStatus <- as.factor(SchoolLevelDataset$MetroStatus)
SchoolLevelDataset$Day <- as.Date(SchoolLevelDataset$Day)
SchoolLevelDataset$Weekday <- factor(SchoolLevelDataset$Weekday, levels = c('Monday', 'Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))

GradeLevelDataset$TEA_Description <- as.factor(GradeLevelDataset$TEA_Description)
GradeLevelDataset$NCES_Description <- as.factor(GradeLevelDataset$NCES_Description)
GradeLevelDataset$MetroStatus <- as.factor(GradeLevelDataset$MetroStatus)
GradeLevelDataset$GradeLevel <- as.factor(GradeLevelDataset$GradeLevel)
GradeLevelDataset$Day <- as.Date(GradeLevelDataset$Day)
GradeLevelDataset$Weekday <- factor(GradeLevelDataset$Weekday, levels = c('Monday', 'Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
GradeLevelDataset$GradeLevel <- factor(GradeLevelDataset$GradeLevel, levels = c('EE','PREK','K','1','2','3','4','5','6','7','8','9','10','11','12'))


DistrictLevelDataset$Day <- as.Date(DistrictLevelDataset$Day)
DistrictLevelDataset$TEA_Description <- as.factor(DistrictLevelDataset$TEA_Description)
DistrictLevelDataset$NCES_Description <- as.factor(DistrictLevelDataset$NCES_Description)
DistrictLevelDataset$MetroStatus <- as.factor(DistrictLevelDataset$MetroStatus)
DistrictLevelDataset$Weekday <- factor(DistrictLevelDataset$Weekday, levels = c('Monday', 'Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))



##############################################################################################
##############################################################################################

# 04/03/2021

# Last touches on zip codes in school level dataset



# Remove extra digits after dash in zip code
SchoolLevelDataset$School_Zip <-  gsub('-.*', "", SchoolLevelDataset$School_Zip)


#unique(SchoolLevelDataset$School)
#unique(SchoolLevelDataset$District)

# Identify proper names of schools to fix
#unique(SchoolLevelDataset[SchoolLevelDataset$District == "KATY ISD",]$School)
#unique(SchoolLevelDataset[SchoolLevelDataset$District == "FORT WORTH ISD",]$School)

# Fort Worth ISD

SchoolLevelDataset[SchoolLevelDataset$District == "FORT WORTH ISD" & SchoolLevelDataset$School == 'Leonard MS',]$School_Zip <- '76116'
SchoolLevelDataset[SchoolLevelDataset$District == "FORT WORTH ISD" & SchoolLevelDataset$School == 'Morningside ES',]$School_Zip <- '76104'
SchoolLevelDataset[SchoolLevelDataset$District == "FORT WORTH ISD" & SchoolLevelDataset$School == 'Riverside MS',]$School_Zip <- '76111'
SchoolLevelDataset[SchoolLevelDataset$District == "FORT WORTH ISD" & SchoolLevelDataset$School == 'Southwest HS',]$School_Zip <- '76133'
SchoolLevelDataset[SchoolLevelDataset$District == "FORT WORTH ISD" & SchoolLevelDataset$School == 'Woodway ES',]$School_Zip <- '76133'


# Katy ISD
SchoolLevelDataset[SchoolLevelDataset$District == "KATY ISD" & SchoolLevelDataset$School == 'Bryant Elementary',]$School_Zip <- '77423'
SchoolLevelDataset[SchoolLevelDataset$District == "KATY ISD" & SchoolLevelDataset$School == 'Campbell Elementary',]$School_Zip <- '77441'
SchoolLevelDataset[SchoolLevelDataset$District == "KATY ISD" & SchoolLevelDataset$School == 'Bear Creek Elementary',]$School_Zip <- '77084'
SchoolLevelDataset[SchoolLevelDataset$District == "KATY ISD" & SchoolLevelDataset$School == 'Cimarron Elementary',]$School_Zip <- '77450'
SchoolLevelDataset[SchoolLevelDataset$District == "KATY ISD" & SchoolLevelDataset$School == 'Exley Elementary',]$School_Zip <- '77450'
SchoolLevelDataset[SchoolLevelDataset$District == "KATY ISD" & SchoolLevelDataset$School == 'Griffin Elementary',]$School_Zip <- '77494'
SchoolLevelDataset[SchoolLevelDataset$District == "KATY ISD" & SchoolLevelDataset$School == 'Hutsell Elementary',]$School_Zip <- '77493'
SchoolLevelDataset[SchoolLevelDataset$District == "KATY ISD" & SchoolLevelDataset$School == 'Leonard Elementary',]$School_Zip <- '77493'
SchoolLevelDataset[SchoolLevelDataset$District == "KATY ISD" & SchoolLevelDataset$School == 'Stanley Elementary',]$School_Zip <- '77494'
SchoolLevelDataset[SchoolLevelDataset$District == "KATY ISD" & SchoolLevelDataset$School == 'Stephens Elementary',]$School_Zip <- '77449'
SchoolLevelDataset[SchoolLevelDataset$District == "KATY ISD" & SchoolLevelDataset$School == 'Sundown Elementary',]$School_Zip <- '77449'
SchoolLevelDataset[SchoolLevelDataset$District == "KATY ISD" & SchoolLevelDataset$School == 'Wilson Elementary',]$School_Zip <- '77494'
SchoolLevelDataset[SchoolLevelDataset$District == "KATY ISD" & SchoolLevelDataset$School == 'Williams Elementary',]$School_Zip <- '77450'



# Factor grades in grade level dataset
GradeLevelDataset$GradeLevel <- factor(GradeLevelDataset$GradeLevel, level = c("EE", "PREK","K", "1","2","3","4","5","6","7","8",
                                                                "9","10","11","12"))


# Remove Weekends 
GradeLevelDataset <- GradeLevelDataset[!(GradeLevelDataset$Weekday == 'Saturday' | GradeLevelDataset$Weekday == 'Sunday'),]
SchoolLevelDataset <- SchoolLevelDataset[!(SchoolLevelDataset$Weekday == 'Saturday' | SchoolLevelDataset$Weekday == 'Sunday'),]
DistrictLevelDataset <- DistrictLevelDataset[!(DistrictLevelDataset$Weekday == 'Saturday' | DistrictLevelDataset$Weekday == 'Sunday'),]



# Update Round and Truncate District Level Dataset Variables (04/29/2021)

DistrictLevelDataset$Mem <- as.integer(DistrictLevelDataset$Mem)
DistrictLevelDataset$Abs <- as.integer(DistrictLevelDataset$Abs)

DistrictLevelDataset$AttendancePercent <- round(DistrictLevelDataset$AttendancePercent,2)
DistrictLevelDataset$AbsentPercent <- round(DistrictLevelDataset$AbsentPercent,2)

names(DistrictLevelDataset)
DistrictLevelDataset[c(5,6)]

# Also round the variables in School Level Dataset

SchoolLevelDataset$AttendancePercent <- round(SchoolLevelDataset$AttendancePercent,2)
SchoolLevelDataset$AbsentPercent <- round(SchoolLevelDataset$AbsentPercent,2)















