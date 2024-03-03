#install.packages("anomalize")
library(hunspell)
library(tidyverse)
#library(tidyr) Part of "tidyverse"
#library(dplyr) Part of "tidyverse"
#library(ggplot2) Part of "tidyverse"
#library(readr) Part of "tidyverse"
#library(stringr) Part of "tidyverse"
#library(forcats) Part of "tidyverse"
#library(tibble) Part of "tidyverse"
#library(purrr) Part of "tidyverse"
#install.packages("quanteda") # New package Quanteda for text analysis
#install.packages("stopwords") # To download all stopwords 
#install.packages("wordcloud2")
#install.packages("wordcloud")
#install.packages("quanteda.textplots")
#install.packages("tm") Install and load the tm package
library(tm)
library(quanteda.textplots)
library(wordcloud2)
library(quanteda)
library(anomalize)
library(lubridate)

#Read data
df_mh_01 <- readxl::read_csv("https://raw.githubusercontent.com/MaxMad17/mental_health_survey/main/mental_health_survey.csv", 
                                      sheet = "Sheet1", col_names = TRUE)
df_mh_02 <- readxl::read_csv("https://raw.githubusercontent.com/MaxMad17/mental_health_survey/main/mental_health_survey.csv", 
                                         sheet = "Sheet 2", col_names = TRUE)

#Have glimpes of 
glimpse(df_mh_01)

#Create list of df columns for comparison  

col_name <- data.frame(Col_1_name =colnames(df_mh_01), Col_2_name =colnames(df_mh_02))%>%
  mutate(match_col = (Col_1_name == Col_2_name))
# Confirm from match column all TRUE.
summarise(col_name)
## note: all col names are same, therefor can rbind together

#Combining both sheets data in one file:-

df_org<- rbind(df_mh_01,df_mh_02)
#creating a copy for cleaning
df<- df_org
summary(df)
#Improving Columns name foe better understanding, will letters to small case
names(df)<- tolower(gsub("\\s","_",names(df)))

## note: only 02 columns, Datastamp and age confirmed 03 "NA" and rest as charter can not delete all NA.
#1 Data cleaning.
#1.1 Identify Missing values, NA, Data type, etc

# Create a copy of the omitted rows
omitted_rows <- df[complete.cases(df), ]

## Note: identified 27 rows containing NA, proceding to drop from main data
df <- na.omit(df)

## Note: total 1248 rows after dropping 27 . Still Comments column have many NA.
## remoning commnets colum and keeping for text analysis.

#1.2 Dropping "comments" column for text analysis as combined 
df_comments <- data.frame(df$comments)
df_comments <- df_comments%>%
  mutate_all(tolower)
df <- df%>%
  select(-comments)

#1.3 Data Type corrections: All data type fine. Date time stamp need to keep date only.
## Creating a new colum info_date, info_time and deleting timestamp
df <- df %>%
  mutate(
    info_date = as.Date(timestamp),
    info_time = format(as.POSIXct(timestamp), "%H:%M:%S")
  ) %>%
  select(-timestamp)

## Another methord for achive above by seprate function just for reffrence.
#df <-df %>%
#  select(timestamp)%>%
#  separate(timestamp,c("date","time"),"\\s") # just to check type

#Change all the column names to lower case (you may want to try the rename_all function for this) 
df <- df %>%
  mutate_all(tolower)
# Extract date and time components
df$info_date <- as.Date(df$info_date)
df$info_time <- as.POSIXct(df$info_time, format = "%H:%M:%S")
df$info_time <- format(df$info_time, "%H:%M:%S")
  
#1.4 Text Cleaning : Walking every Charter type looking into unique values and correcting it

#Col-2. Gender 
# Convert both columns to lowercase
df$gender <- tolower(trimws(df$gender))
c1 <- data.frame(unique_gender=unique(df$gender)) %>%
  arrange(unique_gender)

#creating list of 42 words based on above unique values in gender
new_value<- c("unknown","unknown","unknown", "unknown","unknown",
              "female","male","male","female","unknown",
              "female","female","female","female","female","female",
              "female","female","male","male","male","male","male",
              "male","male","male","male","male","male","male",
              "male","unknown","unknown","unknown","unknown","unknown",
              "female","female","male","unknown","unknown","female")
# Adding to above c1 data frame for final replacement

c1$new_value <- new_value

# now replace gender in df
# Check for 100% match and replace values in df1
df <- df %>%
  mutate(gender = case_when(
    gender %in% c1$unique_gender ~ c1$new_value[match(gender, c1$unique_gender)],
    TRUE ~ as.character(gender)
  ))

unique(df$gender)

# Col 3 : Country[3]

# Convert both columns to lowercase
df$country <- tolower(trimws(df$country))
c2 <- data.frame(unique_country=unique(df$country)) %>%
  arrange(unique_country)
##note : on identification onlu uk/united kingdom and us/united states need correction
# Replacement list
replace_words <- list(
  "uk" = "united kingdom",
  "us" = "united states"
)
# Function to replace values based on predefined matches
replace_values <- function(column) {
  replacements <- column
  for (pattern in names(replace_words)) {
    replacements[column %in% pattern] <- replace_words[[pattern]]
  }
  replacements
}

# Apply the replacement function to all columns
df[3] <- lapply(df[3], replace_values)
unique(df$country)

# Col 1 = age analysis
#converting age char to numner
df$age <- as.numeric(trimws(df$age))
df_age <- data.frame(table(df$age))
df_age<- df_age %>% rename(age = Var1)
# Method : Using indexing
df <- df[df$age >= 3 & df$age <= 99, ]
## Note: rename colum name and identified values below 3 years and above 99 yars outliers therefor dropping

#2 Creating graph 

#2.1 age group group and gender

ggplot(df, aes(x = gender, y = age)) +
  geom_boxplot() +
  labs(title = "Box Plot of Age by Gender", x = "Gender", y = "Age")

#2.2 age group group and gender

unique(df$family_history)
ggplot(df, aes(x = gender, fill = family_history)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  labs(title = "Family History by Gender",
       x = "Gender", y = "Count") +
  scale_fill_manual(values = c("yes" = "red", "no" = "green", "na" ="yellow"))

#2.3 work_interfere and  gender

unique(df$work_interfere)
ggplot(df, aes(x = gender, fill = work_interfere)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  labs(title = "work_interfere by Gender",
       x = "Gender", y = "Count") +
  scale_fill_manual(values = c("often" = "red", "never" = "green", "rarely" ="yellow", "na"= "gray", "0"="gray"))

# scale_color_manual(values = c("Male" = "blue", "Female" = "pink"))

 ## New Section for text analysis using quanteda - 
 
#T.1 Corpus for combining all text
# renaming col no
names(df_comments) <- c("comments")

# Concatenate the values of text_col into a single paragraph using paste
comb_comments <- corpus(df_comments[complete.cases(df_comments$comments), "comments"])
comb_comments

#T.2 removing stop words
# Example: Adding custom stopwords
custom_stopwords <- c("NA","health", "mental")

# Get the default English stopwords
default_stopwords <- stopwords("en")

# Combine default stopwords with custom stopwords
combined_stopwords <- c(default_stopwords, custom_stopwords)
dtm = dfm(comb_comments, stem=T, remove= combined_stopwords,remove_punct =T)

#T.3 remove any additional spaces or spaces using trim
dtm = dfm_trim(dtm,min_termfreq = 10)
#reached max_ndoc ... 1,253 more documents, reached max_nfeat ... 54 more features

#T.4 To create multiple data word clouds :
textplot_wordcloud(dtm, max_words=50 , color= c('darkblue','red'))

#T.4 Create frequency of words
#install.packages("quanteda.textstats")
library(quanteda.textstats)
textstat_frequency(dtm, n=10)

top_10_keywords <- data.frame(textstat_frequency(dtm, n=10))%>%
  select(-group)
# rename column feature to key words
names(top_10_keywords) <-c("Key_word","freq","rank","doc_freq")
str(top_10_keywords)

#2.2 age group group and gender

# Create a bar plot
barplot(top_10_keywords$freq,names.arg = top_10_keywords$Key_word,
        col = "skyblue", main = "Bar Chart", xlab = "Labels",ylab = "Frequencies")

#3.  Save data frame to CSV file
write.csv(df, file = "mental_health_survey_cleaned.csv", row.names = TRUE)
