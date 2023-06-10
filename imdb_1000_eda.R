##### Loading Data #####
getwd()
setwd( "C:/Users/Ayushi/OneDrive/Desktop/R_programs")
df = read.csv("imdb_top_1000.csv")
head(df)

##### Knowing more about Data #####
str(df)
# there are 16 variables in the data and 1000 observations
summary(df)
# many of the variables have character data type so there's a need
# to convert them to categorical wherever required 
# There are 4 Numerical variables present in the data 
# removing some unwanted variables from the data using dplyr 
# Poster_link, overview 
library(dplyr)
df1 = df %>% select(-c(Poster_Link, Overview))
head(df1)
str(df1)
# Now there are 14 Variables and 1000 Observations
summary(df1)
# there are 157 NA's in meta_score &
# 169 NA's in Gross


##### Missing Values #####
# Checking the number of NA values
length(which(is.na(df1))) # 157 + 169 = 326
# visual representation of missing values 
# install.packages("naniar")
library(naniar)
vis_miss(df1)
# From graph we can see that meta_score and Gross
# columns has missing values 
# Replacing the NA values by 0  
df1[is.na(df1)] <- 0  
summary(df1)
vis_miss(df1)
# now there are no missing values in the data
# Now there are 14 variables and 843 Observations

##### Correlation #####
corr = cor(df1[, c("IMDB_Rating","Meta_score",
                   "No_of_Votes","Gross")])
corr
# plotting the correlation
#install.packages("corrplot")
library(corrplot)

corrplot(corr,col = COL2("PuOr",10))


##### Skewness of the data #####
# using ggplot2
library(ggplot2)
ggplot(df1, aes(IMDB_Rating),) + geom_histogram() + ggtitle("Histogram of IMDB ratings of movies")
ggplot(df1, aes(Meta_score)) + geom_histogram() + ggtitle("Histogram of Meta score of movies")
ggplot(df1, aes(No_of_Votes)) + geom_histogram() + ggtitle("Histogram of no of votes of movies")
ggplot(df1, aes(Gross)) + geom_histogram() + ggtitle("Histogram of Gross of movies")

##### Insights #####
# top 25 rated movies
top_25_rated <- df1 %>%
  select(Series_Title, IMDB_Rating) %>%
  arrange(desc(IMDB_Rating)) %>%
  slice(1:25)
top_25_rated

bottom_5_rated <- df1 %>%
  select(Series_Title, IMDB_Rating) %>%
  arrange(desc(IMDB_Rating)) %>%
  tail(5)
bottom_5_rated

# Which director made most of the films
top_ten_directors <- df1 %>%
  select(Director) %>%
  group_by(Director) %>%
  summarise(films = n()) %>%
  arrange(desc(films)) %>%
  slice(1:10)
top_ten_directors

bottom_5_directors <- df1 %>%
  select(Director) %>%
  group_by(Director) %>%
  summarise(films = n()) %>%
  arrange(desc(films)) %>%
  tail(5)
bottom_5_directors

# plotting top 10 directors 
library(tidyverse)
top_ten_directors$Director <- factor(top_ten_directors$Director) %>%
  fct_reorder(top_ten_directors$films)

directors_plot <- ggplot(top_ten_directors) +
  geom_col(aes(films, Director), fill = "Slateblue") +
  labs(
    title = "Directors With the Most Films in IMDB Top 1000",
    subtitle = "1920-2020",
  )
directors_plot

# Top Genres
top_genres <- df1 %>%
  select(Genre) %>%
  group_by(Genre) %>%
  summarise(Genre_Count = n()) %>%
  arrange((desc(Genre_Count))) %>%
  slice(1:10)
top_genres

top_genres$Genre <- factor(top_genres$Genre) %>%
  fct_reorder(top_genres$Genre_Count)

# plot of top 10 genres
top_genres_plot <- ggplot(top_genres) +
  geom_col(aes(Genre_Count, Genre), fill = "purple") +
  labs(
    title = "Top Movie Genres in IMDB Top 1000",
    subtitle = "1920-2020") + xlab("Number of Films")
top_genres_plot

# Plot of Certificate
certificate_plot = ggplot(df1 %>%
                            group_by(Certificate)%>%
                            summarise(count = n()),
                          aes(x=reorder(Certificate,-count),y=count,
                              fill = Certificate))+
  geom_bar(stat = "identity") + xlab("Type of certificate")
certificate_plot

# Which movie earned the most 
top_25_earned <- df1 %>%
  select(Series_Title, Gross) %>%
  arrange(desc(Gross)) %>%
  slice(1:25)
top_25_earned

# which movie was voted the most
top_25_voted <- df1 %>%
  select(Series_Title, No_of_Votes) %>%
  arrange(desc(No_of_Votes)) %>%
  slice(1:25)
top_25_voted

bottom_5_voted <- df1 %>%
  select(Series_Title, No_of_Votes) %>%
  arrange(desc(No_of_Votes)) %>%
  tail(5)
bottom_5_voted
