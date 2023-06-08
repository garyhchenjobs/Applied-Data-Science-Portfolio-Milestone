#Author: Gary Chen
#IST-719
#Final Poster


getwd()
setwd("C:/Users/Gary/Desktop/IST-719/Project")


movies <- read.csv('movies.csv', sep=',', header=TRUE)
View(movies)

#Distribution of Movie Scores
hist(movies$score         
     ,space = 0.3
     ,ylim=c(0, 2000)
     ,xlab='IMDB Rating'
     ,col = "gold"
     #,ylab='Fre'
     ,main="Distribution of IMDB Scores")

#word cloud of movie stars
#install.packages("wordcloud")
library(wordcloud)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("wordcloud2")
library(wordcloud2)
#install.packages("tm")
library(tm)

text <- movies_USA$star

docs <- Corpus(VectorSource(text))


docs <- docs %>%
        tm_map(removeNumbers) %>%
        tm_map(removePunctuation) %>%
        tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
df <- data.frame(word = names(words),freq=words)

set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1,           
          max.words=1000, 
          random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))

#wordcloud2(data=df, size=1.6, color='random-dark')

str(movies)

#install.packages("tidyr")
#install.packages("stringr")
library(tidyr)
library(stringr)

#split the released column into date and country of released
movies_split <- separate(data = movies, col = released, into = c("released_date", "released_country"), sep = "\\(" )

#take out ending ) in released_country column

movies_split$released_country <- str_trim(gsub(")","", movies_split$released_country))

View(movies_split)

#only want USA movie released entries
movies_USA <- movies_split[movies_split$released_country == 'United States',]
head(movies_USA)
str(movies_USA)
#Now we have 6737 observations

hist(movies_USA$year)

#adjust revenue and budget $ inflation to 2020 scale
#install.packages("priceR")
library(priceR)

nrow(movies_USA[is.na(movies_USA$year),])
#2 NULL
nrow(movies_USA[is.na(movies_USA$gross),])
#134 NULL

#Remove rows with NULL Values in year and gross revenue
movies_USA <- movies_USA[!is.na(str_trim(movies_USA$year)),]
movies_USA <- movies_USA[!is.na(str_trim(movies_USA$gross)),]

nrow(movies_USA[is.na(movies_USA$year),])
nrow(movies_USA[is.na(movies_USA$gross),])
str(movies_USA)
#6603 records now


movies_USA$gross_inflation <- round(adjust_for_inflation(movies_USA$gross, movies_USA$year, "US", to_date = 2020), 0)

nrow(movies_USA[is.na(movies_USA$year),])
nrow(movies_USA[is.na(movies_USA$gross),])

str(movies_USA)
View(movies_USA)

total_revenue_by_year <- aggregate(movies_USA$gross_inflation, list(movies_USA$year), sum)
colnames(total_revenue_by_year) <- c("year", "adjusted_total_revenue")
head(total_revenue_by_year)

#Graphs
library(ggplot2)
#install.packages("scales")
library(scales)

#Which year had the biggest box office revenue total (adjusted revenue for inflation to 2020 vs regular gross revenue)? 
plot(total_revenue_by_year$year
     ,total_revenue_by_year$adjusted_total_revenue
     ,type='l'
     ,ylim=c(0, 40000000000)
     ,xlab='Year'
     #,bty='n'
     ,lwd=2.0
     ,ylab='Adjusted Total Revenue')

str(total_revenue_by_year)

revenue_chart <- ggplot(data= total_revenue_by_year, aes(x=year, y =adjusted_total_revenue)) + geom_line(color = "green", size = 1.5) + geom_point() + ggtitle("Gross Movie Revenue by Year") + labs(x="Year", y="Adjusted Revenue Inflation(2020)")
revenue_chart  + scale_y_continuous(labels = dollar) 

#Bring in unadjusted gross revenue
total_revenue_by_year <- aggregate(list(movies_USA$gross_inflation, movies_USA$gross), list(movies_USA$year), sum)
View(total_revenue_by_year)
colnames(total_revenue_by_year) <- c("year", "adjusted_total_revenue_2020", "total_revenue")
head(total_revenue_by_year)

max(total_revenue_by_year$total_revenue)

View(total_revenue_by_year)

revenue_chart2 <- ggplot(data= total_revenue_by_year, aes(x = year)) +                    # basic graphical object
        geom_line(aes(y= adjusted_total_revenue_2020), color="green", size = 1.25) + # first layer
        geom_line(aes(y=total_revenue), color="black", size = 1.25)   # second layer 

revenue_chart2 + scale_y_continuous(labels = dollar) + ggtitle("Gross Movie Revenue by Year") + labs(x="Year", y="Total Revenue")

        


#What were the top genre preferences of each decade? 

#put years into decade bins
#install.packages("lubridate")
library(lubridate)

floor_decade    = function(value){ return(value - value %% 10) }
movies_USA$decade <- floor_decade(movies_USA$year)

View(movies_USA)

scoring_genre <- aggregate(movies_USA$score, list(movies_USA$decade, movies_USA$genre), mean)
colnames(scoring_genre) <- c("decade", "genre", "score")

#Remove 2020 since it is the only year in 20's decade
scoring_genre <- scoring_genre[scoring_genre$decade!="2020",]

View(scoring_genre)

scoring_chart <- ggplot(data = scoring_genre, aes(fill = genre, y = score, x = reorder(decade, genre))) +geom_bar(position="dodge", stat="identity", color= "white")
scoring_chart +ggtitle("Top IMDB Average Scores by Genre") + labs(x="Decades", y="Average IMDB Scores") + coord_flip() + coord_cartesian(ylim = c(0,10))

#install.packages("RColorBrewer")
library(RColorBrewer)
scoring_chart <- ggplot(data = scoring_genre, aes(fill = genre, y = score, x = reorder(decade, genre))) +geom_bar(position="dodge", stat="identity")
scoring_chart +ggtitle("Top IMDB Average Scores by Genre") + labs(x="Decades", y="Average IMDB Scores") + coord_cartesian(ylim = c(0,10)) #+ scale_fill_brewer("Dark2")



#count number of movies from each genre by year
#install.packages("dplyr")
library(dplyr)

genre_count<- movies_USA %>% count(genre, decade)
genre_count <- genre_count[genre_count$decade!="2020",]
View(genre_count)
genre_agg <- aggregate(genre_count$n, list(genre_count$decade), sum)
View(genre_agg)

count_chart <- ggplot(data = genre_count, aes(fill = genre, y = n, x = reorder(decade, genre))) +geom_bar(position="dodge", stat="identity", color = "black")
count_chart +ggtitle("Movies Released by Genre") + labs(x="Decades", y="Number of Movies")

#top Grossing genre by Year
grossing_genre <- aggregate(movies_USA$gross, list(movies_USA$decade, movies_USA$genre), sum)
colnames(grossing_genre) <- c("decade", "genre", "revenue")
grossing_genre <- grossing_genre[grossing_genre$decade!="2020",]

grossing_chart <- ggplot(data = grossing_genre, aes(fill = genre, y = revenue, x = reorder(-decade, genre))) +geom_bar(position="dodge", stat="identity", color = "black")
grossing_chart +ggtitle("Top Grossing Genres By Decade") + labs(x="Decades", y="Total Revenue") + scale_y_continuous(labels = dollar) + coord_flip()


#What are the top 10 movie studios of all time?
top_studio <- aggregate(movies_USA$gross, list(movies_USA$company), sum)
colnames(top_studio) <- c("studio", "total_revenue")
top_studio <- top_studio[order(top_studio$total_revenue, decreasing = T),]
View(top_studio)
top_10_studio <- head(top_studio, 10)
top_10_studio <- top_10_studio[order(top_10_studio$total_revenue, decreasing = T),]
View(top_10_studio)


studio <- ggplot(data = top_10_studio, aes(x= reorder(studio, total_revenue ), y=total_revenue)) + geom_bar(stat="identity", color = "dark green", fill = "light green", color = "black" ) + labs(y="Total Revenue", x="Movie Studio") + scale_y_continuous(labels = dollar)
studio + ggtitle("Top 10 Studios") + coord_flip() 
