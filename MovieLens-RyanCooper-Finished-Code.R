# Note: this process could take a couple of minutes
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(matrix)) install.packages("matrix", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

fdir <- unzip(dl, "ml-10M100K/ratings.dat")
ratings <- fread(text = gsub("::", "\t", readLines(fdir)),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

fdir <- unzip(dl, "ml-10M100K/movies.dat")
movies <- str_split_fixed(readLines(fdir), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")

# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
#}

#remove some variables that will not be needed
rm(temp)
rm(removed)
rm(ratings)
rm(movielens)
rm(test_index)
rm(fdir)

#copy the edx set in memory in case we need to reset.
edx_backup <- edx

#This will cause the remainder of the file to not use scientific notation
options(scipen = 999)

#table dimensions
dims <-dim(edx)
hdr <- head(edx)
#movies
nr <- nrow(edx)
#movies
nc <- ncol(edx)
#movies
tm <- length(unique(edx$movieId))
#users
tu <- length(unique(edx$userId))
#ratings
tr <- length(unique(edx$rating))
#min date of rating
fr <-min(as.Date(as.POSIXct(edx$timestamp, origin="1970-01-01")))
#max date of rating
lr <- max(as.Date(as.POSIXct(edx$timestamp, origin="1970-01-01")))
#ratings
med <- median(edx$rating)
mu <- mean(edx$rating)
sigma <- sd(edx$rating)

#overview table examining the data set
overview <- data.frame(rows = nr, 
                       columns = nc, 
                       movies = tm,
                       users = tu,
                       ratings=tr,
                       firstrating=fr,
                       lastrating=lr,
                       mu=mu,
                       med=med,
                       sigma=sigma)

#charts related to the same subjects will be colored the same
palette<-new.env()
palette[["movie"]]<-"darkslateblue"
palette[["user"]]<-"darkcyan"
palette[["rating"]]<-"chartreuse4"
palette[["year"]]<-"red3"
palette[["month"]]<-"darkorange3"
palette[["genre"]]<-"magenta4"
palette[["genreera"]]<-"maroon4"
palette[["genreuser"]]<-"palevioletred3"
palette[["popular"]]<-"darkgreen"
palette[["count"]]<-"goldenrod3"
palette[["other"]]<-"deeppink3"

overview %>% knitr::kable(label="Overview of EDX Dataset")

#sample 200 random users & movies to create a matrix image
set.seed(1, sample.kind="Rounding")
topX <- 200
topXusers <- sample(unique(edx$userId),topX)

#image a sparse matrix
edx %>% filter(userId %in% topXusers) %>% 
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>% 
  select(sample(ncol(.), topX)) %>% 
  as.matrix() %>% 
  t(.) %>%
  image(1:topX, 1:topX, . , 
        main="Sparse Matrix Sample",
        xlab="Movies Rated", 
        ylab="Users")

#reformat the date
edx_fortify <- edx
edx_fortify <- edx_fortify %>% 
  mutate(ratedate = as.Date(as.POSIXct(timestamp, origin="1970-01-01")))
edx_fortify <- edx_fortify %>% 
  mutate(ratept = as.POSIXct(timestamp, origin="1970-01-01"))
edx_fortify <- edx_fortify %>% 
  mutate(rateyear = format(ratedate, "%Y"))
edx_fortify <- edx_fortify %>% 
  mutate(ratemonth = format(ratedate, "%m"))

#convert the year in the title to a usable year variable and a title without the year
edx_fortify <- edx_fortify %>% 
  mutate(movieyear = as.numeric(str_extract(title,"(?<=\\()\\d{4}?(?=\\))")))
edx_fortify <- edx_fortify %>% 
  mutate(titlenoyear = as.character(str_remove(title,"\\s\\(\\d{4}\\)")))

#check the results
edx_fortify %>% 
  slice(1:10) %>% 
  select(movieyear, titlenoyear, ratemonth, rateyear) %>% 
  knitr::kable(label="Fortified EDX Data")

#frequency of ratings
histrate <- edx_fortify %>% group_by(rating) %>% summarise(ratecount = n()) 
hrplot <- histrate %>% 
  ggplot(aes(x=as.factor(rating),y=ratecount)) + 
  geom_col(color = "black", fill=palette[["rating"]]) + 
  labs(title="Count of Reviews, by Rating", 
        x="Rating", 
        y="Review Count")
hrplot


#show a distribution of ratings
histmovrate <- edx_fortify %>% group_by(rating) %>% summarise(ratecount = n_distinct(movieId)) 
hmplot <- histmovrate %>% 
  ggplot(aes(x=as.factor(rating),y=ratecount)) + 
  geom_col(color = "black", fill=palette[["movie"]]) + 
  labs(title="Count of Distinct Movies, by Rating", 
       subtitle="Number of movies that have received each rating at least once",
        x="Rating", 
        y="Movie Count")
hmplot


#movie rating averages
movie_avgs <- edx_fortify %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating))

#show a histogram of raw movie ratings
movie_avgs %>% 
  ggplot() + 
  geom_histogram(aes(b_i),bins = 10, color="#000000", fill=palette[["movie"]])  + 
  labs(title="Movie Ratings Histogram", 
        x="Mean Ratings", 
       y="Movies Frequency") 

#show a histogram of raw user ratings
edx_fortify %>% 
     group_by(userId) %>% 
     summarize(b_u = mean(rating)) %>% 
     ggplot(aes(b_u)) + 
     geom_histogram(bins = 10, color = "black", fill=palette[["user"]]) + 
  labs(title="User Ratings Histogram", 
        x="Mean Ratings", 
        y="Users Frequency")

#show a histogram of raw movie ratings by years
edx_fortify %>% 
     group_by(movieyear) %>% 
     summarize(b_y = mean(rating)) %>% 
     ggplot(aes(b_y)) + 
     geom_histogram(bins = 10, color = "black", fill=palette[["year"]])+ 
  labs(title="Yearly Ratings Histogram", 
        x="Mean Ratings", 
        y="Years")

#frequency of movie dates
histmovyear <- edx_fortify %>% 
  group_by(movieyear) %>% 
  summarise(ratecount = n()) 
hmplot <- histmovyear %>% 
  ggplot(aes(x=as.factor(movieyear),y=ratecount)) + 
  geom_col(fill=palette[["rating"]])  + 
  labs(title="Count of Reviews, by Movie Year",
        x="Movie Year", 
        y="Ratings Count") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
hmplot

#set a allgenres variable
allgenres <- nrow(unique(edx_fortify$genres))

#get move counts by year
moviecounts <- edx_fortify %>% 
  group_by(movieyear) %>% 
  summarise(moviecount = n_distinct(movieId)) 

#show a bar chart
hgplot <- moviecounts %>% 
  ggplot(aes(x=as.factor(movieyear),y=moviecount)) + 
  geom_col(fill=palette[["movie"]]) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title="Count of Movies, by Movie Year",
        x="Movie Year", 
        y="Movie Count")
hgplot

#set mu to the overall average in the edx data.
mu <- mean(edx_fortify$rating)

#set up averages data
histmovrate <- edx_fortify %>% 
  mutate(movieyear = replace_na(movieyear, 1900)) %>% 
  group_by(movieyear) %>% 
  summarise(rateavg = mean(rating)) 


#create some long names for the eras
name_eras <- c("10s-Early Era","20s-Silent Era",
               "30s-Talkie Era","40s-Technicolor Era","50s-Blacklist Era",
               "60s-Small Studio Era","70s-Special Effects Era",
               "80s-Home Video Era","90s-Digital Era","00s-Franchise Era")

#create some short names for the eras
movie_eras <- c("early","silent","talkie","technicolor","blacklist",
                "smallstudio","specialfx","homevideo","digital","franchise")

year_eras <- c("10s","20s","30s","40s","50s",
                "60s","70s","80s","90s","2Ks")

#set the bounds of each era (we are using decades)
start_eras <- c(1910,1920,1930,1940,1950,1960,1970,1980,1990,2000)
end_eras <- c(1919,1929,1939,1949,1959,1969,1979,1989,1999,2009)
order_eras <- c(1:10)

#combine into a dataframe
mv_eragroups <- data.frame(name=name_eras,
                           code=movie_eras,
                           starts=start_eras,
                           ends=end_eras,
                           eraorder=order_eras)
mv_eragroups %>% knitr::kable(label="Eras")

mv_years <- c()
mv_eras <- c()
mv_names <- c()

#builds a table with an era assigned to each year
i <- 0
y <- 0
for(n in c(1:length(movie_eras)) ){
  i <- i + 1
    for (j in c(start_eras[n]:end_eras[n])){
      y <- y + 1
      mv_years[y] = j
      mv_eras[y] = movie_eras[i]
      mv_names[y] = name_eras[i]
    }
  }
mv_era <- NULL
mv_era <- data.frame(name=mv_names,era=mv_eras,movieyear=mv_years)

#join the era details to the edx data
if(is.null(edx_fortify$era)){
  edx_fortify <- edx_fortify %>% left_join(mv_era, by = "movieyear")
}

#top titles
top_annualrating <- edx_fortify %>%  
  group_by(era, movieId, titlenoyear, movieyear) %>% 
  summarise(ratecount = n(),
            avgrating = round(mean(rating),2), 
            sdrating = sd(rating)) %>% 
  arrange(-ratecount) %>% 
  group_by(era) %>% 
  slice(1:3) %>% 
  mutate(titleny = paste(substr(titlenoyear, 1, 30), 
                         ifelse(str_length(titlenoyear) > 30,'...','') )) %>%
  select(era,avgrating,ratecount, titleny, movieyear) %>% 
  left_join(mv_eragroups, by = c("era"="code")) %>% 
  arrange(eraorder, -avgrating) %>% 
  select(name, era, titleny, avgrating, movieyear, ratecount)

top_annualrating %>% knitr::kable(label="Top 5 Most Rated Movies Each Era")

#show chart of ratings over time
hmrplot <- histmovrate %>% ggplot() + 
  geom_point(aes(x=movieyear,y=rateavg)) + 
  geom_smooth(aes(x=movieyear,y=rateavg))
 
hmrplot <- hmrplot + 
  geom_hline(yintercept = mu, 
             colour="#BB0000", 
             linetype="dashed") +
  
  geom_label(mapping=aes(x=1940, 
                        y=3.52, 
                        label='Mean Rating'), 
            size=4, 
            hjust=-0.4, 
            vjust=0) + 
  geom_text(size=3,aes(label = ifelse(abs(rateavg-lag(rateavg,1)) > 0.25,movieyear,''), 
                       x=movieyear,
                       y=rateavg),
            hjust=0,
            nudge_x=1) +    
  
  geom_vline(xintercept = start_eras[1], colour="#999999") + 
  geom_label(mapping=aes(x=start_eras[1], y=4.25, 
                        label=movie_eras[1]), size=4, angle=90, vjust=1, hjust=0) +
  
  geom_vline(xintercept = start_eras[2], colour="#999999") + 
  geom_label(mapping=aes(x=start_eras[2], y=4.25, 
                        label=movie_eras[2]), size=4, angle=90, vjust=1, hjust=0) +
  
  geom_vline(xintercept = start_eras[3], colour="#999999") + 
  geom_label(mapping=aes(x=start_eras[3], y=4.25, 
                        label=movie_eras[3]), size=4, angle=90, vjust=1, hjust=0) +
  
  geom_vline(xintercept = start_eras[4], colour="#999999") + 
  geom_label(mapping=aes(x=start_eras[4], y=4.25, 
                        label=movie_eras[4]), size=4, angle=90, vjust=1, hjust=0) +
  
  geom_vline(xintercept = start_eras[5], colour="#999999") + 
  geom_label(mapping=aes(x=start_eras[5], y=4.25, 
                        label=movie_eras[5]), size=4, angle=90, vjust=1, hjust=0) +
  
  geom_vline(xintercept = start_eras[6], colour="#999999") + 
  geom_label(mapping=aes(x=start_eras[6], y=4.25, 
                        label=movie_eras[6]), size=4, angle=90, vjust=1, hjust=0) +
  
  geom_vline(xintercept = start_eras[7], colour="#999999") + 
  geom_label(mapping=aes(x=start_eras[7], y=4.25, 
                        label=movie_eras[7]), size=4, angle=90, vjust=1, hjust=0) +
  
  geom_vline(xintercept = start_eras[8], colour="#999999") + 
  geom_label(mapping=aes(x=start_eras[8], y=4.25, 
                        label=movie_eras[8]), size=4, angle=90, vjust=1, hjust=0) +
  
  geom_vline(xintercept = start_eras[9], colour="#999999") + 
  geom_label(mapping=aes(x=start_eras[9], y=4.25, 
                        label=movie_eras[9]), size=4, angle=90, vjust=1, hjust=0) +
  geom_vline(xintercept = start_eras[10], colour="#999999") + 
  geom_label(mapping=aes(x=start_eras[10], y=4.25, 
                        label=movie_eras[10]), size=4, angle=90, vjust=1, hjust=0) +
  
theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
  labs(title="Mean Rating of All Movies by Movie Year",
        x="Movie Year", 
        y="Mean Rating")

hmrplot

#get data by era
histeras <- edx_fortify %>% 
  group_by(era,name) %>% 
  summarise(movies = n_distinct(movieId), 
            avg = round(mean(rating),2), 
            se = sd(rating)/sqrt(n())) 

histeras$era_f = factor(histeras$era, levels=movie_eras)

#plot couunts by era/decade
histeras %>% 
  arrange(era_f) %>% 
  select(-era_f) %>% 
  knitr::kable(label="Ratings & Movie Counts by Era")

#plot ratings by era/decade
histeras %>% 
  ggplot(aes(x = era_f, y = avg, 
             ymin = avg - 2*se, 
             ymax = avg + 2*se)) + 
  geom_point() +
  geom_hline(yintercept = mu, colour="#BB0000", linetype="dashed") +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title="Mean Rating by Era",
        x="Era", 
        y="Mean Rating")

#build a table with movie count & rating count, & rating avg by year
histmovrate <- edx_fortify %>% 
  group_by(movieyear) %>% 
  summarise(rateavg = mean(rating), ratecount = n()) 

histmovrate <- histmovrate %>% 
  left_join(moviecounts, by = "movieyear")

histmovrate <- histmovrate %>%
  mutate(moviecount = moviecount/100)

histmovrate <- histmovrate %>% 
  mutate(ratecount =log10(ratecount))

#pivot to a tidy format
df2 <- tidyr::pivot_longer(histmovrate, 
                           cols=c('rateavg', 'moviecount','ratecount'), 
                           names_to='variable', 
values_to="value")

#render plot
hmrplot <- df2 %>% 
  ggplot(aes(x=as.factor(movieyear),
             y=value, 
             color=variable, 
             group=variable)) + 
  geom_path(size=1) + 
  geom_vline(xintercept=1980) + 
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) + 
  labs(title="Avg Rating, Count of Movies (x100), Count of Ratings (log10) by Movie Year",
        x="Movie Year", 
        y="")
hmrplot


#build a of each movies average rating by year.
moviedates <- edx_fortify %>% 
  group_by(era, movieyear, movieId, title) %>% 
  summarise(rateavg = mean(rating), 
            ratecount = n()) %>% 
  arrange(rateavg)

#render scatterplot with dots for each movie
moviedates %>% ggplot(aes(y=rateavg,
                          x=movieyear,
                          color=log10(ratecount))) + 
  geom_point() + 
   geom_hline(yintercept = mu, 
             colour="#BB0000", 
             linetype="dashed") +
 labs(title="Mean Rating of Each Movie by Movie Year",
        x="Movie Year", 
        y="Mean Rating") + scale_color_viridis_c()

#build a of each movies average rating by year.
moviemonths <- edx_fortify %>% 
  group_by(ratemonth) %>% 
  summarise(rateavg = mean(rating),
            ratesd = sd(rating),
            ratecount = n(), 
            ratese = sd(rating)/sqrt(n())) %>% 
  arrange(rateavg)

#compare this rating by month of movies during the period of the survey
moviemonthsnew <- edx_fortify %>% 
  filter(movieyear >= 1995) %>%
  group_by(ratemonth) %>% 
  summarise(rateavg = mean(rating),
            ratesd = sd(rating),
            ratecount = n(), 
            ratese = sd(rating)/sqrt(n())) %>% 
  arrange(rateavg)

#and movies that pre-dates the period of the ratings survey
moviemonthsold <- edx_fortify %>% 
  filter(movieyear < 1995) %>%
  group_by(ratemonth) %>% 
  summarise(rateavg = mean(rating),
            ratesd = sd(rating),
            ratecount = n(), 
            ratese = sd(rating)/sqrt(n())) %>% 
  arrange(rateavg)

#render scatterplot with dots for each movie
moviemonths %>%
  ggplot(aes(x = ratemonth, 
             y = rateavg, 
             ymin = rateavg - 2*ratese, 
             ymax = rateavg + 2*ratese)) + 
  geom_point() +
  geom_hline(yintercept = mu, 
             colour="#BB0000", 
             linetype="dashed") +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title="Mean Rating of Each Movie by Month of Rating", 
        x="Month", 
        y="Mean Rating")
  

moviemonthsnew %>%
  ggplot(aes(x = ratemonth, 
             y = rateavg, 
             ymin = rateavg - 2*ratese, 
             ymax = rateavg + 2*ratese)) + 
  geom_point() +
  geom_hline(yintercept = mu, 
             colour="#BB0000", 
             linetype="dashed") +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title="Mean Rating of Each Movie by Month of Rating (1995+)", 
        x="Month", 
        y="Mean Rating")
  
moviemonthsold %>%
  ggplot(aes(x = ratemonth, 
             y = rateavg, 
             ymin = rateavg - 2*ratese, 
             ymax = rateavg + 2*ratese)) + 
  geom_point() +
  geom_hline(yintercept = mu, 
             colour="#BB0000", 
             linetype="dashed") +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title="Mean Rating of Each Movie by Month of Rating (<1995)", 
        x="Month", 
        y="Mean Rating")
  

#boxplot of movie ratings by genre
edx_fortify %>% 
  group_by(genres) %>% 
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>% 
  filter(n>50000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, 
             y = avg, 
             ymin = avg - 2*se, 
             ymax = avg + 2*se)) + 
  geom_point() +
  geom_hline(yintercept = mu, 
             colour="#BB0000", 
             linetype="dashed") +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title="Mean Rating by Genre - 50K+ Reviews", 
        x="Genres", 
        y="Mean Rating")

totgenres <- edx_fortify %>% 
  summarise(gcount = n_distinct(genres))
totgenres

#get details about the genres
histgenres <- edx_fortify %>% 
  group_by(genres) %>% 
  summarise(ratingcount = n(), 
            moviecount = n_distinct(movieId), 
            rateavg = mean(rating), 
            ratesd = sd(rating)) 
histgenres <- histgenres %>% 
  mutate(gcount = 0)
histgenres$gcount <- lengths(strsplit(as.character(histgenres$genres), "|", fixed = TRUE))

#show a bar chart
hgplot <- histgenres %>% filter(moviecount>30) %>%
  ggplot(aes(x=as.factor(genres),y=moviecount)) + 
  geom_col(fill=palette[["movie"]]) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title="Count of Movies, by Genre",
       subtitle="Genres with 30+ Movies",
        x="Genre", 
        y="Movie Count")
hgplot

#get the "pure" genres for use in the box plot below
puregenres <- edx_fortify %>% 
  left_join(histgenres, by = "genres") %>% 
  filter(gcount == 1) %>% 
  group_by(genres) %>% 
  summarize(movies = n_distinct(movieId))

pgplot <- puregenres %>%
  ggplot(aes(x=as.factor(genres),y=movies)) + 
  geom_col(fill=palette[["movie"]]) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title="Count of Movies, by Genre",
       subtitle="Pure Genres",
        x="Genre", 
        y="Movie Count")
pgplot

#show box plot
edx_fortify %>% 
  group_by(genres) %>% 
  summarize(n = n(), 
            avg = mean(rating), 
            se = sd(rating)/sqrt(n()))  %>% 
  left_join(histgenres, by = "genres") %>% 
  filter(gcount == 1) %>% 
  ggplot(aes(x =genres, 
             y = avg, 
             ymin = avg - 2*se, 
             ymax = avg + 2*se)) + 
  geom_point() +
  geom_hline(yintercept = mu, colour="#BB0000", linetype="dashed") +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title="Mean Rating by Genre - Pure Genres", 
        x="Genres", 
        y="Mean Rating")




#boxplot of movie ratings by genre
edx_fortify %>% group_by(genres) %>% 
  summarize(n = n(), 
            avg = mean(rating), 
            se = sd(rating)/sqrt(n())) %>% 
  filter(n<10) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, 
             y = avg, 
             ymin = avg - 2*se, 
             ymax = avg + 2*se)) + 
  geom_point() +
  geom_hline(yintercept = mu, colour="#BB0000", linetype="dashed") +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title="Mean Rating by Genre - < 10 ratings.", 
        x="Genres", 
        y="Mean Rating")

#get movie counts by genre counts
genrecounts <- histgenres %>% 
  group_by(gcount) %>% 
  summarize(moviecount = sum(moviecount), 
            rateavg = mean(rateavg)
             ) 

#show bar chart
plotgenrecount <- genrecounts %>% 
  ggplot(aes(x=gcount,y=moviecount)) + 
  geom_col(fill=palette[["movie"]]) + labs(title="Count of Movies by Number of Genres", 
        x="Genre Count", 
        y="Movie Count")
plotgenrecount

#show rating averages by # of genres
plotgenreratings <- genrecounts %>% 
  ggplot(aes(x=gcount,y=rateavg)) + 
  geom_point() + 
  geom_smooth(method = "loess", level=.95) + 
   geom_hline(yintercept = mu, 
              colour="#BB0000", 
              linetype="dashed") +
  labs(title="Mean Rating by Number of Genres", 
        x="Genre Count", 
        y="Mean Rating")

plotgenreratings

#list some popular genres to examine
trygenres <- c("Comedy|Crime|Drama","Drama|Mystery|Thriller",
               "Children|Comedy","Drama|War","Action|Comedy",
               "Horror|Thriller","Drama","Action","Comedy","War","Western","Horror")


#boxplot of movie ratings by genre
ratingsovertime <- edx_fortify %>% 
  filter(genres %in% trygenres) %>%
  group_by(era,genres) %>% 
  summarize(n = n(), 
            avg = mean(rating), 
            se = sd(rating)/sqrt(n()))

ratingsovertime$era_f = factor(ratingsovertime$era, levels=movie_eras)

#show box plot
ratingsovertime %>% ggplot(aes(x = era_f, 
                               y = avg, 
                               ymin = avg - 2*se, 
                               ymax = avg + 2*se)) + 
  geom_point() +
   geom_hline(yintercept = mu, 
              colour="#BB0000", 
              linetype="dashed") +
  geom_errorbar() + 
 facet_wrap(~genres, ncol=2) +
 theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title="Mean Rating by Era, Selected Genres", 
        x="Era", 
        y="Mean Rating")


#show a histogram
edx_fortify %>% 
    group_by(era,genres) %>%
     summarize(b_g = mean(rating))  %>% 
     ggplot(aes(b_g)) + 
     geom_histogram(bins = 10, color = "black",fill=palette[["genre"]]) + 
  labs(title="Genre/Era Ratings Histogram", 
        x="Mean Ratings", 
        y="Genre/Eras")

#list some popular genres to examine
trygenres <- c("Comedy|Crime|Drama","Drama|Mystery|Thriller",
               "Children|Comedy","Drama|War","Action|Comedy",
               "Horror|Thriller","Drama","Action","Comedy","War","Western","Horror")

#select 10 random users with 200+ ratings
tryusers <- edx_fortify %>% 
  group_by(userId) %>% 
  summarize(rcount = n()) %>% 
  filter(rcount > 200) %>% 
  sample_n(10) %>% 
  pull(userId)

#boxplot of movie ratings by genre
ratingsoveruser <- edx_fortify %>% 
  filter(genres %in% trygenres) %>%
  filter(userId %in% tryusers) %>%
  group_by(userId,genres) %>% 
  summarize(n = n(), 
            avg = mean(rating), 
            se = sd(rating)/sqrt(n()))


#show box plot
ratingsoveruser %>% ggplot(aes(x = genres, 
                               y = avg, 
                               ymin = avg - 2*se, 
                               ymax = avg + 2*se)) + 
  geom_point() +
   geom_hline(yintercept = mu, 
              colour="#BB0000", 
              linetype="dashed") +
  geom_errorbar() + 
 facet_wrap(~paste("User:",userId), ncol=2) +
 theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title="Mean Rating by User, Selected Genres", 
        x="User", 
        y="Mean Rating")



#convert the number of ratings of the movie to a popularity rating
#save the table as popgroups which can be joined to the test set as needed
popgroups <- edx_fortify %>%
          group_by(movieId) %>% 
          summarize(popgroup = as.factor(round(log10(n()),1)), 
                    pg = as.numeric((round(log10(n()),1)))) 

#get data
mostrated <- edx_fortify %>% 
  group_by(era, movieyear, movieId, title) %>% 
  summarise(rateavg = mean(rating), 
            ratecount = n()) %>% 
  arrange(ratecount) %>% 
  filter(ratecount > 10) 

#show scatterplot
mostrated %>% 
  ggplot(aes(y=rateavg,x=log10(ratecount),color=era)) + 
  geom_point() + 
  geom_smooth(color="#000000") + 
  geom_hline(yintercept = mu, 
             colour="#BB0000", 
             linetype="dashed") +
  labs(title="Mean Rating by Popularity",
        x="Ratings Count (log10)", 
        y="Mean Rating") + 
  scale_color_discrete(labels=movie_eras)


#get most rated movies by era
mostrated <- edx_fortify %>% 
  group_by(era, movieyear, movieId, title) %>% 
  summarise(rateavg = mean(rating), ratecount = n()) %>% 
  arrange(ratecount) %>% 
  filter(ratecount > 10) 

mostrated$era_f = factor(mostrated$era, levels=movie_eras)

#get mean ratings faceted by era
mostrated %>% 
  ggplot(aes(y=rateavg,x=log10(ratecount),color=era)) + 
  geom_point() + 
  geom_smooth(color='#000000') + 
   geom_hline(yintercept = mu, 
             colour="#BB0000", 
             linetype="dashed") +
 labs(title="Mean Rating by Popularity, Faceted by Era", 
        x="Ratings Count (log10)", 
        y="Mean Rating") + 
  scale_color_discrete(labels=movie_eras) + 
  facet_wrap(~era_f)

#determine correlation coefficient
popularity_cor <- cor(mostrated$rateavg,log10(mostrated$ratecount))
pop_cor_pct <- paste("Correlation Coef.:",round(popularity_cor*100,2),"%")

#show the correlation
paste("Correlation:",round(popularity_cor*100,2),"%")

#setup variables
popgroup_index <- c(0,1,2,3,4)
popgroup_names <- c('Obscure','Uncommon','Common','Popular','Blockbuster')
popgroup_desc <- c('0-9 Reviews','10-99 Reviews','100-999 Reviews','1000-9999 Reviews','10000+ Reviews')

popgroup_info <- data.frame(ratepop = popgroup_index, 
                            popname = popgroup_names, 
                            popdesc = popgroup_desc) 

#create data for rating averages faceted by popularity
firstratedates <- edx_fortify %>% 
  group_by(era, movieyear, movieId, title) %>% 
  summarise(rateavg = mean(rating), 
            yearavg = mean(movieyear), 
            ratecount = n()) %>% 
  arrange(rateavg)  %>% 
  mutate(ratepop = floor(log10(ratecount))) %>% 
  left_join(popgroup_info,by = 'ratepop') 

#plot a scatterchart
firstratedates %>% ggplot(aes(y=rateavg,x=yearavg)) + 
  geom_point() +  
  geom_hline(yintercept = mu, 
             colour="#BB0000", 
             linetype="dashed") +
 geom_smooth(method="loess") + 
  facet_wrap(~ paste(ratepop,popname,popdesc))  + 
  labs(title="Avg Rating by Movie Year, Faceted by Popularity Group",
        x="Movie Year", 
        y="Mean Rating")


#show a histogram
edx_fortify %>% 
     left_join(popgroups, by="movieId") %>%
     group_by(popgroup) %>%
     summarize(b_p = mean(rating))  %>% 
     ggplot(aes(b_p)) + 
     geom_histogram(bins = 10, color = "black",fill=palette[["popular"]])+ 
  labs(title="Popularity Groups Ratings Histogram", 
        x="Mean Ratings", 
        y="Popularity Groups")


makepartitions <- function(dataset,rows,testportion,numparts,seed){
   returnobj <- NULL
    returnobj <- data.frame()
  
    set.seed(seed, sample.kind="Rounding")
        if(rows != -1){
          dataset <- dataset %>% sample_n(rows)
          
        }
      
    edx_part_train_index <- createDataPartition(y = dataset$rating, 
                                               times = numparts, 
                                               p = testportion, 
                                               list = FALSE)
    
    edx_part_train <- edx_fortify[-edx_part_train_index,]
    edx_part_temp <- edx_fortify[edx_part_train_index,]
    
    # Make sure userId and movieId in validation set are also in edx set
    edx_part_valid <- edx_part_temp %>% 
      semi_join(edx_part_train, by = "movieId") %>%
      semi_join(edx_part_train, by = "userId")
    
    # Add rows removed from validation set back into edx set
    edx_part_removed <- anti_join(edx_part_temp, edx_part_valid)
    edx_part_train <- rbind(edx_part_train, edx_part_removed)
    
    rm(edx_part_temp)
    rm(edx_part_removed)
  
  #return the RMSE, prediction, penalty factor, & benchmark time
      returnobj <-list(
                  trainindex=edx_part_train_index,
                  trainDS=edx_part_train,
                  testDS=edx_part_valid
                  )

      class(returnobj) <- "partition"
      returnobj

}

#We will first choose 3 random seed values for the three partitions
#this is important to make sure we are not overfitting the training parameters
seed1 <- 981
seed2 <- 523
seed3 <- 267

#each of these 90/10 partitions will have different rows 
#in the training and test data (with some overlapping)
part9010_A <- makepartitions(edx_fortify,-1,.1,1,seed1)
part9010_B <- makepartitions(edx_fortify,-1,.1,1,seed2)
part9010_C <- makepartitions(edx_fortify,-1,.1,1,seed3)


# Function that returns RMSE
RMSE <- function(true_ratings, predicted_ratings){
     sqrt(mean((true_ratings - predicted_ratings)^2))
}

#partition a training and test data set from the fortified edx 
#data that has already been partitioned
#we will use the "A" partition for the construction of the model
trainDS <- part9010_A$trainDS
testDS <- part9010_A$testDS

#IMPORTANT - Resetting mu here to the training set
#this is the baseline to which all effects will be added/subtracted
mu <- mean(trainDS$rating)

#create a vector of predictions that are all the same (mu) value
#modstart and modfinish will time the routine for benchmarking
modstart <- Sys.time()
predictions <- rep(mu, nrow(trainDS))
modfinish <- Sys.time()

#get the RMSE of the naive model
naive_rmse <- RMSE(testDS$rating,predictions)

#determine the operation speed. This operation will be done for each model
benchmark <- round(difftime(modfinish, modstart, units = "secs"),2)

#modelstep will increment for each new test
modelstep <- 0
modelstep <- modelstep + 1

#show the RMSE results in a kable
rmse_results <- data_frame(step=modelstep,
                           method = "Baseline", 
                           RMSE = naive_rmse, 
                           benchmark = benchmark,
                                     weight = 1,
                                     lowcut = 0)

rmse_results %>% knitr::kable(label="Model Results")

#setup variables
movieweight <- 1

#movie rating averages
movie_avgs <- trainDS %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating))

#show a histogram of raw movie ratings
movie_avgs %>% 
  ggplot() + 
  geom_histogram(aes(b_i),bins = 10, color="#000000", fill=palette[["movie"]])  + 
  labs(title="Movie Ratings Histogram", 
        x="Mean Ratings", 
        y="Movies Frequency")

#determine the mean movie rating
movie_avgs <- trainDS %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu) * movieweight)

#plot a histogram 
movie_avgs %>% 
  ggplot(aes(b_i)) + 
     geom_histogram(bins = 10, color = "black", fill=palette[["movie"]]) + 
     labs(title="Movie Effect Histogram", 
        x="Mean Ratings", 
        y="Movies")


#setup variables
movieweights <- seq(.980,1.02,by=.005)
movie_results <- data.frame()
modelstep <- modelstep + 1

#the best prediction will be assigned to this variable
movie_avgs_best <- c()
last_RMSE <- 1000

for (w in movieweights){

modstart <- Sys.time()

movie_avgs <- trainDS %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu) * w)

#using the testDS set only to make a prediction.
predicted_ratings <- mu + testDS %>% 
     left_join(movie_avgs, by='movieId') %>%
     .$b_i

modfinish <- Sys.time()
benchmark <- round(difftime(modfinish, modstart, units = "secs"),2)

#check prediction vs the results in the testDS set
model_1_rmse <- RMSE(testDS$rating, predicted_ratings)

if(model_1_rmse < last_RMSE){
      movie_avgs_best <- c()
      movie_avgs_best <- movie_avgs
      last_RMSE <- model_1_rmse
    }
    

modelname <- paste("Movie Effect Model")

#checking my prediction vs the results in the testDS set
movie_results <- bind_rows(movie_results,
                          data_frame(step = modelstep, 
                                     method=modelname,
                                     RMSE = model_1_rmse, 
                                     benchmark = benchmark,
                                     weight = w,
                                     lowcut = 0
                                     ))
}

#plot the results of the tuning operation
movie_results %>%   
  ggplot(aes(x=weight,y=RMSE)) + 
     geom_point() + 
     labs(title="Movie Effect Weights", 
        x="Weight", 
        y="RMSE")

#keep the best RMSE
movie_best <-  movie_results %>% slice(which.min(RMSE))

rmse_results <- bind_rows(rmse_results, movie_best)

rmse_results %>% knitr::kable(label="Model Results")

movie_best_RMSE  = movie_best$weight
movieweight = movie_best$weight


#determine the mean user rating
trainDS %>% 
     group_by(userId) %>% 
     summarize(b_u = mean(rating)) %>% 
     ggplot(aes(b_u)) + 
     geom_histogram(bins = 10, color = "black",fill=palette[["user"]]) + 
     labs(title="User Ratings Histogram", 
        x="Mean Ratings", 
        y="Users")

#setup variables
userweight <- 1

#determine the mean user rating after deducting prior effects
user_avgs <- trainDS %>% 
     left_join(movie_avgs, by='movieId') %>%
     group_by(userId) %>%
     summarize(b_u = mean(rating - mu - b_i) * userweight) 

#plot a histogram
user_avgs %>%  
  ggplot(aes(b_u)) + 
     geom_histogram(bins = 10, color = "black",fill=palette[["user"]]) + 
     labs(title="User Effect Histogram",
        x="Ratings Change", 
        y="Users")

#setup variables
userweights <- seq(.940,1.02,by=.005)
user_results <- data.frame()
modelstep <- modelstep + 1

#the best prediction will be assigned to this variable
user_avgs_best <- c()
last_RMSE <- 1000

for (w in userweights){
modstart <- Sys.time()

#determine the mean user rating after deducting prior effects
user_avgs <- trainDS %>% 
     left_join(movie_avgs_best, by='movieId') %>%
     group_by(userId) %>%
     summarize(b_u = mean(rating - mu - b_i) * w) 
  
#predict ratings on the test set
predicted_ratings <- testDS %>% 
     left_join(movie_avgs_best, by='movieId') %>%
     left_join(user_avgs, by='userId') %>%
     mutate(pred = mu + b_i + b_u) %>%
     .$pred

modfinish <- Sys.time()
benchmark <- round(difftime(modfinish, modstart, units = "secs"),2)

modelname <- paste("User Effect Model")

#calculate the RMSE
model_2_rmse <- RMSE(testDS$rating, predicted_ratings)


if(model_2_rmse < last_RMSE){
      user_avgs_best <- c()
      user_avgs_best <- user_avgs
      last_RMSE <- model_2_rmse 
   }

    
#show the results
user_results <- bind_rows(user_results,
                          data_frame(step = modelstep, 
                                     method=modelname,  
                                     RMSE = model_2_rmse,
                                     benchmark = benchmark,
                                     weight = w,
                                     lowcut = 0
                                     ))
}

#plot the RMSE curve
user_results %>%   
  ggplot(aes(x=weight,y=RMSE)) + 
     geom_point() + 
     labs(title="User Effect Weights", 
        x="Weight", 
        y="RMSE")

#kep the best RMSE
user_best <-  user_results %>% slice(which.min(RMSE))

rmse_results <- bind_rows(rmse_results, user_best)

rmse_results %>% knitr::kable(label="Model Results")

user_best_RMSE <- user_best$RMSE
userweight = user_best$weight


#setup variables
yearweight <- 1

#determine the mean movie rating by year
trainDS %>% 
     group_by(movieyear) %>%
     summarize(b_y = mean(rating))  %>% 
     ggplot(aes(b_y)) + 
     geom_histogram(bins = 10, color = "black", fill=palette[["year"]]) + 
     labs(title="Year Ratings Histogram",
        x="Mean Rating", 
        y="Months") 

#determine the mean movie rating by year adjusted for prior effects
year_avgs <- trainDS %>% 
     left_join(movie_avgs, by='movieId') %>%
     left_join(user_avgs, by='userId') %>%
     group_by(movieyear) %>%
     summarize(b_y = mean(rating - mu - b_i - b_u) * yearweight)

#plot a histogram
year_avgs %>% ggplot(aes(b_y)) + 
     geom_histogram(bins = 10, color = "black", fill=palette[["year"]]) + 
     labs(title="Year Effect Histogram",
        x="Ratings Change", 
        y="Years")

#setup variables
yearweights <- seq(1.02,1.08,by=.01)
year_results <- data.frame()
modelstep <- modelstep + 1

#the best prediction will be assigned to this variable
year_avgs_best <- c()
last_RMSE <- 1000

for (w in yearweights){

modstart <- Sys.time()

year_avgs <- trainDS %>% 
     left_join(movie_avgs_best, by='movieId') %>%
     left_join(user_avgs_best, by='userId') %>%
     group_by(movieyear) %>%
     summarize(b_y = mean(rating - mu - b_i - b_u) * w)

#make predictions on the test set
predicted_ratings <- testDS %>% 
     mutate(movieyear=as.numeric(str_extract(title,"(?<=\\()\\d{4}?(?=\\))"))) %>%
     left_join(movie_avgs_best, by='movieId') %>%
     left_join(user_avgs_best, by='userId') %>%
     left_join(year_avgs, by='movieyear') %>%
     mutate(pred = mu + b_i + b_u + b_y) %>%
     .$pred

modfinish <- Sys.time()
benchmark <- round(difftime(modfinish, modstart, units = "secs"),2)

#calculate the RMSE
model_3_rmse <- RMSE(testDS$rating, predicted_ratings)

if(model_3_rmse < last_RMSE){
      year_avgs_best <- c()
      year_avgs_best <- year_avgs
      last_RMSE <- model_3_rmse 
   }

modelname <- paste("Year Effect Model")

#show the results
year_results <- bind_rows(year_results,
                          data_frame(step = modelstep, 
                                     method=modelname,  
                                     RMSE = model_3_rmse,
                                     benchmark = benchmark,
                                     weight = w,
                                     lowcut = 0 ))

}

#plot the RMSE curve
year_results %>%   
  ggplot(aes(x=weight,y=RMSE)) + 
     geom_point() + 
     labs(title="Year Effect Weights", 
        x="Weight", 
        y="RMSE")

#keep the best RMSE
year_best <-  year_results %>% slice(which.min(RMSE))

rmse_results <- bind_rows(rmse_results, year_best)

rmse_results %>% knitr::kable(label="Model Results")

year_best_RMSE <- year_best$RMSE
yearweight = year_best$weight



#setup variables
monthweight <- 1

#determine the mean movie rating by year
trainDS %>% 
     group_by(ratemonth) %>%
     summarize(b_m = mean(rating))  %>% 
     ggplot(aes(b_m)) + 
     geom_histogram(bins = 10, color = "black", fill=palette[["month"]]) + 
     labs(title="Monthly Ratings Histogram",
        x="Mean Rating", 
        y="Months")

#determine the mean movie rating by month adjusted for prior effects
month_avgs <- trainDS %>% 
     left_join(movie_avgs, by='movieId') %>%
     left_join(user_avgs, by='userId') %>%
     left_join(year_avgs, by='movieyear') %>%
     group_by(ratemonth) %>%
     summarize(b_m = mean(rating - mu - b_i - b_y - b_u) * monthweight)

#plot a histogram
month_avgs %>% ggplot(aes(b_m)) + 
     geom_histogram(bins = 10, color = "black", fill=palette[["month"]]) + 
     labs(title="Month Effect Histogram",
        x="Ratings Change", 
        y="Month")


#setup variables
monthweights <- seq(.5,1.5,by=.1)
month_results <- data.frame()
modelstep <- modelstep + 1

#the best prediction will be assigned to this variable
month_avgs_best <- c()
last_RMSE <- 1000

for (w in monthweights){

modstart <- Sys.time()

month_avgs <- trainDS %>% 
     left_join(movie_avgs_best, by='movieId') %>%
     left_join(user_avgs_best, by='userId') %>%
     left_join(year_avgs_best, by='movieyear') %>%
     group_by(ratemonth) %>%
     summarize(b_m = mean(rating - mu - b_i - b_u - b_y) * w)

#make predictions on the test set
predicted_ratings <- testDS %>% 
     mutate(movieyear=as.numeric(str_extract(title,"(?<=\\()\\d{4}?(?=\\))"))) %>%
     left_join(movie_avgs_best, by='movieId') %>%
     left_join(user_avgs_best, by='userId') %>%
     left_join(year_avgs_best, by='movieyear') %>%
     left_join(month_avgs, by='ratemonth') %>%
     mutate(pred = mu + b_i + b_u + b_y + b_m) %>%
     .$pred

modfinish <- Sys.time()
benchmark <- round(difftime(modfinish, modstart, units = "secs"),2)

#calculate the RMSE
model_3_rmse <- RMSE(testDS$rating, predicted_ratings)

if(model_3_rmse < last_RMSE){
      month_avgs_best <- c()
      month_avgs_best <- month_avgs
      last_RMSE <- model_3_rmse 
   }

modelname <- paste("Rate Month Effect Model")

#show the results
month_results <- bind_rows(month_results,
                          data_frame(step = modelstep, 
                                     method=modelname,  
                                     RMSE = model_3_rmse,
                                     benchmark = benchmark,
                                     weight = w,
                                     lowcut = 0 ))
}

#plot the RMSE curve
month_results %>%   
  ggplot(aes(x=weight,y=RMSE)) + 
     geom_point() + 
     labs(title="Rating Month Effect Weights", 
        x="Weight", 
        y="RMSE")

#keep the best RMSE
month_best <-  month_results %>% slice(which.min(RMSE))

rmse_results <- bind_rows(rmse_results, month_best)

rmse_results %>% knitr::kable(label="Model Results")

month_best_RMSE <- month_best$RMSE
monthweight = month_best$weight



#setup variables
popweight <- 1
  
#determine the mean movie rating by popularity group
trainDS %>% 
     left_join(popgroups, by="movieId") %>%
     group_by(popgroup) %>%
     summarize(b_p = mean(rating))  %>% 
     ggplot(aes(b_p)) + 
     geom_histogram(bins = 10, color = "black", fill=palette[["popular"]]) + 
     labs(title="Popularity Ratings Histogram",
        x="Mean Rating", 
        y="Popularity Groups")


#determine the mean movie rating by popularity group adjusted for prior effects
pop_avgs <- trainDS %>% 
     left_join(popgroups, by="movieId") %>%
     left_join(user_avgs, by='userId') %>%
     left_join(movie_avgs, by='movieId') %>%
     left_join(year_avgs, by='movieyear') %>%
     left_join(month_avgs, by='ratemonth') %>%
     group_by(popgroup) %>%
     summarize(b_p = mean(rating - mu - b_i - b_u - b_y - b_m) * popweight) 

#plot a histogram
pop_avgs %>% ggplot(aes(b_p)) + 
     geom_histogram(bins = 10, color = "black", fill=palette[["popular"]]) + 
     labs(title="Popularity Effect Histogram",
        x="Ratings Change", 
        y="Popularity Groups")

#setup variables
popweights <- seq(.5,1.5,by=.1)
pop_results <- data.frame()
modelstep <- modelstep + 1

#the best prediction will be assigned to this variable
pop_avgs_best <- c()
last_RMSE <- 1000

for (w in popweights){
#start benchmark timer
modstart <- Sys.time()

#determine the mean movie rating by popularity group adjusted for prior effects
pop_avgs <- trainDS %>% 
     left_join(popgroups, by="movieId") %>%
     left_join(user_avgs_best, by='userId') %>%
     left_join(movie_avgs_best, by='movieId') %>%
     left_join(year_avgs_best, by='movieyear') %>%
     left_join(month_avgs_best, by='ratemonth') %>%
     group_by(popgroup) %>%
     summarize(b_p = mean(rating - mu - b_i - b_u - b_y - b_m) * w) 

#make predictions on the test set
predicted_ratings <- testDS %>% 
     left_join(popgroups, by="movieId") %>%
     mutate(movieyear=as.numeric(str_extract(title,"(?<=\\()\\d{4}?(?=\\))"))) %>%
     left_join(movie_avgs_best, by='movieId') %>%
     left_join(user_avgs_best, by='userId') %>%
     left_join(year_avgs_best, by='movieyear') %>%
     left_join(month_avgs_best, by='ratemonth') %>%
     left_join(pop_avgs, by='popgroup') %>%
     mutate(pred = mu + b_p + b_i + b_u + b_y + b_m) %>% 
      .$pred

#end benchmark timer
modfinish <- Sys.time()
benchmark <- round(difftime(modfinish, modstart, units = "secs"),2)

#calculate the RMSE
model_5_rmse <- RMSE(testDS$rating, predicted_ratings)


if(model_5_rmse < last_RMSE){
      pop_avgs_best <- c()
      pop_avgs_best <- pop_avgs
      last_RMSE <- model_5_rmse 
   }

modelname <- paste("Popularity Effect Model")

#show the results
pop_results <- bind_rows(pop_results,
                          data_frame(step = modelstep, 
                                     method=modelname,  
                                     RMSE = model_5_rmse,
                                     benchmark = benchmark,
                                     weight = w,
                                     lowcut = 0 ))


}

#plot the RMSE curve
pop_results %>%   
  ggplot(aes(x=weight,y=RMSE)) + 
     geom_point() + 
     labs(title="Popularity Effect Weights", 
        x="Weight", 
        y="RMSE")

#keep the best RMSE
pop_best <-  pop_results %>% slice(which.min(RMSE))

rmse_results <- bind_rows(rmse_results, pop_best)

rmse_results %>% knitr::kable(label="Model Results")

pop_best_RMSE <- pop_best$RMSE
popweight = pop_best$weight



#determine the mean movie rating by genre & era
trainDS %>% 
    group_by(era,genres) %>%
     summarize(b_ge = mean(rating))  %>% 
     ggplot(aes(b_ge)) + 
     geom_histogram(bins = 10, color = "black", fill=palette[["genreera"]]) + 
     labs(title="Genre/Era Ratings Histogram",
        x="Mean Rating", 
        y="Genre/Eras")

#determine the mean movie rating by year adjusted for prior effects
genreera_avgs <- trainDS %>% 
     left_join(popgroups, by="movieId") %>%   
     left_join(pop_avgs_best, by='popgroup') %>%
     left_join(movie_avgs, by='movieId') %>%
     left_join(user_avgs, by='userId') %>%
     left_join(year_avgs, by='movieyear') %>%
     left_join(month_avgs, by='ratemonth') %>%
     group_by(era,genres) %>%
     summarize(b_ge = mean(rating - mu - b_i - b_u - b_y - b_m  - b_p),
               rcount=n_distinct(rating),
               mcount=n_distinct(movieId),
               ucount=n_distinct(userId)) 


#plot a histogram
genreera_avgs %>%  ggplot(aes(b_ge)) + 
     geom_histogram(bins = 10, color = "black", fill=palette[["genreera"]]) + 
     labs(title="Genre/Era Effect Histogram",
        x="Ratings Change", 
        y="Genre/Eras")



#setup variables
genreera_results <- data.frame()
genreeraweights <- seq(.5,1.5,by=.1)
genreeralowcuts <- c(1,2,3)
modelstep <- modelstep + 1

#the best prediction will be assigned to this variable
predicted_ratings_genreera_best <- c()
last_RMSE <- 1000
for (w in genreeraweights){
  for (m in genreeralowcuts){
      
      #start benchmark timer
      modstart <- Sys.time()
     
      #apply a weight multiplier to the b_ge bias
      genreera_avgs_weighted <- genreera_avgs %>% mutate(b_ge = b_ge * w)

      #Set the bias to NA in cases where we feel there is not enough data in the training set to apply a prediction
      genreera_avgs_filtered <- genreera_avgs_weighted %>% 
        mutate(b_ge = ifelse(mcount < m,0,b_ge))
       
      #make predictions on the test set
      predicted_ratings_genreera <- testDS %>% 
           mutate(movieyear=as.numeric(str_extract(title,"(?<=\\()\\d{4}?(?=\\))"))) %>%
           left_join(popgroups, by="movieId") %>%   
           left_join(pop_avgs_best, by='popgroup') %>%
           left_join(movie_avgs_best, by='movieId') %>%
           left_join(user_avgs_best, by='userId') %>%
           left_join(year_avgs_best, by='movieyear') %>%
           left_join(month_avgs_best, by='ratemonth') %>%
           left_join(genreera_avgs_filtered, by=c('era','genres')) %>%
           mutate(pred = mu + b_i + b_u + b_y + b_m + b_p + b_ge) %>%
           .$pred
      
      #end benchmark timer
      modfinish <- Sys.time()
      benchmark <- round(difftime(modfinish, modstart, units = "secs"),2)
      
      modelname <- paste("Genre/Era Effects Model w/ Low Cut")
    
      #calculate the RMSE
      model_7_rmse <- RMSE(testDS$rating, predicted_ratings_genreera)
    
      #if this is the best RMSE - keep the table for use in next step
      if(model_7_rmse < last_RMSE){
        predicted_ratings_genreera_best <- c()
        predicted_ratings_genreera_best <- predicted_ratings_genreera
        last_RMSE <- model_7_rmse
      }
       
      #show the results
      genreera_results <- bind_rows(genreera_results,
                                data_frame(step = modelstep, 
                                           method=modelname,  
                                           RMSE = model_7_rmse,
                                           benchmark = benchmark,
                                           weight = w,
                                          lowcut = m))
      
  }
}

#plot the RMSE curves faceted by lowcuts
genreera_results %>%   
  ggplot() + 
     geom_point(aes(x=weight,y=RMSE)) + 
     facet_wrap(~lowcut) + 
      labs(title="Genre/Era Effect Low Cuts", 
        x="Weight", 
        y="RMSE")

#keep the best RMSE
genreera_best <-  genreera_results %>% slice(which.min(RMSE))

rmse_results <- bind_rows(rmse_results, genreera_best)

rmse_results %>% knitr::kable(label="Model Results")

genreeralowcut = genreera_best$lowcut
genreeraweight = genreera_best$weight


#determine the mean movie rating by genre & era
trainDS %>% 
    group_by(userId,genres) %>%
     summarize(b_gu = mean(rating))  %>% 
     ggplot(aes(b_gu)) + 
     geom_histogram(bins = 10, color = "black", fill=palette[["genreuser"]]) + 
     labs(title="Genre/User Ratings Histogram",
        x="Mean Rating", 
        y="Genre/Users")

#determine the mean movie rating by year adjusted for prior effects
genreuser_avgs <- trainDS %>% 
        left_join(popgroups, by="movieId") %>%   
        left_join(pop_avgs_best, by='popgroup') %>%
        left_join(movie_avgs_best, by='movieId') %>%
        left_join(user_avgs_best, by='userId') %>%
        left_join(year_avgs_best, by='movieyear') %>%
        left_join(month_avgs_best, by='ratemonth') %>%
        group_by(userId,genres) %>%
        summarize(b_gu = mean(rating - mu - b_i - b_u - b_y - b_m - b_p),
               ratecount=n(),
               mcount=n_distinct(movieId)) 


#plot a histogram
genreuser_avgs %>%  ggplot(aes(b_gu)) + 
     geom_histogram(bins = 10, color = "black", fill=palette[["genreuser"]]) + 
     labs(title="Genre/User Effect Histogram",
        x="Ratings Change", 
        y="Genre/Users")



#setup variables
genreuser_results <- data.frame()
genreuserweights <- seq(.1,.5,by=.1)
modelstep <- modelstep + 1
   
#set some reasonable values for a low cut on the number of movies that must have been rated to count this bias
#any genres that don't have this number, will use the previously defined genre/era bias
genreuserlowcuts <- c(1,2,3)

#the best prediction will be assigned to this variable
predicted_ratings_best <- c()
last_RMSE <- 1000
for (w in genreuserweights){
 for (z in genreuserlowcuts){
  
    #start benchmark timer
     modstart <- Sys.time()
 
    #apply a weight multiplier to the b_gu bias
    genreuser_avgs_weighted <- genreuser_avgs %>% 
      mutate(b_gu = b_gu * w)

    #Set the bias to NA in  cases where we feel there is not enough data in the training set to apply a prediction
    genreuser_avgs_adjusted <- genreuser_avgs_weighted %>% 
      mutate(b_gu = ifelse(mcount < z,NA,b_gu))
    
    #make predictions on the test set
    predicted_ratings_partial <- testDS %>% 
         mutate(movieyear=as.numeric(str_extract(title,"(?<=\\()\\d{4}?(?=\\))"))) %>%
         left_join(popgroups, by="movieId") %>%   
         left_join(pop_avgs_best, by='popgroup') %>%
         left_join(movie_avgs_best, by='movieId') %>%
         left_join(user_avgs_best, by='userId') %>%
         left_join(year_avgs_best, by='movieyear') %>%
         left_join(month_avgs_best, by='ratemonth') %>%
         left_join(genreuser_avgs_adjusted, by=c('userId','genres')) %>%
         mutate(pred = mu + b_i + b_u + b_y + b_m + b_p + b_gu) %>%
         .$pred

    #fill in NA values with values from the genre_era predictions
    predicted_ratings <- coalesce(predicted_ratings_partial, predicted_ratings_genreera)
    
    #end benchmark timer
    modfinish <- Sys.time()
    benchmark <- round(difftime(modfinish, modstart, units = "secs"),2)
    
    #calculate the RMSE
    model_8_rmse <- RMSE(testDS$rating, predicted_ratings)
    
     if(model_8_rmse < last_RMSE){
      predicted_ratings_best <- c()
      predicted_ratings_best <- predicted_ratings
      last_RMSE <- model_8_rmse
    }
     
    modelname <- paste("Genre/User Effects Model w/ Low Cut")
    
    #show the results
    genreuser_results <- bind_rows(genreuser_results,
                              data_frame(step = modelstep, 
                                         method=modelname,  
                                         RMSE = model_8_rmse,
                                         benchmark = benchmark,
                                         weight = w,
                                         lowcut = z))

 }
}

#plot the RMSE curves faceted by lowcuts
genreuser_results %>%   
  ggplot(aes(x=weight,y=RMSE)) + 
     geom_point() + 
    facet_wrap(~lowcut) + 
     labs(title="Genre/User Effect Low Cuts", 
        x="Weight", 
        y="RMSE")

#keep the best RMSE
genreuser_best <-  genreuser_results %>% slice(which.min(RMSE))

rmse_results <- bind_rows(rmse_results, genreuser_best)

rmse_results %>% knitr::kable(label="Model Results")

genreuserlowcut = genreuser_best$lowcut
genreuserweight = genreuser_best$weight


#this helper function will round any values above 5 or below .5 since these values can only increase the RMSE
bracket <- function(v){
  v <- max(min(v,5),.5)
  v
}

#alter predictions with the bracket function
predicted_ratings <- sapply(predicted_ratings_best,bracket)

#calculate the RMSE
model_5R_rmse <- RMSE(testDS$rating,predicted_ratings)
modelstep <- modelstep + 1

#show the results
rmse_results <- bind_rows(rmse_results,
                          data_frame(step = modelstep, 
                                     method="Multi-Effect Model w/ Bracket",  
                                     RMSE = model_5R_rmse,
                                     benchmark = benchmark ))
rmse_results %>% knitr::kable(label="Model Results")

#this model will take a given training and test set and return parmeters for optimization.
#the average parameters will be plugged in to the final multieffect model

trainingmodel <- function(trainDS,
                          testDS,
                          movieweights,
                          userweights,
                          yearweights,
                          monthweights,
                          popweights,
                          genreeraweights,
                          genreuserweights,
                          genreeralowcuts,
                          genreuserlowcuts){

#IMPORTANT - Resetting mu here to the training set
#this is the baseline to which all effects will be added/subtracted
mu <- mean(trainDS$rating)
predictions <- rep(mu, nrow(trainDS))

#create a vector of predictions that are all the same (mu) value
#modstart and modfinish will time the routine for benchmarking
modstart <- Sys.time()
predictions <- rep(mu, nrow(trainDS))
modfinish <- Sys.time()

#get the RMSE of the naive model
naive_rmse <- RMSE(testDS$rating,predictions)

#determine the operation speed. This operation will be done for each model
benchmark <- round(difftime(modfinish, modstart, units = "secs"),2)

#modelstep will increment for each new test
modelstep <- 0
modelstep <- modelstep + 1

#show the RMSE results in a kable
rmse_results <- data_frame(step=modelstep,
                           method = "Baseline", 
                           RMSE = naive_rmse, 
                           benchmark = benchmark,
                                     weight = 1,
                                     lowcut = 0)


#-----MOVIE EFFECTS ------


#setup variables
movie_results <- data.frame()
modelstep <- modelstep + 1

#the best prediction will be assigned to this variable
movie_avgs_best <- c()
last_RMSE <- 1000

for (w in movieweights){

modstart <- Sys.time()

movie_avgs <- trainDS %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu) * w)

#using the testDS set only to make a prediction.
predicted_ratings <- mu + testDS %>% 
     left_join(movie_avgs, by='movieId') %>%
     .$b_i

modfinish <- Sys.time()
benchmark <- round(difftime(modfinish, modstart, units = "secs"),2)

#check prediction vs the results in the testDS set
model_1_rmse <- RMSE(testDS$rating, predicted_ratings)

if(model_1_rmse < last_RMSE){
      movie_avgs_best <- c()
      movie_avgs_best <- movie_avgs
      last_RMSE <- model_1_rmse
    }
    

modelname <- paste("Movie Effect Model")

#checking my prediction vs the results in the testDS set
movie_results <- bind_rows(movie_results,
                          data_frame(step = modelstep, 
                                     method=modelname,
                                     RMSE = model_1_rmse, 
                                     benchmark = benchmark,
                                     weight = w,
                                     lowcut = 0
                                     ))
}

#keep the best RMSE
movie_best <-  movie_results %>% slice(which.min(RMSE))

rmse_results <- bind_rows(rmse_results, movie_best)

movieweight = movie_best$weight


#-----USER EFFECTS ------


#setup variables
user_results <- data.frame()
modelstep <- modelstep + 1

#the best prediction will be assigned to this variable
user_avgs_best <- c()
last_RMSE <- 1000

for (w in userweights){
modstart <- Sys.time()

#determine the mean user rating after deducting prior effects
user_avgs <- trainDS %>% 
     left_join(movie_avgs_best, by='movieId') %>%
     group_by(userId) %>%
     summarize(b_u = mean(rating - mu - b_i) * w) 
  
#predict ratings on the test set
predicted_ratings <- testDS %>% 
     left_join(movie_avgs_best, by='movieId') %>%
     left_join(user_avgs, by='userId') %>%
     mutate(pred = mu + b_i + b_u) %>%
     .$pred

modfinish <- Sys.time()
benchmark <- round(difftime(modfinish, modstart, units = "secs"),2)

modelname <- paste("User Effect Model")

#calculate the RMSE
model_2_rmse <- RMSE(testDS$rating, predicted_ratings)


if(model_2_rmse < last_RMSE){
      user_avgs_best <- c()
      user_avgs_best <- user_avgs
      last_RMSE <- model_2_rmse 
   }

    
#show the results
user_results <- bind_rows(user_results,
                          data_frame(step = modelstep, 
                                     method=modelname,  
                                     RMSE = model_2_rmse,
                                     benchmark = benchmark,
                                     weight = w,
                                     lowcut = 0
                                     ))
}

#keep the best RMSE
user_best <-  user_results %>% slice(which.min(RMSE))

rmse_results <- bind_rows(rmse_results, user_best)

userweight = user_best$weight


#-----YEAR EFFECTS ------

#setup variables
year_results <- data.frame()
modelstep <- modelstep + 1

#the best prediction will be assigned to this variable
year_avgs_best <- c()
last_RMSE <- 1000

for (w in yearweights){

modstart <- Sys.time()

year_avgs <- trainDS %>% 
     left_join(movie_avgs_best, by='movieId') %>%
     left_join(user_avgs_best, by='userId') %>%
     group_by(movieyear) %>%
     summarize(b_y = mean(rating - mu - b_i - b_u) * w)

#make predictions on the test set
predicted_ratings <- testDS %>% 
     mutate(movieyear=as.numeric(str_extract(title,"(?<=\\()\\d{4}?(?=\\))"))) %>%
     left_join(movie_avgs_best, by='movieId') %>%
     left_join(user_avgs_best, by='userId') %>%
     left_join(year_avgs, by='movieyear') %>%
     mutate(pred = mu + b_i + b_u + b_y) %>%
     .$pred

modfinish <- Sys.time()
benchmark <- round(difftime(modfinish, modstart, units = "secs"),2)

#calculate the RMSE
model_3_rmse <- RMSE(testDS$rating, predicted_ratings)

if(model_3_rmse < last_RMSE){
      year_avgs_best <- c()
      year_avgs_best <- year_avgs
      last_RMSE <- model_3_rmse 
   }

modelname <- paste("Year Effect Model")

#show the results
year_results <- bind_rows(year_results,
                          data_frame(step = modelstep, 
                                     method=modelname,  
                                     RMSE = model_3_rmse,
                                     benchmark = benchmark,
                                     weight = w,
                                     lowcut = 0))

}


#keep the best RMSE
year_best <-  year_results %>% slice(which.min(RMSE))

rmse_results <- bind_rows(rmse_results, year_best)

yearweight = year_best$weight


#-----RATING MONTH EFFECTS ------


#setup variables
month_results <- data.frame()
modelstep <- modelstep + 1

#the best prediction will be assigned to this variable
month_avgs_best <- c()
last_RMSE <- 1000

for (w in monthweights){

modstart <- Sys.time()

month_avgs <- trainDS %>% 
     left_join(movie_avgs_best, by='movieId') %>%
     left_join(user_avgs_best, by='userId') %>%
     left_join(year_avgs_best, by='movieyear') %>%
     group_by(ratemonth) %>%
     summarize(b_m = mean(rating - mu - b_i - b_u - b_y) * w)

#make predictions on the test set
predicted_ratings <- testDS %>% 
     mutate(movieyear=as.numeric(str_extract(title,"(?<=\\()\\d{4}?(?=\\))"))) %>%
     left_join(movie_avgs_best, by='movieId') %>%
     left_join(user_avgs_best, by='userId') %>%
     left_join(year_avgs_best, by='movieyear') %>%
     left_join(month_avgs, by='ratemonth') %>%
     mutate(pred = mu + b_i + b_u + b_y + b_m) %>%
     .$pred

modfinish <- Sys.time()
benchmark <- round(difftime(modfinish, modstart, units = "secs"),2)

#calculate the RMSE
model_4_rmse <- RMSE(testDS$rating, predicted_ratings)

if(model_4_rmse < last_RMSE){
      month_avgs_best <- c()
      month_avgs_best <- month_avgs
      last_RMSE <- model_4_rmse 
   }

modelname <- paste("Rate Month Effect Model")

#show the results
month_results <- bind_rows(month_results,
                          data_frame(step = modelstep, 
                                     method=modelname,  
                                     RMSE = model_4_rmse,
                                     benchmark = benchmark,
                                     weight = w,
                                     lowcut = 0 ))
}

#keep the best RMSE
month_best <-  month_results %>% slice(which.min(RMSE))

rmse_results <- bind_rows(rmse_results, month_best)

monthweight = month_best$weight


#-----POPULARITY EFFECTS ------


#setup variables
pop_results <- data.frame()
modelstep <- modelstep + 1

#the best prediction will be assigned to this variable
pop_avgs_best <- c()
last_RMSE <- 1000

for (w in popweights){
#start benchmark timer
modstart <- Sys.time()

#determine the mean movie rating by popularity group adjusted for prior effects
pop_avgs <- trainDS %>% 
     left_join(popgroups, by="movieId") %>%
     left_join(user_avgs_best, by='userId') %>%
     left_join(movie_avgs_best, by='movieId') %>%
     left_join(year_avgs_best, by='movieyear') %>%
     left_join(month_avgs_best, by='ratemonth') %>%
     group_by(popgroup) %>%
     summarize(b_p = mean(rating - mu - b_i - b_u - b_y - b_m) * w) 

#make predictions on the test set
predicted_ratings <- testDS %>% 
     left_join(popgroups, by="movieId") %>%
     mutate(movieyear=as.numeric(str_extract(title,"(?<=\\()\\d{4}?(?=\\))"))) %>%
     left_join(movie_avgs_best, by='movieId') %>%
     left_join(user_avgs_best, by='userId') %>%
     left_join(year_avgs_best, by='movieyear') %>%
     left_join(month_avgs_best, by='ratemonth') %>%
     left_join(pop_avgs, by='popgroup') %>%
     mutate(pred = mu + b_p + b_i + b_u + b_y + b_m) %>% 
      .$pred

#end benchmark timer
modfinish <- Sys.time()
benchmark <- round(difftime(modfinish, modstart, units = "secs"),2)

#calculate the RMSE
model_5_rmse <- RMSE(testDS$rating, predicted_ratings)


if(model_5_rmse < last_RMSE){
      pop_avgs_best <- c()
      pop_avgs_best <- pop_avgs
      last_RMSE <- model_5_rmse 
   }

modelname <- paste("Popularity Effect Model")

#show the results
pop_results <- bind_rows(pop_results,
                          data_frame(step = modelstep, 
                                     method=modelname,  
                                     RMSE = model_5_rmse,
                                     benchmark = benchmark,
                                     weight = w,
                                     lowcut = 0 ))


}


#keep the best RMSE
pop_best <-  pop_results %>% slice(which.min(RMSE))

rmse_results <- bind_rows(rmse_results, pop_best)

rmse_results %>% knitr::kable(label="Model Results")

popweight = pop_best$weight

#-----GENRE/ERA EFFECTS ------

#determine the mean movie rating by year adjusted for prior effects
genreera_avgs <- trainDS %>% 
     left_join(popgroups, by="movieId") %>%   
     left_join(pop_avgs_best, by='popgroup') %>%
     left_join(movie_avgs, by='movieId') %>%
     left_join(user_avgs, by='userId') %>%
     left_join(year_avgs, by='movieyear') %>%
     left_join(month_avgs, by='ratemonth') %>%
     group_by(era,genres) %>%
     summarize(b_ge = mean(rating - mu - b_i - b_u - b_y - b_m  - b_p),
               rcount=n_distinct(rating),
               mcount=n_distinct(movieId),
               ucount=n_distinct(userId)) 



#setup variables
genreera_results <- data.frame()
modelstep <- modelstep + 1

#the best prediction will be assigned to this variable
predicted_ratings_genreera_best <- c()
last_RMSE <- 1000
for (w in genreeraweights){
  for (m in genreeralowcuts){
      
      #start benchmark timer
      modstart <- Sys.time()
     
      #apply a weight multiplier to the b_ge bias
      genreera_avgs_weighted <- genreera_avgs %>% mutate(b_ge = b_ge * w)

      #Set the bias to NA in cases where we feel there is not enough data in the training set to apply a prediction
      genreera_avgs_filtered <- genreera_avgs_weighted %>% 
        mutate(b_ge = ifelse(mcount < m,0,b_ge))
       
      #make predictions on the test set
      predicted_ratings_genreera <- testDS %>% 
           mutate(movieyear=as.numeric(str_extract(title,"(?<=\\()\\d{4}?(?=\\))"))) %>%
           left_join(popgroups, by="movieId") %>%   
           left_join(pop_avgs_best, by='popgroup') %>%
           left_join(movie_avgs_best, by='movieId') %>%
           left_join(user_avgs_best, by='userId') %>%
           left_join(year_avgs_best, by='movieyear') %>%
           left_join(month_avgs_best, by='ratemonth') %>%
           left_join(genreera_avgs_filtered, by=c('era','genres')) %>%
           mutate(pred = mu + b_i + b_u + b_y + b_m + b_p + b_ge) %>%
           .$pred
      
      #end benchmark timer
      modfinish <- Sys.time()
      benchmark <- round(difftime(modfinish, modstart, units = "secs"),2)
      
      modelname <- paste("Genre/Era Effects Model w/ Low Cut")
    
      #calculate the RMSE
      model_6_rmse <- RMSE(testDS$rating, predicted_ratings_genreera)
    
      #if this is the best RMSE - keep the table for use in next step
      if(model_6_rmse < last_RMSE){
        predicted_ratings_genreera_best <- c()
        predicted_ratings_genreera_best <- predicted_ratings_genreera
        last_RMSE <- model_6_rmse
      }
       
      #show the results
      genreera_results <- bind_rows(genreera_results,
                                data_frame(step = modelstep, 
                                           method=modelname,  
                                           RMSE = model_6_rmse,
                                           benchmark = benchmark,
                                           weight = w,
                                          lowcut = m))
      
  }
}


#keep the best RMSE
genreera_best <-  genreera_results %>% slice(which.min(RMSE))

rmse_results <- bind_rows(rmse_results, genreera_best)

genreeralowcut = genreera_best$lowcut
genreeraweight = genreera_best$weight



#-----GENRE/USER EFFECTS ------



#determine the mean movie rating by year adjusted for prior effects
genreuser_avgs <- trainDS %>% 
        left_join(popgroups, by="movieId") %>%   
        left_join(pop_avgs_best, by='popgroup') %>%
        left_join(movie_avgs_best, by='movieId') %>%
        left_join(user_avgs_best, by='userId') %>%
        left_join(year_avgs_best, by='movieyear') %>%
        left_join(month_avgs_best, by='ratemonth') %>%
        group_by(userId,genres) %>%
        summarize(b_gu = mean(rating - mu - b_i - b_u - b_y - b_m - b_p),
               ratecount=n(),
               mcount=n_distinct(movieId)) 



#setup variables
genreuser_results <- data.frame()
modelstep <- modelstep + 1
   
#the best prediction will be assigned to this variable
predicted_ratings_best <- c()
last_RMSE <- 1000
for (w in genreuserweights){
 for (z in genreuserlowcuts){
  
    #start benchmark timer
     modstart <- Sys.time()
 
    #apply a weight multiplier to the b_gu bias
    genreuser_avgs_weighted <- genreuser_avgs %>% 
      mutate(b_gu = b_gu * w)

    #Set the bias to NA in  cases where we feel there is not enough data in the training set to apply a prediction
    genreuser_avgs_adjusted <- genreuser_avgs_weighted %>% 
      mutate(b_gu = ifelse(mcount < z,NA,b_gu))
    
    #make predictions on the test set
    predicted_ratings_partial <- testDS %>% 
         mutate(movieyear=as.numeric(str_extract(title,"(?<=\\()\\d{4}?(?=\\))"))) %>%
         left_join(popgroups, by="movieId") %>%   
         left_join(pop_avgs_best, by='popgroup') %>%
         left_join(movie_avgs_best, by='movieId') %>%
         left_join(user_avgs_best, by='userId') %>%
         left_join(year_avgs_best, by='movieyear') %>%
         left_join(month_avgs_best, by='ratemonth') %>%
         left_join(genreuser_avgs_adjusted, by=c('userId','genres')) %>%
         mutate(pred = mu + b_i + b_u + b_y + b_m + b_p + b_gu) %>%
         .$pred

    #fill in NA values with values from the genre_era predictions
    predicted_ratings <- coalesce(predicted_ratings_partial, predicted_ratings_genreera)
    
    #end benchmark timer
    modfinish <- Sys.time()
    benchmark <- round(difftime(modfinish, modstart, units = "secs"),2)
    
    #calculate the RMSE
    model_7_rmse <- RMSE(testDS$rating, predicted_ratings)
    
     if(model_7_rmse < last_RMSE){
      predicted_ratings_best <- c()
      predicted_ratings_best <- predicted_ratings
      last_RMSE <- model_7_rmse
    }
     
    modelname <- paste("Genre/User Effects Model w/ Low Cut")
    
    #show the results
    genreuser_results <- bind_rows(genreuser_results,
                              data_frame(step = modelstep, 
                                         method=modelname,  
                                         RMSE = model_7_rmse,
                                         benchmark = benchmark,
                                         weight = w,
                                         lowcut = z))

 }
}


#keep the best RMSE
genreuser_best <-  genreuser_results %>% slice(which.min(RMSE))

rmse_results <- bind_rows(rmse_results, genreuser_best)

genreuserlowcut = genreuser_best$lowcut
genreuserweight = genreuser_best$weight


#alter predictions with the bracket function
predicted_ratings <- sapply(predicted_ratings_best,bracket)

#calculate the RMSE
model_8_rmse <- RMSE(testDS$rating,predicted_ratings)
modelstep <- modelstep + 1


#show the results
rmse_results <- bind_rows(rmse_results,
                          data_frame(step = modelstep, 
                                     method="Multi-Effect Model w/ Bracket",  
                                     RMSE = model_8_rmse,
                                     benchmark = benchmark ))


#return the RMSE, prediction, penalty factor, & benchmark time
      returnobj <-list(table=rmse_results)

      class(returnobj) <- "resulttable"
      returnobj

  
}

#now we will use the three partitions already created and try to get the best
#weight/lowcut settings by averaging the results of three different splits of the data.
#this code takes a while to run.

train_A <- trainingmodel(part9010_A$trainDS,
                         part9010_A$testDS,
                          movieweights,
                          userweights,
                          yearweights,
                          monthweights,
                          popweights,
                          genreeraweights,
                          genreuserweights,
                          genreeralowcuts,
                          genreuserlowcuts)

train_B <- trainingmodel(part9010_B$trainDS,
                         part9010_B$testDS,
                          movieweights,
                          userweights,
                          yearweights,
                          monthweights,
                          popweights,
                          genreeraweights,
                          genreuserweights,
                          genreeralowcuts,
                          genreuserlowcuts)

train_C <- trainingmodel(part9010_C$trainDS,
                         part9010_C$testDS,
                          movieweights,
                          userweights,
                          yearweights,
                          monthweights,
                          popweights,
                          genreeraweights,
                          genreuserweights,
                          genreeralowcuts,
                          genreuserlowcuts)

#this function will return an average of the 3 parameters
getmean <- function(tbl1,tbl2,tbl3,row,col){
  v1 <- tbl1$table[row,col]
  v2 <- tbl2$table[row,col]
  v3 <- tbl3$table[row,col]
  mean <- (v1+v2+v3) / 3
}

#this block sets the final single parameter values to be used in the final model
avgmovieweight <- getmean(train_A,train_B,train_C,2,5)$weight
avguserweight <- getmean(train_A,train_B,train_C,3,5)$weight
avgyearweight <- getmean(train_A,train_B,train_C,4,5)$weight
avgmonthweight <- getmean(train_A,train_B,train_C,5,5)$weight
avgpopweight <- getmean(train_A,train_B,train_C,6,5)$weight
avggenreeraweight <- getmean(train_A,train_B,train_C,7,5)$weight
avggenreuserweight <- getmean(train_A,train_B,train_C,8,5)$weight
avggenreeralowcut <- round(getmean(train_A,train_B,train_C,7,6))$lowcut
avggenreuserlowcut <- round(getmean(train_A,train_B,train_C,8,6))$lowcut

#build the multi-effect model model function
#we have changed the code to use sum/n, as we will later attempt regularization
#the model will round off predictions over 5 and under .5 using the bracket function

multieffectmodel <- function(trainDS,
                             testDS,
                             movieweight,
                             userweight,
                             yearweight,
                             monthweight,
                             popweight,
                             genreeraweight,
                             genreuserweight,
                             genreeralowcut,
                             genreuserlowcut,
                             lambda){
    returnobj <- NULL
    returnobj <- data.frame()
    
    #start benchmark timer
    modstart <- Sys.time()

    #set mu baseline rating
    mu <- mean(trainDS$rating)
    
    #add the movie effect 
    b_i <- trainDS %>%
              group_by(movieId) %>%
              summarize(b_i = sum(rating - mu) * 
                          movieweight /(n()+lambda))
     
    #add the user effect
    b_u <- trainDS %>% 
              left_join(b_i, by="movieId") %>%
              group_by(userId) %>%
              summarize(b_u = sum(rating - b_i - mu) * 
                          userweight / (n()+lambda)) 
     
    #add the year effect
    b_y <- trainDS %>%
              left_join(b_i, by="movieId") %>%
              left_join(b_u, by="userId") %>%
              group_by(movieyear) %>%
              summarize(b_y = sum(rating - mu - b_i - b_u) * 
                          yearweight / (n()+lambda)) 
    
    #add the month effect
    b_m <- trainDS %>%
              left_join(b_i, by="movieId") %>%
              left_join(b_u, by="userId") %>%
              left_join(b_y, by="movieyear") %>%
              group_by(ratemonth) %>%
              summarize(b_m = sum(rating - mu - b_i - b_u - b_y) * 
                          monthweight / (n()+lambda)) 
    
    #add the popularity effect
    b_p <- trainDS %>% 
              left_join(popgroups, by="movieId") %>%
              left_join(b_i, by="movieId") %>%
              left_join(b_u, by="userId") %>%
              left_join(b_y, by="movieyear") %>%
              left_join(b_m, by="ratemonth") %>%
              group_by(popgroup) %>%
              summarize(b_p = sum(rating - mu - b_i - b_u - b_y - b_m) * 
                          popweight / (n()+lambda)) 
   
    
    # generate predictions for first set
    b_ge <- trainDS %>% 
              left_join(popgroups, by="movieId") %>%   
              left_join(b_i, by="movieId") %>%
              left_join(b_u, by="userId") %>%
              left_join(b_y, by="movieyear") %>%
              left_join(b_m, by="ratemonth") %>%
              left_join(b_p, by = "popgroup") %>% 
              group_by(era,genres) %>%
              summarize(b_ge = sum(rating - mu - b_i - b_u - b_y - b_m - b_p) * 
                          genreeraweight / (n()+lambda),
               rcount=n_distinct(rating),
               mcount=n_distinct(movieId),
               ucount=n_distinct(userId)) 
    
      #Set the bias to 0 in  cases where there is not enough data 
      b_ge <- b_ge %>% mutate(b_ge = ifelse(mcount < genreeralowcut,0,b_ge))
     
      #test using part of the data to predict the rest
      predicted_ratings_genreera <- testDS %>% 
           left_join(popgroups, by="movieId") %>%
                left_join(b_i, by = "movieId") %>%
                left_join(b_u, by = "userId") %>%
                left_join(b_y, by = "movieyear") %>%
                left_join(b_m, by = "ratemonth") %>%
                left_join(b_p, by = "popgroup") %>% 
                left_join(b_ge, by=c('era','genres')) %>% 
                mutate(pred = mu + b_i + b_u + b_y + b_m + b_p + b_ge) %>%
       .$pred
    
      # generate predictions for second set
      b_gu <- trainDS %>% 
              left_join(popgroups, by="movieId") %>%   
              left_join(b_i, by="movieId") %>%
              left_join(b_u, by="userId") %>%
              left_join(b_y, by="movieyear") %>%
              left_join(b_m, by="ratemonth") %>%
              left_join(b_p, by = "popgroup") %>% 
              group_by(userId,genres) %>%
              summarize(b_gu = sum(rating - mu - b_i - b_u - b_y - b_m - b_p) * 
                          genreuserweight / (n()+lambda),
              rcount=n(),
              mcount=n_distinct(movieId)) 
    
      #Set the bias to NA in  cases where there is not enough data 
      b_gu <- b_gu %>% mutate(b_gu = ifelse(mcount < genreuserlowcut,NA,b_gu))
     
   
      #test using part of the data to predict the rest
      predicted_ratings_genreuser <- testDS %>% 
           left_join(popgroups, by="movieId") %>%
                left_join(b_i, by = "movieId") %>%
                left_join(b_u, by = "userId") %>%
                left_join(b_y, by = "movieyear") %>%
                left_join(b_m, by = "ratemonth") %>%
                left_join(b_p, by = "popgroup") %>% 
                left_join(b_gu, by=c('userId','genres')) %>% 
                mutate(pred = mu + b_i + b_u + b_y + b_m + b_p + b_gu) %>%
       .$pred
    
      #combine the genre/era predictions with the genre/user predictions where genre/user combos are NA
      predicted_ratings <- coalesce(predicted_ratings_genreuser, predicted_ratings_genreera)
    
      #run RMSE and return RMSE as numeric
      pred_rate <- as.numeric(predicted_ratings)
      pred_comp <- as.numeric(testDS$rating)
  
      #sapply the bracket function
      pred_raterounded <- sapply(pred_rate,bracket)
      
      #Model is finished/predictions are produced, compute benchmark
      modfinish <- Sys.time()
      bm <- round(difftime(modfinish, modstart, units = "secs"),2)
      
      #calculate the RMSE
      model_0A_rmse <- RMSE(pred_comp,pred_raterounded)
   
      #return the RMSE, prediction, penalty factor, & benchmark time
      returnobj <-list(RMSE=model_0A_rmse,
                  pred=pred_raterounded,
                  penalty = lambda,
                  benchmark = bm
                  )

      class(returnobj) <- "modeltest"
      returnobj
         
}



#cross validate against 3 training partitions

#10% training 90% test
trainDS <-part9010_A$trainDS
testDS <- part9010_A$testDS

#check the RMSE on this partition
multimodel_result <- multieffectmodel(trainDS,
                                  testDS,
                                  avgmovieweight,
                                  avguserweight,
                                  avgyearweight,
                                  avgmonthweight,
                                  avgpopweight,
                                  avggenreeraweight,
                                  avggenreuserweight,
                                  avggenreeralowcut,
                                  avggenreuserlowcut,
                                  0)

edx_9010_rmse <- multimodel_result$RMSE


#try lambdas between 0 and 2 at .5 intervals
#this is a slow operation as it must run the model multiple times
lambdas <- seq(0, 2, .5)

modstart <- Sys.time()

#interate different regularization settings & return the RMSE of each
rmses <- sapply(lambdas, function(lambda){
  return(multieffectmodel(trainDS,
                          testDS,
                          avgmovieweight,
                          avguserweight,
                          avgyearweight,
                          avgmonthweight,
                          avgpopweight,
                          avggenreeraweight,
                          avggenreuserweight,
                          avggenreeralowcut,
                          avggenreuserlowcut,
                          lambda)$RMSE)
})

#plot the RMSE w/ different lambda values
qplot(lambdas, rmses)  

#get the lowest RMSE lambda
lambda <- lambdas[which.min(rmses)]

#for consistency - run the model again (once) with the best lambda passed in
#the RMSE should match the regularization loop above
rmse_regularized <- multieffectmodel(trainDS,
                                  testDS,
                                  avgmovieweight,
                                  avguserweight,
                                  avgyearweight,
                                  avgmonthweight,
                                  avgpopweight,
                                  avggenreeraweight,
                                  avggenreuserweight,
                                  avggenreeralowcut,
                                  avggenreuserlowcut,
                                  lambda)

modelstep <- modelstep + 1

#show the results
rmse_results <- bind_rows(rmse_results,
                          data_frame(step = modelstep, 
                                     method="Regularized Multi Effect Model - Test",  
                                     RMSE = rmse_regularized$RMSE,
                                     benchmark=rmse_regularized$benchmark))
rmse_results %>% knitr::kable(label="Model Results")


# the final test uses the weight, lowcut and lambda values 
# derived from the previous code blocks which did not use the validation/hold out data

# using the original 9,000,055 edx data to train the model 
trainDS <- edx %>% 
           mutate(ratedate = as.Date(as.POSIXct(timestamp, origin="1970-01-01"))) %>% 
           mutate(ratemonth = format(ratedate, "%m")) %>% 
           mutate(movieyear=as.numeric(str_extract(title,"(?<=\\()\\d{4}?(?=\\))"))) %>%
           left_join(mv_era, by = "movieyear")

# the 999,999 row validation data is used only to make predictions
# the ratings in this dataset are used only for comparison with predictions to calculate RMSE
testDS <- validation %>% 
           mutate(ratedate = as.Date(as.POSIXct(timestamp, origin="1970-01-01"))) %>% 
           mutate(ratemonth = format(ratedate, "%m")) %>% 
           mutate(movieyear=as.numeric(str_extract(title,"(?<=\\()\\d{4}?(?=\\))"))) %>%
           left_join(mv_era, by = "movieyear")

#run the final model
rmse_validate <- multieffectmodel(trainDS,
                                  testDS,
                                  avgmovieweight,
                                  avguserweight,
                                  avgyearweight,
                                  avgmonthweight,
                                  avgpopweight,
                                  avggenreeraweight,
                                  avggenreuserweight,
                                  avggenreeralowcut,
                                  avggenreuserlowcut,
                                  lambda)

modelstep <- modelstep + 1

#show final results
rmse_results <- bind_rows(rmse_results,
                          data_frame(step = modelstep, 
                                     method="Regularized Multi-Effect Model - FINAL",  
                                     RMSE = rmse_validate$RMSE,
                                     benchmark=rmse_validate$benchmark))
rmse_results %>% knitr::kable(label="Model Results")

#set a variable for the linear model final RMSE
rmsefinal_lm <- rmse_validate$RMSE


#sample 100000 rows randomly
edx_mini <- sample_n(edx_fortify, 1000000,replace=F)

#90/10 Partition
set.seed(1, sample.kind="Rounding")
edx_mini_train_index <- createDataPartition(y = edx_mini$rating, 
                                           times = 1, p = 0.5, list = FALSE)

edx_mini_train <- edx_mini[-edx_mini_train_index,]
edx_mini_temp <- edx_mini[edx_mini_train_index,]

# Make sure userId and movieId in validation set are also in edx set
edx_mini_valid <- edx_mini_temp %>% 
  semi_join(edx_mini_train, by = "movieId") %>%
  semi_join(edx_mini_train, by = "userId")

# Add rows removed from validation set back into edx set
edx_mini_removed <- anti_join(edx_mini_temp, edx_mini_valid)
edx_mini_train <- rbind(edx_mini_train, edx_mini_removed)

rm(edx_mini_temp)
rm(edx_mini_removed)


#-------------------------------------------------------


#set which datasets to work with
trainDS <- edx_mini_train
testDS <- edx_mini_valid

#edx set is used top train the Recosystem model
trymatrix <- trainDS %>% select(userId, movieId, rating)
trymatrix <- as.matrix(trymatrix)


#validation set is used ONLY to make predicted ratings, thus the rating column may be removed
checkmatrix <- testDS %>% select(userId, movieId)
checkmatrix <- as.matrix(checkmatrix)
       

# create text files for analysis 
write.table(trymatrix , file = "./trysettest.txt" , 
            sep = " " , row.names = FALSE, col.names = FALSE)

write.table(checkmatrix, file = "./checksettest.txt" , 
            sep = " " , row.names = FALSE, col.names = FALSE)

#set the random seed for operations below
# read the files
set.seed(1)
try_set <- data_file("./trysettest.txt")
check_set <- data_file("./checksettest.txt")



#instantiate reco object
obj_reco <- Reco()
modstart <- Sys.time()

# tune training set. 
tuner <- obj_reco$tune(try_set, 
                       opts = list(dim = c(10,20), 
                                    lrate = c(0.1),
                                    costp_l1 = 0, costq_l1 = 0,
                                    nthread = 1, niter = 10))


#train the model
obj_reco$train(try_set, opts = c(tuner$min, nthread = 1, niter = 10))

# Making prediction on validation set and calculate RMSE
# Recosystem saves the data to file reduce memory usage
tf <- "./predicttest.txt"

 #send the prediction to file
obj_reco$predict(check_set, out_file(tf))  


modfinish <-Sys.time()
benchmark <- round(difftime(modfinish, modstart, units = "secs"),2)
modelstep <- modelstep + 1

#get the prediction
predict_rating <- scan(tf)

 #apply the bracket function
pred_raterounded_mini <- sapply(predict_rating,bracket)

#save the final RMSE + return it
rmsetest_mf <- RMSE(testDS$rating,pred_raterounded_mini) 


#show RMSE Results
rmse_results <- bind_rows(rmse_results,
                          data_frame(step = modelstep, 
                                     method="Recosystem - Test",  
                                     RMSE = rmsetest_mf,
                                     benchmark=benchmark))

rmse_results %>% knitr::kable(label="MF Test Results")


#set which datasets to work with - setting to FINAL HOLD OUT DATA
trainDS <- edx
testDS <- validation


#edx set is used top train the Recosystem model
trymatrix <- trainDS %>% select(userId, movieId, rating)
trymatrix <- as.matrix(trymatrix)


#validation set is used ONLY to compare to the predicted ratings
checkmatrix <- testDS %>% select(userId, movieId)
checkmatrix <- as.matrix(checkmatrix)
       

# create text files for analysis 
write.table(trymatrix , file = "./trysetfinal.txt" , 
            sep = " " , row.names = FALSE, col.names = FALSE)

write.table(checkmatrix, file = "./checksetfinal.txt" , 
            sep = " " , row.names = FALSE, col.names = FALSE)

#set the random seed for operations below
# read the files
set.seed(1)
try_set <- data_file("./trysetfinal.txt")
check_set <- data_file("./checksetfinal.txt")



#instantiate reco object
obj_reco <- Reco()
modstart <- Sys.time()

# tune training set. 
tuner <- obj_reco$tune(try_set, 
                       opts = list(dim = c(10,20), 
                                    lrate = c(0.1),
                                    costp_l1 = 0, costq_l1 = 0,
                                    nthread = 1, niter = 10))


#train the model
obj_reco$train(try_set, opts = c(tuner$min, nthread = 1, niter = 10))

# Making prediction on validation set and calculate RMSE
# Recosystem saves the data to file reduce memory usage
tf <- "./predictfinal.txt"

 #send the prediction to file
obj_reco$predict(check_set, out_file(tf))  

modfinish <-Sys.time()
benchmark <- round(difftime(modfinish, modstart, units = "secs"),2)
modelstep <- modelstep + 1

#get the prediction
predict_rating <- scan(tf)

 #apply the bracket function
pred_raterounded <- sapply(predict_rating,bracket)

#save the final RMSE + return it
rmsefinal_mf <- RMSE(testDS$rating,pred_raterounded) 

#show RMSE Results
rmse_results <- bind_rows(rmse_results,
                          data_frame(step = modelstep, 
                                     method="Recosystem - FINAL",  
                                     RMSE = rmsefinal_mf,
                                     benchmark=benchmark))

rmse_results %>% knitr::kable(label="MF Final Results")

#display version info
version

#show table of RMSE results
rmse_results %>% 
  mutate(step_f = factor(step, levels = c(1:modelstep))) %>% 
  arrange(-step_f) %>% 
  ggplot() + 
  geom_point(aes(x=RMSE,
                 y=step_f,
                 size=as.double(benchmark,units="secs")/10)) + 
  geom_text(aes(label = paste('  ',round(RMSE,4),method,'  '),
                x=RMSE,
                y=step_f,
                hjust = ifelse(RMSE<1,0,1)
                )) + 
  theme(legend.position = "none")
