library(readr)
library(ggfortify)
library(arrow)
library(skimr)
library(tidymodels)
library(lubridate)
library(patchwork)
library(stringr)
load("data/youtube_data.rda")

#Allocation of data ----
#set seed
set.seed(123)

#splitting data 
youtube_data_split <- youtube_data %>% 
  #60% for prediction phase and 40$ for eda
  initial_split(prop = 0.6)

#getting prediction phase data 
youtube_pred <- youtube_data_split %>% training()
#saving data 
save(youtube_pred, file = "data/youtube_pred.rda")

#getting data for EDA
youtube_eda <- youtube_data_split %>% testing()

#Univariate Analysis ----

##Numeric variables ----

#creating function to analyze distribution 
univar_numeric_plots <- function(col) {
  #return a ggplot object
  ggplot(data = youtube_eda, 
         #that maps the x axis to the argument 
         mapping = aes(x = {{col}})) +
    #and makes a histogram with 100 bins
    geom_histogram(bins = 100, 
                   #personalize colors to youtube palette 
                   fill = "#cc0000") +
    #personalize theme 
    theme_bw() +
    #title dependent on argument 
    labs(title = paste("Distribution of", deparse(substitute(col)), sep = " "))
}

#apply function to make plots 
uni_a <- univar_numeric_plots(likes) #likes distribution
uni_b <- univar_numeric_plots(categoryId) #categoryId distribution 
uni_c <- univar_numeric_plots(view_count) #view_count distribution 
uni_d <- univar_numeric_plots(dislikes) #dislikes distribution 
uni_e <- univar_numeric_plots(comment_count) #comment_count distribution 
uni_f <- univar_numeric_plots(duration_seconds) #duration_seconds distribution 
uni_g <- univar_numeric_plots(target) #target distribution 

#joining with patchwork
(uni_a + uni_b + uni_c + uni_d) / (uni_e + uni_f + uni_g + uni_h) #analysis provided in Rmd

#transforming variables due to their distribution to reduce skewness 
youtube_eda <- youtube_eda %>%
  mutate(likes = log(likes, base = 10), 
         view_count = log(view_count, base = 10),
         comment_count = log(comment_count, base = 10),
         dislikes = log(dislikes, base = 10),
         duration_seconds = log(duration_seconds, base = 10))

#making plots again 
uni_a_1 <- univar_numeric_plots(likes)
uni_b_1 <- univar_numeric_plots(categoryId)
uni_c_1 <- univar_numeric_plots(view_count)
uni_d_1 <- univar_numeric_plots(dislikes)
uni_e_1 <- univar_numeric_plots(comment_count)
uni_f_1 <- univar_numeric_plots(duration_seconds)
uni_g_1 <- univar_numeric_plots(target)
uni_h_1 <- univar_numeric_plots(hour_released)

#joining with patchwork
(uni_a_1 + uni_b_1 + uni_c_1 + uni_d_1) / #analysis provided in Rmd
  (uni_e_1 + uni_f_1 + uni_g_1 + uni_h_1)

#plot for days between release and becoming trending 
ggplot(data = youtube_eda, 
       mapping = aes(x = time_release_trending)) +
  geom_histogram(bins = 100, 
                 fill = "#cc0000") +
  theme_bw() +
  labs(title = "Distribution of the number of days from \nrelease to becoming trending", 
       x = "number of days") +
  theme(plot.title = element_text(size = 10)) #analysis provided in Rmd

##Non-numeric variables ----

#mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#highlight highest bar plot-function

highlight_max_bar <- function(col) {
  #select the column provided as argument from the youtube_eda set 
  col1 <- as_vector(youtube_eda %>% select({{col}}))
  #select the observation with the highest frequency in the variable 
  #use the mode funtion previously created 
  h <- Mode(col1)
  #I had noticed that R turns dates into numbers, and the mode date 
  #was turned into 18565
  #thus, the conditional below checks whether the argument was of type date or not 
  #if the argument provided by the user was date 
  if(h == 18565) {
    #turn it back into date format 
    h <- as_date(h)
    #make plot
    meanwhile <- youtube_eda %>%
      #creating a extra column called "max"
      #that will be true only for the observation with the mode
      mutate(max = if_else({{col}} == h, 
                           TRUE, 
                           FALSE)) %>%
      #map x to the argument provided and fill to max
      #so the mode observation will be highlighted in a different color 
      ggplot(aes(x = {{col}}, fill = max)) +
      #personalize colors 
      scale_fill_manual(values = c("#6e6e6e", "#cc0000")) +
      #make a bar plot 
      geom_bar(show.legend = FALSE) +
      #personalize theme and labs 
      theme_bw() +
      labs(x = deparse(substitute(col)))
    
    #return plot 
    meanwhile
    
  } else {
    h <- h
    #make plot 
    meanwhile <- youtube_eda %>%
      #creating a extra column called "max"
      #that will be true only for the observation with the mode
      mutate(max = if_else({{col1}} == h, 
                           TRUE, 
                           FALSE)) %>%
      #map x to the argument provided and fill to max
      #so the mode observation will be highlighted in a different color 
      ggplot(aes(x = {{col1}}, fill = max)) +
      #personalize colors 
      scale_fill_manual(values = c("#6e6e6e", "#cc0000")) +
      #make bar plot 
      geom_bar(show.legend = FALSE) +
      #personalize theme and labs 
      theme_bw() +
      labs(x = deparse(substitute(col)))
    
    #return plot
    meanwhile
  }
  
}

#apply function to make plots 
nonuni_a <- highlight_max_bar(day_week_released)
nonuni_b <- highlight_max_bar(comments_disabled)
nonuni_c <- highlight_max_bar(ratings_disabled)
nonuni_d <- highlight_max_bar(has_thumbnail)
nonuni_e <- highlight_max_bar(released_date)

#puting them together with patchwork
nonuni_e / (nonuni_a + nonuni_b + nonuni_c + nonuni_d) #analysis provided in Rmd

#Bivariate Analysis ----

##Numeric variables ----

#function to make bivariate numeric plots
biv_numeric <- function(col) {
  #using youtube_eda data
  ggplot(data = youtube_eda, 
         #mapping x to the argument 
         mapping = aes(x = {{col}}, 
                       #and y to the target variable 
                       y = target)) +
    #geom jitter to control for overplotting 
    geom_jitter(alpha = 0.1, color = "#6e6e6e") +
    #general trend line 
    geom_smooth(se = FALSE, color = "#cc0000") +
    #personalizing color and labs
    theme_bw() +
    labs(title = paste("Scatterplot of target vs", deparse(substitute(col)), sep = " "))
}

#applying function to make plots 
biv_a <- biv_numeric(dislikes)
biv_b <- biv_numeric(comment_count)
biv_c <- biv_numeric(duration_seconds)
biv_d <- biv_numeric(hour_released)
biv_e <- biv_numeric(time_release_trending)

#together with patchwork
biv_e / (biv_a + biv_b) / (biv_c + biv_d) #analysis provided in Rmd

##Non-numeric variables ----

#function to make bivariate non-numeric plots 
biv_non <- function(col) {
  #take data from youtube_eda data
  ggplot(data = youtube_eda, 
         #map x to the argument 
         mapping = aes(x = {{col}}, 
                       #and y to the target variable
                       y = target)) +
    #making a boxplot 
    geom_boxplot(show.legend = FALSE, 
                 #personalizing color, fill, and alpha 
                 fill = "#6e6e6e", 
                 color = "#cc0000", 
                 alpha = 0.5) +
    #personalizing theme 
    theme_bw()
}

#making plot for day_week_release vs target 
#turning the day_week_release into numbers where 
#Monday is represented by 1 and Sunday is represented by 7 
biv_n_a <- youtube_eda %>%
  mutate(day_week_released = ifelse(day_week_released == "Sunday", 
                                    7, 
                                    ifelse(day_week_released == "Monday", 
                                           1, 
                                           ifelse(day_week_released == "Tuesday", 
                                                  2, 
                                                  ifelse(day_week_released == "Wednesday", 
                                                         3, 
                                                         ifelse(day_week_released == "Thursday", 
                                                                4, 
                                                                ifelse(day_week_released == "Friday", 
                                                                       5, 
                                                                       ifelse(day_week_released == "Saturday", 
                                                                              6,
                                                                              "None")))))))) %>%
 #group by day of the week released 
  group_by(day_week_released) %>%
  #and get the median target for each group
  summarise(median_target=median(target)) %>%
  #ungroup
  ungroup() %>%
  #map the day of the week released to x and the median target to y
  ggplot(aes(x = as.numeric(day_week_released), 
             y = median_target)) +
  #make scatterplot
  geom_point(color = "#cc0000") +
  #join the points with a line 
  geom_line(color = "#cc0000") +
  #personalize scale of x, labs, and theme 
  scale_x_continuous(breaks = seq(1,7,1)) +
  labs(x = "Day of the Week Released (1 = Monday, 7 = Sunday)") +
  theme_bw()

#apply th function to create plots 
biv_n_b <- biv_non(comments_disabled)
biv_n_c <- biv_non(has_thumbnail)
biv_n_d <- biv_non(ratings_disabled)

#make plot for category id 
biv_n_e <- youtube_eda %>%
  #make categoryId a factor variable
  mutate(categoryId = factor(categoryId)) %>%
  #create boxplot 
  ggplot(mapping = aes(x = categoryId, 
                       y = target, 
                       fill = categoryId)) +
  geom_boxplot(show.legend = FALSE, 
               fill = "#6e6e6e", 
               color = "#cc0000", 
               alpha = 0.5) +
  theme_bw()

#join plots with patchwork 
biv_n_e / biv_n_a / (biv_n_d + biv_n_c + biv_n_b) #analysis provided in Rmd

#FINISH SECTION 


