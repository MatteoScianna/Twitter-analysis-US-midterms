## R Markdown


---
---
title: "Untitled"
author: "Matteo Scianna"
date: "2022-11-19"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Import libraries

```{r cars}
# Data Wrangling and Visualization
library(glue)
library(cowplot)
library(magrittr)
library(plotly)
library(tidyverse)
#library(widyr)
# Date & Time Manipulation.
library(hms)
library(lubridate) 
# Text Mining
library(tidytext)
library(tm)
library(wordcloud)
# Network Analysis
library(igraph)
# Network Visualization (D3.js)
library(networkD3)
library(quanteda)

```

Data loading: 200133 tweets containing different hashtags (see which in "data_collection" file)

```{r pressure, echo=FALSE}
load("/Users/Matteo/Desktop/Magistrale/Digital_Social_Data/Midterm/Collection/dataset_final_electionweek.Rdata")
tweets.raw.df <- dataset_election_week
rm(dataset_election_week)
```

Here we create a new variables "forgetting" the seconds of create_at_tw column, so that the time in which a tweet is created is rounded to minutes 

```{r}
tweets.raw.df %<>% 
  mutate(Created_At_Round = created_at_tw %>% round(units = 'mins') %>% as.POSIXct())
```

We now plot the time series of tweets count per minute

```{r}
plt <- tweets.raw.df %>% 
  count(Created_At_Round) %>% 
  ggplot(mapping = aes(x = Created_At_Round, y = n)) +
    theme_light() +
    geom_line() +
    xlab(label = 'Date') +
    ylab(label = NULL) +
    ggtitle(label = 'Number of Tweets per Minute')

plt %>% ggplotly() %>%
htmlwidgets::prependContent(htmltools::tags$h1("tweets_minute")) %>% 
 saveNetwork(file = 'tweets_minute.html')
```

Time will be the main criterion through which we'll divide our original dataset into three different datasets, in order to better grasp information and provide a more profound analysis. 
Before to do this, we need to clean the text properly. 


Here we start cleaning the text, converting capital letters to lower and removing rt.

```{r}

#First general cleaning (remove punctuation, lower everything..)
tweets.df <- tweets.raw.df %>% 
  # Remove column.
  select(-  created_at_tw) %>% 
  # Convert to lowercase. 
  mutate(text_tw = text_tw %>% str_to_lower) %>% 
  mutate(text_tw = text_tw %>% str_remove(pattern = '^(rt)')) %>% 
  mutate(text_tw = str_remove_all(text_tw,     "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"))

rm(tweets.raw.df)
  
```

Here we define a function in order to extract all hashtags from a tweet

```{r}
GetHashtags <- function(tweet) {

  hashtag.vector <- str_extract_all(string = tweet, pattern = '#\\S+', simplify = TRUE) %>% 
    as.character()
  
  hashtag.string <- NA
  
  if (length(hashtag.vector) > 0) {
    
    hashtag.string <- hashtag.vector %>% str_c(collapse = ', ')
    
  } 

  return(hashtag.string)
}
```

One kind of analysis that will be performed regards hashtags. 
We create a new dataframe with all hashtags present per tweet and we bind the two dataset.
```{r}

hashtags.df <- tibble(
  Hashtags = tweets.df$text_tw %>% map_chr(.f = ~ GetHashtags(tweet = .x))
)

hashtags.df %>% head()

tweets.df %<>% bind_cols(hashtags.df) 

```

Here we want to have a visual idea of which are the most used hashtags.

```{r}
hashtags.unnested.df <- tweets.df %>% 
  select(Created_At_Round, Hashtags) %>% 
  unnest_tokens(input = Hashtags, output = hashtag)
  
hashtags.unnested.count <- hashtags.unnested.df %>% 
  count(hashtag) %>% 
  drop_na()
```

And plot the corresponding barplot. 

```{r}

plt <- hashtags.unnested.count %>% 
  # Set count threshold. 
  filter(n > 1000) %>%
  mutate(hashtag = reorder(hashtag, n)) %>%
  ggplot(aes(x = hashtag, y = n)) +
  theme_light() + 
  geom_col(fill = 'red', alpha = 0.8) +
  xlab(NULL) +
  ylab("frequence") +
  coord_flip() +
  ggtitle(label = 'Top hashtag Count - min. freq. = 1000')

plt %>% ggplotly() %>%
htmlwidgets::prependContent(htmltools::tags$h1("Top Hashtags")) %>% 
 saveNetwork(file = 'Top_hashtags_no_grouping.html')

```

One idea now could be to group and rename all similar hashtags, so that situations like #votered, #voteredtosavedemocracy, #voteredtosaveamerica, #voterepublicans... will end up in a single hashtag, corresponding to an endorsment to the righy wing. 
We'll end up with only for endorsement hashtags, two for party: #votered and #redwave for republicans; #voteblue and #bluewave for democrats.
In particular, were found
- 19 #voteblue hashtags
- 7 #votered hashtags
- 5 #redwave hashtags
- 4 #bluewave hashtags

Furthermore, we want also to compact all hashtags regarding midterm elections (9 were found). 


```{r}

tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#midterms2022","#midterms")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#votebluetosavedemocracy","#voteblue")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#redwave2022","#redwave")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#election2022","#midterms")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#voteredtosaveamerica","#votered")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#elections2022","#midterms")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#redwavecoming","#redwave")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#votebluein2022","#voteblue")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#votebluetoday","#voteblue")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#midtermelections2022","#midterms")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#votebluetoprotectwomen","#voteblue")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#bluetsunami2022","#bluewave")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#voteredtosaveamerica","#votered")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#voteblueforsomanyreasons","#voteblue")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#votered2022","#votered")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#votebluefordemocracy","#voteblue")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#redtsunami2022","#redwave")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#votebluetoprotectyourrights","#voteblue")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#votebluetosaveamerica","#voteblue")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#midtermelections","#midterms")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#votedemocrat","#voteblue")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#votebluetoendtheinsanity","#voteblue")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#bluetsunami","#bluewave")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#voterepublican","#votered")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#2022elections","#midterms")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#voteblue2022","#voteblue")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#redtsunami","#redwave")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#votebluetomorrow","#voteblue")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#midterm","#midterms")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#bluewave2022","#bluewave")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#votebluetomorrow","#voteblue")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#midte","#midterms")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#votebluedownballotlocalstatefederal","#voteblue")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#midterm2022","#midterms")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#redwave22","#redwave")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#votebluein22","#voteblue")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#votebluetosavesocialsecurityandmedicare","#voteblue")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#votebluedownballot","#voteblue")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#voteredtosavedemocracy","#votered")


tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#midtermsrmss","#midterms")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#midtermsrms","#midterms")


#Fino a 179 occurrences
```

Now again we extract all the hashtags in the new dataset as before. 

```{r}

tweets.df <- tweets.df[,-12]

hashtags.df <- tibble(
  Hashtags = tweets.df$text_tw %>% map_chr(.f = ~ GetHashtags(tweet = .x))
)

hashtags.df %>% head()

tweets.df %<>% bind_cols(hashtags.df) 

```

Define the new hashtags column and plot the barplot 

```{r}
hashtags.unnested.df <- tweets.df %>% 
  select(Created_At_Round, Hashtags) %>% 
  unnest_tokens(input = Hashtags, output = hashtag)
  
hashtags.unnested.count <- hashtags.unnested.df %>% 
  count(hashtag) %>% 
  drop_na()
```


```{r}

plt <- hashtags.unnested.count %>% 
  # Set count threshold. 
  filter(n > 600) %>%
  mutate(hashtag = reorder(hashtag, n)) %>%
  ggplot(aes(x = hashtag, y = n)) +
  theme_light() + 
  geom_col(fill = 'red', alpha = 0.8) +
  xlab(NULL) +
  coord_flip() +
  ggtitle(label = 'Top hashtag Count - min. freq. = 600 (after grouping)')

plt %>% ggplotly()

```

and we merge the two datasets, creating also columns highlighting if tweets contain some specific hashtags

```{r}

tweets.df$is_votered <- str_detect(tweets.df$Hashtags, '#votered')
tweets.df$is_voteblue <- str_detect(tweets.df$Hashtags, '#voteblue')
tweets.df$is_redwave <- str_detect(tweets.df$Hashtags, '#redwave')
tweets.df$is_bluewave <- str_detect(tweets.df$Hashtags, '#bluewave')



```

Here we plot the frequence of specific hashtags during the considered time range
```{r}
plt <- hashtags.unnested.df %>% 
  filter(hashtag %in% c('votered', 'voteblue')) %>% 
  count(Created_At_Round, hashtag) %>% 
  ggplot(mapping = aes(x  = Created_At_Round, y = n, color = hashtag)) +
    theme_light() + 
    xlab(label = 'Date') +
    ggtitle(label = 'Top Hastags Counts') +
    geom_line() + 
    scale_color_manual(values = c('votered' = 'pink', 'voteblue' = 'light blue'))

plt %>% ggplotly()


plt1 <- hashtags.unnested.df %>% 
  filter(hashtag %in% c( "redwave", "bluewave")) %>% 
  count(Created_At_Round, hashtag) %>% 
  ggplot(mapping = aes(x  = Created_At_Round, y = n, color = hashtag)) +
    theme_light() + 
    xlab(label = 'Date') +
    ggtitle(label = 'Top Hastags Counts') +
    geom_line() + 
    scale_color_manual(values = c("redwave" = "red3", "bluewave" = "blue3"))

plt1 %>% ggplotly()
```

Here we can complete the cleaning of the text from a semantic point of viwe, merging together different words corresponding to the same concept.
```{r}
  # Remove unwanted characters. 
  #mutate(text_tw= text_tw %>% str_remove_all(pattern = '\\n')) %>% 
  #mutate(text_tw = text_tw %>% str_remove_all(pattern = '&amp')) %>% 
  #mutate(text_tw = text_tw %>% str_remove_all(pattern = 'https://t.co/[a-z,A-Z,0-9]*')) %>%
  #mutate(text_tw = text_tw %>% str_remove_all(pattern = 'http://t.co/[a-z,A-Z,0-9]*')) %>% 
  #mutate(text_tw = text_tw %>% str_remove_all(pattern = 'https')) %>% 
  #mutate(text_tw = text_tw %>% str_remove_all(pattern = 'http')) %>% 
  # Remove hashtags.
  #mutate(text_tw = text_tw %>% str_remove_all(pattern = '#[a-z,A-Z]*')) %>% 
  # Remove accounts.



tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "@[a-z,A-Z]*","")

  # Remove retweets.
  #mutate(text_tw = text_tw %>% str_remove_all(pattern = 'rt [a-z,A-Z]*: ')) %>% 
   

  #mutate(text_tw = text_tw %>% str_remove_all(pattern = '\\_')) 

#Change some specific characters

#General words
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "http","")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "\b’\b","")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "\n"," ")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#"," ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "isn’t","is_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "isn't","is_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "can't","can_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "can’t","can_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "haven't","have_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "haven’t","have_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "couldn't","could_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "couldn’t","could_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "ain't","am_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "ain’t","am_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "aren't","are_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "aren’t","are_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "won't","will_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "won’t","will_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "wouldn't","would_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "wouldn’t","would_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "don't","do_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "don’t","do_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "doens't","does_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "doens’t","does_not")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "'ve"," have")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "’ve"," have")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "th ","") #problema date
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "oooh"," ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "&amp"," ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "’","")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "'","")



#Some often mentioned people
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "cynthia mckinney","cynthiamckinney")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "kamala harris","kamalaharris")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "megan","megan_mccain")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "megan mccain","megan_mccain")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "megans","megan_mccain")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "megan mccains","megan_mccain")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "meganmccain","megan_mccain")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "don trump","trump")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "donald trump","trump")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "donald trumps","trump")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "trumpism","trump")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "trumps","trump")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "tretta","mia_tretta ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "mia tretta","mia_tretta ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "trettas","mia_tretta ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "miatretta","mia_tretta ")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "kari","kari_lake")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "kari lake","kari_lake")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "kari lakes","kari_lake")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "kari lake’s","kari_lake")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "kari lake's","kari_lake")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "karilake","kari_lake")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "karilakes","kari_lake")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "lake","kari_lake")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "kari_kari_lake","kari_lake")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "meghan mccain","meghan_mccain")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "meghanmccain","meghan_mccain")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "megan mccain","meghan_mccain")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "meghanmccains","meghan_mccain")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "meghans mccain","meghan_mccain")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "meghan ","meghan_mccain")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "megan ","meghan_mccain")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "john ","john_mccain")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "john mccain","john_mccain")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "johns mccain","john_mccain")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "johnmccain","john_mccain")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " pa ","pennsylvania")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "doug","doug_mastriano")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "mastriano","duoug_mastriano")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "doug mastriano","mastriano")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "yevgeny","yevgeny_prigozhin")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "yevgeny prigozhin","yevgeny_prigozhin")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "prigozhin","yevgeny_prigozhin")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "prigozin","yevgeny_prigozhin")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "yevgeni","yevgeny_prigozhin")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "putins","putin")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "vladimir putin","putin")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " joe bid","biden")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " joe ","biden")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " karilake ","kari_lake")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " jewi ","jewish")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " laurenboebert ","lauren_boebert")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " lauren  ","lauren_boebert")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " boebert ","lauren_boebert")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "gen z ","gen_z")


#USA and midterm elections
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "united states","america")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " us ","america")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "usa","america")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "u s a","america")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "united states of america","america")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "states","america")

tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " election2022","midterm_elections")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " elections2022","midterm_elections")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " elections","midterm_elections")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " election","midterm_elections")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " midtermelections","midterm_elections")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " midterms","midterm_elections")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "us midterms","midterm_elections")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "\busmidterms\b","midterm_elections")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "midterms","midterm_elections")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "usmidterms","midterm_elections")

tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "midterm2022","midterm_elections")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "midterms2022","midterm_elections")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " midtermelections22", "midterm_elections")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "midterm_elections2022", "midterm_elections")

#Democrats and Republicans + endorsement
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "democra","democrats")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "democratss","democrats ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " dem ","democrats ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "dems","democrats ")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "blue","democrats ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "demmie","democrats ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "letf","democrats ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " rep ","republicans ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " reps ","republicans ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " republican ","republicans ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " voters ","vote")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " votes ","vote")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " maga","make_america_great_again")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " red ","republicans ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " left wing ","democrats ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "right wing","republicans ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " voteblue","vote_democrats ")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "vote blue","vote_democrats ")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "vote democrats","vote_democrats ")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "votedemocrats","vote_democrats ")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "vote dem","vote_democrats ")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " voteblueforsomanyreasons ","vote_democrats ")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " votered","vote_republicans ")
# tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "vote red","vote_republicans ")
# tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "vote rep","vote_republicans ")
# tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "vote republicans","vote_republicans ")
# tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "voterepublicans ","vote_republicans")
# tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "voterepublicanstosaveamerica ","vote_republicans ")
# tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "voterepublicanstosaveamerica2022 ","vote_republicans ")
# tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "democra ","democrats ")
# tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "bluewave","blue_wave")
# tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "blue wave","blue_wave")
# tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "democratswave","blue_wave ")
# tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "democratwave","blue_wave ")
# tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "democrats wave","blue_wave ")
# tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "democrat wave","blue_wave ")
# tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "red wave","red_wave")
# 
# tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "republicanwave ","red_wave")
# tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "republicanswave ","red_wave")
# tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "republicanswave2022 ","red_wave")
# tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "republicans wave","red_wave")
# tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "republican wave","red_wave")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, " gop ","republican_party ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "republicanparty ","republican_party ")

tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "election day","election_day")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "good morning","good_morning")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "american elections ","midterm_elections")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "americanelection ","midterm_elections")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "midtermelections ","midterm_elections")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "americanelections ","midterm_elections")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "shootmiatretta","shoot mia_tretta")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "new york","new_york")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "wsj","wall_street_journal")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "south texas","south_texas")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "will vote","will_vote")

# Replace accents. 
replacement.list <- list('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u')

tweets.df %<>% 
  mutate(text_tw = chartr(old = names(replacement.list) %>% str_c(collapse = ''), 
                       new = replacement.list %>% str_c(collapse = ''),
                       x = text_tw))
```

So now inside the tweets.df dataset we have two text columns: one with the original text and one with the cleaned one


Then we remove stopwords and unnecessary spaces. 
```{r}
library(quanteda)

tweets.df$text_tw <- removeWords(tweets.df$text_tw, stopwords("english"))
tweets.df$text_tw <- stripWhitespace(tweets.df$text_tw)
```


Now we have our dataset cleaned, containing also information regarding whether each tweet does or does not contain #voteblue or #votered. 
Now we want to remove all hashtags we looked for in our data collection. 
Finally, we consider only tweets containing at least 3 characters. 

```{r}

#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#midterms"," ")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#midtermelections"," ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#midterms"," ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#redwave"," ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#bluewave"," ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#voteblue"," ")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#votedemocrat"," ")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#voteblueforsomanyreasons"," ")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#democratshateamerica"," ")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#redwave2022"," ")
tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#votered"," ")
#tweets.df$text_tw <- str_replace_all(tweets.df$text_tw, "#voteredtosaveamerica"," ")

tweets.df$count <- ntoken(tweets.df$text_tw) # from quanteda packge


tweets.df <- tweets.df %>%
  filter (count > 3)


```


Now we can adjourn our hashtags column, considering only "new" hashtags, not those we started looking for. 
```{r}
tweets.df <- tweets.df[,-12]

hashtags.df <- tibble(
  Hashtags = tweets.df$text_tw %>% map_chr(.f = ~ GetHashtags(tweet = .x))
)

hashtags.df %>% head()

tweets.df %<>% bind_cols(hashtags.df) 


tweets.df <- tweets.df %>%
  mutate(text_tw = str_remove_all(text_tw, '[[:punct:]]')) %>%
  mutate(text_tw = str_remove_all(text_tw,      "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"))

```
Again let's plot

```{r}
hashtags.unnested.df <- tweets.df %>% 
  select(Created_At_Round, Hashtags) %>% 
  unnest_tokens(input = Hashtags, output = hashtag)
  
hashtags.unnested.count <- hashtags.unnested.df %>% 
  count(hashtag) %>% 
  drop_na()
```

```{r}

plt <- hashtags.unnested.count %>% 
  # Set count threshold. 
  filter(n > 600) %>%
  mutate(hashtag = reorder(hashtag, n)) %>%
  ggplot(aes(x = hashtag, y = n)) +
  theme_light() + 
  geom_col(fill = 'red', alpha = 0.8) +
  xlab(NULL) +
  coord_flip() +
  ggtitle(label = 'Top hashtag Count')

plt %>% ggplotly()

```

Now finally we can split the dataset according to two criteria: time and content. 
According to time, we create three different datasets:
- tweets_pre_elections: containing 32606 tweets up to 7/11
- tweets_during_elections: containing 140370 tweets from 7 to 10/11
- tweets_post_elections: containing 23893 tweets from 10/11
According to content, we create four different datasets (really useful?)

```{r}
#library(quanteda)
tweets.df.only.votered <- tweets.df %>%
 filter (is_votered == TRUE)

tweets.df.only.voteblue <- tweets.df %>%
 filter (is_voteblue == TRUE)

tweets.df.only.redwave <- tweets.df %>%
 filter (is_redwave == TRUE)

tweets.df.only.bluewave <- tweets.df %>%
 filter (is_bluewave == TRUE)

beginning <- as.POSIXct(x = '2022-11-07 00:00:00')
finish <- as.POSIXct(x = '2022-11-10 00:00:00')


tweets_post_elections <- tweets.df %>% 
  filter(Created_At_Round > finish)  

tweets_pre_elections <- tweets.df %>% 
  filter(Created_At_Round < beginning) 


tweets_during_elections <- tweets.df %>% 
  filter(Created_At_Round < finish)  %>% 
  filter(Created_At_Round > beginning)
```



