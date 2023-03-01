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

REMOVE RETWEETS!!!!!!!!

```{r}

tweets.df <- tweets_during_elections

tweets.df %>% 
  # Remove hashtags.
  mutate(text_tw = text_tw %>% str_remove_all(pattern = '#[a-z,A-Z]*')) %>% 
  # Remove accounts.
  mutate(text_tw = text_tw %>% str_remove_all(pattern = '@[a-z,A-Z]*')) %>%
  # Remove retweets.
  mutate(text_tw = text_tw %>% str_remove_all(pattern = 'rt [a-z,A-Z]*: '))

```

first of all, we convert our dataset into a corpus
```{r}
corpus <-  Corpus(x = VectorSource(x = tweets.df$text_tw))

tweets.text <- corpus %>% 
  tm_map(removeNumbers) %>% 
  tm_map(PlainTextDocument) # %>% 
  # We could also use stemming by uncommenting the folowing line. 
  # tm_map(stemDocument, 'spanish')

# Recover data into original tibble.
tweets.df %<>% mutate(text_tw = tweets.text[[1]]$content)
```

We begin by counting the most popular words in the tweets.
```{r}

words.df <- tweets.df %>% 
  unnest_tokens(input = text_tw, output = word) 

word.count <- words.df %>% count(word, sort = TRUE)


word.count %>% head(10)

```
We can visualize these counts in a bar plot


```{r}

plt <- word.count %>% 
  # Set count threshold. 
  filter(n > 1000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  theme_light() + 
  geom_col(fill = 'violet', alpha = 0.8) +
  xlab(NULL) +
  ylab("frequence") +
  coord_flip() +
  ggtitle(label = 'Top Word Count (After Elections) - min.freq. = 1000')

plt %>% ggplotly()
```

...or with a wordcloud

```{r}
library(webshot)
webshot::install_phantomjs()
library(wordcloud2)

wordcloud2(
  data = word.count, 
  #freq = word.count$n, 
  #min.freq = 500, 
  color = brewer.pal(8, 'Blues'),
#  figPath = "/Users/Matteo/Desktop/Magistrale/Digital_Social_Data/Midterm/map_usa.png",
  backgroundColor = "grey"
)


# save it in html
#library("htmlwidgets")
#saveWidget(my_graph,"tmp.html",selfcontained = F)

# and in png or pdf
#webshot("tmp.html","fig_1.png", delay =5, vwidth = 480, vheight=480)

```

And then do the same with the split data

FIND PERCENTILE TO PLOT ONLY TOP X WORDS
```{r}

# Before Elections 
words.vr.df <- tweets_during_elections %>% 
  unnest_tokens(input = text_tw, output = word)

word.count.vr <- words.vr.df %>% count(word, sort = TRUE)

plt.m <- word.count.vr %>% 
  filter(n > 800) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  theme_light() + 
  geom_col(fill = 'red', alpha = 0.8) +
  xlab(NULL) +
  coord_flip() +
  ggtitle(label = 'Top Word Count (Up to 07/11)')


# Election day 
words.vb.df <- tweets_during_elections %>% 
  unnest_tokens(input = text_tw, output = word)

word.count.vb <- words.vb.df %>% count(word, sort = TRUE)

plt.d <- word.count.vb %>% 
  filter(n > 2500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  theme_light() + 
  geom_col(fill = 'blue', alpha = 0.8) +
  xlab(NULL) +
  coord_flip() +
  ggtitle(label = 'Top Word Count (From 7 to 9/11)')

# After elections 
words.bw.df <- tweets_post_elections %>% 
  unnest_tokens(input = text_tw, output = word) 

word.count.bw <- words.bw.df %>% count(word, sort = TRUE)

plt.r <- word.count.bw %>% 
  filter(n > 1500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  theme_light() + 
  geom_col(fill = 'blue3', alpha = 0.8) +
  xlab(NULL) +
  coord_flip() +
  ggtitle(label = 'Top Word Count (From 10/11)')

plot_grid(... = plt.m, plt.d, plt.r)

```

We can do an analogous analysis for hashtags

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
  filter(n > 230) %>%
  mutate(hashtag = reorder(hashtag, n)) %>%
  ggplot(aes(x = hashtag, y = n)) +
  theme_light() + 
  geom_col(fill = 'violet', alpha = 0.8) +
  xlab(NULL) +
  ylab("frequence") +
  coord_flip() +
  ggtitle(label = 'Top Hashtags Count (Election Days) - min.freq. = 300')

plt %>% ggplotly()
```

SISTEMA #VOTE_DEMOCRATS
And plot the corresponding wordcloud
```{r}

library(webshot)
webshot::install_phantomjs()
library(wordcloud2)  


cl <- brewer.pal(8, "Blues")

 wordcloud2(
  data = hashtags.unnested.count, 
  #freq = word.count$n, 
  #min.freq = 500, 
  color = cl[as.numeric(as.factor(hashtags.unnested.count$n))],
#  figPath = "/Users/Matteo/Desktop/Magistrale/Digital_Social_Data/Midterm/map_usa.png",
  backgroundColor = "grey"
)

#  figPath = "/Users/Matteo/Desktop/Magistrale/Digital_Social_Data/Midterm/map_usa.png"


#library("htmlwidgets")
#saveWidget(my_graph,"tmp1.html",selfcontained = F)

# and in png or pdf
#webshot("tmp.html","fig_2.png", delay =5, vwidth = 480, vheight=480)

```

```{r}

plt <- hashtags.unnested.count %>% 
  # Set count threshold. 
  filter(n > 130) %>%
  mutate(hashtag = reorder(hashtag, n)) %>%
  ggplot(aes(x = hashtag, y = n)) +
  theme_light() + 
  geom_col(fill = 'green', alpha = 0.8) +
  xlab(NULL) +
  coord_flip() +
  ggtitle(label = 'Top hashtag Count')

plt %>% ggplotly()

```

That is not bad, maybe it would be more useful to plot 3 different plots divided by time but probably no


#Network analysis

We want to count pairwise occurences of words which apperar together in the text, this is what is known as bigram count.



```{r}
bi.gram.words <- tweets.df %>% 
  unnest_tokens(
    input = text_tw, 
    output = bigram, 
    token = 'ngrams', 
    n = 2
  ) %>% 
  filter(! is.na(bigram))

bi.gram.words %>% 
  select(bigram) %>% 
  head(10)
```

Next, we filter for stop words and remove white spaces. -> probably this is not needed anymore

```{r}

bi.gram.words %<>% 
  separate(col = bigram, into = c('word1', 'word2'), sep = ' ') %>% 
  filter(! is.na(word1)) %>% 
  filter(! is.na(word2)) 

```

Finally, we group and count by bigram: for each couple of word detected as a bigram, we count the number of occurrences

```{r}
bi.gram.count <- bi.gram.words %>% 
  count(word1, word2, sort = TRUE) %>% 
  # We rename the weight column so that the 
  # associated network gets the weights (see below).
  rename(weight = n)

bi.gram.count %>% head()
```

Let us now plot the distribution of the weights values

```{r}
bi.gram.count %>% 
  ggplot(mapping = aes(x = weight)) +
    theme_light() +
    geom_histogram() +
    labs(title = "Bigram Weight Distribution")
```

Note that is very skewed, for visualization purposes it might be a good idea to perform a transformation, e.g. log transform:

```{r}
bi.gram.count %>% 
  mutate(weight = log(weight + 1)) %>% 
  ggplot(mapping = aes(x = weight)) +
    theme_light() +
    geom_histogram() +
    labs(title = "Bigram log-Weight Distribution")
```
How to define a weighted network from a bigram count?

Each word is going to represent a node.
Two words are going to be connected if they appear as a bigram.
The weight of an edge is the numer of times the bigram appears in the corpus.
(Optional) We are free to decide if we want the graph to be directed or not.
We are going to use the igraph library to work with networks. The reference A User’s Guide to Network Analysis in R is highly recomended if you want to go deeper into network analysis in R.

For visualization purposes, we can set a threshold which defines the minimal weight allowed in the graph.

Remark: It is necessary to set the weight column name as weight (see igraph docs).

```{r}
threshold <- 300

# For visualization purposes we scale by a global factor. 
ScaleWeight <- function(x, lambda) {
  x / lambda
}

network <-  bi.gram.count %>%
  filter(weight > threshold) %>%
  mutate(weight = ScaleWeight(x = weight, lambda = 2E4)) %>% 
  graph_from_data_frame(directed = FALSE)
```

Let us visualize the network

```{r}
 plot(
  network, 
  vertex.size = 1,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.7, 
  vertex.label.dist = 1,
  edge.color = 'gray', 
  main = 'Bigram Count Network - pre election day', 
  sub = glue('Weight Threshold: {threshold}'), 
  alpha = 50
)
```
We can add some additional information to the visualization: Set the sizes of the nodes and the edges by the degree and weight respectively.

```{r}

V(network)$degree <- strength(graph = network)

# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

my_plot <- plot(
  network, 
  vertex.color = brewer.pal(6, "Blues"),
  # Scale node size by degree.
  vertex.size = 200*V(network)$degree,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.9, 
  vertex.label.dist = 1.6,
  edge.color = 'gray', 
  # Set edge width proportional to the weight relative value.
  edge.width = 3*E(network)$width ,
  main = 'Bigram Count Network', 
  sub = glue('Weight Threshold: {threshold}'), 
  alpha = 50
)

png(file="/Users/Matteo/Desktop/Magistrale/Digital_Social_Data/Midterm/plots/pre_elections/bigram_net_pre_elections.png",
width=600, height=350)
my_plot
dev.off()
```

We can extract the biggest connected component of the network

```{r}

clusters(graph = network)

V(network)$cluster <- clusters(graph = network)$membership

cc.network <- induced_subgraph(
  graph = network,
  vids = which(V(network)$cluster == which.max(clusters(graph = network)$csize))
)


# Store the degree.
V(cc.network)$degree <- strength(graph = cc.network)

# Compute the weight shares.
E(cc.network)$width <- E(cc.network)$weight/max(E(cc.network)$weight)
```
and plot again

```{r}

 plot(
  cc.network, 
  vertex.color = 'lightblue',
  # Scale node size by degree.
  vertex.size = 10*V(cc.network)$degree,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.6, 
  vertex.label.dist = 1.6,
  edge.color = 'gray', 
  # Set edge width proportional to the weight relative value.
  edge.width = 3*E(cc.network)$width ,
  main = 'Bigram Count Network (Biggest Connected Component)', 
  sub = glue('Weiight Threshold: {threshold}'), 
  alpha = 50
)

```

We can go a steph further and make our visualization more dynamic using the networkD3 library.

```{r}


# Treshold
threshold <- 200

network <-  bi.gram.count %>%
  filter(weight > threshold) %>%
  graph_from_data_frame(directed = FALSE)

# Store the degree.
V(network)$degree <- strength(graph = network)
# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

# Create networkD3 object.
network.D3 <- igraph_to_networkD3(g = network)
# Define node size.
network.D3$nodes %<>% mutate(Degree = (1E-2)*V(network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3$links$Width <- 10*E(network)$width

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)

```

Let us now decrease the threshold to get a more complex network (zoom out to see it all!).

```{r}

# Treshold
threshold <- 500

network <-  bi.gram.count %>%
  filter(weight > threshold) %>%
  graph_from_data_frame(directed = FALSE)

# Store the degree.
V(network)$degree <- strength(graph = network)
# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

# Create networkD3 object.
network.D3 <- igraph_to_networkD3(g = network)
# Define node size.
network.D3$nodes %<>% mutate(Degree = (1E-2)*V(network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3$links$Width <- 10*E(network)$width

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)

```

#Skipgram Analysis

Now we are going to consider skipgrams, which allow a “jump” in thw word count:

```{r}

skip.window <- 2

skip.gram.words <- tweets.df %>% 
  unnest_tokens(
    input = text_tw, 
    output = skipgram, 
    token = 'skip_ngrams', 
    n = skip.window
  ) %>% 
  filter(! is.na(skipgram))

```

For example, let's consider the tweet

```{r}
tweets.df %>% 
  slice(23) %>% 
  pull(text_tw)
```
Skipgrams are

```{r}

skip.gram.words %>% 
  select(skipgram) %>% 
  slice(10:20)

```
We now count the skipgrams containing two words.

```{r}

skip.gram.words$num_words <- skip.gram.words$skipgram %>% 
  map_int(.f = ~ ngram::wordcount(.x))

skip.gram.words %<>% filter(num_words == 2) %>% select(- num_words)

skip.gram.words %<>% 
  separate(col = skipgram, into = c('word1', 'word2'), sep = ' ') %>% 
  filter(! is.na(word1)) %>% 
  filter(! is.na(word2)) 

skip.gram.count <- skip.gram.words  %>% 
  count(word1, word2, sort = TRUE) %>% 
  rename(weight = n)

skip.gram.count %>% head()

```
#Visualization

Similarly as above, we construct and visualize the corresponding network (we select the biggest connected component):


```{r}
threshold <- 250

# For visualization purposes we scale by a global factor. 
ScaleWeight <- function(x, lambda) {
  x / lambda
}

network <-  skip.gram.count %>%
  filter(weight > threshold) %>%
  mutate(weight = ScaleWeight(x = weight, lambda = 2E4)) %>% 
  graph_from_data_frame(directed = FALSE)
```

Let us visualize the network

```{r}
 plot(
  network, 
  vertex.size = 1,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.7, 
  vertex.label.dist = 1,
  edge.color = 'gray', 
  main = 'Bigram Count Network - pre election day', 
  sub = glue('Weight Threshold: {threshold}'), 
  alpha = 50
)
```
We can add some additional information to the visualization: Set the sizes of the nodes and the edges by the degree and weight respectively.

```{r}

V(network)$degree <- strength(graph = network)

# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

my_plot <- plot(
  network, 
  vertex.color = brewer.pal(6, "Blues"),
  # Scale node size by degree.
  vertex.size = 50*V(network)$degree,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.9, 
  vertex.label.dist = 1.6,
  edge.color = 'gray', 
  # Set edge width proportional to the weight relative value.
  edge.width = 3*E(network)$width ,
  main = 'Skipgram Count Network', 
  sub = glue('Weight Threshold: {threshold}'), 
  alpha = 50
)

png(file="/Users/Matteo/Desktop/Magistrale/Digital_Social_Data/Midterm/plots/pre_elections/bigram_net_pre_elections.png",
width=600, height=350)
my_plot
dev.off()
```

We can extract the biggest connected component of the network

```{r}

clusters(graph = network)

V(network)$cluster <- clusters(graph = network)$membership

cc.network <- induced_subgraph(
  graph = network,
  vids = which(V(network)$cluster == which.max(clusters(graph = network)$csize))
)


# Store the degree.
V(cc.network)$degree <- strength(graph = cc.network)

# Compute the weight shares.
E(cc.network)$width <- E(cc.network)$weight/max(E(cc.network)$weight)
```
and plot again

```{r}

 plot(
  cc.network, 
  vertex.color = 'lightblue',
  # Scale node size by degree.
  vertex.size = 50*V(cc.network)$degree,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.7, 
  vertex.label.dist = 1.6,
  edge.color = 'gray', 
  # Set edge width proportional to the weight relative value.
  edge.width = 2*E(cc.network)$width ,
  main = 'Skipgram Count Network (Biggest Connected Component)', 
  sub = glue('Weiight Threshold: {threshold}'), 
  alpha = 50
)

```





```{r}

# Treshold
threshold <- 500

network <-  skip.gram.count %>%
  filter(weight > threshold) %>%
  graph_from_data_frame(directed = FALSE)

# Select biggest connected component.  
V(network)$cluster <- clusters(graph = network)$membership

cc.network <- induced_subgraph(
  graph = network,
  vids = which(V(network)$cluster == which.max(clusters(graph = network)$csize))
)

# Store the degree.
V(cc.network)$degree <- strength(graph = cc.network)
# Compute the weight shares.
E(cc.network)$width <- E(cc.network)$weight/max(E(cc.network)$weight)

# Create networkD3 object.
network.D3 <- igraph_to_networkD3(g = cc.network)
# Define node size.
network.D3$nodes %<>% mutate(Degree = 0.01*V(cc.network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3$links$Width <- 1*E(cc.network)$width

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)

```


#Node importance



There are many notions of node importance in a network (A User’s Guide to Network Analysis in R, Section 7.2). Here we compare three of them

Degree centrality
Closeness centrality
Betweenness centrality

```{r}
node.impo.df <- tibble(
  word = V(cc.network)$name,  
  degree = strength(graph = cc.network),
  closeness = closeness(graph = cc.network), 
  betweenness = betweenness(graph = cc.network)
)

```


```{r}

```

Let us see the distribution of these centrality measures.


```{r}
plt.deg <- node.impo.df %>% 
  ggplot(mapping = aes(x = degree)) +
    theme_light() +
    geom_histogram(fill = 'blue', alpha = 0.8, bins = 30)

plt.clo <- node.impo.df %>% 
  ggplot(mapping = aes(x = closeness)) +
    theme_light() +
    geom_histogram(fill = 'red', alpha = 0.8, bins = 30)

plt.bet <- node.impo.df %>% 
  ggplot(mapping = aes(x = betweenness)) +
    theme_light() +
    geom_histogram(fill = 'green4', alpha = 0.8, bins = 30)

plot_grid(
  ... = plt.deg, 
  plt.clo, 
  plt.bet, 
  ncol = 1, 
  align = 'v'
)

```

#Community detection

We can try to find clusters within the network. We use the Louvain Method for community detection:

```{r}

comm.det.obj <- cluster_louvain(
  graph = cc.network, 
  weights = E(cc.network)$weight
)

```

Now we encode the membership as a node atribute (zoom and click on each node to explore the clusters).

```{r}
V(cc.network)$membership <- membership(comm.det.obj)

```

```{r}
network.D3$nodes$Group <- V(cc.network)$membership

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)%>%
  htmlwidgets::prependContent(htmltools::tags$h1("Communities_during_election")) %>% 
  saveNetwork(file = 'Communities_during_election.html')

```





