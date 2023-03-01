# Twitter-analysis-US-midterms
The purpose of this paper is to exploit Twitter data in order to perform sociological analysis. In particular, the frame chosen for this work are past USA midterm elections held on the 8th of November 2022.

## Data cleaning
After having collected almost 200k Tweets regarding US Midterm elections based on some endorsement hashtags, resulting texts were cleaned from both a semantic and syntactic point of view. Stopwords were removed, together with mispelled words and punctuations. Furthermore, different terms referring to the same entity were grouped together. All the cleaning process is available in the file "cleaning.md". With the cleaned text, some preliminary plots were created, considering distribution of tweets through time and frequency of words and hashtags. Below, a barplot of the most used hashtags after having grouped words semantically. 

<img src="https://github.com/MatteoScianna/Twitter-analysis-US-midterms/blob/main/img/top_hashtags_after_grouping.png" width="400" height="400">

#Analysis
With data cleaned, different analysis were performed. Tweets were divided into three groups according to their timeline, resulting in a pre-election dataset, a during-election dataset and a post-election dataset. For all of these, barplots and wordclouds of the most used words and hashtags were made. 
Figure below shows the wordcloud of most used words for the post-elections dataset. 

<img src="https://github.com/MatteoScianna/Twitter-analysis-US-midterms/blob/main/img/post_top_words_count_wordcloud.jpg" width="400" height="200">

Furthermore, a topic analysis was performed. Bigram and skipgram techniques were used in order to extract words appearing together more often. Words are then represented as nodes of a network, with edges between words indicating an actual copresence of the two words in many different tweets. An example for post-elections dataset is shown below. 

<img src="https://github.com/MatteoScianna/Twitter-analysis-US-midterms/blob/main/img/post_skipgram_dynamic.jpg" width="400" height="400">

