####################################################
## A wordcould of my PhD thesis
## Author: Francesca Mancini, code adapted from 
## https://www.r-bloggers.com/word-cloud-in-r/
## and https://www.r-bloggers.com/the-wordcloud2-library/
## Date created: 2019/08/21
## Date modified:
####################################################

library(tm)
library(wordcloud)
library(RColorBrewer)
library(readtext)

# try with package wordcloud

# read pdf and clean
thesis <- readtext("../Managing the wildlife tourism commons.pdf")

thesis_corp <- Corpus(VectorSource(thesis))

thesis_corp <- tm_map(thesis_corp, removePunctuation)

thesis_corp <- tm_map(thesis_corp, removeNumbers)

thesis_corp <- tm_map(thesis_corp, tolower)

thesis_corp <- tm_map(thesis_corp, removeWords, stopwords(kind = "en"))

thesis_corp <- tm_map(thesis_corp, removeWords, c("figure","table","area", "time"))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
thesis_corp <- tm_map(thesis_corp, toSpace, "/")
thesis_corp <- tm_map(thesis_corp, toSpace, "@")
thesis_corp <- tm_map(thesis_corp, toSpace, "\\|")
thesis_corp <- tm_map(thesis_corp, toSpace, "-")
thesis_corp <- tm_map(thesis_corp, toSpace, "_")

# set the colours and plot
color <- brewer.pal(8, "Spectral")

wordcloud(thesis_corp, max.words = 100, random.order = FALSE,
          min.freq = 15, colors = color, scale = c(3, .3))

# this is a bit minimal, let's see if we can do better with wordcloud2

library(wordcloud2)

# this transforms the corpus in a term document matrix
# and then a dataframe with the words and their frequency
dtm <- TermDocumentMatrix(thesis_corp)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
# plot
wordcloud2(d, color = "random-light", backgroundColor = "black")

# this saves it as an html and then as a pdf
library(webshot)
webshot::install_phantomjs()

# Make the graph
my_thesisWordcloud <- wordcloud2(d, color = "random-light", backgroundColor = "black")

# save it in html
library("htmlwidgets")
saveWidget(my_thesisWordcloud,"tmp.html",selfcontained = F)

# and in pdf
webshot("tmp.html","my_thesisWordcloud.pdf", delay =5, vwidth = 1000, vheight=700)
