write.csv(data.frame(
  News_Title = character(),
  News_Description = character(),
  Date = character(),
  Category = character(),
  stringsAsFactors = FALSE
), "bdnews_article.csv", row.names = FALSE)


library(rvest)


url <- "https://bdnews24.com/business/766991fff2aa"


webpage <- read_html(url)


title <- html_text(html_node(webpage, "h1"))


description <- html_text(html_node(webpage, "div.details-brief.dNewsDesc.print-section"))


date_span <- html_nodes(webpage, "div.pub-up p span")[2]
date <- html_text(date_span)


category <- unlist(strsplit(url, "/"))[4]

news_data <- data.frame(
  News_Title = title,
  News_Description = description,
  Date = date,
  Category = category,
  stringsAsFactors = FALSE
)

news_data

write.table(news_data, "bdnews_article.csv",
            sep = ",",
            row.names = FALSE,
            col.names = FALSE,
            append = TRUE,
            quote = TRUE,
            fileEncoding = "UTF-8",
            qmethod = "double")


library(tm)
library(SnowballC)
library(textstem)
library(tokenizers)
library(dplyr)

mydata <- read.csv("C:/Users/Admin/Documents/ids_final_project_group_06_news_raw.csv", 
                   header = TRUE, sep = ",", stringsAsFactors = FALSE)

sum(grepl("\\b(can't|won't|n't|I'm|it's|he's|she's|they're|I've|you've|we're|you'd)\\b", 
          mydata$News_Description, ignore.case = TRUE))

expand_contractions <- function(text) {
  text <- gsub("\\bcan't\\b", "cannot", text, ignore.case = TRUE)
  text <- gsub("\\bwon't\\b", "will not", text, ignore.case = TRUE)
  text <- gsub("n't\\b", " not", text, ignore.case = TRUE)
  text <- gsub("\\bI'm\\b", "I am", text, ignore.case = TRUE)
  text <- gsub("\\bit's\\b", "it is", text, ignore.case = TRUE)
  text <- gsub("\\bhe's\\b", "he is", text, ignore.case = TRUE)
  text <- gsub("\\bshe's\\b", "she is", text, ignore.case = TRUE)
  text <- gsub("\\bthey're\\b", "they are", text, ignore.case = TRUE)
  text <- gsub("\\bI've\\b", "I have", text, ignore.case = TRUE)
  text <- gsub("\\byou've\\b", "you have", text, ignore.case = TRUE)
  text <- gsub("\\bwe're\\b", "we are", text, ignore.case = TRUE)
  text <- gsub("\\byou'd\\b", "you would", text, ignore.case = TRUE)
  text <- gsub("\\bthat's\\b", "that is", text, ignore.case = TRUE)
  text <- gsub("\\bthere's\\b", "there is", text, ignore.case = TRUE)
  text <- gsub("\\bwho's\\b", "who is", text, ignore.case = TRUE)
  text <- gsub("\\bwhat's\\b", "what is", text, ignore.case = TRUE)
  text <- gsub("\\bdoesn't\\b", "does not", text, ignore.case = TRUE)
  text <- gsub("\\bdidn't\\b", "did not", text, ignore.case = TRUE)
  text <- gsub("\\bcouldn't\\b", "could not", text, ignore.case = TRUE)
  text <- gsub("\\bshouldn't\\b", "should not", text, ignore.case = TRUE)
  text <- gsub("\\bwouldn't\\b", "would not", text, ignore.case = TRUE)
  return(text)
}


corpus <- VCorpus(VectorSource(mydata$News_Description))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removePunctuation))
corpus <- tm_map(corpus, content_transformer(removeNumbers))
corpus <- tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, content_transformer(expand_contractions))


mydata$Cleaned_Text <- sapply(corpus, as.character)

mydata$Tokens <- tokenize_words(mydata$Cleaned_Text)
mydata$Tokens_Text <- sapply(mydata$Tokens, paste, collapse = " ")

stop_words <- stopwords("en")
mydata$Tokens_NoStop <- lapply(mydata$Tokens, function(x) setdiff(x, stop_words))
mydata$Tokens_NoStop_Text <- sapply(mydata$Tokens_NoStop, paste, collapse = " ")

corpus_nostop <- VCorpus(VectorSource(mydata$Tokens_NoStop_Text))

corpus_stemmed <- tm_map(corpus_nostop, stemDocument)
mydata$Stemmed_Text <- sapply(corpus_stemmed, as.character)

corpus_lemmatized <- tm_map(corpus_nostop, content_transformer(lemmatize_strings))
mydata$Lemmatized_Text <- sapply(corpus_lemmatized, as.character)

mydata$Tokens <- NULL
mydata$Tokens_NoStop <- NULL

write.csv(mydata, "C:/Users/Admin/Desktop/ids_final_project_group_06_news_clean.csv", 
          row.names = FALSE,
          fileEncoding = "UTF-8")
