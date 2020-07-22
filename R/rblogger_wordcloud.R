library(rvest)
library(stringr)
library(tidyverse)
library(tm)
library(wordcloud)


root <- "https://www.r-bloggers.com/"
all_pages <- c(root, paste0(root, "page/", 2:5))


scrape_post_titles <- function(site) {
    print(paste("Download", site))
    source_html <- read_html(site)
    titles <- source_html %>%
        html_nodes("h3") %>%
        html_nodes("a") %>%
        html_text()
    titles <- titles[!is.na(titles)]
    return(titles)
}

all_titles <- lapply(all_pages, scrape_post_titles)
all_titles <- unlist(all_titles)

cleaned <- all_titles %>%
                tolower() %>%
                removeNumbers() %>%
                removeWords(c("r", "using", stopwords("en"))) %>%
                removePunctuation() %>%
                str_trim()

corpus_cleaned <- Corpus(VectorSource(cleaned))
doc_object <- TermDocumentMatrix(corpus_cleaned)
doc_matrix <- as.matrix(doc_object)
counts <- sort(rowSums(doc_matrix),decreasing=TRUE)
counts <- counts[grepl("^[a-z]+$", names(counts))]
frame_counts <- data.frame(word = names(counts), freq = counts)

wordcloud(words = frame_counts$word,
          freq = frame_counts$freq,
          min.freq = 3,
          max.words=200, random.order=FALSE, rot.per=0.2,
          colors=brewer.pal(8, "Dark2"))
