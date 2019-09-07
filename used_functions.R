removeConfounders <- function(x) {
  x <- gsub("-", " ", x)
  x <- gsub(":", " ", x)
  x <- gsub(" -", " ", x)
  x <- gsub("- ", " ", x)
  x <- gsub(";", " ", x)
  x <- gsub("won't", "will not", x)
  x <- gsub("can't", "cannot", x)
  x <- gsub("'re", " are", x)
  x <- gsub("'ve", " have", x)
  x <- gsub("what's", "what is", x)
  x <- gsub("n't", " not", x)
  x <- gsub("'d", " would", x)
  x <- gsub("'ll", " will", x)
  x <- gsub("'m", " am", x)
}

graphToSpace <- function(x){x <- gsub("[^[:graph:]]", " ", x)}