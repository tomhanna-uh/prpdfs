library(here)
library(pdftools)
library(tesseract)

#read in pdf document using pdf_text() function
rcmp_report <- pdf_text(here("samplepdfs","RCMP_External_Review_Committee_AR_2018.pdf"))

#inspect results, printing results of page 30
rcmp_report[30]

#read in using pdf_data() function
rcmp_report <- pdf_data(here("samplepdfs","RCMP_External_Review_Committee_AR_2018.pdf"))

#inspect results, printing results of page 1 (title page)
rcmp_report[1]

#get info from pdf using pdf_info
rcmp_report_info <- pdf_info(here("samplepdfs","RCMP_External_Review_Committee_AR_2018.pdf"))

#print 2nd, 6th, and 10th items of info list (I inspected earlier and so happen to know these are the number of pages, date created, and attachements variables)
rcmp_report_info[c(2, 6, 10)]

#read in pdf document using pdf_text() function
atip_file <- pdf_ocr_text(here("samplepdfs","Memoranum_to_MND.pdf"))

#inspect results, printing results of page 1
atip_file[1]

rm(list = ls())

#sew wd to where files are located
setwd("~/3 - R Studio Projects/prpdfs")

#create character list of all files ending with .pdf in folder 
pdf_files <- list.files("~/3 - R Studio Projects/prpdfs/initialpdfs", pattern = "pdf$", full.names = TRUE)

#use lapply() to apply pdf_text or other pdftools function iteractively across each of the files
results <- lapply(pdf_files, pdf_text)
results[1]

#load stringr, dplyr, and purrr to manipulate the text into final form
library(stringr)
library(dplyr)
library(purrr)
library(quanteda)

# Clean_String <- function(string){
#     # Lowercase
#     temp <- tolower(string)
#     # Remove everything that is not a number or letter (may want to keep more 
#     # stuff in your actual analyses). 
#     temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
#     # Shrink down to just one white space
#     temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
#     # Split it
#     temp <- stringr::str_split(temp, " ")[[1]]
#     # Get rid of trailing "" if necessary
#     indexes <- which(temp == "")
#     if(length(indexes) > 0){
#         temp <- temp[-indexes]
#     } 
#     return(temp)
# }
# 
# clean_results <- Clean_String(results)
# print(clean_results)
# 
# # sentence <- "The term 'data science' (originally used interchangeably with 'datalogy') has existed for over thirty years and was used initially as a substitute for computer science by Peter Naur in 1960."
# # clean_sentence <- Clean_String(sentence)
# 
# # function to clean text
# Clean_Text_Block <- function(text){
#     # Get rid of blank lines
#     indexes <- which(text == "")
#     if (length(indexes) > 0) {
#         text <- text[-indexes]
#     }
#     # See if we are left with any valid text:
#     if (length(text) == 0) {
#         cat("There was no text in this document! \n")
#         to_return <- list(num_tokens = 0, 
#                           unique_tokens = 0, 
#                           text = "")
#     } else {
#         # If there is valid text, process it.
#         # Loop through the lines in the text and combine them:
#         clean_text <- NULL
#         for (i in 1:length(text)) {
#             # add them to a vector 
#             clean_text <- c(clean_text, Clean_String(text[i]))
#         }
#         # Calculate the number of tokens and unique tokens and return them in a 
#         # named list object.
#         num_tok <- length(clean_text)
#         num_uniq <- length(unique(clean_text))
#         to_return <- list(num_tokens = num_tok, 
#                           unique_tokens = num_uniq, 
#                           text = clean_text)
#     }
#     
#     return(to_return)
# }
# 
# clean_results <- Clean_Text_Block(results)
# str(clean_results)
# 
# Rcpp::sourceCpp('Generate_Document_Word_Matrix.cpp')
# 
# 
# # Create a list containing a vector of tokens in each document for each
# # document. These can be extracted from the cleaned text objects as follows:
# doc_list <- list(results_1 = clean_results$text)  
# 
# # Create a vector of document lengths (in tokens):
# doc_lengths <- c(clean_results$num_tokens)  
# 
# # Generate a vector containing the unique tokens across all documents:
# unique_words <- unique(c(clean_results$text))  
# 
# # The number of unique tokens across all documents:
# n_unique_words <- length(unique_words)  
# 
# # The number of documents we are dealing with:
# ndoc <- 1 
# 
# # Now feed all of this information to the function as follows:
# Doc_Term_Matrix <- Generate_Document_Word_Matrix(
#     number_of_docs = ndoc,
#     number_of_unique_words = n_unique_words,
#     unique_words = unique_words,
#     Document_Words = doc_list,
#     Document_Lengths = doc_lengths)  
# 
# # Make sure to add column names to Doc_Term_Matrix, then take a look:
# colnames(Doc_Term_Matrix) <- unique_words
# 

# http://www.mjdenny.com/Text_Processing_In_R.html

# Create a vector with one string per document:
# docs <- c(paste0(text,collapse = " "),paste0(text2,collapse = " ")) 

# Create a vector with one string per document:


docs <- list()

for (i in 1:102) {
    docs[i] <- c(paste0(results[i],collapse = " ")) 
}

docs <- do.call("rbind",docs)
summary(docs)

# #Formerly, dfm() could be called directly on a character or corpus 
# #object, but we now steer users to tokenise their inputs first using
# #tokens(). 
# 
# doc_tokens <- tokens(docs, what = "word", remove_punct = TRUE, remove_symbols = TRUE, 
#        remove_numbers = TRUE, remove_url = TRUE, remove_separators = TRUE,
#        padding = TRUE)

# # load the package and generate the document-term matrix
# doc_term_matrix <- dfm(docs, stem = FALSE)

# # find the additional terms captured by quanteda
# missing <- doc_term_matrix@Dimnames$features %in% colnames(Doc_Term_Matrix)
# 
# # We can see that our document term matrix now includes terms with - and ' included.
# doc_term_matrix@Dimnames$features[which(missing == 0)]


corp_prop <- corpus(docs)  # build a new corpus from the texts
summary(corp_prop)


#Formerly, dfm() could be called directly on a character or corpus 
#object, but we now steer users to tokenise their inputs first using
#tokens(). 

prop_tokens <- tokens(corp_prop, what = "word", remove_punct = TRUE, remove_symbols = TRUE, 
                     remove_numbers = TRUE, remove_url = TRUE, remove_separators = TRUE,
                     padding = TRUE)
summary(prop_tokens)


# load the package and generate the document-term matrix
doc_term_matrix <- dfm(prop_tokens)
doc_term_matrix <- dfm_select(doc_term_matrix, pattern = c("\\w{3,}"), selection = "keep", valuetype = "regex")

stemmed_matrix <- dfm_wordstem(doc_term_matrix)
stemmed_matrix <- dfm_select(stemmed_matrix, pattern = stopwords("english"), selection = "remove", valuetype = "fixed")

summary(stemmed_matrix)

topfeatures(stemmed_matrix, 100)

set.seed(100)
library("quanteda.textplots")
textplot_wordcloud(stemmed_matrix, min_count = 6, random_order = FALSE, rotation = 0.25,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))