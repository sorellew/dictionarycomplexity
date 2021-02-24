#Measuring Complexity--traditional measures vs. dictionary
#SWG_September 9, 2020
#Farm Bill documents 109-115th Congress 

#load packages
library(dplyr)
library(tidyverse)
library(quanteda)
library(readtext)
 
#setwd
setwd("~/Desktop/Farm Bill docs")

#upload texts and create corpus
CHRG <-  "~/Desktop/Farm Bill docs/CHRG" 
CREC <- "~/Desktop/Farm Bill docs/CREC"
CDOC <- "~/Desktop/Farm Bill docs/CDOC"

#hearings
CHRG109_path <- file.path(CHRG, "CHRG_109/")
CHRG109_text <- readtext(CHRG109_path)

CHRG110_path <- file.path(CHRG, "CHRG_110/")
CHRG110_text <- readtext(CHRG110_path)

CHRG111_path <- file.path(CHRG, "CHRG_111/")
CHRG111_text <- readtext(CHRG111_path)

CHRG112_path <- file.path(CHRG, "CHRG_112/")
CHRG112_text <- readtext(CHRG112_path)

CHRG113_path <- file.path(CHRG, "CHRG_113/")
CHRG113_text <- readtext(CHRG113_path)

CHRG114_path <- file.path(CHRG, "CHRG_114/")
CHRG114_text <- readtext(CHRG114_path)

CHRG115_path <- file.path(CHRG, "CHRG_115/")
CHRG115_text <- readtext(CHRG115_path)

#congressional record
CREC109_path <- file.path(CREC, "CREC_109/")
CREC109_text <- readtext(CREC109_path)
glimpse(CREC109_text)

CREC110_path <- file.path(CREC, "CREC_110/")
CREC110_text <- readtext(CREC110_path)

CREC111_path <- file.path(CREC, "CREC_111/")
CREC111_text <- readtext(CREC111_path)

CREC112_path <- file.path(CREC, "CREC_112/")
CREC112_text <- readtext(CREC112_path)

CREC113_path <- file.path(CREC, "CREC_113/")
CREC113_text <- readtext(CREC113_path)

CREC114_path <- file.path(CREC, "CREC_114/")
CREC114_text <- readtext(CREC114_path)

CREC115_path <- file.path(CREC, "CREC_115/")
CREC115_text <- readtext(CREC115_path)

#combining datasets
cong109_text <- rbind(CHRG109_text, CREC109_text)
head(cong109_text)

cong110_text <- rbind(CHRG110_text, CREC110_text)
cong111_text <- rbind(CHRG111_text, CREC111_text)
cong112_text <- rbind(CHRG112_text, CREC112_text)
cong113_text <- rbind(CHRG113_text, CREC113_text)
cong114_text <- rbind(CHRG114_text, CREC114_text)
cong115_text <- rbind(CHRG115_text, CREC115_text)


#making corpus
cong109_corp <- corpus(cong109_text)
cong110_corp <- corpus(cong110_text)
cong111_corp <- corpus(cong111_text)
cong112_corp <- corpus(cong112_text)
cong113_corp <- corpus(cong113_text)
cong114_corp <- corpus(cong114_text)
cong115_corp <- corpus(cong115_text)

########dictionary time#####

#create tokens, remove punctuation, stopwords, common congressional phrases, create dfm
cong109toks <- tokens(cong109_corp, remove_punct=T, 
                      remove_symbols=T,
                      remove_url=T,
                      remove_numbers=T) %>%
  tokens_select(., min_nchar=4) %>%
  tokens_remove(.,c(stopwords('en')))%>%
  dfm(.,stem=F, verbose=T)
topfeatures()

cong110toks <- tokens(cong110_corp, remove_punct=T, 
                      remove_symbols=T,
                      remove_url=T,
                      remove_numbers=T) %>%
  tokens_select(., min_nchar=4) %>%
  tokens_remove(.,c(stopwords('en')))%>%
  dfm(.,stem=F, verbose=T)

cong111toks <- tokens(cong111_corp, remove_punct=T, 
                      remove_symbols=T,
                      remove_url=T,
                      remove_numbers=T) %>%
  tokens_select(., min_nchar=4) %>%
  tokens_remove(.,c(stopwords('en')))%>%
  dfm(.,stem=F, verbose=T)

cong112toks <- tokens(cong112_corp, remove_punct=T, 
                      remove_symbols=T,
                      remove_url=T,
                      remove_numbers=T) %>%
  tokens_select(., min_nchar=4) %>%
  tokens_remove(.,c(stopwords('en')))%>%
  dfm(.,stem=F, verbose=T)

cong113toks <- tokens(cong113_corp, remove_punct=T, 
                      remove_symbols=T,
                      remove_url=T,
                      remove_numbers=T) %>%
  tokens_select(., min_nchar=4) %>%
  tokens_remove(.,c(stopwords('en')))%>%
  dfm(.,stem=F, verbose=T)

cong114toks <- tokens(cong114_corp, remove_punct=T, 
                      remove_symbols=T,
                      remove_url=T,
                      remove_numbers=T) %>%
  tokens_select(., min_nchar=4) %>%
  tokens_remove(.,c(stopwords('en')))%>%
  dfm(.,stem=F, verbose=T)

cong115toks <- tokens(cong115_corp, remove_punct=T, 
                      remove_symbols=T,
                      remove_url=T,
                      remove_numbers=T) %>%
  tokens_select(., min_nchar=4) %>%
  tokens_remove(.,c(stopwords('en')))%>%
  dfm(.,stem=F, verbose=T)


#checking for top words via texstat_frequency
cong109freq <- textstat_frequency(cong109toks)
top109freq <- head(cong109freq, 25)

cong110freq <- textstat_frequency(cong110toks)
top110freq <- head(cong110freq, 25)

cong111freq <- textstat_frequency(cong111toks)
top111freq <- head(cong111freq, 25)

cong112freq <- textstat_frequency(cong112toks)
top112freq <- head(cong112freq, 25)

cong113freq <- textstat_frequency(cong113toks)
top113freq <- head(cong113freq, 25)

cong114freq <- textstat_frequency(cong114toks)
top114freq <- head(cong114freq, 25)

cong115freq <- textstat_frequency(cong115toks)
top115freq <- head(cong115freq, 25)

#creating a df of top 25 words for visual/ appendix
dftopwords <- data.frame(Column1 = c(top109freq),
                        Column2 = c(top110freq),
                        Column3 = c(top111freq),
                        Column4 = c(top112freq),
                        Column5 = c(top113freq),
                        Column6 = c(top114freq),
                        Column7 = c(top115freq))
write.csv(dftopwords, "topwords_sessioncorp.csv", row.names = F)

#now doing the same thing for a pooled document dataset

fulldoctext <- rbind(cong109_text, cong110_text, cong111_text, cong112_text, 
                      cong113_text, cong114_text, cong115_text)
fullcongcorp <- corpus(fulldoctext)
fullcongtoks <- tokens(fullcongcorp, remove_punct=T, 
                      remove_symbols=T,
                      remove_url=T,
                      remove_numbers=T) %>%
  tokens_select(., min_nchar=4) %>%
  tokens_remove(.,c(stopwords('en')))%>%
  dfm(.,stem=F, verbose=T)

fullcongreq <- textstat_frequency(fullcongtoks)
topfullcongreq <- head(fullcongreq, 25)
print(topfullcongreq)
write.csv(topfullcongreq, "fullcongress_topwords.csv", row.names = F)

head(fulldoctext)
head(cong109_text)

###----establishing a dictionary based on word frequency---##
#using tidytext

library(tidytext)

cong109_textx <- cong109_text %>% 
  unnest_tokens(word, text) %>% 
  count(doc_id, word) %>%
  select(-n) %>%
  ungroup() %>%
  count(word) %>% 
  arrange(desc(n))%>%
  anti_join(stop_words)%>%
  mutate(total=299,
         drop=n/total)

head(cong109_textx)
write_csv(cong109_textx, "cong109textx.csv")

#starting with 60% of docs
cong109dict <- filter(cong109_textx, drop>=0.6)
cong109dict <- c(cong109dict$word)
view(cong109dict)

#75% of docs measurement (robustness check)
cong109dict_75 <- filter(cong109_textx, drop>=0.75)
cong109dict_75 <- c(cong109dict_75$word)
view(cong109dict_75)

#repeating for the rest of the congresses

cong110_textx <- cong110_text %>% 
  unnest_tokens(word, text) %>% 
  count(doc_id, word) %>%
  select(-n) %>%
  ungroup() %>%
  count(word) %>% 
  arrange(desc(n))%>%
  anti_join(stop_words)%>%
  mutate(total=970,
         drop=n/total)

head(cong110_textx)
cong110dict <- filter(cong110_textx, drop>=0.6)
cong110dict <- c(cong110dict$word)

#111
cong111_textx <- cong111_text %>% 
  unnest_tokens(word, text) %>% 
  count(doc_id, word) %>%
  select(-n) %>%
  ungroup() %>%
  count(word) %>% 
  arrange(desc(n))%>%
  anti_join(stop_words)%>%
  mutate(total=367,
         drop=n/total)

head(cong111_textx)
cong111dict <- filter(cong111_textx, drop>= 0.6)
cong111dict <- c(cong111dict$word)

#75% of docs measurement (robustness check)
cong111dict_75 <- filter(cong111_textx, drop>=0.75)
cong111dict_75 <- c(cong111dict_75$word)
view(cong111dict_75)

#112
cong112_textx <- cong112_text %>% 
  unnest_tokens(word, text) %>% 
  count(doc_id, word) %>%
  select(-n) %>%
  ungroup() %>%
  count(word) %>% 
  arrange(desc(n))%>%
  anti_join(stop_words)%>%
  mutate(total=476,
         drop=n/total)

cong112dict <- filter(cong112_textx, drop>= 0.6)
cong112dict <- c(cong112dict$word)

#75% of docs measurement (robustness check)
cong112dict_75 <- filter(cong112_textx, drop>=0.75)
cong112dict_75 <- c(cong112dict_75$word)
view(cong112dict_75)

#113
cong113_textx <- cong113_text %>% 
  unnest_tokens(word, text) %>% 
  count(doc_id, word) %>%
  select(-n) %>%
  ungroup() %>%
  count(word) %>% 
  arrange(desc(n))%>%
  anti_join(stop_words)%>%
  mutate(total=796,
         drop=n/total)

head(cong113_textx)
cong113dict <- filter(cong113_textx, drop>=0.6)
cong113dict <- c(cong113dict$word)

#75% of docs measurement (robustness check)
cong113dict_75 <- filter(cong113_textx, drop>=0.75)
cong113dict_75 <- c(cong113dict_75$word)
view(cong113dict_75)

#114dict
cong114_textx <- cong114_text %>% 
  unnest_tokens(word, text) %>% 
  count(doc_id, word) %>%
  select(-n) %>%
  ungroup() %>%
  count(word) %>% 
  arrange(desc(n))%>%
  anti_join(stop_words)%>%
  mutate(total=219,
         drop=n/total)

cong114dict <- filter(cong114_textx, drop>= 0.6)
cong114dict <- c(cong114dict$word)
view(cong114dict)

#75% of docs measurement (robustness check)
cong114dict_75 <- filter(cong114_textx, drop>=0.75)
cong114dict_75 <- c(cong114dict_75$word)
view(cong114dict_75)

#115dict
cong115_textx <- cong115_text %>% 
  unnest_tokens(word, text) %>% 
  count(doc_id, word) %>%
  select(-n) %>%
  ungroup() %>%
  count(word) %>% 
  arrange(desc(n))%>%
  anti_join(stop_words)%>%
  mutate(total=473,
         drop=n/total)
view(cong115_textx)
cong115dict <- filter(cong115_textx, drop>= 0.6)

cong115dict <- c(cong115dict$word)
view(cong115dict)

#75% of docs measurement (robustness check)
cong115dict_75 <- filter(cong115_textx, drop>=0.75)
cong115dict_75 <- c(cong115dict_75$word)
view(cong115dict_75)

#now creating a dictionary based on word frequency
#this uses quanteda -- taking 

cong109freq <- textstat_frequency(cong109toks)
cong109wordict <-cong109freq[1:15021,]
head(cong109wordict)

cong110freq <- textstat_frequency(cong110toks)
cong110wordict <-cong110freq[1:23361,]

cong111freq <- textstat_frequency(cong111toks)
cong111wordict <-cong111freq[1:17834,]

cong112freq <- textstat_frequency(cong112toks)
cong112wordict <-cong112freq[1:18394,]

cong113freq <- textstat_frequency(cong113toks)
cong113wordict <-cong113freq[1:17624,]

cong114freq <- textstat_frequency(cong114toks)
cong114wordict <-cong114freq[1:17624,]

cong115freq <- textstat_frequency(cong115toks)
cong115wordict <-cong115freq[1:17007,] 
tail(cong115wordict)

#also developed a "congress" dictionary

congress <- c("committee", "house", "congress", "hearing", "session", "washington", "dc",
              "printing", "office", "government", "administration", "mr", "senator", "u.s", "chairman",
              "graphic", "statement", "bill", "american", "law", "america", "subcommittee")

##fin. now, onto the complexity/ readibility measures.

