# Petra Moser & Vasily Rusanov
# Data lab #6
# DL# 6: Biasi, Barbara, and Petra Moser “Effects of Copyright on Science. 
# Evidence from the WWII Book Replication Program” http://ssrn.com/abstract=2542879

library(tidyverse) 
library(huxtable) 
library(haven) # to read .dta files
library(gridExtra)
library(lfe) # to run fixed effect models
rm(list = ls())	

setwd("/Users/Rusanov/Dropbox/NYU/teach innovation ug/data_labs/6_book_republication/code_data/") 
books <- read_stata("books_all.dta")

###
his <- books %>%
  filter(brp==1)%>%
  group_by(id)%>%
  summarise(total_cite = sum(cit_year))


his %>%
  ggplot( aes(x=total_cite)) +
  geom_histogram( binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 5") +
  theme(
    plot.title = element_text(size=15)
  )
###cutoff greater than 200 citations and make a note in write up 
his1 <- books %>%
  filter(brp==1, year_c >1942)%>%
  group_by(id)%>%
  summarise(total_cite = sum(cit_year))

his1 %>%
  ggplot( aes(x=total_cite)) +
  geom_histogram( binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 5") +
  theme(
    plot.title = element_text(size=15)
  )

# explore the data before you do things! This looks at the cities where books are published
# You observe each book in many years. Let's keep only one observation/book and learn
# about the books we have
books_summary <- books %>% 
  # this will only keep the first observation of each book (books are indexed by id). 
  # The exact year depends on how the data is sorted
  distinct(id, .keep_all = TRUE) %>% #.keep_all means "keep variables other than id"
  group_by(field, brp) %>%
  summarize(number_books = n()) %>%
  mutate(brp = recode(brp,  `0` = "non_BRP", `1` = "BRP"))


### Question 3
# Using our summary, let's only keep the BRP books in the fields that have at least 2
# non-BRP books
fields_nonBRP <- books_summary %>%
  spread(brp, number_books) %>%#the new reshape that you should learn
  filter(non_BRP>=2) %>%
  mutate(indic_non_brp_field = 1) # you will see why I created this

# we now have 27 fields that have at lest 2 non-BRP books in them. Merge
# to the original dataset
books <- merge(books, fields_nonBRP,
               by = c("field"),
               all.x = TRUE)
  
books_comparable <- books %>% filter(indic_non_brp_field ==1)
