# Petra Moser & Vasily Rusanov
# Data lab #6
# DL# 6: Biasi, Barbara, and Petra Moser “Effects of Copyright on Science. 
# Evidence from the WWII Book Replication Program” http://ssrn.com/abstract=2542879

library(tidyverse) 
library(huxtable) 
library(haven) # to read .dta files
library(gridExtra)
library(lfe) # to run fixed effect models
library(class)
library(mlr)
rm(list = ls())	

#setwd("/Users/Rusanov/Dropbox/NYU/teach innovation ug/data_labs/6_book_republication/code_data/") 
books <- read_stata("books_all.dta")


###Question 1

books_summary <- books %>% 
  # this will only keep the first observation of each book (books are indexed by id). 
  # The exact year depends on how the data is sorted
  distinct(id, .keep_all = TRUE) %>% #.keep_all means "keep variables other than id"
  group_by(field, brp) %>%
  summarize(number_books = n()) %>%
  mutate(brp = recode(brp,  `0` = "non_BRP", `1` = "BRP"))

books_summary1 <- books_summary %>%
  group_by(brp)%>%
  summarise(total = sum(number_books))
### Answer: 291 and compounds

##histograms
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


###Question 2

##Comparing Citations to the same BRP Book by English-language and Other Authors
regr_table <- books %>%
  filter(brp == 1)%>%
  group_by(year_c,id) %>%
  summarise(Eng = sum(count_eng),
            Other = sum(count_noeng))

reg_table1<- regr_table %>%
  group_by(year_c) %>%
  summarise(Eng = sum(Eng)/n(),
            Other = sum(Other)/n()) %>%
  filter(year_c>=1930) 
  

ggplot(reg_table1) +
  geom_line(aes(x=year_c, y=Eng, colour = "royalblue"), size = .8) +
  geom_point(aes(x=year_c, y=Eng, colour = "royalblue"), size = 1.2) +
  geom_line(aes(x=year_c, y=Other, colour = "darkorange2"), size = .8) +
  geom_point(aes(x=year_c, y=Other, colour = "darkorange2"), size = 1.2) +
  geom_vline(xintercept = 1942, linetype = "dashed", size = 0.75) +
  geom_text(aes(x=1938, label="BRP", y=0.8), 
            colour="black", 
            angle=0, 
            text=element_text(size=10)) +
  scale_color_identity(name = "", 
                       breaks = c("royalblue", "darkorange2"), 
                       labels = c("English", 
                                  "Other"), 
                       guide = "legend") +
  theme_bw() +
  xlab("Year") +
  ylab("Citations per book and year") +
  ggtitle("Figure 1 - Citation BRP Books in English verses Other languages") +
  theme(legend.position = "right", legend.text = element_text(size = 10))


reg_ass1 <- regr_table %>%
  filter(year_c >=1930) %>%
  mutate(change = Eng - Other) 

reg_ass1$post <- ifelse(reg_ass1$year_c>=1942,1,0)
reg_ass1$English <- ifelse(reg_ass1$Eng>0,1,0)
reg_ass1$English_post <- ifelse(reg_ass1$English + reg_ass1$post>=2,1,0)

reg_assumption1 <- felm(change ~ English_post| 
               id +year_c| # two FEs? use a + to add them
               0,
             data=reg_ass1) 
summary(reg_assumption1)



##Comparing BRP and Swiss books

fields_nonBRP <- books_summary %>%
  spread(brp, number_books) %>%#the new reshape that you should learn
  filter(non_BRP>=10) %>%
  mutate(indic_non_brp_field = 1) # you will see why I created this

# we now have 27 fields that have at lest 2 non-BRP books in them. Merge
# to the original dataset
books <- merge(books, fields_nonBRP,
               by = c("field"),
               all.x = TRUE)

books_comparable <- books %>% filter(indic_non_brp_field ==1)

books_comparable <- books_comparable %>%
  filter(chemistry==1|mathematics==1)

#################
##try to calculate similar number of citations here but something went wrong
test <- books_comparable %>% 
  group_by(id,brp)%>%
  summarise(total_cite = sum(cit_year))


testw <- reshape(as.data.frame(test),
                         idvar = c("id"), # which var is id
                         direction = "wide", # how you want things reshaped
                         timevar = "brp",  # which var is repeating
                         sep = "_") 

testw <- testw %>%
  mutate(ch = total_cite_1-total_cite_0)
###################

regr_table2 <- books_comparable %>%
  group_by(year_c,brp,id)%>%
  summarise(Eng = sum(count_eng))

test <- regr_table2 %>%
  group_by(brp)%>%
  summarise(n = n())

regr_table2_1 <- regr_table2 %>%
  group_by(year_c,brp) %>%
  summarise(cit = sum(Eng)/n())


regr_table2_w <- reshape(as.data.frame(regr_table2_1),
                     idvar = c("year_c"), # which var is id
                     direction = "wide", # how you want things reshaped
                     timevar = "brp",  # which var is repeating
                     sep = "_") %>%
  filter(year_c>=1930)



ggplot(regr_table2_w) +
  geom_line(aes(x=year_c, y=cit_0, colour = "royalblue"), size = .8) +
  geom_point(aes(x=year_c, y=cit_0, colour = "royalblue"), size = 1.2) +
  geom_line(aes(x=year_c, y=cit_1, colour = "darkorange2"), size = .8) +
  geom_point(aes(x=year_c, y=cit_1, colour = "darkorange2"), size = 1.2) +
  geom_vline(xintercept = 1942, linetype = "dashed", size = 0.75) +
  geom_text(aes(x=1938, label="BRP", y=0.8), 
            colour="black", 
            angle=0, 
            text=element_text(size=10)) +
  scale_color_identity(name = "", 
                       breaks = c("royalblue", "darkorange2"), 
                       labels = c("Swiss", 
                                  "BRP"), 
                       guide = "legend") +
  theme_bw() +
  xlab("Year") +
  ylab("Citations per book and year") +
  ggtitle("Figure 2 - Citation BRP Books and Swiss Book") +
  theme(legend.position = "right", legend.text = element_text(size = 10))

####regression

reg_ass2 <- regr_table2
reg_ass2$post <- ifelse(reg_ass2$year_c>=1942,1,0)
reg_ass2$post_treated <- ifelse(reg_ass2$post + reg_ass2$brp>=2,1,0)

reg_assumption2 <- felm(Eng ~ post_treated| 
                          id +year_c| # two FEs? use a + to add them
                          0,
                        data=reg_ass2) 
summary(reg_assumption2)



##distance
knn <- books %>%
  group_by(id,field,year_c) %>%
  summarise(cite = sum(count_eng) + sum(count_noeng))

knn1 <-knn  %>%
  filter(year_c<1942) %>%
  group_by(field,id) %>%
  summarise(cite = sum(cite))
knn1 <- knn1%>%
  filter(field!='')

knn1 <- knn1 %>% 
  mutate(v = 1, fie = field) %>% 
  spread(fie, v, fill = 0)

normalize <- function(x){
  return ((x-min(x)) / (max(x)-min(x)))
}
sub <- knn1[,3:36]
subset <- as.data.frame(lapply(knn1[,3:36],normalize))
set.seed(0)
temp <- as.data.frame(mahalanobis(sub, colMeans(sub), cov(sub),tol=1e-20))
final <- cbind(knn1,temp)
names(final)[37] <- "distance"

set <- final %>%
  select('id','distance') %>%
  filter(distance <= 2)
book_wanted <- set[2]



