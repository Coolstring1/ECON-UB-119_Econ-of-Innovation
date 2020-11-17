## Petra Moser & Vasily Rusanov
## Data lab #5
## Giorcelli, Michela and Moser, Petra, Copyrights and Creativity: 
## Evidence from Italian Operas (December 28, 2016).
## Available at SSRN: https://ssrn.com/abstract=2505776
## published version: https://www.journals.uchicago.edu/doi/abs/10.1086/710534
library(tidyverse) 
library(huxtable) 
library(haven) # to read .dta files
library(gridExtra)
library(lfe) # to run fixed effect models
rm(list = ls())	

setwd("/Users/Rusanov/Dropbox/NYU/teach innovation ug/data_labs/5_italian_opera/code_data/") 

#########
# Part 1: figure
#########
# Import the data
# The file 20years.dta has *all*  the operas created in 1781-1821 (even if the composer
# and/or the name is unknown). 
operas <- read_stata("20years.dta")  # read_dta is used for .dta files

#let's look at the different regions in italy
print(unique(operas$state))

operas_c <- operas %>% mutate(treated = state %in% c("lombardy", "venetia")) %>%
  mutate(treated = as.numeric(treated)) %>%
  group_by(year, treated) %>% 
  summarise(operas_count = n(),
            regions = n_distinct(state)) 

# ask yourself the following questions about the lines above
# if you don't know the answer, please ask!
# 1) do you understand what mutate() does?
# 2) why are there two variables in group_by()? 
# what would the result be if there was only one?
# 3) what does n() mean? why not sum()? what is n_distinct()?
# 4) is the result in the long or the wide format?


### GRAPH ###
# reshape data to wide to make graphs
operas_w <- reshape(as.data.frame(operas_c),
                   idvar = "year", # which var is id
                   direction = "wide", # how you want things reshaped
                   timevar = "treated",  # which var is repeating
                   sep = "_")

# Make two Time Series Graphs (figure 1B below, easy to change for Figure 2B)
ggplot(operas_w) +
  geom_line(aes(x=year, y=operas_count_0, colour = "royalblue"), size = .8) +
  geom_point(aes(x=year, y=operas_count_0, colour = "royalblue"), size = 1.2) +
  geom_line(aes(x=year, y=operas_count_1, colour = "darkorange2"), size = .8) +
  geom_point(aes(x=year, y=operas_count_1, colour = "darkorange2"), size = 1.2) +
  scale_color_identity(name = "", 
                       breaks = c("royalblue", "darkorange2"), 
                       labels = c("Operas in regions without copyright", 
                                  "Operas in regions with copyright"), 
                       guide = "legend") +
  theme_bw() +
  xlab("Year") +
  ylab("Operas, total") +
  ggtitle("Figure 1 - Opears in Italian regions") +
  theme(legend.position = "right", legend.text = element_text(size = 10))

# ggsave() is a nice and reproducible way to save graph. Png is a good format.
# you can make the aspect ration different by chaning width and height
ggsave(paste0("figure1.png"),
       width = 8, height = 6, units = "in")


#########
# Part 2: table
#########
# Please learn a new way to reshape. Maybe you'll like it better
# than the inbuilt one
operas_table <- operas %>%
  filter(year %in% 1781:1820) %>% #drop the 1821 operas
  mutate(yr00_20 = as.numeric(year>1800), #year is above 1801
         treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(yr00_20, treated) %>% 
  summarise(operas_count = n(),
            regions = n_distinct(state),
            length = n_distinct(year)) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion, yr00_20) %>%
  spread(treated, operas_peryar_perregion) #the new reshape that you should learn

new_table <- rbind(operas_table ,operas_table) #combine two tables 
# (here, I just combined the table with itself)

#########
# Part 3: regressions
#########
operas_forreg <- operas %>%
  filter(year %in% 1781:1820) %>% #drop the 1821 operas
  mutate(yr00_20 = as.numeric(year>1800), #year is above 1801
         treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(year, yr00_20, treated, state) %>% 
  summarise(operas_count = n(),
            regions = n_distinct(state),
            length = n_distinct(year)) %>%
  mutate(post_treat = yr00_20*treated) 

operas_template <- operas_forreg %>% select(year, state)

operas_fam <- operas %>% filter(title != "" & last_name != "") %>%
  mutate(treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  mutate(treated = as.numeric(treated)) %>%
  group_by(year,state) %>%
  summarize(operas_count = n(),
            regions = n_distinct(state))

operas_fam_col <- merge(operas_template, 
                        operas_fam, 
                        by = c("year","state"), 
                        all.x = TRUE)

# I will first do without fixed effects, and then with (opposit of what the lab wants)
reg1 <- felm(operas_count ~  treated  + yr00_20 +post_treat| 
               0 | #without state fixed effects
               0,
             data=operas_forreg)
tidy(reg1) #nice command for a summary of regression

# now with state FE
reg2 <- felm(operas_count ~ yr00_20 +post_treat| 
               state| #with state fixed effects (fe variable goes after the first | )
               0,
             data=operas_forreg)

# now with state and year FE
reg2 <- felm(operas_count ~ post_treat| 
               state +year| # two FEs? use a + to add them
               0,
             data=operas_forreg)

# alternative way to integrate state fixed effects
reg2_alternative <- felm(operas_count ~ yr00_20 + post_treat + factor(state)|
                        0,
                      data = operas_forreg)

# reg3 will give an error. This is because you can't
# estimate the effect of being treated AND the effect of being a particular state.
reg3 <- felm(operas_count ~ treated + yr00_20 +post_treat| 
               state| #with state fixed effects
               0,
             data=operas_forreg)
# this error is the reason why I remove the treated and teh yr00_20 from the 
# regressor when I include the fixed effects
