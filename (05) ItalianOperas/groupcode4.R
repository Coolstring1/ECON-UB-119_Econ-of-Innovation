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

#setwd("/Users/Rusanov/Dropbox/NYU/teach innovation ug/data_labs/5_italian_opera/code_data/") 

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

operas_w <- operas_w %>% mutate(mean_0 = operas_count_0/regions_0,
                                mean_1 = operas_count_1/regions_1)
# Make two Time Series Graphs (figure 1B below, easy to change for Figure 2B)
ggplot(operas_w) +
  geom_line(aes(x=year, y=mean_0, colour = "royalblue"), size = .8) +
  geom_point(aes(x=year, y=mean_0, colour = "royalblue"), size = 1.2) +
  geom_line(aes(x=year, y=mean_1, colour = "darkorange2"), size = .8) +
  geom_point(aes(x=year, y=mean_1, colour = "darkorange2"), size = 1.2) +
  geom_vline(xintercept = 1801, linetype = "dashed", size = 0.75) +
  geom_text(aes(x=1794, label="1801 Copyright Law", y=5.5), 
            colour="black", 
            angle=0, 
            text=element_text(size=10)) +
  scale_color_identity(name = "", 
                       breaks = c("royalblue", "darkorange2"), 
                       labels = c("Operas in regions without copyright", 
                                  "Operas in regions with copyright"), 
                       guide = "legend") +
  theme_bw() +
  xlab("Year") +
  ylab("Mean New Operas per Year") +
  ggtitle("Figure 1 - Operas in Italian regions from 1780 to 1820") +
  theme(legend.position = "right", legend.text = element_text(size = 10))

# ggsave() is a nice and reproducible way to save graph. Png is a good format.
# you can make the aspect ration different by chaning width and height
ggsave(paste0("figure1.png"),
       width = 8, height = 6, units = "in")


#########
# Part 2: table
#########


operas_table <- operas %>%
  filter(year %in% 1781:1820) %>% #drop the 1821 operas
  mutate(yr00_20 = as.numeric(year>=1801), #year is above 1801
         treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(yr00_20, treated) %>% 
  summarise(operas_count = n(),
            regions = n_distinct(state),
            length = n_distinct(year)) #%>%
  mutate(operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion, yr00_20) %>%
  spread(treated, operas_peryar_perregion)

opera_total_table <- operas %>%
  filter(year %in% 1781:1820) %>%
  mutate(treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(treated)%>%
  summarise(operas_count = n(),
            regions = n_distinct(state),
            length = n_distinct(year)) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion) %>%
  spread(treated, operas_peryar_perregion) %>%
  mutate(yr00_20=2)
  
table1_part1 <- rbind(opera_total_table , operas_table)
#########################

operas_table2 <- operas %>%
  filter(year %in% 1781:1820 & annals == 1) %>% 
  mutate(yr00_20 = as.numeric(year>=1801), #year is above 1801
         treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(yr00_20, treated) %>% 
  summarise(operas_count = n()) %>%
  mutate(regions = ifelse(treated==0, 6, 2),
         length = 20,
         operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion, yr00_20) %>%
  spread(treated, operas_peryar_perregion)

opera_total_table2 <- operas %>%
  filter(year %in% 1781:1820 & annals == 1) %>%
  mutate(treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(treated)%>%
  summarise(operas_count = n()) %>%
  mutate(regions = ifelse(treated==0, 6, 2),
         length = 40) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion) %>%
  spread(treated, operas_peryar_perregion) %>%
  mutate(yr00_20=2)

table1_part2 <- rbind(opera_total_table2 , operas_table2)
#######################

operas_table3 <- operas %>%
  filter(year %in% 1781:1820 & amazon == 1) %>% 
  mutate(yr00_20 = as.numeric(year>=1801), #year is above 1801
         treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(yr00_20, treated) %>% 
  summarise(operas_count = n()) %>%
  mutate(regions = ifelse(treated==0, 6, 2),
         length = 20,
         operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion, yr00_20) %>%
  spread(treated, operas_peryar_perregion)


opera_total_table3 <- operas %>%
  filter(year %in% 1781:1820 & amazon == 1) %>%
  mutate(treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(treated)%>%
  summarise(operas_count = n()) %>%
  mutate(regions = ifelse(treated==0, 6, 2),
         length = 40) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion) %>%
  spread(treated, operas_peryar_perregion) %>%
  mutate(yr00_20=2)

table1_part3 <- rbind(opera_total_table3 , operas_table3)

final_table1 <- rbind(table1_part1,table1_part2,table1_part3)

# 2c popular operas
popular_opera <- operas %>%
  mutate(popular = annals + amazon)
table(popular_opera$popular >0)






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
  group_by(treated) %>%
  mutate(operas_total_peryar_perregion = sum(operas_count)/(regions*length)) %>%
  ungroup() %>%
  select(treated, operas_peryar_perregion, operas_total_peryar_perregion, yr00_20)
total_values = data.frame("yr00_20" = 2, 
                          "0" = operas_table$operas_total_peryar_perregion[1], 
                          "1" = operas_table$operas_total_peryar_perregion[2])
total_values <- total_values %>% rename("0" = X0, "1" = X1)
operas_table = operas_table %>% 
                select(-operas_total_peryar_perregion) %>% 
                spread(treated, operas_peryar_perregion)
operas_table <- rbind(operas_table,total_values)


operas_table_pop_Annals <- operas %>%
  filter(year %in% 1781:1820 & annals == 1) %>% #drop the 1821 operas and include only those in the annals
  mutate(yr00_20 = as.numeric(year>1800), #year is above 1801
         treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(yr00_20, treated) %>% 
  summarise(operas_count = n(),
            regions = case_when(treated == 0 ~ 6,treated == 1 ~ 2),
            length = 20) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length)) %>%
  group_by(treated) %>%
  mutate(operas_total_peryar_perregion = sum(operas_count)/(regions*length)) %>%
  ungroup() %>%
  select(treated, operas_peryar_perregion, operas_total_peryar_perregion, yr00_20) %>%
  distinct()
total_values_pop_Annals = data.frame("yr00_20" = 2, 
                                     "0" = operas_table_pop_Annals$operas_total_peryar_perregion[1], 
                                     "1" = operas_table_pop_Annals$operas_total_peryar_perregion[2])
total_values_pop_Annals <- total_values_pop_Annals %>% rename("0" = X0, "1" = X1)
operas_table_pop_Annals = operas_table_pop_Annals %>% 
  select(-operas_total_peryar_perregion) %>% 
  spread(treated, operas_peryar_perregion)
operas_table_pop_Annals <- rbind(operas_table_pop_Annals,total_values_pop_Annals)


operas_table_pop_Amazon

operas_table_pop_total

operas_table_output <- rbind()
#########
# Part 3: regressions
#########
operas_forreg <- operas %>%
  filter(year %in% 1781:1820) %>% #drop the 1821 operas
  mutate(yr00_20 = as.numeric(year>=1801), #year is above 1801
         treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(year, yr00_20, treated, state) %>% 
  summarise(operas_count = n(),
            regions = n_distinct(state),
            length = n_distinct(year)) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length),
         post_treat = yr00_20*treated) 

operas_template <- operas_forreg %>%select(year,state)

operas_pop <- operas %>% filter(title !='' & first_name !='') %>%
  mutate(treated = state %in% c("lombardy", "venetia")) %>%
  mutate(treated = as.numeric(treated)) %>%
  group_by(year, state) %>% 
  summarise(operas_count = n()) 
  
operas_fam <- merge(operas_template, operas_pop,
                    by=c('year','state'),
                    all.x = TRUE)

operas_fam <- operas_fam %>% mutate( count_operas = replace_na(operas_count,0))%>%
  mutate(post_treat = yr00_20*treated)



reg <- felm(count_operas ~ post_treat| 
               state +year| # two FEs? use a + to add them
               0,
             data=operas_fam) 
summary(reg)


#3b
operas <- operas %>%
  mutate(pop = annals + amazon)

operas$popular <- ifelse(is.na(operas$pop), NA, operas$pop)
operas$popular <- ifelse(operas$popular>0,1,0)

operas_pop2 <- operas %>% filter(title !='' & first_name !=''& popular==1) %>%
  mutate(treated = state %in% c("lombardy", "venetia")) %>%
  mutate(treated = as.numeric(treated)) %>%
  group_by(year, state) %>% 
  summarise(operas_count = n()) 

operas_fam2 <- merge(operas_template, operas_pop2,
                    by=c('year','state'),
                    all.x = TRUE)

operas_fam2 <- operas_fam2 %>% mutate( count_operas = replace_na(operas_count,0))%>%
  mutate(post_treat = yr00_20*treated)

reg2 <- felm(count_operas ~ post_treat| 
              state +year| # two FEs? use a + to add them
              0,
            data=operas_fam2) 
summary(reg2)

#3c
reg3 <- felm(count_operas ~ post_treat + yr00_20 + post_treat| 
              0| # without 2 FE
              0,
            data=operas_fam) 
summary(reg3)
