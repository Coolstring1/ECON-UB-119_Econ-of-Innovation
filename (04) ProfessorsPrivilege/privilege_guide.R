### Petra Moser & Vasily Rusanov
### Data lab #4
### Hvide, Hans K., and Benjamin F. Jones. 2018. 
### "University Innovation and the Professor's Privilege." 
### American Economic Review, 108 (7): 1860-98.

library(tidyverse) 
library(huxtable) 
library(haven) # to read .dta files
library(gridExtra)
library(lfe) # to run fixed effect models
rm(list = ls())	

# Import the data
setwd("~/My Work/Programming Projects/R/Econ of Innovation/(04) ProfessorPrivilege")
startups <- read_stata("aggregate_startups.dta")  # read_dta is used for .dta files
patents <- read_stata("aggcount_patents.dta")
startups_by_nace <- read_stata("science_yearly_nace.dta")

### GRAPH ###
# reshape data to wide to make graphs
startups_w <- reshape(as.data.frame(startups),
                   idvar = "yr", # which var is id
                   direction = "wide", # how you want things reshaped
                   timevar = "treated",  # which var is repeating
                   sep = "_")

patents_w <- reshape(as.data.frame(startups),
                   idvar = "yr", # which var is id
                   direction = "wide", # how you want things reshaped
                   timevar = "treated",  # which var is repeating
                   sep = "_")

# Make two Time Series Graphs (figure 1B below, easy to change for Figure 2B)
ggplot(startups_w) +
  geom_line(aes(x=yr, y=startups_pc_0, colour = "royalblue"), size = .8) +
  geom_point(aes(x=yr, y=startups_pc_0, colour = "royalblue"), size = 2) +
  geom_line(aes(x=yr, y=startups_pc_1, colour = "darkorange2"), size = .8) +
  geom_point(aes(x=yr, y=startups_pc_1, colour = "darkorange2"), size = 2) +
  scale_color_identity(name = "", 
                       breaks = c("royalblue", "darkorange2"), 
                       labels = c("Startups by University researchers", "Startups by non-Univ. workers"), 
                       guide = "legend") +
  theme_bw() +
  xlab("") +
  ylab("Startups founded, per researcher/per worker outside universities") +
  ggtitle("Figure 1 - Startups founded in Norway") +
  geom_vline(xintercept = 2002.5, linetype = "dashed", color = "black", size = .8) +
  theme(legend.position = "right", legend.text = element_text(size = 10))


### Diff-in-diff ###
# Doing diff-in-diff using a t-test
# The easiest way to do it in code is to
# 1) reshape the data to Wide format
# 2) calculate the changes *for each group*
# 3) do a t-test on changes
  
startups_by_nace_w <- startups_by_nace %>% filter(sample %in% c(0,2) & # I love the %in% operator
                                                    stiftaar %in% c(2002,2003)) %>%
  # n_workers reflects the num. of workers in a given group
  # (for sample ==0,, it's "University researchers)
  mutate(startups_p100k = n_startups/n_workers * 100000) %>%
  select(sample, stiftaar, n1, startups_p100k)

startups_by_nace_w <- reshape(as.data.frame(startups_by_nace_w),
                              idvar = c("sample", "n1"), # which var is id
                              direction = "wide", # how you want things reshaped
                              timevar = "stiftaar",  # which var is repeating
                              sep = "_")

startups_by_nace_w <- startups_by_nace_w %>% 
  arrange(sample, n1) %>%
  mutate(perc_change = startups_p100k_2003/startups_p100k_2002 - 1) %>%
  mutate(perc_change = replace_na(perc_change, 0)) # it's a choice: zero to zero means 0% growth
# I recommend that you look at the meaning of NACA codes. You will be 
# surprised to see what kind of startups the researchers stopped founding after 2003. 

# please do not screenshot the output of t-test and then include it
# in your report. Only use the relvant numbers
# if you need to interpret the results of a t-test, ask us.

# assumptions: percentage change is normally distributed, 
# identically for all naca fields within the groups (for example, all )
t.test(perc_change ~sample, 
       data = startups_by_nace_w, 
       paired = FALSE)

# based on the t-test, we strongly reject the null that 
# change in patenting for group 0 is the same as for group 2

### Running a regression ###
#generate startups per 100k people
startups_by_nace <- startups_by_nace %>%
  # n_workers reflects the num. of workers in a given group
  # (for sample ==0,, it's "University researchers)
  mutate(startups_p100k = n_startups/n_workers * 100000) 


reg1 <- felm(startups_p100k ~ treated + after + after_treated, #no fixed efects
             data = startups_by_nace %>% filter(sample %in% c(0,2))) # sample==2 is workers outside unis

reg2 <- felm(startups_p100k ~ treated + after + after_treated | 
              n1, # field fixed effects
             data = startups_by_nace %>% filter(sample %in% c(0,4))) # sample==4 is workers with PhD

# a summary that is easier to read
huxreg(reg1, reg2, 
       stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01))

# make it pretty and export 
# (other formats are supported, not just excel)
huxreg(reg1, reg2, 
       stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01),
       coefs = c("Treated X post-2003" = "after_treated", # rename vars to smthing pretty
                 "Post-2003" = "after",
                 "Treated" = "treated"),
       statistics = c("N" = "nobs", 
                      "R^2" = "r.squared")) %>%
  add_rows(rbind(c("Year FE", "no", "no"), #note! you need more "yes" if you have >3 models
                 c("Control group", "All workers", "With PhD")), 
           copy_cell_props = FALSE,
           after = c(nrow(.) - 3)) %>% 
  quick_docx("report_reg.docx")

  