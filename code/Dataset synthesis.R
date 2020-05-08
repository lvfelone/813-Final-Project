
library("here")
library("tidyverse")
library("broom")
library("expss")


# ------- make MID data set -------

MID_participant <- haven::read_dta(here("data", "MIDB - participant 4.3.dta"))

MID_slim <- select(MID_participant, 
                   ccode,
                   styear,
                   endyear,
                   hostlev)

write_csv(MID_slim, here("data", "MID-slim-export.csv"))



# ------- manipulate MID data set -------

total_hostlev_all <- MID_slim %>% 
  group_by(ccode) %>% 
  count(hostlev) %>% 
  mutate(total = sum(n))


total_hostlev_45 <- MID_slim %>% 
  filter(hostlev == 4 | hostlev == 5) %>% 
  group_by(ccode) %>% 
  count(hostlev) %>% 
  mutate(total_45 = sum(n))



