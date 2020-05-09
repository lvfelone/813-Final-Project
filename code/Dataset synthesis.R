
library("here")
library("tidyverse")
library("broom")


# ------- make MID data set -------

MID_participant <- haven::read_dta(here("data", "MIDB - participant 4.3.dta"))

MID_slim <- select(MID_participant, 
                   ccode,
                   styear,
                   endyear,
                   hostlev)

                   

# ------- manipulate MID data set -------

total_hostlev_all <- MID_slim %>% 
  group_by(ccode) %>% 
  count(hostlev) %>% 
  mutate(total = sum(n))


total_hostlev_45 <- MID_slim %>% 
  group_by(ccode) %>% 
  summarize(
    total_conflicts = sum(hostlev %in% c(4,5))
    ) 

write_csv(total_hostlev_45, here("data", "MID-hostlev-45.csv"))

MID_hostlev_45 <- read_csv(here("data", "MID-hostlev-45.csv"))



# ------- manipulate IGO data set -------

IGO <- read_csv(here("data", "IGO_state_year_formatv3.csv"))

tallied_by_IGO <- IGO %>%
  filter(year %in% c(1816:2010)) %>% 
  group_by(ccode) %>%
  summarize_at(
    .vars = vars(AAAID:Wassen),
    .funs = ~ sum(. %in% c(1, 2))
  ) 


IGO_final <- tallied_by_IGO %>% 
  mutate(
    total_member_years = rowSums(tallied_by_IGO,c(-1))
  ) %>% 
  select(
    ccode,
    total_member_years, 
    AAAID:Wassen
  )


IGO %>% 
  count(ccode)

write_csv(IGO_final, here("data", "IGO-final.csv"))


# ------- power control -------

power <- read_csv(here("data", "NMC_5_0.csv"))

power_slim <- power %>%
  filter(year %in% c(1816:2010)) %>% 
  group_by(ccode) %>%
  summarize_at(
    .vars = vars(milex, milper, tpop, cinc),
    .funs = ~ mean(.)
  ) 

write_csv(power_slim, here("data", "power-slim"))


# ------- merge data -------

Merged_MID_IGO <- left_join(MID_hostlev_45, IGO_final)

Merged_final <- left_join(Merged_MID_IGO, power_slim) %>% 
  select(ccode,
         total_conflicts,
         total_member_years,
         milex,
         milper,
         tpop,
         cinc,
         AAAID:Wassen
         )

write_csv(Merged_final, here("data", "FINAL.csv"))
