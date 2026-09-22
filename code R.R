install.packages("gravity")
install.packages("slider")
install.packages("broom")
install.packages("penppml")
install.packages("rlang")

library(readxl)
library(tidyr)
library(tidyverse) 
library(ggplot2)
library(gravity)
library(slider)   
library(broom)    
library(penppml)
library(stats)
library(fixest)
library(sandwich)    
library(lmtest)      



update.packages(ask = FALSE, checkBuilt = TRUE)

donnees <- read_excel("C:/Users/flori/Documents/eco migration/Devoir bonus/stock_migrant_onu.xlsx")
donnees <- read_excel("C:/Users/flori/Documents/eco migration/Devoir bonus/stock_migrant_onu.xlsx")

donnees_long <- donnees %>%
  pivot_longer(
    cols = matches("^(total|males|females)_\\d+"), 
    names_to = c("sexe", "annee"),                
    names_pattern = "(.*)_(\\d+)",                
    values_to = "valeur"                           
  )

donnees_long <- donnees_long %>%
  mutate(id_bilat = paste(origin, destination, sep = "_"))

donnees_long <- donnees_long %>%
  mutate(id_bil_num = group_indices(., origin, destination))

donnees_diff <- donnees_long %>%
  group_by(id_bil_num, sexe) %>%
  arrange(annee, .by_group = TRUE) %>%
  mutate(
    diff_valeur = valeur - lag(valeur),
    type_flux = ifelse(diff_valeur < 0, "retour", "depart"),
    diff_valeur = abs(diff_valeur)
  ) %>%
  ungroup()



list_countries <- read.csv("C:/Users/flori/Documents/eco migration/Devoir bonus/UNSD_Methodology.csv", sep = ";", stringsAsFactors = FALSE) %>%
  rename_with(~ gsub("\\.", "_", .)) %>%
  select(-Global_Code, -Global_Name) %>%
  rename(
    code      = M49_Code,
    iso3_code = ISO_alpha3_Code,
    iso2_code = ISO_alpha2_Code,
    SIDS      = Small_Island_Developing_States__SIDS_,
    LLDC      = Land_Locked_Developing_Countries__LLDC_,
    LDC       = Least_Developed_Countries__LDC_
  ) %>%
  mutate(
    SIDS = as.numeric(ifelse(SIDS == "x", 1, 0)),
    LLDC = as.numeric(ifelse(LLDC == "x", 1, 0)),
    LDC  = as.numeric(ifelse(LDC == "x", 1, 0))
  )

attr(list_countries$SIDS, "label") <- "Small_Island_Developing_States"
attr(list_countries$LLDC, "label") <- "Land_Locked_Developing_Countries"
attr(list_countries$LDC,  "label") <- "Least_Developed_Countries"



list_countries_orig <- list_countries %>%
  rename_with(~ paste0(., "_o"))


list_countries_dest <- list_countries %>%
  rename_with(~ paste0(., "_d"))


list_by_2 <- expand.grid(
  iso3_code_o = list_countries_orig$iso3_code_o,
  iso3_code_d = list_countries_dest$iso3_code_d,
  stringsAsFactors = FALSE
) %>%
  filter(iso3_code_o != iso3_code_d) %>%
  left_join(list_countries_orig, by = "iso3_code_o") %>%
  left_join(list_countries_dest, by = "iso3_code_d") %>%
  rename(code_origin = code_o,
         code_destination = code_d)


years <- seq(1990, 2020, by = 5)


list_by_2_time <- list_by_2 %>%
  crossing(annee = years)

donnees <- read_excel("C:/Users/flori/Documents/eco migration/Devoir bonus/stock_migrant_onu.xlsx")

donnees_long <- donnees %>%
  pivot_longer(
    cols = matches("_(\\d+)$"),     
    names_to = c("variable", "annee"),
    names_pattern = "(.*)_(\\d+)",
    values_to = "valeur"
  ) %>%
  pivot_wider(
    names_from  = "variable",        
    values_from = "valeur",
    names_prefix = "stock_"
  ) %>%
  mutate(annee = as.integer(annee)) 

donnees_long <- donnees_long %>%
  mutate(
    id_bilat  = paste(origin, destination, sep = "_"),
    id_bil_num = group_indices(., origin, destination)
  )


donnees_flux <- donnees_long %>%
  arrange(id_bil_num, annee) %>%
  group_by(id_bil_num) %>%
  mutate(
    flux_total   = lead(stock_total)   - stock_total, 
    flux_males   = lead(stock_males)   - stock_males,
    flux_females = lead(stock_females) - stock_females,
    migration_total   = if_else(flux_total   > 0, flux_total,   0),
    migration_males   = if_else(flux_males   > 0, flux_males,   0),
    migration_females = if_else(flux_females > 0, flux_females, 0)
  ) %>%
  ungroup()



donnees_reverse <- donnees_flux %>%
  select(annee, code_destination, code_origin, flux_total, flux_males, flux_females) %>%
  mutate(
    reverse_migration_total   = if_else(flux_total   < 0, abs(flux_total),   0),
    reverse_migration_males   = if_else(flux_males   < 0, abs(flux_males),   0),
    reverse_migration_females = if_else(flux_females < 0, abs(flux_females), 0)
  ) %>%
  rename(code_origin_temp      = code_destination,
         code_destination_temp = code_origin) %>%
  rename(code_origin      = code_origin_temp,
         code_destination = code_destination_temp) %>%
  select(-flux_total, -flux_males, -flux_females)


donnees_finales <- donnees_flux %>%
  left_join(
    donnees_reverse,
    by = c("code_destination", "code_origin", "annee")
  ) %>%
  mutate(
    mig_with_return_total   = migration_total   + coalesce(reverse_migration_total, 0),
    mig_with_return_males   = migration_males   + coalesce(reverse_migration_males, 0),
    mig_with_return_females = migration_females + coalesce(reverse_migration_females, 0)
  )


panel_final <- list_by_2_time %>%
  left_join(
    donnees_finales,
    by = c("code_origin", "code_destination", "annee")
  ) %>%
  mutate(
    stock_total   = ifelse(is.na(stock_total), 0, stock_total),
    stock_males   = ifelse(is.na(stock_males), 0, stock_males),
    stock_females = ifelse(is.na(stock_females), 0, stock_females)
  )

flux_cols <- c(
  "flux_females", "flux_males", "flux_total",
  "mig_with_return_females", "mig_with_return_males", "mig_with_return_total",
  "migration_females", "migration_males", "migration_total",
  "reverse_migration_females", "reverse_migration_males", "reverse_migration_total"
)


panel_final <- panel_final %>%
  mutate(across(all_of(flux_cols),
                ~ ifelse(is.na(.) & annee != 2025, 0, .)))


names(panel_final)
summary(panel_final$stock_total)
summary(panel_final$flux_total)


write.csv(panel_final, "panel_final_migration.csv", row.names = FALSE)



cepii_data <- read.csv("C:/Users/flori/Documents/eco migration/Devoir bonus/Gravity_V202211.csv", stringsAsFactors = FALSE) %>%
  filter(year >= 1980)

num_vars_df <- cepii_data %>% select(where(is.numeric))
names(num_vars_df)


vars_ma <- c("pop_o","pop_d","gdp_o","gdp_d","gdpcap_o","gdpcap_d",
             "gdp_ppp_o","gdp_ppp_d","gdpcap_ppp_o","gdpcap_ppp_d",
             "pop_pwt_o","pop_pwt_d","gdp_ppp_pwt_o","gdp_ppp_pwt_d",
             "entry_cost_o","entry_cost_d","entry_proc_o","entry_proc_d",
             "entry_time_o","entry_time_d","entry_tp_o","entry_tp_d",
             "tradeflow_comtrade_o","tradeflow_comtrade_d",
             "tradeflow_baci","manuf_tradeflow_baci","tradeflow_imf_o","tradeflow_imf_d")

vars_growth <- c("pop_o","pop_d","gdp_o","gdp_d","gdpcap_o","gdpcap_d",
                 "gdp_ppp_o","gdp_ppp_d","gdpcap_ppp_o","gdpcap_ppp_d",
                 "pop_pwt_o","pop_pwt_d","gdp_ppp_pwt_o","gdp_ppp_pwt_d",
                 "tradeflow_comtrade_o","tradeflow_comtrade_d",
                 "tradeflow_baci","manuf_tradeflow_baci","tradeflow_imf_o","tradeflow_imf_d")


vars_keep <- unique(c(vars_ma, vars_growth))

vars_keep <- c("year", "iso3_o", "iso3_d", vars_keep)

cepii_ma_growth <- cepii_data %>%
  select(all_of(vars_keep))     

cepii_ma_growth <- cepii_ma_growth %>%
  arrange(iso3_o, iso3_d, year) %>%
  group_by(iso3_o, iso3_d) %>%
  mutate(across(all_of(vars_ma),
                ~ slide_dbl(.x, ~mean(.x, na.rm = TRUE), .before = 4, .complete = TRUE),
                .names = "{.col}_ma5")) %>%
  mutate(across(all_of(vars_growth),
                ~ (lag(.x, 1) - lag(.x, 5)) / lag(.x, 5) * 100,
                .names = "{.col}_growth5")) %>%
  ungroup()%>%
  select(iso3_o, iso3_d, year, matches("ma5$|growth5$"))



cepii_merged <- cepii_data %>%
  left_join(cepii_ma_growth, by = c("iso3_o", "iso3_d", "year")) %>%
  filter(year %% 5 == 0)%>%
  rename(
    annee = year,
    iso3_code_o = iso3_o,
    iso3_code_d = iso3_d
  )


panel_complete <- panel_final %>%
  left_join(cepii_merged, by = c("iso3_code_o", "iso3_code_d", "annee")) 
saveRDS(panel_complete, file = "panel_complete.rds")


install.packages("WDI")
library(WDI)

new_vars <- WDI(indicator = c("unempl" = "SL.UEM.TOTL.ZS", 
                              "pol_stab" = "PV.EST"), 
                start = 1990, end = 2020, extra = TRUE)



new_vars <- WDI(indicator = c("unempl" = "SL.UEM.TOTL.ZS", 
                              "pol_stab" = "PV.EST"), 
                start = 1990, end = 2020, extra = TRUE)


new_vars_clean <- new_vars %>%
  select(iso3c, year, unempl, pol_stab) %>%
  filter(!is.na(iso3c))


panel_augmented <- panel_complete %>%
  left_join(new_vars_clean, by = c("iso3_code_o" = "iso3c", "annee" = "year")) %>%
  rename(unempl_o = unempl, pol_stab_o = pol_stab)


panel_augmented <- panel_augmented %>%
  left_join(new_vars_clean, by = c("iso3_code_d" = "iso3c", "annee" = "year")) %>%
  rename(unempl_d = unempl, pol_stab_d = pol_stab)



summary(panel_augmented[c("migration_total", "dist", "gdpcap_o_ma5", "gdpcap_d_ma5", "pop_o_ma5", "pop_d_ma5", "unempl_o", "unempl_d",
                          "pol_stab_o", "pol_stab_d")])






model_final <- ppml(
  dependent_variable = "migration_total",
  distance = "dist",
  additional_regressors = c(
    "comlang_off",      
    "contig",           
    "pop_o.x", "pop_d.x",
    "gdpcap_o.x", "gdpcap_d.x",  
    "unempl_o", "unempl_d",      
    "pol_stab_o", "pol_stab_d"   
  ),
  pair_id = "id_bil_num",
  time = "annee",
  origin = "iso3_code_o",
  destination = "iso3_code_d",
  data = panel_augmented,  
  fe = TRUE,
  robust = TRUE,
  verbose = TRUE
)

summary(model_final)
capture.output(summary(model_final), file = "Resultats_Regression.txt")

library(fixest)
library(modelsummary)


ppml_fixest <- fepois(
  migration_total ~ log(dist) +              
    comlang_off + contig +   
    log(pop_o.x) + log(pop_d.x) +        
    log(gdpcap_o.x) + log(gdpcap_d.x) +   
    unempl_o + unempl_d +    
    log(pol_stab_o) + log(pol_stab_d) | 
    annee + iso3_code_o + iso3_code_d,
  data = panel_augmented,
  cluster = "id_bil_num"
)


modelsummary(
  list("Modèle PPML" = ppml_fixest),
  fmt = 4,   
  coef_map = c(
    "log(dist)" = "Distance (log)",
    "log(pop_o.x)" = "Pop. Origine (log)",
    "log(pop_d.x)" = "Pop. Destination (log)",
    "log(gdpcap_o.x)" = "PIB/hab Origine (log)",
    "log(gdpcap_d.x)" = "PIB/hab Destination (log)",
    "unempl_o" = "Chômage Origine",
    "unempl_d" = "Chômage Destination",
    "comlang_off" = "Langue Commune",
    "contig" = "frontière",
    "log(pol_stab_o)" = "Insécurité origine",
    "log(pol_stab_d)" = "Insécurité destination"
  ),
  stars = TRUE,
  output = "Tableau_Resultats_Final.html"
)