aggregate_table <- function(dataset, grp) {
  library("dplyr")
  dataset %>% 
    group_by(grp)
    tab_cells(colnames()) %>%
    tab_cols(total(label = "#Total| |"), am) %>% 
    tab_stat_fun(Mean = w_mean, "Std. dev." = w_sd, "Valid N" = w_n, method = list) %>%
    tab_pivot()
  return(dataset)
    
}

energy_supp_by_src <- read.csv("data/electicity_supply_by_fuel_source_clean.csv", stringsAsFactors = FALSE)
aggregate_table(energy_supp_by_src, "Year")
