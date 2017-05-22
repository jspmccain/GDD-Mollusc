#residual plots for analysis

#must load GDD Exclusion Criteria Applied.R, and 2Analysis Pipeline GDD Mollusc.R

setwd("C:/Users/Scott/Dropbox/GDD_2016/Plots/Residual Plots")

df_fin$temp <- df_fin$temp %>% as.factor()

lapply(1:length(unique(df_fin$unique.id)), 
                            FUN = table.pop, 
                            df_fin, 
                            "gdd",
                            "residuals")

lapply(1:length(unique(df_fin$unique.id)), 
                            FUN = table.pop, 
                            df_fin, 
                            "time",
                            "residuals")