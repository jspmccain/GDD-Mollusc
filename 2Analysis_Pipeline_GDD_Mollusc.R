#Analysis Pipeline GDD Mollusc, table and plots. 

###PSEUDOCODE

#filter by dataset
#filter by temperature
#regression for each temperature lm(size ~ gdd), lm(size ~ time)
#REPORT the coefficients for gdd and time, as well as the number of observations, the intercept values, the r squared adjusted, and the p values. Each model for unique temperatures will be one row of the final reported dataframe.

#for the aggregated dataset (ie. not filtered by temperature)
#ancova
#lm(size ~ time*temp)
#lm(size ~ gdd*temp)

#summary(ancova)
#is the interaction between temperature treatments and time significant? 
#IF YES: report that "Different slopes (p value)"
#IF NO: report "Similar slopes (P value) THEN
#test for similar intercepts by running
#lm(size ~ time + temp) or lm(size ~ gdd + temp)

#summary(lm.no.interaction)
#is the variable temp significant? 

##FUNCTIONS

#Model summary function. This function takes a model with just time or gdd as the argument, and returns the model summary in one line as a row of dataframe
temp.reg.sum <- function(lm.model.output){
  lm.out.sum <- summary(lm.model.output)
  coeff.slope <- paste(coefficients(lm.out.sum)[2] %>% round(3), " (", coefficients(lm.out.sum)[4] %>% round(3), ")", sep = "")
  coeff.intercept <- paste(coefficients(lm.out.sum)[1] %>% round(3), " (", coefficients(lm.out.sum)[3] %>% round(3), ")", sep = "")
  r.sq <- lm.out.sum$adj.r.squared %>% round(3)
  p.value <- round(1 - pf(lm.out.sum$fstatistic[1], lm.out.sum$fstatistic[2], lm.out.sum$fstatistic[3]), 3)
  if(p.value < 0.0001){
    p.value2 <- "< 0.0001"
  } else {
    p.value2 <- as.character(p.value)
  }
  df.mid <- data.frame(`Slope Coefficient` = coeff.slope, `Intercept Coefficient` = coeff.intercept, `Adjusted R Squared` = r.sq, `P Value` = p.value2, `Comparison` = "")
  rownames(df.mid) <- NULL
  return(df.mid)
}

#Fits models for a unique temperature value. 
temp.models <- function(number, data.set.b, pred = "time", plotting = "no"){
  df.a <- filter(data.set.b, temp == unique(data.set.b$temp)[number])
  form1 <- paste("size ~", pred, sep = " ")#making a model formula
  lm.out.pred <- lm(data = df.a, as.formula(form1))
  if(plotting == "no"){
    lm.out.pred2 <- temp.reg.sum(lm.out.pred)
    temp.value <- unique(data.set.b$temp)[number]
    data.no <- dim(df.a)[1]
    df.return <- data.frame(`Paper ID` = df.a$unique.id[1], `Temperature` = temp.value, `Measurement Unit` = df.a$size.met[1], `Data.n` = data.no, `Explanatory.Variable` = pred, `Test` = "Regression")
    df.return2 <- cbind(df.return, lm.out.pred2)
  } else {
    pred1 <- predict(lm.out.pred, interval = "confidence")
    df.return2 <- cbind(pred1, temp = rep(df.a$temp[1], dim(df.a)[1])) %>% as.data.frame()#putting the temperature data for each prediction interval
  }
  return(df.return2)
}

##spot checking model summaries for the above functions. 
# test <- filter(df_fin, unique.id == "Hodgson1988_C.hastata_Fig5", temp == 16)
# test.lm <- lm(size ~ gdd, data = test)
# summary(test.lm)
# temp.models(1, data.set.b = test, pred = "gdd")

#the functions below populate a table of analyses. table.pop() is the main wrapper function. It runs a regression model for each temperature, and then determines if the temperature treatments have different slopes (ANCOVA). Select "plotting" if you want a dataframe to be returned that can be implemented for plotting the data and the results conveniently. 

#this function is for plotting the analysis results. the output is such that confidence intervals will be plotted given a certain result. however this function really only makes sense in the context of function below, "table.pop()"
plotting_function <- function(data_frame_temp = df1, 
                              ancova.ano.slope.p.value_i = ancova.ano.slope.p.value,
                              ancova.ano.intercept.p.value_i = ancova.ano.intercept.p.value,
                              pred_i = pred) {
  temperature_predictions <- lapply(1:length(unique(data_frame_temp$temp)), FUN = temp.models, data_frame_temp, pred_i, "yes")
  
  ancova.formula2 <- paste("size ~", pred_i, "+ temp", sep = "")
  ancova.out2 <- lm(data = data_frame_temp, as.formula(ancova.formula2))
  ancova.formula3 <- paste("size ~", pred_i, sep = "")
  ancova.out3 <- lm(data = data_frame_temp, formula = as.formula(ancova.formula3))
  
  #if the slopes are similar, and the intercepts are similar  v
  if(ancova.ano.slope.p.value_i > 0.05 & ancova.ano.intercept.p.value_i > 0.05){
    temperature_predictions_anc <- predict(object = ancova.out3,#need this last model as a prediction, because it only has gdd as an explanatory variables. 
                                           interval = "confidence", 
                                           type = "response")
    temperature_predictions2 <- cbind(temperature_predictions_anc, 
                                      data_frame_temp)
    temperature_predictions2$test2 <- rep("ancova.g", 
                                          dim(data_frame_temp)[1])
  } else if(ancova.ano.slope.p.value_i > 0.05 & ancova.ano.intercept.p.value_i < 0.05){
    temperature_predictions_anc2 <- predict(object = ancova.out2, 
                                            interval = "confidence", 
                                            type = "response")
    temperature_predictions2 <- cbind(temperature_predictions_anc2, 
                                      data_frame_temp)
    temperature_predictions2$test2 <- rep("ancova.tg", 
                                          dim(data_frame_temp)[1])
  } else if(ancova.ano.slope.p.value_i < 0.05){
    temperature_predictions2 <- cbind(do.call("rbind.data.frame", temperature_predictions), data_frame_temp)
    temperature_predictions2$test2 <- rep("regressions", 
                                          dim(data_frame_temp)[1])
    temperature_predictions2 <- temperature_predictions2[,-4]
  }
  pred2 <- rep(pred_i, dim(data_frame_temp)[1])
  temperature_predictions3 <- cbind(temperature_predictions2, pred2) %>% as.data.frame()
  return(temperature_predictions3)
}

#this function is for plotting the residuals of the gdd model results. it directly makes a residual plot and closes the corresponding graphics device. 
residual_checker <- function(data_frame_temp = df1, 
                             ancova.ano.slope.p.value_i = ancova.ano.slope.p.value,
                             ancova.ano.intercept.p.value_i = ancova.ano.intercept.p.value,
                             pred_i = pred){

  ancova.formula2 <- paste("size ~", pred_i, "+ temp", sep = "")
  ancova.out2 <- lm(data = data_frame_temp, as.formula(ancova.formula2))
  ancova.formula3 <- paste("size ~", pred_i, sep = "")
  ancova.out3 <- lm(data = data_frame_temp, formula = as.formula(ancova.formula3))
  
  jpeg(sprintf("%s.jpeg", paste(data_frame_temp$unique.id[1], pred_i)), width=170, height=210, units="mm", res=850)
  
  if(ancova.ano.slope.p.value_i > 0.05 & ancova.ano.intercept.p.value_i > 0.05){
    par(mfrow = c(2, 2))
    plot(ancova.out3, 
         main = data_frame_temp$unique.id[1])
  } else if(ancova.ano.slope.p.value_i > 0.05 & ancova.ano.intercept.p.value_i < 0.05){
    par(mfrow = c(2, 2))
    plot(ancova.out3, 
         main = data_frame_temp$unique.id[1])
  } else if(ancova.ano.slope.p.value_i < 0.05){
    plot(x = 1, y = 1, main = data_frame_temp$unique.id[1])
  }
  
  dev.off()
}

table.pop <- function(number, #this number corresponds to a unique data set identifier.
                           data.set = df_fin, #supplies the function with a certain dataset. 
                           pred = "time", #whether or not you are doing ancovas for "time" or "gdd"
                           plotting = "nope"){#if these results are going into a table, plotting = "nope"/ if these results are going into a figure, plotting = "yes"
  
  #below settings have been used for debugging
  # #
  # number <- 2
  # data.set <- df_fin
  # pred <- "gdd"
  # plotting <- "yes"
  # df1 <- filter(df_fin, unique.id == unique(df_fin$unique.id)[number])
  # #
  
  #filtering each unique data set i
  df1 <- filter(data.set, unique.id == unique(data.set$unique.id)[number])#filtering each unique data set i. Each data set is a unique
  df2 <- lapply(1:length(unique(df1$temp)), FUN = temp.models, df1, pred)#running a regression for each temperature within dataset i
  df3 <- do.call("rbind.data.frame", df2)#Summary data frame of each of these regressions. 
  
  #run the ancova on the df1
  ancova.formula <- paste("size ~ ", pred, "*temp")# size ~ gdd/time*temp
  ancova.out <- lm(data = df1, as.formula(ancova.formula))#running ancova on df1, which is the dataframe of a unique paper id (all temps)
  ancova.ano <- ancova.out %>% anova()#ancova analysis of variance table
  ancova.sum <- ancova.out %>% summary()#ancova summary table
  
  #this if statement makes a string called ancova_statement, which summarises the results of the ancova at a high level. If the interaction between gdd/time with temperature treatments is significant, that indicates that the slopes are different. 
  ancova.ano.slope.p.value <- ancova.ano$`Pr(>F)`[3]
  
  ancova.ano.intercept.p.value <- ancova.ano$`Pr(>F)`[2]
  # ancova.ano.intercept.p.value <- ancova.sum$coefficients[, "Pr(>|t|)"][1]
  
  if(ancova.ano.slope.p.value > 0.05){#If the slopes are similar (not significantly different)
    slopes <- paste("Similar slopes (p = ", ancova.ano.slope.p.value %>% round(3) %>% as.character(), ")", sep = "")#statement about similar slopes
    #then if there are similar slopes, lets test whether there are similar intercepts.
    if(ancova.ano.intercept.p.value > 0.05){ 
      intercepts <- paste("Similar intercepts (p = ", ancova.ano.intercept.p.value %>% round(3) %>% as.character(), ")", sep = "")
    } else {
      intercepts <- paste("Different intercepts (p = ", ancova.ano.intercept.p.value %>% round(3) %>% as.character(), ")", sep = "")
    }
    ancova_statement <- paste(slopes, intercepts, sep = " ")
  } else {
    ancova_statement <- paste("Different slopes (p = ", ancova.ano.slope.p.value %>% round(3) %>% as.character(), ")", sep = "")
  }
  #this if statement changes a p value equal to zero to "p < 0.00001". this is correcting an artifact of the rounding of the p values
  if(grepl(pattern = "(p = 0)", x = ancova_statement)){
    ancova_statement <- gsub(pattern = "(p = 0)", replacement = "(p < 0.001)", x = ancova_statement, fixed = TRUE)
  } else {
    ancova_statement <- ancova_statement
  }
  #if the ancova came out as significant, we have to input summary values of the slope, intercept, r squared and p value. If not, we leave that blank. 
  ancova.formula2 <- paste("size ~", pred, "+ temp", sep = "")
  ancova.out2 <- lm(data = df1, as.formula(ancova.formula2))
  ancova.sum2 <- ancova.out2 %>% summary()
  ancova.ano2 <- ancova.out2 %>% anova()
  
  #this third general linear model is for when the slopes and intercepts are both the same, then I refit a model which just had gdd or time as an explanatory variable
  ancova.formula3 <- paste("size ~", pred, sep = "")
  ancova.out3 <- lm(data = df1, formula = as.formula(ancova.formula3))
  
  #summary of ancova for table:
  coeff.slope <- paste(coefficients(ancova.sum2)[2, 1] %>% round(3), " (", coefficients(ancova.sum2)[2, 2] %>% round(3), ")", sep = "")#the coefficient for the slope as well as the st. err
  coeff.intercept <- paste(coefficients(ancova.sum2)[1,1] %>% round(3), " (", coefficients(ancova.sum2)[1, 2] %>% round(3), ")", sep = "")#the coefficient for the intercept as well as the st. err
  r.sq <- ancova.sum2$adj.r.squared %>% round(3)
  p.value <- round(1 - pf(ancova.sum2$fstatistic[1], ancova.sum2$fstatistic[2], ancova.sum2$fstatistic[3]), 3)
  if(p.value < 0.0001){
    p.value2 <- "< 0.0001"
  } else {
    p.value2 <- as.character(p.value)
  }
  
  #df.return1 and df.return2 are summaries of the paper and then summaries of the analysis. They represent one row in the table.
  df.return1 <- data.frame(`Paper ID` = df1$unique.id[1], 
                           `Temperature` = "", 
                           `Measurement Unit` = df1$size.met[1], 
                           `Data.n` = ancova.out2$residuals %>% length(), 
                           `Explanatory.Variable` = pred, 
                           `Test` = "ANCOVA")
  df.return2 <- data.frame(`Slope Coefficient` = coeff.slope, 
                           `Intercept Coefficient` = coeff.intercept, 
                           `Adjusted R Squared` = r.sq, 
                           `P Value` = p.value2, 
                           Comparison = ancova_statement)
  #if the slopes are similar (not significantly different), then combine the above. If not, then return a blank row.
  if(ancova.ano.slope.p.value > 0.05){
    df.return3 <- cbind(df.return1, df.return2)
  } else {
    #blank row to be returned if slopes are different.
    df.return3 <- data.frame(`Paper ID` = df1$unique.id[1], 
                             `Temperature` = "", 
                             `Measurement Unit` = df1$size.met[1], 
                             `Data.n` = ancova.out$residuals %>% length(), 
                             `Explanatory.Variable` = pred, 
                             `Test` = "ANCOVA", 
                             `Slope Coefficient` = "", 
                             `Intercept Coefficient` = "", 
                             `Adjusted R Squared` = "", 
                             `P Value` = "", 
                             Comparison = ancova_statement)
  }
  #making an empty line for a nice looking table
  df.return4 <- data.frame(`Paper ID` = "", 
                           `Temperature` = "", 
                           `Measurement Unit` = "", 
                           `Data.n` = "", 
                           `Explanatory.Variable` = "", 
                           `Test` = "", 
                           `Slope Coefficient` = "", 
                           `Intercept Coefficient` = "", 
                           `Adjusted R Squared` = "", 
                           `P Value` = "", 
                           Comparison = "")
  
  #final summary for each 
  df.finale <- rbind(df3,#regressions for each temperature treatment
                     df.return3,#ancova results
                     df.return4)#empty row
  
  #this if statement was put it to streamline the figure plotting. 
  if(plotting == "nope"){
    return(df.finale)
  } else if(plotting == "yes"){
    plotting_function(data_frame_temp = df1, 
                      ancova.ano.slope.p.value_i = ancova.ano.slope.p.value,
                      ancova.ano.intercept.p.value_i = ancova.ano.intercept.p.value, 
                      pred_i = pred)
  } else if(plotting == "residuals"){
    residual_checker(data_frame_temp = df1, 
                     ancova.ano.slope.p.value_i = ancova.ano.slope.p.value,
                     ancova.ano.intercept.p.value_i = ancova.ano.intercept.p.value,
                     pred_i = pred)
  }
}

#writing the summary table
setwd("C:/Users/Scott/Dropbox/GDD_2016/Tables")

df_fin$temp <- df_fin$temp %>% as.factor()

summary_table_gdd <- lapply(1:length(unique(df_fin$unique.id)), FUN = table.pop, df_fin, "gdd")
summary_table_gdd_df <- do.call("rbind.data.frame", summary_table_gdd)

summary_table_time <- lapply(1:length(unique(df_fin$unique.id)), FUN = table.pop, df_fin, "time")
summary_table_time_df <- do.call("rbind.data.frame", summary_table_time)

analysis_summary_table <- cbind(summary_table_time_df, summary_table_gdd_df)

write.csv(analysis_summary_table, "analysis_summary2.csv")


#writing a summary file for summary plot.
table.gdd <- summary_table_gdd_df
table.time <- summary_table_time_df

measurement.gdd <- table.gdd[grep(pattern = "slope", x = table.gdd$Comparison),]$Measurement.Unit
measurement.time <- table.time[grep(pattern = "slope", x = table.time$Comparison),]$Measurement.Unit

#####

unique.id <- table.gdd[grep(pattern = "slope", x = table.gdd$Comparison),]$Paper.ID
comparison.gdd <- table.gdd[grep(pattern = "slope", x = table.gdd$Comparison),]$Comparison
comparison.time <- table.time[grep(pattern = "slope", x = table.time$Comparison),]$Comparison

summary_df <- data.frame(unique.pid = unique.id, 
                         gdd.comp = comparison.gdd, 
                         time.comp = comparison.time, 
                         measurement.unit = measurement.time)

write.csv(summary_df, "summary_table_of_results.csv")

##spot checking function output (i.e. doing the models manually and seeing if their output looks the same as the functions above)
# table.pop(grep("Andre2009_O.ocellatus_Fig2", x = unique(df_fin$unique.id)), data.set = df_fin, pred = "time", plotting = "nope")
# data.test <- df_fin %>% filter(unique.id == "Andre2009_O.ocellatus_Fig2")
# lm.out1 <- lm(size ~ gdd*temp, data = data.test)
# anova.test <- lm.out1 %>% anova()
# pred.test <- predict(lm.out1, interval = "confidence") %>% as.data.frame()
# data.test2 <- cbind(data.test, pred.test)

##testing the prediction part of the table.pop function, which is meant for plotting
#molts.gdd <- table.pop(grep("Dom", x = unique(df_fin$unique.id)), data.set = df_fin, pred = "gdd", plotting = "yes")


