#Code for Figures 2-4, Broell, McCain, and Taggart (2017)

#SCripts required to load: Data formatting

setwd("C:\\Users\\Scott\\Dropbox\\GDD_2016\\Data\\Development")

# #final publication plots ------------------------------------------------

df_pub <- df_fin %>% filter(unique.id == "Britz1996_H.midae" | 
                              unique.id == "Prvhinik1984_C.fornicata_Fig1" | 
                              unique.id == "Domingues2002_S.officinalis" |
                              unique.id == "Forsythe1988_O.bimaculoides_female" | 
                              unique.id == "Robert1998_O.edulis_20ppt" | 
                              unique.id == "Robert1998_O.edulis_25ppt" | 
                              unique.id == "Robert1998_O.edulis_30ppt" | 
                              unique.id == "Robert1998_O.edulis_35ppt" |
                              unique.id == "Kubiriza2010_B.nyassanus_Fig2Embryo" |#embryo size 
                              unique.id == "Hayhurt2001_M.edulis_Fig11")#development: developmental stage

df_pub$temp <- df_pub$temp %>% as.character() %>% as.factor()

plot.tables.gdd <- lapply(1:10, FUN = table.pop, df_pub, "gdd", "yes")
plot.tables.time <- lapply(1:10, FUN = table.pop, df_pub, "time", "yes")
  
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# gdd.plot2(plot.tables.gdd[[5]], supply_xlab ="true", supply_ylab = "false", xlab.i = expression(paste("Growing Degree Days (", degree, "C*day)")), ylab.i = "Nothing", y.per = 0.99, x.per = 0.025, legend.present = "no")

#publication level plotting function. Axes are not input because of the final plotting configuration. 

gdd.plot2 <- function(table.pop.out, 
                      supply_xlab = "true", 
                      supply_ylab = "false", 
                      xlab.i = "", 
                      ylab.i = "something", 
                      x.per = 0.1, 
                      y.per = 0.9, 
                      axes.i = "nope", 
                      legend.present = "yes",
                      custom.range = c(130, 190)){
  #capitalizing each first letter of each word
  table.pop.out$temp <- table.pop.out$temp %>% as.character() %>% as.numeric()
  nums <- c(19, 0, 8, 25, 22)
  wow2 <- colorRampPalette(c("grey40", "black"))
  wow3 <- wow2(length(unique(table.pop.out$temp)))
  col_select_num <- which(colnames(table.pop.out) == table.pop.out$pred2[1])
  ylab1 <- table.pop.out$size.met[1]
  ylab2 <- str_split(ylab1, pattern = "\\.") %>% unlist()
  ylabmain <- ylab2[-length(ylab2)]#removing the last item of the vector words, which is the unit
  ylabmain2 <- simpleCap(ylabmain)
  
  ylab.units <- paste("(", ylab2[length(ylab2)], ")", sep = "")
  ylab.main3 <- paste(ylabmain2, sep = " ", collapse = " ")
  ylab.final <- paste(ylab.main3, ylab.units)
  if(col_select_num == "time"){
    xlab.final <- "Time (days)"
  } else {
    xlab.final <- "Growing Degree Days"
  }
  
  if(supply_xlab == "false"){
    xlab.final <- xlab.final
  } else {
    xlab.final <- xlab.i
  }
  
  if(supply_ylab == "false"){
    ylab.final <- ylab.final
  } else {
    ylab.final <- ylab.i
  }
  
  if(axes.i == "nope"){
    plot(x = table.pop.out[ ,col_select_num], 
         y = table.pop.out$size,
         pch = nums[as.factor(table.pop.out$temp)],
         # col = wow3[as.factor(table.pop.out$temp)],
         ylab = ylab.final,
         xlab = xlab.final,
         cex = 2, 
         axes = FALSE,
         cex.lab = 1.55)
  } else if(axes.i == "yes") {
    plot(x = table.pop.out[ ,col_select_num], 
         y = table.pop.out$size,
         pch = nums[as.factor(table.pop.out$temp)],
         # col = wow3[as.factor(table.pop.out$temp)],
         ylab = ylab.final,
         xlab = xlab.final,
         cex = 7,
         cex.lab = 3)
  } else if(axes.i == "standardized") {
    plot(x = table.pop.out[ ,col_select_num],
         y = table.pop.out$size,
         pch = nums[as.factor(table.pop.out$temp)],
         ylab = ylab.final,
         xlab = xlab.final,
         cex = 2,
         axes = FALSE,
         ylim = custom.range,
         cex.lab = 1.55)
  }
  
  
  for(i in 1:length(unique(table.pop.out$temp))){
    df1 <- table.pop.out %>% filter(temp == unique(table.pop.out$temp)[i])
    lines(df1[ ,col_select_num],
          df1$lwr,
          lty = 3)
    lines(df1[ ,col_select_num],
          df1$upr,
          lty = 3)
    lines(df1[ ,col_select_num],
          df1$fit,
          lty = 1)
  }
  if(legend.present == "yes"){
    legend(x = (table.pop.out[, col_select_num] %>% max())*x.per, 
           y = (table.pop.out$size %>% max())*y.per,
           unique(table.pop.out$temp),
           pch = nums,
           # col = wow3,
           bty = "n",
           cex = 1.2)
  }
}


setwd("C:\\Users\\Scott\\Dropbox\\GDD_2016\\Plots\\Publication Plots")


#Figure 2. 
dev.off()

jpeg("Broell et al_MEPS_201611016_Rev2_Fig2.jpeg", width=170, height=210, units="mm", res=1000)

axis_font_size <- 1
axis_number_size <- 1.25

par(mfrow = c(5, 2))
par(cex = 0.6)
par(mar = c(3, 0, 0, 0), oma = c(4, 4, 0.65, 0.65))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
###
gdd.plot2(plot.tables.time[[1]], supply_xlab ="true", supply_ylab = "false", xlab.i = "", ylab.i = "Nothing", y.per = 0.99, x.per = 0.025)
axis(2, at = seq(10, 40, 5), cex.axis = axis_number_size)
axis(1, at = seq(0, 120, 20), cex.axis = axis_number_size)
mtext("Length (mm)", side = 2, outer = TRUE, cex = axis_font_size, line = 2.2, adj = 0.96)
text(x = 0, y = 28.5, "(a)", cex = 1.75)

gdd.plot2(plot.tables.gdd[[1]], supply_xlab ="true", supply_ylab = "false", xlab.i = "", ylab.i = "Nothing", y.per = 0.99, x.per = 0.025, legend.present = "no")
axis(1, at = seq(0, 2000, 500), cex.axis = axis_number_size)
text(x = 0, y = 28.5, "(b)", cex = 1.75)

###
gdd.plot2(plot.tables.time[[2]], supply_xlab = "true", supply_ylab = "false", xlab.i = "", ylab.i = "Nothing", y.per = 0.99, x.per = 0.025)
axis(1, at = seq(0, 250, 50), cex.axis = axis_number_size)
axis(2, at = seq(0, 14, 2), cex.axis = axis_number_size)
mtext("Length (mm)", side = 2, outer = TRUE, cex = axis_font_size, line = 2.2, adj = 0.75)
text(x = 0, y = 12, labels = "(c)", cex = 1.75)

gdd.plot2(plot.tables.gdd[[2]], supply_xlab ="true", supply_ylab = "false", xlab.i = "", ylab.i = "Nothing", y.per = 0.99, x.per = 0.025, legend.present = "no")
axis(1, at = seq(0, 3500, 500), cex.axis = axis_number_size)
text(x = 0, y = 11.5, labels = "(d)", cex = 1.75)
###
gdd.plot2(plot.tables.time[[3]], supply_xlab ="true", supply_ylab = "false", xlab.i = "", ylab.i = "Nothing", y.per = 0.90, x.per = 0.35)
axis(1, at = seq(100, 400, 50), cex.axis = axis_number_size)
axis(2, at = seq(0, 12, 2), cex.axis = axis_number_size)
mtext("Length (mm)", side = 2, outer = TRUE, cex = axis_font_size, line = 2.2, adj = 0.51)
text(x = 140, y = 10, "(e)", cex = 1.75)

gdd.plot2(plot.tables.gdd[[3]], supply_xlab ="true", supply_ylab = "false", xlab.i = "", ylab.i = "Nothing", y.per = 0.99, x.per = 0.025, legend.present = "no")
axis(1, at = seq(2000, 8000, 1000), cex.axis = axis_number_size)
text(x = 2800, y = 10, "(f)", cex = 1.75)

###
gdd.plot2(plot.tables.time[[6]], supply_xlab ="true", supply_ylab = "false", xlab.i = "", ylab.i = "Nothing", y.per = 0.99, x.per = 0.025)
axis(1, cex.axis = axis_number_size)
axis(2, at = seq(200, 1200, 200), cex.axis = axis_number_size)
mtext("Length (um)", side = 2, outer = TRUE, cex = axis_font_size, line = 2.2, adj = 0.3)
text(x = 0, y = 1100, "(g)", cex = 1.75)

gdd.plot2(plot.tables.gdd[[6]], supply_xlab ="true", supply_ylab = "false", xlab.i = "", ylab.i = "Nothing", y.per = 0.99, x.per = 0.025, legend.present = "no")
axis(1, at = seq(0, 250, 50), cex.axis = axis_number_size)
text(x = 0, y = 1100, "(h)", cex = 1.75)

###
gdd.plot2(plot.tables.time[[5]], supply_xlab ="true", supply_ylab = "false", xlab.i = "Time (days)", ylab.i = "Nothing", y.per = 0.99, x.per = 0.025)
axis(1, at = seq(0, 200, 50), cex.axis = axis_number_size)
axis(2, cex.axis = axis_number_size)
text(x = 0.1, y = 600, labels = "(i)", cex = 1.75)

gdd.plot2(plot.tables.gdd[[5]], supply_xlab ="true", supply_ylab = "false", xlab.i = expression(paste("Growing Degree Days (", degree, "C*day)")), ylab.i = "Nothing", y.per = 0.99, x.per = 0.025, legend.present = "no")
axis(1, at = seq(0, 5000, 1000), cex.axis = axis_number_size)
mtext("Embryo Size (ug)", side = 2, outer = TRUE, cex = axis_font_size, line = 2.2, adj = 0.06)
text(x = 0.1, y = 600, labels = "(j)", cex = 1.75)

dev.off()

#Salinity plots

axis_font_size <- 0.95

jpeg("Broell et al_MEPS_201611016_Rev2_Fig3.jpeg", width=170, height=150, units="mm", res=1000)

par(mfrow = c(4, 2))
par(cex = 0.6)
par(mar = c(3, 0, 0, 0), oma = c(4, 4, 0.5, 0.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

#making the GDD plots for different salinities

gdd.plot2(plot.tables.time[[7]], supply_xlab = "true", supply_ylab = "false", xlab.i = "", ylab.i = "Nothing", y.per = 1.09, x.per = 0.025, axes.i = "standardized")
axis(1, cex.axis = axis_number_size)
axis(2, cex.axis = axis_number_size)
# text(x = 0, y = 185, "(a)", cex = 1.5)
text(x = 2, y = 185, "20 ppt", cex = 1.75)

gdd.plot2(plot.tables.gdd[[7]], supply_xlab = "true", supply_ylab = "false", xlab.i = "", ylab.i = "Nothing", y.per = 0.99, x.per = 0.025, legend.present = "no", axes.i = "standardized")
axis(1, at = seq(0, 250, 50), cex.axis = axis_number_size)
mtext("Shell Width (um)", side = 2, outer = TRUE, cex = axis_font_size, line = 2.2, adj = 0.99)#0.325
# text(x = 0, y = 185, "(b)", cex = 1.5)

gdd.plot2(plot.tables.time[[8]], supply_xlab = "true", supply_ylab = "false", xlab.i = "", ylab.i = "Nothing", y.per = 1, x.per = 0.025, axes.i = "standardized")
axis(1, cex.axis = axis_number_size)
axis(2, cex.axis = axis_number_size)
# text(x = 0, y = 185, "(c)", cex = 1.5)
text(x = 2, y = 185, "25 ppt", cex = 1.75)


gdd.plot2(plot.tables.gdd[[8]], supply_xlab = "true", supply_ylab = "false", xlab.i = "", ylab.i = "Nothing", y.per = 0.99, x.per = 0.025, legend.present = "no", axes.i = "standardized")
axis(1, at = seq(0, 250, 50))
mtext("Shell Width (um)", side = 2, outer = TRUE, cex = axis_font_size, line = 2.2, adj = 0.7)#0.325
# text(x = 0, y = 185, "(d)", cex = 1.5)



gdd.plot2(plot.tables.time[[9]], supply_xlab = "true", supply_ylab = "false", xlab.i = "", ylab.i = "Nothing", y.per = 1.03, x.per = 0.025, axes.i = "standardized")
axis(1, cex.axis = axis_number_size)
axis(2, cex.axis = axis_number_size)
# text(x = 0, y = 185, "(e)", cex = 1.5)
text(x = 2, y = 185, "30 ppt", cex = 1.75)


gdd.plot2(plot.tables.gdd[[9]], supply_xlab = "true", supply_ylab = "false", xlab.i = "", ylab.i = "Nothing", y.per = 0.99, x.per = 0.025, legend.present = "no", axes.i = "standardized")
axis(1, at = seq(0, 250, 50), cex.axis = axis_number_size)
mtext("Shell Width (um)", side = 2, outer = TRUE, cex = axis_font_size, line = 2.2, adj = 0.40)#0.325
# text(x = 0, y = 185, "(f)", cex = 1.5)


###
gdd.plot2(plot.tables.time[[10]], supply_xlab = "true", supply_ylab = "false", xlab.i = "Time (days)", ylab.i = "Nothing", y.per = 1.2, x.per = 0.025, axes.i = "standardized")
axis(1, cex.axis = axis_number_size)
axis(2, cex.axis = axis_number_size)
# text(x = 0, y = 185, "(g)", cex = 1.5)
text(x = 2, y = 185, "35 ppt", cex = 1.75)


gdd.plot2(plot.tables.gdd[[10]], supply_xlab = "true", supply_ylab = "false", xlab.i = expression(paste("Growing Degree Days (", degree, "C*day)")), ylab.i = "Nothing", y.per = 0.99, x.per = 0.025, legend.present = "no", axes.i = "standardized")
axis(1, at = seq(0, 250, 50), cex.axis = axis_number_size)
mtext("Shell Width (um)", side = 2, outer = TRUE, cex = axis_font_size, line = 2.2, adj = 0.065)#0.325
# text(x = 0, y = 185, "(h)", cex = 1.5)



dev.off()

#Figure 4 development
jpeg("Broell et al_MEPS_201611016_Rev2_Fig4.jpeg", width=170, height=90, units="mm", res=1000)


par(mfrow = c(1, 2))
par(cex = 0.6)
par(mar = c(3, 0, 0, 0), oma = c(4, 4, 0.5, 0.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))


gdd.plot2(plot.tables.time[[4]], supply_xlab = "true", supply_ylab = "false", xlab.i = "Time (days)", ylab.i = "Nothing", y.per = 0.99, x.per = 0.025)
axis(1, cex.axis = axis_number_size)
axis(2, cex.axis = axis_number_size)
text(x = 3, y = 6.1, "(a)", cex = 1.75)

gdd.plot2(plot.tables.gdd[[4]], supply_xlab = "true", supply_ylab = "false", xlab.i = expression(paste("Growing Degree Days (", degree, "C*day)")), ylab.i = "Nothing", y.per = 0.99, x.per = 0.025, legend.present = "no")
axis(1, at = seq(0, 450, 50), cex.axis = axis_number_size)
mtext("Developmental Stage", side = 2, outer = TRUE, cex = axis_font_size, line = 2.2, adj = 0.5)#0.325
text(x = 30, y = 6.1, "(b)", cex = 1.75)

dev.off()



