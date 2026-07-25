library(mgcv)
##default k
##define M0(Baseline Model)
M0 <- gam(
  nmqm ~
    MSP_f +
    s(wfl.gekappt) +
    s(age) ,
  family = Gamma(link = "log"),
  method = "ML",
  data = rent_data
)
data.frame(
  smooth = sapply(M0$smooth, function(x) x$label),
  k = sapply(M0$smooth, function(x) x$bs.dim)
)

##define M1
M1 <- gam(
  nmqm ~
    MSP_f +
    s(wfl.gekappt) +
    s(age) +
    s(centroid_x, centroid_y),
  family = Gamma(link = "log"),
  method = "ML",
  data = rent_data
)
data.frame(
  smooth = sapply(M1$smooth, function(x) x$label),
  k = sapply(M1$smooth, function(x) x$bs.dim)
)


#proove spatial effekt
##check p value
summary(M1)

##anova
anova(M0, M1, test = "F")

##aic
AIC(M0, M1)




# temporal interaction model
##unordered
M2 <- gam(
  nmqm ~
    MSP_f +
    s(wfl.gekappt) +
    s(age) +
    s(centroid_x, centroid_y, by = MSP_f),
  family = Gamma(link = "log"),
  method = "ML",
  data = rent_data
)

##ordered
M3 <- gam(
  nmqm ~
    MSP_f +
    s(wfl.gekappt) +
    s(age) +
    s(centroid_x, centroid_y) +
    s(centroid_x, centroid_y, by = MSP_f_ordered),
  family = Gamma(link = "log"),
  method = "ML",
  data = rent_data
)



#4.4
##test for Temporal Stability
###basic p
summary(M3)
###Anova
anova(M1, M3, test = "F")
###Aic
AIC(M1, M3)
