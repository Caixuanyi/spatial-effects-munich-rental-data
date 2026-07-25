
##define M0(Baseline Model)
M0_k20 <- gam(
  nmqm ~
    MSP_f +
    s(wfl.gekappt, k = 20) +
    s(age, k = 20) ,
  family = Gamma(link = "log"),
  method = "ML",
  data = rent_data
)


##define M1
M1_k60 <- gam(
  nmqm ~
    MSP_f +
    s(wfl.gekappt, k = 20) +
    s(age, k = 20) +
    s(centroid_x, centroid_y, k = 60),
  family = Gamma(link = "log"),
  method = "ML",
  data = rent_data
)



#proove spatial effekt
##check p value
summary(M1_k60)

##anova
anova(M0_k20, M1_k60, test = "F")

##aic
AIC(M0_k20, M1_k60)




# temporal interaction model
##unordered
M2_k60 <- gam(
  nmqm ~
    MSP_f +
    s(wfl.gekappt, k = 20) +
    s(age, k = 20) +
    s(centroid_x, centroid_y, by = MSP_f, k = 60),
  family = Gamma(link = "log"),
  method = "ML",
  data = rent_data
)

##ordered
M3_k60 <- gam(
  nmqm ~
    MSP_f +
    s(wfl.gekappt, k = 20) +
    s(age, k = 20) +
    s(centroid_x, centroid_y, k = 60) +
    s(centroid_x, centroid_y, by = MSP_f_ordered, k = 20),
  family = Gamma(link = "log"),
  method = "ML",
  data = rent_data
)



#4.4
##test for Temporal Stability
###basic p
summary(M3_k60)
###Anova
anova(M1_k60, M3_k60, test = "F")
###Aic
AIC(M1_k60, M3_k60)

