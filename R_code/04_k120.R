
## M1
M1_k120 <- gam(
  nmqm ~
    MSP_f +
    s(wfl.gekappt, k = 20) +
    s(age, k = 20) +
    s(centroid_x, centroid_y, k = 120),
  family = Gamma(link = "log"),
  method = "ML",
  data = rent_data
)

##significant test
summary(M1_k120)
##anova
anova(M0_k20, M1_k120, test = "F")
##aic
AIC(M0_k20, M1_k120)

## M2
M2_k120 <- gam(
  nmqm ~
    MSP_f +
    s(wfl.gekappt, k = 20) +
    s(age, k = 20) +
    s(
      centroid_x,
      centroid_y,
      by = MSP_f,
      k = 120
    ),
  family = Gamma(link = "log"),
  method = "ML",
  data = rent_data
)



## M3:
M3_k120_diff20 <- gam(
  nmqm ~
    MSP_f +
    s(wfl.gekappt, k = 20) +
    s(age, k = 20) +
    s(
      centroid_x,
      centroid_y,
      k = 120
    ) +
    s(
      centroid_x,
      centroid_y,
      by = MSP_f_ordered,
      k = 20
    ),
  family = Gamma(link = "log"),
  method = "ML",
  data = rent_data
)

##significant
summary(M3_k120_diff20)
##anova
anova(M1_k120, M3_k120_diff20, test = "F")

AIC(M1_k120, M3_k120_diff20)
