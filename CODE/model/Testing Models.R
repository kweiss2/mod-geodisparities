# Testing models again
source("Prep Data.R")

# Formulas
f1 <- ptb ~ year_c + black + f(ID, model = 'iid')
f2 <- ptb ~ year_c + black +
  f(ID, model = 'bym',
    graph = '../../data/spatial/national_knn6.adj',
    hyper = list(prec.unstruct = list(prior = 'loggamma', param = c(1,0.001)),
                 prec.spatial = list(prior = 'loggamma', param = c(1,0.001))),
    scale.model = T)
f3 <- ptb ~ year_c + black +
  f(ID, model = 'bym',
    graph = '../../data/spatial/national_knn6.adj',
    hyper = list(prec.unstruct = list(prior = 'loggamma', param = c(1,0.001)),
                 prec.spatial = list(prior = 'loggamma', param = c(1,0.001))),
    scale.model = T) +
  f(ID2, black, model = 'iid')
f4 <- ptb ~ year_c + black +
  f(ID, model = 'bym',
    graph = '../../data/spatial/national_knn6.adj',
    hyper = list(prec.unstruct = list(prior = 'loggamma', param = c(1,0.001)),
                 prec.spatial = list(prior = 'loggamma', param = c(1,0.001))),
    scale.model = T) +
  f(ID2, black, model = 'bym',
    graph = '../../data/spatial/national_knn6.adj',
    hyper = list(prec.unstruct = list(prior = 'loggamma', param = c(1,0.001)),
                 prec.spatial = list(prior = 'loggamma', param = c(1,0.001))),
    scale.model = T)

# Models -------------------------------------------------------
m1 <- inla(f1, family = 'poisson',
           data = national_sf,
           offset = log(births),
           control.predictor = list(link = 1, 
                                    compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE,
                                  waic = TRUE,
                                  config = T))
summary(m1)


m2 <- inla(f2, family = 'poisson',
           data = national_sf, 
           offset = log(births),
           control.predictor = list(link = 1, compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE, 
                                  waic = TRUE,
                                  config = T))
summary(m2)

m3 <- inla(f3, family = 'poisson',
           data = national_sf,
           offset = log(births),
           control.predictor = list(link = 1, compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE, 
                                  waic = TRUE,
                                  config = T))
summary(m3)

m4 <- inla(f4, family = 'poisson',
           data = national_sf,
           offset = log(births),
           control.predictor = list(link = 1, compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE, 
                                  waic = TRUE,
                                  config = T))
summary(m4)

m5 <- inla(f1, family = 'nbinomial',
           data = national_sf,
           offset = log(births),
           control.predictor = list(link = 1, 
                                    compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE,
                                  waic = TRUE,
                                  config = T))
summary(m1)


m6 <- inla(f2, family = 'nbinomial',
           data = national_sf, 
           offset = log(births),
           control.predictor = list(link = 1, compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE, 
                                  waic = TRUE,
                                  config = T))
summary(m2)

m7 <- inla(f3, family = 'nbinomial',
           data = national_sf,
           offset = log(births),
           control.predictor = list(link = 1, compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE, 
                                  waic = TRUE,
                                  config = T))
summary(m3)

m8 <- inla(f4, family = 'nbinomial',
           data = national_sf,
           offset = log(births),
           control.predictor = list(link = 1, compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE, 
                                  waic = TRUE,
                                  config = T))
summary(m4)


# Compare models -------------------------------------------------------
df <- rbind(cbind(m1$dic$dic, m1$waic$waic, sum(log(m1$cpo$cpo))),
            cbind(m2$dic$dic, m2$waic$waic, sum(log(m2$cpo$cpo))),
            cbind(m3$dic$dic, m3$waic$waic, sum(log(m3$cpo$cpo))),
            cbind(m4$dic$dic, m4$waic$waic, sum(log(m4$cpo$cpo))))
colnames(df) <- c("DIC", "WAIC", "Sum(log(CPO))")
rownames(df) <- c("M1: Poisson iid intercept", 
                  "M2: Poisson bym intercept", 
                  "M3: Poisson bym intercept, iid race slope", 
                  "M4: Poisson bym intercept, bym race slope")
print(df)


df2 <- rbind(cbind(m1$dic$dic, m1$waic$waic, sum(log(m1$cpo$cpo))),
            cbind(m2$dic$dic, m2$waic$waic, sum(log(m2$cpo$cpo))),
            cbind(m3$dic$dic, m3$waic$waic, sum(log(m3$cpo$cpo))),
            cbind(m4$dic$dic, m4$waic$waic, sum(log(m4$cpo$cpo))),
            cbind(m5$dic$dic, m5$waic$waic, sum(log(m5$cpo$cpo))),
            cbind(m6$dic$dic, m6$waic$waic, sum(log(m6$cpo$cpo))),
            cbind(m7$dic$dic, m7$waic$waic, sum(log(m7$cpo$cpo))), 
            cbind(m8$dic$dic, m8$waic$waic, sum(log(m8$cpo$cpo))))
colnames(df2) <- c("DIC", "WAIC", "Sum(log(CPO))")
rownames(df2) <- c("M1: Poisson iid intercept", 
                  "M2: Poisson bym intercept", 
                  "M3: Poisson bym intercept, iid race slope", 
                  "M4: Poisson bym intercept, bym race slope",
                  "M5: Neg Bin iid intercept", 
                  "M6: Neg Bin bym intercept", 
                  "M7: Neg Bin bym intercept, iid race slope", 
                  "M8: Neg Bin bym intercept, bym race slope")
print(df2)
