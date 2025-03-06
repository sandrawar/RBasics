# zadanie 1
dane = c(5.21, 5.15, 5.20, 5.48, 5.19, 5.25, 5.09, 5.17, 4.94, 5.11)
  # a
  t.test(dane, conf.level = 0.95)$conf
  # b
  t.test(dane, mu=5.15) # pv > alpha - nia ma podstaw do odrzucenie H0
  # c
  t.test(dane, mu = 5.20, alt="less") # nie ma podstaw do odrzucenia H0
  # d
  power.t.test(n = length(dane), delta = 0.05, sd = sd(dane), type = "one.sample", 
               alt = "one.sided", sig.level = 0.05)$power
  # e
  power.t.test(n = length(dane), power = 0.8, sd = sd(dane), type = "one.sample", 
               alt = "one.side", sig.level = 0.05)$delta
  # f
  power.t.test(power = 0.8, delta = 0.05, sd = sd(dane), type = "one.sample", 
               alt = "one.side", sig.level = 0.05)$n
  # g
  library(TeachingDemos)
  sigma.test(dane, conf.level = 0.95)$conf
  # h  
  sqrt(sigma.test(dane, conf.level = 0.95)$conf)
  # i
  sigma.test(dane, sigma=0.2) #(, sigmasq = 0.04)

  # zadanie 2  
    # a
    t.test(goats$WeightInitial, conf.level = 0.95)$conf
    # b
    t.test(goats$WeightInitial, mu=23.00, alt="greater")
    # c
    1 - power.t.test(n = 40, delta = 1, sd = sd(goats$WeightInitial), 
                     alt = "one.sided", type = "one.sample")$power
    # d
    power.t.test(power = 0.8, delta = 1, sd = sd(goats$WeightInitial), 
                     alt = "one.sided", type = "one.sample")$n
    # e
    library(TeachingDemos)
    sigma.test(goats$WeightInitial, conf.level = 0.90)$conf
    # f
    sigma.test(goats$WeightInitial, sigmasq=20)
    # g
    sigma.test(goats$WeightInitial, sigma = 3, alt = "greater")
    
# zadanie 3  
    # a
    prop.test(x=128, n=400, alt = "less", p=0.35)
    # b
    prop.test(x=128, n=400, p=0.35, conf.level = 0.95)$conf
    # c
    binom.test(x=3, n=10, alt="less", p=0.35)
    # d
    binom.test(x=3, n=10, p=0.35, conf.level = 0.95)$conf
    