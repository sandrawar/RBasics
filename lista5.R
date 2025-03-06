# zadanie 1
  licznosci = c(380, 340, 380, 500) # prob = c(1/4, 1/4, 1/4, 1/4)
  chisq.test(licznosci) # odrzucamy H0

# zadanie 2
  licznosci = c(36, 42, 14, 8)
  prob = c(0.4, 0.4, 0.1, 0.1)
  chisq.test(x=licznosci, p=prob)

# zadanie 3
  licznosci = c(24, 73, 77, 26)
  prob = dbinom(0:3, size=3, p=0.5)
  chisq.test(x=licznosci, p=prob)
  
# zadanie 4  
  licznosci = c(140, 280, 235, 200, 100, 45)
  dane = rep(0:5, licznosci)
  (est = fitdistr(dane, "Poisson")$est)
  (prob = c(dpois(0:4, est), ppois(4, est, lower.tail = F)))
  chisq.test(x=licznosci, p=prob)  # T~X^2(k - 1 - r), k=6, r=1
  pchisq(9.7363, 4, lower.tail = F)
  
# zadanie 5
  ks.test(infolinia$czas, "pgamma", shape=4.5, rate=4) # ks test hipoteza prosta (z parametrami)
  qqnorm(infolinia$czas)
  qqline(infolinia$czas)
  # a'la zadanie 6
  shapiro.test(infolinia$czas)
  