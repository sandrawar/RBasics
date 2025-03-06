# zadanie 1
  # a
  prop.test(x = c(40,31), n = c(233,220), alt="greater")
  # bi
  power.prop.test(p1=0.17, p2=0.14, n= c(233,220), sig.level = 0.05, alt="one.sided")$power
  # bii
  power.prop.test(p1=0.17, p2=0.14, sig.level = 0.05, alt="one.sided", power = 0.8)$n
  
# zadanie 2
  getA = c(26.4, 22.5, 24.9, 23.7, 21.5)
  getB = c(25.1, 29.0, 23.4, 27.6, 22.3)
  var.test(getA, getB)  # p-value > alpha - nie odrzucamy, alpha = 0.1, traktujemy je jaknby mialy ten sam rozklad
  # a
  t.test(getA, getB, alt="less",var.equal = T) # nie ma podstaw do odrzucenia hipotezy zerowej H0 (mu1 = mu2) (H1 - mu1 < mu2) 
  # b
  sdf = ((5 - 1)*var(getA) + (5 - 1)*var(getB))/(5 + 5 - 2)
  1 - power.t.test(n = 5, alt="one.sided", type="two.sample", delta=2, sd=sqrt(sdf), sig.level = 0.05)$power
  # c
  power.t.test(power=0.75, alt="one.sided", type="two.sample", delta=2, sd=sqrt(sdf), sig.level = 0.05)$n

# zadanie 3
  przed = hemoglobina$przed
  po = hemoglobina$po
  # a
  t.test(przed, po, alt="greater", paired=T)
  # b
  sdf = sd(po - przed) 
  power.t.test(alt="one.sided", delta=1.5, sd = sdf, sig.level = 0.05, n = length(przed))$power

# zadanie 4
  library(MASS)
  x = nlschools$IQ[nlschools$SES > median(nlschools$SES)]
  y = nlschools$IQ[nlschools$SES <= median(nlschools$SES)]
  # a
  u = (mean(x) - mean(y))/sqrt(var(x)/length(x) + var(y)/length(y)) # U
  1 - pnorm(u) # pv, pnorm(u, lower.tail=F) odrzucamy H0 na rzecz H1
  
  