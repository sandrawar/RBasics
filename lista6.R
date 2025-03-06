# zadanie 1
  # sprawdzamy założenia
  srednie = tapply(iris$Sepal.Width, iris$Species, mean)
  boxplot(y~a)
  lines(1:3, srednie, pch=20, type="p", cex=2)
  par(mfrow=c(1,3))  
  tapply(iris$Sepal.Width, iris$Species, function(x){qqnorm(x); qqline(x)})  
  tapply(iris$Sepal.Width, iris$Species, function(x){shapiro.test(x)$p.value})  
  simplify2array(tapply(iris$Sepal.Width, iris$Species, \(x)shapiro.test(x)[1:2]))  
  bartlett.test(iris$Sepal.Width, iris$Species)  
  library(car)  
  leveneTest(iris$Sepal.Width, iris$Species, center=mean)  
  # zmienna czynnikowa?
  class(iris$Species)
  is.factor(iris$Species)
  a = as.factor(iris$Species)  
  
  model = lm(iris$Sepal.Width~iris$Species)
  summary(model)
  anova(model)  
  1 - pf(49.16, 2, 147)  
  pairwise.t.test(iris$Sepal.Width, iris$Species, p.adjust="bonf")  
  (tukey = TukeyHSD(aov(model)))
  plot(tukey) # przedziały ufności nie zawierają 0
  