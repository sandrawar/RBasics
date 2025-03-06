# zadanie 7
mi = 5;
sigma = 2;
alpha = 0.05;
N = 10000;
n = 10;
ile_wpada = replicate(N,{x = rnorm(n,mi,sigma)
                        przedz = t.test(x, conf.level = 1 - alpha)$conf
                        mi>=przedz[1] & mi<=przedz[2]
                        })
ile_wpada[1:20]
sum(ile_wpada)/N

# zadanie 8
mi = 5;
sigma = 2;
alpha = 0.05;
N = 50;
n = 10;
wyniki = replicate(N,{x = rnorm(n,mi,sigma)
         t.test(x, conf.level = 1 - alpha)$conf
         })
dim(wyniki)
matplot(wyniki, rbind(1:N, 1:N), type='l', lty=1, col=c("grey", "blue"), 
        las=1, xlab="granice przedziału", ylab="nr przedziału")
abline(v=mi, col='red')

