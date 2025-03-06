confidence_interval <- function(data, confidence_level) {
  # WYJ: przedział ufności dla średniej na zadanym poziomie ufności
  
  # Sprawdzenie liczności próby
  n = length(data)
  if (n < 100) {
    stop("Minimalna liczność próby to 100")
  }
  
  # Korzystamy z modelu 3 (dla dowolnego rozkładu, skończonej wariancji i dużej próbie)
  alpha = 1 - confidence_level
  interval = mean(data) + c(-1,1) * qnorm(1 - alpha / 2) * (sd(data) / sqrt(n))
  
  return(interval)
}
