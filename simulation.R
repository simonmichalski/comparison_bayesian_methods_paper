
num_trials = 128
n = 40
k = .5
beta = .3

ss = 20
ss_ratio <- c(1.01, 1.02, 1.05, 1.10, 1.15, 1.25, 1.35, 1.45, 
              1.65, 1.85, 2.05, 2.25, 2.65, 3.05, 3.45, 3.85)
delay <- c(1, 3, 5, 8, 14, 30, 60, 120)


compute_sv <- function(ss, ss_ratio, k, delay){
  a = ss*sample(ss_ratio,1)
  k = k
  D = sample(delay,1)
  sv = a/(1+k*D)
  return(sv)
}

softmax <- function(sv, ss, beta){
  p_ll <- exp(sv/beta)/(exp(sv/beta)+exp(ss/beta))
  return(p_ll)
}

sv <- compute_sv(ss, ss_ratio, k, delay)
p_ll <- softmax(sv, ss, beta)




'for (i in n){
  
  for (j in num_trials){
    sv_ll <- compute_sv()
    p_ll <- softmax()
  }
}'