# fix R CMD check for returnType
returnType <- function(...) NULL

defineRegisterInflBetaDist <- function(){

dZOBE <- nimbleFunction(
  run = function(x = double(), alpha = double(), beta = double(), zeroProb = double(),
                 oneProb = double(), log = integer(0, default = 0)) {
    returnType(double())
    if(x < 0){
      if(log == 1) return(-Inf)
      return(0)
    }
    if(x > 1){
      if(log == 1) return(-Inf)
      return(0)
    }
    ## First handle non-zero data
    if(x > 1E-4 & x < (1-1E-4)) {
      ## return the log probability if log = TRUE
      if(log == 1) return(dbeta(x, alpha, beta, log = TRUE) + log(1-zeroProb) + log(1-oneProb))
      ## or the probability if log = FALSE
      else return((1-zeroProb-oneProb) * dbeta(x, alpha, beta, log = FALSE))
    }
    if(x <= 1E-4 & x >= 0) {
    if(log == 1) return(log(zeroProb))
    return(zeroProb)
    }
    if(x >= (1 - 1E-4) & x <= 1) {
    if(log == 1) return(log(oneProb))
    return(oneProb)
    }
}, check = FALSE, where = .GlobalEnv)

rZOBE <- nimbleFunction(
  run = function(n = integer(), alpha = double(), beta = double(), zeroProb = double(),
                 oneProb = double()) {
    returnType(double())
    return1 <- rmulti(1, size = 1, prob = c(zeroProb, 1-zeroProb - oneProb, oneProb))
    if(return1[1] == 1) return(0)
    if(return1[2] == 1) return(rbeta(1, alpha, beta))
    if(return1[3] == 1) return(1)
}, check = FALSE, where = .GlobalEnv)

pZOBE <- nimbleFunction(
  run = function(q = double(), alpha = double(), beta = double(), zeroProb = double(),
                 oneProb = double(), lower.tail = TRUE, log.p = FALSE) {
    
    returnType(double())
    prob <- pbeta(q, alpha, beta) + zeroProb - oneProb
    if(lower.tail == TRUE){
      prob <- 1 - prob
    }
    if(log.p == TRUE){
      prob <- log(prob)
    }
      return(prob)
  }, check = FALSE, where = .GlobalEnv)

qZOBE <- nimbleFunction(
  run = function(p = double(), alpha = double(), beta = double(), zeroProb = double(),
                 oneProb = double(), lower.tail = TRUE, log.p = FALSE) {
    returnType(double())
    if(log.p == TRUE){
      p <- exp(p)
    }
    if(lower.tail == TRUE){
      p <- 1 - p
    }
    q <- qbeta(p, alpha, beta)
    if(p <= zeroProb){
      q <- 0
    }
    if(p > qbeta(p, alpha, beta) + zeroProb){
      q <- 1
    }
    ## First handle non-zero data
    return(qbeta(p, alpha, beta))
  }, check = FALSE, where = .GlobalEnv)


registerDistributions(list(
  dZOBE = list(
    BUGSdist = "dZOBE(alpha, beta, zeroProb, oneProb)",
    discrete = FALSE,
    range = c(0, 1),
    pqAvail = F,
    types = c('value = double()', 'alpha = double()', 'beta = double()', 'zeroProb = double()', 'oneProb = double()')
  ))
)
assign("dZOBE", dZOBE, envir = .GlobalEnv)
assign("rZOBE", rZOBE, envir = .GlobalEnv)
# assign("pZOBE", pZOBE, envir = .GlobalEnv)
# assign("qZOBE", qZOBE, envir = .GlobalEnv)

}
