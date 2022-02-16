modelCode <- nimbleCode({
  
  # likelihood
  for (h_ in 1:nTargets){ # targets
    for (k_ in 1:nProxies){  # proxies
      for (j_ in 1:nFractions){  #  fractions
        for (i_ in 1:nSources){  #  sources
          source.component.value_[h_,i_,j_,k_] <- alpha_[h_,i_] *C_[i_,j_] * 
                                                  (I_[i_,j_,k_] + T_[k_] )
          source.component.weight_[h_,i_,j_,k_] <- alpha_[h_,i_] *C_[i_,j_]
        }
        component.contrib_[h_, j_, k_] <- W_[j_,k_] * 
                                          sum(source.component.value_[h_,1:nSources,j_,k_]) /
                                          sum(source.component.weight_[h_,1:nSources,j_,k_])
      }
      H_[h_,k_] <- (sum(component.contrib_[h_,1:nFractions,k_]) / sum(W_[1:nFractions,k_]))
      # Measurements on targets
      obsvn[h_,k_] ~ dnorm(H_[h_,k_], pow(obsvnError[h_,k_],-2))
    }	
  }
  
  #contribution of components
  for (h_ in 1:nTargets){ # targets	
    for (j_ in 1:nFractions){  #  fractions
      for (i_ in 1:nSources){  #  sources	
        aux3_[h_,i_,j_] <- alpha_[h_,i_]*C_[i_,j_]
      }
      aux4_[h_, j_] <- sum(aux3_[h_,1:nSources,j_])
      beta_[h_, j_] <- aux4_[h_, j_] / sum(aux4_[h_,1:nFractions])
    }
  }
  
  #contribution of a source towards a proxy signal
  for (h_ in 1:nTargets){ # targets	
    for (i_ in 1:nSources){  #  sources
      for (j_ in 1:nFractions){  #  fractions
        aux5_[h_,i_,j_] <- alpha_[h_,i_]*C_[i_,j_]
        aux6_[h_,i_,j_] <- aux5_[h_,i_,j_]/sum(aux5_[h_,1:nSources,j_])
      }
    }
    
    for (k_ in 1:nProxies){ # proxies
      for (i_ in 1:nSources){  #  sources
        for (j_ in 1:nFractions){  #  fractions
          aux7_[h_,i_,k_,j_] <- aux6_[h_,i_,j_]*W_[j_,k_]
        }
        aux8_[h_,k_, i_] <- sum(aux7_[h_,i_,k_,1:nFractions])
        theta_[h_,k_, i_] <- aux8_[h_,k_, i_] / sum(aux8_[h_,k_,1:nSources])
      }
    }
  }
###########################################################################
  # priors
  #prior for intake estimates
  for (h_ in 1:nTargets){ # nTargets: number of individuals
    alpha_[h_,1:nSources] ~ ddirch(sourceDirichPrior[1:nSources]) 
  }

  # # prior for isotopic values of fractions in sources
  for (k_ in 1:nProxies){  # proxies
    for (j_ in 1:nFractions){  #  fractions
      for (i_ in 1:nSources){  #  sources
        I_[i_,j_,k_] ~ dnorm(source[i_,j_,k_], pow(sourceUncert[i_,j_,k_],-2))
      }
    }
  }

  # prior for offset
  for (k_ in 1:nProxies){  # proxies measured per target
    T_[k_] ~ dnorm(weightOffset[k_], pow(weightOffsetUncert[k_],-2))
  }
  
  # prior for concentration values of fractions in sources
  for (j_ in 1:nFractions){  # components of food group
    for (i_ in 1:nSources){  # food groups
      C_[i_,j_] ~ T(dnorm(concentration[i_,j_], pow(concentrationUncert[i_,j_],-2)),0,100)
    }
  }
  
  # prior for  weight contribution
  for (k_ in 1:nProxies){  # proxies measured per target
    for (j_ in 1:nFractions){  # fractions
      W_[j_,k_] ~ T(dnorm(weights[j_,k_], pow(weightsUncert[j_,k_],-2)),0,100)
    }
  }
############################################################################
#user defined priors
pc1 ~ dnorm(0, 0.05)
prior1 ~ dconstraint(alpha_[1:5,3] < 0.2 + pc1)
prior2 ~ dconstraint(alpha_[1,2] + alpha_[1,1] > 0.26)
prior3 ~ dconstraint(alpha_[1,2] + alpha_[1,1] < 0.42)
prior4 ~ dconstraint(alpha_[1,4] + alpha_[1,5] > 0.42)
prior5 ~ dconstraint(alpha_[1,4] + alpha_[1,5] < 0.58)

})