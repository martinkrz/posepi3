# beta for SEIRS (default) and anything else (e.g. SIR) 
get_beta = function(p) {
  if(p$sigma > 0 | p$mu > 0) {
    f = (p$gamma + p$mu + p$alpha) * (p$sigma + p$mu) / p$sigma
  } else {
    f = p$gamma
  }
  beta = p$R0 * f
  return(beta)
}

sample_params = function(params,vars,fns) {
  out = params
  for(i in 1:length(vars)) {
    var   = vars[[i]]
    fn    = fns[[i]]    
    out[[var]] = fn(get(var,params))
  }
  return(out)
}

# inputs:
# 1/omega, 1/mu : years
# p : /year
#
# outputs:
# rates: /day
# time: day
get_params = function(R0=R0_default,
                      ip=ip_default,
                      lp=lp_default,
                      id=idshort_default,
                      le=le_default,
                      al=al_default,
                      p=0,
                      tmax=tmax_default) {
  id  = 365*id
  le  = 365*le
  p   = p/365
  out = list(R0    = R0,
             gamma = ifelse(ip,1/ip,0),
             sigma = ifelse(lp,1/lp,0),
             omega = ifelse(id,1/id,0),
             mu    = ifelse(le,1/le,0),
             alpha = ifelse(al,1/al,0),
             tmax  = 365*tmax,
             p     = p,            
             ip    = ip,
             lp    = lp,
             id    = id,
             le    = le,
             N     = 1
             )
  out$pc    = 1-1/out$R0
  out$beta  = get_beta(out)
  #out$A    = (out$omega+out$mu+out$gamma)/( (out$omega+out$mu) * (out$beta-out$gamma-out$mu) )
  #out$cfr  = out$alpha/(out$gamma+out$mu+out$alpha)
  #out$stars = stars(out)
  #out$period = ieperiod(out)
  #print(out)
  return(out)
}

############################################################################
## SEIRS
seirs = function(params, steps=sir_system_steps) {  
  time     = seq(0,params$tmax,length.out=steps)  
  start = c(S = params$N - sir_init_i, 
            E = 0, 
            I = sir_init_i, 
            R = 0,
            B = sir_init_i)
  out   = ode(y     = start, 
              times = time, 
              func  = seirs_system, 
              parms = unlist(params))
  out   = cbind(out,data.frame(R0=params$R0,beta=params$beta,id=params$id,p=params$p))
  return(as.data.frame(out))
}

# B is cumulative disease burden (total number of cases from start of epidemic)
seirs_system=function(t,x,params) {
  S=x[1]
  E=x[2]
  I=x[3]
  R=x[4]
  B=x[5]
  mu   = params["mu"]
  beta = params["beta"]
  omega= params["omega"]
  sigma= params["sigma"]
  gamma= params["gamma"]
  alpha= params["alpha"]
  p    = params["p"]
  dS  = -beta*S*I + omega*R  + mu       - mu*S         - p*S
  dE  = -sigma*E  + beta*S*I            - mu*E
  dI  = -gamma*I  + sigma*E             - (mu+alpha)*I
  dR  = -omega*R  + gamma*I             - mu*R         + p*S
  dB  =             beta*S*I
  #      ^          ^          ^          ^              ^
  #      outflow    inflow     birth      death          vaccination
  res = c(dS, dE, dI, dR, dB)
  list(res)
}

stars = function(params) {
  Sstar = 1/params$R0
  Istar = params$mu*(1-Sstar)/(params$beta*Sstar - (params$omega*params$gamma/(params$mu+params$omega)))
  Estar = (params$mu+params$gamma)*Istar/params$sigma
  Rstar = params$gamma*Istar/(params$mu+params$omega)
  return(list(S=Sstar,E=Estar,I=Istar,R=Rstar))
}

ieperiod = function(params) {
  stars = stars(params)
  star  = c(stars(params),params)
  names(star)[1:4]=c("S", "E", "I", "R")
  fns=list(
           quote(mu*(1-p-S) - beta*S*I + omega*R    ), 
           quote(beta*S*I   - (mu+sigma)*E          ), 
           quote(sigma*E    - (mu+alpha+gamma)*I    ), 
           quote(gamma*I    - mu*R - omega*R + p*mu )
           )
  aa=as.vector(c(sapply(fns, D, "S"),
                 sapply(fns, D, "E"),
                 sapply(fns, D, "I"),
                 sapply(fns, D, "R")))  
  JJ=matrix(sapply(aa,eval,star),ncol=4)  
  EE=eigen(JJ)$values
  WW=which.max(Im(EE))  
  rp=2*pi/Im(EE[WW])
  return(rp)
}

# https://stats.stackexchange.com/questions/22974/how-to-find-local-peaks-valleys-in-a-series-of-data
find_peaks = function(out,m=1){
  t = out$time
  x = out$I
  shape = diff(sign(diff(x, na.pad = FALSE)))
  pks = sapply(which(shape < 0), FUN = function(i){
    z = i - m + 1
    z = ifelse(z > 0, z, 1)
    w = i + m + 1
    w = ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks = unlist(pks)
  return(data.frame(time=t[pks],
                    y=x[pks],
                    i=1:length(pks),
                    beta=out$beta[1],
                    R0=out$R0[1],
                    id=out$id[1],
                    p=out$p[1]))
}
