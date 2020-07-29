
# Panel 1: trajectories for a range of R0 and id 
calculate1 = function(params1a,params1b,params1c,params1d) { # R0,ip,lp,id,le,alpha,p) {
  #print(params1)
  seirs1 = seirs(params1a,steps=sir_system_steps) 
  seirs2 = seirs(params1b,steps=sir_system_steps) 
  seirs3 = seirs(params1c,steps=sir_system_steps) 
  seirs4 = seirs(params1d,steps=sir_system_steps) 
  #sir_tmax = sir_t_bound(params,tol=sir_init_i/2,tmax=1000) # beta=p$R0*p$gamma,gamma=p$gamma,vac=p$p,tol=sir_init_i/2,tmax=1000)
  #sir      = sir(params,tmax=sir_tmax) #beta=p$R0*p$gamma,gamma=p$gamma,vac=p$p,tmax=sir_tmax)
  return(list(seirs1,seirs2,seirs3,seirs4))
}

# Panel 2: phase plane
calculate2 = function(params2a,params2b,params2c,params2d) {
  R0_max = 4 + (params2a$R0 > 4)
  out    = data.frame()
  params2a0   = params2a
  params2a0$p = 0
  params2c0   = params2c
  params2c0$p = 0
  pmin  = min(params2a$p,params2b$p,params2c$p,params2d$p)
  pmax  = max(params2a$p,params2b$p,params2c$p,params2d$p)
  idmin = min(params2a$id,params2b$id,params2c$id,params2d$id)
  idmax = max(params2a$id,params2b$id,params2c$id,params2d$id)
  models  = c("M1","M1","M1","M2","M2","M2")
  plevel  = c("p1","p3","p2","p1","p3","p2")
  i       = 1
  for(p in list(params2a0,params2a,params2b,params2c0,params2c,params2d)) {
    params      = p
    for(R0 in sort(unique(c(params2a$R0,params2b$R0,seq(R0_min,R0_max,by=R0_step2))))) {
      params$R0   = R0
      params$beta = get_beta(params)
      seirs       = seirs(params,steps=sir_system_steps)
      B           = seirs[sir_system_steps,]$B
      a           = FALSE
      if(R0 == params2b$R0 && params$p == pmin) {
        a = "A1"
      } else if (R0 == params2a$R0 && params$p == pmax) {
        a = "A2"
      }
      this        = data.frame(B=B,R0=R0,p=params$p,id=params$id,pstr=plevel[i],m=models[i],a=a)
      #print(this,i)
      out         = rbind(out,this)
    }
    i = i + 1
  }
  return(list(out))
}

calculate3 = function(params3) { # R0,ip,lp,id,le,alpha,p) {
  out = data.frame()
  for(p in seq(p3_min,p3_max,by=p3_step)) {
    params   = params3
    params$p = p/365
    seirs1  = seirs(params,steps=sir_system_steps) 
    out      = rbind(out,seirs1)
  }
  return(list(out))
}

