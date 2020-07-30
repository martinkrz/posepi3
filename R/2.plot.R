plots2 = function(params2a,params2b,params2c,params2d) {

  #      R0   p   id
  # 2a  max  max  min  M1 A1
  # 2b  min  min  min  M1 A2
  # 2c  max  max  max  M2 A1
  # 2d  min  min  max  M2 A2
  
  out   = df2()
  df1   = out[[1]]
  tmax  = params2a$tmax
  Bmax  = max(df1$B)
  df1$B = df1$B/Bmax
  plot1 = ggplot()
  
  pmin  = min(params2a$p,params2b$p,params2c$p,params2d$p)
  pmax  = max(params2a$p,params2b$p,params2c$p,params2d$p)
  idmin = min(params2a$id,params2b$id,params2c$id,params2d$id)
  idmax = max(params2a$id,params2b$id,params2c$id,params2d$id)

  df1 = df1 %>% group_by(id,R0) %>% mutate(Bmin=min(B),Bmax=max(B)) %>% ungroup(id) %>% ungroup(R0)
  
  plot1 = plot1 + geom_ribbon(data=df1,aes(ymin=Bmin,ymax=Bmax,x=R0,group=m,fill=m),alpha=0.3)
  plot1 = plot1 + geom_line(data=df1,aes(x=R0,y=B,color=pstr,group=paste(m,pstr)),size=plot_line_width) 

  plot1 = plot1 + geom_point(data=df1%>%filter(a!=FALSE),aes(x=R0,y=B,fill=pstr,color=m),stroke=1,shape=21,size=3) 
  plot1 = plot1 + scale_color_manual(values = palette)
  plot1 = plot1 + scale_fill_manual(values = palette)
  
  param1_text = sprintf("<i>R</i><sub>0,1</sub> = %.1f, 1/<i>&beta;</i><sub>1</sub> = %.2f days, 1/<i>&gamma;</i><sub>1</sub> = %d days, 1/<i>&sigma;</i><sub>1</sub> = %d days, 1/<i>&omega;</i><sub>1</sub> = %.1f years, 1/<i>&mu;</i><sub>1</sub> = %d years and <i>&alpha;</i><sub>1</sub> = %.3f/day with <i>p</i><sub>1</sub> = %.0f%% of the population vaccinated",
                       params2a$R0,
                       params2a$beta,
                       params2a$ip,
                       params2a$lp,
                       params2a$id/365,
                       params2a$le/365,
                       params2a$alpha,
                       params2a$p*100)
  
  title1 = sprintf("Effect of management actions on cumulative burden. %s",param1_text)
  
  plot1 = plot1 + my.plot_axis(xlab="R0",ylab="cumulative burden, B",
                               xmin=1,
                               xmax=max(df1$R0),
                               ymin=0,
                               ymax=1,
                               ypercent=1)

  caption1 = paste("Caption.")
  caption1 = paste(caption1,sir_caption(tmax,params2a$p))

  return(list(
    list(plot_theme(plot1)),
    list(5),
    list(title1),
    list(caption1)))
  
}
