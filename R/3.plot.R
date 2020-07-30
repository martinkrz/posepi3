plots3 = function(params) {

  out   = df3()
  df1   = out[[1]]
  tmax  = params$tmax
  Bmax  = max(df1$B)
  df1$B = df1$B/Bmax
  plot1 = ggplot()
  plot2 = ggplot()
  
  #df1 = df1 %>% group_by(id,R0) %>% mutate(Bmin=min(B),Bmax=max(B)) %>% ungroup(id) %>% ungroup(R0)
  plot1 = plot1 + geom_line(data=df1,aes(x=time/365,y=B,group=p),size=plot_line_width)
  plot2 = plot2 + geom_path(data=df1,aes(x=S,y=I,group=p),size=plot_line_width)
  #plot1 = plot1 + geom_line(data=df1,aes(x=R0,y=B,color=pstr,group=paste(m,pstr)),size=plot_line_width) 
  #plot1 = plot1 + geom_point(data=df1%>%filter(a!=FALSE),aes(x=R0,y=B,fill=pstr,color=m),stroke=1,shape=21,size=3) 
  #plot1 = plot1 + scale_color_manual(values = palette)
  #plot1 = plot1 + scale_fill_manual(values = palette)
  
  param1_text = sprintf("<i>R</i><sub>0</sub> = %.1f, 1/<i>&beta;</i><sub>1</sub> = %.2f days, 1/<i>&gamma;</i><sub>1</sub> = %d days, 1/<i>&sigma;</i><sub>1</sub> = %d days, 1/<i>&omega;</i><sub>1</sub> = %.1f years, 1/<i>&mu;</i><sub>1</sub> = %d years and <i>&alpha;</i><sub>1</sub> = %.3f/day",
                       params$R0,
                       params$beta,
                       params$ip,
                       params$lp,
                       params$id/365,
                       params$le/365,
                       params$alpha)

  title1 = sprintf("Effect of vaccination on cumulative disease burden. %s",param1_text)
  plot1 = plot1 + my.plot_axis(xlab="t",ylab="cumulative burden, B",
                               xmin=0,
                               xmax=tmax/365,
                               ymin=0,ymax=1,
                               ypercent)
  plot2 = plot2 + my.plot_axis(xlab="susceptible fraction (%)",ylab="infected fraction (%)",
                               xmin=min(df1$S),
                               xmax=max(df1$S),
                               ymin=0,
                               ymax=max(df1$I),
                               xpercent=1,
                               ypercent=1)
  
  
  caption1 = paste("Caption.")
  caption1 = paste(caption1,sir_caption(tmax,params$p))

  return(list(
    list(plot_theme(plot1),plot_theme(plot2)),
    list(6,7),
    list(title1,title1),
    list(caption1,caption1)))
  
}
