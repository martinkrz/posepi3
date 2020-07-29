output$masthead = renderPrint({ 
  cat(paste("<div id=natmeth><img src='img/nature.methods.png'/></div>",sep=""))
  cat(paste("<div id=mast>Shea, K., Bjørnstad, O., Krzywinski, M. & Altman, N. <a href=''>Points of Significance: Uncertainty and the modelling and management of infectious epidemics.</a> (2020) <i>Nature Methods</i> <b>17</b>:in press.</div>",sep=""))
})

sir_caption = function(tmax,p) {
  paste("Plots were computed numerically using the SEIRS model (see Equation tab) from",varfmt("t"),"= 0 to",varfmt("t"),"=",formatC(tmax,format="f",big.mark=",",digits=0),"days",sprintf("(%.2f years)",tmax/365),"in",formatC(sir_system_steps,format="f",big.mark=",",digits=0),"time steps initialized at <i>t</i> = 0 with",varfmt("Szero = 1 - Izero,",1-sir_init_i-p,prec=3),varfmt("Ezero,",sir_init_i),varfmt("Izero,",sir_init_i,prec=3)," and",varfmt("Rzero = 0."),sep=" ")
}
