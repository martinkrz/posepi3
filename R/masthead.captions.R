output$masthead = renderPrint({ 
  cat(paste("<div id=natmeth><img src='img/nature.methods.png'/></div>",sep=""))
  cat(paste("<div id=mast>Shea, K., Bjørnstad, O., Krzywinski, M. & Altman, N. Points of Significance: Uncertainty and the management of epidemics. (2020) <i>Nature Methods</i> <b>17</b> (in press).</div>",sep=""))
})

sir_caption = function(tmax,p) {
  paste("Plots were computed numerically using the SEIRS model (see Equation tab) from",varfmt("t"),"= 0 to",varfmt("t",tmax/365,prec=1,unit="years"),"in",formatC(sir_system_steps,format="f",big.mark=",",digits=0),"time steps initialized at <i>t</i> = 0 with",varfmt("Szero = 1 - Izero,",1-sir_init_i-p,prec=3),varfmt("Izero ="),varfmt("Bzero",sir_init_i,prec=3)," and",varfmt("Ezero ="),varfmt("Rzero = 0."),sep=" ")
}
