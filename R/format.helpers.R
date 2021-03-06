figure_title = function(index,title) {
  sprintf("<p class=fignumber>Figure %d.</p><p>%s</p>",index,title)
}

evpi_table = function(models,actionsR0,actionsp,matrix,title) {

  k    = 100
  fmt  = "%.1f%%"
  avgs = colMeans(matrix)
  opts = apply(matrix,1,min)     

  cat("<div class=parameters style='float:none;width:100%;'>")
  cat(paste(sprintf("<div class=tabletitle>%s</div>",title)))
  
  cat("<table class=evpi>")
  cat("<tr>")
  cat("<td></td>")
  cat("<td></td>")
  cat("<td class='bline' colspan=2><span class='caps bold outline'>action</span><br>","<span class=params><i>R</i><sub>0</sub>, <i>p</i></span>","</td>")
  cat("<td></td>")
  cat("</tr>")

  cat("<tr>")
  cat("<td></td>")
  cat("<td></td>")
  cat(paste("<td>","<span class=a1>A<sub>1</sub></span>","<br>","<span class=params>",sprintf("%s, %s%%",actionsR0[1],actionsp[1]),"</span>","</td>",sep=""))
  cat(paste("<td>","<span class=a2>A<sub>2</sub></span>","<br>","<span class=params>",sprintf("%s, %s%%",actionsR0[2],actionsp[2]),"</span>","</td>",sep=""))
  cat("<td>optimum</td>")
  cat("</tr>")
  
  cat("<tr>")
  cat("<td rowspan=2 class='rline right'><span class='caps bold outline'>MODEL</span> <span class=params>1/&omega;</span></td>")
  cat(paste("<td class=right>","<span class=m1>M<sub>1</sub></span>"," <span class=params>",sprintf("%.2f",models[1]),"</span>","</td>",sep=""))
  cat(paste("<td class=m1>","<span class=subtle>B<sub>11</sub></span>",sprintf(fmt,k*matrix[1,1]),"</td>",sep=""))
  cat(paste("<td class=m1>","<span class=subtle>B<sub>12</sub></span>",sprintf(fmt,k*matrix[1,2]),"</td>",sep=""))
  cat(paste("<td>",sprintf(fmt,k*opts[1]),"</td>"))
  cat("</tr>")

  cat("<tr>")
  cat(paste("<td class=right>","<span class=m2>M<sub>2</sub></span>"," <span class=params>",sprintf("%.2f",models[2]),"</span>","</td>",sep=""))
  cat(paste("<td class=m2>","<span class=subtle>B<sub>21</sub></span>",sprintf(fmt,k*matrix[2,1]),"</td>"))
  cat(paste("<td class=m2>","<span class=subtle>B<sub>22</sub></span>",sprintf(fmt,k*matrix[2,2]),"</td>"))
  cat(paste("<td>",sprintf(fmt,k*opts[2]),"</td>"))
  cat("</tr>")

  cat("<tr>")
  cat("<td></td>")
  cat(paste("<td class=right>average</td>"))
  cat(paste("<td>",sprintf(fmt,k*avgs[1]),"</td>"))
  cat(paste("<td>",sprintf(fmt,k*avgs[2]),"</td>"))
  cat(paste("<td>",sprintf(fmt,k*mean(opts)),"</td>"))
  cat("</tr>")

  cat("<tr>")
  cat(paste("<td colspan=4 class=right>EVPI</td>"))
  cat(paste("<td class=bold>",sprintf(fmt,k*(min(avgs)-mean(opts)),"</td>")))
  cat("</tr>")

  cat("</table></div>")  
}

table = function(rows,class="normal",title=NULL) {
  cat("<div class=parameters>")
  cat(paste(sprintf("<div class=tabletitle>%s</div>",title)))
  cat("<table class=param>")
  for(i in 1:nrow(rows)) {
    row = rows[i,]
    cat(paste("<tr><td>",row$name,"</td><td>",row$value,"</td></tr>",sep=""))
  }
  cat("</table></div>")
}

varfmt = function(name=NULL,value=NULL,paren=0,prec=1,percent=0,comma=0,units="") {
  trailing = ""
  if(is.null(name)) {
    fmtstr = sprintf("%%.%df",prec)
    if(percent) {
      fmtstr = paste(fmtstr,"%%",sep="")
    }
    if(units != "") {
      fmtstr = paste(fmtstr,units,sep=" ")
    }
    if(percent) {
      value = 100 * value
    }
    str   = sprintf(fmtstr,value)
    if(paren) {    
      str = paste("(",str,")",sep="")
    } 
    return(str)
  }
  rx = str_match_all(name,"^(.+)([.,])$")
  if(length(rx[[1]]) == 3) {
    name     = rx[[1]][[2]]
    trailing = rx[[1]][[3]]
  }
  
 
  if(name == "ip") {
    name    = HTML("<i>ip</i>")
    prec    = 0
    units   = "days"
  } else if (name == "R0") {
    #name    = HTML("<i>R</i><sub>0</sub>")
    prec    = 1
  } else if (name == "S(0)") {
    #name    = HTML("<i>S</i>(0)")
  } else if (name == "I(0)") {
    #name    = HTML("<i>I</i>(0)")
  } else if (name == "R(0)") {
    #name    = HTML("<i>R</i>(0)")
  } else if (name == "pc") {
    #name    = HTML("<i>p</i><sub>c</sub>")
    prec    = 0
    percent = 1
  } else if (name == "p") {
    name    = HTML("<i>p</i>")
    prec    = 0
    percent = 1
  } else if (name == "Imax") {
    #name    = HTML("<i>I</i><sub>max</sub>")
    prec    = 1
    percent = 1
  } else if (name == "t") {
    name    = HTML("<i>t</i>")
    prec    = 2
    units   = "years"
  } else if (name == "C") {
    name    = HTML("<i>C</i>")
    prec    = 0
    percent = 1
  } else if (name == "I") {
    name    = HTML("<i>I</i>")
    prec    = 1
    percent = 1
  } else if (name == "t1") {
    name    = HTML("<i>t</i><sub>1</sub>")
    prec    = 1
    units   = "days"
  } else if (name == "t2") {
    name    = HTML("<i>t</i><sub>2</sub>")
    prec    = 1
    units   = "days"
  } else if (name == "deltat") {
    name    = HTML("Δ<i>t</i>")
    prec    = 1
    units   = "days"
  } else if (name == "Ideltat") {
    name    = HTML("Σ<i>I</i>(Δ<i>t</i>)")
    prec    = 1
    percent = 1
    units   = "cases"
  } else if (name == "I>deltat") {
    name    = HTML("Σ<i>I</i><sub>&gt;</sub>(Δ<i>t</i>)")
    prec    = 1
    percent = 1
    units   = "cases"
  } else if (name == "tmax") {
    name    = HTML("<i>t</i><sub>max</sub>")
    prec    = 1
    units   = "years"  
  } else if (name == "Stmax") {
    name    = HTML("<i>S</i>(<i>t</i><sub>max</sub>)")
    prec    = 1
    percent   = 1
  } else if (name == "Rtmax") {
    name    = HTML("<i>R</i>(<i>t</i><sub>max</sub>)")
    prec    = 1
    percent  = 1
  } else if (name == "Sinf") {
    #name    = HTML("<i>S</i>(∞)")
    prec    = 1
    percent   = 1
  } else if (name == "Rinf") {
    #name    = HTML("<i>R</i>(∞)")
    prec    = 1
    percent = 1
  } else if (name == "beta") {
    #name = HTML("<i>β</i>")
  } else if (name == "gamma") {
    #name = HTML("<i>γ</i>")
  } else if (name == "sigma") {
    #name = HTML("<i>&sigma;</i>")
  } else if (name == "omega") {
    #name = HTML("<i>&omega;</i>")
  } else if (name == "mu") {
    #name = HTML("<i>&mu;</i>")
  }

    # italicize model parameters
  name = str_replace_all(name, "\\b(alpha|beta|gamma|sigma|omega|mu)\\b", function(x){sprintf("<i>&%s;</i>",x)})
  # endemic equilibria e.g. Sinf, Rinf, etc
  name = str_replace_all(name, "\\b(.inf)\\b", function(x){sprintf("<i>%s</i>(∞)",str_remove(x,"inf"))})
  # any function
  name = str_replace_all(name, "(.\\(t\\))", function(x){sprintf("<i>%s</i>(t)",str_remove(x,"\\(t\\)"))})
  # stars,
  name = str_replace_all(name, "\\b(.star)\\b", function(x){sprintf("<i>%s</i><sup>*</sup>",str_remove(x,"star"))})
  # initial values, e.g. Szero
  name = str_replace_all(name, "\\b(.zero)", function(x){sprintf("<i>%s</i>(0)",str_remove(x,"zero"))})
  # R0
  name = str_replace_all(name, "\\bR0\\b", function(x){sprintf("<i>R</i><sub>0</sub>",x)})
  # min,max subscripts
  name = str_replace_all(name, "(min|max|avg)\\b", function(x){sprintf("<sub>%s</sub>",x)})
  # action/model subscripts
  name = str_replace_all(name, "([MA])([1-9])\\b", "\\1<sub>\\2</sub>")
  # vaccination                         
  name = str_replace_all(name, "pcrit", function(x){sprintf("<i>p</i><sub>c</sub>",str_remove(x,"crit"))})
  # vaccination                         
  name = str_replace_all(name, "\\bp\\b", function(x){sprintf("<i>p</i>")})
  # vaccination                         
  name = str_replace_all(name, "\\bpmu\\b", function(x){sprintf("<i>p&mu;</i>")})
  # time                         
  name = str_replace_all(name, "\\b(B|T|t|A)", function(x){sprintf("<i>%s</i>",x)})
  # time                         
  name = str_replace_all(name, "_E", "<sub>E</sub>")
  # time                         
  name = str_replace_all(name, "\\b(inf)\\b", "∞")

  if(! is.null(value)) {
    if(comma) {
      fmtstr = sprintf("%%s = %%s")
    } else {
      fmtstr = sprintf("%%s = %%.%df",prec)
    }
    if(percent) {
      fmtstr = paste(fmtstr,"%%",sep="")
    }
    if(units != "") {
      fmtstr = paste(fmtstr,units,sep=" ")
    }
    if(percent) {
      value = 100 * value
    }
    if(comma) {
      value = formatC(value,format="f",big.mark=",",digits=prec)
    }
    str    = sprintf(fmtstr,name,value)
  } else {
    str = name
  }
  if(trailing != "") {
    str = paste(str,trailing,sep="")
  }
  str = str_replace_all(str, "-", "&ndash;")  
  if(paren) {    
    str = paste("(",str,")",sep="")
  }
  return(str)
}

label_to_percent = function(str) {
  str = sprintf("%f",str*100)
  parse(text=str)
}
label_to_identity = function(str) {
  return(str)
}
ceilToFraction = function(num, den = 1) {
  x = den * ceiling( num / den)
  return(x)
}
floorToFraction = function(num, den = 1) {
  x = den * floor(num / den)
  return(x)
}

makerows = function(items) {
  rows = data.frame()
  for(i in seq(1,length(items),by=2)) {
    rows = rbind(rows,data.frame(name=varfmt(items[i]),value=items[i+1]))
  }
  return(rows)
}

select_int = function(x) {
  return(x[ abs(x-round(x) ) < 1e-6])
}
is_int = function(x) {
  return(abs(x-round(x)) < 1e-6)
}

