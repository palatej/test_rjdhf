#' Title
#'
#' @param y Numerics. original series, perhaps corrected for deterministic effects. In that case it's exp(linearized). 
#' @param lt Numerics. trend of log(y)
#' @param ls Numerics. seasonal or periodic component of log(y)
#' @param li Numerics. irregular component of log(y)
#' @param period Numerics. Length of a cycle (of the periodic component)
#'
#' @return A data.frame with y, t, s, i, so that y=t*s*i
#' @details Based on the bias correction in SEATS. 
#' @examples
biasCorrection<-function(y, lt, ls, li, period){
  # TO DO. Check possible numerical problems in the canonical decomposition (ly != lt+ls+li) and throw a warning if differences are inacceptable
  t<-exp(lt)
  s<-exp(ls)
  i<-exp(li)
  n<-length(y)
  # rounding for non integer periods
  nc<-round(length(y)-n%%period)
  sbias<-mean(s[1:nc])
  s<-s/sbias
  ibias<-mean(i)
  t<-t*(ibias*sbias)
  # we correct some numerical problems that way (as in SEATS)
  sa<-y/s
  i<-sa/t
  
  return(data.frame(y=y, sa=sa, t=t, s=s, i=i))
}

s_axb<-function(a,b){
  f<-array(0, a+b-1)
  q<-rep(1/(a*b), a)
  idx<-1:a
  f[idx]<-f[idx]+q
  for (i in 2:b){
    idx<-idx+1
    f[idx]<-f[idx]+q
  }
  return(f)
}

smooth<-function(x, f, endpoints.copy=T){
  y<-filter(x, f)
  if (endpoints.copy){
    n<-(length(f)-1)/2
    y[1:n]<-y[n+1]
    m<-length(y)
    y[(m-n+1):m]<-y[m-n]
  }
  return (y)
}

#' Title
#'
#' @param y Numerics. original series, perhaps corrected for deterministic effects. In that case it's exp(linearized). 
#' @param lt Numerics. trend of log(y)
#' @param ls Numerics. seasonal or periodic component of log(y)
#' @param li Numerics. irregular component of log(y)
#' @param period Numerics. Length of a cycle (of the periodic component)
#'
#' @return A data.frame with y, t, s, i, so that y=t*s*i
#' @details Based on the bias correction in SEATS. 
#' @examples
biasCorrection2<-function(y, lt, ls, li, period){
  # TO DO. Check possible numerical problems in the canonical decomposition (ly != lt+ls+li) and throw a warning if differences are inacceptable
  t<-exp(lt)
  s<-exp(ls)
  i<-exp(li)
  period<-round(period)
  if ((period %% 2) == 0)
    filter<-s_axb(period, 2)
  else
    filter<-rep(1/period,period)
  sbias<-smooth(s, filter, endpoints.copy = T)
  s<-s/sbias
  t<-t*sbias
  # we correct some numerical problems that way (as in SEATS)
  sa<-y/s
  i<-sa/t
  
  return(data.frame(y=y, sa=sa, t=t, s=s, i=i))
}



