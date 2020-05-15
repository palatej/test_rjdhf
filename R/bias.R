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