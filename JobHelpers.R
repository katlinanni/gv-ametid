require(tidyverse)
require(fuzzyjoin)
require(qdap)
require(readr)
require(readxl)
require(writexl)
require(vctrs)
require(stringdist)


leidur = function(var,x,y,z=NULL, excl1 = NULL, excl2 = NULL) {
  no1 = grep(x, var)
  no2 = grep(y, var)
  if(is.null(z)) no3 = no1 else no3 = grep(z, var)
  i = intersect(intersect(no1,no2),no3)
  if(!is.null(excl1)) i = i[!i %in% grep(excl1, var)]
  if(!is.null(excl2)) i = i[!i %in% grep(excl2, var)]
  var[i]
}

asendaja = function(var,x,y,z=NULL,excl1=NULL,excl2 = NULL,excl3=NULL,excl4=NULL, replace) {
  no1 = grep(x, var)
  no2 = grep(y, var)
  if(is.null(z)) no3 = no1 else no3 = grep(z, var)
  i = intersect(intersect(no1,no2),no3)
  if(!is.null(excl1)) i = i[!i %in% grep(excl1, var)]
  if(!is.null(excl2)) i = i[!i %in% grep(excl2, var)] 
  if(!is.null(excl3)) i = i[!i %in% grep(excl3, var)]
  if(!is.null(excl4)) i = i[!i %in% grep(excl4, var)]
  cat("\nAsendan", length(i), "\n")
  var[i] = replace
  var
}

replaceImmediately = list(
  c("olen ", ""),
  c("olin ", ""),
  c("olnud ", ""),
  c("töötan ", ""),
  c("praegu ", ""),
  c("enne ", ""),
  c("enne-", ""),
  c("nüüd ", ""),
  c("hetkel ", ""),
  c("eesti ", ""),
  c(" ja ", ""),
  c(" and ", ""),
  c(" või ", ""),
  c(" ning ", ""),
  c(" ehk ", ""),
  c(" ei ", ""),
  c(" aastat ", ""),
  c(" по ", ""),
  c(" в ", ""),
  c(" и ", ""),
  c(" на ", ""),
  c(" of ", ""),
  c("  ", " "),
  c("eluaegne", " "),
  c("pedagoog", "õpetaja"),
  c("opetaja", "õpetaja"),
  c("elukutseline", " "),
  c("juhatuse esimees", "juht"),
  c("esimees", "juht"),
  c("peadirektor", "direktor"),
  c("direktori", "juhi"),
  c("direktor", "juht"),
  c("assistent", "abi"),
  c("klenditeenindaja", "klienditeenindaja"),
  c("klinditeenindaja", "klienditeenindaja"),
  c("kliemditeenindaja", "klienditeenindaja"),
  c("klienditeenendaja", "klienditeenindaja"),
  c("juhtiv", "pea"),
  c("isikliku", "oma"),
  c("firma", "ettevõtte"),
  c("ekspert", "spetsialist"),
  c("õpetja", "õpetaja"),
  c("klienditeenintaja", "klienditeenindaja"),
  c("klienditeeindaja", "klienditeenindaja"),
  c("peaspetsilist", "peaspetsialist"),
  c("juhatuse liige,", "")
)
