require(tidyverse)
require(fuzzyjoin)
require(qdap)
require(readr)
setwd("~/Private/gvpers")

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

codes = new = read_tsv("AmetidKodeerimisele.tsv") %>% select(c(1:2,6))

sonad = tolower(codes$workPositionName)
sonad = gsub("olen ", "", sonad, fixed=T)
sonad = gsub("olin ", "", sonad, fixed=T)
sonad = gsub("olnud ", "", sonad, fixed=T)
sonad = gsub("töötan ", "", sonad, fixed=T)
sonad = gsub("praegu ", "", sonad, fixed=T)
sonad = gsub("enne ", "", sonad, fixed=T)
sonad = gsub("enne-", "", sonad, fixed=T)
sonad = gsub("nüüd ", "", sonad, fixed=T)
sonad = gsub("hetkel ", "", sonad, fixed=T)
sonad = gsub("eesti ", "", sonad, fixed=T)
sonad = gsub(" ja ", "", sonad, fixed=T)
sonad = gsub(" and ", "", sonad, fixed=T)
sonad = gsub(" või ", "", sonad, fixed=T)
sonad = gsub(" ning ", "", sonad, fixed=T)
sonad = gsub(" ehk ", "", sonad, fixed=T)
sonad = gsub(" ei ", "", sonad, fixed=T)
sonad = gsub(" aastat ", "", sonad, fixed=T)
sonad = gsub(" по ", "", sonad, fixed=T)
sonad = gsub(" в ", "", sonad, fixed=T)
sonad = gsub(" и ", "", sonad, fixed=T)
sonad = gsub(" на ", "", sonad, fixed=T)
sonad = gsub(" of ", "", sonad, fixed=T)
sonad = gsub("  ", " ", sonad, fixed=T)
sonad = gsub("eluaegne", " ", sonad, fixed=T)
sonad = gsub("pedagoog", "õpetaja", sonad, fixed=T)
sonad = gsub("opetaja", "õpetaja", sonad, fixed=T)
sonad = gsub("elukutseline", " ", sonad, fixed=T)
sonad = gsub("juhatuseesimees", "juht", sonad, fixed=T)
sonad = gsub("esimees", "juht", sonad, fixed=T)
sonad = gsub("peadirektor", "direktor", sonad, fixed=T)
sonad = gsub("direktori", "juhi", sonad, fixed=T)
sonad = gsub("direktor", "juht", sonad, fixed=T)
sonad = gsub("assistent", "abi", sonad, fixed=T)
sonad = gsub("klenditeenindaja", "klienditeenindaja", sonad, fixed=T)
sonad = gsub("klinditeenindaja", "klienditeenindaja", sonad, fixed=T)
sonad = gsub("kliemditeenindaja", "klienditeenindaja", sonad, fixed=T)
sonad = gsub("klienditeenendaja", "klienditeenindaja", sonad, fixed=T)
sonad = gsub("juhtiv", "pea", sonad, fixed=T)
sonad = gsub("isikliku", "oma", sonad, fixed=T)
sonad = gsub("firma", "ettevõtte", sonad, fixed=T)
sonad = gsub("ekspert", "spetsialist", sonad, fixed=T)
sonad = gsub("õpetja", "õpetaja", sonad, fixed=T)

# mõned levinumad venekeelsed - ei tööta täielikult (tean juba miks, parandan esimesel võimalusel)
sonad = gsub("^медсестра\\b", "meditsiiniõde", sonad, fixed=F)
sonad = gsub("^швея\\b", "õmbleja", sonad, fixed=F)
sonad = gsub("^врач\\b", "arst", sonad, fixed=F)
sonad = gsub("^инженер\\b", "insener", sonad, fixed=T)
sonad = gsub("^парикмахер\\b", "juuksur", sonad, fixed=T)
sonad = gsub("^оператор\\b", "operaator", sonad, fixed=T)
sonad = gsub("^администратор\\b", "administraator", sonad, fixed=T)
sonad = gsub("^учитель\\b", "õpetaja", sonad, fixed=T)
sonad = gsub("^бухгалтер\\b", "raamatupidaja", sonad, fixed=T)

## Maybe there are more synonyms to be replaced in one go?
## Levinumad venekeelsed võib ka kohe asendada, nt õpetajad jms - esialgu lisasin, aga siis on natuke segane, kui pool sõna eesti ja pool vene keeles, seega kustutasin ära

sonad = sonad %>% strsplit(",") %>% lapply(function(x) x[1]) %>% unlist
sonad = sonad %>% strsplit(";") %>% lapply(function(x) x[1]) %>% unlist
sonad = sonad %>% strsplit("/") %>% lapply(function(x) x[1]) %>% unlist

new$new = gsub(" ", "", sonad, fixed=T)
dic = unique(new$new[new$AtLeastTwo == 1 & !is.na(new$new)])
check = new$new %>% as.character
checked = check_spelling(strtrim(check,30), dictionary = dic,range = 2, assume.first.correct = F) 
checked = checked %>% filter(!duplicated(row))
new$row = 1:nrow(new)
new = left_join(new, checked[,1:5], by = "row") %>% select(-row)
new$dist = stringdist::stringsim(new$new, new$suggestion)
#new %>% filter(AtLeastTwo == 0) %>% select(scode, not.found:suggestion, dist) %>% arrange(desc(dist)) %>% filter(dist < .75) %>% slice(1:20)
new %>% filter(AtLeastTwo == 0) %>% select(scode, not.found:suggestion, dist) %>% arrange(desc(dist)) %>% filter(dist < .85) %>% slice(1:20)

### ma natuke vaatasin neid > .75 asendusi ja päris mõned tundusid natuke ikka liiga mööda asendused, korrigeerisin .85-ks
## replace
i = !is.na(new$dist) & (new$dist > .85)
new$new[i] = new$suggestion[i]

# siin on käsitsi asendatud faili lisamine - st muudab need nimetused ära, mis käsitsi muudeti
asendus = readxl::read_xlsx("C:/Users/K/Google Drive/GV/tmpAsendatud_KA2.xlsx") %>% filter(asendatud %in% 1) %>% select(workPositionName, rewrite)
new = left_join(new, asendus, by = "workPositionName")
new$newWithManualReplacement = ifelse(!is.na(new$rewrite), new$rewrite, new$new)

# selleks, et saaks kogu koodi ka neist käsitsi muudetud nimedest üle käia, siis nimetasin mõned tunnused ringi
new = rename(new, new1 = new, new = newWithManualReplacement)
new$new <- new$new %>% gsub(" ", "", ., fixed = TRUE) %>% tolower()

## start fishing

#FIE-d -- valdkondade kaupa määratlenud end ainult üksikud, seega mõistlikum kas kõik FIE-d kokku panna või siis valdkonna määratlenud FIE-d ameti järgi gruppi panna
table(leidur(new$new, "fie", "")) 
new$new = asendaja(new$new, "füüsilisest", "", replace = "fie")
new$new = asendaja(new$new, "fie", "", replace = "fie")

unique(leidur(new$new, "eisoovi", ""))
new$new = asendaja(new$new, "eisoovi", "", replace = "NA")
new$new = asendaja(new$new, "ei", "avalda", replace = "NA")

unique(leidur(new$new, "tudeng", ""))
new$new = asendaja(new$new, "tudeng","", excl1 = "nõust", excl2 = "assist", excl3 = "spetsial", replace = "tudeng")
new$new = asendaja(new$new, "üliõpilane","", excl1 = "ema", excl2 = "abiarst", excl3 = "õpetaja", replace = "tudeng")

unique(leidur(new$new, "õpilane", ""))
new$new = asendaja(new$new, "õpilane", "", excl1 = "õpetaja", excl2 = "üliõp", replace = "õpilane")

# järeldoktorandid tegin teaduriteks (N = 5)
unique(leidur(new$new, "doktorant", ""))
new$new = asendaja(new$new, "doktorant", "", excl1 = "nooremteadur", excl2 = "järel", replace = "doktorant")
new$new = asendaja(new$new, "doktor", "järel", replace = "teadur")

# see peaks olema vist Kaitseväe Akadeemia med-õppejõu nimetus
unique(leidur(new$new, "õde", "õpetaja"))
new$new = asendaja(new$new, "õde", "õpetaja", replace = "õppejõud")

unique(leidur(new$new, "med", "õde", excl1 = "hamba"))
new$new = asendaja(new$new, "med","õde", excl1 = "abi", replace = "meditsiiniõde")
new$new = asendaja(new$new, "vanemõde","", replace = "meditsiiniõde")
new$new = asendaja(new$new, "õde","kiirabi", replace = "meditsiiniõde")
new$new = asendaja(new$new, "õde","intensiiv", replace = "meditsiiniõde")
new$new = asendaja(new$new, "õde","psühh", replace = "meditsiiniõde")
new$new = asendaja(new$new, "meditsiiniōde", "", replace = "meditsiiniõde")
new$new = asendaja(new$new, "med.ode", "", replace = "meditsiiniõde")
new$new = asendaja(new$new, "med", "öde", replace = "meditsiiniõde")
new$new = asendaja(new$new, "kiirabi", "brigaadijuht", replace = "meditsiiniõde")
new$new = asendaja(new$new, "pere","õde", excl1 = "ämmaemaja", replace = "meditsiiniõde")
new$new = asendaja(new$new, "anestesist","", replace = "meditsiiniõde")
new$new = asendaja(new$new, "ôde","", replace = "meditsiiniõde")
new$new = asendaja(new$new, "pereode","", replace = "meditsiiniõde")
new$new = asendaja(new$new, "^öde$","", replace = "meditsiiniõde")
new$new = asendaja(new$new, "^ōde$","", replace = "meditsiiniõde")

unique(leidur(new$new, "õend", "juht"))
new$new = asendaja(new$new, "õend","juht", excl1 = "pikka", replace = "õendusjuht")
new$new = asendaja(new$new, "ämmaemand","juht", replace = "õendusjuht")
new$new = asendaja(new$new, "ülemõde","", replace = "õendusjuht")

unique(leidur(new$new, "hamba", "õde"))
new$new = asendaja(new$new, "õde", "hamba", replace = "hambaraviõde")
new$new = asendaja(new$new, "õde", "suuhügieen", replace = "hambaraviõde")
new$new = asendaja(new$new, "", "suuhügienist", replace = "hambaraviõde")

unique(leidur(new$new, "abi", "õde"))
new$new = asendaja(new$new, "abi", "õde", excl1 = "ilu", replace = "hooldusõde")
new$new = asendaja(new$new, "hooldus", "õde", replace = "hooldusõde")

unique(leidur(new$new, "õde", "haigla"))
new$new = asendaja(new$new, "õde", "haigla", excl1 = "hooldus", replace = "meditsiiniõde")
new$new = asendaja(new$new, "õde", "kliin", replace = "meditsiiniõde")
new$new = asendaja(new$new, "õendus","", excl1 = "juht", excl2 = "koordineerija", replace = "meditsiiniõde")

unique(leidur(new$new, "ämma", ""))
new$new = asendaja(new$new, "ämma","", excl1 = "füsio", excl2 = "õpin", excl3 = "abi", replace = "ämmaemand")
new$new = asendaja(new$new, "ämma","abi", replace = "abiämmaemand")

unique(leidur(new$new, "õde", ""))
new$new = asendaja(new$new, "õde","", excl1 = "hamba", excl2 = "vokaal", excl3 = "ilu", excl4 = "hooldus", replace = "meditsiiniõde")

unique(leidur(new$new, "arst", "õppe"))
new$new = asendaja(new$new, "arst", "õppe", replace = "eriarst")

unique(leidur(new$new, "arst", "pere"))
new$new = asendaja(new$new, "arst","pere", excl1 = "regis", excl2 = "juhataja", excl3 = "assist", excl4 = "hammba", replace = "üldarst")
new$new = asendaja(new$new, "arst", "üld", excl1 = "resident", replace = "üldarst")

unique(leidur(new$new, "kirurg", "", excl1 = "assist"))
new$new = asendaja(new$new, "kirurg","", excl1 = "veterinaar", excl2 = "assist", excl3 = "juh", excl4 = "suu", replace = "eriarst")
new$new = asendaja(new$new, "arst","laste", excl1 = "õde", replace = "eriarst")
new$new = asendaja(new$new, "arst","naiste", excl1 = "õde", replace = "eriarst")
new$new = asendaja(new$new, "arst","silma", excl1 = "õde", replace = "eriarst")
new$new = asendaja(new$new, "arst","sise", excl1 = "õde", replace = "eriarst")
new$new = asendaja(new$new, "psühhiaater","", excl1 = "õde", replace = "eriarst")
new$new = asendaja(new$new, "radioloog","", excl1 = "tehnik", excl2 = "abi", excl3 = "hooldaja", replace = "eriarst")
new$new = asendaja(new$new, "kardioloog","", replace = "eriarst")
new$new = asendaja(new$new, "günekoloog","", replace = "eriarst")
new$new = asendaja(new$new, "endokrino","", replace = "eriarst")
new$new = asendaja(new$new, "neurol","", excl1 = "juh", replace = "eriarst")
new$new = asendaja(new$new, "ortopeed","", excl1 = "spets", replace = "eriarst")
new$new = asendaja(new$new, "pediaat","", excl1 = "juh", replace = "eriarst")
new$new = asendaja(new$new, "taastusarst","", replace = "eriarst")
new$new = asendaja(new$new, "kõrva-nina","", replace = "eriarst")
new$new = asendaja(new$new, "anestesioloog","", excl1 = "juh", replace = "eriarst")
new$new = asendaja(new$new, "laboriarst","", replace = "eriarst")
new$new = asendaja(new$new, "gastroenteroloog","", replace = "eriarst")
new$new = asendaja(new$new, "neonatoloog","", replace = "eriarst")
new$new = asendaja(new$new, "onkoloog","", replace = "eriarst")
new$new = asendaja(new$new, "arstterapeutvereteenistuses","", replace = "eriarst")
new$new = asendaja(new$new, "otorinoloog","", replace = "eriarst")
new$new = asendaja(new$new, "andstesioloog","", replace = "eriarst")
new$new = asendaja(new$new, "arst","spetsialist", replace = "eriarst")
new$new = asendaja(new$new, "arst","tööt", replace = "eriarst")
new$new = asendaja(new$new, "arst","korralis", replace = "eriarst")
new$new = asendaja(new$new, "eriarst","", excl1 = "abi", replace = "eriarst")
new$new = asendaja(new$new, "spordiarst","", replace = "eriarst")
new$new = asendaja(new$new, "kopsuarst","", replace = "eriarst")
new$new = asendaja(new$new, "erialaarst","", replace = "eriarst")
new$new = asendaja(new$new, "emoarst","", replace = "eriarst")

unique(leidur(new$new, "arst", "pea"))
new$new = asendaja(new$new, "arst","pea", excl1 = "looma", replace = "tervishoiuteenustejuht")
new$new = asendaja(new$new, "arst","ülem", excl1 = "õde", excl2 = "aset", replace = "tervishoiuteenustejuht")
new$new = asendaja(new$new, "neurol", "juh", replace = "tervishoiuteenustejuht")
new$new = asendaja(new$new, "anestesio", "juh", replace = "tervishoiuteenustejuht")
new$new = asendaja(new$new, "arst", "osakonnaju", replace = "tervishoiuteenustejuht")
new$new = asendaja(new$new, "kliinikujuh", "", replace = "tervishoiuteenustejuht")
new$new = asendaja(new$new, "haiglajuh", "", replace = "tervishoiuteenustejuht")
new$new = asendaja(new$new, "haiglaosakonnajuh", "", replace = "tervishoiuteenustejuht")
new$new = asendaja(new$new, "tervishoiuteenustejuht", "", replace = "tervishoiuteenustejuht")

unique(leidur(new$new, "arst", "resident"))
new$new = asendaja(new$new, "arst","resident", replace = "eriarst")

unique(leidur(new$new, "arst", "hamba"))
new$new = asendaja(new$new, "arst","hamba", excl1 = "abil", excl2="assist", replace = "hambaarst")
new$new = asendaja(new$new, "stomatoloog","", replace = "hambaarst")
new$new = asendaja(new$new, "ortodont","", replace = "hambaarst")

unique(leidur(new$new, "hamba", "abi"))
new$new = asendaja(new$new, "hamba","abi", replace = "muu tervishoiuhooldustöötaja")

unique(leidur(new$new, "hamba", "tehnik"))
new$new = asendaja(new$new, "hambatehnik", "", replace = "hambatehnik")
new$new = asendaja(new$new, "proteesimeister", "", replace = "hambatehnik")

unique(leidur(new$new, "arst", "looma"))
new$new = asendaja(new$new, "arst","looma", excl1 = "abi", excl2="assist", replace = "loomaarst")
new$new = asendaja(new$new, "veteri","", excl1 = "abi", excl2="assist", excl3 = "õde", excl4 = "tehnik", replace = "loomaarst")
new$new = asendaja(new$new, "vetarst","", replace = "loomaarst")

unique(leidur(new$new, "seemendustehn", ""))
new$new = asendaja(new$new, "seemendus","tehn", excl1 = "arst", replace = "veterinaartehnik")
new$new = asendaja(new$new, "veterinaartehnik", "", replace = "veterinaartehnik")

unique(leidur(new$new, "professor", ""))
new$new = asendaja(new$new, "professor","", excl1 = "insti", excl2 = "juhataja", excl3 = "abi", replace = "professor")
new$new = asendaja(new$new, "prof","kaas", replace = "professor")

# eraldi teaduri koodi ei ole - nimetan teaduriteks, aga hiljem lähevad ilmselt ISCO järgi õppejõududeks
unique(leidur(new$new, "teadur", ""))
new$new = asendaja(new$new, "teadur","ülik", replace = "teadur")
new$new = asendaja(new$new, "teadur","vanem", excl1 = "füüsika", excl2 = "spetsialist", replace = "teadur")
new$new = asendaja(new$new, "teadur","pea", replace = "teadur")
new$new = asendaja(new$new, "teadur","juhtiv", replace = "teadur")
new$new = asendaja(new$new, "teadur","noorem", replace = "teadur")
new$new = asendaja(new$new, "teadur","kooli", replace = "teadur")
new$new = asendaja(new$new, "teadur","külalis", replace = "teadur")

unique(leidur(new$new, "õppejõud", "ülik"))
new$new = asendaja(new$new, "õppejõud", "ülik", excl1 = "juht", replace = "õppejõud")
new$new = asendaja(new$new, "koolitusjuht", "ülik", replace = "õppejõud")
new$new = asendaja(new$new, "koolitusjuht", "õppej", replace = "õppejõud")

unique(leidur(new$new, "raamatupida", "pea", "abi"))
new$new = asendaja(new$new, "raamatupida", "pea", "abi", replace = "raamatupidajaassistent")
new$new = asendaja(new$new, "raamatupida", "assist", replace = "raamatupidajaassistent")

unique(leidur(new$new, "raamatupida", "pea"))
new$new = asendaja(new$new, "raamatupida", "pea", excl1 = "finatsjuht", excl2 = "peaspe", excl3 = "finantsjuht", excl4 = "büroojuht", replace = "pearaamatupidaja")
new$new = asendaja(new$new, "pearaamatupidaja-", "", replace = "pearaamatupidaja")
new$new = asendaja(new$new, "raamatu", "pea", excl1 = "juht", replace = "pearaamatupidaja")
new$new = asendaja(new$new, "finants", "kontroll", excl1 = "assis", replace = "pearaamatupidaja")
new$new = asendaja(new$new, "raamatupida", "vanem",excl1 = "õpetaja", excl2 = "vahetuse", replace = "pearaamatupidaja")
new$new = asendaja(new$new, "finantsist", "",excl1 = "käsitöö", replace = "pearaamatupidaja")
new$new = asendaja(new$new, "eelarve", "spets", replace = "pearaamatupidaja")
new$new = asendaja(new$new, "pankrotihaldur", "", replace = "pearaamatupidaja")

unique(leidur(new$new, "müüja", "kons"))
new$new = asendaja(new$new, "müüja", "konsu", "", excl1 = "puhastus", replace = "müüja")

unique(leidur(new$new, "müüja", "klienditeenind"))
new$new = asendaja(new$new, "müüja", "klienditeenind", "", replace = "müüja")
new$new = asendaja(new$new, "müüja", "vanem", "", excl1 = "laojuhataja", replace = "müüja")
new$new = asendaja(new$new, "müüja", "vastutav", "", replace = "müüja")
new$new = asendaja(new$new, "müüa", "", "", replace = "müüja")

unique(leidur(new$new, "müüja", "kaup"))
new$new = asendaja(new$new, "müüja", "kaup", "", excl1="juhataja-", excl2 = "maaletooja", replace = "müüja")
new$new = asendaja(new$new, "müüja", "poe", "", replace = "müüja")
new$new = asendaja(new$new, "klien", "teenin", "kaup", excl1="juht", excl2 = "omanik", excl3 = "juhataja", replace = "müüja")
new$new = asendaja(new$new, "klien", "teenin", "poe", excl1="juht", excl2 = "haldur", replace = "müüja")

unique(leidur(new$new, "massöör", ""))
new$new = asendaja(new$new, "massöör", "", excl1 = "toitumisn", excl2 = "aroomi", excl3 = "logistik", excl4 = "klienditoe", replace = "massöör")
new$new = asendaja(new$new, "massöö", "", excl1 = "toit", excl2 = "log",  excl3 = "klienditoe", replace = "massöör")
new$new = asendaja(new$new, "massaažiterapeut", "", excl1 = "meister", excl2 = "salongi", excl3 = "jooga", excl4 = "õppejõud", replace = "massöör")
new$new = asendaja(new$new, "massaaz", "", excl1 = "meister", replace = "massöör")
new$new = asendaja(new$new, "lümfiterapeut", "", replace = "massöör")

unique(leidur(new$new, "ilu", "teenin"))
new$new = asendaja(new$new, "ilu", "teenind", "", replace = "iluteenindaja")
new$new = asendaja(new$new, "kosmeetik", "", excl1 = "kosmeetika", excl2 = "auto", replace = "iluteenindaja")
new$new = asendaja(new$new, "iluteen", "", replace = "iluteenindaja")
new$new = asendaja(new$new, "küüneteh", "", excl1 = "õmblemine", replace = "iluteenindaja")
new$new = asendaja(new$new, "ripsme", "", replace = "iluteenindaja")
new$new = asendaja(new$new, "maniküü", "", replace = "iluteenindaja")
new$new = asendaja(new$new, "pediküü", "", replace = "iluteenindaja")
new$new = asendaja(new$new, "spa", "teenind", replace = "iluteenindaja")
new$new = asendaja(new$new, "jumest", "", replace = "iluteenindaja")
new$new = asendaja(new$new, "grim", "", replace = "iluteenindaja")
new$new = asendaja(new$new, "püsimeigi", "", replace = "iluteenindaja")
new$new = asendaja(new$new, "spaterapist", "", replace = "iluteenindaja")
new$new = asendaja(new$new, "kosmeto", "", replace = "iluteenindaja")

unique(leidur(new$new, "juuksur", ""))
new$new = asendaja(new$new, "juuksur", "", excl1 = "koerte", excl2 = "kutseõpetaja", excl3 = "vallav", replace = "juuksur")
new$new = asendaja(new$new, "juukse", "", replace = "juuksur")
new$new = asendaja(new$new, "barber", "", replace = "juuksur")

unique(leidur(new$new, "pastor", ""))
new$new = asendaja(new$new, "pastor", "", excl1 = "arendus", replace = "religioonispetsialist")
new$new = asendaja(new$new, "vaimulik", "", replace = "religioonispetsialist")
new$new = asendaja(new$new, "koguduse", "õpe", replace = "religioonispetsialist")
new$new = asendaja(new$new, "kiriku", "õpe", replace = "religioonispetsialist")
new$new = asendaja(new$new, "kaplan", "", excl1 = "valgus", replace = "religioonispetsialist")
new$new = asendaja(new$new, "preester", "", replace = "religioonispetsialist")
new$new = asendaja(new$new, "religioonispets", "", replace = "religioonispetsialist")

unique(leidur(new$new, "õpetaja", "algkool"))
new$new = asendaja(new$new, "õpetaja", "algkool", replace = "algkooliõpetaja")
new$new = asendaja(new$new, "õpetaja", "algklass", replace = "algkooliõpetaja")
new$new = asendaja(new$new, "õpetaja", "eelkool", replace = "algkooliõpetaja")

unique(leidur(new$new, "õpetaja", "põhikool"))
new$new = asendaja(new$new, "õpetaja", "põhikool", excl1 = "gümnaasium", replace = "põhikooliõpetaja")

unique(leidur(new$new, "kooli", "juht"))
new$new = asendaja(new$new, "koolijuht", "", excl1 = "õpetaja;", replace = "haridusasutustejuht")
new$new = asendaja(new$new, "haridusasut", "juht", excl1 = "haldus", excl2 = "it-juht", replace = "haridusasutustejuht")
new$new = asendaja(new$new, "dekanaadijuh", "", replace = "haridusasutustejuht")
new$new = asendaja(new$new, "gümn", "juh", replace = "haridusasutustejuht")

unique(leidur(new$new, "õpetaja", "alushar"))
new$new = asendaja(new$new, "õpetaja", "alushar", excl1 = "abi", replace = "lasteaiaõpetaja")
new$new = asendaja(new$new, "õpetaja", "lasteai", excl1 = "abi", excl2 = "assistent", excl3 = "kultuuri", excl4 = "puhastus", replace = "lasteaiaõpetaja")
new$new = asendaja(new$new, "öpetaja", "lastea", replace = "lasteaiaõpetaja")
new$new = asendaja(new$new, "ōpetaja", "lastea", replace = "lasteaiaõpetaja")
new$new = asendaja(new$new, "õpetaja", "sõim", replace = "lasteaiaõpetaja")
new$new = asendaja(new$new, "õpetaja", "rühm", replace = "lasteaiaõpetaja")

unique(leidur(new$new, "õpetaja", "koolieel"))
new$new = asendaja(new$new, "õpetaja", "koolieel", replace = "lasteaiaõpetaja")

unique(leidur(new$new, "lasteaia", "kasvataja", excl1 = "abi"))
new$new = asendaja(new$new, "lasteaia", "kasvataja", excl1 = "abi", replace = "lasteaiaõpetaja")

unique(leidur(new$new, "õpetaja", "keskkool"))
new$new = asendaja(new$new, "õpetaja", "keskkool", excl1 = "agronoom", excl2 = "põhi", replace = "gümnaasiumiõpetaja")
new$new = asendaja(new$new, "õpetaja", "gümn", excl1 = "põhi", replace = "gümnaasiumiõpetaja")
new$new = asendaja(new$new, "õpetaja", "kesk", "eri", replace = "gümnaasiumiõpetaja")

# koodides eristatakse huvikoolide keeleõpetajaid, aga ilmselt ei saa siin alati ka kindel olla, et kas töökoht on huvikool või üldkool (või mõlemad)
# jätsin praegu keeleõpetajaks, kuigi võtsin välja täiskasvanute keeleõpetajad, kes lähevad selgemalt selle huvikoolide teema alla (võimalik, et lõpuks võiks nad kõik kokku panna lihtsalt õpetajateks?)
unique(leidur(new$new, "õpetaja", "keel"))
new$new = asendaja(new$new, "õpetaja", "keel", excl1 = "tehnoloog", excl2 = "filoloog", excl3 = "täisk", excl4 = "paberi",  replace = "keeleõpetaja")
new$new = asendaja(new$new, "õpetaja", "inglis", excl1 = "täisk", replace = "keeleõpetaja")

new$new = asendaja(new$new, "huvikooliõpetaja", "", replace = "huvikooliõpetaja")
new$new = asendaja(new$new, "huviringi", "", replace = "huvikooliõpetaja")

# muusikakooli- ja muusikaõpetaja kokku panna? Või muusikakooliõpetajad huvikooliõpetajateks
unique(leidur(new$new, "õpetaja", "muusikakool"))
new$new = asendaja(new$new, "õpetaja", "muusikakool", replace = "muusikakooliõpetaja")
new$new = asendaja(new$new, "viiul", "õpetaja", excl1 = "erso", replace = "muusikakooliõpetaja")
new$new = asendaja(new$new, "klaver", "õpetaja", replace = "muusikakooliõpetaja")
new$new = asendaja(new$new, "kitarr", "õpetaja", replace = "muusikakooliõpetaja")
new$new = asendaja(new$new, "flöödi", "õpetaja", replace = "muusikakooliõpetaja")
new$new = asendaja(new$new, "kandle", "õpetaja", replace = "muusikakooliõpetaja")
new$new = asendaja(new$new, "solf", "õpetaja", replace = "muusikakooliõpetaja")
new$new = asendaja(new$new, "pilli", "õpetaja", replace = "muusikakooliõpetaja")
new$new = asendaja(new$new, "laul", "õpetaja", replace = "muusikakooliõpetaja")
new$new = asendaja(new$new, "instru", "õpetaja", replace = "muusikakooliõpetaja")

unique(leidur(new$new, "õpetaja", "muusika"))
new$new = asendaja(new$new, "õpetaja", "muusik", excl1 = "kooli", replace = "muusikaõpetaja")
new$new = asendaja(new$new, "muusikaõpetaja", "varemkoolis", replace = "muusikaõpetaja")

# tegelikult on ka "põhikooliõpetajate" ja "gümnaasiumiõpetajate" all mõni spetsiifilisema suunaga õpetaja (kui tahta erialade/ainete järgi analüüsida, siis võiks sealt ka vaadata)
unique(leidur(new$new, "õpetaja", "matem"))
new$new = asendaja(new$new, "õpetaja", "matem", replace = "reaalaineteõpetaja")
new$new = asendaja(new$new, "õpetaja", "keemia", replace = "reaalaineteõpetaja")
new$new = asendaja(new$new, "õpetaja", "füüsika", replace = "reaalaineteõpetaja")
new$new = asendaja(new$new, "õpetaja", "bioloogia", replace = "reaalaineteõpetaja")
new$new = asendaja(new$new, "õpetaja", "loodus", replace = "reaalaineteõpetaja")
new$new = asendaja(new$new, "õpetaja", "reaal", replace = "reaalaineteõpetaja")
new$new = asendaja(new$new, "õpetaja", "geogr", replace = "reaalaineteõpetaja")
new$new = asendaja(new$new, "õpetaja", "goegr", replace = "reaalaineteõpetaja")

unique(leidur(new$new, "õpetaja", "käsitöö"))
new$new = asendaja(new$new, "õpetaja", "käsitöö", replace = "käsitöö-kunstiõpetaja")
new$new = asendaja(new$new, "õpetaja", "kunst", replace = "käsitöö-kunstiõpetaja")
new$new = asendaja(new$new, "õpetaja", "kodund", replace = "käsitöö-kunstiõpetaja")
new$new = asendaja(new$new, "õpet", "käsitöö", replace = "käsitöö-kunstiõpetaja")
new$new = asendaja(new$new, "õpet", "puut", replace = "käsitöö-kunstiõpetaja")
new$new = asendaja(new$new, "õpet", "keraa", replace = "käsitöö-kunstiõpetaja")

unique(leidur(new$new, "õpetaja", "õpiabi"))
new$new = asendaja(new$new, "õpetaja", "õpiabi", replace = "eripedagoog")
new$new = asendaja(new$new, "õpetaja", "tugi", replace = "eripedagoog")
new$new = asendaja(new$new, "õpetaja", "erivaj", replace = "eripedagoog")
new$new = asendaja(new$new, "õpetaja", "puue", replace = "eripedagoog")
new$new = asendaja(new$new, "hevkoordi", "", replace = "eripedagoog")
new$new = asendaja(new$new, "haridusliku", "koord", replace = "eripedagoog")
new$new = asendaja(new$new, "tugiteen", "koord", replace = "eripedagoog")
new$new = asendaja(new$new, "lasteasutus", "koord", replace = "eripedagoog")
new$new = asendaja(new$new, "eriõpetaja", "", excl1 = "logopeed", excl2 = "üliõp", replace = "eripedagoog")

unique(leidur(new$new, "õpetaja", "abi", "lasteaia"))
new$new = asendaja(new$new, "õpetaja", "abi", "lasteaia", excl1 = "puidu", excl2 = "giid", replace = "abiõpetaja")
new$new = asendaja(new$new, "kasvataja", "abi", replace = "abiõpetaja")
new$new = asendaja(new$new, "õpetaja", "abi", excl1 = "puidu", excl2 = "giid", excl3 = "tegevusterapeut", excl4 = "kokk", replace = "abiõpetaja")
new$new = asendaja(new$new, "lasteaia", "assis", replace = "abiõpetaja")
new$new = asendaja(new$new, "öpetaja", "abi", replace = "abiõpetaja")
new$new = asendaja(new$new, "õp.abi", "", replace = "abiõpetaja")
new$new = asendaja(new$new, "lasteaias", "abi", replace = "abiõpetaja")
new$new = asendaja(new$new, "õppeabi", "", replace = "abiõpetaja")

new$new = asendaja(new$new, "ōpetaja", "", replace = "õpetaja")
new$new = asendaja(new$new, "öpetaja", "", replace = "õpetaja")
new$new = asendaja(new$new, "ôpetaja", "", replace = "õpetaja")
new$new = asendaja(new$new, "^pedagoog$", "", replace = "õpetaja")

unique(leidur(new$new, "õpetaja", "muuseum"))
new$new = asendaja(new$new, "õpet", "muuseum", replace = "muuseumipedagoog")

unique(leidur(new$new, "ehitus", "tööline"))
new$new = asendaja(new$new, "ehitus", "tööline", excl1 = "laot", excl2 = "abi", replace = "ehitustööline")
new$new = asendaja(new$new, "kaevandus", "tööline", excl1 = "laot", excl2 = "abi", replace = "ehitustööline")

unique(leidur(new$new, "ehitus", "tööde", "juh"))
new$new = asendaja(new$new, "ehitus", "tööde", "juh", excl1 = "fie", replace = "ehitustöödejuhataja")
new$new = asendaja(new$new, "ehitus", "obje", "juh", replace = "ehitustöödejuhataja")
new$new = asendaja(new$new, "ehitus", "brig", replace = "ehitustöödejuhataja")

# panin ehitustöödejuhatajate alla ka elektritöödejuhatajad jms - ISCO kirjelduse järgi peaks sobima
unique(leidur(new$new, "töödejuhataja", ""))
new$new = asendaja(new$new, "töödejuhataja", "elektri", replace = "ehitustöödejuhataja")
new$new = asendaja(new$new, "töödejuhataja", "ehit", replace = "ehitustöödejuhataja")

# töödejuhatajaid ka mitmetes valdkondades (kaevandus/tööstus/ehitus)
new$new = asendaja(new$new, "töödejuhat", "tootm", replace = "tootmistöödejuhataja")
new$new = asendaja(new$new, "meister", "tootm", replace = "tootmistöödejuhataja")
new$new = asendaja(new$new, "koordin", "tootm", excl1 = "tellimus", replace = "tootmistöödejuhataja")

# ilma täpsustuseta töödejuhatajaid kõige rohkem (N = 120) - kõik töödejuhatajd kokku panna?
table(leidur(new$new, "töödejuhat", ""))

unique(leidur(new$new, "puu", "sepp"))
new$new = asendaja(new$new, "puu", "sepp", excl1 = "üldehi", excl2 = "ehitaja.", excl3 = "ehitaja-", replace = "puusepp")

unique(leidur(new$new, "politseinik", ""))
new$new = asendaja(new$new, "politseinik", "", replace = "politseinik")
new$new = asendaja(new$new, "politsei", "ametnik", replace = "politseinik")
new$new = asendaja(new$new, "politsei", "patrull", replace = "politseinik")
new$new = asendaja(new$new, "politsei", "krinaal", replace = "politseinik")

unique(leidur(new$new, "valla", "sekretär"))
new$new = asendaja(new$new, "valla", "sekretär", excl1 = "abi", replace = "kõrgem valitsusametnik")
new$new = asendaja(new$new, "linna", "sekretär", replace = "kõrgem valitsusametnik")
new$new = asendaja(new$new, "vallavanem", "", replace = "kõrgem valitsusametnik")
new$new = asendaja(new$new, "linnapea", "", excl1 = "abilinnapeaabi", replace = "kõrgem valitsusametnik")
new$new = asendaja(new$new, "kantsler", "", replace = "kõrgem valitsusametnik")
new$new = asendaja(new$new, "kohaliku", "omavalits", "juhataja", replace = "kõrgem valitsusametnik")
new$new = asendaja(new$new, "ministeer", "juhataja", replace = "kõrgem valitsusametnik")
new$new = asendaja(new$new, "kohtu", "esimees", replace = "kõrgem valitsusametnik")
new$new = asendaja(new$new, "politsei", "peadir", replace = "kõrgem valitsusametnik")
new$new = asendaja(new$new, "prefekt", "", replace = "kõrgem valitsusametnik")
new$new = asendaja(new$new, "kõrgemvalitsusametnik", "", replace = "kõrgem valitsusametnik")

unique(leidur(new$new, "jurist", "kohtu"))
new$new = asendaja(new$new, "jurist", "kohtu", replace = "advokaat-prokurör")
new$new = asendaja(new$new, "advoka", "", replace = "advokaat-prokurör")
new$new = asendaja(new$new, "prokurör", "", replace = "advokaat-prokurör")

unique(leidur(new$new, "ajakirjanik", ""))
new$new = asendaja(new$new, "ajakirjani", "", excl1 = "õpetaja", excl2 = "pressiesi", replace = "ajakirjanik")

unique(leidur(new$new, "ajal", "toimet"))
new$new = asendaja(new$new, "ajal", "toimet", replace = "ajakirjanik")
new$new = asendaja(new$new, "ajak", "toimet", excl1 = "raamatu", replace = "ajakirjanik")
new$new = asendaja(new$new, "reporter", "", replace = "ajakirjanik")
new$new = asendaja(new$new, "saatejuht", "", replace = "ajakirjanik")

unique(leidur(new$new, "programm", "veebi"))
new$new = asendaja(new$new, "programm", "veebi", replace = "veebiprogrammeerija")
new$new = asendaja(new$new, "programm", "web", replace = "veebiprogrammeerija")
new$new = asendaja(new$new, "develop", "web", replace = "veebiprogrammeerija")
new$new = asendaja(new$new, "veebi", "arend", excl1 = "juht", replace = "veebiprogrammeerija")
new$new = asendaja(new$new, "seospe", "", replace = "veebiprogrammeerija")

unique(leidur(new$new, "programmeerija", ""))
new$new = asendaja(new$new, "programmeerija", "", excl1 = "cnc", excl2 = "veebi", excl3 = "automaatika", excl4 = "matemaatik", replace = "programmeerija")
new$new = asendaja(new$new, "tarkvara", "insener", replace = "programmeerija")
new$new = asendaja(new$new, "^programeerija$", "", replace = "programmeerija")
new$new = asendaja(new$new, "tarkvararaendaja", "", replace = "programmeerija")

unique(leidur(new$new, "tarkvara", "arhitekt"))
new$new = asendaja(new$new, "tarkvara", "arhitekt", replace = "programmeerija")
new$new = asendaja(new$new, "tarkvara", "arendaja", replace = "programmeerija")
new$new = asendaja(new$new, "tarkvara", "arend", excl1 = "juht", excl2 = "juhin", replace = "programmeerija")
new$new = asendaja(new$new, "infoteh", "arend", excl1 ="juht", replace = "programmeerija")
new$new = asendaja(new$new, "itarend", "spets", excl1 ="juht", replace = "programmeerija")
new$new = asendaja(new$new, "it-arend", "spets", excl1 ="juht", replace = "programmeerija")
new$new = asendaja(new$new, "it-", "arendaja", replace = "programmeerija")
new$new = asendaja(new$new, "front", "arendaja", replace = "programmeerija")
new$new = asendaja(new$new, "front", "devel", replace = "programmeerija")
new$new = asendaja(new$new, "itarendaja", "", replace = "programmeerija")
new$new = asendaja(new$new, "java", "arendaja", replace = "programmeerija")
new$new = asendaja(new$new, "python", "arendaja", replace = "programmeerija")
new$new = asendaja(new$new, "devops", "", replace = "programmeerija")
new$new = asendaja(new$new, "full", "arendaja", replace = "programmeerija")
new$new = asendaja(new$new, "analüütik", "arendaja", replace = "programmeerija")
new$new = asendaja(new$new, "andmeinsener", "", replace = "programmeerija")
new$new = asendaja(new$new, "software", "engi", excl1 = "lead", excl2 = "manag", excl3 = "quality", replace = "programmeerija")
new$new = asendaja(new$new, "software", "develop", excl1 = "lead", excl2 = "manag", excl3 = "quality", replace = "programmeerija")
# kas lihtsalt "arendaja" ka siia alla? 

unique(leidur(new$new, "süsteem", "arhitekt"))
new$new = asendaja(new$new, "süsteem", "arhitekt", replace = "süsteemianalüütik")
new$new = asendaja(new$new, "süsteem", "analüütik", excl1 = "elektri", excl2 = "turundus", replace = "süsteemianalüütik")
new$new = asendaja(new$new, "äri", "analüütik", "it", replace = "süsteemianalüütik")
new$new = asendaja(new$new, "projektijuhtitvaldkonnas", "", replace = "süsteemianalüütik")
new$new = asendaja(new$new, "projektijuhtits", "", replace = "süsteemianalüütik")
new$new = asendaja(new$new, "projektijuht", "tarkvara", replace = "süsteemianalüütik")
new$new = asendaja(new$new, "itprojektijuht", "", replace = "süsteemianalüütik")
new$new = asendaja(new$new, "it-", "projektijuht", replace = "süsteemianalüütik")
new$new = asendaja(new$new, "itkonsult", "", replace = "süsteemianalüütik")
new$new = asendaja(new$new, "tarkvara", "kons", excl1 = "müügi", replace = "süsteemianalüütik")

unique(leidur(new$new, "tooteomanik", "it"))
new$new = asendaja(new$new, "tooteomanik", "it", excl1 = "andmekaitse", replace = "süsteemianalüütik")
new$new = asendaja(new$new, "ittootejuht", "", replace = "süsteemianalüütik")

unique(leidur(new$new, "liini", "töö"))
new$new = asendaja(new$new, "liini", "töö", excl1 = "kliin", excl2 = "operaator", excl3 = "montöör", replace = "tööstuselihttööline")

unique(leidur(new$new, "bussijuht", ""))
new$new = asendaja(new$new, "bussijuht", "", excl1 = "õpet", excl2 = "auto", excl3 = "omanik", replace = "bussijuht")
new$new = asendaja(new$new, "trammijuht", "", replace = "bussijuht")
new$new = asendaja(new$new, "trolli", "", excl1 = "kontroll", replace = "bussijuht")

# alustatud spetsiifilisemast, nt kaugsõidu-rahvusvahelised-autojuhid eraldi, taksojuhid eraldi -- psühholoogiselt erinev töö?
new$new = asendaja(new$new, "autojuht", "kaug", replace = "kaugsõiduautojuht")
new$new = asendaja(new$new, "rekka", "juht", replace = "kaugsõiduautojuht")
new$new = asendaja(new$new, "pikamaa", "juht", replace = "kaugsõiduautojuht")
new$new = asendaja(new$new, "rahvusvahel", "autojuht", replace = "kaugsõiduautojuht")
new$new = asendaja(new$new, "riikidevahel", "autojuht", replace = "kaugsõiduautojuht")
new$new = asendaja(new$new, "rahvusv", "veo", replace = "kaugsõiduautojuht")
new$new = asendaja(new$new, "kaugsõidu", "juht", replace = "kaugsõiduautojuht")
new$new = asendaja(new$new, "euroopaveo", "juht", replace = "kaugsõiduautojuht")

unique(leidur(new$new, "autojuht", "veo"))
new$new = asendaja(new$new, "autojuht", "veo", excl1 = "metsa", replace = "veoautojuht")
new$new = asendaja(new$new, "autojuht", "betooni", replace = "veoautojuht")
new$new = asendaja(new$new, "juht", "veoki", replace = "veoautojuht")

new$new = asendaja(new$new, "autojuht-", "", replace = "autojuht")
new$new = asendaja(new$new, "autojuht", "kutse", excl1 = "rasket", replace = "autojuht")
new$new = asendaja(new$new, "taksojuht", "", replace = "autojuht")

unique(leidur(new$new, "traktori", ""))
new$new = asendaja(new$new, "traktori", "", excl1 = "auto", excl2 = "raamatupidaja", excl3 = "hoold", excl4 = "ensvaja", replace = "põllujametsamasinajuht")
new$new = asendaja(new$new, "harvester", "", replace = "põllujametsamasinajuht")
new$new = asendaja(new$new, "põllumasinatejuht", "", replace = "põllujametsamasinajuht")

unique(leidur(new$new, "metsa", "auto"))
new$new = asendaja(new$new, "metsa", "auto", replace = "põllujametsamasinajuht")
new$new = asendaja(new$new, "metsa", "masin", excl1 = "hooldus", replace = "põllujametsamasinajuht")
new$new = asendaja(new$new, "forvarder", "", replace = "põllujametsamasinajuht")
new$new = asendaja(new$new, "põllumajandusmas", "", replace = "põllujametsamasinajuht")
new$new = asendaja(new$new, "põllu", "operaat", replace = "põllujametsamasinajuht")
new$new = asendaja(new$new, "forvaaterioperaator", "", replace = "põllujametsamasinajuht")

unique(leidur(new$new, "buldooseri", ""))
new$new = asendaja(new$new, "buldooseri", "", replace = "teemasinajuht")
new$new = asendaja(new$new, "ekskavaator", "", replace = "teemasinajuht")
new$new = asendaja(new$new, "kopa", "juht", replace = "teemasinajuht")
new$new = asendaja(new$new, "laadurijuht", "", excl1 = "logis", replace = "teemasinajuht")
new$new = asendaja(new$new, "teerullijuht", "", replace = "teemasinajuht")
new$new = asendaja(new$new, "laotur", "", replace = "teemasinajuht")
new$new = asendaja(new$new, "autojuht", "laadur", replace = "teemasinajuht")
new$new = asendaja(new$new, "oper", "laadur", replace = "teemasinajuht")
new$new = asendaja(new$new, "oper", "masin", "tee", replace = "teemasinajuht")
new$new = asendaja(new$new, "asfal", "", excl1 = "juhataja", excl2 = "tööline", replace = "teemasinajuht")
new$new = asendaja(new$new, "oper", "greider", replace = "teemasinajuht")
new$new = asendaja(new$new, "teemasinajuht", "", replace = "teemasinajuht")

unique(leidur(new$new, "tõstukijuht", ""))
new$new = asendaja(new$new, "tõstukijuht", "", excl1 = "treial", excl2 = "kuivati", excl3 = "mehaan", excl4 = "lao", replace = "tõstukijuht")

unique(leidur(new$new, "ettekandja", ""))
new$new = asendaja(new$new, "ettekandja", "", replace = "ettekandja")
new$new = asendaja(new$new, "baaridaam", "", excl1 = "juhataja", excl2 = "lillemüüja", replace = "ettekandja")
new$new = asendaja(new$new, "kelner", "", replace = "ettekandja")
new$new = asendaja(new$new, "baarm", "", excl1 = "ehitaja", replace = "ettekandja")
new$new = asendaja(new$new, "barman", "", excl1 = "manager", replace = "ettekandja")
new$new = asendaja(new$new, "kohvik", "teenin", excl1 = "kokk", excl2 = "omanik", replace = "ettekandja")
new$new = asendaja(new$new, "restoran", "teenin", excl1 = "juht", replace = "ettekandja")
new$new = asendaja(new$new, "toitlustus", "teenin", excl1 = "juht", replace = "ettekandja")
new$new = asendaja(new$new, "barista", "", excl1 = "juht", replace = "ettekandja")
new$new = asendaja(new$new, "sushibaar", "", excl1 = "juht", replace = "ettekandja")

unique(leidur(new$new, "tõlkija", ""))
unique(leidur(new$new, "keele", "toimetaja"))
new$new = asendaja(new$new, "tõlkija", "", replace = "filoloog")
new$new = asendaja(new$new, "keele", "toimetaja", replace = "filoloog")
new$new = asendaja(new$new, "tekst", "toimetaja", replace = "filoloog")
new$new = asendaja(new$new, "keel", "tead", replace = "filoloog")
new$new = asendaja(new$new, "filoloog", "", replace = "filoloog")

# sotsiaaltöötajaid on mitmes kategoorias - on sotsiaaltöötajad/tippspetsialistid ja keskastmespetsialistid (sotsiaaltööspetsialistid ISCO nimetuste kaudu)
# lastekaitsjatega on eriti keeruline - tippspetsialist (sotsiaaltöötajatega ühes) on "lastekaitse tippspetsialist" ja keskastmes on "lastekaitsespetsialist"
unique(leidur(new$new, "sotsiaaltöötaja", ""))
new$new = asendaja(new$new, "sotsiaaltöötaja", "", excl1 = "tegevuster", excl2 = "raamatupidaja", excl3 = "eraettevõtja", replace = "sotsiaaltöötaja")
unique(leidur(new$new, "sotsiaaltöö", "spetsialist"))
new$new = asendaja(new$new, "sotsiaaltöö", "spetsialist", excl1 = "huvijuht", replace = "sotsiaaltööspetsialist")
new$new = asendaja(new$new, "usaldusisik", "", replace = "sotsiaaltööspetsialist")

new$new = asendaja(new$new, "laste", "kaitse",replace = "sotsiaaltöötaja")
new$new = asendaja(new$new, "sotsiaalnõunik", "",replace = "sotsiaaltöötaja")
new$new = asendaja(new$new, "kriminaalhooldus", "",replace = "sotsiaaltöötaja")
new$new = asendaja(new$new, "sotsiaal", "peda", excl1 = "õpetaja", excl2 = "juht", replace = "sotsiaaltöötaja")
new$new = asendaja(new$new, "sotsiaal", "nõu", replace = "sotsiaaltöötaja")
new$new = asendaja(new$new, "kogemus", "nõu", replace = "sotsiaaltöötaja")
new$new = asendaja(new$new, "juhtumikorr", "", replace = "sotsiaaltöötaja")
new$new = asendaja(new$new, "sotsiaaltööjuht", "", replace = "sotsiaaltöötaja")
new$new = asendaja(new$new, "pere", "nõustaja", replace = "sotsiaaltöötaja")
new$new = asendaja(new$new, "sotsiaaltöötaja-", "", replace = "sotsiaaltöötaja")
new$new = asendaja(new$new, "sotsiaalõpetaja", "", replace = "sotsiaaltöötaja")
new$new = asendaja(new$new, "noortetööjuht", "", replace = "sotsiaaltöötaja")

unique(leidur(new$new, "müügisekretär", ""))
new$new = asendaja(new$new, "müügisekretär", "", excl1 = "juhiabi", replace = "raamatupidamisekontoritöötaja")

unique(leidur(new$new, "müügi", "tele"))
new$new = asendaja(new$new, "müügi", "tele", replace = "telefonimüüja")
new$new = asendaja(new$new, "internetimüüja", "", replace = "telefonimüüja")
new$new = asendaja(new$new, "telefon", "müü", replace = "telefonimüüja")
new$new = asendaja(new$new, "telemarket", "", replace = "telefonimüüja")

unique(leidur(new$new, "riigiametnik", ""))
new$new = asendaja(new$new, "riigiametnik", "", replace = "riigiametnik")
new$new = asendaja(new$new, "riigiteenistuja", "", replace = "riigiametnik")

unique(leidur(new$new, "eripedagoog", ""))
new$new = asendaja(new$new, "eripedagoog", "", excl1 = "logopeed", replace = "eripedagoog")
new$new = asendaja(new$new, "eriõpetaja", "", excl1 = "üliõpilane", replace = "eripedagoog")

unique(leidur(new$new, "logopeed", ""))
new$new = asendaja(new$new, "logopeed", "", excl1 = "eripedagoog-", replace = "logopeed")

unique(leidur(new$new, "füsiotera", ""))
new$new = asendaja(new$new, "füsiotera", "", excl1 = "treener", excl2 = "õpin", replace = "füsioterapeut")
new$new = asendaja(new$new, "füsioterapeut;", "", replace = "füsioterapeut")
new$new = asendaja(new$new, "füsioterepeut;", "", replace = "füsioterapeut")

unique(leidur(new$new, "müügispetsialist", ""))
new$new = asendaja(new$new, "müügispets", "", excl1 = "juht", replace = "müügiesindaja")
new$new = asendaja(new$new, "turundus", "spetsial", excl1 = "juht", excl2 = "kommunikatsioonija", replace = "müügiesindaja")
new$new = asendaja(new$new, "müügi", "esindaja", excl1 = "meditsiini", excl2 = "agronoom", excl3 = "maakler", excl4 = "logistik", replace = "müügiesindaja")
new$new = asendaja(new$new, "müügi", "esindaja-", replace = "müügiesindaja")
new$new = asendaja(new$new, "müügi", "mees", excl1 = "juht", replace = "müügiesindaja")
new$new = asendaja(new$new, "müügi", "insener", replace = "müügiesindaja")
new$new = asendaja(new$new, "myygi", "spets", replace = "müügiesindaja")
new$new = asendaja(new$new, "müügi", "koordi", replace = "müügiesindaja")
new$new = asendaja(new$new, "e-poe", "koordi", replace = "müügiesindaja")
new$new = asendaja(new$new, "e-kauba", "koordi", replace = "müügiesindaja")
new$new = asendaja(new$new, "tootespetsialist", "", replace = "müügiesindaja")

unique(leidur(new$new, "sekretär", "asja"))
new$new = asendaja(new$new, "sekretär", "asja", replace = "juhiabi")
new$new = asendaja(new$new, "juhiabi", "sekretär", replace = "juhiabi")
new$new = asendaja(new$new, "juhiabi-", "", replace = "juhiabi")
new$new = asendaja(new$new, "referent", "", excl1 = "labor", replace = "juhiabi")
new$new = asendaja(new$new, "asjaajaja", "", excl1 = "raamatup", excl2 = "müügijuht", excl3 = "autojuht", replace = "juhiabi")

unique(leidur(new$new, "analüütik", "finants"))
new$new = asendaja(new$new, "analüütik", "finants", replace = "finantsanalüütik")
new$new = asendaja(new$new, "arhitekt", "finants", replace = "finantsanalüütik")
new$new = asendaja(new$new, "krediidianalüütik", "", replace = "finantsanalüütik")

unique(leidur(new$new, "analüütik", "tarkvara"))
new$new = asendaja(new$new, "analüütik", "tarkvara", excl1 = "müük", replace = "programmeerija")
new$new = asendaja(new$new, "analüütik", "it-", replace = "programmeerija")
new$new = asendaja(new$new, "itanalüütik", "", replace = "programmeerija")

unique(leidur(new$new, "analüütik", "andmebaasi"))
new$new = asendaja(new$new, "analüütik", "andmebaasi", replace = "andmebaasianalüütik")
new$new = asendaja(new$new, "administ", "andmebaasi", replace = "andmebaasianalüütik")
new$new = asendaja(new$new, "haldur", "andmebaasi", replace = "andmebaasianalüütik")
new$new = asendaja(new$new, "arendaja", "andmebaasi", replace = "andmebaasianalüütik")
new$new = asendaja(new$new, "arhitekt", "andmebaasi", replace = "andmebaasianalüütik")
new$new = asendaja(new$new, "haldur", "andme", replace = "andmebaasianalüütik")
new$new = asendaja(new$new, "spets", "andme", replace = "andmebaasianalüütik")

unique(leidur(new$new, "analüütik", "bio"))
new$new = asendaja(new$new, "analüütik", "bio", replace = "bioanalüütik")
new$new = asendaja(new$new, "med", "laborant", replace = "bioanalüütik")
new$new = asendaja(new$new, "med", "labor", excl1 = "juh", replace = "bioanalüütik")
new$new = asendaja(new$new, "hiir", "labor", excl1 = "juh", replace = "bioanalüütik")
new$new = asendaja(new$new, "laborianalüütik", "", replace = "bioanalüütik")

# tegelikult saab neid sõjaväelasi ka erinevatesse astmetesse jagada, aga olen ka mõelnud, et ehk üks sõjaväelaste grupp oleks mõistlik
unique(leidur(new$new, "sõjaväelane", ""))
new$new = asendaja(new$new, "sõjaväelane", "", replace = "sõjaväelane")
new$new = asendaja(new$new, "sõdur", "", replace = "sõjaväelane")
new$new = asendaja(new$new, "tegevväelane", "", replace = "sõjaväelane")
new$new = asendaja(new$new, "kaitseväelane", "", excl1 = "hambaravi", excl2 = "elektrik", replace = "sõjaväelane")

unique(leidur(new$new, "ohvitser", ""))
new$new = asendaja(new$new, "ohvitser", "vanem", replace = "sõjaväelane")
new$new = asendaja(new$new, "ohvitser", "all", replace = "sõjaväelane")
new$new = asendaja(new$new, "ohvitser", "staabi", replace = "sõjaväelane")
new$new = asendaja(new$new, "ohvitser", "väe", replace = "sõjaväelane")
new$new = asendaja(new$new, "kaitsevägi", "", replace = "sõjaväelane")

unique(leidur(new$new, "kaitselii", ""))
new$new = asendaja(new$new, "kaitselii", "", replace = "sõjaväelane")

unique(leidur(new$new, "", "kapten"))
new$new = asendaja(new$new, "kapten", "", excl1 = "erias", excl2 = "lennu", excl3 = "ettevõtte", replace = "kapten-laevajuht")
new$new = asendaja(new$new, "laevajuht", "", replace = "kapten-laevajuht")
new$new = asendaja(new$new, "loots", "", replace = "kapten-laevajuht")
new$new = asendaja(new$new, "tüürimees", "", replace = "kapten-laevajuht")

unique(leidur(new$new, "lasteaia", "juh"))
new$new = asendaja(new$new, "lasteaia", "juht", replace = "lasteaiajuht")
new$new = asendaja(new$new, "lasteaia", "juhataja", excl1 = "majandus", replace = "lasteaiajuht")
new$new = asendaja(new$new, "laste", "õppejuht", replace = "lasteaiajuht")
new$new = asendaja(new$new, "alus", "õppejuht", replace = "lasteaiajuht")

new$new = asendaja(new$new, "lasteaia", "juhataja", "majandus", replace = "majandusjuhataja")
new$new = asendaja(new$new, "majandusjuhataja", "", replace = "majandusjuhataja")
new$new = asendaja(new$new, "maj.alajuh", "", replace = "majandusjuhataja")
new$new = asendaja(new$new, "majandus-juha", "", replace = "majandusjuhataja")

new$new = asendaja(new$new, "õppeala", "juhataja",replace = "haridusasutustejuht")
new$new = asendaja(new$new, "õppejuht", "",replace = "haridusasutustejuht")
new$new = asendaja(new$new, "õppetööjuht", "",replace = "haridusasutustejuht")
new$new = asendaja(new$new, "õppetooli", "",replace = "haridusasutustejuht")
new$new = asendaja(new$new, "instituudi", "juh",replace = "haridusasutustejuht")
new$new = asendaja(new$new, "õppe", "juh", excl1 = "juhtivõp", excl2 = "õppejõud", excl3 = "toitlustus", excl4 = "dotsent",  replace = "haridusasutustejuht")

table(leidur(new$new, "hooldaja", "")) %>% sort
new$new = asendaja(new$new, "hooldaja", "haigla", excl1 = "kliendi", excl2 = "lasteaia", replace = "hooldaja")
new$new = asendaja(new$new, "hooldaja", "sotsiaal",replace = "hooldaja")
new$new = asendaja(new$new, "hooldaja", "omaste",replace = "hooldaja")
new$new = asendaja(new$new, "hooldaja", "hooldekodu",replace = "hooldaja")
new$new = asendaja(new$new, "põetaja", "", replace = "hooldaja")
new$new = asendaja(new$new, "hooldus", "õde", replace = "hooldaja")
new$new = asendaja(new$new, "hoold", "töötaja", excl1 = "tee", excl2 = "turva", replace = "hooldaja")
new$new = asendaja(new$new, "hooldaja", "kodu", replace = "hooldaja")
new$new = asendaja(new$new, "hooldaja", "abi", replace = "hooldaja")
new$new = asendaja(new$new, "hooldaja", "inim", replace = "hooldaja")
new$new = asendaja(new$new, "hooldaja", "pere", replace = "hooldaja")
new$new = asendaja(new$new, "hooldaja", "ema", replace = "hooldaja")
new$new = asendaja(new$new, "hooldaja", "isiklik", replace = "hooldaja")
new$new = asendaja(new$new, "lähihoi", "", replace = "hooldaja")
new$new = asendaja(new$new, "isiklikabistaja", "", replace = "hooldaja")

new$new = asendaja(new$new, "hooldaja", "hobu", replace = "loomahooldaja")
new$new = asendaja(new$new, "hooldaja", "looma", replace = "loomahooldaja")
new$new = asendaja(new$new, "talitaja", "", replace = "loomahooldaja")
new$new = asendaja(new$new, "loomaarsti", "abi", replace = "loomahooldaja")

new$new = asendaja(new$new, "veter", "abi", replace = "loomahooldaja")
new$new = asendaja(new$new, "uluki", "hoold", replace = "loomahooldaja")

unique(leidur(new$new, "kuller", ""))
new$new = asendaja(new$new, "kuller", "", excl1 = "toidu", excl2 = "ratta", excl3 = "raamatu", replace = "postiljon-kuller")
new$new = asendaja(new$new, "kirjakandja", "",replace = "postiljon-kuller")
new$new = asendaja(new$new, "postiljon", "",replace = "postiljon-kuller")
new$new = asendaja(new$new, "postitöötleja", "",replace = "postiljon-kuller")
new$new = asendaja(new$new, "lehekandja", "",replace = "postiljon-kuller")

## toidukullerile ei olegi nagu head vastet - on olemas "rattakuller" (veonduse lihttöölise valdkond), samas kõik toidukullerid pole rattaga
## tavaline "kuller" on aga posti kättetoimetajate valdkonnas, mis ka justkui toidule ei sobi? Samas neid vist liiga palju ka ei ole, et lõputult pead murda

unique(leidur(new$new, "ehitus", "tööline"))
new$new = asendaja(new$new, "ehitus", "tööline", excl1 = "abi", excl2 = "lao", replace = "ehitaja")
new$new = asendaja(new$new, "ehitusvaldkond", "",replace = "ehitaja")
new$new = asendaja(new$new, "üldehitaja", "", excl1 = "keevitaja", replace = "ehitaja")
new$new = asendaja(new$new, "^ehitus$", "", replace = "ehitaja")

# siin jaotatakse neid ehitajaid veel mitmetesse gruppidesse - ilmselt oleks mõistlik midagi ühendada, nt ehitajad ja ehitusviimistlejad?
new$new = asendaja(new$new, "ehitusviimistleja", "",replace = "ehitusviimistleja")
new$new = asendaja(new$new, "siseviimistl", "",replace = "ehitusviimistleja")
new$new = asendaja(new$new, "krohv", "",replace = "ehitusviimistleja")
new$new = asendaja(new$new, "pahteldaja", "",replace = "ehitusviimistleja")
new$new = asendaja(new$new, "plaati", "", excl1 = "trükk", excl2 = "valmistamine", replace = "ehitusviimistleja")
new$new = asendaja(new$new, "katuse", "", excl1 = "keevitaja", excl2 = "projektijuht", excl3 = "prekksepp", replace = "ehitusviimistleja")

unique(leidur(new$new, "san", "tehnik"))
new$new = asendaja(new$new, "san", "tehnik", excl1 = "juhat", excl2 = "mets", excl3 = "keevitaja", replace = "torulukksepp")
new$new = asendaja(new$new, "toru", "lukksepp", replace = "torulukksepp")
new$new = asendaja(new$new, "torumees", "", replace = "torulukksepp")

new$new = asendaja(new$new, "maaler", "", excl1 = "auto", replace = "ehitusmaaler")

table(leidur(new$new, "tegevjuht", "")) %>% sort
new$new = asendaja(new$new, "tegevjuht", "ettev",replace = "tegevjuht")

# personalijuhte on mitmel tasemel (oleneb, kas on juht alluvatega või ilma jne, vaatasin enne selle work_position_name kaudu) - võib-olla kõik kokku panna ja öelda, et mis koodidega personalijuhid ISCO-s on?
# sest ega inimesed ise ka ei tea vahel, kas nad on tipp- või keskastmespetsialistid jne
unique(leidur(new$new, "personalijuht", ""))
new$new = asendaja(new$new, "personalijuht", "", excl1 = "finants", excl2 = "büroo", excl3 = "kvaliteedi", excl4 = "jurist", replace = "personalijuht")
new$new = asendaja(new$new, "personalipartner", "", replace = "personalijuht")
new$new = asendaja(new$new, "hr", "", excl1 = "support", excl2 = "müügi", excl3 = "kassa", replace = "personalijuht")
new$new = asendaja(new$new, "personali-", "juht", replace = "personalijuht")
new$new = asendaja(new$new, "personali", "juhataja", replace = "personalijuht")
new$new = asendaja(new$new, "värbamisjuh", "", replace = "personalijuht")

# psühholooge mitme koodiga, aga võib-olla mõistlik kõik kokku panna ja jälle ära näidata, kes gruppi kuuluvad?
unique(leidur(new$new, "psühho", "",))
new$new = asendaja(new$new, "psühho", "", excl1 = "õpetaja", excl2 = "koorijuht", replace = "psühholoog")

unique(leidur(new$new, "finantsjuht", ""))
new$new = asendaja(new$new, "finantsjuht", "", excl1 = "tarkvara", excl2 = "ettevõttejuht", excl3 = "praktika", replace = "finantsjuht")
new$new = asendaja(new$new, "finantsjuh", "", excl1 = "abi", excl2 = "assis", excl3 = "praktikal", excl4 = "ettevõtte", replace = "finantsjuht")
new$new = asendaja(new$new, "cfo", "", replace = "finantsjuht")
new$new = asendaja(new$new, "finansjuht", "", replace = "finantsjuht")

unique(leidur(new$new, "turundusjuh", "",))
new$new = asendaja(new$new, "turundusjuh", "", excl1 = "abi", replace = "turundusjuht")
new$new = asendaja(new$new, "turundus-", "juht", excl1 = "ettevõtja", replace = "turundusjuht")
new$new = asendaja(new$new, "turundu-", "juht", excl1 = "ettevõtja", replace = "turundusjuht")

new$new = asendaja(new$new, "maja", "haldur", replace = "kinnisvarahaldur")
new$new = asendaja(new$new, "kinnisvara", "haldur", replace = "kinnisvarahaldur")
new$new = asendaja(new$new, "korteriühistu", "esi", replace = "kinnisvarahaldur")
new$new = asendaja(new$new, "kinnisvara", "maakler", excl1 = "raamatupidaja", excl2 = "jurist", replace = "kinnisvarahaldur")
new$new = asendaja(new$new, "küjuh", "", replace = "kinnisvarahaldur")
new$new = asendaja(new$new, "kinnisvaraspetsialist", "", replace = "kinnisvarahaldur")
new$new = asendaja(new$new, "60korterigaelamujuhatusejuht", "", replace = "kinnisvarahaldur")
new$new = asendaja(new$new, "korteriüh", "", excl1 = "erivaja", excl2 = "fie", replace = "kinnisvarahaldur")

new$new = asendaja(new$new, "kinnisvara", "hoold", replace = "haldustöötaja")
new$new = asendaja(new$new, "ruum", "hoold", replace = "haldustöötaja")

new$new = asendaja(new$new, "piloot", "", replace = "piloot")
new$new = asendaja(new$new, "lennuki", "kapten", replace = "piloot")
new$new = asendaja(new$new, "lendur", "", replace = "piloot")

unique(leidur(new$new, "stjua", ""))
new$new = asendaja(new$new, "stju", "", replace = "reisisaatja")
new$new = asendaja(new$new, "kajuti", "", replace = "reisisaatja")

unique(leidur(new$new, "giid", ""))
new$new = asendaja(new$new, "giid", "", excl1 = "tõlkja", excl2 = "firmaomanik", replace = "giid")

unique(leidur(new$new, "apteek", ""))
new$new = asendaja(new$new, "apteeker", "", excl1 = "omanik", replace = "apteeker")
new$new = asendaja(new$new, "proviisor", "", replace = "apteeker")
new$new = asendaja(new$new, "farmatseut", "", replace = "apteeker")

unique(leidur(new$new, "kondiiter", ""))
new$new = asendaja(new$new, "kondiiter", "", excl1 = "kokk-", replace = "pagar-kondiiter")
new$new = asendaja(new$new, "pagar", "", excl1 = "kokk", excl2 = "juh", excl3 = "omanik", excl4 = "personali",  replace = "pagar-kondiiter")
new$new = asendaja(new$new, "küpsetaja", "", replace = "pagar-kondiiter")

unique(leidur(new$new, "kokk", ""))
new$new = asendaja(new$new, "kokk", "pea", excl1 = "kohvikuj", replace = "peakokk")
new$new = asendaja(new$new, "kokk", "vanem", replace = "peakokk")
new$new = asendaja(new$new, "toitlustusjuht", "", replace = "peakokk")
new$new = asendaja(new$new, "söökla", "juhataja", excl1 = "kokk-", replace = "peakokk")
new$new = asendaja(new$new, "toitlustus-jamaj", "", replace = "peakokk")
new$new = asendaja(new$new, "toitlustusejuhataja", "", replace = "peakokk")
new$new = asendaja(new$new, "toitlustusalajuhataja", "", replace = "peakokk")

new$new = asendaja(new$new, "kok", "abi", replace = "kokk")
new$new = asendaja(new$new, "kokk", "pagar", replace = "kokk")
new$new = asendaja(new$new, "kokk", "kondiiter", replace = "kokk")
new$new = asendaja(new$new, "kokk", "restoran", replace = "kokk")
new$new = asendaja(new$new, "kokk-", "", replace = "kokk")

unique(leidur(new$new, "insener", "ehit"))
new$new = asendaja(new$new, "insener", "ehitus", replace = "ehitusinsener")

unique(leidur(new$new, "insener", "elektri",))
new$new = asendaja(new$new, "insener", "elektri", excl1 = "tsehhi", replace = "elektriinsener")
new$new = asendaja(new$new, "insener", "elektroonika", replace = "elektroonikainsener")
new$new = asendaja(new$new, "", "automaatikainsener", replace = "elektroonikainsener")
new$new = asendaja(new$new, "spetsialist", "automaatika", replace = "elektroonikainsener")

unique(leidur(new$new, "insener",""))
new$new = asendaja(new$new, "insener", "pea", excl1 = "ehitus", replace = "insener")
new$new = asendaja(new$new, "inseneer", "", excl1 = "abi", excl2 = "süsteem", replace = "insener")

unique(leidur(new$new, "fotograaf",""))
new$new = asendaja(new$new, "fotograaf", "", excl1 = "kunstnik", excl2 = "mööbli", replace = "fotograaf")
new$new = asendaja(new$new, "videograaf", "", replace = "fotograaf")

unique(leidur(new$new, "näitleja",""))
new$new = asendaja(new$new, "näitleja", "", excl1 = "teatriliidu", excl2 = "nõudepesija", replace = "näitleja")

unique(leidur(new$new, "laulja",""))
new$new = asendaja(new$new, "laulja", "", replace = "muusik")
new$new = asendaja(new$new, "orkestrant", "", replace = "muusik")
new$new = asendaja(new$new, "pianist", "", replace = "muusik")
new$new = asendaja(new$new, "kontsertmeister", "", excl1 = "õpetaja", replace = "muusik")
new$new = asendaja(new$new, "viiul","", excl1 = "õpetaja", replace = "muusik")
new$new = asendaja(new$new, "helilooja", "", replace = "muusik")
new$new = asendaja(new$new, "dirigent", "", excl1 = "õpetaja", replace = "muusik")
new$new = asendaja(new$new, "dirigentja", "", replace = "muusik")
new$new = asendaja(new$new, "koorijuht", "", excl1 = "õpetaja", replace = "muusik")
new$new = asendaja(new$new, "muusik", "erso", replace = "muusik")
new$new = asendaja(new$new, "muusik", "kirik", replace = "muusik")
new$new = asendaja(new$new, "muusik", "orkes", replace = "muusik")
new$new = asendaja(new$new, "muusik", "viiul", replace = "muusik")
new$new = asendaja(new$new, "muusik", "profe", replace = "muusik")
new$new = asendaja(new$new, "muusik", "kutse", replace = "muusik")
new$new = asendaja(new$new, "viiuldaja", "erso", replace = "muusik")
new$new = asendaja(new$new, "orkestriartist", "", replace = "muusik")
new$new = asendaja(new$new, "kooriartist", "", replace = "muusik")
new$new = asendaja(new$new, "ooperisolist", "", replace = "muusik")

unique(leidur(new$new, "raamatukogu","hoidja"))
new$new = asendaja(new$new, "raamatukogu", "hoidja", excl1 = "õpetaja+", replace = "raamatukoguhoidja")
new$new = asendaja(new$new, "bibliograaf", "", replace = "raamatukoguhoidja")
new$new = asendaja(new$new, "dokumendi", "haldu", excl1 = "juht", excl2 = "assis", excl3 = "asja", replace = "raamatukoguhoidja")
new$new = asendaja(new$new, "kataloogija", "", replace = "raamatukoguhoidja")

new$new = asendaja(new$new, "raamatukogu", "töötaja", replace = "raamatukogutöötaja")
new$new = asendaja(new$new, "raamatukogu", "klienditeenind", excl1 = "juh", replace = "raamatukogutöötaja")
new$new = asendaja(new$new, "lugejateenind", "", replace = "raamatukogutöötaja")

unique(leidur(new$new, "raamatukogu","juh"))
new$new = asendaja(new$new, "raamatukogu", "juh", replace = "raamatukogujuht")
new$new = asendaja(new$new, "raamatukogu", "direkt", replace = "raamatukogujuht")

unique(leidur(new$new, "lao", ""))
new$new = asendaja(new$new, "laooperaator", "", replace = "laoametnik")
new$new = asendaja(new$new, "laohoidja", "", replace = "laoametnik")
new$new = asendaja(new$new, "laospetsialist", "", replace = "laoametnik")
new$new = asendaja(new$new, "laoarvestuses", "", replace = "laoametnik")
new$new = asendaja(new$new, "laopidaja", "", replace = "laoametnik")
new$new = asendaja(new$new, "laoametnik", "", replace = "laoametnik")

new$new = asendaja(new$new, "laotöötaja", "", excl1 = "kontrolör", excl2 = "raamatup", excl3 = "logistik", replace = "laotöötaja")
new$new = asendaja(new$new, "laomees", "", replace = "laotöötaja")

new$new = asendaja(new$new, "laotööline", "", replace = "laotööline")
new$new = asendaja(new$new, "lao", "tööline", excl1 = "oskus", replace = "laotööline")

new$new = asendaja(new$new, "laojuhataja", "", replace = "laojuhataja")
new$new = asendaja(new$new, "laojuht", "", replace = "laojuhataja")
new$new = asendaja(new$new, "laohaldur", "", replace = "laojuhataja")
new$new = asendaja(new$new, "laovanem", "", replace = "laojuhataja")
new$new = asendaja(new$new, "ladude", "juh", replace = "laojuhataja")

unique(leidur(new$new, "ettevõtte", "juht", "väike"))
new$new = asendaja(new$new, "ettevõtte", "juh", "väike", excl1 = "tootmis", excl2 = "turundus", replace = "väikeettevõttejuht")
new$new = asendaja(new$new, "firma", "juht", "väike", replace = "väikeettevõttejuht")
new$new = asendaja(new$new, "mikro", "ettev", "juh", replace = "väikeettevõttejuht")
new$new = asendaja(new$new, "ühemeheettevõttejuht", "", replace = "väikeettevõttejuht")

unique(leidur(new$new, "ettevõttejuh", "omanik", excl1 = "väike", excl2 = "mikro"))
new$new = asendaja(new$new, "ettevõttejuh","omanik", excl1 = "väike", excl2 = "mikro", excl3 = "majutus", replace = "ettevõttejuht")

unique(leidur(new$new, "ettevõtte", "tippjuht"))
new$new = asendaja(new$new, "ettevõtte", "tippjuht", replace = "ettevõttejuht")

unique(leidur(new$new, "piirivalvur", ""))
new$new = asendaja(new$new, "piirivalvur", "", replace = "tolliinspektor")
new$new = asendaja(new$new, "piirivalveametnik", "", replace = "tolliinspektor")
new$new = asendaja(new$new, "piirivalveinspektor", "", replace = "tolliinspektor")

unique(leidur(new$new, "metsa", "töö"))
new$new = asendaja(new$new, "metsandusspetsialist", "", replace = "metsandusspetsialist")
new$new = asendaja(new$new, "metsandusespetsialist", "", replace = "metsandusspetsialist")
new$new = asendaja(new$new, "metsaspetsialist", "", replace = "metsandusspetsialist")
new$new = asendaja(new$new, "metsa", "spets", replace = "metsandusspetsialist")

new$new = asendaja(new$new, "metsakorraldaja", "", replace = "metsandustehnik")
new$new = asendaja(new$new, "abimetsaülem", "", replace = "metsandustehnik")
new$new = asendaja(new$new, "metsa", "tehnik", excl1 = "aia", excl2 = "rasketehnika", replace = "metsandustehnik")
new$new = asendaja(new$new, "metsavaht", "", replace = "metsandustehnik")
new$new = asendaja(new$new, "metsavaht", "", replace = "metsandustehnik")
new$new = asendaja(new$new, "metsa", "kasvat", excl1 = "õpetaja", replace = "metsandustehnik")
new$new = asendaja(new$new, "loodusvaht", "", replace = "metsandustehnik")

new$new = asendaja(new$new, "metsaülem", "", replace = "põllumetsandusjuht")

new$new = asendaja(new$new, "metsameister", "", replace = "metsatööline")
new$new = asendaja(new$new, "metsatööline", "", replace = "metsatööline")
new$new = asendaja(new$new, "metsaistutaja", "", replace = "metsatööline")
new$new = asendaja(new$new, "metsalangetaja", "", replace = "metsatööline")
new$new = asendaja(new$new, "metsamees", "", replace = "metsatööline")
new$new = asendaja(new$new, "metsnik", "", replace = "metsatööline")
new$new = asendaja(new$new, "hooldaja", "mets", replace = "metsatööline")
new$new = asendaja(new$new, "raietööline", "", replace = "metsatööline")

# kõik kokku panna?
unique(leidur(new$new, "õmbleja", ""))
new$new = asendaja(new$new, "õmble", "", excl1 = "jalatsite", excl2 = "teenindusjuht", excl3 = "raamatupidaja", excl4 = "rätsep-", replace = "õmbleja")
new$new = asendaja(new$new, "õmleja", "", excl1 = "jalatsite", excl2 = "teenindusjuht", excl3 = "raamatupidaja", excl4 = "rätsep-", replace = "õmbleja")
new$new = asendaja(new$new, "tikkija", "", replace = "õmbleja")
new$new = asendaja(new$new, "ömbleja", "", replace = "õmbleja")

new$new = asendaja(new$new, "juurdelõikaja", "", excl1 = "rätsep", excl2 = "kunstnik", replace = "juurdelõikaja")
new$new = asendaja(new$new, "rätsep", "", excl1 = "tipp", excl2 = "juurdelõikaja-", replace = "rätsep")

new$new = asendaja(new$new, "polsterdaja", "", replace = "polsterdaja")
new$new = asendaja(new$new, "pehmemööblivalmistaja", "", replace = "polsterdaja")
new$new = asendaja(new$new, "kingsepp", "", replace = "kingsepp")
new$new = asendaja(new$new, "jalats", "", excl1 = "kunstnik", replace = "kingsepp")
new$new = asendaja(new$new, "sadulsepp", "", replace = "kingsepp")

unique(leidur(new$new, "sisekujundaja", ""))
new$new = asendaja(new$new, "sisekuju", "", replace = "sisekujundaja")
new$new = asendaja(new$new, "visual", "merch", replace = "sisekujundaja")
new$new = asendaja(new$new, "lavakuju", "", replace = "sisekujundaja")
new$new = asendaja(new$new, "kujundaja", "väljapan", replace = "sisekujundaja")
new$new = asendaja(new$new, "spetsialist", "väljapan", replace = "sisekujundaja")
new$new = asendaja(new$new, "sisedisain", "", replace = "sisekujundaja")

unique(leidur(new$new, "kunstnik", ""))
unique(leidur(new$new, "kunstnik", "mängu"))
new$new = asendaja(new$new, "kunstnik", "mängu", replace = "multimeediadisainer")
new$new = asendaja(new$new, "graafiline", "disainer", excl1 = "turundus", replace = "multimeediadisainer")
new$new = asendaja(new$new, "ux", "dis", replace = "multimeediadisainer")
new$new = asendaja(new$new, "ui", "disainer", replace = "multimeediadisainer")
new$new = asendaja(new$new, "3d", "", replace = "multimeediadisainer")
new$new = asendaja(new$new, "graafika", "disainer", excl1 = "kunstnik", replace = "multimeediadisainer")
new$new = asendaja(new$new, "kujundaja", "graaf", replace = "multimeediadisainer")
new$new = asendaja(new$new, "kujundaja", "deko", replace = "multimeediadisainer")
new$new = asendaja(new$new, "kujundaja", "disainer", replace = "multimeediadisainer")
new$new = asendaja(new$new, "multimeedia", "disainer", replace = "multimeediadisainer")
new$new = asendaja(new$new, "illustraator", "", replace = "multimeediadisainer")
new$new = asendaja(new$new, "animaator", "", replace = "multimeediadisainer")

unique(leidur(new$new, "moe", "kunstnik"))
new$new = asendaja(new$new, "moe", "kunstnik", replace = "moedisainer")
new$new = asendaja(new$new, "moe", "disainer", replace = "moedisainer")
new$new = asendaja(new$new, "moe", "disainer", replace = "moedisainer")
new$new = asendaja(new$new, "rõiva", "disainer", replace = "moedisainer")

new$new = asendaja(new$new, "kunstnik", "maali", replace = "kunstnik")
new$new = asendaja(new$new, "kunstnik-", "", replace = "kunstnik")
new$new = asendaja(new$new, "kunstnikja", "", replace = "kunstnik")
new$new = asendaja(new$new, "kunstnik", "vaba", replace = "kunstnik")
new$new = asendaja(new$new, "kunst", "tead", replace = "kunstnik")

unique(leidur(new$new, "logistik", "juh", excl1 = "abi"))
new$new = asendaja(new$new, "logistik", "juh", excl1 = "abi", excl2 = "auto", excl3 = "laadur", excl4 = "töödejuh", replace = "tarneahelajuht")
new$new = asendaja(new$new, "logistik", "dir", replace = "tarneahelajuht")
new$new = asendaja(new$new, "logistik", "manager", replace = "tarneahelajuht")
new$new = asendaja(new$new, "transpor", "ettev", "juh", replace = "tarneahelajuht")

unique(leidur(new$new, "logistik", "", excl1 = "abi"))
new$new = asendaja(new$new, "logistik", "", excl1 = "abi", excl2 = "juht", excl3 = "assis", excl4 = "raamatup",  replace = "logistik")
new$new = asendaja(new$new, "logistikauto", "", replace = "logistik")
new$new = asendaja(new$new, "veokorraldaja", "", replace = "logistik")

# erinevate koodidega kvaliteedijuhte on - on nö juhtimisanalüütikute grupis ja tootmise kvaliteedijuhid (tehnika, tehnoloogia jne)
unique(leidur(new$new, "kvaliteed", "juh", excl1 = "assist"))
new$new = asendaja(new$new, "kvaliteedi", "juh", excl1 = "assistent", excl2 = "lagede", replace = "kvaliteedijuht")

unique(leidur(new$new, "režissöör", ""))
new$new = asendaja(new$new, "režissöör", "", excl1 = "helir", excl2 = "montaaž", replace = "filmi-teatriprodutsent")
new$new = asendaja(new$new, "filmimontee", "", replace = "filmi-teatriprodutsent")
new$new = asendaja(new$new, "peaoperaa", "", replace = "filmi-teatriprodutsent")
new$new = asendaja(new$new, "produtsent", "film", replace = "filmi-teatriprodutsent")
new$new = asendaja(new$new, "produtsent", "teatri", replace = "filmi-teatriprodutsent")
new$new = asendaja(new$new, "lavastaja", "", replace = "filmi-teatriprodutsent")
new$new = asendaja(new$new, "kunstiline", "juht", replace = "filmi-teatriprodutsent")
new$new = asendaja(new$new, "rezis", "", excl1 = "heli", replace = "filmi-teatriprodutsent")
new$new = asendaja(new$new, "videomont", "", replace = "filmi-teatriprodutsent")

new$new = asendaja(new$new, "režissöör", "heli", excl1 = "filmi", replace = "audiovisuaaltehnik")
new$new = asendaja(new$new, "rezissöör", "heli", excl1 = "filmi", replace = "audiovisuaaltehnik")
new$new = asendaja(new$new, "režissöör", "mont", excl1 = "filmi", replace = "audiovisuaaltehnik")
new$new = asendaja(new$new, "filmi", "operaator", replace = "audiovisuaaltehnik")
new$new = asendaja(new$new, "heli", "operaator", replace = "audiovisuaaltehnik")
new$new = asendaja(new$new, "heli", "tehnik", excl1 = "enne", replace = "audiovisuaaltehnik")
new$new = asendaja(new$new, "heli", "kujund", excl1 = "majandus", replace = "audiovisuaaltehnik")
new$new = asendaja(new$new, "tele", "operaa", excl1 = "telefoni", replace = "audiovisuaaltehnik")
new$new = asendaja(new$new, "video", "operaa", excl1 = "valve", replace = "audiovisuaaltehnik")
new$new = asendaja(new$new, "video", "tehn", excl1 = "erinev", replace = "audiovisuaaltehnik")

unique(leidur(new$new, "elektrik", "auto"))
new$new = asendaja(new$new, "elektrik", "auto", replace = "elektromehaanik")

#siin järgmises on nii elektriseadmete mehaanikud (7412) kui ka elektroonikamehaanikud (7421) - ei suuda vahet teha
unique(leidur(new$new, "elektr", "meh"))
new$new = asendaja(new$new, "elektr", "meh", excl1 = "ittugi", replace = "elektromehaanik")
new$new = asendaja(new$new, "elektr", "laeva", replace = "elektromehaanik")
new$new = asendaja(new$new, "kodumas", "meh", replace = "elektromehaanik")
new$new = asendaja(new$new, "lif", "meh", replace = "elektromehaanik")

# elektrotehnik on 3113, elektroonikatehnik on 3114 - siin ilmselt ka elektroonikatehnikud läksid elektrotehnikute alla, aga ehk ongi mõistlik kokku panna?
# elektroonikud ehk elektroonikatehnikud on allpool veel eraldi tehtud, aga neid on nagunii vähe
unique(leidur(new$new, "elektr", "tehnik"))
new$new = asendaja(new$new, "elektr", "tehnik", replace = "elektrotehnik")
new$new = asendaja(new$new, "elektr", "energ", replace = "elektrotehnik")
new$new = asendaja(new$new, "elektr", "käidu", replace = "elektrotehnik")
new$new = asendaja(new$new, "juht", "käidu", replace = "elektrotehnik")
new$new = asendaja(new$new, "energeetik", "", excl1 = "meister", replace = "elektrotehnik")

unique(leidur(new$new, "elektrik", "", excl1 = "kappide"))
new$new = asendaja(new$new, "elektrik", "", excl1 = "kappide", excl2 = "kilp", excl3 = "keevitaja", replace = "elektrik")

unique(leidur(new$new, "spetsialist", "ostu"))
new$new = asendaja(new$new, "spetsialist", "ostu", replace = "hankespetsialist")
new$new = asendaja(new$new, "spetsialist", "hanke", replace = "hankespetsialist")
new$new = asendaja(new$new, "ostu", "assist", excl1 = "juhi", replace = "hankespetsialist")
new$new = asendaja(new$new, "kauba", "kategooria", replace = "hankespetsialist")
new$new = asendaja(new$new, "hange", "koordin", replace = "hankespetsialist")
new$new = asendaja(new$new, "hanke", "koordin", replace = "hankespetsialist")

table(leidur(new$new, "andme", "", "")) %>% sort(decreasing = T) %>% .[1:30]

unique(leidur(new$new, "andme", "töötl", excl1 = "juht"))
new$new = asendaja(new$new, "andme", "tööt", excl1 = "juht", replace = "andmetöötleja")

unique(leidur(new$new, "andme", "sisest"))
new$new = asendaja(new$new, "andme", "sisest", excl1 = "juhataja-", excl2 = "geodeet", excl3 = "sekretär", replace = "andmesisestaja")

unique(leidur(new$new, "andme", "tead"))
new$new = asendaja(new$new, "andme", "tead", excl1 = "juht", excl2 = "admin", replace = "andmeteadur")
unique(leidur(new$new, "andme", "analüüt"))
new$new = asendaja(new$new, "andme", "analüüt",replace = "andmeteadur")
new$new = asendaja(new$new, "statistik", "", excl1 = "osakond", excl2 = "toimetaja", replace = "andmeteadur")
new$new = asendaja(new$new, "aktuaar", "", replace = "andmeteadur")
new$new = asendaja(new$new, "matemaatik", "",replace = "andmeteadur")

unique(leidur(new$new, "auto", "remondi", ""))
new$new = asendaja(new$new, "auto", "remondi", excl1 = "plekk", excl2 = "töökoja", replace = "automehaanik-lukksepp")
new$new = asendaja(new$new, "auto", "meh", replace = "automehaanik-lukksepp")
new$new = asendaja(new$new, "auto", "diagn", replace = "automehaanik-lukksepp")
new$new = asendaja(new$new, "masina", "diagn", replace = "automehaanik-lukksepp")
new$new = asendaja(new$new, "rehv", "tehnik", replace = "automehaanik-lukksepp")
new$new = asendaja(new$new, "autolukk", "", replace = "automehaanik-lukksepp")
new$new = asendaja(new$new, "sõidukimeh", "", excl1 = "juh", replace = "automehaanik-lukksepp")

unique(leidur(new$new, "keevitaja", "", ""))
new$new = asendaja(new$new, "keevitaja-", "",replace = "keevitaja")
new$new = asendaja(new$new, "keevitaja", "", excl1 = "luk", excl2 = "ehitaja", replace = "keevitaja")

unique(leidur(new$new, "luksepp", "", ""))
new$new = asendaja(new$new, "luksepp", "",replace = "lukksepp")
new$new = asendaja(new$new, "lukksepp", "remondi",replace = "remondilukksepp")

new$new = asendaja(new$new, "lukksepp", "", excl1 = "auto", excl2 = "remondi", excl3 = "treial", excl4 = "toru", replace = "lukksepp")
new$new = asendaja(new$new, "lukksepp-auto", "", replace = "lukksepp")

# masinaoperaatoreid on u 10 erineva koodi all ja üsna keeruline eristada (ISCO-s muidu materjali järgi saab, proovin)
# kuigi võib-olla mõistlik kõik kokku panna?
unique(leidur(new$new, "operaator", "masina", ""))

# see rühm kodeeritakse hiljem ümber 
new$new = asendaja(new$new, "operaator", "masina", "trük", replace = "trükkal")

# N = 6
new$new = asendaja(new$new, "operaator", "klaas", replace = "klaasimasinaoperaator")

# N = 15
new$new = asendaja(new$new, "operaator", "metall", replace = "metallimasinaoperaator")
new$new = asendaja(new$new, "operaator", "frees", replace = "metallimasinaoperaator")

# N = 71
new$new = asendaja(new$new, "operaator", "puidu", replace = "puidumasinaoperaator")
new$new = asendaja(new$new, "operaator", "höövl", replace = "puidumasinaoperaator")
new$new = asendaja(new$new, "operaator", "mööbl", replace = "puidumasinaoperaator")
new$new = asendaja(new$new, "operaator", "nelik", replace = "puidumasinaoperaator")
new$new = asendaja(new$new, "puidupingitööline", "", replace = "puidumasinaoperaator")

# N = 9
new$new = asendaja(new$new, "operaator", "toidu", replace = "toiduainemasinaoperaator")
new$new = asendaja(new$new, "operaator", "saia", replace = "toiduainemasinaoperaator")
new$new = asendaja(new$new, "operaator", "villim", replace = "toiduainemasinaoperaator")
new$new = asendaja(new$new, "operaator", "juustu", replace = "toiduainemasinaoperaator")
new$new = asendaja(new$new, "operaator", "kohupiima", replace = "toiduainemasinaoperaator")
new$new = asendaja(new$new, "operaator", "vorsti", replace = "toiduainemasinaoperaator")

# N = 85
new$new = asendaja(new$new, "operaator", "cnc", replace = "cnc-pingioperaator")
new$new = asendaja(new$new, "operaator", "pingi",replace = "cnc-pingioperaator")
new$new = asendaja(new$new, "pingitööline", "",replace = "cnc-pingioperaator")

# siin siis ikkagi need kõige ebamäärasemad (nt võib olla mitmes valdkonnas), mis siiski masinaoperaatoriks nimetasin 
new$new = asendaja(new$new, "operaator", "pressi", replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "seadme", excl1 = "kaugseire", replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "protsessi", replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "pink", replace = "masinaoperaator")
new$new = asendaja(new$new, "masinateoperaator", "", replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "betoon", replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "katla", replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "tehnika", replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "kopp", replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "ekstruud", replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "elevaator", replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "kuiva", replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "sae", replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "kaabli", replace = "masinaoperaator")
new$new = asendaja(new$new, "hamachertrumlioperaator", "", replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "sõlm", replace = "masinaoperaator")
new$new = asendaja(new$new, "hakkurioperaator", "", replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "stants", replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "pakke", replace = "masinaoperaator")

# kuna need erialdi grupid on üsna väikesed, siis võiks ikka kõik masinaoperaatorid kokku panna? 
# Või siis cnc+puidu+metalli ja siis teised eraldi? Kuigi ilmselt ka nende teiste masinaoperaatorite seas on puidu-metallioperaatoreid
unique(leidur(new$new, "masinaoperaator", ""))
new$new = asendaja(new$new, "masinaoperaator", "", excl1 = "pesu", replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "masin", excl1 = "pesu", replace = "masinaoperaator")

# liinioperaatoritega ka sama jama, et koodi on raske panna
unique(leidur(new$new, "operaator", "liini", ""))
new$new = asendaja(new$new, "operaator", "liini",replace = "liinioperaator")

new$new = asendaja(new$new, "raamatupidaja-", "", replace = "raamatupidaja")
new$new = asendaja(new$new, "raamatupidajaja", "", replace = "raamatupidaja")
new$new = asendaja(new$new, "raanatupidaja-", "", replace = "raamatupidaja")

unique(leidur(new$new, "ökonomist", ""))
new$new = asendaja(new$new, "ökonomist", "", replace = "ökonomist")
new$new = asendaja(new$new, "majandustead", "", replace = "ökonomist")
new$new = asendaja(new$new, "majandus", "analüü", replace = "ökonomist")
new$new = asendaja(new$new, "õkonomist", "", replace = "ökonomist")
new$new = asendaja(new$new, "majandusspetsialist", "", replace = "ökonomist")

new$new = asendaja(new$new, "raamatupi", "tööt", excl1 = "sotsiaal", excl2 = "juhi", replace = "raamatupidamisekontoritöötaja")
new$new = asendaja(new$new, "finants", "tööt", replace = "raamatupidamisekontoritöötaja")
new$new = asendaja(new$new, "arve", "ametn", replace = "raamatupidamisekontoritöötaja")
new$new = asendaja(new$new, "arve", "spets", replace = "raamatupidamisekontoritöötaja")
new$new = asendaja(new$new, "arve", "oper", replace = "raamatupidamisekontoritöötaja")

new$new = asendaja(new$new, "palga", "arvest", excl1 = "juht",  replace = "palgaarvestaja")

new$new = asendaja(new$new, "raamatup", "spetsial", excl1 = "person", excl2 = "andme", replace = "raamatupidamisespetsialist")
new$new = asendaja(new$new, "finantssp", "",  replace = "raamatupidamisespetsialist")

new$new = asendaja(new$new, "eelarvest", "ehitus", replace = "ehitustehnik")
new$new = asendaja(new$new, "eelarvest", "insener", replace = "ehitustehnik") #selles natuke kahtlen?
new$new = asendaja(new$new, "eelarvest", "hoone", replace = "ehitustehnik")

new$new = asendaja(new$new, "eelarvest", "", excl1 = "juh", excl2 = "ehitus", replace = "raamatupidamisespetsialist")
new$new = asendaja(new$new, "assis", "raamatup", replace = "raamatupidamisespetsialist")

new$new = asendaja(new$new, "van", "raamatup", excl1 = "õpet", replace = "pearaamatupidaja")
new$new = asendaja(new$new, "bilans", "raamatup", replace = "pearaamatupidaja")
new$new = asendaja(new$new, "raamatupi", "juhtiv", replace = "pearaamatupidaja")
new$new = asendaja(new$new, "paraamatupi", "", replace = "pearaamatupidaja")
new$new = asendaja(new$new, "peraamatupi", "", replace = "pearaamatupidaja")

new$new = asendaja(new$new, "personali", "arvest", excl1 = "raamatupidajap", excl2 = "juht", replace = "personalitöötaja")
new$new = asendaja(new$new, "personali", "töötaja", excl1 = "raamatupid", excl2 = "sekretär", replace = "personalitöötaja")

# ikka väga kirju seltskond siin raamatupidajates (st paljudel "esimene nimetus" midagi muud) - pusin allpool lõpuni
unique(leidur(new$new, "raamatupida", "", excl1 = "abi"))

unique(leidur(new$new, "geodeet", ""))
new$new = asendaja(new$new, "geodeet", "", replace = "maamõõtja")
new$new = asendaja(new$new, "maamõõtja", "", replace = "maamõõtja")
new$new = asendaja(new$new, "kartograaf", "", replace = "maamõõtja")
new$new = asendaja(new$new, "maakorraldaja", "", replace = "maamõõtja")
new$new = asendaja(new$new, "topogr", "", replace = "maamõõtja")
new$new = asendaja(new$new, "hüdrogr", "", replace = "maamõõtja")
new$new = asendaja(new$new, "geoinfo", "", excl1 = "ispets", excl2 = "ospets", replace = "maamõõtja")
new$new = asendaja(new$new, "gis", "peaspetsialist", replace = "maamõõtja")
new$new = asendaja(new$new, "gis-", "spetsialist", replace = "maamõõtja")
new$new = asendaja(new$new, "geoinfo", "spetsialist", replace = "maamõõtja")
new$new = asendaja(new$new, "geodeesia", "", excl1 = "juh", excl2 = "andmete", replace = "maamõõtja")

unique(leidur(new$new, "veduri", "juh"))
new$new = asendaja(new$new, "veduri", "juht", replace = "rongijuht")

unique(leidur(new$new, "juhiabi", ""))
new$new = asendaja(new$new, "juhiabi", "", excl1 = "kahe", excl2 = "puidurestauraator", excl3 = "huvi", replace = "juhiabi")

unique(leidur(new$new, "kliend", "haldu"))
new$new = asendaja(new$new, "hald", "kliend", "panga", excl1 = "juh", replace = "pangateller")
new$new = asendaja(new$new, "postkontoritöötaja", "", replace = "pangateller")
new$new = asendaja(new$new, "hald", "kliend", "äri", excl1 = "juh", replace = "finantsnõustaja")
new$new = asendaja(new$new, "hald", "kliend", "ãri", excl1 = "juh", replace = "finantsnõustaja")
new$new = asendaja(new$new, "finants", "nõu", excl1 = "raamatu", replace = "finantsnõustaja")
new$new = asendaja(new$new, "võla", "nõu", excl1 = "nõudja", excl2 = "menetl", excl3 = "juht", replace = "finantsnõustaja")
new$new = asendaja(new$new, "invest", "nõu", excl1 = "juht", replace = "finantsnõustaja")
new$new = asendaja(new$new, "portfelli", "haldur", excl1 = "metsa", replace = "finantsnõustaja")
new$new = asendaja(new$new, "finantskoord", "", replace = "finantsnõustaja")
new$new = asendaja(new$new, "koord", "raha", replace = "finantsnõustaja")
new$new = asendaja(new$new, "suurkliendihaldur", "", replace = "finantsnõustaja")

unique(leidur(new$new, "tegevusjuhe", ""))
new$new = asendaja(new$new, "tegevusjuhe", "", replace = "tegevusjuhendaja")

unique(leidur(new$new, "majahoidja", ""))
new$new = asendaja(new$new, "majahoidja", "", excl1 = "kasvataja", excl2 = "koristajama", excl3 = "koristaja-", replace = "hoonehaldaja")
new$new = asendaja(new$new, "kalmistuvaht", "", replace = "hoonehaldaja")
new$new = asendaja(new$new, "kodu", "admini", replace = "hoonehaldaja")
new$new = asendaja(new$new, "sadamavaht", "", replace = "hoonehaldaja")

unique(leidur(new$new, "zoo", "tehnik"))
new$new = asendaja(new$new, "zoo", "tehnik", replace = "biotehnik")
new$new = asendaja(new$new, "bio", "tehnik", excl1 = "insener", excl2 = "spets", replace = "biotehnik")

unique(leidur(new$new, "pesu", "pesij"))
new$new = asendaja(new$new, "pesu", "pesij", excl1 = "auto", replace = "pesupesija")
new$new = asendaja(new$new, "pesum", "oper", replace = "pesumasinaoperaator")
new$new = asendaja(new$new, "pesu", "maja", excl1 = "juh", replace = "pesupesija")

unique(leidur(new$new, "koristaja", ""))
new$new = asendaja(new$new, "koristaja", "", excl1 = "dire", excl2 = "kliendi", excl3 = "objekt", excl4 = "müüja",  replace = "puhastusteenindaja")
unique(leidur(new$new, "puhastus", "teeni"))
new$new = asendaja(new$new, "puhastus", "teen", excl1 = "kliendit", excl2 = "siidi", excl3 = "hooldaja", replace = "puhastusteenindaja")
new$new = asendaja(new$new, "põrandamop", "operaator", replace = "puhastusteenindaja")
new$new = asendaja(new$new, "puhastusoper", "", replace = "puhastusteenindaja")

unique(leidur(new$new, "jurist", ""))
unique(leidur(new$new, "jurist", "abi"))
new$new = asendaja(new$new, "jurist", "abi",  replace = "õigusekeskastmespetsialist")
new$new = asendaja(new$new, "jurist", "assis",  replace = "õigusekeskastmespetsialist")
new$new = asendaja(new$new, "kohtutäitur", "",  replace = "õigusekeskastmespetsialist")
new$new = asendaja(new$new, "väärteom", "",  replace = "õigusekeskastmespetsialist")
new$new = asendaja(new$new, "kohtu", "sekretär",  replace = "õigusekeskastmespetsialist")
new$new = asendaja(new$new, "riigihangetespets", "",  replace = "õigusekeskastmespetsialist")
new$new = asendaja(new$new, "kohtunikuabi", "",  replace = "õigusekeskastmespetsialist")

unique(leidur(new$new, "jurist", ""))
new$new = asendaja(new$new, "jurist", "", excl1 = "kohtu", excl2 = "õpetaja", excl3 = "tegev", replace = "jurist")
new$new = asendaja(new$new, "õigus", "nõu", replace = "jurist")
new$new = asendaja(new$new, "notar", "", excl1 = "büroo", excl2 = "abi", replace = "jurist")

# tootmisjuhte on tegelikult mitmesuguste koodidega (oleneb valdkonnast)
# varasemalt panin lihtsalt tootmisjuhid "töötleva tööstuse tootmisjuhtideks", kuigi see ei pruugi nii olla, aga ma arvan, et teised tootmisjuhid (vesiviljeluse, kaevanduse, meedia) on pigem vähemuses? Põllumaj ilmselt ikka on
unique(leidur(new$new, "tootmis", "juh",))
unique(leidur(new$new, "tootmisjuh", "",))
new$new = asendaja(new$new, "tootmisjuh", "", excl1 = "assis", excl2 = "looma", excl3 = "omanik", replace = "tööstusjuht")
new$new = asendaja(new$new, "tootmisjuh", "looma", replace = "põllumetsandusjuht")

unique(leidur(new$new, "tootmis", "juh",))

unique(leidur(new$new, "müügi", "juht", "kauba"))
new$new = asendaja(new$new, "müügi", "juht", "kauba", replace = "kaubandusjuht")
new$new = asendaja(new$new, "müügi", "juht", "jae", replace = "kaubandusjuht")
new$new = asendaja(new$new, "müügi", "juht", "hulgi", replace = "kaubandusjuht")
new$new = asendaja(new$new, "ostu", "juht", "kauba", replace = "kaubandusjuht")
new$new = asendaja(new$new, "müügi", "juht", "poe", replace = "kaubandusjuht")
new$new = asendaja(new$new, "jae", "juh", "kauba", replace = "kaubandusjuht")
new$new = asendaja(new$new, "kaubandusjuh", "", replace = "kaubandusjuht")
new$new = asendaja(new$new, "kauband", "keskastmej", replace = "kaubandusjuht")
new$new = asendaja(new$new, "kaubandus", "juh", excl1 = "reklaam", excl2 = "lao", excl3 = "teenindus", excl4 = "toote", replace = "kaubandusjuht")
new$new = asendaja(new$new, "ostujuht", "taime", replace = "kaubandusjuht")
new$new = asendaja(new$new, "ostujuht", "koduteh", replace = "kaubandusjuht")

# kaupluse juhatajad on ka tegelikult kaubandusjuhtide all koodi mõttes, aga jätsin esialgu eraldi
new$new = asendaja(new$new, "kauplusejuhataja", "", excl1 = "optometrist", excl2 = "dispetšer", replace = "kauplusejuhataja")
unique(leidur(new$new, "poe", "juh"))
new$new = asendaja(new$new, "poe", "juh", excl1 = "müügijuht", excl2 = "teenindaja", replace = "kauplusejuhataja")
unique(leidur(new$new, "kauplus", "juh"))
new$new = asendaja(new$new, "kauplus", "juh", excl1 = "optometrist", excl2 = "dispetšer", excl3 = "kateg", replace = "kauplusejuhataja")
new$new = asendaja(new$new, "aptee", "juh", excl1 = "lao",  replace = "kauplusejuhataja")
new$new = asendaja(new$new, "supermark", "juh",  replace = "kauplusejuhataja")
new$new = asendaja(new$new, "hüperm", "juh",  replace = "kauplusejuhataja")
new$new = asendaja(new$new, "kpl", "juh", excl1 = "plasti", replace = "kauplusejuhataja")

unique(leidur(new$new, "kategooriajuht", ""))
new$new = asendaja(new$new, "kategooriajuht", "", excl1 = "teenindus", excl2 = "meeskonna", replace = "hankespetsialist")
new$new = asendaja(new$new, "tootejuht", "kauba", replace = "hankespetsialist")
new$new = asendaja(new$new, "hange", "spets", excl1 = "riigi", replace = "hankespetsialist")
new$new = asendaja(new$new, "varustaja", "", excl1 = "mehaanik", replace = "hankespetsialist")
new$new = asendaja(new$new, "ostu", "koord", replace = "hankespetsialist")
new$new = asendaja(new$new, "kaubagrupijuht", "", replace = "hankespetsialist")
new$new = asendaja(new$new, "kaubakategoorijajuht", "", replace = "hankespetsialist")

unique(leidur(new$new, "hotell", "admin"))
new$new = asendaja(new$new, "hotell", "admin", replace = "hotelliadministraator")
new$new = asendaja(new$new, "hotell", "vastuvõtut", replace = "hotelliadministraator")

unique(leidur(new$new, "süsteem", "admin"))
new$new = asendaja(new$new, "süsteem", "admin", replace = "süsteemiadministraator")
new$new = asendaja(new$new, "võrgu", "admin", replace = "süsteemiadministraator")
new$new = asendaja(new$new, "it-spets", "", replace = "süsteemiadministraator")
new$new = asendaja(new$new, "it-pespets", "", replace = "süsteemiadministraator")
new$new = asendaja(new$new, "infotehnoloog", "", excl1 = "juh", excl2 = "audiitor", excl3 = "anal", excl4 = "testija", replace = "süsteemiadministraator")
new$new = asendaja(new$new, "tarkv", "admin", replace = "süsteemiadministraator")
# IT-juht võib olla nii tippspetsialist kui ka juht (IKT-juhid) - oleneb, kas on alluvaid või ei ole
new$new = asendaja(new$new, "itjuht", "", replace = "süsteemiadministraator")
new$new = asendaja(new$new, "it-juht", "", replace = "süsteemiadministraator")
new$new = asendaja(new$new, "itvanemspets", "", replace = "süsteemiadministraator")
new$new = asendaja(new$new, "infosüsteemipeaspetsialist", "", replace = "süsteemiadministraator")
new$new = asendaja(new$new, "itspetsialist", "", replace = "süsteemiadministraator")
new$new = asendaja(new$new, "itpeaspetsialist", "", replace = "süsteemiadministraator")
new$new = asendaja(new$new, "itsüsteem", "spetsialist", replace = "süsteemiadministraator")

unique(leidur(new$new, "itarhitekt", ""))
new$new = asendaja(new$new, "itarhit", "", replace = "süsteemianalüütik")
new$new = asendaja(new$new, "it-arhit", "", replace = "süsteemianalüütik")
new$new = asendaja(new$new, "itlahend", "arhit", replace = "süsteemianalüütik")
new$new = asendaja(new$new, "itinfra", "arhit", replace = "süsteemianalüütik")

unique(leidur(new$new, "ittestija", ""))
new$new = asendaja(new$new, "ittestija", "", replace = "it-testija")
new$new = asendaja(new$new, "it-testija", "", replace = "it-testija")
new$new = asendaja(new$new, "tarkvara", "testija", excl1="juht", replace = "it-testija")
new$new = asendaja(new$new, "qa", "eng", replace = "it-testija")
new$new = asendaja(new$new, "qa", "spets", replace = "it-testija")
new$new = asendaja(new$new, "qa", "ins", replace = "it-testija")
new$new = asendaja(new$new, "qualityass", "", excl1 = "lead", replace = "it-testija")
new$new = asendaja(new$new, "peakasutaja", "", replace = "it-testija")
new$new = asendaja(new$new, "tarkvarakvaliteedispetsialist", "", replace = "it-testija")

unique(leidur(new$new, "arhitekt", "ehitus"))
new$new = asendaja(new$new, "arhitekt", "ehitus", replace = "ehitusarhitekt")
new$new = asendaja(new$new, "arhitekt", "hoone", replace = "ehitusarhitekt")
new$new = asendaja(new$new, "arhitekt", "sise", replace = "ehitusarhitekt")
new$new = asendaja(new$new, "arhitekt", "planeer", replace = "ehitusarhitekt")
new$new = asendaja(new$new, "arhitekt", "linn", replace = "ehitusarhitekt")
new$new = asendaja(new$new, "arhitekt", "omav", replace = "ehitusarhitekt")
new$new = asendaja(new$new, "arhitekt", "valla", replace = "ehitusarhitekt")
new$new = asendaja(new$new, "arhitekt", "pea", replace = "ehitusarhitekt")
new$new = asendaja(new$new, "arhitektuur", "", replace = "ehitusarhitekt")

new$new = asendaja(new$new, "arhitekt", "maastiku", replace = "maastikuarhitekt")

unique(leidur(new$new, "personalispetsialist", "")) 
new$new = asendaja(new$new, "personalispetsialist", "", excl1 = "juht", excl2 = "sekretär", replace = "personalispetsialist")
new$new = asendaja(new$new, "värbamisspetsialist", "", replace = "personalispetsialist")
new$new = asendaja(new$new, "karjääri", "nõust", replace = "personalispetsialist")
new$new = asendaja(new$new, "personali", "spets", replace = "personalispetsialist")
new$new = asendaja(new$new, "karjääri", "spets", replace = "personalispetsialist")

unique(leidur(new$new, "värba", ""))
new$new = asendaja(new$new, "värba", "", excl1 = "juh", excl2 = "assis",  replace = "personalivärbaja")
new$new = asendaja(new$new, "töövahen", "",  replace = "personalivärbaja")
new$new = asendaja(new$new, "tööhõive", "",  replace = "personalivärbaja")

# arendusjuhte ka mitmete koodidega, nö alluvatega/alluvateta ja olenevalt valdkonnast
unique(leidur(new$new, "arendusjuht", ""))
unique(leidur(new$new, "arendusjuht", "tarkvara"))
new$new = asendaja(new$new, "arendusjuht", "tarkvara",  replace = "IKT-juht")
new$new = asendaja(new$new, "arendusjuht", "info",  replace = "IKT-juht")
new$new = asendaja(new$new, "juht", "infotehno", excl1 = "tiimi",  replace = "IKT-juht")
new$new = asendaja(new$new, "itarend", "juht", excl1 = "projekt",  replace = "IKT-juht")
new$new = asendaja(new$new, "it", "arendusjuht", excl1 = "linna", excl2 = "kinnisvara", excl3 = "grupi", excl4 = "koolitus",  replace = "IKT-juht")
new$new = asendaja(new$new, "digi", "arendusjuht", replace = "IKT-juht")
new$new = asendaja(new$new, "digi", "arendusjuht", replace = "IKT-juht")
new$new = asendaja(new$new, "kasutajaliide", "arendusjuht", replace = "IKT-juht")
new$new = asendaja(new$new, "veebi", "arendusjuht", replace = "IKT-juht")
new$new = asendaja(new$new, "tarkvara", "arendusejuht", replace = "IKT-juht")
new$new = asendaja(new$new, "ikt-juht", "", replace = "IKT-juht")

unique(leidur(new$new, "arendusjuht", "toote"))
new$new = asendaja(new$new, "arendusjuht", "toote",  replace = "tootearendusjuht")

unique(leidur(new$new, "kontori", "juhata"))
new$new = asendaja(new$new, "kontori", "juhata", replace = "kontorijuhataja")
new$new = asendaja(new$new, "büroo", "juhata", excl1 = "peaspe", replace = "kontorijuhataja")
new$new = asendaja(new$new, "büroojuht", "", excl1 = "tooteinfo", excl2 = "personali", excl3 = "arendus", replace = "kontorijuhataja")
new$new = asendaja(new$new, "kontorijuht", "", excl1 = "kliendi", replace = "kontorijuhataja")
new$new = asendaja(new$new, "büroo", "juht", excl1 = "projekti", excl2 = "arendus-", excl3 = "firma", excl4 = "tooteinfo", replace = "kontorijuhataja")
new$new = asendaja(new$new, "kantselei", "juh", replace = "kontorijuhataja")
new$new = asendaja(new$new, "büroojuh", "", replace = "kontorijuhataja")
new$new = asendaja(new$new, "osakonna", "juh", "teenindus", excl1 = "planeerimis", replace = "kontorijuhataja")
new$new = asendaja(new$new, "kliend", "koord", replace = "kontorijuhataja")
new$new = asendaja(new$new, "personal", "koord", replace = "kontorijuhataja")
new$new = asendaja(new$new, "asjaajam", "peaspets", replace = "kontorijuhataja")
new$new = asendaja(new$new, "klienditoejuht", "", replace = "kontorijuhataja")

unique(leidur(new$new, "kommunikatsiooni", "juh"))
new$new = asendaja(new$new, "kommunikatsioonijuh", "", replace = "reklaamijuht")
new$new = asendaja(new$new, "reklaamijuht", "", replace = "reklaamijuht")
new$new = asendaja(new$new, "reklaamiosa", "juht", replace = "reklaamijuht")
new$new = asendaja(new$new, "kommunikatsioonios", "juh", replace = "reklaamijuht")
new$new = asendaja(new$new, "kommunikatsiooni", "juh", excl1 = "müügi", excl2 = "omanik", excl3 = "osakonnajuh", replace = "reklaamijuht")

unique(leidur(new$new, "kommunikatsioonispets", ""))
new$new = asendaja(new$new, "kommunikatsiooni", "spets", replace = "kommunikatsioonispetsialist")
new$new = asendaja(new$new, "avalike", "spets", replace = "kommunikatsioonispetsialist")
new$new = asendaja(new$new, "avalike", "suhe", excl1 = "juht", replace = "kommunikatsioonispetsialist")
new$new = asendaja(new$new, "pressi", "esind", replace = "kommunikatsioonispetsialist")
new$new = asendaja(new$new, "suhtekorr", "", replace = "kommunikatsioonispetsialist")
new$new = asendaja(new$new, "kommunikatsiooni", "", excl1 = "juh", excl2 = "assist", excl3 = "insener", excl4 = "tehnik", replace = "kommunikatsioonispetsialist")
new$new = asendaja(new$new, "kommunikatsiooni", "nõu", replace = "kommunikatsioonispetsialist")
new$new = asendaja(new$new, "kommunikatsioonipet", "", replace = "kommunikatsioonispetsialist")
new$new = asendaja(new$new, "kommunikatsiooni", "partner", replace = "kommunikatsioonispetsialist")
new$new = asendaja(new$new, "pressinõunik", "", replace = "kommunikatsioonispetsialist")
new$new = asendaja(new$new, "turunduskommunikatsioon", "", replace = "kommunikatsioonispetsialist")

# siis on veel kommunikatsioonijuhid, kes võivad olla nii juhid kui spetsialistid 
#ISCO-s kommunikatsioonijuht tippspetsialistide all (tegin ka praegu nii), samas on eraldi juhtide all "kommunikatsiooniosakonna juhataja")
unique(leidur(new$new, "kommunikatsioonijuht", ""))
new$new = asendaja(new$new, "kommunikatsioonijuht", "", excl1 = "ettevõtja", excl2 = "äriarendus", replace = "kommunikatsioonijuht")
new$new = asendaja(new$new, "kommunikatsiooni", "juht", excl1 = "ettevõtja", excl2 = "äriarendus", excl3 = "telekomm", excl4 = "müügi", replace = "kommunikatsioonijuht")

unique(leidur(new$new, "treener", ""))

unique(leidur(new$new, "treener", "koer"))
new$new = asendaja(new$new, "treener", "koer", replace = "loomahooldaja")

new$new = asendaja(new$new, "treener", "", excl1 = "juht", excl2 = "õpetaja", excl3 = "ärihaldur", excl4 = "mälu", replace = "treener")
new$new = asendaja(new$new, "treener-", "", replace = "treener")
new$new = asendaja(new$new, "pilateseõpetaja", "", replace = "treener")
new$new = asendaja(new$new, "jooga", "", excl1 = "juh", replace = "treener")
new$new = asendaja(new$new, "spordikohtunik", "", replace = "treener")

unique(leidur(new$new, "kehalise", "õpet"))
new$new = asendaja(new$new, "kehalise", "õpet", replace = "kehalisekasvatuseõpetaja")
new$new = asendaja(new$new, "kehaline", "õpet", replace = "kehalisekasvatuseõpetaja")
new$new = asendaja(new$new, "ujum", "õpet", replace = "kehalisekasvatuseõpetaja")
new$new = asendaja(new$new, "liikum", "õpet", excl1 = "lava", replace = "kehalisekasvatuseõpetaja")

unique(leidur(new$new, "klassi", "õpet"))
new$new = asendaja(new$new, "klassi", "õpet", excl1 = "balleti", replace = "algkooliõpetaja")
new$new = asendaja(new$new, "pikapäeva", "õpet", replace = "põhikooliõpetaja")

unique(leidur(new$new, "tõlk", ""))
new$new = asendaja(new$new, "tõlk", "", excl1 = "rühma", excl2 = "õpetaja", excl3 = "korraldaja", excl4 = "juht", replace = "filoloog")

unique(leidur(new$new, "investor", ""))
new$new = asendaja(new$new, "investor","", excl1 = "head", excl2 = "juhtiv", excl3 = "suhete", replace = "finantsnõustaja")

unique(leidur(new$new, "teadur", ""))
new$new = asendaja(new$new, "teadur-","", excl1 = "leksiko", excl2 = "lõhna", replace = "teadur")
new$new = asendaja(new$new, "teadlaneja","", replace = "teadur")

unique(leidur(new$new, "teadlane", ""))
new$new = asendaja(new$new, "teadlane", "kaas", replace = "teadur")
new$new = asendaja(new$new, "teadlane", "külalis", replace = "teadur")
new$new = asendaja(new$new, "teadlane", "noorem", replace = "teadur")

unique(leidur(new$new, "lektor", ""))
new$new = asendaja(new$new, "lektor", "", excl1 = "juhataja", excl2 = "ettevõtja", excl3 = "teadur", excl4 = "terapeut", replace = "õppejõud")
new$new = asendaja(new$new, "dotsent", "", replace = "õppejõud")
new$new = asendaja(new$new, "õppejõud", "kõrgk", excl1 = "juhat", replace = "õppejõud")
new$new = asendaja(new$new, "õppejõud", "", excl1 = "juh", excl2 = "disainer", excl3 = "õpetajaja", excl4 = "meditsiini", replace = "õppejõud")
new$new = asendaja(new$new, "lektor", "ülik", replace = "õppejõud")
new$new = asendaja(new$new, "õpetaja", "ülik", replace = "õppejõud")
new$new = asendaja(new$new, "õpetaja", "kõrg", replace = "õppejõud")

unique(leidur(new$new, "disainer", "toote"))
new$new = asendaja(new$new, "disainer", "toote", replace = "tootedisainer")
new$new = asendaja(new$new, "ehtedisainer", "", replace = "tootedisainer")

unique(leidur(new$new, "kunstnik", ""))
new$new = asendaja(new$new, "kunstnik", "", excl1 = "retkejuht", excl2 = "õpetaja", excl3 = "terapeut", excl4 = "ehitus", replace = "kunstnik")
new$new = asendaja(new$new, "keraamik", "", replace = "kunstnik")

unique(leidur(new$new, "müügi", "assis"))
new$new = asendaja(new$new, "müügi", "assis", replace = "raamatupidamisekontoritöötaja")

unique(leidur(new$new, "audiitor",""))
unique(leidur(new$new, "audiitor","sise"))
new$new = asendaja(new$new, "siseaud", "", replace = "juhtimisanalüütik")
new$new = asendaja(new$new, "rektor", "nõunik", replace = "juhtimisanalüütik")
new$new = asendaja(new$new, "andmekaitsespet", "", replace = "juhtimisanalüütik")
new$new = asendaja(new$new, "ärikons", "", replace = "juhtimisanalüütik")
new$new = asendaja(new$new, "juhtimiskons", "", replace = "juhtimisanalüütik")
new$new = asendaja(new$new, "juhtimisanal", "", replace = "juhtimisanalüütik")
new$new = asendaja(new$new, "ärianal", "konsul", replace = "juhtimisanalüütik")
new$new = asendaja(new$new, "koordinaator", "arendus", replace = "juhtimisanalüütik")
new$new = asendaja(new$new, "nõun", "arendus", replace = "juhtimisanalüütik")
new$new = asendaja(new$new, "äriarhitekt", "", replace = "juhtimisanalüütik")

new$new = asendaja(new$new, "audiitor", "", excl1 = "keskk", replace = "audiitor")

unique(leidur(new$new, "teenindusjuht",""))
unique(leidur(new$new, "teenindusjuht","kauba"))
new$new = asendaja(new$new, "teenindusjuht", "kauba", replace = "klienditeenindusjuht")
new$new = asendaja(new$new, "müügiteenindusjuht", "", replace = "klienditeenindusjuht")
new$new = asendaja(new$new, "teenindusjuht", "hotelli", replace = "klienditeenindusjuht")
new$new = asendaja(new$new, "klienditeenindusejuh", "", replace = "klienditeenindusjuht")

new$new = asendaja(new$new, "teenindusjuht", "toit", replace = "toitlustuseteenindusjuht")
new$new = asendaja(new$new, "teenindusjuht", "resto", replace = "toitlustuseteenindusjuht")

unique(leidur(new$new, "aednik",""))
new$new = asendaja(new$new, "aednik", "", excl1 = "käsitöö", excl2 = "müüja", excl3 = "laste", replace = "aednik")
new$new = asendaja(new$new, "puu", "hooldaja", replace = "aednik")
new$new = asendaja(new$new, "aednik-", "", replace = "aednik")

unique(leidur(new$new, "radio","tehnik"))
new$new = asendaja(new$new, "radio", "tehnik", replace = "radioloogiatehnik")
new$new = asendaja(new$new, "ultrahelisp", "", replace = "radioloogiatehnik")

unique(leidur(new$new, "lapsehoi",""))
new$new = asendaja(new$new, "lapsehoi", "", excl1 = "sekretär", excl2 = "kokk", replace = "lapsehoidja")
new$new = asendaja(new$new, "öökasva", "", replace = "lapsehoidja")
new$new = asendaja(new$new, "lastehoiu", "", excl1 = "juh", excl2 = "omanik", replace = "lapsehoidja")
new$new = asendaja(new$new, "lapse", "tugiisik", replace = "lapsehoidja")
new$new = asendaja(new$new, "laste", "tugiisik", replace = "lapsehoidja")
new$new = asendaja(new$new, "laste", "hoid", excl1 = "juht", replace = "lapsehoidja")
new$new = asendaja(new$new, "tugiisik", "kool", replace = "lapsehoidja")
new$new = asendaja(new$new, "tugiisik", "noor", replace = "lapsehoidja")

new$new = asendaja(new$new, "lasteho", "juh", replace = "lasteaiajuht")
new$new = asendaja(new$new, "lastehoiu", "oman", replace = "lasteaiajuht")
new$new = asendaja(new$new, "lasteaia", "juh", excl1 = "huvik", replace = "lasteaiajuht")
new$new = asendaja(new$new, "lasteasut", "juh", replace = "lasteaiajuht")

# ISCO-s eristatakse põllumajanduses lihttöötajaid ja oskustöötajaid - vahel on täpsustatud, alati mitte - ühte panna?
unique(leidur(new$new, "põllutöö", ""))
new$new = asendaja(new$new, "põllutöö", "", replace = "põllumajandustöötaja")
new$new = asendaja(new$new, "põllumees", "", replace = "põllumajandustöötaja")
new$new = asendaja(new$new, "põllu", "töö", excl1 = "juh", excl2 = "ettevõtja", replace = "põllumajandustöötaja")
new$new = asendaja(new$new, "talu", "töö", replace = "põllumajandustöötaja")
new$new = asendaja(new$new, "talunik", "", excl1 = "abikaasa", excl2 = "peaenergeetik", excl3 = "riiklik", replace = "põllumajandustöötaja")
new$new = asendaja(new$new, "talupida", "", excl1 = "turismi", replace = "põllumajandustöötaja")
new$new = asendaja(new$new, "farmi", "", excl1 = "juh", excl2 = "operaator", excl3 = "sekretär", replace = "põllumajandustöötaja")

new$new = asendaja(new$new, "lüps", "",replace = "loomakasvataja")
new$new = asendaja(new$new, "loomakasvat", "", excl1 = "mehhan", replace = "loomakasvataja")
new$new = asendaja(new$new, "hobusekasvat", "", replace = "loomakasvataja")
new$new = asendaja(new$new, "kasvat", "veis", replace = "loomakasvataja")
new$new = asendaja(new$new, "kasvat", "karja", replace = "loomakasvataja")
new$new = asendaja(new$new, "kasvat", "kass", replace = "loomakasvataja")
new$new = asendaja(new$new, "farmijuhataja", "", replace = "loomakasvataja")

new$new = asendaja(new$new, "kasvata", "kala", replace = "kalakasvataja")

new$new = asendaja(new$new, "kasvat", "taim", replace = "taimekasvataja")
new$new = asendaja(new$new, "kasvat", "lill", replace = "taimekasvataja")

unique(leidur(new$new, "turvatöö", ""))
new$new = asendaja(new$new, "turvatöö", "", excl1 = "ankass", replace = "turvatöötaja")
new$new = asendaja(new$new, "turvamees", "", replace = "turvatöötaja")
new$new = asendaja(new$new, "valvetöötaja", "", replace = "turvatöötaja")
new$new = asendaja(new$new, "turvateenuseosut", "", replace = "turvatöötaja")
new$new = asendaja(new$new, "muuseumi", "valvur", replace = "turvatöötaja")
new$new = asendaja(new$new, "ranna", "valvur", replace = "turvatöötaja")
new$new = asendaja(new$new, "vetel", "päästja", replace = "turvatöötaja")
new$new = asendaja(new$new, "turvajuht", "", replace = "turvatöötaja") # kutsestandard sellise nimetusega
new$new = asendaja(new$new, "häirejuhtimiskeskusespetsialist", "", replace = "turvatöötaja")

unique(leidur(new$new, "metoodik", ""))
new$new = asendaja(new$new, "metoodik", "", excl1 = "juht", excl2 = "arst", excl3 = "mängudi", replace = "õppemetoodikaspetsialist")
new$new = asendaja(new$new, "juhtõpetaja", "", replace = "õppemetoodikaspetsialist")
new$new = asendaja(new$new, "programmijuht", "ülik", replace = "õppemetoodikaspetsialist")
new$new = asendaja(new$new, "haridus", "nõu", replace = "õppemetoodikaspetsialist")
new$new = asendaja(new$new, "haridus", "spets", excl1 = "tugi", replace = "õppemetoodikaspetsialist")
new$new = asendaja(new$new, "juhtivõpe", "", replace = "õppemetoodikaspetsialist")
new$new = asendaja(new$new, "erialajuht", "", replace = "õppemetoodikaspetsialist")

unique(leidur(new$new, "päästja", ""))
new$new = asendaja(new$new, "päästja", "", excl1 = "vetel", excl2 = "lennujuht", excl3 = "instrukt", replace = "päästja")
new$new = asendaja(new$new, "tuletõrjuja", "", replace = "päästja")
new$new = asendaja(new$new, "deminee", "", replace = "päästja")
new$new = asendaja(new$new, "pääste", "töötaja", replace = "päästja")

unique(leidur(new$new, "sekretär", ""))
unique(leidur(new$new, "sekretär-", ""))
new$new = asendaja(new$new, "sekretär-", "", replace = "sekretär")

new$new = asendaja(new$new, "sekretär", "haigla", replace = "meditsiinisekretär")
new$new = asendaja(new$new, "sekretär", "ravi", replace = "meditsiinisekretär")
new$new = asendaja(new$new, "sekretär", "kliinik", replace = "meditsiinisekretär")
new$new = asendaja(new$new, "sekretär", "patoloogia", replace = "meditsiinisekretär")
new$new = asendaja(new$new, "sekretär", "medits", replace = "meditsiinisekretär")

unique(leidur(new$new, "ajaloolane", ""))
new$new = asendaja(new$new, "ajaloolane", "", replace = "ajaloolane-filosoof-politoloog") #need kõik sama koodi all
new$new = asendaja(new$new, "filos", "", replace = "ajaloolane-filosoof-politoloog")

unique(leidur(new$new, "arhivaar", ""))
new$new = asendaja(new$new, "arhivaar", "", replace = "arhivaar-kuraator")
new$new = asendaja(new$new, "kuraator", "", excl1 = "korrashoidu", excl2 = "botaanik",  replace = "arhivaar-kuraator")
new$new = asendaja(new$new, "varahoidja", "",  replace = "arhivaar-kuraator")
new$new = asendaja(new$new, "galerist", "",  replace = "arhivaar-kuraator")
new$new = asendaja(new$new, "muuseumiteadur", "",  replace = "arhivaar-kuraator")
new$new = asendaja(new$new, "konservaator", "",  replace = "arhivaar-kuraator")
new$new = asendaja(new$new, "arhiivi", "spets",  replace = "arhivaar-kuraator")
new$new = asendaja(new$new, "koguhoidja", "", excl1 = "raamatu", excl2 = "ramatu", replace = "arhivaar-kuraator")

unique(leidur(new$new, "sünoptik", ""))
new$new = asendaja(new$new, "sünoptik","", replace = "meteoroloog")
new$new = asendaja(new$new, "meteoroloog","", replace = "meteoroloog")

unique(leidur(new$new, "optometrist", ""))
new$new = asendaja(new$new, "optometrist","", replace = "optometrist")
new$new = asendaja(new$new, "optik","", excl1 = "konsultant", excl2 = "müük", excl3 = "võrgu", excl4 = "kliendi", replace = "optometrist")
new$new = asendaja(new$new, "optikkl","", replace = "optometrist")

unique(leidur(new$new, "huvijuht", ""))
new$new = asendaja(new$new, "huvijuht","", excl1 = "kommunika", replace = "pedagoogikatippspetsialist")
new$new = asendaja(new$new, "õppegrupp", "juh",replace = "pedagoogikatippspetsialist")
new$new = asendaja(new$new, "haridustehnol", "", excl1 = "juh", excl2 = "õpetaja-", replace = "pedagoogikatippspetsialist")
new$new = asendaja(new$new, "ringijuht", "", excl1 = "õpetaja", replace = "pedagoogikatippspetsialist")
new$new = asendaja(new$new, "õppedisain", "", replace = "pedagoogikatippspetsialist")
new$new = asendaja(new$new, "õppenõu", "", replace = "pedagoogikatippspetsialist")
new$new = asendaja(new$new, "õpilas", "nõu", replace = "pedagoogikatippspetsialist")
new$new = asendaja(new$new, "muuseum", "pedagoog", replace = "pedagoogikatippspetsialist")
new$new = asendaja(new$new, "arhiiv", "õpetaja", replace = "pedagoogikatippspetsialist")
new$new = asendaja(new$new, "õppe", "arend", replace = "pedagoogikatippspetsialist")
new$new = asendaja(new$new, "koordinaator", "õppe", excl1 = "õpetaja", replace = "pedagoogikatippspetsialist")
new$new = asendaja(new$new, "koordinaator", "kool", excl1 = "välissuhete", excl2 = "teadus", replace = "pedagoogikatippspetsialist")
new$new = asendaja(new$new, "koordinaator", "kool", excl1 = "välissuhete", excl2 = "teadus", replace = "pedagoogikatippspetsialist")
new$new = asendaja(new$new, "koordinaator", "õpi", replace = "pedagoogikatippspetsialist")
new$new = asendaja(new$new, "huvi-", "juht", replace = "pedagoogikatippspetsialist")
new$new = asendaja(new$new, "haridus", "ametnik", replace = "pedagoogikatippspetsialist")
new$new = asendaja(new$new, "pedagoogikatippspetsialist", "", replace = "pedagoogikatippspetsialist")
new$new = asendaja(new$new, "õppekonsultant", "", replace = "pedagoogikatippspetsialist")

unique(leidur(new$new, "agronoom", ""))
new$new = asendaja(new$new, "agronoom", "", excl1 = "abi", replace = "agronoom")
new$new = asendaja(new$new, "konsulent", "", replace = "agronoom")

unique(leidur(new$new, "pakkija", ""))
new$new = asendaja(new$new, "pakkija", "", excl1 = "villija", excl2 = "kontrol", replace = "pakkija")

unique(leidur(new$new, "koolitusjuh", ""))
new$new = asendaja(new$new, "koolitusjuh", "", excl1 = "lennu", excl2 = "kutsek", excl3 = "ülik", excl4 = "õppejõud", replace = "töötajate koolitusspetsialist")
new$new = asendaja(new$new, "personaliaren", "", replace = "töötajate koolitusspetsialist")
new$new = asendaja(new$new, "koolitus", "arendus", replace = "töötajate koolitusspetsialist")
new$new = asendaja(new$new, "töötajatekoolitusspetsialist", "", replace = "töötajate koolitusspetsialist")
new$new = asendaja(new$new, "täienduskoolitusespetsialist", "", replace = "töötajate koolitusspetsialist")

# analoogne olukord nagu sotsiaaltöötajatega - noorsootöötajaid on kahes astmes (tippspetsialistid ja keskastmespetsialistid)
unique(leidur(new$new, "noorsoo", "töö"))
new$new = asendaja(new$new, "noorsoo", "töö", excl1 = "kutse", excl2 = "keskuse", excl3 = "juhataja", excl4 = "omavalitsus", replace = "sotsiaaltöötaja")

unique(leidur(new$new, "käsitööline", ""))
new$new = asendaja(new$new, "käsitööline", "", excl1 = "disainer", replace = "käsitööline")
new$new = asendaja(new$new, "käsitöö", "meister", excl1 = "kliendi", replace = "käsitööline")
new$new = asendaja(new$new, "käsitöö", "tegi", replace = "käsitööline")
new$new = asendaja(new$new, "käsitöö", "", excl1 = "kunsti", excl2 = "juh", replace = "käsitööline")

unique(leidur(new$new, "projekteerija", "insener"))
new$new = asendaja(new$new, "projekteerija", "insener", excl1 = "müügi", replace = "ehitusinsener")
new$new = asendaja(new$new, "teedeinsener", "", replace = "ehitusinsener")

unique(leidur(new$new, "projekteerija", "ehitus"))
new$new = asendaja(new$new, "projekteerija", "ehitus", replace = "ehitusprojekteerija")
new$new = asendaja(new$new, "projekteerija", "hoone", replace = "ehitusprojekteerija")
new$new = asendaja(new$new, "projekteerija", "konstruktor", replace = "ehitusprojekteerija")
new$new = asendaja(new$new, "projekteerija", "joon", replace = "ehitusprojekteerija")
new$new = asendaja(new$new, "projekteerija", "teede", replace = "ehitusprojekteerija")
new$new = asendaja(new$new, "ehituskonstruktor", "", replace = "ehitusprojekteerija")

new$new = asendaja(new$new, "projekteerija", "elektri", replace = "elektriinsener")
new$new = asendaja(new$new, "insener", "elektri", replace = "elektriinsener")

# ja siis on terve hulk lihtsalt projekteerijaid, kelle ma varasemalt liigitasin ehitusprojekteerijate alla
new$new = asendaja(new$new, "juhtiv", "projekteerija", replace = "projekteerija")
new$new = asendaja(new$new, "vanem", "projekteerija", replace = "projekteerija")
new$new = asendaja(new$new, "pea", "projekteerija", replace = "projekteerija")
unique(leidur(new$new, "projekteerija", ""))

unique(leidur(new$new, "kutseõpetaja", ""))
new$new = asendaja(new$new, "kuts", "õpetaja", excl1 = "juhatuseliige", replace = "kutseõpetaja")
new$new = asendaja(new$new, "eriala", "õpetaja", replace = "kutseõpetaja")
new$new = asendaja(new$new, "praktika", "juhendaja", replace = "kutseõpetaja")
new$new = asendaja(new$new, "meister-tehno", "", replace = "kutseõpetaja")
new$new = asendaja(new$new, "õpetaja", "ametik", replace = "kutseõpetaja")
new$new = asendaja(new$new, "õpetaja", "meister", replace = "kutseõpetaja")
new$new = asendaja(new$new, "tehnik-õpetaja", "", replace = "kutseõpetaja")
new$new = asendaja(new$new, "koolitusjuhtkutse", "", replace = "kutseõpetaja")
new$new = asendaja(new$new, "koolitusjuhtlennu", "", replace = "kutseõpetaja")

unique(leidur(new$new, "toimetaja", ""))
new$new = asendaja(new$new, "uudis", "toimetaja", replace = "ajakirjanik")
new$new = asendaja(new$new, "tele", "toimetaja", replace = "ajakirjanik")
new$new = asendaja(new$new, "pea", "toimetaja", replace = "ajakirjanik")
new$new = asendaja(new$new, "lehe", "toimetaja", replace = "ajakirjanik")
new$new = asendaja(new$new, "meedia", "toimetaja", replace = "ajakirjanik")
new$new = asendaja(new$new, "proosa", "toimetaja", replace = "ajakirjanik")
new$new = asendaja(new$new, "raadio", "toimetaja", replace = "ajakirjanik")
new$new = asendaja(new$new, "sisuturundus", "toimetaja", replace = "ajakirjanik")
new$new = asendaja(new$new, "reklaami", "toimetaja", replace = "ajakirjanik")


new$new = asendaja(new$new, "raamatu", "toimetaja", replace = "kirjanik")
new$new = asendaja(new$new, "õpik", "toimetaja", replace = "kirjanik")
new$new = asendaja(new$new, "kirjand", "tead", replace = "kirjanik")
new$new = asendaja(new$new, "kirjanik", "", excl1 = "aja", replace = "kirjanik")
new$new = asendaja(new$new, "kirjastaja", "", replace = "kirjanik")
new$new = asendaja(new$new, "dramaturg", "", replace = "kirjanik")

# lihtsalt toimetajaid on ka palju, kelle puhul raske otsustada, kas kirjanik, ajakirjanik või filoloog
new$new = asendaja(new$new, "tegev", "toimetaja", replace = "toimetaja")
new$new = asendaja(new$new, "vanem", "toimetaja", replace = "toimetaja")
new$new = asendaja(new$new, "vastutav", "toimetaja", replace = "toimetaja")
unique(leidur(new$new, "toimetaja", ""))

# komplekteerija ehk (käsitsi)pakkija? Pole kindel, kas on parim valik, aga paremat ka silma ei jäänud
# samas elektroonikaseadmete komplekteerijad jm on pigem elektroonikaseadmete koostajad
# seega lihtsalt "komplekteerijad" on jällegi keeruline kodeerida (varasemalt panin vist pakkijate alla, aga nüüd pigem kahtlen)
unique(leidur(new$new, "komplekteerija", ""))
new$new = asendaja(new$new, "komplekteerija", "elekt", replace = "elektriseadmete koostaja")
new$new = asendaja(new$new, "komplekteerija", "mootor", replace = "elektriseadmete koostaja")
new$new = asendaja(new$new, "komplekteerija", "arvut", replace = "elektriseadmete koostaja")
new$new = asendaja(new$new, "koostaja", "elekt", replace = "elektriseadmete koostaja")
new$new = asendaja(new$new, "oper", "smd", replace = "elektriseadmete koostaja")

new$new = asendaja(new$new, "komplekteerija", "kauba", replace = "pakkija")
new$new = asendaja(new$new, "komplekteerija", "köögivilja", replace = "pakkija")
new$new = asendaja(new$new, "komplekteerija", "lao", replace = "pakkija")
new$new = asendaja(new$new, "komplekteerija", "trüki", replace = "pakkija")

new$new = asendaja(new$new, "komplekteerija", "pui", replace = "muudkoostajad")
new$new = asendaja(new$new, "komplekteerija", "mööbl", replace = "muudkoostajad")
new$new = asendaja(new$new, "koostaja", "too", replace = "muudkoostajad")
new$new = asendaja(new$new, "jalgrat", "kooste", replace = "muudkoostajad")
new$new = asendaja(new$new, "jalgratta", "kinnit", replace = "muudkoostajad")
new$new = asendaja(new$new, "koost", "oper", replace = "muudkoostajad")
new$new = asendaja(new$new, "koost", "avatäide", replace = "muudkoostajad")
new$new = asendaja(new$new, "segatoodetekoostaja", "", replace = "muudkoostajad")
new$new = asendaja(new$new, "mööblipaigal", "", replace = "muudkoostajad")
new$new = asendaja(new$new, "valgustitekoostaja", "", replace = "muudkoostajad")

# variant ka kõik koostajad ühte panna? Kuna lihtsalt "koostajaid" on ka kõige rohkem

new$new = asendaja(new$new, "komplekteeria", "", replace = "komplekteerija")

unique(leidur(new$new, "kohtunik", ""))
new$new = asendaja(new$new, "kohtunik", "", excl1 = "abi", excl2 = "tennis", excl3 = "jalgpall", excl4 = "ratsa", replace = "kohtunik")

# kutseregistri alusel on floristid kioskimüüjad (ehk 5211), aga jätsin praegu eraldi nimetusena
unique(leidur(new$new, "florist", ""))
new$new = asendaja(new$new, "florist", "", excl1 = "juh", replace = "florist")
new$new = asendaja(new$new, "lillesead", "", excl1 = "juh", replace = "florist")
new$new = asendaja(new$new, "lillemüü", "", excl1 = "juh", replace = "florist")

unique(leidur(new$new, "valvur", ""))
new$new = asendaja(new$new, "valvur", "vang", replace = "vangivalvur")
new$new = asendaja(new$new, "ametnik", "vang", replace = "vangivalvur")

new$new = asendaja(new$new, "valvur", "", excl1 = "piiri", excl2 = "vangi", replace = "muulihttööline")

unique(leidur(new$new, "tegevus", "terapeut"))
new$new = asendaja(new$new, "tegevus", "terapeut", replace = "tegevusterapeut")

unique(leidur(new$new, "mesinik", ""))
new$new = asendaja(new$new, "mesinik", "", replace = "mesinik")

unique(leidur(new$new, "diplomaat", ""))
new$new = asendaja(new$new, "diplomaat", "", replace = "kõrgem valitsusametnik")

# ISCO-s eristatakse tippspetsialisti ja spetsialisti - ma praegu ei eristanud, sest neid, kes on end "pea", "juhtiv" jne spetsiks määratlenud, on väga vähe
unique(leidur(new$new, "töökesk", "spets"))
new$new = asendaja(new$new, "töökesk", "spets", excl1 = "juhataja", excl2 = "person", replace = "töökeskkonnaspetsialist")
new$new = asendaja(new$new, "tööohutus", "", replace = "töökeskkonnaspetsialist")

unique(leidur(new$new, "keskkonna", "spets"))
new$new = asendaja(new$new, "keskkonnaspets", "", excl1 = "juhataja", excl2 = "hange", excl3 = "ehitus", excl4 = "töö", replace = "keskkonnanõunik")
new$new = asendaja(new$new, "keskkonna", "spets", excl1 = "töö", excl2 = "ehitus", excl3 = "juhataja", excl4 = "eritöö", replace = "keskkonnanõunik")
new$new = asendaja(new$new, "loodus", "kaitse", excl1 = "juht", replace = "keskkonnanõunik")
new$new = asendaja(new$new, "keskkonna", "nõu", replace = "keskkonnanõunik")
new$new = asendaja(new$new, "keskkonna", "audii", replace = "keskkonnanõunik")
new$new = asendaja(new$new, "keskkonna", "kons", excl1 = "töö", excl2 = "juht", replace = "keskkonnanõunik")
new$new = asendaja(new$new, "ökoloog", "", replace = "keskkonnanõunik")
new$new = asendaja(new$new, "keskkonna", "amet", replace = "keskkonnanõunik")
new$new = asendaja(new$new, "keskkonna", "eksp", excl1 = "töö", replace = "keskkonnanõunik")

unique(leidur(new$new, "reisi", "konsul"))
new$new = asendaja(new$new, "reisi", "konsul", replace = "reisikonsultant")
new$new = asendaja(new$new, "turismiinfo", "", replace = "reisikonsultant")
new$new = asendaja(new$new, "turismispets", "", replace = "reisikonsultant")
new$new = asendaja(new$new, "turismi", "töö", replace = "reisikonsultant")
new$new = asendaja(new$new, "reisi", "korr", replace = "reisikonsultant")
new$new = asendaja(new$new, "reisi", "agent", replace = "reisikonsultant")
new$new = asendaja(new$new, "reisi", "nõustaja", replace = "reisikonsultant")

unique(leidur(new$new, "infospetsialist", ""))
new$new = asendaja(new$new, "infospetsialist", "", excl1 = "karjäär", replace = "klienditoe konsultant")
new$new = asendaja(new$new, "info", "klienditee", replace = "klienditoe konsultant")
new$new = asendaja(new$new, "klienditoe", "kons", replace = "klienditoe konsultant")
new$new = asendaja(new$new, "klienditoe", "haldur", replace = "klienditoe konsultant")
new$new = asendaja(new$new, "klienditoe", "haldur", replace = "klienditoe konsultant")
new$new = asendaja(new$new, "klienditoe", "spets", replace = "klienditoe konsultant")
new$new = asendaja(new$new, "infotöötaja", "", excl1 = "sadul", replace = "klienditoe konsultant")
new$new = asendaja(new$new, "infosekretär", "", replace = "klienditoe konsultant")

unique(leidur(new$new, "kahjukäsitleja", ""))
new$new = asendaja(new$new, "kahjukäsitleja", "", replace = "hindaja")
new$new = asendaja(new$new, "kahju", "käsit", excl1 = "juht", replace = "hindaja")
new$new = asendaja(new$new, "kindlus", "hind", replace = "hindaja")
new$new = asendaja(new$new, "kinnisvara", "hind", replace = "hindaja")
new$new = asendaja(new$new, "hindaja", "risk", excl1 = "taime", replace = "hindaja")
new$new = asendaja(new$new, "hindaja", "", excl1 = "taime", excl2 = "kval", excl3 = "maakler", excl4 = "kliiniline", replace = "hindaja")

unique(leidur(new$new, "reklaamispetsialist", ""))
new$new = asendaja(new$new, "reklaami", "spets", replace = "reklaamispetsialist")
new$new = asendaja(new$new, "brändijuht", "", replace = "reklaamispetsialist")
new$new = asendaja(new$new, "ettevõtlusspets", "", replace = "reklaamispetsialist")
new$new = asendaja(new$new, "copywriter", "", replace = "reklaamispetsialist")
new$new = asendaja(new$new, "reklaami", "kons", replace = "reklaamispetsialist")
# teenusejuhi olemuses natuke kahtlen, aga guugeldades näib, et on sarnane reklaami-turundus-müügispetsialistidele
new$new = asendaja(new$new, "teenusejuht", "", excl1 = "võrgu", excl2 = "rehab", excl3 = "juhataja", replace = "reklaamispetsialist")
new$new = asendaja(new$new, "kliendisuh", "juht", replace = "reklaamispetsialist")
new$new = asendaja(new$new, "loovjuht", "", excl1 = "õpetaja", replace = "reklaamispetsialist")
new$new = asendaja(new$new, "kliendikogemu", "juht", replace = "reklaamispetsialist")
new$new = asendaja(new$new, "kliendilahend", "juht", replace = "reklaamispetsialist")

table(leidur(new$new, "abi", "arst"))
new$new = asendaja(new$new, "abiarst", "", excl1 = "õendus", replace = "abiarst")

unique(leidur(new$new, "õpilane", ""))
new$new = asendaja(new$new, "õpilane", "", excl1 = "õpetaja", excl2 = "üliõp", replace = "õpilane")

unique(leidur(new$new, "tarneahela", "juht"))
new$new = asendaja(new$new, "tarneahela", "juht", excl1 = "spetsialist", replace = "tarneahelajuht")

# "tarneahelaspetsialiste" ISCO-s pole - tundub, et nad on kombinatsioon hankespetsialistidest-ostjatest ja logistikutest? Varem panin hankespetsialistiks
# ühe ingliskeelse kodeerimistabeli järgi on nad ikka hankespetsialistideks liigitatud
unique(leidur(new$new, "tarneahela", "spets"))
new$new = asendaja(new$new, "tarneahela", "spets", replace = "tarneahelaspetsialist")
new$new = asendaja(new$new, "tarneahela", "spets", replace = "hankespetsialist")
new$new = asendaja(new$new, "ostja", "", excl1 = "vanaraua", excl2 = "testostja", replace = "hankespetsialist")

unique(leidur(new$new, "arendusspetsialist", "",))
new$new = asendaja(new$new, "arendusspetsialist", "", excl1 = "toote", replace = "juhtimisanalüütik")
new$new = asendaja(new$new, "arendus", "spets", excl1 = "toote", excl2 = "biopr", excl3 = "süsteem", excl4 = "müügitoe", replace = "juhtimisanalüütik")

unique(leidur(new$new, "maakler", "tolli"))
new$new = asendaja(new$new, "maakler", "tolli", replace = "tollimaakler")
new$new = asendaja(new$new, "dekla", "tolli", replace = "tollimaakler")
new$new = asendaja(new$new, "spets", "tolli", replace = "tollimaakler")
new$new = asendaja(new$new, "tolliagent", "", excl1 = "juh", replace = "tollimaakler")
new$new = asendaja(new$new, "ekspedeerija", "", replace = "tollimaakler")

unique(leidur(new$new, "insp", "tolli"))
new$new = asendaja(new$new, "insp", "tolli", replace = "tolliinspektor")
new$new = asendaja(new$new, "amet", "tolli", replace = "tolliinspektor")
new$new = asendaja(new$new, "amet", "passi", replace = "tolliinspektor")

unique(leidur(new$new, "maakler", "kindlustus"))
new$new = asendaja(new$new, "maakler", "kindlustus", replace = "kindlustusagent")
new$new = asendaja(new$new, "agen", "kindlustus", replace = "kindlustusagent")
new$new = asendaja(new$new, "müügit", "kindlustus", replace = "kindlustusagent")
new$new = asendaja(new$new, "spets", "kindlustus", excl1 = "sotsiaal", replace = "kindlustusagent")
new$new = asendaja(new$new, "", "kindlustus", excl1 = "sotsiaal", excl2 = "juht", excl3 = "teenindaja", excl4 = "tugi", replace = "kindlustusagent")

unique(leidur(new$new, "mehaanika", "insener"))
new$new = asendaja(new$new, "mehaanika", "insener", excl1 = "juht", replace = "mehaanikainsener")
new$new = asendaja(new$new, "lennu", "insener", replace = "mehaanikainsener")
new$new = asendaja(new$new, "laeva", "insener", replace = "mehaanikainsener")
new$new = asendaja(new$new, "vent", "insener", replace = "mehaanikainsener")
new$new = asendaja(new$new, "mehh", "insener", excl1 = "hooldus", replace = "mehaanikainsener")
new$new = asendaja(new$new, "mehaanik", "insener", excl1 = "hooldus", excl2 = "juht", replace = "mehaanikainsener")

# õppekorraldusspetsialistid on koodilt samad mis juhiabid
unique(leidur(new$new, "õppekorrald", "spets"))
new$new = asendaja(new$new, "õppekorrald", "spets", replace = "juhiabi")
new$new = asendaja(new$new, "õppesekretär", "", replace = "juhiabi")
new$new = asendaja(new$new, "praktikakoordi", "", replace = "juhiabi")
new$new = asendaja(new$new, "õppekorraldaja", "", replace = "juhiabi")
new$new = asendaja(new$new, "õppespetsialist", "", replace = "juhiabi")

unique(leidur(new$new, "politsei", ""))
new$new = asendaja(new$new, "politsei", "uurija", replace = "politseiuurija")
new$new = asendaja(new$new, "politsei", "inspektor", replace = "politseiuurija")

unique(leidur(new$new, "uurija", ""))
new$new = asendaja(new$new, "uurija", "kriminaal", replace = "politseiuurija")
new$new = asendaja(new$new, "uurija", "eriasj", replace = "politseiuurija")
new$new = asendaja(new$new, "uurija", "väärt", replace = "politseiuurija")
new$new = asendaja(new$new, "jälitusametnik", "", replace = "politseiuurija")
# lihtsalt "uurijaid on >60, aga kas kõik on politseiuurijad? 

# neid politsei-ameteid on veel, aga ei saa täpselt aru, mis tasemel need eraldi nimetused on (igatühte ka ainult üks)
# ilmselt lihtsalt "politsei" võiks ka politseiniku alla sobida?
unique(leidur(new$new, "politsei", ""))

unique(leidur(new$new, "kasvataja", ""))
new$new = asendaja(new$new, "kasvataja", "", excl1 = "looma", excl2 = "köögiv", excl3 = "taime", excl4 = "kala", replace = "lapsehoidja")

unique(leidur(new$new, "keemik", "insener"))
new$new = asendaja(new$new, "keemi", "insener", replace = "keemiainsener")
new$new = asendaja(new$new, "keemi", "tehnol", replace = "keemiainsener")
new$new = asendaja(new$new, "toidu", "tehnol", excl1 = "tootmis", replace = "keemiainsener")

unique(leidur(new$new, "keemi", "labor", excl1 = "juh"))
new$new = asendaja(new$new, "keemi", "labor", excl1 = "juh", replace = "keemiatööstusetehnik")

unique(leidur(new$new, "keemik", ""))
new$new = asendaja(new$new, "keemik-", "", replace = "keemik")
new$new = asendaja(new$new, "keemik", "", replace = "keemik")

unique(leidur(new$new, "bio", "labor"))
new$new = asendaja(new$new, "bio", "labor", replace = "biotehnik")
new$new = asendaja(new$new, "bio", "tehnik", excl1 = "insener", replace = "biotehnik")

unique(leidur(new$new, "bioloog", ""))
new$new = asendaja(new$new, "kalandus", "tead", replace = "bioloog")
new$new = asendaja(new$new, "bioloog", "", excl1 = "tootejuht", excl2 = "müügi", replace = "bioloog")

unique(leidur(new$new, "autotehnik", ""))
new$new = asendaja(new$new, "autotehnik", "", replace = "masinaehitustehnik")
new$new = asendaja(new$new, "tehnik", "sõiduk", replace = "masinaehitustehnik")
new$new = asendaja(new$new, "tehnik", "lennu", replace = "masinaehitustehnik")
new$new = asendaja(new$new, "tehnik", "soojus", replace = "masinaehitustehnik")
new$new = asendaja(new$new, "tehno", "ülevaat", excl1 = "pealik", excl2 = "juh", replace = "masinaehitustehnik")
new$new = asendaja(new$new, "mootor", "meh", excl1 = "pealik", excl2 = "juh", replace = "masinaehitustehnik")
new$new = asendaja(new$new, "hüdrotehnikainsener", "", replace = "masinaehitustehnik")
new$new = asendaja(new$new, "masinaehitustehnik", "", replace = "masinaehitustehnik")

unique(leidur(new$new, "kiirabi","tehn"))
new$new = asendaja(new$new, "kiirabi", "tehn", replace = "kiirabitehnik")
new$new = asendaja(new$new, "erakorr", "tehn", replace = "kiirabitehnik")
new$new = asendaja(new$new, "parameedik", "", replace = "kiirabitehnik")

unique(leidur(new$new, "ehitusjuht",""))
new$new = asendaja(new$new, "ehitusjuht", "", excl1 = "projekteer", replace = "ehitusjuht")
new$new = asendaja(new$new, "ehitus", "juht", "ettevõtt", replace = "ehitusjuht")
new$new = asendaja(new$new, "ehitusprojektijuht", "", replace = "ehitusjuht")
new$new = asendaja(new$new, "projektijuhteh", "", replace = "ehitusjuht")

unique(leidur(new$new, "laborijuh",""))
new$new = asendaja(new$new, "laborijuh", "", excl1 = "koondis", replace = "laborijuht")
new$new = asendaja(new$new, "laboratoor", "juh", replace = "laborijuht")

unique(leidur(new$new, "kultuur", "töötaja"))
new$new = asendaja(new$new, "kultuur", "töötaja", excl1 = "juht", replace = "muu kultuurivaldkonna spetsialist")
new$new = asendaja(new$new, "kultuur", "spetsialist", replace = "muu kultuurivaldkonna spetsialist")

unique(leidur(new$new, "müügi", "töötaja"))
new$new = asendaja(new$new, "müügi", "töötaja", replace = "müügiesindaja")

unique(leidur(new$new, "perenaine", ""))
unique(leidur(new$new, "perenaine", "majutus"))
new$new = asendaja(new$new, "perenaine", "majutus", replace = "majapidaja")
new$new = asendaja(new$new, "perenaine", "hote", replace = "majapidaja")
new$new = asendaja(new$new, "perenaine", "", excl1 = "koduperenaine", excl2 = "õpetaja", excl3 = "juht", excl4 = "kokk", replace = "majapidaja")
new$new = asendaja(new$new, "haldusspets", "", excl1 = "it-hal", excl2 = "dokhal", excl3 = "kontroh", excl4 = "võlah", replace = "majapidaja")
new$new = asendaja(new$new, "majapidamis", "hote", replace = "majapidaja")
new$new = asendaja(new$new, "majutus", "hote", replace = "majapidaja")
new$new = asendaja(new$new, "õpilaskod", "juh", excl1 = "üliõp", replace = "majapidaja")

# täpsemalt määratlemata puidutöölised ja puidutöötlejad (ISCO-kood 752)
unique(leidur(new$new, "puidutöö", ""))
new$new = asendaja(new$new, "puidutöö", "", excl1 = "juh", excl2 = "teh", replace = "puidutööline")
new$new = asendaja(new$new, "puidulihvija", "", replace = "puidutööline")

unique(leidur(new$new, "tantsuõpe", ""))
new$new = asendaja(new$new, "tantsu", "õpetaja", excl1 = "keeleteh", excl2 = "kontor", replace = "tantsuõpetaja")
new$new = asendaja(new$new, "balle", "õpetaja", replace = "tantsuõpetaja")

# ilmselt mõistlik need trükiga seotud ametid kõik kokku panna?
unique(leidur(new$new, "trüki", "ette"))
new$new = asendaja(new$new, "trüki", "ette", excl1 = "juht", replace = "trükiettevalmistaja")
new$new = asendaja(new$new, "küljendaja", "", replace = "trükiettevalmistaja")
new$new = asendaja(new$new, "trüki", "järel", replace = "trükijäreltöötleja")
new$new = asendaja(new$new, "köitja", "", replace = "trükijäreltöötleja")
new$new = asendaja(new$new, "trüki", "töötleja", replace = "trükijäreltöötleja")

unique(leidur(new$new, "trükkal", ""))
new$new = asendaja(new$new, "trükkal", "", replace = "trükkal")
new$new = asendaja(new$new, "trükimas", "", replace = "trükkal")
new$new = asendaja(new$new, "trüki", "meister", replace = "trükkal")
new$new = asendaja(new$new, "trüki", "spets", replace = "trükkal")
new$new = asendaja(new$new, "trüki", "tööline", replace = "trükkal")
new$new = asendaja(new$new, "trüki", "operaator", replace = "trükkal")

unique(leidur(new$new, "laen", "hald"))
new$new = asendaja(new$new, "laen", "hald", replace = "laenuhaldur")
new$new = asendaja(new$new, "kredii", "hald", excl1 = "juht", replace = "laenuhaldur")
new$new = asendaja(new$new, "laen", "assis", replace = "laenuhaldur")
new$new = asendaja(new$new, "laen", "admin", replace = "laenuhaldur")
new$new = asendaja(new$new, "laen", "spets", replace = "laenuhaldur")

unique(leidur(new$new, "sõiduõpetaja", ""))
new$new = asendaja(new$new, "sõidu", "õp", replace = "sõiduõpetaja")
new$new = asendaja(new$new, "sõidu", "ins", excl1 = "lume", replace = "sõiduõpetaja")
new$new = asendaja(new$new, "auto", "õpet", replace = "sõiduõpetaja")
new$new = asendaja(new$new, "bussi", "õpet", replace = "sõiduõpetaja")
new$new = asendaja(new$new, "auto", "koolit", replace = "sõiduõpetaja")

unique(leidur(new$new, "madrus", ""))
new$new = asendaja(new$new, "madrus", "", replace = "madrus")
new$new = asendaja(new$new, "pootsman", "", replace = "madrus")

unique(leidur(new$new, "inkassaator", ""))
unique(leidur(new$new, "võla", ""))
new$new = asendaja(new$new, "inkassaator", "", replace = "inkassaator")
new$new = asendaja(new$new, "võla", "", excl1 = "juht", replace = "inkassaator")

unique(leidur(new$new, "pereterapeut", ""))
new$new = asendaja(new$new, "pereterapeut", "", replace = "psühholoog")

unique(leidur(new$new, "lihameister", ""))
new$new = asendaja(new$new, "lihameister", "", replace = "lihatöötleja")
new$new = asendaja(new$new, "lihunik", "", replace = "lihatöötleja")
new$new = asendaja(new$new, "liha", "", excl1 = "tehnoloog", excl2 = "personal", excl3 = "peaspets", replace = "lihatöötleja")
new$new = asendaja(new$new, "kala", "töötl", replace = "lihatöötleja")
new$new = asendaja(new$new, "kala", "tootl", replace = "lihatöötleja")
new$new = asendaja(new$new, "kala", "meister", replace = "lihatöötleja")
new$new = asendaja(new$new, "kala", "filee", replace = "lihatöötleja")
new$new = asendaja(new$new, "kala", "toode", replace = "lihatöötleja")
new$new = asendaja(new$new, "kala", "lõik", replace = "lihatöötleja")
new$new = asendaja(new$new, "lihatöötleja", "", replace = "lihatöötleja")

# ISCO 3151 (ehk laevakaptenitega ühes suuremas grupis, mitte masinameh)
unique(leidur(new$new, "laeva", "mehaanik"))
new$new = asendaja(new$new, "laeva", "meh", replace = "laevamehaanik")

# kas on mõtet eristada kassapidajaid müüjatest? Koodid on küll erinevad, aga väga paljud on mõlemat (kas siis panna esimese järgi? Välistamine läheb keeruliseks...)
# praegu panin need, kelle nimes oli "müüja-kassapidaja", "teenindaja-kassapidaja" vms siiski kassapidajaks
unique(leidur(new$new, "kassapid", ""))
new$new = asendaja(new$new, "kassapidaja-", "", replace = "kassapidaja")
new$new = asendaja(new$new, "kassapid", "", excl1 = "kohtu", excl2 = "kontrolör", replace = "kassapidaja")
new$new = asendaja(new$new, "kassiir", "", "", replace = "kassapidaja")

unique(leidur(new$new, "hooldaja", "seadme"))
new$new = asendaja(new$new, "hooldaja", "seadme", replace = "elektroonikamehaanik")
new$new = asendaja(new$new, "hooldaja", "sõidu", replace = "elektroonikamehaanik")
new$new = asendaja(new$new, "hooldaja", "kohvi", replace = "elektroonikamehaanik")
new$new = asendaja(new$new, "hooldaja", "elektr", replace = "elektroonikamehaanik")
new$new = asendaja(new$new, "hooldaja", "arvut", replace = "elektroonikamehaanik")
new$new = asendaja(new$new, "hooldaja", "kudumis", replace = "elektroonikamehaanik") #??
new$new = asendaja(new$new, "hooldaja", "tehnika", replace = "elektroonikamehaanik")
new$new = asendaja(new$new, "meh", "õmblus", replace = "elektroonikamehaanik")

unique(leidur(new$new, "spordiväljak", "hoold"))
new$new = asendaja(new$new, "spordiväljakutehool", "", replace = "välikoristaja")
new$new = asendaja(new$new, "kojamees", "", replace = "välikoristaja")

unique(leidur(new$new, "hooldaja", ""))
new$new = asendaja(new$new, "hooldaja", "", excl1 = "looma", excl2 = "kokk", excl3 = "kliendite", excl4 = "raamatupi", replace = "hooldaja")

unique(leidur(new$new, "", "õpetaja"))
new$new = asendaja(new$new, "õpetaja-", "", replace = "õpetaja")
new$new = asendaja(new$new, "õpetaja", "üldhar", replace = "õpetaja")
new$new = asendaja(new$new, "õpetaja", "koolis", excl1 = "teenuste", excl2 = "jurist", replace = "õpetaja")
new$new = asendaja(new$new, "õpetaja", "vanem", excl1 = "koordi", excl2 = "asendus", replace = "õpetaja")
new$new = asendaja(new$new, "aineõpetaja", "", replace = "õpetaja")
new$new = asendaja(new$new, "õpetaja", "era", excl1 = "täisk", replace = "õpetaja")
new$new = asendaja(new$new, "õpetaja", "kodu", replace = "õpetaja")
new$new = asendaja(new$new, "õpetaja", "põhi", "gümn", replace = "õpetaja")
new$new = asendaja(new$new, "õpetaja", "põhi", "kesk", replace = "õpetaja")
new$new = asendaja(new$new, "´õpetaja", "", replace = "õpetaja")
new$new = asendaja(new$new, "kooliõpetaja", "", excl1 = "alg", excl2 = "põhi", excl3 = "huvi", excl4 = "muusika", replace = "õpetaja")
new$new = asendaja(new$new, "õpetaja", "ei", excl1 = "täisk", replace = "õpetaja")
new$new = asendaja(new$new, "õpetaja", "noor", replace = "õpetaja")
new$new = asendaja(new$new, "õoetaja", "", replace = "õpetaja")

new$new = asendaja(new$new, "õpetaja", "ajal", replace = "humanitaaraineteõpetaja")

# ilmselt mõistlik muude huviringide õpetajatega kokku panna? Tantsuõpetajad jne?
new$new = asendaja(new$new, "õpetaja", "teatr", replace = "draamaõpetaja")
new$new = asendaja(new$new, "õpetaja", "näiter", replace = "draamaõpetaja")
new$new = asendaja(new$new, "õpetaja", "lavalise", replace = "draamaõpetaja")

unique(leidur(new$new, "auto", "maaler"))
new$new = asendaja(new$new, "auto", "maaler", replace = "tootevärvija")
new$new = asendaja(new$new, "tootevärvija", "", replace = "tootevärvija")

unique(leidur(new$new, "dekoraator", ""))
new$new = asendaja(new$new, "dekoraator", "", excl1 = "juht", excl2 = "klien", replace = "sisekujundaja")

# turundusspetsialistid lähevad müügiesindajatega sama koodi alla, seega nimetan need ka müügiesindajateks
unique(leidur(new$new, "turundus", ""))
new$new = asendaja(new$new, "turundus", "assist", replace = "müügiesindaja")
new$new = asendaja(new$new, "turundus", "projekt", "juht", replace = "müügiesindaja")
new$new = asendaja(new$new, "turundus", "konsul", replace = "müügiesindaja")
new$new = asendaja(new$new, "turundus", "koordi", replace = "müügiesindaja")
new$new = asendaja(new$new, "turundus", "ekspe", replace = "müügiesindaja")
new$new = asendaja(new$new, "turundus", "ametn", replace = "müügiesindaja")

# kood 721 lihtsalt?
unique(leidur(new$new, "metallitööline", ""))
new$new = asendaja(new$new, "metalli", "töö", excl1 = "juht", excl2 = "tehnoloog", excl3 = "remont", replace = "metallitööline")

unique(leidur(new$new, "nõu", "pesi"))
new$new = asendaja(new$new, "nõu", "pesi", replace = "köögiabiline")
new$new = asendaja(new$new, "köögi", "abi", replace = "köögiabiline")
new$new = asendaja(new$new, "köögitööline", "", replace = "köögiabiline")

unique(leidur(new$new, "perevanem", ""))
new$new = asendaja(new$new, "perevanem", "", replace = "sotsiaaltööspetsialist")
new$new = asendaja(new$new, "eestkoste", "", replace = "sotsiaaltööspetsialist")

unique(leidur(new$new, "elektroonik", ""))
new$new = asendaja(new$new, "elektroonik", "raadio", replace = "elektrotehnik")
new$new = asendaja(new$new, "elektroonik", "spets", excl1 = "menetlus", excl2 = "poolimis", excl3 = "itja", replace = "elektrotehnik")
new$new = asendaja(new$new, "elektroonikatehnik", "", replace = "elektrotehnik")

unique(leidur(new$new, "kauba", "vastuv"))
new$new = asendaja(new$new, "kauba", "vastuv", excl1 = "juht", replace = "laoametnik")
new$new = asendaja(new$new, "kauba", "väljas", excl1 = "juht", replace = "laoametnik")
new$new = asendaja(new$new, "toodan", "väljas", replace = "laoametnik")
new$new = asendaja(new$new, "lao-operaator", "", replace = "laoametnik")
new$new = asendaja(new$new, "laokoordin", "", replace = "laoametnik")

unique(leidur(new$new, "küsitleja", ""))
new$new = asendaja(new$new, "küsitleja", "", replace = "küsitleja")

unique(leidur(new$new, "lennujuht", ""))
new$new = asendaja(new$new, "lennujuht", "", replace = "lennujuht")
new$new = asendaja(new$new, "lennu", "koordi", replace = "lennujuht")
new$new = asendaja(new$new, "lennu", "juht", excl1 = "grupi", replace = "lennujuht")

unique(leidur(new$new, "viimistleja", ""))
unique(leidur(new$new, "viimistleja", "mööbli"))
new$new = asendaja(new$new, "viimistleja", "mööbli", replace = "tisler")
new$new = asendaja(new$new, "tisler", "", replace = "tisler")
new$new = asendaja(new$new, "viimistleja", "restaur", replace = "tisler")
new$new = asendaja(new$new, "disler", "", replace = "tisler")

new$new = asendaja(new$new, "viimistleja", "sise", replace = "ehitusviimistleja")
new$new = asendaja(new$new, "viimistleja", "ehit", replace = "ehitusviimistleja")
new$new = asendaja(new$new, "viimistleja", "akend", replace = "ehitusviimistleja")
# lihtsalt "viimistlejaid" on ka 21 tk

unique(leidur(new$new, "kliend", "haldu", excl1 = "juh"))
new$new = asendaja(new$new, "kliend", "haldu", "spets", excl1 = "juh", replace = "klienditoe konsultant")
new$new = asendaja(new$new, "kliend", "haldu", excl1 = "juh", excl2 = "disain", excl3 = "teeninda", excl4 = "õpetaja", replace = "klienditoe konsultant")
new$new = asendaja(new$new, "kliendihaldurmüügi", "", replace = "klienditoe konsultant")
new$new = asendaja(new$new, "kliendikonsultant", "", excl1 = "äri", replace = "klienditoe konsultant")

unique(leidur(new$new, "ettevõ", "oma", "väike"))
new$new = asendaja(new$new, "ettevõ", "oma", "väike", excl1 = "õpetaja", replace = "väikeettevõtja")

unique(leidur(new$new, "firma", "oma", "väike"))
new$new = asendaja(new$new, "firma", "oma", "väike", replace = "väikeettevõtja")

# siin ka üks "ehitusettevõtja", kelle saaks põhimõtteliselt kodeerida, aga võib-olla mõistlik jätta kõik "self-empolyed" ikkagi eraldi
unique(leidur(new$new, "ettevõtja", "väike", ""))
new$new = asendaja(new$new, "ettevõtja", "väike", "", replace = "väikeettevõtja")

unique(leidur(new$new, "osaühing", "juh"))
new$new = asendaja(new$new, "osaühing", "juh", excl1 = "disainer", replace = "ettevõtja")

# siin on mõned ka täpsustanud valdkonda, aga vist ei hakka eristama?
unique(leidur(new$new, "ettevõ", "oman", ""))
new$new = asendaja(new$new, "ettevõ", "oman", excl1 = "osakonnajuha", replace = "ettevõtja")
new$new = asendaja(new$new, "ettevõ", "oma", "juh", excl1 = "osakonna", replace = "ettevõtja")
new$new = asendaja(new$new, "ettevõ", "isik", "juh", replace = "ettevõtja")
new$new = asendaja(new$new, "eraettevõtja", "", replace = "ettevõtja")
new$new = asendaja(new$new, "väikeettevõtja", "", replace = "ettevõtja")
new$new = asendaja(new$new, "ettevõtja-", "", replace = "ettevõtja")
new$new = asendaja(new$new, "ettevōtja", "", replace = "ettevõtja")

# siin tundub natuke kirjum seltskond, aga võib-olla ok (st ei leia nii spetsiifilisi koode ISCO-st)
unique(leidur(new$new, "haldusjuht", ""))
new$new = asendaja(new$new, "haldusjuht", "", excl1 = "müügi", excl2 = "riski", excl3 = "info", excl4 = "itja", replace = "haldusjuht")

# administrative services manager on haldusjuht - kas administratiivjuht ka?
unique(leidur(new$new, "administratiivjuht", ""))
new$new = asendaja(new$new, "administratiivjuht", "", replace = "haldusjuht")

unique(leidur(new$new, "sekretär", ""))
new$new = asendaja(new$new, "vallasekretär", "abi", replace = "juhiabi")
new$new = asendaja(new$new, "sekretär", "rektor", replace = "juhiabi")
new$new = asendaja(new$new, "sekretär", "insti", replace = "juhiabi")
new$new = asendaja(new$new, "sekretär", "kool", replace = "juhiabi")
new$new = asendaja(new$new, "sekretär", "akad", replace = "juhiabi")
new$new = asendaja(new$new, "sekretär", "õppeos", replace = "juhiabi")
new$new = asendaja(new$new, "asjaajamisespetsialist", "", replace = "juhiabi")
new$new = asendaja(new$new, "sekretär", "juh", replace = "juhiabi")
new$new = asendaja(new$new, "kantseleispets", "", replace = "juhiabi")

new$new = asendaja(new$new, "sekretär", "istung", replace = "õigusekeskastmespetsialist")

unique(leidur(new$new, "sekretär", ""))
new$new = asendaja(new$new, "sekretär", "", excl1 = "õppe", excl2 = "meditsiini", replace = "juhiabi")

unique(leidur(new$new, "juht", "kraana"))
new$new = asendaja(new$new, "juht", "kraana", excl1 = "ehitaja", replace = "kraanajuht")
new$new = asendaja(new$new, "", "kraana", excl1 = "ehitaja", replace = "kraanajuht")
                
unique(leidur(new$new, "lava", ""))
new$new = asendaja(new$new, "lavatehn", "", replace = "muu kultuurivaldkonna spetsialist")
new$new = asendaja(new$new, "lavameister", "", replace = "muu kultuurivaldkonna spetsialist")
new$new = asendaja(new$new, "valgustehnik", "", replace = "muu kultuurivaldkonna spetsialist")
new$new = asendaja(new$new, "rekvisiitor", "", excl1 = "juht", replace = "muu kultuurivaldkonna spetsialist")
new$new = asendaja(new$new, "teatritehnik", "", replace = "muu kultuurivaldkonna spetsialist")
new$new = asendaja(new$new, "tätovee", "", replace = "muu kultuurivaldkonna spetsialist")
new$new = asendaja(new$new, "kostümee", "", replace = "muu kultuurivaldkonna spetsialist")
new$new = asendaja(new$new, "digitaalsekinotehnik", "", replace = "muu kultuurivaldkonna spetsialist")
new$new = asendaja(new$new, "teleprogrammikoordinaator", "", replace = "muu kultuurivaldkonna spetsialist")
new$new = asendaja(new$new, "muukultuurivaldkonnaspetsialist", "", replace = "muu kultuurivaldkonna spetsialist")
new$new = asendaja(new$new, "trupijuht", "", replace = "muu kultuurivaldkonna spetsialist")

unique(leidur(new$new, "heakorra", ""))
new$new = asendaja(new$new, "heakorra", "spets", replace = "haljastusespetsialist")
new$new = asendaja(new$new, "haljastusspetsialist", "", replace = "haljastusespetsialist")
new$new = asendaja(new$new, "heakorra", "töö", replace = "haljastustööline")
new$new = asendaja(new$new, "haljastustööline", "", replace = "haljastustööline")
new$new = asendaja(new$new, "haljastaja", "", replace = "haljastustööline")

unique(leidur(new$new, "ehitaja", ""))
new$new = asendaja(new$new, "ehitajaja", "", replace = "ehitaja")
new$new = asendaja(new$new, "ehitaja-", "", replace = "ehitaja")
new$new = asendaja(new$new, "ehitusmeister", "", replace = "ehitaja")
new$new = asendaja(new$new, "üldehitus", "", replace = "ehitaja")
new$new = asendaja(new$new, "ehitusremont", "", replace = "ehitaja")
new$new = asendaja(new$new, "remonditööline", "", replace = "ehitaja")
new$new = asendaja(new$new, "remondimees", "", replace = "ehitaja")

unique(leidur(new$new, "autojuht", ""))
new$new = asendaja(new$new, "autojuht", "", excl1 = "ehitaja", excl2 = "laborite", excl3 = "kaugsõidu", excl4 = "bussijuhta",  replace = "autojuht")

unique(leidur(new$new, "klenditeenendaja", ""))
new$new = asendaja(new$new, "klenditeenendaja", "", replace = "klienditeenindaja")

unique(leidur(new$new, "klienditeenidaja", ""))
new$new = asendaja(new$new, "klienditeenidaja", "poe", replace = "müüja")
new$new = asendaja(new$new, "klienditeenidaja", "", replace = "klienditeenindaja")

unique(leidur(new$new, "vanemklienditeenindaja", ""))
new$new = asendaja(new$new, "vanemklienditeenindaja", "", replace = "klienditeenindaja")
new$new = asendaja(new$new, "vanem-klienditeenindaja", "", replace = "klienditeenindaja")
new$new = asendaja(new$new, "klienditeenindaja-", "", excl1 = "juht", replace = "klienditeenindaja")

unique(leidur(new$new, "kontori", "töötaja"))
new$new = asendaja(new$new, "kontori", "töötaja", excl1 = "raamatup", excl2 = "vanem", replace = "kontoritöötaja")
new$new = asendaja(new$new, "", "bürootöötaja", replace = "kontoritöötaja")

unique(leidur(new$new, "teenindaja", "müüja"))
new$new = asendaja(new$new, "teenindaja", "müüja", replace = "müüja")
new$new = asendaja(new$new, "teenindaja", "poe", excl1 = "internet", excl2 = "e-poe", replace = "müüja")

unique(leidur(new$new, "", "müüja-"))
new$new = asendaja(new$new, "müüja-", "", replace = "müüja")
new$new = asendaja(new$new, "^muuja$", "", replace = "müüja")

unique(leidur(new$new, "arvuti", "oper"))
new$new = asendaja(new$new, "arvuti", "oper", excl1 = "katlamaja", replace = "IKT-tehnik")
new$new = asendaja(new$new, "arvuti", "tehnik", replace = "IKT-tehnik")
new$new = asendaja(new$new, "arvut", "hooldus", replace = "IKT-tehnik")
new$new = asendaja(new$new, "ittugiisik", "", replace = "IKT-tehnik")
new$new = asendaja(new$new, "tugiisik", "tarkvara", replace = "IKT-tehnik")
new$new = asendaja(new$new, "itkasutajat", "", excl1 = "juht", excl2 = "tunnuste", replace = "IKT-tehnik")
new$new = asendaja(new$new, "it-kasutajat", "", replace = "IKT-tehnik")
new$new = asendaja(new$new, "infosüsteemikasutajatugi", "", replace = "IKT-tehnik")
new$new = asendaja(new$new, "arvutikasutajatugi", "", replace = "IKT-tehnik")
new$new = asendaja(new$new, "servicedesk-itvaldkonnakasutajatugi", "", replace = "IKT-tehnik")
new$new = asendaja(new$new, "veebi", "tehn", replace = "IKT-tehnik")
new$new = asendaja(new$new, "veebi", "haldur", excl1 = "product", excl2 = "toote", replace = "IKT-tehnik")
new$new = asendaja(new$new, "ithaldur", "haldur", replace = "IKT-tehnik")
new$new = asendaja(new$new, "it-klienditugi", "", replace = "IKT-tehnik")
new$new = asendaja(new$new, "itkklienditugi", "", replace = "IKT-tehnik")
new$new = asendaja(new$new, "klienditugiitvalkonnas", "", replace = "IKT-tehnik")
new$new = asendaja(new$new, "klienditugiveeb", "", replace = "IKT-tehnik")
new$new = asendaja(new$new, "sidevõrguhooldusinsener", "", replace = "IKT-tehnik")
new$new = asendaja(new$new, "itteenuseinsener", "", replace = "IKT-tehnik")
new$new = asendaja(new$new, "mobiilvõrgu", "", replace = "IKT-tehnik")
new$new = asendaja(new$new, "ittugispetsialist", "", replace = "IKT-tehnik")
new$new = asendaja(new$new, "ithooldus", "spets", replace = "IKT-tehnik")
new$new = asendaja(new$new, "ittoe", "spets", replace = "IKT-tehnik")
new$new = asendaja(new$new, "ikt-tehnik", "", replace = "IKT-tehnik")
new$new = asendaja(new$new, "itadministra", "", replace = "IKT-tehnik")
new$new = asendaja(new$new, "ittugi", "", replace = "IKT-tehnik")
new$new = asendaja(new$new, "ittehnik", "", replace = "IKT-tehnik")
new$new = asendaja(new$new, "it-tehnik", "", replace = "IKT-tehnik")
new$new = asendaja(new$new, "it-kasjutajatugi", "", replace = "IKT-tehnik")

unique(leidur(new$new, "müügikonsultant", ""))
new$new = asendaja(new$new, "müügikonsultant", "", excl1 = "stilist", excl2 = "disainer", replace = "müügiesindaja")

unique(leidur(new$new, "tehnik-tehnoloog", ""))
new$new = asendaja(new$new, "tehnik-tehnoloog", "", replace = "tehnik-tehnoloog")

unique(leidur(new$new, "kokk", ""))
new$new = asendaja(new$new, "kokk", "", excl1 = "peakokk", excl2 = "kokku", excl3 = "juht", excl4 = "omanik", replace = "kokk")

unique(leidur(new$new, "koristaja", ""))
new$new = asendaja(new$new, "koristajakl", "", replace = "puhastusteenindaja")
new$new = asendaja(new$new, "toateen", "", replace = "puhastusteenindaja")
new$new = asendaja(new$new, "hotelliteen", "", replace = "puhastusteenindaja")

unique(leidur(new$new, "kärup", ""))
new$new = asendaja(new$new, "kärup", "", replace = "muulihttööline")
new$new = asendaja(new$new, "käskjalg", "", replace = "muulihttööline")
new$new = asendaja(new$new, "piletör", "", replace = "muulihttööline")
new$new = asendaja(new$new, "riidehoidja", "", replace = "muulihttööline")

unique(leidur(new$new, "klienditeenindaja", "", excl1 = "juht"))
new$new = asendaja(new$new, "kliendite", "aptee", excl1 = "juht", replace = "müüja")
new$new = asendaja(new$new, "kliendite", "kaub", excl1 = "juht", replace = "müüja")
new$new = asendaja(new$new, "müüija", "", replace = "müüja")

new$new = asendaja(new$new, "kliendite", "tankla", replace = "kassapidaja")
new$new = asendaja(new$new, "kliendite", "statoil", replace = "kassapidaja")
new$new = asendaja(new$new, "kliendite", "kassa", replace = "kassapidaja")
new$new = asendaja(new$new, "teenindaja", "kassa", replace = "kassapidaja")
new$new = asendaja(new$new, "müüja", "tankla", replace = "kassapidaja")
new$new = asendaja(new$new, "kliendit", "pilet", replace = "kassapidaja")

new$new = asendaja(new$new, "kõnekeskus", "", excl1 = "juht", excl2 = "superv", excl3 = "abiline", excl4 = "admin", replace = "telefonimüüja")

unique(leidur(new$new, "projectmanager", ""))
new$new = asendaja(new$new, "projectmanager", "", excl1 = "comm", excl2 = "engineering", replace = "projektijuht")

unique(leidur(new$new, "klienditugi", ""))
new$new = asendaja(new$new, "klienditugi", "", replace = "klienditoe konsultant")

unique(leidur(new$new, "projektijuht-", ""))
new$new = asendaja(new$new, "projektijuht-", "", replace = "projektijuht")

unique(leidur(new$new, "raamatupida", ""))
unique(leidur(new$new, "raamatupida", "abi"))
new$new = asendaja(new$new, "raamatupida", "abi", replace = "raamatupidamisespetsialist")

unique(leidur(new$new, "revident", ""))
new$new = asendaja(new$new, "revident", "", replace = "pearaamatupidaja")
new$new = asendaja(new$new, "raamatupida", "vastutav", replace = "pearaamatupidaja")
new$new = asendaja(new$new, "raamatupida", "spetsialist", replace = "pearaamatupidaja")

unique(leidur(new$new, "raamatupidaja", "", excl1 = "juht"))
new$new = asendaja(new$new, "raamatupidaja", "", excl1 = "juht", replace = "raamatupidaja")
new$new = asendaja(new$new, "raamatupidaja", "", excl1 = "juht", excl2 = "pearaamatupidaja", replace = "raamatupidaja")
new$new = asendaja(new$new, "raamatupidamine", "", replace = "raamatupidaja")
new$new = asendaja(new$new, "raamatupidamist", "", excl1 = "ettevõtja", replace = "raamatupidaja")
new$new = asendaja(new$new, "raamatupidamiseteenuseosutajaettevõtetele", "", replace = "raamatupidaja")
new$new = asendaja(new$new, "raamatupidja", "", replace = "raamatupidaja")
new$new = asendaja(new$new, "raamtupidaja", "", replace = "raamatupidaja")

new$new = asendaja(new$new, "reklaamimüüja", "", replace = "äriteenusteagent")
new$new = asendaja(new$new, "promootor", "", replace = "äriteenusteagent")

unique(leidur(new$new, "müüja", "", excl1 = "juht"))
new$new = asendaja(new$new, "jaemüüja", "", replace = "müüja")
new$new = asendaja(new$new, "müüja", "", excl1 = "juht", excl2 = "telefonimüüja", replace = "müüja")

unique(leidur(new$new, "spetsialist", "labori"))
new$new = asendaja(new$new, "spetsialist", "labori", "haigla", replace = "bioanalüütik")
new$new = asendaja(new$new, "riiklikuimmunohematoloogiareferentlaborivanemlaborispetsialist", "", replace = "bioanalüütik")
new$new = asendaja(new$new, "verekabineti", "laborispet", replace = "bioanalüütik")
new$new = asendaja(new$new, "toksikol", "laborispet", replace = "bioanalüütik")

# lihtsalt "laborispetsialist" võib sobida mitme koodi alla, oleneb valdkonnast (keemia, meditsiin vist on peamised - aga töö sisu ilmselt pole ülemäära erinev?)
unique(leidur(new$new, "spetsialist", "labori"))
new$new = asendaja(new$new, "spetsialist", "labori", excl1 = "tarkvara", excl2 = "foto", replace = "laborispetsialist")

# laborant on veelgi ebamäärasem - ilmselt võib ükskõik mis valdkonnas olla laborant?
unique(leidur(new$new, "laborant", ""))
new$new = asendaja(new$new, "laborant", "velsker", replace = "bioanalüütik")
new$new = asendaja(new$new, "laborant", "immunoloogia", replace = "bioanalüütik")
new$new = asendaja(new$new, "laborant", "molekulaar", replace = "bioanalüütik")

new$new = asendaja(new$new, "laborant", "abi", replace = "laborant")
new$new = asendaja(new$new, "laborant", "vastu", replace = "laborant")
new$new = asendaja(new$new, "laborant", "valve", replace = "laborant")
new$new = asendaja(new$new, "laborant", "vanem", replace = "laborant")
new$new = asendaja(new$new, "laborant", "spets", replace = "laborant")
new$new = asendaja(new$new, "laborant", "asend", replace = "laborant")
new$new = asendaja(new$new, "laborant", "kliend", replace = "laborant")

unique(leidur(new$new, "juhatuse", "liige"))
new$new = asendaja(new$new, "juhatuse", "liige", excl1 = "juht", excl2 = "peaspet", excl3 = "osakonnaj", excl4 = "tegevdir", replace = "juhatuseliige")
new$new = asendaja(new$new, "juhatuseliigeettevõttejuht", "", replace = "juhatuseliige")

# Elektroonika- ja telekommunikatsiooniseadmete paigaldajad ja hooldajad (742 ehk 7421-7422) - ehk lühendatult elektroonikamehaanikud
# ei ole üldse kindel, kas siia said nüüd õiged - raske eristada ISCO-s mehaanikuid, tehnikuid jmt
unique(leidur(new$new, "hoold", "tehnik"))
new$new = asendaja(new$new, "tehnik", "ithoold", replace = "elektroonikamehaanik")
new$new = asendaja(new$new, "tehnik", "it-hoold", replace = "elektroonikamehaanik")
new$new = asendaja(new$new, "tehnik", "upsihool", replace = "elektroonikamehaanik")
new$new = asendaja(new$new, "tehnik", "hoold", excl1 = "kinnistu", excl2 = "juht", excl3 = "katlamaja", replace = "elektroonikamehaanik")
new$new = asendaja(new$new, "kino", "meh", replace = "elektroonikamehaanik")

unique(leidur(new$new, "insener", "hoold",))
new$new = asendaja(new$new, "insener", "hoold", "seadme", replace = "elektroonikamehaanik")
# jääb veel 27 hooldusinseneri, keda ei oska liigitada - võib olla IT-hooldus või mis iganes

unique(leidur(new$new, "mehaanik", "",))
new$new = asendaja(new$new, "meh", "jalgr", replace = "jalgrattamehaanik")
new$new = asendaja(new$new, "meh", "ratta", replace = "jalgrattamehaanik")

unique(leidur(new$new, "mehhaanik", ""))

# ISCO 7127
unique(leidur(new$new, "meh", "külm",))
new$new = asendaja(new$new, "meh", "külm", replace = "kliimaseadmetemehaanik")
new$new = asendaja(new$new, "meh", "kliima", replace = "kliimaseadmetemehaanik")

# ISCO 7232 (võib-olla teiste masinamehaanikutega kokku panna - automehaanik jne)
new$new = asendaja(new$new, "meh", "lennu", replace = "lennukimehaanik")
new$new = asendaja(new$new, "meh", "õhusõid", replace = "lennukimehaanik")

# neid ebamääraseid meh(h)aanikuid on veel, aga ISCO-s on nii palju erinevaid ametikohti mehaanikutel, sh erinevate üldkategooriate all jne 
unique(leidur(new$new, "mehaanik", "vanem"))
new$new = asendaja(new$new, "meh", "vanem", replace = "mehaanik")
new$new = asendaja(new$new, "meh", "pea", excl1 = "juhataja", excl2 = "tehnika", excl3 = "meister", replace = "mehaanik")

# kas kaupluse omanik "ettevõtjaks"? Või "kaupluse juhatajaks"?
unique(leidur(new$new, "kaup", "oman"))
new$new = asendaja(new$new, "kaup", "oman", replace = "kaupluse omanik")
new$new = asendaja(new$new, "poe", "oman", replace = "kaupluse omanik")

unique(leidur(new$new, "klienditeenindaja", "", excl1 = "juh"))
new$new = asendaja(new$new, "kliendi", "panga", replace = "pangateller")
new$new = asendaja(new$new, "kliendit", "puhke", replace = "hotelliadministraator")

unique(leidur(new$new, "kliendit", "haigl"))
new$new = asendaja(new$new, "kliendit", "haigl", replace = "haiglavastuvõtutöötaja")

new$new = asendaja(new$new, "kliendit", "optik", replace = "müüja")
new$new = asendaja(new$new, "kliendit", "müügi", excl1 = "juht", replace = "müüja")

new$new = asendaja(new$new, "klienditeenindaja", "", excl1 = "juht", excl2 = "insener", replace = "klienditeenindaja")
new$new = asendaja(new$new, "klienditeenindus", "", excl1 = "juht", replace = "klienditeenindaja")
new$new = asendaja(new$new, "klienditeendaja", "", excl1 = "juht", replace = "klienditeenindaja")
new$new = asendaja(new$new, "klienditeendus", "spets", excl1 = "juht", replace = "klienditeenindaja")
new$new = asendaja(new$new, "klienditeenindsja", "", excl1 = "juht", replace = "klienditeenindaja")
new$new = asendaja(new$new, "klienditeenindamine", "", excl1 = "juht", replace = "klienditeenindaja")
new$new = asendaja(new$new, "klienditeeninaja", "", excl1 = "juht", replace = "klienditeenindaja")
new$new = asendaja(new$new, "klienditeenendus", "", excl1 = "juht", replace = "klienditeenindaja")

new$new = asendaja(new$new, "klienditoe", "", excl1 = "juht", replace = "klienditoe konsultant")
new$new = asendaja(new$new, "kliendiinfo", "", excl1 = "juht", replace = "klienditoe konsultant")
new$new = asendaja(new$new, "teenustekasutajatoevanemspetsialist", "", replace = "klienditoe konsultant")

unique(leidur(new$new, "klien", "teeninda", excl1 = "juht"))
new$new = asendaja(new$new, "klien", "teeninda", excl1 = "juht", excl2 = "insener", replace = "klienditeenindaja")

unique(leidur(new$new, "osakonna", "juhataja"))
new$new = asendaja(new$new, "osakonna", "juhataja", "medits", replace = "tervishoiuteenustejuht")
new$new = asendaja(new$new, "juht", "medits", excl1 = "meesk", replace = "tervishoiuteenustejuht")
new$new = asendaja(new$new, "juht", "tervis", "keskas", replace = "tervishoiuteenustejuht")

new$new = asendaja(new$new, "osakonna", "juhataja", "tugit", replace = "sotsiaalhoolekandejuht")
new$new = asendaja(new$new, "osakonna", "juhataja", "sotsiaal", replace = "sotsiaalhoolekandejuht")
new$new = asendaja(new$new, "osakonna", "juhataja", "noorsoot", replace = "sotsiaalhoolekandejuht")

new$new = asendaja(new$new, "osakonna", "juh", "ehit", excl1 = "grupi", replace = "ehitusjuht")

new$new = asendaja(new$new, "osakonna", "juh", "panga", excl1 = "tiimi", replace = "pangajuht")
new$new = asendaja(new$new, "osakonna", "juh", "laen", replace = "pangajuht")
new$new = asendaja(new$new, "osakonna", "juh", "kindlus", replace = "pangajuht")

new$new = asendaja(new$new, "osakonna", "juh", "finant", replace = "finantsjuht")
new$new = asendaja(new$new, "osakonna", "juh", "rahan", replace = "finantsjuht")
new$new = asendaja(new$new, "osakonna", "juh", "eelarv", replace = "finantsjuht")
new$new = asendaja(new$new, "finatsjuht", "", replace = "finantsjuht")

new$new = asendaja(new$new, "osakonna", "juh", "korist", replace = "haldusjuht")
new$new = asendaja(new$new, "osakonna", "juh", "haldus", replace = "haldusjuht")
new$new = asendaja(new$new, "osakonna", "juh", "administratiiv", replace = "haldusjuht")
new$new = asendaja(new$new, "ärijuht", "", replace = "haldusjuht")
new$new = asendaja(new$new, "juh", "korist", replace = "haldusjuht")
new$new = asendaja(new$new, "üldosakonnaju", "", replace = "haldusjuht")

new$new = asendaja(new$new, "koolitus", "juh", "", excl1 = "projektijuht", replace = "töötajate koolitusspetsialist")

new$new = asendaja(new$new, "osakonna", "juh", "haridus", replace = "haridusasutustejuht")
new$new = asendaja(new$new, "autok", "juh", replace = "haridusasutustejuht")
new$new = asendaja(new$new, "koolijuh", "", replace = "haridusasutustejuht")
new$new = asendaja(new$new, "kooli", "juh", excl1 = "koolitaja", excl2 = "projektijuht", excl3 = "üliõpil", excl4 = "koordin", replace = "haridusasutustejuht")
new$new = asendaja(new$new, "osakonna", "juh", "teadus", excl1 = "grupijuht", replace = "haridusasutustejuht")
new$new = asendaja(new$new, "haridusasutustejuht", "", replace = "haridusasutustejuht")
new$new = asendaja(new$new, "õpikeskus", "juh", replace = "haridusasutustejuht")

new$new = asendaja(new$new, "osakonna", "juh", "raamatup", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "osakonna", "juh", "arhiiv", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "osakonna", "juh", "õigus", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "osakonna", "juh", "labor", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "osakonna", "juh", "muuseum", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "osakonna", "juh", "pääste", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "osakonna", "juh", "keskkon", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "osakonna", "juh", "jurii", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "osakonna", "juh", "sisekontr", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "juh", "sisekontr", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "juh", "raamatupidamis", excl1 = "grupi", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "juh", "geodeesia", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "juh", "muuseum", excl1 = "külastus", excl2 = "tegevus", excl3 = "arendusjuht", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "juh", "galerii", excl1 = "külastus", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "juh", "muusem", excl1 = "külastus", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "juh", "projekteer", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "juh", "arhiiv", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "juh", "rahvusvahel", "koostöö", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "juh", "keskkonna", excl1 = "projekt", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "juh", "kinnisvara", "ettev", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "", "õigusvaldkonnajuht", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "vangla", "juh", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "päästemeeskonnavanem", "", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "kutseteenustejuht", "", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "politseiülem", "", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "politsei", "juht", replace = "kutseteenustejuht")

new$new = asendaja(new$new, "osakonnajuh", "aset", excl1 = "tehnika", replace = "osakonnajuhataja")
new$new = asendaja(new$new, "allosakonna", "juh", replace = "osakonnajuhataja")
new$new = asendaja(new$new, "struktuuriüks", "juh", replace = "osakonnajuhataja")

new$new = asendaja(new$new, "osakonna", "juh", "müügi", excl1 = "tellimuste", replace = "müügijuht")
new$new = asendaja(new$new, "osakonna", "juh", "turundus", replace = "müügijuht")
new$new = asendaja(new$new, "müügijuhataja", "", excl1 = "trainee", replace = "müügijuht")

new$new = asendaja(new$new, "itosakonnajuhataja", "", replace = "IKT-juht")
new$new = asendaja(new$new, "osakonna", "juh", "tarkvara", replace = "IKT-juht")
new$new = asendaja(new$new, "osakonna", "juh", "infoteh", replace = "IKT-juht")
new$new = asendaja(new$new, "osakonna", "juh", "ikt", replace = "IKT-juht")
new$new = asendaja(new$new, "osakonna", "juh", "tehnoloogia", replace = "IKT-juht")
new$new = asendaja(new$new, "osakonna", "juh", "telekomm", replace = "IKT-juht")
new$new = asendaja(new$new, "osakonna", "juh", "itplaneer", replace = "IKT-juht")
new$new = asendaja(new$new, "infojuht", "", replace = "IKT-juht")
new$new = asendaja(new$new, "itvaldkonnajuht", "", replace = "IKT-juht")
new$new = asendaja(new$new, "itarendusprojektidejuht", "", replace = "IKT-juht")
new$new = asendaja(new$new, "infoturbejuht", "", replace = "IKT-juht")

new$new = asendaja(new$new, "osakonna", "juh", "tootmis", replace = "tootmisjuht")

new$new = asendaja(new$new, "osakonna", "juh", "tootearendus", replace = "tootearendusjuht")
new$new = asendaja(new$new, "tootearendus", "juht", excl1 = "müügi", replace = "tootearendusjuht")

new$new = asendaja(new$new, "arendusosakonna", "juh", excl1 = "tehnika", excl2 = "projekti", replace = "arendusjuht")
new$new = asendaja(new$new, "äriarendus", "juh", replace = "arendusjuht")
new$new = asendaja(new$new, "arengu-planeeringuosakonnamaa", "", replace = "arendusjuht")
new$new = asendaja(new$new, "osakonna", "juht", "arendus", excl1 = "projekti", excl2 = "alluvuseta", replace = "arendusjuht")
new$new = asendaja(new$new, "innovats", "juht", replace = "arendusjuht")
new$new = asendaja(new$new, "arendusjuht", "", excl1 = "tootearendus", excl2 = "ettevõttejuht", replace = "arendusjuht")
new$new = asendaja(new$new, "arengujuht", "", replace = "arendusjuht")
new$new = asendaja(new$new, "teadusjuht", "", replace = "arendusjuht")

new$new = asendaja(new$new, "osakonna", "juh", "energeet", replace = "tööstusjuht")
new$new = asendaja(new$new, "osakonna", "juh", "enegreetika", replace = "tööstusjuht")
new$new = asendaja(new$new, "energeetikajuht", "", replace = "tööstusjuht")
new$new = asendaja(new$new, "tehnikaosakonnajuhataja", "", replace = "tööstusjuht")
new$new = asendaja(new$new, "remondi-hooldusosakonnajuh", "", replace = "tööstusjuht")
new$new = asendaja(new$new, "koosteosakonnajuhataja", "", replace = "tööstusjuht")
new$new = asendaja(new$new, "osakonna", "juht", "tehn", replace = "tööstusjuht")
new$new = asendaja(new$new, "tööstusjuht", "", replace = "tööstusjuht")
new$new = asendaja(new$new, "tööstusettevõttejuht", "", replace = "tööstusjuht")

new$new = asendaja(new$new, "osakonna", "juh", "reklaam", replace = "reklaamijuht")
new$new = asendaja(new$new, "osakonna", "juh", "avalikesuhe", replace = "reklaamijuht")

# ja siis on hulk osakonnajuhatajaid, kellele ei leia ISCO-st eraldi nimetust ehk panen nad kõik lihtsalt osakonnajuhatajateks
new$new = asendaja(new$new, "osakonna", "juhataja", replace = "osakonnajuhataja")
new$new = asendaja(new$new, "osakonnajuhtaja", "", replace = "osakonnajuhataja")
new$new = asendaja(new$new, "osakonnajuhatja", "", replace = "osakonnajuhataja")
new$new = asendaja(new$new, "division", "juh", replace = "osakonnajuhataja")
new$new = asendaja(new$new, "divisjon", "juh", replace = "osakonnajuhataja")

unique(leidur(new$new, "osakonnajuht", ""))
new$new = asendaja(new$new, "osakonna", "juht", excl1 = "tiimi", excl2 = "projekti", excl3 = "ettevõttejuht", replace = "osakonnajuhataja")
new$new = asendaja(new$new, "allüksus", "juh", replace = "osakonnajuhataja")
new$new = asendaja(new$new, "oskaon", "juh", replace = "osakonnajuhataja")

# siin ka keeruline liigitada tegelikult - on müügijuhid, kes tegelevad nö turundus-müügiga ja siis on kaubanduse müügijuhid (ostujuhid?)
# mõned on ka ostu-müügijuhid? Samas seda ISCO-s ei eristata (vaid kaubanduse ostujuht... ?)
# praegu panin ka ostu-müügijuhid müügijuhtide alla
unique(leidur(new$new, "müügijuht", ""))
new$new = asendaja(new$new, "müügijuht", "", excl1 = "kliendihaldur", replace = "müügijuht")

unique(leidur(new$new, "müügi", "juht"))
new$new = asendaja(new$new, "müügi", "juht", excl1 = "meeskonna", excl2 = "projekti", excl3 = "hooldus", excl4 = "tiimi", replace = "müügijuht")
new$new = asendaja(new$new, "myygi", "juh", replace = "müügijuht")
new$new = asendaja(new$new, "myygi", "juh", replace = "müügijuht")

new$new = asendaja(new$new, "ehitusjuht", "", replace = "ehitusjuht")

unique(leidur(new$new, "projektijuht", ""))
unique(leidur(new$new, "projektijuht", "reklaam"))
new$new = asendaja(new$new, "projektijuht", "reklaam", replace = "reklaamispetsialist")
new$new = asendaja(new$new, "teabejuht", "", replace = "reklaamispetsialist")
new$new = asendaja(new$new, "käibe", "juht", "", replace = "reklaamispetsialist")
new$new = asendaja(new$new, "avalikesuhe", "juht", "", replace = "reklaamispetsialist")
new$new = asendaja(new$new, "kliendisuhete", "spetsialist", "", replace = "reklaamispetsialist")
new$new = asendaja(new$new, "sotsiaalmeedia", "spetsialist", "", replace = "reklaamispetsialist")
new$new = asendaja(new$new, "digiturund", "", "", replace = "reklaamispetsialist")
new$new = asendaja(new$new, "turundaja", "", "", replace = "reklaamispetsialist")

# projektijuhid tunduvad ikka nii ebamäärane grupp, raske neile mingi koodi leida (mõned projektijuhid on ka ISCO-s need kodeerisin juba varasemalt)
# kõik ülejäänud lihtsalt projektijuhtideks?
new$new = asendaja(new$new, "projektijuht", "", replace = "projektijuht")

table(leidur(new$new, "operaator", "", "")) %>% sort(decreasing = T)
new$new = asendaja(new$new, "operaat", "farm", replace = "loomakasvataja")
new$new = asendaja(new$new, "operaat", "sigala", replace = "loomakasvataja")
new$new = asendaja(new$new, "operaat", "lauda", replace = "loomakasvataja")

## peaks veel üle vaatama, kas siia lähevad kõik tehaseoperaatorid (ja võib-olla keegi masinaoperaatoritest veel?)
new$new = asendaja(new$new, "operaat", "jaam", excl1 = "taara", replace = "tööstuseprotsessijuhtimistehnik")
new$new = asendaja(new$new, "operaator", "tehas", replace = "tööstuseprotsessijuhtimistehnik")
new$new = asendaja(new$new, "keemiatööstusetehnik", "", replace = "tööstuseprotsessijuhtimistehnik")
new$new = asendaja(new$new, "roboti", "oper", replace = "tööstuseprotsessijuhtimistehnik")
new$new = asendaja(new$new, "vee", "oper", replace = "tööstuseprotsessijuhtimistehnik")
new$new = asendaja(new$new, "väävl", "oper", replace = "tööstuseprotsessijuhtimistehnik")
new$new = asendaja(new$new, "elektrijaamaoperaator", "", replace = "tööstuseprotsessijuhtimistehnik")
new$new = asendaja(new$new, "gaasioperaator", "", replace = "tööstuseprotsessijuhtimistehnik")
new$new = asendaja(new$new, "tööstuseprotsessijuhtimistehnik", "", replace = "tööstuseprotsessijuhtimistehnik")
new$new = asendaja(new$new, "elektrijaama", "operaator", replace = "tööstuseprotsessijuhtimistehnik")
new$new = asendaja(new$new, "mehhatroonik", "", replace = "tööstuseprotsessijuhtimistehnik")
new$new = asendaja(new$new, "mehatroonik", "", replace = "tööstuseprotsessijuhtimistehnik")

new$new = asendaja(new$new, "oper", "kaamer", replace = "audiovisuaaltehnik")
new$new = asendaja(new$new, "oper", "tv", replace = "audiovisuaaltehnik")
new$new = asendaja(new$new, "oper", "stereo", replace = "audiovisuaaltehnik")

new$new = asendaja(new$new, "oper", "telef", replace = "telefonioperaator")
new$new = asendaja(new$new, "päästekorraldaja", "", replace = "telefonioperaator")

new$new = asendaja(new$new, "operaator-", "", replace = "operaator")
new$new = asendaja(new$new, "operaator", "viimane", replace = "operaator")
new$new = asendaja(new$new, "operaator9", "", replace = "operaator")
# üksikuid operaatoreid on veel, aga enamik on nii ebamäärased, et üsna võimatu on kuhugi grupeerida

table(leidur(new$new, "koordinaator", "", "")) %>% sort(decreasing = T)
unique(leidur(new$new, "koordinaator", "rahvusv"))
new$new = asendaja(new$new, "koordinaator", "rahvusv", replace = "strateegiateväljatöötaja")
new$new = asendaja(new$new, "poliit", "nõu", replace = "strateegiateväljatöötaja")
new$new = asendaja(new$new, "julgeoleku", "noun", replace = "strateegiateväljatöötaja")
new$new = asendaja(new$new, "nimekorr", "", replace = "strateegiateväljatöötaja")
new$new = asendaja(new$new, "koostöö", "koordina", replace = "strateegiateväljatöötaja")
new$new = asendaja(new$new, "rahvusv", "nõun", replace = "strateegiateväljatöötaja")
new$new = asendaja(new$new, "kult", "nõu", replace = "strateegiateväljatöötaja")
new$new = asendaja(new$new, "välissuhete", "", replace = "strateegiateväljatöötaja")
new$new = asendaja(new$new, "rahvusvahelistesuhe", "juh", replace = "strateegiateväljatöötaja")
new$new = asendaja(new$new, "partnersuhe", "juh", replace = "strateegiateväljatöötaja")
new$new = asendaja(new$new, "riskijuht", "", replace = "strateegiateväljatöötaja")
new$new = asendaja(new$new, "innovatsioonispetsialist", "", replace = "strateegiateväljatöötaja")
new$new = asendaja(new$new, "rahvusvahelistesuhetespetsialist", "", replace = "strateegiateväljatöötaja")
new$new = asendaja(new$new, "strateegiateväljatöötaja", "", replace = "strateegiateväljatöötaja")
new$new = asendaja(new$new, "aml", "spetsialist", replace = "strateegiateväljatöötaja")
new$new = asendaja(new$new, "rahapesu", "spetsialist", replace = "strateegiateväljatöötaja")
new$new = asendaja(new$new, "rahapesu", "uurija", replace = "strateegiateväljatöötaja")
new$new = asendaja(new$new, "^aml$", "", replace = "strateegiateväljatöötaja")
new$new = asendaja(new$new, "minist", "nõu", replace = "strateegiateväljatöötaja")

new$new = asendaja(new$new, "bim", "koordina", replace = "joonestaja")
new$new = asendaja(new$new, "joonestaja", "", excl1 = "disainer", replace = "joonestaja")

new$new = asendaja(new$new, "üritus", "koordina", replace = "üritustekorraldaja")
new$new = asendaja(new$new, "üritus", "korr", excl1 = "sponsor", replace = "üritustekorraldaja")
new$new = asendaja(new$new, "konver", "korr", replace = "üritustekorraldaja")
new$new = asendaja(new$new, "pulm", "korr", replace = "üritustekorraldaja")
new$new = asendaja(new$new, "pulm", "korr", replace = "üritustekorraldaja")
new$new = asendaja(new$new, "sündmus", "korr", replace = "üritustekorraldaja")
new$new = asendaja(new$new, "konver", "koord", replace = "üritustekorraldaja")

new$new = asendaja(new$new, "transpor", "koordi", replace = "linnaplaneerija")
new$new = asendaja(new$new, "transpor", "koordi", replace = "linnaplaneerija")
new$new = asendaja(new$new, "teedespets", "", replace = "linnaplaneerija")

new$new = asendaja(new$new, "hotel", "manag", replace = "hotellijuht")
new$new = asendaja(new$new, "hotellijuh", "", replace = "hotellijuht")
new$new = asendaja(new$new, "hotellindus", "juht", replace = "hotellijuht")
new$new = asendaja(new$new, "võõrastemaja", "juh", replace = "hotellijuht")
new$new = asendaja(new$new, "hostel", "juh", replace = "hotellijuht")
new$new = asendaja(new$new, "majutusas", "juh", replace = "hotellijuht")
new$new = asendaja(new$new, "puhkemaja", "juh", replace = "hotellijuht")
new$new = asendaja(new$new, "majutusettev", "juh", replace = "hotellijuht")

table(leidur(new$new, "juhataja", "", "")) %>% sort(decreasing = T)
new$new = asendaja(new$new, "talitusejuhataja", "", replace = "osakonnajuhataja")
new$new = asendaja(new$new, "juhatajaasetä", "", excl1 = "nõunik", excl2 = "vahetusevanem", excl3 = "liiklus", excl4 = "resto", replace = "juhataja")
new$new = asendaja(new$new, "^juhataj$", "", replace = "juhataja")

new$new = asendaja(new$new, "filiaal", "juh", replace = "osakonnajuhataja")
new$new = asendaja(new$new, "jaosk", "juh", excl1 = "mäetöö", excl2 = "politsei", replace = "osakonnajuhataja")
new$new = asendaja(new$new, "oskonn", "juh", replace = "osakonnajuhataja")
new$new = asendaja(new$new, "osak", "juh", excl1 = "tiimi", excl2 = "grupi", excl3 = "ettevõtte", replace = "osakonnajuhataja")

new$new = asendaja(new$new, "juhatajaabi", "", replace = "juhiabi")
new$new = asendaja(new$new, "juhatuseabi", "", replace = "juhiabi")

unique(leidur(new$new, "juh", "kohvik"))
new$new = asendaja(new$new, "juh", "kohvik", replace = "restoranijuht")
new$new = asendaja(new$new, "juh", "resto", excl1 = "kõõgi", excl2 = "treener", replace = "restoranijuht")
new$new = asendaja(new$new, "juh", "toit", excl1 = "kutseõp", excl2 = "teenindusjuht", replace = "restoranijuht")
new$new = asendaja(new$new, "juh", "baar", excl1 = "hamba", replace = "restoranijuht")
new$new = asendaja(new$new, "pidaja", "baar", replace = "restoranijuht")

new$new = asendaja(new$new, "tehas", "juh", excl1 = "kontrol", replace = "tööstusjuht")
new$new = asendaja(new$new, "tsehh", "juh", replace = "tööstusjuht")
new$new = asendaja(new$new, "vabrik", "juh", replace = "tööstusjuht")
new$new = asendaja(new$new, "tootmis", "üksusejuh", replace = "tööstusjuht")
new$new = asendaja(new$new, "tootmis", "ettevõt", "juh", excl1 = "tehnika", replace = "tööstusjuht")
new$new = asendaja(new$new, "tootmisejuh", "", replace = "tööstusjuht")
new$new = asendaja(new$new, "tootmisekeskastmejuht", "", replace = "tööstusjuht")
new$new = asendaja(new$new, "tootmis-tehnikajuht", "", replace = "tööstusjuht")
new$new = asendaja(new$new, "paigaldusjuht", "", replace = "tööstusjuht")
new$new = asendaja(new$new, "tehnikajuht", "", replace = "tööstusjuht")

new$new = asendaja(new$new, "juh", "ehitusfirma",  replace = "ehitusjuht")
new$new = asendaja(new$new, "ehitustejuht", "",  replace = "ehitusjuht")
new$new = asendaja(new$new, "ehitusorg", "juh",  replace = "ehitusjuht")
new$new = asendaja(new$new, "ehitusüks", "juh",  replace = "ehitusjuht")

new$new = asendaja(new$new, "oüjuh", "",  replace = "ettevõttejuht")
new$new = asendaja(new$new, "aktsia", "juh", excl1 = "kinnisvara",  replace = "ettevõttejuht")
new$new = asendaja(new$new, "ettev", "juhtimine",  replace = "ettevõttejuht")
new$new = asendaja(new$new, "ettev]ttejuht", "",  replace = "ettevõttejuht")
new$new = asendaja(new$new, "ettevõttejuhataja", "",  replace = "ettevõttejuht")
new$new = asendaja(new$new, "asutusejuh", "", excl1 = "õpetaja", excl2 = "teadus", excl3 = "puhkeasut", replace = "ettevõttejuht")

new$new = asendaja(new$new, "majandusala", "juh",  replace = "majandusjuhataja")

new$new = asendaja(new$new, "juhatajakohusetäitja", "",  replace = "juhataja")
new$new = asendaja(new$new, "asejuhataja", "", excl1 = "asfald", replace = "juhataja")
new$new = asendaja(new$new, "juhatajs", "", replace = "juhataja")

new$new = asendaja(new$new, "töökoja", "juh", excl1 = "käsitöö", excl2 = "metall", excl3 = "puidu", excl4 = "põllumaj", replace = "teenustejuht")
new$new = asendaja(new$new, "salong", "juh", excl1 = "müügisal", excl2 = "mööblisal", replace = "teenustejuht")
new$new = asendaja(new$new, "reisiette", "juh", replace = "teenustejuht")
new$new = asendaja(new$new, "turismiette", "juh", replace = "teenustejuht")
new$new = asendaja(new$new, "turismiala", "juh", replace = "teenustejuht")
new$new = asendaja(new$new, "spaa", "juh", replace = "teenustejuht")
new$new = asendaja(new$new, "tehnoül", "juh", replace = "teenustejuht")
new$new = asendaja(new$new, "konverents", "juh", excl1 = "programm", replace = "teenustejuht")
new$new = asendaja(new$new, "teenusk", "juh", excl1 = "programm", replace = "teenustejuht")
new$new = asendaja(new$new, "kõnekes", "juh", replace = "teenustejuht")
new$new = asendaja(new$new, "teenustejuht", "", excl1 = "tervishoiu", excl2 = "kutse", replace = "teenustejuht")
new$new = asendaja(new$new, "teenindusettevõttejuht", "", replace = "teenustejuht")
new$new = asendaja(new$new, "kalmistu", "juh", replace = "teenustejuht")

unique(leidur(new$new, "teenindusjuh",""))
new$new = asendaja(new$new, "teenindusjuh", "", excl1 = "kliendi", excl2 = "toitlus", replace = "teenindusjuht")

table(leidur(new$new, "juhataja", "", "")) %>% sort(decreasing = T)
unique(leidur(new$new, "töödejuh", ""))
new$new = asendaja(new$new, "töödejuh", "puhastus", replace = "majapidaja")
new$new = asendaja(new$new, "töödejuh", "puhatus", replace = "majapidaja")
new$new = asendaja(new$new, "ühiselamu", "juh", replace = "majapidaja")
new$new = asendaja(new$new, "majutus", "juh", excl1 = "ettevõt", replace = "majapidaja")

new$new = asendaja(new$new, "tootmisetöödejuh", "", replace = "tootmistöödejuhataja")
new$new = asendaja(new$new, "tootmise-töödejuh", "", replace = "tootmistöödejuhataja")
new$new = asendaja(new$new, "töödejuh", "tehnik", replace = "tootmistöödejuhataja")
new$new = asendaja(new$new, "töödejuh", "mehaan", replace = "tootmistöödejuhataja")
new$new = asendaja(new$new, "tootmisobjektijuht", "", replace = "tootmistöödejuhataja")
new$new = asendaja(new$new, "tootmistöödejuh", "", replace = "tootmistöödejuhataja")

new$new = asendaja(new$new, "töödejuh", "elektri", replace = "ehitustöödejuhataja")
new$new = asendaja(new$new, "töödejuh", "el.", replace = "ehitustöödejuhataja")
new$new = asendaja(new$new, "teemeister", "", replace = "ehitustöödejuhataja")

new$new = asendaja(new$new, "töödejuh", "", excl1 = "tootmis", excl2 = "ehitus", replace = "töödejuhataja")
new$new = asendaja(new$new, "objektijuht", "", replace = "töödejuhataja")
new$new = asendaja(new$new, "tööjuht", "", excl1 = "kliinil", excl2 = "vastuv", excl3 = "konsul", replace = "töödejuhataja")

unique(leidur(new$new, "eraettevõtte", "juh"))
new$new = asendaja(new$new, "eraettevõtte", "juh", replace = "ettevõttejuht")

new$new = asendaja(new$new, "hooldekodujuh", "", replace = "eakatehooldusteenusejuht")
new$new = asendaja(new$new, "hooldekodudejuh", "", replace = "eakatehooldusteenusejuht")
new$new = asendaja(new$new, "vanuritekod", "juh", replace = "eakatehooldusteenusejuht")

new$new = asendaja(new$new, "noortekes", "juh", replace = "vabaajakeskustejuht")
new$new = asendaja(new$new, "rahvamaja", "juh", replace = "vabaajakeskustejuht")
new$new = asendaja(new$new, "kultuur", "juh", excl1 = "programm", excl2 = "valdkon", excl3 = "ameti", replace = "vabaajakeskustejuht")
new$new = asendaja(new$new, "teatrijuht", "", replace = "vabaajakeskustejuht")
new$new = asendaja(new$new, "spordik", "juh", excl1 = "meeskonna", replace = "vabaajakeskustejuht")
new$new = asendaja(new$new, "spordib", "juh", excl1 = "meeskonna", replace = "vabaajakeskustejuht")
new$new = asendaja(new$new, "spordialal", "juh", replace = "vabaajakeskustejuht")
new$new = asendaja(new$new, "kinojuh", "", replace = "vabaajakeskustejuht")
new$new = asendaja(new$new, "kasiinojuh", "", replace = "vabaajakeskustejuht")
new$new = asendaja(new$new, "joogast", "juh", replace = "vabaajakeskustejuht")
new$new = asendaja(new$new, "fitnessi", "juh", replace = "vabaajakeskustejuht")
new$new = asendaja(new$new, "keskastme", "juh", "kultuur", replace = "vabaajakeskustejuht")
new$new = asendaja(new$new, "spordijuht", "", excl1 = "sõjaväe", excl2 = "eksprodi", replace = "vabaajakeskustejuht")
new$new = asendaja(new$new, "kultuurikorraldaja", "", replace = "vabaajakeskustejuht")
new$new = asendaja(new$new, "turismitalu", "juh", replace = "vabaajakeskustejuht")
new$new = asendaja(new$new, "huvikeskuse", "juh", replace = "vabaajakeskustejuht")
new$new = asendaja(new$new, "puhkeasutus", "juh", replace = "vabaajakeskustejuht")

new$new = asendaja(new$new, "päevakes", "juh", replace = "sotsiaalhoolekandejuht")
new$new = asendaja(new$new, "lastekod", "juh", replace = "sotsiaalhoolekandejuht")
new$new = asendaja(new$new, "sotsiaalmaj", "juh", replace = "sotsiaalhoolekandejuht")
new$new = asendaja(new$new, "hoolekande", "juh", replace = "sotsiaalhoolekandejuht")
new$new = asendaja(new$new, "päevak", "juh", replace = "sotsiaalhoolekandejuht")
new$new = asendaja(new$new, "sotsiaalt", "juh", replace = "sotsiaalhoolekandejuht")
new$new = asendaja(new$new, "sotsiaalk", "juh", replace = "sotsiaalhoolekandejuht")
new$new = asendaja(new$new, "sotsiaalv", "juh", replace = "sotsiaalhoolekandejuht")
new$new = asendaja(new$new, "sotsiaala", "juh", replace = "sotsiaalhoolekandejuht")
new$new = asendaja(new$new, "tugiteen", "juh", replace = "sotsiaalhoolekandejuht") # ei pruugi olla sotsiaalhoolekanne, aga... ?
new$new = asendaja(new$new, "noorsoo", "juh", replace = "sotsiaalhoolekandejuht")
new$new = asendaja(new$new, "perek", "juh", replace = "sotsiaalhoolekandejuht")

unique(leidur(new$new, "enda", "ettevõt"))
new$new = asendaja(new$new, "enda", "ettevõt", replace = "ettevõtja")
new$new = asendaja(new$new, "ettevõtja", "", replace = "ettevõtja")
new$new = asendaja(new$new, "ettvõtt", "", replace = "ettevõtja")

table(leidur(new$new, "juht", "", "")) %>% sort(decreasing = T) %>% .[1:150]
unique(leidur(new$new, "keskastmejuht", ""))
new$new = asendaja(new$new, "keskastmejuht", "", replace = "juht")
new$new = asendaja(new$new, "regioonijuht", "", replace = "juht")
new$new = asendaja(new$new, "tippjuht", "", excl1 = "rätsep", replace = "juht")
new$new = asendaja(new$new, "piirkonnajuht", "", replace = "juht")
new$new = asendaja(new$new, "asejuht", "", excl1 = "üliõp", replace = "juht")
new$new = asendaja(new$new, "mtüjuh", "", replace = "juht")
new$new = asendaja(new$new, "talitusejuht", "", replace = "juht")

new$new = asendaja(new$new, "juhatusejuht", "", replace = "ettevõttejuht")
new$new = asendaja(new$new, "asutusejuht", "", excl1 = "õpetaja", excl2 = "kohaliku", excl3 = "kov", replace = "ettevõttejuht")
new$new = asendaja(new$new, "ettevôtte", "ju", replace = "ettevõttejuht")

new$new = asendaja(new$new, "digiturundusejuht", "", replace = "turundusjuht")
new$new = asendaja(new$new, "transpordijuht", "", replace = "tarneahelajuht")
new$new = asendaja(new$new, "transpordiettev", "juh", replace = "tarneahelajuht")

# ISCO-s pole ühtegi meeskonna-tiimijuhi koodi - osad on küll valdkonna ka täpsustanud, aga paljud on väga ebamäärased ehk paneks kõik lihtsalt kokku tiimijuhtideks?
# või on meeskonna/tiimijuht sama mis osakonnajuhataja? Sellisel juhul saaks valdkonna kaudu osad liigitada küll... ?
unique(leidur(new$new, "meeskonnajuht", ""))
new$new = asendaja(new$new, "meeskonnajuht", "", replace = "tiimijuht")
new$new = asendaja(new$new, "tiimijuht", "", replace = "tiimijuht")
new$new = asendaja(new$new, "tiim", "liider", replace = "tiimijuht")

new$new = asendaja(new$new, "vastavuskontroll", "", replace = "jurist")

new$new = asendaja(new$new, "juht", "klienditoe", replace = "klienditeenindusjuht")
new$new = asendaja(new$new, "juht", "klienditeen", replace = "klienditeenindusjuht")
new$new = asendaja(new$new, "kliendijuht", "", excl1 = "võtme", excl2 = "suur", replace = "klienditeenindusjuht")

unique(leidur(new$new, "juht", "kliendi"))
new$new = asendaja(new$new, "juht", "kliendi", excl1 = "klienditeen", excl2 = "lahendus", excl3 = "projekt", replace = "finantsnõustaja")

new$new = asendaja(new$new, "juh", "strateeg", excl1 = "nõust", replace = "strateegiajuht")
new$new = asendaja(new$new, "planeerim", "juh", excl1 = "too", excl2 = "grupi", replace = "strateegiajuht")
new$new = asendaja(new$new, "poliitika", "juh", replace = "strateegiajuht")

unique(leidur(new$new, "fin", "juh"))
new$new = asendaja(new$new, "juh", "fin", excl1 = "tartu", excl2 = "praktika", excl3 = "teenindus", excl4 = "ettevõttejuht", replace = "finantsjuht")
new$new = asendaja(new$new, "juh", "eelarve", replace = "finantsjuht")

new$new = asendaja(new$new, "tegevjuht", "", excl1 = "disainer", replace = "tegevjuht")
new$new = asendaja(new$new, "ceo", "", excl1 = "compliance", replace = "tegevjuht")
new$new = asendaja(new$new, "juhtaja", "", replace = "juhataja")

new$new = asendaja(new$new, "hooldesõiduki", "juh", replace = "teemasinajuht")
new$new = asendaja(new$new, "sõidukijuht", "", replace = "autojuht")

new$new = asendaja(new$new, "juh", "põllumaj", replace = "põllumetsandusjuht")
new$new = asendaja(new$new, "juh", "metsa", excl1 = "masina", replace = "põllumetsandusjuht")

new$new = asendaja(new$new, "müügi", "juhataja", replace = "kaubandusjuht")
new$new = asendaja(new$new, "tankla", "juh", replace = "kaubandusjuht")
new$new = asendaja(new$new, "circlek", "juh", replace = "kaubandusjuht")

unique(leidur(new$new, "ametiühingu", ""))
new$new = asendaja(new$new, "ametiühingu", "", replace = "ametiühinguesimees")

new$new = asendaja(new$new, "apteegi", "abitöö", replace = "muu tervishoiuhooldustöötaja")
new$new = asendaja(new$new, "õe", "abiline", replace = "muu tervishoiuhooldustöötaja")
new$new = asendaja(new$new, "desinfektor", "", replace = "muu tervishoiuhooldustöötaja")
new$new = asendaja(new$new, "labori", "abi", replace = "muu tervishoiuhooldustöötaja")
new$new = asendaja(new$new, "radioloo", "abi", replace = "muu tervishoiuhooldustöötaja")
new$new = asendaja(new$new, "sterilis", "", replace = "muu tervishoiuhooldustöötaja")
new$new = asendaja(new$new, "ravimiuuring", "abi", replace = "muu tervishoiuhooldustöötaja")

unique(leidur(new$new, "arsti", "abi"))
new$new = asendaja(new$new, "arsti", "abi", replace = "abiarst")

# ISCO-s on allolevad kõik ühe koodi all
unique(leidur(new$new, "arheoloog", ""))
new$new = asendaja(new$new, "arheoloog", "", excl1 = "digit", excl2 = "abi", replace = "arheoloog")
new$new = asendaja(new$new, "sotsioloog", "", replace = "sotsioloog")
new$new = asendaja(new$new, "antropoloog", "", replace = "antropoloog")
new$new = asendaja(new$new, "geograaf", "", replace = "geograaf")

unique(leidur(new$new, "registripidaja", ""))
new$new = asendaja(new$new, "registripidaja", "", replace = "registripidaja")
new$new = asendaja(new$new, "registri", "hal", replace = "registripidaja")
new$new = asendaja(new$new, "arhiivispets", "", replace = "registripidaja")
new$new = asendaja(new$new, "registri", "spets", replace = "registripidaja")

unique(leidur(new$new, "plekksepp", ""))
new$new = asendaja(new$new, "plekksepp", "", replace = "plekksepp")

unique(leidur(new$new, "tantsija", ""))
new$new = asendaja(new$new, "tantsija", "", replace = "tantsija")
new$new = asendaja(new$new, "koreograaf", "", replace = "tantsija")
new$new = asendaja(new$new, "ballet", "", replace = "tantsija")
new$new = asendaja(new$new, "repetiitor", "", replace = "tantsija")

unique(leidur(new$new, "betoneerija", ""))
new$new = asendaja(new$new, "betoneeri", "", replace = "betoneerija")
new$new = asendaja(new$new, "betoon", "töö", replace = "betoneerija")
new$new = asendaja(new$new, "armeer", "", replace = "betoneerija")
new$new = asendaja(new$new, "betoon", "valmis", replace = "betoneerija")

unique(leidur(new$new, "ehitustehnik", ""))
new$new = asendaja(new$new, "ehitustehnik", "", excl1 = "masina", excl2 = "võrgu", replace = "ehitustehnik")
new$new = asendaja(new$new, "ehitus", "järeleval", replace = "ehitustehnik")
new$new = asendaja(new$new, "tuleohu", "inspekt", replace = "ehitustehnik")

unique(leidur(new$new, "korstnapüh", ""))
new$new = asendaja(new$new, "korstnapüh", "", replace = "ehitustarindite puhastaja")
new$new = asendaja(new$new, "ehitus", "puhastaja", replace = "ehitustarindite puhastaja")

unique(leidur(new$new, "elektrisead", "koostaja"))
new$new = asendaja(new$new, "elektrisead", "koostaja", replace = "elektriseadmete koostaja")
new$new = asendaja(new$new, "elektr", "mont", replace = "elektriseadmete koostaja")

new$new = asendaja(new$new, "elektriliinide", "paigaldaja", replace = "elektriliinide paigaldaja")
new$new = asendaja(new$new, "kaablikoostaja", "", replace = "elektriliinide paigaldaja")

unique(leidur(new$new, "geoloog", ""))
new$new = asendaja(new$new, "geoloog", "", excl1 = "haldus", replace = "geoloog")
new$new = asendaja(new$new, "geofüüsik", "", replace = "geoloog")

unique(leidur(new$new, "hankejuht", ""))
new$new = asendaja(new$new, "hankejuht", "", replace = "hankejuht")

new$new = asendaja(new$new, "foto", "operaa", replace = "fotomasinaoperaator")

unique(leidur(new$new, "isoleerija", ""))
new$new = asendaja(new$new, "isoleerija", "", replace = "isoleerija")

unique(leidur(new$new, "IT", "turvaspetsialist"))
new$new = asendaja(new$new, "it", "turvaspetsialist", replace = "IT-turvaspetsialist")
new$new = asendaja(new$new, "küberturbe", "", replace = "IT-turvaspetsialist")
new$new = asendaja(new$new, "infoturbespetsialist", "", replace = "IT-turvaspetsialist")

new$new = asendaja(new$new, "juveelitöötleja", "", replace = "juveelitöötleja")
new$new = asendaja(new$new, "juveliir", "", replace = "juveelitöötleja")
new$new = asendaja(new$new, "kulla", "", replace = "juveelitöötleja")
new$new = asendaja(new$new, "väärisk", "", replace = "juveelitöötleja")

unique(leidur(new$new, "kalur", ""))
new$new = asendaja(new$new, "kalur", "", replace = "kalandustöötaja")
new$new = asendaja(new$new, "kalakasvataja", "", replace = "kalandustöötaja")

unique(leidur(new$new, "keskkon", "insp"))
new$new = asendaja(new$new, "keskkon", "insp", replace = "valitsuse haldusalade ametnik")
new$new = asendaja(new$new, "pääste", "spets", excl1 = "kolled", replace = "valitsuse haldusalade ametnik")
new$new = asendaja(new$new, "mere", "insp", replace = "valitsuse haldusalade ametnik")
new$new = asendaja(new$new, "vallavaraspetsialist", "", replace = "valitsuse haldusalade ametnik")
new$new = asendaja(new$new, "valitsusehaldusaladeametnik", "", replace = "valitsuse haldusalade ametnik")

unique(leidur(new$new, "kiropraktik", ""))
new$new = asendaja(new$new, "kiropraktik", "", replace = "kiropraktik")

unique(leidur(new$new, "kiviraidur", ""))
new$new = asendaja(new$new, "kiviraidur", "", replace = "kivitöötleja")
new$new = asendaja(new$new, "kivitöö", "", replace = "kivitöötleja")
new$new = asendaja(new$new, "kivi", "", excl1 = "paigald", replace = "kivitöötleja")

unique(leidur(new$new, "klaasija", ""))
new$new = asendaja(new$new, "klaasija", "", replace = "klaasija")

new$new = asendaja(new$new, "kohaliku", "tervis", replace = "kohaliku omavalitsuse tervisetöötaja")

unique(leidur(new$new, "kuduja", ""))
new$new = asendaja(new$new, "kuduja", "", replace = "tekstiilikäsitöömeister")
new$new = asendaja(new$new, "nahamei", "", replace = "tekstiilikäsitöömeister")
new$new = asendaja(new$new, "heegeldaja", "", replace = "tekstiilikäsitöömeister")
new$new = asendaja(new$new, "tekstiilitoodete", "", replace = "tekstiilikäsitöömeister")

unique(leidur(new$new, "linnapla", ""))
new$new = asendaja(new$new, "linnaplaneerija", "", replace = "linnaplaneerija")
new$new = asendaja(new$new, "liiklusplaneerija", "", replace = "linnaplaneerija")
new$new = asendaja(new$new, "transpordikoordinaator", "", replace = "linnaplaneerija")

unique(leidur(new$new, "maksuamet", ""))
new$new = asendaja(new$new, "maksuametnik", "", replace = "maksuametnik")
new$new = asendaja(new$new, "maksuspetsialist", "", replace = "maksuametnik")

unique(leidur(new$new, "kaub", "vahetusevanem"))
new$new = asendaja(new$new, "kaub", "vahetusevanem", replace = "müüjate vahetusevanem")
new$new = asendaja(new$new, "müüja", "vahetusevanem", replace = "müüjate vahetusevanem")
new$new = asendaja(new$new, "kaup", "vahetusevanem", replace = "müüjate vahetusevanem")

new$new = asendaja(new$new, "piimatöötleja", "", replace = "piimatöötleja")
new$new = asendaja(new$new, "piimatoodetevalmistaja", "", replace = "piimatöötleja")

unique(leidur(new$new, "pillimeister", ""))
new$new = asendaja(new$new, "pillimeister", "", replace = "pillimeister")

new$new = asendaja(new$new, "põllutööline", "", replace = "põllumajanduse lihttööline")
new$new = asendaja(new$new, "aiatööline", "", replace = "põllumajanduse lihttööline")

new$new = asendaja(new$new, "põllumajandustehnik", "", replace = "põllumajandustehnik")

new$new = asendaja(new$new, "seadusandja", "", replace = "seadusandja")
new$new = asendaja(new$new, "minister", "", replace = "seadusandja")
new$new = asendaja(new$new, "riigikoguliige", "", replace = "seadusandja")

unique(leidur(new$new, "rautaja", ""))
new$new = asendaja(new$new, "rautaja", "", replace = "sepp")

new$new = asendaja(new$new, "sotsiaalkindlus", "", replace = "sotsiaalkindlustusametnik")

new$new = asendaja(new$new, "stenografist", "", replace = "masinakirjutaja")
new$new = asendaja(new$new, "masinak", "", replace = "masinakirjutaja")

unique(leidur(new$new, "pakkija", ""))
new$new = asendaja(new$new, "tehase", "abitöö", replace = "tööstuselihttööline")
new$new = asendaja(new$new, "pakkija", "", replace = "tööstuselihttööline")
new$new = asendaja(new$new, "kleepija", "", replace = "tööstuselihttööline")
new$new = asendaja(new$new, "laotööline", "", replace = "tööstuselihttööline")
new$new = asendaja(new$new, "sorteerija", "", excl1 = "murdesõnastiku", replace = "tööstuselihttööline")
new$new = asendaja(new$new, "tööstuselihttööline", "", replace = "tööstuselihttööline")
new$new = asendaja(new$new, "mööblifirmaslihttööline", "", replace = "tööstuselihttööline")

new$new = asendaja(new$new, "telekommunikatsiooniinsener", "", replace = "telekommunikatsiooniinsener")
new$new = asendaja(new$new, "raadioinsener", "", replace = "telekommunikatsiooniinsener")
new$new = asendaja(new$new, "telekommunikatsioonitehnik", "", replace = "telekommunikatsioonitehnik")

new$new = asendaja(new$new, "toitumisspetsialist", "", replace = "toitumisspetsialist")
new$new = asendaja(new$new, "toitumisnõustaja", "", replace = "toitumisspetsialist")

new$new = asendaja(new$new, "toodete", "testija", replace = "toodetetestija")

new$new = asendaja(new$new, "tootmis", "tehnik", replace = "tootmistehnik")
new$new = asendaja(new$new, "robot", "tehnik", replace = "tootmistehnik")
new$new = asendaja(new$new, "taatleja", "", replace = "tootmistehnik")

new$new = asendaja(new$new, "transporditööline", "", replace = "transporditööline")

unique(leidur(new$new, "tööstus", "insener"))
new$new = asendaja(new$new, "tööstus", "insener", replace = "tööstusinsener")
new$new = asendaja(new$new, "tooteinsener", "", replace = "tööstusinsener")
new$new = asendaja(new$new, "tootmistehnoloog", "", replace = "tööstusinsener")

unique(leidur(new$new, "töötervish", "spets"))
new$new = asendaja(new$new, "töötervish", "spets", replace = "töötervishoiuspetsialist")

# coach ehk arengunõustaja?
new$new = asendaja(new$new, "arengunõustaja", "", replace = "muu nõustaja")
new$new = asendaja(new$new, "muunõustaja", "", replace = "muu nõustaja")
new$new = asendaja(new$new, "miljööterapeut", "", replace = "muu nõustaja")

new$new = asendaja(new$new, "astronoom", "", replace = "füüsik")
new$new = asendaja(new$new, "füüsik", "", excl1 = "teadur", excl2 = "töö", excl3 = "geo", replace = "füüsik")

new$new = asendaja(new$new, "litsents", "amet", replace = "litsentsiametnik")

new$new = asendaja(new$new, "täiendmeditsiinispetsialist", "", replace = "täiendmeditsiinispetsialist")
new$new = asendaja(new$new, "kaubaladuja", "", replace = "veondustööline")
new$new = asendaja(new$new, "veondustööline", "", replace = "veondustööline")
new$new = asendaja(new$new, "kaubapaigutaja", "", replace = "veondustööline")

new$new = asendaja(new$new, "liikumisterapeut", "", replace = "liikumisterapeut")

new$new = asendaja(new$new, "keskkonnaanalüütik", "", replace = "keskkonnatehnikaspetsialist")
new$new = asendaja(new$new, "keskkonnatehnikaspetsialist", "", replace = "keskkonnatehnikaspetsialist")

new$new = asendaja(new$new, "kaubamaakler", "", replace = "kaubamaakler")

new$new = asendaja(new$new, "laskur", "", replace = "sportlane")
new$new = asendaja(new$new, "spordiinstruktor", "", replace = "spordiinstruktor")

new$new = asendaja(new$new, "valuutamaakler", "", replace = "valuutamaakler")
new$new = asendaja(new$new, "autopesija", "", replace = "autopesija")
new$new = asendaja(new$new, "stilist", "", replace = "moedisainer")

new$new = asendaja(new$new, "tehnilinejuht", "", replace = "tehniline juht")
new$new = asendaja(new$new, "tehnot", "juh", replace = "tehniline juht")

unique(leidur(new$new, "ettevõttejuht", ""))
new$new = asendaja(new$new, "ettevõttejuht", "", excl1 = "väike", excl2 = "oskus", excl3 = "organiseer", replace = "ettevõttejuht")

unique(leidur(new$new, "teadur", ""))
new$new = asendaja(new$new, "teadur", "", excl1 = "andmeteadur", excl2 = "abi",  replace = "teadur")
new$new = asendaja(new$new, "teadlane", "",  replace = "teadur")

# 7541 (muud oskus- ja käsitöölised -- allveetöötajad)
unique(leidur(new$new, "tuuker", ""))
new$new = asendaja(new$new, "tuuker", "", replace = "allveetöötaja")

unique(leidur(new$new, "teamlead", ""))
new$new = asendaja(new$new, "teamlead", "", excl1 = "support", replace = "tiimijuht")

unique(leidur(new$new, "kujundaja", ""))
new$new = asendaja(new$new, "kujundaja", "", excl1 = "sise", excl2 = "poliitika", excl3 = "küljendaja", replace = "multimeediadisainer")
new$new = asendaja(new$new, "arvutigraafik", "", replace = "multimeediadisainer")


# panin kõik praktikandid kokku (neid on vähe nagunii)
unique(leidur(new$new, "praktikant", ""))
new$new = asendaja(new$new, "praktikant", "", replace = "praktikant")

unique(leidur(new$new, "arhitekt", "")) 
new$new = asendaja(new$new, "arhitekt", "", excl1 = "maastiku", excl2 = "teenuste", excl3 = "turundus", excl4 = "fie", replace = "ehitusarhitekt")

unique(leidur(new$new, "fie", ""))
new$new = asendaja(new$new, "fie", "", replace = "fie")

unique(leidur(new$new, "analüütik", ""))
new$new = asendaja(new$new, "vanemanalüütik", "", replace = "analüütik")
new$new = asendaja(new$new, "peaanalüütik", "", replace = "analüütik")

table(leidur(new$new, "spetsialist", "", "")) %>% sort(decreasing = T) %>% .[1:30]

unique(leidur(new$new, "vanemspetsialist", ""))
new$new = asendaja(new$new, "vanemspetsialist", "", excl1 = "teh", excl2 = "õhu", excl3 = "kliinil", replace = "peaspetsialist")
new$new = asendaja(new$new, "koolitusspetsialist", "", replace = "töötajate koolitusspetsialist")
new$new = asendaja(new$new, "tippspetsialist", "", excl1 = "pedagoogika", replace = "spetsialist")
new$new = asendaja(new$new, "kvalitee", "spets", replace = "kvaliteedispetsialist")
new$new = asendaja(new$new, "spatsialist", "", replace = "spetsialist")

# lihtsalt "maakler" - kinnisvaramaakler?
table(leidur(new$new, "maakler", ""))
new$new = asendaja(new$new, "maakler", "", excl1 = "amaakler", excl2 = "tolli", excl3 = "seenior", replace = "kinnisvarahaldur")

table(leidur(new$new, "insener", "")) %>% sort(decreasing = T)
new$new = asendaja(new$new, "tootmisinsener", "", replace = "tööstusinsener")
new$new = asendaja(new$new, "tehnoloog", "insener", replace = "tööstusinsener")
new$new = asendaja(new$new, "tehnika", "insener", replace = "elektrotehnikainsener")
new$new = asendaja(new$new, "infra", "insener", replace = "ehitusinsener")

table(leidur(new$new, "abi", "töö")) %>% sort(decreasing = T)
new$new = asendaja(new$new, "abi", "töö", excl1 = "staabi", excl2 = "töötuid", replace = "abitööline")
new$new = asendaja(new$new, "abiline", "", excl1 = "kodu", excl2 = "köögi", replace = "abitööline")
new$new = asendaja(new$new, "^abi$", "", replace = "abitööline")

new$new = asendaja(new$new, "ostu", "abi", replace = "müügiesindaja")
new$new = asendaja(new$new, "müügi", "abi", replace = "müügiesindaja")
new$new = asendaja(new$new, "müügitugi", "", replace = "müügiesindaja")
new$new = asendaja(new$new, "müügiedendaja", "", replace = "müügiesindaja")
new$new = asendaja(new$new, "müügiinimene", "", replace = "müügiesindaja")
new$new = asendaja(new$new, "turundusabi", "", replace = "müügiesindaja")
new$new = asendaja(new$new, "turundus", "", excl1 = "juht", excl2 = "arhitekt", replace = "müügiesindaja")

new$new = asendaja(new$new, "koolitaja", "", replace = "töötajate koolitusspetsialist")

new$new = asendaja(new$new, "vaneminspektor", "", replace = "inspektor")
new$new = asendaja(new$new, "peainspektor", "", replace = "inspektor")

new$new = asendaja(new$new, "juhiasetäitja", "", excl1 = "majandus", replace = "juht")

new$new = asendaja(new$new, "lihttööline", "", excl1 = "tööstuse", replace = "muulihttööline")

new$new = asendaja(new$new, "peakonsultant", "", replace = "konsultant")
new$new = asendaja(new$new, "vanemkonsultant", "", excl1 = "tehnil", replace = "konsultant")
new$new = asendaja(new$new, "vanemteenindaja", "", replace = "teenindaja")
new$new = asendaja(new$new, "teenindus", "", replace = "teenindaja")
new$new = asendaja(new$new, "letiteenindaja", "", excl1 = "info", replace = "teenindaja")

new$new = asendaja(new$new, "vanemadministraator", "", replace = "administraator")
new$new = asendaja(new$new, "peaadministraator", "", replace = "administraator")
new$new = asendaja(new$new, "^administraator$", "", replace = "üldadministraator")

new$new = asendaja(new$new, "vanemarendaja", "", replace = "arendaja")

new$new = asendaja(new$new, "automaatik", "", excl1 = "juht", replace = "elektromehaanik")

new$new = asendaja(new$new, "politsei", "", excl1 = "amet", excl2 = "uurija", replace = "politseinik")
new$new = asendaja(new$new, "korrakaitse", "", replace = "politseinik")

new$new = asendaja(new$new, "pottsepp", "", replace = "müürsepp")
new$new = asendaja(new$new, "müürsepp", "", replace = "müürsepp")

new$new = asendaja(new$new, "laboritehnik", "", replace = "keemiatööstustehnik")

table(leidur(new$new, "terapeut", ""))
new$new = asendaja(new$new, "liikumisterapeut", "", replace = "loovterapeut")
new$new = asendaja(new$new, "mänguterapeut", "", replace = "loovterapeut")
new$new = asendaja(new$new, "terapeut", "", excl1 = "loov", excl2 = "füsio", excl3 = "tegevus", replace = "muu nõustaja")

new$new = asendaja(new$new, "peatehnoloog", "", replace = "tehnoloog")

new$new = asendaja(new$new, "productowner", "", replace = "tooteomanik")

new$new = asendaja(new$new, "kontoriabi", "", replace = "kontoriabiline")
new$new = asendaja(new$new, "bürooabi", "", replace = "kontoriabiline")
new$new = asendaja(new$new, "kantselei", "", replace = "kontoriabiline")

new$new = asendaja(new$new, "mehhanisaator", "", replace = "mehhanisaator")
new$new = asendaja(new$new, "mehanisaator", "", replace = "mehhanisaator")

new$new = asendaja(new$new, "vanemmeister", "", replace = "meister")

new$new = asendaja(new$new, "disainer-", "", replace = "disainer")
new$new = asendaja(new$new, "peauurija", "", replace = "uurija")
new$new = asendaja(new$new, "vaneminsener", "", replace = "insener")
new$new = asendaja(new$new, "vanemuurija", "", replace = "uurija")

new$new = asendaja(new$new, "produtsent", "", replace = "filmi-teatriprodutsent")
new$new = asendaja(new$new, "projektiinsener", "", replace = "insener")
new$new = asendaja(new$new, "^cto$", "", replace = "tehnoloogiajuht")
new$new = asendaja(new$new, "keskastmespetsialist", "", excl1 = "õiguse", replace = "spetsialist")
new$new = asendaja(new$new, "peapiiriametnik", "", replace = "tolliinspektor")
new$new = asendaja(new$new, "peaametnik", "", replace = "ametnik")
new$new = asendaja(new$new, "projektidekoordinaator", "", replace = "projektikoordinator")
new$new = asendaja(new$new, "teenendaja", "", replace = "teenindaja")
new$new = asendaja(new$new, "^engineer$", "", replace = "insener")
new$new = asendaja(new$new, "ehitaja", "", excl1 = "laeva", excl2 = "aadi", excl3 = "maastiku", replace = "ehitaja")

#### Stop and replace some more values

uh = table(new$new) %>% .[. > 5] %>% names
kodeerimata = new %>% filter(!new %in% uh)
kodeeritud = new %>% filter(new %in% uh)

dic = unique(kodeeritud$new[!is.na(kodeerimata$new)])
check = kodeerimata$new %>% as.character
checked = check_spelling(strtrim(check,60), dictionary = dic,range = 2, assume.first.correct = F) 
checked = checked %>% filter(!duplicated(row))
kodeerimata$row = 1:nrow(kodeerimata)
kodeerimata = left_join(kodeerimata %>% select(1:4,10:12), checked[,1:5], by = "row") %>% select(-row)
kodeerimata$dist = stringdist::stringsim(kodeerimata$new, kodeerimata$suggestion)
kodeerimata %>% select(scode:dist) %>% arrange(desc(dist)) %>% filter(dist > .89) %>% tail(20)

i = !is.na(kodeerimata$dist) & (kodeerimata$dist > .89)
kodeerimata$new[i] = kodeerimata$suggestion[i]

tmp = new
new = rbind(kodeerimata, kodeeritud) %>% arrange(match(scode, tmp$scode))

### now you can use this to get some more ideas: which words are more coommon among those with unique jobs

uh = table(new$new) %>% .[. == 1] %>% names
kodeerimata = new %>% filter(new %in% uh)

strsplit(kodeerimata$workPositionName, "\\W") %>% unlist %>% 
  table %>% sort(decreasing = T) %>% .[. > 49]

table(leidur(kodeerimata$new, "klien","teeninda", excl1 = "juh")) 
table(leidur(kodeerimata$new, "juht","osakonna")) 
table(leidur(kodeerimata$new, "juht","müügi")) 
table(leidur(kodeerimata$new, "juht","projekt")) 
table(leidur(kodeerimata$new, "juht","klien")) 
table(leidur(kodeerimata$new, "arst","")) 
table(leidur(kodeerimata$new, "teadur","")) 
table(leidur(kodeerimata$new, "nõunik","")) 
table(leidur(kodeerimata$new, "","raamatupid")) 
table(leidur(kodeerimata$new, "","spetsialist")) 
table(leidur(kodeerimata$new, "","koordi")) 
table(leidur(kodeerimata$new, "","operaator")) 
table(leidur(kodeerimata$new, "","abi")) 
table(leidur(kodeerimata$new, "","insener")) 


table(new$new) %>% sort(decreasing = T)
library('vctrs')
vec_count(new$new)

# panin new-tunnuse ISCO-koodidega kokku
ISCO = readxl::read_xlsx("C:/Users/K/Google Drive/GV/new_ISCO.xlsx") %>% select(new, ISCO)
new_isco = left_join(new, ISCO, by = "new")
new_isco$newWithISCO = ifelse(!is.na(new_isco$ISCO), new_isco$ISCO, new_isco$new)

# vaatasin gruppide suuruseid ja korrigeerisin mõned nimetused/ISCO-koordid ülal koodis ja ISCO-tabelis
vec_count = vec_count(new_isco$newWithISCO)

# siin on nii new-tunnus kui ISCO-kood koos - selle põhjal panin veel mõned väga spetsiifilised ametinimetused ühe 4-koodilise nime alla
new_count = vec_count(new$new)
new_count = rename(new_count, new = key)
new_count = left_join(new_count, ISCO, by = "new")

# unspecified managers, CEO-s (ISCO: 1)
new$new = asendaja(new$new, "^ettevõttejuht$", "", replace = "juht")
new$new = asendaja(new$new, "^tegevjuht$", "", replace = "juht")
new$new = asendaja(new$new, "^juhataja$", "", replace = "juht")

# ISCO: 1324
new$new = asendaja(new$new, "^laojuhataja$", "", replace = "tarneahelajuht")

# ISCO: 1342
new$new = asendaja(new$new, "^õendusjuht$", "", replace = "tervishoiuteenustejuht")

# ISCO: 1349
new$new = asendaja(new$new, "^raamatukogujuht$", "", replace = "kutseteenustejuht")
new$new = asendaja(new$new, "^laborijuht$", "", replace = "kutseteenustejuht")

# ISCO: 1420
new$new = asendaja(new$new, "^kauplusejuhataja$", "", replace = "kaubandusjuht")

# ISCO: 2132
new$new = asendaja(new$new, "^metsandusspetsialist$", "", replace = "agronoom")

# ISCO: 2163
new$new = asendaja(new$new, "^moedisainer$", "", replace = "disainer")
new$new = asendaja(new$new, "^tootedisainer$", "", replace = "disainer")

# ISCO: 2269
new$new = asendaja(new$new, "^kiropraktik$", "", replace = "muu tervishoiu tippspetsialist")
new$new = asendaja(new$new, "^loovterapeut$", "", replace = "muu tervishoiu tippspetsialist")
new$new = asendaja(new$new, "^tegevusterapeut$", "", replace = "muu tervishoiu tippspetsialist")

# ISCO: 23 (unspecified teachers)
new$new = asendaja(new$new, "^muusikaõpetaja$", "", replace = "õpetaja")
new$new = asendaja(new$new, "^reaalaineteõpetaja$", "", replace = "õpetaja")
new$new = asendaja(new$new, "^kehalisekasvatuseõpetaja$", "", replace = "õpetaja")

# ISCO: 2310
new$new = asendaja(new$new, "^teadur$", "", replace = "õppejõud")
new$new = asendaja(new$new, "^professor$", "", replace = "õppejõud")

# ISCO: 2341
new$new = asendaja(new$new, "^algkooliõpetaja$", "", replace = "põhikooliõpetaja")
new$new = asendaja(new$new, "^käsitöö-kunstiõpetaja$", "", replace = "põhikooliõpetaja")

# huvikooliõpetaja (ISCO: 2353-2355)
new$new = asendaja(new$new, "^muusikakooliõpetaja$", "", replace = "huvikooliõpetaja")
new$new = asendaja(new$new, "^tantsuõpetaja$", "", replace = "huvikooliõpetaja")
new$new = asendaja(new$new, "^draamaõpetaja$", "", replace = "huvikooliõpetaja")

# ISCO: 2421
new$new = asendaja(new$new, "^kvaliteedijuht$", "", replace = "juhtimisanalüütik")

# ISCO: 2422
new$new = asendaja(new$new, "^haljastusespetsialist$", "", replace = "strateegiateväljatöötaja")

# ISCO: 2632
new$new = asendaja(new$new, "^arheoloog$", "", replace = "arheoloog-antropoloog-jmt")
new$new = asendaja(new$new, "^sotsioloog$", "", replace = "arheoloog-antropoloog-jmt")
new$new = asendaja(new$new, "^geograaf$", "", replace = "arheoloog-antropoloog-jmt")
new$new = asendaja(new$new, "^antropoloog$", "", replace = "arheoloog-antropoloog-jmt")

# ISCO: 2635
new$new = asendaja(new$new, "^muu nõustaja$", "", replace = "sotsiaaltöötaja")

# ISCO: 3112
new$new = asendaja(new$new, "^ehitusprojekteerija$", "", replace = "ehitustehnik")
new$new = asendaja(new$new, "^projekteerija$", "", replace = "ehitustehnik")

# ISCO: 3212 - selles natuke kahtlen
new$new = asendaja(new$new, "^laborispetsialist$", "", replace = "bioanalüütik")

# ISCO: 3355
new$new = asendaja(new$new, "^uurija$", "", replace = "politseiuurija")

# ISCO: 3412
new$new = asendaja(new$new, "^tegevusjuhendaja$", "", replace = "sotsiaaltööspetsialist")

# ISCO: 4321 - selles natuke kahtlen
new$new = asendaja(new$new, "^laotöötaja$", "", replace = "laoametnik")

# ISCO: 5151
new$new = asendaja(new$new, "^majandusjuhataja$", "", replace = "majapidaja")

# ISCO: 5321
new$new = asendaja(new$new, "^tugiisik$", "", replace = "hooldaja")

# ISCO: 7421
new$new = asendaja(new$new, "^elektroonik$", "", replace = "elektroonikamehaanik")

# ISCO: 8219
new$new = asendaja(new$new, "^koostaja$", "", replace = "muukoostaja")

# ISCO: 0
new$new = asendaja(new$new, "^ohvitser$", "", replace = "sõjaväelane")
