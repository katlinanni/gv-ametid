require(tidyverse)
require(fuzzyjoin)
require(qdap)
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

codes = new = read_tsv("AmetidKodeerimisele.tsv")
new$new = new$firstBeforeCommaSlash
new$new = gsub("olin", "", new$new, fixed="")
new$new = gsub("olen", "", new$new, fixed="")
new$new = gsub("praegu", "", new$new, fixed="")
new$new = gsub("klenditeenindaja", "klienditeenindaja", new$new, fixed=T)
#new$new = gsub("kliendideenindus", "klienditeenindaja", new$new, fixed=T)
#new$new = gsub("sotsiaaltöö", "sotsiaaltöötaja", new$new, fixed=T)
#new$new = gsub("põllumajandus", "põllumajandustöötaja", new$new, fixed=T)

dic = unique(new$new[new$AtLeastTwo == 1 & !is.na(new$new)])
check = new$new %>% as.character
checked = check_spelling(strtrim(check,30), dictionary = dic,range = 2, assume.first.correct = F) 
checked = checked %>% filter(!duplicated(row))
new$row = 1:nrow(new)
new = left_join(new, checked[,1:5], by = "row") %>% select(-row)
new$dist = stringdist::stringsim(new$new, new$suggestion.x)
new %>% filter(AtLeastTwo == 0) %>% select(scode, not.found.x:suggestion.x, dist) %>% arrange(desc(dist)) %>% filter(dist < .80) %>% slice(1:20)

## replace
i = !is.na(new$dist) & (new$dist > .80)
new$new[i] = new$suggestion.x[i]

## start fishing
unique(leidur(new$new, "tudeng", ""))
new$new = asendaja(new$new, "tudeng","", excl1 = "nõust", excl2="assist", excl3="spetsial", replace = "tudeng")

unique(leidur(new$new, "üliõpilane", ""))
new$new = asendaja(new$new, "üliõpilane","", excl1 = "ema", excl2="arst", excl3="õpetaja", replace = "tudeng")

unique(leidur(new$new, "med", "õde", excl1 = "hamba"))
new$new = asendaja(new$new, "med","õde", excl1 = "abi", replace = "meditsiiniõde")
new$new = asendaja(new$new, "vanemõde","", replace = "meditsiiniõde")
new$new = asendaja(new$new, "õde","kiirabi", replace = "meditsiiniõde")
new$new = asendaja(new$new, "õde","intensiiv", replace = "meditsiiniõde")
new$new = asendaja(new$new, "õde","psühh", replace = "meditsiiniõde")

unique(leidur(new$new, "pere", "õde"))
new$new = asendaja(new$new, "pere","õde", excl1 = "ämmaemaja", replace = "meditsiiniõde")

unique(leidur(new$new, "õend", "juht"))
new$new = asendaja(new$new, "õend","juht", excl1 = "pikka", replace = "õendusjuht")
new$new = asendaja(new$new, "ämmaemand","juht", replace = "õendusjuht")
new$new = asendaja(new$new, "ülemõde","", replace = "õendusjuht")

unique(leidur(new$new, "hamba", "õde"))
new$new = asendaja(new$new, "õde", "hamba", replace = "hambaraviõde")
new$new = asendaja(new$new, "õde", "suuhügieen", replace = "hambaraviõde")

unique(leidur(new$new, "abi", "õde"))
new$new = asendaja(new$new, "abi", "õde", excl1 = "ilu", replace = "hooldusõde")
new$new = asendaja(new$new, "hooldus", "õde", replace = "hooldusõde")

unique(leidur(new$new, "õde", "haigla"))
new$new = asendaja(new$new, "õde","haigla", excl1 = "hooldus", replace = "meditsiiniõde")
new$new = asendaja(new$new, "õde","kliin", replace = "meditsiiniõde")

unique(leidur(new$new, "ämma", ""))
new$new = asendaja(new$new, "ämma","", excl1 = "õdeä", excl2 = "füsio", excl3 = "õpin", excl4 = "abi", replace = "ämmaemand")
new$new = asendaja(new$new, "ämma","abi", replace = "abiämmaemand")

unique(leidur(new$new, "õde", ""))
new$new = asendaja(new$new, "õde","", excl1 = "hamba", excl2 = "vokaal", excl3 = "ilu", excl4 = "hooldus", replace = "meditsiiniõde")



unique(leidur(new$new, "arst", "õppe"))
new$new = asendaja(new$new, "arst", "õppe", replace = "eriarst")

unique(leidur(new$new, "arst", "pere"))
new$new = asendaja(new$new, "arst","pere", excl1 = "regis", excl2 = "juhataja", excl3 = "assist", excl4 = "hammba", replace = "perearst")

unique(leidur(new$new, "arst", "üld"))
new$new = asendaja(new$new, "arst", "üld", excl1 = "resident", replace = "perearst")

unique(leidur(new$new, "kirurg", "", excl1 = "assist"))
new$new = asendaja(new$new, "kirurg","", excl1 = "veterinaar", excl2 = "assist", excl3 = "juh", excl4 = "suu", replace = "eriarst")

unique(leidur(new$new, "arst", "laste", excl1 = "õde"))
new$new = asendaja(new$new, "arst","laste", excl1 = "õde", replace = "eriarst")

unique(leidur(new$new, "arst", "naiste", excl1 = "õde"))
new$new = asendaja(new$new, "arst","naiste", excl1 = "õde", replace = "eriarst")

unique(leidur(new$new, "arst", "silma", excl1 = "õde"))
new$new = asendaja(new$new, "arst","silma", excl1 = "õde", replace = "eriarst")

unique(leidur(new$new, "arst", "sise", excl1 = "õde"))
new$new = asendaja(new$new, "arst","sise", excl1 = "õde", replace = "eriarst")

unique(leidur(new$new, "psühhiaater", "", excl1 = "õde"))
new$new = asendaja(new$new, "psühhiaater","", excl1 = "õde", replace = "eriarst")

unique(leidur(new$new, "arst", "pea"))
new$new = asendaja(new$new, "arst","pea", excl1 = "looma", replace = "arst-juht")

unique(leidur(new$new, "arst", "ülem", excl1 = "õde", excl2 = "aset"))
new$new = asendaja(new$new, "arst","ülem", excl1 = "õde", excl2 = "aset", replace = "arst-juht")

unique(leidur(new$new, "arst", "resident"))
new$new = asendaja(new$new, "arst","resident", replace = "eriarst")

unique(leidur(new$new, "arst", "hamba"))
new$new = asendaja(new$new, "arst","hamba", excl1 = "abil", excl2="assist", replace = "hambaarst")

unique(leidur(new$new, "hamba", "assis"))
new$new = asendaja(new$new, "hamba","assis", replace = "hambaarstiabiline")
new$new = asendaja(new$new, "hamba","abi", replace = "hambaarstiabiline")

unique(leidur(new$new, "arst", "looma"))
new$new = asendaja(new$new, "arst","looma", excl1 = "abi", excl2="assist", replace = "loomaarst")

unique(leidur(new$new, "veteri", ""))
new$new = asendaja(new$new, "veteri","", excl1 = "abi", excl2="assist", excl3 = "õde", excl4 = "tehnik", replace = "loomaarst")

unique(leidur(new$new, "professor", ""))
new$new = asendaja(new$new, "professor","", excl1 = "insti", excl2 = "juhataja", excl3 = "abi", replace = "professor")

##kas teha kõik teaduriks või nt "rakubioloogia teadur" oleks "bioloog"? ISCO-s iseenesest ei ole "teaduri" ametit (kuigi paljud on lihtsalt "teaduriks" end siiski tituleerinud)
#unique(leidur(new$new, "teadur", ""))
#new$new = asendaja(new$new, "teadur","", replace = "teadur")

##liiga palju välistusi peab tegema?
#unique(leidur(new$new, "lektor", ""))
#new$new = asendaja(new$new, "lektor","", excl1 = "juhataja", excl2 = "terapeut", excl3 = "treener", excl4 = "finants" replace = "lektor")

unique(leidur(new$new, "õppejõud", "ülik"))
new$new = asendaja(new$new, "õppejõud", "ülik", excl1 = "juht", replace = "õppejõud")
unique(leidur(new$new, "õppejõud", ""))

unique(leidur(new$new, "ettevõ", "oma", "väike"))
new$new = asendaja(new$new, "ettevõ", "oma", "väike", excl1 = "õpetaja", replace = "väikeettevõtja")

unique(leidur(new$new, "firma", "oma", "väike"))
new$new = asendaja(new$new, "firma", "oma", "väike", replace = "väikeettevõtja")

#siin ka üks "ehitusettevõtja", kelle saaks põhimõtteliselt kodeerida, aga võib-olla mõistlik jätta kõik "self-empolyed" ikkagi eraldi
unique(leidur(new$new, "ettevõtja", "väike", ""))
new$new = asendaja(new$new, "ettevõtja", "väike", "", replace = "väikeettevõtja")

#see ettevõtjate asi tundub väga lai, mõtlen seda veel

#unique(leidur(new$new, "firma", "oma", ""))
#new$new = asendaja(new$new, "firma", "oma", "", replace = "ettevõtja")
#new$new = asendaja(new$new, "firma", "enda", "", replace = "ettevõtja")

#unique(leidur(new$new, "ettevõ", "oma", ""))
#new$new = asendaja(new$new, "ettevõ", "oma", "", replace = "ettevõtja")
#new$new = asendaja(new$new, "ettevõ", "isiklik", "", replace = "ettevõtja")

#seda peaks ka võib-olla veel natuke mõtlema, kusjuures asetäitjad ja kohusetäitjad pannakse samale reale põhitäitjaga 
#unique(leidur(new$new, "osakonna", "juhataja", excl1 = "ase"))
#new$new = asendaja(new$new, "osakonna", "juhataja", "", excl1 = "ase", excl2 = "abi", excl3 = "kohuset", replace = "osakonnajuhataja")

#liiga lai ju - siin ka neid, kelle esimene amet on siiski midagi muud
#unique(leidur(new$new, "kauplus", "juhataja"))
#new$new = asendaja(new$new, "kauplus", "juhataja", "", replace = "kauplusejuhataja")

unique(leidur(new$new, "poe", "juh"))
new$new = asendaja(new$new, "poe", "juh", excl1 = "müügijuht", excl2 = "teenindaja", replace = "kauplusejuhataja")

unique(leidur(new$new, "raamatupida", "pea", "abi"))
new$new = asendaja(new$new, "raamatupida", "pea", "abi", replace = "raamatupidajaassistent")
new$new = asendaja(new$new, "raamatupida", "assist", replace = "raamatupidajaassistent")

unique(leidur(new$new, "raamatupida", "pea"))
new$new = asendaja(new$new, "raamatupida", "pea", excl1 = "finatsjuht", excl2 = "peaspe", excl3 = "finantsjuht", excl4 = "büroojuht", replace = "pearaamatupidaja")

unique(leidur(new$new, "raamatupida", "vanem"))
new$new = asendaja(new$new, "raamatupida", "vanem",excl1 = "õpetaja", excl2 = "vahetuse", replace = "pearaamatupidaja")

#liiga lai?
#unique(leidur(new$new, "raamatupida", "", excl1 = "abi"))
#new$new = asendaja(new$new, "raamatupidaja", "",excl1 = "abi", excl2 = "assist", excl3 = "vanem", excl4 = "pea", replace = "raamatupidaja")

unique(leidur(new$new, "müüja", "kons"))
new$new = asendaja(new$new, "müüja", "konsu", "", excl1 = "puhastus", replace = "müüja")

unique(leidur(new$new, "müüja", "klienditeenind"))
new$new = asendaja(new$new, "müüja", "klienditeenind", "", replace = "müüja")
new$new = asendaja(new$new, "müüja", "vanem", "", excl1 = "laojuhataja", replace = "müüja")
new$new = asendaja(new$new, "müüja", "vastutav", "", replace = "müüja")

unique(leidur(new$new, "müüja", "kaup"))
new$new = asendaja(new$new, "müüja", "kaup", "", excl1="juhataja-", excl2 = "maaletooja", replace = "müüja")
new$new = asendaja(new$new, "müüja", "poe", "", replace = "müüja")

#liiga lai? Ja äkki ei ole mõtet eristada kassapidajaid ja müüjaid? Neil on küll eraldi koodid, aga mõned "müüja-kassapidajad" läksid juba müüjateks nagunii
#unique(leidur(new$new, "kassapid", ""))
#new$new = asendaja(new$new, "kassapid", "", "", replace = "kassapidaja")
#new$new = asendaja(new$new, "kassiir", "", "", replace = "kassapidaja")

unique(leidur(new$new, "klien", "teenin", "kaup", excl1 = "juht"))
new$new = asendaja(new$new, "klien", "teenin", "kaup", excl1="juht", excl2 = "omanik", excl3 = "juhataja", replace = "müüja")

unique(leidur(new$new, "klien", "teenin", "poe", excl1 = "juht"))
new$new = asendaja(new$new, "klien", "teenin", "poe", excl1="juht", excl2 = "haldur", replace = "müüja")

#unique(leidur(new$new, "koristaja", ""))
#new$new = asendaja(new$new, "koristaja", "", "", replace = "puhastusteenindaja")

unique(leidur(new$new, "kosmeetik", "massöör"))
new$new = asendaja(new$new, "kosmeetik", "massöör", replace = "iluteenindaja")


unique(leidur(new$new, "massöör", ""))
new$new = asendaja(new$new, "massöör", "", excl1 = "toitumisn", excl2 = "aroomi", excl3 = "logistik", excl4 = "klienditoe", replace = "massöör")

unique(leidur(new$new, "ilu", "teenin"))
new$new = asendaja(new$new, "ilu", "teenind", "", replace = "iluteenindaja")

new$new = asendaja(new$new, "klienditeenindsja", "", "", replace = "klienditeenindaja")

unique(leidur(new$new, "juuksur", ""))
new$new = asendaja(new$new, "juuksur", "", excl1 = "koerte", excl2 = "kutseõpetaja", excl3 = "vallav", replace = "juuksur")

unique(leidur(new$new, "õpetaja", "algkool"))
new$new = asendaja(new$new, "õpetaja", "algkool", replace = "algkooliõpetaja")
new$new = asendaja(new$new, "õpetaja", "algklass", replace = "algkooliõpetaja")

unique(leidur(new$new, "õpetaja", "põhikool"))
new$new = asendaja(new$new, "õpetaja", "põhikool", excl1 = "gümnaasium", replace = "põhikooliõpetaja")

unique(leidur(new$new, "õpetaja", "alushar"))
new$new = asendaja(new$new, "õpetaja", "alushar", excl1 = "abi", replace = "lasteaiaõpetaja")

####

unique(leidur(new$new, "õpetaja", "keskkool"))
new$new = asendaja(new$new, "õpetaja", "keskkool", excl1 = "agronoom", excl2 = "põhi", replace = "keskkooliõpetaja")

unique(leidur(new$new, "õpetaja", "gümn"))
new$new = asendaja(new$new, "õpetaja", "gümn", excl1 = "põhi", replace = "keskkooliõpetaja")

unique(leidur(new$new, "õpetaja", "keel"))
new$new = asendaja(new$new, "õpetaja", "keel", replace = "keeleõpetaja")

unique(leidur(new$new, "õpetaja", "muusika"))
new$new = asendaja(new$new, "õpetaja", "keel", replace = "muusika")

unique(leidur(new$new, "õpetaja", "matem"))
new$new = asendaja(new$new, "õpetaja", "matem", replace = "teadusaineteõpetaja")

unique(leidur(new$new, "õpetaja", "keemia"))
new$new = asendaja(new$new, "õpetaja", "keemia", replace = "teadusaineteõpetaja")

unique(leidur(new$new, "õpetaja", "füüsika"))
new$new = asendaja(new$new, "õpetaja", "füüsika", replace = "teadusaineteõpetaja")

unique(leidur(new$new, "õpetaja", "bioloogia"))
new$new = asendaja(new$new, "õpetaja", "bioloogia", replace = "teadusaineteõpetaja")

unique(leidur(new$new, "õpetaja", "loodus"))
new$new = asendaja(new$new, "õpetaja", "loodus", replace = "teadusaineteõpetaja")

unique(leidur(new$new, "õpetaja", "lasteai"))
new$new = asendaja(new$new, "õpetaja", "lasteai", replace = "lasteaiaõpetaja")

unique(leidur(new$new, "õpetaja", "koolieel"))
new$new = asendaja(new$new, "õpetaja", "koolieel", replace = "lasteaiaõpetaja")

unique(leidur(new$new, "õpetaja", "abi"))
new$new = asendaja(new$new, "õpetaja", "abi", replace = "abiõpetaja")

unique(leidur(new$new, "õpetaja", "assistent"))
new$new = asendaja(new$new, "õpetaja", "assistent", replace = "abiõpetaja")

unique(leidur(new$new, "õde", "õpetaja"))
new$new = asendaja(new$new, "õde", "õpetaja", replace = "õppejõud")


unique(leidur(new$new, "müügi", "juht"))
new$new = asendaja(new$new, "müügi", "juht", excl1 = "ostu", replace = "müügijuht")

unique(leidur(new$new, "ehitus", "tööline"))
new$new = asendaja(new$new, "ehitus", "tööline", excl1 = "insener", excl2 = "lao", replace = "ehitustööline")

unique(leidur(new$new, "ehitus", "tööde", "juh"))
new$new = asendaja(new$new, "ehitus", "obj", "juht", excl1="abi", replace = "ehitustöödejuht")

unique(leidur(new$new, "ehitus", "obje", "juh"))
new$new = asendaja(new$new, "ehitus", "obj", "juh", excl1 = "abi", replace = "ehitustöödejuht")

unique(leidur(new$new, "puu", "sepp"))
new$new = asendaja(new$new, "puu", "sepp", replace = "puusepp")

unique(leidur(new$new, "elektrik", ""))
new$new = asendaja(new$new, "elektrik", "", excl1 = "valmistja", replace = "puusepp")

unique(leidur(new$new, "tisler", ""))
new$new = asendaja(new$new, "tisler", "", replace = "puusepp")

unique(leidur(new$new, "politseinik", ""))
new$new = asendaja(new$new, "politseinik", "", replace = "politseinik")

unique(leidur(new$new, "laborant", ""))
new$new = asendaja(new$new, "laborant", "", replace = "laborant")

unique(leidur(new$new, "kosmeetik", ""))
new$new = asendaja(new$new, "kosmeetik", "", excl1 = "auto", replace = "kosmeetik")

unique(leidur(new$new, "jurist", ""))
new$new = asendaja(new$new, "jurist", "", replace = "jurist")

unique(leidur(new$new, "advokaat", ""))
new$new = asendaja(new$new, "advokaat", "", replace = "advokaat")

unique(leidur(new$new, "ajakirjanik", ""))
new$new = asendaja(new$new, "ajakirjanik", "", replace = "ajakirjanik")

unique(leidur(new$new, "programmeerija", ""))
new$new = asendaja(new$new, "programmeerija", "", replace = "programmeerija")

unique(leidur(new$new, "tarkvara", "inse"))
new$new = asendaja(new$new, "tarkvara", "insener", replace = "programmeerija")

unique(leidur(new$new, "tarkvara", "arhitekt"))
new$new = asendaja(new$new, "tarkvara", "arhitekt", replace = "programmeerija")

unique(leidur(new$new, "süsteem", "arhitekt"))
new$new = asendaja(new$new, "süsteem", "arhitekt", replace = "programmeerija")

unique(leidur(new$new, "liini", "töö"))
new$new = asendaja(new$new, "liini", "töö", excl1 = "kliin", replace = "liinitööline")

unique(leidur(new$new, "osaühing", "juh"))
new$new = asendaja(new$new, "osaühing", "juh", replace = "ettevõtja")

new$new = asendaja(new$new, "veok", "juht", replace = "autojuht")
new$new = asendaja(new$new, "veo", "juht", "auto", replace = "autojuht")
new$new = asendaja(new$new, "autojuht", "", excl1 = "õpet", replace = "autojuht")
new$new = asendaja(new$new, "bussijuht", "", excl1 = "õpet", replace = "autojuht")
new$new = asendaja(new$new, "taksojuht", "", replace = "autojuht")


unique(leidur(new$new, "ettekandja", ""))
new$new = asendaja(new$new, "ettekandja", "", replace = "ettekandja")
new$new = asendaja(new$new, "baaridaam", "", replace = "ettekandja")
new$new = asendaja(new$new, "kelner", "", replace = "ettekandja")
new$new = asendaja(new$new, "baarm", "", replace = "ettekandja")
new$new = asendaja(new$new, "barman", "", replace = "ettekandja")
new$new = asendaja(new$new, "kohvik", "teenin", replace = "ettekandja")
new$new = asendaja(new$new, "restoran", "teenin", replace = "ettekandja")

unique(leidur(new$new, "tõlkija", ""))
unique(leidur(new$new, "keele", "toimetaja"))
new$new = asendaja(new$new, "tõlkija", "", replace = "tõlkija-keeletoimetaja")
new$new = asendaja(new$new, "keele", "toimetaja", replace = "tõlkija-keeletoimetaja")

unique(leidur(new$new, "sotsiaaltöötaja", ""))
new$new = asendaja(new$new, "sotsiaaltöötaja", "", replace = "sotsiaaltöötaja")
new$new = asendaja(new$new, "sotsiaaltöötaja", "spetsialist", replace = "sotsiaaltöötaja")

unique(leidur(new$new, "müügisekretär", ""))
new$new = asendaja(new$new, "müügisekretär", "", replace = "müügispetsialist")

unique(leidur(new$new, "müügispetsialist", ""))
new$new = asendaja(new$new, "müügispetsialist", "", replace = "müügispetsialist")

unique(leidur(new$new, "riigiametnik", ""))
new$new = asendaja(new$new, "riigiametnik", "", replace = "riigiametnik")
new$new = asendaja(new$new, "riigiteenistuja", "", replace = "riigiametnik")

unique(leidur(new$new, "eripedagoog", ""))
new$new = asendaja(new$new, "eripedagoog", "", replace = "eripedagoog")
new$new = asendaja(new$new, "logopeed", "", replace = "eripedagoog")

unique(leidur(new$new, "füsiotera", ""))
new$new = asendaja(new$new, "füsiotera", "", excl1 = "treener", excl2 = "õpin", replace = "füsioterapeut")

unique(leidur(new$new, "massöö", ""))
new$new = asendaja(new$new, "massöö", "", excl1 = "toit", excl2 = "log", replace = "massöör")
new$new = asendaja(new$new, "massaažiterapeut", "", excl1 = "toit", excl2 = "log", replace = "massöör")

unique(leidur(new$new, "põllutöö", ""))
new$new = asendaja(new$new, "põllutöö", "", replace = "põllumajandustöötaja")
new$new = asendaja(new$new, "põllumees", "", replace = "põllumajandustöötaja")
new$new = asendaja(new$new, "talu", "töö", excl1 = "riist", replace = "põllumajandustöötaja")
new$new = asendaja(new$new, "traktorist", "", replace = "põllumajandustöötaja")
new$new = asendaja(new$new, "põllumajandustöötaja", "", replace = "põllumajandustöötaja")
new$new = asendaja(new$new, "talunik", "", excl1 = "abikaasa", replace = "põllumajandustöötaja")
new$new = asendaja(new$new, "talupida", "", excl1 = "turismi", replace = "põllumajandustöötaja")
new$new = asendaja(new$new, "farmi", "", excl1 = "juh", replace = "põllumajandustöötaja")


unique(leidur(new$new, "turundus", "spetsial"))
new$new = asendaja(new$new, "turundus", "spetsial", replace = "turundusspetsialist")

unique(leidur(new$new, "müügi", "esindaja"))
new$new = asendaja(new$new, "müügi", "esindaja", replace = "müügiesindaja")
new$new = asendaja(new$new, "müügi", "mees", replace = "müügiesindaja")

unique(leidur(new$new, "hald", "kliend"))
new$new = asendaja(new$new, "hald", "kliend", replace = "kliendihaldur")

unique(leidur(new$new, "sekretär", ""))
new$new = asendaja(new$new, "sekretär", "", replace = "sekretär")

unique(leidur(new$new, "analüütik", "finants"))
new$new = asendaja(new$new, "analüütik", "finants", replace = "ärianalüütik")
new$new = asendaja(new$new, "analüütik", "krediidi", replace = "ärianalüütik")
new$new = asendaja(new$new, "analüütik", "majandus", replace = "ärianalüütik")
new$new = asendaja(new$new, "analüütik", "müügi", replace = "ärianalüütik")
new$new = asendaja(new$new, "analüütik", "riski", replace = "ärianalüütik")
new$new = asendaja(new$new, "analüütik", "tootmis", replace = "ärianalüütik")
new$new = asendaja(new$new, "analüütik", "toote", replace = "ärianalüütik")
new$new = asendaja(new$new, "analüütik", "eelarve", replace = "ärianalüütik")

unique(leidur(new$new, "analüütik", "süsteem"))
new$new = asendaja(new$new, "analüütik", "süsteem", replace = "itanalüütik")
new$new = asendaja(new$new, "analüütik", "tarkvara", replace = "itanalüütik")
new$new = asendaja(new$new, "analüütik", "it-", replace = "itanalüütik")
new$new = asendaja(new$new, "analüütik", "andmebaas", replace = "itanalüütik")

unique(leidur(new$new, "analüütik", "bio"))
new$new = asendaja(new$new, "analüütik", "bio", replace = "laborianalüütik")
new$new = asendaja(new$new, "analüütik", "keemi", replace = "laborianalüütik")

unique(leidur(new$new, "sõjaväelane", ""))
new$new = asendaja(new$new, "sõjaväelane", "", replace = "sõjaväelane")
new$new = asendaja(new$new, "sõdur", "", replace = "sõjaväelane")
new$new = asendaja(new$new, "kaitseväelane", "", replace = "sõjaväelane")

unique(leidur(new$new, "ohvitser", ""))
new$new = asendaja(new$new, "ohvitser", "vanem", replace = "sõjaväelane")
new$new = asendaja(new$new, "ohvitser", "all", replace = "sõjaväelane")
new$new = asendaja(new$new, "ohvitser", "staabi", replace = "sõjaväelane")
new$new = asendaja(new$new, "ohvitser", "väe", replace = "sõjaväelane")
new$new = asendaja(new$new, "kaitsevägi", "", replace = "sõjaväelane")

unique(leidur(new$new, "", "kapten"))
new$new = asendaja(new$new, "kapten", "", excl1 = "laborat", excl2 = "lennu", replace = "kapten-laevajuht")
new$new = asendaja(new$new, "laevajuht", "",replace = "kapten-laevajuht")

unique(leidur(new$new, "direktor", "kooli"))
new$new = asendaja(new$new, "direktor", "kooli",replace = "lasteaia-koolidirektor")
new$new = asendaja(new$new, "direktor", "lasteaia",replace = "lasteaia-koolidirektor")
new$new = asendaja(new$new, "koolijuht", "",replace = "lasteaia-koolidirektor")
new$new = asendaja(new$new, "koolijuhataja", "",replace = "lasteaia-koolidirektor")

new$new = asendaja(new$new, "õppeala", "juhataja",replace = "õppejuht")
new$new = asendaja(new$new, "õppejuht", "",replace = "õppejuht")
new$new = asendaja(new$new, "õppetööjuht", "",replace = "õppejuht")

table(leidur(new$new, "hooldaja", "")) %>% sort
new$new = asendaja(new$new, "hooldaja", "haigla",replace = "hooldaja")
new$new = asendaja(new$new, "hooldaja", "sotsiaal",replace = "hooldaja")
new$new = asendaja(new$new, "hooldaja", "omaste",replace = "hooldaja")
new$new = asendaja(new$new, "hooldaja", "hooldekodu",replace = "hooldaja")

unique(leidur(new$new, "kuller", ""))
new$new = asendaja(new$new, "kuller", "", excl1 = "toidu",replace = "postlijon-kuller")
new$new = asendaja(new$new, "kirjakandja", "",replace = "postlijon-kuller")
new$new = asendaja(new$new, "postlijon", "",replace = "postlijon-kuller")

unique(leidur(new$new, "ehitus", "tööline"))
new$new = asendaja(new$new, "ehitus", "tööline",replace = "ehitaja")
new$new = asendaja(new$new, "ehitusvaldkond", "",replace = "ehitaja")
new$new = asendaja(new$new, "üldehitaja", "",replace = "ehitaja")
new$new = asendaja(new$new, "ehitusviimistleja", "",replace = "ehitaja")
new$new = asendaja(new$new, "siseviimistleja", "",replace = "ehitaja")
new$new = asendaja(new$new, "siseviimistlus", "",replace = "ehitaja")
new$new = asendaja(new$new, "maaler", "", excl1 = "auto", replace = "ehitaja")

table(leidur(new$new, "tegevjuht", "")) %>% sort
new$new = asendaja(new$new, "tegevjuht", "firma",replace = "tegevjuht")
new$new = asendaja(new$new, "tegevjuht", "ettev",replace = "tegevjuht")

new$new = asendaja(new$new, "personalijuht", "",replace = "personalijuht")

unique(leidur(new$new, "tootmis", "juh",))
new$new = asendaja(new$new, "tootmis", "juht", excl1 = "abi", replace = "tootmisjuht")
new$new = asendaja(new$new, "tootmis", "juhat", excl1 = "abi", replace = "tootmisjuht")

unique(leidur(new$new, "psühho", "",))
new$new = asendaja(new$new, "psühho", "",replace = "psühholoog")

unique(leidur(new$new, "finantsjuh", "",))
new$new = asendaja(new$new, "finantsjuh", "", excl1 = "abi", replace = "finantsjuht")

unique(leidur(new$new, "turundusjuh", "",))
new$new = asendaja(new$new, "turundusjuh", "", excl1 = "abi", replace = "turundusjuht")

new$new = asendaja(new$new, "maja", "haldur", replace = "kinnisvarahaldur")
new$new = asendaja(new$new, "kinnisvara", "haldur", replace = "kinnisvarahaldur")
new$new = asendaja(new$new, "kinnisvara", "hoold", replace = "kinnisvarahaldur")
new$new = asendaja(new$new, "kinnisvara", "hool", replace = "kinnisvarahaldur")

new$new = asendaja(new$new, "kinnisvara", "maakler", replace = "maakler")

new$new = asendaja(new$new, "piloot", "", replace = "piloot")
new$new = asendaja(new$new, "lennuki", "kapten", replace = "piloot")

unique(leidur(new$new, "stjua", "",))
new$new = asendaja(new$new, "stju", "", replace = "stjuuard(ess)")

unique(leidur(new$new, "mehaanik", "",))
new$new = asendaja(new$new, "mehaanik", "", replace = "mehaanik")

new$new = asendaja(new$new, "iluteen", "", replace = "iluteenindaja")
new$new = asendaja(new$new, "küüneteh", "", replace = "iluteenindaja")
new$new = asendaja(new$new, "ripsme", "", replace = "iluteenindaja")
new$new = asendaja(new$new, "maniküü", "", replace = "iluteenindaja")
new$new = asendaja(new$new, "pediküü", "", replace = "iluteenindaja")

unique(leidur(new$new, "apteek", "",))
new$new = asendaja(new$new, "apteeker", "", replace = "apteeker")
new$new = asendaja(new$new, "proviisor", "", replace = "apteeker")

unique(leidur(new$new, "tehnik", "hoold",))
new$new = asendaja(new$new, "tehnik", "hoold", replace = "hooldustehnik")

unique(leidur(new$new, "insener", "hoold",))
new$new = asendaja(new$new, "insener", "hoold", replace = "hooldustehnik")

unique(leidur(new$new, "kondiiter", "",))
new$new = asendaja(new$new, "kondiiter", "", excl1 = "kokk", replace = "pagar-kondiiter")
new$new = asendaja(new$new, "pagar", "", excl1 = "kokk", replace = "pagar-kondiiter")

unique(leidur(new$new, "kokk", "",))
new$new = asendaja(new$new, "kokk", "pea", replace = "vanemkokk")
new$new = asendaja(new$new, "kokk", "vanem", replace = "vanemkokk")

new$new = asendaja(new$new, "kokk", "abi", replace = "abikokk")
new$new = asendaja(new$new, "koka", "abi", replace = "abikokk")

new$new = asendaja(new$new, "kokk", "pagar", replace = "kokk")
new$new = asendaja(new$new, "kokk", "kondiiter", replace = "kokk")
new$new = asendaja(new$new, "kokk", "teenin", replace = "kokk")
new$new = asendaja(new$new, "kokk", "restoran", replace = "kokk")

unique(leidur(new$new, "insener", "ehit",))
new$new = asendaja(new$new, "insener", "ehitus", replace = "ehitusinsener")

unique(leidur(new$new, "insener", "elek",))
new$new = asendaja(new$new, "insener", "elek", replace = "elektri-elektroonikainsener")

unique(leidur(new$new, "insener",""))
new$new = asendaja(new$new, "insener", "vanem", replace = "insener")
new$new = asendaja(new$new, "insener", "pea", replace = "insener")

unique(leidur(new$new, "fotograaf",""))
new$new = asendaja(new$new, "fotograaf", "", replace = "fotograaf")

unique(leidur(new$new, "näitleja",""))
new$new = asendaja(new$new, "näitleja", "", excl1 = "vabakutse", replace = "laulja-näitleja")

unique(leidur(new$new, "laulja",""))
new$new = asendaja(new$new, "laulja", "", replace = "laulja-näitleja")

unique(leidur(new$new, "raamatukogu","hoidja"))
new$new = asendaja(new$new, "raamatukogu", "hoidja", replace = "raamatukogutöötaja")
new$new = asendaja(new$new, "raamatukogu", "töötaja", replace = "raamatukogutöötaja")
new$new = asendaja(new$new, "raamatukogu", "teenind", replace = "raamatukogutöötaja")

unique(leidur(new$new, "audiitor",""))
new$new = asendaja(new$new, "audiitor", "", replace = "audiitor")

unique(leidur(new$new, "lao"))
new$new = asendaja(new$new, "laotöötaja", "", replace = "laotöötaja")
new$new = asendaja(new$new, "laomees", "", replace = "laotöötaja")
new$new = asendaja(new$new, "laohoidja", "", replace = "laotöötaja")
new$new = asendaja(new$new, "laotööline", "", replace = "laotöötaja")
new$new = asendaja(new$new, "laopidaja", "", replace = "laotöötaja")
new$new = asendaja(new$new, "laooperaator", "", replace = "laotöötaja")
new$new = asendaja(new$new, "laospetsialist", "", replace = "laotöötaja")
new$new = asendaja(new$new, "laoarvestusespetsialist", "", replace = "laotöötaja")

new$new = asendaja(new$new, "laojuhataja", "", replace = "laojuhataja")
new$new = asendaja(new$new, "laojuht", "", replace = "laojuhataja")
new$new = asendaja(new$new, "laohaldur", "", replace = "laojuhataja")
new$new = asendaja(new$new, "laovanem", "", replace = "laojuhataja")

unique(leidur(new$new, "ettevõtte", "juht", "väike"))
new$new = asendaja(new$new, "ettevõtte", "juht", "väike", replace = "väikeettevõttejuht")
new$new = asendaja(new$new, "firma", "juht", "väike", replace = "väikeettevõttejuht")

unique(leidur(new$new, "ettevõttejuh", "", excl1 = "väike", excl2 = "mikro"))
new$new = asendaja(new$new, "ettevõttejuh","", excl1 = "väike", excl2 = "mikro", replace = "ettevõttejuht")

unique(leidur(new$new, "ettevõtte", "juh", excl1 = "väike", excl2 = "mikro"))
new$new = asendaja(new$new, "ettevõtte", "juht", "tipp", replace = "ettevõttejuht")

unique(leidur(new$new, "piirivalvur", ""))
new$new = asendaja(new$new, "piirivalvur", "", replace = "piirivalvur")

unique(leidur(new$new, "metsa", "töö"))
new$new = asendaja(new$new, "metsandusspetsialist", "", replace = "metsandusspetsialist")
new$new = asendaja(new$new, "metsandusespetsialist", "", replace = "metsandusspetsialist")
new$new = asendaja(new$new, "metsaspetsialist", "", replace = "metsandusspetsialist")
new$new = asendaja(new$new, "metsakorraldaja", "", replace = "metsandusspetsialist")
new$new = asendaja(new$new, "metsaülem", "", replace = "metsandusspetsialist")
new$new = asendaja(new$new, "metsameister", "", replace = "metsandusspetsialist")

new$new = asendaja(new$new, "metsatööline", "", replace = "metsatööline")
new$new = asendaja(new$new, "metsamees", "", replace = "metsatööline")
new$new = asendaja(new$new, "metsamasinaoperaator", "", replace = "metsatööline")
new$new = asendaja(new$new, "harvest", "", replace = "metsatööline")

