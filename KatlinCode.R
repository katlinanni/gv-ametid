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

unique(leidur(new$new, "raamatupida", "pea", "abi"))
new$new = asendaja(new$new, "raamatupida", "pea", "abi", replace = "raamatupidajaassistent")
new$new = asendaja(new$new, "raamatupida", "assist", replace = "raamatupidajaassistent")

unique(leidur(new$new, "raamatupida", "pea"))
new$new = asendaja(new$new, "raamatupida", "pea", excl1 = "finatsjuht", excl2 = "peaspe", excl3 = "finantsjuht", excl4 = "büroojuht", replace = "pearaamatupidaja")
new$new = asendaja(new$new, "pearaamatupidaja-", "", replace = "pearaamatupidaja")
new$new = asendaja(new$new, "raamatu", "pea", excl1 = "juht", replace = "pearaamatupidaja")

unique(leidur(new$new, "raamatupida", "vanem"))
new$new = asendaja(new$new, "raamatupida", "vanem",excl1 = "õpetaja", excl2 = "vahetuse", replace = "pearaamatupidaja")

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

unique(leidur(new$new, "massöör", ""))
new$new = asendaja(new$new, "massöör", "", excl1 = "toitumisn", excl2 = "aroomi", excl3 = "logistik", excl4 = "klienditoe", replace = "massöör")

unique(leidur(new$new, "massöö", ""))
new$new = asendaja(new$new, "massöö", "", excl1 = "toit", excl2 = "log",  excl3 = "klienditoe", replace = "massöör")
new$new = asendaja(new$new, "massaažiterapeut", "", excl1 = "meister", excl2 = "salongi", excl3 = "jooga", excl4 = "õppejõud", replace = "massöör")

unique(leidur(new$new, "ilu", "teenin"))
new$new = asendaja(new$new, "ilu", "teenind", "", replace = "iluteenindaja")

unique(leidur(new$new, "kosmeetik", "", excl1 = "kosmeetika"))
new$new = asendaja(new$new, "kosmeetik", "", excl1 = "kosmeetika", excl2 = "auto", replace = "iluteenindaja")

new$new = asendaja(new$new, "iluteen", "", replace = "iluteenindaja")
new$new = asendaja(new$new, "küüneteh", "", excl1 = "õmblemine", replace = "iluteenindaja")
new$new = asendaja(new$new, "ripsme", "", replace = "iluteenindaja")
new$new = asendaja(new$new, "maniküü", "", replace = "iluteenindaja")
new$new = asendaja(new$new, "pediküü", "", replace = "iluteenindaja")
new$new = asendaja(new$new, "spa", "teenind", replace = "iluteenindaja")

unique(leidur(new$new, "jumest", ""))
new$new = asendaja(new$new, "jumest", "", replace = "iluteenindaja")
new$new = asendaja(new$new, "grim", "", replace = "iluteenindaja")

new$new = asendaja(new$new, "klienditeenindsja", "", "", replace = "klienditeenindaja")

unique(leidur(new$new, "juuksur", ""))
new$new = asendaja(new$new, "juuksur", "", excl1 = "koerte", excl2 = "kutseõpetaja", excl3 = "vallav", replace = "juuksur")
new$new = asendaja(new$new, "juukse", "", replace = "juuksur")
new$new = asendaja(new$new, "barber", "", replace = "juuksur")

unique(leidur(new$new, "õpetaja", "algkool"))
new$new = asendaja(new$new, "õpetaja", "algkool", replace = "algõpetaja")
new$new = asendaja(new$new, "õpetaja", "algklass", replace = "algkooliõpetaja")

unique(leidur(new$new, "õpetaja", "põhikool"))
new$new = asendaja(new$new, "õpetaja", "põhikool", excl1 = "gümnaasium", replace = "põhikooliõpetaja")

unique(leidur(new$new, "kooli", "direktor"))
new$new = asendaja(new$new, "kooli", "direktor", excl1 = "õpetaja;", replace = "haridusasutustejuhid")

unique(leidur(new$new, "õpetaja", "alushar"))
new$new = asendaja(new$new, "õpetaja", "alushar", excl1 = "abi", replace = "lasteaiaõpetaja")

unique(leidur(new$new, "õpetaja", "lasteai", excl1 = "abi"))
new$new = asendaja(new$new, "õpetaja", "lasteai", excl1 = "abi", excl2 = "pastor", excl3 = "kultuuri", excl4 = "puhastus", replace = "lasteaiaõpetaja")

unique(leidur(new$new, "õpetaja", "koolieel"))
new$new = asendaja(new$new, "õpetaja", "koolieel", replace = "lasteaiaõpetaja")

unique(leidur(new$new, "lasteaia", "kasvataja", excl1 = "abi"))
new$new = asendaja(new$new, "lasteaia", "kasvataja", excl1 = "abi", replace = "lasteaiaõpetaja")

unique(leidur(new$new, "õpetaja", "keskkool"))
new$new = asendaja(new$new, "õpetaja", "keskkool", excl1 = "agronoom", excl2 = "põhi", replace = "keskkooliõpetaja")

unique(leidur(new$new, "õpetaja", "gümn"))
new$new = asendaja(new$new, "õpetaja", "gümn", excl1 = "põhi", replace = "keskkooliõpetaja")

#koodides eristatakse huvikoolide keeleõpetajaid, aga ilmselt ei saa siin alati ka kindel olla, et kas töökoht on huvikool või üldkool (või mõlemad)
#jätsin praegu keeleõpetajaks, kuigi võtsin välja täiskasvanute keeleõpetajad, kes lähevad selgemalt selle huvikoolide teema alla (võimalik, et lõpuks võiks nad kõik kokku panna lihtsalt õpetajateks?)
unique(leidur(new$new, "õpetaja", "keel"))
new$new = asendaja(new$new, "õpetaja", "keel", excl1 = "tehnoloog", excl2 = "filoloog", excl3 = "täisk", excl4 = "paberi",  replace = "keeleõpetaja")

unique(leidur(new$new, "õpetaja", "muusikakool"))
new$new = asendaja(new$new, "õpetaja", "muusikakool", replace = "muusikakooliõpetaja")

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

unique(leidur(new$new, "õpetaja", "õpiabi"))
new$new = asendaja(new$new, "õpetaja", "õpiabi", replace = "eripedagoog")

unique(leidur(new$new, "õpetaja", "abi", "lasteaia"))
new$new = asendaja(new$new, "õpetaja", "abi", "lasteaia", excl1 = "puidu", excl2 = "giid", replace = "abiõpetaja")
new$new = asendaja(new$new, "kasvataja", "abi", replace = "abiõpetaja")

unique(leidur(new$new, "õpetaja", "abi"))
new$new = asendaja(new$new, "õpetaja", "abi", excl1 = "puidu", excl2 = "giid", excl3 = "tegevusterapeut", excl4 = "kokk", replace = "abiõpetaja")

unique(leidur(new$new, "õde", "õpetaja"))
new$new = asendaja(new$new, "õde", "õpetaja", replace = "õppejõud")

unique(leidur(new$new, "ehitus", "tööline"))
new$new = asendaja(new$new, "ehitus", "tööline", excl1 = "laot", excl2 = "abi", replace = "ehitustööline")

unique(leidur(new$new, "ehitus", "tööde", "juh"))
new$new = asendaja(new$new, "ehitus", "tööde", "juh", excl1 = "fie", replace = "ehitustöödejuht")

unique(leidur(new$new, "ehitus", "obje", "juh"))
new$new = asendaja(new$new, "ehitus", "obje", "juh", replace = "ehitustöödejuht")

unique(leidur(new$new, "puu", "sepp"))
new$new = asendaja(new$new, "puu", "sepp", excl1 = "üldehi", excl2 = "ehitaja.", excl3 = "ehitaja-", replace = "puusepp")

unique(leidur(new$new, "tisler", ""))
new$new = asendaja(new$new, "tisler", "", replace = "puusepp")

unique(leidur(new$new, "politseinik", ""))
new$new = asendaja(new$new, "politseinik", "", replace = "politseinik")

#unique(leidur(new$new, "laborant", ""))
#new$new = asendaja(new$new, "laborant", "", replace = "laborant")

unique(leidur(new$new, "valla", "sekretär"))
new$new = asendaja(new$new, "valla", "sekretär", excl1 = "abi", replace = "vallasekretär")

unique(leidur(new$new, "jurist", "kohtu"))
new$new = asendaja(new$new, "jurist", "kohtu", replace = "kohtujurist-advokaat")
new$new = asendaja(new$new, "advoka", "", replace = "kohtujurist-advokaat")

unique(leidur(new$new, "prokurör", ""))
new$new = asendaja(new$new, "prokurör", "", replace = "prokurör")

unique(leidur(new$new, "ajakirjanik", ""))
new$new = asendaja(new$new, "ajakirjanik", "", excl1 = "õpetaja", excl2 = "pressiesi", replace = "ajakirjanik")

unique(leidur(new$new, "ajal", "toimet"))
new$new = asendaja(new$new, "ajal", "toimet", replace = "ajakirjanik")
new$new = asendaja(new$new, "ajak", "toimet", excl1 = "raamatu", replace = "ajakirjanik")
new$new = asendaja(new$new, "reporter", "", replace = "ajakirjanik")

unique(leidur(new$new, "programm", "veebi"))
new$new = asendaja(new$new, "programm", "veebi", replace = "veebiprogrammeerija")
new$new = asendaja(new$new, "programm", "web", replace = "veebiprogrammeerija")
new$new = asendaja(new$new, "develop", "web", replace = "veebiprogrammeerija")

unique(leidur(new$new, "programmeerija", ""))
new$new = asendaja(new$new, "programmeerija", "", excl1 = "cnc", excl2 = "veebi", excl3 = "automaatika", excl4 = "matemaatik", replace = "programmeerija")

unique(leidur(new$new, "tarkvara", "inse"))
new$new = asendaja(new$new, "tarkvara", "insener", replace = "programmeerija")

unique(leidur(new$new, "tarkvara", "arhitekt"))
new$new = asendaja(new$new, "tarkvara", "arhitekt", replace = "programmeerija")
new$new = asendaja(new$new, "tarkvara", "arendaja", replace = "programmeerija")

unique(leidur(new$new, "tarkvara", "arend", excl1 = "juht"))
new$new = asendaja(new$new, "tarkvara", "arend", excl1 = "juht", excl2 = "juhin", replace = "programmeerija")
new$new = asendaja(new$new, "infoteh", "arend", excl1 ="juht", replace = "programmeerija")

unique(leidur(new$new, "süsteem", "arhitekt"))
new$new = asendaja(new$new, "süsteem", "arhitekt", replace = "süsteemianalüütik")

unique(leidur(new$new, "süsteem", "analüütik"))
new$new = asendaja(new$new, "süsteem", "analüütik", excl1 = "elektri", excl2 = "turundus", replace = "süsteemianalüütik")

unique(leidur(new$new, "äri", "analüütik", "it"))
new$new = asendaja(new$new, "äri", "analüütik", "it", replace = "süsteemianalüütik")

unique(leidur(new$new, "tooteomanik", "it"))
new$new = asendaja(new$new, "tooteomanik", "it", excl1 = "andmekaitse", replace = "süsteemianalüütik")

unique(leidur(new$new, "liini", "töö"))
new$new = asendaja(new$new, "liini", "töö", excl1 = "kliin", excl2 = "operaator", excl3 = "montöör", replace = "liinitööline")

unique(leidur(new$new, "osaühing", "juh"))
new$new = asendaja(new$new, "osaühing", "juh", excl1 = "disainer", replace = "ettevõtja")

unique(leidur(new$new, "bussijuht", ""))
new$new = asendaja(new$new, "bussijuht", "", excl1 = "õpet", excl2 = "auto", replace = "bussijuht")
new$new = asendaja(new$new, "trammijuht", "", replace = "bussijuht")
#trollijuht ka siia, aga liiga palju muid kontrollijuhte tuleb kaasa?

# Vbl peaks alustama spetsiifilisemast, nt kaugsõidu-rahvusvahelised-autojuhid eraldi, taksojuhid eraldi -- psühholoogiselt erinev töö?
new$new = asendaja(new$new, "autojuht", "kaug",replace = "kaugsõiduautojuht")
new$new = asendaja(new$new, "rekka", "juht",replace = "kaugsõiduautojuht")
new$new = asendaja(new$new, "pikamaa", "juht",replace = "kaugsõiduautojuht")
new$new = asendaja(new$new, "rahvusvahel", "autojuht",replace = "kaugsõiduautojuht")

unique(leidur(new$new, "autojuht", "veo"))
new$new = asendaja(new$new, "autojuht", "veo", excl1 = "metsa", replace = "autojuht")
new$new = asendaja(new$new, "taksojuht", "", replace = "autojuht")
new$new = asendaja(new$new, "autojuht", "betooni", replace = "autojuht")

unique(leidur(new$new, "traktori", ""))
new$new = asendaja(new$new, "traktori", "", excl1 = "auto", excl2 = "raamatupidaja", excl3 = "hoold", excl4 = "ensvaja", replace = "põllumasinajuht")
new$new = asendaja(new$new, "harvester", "", replace = "põllumasinajuht")

unique(leidur(new$new, "metsa", "auto"))
new$new = asendaja(new$new, "metsa", "auto", replace = "metsamasinajuht")
new$new = asendaja(new$new, "metsa", "masin", excl1 = "hooldus", replace = "metsamasinajuht")
new$new = asendaja(new$new, "forvarder", "", replace = "metsamasinajuht")

unique(leidur(new$new, "buldooseri", ""))
new$new = asendaja(new$new, "buldooseri", "", replace = "teemasinajuht")
new$new = asendaja(new$new, "ekskavaator", "", replace = "teemasinajuht")

unique(leidur(new$new, "ettekandja", ""))
new$new = asendaja(new$new, "ettekandja", "", replace = "ettekandja")
new$new = asendaja(new$new, "baaridaam", "", excl1 = "juhataja", excl2 = "lillemüüja", replace = "ettekandja")
new$new = asendaja(new$new, "kelner", "", replace = "ettekandja")
new$new = asendaja(new$new, "baarm", "", excl1 = "ehitaja", replace = "ettekandja")
new$new = asendaja(new$new, "barman", "", excl1 = "manager", replace = "ettekandja")
new$new = asendaja(new$new, "kohvik", "teenin", excl1 = "kokk", excl2 = "omanik", replace = "ettekandja")
new$new = asendaja(new$new, "restoran", "teenin", excl1 = "juht", replace = "ettekandja")

unique(leidur(new$new, "tõlkija", ""))
unique(leidur(new$new, "keele", "toimetaja"))
new$new = asendaja(new$new, "tõlkija", "", replace = "tõlkija-toimetaja")
new$new = asendaja(new$new, "keele", "toimetaja", replace = "tõlkija-toimetaja")

# sotsiaaltöötajaid on jälle mitmes kategoorias - on sotsiaaltöötajad/tippspetsialistid ja keskastmespetsialistid (sotsiaaltööspetsialistid ISCO nimetuste kaudu)
# lastekaitsjatega on eriti keeruline - tippspetsialist (sotsiaaltöötajatega ühes) on "lastekaitse tippspetsialist" ja keskasmes on "lastekaitsespetsialist"
unique(leidur(new$new, "sotsiaaltöötaja", ""))
new$new = asendaja(new$new, "sotsiaaltöötaja", "", excl1 = "tegevuster", excl2 = "raamatupidaja", excl3 = "eraettevõtja", replace = "sotsiaaltöötaja")
unique(leidur(new$new, "sotsiaaltöö", "spetsialist"))
new$new = asendaja(new$new, "sotsiaaltöö", "spetsialist", excl1 = "huvijuht", replace = "sotsiaaltööspetsialist")

new$new = asendaja(new$new, "laste", "kaitse",replace = "sotsiaaltöötaja")
new$new = asendaja(new$new, "sotsiaalnõunik", "",replace = "sotsiaaltöötaja")
new$new = asendaja(new$new, "kriminaalhooldus", "",replace = "sotsiaaltöötaja")
new$new = asendaja(new$new, "sotsiaal", "peda", excl1 = "õpetaja", excl2 = "juht", replace = "sotsiaaltöötaja")
new$new = asendaja(new$new, "sotsiaal", "nõu", replace = "sotsiaaltöötaja")
new$new = asendaja(new$new, "kogemus", "nõu", replace = "sotsiaaltöötaja")

unique(leidur(new$new, "müügisekretär", ""))
new$new = asendaja(new$new, "müügisekretär", "", excl1 = "juhiabi", replace = "raamatupidamisekontoritöötaja")

unique(leidur(new$new, "müügi", "tele"))
new$new = asendaja(new$new, "müügi", "tele", replace = "telefonimüüja")


unique(leidur(new$new, "riigiametnik", ""))
new$new = asendaja(new$new, "riigiametnik", "", replace = "riigiametnik")
new$new = asendaja(new$new, "riigiteenistuja", "", replace = "riigiametnik")

unique(leidur(new$new, "eripedagoog", ""))
new$new = asendaja(new$new, "eripedagoog", "", excl1 = "logopeed", replace = "eripedagoog")

unique(leidur(new$new, "logopeed", ""))
new$new = asendaja(new$new, "logopeed", "", excl1 = "eripedagoog-", replace = "logopeed")

unique(leidur(new$new, "füsiotera", ""))
new$new = asendaja(new$new, "füsiotera", "", excl1 = "treener", excl2 = "õpin", replace = "füsioterapeut")
new$new = asendaja(new$new, "füsioterapeut;", "", replace = "füsioterapeut")
new$new = asendaja(new$new, "füsioterepeut;", "", replace = "füsioterapeut")

unique(leidur(new$new, "müügispetsialist", ""))
new$new = asendaja(new$new, "müügispetsialist", "", excl1 = "juht", replace = "müügiesindaja")

unique(leidur(new$new, "turundus", "spetsial"))
new$new = asendaja(new$new, "turundus", "spetsial", excl1 = "juht", excl2 = "kommunikatsioonija", replace = "müügiesindaja")

unique(leidur(new$new, "müügi", "esindaja"))
new$new = asendaja(new$new, "müügi", "esindaja", excl1 = "meditsiini", excl2 = "agronoom", excl3 = "maakler", excl4 = "logistik", replace = "müügiesindaja")
new$new = asendaja(new$new, "müügi", "esindaja-", replace = "müügiesindaja")
new$new = asendaja(new$new, "müügi", "mees", excl1 = "juht", replace = "müügiesindaja")

unique(leidur(new$new, "sekretär", "asja"))
new$new = asendaja(new$new, "sekretär", "asja", replace = "juhiabi")
new$new = asendaja(new$new, "juhiabi", "sekretär", replace = "juhiabi")
new$new = asendaja(new$new, "juhiabi-", "", replace = "juhiabi")

#unique(leidur(new$new, "sekretär", ""))
#new$new = asendaja(new$new, "sekretär", "", replace = "sekretär")

unique(leidur(new$new, "analüütik", "finants"))
new$new = asendaja(new$new, "analüütik", "finants", replace = "finantsanalüütik")

#ISCO-s on "ärianalüütik" ainult IT-elukutsete all, mõtlen veel selle peale
#new$new = asendaja(new$new, "analüütik", "krediidi", replace = "ärianalüütik")
#new$new = asendaja(new$new, "analüütik", "majandus", replace = "ärianalüütik")
#new$new = asendaja(new$new, "analüütik", "müügi", replace = "ärianalüütik")
#new$new = asendaja(new$new, "analüütik", "riski", replace = "ärianalüütik")
#new$new = asendaja(new$new, "analüütik", "tootmis", replace = "ärianalüütik")
#new$new = asendaja(new$new, "analüütik", "toote", replace = "ärianalüütik")
#new$new = asendaja(new$new, "analüütik", "eelarve", replace = "ärianalüütik")

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

unique(leidur(new$new, "analüütik", "bio"))
new$new = asendaja(new$new, "analüütik", "bio", replace = "bioanalüütik")
new$new = asendaja(new$new, "med", "laborant", replace = "bioanalüütik")
new$new = asendaja(new$new, "med", "labor", excl1 = "juh", replace = "bioanalüütik")

#tegelikult saab neid sõjaväelasi ka erinevatesse astmetesse jagada, aga olen ka mõelnud, et ehk üks sõjaväelaste grupp oleks mõistlik
unique(leidur(new$new, "sõjaväelane", ""))
new$new = asendaja(new$new, "sõjaväelane", "", replace = "sõjaväelane")
new$new = asendaja(new$new, "sõdur", "", replace = "sõjaväelane")
new$new = asendaja(new$new, "kaitseväelane", "", excl1 = "hambaravi", excl2 = "elektrik", replace = "sõjaväelane")

unique(leidur(new$new, "ohvitser", ""))
new$new = asendaja(new$new, "ohvitser", "vanem", replace = "sõjaväelane")
new$new = asendaja(new$new, "ohvitser", "all", replace = "sõjaväelane")
new$new = asendaja(new$new, "ohvitser", "staabi", replace = "sõjaväelane")
new$new = asendaja(new$new, "ohvitser", "väe", replace = "sõjaväelane")
new$new = asendaja(new$new, "kaitsevägi", "", replace = "sõjaväelane")

unique(leidur(new$new, "", "kapten"))
new$new = asendaja(new$new, "kapten", "", excl1 = "erias", excl2 = "lennu", excl3 = "ettevõtte", replace = "kapten-laevajuht")
new$new = asendaja(new$new, "laevajuht", "", replace = "kapten-laevajuht")
new$new = asendaja(new$new, "loots", "", replace = "kapten-laevajuht")
new$new = asendaja(new$new, "tüürimees", "", replace = "kapten-laevajuht")

unique(leidur(new$new, "lasteaia", "direktor"))
new$new = asendaja(new$new, "direktor", "lasteaia",replace = "lasteaiadirektor")
new$new = asendaja(new$new, "lasteaia", "juht", replace = "lasteaiadirektor")
new$new = asendaja(new$new, "lasteaia", "juhataja", excl1 = "majandus", replace = "lasteaiadirektor")
new$new = asendaja(new$new, "laste", "õppejuht", replace = "lasteaiadirektor")
new$new = asendaja(new$new, "alus", "õppejuht", replace = "lasteaiadirektor")

new$new = asendaja(new$new, "lasteaia", "juhataja", "majandus", replace = "majandusjuhataja")
new$new = asendaja(new$new, "majandusjuhataja", "", replace = "majandusjuhataja")

new$new = asendaja(new$new, "õppeala", "juhataja",replace = "haridusasutustejuhid")
new$new = asendaja(new$new, "õppejuht", "",replace = "haridusasutustejuhid")
new$new = asendaja(new$new, "õppetööjuht", "",replace = "haridusasutustejuhid")
new$new = asendaja(new$new, "õppetooli", "",replace = "haridusasutustejuhid")
new$new = asendaja(new$new, "instituudi", "juh",replace = "haridusasutustejuhid")

unique(leidur(new$new, "õppegrupp", "juh"))
new$new = asendaja(new$new, "õppegrupp", "juh",replace = "pedagoogikatippspetsialist")

unique(leidur(new$new, "õppe", "juh"))
new$new = asendaja(new$new, "õppe", "juh", excl1 = "juhtivõp", excl2 = "õppejõud", excl3 = "toitlustus", excl4 = "dotsent",  replace = "haridusasutustejuhid")

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

new$new = asendaja(new$new, "hooldaja", "hobu", replace = "loomahooldaja")
new$new = asendaja(new$new, "hooldaja", "looma", replace = "loomahooldaja")
new$new = asendaja(new$new, "talitaja", "", replace = "loomahooldaja")
new$new = asendaja(new$new, "loomaarsti", "abi", replace = "loomahooldaja")
new$new = asendaja(new$new, "loomaarsti", "assis", replace = "loomahooldaja")
new$new = asendaja(new$new, "loomaarsti", "assis", replace = "loomahooldaja")
new$new = asendaja(new$new, "veter", "assis", replace = "loomahooldaja")
new$new = asendaja(new$new, "veter", "assis", replace = "loomahooldaja")
new$new = asendaja(new$new, "veter", "abi", replace = "loomahooldaja")
new$new = asendaja(new$new, "uluki", "hoold", replace = "loomahooldaja")

unique(leidur(new$new, "kuller", ""))
new$new = asendaja(new$new, "kuller", "", excl1 = "toidu", excl2 = "ratta", excl3 = "raamatu", replace = "postlijon-kuller")
new$new = asendaja(new$new, "kirjakandja", "",replace = "postlijon-kuller")
new$new = asendaja(new$new, "postlijon", "",replace = "postlijon-kuller")
## toidukullerile ei olegi nagu head vastet - on olemas "rattakuller" (veonduse lihttöölise valdkond), samas kõik toidukullerid pole rattaga
## tavaline "kuller" on aga posti kättetoimetajate valdkonnas, mis ka justkui toidule ei sobi? Samas neid vist liiga palju ka ei ole, et lõputult pead murda


unique(leidur(new$new, "ehitus", "tööline"))
new$new = asendaja(new$new, "ehitus", "tööline", excl1 = "abi", excl2 = "lao", replace = "ehitaja")
new$new = asendaja(new$new, "ehitusvaldkond", "",replace = "ehitaja")
new$new = asendaja(new$new, "üldehitaja", "", excl1 = "keevitaja", replace = "ehitaja")

#siin jaotatakse neid ehitajaid veel mitmetesse gruppidesse - ilmselt oleks mõistlik midagi ühendada, nt ehitajad ja ehitusviimistlejad?
new$new = asendaja(new$new, "ehitusviimistleja", "",replace = "ehitusviimistleja")
new$new = asendaja(new$new, "siseviimistl", "",replace = "ehitusviimistleja")
new$new = asendaja(new$new, "krohv", "",replace = "ehitusviimistleja")
new$new = asendaja(new$new, "plaati", "", excl1 = "trükk", excl2 = "valmistamine", replace = "ehitusviimistleja")
new$new = asendaja(new$new, "katuse", "", excl1 = "keevitaja", excl2 = "projektijuht", excl3 = "prekksepp", replace = "ehitusviimistleja")
unique(leidur(new$new, "san", "tehnik"))
new$new = asendaja(new$new, "san", "tehnik", excl1 = "juhat", excl2 = "mets", excl3 = "keevitaja", replace = "torulukksepp")
new$new = asendaja(new$new, "toru", "lukksepp", replace = "torulukksepp")

new$new = asendaja(new$new, "maaler", "", excl1 = "auto", replace = "ehitusmaalrid")

table(leidur(new$new, "tegevjuht", "")) %>% sort
new$new = asendaja(new$new, "tegevjuht", "firma",replace = "tegevjuht")
new$new = asendaja(new$new, "tegevjuht", "ettev",replace = "tegevjuht")

##personalijuhte on mitmel tasemel (oleneb, kas on juht alluvatega või ilma jne, vaatasin enne selle work_position_name kaudu) - võib-olla kõik kokku panna ja öelda, et mis koodidega personalijuhid ISCO-s on?
## sest ega inimesed ise ka ei tea vahel, kas nad on tipp- või keskastmespetsialistid jne
unique(leidur(new$new, "personalijuht", ""))
new$new = asendaja(new$new, "personalijuht", "", excl1 = "finants", excl2 = "büroo", excl3 = "kvaliteedi", excl4 = "jurist", replace = "personalijuht")
new$new = asendaja(new$new, "personalipartner", "", replace = "personalijuht")
new$new = asendaja(new$new, "hr", "", excl1 = "support", excl2 = "müügi", excl3 = "kassa", replace = "personalijuht")

#psühholooge mitme koodiga, aga võib-olla mõistlik kõik kokku panna ja jälle ära näidata, kes gruppi kuuluvad?
unique(leidur(new$new, "psühho", "",))
new$new = asendaja(new$new, "psühho", "", excl1 = "õpetaja", excl2 = "koorijuht", replace = "psühholoog")

unique(leidur(new$new, "finantsjuht", ""))
new$new = asendaja(new$new, "finantsjuht", "", excl1 = "tarkvara", excl2 = "ettevõttejuht", excl3 = "praktika", replace = "finantsjuht")
new$new = asendaja(new$new, "finantsjuh", "", excl1 = "abi", excl2 = "assis", excl3 = "praktikal", excl4 = "ettevõtte", replace = "finantsjuht")
new$new = asendaja(new$new, "finantsdir", "", replace = "finantsjuht")
new$new = asendaja(new$new, "cfo", "", replace = "finantsjuht")

unique(leidur(new$new, "turundusjuh", "",))
new$new = asendaja(new$new, "turundusjuh", "", excl1 = "abi", replace = "turundusjuht")

new$new = asendaja(new$new, "maja", "haldur", replace = "kinnisvarahaldur")
new$new = asendaja(new$new, "kinnisvara", "haldur", replace = "kinnisvarahaldur")
new$new = asendaja(new$new, "korteriühistu", "esi", replace = "kinnisvarahaldur")
new$new = asendaja(new$new, "kinnisvara", "maakler", excl1 = "raamatupidaja", excl2 = "jurist", replace = "kinnisvarahaldur")

new$new = asendaja(new$new, "kinnisvara", "hoold", replace = "kinnisvarahooldaja")

new$new = asendaja(new$new, "piloot", "", replace = "piloot")
new$new = asendaja(new$new, "lennuki", "kapten", replace = "piloot")

unique(leidur(new$new, "stjua", "",))
new$new = asendaja(new$new, "stju", "", replace = "reisisaatja")
new$new = asendaja(new$new, "kajuti", "", replace = "reisisaatja")

unique(leidur(new$new, "giid", "",))
new$new = asendaja(new$new, "giid", "", excl1 = "tõlkja", excl2 = "firmaomanik", replace = "giid")

#unique(leidur(new$new, "mehaanik", "",))
#new$new = asendaja(new$new, "mehaanik", "", replace = "mehaanik")

unique(leidur(new$new, "apteek", "",))
new$new = asendaja(new$new, "apteeker", "", excl1 = "omanik", replace = "apteeker")
new$new = asendaja(new$new, "proviisor", "", replace = "apteeker")

#unique(leidur(new$new, "tehnik", "hoold",))
#new$new = asendaja(new$new, "tehnik", "hoold", replace = "hooldustehnik")

#unique(leidur(new$new, "insener", "hoold",))
#new$new = asendaja(new$new, "insener", "hoold", replace = "hooldustehnik")

unique(leidur(new$new, "kondiiter", "",))
new$new = asendaja(new$new, "kondiiter", "", excl1 = "kokk-", replace = "pagar-kondiiter")
new$new = asendaja(new$new, "pagar", "", excl1 = "kokk", excl2 = "juh", excl3 = "omanik", excl4 = "personali",  replace = "pagar-kondiiter")

unique(leidur(new$new, "kokk", "",))
new$new = asendaja(new$new, "kokk", "pea", excl1 = "kohvikuj", replace = "peakokk")
new$new = asendaja(new$new, "kokk", "vanem", replace = "peakokk")
new$new = asendaja(new$new, "toitlustusjuht", "", replace = "peakokk")
new$new = asendaja(new$new, "söökla", "juhataja", excl1 = "kokk-", replace = "peakokk")

new$new = asendaja(new$new, "kok", "abi", replace = "kokk")
new$new = asendaja(new$new, "kokk", "pagar", replace = "kokk")
new$new = asendaja(new$new, "kokk", "kondiiter", replace = "kokk")
new$new = asendaja(new$new, "kokk", "restoran", replace = "kokk")

unique(leidur(new$new, "insener", "ehit",))
new$new = asendaja(new$new, "insener", "ehitus", replace = "ehitusinsener")
new$new = asendaja(new$new, "insener", "ehitus", replace = "ehitusinsener")

unique(leidur(new$new, "insener", "elektri",))
new$new = asendaja(new$new, "insener", "elektri", excl1 = "tsehhi", replace = "elektriinsener")
new$new = asendaja(new$new, "insener", "elektroonika", replace = "elektroonikainsener")

unique(leidur(new$new, "insener",""))
new$new = asendaja(new$new, "insener", "pea", excl1 = "ehitus", replace = "insener")

#siin kuidagi kirju seltskond?
#new$new = asendaja(new$new, "insener", "vanem", replace = "insener")

unique(leidur(new$new, "fotograaf",""))
new$new = asendaja(new$new, "fotograaf", "", excl1 = "kunstnik", excl2 = "mööbli", replace = "fotograaf")

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
new$new = asendaja(new$new, "koorijuht", "", excl1 = "õpetaja", replace = "muusik")
new$new = asendaja(new$new, "muusik", "erso", replace = "muusik")
new$new = asendaja(new$new, "muusik", "kirik", replace = "muusik")
new$new = asendaja(new$new, "muusik", "orkes", replace = "muusik")
new$new = asendaja(new$new, "muusik", "viiul", replace = "muusik")
new$new = asendaja(new$new, "muusik", "profe", replace = "muusik")
new$new = asendaja(new$new, "muusik", "kutse", replace = "muusik")

unique(leidur(new$new, "raamatukogu","hoidja"))
new$new = asendaja(new$new, "raamatukogu", "hoidja", excl1 = "õpetaja+", replace = "raamatukoguhoidja")
new$new = asendaja(new$new, "raamatukogu", "töötaja", replace = "raamatukogutöötaja")
new$new = asendaja(new$new, "raamatukogu", "klienditeenind", excl1 = "juh", replace = "raamatukogutöötaja")

unique(leidur(new$new, "raamatukogu","juh"))
new$new = asendaja(new$new, "raamatukogu", "juh", replace = "raamatukogujuht")

#unique(leidur(new$new, "audiitor",""))
#new$new = asendaja(new$new, "audiitor", "", replace = "audiitor")

unique(leidur(new$new, "lao", ""))
new$new = asendaja(new$new, "laooperaator", "", replace = "laoametnik")
new$new = asendaja(new$new, "laohoidja", "", replace = "laoametnik")
new$new = asendaja(new$new, "laospetsialist", "", replace = "laoametnik")
new$new = asendaja(new$new, "laoarvestuses", "", replace = "laoametnik")
new$new = asendaja(new$new, "laopidaja", "", replace = "laoametnik")

new$new = asendaja(new$new, "laotöötaja", "", excl1 = "kontrolör", excl2 = "raamatup", excl3 = "logistik", replace = "laotöötaja")
new$new = asendaja(new$new, "laomees", "", replace = "laotöötaja")

new$new = asendaja(new$new, "laotööline", "", replace = "laotööline")

new$new = asendaja(new$new, "laojuhataja", "", replace = "laojuhataja")
new$new = asendaja(new$new, "laojuht", "", replace = "laojuhataja")
new$new = asendaja(new$new, "laohaldur", "", replace = "laojuhataja")
new$new = asendaja(new$new, "laovanem", "", replace = "laojuhataja")

unique(leidur(new$new, "ettevõtte", "juht", "väike"))
new$new = asendaja(new$new, "ettevõtte", "juht", "väike", excl1 = "tootmis", excl2 = "turundus", replace = "väikeettevõttejuht")
new$new = asendaja(new$new, "firma", "juht", "väike", replace = "väikeettevõttejuht")

unique(leidur(new$new, "ettevõttejuh", "omanik", excl1 = "väike", excl2 = "mikro"))
new$new = asendaja(new$new, "ettevõttejuh","omanik", excl1 = "väike", excl2 = "mikro", excl3 = "majutus", replace = "ettevõttejuht")

unique(leidur(new$new, "ettevõtte", "tippjuht"))
new$new = asendaja(new$new, "ettevõtte", "tippjuht", replace = "ettevõttejuht")

unique(leidur(new$new, "piirivalvur", ""))
new$new = asendaja(new$new, "piirivalvur", "", replace = "piirivalvur")

unique(leidur(new$new, "metsa", "töö"))
new$new = asendaja(new$new, "metsandusspetsialist", "", replace = "metsandusspetsialist")
new$new = asendaja(new$new, "metsandusespetsialist", "", replace = "metsandusspetsialist")
new$new = asendaja(new$new, "metsaspetsialist", "", replace = "metsandusspetsialist")

new$new = asendaja(new$new, "metsakorraldaja", "", replace = "metsandustehnik")
new$new = asendaja(new$new, "abimetsaülem", "", replace = "metsandustehnik")
new$new = asendaja(new$new, "metsa", "tehnik", excl1 = "aia", excl2 = "rasketehnika", replace = "metsandustehnik")
new$new = asendaja(new$new, "metsavaht", "", replace = "metsandustehnik")

new$new = asendaja(new$new, "metsaülem", "", replace = "metsandusjuht")

new$new = asendaja(new$new, "metsameister", "", replace = "metsatööline")
new$new = asendaja(new$new, "metsatööline", "", replace = "metsatööline")
new$new = asendaja(new$new, "metsaistutaja", "", replace = "metsatööline")
new$new = asendaja(new$new, "metsalangetaja", "", replace = "metsatööline")
new$new = asendaja(new$new, "metsamees", "", replace = "metsatööline")
new$new = asendaja(new$new, "metsnik", "", replace = "metsatööline")

## kõik kokku panna?
unique(leidur(new$new, "õmbleja", ""))
new$new = asendaja(new$new, "õmbleja", "", excl1 = "jalatsite", excl2 = "teenindusjuht", excl3 = "raamatupidaja", excl4 = "rätsep-", replace = "õmbleja")
new$new = asendaja(new$new, "juurdelõikaja", "", excl1 = "rätsep", excl2 = "kunstnik", replace = "juurdelõikaja")
new$new = asendaja(new$new, "rätsep", "", excl1 = "tipp", excl2 = "juurdelõikaja-", replace = "rätsep")

new$new = asendaja(new$new, "polsterdaja", "", replace = "polsterdaja")
new$new = asendaja(new$new, "kingsepp", "", replace = "kingsepp")
new$new = asendaja(new$new, "jalats", "", excl1 = "kunstnik", replace = "kingsepp")


unique(leidur(new$new, "kunstnik", ""))
unique(leidur(new$new, "kunstnik", "mängu"))
new$new = asendaja(new$new, "kunstnik", "mängu", replace = "multimeediadisainer")
new$new = asendaja(new$new, "graafiline", "disainer", excl1 = "turundus", replace = "multimeediadisainer")
new$new = asendaja(new$new, "ux", "dis", replace = "multimeediadisainer")
new$new = asendaja(new$new, "ui", "disainer", replace = "multimeediadisainer")
new$new = asendaja(new$new, "3d", "", replace = "multimeediadisainer")

unique(leidur(new$new, "moe", "kunstnik"))
new$new = asendaja(new$new, "moe", "kunstnik", replace = "moedisainer")
new$new = asendaja(new$new, "moe", "disainer", replace = "moedisainer")

new$new = asendaja(new$new, "kunstnik", "maali", replace = "kunstnik")

unique(leidur(new$new, "logistik", "juh", excl1 = "abi"))
new$new = asendaja(new$new, "logistik", "juh", excl1 = "abi", excl2 = "auto", excl3 = "laadur", excl4 = "töödejuh", replace = "logistikajuht")
new$new = asendaja(new$new, "logistik", "dir", replace = "logistikajuht")
new$new = asendaja(new$new, "logistik", "manager", replace = "logistikajuht")

unique(leidur(new$new, "logistik", "", excl1 = "abi"))
new$new = asendaja(new$new, "logistik", "", excl1 = "abi", excl2 = "juht", excl3 = "assis", excl4 = "raamatup",  replace = "logistik")

#erinevaid kvaliteedijuhte on - on nö juhtimisanalüütikute grupis ja tootmise kvaliteedijuhid (tehnika, tehnoloogia jne)
unique(leidur(new$new, "kvaliteed", "juh", excl1 = "assist"))
new$new = asendaja(new$new, "kvaliteedi", "juh", excl1 = "assistent", excl2 = "lagede", replace = "kvaliteedijuht")

unique(leidur(new$new, "režissöör", ""))
new$new = asendaja(new$new, "režissöör", "", excl1 = "helir", excl2 = "montaaž", replace = "filmirežissöör")
new$new = asendaja(new$new, "režissöör", "heli", excl1 = "filmi", replace = "keskastmerežissöör")
new$new = asendaja(new$new, "režissöör", "mont", excl1 = "filmi", replace = "keskastmerežissöör")


unique(leidur(new$new, "elektrik", "auto"))
new$new = asendaja(new$new, "elektrik", "auto", replace = "elektromehhaanik")

#siin järgmises on nii elektriseadmete mehaanikud (7412) kui ka elektroonikaseadmete mehhaanikud (7421) - ei suuda vahet teha
unique(leidur(new$new, "elektr", "meh"))
new$new = asendaja(new$new, "elektr", "meh", excl1 = "ittugi", replace = "elektromehhaanik")
new$new = asendaja(new$new, "elektr", "laeva", replace = "elektromehhaanik")

unique(leidur(new$new, "elektr", "tehnik"))
new$new = asendaja(new$new, "elektr", "tehnik", replace = "elektrotehnik")
new$new = asendaja(new$new, "elektr", "energ", replace = "elektrotehnik")
new$new = asendaja(new$new, "elektr", "käidu", replace = "elektrotehnik")

unique(leidur(new$new, "elektrik", "", excl1 = "kappide"))
new$new = asendaja(new$new, "elektrik", "", excl1 = "kappide", excl2 = "kilp", excl3 = "keevitaja", replace = "elektrik")

table(leidur(new$new, "spetsialist", "", "")) %>% sort(decreasing = T) %>% .[1:30]

#unique(leidur(new$new, "peaspetsialist", "", excl1 = "it"))
#new$new = asendaja(new$new, "peaspetsialist", "", excl1 = "it", replace = "vanemspetsialist")

#unique(leidur(new$new, "vanemspetsialist", "", excl1 = "it"))
#new$new = asendaja(new$new, "vanemspetsialist", "", excl1 = "it", replace = "vanemspetsialist")

#unique(leidur(new$new, "spetsialist", "juhtiv", excl1 = "it"))
#new$new = asendaja(new$new, "spetsialist", "juhtiv", excl1 = "it", replace = "vanemspetsialist")

unique(leidur(new$new, "spetsialist", "ostu"))
new$new = asendaja(new$new, "spetsialist", "ostu", replace = "hankepetsialist")
new$new = asendaja(new$new, "spetsialist", "hanke", replace = "hankepetsialist")
new$new = asendaja(new$new, "ostu", "assist", excl1 = "juhi", replace = "hankepetsialist")
new$new = asendaja(new$new, "kauba", "kategooria", replace = "hankepetsialist")

# siin tundub väga kirju seltskond, 
#unique(leidur(new$new, "spetsialist", "labori"))
#new$new = asendaja(new$new, "spetsialist", "labori", replace = "laborispetsialist")

### veel spetsialiste sorteerida teemade järgi. Näiteks lastekaitsjad on nagu sotsiaaltöötajad? It omaette? Koolitus, andmed ..

table(leidur(new$new, "andme", "", "")) %>% sort(decreasing = T) %>% .[1:30]

unique(leidur(new$new, "andme", "töötl", excl1 = "it"))
new$new = asendaja(new$new, "andme", "tööt", excl1 = "juht", replace = "andmetöötleja")

unique(leidur(new$new, "andme", "sisest", excl1 = "it"))
new$new = asendaja(new$new, "andme", "sisest", excl1 = "juhataja-", excl2 = "geodeet", replace = "andmesisestaja")

# andmeteadurid ja matemaatikud sama koodi all (aga nimetasin esialgu ikka erinavalt)
unique(leidur(new$new, "andme", "tead"))
new$new = asendaja(new$new, "andme", "tead", excl1 = "juht", excl2 = "admin", replace = "andmeteadur")
unique(leidur(new$new, "andme", "analüüt"))
new$new = asendaja(new$new, "andme", "analüüt",replace = "andmeteadur")
new$new = asendaja(new$new, "statistik", "", excl1 = "osakond", excl2 = "toimetaja", replace = "andmeteadur")
new$new = asendaja(new$new, "aktuaar", "", replace = "andmeteadur")
new$new = asendaja(new$new, "matemaatik", "",replace = "matemaatik")

unique(leidur(new$new, "auto", "remondi", ""))
new$new = asendaja(new$new, "auto", "remondi", excl1 = "plekk", excl2 = "töökoja", replace = "automehhaanik-lukksepp")
new$new = asendaja(new$new, "auto", "meh", replace = "automehhaanik-lukksepp")
new$new = asendaja(new$new, "auto", "diagn", replace = "automehhaanik-lukksepp")
new$new = asendaja(new$new, "masina", "diagn", replace = "automehhaanik-lukksepp")
new$new = asendaja(new$new, "rehv", "tehnik", replace = "automehhaanik-lukksepp")

unique(leidur(new$new, "keevitaja", "", ""))
new$new = asendaja(new$new, "keevitaja-", "",replace = "keevitaja")
new$new = asendaja(new$new, "keevitaja", "", excl1 = "luk", excl2 = "trei", excl3 = "ehitaja", replace = "keevitaja")

unique(leidur(new$new, "luksepp", "", ""))
new$new = asendaja(new$new, "luksepp", "",replace = "lukksepp")
new$new = asendaja(new$new, "lukksepp", "remondi",replace = "remondilukksepp")

new$new = asendaja(new$new, "lukksepp", "", excl1 = "auto", excl2 = "remondi", excl3 = "treial", excl4 = "toru", replace = "lukksepp")

# masinaoperaatoreid on u 10 erineva koodi all ja üsna keeruline eristada (ISCO-s muidu materjali järgi saab, proovin)
#unique(leidur(new$new, "operaator", "masina", ""))

new$new = asendaja(new$new, "operaator", "masina", "trük", replace = "trükimasinaoperaator")
new$new = asendaja(new$new, "operaator", "klaas", replace = "klaasimasinaoperaator")
new$new = asendaja(new$new, "operaator", "metall", replace = "metallimasinaoperaator")
new$new = asendaja(new$new, "operaator", "puidu", replace = "puidumasinaoperaator")

new$new = asendaja(new$new, "operaator", "toidu", replace = "toiduainemasinaoperaator")
new$new = asendaja(new$new, "operaator", "saia", replace = "toiduainemasinaoperaator")
new$new = asendaja(new$new, "operaator", "villim", replace = "toiduainemasinaoperaator")
new$new = asendaja(new$new, "operaator", "juustu", replace = "toiduainemasinaoperaator")
new$new = asendaja(new$new, "operaator", "kohupiima", replace = "toiduainemasinaoperaator")

#new$new = asendaja(new$new, "operaator", "masina",replace = "masinaoperaator")
#new$new = asendaja(new$new, "operaator", "seadme",replace = "masinaoperaator")
#new$new = asendaja(new$new, "operaator", "pingi",replace = "masinaoperaator")
#new$new = asendaja(new$new, "operaator", "pink",replace = "masinaoperaator")
#new$new = asendaja(new$new, "operaator", "tehas",replace = "masinaoperaator")

# liinioperaatoritega ka sama jama, et koodi on raske panna
#unique(leidur(new$new, "operaator", "liini", ""))
#new$new = asendaja(new$new, "operaator", "liini",replace = "liinioperaator")

#proovin neid raamatupidajaid lahti harutada
unique(leidur(new$new, "raamatupida", "", excl1 = "abi"))
#new$new = asendaja(new$new, "raamatupida", "",excl1 = "abi", excl2 = "assist", excl3 = "vanem", excl4 = "pea", replace = "raamatupidaja")

new$new = asendaja(new$new, "raamatupidaja-", "", replace = "raamatupidaja")
new$new = asendaja(new$new, "raamatupidajaja", "", replace = "raamatupidaja")
unique(leidur(new$new, "ökonomist", ""))
new$new = asendaja(new$new, "ökonomist", "", replace = "ökonomist")
new$new = asendaja(new$new, "majandustead", "", replace = "ökonomist")
new$new = asendaja(new$new, "majandus", "analüü", replace = "ökonomist")

new$new = asendaja(new$new, "raamatupi", "tööt", excl1 = "sotsiaal", excl2 = "juhi", replace = "raamatupidamistöötaja")
new$new = asendaja(new$new, "finants", "tööt", replace = "raamatupidamistöötaja")
new$new = asendaja(new$new, "arve", "ametn", replace = "raamatupidamistöötaja")
new$new = asendaja(new$new, "palga", "arvest", excl1 = "juht",  replace = "palgaarvestaja")

new$new = asendaja(new$new, "raamatup", "spetsial", excl1 = "person", excl2 = "andme", replace = "raamatupidamisespetsialist")
new$new = asendaja(new$new, "finantssp", "",  replace = "raamatupidamisespetsialist")

new$new = asendaja(new$new, "eelarvest", "ehitus", replace = "ehituse eelarvestaja")
new$new = asendaja(new$new, "eelarvest", "insener", replace = "ehituse eelarvestaja") #selles natuke kahtlen?
new$new = asendaja(new$new, "eelarvest", "hoone", replace = "ehituse eelarvestaja")

new$new = asendaja(new$new, "eelarvest", "", excl1 = "juh", excl2 = "ehitus", replace = "raamatupidamisespetsialist")
new$new = asendaja(new$new, "assis", "raamatup", replace = "raamatupidamisespetsialist")

new$new = asendaja(new$new, "van", "raamatup", excl1 = "õpet", replace = "pearaamatupidaja")
new$new = asendaja(new$new, "bilans", "raamatup", replace = "pearaamatupidaja")
new$new = asendaja(new$new, "raamatupi", "juhtiv", replace = "pearaamatupidaja")
new$new = asendaja(new$new, "paraamatupi", "", replace = "pearaamatupidaja")
new$new = asendaja(new$new, "peraamatupi", "", replace = "pearaamatupidaja")

new$new = asendaja(new$new, "personali", "arvest", excl1 = "raamatupidajap", excl2 = "juht", replace = "personalitöötaja")
new$new = asendaja(new$new, "personali", "töötaja", excl1 = "raamatupid", excl2 = "sekretär", replace = "personalitöötaja")

# ikka väga kirju seltskond siin raamatupidajates (st paljudel "esimene nimetus" midagi muud)
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

# geoinfospetsialist on veel eraldi kood (keskastme spets), kuigi neid ainult 2 tk
new$new = asendaja(new$new, "geoinfo", "spetsialist", replace = "ehitustehnik")

unique(leidur(new$new, "veduri", "juh"))
new$new = asendaja(new$new, "veduri", "juht", replace = "rongijuht")

unique(leidur(new$new, "juhiabi", ""))
new$new = asendaja(new$new, "juhiabi", "", excl1 = "kahe", excl2 = "puidurestauraator", excl3 = "huvi", replace = "juhiabi")

unique(leidur(new$new, "kliend", "haldu"))
new$new = asendaja(new$new, "hald", "kliend", "panga", excl1 = "juh", replace = "pangateller")
new$new = asendaja(new$new, "hald", "kliend", "äri", excl1 = "juh", replace = "finantsnõustaja") # selles ärikliendis natuke kahtlen, aga nii ISCO ütleb
new$new = asendaja(new$new, "hald", "kliend", "ãri", excl1 = "juh", replace = "finantsnõustaja")
new$new = asendaja(new$new, "finants", "nõu", excl1 = "raamatu", replace = "finantsnõustaja")
new$new = asendaja(new$new, "võla", "nõu", excl1 = "nõudja", excl2 = "menetl", excl3 = "juht", replace = "finantsnõustaja")
new$new = asendaja(new$new, "invest", "nõu", excl1 = "juht", replace = "finantsnõustaja")
# suurkliendihaldur on ka tegelikult "finantsnõustaja", kuigi minu arvates peaks olema kindel, et tegu siis finatssektoriga - suurkliendid võivad ju ka nt müügitöös või kus iganes olla

# liiga palju välistusi peab tegema, hiljem üle vaadata
#unique(leidur(new$new, "kliend", "haldu"))
#unique(leidur(new$new, "hooldaja", ""))

unique(leidur(new$new, "tegevusjuhe", ""))
new$new = asendaja(new$new, "tegevusjuhe", "", replace = "tegevusjuhendaja")

unique(leidur(new$new, "majahoidja", ""))
new$new = asendaja(new$new, "majahoidja", "", excl1 = "kasvataja", excl2 = "koristajama", excl3 = "koristaja-", replace = "hoonehaldaja")
new$new = asendaja(new$new, "kalmistuvaht", "", replace = "hoonehaldaja")
new$new = asendaja(new$new, "kodu", "admini", replace = "hoonehaldaja")

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
new$new = asendaja(new$new, "puhastus", "teeni", excl1 = "kliendit", excl2 = "siidi", excl3 = "hooldaja", replace = "puhastusteenindaja")

unique(leidur(new$new, "jurist", ""))
unique(leidur(new$new, "jurist", "abi"))
new$new = asendaja(new$new, "jurist", "abi",  replace = "õigusekeskastmespetsialist")
new$new = asendaja(new$new, "jurist", "assis",  replace = "õigusekeskastmespetsialist")
new$new = asendaja(new$new, "kohtutäitur", "",  replace = "õigusekeskastmespetsialist")
new$new = asendaja(new$new, "väärteom", "",  replace = "õigusekeskastmespetsialist")

new$new = asendaja(new$new, "jurist", "", excl1 = "kohtu", excl2 = "õpetaja", excl3 = "tegev", replace = "jurist")

# tootmisjuhte on tegelikult mitmesuguste koodidega (oleneb valdkonnast)
# varasemalt panin lihtsalt tootmisjuhid "töötleva tööstuse tootmisjuhtideks", kuigi see ei pruugi nii olla, aga ma arvan, et teised tootmisjuhid (vesiviljeluse, kaevanduse, meedia) on pigem vähemuses? Põllumaj ilmselt ikka on
unique(leidur(new$new, "tootmis", "juh",))
unique(leidur(new$new, "tootmisjuh", "",))
new$new = asendaja(new$new, "tootmisjuh", "", excl1 = "assis", excl2 = "looma", excl3 = "omanik", replace = "tööstusetootmisjuht")
new$new = asendaja(new$new, "tootmisjuh", "looma", replace = "põllumajandusetootmisjuht")

#unique(leidur(new$new, "tootmis", "juh",))

# siin ka keeruline liigitada tegelikult - on müügijuhid, kes tegelevad nö turundus-müügiga ja siis on kaubanduse müügijuhid (viimased siis need ostu-müügijuhid?)
#unique(leidur(new$new, "müügijuht", ""))
#new$new = asendaja(new$new, "müügijuht", "", excl1 = "sekretär", excl2 = "proosa", excl3 = "kliendihaldur", excl4 = "ofta", replace = "müügijuht")

#unique(leidur(new$new, "müügi", "juht"))
#new$new = asendaja(new$new, "müügi", "juht", excl1 = "ostu", replace = "müügijuht")

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

# kaupluse juhatajad on ka tegelikult kaubandusjuhtide all koodi mõttes, aga jätsin esialgu eraldi
new$new = asendaja(new$new, "kauplusejuhataja", "", excl1 = "optometrist", excl2 = "dispetšer", replace = "kauplusejuhataja")
unique(leidur(new$new, "poe", "juh"))
new$new = asendaja(new$new, "poe", "juh", excl1 = "müügijuht", excl2 = "teenindaja", replace = "kauplusejuhataja")
unique(leidur(new$new, "kauplus", "juh"))
new$new = asendaja(new$new, "kauplus", "juh", excl1 = "optometrist", excl2 = "dispetšer", excl3 = "kateg", replace = "kauplusejuhataja")
new$new = asendaja(new$new, "aptee", "juh", excl1 = "lao",  replace = "kauplusejuhataja")
new$new = asendaja(new$new, "supermark", "juh",  replace = "kauplusejuhataja")
new$new = asendaja(new$new, "hüperm", "juh",  replace = "kauplusejuhataja")
new$new = asendaja(new$new, "kaubanduskesk", "juh", excl1 = "reklaami",  replace = "kauplusejuhataja")

unique(leidur(new$new, "kategooriajuht", ""))
new$new = asendaja(new$new, "kategooriajuht", "", excl1 = "teenindus", excl2 = "meeskonna", replace = "hankespetsialist")
new$new = asendaja(new$new, "tootejuht", "kauba", replace = "hankespetsialist")

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

unique(leidur(new$new, "arhitekt", "")) #lihtsalt arhitektid ka ilmselt ehitusarhitektid

##nende põllumajandustöötajatega oli palju mõtlemist, vaatan hiljem
#unique(leidur(new$new, "põllutöö", ""))
#new$new = asendaja(new$new, "põllutöö", "", replace = "põllumajandustöötaja")
#new$new = asendaja(new$new, "põllumees", "", replace = "põllumajandustöötaja")
#new$new = asendaja(new$new, "talu", "töö", excl1 = "riist", replace = "põllumajandustöötaja")
#new$new = asendaja(new$new, "põllumajandustöötaja", "", replace = "põllumajandustöötaja")
#new$new = asendaja(new$new, "talunik", "", excl1 = "abikaasa", replace = "põllumajandustöötaja")
#new$new = asendaja(new$new, "talupida", "", excl1 = "turismi", replace = "põllumajandustöötaja")
#new$new = asendaja(new$new, "farmi", "", excl1 = "juh", replace = "põllumajandustöötaja")
#new$new = asendaja(new$new, "lüps", "",replace = "põllumajandustöötaja")
