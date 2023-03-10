
unique(leidur(new$new, "logistik", "juh", excl1 = "abi"))
new$new = asendaja(new$new, "logistik", "juh", excl1 = "abi", replace = "logistikajuht")

unique(leidur(new$new, "logistik", "", excl1 = "abi"))
new$new = asendaja(new$new, "logistik", "", excl1 = "abi", replace = "logistik")

unique(leidur(new$new, "kvaliteed", "juh", excl1 = "assist"))
new$new = asendaja(new$new, "kvaliteedi", "juh", excl1 = "assistent", replace = "kvaliteedijuht")

unique(leidur(new$new, "muusik", "", excl1 = "õpet"))
new$new = asendaja(new$new, "muusik", "erso", replace = "muusik")
new$new = asendaja(new$new, "muusik", "kirik", replace = "muusik")
new$new = asendaja(new$new, "muusik", "orkes", replace = "muusik")
new$new = asendaja(new$new, "muusik", "viiul", replace = "muusik")
new$new = asendaja(new$new, "muusik", "profe", replace = "muusik")
new$new = asendaja(new$new, "muusik", "kutse", replace = "muusik")

unique(leidur(new$new, "režissöör", ""))
new$new = asendaja(new$new, "režissöör", "", excl1 = "assistent", replace = "režissöör")

unique(leidur(new$new, "elektrik", "", excl1 = "kappide"))
new$new = asendaja(new$new, "elektrik", "", excl1 = "kappide", excl2 = "valimistaja", excl3 = "juhatuse", replace = "elektrik")

table(leidur(new$new, "spetsialist", "", "")) %>% sort(decreasing = T) %>% .[1:30]

unique(leidur(new$new, "peaspetsialist", "", excl1 = "it"))
new$new = asendaja(new$new, "peaspetsialist", "", excl1 = "it", replace = "vanemspetsialist")

unique(leidur(new$new, "vanemspetsialist", "", excl1 = "it"))
new$new = asendaja(new$new, "vanemspetsialist", "", excl1 = "it", replace = "vanemspetsialist")

unique(leidur(new$new, "spetsialist", "juhtiv", excl1 = "it"))
new$new = asendaja(new$new, "spetsialist", "juhtiv", excl1 = "it", replace = "vanemspetsialist")

unique(leidur(new$new, "spetsialist", "ostu"))
new$new = asendaja(new$new, "spetsialist", "ostu", replace = "hankepetsialist")
new$new = asendaja(new$new, "spetsialist", "hanke", replace = "hankepetsialist")

unique(leidur(new$new, "spetsialist", "labori"))
new$new = asendaja(new$new, "spetsialist", "labori", replace = "laborispetsialist")

### veel spetsialiste sorteerida teemade järgi. Näiteks lastekaitsjad on nagu sotsiaaltöötajad? It omaette? Koolitus, andmed ..

table(leidur(new$new, "andme", "", "")) %>% sort(decreasing = T) %>% .[1:30]

unique(leidur(new$new, "andme", "töötl", excl1 = "it"))
new$new = asendaja(new$new, "andme", "tööt", excl1 = "juht", replace = "andmetöötleja")

unique(leidur(new$new, "andme", "sisest", excl1 = "it"))
new$new = asendaja(new$new, "andme", "sisest", excl1 = "juht", replace = "andmesisestaja")

unique(leidur(new$new, "andme", "tead"))
new$new = asendaja(new$new, "andme", "tead", excl1 = "juht", replace = "andmeteadlane")

unique(leidur(new$new, "andme", "analüüt"))
new$new = asendaja(new$new, "andme", "analüüt",replace = "andmeanalüütik")

unique(leidur(new$new, "sotsiaal", "tööta", ""))
new$new = asendaja(new$new, "sotsiaal", "töötaja",replace = "sotsiaaltöötaja")
new$new = asendaja(new$new, "sotsiaaltöö", "spetsialist",replace = "sotsiaaltöötaja")
new$new = asendaja(new$new, "laste", "kaitse",replace = "sotsiaaltöötaja") ## läheb?
new$new = asendaja(new$new, "sotsiaalnõunik", "",replace = "sotsiaaltöötaja")

unique(leidur(new$new, "luksepp", "", ""))
new$new = asendaja(new$new, "luksepp", "",replace = "luksepp")
new$new = asendaja(new$new, "lukksepp", "",replace = "luksepp")

unique(leidur(new$new, "auto", "remondi", ""))
new$new = asendaja(new$new, "auto", "remondi",replace = "luksepp")
new$new = asendaja(new$new, "auto", "plekk",replace = "luksepp")
new$new = asendaja(new$new, "auto", "maaler",replace = "luksepp")
new$new = asendaja(new$new, "auto", "klaasi",replace = "luksepp")

unique(leidur(new$new, "operaator", "masina", ""))
new$new = asendaja(new$new, "operaator", "masina",replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "seadme",replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "pingi",replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "pink",replace = "masinaoperaator")
new$new = asendaja(new$new, "operaator", "tehas",replace = "masinaoperaator")

new$new = asendaja(new$new, "autojuht", "kaug",replace = "kaugsõiduautojuht")
new$new = asendaja(new$new, "rekka", "juht",replace = "kaugsõiduautojuht")
new$new = asendaja(new$new, "pikamaa", "juht",replace = "kaugsõiduautojuht")
new$new = asendaja(new$new, "rahvusvahel", "autojuht",replace = "kaugsõiduautojuht")


new$new = asendaja(new$new, "bussi", "juht", excl1 = "õpetaja", replace = "bussijuht")

unique(leidur(new$new, "operaator", "liini", ""))
new$new = asendaja(new$new, "operaator", "liini",replace = "liinioperaator")
