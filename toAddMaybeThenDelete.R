
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

### veel spetsialiste sorteerida teemade järgi. Näiteks lastekaitsjad on nagu sotsiaaltöötajad? It omaette? Koolitus, andmed ..

table(leidur(new$new, "andme", "", "")) %>% sort(decreasing = T) %>% .[1:30]

unique(leidur(new$new, "andme", "töötl", excl1 = "it"))
new$new = asendaja(new$new, "andme", "tööt", excl1 = "juht", replace = "andmetöötleja")

unique(leidur(new$new, "andme", "sisest", excl1 = "it"))
new$new = asendaja(new$new, "andme", "sisest", excl1 = "juht", replace = "andmesisestaja")

