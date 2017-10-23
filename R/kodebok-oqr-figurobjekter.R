#' Funksjon for å figurer og kodebok riktig i rapport SmerteReg.
#' 
#'Funksjon for å lese inn kodebok og å gjøre den om til standardisert format (i tillegg til funksjon som bruker ggplot2 som også ligger her).
#' @param adresse <Adresse der kodebok kan hentes fra.
#' @return En standardisert kodebok.
#'
#' @export


# Lesing/tolking av det flotte kodebokformatet til OQR


# Oppsett -----------------------------------------------------------------

# Ikkje gjer om tekst til faktorar automatisk
options(stringsAsFactors = FALSE)

# Nødvendige pakkar
#library(tidyverse) # Ymse standardpakkar
#library(stringr)   # Tekstmassering
#library(magrittr)  # Funksjonar som kan brukast med røyr-operatoren
#library(readr)     # For innlesing av CSV-filer

# grafobjekter ----------------------------------------------------------

colPrim=c("#000059", "#084594", "#2171b5", "#4292c6", "#6baed6", "#c6dbef") # Primærfarge (mørk til lys)
colNoyt=c("#4D4D4D", "#737373", "#A6A6A6", "#DADADA") # Nøytralfarge
colKontr="#FF7260"                                    # Kontrastfarge

# ggplot2-tema for figurar
if(!exists("skriftstorleik")) 
  skriftstorleik = 13
tema = theme_light(base_size=skriftstorleik)
tema$panel.grid.minor$colour="white"
tema$strip.background$fill="#f3f1ee"
tema$strip.background$colour="#e4e0da"
tema$strip.text.x$colour="black"
tema$panel.spacing=unit("13" ,"pt")
tema$panel.border$colour=tema$strip.background$colour
tema$panel.grid.major$colour=tema$strip.background$colour
tema$panel.grid.minor$colour=tema$strip.background$fill
tema$axis.title.y$angle=0
tema$axis.title.y$margin=margin(r=5)
tema$axis.title.x$margin=margin(t=5)

# Fjern vannrette eller loddrette rutenett
fjern_x = theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank())
fjern_y = theme(panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank())

# Les inn kodebok og gjer om til standardformat ---------------------------

# Les inn kodebok
les_oqr_kb = function(adresse) {
  kodebok_oqr_format = read_delim(
    adresse, 
    delim = ";", quote="\"",
    col_types = cols(
      skjemanavn = col_character(),
      navn_i_rapporteket = col_character(),
      ledetekst = col_character(),
      obligatorisk = col_character(),
      type = col_character(),
      listeverdier = col_character(),
      listetekst = col_character(),
      normalintervall_start_numerisk = col_character(), # Sjå merknad nedanfor om årsaka til denne og dei tre neste må vera tekst
      normalintervall_slutt_numerisk = col_character(),
      maksintervall_start_numerisk = col_character(),
      maksintervall_slutt_numerisk = col_character(),
      normalintervall_start_dato = col_character(),
      normalintervall_slutt_dato = col_character(),
      maksintervall_start_dato = col_character(),
      maksintervall_slutt_dato = col_character(),
      antall_tegn = col_integer(),
      lovlige_tegn = col_character(),
      desimaler = col_integer(),
      aktiveringsspoersmaal = col_character(),
      underspoersmaal = col_character(),
      innfoert_dato = col_character(),
      utfaset_dato = col_character(),
      tabell = col_character(),
      fysisk_feltnavn = col_character(),
      kommentar = col_character(),
      variabel_id = col_character(),
      hjelpetekst = col_character()
    ))
  
  # Dei numeriske min- og maksverdiane kan ifølgje dokumentasjonen
  # http://helseregister.no/confluence/display/KG/Klokeboken
  # òg vera tekst som referer til andre felt, eks. «birthYear»
  # eller «todayYear». Dei kan me ikkje bruka, så me fjernar
  # dei rett og slett. Dette gjer me lettast ved å prøva
  # å gjera tekst om til tal med as.numeric(), som gjev ut
  # NA for alt som ikkje ser ut som tal.
  tekst_til_tal = function(x) {
    suppressWarnings(as.numeric(x))
  }
  
  # Gjer om kodeboka til vårt *standardiserte* format
  # (Det finst ikkje heilt ei 1-til-1-kopling, men me
  #  gjer so godt me kan, og set verdiar til NA der
  #  det ikkje finst nokon tilsvarande.)
  #
  # fixme: Vårt kodebokformat bør nok oppdaterast til 
  #        å støtta min- og maks-verdiar for datoar òg.
  kodebok = kodebok_oqr_format %>% 
    mutate(skjema_id = tabell,
           skjemanamn = skjemanavn,
           oqr_variabel_id_norsk = navn_i_rapporteket,
           oqr_variabel_id_engelsk = variabel_id,
           variabeletikett = ledetekst,
           forklaring = hjelpetekst,
           variabeltype = type,
           verdi = listeverdier,
           verditekst = listetekst,
           desimalar = desimaler,
           min = tekst_til_tal(maksintervall_start_numerisk),
           maks = tekst_til_tal(maksintervall_slutt_numerisk),
           min_rimeleg = tekst_til_tal(normalintervall_start_numerisk),
           maks_rimeleg = tekst_til_tal(normalintervall_slutt_numerisk),
           kommentar = kommentar,
           kategori = NA_character_,
           innleiing = NA_character_,
           eining = NA_character_,
           unik = NA_character_,
           manglande = NA_character_,
           kommentar_rimeleg = NA_character_,
           utrekningsformel = NA_character_,
           logikk = NA_character_,
           obligatorisk = str_to_lower(obligatorisk),
           variabel_id = str_to_lower(variabel_id))
  
  # Ein «Statusvariabel» er eigentleg ein kategorisk variabel
  # som kan ta tre verdiar, -1, 0 og 1 (oppretta, lagra og ferdigstilt).
  # Me gjer derfor kvar statusvariabel om til ein kategorisk variabel.
  # Kodeboka må utvidast med nye rader, og me gjer det iterativt,
  # éin gong for kvar statusvariabel (i teorien litt tregt/suboptimalt,
  # men i praksis kjemperaskt, sidan me ikkje har kodebøker med tusenvis
  # av statusvariablar, berre maks éin per skjema).
  while(any(kodebok$variabeltype == "Statusvariabel")) {
    # Radnummeret til første (ubehandla) statusvariabel
    ind = which(kodebok$variabeltype == "Statusvariabel")[1]
    
    # Rada må bytast ut med tre rader, éi for kvar moglege verdi.
    # Dette gjer me først ved å utvida kodeboka med to ekstra,
    # identiske rader rett etter rada. Så set me inn rette verdiar.
    #
    # Gjenta aktuell rad tre gongar (når me gjer det slik,
    # funkar det òg viss rada er første eller siste rad).
    kodebok = kodebok[append(seq_len(nrow(kodebok)),
                             values = c(ind, ind), after = ind),]
    
    # Legg rette verdiar inn i dei tre nye radene
    nyind = c(ind, ind + 1, ind + 2)
    kodebok$verdi[nyind] = -1:1
    kodebok$verditekst[nyind] = c("Opprettet", "Lagret", "Ferdigstilt")
    kodebok$variabeltype[nyind] = "Listevariabel"
  }
  
  # Oversikt over variabeltypar i OQR og tilhøyrande standardnamn som me brukar
  vartype_oqr_standard = tribble(
    ~type_oqr, ~type_standard,
    "Listevariabel", "kategorisk",
    "Tekstvariabel", "tekst",
    "Stor tekstvariabel", "tekst",
    "Avkrysningsboks", "boolsk",
    "Datovariabel", "dato",
    "Skjult variabel", "tekst",
    "Tallvariabel", "numerisk",
    "Tidsvariabel", "kl",
    "TIMESTAMP", "dato_kl"
  )
  
  # Stopp viss det dukkar opp variabeltypar me ikkje kjenner til
  nye_vartypar = na.omit(setdiff(kodebok$variabeltype, vartype_oqr_standard$type_oqr))
  if(length(nye_vartypar) > 0) {
    stop("Kodeboka har variabeltypar me ikkje har standardnamn på: ",
         str_c(nye_vartypar, collapse=", "))
  }
  
  # Byt ut variabeltype-verdiane med våre standardiserte namn
  kodebok$variabeltype = vartype_oqr_standard$type_standard[
    match(kodebok$variabeltype, vartype_oqr_standard$type_oqr)]
  
  # Dei variabelnamna me brukar, og i ei fornuftig rekkjefølgje
  std_namn = c("skjema_id", "skjemanamn", "kategori", "innleiing", "variabel_id", 
               "variabeletikett", "forklaring", "variabeltype", "eining", "unik", 
               "obligatorisk", "verdi", "verditekst", "manglande", "desimalar", 
               "min", "maks", "min_rimeleg", "maks_rimeleg", "kommentar_rimeleg", 
               "utrekningsformel", "logikk", "kommentar")
  
  # Treng òg nokre ekstranamn som me brukar (berre) for å lesa inn datafiler
  ekstra_namn = c("oqr_variabel_id_engelsk", "oqr_variabel_id_norsk")
  
  # Ta med dei namna me brukar, i fornuftig rekkjefølgje
  kodebok = kodebok %>% select_(.dots=c(std_namn, ekstra_namn))
  
  # Returner kodeboka 
  kodebok
}



# Eksempel  -----------------------------------------------------------

# # Les inn eksempeldata

# # Les inn kodeboka
# adresse_kb = "\\\\ihelse.net\\Kvalitetsregister\\HBE\\2014-00752\\OpenQReg\\kodebok\\SmerteReg_klokeboken.csv_24.04.2017.csv"
# kb = les_oqr_kb(adresse_kb)

