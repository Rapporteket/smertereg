# Ikkje gjer om tekst til faktorar automatisk
options(stringsAsFactors = FALSE)

#' Kodebok til standardformat
#'  
#' Lese inn kodebok og gjøre om til standardformat
#' @param adresse Kodebokens adresse
#' @return "Ny" kodebok
#' 
#' @export 
les_oqr_kb = function(adresse) {
  kodebok_oqr_format = readr::read_delim(
    adresse, 
    delim = ";", quote="\"",
    col_types = readr::cols(
      skjemanavn = readr::col_character(),
      navn_i_rapporteket = readr::col_character(),
      ledetekst = readr::col_character(),
      obligatorisk = readr::col_character(),
      type = readr::col_character(),
      listeverdier = readr::col_character(),
      listetekst = readr::col_character(),
      normalintervall_start_numerisk = readr::col_character(), # Sjå merknad nedanfor om årsaka til denne og dei tre neste må vera tekst
      normalintervall_slutt_numerisk = readr::col_character(),
      maksintervall_start_numerisk = readr::col_character(),
      maksintervall_slutt_numerisk = readr::col_character(),
      normalintervall_start_dato = readr::col_character(),
      normalintervall_slutt_dato = readr::col_character(),
      maksintervall_start_dato = readr::col_character(),
      maksintervall_slutt_dato = readr::col_character(),
      antall_tegn = readr::col_integer(),
      lovlige_tegn = readr::col_character(),
      desimaler = readr::col_integer(),
      aktiveringsspoersmaal = readr::col_character(),
      underspoersmaal = readr::col_character(),
      innfoert_dato = readr::col_character(),
      utfaset_dato = readr::col_character(),
      tabell = readr::col_character(),
      fysisk_feltnavn = readr::col_character(),
      kommentar = readr::col_character(),
      variabel_id = readr::col_character(),
      hjelpetekst = readr::col_character()
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
  kodebok = kodebok_oqr_format %>% 
    dplyr::mutate(skjema_id = tabell,
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


# Les datadump frå OQR-register -------------------------------------------

# Bruk oppgitt kodebok til å henta inn data frå
# OQR-fil slik at variablane får rett format
# (tal, tekst, dato osv.)
# Argument:
#   adresse: adressa til datafila
#        kb: standardisert kodebok
#' Lese datadump
#'  
#' Bruk oppgitt kodebok til å henta inn data fra fil slik at variablene har rett format
#' @param adresse Datafilens adresse
#' @return Datasettet klar til bruk
#' 
#' @export 
les_dd_oqr = function(adresse, kb, datoformat="%Y-%m-%d", dd_kolnamn_er_norsk = TRUE) {
  # Les inn variabelnamna som vert brukt i datafila
  varnamn_fil = scan(adresse, fileEncoding="UTF-8", what = "character",
                     sep=";", nlines = 1, quiet=TRUE)
  
  # Datafila *kan* ikkje innehalda duplikate kolonnenamn,
  # sidan me då ikkje kan veta kva kolonne eit namn svarar til.
  # Stopp derfor viss me finn duplikate namn.
  dupnamn = duplicated(varnamn_fil)
  if(any(dupnamn)) {
    stop("Datafila har duplikate variabelnamn:\n",
         str_c(varnamn_fil[dupnamn], collapse="\n"))
  }
  
  # disse variabelnamna er ikkje dei vi brukar. 
  # henter inn namna som vi faktisk brukar
  # avhengig om variabelnavnene er norske eller engelske
  # i de ulike kodebøkene velger vi å matche mot norske 
  # eller engelske navn for å få inn de vi vil ha
  if(dd_kolnamn_er_norsk){
    dd_kolid = "oqr_variabel_id_norsk"
  } else {
    dd_kolid = "oqr_variabel_id_engelsk"
  }
  varnamn = kb$variabel_id[match(varnamn_fil, kb[[dd_kolid]])] %>%
    coalesce(varnamn_fil)
  
  # Hent ut første linje frå kodeboka, dvs. den linja som
  # inneheld aktuell informasjon
  kb_info = kb %>% distinct(variabel_id, .keep_all = TRUE)
  
  # Forkortingsbokstavane som read_csv() brukar (fixme: utvide med fleire)
  spek_csv_oqr = tribble(
    ~variabeltype, ~csv_bokstav,
    "kategorisk", "n",
    "tekst", "c",
    "boolsk", "c",  
    "dato_kl", "c", 
    "numerisk", "d",
    "dato", "D",
    "kl", "t"
  )
  spek_innlesing = tibble(variabel_id=varnamn) %>% 
    left_join(kb_info, by="variabel_id") %>% 
    left_join(spek_csv_oqr, by="variabeltype")
  
  # Har kodeboka variablar av ein type me ikkje har lagt inn støtte for?
  # Dette skal ikkje skje, så avbryt om så er tilfelle.
  nye_typar = setdiff(kb_info$variabeltype, spek_csv_oqr$variabeltype)
  if(length(nye_typar) > 0) {
    stop("Kodeboka har variablar av ein type me ikkje støttar (legg inn støtte!):\n",
         str_c(nye_typar, collapse="\n"))
  }
  
  # Er det nokon variablar me manglar metadata for (dvs. variablar
  # som finst i datafila men *ikkje* i kodeboka)?
  manglar_metadata = is.na(spek_innlesing$csv_bokstav)
  ukjende_var = spek_innlesing$variabel_id[manglar_metadata]
  if(any(manglar_metadata)) {
    warning("Manglar metadata for nokre variablar. Dei vert derfor\n",
            "handterte som tekst og variabelnamna vert gjorde om til\n",
            "små bokstavar og får prefikset «oqr_».\n",
            "Problematiske variablar:\n",
            str_c(ukjende_var, collapse="\n"))
    spek_innlesing$csv_bokstav[manglar_metadata] = "c"
    spek_innlesing$variabel_id[manglar_metadata] = str_to_lower(str_c("oqr_", spek_innlesing$variabel_id[manglar_metadata]))
  }
  
  # Les inn datasettet
  kol_typar = str_c(spek_innlesing$csv_bokstav, collapse="")
  d = read_delim(adresse, 
                 delim = ";", quote = "\"", trim_ws = FALSE, na = "null",
                 col_names = spek_innlesing$variabel_id, col_types = kol_typar, skip = 1, # Hopp over overskriftsrada
                 locale = locale(decimal_mark = ",", grouping_mark="",
                                 date_format=datoformat, time_format="%H:%M:%S"))
  
  # Gjer om boolske variablar til ekte boolske variablar
  oqr_boolsk_til_boolsk = function(x) {
    # Sjekk først at det berre er gyldige verdiar
    er_gyldig = (x %in% 0:1) | is.na(x)
    if(!all(er_gyldig)) {
      stop("Finst ugyldige verdiar i boolsk variablar (skal vera 0, 1 eller NA)")
    } else
    {
      x == 1 # Gjer om til boolsk variabel
    }
  }
  boolsk_ind = which(spek_innlesing$variabeltype == "boolsk")
  d[, boolsk_ind] = lapply(d[, boolsk_ind], oqr_boolsk_til_boolsk)
  
  # Gjer om tidsvariablar til ekte tidsvariablar
  dt_ind = which(spek_innlesing$variabeltype == "dato_kl")
  d[, dt_ind] = lapply(d[, dt_ind], parse_datetime, format = "%Y-%m-%d %H:%M:%S")
  # Returner datasettet
  d
}