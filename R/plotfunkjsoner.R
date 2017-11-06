#' Aritmetisk tallfølge
#'  
#' Funksjon som lager funksjon som tar inn to tall
#' og lager aritmetisk tallfølge med valgfri
#' intervallbredde slik at alle tall i følgen er
#' multiplum av intervallbredden og de to tallene
#' er innenfor range() av følgen.
#' 
#' @param bredde Ønsket bredde på intervallet
#' @return Et intervall med valgt bredde som oppfyller kravene til bruk i f.eks ggplot2
#' 
#' @export 
breaks_bredde = function(bredde = 5, min = NULL, maks = NULL) {
  function(lims) {
    lims = c(max(min, lims[1]), min(maks, lims[2]))
    seq(round_any(lims[1], bredde, floor),
        round_any(lims[2], bredde, ceiling),
        by = bredde)
  }
}

#' Flytte på labels inni graf
#'  
#' Funksjon som lager funksjon som tar inn to tall
#' og lager aritmetisk tallfølge med valgfri
#' intervallbredde slik at alle tall i følgen er
#' multiplum av intervallbredden og de to tallene
#' er innenfor range() av følgen.
#' 
#' @param y  y-koordinat til (midten av) teksten
#' @return En koordinat med valgt bredde som oppfyller kravene til bruk i f.eks ggplot2
#' 
#' @export 
flytt_opp = function(y, tekst, hoyde = .015) {
  tekst_ny = tekst[order(y)]
  y = y[order(y)]
  linjer = tekst_ny %>% str_split("\n") %>% sapply(length)
  nedre = y - linjer * hoyde / 2
  ovre = y + linjer * hoyde / 2
  for (i in 2:length(y)) {
    avs = nedre[i] - ovre[i - 1]
    if (avs < 0) {
      y[i] = y[i] - avs
      ovre[i] = ovre[i] - avs # Nedre treng me ikkje endra, sidan me ikkje brukar han
    }
  }
  y[match(tekst, tekst_ny)]
}


#'  Lag linjegraf med 95 % konfidensintervall
#'  
#' Funksjon som lager 95% konf.int.
#' 
#' @param refline  y-koordinat til vannrett referanselinje
#' @return En linjegraf
#' 
#' @export 
graf_linje = function(refline=NULL, refline_df=NULL, xlab="\uc5r", ylab=NULL,
                      angle=TRUE, konfint=TRUE) {
  grafdel = list()
  # Legg ev. til referanselinje(r)
  if(!is.null(refline)) {
    if(is.null(refline_df)) {
      grafdel = append(grafdel, list(geom_hline(yintercept=refline, col=colPrim[6], size=2)))
    } else {
      grafdel = append(grafdel, list(geom_hline(data=refline_df,
                                                mapping=aes_string(yintercept=refline),
                                                col=colPrim[6], size=2)))
    }
  }
  # Legg ev. til konfidensintervall (bak alt anna)
  if(konfint) {
    grafdel = append(grafdel, geom_linerange(size=.5, colour=colPrim[5]))
  }
  # Legg til resten
  grafdel = append(
    grafdel,
    list(
      geom_line(colour=colPrim[3], size = 1),     # Linjer over tid
      geom_point(size=2, colour=colPrim[2]),      # Punkt
      xlab(xlab),
      ylab(ylab),
      tema,
      fjern_x
    )
  )
  if(angle) {
    grafdel = append(grafdel, list(theme(axis.text.x  = 
                                           element_text(angle = 45, vjust = 0.5))))
  }
  grafdel
}

#' Normaliser variabelnamn til å ha _ som skiljeteikn og berre små bokstavar
#'  
#' Normaliser variabelnavnene i kodeboken
#' 
#' @param x  et navn som skal normaliseres
#' @return Riktig type navn
#' 
#' @export 
normaliser_varnamn = function(x) {
  teikn = x %>% 
    str_split("") # Splitt i enkeltteikn
  
  # Putt inn _ før alle store bokstavar (utanom første teikn i strengen)
  teikn = teikn %>% map(~str_replace_all(., "([[:upper:]])", "_\\1"))
  
  teikn %>% map_chr(~paste0(., collapse="")) %>%  # Slå saman til lange strengar igjen
    str_replace_all("[\\._ ]+", "_") %>%          # Erstatt etterfølgjande punktum, mellomrom og/eller _ med éin _,
    str_replace_all("^_", "") %>%                 # Fjern ev. _ på starten av strengane
    tolower                                       # Gjer om til små bokstavar
}