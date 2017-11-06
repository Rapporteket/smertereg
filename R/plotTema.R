#' Endre utseende på plot og figurer 
#'  
#' Funksjon for å endre standardinnstillinger i ggplot2 til format som er tilpasset rapportene: fjerner rutenett
#' @param fjern_x Tema som legges inn i plotfunksjonen i R
#' @return Funksjoner som realiserer dette (fjern_x, fjern_y)
#' 
#' @export 

colPrim=c("#000059", "#084594", "#2171b5", "#4292c6", "#6baed6", "#c6dbef") # Primærfarge (mørk til lys)
colNoyt=c("#4D4D4D", "#737373", "#A6A6A6", "#DADADA") # Nøytralfarge
colKontr="#FF7260"                                    # Kontrastfarge
# grafobjekter ----------------------------------------------------------
# de offisielle fargene
# ggplot2-tema for figurar
if(!exists("skriftstorleik")) # Skriftstorleik bør vera definert i kvar årsrapportfil
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
