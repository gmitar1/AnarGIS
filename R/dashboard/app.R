library('shiny')
library('RSQLite')
library('DBI')
library('ggplot2')

#### FUNKCIJE ####

ucitavanje_baze <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), 'podaci.sqlite')
  
  tabele <- list()
  tabele$`klima_stanice` <- DBI::dbReadTable(con, 'klima_stanice')
  tabele$`klima_merenja` <- DBI::dbReadTable(con, 'klima_merenja_dnevno')
  tabele$`klima_stanice_mesecno` <- DBI::dbReadTable(con, 'klima_stanice_mesecno')
  tabele$`klima_merenja_mesecno` <- DBI::dbReadTable(con, 'klima_merenja_mesecno')
  tabele$`kvazduh_stanice` <- DBI::dbReadTable(con, 'kvazduh_stanice')
  tabele$`kvazduh_merenja` <- DBI::dbReadTable(con, 'kvazduh_merenja_dnevno')
  tabele$`pdvode_stanice` <- DBI::dbReadTable(con, 'pdvode_stanice')
  tabele$`pdvode_tela` <- DBI::dbReadTable(con, 'pdvode_tela')
  tabele$`pdvode_merenja` <- DBI::dbReadTable(con, 'pdvode_merenja_dnevno')
  tabele$`pvode_stanice` <- DBI::dbReadTable(con, 'pvode_stanice')
  tabele$`pvode_reke` <- DBI::dbReadTable(con, 'pvode_reke')
  tabele$`pvode_merenja` <- DBI::dbReadTable(con, 'pvode_merenja_dnevno')
  tabele$`dashboard_mapa_gradovi` <- DBI::dbReadTable(con, 'dashboard_mapa_gradovi')
  
  DBI::dbDisconnect(con)
  
  tabele <- konverzija_kolona_u_numericki_tip(tabele)
  
  return(tabele)
}

konverzija_kolona_u_numericki_tip <- function(tabele) {
  numericke_kolone <- list(
    klima_merenja = c('temperatura_max', 'temperatura_min', 'pritisak_sr', 'relativna_vlaznost_sr', 'napon_vodene_pare_sr', 'brzina_vetra_sr', 'oblacnost_sr', 'insolacija', 'padavine', 'sneg_novo')
    ,klima_merenja_mesecno = c('temperatura_max', 'temperatura_min', 'pritisak_sr', 'relativna_vlaznost_sr', 'napon_vodene_pare_sr', 'brzina_vetra_sr', 'oblacnost_sr', 'insolacija', 'padavine_suma', 'sneg_novo')
    ,kvazduh_merenja = c('so2', 'no2', 'o3', 'co', 'pm10', 'pm25')
    ,pvode_merenja = c('nivo', 'protok', 'temperatura')
    ,pdvode_merenja = c('nivo', 'temperatura')
  )
  
  for (naziv in names(numericke_kolone)) {
    if (naziv %in% names(tabele)) {
      for (col in numericke_kolone[[naziv]]) {
        if (col %in% names(tabele[[naziv]])) {
          tabele[[naziv]][[col]] <- suppressWarnings(
            as.numeric(tabele[[naziv]][[col]])
          )
        }
      }
    }
  }
  return(tabele)
}

pronadji_najblizi_id_stanice <- function(koordinate, tabela) {
  najblizi_idjevi <- which.min((tabela$`geografska_sirina` - koordinate['geografska_sirina'])^2 + (tabela$`geografska_duzina` - koordinate['geografska_duzina'])^2)
  id <- tabela$`id`[najblizi_idjevi]
  
  return(id)
}

dodaj_stanice <- function(koordinate, tabele) {
  stanice <- list()
  
  stanice$`klima` <- pronadji_najblizi_id_stanice(koordinate, tabele$`klima_stanice`)
  stanice$`klima_mesecno` <- pronadji_najblizi_id_stanice(koordinate, tabele$`klima_stanice_mesecno`)
  stanice$`pvode` <- pronadji_najblizi_id_stanice(koordinate, tabele$`pvode_stanice`)
  stanice$`pdvode` <- pronadji_najblizi_id_stanice(koordinate, tabele$`pdvode_stanice`)
  stanice$`kvazduh` <- pronadji_najblizi_id_stanice(koordinate, tabele$`kvazduh_stanice`)
  
  return(stanice)
}

filtriraj_tabele_prema_stanici <- function(stanice, tabele) {

  tabele$`klima_stanice` <- tabele$`klima_stanice`[tabele$`klima_stanice`$`id` == stanice[['klima']],]
  tabele$`klima_merenja` <- tabele$`klima_merenja`[tabele$`klima_merenja`$`id_stanice` == stanice[['klima']],]
  
  tabele$`klima_stanice_mesecno` <- tabele$`klima_stanice_mesecno`[tabele$`klima_stanice_mesecno`$`id` == stanice[['klima_mesecno']],]
  tabele$`klima_merenja_mesecno` <- tabele$`klima_merenja_mesecno`[tabele$`klima_merenja_mesecno`$`id_stanice` == stanice[['klima_mesecno']],]
  
  tabele$`kvazduh_stanice` <- tabele$`kvazduh_stanice`[tabele$`kvazduh_stanice`$`id` == stanice[['kvazduh']],]
  tabele$`kvazduh_merenja` <- tabele$`kvazduh_merenja`[tabele$`kvazduh_merenja`$`id_stanice` == stanice[['kvazduh']],]
  
  tabele$`pdvode_stanice` <- tabele$`pdvode_stanice`[tabele$`pdvode_stanice`$`id` == stanice[['pdvode']],]
  tabele$`pdvode_merenja` <- tabele$`pdvode_merenja`[tabele$`pdvode_merenja`$`id_stanice` == stanice[['pdvode']],]
  
  tabele$`pvode_stanice` <- tabele$`pvode_stanice`[tabele$`pvode_stanice`$`id` == stanice[['pvode']],]
  tabele$`pvode_merenja` <- tabele$`pvode_merenja`[tabele$`pvode_merenja`$`id_stanice` == stanice[['pvode']],]
  
  return(tabele)
}

napravi_grafikon <- function(tabela, metrika, naslov = '???', jedinica = '?') {
  ggplot2::ggplot(tabela, aes(x = as.Date(datum), y = {{metrika}}, color = {{metrika}})) + 
    geom_line(linewidth = 0.2, alpha = 0.9) +
    scale_colour_gradient(low = '#A0F549', high = '#CF3E63') +
    scale_x_date(breaks = '1 year', date_labels = '%Y', limits = as.Date(c('2000-01-01', '2024-12-31'))) +
    ggtitle(naslov) +
    ylab(jedinica) +
    xlab(element_blank()) +
    tema
}

tema <- theme(
  plot.background = element_rect(fill = '#333333', color = NA),
  panel.background = element_rect(fill = '#333333'),
  panel.border = element_rect(color = '#87cf3e', fill = NA, linewidth = 1),
  plot.title = element_text(color = '#87cf3e', size = 16, hjust = 0, face = 'bold'),
  axis.title = element_text(color = '#87cf3e', size = 12),
  axis.text = element_text(color = '#87cf3e', size = 12),
  axis.line = element_line(color = '#345018'),  # Mint green
  axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
  panel.grid.major = element_line(color = '#345018', linewidth = 0.15),
  panel.grid.minor = element_blank(),
  legend.position='none',
  text = element_text(family = 'mono')
)

postavi_automatsko_gasenje_sesije <- function(session, input, dozvoljeno_vreme_minuta = 5) {
  dozvoljeno_vreme_sekunde <- dozvoljeno_vreme_minuta * 60
  
  vreme_poslednje_aktivnosti <- reactiveVal(Sys.time())
  
  observeEvent(input$map_click, {vreme_poslednje_aktivnosti(Sys.time())}, ignoreNULL = FALSE)
  
  observeEvent(input$update, {vreme_poslednje_aktivnosti(Sys.time())})
  
  observe({
    invalidateLater(30000)  # Proveri svakih 30 sekundi
    vreme_trajanja_neaktivnosti <- difftime(Sys.time(), vreme_poslednje_aktivnosti(), units = 'secs'    )
    if (as.numeric(vreme_trajanja_neaktivnosti) >= dozvoljeno_vreme_sekunde) {
      session$close()
    }
  })
  
  return(vreme_poslednje_aktivnosti)
}

#### RSHINY APLIKACIJA ####

#### FRONTEND ####

ui <- fluidPage(
  
  # CSS STILOVI
  includeCSS('stilovi.css')
  
  #ELEMENTI
  ,div(class = 'zaglavlje-aplikacije', h1('AnarGIS Dashboard', class = 'naslov-aplikacije'))
  
  ,div(
    class = 'kontejner-dashboard',
    div(
      class = 'panel-mapa'
      ,fluidRow(column(width = 12, h4('Izaberite lokaciju na mapi', class = 'naslov-panela')))
      ,fluidRow(
        column(width = 4, div(class = 'kontejner-mapa', div(class = 'kontejner-mapa-unutrasnjost', plotOutput('serbia_map', click = 'map_click', width = '100%', height = '100%')))),
        column(width = 4, div(class = 'kartica-koordinate kartica-tekst-centriran', h5('Izabrane koordinate:'), verbatimTextOutput('izabrane_koordinate')), div(class = 'kontejner-dugme', actionButton('update', 'Prikaži podatke', class = 'dugme')))
      )
    )
    ,div(class = 'zaglavlje-sekcije', div(class = 'naslov-sekcije', 'DNEVNI METEOROLOŠKI PODACI'), uiOutput('klima_stanica_info', class = 'stanica-info'))
    ,div(class = 'kontejner-grafikon', plotOutput('temperatura_max', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('temperatura_min', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('pritisak_sr', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('relativna_vlaznost_sr', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('napon_vodene_pare_sr', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('oblacnost_sr', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('brzina_vetra_sr', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('insolacija', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('padavine', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('sneg_novo', height = '200px'))
    
    ,div(class = 'zaglavlje-sekcije', div(class = 'naslov-sekcije', 'MESEČNI METEOROLOŠKI PODACI'), uiOutput('klima_mesecno_stanica_info', class = 'stanica-info'))
    ,div(class = 'kontejner-grafikon', plotOutput('temperatura_max2', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('temperatura_min2', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('pritisak_sr2', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('relativna_vlaznost_sr2', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('napon_vodene_pare_sr2', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('oblacnost_sr2', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('brzina_vetra_sr2', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('insolacija2', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('padavine2', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('sneg_novo2', height = '200px'))
    
    ,div(class = 'zaglavlje-sekcije zaglavlje-sekcije-kvazduh', div(class = 'naslov-sekcije', 'KVALITET VAZDUHA'), uiOutput('kvazduh_stanica_info', class = 'stanica-info'))
    ,div(class = 'kontejner-grafikon', plotOutput('so2', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('no2', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('o3', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('co', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('pm10', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('pm25', height = '200px'))
    
    ,div(class = 'zaglavlje-sekcije zaglavlje-sekcije-pvode', div(class = 'naslov-sekcije', 'POVRŠINSKE VODE'), uiOutput('pvode_stanica_info', class = 'stanica-info'))
    ,div(class = 'kontejner-grafikon', plotOutput('pvode_nivo', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('pvode_protok', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('pvode_temperatura', height = '200px'))
    
    ,div(class = 'zaglavlje-sekcije zaglavlje-sekcije-pdvode', div(class = 'naslov-sekcije', 'PODZEMNE VODE'), uiOutput('pdvode_stanica_info', class = 'stanica-info'))
    ,div(class = 'kontejner-grafikon', plotOutput('pdvode_nivo', height = '200px'))
    ,div(class = 'kontejner-grafikon', plotOutput('pdvode_temperatura', height = '200px'))
  )
)

#### BACKEND ####

server <- function(input, output, session) {
  
  postavi_automatsko_gasenje_sesije(session, input, dozvoljeno_vreme_minuta = 5)
  
  tabele <- ucitavanje_baze()
  
  # Sekcija sa mapom
  
  pocetne_koordinate <- c(geografska_sirina = 43.6, geografska_duzina = 20.9)
  granice_mape <- c(xmin = 18.5, xmax = 23.5, ymin = 42, ymax = 46.5)
  
  izabrane_koordinate <- reactiveValues(geografska_sirina = pocetne_koordinate[['geografska_sirina']], geografska_duzina = pocetne_koordinate[['geografska_duzina']])
  
  output$serbia_map <- renderPlot({
    koeficijent_skaliranja <- cos(mean(c(granice_mape[['ymin']], granice_mape[['ymax']])) * pi / 180)
    
    par(bg = '#222222', fg = '#A0F549', col.axis = '#A0F549', col.lab = '#A0F549', col.main = '#A0F549', mar = c(4, 4, 2, 1), family = 'mono', xaxs = 'i', yaxs = 'i')
    plot(
        NA, 
        ,xlim = c(granice_mape[['xmin']], granice_mape[['xmax']])
        ,ylim = c(granice_mape[['ymin']], granice_mape[['ymax']])
        ,xlab = 'Geografska dužina'
        ,ylab = 'Geografska širina'
        ,axes = TRUE
        ,frame.plot = TRUE
        ,main = 'Kliknite na mapu za izbor lokacije'
        ,asp = 1 / koeficijent_skaliranja # odnos x i y ose kako bi mapa ličina na pravu projekciju
        )
    grid(col = '#444444', lty = 'dotted')
    points(izabrane_koordinate$geografska_duzina, izabrane_koordinate$geografska_sirina, pch = 19, col = '#A0F549', cex = 2, lwd = 2)
    box(col = '#87cf3e', lwd = 2)
    gradovi <- tabele$dashboard_mapa_gradovi
    points(gradovi$geografska_duzina, gradovi$geografska_sirina, pch = 4, col = '#CF3E63', cex = 1)
    text(gradovi$geografska_duzina + 0.2, gradovi$geografska_sirina + 0.01, gradovi$naziv, col = '#CF3E63', cex = 0.8, pos = 3)
  })
  
  observeEvent(input$map_click, {
    izabrane_koordinate$geografska_sirina <- input$map_click$y
    izabrane_koordinate$geografska_duzina <- input$map_click$x
  })
  
  output$izabrane_koordinate <- renderText({
    paste0('Geografska širina: ', round(izabrane_koordinate$geografska_sirina, 4), '\nGeografska dužina: ', round(izabrane_koordinate$geografska_duzina, 4))
  })
  
  tabele_reaktivne <- reactiveValues()
  
  observeEvent(input$update, {
    koordinate <- c(geografska_sirina = izabrane_koordinate$geografska_sirina, geografska_duzina = izabrane_koordinate$geografska_duzina)
    stanice <- dodaj_stanice(koordinate, tabele)
    tabele_reaktivne$filtrirano <- filtriraj_tabele_prema_stanici(stanice, tabele)
  })
  
  observe({
    koordinate <- c(geografska_sirina = pocetne_koordinate[['geografska_sirina']], geografska_duzina = pocetne_koordinate[['geografska_duzina']])
    stanice <- dodaj_stanice(koordinate, tabele)
    tabele_reaktivne$filtrirano <- filtriraj_tabele_prema_stanici(stanice, tabele)
  })

  #### NASLOVI SEKCIJA ZA GRAFIKONE  
  
  # DNEVNA METEOROLOGIJA
  output$klima_stanica_info <- renderUI({
    req(tabele_reaktivne$filtrirano)
    HTML(paste0("Najbliža stanica: <span style='color:#A0F549; font-weight:bold;'>", tabele_reaktivne$filtrirano$klima_stanice$naziv[1], '</span>'))
  })
  
  # MESEČNA METEOROLOGIJA
  output$klima_mesecno_stanica_info <- renderUI({
    req(tabele_reaktivne$filtrirano)
    HTML(paste0("Najbliža stanica: <span style='color:#A0F549; font-weight:bold;'>", tabele_reaktivne$filtrirano$klima_stanice_mesecno$naziv[1], '</span>'))
  })
  
  # KVAZDUH
  output$kvazduh_stanica_info <- renderUI({
    req(tabele_reaktivne$filtrirano)
    HTML(paste0("Najbliža stanica: <span style='color:#A0F549; font-weight:bold;'>", tabele_reaktivne$filtrirano$kvazduh_stanice$naziv[1], '</span>'))
  })
  
  # PVODE
  output$pvode_stanica_info <- renderUI({
    req(tabele_reaktivne$filtrirano)
    
    pvode_stanice_naziv <- tabele_reaktivne$filtrirano$pvode_stanice$naziv[1]
    id_reke <- tabele_reaktivne$filtrirano$pvode_stanice$id_reke[1]
    reka_naziv <- tabele$pvode_reke[tabele$pvode_reke$id == id_reke, ]$naziv[1]
    
    HTML(paste0("Najbliža stanica: <span style='color:#A0F549; font-weight:bold;'>", paste0(pvode_stanice_naziv, ' (', reka_naziv, ')'), '</span>'))
  })
  
  # PDVODE
  output$pdvode_stanica_info <- renderUI({
    req(tabele_reaktivne$filtrirano)
    
    pdvode_stanice_naziv <- tabele_reaktivne$filtrirano$pdvode_stanice$naziv[1]
    id_tela <- tabele_reaktivne$filtrirano$pdvode_stanice$id_tela[1]
    telo_naziv <- tabele$pdvode_tela[tabele$pdvode_tela$id == id_tela, ]$naziv[1]
    
    HTML(paste0("Najbliža stanica: <span style='color:#A0F549; font-weight:bold;'>", paste0(pdvode_stanice_naziv, ' (', telo_naziv, ')'), '</span>'))
  })    
  
  # GRAFIKONI
  # SEKCIJA DNEVNI METEOROLOŠKI PODACI
  output$temperatura_max <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja, temperatura_max, 'Maksimalna dnevna temperatura', '°C')
  })
  
  output$temperatura_min <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja, temperatura_min, 'Minimalna dnevna temperatura', '°C')
  })
  
  output$pritisak_sr <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja, pritisak_sr, 'Srednji dnevni atmosferski pritisak', 'mb')
  })
  
  output$relativna_vlaznost_sr <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja, relativna_vlaznost_sr, 'Srednja dnevna relativna vlažnost', '%')
  })  
  
  output$napon_vodene_pare_sr <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja, napon_vodene_pare_sr, 'Srednji dnevni napon vodene pare', 'mb')
  })  
  
  output$insolacija <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja, insolacija, 'Insolacija', 'h')
  })    
  
  output$oblacnost_sr <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja, oblacnost_sr, 'Srednja dnevna oblačnost', 'desetina')
  })    
  
  output$brzina_vetra_sr <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja, brzina_vetra_sr, 'Srenja dnevna brzina vetra', 'm/s')
  })    
  
  output$padavine <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja, padavine, 'Ukupna dnevna količina padavine', 'mm')
  })
  
  output$sneg_novo <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja, sneg_novo, 'Ukupna dnevna količina novog snega', 'cm')
  })  
  
  # SEKCIJA MESEČNI METEOROLOŠKI PODACI
  output$temperatura_max2 <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja_mesecno, temperatura_max, 'Srednja mesečna maksimalna temperatura', '°C')
  })
  
  output$temperatura_min2 <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja_mesecno, temperatura_min, 'Srednja mesečna minimalna temperatura', '°C')
  })
  
  output$pritisak_sr2 <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja_mesecno, pritisak_sr, 'Srednji mesečni atmosferski pritisak', 'mb')
  })
  
  output$relativna_vlaznost_sr2 <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja_mesecno, relativna_vlaznost_sr, 'Srednja mesečna relativna vlažnost', '%')
  })  
  
  output$napon_vodene_pare_sr2 <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja_mesecno, napon_vodene_pare_sr, 'Srednji mesečni napon vodene pare', 'mb')
  })  
  
  output$insolacija2 <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja_mesecno, insolacija, 'Srednja mesečna insolacija', 'h')
  })    
  
  output$oblacnost_sr2 <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja_mesecno, oblacnost_sr, 'Srednja mesečna oblačnost', 'desetina')
  })    
  
  output$brzina_vetra_sr2 <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja_mesecno, brzina_vetra_sr, 'Srenja mesečna brzina vetra', 'm/s')
  })    
  
  output$padavine2 <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja_mesecno, padavine_suma, 'Ukupna mesečna količina padavina', 'mm')
  })
  
  output$sneg_novo2 <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$klima_merenja_mesecno, sneg_novo, 'Ukupna mesečna količina novog snega', 'cm')
  })    
  
  # SEKCIJA KVALITET VAZDUHA
  output$so2 <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$kvazduh_merenja, so2, 'Koncentracija supor-dioksida', 'µg/m³')
  })
  
  output$no2 <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$kvazduh_merenja, no2, 'Koncentracija azot-dioksida', 'µg/m³')
  })
  
  output$o3 <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$kvazduh_merenja, o3, 'Koncentracija prizemnog ozona', 'µg/m³')
  })
  
  output$co <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$kvazduh_merenja, co, 'Koncentracija ugljen-monoksida', 'µg/m³')
  })
  
  output$pm10 <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$kvazduh_merenja, so2, 'Koncentracija suspendovanih PM10 čestica', 'µg/m³')
  })
  
  output$pm25 <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$kvazduh_merenja, pm25, 'Koncentracija suspendovanih PM2.5 čestica', 'µg/m³')
  })
  
  
  # SEKCIJA POVRŠINSKE VODE
  output$pvode_nivo <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$pvode_merenja, nivo, 'Vodostaj', 'cm')
  })
  
  output$pvode_protok <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$pvode_merenja, protok, 'Protok', 'm³/s')
  })
  
  output$pvode_temperatura <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$pvode_merenja, temperatura, 'Temperatura vode', '°C')
  })
  
  
  # SEKCIJA PODZEMNE VODE
  output$pdvode_nivo <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$pdvode_merenja, nivo, 'Nivo vode u pijezometru', 'cm')
  })
  
  output$pdvode_temperatura <- renderPlot({
    req(tabele_reaktivne$filtrirano)
    napravi_grafikon(tabele_reaktivne$filtrirano$pdvode_merenja, temperatura, 'Temperatura vode', '°C')
  })  
}

#### POKRETANJE APLIKACIJE ####
shinyApp(ui = ui, server = server)