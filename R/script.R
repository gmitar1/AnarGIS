library('dplyr')
library('RSQLite')
library('DBI')
library('ggplot2')
library('lubridate')

#### FUNKCIJE ####
ucitavanje_baze <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), 'podaci.gpkg')
  
  tabele <- list()
  tabele$`klima_stanice` <- DBI::dbReadTable(con, 'klima_stanice')
  tabele$`klima_merenja_dnevno` <- DBI::dbReadTable(con, 'klima_merenja_dnevno')
  tabele$`klima_merenja_mesecno` <- DBI::dbReadTable(con, 'klima_merenja_mesecno')
  tabele$`kvazduh_stanice` <- DBI::dbReadTable(con, 'kvazduh_stanice')
  tabele$`kvazduh_merenja` <- DBI::dbReadTable(con, 'kvazduh_merenja_dnevno')
  tabele$`pdvode_stanice` <- DBI::dbReadTable(con, 'pdvode_stanice')
  tabele$`pdvode_tela` <- DBI::dbReadTable(con, 'pdvode_tela')
  tabele$`pdvode_merenja` <- DBI::dbReadTable(con, 'pdvode_merenja_dnevno')
  tabele$`pvode_stanice` <- DBI::dbReadTable(con, 'pvode_stanice')
  tabele$`pvode_reke` <- DBI::dbReadTable(con, 'pvode_reke')
  tabele$`pvode_merenja` <- DBI::dbReadTable(con, 'pvode_merenja_dnevno')
  
  DBI::dbDisconnect(con)
  
  tabele$`klima_merenja_mesecno` <- tabele$`klima_merenja_mesecno` %>%
    mutate(
      datum = ceiling_date(
        as.Date(paste(godina, mesec, "01", sep = "-")), 
        "month"
      ) - days(1)
    )
  
  return(tabele)
}


#### OLD COD
napravi_grafikon_old <- function(tabela, metrika, naslov = '???', jedinica = '?', stanica = '', dodatna_anotacija = '') {
  metrika_sym <- rlang::sym(metrika)
  
  plot <- ggplot2::ggplot(tabela, aes(x = as.Date(datum), y = !!metrika_sym, color = !!metrika_sym)) + 
    geom_line(linewidth = 0.2, alpha = 0.9) +
    scale_colour_gradient(low = '#A0F549', high = '#CF3E63') +
    scale_x_date(breaks = '1 year', date_labels = '%Y', limits = as.Date(c('2000-01-01', '2024-12-31'))) +
    ggtitle(naslov) +
    ylab(jedinica) +
    xlab(element_blank()) +
    tema
  
  # Add station annotation on the right if provided
  if (stanica != "") {
    plot <- plot + 
      annotate("text", 
               x = as.Date('2024-12-31'), 
               y = Inf, 
               label = paste0('Stanica: ', stanica), 
               hjust = 1, 
               vjust = 1.5,
               color = '#87cf3e',
               family = 'mono',
               size = 4)
  }
  
  # Add additional annotation on the left if provided
  if (dodatna_anotacija != "") {
    plot <- plot + 
      annotate("text", 
               x = as.Date('2000-01-01'), 
               y = Inf, 
               label = dodatna_anotacija, 
               hjust = 0, 
               vjust = 1.5,
               color = '#87cf3e',
               family = 'mono',
               size = 4)
  }
  
  return(plot)
}

napravi_grafikon <- function(tabela, metrika, naslov = '???', jedinica = '?', stanica = '', dodatna_anotacija = '') {
  metrika_sym <- rlang::sym(metrika)
  
  # Define which metrics should be bar charts
  bar_metrics <- c(
    # klima_dnevno
    'padavine', 'sneg_novo', 'sneg_ukupno',
    # klima_mesecno
    'vetar_gt_6b', 'vetar_gt_8b', 'padavine_suma', 'padavine_max', 
    'sneg_novo', 'br_dana_kisa', 'br_dana_sneg', 'br_dana_magla'
  )
  
  # Create the base plot
  plot <- ggplot2::ggplot(tabela, aes(x = as.Date(datum), y = !!metrika_sym, fill = !!metrika_sym)) + 
    ggtitle(naslov) +
    ylab(jedinica) +
    xlab(element_blank()) +
    tema
  
  # Choose geom based on metric type
  if (metrika %in% bar_metrics) {
    plot <- plot + 
      geom_col(width = 1, alpha = 0.9, color = '#A0F549', fill = '#A0F549') 
  } else {
    plot <- plot + 
      geom_line(linewidth = 0.2, alpha = 0.9, aes(color = !!metrika_sym)) +
      scale_colour_gradient(low = '#A0F549', high = '#CF3E63')
  }
  
  # Add x-axis scaling (common for both plot types)
  plot <- plot + 
    scale_x_date(breaks = '1 year', date_labels = '%Y', limits = as.Date(c('2000-01-01', '2024-12-31')))
  
  # Add station annotation on the right if provided
  if (stanica != "") {
    plot <- plot + 
      annotate("text", 
               x = as.Date('2024-12-31'), 
               y = Inf, 
               label = paste0('Stanica: ', stanica), 
               hjust = 1, 
               vjust = 1.5,
               color = '#87cf3e',
               family = 'mono',
               size = 4)
  }
  
  # Add additional annotation on the left if provided
  if (dodatna_anotacija != "") {
    plot <- plot + 
      annotate("text", 
               x = as.Date('2000-01-01'), 
               y = Inf, 
               label = dodatna_anotacija, 
               hjust = 0, 
               vjust = 1.5,
               color = '#87cf3e',
               family = 'mono',
               size = 4)
  }
  
  return(plot)
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

tabele <- ucitavanje_baze()

vrednosti <- list()

vrednosti$klima_dnevno$id <- tabele$klima_stanice %>% filter(id %in% tabele$klima_merenja_dnevno$id_stanice) %>% distinct(id) %>% pull(id)
vrednosti$klima_dnevno$metrike <- c('temperatura_max', 'temperatura_min', 'temperatura_sr','pritisak_sr', 'relativna_vlaznost_sr', 'napon_vodene_pare_sr', 'oblacnost_sr', 'brzina_vetra_sr', 'insolacija', 'padavine', 'sneg_novo', 'sneg_ukupno')
vrednosti$klima_dnevno$naslovi <- c('Maksimalna dnevna temperatura', 'Minimalna dnevna temperatura', 'Srednja dnevna temperatura','Srednji dnevni atmosferski pritisak', 'Srednja dnevna relativna vlažnost', 'Srednji dnevni napon vodene pare', 'Srednja dnevna oblačnost', 'Srednja dnevna brzina vetra', 'Insolacija', 'Ukupna dnevna količina padavina', 'Dnevna količina novog snega', 'Ukupna količina snega na dan')
vrednosti$klima_dnevno$jedinice <- c('°C', '°C', '°C', 'mb', '%', 'mb', 'desetina', 'm/s', 'h', 'mm', 'cm', 'cm')

vrednosti$klima_mesecno$id <- tabele$klima_stanice %>% filter(id %in% tabele$klima_merenja_mesecno$id_stanice) %>% distinct(id) %>% pull(id)
vrednosti$klima_mesecno$metrike <- c('temperatura_max', 'temperatura_min', 'temperatura_max2', 'temperatura_min2', 'pritisak_sr', 'relativna_vlaznost_sr', 'napon_vodene_pare_sr', 'oblacnost_sr', 'brzina_vetra_sr', 'vetar_gt_6b', 'vetar_gt_8b', 'insolacija', 'padavine_suma', 'padavine_max', 'sneg_novo', 'br_dana_kisa', 'br_dana_sneg', 'br_dana_magla')
vrednosti$klima_mesecno$naslovi <- c('Srednja maksimalna mesečna temperatura vazduha', 'Srednja minimalna mesečna temperatura vazduha', 'Maksimalna mesečna temperatura vazduha', 'Minimalna mesečna temperatura vazduha', 'Srednji mesečni atmosferski pritisak', 'Srednja mesečna relativna vlažnost', 'Srednji mesečni napon vodene pare', 'Srednja mesečna oblačnost', 'Srednja mesečna brzina vetra', 'Broj dana sa brzinom vetra većom od 6 bosfora', 'Broj dana sa brzinom vetra većom od 8 bosfora', 'Ukupna mesečna insolacija', 'Ukupna mesečna količina padavina', 'Maksimalna dnevna količina padavina','Ukupna mesečna količina novog snega', 'Broj dana sa pojavom kiše', 'Broj dana sa pojavom snega', 'Broj dana sa pojavom magle')
vrednosti$klima_mesecno$jedinice <- c('°C', '°C', '°C', '°C', 'mb', '%', 'mb', 'desetina', 'm/s', 'dana', 'dana','h', 'mm', 'mm', 'cm', 'dana', 'dana', 'dana')

vrednosti$kvazduh$id <- tabele$kvazduh_stanice %>% filter(id %in% tabele$kvazduh_merenja$id_stanice) %>% distinct(id) %>% pull(id)
vrednosti$kvazduh$metrike <- c('so2', 'no2', 'o3', 'co', 'pm10', 'pm25')
vrednosti$kvazduh$naslovi <- c('Koncentracija sumpor-dioksida', 'Koncentracija azot-dioksida', 'Koncentracija prizemnog ozona', 'Koncentracija ugljen-monoksida', 'Koncentracija suspendovanih PM10 čestica', 'Koncentracija suspendovanih PM2.5 čestica')
vrednosti$kvazduh$jedinice <- c('µg/m³', 'µg/m³', 'µg/m³', 'µg/m³', 'µg/m³', 'µg/m³')

vrednosti$pvode$id <- tabele$pvode_stanice %>% filter(id %in% tabele$pvode_merenja$id_stanice) %>% distinct(id) %>% pull(id)
vrednosti$pvode$metrike <- c('nivo', 'protok', 'temperatura')
vrednosti$pvode$naslovi <- c('Vodostaj', 'Protok vode', 'Temperatura vode')
vrednosti$pvode$jedinice <- c('cm', 'm³/s', '°C')

vrednosti$pdvode$id <- tabele$pdvode_stanice %>% filter(id %in% tabele$pdvode_merenja$id_stanice) %>% distinct(id) %>% pull(id)
vrednosti$pdvode$metrike <- c('nivo', 'temperatura')
vrednosti$pdvode$naslovi <- c('Nivo podzemne vode', 'Temperatura vode')
vrednosti$pdvode$jedinice <- c('cm', '°C')

#### KLIMA DNEVNO
for (s in vrednosti$klima_dnevno$id) {
  
  for (i in seq_along(vrednosti$klima_dnevno$metrike)) {
    
    metrika <- vrednosti$klima_dnevno$metrike[i]
    naslov <- vrednosti$klima_dnevno$naslovi[i]
    jedinica <- vrednosti$klima_dnevno$jedinice[i]
    
    naziv_stanice <- tabele$klima_stanice %>% filter(id == s) %>% pull(naziv)
    merenje <- tabele$klima_merenja_dnevno %>% filter(id_stanice == s)
    
    p <- napravi_grafikon(merenje, metrika, naslov, jedinica, naziv_stanice)
    
    ime_fajla <- paste0("output/klima_merenja_dnevno/klima_merenja_dnevno_", s, "_", metrika, ".png")
    
    ggsave(filename = ime_fajla, plot = p, device = "png", width = 10, height = 3, dpi = 90)
    
    cat("Saved:", ime_fajla, "\n")
  }
}

#### KLIMA MESEČNO
for (s in vrednosti$klima_mesecno$id) {
  
  for (i in seq_along(vrednosti$klima_mesecno$metrike)) {
    
    metrika <- vrednosti$klima_mesecno$metrike[i]
    naslov <- vrednosti$klima_mesecno$naslovi[i]
    jedinica <- vrednosti$klima_mesecno$jedinice[i]
    
    naziv_stanice <- tabele$klima_stanice %>% filter(id == s) %>% pull(naziv)
    merenje <- tabele$klima_merenja_mesecno %>% filter(id_stanice == s)
    
    p <- napravi_grafikon(merenje, metrika, naslov, jedinica, naziv_stanice)
    
    ime_fajla <- paste0("output/klima_merenja_mesecno/klima_merenja_mesecno_", s, "_", metrika, ".png")
    
    ggsave(filename = ime_fajla, plot = p, device = "png", width = 10, height = 3, dpi = 90)
    
    cat("Saved:", ime_fajla, "\n")
  }
}

#### KVAZDUH DNEVNO
for (s in vrednosti$kvazduh$id) {
  
  for (i in seq_along(vrednosti$kvazduh$metrike)) {
    
    metrika <- vrednosti$kvazduh$metrike[i]
    naslov <- vrednosti$kvazduh$naslovi[i]
    jedinica <- vrednosti$kvazduh$jedinice[i]
    
    naziv_stanice <- tabele$kvazduh_stanice %>% filter(id == s) %>% pull(naziv)
    merenje <- tabele$kvazduh_merenja %>% filter(id_stanice == s)
    
    p <- napravi_grafikon(merenje, metrika, naslov, jedinica, naziv_stanice)
    
    ime_fajla <- paste0("output/kvazduh_merenja_dnevno/kvazduh_merenja_dnevno_", s, "_", metrika, ".png")
    
    ggsave(filename = ime_fajla, plot = p, device = "png", width = 10, height = 3, dpi = 90)
    
    cat("Saved:", ime_fajla, "\n")
  }
}

#### PVODE DNEVNO
for (s in vrednosti$pvode$id) {
  
  for (i in seq_along(vrednosti$pvode$metrike)) {
    
    metrika <- vrednosti$pvode$metrike[i]
    naslov <- vrednosti$pvode$naslovi[i]
    jedinica <- vrednosti$pvode$jedinice[i]
    
    naziv_stanice <- tabele$pvode_stanice %>% filter(id == s) %>% pull(naziv)
    merenje <- tabele$pvode_merenja %>% filter(id_stanice == s)
    
    id_reke <- tabele$pvode_stanice %>% filter(id == s) %>% pull(id_reke)
    naziv_reke <- tabele$pvode_reke %>% filter(id == id_reke) %>% pull(naziv)
    
    p <- napravi_grafikon(merenje, metrika, naslov, jedinica, naziv_stanice, dodatna_anotacija = paste0('Reka: ', naziv_reke))
    
    ime_fajla <- paste0("output/pvode_merenja_dnevno/pvode_merenja_dnevno_", s, "_", metrika, ".png")
    
    ggsave(filename = ime_fajla, plot = p, device = "png", width = 10, height = 3, dpi = 90)
    
    cat("Saved:", ime_fajla, "\n")
  }
}

#### PDVODE DNEVNO
for (s in vrednosti$pdvode$id) {
  
  for (i in seq_along(vrednosti$pdvode$metrike)) {
    
    metrika <- vrednosti$pdvode$metrike[i]
    naslov <- vrednosti$pdvode$naslovi[i]
    jedinica <- vrednosti$pdvode$jedinice[i]
    
    naziv_stanice <- tabele$pdvode_stanice %>% filter(id == s) %>% pull(naziv)
    merenje <- tabele$pdvode_merenja %>% filter(id_stanice == s)
    
    id_tela <- tabele$pdvode_stanice %>% filter(id == s) %>% pull(id_tela)
    naziv_tela <- tabele$pdvode_tela %>% filter(id == id_tela) %>% pull(naziv)
    
    p <- napravi_grafikon(merenje, metrika, naslov, jedinica, naziv_stanice, dodatna_anotacija = paste0('Telo: ', naziv_tela))
    
    ime_fajla <- paste0("output/pdvode_merenja_dnevno/pdvode_merenja_dnevno_", s, "_", metrika, ".png")
    
    ggsave(filename = ime_fajla, plot = p, device = "png", width = 10, height = 3, dpi = 90)
    
    cat("Saved:", ime_fajla, "\n")
  }
}