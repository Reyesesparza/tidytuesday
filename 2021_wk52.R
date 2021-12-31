library(tidyverse)
library(patchwork)


## Datos ##

starbucks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')

## Trabajando Datos mis bebidas favoritas##


favoritos <- starbucks %>%
  filter( (product_name == "Espresso" & size == "doppio") | (product_name == "Caffè Mocha" & size == "grande" & milk == 5)
          | (product_name == "Espresso - Caffè Americano" &  size == "grande")) %>%
  filter( whip == 0)


## Funcion para convertir a numericas las columnas que quiera ##
convertir_a_valor <- function(datos, columnas){
  datos = as.data.frame(datos)
  columnas <- as.vector(columnas)
  n_col = length(columnas)
  nb_col = vector("numeric")
  
  for (i in 1:n_col) {
    
    nb_col[i] <- columnas[i]
    datos[nb_col[i]] <- as.numeric(unlist(datos[nb_col[i]]))
    
  }
  
  return(datos)
}



## Etiquetas ##

favoritos_l <- convertir_a_valor(favoritos, c(3:15)) %>%
  pivot_longer(cols = 3:15, names_to = "extra", values_to = "dato") %>%
  filter( ! extra %in%  c("serv_size_m_l", "milk", "whip", "fiber_g", "trans_fat_g" ))


etiqueta <- favoritos_l %>%
  filter( extra %in% c("caffeine_mg", "calories","sodium_mg")) %>%
  filter( dato > 15)







## Trabajando Datos Resumen ##

starbucks_sum <- starbucks %>%
  group_by( product_name, size, milk) %>%
  summarise( caffeine_mg = median(caffeine_mg),
             calories = median(calories)) %>%
  ungroup() %>%
  filter( ! str_detect(product_name, "brewed|Brewed"))





## Graficar Mis bebidas favoritas ##

## Tema Sencillo ##

tema_1 <- function(base_size = 10, base_family = "Raleway"){
  
  color_background = "#FFFFFF"
  color_grid_major = "#EBEBEB" 
  color_axis_text = "#575757"  
  color_axis_title = "#575757" 
  color_title = "#575757"
  color_subtitle = "#575757"
  strip_background_color = "#575757"
  
  
  aqui <- theme_minimal(base_size = base_size) + 
    theme( panel.background = element_rect(fill = color_background, color = color_background))+
    theme(plot.background = element_rect(fill = color_background, color = color_background)) +
    theme( panel.grid.major = element_line(color = color_grid_major)) +
    theme( panel.grid.minor = element_line(color = color_grid_major)) +
    theme( axis.ticks = element_blank())+
    theme(plot.title = element_text(  color = color_title, family = base_family, size = rel(1.6), vjust = 1.5, hjust = .5)) +
    theme(plot.subtitle = element_text( color = color_subtitle, family = base_family, size = base_size + 2, hjust = .5)) +
    theme(plot.caption = element_text( color = color_subtitle, family = base_family, size = base_size)) +
    theme(axis.text.x = element_text( color = color_axis_text, family = base_family, size = base_size)) +
    theme(axis.text.y = element_text( color = color_axis_text, family = base_family, size = base_size)) +
    theme(axis.text = element_text( color = color_axis_text, family = base_family, size = base_size)) +
    theme(axis.title.x = element_text(color = color_axis_title, family = base_family, size = base_size + 1.2, vjust = 1.25)) +
    theme(axis.title.y = element_text(color = color_axis_title, family = base_family, size = base_size + 1.2, vjust = 1.25)) +
    theme( axis.title = element_text(color = color_axis_title, family = base_family, size = base_size + 1.2))
  
  
  
  aqui
  
}


## Grafico polar ##

g1 <- ggplot()+
  geom_bar( data = favoritos_l[favoritos_l$product_name == "Espresso - Caffè Americano",], aes( x = extra, y = dato), fill = "#128232", stat = "identity", alpha = .7)+
  geom_bar( data = favoritos_l[favoritos_l$product_name == "Caffè Mocha",], aes( x = extra, y = dato), fill = "#C29742", stat = "identity", alpha = .7) +
  geom_bar( data = favoritos_l[favoritos_l$product_name == "Espresso",], aes( x = extra, y = dato), fill = "#5F4209", stat = "identity", alpha = .7)+
  geom_text( data = etiqueta[etiqueta$product_name == "Espresso - Caffè Americano",], aes( x = extra, y = dato, label = dato), size = 4, color = "#404040", vjust= +1.2,fontface = 'bold') +
  geom_text( data = etiqueta[etiqueta$product_name == "Caffè Mocha",], aes( x = extra, y = dato, label = dato), size = 4, color = "#404040", vjust = +1.2, fontface = 'bold') +
  geom_text( data = etiqueta[etiqueta$product_name == "Espresso",], aes( x = extra, y = dato, label = dato), size = 4, color = "#404040", vjust = +1.2, fontface = 'bold')+
  coord_polar()+
  tema_1()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank())+
  labs(title = "¿Cual de mis cafes favoritos tiene mas cafeina y calorias?")







## Grafico de dispersion ##

leyendas <- c( "Caffè Mocha" = "#C29742", "Espresso" =  "#5F4209", "Espresso - Caffè Americano"= "#128232" )

g2 <- ggplot()+
  geom_point(data = starbucks_sum, aes( x = caffeine_mg, y = calories),   color = "#D5D8DC")+
  geom_point( data = favoritos, aes ( x = caffeine_mg, y = calories, color = product_name), size = 4)+
  geom_vline(xintercept = median(starbucks_sum$caffeine_mg), color = "#EBEBEB", size = 1.4)+
  geom_hline(yintercept = median(starbucks_sum$calories), color = "#EBEBEB", size = 1.4)+
  labs(title = "Si comparamos mis favoritos vs todas las bebidas que ofrece starbucks \n ¿en que grupo estoy?")+
  tema_1()+
  theme(panel.grid.major  = element_blank(),
        panel.grid.minor = element_blank())+
  scale_color_manual(values = leyendas, name = "Mis cafes favoritos", labels = c("Caffe Mocha","Espresso","Americano"))+
  theme(legend.position = "left",
        legend.justification = "top",
        legend.text = element_text(color = "#575757", family = "Raleway", size = 15 ),
        legend.title = element_text(color = "#575757", family = "Raleway", size = 15))+
  annotate("text", x = favoritos$caffeine_mg[favoritos$product_name == "Espresso - Caffè Americano" ],
           y = favoritos$calories[favoritos$product_name == "Espresso - Caffè Americano"] + 14,
           label = "Los de diario, con alto contenido de cafeina", color = "#575757", family = "Raleway") +
  annotate("text", x = favoritos$caffeine_mg[favoritos$product_name == "Caffè Mocha"],
           y = favoritos$calories[favoritos$product_name == "Caffè Mocha"] - 20,
           label = "El que lleva chocolate, para acompañar al pan  + calorias", color = "#575757", family = "Raleway")+
  annotate("text", x = favoritos$caffeine_mg[favoritos$product_name == "Espresso - Caffè Americano" ],
           y = favoritos$calories[favoritos$product_name == "Espresso - Caffè Americano"] -10,
           label = "Americano", color = "#128232", family = "Raleway", fontface = "bold")+
  annotate("text", x = favoritos$caffeine_mg[favoritos$product_name == "Caffè Mocha" ],
           y = favoritos$calories[favoritos$product_name == "Caffè Mocha"] +15,
           label = "Caffe Mocha", color = "#C29742", family = "Raleway", fontface="bold")+
  annotate("text", x = favoritos$caffeine_mg[favoritos$product_name == "Espresso" ],
           y = favoritos$calories[favoritos$product_name == "Espresso"] -10,
           label = "Espresso", color = "#5F4209", family = "Raleway", fontface="bold")



## uniendo graficos ##


grafico <- g1 + g2 + plot_annotation(title = "¿Te has preguntado alguna vez cuánta cafeína y calorías tomas en tu café?",
                                     subtitle = "Con la informacion de Starbucks Coffee Company Beverage Nutrition Information puedes saberlo, este es mi caso",
                                     caption = "fuente: Starbucks Coffee Company Beverage Nutrition Information \n
                                      viz por: @ReyesEsparza10",
                                     theme = theme(plot.title = element_text(color = "#034C18", family = "Raleway", size = 35, hjust = .5),
                                                   plot.subtitle = element_text(color = "#034C18", family = "Raleway", size = 20, hjust = .5),
                                                   plot.caption = element_text(color = "#034C18", family = "Raleway", size = 9)))

## Guardando grafico ##

ggsave(grafico, filename = "grafico.png", width = 19, height = 10)

