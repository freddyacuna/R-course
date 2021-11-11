library(readr)
library(dplyr)
library(tidytext)
library(pdftools)
library(ggwordcloud)
library(ggplot2)
library(stringr)

# Pregunta 1
# Construya una nube de palabras a partir del mensaje del Presidente Sebastián Piñera pronunciado ante el
# Congreso Nacional el 1 de junio del 2021. El mensaje presidencial 2021 puede encontrarlo en la siguiente
# página: https://www.bcn.cl/historiapolitica/corporaciones/cuentas_publicas/detalle?tipo=presidentes

# Lectura de archivo
speech2021 <- pdf_text("datos/DiscursoPresidencial2021.pdf")
discurso <- as_tibble(speech2021)

# Tokenización
palabras <- discurso %> % unnest_tokens(word, value)

# Eliminar palabras vacías
stop_spanish <- get_stopwords("es")
palabras_sinstop <- palabras %> %
anti_join(stop_spanish, by= "word")

# Frecuencia de palabras
freq <- palabras_sinstop %> % count(word, sort = TRUE)

# Nube de palabras
set.seed(5235)
freq %> % mutate(angulo = 45 * sample(seq(-2,2,0.5), n(), replace = TRUE)) %> %
filter(n>=15) %> %
ggplot(aes(label = word, size = n, color = n, angle = angulo)) +
geom_text_wordcloud_area() +
scale_size_area(max_size = 20) +
scale_color_gradient(low = "#fee0d2", high = "#67000d") +
theme_minimal()

# Pregunta 2
# Usando los datos de oscar_movies1927_2020.csv que contiene información sobre los Premios Oscar para el
# período 1927-2020, reponda las siguientes preguntas:
movies <- read_csv("datos/oscar_movies1927_2020.csv")

# (a) ¿Cuántas observaciones y qué variables contiene la base de datos?
glimpse(movies)

# (b) Liste los nombres nominados en distintas categorías que comiencen con Richard o James.
movies %> % pull(name) %> % str_subset(pattern = "ˆ(Richard|James)")

# (c) Presente un listado de películas cuyos títulos comienzan o terminan en cuatro dígitos.
movies %> % filter(film %in % str_subset(film, pattern = "ˆ\\d{4}|\\d{4}$")) %> %
distinct(film)

# (d) ¿Cuáles son los diez nombres de pila más frecuentes entre los nominados en la categoría “Mejor actor”?
movies %> % filter(category == "ACTOR") %> %
mutate(nombre_pila = str_extract(name, pattern = "ˆ\\w+")) %> %
count(nombre_pila, sort = TRUE) %> % slice_head(n = 10)
