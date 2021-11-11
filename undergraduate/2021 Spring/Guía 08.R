# Pregunta 1
# El vector x contiene una lista de países, los cuales comienzan en mayúsculas y minúsculas y, además, incluyen
# caracteres que no son letras.

x <- c("argentina(1)", "Chile", "el salvador", "guatemala", "Honduras", "Uganda")

# (a) Elimine cualquier caracter que no sea una letra
country <- str_replace_all(x, pattern = "[\\d\\(\\)]", "")
country
str_replace_all(x, pattern = "[^a-zA-Z\\s]", "")

# (b) Cambia a mayúscula la primera letra de cada uno de los países cuando sea necesario
country <- str_to_title(country)
country

# (c) Cambiar a mayúsculas todas las letras de cada uno de los países
str_to_upper(country)

# (d) ¿Cuántos nombres de países comienzan con una vocal? ¿Qué países comienzan con una vocal?
sum(str_detect(country,"^[AEIOU]"))
str_subset(country,"^[AEIOU]")

# (e) ¿Cuántos nombres de países comienzan con una consonante? ¿Qué países comienzan con una consonante?
sum(str_detect(country,"^[ˆAEIOU]"))
str_subset(country,"^[^AEIOU]")

# (f) Liste los países cuyos nombres comienzan y terminan con una consonante
str_subset(country, "^[^AEIOU].*[^aeiou]$")
country[str_detect(country, "^[^AEIOU].*[^aeiou]$")]

# (g) Liste los países cuyos nombres comienzan con A o U

str_subset(country, "^[AU]")

# (h) ¿Cuántas letras tiene cada país?
country
str_length(country)
str_count(country, "[a-zA-Z]")

# (i) ¿Cuántas vocales, en promedio, tiene el nombre de cada país?
str_count(country, "[aeiouAEIOU]")
mean(str_count(country, "[aeiouAEIOU]"))

# (j) Obtenga la letra del medio de cada uno de los países ¿Qué recomienda hacer si el largo del string es un
# número par?
country
largo <- str_count(country, "[a-zA-Z]")
largo
m <- ceiling(largo/2)
m
str_sub(country, m, m)

# (k) Liste los países que comienzan con 2 consonantes
str_subset(country, "^[^aeiouAEIOU]{2}")
