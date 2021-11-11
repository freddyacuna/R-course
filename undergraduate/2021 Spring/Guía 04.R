library(dplyr)
library(forcats)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(ggfittext)
library(treemapify)
options(scipen=999)


wbData3f %>% filter(country %in% c("Argentina", "Chile", "United States")) %>%
ggplot(aes(x = year, y = credit)) +
geom_line(aes(colour = country))


wbData3f %>% filter(country %in% c("Argentina", "Chile", "United States")) %>%
ggplot(aes(x = year, y = credit, color = country)) +
geom_line(size=1) +
scale_x_continuous(breaks = seq(1960, 2020,10)) +
labs(title = "Crédito interno al sector privado otorgado por los bancos (% del PIB)",
subtitle = "1960-2018",
caption = paste("Fuente: Elaboración propia en base a WDI"),
x = "Año",
y = "Credito bancario/PIB (%)") +
scale_colour_manual(name = "",
values = c(Argentina = "#1b9e77",
Chile = "#d95f02",
'United States' = "#7570b3"),
labels = c("Argentina", "Chile", "Estados Unidos")) +
theme(
rect = element_blank(),
axis.title = element_text(family = "Helvetica", size = 9, colour = "#969696"),
axis.text = element_text(family = "Helvetica", size = 9, colour = "#969696"),
axis.line = element_blank(),
axis.ticks = element_line(colour = "#969696"),
plot.title = element_text(family = "AvantGarde", size = 11, colour = "#969696"),
plot.subtitle = element_text(family = "AvantGarde", size = 9, colour = "#969696"),
plot.caption = element_text(family = "Helvetica", size = 7, colour = "#969696"),
legend.text = element_text(family = "Helvetica", size = 8, colour = "#636363"),
legend.position = c(0.9, 0.4)
)



# fill = "#31a377"
wbData3f %>% filter(year == 2018) %>%
ggplot(aes(x = region)) +
geom_bar(aes(fill = region)) +
coord_flip()
wbData3f %>% filter(year == 2018) %>%
ggplot(aes(x = fct_rev(fct_infreq(as_factor(region))))) +
geom_bar(fill = "#31a377") +
scale_y_continuous(limits = c(0,65), expand = c(0,0)) +
labs(title = "Número de países por región, 2018",
x = NULL,
y = NULL) +
geom_text(aes(label= ..count..), stat = "count", hjust = -0.8) +
coord_flip() +
theme(rect = element_blank(),
axis.text.x = element_blank(),
axis.ticks = element_blank())



wbData3f %>% filter(year == 2019) %>%
slice_max(population, n = 10) %>%
mutate(prop = round(100*population/sum(population),1)) %>%
select(country, population, prop) %>%
ggplot(aes(x= 1, y = prop, fill = country)) + 
geom_bar(stat = "identity", color = "white") +
coord_polar(theta = 'y') +
theme_void()
wbData3f %>% filter(year == 2019) %>%
slice_max(population, n = 10) %>%
mutate(prop = round(100*population/sum(population),1)) %>%
select(country, population, prop) %>%
ggplot(aes(area= prop, fill = country, label = country)) +
geom_treemap(show.legend = FALSE, color = "white") +
geom_treemap_text(place = "topleft", max.size = 10, padding.y = grid::unit(2, "mm"),
grow = TRUE, colour = "white") +
scale_x_continuous(limits = c(0,1)) +
scale_y_continuous(limits = c(0,1)) +
annotate(geom = "text", x = 0.58, y = 0.01, label = "32%", size = 6, color = "#FFFFFF") +
annotate(geom = "text", x = 0.58, y = 0.58, label = "31%", size = 6, color = "#FFFFFF") +
theme(axis.title =element_blank(),
axis.text =element_blank(),
axis.ticks =element_blank())


wbData3f %>% filter(year == 2019) %>% slice_max(gdppc, n = 10) %>%
ggplot(aes(x = fct_reorder(as_factor(country), gdppc), y = gdppc,
label = paste("USD", round(gdppc,0)))) +
geom_col(fill = "#013e63") +
scale_y_continuous(limits = c(0,130000), expand = c(0,0)) +
labs(title = "Diez países con el mayor PIB per cápita, 2019",
caption = paste("Fuente: Elaboración propia en base a", "\n",
"World Development Indicators"),
x = NULL,
y = NULL) +
geom_bar_text(position = "stack", reflow = TRUE, size = 9) +
coord_flip() +
theme(rect = element_blank(),
axis.text.x = element_blank(),
axis.ticks = element_blank())
wbData3f %>% filter(year == 2019) %>% slice_max(gdppc, n = 10) %>%
ggplot(aes(x = fct_reorder(as_factor(country), gdppc), y = gdppc,
label = paste("USD", round(gdppc,0)))) +
geom_col(aes(fill = region)) +
scale_y_continuous(limits = c(0,130000), expand = c(0,0)) +
scale_fill_brewer(name = NULL,
guide = guide_legend(nrow = 2),
palette = "Paired") +
labs(title = "Diez países con el mayor PIB per cápita, 2019",
caption = paste("Fuente: Elaboración propia en base a", "\n",
"World Development Indicators"),
x = NULL,
y = NULL) +
geom_bar_text(position = "stack", reflow = TRUE, size = 9) +
coord_flip() +
theme(rect = element_blank(),
axis.text.x = element_blank(),
axis.ticks = element_blank(),
legend.position= "top")
wbData3f %>% filter(year == 2019) %>% slice_max(gdppc, n = 10) %>%
mutate(highlight = case_when(
region == "Este Asiático y Pacífico" ~ "Este Asiático y Pacífico",
region == "Medio Oriente y Norte de Africa" ~ "Medio Oriente y Norte de Africa",
TRUE ~ "Otra región")
) %>%
ggplot(aes(x = fct_reorder(as_factor(country), gdppc), y = gdppc,
label = paste("USD", round(gdppc,0)))) +
geom_col(aes(fill = highlight)) +
scale_y_continuous(limits = c(0,140000), expand = c(0,0)) +
scale_fill_manual(name = NULL,
values = c("#013e63", "#8FA5C3", "#C5C5C5")) +
labs(title = "Diez países con el mayor PIB per cápita, 2019",
caption = paste("Fuente: Elaboración propia en base a", "\n",
"World Development Indicators"),
x = NULL,
y = NULL) +
geom_bar_text(position = "stack", reflow = TRUE, size = 9) +
coord_flip() +
theme(rect = element_blank(),
axis.text.x = element_blank(),
axis.ticks = element_blank(),
legend.position= "top",
legend.text = element_text(size = 8))


wbData3f %>% filter(year == 2018) %>%
group_by(region) %>%
summarise(median_gdppc = median(gdppc, na.rm = TRUE)) %>%
ggplot(aes(x = fct_reorder(as_factor(region), median_gdppc),
y = median_gdppc)) +
geom_segment(aes(xend = region, y = 0, yend = median_gdppc),
colour = "#878787") +
geom_point(colour = "#b06b12", size = 2) +
scale_y_continuous(limits = c(0,80000), expand = c(0,0)) +
labs(title = "Mediana del PIB per cápita por región, 2018",
caption = "Fuente: Elaboración propia en base a WDI",
x = NULL,
y = NULL) +
geom_text(aes(label = paste("USD", round(median_gdppc,0))),
hjust = -0.1, size = 3) +
coord_flip() +
theme(rect = element_blank(),
axis.text.x = element_blank(),
axis.ticks = element_blank())


wbData3f %>% filter(year == 2018) %>%
ggplot(aes(x = region, y = credit, fill = region)) +
geom_boxplot(show.legend = FALSE) +
labs(title = "Crédito interno al sector privado otorgado por los bancos (% del PIB)",
subtitle ="2018",
x = NULL,
y = "Credito bancario/PIB (%)") +
coord_flip() +
theme_classic()



wbData3f %>% filter(year == 2019) %>%
ggplot(aes(x = gdppc)) +
geom_histogram(binwidth = 5000, col = "#ffffff", fill = "#800000") +
scale_x_continuous(expand = c(0,0),
limits = c(0, 140000),
labels = function(x) x/1000,
breaks = c(0,25000,50000,75000,100000, 125000)) +
scale_y_continuous(expand = c(0,0)) +
labs(title = "PIB per cápita alrededor del mundo",
subtitle = "2019",
x = "PIB per cápita (miles de dólares)",
y = "Frecuencia") +
theme_classic()



# ecdf: empirical cumulative distribution function
wbData3f %>% filter(year==2019) %>%
ggplot(aes(x = gdppc)) +
stat_ecdf(geom = "step") +
scale_x_continuous(labels = function(x) x/1000,
breaks = c(0,25000,50000,75000,100000, 125000),
expand = c(0,0)) +
scale_y_continuous(expand = c(0,0)) +
labs(title = "PIB per cápita mundial 2019",
subtitle = "Distribución acumulada",
x = "PIB per cápita (miles de dólares)",
y = "Proporción de países") +
theme_classic()


wbData3f %>% filter(year==2018) %>%
ggplot(aes(x=gdppc, y=atm))+
geom_point(alpha = 0.4, size = 0.8) +
labs(title = "ATMs versus PIB per cápita",
subtitle = "2018",
caption = "Fuente: Elaboración propia en base a WDI.",
x = "PIB per cápita",
y = "ATMs/100000 adultos") +
theme(panel.background = element_blank(),
axis.line = element_line())
wbData3f %>% filter(year==2018) %>%
ggplot(aes(x=gdppc, y=atm))+
geom_point(alpha = 0.4, size = 0.8) +
scale_x_log10() +
labs(title = "ATMs versus PIB per cápita",
subtitle = "2018",
caption = "Fuente: Elaboración propia en base a WDI.",
x = "PIB per cápita",
y = "ATMs/100000 adultos") +
theme(rect = element_blank(),
axis.line = element_line())
