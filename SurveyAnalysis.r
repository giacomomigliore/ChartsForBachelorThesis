questionario2016 <- read.table("Magnificat_Questionario2016.txt", header = TRUE, sep = ",")

rmf::frequenze(questionario2016$Provenienza)
# x 
#   x     n            f  
# +---+-----+------------+ 
# | 1 | 233 |  34.984985 | 
# | 2 | 376 |  56.456456 | 
# | 3 |  57 |   8.558559 | 
# +---+-----+------------+ 
#       666   100.000000   
# Osservazioni mancanti: 0 

mean(questionario2016$Spesa)
# [1] 9.605856

# Spesa media divisa per categoria
by(questionario2016$Spesa, questionario2016$Provenienza, mean)
# questionario2016$Provenienza: 1 Cuneo
# [1] 7.214592
# ------------------------------------------------------------------------ 
# questionario2016$Provenienza: 2 Fuori provincia
# [1] 10.94016
# ------------------------------------------------------------------------ 
# questionario2016$Provenienza: 3 Estero
# [1] 10.57895

questionario2017 <- read.table("Magnificat - Questionario2017.txt", header = TRUE, sep = ",")

rmf::frequenze(questionario2017$Provenienza)
# x 
#   x     n            f  
# +---+-----+------------+ 
# | 1 |  89 |  22.646310 | 
# | 2 | 274 |  69.720102 | 
# | 3 |  30 |   7.633588 | 
# +---+-----+------------+ 
#       393   100.000000   
# Osservazioni mancanti: 0 

mean(questionario2017$Spesa)
# [1] 11.73537

by(questionario2017$Spesa, questionario2017$Provenienza, mean)
# questionario2017$Provenienza: 1 Cuneo
# [1] 6.938202
# ------------------------------------------------------------------------ 
# questionario2017$Provenienza: 2 Fuori provincia
# [1] 13.19343
# ------------------------------------------------------------------------ 
# questionario2017$Provenienza: 3 Estero
# [1] 12.65

questionario2015 <- read.table(file="Magnificat2015.txt", header = T, sep = ",")

# Rimuovo campi spesa (e provenienza per il 2015) vuoti: 2015 da 1462 a 1372, 2016 da 666 elementi a 488 e 2017 da 393 a 300
questionario2015_1 <- questionario2015[! (is.na(questionario2015$Provenienza) | is.na(questionario2015$Spesa)),]
questionario2015 <- questionario2015_1
rm(questionario2015_1)
questionario2016_1 <- questionario2016[! questionario2016$Spesa %in% 0,]
questionario2016 <- questionario2016_1
rm(questionario2016_1)
questionario2017_1 <- questionario2017[! questionario2017$Spesa %in% 0,]
questionario2017 <- questionario2017_1
rm(questionario2017_1)

# Ripeto le analisi già fatto con i nuovi dati
# rmf::frequenze(questionario201x$Provenienza)
# mean(questionario201x$Spesa)
# by(questionario201x$Spesa, questionario201x$Provenienza, mean)

# 2015
rmf::frequenze(questionario2015$Provenienza)
# x 
#   x      n            f  
# +---+------+------------+ 
# | 1 |  781 |  56.924198 | 
# | 2 |  521 |  37.973761 | 
# | 3 |   70 |   5.102041 | 
# +---+------+------------+ 
#       1372   100.000000   
# Osservazioni mancanti: 0 
mean(questionario2015$Spesa)
# [1] 8.012391
by(questionario2015$Spesa, questionario2015$Provenienza, mean)
# questionario2015$Provenienza: 1
# [1] 5.820102
# ------------------------------------------------------------------------------------ 
# questionario2015$Provenienza: 2
# [1] 9.884837
# ------------------------------------------------------------------------------------ 
# questionario2015$Provenienza: 3
# [1] 18.53571

# 2016
rmf::frequenze(questionario2016$Provenienza)
# x 
#   x     n            f  
# +---+-----+------------+ 
# | 1 | 167 |  34.221311 | 
# | 2 | 274 |  56.147541 | 
# | 3 |  47 |   9.631148 | 
# +---+-----+------------+ 
#       488   100.000000   
# Osservazioni mancanti: 0 
mean(questionario2016$Spesa)
# [1] 13.10963
by(questionario2016$Spesa, questionario2016$Provenienza, mean)
# questionario2016$Provenienza: 1
# [1] 10.06587
# ------------------------------------------------------------------------------------ 
# questionario2016$Provenienza: 2
# [1] 15.01277
# ------------------------------------------------------------------------------------ 
# questionario2016$Provenienza: 3
# [1] 12.82979

# 2017
rmf::frequenze(questionario2017$Provenienza)
# x 
#   x     n           f  
# +---+-----+-----------+ 
# | 1 |  64 |  21.33333 | 
# | 2 | 209 |  69.66667 | 
# | 3 |  27 |   9.00000 | 
# +---+-----+-----------+ 
#       300   100.00000   
# Osservazioni mancanti: 0 
mean(questionario2017$Spesa)
# [1] 15.37333
by(questionario2017$Spesa, questionario2017$Provenienza, mean)
# questionario2017$Provenienza: 1
# [1] 9.648438
# ------------------------------------------------------------------------------------ 
# questionario2017$Provenienza: 2
# [1] 17.29665
# ------------------------------------------------------------------------------------ 
# questionario2017$Provenienza: 3
# [1] 14.05556

# Grafico Spesa media pro capite
barplot(c(mean(questionario2015$Spesa), mean(questionario2016$Spesa), mean(questionario2017$Spesa)), main="Spesa media pro capite", names.arg = c("2015", "2016", "2017"), ylab = "spesa", ylim = c(0, 20), col = "green")

# Grafico di spese medie pro capite per provenienza per anno, raggruppato per anno
# Vettori delle medie per categoria, uno per ogni anno
mediePerProvenienza2015 <- c(mean(questionario2015[which(questionario2015$Provenienza == 1),]$Spesa), mean(questionario2015[which(questionario2015$Provenienza == 2),]$Spesa), mean(questionario2015[which(questionario2015$Provenienza == 3),]$Spesa))
mediePerProvenienza2016 <- c(mean(questionario2016[which(questionario2016$Provenienza == 1),]$Spesa), mean(questionario2016[which(questionario2016$Provenienza == 2),]$Spesa), mean(questionario2016[which(questionario2016$Provenienza == 3),]$Spesa))
mediePerProvenienza2017 <- c(mean(questionario2017[which(questionario2017$Provenienza == 1),]$Spesa), mean(questionario2017[which(questionario2017$Provenienza == 2),]$Spesa), mean(questionario2017[which(questionario2017$Provenienza == 3),]$Spesa))

# Ne creo una matrice
dati <- matrix(c(mediePerProvenienza2015, mediePerProvenienza2016, mediePerProvenienza2017), ncol  = 3)
colnames(dati) <- c("2015", "2016", "2017")
rownames(dati) <- c("Provincia di Cuneo", "Italia", "Estero")

# Grafico
barplot(dati, main="Spesa pro capite per provenienza", ylab = "spesa", col=c("green","blue","red"), beside = T, border = "white", legend = rownames(dati), args.legend = list(x=9), ylim = c(0,30))
rm(mediePerProvenienza2015, mediePerProvenienza2016, mediePerProvenienza2017, dati)

# Grafico Spesa relativa degli intervistati per provenienza
# Vettori delle somma della spesa per categoria, uno per ogni anno
sommaSpesaPerProvenienza2015 <- c(sum(questionario2015[which(questionario2015$Provenienza == 1),]$Spesa), sum(questionario2015[which(questionario2015$Provenienza == 2),]$Spesa), sum(questionario2015[which(questionario2015$Provenienza == 3),]$Spesa))
sommaSpesaPerProvenienza2016 <- c(sum(questionario2016[which(questionario2016$Provenienza == 1),]$Spesa), sum(questionario2016[which(questionario2016$Provenienza == 2),]$Spesa), sum(questionario2016[which(questionario2016$Provenienza == 3),]$Spesa))
sommaSpesaPerProvenienza2017 <- c(sum(questionario2017[which(questionario2017$Provenienza == 1),]$Spesa), sum(questionario2017[which(questionario2017$Provenienza == 2),]$Spesa), sum(questionario2017[which(questionario2017$Provenienza == 3),]$Spesa))

# Ne creo una matrice
dati <- matrix(c(sommaSpesaPerProvenienza2015, sommaSpesaPerProvenienza2016, sommaSpesaPerProvenienza2017), ncol  = 3)
colnames(dati) <- c("2015", "2016", "2017")
rownames(dati) <- c("Provincia di Cuneo", "Italia", "Estero")

# La somma delle spese sono molto diverse, le trasformo in percentuali e poi plotto
datiPercentuali <- apply(dati, 2, function(x){x*100/sum(x)})
barplot(datiPercentuali, main="Spesa relativa degli intervistati per provenienza", ylab = "spesa", col=c("green","blue","red"), border = "white", legend = rownames(dati), args.legend = list(x=9))
rm(dati, datiPercentuali, sommaSpesaPerProvenienza2015, sommaSpesaPerProvenienza2016, sommaSpesaPerProvenienza2017)

# Dopo un ricontrollo della correttezza della lettura dei dati, ripeto i calcoli che coinvolgono i risultati del 2017
questionario2017_1 <- read.table("Magnifica_Questionario2017_1.txt", header = TRUE, sep = ",")
questionario2017_2 <- questionario2017_1[! (is.na(questionario2017_1$Spesa) | is.na(questionario2017_1$Provenienza)),]
questionario2017 <- questionario2017_2
rm(questionario2017_1, questionario2017_2)

rmf::frequenze(questionario2017$Provenienza)
# x 
#   x     n            f  
# +---+-----+------------+ 
# | 1 |  76 |  24.050633 | 
# | 2 | 212 |  67.088608 | 
# | 3 |  28 |   8.860759 | 
# +---+-----+------------+ 
#       316   100.000000   
# Osservazioni mancanti: 0 
mean(questionario2017$Spesa)
# [1] 13.36551
by(questionario2017$Spesa, questionario2017$Provenienza, mean)
# questionario2017$Provenienza: 1
# [1] 8.263158
# ------------------------------------------------------------------------ 
# questionario2017$Provenienza: 2
# [1] 15.21934
# ------------------------------------------------------------------------ 
# questionario2017$Provenienza: 3
# [1] 13.17857

# IN CARTELLA ho rinominato i questionari in RisultatiQuestionarioSpesa201x.txt

# Grafici della provenienza
par(mfrow=c(2,2))
pie(table(questionario2015$Provenienza), col = c("green","blue","red") ,main="Provenienza 2015", labels = c("","",""))
pie(table(questionario2016$Provenienza), col = c("green","blue","red") ,main="Provenienza 2016", labels = c("","",""))
pie(table(questionario2017$Provenienza), col = c("green","blue","red") ,main="Provenienza 2017", labels = c("","",""))

# Grafico spesa media per anno
mediePerProvenienza2015 <- c(mean(questionario2015[which(questionario2015$Provenienza == 1),]$Spesa), mean(questionario2015[which(questionario2015$Provenienza == 2),]$Spesa), mean(questionario2015[which(questionario2015$Provenienza == 3),]$Spesa))
mediePerProvenienza2016 <- c(mean(questionario2016[which(questionario2016$Provenienza == 1),]$Spesa), mean(questionario2016[which(questionario2016$Provenienza == 2),]$Spesa), mean(questionario2016[which(questionario2016$Provenienza == 3),]$Spesa))
mediePerProvenienza2017 <- c(mean(questionario2017[which(questionario2017$Provenienza == 1),]$Spesa), mean(questionario2017[which(questionario2017$Provenienza == 2),]$Spesa), mean(questionario2017[which(questionario2017$Provenienza == 3),]$Spesa))
medie <- matrix(c(mediePerProvenienza2015, mediePerProvenienza2016, mediePerProvenienza2017), nrow = 3)
plot(medie[1,], type="o", col="green", ylim=c(0,20), main="Spesa media per anno", ylab = "spesa", xlab="anno")
lines(medie[2,], type="o", col="blue")
lines(medie[3,], type="o", col="red")


# """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


# NUOVI DATI QUESTIONARI 2017
# Da verificare con i dati inseriti nei questionari online
> questionario2017_1 <- read.table("RisultatiQuestionarioSpesa2017.txt", header = TRUE, sep = ",")
> questionario2017_2 <- questionario2017_1[! (is.na(questionario2017_1$Spesa) | is.na(questionario2017_1$Provenienza)),]
> questionario2017 <- questionario2017_2
> rm(questionario2017_1, questionario2017_2)
> rmf::frequenze(questionario2017$Provenienza)
# x 
#   x     n            f  
# +---+-----+------------+ 
# | 1 | 147 |  30.371901 | 
# | 2 | 306 |  63.223140 | 
# | 3 |  31 |   6.404959 | 
# +---+-----+------------+ 
#       484   100.000000   
# Osservazioni mancanti: 0 
> mean(questionario2017$Spesa)
# [1] 13.35331
> by(questionario2017$Spesa, questionario2017$Provenienza, mean)
# questionario2017$Provenienza: 1
# [1] 8.241497
# --------------------------------- 
# questionario2017$Provenienza: 2
# [1] 15.92157
# --------------------------------- 
# questionario2017$Provenienza: 3
# [1] 12.24194


# """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


# GGPLOT2
library(ggplot2)
library(reshape2)

# Grafico di spese medie pro capite per provenienza per anno, raggruppato per anno
# Vettori delle medie per categoria, uno per ogni anno
mediePerProvenienza2015 <- c(mean(questionario2015[which(questionario2015$Provenienza == 1),]$Spesa), mean(questionario2015[which(questionario2015$Provenienza == 2),]$Spesa), mean(questionario2015[which(questionario2015$Provenienza == 3),]$Spesa))
mediePerProvenienza2016 <- c(mean(questionario2016[which(questionario2016$Provenienza == 1),]$Spesa), mean(questionario2016[which(questionario2016$Provenienza == 2),]$Spesa), mean(questionario2016[which(questionario2016$Provenienza == 3),]$Spesa))
mediePerProvenienza2017 <- c(mean(questionario2017[which(questionario2017$Provenienza == 1),]$Spesa), mean(questionario2017[which(questionario2017$Provenienza == 2),]$Spesa), mean(questionario2017[which(questionario2017$Provenienza == 3),]$Spesa))

# Ne creo un dataframe
provenienza <- c("Province of Cuneo", "Rest of Italy", "Foreigners")
data <- data.frame(mediePerProvenienza2015, mediePerProvenienza2016, mediePerProvenienza2017, provenienza)
colnames(data) <- c("2015", "2016", "2017", "Provenienza")
data.m <- melt(data, id.vars = "Provenienza")
colnames(data.m) <- c("Provenienza", "Anno", "Spesa")
data.m$Spesa <- round (data.m$Spesa, 2)
data.m$Provenienza <- factor(data.m$Provenienza, c("Province of Cuneo", "Rest of Italy", "Foreigners"))

# Grafico senza il titolo
ggplot(data.m, aes(Anno, Spesa, fill = Provenienza)) + 
	geom_bar(position = "dodge", stat ="identity") + 
	labs(x = "Year", y = expression(paste("Average expenditure"))) +
	theme(text = element_text(size = 15)) +
	guides(fill=guide_legend(title="Provenience of visitors")) +
	ylim(c(0,22.5)) + 
	geom_text(aes(label=paste(Spesa, "\u20ac", sep = " ")), vjust=1.6, position = position_dodge(0.9), size=3.5) + 
	scale_fill_brewer(palette = "Paired")


# Grafico con il titolo
# ggplot(data.m, aes(Anno, Spesa, fill = Provenienza)) + 
#	geom_bar(position = "dodge", stat ="identity") + 
#	ggtitle("Per capita expenditure") +
#	labs(x = "Year", y = expression(paste("Average expenditure"))) +
#	theme(plot.title = element_text(size=15, margin = margin(10, 0, 15, 0), hjust = 0.5), text = element_text(size = 15)) +
#	guides(fill=guide_legend(title="Provenience of visitors")) +
#	ylim(c(0,22.5)) + 
#	geom_text(aes(label=paste(Spesa, "\u20ac", sep = " ")), vjust=1.6, position = position_dodge(0.9), size=3.5) +
#	scale_fill_brewer(palette = "Paired")


rm(data, data.m, provenienza, mediePerProvenienza2015, mediePerProvenienza2017, mediePerProvenienza2016)


# """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


# Grafico a torta

# GGPLOT2
library(ggplot2)
library(reshape2)

mediePerProvenienza2015 <- c(mean(questionario2015[which(questionario2015$Provenienza == 1),]$Spesa), mean(questionario2015[which(questionario2015$Provenienza == 2),]$Spesa), mean(questionario2015[which(questionario2015$Provenienza == 3),]$Spesa))
mediePerProvenienza2016 <- c(mean(questionario2016[which(questionario2016$Provenienza == 1),]$Spesa), mean(questionario2016[which(questionario2016$Provenienza == 2),]$Spesa), mean(questionario2016[which(questionario2016$Provenienza == 3),]$Spesa))
mediePerProvenienza2017 <- c(mean(questionario2017[which(questionario2017$Provenienza == 1),]$Spesa), mean(questionario2017[which(questionario2017$Provenienza == 2),]$Spesa), mean(questionario2017[which(questionario2017$Provenienza == 3),]$Spesa))

# IMPATTO

provenienza <- c("Province of Cuneo", "Rest of Italy", "Foreigners")
impatto = c(sum(questionario2015$Provenienza == 1)*mediePerProvenienza2015[1] + sum(questionario2016$Provenienza == 1)*mediePerProvenienza2016[1] + sum(questionario2017$Provenienza == 1)*mediePerProvenienza2017[1],
+               sum(questionario2015$Provenienza == 2)* mediePerProvenienza2015[2] + sum(questionario2016$Provenienza == 2)* mediePerProvenienza2016[2] + sum(questionario2017$Provenienza == 2)* mediePerProvenienza2017[2],
+               sum(questionario2015$Provenienza == 3)*mediePerProvenienza2015[3] + sum(questionario2016$Provenienza == 3)*mediePerProvenienza2016[3] + sum(questionario2017$Provenienza == 3)*mediePerProvenienza2017[3])
impattoPercentuale <- round(impatto*100/sum(impatto), 1)
df <- data.frame(provenienza, impattoPercentuale)
df$provenienza <- factor(df$provenienza, c("Province of Cuneo", "Rest of Italy", "Foreigners"))
blank_theme <- theme_minimal()+
  theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.border = element_blank(),
  panel.grid=element_blank(),
  axis.ticks = element_blank(),
  plot.title=element_text(size=14, face="bold")
  )
graficoImpatto <- ggplot(df, aes (x = "", y = impattoPercentuale, fill = provenienza)) + 
	geom_bar(width = 1, stat = "identity") + 
	coord_polar("y", start=0) + 
	scale_fill_brewer(palette = "Paired") + 
	blank_theme + 
	theme(axis.text.x = element_blank(), text = element_text(size = 15)) +
	geom_text(aes(label =paste(impattoPercentuale,"%")), position = position_stack(vjust = 0.5)) +
	guides(fill=guide_legend(title="Provenience of visitors")) 


# """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


#PROVENIENZA

# GGPLOT2
library(ggplot2)
library(reshape2)

provenienza <- c("Province of Cuneo", "Rest of Italy", "Foreigners")
provenienti <- c(sum(questionario2015$Provenienza == 1) + sum(questionario2016$Provenienza == 1) + sum(questionario2017$Provenienza == 1),
		 sum(questionario2015$Provenienza == 2) + sum(questionario2016$Provenienza == 2) + sum(questionario2017$Provenienza == 2),
		 sum(questionario2015$Provenienza == 3) + sum(questionario2016$Provenienza == 3) + sum(questionario2017$Provenienza == 3))
provenientiPercentuale <- round(c(provenienti[1]/sum(provenienti),
				  provenienti[2]/sum(provenienti),
				  provenienti[3]/sum(provenienti))*100, 1)

df$provenienza <- factor(df$provenienza, c("Province of Cuneo", "Rest of Italy", "Foreigners"))
blank_theme <- theme_minimal()+
  theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.border = element_blank(),
  panel.grid=element_blank(),
  axis.ticks = element_blank(),
  plot.title=element_text(size=14, face="bold")
  )
graficoProvenienza <- ggplot(df, aes (x = "", y = provenientiPercentuale, fill = provenienza)) + 
	geom_bar(width = 1, stat = "identity") + 
	coord_polar("y", start=0) + 
	scale_fill_brewer(palette = "Paired") + 
	blank_theme + 
	theme(axis.text.x = element_blank(), text = element_text(size = 15)) +
	geom_text(aes(label =paste(provenientiPercentuale,"%")), position = position_stack(vjust = 0.5)) +
	guides(fill=guide_legend(title="Provenience of visitors"))


# """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


#COMBINO I GRAFICI
library(grid)
library(gridExtra)
library(cowplot)

graficoImpatto <- graficoImpatto + theme(plot.margin = unit(c(6,0,6,0), "pt"))
graficoProvenienza <- graficoProvenienza + theme(plot.margin = unit(c(6,0,6,0), "pt"))

prow <- plot_grid(graficoProvenienza + theme(legend.position = "non2"), graficoImpatto + theme(legend.position = "none"), align = 'vh', labels = c("4", "5"), hjust = -1, nrow = 1)
legenda <- get_legend(graficoImpatto + theme(legend.position = "bottom"))
grafico <- plot_grid(prow, legenda, ncol = 1, rel_heights = c(1,.2))
grafico

rm(blank_theme, df, grafico, graficoImpatto, graficoProvenienza, legenda, prow, impatto, impattoPercentuale, mediePerProvenienza2015, mediePerProvenienza2016, mediePerProvenienza2017, provenienti, provenientiPercentuale, provenienza)

# Copiate a risoluzione 750x350

# CITAZIONE
# http://blog.apastyle.org/apastyle/2015/01/how-to-cite-software-in-apa-style.html

# ggplot2
# H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2009.
# R Core Team (2017). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

# https://stackoverflow.com/questions/15688758/r-stats-citation-for-a-scientific-paper
# Wickham, H. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2009.

# google
# Wickham, H. (2010). ggplot2: elegant graphics for data analysis. J Stat Softw, 35(1), 65-88.

# https://stat.ethz.ch/pipermail/r-help/2008-May/161481.html
# R Development Core Team (2008). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. ISBN 3-900051-07-0, URL http://www.R-project.org.

# MARGINE DI ERRORE
questionariSpesa <- c(questionario2015$Spesa, questionario2016$Spesa, questionario2017$Spesa)
mean(questionariSpesa)
sdcorretto <- sd(questionariSpesa)/sqrt(length(questionariSpesa)-1)*sqrt(1-length(questionariSpesa)/50000)
1.96*sdcorretto/sqrt(length(questionariSpesa))

questionariSpesa <- c(questionario2015$Spesa, questionario2016$Spesa, questionario2017$Spesa)
media <- mean(questionariSpesa)
numerositaPopolazione <- 59640
# Z (95%) = 1.96, la formula è x*deviazioneStandard/radice(NumerositàCampione)*radice((NumerositàPopolazione-NumerositàCampione)/(NumerositàPopolazione-1))
erroreCampionario <- 1.96*sd(questionariSpesa)/sqrt(length(questionariSpesa))*sqrt((numerositaPopolazione-length(questionariSpesa))/(numerositaPopolazione-1))
margineDiErrore <- ((media-erroreCampionario)-media)/(media-erroreCampionario)
margineDiErrore
rm(margineDiErrore, erroreCampionario, questionariSpesa, media, numerositaPopolazione)

# con output
questionariSpesa <- c(questionario2015$Spesa, questionario2016$Spesa, questionario2017$Spesa)
length(questionariSpesa)
numerositaPopolazione <- 59640
numerositaPopolazione
# Z (95%) = 1.96, la formula è x*deviazioneStandard/radice(NumerositàCampione)*radice((NumerositàPopolazione-NumerositàCampione)/(NumerositàPopolazione-1))
erroreCampionario <- 1.96*sd(questionariSpesa)/sqrt(length(questionariSpesa))*sqrt((numerositaPopolazione-length(questionariSpesa))/(numerositaPopolazione-1))
erroreCampionario
margineDiErrore <- ((media-erroreCampionario)-media)/(media-erroreCampionario)
margineDiErrore
rm(margineDiErrore, erroreCampionario, questionariSpesa, media, numerositaPopolazione)

# con output 7/02
questionariSpesa <- c(questionario2015$Spesa, questionario2016$Spesa, questionario2017$Spesa)
length(questionariSpesa)
media <- mean(questionariSpesa)
# Z (95%) = 1.96, la formula è x*deviazioneStandard/radice(NumerositàCampione)
erroreCampionario <- 1.96*sd(questionariSpesa)/sqrt(length(questionariSpesa))
erroreCampionario
margineDiErrore <- (media - (media-erroreCampionario))/(media)
margineDiErrore
margineDiErrore <- (media - (media+erroreCampionario))/(media)
margineDiErrore
rm(margineDiErrore, erroreCampionario, questionariSpesa, media)
