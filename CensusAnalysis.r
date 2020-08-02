# Età popolazione
barplot(colSums(localita[,28:43]))

# Popolazione residente che si sposta giornalmente nel o al di fuori del comune di dimora abituale
> colonne <- c(sum(localita$P137), sum(localita$P138))
> barplot(colonne,
+         main="Popolazione residente che si sposta giornalmente",
+         ylab = "popolazione",
+         col=c("darkblue","red"),
+         legend.text = c("nel comune", "al di fuori del comune"),
+         args.legend = list(x=1.15, y=1000))
