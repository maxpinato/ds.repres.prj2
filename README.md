#Data Science - Reproducible Research
Lo scopo di questo progetto è verificare le conoscenze di KNITR

##Appunti
Ho utilizzato sandbox.R per fare i test preparatori per la realizzazione del documento.
Sostanzialmente le domande sono semplici.
Nel primo caso, la risposta è semplice.
Guardo le fatalities e TORNADO è il vincitore.
Ho voluto mostrare i primi 5 eventi.

###23.09.2016
Non mi piaceva la visualizzazione delle label (di fatto si sovrapponevano).
Ho utilizzato la funzione [abbreviate](https://stat.ethz.ch/R-manual/R-devel/library/base/html/abbreviate.html) 
```
abbreviate(name,minlength)
```
Veramente molto utile.
Ad ogni modo, per completare la verifica relativamente ai più pericolosi devo aggiungere anche injurious.
Appunto per dopo ... i costi sono contenuti in 

###28.09.2016
Ho trovato un ottima pubblicazione da parte di uno studente che mi ha ispirato.
In particolare rispetto alla mia realizzazione, ottima l'idea di utilizzare
la clusterizzazione per raggruppare gli eventi simili.
Non sono convintissimo dell'offset di taglio dell'albero che ho scelto, perchè in alcuni casi mi raggruppa mele con pere, ma nonostante sia alto comunque non riesce a raggruppare alcuni eventi che si assomigliano (è abbastanza chiaro nella top-5 dei costi).
Ad ogni modo, è stato il primo utilizzo che ho fatto della clusterizzazione.
Sostanzialmente i passi sono i seguenti:
- Prendo un elenco di stringhe
- Ne calcolo la distanza `stringdistmatrix(s1,s2,method)` 
- Calcolo l'albero `hclust(as.dist(distancematrix))`
- Lo taglio ad una certa altezza `cutree(albero,h=altezza)`
Tutte le altre istruzioni sono per comodità (lo trasformo sostanzialmente in un data.frame con cui mi è più facile lavorare grazie a dplyr).
