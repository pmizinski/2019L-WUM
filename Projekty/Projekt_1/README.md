# Projekt 1: klasyfikacja

Proponowane tematy:

Propozycje tematów z zeszłych lat: https://docs.google.com/document/d/1kTyfdKqY1N65mhAEY2Ondh0tS7W046suiFzgxj0vVkM/edit?usp=sharing

Nowe propozycje tematów: 

  1. Skoring kredytowy
  
  - https://archive.ics.uci.edu/ml/datasets/Statlog+%28German+Credit+Data%29 - klasyczne dane niemieckie. Ciekawy artykuł opisujący dwa skrajne podejścia i pewne typowe metody: https://www.researchgate.net/publication/315581050_On_the_Practical_Relevance_of_Modern_Machine_Learning_Algorithms_for_Credit_Scoring_Applications
  - https://www.kaggle.com/c/home-credit-default-risk - konkurs Kaggle
  - https://community.fico.com/s/explainable-machine-learning-challenge - FICO challenge (credit scoring + interpretability) - dane na życzenie
  2. Sprawiedliwość (fairness) w ML
  - https://www.kaggle.com/danofer/compass - obciążenie modeli przewidujących recydywistów
  - https://www.kaggle.com/new-york-state/nys-recidivism-beginning-2008 - recydywiści w NY
  - (nieaktualne) https://github.com/dtroupe18/Predicting-Recidivism-and-Analysis-of-National-Corrections-Data-1991-2014 - kolejny zbiór danych (ew. https://www.icpsr.umich.edu/icpsrweb/NACJD/studies/3355/datadocumentation, nie trzeba pracować na całych danych) 




Dla użytkowników knitra:

```
date: "`r format(Sys.time(), '%d - %m - %Y')`"
output:
  html_document:
    df_print: paged
    toc: true
    toc_float: true
    code_folding: hide
    number_sections: true
```

Dla osób, które wybrały COMPAS, opis zmiennych:
https://github.com/pseshadri18/compas-recidivism/blob/master/compas_report.Rmd
