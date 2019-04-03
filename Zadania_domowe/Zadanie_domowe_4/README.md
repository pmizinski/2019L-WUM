# Zadanie domowe 4

Zadanie dotyczy algorytmu Support Vector Machine.
Będzie omówiona na wykładzie, można też przeczytać
http://pyml.sourceforge.net/doc/howto.pdf

Wykorzystaj dwa zbiory danych:

  - apartments z R-owego pakietu DALEX,
  - dowolny, wybrany przez siebie zbiór danych (najlepiej z większą liczbą zmiennych numerycznych niż apartments).

1. Dopasuj SVM do obu zbiorów danych.
2. Sprawdź, czy zalinkowany artykuł słusznie zwraca uwagę na skalowanie danych.
3. Przeczytaj, jakie są najważniejsze hiperparametry SVM. Spróbuj je zoptymalizować metodą random search. Najprościej optymalizować hiperparametry w SVM z jądrem gaussowskim, ale można też poszukać najlepszego jądra (-;
4. Wykorzystaj pakiet DALEX (lub Skater/sklearn ensemble), by narysować PDP/ALE plot dla dopasowanych modeli przed i po optymalizacji hiperparametrów. Porównaj z dowolnym modelem drzewiastym dopasowanym na tych samych danych.

BONUS: zastosuj optymalizację bayesowską w punkcie 3.

W razie pytań zapraszam do kontaktu.

Termin oddania: 16 IV 2019 r.
