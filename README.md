Kazik
======
Kazik jest zbiorem skryptów napisanych w R, które mają następujące zadania:

1. Pobierają z serwera Kognilabu dane z przeprowadzanych na nim badań ankietowych.
 - dane w postaci surowej zapisywane są do folderu ``(data)/rawData/`` )


2. Oczyszczają je tak, żeby były bardziej czytelne zarówno dla komputera jak i dla człowieka.
 - dane w postaci oczyszczonej zapisywane są w głownym katalogu z nazwą ``clean(czyje badanie)(numer ankiety).csv``


3. Generują za pomocą pakietu *rmarkdown* raporty z badań.
 - raporty z badań zapisywane są w folderze `raports` i mają rozszerzenie `.html`.
 - w folderze `raports` znajduje się również plik `info.html` w którym znaleźć można dane dotyczące tego jakie ankiety aktualnie Kazik obserwuje oraz ilu mają uczestników.
 
