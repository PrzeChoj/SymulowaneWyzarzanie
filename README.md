# Symulowane Wyzarzanie

Repozytorium projektu na przedmiot Algorytmiczne Zastosowania Łańcuchów Markowa

## Cele projektu

1. Zaimplementowanie algorytmu Symulowanego Wyżarzania do szukania maksimum funkcji, której dziedziną jest zbiór permutacji
2. Przetestowanie działania algotymu na prostej funkcji testowej
3. Przetestowanie różnych metod obniżania temperatury
4. Przetestowanie najlepiej działającej wersji na funkcji celu z pakietu `gips`

## Co zrobimy

1. Odpalenie na funkcji celu z `gips`'a to tylko showcase na koniec
2. Implementacja symulowanego wyżarzania
3. Testowanie dla różnych metod powiększania bety
4. Rożne warunki stopu
   - gdy acceptance rate dla danej temperatury okaże się odpowiednio mały,
   - gdy kilka razy z rzędu acceptance rate będzie niski,
   - gdy dodatkowo kilka razy z rzędu nie zmieni się wynikowa permutacja,
5. Sprawdzić, czy zgodnie z oczekiwaniem znajduje maxima lokalne
6. Czy algorytm będzie znajdował tak samo często maxy dla tych, które są kierowane przez strukturę jak i te bez sugestii? Np. wybrać losową permutację p-elementową i powiedzieć, że ona jest posortowana. Jaka jest szansa, że on w nią trafi? Jak to się zmiena z p? Jak to się zmienia z n?
7. Sprawdzić jak się zmienia acceptance rate dla kolejnych temperatur, jak się zmienia wartość funkcji dla wybranych przy kolejnych temperaturach permutacjach + można testować to dla różnych doborów bety (Ad 3)

Deadline:
03.06.2022 r.
