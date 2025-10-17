# Лабораторная работа №2 — `rb-dict`

---

- **Студент:** `Мироненко Артём`
- **Группа:** `P3331`
- **Университет:** `ИТМО`
- **Курс:** `Функциональное программирование`
- **Язык:** `OCaml`
- **Лицензия:** `MIT`
- **Репозиторий:** [tteemma/fp-lab2](https://github.com/tteemma/fp-lab2)

---

## Иммутабельный словарь (Red–Black Tree Dict)

Функционально-полиморфная библиотека ассоциативного массива (словаря),
реализованная поверх самобалансирующегося **красно-чёрного дерева**.  
Структура данных неизменяема, поддерживает основные операции коллекций,
свёртки, `map`/`filter`, моноидальную операцию объединения,
а также эффективное сравнение на уровне API.

---

### Структура репозитория

- `lib/rb_tree.ml` — реализация красно-чёрного дерева
- `lib/rb_dict.ml` — реализация словаря на основе дерева
- `lib/labwork2.ml`, `lib/labwork2.mli` — публичный модуль, реэкспорт `RBDict`
- `test/test_labwork2.ml` — unit и property-based тесты (`OUnit2` + `QCheck`)
- `bin/main.ml` — пример использования CLI
- `dune-project`, `lib/dune`, `test/dune`, `bin/dune` — сборка через Dune
- `.github/workflows/` — CI (сборка, тесты, покрытие)

---

## Требования и реализованный функционал

- **Функциональные:**

  - добавление / удаление элементов (`add`, `remove`);
  - фильтрация (`filter`);
  - отображение (`map`);
  - свёртки (левая / правая: `fold_left`, `fold_right`);
  - структура является **моноидом** (`empty` — нейтральный элемент, `union` — ассоциативная операция);
  - полиморфизм по значениям и параметризация по типу ключа (через `OrderedType`).

- **Нефункциональные:**
  - полная **иммутабельность**;
  - разделение интерфейса / реализации (`.mli` / `.ml`);
  - тестирование только через публичный API;
  - эффективное сравнение деревьев без конверсии в списки;
  - стиль — идиоматичный для OCaml, сборка и тестирование через `Dune`.

---

## 📘 Ключевые элементы реализации

### Интерфейс параметризуемого словаря (`lib/rb_dict.mli`)

```ocaml
module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type key
  type +'a t
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val remove : key -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val find_opt : key -> 'a t -> 'a option
  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val fold_left : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val fold_right : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val union : 'a t -> 'a t -> 'a t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

module Make (Ord : OrderedType) : S with type key = Ord.t
```

---

### Внутренняя структура — Красно-Чёрное дерево (`lib/rb_tree.ml`)

```ocaml
type color = R | B

type ('k, 'v) node =
  | E
  | T of color * ('k,'v) node * 'k * 'v * ('k,'v) node
```

---

### Вставка элемента с балансировкой

```ocaml
let rec insert cmp key value = function
  | E -> T (R, E, key, value, E)
  | T (color, left, k, v, right) as node ->
      let c = cmp key k in
      if c < 0 then balance (T (color, insert cmp key value left, k, v, right))
      else if c > 0 then balance (T (color, left, k, v, insert cmp key value right))
      else T (color, left, key, value, right)
```

---

### Балансировка дерева

```ocaml
let balance = function
  | B, T (R, T (R, a, xk, xv, b), yk, yv, c), zk, zv, d
  | B, T (R, a, xk, xv, T (R, b, yk, yv, c)), zk, zv, d
  | B, a, xk, xv, T (R, T (R, b, yk, yv, c), zk, zv, d)
  | B, a, xk, xv, T (R, b, yk, yv, T (R, c, zk, zv, d)) ->
      T (R, T (B, a, xk, xv, b), yk, yv, T (B, c, zk, zv, d))
  | color, a, xk, xv, b -> T (color, a, xk, xv, b)
```

---

### Поиск значения по ключу

```ocaml
let rec lookup cmp x = function
  | E -> None
  | T (_, a, y, v, b) ->
      let c = cmp x y in
      if c < 0 then lookup cmp x a
      else if c > 0 then lookup cmp x b
      else Some v
```

---

### Пример использования

```ocaml
open Labwork2
module IntOrd = struct type t = int let compare = Stdlib.compare end
module Dict = RBDict.Make(IntOrd)

let d =
  Dict.empty
  |> Dict.add 1 "one"
  |> Dict.add 2 "two"
  |> Dict.add 3 "three"

let sum = Dict.fold_left (fun acc _ _ -> acc + 1) 0 d
let only_even = Dict.filter (fun k _ -> k mod 2 = 0) d
```

---

## Тестирование

- Unit-тесты: вставка / поиск, удаление, `map` / `filter`, свёртки.
- Property-based тесты: 3+ свойств (включая моноид):
  - левый нейтральный: `union empty x == x`
  - правый нейтральный: `union x empty == x`
  - ассоциативность: `union (union x y) z == union x (union y z)`
  - композиция `map`

Запуск локально:

```bash
opam exec -- dune runtest
```

#### Пример вывода тестов

```
Testing `Labwork2 Dict'.
This run has ID `YGQK0ZPX'.

  [OK]          unit                0   insert & find.
  [OK]          unit                1   remove.
  [OK]          unit                2   map & filter.
  [OK]          unit                3   folds.
  [OK]          properties          0   append left identity.
  [OK]          properties          1   append right identity.
  [OK]          properties          2   append associativity.
  [OK]          properties          3   map composition.

Full test results in _build/_tests/Labwork2 Dict.
Test Successful in 0.02s. 8 tests run.
```

---

## Покрытие кода

Покрытие тестами публикуется автоматически на **GitHub Pages**:  
👉 [Отчёт о покрытии (GitHub Pages)](https://tteemma.github.io/fp-lab2/)

---

## CI/CD

- `main.yml` — сборка и тестирование при каждом push / PR
- `coverage.yml` — публикация HTML-отчёта покрытия на GitHub Pages

| Workflow      | Статус                                                                                   |
| ------------- | ---------------------------------------------------------------------------------------- |
| Build & Tests | ![CI](https://github.com/tteemma/fp-lab2/actions/workflows/main.yml/badge.svg)           |
| Coverage      | ![Coverage](https://github.com/tteemma/fp-lab2/actions/workflows/coverage.yml/badge.svg) |

---

## Выводы

- Использование функторов позволяет обобщить реализацию по типу ключей.
- Красно-чёрное дерево гарантирует `O(log n)` на `add` / `remove` / `find`.
- Реализация иммутабельна, операции не изменяют существующее состояние.
- Property-based тесты проверяют алгебраические законы (моноид, композиция).
- Разделение интерфейса и реализации делает код устойчивым к изменениям.

---

**© 2025 Мироненко Артём — Университет ИТМО**
