## Тимошков Александр Андреевич ФП2
### ТЗ
Реализовать словарь на бинарном дереве поиска. Структура должна иметь интерфейс словаря, иметь следующий функционал:
- добавление и удаление элементов;
- фильтрация;
- отображение (map);
- свертки (левая и правая);
- структура должна быть моноидом.
- структура должа быть полиморфна
Так же требуется сделать юнит и проперти бейзд тесты. 
### Реализация

```fsharp
type private Tree<'K, 'V> =
    | Empty
    | Node of left: Tree<'K, 'V> * key: 'K * value: 'V * right: Tree<'K, 'V>

type Dict<'K, 'V> = private { tree: Tree<'K, 'V> }
```

Словарь реализован как обертка над бинарным деревом поиска. Дерево может быть пустым (`Empty`) или узлом с левым и правым поддеревом, ключом и значением.

#### Поиск элемента
```fsharp
let tryFind k (d: Dict<'K, 'V>) =
    let rec go t =
        match t with
        | Empty -> None
        | Node(l, kk, v, r) ->
            if k = kk then Some v
            elif k < kk then go l
            else go r
```
Рекурсивный спуск: если ключ меньше текущего — идем налево, если больше — направо.

#### Добавление элемента
```fsharp
let add k v (d: Dict<'K, 'V>) =
    let rec ins t =
        match t with
        | Empty -> Node(Empty, k, v, Empty)
        | Node(l, kk, vv, r) ->
            if k = kk then Node(l, kk, v, r)  // замена значения
            elif k < kk then Node(ins l, kk, vv, r)
            else Node(l, kk, vv, ins r)
```
При совпадении ключа — обновляет значение, иначе рекурсивно добавляет в нужное поддерево.

#### Удаление элемента
```fsharp
let remove k (d: Dict<'K, 'V>) =
    let rec del t =
        match t with
        | Node(l, kk, vv, r) when k = kk ->
            match l, r with
            | Empty, _ -> r
            | _, Empty -> l
            | _, _ ->
                let (mk, mv, rest) = popMin r  
                Node(l, mk, mv, rest)
```
При удалении узла с двумя детьми заменяет его минимальным элементом из правого поддерева.

#### Свертки (fold)
```fsharp
let foldLeft f (state: 'S) (d: Dict<'K, 'V>) : 'S =
    let rec go s t =
        match t with
        | Empty -> s
        | Node(l, k, v, r) ->
            let s' = go s l        
            let s'' = f s' k v     
            go s'' r               
```
Левая свертка обходит дерево в порядке возрастания ключей.

#### Фильтрация
```fsharp
let filter (pred: 'K -> 'V -> bool) (d: Dict<'K, 'V>) : Dict<'K, 'V> =
    let rec go t =
        match t with
        | Node(l, k, v, r) ->
            let l' = go l
            let r' = go r
            if pred k v then Node(l', k, v, r')
            else
                match l', r' with
                | Empty, _ -> r'
                | _, Empty -> l'
                | _, _ -> let (mk, mv, rest) = popMin r'
                          Node(l', mk, mv, rest)
```
Рекурсивная фильтрация с восстановлением структуры дерева.

#### Операция appendWith с моноидом
```fsharp
[<NoComparison; NoEquality>]
type ValueMonoid<'V> = { empty: 'V; append: 'V -> 'V -> 'V }

let appendWith (m: ValueMonoid<'V>) (a: Dict<'K, 'V>) (b: Dict<'K, 'V>) =
    foldLeft
        (fun acc k v ->
            match tryFind k acc with
            | None -> add k v acc
            | Some _ -> acc)
        b
        a
```
Объединяет два словаря, используя структуру моноида для значений.


### Вывод
Выполняя данную лабораторную работу я научился делать собственные структуры данных на F#. Из особенно интересного для меня было открытие property-based тестирования, библиотека которая по "описанию" структуры сама генерирует тесты.