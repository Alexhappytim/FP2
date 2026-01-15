namespace BinaryDict

module BinaryDict =
    type private Tree<'K,'V> =
        | Empty
        | Node of left:Tree<'K,'V> * key:'K * value:'V * right:Tree<'K,'V>
    type Dict<'K,'V> = private {
        tree: Tree<'K,'V>
    }

    let empty<'K,'V when 'K : comparison> () : Dict<'K,'V> = { tree = Empty }

    let isEmpty (d: Dict<'K,'V>) =
        match d.tree with Empty -> true | _ -> false

    let count (d: Dict<'K,'V>) =
        let rec loop acc t =
            match t with
            | Empty -> acc
            | Node(l,_,_,r) -> loop (acc+1) l |> loop <| r
        loop 0 d.tree

    let tryFind k (d: Dict<'K,'V>) =
        let rec go t =
            match t with
            | Empty -> None
            | Node(l,kk,v,r) ->
                if k = kk then Some v
                elif k < kk then go l
                else go r
        go d.tree

    let containsKey k d =
        match tryFind k d with None -> false | Some _ -> true

    let add k v (d: Dict<'K,'V>) =
        let rec ins t =
            match t with
            | Empty -> Node(Empty, k, v, Empty)
            | Node(l,kk,vv,r) ->
                if k = kk then Node(l, kk, v, r)
                elif k < kk then Node(ins l, kk, vv, r)
                else Node(l, kk, vv, ins r)
        { tree = ins d.tree }

    let private popMin t =
        let rec go acc =
            match acc with
            | Empty -> failwith "Empty tree"
            | Node(Empty, k, v, r) -> (k, v, r)
            | Node(l, k, v, r) ->
                let (mk,mv,rest) = go l
                (mk,mv, Node(rest, k, v, r))
        go t

    let remove k (d: Dict<'K,'V>) =
        let rec del t =
            match t with
            | Empty -> Empty
            | Node(l,kk,vv,r) ->
                if k = kk then
                    match l, r with
                    | Empty, _ -> r
                    | _, Empty -> l
                    | _, _ ->
                        let (mk,mv,rest) = popMin r
                        Node(l, mk, mv, rest)
                elif k < kk then Node(del l, kk, vv, r)
                else Node(l, kk, vv, del r)
        { tree = del d.tree }

    /// Fold left (in-order ascending by key)
    let foldLeft f (state:'S) (d: Dict<'K,'V>) : 'S =
        let rec go s t =
            match t with
            | Empty -> s
            | Node(l,k,v,r) ->
                let s' = go s l
                let s'' = f s' k v
                go s'' r
        go state d.tree

    /// Fold right (in-order descending by key)
    let foldRight f (state:'S) (d: Dict<'K,'V>) : 'S =
        let rec go s t =
            match t with
            | Empty -> s
            | Node(l,k,v,r) ->
                let s' = go s r
                let s'' = f k v s'
                go s'' l
        go state d.tree

    /// Map over values, preserving keys and ordering
    let mapValues (f:'V->'U) (d: Dict<'K,'V>) : Dict<'K,'U> =
        let rec go t =
            match t with
            | Empty -> Empty
            | Node(l,k,v,r) -> Node(go l, k, f v, go r)
        { tree = go d.tree }

    /// Filter by predicate on key and value
    let filter (pred:'K->'V->bool) (d: Dict<'K,'V>) : Dict<'K,'V> =
        let rec go t =
            match t with
            | Empty -> Empty
            | Node(l,k,v,r) ->
                let l' = go l
                let r' = go r
                if pred k v then Node(l', k, v, r')
                else
                    // remove this node by merging children
                    match l', r' with
                    | Empty, _ -> r'
                    | _, Empty -> l'
                    | _, _ ->
                        let (mk,mv,rest) = popMin r'
                        Node(l', mk, mv, rest)
        { tree = go d.tree }

    /// Convert to sequence of key-value pairs (in-order ascending)
    let toSeq (d: Dict<'K,'V>) : seq<'K * 'V> =
        let rec loop t = seq {
            match t with
            | Empty -> ()
            | Node(l,k,v,r) ->
                yield! loop l
                yield (k,v)
                yield! loop r
        }
        loop d.tree

    /// Efficient equality: sizes must match and every key must map to equal value
    let equals (a: Dict<'K,'V>) (b: Dict<'K,'V>) =
        if count a <> count b then false
        else
            foldLeft (fun ok k v -> ok && (tryFind k b = Some v)) true a

    /// Build dictionary from a sequence, last win on duplicate keys
    let ofSeq (items: seq<'K*'V>) : Dict<'K,'V> =
        let rec addAll d itr =
            match itr with
            | [] -> d
            | (k,v)::xs -> addAll (add k v d) xs
        items |> Seq.toList |> addAll (empty())

    /// Value monoid specification used for dictionary union
    type ValueMonoid<'V> = { empty: 'V; append: 'V -> 'V -> 'V }

    /// Monoid empty element for dictionaries
    let emptyMonoid<'K,'V when 'K : comparison> () : Dict<'K,'V> = empty<'K,'V>()

    /// Monoid append: union of dictionaries, second dictionary values win on duplicate keys
    /// [1,2] + [1,1] = [1,1]
    let appendWith (m: ValueMonoid<'V>) (a: Dict<'K,'V>) (b: Dict<'K,'V>) : Dict<'K,'V> =
        // Start with all of b's entries, then add entries from a only if key doesn't exist
        foldLeft (fun acc k v ->
            match tryFind k acc with
            | None -> add k v acc
            | Some _ -> acc  // key exists in b, keep b's value
        ) b a




    /// Keys
    let keys<'K,'V when 'K : comparison> (d: Dict<'K,'V>) = d |> toSeq |> Seq.map fst

    /// Values
    let values<'K,'V when 'K : comparison> (d: Dict<'K,'V>) = d |> toSeq |> Seq.map snd

    /// Fold over pairs as list for test convenience
    let toList<'K,'V when 'K : comparison> (d: Dict<'K,'V>) = d |> toSeq |> Seq.toList
