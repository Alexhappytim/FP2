module Tests

open Xunit
open FsCheck.Xunit
open BinaryDict

type IntMonoid = BinaryDict.ValueMonoid<int>

[<Fact>]
let ``add and find work`` () =
    let d = BinaryDict.empty<int, int> () |> BinaryDict.add 1 10
    Assert.Equal(Some 10, BinaryDict.tryFind 1 d)
    Assert.Equal(None, BinaryDict.tryFind 2 d)

[<Fact>]
let ``remove deletes key`` () =
    let d =
        BinaryDict.empty<int, string> ()
        |> BinaryDict.add 2 "two"
        |> BinaryDict.add 1 "one"
        |> BinaryDict.add 3 "three"

    let d' = BinaryDict.remove 2 d
    Assert.True(BinaryDict.containsKey 2 d)
    Assert.False(BinaryDict.containsKey 2 d')
    Assert.True(BinaryDict.containsKey 1 d')
    Assert.True(BinaryDict.containsKey 3 d')

[<Fact>]
let ``mapValues preserves keys`` () =
    let d = BinaryDict.ofSeq [ 1, 2; 2, 3; 3, 4 ]
    let d2 = BinaryDict.mapValues (fun v -> v * 10) d
    Assert.Equal<int>([ 1; 2; 3 ] |> Seq.ofList, BinaryDict.keys d2)
    Assert.Equal<int>([ 20; 30; 40 ] |> Seq.ofList, BinaryDict.values d2)

[<Fact>]
let ``filter keeps predicate-satisfying pairs`` () =
    let d = BinaryDict.ofSeq [ 1, 2; 2, 5; 3, 1; 4, 8 ]
    let d2 = BinaryDict.filter (fun k v -> k % 2 = 0 && v > 3) d
    Assert.Equal<int>([ 2; 4 ] |> Seq.ofList, BinaryDict.keys d2)
    Assert.Equal<int>([ 5; 8 ] |> Seq.ofList, BinaryDict.values d2)

[<Fact>]
let ``folds compute sums`` () =
    let d = BinaryDict.ofSeq [ for i in 1..10 -> (i, i) ]
    let sumL = BinaryDict.foldLeft (fun s _ v -> s + v) 0 d
    let sumR = BinaryDict.foldRight (fun _ v s -> v + s) 0 d
    Assert.Equal(55, sumL)
    Assert.Equal(55, sumR)

[<Fact>]
let ``equals ignores insertion order`` () =
    let pairs = [ 3, "three"; 1, "one"; 2, "two" ]
    let d1 = BinaryDict.ofSeq pairs
    let d2 = BinaryDict.ofSeq (List.rev pairs)
    Assert.True(BinaryDict.equals d1 d2)

let private intAddMonoid: IntMonoid = { empty = 0; append = (+) }

[<Fact>]
let ``appendWith empty a with nonempty b`` () =
    let a = BinaryDict.empty<int, int> ()
    let b = BinaryDict.ofSeq [ (0, 0) ]
    let result = BinaryDict.appendWith intAddMonoid a b
    Assert.Equal<int * int>([ (0, 0) ], BinaryDict.toList result)

[<Fact>]
let ``appendWith nonempty a with empty b`` () =
    let a = BinaryDict.ofSeq [ (0, 0) ]
    let b = BinaryDict.empty<int, int> ()
    let result = BinaryDict.appendWith intAddMonoid a b
    Assert.Equal<int * int>([ (0, 0) ], BinaryDict.toList result)

[<Fact>]
let ``appendWith associativity with same keys`` () =
    let a = BinaryDict.ofSeq [ (1, 1) ]
    let b = BinaryDict.ofSeq [ (1, 2) ]
    let c = BinaryDict.ofSeq [ (1, 3) ]

    let left =
        BinaryDict.appendWith intAddMonoid (BinaryDict.appendWith intAddMonoid a b) c

    let right =
        BinaryDict.appendWith intAddMonoid a (BinaryDict.appendWith intAddMonoid b c)

    Assert.True(BinaryDict.equals left right)

[<Property>]
let ``Monoid identity: empty is neutral`` (xs: list<int * int>) =
    let a = BinaryDict.ofSeq xs
    let e = BinaryDict.empty<int, int> ()

    BinaryDict.equals (BinaryDict.appendWith intAddMonoid a e) a
    && BinaryDict.equals (BinaryDict.appendWith intAddMonoid e a) a

[<Property>]
let ``Monoid associativity`` (xs: list<int * int>, ys: list<int * int>, zs: list<int * int>) =
    let a = BinaryDict.ofSeq xs
    let b = BinaryDict.ofSeq ys
    let c = BinaryDict.ofSeq zs

    let left =
        BinaryDict.appendWith intAddMonoid (BinaryDict.appendWith intAddMonoid a b) c

    let right =
        BinaryDict.appendWith intAddMonoid a (BinaryDict.appendWith intAddMonoid b c)

    BinaryDict.equals left right

[<Property>]
let ``mapValues transforms values and preserves keys`` (xs: list<int * int>, f: int -> int) =
    let d = BinaryDict.ofSeq xs
    let d2 = BinaryDict.mapValues f d
    let keys1 = BinaryDict.keys d |> Seq.toList
    let keys2 = BinaryDict.keys d2 |> Seq.toList

    keys1 = keys2
    && (keys1
        |> List.forall (fun k -> BinaryDict.tryFind k d2 = (BinaryDict.tryFind k d |> Option.map f)))

[<Property>]
let ``filter by even keys keeps subset`` (xs: list<int * int>) =
    let d = BinaryDict.ofSeq xs
    let d2 = BinaryDict.filter (fun k _ -> k % 2 = 0) d
    let keys2 = BinaryDict.keys d2 |> Set.ofSeq

    let expected =
        xs |> Seq.choose (fun (k, _) -> if k % 2 = 0 then Some k else None) |> Set.ofSeq

    keys2 = expected
