[<RequireQualifiedAccess>]
module TreeSet

    type TreeSet<'a> = TreeSet of RedBlackTree.RedBlackTree<'a>

    let empty<'a when 'a : comparison> = TreeSet(RedBlackTree.empty<'a>)

    let add (x: 'a) (set: TreeSet<'a>) : TreeSet<'a> =
        let (TreeSet tree) = set
        TreeSet(RedBlackTree.add x tree)

    let singleton (x: 'a) : TreeSet<'a> = add x empty

    let remove (x: 'a) (set: TreeSet<'a>) : TreeSet<'a> =
        let (TreeSet tree) = set
        TreeSet(RedBlackTree.remove x tree)

    let isEmpty (set: TreeSet<'a>) : bool =
        let (TreeSet tree) = set
        RedBlackTree.isEmpty tree

    let count (set: TreeSet<'a>) : int =
        let (TreeSet tree) = set
        RedBlackTree.size tree

    let contains (x: 'a) (set: TreeSet<'a>) : bool =
        let (TreeSet tree) = set
        RedBlackTree.contains x tree

    let exists (f: 'a -> bool) (set: TreeSet<'a>) : bool =
        let (TreeSet tree) = set
        RedBlackTree.exists f tree

    let forall (f: 'a -> bool) (set: TreeSet<'a>) : bool =
        let (TreeSet tree) = set
        RedBlackTree.forall f tree

    let fold (f: 'b -> 'a -> 'b) (z: 'b) (set: TreeSet<'a>) : 'b =
        let (TreeSet tree) = set
        RedBlackTree.fold f z tree

    let foldBack (f: 'a -> 'b -> 'b) (z: 'b) (set: TreeSet<'a>) : 'b =
        let (TreeSet tree) = set
        RedBlackTree.foldBack f z tree

    let filter (f: 'a -> bool) (set: TreeSet<'a>) : TreeSet<'a> =
        fold (fun acc x -> if f x then add x acc else acc) empty set

    let map (f: 'a -> 'b) (set: TreeSet<'a>) : TreeSet<'b> =
        fold (fun acc x -> add (f x) acc) empty set

    let union (set1: TreeSet<'a>) (set2: TreeSet<'a>) : TreeSet<'a> =
        fold (fun acc x -> add x acc) empty set1

    let difference (set1: TreeSet<'a>) (set2: TreeSet<'a>) : TreeSet<'a> =
        fold (fun acc x -> if not (contains x set2) then add x acc else acc) empty set1

    let intersect (set1: TreeSet<'a>) (set2: TreeSet<'a>) : TreeSet<'a> =
        fold (fun acc x -> if (contains x set2) then add x acc else acc) empty set1

    let ofList xs =
        let rec ofList' acc ys =
            match ys with
            | [] -> acc
            | z :: zs ->
                ofList' (add z acc) zs
        ofList' empty xs