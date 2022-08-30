[<RequireQualifiedAccess>]
module RedBlackTree

    type Color = R | B | BB
    type RedBlackTree<'a> = E | EE | T of Color * RedBlackTree<'a> * 'a * RedBlackTree<'a>

    let empty<'a when 'a : comparison> : RedBlackTree<'a> = E

    let add (x: 'a when 'a : comparison) (tree: RedBlackTree<'a>) : RedBlackTree<'a> =
        let lbalance t =
            match t with
            | T(B, T(R, T(R, a, x, b), y, c), z, d) -> T(R, T(B, a, x, b), y, T(B, c, z, d))
            | T(B, T(R, a, x, T(R, b, y, c)), z, d) -> T(R, T(B, a, x, b), y, T(B, c, z, d))
            | _ -> t
        let rbalance t =
            match t with
            | T(B, a, x, T(R, T(R, b, y, c), z, d)) -> T(R, T(B, a, x, b), y, T(B, c, z, d))
            | T(B, a, x, T(R, b, y, T(R, c, z, d))) -> T(R, T(B, a, x, b), y, T(B, c, z, d))
            | _ -> t
        let rec add' x t =
            match t with
            | E -> T(R, E, x, E)
            | T(color, a, y, b) when x < y ->
                lbalance (T(color, (add' x a), y, b))
            | T(color, a, y, b) when x > y ->
                rbalance (T(color, a, y, (add' x b)))
            | _ -> t
        let blacken t =
            match t with
            | T(R, T(R, a, x, b), y, c) -> T(B, T(R, a, x, b), y, c)
            | T(R, a, x, T(R, b, y, c)) -> T(B, a, x, T(R, b, y, c))
            | _ -> t
        blacken (add' x tree)

    let remove (x: 'a when 'a : comparison) (tree: RedBlackTree<'a>) : RedBlackTree<'a> =
        let balance t =
            match t with
            | T(BB, a, x, T(R, T(R, b, y, c), z, d)) -> T(B, T(B, a, x, b), y, T(B, c, z, d))
            | T(BB, T(R, a, x, T(R, b, y, c)), z, d) -> T(B, T(B, a, x, b), y, T(B, c, z, d))
            | _ -> t
        let redden t =
            match t with
            | T(B, T(B, a, x, b), y, T(B, c, z, d)) -> T(R, T(B, a, x, b), y, T(B, c, z, d))
            | EE -> E
            | _ -> t
        let rotate t =
            match t with
            | T(R, T(BB, a, x, b), y, T(B, c, z, d)) -> balance (T(B, T(R, T(B, a, x, b), y, c), z, d))
            | T(R, EE, y, T(B, c, z, d)) -> balance(T(B, T(R, E, y, c), z, d))
            | T(R, T(B, a, x, b), y, T(BB, c, z, d)) -> balance (T(B, a, x, T(R, b, y, T(B, c, z, d))))
            | T(R, T(B, a, x, b), y, EE) -> balance (T(B, a, x, T(R, b, y, E)))
            | T(B, T(BB, a, x, b), y, T(B, c, z, d)) -> balance (T(BB, T(R, T(B, a, x, b), y, c), z, d))
            | T(B, EE, y, T(B, c, z, d)) -> balance (T(BB, T(R, E, y, c), z, d))
            | T(B, T(B, a, x, b), y, T(BB, c, z, d)) -> balance (T(BB, a, x, T(R, b, y, T(B, c, z, d))))
            | T(B, T(B, a, x, b), y, EE) -> balance (T(BB, a, x, T(R, b, y, E)))
            | T(B, T(BB, a, w, b), x, T(R, T(B, c, y, d), z, e)) -> T(B, balance (T(B, T(R, T(B, a, w, b), x, c), y, d)), z, e)
            | T(B, EE, x, T(R, T(B, c, y, d), z, e)) -> T(B, balance (T(B, T(R, E, x, c), y, d)), z, e)
            | T(B, T(R, a, w, T(B, b, x, c)), y, T(BB, d, z, e)) -> T(B, a, w, balance (T(B, b, x, T(R, c, y, T(B, d, z, e)))))
            | T(B, T(R, a, w, T(B, b, x, c)), y, EE) -> T(B, a, w, balance (T(B, b, x, T(R, c, y, E))))
            | _ -> t
        let rec removeMin t =
            match t with
                | T(R, E, x, E) -> (x, E)
                | T(B, E, x, E) -> (x, EE)
                | T(B, E, x, T(R, E, y, E)) -> (x, T(B, E, y, E))
                | T(c, a, x, b) ->
                    let (z, e) = removeMin a
                    (z, rotate (T(c, e, x, b)))
        let rec remove' t =
            match t with
            | E -> E
            | T(R, E, y, E) when x = y -> E
            | T(R, E, y, E) when x <> y -> t
            | T(B, E, y, E) when x = y -> EE
            | T(B, E, y, E) when x <> y -> t
            | T(B, T(R, E, y, E), z, E) when x < z -> T(B, remove' (T(R, E, y, E)), z, E)
            | T(B, T(R, E, y, E), z, E) when x = z -> T(B, E, y, E)
            | T(B, T(R, E, y, E), z, E) when x > z -> T(B, T(R, E, y, E), z, E)
            | T(c, a, y, b) when x < y ->
                rotate (T(c, remove' a, y, b))
            | T(c, a, y, b) when x = y ->
                let (z, e) = removeMin b
                rotate (T(c, a, z, e))
            | T(c, a, y, b) when x > y ->
                rotate (T(c, a, y, remove' b))
        redden (remove' tree)

    let isEmpty (tree: RedBlackTree<'a>) : bool =
        match tree with
        | T(_) -> false
        | _ -> true

    let rec height (tree: RedBlackTree<'a>) : int =
        match tree with
        | T(_, a, _ ,b) -> 1 + max (height a) (height b)
        | _ -> 0

    let rec size (tree: RedBlackTree<'a>) : int =
        match tree with
        | T(_, a, _, b) -> 1 + (size a) + (size b)
        | _ -> 0

    let rec contains<'a when 'a : comparison> (x: 'a) (tree: RedBlackTree<'a>) : bool =
        match tree with
        | T(_, a, y, b) ->
            if x < y then
                contains x a
            elif x > y then
                contains x b
            else
                true
        | _ -> false

    let rec exists (f: 'a -> bool) (tree: RedBlackTree<'a>) : bool =
        match tree with
        | T(_, a, x, b) -> f x || exists f a || exists f b
        | _ -> false

    let rec forall (f: 'a -> bool) (tree: RedBlackTree<'a>) : bool =
        match tree with
        | T(_, a, x, b) -> f x && forall f a && forall f b
        | _ -> true

    let rec fold (f: 'b -> 'a -> 'b) (z: 'b) (tree: RedBlackTree<'a>) : 'b =
        match tree with
        | T(_, a, x, b) -> fold f (f (fold f z a) x) b
        | _ -> z

    let rec foldBack (f: 'a -> 'b -> 'b) (z: 'b) (tree: RedBlackTree<'a>) : 'b =
        match tree with
        | T(_, a, x, b) -> foldBack f (f x (foldBack f z b)) a
        | _ -> z

    let rec minElement<'a when 'a : comparison> (tree: RedBlackTree<'a>) : 'a option =
        match tree with
        | T(_, E, x, _) -> Some x
        | T(_, EE, x, _) -> Some x
        | T(_, a, _, _) -> minElement a
        | _ -> None

    let rec maxElement<'a when 'a : comparison> (tree: RedBlackTree<'a>) : 'a option =
        match tree with
        | T(_, _, x, E) -> Some x
        | T(_, _, x, EE) -> Some x
        | T(_, _, _, b) -> maxElement b
        | _ -> None