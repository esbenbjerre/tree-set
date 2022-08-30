#load "RedBlackTree.fs"
#load "TreeSet.fs"

#time
let xs = [1..10000] |> Set.ofList |> Set.isEmpty
#time

#time
let ys = [1..10000] |> TreeSet.ofList |> TreeSet.isEmpty
#time