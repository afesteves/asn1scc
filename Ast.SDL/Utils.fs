module Utils
open FSharp.Reflection

let print x = printfn "%A" x

let cons x xs = x :: xs

let partitionsWith baseCase recursion xs = List.fold recursion baseCase xs

let partitions2 xs = xs |> partitionsWith ([], []) (fun (a,b) -> 
    function
    | Choice1Of2 y -> (y :: a, b)
    | Choice2Of2 y -> (a, y :: b)
  )

let partitions3 xs = xs |> partitionsWith ([], [], []) (fun (a,b,c) -> 
    function
    | Choice1Of3 y -> (y :: a, b, c)
    | Choice2Of3 y -> (a, y :: b, c)
    | Choice3Of3 y -> (a, b, y :: c)
  )

let partitions4 xs = xs |> partitionsWith ([], [], [], []) (fun (a,b,c,d) -> 
    function
    | Choice1Of4 y -> (y :: a, b, c, d)
    | Choice2Of4 y -> (a, y :: b, c, d)
    | Choice3Of4 y -> (a, b, y :: c, d)
    | Choice4Of4 y -> (a, b, c, y :: d)
  )

let partitions5 xs = xs |> partitionsWith ([], [], [], [], []) (fun (a,b,c,d,e) -> 
    function
    | Choice1Of5 y -> (y :: a, b, c, d, e)
    | Choice2Of5 y -> (a, y :: b, c, d, e)
    | Choice3Of5 y -> (a, b, y :: c, d, e)
    | Choice4Of5 y -> (a, b, c, y :: d, e)
    | Choice5Of5 y -> (a, b, c, d, y :: e)
  )

let partitions6 xs = xs |> partitionsWith ([], [], [], [], [], []) (fun (a,b,c,d,e,f) -> 
    function
    | Choice1Of6 y -> (y :: a, b, c, d, e, f)
    | Choice2Of6 y -> (a, y :: b, c, d, e, f)
    | Choice3Of6 y -> (a, b, y :: c, d, e, f)
    | Choice4Of6 y -> (a, b, c, y :: d, e, f)
    | Choice5Of6 y -> (a, b, c, d, y :: e, f)
    | Choice6Of6 y -> (a, b, c, d, e, y :: f)
  )

let partitions7 xs = xs |> partitionsWith ([], [], [], [], [], [], []) (fun (a,b,c,d,e,f,g) -> 
    function
    | Choice1Of7 y -> (y :: a, b, c, d, e, f, g)
    | Choice2Of7 y -> (a, y :: b, c, d, e, f, g)
    | Choice3Of7 y -> (a, b, y :: c, d, e, f, g)
    | Choice4Of7 y -> (a, b, c, y :: d, e, f, g)
    | Choice5Of7 y -> (a, b, c, d, y :: e, f, g)
    | Choice6Of7 y -> (a, b, c, d, e, y :: f, g)
    | Choice7Of7 y -> (a, b, c, d, e, f, y :: g)
  )
