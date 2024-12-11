//Halfway worked my way through this blogpost in haskell: https://mrmr.io/memoization-in-haskell
type State<'s,'a> = 's -> 'a * 's

module State =
    let get () : State<'s,'s> = fun s -> s,s
    let put (s : 's) : State<'s,unit> = fun _ -> (), s

    //applicative apply : M (a -> b) -> M a -> M b
    let pure' (a:'a) : State<'s,'a> = fun s -> a,s
    let apply (sf : State<'s, 'a->'b>) (sa : State<'s, 'a>) : State<'s, 'b> =
        fun s ->
            let f, s1 = sf s
            let a, s2 = sa s1
            f a, s2
    let (<*>) = apply
    let (<!>) f a = (pure' f) <*> a
    
    //monad
    let return' = pure'
    let bind (a : State<'s,'a>) (f : 'a -> State<'s,'b>) : State<'s,'b> =
        (fun s -> let x, ss = a s
                  (f x) ss)
           
    type Builder() =
        member x.Bind(state, func) = bind state func
        member x.Return(value) = return' value
        member x.ReturnFrom(value) = value
        
    let state = Builder()

open State

let memo (n : 'a) compute : State<Map<'a,'b>,'b> =
    state {
        let! cache = get ()
        match cache |> Map.tryFind n with
        | Some result -> return result
        | None ->
            let! b = compute n
            let! cache = get ()
            do! put (cache |> Map.add n b)
            return b
    }

let rec fib n =
    memo n (fun n ->
    state {
        match n with
        | 1 -> return 1UL
        | 2 -> return 1UL
        | _ -> return! (+) <!> (fib (n - 1)) <*> (fib (n - 2))    
    })
    
fib 4_000 Map.empty |> fst