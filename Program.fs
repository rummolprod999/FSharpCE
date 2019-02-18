namespace ParserFsharp
open System

type LoggingBuilder() =
    let log p = printfn "expression is %A" p

    member this.Bind(x, f) = 
        log x
        f x

    member this.Return(x) = 
        x

type ListBuilder() =
    member this.Bind(m, f) =
        printfn "Bind fun execute %A" m
        m |> List.collect f

    member this.Zero() = 
        printfn "Zero"
        []

    member this.Return(x) = 
        printfn "Return an unwrapped %A as a list" x
        [x]

    member this.Yield(x) = 
        printfn "Yield an unwrapped %A as a list" x
        [x]
        
    member this.For(m,f) =
        printfn "For %A" m
        this.Bind(m, f)
 
 type TraceBuilderWhile() =
    member this.Bind(m, f) = 
        match m with 
        | None -> 
            printfn "Binding with None. Exiting."
        | Some a -> 
            printfn "Binding with Some(%A). Continuing" a
        Option.bind f m

    member this.Return(x) = 
        Some x

    member this.ReturnFrom(x) = 
        x
        
    member this.Zero() = 
        printfn "Zero"
        //this.Return ()
        None

    member this.Delay(f) = 
        printfn "Delay"
        f

    member this.Run(f) = 
        f()

    member this.While(guard, body) =
        printfn "While: test"
        if not (guard()) 
        then 
            printfn "While: zero"
            this.Zero() 
        else
            printfn "While: body"
            this.Bind( body(), fun () -> 
                this.While(guard, body))

type TraceBuilder() =
    member this.Bind(m, f) = 
        match m with 
        | None -> 
            printfn "Binding with None. Exiting."
        | Some a -> 
            printfn "Binding with Some(%A). Continuing" a
        Option.bind f m

    member this.Return(x) = 
        printfn "Return an unwrapped %A as an option" x
        Some x

    member this.Zero() = 
        printfn "Zero"
        None

    member this.Combine (m, f) = 
        printfn "Combine. Starting second param %A" f
        let y = f()
        printfn "Combine. Finished second param %A. Result is %A" f y

        match (m, y) with
        | Some a, Some b ->
            printfn "combining %A and %A" a b 
            Some (a + b)
        | Some a, None ->
            printfn "combining %A with None" a 
            Some a
        | None, Some b ->
            printfn "combining None with %A" b 
            Some b
        | None, None ->
            printfn "combining None with None"
            None

    member this.Delay(funcToDelay) = 
        let delayed = fun () ->
            printfn "%A - Starting Delayed Fn." funcToDelay
            let delayedResult = funcToDelay()
            printfn "%A - Finished Delayed Fn. Result is %A" funcToDelay delayedResult
            delayedResult
        printfn "%A - Delaying using %A" funcToDelay delayed
        delayed 

    member this.Run(funcToRun) = 
        printfn "%A - Run Start." funcToRun
        let runResult = funcToRun()
        printfn "%A - Run End. Result is %A" funcToRun runResult
        runResult

type OptionBuilder() =

    member this.Bind(x, f) =
        printfn "this.Bind: %A" x
        match x with
        | None -> None
        | Some v -> f v

    member this.Return(x) = Some x 

module EntryPoint =
            
        [<EntryPoint>]
        let main argv =
            let trace = TraceBuilder()
            let m = trace { 
                            printfn "Part 1: about to return 1"
                            //return 1
                            if true then
                                let! x = Some 1
                                printfn "inside if branch"
                            else printfn "inside else branch"
                            printfn "Part 2: after return has happened"
                            printfn "AAAAAAAAAAAAAAA"
                            return 2
                            } 
            let optM = OptionBuilder()
            let n = optM { let x = 56
                           let! y = Some 78
                           let! z = None
                           let! v = Some 42
                           return x+y+v }
            printfn "%A" n
            let myBools = [true; false; true]
            let resB = myBools |> List.filter id |> List.length |> (%) 2 <> 0
            printfn "%A" resB
            let A = array2D [[1;2;3];[4;5;6];[7;8;9]]
            printfn "Full matrix:\n %A" A
            let row0 = A.[0,*]
            printfn "Row 0: %A" row0
            let col0 = A.[*,0]
            printfn "Column 0: %A" col0
            let subA = A.[*,0..1]
            printfn "%A" subA
            let subA' = A.[0..1,*]
            printfn "%A" subA'
            0 