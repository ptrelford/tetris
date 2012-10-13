﻿[<AutoOpen>]
module Threading

open System
 
let synchronize f =
    let ctx = System.Threading.SynchronizationContext.Current 
    f (fun g arg ->
        let nctx = System.Threading.SynchronizationContext.Current 
        if ctx <> null && ctx <> nctx then ctx.Post((fun _ -> g(arg)), null)
        else g(arg) )

type Microsoft.FSharp.Control.Async with 
    static member AwaitObservable(ev1:System.IObservable<'a>) =
        synchronize (fun f ->
        Async.FromContinuations((fun (cont,econt,ccont) -> 
            let rec callback = (fun value ->
                remover.Dispose()
                f cont value )
            and remover : IDisposable  = ev1.Subscribe(callback) 
            () )))
    static member AwaitEvent(ev1:System.IObservable<'a>) =
        Microsoft.FSharp.Control.Async.AwaitObservable ev1