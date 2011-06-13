namespace Tetris

open System.Windows

type App() as app =
    inherit Application()
    let main = new Game.WellControl()
    do 
        app.Startup.Add(fun _ -> app.RootVisual <- main)
        //app.Exit.Add(fun _ -> (main :> System.IDisposable).Dispose())