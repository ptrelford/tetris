namespace Tetris

type App() as app =
    inherit System.Windows.Application()
    let main = new Game.GameControl()
    do  app.Startup.Add(fun _ -> app.RootVisual <- main)        