﻿(*[omit:Skip module definition on TryFSharp.org]*)
#if INTERACTIVE
#else
module Game
#endif
(*[/omit]*)

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open System.Windows.Media
open System.Windows.Shapes

(*[omit:Keyboard input]*)
type KeyState (control:Control) =
    let mutable keysDown = Set.empty
    let mutable keyUps = List.empty
    let addKey key () = keyUps <- key :: keyUps
    let readKeyUps key () =
        let ofKey, otherKeys = 
            keyUps |> List.partition ((=) key)
        keyUps <- otherKeys
        List.length ofKey
    let sync = obj()
    do  control.KeyDown.Add (fun e ->
            keysDown <- keysDown.Add e.Key
        )
    do  control.KeyUp.Add (fun e -> 
            keysDown <- keysDown.Remove e.Key
            lock sync (e.Key |> addKey)
        )        
    member this.IsKeyDown key = keysDown.Contains key
    member this.IsAnyKeyDwn () = keysDown.Count > 0
    member this.ReadKeyPressed key =
        let keyUps = lock sync (key |> readKeyUps)
        keyUps > 0
    member this.ReadKeyPresses key =
        let keyUps = lock sync (key |> readKeyUps)
        keyUps + (if keysDown.Contains key then 1 else 0)
(*[/omit]*)

let tetrads =
    [
        [0,0;0,1;0,2;0,3],Colors.Red, Colors.Yellow
        [0,0;1,0;0,1;1,1],Colors.Blue, Colors.Cyan
        [0,0;1,0;2,0;1,1],Colors.Purple, Colors.Magenta
        [0,0;1,0;2,0;0,1],Colors.Yellow, Colors.Orange
        [0,0;1,0;2,0;2,1],Colors.White, Colors.LightGray
        [0,0;1,0;1,1;2,1],Colors.Green, Colors.Gray
        [0,1;1,1;1,0;2,0],Colors.Brown, Colors.DarkGray
    ]

type Block = { X:int; Y:int; Rectangle:Rectangle }
type Tetrad = { Blocks:Block list; Canvas:Canvas }

let setPosition (block:#UIElement) (x,y) =    
    block.SetValue(Canvas.LeftProperty, x)
    block.SetValue(Canvas.TopProperty, y)

let blockSize = 16.0
let toPosition (x,y) = float x * blockSize, float y * blockSize

let positionBlock block =
    (block.X, block.Y) |> toPosition |> setPosition block.Rectangle

let positionBlocks blocks = 
    blocks |> List.iter positionBlock

let positionTetrad tetrad (x,y) =
    (x,y) |> toPosition |> setPosition tetrad.Canvas
    
let createTetrad (coordinates,stroke,fill) =    
    let createRectangle () =
        Rectangle(
            Width=blockSize,Height=blockSize,
            Fill=SolidColorBrush fill,
            Stroke=SolidColorBrush stroke,
            StrokeThickness=2.0)    
    let createBlocks coordinates =
        coordinates |> List.map (fun (x,y) ->
            let rectangle = createRectangle ()        
            { X=x; Y=y; Rectangle=rectangle }
        )
    let composeBlocks blocks =
        let canvas = new Canvas()
        blocks |> List.iter (fun block ->            
            canvas.Children.Add block.Rectangle
        )
        canvas
    let blocks = createBlocks coordinates
    positionBlocks blocks
    let canvas = composeBlocks blocks
    { Blocks=blocks; Canvas=canvas }

let wellWidth, wellHeight = 10, 20

type Well() =
    let canvas = Canvas() 
    let matrix = Array2D.create wellWidth wellHeight None
    let addBlock (x,y) block =
        matrix.[x,y] <- Some block
        canvas.Children.Add block
    let clear () =
        matrix |> Array2D.iteri (fun x y block ->
            block |> Option.iter (fun block ->
                canvas.Children.Remove block |> ignore
                matrix.[x,y] <- None
            )
        )        
    let isBlocked (x,y) =
        if x < 0 || x >= wellWidth then true
        elif y < 0 || y >= wellHeight then true
        else
            matrix.[x,y] |> Option.exists (fun x -> true)
    let clearLines () =
        let lineBlockCounts =
            [0..wellHeight-1] |> List.map (fun y ->
                [0..wellWidth-1] 
                |> List.map (fun x -> matrix.[x,y])
                |> List.map Option.count 
                |> List.reduce (+), y
            )
        let clearLine y =
            for x = 0 to wellWidth-1 do
                matrix.[x,y] |> Option.iter (fun block -> 
                    canvas.Children.Remove block |> ignore)
                matrix.[x,y] <- None
        let fallDownTo y =
            for i = y-1 downto 1 do
                for x = 0 to wellWidth-1 do
                    let block = matrix.[x,i]                        
                    block |> Option.iter (fun block ->                         
                        setPosition block (toPosition (x,i+1))
                        matrix.[x,i+1] <- Some block
                        matrix.[x,i] <- None
                    )
        let cleared =
            lineBlockCounts 
            |> List.filter (fun (count,_) -> count = wellWidth)
        cleared |> List.iter (fun (_,y) ->
            clearLine y
            fallDownTo y             
        )
        cleared |> List.length
    member well.IsBlocked = isBlocked
    member well.AddBlock (x,y) (block:UIElement) = addBlock (x,y) block         
    member well.ClearLines () = clearLines ()
    member well.Clear () = clear ()
    member well.Control = canvas

type GameControl() as this =
    inherit UserControl(
            Width = float wellWidth*blockSize, 
            Height = float wellHeight*blockSize,
            IsTabStop = true)

    let settings = 
        System.IO.IsolatedStorage.IsolatedStorageSettings.ApplicationSettings
    let keys = KeyState(this)    
    let well = Well()           
    let canvas = Canvas(Background=SolidColorBrush Colors.Black)
    do  canvas.Children.Add(well.Control)
    let layout = Grid()
    do  layout.Children.Add canvas
    let whiteBrush = SolidColorBrush Colors.White
    let scoreBlock = TextBlock(Foreground=whiteBrush)
    let highBlock = TextBlock(Foreground=whiteBrush, TextAlignment=TextAlignment.Right)
    do  layout.Children.Add scoreBlock
    do  layout.Children.Add highBlock
    do  this.Content <- layout

    let isTetradBlocked (tetrad) (x,y) =
        tetrad.Blocks |> List.exists (fun block ->            
            (block.X + x, block.Y + y) |> well.IsBlocked
        )

    let rotateTetrad tetrad =
        let blocks = 
            tetrad.Blocks |> List.map (fun block ->
                {block with X = block.Y; Y = -block.X}
            )                        
        { tetrad with Blocks=blocks }

    let controlTetrad tetrad (x,y) =
        let dx = 
            keys.ReadKeyPresses Key.Right - keys.ReadKeyPresses Key.Left
            |> sign                                              
        let rotate = keys.ReadKeyPressed Key.Up                              
        let newTetrad = if rotate then rotateTetrad(!tetrad) else !tetrad            
        if not (isTetradBlocked newTetrad (!x+dx,!y+1)) then
            positionBlocks newTetrad.Blocks
            tetrad := newTetrad
            x := !x + dx

    let dockTetrad (tetrad) (x,y) =
        tetrad.Blocks |> List.iter (fun block ->
            tetrad.Canvas.Children.Remove block.Rectangle |> ignore
            let x',y' = block.X + x, block.Y + y
            setPosition block.Rectangle (toPosition (x', y'))                                    
            block.Rectangle |> well.AddBlock (x',y') 
        )

    let playTetrad tetrad (x,y) = async {
        positionTetrad !tetrad (!x,!y)                                                 
        canvas.Children.Add (!tetrad).Canvas
        let speed = ref 300       
        while not (isTetradBlocked !tetrad (!x,!y)) do            
            do! Async.Sleep !speed
            if keys.ReadKeyPressed Key.Down then speed := 30                
            controlTetrad tetrad (x,y)
            incr y
            if isTetradBlocked !tetrad (!x,!y+1) then
                dockTetrad (!tetrad) (!x,!y)
                canvas.Children.Remove (!tetrad).Canvas |> ignore
            positionTetrad !tetrad (!x,!y)                   
        }
 
    let rand = Random()
    
    let showScore score=   
        scoreBlock.Text <- sprintf "SCORE %i" score

    let rec inGameLoop score = async {               
        let index = rand.Next tetrads.Length 
        let tetrad = ref (createTetrad tetrads.[index])
        let x, y = ref (wellWidth/2 - 2), ref 0      
        if not (isTetradBlocked !tetrad (!x,!y+1)) then            
            do! playTetrad tetrad (x,y) 
            score := !score + 100 * well.ClearLines()
            showScore !score
            return! inGameLoop score  
        }        

    let message s =
        TextBlock(
            Text=s,
            HorizontalAlignment = HorizontalAlignment.Center,
            VerticalAlignment = VerticalAlignment.Center,
            Foreground = SolidColorBrush Colors.White,
            FontSize = 16.0)        

    let prompt text action = async {
        let start = message text
        layout.Children.Add start
        do! action()
        layout.Children.Remove start |> ignore
        }
        
    let awaitingClick () = 
        this.MouseLeftButtonDown |> Async.AwaitEvent |> Async.Ignore
    let paused () =  
        Async.Sleep 5000

    let readHighScore () =
        if settings.Contains "HighScore" 
        then settings.["HighScore"] :?> int 
        else 0

    let updateHighScore score highScore =
        if score > highScore then                
            settings.["HighScore"] <- score   
            score
        else highScore

    let showHighScore highScore =
        highBlock.Text <- sprintf "HIGH %i" highScore                

    let rec gameLoop highScore =  async {
        let score = ref 0 
        showScore !score
        showHighScore highScore
        do! prompt "Click To Start" awaitingClick                                                 
        do! inGameLoop score
        do! prompt "Game Over" paused
        let highScore = updateHighScore !score highScore            
        well.Clear()
        return! gameLoop highScore
        } 
      
    do  readHighScore() |> gameLoop |> Async.StartImmediate

(*[omit:Run script on TryFSharp.org]*)
#if INTERACTIVE
open Microsoft.TryFSharp
App.Dispatch (fun() -> 
    App.Console.ClearCanvas()
    let canvas = App.Console.Canvas
    let control = GameControl()    
    control |> canvas.Children.Add
    App.Console.CanvasPosition <- CanvasPosition.Right
    control.Focus() |> ignore
)
#endif
(*[/omit]*)