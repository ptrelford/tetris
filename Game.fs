﻿(*[omit:Mouse input]*)
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

(*[omit:Mouse input]*)
module MotionTracker =   
    type point = float * float        
    type message =
        | Clear
        | Add of point
        | Take of point * AsyncReplyChannel<point>
        | Exit
    let start () = MailboxProcessor.Start(fun inbox ->
        let rec loop delta = async {
            let! m = inbox.Receive()
            match m with
            | Clear -> return! loop (0.0,0.0)
            | Add (dx,dy) -> 
                let dx',dy' = delta
                return! loop (dx+dx',dy+dy')
            | Take((tx,ty),channel) ->
                let dx, dy = delta
                let sx, sy = sign dx |> float, sign dy |> float
                let x, y = abs dx, abs dy
                let dx' = if x >= tx then tx else 0.0 
                let dy' = if y >= ty then ty else 0.0
                channel.Reply (sx * dx', sy * dy')
                let x, y = x - dx', y - dy'
                return! loop (sx * x, sy * y)
            | Exit -> ()
        }
        loop (0.0,0.0)
        )
        
type MouseTracking (item:UIElement) =    
    let mutable (disposables:IDisposable list) = []
    let remember d = disposables <- d::disposables
    let forget () = for d in disposables do d.Dispose()
    let x = ref 0.0
    let y = ref 0.0
    let isDown = ref false
    let tracker = MotionTracker.start ()    
    do  item.MouseLeftButtonDown
        |> Observable.subscribe (fun me ->            
            isDown := true            
            let position = me.GetPosition(null)
            x := position.X
            y := position.Y
            let success = item.CaptureMouse()
            item.Opacity <- 0.5
        ) |> remember
        item.MouseLeftButtonUp
        |> Observable.subscribe (fun me ->
            isDown := false
            item.Opacity <- 1.0
            item.ReleaseMouseCapture()
            tracker.Post(MotionTracker.Clear)
        ) |> remember
        item.MouseMove
        |> Observable.subscribe (fun me ->
            if !isDown then
                let p = me.GetPosition(null)
                let dx = p.X - !x
                let dy = p.Y - !y
                tracker.Post(MotionTracker.Add(dx,dy))   
                x := p.X; y := p.Y
        ) |> remember
    member this.TakeMotion() =
        tracker.PostAndReply(fun reply -> MotionTracker.Take((8.0,8.0),reply))
    interface IDisposable with
        member this.Dispose() = 
            tracker.Post(MotionTracker.Exit)
            forget ()
(*[/omit]*)

type KeyState (control:Control) =
    let mutable keysDown = Set.empty
    do  control.KeyDown.Add (fun e -> keysDown <- keysDown.Add e.Key)
    do  control.KeyUp.Add (fun e -> keysDown <- keysDown.Remove e.Key)    
    member this.IsKeyDown key = keysDown.Contains key
    member this.IsAnyKeyDwn () = keysDown.Count > 0

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
    let checkLines () =
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
        lineBlockCounts |> List.iter (fun (count,y) -> 
            if count = wellWidth then
                clearLine y
                fallDownTo y             
        )
    member well.IsBlocked = isBlocked
    member well.AddBlock (x,y) (block:UIElement) = addBlock (x,y) block         
    member well.CheckLines () = checkLines ()
    member well.Clear () = clear ()
    member well.Control = canvas

type GameControl() as this =
    inherit UserControl(
            Width = float wellWidth*blockSize, 
            Height = float wellHeight*blockSize)

    let keys = KeyState(this)    
    let well = Well()           
    let canvas = Canvas(Background=SolidColorBrush Colors.Black)
    do  canvas.Children.Add(well.Control)
    let layout = Grid()
    do  layout.Children.Add canvas
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

    let readKeys tetrad =   
        let mutable dx = 0
        if keys.IsKeyDown Key.Left then dx <- dx - 1
        if keys.IsKeyDown Key.Right then dx <- dx + 1                                  
        if keys.IsKeyDown Key.Up then
            rotateTetrad tetrad, dx            
        else
            tetrad,dx 

    let dockTetrad (tetrad) (x,y) =
        tetrad.Blocks |> List.iter (fun block ->
            tetrad.Canvas.Children.Remove block.Rectangle |> ignore
            let x',y' = block.X + x, block.Y + y
            setPosition block.Rectangle (toPosition (x', y'))                                    
            block.Rectangle |> well.AddBlock (x',y') 
        )

    let playTetrad tetrad (x,y) = async {
        let mouse = new MouseTracking((!tetrad).Canvas)                    
        positionTetrad !tetrad (!x,!y)                                                 
        canvas.Children.Add (!tetrad).Canvas
        while not (isTetradBlocked !tetrad (!x,!y)) do
            do! Async.Sleep 300        
            let newTetrad, dx = readKeys !tetrad
            let dx', _ = mouse.TakeMotion()
            let dx = dx + sign dx'
            if not (isTetradBlocked newTetrad (!x+dx,!y+1)) then
                positionBlocks newTetrad.Blocks
                tetrad := newTetrad
                x := !x + dx
            incr y
            if isTetradBlocked !tetrad (!x,!y+1) then
                dockTetrad (!tetrad) (!x,!y)
                canvas.Children.Remove (!tetrad).Canvas |> ignore
            positionTetrad !tetrad (!x,!y)   
        (mouse :> IDisposable).Dispose()        
        }

    let rand = Random()  
    let rec inGameLoop () = async {  
        let index = rand.Next tetrads.Length 
        let tetrad = ref (createTetrad tetrads.[index])
        let x, y = ref (wellWidth/2 - 2), ref 0      
        if not (isTetradBlocked !tetrad (!x,!y+1)) then
            do! playTetrad tetrad (x,y) 
            well.CheckLines()
            return! inGameLoop ()   
        }

    let createMessage s =
        let t = TextBlock(Text=s)
        t.HorizontalAlignment <- HorizontalAlignment.Center
        t.VerticalAlignment <- VerticalAlignment.Center
        t.Foreground <- SolidColorBrush Colors.White
        t
        
    let rec gameLoop () =  async {
        let start = createMessage "Click To Start"
        layout.Children.Add start
        do! this.MouseLeftButtonDown |> Async.AwaitEvent |> Async.Ignore
        layout.Children.Remove start |> ignore
        do! inGameLoop () 
        let gameOver = createMessage "Game Over"
        layout.Children.Add gameOver
        do! Async.Sleep 5000
        layout.Children.Remove gameOver |> ignore
        well.Clear()
        return! gameLoop ()
        } 
    do  gameLoop() |> Async.StartImmediate

(*[omit:Run script on TryFSharp.org]*)
#if INTERACTIVE
open Microsoft.TryFSharp
App.Dispatch (fun() -> 
    App.Console.ClearCanvas()
    WellControl() |> App.Console.Canvas.Children.Add
    App.Console.CanvasPosition <- CanvasPosition.Right
)
#endif
(*[/omit]*)