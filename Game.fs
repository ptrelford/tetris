#if INTERACTIVE
#else
module Game
#endif
    
open System
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open System.Windows.Media
open System.Windows.Shapes

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

type KeyState (control:Control) =
    let mutable keysDown = Set.empty
    do  control.KeyDown.Add (fun e -> keysDown <- keysDown.Add e.Key)
    do  control.KeyUp.Add (fun e -> keysDown <- keysDown.Remove e.Key)    
    member this.IsKeyDown key = keysDown.Contains key
    member this.IsAnyKeyDwn () = keysDown.Count > 0

let position (block:#UIElement) (x,y) =    
    block.SetValue(Canvas.LeftProperty, x)
    block.SetValue(Canvas.TopProperty, y)

let blockSize = 16.0
let toPosition (x,y) = float x * blockSize, float y * blockSize

type Block = { X:int; Y:int; Rectangle:Rectangle }
type Tetrad = { Blocks:Block list; Canvas:Canvas }

let positionBlock block =
    (block.X, block.Y) |> toPosition |> position block.Rectangle

let positionBlocks blocks = blocks |> List.iter positionBlock

let positionTetrad tetrad (x,y) =
    (x,y) |> toPosition |> position tetrad.Canvas
    
let createTetrad (coordinates,stroke,fill) =    
    let createRectangle () =
        Rectangle(
            Width=blockSize,Height=16.0,
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

let wellWidth, wellHeight = 10,20

type WellControl() as this =
    inherit UserControl(
            Width = float wellWidth*blockSize, 
            Height = float wellHeight*blockSize)

    let keys = KeyState(this)

    let canvas = new Canvas(Background=SolidColorBrush Colors.Black)    
    do  this.Content <- canvas

    let well = Array2D.create wellWidth wellHeight None

    let isBlocked (x,y) =
        if x < 0 || x >= wellWidth then true
        elif y < 0 || y >= wellHeight then true
        else
            well.[x,y] |> Option.exists (fun x -> true)

    let blocked (tetrad) (x,y) =
        tetrad.Blocks |> List.exists (fun block ->            
            (block.X + x, block.Y + y) |> isBlocked
        )

    let update tetrad =   
        let mutable dx = 0
        if keys.IsKeyDown Key.Left then dx <- dx - 1
        if keys.IsKeyDown Key.Right then dx <- dx + 1                   
        if keys.IsKeyDown Key.Up then
            let blocks = 
                tetrad.Blocks |> List.map (fun block ->
                    {block with X = block.Y; Y = -block.X}
                )                        
            { tetrad with Blocks=blocks}, dx            
        else
            tetrad,dx 

    let dock (tetrad) (x,y) =
        tetrad.Blocks |> List.iter (fun block ->
            tetrad.Canvas.Children.Remove block.Rectangle |> ignore
            let x',y' = block.X + x, block.Y + y
            position block.Rectangle (toPosition (x', y'))
            canvas.Children.Add block.Rectangle                        
            well.[x',y'] <- Some block.Rectangle
        )

    let rand = Random()  

    let runTetrad () = async {
        let index = rand.Next tetrads.Length 
        let tetrad = ref (createTetrad tetrads.[index])      
        let x, y = ref (rand.Next (wellWidth-4)), ref 0        
        positionTetrad !tetrad (!x,!y)                                                 
        canvas.Children.Add (!tetrad).Canvas 
        while not (blocked !tetrad (!x,!y)) do
            do! Async.Sleep 300        
            let newTetrad, dx = update !tetrad            
            if not (blocked newTetrad (!x+dx,!y+1)) then
                positionBlocks newTetrad.Blocks
                tetrad := newTetrad
                x := !x + dx
            incr y      
            if blocked !tetrad (!x,!y+1) then
                dock (!tetrad) (!x,!y)
                canvas.Children.Remove (!tetrad).Canvas |> ignore
            positionTetrad !tetrad (!x,!y)   
        let lines =
            [0..wellHeight-1] |> List.map (fun y ->
                [0..wellWidth-1] 
                |> List.map (fun x -> well.[x,y])
                |> List.map Option.count 
                |> List.reduce (+), y
            )
        lines |> List.iter (fun (count,y) -> 
            if count = wellWidth then
                for x = 0 to wellWidth-1 do
                    well.[x,y] |> Option.iter (fun r -> canvas.Children.Remove r |> ignore)
                    well.[x,y] <- None
                for i = y-1 downto 1 do
                    for x = 0 to wellWidth-1 do
                        let r = well.[x,i]                        
                        r |> Option.iter (fun r' ->                         
                            position r' (toPosition (x,i+1))
                            well.[x,i+1] <- r
                            well.[x,i] <- None
                        )
        )
        }

    do  async {             
            while true do do! runTetrad () 
        }
        |> Async.StartImmediate

#if INTERACTIVE
open Microsoft.TryFSharp
App.Dispatch (fun() -> 
    App.Console.ClearCanvas()
    WellControl() |> App.Console.Canvas.Children.Add
    App.Console.CanvasPosition <- CanvasPosition.Right
)
#endif