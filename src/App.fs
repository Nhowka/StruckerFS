module Client

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import


let (|DivRem|) a n =
    (n/a,n%a)
let (|Float|) a = float a


type 'a Point = {x: ^a; y: ^a}
type 'a Size = {w: ^a; h: ^a}


type 'a Rect =
    {
        p : 'a Point
        s : 'a Size
    }
let inside {p={x=x;y=y};s={h=h;w=w}} {x=x';y=y'} =        
        x' >= x && x' <= ( x + w ) && y' >= y && y' <= ( y + h )


type Card(order)=
    let (Float kind) = order%13;
    let (Float suit) = order/13;
    let selected = ref false
    static let deck =
        let image = Browser.document.createElement_img()
        image.src <- "Deck.svg"
        image
    static let back =
        let image = Browser.document.createElement_img()
        image.src <- "Back.svg"
        image
    member val IsSelected = !selected
    member t.Toggle () =
        selected:= t.IsSelected |> not
        t.IsSelected
    static member DrawTo ((ctx:Browser.CanvasRenderingContext2D),{p={x=x; y=y};s={w=w;h=h}}) =
        ctx.drawImage(!^back,x,y,w,h)
    member t.DrawTo((ctx:Browser.CanvasRenderingContext2D),{p={x=x; y=y};s={w=w;h=h}}) =
        ctx.drawImage(!^deck, 101.*kind,101.*suit, 101.,101.,x,y,w,h)
        if t.IsSelected then
            ctx.save()
            ctx.fillStyle <- !^"rgba(255, 246, 116, 0.5)"
            ctx.fillRect(x,y,w,h)
            ctx.restore()
    member t.OnClick = t.Toggle()
let deck = Array.init 52 Card
type Deck =         
    static member NewShuffle() =
        let r = System.Random()
        for i in 51..-1..1 do
            let p = r.Next(i+1)
            let t = deck.[p]
            deck.[p]<-deck.[i]
            deck.[i]<-t           
        deck |> List.ofArray

let spreadOn n {x=x;y=y} {h=h;w=w} =
    let (s,x',y') =
        match n-1,h,w with
        |Float n,h,w when h>w ->
            w,0.,(h-w)/n
        |Float n,h,w -> h,(w-h)/n,0.
    List.init n (fun (Float n) ->{p={x=x+x'*n;y=y+y'*n};s={h=s;w=s}})



type Game() =
    member t.Start() = ()

let draw() = [
    let canvas = Browser.document.getElementsByTagName_canvas().[0]
    let body = Browser.document.body :?> Browser.HTMLBodyElement    
    canvas.width <- body.clientWidth
    canvas.height <- body.clientHeight
    let n = 2
    let s = (max  canvas.width canvas.height) / (float n)
    let s'={h=s;w=s}
    let ctx = canvas.getContext_2d()
    
    spreadOn n {x=0.;y=0.} {h=canvas.height;w=canvas.width}
    |> List.zip (Deck.NewShuffle())
    |> List.iter (fun (c,n)-> c.DrawTo(ctx,n))   
]
let init() =
    let body = Browser.document.body :?> Browser.HTMLBodyElement    
    body.onresize <- fun _ -> draw() :> obj
    body.onload <- fun _ -> draw() :> obj
    
    
init()