module Client

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import


let (|DivRem|) a n =
    (n/a,n%a)
let (|Float|) a = float a


type 'a Point =
    {x: ^a; y: ^a}
    static member Empty = {x=Unchecked.defaultof<'a>;y=Unchecked.defaultof<'a>}
type 'a Size =
    {w: ^a; h: ^a}
    static member Empty = {w=Unchecked.defaultof<'a>;h=Unchecked.defaultof<'a>}


type 'a Rect =
    {
        p : 'a Point
        s : 'a Size
    }
    static member Empty = {p=Point<'a>.Empty;s=Size<'a>.Empty}

let inside {p={x=x;y=y};s={h=h;w=w}} {x=x';y=y'} =        
        x' >= x && x' <= ( x + w ) && y' >= y && y' <= ( y + h )


type Card=
    {
        Kind: int
        Suit: int
        Selected: bool
        LastRendering: float Rect
    }    
    static member private deck =
        let image = Browser.document.createElement_img()
        image.src <- "Deck.svg"
        image
    static member private back =
        let image = Browser.document.createElement_img()
        image.src <- "Back.svg"
        image
    member t.IsSelected = t.Selected
    member t.Toggle () =
        {t with Selected = not t.Selected}
    static member DrawTo ((ctx:Browser.CanvasRenderingContext2D),{p={x=x; y=y};s={w=w;h=h}}) =
        ctx.drawImage(!^Card.back,x,y,w,h)
    member t.DrawTo((ctx:Browser.CanvasRenderingContext2D),({p={x=x; y=y};s={w=w;h=h}} as rect)) =
        ctx.drawImage(!^Card.deck, 101*t.Kind|>float,101*t.Suit |> float, 101.,101.,x,y,w,h)
        if t.IsSelected then
            ctx.save()
            ctx.fillStyle <- !^"rgba(255, 246, 116, 0.5)"
            ctx.fillRect(x,y,w,h)
            ctx.restore()
        {t with LastRendering=rect}
    member t.OnClick x y = if inside t.LastRendering {x=x;y=y} then Some t.Toggle else None
    static member Create n = {Kind=n%13;Suit=n/13;Selected=false;LastRendering=Rect.Empty}
let deck = Array.init 52 Card.Create
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


type Player =
    {
        Hand: Card list
        Order: int
        SelectedCards: (Card*Card) option
        LastRendering: float Rect
        IsHuman: bool
    }
    static member Create n = {Hand=[];Order=n; SelectedCards=None; LastRendering=Rect.Empty; IsHuman = true}
    member t.DrawTo((ctx:Browser.CanvasRenderingContext2D),({p={x=x; y=y} as p;s={w=w;h=h} as s} as rect)) =
        
        {t with LastRendering=rect;Hand=t.Hand |> List.zip (spreadOn (List.length t.Hand) p s) |> List.map (fun (r,e)->e.DrawTo(ctx,r))}

type GameState =
    {
        Deck:Card list
        TeamOne: int
        TeamTwo: int
        Round: int []
        Table: Card list
        PlayerToPlay:Player
        Players: Player[]
    }    
    static member Empty =
        let players = Array.init 4 Player.Create
        {Deck=Deck.NewShuffle();TeamOne=0;TeamTwo=0;Round=Array.zeroCreate 3;Table=[];PlayerToPlay=players.[0];Players=players}

type DrawingMessages =
    | DrawCanvas of GameState*AsyncReplyChannel<GameState>

type GameMessages =
    | Reset
    | Draw
    | PlayCards of Player*Card[]
    | SendClick of x:float*y:float
    
let drawing = MailboxProcessor.Start <| fun n ->
    let rec proc (body:Browser.HTMLBodyElement) (canvas:Browser.HTMLCanvasElement) = async{
        let! DrawCanvas (g,r) = n.Receive()
        let ctx = canvas.getContext_2d()
        let gp' = g.Players |> Array.map (fun t-> t.DrawTo(ctx, Rect.Empty))        
        r.Reply {g with Players=gp'}
        return! proc body canvas
    }
    proc (Browser.document.body :?> Browser.HTMLBodyElement) (Browser.document.getElementsByTagName_canvas().[0])

    

let rec game = MailboxProcessor.Start <| fun n ->
    let rec proc g =
        async {
            let! m = n.Receive()
            match m with
            | Draw ->
                printfn "Draw called"
                let! g = drawing.PostAndAsyncReply (fun n -> DrawCanvas (g,n))
                return! proc g
            | Reset -> failwith "Not Implemented"
            | PlayCards(_, _) -> failwith "Not Implemented"
            | SendClick(x, y) ->                 
                printfn "%f %f" x y
            return! proc g
        }
    proc GameState.Empty


let draw() = [
    let canvas = Browser.document.getElementsByTagName_canvas().[0]
    let body = Browser.document.body :?> Browser.HTMLBodyElement    
    canvas.width <- body.clientWidth
    canvas.height <- body.clientHeight
    let n = 2
    let s = (max  canvas.width canvas.height) / (float n)
    let s'={h=s;w=s}
    let ctx = canvas.getContext_2d()
    
    yield! spreadOn n {x=0.;y=0.} {h=canvas.height;w=canvas.width}
    |> List.zip (Deck.NewShuffle())
    |> List.map (fun (c,n)-> c.DrawTo(ctx,n))   
]
let init() =
    let body = Browser.document.body :?> Browser.HTMLBodyElement 
    let canvas = Browser.document.getElementsByTagName_canvas().[0]
    canvas.onclick <- fun e -> game.Post (SendClick (e.clientX, e.clientY))  |> box
    body.onresize <- fun _ -> game.Post Draw :> obj
    body.onload <- fun _ -> draw() :> obj
    
    
init()