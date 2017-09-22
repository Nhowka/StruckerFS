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
        t.SelectedCards |> Option.iter (fun (a,b) -> [a;b] |> List.zip (spreadOn (List.length t.Hand) p s) |> List.iter (fun (r,e)->e.DrawTo(ctx,r) |> ignore))
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
    | DrawAndUpdate of GameState*AsyncReplyChannel<GameState>
    

type GameMessages =
    | Reset
    | NextRound
    | Draw
    | PlayCards of Player*(Card*Card)
    | SendClick of x:float*y:float

let getRect {h=h;w=w} n =  
    let s = (min h w)*0.2
    let t = (min h w)*0.5
    let (x',y')=
        match (h,w) with
        |(h,w) when h>w -> 0.,(h-w)/4.
        |(h,w) when h<w -> (w-h)/4.,0.
        |_ -> 0.,0.
    match n with
    |1 -> {p={x=x';y=(h-t)/2.};s={h=t;w=s}}
    |2 -> {p={x=(w-t)/2.;y=y'};s={h=s;w=t}}
    |3 -> {p={x=w-s-x';y=(h-t)/2.};s={h=t;w=s}}
    |0 -> {p={x=(w-t)/2.;y=h-s-y'};s={h=s;w=t}}
    |_ -> {p={x=(w-t)/2.;y=(h-s)/2.};s={h=s;w=t}}

let drawing = MailboxProcessor.Start <| fun n ->
    let rec proc (body:Browser.HTMLBodyElement) (canvas:Browser.HTMLCanvasElement) = async{
        let! DrawAndUpdate(g,r) = n.Receive()
        canvas.width <- body.clientWidth
        canvas.height <- body.clientHeight        
        let ctx = canvas.getContext_2d()
        let rc = {h=body.clientHeight;w=body.clientWidth}
        let p = g.Players |> Array.mapi (fun n t-> t.DrawTo(ctx, getRect rc n))
        let {p=p';s=s'}= getRect rc 4
        g.Table |> printfn "%A"
        g.Table |> List.zip (spreadOn (List.length g.Table) p' s') |> List.iter (fun (r,e)->e.DrawTo(ctx,r) |> ignore)                      
        r.Reply {g with Players=p}
        printfn "Draw Called"        
        return! proc body canvas
    }
    proc (Browser.document.body :?> Browser.HTMLBodyElement) (Browser.document.getElementsByTagName_canvas().[0])

    

let rec game = MailboxProcessor.Start <| fun n ->
    let rec proc g =
        async {
            let! m = n.Receive()
            match m with
            | Draw ->
                //printfn "Draw called"
                let! g = drawing.PostAndAsyncReply (fun n -> DrawAndUpdate (g,n))
                return! proc g
            | Reset -> 
                game.Post NextRound
                return! proc GameState.Empty
            | PlayCards(_, _) -> failwith "Not Implemented"
            | SendClick(x, y) ->                 
                let! g = drawing.PostAndAsyncReply (fun n -> DrawAndUpdate (g,n))
                return! proc g
            | NextRound ->                 
                let rec giveCards (g:GameState) p n d =                    
                    match (p,n) with
                    |(0,0) -> {g with Table = d |> List.take 3}
                    |(0,n) -> giveCards g 4 (n-1) d
                    |(p,n) ->                        
                        match d with
                        |a::b::d ->
                            let g' = g
                            let h' = a::b::g'.Players.[p-1].Hand
                            g'.Players.[p-1]<-{g'.Players.[p-1] with Hand=h'} 
                            giveCards g' (p-1) n d                            
                        |_ -> failwith "Deck should have enough cards"
                let! g = drawing.PostAndAsyncReply (fun n -> DrawAndUpdate (giveCards g 4 2 g.Deck,n))
                do! Async.Sleep 1000
                return! proc g
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
    body.onload <- fun _ -> game.Post NextRound :> obj
    
    
init()