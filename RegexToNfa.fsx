type Regex =
    // 記号
    | Char of char
    // 選択 M | N
    | Alt of Regex * Regex
    // 連接 M・N
    | Seq of Regex * Regex
    // ""
    | Epsilon
    // 反復 M*
    | Star of Regex

let rec altall =
    function
    | [] -> failwith "空のリストです。"
    | [x] -> Regex.Char(x)
    | x :: xs -> Regex.Alt(Regex.Char(x), altall xs)

let charRange ranges =    
    ranges
    |> Seq.collect (fun (s, e) -> seq { s .. e } |> Seq.map char)
    |> Seq.distinct
    |> List.ofSeq
    |> altall

let stringRegex (s: string) = altall (List.ofSeq s)

let plus regex =
    Regex.Seq(regex, Regex.Star(regex))

type Label =
    | Char of char
    | Epsilon

type NfaNode =
    { Id: int; mutable Edges: (Label * NfaNode) list; mutable IsFinal: bool }

let mutable _nodeId = 0
let nodeId () =
    _nodeId <- _nodeId + 1
    _nodeId

let newNode () =
    { Id = nodeId(); Edges = []; IsFinal = false }

let rec regexToNfa (parent: NfaNode) regex =
    match regex with
    | Regex.Char(c) ->
        let node = newNode()
        parent.Edges <- (Label.Char(c), node) :: parent.Edges
        node
    | Regex.Alt(x, y) ->
        let node1 = newNode()
        parent.Edges <- (Label.Epsilon, node1) :: parent.Edges
        let node2 = newNode()
        let xnode = regexToNfa node1 x
        xnode.Edges <- (Label.Epsilon, node2) :: xnode.Edges
        let ynode = regexToNfa node1 y
        ynode.Edges <- (Label.Epsilon, node2) :: ynode.Edges
        node2
    | Regex.Seq(x, y) ->
        let node = regexToNfa parent x
        regexToNfa node y
    | Regex.Epsilon ->
        let node = newNode()
        parent.Edges <- (Label.Epsilon, node) :: parent.Edges
        node
    | Regex.Star(x) ->
        let node = newNode()
        parent.Edges <- (Label.Epsilon, node) :: parent.Edges
        let xnode = regexToNfa node x
        xnode.Edges <- (Label.Epsilon, node) :: xnode.Edges
        node

let nfaToDfa (node: NfaNode) =
    let edge (s: NfaNode) c =
        s.Edges
        |> Seq.filter (fun (l, _) -> l = c)
        |> Seq.map (fun (_, x) -> x)
        |> Set.ofSeq

    let rec closure t =
        let t' =
            t |> Seq.map (fun s -> edge s Label.Epsilon)
            |> Set.unionMany
        if Set.count t = Set.count t' then t'
        else closure t'

    let dfaEdge d c =
        d |> Seq.map (fun s -> edge s c)
        |> Set.unionMany
        |> closure

    let sigma =
        let tmpSet = System.Collections.Generic.HashSet<int>()
        let rec charEdges n =
            n.Edges            
            |> Seq.choose(function
                          | Label.Char(c), _ -> Some(c)
                          | Label.Epsilon, _ -> None)
            |> Seq.append
                (n.Edges
                 |> Seq.filter (fun (_, x) -> tmpSet.Add(x.Id)) // block infinite loop
                 |> Seq.collect (fun (_, x) -> charEdges x))
        Set.ofSeq (charEdges node)

    let mutable p, j = 1, 0
    let states = System.Collections.Generic.Dictionary<_, _>()
    states.[0] <- Set.empty
    states.[1] <- Set.ofArray [| node |]
    let trans = System.Collections.Generic.Dictionary<_, _>()
    while j <= p do
        for c in sigma do
            let e = dfaEdge states.[j] (Label.Char(c))
            let i = 
                seq { 0 .. p }
                |> Seq.filter (fun i -> states.ContainsKey(i) && e = states.[i])
                |> Seq.tryHead
            match i with
            | Some(i) -> trans.[(j, c)] <- i
            | None ->
                p <- p + 1
                states.[p] <- e
                trans.[(j, c)] <- p
        j <- j + 1

    states |> Seq.map (|KeyValue|) |> Map.ofSeq,
    trans |> Seq.map (|KeyValue|) |> Map.ofSeq

let runDfa (states: Map<int, Set<NfaNode>>) (trans: Map<(int * char), int>) (s: string) startPos =
    let isFinal state =
        states.[state] |> Set.exists (fun node -> node.IsFinal)

    let mutable lastFinal = None
    let rec loop pos state =
        let error () =
            match lastFinal with
            | Some(x) -> x
            | None -> 0, pos

        if pos >= s.Length then error()
        else
            match trans.TryFind((state, s.[pos])) with
            | Some(x) ->
                if isFinal state then
                    lastFinal <- Some(state, pos)
                loop (pos + 1) x
            | None -> error()

    loop startPos 1

let tokenize states trans (s: string) =
    let mutable pos = 0
    seq {
        while pos < s.Length do
            let state, endPos = runDfa states trans s pos
            yield (state, s.Substring(pos, endPos - pos))
            pos <-
                if state = 0 then pos + 1
                else endPos + 1
    }

let rootNode = newNode()

let ifRegex = stringRegex "if"
let ifNfa = regexToNfa rootNode ifRegex
ifNfa.IsFinal <- true

let idRegex =
    Regex.Seq(charRange['a', 'z'],
        Regex.Star(charRange['a', 'z'; '0', '9']))
let idNfa = regexToNfa rootNode idRegex
idNfa.IsFinal <- true

let numRegex = plus(charRange['0', '9'])
let numNfa = regexToNfa rootNode numRegex
numNfa.IsFinal <- true

let whitespaceRegex = plus(altall[' '; '\r'; '\n'; '\t'])
let whitespaceNfa = regexToNfa rootNode whitespaceRegex
whitespaceNfa.IsFinal <- true

let states, trans = nfaToDfa rootNode

printfn "DFA 遷移"
Map.iter (fun k v -> printfn "%A => %d" k v) trans

let result = tokenize states trans "if 01234 if00 --++ yeah"

for state, value in result do
    printfn "%d %s" state value
