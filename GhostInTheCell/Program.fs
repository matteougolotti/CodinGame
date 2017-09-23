// https://www.codingame.com/ide/puzzle/ghost-in-the-cell

open System


type Player = MAX | MIN | NONE


let not player =
  match player with
  | MAX -> MIN
  | MIN -> MAX
  | NONE -> NONE


type Factory (id : int, owner : Player, cyborgs : int, production : int) =
  member this.id = id
  member this.owner = owner
  member this.cyborgs = cyborgs
  member this.production = production


type Troop (id : int, owner : Player, src : int, dst : int, cyborgs : int, turnsLeft : int) =
  member this.id = id
  member this.owner = owner
  member this.src = src
  member this.dst = dst
  member this.cyborgs = cyborgs
  member this.turnsLeft = turnsLeft


type ActionType = MOVE | WAIT


type Action (action : ActionType, source : int, destination : int, cyborgs : int) =
  member this.action = action
  member this.source = source
  member this.destination = destination
  member this.cyborgs = cyborgs


type Turn (factories : Factory[], troops : Troop[], player : Player) =
  member this.factories = factories
  member this.troops = troops
  member this.player = player


let isMaxPlayer p =
  match p with
  | y when y = 1 -> MAX
  | _ -> MIN


let action str =
  match str with
  | "MOVE" -> MOVE
  | _ -> WAIT


let playerFactories (graph : int[][]) (player : Player) (factories : Factory[]) =
  Array.filter (fun (f : Factory) -> f.owner = player) factories
  |> Array.map (fun f -> f.id)


let enemyNeighbours (graph : int[][]) (factories : Factory[]) (factory : Factory) (player : Player) =
  [| for i in graph.[factory.id] do yield factories.[i] |]
  |> playerFactories graph player


let fightInnerBattle (graph : int[][]) (factory : Factory) (defending : int) (attacking : int) =
  let survivors = defending - attacking
  match survivors with
  | y when y > 0 -> Factory (factory.id, factory.owner, survivors, factory.production)
  | y when y < 0 -> Factory (factory.id, not factory.owner, -survivors, factory.production)
  | _ -> Factory (factory.id, NONE, 0, factory.production)


let fightOuterBattle (graph : int[][]) (troops : Troop[]) (player : Player) (factory : Factory) =
  let leavingCyborgs = troops
                       |> Array.filter (fun t -> t.src = factory.id && t.turnsLeft = graph.[t.src].[t.dst]) 
                       |> Array.length
  let cyborgs = match factory.owner with
                | NONE -> 0
                | _ -> factory.cyborgs + factory.production - leavingCyborgs
  let defendingInCyborgs = troops 
                           |> Array.filter (fun t -> t.dst = factory.id && t.owner = factory.owner && t.turnsLeft = 0)
                           |> Array.length
  let attackingInCyborgs = troops
                           |> Array.filter (fun t -> t.dst = factory.id && t.owner <> factory.owner && t.turnsLeft = 0)
                           |> Array.length
  let survivors = defendingInCyborgs - attackingInCyborgs
  match survivors with
  | y when y < 0 -> fightInnerBattle graph factory cyborgs -survivors
  | _ -> Factory (factory.id, factory.owner, cyborgs + survivors, factory.production)


let nextTurn (graph : int[][]) (prevTurn : Turn) (action : Action) (player : Player) =
  let troops = prevTurn.troops
               |> Array.filter (fun t -> t.turnsLeft > 0)
               |> Array.map (fun t -> Troop (t.id, t.owner, t.src, t.dst, t.cyborgs, (t.turnsLeft - 1)))
  let factories = prevTurn.factories
                  |> Array.map (fightOuterBattle graph prevTurn.troops player)
  Turn (factories, troops, player)


let moves (graph : int[][]) (factories : Factory[]) (player : Player) =
  let srcFactories = playerFactories graph player factories
  let rec generateMoves src dst cyborgs firstMove = seq {
    if firstMove then
      yield Action(WAIT, 0, 0 ,0)
    let dstFactories = 
      enemyNeighbours graph factories factories.[srcFactories.[src]] player
    if factories.[srcFactories.[src]].cyborgs > 1 then
      yield Action(MOVE, srcFactories.[src], dstFactories.[dst], cyborgs)
    if cyborgs < (factories.[srcFactories.[src]].cyborgs + 1) then
      yield! generateMoves src dst (cyborgs + 1) false
    if src < srcFactories.Length then
      yield! generateMoves (src + 1) dst cyborgs false
    if dst < dstFactories.Length then
      yield! generateMoves src (dst + 1) cyborgs false
  }
  generateMoves 0 0 1 true


let gameover (turn : Turn) =
  let max = turn.factories
            |> Array.filter (fun f -> f.owner = MAX)
            |> Array.length
  let min = turn.factories
            |> Array.filter (fun f -> f.owner = MIN)
            |> Array.length
  
  if max = 0 || min = 0 then true
  else false


let score (turn : Turn) =
  let max = turn.factories
            |> Array.filter (fun f -> f.owner = MAX)
            |> Array.length
  let min = turn.factories
            |> Array.filter (fun f -> f.owner = MIN)
            |> Array.length
  max - min


let rec minimax (graph : int[][]) (turn : Turn) (depth : int) (α : int) (β : int) (player : Player) =
  let mutable a = α
  let mutable b = β
  if depth = 0 || gameover turn then
    score turn
  else if player = MAX then
    let mutable v = Microsoft.FSharp.Core.int.MinValue
    let actions = moves graph turn.factories player
    let mutable i = 0
    let mutable n = true
    while n do
      let move = actions |> Seq.item i
      i <- i + 1
      let newTurn = nextTurn graph turn move player
      v <- max v (minimax graph newTurn (depth - 1) a b MIN)
      a <- max a v
      if b <= a then
        n <- false
    v
  else
    let mutable v = Microsoft.FSharp.Core.int.MaxValue
    let actions = moves graph turn.factories player
    let mutable i = 0
    let mutable n = true
    while n do
      let move = actions |> Seq.item i
      i <- i + 1
      let newTurn = nextTurn graph turn move player
      v <- max v (minimax graph newTurn (depth - 1) a b MIN)
      a <- max a v
      if b <= a then
        n <- false
    v


let initFactoriesConnections n =
    [| for i in [0..n-1] do yield [| for j in [0..n-1] do yield -1 |] |]

///////////////////////// GAME START ///////////////////////////////

let factoryCount = int(Console.In.ReadLine()) (* the number of factories *)
let linkCount = int(Console.In.ReadLine()) (* the number of links between factories *)

let mutable mutableGraph = initFactoriesConnections linkCount

for i in 0 .. linkCount - 1 do
    let token = (Console.In.ReadLine()).Split [|' '|]
    let factory1 = int(token.[0])
    let factory2 = int(token.[1])
    let distance = int(token.[2])
    mutableGraph.[factory1].[factory2] <- distance
    mutableGraph.[factory2].[factory1] <- distance
    ()

let graph = mutableGraph

(* game loop *)
while true do
    
    let mutable mutFactories = [||]
    let mutable mutTroops = [||]

    let entityCount = int(Console.In.ReadLine()) (* the number of entities (e.g. factories and troops) *)
    for i in 0 .. entityCount - 1 do
        let token1 = (Console.In.ReadLine()).Split [|' '|]
        let entityId = int(token1.[0])
        let entityType = token1.[1]
        let arg1 = int(token1.[2])
        let arg2 = int(token1.[3])
        let arg3 = int(token1.[4])
        let arg4 = int(token1.[5])
        let arg5 = int(token1.[6])

        match entityType with
        | "FACTORY" -> mutFactories <- Array.append mutFactories [| Factory (entityId, isMaxPlayer (arg1), arg2, arg3) |]
        | "TROOP" -> mutTroops <- Array.append mutTroops [| Troop (entityId, isMaxPlayer (arg1), arg2, arg3, arg4, arg5) |]
        | _ -> ()

        ()
    
    let factories = mutFactories
    let troops = mutTroops
    
    (* Write an action using printfn *)
    (* To debug: Console.Error.WriteLine("Debug message") *)
    

    (* Any valid action, such as "WAIT" or "MOVE source destination cyborgs" *)
    printfn "WAIT"
    ()
