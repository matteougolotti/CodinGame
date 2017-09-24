// https://www.codingame.com/ide/puzzle/ghost-in-the-cell

open System
open System.Collections.Generic


type Player = MAX | MIN | NONE


let other player =
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
  member this.ToString = 
    this.action.ToString() +
    this.source.ToString() +
    this.destination.ToString() +
    this.cyborgs.ToString()


type Turn (factories : Factory[], troops : Troop[], player : Player) =
  member this.factories = factories
  member this.troops = troops
  member this.player = player


let isMaxPlayer p =
  match p with
  | y when y = 1 -> MAX
  | y when y = 0 -> NONE
  | _ -> MIN


let action str =
  match str with
  | "MOVE" -> MOVE
  | _ -> WAIT


let playerFactories (player : Player) (factories : Factory[]) =
  factories
  |> Array.filter (fun f -> f.owner = player)
  |> Array.map (fun f -> f.id)


let enemyNeighbours (graph : int[][]) (factories : Factory[]) (factory : Factory) (player : Player) =
  factories
  |> Array.filter (fun f -> graph.[factory.id].[f.id] >= 0)
  |> Array.map (fun f -> f.id)


type MoveGenerator (graph : int[][], factories : Factory[], player : Player) =
  member private this.graph = graph
  member private this.factories = factories
  member private this.player = player
  member private this.prevActions = new HashSet<string>();
  member private this.rnd = new Random();
  member private this.srcFactories = playerFactories player factories
  member private this.waited = false
  member this.nextRandomMove =
    let src = this.rnd.Next(this.srcFactories.Length)
    let dstFactories = enemyNeighbours this.graph this.factories this.factories.[this.srcFactories.[src]] this.player
    let dst = this.rnd.Next(dstFactories.Length)
    let cyborgs = this.rnd.Next(this.factories.[this.srcFactories.[src]].cyborgs)
    let action = Action(MOVE, this.srcFactories.[src], dstFactories.[dst], cyborgs)
    let a = action.ToString
    if not (this.prevActions.Contains action.ToString) then
      let res = this.prevActions.Add action.ToString
      action
    else this.nextRandomMove


let fightInnerBattle (graph : int[][]) (factory : Factory) (defending : int) (attacking : int) =
  let survivors = defending - attacking
  match survivors with
  | y when y > 0 -> Factory (factory.id, factory.owner, survivors, factory.production)
  | y when y < 0 -> Factory (factory.id, other factory.owner, -survivors, factory.production)
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


type Result (won : int, played : int) =
  member this.won = won
  member this.played = played


let rec montecarlo (graph : int[][]) (turn : Turn) (depth : int) (player : Player) : Result =
  if (depth = 0) || (gameover turn) then
    let s = score turn
    if s > 0 then Result (1, 1)
    else Result (0, 1)
  else
    let mutable won = 0
    let mutable played = 0
    let rnd = new Random()
    let mg = MoveGenerator (graph, turn.factories, player)
    let movesToPlay = rnd.Next(10)
    for i in [0..movesToPlay] do
      let move = mg.nextRandomMove
      let newTurn = nextTurn graph turn move player
      let res = montecarlo graph newTurn (depth - 1) (other player)
      won <- won + res.won
      played <- played + res.played
    Result (won, played)


let rec minimax (graph : int[][]) (turn : Turn) (depth : int) (α : int) (β : int) (player : Player) =
  let mutable a = α
  let mutable b = β
  let mutable v = match player with
                  | MAX -> Microsoft.FSharp.Core.int.MinValue
                  | _ -> Microsoft.FSharp.Core.int.MaxValue
  if depth = 0 || gameover turn then
    score turn
  else
    let mutable firstMove = true
    let mutable prune = false
    let srcFactories = playerFactories player turn.factories
    let mutable src = 0
    while not prune && src < srcFactories.Length do
      let dstFactories = enemyNeighbours graph turn.factories turn.factories.[srcFactories.[src]] player
      let mutable dst = 0
      while not prune && dst < dstFactories.Length do
        let mutable cyborgs = 1
        while not prune && cyborgs < turn.factories.[srcFactories.[src]].cyborgs do
          let move = match firstMove with
                     | true -> Action (WAIT, 0, 0, 0)
                     | _ -> Action (MOVE, src, dst, cyborgs)
          firstMove <- false
          let newTurn = nextTurn graph turn move player
          match player with
          | MAX -> v <- max v (minimax graph newTurn (depth - 1) a b MIN)
                   a <- max a v
                   if b <= a then prune <- true
          | _ -> v <- min v (minimax graph newTurn (depth - 1) a b MAX)
                 b <- min b v
                 if b <= a then prune <- true
          cyborgs <- cyborgs + 1
        dst <- dst + 1
      src <- src + 1
    v


let initFactoriesConnections n =
    [| for i in [0..n-1] do yield [| for j in [0..n-1] do yield -1 |] |]

///////////////////////// GAME START ///////////////////////////////

let factoryCount = int(Console.In.ReadLine()) (* the number of factories *)
let linkCount = int(Console.In.ReadLine()) (* the number of links between factories *)

Console.Error.WriteLine("Factory count: " + factoryCount.ToString())
Console.Error.WriteLine("Links count: " + linkCount.ToString())

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

        Console.Error.WriteLine("EntityId: " + entityId.ToString())
        Console.Error.WriteLine("EntityType: " + entityType.ToString())
        Console.Error.WriteLine("Arg1: " + arg1.ToString())
        Console.Error.WriteLine("Arg2: " + arg2.ToString())
        Console.Error.WriteLine("Arg3: " + arg3.ToString())
        Console.Error.WriteLine("Arg4: " + arg4.ToString())
        Console.Error.WriteLine("Arg5: " + arg5.ToString())

        ()
    
    let factories = mutFactories
    let troops = mutTroops
    
    (* Write an action using printfn *)
    (* To debug: Console.Error.WriteLine("Debug message") *)
    let α = Microsoft.FSharp.Core.int.MinValue
    let β = Microsoft.FSharp.Core.int.MaxValue

    let mutable bestAction = WAIT
    let mutable bestSrc = 0
    let mutable bestDst = 0
    let mutable bestCyborgs = 0

    let mutable best =  Microsoft.FSharp.Core.int.MinValue
    let mutable stop = false
    let mutable firstMove = true

    let mutable src = 0
    let srcFactories = playerFactories MAX factories
    while not stop && src < srcFactories.Length do
      let mutable dst = 0
      let dstFactories = enemyNeighbours graph factories factories.[srcFactories.[src]] MAX
      while not stop && dst < dstFactories.Length do
        let mutable cyborgs = 1
        while not stop && cyborgs < factories.[srcFactories.[src]].cyborgs do
          let move = match firstMove with
                     | true -> Action (WAIT, 0, 0, 0)
                     | _ -> Action (MOVE, srcFactories.[src], dstFactories.[dst], cyborgs)
          firstMove <- false
          let turn = Turn (factories, troops, MAX)
          let newScore = minimax graph turn 1 α β MAX
          if newScore > best then
            if not firstMove then
              bestAction <- MOVE
            bestSrc <- src
            bestDst <- dst
            bestCyborgs <- cyborgs
            else firstMove <- false
          cyborgs <- cyborgs + 1
        dst <- dst + 1
      src <- src + 1
    
    if bestAction = WAIT then printfn "WAIT"
    else printfn "MOVE %d %d %d" bestSrc bestDst bestCyborgs

    (* Any valid action, such as "WAIT" or "MOVE source destination cyborgs" *)
    printfn "WAIT"
    ()
