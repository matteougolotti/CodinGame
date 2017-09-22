open System

type Factory (owner : int, cyborgs : int, production : int) =
  member this.owner = owner
  member this.cyborgs = cyborgs
  member this.production = production

type Troop (owner : int, src : int, dst : int, cyborgs : int, turnsLeft : int) =
  member this.owner = owner
  member this.src = src
  member this.dst = dst
  member this.cyborgs = cyborgs
  member this.turnsLeft = turnsLeft

let initFactoriesConnections n =
    [| for i in [0..n-1] do yield [| for j in [0..n-1] do yield -1 |] |]

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
        | "FACTORY" -> mutFactories <- Array.append mutFactories [| Factory(arg1, arg2, arg3) |]
        | "TROOP" -> mutTroops <- Array.append mutTroops [| Troop(arg1, arg2, arg3, arg4, arg5) |]
        | _ -> ()

        ()
    
    let factories = mutFactories
    let troops = mutTroops
    
    (* Write an action using printfn *)
    (* To debug: Console.Error.WriteLine("Debug message") *)
    

    (* Any valid action, such as "WAIT" or "MOVE source destination cyborgs" *)
    printfn "WAIT"
    ()