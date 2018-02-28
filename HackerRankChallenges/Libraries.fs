namespace HackerRankChallenges

open System
open System.Collections.Generic
open System.Linq

module Libraries =

    type Group = HashSet<int>
    type GroupStore = Dictionary<int, Group>

    type Edge = (int * int)

    let constructGroups (edges : Edge list) : Group list =
        let store = GroupStore ()
        let cityToGroup = Dictionary<int, Group> ()

        let addEdge ((a,b) : Edge) =
            let aHasSet = cityToGroup.ContainsKey a
            let bHasSet = cityToGroup.ContainsKey b

            match aHasSet,bHasSet with
            | false,false ->
                let ctr = store.Count
                let newSet = HashSet<int>()
                newSet.Add(a) |> ignore
                newSet.Add(b) |> ignore
                cityToGroup.[a] <- newSet
                cityToGroup.[b] <- newSet
                store.[ctr] <- newSet
            | true, false ->
                let aSet = cityToGroup.[a]
                aSet.Add(b) |> ignore
                cityToGroup.[b] <- aSet
            | false, true ->
                let bSet = cityToGroup.[b]
                bSet.Add(a) |> ignore
                cityToGroup.[a] <- bSet
            | true, true ->
                let aSet = cityToGroup.[a]
                let bSet = cityToGroup.[b]

                for b in bSet do
                    aSet.Add(b) |> ignore
                    cityToGroup.[b] <- aSet

        edges |> List.iter addEdge

        store.Values.ToList()
        |> List.ofSeq