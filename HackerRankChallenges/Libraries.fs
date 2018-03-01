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

        let addTo (v : int) (g : Group) =
            g.Add(v) |> ignore
            cityToGroup.[v] <- g

        let mergeGroups (larger : Group) (smaller : Group) =
            for v in smaller do
                larger.Add(v) |> ignore
                cityToGroup.[v] <- larger

        let makeNewGroup (a : int) (b : int) =
            let ctr = store.Count
            let newSet = HashSet<int>()
            newSet.Add(a) |> ignore
            newSet.Add(b) |> ignore
            cityToGroup.[a] <- newSet
            cityToGroup.[b] <- newSet
            store.[ctr] <- newSet

        let addEdge ((a,b) : Edge) =
            let aHasSet = cityToGroup.ContainsKey a
            let bHasSet = cityToGroup.ContainsKey b

            match aHasSet,bHasSet with
            | false,false ->
                makeNewGroup a b
            | true, false ->
                addTo b (cityToGroup.[a])
            | false, true ->
                addTo a (cityToGroup.[b])
            | true, true ->
                let aSet = cityToGroup.[a]
                let bSet = cityToGroup.[b]
                if aSet.Count > bSet.Count then
                    mergeGroups aSet bSet
                else mergeGroups bSet aSet

        edges |> List.iter addEdge

        store.Values.ToList()
        |> List.ofSeq