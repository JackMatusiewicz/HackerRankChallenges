namespace HackerRankChallenges

open System
open System.Numerics

module BigSorting =

    let getLines()=
        let rec read (acc : string list) (input : string) =
            if input = null then
                acc
            else
                read (input :: acc) (Console.ReadLine())
        read [] (Console.ReadLine())

    let private compare (a : string) (b : string) =
        let rec comp (a : char list) (b : char list) =
            if a = [] then
                0
            else
                let aHead = List.head a
                let bHead = List.head b
                if aHead = bHead then
                    comp (List.tail a) (List.tail b)
                else
                    (int aHead) - (int bHead)

        if a.Length <> b.Length then
            a.Length - b.Length
        else
            comp (a.ToCharArray() |> List.ofArray) (b.ToCharArray() |> List.ofArray)

    let sort () =
        [
            "31415926535897932384626433832795"
            "1"
            "3"
            "10"
            "3"
            "5"
        ]
        |> List.sortWith compare
        |> List.iter (printfn "%s")