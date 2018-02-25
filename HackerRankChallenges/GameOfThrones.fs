namespace HackerRankChallenges

open System
open System.Collections.Generic

module GameOfThrones =

    //Solution of "Game of Thrones I"
    let anagramCanBePalindrome (input : string) =
        let rec calculate (cache : Dictionary<char, uint32>) (numberOfOdds : int) (chars : char list) : string =
            match chars with
            | [] -> if (input.Length % 2) = numberOfOdds then "YES" else "NO"
            | hd :: tl ->
                if cache.ContainsKey(hd) then
                    cache.[hd] <- cache.[hd] + 1u
                else
                    cache.Add(hd,1u)
                match cache.[hd] % 2u with
                | 0u -> calculate cache (numberOfOdds-1) tl
                | 1u -> calculate cache (numberOfOdds+1) tl
        calculate (Dictionary<char,uint32>()) 0 (List.ofSeq input)

    ///Reads all input lines and returns a list of them.
    let read_and_parse()=
        let rec read (acc : string list) (input : string) =
            if input = null then
                acc
            else
                read (input :: acc) (Console.ReadLine())
        read [] (Console.ReadLine())

    let solve =
        read_and_parse
        >> List.map anagramCanBePalindrome
        >> List.iter (printfn "%s")