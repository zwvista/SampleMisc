// Copyright (c) Microsoft Corporation 2005-2009

module ZWSamples
open System
open Sample.Support

//--------------------------------------------------------
  
[<Support("Sample1")>]
let print1 (text : string) offset =
    text.ToCharArray() 
    |> Array.mapi (fun i c -> (i, c))
    |> Seq.groupBy (fun (i, _) -> i % offset)
    |> Seq.map (fun (_, s) ->
        s
        |> Seq.map (fun (_, c) -> c.ToString()) 
        |> Seq.reduce (fun c1 c2 -> c2 + "|" + c1))
    |> Seq.iter (fun str -> printfn "%s" str)

[<Category("趣味编程");
  Title("静夜思1");
  Description("静夜思")>]
let Sample1() =
    print1 "床前明月光疑是地上霜举头望明月低头思故乡" 5

[<Support("Sample2")>]
let print2 (text : string) offset =
    query {
        for (c, i) in Seq.zip text (seq {0 .. text.Length - 1}) do
        groupValBy (string c) (i % offset) into g
        select (g |> Seq.reduce (fun c1 c2 -> c2 + "|" + c1))
    } |> Seq.iter (printfn "%s")

[<Category("趣味编程");
  Title("静夜思2");
  Description("静夜思")>]
let Sample2() =
    print2 "床前明月光疑是地上霜举头望明月低头思故乡" 5

