// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.Collections.Generic
open System.Windows.Forms
open System.IO
open Sample.Support


// A dummy type, used to hook our own assembly.  A common .NET reflection idiom
type ThisAssem = { dummy: int }

/// <summary>
/// The main entry point for the application.
/// </summary>
[<STAThread>]
[<EntryPoint>]
let main(args) = 
    let assem = (typeof<ThisAssem>).Assembly 
    let harnesses = getSamples(assem)
    match args with 
    | [| _; "/runall" |] -> 
        harnesses 
        |> List.iter (fun (_,samples) -> samples |> List.iter (fun s -> if s.Name <> "ExceptionSample1" then s.Run()))
    | _ -> 
        Application.EnableVisualStyles();
        let form = new Display.SampleForm("F# Micro Samples Explorer", harnesses) 
        ignore(form.ShowDialog())
    0
