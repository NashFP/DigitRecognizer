// F# machine learning digit recognizer
// See instructions at http://git.io/XfQLeQ

open System.IO

// Record type to store digit data
type Record = { Digit: int; Pixels: int[] }

// Function to load the data
let read file =
    File.ReadAllLines(file).[1..]
    |> Array.map (fun line -> line.Split(',') |> Array.map int )
    |> Array.map (fun line -> { Digit = line.[0]; Pixels = line.[1..] })

// Load the data
let training = read @"c:\trainingsample.csv"
let data = read @"c:\validationsample.csv"

// Function to compute difference between two digits
let diff x y =
    Seq.zip x.Pixels y.Pixels
    |> Seq.map (fun (x, y) -> (x - y) * (x - y))
    |> Seq.sum

// Function to find the closest matching digit
let classify x =
    (training
    |> Seq.minBy (fun t -> diff x t)).Digit

// Compute the pairs of actual versus expected digits
// Use Seq.cache so it is only slow the first time enumerating
let pairs =
    data
    |> Seq.map (fun x -> classify x, x.Digit)
    |> Seq.cache

// Function to print the pairs
let print() =
    pairs
    |> Seq.iter (printfn "%A")

// Function to calculate the success rate
let rate() =
    (/) (pairs |> Seq.filter (fun (x, y) -> x = y) |> Seq.length |> float)
        (pairs |> Seq.length |> float)

// Print the pairs and the success rate
print()
rate()
