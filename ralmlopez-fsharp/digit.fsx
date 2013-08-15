open System
open System.IO

type Record = { Label:int; Pixels:int[] }

let loadData path =
    let data = File.ReadAllLines(path)
 
    let splitCommas (row:string) = 
        row.Split([|','|])

    let toInteger s =
        Array.map (fun a -> Int32.Parse(a)) s

    let removeHeader (d:string[]) = 
        d.[1..(d.Length - 1)]

    let toRecords (r:int[]) =
        {Label = r.[0] ; Pixels = r.[1..r.Length - 1] };
      
    data
        |> removeHeader
        |> Array.map splitCommas
        |> Array.map toInteger
        |> Array.map toRecords 
              
let trainingData = loadData @"C:\trainingsample.csv"
let validationData = loadData @"C:\validationsample.csv"

let distance p1 p2 = 
    let squareDifference data1 data2 = 
        let square x = x * x  

        Array.map2 (fun p1 p2 -> square (p1 - p2)) data1 data2 

    squareDifference p1 p2
        |> Array.sum
        |> float
        |> Math.Sqrt

type DistanceRecord = { Label:int; Distance:float; }

let findClosest unknown = 
    trainingData
    |> Array.map (fun d -> {Label = d.Label; Distance = distance d.Pixels unknown})
    |> Array.minBy (fun x -> x.Distance)

type ResultRecord = { Label:int; Guess:int; }

let efficiency  =
    let percentage x y =
        ((float)y / (float)x) * 100.0
           
    validationData
        |> Array.map (fun v -> {Label = v.Label; Guess = (findClosest v.Pixels).Label})
        |> Array.map (fun x -> if x.Label = x.Guess then 1 else 0)
        |> Array.sum
        |> percentage validationData.Length
