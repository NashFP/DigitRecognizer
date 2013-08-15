open System
open System.IO

let trainingFile = @"c:\temp\trainingsample.csv"
let validationFile = @"c:\temp\validationsample.csv"

type sample = { Label:int; Pixels:int[]}

let fileToSamples path =
    File.ReadAllLines(path)                 // text file to string[]
    |> Array.map(fun s -> s.Split(','))     // string[] to string[][]
    |> (fun s -> s.[1..])                   // remove header
    |> Array.map(fun s ->                   // string[][] to int[][]
        s |> Array.map(fun t -> 
            Convert.ToInt32(t)))            

    |> Array.map(fun s ->                   // int[][] to sample type
        { 
            Label = s.[0]; 
            Pixels = s.[1..]
        })                                  
    

// Math reminder: the euclidean distance is
// distance [ x1; y1; z1 ] [ x2; y2; z2 ] = 
// (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2) 
let getEuclideanDistance firstPixels secondPixels = 
    Array.map2(fun p1 p2 -> (p1 - p2) * (p1 - p2)) firstPixels secondPixels
    |> Array.sum

// read our training samples
let trainingSamples = fileToSamples trainingFile

// classifier picks a training sample with minimum euclidean distance
let classifier pixels =
    trainingSamples 
    |> Array.minBy(fun x -> getEuclideanDistance x.Pixels pixels) 
    |> fun x -> x.Label    

// read our validation samples
let validationSamples = fileToSamples validationFile

// let's test our classifier
let testClassifier =     
    let actualValues = 
        validationSamples 
        |> Array.map(fun x -> classifier x.Pixels)

    let expectedValues = Array.map(fun x -> x.Label) validationSamples

    Array.map2(fun a e -> a = e) actualValues expectedValues
    |> fun s -> (double)(s |> Array.filter(fun x -> x = true)).Length 
                / (double)s.Length
    |> fun s -> s * (double)100    
