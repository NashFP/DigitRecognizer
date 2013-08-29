namespace DigitRecognizer

open System

module Processor =
    type sample = { Label:int; Pixels:int[]}

    let pixelsNum = 28

    let pixelsListTo2D (pixels:int[]) =
        Array2D.init pixelsNum pixelsNum (fun i j -> pixels.[(pixelsNum * i) + j])
    
    let stringToSamples (raw:string) = 
        raw.Split([|"\r\n"|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map(fun s -> s.Split(','))     // string[] to string[][]
        |> (fun s -> s.[1..])                   // remove header
        |> Array.map(fun s ->                   // string[][] to int[][]
            s |> Array.map(fun t -> 
                Convert.ToInt32(t)))            

        |> Array.map(fun s ->                   // int[][] to sample type
            { 
                Label = s.[0]; 
                Pixels = s.[1..];                
            })   
            
    // Math reminder: the euclidean distance is
    // distance [ x1; y1; z1 ] [ x2; y2; z2 ] = 
    // (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2) 
    let getEuclideanDistance firstPixels secondPixels = 
        Array.map2(fun p1 p2 -> (p1 - p2) * (p1 - p2)) firstPixels secondPixels
        |> Array.sum
    
    // classifier picks a training sample with minimum euclidean distance
    let classifier samples pixels =        
        samples 
        |> Array.map(fun x -> (x, getEuclideanDistance x.Pixels pixels))
        |> Array.minBy(fun x -> snd(x))