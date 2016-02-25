(* OCaml Array version of Digit Recognizer (lines of numbers are
 * stored in an array instead of a list for faster access
 * To compile:
 *   ocamlopt -o digitrec_array str.cmxa digitrec_array.ml
 * To run:
 *  digitrec_array ../test-sample.csv ../training-sample.csv
 *
 * On my system it runs in about 10.4 seconds.
 *)

(* Splits a comma-separated list of numbers into a pair containing the first
 * number in the list (the match digit) and an array containing the rest of the
 * numbers *)
let parseLine line =
  let values = List.map int_of_string (Str.split (Str.regexp "[,;]") line)
  in
    (List.hd values, Array.of_list (List.tl values))

let parseFile lines =
  List.map parseLine lines;;

let make_stream filename =
  let channel = open_in filename in
    Stream.from (fun _ ->
      try Some (input_line channel) with End_of_file -> None);;

(* Reads a file and returns its contents as a list of lines *)
let lines filename =
  let rec readLines channel lineList =
    try readLines channel ((input_line channel) :: lineList)
    with End_of_file -> List.rev lineList
  in
    readLines (open_in filename) [];;

let arrayZipFold f init a1 a2 =
  let rec iterator i accum =
    if (i >= Array.length a1) || (i >= Array.length a2) then
      accum
    else
      iterator (i+1) (f (Array.get a1 i) (Array.get a2 i) accum)
  in
    iterator 0 init;;

(* Compares two arrays of pixels and returns the sum of the squares of the differences *)
let compareSamples (testCh,testSample) (trainingCh,trainingSample) =
  let sqr x = x * x in
  let comparePixels p1 p2 = sqr (p1 - p2) in
  let pixelSum p1 p2 s = s + comparePixels p1 p2
  in
  (trainingCh,
    arrayZipFold pixelSum 0 testSample trainingSample);;

(* Finds the closest match by looking for the pair with the lowest difference score *)
let recognizeSample testSample trainingSamples =
  let minimumScore (d1,s1) (d2,s2) = if s1 < s2 then (d1,s1) else (d2,s2)
in
  List.fold_left minimumScore (0-1,999999999) (List.map (compareSamples testSample) trainingSamples);;

(* Returns true if the recognizer recognized the correct digit *)
let sampleMatches trainingSamples (testCh,testSample) =
  testCh = fst (recognizeSample (testCh,testSample) trainingSamples);;

let runme () =
  let testFile = Sys.argv.(1) in
  let trainingFile = Sys.argv.(2) in
  let testSample = parseFile (List.tl (lines testFile)) in
  let trainingSample = parseFile (List.tl (lines trainingFile)) in
  let numMatched = List.length (List.filter (sampleMatches trainingSample) testSample) in
  let numSamples = List.length testSample in
  let successRate = (float_of_int numMatched) /. (float_of_int numSamples)
in
  print_float successRate;
  print_newline();;

runme();;
