(* SML port of Haskell Digit Recognizer
*  run with: sml digitrec.sml ../test-sample.csv ../training-sample.csv
* or
*  compile with MLton:
*    mlton digitrec.sml
*  and run:
*    digitrec ../test-sample.csv ../training-sample.csv
*
* MLton seems to run much slower than SML/NJ for this version on my system:
* SML/NJ: 36.6 seconds
* MLton: 78.8 seconds
*)

(* Splits a comma-separated list of numbers into a list of ints *)
fun parseLine line =
  map (fn s => Option.valOf (Int.fromString s))
      (String.tokens (fn c => (c = #",") orelse (c = #";")) line);;

fun parseFile lines =
  map parseLine lines;;

(* Takes a function and two lists and applies the func to each nth element *)
fun zipWith f [] _ = []
  | zipWith f _ [] = []
  | zipWith f (x::xs) (y::ys) = f(x,y) :: (zipWith f xs ys);;

(* Removes newlines and carriage returns from a string *)
fun strip s =
  String.implode (List.filter (fn c => (c <> #"\r") andalso (c <> #"\n")) (String.explode s));;

(* Reads a file and returns its contents as a list of lines *)
fun lines filename = let
  val ins = TextIO.openIn filename
  fun readLoop ins =
    case TextIO.inputLine ins of
        SOME line => (strip line) :: readLoop ins
      | NONE => []
in
  readLoop ins before TextIO.closeIn ins
end;;

(* Compares two lists of pixels and returns the sum of the squares of the differences *)
fun compareSamples testSample trainingSample = let
  fun sqr x = x * x
  fun comparePixels (p1,p2) = sqr (p1 - p2)
in
  (List.hd trainingSample,
    List.foldl op+ 0 (zipWith comparePixels (List.tl testSample) (List.tl trainingSample)))
end;;

(* Finds the closest match by looking for the pair with the lowest difference score *)
fun recognizeSample testSample trainingSamples = let
  fun minimumScore ((d1,s1),(d2,s2)) = if s1 < s2 then (d1,s1) else (d2,s2)
in
    foldl minimumScore (0-1,999999999) (map (compareSamples testSample) trainingSamples)
end;;

(* Returns true if the recognizer recognized the correct digit *)
fun sampleMatches trainingSamples testSample =
  List.hd testSample = #1 (recognizeSample testSample trainingSamples);;

fun runme () = let
  val args = CommandLine.arguments()
  val testFile = List.hd args
  val trainingFile = List.hd (List.tl args)
  val testSample = parseFile (List.tl (lines testFile))
  val trainingSample = parseFile (List.tl (lines trainingFile))
  val numMatched = List.length (List.filter (sampleMatches trainingSample) testSample)
  val numSamples = List.length testSample
  val successRate = (Real.fromInt numMatched) / (Real.fromInt numSamples)
in
  print (Real.toString successRate);
  print "\n"
end;;

runme();;
val () = OS.Process.exit OS.Process.success;;
