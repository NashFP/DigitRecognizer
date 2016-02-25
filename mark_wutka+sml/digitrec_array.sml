(* Array version of Digit Recognizer (lines of numbers are
 * stored in an array instead of a list for faster access
 *
 * run with sml digitrec_array.sml ../test-sample.csv ../training-sample.csv
 * or
 * compile with MLton:
 *   mlton digitrec_array.sml
 * and run:
 *   digitrec_array ../test-sample.csv ../training-sample.csv
 *
 * MLton is much faster than SML for this on my system:
 * SML/NJ: 15.5 seconds
 * MLton: 4.0 seconds
 *)

(* Splits a comma-separated list of numbers into a pair containing the first
 * number in the list (the match digit) and an array containing the rest of the
 * numbers *)
fun parseLine line = let
  val values = map (fn s => Option.valOf (Int.fromString s))
      (String.tokens (fn c => (c = #",") orelse (c = #";")) line)
in
  (List.hd values, Array.fromList (List.tl values))
end;;

fun parseFile lines =
  map parseLine lines;;

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

(* Compares two arrays of pixels and returns the sum of the squares of the differences *)
fun compareSamples (testCh,testSample) (trainingCh,trainingSample) = let
  fun sqr x = x * x
  fun comparePixels (p1,p2) = sqr (p1 - p2)
  fun pixelSum (i, a, s) = s + comparePixels(a, Array.sub (trainingSample,i))
in
  (trainingCh,
    Array.foldli pixelSum 0 testSample)
end;;

(* Finds the closest match by looking for the pair with the lowest difference score *)
fun recognizeSample testSample trainingSamples = let
  fun minimumScore ((d1,s1),(d2,s2)) = if s1 < s2 then (d1,s1) else (d2,s2)
in
    foldl minimumScore (0-1,999999999) (map (compareSamples testSample) trainingSamples)
end;;

(* Returns true if the recognizer recognized the correct digit *)
fun sampleMatches trainingSamples (testCh,testSample) =
  testCh = #1 (recognizeSample (testCh,testSample) trainingSamples);;

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
