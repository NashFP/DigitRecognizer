defmodule DigitRecognizer do
	def run do
		training_sample = read("../training-sample.csv")
		test_sample = read("../test-sample.csv")
		score(test_sample, training_sample)
	end

	def read(path) do
		path
		|> File.stream! 
		|> Stream.drop(1)
		|> Enum.map(&to_observation/1)
	end

	defp to_observation(line) do
		line
		|> String.rstrip(?\n) 
		|> String.split(",") 
		|> Enum.map(&(String.to_integer(&1)))
		|> list_to_observation
	end

	defp list_to_observation([head|tail]) do
		%{label: head, pixels: tail}
	end

	defp score(test_sample, training_sample) do
		hits = Enum.count(test_sample, &(predict(training_sample, &1.pixels)==&1.label))
		total = Enum.count(test_sample)
		hits/total
	end

	defp predict(training_set, mystery_pixels) do
		best = Enum.min_by(training_set, &(distance(&1.pixels, mystery_pixels)))
		best.label
	end

	defp distance(a_pixels, b_pixels) do
		Enum.zip(a_pixels, b_pixels)
		|> List.foldl(0, fn({a,b}, sum) -> sum+abs(a-b) end)
	end
end
