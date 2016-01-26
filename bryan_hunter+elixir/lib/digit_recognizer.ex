defmodule DigitRecognizer do

	defp list_to_observation([head|tail]) do
		%{label: head, pixels: tail}
	end

	def to_observation(line) do
		line
		|> String.rstrip(?\n) 
		|> String.split(",") 
		|> Enum.map(&(String.to_integer(&1)))
		|> list_to_observation
	end

	def read(path) do
		path
		|> File.stream! 
		|> Stream.drop(1)
		|> Enum.map(&to_observation/1)
	end

	defp distance_loop([], [], sum), do: sum

	defp distance_loop([a_head|a_tail], [b_head|b_tail], sum) do
		distance_loop(a_tail, b_tail, sum+abs(a_head-b_head))
	end

	def distance(a_pixels, b_pixels) do
		distance_loop(a_pixels, b_pixels, 0)
	end

	def predict_loop_for_best([], _mystery_pixels, closest_label, _closest_distance) do
		closest_label
	end
	
	def predict_loop_for_best([observation|tail], mystery_pixels, closest_label, closest_distance) do
		dist = distance(observation.pixels, mystery_pixels)

		if (closest_distance == nil or dist < closest_distance) do
			predict_loop_for_best(tail, mystery_pixels, observation.label, dist)
		else
			predict_loop_for_best(tail, mystery_pixels, closest_label, closest_distance)
		end
	end

	def predict(training_set, mystery_pixels) do
		# for each observation in the trainingSet compare its distance from the mystery_pixels.
		# Keep the label of the observation with the smallest distance
		predict_loop_for_best(training_set, mystery_pixels, nil, nil)
	end

	def score(test_sample, training_sample) do
		hits = 
			test_sample 
			|> Enum.count &(predict(training_sample, &1.pixels) == &1.label)

		hits / Enum.count(test_sample)
	end

	def run do
		training_sample = read("../../training-sample.csv")
		test_sample = read("../../test-sample.csv")
		score(test_sample, training_sample)
	end
end
