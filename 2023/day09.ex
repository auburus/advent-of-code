defmodule Day09 do
  defp getInput() do
    IO.read(:all)
    |> String.split("\n")
    |> Enum.filter(fn l -> l != "" end)
  end

  defp parseInput(input) do
    input
    |> Enum.map(fn x -> x |> String.split(" ") |> Enum.map(&(Integer.parse(&1) |> elem(0))) end)
  end

  defp expandSequence(sequence) do
    diff_sequence =
      sequence |> Enum.zip(sequence |> Enum.drop(1)) |> Enum.map(fn {a, b} -> b - a end)

    cond do
      Enum.all?(diff_sequence, &(&1 == 0)) ->
        [hd(sequence) | sequence]

      true ->
        sequence ++ [List.last(sequence) + (diff_sequence |> expandSequence() |> List.last())]
    end
  end

  defp expandSequenceBackwards(sequence) do
    diff_sequence =
      sequence |> Enum.zip(sequence |> Enum.drop(1)) |> Enum.map(fn {a, b} -> b - a end)

    cond do
      Enum.all?(diff_sequence, &(&1 == 0)) -> [hd(sequence) | sequence]
      true -> [hd(sequence) - (diff_sequence |> expandSequenceBackwards() |> hd()) | sequence]
    end
  end

  defp problem1(input) do
    input
    |> Enum.map(&expandSequence/1)
    |> Enum.map(&List.last/1)
    |> Enum.sum()
  end

  defp problem2(input) do
    input
    |> Enum.map(&expandSequenceBackwards/1)
    |> Enum.map(&hd/1)
    |> Enum.sum()
  end

  def main() do
    input =
      getInput()
      |> parseInput()

    # IO.inspect(Enum.sort(input))
    IO.puts("Part 1: ")

    input
    |> problem1()
    # |> Enum.map(&IO.inspect/1)
    |> IO.inspect(charlists: :as_lists)

    IO.puts("Part 2: ")

    input
    |> problem2()
    # |> Enum.map(&IO.inspect/1)
    |> IO.inspect(charlists: :as_lists)
  end
end

Day09.main()
