defmodule Day01 do
  defp getInput() do
    IO.read(:all)
    |> String.split("\n")
    |> Enum.filter(fn l -> l != "" end)
  end

  defp isNumber?(c) do
    cond do
      Regex.match?(~r/\d/u, c) -> true
      true -> false
    end
  end

  defp replaceNumberWords(line) do
    line
    |> String.replace("one", "o1ne")
    |> String.replace("two", "t2wo")
    |> String.replace("three", "t3hree")
    |> String.replace("four", "f4our")
    |> String.replace("five", "f5ive")
    |> String.replace("six", "s6ix")
    |> String.replace("seven", "s7even")
    |> String.replace("eight", "e8ight")
    |> String.replace("nine", "n9ine")
  end

  defp parseLine2(line) do
    line
    |> replaceNumberWords()
    |> String.graphemes()
    |> Enum.filter(&isNumber?/1)
    |> then(fn l -> List.first(l) <> List.last(l) end)
    |> Integer.parse()
    |> then(fn {a, _} -> a end)
  end

  defp parseLine(line) do
    line
    |> String.graphemes()
    |> Enum.filter(&isNumber?/1)
    |> then(fn l -> List.first(l) <> List.last(l) end)
    |> Integer.parse()
    |> then(fn {a, _} -> a end)
  end

  defp problem1(input) do
    input
    |> Enum.map(fn l -> parseLine(l) end)
    |> Enum.sum()
  end

  defp problem2(input) do
    input
    |> Enum.map(fn l -> parseLine2(l) end)
    |> Enum.sum()
  end

  def main() do
    input = getInput()
    IO.puts("Part 1: ")

    input
    |> problem1()
    |> IO.puts()

    IO.puts("Part 2: ")

    input
    |> problem2()
    |> IO.puts()
  end
end

Day01.main()
