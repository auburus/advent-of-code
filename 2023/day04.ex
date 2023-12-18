defmodule Day04 do
  defp getInput() do
    IO.read(:all)
    |> String.split("\n")
    |> Enum.filter(fn l -> l != "" end)
  end

  defp points(winners, numbers) do
    MapSet.new(winners)
    |> MapSet.intersection(MapSet.new(numbers))
    |> MapSet.size()
    |> then(fn size ->
      cond do
        size == 0 -> 0
        true -> :math.pow(2, size - 1) |> round
      end
    end)
  end

  defp cards_won(winners, numbers) do
    MapSet.new(winners)
    |> MapSet.intersection(MapSet.new(numbers))
    |> MapSet.size()
  end

  defp problem1(input) do
    input
    |> Enum.map(fn {winners, numbers} -> points(winners, numbers) end)
    |> Enum.sum()
  end

  defp problem2(input) do
    copies =
      input
      |> Enum.with_index(1)
      |> Enum.reduce(Map.new(), fn {_, i}, acc -> acc |> Map.put_new(i, 1) end)

    input
    |> Enum.with_index(offset = 1)
    |> Enum.reduce(acc = copies, fn {{winners, numbers}, i}, copies ->
      Stream.iterate(1, &(&1 + 1))
      |> Stream.drop(i)
      |> Enum.take(cards_won(winners, numbers))
      |> Enum.reduce(copies, fn index_to_update, copies ->
        copies
        |> Map.update!(index_to_update, fn x ->
          x + copies[i]
        end)
      end)
    end)
    |> Map.values()
    |> Enum.sum()
  end

  defp parseInput(line) do
    [winners, numbers] =
      line
      |> String.split(":")
      |> Enum.drop(1)
      |> List.flatten()
      |> hd
      |> String.trim()
      |> String.split(" | ")
      |> Enum.map(fn x ->
        x
        |> String.split(" ")
        |> Enum.map(&String.trim/1)
        |> Enum.filter(fn x -> x != "" end)
      end)

    {winners, numbers}
  end

  def main() do
    input =
      getInput()
      |> Enum.map(&parseInput/1)

    # IO.inspect(Enum.sort(input))
    IO.puts("Part 1: ")

    input
    |> problem1()
    # |> Enum.map(&IO.inspect/1)
    |> IO.inspect()

    IO.puts("Part 2: ")

    input
    |> problem2()
    # |> Enum.map(&IO.inspect/1)
    |> IO.inspect()
  end
end

Day04.main()
