defmodule Day08 do
  defp getInput() do
    IO.read(:all)
    |> String.split("\n")
    |> Enum.filter(fn l -> l != "" end)
  end

  defp parseInput(input) do
    {
      input
      |> hd()
      |> String.graphemes()
      |> Enum.map(fn x ->
        case x do
          "L" -> :left
          "R" -> :right
        end
      end),
      input
      |> Enum.drop(1)
      |> Enum.map(fn l ->
        l
        |> String.split(" = ")
        |> then(fn [id, x] ->
          {String.trim(id),
           x
           |> String.replace("(", "")
           |> String.replace(")", "")
           |> String.replace(",", "")
           |> String.split(" ")
           |> then(fn [l, r] -> %{left: l, right: r} end)}
        end)
      end)
      |> Map.new()
    }
  end

  defp problem1(input) do
    {ins, map} = input

    cond do
      "AAA" not in Map.keys(map) ->
        "AAA not in map. Unable to proceed"

      true ->
        Stream.cycle(ins)
        |> Stream.scan("AAA", fn turn, pos ->
          map[pos][turn]
        end)
        |> Enum.take_while(&(&1 != "ZZZ"))
        |> length()
        |> then(&(&1 + 1))
    end
  end

  defp cycleLength({i, node}, ins, map, viewedStates) do
    key = {rem(i, length(ins)), node}
    turn = ins |> Enum.at(rem(i, length(ins)))

    case Map.has_key?(viewedStates, key) do
      true ->
        {viewedStates[key], i, i - viewedStates[key]}

      false ->
        cycleLength({i + 1, map[node][turn]}, ins, map, viewedStates |> Map.put_new(key, i))
    end
  end

  # k 12 -> 'Z'
  # 12643*n + 12 -> 'Z'
  defp cycleLength(startingNode, ins, map), do: cycleLength({0, startingNode}, ins, map, %{})

  defp problem2(input) do
    {ins, map} = input

    startingNodes = map |> Map.keys() |> Enum.filter(fn x -> x |> String.ends_with?("A") end)

    # I've used the following to explore the data, and get
    # for each node, the start and end of a cycle, the cycle length
    # and the position of 'Z's.
    # Observations:
    # - Only one position with 'Z' per starting node
    # - The position that ends with 'Z' happens to be the cycle length
    #
    # For that, the result is the gcd of the cycle lengths

    # startingNodes
    # |> Enum.map(fn node ->
    #   cycleLength(node, ins, map)
    #   |> then(fn {start, finish, cLength} ->
    #     Stream.cycle(ins)
    #     |> Stream.scan(node, fn turn, pos -> map[pos][turn] end)
    #     |> Enum.take(cLength * 2)
    #     |> Enum.zip(1..(cLength * 2))
    #     |> Enum.filter(fn {x, i} -> String.ends_with?(x, "Z") or i == start or i == finish end)
    #     |> then(fn x -> {node, start, finish, cLength, x} end)
    #   end)
    # end)

    startingNodes
    |> Enum.map(fn node ->
      cycleLength(node, ins, map)
      |> then(fn {_, _, cLength} -> cLength end)
    end)
    |> Enum.reduce(fn a, b -> div(a * b, Integer.gcd(a, b)) end)
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

Day08.main()
