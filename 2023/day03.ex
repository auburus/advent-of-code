defmodule Day02 do
  defp getInput() do
    IO.read(:all)
    |> String.split("\n")
    |> Enum.filter(fn l -> l != "" end)
  end

  defp parseGrid(xs), do: parseGrid(xs, {0, 0}, %{})
  defp parseGrid([], _, grid), do: grid
  defp parseGrid(["\n" | xs], {x, _y}, grid), do: parseGrid(xs, {x + 1, 0}, grid)
  defp parseGrid([c | cs], {x, y}, grid), do: parseGrid(cs, {x, y + 1}, Map.put(grid, {x, y}, c))

  defp isNumber?(index, grid) do
    cond do
      grid[index] in String.graphemes("0123456789") -> true
      true -> false
    end
  end

  defp isSymbol?(index, grid) do
    cond do
      grid[index] == nil -> false
      grid[index] == "." -> false
      isNumber?(index, grid) -> false
      true -> true
    end
  end

  defp locateNumbers(grid) do
    grid
    |> Map.keys()
    |> Enum.sort()
    |> then(fn indices -> doLocateNumbers(grid, indices, []) end)
  end

  defp doLocateNumbers(_grid, [], acc), do: acc

  defp doLocateNumbers(grid, [i | indices], acc) do
    cond do
      not isNumber?(i, grid) ->
        doLocateNumbers(grid, indices, acc)

      true ->
        number =
          [i | indices]
          # Only current row
          |> Enum.take_while(fn {x, _} -> x == Kernel.elem(i, 0) end)
          |> Enum.take_while(fn i -> isNumber?(i, grid) end)
          |> Enum.map(fn i -> grid[i] end)
          |> List.to_string()

        doLocateNumbers(
          grid,
          [i | indices] |> Enum.drop(number |> String.length()),
          [{i, number} | acc]
        )
    end
  end

  defp isPartNumber?({{x, y}, number}, grid) do
    numberIndices =
      0..(String.length(number) - 1)
      |> Enum.map(fn i -> {x, y + i} end)

    surroundings =
      numberIndices
      |> Enum.map(fn {x, y} -> for i <- -1..1, j <- -1..1, do: {x + i, y + j} end)
      |> List.flatten()
      |> Enum.dedup()

    surroundings
    |> Enum.map(fn i -> isSymbol?(i, grid) end)
    |> Enum.any?()
  end

  defp problem1(input) do
    input
    |> locateNumbers()
    |> Enum.filter(fn x -> isPartNumber?(x, input) end)
    |> Enum.map(fn {_pos, number} -> Integer.parse(number) |> Kernel.elem(0) end)
    |> Enum.sum()
  end

  defp locateMaybeParts(grid) do
    grid
    |> Map.keys()
    |> Enum.filter(fn x -> grid[x] == "*" end)
  end

  defp findAdjacentNumbers({x, y}, grid) do
    for i <- -1..1, j <- -1..1 do
      {x + i, y + j}
    end
    |> Enum.filter(fn i -> isNumber?(i, grid) end)
    |> Enum.map(fn x -> findFullNumberForPos(x, grid) end)
    |> Enum.sort()
    |> Enum.dedup()
  end

  # Assume {x,y} is a number
  defp findFullNumberForPos({x, y}, grid) do
    previous =
      Stream.iterate({x, y}, fn {x, y} -> {x, y - 1} end)
      |> Enum.take_while(fn x -> isNumber?(x, grid) end)

    next =
      Stream.iterate({x, y}, fn {x, y} -> {x, y + 1} end)
      |> Enum.take_while(fn x -> isNumber?(x, grid) end)

    (previous ++ next)
    |> Enum.sort()
    |> Enum.dedup()
    |> then(fn xs -> {hd(xs), xs |> Enum.map(fn pos -> grid[pos] end) |> List.to_string()} end)
  end

  defp problem2(input) do
    input
    |> locateMaybeParts()
    |> Enum.sort()
    |> Enum.map(fn x -> findAdjacentNumbers(x, input) end)
    |> Enum.filter(fn adj -> length(adj) == 2 end)
    |> Enum.map(fn x ->
      x
      |> Enum.map(fn {_, b} -> b end)
      |> Enum.map(&Integer.parse/1)
      |> Enum.map(fn {a, _} -> a end)
      |> Enum.product()
    end)
    |> Enum.sum()
  end

  def main() do
    input =
      getInput()
      |> Enum.join("\n")
      |> String.graphemes()
      |> parseGrid()

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

Day02.main()
