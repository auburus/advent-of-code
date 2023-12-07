defmodule Day02 do
  defp getInput() do
    IO.read(:all)
    |> String.split("\n")
    |> Enum.filter(fn l -> l != "" end)
  end

  defp parseGame(line) do
    numbers = String.graphemes("0123456789")

    game_id =
      line
      |> String.graphemes()
      |> Enum.drop_while(fn c -> not Enum.member?(numbers, c) end)
      |> Enum.take_while(fn c -> Enum.member?(numbers, c) end)
      |> Enum.join("")
      |> Integer.parse()
      |> Tuple.to_list()
      |> hd

    sets =
      line
      |> String.split(":")
      |> Enum.drop(1)
      |> hd
      |> String.trim(" ")
      |> String.split(";")
      |> Enum.map(&parseSet/1)

    %{game_id: game_id, sets: sets}
  end

  defp parseSet(set) do
    set
    |> String.replace(",", "")
    |> String.trim()
    |> String.split(" ")
    |> parseColor()
  end

  defp parseColor(xs), do: parseColor(xs, %{blue: 0, green: 0, red: 0})
  defp parseColor([], acc), do: acc

  defp parseColor([x, "blue" | xs], acc),
    do: parseColor(xs, %{acc | :blue => Integer.parse(x) |> Tuple.to_list() |> hd})

  defp parseColor([x, "green" | xs], acc),
    do: parseColor(xs, %{acc | :green => Integer.parse(x) |> Tuple.to_list() |> hd})

  defp parseColor([x, "red" | xs], acc),
    do: parseColor(xs, %{acc | :red => Integer.parse(x) |> Tuple.to_list() |> hd})

  # game_id = Regex.named_captures(~r/Game (?<id>\d+):/, line)
  # game_id = line
  # |> Regex.named_captures()
  # |> String.split(":")
  # |> then(fn [])

  defp possibleSet?(%{:red => red, :blue => blue, :green => green}, %{
         :red => total_red,
         :blue => total_blue,
         :green => total_green
       }) do
    cond do
      red > total_red -> false
      blue > total_blue -> false
      green > total_green -> false
      true -> true
    end
  end

  defp possibleGame?(game, total_marbles) do
    game[:sets]
    |> Enum.map(fn set -> possibleSet?(set, total_marbles) end)
    |> Enum.all?()
  end

  defp problem1(input) do
    input
    |> Enum.filter(fn game -> possibleGame?(game, %{red: 12, green: 13, blue: 14}) end)
    |> Enum.map(fn game -> game[:game_id] end)
    |> Enum.sum()

    # |> Enum.map(fn l -> parseLine(l) end)
    # |> Enum.sum()
  end

  defp minimumMarbles(game) do
    game[:sets]
    |> Enum.reduce(fn set, minimum ->
      %{
        red: max(set[:red], minimum[:red]),
        blue: max(set[:blue], minimum[:blue]),
        green: max(set[:green], minimum[:green])
      }
    end)
  end

  defp power(marbles), do: marbles[:red] * marbles[:blue] * marbles[:green]

  defp problem2(input) do
    input
    |> Enum.map(&minimumMarbles/1)
    |> Enum.map(&power/1)
    |> Enum.sum()

    # |> Enum.map(fn l -> parseLine2(l) end)
    # |> Enum.sum()
  end

  def main() do
    input =
      getInput()
      |> Enum.map(&parseGame/1)

    # |> Enum.map(fn l -> l <> "\n" end)

    IO.puts("Part 1: ")

    input
    |> problem1()
    # |> Enum.map(&IO.inspect/1)
    |> IO.puts()

    IO.puts("Part 2: ")

    input
    |> problem2()
    # |> Enum.map(&IO.inspect/1)
    |> IO.puts()
  end
end

Day02.main()
