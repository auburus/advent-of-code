defmodule Day05 do
  defp getInput() do
    IO.read(:all)
    |> String.split("\n")
    |> Enum.filter(fn l -> l != "" end)
  end

  defp parseInput(input) do
    time =
      input
      |> hd()
      |> String.split("Time:")
      |> Enum.drop(1)
      |> hd()
      |> String.trim(" ")
      |> String.split(" ")
      |> Enum.filter(fn x -> x != "" end)
      |> Enum.map(fn x -> x |> String.trim(" ") |> Integer.parse() |> elem(0) end)

    distance =
      input
      |> Enum.drop(1)
      |> hd()
      |> String.split("Distance:")
      |> Enum.drop(1)
      |> hd()
      |> String.trim(" ")
      |> String.split(" ")
      |> Enum.filter(fn x -> x != "" end)
      |> Enum.map(fn x -> x |> String.trim(" ") |> Integer.parse() |> elem(0) end)

    Enum.zip(time, distance)
  end

  defp parseInput2(input) do
    time =
      input
      |> hd()
      |> String.split("Time:")
      |> Enum.drop(1)
      |> hd()
      |> String.replace(" ", "")
      |> then(fn x -> x |> Integer.parse() |> elem(0) end)

    distance =
      input
      |> Enum.drop(1)
      |> hd()
      |> String.split("Distance:")
      |> Enum.drop(1)
      |> hd()
      |> String.replace(" ", "")
      |> then(fn x -> x |> Integer.parse() |> elem(0) end)

    {time, distance}
  end

  # x * (T - x) >= D
  # -x^2 + Tx -D >= 0
  # x^2 - Tx + D <= 0

  # -T +- sqrt(T^2 - 4*D) / 2
  defp problem1(input) do
    input
    |> Enum.map(fn {t, d} ->
      {floor((t - :math.sqrt(t * t - 4 * d)) / 2), ceil((t + :math.sqrt(t * t - 4 * d)) / 2)}
      |> then(fn {a, b} -> b - 1 - (a + 1) + 1 end)
    end)
    |> Enum.product()
  end

  defp problem2(input) do
    input
    |> then(fn x -> problem1([x]) end)
  end

  def main() do
    input =
      getInput()

    IO.puts("Part 1: ")

    input
    |> parseInput()
    |> problem1()
    # |> Enum.map(&IO.inspect/1)
    |> IO.inspect(charlists: :as_lists)

    IO.puts("Part 2: ")

    input
    |> parseInput2()
    |> problem2()
    # |> Enum.map(&IO.inspect/1)
    |> IO.inspect(charlists: :as_lists)
  end
end

Day05.main()
