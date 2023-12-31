defmodule Day07 do
  # wrong answer: 249942407

  @cardvalues %{
    "2" => 2,
    "3" => 3,
    "4" => 4,
    "5" => 5,
    "6" => 6,
    "7" => 7,
    "8" => 8,
    "9" => 9,
    "T" => 10,
    "J" => 11,
    "Q" => 12,
    "K" => 13,
    "A" => 14
  }

  @cardvalues2 %{
    "J" => 1,
    "2" => 2,
    "3" => 3,
    "4" => 4,
    "5" => 5,
    "6" => 6,
    "7" => 7,
    "8" => 8,
    "9" => 9,
    "T" => 10,
    "Q" => 12,
    "K" => 13,
    "A" => 14
  }

  @score %{
    five_of_a_kind: 6,
    four_of_a_kind: 5,
    full_house: 4,
    three_of_a_kind: 3,
    two_pair: 2,
    one_pair: 1,
    high_card: 0
  }

  defp getInput() do
    IO.read(:all)
    |> String.split("\n")
    |> Enum.filter(fn l -> l != "" end)
  end

  defp handStrength(hand) do
    case hand
         |> Enum.frequencies()
         |> Map.values()
         |> Enum.filter(&(&1 != 1))
         |> Enum.sort(:desc) do
      [5] -> :five_of_a_kind
      [4] -> :four_of_a_kind
      [3, 2] -> :full_house
      [3] -> :three_of_a_kind
      [2, 2] -> :two_pair
      [2] -> :one_pair
      [] -> :high_card
    end
  end

  defp handStrengthWithJokers(hand) do
    handWithoutJokers = hand |> Enum.filter(&(&1 != "J"))

    case length(handWithoutJokers) do
      0 ->
        :five_of_a_kind

      1 ->
        :five_of_a_kind

      2 ->
        case handStrength(handWithoutJokers) do
          :one_pair -> :five_of_a_kind
          :high_card -> :four_of_a_kind
        end

      3 ->
        case handStrength(handWithoutJokers) do
          :three_of_a_kind -> :five_of_a_kind
          :one_pair -> :four_of_a_kind
          :high_card -> :three_of_a_kind
        end

      4 ->
        case handStrength(handWithoutJokers) do
          :four_of_a_kind -> :five_of_a_kind
          :three_of_a_kind -> :four_of_a_kind
          :two_pair -> :full_house
          :one_pair -> :three_of_a_kind
          :high_card -> :one_pair
        end

      5 ->
        handStrength(handWithoutJokers)
    end
  end

  defp parseInput(input) do
    input
    |> Enum.map(fn l ->
      l
      |> String.split(" ")
      |> then(fn [a, b] -> {a |> String.graphemes(), Integer.parse(b) |> elem(0)} end)
    end)
  end

  defp problem1(input) do
    input
    |> Enum.sort(fn {hand1, _}, {hand2, _} ->
      cond do
        @score[handStrength(hand1)] < @score[handStrength(hand2)] ->
          true

        @score[handStrength(hand1)] == @score[handStrength(hand2)] ->
          Enum.zip(hand1, hand2)
          |> Enum.drop_while(fn {x, y} -> x == y end)
          |> hd()
          |> then(fn {x, y} -> @cardvalues[x] < @cardvalues[y] end)

        true ->
          false
      end
    end)
    |> Enum.zip(1..length(input))
    |> Enum.map(fn {{_, bid}, pos} -> bid * pos end)
    |> Enum.sum()
  end

  defp problem2(input) do
    input
    |> Enum.sort(fn {hand1, _}, {hand2, _} ->
      cond do
        @score[handStrengthWithJokers(hand1)] < @score[handStrengthWithJokers(hand2)] ->
          true

        @score[handStrengthWithJokers(hand1)] == @score[handStrengthWithJokers(hand2)] ->
          Enum.zip(hand1, hand2)
          |> Enum.drop_while(fn {x, y} -> x == y end)
          |> hd()
          |> then(fn {x, y} -> @cardvalues2[x] < @cardvalues2[y] end)

        true ->
          false
      end
    end)
    |> Enum.zip(1..length(input))
    |> Enum.map(fn {{_, bid}, pos} -> bid * pos end)
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

Day07.main()
