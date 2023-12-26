defmodule Day05 do
  defp getInput() do
    IO.read(:all)
    # |> String.split("\n")
    # |> Enum.filter(fn l -> l != "" end)
  end

  defp parseInt(int) do
    int |> Integer.parse() |> elem(0)
  end

  defp map_value(value, map) do
    case map[:ranges]
         |> Enum.filter(fn {{src_start, src_end}, _} ->
           src_start <= value and value <= src_end
         end) do
      [{{src_start, _src_end}, {dst_start, _dst_end}}] ->
        dst_start + (value - src_start)

      [] ->
        value
    end
  end

  defp inverse_map(value, map) do
    case map[:ranges]
         |> Enum.filter(fn {_, {dst_start, dst_end}} ->
           dst_start <= value and value <= dst_end
         end) do
      [{{src_start, _src_end}, {dst_start, _dst_end}}] ->
        src_start + (value - dst_start)

      [] ->
        value
    end
  end

  defp combineRanges(ranges1, [], acc), do: Enum.reverse(Enum.reverse(ranges1) ++ acc)
  defp combineRanges([], ranges2, acc), do: Enum.reverse(Enum.reverse(ranges2) ++ acc)

  defp combineRanges([{start1, end1} | ranges1], [{start2, end2} | ranges2], acc) do
    min_start = min(start1, start2)

    min_end =
      [start1 - 1, start2 - 1, end1, end2]
      |> Enum.filter(fn x -> x >= min_start end)
      |> Enum.min()

    new_ranges1 =
      cond do
        min_end < start1 -> [{start1, end1} | ranges1]
        min_end < end1 -> [{min_end + 1, end1} | ranges1]
        true -> ranges1
      end

    new_ranges2 =
      cond do
        min_end < start2 -> [{start2, end2} | ranges2]
        min_end < end2 -> [{min_end + 1, end2} | ranges2]
        true -> ranges2
      end

    combineRanges(
      new_ranges1,
      new_ranges2,
      [{min_start, min_end} | acc]
    )
  end

  defp combineMaps(map1, map2) do
    ranges1 =
      map1[:ranges]
      |> Enum.sort_by(fn {{src_start, _src_end}, {_dst_start, _dst_end}} -> src_start end)
      |> then(fn [{{src_start, src_end}, dst} | range] ->
        cond do
          src_start == 0 -> [{{src_start, src_end}, dst} | range]
          true -> [{{0, src_start - 1}, {0, src_start - 1}}, {{src_start, src_end}, dst} | range]
        end
      end)
      |> Enum.sort_by(fn {{_src_start, _src_end}, {dst_start, _dst_end}} -> dst_start end)
      |> Enum.map(fn x -> elem(x, 1) end)

    ranges2 =
      map2[:ranges]
      |> Enum.sort_by(fn {{src_start, _src_end}, {_dst_start, _dst_end}} -> src_start end)
      |> Enum.map(fn x -> elem(x, 0) end)

    %{
      src: map1[:src],
      dst: map2[:dst],
      ranges:
        combineRanges(ranges1, ranges2, [])
        |> Enum.map(fn {start, end_} ->
          {{inverse_map(start, map1), inverse_map(end_, map1)},
           {map_value(start, map2), map_value(end_, map2)}}
        end)
    }
  end

  defp getMapBySource(maps, sourceType) do
    maps
    |> Enum.filter(fn m -> m[:src] == sourceType end)
    |> hd
  end

  defp parseMap(mapStr) do
    [src, dst] =
      mapStr |> String.split("\n") |> hd |> String.split(" map") |> hd |> String.split("-to-")

    %{
      src: src,
      dst: dst,
      ranges:
        mapStr
        |> String.trim("\n")
        |> String.split("\n")
        |> tl
        |> Enum.map(fn x ->
          x |> String.trim(" ") |> String.split(" ") |> Enum.map(&parseInt/1)
        end)
        |> Enum.map(fn [dst, src, length] ->
          {{src, src + length - 1}, {dst, dst + length - 1}}
        end)
    }
  end

  defp parseInput(input) do
    %{
      seeds:
        input
        |> String.split("seeds:")
        |> Enum.drop(1)
        |> hd
        |> String.split("\n")
        |> hd
        |> String.trim(" ")
        |> String.split(" ")
        |> Enum.map(&parseInt/1),
      maps:
        input
        |> String.split("\n\n")
        |> Enum.drop(1)
        |> Enum.map(&parseMap/1)
    }
  end

  defp valueIsInSeedRanges?(value, seedRanges) do
    seedRanges
    |> Enum.any?(fn {start, end_} -> value >= start and value <= end_ end)
  end

  defp problem1(input) do
    reduced_map = Enum.reduce(input[:maps], fn x, acc -> combineMaps(acc, x) end)

    input[:seeds]
    |> Enum.map(fn x -> map_value(x, reduced_map) end)
    |> Enum.min()
  end

  defp problem2(input) do
    reduced_map = Enum.reduce(input[:maps], fn x, acc -> combineMaps(acc, x) end)

    seed_ranges =
      input[:seeds]
      |> Enum.chunk_every(2)
      |> Enum.map(fn [a, b] -> {a, a + b - 1} end)
      |> Enum.sort()

    combineRanges(
      seed_ranges |> then(fn [{a, b} | l] -> [{0, a - 1}, {a, b} | l] end),
      reduced_map[:ranges] |> Enum.map(fn x -> elem(x, 0) end) |> Enum.sort(),
      []
    )
    |> Enum.filter(fn {start, _end_} -> valueIsInSeedRanges?(start, seed_ranges) end)
    |> Enum.map(fn {start, end_} ->
      {map_value(start, reduced_map), map_value(end_, reduced_map)}
    end)
    |> Enum.map(fn x -> x |> elem(0) end)
    |> Enum.min()
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

Day05.main()
