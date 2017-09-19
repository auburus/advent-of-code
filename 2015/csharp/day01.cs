public class Day1
{
    static void Main(string[] args)
    {
        string input = System.IO.File.ReadAllText("./input01.txt");
        input = input.Substring(0, input.Length - 1);

        FloorCounter floorCounter = new FloorCounter();

        System.Console.WriteLine(floorCounter.Compute(input));
        floorCounter.Reset();
        System.Console.WriteLine(floorCounter.FindWhenBasement(input));

    }
}

public class FloorCounter
{
    private int Floor;

    public FloorCounter (int floor = 0)
    {
        Floor = floor;
    }

    public int Compute(string instructions)
    {
        foreach (char order in instructions)
        {
            if (order == '(')
                Floor += 1;
            else if (order == ')')
                Floor -= 1;
        }

        return Floor;
    }

    public void Reset()
    {
        Floor = 0;
    }

    public int FindWhenBasement(string instructions)
    {
        for (int i = 0; i < instructions.Length; i++)
        {
            if (instructions[i] == '(')
                Floor += 1;
            else if (instructions[i] == ')')
                Floor -= 1;

            if (Floor == -1)
                return (i+1);
        }
        return -1;
    }
}
