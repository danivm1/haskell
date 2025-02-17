Func<int, int> add(int x) => y => x + y;

Console.WriteLine(add(1)(2));