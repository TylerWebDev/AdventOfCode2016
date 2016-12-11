using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
class Program
{
    static void Main(string[] args)
    {
        var triangles = new List<Triangle>();
        var sides = new List<int[]>();
        var file = File.ReadAllLines("input.txt");
        foreach (var line in file) {
            var s =  Regex.Replace(line.Trim(),@"\s+",",")
                        .Split(',')
                        .Select(a => Convert.ToInt32(a))
                        .ToArray();
            sides.Add(s);
            if (s.Length > 3 || s.Length < 3)
                throw new Exception("Not a triangle");
            triangles.Add(new Triangle(s[0], s[1], s[2]));
        }
        Console.WriteLine("The number of valid triangles is " + triangles.Where(t => t.Valid).Count());
        triangles = new List<Triangle>();
        var vSides = sides.Select((x, i) => new { Index = i, Value = x })
            .GroupBy(x => x.Index / 3)
            .Select(x => x.Select(v => v.Value).ToArray())
            .ToList();
        foreach (var vSide in vSides) {
            triangles.Add(new Triangle(vSide[0][0], vSide[1][0], vSide[2][0]));
            triangles.Add(new Triangle(vSide[0][1], vSide[1][1], vSide[2][1]));
            triangles.Add(new Triangle(vSide[0][2], vSide[1][2], vSide[2][2]));
        }
        Console.WriteLine("No...wait..." + triangles.Where(t => t.Valid).Count());
    }
}
