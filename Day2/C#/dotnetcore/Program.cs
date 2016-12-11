using System;
using System.Collections.Generic;
using System.IO;
using System.Threading;

class Program
{
    static void Main(string[] args)
    {
        Pad pad;
        if (args.Length > 0)
        pad = new Pad(2);
        else
        pad = new Pad(1);
        var code = new List<string>();
        var moves = new List<char[]>();
        var input = File.ReadAllLines("input.txt");
        foreach (var i in input) {
            moves.Add(i.ToLower().ToCharArray());
        }
        foreach (var move in moves) {
            foreach (var m in move) {
                pad.Move(m);
            }
            code.Add(pad.Current);
        }
        var pause = 500;
        pad.Print("",false);
        Thread.Sleep(pause);
        foreach (var c in code) {
            pad.Print(c,true);
            Thread.Sleep(pause);
            pad.Print(c,false);
            Thread.Sleep(pause);
        }
        Console.WriteLine("The code is " + String.Join(" ",code.ToArray()));
    }
}
