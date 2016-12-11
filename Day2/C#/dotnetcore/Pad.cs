using System.Collections.Generic;
using System.Linq;
internal partial class Pad {

    private readonly List<List<string>> _pad;
    private int MaxRow {get;set;}
    internal int Column {get; private set;}
    internal int Row {get; private set;}
    internal string Current => _pad[Row][Column];

    internal Pad(int part) {
        if (part == 1) {
            _pad = new List<List<string>> { new List<string>{ "1", "2", "3" }, new List<string>{ "4", "5", "6" }, new List<string>{ "7", "8", "9" } };
            Column = 1;
            Row = 1;
        }
            
        else {
            _pad = new List<List<string>> { new List<string>{ null,null,"1",null,null},new List<string>{null,"2","3","4",null}, new List<string>{"5","6","7","8","9"}, new List<string>{null,"A","B","C",null},new List<string>{null,null,"D",null,null}};
            Column = 0;
            Row = 2;
        }

        MaxRow = _pad.Max(r => r.Count);
    }
}