using System;
using System.Linq;
internal partial class Pad {
    internal void Move(char m) {
        switch (m) {
            case 'u':
            Row = Go("Row",-1);
            break;
            case 'd':
            Row = Go("Row",1);
            break;
            case 'l':
            Column = Go("Column",-1);
            break;
            case 'r':
            Column = Go("Column",1);
            break;
        }
    }
    
    private int Go(string dir, int i) {
        if (dir == "Row") {
            var newRow = Row + i;
            if (_pad.Count > newRow && newRow >= 0 && _pad[newRow][Column] != null)
                return newRow;
            return Row;
        }
        else if (dir == "Column") {
            var newColumn = Column + i;
            if (_pad[Row].Count > newColumn && newColumn >= 0 && _pad[Row][newColumn] != null)
                return newColumn;
            return Column;
        }
        else
        throw new Exception("Not a valid direction");
    }

    internal void Print(string code, bool press) {
        Console.Clear();
        for (int x = 0; x < _pad.Count; x++)
        {
            var line = "";
            int buffer = 0;
            var rowCount = _pad[x].Where(r => r != null).Count();
            if (rowCount < MaxRow) {
                buffer = (MaxRow - rowCount)/2;
                line += WriteBuffer(buffer);
            }
            for (int y = 0; y < _pad[x].Count; y++)
            {
                if (_pad[x][y] == null)
                    continue;
                if (press && _pad[x][y] == code) {
                    line += " [( )] ";
                }
                else {
                    line += " [ " + _pad[x][y] + " ] ";
                }
            }
            if (buffer > 0) {
                line += WriteBuffer(buffer);
            }
            Console.WriteLine(line);
            Console.WriteLine();
        }
    }

    private string WriteBuffer(int buffer) {
        var line = "";
        for (int i = 0; i < buffer; i++)
        {
            line += "       ";
        }
        return line;
    }
}