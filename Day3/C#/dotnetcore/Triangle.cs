using System.Collections.Generic;
internal class Triangle {
    internal int Side1 {get;private set;}
    internal int Side2 {get;private set;}
    internal int Side3 {get;private set;}
    internal bool Valid => Side1+Side2 > Side3;
    internal Triangle(int side1, int side2, int side3)
    {
        var sides = new List<int>{side1, side2, side3};
        sides.Sort();
        Side1 = sides[0];
        Side2 = sides[1];
        Side3 = sides[2];
    }
}