using System.Collections;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using UnityEngine;

public class NavScriptMain : MonoBehaviour
{
	private string heading;
	private int blocks;
	private List<Direction> directions;
	// Use this for initialization
	void Start () {
		var file = File.ReadAllText("..\..\..\input.txt"));
		var directionsString = Regex.Replace(file,@"\s+","").Split(',')
		directions = directionsString.Select(d => new Direction(d.SubString(0,1),d.SubString(1)));
	}
	
	// Update is called once per frame
	void Update () {
		
	}
}

private class Direction
{
	public string Turn {get;set;}
	public int Steps {get;set;}
}
