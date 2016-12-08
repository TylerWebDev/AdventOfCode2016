<cfscript>
	// convert input file to array
	data = FileRead('input.txt');
	arrayOfLines = data.listToArray(Chr(10) & Chr(13));
	triangles = arrayOfLines.reduce(function(result, line) {
		result = result ? : [];
		result.append(line.listToArray(' '));
		return result;
	});

	// count valid triangles (part one)
	count = 0;
	for (t in triangles) {
	  // triangle is an element of triangles
	 	if ( t[1]+t[2] > t[3] && t[2]+t[3] > t[1] && t[1]+t[3] > t[2] )  {
	 		count++;
	 	}
	}

	// count valid triangles (part two)
	count2 = 0;
	for ( row=1; row<=triangles.len();) {
		tri = [ [], [], [] ];
		for ( i=1; i<=3; i++ ) {
			tri[1].append(triangles[row][1]);
			tri[2].append(triangles[row][2]);
			tri[3].append(triangles[row][3]);
			row++;
		}
		for ( i=1; i<=3; i++ ) {
			t = tri[i];
			if ( t[1]+t[2] > t[3] && t[2]+t[3] > t[1] && t[1]+t[3] > t[2] )  {
		 		count2++;
		 	}
		}
	}
</cfscript><cfoutput>#count#
#count2#</cfoutput>
<!--- <cfdump var="#triangles#" format="text" /> --->