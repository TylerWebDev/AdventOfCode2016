<cfscript>
	// convert input file to array
	data = FileRead('input.txt');
	ar = [];
	for (var i=1; i<=data.len(); i++) {
		if ( data[i] == Chr(10) ) {
			ar.append('line');
		}
		else if ( data[i] != Chr(13) ) {
			ar.append(data[i]);
		}
	}

	// process array
	buttons = [];
	pos = {x:1, y:1};
	for (d in ar) {
		// d is an element of ar
		switch (d) {
			case 'U' : {
				pos.y = pos.y == 0 ? pos.y : pos.y-1;
				break;
			}
			case 'D' : {
				pos.y = pos.y == 2 ? pos.y : pos.y+1;
				break;
			}
			case 'L' : {
				pos.x = pos.x == 0 ? pos.x : pos.x-1;
				break;
			}
			case 'R' : {
				pos.x = pos.x == 2 ? pos.x : pos.x+1;
				break;
			}
			case 'line' : {
				buttons.append(Duplicate(pos));	
			}
		} 
	}
	buttons.append(Duplicate(pos));	

	// convert array of coordinates to string of buttons
	seq = '';
	for (button in buttons) {
		seq &= button.x+1 + button.y*3;
	}
</cfscript><cfoutput>#seq#</cfoutput>