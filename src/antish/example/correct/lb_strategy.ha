rec main = {
	explore;
}

rec explore = {
	try {
		let marks = (Marker 1 Ahead || Marker 2 Ahead || Marker 3 Ahead || Marker 4 Ahead),
			go m = {
				food;
				Move;

				if !(marks) then {
					Mark m;	
				}
			}
		in {
			go 1;
			go 2;
			go 3;
		}
	} catch {
		Turn Right;
		Move;
	}
	explore;
}


rec food = {
	if (Food Here) then {
		defend;
	}
}

rec defend = {
	Turn Right;
	defend;
}

