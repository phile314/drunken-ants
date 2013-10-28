
main = {
    findFood;
}

rec findFood = {
    if Food Ahead then {
        food;
    } else {
		if (Marker 1 Ahead) then {
			Turn Right;
		} else {
			Move;
		}
		findFood;
	}
}

food = {
	PickUp; 
	uturn;
	goHome;
}

uturn = {
	Turn Right;
	Turn Right;
	Turn Right;
}

goHome = {
	if (Home Here) then {
		Drop;
	} else {
		if (Marker 1 Here) then {
			Move;
		} else {
			Turn Left;
		}
	}
}
