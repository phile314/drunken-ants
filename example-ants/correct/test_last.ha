rec turnBack x = {
	turnBack;
}

goHome = {
	move;
	turn;
	let y = 6,
		x = 5
	in {
		move;
		Move;
		MarkCall 3;
		try {
			damm;
			Move;
		} catch {
			turnBack 3;
		}
		with probability 0.25 do {
			Mark 3;
		} otherwise {
			Drop;
		}
	}
}
