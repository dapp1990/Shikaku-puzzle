:- lib(ic).
:- compile("print").
:- compile("puzzles").

/*************
*	Solver
**************/

shikaku(IdProblem) :-
	write(' '), 
	problem(IdProblem,Width,Height,Hints),
	writeln(IdProblem), 
	statistics(runtime, _),
	solver(Width,Height,Hints,Solution),
	statistics(runtime, [_, Time]),
	write(Time), 
	writeln('ms'),
	show(Width, Height, Hints, Solution),
	fail.

solver(Width,Height,Hints,Solution) :-
	(foreach((I,J,N),Hints), foreach(X1,X1s), foreach(Y1,Y1s), foreach(X2,X2s), foreach(Y2,Y2s), foreach(rect(c(I,J),c(X1,Y1),s(W,H)), Solution),
		param(Width,Height)
		do
			W :: 1..N, 
			H :: 1..N,

			W*H #= N,
			X1+W-1 #= X2,
			Y1+H-1 #= Y2,

			% -- rectangles inside frame	
			0 #< X1,
			0 #< Y1,
			Width #>= X2,
			Height #>= Y2,

			% -- hint inside rectangle
			inside(I,J,X1,Y1,X2,Y2)
	),

	% -- no_overlaped rectangles
	no_overlap(X1s,Y1s,X2s,Y2s),

	term_variables(Solution, Ss),

    search(Ss,0,anti_first_fail,indomain_max,complete,[backtrack(A)]),

	write('backtracks '), 
	writeln(A).


inside(I,J,X1,Y1,X2,Y2) :-
	I #>= X1,
	J #>= Y1,
	I #=< X2,
	J #=< Y2.

no_overlap(Xs, Ys, Ws, Hs) :-
        ( fromto(Xs, [X|XXs], XXs, []),
          fromto(Ys, [Y|YYs], YYs, []),
          fromto(Ws, [W|WWs], WWs, []),
          fromto(Hs, [H|HHs], HHs, [])
        	do
            (foreach(X1,XXs), foreach(Y1,YYs), foreach(W1,WWs), foreach(H1,HHs),
            	param(X,Y,W,H) 
            	do
                    no_overlap(X, Y, W, H, X1, Y1, W1, H1)
            )
        ).

no_overlap(X1, Y1, X2, Y2, Xx1, Yy1, Xx2, Yy2) :-
        X2 #< Xx1 
        or 
        Xx2 #< X1 
        or 
        Y2 #< Yy1 
        or 
        Yy2 #< Y1.