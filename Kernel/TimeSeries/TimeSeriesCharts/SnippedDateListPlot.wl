(* ::Package:: *)

(*Wolfram Language package*) 
Options[SnippedDateListPlot] =  Options[DateListPlot];

Off[NumberForm::sigz];


  With[{opt = First /@ Options[DateListPlot]},

SnippedDateListPlot[l_,range1_,range2_, OptionsPattern[]]:= Module[
	{ytick1, ytick2,  target,jn},
	
	ytick1 = FindDivisions[range1, 5] /. y_?NumericQ :> {y, y} /. {y_?NumericQ, _} /; y >= range1[[2]] :> Sequence[];
	ytick2 = FindDivisions[range2, 5] /. y_?NumericQ :> {y - range2[[1]] + range1[[2]], y} /. {y_?NumericQ, _} /; y <= range1[[2]] :> Sequence[];
	
	target = Subtract @@ Reverse@range1/(Subtract @@ Reverse@range1 + Subtract @@ Reverse@range2);
	
	jn=Join[{White, Rectangle[Scaled[{-0.1, 0.98 target}], Scaled[{1.1, 1.02 target}]],
			Black, 
			Text[Rotate["\\", \[Pi]/2], Scaled[{0, 0.98 target}], {-1.5, 0}],
			Text[Rotate["\\", \[Pi]/2], Scaled[{0, 1.02 target}], {-1.5, 0}]}];
	
	DateListPlot[
	(TimeSeriesPureF[If[# > range2[[1]],  # - range2[[1]] + range1[[2]],#]&,#]&/@l),
		
		PlotRange ->{All,{range1[[1]], range1[[2]]+Subtract @@ Reverse@range2}}, 
		Axes->True,
		Ticks -> {Automatic, Join[ytick1, ytick2]},
		Epilog -> jn,
			Sequence @@ ((# -> OptionValue[#]) & /@ opt)
  ]
  ]]
   

