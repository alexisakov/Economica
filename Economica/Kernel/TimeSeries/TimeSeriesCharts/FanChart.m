(* Wolfram Language package *)

(* Mathematica Package *)
BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

FanChart::usage="FanChart[{fact, forecast, lower,upper}] is used to BoE fan charts."

Begin["`Private`"] (* Begin Private Context *) 

ClearAll[FanChart];

Options[FanChart] = Join[DeleteCases[Options[DateListPlot], GridLines -> _], 
	{GridLines -> None, ShadeColor -> LightGray, ShadeOpacity -> 1}];

(*FanChart[{fact_, forecast_, lower_, upper_}, OptionsPattern[]] := DateListPlot[
		{fact, forecast, lower, upper}, 
		Filling->{3->{2},4->{2}}];
*)


With[{opt = First /@ Options[DateListPlot]},
  FanChart[{fact_, forecast_, lower_, upper_}, OptionsPattern[]] := DateListPlot[
    	{fact, forecast, lower, upper}, 
		Filling->{3->{2},4->{2}},
		 Sequence @@ ((# -> OptionValue[#]) & /@ opt)]
    ];
    	
    
    	
     



End[] (* End Private Context *)
EndPackage[]