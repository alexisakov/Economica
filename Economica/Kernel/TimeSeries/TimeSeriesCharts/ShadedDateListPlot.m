(* Mathematica Package *)
BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

ShadedDateListPlot::usage="ShadedDateListPlot[ddata, ShadeDates] is used to make NBER recession plots."

Begin["`Private`"] (* Begin Private Context *) 

ClearAll[ShadedDateListPlot];
Options[ShadedDateListPlot] = Join[DeleteCases[Options[DateListPlot], GridLines -> _], 
	{GridLines -> None, ShadeColor -> LightGray, ShadeOpacity -> 1}];

With[{opt = First /@ Options[DateListPlot]},
  ShadedDateListPlot[ddata_, ShadeDates_, OptionsPattern[]] :=
  Module[{yRange, yPadding},
    With[{plot = 
       DateListPlot[ddata, 
        Sequence @@ ((# -> OptionValue[#]) & /@ opt)]},
  yRange = 
      Last[PlotRange /. AbsoluteOptions[plot, PlotRange]];
     yPadding = 
      If[OptionValue[Frame] === True, + .001 Subtract @@ yRange, 
       0];
	Show[plot,
		Prolog -> {{OptionValue[ShadeColor], 
        Opacity[OptionValue[ShadeOpacity]], 
        (Rectangle[{AbsoluteTime[#[[1]]], yRange[[1]] - yPadding}, 
			{AbsoluteTime[#[[2]]], yRange[[2]] + yPadding}]&)/@ShadeDates},{OptionValue[Prolog]}}]]]];

End[] (* End Private Context *)
EndPackage[]