Package["Economica`"]

FanChart::usage="FanChart[{fact, forecast, lower,upper}] is used to BoE fan charts."

ClearAll[FanChart];

Options[FanChart] = Join[DeleteCases[Options[DateListPlot], GridLines -> _], 
	{GridLines -> None, ShadeColor -> LightGray, ShadeOpacity -> 1}];

(*FanChart[{fact_, forecast_, lower_, upper_}, OptionsPattern[]] := DateListPlot[
		{fact, forecast, lower, upper}, 
		Filling->{3->{2},4->{2}}];*)

With[{opt = First /@ Options[DateListPlot]},
  FanChart[{fact_, forecast_, lower_, upper_}, OptionsPattern[]] := DateListPlot[
    	{fact, forecast, lower, upper}, 
		Filling->{3->{2},4->{2}},
		 Sequence @@ ((# -> OptionValue[#]) & /@ opt)]
    ];
    	
PackageExport["FanChart"]