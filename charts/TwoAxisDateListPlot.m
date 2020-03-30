Package["Economica`"]

Options[TwoAxisDateListPlot] =  Join[Options[DateListPlot],{ReversedAxis->False}];

Off[NumberForm::sigz];

With[
 {opt = First /@ Options[DateListPlot]},
 TwoAxisDateListPlot[xx_, yy_, OptionsPattern[]] := Module[
   {list1, list2,
    epilogData, firstaxisrange, secondaxisrange, da1, da2},
   list1 = If[Head[xx] === TemporalData, List@xx, xx];
   list2 = If[Head[yy] === TemporalData, List@yy, yy];
   
   firstaxisrange = {Min@#, Max@#} &@Flatten[#["Values"] & /@ list1];
   secondaxisrange = {Min@#, Max@#} &@Flatten[#["Values"] & /@ list2];
   
   epilogData = 
    Rescale[#, secondaxisrange, firstaxisrange ] & /@ list2;
   
   epilogData = Flatten[
     {ColorDataVTBCapital2018 /@ 
        Range[Length@list1 + 1, Length@list1 + Length@list2],
       Line[{#["Dates"], #["Values"]}\[Transpose]] & /@ 
        epilogData}\[Transpose]];
    epilogData=Prepend[epilogData,Thickness[0.004]];
   
   da1 = linspace[#1, #2 + 0., 5] & @@ Round@firstaxisrange;
   da2 = linspace[#1, #2 + 0., 5] & @@ Round@secondaxisrange;
   
   (*the chart itself*)
   DateListPlot[list1,
   Epilog -> epilogData,
      Prolog -> OptionValue[Prolog],
      Frame -> {True, True, False, True},
      Axes -> False,
      FrameTicks -> {
      		{
      		{da1, Round[#,0.1] & /@ da1}\[Transpose], 
      		{da1, If[OptionValue[ReversedAxis], -1, 1]*(Round[#,0.1] & /@ da2)}\[Transpose]},
      		{Automatic, None}
      	}


   ]
   ]
   ]

PackageExport["TwoAxisDateListPlot"]