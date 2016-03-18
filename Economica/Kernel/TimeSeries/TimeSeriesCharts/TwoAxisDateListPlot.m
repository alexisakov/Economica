(*Wolfram Language package*) 
Options[TwoAxisDateListPlot] =  Options[DateListPlot];

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
     {ColorData["Rainbow"] /@ 
        Range[Length@list1 + 1, Length@list1 + Length@list2],
       Line[{#["Dates"], #["Values"]}\[Transpose]] & /@ 
        epilogData}\[Transpose]];
    epilogData=Prepend[epilogData,Thickness[0.004]];
   
   da1 = linspace[#1, #2 + 0., 5] & @@ Round@firstaxisrange;
   da2 = linspace[#1, #2 + 0., 5] & @@ Round@secondaxisrange;
   
   (*the chart itself*)
   DateListPlot[list1,
    Epilog -> epilogData,
    FrameTicks -> {Quiet[
       {{da1, IntegerPart[#]& /@ da1}\[Transpose],
        {da1, IntegerPart[#]& /@ da2}\[Transpose]}],
      {True, None}}, Frame -> {True, True, False, True}, Axes -> False,
      Sequence @@ ((# -> OptionValue[#]) & /@ opt)]
   ]]