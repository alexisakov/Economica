(* Wolfram Language package *)

Options[TimeSeriesBarChart] = Join[Options[BarChart],
  {DateLabelStep -> {12, "Month"},
   DateTicksFormat -> {"MonthNameShort", "/", "Year"}}]
   
   (*The verison for one series is still under construction*)
  TimeSeriesBarChart[ts_TimeSeries] := Module[{},
  BarChart[ts]
  ]
  
  
  With[{opt = First /@ Options[BarChart]},
  TimeSeriesBarChart[ts_List, OptionsPattern[]] := Module[{
     vals = (#["Values"] & /@ ts)\[Transpose],
     dts = ts[[1]]["Dates"],
     mts, labels, ticks},
    mts = TimeSeries[{dts, vals}\[Transpose]];
    
    (*constructing the labels*)
    labels = 
     DateRange[First@dts, Last@dts, OptionValue[DateLabelStep]];
    ticks = 
     If[MemberQ[labels, #], 
        VTBTextStyle@DateString[#, OptionValue[DateTicksFormat]], 
        ""] & /@ dts;
    BarChart[mts,
     ChartLabels -> {ticks, None},
     Sequence @@ ((# -> OptionValue[#]) & /@ opt)]
    ]
  ];