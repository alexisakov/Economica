Package["Economica`"]

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
     labels = DateRange[First@dts, Last@dts, OptionValue[DateLabelStep]];
     ticks = If[MemberQ[DateString/@labels, DateString@#], VTBTextStyle@DateString[#, OptionValue[DateTicksFormat]], ""] & /@ dts;
     BarChart[mts,
     ChartLabels -> {ticks, None}
     (*Sequence @@ ((# -> OptionValue[#]) & /@ opt)*)
     ]
    ]
  ];
  
  With[{opt = First /@ Options[BarChart]},
	TimeSeriesBarChart[ts_List, tsLine_List, OptionsPattern[]] := Module[{
		vals = (#["Values"] & /@ ts)\[Transpose],
		dts = ts[[1]]["Dates"],
		mts, labels, ticks},
		mts = TimeSeries[{dts, vals}\[Transpose]];
       (*constructing the labels*)
		labels = DateRange[First@dts, Last@dts, OptionValue[DateLabelStep]];
		ticks = If[MemberQ[DateString/@labels, DateString@#], VTBTextStyle@DateString[#, OptionValue[DateTicksFormat]],""] & /@ dts;
		
		(*make the lines*)
		
		epiColor = ColorDataVTBCapital2018 /@ Range[Length@ts + 1, Length@ts + Length@tsLine];
		epiLen = Length@((First@tsLine)["Dates"]);
		epiLines = (Line[{Range@epiLen,  #["Values"]}\[Transpose]])&/@tsLine;
		epilog=Riffle[epiColor,epiLines];
	    epilog=Prepend[epilog,Thickness[0.004]];
		epilog=Join[{Thickness[0.006],White,epiLines},epilog];
		
		BarChart[mts,
			ChartLabels -> {ticks, None}, 
			Epilog->epilog,
      Prolog->OptionValue@Prolog
      ]
    ]
  ];

  PackageExport["TimeSeriesBarChart"]