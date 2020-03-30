(* ::Package:: *)

Package["Economica`"]


ClearAll[TimeSeriesRepeatExtend];
Options[TimeSeriesRepeatExtend]={Frequency->"Month"};


TimeSeriesRepeatExtend[ts_,n_,OptionsPattern[]]:=Switch[
	OptionValue[Frequency],
		"Month",
		( 
			num=12;
			lastDate=ts["LastDate"];
			lastDateMinus11mo = lastDate - Quantity[11, "Months"];
			fullYears = Ceiling[n/num];

			tsExtended = Nest[TimeSeriesInsert[#, 
							TimeSeriesWindow[TimeSeriesMonthShift[#, 12], 
								{#["LastDate"] + Quantity[1, "Months"], #["LastDate"] + Quantity[12, "Months"]}]] &, ts, fullYears];

			TimeSeriesWindow[tsExtended,{ts["FirstDate"],lastDate + Quantity[n, "Months"]}]


				),
		"Quarter",
		( 
			num=4;
			LastDate=(DateList@(TimeSeriesMonthShift[ts,3])["LastDate"])[[#]]&/@({1,2});
			tsExtend=TimeSeriesWindow[ts,{LastDate-{1,0},LastDate}];
			tsnew=ts;
			i=0;
			While[i<=(n-Mod[n,num])/num,
			tsnew=TimeSeriesInsert[tsnew,TimeSeriesMonthShift[tsExtend,num*(3i+3)]];
			i=i+1;
			];
			If[Mod[LastDate[[2]]+3*n-3,3*num]!=0,
			tsnew=TimeSeriesWindow[tsnew,{(DateList@tsnew["FirstDate"])[[#]]&/@({1,2}),{LastDate[[1]]+((LastDate[[2]]+3*n-3)-Mod[LastDate[[2]]+3*n-3,3*num])/(3*num),Mod[LastDate[[2]]+3*n-3,3*num]}}],
			tsnew=TimeSeriesWindow[tsnew,{(DateList@tsnew["FirstDate"])[[#]]&/@({1,2}),{LastDate[[1]]+((LastDate[[2]]+3*n-3)-Mod[LastDate[[2]]+3*n-3,3*num])/(3*num)-1,Mod[LastDate[[2]]+3*n-3,3*num]+12}}]]
		)
]


PackageExport["TimeSeriesRepeatExtend"]
