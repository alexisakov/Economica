(* ::Package:: *)

Package["Economica`"]


ClearAll[TimeSeriesRepeatExtend];

TimeSeriesRepeatExtend[ts_, n_] := Switch[
  	(MinimumTimeIncrement@ts)[[2]],
  		"Month",
  		(
   				num = 12;
   				lastDate = ts["LastDate"];
   				fullYears = Ceiling[n/num];
   				tsExtended = Nest[TimeSeriesInsert
       									[#, TimeSeriesWindow[TimeSeriesMonthShift[#, 12],
        														{#["LastDate"] + 
          Quantity[1, "Months"], #["LastDate"] + 
          Quantity[12, "Months"]}]] &, ts, fullYears];
   				TimeSeriesWindow[
    tsExtended, {ts["FirstDate"], lastDate + Quantity[n, "Months"]}]
   
   		),
  		"Quarter",
  		(
   				num = 4;
   				lastDate = ts["LastDate"];
   				fullYears = Ceiling[n/num];
   				tsExtended = Nest[TimeSeriesInsert
       									[#, TimeSeriesWindow[TimeSeriesShift[#, {12, "Month"}],
        														{#["LastDate"] + 
          Quantity[3, "Months"], #["LastDate"] + 
          Quantity[12, "Months"]}]] &, ts, fullYears];
   				
   				TimeSeriesWindow[
    tsExtended, {ts["FirstDate"], lastDate + Quantity[3 n, "Months"]}]
   		)
  ]


PackageExport["TimeSeriesRepeatExtend"]
