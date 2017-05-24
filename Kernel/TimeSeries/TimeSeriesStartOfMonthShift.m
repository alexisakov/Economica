TimeSeriesStartOfMonthShift[ts_] := Module[{
   dates = ts["Dates"], values = ts["Values"],
   datelist},
  datelist = (DateList /@ dates)[[All, 1 ;; 3]];
  datelist[[All, 3]] = 1;
  TimeSeries[Transpose[{datelist, values}]]
  ]