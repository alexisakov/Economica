
(*	TODO: Write an explanation of the this symbol.
  *)
  
  

ClearAll[TimeSeriesJoin];

TimeSeriesJoin[ts1_, ts2_] := Module[
  {dv = {ts2["Dates"], ts2["Values"]}\[Transpose]},
  Fold[TimeSeriesInsert[#1, #2] &, ts1, dv]
  ]
