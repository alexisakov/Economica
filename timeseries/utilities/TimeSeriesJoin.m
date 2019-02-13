
Package["Economica`"]

ClearAll[TimeSeriesJoin];

TimeSeriesJoin[ts1_, ts2_] := 
 TimeSeries[
  Transpose[{
  	Join[ts1["Dates"], ts2["Dates"]], 
    Join[ts1["Values"], ts2["Values"]]
}]]


TimeSeriesInsertPreserve[ts1_, ts2_] := Module[
  {d1 = ts2["Dates"],
   d2 = Complement[ts1["Dates"], ts2["Dates"]]},
  TimeSeries[
   Sort[
    Join[
     {#, ts1@#} & /@ d2,
     ts2["DatePath"]
     ]
    ]
   ]
  ]




PackageExport["TimeSeriesJoin"]
PackageExport["TimeSeriesInsertPreserve"]



(*	TimeSeriesJoin[ts1_, ts2_] := Module[
  {dv = {ts2["Dates"], ts2["Values"]}\[Transpose]},
  Fold[TimeSeriesInsert[#1, #2] &, ts1, dv]
  ]
  *)


