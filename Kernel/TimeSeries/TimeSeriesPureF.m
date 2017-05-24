<<<<<<< HEAD
TimeSeriesPureF[f_, ts_] := 
=======
TimeSeriesPureF[f_, ts_] := 
>>>>>>> origin/master
 TimeSeries[Transpose@{ts["Dates"], f /@ ts["Values"]}]