Package["Economica`"]

TimeSeriesMin[ts1_TemporalData, ts2_TemporalData] := 
 ts1 - ((ts1 - ts2)) UnitStep[(ts1 - ts2)];
TimeSeriesMax[ts1_TemporalData, ts2_TemporalData] := 
 ts1 - ((ts1 - ts2)) UnitStep[-(ts1 - ts2)];
 
TimeSeriesQuantile[l_List, q_Real] :=  TimeSeriesThread[Quantile[# /. (Null | "" -> Sequence[]), q] &, l];

PackageExport["TimeSeriesMin"]
PackageExport["TimeSeriesMax"]
PackageExport["TimeSeriesQuantile"]