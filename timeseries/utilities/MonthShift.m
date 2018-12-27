Package["Economica`"]

(* Exported symbols added here with SymbolName::usage *)  

MonthShift::usage="Function by Mike Honeychurch. 
Available here: http://mathematica.stackexchange.com/questions/71806/why-does-timeseriesshift-is-just-adding-a-fixed-number-of-days.";
TimeSeriesMonthShift::usage="Function by Mike Honeychurch."


(*TODO: Write a function that exports in IRIS format.*)

MonthShift[timeSeries_, months_] := 
 Module[{ts = timeSeries}, 
  ts[[2, 2, 1, 1]] = (DatePlus[#1, {months, "Month"}] &) /@ ts[[2, 2, 1, 1]]; ts];

TimeSeriesMonthShift[timeSeries_, months_] := Module[
 		{ 	dates = timeSeries["Dates"], vals = timeSeries["Values"]}, 
 	TimeSeries[
 		Transpose[{
 			DatePlus[#1, {months, "Month"}]&/@dates,vals}]
 		]
 	];


PackageExport["MonthShift"]
PackageExport["TimeSeriesMonthShift"]
