(* ::Package:: *)

(* :Title: Season *)

(* :Author: Alex Isakov *)

(* :Summary:
  *)

(* :Mathematica Version: 10.0.1 
  *)

(*DONE: Don't do seasonal adjustment in this package. *)
  
  

ClearAll[TimeSeriesTransform]
Options[TimeSeriesTransform] = {Frequency -> 12};

TimeSeriesYoY[ts_]:=100*(ts/TimeSeriesMonthShift[ts,12]-1);
TimeSeriesMoM[ts_]:=100*(ts/TimeSeriesMonthShift[ts,1]-1);
TimeSeriesQoQ[ts_]:=100*(ts/TimeSeriesMonthShift[ts,3]-1);



TimeSeriesIntegrate[ts_] := 
 TimeSeries[{ts["Dates"], 
    Rest@FoldList[#1*(1 + #2/100) &, 1, ts["Values"]]}\[Transpose]];



 TimeSeriesYoYIntegrate[growthRatesTimeSeries_, initialConditionsTimeseries_] := Module[{
   frq = Length@initialConditionsTimeseries["Values"],
   mults = growthRatesTimeSeries["Values"],
   level = List[]
   },
  level = Flatten@FoldList[
     #1[[;; Length@#2]]*(1 + #2/100) &,
     initialConditionsTimeseries["Values"],
     Partition[growthRatesTimeSeries["Values"], frq, frq, 1, {}]
     ];
  TimeSeries[{Join[initialConditionsTimeseries["Dates"], growthRatesTimeSeries["Dates"]], level}\[Transpose]]
  ];

(*TODO: This should be rewritten to take advantage not of frequency but to difference within the year.
   It also should give error if the data does not start from the first month or quarter of the year.*)
TimeSeriesYTDToLevel[ts_, freq_: 4] := Module[{
   dt = ts["Dates"],
   yrs ,
   tws
   },
  yrs = Union[(DateList /@ dt)[[All, 1]]];
  tws = Flatten@(Prepend[
        Differences[
         TimeSeriesWindow[ts, {{#, 1}, {#, 12}}]["Values"]], 
        TimeSeriesWindow[ts, {{#, 1}, {#, 12}}]["FirstValue"]] & /@ 
      yrs);
  TimeSeries[{dt, tws}\[Transpose]]
  ];



TimeSeriesTransform::unknwntrnsfm="This transform is yet unknown. Consider adding it."
   
TimeSeriesTransform[ts_List, FROM_, TO_, OptionsPattern[]] := Module[
  {dates = ts[[All, 1]],
   data = ts[[All, 2]]},
  Which[
   FROM == TO, Null,
   
   FROM == "Level" && TO == "QoQ",
   	data = Ratios[data] - 1; dates = Rest@dates;,
   

   
   FROM == "YTD" && TO == "Level",
   data = Flatten[
   	(Partition[data, OptionValue[Frequency], OptionValue[Frequency], 1, {}] 
   		- (Partition[data, OptionValue[Frequency], OptionValue[Frequency], 1, {}] /. {x_Real, z__} :> 
   			{0, x, Sequence @@ Most@List@z}))];,
   True, 
   Message[TimeSeriesTransform::unknwntrnsfm];Abort[];
   ];
  (*Return the results.*)
  
(*  If[OptionValue[SeasonallyAdjusted], 
   data = First[
      adjustX13[{dates, 
         data /. x_Real :> Round[x, 0.0001]}\[Transpose], 
       Output -> {"d11"}]][[All, 2]], Null];*)
  {dates, data}\[Transpose]
  ]
  
TimeSeriesTransform::usage="TimeSeriesFillMissing[X,dateList] helps to fill the dates that are missing in X but are present in dateList with Nulls."
  
ClearAll@TimeSeriesTransform;
TimeSeriesTransform[X_List, dateRange_List] := Module[{
   data = 
    Select[X, 
     AbsoluteTime@First@# >= AbsoluteTime@First@dateRange && 
       AbsoluteTime@First@# <=  AbsoluteTime@Last@dateRange &],
   shorterDate = dateRange[[All, 1 ;; 3]]
   },
  data[[All, 1]] = data[[All, 1, 1 ;; 3]];
  
  (*Replace[Join[List/@shorterDate,#]~GatherBy~First,{{x_List}/;
  Length@x\[Equal]3\[RuleDelayed]{x,Null},{__,x_}\[RuleDelayed]x},
  1]& @data*)
  Replace[
   GatherBy[Join[List /@ shorterDate, #] & @data, First],
   {
    {x_List} :> {First@x, Null}, {x_List, y_List} :> y
    },
   1]];

Protect[Frequency];

TimeSeriesBarChartCombine[ts1_, ts2_] := TimeSeriesThread[
  If[
    Sign[#[[1]]] == Sign[#[[2]]],
    #[[1]] + #[[2]],
    #[[2]]] &,
  {ts1, ts2}]
  
