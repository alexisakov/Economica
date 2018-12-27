(* ::Package:: *)

(* :Title: Season *)

(* :Author: Alex Isakov *)

(* :Summary:
  *)

(* :Mathematica Version: 10.0.1 
  *)

(**)
  
  

ClearAll[TimeSeriesMakeCommonRange]

  
TimeSeriesMakeCommonRange::usage="TimeSeriesFillMissing[l_List] transforms each TimeSeries in the list l so that all of them have the same range filling in missings with zeroes"
  
TimeSeriesMakeCommonRange[
  lst : {(TemporalData[TimeSeries, ___]) ..}] := Module[
  {dt = Union@Flatten[#["Dates"] & /@ lst], zrs, ldt},
  ldt = Length@dt;
  TimeSeries[
     Sort[Join[{#["Dates"], #["Values"]}\[Transpose], {Complement[
          dt, #["Dates"]], 
         ConstantArray[0, 
          Length[Complement[dt, #["Dates"]]]]}\[Transpose]]]] & /@ lst
  ]
