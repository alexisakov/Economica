(* Mathematica Package *)

BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

MonthShift::usage="Function by Mike Honeychurch. 
Available here: http://mathematica.stackexchange.com/questions/71806/why-does-timeseriesshift-is-just-adding-a-fixed-number-of-days.";
TimeSeriesMonthShift::usage="Function by Mike Honeychurch."


Begin["`Private`"] (* Begin Private Context *) 
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

End[] (* End Private Context *)

EndPackage[]

(*
http://www.groupsrv.com/usenet/view.php?c=science&g=1681&id=487929&p=0
On 28 Jun 2006 I posted Mathematica code that does general orthomax
rotation in the sci.stat.math thread "varimax rotation software",
http://groups.google.com/group/sci.stat.math/msg/03c5f332f2ee65aa

Here is a driver for it:

orthomax[A_,gamma_] := Block[{eps = 10.^-6, m = Length [No Spam] A,
j,k,L,c,s, B = A}, j = k = 1; L = 0; While[L++ < m(m-1)/2,
If[++k > m, If[++j == m, j = 1]; k = j+1];
{c,s} = orthang[B[[j]],B[[k]],gamma];
If[Abs [No Spam] s > eps, L = 1; B[[{j,k}]] = {{c,s},{-s,c}}.B[[{j,k}]]]];
If[Negative [No Spam] Tr [No Spam] #,-#,#]& / [No Spam] B ]

Note that A and B are "short":
rows are factors, columns are variables.
This is not the usual orientation,
but it's more convenient for mathematica.

Here is some sample data, taken from Jennrich & Sampson (1966).
It's Thurstone's (1947, p 194) centroid solution for his
20-variable box problem:

A =
{{ 659, 725, 665, 869, 834, 836, 856, 848, 861, 880,
889, 875, 667, 717, 634, 936, 966, 625, 702, 664},
{-736, 180, 537,-209, 182, 519,-452,-426, 416,-341,
-147, 485,-725, 246, 501, 257,-239,-720, 112, 536},
{ 138,-656, 500,-443, 508, 152,-269, 320,-299,-354,
436, -93, 109,-619, 522, 165, -83, 166,-650, 488}}/1000.

B = orthomax[A,1]

{{.051984, .145676, .983588, .134662, .887270, .871535, .091161,
.443444, .566022, .118424, .692216, .732862, .046026, .199695,
.957886, .790063, .383899, .057326, .097329, .975463},
{.990157, .142408, .050110, .576413, .431148, .094008, .796706,
.886806, .102018, .705320, .710642, .095393, .980282, .092492,
.065389, .362483, .726284, .963460, .184625, .047984},
{.109127, .973067, .103141, .802935, .116651, .472176, .605268,
.141113, .820360, .710279, .133607, .680670, .139010, .953590,
.060165, .462359, .567715, .070985, .940366, .111829}}

*)