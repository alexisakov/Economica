(* Mathematica Package *)

BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

GrangerCausalityTest::usage="GrangerCausalityTest[dat, lag]  returns a p-value for the null hypothesis that the first element in dat does not cause the second element in dat."


Begin["`Private`"] (* Begin Private Context *) 

getLags[data_, i_] := Drop[data, -i];

GrangerCausalityTest[dat_, lag_: 3] := Module[
  {xx, x, y, laggeddata, le, res1, res2, grangerstat},
  le = Select[dat, (And @@ (NumericQ /@ #)) &];
  xx = Flatten[Most /@ le]; y = Last /@ le;
  laggeddata = Transpose[
    Map[Take[#, -Length[y] + lag] &,
     Join[Table[getLags[y, i], {i, lag}], 
      Table[getLags[xx, i], {i, lag}], {y}]]
    ];
  res1 = Total[
    LinearModelFit[laggeddata, 
      Table[Subscript[x, i], {i, Length[First[laggeddata]] - 1}], 
      Table[Subscript[x, i], {i, Length[First[laggeddata]] - 1}]][
     "FitResiduals"]^2];
  res2 = Total[
    LinearModelFit[laggeddata, 
      Table[Subscript[x, i], {i, (Length[First[laggeddata]] - 1)/2}], 
      Table[Subscript[x, i], {i, Length[First[laggeddata]] - 1}]][
     "FitResiduals"]^2];
  grangerstat = ((res2 - res1)/
      lag)/(res1/(Length[laggeddata] - 2*lag - 1));
  1 - CDF[FRatioDistribution[lag, Length[laggeddata] - 2*lag - 1], 
    grangerstat]
  ];


End[] (* End Private Context *)

EndPackage[]