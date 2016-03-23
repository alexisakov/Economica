(* Mathematica package *)

BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

DFMStandardizeMissing::usage="DFMStandardizeMissing[X] is a function to standardize matrices with missing observations.
It now only treats matrices and not vectors and only treats as missing string observations.";

DFMCorrectOutliers::usage="DFMCorrectOutliers[X] is an internal function to correct for outliers";

DFMFilter::usage="DFMFilter[X] is an internal filter function for DFM."

Begin["`Private`"] (* Begin Private Context *) 


DFMMovingAverageCentered[x_, k_] := Module[{
   y = ConstantArray[0, Length@x],
   xx
   },
  xx = N@Join[ConstantArray[First@x, k], x, ConstantArray[Last@x, k]];
  y = Mean /@ Partition[xx, 2 k + 1, 1]
  ];
  
Options[DFMCorrectOutliers] = {
	CorrectionType -> "ReplaceMissingWithCenteredAverage", 
	CenteredAverageWidth -> 3};
  
DFMCorrectOutliers[x_?VectorQ, OptionsPattern[]] := Module[{
   xx = x, n,
   Xc, Jc, Jmiss, median, xMA},
  n = Length@(xx);
  Xc = ConstantArray[Null, n];
  Jc = ConstantArray[0, n];
  Jmiss = Flatten@Position[x, Null];
  median = Median[x /. Null -> Sequence[]];
  Which[
   OptionValue@CorrectionType == "ReplaceMissingWithCenteredAverage",
   	xx[[Jmiss]] = median;
   	xMA = DFMMovingAverageCentered[xx, OptionValue[CenteredAverageWidth]];
   	xx[[Jmiss]] = xMA[[Jmiss]];,
   True,
   	Null;
   ];
  xx
  ];

DFMCorrectOutliers[x_?MatrixQ, OptionsPattern[]] := Module[{},
  (DFMCorrectOutliers[#, 
       Sequence @@ 
        Options@DFMCorrectOutliers] & /@ (x\[Transpose]))\[Transpose]
  ];
  
DFMStandardizeMissing[X_] := Module[
  {mask, \[Mu], \[Sigma],
   XwoNulls = (X\[Transpose]) /. Null :> Sequence[],
   Xdim = Dimensions[X]},
  mask = X /. {Null :> 0, x_?NumericQ :> 1};
  \[Mu] = Mean /@ XwoNulls;
  \[Sigma] = StandardDeviation /@ XwoNulls;
  ((X - ConstantArray[\[Mu], 
        First@Xdim]).DiagonalMatrix[\[Sigma]^-1]) /. 
   x_ /; MemberQ[{Power, Plus, Times}, Head@x] && Not@NumericQ[x] :> 
    Null
  ];

DFMFilter[b_List, a_List, x_List] := Module[{
   y, L = Length@x, P = Length@b - 1, Q = Length@a - 1, X}, 
  MapIndexed[(X[#2[[1]]] = #) &, x]; X[_] = 0; 
  y[0 | 0. | _?Negative] = 0; 
  y[n_] := y[
     n] = (Total[b Table[X[n - i], {i, 0, P}]] - 
       Total[Rest@a Table[y[n - j], {j, Q}]])/First@a; 
  Table[y[n], {n, 1, L}]];







(*DFMStandardizeMissing[X_] := Module[
  {mask, \[Mu], \[Sigma],
   Xtr = (X\[Transpose]) /. x_String :> Sequence[],
   Xdim = Dimensions[X]},
  mask = X /. {x_String :> 0, x_?NumericQ :> 1};
  \[Mu] = Mean /@ Xtr;
  \[Sigma] = StandardDeviation /@ Xtr;
  ((X - ConstantArray[\[Mu], First@Xdim]).DiagonalMatrix[\[Sigma]])*
   mask
  ]
*)


End[] (* End Private Context *)

EndPackage[]