(* Mathematica Package *)

BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  


zeros::usage="zeros[x1,x2,...] "
eye::usage="zeros[n] "
squeeze::usage="squeeze[M] "
linspace::usage="linspace[x1,x2,n] "


(*Sources:
1. Diebold, F.X. and R.S. Mariano (1995), "Comparing predictive accuracy", Journal of Business & Economic Statistics, 13, 253-263.
2. Ibisevic, S., DM Test, MATLAB Central, URL: http://www.mathworks.com/matlabcentral/fileexchange/33979-diebold-mariano-test-statistic
3. http://mathematica.stackexchange.com/questions/38530/how-to-partition-a-list-in-a-specific-way/38550#38550

*)



Begin["`Private`"] (* Begin Private Context *) 


zeros[x : Sequence[Integer__]] := ConstantArray[0, {x}];
eye[x_Integer] := IdentityMatrix[x];
squeeze[A_] := ArrayReshape[A, Dimensions[A]~DeleteCases~1];


linspace[x_?NumericQ, y_?NumericQ, n_Integer] := Array[# &, n, {x, y}];

linspace[x_List, y_List, n_Integer] := 
 Module[{temp},
  If[Dimensions@x != Dimensions@y, Abort[]];
  temp = MapThread[linspace[#1, #2, n] &, {x, y}, 1];
  temp\[Transpose]
  ];




End[] (* End Private Context *)

EndPackage[]