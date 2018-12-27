(* Mathematica Package *)

Package["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  


DieboldMarianoTest::usage="DieboldMarianoTest[e1,e2,h]  returns a p-value for the null hypothesis that the two forecasters are equally accurate.
Use as inputs e1 - first forecaster errors, e2 - second forecaster errors, h - parameter to account for the autocorrelation in the loss differential."


(*Sources:
1. Diebold, F.X. and R.S. Mariano (1995), "Comparing predictive accuracy", Journal of Business & Economic Statistics, 13, 253-263.
2. Ibisevic, S., DM Test, MATLAB Central, URL: http://www.mathworks.com/matlabcentral/fileexchange/33979-diebold-mariano-test-statistic
*)

DieboldMarianoTest[errors1_, errors2_, h_] := Module[
  {T = Length[errors1],
   d = errors1^2 - errors2^2,
   gamma = {}, dMean, gamma0, sampleCov, varD, x, i
   },
  dMean = Mean[d];
  gamma0 = Variance[d];
  If[h > 1,
   	gamma = ConstantArray[0, h - 1];
   	For[i = 1, i <= h - 1, i++,
    		sampleCov = Covariance[d[[1 + i ;; T]], d[[1 ;; T - i]]];
    		gamma = Append[gamma, sampleCov];
    		];
   	varD = gamma0 + 2 Total[gamma],
   	varD = gamma0;
   ];
  Probability[x >= varD, x \[Distributed] NormalDistribution[]]
  ];
  
  DieboldMarianoTest[errors1_, errors2_] := DieboldMarianoTest[errors1, errors2, 1];




PackageExport["DieboldMarianoTest"]