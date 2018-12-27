Package["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

DFMTransitionEquationFit::usage="Internal DFM functions";


Options[DFMTransitionEquationFit]={
	NumberOfDynamicFactors->2,
	NumberOfDynamicFactorsLags->2};

DFMTransitionEquationFit[F_, ModelParameters_, OptionsPattern[]] := 
 Module[
  {modelParameters = ModelParameters,
   nobs, r, p, identityMat, A, A1, errors, q,
   eigValues, eigVectors, varmodel, H, Q},
  {nobs, r} = Dimensions@(F);
  (*TODO: Implement lag selection and dynamic factor selection.*)
  If[
   OptionValue@NumberOfDynamicFactors == Automatic || 
    OptionValue@NumberOfDynamicFactorsLags == Automatic,
   	Message[DFMTransitionEquationFit::nyimp]; Abort[]];
  	
  p = OptionValue@NumberOfDynamicFactorsLags;
  
  identityMat = IdentityMatrix[r*p];
  A = Join[zeros[r, r*p], identityMat[[;; -1 - r]]];
  
  varmodel = VARModelFit[F, p, 0];
  A1 = varmodel["Ft"];
  errors = varmodel["FitResiduals"];(*z-Z.A1;*)
  
  A[[ ;; r, ;; r*p]] = A1\[Transpose];
  (* Find optimal q as from Bai& Ng (2005)*)
  q = Min@{OptionValue@NumberOfDynamicFactors, r};
  (*% Form reduced rank covariances Q=H*H' from resids E*)
  If[ r > 1,
   	{eigValues, eigVectors} = Eigensystem[Covariance@errors, q];
   	eigValues = DiagonalMatrix[eigValues];	
   	eigVectors = eigVectors\[Transpose];
   	eigVectors = eigVectors.DiagonalMatrix[Sign@(eigVectors[[1, All]])];
   ,
   (*else*)	
   eigVectors = 1; 
   eigValues = Covariance@(errors)];
  
  H = ConstantArray[0, {p*r, p*r}];
  Q = ConstantArray[0, {p*r, p*r}];
  
  
  
  H[[1 ;; r, 1 ;; q]] = eigVectors.Sqrt[eigValues];
  Q[[1 ;; r, 1 ;; r]] = eigVectors.eigValues.(eigVectors\[Transpose]);
  
  (*final asignments*)
  modelParameters["A"] = A; modelParameters["H"] = H; 
  modelParameters["p"] = p;
  modelParameters["Q"] = Q; modelParameters["q"] = q; 
  modelParameters["r"] = r;
  (*export*)
  modelParameters
  ]


