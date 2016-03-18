(* Mathematica package *)

BeginPackage["Economica`"]
(* Exported symbols added here with SymbolName::usage *)  

DFMOLSModelFit::usage="Internal DFM function";

Begin["`Private`"] (* Begin Private Context *) 

Options[DFMOLSModelFit] = {
	StandardizeData -> False,
	IncludeConstantBasis -> False};
  
(*TODO: Check all the stages of the OLS*)
DFMOLSModelFit::nyimp = 
  "Standardize with missing values is not yet implemented.";
  
  
DFMOLSModelFit[DFMParameters_, OptionsPattern[]] := Module[{
	z = DFMParameters["OLSX"], 
  	y = DFMParameters["OLSY"],
   	zNull, yNull, positionNull, nonNullPositions, b, \[CapitalSigma],
   parameters=DFMParameters},
  If[OptionValue[StandardizeData],
   Message[DFMOLSModelFit::nyimp]; Abort[]];
  
  If[OptionValue[IncludeConstantBasis],
  	z = Join[ConstantArray[{1}, Length@z], z, 2]];
  (*Delete all missing observations*)
  zNull = Position[z, {___, Null, ___}];
  yNull = If[
  	VectorQ@y, Position[y, Null], Position[y, {___, Null, ___}]
  	];
  positionNull = Union[Join[Flatten@yNull, Flatten@zNull]];
  nonNullPositions = Complement[Range@Length@z, positionNull];
  z = z[[nonNullPositions]];
  y = y[[nonNullPositions]];
  (*Estimate betas*)
  b = LeastSquares[z, y];

	If[MatrixQ@Covariance[y - z.b],
	  \[CapitalSigma] = CholeskyDecomposition[Covariance[y - z.b]],
	  \[CapitalSigma] = Sqrt@Covariance[y - z.b]
	  ];

	parameters["s"]= \[CapitalSigma];
    parameters["beta"]= List/@b;
    
    parameters    
  ]

End[] (* End Private Context *)

EndPackage[]