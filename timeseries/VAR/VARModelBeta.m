Package["Economica`"]

(* Exported symbols added here with SymbolName::usage *)  


VARModel::usage="VARModel[endo,nlags,ccase,exog,nlagsex]  performs an OLS estimation of the vector autoregressive process."

(*Sources:
*)

(*


TODO: Add lag choice procedures
TODO: Add tests for residiual stationarity, normality, lack of autocorrelation.
TODO: Check VAR stability
TODO: Add the standard form for output with Format[bin[x_,y_],StandardForm]:=MatrixForm[{{x},{y}}]

*)

VARModelFit[X_List,p_Integer,OptionsPattern[]]:=Module[
	{CoefficientEstimates,Y,Z,B},
Y=X[[p+1;;]]\[Transpose];
Z=Prepend[Flatten[#],1]&/@Partition[X[[;;-p]],p,1]\[Transpose];
B=Y.Z\[Transpose].Inverse[(Z.Z\[Transpose])];
CoefficientEstimates=B;
FittedModel[{"VARModel",p},
		X,
		Evaluate@CoefficientEstimates,
		(B.Z)\[Transpose],(*estimated Y*)
		(*error*)(Y-B.Z)\[Transpose]
]
]


(* Functions to estimate the parameters*)


(* 1. Yule-Walker Estimation
*)
varEstimateYW[data_,order_]:=Module[
{rows,m,means,autocovs,autocovMatrix,autocovs2,autocovVector,s},
rows=data\[Transpose]; (*first row are names*)
m=Length[rows];
means={Map[Mean[#]&,rows]}//Transpose;
autocovs=Table[Table[autocov[rows,k],{k,0-s,order-1-s}],{s,0,order-1}];
autocovMatrix=Partition[Flatten[Table[Table[Flatten[(autocovs[[j]])[[All,i]]],{i,1,m}],{j,1,order}]],m*order];
autocovs2=Table[autocov[rows,k],{k,1,order}];
autocovVector=Table[Flatten[(autocovs2)[[All,i]]],{i,1,m}];

autocovVector.Inverse[autocovMatrix]
];

autocov[rows_,lag_]:=Module[{ytt,time1,means},
means={Map[Mean[#]&,rows]}//Transpose;
ytt=Transpose[rows];
time1=Length[ytt];
1./time1(*-Abs[lag]*)*If[lag>=0,
			Sum[(Transpose[{ytt[[kk]]}]-means).Transpose[Transpose[{ytt[[kk-Abs[lag]]]}]-means],{kk,Abs[lag]+1,time1}],
			Transpose[Sum[(Transpose[{ytt[[kk]]}]-means).Transpose[Transpose[{ytt[[kk-Abs[lag]]]}]-means],{kk,Abs[lag]+1,time1}]]
			]
];

(* 2. OLS Estimation
*)

VARBetaEstimateOLS[data_,order_]:=Module[{rows,matX,Y},
rows=data\[Transpose];
matX=Partition[
Flatten[Reverse[Table[
	Table[Take[rows[[k]],{i,Length[rows[[1]]]-order+i-1}],{k,1,Length[rows]}]
	,{i,1,order}
]]],Length[rows[[1]]]-order]//Transpose;
Y=Map[Take[#,-Length[rows[[1]]]+order]&,rows];
Map[vector[matX,#]&,Y]
];

vector[x_,y_]:=Inverse[Transpose[x].x].Transpose[x].y;


(*
This code is for generating random orthonormal matrices.
It is written by L.Shifring.
http://www.mathprogramming-intro.org/book/node531.html
*)
oneStepOrtogonalize[vec_List,{}]:=vec;
oneStepOrtogonalize[vec_List,vecmat_?MatrixQ]:=Fold[#1-(vec.#2)/(#2.#2)*#2&,vec,vecmat]
GSOrtogonalize[startvecs_?MatrixQ]:=Fold[Append[#1,oneStepOrtogonalize[#2,#1]]&,{},startvecs];
normalize[vec_List]:=vec/Sqrt[Dot[vec,vec]];
GSOrthoNormalize[startvecs_?MatrixQ]:=Map[normalize,GSOrtogonalize[startvecs]];
randomOrthogonal[n_Integer?Positive]:=GSOrthoNormalize[NestWhile[Array[Random[]&,{n,n}]&,{{0}},
Det[#]===0&]]


