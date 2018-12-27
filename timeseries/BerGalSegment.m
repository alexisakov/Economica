Package["Economica`"]

(*BerGal segmentation
iav 26 jul 2012*)

delta[list_,minlength_:3]:=Module[
	{mu,sig,P1,P2,part},
	Which[
	(*if there is a possibility that when we cut the list it will be shorter that threshold we do not do that*)
	Length[list]<= 2 minlength,
	list,
	(*if the list is flat use this algo*)
	Length[Dimensions[list]]==1,
	mu=Mean[list];
	sig=StandardDeviation[list];
	P1=Likelihood[NormalDistribution[mu,sig],list];
	part={Take[list,{1,#}],Take[list,{#+1,Length[list]}]}&/@Range[minlength,Length[list]-minlength];

	P2=Times[Sequence@@Flatten[#]]&/@Map[Likelihood[NormalDistribution[Mean[#],StandardDeviation[#]],#]&,part,{-2}];
	P2/P1,
	(*if the list is not flat use this*)
	True,
	mu=Mean[list];
	sig=Covariance[list];
	P1=Likelihood[MultinormalDistribution[mu,sig],list];
	part={Take[list,{1,#}],Take[list,{#+1,Length[list]}]}&/@Range[minlength,Length[list]-minlength];

	P2=Times[Sequence@@Flatten[#]]&/@Map[Likelihood[MultinormalDistribution[Mean[#],Abs[Covariance[#]]],#]&,part,{-3}];
	P2/P1
	]
	]

(*cut - splits the list in two segments if it is long enough*)
cut[list_,minlength_:3,criticalValue_:6]:=Module[
	{ds,mx,pstn},
	Which[Length[list]<= 2 minlength,
	list,
	True,
	ds=delta[list,minlength];
	mx=Check[Max[ds],0];
	Which[mx<criticalValue,
	list,
	True,
	pstn=First[Position[ds,mx]][[1]];
	{list[[;;pstn+minlength-1]],list[[pstn+minlength;;]]}
	]]]
(*function glue glues together consecutive segments*)
glue[list_]:=Module[{},
Which[Length[list]<=4,list,
True,Flatten[#,1]&/@Partition[list,3,3,1,{}]
]]

(*Complete*)
BerGalSegment[list_,minlength_:3,criticalValue_:6]:=Module[
	{ll,depth=Which[Length[Dimensions[list]]==1,-2,True,-3]},
	ll=FixedPoint[
	Map[cut[#,minlength,criticalValue]&,#,{depth}]&,{list}];
	ll=Cases[ll,List[__],{depth}];
	Do[
	ll=glue[ll];
	ll=FixedPoint[
	Map[Function[u,cut[u,minlength,criticalValue]],#,{depth}]&,{ll}];
	ll=Cases[ll,List[__],{depth}];
	ll>>"storedresult";
	,{i,10}];
	ll]

(*this is a function to plot segments given segmentation and date list.
total number of observations should match that of the dates*)
segPlot[segs_,dates_,opts___]/;Length[Flatten[segs,1]]==Length[dates]:=Module[
{(*first we should segment dates *)
d1=Take[dates,#]&/@(#+{1,0}&/@Partition[Prepend[Accumulate[Length/@segs],0],2,1]),dt},
(*we then attach obsevations to dates and then flatten the array with the case trick*)
dt=Cases[MapThread[{#1,#2}\[Transpose]&,{d1,segs}],List[___],{-4}];
DateListPlot[dt,opts]
]

PackageExport["segPlot"]
PackageExport["BerGalSegment"]