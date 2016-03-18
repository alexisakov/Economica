(* :Title: HPFilter *)

(* :Author: Johannes Ludsteck, code improvements by Ekkehart Schlicht and Luci Ellis*)

(* :Summary:
   This package provides functions to compute the
   Hodrick-Prescott-Filter and to estimate the optimum smoothing
   constant.
   References:
   Estimating the Smoothing Parameter in the So-called Hodrick-Prescott Filter, Journal of the Japan Statistical Society, 35(1), 2005, 99-119.
*)

(*
$Log: HPFilter.m,v $
Revision 1.5 2013/09/04 21:00
Fixes to make compatible with version 9
added one-sided version

Revision 1.4  2005/10/05 16:54:22  katz
Tune to increase speed. Some matrices are wrapped in N[].
Transpose[p].p is passed as argument to H[...].

Revision 1.3  2004/08/03 18:51:56  katz
Added $Log: HPFilter.m,v $
Added Revision 1.4  2005/10/05 16:54:22  katz
Added Tune to increase speed. Some matrices are wrapped in N[].
Added Transpose[p].p is passed as argument to H[...].
Added to obtain version information for future rcs logins.
*)

(* :Mathematica Version: 5.0.1 *)

BeginPackage["HPFilter`HPFilter`"];

Unprotect[{HPFilter, HPlot}];

HPFilter::usage = "HPFilter[x] computes the Hodrick-Prescott Filter according to
the method by Schlicht (2004). In case of success the Function returns a list
{ tr, { alpha, vu, vv } }
where tr is the estimated smooth component, alpha is the optimum smoothing constant,
vu is the variance of the irregular component and vv the variance of tr.
If numerical problems arise during the search for the optimum smoothing constant,
HPFilter returns the object Null. This flag indicates specification or data problems:
Either the data do not contain sufficient information to identify the smoothing
constant or the data generating process of the series cannot be approximanted
by the process underlying the model.
HPFilter[x, nmaxopts] hands nmaxopts over to NMaximize which is called by
HPFilter to compute the optimum smoothing parameter. Note that (due to a bug in Mathematica) HPFilter uses the NMaximize option Method -> {NelderMead, PostProcess->False}. If this is changed by the user, HPFilter may not work properly or even crash Mathematica. If the option ReportPredictedTrendVariance -> True is given, HPFilter returns
{ tr, vtr, {alpha, vu, vu}} where vtr is the list of variances of the predicted
trend. HPFilter[x, alpha] computes and returns the smooth trend for given
smoothing parameter alpha.";

HPlot::usage = "HPlot[x, {amin, amax}] generates a plot of the criterion
function H[x,alpha] of series x in the range {amin, amax}."

OneSidedHPFilter::usage = "OneSidedHPFilter[data,alpha] computes the one-sided Hodrick-Prescott filter, with an initial sample of 3 and a smoothing parameter of alpha.
OneSidedHPFilter[data] computes the one-sided Hodrick-Prescott filter, with an initial sample of 3 and an automatically selected smoothing parameter that is optimised for the whole sample."

Options[HPFilter] = { ReportPredictedTrendVariance -> False };
SetOptions[NMaximize, Method -> {"NelderMead", "PostProcess" -> False}]

Begin["`Private`"];



makeP[T_] := SparseArray[
    {{i_, i_} -> 1,
      {i_, j_} /; j == i + 1 -> -2,
      {i_, j_} /; j == i + 2 -> 1},
    {T - 2, T}];

makeI[T_] := SparseArray[{i_, i_} -> 1, {T, T}];

H[x_, a_?NumericQ, i_, p_, pp_, ev_] := Module[{M, detM, R, u, v, T, y},
    T = Length[x];
    M = i + a pp;
    y = LinearSolve[M, x, Method -> "Cholesky"];
    u = x - y; v = p.y;
    R = (u.u + a v.v);
    detM = a^T Apply[Times, ev + 1/a];
    - Log[detM] + (T-2) (Log[a]-Log[R])];


HPFilter[x_List, opts:OptionsPattern[{HPFilter,NMaximize}]] :=
  Module[{T = Length[x], R, a, asol,
          rptv, ptv, y, u, v, vu, vv, p, pp, ev, i, result},
          
    rptv = OptionValue[ReportPredictedTrendVariance];
          
    p = N[makeP[T]];
    pp = Transpose[p].p; 
    ev = Eigenvalues[pp];
    i = N[makeI[T]];

    (* determine optimum value of alpha *)
    asol = Check[NMaximize[{H[x, a, i, p, pp, ev], 0.000001<= a},
                           {{a, 1.0, 5.0}},
                            Method -> OptionValue[Method]],
                 $Failed, NMaximize::"cvmit", LinearSolve::"npdef", Inverse::"luc"];


    If[asol === $Failed, Return[], a = a /. asol[[2]]];
    (* compute the smooth trend *)
    y = LinearSolve[i + a Transpose[p].p, x, Method -> "Cholesky"];

    u = x - y; v = p.y;
    R = (u.u + a v.v);
    vu = R/T; vv = R/(T a);
    result = {y, {a, vu, vv}};

    ptv = Check[Tr[vu Inverse[i + a Transpose[p].p], List],
                $Failed, Inverse::"luc"];

    If[ptv === $Failed, Return[]];

    If[rptv, result = Insert[result, ptv, 2]];

    result];

HPFilter[x_List, a_?Positive] :=
  Module[{T = Length[x], y, p},

    p = makeP[T]; i = makeI[T];
    y = Check[LinearSolve[i + a Transpose[p].p, x, Method -> "Cholesky"],
              $Failed, LinearSolve::"npdef"];
    If[y === $Failed, Null, y]];


HPlot[x_ /; VectorQ[x, NumericQ], {aMin_?NumberQ, aMax_?NumberQ}, opts___] := Module[
    {a, i = makeI[Length[x]], p = makeP[Length[x]]},
    Plot[H[x, a, i, p], {a, aMin, aMax}, opts]];

OneSidedHPFilter[data:{__?NumericQ}, a_?Positive] := 
 Table[Last[HPFilter[Take[data, i], a]], {i, 3, Length[data]}]

OneSidedHPFilter[data:{__?NumericQ} ] := 
With[{a = HPFilter[data][[2,1]]},
 Table[Last[HPFilter[Take[data, i], a]], {i, 3, Length[data]}] ]

SetAttributes[{HPFilter, HPlot}, {Protected, ReadProtected}];

End[];
EndPackage[];

