(* Mathematica package *)

(*This file loads all other VAR files

DONE: Write the loader (2014-09-20)
*)


Get["VARMakeXY`",Path -> {DirectoryName[$InputFileName]}];
Get["VARMakeLags`",Path -> {DirectoryName[$InputFileName]}];
Get["VARModelFit`",Path -> {DirectoryName[$InputFileName]}];
Get["VARImpulseResponse`",Path -> {DirectoryName[$InputFileName]}];
Get["VARAIC`",Path -> {DirectoryName[$InputFileName]}];
Get["VARImpulseResponseConfidenceInterval`",Path -> {DirectoryName[$InputFileName]}];