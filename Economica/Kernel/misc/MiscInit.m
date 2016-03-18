(* Mathematica package *)

(*This file loads all other DFM files

DONE: Write the loader (2014-11-08)
*)
Block[
	{fileList=FileNames["*.m",DirectoryName[$InputFileName],2]},
	fileList=DeleteCases[fileList, $InputFileName];
	Get[#,Path -> {DirectoryName[$InputFileName]}]&/@fileList];
