(* Initiate seasonal adjustment. *)


Block[
	{fileList=FileNames["*.m",DirectoryName[$InputFileName]]},
	fileList=DeleteCases[fileList, $InputFileName];
	Get[#,Path -> {DirectoryName[$InputFileName]}]&/@fileList];