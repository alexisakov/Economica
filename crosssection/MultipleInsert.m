Package["Economica`"]
(* Multiple insert is a function to insert simultaneously many values
into a list.
Source: http://mathematica.stackexchange.com/questions/30647/looking-for-a-way-to-insert-multiple-elements-into-multiple-positions-simultaneo*)

MultipleInsert[list_, val_, pos_] := 
 With[{no = Length[list], ni = Length[val]}, 
  SparseArray[Automatic, {2, no + 1}, 
     0, {1, {{0, ni, no + ni}, pos~Join~Range[no]~Partition~1}, 
      val~Join~list}]\[Transpose]["NonzeroValues"]]

PackageExport["MultipleInsert"]