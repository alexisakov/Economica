Package["Economica`"]



ToSignedTextNumber[num_, prec_] := 
 StringTake[If[Sign@num == 1, "+", ""] <> 
  StringReplace[
   ToString@
    PaddedForm[num, {IntegerPart[Log[10, Abs[num]]] + prec, prec}], 
   WhitespaceCharacter :> ""],
   {1,-2}]
   
PackageExport["ToSignedTextNumber"]