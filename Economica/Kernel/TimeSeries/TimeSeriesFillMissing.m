
(*	TODO: Write an explanation of the this symbol.
  *)
  
  

ClearAll[TimeSeriesFillMissing];

TimeSeriesFillMissing[X_List, dateRange_List] := Module[{
   data = 
    Select[X, 
     AbsoluteTime@First@# >= AbsoluteTime@First@dateRange && 
       AbsoluteTime@First@# <=  AbsoluteTime@Last@dateRange &],
   shorterDate = dateRange[[All, 1 ;; 3]]
   },
  data[[All, 1]] = data[[All, 1, 1 ;; 3]];
  
  Replace[
   GatherBy[Join[List /@ shorterDate, #] & @data, First],
   {
    {x_List} :> {First@x, Null}, {x_List, y_List} :> y
    },
   1]]
  
