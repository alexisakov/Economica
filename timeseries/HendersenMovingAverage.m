Package["Economica`"]

(* :Summary:
See: Office for National Statistics (2006), Fitting trends to time series data, http://goo.gl/S6UfHh *)

HendersenMovingAverage::usage = "";

HendersenMovingAverage[l_, m_] := Module[
  {w = (315 ((m + 1)^2 - #^2) ((m + 2)^2 - #^2) ((m + 
           3)^2 - #^2) (3 (m + 2)^2 - 11 #^2 - 16))/(
      8 (m + 2) ((m + 2)^2 - 1) (4 (m + 2)^2 - 1) (4 (m + 2)^2 - 
         9) (4 (m + 2)^2 - 25)) & /@ Range[-m, m]
   },
  
  MovingAverage[l, w]
  
  ]
PackageExport["HendersenMovingAverage"]

