Package["Economica`"]


(* Exported symbols added here with SymbolName::usage *)  

(*
Source: By S. Dickson after H. Chernov, http://mathworld.wolfram.com/notebooks/Statistics/ChernoffFace.nb.
*)

ChernoffFace::usage = "ChernoffFace[{headecc_, eyesize_, eyespacing_, 
		eyeeccent_, pupsize_, browslant_, nozesize_,
		mouthshape_, mouthsize_, mouthopening_}]";

Off[General::spell1];

ChernoffFace[{headecc_, eyesize_, eyespacing_, eyeeccent_, 
	  pupsize_, browslant_, nozesize_, mouthshape_,
	  mouthsize_, mouthopening_}] :=
    Graphics[Flatten[
       {head[headecc],
        eyes[eyesize, eyespacing, eyeeccent, 
        	 pupsize],
        brows[browslant], nose[nozesize],
        mouth[mouthshape, mouthsize, mouthopening]}], 
                    AspectRatio -> Automatic,  
                    PlotRange-> {{-1.5, 1.5}, 
                    			 {-1.5, 1.5}}];
                    			 
 head[eccent_] := Block[
 		{xrad = 1 + (eccent - 5) / 25, 
  		 yrad = 1 - (eccent - 5) / 25},
         	Circle[{0, 0}, (1 + Abs[eccent - 5]/20)*{xrad, yrad}]];
         	          
eyes[size_, spacing_, eccent_, pupsize_] := Block[
        {xcenter = (1/3) + (spacing - 5) / 30,
         xrad = (1/6) + ((size - 5) + (eccent - 5)) 
         				  / 70,
         yrad = (1/6) + ((size - 5) - (eccent - 5)) 
         				  / 70},
  {Circle[{xcenter, 1/3},  {xrad, yrad}],
    PointSize[(pupsize + 1) / 200], 
    Point[{xcenter, 1/3}],
    Circle[{-xcenter, 1/3},  {xrad, yrad}],
    PointSize[(pupsize + 1) / 200], 
    Point[{-xcenter, 1/3}]}];
                    			 
brows[slant_] :=  Block[
       {xstart = (1/3) - (1/6) Cos[(slant - 5) Pi 
       							    / 20],
        ystart = (2/3) - (1/6) Sin[(slant - 5) Pi 
        							/ 20],
        xend = (1/3) + (1/6) Cos[(slant - 5) Pi 
        						  / 20],
        yend =  (2/3) + (1/6) Sin[(slant - 5) Pi 
        						   / 20]},
    {Line[{{xstart, ystart}, {xend, yend}}],
     Line[{{-xstart, ystart}, {-xend, yend}}]}
                       ];
nose[size_] := Block[
        {scale = 1 + (size - 5) / 13},
 Line[scale {{0, 1/6}, {-1/6, -1/6}, 
 			 {1/6, -1/6}, {0, 1/6}}]
                    ];
mouth[shape_, size_, opening_] := Block[
  {fx, gx, xstart, xend, ystart, ymin, ymax, xstep },
 xstart = -1/3 - (size - 5) / 15.; 
 xend = 1/3 + (size - 5) / 15.;
 ystart = -1/2 + (shape - 5.) * size / 150;
 ymax = -1/2 + (.9 opening - 1) / 27;
 ymin = -1/2 - (.9 opening - 1) / 30;
 fx = Fit[{{xstart, ystart}, {0, ymax}, 
 		   {xend, ystart}}, {1, x, x^2}, x];
 gx = Fit[{{xstart, ystart}, {0, ymin},
 		   {xend, ystart}}, {1, x, x^2}, x];
 xstep = (xend - xstart) / 10;
    {Line[Table[{x, fx}, {x, xstart, xend, xstep}]],
     Line[Table[{x, gx}, {x, xstart, xend, xstep}]]}];
     
On[General::spell1];

PackageExport["ChernoffFace"]