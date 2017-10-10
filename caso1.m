(* ::Package:: *)

(*Ejercicio 1.2*)
Needs[ "RandomData`"]
RandomTable = Table [RandomData[],1000];
Histogram[RandomTable]



(*Ejercicio 1.3*)
lambda=50;
RandomExpTable=Table[RandomExp[lambda],1000];
Histogram[RandomExpTable]


(*Ejercicio 2.1*)
nmax = 5000;
lambda = 50;
mu = 100;
(*Ejercicio 2.2*)
InterArrivalsTime = Table[RandomExp[lambda],nmax];
(*Ejercicio 2.3*)
ServiceTime = Table[RandomExp[mu],nmax];
(*Ejercicio 2.4*)
AcumSeries[listInterArrivalsTime_]:= Module[{AcumVal=0},AcumVal+=#&/@listInterArrivalsTime];
Arrivals = AcumSeries[InterArrivalsTime];
 (*Arrivals = Accumulate[InterArrivalsTime];*)
 (*& para que considere que es una funci\[OAcute]n*)
 (*Ejercicio 2.5*)
 FifoSchedulling [arrivals_,service_]:=Module[{n,checkTime},n=1;checkTime=arrivals[[1]]; (If [checkTime >= #,checkTime+=service[[n++]],checkTime=#+service[[n++]]])&/@arrivals];
 Departures = FifoSchedulling [InterArrivalsTime,ServiceTime];
 (*Ejercicio 2.6*)
 Manipulate[Show[ListPlot[Arrivals[[origin;;origin+width]]],ListPlot[Departures[[origin;;origin+width]]]],{origin,1,nmax-width,1},{width,1,50,1}]



