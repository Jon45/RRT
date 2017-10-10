(* ::Package:: *)

(*Ejercicio 1.2*)
Needs[ "RandomData`"]
RandomTable = Table [RandomData[],1000];
Histogram[RandomTable]



(*Ejercicio 1.3*)
lambda=50;
RandomExpTable=Table[RandomExp[lambda],1000];
Histogram[RandomExpTable]
