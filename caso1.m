(* ::Package:: *)

(*Ejercicio 2*)
Needs[ "RandomData`"]
RandomTable = Table [RandomData[],1000];
Histogram[RandomTable]




