(* ::Package:: *)

<<  /home/jon/RRT/RandomData.m
<<  /home/jon/RRT/drawTx.m


a={0,9600,1,0,0};
SetIniParDraw[.5,0];
Show[DrawPacketTx[a],DrawWin[0,10,8]]  (*Tiempo de ack: bits, tiempo del paquete es longitud*)

lstPck = Table[{2+n*10,100,n+1,0,0},{n,0,9}];
Show[DrawPacketTx[#]&/@lstPck,DrawWin[0,100,8]]


(*Ejercicio 2*)

nmax = 1000;
lambda = 1;
mu = 10;

InterArrivalsTime = Table[RandomExp[lambda],nmax];

ServiceTime = Table[RandomExp[mu],nmax];

AcumSeries[listInterArrivalsTime_]:= Module[{AcumVal=0},AcumVal+=#&/@listInterArrivalsTime];
Arrivals = AcumSeries[InterArrivalsTime];

FifoSchedulling [arrivals_,service_]:=Module[{n,checkTime},n=1;checkTime=arrivals[[1]]; (If [checkTime >= #,checkTime+=service[[n++]],checkTime=#+service[[n++]]])&/@arrivals];
Departures = FifoSchedulling [Arrivals,ServiceTime];

injTimes = Departures - ServiceTime;
(*sequenceNumbers = Table[n,{n,1,9,1}];
lstPck = Table[{2+n*10,10,n+1,0,0},{n,0,9}]*)

sequenceNumbers = Table[n,{n,1,Length[injTimes],1}];
lstPck = MapThread[({#1,9600*#2,#3,0,0})&,{injTimes,ServiceTime,sequenceNumbers}];


Manipulate [ Show [ DrawWin[origin,ww,8],Map[DrawPacketTx[#]&, SelectPacketInWin[lstPck]]],{origin,0,100-ww},{ww,1,50}]



