(* ::Package:: *)

(* ::Section::Closed:: *)
(*Package Imports*)


<<MaTeX`;
<<Notation`;


(* ::Section::Closed:: *)
(*Notation*)


(* ::Text:: *)
(*Symbolizes every subscripted variable (Warning this breaks functions like TraditionalForm and TeXForm)*)


Symbolize[ParsedBoxWrapper[SubscriptBox["_","_"]]]


(* ::Section:: *)
(*Assumptions*)


$Assumptions={{\[Gamma],\[Beta],r,\[Theta],\[Phi],t,\[Tau],R,\[Omega],k,\[Epsilon],\[Mu],\[Sigma],Subscript[\[Mu], 0],Subscript[\[Epsilon], 0],c}\[Element]PositiveReals,{x,y,z,Subscript[E, 0],Subscript[B, 0]}\[Element]Reals,{m,n,k}\[Element]NonNegativeIntegers};


(* ::Section::Closed:: *)
(*Constants*)


fundamentalConstants={q->Quantity[1,"ElementaryCharge"],Subscript[\[Mu], 0]->Quantity[1,"MagneticConstant"],c->Quantity[1,"SpeedOfLight"],Subscript[m, e]->Quantity[1,"ElectronMass"],g->Quantity[1,"Gravs"],Subscript[\[Epsilon], 0]->Quantity[1,"ElectricConstant"],Subscript[m, p]->Quantity[1,"ProtonMass"]};


(* ::Section:: *)
(*General Functions*)


TimeAverage[expr_]:=(\[Omega] \!\(
\*SubsuperscriptBox[\(\[Integral]\), \(0\), 
FractionBox[\(2\ \[Pi]\), \(\[Omega]\)]]\(expr \[DifferentialD]t\)\))/(2 \[Pi])
CartesianBasis=OverHat/@{x,y,z};
SphericalBasis=OverHat/@{r,\[Theta],\[Phi]};
CartesianToSphericalField[field_]:=TransformedField["Cartesian"->"Spherical",field,{x,y,z}->{r,\[Theta],\[Phi]}]
SphericalToCartesianField[field_]:=TransformedField["Spherical"->"Cartesian",field,{r,\[Theta],\[Phi]}->{x,y,z}]
(* Do not change the form of this or for some reason this won't compile *)
SphericalIntegral[expr_]:=\!\(\*
TagBox[
StyleBox[
RowBox[{"Integrate", "[", 
RowBox[{
RowBox[{"Times", "[", 
RowBox[{
RowBox[{"Power", "[", 
RowBox[{"r", ",", "2"}], "]"}], ",", "expr", ",", 
RowBox[{"Sin", "[", "\\[Theta]", "]"}]}], "]"}], ",", 
RowBox[{"List", "[", 
RowBox[{"\\[Theta]", ",", "0", ",", "Pi"}], "]"}], ",", 
RowBox[{"List", "[", 
RowBox[{"\\[Phi]", ",", "0", ",", 
RowBox[{"Times", "[", 
RowBox[{"2", ",", "Pi"}], "]"}]}], "]"}]}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\)
QuantityReplace[quants_,OptionsPattern[{Fun->True}]][expr_]:=expr/. Table[If[QuantityQ[q[[2]]],q[[1]]->q[[2]],q[[1]]->Quantity[q[[2]]]],{q,quants}]/. If[OptionValue[Fun],fundamentalConstants,{}]
PoyntingVector[ef_,bf_]:=FullSimplify[Re[ComplexExpand[ef]]]\[Cross]FullSimplify[Re[ComplexExpand[bf]]]/Subscript[\[Mu], 0]


(* ::Section:: *)
(*Optics*)


(* ::Section:: *)
(*Conductors*)


KCond=Sqrt[Subscript[\[Mu], 0] \[Epsilon] \[Omega]^2+I Subscript[\[Mu], 0] \[Sigma] \[Omega]];
kCond=\[Omega] Sqrt[(Subscript[\[Mu], 0] \[Epsilon])/2] Sqrt[Sqrt[1+(\[Sigma]/(\[Epsilon] \[Omega]))^2]+1];
dCondInv=\[Omega] Sqrt[(Subscript[\[Mu], 0] \[Epsilon])/2] Sqrt[Sqrt[1+(\[Sigma]/(\[Epsilon] \[Omega]))^2]-1];
bCond=(c Sqrt[\[Epsilon] Subscript[\[Mu], 0]] (I Sqrt[-1+Sqrt[1+\[Sigma]^2/(\[Epsilon]^2 \[Omega]^2)]]+Sqrt[1+Sqrt[1+\[Sigma]^2/(\[Epsilon]^2 \[Omega]^2)]]))/(Sqrt[2] n);
RFCond=(n^2 \[Omega]-c^2 Subscript[\[Mu], 0] Sqrt[\[Sigma]^2+\[Epsilon]^2 \[Omega]^2]-I Sqrt[2] c n Sqrt[Subscript[\[Mu], 0] \[Omega] (-\[Epsilon] \[Omega]+Sqrt[\[Sigma]^2+\[Epsilon]^2 \[Omega]^2])])/(n^2 \[Omega]+c^2 Subscript[\[Mu], 0] Sqrt[\[Sigma]^2+\[Epsilon]^2 \[Omega]^2]+Sqrt[2] c n Sqrt[Subscript[\[Mu], 0] \[Omega] (\[Epsilon] \[Omega]+Sqrt[\[Sigma]^2+\[Epsilon]^2 \[Omega]^2])]);
TFCond=(4 n)/(2 n+Sqrt[2] c (I Sqrt[\[Epsilon] Subscript[\[Mu], 0] (-1+Sqrt[1+\[Sigma]^2/(\[Epsilon]^2 \[Omega]^2)])]+Sqrt[\[Epsilon] Subscript[\[Mu], 0] (1+Sqrt[1+\[Sigma]^2/(\[Epsilon]^2 \[Omega]^2)])]));
RCfCond=(n^4 \[Omega]^2-2 c^2 n^2 \[Epsilon] Subscript[\[Mu], 0] \[Omega]^2+c^4 \!\(\*SubsuperscriptBox[\(\[Mu]\), \(0\), \(2\)]\) (\[Sigma]^2+\[Epsilon]^2 \[Omega]^2))/(n^2 \[Omega]+c^2 Subscript[\[Mu], 0] Sqrt[\[Sigma]^2+\[Epsilon]^2 \[Omega]^2]+Sqrt[2] c n Sqrt[Subscript[\[Mu], 0] \[Omega] (\[Epsilon] \[Omega]+Sqrt[\[Sigma]^2+\[Epsilon]^2 \[Omega]^2])])^2;


(* ::Section:: *)
(*Plasma*)


(* ::Section:: *)
(*Transmission Lines*)


(* ::Section:: *)
(*Dipoles*)


DipoleEField[p_]:=(Subscript[\[Mu], 0] (({x,y,z}.\!\(
\*SubscriptBox[\(\[PartialD]\), \({t, 2}\)]p\) {x,y,z})/r^2-\!\(
\*SubscriptBox[\(\[PartialD]\), \({t, 2}\)]p\)))/(4 \[Pi] r)
DipoleBField[p_]:=-((Subscript[\[Mu], 0] {x,y,z}\[Cross]\!\(
\*SubscriptBox[\(\[PartialD]\), \({t, 2}\)]p\))/((4 \[Pi] r c) r))
DipolePoyntingVector[p_]:=(Subscript[\[Mu], 0] (1/(4 \[Pi] r))^2 (Total[(\!\(
\*SubscriptBox[\(\[PartialD]\), \({t, 2}\)]p\))^2]-({x,y,z}.\!\(
\*SubscriptBox[\(\[PartialD]\), \({t, 2}\)]p\)/r)^2) {x,y,z})/(c r)


(* ::Section::Closed:: *)
(*Relativity*)


BetaToGamma[\[Beta]_]:=1/Sqrt[1-\[Beta]^2]
GammaToBeta[\[Gamma]_]:=Sqrt[-1+\[Gamma]^2]/\[Gamma]
FieldTensor[e_,b_]:=Normal[Quiet[(#1-Transpose[#1]&)[SparseArray[{{1,n_}/;n>1->e[[n-1]]/c,{2,n_}/;n>2->Sign[7-2 n] b[[6-n]],{3,4}->b[[1]]},{4,4}]]]]
EfieldExtract[F_]:=c F[[1,2;;All]]
BfieldExtract[F_]:={F[[3,4]],-F[[2,4]],F[[2,3]]}
LorentzTensor=MatrixForm[{{\[Gamma],-\[Beta] \[Gamma],0,0},{-\[Beta] \[Gamma],\[Gamma],0,0},{0,0,1,0},{0,0,0,1}}];
FieldTensorLorentzTransformation[F_]:=Simplify[LorentzTensor.F.LorentzTensor]
