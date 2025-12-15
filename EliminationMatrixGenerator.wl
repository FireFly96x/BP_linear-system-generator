(* ::Package:: *)

BeginPackage["MojeGeneratory`EliminationMatrixGenerator`",
  {"MojeGeneratory`Common`"}
];

Internal`$ContextMarks = False;

Gen01::usage =
"Gen01[diff, mode, opts] vygeneruje pr\[IAcute]klad rie\[SHacek]enia s\[UAcute]stavy line\[AAcute]rnych rovn\[IAcute]c s\[CHacek]\[IAcute]tavacou (elimina\[CHacek]nou) met\[OAcute]dou.

diff:
  \"EASY\"   (2\[Times]2)
  \"MEDIUM\" (3\[Times]3) (moment\[AAcute]lne v k\[OAcute]de e\[SHacek]te nie je implementovan\[EAcute])
  \"HARD\"   (3\[Times]3) (moment\[AAcute]lne v k\[OAcute]de e\[SHacek]te nie je implementovan\[EAcute])

mode:
  \"TASK\"              \[Dash] vyp\[IAcute]\[SHacek]e iba zadanie
  \"TASK_RESULT\"       \[Dash] zadanie + v\[YAcute]sledok
  \"TASK_STEPS_RESULT\" \[Dash] zadanie + postup + v\[YAcute]sledok

opts:
  Visualization -> True|False   (graf len pre 2\[Times]2)
  SolutionType   -> Automatic|\"ONE\"|\"NONE\"|\"INFINITE\"
    - ak sa nezad\[AAcute] (Automatic): 80% \[SHacek]anca na pr\[AAcute]ve jedno rie\[SHacek]enie
    - \"ONE\"/\"NONE\"/\"INFINITE\" sl\[UAcute]\[ZHacek]i len na riadenie generovania; pou\[ZHacek]\[IAcute]vate\:013eovi sa nikdy nevypisuje.";

Gen01::baddiff  = "Neplatn\[AAcute] obtia\[ZHacek]nos\[THacek] `1`. Pou\[ZHacek]i \"EASY\"|\"MEDIUM\"|\"HARD\".";
Gen01::badmode  = "Neplatn\[YAcute] re\[ZHacek]im `1`. Pou\[ZHacek]i \"TASK\"|\"TASK_RESULT\"|\"TASK_STEPS_RESULT\".";
Gen01::notimpl  = "Obtia\[ZHacek]nos\[THacek] `1` zatia\:013e nie je implementovan\[AAcute] v tomto gener\[AAcute]tore.";
Gen01::fail     = "Nepodarilo sa vygenerova\[THacek] vhodn\[YAcute] pr\[IAcute]klad.";

Options[Gen01] = {
  SolutionType -> Automatic,
  Visualization -> False
};

Begin["`Private`"];

(* ======================== POMOCN\[CapitalEAcute] ======================== *)

cellPrint[expr_, style_String] :=
  If[Head[EvaluationNotebook[]] === NotebookObject,
    CellPrint @ Cell[expr, style],
    Print[expr]
  ];

CellSection[str_] := cellPrint[str, "Section"];
CellSubsection[str_] := cellPrint[str, "Subsection"];
CellText2[str_] := cellPrint[str, "Text"];
CellExpr2[expr_] := cellPrint[BoxData @ ToBoxes[expr], "Input"];

systemEquations[A_, b_, vars_] := Thread[A . vars == b];

coeffRangeByDiff["EASY"] := 5;
coeffRangeByDiff["MEDIUM"] := 5;
coeffRangeByDiff["HARD"] := 6;
coeffRangeByDiff[_] := 5;

boundByDiff["EASY"] := 60;
boundByDiff["MEDIUM"] := 90;
boundByDiff["HARD"] := 120;
boundByDiff[_] := 90;

smallInt[r_] := RandomInteger[{-r, r}];

randomRow[n_, r_] :=
 Module[{v},
  v = Table[smallInt[r], {n}];
  If[AllTrue[v, # == 0 &], randomRow[n, r], v]
 ];

matrixDetNonZeroQ[A_] := Quiet @ Check[Det[A] =!= 0, False];

numbersNiceQ[A_, b_, diff_] :=
 Module[{bd = boundByDiff[diff]},
  Max[Abs @ Join[Flatten[A], Flatten[b]]] <= bd
 ];

(* bezpe\[CHacek]n\[EAcute] \[OpenCurlyDoubleQuote]dr\[ZHacek]iace\[CloseCurlyDoubleQuote] rovnice bez lhs$ / cy$ / {x,y}[[2]] *)
holdEq[lhs_, rhs_] := With[{L = lhs, R = rhs}, HoldForm[L == R]];

highlightTerm[expr_] := Style[expr, Bold, Background -> Lighter[Yellow, 0.7]];

multipliedEq[row_, rhs_, vars_, mult_, cancelIndex_] := Module[{lhsTerms, lhsExpr, rr},
  rr = mult*row;
  lhsTerms = Table[
    If[i == cancelIndex,
      highlightTerm[rr[[i]]*vars[[i]]],
      rr[[i]]*vars[[i]]
    ],
    {i, Length[vars]}
  ];
  lhsExpr = Plus @@ lhsTerms;
  holdEq[lhsExpr, mult*rhs]
];

(* ======================== GENEROVANIE ======================== *)

generateSystemOne[dim_, diff_] :=
 Module[{r, x0, A, b},
  r = coeffRangeByDiff[diff];
  x0 = RandomInteger[{-10, 10}, dim];
  A = Table[randomRow[dim, r], {dim}];
  If[!matrixDetNonZeroQ[A], Return[$Failed]];
  b = A . x0;
  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b, "x0" -> x0|>
 ];

generateSystemNone2[diff_] :=
 Module[{r, row1, row2, k, c1, c2, A, b},
  r = coeffRangeByDiff[diff];
  row1 = randomRow[2, r];
  k = RandomChoice[{2, -2, 3, -3}];
  row2 = k row1;

  c1 = RandomInteger[{-20, 20}];
  c2 = k c1 + RandomChoice[{-3, -2, -1, 1, 2, 3}];

  A = {row1, row2};
  b = {c1, c2};

  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b|>
 ];

generateSystemInfinite2[diff_] :=
 Module[{r, row1, row2, k, c1, A, b},
  r = coeffRangeByDiff[diff];

  (* aby parametriz\[AAcute]cia nemala zlomky: koeficient pri x bude \[PlusMinus]1 *)
  row1 = {RandomChoice[{-1, 1}], RandomInteger[{-r, r}]};
  If[row1[[2]] == 0, row1[[2]] = RandomChoice[{-r, -1, 1, r}]];

  k = RandomChoice[{2, -2, 3, -3}];
  row2 = k row1;

  c1 = RandomInteger[{-20, 20}];
  A = {row1, row2};
  b = {c1, k c1};

  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b|>
 ];

(* ======================== KROKY 2\[Times]2 ======================== *)

stepsOne2[A_, b_, vars_] :=
 Module[
  {a, c, d, f, l, m1, m2, by, cy, eqY, yVal, xVal},

  a = A[[1]]; c = b[[1]];
  d = A[[2]]; f = b[[2]];

  (* zru\[SHacek]\[IAcute]me x *)
  l = LCM[Abs[a[[1]]], Abs[d[[1]]]];
  m1 = l/a[[1]];
  m2 = -l/d[[1]];

  by = m1*a[[2]] + m2*d[[2]];
  cy = m1*c + m2*f;

  eqY  = holdEq[by*vars[[2]], cy];
  yVal = vars[[2]] /. First[Solve[ReleaseHold[eqY], vars[[2]]]];
  xVal = vars[[1]] /. First[Solve[a[[1]]*vars[[1]] + a[[2]]*yVal == c, vars[[1]]]];

  <|
    "Text" -> {
      "Slovne: Zru\[SHacek]\[IAcute]me premenn\[UAcute] " <> ToString[vars[[1]]] <> " vhodnou line\[AAcute]rnou kombin\[AAcute]ciou rovn\[IAcute]c.",
      "Medziv\[YAcute]sledn\[AAcute] s\[UAcute]stava: ostane rovnica len s " <> ToString[vars[[2]]] <> ".",
      "Zhrnutie: najprv vypo\[CHacek]\[IAcute]tame " <> ToString[vars[[2]]] <> ", potom dosad\[IAcute]me a dopo\[CHacek]\[IAcute]tame " <> ToString[vars[[1]]] <> "."
    },
    "Expr" -> {
      "P\[OHat]vodn\[AAcute] s\[UAcute]stava:",
      systemEquations[A, b, vars],

      "Vyn\[AAcute]sob\[IAcute]me 1. rovnicu \[CHacek]\[IAcute]slom " <> ToString[m1] <> " a 2. rovnicu \[CHacek]\[IAcute]slom " <> ToString[m2] <> " (zv\[YAcute]raznen\[EAcute] \[CHacek]leny sa vyru\[SHacek]ia):",
      multipliedEq[a, c, vars, m1, 1],
      multipliedEq[d, f, vars, m2, 1],

      "S\[CHacek]\[IAcute]tan\[IAcute]m rovn\[IAcute]c dostaneme:",
      eqY,

      "Teda:",
      holdEq[vars[[2]], yVal],

      "Dosad\[IAcute]me do 1. rovnice a dopo\[CHacek]\[IAcute]tame " <> ToString[vars[[1]]] <> ":",
      holdEq[vars[[1]], xVal]
    },
    "Solution" -> <|vars[[1]] -> xVal, vars[[2]] -> yVal|>
  |>
 ];

stepsNone2[A_, b_, vars_] :=
 Module[
  {a, c, d, f, l, m1, m2, cy, eqContrad},

  a = A[[1]]; c = b[[1]];
  d = A[[2]]; f = b[[2]];

  l = LCM[Abs[a[[1]]], Abs[d[[1]]]];
  m1 = l/a[[1]];
  m2 = -l/d[[1]];

  cy = m1*c + m2*f;
  eqContrad = holdEq[0, cy];

  <|
    "Text" -> {
      "Slovne: Pok\[UAcute]sime sa zru\[SHacek]i\[THacek] premenn\[UAcute] " <> ToString[vars[[1]]] <> ".",
      "Medziv\[YAcute]sledok: vznikne rovnica bez premenn\[YAcute]ch.",
      "Zhrnutie: Ak vyjde tvar 0 = nenulov\[EAcute] \[CHacek]\[IAcute]slo, s\[UAcute]stava nem\[AAcute] rie\[SHacek]enie."
    },
    "Expr" -> {
      "P\[OHat]vodn\[AAcute] s\[UAcute]stava:",
      systemEquations[A, b, vars],

      "Vyn\[AAcute]sob\[IAcute]me 1. rovnicu \[CHacek]\[IAcute]slom " <> ToString[m1] <> " a 2. rovnicu \[CHacek]\[IAcute]slom " <> ToString[m2] <> " (zv\[YAcute]raznen\[EAcute] \[CHacek]leny sa vyru\[SHacek]ia):",
      multipliedEq[a, c, vars, m1, 1],
      multipliedEq[d, f, vars, m2, 1],

      "Po s\[CHacek]\[IAcute]tan\[IAcute] dostaneme konkr\[EAcute]tny rozpor:",
      eqContrad
    },
    "ReasonEq" -> eqContrad,
    "Solution" -> "NONE"
  |>
 ];

stepsInfinite2[A_, b_, vars_] :=
 Module[
  {a, c, d, f, l, m1, m2, cy, eqIdentity, t, xExpr},

  a = A[[1]]; c = b[[1]];
  d = A[[2]]; f = b[[2]];

  l = LCM[Abs[a[[1]]], Abs[d[[1]]]];
  m1 = l/a[[1]];
  m2 = -l/d[[1]];

  cy = m1*c + m2*f;
  eqIdentity = holdEq[0, cy]; (* tu vyjde 0 = 0 *)

  t = \[FormalT];
  xExpr = (c - a[[2]]*t)/a[[1]]; (* a[[1]] je \[PlusMinus]1 -> bez zlomkov *)

  <|
    "Text" -> {
      "Slovne: Zru\[SHacek]\[IAcute]me premenn\[UAcute] " <> ToString[vars[[1]]] <> ". Ak dostaneme identitu 0 = 0, rovnice s\[UAcute] z\[AAcute]visl\[EAcute].",
      "Medziv\[YAcute]sledok: jedna rovnica sa \:201estrat\[IAcute]\[OpenCurlyDoubleQuote] a zostane vo\:013en\[YAcute] parameter.",
      "Zhrnutie: S\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute], lebo nevie ur\[CHacek]i\[THacek] obe premenn\[EAcute] jednozna\[CHacek]ne."
    },
    "Expr" -> {
      "P\[OHat]vodn\[AAcute] s\[UAcute]stava:",
      systemEquations[A, b, vars],

      "Vyn\[AAcute]sob\[IAcute]me 1. rovnicu \[CHacek]\[IAcute]slom " <> ToString[m1] <> " a 2. rovnicu \[CHacek]\[IAcute]slom " <> ToString[m2] <> " (zv\[YAcute]raznen\[EAcute] \[CHacek]leny sa vyru\[SHacek]ia):",
      multipliedEq[a, c, vars, m1, 1],
      multipliedEq[d, f, vars, m2, 1],

      "Po s\[CHacek]\[IAcute]tan\[IAcute] dostaneme konkr\[EAcute]tnu identitu:",
      eqIdentity,

      "Zave\[DHacek]me parameter t pre " <> ToString[vars[[2]]] <> ":",
      holdEq[vars[[2]], t],

      "Potom z 1. rovnice vyjde:",
      holdEq[vars[[1]], xExpr]
    },
    "ReasonEq" -> eqIdentity,
    "Solution" -> "INFINITE"
  |>
 ];

(* ======================== VIZUALIZ\[CapitalAAcute]CIA ======================== *)

visualize2[A_, b_, vars_] :=
 Module[{x, y, f1, f2},
  {x, y} = vars;
  f1 = A[[1]] . vars - b[[1]];
  f2 = A[[2]] . vars - b[[2]];
  CellText2["Vizualiz\[AAcute]cia (priamky):"];
  CellExpr2 @ ContourPlot[
    {f1, f2},
    {x, -10, 10}, {y, -10, 10},
    Contours -> {0},
    PlotPoints -> 60
  ]
 ];

(* ======================== HLAVN\[CapitalAAcute] FUNKCIA ======================== *)

Gen01[diff_String, mode_String, opts : OptionsPattern[]] :=
 Module[
  {dim, vars, vis, st, gen, data, A, b, steps, sol},

  If[!MemberQ[{"EASY", "MEDIUM", "HARD"}, diff],
    Message[Gen01::baddiff, diff]; Return[$Failed]
  ];

  If[!MemberQ[{"TASK", "TASK_RESULT", "TASK_STEPS_RESULT"}, mode],
    Message[Gen01::badmode, mode]; Return[$Failed]
  ];

  If[diff =!= "EASY",
    Message[Gen01::notimpl, diff];
    CellText2["T\[AAcute]to obtia\[ZHacek]nos\[THacek] zatia\:013e nie je implementovan\[AAcute] v gener\[AAcute]tore Gen01."];
    Return[$Failed]
  ];

  st  = ResolveSolutionType[OptionValue[SolutionType]];
  dim = DimensionByDifficulty["Elimination", diff];
  vars = Take[{x, y, z}, dim];
  vis = TrueQ[OptionValue[Visualization]];

  gen := Which[
    dim == 2 && st == "ONE",      generateSystemOne[2, diff],
    dim == 2 && st == "NONE",     generateSystemNone2[diff],
    dim == 2 && st == "INFINITE", generateSystemInfinite2[diff],
    True, $Failed
  ];

  data = WithRetries[Function[Null, gen], 200];
  If[data === $Failed, Message[Gen01::fail]; Return[$Failed]];

  A = data["A"]; b = data["b"];

  CellSection["S\[CHacek]\[IAcute]tavacia (elimina\[CHacek]n\[AAcute]) met\[OAcute]da"];
  CellSubsection["Zadanie"];
  CellExpr2[systemEquations[A, b, vars]];

  If[mode === "TASK",
    Return[<|"A" -> A, "b" -> b, "vars" -> vars|>]
  ];

  steps = Which[
    dim == 2 && st == "ONE",      stepsOne2[A, b, vars],
    dim == 2 && st == "NONE",     stepsNone2[A, b, vars],
    dim == 2 && st == "INFINITE", stepsInfinite2[A, b, vars]
  ];

  If[mode === "TASK_STEPS_RESULT",
    CellSubsection["Postup"];
    Scan[CellText2, steps["Text"]];
    Scan[
      If[StringQ[#], CellText2[#], CellExpr2[#]] &,
      steps["Expr"]
    ];
  ];

  CellSubsection["V\[YAcute]sledok"];
  sol = steps["Solution"];

  Which[
    sol === "NONE",
      CellExpr2[steps["ReasonEq"]];
      CellText2["Dan\[AAcute] s\[UAcute]stava nem\[AAcute] rie\[SHacek]enie, preto\[ZHacek]e pri elimin\[AAcute]cii vznikne rozpor."],

    sol === "INFINITE",
      CellExpr2[steps["ReasonEq"]];
      CellText2["Dan\[AAcute] s\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute], preto\[ZHacek]e pri elimin\[AAcute]cii vznikne identita 0 = 0 a zostane vo\:013en\[YAcute] parameter."],

    AssociationQ[sol],
      CellText2[
        "Rie\[SHacek]en\[IAcute]m s\[UAcute]stavy je " <>
        ToString[vars[[1]]] <> " = " <> ToString[sol[vars[[1]]]] <> ", " <>
        ToString[vars[[2]]] <> " = " <> ToString[sol[vars[[2]]]] <> "."
      ]
  ];

  If[vis && dim == 2, visualize2[A, b, vars]];

  Null
 ];

End[];
EndPackage[];
