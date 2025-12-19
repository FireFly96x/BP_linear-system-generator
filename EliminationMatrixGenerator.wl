(* ::Package:: *)

(* Balík pre generovanie príkladov na sčítaciu (eliminačnú) metódu *)
BeginPackage["MojeGeneratory`EliminationMatrixGenerator`",
  {"MojeGeneratory`Common`"}
];

Internal`$ContextMarks = False;

(* Definícia usage správ pre verejné funkcie a chybové hlásenia *)
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
  Visualization -> True|False   (jeden graf s oboma priamkami; iba pre 2\[Times]2)
  SolutionType   -> Automatic|\"ONE\"|\"NONE\"|\"INFINITE\"
    - ak sa nezad\[AAcute] (Automatic): 80% \[SHacek]anca na pr\[AAcute]ve jedno rie\[SHacek]enie
    - \"ONE\"/\"NONE\"/\"INFINITE\" sl\[UAcute]\[ZHacek]i len na riadenie generovania; pou\[ZHacek]\[IAcute]vate\:013eovi sa nevypisuje.";

Gen01::baddiff  = "Neplatn\[AAcute] obtia\[ZHacek]nos\[THacek] `1`. Pou\[ZHacek]i \"EASY\"|\"MEDIUM\"|\"HARD\".";
Gen01::badmode  = "Neplatn\[YAcute] re\[ZHacek]im `1`. Pou\[ZHacek]i \"TASK\"|\"TASK_RESULT\"|\"TASK_STEPS_RESULT\".";
Gen01::notimpl  = "Obtia\[ZHacek]nos\[THacek] `1` zatia\:013e nie je implementovan\[AAcute] v tomto gener\[AAcute]tore.";
Gen01::fail     = "Nepodarilo sa vygenerova\[THacek] vhodn\[YAcute] pr\[IAcute]klad.";

(* Predvolené nastavenia pre hlavnú funkciu *)
Options[Gen01] = {
  SolutionType -> Automatic,
  Visualization -> False
};

Begin["`Private`"];

(* -------------------------------------------------------------------------- *)
(* POMOCNÉ FUNKCIE PRE FORMÁTOVANIE VÝSTUPU                   *)
(* -------------------------------------------------------------------------- *)

(* Zobrazí rovnicu v tvare 'LHS == RHS' bez toho, aby ju Mathematica vyhodnotila *)
holdEq[lhs_, rhs_] := HoldForm[lhs == rhs];

(* Skratka pre tradičné matematické formátovanie *)
eqTF[expr_] := TraditionalForm[expr];

(* Vytvorí riadok s rovnicou a poznámkou zarovnanou vpravo (napríklad inštrukcia pre úpravu) *)
eqWithNote[eq_, note_String] :=
    Grid[{{eqTF[eq], Style[note, "Text", GrayLevel[0.25], Italic, FontSize -> 14]}},
      Alignment -> {Left, Center}, Spacings -> {1, 0}];

(* Vizuálne zvýrazní člen rovnice (používa sa na označenie člena, ktorý sa bude eliminovať) *)
highlightTerm[term_] := Style[term, Bold, Background -> RGBColor[1, 0.9, 0.6]];

(* Formátuje celú sústavu rovníc do mriežky tak, aby boli zarovnané podľa znamienka rovnosti *)
systemColumn[A_, b_, vars_] :=
    Grid[
      Table[{
        TraditionalForm[A[[i]] . vars],
        "=",
        TraditionalForm[b[[i]]]
      }, {i, Length[b]}],
      Alignment -> {{Right, Center, Left}},
      Spacings -> {0.5, 0.8}
    ];

(* Generuje text legendy pre graf (rovnica priamky v tvare y = kx + q alebo x = c) *)
lineLegendText[a_, b_, c_] := Module[{m, q, fmt},
  fmt[t_] := ToString[TraditionalForm[Together[t]]];

  If[b == 0,
    "x = " <> fmt[c/a],
    m = Together[-a/b];
    q = Together[c/b];
    "y = " <> fmt[m] <> "x" <> If[q >= 0, " + " <> fmt[q], " - " <> fmt[Abs[q]]]
  ]
];

(* Vytvorí formátovanú poznámku o násobení rovnice (napr. "/ * (-1)") *)
multiplyNote[m_] := Which[
  m == 1, "",
  m == -1, Style["/ \[CenterDot] (-1)", "Text", GrayLevel[0.25], Italic, FontSize -> 14],
  True, Style["/ \[CenterDot] " <> ToString[m], "Text", GrayLevel[0.25], Italic, FontSize -> 14]
];

(* Zoradí vizuálne zobrazenie ľavej strany rovnice v poradí X a potom Y *)
(* Rieši problém, kedy Mathematica automaticky presúva štylované objekty na koniec výrazu *)
orderedLHS[termX_, coeffY_, varY_] := Row[{
  termX,
  If[coeffY < 0, " - ", " + "],
  If[Abs[coeffY] == 1, varY, Row[{Abs[coeffY], varY}]]
}];

(* -------------------------------------------------------------------------- *)
(* GENEROVANIE DÁT                                  *)
(* -------------------------------------------------------------------------- *)

(* Nastavenie rozsahov koeficientov podľa obtiažnosti *)
coeffRangeByDiff["EASY"] := 5;
coeffRangeByDiff[_] := 5;

(* Nastavenie maximálnej povolenej hodnoty v rovniciach *)
boundByDiff["EASY"] := 60;
boundByDiff[_] := 90;

(* Vygeneruje náhodný riadok matice (koeficienty rovnice), vyhýba sa nulovému riadku *)
randomRow[n_, r_] := Module[{v},
  v = RandomInteger[{-r, r}, n];
  If[AllTrue[v, # == 0 &], randomRow[n, r], v]
];

(* Kontroluje, či sú všetky čísla v sústave "pekné" (v rámci limitov) *)
numbersNiceQ[A_, b_, diff_] := Module[{bd = boundByDiff[diff]},
  Max[Abs @ Join[Flatten[A], Flatten[b]]] <= bd
];

(* Generátor: Sústava s práve jedným riešením *)
generateSystemOne[dim_, diff_] := Module[{r, x0, A, b},
  r = coeffRangeByDiff[diff];
  x0 = RandomInteger[{-10, 10}, dim];
  A = Table[randomRow[dim, r], {dim}];

  (* Pre vizuálnu elimináciu je lepšie, ak v 2D prípade nie sú nulové koeficienty pri X *)
  If[dim == 2 && (A[[1, 1]] == 0 || A[[2, 1]] == 0), Return[$Failed]];

  (* Determinant musí byť nenulový pre jedno riešenie *)
  If[Det[A] == 0, Return[$Failed]];
  b = A . x0;

  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b, "x0" -> x0, "type" -> "ONE"|>
];

(* Generátor: Sústava, ktorá nemá riešenie (rovnobežné priamky) *)
generateSystemNone2[diff_] := Module[{r, row1, row2, k, c1, c2, A, b},
  r = coeffRangeByDiff[diff];
  row1 = randomRow[2, r];

  (* Zabezpečíme nenulový koeficient pre prvý prvok *)
  If[row1[[1]] == 0, row1[[1]] = RandomChoice[{-r, -1, 1, r}]];

  k = RandomChoice[{-3, -2, 2, 3}];
  row2 = k * row1; (* Druhý riadok je násobkom prvého *)

  c1 = RandomInteger[{-10, 10}];
  c2 = k * c1 + RandomChoice[{-5, -3, 3, 5}]; (* Pravá strana nie je násobkom -> spor *)

  A = {row1, row2};
  b = {c1, c2};

  If[A[[1, 1]] == 0 || A[[2, 1]] == 0, Return[$Failed]];
  If[!numbersNiceQ[A, b, diff], Return[$Failed]];

  <|"A" -> A, "b" -> b, "type" -> "NONE"|>
];

(* Generátor: Sústava s nekonečným množstvom riešení (totožné priamky) *)
generateSystemInfinite2[diff_] := Module[{r, row1, row2, k, c1, c2, A, b},
  r = coeffRangeByDiff[diff];
  row1 = randomRow[2, r];

  If[row1[[1]] == 0, row1[[1]] = RandomChoice[{-r, -1, 1, r}]];

  k = RandomChoice[{-3, -2, 2, 3}];
  row2 = k * row1;

  c1 = RandomInteger[{-10, 10}];
  c2 = k * c1; (* Aj pravá strana je násobkom -> rovnaká informácia *)

  A = {row1, row2};
  b = {c1, c2};

  If[A[[1, 1]] == 0 || A[[2, 1]] == 0, Return[$Failed]];
  If[!numbersNiceQ[A, b, diff], Return[$Failed]];

  <|"A" -> A, "b" -> b, "type" -> "INFINITE"|>
];

(* -------------------------------------------------------------------------- *)
(* LOGIKA RIEŠENIA A KROKOVANIE                         *)
(* -------------------------------------------------------------------------- *)

(* Analyzuje stĺpec matice a určí skóre vhodnosti pre elimináciu tejto premennej *)
(* Skóre je založené na najmenšom spoločnom násobku (LCM) koeficientov *)
analyzeVariableElimination[colIndex_, A_] := Module[
  {c1, c2, lcm, mul1, mul2, score},
  c1 = A[[1, colIndex]];
  c2 = A[[2, colIndex]];

  (* Ak je niektorý koeficient nula, pre sčítaciu metódu to nie je ideálny príklad na demonštráciu *)
  If[c1 == 0 || c2 == 0, Return[<|"Score" -> 9999|>]];

  lcm = LCM[Abs[c1], Abs[c2]];
  mul1 = lcm / Abs[c1];
  mul2 = lcm / Abs[c2];

  (* Penalizácia ak musíme násobiť obe rovnice, preferencia menšieho LCM *)
  score = lcm + If[mul1 > 1 && mul2 > 1, 1000, 0];

  <|"Score" -> score, "LCM" -> lcm, "RawMul1" -> mul1, "RawMul2" -> mul2, "Coeffs" -> {c1, c2}|>
];

(* Pripraví úvodnú fázu riešenia: výber premennej a úpravu rovníc násobením *)
eliminationStart[A_, b_, vars_] := Module[
  {content = {}, x, y, a1, b1, c1, a2, b2, c2,
    resX, resY, choice, targetVar, elimReason,
    rawM1, rawM2, m1, m2, k1, k2,
    eq1Mod, eq2Mod, hlX, hlY, cXMod, cYMod},

  x = vars[[1]]; y = vars[[2]];
  a1 = A[[1, 1]]; b1 = A[[1, 2]]; c1 = b[[1]];
  a2 = A[[2, 1]]; b2 = A[[2, 2]]; c2 = b[[2]];

  (* 1. Analyzujeme obe premenné X a Y *)
  resX = analyzeVariableElimination[1, A];
  resY = analyzeVariableElimination[2, A];

  (* 2. Rozhodneme, ktorú premennú je ľahšie eliminovať *)
  If[resY["Score"] < resX["Score"],
    (* Eliminujeme Y *)
    choice = "Y";
    targetVar = y;
    {k1, k2} = resY["Coeffs"];
    {rawM1, rawM2} = {resY["RawMul1"], resY["RawMul2"]};
    If[rawM1 == 1 || rawM2 == 1,
      elimReason = "sta\[CHacek]\[IAcute] vyn\[AAcute]sobi\[THacek] len jednu rovnicu a \[CHacek]\[IAcute]sla ostan\[UAcute] mal\[EAcute].",
      elimReason = "koeficienty maj\[UAcute] men\[SHacek]\[IAcute] spolo\[CHacek]n\[YAcute] n\[AAcute]sobok."
    ];,
    (* Eliminujeme X (predvolené) *)
    choice = "X";
    targetVar = x;
    {k1, k2} = resX["Coeffs"];
    {rawM1, rawM2} = {resX["RawMul1"], resX["RawMul2"]};
    If[rawM1 == 1 || rawM2 == 1,
      elimReason = "sta\[CHacek]\[IAcute] vyn\[AAcute]sobi\[THacek] len jednu rovnicu.",
      elimReason = "je to v\[YAcute]hodnej\[SHacek]ie pre v\[YAcute]po\[CHacek]et."
    ];
  ];

  AppendTo[content,
    "Rozhodneme sa eliminova\[THacek] premenn\[UAcute] " <> ToString[targetVar] <> ", preto\[ZHacek]e " <> elimReason
  ];

  (* 3. Určenie znamienok násobiteľov *)
  (* Ak majú koeficienty rovnaké znamienka, jeden násobiteľ musí byť záporný *)

  If[Sign[k1] != Sign[k2],
    (* Opačné znamienka -> násobíme kladnými číslami *)
    m1 = rawM1;
    m2 = rawM2;,

    (* Rovnaké znamienka -> heuristika na výber rovnice pre negáciu *)
    If[c1 < 0,
      m1 = -rawM1; m2 = rawM2,
      If[c2 < 0,
        m1 = rawM1; m2 = -rawM2,
        m1 = rawM1; m2 = -rawM2
      ]
    ]
  ];

  (* Zobrazíme pôvodné rovnice s inštrukciou pre násobenie *)
  AppendTo[content, Grid[{
    {eqTF[holdEq[a1*x + b1*y, c1]], multiplyNote[m1]},
    {eqTF[holdEq[a2*x + b2*y, c2]], multiplyNote[m2]}
  }, Alignment -> {{Left, Left}, Baseline}, Spacings -> {1, 0.5}]];

  (* Vypočítame koeficienty po úprave *)
  cXMod = m1*a1; cYMod = m1*b1;
  term1 = If[choice == "X", orderedLHS[highlightTerm[cXMod*x], cYMod, y], orderedLHS[cXMod*x, cYMod, highlightTerm[y]]];

  (* Pripravíme zobrazenie upravených rovníc so zvýraznením eliminovaného člena *)
  If[choice == "X",
    eq1ModDisplay = holdEq[orderedLHS[highlightTerm[m1*a1*x], m1*b1, y], m1*c1];
    eq2ModDisplay = holdEq[orderedLHS[highlightTerm[m2*a2*x], m2*b2, y], m2*c2];,

    eq1ModDisplay = holdEq[orderedLHS[m1*a1*x, m1*b1, highlightTerm[y]], m1*c1];
    eq2ModDisplay = holdEq[orderedLHS[m2*a2*x, m2*b2, highlightTerm[y]], m2*c2];
  ];

  AppendTo[content, Column[{eqTF[eq1ModDisplay], eqTF[eq2ModDisplay]}, Spacings -> 0.5, Alignment -> Left]];
  AppendTo[content, "S\[CHacek]\[IAcute]tame upraven\[EAcute] rovnice. \[CapitalCHacek]leny s premennou " <> ToString[targetVar] <> " sa vyru\[SHacek]ia."];

  <|"content" -> content, "m1" -> m1, "m2" -> m2, "EliminatedVariable" -> choice, "failed" -> False|>
];

(* Generovanie krokov pre jedno riešenie (ONE) *)
stepsOne2[A_, b_, vars_] := Module[
  {data, content, m1, m2, a1, b1, c1, a2, b2, c2, x, y,
    sumRHS, sumCoeffX, sumCoeffY, calcVar, calcVal, otherVar, otherVal,
    stepsY, stepsSub, valProduct, lhsSimple, op, rhsRem, elimVarStr,
    explicitSubstLHS, calculatedSubstLHS, termRemAbs,
    substCoeff, substConst},

  x = vars[[1]]; y = vars[[2]];
  a1 = A[[1, 1]]; b1 = A[[1, 2]]; c1 = b[[1]];
  a2 = A[[2, 1]]; b2 = A[[2, 2]]; c2 = b[[2]];

  data = eliminationStart[A, b, vars];
  If[data["failed"], Return[$Failed]];

  content = data["content"];
  m1 = data["m1"]; m2 = data["m2"];
  elimVarStr = data["EliminatedVariable"];

  sumRHS = m1 c1 + m2 c2;
  sumCoeffX = m1 a1 + m2 a2;
  sumCoeffY = m1 b1 + m2 b2;

  (* 1. Fáza: Sčítanie rovníc a výpočet prvej premennej *)
  stepsY = {};

  If[elimVarStr == "X",
    (* Zostalo nám Y *)
    AppendTo[content, eqTF[holdEq[0*x + sumCoeffY*y, sumRHS]]];
    If[sumCoeffY == 0, Return[$Failed]];

    calcVar = y;
    calcVal = sumRHS / sumCoeffY;
    otherVar = x;

    If[sumCoeffY < 0,
      AppendTo[stepsY, eqWithNote[holdEq[sumCoeffY y, sumRHS], "/ \[CenterDot] (-1)"]];
      AppendTo[stepsY, eqWithNote[holdEq[-sumCoeffY y, -sumRHS], "/ : " <> ToString[-sumCoeffY]]];
      ,
      AppendTo[stepsY, eqWithNote[holdEq[sumCoeffY y, sumRHS], "/ : " <> ToString[sumCoeffY]]];
    ];
    AppendTo[stepsY, eqTF[holdEq[y, calcVal]]];
    ,

    (* Zostalo nám X *)
    AppendTo[content, eqTF[holdEq[sumCoeffX*x + 0*y, sumRHS]]];
    If[sumCoeffX == 0, Return[$Failed]];

    calcVar = x;
    calcVal = sumRHS / sumCoeffX;
    otherVar = y;

    If[sumCoeffX < 0,
      AppendTo[stepsY, eqWithNote[holdEq[sumCoeffX x, sumRHS], "/ \[CenterDot] (-1)"]];
      AppendTo[stepsY, eqWithNote[holdEq[-sumCoeffX x, -sumRHS], "/ : " <> ToString[-sumCoeffX]]];
      ,
      AppendTo[stepsY, eqWithNote[holdEq[sumCoeffX x, sumRHS], "/ : " <> ToString[sumCoeffX]]];
    ];
    AppendTo[stepsY, eqTF[holdEq[x, calcVal]]];
  ];

  AppendTo[content, Column[stepsY, Alignment -> Left, Spacings -> 0.5]];

  (* 2. Fáza: Spätná substitúcia do prvej rovnice *)
  AppendTo[content, "Dosad\[IAcute]me vypo\[CHacek]\[IAcute]tan\[UAcute] hodnotu " <> ToString[calcVar] <> " do prvej rovnice."];
  stepsSub = {};

  AppendTo[stepsSub, eqWithNote[holdEq[a1 x + b1 y, c1], "/ " <> ToString[calcVar] <> " = " <> ToString[calcVal]]];

  If[elimVarStr == "X",
    substCoeff = a1; (* Hľadáme X, poznáme Y *)
    substConst = b1;
    termRemAbs = If[Abs[a1] == 1, x, Row[{Abs[a1], x}]];

    explicitSubstLHS = Row[{
      substConst, "\[CenterDot]", If[calcVal < 0, Row[{"(", calcVal, ")"}], calcVal],
      If[substCoeff < 0, " - ", " + "],
      termRemAbs
    }];

    valProduct = substConst * calcVal;
    calculatedSubstLHS = Row[{
      valProduct,
      If[substCoeff < 0, " - ", " + "],
      termRemAbs
    }];
    ,

    substCoeff = b1; (* Hľadáme Y, poznáme X *)
    substConst = a1;
    termRemAbs = If[Abs[b1] == 1, y, Row[{Abs[b1], y}]];

    explicitSubstLHS = Row[{
      substConst, "\[CenterDot]", If[calcVal < 0, Row[{"(", calcVal, ")"}], calcVal],
      If[substCoeff < 0, " - ", " + "],
      termRemAbs
    }];

    valProduct = substConst * calcVal;
    calculatedSubstLHS = Row[{
      valProduct,
      If[substCoeff < 0, " - ", " + "],
      termRemAbs
    }];
  ];

  AppendTo[stepsSub, eqTF[holdEq[explicitSubstLHS, c1]]];

  op = If[valProduct > 0, "/ - " <> ToString[valProduct], "/ + " <> ToString[Abs[valProduct]]];
  AppendTo[stepsSub, eqWithNote[holdEq[calculatedSubstLHS, c1], op]];

  rhsRem = c1 - valProduct;

  If[substCoeff == 0, Return[$Failed]];

  (* Doriešenie druhej premennej *)
  If[Abs[substCoeff] =!= 1,
    termUnknown = If[elimVarStr == "X", a1 x, b1 y];
    AppendTo[stepsSub, eqWithNote[holdEq[termUnknown, rhsRem], "/ : " <> ToString[substCoeff]]];
  ];
  If[substCoeff == -1,
    termUnknown = If[elimVarStr == "X", -x, -y];
    AppendTo[stepsSub, eqWithNote[holdEq[termUnknown, rhsRem], "/ : (-1)"]];
  ];

  otherVal = rhsRem / substCoeff;
  AppendTo[stepsSub, eqTF[holdEq[otherVar, otherVal]]];

  AppendTo[content, Column[stepsSub, Alignment -> Left, Spacings -> 0.5]];

  solPair = If[elimVarStr == "X", {otherVal, calcVal}, {calcVal, otherVal}];

  <|"Content" -> content, "Solution" -> solPair|>
];

(* Generovanie krokov pre žiadne riešenie (NONE) *)
stepsNone2[A_, b_, vars_] := Module[
  {data, content, m1, m2, b1, b2, c1, c2, sumRHS, coeffY, x, y},

  x = vars[[1]]; y = vars[[2]];

  data = eliminationStart[A, b, vars];
  If[data["failed"], Return[$Failed]];

  content = data["content"];
  m1 = data["m1"]; m2 = data["m2"];
  b1 = A[[1, 2]]; b2 = A[[2, 2]];
  c1 = b[[1]]; c2 = b[[2]];

  sumRHS = m1 c1 + m2 c2;

  (* Vizuálne ukážeme, že premenné vypadli: 0x + 0y = číslo *)
  AppendTo[content, eqTF[holdEq[0*x + 0*y, sumRHS]]];
  AppendTo[content, eqTF[holdEq[0, sumRHS]]];

  AppendTo[content, "Dostali sme rovnos\[THacek] 0 = " <> ToString[sumRHS] <> ", \[CHacek]o neplat\[IAcute]."];
  AppendTo[content, "Preto\[ZHacek]e sme dostali nepravdiv\[YAcute] v\[YAcute]rok (spor), s\[UAcute]stava nem\[AAcute] \[ZHacek]iadne rie\[SHacek]enie."];

  <|"Content" -> content, "Solution" -> "NONE"|>
];

(* Generovanie krokov pre nekonečne veľa riešení (INFINITE) *)
stepsInfinite2[A_, b_, vars_] := Module[
  {data, content, m1, m2, b1, b2, c1, c2, sumRHS, coeffY, x, y, a1, solSet},

  x = vars[[1]]; y = vars[[2]];
  a1 = A[[1, 1]]; b1 = A[[1, 2]]; c1 = b[[1]];

  data = eliminationStart[A, b, vars];
  If[data["failed"], Return[$Failed]];

  content = data["content"];
  m1 = data["m1"]; m2 = data["m2"];
  b1 = A[[1, 2]]; b2 = A[[2, 2]];
  c1 = b[[1]]; c2 = b[[2]];

  sumRHS = m1 c1 + m2 c2; (* Bude 0 *)

  (* Vizuálne ukážeme identitu: 0x + 0y = 0 *)
  AppendTo[content, eqTF[holdEq[0*x + 0*y, sumRHS]]];
  AppendTo[content, eqTF[holdEq[0, 0]]];

  AppendTo[content, "Dostali sme pravdiv\[UAcute] rovnos\[THacek] 0 = 0. S\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute]."];
  AppendTo[content, "Vyjadrenie rie\[SHacek]enia pomocou parametra p:"];

  (* Vypíšeme parametrické riešenie *)
  AppendTo[
    content,
    Column[
      {
        eqTF[holdEq[x, (c1 - b1*y)/a1]],
        eqTF[holdEq[y, y]]
      },
      Alignment -> Left,
      Spacings -> 0.5
    ]
  ];

  <|"Content" -> content, "Solution" -> "INFINITE"|>
];

(* -------------------------------------------------------------------------- *)
(* VIZUALIZÁCIA                                  *)
(* -------------------------------------------------------------------------- *)

visualize2[A_, b_, vars_, sol_] := Module[
  { x, y, pt, subtitle, xrange, yrange, lineSeg, seg1, seg2, g, col1, col2, legend1, legend2, center, half},

  {x, y} = vars;
  half = 10;

  (* Nastavenie stredu grafu podľa riešenia *)
  If[MatchQ[sol, {_?NumericQ, _?NumericQ}],
    pt = sol;
    center = 5 Round[pt/5];
    xrange = center[[1]] + {-half, half};
    yrange = center[[2]] + {-half, half};

    (* Zabezpečenie, aby osi boli viditeľné *)
    xrange = {Min[xrange[[1]], 0], Max[xrange[[2]], 0]};
    yrange = {Min[yrange[[1]], 0], Max[yrange[[2]], 0]};

    subtitle = Row[{
      "Na grafe s\[UAcute] zn\[AAcute]zornen\[EAcute] obe priamky. Ich priese\[CHacek]n\[IAcute]k je vyzna\[CHacek]en\[YAcute] kru\[ZHacek]nicou a zodpoved\[AAcute] rie\[SHacek]eniu s\[UAcute]stavy [",
      TraditionalForm[Together[pt[[1]]]], ", ",
      TraditionalForm[Together[pt[[2]]]], "]."
    }];
    ,
    (* Fallback pre prípady bez jedného riešenia *)
    pt = None;
    center = {0, 0};
    xrange = {-10, 10};
    yrange = {-10, 10};

    subtitle = If[sol === "NONE",
      "Priamky s\[UAcute] rovnobe\[ZHacek]n\[EAcute] a nemaj\[UAcute] spolo\[CHacek]n\[YAcute] bod (s\[UAcute]stava nem\[AAcute] rie\[SHacek]enie).",
      "Priamky s\[UAcute] toto\[ZHacek]n\[EAcute] (s\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute])."
    ];
  ];

  CellText[subtitle];

  (* Pomocná funkcia na vytvorenie úsečky v rámci hraníc grafu *)
  lineSeg[{a_, bb_}, c_] := Module[{pA, pB},
    If[bb =!= 0,
      (* y = (c - ax)/b *)
      pA = {xrange[[1]], (c - a*xrange[[1]])/bb};
      pB = {xrange[[2]], (c - a*xrange[[2]])/bb};,
      (* vertikálna čiara x = c/a *)
      pA = {c/a, yrange[[1]]};
      pB = {c/a, yrange[[2]]};
    ];
    Line[{pA, pB}]
  ];

  seg1 = lineSeg[A[[1]], b[[1]]];
  seg2 = lineSeg[A[[2]], b[[2]]];

  (* Výpočet pre pekné umiestnenie štítku s súradnicami *)
  v1 = Normalize[{A[[1, 2]], -A[[1, 1]]}];
  v2 = Normalize[{A[[2, 2]], -A[[2, 1]]}];
  bisector = If[v1 . v2 < 0, Normalize[v1 + v2], Normalize[v1 - v2]];
  labelOffset = 1.2 bisector;

  col1 = Magenta;
  col2 = Blue;

  legend1 = lineLegendText[A[[1, 1]], A[[1, 2]], b[[1]]];
  legend2 = lineLegendText[A[[2, 1]], A[[2, 2]], b[[2]]];

  g = Legended[
    Graphics[
      {
        {col1, Thick, seg1},
        {col2, Thick, seg2},

        If[pt =!= None,
          {
            {Black, Thick, Circle[pt, 0.4]},
            {Green, PointSize[0.02], Point[pt]},
            Text[
              Style[
                Row[{"[", TraditionalForm[Together[pt[[1]]]], ", ",
                  TraditionalForm[Together[pt[[2]]]], "]"}],
                14
              ],
              pt + labelOffset
            ]
          },
          {}
        ]
      },
      PlotRange -> {xrange, yrange},
      PlotRangeClipping -> True,
      GridLines -> Automatic,
      Axes -> True,
      ImageSize -> Medium,
      Method -> {
        "CoordinatesToolOptions" -> {
          "DisplayFunction" -> (Row[{"x=", NumberForm[#[[1]], {Infinity, 2}], ", y=", NumberForm[#[[2]], {Infinity, 2}]}] &),
          "CopiedValueFunction" -> (#[[1 ;; 2]] &)
        }
      }
    ],
    Placed[LineLegend[{col1, col2}, {legend1, legend2}], After]
  ];

  CellExpr @ g
];

(* -------------------------------------------------------------------------- *)
(* HLAVNÁ FUNKCIA                                 *)
(* -------------------------------------------------------------------------- *)

Gen01[diff_String, mode_String, opts : OptionsPattern[]] :=
    Module[{dim, vars, st, gen, data, A, b, steps, sol},

      (* Validácia vstupov *)
      If[!MemberQ[{"EASY", "MEDIUM", "HARD"}, diff], Message[Gen01::baddiff, diff]; Return[$Failed]];
      If[!MemberQ[{"TASK", "TASK_RESULT", "TASK_STEPS_RESULT"}, mode], Message[Gen01::badmode, mode]; Return[$Failed]];
      If[diff =!= "EASY", Message[Gen01::notimpl, diff]; Return[$Failed]];

      st = ResolveSolutionType[OptionValue[SolutionType]];
      dim = DimensionByDifficulty["Elimination", diff];
      vars = Take[{x, y, z}, dim];

      (* Výber vhodného generátora podľa typu riešenia *)
      gen := Which[
        dim == 2 && st == "ONE",      generateSystemOne[2, diff],
        dim == 2 && st == "NONE",     generateSystemNone2[diff],
        dim == 2 && st == "INFINITE", generateSystemInfinite2[diff],
        True, $Failed
      ];

      (* Pokus o generovanie s opakovaním pri neúspechu *)
      data = WithRetries[Function[Null, gen], 200];
      If[data === $Failed, Message[Gen01::fail]; Return[$Failed]];

      A = data["A"]; b = data["b"];

      (* Výpis hlavičky a zadania *)
      CellSection["S\[CHacek]\[IAcute]tavacia (elimina\[CHacek]n\[AAcute]) met\[OAcute]da"];
      CellSubsection["Zadanie"];
      CellText["Vyrie\[SHacek]te nasleduj\[UAcute]cu s\[UAcute]stavu line\[AAcute]rnych rovn\[IAcute]c pomocou s\[CHacek]\[IAcute]tavacej (elimina\[CHacek]nej) met\[OAcute]dy."];
      CellExpr @ systemColumn[A, b, vars];

      (* Ak chce užívateľ iba zadanie, končíme tu *)
      If[mode === "TASK",
        Return[<|"A" -> A, "b" -> b, "vars" -> vars|>]
      ];

      (* Generovanie krokov riešenia *)
      steps = Which[
        data["type"] == "ONE",      stepsOne2[A, b, vars],
        data["type"] == "NONE",     stepsNone2[A, b, vars],
        data["type"] == "INFINITE", stepsInfinite2[A, b, vars],
        True, $Failed
      ];

      If[steps === $Failed, Message[Gen01::fail]; Return[$Failed]];
      sol = steps["Solution"];

      (* Výpis postupu *)
      If[mode === "TASK_STEPS_RESULT",
        CellSubsection["Postup"];
        Scan[
          Which[
            StringQ[#], CellText[#],
            Head[#] === Graphics, CellPrint[Cell[BoxData[ToBoxes[#]], "Graphics"]],
            True, CellExpr[#]
          ] &,
          steps["Content"]
        ];
      ];

      (* Výpis výsledku *)
      CellSubsection["V\[YAcute]sledok"];

      Switch[sol,
        "NONE",
        CellText["S\[UAcute]stava nem\[AAcute] rie\[SHacek]enie."];
        CellExpr @ eqTF[False],
        "INFINITE",
        CellText["S\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute]."];,
        _,
        CellText["Rie\[SHacek]en\[IAcute]m s\[UAcute]stavy je:"];
        CellExpr @ Column[{eqTF[x == sol[[1]]], eqTF[y == sol[[2]]]}, Alignment -> Left];
      ];

      (* Voliteľná vizualizácia *)
      If[OptionValue[Visualization] && dim == 2,
        visualize2[A, b, vars, sol];
      ];

      Null
    ];

(* Pomocné funkcie pre tlač do buniek notebooku *)
CellSection[str_String] := CellPrintStyle[str, "Section"];
CellSubsection[str_String] := CellPrintStyle[str, "Subsection"];

End[];
EndPackage[];