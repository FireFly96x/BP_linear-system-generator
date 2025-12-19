(* ::Package:: *)

(*
  Balíček: EliminationMatrixGenerator
  Popis: Generátor príkladov na riešenie sústav lineárnych rovníc sčítacou (eliminačnou) metódou.
  Obsahuje logiku pre generovanie zadaní, krokov riešenia a vizualizáciu.
*)

BeginPackage["MojeGeneratory`EliminationMatrixGenerator`",
  {"MojeGeneratory`Common`"}
];

Internal`$ContextMarks = False;

(* Dokumentácia pre hlavnú funkciu Gen01 *)
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

(* Chybové hlášky *)
Gen01::baddiff  = "Neplatn\[AAcute] obtia\[ZHacek]nos\[THacek] `1`. Pou\[ZHacek]i \"EASY\"|\"MEDIUM\"|\"HARD\".";
Gen01::badmode  = "Neplatn\[YAcute] re\[ZHacek]im `1`. Pou\[ZHacek]i \"TASK\"|\"TASK_RESULT\"|\"TASK_STEPS_RESULT\".";
Gen01::notimpl  = "Obtia\[ZHacek]nos\[THacek] `1` zatia\:013e nie je implementovan\[AAcute] v tomto gener\[AAcute]tore.";
Gen01::fail     = "Nepodarilo sa vygenerova\[THacek] vhodn\[YAcute] pr\[IAcute]klad.";

Options[Gen01] = {
  SolutionType -> Automatic,
  Visualization -> False
};

Begin["`Private`"];

(* --- Pomocné funkcie pre výpis buniek (Cells) --- *)

CellBox[expr_] := CellPrint @ Cell[
  BoxData @ ToBoxes[expr, TraditionalForm],
  "DisplayFormula",
  ShowStringCharacters -> False
];

renderItem[item_] := Which[
  StringQ[item], CellText[item],
  (* Pridaná podpora pre Style[_String, ...] aby sa to vypísalo ako Text a nie ako Formula *)
  MatchQ[item, Style[_String, ___]], CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Text", ShowStringCharacters -> False],
  Head[item] === Graphics, CellPrint @ Cell[BoxData @ ToBoxes[item], "Graphics"],
  True, CellBox[item]
];

(* Zvýraznenie člena rovnice (napr. pri eliminácii) *)
highlightTerm[term_] := Style[term, Bold, RGBColor[0.8, 0, 0]];

(* Zvýraznenie mriežky/tabuľky s výsledkom *)
highlightGrid[grid_] := Style[grid, Background -> RGBColor[0.95, 0.95, 0.95], Frame -> True, FrameStyle -> None, FrameMargins -> 5];

(* Funkcia na zarovnanie rovníc do mriežky (Grid).
   Používa Map (/@) namiesto ReplaceAll (/.), aby sa predišlo chybám pri zoznamoch s dĺžkou 3.
*)
alignedEquations[data_] := Module[
  {
    eqSign = Style["=", 16],
    vbar   = Style["\[VerticalSeparator]", GrayLevel[.25]],
    stepRow
  },

  (* Formátovanie jedného riadku mriežky *)
  stepRow[{lhs_, rhs_, note_}] := {
    lhs,
    eqSign,
    rhs,
    If[note === "" || note === None,
      "",
      Style[Row[{vbar, Spacer[4], note}], GrayLevel[.6], FontSize -> 13]
    ]
  };
  (* Fallback pre riadok bez poznámky, ak by bol zadaný len s 2 prvkami *)
  stepRow[{lhs_, rhs_}] := stepRow[{lhs, rhs, ""}];

  Grid[
    stepRow /@ data,
    Alignment -> {{Right, Center, Left, Left}},
    Spacings -> {0.5, 0.6},
    BaseStyle -> {FontSize -> 14}
  ]
];

(* Grid s extra medzerou medzi skupinami riadkov (napr. po 2. riadku) *)
alignedEquationsGrouped[data_, breaks_List : {2}, gap_ : 1.25] := Module[
  {eqSign = Style["=", 16], vbar = Style["\[VerticalSeparator]", GrayLevel[.25]],
    stepRow, n, baseGap = 0.6, rowGaps},

  stepRow[{lhs_, rhs_, note_}] := {
    lhs,
    eqSign,
    rhs,
    If[note === "" || note === None,
      "",
      Style[Row[{vbar, Spacer[4], note}], GrayLevel[.6], FontSize -> 13]
    ]
  };
  stepRow[{lhs_, rhs_}] := stepRow[{lhs, rhs, ""}];

  n = Length[data];

  (* Spacings pre riadky: {top, medzi1, medzi2, ..., bottom} *)
  rowGaps = Join[
    {baseGap},
    Table[If[MemberQ[breaks, i], gap, baseGap], {i, 1, Max[0, n - 1]}],
    {baseGap}
  ];

  Grid[
    stepRow /@ data,
    Alignment -> {{Right, Center, Left, Left}},
    Spacings -> {0.5, rowGaps},
    BaseStyle -> {FontSize -> 14}
  ]

];

(* Formátovanie ľavej strany rovnice (lineárna kombinácia premenných) *)
formatLHS[cx_, cy_, choice_] := Module[{tX, tY, sign},
  tX = If[choice == "X", highlightTerm[cx x], cx x];

  If[cy == 0,
    TraditionalForm[tX],

    sign = If[cy < 0, " - ", " + "];
    tY = If[Abs[cy] == 1, y, Abs[cy] y];
    tY = If[choice == "Y", highlightTerm[tY], tY];

    Row[{TraditionalForm[tX], sign, TraditionalForm[tY]}]
  ]
];

(* Vytvorenie popisu legendy pre graf (rovnica priamky y = kx + q alebo x = c) *)
lineLegendText[a_, b_, c_] := Module[{m, q, fmt, mStr},
  fmt[t_] := ToString[TraditionalForm[Together[t]]];

  If[b == 0,
    "x = " <> fmt[c/a],
    m = Together[-a/b];
    q = Together[c/b];

    mStr = Which[
      m === 1,  "",
      m === -1, "-",
      True,      fmt[m]
    ];

    "y = " <> mStr <> "x" <>
        Which[
          q === 0, "",
          q > 0, " + " <> fmt[q],
          True,  " - " <> fmt[Abs[q]]
        ]
  ]
];


(* Formátovanie poznámky pre násobenie rovnice (napr. "· (-2)") *)
multiplyNoteString[m_] := Which[
  m == 1, "",
  m < 0, "\[CenterDot] (" <> ToString[m] <> ")",
  True, "\[CenterDot] " <> ToString[m]
];

(* --- Nastavenia generovania podľa obťažnosti --- *)

coeffRangeByDiff["EASY"] := 5;
coeffRangeByDiff[_] := 5;

boundByDiff["EASY"] := 60;
boundByDiff[_] := 90;

(* Generuje náhodný riadok koeficientov, vyhýba sa nulovému riadku *)
randomRow[n_, r_] := Module[{v},
  v = RandomInteger[{-r, r}, n];
  If[AllTrue[v, # == 0 &], randomRow[n, r], v]
];

(* 2D riadok pre EASY: tvar a x ± y (poradie je x,y = b) *)
randomRow2NoZeros["EASY", r_] := Module[{a, s},
  a = RandomInteger[{-r, r}];
  If[a == 0, Return[randomRow2NoZeros["EASY", r]]];
  s = RandomChoice[{-1, 1}];
  {a, s}
];

randomRow2NoZeros[_, r_] := Module[{v},
  v = RandomInteger[{-r, r}, 2];
  If[v[[1]] == 0 || v[[2]] == 0, randomRow2NoZeros["OTHER", r], v]
];


(* Kontroluje, či sú čísla v sústave "pekné" (nie príliš veľké) *)
numbersNiceQ[A_, b_, diff_] := Module[{bd = boundByDiff[diff]},
  Max[Abs @ Join[Flatten[A], Flatten[b]]] <= bd
];

(* --- Generátory sústav --- *)

(* Generuje sústavu s práve jedným riešením *)
generateSystemOne[dim_, diff_] := Module[{r, x0, A, b},
  r = coeffRangeByDiff[diff];
  x0 = RandomInteger[{-10, 10}, dim];
  A = If[dim == 2,
    Table[randomRow2NoZeros[diff, r], {2}]
    Table[randomRow[dim, r], {dim}]
  ];

  If[Det[A] == 0, Return[$Failed]];
  b = A . x0;
  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b, "x0" -> x0, "type" -> "ONE"|>
];

(* Generuje sústavu bez riešenia (rovnobežné priamky) *)
generateSystemNone2[diff_] := Module[{r, row1, row2, k, c1, c2, A, b},
  r = coeffRangeByDiff[diff];
  row1 = randomRow2NoZeros[diff, r];
  k = RandomChoice[{-3, -2, 2, 3}]; (* Násobok pre lineárnu závislosť *)
  row2 = k * row1;
  c1 = RandomInteger[{-10, 10}];
  c2 = k * c1 + RandomChoice[{-5, -3, 3, 5}]; (* Pravá strana nezodpovedá násobku -> spor *)
  A = {row1, row2};
  b = {c1, c2};
  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b, "type" -> "NONE"|>
];

(* Generuje sústavu s nekonečne veľa riešeniami (identické priamky) *)
generateSystemInfinite2[diff_] := Module[{r, row1, row2, k, c1, c2, A, b},
  r = coeffRangeByDiff[diff];
  row1 = randomRow2NoZeros[diff, r];
  k = RandomChoice[{-3, -2, 2, 3}];
  row2 = k * row1;
  c1 = RandomInteger[{-10, 10}];
  c2 = k * c1; (* Pravá strana zodpovedá násobku -> identita *)
  A = {row1, row2};
  b = {c1, c2};
  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b, "type" -> "INFINITE"|>
];

(* --- Analýza a logika eliminácie --- *)

(* Analyzuje stĺpec matice a navrhne koeficienty na elimináciu *)
analyzeVariableElimination[colIndex_, A_] := Module[
  {c1, c2, lcm, mul1, mul2, score},
  c1 = A[[1, colIndex]];
  c2 = A[[2, colIndex]];
  If[c1 == 0 || c2 == 0, Return[<|"Score" -> 9999|>]];
  lcm = LCM[Abs[c1], Abs[c2]];
  mul1 = lcm / Abs[c1];
  mul2 = lcm / Abs[c2];
  (* Skóre preferuje menšie LCM a prípady, kde netreba násobiť obe rovnice *)
  score = lcm + If[mul1 > 1 && mul2 > 1, 1000, 0];
  <|"Score" -> score, "LCM" -> lcm, "RawMul1" -> mul1, "RawMul2" -> mul2, "Coeffs" -> {c1, c2}|>
];

(* Začne eliminačný proces: vyberie premennú a vypočíta multiplikátory *)
eliminationStart[A_, b_, vars_] := Module[
  {content = {}, x, y, a1, b1, c1, a2, b2, c2,
    resX, resY, choice, targetVar, elimReason,
    rawM1, rawM2, m1, m2, k1, k2,
    c1x, c1y, c1rhs, c2x, c2y, c2rhs,
    rows1, rows2, needsMultiplication, preparedRows},

  x = vars[[1]]; y = vars[[2]];
  a1 = A[[1, 1]]; b1 = A[[1, 2]]; c1 = b[[1]];
  a2 = A[[2, 1]]; b2 = A[[2, 2]]; c2 = b[[2]];

  (* Porovnanie, či je lepšie eliminovať X alebo Y *)
  resX = analyzeVariableElimination[1, A];
  resY = analyzeVariableElimination[2, A];

  If[resY["Score"] < resX["Score"],
    choice = "Y"; targetVar = y;
    {k1, k2} = resY["Coeffs"];
    {rawM1, rawM2} = {resY["RawMul1"], resY["RawMul2"]};
    ,
    (* else: eliminujeme X *)
    choice = "X"; targetVar = x;
    {k1, k2} = resX["Coeffs"];
    {rawM1, rawM2} = {resX["RawMul1"], resX["RawMul2"]};
  ];

  AppendTo[content, Style["1. Pr\[IAcute]prava na elimin\[AAcute]ciu - vyru\[SHacek]enie jednej premennej", Bold]];

  (* Zistíme, či treba vôbec násobiť: ak sú koeficienty pri cieľovej premennej už opačné, netreba *)
  needsMultiplication = (Sign[k1] == Sign[k2]) || (rawM1 =!= 1) || (rawM2 =!= 1);

  If[needsMultiplication,
    AppendTo[content,
      "Aby sme vyru\[SHacek]ili premenn\[UAcute] " <> ToString[targetVar] <>
          ", uprav\[IAcute]me rovnice n\[AAcute]soben\[IAcute]m tak, aby mali pri tejto premennej rovnak\[YAcute] koeficient s opa\[CHacek]n\[YAcute]m znamienkom."
    ],
    AppendTo[content,
      "Koeficienty pri premennej " <> ToString[targetVar] <>
          " s\[UAcute] u\[ZHacek] opa\[CHacek]n\[EAcute], preto nemus\[IAcute]me ni\[CHacek] \[ZHacek]iadnym \[CHacek]\[IAcute]slom pren\[AAcute]sobova\[THacek]. M\[OHat]\[ZHacek]eme hne\[DHacek] prejs\[THacek] na s\[CHacek]\[IAcute]tanie rovnic a ozna\[CHacek]i\[THacek] si \[CHacek]leny, ktor\[EAcute] sa vyru\[SHacek]ia."
    ]
  ];

  (* Určenie znamienok multiplikátorov, aby došlo k odčítaniu *)
  If[Sign[k1] != Sign[k2],
    m1 = rawM1; m2 = rawM2;,
    If[c1 < 0, m1 = -rawM1; m2 = rawM2,
      If[c2 < 0, m1 = rawM1; m2 = -rawM2,
        m1 = rawM1; m2 = -rawM2]]
  ];

  rows1 = {
    {a1*x + b1*y, c1, multiplyNoteString[m1]},
    {a2*x + b2*y, c2, multiplyNoteString[m2]}
  };

  (* Aplikácia násobenia / príprava na elimináciu *)
  c1x = m1*a1; c1y = m1*b1; c1rhs = m1*c1;
  c2x = m2*a2; c2y = m2*b2; c2rhs = m2*c2;

  rows2 = {
    {formatLHS[c1x, c1y, choice], c1rhs, ""},
    {formatLHS[c2x, c2y, choice], c2rhs, ""}
  };

  If[needsMultiplication,
    (* Štandard: ukážeme 2+2 (pred a po) *)
    AppendTo[content, alignedEquationsGrouped[Join[rows1, rows2], {2}, 1]],
    (* Netreba násobiť: preskočíme "pred", rovno ukážeme pripravené rovnice so zvýraznením eliminovaných členov *)
    preparedRows = {
      {formatLHS[a1, b1, choice], c1, ""},
      {formatLHS[a2, b2, choice], c2, ""}
    };
    AppendTo[content, alignedEquations[preparedRows]]
  ];

  <|"content" -> content, "m1" -> m1, "m2" -> m2, "EliminatedVariable" -> choice, "failed" -> False|>
];

(* Generovanie krokov pre jedno riešenie *)
stepsOne2[A_, b_, vars_] := Module[
  {data, content, m1, m2, a1, b1, c1, a2, b2, c2, x, y,
    sumRHS, sumCoeffX, sumCoeffY, calcVar, calcVal, otherVar, otherVal,
    stepsY, stepsSub, valProduct, op, rhsRem, elimVarStr,
    explicitSubstLHS, termRemAbs, substCoeff, substConst, termResult},

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

  AppendTo[content, Style["2. S\[CHacek]\[IAcute]tanie rovn\[IAcute]c \[Dash] z\[IAcute]skame jednu premenn\[UAcute]", Bold]];
  AppendTo[content,
    "Teraz rovnice s\[CHacek]\[IAcute]tame. Premenn\[AAcute] " <> ToString[If[elimVarStr=="X", x, y]] <>
        " sa vyru\[SHacek]\[IAcute] (zmizne), preto\[ZHacek]e m\[AAcute] v oboch rovniciach opa\[CHacek]n\[EAcute] koeficienty."];

  stepsY = {};

  (* --- Explicitný krok: sčítanie rovníc ešte pred zjednodušením --- *)
  Module[{c1x2, c1y2, c1rhs2, c2x2, c2y2, c2rhs2, signSep, explicitLHS, explicitRHS},
    c1x2 = m1*a1;  c1y2 = m1*b1;  c1rhs2 = m1*c1;
    c2x2 = m2*a2;  c2y2 = m2*b2;  c2rhs2 = m2*c2;

    signSep[v_] := If[v < 0, " - ", " + "];

    explicitLHS = Row[{
      TraditionalForm[c1x2*x], signSep[c2x2], TraditionalForm[Abs[c2x2]*x],
      signSep[c1y2], TraditionalForm[Abs[c1y2]*y],
      signSep[c2y2], TraditionalForm[Abs[c2y2]*y]
    }];

    explicitRHS = Row[{c1rhs2, signSep[c2rhs2], Abs[c2rhs2]}];

    AppendTo[stepsY, {explicitLHS, explicitRHS, ""}];
  ];

  (* Výpočet premennej, ktorá ostala po eliminácii *)
  If[elimVarStr == "X",
    termResult = sumCoeffY*y;
    AppendTo[stepsY, {termResult, sumRHS, ""}];

    (* Vypiseme doterajsie kroky do gridu, vycistime buffer a vypiseme text ako text *)
    AppendTo[content, alignedEquations[stepsY]];
    stepsY = {};

    If[sumCoeffY == 1,
      AppendTo[content, "Po s\[CHacek]\[IAcute]tan\[IAcute] sme dostali jednoduch\[UAcute] rovnicu, z ktorej hne\[DHacek] ur\[CHacek]\[IAcute]me hodnotu premennej " <> ToString[y] <> "."];
      ,
      AppendTo[content, "Zostala n\[AAcute]m rovnica s jednou premennou. Uprav\[IAcute]me ju (napr. vydelen\[IAcute]m koeficientom), aby sme dostali samotn\[UAcute] premenn\[UAcute]."];
    ];

    If[sumCoeffY == 0, Return[$Failed]];
    calcVar = y; calcVal = sumRHS / sumCoeffY; otherVar = x;

    If[sumCoeffY =!= 1,
      AppendTo[stepsY, {sumCoeffY y, sumRHS, ": " <> ToString[sumCoeffY]}];
    ];
  ];

  If[elimVarStr == "Y",
    termResult = sumCoeffX*x;
    AppendTo[stepsY, {termResult, sumRHS, ""}];

    (* Vypiseme doterajsie kroky do gridu, vycistime buffer a vypiseme text ako text *)
    AppendTo[content, alignedEquations[stepsY]];
    stepsY = {};

    If[sumCoeffX == 1,
      AppendTo[content, "Po s\[CHacek]\[IAcute]tan\[IAcute] sme dostali jednoduch\[UAcute] rovnicu, z ktorej hne\[DHacek] ur\[CHacek]\[IAcute]me hodnotu premennej " <> ToString[x] <> "."];
      ,
      AppendTo[content, "Zostala n\[AAcute]m rovnica s jednou premennou. Uprav\[IAcute]me ju (napr. vydelen\[IAcute]m koeficientom), aby sme dostali samotn\[UAcute] premenn\[UAcute]."];
    ];

    If[sumCoeffX == 0, Return[$Failed]];
    calcVar = x; calcVal = sumRHS / sumCoeffX; otherVar = y;

    If[sumCoeffX =!= 1,
      AppendTo[stepsY, {sumCoeffX x, sumRHS, ": " <> ToString[sumCoeffX]}];
    ];
  ];

  If[Length[stepsY] > 0, AppendTo[content, alignedEquations[stepsY]]];
  AppendTo[content, highlightGrid[alignedEquations[{{calcVar, calcVal, ""}}]]];


  AppendTo[content, Style["3. Dosadenie \[Dash] vypo\[CHacek]\[IAcute]tame druh\[UAcute] premenn\[UAcute]", Bold]];
  AppendTo[content,
    "Vypo\[CHacek]\[IAcute]tan\[UAcute] hodnotu " <> ToString[calcVar] <>
        " dosad\[IAcute]me do jednej z p\[OAcute]vodn\[YAcute]ch rovn\[IAcute]c (napr. do prvej). Z\[IAcute]skame rovnicu len s druhou premennou a vyr\[AAcute]tame jej hodnotu."];

  stepsSub = {};

  AppendTo[stepsSub, {a1 x + b1 y, c1, Row[{calcVar, " = ", TraditionalForm[Together[calcVal]]}]}];

  (* Dosadenie vypočítanej hodnoty späť do rovnice *)
  If[elimVarStr == "X",
    substCoeff = a1; substConst = b1;

    explicitSubstLHS = Row[{
      If[substCoeff == 1, x, If[substCoeff == -1, -x, Row[{substCoeff, x}]]],
      If[substConst < 0, " - ", " + "],
      Abs[substConst], " \[CenterDot] ",
      If[calcVal < 0, Row[{"(", calcVal, ")"}], calcVal]
    }];

    valProduct = substConst * calcVal;

    calculatedSubstLHS = Row[{
      If[substCoeff == 1, x, If[substCoeff == -1, -x, Row[{substCoeff, x}]]],
      If[valProduct < 0, " - ", " + "],
      Abs[valProduct]
    }];
    ,
    (* else: eliminovali sme Y, teda počítali X, teraz dosádzame za X *)
    substCoeff = b1; substConst = a1;

    explicitSubstLHS = Row[{
      substConst, " \[CenterDot] ",
      If[calcVal < 0, Row[{"(", calcVal, ")"}], calcVal],
      If[substCoeff < 0, " - ", " + "],
      If[Abs[substCoeff] == 1, y, Row[{Abs[substCoeff], y}]]
    }];


    valProduct = substConst * calcVal;

    calculatedSubstLHS = Row[{
      valProduct,
      If[substCoeff < 0, " - ", " + "],
      If[Abs[substCoeff] == 1, y, Row[{Abs[substCoeff], y}]]
    }];
  ];

  AppendTo[stepsSub, {explicitSubstLHS, c1, ""}];

  op = If[valProduct > 0, "- " <> ToString[valProduct], "+ " <> ToString[Abs[valProduct]]];
  AppendTo[stepsSub, {calculatedSubstLHS, c1, op}];

  rhsRem = c1 - valProduct;

  If[substCoeff =!= 1,
    termUnknown = If[elimVarStr == "X", a1 x, b1 y];
    AppendTo[stepsSub, {termUnknown, rhsRem, ": " <> ToString[substCoeff]}];
  ];

  AppendTo[content, alignedEquations[stepsSub]];

  otherVal = rhsRem / substCoeff;
  AppendTo[content, highlightGrid[alignedEquations[{{otherVar, otherVal, ""}}]]];

  solPair = If[elimVarStr == "X", {otherVal, calcVal}, {calcVal, otherVal}];

  <|"Content" -> content, "Solution" -> solPair|>
];

(* Generovanie krokov pre prípad "žiadne riešenie" *)
stepsNone2[A_, b_, vars_] := Module[
  {data, content, m1, m2, a1, b1, c1, a2, b2, c2, x, y, sumRHS, stepsY},

  x = vars[[1]]; y = vars[[2]];
  a1 = A[[1, 1]]; b1 = A[[1, 2]]; c1 = b[[1]];
  a2 = A[[2, 1]]; b2 = A[[2, 2]]; c2 = b[[2]];

  data = eliminationStart[A, b, vars];
  If[data["failed"], Return[$Failed]];

  content = data["content"];
  m1 = data["m1"]; m2 = data["m2"];
  sumRHS = m1 c1 + m2 c2;

  AppendTo[content, Style["2. S\[CHacek]\[IAcute]tanie rovn\[IAcute]c \[Dash] kontrola, \[CHacek]i nevznikne spor", Bold]];
  AppendTo[content, "Rovnice s\[CHacek]\[IAcute]tame (po \[UAcute]prave z kroku 1). Sledujeme, \[CHacek]i nevznikne nemo\[ZHacek]n\[AAcute] rovnos\[THacek]."];

  stepsY = {};

  (* Explicit block *)
  Module[{c1x2, c1y2, c1rhs2, c2x2, c2y2, c2rhs2, signSep, explicitLHS, explicitRHS},
    c1x2 = m1*a1;  c1y2 = m1*b1;  c1rhs2 = m1*c1;
    c2x2 = m2*a2;  c2y2 = m2*b2;  c2rhs2 = m2*c2;

    signSep[v_] := If[v < 0, " - ", " + "];

    explicitLHS = Row[{
      TraditionalForm[c1x2*x], signSep[c2x2], TraditionalForm[Abs[c2x2]*x],
      signSep[c1y2], TraditionalForm[Abs[c1y2]*y],
      signSep[c2y2], TraditionalForm[Abs[c2y2]*y]
    }];

    explicitRHS = Row[{c1rhs2, signSep[c2rhs2], Abs[c2rhs2]}];

    AppendTo[stepsY, {explicitLHS, explicitRHS, ""}];
  ];

  (* Result block *)
  AppendTo[stepsY, {0, sumRHS, ""}];

  AppendTo[content, alignedEquations[stepsY]];

  AppendTo[content, Style["3. Z\[AAcute]ver", Bold]];
  AppendTo[content, "Po s\[CHacek]\[IAcute]tan\[IAcute] vy\[SHacek]la nepravdiv\[AAcute] rovnos\[THacek] (napr. 0 = nenulov\[EAcute] \[CHacek]\[IAcute]slo). To je spor, preto s\[UAcute]stava nem\[AAcute] rie\[SHacek]enie."];

  <|"Content" -> content, "Solution" -> "NONE"|>
];

(* Generovanie krokov pre prípad "nekonečne veľa riešení" *)
stepsInfinite2[A_, b_, vars_] := Module[
  {data, content, m1, m2, a1, b1, c1, a2, b2, c2, x, y, stepsY},

  x = vars[[1]]; y = vars[[2]];
  a1 = A[[1, 1]]; b1 = A[[1, 2]]; c1 = b[[1]];
  a2 = A[[2, 1]]; b2 = A[[2, 2]]; c2 = b[[2]];

  data = eliminationStart[A, b, vars];
  If[data["failed"], Return[$Failed]];

  content = data["content"];
  m1 = data["m1"]; m2 = data["m2"];

  AppendTo[content, Style["2. S\[CHacek]\[IAcute]tanie rovn\[IAcute]c \[Dash] over\[IAcute]me, \[CHacek]i s\[UAcute] rovnice toto\[ZHacek]n\[EAcute]", Bold]];
  AppendTo[content, "Rovnice s\[CHacek]\[IAcute]tame (po \[UAcute]prave z kroku 1). Ak vyjde 0 = 0, znamen\[AAcute] to, \[ZHacek]e sme dostali toto\[ZHacek]n\[UAcute] rovnicu."];

  stepsY = {};

  (* Explicit block *)
  Module[{c1x2, c1y2, c1rhs2, c2x2, c2y2, c2rhs2, signSep, explicitLHS, explicitRHS},
    c1x2 = m1*a1;  c1y2 = m1*b1;  c1rhs2 = m1*c1;
    c2x2 = m2*a2;  c2y2 = m2*b2;  c2rhs2 = m2*c2;

    signSep[v_] := If[v < 0, " - ", " + "];

    explicitLHS = Row[{
      TraditionalForm[c1x2*x], signSep[c2x2], TraditionalForm[Abs[c2x2]*x],
      signSep[c1y2], TraditionalForm[Abs[c1y2]*y],
      signSep[c2y2], TraditionalForm[Abs[c2y2]*y]
    }];

    explicitRHS = Row[{c1rhs2, signSep[c2rhs2], Abs[c2rhs2]}];

    AppendTo[stepsY, {explicitLHS, explicitRHS, ""}];
  ];

  (* Result block *)
  AppendTo[stepsY, {0, 0, ""}];

  AppendTo[content, alignedEquations[stepsY]];

  AppendTo[content, Style["3. Z\[AAcute]ver", Bold]];
  AppendTo[content, "Po s\[CHacek]\[IAcute]tan\[IAcute] vy\[SHacek]la pravdiv\[AAcute] rovnos\[THacek] 0 = 0. To znamen\[AAcute], \[ZHacek]e druh\[AAcute] rovnica je len n\[AAcute]sobkom prvej (opisuj\[UAcute] t\[UAcute] ist\[UAcute] priamku). S\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute]."];

  (* Odstránené redundantné parametrické vyjadrenie - rieši to Gen01 *)

  <|"Content" -> content, "Solution" -> "INFINITE"|>
];

(* Vizualizácia sústavy 2x2 *)
visualize2[A_, b_, vars_, sol_] := Module[
  { x, y, pt, subtitle, xrange, yrange, lineSeg, seg1, seg2, g, col1, col2, legend1, legend2, center, half, labelOffset},

  {x, y} = vars;

  half = 10;
  labelOffset = {0, 1.3};

  (* Určenie rozsahu a titulku podľa typu riešenia *)
  If[MatchQ[sol, {_?NumericQ, _?NumericQ}],
    pt = sol;
    center = 5 Round[pt/5];
    xrange = center[[1]] + {-half, half};
    yrange = center[[2]] + {-half, half};
    xrange = {Min[xrange[[1]], 0], Max[xrange[[2]], 0]};
    yrange = {Min[yrange[[1]], 0], Max[yrange[[2]], 0]};

    subtitle = Row[{
      "V grafe s\[UAcute] zobrazen\[EAcute] obe priamky. Ich priese\[CHacek]n\[IAcute]k je rie\[SHacek]en\[IAcute]m s\[UAcute]stavy a je vyzna\[CHacek]en\[YAcute] v bode ",
      "[", TraditionalForm[Together[pt[[1]]]], ", ",
      TraditionalForm[Together[pt[[2]]]], "]."
    }];,

    pt = None;
    center = {0, 0};
    xrange = {-10, 10};
    yrange = {-10, 10};

    subtitle = If[sol === "NONE",
      "Priamky s\[UAcute] rovnobe\[ZHacek]n\[EAcute], nepret\[IAcute]naj\[UAcute] sa \[Dash] s\[UAcute]stava nem\[AAcute] rie\[SHacek]enie.",
      "Priamky s\[UAcute] toto\[ZHacek]n\[EAcute] (prekr\[YAcute]vaj\[UAcute] sa) \[Dash] s\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute]."
    ];
  ];

  CellText[subtitle];

  lineSeg[{a_, bb_}, c_] := Module[{pA, pB},
    If[bb =!= 0,
      pA = {xrange[[1]], (c - a*xrange[[1]])/bb};
      pB = {xrange[[2]], (c - a*xrange[[2]])/bb};,
      pA = {c/a, yrange[[1]]};
      pB = {c/a, yrange[[2]]};
    ];
    Line[{pA, pB}]
  ];

  seg1 = lineSeg[A[[1]], b[[1]]];
  seg2 = lineSeg[A[[2]], b[[2]]];

  col1 = Magenta; col2 = Blue;
  legend1 = lineLegendText[A[[1, 1]], A[[1, 2]], b[[1]]];
  legend2 = lineLegendText[A[[2, 1]], A[[2, 2]], b[[2]]];

  g = Legended[
    Graphics[
      {
        If[sol === "INFINITE",
          {
            {col1, AbsoluteThickness[2], Opacity[0.95], seg1},
            {col2, AbsoluteThickness[2], Opacity[0.95], Dashing[{0.03, 0.03}], seg2}
          },
          {
            {col1, Thick, seg1},
            {col2, Thick, seg2}
          }
        ],

        If[pt =!= None,
          {
            {Black, Thick, Circle[pt, 0.4]},
            {Green, PointSize[0.02], Point[pt]},
            Inset[
              Style[
                Row[{"[", TraditionalForm[Together[pt[[1]]]], ", ",
                  TraditionalForm[Together[pt[[2]]]], "]"}],
                14
              ],
              pt + labelOffset,
              Background -> Directive[White, Opacity[0.8]],
              FrameMargins -> {{6, 6}, {3, 3}}
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
    Placed[
      LineLegend[{col1, col2}, {legend1, legend2}],
      After
    ]
  ];



  CellBox @ g
];

(* Hlavná funkcia generátora *)
Gen01[diff_String, mode_String, opts : OptionsPattern[]] :=
    Module[{dim, vars, st, gen, data, A, b, steps, sol},

      If[!MemberQ[{"EASY", "MEDIUM", "HARD"}, diff], Message[Gen01::baddiff, diff]; Return[$Failed]];
      If[!MemberQ[{"TASK", "TASK_RESULT", "TASK_STEPS_RESULT"}, mode], Message[Gen01::badmode, mode]; Return[$Failed]];
      If[diff =!= "EASY", Message[Gen01::notimpl, diff]; Return[$Failed]];

      st = ResolveSolutionType[OptionValue[SolutionType]];
      dim = DimensionByDifficulty["Elimination", diff];
      vars = Take[{x, y, z}, dim];

      gen := Which[
        dim == 2 && st == "ONE",      generateSystemOne[2, diff],
        dim == 2 && st == "NONE",     generateSystemNone2[diff],
        dim == 2 && st == "INFINITE", generateSystemInfinite2[diff],
        True, $Failed
      ];

      (* Pokus o vygenerovanie "pekného" zadania s opakovaním *)
      data = WithRetries[Function[Null, gen], 200];
      If[data === $Failed, Message[Gen01::fail]; Return[$Failed]];

      A = data["A"]; b = data["b"];

      CellSection["S\[CHacek]\[IAcute]tavacia (elimina\[CHacek]n\[AAcute]) met\[OAcute]da"];
      CellSubsection["Zadanie"];

      CellText["Vyrie\[SHacek]te nasleduj\[UAcute]cu s\[UAcute]stavu line\[AAcute]rnych rovn\[IAcute]c s\[CHacek]\[IAcute]tacou (elimina\[CHacek]nou) met\[OAcute]dou."];

      CellBox @ alignedEquations[Table[{A[[i]] . vars, b[[i]], ""}, {i, Length[b]}]];

      If[mode === "TASK",
        Return[<|"A" -> A, "b" -> b, "vars" -> vars|>]
      ];

      steps = Which[
        data["type"] == "ONE",      stepsOne2[A, b, vars],
        data["type"] == "NONE",     stepsNone2[A, b, vars],
        data["type"] == "INFINITE", stepsInfinite2[A, b, vars],
        True, $Failed
      ];

      If[steps === $Failed, Message[Gen01::fail]; Return[$Failed]];

      sol = steps["Solution"];

      If[mode === "TASK_STEPS_RESULT",
        CellSubsection["Postup"];
        Scan[renderItem, steps["Content"]];
      ];

      CellSubsection["V\[YAcute]sledok"];

      Switch[sol,
        "NONE",
        CellText["S\[UAcute]stava nem\[AAcute] rie\[SHacek]enie (pri s\[CHacek]\[IAcute]tan\[IAcute] vznikol spor)."];
        Null,
        "INFINITE",
        CellText["S\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute]. Rie\[SHacek]enia zap\[IAcute]\[SHacek]eme pomocou parametra (jednu premenn\[UAcute] si zvol\[IAcute]me vo\:013ene a druh\[UAcute] dopo\[CHacek]\[IAcute]tame)."];
        Module[{par, exprX, exprY, a1 = A[[1, 1]], b1 = A[[1, 2]], c1 = b[[1]], baseEq, solvedEq},  par = \[FormalT];

        CellText["Vyjadr\[IAcute]me jednu premenn\[UAcute] z jednej rovnice (napr. y vyjadr\[IAcute]me pomocou x)."];

        If[b1 =!= 0,
          (* vyjadríme y z a1 x + b1 y = c1 *)
          baseEq   = a1 x + b1 y;
          solvedEq = Simplify[(c1 - a1 x)/b1];

          CellBox @ alignedEquations[{{baseEq, c1, ""}}];
          CellBox @ alignedEquations[{{y, solvedEq, ""}}];

          CellText["Zvol\[IAcute]me parameter (vo\:013en\[AAcute] hodnota):"];
          CellBox @ Grid[
            {{x, "=", par, ",", par, "\[Element]", "\[DoubleStruckR]"}},
            Alignment -> {{Right, Center, Left, Center, Left, Left}},
            Spacings -> {0.6, 0.8}
          ];

          CellText["Dosad\[IAcute]me parameter a dostaneme tvar pre druh\[UAcute] premenn\[UAcute]:"];
          CellBox @ alignedEquations[{{y, Simplify[solvedEq /. x -> par], ""}}];
          ,
          (* b1 == 0 -> vyjadríme x z a1 x = c1 *)
          baseEq   = a1 x;
          solvedEq = Simplify[c1/a1];

          CellBox @ alignedEquations[{{baseEq, c1, ""}}];
          CellBox @ alignedEquations[{{x, solvedEq, ""}}];

          CellText["Zvol\[IAcute]me parameter (vo\:013en\[AAcute] hodnota):"];
          CellBox @ Grid[
            {{y, "=", par, ",", par, "\[Element]", "\[DoubleStruckR]"}},
            Alignment -> {{Right, Center, Left, Center, Left, Left}},
            Spacings -> {0.6, 0.8}
          ];
          CellText["V tomto pr\[IAcute]pade vy\[SHacek]lo x ako kon\[SHacek]tanta a premenn\[AAcute] y m\[OAcute]\[ZHacek]e by\[THacek] \:013eubovo\:013en\[AAcute] (parameter)."];
        ];

        If[b1 != 0,
          exprX = par;
          exprY = Simplify[(c1 - a1*par)/b1];,
          exprY = par;
          exprX = Simplify[c1/a1];
        ];


        (* Parametrické vyjadrenie *)
        CellBox @ Grid[
          {
            {x, "=", TraditionalForm[exprX]},
            {y, "=", TraditionalForm[exprY]}
          },
          Alignment -> {{Right, Center, Left}},
          Spacings -> {0.6, 0.8}
        ];


        (* Zápis množiny K *)
        CellPrint @ Cell[BoxData @ FormBox[
          RowBox[{
            StyleBox["K", FontSlant->"Italic"],
            "=",
            RowBox[{"{",
              RowBox[{
                RowBox[{"[",
                  RowBox[{
                    ToBoxes[exprX, TraditionalForm], ";", " ",
                    ToBoxes[exprY, TraditionalForm]
                  }],
                  "]"}],
                " ", "\[VerticalSeparator]", " ",
                RowBox[{
                  ToBoxes[par, TraditionalForm],
                  "\[Element]",
                  "\[DoubleStruckR]"
                }]
              }],
              "}"}]
          }],
          TraditionalForm],
          "DisplayFormula",
          BaseStyle -> {FontSize -> 14}
        ];
        ];,
        _,
        CellPrint @ Cell[
          BoxData @ ToBoxes[
            Row[{
              "Rie\[SHacek]en\[IAcute]m s\[UAcute]stavy rovn\[IAcute]c je usporiadan\[AAcute] dvojica \[CHacek]\[IAcute]sel "              Style[
                Row[{"[", TraditionalForm[Together[sol[[1]]]], ", ", TraditionalForm[Together[sol[[2]]]], "]"}],
                Bold
              ]
            }],
            TraditionalForm
          ],
          "Text",
          ShowStringCharacters -> False
        ];
      ];

      If[OptionValue[Visualization] && dim == 2,
        visualize2[A, b, vars, sol];
      ];

      Null
    ];

CellSection[str_String] := CellPrintStyle[str, "Section"];
CellSubsection[str_String] := CellPrintStyle[str, "Subsection"];

End[];
EndPackage[];