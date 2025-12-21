(* ::Package:: *)

(*
  Package: EliminationMatrixGenerator
  Description: Generates didactic materials for solving linear systems via the elimination method.
  Refactored to minimize code duplication while preserving all pedagogical steps.
  Updated: Dynamic step numbering added.
*)

BeginPackage["MojeGeneratory`EliminationMatrixGenerator`",
  {"MojeGeneratory`Common`"}
];
$CharacterEncoding = "UTF-8";
Internal`$ContextMarks = False;

(* Public interface *)

Gen01::usage = "Gen01[diff, mode, opts] vygeneruje príklad riešenia sústavy lineárnych rovníc eliminačnou metódou (sčítaním/odčítaním rovníc).

diff: \"EASY\" (2x2), \"MEDIUM\" (3x3), \"HARD\" (3x3 - not impl)
mode: \"TASK\", \"TASK_RESULT\", \"TASK_STEPS_RESULT\"
opts: Visualization -> True|False, SolutionType -> Automatic|\"ONE\"|\"NONE\"|\"INFINITE\"";

(* Error messages *)
Gen01::baddiff  = "Neplatná obtiažnosť `1`. Použi \"EASY\"|\"MEDIUM\"|\"HARD\".";
Gen01::badmode  = "Neplatný režim `1`. Použi \"TASK\"|\"TASK_RESULT\"|\"TASK_STEPS_RESULT\".";
Gen01::notimpl  = "Obtiažnosť `1` zatiaľ nie je implementovaná.";
Gen01::fail     = "Nepodarilo sa vygenerovať vhodný príklad.";

Options[Gen01] = {SolutionType -> Automatic, Visualization -> False};

Begin["`Private`"];

(* Step numbering for visualization *)
stepsCounter = 0;
stepHeader[text_String] := (stepsCounter++; Style[ToString[stepsCounter] <> ". " <> text, Bold]);

(* --- 1. FORMATTING & HELPERS --- *)

CellSection[str_] := CellPrintStyle[str, "Section"];
CellSubsection[str_] := CellPrintStyle[str, "Subsection"];
CellTextU[str_] := CellPrint @ Cell[str, "Text", ShowStringCharacters -> False];
CellTextExpr[expr_] := CellPrint @ Cell[BoxData @ ToBoxes[expr, StandardForm], "Text", ShowStringCharacters -> False];
CellBox[expr_] := CellPrint @ Cell[BoxData @ ToBoxes[expr, TraditionalForm], "DisplayFormula", ShowStringCharacters -> False];

renderItem[item_] := Which[
  StringQ[item], CellTextU[item],
  MatchQ[item, Style[_String, ___]], CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Text", ShowStringCharacters -> False],
  Head[item] === Graphics || Head[item] === Graphics3D, CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Graphics"],
  True, CellBox[item]
];

highlightTerm[term_] := Style[term, Bold, RGBColor[0.8, 0, 0]];
highlightGrid[grid_] := Style[grid, Background -> RGBColor[0.95, 0.95, 0.95], Frame -> True, FrameStyle -> None, FrameMargins -> 5];

(* Grid Formatters *)
alignedEquations[data_] := Module[{eqSign = Style["=", 16], vbar = Style["|", GrayLevel[.25]], stepRow},
  stepRow[{lhs_, rhs_, note_}] := {lhs, eqSign, rhs, If[note === "" || note === None, "", Style[Row[{vbar, Spacer[4], note}], GrayLevel[.6], FontSize -> 13]]};
  stepRow[{lhs_, rhs_}] := stepRow[{lhs, rhs, ""}];
  Grid[stepRow /@ data, Alignment -> {{Right, Center, Left, Left}}, Spacings -> {0.5, 0.6}, BaseStyle -> {FontSize -> 14}]
];

alignedEquationsGrouped[data_, breaks_List : {2}, gap_ : 1.25] := Module[{baseGap = 0.6, rowGaps, n = Length[data]},
  rowGaps = Join[{baseGap}, Table[If[MemberQ[breaks, i], gap, baseGap], {i, 1, Max[0, n - 1]}], {baseGap}];
  Grid[
    (Function[{row}, {row[[1]], Style["=", 16], row[[2]], If[Length[row] > 2 && row[[3]] =!= "", Style[Row[{Style["|", GrayLevel[.25]], Spacer[4], row[[3]]}], GrayLevel[.6], FontSize -> 13], ""]}] /@ data),
    Alignment -> {{Right, Center, Left, Left}}, Spacings -> {0.5, rowGaps}, BaseStyle -> {FontSize -> 14}
  ]
];

(* LHS Formatting *)
formatLHS[cx_, cy_, choice_, vars2_] := Module[{x, y, tX, tY, sign},
  {x, y} = vars2;
  tX = If[choice == "X", highlightTerm[cx x], cx x];
  If[cy == 0, TraditionalForm[tX],
    sign = If[cy < 0, " - ", " + "];
    tY = If[choice == "Y", highlightTerm[If[Abs[cy] == 1, y, Abs[cy] y]], If[Abs[cy] == 1, y, Abs[cy] y]];
    Row[{TraditionalForm[tX], sign, TraditionalForm[tY]}]
  ]
];

formatLHS3[cx_, cy_, cz_, choice_, vars3_:{x,y,z}] := Module[{terms = {}, addTerm, x, y, z},
  {x, y, z} = vars3;
  addTerm[c_, v_, ch_] := If[c != 0,
    Module[{t, s},
      s = If[Length[terms] > 0, If[c < 0, " - ", " + "], If[c < 0, "-", ""]];
      t = If[Abs[c] == 1, v, Abs[c] v];
      If[ch == ToString[v] || (ch == "X" && v===x) || (ch == "Y" && v===y) || (ch == "Z" && v===z), t = highlightTerm[t]];
      AppendTo[terms, s]; AppendTo[terms, TraditionalForm[t]];
    ]];
  addTerm[cx, x, choice]; addTerm[cy, y, choice]; addTerm[cz, z, choice];
  If[Length[terms] == 0, TraditionalForm[0], Row[terms]]
];

(* Notes and Substitutions *)
multiplyNote[m_] := Which[
  m == 1, "",
  m < 0, Row[{"\[CenterDot] (", m, ")"}],
  True,  Row[{"\[CenterDot] ", m}]
];

multiplyNoteRow[m_] := Module[{mBox},
  mBox = If[m < 0, Row[{"(", m, ")"}], m];
  Row[{"\[CenterDot] ", mBox}]
];

substNote[solMap_, remVars_, row_, vars_] := Module[{usedVars},
  usedVars = Select[remVars, row[[First@First@Position[vars, #]]] =!= 0 &];
  If[usedVars === {}, "", Row[Riffle[(Row[{#, " \[Rule] ", tf[solMap[#]]}] & /@ usedVars), ", "]]]
];

tf[val_] := TraditionalForm[Together[val]];

parenIfNeg[val_] := If[NumericQ[val] && val < 0,
  Row[{"(", tf[val], ")"}],
  tf[val]
];

signStr[c_] := If[c < 0, " - ", " + "];

checkTermsRow[row_, sol_] := Module[{n = Length[row], first = True, out = {}},
  Do[
    If[row[[j]] === 0, Continue[]];
    If[first,
      out = Join[out,
        If[row[[j]] < 0,
          {"-", TraditionalForm[Abs[row[[j]]]], "\[CenterDot]", parenIfNeg[sol[[j]]]},
          {TraditionalForm[row[[j]]], "\[CenterDot]", parenIfNeg[sol[[j]]]}
        ]
      ];
      first = False;,
      out = Join[out, {signStr[row[[j]]], TraditionalForm[Abs[row[[j]]]], "\[CenterDot]", parenIfNeg[sol[[j]]]}];
    ];
    , {j, 1, n}];
  If[out === {}, TraditionalForm[0], Row[out]]
];

appendCorrectnessCheck[content_, A_, b_, vars_, sol_] := Module[
  {c = content, n = Length[vars], solN, row, idx, prods, lhs, prodRow},

  solN = Together /@ sol;

  c = Append[c, stepHeader["Skúška správnosti"]];
  c = Append[c, "Skúšku správnosti robíme dosadením vypočítaných hodnôt neznámych do všetkých rovníc:"];

  Do[
    row = A[[i]];
    lhs = Together[row.solN];

    prodRow = Module[{out = {}, first = True, p},
      Do[
        If[row[[j]] === 0, Continue[]];
        p = Together[row[[j]]*solN[[j]]];
        If[first,
          out = Join[out, {TraditionalForm[p]}]; first = False;,
          out = Join[out, {signStr[row[[j]]], TraditionalForm[Abs[p]]}]
        ];
        , {j, 1, n}];
      If[out === {}, TraditionalForm[0], Row[out]]
    ];

    c = Join[c, {
      Row[{"ĽS" <> ToString[i] <> " = ", checkTermsRow[row, solN], " = ", prodRow, " = ", TraditionalForm[lhs]}],
      Row[{"PS" <> ToString[i] <> " = ", TraditionalForm[b[[i]]]}],
      "ĽS" <> ToString[i] <> " = PS" <> ToString[i]
    }];
    , {i, 1, n}];

  c
];

coeffTimesValue[coeff_, val_] := Which[coeff === 0, 0, coeff === 1, parenIfNeg[val], coeff === -1, Row[{"-", parenIfNeg[val]}], True, Row[{coeff, " \[CenterDot] ", parenIfNeg[val]}]];

(* Substitution LHS formatters *)
formatSubstLHS3Generic[row_, vars_, solMap_, unknownVar_, evalMode_] := Module[{terms = {}, first = True, addTerm},
  addTerm[content_, sign_] := (AppendTo[terms, If[first, If[sign === -1, Row[{"-", content}], content], Row[{If[sign === -1, " - ", " + "], content}]]]; first = False;);

  Do[
    With[{c = row[[i]], v = vars[[i]]},
      If[c =!= 0,
        If[v === unknownVar,
          addTerm[TraditionalForm[If[Abs[c] === 1, v, Abs[c] v]], Sign[c]],
          If[evalMode,
            With[{prod = Together[c * solMap[v]]},
              If[!PossibleZeroQ[prod], addTerm[TraditionalForm[Abs[prod]], Sign[prod]]]],
            addTerm[coeffTimesValue[Abs[c], solMap[v]], Sign[c]]
          ]
        ]
      ]
    ], {i, 1, Length[vars]}
  ];
  If[Length[terms] == 0, TraditionalForm[0], Row[terms]]
];

formatSubstLHS3[row_, vars_, solMap_, uVar_] := formatSubstLHS3Generic[row, vars, solMap, uVar, False];
formatSubstLHS3Eval[row_, vars_, solMap_, uVar_] := formatSubstLHS3Generic[row, vars, solMap, uVar, True];


(* --- 2. GENERATION LOGIC --- *)

coeffRangeByDiff["EASY"] := 5;
coeffRangeByDiff[_] := 5;
boundByDiff["EASY"] := 60;
boundByDiff[_] := 90;

randomRow[n_, r_, allowZero_:True] := Module[{v},
  v = RandomInteger[{-r, r}, n];
  If[AllTrue[v, # == 0 &] || (!allowZero && AnyTrue[v, #==0&]), randomRow[n, r, allowZero], v]
];

numbersNiceQ[A_, b_, diff_] := Max[Abs @ Join[Flatten[A], Flatten[b]]] <= boundByDiff[diff];

(* Unified Singular System Generator (None/Infinite for 2D/3D) *)
generateSingularSystem[dim_, diff_, type_] := Module[
  {r = coeffRangeByDiff[diff], A, b, row1, row2, row3, k1, k2, c1, c2, c3, contradiction},
  contradiction = If[type == "NONE", RandomChoice[{-5, -3, 3, 5}], 0];

  If[dim == 2,
    row1 = randomRow[2, r, False];
    k1 = RandomChoice[{-3, -2, 2, 3}];
    row2 = k1 * row1;
    c1 = RandomInteger[{-10, 10}];
    c2 = k1 * c1 + contradiction;
    A = {row1, row2}; b = {c1, c2},

    (* dim 3 *)
    row1 = randomRow[3, r];
    row2 = randomRow[3, r];
    If[MatrixRank[{row1, row2}] < 2, Return[generateSingularSystem[dim, diff, type]]];

    k1 = RandomChoice[{-2, -1, 1, 2}];
    k2 = RandomChoice[{-2, -1, 1, 2}];
    row3 = k1 * row1 + k2 * row2;

    c1 = RandomInteger[{-5, 5}];
    c2 = RandomInteger[{-5, 5}];
    c3 = k1 * c1 + k2 * c2 + contradiction;
    A = {row1, row2, row3}; b = {c1, c2, c3}
  ];

  If[!numbersNiceQ[A, b, diff] || (dim==3 && Count[Flatten[A], 0] > 1), Return[$Failed]];
  <|"A" -> A, "b" -> b, "type" -> type|>
];

generateSystemOne[dim_, diff_] := Module[{r = coeffRangeByDiff[diff], x0, A, b},
  x0 = RandomInteger[{-5, 5}, dim];
  If[dim == 2,
    A = Table[randomRow[2, r, False], {2}],
    A = Table[randomRow[3, r], {3}]
  ];

  If[Det[A] == 0 || (dim == 3 && Count[Flatten[A], 0] > 1), Return[$Failed]];
  b = A . x0;
  If[!numbersNiceQ[A, b, diff], Return[$Failed]];
  <|"A" -> A, "b" -> b, "x0" -> x0, "type" -> "ONE"|>
];

(* --- 3. 2x2 STEP LOGIC --- *)

(* Analyzes columns to find best elimination candidate *)
analyzeVariableElimination[A_] := Module[{scores, best},
  scores = Table[
    Module[{c1 = A[[1, i]], c2 = A[[2, i]], lcm},
      If[c1 == 0 || c2 == 0, 9999,
        lcm = LCM[Abs[c1], Abs[c2]];
        lcm + If[(lcm/Abs[c1]) > 1 && (lcm/Abs[c2]) > 1, 1000, 0] (* Penalty if both need mult *)
      ]
    ], {i, 1, 2}];
  If[scores[[2]] < scores[[1]], 2, 1]
];

(* Prepares coefficients and multipliers for 2x2 elimination - Common Logic *)
eliminationStart2D[A_, b_, vars_] := Module[
  {idx, x, y, k1, k2, lcm, m1, m2, choice, targetVar, needsMult, content = {}, rows1, rows2},

  idx = analyzeVariableElimination[A];
  targetVar = vars[[idx]];
  choice = If[idx == 1, "X", "Y"];

  k1 = A[[1, idx]]; k2 = A[[2, idx]];
  lcm = LCM[Abs[k1], Abs[k2]];
  m1 = lcm / Abs[k1];
  m2 = lcm / Abs[k2];
  If[Sign[k1] === Sign[k2], m2 = -m2];

  needsMult = !(Sign[k1] =!= Sign[k2] && m1 === 1 && m2 === 1);

  AppendTo[content, stepHeader["Príprava na elimináciu"]];
  If[needsMult,
    AppendTo[content, "Chceme vyrušiť premennú " <> ToString[targetVar] <> ". Rovnice preto prenásobíme tak, aby mali pri nej rovnaký koeficient s opačným znamienkom."];,
    AppendTo[content, "Koeficienty pri premennej " <> ToString[targetVar] <> " sú už opačné, takže môžeme hneď sčítať rovnice a premennú vyrušiť."];
  ];

  rows1 = {
    {formatLHS[A[[1,1]], A[[1,2]], choice, vars], b[[1]], multiplyNote[m1]},
    {formatLHS[A[[2,1]], A[[2,2]], choice, vars], b[[2]], multiplyNote[m2]}
  };

  rows2 = {
    {formatLHS[m1 A[[1,1]], m1 A[[1,2]], "", vars], m1 b[[1]], ""},
    {formatLHS[m2 A[[2,1]], m2 A[[2,2]], "", vars], m2 b[[2]], ""}
  };

  If[needsMult,
    AppendTo[content, alignedEquationsGrouped[Join[rows1, rows2], {2}, 1]],
    AppendTo[content, alignedEquations[{
      {formatLHS[A[[1,1]], A[[1,2]], choice, vars], b[[1]], ""},
      {formatLHS[A[[2,1]], A[[2,2]], choice, vars], b[[2]], ""}
    }]]
  ];

  <|"content" -> content, "m1" -> m1, "m2" -> m2, "EliminatedVariable" -> choice, "A_mod" -> {m1 A[[1]], m2 A[[2]]}, "b_mod" -> {m1 b[[1]], m2 b[[2]]}|>
];

renderAddition2D[rowMod_, rhsMod_, vars_] := Module[{signSep},
  signSep[v_] := If[v < 0, " - ", " + "];

  alignedEquations[{{
    Row[{
      TraditionalForm[rowMod[[1,1]] vars[[1]]], signSep[rowMod[[2,1]]], TraditionalForm[Abs[rowMod[[2,1]]] vars[[1]]],
      signSep[rowMod[[1,2]]], TraditionalForm[Abs[rowMod[[1,2]]] vars[[2]]],
      signSep[rowMod[[2,2]]], TraditionalForm[Abs[rowMod[[2,2]]] vars[[2]]]
    }],
    Row[{rhsMod[[1]], signSep[rhsMod[[2]]], Abs[rhsMod[[2]]]}],
    ""
  }}]
];

(* Steps: 2x2 Unique Solution *)
stepsOne2[A_, b_, vars_] := Module[
  {data, content, m1, m2, elimVarStr, x, y, sumRHS, sumRow, keepIdx, elimIdx, keepVar, elimVar,
    stepsY, valKeep, valElim, stepsSub, rowOrig, rhsOrig, valProd, rhsRem},

  {x, y} = vars;
  data = eliminationStart2D[A, b, vars];
  content = data["content"];
  elimVarStr = data["EliminatedVariable"];
  {m1, m2} = {data["m1"], data["m2"]};

  sumRow = Total[data["A_mod"]];
  sumRHS = Total[data["b_mod"]];

  elimIdx = If[elimVarStr == "X", 1, 2];
  keepIdx = 3 - elimIdx;
  elimVar = vars[[elimIdx]];
  keepVar = vars[[keepIdx]];

  AppendTo[content, stepHeader["Sčítanie rovníc"]];
  AppendTo[content, "Sčítame rovnice. Premenná " <> ToString[elimVar] <> " vypadne, lebo má opačné koeficienty."];

  stepsY = {};

  (* vizualizácia sčítania *)
  AppendTo[content, renderAddition2D[data["A_mod"], data["b_mod"], vars]];

  AppendTo[stepsY, {sumRow[[keepIdx]] keepVar, sumRHS, ""}];
  AppendTo[content, alignedEquations[stepsY]];

  If[sumRow[[keepIdx]] == 1,
    AppendTo[content, "Po sčítaní sme dostali jednoduchú rovnicu, z ktorej hneď určíme hodnotu premennej " <> ToString[keepVar] <> "."],
    AppendTo[content, "Zostala nám rovnica s jednou premennou. Rovnicu upravíme tak, aby bola neznáma vyjadrená samostatne."];
    AppendTo[content, alignedEquations[{{sumRow[[keepIdx]] keepVar, sumRHS, ": " <> ToString[sumRow[[keepIdx]]]}}]];
  ];

  valKeep = sumRHS / sumRow[[keepIdx]];
  AppendTo[content, highlightGrid[alignedEquations[{{keepVar, valKeep, ""}}]]];

  AppendTo[content, stepHeader["Dosadenie"]];
  AppendTo[content, "Vypočítanú hodnotu " <> ToString[keepVar] <> " dosadíme do jednej pôvodnej rovnice a dopočítame " <> ToString[elimVar] <> "."];

  rowOrig = A[[1]]; rhsOrig = b[[1]];
  stepsSub = {};
  AppendTo[stepsSub, {rowOrig[[1]] vars[[1]] + rowOrig[[2]] vars[[2]], rhsOrig, Row[{keepVar, " \[Rule] ", tf[valKeep]}]}];

  (* vizualizácia dosadenia *)
  AppendTo[stepsSub, {formatSubstLHS3Generic[rowOrig, vars, <|keepVar->valKeep|>, elimVar, False], rhsOrig, ""}];

  valProd = rowOrig[[keepIdx]] * valKeep;
  Module[{noteShift},
    noteShift = Which[PossibleZeroQ[valProd], "", TrueQ[valProd > 0], Row[{"- ", TraditionalForm[valProd]}], True, Row[{"+ ", TraditionalForm[Abs[valProd]]}]];
    AppendTo[stepsSub, {formatSubstLHS3Generic[rowOrig, vars, <|keepVar->valKeep|>, elimVar, True], rhsOrig, noteShift}];
  ];

  rhsRem = rhsOrig - valProd;
  If[rowOrig[[elimIdx]] =!= 1,
    AppendTo[stepsSub, {rowOrig[[elimIdx]] elimVar, rhsRem, ": " <> ToString[rowOrig[[elimIdx]]]}];
  ];

  valElim = rhsRem / rowOrig[[elimIdx]];
  AppendTo[stepsSub, {elimVar, tf[valElim], ""}];
  AppendTo[content, alignedEquations[stepsSub]];
  AppendTo[content, highlightGrid[alignedEquations[{{elimVar, tf[valElim], ""}}]]];

  (* skúška správnosti *)
  content = appendCorrectnessCheck[
    content, A, b, vars,
    If[elimIdx == 1, {valElim, valKeep}, {valKeep, valElim}]
  ];

  <|"Content" -> content, "Solution" -> If[elimIdx == 1, {valElim, valKeep}, {valKeep, valElim}]|>
];

stepsSingular2[A_, b_, vars_, kind_String, includeConclusion_: True] := Module[
  {data, content, sumRHS, rowMod, rhsMod, rhsLine, introText, conclText},

  data = eliminationStart2D[A, b, vars];
  content = data["content"];
  rowMod = data["A_mod"];
  rhsMod = data["b_mod"];
  sumRHS = Total[rhsMod];

  AppendTo[content, stepHeader["Sčítanie rovníc"]];

  introText = Switch[kind,
    "NONE",
    "Sčítaním rovníc overíme konzistenciu sústavy. Ak vznikne nepravdivá rovnosť (napr. \(0=k\), \(k\neq 0\)), sústava nemá riešenie.",
    "INFINITE",
    "Rovnice sčítame. Ak vyjde \(0=0\), rovnice sú totožné a riešení je nekonečne veľa.",
    _,
    "Sčítame rovnice."
  ];
  AppendTo[content, introText];
  AppendTo[content, renderAddition2D[rowMod, rhsMod, vars]];

  rhsLine = Switch[kind,
    "NONE", sumRHS,
    "INFINITE", 0,
    _, sumRHS
  ];
  AppendTo[content, alignedEquations[{{0, rhsLine, ""}}]];

  If[includeConclusion,
    AppendTo[content, stepHeader["Záver"]];
    conclText = Switch[kind,
      "NONE",
      "Po sčítaní vyšla nepravdivá rovnosť (napr. 0 = nenulové číslo). To je spor, preto sústava nemá riešenie.",
      "INFINITE",
      "Po sčítaní vyšla pravdivá rovnosť 0 = 0. To znamená, že druhá rovnica je len násobkom prvej (opisujú tú istú priamku). Sústava má nekonečne veľa riešení.",
      _,
      "Sústava je singulárna."
    ];
    AppendTo[content, conclText];
  ];

  <|"Content" -> content, "Solution" -> kind|>
];


stepsNone2[A_, b_, vars_, includeConclusion_: True] :=
    stepsSingular2[A, b, vars, "NONE", includeConclusion];

stepsInfinite2[A_, b_, vars_, includeConclusion_: True] :=
    stepsSingular2[A, b, vars, "INFINITE", includeConclusion];

(* --- 4. 3x3 STEP LOGIC --- *)

(* Selects the best pair of rows [i, j] to eliminate a specific column variable *)
pickBestElimPair[rowIdx_List, elimCol_Integer, A_] := Module[{pairs, scorePair},
  pairs = Subsets[rowIdx, {2}];
  scorePair[{i_, j_}] := Module[{c1 = A[[i, elimCol]], c2 = A[[j, elimCol]]},
    If[c1 == 0 || c2 == 0, Infinity, If[Abs[c1] == Abs[c2] && Sign[c1] =!= Sign[c2], 0, LCM[Abs[c1], Abs[c2]]]]
  ];
  First @ MinimalBy[pairs, scorePair]
];

analyzeElimination3[A_] := Module[{scores},
  scores = Table[Module[{c=A[[All,j]]}, If[c[[1]]==0, 9999, LCM[Abs[c[[1]]],If[c[[2]]==0,1,Abs[c[[2]]]]] + LCM[Abs[c[[1]]],If[c[[3]]==0,1,Abs[c[[3]]]]]]], {j,1,3}];
  Ordering[scores, 1][[1]]
];

(* Performs elimination of a specific variable between two rows *)
reducePair3[rowA_, rhsA_, rowB_, rhsB_, elimCol_, vars_, tagA_, tagB_] := Module[
  {content = {}, valA = rowA[[elimCol]], valB = rowB[[elimCol]], choiceStr = {"X", "Y", "Z"}[[elimCol]],
    m1, m2, lcm, newRow, newRHS},

  If[valA == 0 || valB == 0,
    AppendTo[content, alignedEquations[{
      {formatLHS3[rowA[[1]], rowA[[2]], rowA[[3]], choiceStr], rhsA, ""},
      {formatLHS3[rowB[[1]], rowB[[2]], rowB[[3]], choiceStr], rhsB, ""}
    }]];
    If[valB == 0, {newRow, newRHS} = {rowB, rhsB}, {newRow, newRHS} = {rowA, rhsA}];,

    (* Elimination logic *)
    lcm = LCM[Abs[valA], Abs[valB]];
    m1 = lcm/Abs[valA]; m2 = lcm/Abs[valB];
    If[Sign[valA] == Sign[valB], m2 = -m2];

    AppendTo[content, alignedEquations[{
      {formatLHS3[rowA[[1]], rowA[[2]], rowA[[3]], choiceStr], rhsA, multiplyNoteRow[m1]},
      {formatLHS3[rowB[[1]], rowB[[2]], rowB[[3]], choiceStr], rhsB, multiplyNoteRow[m2]}
    }]];

    newRow = m1*rowA + m2*rowB;
    newRHS = m1*rhsA + m2*rhsB;
  ];

  AppendTo[content, alignedEquations[{{Style[formatLHS3[newRow[[1]], newRow[[2]], newRow[[3]], ""], Darker[Green, 0.2]], Style[newRHS, Darker[Green, 0.2]], ""}}]];
  <|"Row" -> newRow, "RHS" -> newRHS, "Content" -> content|>
];

(* Shared logic for 3x3 reduction *)
reduce3to2[A_, b_, vars_] := Module[
  {content = {}, elimCol, elimVar, zeroRows, nonZeroRows, pairs = {}, iKeep, rowIV, rhsIV, rowV, rhsV, remCols, remVars, A2, b2, twoCombosQ, pair, i1, i2},

  AppendTo[content, stepHeader["Redukcia sústavy 3x3 na 2x2"]];
  elimCol = analyzeElimination3[A];
  elimVar = vars[[elimCol]];
  AppendTo[content, "Vyrušíme premennú " <> ToString[elimVar] <> ", aby sme získali sústavu 2×2."];

  zeroRows = Flatten @ Position[A[[All, elimCol]], 0];
  nonZeroRows = Complement[Range[3], zeroRows];

  (* Strategy Selection *)
  If[Length[zeroRows] >= 1 && Length[nonZeroRows] >= 2,
    twoCombosQ = False;
    iKeep = First[zeroRows];
    pair = pickBestElimPair[nonZeroRows, elimCol, A];
    {i1, i2} = pair;
    AppendTo[content, Style["a) Kombinácia " <> ToString[i1] <> ". a " <> ToString[i2] <> ". rovnice:", Italic]];
    With[{res = reducePair3[A[[i1]], b[[i1]], A[[i2]], b[[i2]], elimCol, vars, "", ""]},
      content = Join[content, res["Content"]]; rowIV = res["Row"]; rhsIV = res["RHS"];
    ];
    rowV = A[[iKeep]]; rhsV = b[[iKeep]];
    AppendTo[content, Style["b) Rovnica bez vyrušovanej premennej (použijeme ju priamo):", Italic]];
    AppendTo[content, alignedEquations[{{formatLHS3[rowV[[1]], rowV[[2]], rowV[[3]], ""], rhsV, ""}}]];
    ,
    twoCombosQ = True;
    AppendTo[content, Style["a) Kombinácia 1. a 2. rovnice:", Italic]];
    With[{res = reducePair3[A[[1]], b[[1]], A[[2]], b[[2]], elimCol, vars, "", ""]},
      content = Join[content, res["Content"]]; rowIV = res["Row"]; rhsIV = res["RHS"];
    ];
    AppendTo[content, Style["b) Kombinácia 1. a 3. rovnice:", Italic]];
    With[{res = reducePair3[A[[1]], b[[1]], A[[3]], b[[3]], elimCol, vars, "", ""]},
      content = Join[content, res["Content"]]; rowV = res["Row"]; rhsV = res["RHS"];
    ];
  ];

  remCols = Delete[Range[3], elimCol];
  remVars = vars[[remCols]];
  A2 = {rowIV[[remCols]], rowV[[remCols]]};
  b2 = {rhsIV, rhsV};

  <|"Content" -> content, "A2" -> A2, "b2" -> b2, "remVars" -> remVars, "elimCol" -> elimCol, "elimVar" -> elimVar, "twoCombosQ" -> twoCombosQ|>
];

(* Steps: 3x3 Unique *)
stepsOne3[A_, b_, vars_] := Module[
  {red, content, A2, b2, remVars, sol2x2, solMap, finalVar, finalVal, row, rhs, knownSum, rhsShift, noteShift, coeffU, subSteps},

  red = reduce3to2[A, b, vars];
  content = red["Content"]; {A2, b2, remVars} = {red["A2"], red["b2"], red["remVars"]};

  AppendTo[content, stepHeader["Riešenie odvodenej sústavy 2×2"]];
  If[red["twoCombosQ"],
    AppendTo[content, "Dostali sme dve nové rovnice s dvoma neznámymi."],
    AppendTo[content, "Z jednej dvojice rovnic sme elimináciou získali jednu novú rovnicu a druhá rovnica bola už v zadaní bez vyrušovanej premennej. Spolu tvoria sústavu 2×2."]
  ];
  AppendTo[content, alignedEquations[{
    {formatLHS[A2[[1,1]], A2[[1,2]], "", remVars], b2[[1]], ""},
    {formatLHS[A2[[2,1]], A2[[2,2]], "", remVars], b2[[2]], ""}
  }]];

  sol2x2 = stepsOne2[A2, b2, remVars];
  If[sol2x2 === $Failed, Return[$Failed]];

  (* v 3×3 nechceme skúšku zzobraziť predčasne pre 2x2 *)
  Module[{c2 = sol2x2["Content"], pos},
    pos = FirstPosition[
      c2,
      Style[s_String, Bold, ___] /; StringContainsQ[s, "Skúška správnosti"],
      Missing["NotFound"]
    ];
    If[pos === Missing["NotFound"],
      content = Join[content, c2],
      content = Join[content, Take[c2, pos[[1]] - 1]]
    ];
  ];

  solMap = AssociationThread[remVars -> sol2x2["Solution"]];
  finalVar = red["elimVar"];

  AppendTo[content, stepHeader["Dosadenie do pôvodnej rovnice"]];
  AppendTo[content, "Dosadíme známe premenné do jednej z pôvodých rovníc a dopočítame poslednú neznámu."];

  row = A[[1]]; rhs = b[[1]]; coeffU = row[[red["elimCol"]]];
  subSteps = {};
  AppendTo[subSteps, {formatLHS3[row[[1]], row[[2]], row[[3]], ""], rhs, substNote[solMap, remVars, row, vars]}];
  AppendTo[subSteps, {formatSubstLHS3[row, vars, solMap, finalVar], rhs, ""}];

  knownSum = Together @ Total @ Table[If[i == red["elimCol"], 0, row[[i]] * solMap[vars[[i]]]], {i, 1, 3}];
  rhsShift = Together[rhs - knownSum];
  noteShift = Which[PossibleZeroQ[knownSum], "", TrueQ[knownSum > 0], Row[{"- ", TraditionalForm[knownSum]}], True, Row[{"+ ", TraditionalForm[Abs[knownSum]]}]];

  AppendTo[subSteps, {formatSubstLHS3Eval[row, vars, solMap, finalVar], rhs, noteShift}];

  If[coeffU === 1,
    AppendTo[subSteps, {TraditionalForm[finalVar], TraditionalForm[rhsShift], ""}],
    AppendTo[subSteps, {TraditionalForm[coeffU finalVar], TraditionalForm[rhsShift], ": " <> ToString[coeffU]}];
    AppendTo[subSteps, {TraditionalForm[finalVar], tf[rhsShift/coeffU], ""}]
  ];
  finalVal = Together[rhsShift/coeffU];

  AppendTo[content, alignedEquations[subSteps]];
  AppendTo[content, highlightGrid[alignedEquations[{{finalVar, TraditionalForm[finalVal], ""}}]]];

  (* skúška správnosti *)
  content = appendCorrectnessCheck[
    content, A, b, vars,
    Table[If[i == red["elimCol"], finalVal, solMap[vars[[i]]]], {i, 1, 3}]
  ];


  <|"Content" -> content, "Solution" -> Table[If[i == red["elimCol"], finalVal, solMap[vars[[i]]]], {i, 1, 3}]|>
];

stepsSingular3[A_, b_, vars_, kind_String] := Module[
  {red, content, A2, b2, remVars, sol2x2, headerText, midText, conclText},

  red = reduce3to2[A, b, vars];
  content = red["Content"];
  {A2, b2, remVars} = {red["A2"], red["b2"], red["remVars"]};

  AppendTo[content, stepHeader["Riešenie odvodenej sústavy 2×2"]];

  headerText = Switch[kind,
    "NONE",
    If[red["twoCombosQ"],
      "Dostali sme dve nové rovnice s dvoma neznámymi. Ak po úpravách vznikne spor (napr. 0 = nenulové číslo), pôvodná sústava 3×3 nemá riešenie.",
      "Z jednej dvojice rovníc sme elimináciou získali jednu novú rovnicu a druhá rovnica bola už v zadaní bez vyrušovanej premennej. Spolu tvoria sústavu 2×2. Ak v nej vznikne spor, pôvodná sústava 3×3 nemá riešenie."
    ],
    "INFINITE",
    If[red["twoCombosQ"],
      "Dostali sme dve nové rovnice s dvoma neznámymi. Ak po úpravách vyjde totožná rovnica (napr. 0 = 0), znamená to nekonečne veľa riešení.",
      "Z jednej dvojice rovníc sme elimináciou získali jednu novú rovnicu a druhá rovnica bola už v zadaní bez vyrušovanej premennej. Spolu tvoria sústavu 2×2. Ak v nej vyjde totožná rovnica (0 = 0), sústava má nekonečne veľa riešení."
    ],
    _,
    "Riešime odvodenej sústavu 2×2."
  ];
  AppendTo[content, headerText];

  AppendTo[content, alignedEquations[{
    {formatLHS[A2[[1,1]], A2[[1,2]], "", remVars], b2[[1]], ""},
    {formatLHS[A2[[2,1]], A2[[2,2]], "", remVars], b2[[2]], ""}
  }]];

  sol2x2 = stepsSingular2[A2, b2, remVars, kind, False];

  If[sol2x2 === $Failed,
    midText = Switch[kind,
      "NONE", "Po úpravách dostávame spornú rovnicu (napr. 0 = k). Preto sústava nemá riešenie.",
      "INFINITE", "Sústava má nekonečne veľa riešení.",
      _, ""
    ];
    If[midText =!= "", AppendTo[content, midText];],
    content = Join[content, sol2x2["Content"]];
  ];

  AppendTo[content, stepHeader["Záver"]];
  conclText = Switch[kind,
    "NONE", "Keďže po eliminácii vznikol spor, pôvodná sústava 3×3 nemá riešenie.",
    "INFINITE", "Keďže po eliminácii vyšla totožná rovnica, pôvodná sústava 3×3 má nekonečne veľa riešení.",
    _, "Záver."
  ];
  AppendTo[content, conclText];

  <|"Content" -> content, "Solution" -> kind|>
];

stepsNone3[A_, b_, vars_] := stepsSingular3[A, b, vars, "NONE"];

stepsInfinite3[A_, b_, vars_] := stepsSingular3[A, b, vars, "INFINITE"];

(* --- 5. ADVANCED 3D INFINITE FORMATTER (Restored) --- *)

chooseNiceParametrization3[A_, b_, vars_] := Module[
  {eqs = Thread[A.vars == b], rank, nFree, params0, candidates, try, results},
  rank = MatrixRank[A]; nFree = 3 - rank;
  If[nFree <= 0, Return[$Failed]];

  params0 = Take[{\[FormalT], \[FormalS], \[FormalR]}, nFree];
  candidates = Subsets[vars, {nFree}];

  try[freeVars_] := Module[{remVars, sol, rules, exprs, scales, sc, applyScales, scoreExpr},
    remVars = Complement[vars, freeVars];
    sol = Quiet @ Check[Solve[eqs /. Thread[freeVars -> params0], remVars, Reals], $Failed];
    If[sol === $Failed || sol === {}, Return[Nothing]];

    rules = Join[Thread[freeVars -> params0], sol[[1]]];
    exprs = Together[vars /. rules];

    scales = Table[LCM @@ (Denominator /@ Rationalize[Coefficient[exprs, params0[[j]]], 0]), {j, nFree}];
    exprs = Together[exprs /. Thread[params0 -> (scales*params0)]];

    sc = Total[(Total[If[#==1,0,#]& /@ (Denominator/@Rationalize[Join[{# /. Thread[params0->0]}, Coefficient[#, params0]], 0])])& /@ exprs] + Total[If[#==1,0,#]&/@scales];
    <|"FreeVars" -> freeVars, "Params" -> params0, "Exprs" -> exprs, "Score" -> sc|>
  ];

  results = try /@ candidates;
  If[results === {}, $Failed, First @ MinimalBy[results, #Score &]]
];

printInfiniteResult3[A_, b_, vars_] := Module[
  {nVars=Length[vars], aug, rref, pivotCols={}, freeCols, nFree, formalParams, exprs, best, kBox, vecBox, condBox},

  aug = Normal @ Join[A, Transpose[{b}], 2];
  rref = Normal @ RowReduce[aug];

  If[AnyTrue[rref, (AllTrue[#[[1 ;; nVars]], PossibleZeroQ] && !PossibleZeroQ[#[[nVars + 1]]]) &],
    CellTextU["Sústava nemá riešenie (v redukovanej sústave vznikol spor)."]; Return[<|"Type" -> "NONE"|>]];

  best = chooseNiceParametrization3[A, b, vars];

  If[best =!= $Failed,
    {formalParams, exprs} = {best["Params"], best["Exprs"]},

    (* FALLBACK *)
    Do[If[(p=SelectFirst[Range[nVars], !PossibleZeroQ[rref[[i, #]]] &, Missing["NotFound"]]) =!= Missing["NotFound"], AppendTo[pivotCols, p]], {i, Length[rref]}];
    freeCols = Complement[Range[nVars], pivotCols]; nFree = Length[freeCols];
    formalParams = Take[{\[FormalT], \[FormalS], \[FormalR]}, nFree]; exprs = ConstantArray[0, nVars];
    Do[exprs[[freeCols[[k]]]] = formalParams[[k]], {k, nFree}];
    Do[
      p=SelectFirst[Range[nVars], !PossibleZeroQ[rref[[i, #]]] &, Missing["NotFound"]];
      If[p=!=Missing["NotFound"], exprs[[p]] = Together[(rref[[i, nVars+1]] - If[nFree>0, rref[[i, freeCols]].exprs[[freeCols]], 0]) / rref[[i, p]]]],
      {i, Length[rref]}];
  ];

  CellTextU["Riešenia zapíšeme pomocou " <> If[Length[formalParams]==1, "parametra", "parametrov"] <> "."];
  CellBox @ Grid[Table[{formalParams[[k]], "\[Element]", "\[DoubleStruckR]"}, {k, Length[formalParams]}], Alignment -> {{Center, Center, Left}}];

  CellTextU["Potom platí:"];
  CellBox @ Grid[Table[{vars[[k]], "=", TraditionalForm[exprs[[k]]]}, {k, 3}], Alignment -> {{Right, Center, Left}}];

  vecBox = RowBox[{"[", RowBox[Riffle[ToBoxes[#, TraditionalForm] & /@ exprs, "; "]], "]"}];
  condBox = RowBox[{RowBox[Riffle[ToBoxes[#, TraditionalForm] & /@ formalParams, ", "]], "\[Element]", If[Length[formalParams]==1, "\[DoubleStruckR]", SuperscriptBox["\[DoubleStruckR]", ToString[Length[formalParams]]]]}];

  kBox = RowBox[{StyleBox["K", FontSlant -> "Italic"], "=", RowBox[{"{", RowBox[{vecBox, " ", "\[VerticalSeparator]", " ", condBox}], "}"}]}];
  CellPrint @ Cell[BoxData @ FormBox[kBox, TraditionalForm], "DisplayFormula", BaseStyle -> {FontSize -> 14}];
  <|"Type" -> "INFINITE"|>
];

(* --- 6. VISUALIZATION --- *)

lineLegendText[a_, b_, c_] := Module[{m, q, fmt = ToString[tf[#]]&},
  If[b == 0, "x = " <> fmt[c/a],
    m = Together[-a/b]; q = Together[c/b];
    "y = " <> Which[m === 1, "", m === -1, "-", True, fmt[m]] <> "x" <> Which[q === 0, "", q > 0, " + " <> fmt[q], True, " - " <> fmt[Abs[q]]]
  ]];

visualize2[A_, b_, vars_, sol_] := Module[
  {x, y, pt, xrange, yrange, seg, center, subtitle, range = 10,
    lineStyles, lineLabels, extraLegStyles, extraLegLabels, legend},

  {x, y} = vars;

  (* určenie bodu prieniku a rozsahu *)
  If[MatchQ[sol, {_?NumericQ, _?NumericQ}],
    pt = sol;
    center = 5 Round[pt/5];
    range = 10;
    xrange = center[[1]] + {-range, range};
    yrange = center[[2]] + {-range, range};
    subtitle = "Priamky sa pretínajú v jednom bode (riešenie sústavy).",

    pt = None;
    xrange = {-10, 10};
    yrange = {-10, 10};
    subtitle = If[sol === "NONE",
      "Priamky sú rovnobežné, nepretínajú sa – sústava nemá riešenie.",
      "Priamky sú totožné (prekrývajú sa) – sústava má nekonečne veľa riešení."
    ]
  ];

  seg[row_, rhs_] := With[{a = row[[1]], bb = row[[2]]},
    If[bb != 0,
      Line[{{xrange[[1]], (rhs - a*xrange[[1]])/bb}, {xrange[[2]], (rhs - a*xrange[[2]])/bb}}],
      Line[{{rhs/a, yrange[[1]]}, {rhs/a, yrange[[2]]}}]
    ]
  ];

  CellTextExpr[subtitle];

  lineStyles =
      If[sol === "INFINITE",
        {Directive[Magenta, AbsoluteThickness[2], Opacity[0.9]], Directive[Blue, AbsoluteThickness[2], Opacity[0.9], Dashing[0.05]]},
        {Directive[Magenta, Thick],Directive[Blue, Thick]}
      ];

  lineLabels = {
    lineLegendText[A[[1, 1]], A[[1, 2]], b[[1]]],
    lineLegendText[A[[2, 1]], A[[2, 2]], b[[2]]]
  };

  (* legenda pre priesečník *)
  {extraLegStyles, extraLegLabels} =
      If[pt =!= None, {
          {Directive[Black]},
          {Row[{"prienik: [",
            TraditionalForm @ Together[pt[[1]]], ", ",
            TraditionalForm @ Together[pt[[2]]], "]"}]}
        }, {{}, {}}
      ];

  legend = LineLegend[
    Join[lineStyles, extraLegStyles],
    Join[lineLabels, extraLegLabels],
    LegendMarkers -> Join[ {None, None},
      If[pt =!= None, {Graphics[{Black, Disk[]}, ImageSize -> 10]}, {}]
    ]
  ];

  CellBox @ Legended[
    Graphics[
      { {lineStyles[[1]], seg[A[[1]], b[[1]]]},
        {lineStyles[[2]], seg[A[[2]], b[[2]]]},
        If[pt =!= None,
          { {Black, Thick, Circle[pt, 0.4]}, {Green, PointSize[0.02], Point[pt]}},
          {}
        ]
      },
      PlotRange -> {xrange, yrange},
      Axes -> True,
      GridLines -> Automatic,
      ImageSize -> Medium
    ],
    legend
  ]
];

systemIntersection3[A_, b_, vars_] := Module[{rA=MatrixRank[A], rAb=MatrixRank[Join[A, Transpose[{b}], 2]], ns},
  If[rAb > rA, <|"Type" -> "NONE"|>,
    If[rA == 3, <|"Type" -> "POINT", "Point" -> LinearSolve[A, b]|>,
      ns = NullSpace[A];
      If[Length[ns] == 1, <|"Type" -> "LINE", "Point" -> (vars /. First@FindInstance[A.vars == b, vars, Reals]), "Dir" -> ns[[1]]|>,
        If[Length[ns] >= 2, <|"Type" -> "PLANE"|>, <|"Type" -> "INFINITE"|>]]]]];

visualize3[A_, b_, vars_, sol_] := Module[
  {x, y, z, range = 10, xmin, xmax, ymin, ymax, zmin, zmax, n1, n2, n3, d1, d2, d3, inter, best, subtitle, planes, mark, plot, eqLbl, planeStyles, planeLabels, extraLegStyles, extraLegLabels, legend},

  {x, y, z} = vars;
  {xmin, xmax} = {-range, range};
  {ymin, ymax} = {-range, range};
  {zmin, zmax} = {-range, range};

  n1 = N @ A[[1]]; d1 = N @ b[[1]];
  n2 = N @ A[[2]]; d2 = N @ b[[2]];
  n3 = N @ A[[3]]; d3 = N @ b[[3]];

  inter = systemIntersection3[A, b, vars];
  best = If[inter["Type"] === "LINE", chooseNiceParametrization3[A, b, vars], $Failed];

  subtitle = Switch[inter["Type"],
    "POINT", "Tri roviny majú spoločný prienik v jednom bode (riešenie sústavy).",
    "LINE",  "Tri roviny majú spoločný prienik – priamku (nekonečne veľa riešení).",
    "PLANE", "Všetky tri rovnice opisujú tú istú rovinu (nekonečne veľa riešení).",
    "NONE",  "Roviny nemajú spoločný prienik všetkých troch naraz (sústava nemá riešenie).",
    _,       "Prienik sa nepodarilo jednoznačne určiť."
  ];
  CellTextU[subtitle];

  (* popisy rovín do legendy *)
  eqLbl[row_, rhs_] := TraditionalForm[row.{x, y, z} == rhs];
  planeLabels = {eqLbl[A[[1]], b[[1]]], eqLbl[A[[2]], b[[2]]], eqLbl[A[[3]], b[[3]]]};
  planeStyles = {Cyan, Magenta, Yellow};

  planes = ContourPlot3D[
    {n1.{x, y, z} == d1, n2.{x, y, z} == d2, n3.{x, y, z} == d3},
    {x, xmin, xmax}, {y, ymin, ymax}, {z, zmin, zmax},
    Mesh -> None,
    PlotPoints -> 25,
    PerformanceGoal -> "Speed",
    ContourStyle -> {
      Directive[Cyan, Opacity[0.4]],
      Directive[Magenta, Opacity[0.4]],
      Directive[Yellow, Opacity[0.4]]
    },
    BoundaryStyle -> None
  ];

  (* zvýraznenie prieniku *)
  mark = Graphics3D @ Switch[inter["Type"],
    "POINT", {Black, PointSize[0.03], Point[N @ inter["Point"]], Black, Sphere[N @ inter["Point"], 0.35]},
    "LINE", Module[{p0, v},
      p0 = N @ inter["Point"];
      v  = N @ inter["Dir"];
      {Red, Specularity[White, 20], Tube[{p0 - 20 v, p0 + 20 v}, 0.2]}
    ], _, {}
  ];

  plot = Show[planes, mark,
    PlotRange -> {{xmin, xmax}, {ymin, ymax}, {zmin, zmax}},
    BoxRatios -> {1, 1, 1}, Axes -> True,
    AxesLabel -> {"x", "y", "z"}, SphericalRegion -> True,
    ImageSize -> 400, Lighting -> "Neutral", ViewAngle -> 35 Degree, ViewPoint -> {2.2, -2.0, 1.4},
    Method -> {"MouseInteraction" -> {"Rotate" -> True, "Pan" -> False, "Zoom" -> False}}
  ];

  (* legenda pre prienik *)
  {extraLegStyles, extraLegLabels} = Switch[inter["Type"],
    "POINT", {{Black}, {Row[{"prienik: [", Sequence @@ Riffle[TraditionalForm /@ inter["Point"], ", "], "]"}]}},
    "LINE",  {{Red}, {Row[{"priesečník: ", TraditionalForm @ best["Exprs"], ", ", TraditionalForm[\[FormalT]], "\[Element]", "\[DoubleStruckR]"}]}},
    _, {{}, {}}
  ];

  legend = If[extraLegStyles === {},
    SwatchLegend[planeStyles, planeLabels],
    SwatchLegend[Join[planeStyles, extraLegStyles], Join[planeLabels, extraLegLabels]]
  ];

  CellBox @ Legended[plot, legend];
];

(* --- 7. MAIN CONTROLLER --- *)

Gen01[diff_String, mode_String, opts : OptionsPattern[]] := Module[
  {dim, vars, st, data, A, b, steps, sol, genFunc, stepsFunc},

  If[!MemberQ[{"EASY", "MEDIUM", "HARD"}, diff], Message[Gen01::baddiff, diff]; Return[$Failed]];
  If[!MemberQ[{"TASK", "TASK_RESULT", "TASK_STEPS_RESULT"}, mode], Message[Gen01::badmode, mode]; Return[$Failed]];
  If[diff === "HARD", Message[Gen01::notimpl, diff]; Return[$Failed]];

  st = Replace[OptionValue[SolutionType], Automatic -> RandomChoice[{"ONE", "ONE", "ONE", "NONE", "INFINITE"}]];
  dim = Switch[diff, "EASY", 2, "MEDIUM", 3];
  vars = Take[{x, y, z}, dim];

  genFunc = If[st == "ONE",
    generateSystemOne[dim, diff],
    generateSingularSystem[dim, diff, st]
  ];

  data = WithRetries[Function[Null, genFunc], 200];
  If[data === $Failed, Message[Gen01::fail]; Return[$Failed]];

  A = data["A"]; b = data["b"];

  CellSection["Eliminačná metóda"];
  CellSubsection["Zadanie"];
  CellTextU["Riešte sústavu rovníc"];
  
  If[dim == 2,
    CellBox @ alignedEquations[
      Table[{formatLHS[A[[i,1]], A[[i,2]], "", vars], b[[i]], ""}, {i, 2}]
    ],
    CellBox @ alignedEquations[
      Table[{formatLHS3[A[[i,1]], A[[i,2]], A[[i,3]], ""], b[[i]], ""}, {i, 3}]
    ]
  ];
  CellTextU["Riešte v množine reálnych čísel eliminačnou metódou (sčítaním/odčítaním rovníc)."];

  (* step counter before step generation logic runs *)
  stepsCounter = 0;

  stepsFunc = Which[
    dim == 2 && st == "ONE",      stepsOne2,
    dim == 2 && st == "NONE",     stepsNone2,
    dim == 2 && st == "INFINITE", stepsInfinite2,
    dim == 3 && st == "ONE",      stepsOne3,
    dim == 3 && st == "NONE",     stepsNone3,
    dim == 3 && st == "INFINITE", stepsInfinite3
  ];

  steps = stepsFunc[A, b, vars];
  If[steps === $Failed, Message[Gen01::fail]; Return[$Failed]];
  sol = steps["Solution"];

  If[mode === "TASK_STEPS_RESULT", CellSubsection["Postup"]; Scan[renderItem, steps["Content"]]];

  If[mode =!= "TASK",
    CellSubsection["Výsledok"];
    Switch[sol,
      "NONE", CellTextU["Sústava nemá riešenie (pri sčítaní vznikol spor)."],
      "INFINITE",
      If[dim == 3, printInfiniteResult3[A, b, vars],
        Module[{par=\[FormalT], a1=A[[1,1]], b1=A[[1,2]], c1=b[[1]], baseEq, solvedEq, exprX, exprY},
          CellTextU["Sústava má nekonečne veľa riešení. Riešenia zapíšeme parametricky."];
          CellTextU["Vyjadríme jednu premennú z jednej rovnice (napr. y vyjadríme pomocou x)."];

          If[b1!=0,
            baseEq=a1 vars[[1]] + b1 vars[[2]]; solvedEq=Simplify[(c1 - a1 vars[[1]])/b1];
            CellBox@alignedEquations[{{baseEq, c1, ""}}];
            CellBox@alignedEquations[{{vars[[2]], solvedEq, ""}}];
            CellTextU["Zvolíme parameter (voľná hodnota):"];
            CellBox@Grid[{{vars[[1]], "=", par, ",", par, "\[Element]", "\[DoubleStruckR]"}}, Alignment->{{Right,Center,Left,Center,Left,Left}}];
            CellTextU["Dosadíme parameter a dostaneme tvar pre druhú premennú:"];
            CellBox@alignedEquations[{{vars[[2]], Simplify[solvedEq /. vars[[1]]->par], ""}}];
            exprX=par; exprY=Simplify[(c1 - a1 par)/b1];
            ,
            baseEq=a1 vars[[1]]; solvedEq=Simplify[c1/a1];
            CellBox@alignedEquations[{{baseEq, c1, ""}}];
            CellBox@alignedEquations[{{vars[[1]], solvedEq, ""}}];
            CellTextU["Zvolíme parameter (voľná hodnota):"];
            CellBox@Grid[{{vars[[2]], "=", par, ",", par, "\[Element]", "\[DoubleStruckR]"}}, Alignment->{{Right,Center,Left,Center,Left,Left}}];
            exprY=par; exprX=Simplify[c1/a1];
          ];

          CellBox @ Grid[{{vars[[1]], "=", TraditionalForm[exprX]}, {vars[[2]], "=", TraditionalForm[exprY]}}, Alignment -> {{Right, Center, Left}}, Spacings -> {0.6, 0.8}];
          CellPrint @ Cell[BoxData @ FormBox[RowBox[{StyleBox["K", FontSlant -> "Italic"], "=", RowBox[{"{", RowBox[{RowBox[{"[", RowBox[{ToBoxes[exprX, TraditionalForm], ";", " ", ToBoxes[exprY, TraditionalForm]}], "]"}], " ", "\[VerticalSeparator]", " ", RowBox[{ToBoxes[par, TraditionalForm], "\[Element]", "\[DoubleStruckR]"}]}], "}"}]}], TraditionalForm], "DisplayFormula", BaseStyle -> {FontSize -> 14}];
        ]
      ], _, (* ONE *)
      CellPrint @ Cell[BoxData @ ToBoxes[
        If[dim == 2,
          Row[{"Riešením sústavy rovníc je usporiadaná dvojica čísel [x,y] = ", Style[Row[{"[", tf[sol[[1]]], ", ", tf[sol[[2]]], "]"}], Bold]}],
          Row[{"Riešením sústavy rovníc je usporiadaná trojica čísel [x,y,z] = ", Style[Row[{"[", tf[sol[[1]]], ", ", tf[sol[[2]]], ", ", tf[sol[[3]]], "]"}], Bold]}]],
        TraditionalForm], "Text", ShowStringCharacters -> False
      ]
    ];
    If[OptionValue[Visualization], If[dim == 2, visualize2[A, b, vars, sol], visualize3[A, b, vars, sol]]];
  ];
];

End[];
EndPackage[];