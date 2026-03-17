(* ::Package:: *)

BeginPackage["`MatrixGenerator`"];

$CharacterEncoding = "UTF-8";
Internal`$ContextMarks = False;

GenTriangular::usage = "GenTriangular[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc v trojuholníkovej sústave pomocou augmentovanej matice \
diff: \"EASY\" (4x4), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts:
  SolutionType -> Automatic | \"ONE\" | \"NONE\" | \"INFINITE\"   (Automatic vyberá typ náhodne)
  TriangularType -> Automatic | \"L\" | \"U\"";

GenGauss::usage = "GenGauss[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc pomocou Gaussovej metódy \
diff: \"EASY\" (4x4), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> Automatic | \"ONE\" | \"NONE\" | \"INFINITE\"   (Automatic vyberá typ náhodne)";

GenGaussJordan::usage = "GenGaussJordan[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc pomocou Gaussovej-Jordanovej metódy \
(prevod na tvar (I | x)) so zobrazením celočíselných riadkových úprav na augmentovanej matici.
diff: \"EASY\" (4x4), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> Automatic | \"ONE\" | \"NONE\" | \"INFINITE\"   (Automatic vyberá typ náhodne)";

GenGaussJordanPivot::usage = "GenGaussJordanPivot[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc pomocou Gaussovej-Jordanovej metódy \
s pivotovaním výberom najmenšieho možného pivotu v st\:013apci, so zobrazením celočíselných riadkových úprav na augmentovanej matici.
diff: \"EASY\" (4x4), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> Automatic | \"ONE\" | \"NONE\" | \"INFINITE\"   (Automatic vyberá typ náhodne)";

GenInverse::usage = "GenInverse[diff, mode, opts] vygeneruje didaktický príklad výpočtu inverznej matice pomocou Gauss-Jordanovej metódy v tvare (A|E) -> (E|A^(-1)).
diff: \"EASY\" (4x4), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> \"ONE\"   (iba jeden typ riešenia, pretože inverzná matica existuje len pre regulárnu maticu)";

GenLU::usage = "GenLU[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc pomocou LU rozkladu (Doolittle).
diff: \"EASY\" (4x4), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> \"ONE\"   (iba jeden typ riešenia, pretože pre Doolittle bez pivotovania vyžadujeme regulárnu maticu s nenulovými hlavnými pivotmi počas rozkladu)";

GenTriangular::baddiff  = "Neplatná úroveň obtiažnosti `1`. Použiť \"EASY\"|\"MEDIUM\"|\"HARD\".";
GenTriangular::badmode  = "Neplatný režim výstupu `1`. Použiť \"TASK\"|\"TASK_RESULT\"|\"TASK_STEPS_RESULT\".";
GenTriangular::badst    = "Neplatný typ riešenia `1`. Použiť Automatic|\"ONE\"|\"NONE\"|\"INFINITE\".";
GenTriangular::badtri   = "Neplatný typ trojuholníkovej matice `1`. Použiť Automatic|\"L\"|\"U\".";
GenTriangular::fail     = "Nepodarilo sa vygenerovať sústavu s požadovanými parametrami.";

GenGauss::baddiff = GenTriangular::baddiff;
GenGauss::badmode = GenTriangular::badmode;
GenGauss::badst   = GenTriangular::badst;
GenGauss::fail    = GenTriangular::fail;
GenGaussJordan::baddiff = GenTriangular::baddiff;
GenGaussJordan::badmode = GenTriangular::badmode;
GenGaussJordan::badst   = GenTriangular::badst;
GenGaussJordan::fail    = GenTriangular::fail;
GenGaussJordanPivot::baddiff = GenTriangular::baddiff;
GenGaussJordanPivot::badmode = GenTriangular::badmode;
GenGaussJordanPivot::badst   = GenTriangular::badst;
GenGaussJordanPivot::fail    = GenTriangular::fail;
GenInverse::baddiff = GenTriangular::baddiff;
GenInverse::badmode = GenTriangular::badmode;
GenInverse::badst = GenTriangular::badst;
GenInverse::fail = "Nepodarilo sa vygenerovať regulárnu maticu pre výpočet inverznej matice.";
GenLU::baddiff = GenTriangular::baddiff;
GenLU::badmode = GenTriangular::badmode;
GenLU::badst   = GenTriangular::badst;
GenLU::fail    = "Nepodarilo sa vygenerovať sústavu vhodnú pre LU rozklad bez pivotovania.";

$CommonGeneratorOptions = {SolutionType -> Automatic, TriangularType -> Automatic};

Options[GenTriangular] = $CommonGeneratorOptions;
Options[GenGauss] = $CommonGeneratorOptions;
Options[GenGaussJordan] = $CommonGeneratorOptions;
Options[GenGaussJordanPivot] = $CommonGeneratorOptions;
Options[GenInverse] = {SolutionType -> "ONE"};
Options[GenLU] = {SolutionType -> "ONE"};

$FailedScrambleCount;

Begin["`Private`"];

(* ~-~-~ VALIDATION ~-~-~ *)

DimensionByDifficulty[diff_String] := Switch[diff, "EASY", 4, "MEDIUM", 5, "HARD", 6];

ValidateDifficulty[diff_] := MemberQ[{"EASY", "MEDIUM", "HARD"}, diff];
ValidateMode[mode_] := MemberQ[{"TASK", "TASK_RESULT", "TASK_STEPS_RESULT"}, mode];
ValidateSolutionType[st_] := TrueQ[st === Automatic] || MemberQ[{"ONE", "NONE", "INFINITE"}, st];
ResolveSolutionType[st_] := If[st =!= Automatic, st, RandomChoice[{0.8, 0.1, 0.1} -> {"ONE", "NONE", "INFINITE"}]];
validateTriangularType[tri_] := TrueQ[tri === Automatic] || MemberQ[{"L", "U"}, tri];
resolveTriangularType[tri_] := If[tri === Automatic, RandomChoice[{"L", "U"}], tri];

(* ~-~-~ CELL PRINTING ~-~-~ *)

inNotebookQ[] := Head @ Quiet[EvaluationNotebook[]] === NotebookObject;
printCellStyle[expr_, style_String] := If[inNotebookQ[], CellPrint @ Cell[expr, style, ShowStringCharacters -> False], Print[expr]];
printTextCell[str_String] := printCellStyle[str, "Text"];
printSectionCell[str_String] := printCellStyle[str, "Section"];
printSubsectionCell[str_String] := printCellStyle[str, "Subsection"];
printFormulaCell[expr_] := Module[{boxes}, boxes = Quiet @ Check[BoxData @ ToBoxes[expr, StandardForm], expr]; printCellStyle[boxes, "DisplayFormula"]];

highlightGrid[grid_] := Style[grid, Background -> RGBColor[0.95, 0.95, 0.95], Frame -> True, FrameStyle -> None, FrameMargins -> 5];
highlightTerm[term_] := Style[term, Bold, RGBColor[0.8, 0, 0]];
tf[val_] := TraditionalForm[val];
tft[val_] := tf[Together[val]];

(* rovnice pre dosadzovanie v gauss *)
gaussBackSubstEquations[aug_, vars_, sol0_, skipIdx_, content_] := Module[
  {n = Length[aug], sol = sol0, row, pivot, rhsVal, terms, symExpr, subExpr, sumProducts, exprVal, boldVal, coeffTimes, out = content, paramPrintedQ = False},

  boldVal[val_] := Style[
    If[IntegerQ[val] && val < 0, Row[{"(", tft[val], ")"}],
      If[IntegerQ[val], tft[val], TraditionalForm[val]]
    ],
    Bold
  ];
  coeffTimes[a_, x_] := If[a === 1, x, Row[{tf[a], "\[CenterDot]", x}]];

  Do[
    If[IntegerQ[skipIdx] && i === skipIdx, Continue[]];

    row = aug[[i]]; pivot = row[[i]]; rhsVal = row[[n + 1]];
    If[pivot === 0, Continue[]];

    terms = Select[Table[{row[[j]], sol[[j]]}, {j, i + 1, n}], #[[1]] =!= 0 &];

    symExpr = Row @ Flatten @ Join[
      {tft[rhsVal]},
      Table[
        With[{a = row[[j]], v = Style[tf[vars[[j]]], Bold]},
          {If[a > 0, " - ", " + "], coeffTimes[Abs[a], v]}
        ],
        {j, i + 1, n}
      ]
    ];
    AppendTo[out, Which[
      pivot === 1, Row[{tf[vars[[i]]], " = ", symExpr}],
      pivot === -1, Row[{tf[vars[[i]]], " = -(", symExpr, ")"}],
      True, Row[{tf[vars[[i]]], " = (", symExpr, ")/", tf[pivot]}]
    ]];

    subExpr = Row @ Flatten @ Join[
      {tft[rhsVal]},
      Table[
        With[{a = row[[j]], val = boldVal[sol[[j]]]},
          {If[a > 0, " - ", " + "], coeffTimes[Abs[a], val]}
        ],
        {j, i + 1, n}
      ]
    ];
    AppendTo[out, Which[
      pivot === 1, Row[{tf[vars[[i]]], " = ", subExpr}],
      pivot === -1, Row[{tf[vars[[i]]], " = -(", subExpr, ")"}],
      True, Row[{tf[vars[[i]]], " = (", subExpr, ")/", tf[pivot]}]
    ]];

    sumProducts = Total[terms[[All, 1]]*terms[[All, 2]]];
    exprVal = Together[(rhsVal - sumProducts)/pivot];
    sol[[i]] = exprVal;

    AppendTo[out, highlightGrid @ Grid[
      {{tf[vars[[i]]], "=", TraditionalForm[exprVal]}},
      Alignment -> {{Right, Center, Left}}, BaseStyle -> {FontSize -> 16}
    ]];
    ,
    {i, n, 1, -1}
  ];

  {sol, out}
];

(* ~-~-~ STEP RENDERING ~-~-~ *)

withStepCounter[renderFn_] := Block[{stepsCounter = 0}, renderFn[]];
makeStepHeader[text_String] := (stepsCounter++; Style[ToString[stepsCounter] <> ". " <> text, Bold]);
renderStepItem[item_] := Which[
  StringQ[item], printTextCell[item],
  MatchQ[item, Style[_String, ___]], CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Text", ShowStringCharacters -> False],
  Head[item] === Cell, CellPrint[item],
  Head[item] === Graphics || Head[item] === Graphics3D, CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Graphics"],
  True, printFormulaCell[item]
];

buildVars[n_] := Take[{a, b, c, d, e, f}, n];

(* pre output vypise infinte ↓, neskor to vymyslim inak *)
infiniteSolutionFromSolvedAug[data_Association] := Module[
  {n = data["n"], augS, A, b, idx, solExprs, pivot},
  augS = data["SolvedAug"];
  A = augS[[All, 1 ;; n]];
  b = augS[[All, n + 1]];
  idx = data["ParamIdx"];

  solExprs = ConstantArray[0, n];
  solExprs[[idx]] = \[FormalT];

  Do[
    If[i === idx, Continue[]];
    pivot = A[[i, i]];
    solExprs[[i]] = Expand[(b[[i]] - A[[i, idx]]*\[FormalT])/pivot];
    ,
    {i, 1, n}
  ];

  solExprs
];

buildTaskEquations[A_, b_, vars_] := MapThread[HoldForm[#1 == #2] &, {A . vars, b}];
augFromAb[A_, b_] := Join[A, List /@ b, 2];


(* ~-~-~ ROW OPERATIONS - delenie, kombinácia ~-~-~ *)

(* note pre delenie riadku *)
rowNoteDivide[i_, p_] := Row[{"R", i, " \[LeftArrow] R", i, " / ", tf[p]}];
rowApplyDivide[aug_, i_Integer, p_Integer] := ReplacePart[aug, i -> (aug[[i]]/p)];

(* note pre kombináciu riadkov *)
rowNoteCombine[i_, terms_List] := Module[{base = Row[{"R", i, " \[LeftArrow] R", i}]},
  Row @ Prepend[(Row[{If[#2 < 0, " - ", " + "], tf[Abs[#2]], "\[CenterDot]R", #1}] & @@@ terms), base]
];

rowApplyCombine[aug_, i_Integer, terms_List] := Module[{row = aug[[i]]},
  ReplacePart[aug,  i -> (row + Total[terms[[All, 2]] aug[[terms[[All, 1]]]]])]
];

augRender2[before_, after_, notes_, hiBefore_, hiAfter_] := Grid[{{
  alignedAugmentedMatrix[before, notes, hiBefore], Spacer[18],
  alignedAugmentedMatrix[after, {}, hiAfter]}},
  Alignment -> {Left, Center, Left}, Spacings -> {0, 0}
];

augRender3[before_, mid_, after_, notes1_, notes2_, hi1_, hi2_, hi3_] := Grid[
  {{alignedAugmentedMatrix[before, notes1, hi1],
    Spacer[18], alignedAugmentedMatrix[mid, notes2, hi2],   (* "/gcd" *)
    Spacer[18], alignedAugmentedMatrix[after, {}, hi3]      (* final *)
  }}, Alignment -> {Left, Center, Left}, Spacings -> {0, 0}
];

SetAttributes[rowAppendElimStep, HoldFirst];

rowAppendElimStep[content_, before_, elimRes_, r_Integer, i_Integer, n_Integer, hiBase_Association] := Module[{notes, notes2, mid, after2, hi1, hi2, hi3},
  notes = ConstantArray["", n];
  notes[[r]] = rowNoteElim[r, i, elimRes["p2"], elimRes["a2"]];

  hi1 = Join[hiBase, <|"ActiveRow" -> r, "PivotPos" -> {i, i}|>];
  hi2 = Join[hiBase, <|"ActiveRow" -> r, "PivotPos" -> {i, i}, "GreenCells" -> {{r, i}}|>];
  hi3 = hi2;

  If[elimRes["DivG"] > 1,
    mid = elimRes["AugRaw"];
    after2 = elimRes["Aug"];

    notes2 = ConstantArray["", n];
    notes2[[r]] = rowNoteDivide[r, elimRes["DivG"]];

    AppendTo[content, augRender3[before, mid, after2, notes, notes2, hi1, hi2, hi3]];
    after2
    ,
    after2 = elimRes["Aug"];
    AppendTo[content, augRender2[before, after2, notes, hi1, hi2]];
    after2
  ]
];

(* ~-~-~ MATRIX ROW HELPERS ~-~-~ *)

rowAbsGCD[row_List] := Module[{g = Apply[GCD, Abs[row]]}, If[g === 0, 1, g]];

normalizeRow[row_List] := Module[{g = rowAbsGCD[row]}, If[g > 1, row/g, row]];

choosePivotRow[aug_, i_Integer] := Module[{n = Length[aug], candidates, best},
  candidates = Select[Range[i, n], aug[[#, i]] =!= 0 &];
  If[candidates === {},
    i,
    best = First @ MinimalBy[candidates, {Abs[aug[[#, i]]], Total[Abs[aug[[#]]]]} &];
    best
  ]
];

(* obmedzené "pivotovanie" pre obyčajny gauss *)
choosePivotRowIfZero[aug_, i_Integer] := Module[{n = Length[aug], candidates},
  If[aug[[i, i]] =!= 0, Return[i]];
  candidates = Select[Range[i + 1, n], aug[[#, i]] =!= 0 &];
  If[candidates === {}, i, First[candidates]]
];

rowNoteSwap[i_, k_] := Row[{"R", i, " \[LeftRightArrow] R", k}];

rowApplySwap[aug_, i_Integer, k_Integer] := ReplacePart[aug, {i -> aug[[k]], k -> aug[[i]]}];

rowNoteElim[r_, i_, p2_, a2_] := Module[{leftPart, rightPart, op},
  leftPart = If[p2 === 1, Row[{"R", r}], Row[{tf[p2], "\[CenterDot]", "R", r}]];
  rightPart = If[Abs[a2] === 1, Row[{"R", i}], Row[{tf[Abs[a2]], "\[CenterDot]", "R", i}]];
  op = If[a2 < 0, " + ", " - "];

  Row[{"R", r, " \[LeftArrow] ", leftPart, op, rightPart}]
];

rowApplyElimStable[aug_, r_Integer, i_Integer] := Module[{p, a, g1, p2, a2, rowRaw, g2, rowFinal, augRaw, augFinal},
  p = aug[[i, i]]; a = aug[[r, i]];

  If[a === 0,
    <|"Aug" -> aug, "AugRaw" -> aug, "p2" -> 0, "a2" -> 0, "DivG" -> 1|>,
    g1 = GCD[p, a];
    p2 = p/g1;
    a2 = a/g1;

    (* medzi-krok *)
    rowRaw = p2*aug[[r]] - a2*aug[[i]];
    g2 = rowAbsGCD[rowRaw];

    rowFinal = If[g2 > 1, rowRaw/g2, rowRaw];
    augRaw = ReplacePart[aug, r -> rowRaw];
    augFinal = ReplacePart[aug, r -> rowFinal];

    <|"Aug" -> augFinal, "AugRaw" -> augRaw, "p2" -> p2, "a2" -> a2, "DivG" -> g2|>
  ]
];

(* pre INFINITE a NONE *)
contradictionRowQ[row_List] := Module[{lhs = Most[row], rhs = Last[row]}, (AllTrue[lhs, # === 0 &] && rhs =!= 0)];
findContradictionRow[aug_] := Module[{idx = FirstCase[Range[Length[aug]], i_ /; contradictionRowQ[aug[[i]]], Missing["NotFound"]]}, idx];

(* ~-~-~ MATRIX VISUALIZATION ~-~-~ *)

alignedAugmentedMatrix[aug_, notes_List : {}, hi_Association : <||>] := Module[{nRows, nCols, nA, notes2, pivotPos, activeRow, sourceRows, greenCells, bar, rowColor, sourceColor, boldDiagQ, wrapBg, makeCell, makeBar, leftBracketCell, rightBracketCell, rows, matrixGrid, notesGrid},
  {nRows, nCols} = Dimensions[aug];
  nA = nCols - 1;

  notes2 = If[notes === {}, ConstantArray["", nRows], PadRight[notes, nRows, ""]];
  pivotPos = Lookup[hi, "PivotPos", None];
  activeRow = Lookup[hi, "ActiveRow", None];

  sourceRows = Lookup[hi, "SourceRows", {}];
  greenCells = Lookup[hi, "GreenCells", {}];
  boldDiagQ = TrueQ @ Lookup[hi, "BoldDiagonal", False];

  bar = Style["|", GrayLevel[.35], FontSize -> 16];

  rowColor = RGBColor[0.90, 0.95, 1];
  sourceColor = RGBColor[0.95, 0.92, 1.00];

  wrapBg[i_, expr_] := Module[{bg = None},
    If[IntegerQ[activeRow] && i === activeRow, bg = rowColor,
      If[MemberQ[sourceRows, i], bg = sourceColor]
    ];
    Item[expr, Background -> bg]
  ];

  makeCell[i_, j_, val_] := Module[{cell = TraditionalForm[val], isGreen, showPivotQ, isDiag},
    isGreen = MemberQ[greenCells, {i, j}];
    isDiag = boldDiagQ && (j <= nA) && (i === j);

    showPivotQ = ListQ[pivotPos] && ((IntegerQ[activeRow] && activeRow === pivotPos[[1]]) || MemberQ[sourceRows, pivotPos[[1]]]);

    If[isGreen,
      cell = Style[cell, Darker[Green], Bold],
      If[showPivotQ && pivotPos === {i, j},
        cell = Style[cell, Bold],
        If[isDiag, cell = Style[cell, Bold]]
      ]
    ];

    wrapBg[i, Pane[cell, ImageSize -> {Automatic, 18}, Alignment -> {Right, Center}]]
  ];

  makeBar[i_] := wrapBg[i, bar];

  leftBracketCell = Item["", Frame -> {{True, False}, {True, True}}];
  rightBracketCell = Item["", Frame -> {{False, True}, {True, True}}];

  rows = Table[
    Join[
      {If[i === 1, leftBracketCell, SpanFromAbove]},
      Table[makeCell[i, j, aug[[i, j]]], {j, 1, nA}],
      {makeBar[i], makeCell[i, nA + 1, aug[[i, nA + 1]]]},
      {If[i === 1, rightBracketCell, SpanFromAbove]}
    ],
    {i, 1, nRows}
  ];

  matrixGrid = Grid[
    rows,
    Alignment -> Join[{Center}, ConstantArray[Right, nA], {Center, Right, Center}],
    Spacings -> {1, 1},
    BaseStyle -> {FontSize -> 14},
    ItemSize -> {({#, Automatic} & /@ Join[{0.2}, ConstantArray[1.2, nA], {0.2, 1.2, 0.2}]), Automatic}
  ];

  notesGrid = Grid[
    List /@ (
      Item[
        Pane[Style[#, GrayLevel[.35], FontSize -> 13], {150, Automatic}, Alignment -> Left],
        Background -> White
      ] & /@ notes2
    ),
    Alignment -> Left, Spacings -> {0, 1.15}, BaseStyle -> {FontSize -> 14}
  ];

  Grid[{{matrixGrid, Spacer[12], notesGrid}}, Alignment -> {Left, Center, Left}, Spacings -> {0, 0}]
];

(* renderovanie pre tvar (A|E)                                            *)
alignedAugmentedMatrixInverse[aug_, notes_List : {}, hi_Association : <||>] := Module[
  {nRows, nCols, nA, notes2, pivotPos, activeRow, sourceRows, greenCells,
    bar, rowColor, sourceColor, boldIdentityQ, wrapBg, makeCell, makeBar,
    leftBracketCell, rightBracketCell, rows, matrixGrid, notesGrid, showPivotQ},

  {nRows, nCols} = Dimensions[aug];
  nA = Quotient[nCols, 2];

  notes2 = If[notes === {}, ConstantArray["", nRows], PadRight[notes, nRows, ""]];
  pivotPos = Lookup[hi, "PivotPos", None];
  activeRow = Lookup[hi, "ActiveRow", None];
  sourceRows = Lookup[hi, "SourceRows", {}];
  greenCells = Lookup[hi, "GreenCells", {}];
  boldIdentityQ = TrueQ @ Lookup[hi, "BoldIdentity", False];

  bar = Style["|", GrayLevel[.35], FontSize -> 16];
  rowColor = RGBColor[0.90, 0.95, 1];
  sourceColor = RGBColor[0.95, 0.92, 1.00];

  wrapBg[i_, expr_] := Module[{bg = None},
    If[IntegerQ[activeRow] && i === activeRow, bg = rowColor,
      If[MemberQ[sourceRows, i], bg = sourceColor]
    ];
    Item[expr, Background -> bg]
  ];

  showPivotQ = ListQ[pivotPos] && ((IntegerQ[activeRow] && activeRow === pivotPos[[1]]) || MemberQ[sourceRows, pivotPos[[1]]]);

  makeCell[i_, j_, val_] := Module[{cell = TraditionalForm[val], isGreen, isIdentity},
    isGreen = MemberQ[greenCells, {i, j}];
    isIdentity = boldIdentityQ && j > nA && i === j - nA && val === 1;

    If[isGreen,
      cell = Style[cell, Darker[Green], Bold],
      If[showPivotQ && pivotPos === {i, j},
        cell = Style[cell, Bold],
        If[isIdentity, cell = Style[cell, Bold]]
      ]
    ];

    wrapBg[i, Pane[cell, ImageSize -> {Automatic, 18}, Alignment -> {Right, Center}]]
  ];

  makeBar[i_] := wrapBg[i, bar];

  leftBracketCell = Item["", Frame -> {{True, False}, {True, True}}];
  rightBracketCell = Item["", Frame -> {{False, True}, {True, True}}];

  rows = Table[
    Join[
      {If[i === 1, leftBracketCell, SpanFromAbove]},
      Table[makeCell[i, j, aug[[i, j]]], {j, 1, nA}],
      {makeBar[i]},
      Table[makeCell[i, j, aug[[i, j]]], {j, nA + 1, nCols}],
      {If[i === 1, rightBracketCell, SpanFromAbove]}
    ],
    {i, 1, nRows}
  ];

  matrixGrid = Grid[
    rows,
    Alignment -> Join[{Center}, ConstantArray[Right, nA], {Center}, ConstantArray[Right, nA], {Center}],
    Spacings -> {1, 1},
    BaseStyle -> {FontSize -> 14},
    ItemSize -> {Join[{0.2}, ConstantArray[1.2, nA], {0.2}, ConstantArray[1.2, nA], {0.2}], Automatic}
  ];

  notesGrid = Grid[
    List /@ (
      Item[
        Pane[Style[#, GrayLevel[.35], FontSize -> 13], {170, Automatic}, Alignment -> Left],
        Background -> White
      ] & /@ notes2
    ),
    Alignment -> Left,
    Spacings -> {0, 1.15},
    BaseStyle -> {FontSize -> 14}
  ];

  Grid[{{matrixGrid, Spacer[12], notesGrid}}, Alignment -> {Left, Center, Left}, Spacings -> {0, 0}]
];

augRender2Inverse[before_, after_, notes_, hiBefore_, hiAfter_] := Grid[{{
  alignedAugmentedMatrixInverse[before, notes, hiBefore], Spacer[18],
  alignedAugmentedMatrixInverse[after, {}, hiAfter]
}},
  Alignment -> {Left, Center, Left},
  Spacings -> {0, 0}
];

augRender3Inverse[before_, mid_, after_, notes1_, notes2_, hi1_, hi2_, hi3_] := Grid[{{
  alignedAugmentedMatrixInverse[before, notes1, hi1], Spacer[18],
  alignedAugmentedMatrixInverse[mid, notes2, hi2], Spacer[18],
  alignedAugmentedMatrixInverse[after, {}, hi3]
}},
  Alignment -> {Left, Center, Left},
  Spacings -> {0, 0}
];

SetAttributes[rowAppendElimStepInverse, HoldFirst];

rowAppendElimStepInverse[content_, before_, elimRes_, r_Integer, i_Integer, n_Integer, hiBase_Association] := Module[
  {notes, notes2, mid, after2, hi1, hi2, hi3},

  notes = ConstantArray["", n];
  notes[[r]] = rowNoteElim[r, i, elimRes["p2"], elimRes["a2"]];

  hi1 = Join[hiBase, <|"ActiveRow" -> r, "PivotPos" -> {i, i}|>];
  hi2 = Join[hiBase, <|"ActiveRow" -> r, "PivotPos" -> {i, i}, "GreenCells" -> {{r, i}}|>];
  hi3 = hi2;

  If[elimRes["DivG"] > 1,
    mid = elimRes["AugRaw"];
    after2 = elimRes["Aug"];

    notes2 = ConstantArray["", n];
    notes2[[r]] = rowNoteDivide[r, elimRes["DivG"]];

    AppendTo[content, augRender3Inverse[before, mid, after2, notes, notes2, hi1, hi2, hi3]];
    after2
    ,
    after2 = elimRes["Aug"];
    AppendTo[content, augRender2Inverse[before, after2, notes, hi1, hi2]];
    after2
  ]
];

luEntrySymbol[sym_String, i_, j_] := Subscript[Style[sym, Italic], Row[{i, ",", j}]];
luScalarSymbol[sym_String, i_] := Subscript[Style[sym, Italic], i];

luFactorDisplay[val_] := If[NumberQ[val] && val < 0, Row[{"(", tft[val], ")"}], tft[val]];

luCoeffTimes[a_, x_] := Which[
  a === 1, x,
  a === -1, Row[{"-", x}],
  True, Row[{tft[a], "\[CenterDot]", x}]
];

luSignedScalarSum[vals_List] := Module[{clean, first, rest},
  clean = Select[Together /@ vals, # =!= 0 &];
  If[clean === {}, Return[tft[0]]];

  first = First[clean];
  rest = Rest[clean];

  Row @ Flatten @ Join[
    {
      If[first < 0, Row[{"-", tft[Abs[first]]}], tft[first]]
    },
    Table[
      If[val < 0,
        {" - ", tft[Abs[val]]},
        {" + ", tft[val]}
      ],
      {val, rest}
    ]
  ]
];

luSumDisplay[terms_List] := luSignedScalarSum[Times @@@ Select[terms, #[[1]] =!= 0 && #[[2]] =!= 0 &]];

luLinearCombinationDisplay[terms_List] := Module[{clean, first, rest},
  clean = Select[terms, #[[1]] =!= 0 &];
  If[clean === {}, Return[tft[0]]];

  first = First[clean];
  rest = Rest[clean];

  Row @ Flatten @ Join[
    {
      If[first[[1]] < 0,
        luCoeffTimes[Abs[first[[1]]], first[[2]]] /. Row[{"-", x_}] :> Row[{"-", x}],
        luCoeffTimes[first[[1]], first[[2]]]
      ]
    },
    Table[
      If[term[[1]] < 0,
        {" - ", luCoeffTimes[Abs[term[[1]]], term[[2]]]},
        {" + ", luCoeffTimes[term[[1]], term[[2]]]}
      ],
      {term, rest}
    ]
  ]
];

luMatrixPairGrid[L_, U_] := highlightGrid @ Grid[
  {{
    Style["L =", Bold, FontSize -> 16],
    TraditionalForm[MatrixForm[L]],
    Spacer[20],
    Style["U =", Bold, FontSize -> 16],
    TraditionalForm[MatrixForm[U]]
  }},
  Alignment -> Left,
  Spacings -> {2, 1}
];

luVectorGrid[label_String, vec_List] := highlightGrid @ Grid[
  {{
    Style[label <> " =", Bold, FontSize -> 16],
    TraditionalForm[MatrixForm[vec]]
  }},
  Alignment -> {{Right, Left}},
  Spacings -> {2, 1}
];

luGeneralMatricesGrid[n_Integer] := Module[{Lsym, Usym},
  Lsym = Table[
    Which[
      i < j, 0,
      i == j, 1,
      True, luEntrySymbol["l", i, j]
    ],
    {i, 1, n}, {j, 1, n}
  ];
  Usym = Table[
    Which[
      i > j, 0,
      True, luEntrySymbol["u", i, j]
    ],
    {i, 1, n}, {j, 1, n}
  ];
  luMatrixPairGrid[Lsym, Usym]
];

luFormulaUGeneral[i_Integer, j_Integer] := If[i === 1,
  Row[{luEntrySymbol["u", i, j], " = ", luEntrySymbol["a", i, j]}],
  Row[{
    luEntrySymbol["u", i, j], " = ",
    luEntrySymbol["a", i, j], " - ",
    Underoverscript["\[Sum]", Row[{k, " = 1"}], i - 1],
    Row[{luEntrySymbol["l", i, k], luEntrySymbol["u", k, j]}]
  }]
];

luFormulaLGeneral[j_Integer, i_Integer] := If[i === 1,
  Row[{luEntrySymbol["l", j, i], " = ", luEntrySymbol["a", j, i], "/", luEntrySymbol["u", i, i]}],
  Row[{
    luEntrySymbol["l", j, i], " = (",
    luEntrySymbol["a", j, i], " - ",
    Underoverscript["\[Sum]", Row[{k, " = 1"}], i - 1],
    Row[{luEntrySymbol["l", j, k], luEntrySymbol["u", k, i]}],
    ")/", luEntrySymbol["u", i, i]
  }]
];

luEquationForwardDisplay[row_List, rhs_, vars_List, idx_Integer] := Module[{terms},
  terms = Table[{row[[j]], vars[[j]]}, {j, 1, idx}];
  Row[{luLinearCombinationDisplay[terms], " = ", tft[rhs]}]
];

luEquationBackwardDisplay[row_List, rhs_, vars_List, idx_Integer, n_Integer] := Module[{terms},
  terms = Table[{row[[j]], vars[[j]]}, {j, idx, n}];
  Row[{luLinearCombinationDisplay[terms], " = ", tft[rhs]}]
];

luMatrixProductDisplay[left_, right_] := Module[{rawMatrix, evalMatrix, finalMatrix, terms, vals},
  rawMatrix = Table[
    terms = Select[Transpose[{left[[i]], right[[All, j]]}], #[[1]] =!= 0 && #[[2]] =!= 0 &];
    If[terms === {},
      0,
      Row @ Riffle[(Row[{luFactorDisplay[#[[1]]], "\[CenterDot]", luFactorDisplay[#[[2]]]}] & /@ terms), " + "]
    ],
    {i, 1, Length[left]}, {j, 1, Length[right[[1]]]}
  ];

  evalMatrix = Table[
    vals = Select[Times @@@ Transpose[{left[[i]], right[[All, j]]}], # =!= 0 &];
    If[vals === {},
      0,
      luSignedScalarSum[vals]
    ],
    {i, 1, Length[left]}, {j, 1, Length[right[[1]]]}
  ];

  finalMatrix = Together[left . right];

  highlightGrid @ Grid[
    {{
      TraditionalForm[MatrixForm[left]],
      Style["·", Bold, FontSize -> 16],
      TraditionalForm[MatrixForm[right]],
      Style["=", Bold, FontSize -> 16],
      TraditionalForm[MatrixForm[rawMatrix]],
      Style["=", Bold, FontSize -> 16],
      TraditionalForm[MatrixForm[evalMatrix]],
      Style["=", Bold, FontSize -> 16],
      TraditionalForm[MatrixForm[finalMatrix]]
    }},
    Alignment -> Center,
    Spacings -> {1, 1},
    BaseStyle -> {FontSize -> 14}
  ]
];

(* ~-~-~ MATRIX GENERATION ~-~-~ *)

$bRange = {-10, 10};
nonzeroRange[min_, max_] := DeleteCases[Range[min, max], 0];

$MaxBounds = 99; (*väčšie číslo sa nemôže ukázať*)
$Bounds = Quotient[$MaxBounds, 2.5]; (*väčšie číslo sa nemôže vygenerovať*)
$MaxRetryCount = 150;

matrixMaxAbs[m_] := Max[Abs[Flatten[m]]];

(* spoločný trace pre doprednú elimináciu *)
collectForwardEliminationTrace[aug_, pivotMode_: "ZERO"] := Module[
  {workAug, n, i, r, pivotRowFn, pivotRow, pivotValue, elimRes, trace = {}},

  workAug = aug;
  n = Length[workAug];

  pivotRowFn = Switch[pivotMode,
    "MIN", choosePivotRow,
    _, choosePivotRowIfZero
  ];

  AppendTo[trace, <|"Type" -> "Start", "Matrix" -> workAug|>];

  Do[
    pivotRow = pivotRowFn[workAug, i];

    If[pivotRow =!= i,
      workAug = rowApplySwap[workAug, i, pivotRow];
      AppendTo[trace, <|
        "Type" -> "Swap",
        "PivotCol" -> i,
        "Row" -> i,
        "SourceRow" -> pivotRow,
        "Matrix" -> workAug
      |>];
    ];

    pivotValue = workAug[[i, i]];
    If[pivotValue === 0, Continue[]];

    Do[
      If[workAug[[r, i]] =!= 0,
        elimRes = rowApplyElimStable[workAug, r, i];

        If[elimRes["DivG"] > 1,
          AppendTo[trace, <|
            "Type" -> "ElimRaw",
            "PivotCol" -> i,
            "Row" -> r,
            "Matrix" -> elimRes["AugRaw"]
          |>]
        ];

        workAug = elimRes["Aug"];
        AppendTo[trace, <|
          "Type" -> "ElimFinal",
          "PivotCol" -> i,
          "Row" -> r,
          "Matrix" -> workAug
        |>];
      ],
      {r, i + 1, n}
    ],
    {i, 1, n - 1}
  ];

  <|
    "FinalAug" -> workAug,
    "Trace" -> trace
  |>
];

forwardEliminationWithinBoundsQ[aug_, pivotMode_: "ZERO"] := Module[
  {traceData, limit},

  limit = $MaxBounds;
  traceData = collectForwardEliminationTrace[aug, pivotMode];

  AllTrue[
    traceData["Trace"],
    matrixMaxAbs[#["Matrix"]] <= limit &
  ]
];

collectInverseEliminationTrace[aug_, pivotMode_: "MIN"] := Module[
  {workAug, n, i, r, pivotRowFn, pivotRow, pivotValue, elimRes, trace = {}, after},

  workAug = aug;
  n = Length[workAug];

  pivotRowFn = Switch[pivotMode,
    "MIN", choosePivotRow,
    _, choosePivotRowIfZero
  ];

  AppendTo[trace, <|"Type" -> "Start", "Matrix" -> workAug|>];

  (* dopredná eliminácia *)
  Do[
    pivotRow = pivotRowFn[workAug, i];

    If[pivotRow =!= i,
      workAug = rowApplySwap[workAug, i, pivotRow];
      AppendTo[trace, <|
        "Type" -> "Swap",
        "Phase" -> "Forward",
        "PivotCol" -> i,
        "Row" -> i,
        "SourceRow" -> pivotRow,
        "Matrix" -> workAug
      |>];
    ];

    pivotValue = workAug[[i, i]];
    If[pivotValue === 0, Continue[]];

    Do[
      If[workAug[[r, i]] =!= 0,
        elimRes = rowApplyElimStable[workAug, r, i];

        If[elimRes["DivG"] > 1,
          AppendTo[trace, <|
            "Type" -> "ElimRaw",
            "Phase" -> "Forward",
            "PivotCol" -> i,
            "Row" -> r,
            "Matrix" -> elimRes["AugRaw"]
          |>]
        ];

        workAug = elimRes["Aug"];
        AppendTo[trace, <|
          "Type" -> "ElimFinal",
          "Phase" -> "Forward",
          "PivotCol" -> i,
          "Row" -> r,
          "Matrix" -> workAug
        |>];
      ],
      {r, i + 1, n}
    ],
    {i, 1, n - 1}
  ];

  (* spätná eliminácia *)
  Do[
    pivotValue = workAug[[i, i]];
    If[pivotValue === 0, Continue[]];

    Do[
      If[workAug[[r, i]] =!= 0,
        elimRes = rowApplyElimStable[workAug, r, i];

        If[elimRes["DivG"] > 1,
          AppendTo[trace, <|
            "Type" -> "ElimRaw",
            "Phase" -> "Backward",
            "PivotCol" -> i,
            "Row" -> r,
            "Matrix" -> elimRes["AugRaw"]
          |>]
        ];

        workAug = elimRes["Aug"];
        AppendTo[trace, <|
          "Type" -> "ElimFinal",
          "Phase" -> "Backward",
          "PivotCol" -> i,
          "Row" -> r,
          "Matrix" -> workAug
        |>];
      ],
      {r, 1, i - 1}
    ],
    {i, n, 2, -1}
  ];

  (* normalizácia pivotov *)
  Do[
    pivotValue = workAug[[i, i]];
    If[pivotValue === 0, Continue[]];

    If[pivotValue =!= 1,
      after = rowApplyDivide[workAug, i, pivotValue];
      workAug = after;

      AppendTo[trace, <|
        "Type" -> "Divide",
        "Phase" -> "Normalize",
        "Row" -> i,
        "Matrix" -> workAug
      |>];
    ],
    {i, 1, n}
  ];

  <|
    "FinalAug" -> workAug,
    "Trace" -> trace
  |>
];

inverseEliminationWithinBoundsQ[aug_, pivotMode_: "MIN"] := Module[
  {traceData, limit},

  limit = $MaxBounds;
  traceData = collectInverseEliminationTrace[aug, pivotMode];

  AllTrue[
    traceData["Trace"],
    matrixMaxAbs[#["Matrix"]] <= limit &
  ]
];

luSolveData[A_, b_] := Module[
  {n, L, U, y, x, i, j, terms, sumTerm, pivotValue},

  n = Length[A];
  L = IdentityMatrix[n];
  U = ConstantArray[0, {n, n}];
  y = ConstantArray[0, n];
  x = ConstantArray[0, n];

  Do[
    Do[
      terms = Table[L[[i, k]]*U[[k, j]], {k, 1, i - 1}];
      sumTerm = Total[terms];
      U[[i, j]] = Together[A[[i, j]] - sumTerm];
      ,
      {j, i, n}
    ];

    pivotValue = Together[U[[i, i]]];
    If[pivotValue === 0, Return[$Failed]];

    Do[
      terms = Table[L[[j, k]]*U[[k, i]], {k, 1, i - 1}];
      sumTerm = Total[terms];
      L[[j, i]] = Together[(A[[j, i]] - sumTerm)/pivotValue];
      ,
      {j, i + 1, n}
    ];
    ,
    {i, 1, n}
  ];

  Do[
    terms = Table[L[[i, k]]*y[[k]], {k, 1, i - 1}];
    sumTerm = Total[terms];
    y[[i]] = Together[b[[i]] - sumTerm];
    ,
    {i, 1, n}
  ];

  Do[
    pivotValue = Together[U[[i, i]]];
    If[pivotValue === 0, Return[$Failed]];

    terms = Table[U[[i, k]]*x[[k]], {k, i + 1, n}];
    sumTerm = Total[terms];
    x[[i]] = Together[(y[[i]] - sumTerm)/pivotValue];
    ,
    {i, n, 1, -1}
  ];

  <|"L" -> L, "U" -> U, "Y" -> y, "X" -> x|>
];

luDecompositionWithinBoundsQ[data_Association] := Module[
  {luData, limit},

  limit = $MaxBounds;
  luData = luSolveData[data["A"], data["b"]];

  If[luData === $Failed,
    Return[False]
  ];

  AllTrue[
    {data["A"], data["b"], luData["L"], luData["U"], luData["Y"], luData["X"]},
    matrixMaxAbs[#] <= limit &
  ]
];

generateDataWithBounds[
  diff_String,
  n_Integer,
  solType_,
  triType_,
  scrambleFn_,
  pivotMode_: "ZERO",
  boundAugFn_: Automatic,
  boundCheckFn_: Automatic
] := Module[
  {data, retries = 0, augForCheck, resolvedBoundAugFn, resolvedBoundCheckFn},

  resolvedBoundAugFn = If[
    boundAugFn === Automatic,
    Function[d, d["Aug"]],
    boundAugFn
  ];

  resolvedBoundCheckFn = If[
    boundCheckFn === Automatic,
    forwardEliminationWithinBoundsQ,
    boundCheckFn
  ];

  While[retries < $MaxRetryCount,
    data = generateData[diff, n, solType, triType, scrambleFn];
    augForCheck = resolvedBoundAugFn[data];

    If[TrueQ[resolvedBoundCheckFn[augForCheck, pivotMode]],
      Return[Append[data, "RetryCount" -> retries]]
    ];

    retries++;
  ];

  $Failed
];

kSetTri := nonzeroRange[-4, 7];
kSetGauss := nonzeroRange[-2, 3];

lowerNonzeroCount[m_] := Count[LowerTriangularize[m, -1], x_ /; x =!= 0, {2}];

(* vytvorenie vyriešenej augmentovanej matice *)
makeDiagonalAug[diff_String, n_Integer, solType_String, triType_String] := Module[
  {A, b, x, idx, paramIdx, badRow, rhsNonzero},

  rhsNonzero = DeleteCases[Range[$bRange[[1]], $bRange[[2]]], 0];

  (* štart: I|b *)
  A = IdentityMatrix[n];
  b = RandomInteger[$bRange, n];
  x = b;

  (* pri smere "↓" dávame špeciálny riadok dole *)
  idx = n;
  paramIdx = Missing["NotApplicable"];
  badRow = Missing["NotApplicable"];

  Switch[solType,
    "ONE", Null
    ,
    "INFINITE",
    (* posledný riadok 0 = 0 *)
    A[[idx]] = ConstantArray[0, n];
    b[[idx]] = 0;

    Module[{paramCol, rows, k},
      paramCol = RandomInteger[{1, n}];
      rows = RandomSample[Range[1, n - 1], RandomInteger[{1, Max[1, n - 1]}]];
      Do[
        k = RandomChoice[kSetTri];
        A[[r, paramCol]] = k;
        ,
        {r, rows}
      ];
      paramIdx = idx;
    ];

    x = "INFINITE";
    ,
    "NONE",
    (* posledný riadok 0 = c, c != 0 *)
    A[[idx]] = ConstantArray[0, n];
    b[[idx]] = RandomChoice[rhsNonzero];

    x = "NONE";
    badRow = idx;
  ];

  <|"Aug" -> augFromAb[A, b], "x" -> x, "BadRow" -> badRow, "ParamIdx" -> paramIdx|>
];

(* generovanie dát *)
generateData[diff_String, n_, solType_, triType_, scrambleFn_] := Module[{solved, augSolved, augTask, A, b, vars},
  solved = makeDiagonalAug[diff, n, solType, triType];
  augSolved = solved["Aug"];
  augTask = scrambleFn[diff, augSolved, triType, solType];

  A = augTask[[All, 1 ;; n]];
  b = augTask[[All, n + 1]];
  vars = buildVars[n];

  <|"A" -> A, "b" -> b, "x" -> solved["x"], "TriType" -> triType, "SolutionType" -> solType,
    "Aug" -> augTask, "SolvedAug" -> augSolved, "Vars" -> vars, "n" -> n,
    "BadRow" -> solved["BadRow"], "ParamIdx" -> solved["ParamIdx"], "Difficulty" -> diff|>
];

genScrambleTriang[diff_String, aug0_, triType_String, solType_String : "ONE", Gauss_ : True] := Module[{aug = aug0, n = Length[aug0], bnd, kSet, withinQ, protectedLastRowQ, chooseK, chooseS, i, r, k, s},
  bnd = $Bounds;
  kSet = If[TrueQ[Gauss], kSetGauss, kSetTri];
  withinQ[row_] := Max[Abs[row]] <= bnd;

  protectedLastRowQ[rowIdx_] := (solType === "NONE" || solType === "INFINITE") && (rowIdx === n);

  (* násobok - zvyšné koeficienty *)
  chooseK[target_, src_] := Module[{k0, cand, ks},
    k0 = RandomChoice[kSet];
    cand = target + k0*src; If[withinQ[cand], Return[k0]];
    cand = target - k0*src; If[withinQ[cand], Return[-k0]];
    ks = SortBy[kSet, Abs];
    Do[ cand = target + kk*src; If[withinQ[cand], Return[kk]]; cand = target - kk*src; If[withinQ[cand], Return[-kk]];
      , {kk, ks}
    ]; 0
  ];

  (* scaling - koeficient pivotu *)
  chooseS[row_] := Module[{s0, cand, ss},
    If[!TrueQ[Gauss], Return[1]];
    s0 = RandomChoice[kSet];
    cand = s0*row; If[withinQ[cand], Return[s0]];
    cand = -s0*row; If[withinQ[cand], Return[-s0]];
    ss = SortBy[kSet, Abs];
    Do[ cand = t*row; If[withinQ[cand], Return[t]]; cand = -t*row; If[withinQ[cand], Return[-t]];
      , {t, ss}
    ]; 1
  ];

  If[triType === "L",
    For[i = n, i >= 1, i--,
      If[solType =!= "NONE" || !TrueQ@contradictionRowQ[aug[[i]]],
        For[r = i + 1, r <= n, r++,
          If[protectedLastRowQ[r], Continue[]];
          k = chooseK[aug[[r]], aug[[i]]];
          If[k =!= 0, aug[[r]] = aug[[r]] + k*aug[[i]]];
        ]
      ];
      s = chooseS[aug[[i]]];
      If[s =!= 1, aug[[i]] = s*aug[[i]]];
    ],
    For[i = 1, i <= n, i++,
      If[solType =!= "NONE" || !TrueQ@contradictionRowQ[aug[[i]]],
        For[r = i - 1, r >= 1, r--,
          k = chooseK[aug[[r]], aug[[i]]];
          If[k =!= 0, aug[[r]] = aug[[r]] + k*aug[[i]]];
        ]
      ];
      s = chooseS[aug[[i]]];
      If[s =!= 1, aug[[i]] = s*aug[[i]]];
    ]
  ];
  aug
];

genScrambleGauss[diff_String, aug0_, triType_String, solType_String : "ONE"] := Module[{n, pairs, chosenPairs, kSet, bnd, maxAttempts, maxKTries, aug, r, i, k, rowNew, currentLower, okQ},
  n = Length[aug0];
  pairs = Flatten[Table[{r, i}, {i, 1, n - 1}, {r, i + 1, n}], 1];

  kSet = kSetGauss;
  bnd = $Bounds;

  maxAttempts = 40;   (* koľkokrát reštartovať celý scramble *)
  maxKTries = 5;     (* koľko rôznych k skúsiť pre jeden pár *)

  Do[
    aug = genScrambleTriang[diff, aug0, "U", solType, False];
    aug = Map[normalizeRow, aug];

    chosenPairs = RandomSample[pairs, Length[pairs]];

    Do[
      {r, i} = pair;
      okQ = False;
      Do[
        k = RandomChoice[kSet];
        rowNew = aug[[r]] + k aug[[i]];
        rowNew = normalizeRow[rowNew];
        If[Max[Abs[rowNew]] <= bnd, aug[[r]] = rowNew; okQ = True; Break[];];
        ,
        {t, 1, maxKTries}
      ];
      , {pair, chosenPairs}
    ];
    currentLower = lowerNonzeroCount[aug[[All, 1 ;; n]]];
    If[currentLower == Length[pairs], Return[aug]];
    , {attempt, 1, maxAttempts}
  ];
  aug
];

genScrambleLU[diff_String, aug0_, triType_String, solType_String : "ONE"] := Module[
  {n, x, L, U, A, b, valueLimit, diagLimit, lowerPool, upperPool, diagPool},

  n = Length[aug0];
  x = aug0[[All, n + 1]];

  valueLimit = Max[1, Quotient[$Bounds, Switch[diff, "EASY", 4, "MEDIUM", 6, "HARD", 8]]];  diagLimit = Max[2, valueLimit];

  lowerPool = Join[Range[-valueLimit, -1], Range[valueLimit], Range[valueLimit], Range[valueLimit]];
  upperPool = Join[Range[-valueLimit, -1], Range[valueLimit], Range[valueLimit], Range[valueLimit]];
  diagPool = DeleteCases[Range[-diagLimit, diagLimit], -1 | 0 | 1];

  L = IdentityMatrix[n];
  Do[
    L[[i, j]] = RandomChoice[lowerPool];
    , {i, 2, n}, {j, 1, i - 1}
  ];

  U = ConstantArray[0, {n, n}];
  Do[
    U[[i, i]] = RandomChoice[diagPool];
    Do[
      U[[i, j]] = RandomChoice[upperPool];
      , {j, i + 1, n}
    ];
    , {i, 1, n}
  ];

  A = Together[L . U];
  b = Together[A . x];

  augFromAb[A, b]
];

(* ~-~-~ STEP GENERATION ~-~-~ *)

stepsTriangular[data_Association] := Module[{content = {}, n, aug, vars, tri, st, order, addHeader, addText, addMatrix, addConclusion, addCheckHeader, notes, result, sol},
  n = data["n"]; aug = data["Aug"]; vars = data["Vars"];
  tri = data["TriType"]; st = data["SolutionType"];
  order = If[tri === "U", Range[n, 1, -1],Range[1, n]];

  addHeader[text_] := AppendTo[content, makeStepHeader[text]];
  addText[text_] := AppendTo[content, text];
  addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] := AppendTo[content, alignedAugmentedMatrix[m, rowNotes, hi]];
  addConclusion[lines_List] := (addHeader["Záver"]; Scan[addText, lines]);
  addCheckHeader[extra_List : {}] := (addHeader["Skúška správnosti"]; Scan[addText, extra]);

  addHeader["Prepis sústavy do augmentovanej matice"];
  addText["Sústavu najprv prepíšeme do augmentovanej matice. Od tohto momentu pracujeme už len s maticou a vykonávame ekvivalentné riadkové úpravy."];
  addMatrix[aug];

  result = Switch[st,
    "ONE",
    Module[{terms, solLocal},
      solLocal = ConstantArray[None, n];

      addHeader["Riadkové úpravy"];
      addText["Riadky upravujeme tak, aby v každom kroku zostala v riadku iba jedna nová neznáma. Najprv odstránime členy s už známymi premennými a potom (ak je to potrebné) riadok vydelíme pivotom, aby sme dostali jednoduchý tvar rovnice."];

      Do[
        terms = If[tri === "U",
          Select[Table[{j, -aug[[i, j]]}, {j, i + 1, n}], #[[2]] =!= 0 &],
          Select[Table[{j, -aug[[i, j]]}, {j, 1, i - 1}], #[[2]] =!= 0 &]
        ];

        Module[{before0, mid0, after0, notes1, notes2, hi1, hi2, hi3, p},

          before0 = aug;

          (* kombinácia (ak treba) *)
          If[terms =!= {},
            mid0 = rowApplyCombine[before0, i, terms];
            notes1 = ConstantArray["", n];
            notes1[[i]] = rowNoteCombine[i, terms];

            hi1 = <|"ActiveRow" -> i, "SourceRows" -> terms[[All, 1]], "PivotPos" -> {i, i}|>;
            hi2 = hi1;
            ,
            mid0 = before0;
            notes1 = ConstantArray["", n];
            hi1 = <|"ActiveRow" -> i, "PivotPos" -> {i, i}|>;
            hi2 = hi1;
          ];

          (* delenie (ak treba) *)
          p = mid0[[i, i]];
          If[p =!= 1,
            after0 = rowApplyDivide[mid0, i, p];
            notes2 = ConstantArray["", n];
            notes2[[i]] = rowNoteDivide[i, p];

            hi3 = <|"ActiveRow" -> i, "PivotPos" -> {i, i}, "GreenCells" -> {{i, i}, {i, n + 1}}|>;

            (* ak bola aj kombinácia, 3; inak stačia 2 stlpce *)
            If[terms =!= {},
              AppendTo[content, augRender3[before0, mid0, after0, notes1, notes2, hi1, hi2, hi3]],
              AppendTo[content, augRender2[mid0, after0, notes2, hi2, hi3]]
            ];

            aug = after0;
            ,
            (* bez delenia v medzikroku *)
            If[terms =!= {},
              AppendTo[content, augRender2[before0, mid0, notes1, hi1, hi2]];
              aug = mid0;
            ];
          ];
        ];

        solLocal[[i]] = aug[[i, n + 1]];
        AppendTo[content, Spacer[6]];
        AppendTo[content, highlightGrid @ Grid[
          {{tf[vars[[i]]], "=", tft[solLocal[[i]]]}},
          Alignment -> {{Right, Center, Left}}, BaseStyle -> {FontSize -> 16}
        ]];
        AppendTo[content, Spacer[6]];
        ,
        {i, order}
      ];

      addCheckHeader[{"Výpočet overíme porovnaním A \[CenterDot] x s pravou stranou b (po riadkoch)."}];
      content = Join[content, verificationSteps[data, solLocal]];

      <|"Solution" -> solLocal|>
    ],

    "NONE",
    Module[{badIdx},
      badIdx = data["BadRow"];

      addHeader["Analýza riadkov"];
      addText["Hľadáme riadok, v ktorom sú všetky koeficienty pri neznámych nulové, ale pravá strana je nenulová. Takýto riadok predstavuje spor tvaru 0 = k, kde k \[NotEqual] 0."];

      notes = ReplacePart[ConstantArray["", n], badIdx -> "SPOR: 0 = " <> ToString[aug[[badIdx, n + 1]]]];
      addMatrix[aug, notes, <|"ActiveRow" -> badIdx|>];
      addCheckHeader[{"Pri sústave bez riešenia nerobíme klasickú skúšku dosadením. Overíme, že spor je naozaj nevyhnutný pomocou Frobeniovej vety (porovnanie hodností)."}];
      content = Join[content, verificationStepsNone[data]];
      addConclusion[{"Sústava preto nemá riešenie."}];

      <|"Solution" -> "NONE"|>
    ],

    "INFINITE",
    Module[{paramIdx, solExprs, pivot, row, knownTerm},
      paramIdx = data["ParamIdx"];

      addHeader["Analýza riadkov"];
      addText["Ak sa objaví nulový riadok, znamená to, že jedna z premenných nie je určená jednoznačne. Túto premennú zvolíme ako voľný parameter a ostatné premenné vyjadríme pomocou neho."];

      notes = ReplacePart[ConstantArray["", n], paramIdx -> "nulový riadok -> parameter"];
      addMatrix[aug, notes, <|"ActiveRow" -> paramIdx|>];
      addText[Row[{"Premennú ", vars[[paramIdx]], " zvolíme za parameter ", TraditionalForm[\[FormalT]], "."}]];
      AppendTo[content, Spacer[6]];
      AppendTo[content,
        highlightGrid @ Grid[
          {{tf[vars[[paramIdx]]], "=", TraditionalForm[\[FormalT]]}},
          Alignment -> {{Right, Center, Left}},
          BaseStyle -> {FontSize -> 16}
        ]
      ];
      AppendTo[content, Spacer[6]];

      addHeader["Vyjadrenie ostatných premenných pomocou parametra"];

      solExprs = ConstantArray[0, n];
      solExprs[[paramIdx]] = \[FormalT];

      Do[
        If[i === paramIdx, Continue[]];

        row = aug[[i]];
        pivot = row[[i]];

        knownTerm = Total@Table[If[j === i, 0, row[[j]]*solExprs[[j]]], {j, 1, n}];

        solExprs[[i]] = Expand[(row[[n + 1]] - knownTerm)/pivot];

        notes = ConstantArray["", n];
        notes[[i]] = Row[{vars[[i]], " = ", TraditionalForm[solExprs[[i]]]}];

        addMatrix[
          aug,
          notes,
          <|
            "ActiveRow" -> i,
            "PivotPos" -> {i, i},
            "GreenCells" -> {{i, i}, {i, n + 1}}
          |>
        ];

        AppendTo[content, Spacer[6]];
        AppendTo[content,
          highlightGrid @ Grid[
            {{tf[vars[[i]]], "=", TraditionalForm[solExprs[[i]]]}},
            Alignment -> {{Right, Center, Left}},
            BaseStyle -> {FontSize -> 16}
          ]
        ];
        AppendTo[content, Spacer[6]];
        ,
        {i, order}
      ];


      addCheckHeader[{"Dosadíme parametrické riešenie do pôvodných rovníc. Po úprave musí v každom riadku vyjsť identita (napr. 0 = 0) pre ľubovoľné \[FormalT] \[Element] \[DoubleStruckCapitalZ]."}];
      content = Join[content, verificationStepsInfinite[data, solExprs]];

      addConclusion[{
        "Sústava má nekonečne veľa riešení v tvare:",
        Row[{"[", Row @ Riffle[solExprs, ", "], "], ", \[FormalT], " \[Element] ", Integers}]
      }];

      <|"Solution" -> "INFINITE"|>
    ]
  ];

  sol = result["Solution"];
  <|"Content" -> content, "Solution" -> sol|>
];

stepsGauss[data_Association] := Module[{content = {}, n, aug, vars, st, addHeader, addText, addMatrix, notes, before, after, kPivot, elimRes, pNow, idx, solLocal, paramIdx, tmp},
  n = data["n"]; aug = data["Aug"]; vars = data["Vars"]; st = data["SolutionType"];

  addHeader[text_] := AppendTo[content, makeStepHeader[text]];
  addText[text_] := AppendTo[content, text];
  addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] := AppendTo[content, alignedAugmentedMatrix[m, rowNotes, hi]];

  addHeader["Prepis sústavy do augmentovanej matice"];
  addText["Sústavu prepíšeme do augmentovanej matice a následne vykonáme Gaussovu elimináciu, aby sme zrušili prvky pod hlavnou diagonálou."];
  addMatrix[aug];

  addHeader["Dopredná eliminácia (na horný trojuholník)"];
  addText["Postupujeme po st\:013apcoch zľava doprava. V každom kroku zvolíme pivot (ak treba, prehodíme riadky) a následne rušíme prvky pod pivotom celočíselnými riadkovými úpravami. Koeficienty priebežne skracujeme pomocou gcd a riadky normalizujeme."];

  Do[
    kPivot = choosePivotRowIfZero[aug, i];
    If[kPivot =!= i,
      before = aug;
      after = rowApplySwap[before, i, kPivot];
      notes = ConstantArray["", n]; notes[[i]] = rowNoteSwap[i, kPivot];
      AppendTo[content, augRender2[
        before, after, notes,
        <|"ActiveRow" -> i, "SourceRows" -> {kPivot}, "PivotPos" -> {i, i}|>,
        <|"ActiveRow" -> kPivot, "SourceRows" -> {i}, "PivotPos" -> {i, i}|>
      ]];
      aug = after;
    ];

    pNow = aug[[i, i]];
    If[pNow === 0, Continue[]];

    Do[
      If[aug[[r, i]] =!= 0,
        before = aug;
        elimRes = rowApplyElimStable[before, r, i];
        aug = rowAppendElimStep[content, before, elimRes, r, i, n, <|"SourceRows" -> {i}|>];
      ], {r, i + 1, n}
    ], {i, 1, n - 1}
  ];

  addHeader["Tvar po Gaussovej eliminácii"];
  addText["Po doprednej eliminácii dostaneme hornú trojuholníkovú sústavu. Teraz môžeme určiť neznáme spätným dosadzovaním, začíname od posledného riadku."];
  addMatrix[aug, {}, <|"BoldDiagonal" -> True|>];

  If[st === "NONE",
    idx = FirstCase[Range[n], k_ /; aug[[k, k]] === 0 && aug[[k, n + 1]] =!= 0, Missing["NotFound"]];
    addText["Na diagonále sa nachádza nulový pivot a zároveň je v príslušnom riadku nenulová pravá strana. To vedie k rovnici tvaru 0 = k, kde k \[NotEqual] 0, takže sústava nemá riešenie."];
    If[IntegerQ[idx],
      notes = ConstantArray["", n]; notes[[idx]] = "pivot = 0";
      addMatrix[aug, notes, <|"ActiveRow" -> idx, "BoldDiagonal" -> True|>],
      addMatrix[aug, {}, <|"BoldDiagonal" -> True|>]
    ];

    addHeader["Skúška správnosti"];
    addText["Pri sústave bez riešenia nerobíme klasickú skúšku dosadením. Overíme, že spor je naozaj nevyhnutný pomocou Frobeniovej vety (porovnanie hodností)."];
    content = Join[content, verificationStepsNone[data]];

    addHeader["Záver"];
    addText["Sústava nemá riešenie."];

    Return[<|"Content" -> content, "Solution" -> "NONE"|>];
  ];

  If[st === "ONE",
    addHeader["Spätné dosadzovanie v rovniciach"];
    tmp = gaussBackSubstEquations[aug, vars, ConstantArray[0, n], None, content];
    solLocal = tmp[[1]];
    content = tmp[[2]];

    addHeader["Skúška správnosti"];
    addText["Overíme porovnaním A \[CenterDot] x s pravou stranou b (po riadkoch)."];
    content = Join[content, verificationSteps[data, solLocal]];
    Return[<|"Content" -> content, "Solution" -> solLocal|>];
  ];

  If[st === "INFINITE",
    paramIdx = FirstCase[Range[n], k_ /; aug[[k, k]] === 0 && aug[[k, n + 1]] === 0, None];

    addHeader["Spätné dosadzovanie s parametrom"];
    solLocal = ConstantArray[0, n];
    If[IntegerQ[paramIdx], solLocal[[paramIdx]] = \[FormalT]];
    tmp = gaussBackSubstEquations[aug, vars, solLocal, paramIdx, content];
    solLocal = tmp[[1]];
    content = tmp[[2]];

    addHeader["Skúška správnosti"];
    addText["Dosadíme parametrické riešenie do pôvodných rovníc. Po úprave musí v každom riadku vyjsť identita pre ľubovoľné \[FormalT] \[Element] \[DoubleStruckCapitalZ]."];
    content = Join[content, verificationStepsInfinite[data, solLocal]];

    addHeader["Záver"];
    addText["Sústava má nekonečne veľa riešení."];
    Return[<|"Content" -> content, "Solution" -> "INFINITE"|>];
  ];


  <|"Content" -> content, "Solution" -> aug[[All, n + 1]]|>
];

stepsGaussJordan[data_Association, pivotQ_?BooleanQ] := Module[
  {content = {}, n, aug, vars, st, addHeader, addText, addMatrix, notes, before, after,
    kPivot, elimRes, pNow, solLocal, paramIdx, solExprs, row, pivot, knownTerm, pivotRowFn},

  n = data["n"]; aug = data["Aug"]; vars = data["Vars"]; st = data["SolutionType"];

  pivotRowFn = If[pivotQ, choosePivotRow, choosePivotRowIfZero];

  addHeader[text_] := AppendTo[content, makeStepHeader[text]];
  addText[text_] := AppendTo[content, text];
  addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] := AppendTo[content, alignedAugmentedMatrix[m, rowNotes, hi]];

  addHeader["Prepis sústavy do augmentovanej matice"];
  addText["Sústavu prepíšeme do augmentovanej matice a vykonáme Gaussovu-Jordanovu elimináciu tak, aby sa ľavá časť zmenila na jednotkovú maticu."];
  addMatrix[aug];

  addHeader["Dopredná eliminácia (nulovanie pod diagonálou)"];
  addText[
    If[pivotQ,
      "Postupujeme po stĺpcoch zľava doprava. V každom stĺpci vyberieme pivot ako nenulový prvok s najmenšou absolútnou hodnotou a prípadne prehodíme riadky. Pomocou pivotového riadku potom nulujeme prvky pod ním celočíselnými úpravami. Po každom kroku riadky skracujeme pomocou najväčšieho spoločného deliteľa (gcd) a normalizujeme.",
      "Postupujeme po stĺpcoch zľava doprava. Ak je aktuálny pivot nulový, prehodíme riadok s niektorým nižším riadkom, ktorý má v danom stĺpci nenulový prvok. Potom pomocou pivotového riadku nulujeme prvky pod ním celočíselnými úpravami. Po každom kroku riadky skracujeme pomocou najväčšieho spoločného deliteľa (gcd) a normalizujeme."
    ]
  ];

  Do[
    kPivot = pivotRowFn[aug, i];
    If[kPivot =!= i,
      before = aug;
      after = rowApplySwap[before, i, kPivot];
      notes = ConstantArray["", n]; notes[[i]] = rowNoteSwap[i, kPivot];
      AppendTo[content, augRender2[
        before, after, notes,
        <|"ActiveRow" -> i, "SourceRows" -> {kPivot}, "PivotPos" -> {i, i}|>,
        <|"ActiveRow" -> kPivot, "SourceRows" -> {i}, "PivotPos" -> {i, i}|>
      ]];
      aug = after;
    ];

    pNow = aug[[i, i]];
    If[pNow === 0, Continue[]];

    Do[
      If[aug[[r, i]] =!= 0,
        before = aug;
        elimRes = rowApplyElimStable[before, r, i];
        aug = rowAppendElimStep[content, before, elimRes, r, i, n, <|"SourceRows" -> {i}|>];
      ],
      {r, i + 1, n}
    ],
    {i, 1, n - 1}
  ];

  addHeader["Spätná eliminácia (nulovanie nad diagonálou)"];
  addText["Potom zrušíme prvky nad diagonálou, aby sme v ľavej časti dostali jednotkovú maticu."];

  Do[
    pNow = aug[[i, i]];
    If[pNow === 0, Continue[]];

    Do[
      If[aug[[r, i]] =!= 0,
        before = aug;
        elimRes = rowApplyElimStable[before, r, i];
        aug = rowAppendElimStep[content, before, elimRes, r, i, n, <||>];
      ],
      {r, 1, i - 1}
    ],
    {i, n, 2, -1}
  ];

  If[st === "NONE",
    Module[{badIdx},
      badIdx = findContradictionRow[aug];
      addHeader["Analýza riadkov"];
      addText["Hľadáme riadok, v ktorom sú všetky koeficienty pri neznámych nulové, ale pravá strana je nenulová. Takýto riadok znamená spor tvaru 0 = k, kde k \[NotEqual] 0."];
      notes = ConstantArray["", n];
      If[IntegerQ[badIdx], notes[[badIdx]] = "SPOR: 0 = " <> ToString[aug[[badIdx, n + 1]]]];
      addMatrix[aug, notes, <|"ActiveRow" -> If[IntegerQ[badIdx], badIdx, None]|>];

      addHeader["Skúška správnosti"];
      addText["Pri sústave bez riešenia nerobíme klasickú skúšku dosadením. Overíme pomocou Frobeniovej vety (porovnanie hodností)."];
      content = Join[content, verificationStepsNone[data]];

      addHeader["Záver"];
      addText["Sústava nemá riešenie."];

      Return[<|"Content" -> content, "Solution" -> "NONE"|>];
    ];
  ];

  addHeader["Normalizácia pivotov na 1"];

  Do[
    pNow = aug[[i, i]];
    If[pNow === 0, Continue[]];

    If[pNow =!= 1,
      before = aug;
      after = rowApplyDivide[before, i, pNow];
      notes = ConstantArray["", n]; notes[[i]] = rowNoteDivide[i, pNow];
      AppendTo[content, augRender2[
        before, after, notes,
        <|"ActiveRow" -> i, "PivotPos" -> {i, i}|>,
        <|"ActiveRow" -> i, "PivotPos" -> {i, i}, "GreenCells" -> {{i, i}, {i, n + 1}}|>
      ]];
      aug = after,
      addMatrix[aug, ConstantArray["", n], <|"ActiveRow" -> i, "PivotPos" -> {i, i}, "GreenCells" -> {{i, i}, {i, n + 1}}|>]
    ];

    AppendTo[content, Spacer[6]];
    AppendTo[content, highlightGrid @ Grid[
      {{tf[vars[[i]]], "=", tft[aug[[i, n + 1]]] }},
      Alignment -> {{Right, Center, Left}},
      BaseStyle -> {FontSize -> 16}
    ]];
    AppendTo[content, Spacer[6]];
    ,
    {i, 1, n}
  ];

  addHeader["Hotový tvar (I | x)"];
  addMatrix[aug];

  If[st === "INFINITE",
    paramIdx = data["ParamIdx"];

    addHeader["Analýza riadkov"];
    addText["Ak sa objaví nulový riadok, znamená to, že jedna z premenných nie je určená jednoznačne. Túto premennú zvolíme ako voľný parameter a ostatné premenné vyjadríme pomocou neho."];
    notes = ReplacePart[ConstantArray["", n], paramIdx -> "nulový riadok -> parameter"];
    addMatrix[aug, notes, <|"ActiveRow" -> paramIdx|>];

    addText[Row[{"Premennú ", vars[[paramIdx]], " zvolíme za parameter ", TraditionalForm[\[FormalT]], " a ponecháme ju v riešení ako symbol."}]];
    AppendTo[content, Spacer[6]];
    AppendTo[content, highlightGrid @ Grid[
      {{tf[vars[[paramIdx]]], "=", TraditionalForm[\[FormalT]]}},
      Alignment -> {{Right, Center, Left}},
      BaseStyle -> {FontSize -> 16}
    ]];
    AppendTo[content, Spacer[6]];

    addHeader["Vyjadrenie ostatných premenných pomocou parametra"];

    solExprs = ConstantArray[0, n];
    solExprs[[paramIdx]] = \[FormalT];

    Do[
      If[i === paramIdx, Continue[]];
      row = aug[[i]];
      pivot = row[[i]];
      If[pivot === 0, Continue[]];

      knownTerm = Total@Table[If[j === i, 0, row[[j]]*solExprs[[j]]], {j, 1, n}];
      solExprs[[i]] = Expand[(row[[n + 1]] - knownTerm)/pivot];

      notes = ConstantArray["", n];
      notes[[i]] = Row[{vars[[i]], " = ", TraditionalForm[solExprs[[i]]]}];

      addMatrix[
        aug,
        notes,
        <|"ActiveRow" -> i, "PivotPos" -> {i, i}, "GreenCells" -> {{i, i}, {i, n + 1}}|>
      ];

      AppendTo[content, Spacer[6]];
      AppendTo[content, highlightGrid @ Grid[
        {{tf[vars[[i]]], "=", TraditionalForm[solExprs[[i]]] }},
        Alignment -> {{Right, Center, Left}},
        BaseStyle -> {FontSize -> 16}
      ]];
      AppendTo[content, Spacer[6]];
      ,
      {i, n, 1, -1}
    ];

    addHeader["Skúška správnosti"];
    addText["Dosadíme parametrické riešenie do pôvodných rovníc. Po úprave musí v každom riadku vyjsť identita (napr. 0 = 0) pre ľubovoľné \[FormalT] \[Element] \[DoubleStruckCapitalZ]."];
    content = Join[content, verificationStepsInfinite[data, solExprs]];

    addHeader["Záver"];
    addText["Sústava má nekonečne veľa riešení."];

    Return[<|"Content" -> content, "Solution" -> "INFINITE"|>];
  ];

  solLocal = aug[[All, n + 1]];

  addHeader["Skúška správnosti"];
  addText["Overíme porovnaním A \[CenterDot] x s pravou stranou b (po riadkoch)."];
  content = Join[content, verificationSteps[data, solLocal]];

  <|"Content" -> content, "Solution" -> solLocal|>
];

stepsInverseMatrix[data_Association] := Module[
  {content = {}, n, A, b, vars, augInv, invMatrix, xResult,
    addHeader, addText, addMatrix, notes, before, after, kPivot, elimRes, pNow},

  n = data["n"];
  A = data["A"];
  b = data["b"];
  vars = data["Vars"];

  addHeader[text_] := AppendTo[content, makeStepHeader[text]];
  addText[text_] := AppendTo[content, text];
  addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] := AppendTo[content, alignedAugmentedMatrixInverse[m, rowNotes, hi]];

  addHeader["Prepis matice do tvaru (A | E)"];
  addText["Na výpočet inverznej matice použijeme Gaussovu-Jordanovu metódu. Na ľavej strane zapíšeme pôvodnú maticu A a na pravej strane jednotkovú maticu E. Rovnaké elementárne riadkové úpravy, ktoré prevedú ľavú časť na E, prevedú pravú časť na A^(-1)."];

  augInv = Join[A, IdentityMatrix[n], 2];
  addMatrix[augInv];

  addHeader["Dopredná eliminácia (nulovanie pod diagonálou)"];
  addText["Postupujeme po stĺpcoch zľava doprava. V každom stĺpci vyberieme pivot ako nenulový prvok s najmenšou absolútnou hodnotou a prípadne prehodíme riadky. Pomocou pivotového riadku potom nulujeme prvky pod ním celočíselnými riadkovými úpravami. Koeficienty priebežne skracujeme pomocou gcd a riadky normalizujeme."];

  Do[
    kPivot = choosePivotRow[augInv, i];

    If[kPivot =!= i,
      before = augInv;
      after = rowApplySwap[before, i, kPivot];
      notes = ConstantArray["", n];
      notes[[i]] = rowNoteSwap[i, kPivot];
      AppendTo[content, augRender2Inverse[
        before, after, notes,
        <|"ActiveRow" -> i, "SourceRows" -> {kPivot}, "PivotPos" -> {i, i}|>,
        <|"ActiveRow" -> kPivot, "SourceRows" -> {i}, "PivotPos" -> {i, i}|>
      ]];
      augInv = after;
    ];

    pNow = augInv[[i, i]];
    If[pNow === 0, Continue[]];

    Do[
      If[augInv[[r, i]] =!= 0,
        before = augInv;
        elimRes = rowApplyElimStable[before, r, i];
        augInv = rowAppendElimStepInverse[content, before, elimRes, r, i, n, <|"SourceRows" -> {i}|>];
      ],
      {r, i + 1, n}
    ];
    ,
    {i, 1, n - 1}
  ];

  addHeader["Spätná eliminácia (nulovanie nad diagonálou)"];
  addText["Potom zrušíme prvky nad diagonálou, aby sme v ľavej časti dostali diagonálny tvar a pripravili maticu na poslednú normalizáciu pivotov."];

  Do[
    pNow = augInv[[i, i]];
    If[pNow === 0, Continue[]];

    Do[
      If[augInv[[r, i]] =!= 0,
        before = augInv;
        elimRes = rowApplyElimStable[before, r, i];
        augInv = rowAppendElimStepInverse[content, before, elimRes, r, i, n, <||>];
      ],
      {r, 1, i - 1}
    ];
    ,
    {i, n, 2, -1}
  ];

  addHeader["Normalizácia pivotov na 1"];
  addText["Nakoniec vydelíme každý riadok jeho pivotom. Tým dostaneme na ľavej strane jednotkovú maticu E a na pravej strane hľadanú inverznú maticu A^(-1)."];

  Do[
    pNow = augInv[[i, i]];
    If[pNow === 0, Continue[]];

    If[pNow =!= 1,
      before = augInv;
      after = rowApplyDivide[before, i, pNow];
      notes = ConstantArray["", n];
      notes[[i]] = rowNoteDivide[i, pNow];
      AppendTo[content, augRender2Inverse[
        before, after, notes,
        <|"ActiveRow" -> i, "PivotPos" -> {i, i}|>,
        <|"ActiveRow" -> i, "PivotPos" -> {i, i}, "GreenCells" -> Table[{i, j}, {j, 1, 2 n}]|>
      ]];
      augInv = after
    ];
    ,
    {i, 1, n}
  ];

  addHeader["Hotový tvar (E | A^(-1))"];
  addText["Ľavá časť je teraz jednotková matica. Pravá časť preto predstavuje inverznú maticu A^(-1)."];
  addMatrix[augInv, {}, <|"BoldIdentity" -> True|>];

  invMatrix = augInv[[All, n + 1 ;; 2 n]];

  AppendTo[content, Spacer[8]];
  AppendTo[content,
    highlightGrid @ Grid[
      {{Style["A^(-1) =", Bold, FontSize -> 16], TraditionalForm[MatrixForm[invMatrix]]}},
      Alignment -> {{Right, Left}},
      Spacings -> {2, 1}
    ]
  ];
  AppendTo[content, Spacer[8]];

  addHeader["Výpočet riešenia x = A^(-1) · b"];
  addText["Keď už poznáme inverznú maticu, riešenie pôvodnej sústavy dostaneme vynásobením x = A^(-1) · b."];

  xResult = invMatrix . b;

  AppendTo[content, highlightGrid @ Grid[
    {{
      Style["x =", Bold],
      TraditionalForm[MatrixForm[invMatrix]],
      Style["·", Bold],
      TraditionalForm[MatrixForm[b]],
      Style["=", Bold],
      TraditionalForm[MatrixForm[xResult]]
    }},
    Alignment -> Center,
    Spacings -> {1, 1}
  ]];

  AppendTo[content, Spacer[8]];
  AppendTo[content, highlightGrid @ Grid[
    Table[{tf[vars[[i]]], "=", tft[xResult[[i]]]}, {i, 1, n}],
    Alignment -> {{Right, Center, Left}},
    BaseStyle -> {FontSize -> 16}
  ]];
  AppendTo[content, Spacer[8]];

  addHeader["Skúška správnosti"];
  addText["Overíme najprv, že A · A^(-1) = E. Potom ešte skontrolujeme, že pre vypočítané x platí A · x = b."];

  Module[{product, isIdentity},
    product = Together[A . invMatrix];
    isIdentity = product === IdentityMatrix[n];

    AppendTo[content, Grid[
      {{
        Style["A · A^(-1) =", FontSize -> 13],
        TraditionalForm[MatrixForm[product]],
        If[isIdentity, Style["OK", Darker[Green], Bold], Style["CHYBA", Red, Bold]]
      }},
      Alignment -> Left,
      Spacings -> {1, 0.4},
      BaseStyle -> {FontSize -> 13}
    ]];
  ];

  AppendTo[content, Spacer[6]];
  content = Join[content, verificationSteps[data, xResult]];

  addHeader["Záver"];
  addText["Inverzná matica bola úspešne vypočítaná pomocou Gaussovej-Jordanovej metódy. Riešenie sústavy sme následne dostali zo vzťahu x = A^(-1) · b."];

  <|"Content" -> content, "Solution" -> xResult, "InverseMatrix" -> invMatrix|>
];

stepsLU[data_Association] := Module[{content = {}, n, A, b, vars, luData, L, U, y, x, addHeader, addText, addMatrixPair, addVector, addFormula, i, j, terms, sumTerm, pivotValue, luProduct, lowerCheck, upperCheck, xSymbols, lhsRow},
  n = data["n"];
  A = data["A"];
  b = data["b"];
  vars = data["Vars"];
  xSymbols = vars;

  addHeader[text_] := AppendTo[content, makeStepHeader[text]];
  addText[text_] := AppendTo[content, text];
  addMatrixPair[l_, u_] := AppendTo[content, luMatrixPairGrid[l, u]];
  addVector[label_, vec_] := AppendTo[content, luVectorGrid[label, vec]];
  addFormula[expr_] := AppendTo[content, expr];

  addHeader["Prepis sústavy do maticového tvaru"];
  addText["Sústavu najprv prepíšeme do maticového tvaru A · x = b. Pri Doolittleho LU rozklade chceme maticu A rozložiť na súčin A = L · U, kde L je dolná trojuholníková matica s jednotkami na diagonále a U je horná trojuholníková matica."];
  AppendTo[content, highlightGrid @ Grid[
    {{
      Style["A =", Bold, FontSize -> 16],
      TraditionalForm[MatrixForm[A]],
      Spacer[18],
      Style["x =", Bold, FontSize -> 16],
      TraditionalForm[MatrixForm[xSymbols]],
      Spacer[18],
      Style["b =", Bold, FontSize -> 16],
      TraditionalForm[MatrixForm[b]]
    }},
    Alignment -> Left,
    Spacings -> {2, 1}
  ]];
  AppendTo[content, highlightGrid @ Grid[
    {
      {Style["A · x = b", Bold]},
      {Style["A = L · U  ⇒  L · U · x = b", Bold]},
      {Style["Označme U · x = y.", Bold]},
      {Style["Potom riešime najprv L · y = b a následne U · x = y.", Bold]}
    },
    Alignment -> Left,
    Spacings -> {1, 0.6}
  ]];

  luData = luSolveData[A, b];

  (* debug - vypisanie L a U *)
  Print[Row[{"L = ", MatrixForm[luData["L"]]}]];
  Print[Row[{"U = ", MatrixForm[luData["U"]]}]];


  If[luData === $Failed,
    addHeader["Záver"];
    addText["Pri tejto matici sa počas Doolittleho rozkladu objavil nulový pivot, takže LU rozklad bez pivotovania nemožno použiť."];
    Return[<|"Content" -> content, "Solution" -> Missing["NotAvailable"]|>];
  ];

  L = IdentityMatrix[n];
  U = ConstantArray[0, {n, n}];

  addHeader["Všeobecný tvar matíc L a U"];
  addText["Pri Doolittleho rozklade má matica L na diagonále samé jednotky. Neznáme prvky pod diagonálou označíme l_(i,j) a neznáme prvky matice U označíme u_(i,j)."];
  AppendTo[content, luGeneralMatricesGrid[n]];

  addHeader["Inicializácia matíc"];
  addText["Na začiatku poznáme len to, že L má jednotkovú diagonálu. Ostatné prvky budeme dopočítavať postupne po riadkoch matice U a po stĺpcoch matice L."];
  addMatrixPair[L, U];

  addHeader["Výpočet rozkladu A = L · U"];
  addText["V každom kroku najprv vypočítame prvky i-teho riadku matice U a potom prvky i-teho stĺpca matice L pod diagonálou. Pri každom prvku si ukážeme všeobecný vzorec, konkrétne dosadenie aj výsledok."];

  Do[
    addHeader["Výpočet " <> ToString[i] <> ". riadku matice U a " <> ToString[i] <> ". stĺpca matice L"];
    addText["Najprv určíme prvky u_(" <> ToString[i] <> ",j), potom s použitím pivotu u_(" <> ToString[i] <> "," <> ToString[i] <> ") dopočítame prvky l_(j," <> ToString[i] <> ") pod diagonálou."];

    Do[
      terms = Table[{L[[i, k]], U[[k, j]]}, {k, 1, i - 1}];
      sumTerm = Total[Times @@@ terms];
      U[[i, j]] = Together[A[[i, j]] - sumTerm];

      addText["Výpočet prvku matice U:"];
      addFormula[luFormulaUGeneral[i, j]];
      addFormula[
        If[terms === {},
          Row[{luEntrySymbol["u", i, j], " = ", tft[A[[i, j]]], " = ", tft[U[[i, j]]]}],
          Row[{
            luEntrySymbol["u", i, j], " = ", tft[A[[i, j]]],
            " - (", luSumDisplay[terms], ") = ", tft[U[[i, j]]]
          }]
        ]
      ];
      AppendTo[content, highlightGrid @ Grid[
        {{luEntrySymbol["u", i, j], "=", tft[U[[i, j]]]}},
        Alignment -> {{Right, Center, Left}},
        BaseStyle -> {FontSize -> 16}
      ]];
      ,
      {j, i, n}
    ];

    pivotValue = Together[U[[i, i]]];
    addText["Z diagonálneho prvku dostávame pivot tohto kroku:"];
    AppendTo[content, highlightGrid @ Grid[
      {{luEntrySymbol["u", i, i], "=", tft[pivotValue]}},
      Alignment -> {{Right, Center, Left}},
      BaseStyle -> {FontSize -> 16}
    ]];

    Do[
      terms = Table[{L[[j, k]], U[[k, i]]}, {k, 1, i - 1}];
      sumTerm = Total[Times @@@ terms];
      L[[j, i]] = Together[(A[[j, i]] - sumTerm)/pivotValue];

      addText["Výpočet prvku matice L:"];
      addFormula[luFormulaLGeneral[j, i]];
      addFormula[
        If[terms === {},
          Row[{
            luEntrySymbol["l", j, i], " = ", tft[A[[j, i]]], "/", tft[pivotValue],
            " = ", tft[L[[j, i]]]
          }],
          Row[{
            luEntrySymbol["l", j, i], " = (", tft[A[[j, i]]],
            " - (", luSumDisplay[terms], "))/", tft[pivotValue],
            " = ", tft[L[[j, i]]]
          }]
        ]
      ];
      AppendTo[content, highlightGrid @ Grid[
        {{luEntrySymbol["l", j, i], "=", tft[L[[j, i]]]}},
        Alignment -> {{Right, Center, Left}},
        BaseStyle -> {FontSize -> 16}
      ]];
      ,
      {j, i + 1, n}
    ];

    addText["Po dokončení tohto kroku majú matice L a U tvar:"];
    addMatrixPair[L, U];
    ,
    {i, 1, n}
  ];

  addHeader["Hotový rozklad A = L · U"];
  addText["Po dokončení výpočtu máme maticu L s jednotkovou diagonálou a hornú trojuholníkovú maticu U."];
  addMatrixPair[L, U];

  luProduct = Together[L . U];
  addText["Každý prvok súčinu L · U vzniká ako skalárny súčin príslušného riadku matice L a stĺpca matice U. Najprv si ukážeme dosadenie, potom vypočítané súčiny a nakoniec výsledný prvok."];
  AppendTo[content, luMatrixProductDisplay[L, U]];
  AppendTo[content, Grid[
    {{
      Style["L · U =", FontSize -> 13],
      TraditionalForm[MatrixForm[luProduct]],
      If[luProduct === A, Style["OK", Darker[Green], Bold], Style["CHYBA", Red, Bold]]
    }},
    Alignment -> Left,
    Spacings -> {1, 0.4},
    BaseStyle -> {FontSize -> 13}
  ]];

  addHeader["Riešenie pomocnej sústavy L · y = b"];
  addText["Keďže L je dolná trojuholníková matica s jednotkami na diagonále, pomocný vektor y určujeme dopredným dosadzovaním zhora nadol. V každom riadku najprv zapíšeme rovnicu, potom dosadíme už známe hodnoty a nakoniec vyjadríme novú neznámu."];

  y = ConstantArray[0, n];
  Do[
    terms = Table[{L[[i, k]], y[[k]]}, {k, 1, i - 1}];
    sumTerm = Total[Times @@@ terms];
    y[[i]] = Together[b[[i]] - sumTerm];

    lhsRow = luEquationForwardDisplay[L[[i]], b[[i]], Table[luScalarSymbol["y", k], {k, 1, n}], i];
    addText["Rovnica z " <> ToString[i] <> ". riadku:"];
    addFormula[lhsRow];
    addFormula[
      If[terms === {},
        Row[{luScalarSymbol["y", i], " = ", tft[b[[i]]], " = ", tft[y[[i]]]}],
        Row[{
          luScalarSymbol["y", i], " = ", tft[b[[i]]],
          " - (", luSumDisplay[terms], ") = ", tft[y[[i]]]
        }]
      ]
    ];

    AppendTo[content, highlightGrid @ Grid[
      {{luScalarSymbol["y", i], "=", tft[y[[i]]]}},
      Alignment -> {{Right, Center, Left}},
      BaseStyle -> {FontSize -> 16}
    ]];
    ,
    {i, 1, n}
  ];

  addVector["y", y];

  addHeader["Riešenie trojuholníkovej sústavy U · x = y"];
  addText["Po určení pomocného vektora y riešime hornú trojuholníkovú sústavu U · x = y spätným dosadzovaním od poslednej rovnice. Opäť vždy zapíšeme rovnicu, dosadenie známych hodnôt a výsledok."];
  AppendTo[content, alignedAugmentedMatrix[augFromAb[U, y], {}, <|"BoldDiagonal" -> True|>]];

  x = ConstantArray[0, n];
  Do[
    terms = Table[{U[[i, k]], x[[k]]}, {k, i + 1, n}];
    sumTerm = Total[Times @@@ terms];
    x[[i]] = Together[(y[[i]] - sumTerm)/U[[i, i]]];

    lhsRow = luEquationBackwardDisplay[U[[i]], y[[i]], vars, i, n];
    addText["Rovnica z " <> ToString[i] <> ". riadku:"];
    addFormula[lhsRow];
    addFormula[
      If[terms === {},
        Row[{vars[[i]], " = ", tft[y[[i]]], "/", tft[U[[i, i]]], " = ", tft[x[[i]]]}],
        Row[{
          vars[[i]], " = (", tft[y[[i]]], " - (", luSumDisplay[terms], "))/", tft[U[[i, i]]],
          " = ", tft[x[[i]]]
        }]
      ]
    ];

    AppendTo[content, highlightGrid @ Grid[
      {{tf[vars[[i]]], "=", tft[x[[i]]]}},
      Alignment -> {{Right, Center, Left}},
      BaseStyle -> {FontSize -> 16}
    ]];
    ,
    {i, n, 1, -1}
  ];

  addHeader["Skúška správnosti"];
  addText["Overíme najprv rozklad A = L · U, potom pomocnú sústavu L · y = b a nakoniec pôvodnú sústavu A · x = b."];

  lowerCheck = Together[L . y];
  upperCheck = Together[U . x];

  AppendTo[content, Grid[
    {{
      Style["L · U =", FontSize -> 13],
      TraditionalForm[MatrixForm[luProduct]],
      Style["A =", FontSize -> 13],
      TraditionalForm[MatrixForm[A]],
      If[luProduct === A, Style["OK", Darker[Green], Bold], Style["CHYBA", Red, Bold]]
    }},
    Alignment -> Left,
    Spacings -> {1, 0.4},
    BaseStyle -> {FontSize -> 13}
  ]];

  AppendTo[content, Spacer[6]];
  AppendTo[content, Grid[
    {{
      Style["L · y =", FontSize -> 13],
      TraditionalForm[MatrixForm[lowerCheck]],
      Style["b =", FontSize -> 13],
      TraditionalForm[MatrixForm[b]],
      If[lowerCheck === b, Style["OK", Darker[Green], Bold], Style["CHYBA", Red, Bold]]
    }},
    Alignment -> Left,
    Spacings -> {1, 0.4},
    BaseStyle -> {FontSize -> 13}
  ]];

  AppendTo[content, Spacer[6]];
  AppendTo[content, Grid[
    {{
      Style["U · x =", FontSize -> 13],
      TraditionalForm[MatrixForm[upperCheck]],
      Style["y =", FontSize -> 13],
      TraditionalForm[MatrixForm[y]],
      If[upperCheck === y, Style["OK", Darker[Green], Bold], Style["CHYBA", Red, Bold]]
    }},
    Alignment -> Left,
    Spacings -> {1, 0.4},
    BaseStyle -> {FontSize -> 13}
  ]];

  AppendTo[content, Spacer[6]];
  content = Join[content, verificationSteps[data, x]];

  addHeader["Záver"];
  addText["Sústava bola vyriešená pomocou Doolittleho LU rozkladu bez pivotovania. Najprv sme zostrojili rozklad A = L · U, potom sme vyriešili pomocnú sústavu L · y = b a napokon trojuholníkovú sústavu U · x = y."];

  <|
    "Content" -> content,
    "Solution" -> x,
    "L" -> L,
    "U" -> U,
    "Y" -> y
  |>
];

(* ~-~-~ VERIFICATION STEPS ~-~-~ *)

verificationSteps[data_Association, sol_List] := Module[{content = {}, A = data["A"], b = data["b"], n = data["n"], lhs},
  Do[
    lhs = A[[i]] . sol;
    AppendTo[content,
      Grid[
        {
          {Row[{"LS", i, ":  ", tf[A[[i]]], " \[CenterDot] ", tf[sol], " = ", tft[lhs]}]},
          {Row[{"PS", i, " = ", tft[b[[i]]]}]},
          {If[lhs === b[[i]], Style["LS = PS (OK)", Darker[Green]], Style["LS \[NotEqual] PS (CHYBA)", Red]]}
        },
        Alignment -> Left,
        Spacings -> {0, 0.4},
        BaseStyle -> {FontSize -> 13}
      ]
    ], {i, 1, n}
  ];

  content
];
verificationStepsNone[data_Association] := Module[{content = {}, A = data["A"], b = data["b"], aug0, rA, rAug, n, badIdx, rhsVal},

  n = Length[b];
  aug0 = augFromAb[A, b];
  rA = MatrixRank[A];
  rAug = MatrixRank[aug0];

  AppendTo[content,
    Grid[
      {
        {Row[{"hodnosť(A) = ", rA}]},
        {Row[{"hodnosť([A|b]) = ", rAug}]},
        {If[rA < rAug,
          Style["hodnosť(A) < hodnosť([A|b])  \[Rule]  sústava nemá riešenie (OK)", Darker[Green]],
          Style["hodnosti sa nerovnajú tak, ako majú pre spor - over postup (CHYBA)", Red]
        ]}
      },
      Alignment -> Left,
      Spacings -> {0, 0.4},
      BaseStyle -> {FontSize -> 13}
    ]
  ];
  content
];
verificationStepsInfinite[data_Association, solExprs_List] := Module[{content = {}, A = data["A"], b = data["b"], n = data["n"], lhs, diff, okQ, coeffs},
  Do[
    lhs = Together[A[[i]] . solExprs];
    diff = Together[lhs - b[[i]]];

    okQ = If[diff === 0,
      True,
      If[PolynomialQ[diff, \[FormalT]],
        coeffs = CoefficientList[Expand[diff], \[FormalT]];
        AllTrue[coeffs, # === 0 &],
        False
      ]
    ];

    AppendTo[content,
      Grid[
        {
          {Row[{"Riadok ", i, ":  ", tf[A[[i]]], " \[CenterDot] ", TraditionalForm[solExprs], " = ", TraditionalForm[lhs]}]},
          {Row[{"PS", i, " = ", TraditionalForm[b[[i]]]}]},
          {Row[{"LS - PS = ", TraditionalForm[diff]}]},
          {If[okQ, Style["LS = PS (OK)", Darker[Green]], Style["LS \[NotEqual] PS (CHYBA)", Red]]}
        },
        Alignment -> Left,
        Spacings -> {0, 0.4},
        BaseStyle -> {FontSize -> 13}
      ]
    ], {i, 1, n}
  ];

  content
];

verificationStepsInfiniteRank[data_Association] := Module[{content = {}, A = data["A"], b = data["b"], n, aug0, rA, rAug},
  n = Length[b];
  aug0 = augFromAb[A, b];
  rA = MatrixRank[A];
  rAug = MatrixRank[aug0];

  AppendTo[content,
    Grid[
      {
        {Row[{"hodnosť(A) = ", rA}]},
        {Row[{"hodnosť([A|b]) = ", rAug}]},
        {If[rA === rAug && rA < n,
          Style["hodnosť(A) = hodnosť([A|b]) < n  \[Rule]  sústava má nekonečne veľa riešení (OK)", Darker[Green]],
          Style["hodnosti nesp\:013aňajú podmienku pre nekonečne veľa riešení - over postup (CHYBA)", Red]
        ]}
      },
      Alignment -> Left, Spacings -> {0, 0.4}, BaseStyle -> {FontSize -> 13}
    ]
  ];
  content
];


(* ~-~-~ MAIN CONTROLLER ~-~-~ *)
printDefaultTask[data_Association, vars_List] := Module[{},
  printTextCell["Riešte sústavu rovníc v množine celých čísel."];
  printFormulaCell @ Grid[
    List /@ (tf /@ buildTaskEquations[data["A"], data["b"], vars]),
    Alignment -> Left,
    Spacings -> {0, 0.8}
  ];
  printTextCell["Riešte pomocou augmentovanej matice."];
];

printTaskInverse[data_Association, vars_List] := Module[{},
  printTextCell["Vypočítajte inverznú maticu k matici sústavy a následne pomocou nej určte riešenie sústavy v množine celých čísel."];
  printFormulaCell @ Grid[
    List /@ (tf /@ buildTaskEquations[data["A"], data["b"], vars]),
    Alignment -> Left,
    Spacings -> {0, 0.8}
  ];
  printTextCell["Použite postup cez augmentovanú maticu v tvare (A | E)."];
];

printTaskLU[data_Association, vars_List] := Module[{},
  printTextCell["Rozložte maticu sústavy pomocou LU rozkladu (Doolittle, bez pivotovania) v tvare A = L · U, kde L má jednotky na diagonále. Potom vyriešte pomocnú sústavu L · y = b a následne U · x = y."];
  printFormulaCell @ Grid[
    List /@ (tf /@ buildTaskEquations[data["A"], data["b"], vars]),
    Alignment -> Left,
    Spacings -> {0, 0.8}
  ];
  printTextCell["Pracujte priamo s maticami L a U bez pivotovania."];
];

printDefaultResult[data_Association, vars_List, st_] := Module[{},
  If[st === "ONE",
    printFormulaCell[
      Row[Flatten[{"(", Riffle[vars, ", "], ") = (", Riffle[TraditionalForm /@ data["x"], ", "], ")"}]]
    ]
  ];

  If[st === "NONE",
    printTextCell["Sústava nemá riešenie."]
  ];

  If[st === "INFINITE",
    printTextCell["Sústava má nekonečne veľa riešení."];
    Module[{solExprs = infiniteSolutionFromSolvedAug[data]},
      printFormulaCell[
        Row[{"K = { [", Row @ Riffle[TraditionalForm /@ solExprs, ", "], "], ", \[FormalT], " \[Element] ", Integers, " }"}]
      ];
    ];
  ];
];

printResultInverse[data_Association, vars_List, st_, steps_] := Module[
  {solution, invMatrix},

  solution = Which[
    AssociationQ[steps] && KeyExistsQ[steps, "Solution"], steps["Solution"],
    KeyExistsQ[data, "x"], data["x"],
    True, Missing["NotAvailable"]
  ];

  invMatrix = Which[
    AssociationQ[steps] && KeyExistsQ[steps, "InverseMatrix"], steps["InverseMatrix"],
    True, Missing["NotAvailable"]
  ];

  If[MatrixQ[invMatrix],
    printTextCell["Inverzná matica:"];
    printFormulaCell[TraditionalForm[MatrixForm[invMatrix]]];
  ];

  If[ListQ[solution],
    printTextCell["Riešenie sústavy:"];
    printFormulaCell[
      Row[Flatten[{"(", Riffle[vars, ", "], ") = (", Riffle[TraditionalForm /@ solution, ", "], ")"}]]
    ];
  ];
];

printResultLU[data_Association, vars_List, st_, steps_] := Module[
  {solution, lMatrix, uMatrix, yVector},

  solution = Which[
    AssociationQ[steps] && KeyExistsQ[steps, "Solution"], steps["Solution"],
    KeyExistsQ[data, "x"], data["x"],
    True, Missing["NotAvailable"]
  ];

  lMatrix = Which[
    AssociationQ[steps] && KeyExistsQ[steps, "L"], steps["L"],
    True, Missing["NotAvailable"]
  ];

  uMatrix = Which[
    AssociationQ[steps] && KeyExistsQ[steps, "U"], steps["U"],
    True, Missing["NotAvailable"]
  ];

  yVector = Which[
    AssociationQ[steps] && KeyExistsQ[steps, "Y"], steps["Y"],
    True, Missing["NotAvailable"]
  ];

  If[MatrixQ[lMatrix],
    printTextCell["Matica L:"];
    printFormulaCell[TraditionalForm[MatrixForm[lMatrix]]];
  ];

  If[MatrixQ[uMatrix],
    printTextCell["Matica U:"];
    printFormulaCell[TraditionalForm[MatrixForm[uMatrix]]];
  ];

  If[ListQ[yVector],
    printTextCell["Pomocný vektor y:"];
    printFormulaCell[TraditionalForm[MatrixForm[yVector]]];
  ];

  If[ListQ[solution],
    printTextCell["Riešenie sústavy:"];
    printFormulaCell[
      Row[Flatten[{"(", Riffle[vars, ", "], ") = (", Riffle[TraditionalForm /@ solution, ", "], ")"}]]
    ];
  ];
];

runMatrixGenerator[spec_Association, diff_String, mode_String, opts : OptionsPattern[]] := Module[
  {n, vars, st, tri, data, steps = Missing["NotComputed"], validateExtraQ, resolveExtra,
    sectionTitle, stepFn, scrambleFn, taskPrinter, resultPrinter, useRetryQ, pivotMode,
    boundAugFn, boundCheckFn},

  If[!TrueQ[ValidateDifficulty[diff]],
    Message[spec["MsgPrefix"]::baddiff, diff];
    Return[]
  ];

  If[!TrueQ[ValidateMode[mode]],
    Message[spec["MsgPrefix"]::badmode, mode];
    Return[]
  ];

  With[{stOpt = OptionValue[spec["EntryFn"], {opts}, SolutionType]},
    If[!TrueQ[ValidateSolutionType[stOpt]],
      Message[spec["MsgPrefix"]::badst, stOpt];
      Return[]
    ];
  ];

  validateExtraQ = Lookup[spec, "ValidateExtra", (True &)];
  If[!TrueQ[validateExtraQ[spec, {opts}]],
    Return[]
  ];

  st = ResolveSolutionType[OptionValue[spec["EntryFn"], {opts}, SolutionType]];

  resolveExtra = Lookup[spec, "ResolveExtra", (Missing["NotUsed"] &)];
  tri = resolveExtra[spec, {opts}];

  n = DimensionByDifficulty[diff];
  vars = buildVars[n];

  scrambleFn = spec["ScrambleFn"];
  useRetryQ = TrueQ @ Lookup[spec, "UseForwardBoundRetry", False];
  pivotMode = Lookup[spec, "ForwardPivotMode", "ZERO"];
  boundAugFn = Lookup[spec, "ForwardBoundAugFn", Automatic];
  boundCheckFn = Lookup[spec, "ForwardBoundCheckFn", Automatic];

  data = If[useRetryQ,
    generateDataWithBounds[diff, n, st, tri, scrambleFn, pivotMode, boundAugFn, boundCheckFn],
    generateData[diff, n, st, tri, scrambleFn]
  ];

  If[data === $Failed,
    Message[spec["MsgPrefix"]::fail];
    Return[]
  ];

  sectionTitle = spec["SectionTitle"];
  printSectionCell[sectionTitle];
  printSubsectionCell["Zadanie"];

  taskPrinter = Lookup[spec, "TaskPrinter", Automatic];
  If[taskPrinter === Automatic,
    printDefaultTask[data, vars],
    taskPrinter[data, vars]
  ];

  If[KeyExistsQ[data, "RetryCount"],
    printTextCell["Počet pregenerovaní: " <> ToString[data["RetryCount"]]];
  ];

  If[mode === "TASK_STEPS_RESULT",
    withStepCounter @ Function[Null,
      printSubsectionCell["Postup"];
      stepFn = Lookup[spec, "StepsFn", None];

      If[stepFn === None,
        printTextCell["Postup pre túto metódu zatiaľ nie je dopracovaný v tomto balíku."],
        steps = stepFn[data];
        Scan[renderStepItem, steps["Content"]];
      ];
    ]
  ];

  If[mode =!= "TASK",
    If[!(mode === "TASK_STEPS_RESULT" && TrueQ @ Lookup[spec, "InlineSolutionQ", False]),
      printSubsectionCell["Výsledok"];

      resultPrinter = Lookup[spec, "ResultPrinter", Automatic];
      If[resultPrinter === Automatic,
        printDefaultResult[data, vars, st],
        If[steps === Missing["NotComputed"] && mode === "TASK_RESULT",
          stepFn = Lookup[spec, "StepsFn", None];
          If[stepFn =!= None,
            steps = stepFn[data]
          ];
        ];
        resultPrinter[data, vars, st, steps]
      ];
    ];
  ];
];

GenTriangular[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenTriangular, "MsgPrefix" -> GenTriangular, "DimKey" -> "Triangular", "SectionTitle" -> "Trojuholníková metóda",
    "ScrambleFn" -> genScrambleTriang, "StepsFn" -> stepsTriangular,
    "ValidateExtra" -> Function[{specLocal, passedOpts},
      With[{triOpt = OptionValue[specLocal["EntryFn"], passedOpts, TriangularType]},
        If[!TrueQ[validateTriangularType[triOpt]],
          Message[specLocal["MsgPrefix"]::badtri, triOpt];
          False, True
        ]
      ]
    ],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, resolveTriangularType[OptionValue[specLocal["EntryFn"], passedOpts, TriangularType]]]
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

GenGauss[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenGauss,
    "MsgPrefix" -> GenGauss,
    "DimKey" -> "Gauss",
    "SectionTitle" -> "Gaussova eliminačná metóda",
    "ScrambleFn" -> genScrambleGauss,
    "StepsFn" -> stepsGauss,
    "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "UseForwardBoundRetry" -> True,
    "ForwardPivotMode" -> "ZERO"
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

GenGaussJordan[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenGaussJordan,
    "MsgPrefix" -> GenGaussJordan,
    "DimKey" -> "GaussJordan",
    "SectionTitle" -> "Gauss-Jordanova metóda",
    "ScrambleFn" -> genScrambleGauss,
    "StepsFn" -> (stepsGaussJordan[#, False] &),
    "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "UseForwardBoundRetry" -> True,
    "ForwardPivotMode" -> "ZERO"
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

GenGaussJordanPivot[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenGaussJordanPivot,
    "MsgPrefix" -> GenGaussJordanPivot,
    "DimKey" -> "GaussJordanPivot",
    "SectionTitle" -> "Gauss-Jordanova metóda s pivotovaním",
    "ScrambleFn" -> genScrambleGauss,
    "StepsFn" -> (stepsGaussJordan[#, True] &),
    "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "UseForwardBoundRetry" -> True,
    "ForwardPivotMode" -> "MIN"
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

GenInverse[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenInverse,
    "MsgPrefix" -> GenInverse,
    "DimKey" -> "Inverse",
    "SectionTitle" -> "Výpočet inverznej matice",
    "ScrambleFn" -> genScrambleGauss,
    "StepsFn" -> stepsInverseMatrix,
    "ValidateExtra" -> Function[{specLocal, passedOpts},
      With[{stOpt = OptionValue[specLocal["EntryFn"], passedOpts, SolutionType]},
        If[stOpt =!= "ONE",
          Message[specLocal["MsgPrefix"]::badst, stOpt];
          False,
          True
        ]
      ]
    ],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "TaskPrinter" -> printTaskInverse,
    "ResultPrinter" -> printResultInverse,
    "UseForwardBoundRetry" -> True,
    "ForwardPivotMode" -> "MIN",
    "ForwardBoundAugFn" -> Function[data, Join[data["A"], IdentityMatrix[data["n"]], 2]],
    "ForwardBoundCheckFn" -> inverseEliminationWithinBoundsQ
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

GenLU[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenLU,
    "MsgPrefix" -> GenLU,
    "DimKey" -> "LU",
    "SectionTitle" -> "LU rozklad (Doolittle)",
    "ScrambleFn" -> genScrambleLU,
    "StepsFn" -> stepsLU,
    "ValidateExtra" -> Function[{specLocal, passedOpts},
      With[{stOpt = OptionValue[specLocal["EntryFn"], passedOpts, SolutionType]},
        If[stOpt =!= "ONE",
          Message[specLocal["MsgPrefix"]::badst, stOpt];
          False,
          True
        ]
      ]
    ],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "TaskPrinter" -> printTaskLU,
    "ResultPrinter" -> printResultLU,
    "UseForwardBoundRetry" -> True,
    "ForwardBoundAugFn" -> Function[data, data],
    "ForwardBoundCheckFn" -> luDecompositionWithinBoundsQ
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

End[];
EndPackage[];
