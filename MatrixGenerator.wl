(* ::Package:: *)

BeginPackage["`MatrixGenerator`"];

$CharacterEncoding = "UTF-8";
Internal`$ContextMarks = False;

GenTriangular::usage = "GenTriangular[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc v trojuholníkovej sústave pomocou augmentovanej matice \
diff: \"EASY\" (3x3), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts:
  SolutionType -> Automatic | \"ONE\" | \"NONE\" | \"INFINITE\"   (Automatic vyberá typ náhodne)
  TriangularType -> Automatic | \"L\" | \"U\"";

GenGauss::usage = "GenGauss[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc pomocou Gaussovej metódy \
diff: \"EASY\" (3x3), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> Automatic | \"ONE\" | \"NONE\" | \"INFINITE\"   (Automatic vyberá typ náhodne)";

GenGaussJordan::usage = "GenGaussJordan[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc pomocou Gaussovej-Jordanovej metódy \
(prevod na tvar (I | x)) so zobrazením celočíselných riadkových úprav na augmentovanej matici.
diff: \"EASY\" (3x3), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> Automatic | \"ONE\" | \"NONE\" | \"INFINITE\"   (Automatic vyberá typ náhodne)";

GenGaussJordanPivot::usage = "GenGaussJordanPivot[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc pomocou Gaussovej-Jordanovej metódy \
s pivotovaním výberom najmenšieho možného pivotu v st\:013apci, so zobrazením celočíselných riadkových úprav na augmentovanej matici.
diff: \"EASY\" (3x3), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> Automatic | \"ONE\" | \"NONE\" | \"INFINITE\"   (Automatic vyberá typ náhodne)";

GenInverse::usage = "GenInverse[diff, mode, opts] vygeneruje didaktický príklad výpočtu inverznej matice pomocou Gauss-Jordanovej metódy v tvare (A|E) -> (E|A^(-1)).
diff: \"EASY\" (3x3), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> \"ONE\"   (iba jeden typ riešenia, pretože inverzná matica existuje len pre regulárnu maticu)";

GenLU::usage = "GenLU[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc pomocou LU rozkladu (Doolittle).
diff: \"EASY\" (3x3), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> \"ONE\"   (iba jeden typ riešenia, pretože pre Doolittle bez pivotovania vyžadujeme regulárnu maticu s nenulovými hlavnými pivotmi počas rozkladu)";

GenCramer::usage = "GenCramer[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc pomocou Cramerovho pravidla.
diff: \"EASY\" (3x3), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> \"ONE\"   (iba jeden typ riešenia, pretože Cramerovo pravidlo vyžaduje regulárnu maticu s nenulovým determinantom)";

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
GenCramer::baddiff = GenTriangular::baddiff;
GenCramer::badmode = GenTriangular::badmode;
GenCramer::badst   = GenTriangular::badst;
GenCramer::fail    = "Nepodarilo sa vygenerovať regulárnu sústavu vhodnú pre Cramerovo pravidlo.";

$CommonGeneratorOptions = {SolutionType -> Automatic, TriangularType -> Automatic};

Options[GenTriangular] = $CommonGeneratorOptions;
Options[GenGauss] = $CommonGeneratorOptions;
Options[GenGaussJordan] = $CommonGeneratorOptions;
Options[GenGaussJordanPivot] = $CommonGeneratorOptions;
Options[GenInverse] = {SolutionType -> "ONE"};
Options[GenLU] = {SolutionType -> "ONE"};
Options[GenCramer] = {SolutionType -> "ONE"};

$FailedScrambleCount;

Begin["`Private`"];

(* ~-~-~ VALIDATION ~-~-~ *)

DimensionByDifficulty[diff_String] := Switch[diff, "EASY", 3, "MEDIUM", 5, "HARD", 6];

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

(* základné zvýraznenie ľavej strany rovnosti *)
lhsStyle[expr_] := Style[expr, Bold];

SetAttributes[addGap, HoldFirst];

addGap[content_, h_: 5] := AppendTo[content, Cell["", "Text", CellMargins -> {{Inherited, Inherited}, {0, 0}}, CellSize -> {Automatic, h}]];

(* rovnice pre dosadzovanie v gauss *)
gaussBackSubstEquations[aug_, vars_, sol0_, skipIdx_, content_] := Module[
  {n = Length[aug], sol = sol0, row, pivot, rhsVal, terms, symExpr, subExpr, sumProducts, exprVal, boldVal, coeffTimes, out = content},
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
      pivot === 1, Row[{tf[lhsStyle[vars[[i]]]], " = ", symExpr}],
      pivot === -1, Row[{tf[lhsStyle[vars[[i]]]], " = -(", symExpr, ")"}],
      True, Row[{tf[lhsStyle[vars[[i]]]], " = (", symExpr, ") / ", tf[pivot]}]
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
      pivot === 1, Row[{tf[lhsStyle[vars[[i]]]], " = ", subExpr}],
      pivot === -1, Row[{tf[lhsStyle[vars[[i]]]], " = -(", subExpr, ")"}],
      True, Row[{tf[lhsStyle[vars[[i]]]], " = (", subExpr, ") / ", tf[pivot]}]
    ]];

    sumProducts = Total[terms[[All, 1]]*terms[[All, 2]]];
    exprVal = Together[(rhsVal - sumProducts)/pivot];
    sol[[i]] = exprVal;

    AppendTo[out, highlightGrid @ Grid[
      {{tf[lhsStyle[vars[[i]]]], "=", TraditionalForm[exprVal]}},
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
  Head[item] === Spacer, printCellStyle["", "Text"],
  MatchQ[item, Style[_, ___]], CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Text", ShowStringCharacters -> False],
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

luSumNeedsParensQ[terms_List] := Module[{vals, sumVal},
  vals = Times @@@ Select[terms, #[[1]] =!= 0 && #[[2]] =!= 0 &];
  If[vals === {}, Return[False]];
  If[Length[vals] > 1, Return[True]];
  sumVal = Together[First[vals]];
  NumericQ[sumVal] && sumVal < 0
];

luWrappedSumDisplay[terms_List] := Module[{sumDisp},
  sumDisp = luSumDisplay[terms];
  If[luSumNeedsParensQ[terms], Row[{"(", sumDisp, ")"}], sumDisp]
];

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

luMatrixPairGrid[L_, U_, lBold_List : {}, uBold_List : {}] := Module[
  {styledL, styledU},
  styledL = MapIndexed[
    If[MemberQ[lBold, #2], Style[#1, Bold], #1] &,
    L,
    {2}
  ];
  styledU = MapIndexed[
    If[MemberQ[uBold, #2], Style[#1, Bold], #1] &,
    U,
    {2}
  ];

  highlightGrid @ Grid[
    {{
      Style["L =", Bold, FontSize -> 16],
      TraditionalForm[MatrixForm[styledL]],
      Spacer[20],
      Style["U =", Bold, FontSize -> 16],
      TraditionalForm[MatrixForm[styledU]]
    }},
    Alignment -> Left,
    Spacings -> {2, 1}
  ]
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
  Row[{luEntrySymbol["l", j, i], " = ", luEntrySymbol["a", j, i], " / ", luEntrySymbol["u", i, i]}],
  Row[{
    luEntrySymbol["l", j, i], " = (",
    luEntrySymbol["a", j, i], " - ",
    Underoverscript["\[Sum]", Row[{k, " = 1"}], i - 1],
    Row[{luEntrySymbol["l", j, k], luEntrySymbol["u", k, i]}],
    ") / ", luEntrySymbol["u", i, i]
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

luMatrixProductDisplay[left_, right_] := Module[
  {rawMatrix, finalMatrix, terms, firstTerm, restTerms, shortExpr, tooltipExpr},

  rawMatrix = Table[
    terms = Select[
      Transpose[{left[[i]], right[[All, j]]}],
      #[[1]] =!= 0 && #[[2]] =!= 0 &
    ];

    If[
      terms === {},
      0,
      firstTerm = Row[{
        luFactorDisplay[terms[[1, 1]]],
        "\[CenterDot]",
        luFactorDisplay[terms[[1, 2]]]
      }];

      If[
        Length[terms] == 1,
        firstTerm,
        restTerms = Rest[terms];

        tooltipExpr = Row @ Riffle[
          (Row[{luFactorDisplay[#[[1]]], "\[CenterDot]", luFactorDisplay[#[[2]]]}] & /@ restTerms),
          " + "
        ];

        shortExpr = Row[{
          firstTerm,
          " + ",
          MouseAppearance[
            Tooltip[
              Style["...", Blue],
              Framed[
                tooltipExpr,
                Background -> White,
                FrameStyle -> GrayLevel[0.8],
                RoundingRadius -> 4,
                FrameMargins -> 5
              ],
              TooltipStyle -> {CellFrame -> 0}
            ],
            "LinkHand"
          ]
        }];

        shortExpr
      ]
    ],
    {i, 1, Length[left]}, {j, 1, Length[right[[1]]]}
  ];

  finalMatrix = Together[left . right];

  highlightGrid @ Grid[
    {{
      TraditionalForm[MatrixForm[left]],
      Style["\[CenterDot]", Bold, FontSize -> 16],
      TraditionalForm[MatrixForm[right]],
      Style["=", Bold, FontSize -> 16],
      TraditionalForm[MatrixForm[rawMatrix]],
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

$MaxBounds = 30; (*väčšie číslo sa nemôže ukázať*)
$Bounds = Quotient[$MaxBounds, 1.4 + 0.156 Sqrt[$MaxBounds]];
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

genScrambleCramer[diff_String, aug0_, triType_String, solType_String : "ONE"] := Module[
  {n, solutionVector, A, b, tries = 0},

  n = Length[aug0];
  solutionVector = aug0[[All, n + 1]];

  While[tries < $MaxRetryCount,
    A = Switch[
      diff,
      "EASY", generateCramerEasyMatrix[solutionVector],
      "MEDIUM", generateCramerMediumMatrix[solutionVector],
      "HARD", generateCramerHardMatrix[solutionVector],
      _, $Failed
    ];

    If[A === $Failed,
      tries++;
      Continue[];
    ];

    b = A . solutionVector;

    If[cramerDeterminantsWithinBoundsQ[A, b],
      Return[augFromAb[A, b]]
    ];

    tries++;
  ];

  $Failed
];

(* pomocné helpery pre Cramera *)

replaceColumn[matrix_, column_Integer, values_List] := Module[{updated = matrix},
  Do[updated[[i, column]] = values[[i]], {i, 1, Length[values]}];
  updated
];

cramerSignedTermDisplay[coeff_, body_, firstQ_] := Module[{absCoeff = Abs[coeff]},
  If[firstQ,
    If[coeff < 0,
      Row[{"-", cramerFactor[absCoeff], " \[CenterDot] ", body}],
      Row[{cramerFactor[coeff], " \[CenterDot] ", body}]
    ],
    If[coeff < 0,
      Row[{" - ", cramerFactor[absCoeff], " \[CenterDot] ", body}],
      Row[{" + ", cramerFactor[coeff], " \[CenterDot] ", body}]
    ]
  ]
];

(* zloží súčet členov laplaceovho rozvoja so zachovaním znamienok *)
cramerDetTermSum[terms_List] := Row @ Table[
  cramerSignedTermDisplay[
    terms[[k, 1]],
    cramerDetLabel[terms[[k, 2]]],
    k === 1
  ],
  {k, 1, Length[terms]}
];

cramerValueTermSum[coeffs_List, values_List] := Row @ Table[
  cramerSignedTermDisplay[
    coeffs[[k]],
    cramerFactor[values[[k]]],
    k === 1
  ],
  {k, 1, Length[coeffs]}
];

(* nájde stĺpec s práve jedným nenulovým prvkom *)
cramerSingletonColumnGlobalIndex[matrix_] := Module[
  {columnCounts},
  columnCounts = Count[#, x_ /; x =!= 0] & /@ Transpose[matrix];

  FirstCase[
    Range[Length[columnCounts]],
    j_ /; columnCounts[[j]] == 1,
    Missing["NotFound"]
  ]
];

(* v zadanom stĺpci nájde riadok jediného nenulového prvku *)
cramerSingletonRowInColumn[matrix_, column_Integer] := Module[
  {rows},
  rows = Select[
    Range[Length[matrix]],
    matrix[[#, column]] =!= 0 &
  ];

  If[Length[rows] == 1,
    First[rows],
    Missing["NotFound"]
  ]
];

(* nájde vhodnú laplaceovu líniu: najprv riadok, potom stĺpec *)
cramerSingletonLineData[matrix_] := Module[
  {rowIndex, colIndex, pivotRow, pivotColumn},

  rowIndex = cramerSingletonRowIndex[matrix];
  If[rowIndex =!= Missing["NotFound"],
    colIndex = cramerSingletonColumnIndex[matrix, rowIndex];

    If[colIndex =!= Missing["NotFound"],
      Return[<|
        "Type" -> "Row",
        "LineIndex" -> rowIndex,
        "PivotRow" -> rowIndex,
        "PivotColumn" -> colIndex
      |>]
    ];
  ];

  colIndex = cramerSingletonColumnGlobalIndex[matrix];
  If[colIndex =!= Missing["NotFound"],
    rowIndex = cramerSingletonRowInColumn[matrix, colIndex];

    If[rowIndex =!= Missing["NotFound"],
      Return[<|
        "Type" -> "Column",
        "LineIndex" -> colIndex,
        "PivotRow" -> rowIndex,
        "PivotColumn" -> colIndex
      |>]
    ];
  ];

  Missing["NotFound"]
];

(* nájde najriedší riadok alebo stĺpec *)
cramerSparseLineData[matrix_] := Module[
  {rowCounts, colCounts, minRowCount, minColCount, rowIndex, colIndex, nonzeroIndices},

  rowCounts = Count[#, x_ /; x =!= 0] & /@ matrix;
  colCounts = Count[#, x_ /; x =!= 0] & /@ Transpose[matrix];

  minRowCount = Min[rowCounts];
  minColCount = Min[colCounts];

  If[minRowCount <= minColCount,
    rowIndex = First @ FirstPosition[rowCounts, minRowCount];
    nonzeroIndices = Select[
      Range[Length[matrix[[rowIndex]]]],
      matrix[[rowIndex, #]] =!= 0 &
    ];

    <|
      "Type" -> "Row",
      "LineIndex" -> rowIndex,
      "NonzeroIndices" -> nonzeroIndices
    |>,
    colIndex = First @ FirstPosition[colCounts, minColCount];
    nonzeroIndices = Select[
      Range[Length[matrix]],
      matrix[[#, colIndex]] =!= 0 &
    ];

    <|
      "Type" -> "Column",
      "LineIndex" -> colIndex,
      "NonzeroIndices" -> nonzeroIndices
    |>
  ]
];

(* text pre laplaceov rozvoj *)
cramerLaplaceExplanation[lineData_Association] := If[
  lineData["Type"] === "Row",
  Row[{
    "Použijeme Laplaceov rozvoj podľa ", lineData["LineIndex"],
    ". riadku, lebo obsahuje jeden nenulový prvok."
  }],
  Row[{
    "Použijeme Laplaceov rozvoj podľa ", lineData["LineIndex"],
    ". stĺpca, lebo obsahuje jeden nenulový prvok."
  }]
];

(* text pre výber najriedšej línie *)
cramerSparseExplanation[lineData_Association] := If[
  lineData["Type"] === "Row",
  Row[{
    "Teraz rozvinieme determinant podľa ", lineData["LineIndex"],
    ". riadku, lebo obsahuje najviac núl."
  }],
  Row[{
    "Teraz rozvinieme determinant podľa ", lineData["LineIndex"],
    ". stĺpca, lebo obsahuje najviac núl."
  }]
];

cramerMatrixLabel[var_] := Subscript[Style["A", Italic], Style[var, Italic]];
cramerDetLabel[label_] := Row[{"det(", label, ")"}];
cramerResultStyle[expr_] := Style[expr, Bold, Blue];

cramerStyledMatrix[matrix_, hi_Association : <||>] := Module[
  {
    activeRow, activeColumn, pivotPos, focusCells,
    columnAsRowQ, rowTextColor, colTextColor, focusTextColor, pivotTextColor, zeroTextColor
  },

  activeRow = Lookup[hi, "ActiveRow", None];
  activeColumn = Lookup[hi, "ActiveColumn", None];
  pivotPos = Lookup[hi, "PivotPos", None];
  focusCells = Lookup[hi, "FocusCells", {}];
  columnAsRowQ = TrueQ @ Lookup[hi, "ColumnAsRow", False];

  rowTextColor = RGBColor[0.68, 0.45, 0.04];
  colTextColor = RGBColor[0.18, 0.56, 0.24];
  focusTextColor = RGBColor[0.20, 0.40, 0.78];
  pivotTextColor = RGBColor[0.16, 0.34, 0.90];
  zeroTextColor = GrayLevel[0.50];

  MapIndexed[
    Module[{i = #2[[1]], j = #2[[2]], styleOpts = {}, textColor = Automatic, styleArgs, inRowQ, inColumnQ, inFocusQ},
      inRowQ = IntegerQ[activeRow] && i === activeRow;
      inColumnQ = IntegerQ[activeColumn] && j === activeColumn;
      inFocusQ = MemberQ[focusCells, {i, j}];

      If[ListQ[pivotPos] && pivotPos === {i, j},
        textColor = pivotTextColor;
        styleOpts = {Bold};,
        If[inFocusQ,
          textColor = focusTextColor,
          If[inColumnQ,
            textColor = If[columnAsRowQ, rowTextColor, colTextColor],
            If[inRowQ,
              textColor = rowTextColor,
              If[#1 === 0,
                textColor = zeroTextColor
              ]
            ]
          ]
        ]
      ];

      styleArgs = Join[
        If[textColor === Automatic, {}, {textColor}],
        styleOpts
      ];

      Style[#1, Sequence @@ styleArgs]
    ] &,
    matrix,
    {2}
  ]
];

cramerMatrixCard[matrix_, hi_Association : <||>] := TraditionalForm[
  MatrixForm[cramerStyledMatrix[matrix, hi]]
];

cramerReductionHighlight[lineData_Association, extra_Association : <||>] := Module[
  {base},
  base = If[
    lineData["Type"] === "Row",
    <|
      "ActiveRow" -> lineData["LineIndex"],
      "ActiveColumn" -> lineData["PivotColumn"],
      "PivotPos" -> {lineData["PivotRow"], lineData["PivotColumn"]},
      "ColumnAsRow" -> True
    |>,
    <|
      "ActiveRow" -> lineData["PivotRow"],
      "ActiveColumn" -> lineData["LineIndex"],
      "PivotPos" -> {lineData["PivotRow"], lineData["PivotColumn"]},
      "ColumnAsRow" -> True
    |>
  ];
  Join[base, extra]
];

cramerLaplaceReductionPanel[matrix_, lineData_Association, minorLabel_, minorMatrix_] := Grid[
  {{
    cramerMatrixCard[matrix, cramerReductionHighlight[lineData]],
    Style["\[LongRightArrow]", Bold, FontSize -> 24, GrayLevel[0.2]],
    Grid[
      {{
        Style[Row[{minorLabel, " ="}], Bold, FontSize -> 15],
        cramerMatrixCard[minorMatrix, <|"FontSize" -> 13, "CellWidth" -> 1.05|>]
      }},
      Alignment -> {Left, Center},
      Spacings -> {0.8, 0.4}
    ]
  }},
  Alignment -> {Left, Center, Left},
  Spacings -> {1.8, 1}
];

cramerLaplaceVisualizationTitle[lineData_Association] := If[
  lineData["Type"] === "Row",
  "Vizualizácia Laplaceovho rozvoja podľa " <> ToString[lineData["LineIndex"]] <> ". riadku:",
  "Vizualizácia Laplaceovho rozvoja podľa " <> ToString[lineData["LineIndex"]] <> ". stĺpca:"
];

cramerTermHighlight[sparseLine_Association, termIndex_Integer] := If[
  sparseLine["Type"] === "Row",
  <|
    "ActiveRow" -> sparseLine["LineIndex"],
    "ActiveColumn" -> termIndex,
    "PivotPos" -> {sparseLine["LineIndex"], termIndex},
    "FontSize" -> 12,
    "CellWidth" -> 0.95
  |>,
  <|
    "ActiveRow" -> termIndex,
    "ActiveColumn" -> sparseLine["LineIndex"],
    "PivotPos" -> {termIndex, sparseLine["LineIndex"]},
    "FontSize" -> 12,
    "CellWidth" -> 0.95
  |>
];

cramerLaplaceTermVisual[sourceMatrix_, sparseLine_Association, coeff_, termIndex_Integer, termLabel_, minorMatrix_] := Grid[
  {
    {cramerMatrixCard[sourceMatrix, cramerTermHighlight[sparseLine, termIndex]]},
    {Style[Row[{"\[DownArrow] ", termLabel}], Bold, FontSize -> 14, GrayLevel[0.25]]},
    {
      Grid[
        {{
          Style[cramerFactor[coeff], Bold, RGBColor[0.20, 0.38, 0.93], FontSize -> 15],
          Style["\[CenterDot]", Bold, RGBColor[0.20, 0.38, 0.93], FontSize -> 15],
          cramerMatrixCard[minorMatrix, <|"FontSize" -> 12, "CellWidth" -> 0.95|>]
        }},
        Alignment -> {Center, Center, Center},
        Spacings -> {0.5, 0.4}
      ]
    }
  },
  Alignment -> Center,
  Spacings -> {0.8, 0.7}
];

cramerLaplaceTermPanel[sourceMatrix_, sparseLine_Association, termInfos_List, termDataList_List, termIndices_List] := Module[
  {termVisuals},
  termVisuals = Table[
    cramerLaplaceTermVisual[
      sourceMatrix,
      sparseLine,
      termInfos[[k, 1]],
      termIndices[[k]],
      termInfos[[k, 2]],
      termDataList[[k]]["Matrix"]
    ],
    {k, 1, Length[termInfos]}
  ];

  highlightGrid @ Column[
    {
      Style[cramerLaplaceVisualizationTitle[sparseLine], FontSize -> 14, GrayLevel[0.15]],
      Grid[{termVisuals}, Alignment -> Center, Spacings -> {1.8, 0.8}]
    },
    Spacings -> 0.9
  ]
];

cramerFactor[value_] := If[
  NumberQ[value] && value < 0,
  Row[{"(", tft[value], ")"}],
  tft[value]
];

cramerLabeledMatrixGrid[label_, matrix_, hi_Association : <||>] := Grid[
  {{
    Style[Row[{label, " ="}], Bold, FontSize -> 16],
    cramerMatrixCard[matrix, hi]
  }},
  Alignment -> Left,
  Spacings -> {2, 1}
];

cramerEqualityGrid[label_, value_] := highlightGrid @ Grid[
  {{
    Style[label, Bold, FontSize -> 15],
    "=",
    cramerResultStyle[tft[value]]
  }},
  Alignment -> {{Right, Center, Left}},
  BaseStyle -> {FontSize -> 16}
];

cramerSolutionGrid[vars_List, values_List] := highlightGrid @ Grid[
  Table[
    {tf[lhsStyle[vars[[i]]]], "=", TraditionalForm[values[[i]]]},
    {i, 1, Length[vars]}
  ],
  Alignment -> {{Right, Center, Left}},
  BaseStyle -> {FontSize -> 16}
];

cramerZeroRowIndex[matrix_] := FirstCase[
  Range[Length[matrix]],
  row_ /; AllTrue[matrix[[row]], # === 0 &],
  Missing["NotFound"]
];

cramerMinor[matrix_, row_Integer, column_Integer] := Module[
  {withoutRow},
  withoutRow = Delete[matrix, row];
  Map[Delete[#, column] &, withoutRow]
];

cramerSingletonRowIndex[matrix_] := Module[
  {rowCounts},
  rowCounts = Count[#, x_ /; x =!= 0] & /@ matrix;
  FirstCase[
    Range[Length[rowCounts]],
    i_ /; rowCounts[[i]] == 1,
    Missing["NotFound"]
  ]
];

cramerSingletonColumnIndex[matrix_, row_Integer] := Module[
  {cols},
  cols = Select[
    Range[Length[matrix[[row]]]],
    matrix[[row, #]] =!= 0 &
  ];

  If[Length[cols] == 1,
    First[cols],
    Missing["NotFound"]
  ]
];

cramerSparseRowIndex[matrix_] := First @ MinimalBy[
  Range[Length[matrix]],
  Count[matrix[[#]], x_ /; x =!= 0] &
];

cramer3x3PositiveColors = {
  RGBColor[0.10, 0.60, 0.22],
  RGBColor[0.87, 0.48, 0.07],
  RGBColor[0.73, 0.63, 0.05]
};

cramer3x3NegativeColors = {
  RGBColor[0.12, 0.37, 0.92],
  RGBColor[0.53, 0.29, 0.88],
  RGBColor[0.00, 0.60, 0.78]
};

cramer3x3ModeColor[mode_String, pos_List] := Module[{groups, colors},
  {groups, colors} = Switch[
    mode,
    "Positive",
    {
      {
        {{1, 1}, {2, 2}, {3, 3}},
        {{1, 2}, {2, 3}, {3, 1}},
        {{1, 3}, {2, 1}, {3, 2}}
      },
      cramer3x3PositiveColors
    },
    "Negative",
    {
      {
        {{1, 3}, {2, 2}, {3, 1}},
        {{1, 1}, {2, 3}, {3, 2}},
        {{1, 2}, {2, 1}, {3, 3}}
      },
      cramer3x3NegativeColors
    }
  ];

  FirstCase[
    Range[Length[groups]],
    k_ /; MemberQ[groups[[k]], pos] :> colors[[k]],
    Black
  ]
];

cramer3x3StyledMatrixByMode[matrix_, mode_String] := Module[
  {styled},
  styled = MapIndexed[
    Style[#1, FontColor -> cramer3x3ModeColor[mode, #2], Bold] &,
    matrix,
    {2}
  ];
  TraditionalForm[MatrixForm[styled]]
];

cramer3x3TermProduct[values_List, color_] := Row @ Riffle[
  (Style[cramerFactor[#], FontColor -> color, Bold] & /@ values),
  Style["\[CenterDot]", FontColor -> color, Bold]
];

cramer3x3FormulaDisplay[matrix_] := Module[
  {a, b, c, d, e, f, g, h, i},
  {{a, b, c}, {d, e, f}, {g, h, i}} = matrix;

  Row[{
    cramer3x3TermProduct[{a, e, i}, cramer3x3PositiveColors[[1]]],
    " + ",
    cramer3x3TermProduct[{b, f, g}, cramer3x3PositiveColors[[2]]],
    " + ",
    cramer3x3TermProduct[{c, d, h}, cramer3x3PositiveColors[[3]]],
    " - ",
    cramer3x3TermProduct[{c, e, g}, cramer3x3NegativeColors[[1]]],
    " - ",
    cramer3x3TermProduct[{a, f, h}, cramer3x3NegativeColors[[2]]],
    " - ",
    cramer3x3TermProduct[{b, d, i}, cramer3x3NegativeColors[[3]]]
  }]
];

cramer3x3VisualPanel[label_, matrix_] := Grid[
  {{
    Style[Row[{label, " ="}], Bold, FontSize -> 16],
    TraditionalForm[MatrixForm[matrix]],
    Style["\[LongRightArrow]", Bold, FontSize -> 26],
    Grid[
      {{
        Style["+", Bold, FontSize -> 28],
        cramer3x3StyledMatrixByMode[matrix, "Positive"]
      }},
      Alignment -> {Left, Top},
      Spacings -> {0.4, 0}
    ],
    Grid[
      {{
        Style["-", Bold, FontSize -> 28],
        cramer3x3StyledMatrixByMode[matrix, "Negative"]
      }},
      Alignment -> {Left, Top},
      Spacings -> {0.4, 0}
    ]
  }},
  Alignment -> {Left, Center, Center, Left, Left},
  Spacings -> {1.5, 1}
];

(* vykreslí determinant 3×3 štandardným vzorcom *)
renderCramer3x3Det[matrix_, label_] := Module[
  {content = {}, value},

  value = Together[Det[matrix]];

  AppendTo[content, Row[{
    "Determinant matice 3×3 vypočítame pomocou ",
    Style["Sarrusovho pravidla", Bold],
    "."
  }]];
  AppendTo[content, cramer3x3VisualPanel[label, matrix]];
  AppendTo[content, Row[{cramerDetLabel[label], " = ", cramer3x3FormulaDisplay[matrix]}]];
  AppendTo[content, cramerEqualityGrid[cramerDetLabel[label], value]];

  <|"Content" -> content, "Value" -> value, "Matrix" -> matrix|>
];

(* vykreslí determinant 5×5 cez dva laplaceove rozvoje a následný determinant 3×3 *)
renderCramerMediumReduction[matrix_, label_] := Module[
  {
    content = {}, line1, line2,
    signed1, signed2, minor4, minor3,
    minor4Label, minor3Label, det3Data, det4Value, value
  },

  minor4Label = Subscript[Style["M", Italic], 4];
  minor3Label = Subscript[Style["M", Italic], 3];

  AppendTo[content, cramerLabeledMatrixGrid[label, matrix]];

  line1 = cramerSingletonLineData[matrix];
  If[line1 === Missing["NotFound"],
    value = Together[Det[matrix]];
    AppendTo[content, "Matica nemá vhodný riedky riadok ani stĺpec, preto determinant dopočítame priamo."];
    AppendTo[content, cramerEqualityGrid[cramerDetLabel[label], value]];
    Return[<|"Content" -> content, "Value" -> value, "Matrix" -> matrix|>];
  ];

  signed1 = Together[
    (-1)^(line1["PivotRow"] + line1["PivotColumn"]) *
        matrix[[line1["PivotRow"], line1["PivotColumn"]]]
  ];
  minor4 = cramerMinor[matrix, line1["PivotRow"], line1["PivotColumn"]];

  AppendTo[content, cramerLaplaceExplanation[line1]];
  AppendTo[content, cramerLaplaceReductionPanel[matrix, line1, minor4Label, minor4]];
  AppendTo[content, Row[{
    cramerDetLabel[label], " = ", cramerFactor[signed1],
    " \[CenterDot] ", cramerDetLabel[minor4Label]
  }]];

  If[cramerZeroRowIndex[minor4] =!= Missing["NotFound"],
    det4Value = 0;
    AppendTo[content, "Minor 4×4 obsahuje nulový riadok, preto jeho determinant je 0."];
    AppendTo[content, cramerEqualityGrid[cramerDetLabel[minor4Label], det4Value]];

    value = Together[signed1 det4Value];
    AppendTo[content, Row[{
      cramerDetLabel[label], " = ",
      cramerFactor[signed1], " \[CenterDot] ", cramerDetLabel[minor4Label],
      " = ",
      cramerFactor[signed1], " \[CenterDot] ", cramerFactor[det4Value],
      " = ",
      cramerFactor[value]
    }]];
    AppendTo[content, cramerEqualityGrid[cramerDetLabel[label], value]];

    Return[<|"Content" -> content, "Value" -> value, "Matrix" -> matrix|>];
  ];

  line2 = cramerSingletonLineData[minor4];
  If[line2 === Missing["NotFound"],
    det4Value = Together[Det[minor4]];
    AppendTo[content, "Minor 4×4 už nemá vhodný riedky riadok ani stĺpec, preto jeho determinant dopočítame priamo."];
    AppendTo[content, cramerEqualityGrid[cramerDetLabel[minor4Label], det4Value]];

    value = Together[signed1 det4Value];
    AppendTo[content, Row[{
      cramerDetLabel[label], " = ",
      cramerFactor[signed1], " \[CenterDot] ", cramerDetLabel[minor4Label],
      " = ",
      cramerFactor[signed1], " \[CenterDot] ", cramerFactor[det4Value],
      " = ",
      cramerFactor[value]
    }]];
    AppendTo[content, cramerEqualityGrid[cramerDetLabel[label], value]];

    Return[<|"Content" -> content, "Value" -> value, "Matrix" -> matrix|>];
  ];

  signed2 = Together[
    (-1)^(line2["PivotRow"] + line2["PivotColumn"]) *
        minor4[[line2["PivotRow"], line2["PivotColumn"]]]
  ];
  minor3 = cramerMinor[minor4, line2["PivotRow"], line2["PivotColumn"]];

  AppendTo[content, cramerLaplaceExplanation[line2]];
  AppendTo[content, cramerLaplaceReductionPanel[minor4, line2, minor3Label, minor3]];
  AppendTo[content, Row[{
    cramerDetLabel[minor4Label], " = ", cramerFactor[signed2],
    " \[CenterDot] ", cramerDetLabel[minor3Label]
  }]];

  det3Data = renderCramer3x3Det[minor3, minor3Label];
  content = Join[content, det3Data["Content"]];

  det4Value = Together[signed2 det3Data["Value"]];
  AppendTo[content, Row[{
    cramerDetLabel[minor3Label], " = ",
    cramerFactor[det3Data["Value"]]
  }]];
  AppendTo[content, Row[{
    cramerDetLabel[minor4Label], " = ",
    cramerFactor[signed2], " \[CenterDot] ", cramerDetLabel[minor3Label],
    " = ",
    cramerFactor[signed2], " \[CenterDot] ", cramerFactor[det3Data["Value"]],
    " = ",
    cramerFactor[det4Value]
  }]];
  AppendTo[content, cramerEqualityGrid[cramerDetLabel[minor4Label], det4Value]];

  value = Together[signed1 det4Value];
  AppendTo[content, Row[{
    cramerDetLabel[label], " = ",
    cramerFactor[signed1], " \[CenterDot] ", cramerDetLabel[minor4Label],
    " = ",
    cramerFactor[signed1], " \[CenterDot] ", cramerFactor[det4Value],
    " = ",
    cramerFactor[value]
  }]];
  AppendTo[content, cramerEqualityGrid[cramerDetLabel[label], value]];

  <|"Content" -> content, "Value" -> value, "Matrix" -> matrix|>
];

(* vykreslí determinant 6×6 cez dva laplaceove rozvoje na 4×4 a potom rozvoj podľa najriedšej línie *)
renderCramerHardReduction[matrix_, label_] := Module[
  {
    content = {}, line1, line2, sparseLine,
    signed1, signed2, minor5, minor4,
    minor5Label, minor4Label,
    det5Value, det4Value, innerValue, value,
    termDataList = {}, termInfos = {}, allTermDataList = {}, allTermInfos = {},
    termIndex, coeff, minor3, termLabel, termData, termIndices, termValues
  },

  minor5Label = Subscript[Style["M", Italic], 5];
  minor4Label = Subscript[Style["M", Italic], 4];

  AppendTo[content, cramerLabeledMatrixGrid[label, matrix]];

  line1 = cramerSingletonLineData[matrix];
  If[line1 === Missing["NotFound"],
    value = Together[Det[matrix]];
    AppendTo[content, "Matica nemá vhodný riedky riadok ani stĺpec, preto determinant dopočítame priamo."];
    AppendTo[content, cramerEqualityGrid[cramerDetLabel[label], value]];
    Return[<|"Content" -> content, "Value" -> value, "Matrix" -> matrix|>];
  ];

  signed1 = Together[
    (-1)^(line1["PivotRow"] + line1["PivotColumn"]) *
        matrix[[line1["PivotRow"], line1["PivotColumn"]]]
  ];
  minor5 = cramerMinor[matrix, line1["PivotRow"], line1["PivotColumn"]];

  AppendTo[content, cramerLaplaceExplanation[line1]];
  AppendTo[content, cramerLaplaceReductionPanel[matrix, line1, minor5Label, minor5]];
  AppendTo[content, Row[{
    cramerDetLabel[label], " = ", cramerFactor[signed1],
    " \[CenterDot] ", cramerDetLabel[minor5Label]
  }]];

  If[cramerZeroRowIndex[minor5] =!= Missing["NotFound"],
    det5Value = 0;
    AppendTo[content, "Minor 5×5 obsahuje nulový riadok, preto jeho determinant je 0."];
    AppendTo[content, cramerEqualityGrid[cramerDetLabel[minor5Label], det5Value]];

    value = Together[signed1 det5Value];
    AppendTo[content, Row[{
      cramerDetLabel[label], " = ",
      cramerFactor[signed1], " \[CenterDot] ", cramerDetLabel[minor5Label],
      " = ",
      cramerFactor[signed1], " \[CenterDot] ", cramerFactor[det5Value],
      " = ",
      cramerFactor[value]
    }]];
    AppendTo[content, cramerEqualityGrid[cramerDetLabel[label], value]];

    Return[<|"Content" -> content, "Value" -> value, "Matrix" -> matrix|>];
  ];

  line2 = cramerSingletonLineData[minor5];
  If[line2 === Missing["NotFound"],
    det5Value = Together[Det[minor5]];
    AppendTo[content, "Minor 5×5 už nemá vhodný riedky riadok ani stĺpec, preto jeho determinant dopočítame priamo."];
    AppendTo[content, cramerEqualityGrid[cramerDetLabel[minor5Label], det5Value]];

    value = Together[signed1 det5Value];
    AppendTo[content, Row[{
      cramerDetLabel[label], " = ",
      cramerFactor[signed1], " \[CenterDot] ", cramerDetLabel[minor5Label],
      " = ",
      cramerFactor[signed1], " \[CenterDot] ", cramerFactor[det5Value],
      " = ",
      cramerFactor[value]
    }]];
    AppendTo[content, cramerEqualityGrid[cramerDetLabel[label], value]];

    Return[<|"Content" -> content, "Value" -> value, "Matrix" -> matrix|>];
  ];

  signed2 = Together[
    (-1)^(line2["PivotRow"] + line2["PivotColumn"]) *
        minor5[[line2["PivotRow"], line2["PivotColumn"]]]
  ];
  minor4 = cramerMinor[minor5, line2["PivotRow"], line2["PivotColumn"]];

  AppendTo[content, cramerLaplaceExplanation[line2]];
  AppendTo[content, cramerLaplaceReductionPanel[minor5, line2, minor4Label, minor4]];
  AppendTo[content, Row[{
    cramerDetLabel[minor5Label], " = ", cramerFactor[signed2],
    " \[CenterDot] ", cramerDetLabel[minor4Label]
  }]];

  If[cramerZeroRowIndex[minor4] =!= Missing["NotFound"],
    det4Value = 0;
    AppendTo[content, "Minor 4×4 obsahuje nulový riadok, preto jeho determinant je 0."];
    AppendTo[content, cramerEqualityGrid[cramerDetLabel[minor4Label], det4Value]];

    det5Value = Together[signed2 det4Value];
    AppendTo[content, Row[{
      cramerDetLabel[minor5Label], " = ",
      cramerFactor[signed2], " \[CenterDot] ", cramerDetLabel[minor4Label],
      " = ",
      cramerFactor[signed2], " \[CenterDot] ", cramerFactor[det4Value],
      " = ",
      cramerFactor[det5Value]
    }]];
    AppendTo[content, cramerEqualityGrid[cramerDetLabel[minor5Label], det5Value]];

    value = Together[signed1 det5Value];
    AppendTo[content, Row[{
      cramerDetLabel[label], " = ",
      cramerFactor[signed1], " \[CenterDot] ", cramerDetLabel[minor5Label],
      " = ",
      cramerFactor[signed1], " \[CenterDot] ", cramerFactor[det5Value],
      " = ",
      cramerFactor[value]
    }]];
    AppendTo[content, cramerEqualityGrid[cramerDetLabel[label], value]];

    Return[<|"Content" -> content, "Value" -> value, "Matrix" -> matrix|>];
  ];

  sparseLine = cramerSparseLineData[minor4];
  AppendTo[content, cramerSparseExplanation[sparseLine]];

  termIndices = If[
    sparseLine["Type"] === "Row",
    Range[Length[minor4[[sparseLine["LineIndex"]]]]],
    Range[Length[minor4]]
  ];

  Do[
    If[sparseLine["Type"] === "Row",
      coeff = Together[
        (-1)^(sparseLine["LineIndex"] + termIndex) *
            minor4[[sparseLine["LineIndex"], termIndex]]
      ];
      minor3 = cramerMinor[minor4, sparseLine["LineIndex"], termIndex];
      ,
      coeff = Together[
        (-1)^(termIndex + sparseLine["LineIndex"]) *
            minor4[[termIndex, sparseLine["LineIndex"]]]
      ];
      minor3 = cramerMinor[minor4, termIndex, sparseLine["LineIndex"]];
    ];

    termLabel = Subscript[Style["N", Italic], Length[allTermInfos] + 1];

    termData = renderCramer3x3Det[minor3, termLabel];
    AppendTo[allTermInfos, {coeff, termLabel}];
    AppendTo[allTermDataList, termData];

    If[coeff =!= 0,
      AppendTo[termInfos, {coeff, termLabel}];
      AppendTo[termDataList, termData];
    ];
    ,
    {termIndex, termIndices}
  ];

  AppendTo[content, cramerLaplaceTermPanel[minor4, sparseLine, allTermInfos, allTermDataList, termIndices]];

  AppendTo[content, Row[{
    cramerDetLabel[minor4Label], " = ",
    cramerDetTermSum[allTermInfos]
  }]];

  If[Length[termInfos] < Length[allTermInfos],
    AppendTo[content, Row[{
      cramerDetLabel[minor4Label], " = ",
      cramerDetTermSum[termInfos]
    }]];
  ];

  Do[
    AppendTo[content, Row[{
      "Člen ", k, ": ",
      cramerFactor[termInfos[[k, 1]]], " \[CenterDot] ",
      cramerDetLabel[termInfos[[k, 2]]]
    }]];
    content = Join[content, termDataList[[k]]["Content"]];
    ,
    {k, 1, Length[termInfos]}
  ];

  innerValue = Together[
    Total@Table[termInfos[[k, 1]] termDataList[[k]]["Value"], {k, 1, Length[termInfos]}]
  ];
  termValues = termDataList[[All, "Value"]];
  AppendTo[content, Row[{
    cramerDetLabel[minor4Label], " = ",
    cramerDetTermSum[termInfos],
    " = ",
    cramerValueTermSum[termInfos[[All, 1]], termValues],
    " = ",
    cramerFactor[innerValue]
  }]];
  AppendTo[content, cramerEqualityGrid[cramerDetLabel[minor4Label], innerValue]];

  det5Value = Together[signed2 innerValue];
  AppendTo[content, Row[{
    cramerDetLabel[minor4Label], " = ",
    cramerFactor[innerValue]
  }]];
  AppendTo[content, Row[{
    cramerDetLabel[minor5Label], " = ",
    cramerFactor[signed2], " \[CenterDot] ", cramerDetLabel[minor4Label],
    " = ",
    cramerFactor[signed2], " \[CenterDot] ", cramerFactor[innerValue],
    " = ",
    cramerFactor[det5Value]
  }]];
  AppendTo[content, cramerEqualityGrid[cramerDetLabel[minor5Label], det5Value]];

  value = Together[signed1 det5Value];
  AppendTo[content, Row[{
    cramerDetLabel[label], " = ",
    cramerFactor[signed1], " \[CenterDot] ", cramerDetLabel[minor5Label],
    " = ",
    cramerFactor[signed1], " \[CenterDot] ", cramerFactor[det5Value],
    " = ",
    cramerFactor[value]
  }]];
  AppendTo[content, cramerEqualityGrid[cramerDetLabel[label], value]];

  <|"Content" -> content, "Value" -> value, "Matrix" -> matrix|>
];

renderCramerDeterminant[matrix_, label_] := Switch[
  Length[matrix],
  3, renderCramer3x3Det[matrix, label],
  5, renderCramerMediumReduction[matrix, label],
  6, renderCramerHardReduction[matrix, label],
  _, <|
    "Content" -> {
      cramerLabeledMatrixGrid[label, matrix],
      cramerEqualityGrid[cramerDetLabel[label], Together[Det[matrix]]]
    },
    "Value" -> Together[Det[matrix]],
    "Matrix" -> matrix
  |>
];

cramerRandomNonzeroValue[maxAbs_Integer : 4] := RandomChoice[
  DeleteCases[Range[-maxAbs, maxAbs], 0]
];

cramerRandomInvertible3x3[maxAbs_Integer : 4, maxDet_Integer : 30] := Module[
  {candidate, tries = 0, pool},
  pool = Join[Range[-maxAbs, -1], Range[1, maxAbs], Range[-maxAbs, -1], Range[1, maxAbs]];

  While[tries < $MaxRetryCount,
    candidate = RandomChoice[pool, {3, 3}];
    If[Det[candidate] =!= 0 && Abs[Det[candidate]] <= maxDet,
      Return[candidate]
    ];
    tries++;
  ];

  $Failed
];

cramerOrthogonalTwoNonzeroRow[xCore_List] := Module[
  {pairs, chosenPair, row, p, q, g},

  pairs = Select[
    Subsets[Range[Length[xCore]], {2}],
    With[{u = xCore[[#[[1]]]], v = xCore[[#[[2]]]]},
      (u === 0 && v === 0) || (u =!= 0 && v =!= 0)
    ] &
  ];

  If[pairs === {},
    chosenPair = {1, 2},
    chosenPair = RandomChoice[pairs]
  ];

  {p, q} = chosenPair;
  row = ConstantArray[0, Length[xCore]];

  If[xCore[[p]] === 0 && xCore[[q]] === 0,
    row[[p]] = 1;
    row[[q]] = 1;
    ,
    g = GCD[Abs[xCore[[p]]], Abs[xCore[[q]]]];
    If[g === 0, g = 1];

    row[[p]] = Quotient[xCore[[q]], g];
    row[[q]] = -Quotient[xCore[[p]], g];
  ];

  normalizeRow[row]
];

generateCramerEasyMatrix[solutionVector_List] := Module[
  {candidate, rhsVector, tries = 0, pool},

  pool = Join[Range[-4, -1], Range[1, 4], Range[-4, -1], Range[1, 4]];

  While[tries < $MaxRetryCount,
    candidate = RandomChoice[pool, {3, 3}];
    rhsVector = candidate . solutionVector;

    If[
      Det[candidate] =!= 0 &&
          Abs[Det[candidate]] <= Min[30, $MaxBounds] &&
          matrixMaxAbs[rhsVector] <= $MaxBounds,
      Return[candidate]
    ];

    tries++;
  ];

  $Failed
];

(* vygeneruje 5x5 maticu, ktorá sa po dvoch laplaceových krokoch zvedie na 3x3 *)
generateCramerMediumMatrix[solutionVector_List] := Module[
  {core, candidate, rhsVector, s1, s2, tries = 0},

  While[tries < $MaxRetryCount,
    core = cramerRandomInvertible3x3[3, 18];
    If[core === $Failed,
      Return[$Failed]
    ];

    s1 = cramerRandomNonzeroValue[3];
    s2 = cramerRandomNonzeroValue[3];

    candidate = ConstantArray[0, {5, 5}];
    candidate[[1, 1]] = s1;
    candidate[[2, 2]] = s2;
    candidate[[3 ;; 5, 3 ;; 5]] = core;

    rhsVector = candidate . solutionVector;

    If[
      Det[candidate] =!= 0 &&
          Abs[Det[candidate]] <= Min[80, $MaxBounds] &&
          matrixMaxAbs[rhsVector] <= $MaxBounds,
      Return[candidate]
    ];

    tries++;
  ];

  $Failed
];

generateCramerHardMatrix[solutionVector_List] := Module[
  {coreSolution, specialRow, otherRows, core, candidate, rhsVector, s1, s2, tries = 0, pool},

  coreSolution = solutionVector[[3 ;; 6]];
  pool = Join[Range[-3, -1], Range[1, 3], Range[-3, -1], Range[1, 3]];

  While[tries < $MaxRetryCount,
    s1 = cramerRandomNonzeroValue[2];
    s2 = cramerRandomNonzeroValue[2];

    specialRow = cramerOrthogonalTwoNonzeroRow[coreSolution];
    otherRows = RandomChoice[pool, {3, 4}];
    core = Join[{specialRow}, otherRows];

    If[
      Det[core] === 0 ||
          Abs[Det[core]] > Min[50, $MaxBounds] ||
          Count[specialRow, elem_ /; elem =!= 0] > 2,
      tries++;
      Continue[];
    ];

    candidate = ConstantArray[0, {6, 6}];
    candidate[[1, 1]] = s1;
    candidate[[2, 2]] = s2;
    candidate[[3 ;; 6, 3 ;; 6]] = core;

    rhsVector = candidate . solutionVector;

    If[
      Det[candidate] =!= 0 &&
          Abs[Det[candidate]] <= $MaxBounds &&
          matrixMaxAbs[rhsVector] <= $MaxBounds,
      Return[candidate]
    ];

    tries++;
  ];

  $Failed
];

cramerSolveData[A_, b_] := Module[
  {detA, auxMatrices, auxDeterminants, solution},

  detA = Together[Det[A]];
  auxMatrices = Table[replaceColumn[A, i, b], {i, 1, Length[b]}];
  auxDeterminants = Together /@ (Det /@ auxMatrices);

  solution = If[
    detA === 0,
    ConstantArray[Indeterminate, Length[b]],
    Together /@ (auxDeterminants/detA)
  ];

  <|
    "DetA" -> detA,
    "AuxMatrices" -> auxMatrices,
    "AuxDeterminants" -> auxDeterminants,
    "Solution" -> solution
  |>
];

cramerDeterminantsWithinBoundsQ[A_, b_] := Module[
  {solveData, allDeterminants},

  solveData = cramerSolveData[A, b];
  allDeterminants = Join[{solveData["DetA"]}, solveData["AuxDeterminants"]];

  AllTrue[
    allDeterminants,
    IntegerQ[#] && Abs[#] <= $MaxBounds &
  ]
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
        addGap[content, 6];
        AppendTo[content, highlightGrid @ Grid[
          {{tf[lhsStyle[vars[[i]]]], "=", tft[solLocal[[i]]]}},
          Alignment -> {{Right, Center, Left}}, BaseStyle -> {FontSize -> 16}
        ]];
        addGap[content, 6];
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
      addGap[content, 6];
      AppendTo[content,
        highlightGrid @ Grid[
          {{tf[vars[[paramIdx]]], "=", TraditionalForm[\[FormalT]]}},
          Alignment -> {{Right, Center, Left}},
          BaseStyle -> {FontSize -> 16}
        ]
      ];
      addGap[content, 6];

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
        notes[[i]] = Row[{lhsStyle[vars[[i]]], " = ", TraditionalForm[solExprs[[i]]]}];

        addMatrix[
          aug,
          notes,
          <|
            "ActiveRow" -> i,
            "PivotPos" -> {i, i},
            "GreenCells" -> {{i, i}, {i, n + 1}}
          |>
        ];

        addGap[content, 6];
        AppendTo[content,
          highlightGrid @ Grid[
            {{tf[lhsStyle[vars[[i]]]], "=", TraditionalForm[solExprs[[i]]]}},
            Alignment -> {{Right, Center, Left}},
            BaseStyle -> {FontSize -> 16}
          ]
        ];
        addGap[content, 6];
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

    addGap[content, 6];
    AppendTo[content, highlightGrid @ Grid[
      {{tf[lhsStyle[vars[[i]]]], "=", tft[aug[[i, n + 1]]] }},
      Alignment -> {{Right, Center, Left}},
      BaseStyle -> {FontSize -> 16}
    ]];
    addGap[content, 6];
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
    addGap[content, 6];
    AppendTo[content, highlightGrid @ Grid[
      {{tf[vars[[paramIdx]]], "=", TraditionalForm[\[FormalT]]}},
      Alignment -> {{Right, Center, Left}},
      BaseStyle -> {FontSize -> 16}
    ]];
    addGap[content, 6];

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
      notes[[i]] = Row[{lhsStyle[vars[[i]]], " = ", TraditionalForm[solExprs[[i]]]}];

      addMatrix[
        aug,
        notes,
        <|"ActiveRow" -> i, "PivotPos" -> {i, i}, "GreenCells" -> {{i, i}, {i, n + 1}}|>
      ];

      addGap[content, 6];
      AppendTo[content, highlightGrid @ Grid[
        {{tf[lhsStyle[vars[[i]]]], "=", TraditionalForm[solExprs[[i]]] }},
        Alignment -> {{Right, Center, Left}},
        BaseStyle -> {FontSize -> 16}
      ]];
      addGap[content, 6];
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
    Table[{tf[lhsStyle[vars[[i]]]], "=", tft[xResult[[i]]]}, {i, 1, n}],
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

  addGap[content, 6];
  content = Join[content, verificationSteps[data, xResult]];

  addHeader["Záver"];
  addText["Inverzná matica bola úspešne vypočítaná pomocou Gaussovej-Jordanovej metódy. Riešenie sústavy sme následne dostali zo vzťahu x = A^(-1) · b."];

  <|"Content" -> content, "Solution" -> xResult, "InverseMatrix" -> invMatrix|>
];

stepsLU[data_Association] := Module[
  {
    content = {}, n, A, b, vars, luData, L, U, y, x,
    addHeader, addText, addMatrixPair, addVector, addFormula, addSubHeader, resultStyle,
    i, j, terms, sumTerm, pivotValue, luProduct, lowerCheck, upperCheck,
    xSymbols, formatLinearEquation, formatForwardEquation, formatBackwardEquation,
    symbolicProductSum, numericProductSum, sigmaUDisplay, sigmaLDisplay,
    buildUFormulaLines, buildLFormulaLines, buildYFormulaLines, currentLBoldPositions, currentUBoldPositions
  },

  n = data["n"];
  A = data["A"];
  b = data["b"];
  vars = data["Vars"];
  xSymbols = vars;

  addHeader[text_] := AppendTo[content, makeStepHeader[text]];
  addSubHeader[text_] := AppendTo[content, Style[text, Bold, FontSize -> 15]];
  addText[text_] := AppendTo[content, text];
  addMatrixPair[l_, u_, lBold_List : {}, uBold_List : {}] := AppendTo[
    content, luMatrixPairGrid[l, u, lBold, uBold]
  ];  addVector[label_, vec_] := AppendTo[content, luVectorGrid[label, vec]];
  addFormula[expr_] := AppendTo[content, expr];
  resultStyle[expr_] := Style[expr, Bold, Blue];

  currentLBoldPositions[step_] := Join[
    Table[{r, r}, {r, 1, n}], Flatten[Table[{r, c}, {c, 1, Min[step, n - 1]}, {r, c + 1, n}], 1]
  ];

  currentUBoldPositions[step_] := Flatten[
    Table[{r, c}, {r, 1, step}, {c, r, n}], 1
  ];

  (* pomocný formát lineárnej rovnice so znamienkami *)
  formatLinearEquation[coeffList_, symbolList_, rhs_] := Module[
    {pairs, nz, firstPair, pieces = {}, c, s, absC},
    pairs = Transpose[{coeffList, symbolList}];
    nz = Select[pairs, #[[1]] =!= 0 &];

    If[nz === {},
      Return[Row[{0, " = ", tft[rhs]}]]
    ];

    firstPair = First[nz];
    c = firstPair[[1]];
    s = firstPair[[2]];
    absC = Abs[c];

    AppendTo[
      pieces,
      Which[
        c === 1, s,
        c === -1, Row[{"-", s}],
        c < 0, Row[{"-", tft[absC], "\[CenterDot]", s}],
        True, Row[{tft[c], "\[CenterDot]", s}]
      ]
    ];

    Do[
      c = pair[[1]];
      s = pair[[2]];
      absC = Abs[c];

      AppendTo[
        pieces,
        Which[
          c === 1, Row[{" + ", s}],
          c === -1, Row[{" - ", s}],
          c > 0, Row[{" + ", tft[absC], "\[CenterDot]", s}],
          True, Row[{" - ", tft[absC], "\[CenterDot]", s}]
        ]
      ];
      ,
      {pair, Rest[nz]}
    ];

    Row[{Row[pieces], " = ", tft[rhs]}]
  ];

  (* rovnica pre L.y = b *)
  formatForwardEquation[row_, rhs_, i_] := Module[
    {coeffList, symbolList},
    coeffList = row[[1 ;; i]];
    symbolList = Table[luScalarSymbol["y", k], {k, 1, i}];
    formatLinearEquation[coeffList, symbolList, rhs]
  ];

  (* rovnica pre U.x = y *)
  formatBackwardEquation[row_, rhs_, i_] := Module[
    {coeffList, symbolList},
    coeffList = row[[i ;; n]];
    symbolList = vars[[i ;; n]];
    formatLinearEquation[coeffList, symbolList, rhs]
  ];

  (* symbolický rozpis sumy bez sigma *)
  symbolicProductSum[factors_List] := If[
    factors === {},
    tft[0],
    Row @ Riffle[(Row[#[[1 ;; 2]]] & /@ factors), " + "]
  ];

  (* číselný rozpis súčinu bez sčítania *)
  numericProductSum[terms_List] := If[
    terms === {},
    tft[0],
    Row @ Riffle[
      (Row[{luFactorDisplay[#[[1]]], "\[CenterDot]", luFactorDisplay[#[[2]]]}] & /@ terms),
      " + "
    ]
  ];

  (* telo sumy so symbolickým k bez interného k$123 *)
  sigmaUDisplay[rowIdx_, colIdx_] := Row[{
    Subscript[Style["l", Italic], Row[{rowIdx, ",", Style["k", Italic]}]],
    Subscript[Style["u", Italic], Row[{Style["k", Italic], ",", colIdx}]]
  }];

  sigmaLDisplay[rowIdx_, colIdx_] := Row[{
    Subscript[Style["l", Italic], Row[{rowIdx, ",", Style["k", Italic]}]],
    Subscript[Style["u", Italic], Row[{Style["k", Italic], ",", colIdx}]]
  }];

  (* riadky výpočtu pre prvok U *)
  buildUFormulaLines[i_, j_, terms_, value_] := Module[
    {symbolicTerms},
    If[terms === {},
      {
        Row[{
          lhsStyle[luEntrySymbol["u", i, j]], " = ",
          luEntrySymbol["a", i, j], " = ",
          resultStyle[tft[value]]
        }]
      },
      symbolicTerms = Table[
        {luEntrySymbol["l", i, kk], luEntrySymbol["u", kk, j]},
        {kk, 1, i - 1}
      ];
      {
        Row[{
          lhsStyle[luEntrySymbol["u", i, j]], " = ",
          luEntrySymbol["a", i, j], " - ",
          Underoverscript["\[Sum]", Row[{Style["k", Italic], " = 1"}], i - 1],
          sigmaUDisplay[i, j],
          " = ",
          luEntrySymbol["a", i, j], " - (", symbolicProductSum[symbolicTerms], ")"
        }],
        Row[{
          lhsStyle[luEntrySymbol["u", i, j]], " = ",
          tft[A[[i, j]]], " - (", numericProductSum[terms], ")"
        }],
        Row[{
          lhsStyle[luEntrySymbol["u", i, j]], " = ",
          tft[A[[i, j]]], " - ", luWrappedSumDisplay[terms], " = ",
          resultStyle[tft[value]]
        }]
      }
    ]
  ];
  (* riadky výpočtu pre prvok L *)
  buildLFormulaLines[j_, i_, terms_, pivot_, value_] := Module[
    {symbolicTerms},
    If[terms === {},
      {
        Row[{
          lhsStyle[luEntrySymbol["l", j, i]], " = ",
          luEntrySymbol["a", j, i], " / ", luEntrySymbol["u", i, i],
          " = ", tft[A[[j, i]]], " / ", tft[pivot],
          " = ", resultStyle[tft[value]]
        }]
      },
      symbolicTerms = Table[
        {luEntrySymbol["l", j, kk], luEntrySymbol["u", kk, i]},
        {kk, 1, i - 1}
      ];
      {
        Row[{
          lhsStyle[luEntrySymbol["l", j, i]], " = (",
          luEntrySymbol["a", j, i], " - ",
          Underoverscript["\[Sum]", Row[{Style["k", Italic], " = 1"}], i - 1],
          sigmaLDisplay[j, i],
          ") / ", luEntrySymbol["u", i, i],
          " = (",
          luEntrySymbol["a", j, i], " - (", symbolicProductSum[symbolicTerms], ")) / ",
          luEntrySymbol["u", i, i]
        }],
        Row[{
          lhsStyle[luEntrySymbol["l", j, i]], " = (",
          tft[A[[j, i]]], " - (", numericProductSum[terms], ")) / ", tft[pivot]
        }],
        Row[{
          lhsStyle[luEntrySymbol["l", j, i]], " = (",
          tft[A[[j, i]]], " - ", luWrappedSumDisplay[terms], ") / ", tft[pivot],
          " = ", resultStyle[tft[value]]
        }]
      }
    ]
  ];
  buildYFormulaLines[i_, terms_, value_] := Module[{},
    If[terms === {},
      {
        Row[{
          lhsStyle[luScalarSymbol["y", i]], " = ",
          resultStyle[tft[value]]
        }]
      },
      {
        formatForwardEquation[L[[i]], b[[i]], i],
        Row[{
          lhsStyle[luScalarSymbol["y", i]], " = ",
          tft[b[[i]]], " - (", numericProductSum[terms], ")"
        }],
        Row[{
          lhsStyle[luScalarSymbol["y", i]], " = ",
          tft[b[[i]]], " - ", luWrappedSumDisplay[terms], " = ",
          resultStyle[tft[value]]
        }]
      }
    ]
  ];
  addHeader["Prepis sústavy do maticového tvaru"];
  addText["Sústavu zapíšeme v maticovom tvare A \[CenterDot] x = b."];

  AppendTo[
    content,
    highlightGrid @ Grid[
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
    ]
  ];

  addText["Pri LU rozklade chceme maticu A zapísať ako súčin A = L \[CenterDot] U."];

  AppendTo[
    content,
    highlightGrid @ Grid[
      {
        {Style["A \[CenterDot] x = b", Bold]},
        {Style["A = L \[CenterDot] U", Bold]},
        {Style["Potom označíme", Plain]},
        {Style["U \[CenterDot] x = y", Bold]},
        {Style["a sústavu vyriešime v dvoch krokoch:", Plain]},
        {Style["1. vyriešime L \[CenterDot] y = b", Plain]},
        {Style["2. potom vyriešime U \[CenterDot] x = y", Plain]}
      },
      Alignment -> Left,
      Spacings -> {1, 0.6}
    ]
  ];

  luData = luSolveData[A, b];

  If[luData === $Failed,
    addHeader["Výsledok"];
    addText["Pri tejto matici sa počas Doolittleho rozkladu objavil nulový pivot, takže LU rozklad bez pivotovania nemožno použiť."];
    Return[<|"Content" -> content, "Solution" -> Missing["NotAvailable"]|>];
  ];

  L = IdentityMatrix[n];
  U = ConstantArray[0, {n, n}];

  addHeader["Inicializácia matíc"];
  addText["Na začiatku poznáme iba jednotkovú diagonálu matice L. Ostatné prvky matíc L a U budeme dopočítavať postupne po krokoch."];
  addMatrixPair[L, U, Table[{r, r}, {r, 1, n}], {}];

  Do[
    If[
      i < n,
      addHeader[
        "Krok " <> ToString[i] <> " – výpočet " <> ToString[i] <>
            ". riadku matice U a " <> ToString[i] <> ". stĺpca matice L"
      ],
      addHeader["Krok " <> ToString[i] <> " – výpočet posledného prvku matice U"]
    ];

    If[i < n,
      addText["Najprv určujeme prvky matice U."];
      addSubHeader["Prvky matice U:"];
      Do[
        terms = Table[{L[[i, kk]], U[[kk, j]]}, {kk, 1, i - 1}];
        sumTerm = Total[Times @@@ terms];
        U[[i, j]] = Together[A[[i, j]] - sumTerm];
        Scan[addFormula, buildUFormulaLines[i, j, terms, U[[i, j]]]];

        If[j < n && i > 1,
          addGap[content, 3]
        ];
        ,
        {j, i, n}
      ];
      ,
      addSubHeader["Prvok matice U:"];
      terms = Table[{L[[i, kk]], U[[kk, i]]}, {kk, 1, i - 1}];
      sumTerm = Total[Times @@@ terms];
      U[[i, i]] = Together[A[[i, i]] - sumTerm];
      Scan[addFormula, buildUFormulaLines[i, i, terms, U[[i, i]]]];
    ];

    pivotValue = Together[U[[i, i]]];
    addText[Row[{"Pivot v tomto kroku je ", luEntrySymbol["u", i, i], " = ", tft[pivotValue], "."}]];

    If[i < n,
      addSubHeader["Prvky matice L:"];
      Do[
        terms = Table[{L[[j, kk]], U[[kk, i]]}, {kk, 1, i - 1}];
        sumTerm = Total[Times @@@ terms];
        L[[j, i]] = Together[(A[[j, i]] - sumTerm)/pivotValue];
        Scan[addFormula, buildLFormulaLines[j, i, terms, pivotValue, L[[j, i]]]];

        If[j < n && i > 1,
          addGap[content, 5]
        ];
        ,
        {j, i + 1, n}
      ];
    ];

    If[i < n,
      addText["Po tomto kroku majú matice tvar:"];
      addMatrixPair[L, U, currentLBoldPositions[i], currentUBoldPositions[i]];
    ];
    ,
    {i, 1, n}
  ];

  addHeader["Hotový rozklad A = L \[CenterDot] U"];
  addText["Po dokončení výpočtu máme maticu L s jednotkovou diagonálou a hornú trojuholníkovú maticu U."];
  addMatrixPair[
    L, U,
    Join[Table[{r, r}, {r, 1, n}], Flatten[Table[{r, c}, {c, 1, n - 1}, {r, c + 1, n}], 1]],
    Flatten[Table[{r, c}, {r, 1, n}, {c, r, n}], 1]
  ];
  addHeader["Overenie rozkladu L \[CenterDot] U = A"];
  luProduct = Together[L . U];
  addText["Každý prvok súčinu L \[CenterDot] U vzniká ako skalárny súčin príslušného riadku matice L a stĺpca matice U. Najprv si ukážeme dosadenie, potom vypočítané súčiny a nakoniec výsledný prvok."];
  AppendTo[content, luMatrixProductDisplay[L, U]];

  AppendTo[
    content,
    Grid[
      {{
        Style["L \[CenterDot] U =", FontSize -> 13],
        TraditionalForm[MatrixForm[luProduct]],
        If[luProduct === A, Style["OK", Darker[Green], Bold], Style["CHYBA", Red, Bold]]
      }},
      Alignment -> Left,
      Spacings -> {1, 0.4},
      BaseStyle -> {FontSize -> 13}
    ]
  ];

  addHeader["Riešenie pomocnej sústavy L \[CenterDot] y = b"];
  addText["Keďže L je dolná trojuholníková matica s jednotkami na diagonále, pomocný vektor y určujeme dopredným dosadzovaním zhora nadol."];
  AppendTo[content, alignedAugmentedMatrix[augFromAb[L, b], {}, <|"BoldDiagonal" -> True|>]];

  y = ConstantArray[0, n];
  Do[
    terms = Table[{L[[i, k]], y[[k]]}, {k, 1, i - 1}];
    sumTerm = Total[Times @@@ terms];
    y[[i]] = Together[b[[i]] - sumTerm];

    Scan[addFormula, buildYFormulaLines[i, terms, y[[i]]]];

    If[i < n,
      addGap[content, 5]
    ];
    ,
    {i, 1, n}
  ];

  addVector["y", y];
  addHeader["Riešenie sústavy U \[CenterDot] x = y"];
  addText["Po určení pomocného vektora y riešime hornú trojuholníkovú sústavu U \[CenterDot] x = y spätným dosadzovaním od poslednej rovnice."];
  AppendTo[content, alignedAugmentedMatrix[augFromAb[U, y], {}, <|"BoldDiagonal" -> True|>]];

  x = ConstantArray[0, n];
  Do[
    terms = Table[{U[[i, k]], x[[k]]}, {k, i + 1, n}];
    sumTerm = Total[Times @@@ terms];
    x[[i]] = Together[(y[[i]] - sumTerm)/U[[i, i]]];

    addFormula[formatBackwardEquation[U[[i]], y[[i]], i]];

    addFormula[
      If[
        terms === {},
        Row[{lhsStyle[vars[[i]]], " = ", tft[y[[i]]], " / ", tft[U[[i, i]]], " = ", resultStyle[tft[x[[i]]]]}],
        Row[{
          lhsStyle[vars[[i]]], " = (", tft[y[[i]]], " - ", luWrappedSumDisplay[terms], ") / ", tft[U[[i, i]]],
          " = ", resultStyle[tft[x[[i]]]]
        }]
      ]
    ];
    ,
    {i, n, 1, -1}
  ];

  addHeader["Skúška správnosti"];
  addText["Overíme najprv rozklad A = L \[CenterDot] U, potom pomocnú sústavu L \[CenterDot] y = b, ďalej sústavu U \[CenterDot] x = y a napokon pôvodnú sústavu A \[CenterDot] x = b."];

  lowerCheck = Together[L . y];
  upperCheck = Together[U . x];

  AppendTo[
    content,
    Grid[
      {{
        Style["L \[CenterDot] U =", FontSize -> 13],
        TraditionalForm[MatrixForm[luProduct]],
        Style["A =", FontSize -> 13],
        TraditionalForm[MatrixForm[A]],
        If[luProduct === A, Style["OK", Darker[Green], Bold], Style["CHYBA", Red, Bold]]
      }},
      Alignment -> Left,
      Spacings -> {1, 0.4},
      BaseStyle -> {FontSize -> 13}
    ]
  ];

  addGap[content, 6];

  AppendTo[
    content,
    Grid[
      {{
        Style["L \[CenterDot] y =", FontSize -> 13],
        TraditionalForm[MatrixForm[lowerCheck]],
        Style["b =", FontSize -> 13],
        TraditionalForm[MatrixForm[b]],
        If[lowerCheck === b, Style["OK", Darker[Green], Bold], Style["CHYBA", Red, Bold]]
      }},
      Alignment -> Left,
      Spacings -> {1, 0.4},
      BaseStyle -> {FontSize -> 13}
    ]
  ];

  addGap[content, 6];

  AppendTo[
    content,
    Grid[
      {{
        Style["U \[CenterDot] x =", FontSize -> 13],
        TraditionalForm[MatrixForm[upperCheck]],
        Style["y =", FontSize -> 13],
        TraditionalForm[MatrixForm[y]],
        If[upperCheck === y, Style["OK", Darker[Green], Bold], Style["CHYBA", Red, Bold]]
      }},
      Alignment -> Left,
      Spacings -> {1, 0.4},
      BaseStyle -> {FontSize -> 13}
    ]
  ];

  <|
    "Content" -> content,
    "Solution" -> x,
    "L" -> L,
    "U" -> U,
    "Y" -> y
  |>
];

stepsCramer[data_Association] := Module[
  {
    content = {}, n, A, b, vars, solveData, detData, auxData, auxLabel,
    addHeader, addText
  },

  n = data["n"];
  A = data["A"];
  b = data["b"];
  vars = data["Vars"];

  solveData = cramerSolveData[A, b];

  addHeader[text_] := AppendTo[content, makeStepHeader[text]];
  addText[text_] := AppendTo[content, text];

  addHeader["Prepis sústavy do maticového tvaru"];
  addText["Sústavu zapíšeme v tvare A \[CenterDot] x = b. Pri Cramerovom pravidle potrebujeme determinant matice A a determinanty pomocných matíc, ktoré vzniknú nahradením jedného stĺpca vektorom b."];
  AppendTo[content, highlightGrid @ Grid[
    {{
      Style["A =", Bold, FontSize -> 16],
      TraditionalForm[MatrixForm[A]],
      Spacer[18],
      Style["b =", Bold, FontSize -> 16],
      TraditionalForm[MatrixForm[b]]
    }},
    Alignment -> Left,
    Spacings -> {2, 1}
  ]];

  addHeader["Výpočet det(A)"];
  detData = renderCramerDeterminant[A, Style["A", Italic]];
  content = Join[content, detData["Content"]];

  If[solveData["DetA"] === 0,
    addHeader["Záver"];
    addText["Keďže det(A) = 0, Cramerovo pravidlo nemožno použiť."];
    Return[<|
      "Content" -> content,
      "Solution" -> Missing["NotAvailable"],
      "DetA" -> solveData["DetA"],
      "AuxDeterminants" -> {}
    |>];
  ];

  Do[
    auxLabel = cramerMatrixLabel[vars[[i]]];

    addHeader["Pomocná matica pre premennú " <> ToString[vars[[i]], InputForm]];
    addText[Row[{
      "Maticu ", auxLabel,
      " dostaneme tak, že ",
      i, ". stĺpec matice A nahradíme vektorom b."
    }]];

    auxData = renderCramerDeterminant[solveData["AuxMatrices"][[i]], auxLabel];
    content = Join[content, auxData["Content"]];

    AppendTo[content, highlightGrid @ Grid[
      {{
        tf[lhsStyle[vars[[i]]]],
        "=",
        Row[{cramerDetLabel[auxLabel], " / ", cramerDetLabel[Style["A", Italic]]}],
        "=",
        Row[{tft[solveData["AuxDeterminants"][[i]]], " / ", tft[solveData["DetA"]]}],
        "=",
        cramerResultStyle[tft[solveData["Solution"][[i]]]]
      }},
      Alignment -> {{Right, Center, Left, Center, Left, Center, Left}},
      BaseStyle -> {FontSize -> 15}
    ]];
    ,
    {i, 1, n}
  ];

  addHeader["Skúška správnosti"];
  addText["Overíme porovnaním A \[CenterDot] x s pravou stranou b (po riadkoch)."];
  content = Join[content, verificationSteps[data, solveData["Solution"]]];

  addHeader["Záver"];
  AppendTo[content, cramerSolutionGrid[vars, solveData["Solution"]]];

  <|
    "Content" -> content,
    "Solution" -> solveData["Solution"],
    "DetA" -> solveData["DetA"],
    "AuxDeterminants" -> solveData["AuxDeterminants"]
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

printTaskCramer[data_Association, vars_List] := Module[{},
  printTextCell["Riešte sústavu rovníc pomocou Cramerovho pravidla."];
  printFormulaCell @ Grid[
    List /@ (tf /@ buildTaskEquations[data["A"], data["b"], vars]),
    Alignment -> Left,
    Spacings -> {0, 0.8}
  ];
  printTextCell["Nezabudnite najprv vypočítať determinant matice A a potom determinanty matíc vzniknutých z A dosadením vektora b do jednotlivých stĺpcov."];
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

printResultCramer[data_Association, vars_List, st_, steps_] := Module[
  {solveData, detA, auxDeterminants, solution},

  solveData = cramerSolveData[data["A"], data["b"]];

  detA = Which[
    AssociationQ[steps] && KeyExistsQ[steps, "DetA"], steps["DetA"],
    True, solveData["DetA"]
  ];

  auxDeterminants = Which[
    AssociationQ[steps] && KeyExistsQ[steps, "AuxDeterminants"], steps["AuxDeterminants"],
    True, solveData["AuxDeterminants"]
  ];

  solution = Which[
    AssociationQ[steps] && KeyExistsQ[steps, "Solution"], steps["Solution"],
    True, solveData["Solution"]
  ];

  printTextCell["Determinant matice A:"];
  printFormulaCell[
    cramerEqualityGrid[cramerDetLabel[Style["A", Italic]], detA]
  ];

  printTextCell["Pomocné determinanty:"];
  printFormulaCell[
    highlightGrid @ Grid[
      Table[
        {
          cramerDetLabel[cramerMatrixLabel[vars[[i]]]],
          "=",
          cramerResultStyle[tft[auxDeterminants[[i]]]]
        },
        {i, 1, Length[vars]}
      ],
      Alignment -> {{Right, Center, Left}},
      BaseStyle -> {FontSize -> 15}
    ]
  ];

  printTextCell["Riešenie sústavy:"];
  printFormulaCell[
    cramerSolutionGrid[vars, solution]
  ];
];

runMatrixGenerator[spec_Association, diff_String, mode_String, opts : OptionsPattern[]] := Module[
  {n, vars, st, tri, data, steps = Missing["NotComputed"], validateExtraQ, resolveExtra,
    sectionTitle, stepFn, scrambleFn, taskPrinter, resultPrinter, useRetryQ, pivotMode,
    boundAugFn, boundCheckFn},

  If[!TrueQ[ValidateDifficulty[diff]],
    Message[MessageName[spec["MsgPrefix"], "baddiff"], diff];
    Return[]
  ];

  If[!TrueQ[ValidateMode[mode]],
    Message[MessageName[spec["MsgPrefix"], "badmode"], mode];
    Return[]
  ];

  With[{stOpt = OptionValue[spec["EntryFn"], {opts}, SolutionType]},
    If[!TrueQ[ValidateSolutionType[stOpt]],
      Message[MessageName[spec["MsgPrefix"], "badst"], stOpt];
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
    With[{msg = spec["MsgPrefix"]},
      Message[msg::fail]
    ];
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
          Message[MessageName[specLocal["MsgPrefix"], "badtri"], triOpt];
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
          Message[MessageName[specLocal["MsgPrefix"], "badst"], stOpt];
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
          Message[MessageName[specLocal["MsgPrefix"], "badst"], stOpt];
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
    "ForwardBoundCheckFn" -> Function[{data, pivotMode}, luDecompositionWithinBoundsQ[data]]
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

GenCramer[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenCramer,
    "MsgPrefix" -> GenCramer,
    "DimKey" -> "Cramer",
    "SectionTitle" -> "Cramerovo pravidlo",
    "ScrambleFn" -> genScrambleCramer,
    "StepsFn" -> stepsCramer,
    "ValidateExtra" -> Function[{specLocal, passedOpts},
      With[{stOpt = OptionValue[specLocal["EntryFn"], passedOpts, SolutionType]},
        If[stOpt =!= "ONE",
          Message[MessageName[specLocal["MsgPrefix"], "badst"], stOpt];
          False,
          True
        ]
      ]
    ],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "TaskPrinter" -> printTaskCramer,
    "ResultPrinter" -> printResultCramer
  |>;

  runMatrixGenerator[spec, diff, mode, opts]
];

End[];
EndPackage[];
