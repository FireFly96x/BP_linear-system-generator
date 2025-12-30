(* ::Package:: *)

(*
  Package: MatrixGenerator
  Description: Generates didactic materials for solving triangular linear systems using augmented matrices
               and pure row-based substitution (no equations after the conversion step).
  Guarantees: Integers only, coefficients and RHS always within bounds, no fractions anywhere.
  Updated: Dynamic step numbering added, fixed validation, strict integer rules, visual improvements, new bounds.
*)

BeginPackage["MojeGeneratory`MatrixGenerator`"];

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
s pivotovaním výberom najväčšieho možného pivotu v stĺpci, so zobrazením celočíselných riadkových úprav na augmentovanej matici.

diff: \"EASY\" (4x4), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> Automatic | \"ONE\" | \"NONE\" | \"INFINITE\"   (Automatic vyberá typ náhodne)";

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

$CommonGeneratorOptions = {SolutionType -> Automatic, TriangularType -> Automatic};

Options[GenTriangular] = $CommonGeneratorOptions;
Options[GenGauss] = $CommonGeneratorOptions;
Options[GenGaussJordan] = $CommonGeneratorOptions;
Options[GenGaussJordanPivot] = $CommonGeneratorOptions;

$FailedScrambleCount;

Begin["`Private`"];

(* ~-~-~ VALIDATION ~-~-~ *)

DimensionByDifficulty[diff_String] := Switch[diff, "EASY", 4, "MEDIUM", 5, "HARD", 6];

ValidateDifficulty[diff_] := MemberQ[{"EASY", "MEDIUM", "HARD"}, diff];
ValidateMode[mode_] := MemberQ[{"TASK", "TASK_RESULT", "TASK_STEPS_RESULT"}, mode];
ValidateSolutionType[st_] := TrueQ[st === Automatic] || MemberQ[{"ONE", "NONE", "INFINITE"}, st];
ResolveSolutionType[st_] := If[st =!= Automatic, st, RandomChoice[{0.6, 0.2, 0.2} -> {"ONE", "NONE", "INFINITE"}]];
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

(* pre output vypis infinte ↓, neskor vymyslim inak *)
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
rowNoteDivide[i_, p_] := Row[{"R", i, " ← R", i, " / ", tf[p]}];
rowApplyDivide[aug_, i_Integer, p_Integer] := ReplacePart[aug, i -> (aug[[i]]/p)];

(* note pre kombináciu riadkov *)
rowNoteCombine[i_, terms_List] := Module[{base = Row[{"R", i, " ← R", i}]},
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

    AppendTo[content,
      augRender3[before, mid, after2, notes, notes2, hi1, hi2, hi3]
    ];
    after2
    ,
    after2 = elimRes["Aug"];
    AppendTo[content,
      augRender2[before, after2, notes, hi1, hi2]
    ];
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

rowNoteSwap[i_, k_] := Row[{"R", i, " ↔ R", k}];

rowApplySwap[aug_, i_Integer, k_Integer] := ReplacePart[aug, {i -> aug[[k]], k -> aug[[i]]}];

rowNoteElim[r_, i_, p2_, a2_] := Module[{leftPart, rightPart, op},
  leftPart = If[p2 === 1, Row[{"R", r}], Row[{tf[p2], "\[CenterDot]", "R", r}]];
  rightPart = If[Abs[a2] === 1, Row[{"R", i}], Row[{tf[Abs[a2]], "\[CenterDot]", "R", i}]];
  op = If[a2 < 0, " + ", " - "];

  Row[{"R", r, " ← ", leftPart, op, rightPart}]
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

(* ~-~-~ MATRIX GENERATION ~-~-~ *)

$bRange = {-10, 10};
nonzeroRange[min_, max_] := DeleteCases[Range[min, max], 0];
boundsByDifficulty[diff_String] := Switch[diff, "EASY", 50, "MEDIUM", 45, "HARD", 40];
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

  (* pri vašom smere „upper“ (zdola nahor) dávame špeciálny riadok dole *)
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

(* generovanie dát aby postup bol bez zlomkov *)
generateData[diff_String, n_, solType_, triType_, scrambleFn_] := Module[{solved, augSolved, augTask, A, b, vars},
  solved = makeDiagonalAug[diff, n, solType, triType];
  augSolved = solved["Aug"];
  augTask = scrambleFn[diff, augSolved, triType, solType];

  A = augTask[[All, 1 ;; n]];
  b = augTask[[All, n + 1]];
  vars = buildVars[n];

  <|"A" -> A, "b" -> b, "x" -> solved["x"], "TriType" -> triType, "SolutionType" -> solType,
    "Aug" -> augTask, "SolvedAug" -> augSolved, "Vars" -> vars, "n" -> n,
    "BadRow" -> solved["BadRow"], "ParamIdx" -> solved["ParamIdx"]|>
];

genScrambleTriang[diff_String, aug0_, triType_String, solType_String : "ONE", Gauss_ : True] := Module[{aug = aug0, n = Length[aug0], bnd, kSet, withinQ, protectedLastRowQ, chooseK, chooseS, i, r, k, s},
  bnd = boundsByDifficulty[diff];
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
  bnd = boundsByDifficulty[diff];

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

            (* ak bola aj kombinácia, zobraz 3; inak stačí 2 stlpce *)
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
  addText["Postupujeme po stĺpcoch zľava doprava. V každom kroku zvolíme pivot (ak treba, prehodíme riadky) a následne rušíme prvky pod pivotom celočíselnými riadkovými úpravami. Koeficienty priebežne skracujeme pomocou gcd a riadky normalizujeme."];

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
  {content = {}, n, aug, vars, st, addHeader, addText, addMatrix, notes, before, after, kPivot, elimRes, pNow,
    solLocal, paramIdx, solExprs, row, pivot, knownTerm, pivotRowFn},

  n = data["n"]; aug = data["Aug"]; vars = data["Vars"]; st = data["SolutionType"];

  pivotRowFn = If[pivotQ, choosePivotRow, choosePivotRowIfZero];

  addHeader[text_] := AppendTo[content, makeStepHeader[text]];
  addText[text_] := AppendTo[content, text];
  addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] := AppendTo[content, alignedAugmentedMatrix[m, rowNotes, hi]];

  addHeader["Prepis sústavy do augmentovanej matice"];
  addText["Sústavu prepíšeme do augmentovanej matice a vykonáme Gaussovu–Jordanovu elimináciu tak, aby sa ľavá časť zmenila na jednotkovú maticu."];
  addMatrix[aug];

  addHeader["Dopredná eliminácia (nulovanie pod diagonálou)"];
  addText[
    "Postupujeme po stĺpcoch zľava doprava. V každom stĺpci vyberieme pivot ako nenulový prvok s najmenšou absolútnou hodnotou a prípadne prehodíme riadky. Pomocou pivotového riadku potom nulujeme prvky pod ním celočíselnými úpravami. Po každom kroku riadky skracujeme pomocou najväčšieho spoločného deliteľa (gcd) a normalizujeme."
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
      ], {r, i + 1, n}
    ], {i, 1, n - 1}
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
      ], {r, 1, i - 1}
    ], {i, n, 2, -1}
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
    , {i, 1, n}
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
      , {i, n, 1, -1}
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

(* ~-~-~ VERIFICATION STEPS ~-~-~ *)

verificationSteps[data_Association, sol_List] := Module[{content = {}, A = data["A"], b = data["b"], n = data["n"], lhs},

  Do[
    lhs = A[[i]] . sol;
    AppendTo[content,
      Grid[
        {
          {Row[{"LS", i, ":  ", tf[A[[i]]], " \[CenterDot] ", tf[sol], " = ", tft[lhs]}]},
          {Row[{"PS", i, " = ", tft[b[[i]]]}]},
          {If[lhs === b[[i]], Style["ĽS = PS (OK)", Darker[Green]], Style["ĽS \[NotEqual] PS (CHYBA)", Red]]}
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
          Style["hodnosti sa nerovnajú tak, ako majú pre spor \[Dash] over postup (CHYBA)", Red]
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
          {Row[{"ĽS - PS = ", TraditionalForm[diff]}]},
          {If[okQ, Style["ĽS = PS (OK)", Darker[Green]], Style["ĽS \[NotEqual] PS (CHYBA)", Red]]}
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
          Style["hodnosti nespĺňajú podmienku pre nekonečne veľa riešení \[Dash] over postup (CHYBA)", Red]
        ]}
      },
      Alignment -> Left, Spacings -> {0, 0.4}, BaseStyle -> {FontSize -> 13}
    ]
  ];
  content
];


(* ~-~-~ MAIN CONTROLLER ~-~-~ *)

runMatrixGenerator[spec_Association, diff_String, mode_String, opts : OptionsPattern[]] := Module[{n, vars, st, tri, data, steps, validateExtraQ, resolveExtra, sectionTitle, stepFn, scrambleFn},
  (* spoločné validácie *)
  If[!TrueQ[ValidateDifficulty[diff]], Message[spec["MsgPrefix"]::baddiff, diff]; Return[]];
  If[!TrueQ[ValidateMode[mode]], Message[spec["MsgPrefix"]::badmode, mode]; Return[]];
  With[{stOpt = OptionValue[spec["EntryFn"], {opts}, SolutionType]},
    If[!TrueQ[ValidateSolutionType[stOpt]],
      Message[spec["MsgPrefix"]::badst, stOpt]; Return[]
    ];
  ];

  (* špecifické validácie *)
  validateExtraQ = Lookup[spec, "ValidateExtra", (True &)];
  If[!TrueQ[validateExtraQ[spec, {opts}]], Return[]];

  (* riešenie typu sústavy *)
  st = ResolveSolutionType[OptionValue[spec["EntryFn"], {opts}, SolutionType]];

  (* špecifické riešenie parametrov *)
  resolveExtra = Lookup[spec, "ResolveExtra", (Missing["NotUsed"] &)];
  tri = resolveExtra[spec, {opts}];

  (* rozmery *)
  n = DimensionByDifficulty[diff];
  vars = buildVars[n];

  (* dáta *)
  scrambleFn = spec["ScrambleFn"];
  data = generateData[diff, n, st, tri, scrambleFn];

  (* tlač zadania *)
  sectionTitle = spec["SectionTitle"];
  printSectionCell[sectionTitle];
  printSubsectionCell["Zadanie"];
  printTextCell["Riešte sústavu rovníc v množine celých čísel."];
  printFormulaCell @ Grid[List /@ (tf /@ buildTaskEquations[data["A"], data["b"], vars]), Alignment -> Left, Spacings -> {0, 0.8}];

  printTextCell["Riešte pomocou augmentovanej matice."];

  (* postup *)
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

  (* výsledok *)
  If[mode =!= "TASK",
    If[!(mode === "TASK_STEPS_RESULT" && TrueQ @ Lookup[spec, "InlineSolutionQ", False]),
      printSubsectionCell["Výsledok"];

      If[st === "ONE",
        printFormulaCell[
          Row[Flatten[{"(", Riffle[vars, ", "], ") = (", Riffle[TraditionalForm /@ data["x"], ", "], ")"}]]
        ]
      ];

      If[st === "NONE", printTextCell["Sústava nemá riešenie."]];

      If[st === "INFINITE",
        printTextCell["Sústava má nekonečne veľa riešení."];
        Module[{solExprs = infiniteSolutionFromSolvedAug[data]},
          printFormulaCell[
            Row[{"K = { [", Row @ Riffle[TraditionalForm /@ solExprs, ", "], "], ", \[FormalT], " \[Element] ", Integers, " }"}]
          ];
        ];
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
    "EntryFn" -> GenGauss, "MsgPrefix" -> GenGauss, "DimKey" -> "Gauss", "SectionTitle" -> "Gaussova eliminačná metóda",
    "ScrambleFn" -> genScrambleGauss, "StepsFn" -> stepsGauss, "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"]
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

GenGaussJordan[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenGaussJordan, "MsgPrefix" -> GenGaussJordan, "DimKey" -> "GaussJordan", "SectionTitle" -> "Gauss-Jordanova metóda",
    "ScrambleFn" -> genScrambleGauss, "StepsFn" -> (stepsGaussJordan[#, False] &),
    "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"]
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

GenGaussJordanPivot[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenGaussJordanPivot, "MsgPrefix" -> GenGaussJordanPivot, "DimKey" -> "GaussJordanPivot", "SectionTitle" -> "Gauss-Jordanova metóda s pivotovaním",
    "ScrambleFn" -> genScrambleGauss, "StepsFn" -> (stepsGaussJordan[#, True] &),
    "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"]
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

End[];
EndPackage[];