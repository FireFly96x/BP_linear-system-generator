(* ::Package:: *)

BeginPackage["`MatrixGenerator`"];

$CharacterEncoding = "UTF-8";
Internal`$ContextMarks = False;

GenTriangular::usage = "GenTriangular[diff, mode, opts] vygeneruje didaktick\[YAcute] pr\[IAcute]klad rie\[SHacek]enia s\[UAcute]stavy line\[AAcute]rnych rovn\[IAcute]c v trojuholn\[IAcute]kovej s\[UAcute]stave pomocou augmentovanej matice \

diff: \"EASY\" (4x4), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts:
  SolutionType -> Automatic | \"ONE\" | \"NONE\" | \"INFINITE\"   (Automatic vyber\[AAcute] typ n\[AAcute]hodne)
  TriangularType -> Automatic | \"L\" | \"U\"";

GenGauss::usage = "GenGauss[diff, mode, opts] vygeneruje didaktick\[YAcute] pr\[IAcute]klad rie\[SHacek]enia s\[UAcute]stavy line\[AAcute]rnych rovn\[IAcute]c pomocou Gaussovej met\[OAcute]dy \
diff: \"EASY\" (4x4), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> Automatic | \"ONE\" | \"NONE\" | \"INFINITE\"   (Automatic vyber\[AAcute] typ n\[AAcute]hodne)";

GenGaussJordan::usage = "GenGaussJordan[diff, mode, opts] vygeneruje didaktick\[YAcute] pr\[IAcute]klad rie\[SHacek]enia s\[UAcute]stavy line\[AAcute]rnych rovn\[IAcute]c pomocou Gaussovej-Jordanovej met\[OAcute]dy \
(prevod na tvar (I | x)) so zobrazen\[IAcute]m celo\[CHacek]\[IAcute]seln\[YAcute]ch riadkov\[YAcute]ch \[UAcute]prav na augmentovanej matici.

diff: \"EASY\" (4x4), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> Automatic | \"ONE\" | \"NONE\" | \"INFINITE\"   (Automatic vyber\[AAcute] typ n\[AAcute]hodne)";

GenGaussJordanPivot::usage = "GenGaussJordanPivot[diff, mode, opts] vygeneruje didaktick\[YAcute] pr\[IAcute]klad rie\[SHacek]enia s\[UAcute]stavy line\[AAcute]rnych rovn\[IAcute]c pomocou Gaussovej-Jordanovej met\[OAcute]dy \
s pivotovan\[IAcute]m v\[YAcute]berom najmen\[SHacek]ieho mo\[ZHacek]n\[EAcute]ho pivotu v st\:013apci, so zobrazen\[IAcute]m celo\[CHacek]\[IAcute]seln\[YAcute]ch riadkov\[YAcute]ch \[UAcute]prav na augmentovanej matici.

diff: \"EASY\" (4x4), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> Automatic | \"ONE\" | \"NONE\" | \"INFINITE\"   (Automatic vyber\[AAcute] typ n\[AAcute]hodne)";

GenTriangular::baddiff  = "Neplatn\[AAcute] \[UAcute]rove\[NHacek] obtia\[ZHacek]nosti `1`. Pou\[ZHacek]i\[THacek] \"EASY\"|\"MEDIUM\"|\"HARD\".";
GenTriangular::badmode  = "Neplatn\[YAcute] re\[ZHacek]im v\[YAcute]stupu `1`. Pou\[ZHacek]i\[THacek] \"TASK\"|\"TASK_RESULT\"|\"TASK_STEPS_RESULT\".";
GenTriangular::badst    = "Neplatn\[YAcute] typ rie\[SHacek]enia `1`. Pou\[ZHacek]i\[THacek] Automatic|\"ONE\"|\"NONE\"|\"INFINITE\".";
GenTriangular::badtri   = "Neplatn\[YAcute] typ trojuholn\[IAcute]kovej matice `1`. Pou\[ZHacek]i\[THacek] Automatic|\"L\"|\"U\".";
GenTriangular::fail     = "Nepodarilo sa vygenerova\[THacek] s\[UAcute]stavu s po\[ZHacek]adovan\[YAcute]mi parametrami.";

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

(* pre output vypis infinte \[DownArrow], neskor vymyslim inak *)
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


(* ~-~-~ ROW OPERATIONS - delenie, kombin\[AAcute]cia ~-~-~ *)

(* note pre delenie riadku *)
rowNoteDivide[i_, p_] := Row[{"R", i, " \[LeftArrow] R", i, " / ", tf[p]}];
rowApplyDivide[aug_, i_Integer, p_Integer] := ReplacePart[aug, i -> (aug[[i]]/p)];

(* note pre kombin\[AAcute]ciu riadkov *)
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

(* obmedzen\[EAcute] "pivotovanie" pre oby\[CHacek]ajny gauss *)
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

(* ~-~-~ MATRIX GENERATION ~-~-~ *)

$bRange = {-10, 10};
nonzeroRange[min_, max_] := DeleteCases[Range[min, max], 0];
boundsByDifficulty[diff_String] := Switch[diff, "EASY", 50, "MEDIUM", 45, "HARD", 40];
kSetTri := nonzeroRange[-4, 7];
kSetGauss := nonzeroRange[-2, 3];

lowerNonzeroCount[m_] := Count[LowerTriangularize[m, -1], x_ /; x =!= 0, {2}];


(* vytvorenie vyrie\[SHacek]enej augmentovanej matice *)
makeDiagonalAug[diff_String, n_Integer, solType_String, triType_String] := Module[
  {A, b, x, idx, paramIdx, badRow, rhsNonzero},

  rhsNonzero = DeleteCases[Range[$bRange[[1]], $bRange[[2]]], 0];

  (* \[SHacek]tart: I|b *)
  A = IdentityMatrix[n];
  b = RandomInteger[$bRange, n];
  x = b;

  (* pri va\[SHacek]om smere \:201eupper\[OpenCurlyDoubleQuote] (zdola nahor) d\[AAcute]vame \[SHacek]peci\[AAcute]lny riadok dole *)
  idx = n;
  paramIdx = Missing["NotApplicable"];
  badRow = Missing["NotApplicable"];

  Switch[solType,
    "ONE", Null
    ,
    "INFINITE",
    (* posledn\[YAcute] riadok 0 = 0 *)
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
    (* posledn\[YAcute] riadok 0 = c, c != 0 *)
    A[[idx]] = ConstantArray[0, n];
    b[[idx]] = RandomChoice[rhsNonzero];

    x = "NONE";
    badRow = idx;
  ];

  <|"Aug" -> augFromAb[A, b], "x" -> x, "BadRow" -> badRow, "ParamIdx" -> paramIdx|>
];

(* generovanie d\[AAcute]t aby postup bol bez zlomkov *)
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

  (* n\[AAcute]sobok - zvy\[SHacek]n\[EAcute] koeficienty *)
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

  maxAttempts = 40;   (* ko\:013ekokr\[AAcute]t re\[SHacek]tartova\[THacek] cel\[YAcute] scramble *)
  maxKTries = 5;     (* ko\:013eko r\[OHat]znych k sk\[UAcute]si\[THacek] pre jeden p\[AAcute]r *)

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
  addConclusion[lines_List] := (addHeader["Z\[AAcute]ver"]; Scan[addText, lines]);
  addCheckHeader[extra_List : {}] := (addHeader["Sk\[UAcute]\[SHacek]ka spr\[AAcute]vnosti"]; Scan[addText, extra]);

  addHeader["Prepis s\[UAcute]stavy do augmentovanej matice"];
  addText["S\[UAcute]stavu najprv prep\[IAcute]\[SHacek]eme do augmentovanej matice. Od tohto momentu pracujeme u\[ZHacek] len s maticou a vykon\[AAcute]vame ekvivalentn\[EAcute] riadkov\[EAcute] \[UAcute]pravy."];
  addMatrix[aug];

  result = Switch[st,
    "ONE",
    Module[{terms, solLocal},
      solLocal = ConstantArray[None, n];

      addHeader["Riadkov\[EAcute] \[UAcute]pravy"];
      addText["Riadky upravujeme tak, aby v ka\[ZHacek]dom kroku zostala v riadku iba jedna nov\[AAcute] nezn\[AAcute]ma. Najprv odstr\[AAcute]nime \[CHacek]leny s u\[ZHacek] zn\[AAcute]mymi premenn\[YAcute]mi a potom (ak je to potrebn\[EAcute]) riadok vydel\[IAcute]me pivotom, aby sme dostali jednoduch\[YAcute] tvar rovnice."];

      Do[
        terms = If[tri === "U",
          Select[Table[{j, -aug[[i, j]]}, {j, i + 1, n}], #[[2]] =!= 0 &],
          Select[Table[{j, -aug[[i, j]]}, {j, 1, i - 1}], #[[2]] =!= 0 &]
        ];

        Module[{before0, mid0, after0, notes1, notes2, hi1, hi2, hi3, p},

          before0 = aug;

          (* kombin\[AAcute]cia (ak treba) *)
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

            (* ak bola aj kombin\[AAcute]cia, zobraz 3; inak sta\[CHacek]\[IAcute] 2 stlpce *)
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

      addCheckHeader[{"V\[YAcute]po\[CHacek]et over\[IAcute]me porovnan\[IAcute]m A \[CenterDot] x s pravou stranou b (po riadkoch)."}];
      content = Join[content, verificationSteps[data, solLocal]];

      <|"Solution" -> solLocal|>
    ],

    "NONE",
    Module[{badIdx},
      badIdx = data["BadRow"];

      addHeader["Anal\[YAcute]za riadkov"];
      addText["H\:013ead\[AAcute]me riadok, v ktorom s\[UAcute] v\[SHacek]etky koeficienty pri nezn\[AAcute]mych nulov\[EAcute], ale prav\[AAcute] strana je nenulov\[AAcute]. Tak\[YAcute]to riadok predstavuje spor tvaru 0 = k, kde k \[NotEqual] 0."];

      notes = ReplacePart[ConstantArray["", n], badIdx -> "SPOR: 0 = " <> ToString[aug[[badIdx, n + 1]]]];
      addMatrix[aug, notes, <|"ActiveRow" -> badIdx|>];
      addCheckHeader[{"Pri s\[UAcute]stave bez rie\[SHacek]enia nerob\[IAcute]me klasick\[UAcute] sk\[UAcute]\[SHacek]ku dosaden\[IAcute]m. Over\[IAcute]me, \[ZHacek]e spor je naozaj nevyhnutn\[YAcute] pomocou Frobeniovej vety (porovnanie hodnost\[IAcute])."}];
      content = Join[content, verificationStepsNone[data]];
      addConclusion[{"S\[UAcute]stava preto nem\[AAcute] rie\[SHacek]enie."}];

      <|"Solution" -> "NONE"|>
    ],

    "INFINITE",
    Module[{paramIdx, solExprs, pivot, row, knownTerm},
      paramIdx = data["ParamIdx"];

      addHeader["Anal\[YAcute]za riadkov"];
      addText["Ak sa objav\[IAcute] nulov\[YAcute] riadok, znamen\[AAcute] to, \[ZHacek]e jedna z premenn\[YAcute]ch nie je ur\[CHacek]en\[AAcute] jednozna\[CHacek]ne. T\[UAcute]to premenn\[UAcute] zvol\[IAcute]me ako vo\:013en\[YAcute] parameter a ostatn\[EAcute] premenn\[EAcute] vyjadr\[IAcute]me pomocou neho."];

      notes = ReplacePart[ConstantArray["", n], paramIdx -> "nulov\[YAcute] riadok -> parameter"];
      addMatrix[aug, notes, <|"ActiveRow" -> paramIdx|>];
      addText[Row[{"Premenn\[UAcute] ", vars[[paramIdx]], " zvol\[IAcute]me za parameter ", TraditionalForm[\[FormalT]], "."}]];
      AppendTo[content, Spacer[6]];
      AppendTo[content,
        highlightGrid @ Grid[
          {{tf[vars[[paramIdx]]], "=", TraditionalForm[\[FormalT]]}},
          Alignment -> {{Right, Center, Left}},
          BaseStyle -> {FontSize -> 16}
        ]
      ];
      AppendTo[content, Spacer[6]];

      addHeader["Vyjadrenie ostatn\[YAcute]ch premenn\[YAcute]ch pomocou parametra"];

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


      addCheckHeader[{"Dosad\[IAcute]me parametrick\[EAcute] rie\[SHacek]enie do p\[OHat]vodn\[YAcute]ch rovn\[IAcute]c. Po \[UAcute]prave mus\[IAcute] v ka\[ZHacek]dom riadku vyjs\[THacek] identita (napr. 0 = 0) pre \:013eubovo\:013en\[EAcute] \[FormalT] \[Element] \[DoubleStruckCapitalZ]."}];
      content = Join[content, verificationStepsInfinite[data, solExprs]];

      addConclusion[{
        "S\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute] v tvare:",
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

  addHeader["Prepis s\[UAcute]stavy do augmentovanej matice"];
  addText["S\[UAcute]stavu prep\[IAcute]\[SHacek]eme do augmentovanej matice a n\[AAcute]sledne vykon\[AAcute]me Gaussovu elimin\[AAcute]ciu, aby sme zru\[SHacek]ili prvky pod hlavnou diagon\[AAcute]lou."];
  addMatrix[aug];

  addHeader["Dopredn\[AAcute] elimin\[AAcute]cia (na horn\[YAcute] trojuholn\[IAcute]k)"];
  addText["Postupujeme po st\:013apcoch z\:013eava doprava. V ka\[ZHacek]dom kroku zvol\[IAcute]me pivot (ak treba, prehod\[IAcute]me riadky) a n\[AAcute]sledne ru\[SHacek]\[IAcute]me prvky pod pivotom celo\[CHacek]\[IAcute]seln\[YAcute]mi riadkov\[YAcute]mi \[UAcute]pravami. Koeficienty priebe\[ZHacek]ne skracujeme pomocou gcd a riadky normalizujeme."];

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

  addHeader["Tvar po Gaussovej elimin\[AAcute]cii"];
  addText["Po doprednej elimin\[AAcute]cii dostaneme horn\[UAcute] trojuholn\[IAcute]kov\[UAcute] s\[UAcute]stavu. Teraz m\[OHat]\[ZHacek]eme ur\[CHacek]i\[THacek] nezn\[AAcute]me sp\[ADoubleDot]tn\[YAcute]m dosadzovan\[IAcute]m, za\[CHacek]\[IAcute]name od posledn\[EAcute]ho riadku."];
  addMatrix[aug, {}, <|"BoldDiagonal" -> True|>];

  If[st === "NONE",
    idx = FirstCase[Range[n], k_ /; aug[[k, k]] === 0 && aug[[k, n + 1]] =!= 0, Missing["NotFound"]];
    addText["Na diagon\[AAcute]le sa nach\[AAcute]dza nulov\[YAcute] pivot a z\[AAcute]rove\[NHacek] je v pr\[IAcute]slu\[SHacek]nom riadku nenulov\[AAcute] prav\[AAcute] strana. To vedie k rovnici tvaru 0 = k, kde k \[NotEqual] 0, tak\[ZHacek]e s\[UAcute]stava nem\[AAcute] rie\[SHacek]enie."];
    If[IntegerQ[idx],
      notes = ConstantArray["", n]; notes[[idx]] = "pivot = 0";
      addMatrix[aug, notes, <|"ActiveRow" -> idx, "BoldDiagonal" -> True|>],
      addMatrix[aug, {}, <|"BoldDiagonal" -> True|>]
    ];

    addHeader["Sk\[UAcute]\[SHacek]ka spr\[AAcute]vnosti"];
    addText["Pri s\[UAcute]stave bez rie\[SHacek]enia nerob\[IAcute]me klasick\[UAcute] sk\[UAcute]\[SHacek]ku dosaden\[IAcute]m. Over\[IAcute]me, \[ZHacek]e spor je naozaj nevyhnutn\[YAcute] pomocou Frobeniovej vety (porovnanie hodnost\[IAcute])."];
    content = Join[content, verificationStepsNone[data]];

    addHeader["Z\[AAcute]ver"];
    addText["S\[UAcute]stava nem\[AAcute] rie\[SHacek]enie."];

    Return[<|"Content" -> content, "Solution" -> "NONE"|>];
  ];

  If[st === "ONE",
    addHeader["Sp\[ADoubleDot]tn\[EAcute] dosadzovanie v rovniciach"];
    tmp = gaussBackSubstEquations[aug, vars, ConstantArray[0, n], None, content];
    solLocal = tmp[[1]];
    content = tmp[[2]];

    addHeader["Sk\[UAcute]\[SHacek]ka spr\[AAcute]vnosti"];
    addText["Over\[IAcute]me porovnan\[IAcute]m A \[CenterDot] x s pravou stranou b (po riadkoch)."];
    content = Join[content, verificationSteps[data, solLocal]];
    Return[<|"Content" -> content, "Solution" -> solLocal|>];
  ];

  If[st === "INFINITE",
    paramIdx = FirstCase[Range[n], k_ /; aug[[k, k]] === 0 && aug[[k, n + 1]] === 0, None];

    addHeader["Sp\[ADoubleDot]tn\[EAcute] dosadzovanie s parametrom"];
    solLocal = ConstantArray[0, n];
    If[IntegerQ[paramIdx], solLocal[[paramIdx]] = \[FormalT]];
    tmp = gaussBackSubstEquations[aug, vars, solLocal, paramIdx, content];
    solLocal = tmp[[1]];
    content = tmp[[2]];

    addHeader["Sk\[UAcute]\[SHacek]ka spr\[AAcute]vnosti"];
    addText["Dosad\[IAcute]me parametrick\[EAcute] rie\[SHacek]enie do p\[OHat]vodn\[YAcute]ch rovn\[IAcute]c. Po \[UAcute]prave mus\[IAcute] v ka\[ZHacek]dom riadku vyjs\[THacek] identita pre \:013eubovo\:013en\[EAcute] \[FormalT] \[Element] \[DoubleStruckCapitalZ]."];
    content = Join[content, verificationStepsInfinite[data, solLocal]];

    addHeader["Z\[AAcute]ver"];
    addText["S\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute]."];
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

  addHeader["Prepis s\[UAcute]stavy do augmentovanej matice"];
  addText["S\[UAcute]stavu prep\[IAcute]\[SHacek]eme do augmentovanej matice a vykon\[AAcute]me Gaussovu\[Dash]Jordanovu elimin\[AAcute]ciu tak, aby sa \:013eav\[AAcute] \[CHacek]as\[THacek] zmenila na jednotkov\[UAcute] maticu."];
  addMatrix[aug];

  addHeader["Dopredn\[AAcute] elimin\[AAcute]cia (nulovanie pod diagon\[AAcute]lou)"];
  addText[
    "Postupujeme po st\:013apcoch z\:013eava doprava. V ka\[ZHacek]dom st\:013apci vyberieme pivot ako nenulov\[YAcute] prvok s najmen\[SHacek]ou absol\[UAcute]tnou hodnotou a pr\[IAcute]padne prehod\[IAcute]me riadky. Pomocou pivotov\[EAcute]ho riadku potom nulujeme prvky pod n\[IAcute]m celo\[CHacek]\[IAcute]seln\[YAcute]mi \[UAcute]pravami. Po ka\[ZHacek]dom kroku riadky skracujeme pomocou najv\[ADoubleDot]\[CHacek]\[SHacek]ieho spolo\[CHacek]n\[EAcute]ho delite\:013ea (gcd) a normalizujeme."
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

  addHeader["Sp\[ADoubleDot]tn\[AAcute] elimin\[AAcute]cia (nulovanie nad diagon\[AAcute]lou)"];
  addText["Potom zru\[SHacek]\[IAcute]me prvky nad diagon\[AAcute]lou, aby sme v \:013eavej \[CHacek]asti dostali jednotkov\[UAcute] maticu."];

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
      addHeader["Anal\[YAcute]za riadkov"];
      addText["H\:013ead\[AAcute]me riadok, v ktorom s\[UAcute] v\[SHacek]etky koeficienty pri nezn\[AAcute]mych nulov\[EAcute], ale prav\[AAcute] strana je nenulov\[AAcute]. Tak\[YAcute]to riadok znamen\[AAcute] spor tvaru 0 = k, kde k \[NotEqual] 0."];
      notes = ConstantArray["", n];
      If[IntegerQ[badIdx], notes[[badIdx]] = "SPOR: 0 = " <> ToString[aug[[badIdx, n + 1]]]];
      addMatrix[aug, notes, <|"ActiveRow" -> If[IntegerQ[badIdx], badIdx, None]|>];

      addHeader["Sk\[UAcute]\[SHacek]ka spr\[AAcute]vnosti"];
      addText["Pri s\[UAcute]stave bez rie\[SHacek]enia nerob\[IAcute]me klasick\[UAcute] sk\[UAcute]\[SHacek]ku dosaden\[IAcute]m. Over\[IAcute]me pomocou Frobeniovej vety (porovnanie hodnost\[IAcute])."];
      content = Join[content, verificationStepsNone[data]];

      addHeader["Z\[AAcute]ver"];
      addText["S\[UAcute]stava nem\[AAcute] rie\[SHacek]enie."];

      Return[<|"Content" -> content, "Solution" -> "NONE"|>];
    ];
  ];

  addHeader["Normaliz\[AAcute]cia pivotov na 1"];

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

  addHeader["Hotov\[YAcute] tvar (I | x)"];
  addMatrix[aug];

  If[st === "INFINITE",
    paramIdx = data["ParamIdx"];

    addHeader["Anal\[YAcute]za riadkov"];
    addText["Ak sa objav\[IAcute] nulov\[YAcute] riadok, znamen\[AAcute] to, \[ZHacek]e jedna z premenn\[YAcute]ch nie je ur\[CHacek]en\[AAcute] jednozna\[CHacek]ne. T\[UAcute]to premenn\[UAcute] zvol\[IAcute]me ako vo\:013en\[YAcute] parameter a ostatn\[EAcute] premenn\[EAcute] vyjadr\[IAcute]me pomocou neho."];
    notes = ReplacePart[ConstantArray["", n], paramIdx -> "nulov\[YAcute] riadok -> parameter"];
    addMatrix[aug, notes, <|"ActiveRow" -> paramIdx|>];

    addText[Row[{"Premenn\[UAcute] ", vars[[paramIdx]], " zvol\[IAcute]me za parameter ", TraditionalForm[\[FormalT]], " a ponech\[AAcute]me ju v rie\[SHacek]en\[IAcute] ako symbol."}]];
    AppendTo[content, Spacer[6]];
    AppendTo[content, highlightGrid @ Grid[
      {{tf[vars[[paramIdx]]], "=", TraditionalForm[\[FormalT]]}},
      Alignment -> {{Right, Center, Left}},
      BaseStyle -> {FontSize -> 16}
    ]];
    AppendTo[content, Spacer[6]];

    addHeader["Vyjadrenie ostatn\[YAcute]ch premenn\[YAcute]ch pomocou parametra"];

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

    addHeader["Sk\[UAcute]\[SHacek]ka spr\[AAcute]vnosti"];
    addText["Dosad\[IAcute]me parametrick\[EAcute] rie\[SHacek]enie do p\[OHat]vodn\[YAcute]ch rovn\[IAcute]c. Po \[UAcute]prave mus\[IAcute] v ka\[ZHacek]dom riadku vyjs\[THacek] identita (napr. 0 = 0) pre \:013eubovo\:013en\[EAcute] \[FormalT] \[Element] \[DoubleStruckCapitalZ]."];
    content = Join[content, verificationStepsInfinite[data, solExprs]];

    addHeader["Z\[AAcute]ver"];
    addText["S\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute]."];

    Return[<|"Content" -> content, "Solution" -> "INFINITE"|>];
  ];

  solLocal = aug[[All, n + 1]];

  addHeader["Sk\[UAcute]\[SHacek]ka spr\[AAcute]vnosti"];
  addText["Over\[IAcute]me porovnan\[IAcute]m A \[CenterDot] x s pravou stranou b (po riadkoch)."];
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
          {If[lhs === b[[i]], Style["\:013dS = PS (OK)", Darker[Green]], Style["\:013dS \[NotEqual] PS (CHYBA)", Red]]}
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
        {Row[{"hodnos\[THacek](A) = ", rA}]},
        {Row[{"hodnos\[THacek]([A|b]) = ", rAug}]},
        {If[rA < rAug,
          Style["hodnos\[THacek](A) < hodnos\[THacek]([A|b])  \[Rule]  s\[UAcute]stava nem\[AAcute] rie\[SHacek]enie (OK)", Darker[Green]],
          Style["hodnosti sa nerovnaj\[UAcute] tak, ako maj\[UAcute] pre spor \[Dash] over postup (CHYBA)", Red]
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
          {Row[{"\:013dS - PS = ", TraditionalForm[diff]}]},
          {If[okQ, Style["\:013dS = PS (OK)", Darker[Green]], Style["\:013dS \[NotEqual] PS (CHYBA)", Red]]}
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
        {Row[{"hodnos\[THacek](A) = ", rA}]},
        {Row[{"hodnos\[THacek]([A|b]) = ", rAug}]},
        {If[rA === rAug && rA < n,
          Style["hodnos\[THacek](A) = hodnos\[THacek]([A|b]) < n  \[Rule]  s\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute] (OK)", Darker[Green]],
          Style["hodnosti nesp\:013a\[NHacek]aj\[UAcute] podmienku pre nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute] \[Dash] over postup (CHYBA)", Red]
        ]}
      },
      Alignment -> Left, Spacings -> {0, 0.4}, BaseStyle -> {FontSize -> 13}
    ]
  ];
  content
];


(* ~-~-~ MAIN CONTROLLER ~-~-~ *)

runMatrixGenerator[spec_Association, diff_String, mode_String, opts : OptionsPattern[]] := Module[{n, vars, st, tri, data, steps, validateExtraQ, resolveExtra, sectionTitle, stepFn, scrambleFn},
  (* spolo\[CHacek]n\[EAcute] valid\[AAcute]cie *)
  If[!TrueQ[ValidateDifficulty[diff]], Message[spec["MsgPrefix"]::baddiff, diff]; Return[]];
  If[!TrueQ[ValidateMode[mode]], Message[spec["MsgPrefix"]::badmode, mode]; Return[]];
  With[{stOpt = OptionValue[spec["EntryFn"], {opts}, SolutionType]},
    If[!TrueQ[ValidateSolutionType[stOpt]],
      Message[spec["MsgPrefix"]::badst, stOpt]; Return[]
    ];
  ];

  (* \[SHacek]pecifick\[EAcute] valid\[AAcute]cie *)
  validateExtraQ = Lookup[spec, "ValidateExtra", (True &)];
  If[!TrueQ[validateExtraQ[spec, {opts}]], Return[]];

  (* rie\[SHacek]enie typu s\[UAcute]stavy *)
  st = ResolveSolutionType[OptionValue[spec["EntryFn"], {opts}, SolutionType]];

  (* \[SHacek]pecifick\[EAcute] rie\[SHacek]enie parametrov *)
  resolveExtra = Lookup[spec, "ResolveExtra", (Missing["NotUsed"] &)];
  tri = resolveExtra[spec, {opts}];

  (* rozmery *)
  n = DimensionByDifficulty[diff];
  vars = buildVars[n];

  (* d\[AAcute]ta *)
  scrambleFn = spec["ScrambleFn"];
  data = generateData[diff, n, st, tri, scrambleFn];

  (* tla\[CHacek] zadania *)
  sectionTitle = spec["SectionTitle"];
  printSectionCell[sectionTitle];
  printSubsectionCell["Zadanie"];
  printTextCell["Rie\[SHacek]te s\[UAcute]stavu rovn\[IAcute]c v mno\[ZHacek]ine cel\[YAcute]ch \[CHacek]\[IAcute]sel."];
  printFormulaCell @ Grid[List /@ (tf /@ buildTaskEquations[data["A"], data["b"], vars]), Alignment -> Left, Spacings -> {0, 0.8}];

  printTextCell["Rie\[SHacek]te pomocou augmentovanej matice."];

  (* postup *)
  If[mode === "TASK_STEPS_RESULT",
    withStepCounter @ Function[Null,
      printSubsectionCell["Postup"];
      stepFn = Lookup[spec, "StepsFn", None];
      If[stepFn === None,
        printTextCell["Postup pre t\[UAcute]to met\[OAcute]du zatia\:013e nie je dopracovan\[YAcute] v tomto bal\[IAcute]ku."],
        steps = stepFn[data];
        Scan[renderStepItem, steps["Content"]];
      ];
    ]
  ];

  (* v\[YAcute]sledok *)
  If[mode =!= "TASK",
    If[!(mode === "TASK_STEPS_RESULT" && TrueQ @ Lookup[spec, "InlineSolutionQ", False]),
      printSubsectionCell["V\[YAcute]sledok"];

      If[st === "ONE",
        printFormulaCell[
          Row[Flatten[{"(", Riffle[vars, ", "], ") = (", Riffle[TraditionalForm /@ data["x"], ", "], ")"}]]
        ]
      ];

      If[st === "NONE", printTextCell["S\[UAcute]stava nem\[AAcute] rie\[SHacek]enie."]];

      If[st === "INFINITE",
        printTextCell["S\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute]."];
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
    "EntryFn" -> GenTriangular, "MsgPrefix" -> GenTriangular, "DimKey" -> "Triangular", "SectionTitle" -> "Trojuholn\[IAcute]kov\[AAcute] met\[OAcute]da",
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
    "EntryFn" -> GenGauss, "MsgPrefix" -> GenGauss, "DimKey" -> "Gauss", "SectionTitle" -> "Gaussova elimina\[CHacek]n\[AAcute] met\[OAcute]da",
    "ScrambleFn" -> genScrambleGauss, "StepsFn" -> stepsGauss, "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"]
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

GenGaussJordan[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenGaussJordan, "MsgPrefix" -> GenGaussJordan, "DimKey" -> "GaussJordan", "SectionTitle" -> "Gauss-Jordanova met\[OAcute]da",
    "ScrambleFn" -> genScrambleGauss, "StepsFn" -> (stepsGaussJordan[#, False] &),
    "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"]
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

GenGaussJordanPivot[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenGaussJordanPivot, "MsgPrefix" -> GenGaussJordanPivot, "DimKey" -> "GaussJordanPivot", "SectionTitle" -> "Gauss-Jordanova met\[OAcute]da s pivotovan\[IAcute]m",
    "ScrambleFn" -> genScrambleGauss, "StepsFn" -> (stepsGaussJordan[#, True] &),
    "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"]
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

End[];
EndPackage[];
