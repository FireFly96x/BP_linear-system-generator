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

GenTriangular::usage = "GenTriangular[diff, mode, opts] vygeneruje didaktick\[YAcute] pr\[IAcute]klad rie\[SHacek]enia trojuholn\[IAcute]kovej s\[UAcute]stavy line\[AAcute]rnych rovn\[IAcute]c \
pomocou augmentovanej matice a dosadzovania po riadkoch.

diff: \"EASY\" (4\[Times]4), \"MEDIUM\" (5\[Times]5), \"HARD\" (6\[Times]6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts:
  SolutionType -> Automatic | \"ONE\" | \"NONE\" | \"INFINITE\"   (Automatic vyber\[AAcute] typ n\[AAcute]hodne)
  TriangularType -> Automatic | \"L\" | \"U\"";

GenGauss::usage = "GenGauss[diff, mode, opts] vygeneruje didaktick\[YAcute] pr\[IAcute]klad rie\[SHacek]enia s\[UAcute]stavy line\[AAcute]rnych rovn\[IAcute]c pomocou Gaussovej elimina\[CHacek]nej met\[OAcute]dy \
(dopredn\[AAcute] elimin\[AAcute]cia na horn\[YAcute] trojuholn\[IAcute]k) so zobrazen\[IAcute]m celo\[CHacek]\[IAcute]seln\[YAcute]ch riadkov\[YAcute]ch \[UAcute]prav na augmentovanej matici.

diff: \"EASY\" (4\[Times]4), \"MEDIUM\" (5\[Times]5), \"HARD\" (6\[Times]6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts:
  SolutionType -> Automatic | \"ONE\" | \"NONE\" | \"INFINITE\"   (Automatic vyber\[AAcute] typ n\[AAcute]hodne)";

GenGaussJordan::usage = "GenGaussJordan[diff, mode, opts] vygeneruje didaktick\[YAcute] pr\[IAcute]klad rie\[SHacek]enia s\[UAcute]stavy line\[AAcute]rnych rovn\[IAcute]c pomocou Gaussovej\[Dash]Jordanovej met\[OAcute]dy \
(prevod na tvar (I | x)) so zobrazen\[IAcute]m celo\[CHacek]\[IAcute]seln\[YAcute]ch riadkov\[YAcute]ch \[UAcute]prav na augmentovanej matici.

diff: \"EASY\" (4\[Times]4), \"MEDIUM\" (5\[Times]5), \"HARD\" (6\[Times]6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts:
  SolutionType -> Automatic | \"ONE\" | \"NONE\" | \"INFINITE\"   (Automatic vyber\[AAcute] typ n\[AAcute]hodne)";

GenGaussJordanPivot::usage = "GenGaussJordanPivot[diff, mode, opts] vygeneruje didaktick\[YAcute] pr\[IAcute]klad rie\[SHacek]enia s\[UAcute]stavy line\[AAcute]rnych rovn\[IAcute]c pomocou Gaussovej\[Dash]Jordanovej met\[OAcute]dy \
s pivotovan\[IAcute]m (v\[YAcute]ber vhodn\[EAcute]ho pivotov\[EAcute]ho riadku) a so zobrazen\[IAcute]m celo\[CHacek]\[IAcute]seln\[YAcute]ch riadkov\[YAcute]ch \[UAcute]prav na augmentovanej matici.

diff: \"EASY\" (4\[Times]4), \"MEDIUM\" (5\[Times]5), \"HARD\" (6\[Times]6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts:
  SolutionType -> Automatic | \"ONE\" | \"NONE\" | \"INFINITE\"   (Automatic vyber\[AAcute] typ n\[AAcute]hodne)";

GenTriangular::baddiff  = "Neplatn\[AAcute] obtia\[ZHacek]nos\[THacek] `1`. Pou\[ZHacek]i \"EASY\"|\"MEDIUM\"|\"HARD\".";
GenTriangular::badmode  = "Neplatn\[YAcute] re\[ZHacek]im `1`. Pou\[ZHacek]i \"TASK\"|\"TASK_RESULT\"|\"TASK_STEPS_RESULT\".";
GenTriangular::badst    = "Neplatn\[YAcute] typ rie\[SHacek]enia `1`. Pou\[ZHacek]i Automatic|\"ONE\"|\"NONE\"|\"INFINITE\".";
GenTriangular::badtri   = "Neplatn\[YAcute] typ trojuholn\[IAcute]kovej s\[UAcute]stavy `1`. Pou\[ZHacek]i Automatic|\"L\"|\"U\".";
GenTriangular::fail     = "Nepodarilo sa vygenerova\[THacek] vhodn\[YAcute] pr\[IAcute]klad.";

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

(* predvolen\[EAcute] mo\[ZHacek]nosti bal\[IAcute]ka *)
$CommonGeneratorOptions = {SolutionType -> Automatic, TriangularType -> Automatic};

Options[GenTriangular] = $CommonGeneratorOptions;
Options[GenGauss] = $CommonGeneratorOptions;
Options[GenGaussJordan] = $CommonGeneratorOptions;
Options[GenGaussJordanPivot] = $CommonGeneratorOptions;

Begin["`Private`"];

(* ~-~-~ VALIDATION ~-~-~ *)

DimensionByDifficulty[diff_String] := Switch[diff, "EASY", 4, "MEDIUM", 5, "HARD", 6, _, 4];

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
printExprCell[expr_] := Module[{boxes}, boxes = Quiet @ Check[BoxData @ ToBoxes[expr, StandardForm], expr]; printCellStyle[boxes, "Input"]];
printSectionCell[str_String] := printCellStyle[str, "Section"];
printSubsectionCell[str_String] := printCellStyle[str, "Subsection"];
printTextExprCell[expr_] := Module[{boxes}, boxes = Quiet @ Check[BoxData @ ToBoxes[expr, StandardForm], expr]; printCellStyle[boxes, "Text"]];
printFormulaCell[expr_] := Module[{boxes}, boxes = Quiet @ Check[BoxData @ ToBoxes[expr, StandardForm], expr]; printCellStyle[boxes, "DisplayFormula"]];

highlightGrid[grid_] := Style[grid, Background -> RGBColor[0.95, 0.95, 0.95], Frame -> True, FrameStyle -> None, FrameMargins -> 5];
highlightTerm[term_] := Style[term, Bold, RGBColor[0.8, 0, 0]];
tf[val_] := TraditionalForm[val];
tft[val_] := tf[Together[val]];

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
infiniteSolutionFromSolvedAug[data_Association] := Module[{n = data["n"], augS, A, b, idx, solExprs},
  augS = data["SolvedAug"];
  A = augS[[All, 1 ;; n]];
  b = augS[[All, n + 1]];
  idx = data["ParamIdx"];

  solExprs = ConstantArray[0, n];
  solExprs[[idx]] = \[FormalT];

  Do[If[i =!= idx, solExprs[[i]] = Expand[b[[i]] - A[[i, idx]]*\[FormalT]]], {i, 1, n}];

  solExprs
];
buildTaskEquations[A_, b_, vars_] := MapThread[HoldForm[#1 == #2] &, {A . vars, b}];
augFromAb[A_, b_] := Join[A, List /@ b, 2];

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

(* koeficienty pre riadkov\[EAcute] oper\[AAcute]cie *)
genKSet[n_] := If[n <= 4, DeleteCases[Range[-5, 5], 0], DeleteCases[Range[-2, 2], 0]];
genMSet[n_] := If[n <= 4, DeleteCases[Range[-4, 4], 0], {-2, -1, 1, 2}];

$rhsNonzeroRange = Join[Range[-10, -1], Range[1, 10]];

(* n\[AAcute]hodn\[YAcute] nenulov\[YAcute] prvok z mno\[ZHacek]iny *)
genPickNonzero[set_List] := RandomChoice[set];

(* vyrob\[IAcute] vyrie\[SHacek]en\[UAcute] augmentovan\[UAcute] maticu [A|b] pod\:013ea prototypu *)
makeSolvedAug[n_Integer, solType_String, triType_String] := Module[{A, b, x, idx, paramIdx, badRow},
  A = IdentityMatrix[n];
  b = RandomInteger[$bRange, n];

  idx = RandomInteger[{1, n}];
  paramIdx = Missing["NotApplicable"];
  badRow = Missing["NotApplicable"];

  Switch[solType,
    "ONE", x = b;
    ,
    "INFINITE", A[[idx]] = ConstantArray[0, n]; b[[idx]] = 0;
      If[triType === "U",
        Do[
          A[[i, idx]] = genPickNonzero[genKSet[n]],
          {i, 1, idx - 1}
        ],
        Do[
          A[[i, idx]] = genPickNonzero[genKSet[n]],
          {i, idx + 1, n}
        ]
      ];

      x = "INFINITE";
      paramIdx = idx;
    ,
    "NONE", A[[idx]] = ConstantArray[0, n]; b[[idx]] = RandomChoice[$rhsNonzeroRange];
      x = "NONE";
      badRow = idx;
  ];

  <|"Aug" -> augFromAb[A, b], "x" -> x, "BadRow" -> badRow, "ParamIdx" -> paramIdx|>
];

(* generovanie d\[AAcute]t aby postup bol bez zlomkov *)
generateData[n_, solType_, triType_, scrambleFn_] := Module[{solved, augSolved, augTask, A, b, vars},
  solved = makeSolvedAug[n, solType, triType];
  augSolved = solved["Aug"];
  augTask = scrambleFn[augSolved, triType, solType];

  A = augTask[[All, 1 ;; n]];
  b = augTask[[All, n + 1]];
  vars = buildVars[n];

  <|"A" -> A, "b" -> b, "x" -> solved["x"], "TriType" -> triType, "SolutionType" -> solType, "Aug" -> augTask, "SolvedAug" -> augSolved, "Vars" -> vars, "n" -> n, "BadRow" -> solved["BadRow"], "ParamIdx" -> solved["ParamIdx"]|>
];

(* scrambling a \[SHacek]k\[AAcute]lovanie pivotu *)
genScrambleTri[aug0_, triType_String, solType_String : "ONE"] := Module[{aug = aug0, n, i, r, k, m},
  n = Length[aug];

  If[triType === "U",
    Do[
      Do[ k = genPickNonzero[genKSet[n]]; aug = rowAddMultiple[aug, r, i, k]
        , {r, 1, i - 1}
      ];

      m = genPickNonzero[genMSet[n]]; aug = rowScale[aug, i, m];
      , {i, 1, n}
    ],
    Do[
      Do[ k = genPickNonzero[genKSet[n]]; aug = rowAddMultiple[aug, r, i, k]
        , {r, i + 1, n}
      ];

      m = genPickNonzero[genMSet[n]]; aug = rowScale[aug, i, m];
      , {i, n, 1, -1}
    ]
  ];

  aug
];

genScrambleGauss[aug0_, triType_String, solType_String : "ONE"] := Module[{aug, n, i, r, k},
  aug = genScrambleTri[aug0, "U", solType];
  n = Length[aug];

  Do[
    Do[
      If[RandomReal[] < 0.65,
        k = genPickNonzero[genKSet[n]];
        aug = rowAddMultiple[aug, r, i, k];
        aug = normalizeAugRow[aug, r];
      ], {r, i + 1, n}
    ], {i, 1, n - 1}
  ];

  aug
];

genScrambleGJ[aug0_, triType_String, solType_String : "ONE"] := genScrambleGauss[aug0, triType, solType];

genScrambleGJP[aug0_, triType_String, solType_String : "ONE"] := genScrambleGauss[aug0, triType, solType];

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

(* R_r <- R_r + k R_i *)
rowAddMultiple[aug_, r_Integer, i_Integer, k_Integer] := ReplacePart[aug, r -> (aug[[r]] + k*aug[[i]])];

(* R_i <- m R_i *)
rowScale[aug_, i_Integer, m_Integer] := ReplacePart[aug, i -> (m*aug[[i]])];

augRender2[before_, after_, notes_, hiBefore_, hiAfter_] := Grid[
  {{alignedAugmentedMatrix[before, notes, hiBefore], Spacer[18], alignedAugmentedMatrix[after, {}, hiAfter]}},
  Alignment -> {Left, Center, Left}, Spacings -> {0, 0}
];

augRender3[before_, mid_, after_, notes1_, notes2_, hi1_, hi2_, hi3_] := Grid[
  {{alignedAugmentedMatrix[before, notes1, hi1],
    Spacer[18],
    alignedAugmentedMatrix[mid, notes2, hi2],   (* "/gcd" *)
    Spacer[18],
    alignedAugmentedMatrix[after, {}, hi3]      (* final *)
  }}, Alignment -> {Left, Center, Left}, Spacings -> {0, 0}
];

SetAttributes[rowAppendElimStep, HoldFirst];

rowAppendElimStep[content_, before_, elimRes_, r_Integer, i_Integer, n_Integer, hiBase_Association] :=
    Module[{notes, notes2, mid, after2, hi1, hi2, hi3},
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

normalizeRowByGCD[row_List] := Module[{g = rowAbsGCD[row]}, If[g > 1, row/g, row]];

normalizeAugRow[aug_, r_Integer] := ReplacePart[aug, r -> normalizeRowByGCD[aug[[r]]]];

choosePivotRow[aug_, i_Integer] := Module[{n = Length[aug], candidates, best},
  candidates = Select[Range[i, n], aug[[#, i]] =!= 0 &];
  If[candidates === {},
    i,
    best = First @ MaximalBy[candidates, {Abs[aug[[#, i]]], -Total[Abs[aug[[#]]]]} &];
    best
  ]
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

(* ~-~-~ STEP GENERATION ~-~-~ *)

stepsTriangular[data_Association] := Module[{content = {}, n, aug, vars, tri, st, order, addHeader, addText, addMatrix, addConclusion, addCheckHeader, notes, result, sol},
  n = data["n"]; aug = data["Aug"]; vars = data["Vars"]; tri = data["TriType"]; st = data["SolutionType"];
  order = If[tri === "U", Range[n, 1, -1], Range[1, n]];

  addHeader[text_] := AppendTo[content, makeStepHeader[text]];
  addText[text_] := AppendTo[content, text];
  addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] := AppendTo[content, alignedAugmentedMatrix[m, rowNotes, hi]];
  addConclusion[lines_List] := (addHeader["Z\[AAcute]ver"]; Scan[addText, lines]);
  addCheckHeader[extra_List : {}] := (addHeader["Sk\[UAcute]\[SHacek]ka spr\[AAcute]vnosti"]; Scan[addText, extra]);

  addHeader["Prepis s\[UAcute]stavy do augmentovanej matice"];
  addText["Zap\[IAcute]\[SHacek]eme s\[UAcute]stavu do augmentovanej matice a \[DHacek]alej pracujeme u\[ZHacek] len s maticou."];
  addMatrix[aug];

  result = Switch[st,
    "ONE",
    Module[{before, after, terms, p, solLocal},
      solLocal = ConstantArray[None, n];

      addHeader["Riadkov\[EAcute] \[UAcute]pravy"];
      addText["Postupne upravujeme riadky tak, aby sa dali premenn\[EAcute] dopo\[CHacek]\[IAcute]ta\[THacek] dosadzovan\[IAcute]m po riadkoch."];

      Do[
        terms = If[tri === "U",
          Select[Table[{j, -aug[[i, j]]}, {j, i + 1, n}], #[[2]] =!= 0 &],
          Select[Table[{j, -aug[[i, j]]}, {j, 1, i - 1}], #[[2]] =!= 0 &]
        ];

        If[terms =!= {},
          before = aug;
          after = rowApplyCombine[before, i, terms];
          notes = ConstantArray["", n]; notes[[i]] = rowNoteCombine[i, terms];
          AppendTo[content, augRender2[
            before, after, notes,
            <|"ActiveRow" -> i, "SourceRows" -> terms[[All, 1]], "PivotPos" -> {i, i}|>,
            <|"ActiveRow" -> i, "SourceRows" -> terms[[All, 1]], "PivotPos" -> {i, i}|>
          ]];
          aug = after;
        ];

        p = aug[[i, i]];
        If[p =!= 1,
          before = aug;
          after = rowApplyDivide[before, i, p];
          notes = ConstantArray["", n]; notes[[i]] = rowNoteDivide[i, p];
          AppendTo[content, augRender2[
            before, after, notes,
            <|"ActiveRow" -> i, "PivotPos" -> {i, i}|>,
            <|"ActiveRow" -> i, "PivotPos" -> {i, i}, "GreenCells" -> {{i, i}, {i, n + 1}}|>
          ]];
          aug = after;
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
      addText["H\:013ead\[AAcute]me riadok, ktor\[YAcute] predstavuje spor (nulov\[EAcute] koeficienty, ale nenulov\[AAcute] prav\[AAcute] strana)."];

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
      addText["Nulov\[YAcute] riadok znamen\[AAcute], \[ZHacek]e jedna premenn\[AAcute] m\[OHat]\[ZHacek]e by\[THacek] vo\:013en\[AAcute] (parameter)."];

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

      addHeader["Vyjadrenie ostatn\[YAcute]ch premenn\[YAcute]ch"];

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


      addCheckHeader[{"Dosad\[IAcute]me parametrick\[EAcute] rie\[SHacek]enie do p\[OHat]vodn\[YAcute]ch rovn\[IAcute]c. Po \[UAcute]prave mus\[IAcute] v ka\[ZHacek]dom riadku vyjs\[THacek] identita (napr. 0 = 0) pre \:013eubovo\:013en\[EAcute] \[FormalT] \[Element] \:2124."}];
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

stepsGauss[data_Association] := Module[{content = {}, n, aug, vars, st, addHeader, addText, addMatrix, notes, before, after, i, r, kPivot, elimRes, pNow, idx},
  n = data["n"]; aug = data["Aug"]; vars = data["Vars"]; st = data["SolutionType"];

  addHeader[text_] := AppendTo[content, makeStepHeader[text]];
  addText[text_] := AppendTo[content, text];
  addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] := AppendTo[content, alignedAugmentedMatrix[m, rowNotes, hi]];

  addHeader["Prepis s\[UAcute]stavy do augmentovanej matice"];
  addText["Zap\[IAcute]\[SHacek]eme s\[UAcute]stavu do augmentovanej matice a vykon\[AAcute]me dopredn\[UAcute] elimin\[AAcute]ciu (Gauss)."];
  addMatrix[aug];

  addHeader["Dopredn\[AAcute] elimin\[AAcute]cia (na horn\[YAcute] trojuholn\[IAcute]k)"];
  addText["Nulujeme prvky pod diagon\[AAcute]lou celo\[CHacek]\[IAcute]selne. Koeficienty redukujeme cez gcd a riadky priebe\[ZHacek]ne normalizujeme."];

  Do[
    kPivot = choosePivotRow[aug, i];
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
  addText["Dostali sme horn\[UAcute] trojuholn\[IAcute]kov\[UAcute] s\[UAcute]stavu."];

  If[st === "NONE",
    idx = FirstCase[Range[n], k_ /; aug[[k, k]] === 0 && aug[[k, n + 1]] =!= 0, Missing["NotFound"]];
    addText["Na diagon\[AAcute]le sa nach\[AAcute]dza nulov\[YAcute] pivot. Pri sp\[ADoubleDot]tnom dosadzovan\[IAcute] by vznikla rovnica tvaru 0 = k, kde k \[NotEqual] 0, \[CHacek]o je spor."];
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

  If[st === "INFINITE",
    idx = FirstCase[Range[n], k_ /; aug[[k, k]] === 0 && aug[[k, n + 1]] === 0, Missing["NotFound"]];
    addText["Na diagon\[AAcute]le sa nach\[AAcute]dza nulov\[YAcute] pivot. Pri sp\[ADoubleDot]tnom dosadzovan\[IAcute] by jedna premenn\[AAcute] ch\[YAcute]bala (bola by vo\:013en\[AAcute]), tak\[ZHacek]e s\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute]."];
    If[IntegerQ[idx],
      notes = ConstantArray["", n]; notes[[idx]] = "pivot = 0";
      addMatrix[aug, notes, <|"ActiveRow" -> idx, "BoldDiagonal" -> True|>],
      addMatrix[aug, {}, <|"BoldDiagonal" -> True|>]
    ];

    addHeader["Sk\[UAcute]\[SHacek]ka spr\[AAcute]vnosti"];
    addText["Over\[IAcute]me pomocou Frobeniovej vety (porovnanie hodnost\[IAcute])."];
    content = Join[content, verificationStepsInfiniteRank[data]];

    addHeader["Z\[AAcute]ver"];
    addText["S\[UAcute]stava m\[AAcute] nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute]."];

    Return[<|"Content" -> content, "Solution" -> "INFINITE"|>];
  ];

  addHeader["Sk\[UAcute]\[SHacek]ka spr\[AAcute]vnosti"];
  addText["Over\[IAcute]me porovnan\[IAcute]m A \[CenterDot] x s pravou stranou b (po riadkoch)."];
  content = Join[content, verificationSteps[data, aug[[All, n + 1]]]];

  <|"Content" -> content, "Solution" -> aug[[All, n + 1]]|>
];

stepsGaussJordan[data_Association] := Module[{content = {}, n, aug, vars, st, addHeader, addText, addMatrix, notes, before, after, kPivot, elimRes, pNow, solLocal, paramIdx, solExprs, row, pivot, knownTerm},
  n = data["n"]; aug = data["Aug"]; vars = data["Vars"]; st = data["SolutionType"];

  addHeader[text_] := AppendTo[content, makeStepHeader[text]];
  addText[text_] := AppendTo[content, text];
  addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] := AppendTo[content, alignedAugmentedMatrix[m, rowNotes, hi]];

  addHeader["Prepis s\[UAcute]stavy do augmentovanej matice"];
  addText["Zap\[IAcute]\[SHacek]eme s\[UAcute]stavu do augmentovanej matice a prevedieme ju na jednotkov\[UAcute] maticu (Gauss-Jordan)."];
  addMatrix[aug];

  addHeader["Dopredn\[AAcute] elimin\[AAcute]cia (nulovanie pod diagon\[AAcute]lou)"];
  addText["Nulujeme prvky pod diagon\[AAcute]lou celo\[CHacek]\[IAcute]selne, s redukciou cez gcd a normaliz\[AAcute]ciou riadkov."];

  Do[
    kPivot = choosePivotRow[aug, i];
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
  addText["Nulujeme prvky nad diagon\[AAcute]lou rovnak\[YAcute]m stabiln\[YAcute]m celo\[CHacek]\[IAcute]seln\[YAcute]m krokom."];

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
      addText["H\:013ead\[AAcute]me riadok, ktor\[YAcute] predstavuje spor (nulov\[EAcute] koeficienty, ale nenulov\[AAcute] prav\[AAcute] strana)."];
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

  addHeader["Normaliz\[AAcute]cia pivotov"];

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
    addText["Nulov\[YAcute] riadok znamen\[AAcute], \[ZHacek]e jedna premenn\[AAcute] m\[OHat]\[ZHacek]e by\[THacek] vo\:013en\[AAcute] (parameter)."];
    notes = ReplacePart[ConstantArray["", n], paramIdx -> "nulov\[YAcute] riadok -> parameter"];
    addMatrix[aug, notes, <|"ActiveRow" -> paramIdx|>];

    addText[Row[{"Premenn\[UAcute] ", vars[[paramIdx]], " zvol\[IAcute]me za parameter ", TraditionalForm[\[FormalT]], "."}]];
    AppendTo[content, Spacer[6]];
    AppendTo[content, highlightGrid @ Grid[
      {{tf[vars[[paramIdx]]], "=", TraditionalForm[\[FormalT]]}},
      Alignment -> {{Right, Center, Left}},
      BaseStyle -> {FontSize -> 16}
    ]];
    AppendTo[content, Spacer[6]];

    addHeader["Vyjadrenie ostatn\[YAcute]ch premenn\[YAcute]ch"];

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
    addText["Dosad\[IAcute]me parametrick\[EAcute] rie\[SHacek]enie do p\[OHat]vodn\[YAcute]ch rovn\[IAcute]c. Po \[UAcute]prave mus\[IAcute] v ka\[ZHacek]dom riadku vyjs\[THacek] identita (napr. 0 = 0) pre \:013eubovo\:013en\[EAcute] \[FormalT] \[Element] \:2124."];
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

stepsGaussJordanPivot[data_Association] := Module[{content = {}, n, aug, vars, st, addHeader, addText, addMatrix, notes, before, after, kPivot, elimRes, pNow, solLocal, paramIdx, solExprs, row, pivot, knownTerm},
  n = data["n"]; aug = data["Aug"]; vars = data["Vars"]; st = data["SolutionType"];

  addHeader[text_] := AppendTo[content, makeStepHeader[text]];
  addText[text_] := AppendTo[content, text];
  addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] := AppendTo[content, alignedAugmentedMatrix[m, rowNotes, hi]];

  addHeader["Prepis s\[UAcute]stavy do augmentovanej matice"];
  addText["Zap\[IAcute]\[SHacek]eme s\[UAcute]stavu do augmentovanej matice a prevedieme ju na jednotkov\[UAcute] maticu (Gauss-Jordan)."];
  addMatrix[aug];

  addHeader["Dopredn\[AAcute] elimin\[AAcute]cia (nulovanie pod diagon\[AAcute]lou)"];
  addText["Nulujeme prvky pod diagon\[AAcute]lou celo\[CHacek]\[IAcute]selne, s redukciou cez gcd a normaliz\[AAcute]ciou riadkov."];

  Do[
    kPivot = choosePivotRow[aug, i];
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
  addText["Nulujeme prvky nad diagon\[AAcute]lou rovnak\[YAcute]m stabiln\[YAcute]m celo\[CHacek]\[IAcute]seln\[YAcute]m krokom."];

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
      addText["H\:013ead\[AAcute]me riadok, ktor\[YAcute] predstavuje spor (nulov\[EAcute] koeficienty, ale nenulov\[AAcute] prav\[AAcute] strana)."];

      notes = ConstantArray["", n];
      If[IntegerQ[badIdx], notes[[badIdx]] = "SPOR: 0 = " <> ToString[aug[[badIdx, n + 1]]]];
      addMatrix[aug, notes, <|"ActiveRow" -> If[IntegerQ[badIdx], badIdx, None]|>];

      addHeader["Sk\[UAcute]\[SHacek]ka spr\[AAcute]vnosti"];
      addText["Pri s\[UAcute]stave bez rie\[SHacek]enia nerob\[IAcute]me klasick\[UAcute] sk\[UAcute]\[SHacek]ku dosaden\[IAcute]m. Over\[IAcute]me, \[ZHacek]e spor je naozaj nevyhnutn\[YAcute] pomocou Frobeniovej vety (porovnanie hodnost\[IAcute])."];
      content = Join[content, verificationStepsNone[data]];

      addHeader["Z\[AAcute]ver"];
      addText["S\[UAcute]stava preto nem\[AAcute] rie\[SHacek]enie."];

      Return[<|"Content" -> content, "Solution" -> "NONE"|>];
    ];
  ];

  addHeader["Normaliz\[AAcute]cia pivotov"];

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
      addMatrix[
        aug, ConstantArray["", n],
        <|"ActiveRow" -> i, "PivotPos" -> {i, i}, "GreenCells" -> {{i, i}, {i, n + 1}}|>
      ]
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
    addText["Nulov\[YAcute] riadok znamen\[AAcute], \[ZHacek]e jedna premenn\[AAcute] m\[OHat]\[ZHacek]e by\[THacek] vo\:013en\[AAcute] (parameter)."];

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

    addHeader["Vyjadrenie ostatn\[YAcute]ch premenn\[YAcute]ch"];

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
      AppendTo[content,
        highlightGrid @ Grid[
          {{tf[vars[[i]]], "=", TraditionalForm[solExprs[[i]]] }},
          Alignment -> {{Right, Center, Left}},
          BaseStyle -> {FontSize -> 16}
        ]
      ];
      AppendTo[content, Spacer[6]];
      , {i, n, 1, -1}
    ];

    addHeader["Sk\[UAcute]\[SHacek]ka spr\[AAcute]vnosti"];
    addText["Dosad\[IAcute]me parametrick\[EAcute] rie\[SHacek]enie do p\[OHat]vodn\[YAcute]ch rovn\[IAcute]c. Po \[UAcute]prave mus\[IAcute] v ka\[ZHacek]dom riadku vyjs\[THacek] identita (napr. 0 = 0) pre \:013eubovo\:013en\[EAcute] \[FormalT] \[Element] \:2124."];
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

(* ~-~-~ SK\[CapitalUAcute]\[CapitalSHacek]KA SPR\[CapitalAAcute]VNOSTI ~-~-~ *)

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
  If[!TrueQ[validateExtraQ[spec, opts]], Return[]];

  (* rie\[SHacek]enie typu s\[UAcute]stavy *)
  st = ResolveSolutionType[OptionValue[spec["EntryFn"], {opts}, SolutionType]];

  (* \[SHacek]pecifick\[EAcute] rie\[SHacek]enie parametrov *)
  resolveExtra = Lookup[spec, "ResolveExtra", (Missing["NotUsed"] &)];
  tri = resolveExtra[spec, opts];

  (* rozmery *)
  n = DimensionByDifficulty[diff];
  vars = buildVars[n];

  (* d\[AAcute]ta *)
  scrambleFn = spec["ScrambleFn"];
  data = generateData[n, st, tri, scrambleFn];

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
    "ScrambleFn" -> genScrambleTri, "StepsFn" -> stepsTriangular,
    "ValidateExtra" -> Function[{specLocal, passedOpts},
      With[{triOpt = OptionValue[specLocal["EntryFn"], {passedOpts}, TriangularType]},
        If[!TrueQ[validateTriangularType[triOpt]],
          Message[specLocal["MsgPrefix"]::badtri, triOpt];
          False,
          True
        ]
      ]
    ],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, resolveTriangularType[OptionValue[specLocal["EntryFn"], {passedOpts}, TriangularType]]]
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
    "ScrambleFn" -> genScrambleGJ, "StepsFn" -> stepsGaussJordan,
    "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"]
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

GenGaussJordanPivot[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenGaussJordanPivot, "MsgPrefix" -> GenGaussJordanPivot, "DimKey" -> "GaussJordanPivot", "SectionTitle" -> "Gauss-Jordanova met\[OAcute]da s pivotovan\[IAcute]m",
    "ScrambleFn" -> genScrambleGJP, "StepsFn" -> stepsGaussJordanPivot,
    "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"]
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

End[];
EndPackage[];
