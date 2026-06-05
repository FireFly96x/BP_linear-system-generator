(* ::Package:: *)

BeginPackage["`MatrixGenerator`"];

$CharacterEncoding = "UTF-8";
Internal`$ContextMarks = False;

GenTriangular::usage = "GenTriangular[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc v trojuholníkovej sústave pomocou augmentovanej matice \
diff: \"EASY\" (3x3), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> \"ONE\" | \"NONE\" | \"INFINITE\"";

GenGauss::usage = "GenGauss[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc pomocou Gaussovej metódy \
diff: \"EASY\" (3x3), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> \"ONE\" | \"NONE\" | \"INFINITE\"";

GenGaussJordan::usage = "GenGaussJordan[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc pomocou Gaussovej-Jordanovej metódy \
(prevod na tvar (I | x)) so zobrazením celočíselných riadkových úprav na augmentovanej matici.
diff: \"EASY\" (3x3), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> \"ONE\" | \"NONE\" | \"INFINITE\"";

GenGaussJordanPivot::usage = "GenGaussJordanPivot[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc pomocou Gaussovej-Jordanovej metódy \
s pivotovaním výberom najmenšieho možného pivotu v st\:013apci, so zobrazením celočíselných riadkových úprav na augmentovanej matici.
diff: \"EASY\" (3x3), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> \"ONE\" | \"NONE\" | \"INFINITE\"";

GenElemGJ::usage = "GenElemGJ[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc pomocou Gaussovej-Jordanovej metódy \
s explicitným zápisom elementárnych matíc E_i, takže po každom kroku platí M_i = E_i M_(i-1).
diff: \"EASY\" (3x3), \"MEDIUM\" (4x4), \"HARD\" (5x5)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> \"ONE\"";

GenInverse::usage = "GenInverse[diff, mode, opts] vygeneruje didaktický príklad výpočtu inverznej matice pomocou Gauss-Jordanovej metódy v tvare (A|E) -> (E|A^-1).
diff: \"EASY\" (3x3), \"MEDIUM\" (4x4), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> \"ONE\"";

GenLU::usage = "GenLU[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc pomocou LU rozkladu (Doolittle).
diff: \"EASY\" (3x3), \"MEDIUM\" (4x4), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> \"ONE\"";

GenCholesky::usage = "GenCholesky[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc pomocou Choleského rozkladu.
diff: \"EASY\" (3x3), \"MEDIUM\" (4x4), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> \"ONE\"";

GenCramer::usage = "GenCramer[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc pomocou Cramerovho pravidla.
diff: \"EASY\" (3x3), \"MEDIUM\" (4x4), \"HARD\" (5x5)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> \"ONE\"";

GenElimination::usage = "GenElimination[diff, mode, opts] vygeneruje príklad riešenia sústavy lineárnych rovníc eliminačnou metódou (sčítaním rovníc).
diff: \"EASY\" (2x2), \"MEDIUM\" (3x3), \"HARD\" (3x3)
mode: \"TASK\", \"TASK_RESULT\", \"TASK_STEPS_RESULT\"
opts: Visualization -> True|False, SolutionType -> Automatic|\"ONE\"|\"NONE\"|\"INFINITE\"";

GenSubstitution::usage = "GenSubstitution[diff, mode, opts] vygeneruje príklad riešenia sústavy lineárnych rovníc dosadzovacou (substitučnou) metódou.
diff: \"EASY\" (2x2), \"MEDIUM\" (3x3), \"HARD\" (3x3)
mode: \"TASK\", \"TASK_RESULT\", \"TASK_STEPS_RESULT\"
opts: Visualization -> True|False, SolutionType -> Automatic|\"ONE\"|\"NONE\"|\"INFINITE\"";

GenTriangular::baddiff  = "Neplatná úroveň obtiažnosti `1`. Použiť \"EASY\"|\"MEDIUM\"|\"HARD\".";
GenTriangular::badmode  = "Neplatný režim výstupu `1`. Použiť \"TASK\"|\"TASK_RESULT\"|\"TASK_STEPS_RESULT\".";
GenTriangular::badst    = "Neplatný typ riešenia `1`. Použiť Automatic|\"ONE\"|\"NONE\"|\"INFINITE\".";
GenTriangular::badtf    = "Neplatný formát zadania `1`. Použiť \"EQUATIONS\"|\"MATRIX\".";
GenTriangular::fail     = "Nepodarilo sa vygenerovať sústavu s požadovanými parametrami.";

GenGauss::baddiff = GenTriangular::baddiff;
GenGauss::badmode = GenTriangular::badmode;
GenGauss::badst   = GenTriangular::badst;
GenGauss::badtf = GenTriangular::badtf;
GenGauss::fail    = GenTriangular::fail;
GenGaussJordan::baddiff = GenTriangular::baddiff;
GenGaussJordan::badmode = GenTriangular::badmode;
GenGaussJordan::badst   = GenTriangular::badst;
GenGaussJordan::badtf = GenTriangular::badtf;
GenGaussJordan::fail    = GenTriangular::fail;
GenGaussJordanPivot::baddiff = GenTriangular::baddiff;
GenGaussJordanPivot::badmode = GenTriangular::badmode;
GenGaussJordanPivot::badst   = GenTriangular::badst;
GenGaussJordanPivot::badtf = GenTriangular::badtf;
GenGaussJordanPivot::fail    = GenTriangular::fail;
GenElemGJ::baddiff = GenTriangular::baddiff;
GenElemGJ::badmode = GenTriangular::badmode;
GenElemGJ::badst   = "Pre túto metódu je povolené len SolutionType -> \"ONE\".";
GenElemGJ::badtf = GenTriangular::badtf;
GenElemGJ::fail    = GenTriangular::fail;
GenInverse::baddiff = GenTriangular::baddiff;
GenInverse::badmode = GenTriangular::badmode;
GenInverse::badst = "Pre túto metódu je povolené len SolutionType -> \"ONE\".";
GenInverse::badtf = GenTriangular::badtf;
GenInverse::fail = "Nepodarilo sa vygenerovať regulárnu maticu pre výpočet inverznej matice.";
GenLU::baddiff = GenTriangular::baddiff;
GenLU::badmode = GenTriangular::badmode;
GenLU::badst   = "Pre túto metódu je povolené len SolutionType -> \"ONE\".";
GenLU::badtf = GenTriangular::badtf;
GenLU::fail    = "Nepodarilo sa vygenerovať sústavu vhodnú pre LU rozklad bez pivotovania.";
GenCholesky::baddiff = GenTriangular::baddiff;
GenCholesky::badmode = GenTriangular::badmode;
GenCholesky::badst   = "Pre túto metódu je povolené len SolutionType -> \"ONE\".";
GenCholesky::badtf = GenTriangular::badtf;
GenCholesky::fail    = "Nepodarilo sa vygenerovať symetrickú kladne definitnú sústavu vhodnú pre Choleského rozklad.";
GenCramer::baddiff = GenTriangular::baddiff;
GenCramer::badmode = GenTriangular::badmode;
GenCramer::badst   = "Pre túto metódu je povolené len SolutionType -> \"ONE\".";
GenCramer::badtf = GenTriangular::badtf;
GenCramer::fail    = "Nepodarilo sa vygenerovať regulárnu sústavu vhodnú pre Cramerovo pravidlo.";

GenElimination::baddiff = GenTriangular::baddiff;
GenElimination::badmode = GenTriangular::badmode;
GenElimination::badst   = GenTriangular::badst;
GenElimination::fail    = GenTriangular::fail;
GenSubstitution::baddiff = GenTriangular::baddiff;
GenSubstitution::badmode = GenTriangular::badmode;
GenSubstitution::badst   = GenTriangular::badst;
GenSubstitution::fail    = GenTriangular::fail;

$CommonGeneratorOptions = {SolutionType -> "ONE", TaskFormat -> "EQUATIONS"};

Options[GenTriangular] = $CommonGeneratorOptions;
Options[GenGauss] = $CommonGeneratorOptions;
Options[GenGaussJordan] = $CommonGeneratorOptions;
Options[GenGaussJordanPivot] = $CommonGeneratorOptions;
Options[GenElemGJ] = $CommonGeneratorOptions;
Options[GenInverse] = $CommonGeneratorOptions;
Options[GenLU] = $CommonGeneratorOptions;
Options[GenCholesky] = $CommonGeneratorOptions;
Options[GenCramer] = $CommonGeneratorOptions;

Options[GenElimination] = {SolutionType -> "ONE", Visualization -> False};
Options[GenSubstitution] = {SolutionType -> "ONE", Visualization -> False};

$ElemStepCounter = 0;
$ElemMatrixCounter = 0;

Begin["`Private`"];

(* ~-~-~ VALIDATION ~-~-~ *)

DimensionByMethodDifficulty[dimKey_String, diff_String] := Switch[
  dimKey,

  "Inverse" | "LU" | "Cholesky",
  Switch[diff, "EASY", 3, "MEDIUM", 4, "HARD", 6],

  "ElemGaussJordan" |  "Cramer", (*Cramer sa nemôže meniť - ma fixne kroky ku rozmerom*)
  Switch[diff, "EASY", 3, "MEDIUM", 4, "HARD", 5],

  "Elimination" | "Substitution",
  Switch[diff, "EASY", 2, "MEDIUM", 3, "HARD", 3],
  _,
  Switch[diff, "EASY", 3, "MEDIUM", 5, "HARD", 6]
];

ValidateDifficulty[diff_] := MemberQ[{"EASY", "MEDIUM", "HARD"}, diff];
ValidateMode[mode_] := MemberQ[{"TASK", "TASK_RESULT", "TASK_STEPS_RESULT"}, mode];
ValidateSolutionType[st_] := TrueQ[st === Automatic] || MemberQ[{"ONE", "NONE", "INFINITE"}, st];
ResolveSolutionType[st_] := If[st =!= Automatic, st, RandomChoice[{0.8, 0.1, 0.1} -> {"ONE", "NONE", "INFINITE"}]];
ValidateTaskFormat[taskFormat_] := MemberQ[{"EQUATIONS", "MATRIX"}, taskFormat];

(* validácia metód, ktoré povoľujú len jedno riešenie *)
validateOnlyOneSolutionType[specLocal_, passedOpts_] := With[
  {
    stOpt = OptionValue[specLocal["EntryFn"], passedOpts, SolutionType],
    msg = specLocal["MsgPrefix"]
  },
  If[stOpt =!= "ONE",
    Message[msg::badst, stOpt];
    False,
    True
  ]
];

(* ~-~-~ CELL PRINTING ~-~-~ *)

printCellStyle[expr_, style_String] := If[
  Head @ Quiet[EvaluationNotebook[]] === NotebookObject,
  CellPrint @ Cell[expr, style, ShowStringCharacters -> False],
  Print[expr]
];
printTextCell[str_String] := printCellStyle[str, "Text"];
printSectionCell[str_String] := printCellStyle[str, "Section"];
printSubsectionCell[str_String] := printCellStyle[str, "Subsection"];
printFormulaCell[expr_] := Module[{ boxes}, boxes = Quiet @ Check[BoxData @ ToBoxes[expr, StandardForm], expr]; printCellStyle[boxes, "DisplayFormula"]];

highlightGrid[grid_] := Style[grid, Background -> RGBColor[0.95, 0.95, 0.95], Frame -> True, FrameStyle -> None, FrameMargins -> 5];
tf[val_] := TraditionalForm[val];
tft[val_] := tf[Together[val]];

(* symbol ekvivalentnej riadkovej úpravy *)
rowEquivalentSymbol[] := Style["\[TildeTilde]", Bold, FontSize -> 18];

(* základné zvýraznenie ľavej strany rovnosti *)
lhsStyle[expr_] := Style[expr, Bold];
inverseASymbol[] := Superscript[Style["A", Italic], -1];
transposeLSymbol[] := Superscript[Style["L", Italic], Style["T", Italic]];

resultEquationLine[lhs_, rhs_] := Row[{lhsStyle[lhs], " = ", Style[tft[rhs], Bold, Blue]}];
plainEquationLine[lhs_, rhs_] := Row[{lhsStyle[lhs], " = ", tft[rhs]}];

highlightResultEquation[lhs_, rhs_] := highlightGrid @ Grid[
  {{
    tf[lhsStyle[lhs]],
    "=",
    TraditionalForm[rhs]
  }},
  Alignment -> {{Right, Center, Left}},
  BaseStyle -> {FontSize -> 16}
];

SetAttributes[addGap, HoldFirst];
addGap[content_, h_: 5] := AppendTo[content, Cell["", "Text", CellMargins -> {{Inherited, Inherited}, {0, 0}}, CellSize -> {Automatic, h}]];

SetAttributes[appendStepHeader, HoldFirst];
appendStepHeader[content_, text_, gap_: 2] := (
  If[Length[content] > 0, addGap[content, gap]]; AppendTo[content, makeStepHeader[text]]
);

(* ~-~-~ STEP RENDERING ~-~-~ *)

withStepCounter[renderFn_] := Block[{stepsCounter = 0}, renderFn[]];
makeStepHeader[text_] := (stepsCounter++;Style[Row[{stepsCounter, ". ", text}], Bold]);

(* vykreslenie jedného prvku postupu podľa jeho typu *)
renderStepItem[item_] := Which[
  StringQ[item], printTextCell[item],
  Head[item] === Spacer, printCellStyle["", "Text"],
  MatchQ[item, Style[_, ___]], CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Text", ShowStringCharacters -> False],
  Head[item] === Cell, CellPrint[item],
  Head[item] === Graphics || Head[item] === Graphics3D, CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Graphics"],
  True, printFormulaCell[item]
];

buildMatrixVars[n_] := Take[{a, b, c, d, e, f}, n];
buildEquationVars[n_Integer] := Take[{x, y, z}, n];

(* zostavenie parametrického riešenia z upravenej rozšírenej matice *)
infiniteSolutionFromSolvedAug[data_Association] := Module[{ n = data["n"], augS, A, b, idxs, params, solExprs, pivot, knownTerm},

  augS = data["SolvedAug"];
  A = augS[[All, 1 ;; n]];
  b = augS[[All, n + 1]];

  idxs = Lookup[data, "ParamIdxs", {data["ParamIdx"]}];
  params = If[Length[idxs] === 1, {\[FormalT]}, {\[FormalS], \[FormalT]}];

  solExprs = ConstantArray[0, n];

  Do[
    solExprs[[idxs[[k]]]] = params[[k]];
    ,
    {k, 1, Length[idxs]}
  ];

  Do[
    If[MemberQ[idxs, i], Continue[]];

    pivot = A[[i, i]];
    If[pivot === 0, Continue[]];

    knownTerm = Total@Table[A[[i, idxs[[k]]]]*params[[k]], {k, 1, Length[idxs]}];
    solExprs[[i]] = Expand[(b[[i]] - knownTerm)/pivot];
    ,
    {i, 1, n}
  ];

  solExprs
];

augFromAb[A_, b_] := Join[A, List /@ b, 2];

alignedTaskEquations[A_, b_, vars_] := Grid[
  Table[
    {
      tf[A[[i]] . vars],
      Style["=", 16],
      tf[b[[i]]]
    },
    {i, 1, Length[b]}
  ],
  Alignment -> {{Right, Center, Left}},
  Spacings -> {0.5, 0.8},
  BaseStyle -> {FontSize -> 14}
];

(* ~-~-~ ROW OPERATIONS - delenie, kombinácia ~-~-~ *)

(* note pre delenie riadku *)
rowNoteDivide[i_, p_] := Row[{"R", i, " \[LeftArrow] R", i, " / ", tf[p]}];
rowApplyDivide[aug_, i_Integer, p_Integer] := ReplacePart[aug, i -> (aug[[i]]/p)];

augRender2[before_, after_, notes_, hiBefore_, hiAfter_] := Grid[
  {{
    alignedAugmentedMatrix[before, notes, hiBefore],
    rowEquivalentSymbol[],
    alignedAugmentedMatrix[after, {}, hiAfter]
  }},
  Alignment -> {Left, Center, Left},
  Spacings -> {1.1, 0}
];

augRender3[before_, mid_, after_, notes1_, notes2_, hi1_, hi2_, hi3_] := Grid[
  {{
    alignedAugmentedMatrix[before, notes1, hi1],
    rowEquivalentSymbol[],
    alignedAugmentedMatrix[mid, notes2, hi2],
    rowEquivalentSymbol[],
    alignedAugmentedMatrix[after, {}, hi3]
  }},
  Alignment -> {Left, Center, Left, Center, Left},
  Spacings -> {1.1, 0}
];

(* pomenovaný stav matice *)
namedAugmentedStateCard[label_, aug_, notes_List : {}, hi_Association : <||>] := Grid[
  {{
    Style[Row[{label, " ="}], Bold, FontSize -> 16],
    alignedAugmentedMatrix[aug, notes, hi]
  }},
  Alignment -> Left,
  Spacings -> {2, 1}
];

SetAttributes[appendElemTransition, HoldFirst];

(* vykreslenie jedného kroku cez elementárnu maticu *)
appendElemTransition[content_, before_, after_, note_, eMat_, targetRow_Integer, n_Integer, eIndex_Integer,
  mIndex_Integer, boldPos_: Automatic, hiBefore_Association : <||>, hiAfter_Association : <||>, eOrangePos_: {}
] := Module[
  {notes, eLabel, prevLabel, nextLabel, eHi, eBoldPositions, eActiveCol,
    eOrangeCells, afterWithTooltips},

  notes = ConstantArray["", n];
  notes[[targetRow]] = note;

  eLabel = Subscript[Style["E", Italic], eIndex];
  prevLabel = Subscript[Style["M", Italic], mIndex - 1];
  nextLabel = Subscript[Style["M", Italic], mIndex];

  eBoldPositions = Which[
    MatchQ[boldPos, {_Integer, _Integer}], {boldPos},
    ListQ[boldPos] && AllTrue[boldPos, MatchQ[#, {_Integer, _Integer}] &], boldPos,
    True, {}
  ];

  eOrangeCells = Which[
    MatchQ[eOrangePos, {_Integer, _Integer}], {eOrangePos},
    ListQ[eOrangePos] && AllTrue[eOrangePos, MatchQ[#, {_Integer, _Integer}] &], eOrangePos,
    True, {}
  ];

  eActiveCol = If[MatchQ[boldPos, {_Integer, _Integer}], boldPos[[2]], targetRow];

  eHi = <|
    "ActiveRow" -> targetRow,
    "ActiveCol" -> eActiveCol,
    "BoldPositions" -> eBoldPositions,
    "OrangeCells" -> eOrangeCells
  |>;

  afterWithTooltips = dotProductTooltipMatrix[eMat, before];

  addGap[content, 1];

  AppendTo[
    content,
    Grid[
      {{
        labeledMatrixBlock[eLabel, styledPlainMatrix[eMat, eHi]],
        Style["\[CenterDot]", Bold, FontSize -> 18],
        labeledMatrixBlock[prevLabel, alignedAugmentedMatrix[before, notes, hiBefore]],
        Style["=", Bold, FontSize -> 18],
        labeledMatrixBlock[nextLabel, alignedAugmentedMatrix[afterWithTooltips, {}, hiAfter]]
      }},
      Alignment -> {Left, Center, Left, Center, Left},
      Spacings -> {1.2, 0.8}
    ]
  ];
];

SetAttributes[applyElemMultiplyStep, HoldFirst];

(* krok násobenia riadku aj s elementárnou maticou *)
applyElemMultiplyStep[content_, aug_, rowIdx_Integer, factor_, n_Integer, pivotPos_: None] := Module[{ before, after, eMat, hi, note},
  before = aug;
  after = ReplacePart[before, rowIdx -> factor before[[rowIdx]]];
  eMat = elemMatrixScale[n, rowIdx, factor];

  note = Row[{"R", rowIdx, " \[LeftArrow] ", tf[factor], "\[CenterDot]R", rowIdx}];

  hi = If[ListQ[pivotPos],
    <|"ActiveRow" -> rowIdx, "PivotPos" -> pivotPos|>,
    <|"ActiveRow" -> rowIdx|>
  ];

  $ElemStepCounter++;
  $ElemMatrixCounter++;

  appendElemTransition[
    content, before, after, note, eMat,
    rowIdx, n, $ElemStepCounter, $ElemMatrixCounter, {rowIdx, rowIdx}, hi, hi
  ];

  after
];

SetAttributes[applyElemCombineStep, HoldFirst];

(* krok pripočítania násobkov iných riadkov k cieľovému riadku *)
applyElemCombineStep[content_, aug_, rowIdx_Integer, terms_List, n_Integer, pivotPos_: None] := Module[
  {before, after, eMat, hiBefore, hiAfter, note, termDisplay, zeroCell},

  before = aug;
  after = ReplacePart[
    before,
    rowIdx -> before[[rowIdx]] + Total[terms[[All, 2]] before[[terms[[All, 1]]]]]
  ];

  eMat = IdentityMatrix[n];

  Scan[
    Function[term, eMat[[rowIdx, term[[1]]]] = Together[eMat[[rowIdx, term[[1]]]] + term[[2]]]],
    terms
  ];

  termDisplay[row_, coeff_] := Row[{
    If[coeff < 0, " - ", " + "],
    If[Abs[coeff] === 1, Row[{"R", row}], Row[{tf[Abs[coeff]], "\[CenterDot]", "R", row}]]
  }];

  note = Row @ Prepend[termDisplay @@@ terms, Row[{"R", rowIdx, " \[LeftArrow] R", rowIdx}]];

  zeroCell = If[ListQ[pivotPos], {{rowIdx, pivotPos[[2]]}}, {}];

  (* pred úpravou zvýrazníme menený riadok, zdrojový riadok a nulovaný prvok *)
  hiBefore = Join[
    <|"ActiveRow" -> rowIdx, "SourceRows" -> terms[[All, 1]]|>,
    If[ListQ[pivotPos], <|"PivotPos" -> pivotPos, "OrangeCells" -> zeroCell|>, <||>]
  ];

  (* po úprave zvýrazníme výslednú nulu červenou *)
  hiAfter = Join[
    <|"ActiveRow" -> rowIdx|>,
    If[ListQ[pivotPos], <|"PivotPos" -> pivotPos, "ZeroCells" -> zeroCell|>, <||>]
  ];

  $ElemStepCounter++;
  $ElemMatrixCounter++;

  appendElemTransition[
    content, before, after, note, eMat,
    rowIdx, n, $ElemStepCounter, $ElemMatrixCounter,
    {rowIdx, terms[[1, 1]]},
    hiBefore,
    hiAfter,
    {{rowIdx, terms[[1, 1]]}}
  ];

  after
];

SetAttributes[applyElemDivideStep, HoldFirst];

applyElemDivideStep[content_, aug_, rowIdx_Integer, divisor_, n_Integer, pivotPos_: None] := Module[{ before, after, eMat, hi},
  before = aug;
  after = rowApplyDivide[before, rowIdx, divisor];
  eMat = elemMatrixScale[n, rowIdx, 1/divisor];

  hi = If[ListQ[pivotPos],
    <|"ActiveRow" -> rowIdx, "PivotPos" -> pivotPos|>,
    <|"ActiveRow" -> rowIdx|>
  ];

  $ElemStepCounter++;
  $ElemMatrixCounter++;

  appendElemTransition[
    content, before, after, rowNoteDivide[rowIdx, divisor], eMat,
    rowIdx, n, $ElemStepCounter, $ElemMatrixCounter, {rowIdx, rowIdx}, hi, hi
  ];

  after
];

SetAttributes[applyJordanSwapStep, HoldFirst];

applyJordanSwapStep[content_, aug_, i_Integer, k_Integer, n_Integer, showElemQ_?BooleanQ] := Module[{ before, after, notes, eMat, hi1, hi2},
  before = aug;
  after = rowApplySwap[before, i, k];

  hi1 = <|"ActiveRow" -> i, "SourceRows" -> {k}, "PivotPos" -> {i, i}|>;
  hi2 = <|"ActiveRow" -> k, "SourceRows" -> {i}, "PivotPos" -> {i, i}|>;

  If[!showElemQ,
    notes = ConstantArray["", n];
    notes[[i]] = rowNoteSwap[i, k];
    AppendTo[content, augRender2[before, after, notes, hi1, hi2]];
    Return[after];
  ];

  (*swap*)
  eMat = IdentityMatrix[n];
  eMat[[{i, k}]] = eMat[[{k, i}]];
  $ElemStepCounter++;
  $ElemMatrixCounter++;

  appendElemTransition[
    content, before, after, rowNoteSwap[i, k], eMat,
    i, n, $ElemStepCounter, $ElemMatrixCounter, Automatic, hi1, hi2
  ];

  after
];

SetAttributes[applyJordanElimStep, HoldFirst];

(* jeden eliminačný krok pre Gauss-Jordanovu úpravu *)
applyJordanElimStep[content_, aug_, r_Integer, i_Integer, n_Integer, hiBase_Association, showElemQ_?BooleanQ] := Module[
  {workAug, before, elimRes, p, a, directCoeff, g, p2, a2, g2},

  If[!showElemQ,
    before = aug;
    elimRes = rowApplyElimStable[before, r, i];
    Return[rowAppendElimStep[content, before, elimRes, r, i, n, hiBase]]
  ];

  workAug = aug;
  a = workAug[[r, i]];
  If[a === 0, Return[workAug]];

  p = workAug[[i, i]];
  If[p === 0, Return[workAug]];

  (* ak sa dá nulovať jednou celočíselnou kombináciou, nerobíme zbytočné násobenie riadku *)
  directCoeff = Together[-a/p];

  If[IntegerQ[directCoeff],
    Return[applyElemCombineStep[content, workAug, r, {{i, directCoeff}}, n, {i, i}]]
  ];

  (* inak použijeme stabilnú celočíselnú úpravu bez zlomkov *)
  g = GCD[p, a];
  p2 = p/g;
  a2 = a/g;

  If[p2 =!= 1, workAug = applyElemMultiplyStep[content, workAug, r, p2, n, {i, i}]];
  If[a2 =!= 0, workAug = applyElemCombineStep[content, workAug, r, {{i, -a2}}, n, {i, i}]];

  g2 = rowAbsGCD[workAug[[r]]];
  If[g2 =!= 1, workAug = applyElemDivideStep[content, workAug, r, g2, n, {i, i}]];

  workAug
];

SetAttributes[rowAppendElimStep, HoldFirst];

(* pridanie eliminačného kroku vrátane prípadného skrátenia riadku *)
rowAppendElimStep[content_, before_, elimRes_, r_Integer, i_Integer, n_Integer, hiBase_Association] := Module[{ notes, notes2, mid, after2, hi1, hi2, hi3},
  notes = ConstantArray["", n];
  notes[[r]] = rowNoteElim[r, i, elimRes["p2"], elimRes["a2"]];

  hi1 = Join[hiBase, <|
    "ActiveRow" -> r,
    "PivotPos" -> {i, i},
    "OrangeCells" -> {{r, i}}
  |>];
  hi2 = Join[hiBase, <|"ActiveRow" -> r, "PivotPos" -> {i, i}, "ZeroCells" -> {{r, i}}|>];
  hi3 = hi2;

  If[elimRes["DivG"] =!= 1,
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

SetAttributes[rowAppendElimStepInverse, HoldFirst];

rowAppendElimStepInverse[content_, before_, elimRes_, r_Integer, i_Integer, n_Integer, hiBase_Association] := Module[{ notes, notes2, mid, after2, hi1, hi2, hi3},

  notes = ConstantArray["", n];
  notes[[r]] = rowNoteElim[r, i, elimRes["p2"], elimRes["a2"]];

  hi1 = Join[hiBase, <|
    "ActiveRow" -> r,
    "PivotPos" -> {i, i},
    "OrangeCells" -> {{r, i}}
  |>];
  hi2 = Join[hiBase, <|"ActiveRow" -> r, "PivotPos" -> {i, i}, "ZeroCells" -> {{r, i}}|>];
  hi3 = hi2;

  If[elimRes["DivG"] =!= 1,
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

(* ~-~-~ MATRIX ROW HELPERS ~-~-~ *)

rowAbsGCD[row_List] := Module[{ g = Apply[GCD, Abs[row]]}, If[g === 0, 1, g]];

normalizeRow[row_List] := Module[{ g = rowAbsGCD[row], first},
  first = FirstCase[Most[row], x_ /; x =!= 0, 1];
  If[g > 1, row/g, If[first === -1, -row, row]]
];

rowNoteSwap[i_, k_] := Row[{"R", i, " \[LeftRightArrow] R", k}];

rowApplySwap[aug_, i_Integer, k_Integer] := ReplacePart[aug, {i -> aug[[k]], k -> aug[[i]]}];

rowNoteElim[r_, i_, p2_, a2_] := Module[{ leftPart, rightPart, op},
  leftPart = If[p2 === 1, Row[{"R", r}], Row[{tf[p2], "\[CenterDot]", "R", r}]];
  rightPart = If[Abs[a2] === 1, Row[{"R", i}], Row[{tf[Abs[a2]], "\[CenterDot]", "R", i}]];
  op = If[a2 < 0, " + ", " - "];

  Row[{"R", r, " \[LeftArrow] ", leftPart, op, rightPart}]
];

(* celočíselné vynulovanie prvku pomocou pivotného riadku bez zbytočných zlomkov *)
rowApplyElimStable[aug_, r_Integer, i_Integer] := Module[{ p, a, g1, p2, a2, rowRaw, g2, div, rowFinal, augRaw, augFinal},

  p = aug[[i, i]];
  a = aug[[r, i]];

  If[a === 0,
    <|"Aug" -> aug, "AugRaw" -> aug, "p2" -> 0, "a2" -> 0, "DivG" -> 1|>,
    g1 = GCD[p, a];
    p2 = p/g1;
    a2 = a/g1;

    rowRaw = p2*aug[[r]] - a2*aug[[i]];
    g2 = rowAbsGCD[rowRaw];

    div = If[g2 > 1, g2, If[rowRaw[[r]] === -1, -1, 1]];
    rowFinal = If[div =!= 1, rowRaw/div, rowRaw];

    augRaw = ReplacePart[aug, r -> rowRaw];
    augFinal = ReplacePart[aug, r -> rowFinal];

    <|"Aug" -> augFinal, "AugRaw" -> augRaw, "p2" -> p2, "a2" -> a2, "DivG" -> div|>
  ]
];

elemMatrixScale[n_Integer, i_Integer, factor_] := Module[{ e = IdentityMatrix[n]},
  e[[i, i]] = factor;
  e
];

(* pre INFINITE a NONE *)
contradictionRowQ[row_List] := Module[{ lhs = Most[row], rhs = Last[row]}, (AllTrue[lhs, # === 0 &] && rhs =!= 0)];
findContradictionRow[aug_] := Module[{ idx = FirstCase[Range[Length[aug]], i_ /; contradictionRowQ[aug[[i]]], Missing["NotFound"]]}, idx];

(* ~-~-~ MATRIX VISUALIZATION ~-~-~ *)

(* výsledok násobenia matíc s tooltipmi pre skalárne súčiny *)
dotProductTooltipMatrix[left_, right_] := Module[{ makeTermDisplay, makeTooltipCell},

  makeTermDisplay[a_, b_] := Row[{luFactorDisplay[a], "\[CenterDot]", luFactorDisplay[b]}];

  makeTooltipCell[i_, j_] := Module[{ terms, value, tooltipExpr},
    terms = Transpose[{left[[i]], right[[All, j]]}];
    value = Together[left[[i]] . right[[All, j]]];

    tooltipExpr = Row[{
      Row @ Riffle[(makeTermDisplay[#[[1]], #[[2]]] & /@ terms), " + "],
      " = ",
      tft[value]
    }];

    MouseAppearance[
      Tooltip[
        TraditionalForm[value],
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
  ];

  Table[
    makeTooltipCell[i, j],
    {i, 1, Length[left]},
    {j, 1, Length[right[[1]]]}
  ]
];

(* zvýraznené zobrazenie obyčajnej matice s riadkom a stĺpcom *)
styledPlainMatrix[m_, hi_Association : <||>] := Module[
  {nRows, nCols, activeRows, sourceRows, activeCols, sourceCols, boldPositions,
    zeroCells, orangeCells, cellBg, makeCell, leftBracketCell, rightBracketCell, rows},

  {nRows, nCols} = Dimensions[m];

  activeRows = DeleteCases[Flatten @ {Lookup[hi, "ActiveRows", {}], Lookup[hi, "ActiveRow", None]}, None];
  sourceRows = Flatten @ {Lookup[hi, "SourceRows", {}]};
  activeCols = DeleteCases[Flatten @ {Lookup[hi, "ActiveCols", {}], Lookup[hi, "ActiveCol", None]}, None];
  sourceCols = Flatten @ {Lookup[hi, "SourceCols", {}]};

  boldPositions = Lookup[hi, "BoldPositions", {}];
  If[MatchQ[boldPositions, {_Integer, _Integer}], boldPositions = {boldPositions}];

  zeroCells = Lookup[hi, "ZeroCells", {}];
  orangeCells = Lookup[hi, "OrangeCells", {}];

  cellBg[i_, j_] := Module[{aRowQ, sRowQ, aColQ, sColQ},
    aRowQ = MemberQ[activeRows, i]; sRowQ = MemberQ[sourceRows, i];
    aColQ = MemberQ[activeCols, j]; sColQ = MemberQ[sourceCols, j];

    Which[
      aRowQ && aColQ, RGBColor[0.86, 0.93, 1.00],
      sRowQ && aColQ, RGBColor[0.92, 0.90, 1.00],
      aRowQ && sColQ, RGBColor[0.90, 0.96, 0.94],
      sRowQ && sColQ, RGBColor[0.95, 0.92, 0.98],
      aRowQ, RGBColor[0.90, 0.95, 1.00],
      sRowQ, RGBColor[0.95, 0.92, 1.00],
      aColQ, RGBColor[1.00, 0.97, 0.88],
      sColQ, RGBColor[0.98, 0.95, 0.90],
      True, None
    ]
  ];

  makeCell[i_, j_, val_] := Module[{cell},
    cell = If[
      MemberQ[{Tooltip, MouseAppearance, Style, Row, Grid, Pane, Framed, TraditionalForm}, Head[val]],
      val,
      TraditionalForm[val]
    ];

    Which[
      MemberQ[zeroCells, {i, j}], cell = Style[cell, Red, Bold],
      MemberQ[orangeCells, {i, j}], cell = Style[cell, Orange, Bold],
      MemberQ[boldPositions, {i, j}] || (MemberQ[activeRows, i] && MemberQ[activeCols, j]), cell = Style[cell, Bold]
    ];

    Item[
      Pane[cell, ImageSize -> {Automatic, 18}, Alignment -> {Right, Center}],
      Background -> cellBg[i, j]
    ]
  ];

  leftBracketCell = Item["", Frame -> {{True, False}, {True, True}}];
  rightBracketCell = Item["", Frame -> {{False, True}, {True, True}}];

  rows = Table[
    Join[
      {If[i === 1, leftBracketCell, SpanFromAbove]},
      Table[makeCell[i, j, m[[i, j]]], {j, 1, nCols}],
      {If[i === 1, rightBracketCell, SpanFromAbove]}
    ],
    {i, 1, nRows}
  ];

  Grid[
    rows,
    Alignment -> Join[{Center}, ConstantArray[Right, nCols], {Center}],
    Spacings -> {1, 1},
    BaseStyle -> {FontSize -> 14}
  ]
];

(* blok s popisom nad maticou *)
labeledMatrixBlock[label_, body_] := Column[
  {
    Style[label, Bold, FontSize -> 15],
    body
  },
  Alignment -> Center,
  Spacings -> {0.4}
];

(* renderovanie rozšírenej matice (A|b) so zvýraznením *)
alignedAugmentedMatrix[aug_, notes_List : {}, hi_Association : <||>] := Module[{ nRows, nCols, nA, notes2, pivotPos, activeRows, sourceRows, activeCols, sourceCols, ZeroCells, orangeCells, bar,
  leftLabel, rightLabel, showLabelsQ, cellBg, makeCell, makeBar, leftBracketCell, rightBracketCell, rows, matrixGrid, notesGrid, notesWithLabels, colSizes, labelGrid, matrixWithLabels},

  {nRows, nCols} = Dimensions[aug];
  nA = nCols - 1;

  notes2 = If[notes === {}, ConstantArray["", nRows], PadRight[notes, nRows, ""]];
  pivotPos = Lookup[hi, "PivotPos", None];
  leftLabel = Lookup[hi, "LeftLabel", None];
  rightLabel = Lookup[hi, "RightLabel", None];
  showLabelsQ = leftLabel =!= None || rightLabel =!= None;

  activeRows = DeleteCases[Flatten @ {Lookup[hi, "ActiveRows", {}], Lookup[hi, "ActiveRow", None]}, None];
  sourceRows = Flatten @ {Lookup[hi, "SourceRows", {}]};

  activeCols = DeleteCases[
    Flatten @ {
      Lookup[hi, "ActiveCols", {}],
      Lookup[hi, "ActiveCol", None],
      If[ListQ[pivotPos], pivotPos[[2]], Nothing]
    },
    None
  ];
  sourceCols = Flatten @ {Lookup[hi, "SourceCols", {}]};

  ZeroCells = Lookup[hi, "ZeroCells", {}];
  orangeCells = Lookup[hi, "OrangeCells", {}];

  bar = Style["|", GrayLevel[.35], FontSize -> 16];

  cellBg[i_, j_] := Module[{ aRowQ, sRowQ, aColQ, sColQ},
    aRowQ = MemberQ[activeRows, i];
    sRowQ = MemberQ[sourceRows, i];
    aColQ = MemberQ[activeCols, j];
    sColQ = MemberQ[sourceCols, j];

    Which[
      aRowQ && aColQ, RGBColor[0.86, 0.93, 1.00],
      sRowQ && aColQ, RGBColor[0.92, 0.90, 1.00],
      aRowQ && sColQ, RGBColor[0.90, 0.96, 0.94],
      sRowQ && sColQ, RGBColor[0.95, 0.92, 0.98],
      aRowQ, RGBColor[0.90, 0.95, 1.00],
      sRowQ, RGBColor[0.95, 0.92, 1.00],
      aColQ, RGBColor[1.00, 0.97, 0.88],
      sColQ, RGBColor[0.98, 0.95, 0.90],
      True, None
    ]
  ];

  makeCell[i_, j_, val_] := Module[{cell, isGreen, isOrange, isDiag, isPivot},
    cell = If[
      MemberQ[{Tooltip, MouseAppearance, Style, Row, Grid, Pane, Framed, TraditionalForm}, Head[val]],
      val,
      TraditionalForm[val]
    ];

    isGreen = MemberQ[ZeroCells, {i, j}];
    isOrange = MemberQ[orangeCells, {i, j}];
    isDiag = j <= nA && i === j;
    isPivot = ListQ[pivotPos] && pivotPos === {i, j};

    Which[
      isGreen, cell = Style[cell, Red, Bold],
      isOrange, cell = Style[cell, Orange, Bold],
      isPivot || isDiag, cell = Style[cell, Bold]
    ];

    Item[
      Pane[cell, ImageSize -> {Automatic, 18}, Alignment -> {Right, Center}],
      Background -> cellBg[i, j]
    ]
  ];

  makeBar[i_] := Item[
    bar,
    Background -> Which[
      MemberQ[activeRows, i], RGBColor[0.90, 0.95, 1.00],
      MemberQ[sourceRows, i], RGBColor[0.95, 0.92, 1.00],
      True, None
    ]
  ];

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

  colSizes = Join[{0.2}, ConstantArray[1.2, nA], {0.2, 1.2, 0.2}];

  matrixGrid = Grid[
    rows,
    Alignment -> Join[{Center}, ConstantArray[Right, nA], {Center, Right, Center}],
    Spacings -> {1, 1},
    BaseStyle -> {FontSize -> 14},
    ItemSize -> {colSizes, Automatic}
  ];

  labelGrid = Grid[
    {
      Join[
        {""},
        {Item[If[leftLabel === None, "", Style[leftLabel, Bold, FontSize -> 15]], Alignment -> Center]},
        ConstantArray[SpanFromLeft, nA - 1],
        {""},
        {Item[If[rightLabel === None, "", Style[rightLabel, Bold, FontSize -> 15]], Alignment -> Center]},
        {""}
      ]
    },
    Alignment -> Center,
    Spacings -> {1, 0},
    BaseStyle -> {FontSize -> 14},
    ItemSize -> {colSizes, Automatic}
  ];

  matrixWithLabels = If[
    showLabelsQ,
    Column[{labelGrid, matrixGrid}, Alignment -> Center, Spacings -> {0.15}],
    matrixGrid
  ];

  notesGrid = Grid[
    List /@ (
      Item[
        Pane[Style[#, GrayLevel[.35], FontSize -> 13], {150, Automatic}, Alignment -> Left],
        Background -> White
      ] & /@ notes2
    ),
    Alignment -> Left,
    Spacings -> {0, 1.15},
    BaseStyle -> {FontSize -> 14}
  ];

  notesWithLabels = If[
    showLabelsQ,
    Column[
      {
        Style["\[InvisibleSpace]", Bold, FontSize -> 15],
        notesGrid
      },
      Alignment -> Left,
      Spacings -> {0.15}
    ],
    notesGrid
  ];

  Grid[
    {{matrixWithLabels, Spacer[12], notesWithLabels}},
    Alignment -> {Left, Center, Left},
    Spacings -> {0, 0}
  ]
];

(* renderovanie pre tvar (A|E)                                            *)
alignedAugmentedMatrixInverse[aug_, notes_List : {}, hi_Association : <||>] := Module[
  {nRows, nCols, nA, notes2, pivotPos, activeRow, sourceRows, zeroCells, orangeCells,
    bar, rowColor, sourceColor, wrapBg, makeCell, makeBar, leftBracketCell,
    rightBracketCell, rows, matrixGrid, notesGrid, notesWithLabels, showPivotQ,
    leftLabel, rightLabel, showLabelsQ, colSizes, labelGrid, matrixWithLabels},

  {nRows, nCols} = Dimensions[aug];
  nA = Quotient[nCols, 2];

  notes2 = If[notes === {}, ConstantArray["", nRows], PadRight[notes, nRows, ""]];
  pivotPos = Lookup[hi, "PivotPos", None];
  activeRow = Lookup[hi, "ActiveRow", None];
  sourceRows = Lookup[hi, "SourceRows", {}];
  zeroCells = Lookup[hi, "ZeroCells", {}];
  orangeCells = Lookup[hi, "OrangeCells", {}];

  leftLabel = Lookup[hi, "LeftLabel", None];
  rightLabel = Lookup[hi, "RightLabel", None];
  showLabelsQ = leftLabel =!= None || rightLabel =!= None;

  bar = Style["|", GrayLevel[.35], FontSize -> 16];
  rowColor = RGBColor[0.90, 0.95, 1];
  sourceColor = RGBColor[0.95, 0.92, 1.00];

  wrapBg[i_, expr_] := Module[{bg = None},
    If[IntegerQ[activeRow] && i === activeRow, bg = rowColor];
    If[MemberQ[sourceRows, i], bg = sourceColor];
    Item[expr, Background -> bg]
  ];

  showPivotQ = ListQ[pivotPos] &&
      ((IntegerQ[activeRow] && activeRow === pivotPos[[1]]) || MemberQ[sourceRows, pivotPos[[1]]]);

  makeCell[i_, j_, val_] := Module[{cell, isDiagLeft, isDiagRight, isDiag},
    cell = TraditionalForm[val];

    isDiagLeft = j <= nA && i === j;
    isDiagRight = j > nA && i === j - nA;
    isDiag = isDiagLeft || isDiagRight;

    Which[
      MemberQ[zeroCells, {i, j}], cell = Style[cell, Red, Bold],
      MemberQ[orangeCells, {i, j}], cell = Style[cell, Orange, Bold],
      showPivotQ && pivotPos === {i, j}, cell = Style[cell, Bold],
      isDiag, cell = Style[cell, Bold]
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

  colSizes = Join[{0.2}, ConstantArray[1.2, nA], {0.2}, ConstantArray[1.2, nA], {0.2}];

  matrixGrid = Grid[
    rows,
    Alignment -> Join[{Center}, ConstantArray[Right, nA], {Center}, ConstantArray[Right, nA], {Center}],
    Spacings -> {1, 1},
    BaseStyle -> {FontSize -> 14},
    ItemSize -> {colSizes, Automatic}
  ];

  labelGrid = Grid[
    {Join[
      {""},
      {Item[If[leftLabel === None, "", Style[leftLabel, Bold, FontSize -> 15]], Alignment -> Center]},
      ConstantArray[SpanFromLeft, nA - 1],
      {""},
      {Item[If[rightLabel === None, "", Style[rightLabel, Bold, FontSize -> 15]], Alignment -> Center]},
      ConstantArray[SpanFromLeft, nA - 1],
      {""}
    ]},
    Alignment -> Center,
    Spacings -> {1, 0},
    BaseStyle -> {FontSize -> 14},
    ItemSize -> {colSizes, Automatic}
  ];

  matrixWithLabels = If[
    showLabelsQ,
    Column[{labelGrid, matrixGrid}, Alignment -> Center, Spacings -> {0.15}],
    matrixGrid
  ];

  notesGrid = Grid[
    List /@ (
      Item[
        Pane[
          If[StringQ[#], Style[#, GrayLevel[.35], FontSize -> 13], #],
          {Automatic, Automatic},
          Alignment -> Left
        ],
        Background -> White
      ] & /@ notes2
    ),
    Alignment -> Left,
    Spacings -> {0, 1.15},
    BaseStyle -> {FontSize -> 14}
  ];

  notesWithLabels = If[
    showLabelsQ,
    Column[{Style["\[InvisibleSpace]", Bold, FontSize -> 15], notesGrid}, Alignment -> Left, Spacings -> {0.15}],
    notesGrid
  ];

  Grid[
    {{matrixWithLabels, Spacer[12], notesWithLabels}},
    Alignment -> {Left, Center, Left},
    Spacings -> {0, 0}
  ]
];

augRender2Inverse[before_, after_, notes_, hiBefore_, hiAfter_] := Grid[
  {{
    alignedAugmentedMatrixInverse[before, notes, hiBefore],
    rowEquivalentSymbol[],
    alignedAugmentedMatrixInverse[after, {}, hiAfter]
  }},
  Alignment -> {Left, Center, Left},
  Spacings -> {1.1, 0}
];

augRender3Inverse[before_, mid_, after_, notes1_, notes2_, hi1_, hi2_, hi3_] := Grid[
  {{
    alignedAugmentedMatrixInverse[before, notes1, hi1],
    rowEquivalentSymbol[],
    alignedAugmentedMatrixInverse[mid, notes2, hi2],
    rowEquivalentSymbol[],
    alignedAugmentedMatrixInverse[after, {}, hi3]
  }},
  Alignment -> {Left, Center, Left, Center, Left},
  Spacings -> {1.1, 0}
];

(* ~-~-~ BOUNDS & SOLVER HELPERS ~-~-~ *)

$bRange = {-10, 10};
nonzeroRange[min_, max_] := DeleteCases[Range[min, max], 0];

$MaxBounds = 20; (* zmeniť podľa preferencie *)
$Bounds = 4 + Quotient[$MaxBounds, 1.4 + 0.156 Sqrt[$MaxBounds]];
$MaxRetryCount = 1000;

kSet := nonzeroRange[-9, 9];
strongKSet[] := DeleteCases[kSet, -1 | 1];

matrixMaxAbs[m_] := Max[Abs[Flatten[m]]];

SetAttributes[appendNoneConclusionAndStop, HoldFirst];

(* ukončenie výpočtu pri spornom riadku 0 = k *)
appendNoneConclusionAndStop[content_, aug_, data_Association, showElemQ_: False, mIndex_: None] := Module[{ n, badIdx, notes},

  n = data["n"];
  badIdx = findContradictionRow[aug];
  If[badIdx === Missing["NotFound"], Return[Null]];

  appendStepHeader[content, "Analýza riadkov"];
  AppendTo[content, "Po doprednej eliminácii sme dostali riadok tvaru 0 = k, kde k \[NotEqual] 0. To je spor, preto sústava nemá riešenie a ďalej už nemusíme pokračovať."];

  notes = ConstantArray["", n];
  notes[[badIdx]] = "SPOR: 0 = " <> ToString[aug[[badIdx, n + 1]]];

  If[TrueQ[showElemQ] && IntegerQ[mIndex],
    AppendTo[content, namedAugmentedStateCard[
      Subscript[Style["M", Italic], mIndex],
      aug,
      notes,
      <|"ActiveRow" -> badIdx|>
    ]],
    AppendTo[content, alignedAugmentedMatrix[aug, notes, <|"ActiveRow" -> badIdx|>]]
  ];

  appendStepHeader[content, "Skúška správnosti"];
  AppendTo[content, "Skontrolujeme to pomocou Frobeniovej vety porovnaním hodností - hodnosť chápeme ako počet nenulových riadkov po úpravách"];
  content = Join[content, verificationStepsNone[data]];
  Throw[<|"Content" -> content, "Solution" -> "NONE"|>, "StopMatrixSteps"]
];

(* opakované generovanie, kým medzikroky neprekročia zadané hranice *)
generateDataWithBounds[diff_String, n_Integer, solType_, triType_, scrambleFn_, pivotMode_: "ZERO", boundAugFn_: Automatic, boundCheckFn_: Automatic] := Module[{ data, retries = 0, augForCheck, resolvedBoundAugFn, resolvedBoundCheckFn},

  resolvedBoundAugFn = If[
    boundAugFn === Automatic,
    Function[d, d["Aug"]],
    boundAugFn
  ];

  resolvedBoundCheckFn = If[
    boundCheckFn === Automatic,
    gaussForwardEliminationWithinBoundsQ,
    boundCheckFn
  ];

  While[retries < $MaxRetryCount,
    data = generateData[diff, n, solType, triType, scrambleFn];

    If[data === $Failed,
      retries++;
      Continue[]
    ];

    augForCheck = resolvedBoundAugFn[data];

    If[augForCheck === $Failed,
      retries++;
      Continue[]
    ];

    If[TrueQ[resolvedBoundCheckFn[augForCheck, pivotMode]],
      Return[data]
    ];

    retries++;
  ];

  $Failed
];

(* ~-~-~ MATRIX GENERATION ~-~-~ *)

kSetTri := nonzeroRange[-4, 7];
kSetGauss := nonzeroRange[-2, 3];

(* východisková diagonálna rozšírená matica podľa typu riešenia *)
makeDiagonalAug[n_Integer, solType_String] := Module[{ A, b, x, idx, paramIdx, paramIdxs = {}, badRow, rhsNonzero, numParams, pivotRows, coeffPool, buildParamColumn, col1, col2, tries},

  rhsNonzero = DeleteCases[Range[$bRange[[1]], $bRange[[2]]], 0];

  (* štart: I|b *)
  A = IdentityMatrix[n];
  b = RandomInteger[$bRange, n];
  x = b;

  idx = n;
  paramIdx = Missing["NotApplicable"];
  badRow = Missing["NotApplicable"];

  (* vytvorenie stĺpca pre parameter *)
  buildParamColumn[rowCount_Integer] := Module[
    {col, minNonzero, requiredPos, requiredVals, optionalPos},

    col = ConstantArray[0, rowCount];
    minNonzero = Min[2, rowCount];

    (* aspoň 2 nenulové výskyty parametra, ak sa dá *)
    requiredPos = RandomSample[Range[rowCount], minNonzero];

    (* rôzne koeficienty a nikdy nie ±1 *)
    requiredVals = RandomSample[coeffPool, minNonzero];
    Do[
      col[[requiredPos[[k]]]] = requiredVals[[k]],
      {k, 1, minNonzero}
    ];

    (* voliteľne doplníme ďalšie výskyty *)
    optionalPos = Complement[Range[rowCount], requiredPos];
    Scan[
      Function[pos,
        If[RandomChoice[{0.65, 0.35} -> {True, False}],
          col[[pos]] = RandomChoice[coeffPool]
        ]
      ],
      optionalPos
    ];

    col
  ];

  Switch[solType,
    "ONE",
    Null
    ,

    "INFINITE",
    numParams = If[n === 3, 1, 2];
    paramIdxs = If[numParams === 1, {n}, {n - 1, n}];
    pivotRows = Range[n - numParams];
    coeffPool = DeleteCases[kSetTri, 1 | -1];

    (* nulové riadky pre voľné premenné *)
    Do[
      A[[r]] = ConstantArray[0, n];
      b[[r]] = 0;
      ,
      {r, paramIdxs}
    ];

    (* prvý parameter *)
    col1 = buildParamColumn[Length[pivotRows]];
    Do[
      A[[pivotRows[[r]], paramIdxs[[1]]]] = col1[[r]];
      ,
      {r, 1, Length[pivotRows]}
    ];

    (* druhý parameter len pre n > 3 *)
    If[numParams === 2,
      tries = 0;
      col2 = buildParamColumn[Length[pivotRows]];

      While[
        tries < 60 &&
            (
              col2 === col1 ||
                  col2 === -col1 ||
                  MatrixRank[{col1, col2}] < 2
            ),
        col2 = buildParamColumn[Length[pivotRows]];
        tries++;
      ];

      (* poistka, aby sa stĺpce nikdy nepodobali *)
      If[
        col2 === col1 ||
            col2 === -col1 ||
            MatrixRank[{col1, col2}] < 2,
        col2 = RotateLeft[col1];
        col2[[1]] = RandomChoice[DeleteCases[coeffPool, col1[[1]] | -col1[[1]]]];
      ];

      Do[
        A[[pivotRows[[r]], paramIdxs[[2]]]] = col2[[r]];
        ,
        {r, 1, Length[pivotRows]}
      ];
    ];

    x = "INFINITE";
    paramIdx = Last[paramIdxs];
    ,

    "NONE",
    coeffPool = DeleteCases[kSetTri, 0];

    (* posledný riadok ostáva spor 0 = c, c != 0 *)
    A[[idx]] = ConstantArray[0, n];
    b[[idx]] = RandomChoice[rhsNonzero];

    (* horné pivotové riadky nech obsahujú aj ďalšie koeficienty *)
    Do[
      Do[
        A[[i, j]] = RandomChoice[Join[{0, 0}, coeffPool]],
        {j, i + 1, n}
      ],
      {i, 1, n - 1}
    ];

    (* posledná premenná aby nebola 0 vsade*)
    Do[
      If[A[[i, n]] === 0,
        A[[i, n]] = RandomChoice[DeleteCases[coeffPool, -1 | 1]]
      ],
      {i, Max[1, n - 2], n - 1}
    ];

    x = "NONE";
    badRow = idx;
  ];

  <|
    "Aug" -> augFromAb[A, b],
    "x" -> x,
    "BadRow" -> badRow,
    "ParamIdx" -> paramIdx,
    "ParamIdxs" -> paramIdxs
  |>
];

(* -- Scramble Helpers -- *)

(* plánované stĺpce, v ktorých má nastať výmena pivotu *)
gaussPlannedPivotSwapColumns[pivotCount_Integer] := Module[{ possibleCols},
  possibleCols = Range[Max[0, pivotCount - 1]];

  Which[
    possibleCols === {}, {},
    Length[possibleCols] === 1, {1},
    Length[possibleCols] === 2, {1, 2},
    True, {2, Last[possibleCols]}
  ]
];

(* zistenie, v ktorých stĺpcoch trace skutočne zmenil pivot na menší *)
gaussObservedPivotSwapColumns[trace_List] := Module[{ cols = {}, step, prev, i, k, currentPivot, newPivot},

  Do[
    step = trace[[t]];
    If[Lookup[step, "Type", None] =!= "Swap" || t == 1, Continue[]];

    prev = trace[[t - 1, "Matrix"]];
    i = step["PivotCol"];
    k = step["SourceRow"];

    currentPivot = prev[[i, i]];
    newPivot = prev[[k, i]];

    If[
      currentPivot =!= 0 &&
          newPivot =!= 0 &&
          Abs[newPivot] < Abs[currentPivot],
      AppendTo[cols, i]
    ];
    ,
    {t, 1, Length[trace]}
  ];

  cols
];

(* vynútenie výmeny susedného pivotu pri zachovaní hraníc *)
gaussForceAdjacentPivotSwap[aug_, i_Integer, bnd_Integer] := Module[{ work = aug, rowI, rowK, factors, chosen},

  If[i >= Length[aug], Return[work]];

  rowI = work[[i]];
  rowK = work[[i + 1]];

  If[rowI[[i]] === 0 || rowK[[i]] === 0, Return[work]];

  factors = {2, 3, -2, -3};

  chosen = SelectFirst[
    factors,
    Module[{ cand = # rowK},
      Max[Abs[cand]] <= bnd &&
          cand[[i]] =!= 0 &&
          Abs[cand[[i]]] > Abs[rowI[[i]]]
    ] &,
    Missing["NotFound"]
  ];

  If[chosen =!= Missing["NotFound"],
    work[[i + 1]] = chosen work[[i + 1]];
  ];

  rowApplySwap[work, i, i + 1]
];

(* trace doprednej Gaussovej eliminácie pre kontrolu medzikrokov *)
gaussForwardEliminationTrace[aug_, pivotMode_: "ZERO"] := Module[{ workAug, n, i, r, pivotRowFn, pivotRow, pivotValue, elimRes, trace = {}},

  workAug = aug;
  n = Length[workAug];

  pivotRowFn = Switch[pivotMode,
    "MIN", gaussPivotRowByMinAbs,
    _, gaussPivotRowByNonzero
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


(* kontrola matice počas doprednej eliminácie *)
gaussForwardEliminationWithinBoundsQ[aug_, pivotMode_: "ZERO"] := Module[{ traceData, limit},

  limit = $MaxBounds;
  traceData = gaussForwardEliminationTrace[aug, pivotMode];

  AllTrue[
    traceData["Trace"],
    matrixMaxAbs[#["Matrix"]] <= limit &
  ]
];

(* kontrola matice počas G-J eliminácie (aj inverse) *)
gaussJordanEliminationWithinBoundsQ[aug_, pivotMode_: "MIN"] := Module[{ traceData, limit},

  limit = $MaxBounds;
  traceData = gaussJordanEliminationTrace[aug, pivotMode];

  AllTrue[
    traceData["Trace"],
    matrixMaxAbs[#["Matrix"]] <= limit &
  ]
];

(* kontrola A,b,L,U,y,x *)
luDecompositionWithinBoundsQ[data_Association] := Module[{ luData, limit},

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

(* kontrola A,b,L,L^T,y,x *)
choleskyDecompositionWithinBoundsQ[data_Association] := Module[{ choleskyData, limit},

  limit = $MaxBounds;
  choleskyData = choleskySolveData[data["A"], data["b"]];

  If[choleskyData === $Failed, Return[False]];

  AllTrue[
    {data["A"], data["b"], choleskyData["L"], Transpose[choleskyData["L"]], choleskyData["Y"], choleskyData["X"]},
    matrixMaxAbs[#] <= limit &
  ]
];

(* kontrola determinantov A a pomocných matíc *)
cramerDeterminantsWithinBoundsQ[A_, b_] := Module[{ solveData, allDeterminants},

  solveData = cramerSolveData[A, b];
  allDeterminants = Join[{solveData["DetA"]}, solveData["AuxDeterminants"]];

  AllTrue[
    allDeterminants,
    IntegerQ[#] && Abs[#] <= $MaxBounds &
  ]
];


equationOperationCount[diff_String, dim_Integer, method_String] := Switch[
  method,

  "Elimination",
  Switch[diff, "EASY", dim, "MEDIUM", 2 dim, "HARD", 3 dim],

  "Substitution",
  Switch[diff, "EASY", dim, "MEDIUM", 2 dim, "HARD", 2 dim + 2],
  _,
  dim
];

(* ~-~-~ DATA GENERATION ~-~-~ *)

generateData[diff_String, n_, solType_, triType_, scrambleFn_] := Module[{ solved, augSolved, augTask, A, b, vars},

  solved = makeDiagonalAug[n, solType];
  augSolved = solved["Aug"];

  augTask = scrambleFn[diff, augSolved, triType, solType];

  If[augTask === $Failed, Return[$Failed]];

  A = augTask[[All, 1 ;; n]];
  b = augTask[[All, n + 1]];
  vars = buildMatrixVars[n];

  <|
    "A" -> A,
    "b" -> b,
    "x" -> solved["x"],
    "TriType" -> triType,
    "SolutionType" -> solType,
    "Aug" -> augTask,
    "SolvedAug" -> augSolved,
    "Vars" -> vars,
    "n" -> n,
    "BadRow" -> solved["BadRow"],
    "ParamIdx" -> solved["ParamIdx"],
    "ParamIdxs" -> Lookup[solved, "ParamIdxs", {}]
  |>
];

genScrambleTriang[diff_String, aug0_, triType_String, solType_String : "ONE", Gauss_ : True] := Module[{ aug = aug0, n = Length[aug0], bnd, kSet, withinQ, protectedRowQ, chooseK, chooseS, i, r, k, s},

  bnd = $Bounds;
  kSet = If[TrueQ[Gauss], kSetGauss, kSetTri];
  withinQ[row_] := Max[Abs[row]] <= bnd;

  protectedRowQ[rowIdx_] := Switch[solType,
    "NONE",
    rowIdx === n,

    "INFINITE",
    If[n === 3,
      rowIdx === n,
      MemberQ[{n - 1, n}, rowIdx]
    ],

    _,
    False
  ];

  chooseK[target_, src_] := Module[{ k0, cand, ks},
    k0 = RandomChoice[kSet];
    cand = target + k0*src; If[withinQ[cand], Return[k0]];
    cand = target - k0*src; If[withinQ[cand], Return[-k0]];
    ks = SortBy[kSet, Abs];
    Do[
      cand = target + kk*src; If[withinQ[cand], Return[kk]];
      cand = target - kk*src; If[withinQ[cand], Return[-kk]];
      ,
      {kk, ks}
    ];
    1
  ];

  chooseS[row_] := Module[{ s0, cand, ss},
    If[!TrueQ[Gauss], Return[1]];
    s0 = RandomChoice[kSet];
    cand = s0*row; If[withinQ[cand], Return[s0]];
    cand = -s0*row; If[withinQ[cand], Return[-s0]];
    ss = SortBy[kSet, Abs];
    Do[
      cand = t*row; If[withinQ[cand], Return[t]];
      cand = -t*row; If[withinQ[cand], Return[-t]];
      ,
      {t, ss}
    ];
    1
  ];

  If[triType === "L",
    For[i = n, i >= 1, i--,
      If[solType =!= "NONE" || !TrueQ@contradictionRowQ[aug[[i]]],
        For[r = i + 1, r <= n, r++,
          If[protectedRowQ[r], Continue[]];
          k = chooseK[aug[[r]], aug[[i]]];
          If[k =!= 0,
            aug[[r]] = aug[[r]] + k*aug[[i]];
          ];
        ]
      ];
      s = chooseS[aug[[i]]];
      If[s =!= 1,
        aug[[i]] = s*aug[[i]];
      ];
    ],
    For[i = 1, i <= n, i++,
      If[solType =!= "NONE" || !TrueQ@contradictionRowQ[aug[[i]]],
        For[r = i - 1, r >= 1, r--,
          k = chooseK[aug[[r]], aug[[i]]];
          If[k =!= 0,
            aug[[r]] = aug[[r]] + k*aug[[i]];
          ];
        ]
      ];
      s = chooseS[aug[[i]]];
      If[s =!= 1,
        aug[[i]] = s*aug[[i]];
      ];
    ]
  ];

  aug
];

genScrambleGauss[diff_String, aug0_, triType_String, solType_String : "ONE"] := Module[{ n, pairs, chosenPairs, kSet, bnd, maxAttempts, maxKTries, aug, r, i, k, rowNew, currentLower},
  n = Length[aug0];
  pairs = Flatten[Table[{r, i}, {r, 2, n}, {i, 1, r - 1}], 1]; (* (2,1) => R2=R2+k.R1; (3,1); (3,2) *)

  kSet = kSetGauss;
  bnd = $Bounds;

  maxAttempts = $MaxRetryCount;   (* koľkokrát reštartovať celý scramble *)
  maxKTries = 10;     (* koľko rôznych k skúsiť pre jeden pár *)

  Do[
    aug = genScrambleTriang[diff, aug0, "U", solType, False];
    aug = Map[normalizeRow, aug];

    chosenPairs = If[solType === "ONE", RandomSample[pairs, Length[pairs]], pairs];

    Do[
      {r, i} = pair;

      Do[
        k = RandomChoice[kSet];
        rowNew = aug[[r]] + k aug[[i]];
        rowNew = normalizeRow[rowNew];

        If[
          Max[Abs[rowNew]] <= bnd,
          aug[[r]] = rowNew;
          Break[];
        ];
        ,
        {t, 1, maxKTries}
      ];
      ,
      {pair, chosenPairs}
    ];

    currentLower = Count[LowerTriangularize[aug[[All, 1 ;; n]], -1], x_ /; x =!= 0, {2}];
    If[currentLower == Length[pairs], Return[aug]];
    ,
    {attempt, 1, maxAttempts}
  ];
  aug
];

genScrambleGaussJordanPivot[diff_String, aug0_, triType_String, solType_String : "ONE"] := Module[{ aug, n, pivotCount, plannedCols, bnd, tries = 0, trace, observedCols},

  n = Length[aug0];
  bnd = $Bounds;

  pivotCount = Count[
    aug0[[All, 1 ;; n]],
    row_ /; !AllTrue[row, # === 0 &]
  ];

  plannedCols = gaussPlannedPivotSwapColumns[pivotCount];

  While[tries < $MaxRetryCount,
    aug = genScrambleGauss[diff, aug0, triType, solType];

    Do[
      aug = gaussForceAdjacentPivotSwap[aug, i, bnd];
      ,
      {i, Reverse[plannedCols]}
    ];

    trace = gaussForwardEliminationTrace[aug, "MIN"]["Trace"];
    observedCols = DeleteDuplicates[gaussObservedPivotSwapColumns[trace]];

    If[
      (n == 3 && Length[observedCols] == 1) ||
          (n > 3 && Length[observedCols] >= 2),
      Return[aug]
    ];
    tries++;
  ];

  aug
];

genScrambleLU[diff_String, aug0_, triType_String, solType_String : "ONE"] := Module[{ n, x, L, U, A, b, valueLimit, diagLimit, lowerPool, upperPool, diagPool},

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

genScrambleCholesky[diff_String, aug0_, triType_, solType_String : "ONE"] := Module[{ n, solutionVector, lMatrix, aMatrix, bVector, tries = 0, lowerPool, diagPool, diagMax, lowerMax},

  n = Length[aug0];
  solutionVector = aug0[[All, n + 1]];

  {diagMax, lowerMax} = Switch[
    diff,
    "EASY", {4, 2},
    "MEDIUM", {3, 2},
    "HARD", {3, 1},
    _, {3, 1}
  ];

  lowerPool = Join[Range[-lowerMax, -1], {0, 0}, Range[1, lowerMax]];
  diagPool = Range[2, diagMax];

  While[tries < $MaxRetryCount,
    lMatrix = ConstantArray[0, {n, n}];

    Do[
      lMatrix[[i, i]] = RandomChoice[diagPool];
      Do[
        lMatrix[[i, j]] = RandomChoice[lowerPool];
        ,
        {j, 1, i - 1}
      ];
      ,
      {i, 1, n}
    ];

    aMatrix = Together[lMatrix . Transpose[lMatrix]];
    bVector = Together[aMatrix . solutionVector];

    If[
      matrixMaxAbs[aMatrix] <= $MaxBounds &&
          matrixMaxAbs[bVector] <= $MaxBounds &&
          choleskyDecompositionWithinBoundsQ[<|"A" -> aMatrix, "b" -> bVector|>],
      Return[augFromAb[aMatrix, bVector]]
    ];

    tries++;
  ];

  $Failed
];

genScrambleCramer[diff_String, aug0_, triType_String, solType_String : "ONE"] := Module[{ n, solutionVector, A, b, tries = 0},

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


makeDiagonalEquationSystem[dim_Integer, solType_String] := Module[{A, b, x0, paramCol, badRhs, pivotCount, coeffPool},
  coeffPool = nonzeroRange[-$Bounds, $Bounds];

  Switch[
    solType,

    "ONE",
    x0 = RandomInteger[$bRange, dim];
    A = IdentityMatrix[dim];
    b = x0;

    <|
      "A" -> A,
      "b" -> b,
      "x0" -> x0,
      "type" -> "ONE"
    |>,

    "INFINITE",
    pivotCount = dim - 1;
    paramCol = RandomChoice[coeffPool, pivotCount];

    A = ConstantArray[0, {dim, dim}];

    Do[
      A[[i, i]] = 1,
      {i, 1, pivotCount}
    ];

    Do[
      A[[i, dim]] = paramCol[[i]],
      {i, 1, pivotCount}
    ];

    b = Join[RandomInteger[$bRange, pivotCount], {0}];

    <|
      "A" -> A,
      "b" -> b,
      "type" -> "INFINITE",
      "ParamIdx" -> dim,
      "ParamSymbol" -> \[FormalT]
    |>,

    "NONE",
    pivotCount = dim - 1;
    paramCol = RandomChoice[coeffPool, pivotCount];
    badRhs = RandomChoice[coeffPool];

    A = ConstantArray[0, {dim, dim}];

    Do[
      A[[i, i]] = 1,
      {i, 1, pivotCount}
    ];

    Do[
      A[[i, dim]] = paramCol[[i]],
      {i, 1, pivotCount}
    ];

    b = Join[RandomInteger[$bRange, pivotCount], {badRhs}];

    <|
      "A" -> A,
      "b" -> b,
      "type" -> "NONE",
      "BadRow" -> dim
    |>,

    _,
    $Failed
  ]
];

scrambleEquationRows[diff_String, baseData_Association, method_String] := Module[
  {aug, n, bnd, kSet, opCount, pairs, targetRow, sourceRow, k, rowNew, unitPositions},

  aug = augFromAb[baseData["A"], baseData["b"]];
  n = Length[aug];

  bnd = $MaxBounds;
  kSet = nonzeroRange[-$Bounds, $Bounds];
  opCount = equationOperationCount[diff, n, method];

  pairs = DeleteCases[Tuples[Range[n], 2], {i_, i_}];

  Do[
    {targetRow, sourceRow} = RandomChoice[pairs];
    k = RandomChoice[kSet];

    rowNew = normalizeRow[aug[[targetRow]] + k aug[[sourceRow]]];

    If[
      method === "Substitution",
      unitPositions = Position[rowNew[[1 ;; n]], 1 | -1];

      If[
        unitPositions =!= {} && Max[Abs[rowNew]] <= bnd,
        aug[[targetRow]] = rowNew
      ],
      If[
        Max[Abs[rowNew]] <= bnd,
        aug[[targetRow]] = rowNew
      ]
    ],
    {op, 1, opCount}
  ];

  normalizeRow /@ aug
];

genScrambleElimination[diff_String, baseData_Association] := Module[
  {directData, aug, dim, solType},

  dim = Length[baseData["A"]];
  solType = Lookup[baseData, "type", "ONE"];

  If[MemberQ[{"ONE", "NONE", "INFINITE"}, solType],
    Which[
      dim === 2,
      directData = makeDirect2x2OneEquationSystem[solType];

      If[directData =!= $Failed,
        Return[Join[directData, <|"ScrambleType" -> "Elimination"|>]]
      ],

      dim === 3 && MemberQ[{"MEDIUM", "HARD"}, diff],
      directData = makeDirect3x3OneEquationSystem[diff, "Elimination", solType];

      If[directData =!= $Failed,
        Return[Join[directData, <|"ScrambleType" -> "Elimination"|>]]
      ]
    ]
  ];

  aug = scrambleEquationRows[diff, baseData, "Elimination"];

  Join[
    baseData,
    abFromAug[aug],
    <|"Aug" -> aug, "ScrambleType" -> "Elimination"|>
  ]
];

makeDirect2x2OneEquationSystemForSolution[solution_List, solType_String : "ONE"] := Module[
  {vars, solveIndex, otherIndex, unitCoeff, otherCoeffFirst, rhsFirst, rowMultiplier,
    targetCoeffSecond, kCoeff, conflictShift, otherCoeffSecond, rhsSecond,
    rowFirst, rowSecond, A, rhs, aug, solvedVar, otherVar, retries = 0},

  vars = buildEquationVars[2];

  While[retries < $MaxRetryCount,
    solveIndex = RandomChoice[{1, 2}];
    otherIndex = 3 - solveIndex;

    (* prva rovnica ma jednu lahko vyjadritelnu premennu *)
    unitCoeff = RandomChoice[{-1, 1}];
    otherCoeffFirst = RandomChoice[strongKSet[]];
    rhsFirst = unitCoeff*solution[[solveIndex]] + otherCoeffFirst*solution[[otherIndex]];

    If[Abs[rhsFirst] > $MaxBounds, retries++; Continue[]];

    If[solType === "ONE",
      targetCoeffSecond = RandomChoice[kSet];
      kCoeff = RandomChoice[kSet];
      conflictShift = 0;,

    (* infinite/none: druha rovnica je nenulovy nenasobok prvej rovnice jednotkou *)
      rowMultiplier = RandomChoice[strongKSet[]];
      targetCoeffSecond = unitCoeff*rowMultiplier;
      kCoeff = 0;
      conflictShift = If[solType === "NONE", RandomChoice[kSet], 0];
    ];

    otherCoeffSecond = kCoeff + targetCoeffSecond*otherCoeffFirst/unitCoeff;
    rhsSecond = kCoeff*solution[[otherIndex]] + targetCoeffSecond*rhsFirst/unitCoeff + conflictShift;

    rowFirst = ConstantArray[0, 2];
    rowSecond = ConstantArray[0, 2];

    rowFirst[[solveIndex]] = unitCoeff;
    rowFirst[[otherIndex]] = otherCoeffFirst;

    rowSecond[[solveIndex]] = targetCoeffSecond;
    rowSecond[[otherIndex]] = otherCoeffSecond;

    A = {rowFirst, rowSecond};
    rhs = {rhsFirst, rhsSecond};

    If[
      AllTrue[Join[rowFirst, rowSecond], # =!= 0 &] &&
          Max[Abs[Join[Flatten[A], rhs]]] <= $MaxBounds &&
          equationSolutionTypeMatchesQ[A, rhs, solType],
      aug = augFromAb[A, rhs];

      solvedVar = vars[[solveIndex]];
      otherVar = vars[[otherIndex]];

      Return[
        <|
          "A" -> A,
          "b" -> rhs,
          "x0" -> solution,
          "type" -> solType,
          "Aug" -> aug,
          "Direct2x2Q" -> True,
          "GeneratedRule" -> (solvedVar -> Together[(rhsFirst - otherCoeffFirst*otherVar)/unitCoeff])
        |>
      ]
    ];

    retries++;
  ];

  $Failed
];

makeDirect2x2OneEquationSystem[solType_String : "ONE"] := makeDirect2x2OneEquationSystemForSolution[RandomInteger[$bRange, 2], solType];

makeDirect3x3OneEquationSystem[diff_String, method_String : "Substitution", solType_String : "ONE"] := Module[
  {vars, solution, elimIndex, solvedIndex, remIdxs, remVars, unitCoeff, coeffPool,
    coeffs, firstRow, secondRow, thirdRow, firstRhs, secondRhs, thirdRhs,
    twoData, A2, rhs2, tCoeff, liftedRow, liftedRhs, A, rhs, aug,
    ruleExpr, mediumQ, validShapeQ, pivotCoeff, splitRow, anchorRow,
    pickSplitCoeff, pickAnchorCoeff, retries = 0},

  vars = buildEquationVars[3];
  coeffPool = strongKSet[];
  mediumQ = diff === "MEDIUM";

  pickSplitCoeff[target_] := Module[{candidates},
    candidates = Select[kSet, # =!= target && Abs[target - #] <= $MaxBounds &];
    If[candidates === {}, $Failed, RandomChoice[candidates]]
  ];

  pickAnchorCoeff[targetA_, targetB_] := Module[{candidates},
    candidates = Select[
      kSet,
      # =!= targetA && # =!= targetB &&
          Abs[targetA - #] <= $MaxBounds &&
          Abs[targetB - #] <= $MaxBounds &
    ];
    If[candidates === {}, $Failed, RandomChoice[candidates]]
  ];

  While[retries < $MaxRetryCount,
    solution = RandomInteger[$bRange, 3];
    solvedIndex = RandomChoice[Range[3]];
    elimIndex = solvedIndex;
    remIdxs = Delete[Range[3], solvedIndex];
    remVars = vars[[remIdxs]];

    twoData = makeDirect2x2OneEquationSystemForSolution[solution[[remIdxs]], solType];
    If[twoData === $Failed, retries++; Continue[]];

    A2 = twoData["A"];
    rhs2 = twoData["b"];

    If[method === "Substitution",
      unitCoeff = RandomChoice[{-1, 1}];

      (* medium ma v prvej rovnici prave jednu nulu, hard nema nulu *)
      coeffs = If[
        mediumQ,
        ReplacePart[ConstantArray[0, 2], RandomChoice[{1, 2}] -> RandomChoice[coeffPool]],
        RandomChoice[coeffPool, 2]
      ];

      firstRow = ConstantArray[0, 3];
      firstRow[[solvedIndex]] = unitCoeff;
      firstRow[[remIdxs]] = coeffs;
      firstRhs = firstRow . solution;

      If[Abs[firstRhs] > $MaxBounds, retries++; Continue[]];

      A = {firstRow};
      rhs = {firstRhs};

      (* 2x2 rovnice zdvihneme do 3x3 tak, aby po dosadeni vznikli naspat *)
      Do[
        tCoeff = RandomChoice[kSet];

        liftedRow = ConstantArray[0, 3];
        liftedRow[[solvedIndex]] = tCoeff;
        liftedRow[[remIdxs]] = A2[[i]] + tCoeff*coeffs/unitCoeff;

        liftedRhs = rhs2[[i]] + tCoeff*firstRhs/unitCoeff;

        AppendTo[A, liftedRow];
        AppendTo[rhs, liftedRhs],
        {i, 1, 2}
      ];

      validShapeQ = If[
        mediumQ,
        Count[firstRow, 0] === 1 && AllTrue[Flatten[A[[2 ;;]]], # =!= 0 &],
        AllTrue[Flatten[A], # =!= 0 &]
      ];

      If[
        validShapeQ &&
            Max[Abs[Join[Flatten[A], rhs]]] <= $MaxBounds &&
            equationSolutionTypeMatchesQ[A, rhs, solType],
        aug = augFromAb[A, rhs];
        ruleExpr = Together[(firstRhs - firstRow[[remIdxs]] . remVars)/unitCoeff];

        Return[
          <|
            "A" -> A,
            "b" -> rhs,
            "x0" -> solution,
            "type" -> solType,
            "Aug" -> aug,
            "Direct3x3Q" -> True,
            "Direct3x3Method" -> "Substitution",
            "GeneratedRule" -> (vars[[solvedIndex]] -> ruleExpr),
            "SolvedVarIndex" -> solvedIndex,
            "RemainingVarIndexes" -> remIdxs,
            "Base2x2" -> twoData
          |>
        ]
      ],
      If[method === "Elimination",
        pivotCoeff = RandomChoice[{-1, 1}];

        If[mediumQ,
        (* medium: prva rovnica uz je jedna 2x2 rovnica a ma nulovy koeficient pri eliminovanej premennej *)
          firstRow = ConstantArray[0, 3];
          firstRow[[elimIndex]] = 0;
          firstRow[[remIdxs]] = A2[[1]];
          firstRhs = rhs2[[1]];

          splitRow = pickSplitCoeff /@ A2[[2]];
          If[MemberQ[splitRow, $Failed], retries++; Continue[]];

          secondRow = ConstantArray[0, 3];
          thirdRow = ConstantArray[0, 3];

          secondRow[[elimIndex]] = pivotCoeff;
          thirdRow[[elimIndex]] = -pivotCoeff;

          secondRow[[remIdxs]] = splitRow;
          thirdRow[[remIdxs]] = A2[[2]] - splitRow;

          secondRhs = secondRow . solution;
          thirdRhs = rhs2[[2]] - secondRhs;,

        (* hard: bez nul, prva rovnica sa scita s druhou aj tretou *)
          anchorRow = MapThread[pickAnchorCoeff, {A2[[1]], A2[[2]]}];
          If[MemberQ[anchorRow, $Failed], retries++; Continue[]];

          firstRow = ConstantArray[0, 3];
          secondRow = ConstantArray[0, 3];
          thirdRow = ConstantArray[0, 3];

          firstRow[[elimIndex]] = pivotCoeff;
          secondRow[[elimIndex]] = -pivotCoeff;
          thirdRow[[elimIndex]] = -pivotCoeff;

          firstRow[[remIdxs]] = anchorRow;
          secondRow[[remIdxs]] = A2[[1]] - anchorRow;
          thirdRow[[remIdxs]] = A2[[2]] - anchorRow;

          firstRhs = firstRow . solution;
          secondRhs = rhs2[[1]] - firstRhs;
          thirdRhs = rhs2[[2]] - firstRhs;
        ];

        A = {firstRow, secondRow, thirdRow};
        rhs = {firstRhs, secondRhs, thirdRhs};

        validShapeQ = If[
          mediumQ,
          Count[firstRow, 0] === 1 && AllTrue[Join[secondRow, thirdRow], # =!= 0 &],
          AllTrue[Flatten[A], # =!= 0 &]
        ];

        If[
          validShapeQ &&
              Max[Abs[Join[Flatten[A], rhs]]] <= $MaxBounds &&
              equationSolutionTypeMatchesQ[A, rhs, solType] && pickElimVar3[A] === elimIndex,
          Return[
            <|
              "A" -> A,
              "b" -> rhs,
              "x0" -> solution,
              "type" -> solType,
              "Aug" -> augFromAb[A, rhs],
              "Direct3x3Q" -> True,
              "Direct3x3Method" -> "Elimination",
              "EliminationTargetIndex" -> elimIndex,
              "EliminationTarget" -> vars[[elimIndex]],
              "RemainingVarIndexes" -> remIdxs,
              "Base2x2" -> twoData
            |>
          ]
        ]
      ]
    ];

    retries++;
  ];

  $Failed
];

genScrambleSubstitution[diff_String, baseData_Association] := Module[
  {directData, aug, dim, solType},

  dim = Length[baseData["A"]];
  solType = Lookup[baseData, "type", "ONE"];

  If[MemberQ[{"ONE", "NONE", "INFINITE"}, solType],
    Which[
      dim === 2,
      directData = makeDirect2x2OneEquationSystem[solType];

      If[directData =!= $Failed,
        Return[Join[directData, <|"ScrambleType" -> "Substitution"|>]]
      ],

      dim === 3,
      directData = makeDirect3x3OneEquationSystem[diff, "Substitution", solType];

      If[directData =!= $Failed,
        Return[Join[directData, <|"ScrambleType" -> "Substitution"|>]]
      ];

      (* pri HARD 3x3 nechceme fallback na trojuholníkovú/riedku sústavu *)
      If[diff === "HARD",
        Return[$Failed]
      ]
    ]
  ];

  aug = scrambleEquationRows[diff, baseData, "Substitution"];

  Join[
    baseData,
    abFromAug[aug],
    <|"Aug" -> aug, "ScrambleType" -> "Substitution"|>
  ]
];

(* ~-~-~ STEP GENERATION HELPERS ~-~-~ *)

(* spätné dosadzovanie pre trojuholníkovú maticu *)
appendTriangularSubstitutionSteps[mat_, rhs_, vars_, sol0_, order_List, content_, initialKnownIdxs_List : {}, skipIdxs_List : {}] := Module[{ n = Length[mat], sol = sol0, out = content,
  solvedIdxs, boldVal, coeffTimes, addOneRow},

  solvedIdxs = initialKnownIdxs;

  boldVal[val_] := Module[{ expandedVal = Expand[val]},
    Style[
      If[(IntegerQ[expandedVal] && expandedVal < 0) || MatchQ[expandedVal, _Plus],
        Row[{"(", TraditionalForm[expandedVal], ")"}],
        TraditionalForm[expandedVal]
      ],
      Bold
    ]
  ];

  coeffTimes[a_, x_] := If[a === 1, x, Row[{tf[a], "\[CenterDot]", x}]];

  addOneRow[rowIdx_Integer] := Module[
    {row, pivot, rhsVal, knownIdxs, sumProducts, exprVal, symExpr, subExpr, buildExpr, formatSolveLine},

    If[MemberQ[skipIdxs, rowIdx], Return[]];

    row = mat[[rowIdx]];
    pivot = row[[rowIdx]];
    rhsVal = rhs[[rowIdx]];

    If[pivot === 0, Return[]];

    knownIdxs = Select[
      Complement[Range[n], {rowIdx}],
      row[[#]] =!= 0 && MemberQ[solvedIdxs, #] &
    ];

    sumProducts = If[knownIdxs === {}, 0, Total@Table[row[[colIdx]] sol[[colIdx]], {colIdx, knownIdxs}]];
    exprVal = Expand[Together[(rhsVal - sumProducts)/pivot]];
    sol[[rowIdx]] = exprVal;

    buildExpr[valueQ_] := Row @ Flatten @ Join[
      {TraditionalForm[Expand[rhsVal]]},
      Table[
        With[
          {a = row[[colIdx]], v = If[valueQ, boldVal[sol[[colIdx]]], Style[tf[vars[[colIdx]]], Bold]]},
          {If[a > 0, " - ", " + "], coeffTimes[Abs[a], v]}
        ],
        {colIdx, knownIdxs}
      ]
    ];

    formatSolveLine[expr_, hasKnownQ_] := Which[
      pivot === 1, Row[{tf[lhsStyle[vars[[rowIdx]]]], " = ", expr}],
      pivot === -1, Row[{tf[lhsStyle[vars[[rowIdx]]]], " = -(", expr, ")"}],
      hasKnownQ, Row[{tf[lhsStyle[vars[[rowIdx]]]], " = (", expr, ") / ", tf[pivot]}],
      True, Row[{tf[lhsStyle[vars[[rowIdx]]]], " = ", expr, " / ", tf[pivot]}]
    ];

    If[knownIdxs =!= {} || pivot =!= 1,
      symExpr = buildExpr[False];
      AppendTo[out, formatSolveLine[symExpr, knownIdxs =!= {}]];

      If[knownIdxs =!= {},
        subExpr = buildExpr[True];
        AppendTo[out, formatSolveLine[subExpr, True]];
      ];
    ];

    AppendTo[out, highlightResultEquation[vars[[rowIdx]], exprVal]];

    AppendTo[solvedIdxs, rowIdx];
  ];

  Scan[addOneRow, order];

  {sol, out}
];

(* --- Gauss / Gauss-Jordan / Elementar HELPERS --- *)

(* výber riadku s najmenším pivotom v absolútnej hodnote *)
gaussPivotRowByMinAbs[aug_, i_Integer] := Module[{ n = Length[aug], candidates, currentPivot, betterCandidates},

  candidates = Select[Range[i, n], aug[[#, i]] =!= 0 &];
  If[candidates === {}, Return[i]];

  currentPivot = aug[[i, i]];

  If[currentPivot =!= 0,
    betterCandidates = Select[candidates, Abs[aug[[#, i]]] < Abs[currentPivot] &];
    If[betterCandidates === {},
      i,
      First @ MinimalBy[betterCandidates, Abs[aug[[#, i]]] &]
    ],
    First @ MinimalBy[candidates, Abs[aug[[#, i]]] &]
  ]
];

(* výber prvého nenulového pivotu pod diagonálou *)
gaussPivotRowByNonzero[aug_, i_Integer] := Module[{ n = Length[aug], candidates},
  If[aug[[i, i]] =!= 0, Return[i]];
  candidates = Select[Range[i + 1, n], aug[[#, i]] =!= 0 &];
  If[candidates === {}, i, First[candidates]]
];

(* vysvetlenie výmeny pivotových riadkov *)
gaussPivotSwapExplanation[aug_, i_Integer, k_Integer] := Module[{ currentPivot, newPivot},
  currentPivot = aug[[i, i]];
  newPivot = aug[[k, i]];

  If[currentPivot === 0,
    Row[{
      "V ", i, ". stĺpci je aktuálny pivot nulový, preto si nižšie vyberieme nenulový pivot ",
      tf[newPivot], " z riadku R", k, "."
    }],
    Row[{
      "V ", i, ". stĺpci sa nižšie nachádza menší pivot v absolútnej hodnote. Preto namiesto ",
      tf[currentPivot], " zvolíme ", tf[newPivot], " a prehodíme R", i, " a R", k, "."
    }]
  ]
];

(* trace celej Gauss-Jordanovej eliminácie *)
gaussJordanEliminationTrace[aug_, pivotMode_: "MIN"] := Module[{ workAug, n, i, r, pivotRowFn, pivotRow, pivotValue, elimRes, trace = {}, after},

  workAug = aug;
  n = Length[workAug];

  pivotRowFn = Switch[pivotMode,
    "MIN", gaussPivotRowByMinAbs,
    _, gaussPivotRowByNonzero
  ];

  AppendTo[trace, <|"Type" -> "Start", "Matrix" -> workAug|>];

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


(* --- LU / Cholesky HELPERS --- *)

(* výpočet dát pre LU rozklad a následné dosadenie *)
luSolveData[A_, b_] := Module[{ n, L, U, y, x, i, j, terms, sumTerm, pivotValue},

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

(* výpočet dát pre Choleského rozklad a následné dosadenie *)
choleskySolveData[A_, b_] := Module[{ n, L, y, x, i, j, diagTerms, mixedTerms, diagRadicand, diagValue, sumTerm},

  n = Length[A];

  If[!SymmetricMatrixQ[A], Return[$Failed]];

  L = ConstantArray[0, {n, n}];
  y = ConstantArray[0, n];
  x = ConstantArray[0, n];

  Do[
    diagTerms = Table[L[[i, k]]^2, {k, 1, i - 1}];
    diagRadicand = Together[A[[i, i]] - Total[diagTerms]];

    If[!IntegerQ[diagRadicand] || diagRadicand <= 0, Return[$Failed]];

    diagValue = Sqrt[diagRadicand];
    If[!IntegerQ[diagValue], Return[$Failed]];

    L[[i, i]] = diagValue;

    Do[
      mixedTerms = Table[L[[j, k]]*L[[i, k]], {k, 1, i - 1}];
      sumTerm = Total[mixedTerms];
      L[[j, i]] = Together[(A[[j, i]] - sumTerm)/L[[i, i]]];

      If[!IntegerQ[L[[j, i]]], Return[$Failed]];
      ,
      {j, i + 1, n}
    ];
    ,
    {i, 1, n}
  ];

  Do[
    mixedTerms = Table[L[[i, k]]*y[[k]], {k, 1, i - 1}];
    sumTerm = Total[mixedTerms];
    y[[i]] = Together[(b[[i]] - sumTerm)/L[[i, i]]];

    If[!IntegerQ[y[[i]]], Return[$Failed]];
    ,
    {i, 1, n}
  ];

  Do[
    mixedTerms = Table[L[[k, i]]*x[[k]], {k, i + 1, n}];
    sumTerm = Total[mixedTerms];
    x[[i]] = Together[(y[[i]] - sumTerm)/L[[i, i]]];

    If[!IntegerQ[x[[i]]], Return[$Failed]];
    ,
    {i, n, 1, -1}
  ];

  <|"L" -> L, "Y" -> y, "X" -> x|>
];

luEntrySymbol[sym_String, i_, j_] := Subscript[Style[sym, Italic], Row[{i, ",", j}]];

luScalarSymbol[sym_String, i_] := Subscript[Style[sym, Italic], i];

luFactorDisplay[val_] := If[NumberQ[val] && val < 0, Row[{"(", tft[val], ")"}], tft[val]];

luCoeffTimes[a_, x_] := Which[
  a === 1, x,
  a === -1, Row[{"-", x}],
  True, Row[{tft[a], "\[CenterDot]", x}]
];

luSumDisplay[terms_List] := Module[{values, clean, first, rest},
  values = Times @@@ Select[terms, #[[1]] =!= 0 && #[[2]] =!= 0 &];
  clean = Select[Together /@ values, # =!= 0 &];

  If[clean === {}, Return[tft[0]]];

  first = First[clean];
  rest = Rest[clean];

  Row @ Flatten @ Join[
    {If[first < 0, Row[{"-", tft[Abs[first]]}], tft[first]]},
    Table[
      If[val < 0,
        {" - ", tft[Abs[val]]},
        {" + ", tft[val]}],
      {val, rest}
    ]
  ]
];

luWrappedSumDisplay[terms_List] := Module[{vals, sumVal, sumDisp, needsParensQ},
  vals = Times @@@ Select[terms, #[[1]] =!= 0 && #[[2]] =!= 0 &];
  sumDisp = luSumDisplay[terms];

  needsParensQ = Which[
    vals === {}, False,
    Length[vals] > 1, True,
    True,
    sumVal = Together[First[vals]];
    NumericQ[sumVal] && sumVal < 0
  ];

  If[needsParensQ, Row[{"(", sumDisp, ")"}], sumDisp]
];

luLinearCombinationDisplay[terms_List] := Module[{ clean, first, rest, formatPositiveTerm, formatFirstTerm, formatNextTerm},

  clean = Select[terms, Together[#[[1]]] =!= 0 &];
  If[clean === {}, Return[tft[0]]];

  formatPositiveTerm[{coef_, var_}] := luCoeffTimes[coef, var];

  formatFirstTerm[{coef_, var_}] := If[coef < 0,
    Row[{"-", formatPositiveTerm[{Abs[coef], var}]}],
    formatPositiveTerm[{coef, var}]
  ];

  formatNextTerm[{coef_, var_}] := If[coef < 0,
    {" - ", formatPositiveTerm[{Abs[coef], var}]},
    {" + ", formatPositiveTerm[{coef, var}]}
  ];

  first = First[clean];
  rest = Rest[clean];

  Row @ Flatten @ Join[
    {formatFirstTerm[first]},
    formatNextTerm /@ rest
  ]
];

choleskySqrtDisplay[arg_] := Row[{"\[Sqrt]", "(", arg, ")"}];

choleskySymbolicSquareSum[indices_List, row_Integer] := If[
  indices === {},
  tft[0],
  Row @ Riffle[
    (Superscript[luEntrySymbol["l", row, #], "2"] & /@ indices),
    " + "
  ]
];

choleskySymbolicProductSum[indices_List, row_Integer, col_Integer] := If[
  indices === {},
  tft[0],
  Row @ Riffle[
    (Row[{luEntrySymbol["l", row, #], "\[CenterDot]", luEntrySymbol["l", col, #]}] & /@ indices),
    " + "
  ]
];

choleskyNumericSquareSum[vals_List] := If[
  vals === {},
  tft[0],
  Row @ Riffle[
    (Superscript[luFactorDisplay[#], "2"] & /@ vals),
    " + "
  ]
];

buildCholeskyDiagonalLines[i_Integer, A_, L_, value_] := Module[{ diagTerms, diagRadicand},

  diagTerms = Table[L[[i, k]]^2, {k, 1, i - 1}];
  diagRadicand = Together[A[[i, i]] - Total[diagTerms]];

  If[diagTerms === {},
    {
      Row[{
        lhsStyle[luEntrySymbol["l", i, i]], " = ",
        choleskySqrtDisplay[luEntrySymbol["a", i, i]], " = ",
        choleskySqrtDisplay[tft[A[[i, i]]]], " = ",
        Style[tft[value], Bold, Blue]
      }]
    },
    {
      Row[{
        lhsStyle[luEntrySymbol["l", i, i]], " = ",
        choleskySqrtDisplay[
          Row[{luEntrySymbol["a", i, i], " - (", choleskySymbolicSquareSum[Range[i - 1], i], ")"}]
        ]
      }],
      Row[{
        lhsStyle[luEntrySymbol["l", i, i]], " = ",
        choleskySqrtDisplay[
          Row[{tft[A[[i, i]]], " - (", choleskyNumericSquareSum[L[[i, 1 ;; i - 1]]], ")"}]
        ]
      }],
      Row[{
        lhsStyle[luEntrySymbol["l", i, i]], " = ",
        choleskySqrtDisplay[tft[diagRadicand]], " = ",
        Style[tft[value], Bold, Blue]
      }]
    }
  ]
];

buildCholeskyOffDiagonalLines[j_Integer, i_Integer, A_, L_, diagValue_, value_] := Module[{ mixedTerms, numerator},

  mixedTerms = Table[{L[[j, k]], L[[i, k]]}, {k, 1, i - 1}];
  numerator = Together[A[[j, i]] - Total[Times @@@ mixedTerms]];

  If[mixedTerms === {},
    {
      Row[{
        lhsStyle[luEntrySymbol["l", j, i]], " = ",
        luEntrySymbol["a", j, i], " / ", luEntrySymbol["l", i, i],
        " = ", tft[A[[j, i]]], " / ", tft[diagValue],
        " = ", Style[tft[value], Bold, Blue]
      }]
    },
    {
      Row[{
        lhsStyle[luEntrySymbol["l", j, i]], " = (",
        luEntrySymbol["a", j, i], " - (", choleskySymbolicProductSum[Range[i - 1], j, i], ")) / ",
        luEntrySymbol["l", i, i]
      }],
      Row[{
        lhsStyle[luEntrySymbol["l", j, i]], " = (",
        tft[A[[j, i]]], " - ", luWrappedSumDisplay[mixedTerms], ") / ", tft[diagValue]
      }],
      Row[{
        lhsStyle[luEntrySymbol["l", j, i]], " = ",
        tft[numerator], " / ", tft[diagValue],
        " = ", Style[tft[value], Bold, Blue]
      }]
    }
  ]
];

(* --- Cramer HELPERS --- *)

cramerRandomNonzeroValue[maxAbs_Integer : 4] := RandomChoice[DeleteCases[Range[-maxAbs, maxAbs], 0]];

(* náhodná regulárna 3x3 matica s obmedzeným determinantom *)
cramerRandomInvertible3x3[maxAbs_Integer : 4, maxDet_Integer : 30] := Module[{ candidate, tries = 0, pool},
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

generateCramerEasyMatrix[solutionVector_List] := Module[{ candidate, rhsVector, tries = 0, pool},

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

(* determinant 4x4 cez jeden Laplaceov rozvoj a determinant 3x3 *)
renderCramer4x4Reduction[matrix_, label_] := Module[{ content = {}, line1, signed1, minor3, minor3Label, det3Data, value},

  minor3Label = Subscript[Style["M", Italic], 3];

  AppendTo[content, cramerLabeledMatrixGrid[label, matrix]];

  line1 = cramerSingletonLineData[matrix];
  If[line1 === Missing["NotFound"],
    value = Together[Det[matrix]];
    AppendTo[content, "Matica nemá vhodný riedky riadok ani stĺpec, preto determinant dopočítame priamo."];
    AppendTo[content, resultEquationLine[cramerDetLabel[label], value]];
    Return[<|"Content" -> content, "Value" -> value, "Matrix" -> matrix|>];
  ];

  signed1 = Together[
    (-1)^(line1["PivotRow"] + line1["PivotColumn"]) *
        matrix[[line1["PivotRow"], line1["PivotColumn"]]]
  ];
  minor3 = cramerMinor[matrix, line1["PivotRow"], line1["PivotColumn"]];

  AppendTo[content, cramerLaplaceExplanation[line1]];
  AppendTo[content, cramerLaplaceReductionPanel[matrix, line1, minor3Label, minor3]];
  AppendTo[content, Row[{
    cramerDetLabel[label], " = ", cramerFactor[signed1],
    " \[CenterDot] ", cramerDetLabel[minor3Label]
  }]];

  det3Data = renderCramer3x3Det[minor3, minor3Label];
  content = Join[content, det3Data["Content"]];

  addGap[content, 4];
  value = Together[signed1 det3Data["Value"]];

  AppendTo[content, Row[{
    cramerDetLabel[label], " = ",
    cramerFactor[signed1], " \[CenterDot] ", cramerDetLabel[minor3Label],
    " = ",
    cramerFactor[signed1], " \[CenterDot] ", cramerFactor[det3Data["Value"]],
    " = ",
    cramerFactor[value]
  }]];
  AppendTo[content, resultEquationLine[cramerDetLabel[label], value]];

  <|"Content" -> content, "Value" -> value, "Matrix" -> matrix|>
];

generateCramerMediumMatrix[solutionVector_List] := Module[{ core, candidate, rhsVector, s1, tries = 0},

  While[tries < $MaxRetryCount,
    core = cramerRandomInvertible3x3[3, 18];
    If[core === $Failed,
      Return[$Failed]
    ];

    s1 = cramerRandomNonzeroValue[3];

    candidate = ConstantArray[0, {4, 4}];
    candidate[[1, 1]] = s1;
    candidate[[2 ;; 4, 2 ;; 4]] = core;

    rhsVector = candidate . solutionVector;

    If[
      Det[candidate] =!= 0 &&
          Abs[Det[candidate]] <= Min[60, $MaxBounds] &&
          matrixMaxAbs[rhsVector] <= $MaxBounds,
      Return[candidate]
    ];

    tries++;
  ];

  $Failed
];

generateCramerHardMatrix[solutionVector_List] := Module[{ core, candidate, rhsVector, s1, s2, tries = 0},

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

(* determinanty hlavnej a pomocných matíc pre Cramerovo pravidlo *)
cramerSolveData[A_, b_] := Module[{ detA, auxMatrices, auxDeterminants, solution},

  detA = Together[Det[A]];
  auxMatrices = Table[
    ReplacePart[A, Table[{r, i} -> b[[r]], {r, 1, Length[b]}]],
    {i, 1, Length[b]}
  ];
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

(* text pre laplaceov rozvoj *)
cramerLaplaceExplanation[lineData_Association] := If[
  lineData["Type"] === "Row",
  Row[{"Použijeme Laplaceov rozvoj podľa ", lineData["LineIndex"], ". riadku, lebo obsahuje jeden nenulový prvok."}],
  Row[{"Použijeme Laplaceov rozvoj podľa ", lineData["LineIndex"], ". stĺpca, lebo obsahuje jeden nenulový prvok."}]
];

cramerMatrixLabel[var_] := Subscript[Style["A", Italic], Style[var, Italic]];

cramerDetLabel[label_] := Row[{"det(", label, ")"}];

(* vykreslenie matice so zvýraznením riadku, stĺpca alebo pivotu *)
cramerMatrixCard[matrix_, hi_Association : <||>] := Module[
  {activeRow, activeColumn, pivotPos, focusCells, columnAsRowQ, rowTextColor, colTextColor, focusTextColor, pivotTextColor, zeroTextColor, styledMatrix},

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

  styledMatrix = MapIndexed[
    Function[{value, index},
      Module[{i = index[[1]], j = index[[2]], styleOpts = {}, textColor = Automatic, styleArgs, inRowQ, inColumnQ, inFocusQ},
        inRowQ = IntegerQ[activeRow] && i === activeRow;
        inColumnQ = IntegerQ[activeColumn] && j === activeColumn;
        inFocusQ = MemberQ[focusCells, {i, j}];

        Which[
          ListQ[pivotPos] && pivotPos === {i, j},
          textColor = pivotTextColor;
          styleOpts = {Bold},

          inFocusQ, textColor = focusTextColor,

          inColumnQ, textColor = If[columnAsRowQ, rowTextColor, colTextColor],

          inRowQ, textColor = rowTextColor,

          value === 0, textColor = zeroTextColor,

          True, Null
        ];

        styleArgs = Join[If[textColor === Automatic, {}, {textColor}], styleOpts]; Style[TraditionalForm[value], Sequence @@ styleArgs]
      ]
    ],
    matrix,
    {2}];

  styledPlainMatrix[styledMatrix]
];

(* panel pre Laplaceov rozvoj so zvýrazneným minorom *)
cramerLaplaceReductionPanel[matrix_, lineData_Association, minorLabel_, minorMatrix_] := Module[{highlight},
  highlight = If[
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

  Grid[
    {{
      cramerMatrixCard[matrix, highlight],
      Style["\[LongRightArrow]", Bold, FontSize -> 24, GrayLevel[0.2]],
      labeledMatrixBlock[
        minorLabel,
        cramerMatrixCard[minorMatrix, <|"FontSize" -> 13, "CellWidth" -> 1.05|>]
      ]
    }},
    Alignment -> {Center, Center, Center},
    Spacings -> {1.8, 1}
  ]
];

cramerFactor[value_] := If[
  NumberQ[value] && value < 0,
  Row[{"(", tft[value], ")"}],
  tft[value]
];

cramerLabeledMatrixGrid[label_, matrix_, hi_Association : <||>] := labeledMatrixBlock[label, cramerMatrixCard[matrix, hi]];

(* porovnanie pôvodnej a pomocnej matice so zvýrazneným nahradeným stĺpcom *)
cramerAuxiliaryMatrixPanel[A_, auxMatrix_, column_Integer, auxLabel_] := Module[{ leftBg, rightBg, matrixWithColumnBackground},

  leftBg = RGBColor[0.95, 0.92, 1.00];
  rightBg = RGBColor[0.90, 0.95, 1.00];

  (* lokálne vykreslenie matice so zvýrazneným stĺpcom *)
  matrixWithColumnBackground[m_, bg_] := Module[
    {nRows, nCols, leftBracketCell, rightBracketCell, makeCell, rows},

    {nRows, nCols} = Dimensions[m];

    makeCell[i_, j_] := Module[{ cell},
      cell = TraditionalForm[m[[i, j]]];

      If[j === column,
        cell = Style[cell, Bold]
      ];

      Item[
        Pane[cell, ImageSize -> {Automatic, 18}, Alignment -> {Right, Center}],
        Background -> If[j === column, bg, None]
      ]
    ];

    leftBracketCell = Item["", Frame -> {{True, False}, {True, True}}];
    rightBracketCell = Item["", Frame -> {{False, True}, {True, True}}];

    rows = Table[
      Join[
        {If[i === 1, leftBracketCell, SpanFromAbove]},
        Table[makeCell[i, j], {j, 1, nCols}],
        {If[i === 1, rightBracketCell, SpanFromAbove]}
      ],
      {i, 1, nRows}
    ];

    Grid[
      rows,
      Alignment -> Join[{Center}, ConstantArray[Right, nCols], {Center}],
      Spacings -> {1, 1},
      BaseStyle -> {FontSize -> 14}
    ]
  ];

  Grid[
    {{
      labeledMatrixBlock[
        Style["A", Italic],
        matrixWithColumnBackground[A, leftBg]
      ],
      Style["\[LongRightArrow]", Bold, FontSize -> 24, GrayLevel[0.2]],
      labeledMatrixBlock[
        auxLabel,
        matrixWithColumnBackground[auxMatrix, rightBg]
      ]
    }},
    Alignment -> {Center, Center, Center},
    Spacings -> {1.5, 0}
  ]
];

cramerZeroRowIndex[matrix_] := FirstCase[Range[Length[matrix]], row_ /; AllTrue[matrix[[row]], # === 0 &], Missing["NotFound"]];

cramerMinor[matrix_, row_Integer, column_Integer] := Module[{ withoutRow},
  withoutRow = Delete[matrix, row];
  Map[Delete[#, column] &, withoutRow]
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

(* farebné zvýraznenie diagonál pre Sarrusovo pravidlo *)
cramer3x3StyledMatrixByMode[matrix_, mode_String] := Module[{groups, colors, modeColor, styled},

  {groups, colors} = Switch[
    mode,
    "Positive",
    {
      {{{1, 1}, {2, 2}, {3, 3}}, {{1, 2}, {2, 3}, {3, 1}}, {{1, 3}, {2, 1}, {3, 2}}},
      cramer3x3PositiveColors
    },
    "Negative",
    {
      {{{1, 3}, {2, 2}, {3, 1}}, {{1, 1}, {2, 3}, {3, 2}}, {{1, 2}, {2, 1}, {3, 3}}},
      cramer3x3NegativeColors
    }
  ];

  modeColor[pos_] := FirstCase[
    Range[Length[groups]],
    k_ /; MemberQ[groups[[k]], pos] :> colors[[k]],
    Black
  ];

  styled = MapIndexed[
    Style[TraditionalForm[#1], FontColor -> modeColor[#2], Bold] &,
    matrix,
    {2}
  ];

  styledPlainMatrix[styled]
];

(* vzorec Sarrusovho pravidla pre konkrétnu maticu 3x3 *)
cramer3x3FormulaDisplay[matrix_] := Module[{a, b, c, d, e, f, g, h, i, termProduct},
  {{a, b, c}, {d, e, f}, {g, h, i}} = matrix;

  termProduct[values_List, color_] := Row @ Riffle[
    (Style[cramerFactor[#], FontColor -> color, Bold] & /@ values),
    Style["\[CenterDot]", FontColor -> color, Bold]
  ];

  Row[{
    termProduct[{a, e, i}, cramer3x3PositiveColors[[1]]],
    " + ",
    termProduct[{b, f, g}, cramer3x3PositiveColors[[2]]],
    " + ",
    termProduct[{c, d, h}, cramer3x3PositiveColors[[3]]],
    " - ",
    termProduct[{c, e, g}, cramer3x3NegativeColors[[1]]],
    " - ",
    termProduct[{a, f, h}, cramer3x3NegativeColors[[2]]],
    " - ",
    termProduct[{b, d, i}, cramer3x3NegativeColors[[3]]]
  }]
];

(* zápis súčtu so znamienkami bez nulových členov *)
cramerSignedValueSum[values_List] := Module[{ clean, first, rest},
  clean = Select[Together /@ values, # =!= 0 &];

  If[clean === {},
    Return[tft[0]]
  ];

  first = First[clean];
  rest = Rest[clean];

  Row @ Flatten @ Join[
    {
      If[first < 0,
        Row[{"-", tft[Abs[first]]}],
        tft[first]
      ]
    },
    Table[
      If[value < 0,
        {" - ", tft[Abs[value]]},
        {" + ", tft[value]}
      ],
      {value, rest}
    ]
  ]
];

(* vizuálne rozdelenie kladných a záporných diagonál determinantov 3x3 *)
cramer3x3VisualPanel[label_, matrix_] := Grid[{{
    labeledMatrixBlock[label, cramerMatrixCard[matrix]],
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
  Alignment -> {Center, Center, Center, Center},
  Spacings -> {1.5, 1}
];

(* vykreslí determinant 3×3 štandardným vzorcom *)
renderCramer3x3Det[matrix_, label_] := Module[{ content = {}, value, knownQ, knownMatrices, knownPos, knownData},
  value = Together[Det[matrix]];

  (* ak sa rovnaká 3x3 matica už počítala, nerozpisujeme Sarrusa znova *)
  knownQ = ValueQ[cramerKnown3x3] && ListQ[cramerKnown3x3];

  If[knownQ,
    knownMatrices = Lookup[cramerKnown3x3, "Matrix", {}];
    knownPos = FirstPosition[knownMatrices, matrix, Missing["NotFound"]];

    If[knownPos =!= Missing["NotFound"],
      knownData = cramerKnown3x3[[First[knownPos]]];

      AppendTo[content, "Tento determinant matice 3×3 sme už vypočítali vyššie, preto Sarrusovo pravidlo nemusíme znova rozpisovať."];
      AppendTo[content, resultEquationLine[cramerDetLabel[label], knownData["Value"]]];

      Return[
        <|
          "Content" -> content,
          "Value" -> knownData["Value"],
          "Matrix" -> matrix
        |>
      ];
    ];
  ];

  AppendTo[content, Row[{
    "Determinant matice 3×3 vypočítame pomocou ",
    Style["Sarrusovho pravidla", Bold],
    "."
  }]];

  AppendTo[content, cramer3x3VisualPanel[label, matrix]];
  AppendTo[content, Row[{cramerDetLabel[label], " = ", cramer3x3FormulaDisplay[matrix]}]];
  (*render 3x3 Det*)
  AppendTo[
    content,
    Module[{a, b, c, d, e, f, g, h, ii},
      {{a, b, c}, {d, e, f}, {g, h, ii}} = matrix;

      Row[{
        cramerDetLabel[label],
        " = ",
        cramerSignedValueSum[{a*e*ii, b*f*g, c*d*h, -c*e*g, -a*f*h, -b*d*ii}]
      }]
    ]
  ];

  AppendTo[content, resultEquationLine[cramerDetLabel[label], value]];

  If[knownQ,
    AppendTo[
      cramerKnown3x3,
      <|"Matrix" -> matrix, "Value" -> value|>
    ];
  ];

  <|"Content" -> content, "Value" -> value, "Matrix" -> matrix|>
];

(* nájde vhodnú laplaceovu line: najprv riadok, potom stĺpec *)
cramerSingletonLineData[matrix_] := Module[{ rowCounts, columnCounts, rowIndex, columnIndex, rowsInColumn},

(* najprv hľadáme riadok s práve jedným nenulovým prvkom *)
  rowCounts = Count[#, x_ /; x =!= 0] & /@ matrix;

  rowIndex = FirstCase[
    Range[Length[rowCounts]],
    i_ /; rowCounts[[i]] == 1,
    Missing["NotFound"]
  ];

  If[rowIndex =!= Missing["NotFound"],
    columnIndex = FirstCase[
      Range[Length[matrix[[rowIndex]]]],
      j_ /; matrix[[rowIndex, j]] =!= 0,
      Missing["NotFound"]
    ];

    If[columnIndex =!= Missing["NotFound"],
      Return[<|
        "Type" -> "Row",
        "LineIndex" -> rowIndex,
        "PivotRow" -> rowIndex,
        "PivotColumn" -> columnIndex
      |>]
    ];
  ];

  (* ak vhodný riadok neexistuje, hľadáme stĺpec s práve jedným nenulovým prvkom *)
  columnCounts = Count[#, x_ /; x =!= 0] & /@ Transpose[matrix];

  columnIndex = FirstCase[
    Range[Length[columnCounts]],
    j_ /; columnCounts[[j]] == 1,
    Missing["NotFound"]
  ];

  If[columnIndex =!= Missing["NotFound"],
    rowsInColumn = Select[
      Range[Length[matrix]],
      matrix[[#, columnIndex]] =!= 0 &
    ];

    If[Length[rowsInColumn] == 1, Return[<|"Type" -> "Column", "LineIndex" -> columnIndex, "PivotRow" -> First[rowsInColumn], "PivotColumn" -> columnIndex|>]];
  ];

  Missing["NotFound"]
];

(* vykreslí determinant 5×5 cez dva laplaceove rozvoje a následný determinant 3×3 *)
renderCramer5x5Reduction[matrix_, label_] := Module[{ content = {}, line1, line2, signed1, signed2, minor4, minor3, minor4Label, minor3Label, det3Data, det4Value, value},

  minor4Label = Subscript[Style["M", Italic], 4];
  minor3Label = Subscript[Style["M", Italic], 3];

  AppendTo[content, cramerLabeledMatrixGrid[label, matrix]];

  line1 = cramerSingletonLineData[matrix];
  If[line1 === Missing["NotFound"],
    value = Together[Det[matrix]];
    AppendTo[content, "Matica nemá vhodný riedky riadok ani stĺpec, preto determinant dopočítame priamo."];
    AppendTo[content, resultEquationLine[cramerDetLabel[label], value]];
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
    AppendTo[content, resultEquationLine[cramerDetLabel[minor4Label], det4Value]];

    addGap[content, 4];

    value = Together[signed1 det4Value];
    AppendTo[content, Row[{
      cramerDetLabel[label], " = ",
      cramerFactor[signed1], " \[CenterDot] ", cramerDetLabel[minor4Label],
      " = ",
      cramerFactor[signed1], " \[CenterDot] ", cramerFactor[det4Value],
      " = ",
      cramerFactor[value]
    }]];
    AppendTo[content, resultEquationLine[cramerDetLabel[label], value]];

    Return[<|"Content" -> content, "Value" -> value, "Matrix" -> matrix|>];
  ];

  line2 = cramerSingletonLineData[minor4];
  If[line2 === Missing["NotFound"],
    det4Value = Together[Det[minor4]];
    AppendTo[content, "Minor 4×4 už nemá vhodný riedky riadok ani stĺpec, preto jeho determinant dopočítame priamo."];
    AppendTo[content, resultEquationLine[cramerDetLabel[minor4Label], det4Value]];

    value = Together[signed1 det4Value];
    AppendTo[content, Row[{
      cramerDetLabel[label], " = ",
      cramerFactor[signed1], " \[CenterDot] ", cramerDetLabel[minor4Label],
      " = ",
      cramerFactor[signed1], " \[CenterDot] ", cramerFactor[det4Value],
      " = ",
      cramerFactor[value]
    }]];
    AppendTo[content, resultEquationLine[cramerDetLabel[label], value]];

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

  addGap[content, 4];

  det4Value = Together[signed2 det3Data["Value"]];

  AppendTo[content, Row[{
    cramerDetLabel[minor4Label], " = ",
    cramerFactor[signed2], " \[CenterDot] ", cramerDetLabel[minor3Label],
    " = ",
    cramerFactor[signed2], " \[CenterDot] ", cramerFactor[det3Data["Value"]],
    " = ",
    cramerFactor[det4Value]
  }]];
  AppendTo[content, resultEquationLine[cramerDetLabel[minor4Label], det4Value]];

  addGap[content, 4];

  value = Together[signed1 det4Value];

  AppendTo[content, Row[{
    cramerDetLabel[label], " = ",
    cramerFactor[signed1], " \[CenterDot] ", cramerDetLabel[minor4Label],
    " = ",
    cramerFactor[signed1], " \[CenterDot] ", cramerFactor[det4Value],
    " = ",
    cramerFactor[value]
  }]];
  AppendTo[content, resultEquationLine[cramerDetLabel[label], value]];

  <|"Content" -> content, "Value" -> value, "Matrix" -> matrix|>
];

(* výber vhodného spôsobu výpočtu determinantu podľa veľkosti matice *)
renderCramerDeterminant[matrix_, label_] := Switch[
  Length[matrix],
  3, renderCramer3x3Det[matrix, label],
  4, renderCramer4x4Reduction[matrix, label],
  5, renderCramer5x5Reduction[matrix, label],
  _, <|"Content" -> {cramerLabeledMatrixGrid[label, matrix], resultEquationLine[cramerDetLabel[label], Together[Det[matrix]]]},
    "Value" -> Together[Det[matrix]], "Matrix" -> matrix|>
];



(* --- Equations HELPERS --- *)

equationSolutionTypeMatchesQ[A_, rhs_, solType_String] := Module[{rankA, rankAug},
  rankA = MatrixRank[A];
  rankAug = MatrixRank[Join[A, Transpose[{rhs}], 2]];

  Switch[
    solType,
    "ONE", rankA === Length[A] && rankAug === rankA,
    "INFINITE", rankAug === rankA && rankA < Length[A],
    "NONE", rankAug > rankA,
    _, False
  ]
];

(* ~-~-~ VALIDATION ~-~-~ *)

(* formátovanie členov lineárneho výrazu so znamienkami *)
renderTermsRow[terms_List, mode_ : "Numeric", highlightVar_ : None] := Module[
  {pairs, out = {}, first = True, c, v, zeroQ, negQ, highlightedQ, t, varStyle},

  pairs = Select[terms, MatchQ[#, {_, _}] &];
  zeroQ = If[mode === "Symbolic", PossibleZeroQ, (# == 0 &)];
  negQ = If[mode === "Symbolic", (TrueQ[# < 0] &), (# < 0 &)];

  If[pairs === {} || AllTrue[pairs[[All, 1]], zeroQ], Return[tf[0]]];

  Do[
    {c, v} = pairs[[i]];

    If[zeroQ[c], Continue[]];

    highlightedQ = Which[
      highlightVar === None, False,
      ListQ[highlightVar], MemberQ[highlightVar, v],
      True, v === highlightVar
    ];

    varStyle = If[
      highlightedQ,
      If[
        ListQ[highlightVar],
        Style[tf[v], Bold],
        Style[tf[v], Bold, RGBColor[0.8, 0, 0]]
      ],
      tf[v]
    ];

    t = Which[
      v === None,
      tf[Abs[c]],

      highlightedQ && Abs[c] === 1,
      varStyle,

      highlightedQ,
      Row[{tf[Abs[c]], "", varStyle}],

      Abs[c] === 1,
      tf[v],

      True,
      tf[Abs[c] v]
    ];

    If[
      first,
      If[negQ[c], out = Join[out, {"-", t}], out = Join[out, {t}]];
      first = False,
      out = Join[out, {If[negQ[c], " - ", " + "], t}]
    ],
    {i, 1, Length[pairs]}
  ];

  If[out === {}, tf[0], Row[out]]
];

(* zarovnanie viacerých riadkov rovníc s poznámkami vpravo *)
alignedEquations[data_, breaks_List : {}, gap_ : 1.25] := Module[{eq = Style["=", 16], bar = Style["|", GrayLevel[.25]], n, rowGaps, stepRow, baseGap = 0.5, bigGap = gap},
  n = Length[data];
  rowGaps = ConstantArray[baseGap, Max[n, 1]];
  rowGaps[[1]] = 0;
  Do[If[IntegerQ[b] && 1 <= b <= n - 1, rowGaps[[b + 1]] = bigGap], {b, breaks}];

  stepRow[{lhs_, rhs_, note_}] := {lhs, eq, rhs, If[note === "" || note === None, "", Style[Row[{bar, Spacer[4], note}], GrayLevel[.6], FontSize -> 13]]};
  stepRow[{lhs_, rhs_}] := stepRow[{lhs, rhs, ""}];

  Grid[stepRow /@ data, Alignment -> {{Right, Center, Left, Left}}, Spacings -> {0.5, rowGaps}, BaseStyle -> {FontSize -> 14}]
];

wrapNegValue[val_] := Which[
  NumericQ[val] && val < 0, Row[{"(", tft[val], ")"}],
  Head[val] === Plus, Row[{"(", tft[val], ")"}],
  MatchQ[val, Times[c_?NumericQ, __] /; c < 0], Row[{"(", tft[val], ")"}],
  MatchQ[val, Times[-1, __]], Row[{"(", tft[val], ")"}],
  MatchQ[val, _Rational] && val < 0, Row[{"(", tft[val], ")"}],
  True, tft[val]
];

coeffVal[coeff_, val_] := Which[
  coeff === 0, 0,
  coeff === 1, wrapNegValue[val],
  coeff === -1, Row[{"-", wrapNegValue[val]}],
  NumberQ[val], Row[{tft[coeff], "\[CenterDot]", wrapNegValue[val]}],
  True, Row[{tft[coeff], wrapNegValue[val]}]
];

signBtwTerms[c_] := If[c < 0, " - ", " + "];

secondEquationStyle[] := RGBColor[0.05, 0.25, 0.85];

styledTermBody[coef_, symbol_, style_ : None] := Module[{body},
  body = If[
    symbol === None,
    tf[Abs[coef]],
    tf[If[Abs[coef] === 1, symbol, Abs[coef] symbol]]
  ];

  If[style === None, body, Style[body, style]]
];

signedPiecesRow[pieces_List] := Module[{out = {}, first = True, coef, body, negQ},
  Do[
    {coef, body} = pieces[[i]];

    If[PossibleZeroQ[coef], Continue[]];

    negQ = TrueQ[coef < 0];

    If[first,
      If[negQ, out = Join[out, {"-", body}], out = Join[out, {body}]];
      first = False;,
      out = Join[out, {If[negQ, " - ", " + "], body}]
    ],
    {i, 1, Length[pieces]}
  ];

  If[out === {}, tf[0], Row[out]]
];

additionExpandedEquationRow[rowMod_, rhsMod_, vars_, secondStyle_ : Automatic] := Module[
  {style, lhsPieces, rhsPieces},

  style = If[secondStyle === Automatic, secondEquationStyle[], secondStyle];

  lhsPieces = Join[
    DeleteCases[
      MapThread[
        If[PossibleZeroQ[#1], Nothing, {#1, styledTermBody[#1, #2]}] &,
        {rowMod[[1]], vars}
      ],
      Nothing
    ],
    DeleteCases[
      MapThread[
        If[PossibleZeroQ[#1], Nothing, {#1, styledTermBody[#1, #2, style]}] &,
        {rowMod[[2]], vars}
      ],
      Nothing
    ]
  ];

  rhsPieces = DeleteCases[
    {
      If[PossibleZeroQ[rhsMod[[1]]], Nothing, {rhsMod[[1]], tf[Abs[rhsMod[[1]]]]}],
      If[PossibleZeroQ[rhsMod[[2]]], Nothing, {rhsMod[[2]], Style[tf[Abs[rhsMod[[2]]]], style]}]
    },
    Nothing
  ];

  {signedPiecesRow[lhsPieces], signedPiecesRow[rhsPieces], ""}
];

equationRowsFromSystem[A_, b_, vars_] := Table[
  {renderTermsRow[Transpose[{A[[i]], vars}]], b[[i]], ""},
  {i, 1, Length[b]}
];

deduplicateEquationRows[rows_List] := Module[{out = {}, lastKey = None, key},
  Do[
    key = ToString[{rows[[i, 1]], rows[[i, 2]]}, InputForm];

    If[key === lastKey,
      If[Length[rows[[i]]] >= 3 && rows[[i, 3]] =!= "" && out =!= {},
        out[[-1, 3]] = rows[[i, 3]]
      ],
      AppendTo[out, rows[[i]]];
      lastKey = key
    ],
    {i, 1, Length[rows]}
  ];

  out
];

addNote[k_] := Which[
  PossibleZeroQ[k], "",
  TrueQ[k > 0], Row[{"+ ", tft[k]}],
  TrueQ[k < 0], Row[{"- ", tft[Abs[k]]}],
  MatchQ[k, Times[c_?NumericQ, __] /; c < 0], Row[{tft[k]}],
  MatchQ[k, Times[-1, __]], Row[{tft[k]}],
  MatchQ[k, Plus[c_, __]] && (TrueQ[c < 0] || (MatchQ[c, Times[n_, __]] && TrueQ[n < 0])), Row[{tft[k]}],
  True, Row[{"+ ", tft[k]}]
];

multNote[m_] := If[PossibleZeroQ[m - 1], "", Row[{"\[CenterDot]", " ", wrapNegValue[m]}]];
divNote[d_] := If[PossibleZeroQ[d - 1], "", Row[{":", " ", wrapNegValue[d]}]];

(* poznámka k dosadeniu už vypočítaných premenných *)
substNote[solMap_, remVars_, row_, vars_] := Module[{rowByVar, usedVars},
  rowByVar = AssociationThread[vars -> row];

  usedVars = Select[
    remVars,
    KeyExistsQ[solMap, #] && KeyExistsQ[rowByVar, #] && rowByVar[#] =!= 0 &
  ];

  If[
    usedVars === {},
    "",
    Row[
      Riffle[
        (
          Row[{
            #,
            " \[Rule] ",
            formatLinearExpr[
              Expand[Together[solMap[#]]],
              DeleteCases[vars, #]
            ]
          }]
        ) & /@ usedVars,
        ", "
      ]
    ]
  ]
];
(* kroky izolovania premennej z rovnice s jedným neznámym členom *)
isolateVarFromCoeffEqSteps[c_, var_, rhs_] := Module[{steps = {}, value},
  Which[
    PossibleZeroQ[c],
    <|"Type" -> "ZERO_COEFF", "Steps" -> steps, "Value" -> $Failed|>,

    c === 1, ( value = Together[rhs];
  AppendTo[steps, {tf[var], tf[value], ""}];
  <|"Type" -> "GENERAL", "Steps" -> steps, "Value" -> value|>
  ),
    c === -1, ( AppendTo[steps, {tf[-var], tf[Together[rhs]], multNote[-1]}];
  value = Together[-rhs];
  AppendTo[steps, {tf[var], tf[value], ""}];
  <|"Type" -> "GENERAL", "Steps" -> steps, "Value" -> value|>
  ),

    True, (AppendTo[steps, {tf[c var], tf[Together[rhs]], divNote[c]}];
  value = Together[rhs/c];
  AppendTo[steps, {tf[var], tf[value], ""}];
  <|"Type" -> "GENERAL", "Steps" -> steps, "Value" -> value|>
  )
  ]
];

(* ľavá strana rovnice po dosadení známych premenných *)
formatSubstLHS[row_, vars_, solMap_, unknownVar_, evalMode_ : False] := Module[
  {
    terms = {}, first = True, addTerm, emitKnownTerm, emitUnknownTerm,
    expr, exprVars, exprCoeffs, exprConst, exprTerms, hasVariableTermsQ,
    exprBody, prod
  },

  addTerm[content_, sign_] := (
    AppendTo[
      terms,
      If[
        first,
        If[sign === -1, Row[{"-", content}], content],
        Row[{If[sign === -1, " - ", " + "], content}]
      ]
    ];
    first = False
  );

  emitKnownTerm[c_, v_] := Module[{},
    If[!KeyExistsQ[solMap, v], Return[Null]];

    If[
      TrueQ[evalMode],
      prod = Together[c solMap[v]];
      If[!PossibleZeroQ[prod], addTerm[tf[Abs[prod]], Sign[prod]]],

      expr = Expand[Together[solMap[v]]];
      exprVars = DeleteCases[vars, v];

      {exprCoeffs, exprConst} = linearDecompose[expr, exprVars];

      exprTerms = Join[
        DeleteCases[
          Table[
            If[
              PossibleZeroQ[exprCoeffs[[k]]],
              Nothing,
              {exprCoeffs[[k]], exprVars[[k]]}
            ],
            {k, 1, Length[exprVars]}
          ],
          Nothing
        ],
        If[PossibleZeroQ[exprConst], {}, {{exprConst, None}}]
      ];

      hasVariableTermsQ = AnyTrue[exprTerms, #[[2]] =!= None &];

      exprBody = If[
        hasVariableTermsQ,
        Row[{"(", renderTermsRow[exprTerms, "Symbolic", exprVars], ")"}],
        Style[Row[{"(", renderTermsRow[exprTerms, "Symbolic"], ")"}], Bold]
      ];

      addTerm[
        If[
          Abs[c] === 1,
          exprBody,
          Row[{tf[Abs[c]], "\[CenterDot]", exprBody}]
        ],
        Sign[c]
      ]
    ]
  ];

  emitUnknownTerm[c_, v_] := addTerm[tf[If[Abs[c] === 1, v, Abs[c] v]], Sign[c]];

  Do[
    With[{c = row[[i]], v = vars[[i]]},
      If[
        c =!= 0,
        If[v === unknownVar, emitUnknownTerm[c, v], emitKnownTerm[c, v]]
      ]
    ],
    {i, 1, Length[vars]}
  ];

  If[terms === {}, tf[0], Row[terms]]
];

verificationStepsEquation[A_, b_, vars_, sol_] := Module[
  {
    content = {}, solN = Together /@ sol, lhs, prodRow, sumRow,
    prodParts, sumParts, firstProd, firstSum, addProd, addSum, okQ
  },

  Do[
    prodParts = {};
    sumParts = {};
    firstProd = True;
    firstSum = True;

    addProd[coef_, val_] := Module[{body},
      If[PossibleZeroQ[coef], Return[Null]];

      body = Row[{tf[Abs[coef]], "\[CenterDot]", Style[wrapNegValue[val], Bold]}];

      If[firstProd,
        prodParts = Join[prodParts, If[TrueQ[coef < 0], {"-", body}, {body}]];
        firstProd = False,
        prodParts = Join[prodParts, {If[TrueQ[coef < 0], " - ", " + "], body}]
      ];
    ];

    addSum[term_] := Module[{body},
      If[PossibleZeroQ[term], Return[Null]];

      body = tf[Abs[term]];

      If[firstSum,
        sumParts = Join[sumParts, If[TrueQ[term < 0], {"-", body}, {body}]];
        firstSum = False,
        sumParts = Join[sumParts, {If[TrueQ[term < 0], " - ", " + "], body}]
      ];
    ];

    Do[
      addProd[A[[i, j]], solN[[j]]];
      addSum[Together[A[[i, j]] solN[[j]]]],
      {j, 1, Length[solN]}
    ];

    prodRow = If[prodParts === {}, tf[0], Row[prodParts]];
    sumRow = If[sumParts === {}, tf[0], Row[sumParts]];

    lhs = Together[A[[i]] . solN];
    okQ = PossibleZeroQ[Together[lhs - b[[i]]]];

    AppendTo[
      content,
      Grid[
        {
          {Row[{"LS", i, " = ", prodRow, " = ", sumRow, " = ", Style[tft[lhs], Bold]}]},
          {Row[{"PS", i, " = ", Style[tft[b[[i]]], Bold]}]},
          {
            If[okQ,
              Style[Row[{"LS", i, " = PS", i, " (OK)"}], Darker[Green]],
              Style[Row[{"LS", i, " \[NotEqual] PS", i, " (CHYBA)"}], Red]
            ]
          }
        },
        Alignment -> Left,
        Spacings -> {0, 0.4},
        BaseStyle -> {FontSize -> 13}
      ]
    ],
    {i, Length[b]}
  ];

  content
];

(* výber jednoduchej rovnice a voľnej premennej pre parametrické riešenie *)
chooseInfiniteEquationRelation[eqs_List, varsNow_List] := Module[
  {paramSymbol = \[FormalT], candidates = {}, normalized, row, rhs, solveIdx, paramIdx,
    exprWithVars, exprWithParam, coeffParam, constParam, denScore, score},

  Do[
    normalized = normalizeEquationRow[Join[eq[[1]], {eq[[2]]}]];
    row = Most[normalized];
    rhs = Last[normalized];

    If[equationClass[row, rhs] =!= "NORMAL", Continue[]];

    Do[
      If[PossibleZeroQ[row[[solveIdx]]], Continue[]];

      Do[
        If[paramIdx === solveIdx, Continue[]];

        exprWithVars = Together[
          (rhs - Total@Table[
            If[j === solveIdx, 0, row[[j]] varsNow[[j]]],
            {j, 1, Length[varsNow]}
          ]) / row[[solveIdx]]
        ];

        If[
          !FreeQ[
            exprWithVars,
            Alternatives @@ Delete[varsNow, {{solveIdx}, {paramIdx}}]
          ],
          Continue[]
        ];

        exprWithParam = Together[exprWithVars /. varsNow[[paramIdx]] -> paramSymbol];

        coeffParam = Together[Coefficient[exprWithParam, paramSymbol]];
        constParam = Together[exprWithParam /. paramSymbol -> 0];
        denScore = Max[Denominator /@ {coeffParam, constParam}];

        score = {
          denScore,
          LeafCount[exprWithParam],
          Total[Abs[row]],
          Abs[row[[solveIdx]]]
        };

        AppendTo[
          candidates,
          <|
            "Row" -> row,
            "RHS" -> rhs,
            "SolveIndex" -> solveIdx,
            "SolveVar" -> varsNow[[solveIdx]],
            "ParamIndex" -> paramIdx,
            "ParamVar" -> varsNow[[paramIdx]],
            "ParamSymbol" -> paramSymbol,
            "ExprWithVars" -> exprWithVars,
            "ExprWithParam" -> exprWithParam,
            "Score" -> score
          |>
        ],
        {paramIdx, 1, Length[varsNow]}
      ],
      {solveIdx, 1, Length[varsNow]}
    ],
    {eq, eqs}
  ];

  If[candidates === {}, $Failed, First @ MinimalBy[candidates, #Score &]]
];

SetAttributes[appendEquationInfiniteParametrization, HoldFirst];

(* doplnenie krokov parametrizácie pre nekonečne veľa riešení *)
appendEquationInfiniteParametrization[content_, config_Association, A_, b_, origVars_, eqs_, varsNow_, stack_] := Module[
  {relation, solMap, solExprs, paramVar, paramSymbol, solveVar, row, rhs, solveIdx,
    unknownCoeff, knownSum, rhsAfterMove, valueExpanded, valueExpandedDisplay,
    valueFactored, valueFinal, substLHS, noteShift, equationRows,
    item, back, backVar},

  relation = chooseInfiniteEquationRelation[eqs, varsNow];

  If[relation === $Failed,
    Return[$Failed]
  ];

  paramVar = relation["ParamVar"];
  paramSymbol = relation["ParamSymbol"];
  solveVar = relation["SolveVar"];

  row = relation["Row"];
  rhs = relation["RHS"];
  solveIdx = relation["SolveIndex"];
  unknownCoeff = row[[solveIdx]];

  appendStepHeader[content, "Vyjadrenie riešenia pomocou parametra"];

  AppendTo[
    content,
    "Keďže sústava má nekonečne veľa riešení, jednu neznámu zvolíme ako parameter. Vyberieme takú voľnú premennú, aby pri vyjadrení nevznikali zbytočné zlomky."
  ];

  AppendTo[
    content,
    "Použijeme jednoduchú rovnicu, ktorá vznikla v redukovanej sústave:"
  ];

  AppendTo[
    content,
    alignedEquations[
      {{
        renderTermsRow[Transpose[{row, varsNow}]],
        rhs,
        ""
      }}
    ]
  ];

  AppendTo[
    content,
    Row[{
      "Voľnú premennú zvolíme ",
      tf[paramVar],
      " = ",
      TraditionalForm[paramSymbol],
      "."
    }]
  ];

  AppendTo[
    content,
    highlightResultEquation[paramVar, paramSymbol]
  ];

  (* znovu prepíšeme rovnicu už s parametrom *)
  substLHS = formatSubstLHS[
    row,
    varsNow,
    <|paramVar -> paramSymbol|>,
    solveVar,
    False
  ];

  knownSum = Together @ Total @ Table[
    If[
      j === solveIdx,
      0,
      row[[j]] * If[varsNow[[j]] === paramVar, paramSymbol, varsNow[[j]]]
    ],
    {j, 1, Length[varsNow]}
  ];

  noteShift = addNote[-knownSum];

  (* najprv necháme rozpísaný tvar, nezlučujeme ho cez Together *)
  rhsAfterMove = Expand[rhs - knownSum];
  valueExpanded = Expand[rhsAfterMove / unknownCoeff];

  valueExpandedDisplay = renderTermsRow[
    DeleteCases[
      {
        {Expand[valueExpanded /. paramSymbol -> 0], None},
        {Expand[Coefficient[valueExpanded, paramSymbol]], paramSymbol}
      },
      {coef_ /; PossibleZeroQ[coef], _}
    ],
    "Symbolic"
  ];

  (* až tu robíme pekný faktorizovaný výsledok *)
  valueFactored = Factor[Together[valueExpanded]];

  valueFinal = If[
    valueFactored =!= valueExpanded && LeafCount[valueFactored] <= LeafCount[valueExpanded],
    valueFactored,
    valueExpanded
  ];

  equationRows = {
    {substLHS, rhs, noteShift}
  };

  equationRows = Join[
    equationRows,
    Which[
      unknownCoeff === 1,
      {
        {tf[solveVar], valueExpandedDisplay, ""}
      },

      unknownCoeff === -1,
      {
        {tf[-solveVar], tf[rhsAfterMove], multNote[-1]},
        {tf[solveVar], valueExpandedDisplay, ""}
      },

      True,
      {
        {tf[unknownCoeff solveVar], tf[rhsAfterMove], divNote[unknownCoeff]},
        {tf[solveVar], valueExpandedDisplay, ""}
      }
    ]
  ];

  If[valueFinal =!= valueExpanded,
    AppendTo[equationRows, {tf[solveVar], tf[valueFinal], ""}]
  ];

  AppendTo[
    content,
    alignedEquations[equationRows]
  ];

  AppendTo[
    content,
    highlightResultEquation[solveVar, valueFinal]
  ];

  solMap = <|
    paramVar -> paramSymbol,
    solveVar -> valueFinal
  |>;

  Do[
    item = stack[[k]];
    backVar = config["BackVarFn"][item];

    AppendTo[
      content,
      Style[
        "Dopočítame premennú " <> ToString[backVar] <> " dosadením do vhodnej rovnice:"
        Italic
      ]
    ];

    back = config["BackFn"][item, solMap];

    If[Lookup[back, "Type", "ONE"] =!= "ONE",
      Return[$Failed]
    ];

    AppendTo[content, alignedEquations[back["Steps"]]];

    AppendTo[
      content,
      highlightResultEquation[backVar, back["Value"]]
    ];

    solMap[backVar] = back["Value"],
    {k, Length[stack], 1, -1}
  ];

  solExprs = Together /@ (solMap /@ origVars);

  <|
    "Content" -> content,
    "Solution" -> solExprs
  |>
];

SetAttributes[appendEquationVerification, HoldFirst];

appendEquationVerification[content_, A_, b_, vars_, result_] := Module[{items},

  If[result === "NONE",
    Return[Null]
  ];

  appendStepHeader[content, "Skúška správnosti"];

  If[!ListQ[result],
    Return[$Failed]
  ];

  AppendTo[
    content,
    "Správnosť overíme dosadením nájdeného riešenia do sústavy rovníc a porovnáme ľavú a pravú stranu v každom riadku:"
  ];

  items = verificationStepsEquation[A, b, vars, result];

  Scan[AppendTo[content, #] &, items]
];

(* ~-~-~ HARD DISPLAY HELPERS ~-~-~ *)

scaleTerms[terms_List, k_Integer] := ({k #[[1]], #[[2]]} & /@ terms);

(* vytvorenie zámerne zložitejšieho tvaru jednej rovnice *)
buildHardEq[row_, rhs_, vars_] := Module[{n = Length[vars], idxMove, cLeftPool, cLeft, varTerms, kept, moved, leftBase, rightBase},
  idxMove = RandomSample[Range[n], RandomChoice[Range[1, n - 1]]];
  cLeftPool = DeleteCases[Range[-7, 7], 0];
  cLeft = RandomChoice[cLeftPool];
  varTerms = MapThread[List, {row, vars}];
  kept = varTerms[[Complement[Range[n], idxMove]]];
  moved = ({-#[[1]], #[[2]]} & /@ varTerms[[idxMove]]);
  leftBase = RandomSample @ Join[kept, {{cLeft, None}}];
  rightBase = RandomSample @ Join[moved, {{rhs + cLeft, None}}];
  <|"MoveIdx" -> idxMove, "CLeft" -> cLeft, "LeftBaseTerms" -> leftBase, "RightBaseTerms" -> rightBase|>
];

pickHardMultipliers15[n_] := Module[{ks},
  ks = RandomChoice[Range[5], n];
  While[Max[ks] < 4, ks = RandomChoice[Range[5], n]];
  ks
];

(* príprava hard zadania v nenormalizovanom tvare *)
buildHardDisplay[data_Association, vars_] := Module[{A = data["A"], b = data["b"], n = Length[vars], ks, eqDisp = {}, leftBaseAll = {}, rightBaseAll = {}, m, lMult, rMult},
  ks = pickHardMultipliers15[n];

  Do[
    m = buildHardEq[A[[i]], b[[i]], vars];
    lMult = scaleTerms[m["LeftBaseTerms"], ks[[i]]];
    rMult = scaleTerms[m["RightBaseTerms"], ks[[i]]];

    AppendTo[leftBaseAll, m["LeftBaseTerms"]];
    AppendTo[rightBaseAll, m["RightBaseTerms"]];
    AppendTo[eqDisp, {renderTermsRow[lMult], renderTermsRow[rMult], ""}],
    {i, 1, n}
  ];

  Join[data, <|
    "HardQ" -> True,
    "Multipliers" -> ks,
    "EqDisplay" -> eqDisp,
    "HardLeftBaseTerms" -> leftBaseAll,
    "HardRightBaseTerms" -> rightBaseAll
  |>]
];

zeroCoeff3[A_] := Module[{mask, zeroRowsByCol, zeroColsByRow},
  mask = Map[# == 0 &, A, {2}];
  zeroRowsByCol = Table[Flatten @ Position[mask[[All, j]], True], {j, 1, 3}];
  zeroColsByRow = Table[Flatten @ Position[mask[[i]], True], {i, 1, 3}];
  <|"Mask" -> mask, "ZeroRowsByCol" -> zeroRowsByCol, "ZeroColsByRow" -> zeroColsByRow|>
];

(* normalizačné kroky pre hard sústavu 3x3 *)
hardNormalizationSteps3[A_, b_, vars_, data_Association] := Module[{content = {}, ks, leftBase, rightBase, k, lMult, rMult, coeffL, coeffR, constL, constR, addTerms, addNoteFromTerms,
  addNoteLocal, rowStd, rhsStd, rowsStd, rhsStdAll, gcds, rowDiv, rhsDiv, rowsFinal, rhsFinal, anyDivQ},

  termsToCoeffsConst[terms_List] := Module[{cVar, cConst},
    cVar = Table[Total @ Cases[terms, {cc_, vars[[j]]} :> cc], {j, 1, Length[vars]}];
    cConst = Total @ Cases[terms, {cc_, None} :> cc];
    {cVar, cConst}
  ];

  addNoteFromTerms[terms_List] := Module[{pairs, pieces},
    pairs = Select[terms, MatchQ[#, {_, _}] && #[[1]] =!= 0 &];
    If[pairs === {}, Return[""]];
    pieces = Table[
      With[{cc = pairs[[j, 1]], sym = pairs[[j, 2]]},
        Row[{If[cc >= 0, "+ ", "- "], If[sym === None, tf[Abs[cc]], tf[If[Abs[cc] === 1, sym, Abs[cc] sym]]]}]
      ],
      {j, 1, Length[pairs]}
    ];
    Row @ Riffle[pieces, " "]
  ];

  ks = data["Multipliers"];
  leftBase = data["HardLeftBaseTerms"];
  rightBase = data["HardRightBaseTerms"];

  appendStepHeader[content, "Normalizácia"];
  AppendTo[content, "V každej rovnici presunieme všetky členy s neznámymi na ľavú stranu a všetky konštanty na pravú stranu."];

  rowsStd = ConstantArray[0, {3, 3}];
  rhsStdAll = ConstantArray[0, 3];

  AppendTo[content,
    alignedEquations @ Table[
      k = ks[[i]];
      lMult = scaleTerms[leftBase[[i]], k];
      rMult = scaleTerms[rightBase[[i]], k];

      {coeffL, constL} = termsToCoeffsConst[lMult];
      {coeffR, constR} = termsToCoeffsConst[rMult];

      addTerms = Join[
        If[constL =!= 0, {{-constL, None}}, {}],
        Join @@ Table[If[coeffR[[j]] =!= 0, {{-coeffR[[j]], vars[[j]]}}, {}], {j, 1, 3}]
      ];
      addNoteLocal = addNoteFromTerms[addTerms];

      rowStd = coeffL - coeffR;
      rhsStd = constR - constL;

      rowsStd[[i]] = rowStd;
      rhsStdAll[[i]] = rhsStd;

      {renderTermsRow[lMult], renderTermsRow[rMult], addNoteLocal},
      {i, 1, 3}
    ]
  ];

  AppendTo[content, "Po úprave dostaneme:"];
  AppendTo[content, alignedEquations @ Table[{renderTermsRow[Transpose[{rowsStd[[i]], vars}]], rhsStdAll[[i]], ""}, {i, 1, 3}]];

  gcds = Table[GCD @@ Abs @ Join[rowsStd[[i]], {rhsStdAll[[i]]}], {i, 1, 3}];
  anyDivQ = AnyTrue[gcds, # > 1 &];

  rowsFinal = rowsStd;
  rhsFinal = rhsStdAll;

  If[anyDivQ,
    AppendTo[content, "Ak majú všetky koeficienty v rovnici spoločný deliteľ väčší ako 1, rovnicu vydelíme týmto číslom, aby sme dostali jednoduchší tvar."];
    Do[
      If[gcds[[i]] > 1,
        rowDiv = rowsFinal[[i]]/gcds[[i]];
        rhsDiv = rhsFinal[[i]]/gcds[[i]];
        AppendTo[content, alignedEquations[{{renderTermsRow[Transpose[{rowsFinal[[i]], vars}]], rhsFinal[[i]], divNote[gcds[[i]]]}}]];
        rowsFinal[[i]] = rowDiv;
        rhsFinal[[i]] = rhsDiv;
      ],
      {i, 1, 3}
    ];
    AppendTo[content, "Po úpravách dostaneme sústavu rovníc v štandardnom tvare, pripravenú na riešenie:"];
    AppendTo[content, alignedEquations @ Table[{renderTermsRow[Transpose[{rowsFinal[[i]], vars}]], rhsFinal[[i]], ""}, {i, 1, 3}]];
  ];

  content
];

(* ~-~-~ INFINITE SOLUTIONS & PARAMETRIZATION ~-~-~ *)

(* voľba parametrizácie s čo najmenšími menovateľmi *)
chooseParametrization[A_, b_, vars_] := Module[{eqs, candidates, try, results},
  eqs = Thread[A . vars == b];
  candidates = List /@ vars;
  try[{v_}] := Module[{sol, rules, exprs, dens},
    sol = Quiet @ Solve[eqs /. v -> \[FormalT], Complement[vars, {v}], Reals];
    If[sol === {} || sol === $Failed, Return[Nothing]];
    rules = Join[{v -> \[FormalT]}, sol[[1]]];
    exprs = Together /@ (vars /. rules);
    dens = Denominator /@ Rationalize[exprs, 0];
    <|"Var" -> v, "Exprs" -> exprs, "Score" -> Max[dens]|>
  ];
  results = try /@ candidates;
  If[results === {}, $Failed, First @ MinimalBy[results, #Score &]]
];

(* ~-~-~ SYSTEM GENERATION ~-~-~ *)

$EquationMaxRetryCount = 300;

abFromAug[aug_] := Module[{nCols},
  nCols = Length[First[aug]];
  <|"A" -> aug[[All, 1 ;; nCols - 1]], "b" -> aug[[All, nCols]]|>
];

normalizeEquationRow[row_List] := Module[{g, first},
  g = rowAbsGCD[row];
  first = FirstCase[Most[row], x_ /; x =!= 0, 1];
  If[g > 1, row/g, If[first === -1, -row, row]]
];

validEquationDataQ[data_Association, diff_String] := Module[
  {aug, n, coefficientRows, noZeroEquationRowsQ, noZeroCoefficientsQ},

  If[data === $Failed, Return[False]];

  aug = Lookup[data, "Aug", augFromAb[data["A"], data["b"]]];
  n = Length[First[aug]] - 1;

  coefficientRows = aug[[All, 1 ;; n]];

  (* riadok nesmie byť celý nulový *)
  noZeroEquationRowsQ = AllTrue[
    coefficientRows,
    !AllTrue[#, PossibleZeroQ] &
  ];

  (* HARD 3x3: v zadaní nechceme nulové koeficienty *)
  noZeroCoefficientsQ = If[
    diff === "HARD" && n === 3,
    AllTrue[Flatten[coefficientRows], !PossibleZeroQ[#] &],
    True
  ];

  matrixMaxAbs[aug] <= $MaxBounds &&
      noZeroEquationRowsQ &&
      noZeroCoefficientsQ
];

addHardDisplayIfNeeded[data_Association, dim_Integer, diff_String, vars_List] := Module[{out = data},
  If[diff === "HARD" && dim === 3,
    out = buildHardDisplay[out, vars]
  ];
  out
];

generateEquationDataWithBounds[dim_Integer, diff_String, solType_String, vars_List, scrambleFn_] := Module[
  {baseData, data, retries = 0},

  While[retries < $EquationMaxRetryCount,
    baseData = makeDiagonalEquationSystem[dim, solType];

    If[baseData === $Failed,
      retries++;
      Continue[]
    ];

    data = scrambleFn[diff, baseData];

    If[TrueQ[validEquationDataQ[data, diff]],
      Return[addHardDisplayIfNeeded[data, dim, diff, vars]]
    ];

    retries++;
  ];

  $Failed
];


(* ~-~-~ VISUALIZATION HELPERS ~-~-~ *)

(* typ spoločného prieniku troch rovín *)
systemIntersection3[A_, b_, vars_] := Module[{rA = MatrixRank[A], rAb = MatrixRank[Join[A, Transpose[{b}], 2]], ns},
  If[rAb > rA, <|"Type" -> "NONE"|>,
    If[rA == 3, <|"Type" -> "POINT", "Point" -> LinearSolve[A, b]|>,
      ns = NullSpace[A];
      If[Length[ns] == 1, <|"Type" -> "LINE", "Point" -> (vars /. First @ FindInstance[A . vars == b, vars, Reals]), "Dir" -> ns[[1]]|>,
        If[Length[ns] >= 2, <|"Type" -> "PLANE"|>, <|"Type" -> "INFINITE"|>]
      ]
    ]
  ]
];

(* graf pre 2D a 3D *)
visualize2[A_, b_, vars_, sol_] := Module[{x, y, pt, xrange, yrange, seg, center, subtitle, range = 10, lineStyles, lineLabels, extraLegStyles, extraLegLabels, legend},
  {x, y} = vars;

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
    subtitle = If[sol === "NONE", "Priamky sú rovnobežné, nepretínajú sa - sústava nemá riešenie.", "Priamky sú totožné (prekrývajú sa) - sústava má nekonečne veľa riešení."]
  ];

  seg[row_, rhs_] := With[{a = row[[1]], bb = row[[2]]},
    If[bb =!= 0, InfiniteLine[{{0, rhs/bb}, {1, (rhs - a)/bb}}], InfiniteLine[{{rhs/a, 0}, {rhs/a, 1}}]]
  ];

  printCellStyle[BoxData @ ToBoxes[subtitle, StandardForm], "Text"];

  lineStyles = If[sol === "INFINITE",
    {Directive[Magenta, AbsoluteThickness[2], Opacity[0.9]], Directive[Blue, AbsoluteThickness[2], Opacity[0.9], Dashing[0.05]]},
    {Directive[Magenta, Thick], Directive[Blue, Thick]}
  ];

  lineLabels = {tf[A[[1, 1]] x + A[[1, 2]] y == b[[1]]], tf[A[[2, 1]] x + A[[2, 2]] y == b[[2]]]};

  {extraLegStyles, extraLegLabels} =
      If[pt =!= None, {{Directive[Black]}, {Row[{"prienik: [", TraditionalForm @ Together[pt[[1]]], ", ", TraditionalForm @ Together[pt[[2]]], "]"}]}},
        {{}, {}}
      ];

  legend = LineLegend[
    Join[lineStyles, extraLegStyles],
    Join[lineLabels, extraLegLabels],
    LegendMarkerSize -> {50, 20},
    LegendMarkers -> Join[{None, None}, If[pt =!= None, {Graphics[{Black, Disk[]}, ImageSize -> 10]}, {}]]
  ];

  printFormulaCell @ Legended[
    Graphics[
      {{lineStyles[[1]], seg[A[[1]], b[[1]]]}, {lineStyles[[2]], seg[A[[2]], b[[2]]]}, If[pt =!= None, {{Black, Thick, Circle[pt, 0.4]}, {Green, PointSize[0.02], Point[pt]}}, {}]},
      PlotRange -> {xrange, yrange}, Axes -> True, GridLines -> Automatic, ImageSize -> Medium, PlotRangeClipping -> True
    ],
    legend
  ]
];
visualize3[A_, b_, vars_, sol_] := Module[{x, y, z, range = 15, xmin, xmax, ymin, ymax, zmin, zmax, n1, n2, n3, d1, d2, d3, inter, best, subtitle, planes, mark, plot, eqLbl, planeStyles, planeLabels, extraLegStyles, extraLegLabels, legend},
  {x, y, z} = vars;
  {xmin, xmax} = {-range, range}; {ymin, ymax} = {-range, range}; {zmin, zmax} = {-range, range};

  n1 = N @ A[[1]]; d1 = N @ b[[1]];
  n2 = N @ A[[2]]; d2 = N @ b[[2]];
  n3 = N @ A[[3]]; d3 = N @ b[[3]];

  inter = systemIntersection3[A, b, vars];
  best = If[inter["Type"] === "LINE", chooseParametrization[A, b, vars], $Failed];

  subtitle = Switch[inter["Type"],
    "POINT", "Tri roviny majú spoločný prienik v jednom bode (riešenie sústavy).",
    "LINE", "Tri roviny majú spoločný prienik - priamku (nekonečne veľa riešení).",
    "PLANE", "Všetky tri rovnice opisujú tú istú rovinu (nekonečne veľa riešení).",
    "NONE", "Roviny nemajú spoločný prienik všetkých troch naraz (sústava nemá riešenie).",
    _, "Prienik sa nepodarilo jednoznačne určiť."
  ];

  printCellStyle[BoxData @ ToBoxes[subtitle, StandardForm], "Text"];

  eqLbl[row_, rhs_] := tf[row . {x, y, z} == rhs];
  planeLabels = {eqLbl[A[[1]], b[[1]]], eqLbl[A[[2]], b[[2]]], eqLbl[A[[3]], b[[3]]]};
  planeStyles = {Cyan, Magenta, Yellow};

  planes = ContourPlot3D[
    {n1 . {x, y, z} == d1, n2 . {x, y, z} == d2, n3 . {x, y, z} == d3},
    {x, xmin, xmax}, {y, ymin, ymax}, {z, zmin, zmax},
    Mesh -> None, PlotPoints -> 25, PerformanceGoal -> "Speed",
    ContourStyle -> {Directive[Cyan, Opacity[0.4]], Directive[Magenta, Opacity[0.4]], Directive[Yellow, Opacity[0.4]]},
    BoundaryStyle -> None
  ];

  mark = Graphics3D @ Switch[inter["Type"],
    "POINT", {Black, PointSize[0.03], Point[N @ inter["Point"]], Black, Sphere[N @ inter["Point"], 0.35]},
    "LINE", Module[{p0, v}, p0 = N @ inter["Point"]; v = 20 N @ inter["Dir"]; {Black, Specularity[White, 20], Tube[{p0 - v, p0 + v}, 0.18]}],
    _, {}
  ];

  plot = Show[
    planes, mark,
    PlotRange -> {{xmin, xmax}, {ymin, ymax}, {zmin, zmax}},
    BoxRatios -> {1, 1, 1},
    Axes -> True, AxesLabel -> {"x", "y", "z"},
    SphericalRegion -> True, ImageSize -> 400,
    Lighting -> "Neutral",
    ViewAngle -> 35 Degree,
    ViewPoint -> {2.2, -2.0, 1.4},
    Method -> {"MouseInteraction" -> {"Rotate" -> True, "Pan" -> False, "Zoom" -> False}}
  ];

  {extraLegStyles, extraLegLabels} = Switch[inter["Type"],
    "POINT", {{Black}, {Row[{"prienik: [", Sequence @@ Riffle[TraditionalForm /@ inter["Point"], ", "], "]"}]}},
    "LINE", {{Black}, {Row[{"priesečník: ", TraditionalForm @ best["Exprs"], ", ", tf[\[FormalT]], "\[Element]", "\[DoubleStruckR]"}]}},
    _, {{}, {}}
  ];

  legend = If[extraLegStyles === {}, SwatchLegend[planeStyles, planeLabels], SwatchLegend[Join[planeStyles, extraLegStyles], Join[planeLabels, extraLegLabels]]];
  printFormulaCell @ Legended[plot, legend];
];

(* ~-~-~ ELIMINATION HELPERS ~-~-~ *)

equationClass[row_List, rhs_] := Which[
  AllTrue[row, PossibleZeroQ] && PossibleZeroQ[rhs], "IDENTITY",
  AllTrue[row, PossibleZeroQ] && !PossibleZeroQ[rhs], "CONTRADICTION",
  True, "NORMAL"
];

solveOneVarEquationSteps[{row_List, rhs_}, {var_}] := Module[{c = row[[1]], cls, steps = {}, value},
  cls = equationClass[row, rhs];
  If[cls === "CONTRADICTION", Return[<|"Type" -> "NONE", "Content" -> {{tf[0], tf[rhs], ""}}|>]];
  If[cls === "IDENTITY", Return[<|"Type" -> "INFINITE", "Content" -> {{tf[0], tf[0], ""}}|>]];

  Module[{iso},
    iso = isolateVarFromCoeffEqSteps[c, var, rhs];
    steps = Join[steps, iso["Steps"]];
    <|"Type" -> "ONE", "Value" -> iso["Value"], "Content" -> steps|>
  ]
];

(* spätné dopočítanie jednej premennej z pivotnej rovnice *)
backSubstituteVariableSteps[{row_List, rhs_}, vars_List, solMap_Association, solvedVar_] := Module[
  {steps = {}, currentLHS, substLHS, expandedTerms = {}, combinedTerms, combinedLHS,
    unknownCoeff, knownTerms, knownSum, rhsShift, noteShift, valueExpanded, valueFactored, valueFinal,
    formalParams, displayVars, c, v, expr, summands, termVar, termCoeff, keyOrder = {}, coeffByKey = <||>,
    addCombinedTerm, renderMovedRHS, moveTerms, movedRHS, finalRows, rowKey, lastKey = None, dedupRows = {},
showSubstitutionRowQ = False},

(* pôvodná rovnica a dosadenie *)
  currentLHS = renderTermsRow[Transpose[{row, vars}], "Numeric", Keys[solMap]];
  substLHS = formatSubstLHS[row, vars, solMap, solvedVar, False];

  formalParams = DeleteDuplicates @ Cases[
    Values[solMap],
    Alternatives @@ {\[FormalT], \[FormalS], \[FormalR]},
    Infinity
  ];

  displayVars = DeleteDuplicates@Join[formalParams, vars];

  (* roznásobenie po dosadení *)
  Do[
    c = row[[i]];
    v = vars[[i]];

    If[PossibleZeroQ[c], Continue[]];

    If[v === solvedVar,
      AppendTo[expandedTerms, {c, v}],

      If[KeyExistsQ[solMap, v],

      (* pri dosadení najprv zobrazíme nahradený výraz, až potom ho upravíme *)
        If[
          Abs[c] =!= 1 || !TrueQ[Expand[c solMap[v]] === c solMap[v]],
          showSubstitutionRowQ = True
        ];

        expr = Expand[c solMap[v]];
        summands = If[Head[expr] === Plus, List @@ expr, {expr}];

        Do[
          termVar = SelectFirst[displayVars, !FreeQ[term, #] &, None];

          termCoeff = If[
            termVar === None,
            Together[term],
            Together[term/termVar]
          ];

          AppendTo[expandedTerms, {termCoeff, termVar}],
          {term, summands}
        ],

        AppendTo[expandedTerms, {c, v}]
      ]
    ],
    {i, 1, Length[vars]}
  ];

  (* sčítanie podobných členov *)
  addCombinedTerm[{coef_, symbol_}] := (
    If[!KeyExistsQ[coeffByKey, symbol],
      AppendTo[keyOrder, symbol];
      coeffByKey[symbol] = 0
    ];

    coeffByKey[symbol] = Together[coeffByKey[symbol] + coef]
  );

  Scan[addCombinedTerm, expandedTerms];

  combinedTerms = Select[
    Table[{Together[coeffByKey[key]], key}, {key, keyOrder}],
    !PossibleZeroQ[#[[1]]] &
  ];

  combinedLHS = renderTermsRow[combinedTerms, "Symbolic"];

  unknownCoeff = Total @ Cases[
    combinedTerms,
    {coef_, symbol_} /; symbol === solvedVar :> coef
  ];

  knownTerms = Select[combinedTerms, #[[2]] =!= solvedVar &];

  knownSum = Total @ Map[
    If[#[[2]] === None, #[[1]], #[[1]] #[[2]]] &,
    knownTerms
  ];

  If[PossibleZeroQ[unknownCoeff],
    Return[
      <|
        "Type" -> If[PossibleZeroQ[rhs - knownSum], "INFINITE", "NONE"],
        "Steps" -> steps
      |>
    ]
  ];

  noteShift = addNote[-knownSum];

  (* najprv necháme rozpísanú pravú stranu *)
  rhsShift = Expand[rhs - knownSum];

  (* pravá strana po presunutí známych členov *)
  moveTerms = {-#[[1]], #[[2]]} & /@ knownTerms;

  renderMovedRHS[] := Row @ Flatten@Join[
    {tf[rhs]},
    Table[
      With[
        {coef = moveTerms[[i, 1]], symbol = moveTerms[[i, 2]]},
        If[PossibleZeroQ[coef],
          Nothing,
          If[TrueQ[coef < 0],
            {
              " - ",
              tf[If[symbol === None, Abs[coef], If[Abs[coef] === 1, symbol, Abs[coef] symbol]]]
            },
            {
              " + ",
              tf[If[symbol === None, Abs[coef], If[Abs[coef] === 1, symbol, Abs[coef] symbol]]]
            }
          ]
        ]
      ],
      {i, 1, Length[moveTerms]}
    ]
  ];

  movedRHS = renderMovedRHS[];

  (* najprv rozpísaný tvar, až potom faktorizovaný výsledok *)
  valueExpanded = Expand[rhsShift/unknownCoeff];
  valueFactored = Factor[Together[valueExpanded]];

  valueFinal = If[
    valueFactored =!= valueExpanded && LeafCount[valueFactored] <= LeafCount[valueExpanded],
    valueFactored,
    valueExpanded
  ];

  finalRows = {
    {currentLHS, rhs, substNote[solMap, Keys[solMap], row, vars]}
  };

  If[TrueQ[showSubstitutionRowQ],
    AppendTo[finalRows, {substLHS, rhs, ""}];
    AppendTo[finalRows, {renderTermsRow[expandedTerms, "Symbolic"], rhs, ""}]
  ];

  If[
    unknownCoeff === 1 && PossibleZeroQ[knownSum],

  (* ak sme už po dosadení vypísali zjednodušený riadok, ďalší rovnaký nepridávame *)
    If[!TrueQ[showSubstitutionRowQ],
      AppendTo[finalRows, {tf[solvedVar], tf[valueExpanded], ""}]
    ],

    AppendTo[finalRows, {combinedLHS, rhs, noteShift}];

    finalRows = Join[
      finalRows,
      Which[
        unknownCoeff === 1,
        {
          {tf[solvedVar], movedRHS, ""},
          {tf[solvedVar], tf[valueExpanded], ""}
        },

        unknownCoeff === -1,
        {
          {tf[-solvedVar], movedRHS, multNote[-1]},
          {tf[solvedVar], tf[valueExpanded], ""}
        },

        True,
        {
          {tf[unknownCoeff solvedVar], movedRHS, divNote[unknownCoeff]},
          {tf[solvedVar], tf[valueExpanded], ""}
        }
      ]
    ];
  ];

  If[valueFinal =!= valueExpanded,
    AppendTo[finalRows, {tf[solvedVar], tf[valueFinal], ""}]
  ];

  (* odstránenie duplicitných susedných riadkov *)
  Do[
    rowKey = ToString[{finalRows[[i, 1]], finalRows[[i, 2]]}, InputForm];

    If[rowKey === lastKey,
      If[finalRows[[i, 3]] =!= "" && dedupRows =!= {},
        dedupRows[[-1, 3]] = finalRows[[i, 3]]
      ],
      AppendTo[dedupRows, finalRows[[i]]];
      lastKey = rowKey
    ],
    {i, 1, Length[finalRows]}
  ];

  <|
    "Type" -> "ONE",
    "Value" -> valueFinal,
    "Steps" -> dedupRows
  |>
];

backSubstituteVariableSteps[solvedVar_, expr_, vars_List, solMap_Association] := Module[
  {eqExpr, row, rhs},

(* výraz solvedVar = expr prepíšeme na rovnicu solvedVar - expr = 0 *)
  eqExpr = Expand[solvedVar - expr];

  row = Together /@ (Coefficient[eqExpr, #] & /@ vars);
  rhs = Together[-(eqExpr /. (Rule[#, 0] & /@ vars))];

  backSubstituteVariableSteps[
    {row, rhs},
    vars,
    solMap,
    solvedVar
  ]
];

(* jeden redukčný krok eliminačnej metódy *)
reduceOnceByElimination[eqs_List, vars_List] := Module[
  {
    n = Length[vars], A, b, content = {}, data2, sumRow, sumRHS,
    elimIdx, keepIdx, keepVar, elimVar, pivotEq, newEq, cls,
    lastSolve, red, A2, b2, remVars, idx
  },

  A = eqs[[All, 1]];
  b = eqs[[All, 2]];

  If[n === 2,
    Module[
      {
        rowsShow, zeroCase, rowKeep, elimIdx0, keepIdx0,
        elimVar0, keepVar0, pivotEq0, newEq0, cls0
      },

      rowsShow = {
        {renderTermsRow[Transpose[{A[[1]], vars}]], b[[1]], ""},
        {renderTermsRow[Transpose[{A[[2]], vars}]], b[[2]], ""}
      };

      zeroCase = Which[
        PossibleZeroQ[A[[1, 1]]], {1, 1},
        PossibleZeroQ[A[[2, 1]]], {2, 1},
        PossibleZeroQ[A[[1, 2]]], {1, 2},
        PossibleZeroQ[A[[2, 2]]], {2, 2},
        True, None
      ];

      If[zeroCase =!= None,
        {rowKeep, elimIdx0} = zeroCase;
        keepIdx0 = 3 - elimIdx0;

        elimVar0 = vars[[elimIdx0]];
        keepVar0 = vars[[keepIdx0]];
        pivotEq0 = {A[[3 - rowKeep]], b[[3 - rowKeep]]};

        appendStepHeader[content, "Priama redukcia"];

        AppendTo[
          content,
          "V jednej rovnici je koeficient pri premennej " <> ToString[elimVar0] <>
              " nulový, preto už táto rovnica obsahuje iba " <> ToString[keepVar0] <> "."
        ];

        AppendTo[content, alignedEquations[rowsShow]];

        newEq0 = {{{A[[rowKeep, keepIdx0]]}, b[[rowKeep]]}};
        cls0 = equationClass[{A[[rowKeep, keepIdx0]]}, b[[rowKeep]]];

        Return[
          <|
            "Content" -> content,
            "NewEqs" -> newEq0,
            "NewVars" -> {keepVar0},
            "ElimVar" -> elimVar0,
            "PivotEq" -> pivotEq0,
            "Classes" -> {cls0}
          |>
        ]
      ];

      data2 = eliminationStart2[A, b, vars];
      content = Join[content, data2["content"]];

      sumRow = Total[data2["A_mod"]];
      sumRHS = Total[data2["b_mod"]];

      elimIdx = If[data2["EliminatedVariable"] === "X", 1, 2];
      keepIdx = 3 - elimIdx;
      elimVar = vars[[elimIdx]];
      keepVar = vars[[keepIdx]];

      appendStepHeader[content, "Sčítanie rovníc"];

      AppendTo[
        content,
        "Sčítame rovnice, aby sme eliminovali premennú " <> ToString[elimVar] <> "."
      ];

      newEq = {{{sumRow[[keepIdx]]}, sumRHS}};
      cls = equationClass[{sumRow[[keepIdx]]}, sumRHS];

      lastSolve = solveOneVarEquationSteps[First[newEq], {keepVar}];

      AppendTo[
        content,
        alignedEquations[
          Join[
            Table[
              {
                renderTermsRow[Transpose[{data2["A_mod"][[i]], vars}]],
                data2["b_mod"][[i]],
                ""
              },
              {i, 1, 2}
            ],
            {additionEquationRow2[data2["A_mod"], data2["b_mod"], vars]},
            lastSolve["Content"]
          ],
          {2, 3},
          1
        ]
      ];

      pivotEq = {A[[1]], b[[1]]};

      Return[
        <|
          "Content" -> content,
          "NewEqs" -> newEq,
          "NewVars" -> {keepVar},
          "ElimVar" -> elimVar,
          "PivotEq" -> pivotEq,
          "Classes" -> {cls}
        |>
      ]
    ]
  ];

  red = reduce3to2[A, b, vars];

  If[red === $Failed, Return[$Failed]];

  content = Join[content, red["Content"]];
  A2 = red["A2"];
  b2 = red["b2"];
  remVars = red["remVars"];
  idx = red["SubstRowIndex"];

  pivotEq = {A[[idx]], b[[idx]]};

  <|
    "Content" -> content,
    "NewEqs" -> Table[{A2[[i]], b2[[i]]}, {i, 1, 2}],
    "NewVars" -> remVars,
    "ElimVar" -> red["elimVar"],
    "PivotEq" -> pivotEq,
    "Classes" -> (equationClass @@@ Thread[{A2, b2}])
  |>
];

(* výber premennej na elimináciu v sústave 2x2 *)
pickElimVar2[A_] := Module[{scores},
  scores = Table[
    Module[{c1 = A[[1, i]], c2 = A[[2, i]], lcm},
      If[c1 == 0 || c2 == 0, 9999,
        lcm = LCM[Abs[c1], Abs[c2]];
        lcm + If[(lcm/Abs[c1]) > 1 && (lcm/Abs[c2]) > 1, 1000, 0]
      ]
    ],
    {i, 1, 2}
  ];
  If[scores[[2]] < scores[[1]], 2, 1]
];

(* výber premennej na elimináciu v sústave 3x3 *)
pickElimVar3[A_] := Module[{zp, scorePair, zeroCols, scoreZeroCol, baseScores},
  zp = zeroCoeff3[A];
  scorePair[c1_, c2_] := If[Abs[c1] == Abs[c2] && Sign[c1] =!= Sign[c2], 0, LCM[Abs[c1], Abs[c2]]];
  zeroCols = Select[Range[3], zp["ZeroRowsByCol"][[#]] =!= {} &];
  If[zeroCols =!= {},
    scoreZeroCol[j_] := Module[{zeroRows, nonZeroRows, c1, c2},
      zeroRows = zp["ZeroRowsByCol"][[j]];
      nonZeroRows = Complement[Range[3], zeroRows];
      Which[
        Length[nonZeroRows] >= 2, {c1, c2} = A[[nonZeroRows[[1 ;; 2]], j]]; {0, scorePair[c1, c2]},
        Length[zeroRows] >= 2, {1, 0},
        True, {2, Infinity}
      ]
    ];
    Return[First @ MinimalBy[zeroCols, scoreZeroCol]];
  ];
  baseScores = Table[scorePair[A[[1, j]], A[[2, j]]] + scorePair[A[[1, j]], A[[3, j]]], {j, 1, 3}];
  Ordering[baseScores, 1][[1]]
];

(* príprava dvojice rovníc na vyrušenie zvolenej premennej *)
eliminationStart2[A_, b_, vars_] := Module[{idx, k1, k2, lcm, m1, m2, choice, targetVar, needsMult, content = {}, rows1, rows2},
  idx = pickElimVar2[A];
  targetVar = vars[[idx]];
  choice = If[idx == 1, "X", "Y"];
  k1 = A[[1, idx]]; k2 = A[[2, idx]];
  lcm = LCM[Abs[k1], Abs[k2]];
  m1 = lcm / Abs[k1];
  m2 = lcm / Abs[k2];
  If[Sign[k1] === Sign[k2], m2 = -m2];
  needsMult = !(Sign[k1] =!= Sign[k2] && m1 === 1 && m2 === 1);

  appendStepHeader[content, "Príprava na elimináciu"];
  If[needsMult,
    AppendTo[content, "Chceme vyrušiť premennú " <> ToString[targetVar] <> ". Rovnice preto prenásobíme tak, aby mali pri nej rovnaký koeficient s opačným znamienkom."],
    AppendTo[content, "Koeficienty pri premennej " <> ToString[targetVar] <> " sú už opačné, takže môžeme hneď sčítať rovnice a premennú vyrušiť."]
  ];

  rows1 = {
    {renderTermsRow[Transpose[{A[[1]], vars}], "Numeric", targetVar], b[[1]], multNote[m1]},
    {renderTermsRow[Transpose[{A[[2]], vars}], "Numeric", targetVar], b[[2]], multNote[m2]}
  };

  rows2 = {
    {renderTermsRow[Transpose[{m1 A[[1]], vars}]], m1 b[[1]], ""},
    {renderTermsRow[Transpose[{m2 A[[2]], vars}]], m2 b[[2]], ""}
  };

  If[needsMult,
    AppendTo[content, alignedEquations[Join[rows1, rows2], {2}, 1]],
    AppendTo[content, alignedEquations[{{renderTermsRow[Transpose[{A[[1]], vars}], "Numeric", targetVar], b[[1]], ""}, {renderTermsRow[Transpose[{A[[2]], vars}], "Numeric", targetVar], b[[2]], ""}}]]
  ];

  <|"content" -> content, "m1" -> m1, "m2" -> m2, "EliminatedVariable" -> choice, "A_mod" -> {m1 A[[1]], m2 A[[2]]}, "b_mod" -> {m1 b[[1]], m2 b[[2]]}|>
];

(* výber najvýhodnejšej dvojice riadkov pre elimináciu v 3x3 *)
pickBestElimPair[rowIdx_List, elimCol_Integer, A_] := Module[{pairs, scorePair},
  pairs = Subsets[rowIdx, {2}];
  scorePair[{i_, j_}] := Module[{c1 = A[[i, elimCol]], c2 = A[[j, elimCol]]},
    If[c1 == 0 || c2 == 0, Infinity, If[Abs[c1] == Abs[c2] && Sign[c1] =!= Sign[c2], 0, LCM[Abs[c1], Abs[c2]]]]
  ];
  First @ MinimalBy[pairs, scorePair]
];

(* výber riadku, z ktorého sa bude spätne dosadzovať v 3x3 *)
pickSubstRow3[zp_, elimCol_Integer, A_] := Module[{rows = Range[3], allNonZero, elimNonZero, score},
  allNonZero = Select[rows, zp["ZeroColsByRow"][[#]] === {} && A[[#, elimCol]] =!= 0 &];
  elimNonZero = Select[rows, A[[#, elimCol]] =!= 0 &];
  score[i_] := Module[{row = A[[i]], c = A[[i, elimCol]]}, {If[Abs[c] == 1, 0, 1], Abs[c], Total[Abs[row]]}];
  Which[
    allNonZero =!= {}, <|"Index" -> First @ MinimalBy[allNonZero, score], "AllNonZeroQ" -> True|>,
    elimNonZero =!= {}, <|"Index" -> First @ MinimalBy[elimNonZero, score], "AllNonZeroQ" -> False|>,
    True, $Failed
  ]
];

(* eliminácia jednej premennej z dvojice rovníc *)
reducePair3[rowA_, rhsA_, rowB_, rhsB_, elimCol_, vars_] := Module[
  {
    content = {},
    valA = rowA[[elimCol]],
    valB = rowB[[elimCol]],
    lcm, m1, m2, needsMult,
    rowA2, rhsA2, rowB2, rhsB2,
    newRow, newRHS, rows1, rows2
  },

  If[PossibleZeroQ[valA] || PossibleZeroQ[valB],
    AppendTo[
      content,
      alignedEquations[
        {
          {renderTermsRow[Transpose[{rowA, vars}], "Numeric", vars[[elimCol]]], rhsA, ""},
          {renderTermsRow[Transpose[{rowB, vars}], "Numeric", vars[[elimCol]]], rhsB, ""}
        }
      ]
    ];

    If[PossibleZeroQ[valB],
      {newRow, newRHS} = {rowB, rhsB},
      {newRow, newRHS} = {rowA, rhsA}
    ];

    AppendTo[
      content,
      alignedEquations[
        {{
          Style[renderTermsRow[Transpose[{newRow, vars}]], Bold],
          Style[newRHS, Bold],
          ""
        }}
      ]
    ];

    Return[<|"Row" -> newRow, "RHS" -> newRHS, "Content" -> content|>]
  ];

  lcm = LCM[Abs[valA], Abs[valB]];
  m1 = lcm/Abs[valA];
  m2 = lcm/Abs[valB];

  If[Sign[valA] == Sign[valB], m2 = -m2];

  needsMult = !(m1 === 1 && m2 === 1);

  rows1 = {
    {renderTermsRow[Transpose[{rowA, vars}], "Numeric", vars[[elimCol]]], rhsA, multNote[m1]},
    {renderTermsRow[Transpose[{rowB, vars}], "Numeric", vars[[elimCol]]], rhsB, multNote[m2]}
  };

  rowA2 = m1 rowA;
  rhsA2 = m1 rhsA;
  rowB2 = m2 rowB;
  rhsB2 = m2 rhsB;

  rows2 = {
    {renderTermsRow[Transpose[{rowA2, vars}]], rhsA2, ""},
    {renderTermsRow[Transpose[{rowB2, vars}]], rhsB2, ""}
  };

  If[needsMult,
    AppendTo[content, alignedEquations[Join[rows1, rows2], {2}, 1]],
    AppendTo[content, alignedEquations[rows1]]
  ];

  newRow = rowA2 + rowB2;
  newRHS = rhsA2 + rhsB2;

  AppendTo[
    content,
    alignedEquations[
      {
        additionEquationRow2[{rowA2, rowB2}, {rhsA2, rhsB2}, vars],
        {
          Style[renderTermsRow[Transpose[{newRow, vars}]], Bold],
          Style[newRHS, Bold],
          ""
        }
      },
      {1},
      1
    ]
  ];

  <|"Row" -> newRow, "RHS" -> newRHS, "Content" -> content|>
];

additionEquationRow2[rowMod_, rhsMod_, vars_] := Module[
  {
    secondStyle = RGBColor[0.05, 0.25, 0.85],
    lhsParts = {}, rhsParts = {},
    firstLHS = True, firstRHS = True,
    addToLHS, addToRHS
  },

  addToLHS[coef_, v_, style_ : None] := Module[{bodyExpr, body},
    If[PossibleZeroQ[coef], Return[Null]];

    bodyExpr = If[Abs[coef] === 1, v, Abs[coef] v];
    body = If[style === None, tf[bodyExpr], Style[tf[bodyExpr], style]];

    If[firstLHS,
      lhsParts = Join[lhsParts, If[TrueQ[coef < 0], {"-", body}, {body}]];
      firstLHS = False,
      lhsParts = Join[lhsParts, {If[TrueQ[coef < 0], " - ", " + "], body}]
    ];
  ];

  addToRHS[coef_, style_ : None] := Module[{body},
    If[PossibleZeroQ[coef], Return[Null]];

    body = If[style === None, tf[Abs[coef]], Style[tf[Abs[coef]], style]];

    If[firstRHS,
      rhsParts = Join[rhsParts, If[TrueQ[coef < 0], {"-", body}, {body}]];
      firstRHS = False,
      rhsParts = Join[rhsParts, {If[TrueQ[coef < 0], " - ", " + "], body}]
    ];
  ];

  Do[
    addToLHS[rowMod[[1, i]], vars[[i]]];
    addToLHS[rowMod[[2, i]], vars[[i]], secondStyle],
    {i, 1, Length[vars]}
  ];

  addToRHS[rhsMod[[1]]];
  addToRHS[rhsMod[[2]], secondStyle];

  {
    If[lhsParts === {}, tf[0], Row[lhsParts]],
    If[rhsParts === {}, tf[0], Row[rhsParts]],
    ""
  }
];

(* redukcia sústavy 3x3 na dvojicu rovníc 2x2 *)
reduce3to2[A_, b_, vars_] := Module[
  {
    content = {}, zp, substPick, elimCol, elimVar, zeroRows, nonZeroRows,
    iKeep, rowIV, rhsIV, rowV, rhsV, remCols, remVars, A2, b2,
    twoCombosQ, pair, i1, i2
  },

  appendStepHeader[content, "Redukcia sústavy 3x3 na 2x2"];

  zp = zeroCoeff3[A];
  elimCol = pickElimVar3[A];
  elimVar = vars[[elimCol]];

  AppendTo[content, "Eliminujeme premennú " <> ToString[elimVar] <> ", aby sme získali sústavu 2x2."];

  zeroRows = zp["ZeroRowsByCol"][[elimCol]];
  nonZeroRows = Complement[Range[3], zeroRows];

  If[Length[zeroRows] >= 1,
    twoCombosQ = False;

    If[Length[nonZeroRows] >= 2,
      iKeep = First[zeroRows];
      pair = pickBestElimPair[nonZeroRows, elimCol, A];
      {i1, i2} = pair;

      AppendTo[content, Style["a) Kombinácia " <> ToString[i1] <> ". a " <> ToString[i2] <> ". rovnice:", Italic]];

      With[{res = reducePair3[A[[i1]], b[[i1]], A[[i2]], b[[i2]], elimCol, vars]},
        content = Join[content, res["Content"]];
        rowIV = res["Row"];
        rhsIV = res["RHS"];
      ];

      rowV = A[[iKeep]];
      rhsV = b[[iKeep]];

      AppendTo[content, Style["b) Rovnica bez eliminovanej premennej (použijeme ju priamo):", Italic]];
      AppendTo[content, alignedEquations[{{renderTermsRow[Transpose[{rowV, vars}]], rhsV, ""}}]],

      {i1, i2} = zeroRows[[1 ;; 2]];

      rowIV = A[[i1]];
      rhsIV = b[[i1]];
      rowV = A[[i2]];
      rhsV = b[[i2]];

      AppendTo[content, Style["a) Rovnice bez eliminovanej premennej (použijeme ich priamo):", Italic]];
      AppendTo[
        content,
        alignedEquations[
          {
            {renderTermsRow[Transpose[{rowIV, vars}]], rhsIV, ""},
            {renderTermsRow[Transpose[{rowV, vars}]], rhsV, ""}
          }
        ]
      ];
    ],

    twoCombosQ = True;

    AppendTo[content, Style["a) Kombinácia 1. a 2. rovnice:", Italic]];

    With[{res = reducePair3[A[[1]], b[[1]], A[[2]], b[[2]], elimCol, vars]},
      content = Join[content, res["Content"]];
      rowIV = res["Row"];
      rhsIV = res["RHS"];
    ];

    AppendTo[content, Style["b) Kombinácia 1. a 3. rovnice:", Italic]];

    With[{res = reducePair3[A[[1]], b[[1]], A[[3]], b[[3]], elimCol, vars]},
      content = Join[content, res["Content"]];
      rowV = res["Row"];
      rhsV = res["RHS"];
    ];
  ];

  remCols = Delete[Range[3], elimCol];
  remVars = vars[[remCols]];

  A2 = {rowIV[[remCols]], rowV[[remCols]]};
  b2 = {rhsIV, rhsV};

  substPick = pickSubstRow3[zp, elimCol, A];

  If[substPick === $Failed, Return[$Failed]];

  <|
    "Content" -> content,
    "A2" -> A2,
    "b2" -> b2,
    "remVars" -> remVars,
    "elimCol" -> elimCol,
    "elimVar" -> elimVar,
    "twoCombosQ" -> twoCombosQ,
    "SubstRowIndex" -> substPick["Index"],
    "SubstAllNonZeroQ" -> substPick["AllNonZeroQ"]
  |>
];

(* ~-~-~ SUBSTITUTION HELPERS ~-~-~ *)

(* jeden redukčný krok dosadzovacej metódy *)
reduceOnceBySubstitution[eqs_List, vars_List] := Module[{n = Length[vars], A, b, content = {}, rI, cI, solveData, substRule, elimVar, remVars, res, newEq, cls, lastSolve, red, A2, b2},
  A = eqs[[All, 1]]; b = eqs[[All, 2]];

  If[n === 2,
    (
      {rI, cI} = pickSubstSolve2[A, b, vars];
      elimVar = vars[[cI]];
      remVars = Delete[vars, cI];

      appendStepHeader[content, "Vyjadrenie neznámej"];
      AppendTo[content, "Z " <> ToString[rI] <> ". rovnice vyjadríme neznámu " <> ToString[elimVar] <> "."];

      solveData = solveForVarSteps[A[[rI]], b[[rI]], vars, cI];
      AppendTo[content, alignedEquations[solveData["Content"]]];
      substRule = solveData["Rule"];

      appendStepHeader[content, "Dosadenie"];
      AppendTo[content, "Výraz dosadíme do druhej rovnice a upravíme ju."];

      res = substituteIntoEquationSteps[A[[3 - rI]], b[[3 - rI]], vars, substRule, remVars];
      AppendTo[content, alignedEquations[res["Content"]]];

      newEq = {res["NewEq"][[1]], res["NewEq"][[2]]};
      cls = equationClass[newEq[[1]], newEq[[2]]];

      lastSolve = solveOneVarEquationSteps[newEq, remVars];

      Return[<|"Content" -> content, "NewEqs" -> {newEq}, "NewVars" -> remVars, "SolvedVar" -> elimVar, "RuleExpr" -> substRule[[2]], "Classes" -> {cls}|>];
    )
  ];

  (* n === 3 *)
  red = reduce3to2BySubstitution[A, b, vars];
  If[red === $Failed, Return[$Failed]];

  content = Join[content, red["Content"]];
  A2 = red["A2"]; b2 = red["b2"]; remVars = red["remVars"];

  <|
    "Content" -> content,
    "NewEqs" -> Table[{A2[[i]], b2[[i]]}, {i, 1, 2}],
    "NewVars" -> remVars,
    "SolvedVar" -> red["elimVar"],
    "RuleExpr" -> red["substRule"][[2]],
    "Classes" -> (equationClass @@@ Thread[{A2, b2}])
  |>
];

orderTermsByVars[terms_List, vars_List] := Module[{pairs, varOrder, key},
  pairs = Select[terms, MatchQ[#, {_, _}] &];
  varOrder = AssociationThread[vars -> Range[Length[vars]]];
  key[t_] := Which[t[[2]] === None, Infinity, KeyExistsQ[varOrder, t[[2]]], varOrder[t[[2]]], True, Infinity];
  SortBy[pairs, key]
];

linearDecompose[expr_, vrs_List] := Module[{ee, coeffs, c0},
  ee = Expand[Together[expr]];
  coeffs = Together /@ (Coefficient[ee, #] & /@ vrs);
  c0 = Together[ee /. (Rule[#, 0] & /@ vrs)];
  {coeffs, c0}
];

formatLinearExpr[expr_, vrs_List] := Module[{coeffs, c0, terms = {}},
  {coeffs, c0} = linearDecompose[expr, vrs];
  Do[If[!PossibleZeroQ[coeffs[[k]]], AppendTo[terms, {coeffs[[k]], vrs[[k]]}]], {k, 1, Length[vrs]}];
  If[!PossibleZeroQ[c0], AppendTo[terms, {c0, None}]];
  renderTermsRow[terms]
];

formatSubstOnceLHS[row_, vars_, targetVar_, substExpr_] := Module[
  {
    terms = {}, first = True, addTerm, substVars, expandedSubstExpr,
    substCoeffs, substConst, substTerms, hasVariableTermsQ, substBody
  },

  addTerm[content_, sign_] := (
    AppendTo[
      terms,
      If[
        first,
        If[sign === -1, Row[{"-", content}], content],
        Row[{If[sign === -1, " - ", " + "], content}]
      ]
    ];
    first = False
  );

  substVars = DeleteCases[vars, targetVar];
  expandedSubstExpr = Expand[Together[substExpr]];

  {substCoeffs, substConst} = linearDecompose[expandedSubstExpr, substVars];

  substTerms = Join[
    DeleteCases[
      Table[
        If[
          PossibleZeroQ[substCoeffs[[k]]],
          Nothing,
          {substCoeffs[[k]], substVars[[k]]}
        ],
        {k, 1, Length[substVars]}
      ],
      Nothing
    ],
    If[PossibleZeroQ[substConst], {}, {{substConst, None}}]
  ];

  hasVariableTermsQ = AnyTrue[substTerms, #[[2]] =!= None &];

  substBody = If[
    hasVariableTermsQ,
    Row[{"(", renderTermsRow[substTerms, "Symbolic", substVars], ")"}],
    Style[Row[{"(", renderTermsRow[substTerms, "Symbolic"], ")"}], Bold]
  ];

  Do[
    With[{c = row[[i]], v = vars[[i]]},
      If[
        c =!= 0,
        If[
          v === targetVar,
          addTerm[
            If[
              Abs[c] === 1,
              substBody,
              Row[{tf[Abs[c]], "\[CenterDot]", substBody}]
            ],
            Sign[c]
          ],
          addTerm[tf[If[Abs[c] === 1, v, Abs[c] v]], Sign[c]]
        ]
      ]
    ],
    {i, 1, Length[vars]}
  ];

  If[terms === {}, tf[0], Row[terms]]
];

pickSubstSolve2[A_, b_, vars_] := Module[{scores},
  scores = Table[With[{c = A[[i, j]]}, If[c == 0, Infinity, If[Abs[c] == 1, 0, Abs[c] + 10]]], {i, 2}, {j, 2}];
  First @ Position[scores, Min[scores]]
];

pickSubstSolve3[A_, b_, vars_] := Module[{scores},
  scores = Table[With[{c = A[[i, j]]}, If[c == 0, Infinity, If[Abs[c] == 1, 0 + Count[A[[i]], 0]*(-1), Abs[c] + 100]]], {i, 3}, {j, 3}];
  First @ Position[scores, Min[scores]]
];

(* kroky vyjadrenia jednej premennej z rovnice *)
solveForVarSteps[row_, rhs_, vars_, varIndex_] := Module[
  {
    targetVar, c, otherVars, otherTerms, moveExpr, rhsExpr, solExpr,
    stepsIso = {}, moveNote, currentLHS, rhsDisplay
  },

  targetVar = vars[[varIndex]];
  c = row[[varIndex]];
  otherVars = Delete[vars, varIndex];

  (* členy mimo vyjadrovanej premennej držíme rozvinuté *)
  otherTerms = Expand[Together[Delete[row, varIndex] . otherVars]];
  moveExpr = Expand[Together[-otherTerms]];

  currentLHS = renderTermsRow[Transpose[{row, vars}], "Numeric", {targetVar}];
  moveNote = If[PossibleZeroQ[moveExpr], "", addNote[moveExpr]];

  AppendTo[stepsIso, {currentLHS, tf[rhs], moveNote}];

  rhsExpr = Expand[Together[rhs + moveExpr]];
  rhsDisplay = formatLinearExpr[rhsExpr, otherVars];

  Which[
    c === 1,
    Null,

    c === -1,
    AppendTo[stepsIso, {tf[-targetVar], rhsDisplay, multNote[-1]}],

    True,
    AppendTo[stepsIso, {tf[c targetVar], rhsDisplay, divNote[c]}]
  ];

  (* do pravidla aj do výstupu ukladáme rozvinutý výraz *)
  solExpr = Expand[Together[rhsExpr/c]];

  AppendTo[
    stepsIso,
    {
      Style[tf[targetVar], Bold],
      Style[formatLinearExpr[solExpr, otherVars], Bold],
      ""
    }
  ];

  <|
    "Content" -> stepsIso,
    "Rule" -> (targetVar -> solExpr),
    "Expr" -> solExpr,
    "Var" -> targetVar
  |>
];

(* dosadenie vyjadrenej premennej do ďalšej rovnice *)
substituteIntoEquationSteps[row_, rhs_, vars_, rule_, remainingVars_] := Module[
  {
    targetVar, substExpr, expandedSubstExpr, substRule, stepRows,
    currentLHS, sNote, pos, targetCoeff, allTerms, baseBefore, baseAfter,
    subCoeffs, subConst, expandedSubTerms, distTerms, lhsCombined,
    newRow, constLeft, newRHS, c, lhsTermsWithConst
  },

  targetVar = rule[[1]];
  substExpr = rule[[2]];
  expandedSubstExpr = Expand[Together[substExpr]];
  substRule = targetVar -> expandedSubstExpr;
  stepRows = {};

  currentLHS = renderTermsRow[Transpose[{row, vars}], "Numeric", {targetVar}];
  sNote = substNote[<|targetVar -> expandedSubstExpr|>, {targetVar}, row, vars];

  AppendTo[stepRows, {currentLHS, tf[rhs], sNote}];

  pos = First @ First @ Position[vars, targetVar];
  targetCoeff = row[[pos]];

  If[
    PossibleZeroQ[targetCoeff],

    newRow = Coefficient[row . vars, #] & /@ remainingVars;
    constLeft = (row . vars) /. (Rule[#, 0] & /@ remainingVars);
    newRHS = rhs - constLeft;

    If[
      Length[remainingVars] === 1,
      Module[{c = newRow[[1]], v = remainingVars[[1]], iso},
        iso = isolateVarFromCoeffEqSteps[c, v, newRHS];

        Which[
          c === 1,
          Return[
            <|
              "Content" -> {{
                Style[renderTermsRow[Transpose[{row, vars}]], Bold],
                Style[tf[rhs], Bold],
                ""
              }},
              "NewEq" -> {newRow, newRHS}
            |>
          ],

          c =!= 0 && iso["Type"] === "GENERAL",
          Return[
            <|
              "Content" -> iso["Steps"],
              "NewEq" -> {{1}, iso["Value"]}
            |>
          ],

          True,
          Return[
            <|
              "Content" -> {{
                Style[renderTermsRow[Transpose[{row, vars}]], Bold],
                Style[tf[rhs], Bold],
                ""
              }},
              "NewEq" -> {newRow, newRHS}
            |>
          ]
        ]
      ],

      Return[
        <|
          "Content" -> {{
            Style[renderTermsRow[Transpose[{row, vars}]], Bold],
            Style[tf[rhs], Bold],
            ""
          }},
          "NewEq" -> {newRow, newRHS}
        |>
      ]
    ];
  ];

  allTerms = MapThread[List, {row, vars}];

  baseBefore = Select[Take[allTerms, pos - 1], !PossibleZeroQ[#[[1]]] &];
  baseAfter = Select[Drop[allTerms, pos], !PossibleZeroQ[#[[1]]] &];

  {subCoeffs, subConst} = linearDecompose[expandedSubstExpr, remainingVars];

  expandedSubTerms = Join[
    DeleteCases[
      Table[
        If[
          PossibleZeroQ[subCoeffs[[k]]],
          Nothing,
          {targetCoeff subCoeffs[[k]], remainingVars[[k]]}
        ],
        {k, 1, Length[remainingVars]}
      ],
      Nothing
    ],
    If[PossibleZeroQ[subConst], {}, {{targetCoeff subConst, None}}]
  ];

  distTerms = Join[baseBefore, expandedSubTerms, baseAfter];

  If[
    Abs[targetCoeff] =!= 1,
    AppendTo[stepRows, {formatSubstOnceLHS[row, vars, targetVar, expandedSubstExpr], tf[rhs], ""}]
  ];

  AppendTo[stepRows, {renderTermsRow[distTerms, "Symbolic"], tf[rhs], ""}];

  lhsCombined = Expand[row . vars /. substRule];

  newRow = Coefficient[lhsCombined, #] & /@ remainingVars;
  constLeft = lhsCombined /. (Rule[#, 0] & /@ remainingVars);
  newRHS = rhs - constLeft;

  lhsTermsWithConst = Join[
    DeleteCases[
      Table[
        If[PossibleZeroQ[newRow[[k]]], Nothing, {newRow[[k]], remainingVars[[k]]}],
        {k, 1, Length[remainingVars]}
      ],
      Nothing
    ],
    If[PossibleZeroQ[constLeft], {}, {{constLeft, None}}]
  ];

  If[
    !PossibleZeroQ[constLeft],
    AppendTo[
      stepRows,
      {
        renderTermsRow[lhsTermsWithConst],
        tf[rhs],
        addNote[-constLeft]
      }
    ]
  ];

  AppendTo[
    stepRows,
    {
      Style[renderTermsRow[Transpose[{newRow, remainingVars}]], Bold],
      Style[tf[newRHS], Bold],
      ""
    }
  ];

  If[
    Length[remainingVars] === 1,

    With[
      {
        c = newRow[[1]],
        v = remainingVars[[1]],
        iso = isolateVarFromCoeffEqSteps[newRow[[1]], remainingVars[[1]], newRHS]
      },
      Which[
        c === 1,
        <|"Content" -> stepRows, "NewEq" -> {newRow, newRHS}|>,

        c =!= 0 && iso["Type"] === "GENERAL",
        <|
          "Content" -> Join[Most[stepRows], iso["Steps"]],
          "NewEq" -> {{1}, iso["Value"]}
        |>,

        True,
        <|"Content" -> stepRows, "NewEq" -> {newRow, newRHS}|>
      ]
    ],

    <|"Content" -> stepRows, "NewEq" -> {newRow, newRHS}|>
  ]
];

(* redukcia sústavy 3x3 na 2x2 pomocou dosadzovania *)
reduce3to2BySubstitution[A_, b_, vars_] := Module[{content = {}, rI, cI, solveData, elimVar, substRule, otherRowsIdx, A2, b2, remVars, remCols, idx},
  {rI, cI} = pickSubstSolve3[A, b, vars];
  elimVar = vars[[cI]];

  appendStepHeader[content, "Vyjadrenie neznámej z jednej rovnice"];
  AppendTo[content, "Vyberieme si " <> ToString[rI] <> ". rovnicu a vyjadríme z nej neznámu " <> ToString[elimVar] <> "."];

  solveData = solveForVarSteps[A[[rI]], b[[rI]], vars, cI];
  AppendTo[content, alignedEquations[solveData["Content"]]];

  substRule = solveData["Rule"];

  appendStepHeader[content, "Dosadenie do zvyšných rovníc"];
  AppendTo[content, "Získaný výraz dosadíme do ostatných dvoch rovníc a upravíme ich."];

  otherRowsIdx = Delete[Range[3], rI];
  remCols = Delete[Range[3], cI];
  remVars = vars[[remCols]];
  A2 = {}; b2 = {};

  Do[
    idx = otherRowsIdx[[k]];
    AppendTo[content, Style["Dosadenie do " <> ToString[idx] <> ". rovnice:", Italic]];
    With[{res = substituteIntoEquationSteps[A[[idx]], b[[idx]], vars, substRule, remVars]},
      AppendTo[content, alignedEquations[res["Content"]]];
      AppendTo[A2, res["NewEq"][[1]]];
      AppendTo[b2, res["NewEq"][[2]]];
    ],
    {k, 1, 2}
  ];

  <|"Content" -> content, "A2" -> A2, "b2" -> b2, "remVars" -> remVars, "elimVar" -> elimVar, "substRule" -> substRule, "elimCol" -> cI, "sourceRow" -> rI|>
];


(* ~-~-~ STEP GENERATION ~-~-~ *)

stepsGauss[data_Association] := Catch[Module[ {content = {}, n, aug, vars, st, addHeader, addText, addMatrix, notes, before, after, kPivot, elimRes, pNow, solLocal, tmp},
  n = data["n"]; aug = data["Aug"]; vars = data["Vars"]; st = data["SolutionType"];

  addHeader[text_] := appendStepHeader[content, text];
  addText[text_] := AppendTo[content, text];
  addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] := AppendTo[content, alignedAugmentedMatrix[m, rowNotes, hi]];

  addHeader["Prepis sústavy do rozšírenej matice"];
  addText["Sústavu si najprv prepíšeme do rozšírenej matice, aby sa nám s ňou ďalej ľahšie pracovalo."];
  addMatrix[aug, {}, <|"LeftLabel" -> Style["A", Italic], "RightLabel" -> Style["b", Italic]|>];

  addHeader["Dopredná eliminácia"];
  addText["V danom stĺpci pracujeme s prvkom na hlavnej diagonále. Ak je nulový, prehodíme riadky. Potom vynulujeme prvky pod ním."];

  Do[
    kPivot = gaussPivotRowByNonzero[aug, i];
    If[kPivot =!= i,
      before = aug;
      after = rowApplySwap[before, i, kPivot];
      notes = ConstantArray["", n];
      notes[[i]] = rowNoteSwap[i, kPivot];
      AppendTo[content, augRender2[
        before, after, notes,
        <|"ActiveRow" -> i, "SourceRows" -> {kPivot}, "PivotPos" -> {i, i}|>,
        <|"ActiveRow" -> kPivot, "SourceRows" -> {i}, "PivotPos" -> {i, i}|>
      ]];
      aug = after;
      If[st === "NONE", appendNoneConclusionAndStop[content, aug, data]];
    ];

    pNow = aug[[i, i]];
    If[pNow === 0, Continue[]];

    Do[
      If[aug[[r, i]] =!= 0,
        before = aug;
        elimRes = rowApplyElimStable[before, r, i];
        aug = rowAppendElimStep[content, before, elimRes, r, i, n, <|"SourceRows" -> {i}|>];
        If[st === "NONE", appendNoneConclusionAndStop[content, aug, data]];
      ],
      {r, i + 1, n}
    ],
    {i, 1, n - 1}
  ];

  addHeader["Tvar po Gaussovej eliminácii"];
  addText["Po úpravách dostaneme horný trojuholníkový tvar. Neznáme potom určíme spätným dosadzovaním od posledného riadku."];
  addMatrix[aug, {}, <|"LeftLabel" -> Style["U", Italic], "RightLabel" -> Style["b", Italic]|>];

  If[st === "INFINITE" && AnyTrue[aug, AllTrue[#, # === 0 &] &],
    addText["Tu už vidíme, že vyšiel riadok 0 = 0, takže sústava má nekonečne veľa riešení. Ešte však pokračujeme ďalej, aby sme riešenie vedeli pekne zapísať pomocou parametra."]
  ];

  If[st === "ONE",
    addHeader["Spätné dosadzovanie v rovniciach"];
    tmp = appendTriangularSubstitutionSteps[
      aug[[All, 1 ;; n]],
      aug[[All, n + 1]],
      vars,
      ConstantArray[0, n],
      Range[n, 1, -1],
      content
    ];

    solLocal = tmp[[1]];
    content = tmp[[2]];

    addHeader["Skúška správnosti"];
    addText["Porovnáme A \[CenterDot] x s pravou stranou b po riadkoch."];
    content = Join[content, verificationSteps[data, solLocal]];
    Return[<|"Content" -> content, "Solution" -> solLocal|>];
  ];

  If[st === "INFINITE",
    Module[{ paramIdxs, paramSymbols},
      paramIdxs = Lookup[data, "ParamIdxs", {n - 1, n}];
      paramSymbols = If[Length[paramIdxs] === 1, {\[FormalT]}, {\[FormalS], \[FormalT]}];

      addHeader[
        If[Length[paramIdxs] === 1,
          "Spätné dosadzovanie s parametrom",
          "Spätné dosadzovanie s parametrami"
        ]
      ];

      addText[
        If[Length[paramIdxs] === 1,
          Row[{
            "Jedna premenná je voľná. Označíme ju ",
            tf[vars[[paramIdxs[[1]]]]], " = ", TraditionalForm[paramSymbols[[1]]], "."
          }],
          Row[{
            "Dve premenné sú voľné. Označíme ich ",
            tf[vars[[paramIdxs[[1]]]]], " = ", TraditionalForm[paramSymbols[[1]]],
            " a ",
            tf[vars[[paramIdxs[[2]]]]], " = ", TraditionalForm[paramSymbols[[2]]], "."
          }]
        ]
      ];

      solLocal = ConstantArray[0, n];
      Do[
        solLocal[[paramIdxs[[k]]]] = paramSymbols[[k]],
        {k, 1, Length[paramIdxs]}
      ];

      tmp = appendTriangularSubstitutionSteps[
        aug[[All, 1 ;; n]],
        aug[[All, n + 1]],
        vars,
        solLocal,
        Range[n, 1, -1],
        content,
        paramIdxs,
        paramIdxs
      ];

      solLocal = tmp[[1]];
      content = tmp[[2]];

      addHeader["Skúška správnosti"];
      addText[
        If[Length[paramIdxs] === 1,
          "Dosadíme parametrické riešenie do pôvodných rovníc. V každom riadku musí vyjsť identita pre ľubovoľné \[FormalT] \[Element] \[DoubleStruckCapitalR].",
          "Dosadíme parametrické riešenie do pôvodných rovníc. V každom riadku musí vyjsť identita pre ľubovoľné \[FormalS], \[FormalT] \[Element] \[DoubleStruckCapitalR]."
        ]
      ];
      content = Join[content, verificationStepsInfinite[data, solLocal]];

      Return[<|"Content" -> content, "Solution" -> "INFINITE"|>];
    ]
  ];

  <|"Content" -> content, "Solution" -> aug[[All, n + 1]]|>
], "StopMatrixSteps"];

stepsGaussJordanShared[data_Association, pivotQ_?BooleanQ, showElemQ_?BooleanQ] := Block[
  {$ElemStepCounter = 0, $ElemMatrixCounter = 0},
  Catch[
    Module[
      {
        content = {}, n, aug, vars, st, addHeader, addText, addMatrix,
        notes, pNow, solLocal, solExprs, pivotRowFn
      },

      n = data["n"];
      aug = data["Aug"];
      vars = data["Vars"];
      st = data["SolutionType"];

      pivotRowFn = If[pivotQ, gaussPivotRowByMinAbs, gaussPivotRowByNonzero];

      addHeader[text_] := appendStepHeader[content, text];
      addText[text_] := AppendTo[content, text];
      addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] :=
          AppendTo[content, alignedAugmentedMatrix[m, rowNotes, hi]];

      addHeader["Prepis sústavy do rozšírenej matice"];

      If[showElemQ,
        addText[
          "Sústavu si najprv prepíšeme do rozšírenej matice a označíme ju M₀. Keďže zobrazujeme aj elementárne matice, pri každej riadkovej úprave uvedieme maticu Eᵢ, pre ktorú platí Mᵢ = Eᵢ · Mᵢ₋₁."
        ];
        AppendTo[
          content,
          namedAugmentedStateCard[
            Subscript[Style["M", Italic], 0],
            aug,
            {},
            <|"LeftLabel" -> Style["A", Italic], "RightLabel" -> Style["b", Italic]|>
          ]
        ],
        addText[
          "Sústavu si najprv prepíšeme do rozšírenej matice, aby sa nám s ňou ďalej ľahšie pracovalo. Úpravami ju prevedieme na tvar (I | x)."
        ];
        addMatrix[
          aug,
          {},
          <|"LeftLabel" -> Style["A", Italic], "RightLabel" -> Style["b", Italic]|>
        ]
      ];

      addHeader["Dopredná eliminácia"];

      addText[
        If[showElemQ,
          If[pivotQ,
            "V aktuálnom stĺpci vyberieme nenulové číslo s najmenšou absolútnou hodnotou. Ak je v inom riadku, riadky prehodíme. Následne vynulujeme čísla pod ním. Každú riadkovú úpravu zapíšeme aj pomocou elementárnej matice.",
            "V danom stĺpci pracujeme s prvkom na hlavnej diagonále. Ak je nulový, prehodíme riadky. Potom vynulujeme prvky pod ním. Každú riadkovú úpravu zapíšeme aj pomocou elementárnej matice."
          ],
          If[pivotQ,
            "V aktuálnom stĺpci vyberieme nenulové číslo s najmenšou absolútnou hodnotou. Ak je v inom riadku, riadky prehodíme. Následne vynulujeme čísla pod ním.",
            "V danom stĺpci pracujeme s prvkom na hlavnej diagonále. Ak je nulový, prehodíme riadky. Potom vynulujeme prvky pod ním."
          ]
        ]
      ];

      If[
        showElemQ,
        addText[
          "Tip: Keď prejdete kurzorom nad prvky výslednej matice, zobrazí sa vám príslušný skalárny súčin."
        ]
      ];

      Do[
        Module[{kPivot},
          kPivot = pivotRowFn[aug, i];

          If[pivotQ && kPivot =!= i,
            addGap[content, 1];
            addText[gaussPivotSwapExplanation[aug, i, kPivot]];
          ];

          If[kPivot =!= i,
            aug = applyJordanSwapStep[content, aug, i, kPivot, n, showElemQ];

            If[st === "NONE",
              appendNoneConclusionAndStop[content, aug, data, showElemQ, $ElemMatrixCounter]
            ];
          ];
        ];

        pNow = aug[[i, i]];
        If[pNow === 0, Continue[]];

        Do[
          If[aug[[r, i]] =!= 0,
            aug = applyJordanElimStep[
              content,
              aug,
              r,
              i,
              n,
              <|"SourceRows" -> {i}|>,
              showElemQ
            ];

            If[st === "NONE",
              appendNoneConclusionAndStop[content, aug, data, showElemQ, $ElemMatrixCounter]
            ];
          ],
          {r, i + 1, n}
        ],
        {i, 1, n - 1}
      ];

      If[st === "INFINITE" && AnyTrue[aug, AllTrue[#, # === 0 &] &],
        addText[
          "Tu už vidíme nulový riadok 0 = 0, preto sústava nemá jediné riešenie. Ešte spravíme spätnú úpravu, aby sme riešenie vedeli prehľadne zapísať pomocou parametra."
        ]
      ];

      addHeader["Spätná eliminácia"];

      addText[
        If[showElemQ,
          "Teraz vynulujeme čísla nad pivotmi. Každú riadkovú úpravu zapíšeme aj pomocou elementárnej matice.",
          "Teraz vynulujeme čísla nad pivotmi."
        ]
      ];

      If[
        showElemQ,
        addText[
          "Tip: Keď prejdete kurzorom nad prvky výslednej matice, zobrazí sa vám príslušný skalárny súčin."
        ]
      ];

      Do[
        pNow = aug[[i, i]];
        If[pNow === 0, Continue[]];

        Do[
          If[aug[[r, i]] =!= 0,
            aug = applyJordanElimStep[content, aug, r, i, n, <||>, showElemQ]
          ],
          {r, 1, i - 1}
        ],
        {i, n, 2, -1}
      ];

      Do[
        pNow = aug[[i, i]];

        If[pNow =!= 0 && pNow =!= 1,
          If[showElemQ,
            aug = applyElemDivideStep[content, aug, i, pNow, n, {i, i}],

            Module[{before, after},
              before = aug;
              after = rowApplyDivide[before, i, pNow];

              notes = ConstantArray["", n];
              notes[[i]] = rowNoteDivide[i, pNow];

              AppendTo[
                content,
                augRender2[
                  before,
                  after,
                  notes,
                  <|"ActiveRow" -> i, "PivotPos" -> {i, i}|>,
                  <|"ActiveRow" -> i, "PivotPos" -> {i, i}|>
                ]
              ];

              aug = after;
            ]
          ]
        ],
        {i, 1, n}
      ];

      If[st === "ONE",
        addHeader["Hotový tvar (I | x)"];
        addText[
          "Po úpravách dostaneme tvar (I | x). Riešenie prečítame z pravej strany."
        ],

        addHeader["Upravený tvar matice"];
        addText[
          "Po úpravách máme zjednodušený tvar matice. Z neho určíme voľné premenné a zapíšeme riešenie pomocou parametrov."
        ]
      ];

      notes = If[
        st === "ONE",
        Table[
          Row[{lhsStyle[vars[[i]]], " = ", TraditionalForm[aug[[i, n + 1]]]}],
          {i, 1, n}
        ],
        ConstantArray["", n]
      ];

      If[showElemQ,
        AppendTo[
          content,
          namedAugmentedStateCard[
            Subscript[Style["M", Italic], $ElemMatrixCounter],
            aug,
            notes,
            If[
              st === "ONE",
              <|"LeftLabel" -> Style["I", Italic], "RightLabel" -> Style["x", Italic]|>,
              <|"LeftLabel" -> Style["A", Italic], "RightLabel" -> Style["b", Italic]|>
            ]
          ]
        ],
        addMatrix[
          aug,
          notes,
          If[
            st === "ONE",
            <|"LeftLabel" -> Style["I", Italic], "RightLabel" -> Style["x", Italic]|>,
            <|"LeftLabel" -> Style["A", Italic], "RightLabel" -> Style["b", Italic]|>
          ]
        ]
      ];

      If[st === "INFINITE",
        Module[{paramIdxs, paramSymbols},
          paramIdxs = Lookup[data, "ParamIdxs", {n - 1, n}];
          paramSymbols = If[Length[paramIdxs] === 1, {\[FormalT]}, {\[FormalS], \[FormalT]}];

          addHeader["Analýza riadkov"];

          addText[
            If[Length[paramIdxs] === 1,
              "Jeden nulový riadok znamená, že jedna premenná je voľná. Označíme ju parametrom a ostatné premenné vyjadríme pomocou neho.",
              "Dva nulové riadky znamenajú, že dve premenné sú voľné. Označíme ich parametrami a ostatné premenné vyjadríme pomocou nich."
            ]
          ];

          notes = ConstantArray["", n];
          Scan[(notes[[#]] = "nulový riadok -> parameter") &, paramIdxs];

          addMatrix[aug, notes, <|"ActiveRows" -> paramIdxs|>];

          Do[
            addText[
              Row[{
                "Premennú ",
                vars[[paramIdxs[[k]]]],
                " označíme parametrom ",
                TraditionalForm[paramSymbols[[k]]],
                "."
              }]
            ];

            AppendTo[
              content,
              highlightGrid @ Grid[
                {{tf[vars[[paramIdxs[[k]]]]], "=", TraditionalForm[paramSymbols[[k]]]}},
                Alignment -> {{Right, Center, Left}},
                BaseStyle -> {FontSize -> 16}
              ]
            ],
            {k, 1, Length[paramIdxs]}
          ];

          addHeader["Vyjadrenie ostatných premenných pomocou parametrov"];

          solExprs = ConstantArray[0, n];

          Do[
            solExprs[[paramIdxs[[k]]]] = paramSymbols[[k]],
            {k, 1, Length[paramIdxs]}
          ];

          Module[{tmp},
            tmp = appendTriangularSubstitutionSteps[
              aug[[All, 1 ;; n]],
              aug[[All, n + 1]],
              vars,
              solExprs,
              Range[n, 1, -1],
              content,
              paramIdxs,
              paramIdxs
            ];

            solExprs = tmp[[1]];
            content = tmp[[2]];
          ];

          addHeader["Skúška správnosti"];

          addText[
            If[Length[paramIdxs] === 1,
              "Dosadíme parametrické riešenie do pôvodných rovníc. V každom riadku musí vyjsť identita pre ľubovoľné \[FormalT] \[Element] \[DoubleStruckCapitalR].",
              "Dosadíme parametrické riešenie do pôvodných rovníc. V každom riadku musí vyjsť identita pre ľubovoľné \[FormalS], \[FormalT] \[Element] \[DoubleStruckCapitalR]."
            ]
          ];

          content = Join[content, verificationStepsInfinite[data, solExprs]];

          Return[<|"Content" -> content, "Solution" -> "INFINITE"|>];
        ]
      ];

      solLocal = aug[[All, n + 1]];

      addHeader["Skúška správnosti"];
      addText["Porovnáme A \[CenterDot] x s pravou stranou b po riadkoch."];

      content = Join[content, verificationSteps[data, solLocal]];

      <|"Content" -> content, "Solution" -> solLocal|>
    ],
    "StopMatrixSteps"
  ]
];

stepsInverseMatrix[data_Association] := Module[{ content = {}, n, A, b, vars, augInv, invMatrix, xResult, addHeader, addText, addMatrix, notes, before, after, kPivot, elimRes, pNow},
  n = data["n"];
  A = data["A"];
  b = data["b"];
  vars = data["Vars"];

  addHeader[text_] := appendStepHeader[content, text];
  addText[text_String] := AppendTo[content, text];
  addText[expr_] := AppendTo[content, Cell[BoxData @ ToBoxes[expr, StandardForm], "Text", ShowStringCharacters -> False]];
  addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] := AppendTo[content, alignedAugmentedMatrixInverse[m, rowNotes, hi]];

  addHeader["Prepis matice do tvaru (A | E)"];
  addText[Row[{"K matici A pripíšeme jednotkovú maticu E. Na oboch stranách budeme robiť rovnaké riadkové úpravy. Cieľom je dostať vľavo E, vpravo potom vznikne ", inverseASymbol[], "."}]];

  augInv = Join[A, IdentityMatrix[n], 2];
  addMatrix[augInv, {}, <|"LeftLabel" -> Style["A", Italic], "RightLabel" -> Style["E", Italic]|>];

  addHeader["Dopredná eliminácia"];
  addText["V aktuálnom stĺpci vyberieme nenulové číslo s najmenšou absolútnou hodnotou. Ak je v inom riadku, riadky prehodíme. Následne vynulujeme čísla pod ním."];

  Do[
    kPivot = gaussPivotRowByMinAbs[augInv, i];

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

  addHeader["Spätná eliminácia"];
  addText["Teraz vynulujeme čísla nad pivotmi. Tým sa ľavá časť postupne mení na jednotkovú maticu."];

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

  addHeader[Row[{"Hotový tvar"}]];
  addText[Row[{"Po úpravách je vľavo jednotková matica E. Pravá časť je preto inverzná matica ", inverseASymbol[], "."}]];
  addMatrix[
    augInv,
    {},
    <|"LeftLabel" -> Style["E", Italic], "RightLabel" -> inverseASymbol[]|>
  ];

  invMatrix = augInv[[All, n + 1 ;; 2 n]];

  addHeader[Row[{"Výpočet riešenia"}]];
  addText[Row[{"Keď už poznáme inverznú maticu, riešenie sústavy vypočítame zo vzťahu x = ", inverseASymbol[], " \[CenterDot] b."}]];
  addText["Tip: Keď prejdete kurzorom nad prvky výsledného vektora x, zobrazí sa skalárny súčin, z ktorého dané číslo vzniklo."];

  xResult = invMatrix . b;

  AppendTo[content, Module[{ resultNotes},
    resultNotes = Grid[
      List /@ MapThread[
        Style[Row[{lhsStyle[#1], " = ", tft[#2]}], GrayLevel[.35], FontSize -> 13] &,
        {vars, xResult}
      ],
      Alignment -> Left,
      Spacings -> {0, 1.15}
    ];

    Grid[
      {{
        Style["x =", Bold, FontSize -> 16],
        labeledMatrixBlock[inverseASymbol[], styledPlainMatrix[invMatrix]],
        Style["\[CenterDot]", Bold, FontSize -> 18],
        labeledMatrixBlock[Style["b", Italic], styledPlainMatrix[List /@ b]],
        Style["=", Bold, FontSize -> 18],
        labeledMatrixBlock[Style["x", Italic], styledPlainMatrix[dotProductTooltipMatrix[invMatrix, List /@ b]]],
        Spacer[3],
        Column[{Style["\[InvisibleSpace]", Bold, FontSize -> 15], resultNotes},
          Alignment -> Left, Spacings -> {4.4}
        ]
      }},
      Alignment -> {Left, Center, Center, Center, Center, Center, Center, Left},
      Spacings -> {1.1, 0}
    ]
  ]];

  addHeader["Skúška správnosti"];
  addText[Row[{"Najprv overíme, že A \[CenterDot] ", inverseASymbol[], " = E. Potom ešte skontrolujeme, že vypočítané riešenie spĺňa A \[CenterDot] x = b."}]];

  Module[{ product, isIdentity},
    product = Together[A . invMatrix];
    isIdentity = product === IdentityMatrix[n];

    AppendTo[content, Grid[
      {{
        labeledMatrixBlock[Row[{Style["A", Italic], " \[CenterDot] ", inverseASymbol[]}], styledPlainMatrix[dotProductTooltipMatrix[A, invMatrix]]],
        Style["=", Bold, FontSize -> 18],
        labeledMatrixBlock[Style["E", Italic], styledPlainMatrix[IdentityMatrix[n]]],
        If[isIdentity, Style["Správne", Darker[Green], Bold], Style["Nesprávne", Red, Bold]]
      }},
      Alignment -> {Center, Center, Center, Center},
      Spacings -> {1.2, 0},
      BaseStyle -> {FontSize -> 13}
    ]];
  ];

  content = Join[content, verificationSteps[data, xResult]];

  <|"Content" -> content, "Solution" -> xResult, "InverseMatrix" -> invMatrix|>
];

stepsLU[data_Association] := Module[{ content = {}, n, A, b, vars, luData, L, U, y, x, tmp, addHeader, addText, addMatrixPair, addFormula, addSubHeader, resultStyle,
    prettyMatrix, prettyVector, appendProductDisplay, appendMatrixEquality, appendVectorEquality, i, j, terms, sumTerm, pivotValue, luProduct, lowerCheck, upperCheck,
    xSymbols, formatLinearEquation, formatForwardEquation, formatBackwardEquation, symbolicProductSum, numericProductSum, sigmaUDisplay, sigmaLDisplay,
    buildUFormulaLines, buildLFormulaLines, buildYFormulaLines, currentLBoldPositions, currentUBoldPositions, taskFormat
  },

  n = data["n"];
  A = data["A"];
  b = data["b"];
  vars = data["Vars"];
  xSymbols = vars;
  taskFormat = Lookup[data, "TaskFormat", "EQUATIONS"];

  addHeader[text_] := appendStepHeader[content, text];
  addSubHeader[text_] := AppendTo[content, Style[text, Bold, FontSize -> 15]];
  addText[text_String] := AppendTo[content, text];
  addText[expr_] := AppendTo[content, Cell[BoxData @ ToBoxes[expr, StandardForm], "Text", ShowStringCharacters -> False]];
  addFormula[expr_] := AppendTo[content, expr];
  resultStyle[expr_] := Style[expr, Bold, Blue];

  prettyMatrix[label_, mat_, bold_List : {}] := labeledMatrixBlock[label, styledPlainMatrix[mat, <|"BoldPositions" -> bold|>]];
  prettyVector[label_, vec_List] := labeledMatrixBlock[label, styledPlainMatrix[List /@ vec]];

  addMatrixPair[l_, u_, lBold_List : {}, uBold_List : {}] := AppendTo[content, Grid[
    {{
      prettyMatrix[Style["L", Italic], l, lBold],
      Spacer[20],
      prettyMatrix[Style["U", Italic], u, uBold]
    }},
    Alignment -> {Center, Center, Center},
    Spacings -> {1.2, 0}
  ]];

  appendProductDisplay[left_, right_, result_] := AppendTo[content, Grid[
    {{
      prettyMatrix[Style["L", Italic], left],
      Style["\[CenterDot]", Bold, FontSize -> 18],
      prettyMatrix[Style["U", Italic], right],
      Style["=", Bold, FontSize -> 18],
      prettyMatrix[Style["A", Italic], dotProductTooltipMatrix[left, right]]
    }},
    Alignment -> {Center, Center, Center, Center, Center},
    Spacings -> {1.2, 0}
  ]];

  appendMatrixEquality[leftLabel_, leftMat_, rightLabel_, rightMat_, okQ_] := AppendTo[content, Grid[
    {{
      prettyMatrix[leftLabel, leftMat],
      Style["=", Bold, FontSize -> 18],
      prettyMatrix[rightLabel, rightMat],
      If[okQ, Style["OK", Darker[Green], Bold], Style["CHYBA", Red, Bold]]
    }},
    Alignment -> {Center, Center, Center, Center},
    Spacings -> {1.2, 0},
    BaseStyle -> {FontSize -> 13}
  ]];

  appendVectorEquality[leftLabel_, leftVec_, rightLabel_, rightVec_, okQ_] := AppendTo[content, Grid[
    {{
      prettyVector[leftLabel, leftVec],
      Style["=", Bold, FontSize -> 18],
      prettyVector[rightLabel, rightVec],
      If[okQ, Style["OK", Darker[Green], Bold], Style["CHYBA", Red, Bold]]
    }},
    Alignment -> {Center, Center, Center, Center},
    Spacings -> {1.2, 0},
    BaseStyle -> {FontSize -> 13}
  ]];

  currentLBoldPositions[step_] := Join[
    Table[{r, r}, {r, 1, n}],
    Flatten[Table[{r, c}, {c, 1, Min[step, n - 1]}, {r, c + 1, n}], 1]
  ];

  currentUBoldPositions[step_] := Flatten[
    Table[{r, c}, {r, 1, step}, {c, r, n}],
    1
  ];

  (* pomocný formát lineárnej rovnice so znamienkami *)
  formatLinearEquation[coeffList_, symbolList_, rhs_] := Module[
    {pairs, nz, firstPair, pieces = {}, c, s, absC},

    pairs = Transpose[{coeffList, symbolList}];
    nz = Select[pairs, #[[1]] =!= 0 &];

    If[nz === {}, Return[Row[{0, " = ", tft[rhs]}]]];

    firstPair = First[nz];
    c = firstPair[[1]];
    s = firstPair[[2]];
    absC = Abs[c];

    AppendTo[pieces, Which[
      c === 1, s,
      c === -1, Row[{"-", s}],
      c < 0, Row[{"-", tft[absC], "\[CenterDot]", s}],
      True, Row[{tft[c], "\[CenterDot]", s}]
    ]];

    Do[
      c = pair[[1]];
      s = pair[[2]];
      absC = Abs[c];

      AppendTo[pieces, Which[
        c === 1, Row[{" + ", s}],
        c === -1, Row[{" - ", s}],
        c > 0, Row[{" + ", tft[absC], "\[CenterDot]", s}],
        True, Row[{" - ", tft[absC], "\[CenterDot]", s}]
      ]];
      ,
      {pair, Rest[nz]}
    ];

    Row[{Row[pieces], " = ", tft[rhs]}]
  ];

  (* rovnica pre L.y = b *)
  formatForwardEquation[row_, rhs_, i_] := Module[{ coeffList, symbolList},
    coeffList = row[[1 ;; i]];
    symbolList = Table[luScalarSymbol["y", k], {k, 1, i}];
    formatLinearEquation[coeffList, symbolList, rhs]
  ];

  (* rovnica pre U.x = y *)
  formatBackwardEquation[row_, rhs_, i_] := Module[{ coeffList, symbolList},
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
  buildUFormulaLines[i_, j_, terms_, value_] := Module[{ symbolicTerms},
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
  buildLFormulaLines[j_, i_, terms_, pivot_, value_] := Module[{ symbolicTerms},
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

  buildYFormulaLines[i_, terms_, value_] := Module[{ },
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

  addHeader[If[taskFormat === "MATRIX",
    "Používané vzťahy",
    "Maticový tvar a používané vzťahy"]
  ];

  If[taskFormat =!= "MATRIX",
    addText["Sústavu najprv zapíšeme v maticovom tvare A \[CenterDot] x = b."];

    AppendTo[content, Grid[
      {{
        prettyMatrix[Style["A", Italic], A],
        Style["\[CenterDot]", Bold, FontSize -> 18],
        prettyVector[Style["x", Italic], xSymbols],
        Style["=", Bold, FontSize -> 18],
        prettyVector[Style["b", Italic], b]
      }},
      Alignment -> {Center, Center, Center, Center, Center},
      Spacings -> {1.2, 0}
    ]];
  ];

  addText["LU rozklad rozloží maticu A na súčin dvoch matíc: dolnej trojuholníkovej matice L a hornej trojuholníkovej matice U."];
  addText["Budeme používať tieto vzťahy:"];

  AppendTo[content, Grid[
    {
      {Style[Row[{Style["A", Italic], " \[CenterDot] ", Style["x", Italic], " = ", Style["b", Italic]}], Bold]},
      {Style[Row[{Style["A", Italic], " = ", Style["L", Italic], " \[CenterDot] ", Style["U", Italic]}], Bold]},
      {Style[Row[{Style["L", Italic], " \[CenterDot] ", Style["y", Italic], " = ", Style["b", Italic]}], Bold]},
      {Style[Row[{Style["U", Italic], " \[CenterDot] ", Style["x", Italic], " = ", Style["y", Italic]}], Bold]}
    },
    Alignment -> Left,
    Spacings -> {1, 0.55},
    BaseStyle -> {FontSize -> 14}
  ]];

  luData = luSolveData[A, b];

  If[luData === $Failed,
    addHeader["Výsledok"];
    addText["Počas rozkladu sa objavil nulový pivot, preto LU rozklad bez pivotovania nemožno použiť."];
    Return[<|"Content" -> content, "Solution" -> Missing["NotAvailable"]|>]
  ];

  L = IdentityMatrix[n];
  U = ConstantArray[0, {n, n}];

  addHeader["Inicializácia matíc"];
  addText["Začíname s jednotkovou diagonálou v matici L. Ostatné prvky matíc L a U budeme postupne dopočítavať."];
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
      addText["Najprv vypočítame prvky aktuálneho riadku matice U."];
      addSubHeader["Prvky matice U:"];
      Do[
        terms = Table[{L[[i, kk]], U[[kk, j]]}, {kk, 1, i - 1}];
        sumTerm = Total[Times @@@ terms];
        U[[i, j]] = Together[A[[i, j]] - sumTerm];
        Scan[addFormula, buildUFormulaLines[i, j, terms, U[[i, j]]]];

        If[j < n && i > 1, addGap[content, 3]];
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

        If[j < n && i > 1, addGap[content, 5]];
        ,
        {j, i + 1, n}
      ];
    ];

    If[i < n,
      addText["Po tomto kroku už poznáme tieto nové prvky matíc L a U:"];
      addMatrixPair[L, U, currentLBoldPositions[i], currentUBoldPositions[i]];
    ];
    ,
    {i, 1, n}
  ];

  addHeader["Hotový rozklad A = L \[CenterDot] U"];
  addText["Po výpočte dostaneme maticu L s jednotkami na diagonále a hornú trojuholníkovú maticu U."];

  addMatrixPair[
    L, U,
    Join[Table[{r, r}, {r, 1, n}], Flatten[Table[{r, c}, {c, 1, n - 1}, {r, c + 1, n}], 1]],
    Flatten[Table[{r, c}, {r, 1, n}, {c, r, n}], 1]
  ];

  addHeader["Overenie rozkladu L \[CenterDot] U = A"];
  addText["Skontrolujeme, že súčin L \[CenterDot] U sa rovná matici A."];
  addText["Tip: Keď prejdete kurzorom nad prvky výslednej matice, zobrazí sa výpočet, z ktorého dané číslo vzniklo."];

  luProduct = Together[L . U];

  appendProductDisplay[L, U, luProduct];

  addHeader["Riešenie pomocnej sústavy L \[CenterDot] y = b"];
  addText["Najprv vyriešime pomocnú sústavu L \[CenterDot] y = b. Keďže L je dolná trojuholníková matica, použijeme dopredné dosadzovanie."];
  AppendTo[content, alignedAugmentedMatrix[augFromAb[L, b], {}, <|"BoldDiagonal" -> True, "LeftLabel" -> Style["L", Italic], "RightLabel" -> Style["b", Italic]|>]];

  tmp = appendTriangularSubstitutionSteps[
    L, b,
    Table[luScalarSymbol["y", k], {k, 1, n}],
    ConstantArray[0, n],
    Range[n],
    content
  ];

  y = tmp[[1]];
  content = tmp[[2]];

  AppendTo[content, prettyVector[Style["y", Italic], y]];

  addHeader["Riešenie sústavy U \[CenterDot] x = y"];
  addText["Potom vyriešime sústavu U \[CenterDot] x = y. Keďže U je horná trojuholníková matica, použijeme spätné dosadzovanie."];
  AppendTo[content, alignedAugmentedMatrix[augFromAb[U, y], {}, <|"BoldDiagonal" -> True, "LeftLabel" -> Style["U", Italic], "RightLabel" -> Style["y", Italic]|>]];

  tmp = appendTriangularSubstitutionSteps[
    U,
    y,
    vars,
    ConstantArray[0, n],
    Range[n, 1, -1],
    content
  ];

  x = tmp[[1]];
  content = tmp[[2]];

  addHeader["Skúška správnosti"];
  addText["Skontrolujeme rozklad A = L \[CenterDot] U, pomocnú sústavu L \[CenterDot] y = b, sústavu U \[CenterDot] x = y aj pôvodnú sústavu A \[CenterDot] x = b."];

  lowerCheck = Together[L . y];
  upperCheck = Together[U . x];

  appendMatrixEquality[Row[{Style["L", Italic], " \[CenterDot] ", Style["U", Italic]}], luProduct, Style["A", Italic], A, luProduct === A];

  addGap[content, 6];
  appendVectorEquality[Row[{Style["L", Italic], " \[CenterDot] ", Style["y", Italic]}], lowerCheck, Style["b", Italic], b, lowerCheck === b];

  addGap[content, 6];
  appendVectorEquality[Row[{Style["U", Italic], " \[CenterDot] ", Style["x", Italic]}], upperCheck, Style["y", Italic], y, upperCheck === y];

  addGap[content, 6];
  content = Join[content, verificationSteps[data, x]];

  <|
    "Content" -> content,
    "Solution" -> x,
    "L" -> L,
    "U" -> U,
    "Y" -> y
  |>
];

stepsCholesky[data_Association] := Module[{ content = {}, n, A, b, vars, choleskyData, L, LT, y, x, tmp, addHeader, addText, addSubHeader, addFormula, addMatrixPair, prettyMatrix,
  prettyVector, appendProductDisplay, appendMatrixEquality, appendVectorEquality, i, j, productCheck, lowerCheck, upperCheck, ySymbols, currentLBoldPositions, currentLTBoldPositions, taskFormat},

  n = data["n"];
  A = data["A"];
  b = data["b"];
  vars = data["Vars"];
  ySymbols = Table[luScalarSymbol["y", k], {k, 1, n}];
  taskFormat = Lookup[data, "TaskFormat", "EQUATIONS"];

  addHeader[text_] := appendStepHeader[content, text];
  addSubHeader[text_] := AppendTo[content, Style[text, Bold, FontSize -> 15]];
  addText[text_String] := AppendTo[content, text];
  addText[expr_] := AppendTo[content, Cell[BoxData @ ToBoxes[expr, StandardForm], "Text", ShowStringCharacters -> False]];
  addFormula[expr_] := AppendTo[content, expr];

  prettyMatrix[label_, mat_, bold_List : {}] := labeledMatrixBlock[label, styledPlainMatrix[mat, <|"BoldPositions" -> bold|>]];
  prettyVector[label_, vec_List] := labeledMatrixBlock[label, styledPlainMatrix[List /@ vec]];

  addMatrixPair[l_, lt_, lBold_List : {}, ltBold_List : {}] := AppendTo[content, Grid[
    {{
      prettyMatrix[Style["L", Italic], l, lBold],
      Spacer[20],
      prettyMatrix[transposeLSymbol[], lt, ltBold]
    }},
    Alignment -> {Center, Center, Center},
    Spacings -> {1.2, 0}
  ]];

  appendProductDisplay[left_, right_, result_] := AppendTo[content, Grid[
    {{
      prettyMatrix[Style["L", Italic], left],
      Style["\[CenterDot]", Bold, FontSize -> 18],
      prettyMatrix[transposeLSymbol[], right],
      Style["=", Bold, FontSize -> 18],
      prettyMatrix[Style["A", Italic], dotProductTooltipMatrix[left, right]]
    }},
    Alignment -> {Center, Center, Center, Center, Center},
    Spacings -> {1.2, 0}
  ]];

  appendMatrixEquality[leftLabel_, leftMat_, rightLabel_, rightMat_, okQ_] := AppendTo[content, Grid[
    {{
      prettyMatrix[leftLabel, leftMat],
      Style["=", Bold, FontSize -> 18],
      prettyMatrix[rightLabel, rightMat],
      If[okQ, Style["OK", Darker[Green], Bold], Style["CHYBA", Red, Bold]]
    }},
    Alignment -> {Center, Center, Center, Center},
    Spacings -> {1.2, 0},
    BaseStyle -> {FontSize -> 13}
  ]];

  appendVectorEquality[leftLabel_, leftVec_, rightLabel_, rightVec_, okQ_] := AppendTo[content, Grid[
    {{
      prettyVector[leftLabel, leftVec],
      Style["=", Bold, FontSize -> 18],
      prettyVector[rightLabel, rightVec],
      If[okQ, Style["OK", Darker[Green], Bold], Style["CHYBA", Red, Bold]]
    }},
    Alignment -> {Center, Center, Center, Center},
    Spacings -> {1.2, 0},
    BaseStyle -> {FontSize -> 13}
  ]];

  currentLBoldPositions[step_] := Flatten[Table[{r, c}, {c, 1, step}, {r, c, n}], 1];
  currentLTBoldPositions[step_] := Reverse /@ currentLBoldPositions[step];

  addHeader[If[taskFormat === "MATRIX",
    "Používané vzťahy",
    "Maticový tvar a používané vzťahy"]
  ];

  If[taskFormat =!= "MATRIX",
    addText["Sústavu prepíšeme do maticového tvaru."];

    AppendTo[content, Grid[
      {{
        prettyMatrix[Style["A", Italic], A],
        Style["\[CenterDot]", Bold, FontSize -> 18],
        prettyVector[Style["x", Italic], vars],
        Style["=", Bold, FontSize -> 18],
        prettyVector[Style["b", Italic], b]
      }},
      Alignment -> {Center, Center, Center, Center, Center},
      Spacings -> {1.2, 0}
    ]];
  ];

  addText["Pri Choleského rozklade rozložíme maticu A na súčin L \[CenterDot] Lᵀ. Potom namiesto jednej sústavy riešime dve jednoduchšie trojuholníkové sústavy."];
  addText["Budeme používať tieto vzťahy:"];

  AppendTo[content, Grid[
    {
      {Style[Row[{Style["A", Italic], " \[CenterDot] ", Style["x", Italic], " = ", Style["b", Italic]}], Bold]},
      {Style[Row[{Style["A", Italic], " = ", Style["L", Italic], " \[CenterDot] ", transposeLSymbol[]}], Bold]},
      {Style[Row[{Style["L", Italic], " \[CenterDot] ", Style["y", Italic], " = ", Style["b", Italic]}], Bold]},
      {Style[Row[{transposeLSymbol[], " \[CenterDot] ", Style["x", Italic], " = ", Style["y", Italic]}], Bold]}
    },
    Alignment -> Left,
    Spacings -> {1, 0.55},
    BaseStyle -> {FontSize -> 14}
  ]];

  choleskyData = choleskySolveData[A, b];

  If[choleskyData === $Failed,
    addHeader["Výsledok"];
    addText[Row[{"Pre túto maticu sa nepodarilo zostrojiť Choleského rozklad A = L \[CenterDot] ", transposeLSymbol[], "."}]];
    Return[<|"Content" -> content, "Solution" -> Missing["NotAvailable"]|>]
  ];

  L = ConstantArray[0, {n, n}];
  LT = ConstantArray[0, {n, n}];

  addHeader["Inicializácia matice L"];
  addText[Row[{"Maticu L budeme vypĺňať postupne po stĺpcoch. Z nej potom automaticky dostaneme transponovanú maticu ", transposeLSymbol[], "."}]];
  addMatrixPair[L, LT];

  Do[
    addHeader["Krok " <> ToString[i] <> " – výpočet " <> ToString[i] <> ". stĺpca matice L"];

    addSubHeader["Diagonálny prvok"];
    Scan[addFormula, buildCholeskyDiagonalLines[i, A, L, choleskyData["L"][[i, i]]]];

    L[[i, i]] = choleskyData["L"][[i, i]];

    If[i < n,
      addSubHeader["Prvky pod diagonálou"];
      Do[
        Scan[addFormula, buildCholeskyOffDiagonalLines[j, i, A, L, L[[i, i]], choleskyData["L"][[j, i]]]];
        L[[j, i]] = choleskyData["L"][[j, i]];
        If[j < n, addGap[content, 4]];
        ,
        {j, i + 1, n}
      ];
    ];

    LT = Transpose[L];
    addText["Po tomto kroku už poznáme tieto nové prvky matíc:"];
    addMatrixPair[L, LT, currentLBoldPositions[i], currentLTBoldPositions[i]];
    ,
    {i, 1, n}
  ];

  LT = Transpose[L];
  y = choleskyData["Y"];
  x = choleskyData["X"];

  addHeader[Row[{"Hotový rozklad A = L \[CenterDot] ", transposeLSymbol[]}]];
  addText[Row[{"Po výpočte máme dolnú trojuholníkovú maticu L a jej transponovanú maticu ", transposeLSymbol[], "."}]];
  addMatrixPair[L, LT, currentLBoldPositions[n], currentLTBoldPositions[n]];

  addHeader[Row[{"Overenie rozkladu L \[CenterDot] ", transposeLSymbol[], " = A"}]];
  addText[Row[{"Skontrolujeme, že súčin L \[CenterDot] ", transposeLSymbol[], " sa rovná matici A."}]];
  addText["Tip: Keď prejdete kurzorom nad prvky výslednej matice, zobrazí sa výpočet, z ktorého dané číslo vzniklo."];

  productCheck = Together[L . LT];

  appendProductDisplay[L, LT, productCheck];

  addHeader["Riešenie pomocnej sústavy L \[CenterDot] y = b"];
  addText["Najprv vyriešime pomocnú sústavu L \[CenterDot] y = b. Keďže L je dolná trojuholníková matica, použijeme dopredné dosadzovanie."];
  AppendTo[content, alignedAugmentedMatrix[augFromAb[L, b], {}, <|"LeftLabel" -> Style["L", Italic], "RightLabel" -> Style["b", Italic]|>]];

  tmp = appendTriangularSubstitutionSteps[
    L,
    b,
    ySymbols,
    ConstantArray[0, n],
    Range[n],
    content
  ];

  y = tmp[[1]];
  content = tmp[[2]];

  AppendTo[content, prettyVector[Style["y", Italic], y]];

  addHeader[Row[{"Riešenie sústavy ", transposeLSymbol[], " \[CenterDot] x = y"}]];
  addText[Row[{"Potom vyriešime sústavu ", transposeLSymbol[], " \[CenterDot] x = y. Keďže ", transposeLSymbol[], " je horná trojuholníková matica, použijeme spätné dosadzovanie."}]];
  AppendTo[content, alignedAugmentedMatrix[augFromAb[LT, y], {}, <|"LeftLabel" -> transposeLSymbol[], "RightLabel" -> Style["y", Italic]|>]];

  tmp = appendTriangularSubstitutionSteps[
    LT,
    y,
    vars,
    ConstantArray[0, n],
    Range[n, 1, -1],
    content
  ];

  x = tmp[[1]];
  content = tmp[[2]];

  addHeader["Skúška správnosti"];
  addText[Row[{"Skontrolujeme rozklad A = L \[CenterDot] ", transposeLSymbol[], ", pomocnú sústavu L \[CenterDot] y = b, sústavu ", transposeLSymbol[], " \[CenterDot] x = y aj pôvodnú sústavu A \[CenterDot] x = b."}]];

  lowerCheck = Together[L . y];
  upperCheck = Together[LT . x];

  appendMatrixEquality[Row[{Style["L", Italic], " \[CenterDot] ", transposeLSymbol[]}], productCheck, Style["A", Italic], A, productCheck === A];

  addGap[content, 6];
  appendVectorEquality[Row[{Style["L", Italic], " \[CenterDot] ", Style["y", Italic]}], lowerCheck, Style["b", Italic], b, lowerCheck === b];

  addGap[content, 6];
  appendVectorEquality[Row[{transposeLSymbol[], " \[CenterDot] ", Style["x", Italic]}], upperCheck, Style["y", Italic], y, upperCheck === y];

  addGap[content, 6];
  content = Join[content, verificationSteps[data, x]];

  <|
    "Content" -> content,
    "Solution" -> x,
    "L" -> L,
    "LT" -> LT,
    "Y" -> y
  |>
];

stepsCramer[data_Association] := Block[{cramerKnown3x3 = {}}, Module[{ content = {}, n, A, b, vars, taskFormat, solveData, detData, auxData, auxLabel, addHeader, addText},

  n = data["n"];
  A = data["A"];
  b = data["b"];
  vars = data["Vars"];
  taskFormat = Lookup[data, "TaskFormat", "EQUATIONS"];

  solveData = cramerSolveData[A, b];

  addHeader[text_] := appendStepHeader[content, text];
  addText[text_] := AppendTo[content, text];

  If[taskFormat =!= "MATRIX",
    addHeader["Prepis sústavy do maticového tvaru"];

    addText["Sústavu zapíšeme v tvare A \[CenterDot] x = b. Najprv vypočítame det(A). Ak det(A) nie je nula, môžeme použiť Cramerovo pravidlo."];

    AppendTo[content, Grid[
      {{
        labeledMatrixBlock[Style["A", Italic], styledPlainMatrix[A]],
        Style["\[CenterDot]", Bold, FontSize -> 18],
        labeledMatrixBlock[Style["x", Italic], styledPlainMatrix[List /@ vars]],
        Style["=", Bold, FontSize -> 18],
        labeledMatrixBlock[Style["b", Italic], styledPlainMatrix[List /@ b]]
      }},
      Alignment -> {Center, Center, Center, Center, Center},
      Spacings -> {1.2, 0}
    ]];
  ];

  addHeader["Výpočet det(A)"];
  detData = renderCramerDeterminant[A, Style["A", Italic]];
  content = Join[content, detData["Content"]];

  If[solveData["DetA"] === 0,
    addHeader["Záver"];
    addText["Keďže det(A) = 0, Cramerovo pravidlo nemožno použiť."];
    Return[<|
      "Content" -> content, "Solution" -> Missing["NotAvailable"], "DetA" -> solveData["DetA"], "AuxDeterminants" -> {}
    |>];
  ];

  Do[
    auxLabel = cramerMatrixLabel[vars[[i]]];

    addHeader["Pomocná matica pre premennú " <> ToString[vars[[i]], InputForm]];
    addText["V pomocnej matici nahradíme príslušný stĺpec matice A vektorom pravej strany b."];
    AppendTo[
      content,
      cramerAuxiliaryMatrixPanel[
        A,
        solveData["AuxMatrices"][[i]],
        i,
        auxLabel
      ]
    ];

    auxData = renderCramerDeterminant[solveData["AuxMatrices"][[i]], auxLabel];
    content = Join[content, auxData["Content"]];

    addGap[content, 3];

    AppendTo[content, Row[
      {
        lhsStyle[tf[vars[[i]]]],
        " = ",
        Row[{cramerDetLabel[auxLabel], " / ", cramerDetLabel[Style["A", Italic]]}],
        " = ",
        Row[{tft[solveData["AuxDeterminants"][[i]]], " / ", tft[solveData["DetA"]]}],
        " = ",
        tft[solveData["Solution"][[i]]]
      }
    ]];


    AppendTo[
      content,
      highlightResultEquation[
        vars[[i]],
        solveData["Solution"][[i]]
      ]
    ];
    ,
    {i, 1, n}
  ];

  addHeader["Skúška správnosti"];
  addText["Porovnáme A \[CenterDot] x s pravou stranou b po riadkoch."];
  content = Join[content, verificationSteps[data, solveData["Solution"]]];
  <|
    "Content" -> content,
    "Solution" -> solveData["Solution"],
    "DetA" -> solveData["DetA"],
    "AuxDeterminants" -> solveData["AuxDeterminants"]
  |>
]];

stepsEquationCore[config_Association, A_, b_, vars_, data_ : <||>] := Module[
  {content = {}, kind, eqs, varsNow, stack = {}, step, lastSolve, solMap, back, solVec, origVars = vars, item, result},

  kind = Lookup[data, "type", "ONE"];

  (* hard normalizácia len pre hard 3x3 *)
  If[TrueQ[data["HardQ"]] && Length[vars] === 3,
    content = Join[content, hardNormalizationSteps3[A, b, vars, data]]
  ];

  (* počiatočný zápis rovníc *)
  eqs = MapThread[List, {A, b}];
  varsNow = vars;

  (* redukcia, kým nezostane jedna neznáma *)
  While[Length[varsNow] > 1,
    step = config["ReduceOnceFn"][eqs, varsNow];
    If[step === $Failed, Return[$Failed]];

    content = Join[content, step["Content"]];

    If[AnyTrue[step["Classes"], # === "CONTRADICTION" &],
      AppendTo[content, config["ReduceNoneText"]];
      appendEquationVerification[content, A, b, origVars, "NONE"];
      Return[<|"Content" -> content, "Solution" -> "NONE"|>]
    ];

    If[AllTrue[step["Classes"], # === "IDENTITY" &],
      Module[{infData},

        AppendTo[content, config["ReduceInfiniteText"]];

        infData = appendEquationInfiniteParametrization[content, config, A, b, origVars, eqs, varsNow, stack];

        If[infData === $Failed,
          Return[$Failed]
        ];

        appendStepHeader[content, "Skúška správnosti"];
        AppendTo[
          content,
          "Dosadíme parametrické riešenie do pôvodných rovníc. V každom riadku musí vyjsť identita pre ľubovoľné \[FormalT] \[Element] \[DoubleStruckCapitalR]."
        ];

        content = Join[
          content,
          verificationStepsEquationInfinite[A, b, origVars, infData["Solution"]]
        ];

        Return[
          <|
            "Content" -> content,
            "Solution" -> "INFINITE",
            "ParametricSolution" -> infData["Solution"]
          |>
        ]
      ]
    ];

    AppendTo[stack, config["StackItemFn"][step, varsNow]];
    eqs = step["NewEqs"];
    varsNow = step["NewVars"];
  ];

  (* riešenie poslednej rovnice *)
  lastSolve = solveOneVarEquationSteps[First[eqs], varsNow];

  If[lastSolve["Type"] =!= "ONE",
    result = If[lastSolve["Type"] === "NONE", "NONE", "INFINITE"];

    AppendTo[
      content,
      If[result === "NONE", config["FinalNoneText"], config["FinalInfiniteText"]]
    ];

    If[result === "INFINITE",
      Module[{paramSymbol = \[FormalT], solMap, solExprs, item, back, backVar},

        appendStepHeader[content, "Vyjadrenie riešenia pomocou parametra"];

        AppendTo[
          content,
          Row[{
            "Voľnú premennú zvolíme ",
            tf[varsNow[[1]]],
            " = ",
            TraditionalForm[paramSymbol],
            "."
          }]
        ];

        AppendTo[
          content,
          highlightResultEquation[varsNow[[1]], paramSymbol]
        ];

        solMap = <|varsNow[[1]] -> paramSymbol|>;

        Do[
          item = stack[[k]];
          backVar = config["BackVarFn"][item];

          AppendTo[
            content,
            Style[
              "Dopočítame premennú " <> ToString[backVar] <> " dosadením do vhodnej pivotnej rovnice:",
              Italic
            ]
          ];

          back = config["BackFn"][item, solMap];

          If[Lookup[back, "Type", "ONE"] =!= "ONE",
            Return[$Failed]
          ];

          AppendTo[content, alignedEquations[back["Steps"]]];
          AppendTo[
            content,
            highlightGrid[
              alignedEquations[
                {{
                  backVar,
                  tf[back["Value"]],
                  ""
                }}
              ]
            ]
          ];

          solMap[backVar] = back["Value"],
          {k, Length[stack], 1, -1}
        ];

        solExprs = Together /@ (solMap /@ origVars);

        appendStepHeader[content, "Skúška správnosti"];
        AppendTo[
          content,
          "Dosadíme parametrické riešenie do pôvodných rovníc. V každom riadku musí vyjsť identita pre ľubovoľné \[FormalT] \[Element] \[DoubleStruckCapitalR]."
        ];

        content = Join[
          content,
          verificationStepsEquationInfinite[
            A,
            b,
            origVars,
            solExprs
          ]
        ];

        Return[
          <|
            "Content" -> content,
            "Solution" -> "INFINITE",
            "ParametricSolution" -> solExprs
          |>
        ]
      ]
    ];

    appendEquationVerification[content, A, b, origVars, result];
    Return[<|"Content" -> content, "Solution" -> result|>]
  ];

  AppendTo[
    content,
    highlightResultEquation[varsNow[[1]], lastSolve["Value"]]
  ];

  solMap = <|varsNow[[1]] -> lastSolve["Value"]|>;

  (* spätné dopočítanie *)
  Do[
    item = stack[[k]];
    appendStepHeader[content, config["BackHeader"]];

    back = config["BackFn"][item, solMap];

    If[Lookup[back, "Type", "ONE"] =!= "ONE",
      result = If[back["Type"] === "NONE", "NONE", "INFINITE"];

      If[result === "INFINITE",
        Return[$Failed]
      ];

      appendEquationVerification[content, A, b, origVars, result];
      Return[<|"Content" -> content, "Solution" -> result|>]
    ];

    AppendTo[content, alignedEquations[back["Steps"]]];
    AppendTo[
      content,
      highlightResultEquation[config["BackVarFn"][item], back["Value"]]
    ];
    solMap[config["BackVarFn"][item]] = back["Value"],
    {k, Length[stack], 1, -1}
  ];

  solVec = solMap /@ origVars;

  If[kind === "ONE",
    appendEquationVerification[content, A, b, origVars, solVec];
    Return[<|"Content" -> content, "Solution" -> solVec|>]
  ];

  If[kind === "NONE",
    appendEquationVerification[content, A, b, origVars, "NONE"];
    Return[<|"Content" -> content, "Solution" -> "NONE"|>]
  ];

  If[kind === "INFINITE",
    Return[$Failed]
  ];

  <|"Content" -> content, "Solution" -> solVec|>
];

stepsElimination[A_, b_, vars_, data_ : <||>] := stepsEquationCore[
  <|
    "ReduceOnceFn" -> reduceOnceByElimination,
    "ReduceNoneText" -> "Počas eliminácie sme dostali nepravdivú rovnosť, teda spor. Sústava preto nemá riešenie.",
    "ReduceInfiniteText" -> "Počas eliminácie sme dostali identitu, teda vždy pravdivú rovnosť. Jedna neznáma zostáva voľná, preto má sústava nekonečne veľa riešení.",
    "FinalNoneText" -> "V poslednej rovnici sme dostali spor, preto sústava nemá riešenie.",
    "FinalInfiniteText" -> "V poslednej rovnici sme dostali identitu, preto má sústava nekonečne veľa riešení.",
    "BackHeader" -> "Spätné dosadenie do vhodnej rovnice",
    "StackItemFn" -> Function[{step, varsNow},
      <|"PivotEq" -> step["PivotEq"], "VarsBefore" -> varsNow, "ElimVar" -> step["ElimVar"]|>
    ],
    "BackVarFn" -> Function[item, item["ElimVar"]],
    "BackFn" -> Function[{item, solMap},
      backSubstituteVariableSteps[item["PivotEq"], item["VarsBefore"], solMap, item["ElimVar"]]
    ]
  |>,
  A,
  b,
  vars,
  data
];

stepsSubstitution[A_, b_, vars_, data_ : <||>] := stepsEquationCore[
  <|
    "ReduceOnceFn" -> reduceOnceBySubstitution,
    "ReduceNoneText" -> "Počas dosadzovania sme dostali nepravdivú rovnosť, teda spor. Sústava preto nemá riešenie.",
    "ReduceInfiniteText" -> "Počas dosadzovania sme dostali identitu, teda vždy pravdivú rovnosť. Jedna neznáma zostáva voľná, preto má sústava nekonečne veľa riešení.",
    "FinalNoneText" -> "V poslednej rovnici sme dostali spor, preto sústava nemá riešenie.",
    "FinalInfiniteText" -> "V poslednej rovnici sme dostali identitu, preto má sústava nekonečne veľa riešení.",
    "BackHeader" -> "Spätné dosadenie",
    "StackItemFn" -> Function[{step, varsNow},
      <|
        "SolvedVar" -> step["SolvedVar"],
        "Expr" -> step["RuleExpr"],
        "VarsBefore" -> varsNow
      |>
    ],
    "BackVarFn" -> Function[item, item["SolvedVar"]],
    "BackFn" -> Function[{item, solMap},
      backSubstituteVariableSteps[item["SolvedVar"], item["Expr"], item["VarsBefore"], solMap]
    ]
  |>, A, b, vars, data
];
(* ~-~-~ VERIFICATION STEPS ~-~-~ *)

verificationSteps[data_Association, sol_List] := Module[{ content = {}, A = data["A"], b = data["b"], n = data["n"], lhs},
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
verificationStepsNone[data_Association] := Module[{ content = {}, A = data["A"], b = data["b"], aug0, rA, rAug, n},

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
verificationStepsInfinite[data_Association, solExprs_List] := Module[{ content = {}, A = data["A"], b = data["b"], n = data["n"], lhs, diff, okQ, coeffs},

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

(* ~-~-~ TASK / RESULT PRINTING ~-~-~ *)

printTextBlock[text_] := Module[{ }, If[StringQ[text], printTextCell[text], printCellStyle[BoxData @ ToBoxes[text, StandardForm], "Text"]]];

matrixTaskEquation[A_, b_, xSymbols_] := Module[{prettyMatrix, prettyVector},
  prettyMatrix[label_, mat_] := labeledMatrixBlock[label, styledPlainMatrix[mat]];
  prettyVector[label_, vec_List] := labeledMatrixBlock[label, styledPlainMatrix[List /@ vec]];

  Grid[
    {{
      prettyMatrix[Style["A", Italic], A],
      Style["\[CenterDot]", Bold, FontSize -> 18],
      prettyVector[Style["x", Italic], xSymbols],
      Style["=", Bold, FontSize -> 18],
      prettyVector[Style["b", Italic], b]
    }},
    Alignment -> {Center, Center, Center, Center, Center},
    Spacings -> {1.2, 0}
  ]
];

printTaskBlock[data_Association, vars_List, text_, taskFormat_ : "EQUATIONS"] := Module[{A, b},
  A = data["A"];
  b = data["b"];

  printTextBlock[text];

  If[taskFormat === "MATRIX",
    printFormulaCell @ matrixTaskEquation[A, b, vars],
    printFormulaCell @ alignedTaskEquations[A, b, vars]
  ];
];

solutionRow[vars_List, solution_List] := Row[Flatten[{"(", Riffle[vars, ", "], ") = (", Riffle[TraditionalForm /@ solution, ", "], ")"}]];

printResultBlock[text_, expr_] := Module[{ }, printTextBlock[text]; printFormulaCell[expr]];

printDefaultResult[result_, vars_, paramSol_: Missing["NotAvailable"]] := Which[
  ListQ[result],
  printResultBlock["Riešenie sústavy:", solutionRow[vars, result]],

  result === "NONE",
  printTextCell["Sústava nemá riešenie."];
  printFormulaCell[Row[{"K = ", "\[EmptySet]"}]],

  result === "INFINITE",
  printTextCell["Sústava má nekonečne veľa riešení."];
  If[ListQ[paramSol],
    printFormulaCell[
      Row[{
        "K = { [",
        Row @ Riffle[TraditionalForm /@ paramSol, ", "],
        "], ",
        \[FormalT],
        " \[Element] ",
        Integers,
        " }"
      }]
    ]
  ],

  True,
  printTextCell["Výsledok sa nepodarilo jednoznačne určiť."]
];

printResultInverse[data_Association, vars_List, st_, steps_] := Module[{stepData, solution, invMatrix},
  stepData = If[AssociationQ[steps], steps, <||>];

  solution = Lookup[stepData, "Solution", Lookup[data, "x", Missing["NotAvailable"]]];
  invMatrix = Lookup[stepData, "InverseMatrix", Missing["NotAvailable"]];

  If[MatrixQ[invMatrix], printResultBlock["Inverzná matica:", TraditionalForm[MatrixForm[invMatrix]]]];

  If[ListQ[solution], printResultBlock["Riešenie sústavy:", solutionRow[vars, solution]]]];

printResultLU[data_Association, vars_List, st_, steps_] := Module[{stepData, solution, lMatrix, uMatrix, yVector},
  stepData = If[AssociationQ[steps], steps, <||>];

  solution = Lookup[stepData, "Solution", Lookup[data, "x", Missing["NotAvailable"]]];
  lMatrix = Lookup[stepData, "L", Missing["NotAvailable"]];
  uMatrix = Lookup[stepData, "U", Missing["NotAvailable"]];
  yVector = Lookup[stepData, "Y", Missing["NotAvailable"]];

  If[MatrixQ[lMatrix], printResultBlock["Matica L:", TraditionalForm[MatrixForm[lMatrix]]]];

  If[MatrixQ[uMatrix], printResultBlock["Matica U:", TraditionalForm[MatrixForm[uMatrix]]]];

  If[ListQ[yVector], printResultBlock["Pomocný vektor y:", TraditionalForm[MatrixForm[yVector]]]];

  If[ListQ[solution], printResultBlock["Riešenie sústavy:", solutionRow[vars, solution]]]];

printResultCholesky[data_Association, vars_List, st_, steps_] := Module[{stepData, solution, lMatrix, ltMatrix, yVector},
  stepData = If[AssociationQ[steps], steps, <||>];

  solution = Lookup[stepData, "Solution", Lookup[data, "x", Missing["NotAvailable"]]];
  lMatrix = Lookup[stepData, "L", Missing["NotAvailable"]];
  ltMatrix = If[MatrixQ[lMatrix], Transpose[lMatrix], Missing["NotAvailable"]];
  yVector = Lookup[stepData, "Y", Missing["NotAvailable"]];

  If[MatrixQ[lMatrix], printResultBlock["Matica L:", TraditionalForm[MatrixForm[lMatrix]]]];

  If[MatrixQ[ltMatrix], printResultBlock[Row[{"Matica ", transposeLSymbol[], ":"}], TraditionalForm[MatrixForm[ltMatrix]]]];

  If[ListQ[yVector], printResultBlock["Pomocný vektor y:", TraditionalForm[MatrixForm[yVector]]]];

  If[ListQ[solution], printResultBlock["Riešenie sústavy:", solutionRow[vars, solution]]]];

printResultCramer[data_Association, vars_List, st_, steps_] := Module[{stepData, solveData, detA, auxDeterminants, solution},
  stepData = If[AssociationQ[steps], steps, <||>];
  solveData = cramerSolveData[data["A"], data["b"]];

  detA = Lookup[stepData, "DetA", solveData["DetA"]];
  auxDeterminants = Lookup[stepData, "AuxDeterminants", solveData["AuxDeterminants"]];
  solution = Lookup[stepData, "Solution", solveData["Solution"]];

  printResultBlock["Determinant matice A:", plainEquationLine[cramerDetLabel[Style["A", Italic]], detA]];

  printResultBlock["Pomocné determinanty:",
    Grid[
      Table[
        {plainEquationLine[
          cramerDetLabel[cramerMatrixLabel[vars[[i]]]],
          auxDeterminants[[i]]]},
        {i, 1, Length[vars]}],
      Alignment -> Left,
      Spacings -> {0, 0.8}]];

  If[ListQ[solution], printResultBlock["Riešenie sústavy:", solutionRow[vars, solution]]]];

printEquationTaskBlock[data_Association, vars_List, taskText_] := Module[{A, b, n},
  A = data["A"];
  b = data["b"];
  n = Length[b];

  printTextBlock[taskText];

  (* hard zadanie vypiseme v nenormalizovanom tvare *)
  If[TrueQ[Lookup[data, "HardQ", False]] && KeyExistsQ[data, "EqDisplay"],
    printFormulaCell @ alignedEquations[data["EqDisplay"]];
    Return[]
  ];

  printFormulaCell @ alignedEquations[
    Table[
      {renderTermsRow[Transpose[{A[[i]], vars}]], b[[i]], ""},
      {i, 1, n}
    ]
  ];
];

(* ~-~-~ MAIN CONTROLLER ~-~-~ *)

runMatrixGenerator[spec_Association, diff_String, mode_String, opts : OptionsPattern[]] := Module[{ n, vars, st, tri, data, steps = Missing["NotComputed"], validateExtraQ, resolveExtra,
  sectionTitle, stepFn, scrambleFn, taskPrinter, resultPrinter, useRetryQ, pivotMode, boundAugFn, boundCheckFn, taskText, taskFormat},

  If[!TrueQ[ValidateDifficulty[diff]],
    With[{msg = spec["MsgPrefix"]}, Message[msg::baddiff, diff]];
    Return[]
  ];

  If[!TrueQ[ValidateMode[mode]],
    With[{msg = spec["MsgPrefix"]}, Message[msg::badmode, mode]];
    Return[]
  ];

  With[{stOpt = OptionValue[spec["EntryFn"], {opts}, SolutionType]},
    If[!TrueQ[ValidateSolutionType[stOpt]],
      With[{msg = spec["MsgPrefix"]}, Message[msg::badst, stOpt]];
      Return[]
    ];
  ];

  taskFormat = OptionValue[spec["EntryFn"], {opts}, TaskFormat];

  If[!TrueQ[ValidateTaskFormat[taskFormat]],
    With[{msg = spec["MsgPrefix"]}, Message[msg::badtf, taskFormat]];
    Return[]
  ];

  validateExtraQ = Lookup[spec, "ValidateExtra", (True &)];
  If[!TrueQ[validateExtraQ[spec, {opts}]],
    Return[]
  ];

  st = ResolveSolutionType[OptionValue[spec["EntryFn"], {opts}, SolutionType]];

  resolveExtra = Lookup[spec, "ResolveExtra", (Missing["NotUsed"] &)];
  tri = resolveExtra[spec, {opts}];

  n = DimensionByMethodDifficulty[spec["DimKey"], diff];
  vars = buildMatrixVars[n];

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

  data = Association[data, "TaskFormat" -> taskFormat];

  sectionTitle = spec["SectionTitle"];
  printSectionCell[sectionTitle];
  printSubsectionCell["Zadanie"];

  taskPrinter = Lookup[spec, "TaskPrinter", Automatic];
  taskText = Lookup[spec, "TaskText", "Riešte sústavu rovníc."];

  If[taskPrinter === Automatic, printTaskBlock[data, vars, taskText, taskFormat], taskPrinter[data, vars]];

  (*If[KeyExistsQ[data, "RetryCount"],*)
    (*printTextCell["Počet pregenerovaní: " <> ToString[data["RetryCount"]]];*)
  (*];*)

  If[mode === "TASK_STEPS_RESULT",
    withStepCounter @ Function[Null,
      printSubsectionCell["Postup"];
      stepFn = Lookup[spec, "StepsFn", None];

      If[stepFn === None,
        printTextCell["Postup pre túto metódu nie je vytvorený."],
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
        printDefaultResult[
          If[st === "ONE", data["x"], st],
          vars,
          If[st === "INFINITE", infiniteSolutionFromSolvedAug[data], Missing["NotAvailable"]]
        ],
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

runEquationGenerator[spec_Association, diff_String, mode_String, opts___?OptionQ] := Module[
  {n, vars, stOpt, st, data, steps = Missing["NotComputed"], stepFn, result,
    entryFn, msgPrefix, visQ},

  entryFn = spec["EntryFn"];
  msgPrefix = spec["MsgPrefix"];

  If[!TrueQ[ValidateDifficulty[diff]],
    With[{msg = msgPrefix}, Message[msg::baddiff, diff]];
    Return[]
  ];

  If[!TrueQ[ValidateMode[mode]],
    With[{msg = msgPrefix}, Message[msg::badmode, mode]];
    Return[]
  ];

  stOpt = With[{fn = entryFn}, OptionValue[fn, {opts}, SolutionType]];

  If[!TrueQ[ValidateSolutionType[stOpt]],
    With[{msg = msgPrefix}, Message[msg::badst, stOpt]];
    Return[]
  ];

  st = ResolveSolutionType[stOpt];

  n = DimensionByMethodDifficulty[spec["DimKey"], diff];
  vars = buildEquationVars[n];

  data = generateEquationDataWithBounds[
    n,
    diff,
    st,
    vars,
    spec["ScrambleFn"]
  ];

  If[data === $Failed,
    With[{msg = msgPrefix}, Message[msg::fail]];
    Return[]
  ];

  printSectionCell[spec["SectionTitle"]];
  printSubsectionCell["Zadanie"];

  printEquationTaskBlock[
    data,
    vars,
    spec["TaskText"]
  ];

  If[mode =!= "TASK",
    steps = withStepCounter @ Function[Null,
      stepFn = spec["StepsFn"];
      stepFn[data["A"], data["b"], vars, data]
    ];

    If[steps === $Failed,
      With[{msg = msgPrefix}, Message[msg::fail]];
      Return[]
    ];
  ];

  If[mode === "TASK_STEPS_RESULT",
    printSubsectionCell["Postup"];
    Scan[renderStepItem, steps["Content"]];
  ];

  If[mode =!= "TASK",
    printSubsectionCell["Výsledok"];

    result = steps["Solution"];

    printDefaultResult[
      result, vars, Lookup[steps, "ParametricSolution", Missing["NotAvailable"]]
    ];

    visQ = TrueQ @ With[{fn = entryFn}, OptionValue[fn, {opts}, Visualization]];

    If[visQ,
      printSubsectionCell["Vizualizácia"];

      If[n === 2,
        visualize2[data["A"], data["b"], vars, result],
        visualize3[data["A"], data["b"], vars, result]
      ];
    ];
  ];

  Null
];

GenTriangular[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenTriangular,
    "MsgPrefix" -> GenTriangular,
    "DimKey" -> "Triangular",
    "SectionTitle" -> "Trojuholníková metóda",
    "ScrambleFn" -> genScrambleTriang,
    "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"]
  |>;

  runMatrixGenerator[spec, diff, mode, opts]
];

GenGauss[diff_String, mode_String, opts : OptionsPattern[]] := Module[{ spec},
  spec = <|
    "EntryFn" -> GenGauss,
    "MsgPrefix" -> GenGauss,
    "DimKey" -> "Gauss",
    "SectionTitle" -> "Gaussova eliminačná metóda",
    "TaskText" -> "Riešte sústavu rovníc pomocou Gaussovej eliminačnej metódy.",
    "ScrambleFn" -> genScrambleGauss,
    "StepsFn" -> stepsGauss,
    "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "UseForwardBoundRetry" -> True,
    "ForwardPivotMode" -> "ZERO"
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

GenGaussJordan[diff_String, mode_String, opts : OptionsPattern[]] := Module[{ spec},
  spec = <|
    "EntryFn" -> GenGaussJordan,
    "MsgPrefix" -> GenGaussJordan,
    "DimKey" -> "GaussJordan",
    "SectionTitle" -> "Gauss-Jordanova metóda",
    "TaskText" -> "Riešte sústavu rovníc pomocou Gauss-Jordanovej metódy.",
    "ScrambleFn" -> genScrambleGauss,
    "StepsFn" -> (stepsGaussJordanShared[#, False, False] &),
    "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "UseForwardBoundRetry" -> True,
    "ForwardPivotMode" -> "ZERO"
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

GenGaussJordanPivot[diff_String, mode_String, opts : OptionsPattern[]] := Module[{ spec},
  spec = <|
    "EntryFn" -> GenGaussJordanPivot,
    "MsgPrefix" -> GenGaussJordanPivot,
    "DimKey" -> "GaussJordanPivot",
    "SectionTitle" -> "Gauss-Jordanova metóda s pivotovaním",
    "TaskText" -> "Riešte sústavu rovníc pomocou Gauss-Jordanovej metódy s pivotovaním.",
    "ScrambleFn" -> genScrambleGaussJordanPivot,
    "StepsFn" -> (stepsGaussJordanShared[#, True, False] &),
    "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "UseForwardBoundRetry" -> True,
    "ForwardPivotMode" -> "MIN"
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

GenElemGJ[diff_String, mode_String, opts : OptionsPattern[]] := Module[{ spec},
  spec = <|
    "EntryFn" -> GenElemGJ,
    "MsgPrefix" -> GenElemGJ,
    "DimKey" -> "ElemGaussJordan",
    "SectionTitle" -> "Gauss-Jordanova metóda pomocou elementárnych matíc",
    "TaskText" -> "Riešte sústavu rovníc pomocou Gauss-Jordanovej metódy s elementárnymi maticami.",
    "ScrambleFn" -> genScrambleGauss,
    "StepsFn" -> (stepsGaussJordanShared[#, False, True] &),
    "ValidateExtra" -> validateOnlyOneSolutionType,
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "UseForwardBoundRetry" -> True,
    "ForwardPivotMode" -> "ZERO"
  |>;

  runMatrixGenerator[spec, diff, mode, opts]
];

GenInverse[diff_String, mode_String, opts : OptionsPattern[]] := Module[{ spec},
  spec = <|
    "EntryFn" -> GenInverse,
    "MsgPrefix" -> GenInverse,
    "DimKey" -> "Inverse",
    "SectionTitle" -> "Výpočet inverznej matice",
    "ScrambleFn" -> genScrambleGauss,
    "StepsFn" -> stepsInverseMatrix,
    "ValidateExtra" -> validateOnlyOneSolutionType,
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "TaskText" -> "Vypočítajte inverznú maticu a následne pomocou nej určte riešenie sústavy.",
    "ResultPrinter" -> printResultInverse,
    "UseForwardBoundRetry" -> True,
    "ForwardPivotMode" -> "MIN",
    "ForwardBoundAugFn" -> Function[data, Join[data["A"], IdentityMatrix[data["n"]], 2]],
    "ForwardBoundCheckFn" -> gaussJordanEliminationWithinBoundsQ
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

GenLU[diff_String, mode_String, opts : OptionsPattern[]] := Module[{ spec},
  spec = <|
    "EntryFn" -> GenLU,
    "MsgPrefix" -> GenLU,
    "DimKey" -> "LU",
    "SectionTitle" -> "LU rozklad – Doolittleova metóda",
    "ScrambleFn" -> genScrambleLU,
    "StepsFn" -> stepsLU,
    "ValidateExtra" -> validateOnlyOneSolutionType,
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "TaskText" -> "Rozložte maticu sústavy Doolittleovou metódou v tvare A = L \[CenterDot] U, kde L má jednotky na diagonále. Potom vyriešte sústavy L \[CenterDot] y = b a U \[CenterDot] x = y.",
    "ResultPrinter" -> printResultLU,
    "UseForwardBoundRetry" -> True,
    "ForwardBoundAugFn" -> Function[data, data],
    "ForwardBoundCheckFn" -> Function[{data, pivotMode}, luDecompositionWithinBoundsQ[data]]
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

GenCholesky[diff_String, mode_String, opts : OptionsPattern[]] := Module[{ spec},
  spec = <|
    "EntryFn" -> GenCholesky,
    "MsgPrefix" -> GenCholesky,
    "DimKey" -> "Cholesky",
    "SectionTitle" -> "Choleského rozklad",
    "ScrambleFn" -> genScrambleCholesky,
    "StepsFn" -> stepsCholesky,
    "ValidateExtra" -> validateOnlyOneSolutionType,
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "TaskText" -> Row[{"Rozložte maticu sústavy pomocou Choleského rozkladu v tvare A = L \[CenterDot] ", transposeLSymbol[], ". Potom vyriešte sústavy L \[CenterDot] y = b a ", transposeLSymbol[], " \[CenterDot] x = y."}],
    "ResultPrinter" -> printResultCholesky,
    "UseForwardBoundRetry" -> True,
    "ForwardBoundAugFn" -> Function[data, data],
    "ForwardBoundCheckFn" -> Function[{data, pivotMode}, choleskyDecompositionWithinBoundsQ[data]]
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
    "ValidateExtra" -> validateOnlyOneSolutionType,
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "TaskText" -> "Riešte sústavu rovníc pomocou Cramerovho pravidla.",
    "ResultPrinter" -> printResultCramer
  |>;

  runMatrixGenerator[spec, diff, mode, opts]
];

GenElimination[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenElimination,
    "MsgPrefix" -> GenElimination,
    "DimKey" -> "Elimination",
    "SectionTitle" -> "Eliminačná (sčítacia) metóda",
    "TaskText" -> "Riešte sústavu rovníc eliminačnou metódou (sčítaním rovníc).",
    "ScrambleFn" -> genScrambleElimination,
    "StepsFn" -> stepsElimination
  |>;

  runEquationGenerator[spec, diff, mode, opts]
];

GenSubstitution[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenSubstitution,
    "MsgPrefix" -> GenSubstitution,
    "DimKey" -> "Substitution",
    "SectionTitle" -> "Dosadzovacia (substitučná) metóda",
    "TaskText" -> "Riešte sústavu rovníc dosadzovacou (substitučnou) metódou.",
    "ScrambleFn" -> genScrambleSubstitution,
    "StepsFn" -> stepsSubstitution
  |>;

  runEquationGenerator[spec, diff, mode, opts]
];

End[];
EndPackage[];
