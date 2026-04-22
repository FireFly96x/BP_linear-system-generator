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
diff: \"EASY\" (3x3), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> \"ONE\"";

GenCramer::usage = "GenCramer[diff, mode, opts] vygeneruje didaktický príklad riešenia sústavy lineárnych rovníc pomocou Cramerovho pravidla.
diff: \"EASY\" (3x3), \"MEDIUM\" (4x4), \"HARD\" (5x5)
mode: \"TASK\" | \"TASK_RESULT\" | \"TASK_STEPS_RESULT\"
opts: SolutionType -> \"ONE\"";

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
GenElemGJ::baddiff = GenTriangular::baddiff;
GenElemGJ::badmode = GenTriangular::badmode;
GenElemGJ::badst   = "Pre túto metódu je povolené len SolutionType -> \"ONE\".";
GenElemGJ::fail    = GenTriangular::fail;
GenInverse::baddiff = GenTriangular::baddiff;
GenInverse::badmode = GenTriangular::badmode;
GenInverse::badst = "Pre túto metódu je povolené len SolutionType -> \"ONE\".";
GenInverse::fail = "Nepodarilo sa vygenerovať regulárnu maticu pre výpočet inverznej matice.";
GenLU::baddiff = GenTriangular::baddiff;
GenLU::badmode = GenTriangular::badmode;
GenLU::badst   = "Pre túto metódu je povolené len SolutionType -> \"ONE\".";
GenLU::fail    = "Nepodarilo sa vygenerovať sústavu vhodnú pre LU rozklad bez pivotovania.";
GenCholesky::baddiff = GenTriangular::baddiff;
GenCholesky::badmode = GenTriangular::badmode;
GenCholesky::badst   = "Pre túto metódu je povolené len SolutionType -> \"ONE\".";
GenCholesky::fail    = "Nepodarilo sa vygenerovať symetrickú kladne definitnú sústavu vhodnú pre Choleského rozklad.";
GenCramer::baddiff = GenTriangular::baddiff;
GenCramer::badmode = GenTriangular::badmode;
GenCramer::badst   = "Pre túto metódu je povolené len SolutionType -> \"ONE\".";
GenCramer::fail    = "Nepodarilo sa vygenerovať regulárnu sústavu vhodnú pre Cramerovo pravidlo.";

$CommonGeneratorOptions = {SolutionType -> Automatic, TriangularType -> Automatic};

Options[GenTriangular] = $CommonGeneratorOptions;
Options[GenGauss] = $CommonGeneratorOptions;
Options[GenGaussJordan] = $CommonGeneratorOptions;
Options[GenGaussJordanPivot] = $CommonGeneratorOptions;
Options[GenElemGJ] = {SolutionType -> "ONE"};
Options[GenInverse] = {SolutionType -> "ONE"};
Options[GenLU] = {SolutionType -> "ONE"};
Options[GenCholesky] = {SolutionType -> "ONE"};
Options[GenCramer] = {SolutionType -> "ONE"};

$ElemStepCounter = 0;
$ElemMatrixCounter = 0;

Begin["`Private`"];

(* ~-~-~ VALIDATION ~-~-~ *)

DimensionByDifficulty[diff_String] := Switch[diff, "EASY", 3, "MEDIUM", 5, "HARD", 6];

DimensionByMethodDifficulty[dimKey_String, diff_String] := Switch[
  dimKey,

  "Triangular" | "Gauss" | "GaussJordan" | "GaussJordanPivot",
  Switch[diff, "EASY", 3, "MEDIUM", 5, "HARD", 6],

  "Inverse" | "LU" | "Cholesky",
  Switch[diff, "EASY", 3, "MEDIUM", 4, "HARD", 6],

  "ElemGaussJordan" |  "Cramer", (*Cramer sa nemôže meniť - ma fixne kroky ku rozmerom*)
  Switch[diff, "EASY", 3, "MEDIUM", 4, "HARD", 5],

  _,
  DimensionByDifficulty[diff]
];

ValidateDifficulty[diff_] := MemberQ[{"EASY", "MEDIUM", "HARD"}, diff];
ValidateMode[mode_] := MemberQ[{"TASK", "TASK_RESULT", "TASK_STEPS_RESULT"}, mode];
ValidateSolutionType[st_] := TrueQ[st === Automatic] || MemberQ[{"ONE", "NONE", "INFINITE"}, st];
ResolveSolutionType[st_] := If[st =!= Automatic, st, RandomChoice[{0.8, 0.1, 0.1} -> {"ONE", "NONE", "INFINITE"}]];
validateTriangularType[tri_] := TrueQ[tri === Automatic] || MemberQ[{"L", "U"}, tri];
resolveTriangularType[tri_] := If[tri === Automatic, RandomChoice[{"L", "U"}], tri];

(* validácia metód, ktoré povoľujú len jedno riešenie *)
validateOnlyOneSolutionType[specLocal_, passedOpts_] := With[
  {stOpt = OptionValue[specLocal["EntryFn"], passedOpts, SolutionType]},
  If[stOpt =!= "ONE",
    Message[MessageName[specLocal["MsgPrefix"], "badst"], stOpt];
    False, True
  ]
];

(* ~-~-~ CELL PRINTING ~-~-~ *)

inNotebookQ[] := Head @ Quiet[EvaluationNotebook[]] === NotebookObject;
printCellStyle[expr_, style_String] := If[inNotebookQ[], CellPrint @ Cell[expr, style, ShowStringCharacters -> False], Print[expr]];
printTextCell[str_String] := printCellStyle[str, "Text"];
printSectionCell[str_String] := printCellStyle[str, "Section"];
printSubsectionCell[str_String] := printCellStyle[str, "Subsection"];
printFormulaCell[expr_] := Module[{boxes}, boxes = Quiet @ Check[BoxData @ ToBoxes[expr, StandardForm], expr]; printCellStyle[boxes, "DisplayFormula"]];

highlightGrid[grid_] := Style[grid, Background -> RGBColor[0.95, 0.95, 0.95], Frame -> True, FrameStyle -> None, FrameMargins -> 5];
tf[val_] := TraditionalForm[val];
tft[val_] := tf[Together[val]];

(* symbol ekvivalentnej riadkovej úpravy *)
rowEquivalentSymbol[] := Style["\[TildeTilde]", Bold, FontSize -> 18];

(* základné zvýraznenie ľavej strany rovnosti *)
lhsStyle[expr_] := Style[expr, Bold];
inverseASymbol[] := Superscript[Style["A", Italic], -1];
transposeLSymbol[] := Superscript[Style["L", Italic], Style["T", Italic]]

resultValueStyle[expr_] := Style[expr, Bold, Blue];
resultEquationLine[lhs_, rhs_] := Row[{lhsStyle[lhs], " = ", resultValueStyle[tft[rhs]]}];
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
  {n = data["n"], augS, A, b, idxs, params, solExprs, pivot, knownTerm},

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

buildTaskEquations[A_, b_, vars_] := MapThread[HoldForm[#1 == #2] &, {A . vars, b}];
augFromAb[A_, b_] := Join[A, List /@ b, 2];


(* ~-~-~ ROW OPERATIONS - delenie, kombinácia ~-~-~ *)

(* note pre delenie riadku *)
rowNoteDivide[i_, p_] := Row[{"R", i, " \[LeftArrow] R", i, " / ", tf[p]}];
rowApplyDivide[aug_, i_Integer, p_Integer] := ReplacePart[aug, i -> (aug[[i]]/p)];

(* násobenie riadku skalárom *)
rowNoteMultiply[i_, p_] := Row[{"R", i, " \[LeftArrow] ", tf[p], "\[CenterDot]R", i}];
rowApplyMultiply[aug_, i_Integer, p_] := ReplacePart[aug, i -> (p aug[[i]])];

(* note pre kombináciu riadkov *)
rowNoteCombine[i_, terms_List] := Module[
  {base = Row[{"R", i, " \[LeftArrow] R", i}], termDisplay},

  termDisplay[row_, coeff_] := Row[{
    If[coeff < 0, " - ", " + "],
    If[Abs[coeff] === 1,
      Row[{"R", row}],
      Row[{tf[Abs[coeff]], "\[CenterDot]", "R", row}]
    ]
  }];

  Row @ Prepend[(termDisplay @@@ terms), base]
];

rowApplyCombine[aug_, i_Integer, terms_List] := Module[{row = aug[[i]]},
  ReplacePart[aug,  i -> (row + Total[terms[[All, 2]] aug[[terms[[All, 1]]]]])]
];

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

appendElemTransition[
  content_, before_, after_, note_, eMat_, targetRow_Integer, n_Integer,
  eIndex_Integer, mIndex_Integer, boldPos_: Automatic,
  hiBefore_Association : <||>, hiAfter_Association : <||>
] := Module[
  {notes, eLabel, prevLabel, nextLabel, eHi, eBoldPositions, eActiveCol},

  notes = ConstantArray["", n];
  notes[[targetRow]] = note;

  eLabel = Subscript[Style["E", Italic], eIndex];
  prevLabel = Subscript[Style["M", Italic], mIndex - 1];
  nextLabel = Subscript[Style["M", Italic], mIndex];

  eBoldPositions = Which[
    MatchQ[boldPos, {_Integer, _Integer}],
    {boldPos},
    ListQ[boldPos] && AllTrue[boldPos, MatchQ[#, {_Integer, _Integer}] &],
    boldPos,
    True,
    {}
  ];

  eActiveCol = Which[
    MatchQ[boldPos, {_Integer, _Integer}],
    boldPos[[2]],
    True,
    targetRow
  ];

  eHi = <|
    "ActiveRow" -> targetRow,
    "ActiveCol" -> eActiveCol,
    "BoldPositions" -> eBoldPositions
  |>;

  addGap[content, 1];

  AppendTo[
    content,
    Grid[
      {{
        labeledMatrixBlock[eLabel, styledPlainMatrix[eMat, eHi]],
        Style["\[CenterDot]", Bold, FontSize -> 18],
        labeledMatrixBlock[
          prevLabel,
          alignedAugmentedMatrix[
            before,
            notes,
            hiBefore
          ]
        ],
        Style["=", Bold, FontSize -> 18],
        labeledMatrixBlock[
          nextLabel,
          alignedAugmentedMatrix[
            after,
            {},
            hiAfter
          ]
        ]
      }},
      Alignment -> {Left, Center, Left, Center, Left},
      Spacings -> {1.2, 0.8}
    ]
  ];
];

SetAttributes[applyElemMultiplyStep, HoldFirst];

applyElemMultiplyStep[
  content_, aug_, rowIdx_Integer, factor_, n_Integer, pivotPos_: None
] := Module[{before, after, eMat, hi},
  before = aug;
  after = rowApplyMultiply[before, rowIdx, factor];
  eMat = elemMatrixScale[n, rowIdx, factor];

  hi = If[ListQ[pivotPos],
    <|"ActiveRow" -> rowIdx, "PivotPos" -> pivotPos|>,
    <|"ActiveRow" -> rowIdx|>
  ];

  $ElemStepCounter++;
  $ElemMatrixCounter++;

  appendElemTransition[
    content, before, after, rowNoteMultiply[rowIdx, factor], eMat,
    rowIdx, n, $ElemStepCounter, $ElemMatrixCounter, {rowIdx, rowIdx}, hi, hi
  ];

  after
];

SetAttributes[applyElemCombineStep, HoldFirst];

applyElemCombineStep[
  content_, aug_, rowIdx_Integer, terms_List, n_Integer, pivotPos_: None
] := Module[{before, after, eMat, hiBefore, hiAfter},
  before = aug;
  after = rowApplyCombine[before, rowIdx, terms];
  eMat = elemMatrixCombine[n, rowIdx, terms];

  (* pred úpravou chceme vidieť menený riadok aj zdrojové riadky *)
  hiBefore = Join[
    <|"ActiveRow" -> rowIdx, "SourceRows" -> terms[[All, 1]]|>,
    If[ListQ[pivotPos], <|"PivotPos" -> pivotPos|>, <||>]
  ];

  (* po úprave chceme zvýrazniť už iba výsledný menený riadok *)
  hiAfter = Join[
    <|"ActiveRow" -> rowIdx|>,
    If[ListQ[pivotPos], <|"PivotPos" -> pivotPos|>, <||>]
  ];

  $ElemStepCounter++;
  $ElemMatrixCounter++;

  appendElemTransition[
    content, before, after, rowNoteCombine[rowIdx, terms], eMat,
    rowIdx, n, $ElemStepCounter, $ElemMatrixCounter, {rowIdx, terms[[1, 1]]}, hiBefore, hiAfter
  ];

  after
];

SetAttributes[applyElemDivideStep, HoldFirst];

applyElemDivideStep[
  content_, aug_, rowIdx_Integer, divisor_, n_Integer, pivotPos_: None
] := Module[{before, after, eMat, hi},
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

applyJordanSwapStep[
  content_, aug_, i_Integer, k_Integer, n_Integer, showElemQ_?BooleanQ
] := Module[{before, after, notes, eMat, hi1, hi2},
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

  eMat = elemMatrixSwap[n, i, k];
  $ElemStepCounter++;
  $ElemMatrixCounter++;

  appendElemTransition[
    content, before, after, rowNoteSwap[i, k], eMat,
    i, n, $ElemStepCounter, $ElemMatrixCounter, Automatic, hi1, hi2
  ];

  after
];

SetAttributes[applyJordanElimStep, HoldFirst];

applyJordanElimStep[
  content_, aug_, r_Integer, i_Integer, n_Integer,
  hiBase_Association, showElemQ_?BooleanQ
] := Module[
  {workAug, before, elimRes, p, a, g, p2, a2, g2},

  If[!showElemQ,
    before = aug;
    elimRes = rowApplyElimStable[before, r, i];
    Return[rowAppendElimStep[content, before, elimRes, r, i, n, hiBase]];
  ];

  workAug = aug;

  a = workAug[[r, i]];
  If[a === 0, Return[workAug]];

  p = workAug[[i, i]];
  g = GCD[p, a];
  p2 = p/g;
  a2 = a/g;

  If[p2 =!= 1,
    workAug = applyElemMultiplyStep[content, workAug, r, p2, n, {i, i}]
  ];

  If[a2 =!= 0,
    workAug = applyElemCombineStep[content, workAug, r, {{i, -a2}}, n, {i, i}]
  ];

  g2 = rowAbsGCD[workAug[[r]]];
  If[g2 =!= 1,
    workAug = applyElemDivideStep[content, workAug, r, g2, n, {i, i}]
  ];

  workAug
];

SetAttributes[rowAppendElimStep, HoldFirst];

rowAppendElimStep[content_, before_, elimRes_, r_Integer, i_Integer, n_Integer, hiBase_Association] := Module[{notes, notes2, mid, after2, hi1, hi2, hi3},
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

rowAppendElimStepInverse[content_, before_, elimRes_, r_Integer, i_Integer, n_Integer, hiBase_Association] := Module[
  {notes, notes2, mid, after2, hi1, hi2, hi3},

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

rowAbsGCD[row_List] := Module[{g = Apply[GCD, Abs[row]]}, If[g === 0, 1, g]];

normalizeRow[row_List] := Module[{g = rowAbsGCD[row], first},
  first = FirstCase[Most[row], x_ /; x =!= 0, 1];
  If[g > 1, row/g, If[first === -1, -row, row]]
];

rowNoteSwap[i_, k_] := Row[{"R", i, " \[LeftRightArrow] R", k}];

rowApplySwap[aug_, i_Integer, k_Integer] := ReplacePart[aug, {i -> aug[[k]], k -> aug[[i]]}];

rowNoteElim[r_, i_, p2_, a2_] := Module[{leftPart, rightPart, op},
  leftPart = If[p2 === 1, Row[{"R", r}], Row[{tf[p2], "\[CenterDot]", "R", r}]];
  rightPart = If[Abs[a2] === 1, Row[{"R", i}], Row[{tf[Abs[a2]], "\[CenterDot]", "R", i}]];
  op = If[a2 < 0, " + ", " - "];

  Row[{"R", r, " \[LeftArrow] ", leftPart, op, rightPart}]
];

rowApplyElimStable[aug_, r_Integer, i_Integer] := Module[
  {p, a, g1, p2, a2, rowRaw, g2, div, rowFinal, augRaw, augFinal},

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

(* elementárne matice *)
elemMatrixSwap[n_Integer, i_Integer, k_Integer] := Module[{e = IdentityMatrix[n]},
  e[[{i, k}]] = e[[{k, i}]];
  e
];

elemMatrixScale[n_Integer, i_Integer, factor_] := Module[{e = IdentityMatrix[n]},
  e[[i, i]] = factor;
  e
];

elemMatrixCombine[n_Integer, i_Integer, terms_List] := Module[{e = IdentityMatrix[n]},
  Scan[
    Function[{term},
      With[{src = term[[1]], coeff = term[[2]]},
        e[[i, src]] = Together[e[[i, src]] + coeff]
      ]
    ],
    terms
  ];
  e
];

(* pre INFINITE a NONE *)
contradictionRowQ[row_List] := Module[{lhs = Most[row], rhs = Last[row]}, (AllTrue[lhs, # === 0 &] && rhs =!= 0)];
findContradictionRow[aug_] := Module[{idx = FirstCase[Range[Length[aug]], i_ /; contradictionRowQ[aug[[i]]], Missing["NotFound"]]}, idx];

(* ~-~-~ MATRIX VISUALIZATION ~-~-~ *)

matrixCellDisplay[val_] := If[MemberQ[{Tooltip, MouseAppearance, Style, Row, Grid, Pane, Framed, TraditionalForm}, Head[val]], val, TraditionalForm[val]];

dotProductTooltipMatrix[left_, right_] := Module[
  {makeTermDisplay, makeTooltipCell},

  makeTermDisplay[a_, b_] := Row[{luFactorDisplay[a], "\[CenterDot]", luFactorDisplay[b]}];

  makeTooltipCell[i_, j_] := Module[{terms, value, tooltipExpr},
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

(*pre skalárny súćin*)
matrixProductDisplay[left_, right_] := highlightGrid @ Grid[
  {{
    styledPlainMatrix[left],
    Style["\[CenterDot]", Bold, FontSize -> 16],
    styledPlainMatrix[right],
    Style["=", Bold, FontSize -> 16],
    styledPlainMatrix[dotProductTooltipMatrix[left, right]]
  }},
  Alignment -> Center,
  Spacings -> {1, 1},
  BaseStyle -> {FontSize -> 14}
];

(* zvýraznené zobrazenie obyčajnej matice s riadkom a stĺpcom *)
styledPlainMatrix[m_, hi_Association : <||>] := Module[
  {
    nRows, nCols, activeRows, sourceRows, activeCols, sourceCols,
    boldPositions, cellBg, makeCell, leftBracketCell, rightBracketCell,
    rows
  },

  {nRows, nCols} = Dimensions[m];

  activeRows = DeleteCases[
    Flatten @ {Lookup[hi, "ActiveRows", {}], Lookup[hi, "ActiveRow", None]},
    None
  ];
  sourceRows = Flatten @ {Lookup[hi, "SourceRows", {}]};

  activeCols = DeleteCases[
    Flatten @ {Lookup[hi, "ActiveCols", {}], Lookup[hi, "ActiveCol", None]},
    None
  ];
  sourceCols = Flatten @ {Lookup[hi, "SourceCols", {}]};

  boldPositions = Lookup[hi, "BoldPositions", {}];
  If[MatchQ[boldPositions, {_Integer, _Integer}],
    boldPositions = {boldPositions}
  ];

  cellBg[i_, j_] := Module[{aRowQ, sRowQ, aColQ, sColQ},
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

  makeCell[i_, j_, val_] := Module[{cell = matrixCellDisplay[val]},
    If[
      MemberQ[boldPositions, {i, j}] || (MemberQ[activeRows, i] && MemberQ[activeCols, j]),
      cell = Style[cell, Bold]
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

matrixBlock[label_, m_, bold_List : {}] := labeledMatrixBlock[label, styledPlainMatrix[m, <|"BoldPositions" -> bold|>]];
vectorBlock[label_, v_List] := labeledMatrixBlock[label, styledPlainMatrix[List /@ v]];

alignedAugmentedMatrix[aug_, notes_List : {}, hi_Association : <||>] := Module[
  {
    nRows, nCols, nA, notes2, pivotPos, activeRows, sourceRows,
    activeCols, sourceCols, ZeroCells, orangeCells, bar,
    leftLabel, rightLabel, showLabelsQ,
    cellBg, makeCell, makeBar, leftBracketCell, rightBracketCell,
    rows, matrixGrid, notesGrid, notesWithLabels, colSizes, labelGrid, matrixWithLabels
  },

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

  cellBg[i_, j_] := Module[{aRowQ, sRowQ, aColQ, sColQ},
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

  makeCell[i_, j_, val_] := Module[{cell = TraditionalForm[val], isGreen, isOrange, isDiag, isPivot},
    isGreen = MemberQ[ZeroCells, {i, j}];
    isOrange = MemberQ[orangeCells, {i, j}];
    isDiag = j <= nA && i === j;
    isPivot = ListQ[pivotPos] && pivotPos === {i, j};

    If[isGreen,
      cell = Style[cell, Red, Bold],
      If[isOrange,
        cell = Style[cell, Orange, Bold],
        If[isPivot || isDiag, cell = Style[cell, Bold]]
      ]
    ];

    Item[Pane[cell, ImageSize -> {Automatic, 18}, Alignment -> {Right, Center}], Background -> cellBg[i, j]]
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
  {nRows, nCols, nA, notes2, pivotPos, activeRow, sourceRows, ZeroCells, bar,
    rowColor, sourceColor, wrapBg, makeCell, makeBar, leftBracketCell, rightBracketCell,
    rows, matrixGrid, notesGrid, notesWithLabels, showPivotQ, boldDiagQ, leftLabel, rightLabel,
    showLabelsQ, colSizes, labelGrid, matrixWithLabels},

  {nRows, nCols} = Dimensions[aug];
  nA = Quotient[nCols, 2];

  notes2 = If[notes === {}, ConstantArray["", nRows], PadRight[notes, nRows, ""]];
  pivotPos = Lookup[hi, "PivotPos", None];
  activeRow = Lookup[hi, "ActiveRow", None];
  sourceRows = Lookup[hi, "SourceRows", {}];
  ZeroCells = Lookup[hi, "ZeroCells", {}];

  leftLabel = Lookup[hi, "LeftLabel", None];
  rightLabel = Lookup[hi, "RightLabel", None];
  showLabelsQ = leftLabel =!= None || rightLabel =!= None;

  bar = Style["|", GrayLevel[.35], FontSize -> 16];
  rowColor = RGBColor[0.90, 0.95, 1];
  sourceColor = RGBColor[0.95, 0.92, 1.00];

  wrapBg[i_, expr_] := Module[{bg = None},
    If[IntegerQ[activeRow] && i === activeRow, bg = rowColor,
      If[MemberQ[sourceRows, i], bg = sourceColor]
    ];
    Item[expr, Background -> bg]
  ];

  showPivotQ = ListQ[pivotPos] &&
      ((IntegerQ[activeRow] && activeRow === pivotPos[[1]]) || MemberQ[sourceRows, pivotPos[[1]]]);

  makeCell[i_, j_, val_] := Module[
    {cell = TraditionalForm[val], isGreen, isDiagLeft, isDiagRight, isDiag},

    isGreen = MemberQ[ZeroCells, {i, j}];
    isDiagLeft = j <= nA && i === j;
    isDiagRight = j > nA && i === (j - nA);
    isDiag = isDiagLeft || isDiagRight;

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
      {makeBar[i]},
      Table[makeCell[i, j, aug[[i, j]]], {j, nA + 1, nCols}],
      {If[i === 1, rightBracketCell, SpanFromAbove]}
    ],
    {i, 1, nRows}
  ];

  colSizes = Join[{0.2}, ConstantArray[1.2, nA], {0.2}, ConstantArray[1.2, nA], {0.2}];

  matrixGrid = Grid[
    rows,
    Alignment -> Join[
      {Center},
      ConstantArray[Right, nA],
      {Center},
      ConstantArray[Right, nA],
      {Center}
    ],
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
        ConstantArray[SpanFromLeft, nA - 1],
        {""}
      ]
    },
    Alignment -> Center,
    Spacings -> {1, 0},
    BaseStyle -> {FontSize -> 14},
    ItemSize -> {colSizes, Automatic}
  ];

  matrixWithLabels = If[showLabelsQ,
    Column[{labelGrid, matrixGrid}, Alignment -> Center, Spacings -> {0.15}],
    matrixGrid
  ];

  notesGrid = Grid[
    List /@ (
      Item[
        Pane[
          If[StringQ[#],
            Style[#, GrayLevel[.35], FontSize -> 13],
            #
          ],
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

$MaxBounds = 20; (*väčšie číslo sa nemôže ukázať*)
$Bounds = 4 + Quotient[$MaxBounds, 1.4 + 0.156 Sqrt[$MaxBounds]];
$MaxRetryCount = 150;

matrixMaxAbs[m_] := Max[Abs[Flatten[m]]];


SetAttributes[appendNoneConclusionAndStop, HoldFirst];

appendNoneConclusionAndStop[content_, aug_, data_Association, showElemQ_: False, mIndex_: None] := Module[
  {n, badIdx, notes},

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
  AppendTo[content, "Skontrolujeme to pomocou Frobeniovej vety porovnaním hodností."];
  content = Join[content, verificationStepsNone[data]];
  Throw[<|"Content" -> content, "Solution" -> "NONE"|>, "StopMatrixSteps"]
];

generateDataWithBounds[diff_String, n_Integer, solType_, triType_, scrambleFn_, pivotMode_: "ZERO", boundAugFn_: Automatic, boundCheckFn_: Automatic] := Module[
  {data, retries = 0, augForCheck, resolvedBoundAugFn, resolvedBoundCheckFn},

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

lowerNonzeroCount[m_] := Count[LowerTriangularize[m, -1], x_ /; x =!= 0, {2}];

(* vytvorenie vyriešenej augmentovanej matice *)
makeDiagonalAug[n_Integer, solType_String] := Module[
  {
    A, b, x, idx, paramIdx, paramIdxs = {}, badRow, rhsNonzero,
    numParams, pivotRows, coeffPool, buildParamColumn,
    col1, col2, tries
  },

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

(* ~-~-~ DATA GENERATION ~-~-~ *)

generateData[diff_String, n_, solType_, triType_, scrambleFn_] := Module[
  {solved, augSolved, augTask, A, b, vars},

  solved = makeDiagonalAug[n, solType];
  augSolved = solved["Aug"];

  augTask = scrambleFn[diff, augSolved, triType, solType];

  If[augTask === $Failed,
    Return[$Failed]
  ];

  A = augTask[[All, 1 ;; n]];
  b = augTask[[All, n + 1]];
  vars = buildVars[n];

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
genScrambleTriang[diff_String, aug0_, triType_String, solType_String : "ONE", Gauss_ : True] := Module[
  {aug = aug0, n = Length[aug0], bnd, kSet, withinQ, protectedLastRowQ, chooseK, chooseS, i, r, k, s},

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

  chooseK[target_, src_] := Module[{k0, cand, ks},
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

  chooseS[row_] := Module[{s0, cand, ss},
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

genScrambleCholesky[diff_String, aug0_, triType_, solType_String : "ONE"] := Module[
  {n, solutionVector, lMatrix, aMatrix, bVector, tries = 0, lowerPool, diagPool, diagMax, lowerMax},

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


(* ~-~-~ STEP GENERATION HELPERS ~-~-~ *)

appendTriangularSubstitutionSteps[
  mat_, rhs_, vars_, sol0_, order_List, content_,
  initialKnownIdxs_List : {}, skipIdxs_List : {}
] := Module[
  {n = Length[mat], sol = sol0, out = content, solvedIdxs, boldVal, coeffTimes, addOneRow},

  solvedIdxs = initialKnownIdxs;

  boldVal[val_] := Module[{expandedVal = Expand[val]},
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
gaussPivotRowByMinAbs[aug_, i_Integer] := Module[
  {n = Length[aug], candidates, currentPivot, betterCandidates},

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
gaussPivotRowByNonzero[aug_, i_Integer] := Module[{n = Length[aug], candidates},
  If[aug[[i, i]] =!= 0, Return[i]];
  candidates = Select[Range[i + 1, n], aug[[#, i]] =!= 0 &];
  If[candidates === {}, i, First[candidates]]
];

(* vysvetlenie výmeny pivotových riadkov *)
gaussPivotSwapExplanation[aug_, i_Integer, k_Integer] := Module[{currentPivot, newPivot},
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

(* stĺpce, v ktorých vynucujeme pivotovanie *)
gaussPlannedPivotSwapColumns[pivotCount_Integer] := Module[{possibleCols},
  possibleCols = Range[Max[0, pivotCount - 1]];

  Which[
    possibleCols === {}, {},
    Length[possibleCols] === 1, {1},
    Length[possibleCols] === 2, {1, 2},
    True, {2, Last[possibleCols]}
  ]
];

(* stĺpce so zmysluplnou výmenou pivotu *)
gaussObservedPivotSwapColumns[trace_List] := Module[
  {cols = {}, step, prev, i, k, currentPivot, newPivot},

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

(* vynútenie susednej výmeny pre pivotovanie *)
gaussForceAdjacentPivotSwap[aug_, i_Integer, bnd_Integer] := Module[
  {work = aug, rowI, rowK, factors, chosen},

  If[i >= Length[aug], Return[work]];

  rowI = work[[i]];
  rowK = work[[i + 1]];

  If[rowI[[i]] === 0 || rowK[[i]] === 0, Return[work]];

  factors = {2, 3, -2, -3};

  chosen = SelectFirst[
    factors,
    Module[{cand = # rowK},
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

(* trace doprednej eliminácie pre Gaussa *)
gaussForwardEliminationTrace[aug_, pivotMode_: "ZERO"] := Module[
  {workAug, n, i, r, pivotRowFn, pivotRow, pivotValue, elimRes, trace = {}},

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

(* kontrola medzí počas Gaussovej eliminácie *)
gaussForwardEliminationWithinBoundsQ[aug_, pivotMode_: "ZERO"] := Module[
  {traceData, limit},

  limit = $MaxBounds;
  traceData = gaussForwardEliminationTrace[aug, pivotMode];

  AllTrue[
    traceData["Trace"],
    matrixMaxAbs[#["Matrix"]] <= limit &
  ]
];

(* trace celej Gauss-Jordanovej eliminácie *)
gaussJordanEliminationTrace[aug_, pivotMode_: "MIN"] := Module[
  {workAug, n, i, r, pivotRowFn, pivotRow, pivotValue, elimRes, trace = {}, after},

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

(* kontrola medzí počas Gauss-Jordanovej eliminácie *)
gaussJordanEliminationWithinBoundsQ[aug_, pivotMode_: "MIN"] := Module[
  {traceData, limit},

  limit = $MaxBounds;
  traceData = gaussJordanEliminationTrace[aug, pivotMode];

  AllTrue[
    traceData["Trace"],
    matrixMaxAbs[#["Matrix"]] <= limit &
  ]
];

(* premiešanie sústavy pre Gaussovu metódu *)
genScrambleGauss[diff_String, aug0_, triType_String, solType_String : "ONE"] := Module[{n, pairs, chosenPairs, kSet, bnd, maxAttempts, maxKTries, aug, r, i, k, rowNew, currentLower},
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

    currentLower = lowerNonzeroCount[aug[[All, 1 ;; n]]];
    If[currentLower == Length[pairs], Return[aug]];
    ,
    {attempt, 1, maxAttempts}
  ];
  aug
];

(* premiešanie sústavy pre pivotovaný Gauss-Jordan *)
genScrambleGaussJordanPivot[diff_String, aug0_, triType_String, solType_String : "ONE"] := Module[
  {aug, n, pivotCount, plannedCols, bnd, tries = 0, trace, observedCols},

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

(* --- LU / Cholesky HELPERS --- *)

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

choleskySolveData[A_, b_] := Module[
  {n, L, y, x, i, j, diagTerms, mixedTerms, diagRadicand, diagValue, sumTerm},

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

choleskyDecompositionWithinBoundsQ[data_Association] := Module[
  {choleskyData, limit},

  limit = $MaxBounds;
  choleskyData = choleskySolveData[data["A"], data["b"]];

  If[choleskyData === $Failed, Return[False]];

  AllTrue[
    {data["A"], data["b"], choleskyData["L"], Transpose[choleskyData["L"]], choleskyData["Y"], choleskyData["X"]},
    matrixMaxAbs[#] <= limit &
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

luLinearCombinationDisplay[terms_List] := Module[
  {clean, first, rest, formatPositiveTerm, formatFirstTerm, formatNextTerm},

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
matrixPairGrid[leftLabel_, leftMatrix_, rightLabel_, rightMatrix_, leftBold_List : {}, rightBold_List : {}] := Module[
  {styledLeft, styledRight},

  styledLeft = MapIndexed[
    If[MemberQ[leftBold, #2], Style[#1, Bold], #1] &,
    leftMatrix,
    {2}
  ];

  styledRight = MapIndexed[
    If[MemberQ[rightBold, #2], Style[#1, Bold], #1] &,
    rightMatrix,
    {2}
  ];

  highlightGrid @ Grid[
    {{
      Style[Row[{leftLabel, " ="}], Bold, FontSize -> 16],
      TraditionalForm[MatrixForm[styledLeft]],
      Spacer[20],
      Style[Row[{rightLabel, " ="}], Bold, FontSize -> 16],
      TraditionalForm[MatrixForm[styledRight]]
    }},
    Alignment -> Left,
    Spacings -> {2, 1}
  ]
];

luMatrixPairGrid[L_, U_, lBold_List : {}, uBold_List : {}] :=
    matrixPairGrid["L", L, "U", U, lBold, uBold];

choleskyMatrixPairGrid[L_, LT_, lBold_List : {}, ltBold_List : {}] := highlightGrid @ Grid[
  {{
    matrixBlock[Style["L", Italic], L, lBold],
    Spacer[20],
    matrixBlock[transposeLSymbol[], LT, ltBold]
  }},
  Alignment -> {Center, Center, Center},
  Spacings -> {1.2, 0}
];

namedVectorGrid[label_String, vec_List] := highlightGrid @ Grid[
  {{
    vectorBlock[Style[label, Italic], vec]
  }},
  Alignment -> Left,
  Spacings -> {1, 0}
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

forwardEquationDisplay[row_List, rhs_, vars_List, idx_Integer] := Module[{terms},
  terms = Table[{row[[j]], vars[[j]]}, {j, 1, idx}];
  Row[{luLinearCombinationDisplay[terms], " = ", tft[rhs]}]
];

backwardEquationDisplay[row_List, rhs_, vars_List, idx_Integer, n_Integer] := Module[{terms},
  terms = Table[{row[[j]], vars[[j]]}, {j, idx, n}];
  Row[{luLinearCombinationDisplay[terms], " = ", tft[rhs]}]
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

buildCholeskyDiagonalLines[i_Integer, A_, L_, value_] := Module[
  {diagTerms, diagRadicand},

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

buildCholeskyOffDiagonalLines[j_Integer, i_Integer, A_, L_, diagValue_, value_] := Module[
  {mixedTerms, numerator},

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

replaceColumn[matrix_, column_Integer, values_List] := Module[{updated = matrix},
  Do[updated[[i, column]] = values[[i]], {i, 1, Length[values]}];
  updated
];

cramerRandomNonzeroValue[maxAbs_Integer : 4] := RandomChoice[DeleteCases[Range[-maxAbs, maxAbs], 0]];

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

renderCramer4x4Reduction[matrix_, label_] := Module[
  {content = {}, line1, signed1, minor3, minor3Label, det3Data, value},

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

generateCramerMediumMatrix[solutionVector_List] := Module[
  {core, candidate, rhsVector, s1, tries = 0},

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

generateCramerHardMatrix[solutionVector_List] := Module[
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

(* text pre laplaceov rozvoj *)
cramerLaplaceExplanation[lineData_Association] := If[
  lineData["Type"] === "Row",
  Row[{"Použijeme Laplaceov rozvoj podľa ", lineData["LineIndex"], ". riadku, lebo obsahuje jeden nenulový prvok."}],
  Row[{"Použijeme Laplaceov rozvoj podľa ", lineData["LineIndex"], ". stĺpca, lebo obsahuje jeden nenulový prvok."}]
];

cramerMatrixLabel[var_] := Subscript[Style["A", Italic], Style[var, Italic]];
cramerDetLabel[label_] := Row[{"det(", label, ")"}];

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

      Style[TraditionalForm[#1], Sequence @@ styleArgs]
    ] &,
    matrix,
    {2}
  ]
];

cramerMatrixCard[matrix_, hi_Association : <||>] := styledPlainMatrix[
  cramerStyledMatrix[matrix, hi]
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
    labeledMatrixBlock[
      minorLabel,
      cramerMatrixCard[minorMatrix, <|"FontSize" -> 13, "CellWidth" -> 1.05|>]
    ]
  }},
  Alignment -> {Center, Center, Center},
  Spacings -> {1.8, 1}
];

cramerLaplaceVisualizationTitle[lineData_Association] := If[
  lineData["Type"] === "Row",
  "Vizualizácia Laplaceovho rozvoja podľa " <> ToString[lineData["LineIndex"]] <> ". riadku:",
  "Vizualizácia Laplaceovho rozvoja podľa " <> ToString[lineData["LineIndex"]] <> ". stĺpca:"
];

cramerFactor[value_] := If[
  NumberQ[value] && value < 0,
  Row[{"(", tft[value], ")"}],
  tft[value]
];

cramerLabeledMatrixGrid[label_, matrix_, hi_Association : <||>] := labeledMatrixBlock[
  label,
  cramerMatrixCard[matrix, hi]
];

cramerAuxiliaryMatrixPanel[A_, auxMatrix_, column_Integer, auxLabel_] := Module[
  {leftBg, rightBg, matrixWithColumnBackground},

  leftBg = RGBColor[0.95, 0.92, 1.00];
  rightBg = RGBColor[0.90, 0.95, 1.00];

  (* lokálne vykreslenie matice so zvýrazneným stĺpcom *)
  matrixWithColumnBackground[m_, bg_] := Module[
    {nRows, nCols, leftBracketCell, rightBracketCell, makeCell, rows},

    {nRows, nCols} = Dimensions[m];

    makeCell[i_, j_] := Module[{cell},
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
    Style[TraditionalForm[#1], FontColor -> cramer3x3ModeColor[mode, #2], Bold] &,
    matrix,
    {2}
  ];

  styledPlainMatrix[styled]
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

cramer3x3ProductValues[matrix_] := Module[
  {a, b, c, d, e, f, g, h, i},

  {{a, b, c}, {d, e, f}, {g, h, i}} = matrix;

  {
    a*e*i,
    b*f*g,
    c*d*h,
    -c*e*g,
    -a*f*h,
    -b*d*i
  }
];

cramerSignedValueSum[values_List] := Module[
  {clean, first, rest},

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

cramer3x3VisualPanel[label_, matrix_] := Grid[
  {{
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
renderCramer3x3Det[matrix_, label_] := Module[
  {content = {}, value, knownQ, knownMatrices, knownPos, knownData},

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

  AppendTo[content, Row[
    {
      cramerDetLabel[label],
      " = ",
      cramer3x3FormulaDisplay[matrix]
    }
  ]];

  AppendTo[content, Row[
    {
      cramerDetLabel[label],
      " = ",
      cramerSignedValueSum[cramer3x3ProductValues[matrix]]
    }
  ]];

  AppendTo[content, resultEquationLine[cramerDetLabel[label], value]];

  If[knownQ,
    AppendTo[
      cramerKnown3x3,
      <|
        "Matrix" -> matrix,
        "Value" -> value
      |>
    ];
  ];

  <|
    "Content" -> content,
    "Value" -> value,
    "Matrix" -> matrix
  |>
];

(* vykreslí determinant 5×5 cez dva laplaceove rozvoje a následný determinant 3×3 *)
renderCramer5x5Reduction[matrix_, label_] := Module[
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

renderCramerDeterminant[matrix_, label_] := Switch[
  Length[matrix],
  3, renderCramer3x3Det[matrix, label],
  4, renderCramer4x4Reduction[matrix, label],
  5, renderCramer5x5Reduction[matrix, label],
  _, <|
    "Content" -> {
      cramerLabeledMatrixGrid[label, matrix],
      resultEquationLine[cramerDetLabel[label], Together[Det[matrix]]]
    },
    "Value" -> Together[Det[matrix]],
    "Matrix" -> matrix
  |>
];

(* ~-~-~ STEP GENERATION ~-~-~ *)

stepsGauss[data_Association] := Catch[Module[ {content = {}, n, aug, vars, st, addHeader, addText, addMatrix, notes, before, after, kPivot, elimRes, pNow, solLocal, tmp},
  n = data["n"]; aug = data["Aug"]; vars = data["Vars"]; st = data["SolutionType"];

  addHeader[text_] := appendStepHeader[content, text];
  addText[text_] := AppendTo[content, text];
  addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] := AppendTo[content, alignedAugmentedMatrix[m, rowNotes, hi]];

  addHeader["Prepis sústavy do augmentovanej matice"];
  addText["Sústavu zapíšeme do augmentovanej matice. Potom vynulujeme prvky pod hlavnou diagonálou."];
  addMatrix[aug, {}, <|"LeftLabel" -> Style["A", Italic], "RightLabel" -> Style["b", Italic]|>];

  addHeader["Dopredná eliminácia (na horný trojuholník)"];
  addText["Postupujeme po stĺpcoch zľava doprava. Vyberieme pivot, podľa potreby prehodíme riadky a potom vynulujeme prvky pod pivotom."];

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
  addText["Po týchto úpravách dostaneme hornú trojuholníkovú sústavu. Neznáme určíme spätným dosadzovaním od posledného riadku."];
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
    Module[{paramIdxs, paramSymbols},
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
            "Voľnú premennú zvolíme ",
            tf[vars[[paramIdxs[[1]]]]], " = ", TraditionalForm[paramSymbols[[1]]], "."
          }],
          Row[{
            "Voľné premenné zvolíme ",
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
          "Dosadíme parametrické riešenie do pôvodných rovníc. V každom riadku musí vyjsť identita pre ľubovoľné \[FormalT] \[Element] \[DoubleStruckCapitalZ].",
          "Dosadíme parametrické riešenie do pôvodných rovníc. V každom riadku musí vyjsť identita pre ľubovoľné \[FormalS], \[FormalT] \[Element] \[DoubleStruckCapitalZ]."
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
      {content = {}, n, aug, vars, st, addHeader, addText, addMatrix, notes, pNow, solLocal, solExprs, row, pivot, knownTerm, pivotRowFn},

      n = data["n"]; aug = data["Aug"]; vars = data["Vars"]; st = data["SolutionType"];
      pivotRowFn = If[pivotQ, gaussPivotRowByMinAbs, gaussPivotRowByNonzero];

      addHeader[text_] := appendStepHeader[content, text];
      addText[text_] := AppendTo[content, text];
      addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] := AppendTo[content, alignedAugmentedMatrix[m, rowNotes, hi]];

      addHeader["Prepis sústavy do augmentovanej matice"];
      If[showElemQ,
        addText["Sústavu zapíšeme do augmentovanej matice a označíme ju M₀. Pri každej úprave uvedieme aj príslušnú elementárnu maticu Eᵢ, takže bude platiť Mᵢ = Eᵢ · Mᵢ₋₁."];
        AppendTo[content, namedAugmentedStateCard[Subscript[Style["M", Italic], 0], aug, {}, <|"LeftLabel" -> Style["A", Italic], "RightLabel" -> Style["b", Italic]|>]],
        addText["Sústavu zapíšeme do augmentovanej matice. Úpravami ju prevedieme na tvar (I | x)."];
        addMatrix[aug, {}, <|"LeftLabel" -> Style["A", Italic], "RightLabel" -> Style["b", Italic]|>]
      ];

      addHeader["Dopredná eliminácia (nulovanie pod diagonálou)"];
      addText[
        If[showElemQ,
          If[pivotQ,
            "Na začiatku každého stĺpca vyberieme najmenší pivot. Ak treba, prehodíme riadky a potom vynulujeme prvky pod pivotom a každú úpravu zapíšeme aj pomocou elementárnej matice.",
            "Pomocou riadkových úprav vynulujeme prvky pod pivotmi. Ak je pivot nulový, najprv prehodíme riadky. Každú úpravu zapíšeme aj pomocou elementárnej matice."
          ],
          If[pivotQ,
            "Na začiatku každého stĺpca vyberieme najmenší pivot. Ak treba, prehodíme riadky a potom vynulujeme prvky pod pivotom.",
            "Pomocou riadkových úprav vynulujeme prvky pod pivotmi. Ak je pivot nulový, najprv prehodíme riadky."
          ]
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
            aug = applyJordanElimStep[content, aug, r, i, n, <|"SourceRows" -> {i}|>, showElemQ];
            If[st === "NONE",
              appendNoneConclusionAndStop[content, aug, data, showElemQ, $ElemMatrixCounter]
            ];
          ],
          {r, i + 1, n}
        ],
        {i, 1, n - 1}
      ];

      If[st === "INFINITE" && AnyTrue[aug, AllTrue[#, # === 0 &] &],
        addText["Tu už vidíme, že vyšiel riadok 0 = 0, takže sústava má nekonečne veľa riešení. Ešte však spravíme aj spätnú úpravu, aby sa neznáme dali ľahšie zapísať pomocou parametra."]
      ];

      addHeader["Spätná eliminácia (nulovanie nad diagonálou)"];
      addText[
        If[showElemQ,
          "Rovnakým spôsobom vynulujeme aj prvky nad pivotmi. Každú úpravu zapíšeme aj pomocou elementárnej matice.",
          "Rovnakým spôsobom vynulujeme aj prvky nad pivotmi."
        ]
      ];

      Do[
        pNow = aug[[i, i]];
        If[pNow === 0, Continue[]];

        Do[
          If[aug[[r, i]] =!= 0,
            aug = applyJordanElimStep[content, aug, r, i, n, <||>, showElemQ];
          ],
          {r, 1, i - 1}
        ];

        pNow = aug[[i, i]];
        If[pNow =!= 0 && pNow =!= 1,
          If[showElemQ,
            aug = applyElemDivideStep[content, aug, i, pNow, n, {i, i}],
            Module[{before, after},
              before = aug;
              after = rowApplyDivide[before, i, pNow];
              notes = ConstantArray["", n];
              notes[[i]] = rowNoteDivide[i, pNow];
              AppendTo[content, augRender2[
                before, after, notes,
                <|"ActiveRow" -> i, "PivotPos" -> {i, i}|>,
                <|"ActiveRow" -> i, "PivotPos" -> {i, i}, "ZeroCells" -> {{i, i}, {i, n + 1}}|>
              ]];
              aug = after;
            ]
          ]
        ];
        ,
        {i, n, 2, -1}
      ];

      pNow = aug[[1, 1]];
      If[pNow =!= 0 && pNow =!= 1,
        If[showElemQ,
          aug = applyElemDivideStep[content, aug, 1, pNow, n, {1, 1}],
          Module[{before, after},
            before = aug;
            after = rowApplyDivide[before, 1, pNow];
            notes = ConstantArray["", n];
            notes[[1]] = rowNoteDivide[1, pNow];
            AppendTo[content, augRender2[
              before, after, notes,
              <|"ActiveRow" -> 1, "PivotPos" -> {1, 1}|>,
              <|"ActiveRow" -> 1, "PivotPos" -> {1, 1}, "ZeroCells" -> {{1, 1}, {1, n + 1}}|>
            ]];
            aug = after;
          ]
        ]
      ];

      addHeader["Hotový tvar (I | x)"];
      addText["Po úpravách dostaneme tvar (I | x). Riešenie prečítame z pravej strany."];

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
            <|"LeftLabel" -> Style["I", Italic], "RightLabel" -> Style["x", Italic]|>
          ]
        ],
        addMatrix[
          aug,
          notes,
          <|"LeftLabel" -> Style["I", Italic], "RightLabel" -> Style["x", Italic]|>
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


          addMatrix[aug, notes, <|"ActiveRows" -> paramIdxs|>]

          Do[
            addText[Row[{"Premennú ", vars[[paramIdxs[[k]]]], " označíme parametrom ", TraditionalForm[paramSymbols[[k]]], "."}]];
            AppendTo[content, highlightGrid @ Grid[
              {{tf[vars[[paramIdxs[[k]]]]], "=", TraditionalForm[paramSymbols[[k]]] }},
              Alignment -> {{Right, Center, Left}},
              BaseStyle -> {FontSize -> 16}
            ]];
            ,
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
              aug[[All, 1 ;; n]], aug[[All, n + 1]],
              vars, solExprs, Range[n, 1, -1], content, paramIdxs, paramIdxs
            ];

            solExprs = tmp[[1]];
            content = tmp[[2]];
          ];

          addHeader["Skúška správnosti"];
          addText[
            If[Length[paramIdxs] === 1,
              "Dosadíme parametrické riešenie do pôvodných rovníc. V každom riadku musí vyjsť identita pre ľubovoľné \[FormalT] \[Element] \[DoubleStruckCapitalZ].",
              "Dosadíme parametrické riešenie do pôvodných rovníc. V každom riadku musí vyjsť identita pre ľubovoľné \[FormalS], \[FormalT] \[Element] \[DoubleStruckCapitalZ]."
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

stepsInverseMatrix[data_Association] := Module[
  {content = {}, n, A, b, vars, augInv, invMatrix, xResult,
    addHeader, addText, addMatrix, notes, before, after, kPivot, elimRes, pNow},

  n = data["n"];
  A = data["A"];
  b = data["b"];
  vars = data["Vars"];

  addHeader[text_] := appendStepHeader[content, text];
  addText[text_String] := AppendTo[content, text];
  addText[expr_] := AppendTo[content, Cell[BoxData @ ToBoxes[expr, StandardForm], "Text", ShowStringCharacters -> False]];
  addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] := AppendTo[content, alignedAugmentedMatrixInverse[m, rowNotes, hi]];

  addHeader["Prepis matice do tvaru (A | E)"];
  addText[Row[{"Na ľavú stranu zapíšeme maticu A a na pravú jednotkovú maticu E. Rovnakými úpravami dostaneme z ľavej časti E a z pravej ", inverseASymbol[], "."}]];

  augInv = Join[A, IdentityMatrix[n], 2];
  addMatrix[augInv, {}, <|"LeftLabel" -> Style["A", Italic], "RightLabel" -> Style["E", Italic]|>];

  addHeader["Dopredná eliminácia (nulovanie pod diagonálou)"];
  addText["Postupujeme po stĺpcoch zľava doprava. Vyberieme vhodný pivot a vynulujeme prvky pod ním."];

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
  addText["Rovnakým spôsobom vynulujeme aj prvky nad diagonálou."];

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
  addText[Row[{"Ľavá časť je jednotková matica. Pravá časť je teda inverzná matica ", inverseASymbol[], "."}]];
  addMatrix[
    augInv,
    {},
    <|"LeftLabel" -> Style["E", Italic], "RightLabel" -> inverseASymbol[]|>
  ];

  invMatrix = augInv[[All, n + 1 ;; 2 n]];

  addHeader[Row[{"Výpočet riešenia"}]];
  addText[Row[{"Riešenie teraz vypočítame zo vzťahu x = ", inverseASymbol[], " \[CenterDot] b."}]];
  addText["Tip: Keď prejdete kurzorom nad prvky výsledného vektora x, zobrazí sa skalárny súčin, z ktorého dané číslo vzniklo."];

  xResult = invMatrix . b;

  AppendTo[content, Module[{resultNotes},
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
  addText[Row[{"Najprv skontrolujeme, že A \[CenterDot] ", inverseASymbol[], " = E. Potom overíme, že platí aj A \[CenterDot] x = b."}]];

  Module[{product, isIdentity},
    product = Together[A . invMatrix];
    isIdentity = product === IdentityMatrix[n];

    AppendTo[content, Grid[
      {{
        labeledMatrixBlock[Row[{Style["A", Italic], " \[CenterDot] ", inverseASymbol[]}], styledPlainMatrix[dotProductTooltipMatrix[A, invMatrix]]],
        Style["=", Bold, FontSize -> 18],
        labeledMatrixBlock[Style["E", Italic], styledPlainMatrix[IdentityMatrix[n]]],
        If[isIdentity, Style["OK", Darker[Green], Bold], Style["CHYBA", Red, Bold]]
      }},
      Alignment -> {Center, Center, Center, Center},
      Spacings -> {1.2, 0},
      BaseStyle -> {FontSize -> 13}
    ]];
  ];

  content = Join[content, verificationSteps[data, xResult]];

  <|"Content" -> content, "Solution" -> xResult, "InverseMatrix" -> invMatrix|>
];

stepsLU[data_Association] := Module[
  {
    content = {}, n, A, b, vars, luData, L, U, y, x, tmp,
    addHeader, addText, addMatrixPair, addVector, addFormula, addSubHeader, resultStyle,
    prettyMatrix, prettyVector, appendProductDisplay, appendMatrixEquality, appendVectorEquality,
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

  addVector[label_, vec_] := AppendTo[content, highlightGrid @ Grid[
    {{prettyVector[Style[label, Italic], vec]}},
    Alignment -> Left,
    Spacings -> {1, 0}
  ]];

  appendProductDisplay[left_, right_, result_] := AppendTo[content, highlightGrid @ Grid[
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
  formatForwardEquation[row_, rhs_, i_] := Module[{coeffList, symbolList},
    coeffList = row[[1 ;; i]];
    symbolList = Table[luScalarSymbol["y", k], {k, 1, i}];
    formatLinearEquation[coeffList, symbolList, rhs]
  ];

  (* rovnica pre U.x = y *)
  formatBackwardEquation[row_, rhs_, i_] := Module[{coeffList, symbolList},
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
  buildUFormulaLines[i_, j_, terms_, value_] := Module[{symbolicTerms},
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
  buildLFormulaLines[j_, i_, terms_, pivot_, value_] := Module[{symbolicTerms},
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

  addHeader["Maticový tvar a používané vzťahy"];
  addText["Sústavu prepíšeme do maticového tvaru."];

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

  addText["Pri LU rozklade budeme používať tieto vzťahy:"];

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
  addText["Na začiatku poznáme len jednotkovú diagonálu matice L. Ostatné prvky matíc L a U dopočítame postupne."];
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
      addText["Najprv vypočítame prvky matice U."];
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
      addText["Po tomto kroku dostaneme:"];
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
  appendMatrixEquality[Row[{Style["L", Italic], " \[CenterDot] ", Style["U", Italic]}], luProduct, Style["A", Italic], A, luProduct === A];

  addHeader["Riešenie pomocnej sústavy L \[CenterDot] y = b"];
  addText["Keďže L je dolná trojuholníková matica s jednotkami na diagonále, vektor y určíme dopredným dosadzovaním."];
  AppendTo[content, alignedAugmentedMatrix[augFromAb[L, b], {}, <|"BoldDiagonal" -> True, "LeftLabel" -> Style["L", Italic], "RightLabel" -> Style["b", Italic]|>]];

  tmp = appendTriangularSubstitutionSteps[
    L,
    b,
    Table[luScalarSymbol["y", k], {k, 1, n}],
    ConstantArray[0, n],
    Range[n],
    content
  ];

  y = tmp[[1]];
  content = tmp[[2]];

  addVector["y", y];

  addHeader["Riešenie sústavy U \[CenterDot] x = y"];
  addText["Keď poznáme vektor y, vyriešime sústavu U \[CenterDot] x = y spätným dosadzovaním."];
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

stepsCholesky[data_Association] := Module[
  {
    content = {}, n, A, b, vars, choleskyData, L, LT, y, x, tmp,
    addHeader, addText, addSubHeader, addFormula, addMatrixPair, addVector,
    prettyMatrix, prettyVector, appendProductDisplay, appendMatrixEquality, appendVectorEquality,
    i, j, productCheck, lowerCheck, upperCheck, ySymbols,
    currentLBoldPositions, currentLTBoldPositions
  },

  n = data["n"];
  A = data["A"];
  b = data["b"];
  vars = data["Vars"];
  ySymbols = Table[luScalarSymbol["y", k], {k, 1, n}];

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

  addVector[label_, vec_] := AppendTo[content, highlightGrid @ Grid[
    {{prettyVector[Style[label, Italic], vec]}},
    Alignment -> Left,
    Spacings -> {1, 0}
  ]];

  appendProductDisplay[left_, right_, result_] := AppendTo[content, highlightGrid @ Grid[
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

  addHeader["Maticový tvar a používané vzťahy"];
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

  addText["Pri Choleského rozklade budeme používať tieto vzťahy:"];

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
  addText[Row[{"Prvky matice L vypočítame postupne po stĺpcoch. Po každom kroku si ukážeme tvar matíc L a ", transposeLSymbol[], "."}]];
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
    addText["Po tomto kroku dostaneme:"];
    addMatrixPair[L, LT, currentLBoldPositions[i], currentLTBoldPositions[i]];
    ,
    {i, 1, n}
  ];

  LT = Transpose[L];
  y = choleskyData["Y"];
  x = choleskyData["X"];

  addHeader[Row[{"Hotový rozklad A = L \[CenterDot] ", transposeLSymbol[]}]];
  addText[Row[{"Po výpočte dostaneme dolnú trojuholníkovú maticu L a jej transpozíciu ", transposeLSymbol[], "."}]];
  addMatrixPair[L, LT, currentLBoldPositions[n], currentLTBoldPositions[n]];

  addHeader[Row[{"Overenie rozkladu L \[CenterDot] ", transposeLSymbol[], " = A"}]];
  addText[Row[{"Skontrolujeme, že súčin L \[CenterDot] ", transposeLSymbol[], " sa rovná matici A."}]];
  addText["Tip: Keď prejdete kurzorom nad prvky výslednej matice, zobrazí sa výpočet, z ktorého dané číslo vzniklo."];

  productCheck = Together[L . LT];

  appendProductDisplay[L, LT, productCheck];
  appendMatrixEquality[Row[{Style["L", Italic], " \[CenterDot] ", transposeLSymbol[]}], productCheck, Style["A", Italic], A, productCheck === A];

  addHeader["Riešenie pomocnej sústavy L \[CenterDot] y = b"];
  addText["Keďže L je dolná trojuholníková matica, vektor y určíme dopredným dosadzovaním."];
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

  addVector["y", y];

  addHeader[Row[{"Riešenie sústavy ", transposeLSymbol[], " \[CenterDot] x = y"}]];
  addText[Row[{"Keď poznáme vektor y, vyriešime sústavu ", transposeLSymbol[], " \[CenterDot] x = y spätným dosadzovaním."}]];
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

stepsCramer[data_Association] := Block[{cramerKnown3x3 = {}}, Module[
  {
    content = {}, n, A, b, vars, solveData, detData, auxData, auxLabel,
    addHeader, addText
  },

  n = data["n"];
  A = data["A"];
  b = data["b"];
  vars = data["Vars"];

  solveData = cramerSolveData[A, b];

  addHeader[text_] := appendStepHeader[content, text];
  addText[text_] := AppendTo[content, text];

  addHeader["Prepis sústavy do maticového tvaru"];
  addText["Sústavu zapíšeme v tvare A \[CenterDot] x = b. Potom vypočítame determinant matice A a determinanty pomocných matíc."];
  AppendTo[content, Grid[
    {{
      matrixBlock[Style["A", Italic], A],
      Spacer[20],
      vectorBlock[Style["b", Italic], b]
    }},
    Alignment -> {Center, Center, Center},
    Spacings -> {1.2, 0}
  ]];

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
verificationStepsNone[data_Association] := Module[{content = {}, A = data["A"], b = data["b"], aug0, rA, rAug, n},

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

(* ~-~-~ TASK / RESULT PRINTING ~-~-~ *)

printDefaultTask[data_Association, vars_List] := Module[{},
  printTextCell["Riešte sústavu rovníc."];
  printFormulaCell @ Grid[
    List /@ (tf /@ buildTaskEquations[data["A"], data["b"], vars]),
    Alignment -> Left,
    Spacings -> {0, 0.8}
  ];
];

printTaskInverse[data_Association, vars_List] := Module[{},
  printTextCell["Vypočítajte inverznú maticu a potom pomocou nej určte riešenie sústavy."];
  printFormulaCell @ Grid[
    List /@ (tf /@ buildTaskEquations[data["A"], data["b"], vars]),
    Alignment -> Left,
    Spacings -> {0, 0.8}
  ];
];

printTaskLU[data_Association, vars_List] := Module[{},
  printTextCell["Rozložte maticu sústavy pomocou LU rozkladu (Doolittle, bez pivotovania) v tvare A = L · U, kde L má jednotky na diagonále. Potom vyriešte sústavy L · y = b a U · x = y."];
  printFormulaCell @ Grid[
    List /@ (tf /@ buildTaskEquations[data["A"], data["b"], vars]),
    Alignment -> Left,
    Spacings -> {0, 0.8}
  ];
  printTextCell["Pracujte priamo s maticami L a U bez pivotovania."];
];

printTaskCholesky[data_Association, vars_List] := Module[{},
  printCellStyle[
    BoxData @ ToBoxes[
      Row[{"Rozložte maticu sústavy pomocou Choleského rozkladu v tvare A = L \[CenterDot] ", transposeLSymbol[], ". Potom vyriešte sústavy L \[CenterDot] y = b a ", transposeLSymbol[], " \[CenterDot] x = y."}],
      StandardForm
    ], "Text"
  ];
  printFormulaCell @ Grid[
    List /@ (tf /@ buildTaskEquations[data["A"], data["b"], vars]),
    Alignment -> Left,
    Spacings -> {0, 0.8}
  ];
];

printTaskCramer[data_Association, vars_List] := Module[{},
  printTextCell["Riešte sústavu rovníc pomocou Cramerovho pravidla."];
  printFormulaCell @ Grid[
    List /@ (tf /@ buildTaskEquations[data["A"], data["b"], vars]),
    Alignment -> Left,
    Spacings -> {0, 0.8}
  ];
  printTextCell["Najprv vypočítajte determinant matice A a potom determinanty matíc, ktoré vzniknú nahradením jednotlivých stĺpcov vektorom b."];
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
    Module[{solExprs = infiniteSolutionFromSolvedAug[data], paramIdxs},
      paramIdxs = Lookup[data, "ParamIdxs", {}];

      printFormulaCell[
        If[Length[paramIdxs] === 1,
          Row[{"K = { [", Row @ Riffle[TraditionalForm /@ solExprs, ", "], "], ", \[FormalT], " \[Element] ", Integers, " }"}],
          Row[{"K = { [", Row @ Riffle[TraditionalForm /@ solExprs, ", "], "], ", \[FormalS], ", ", \[FormalT], " \[Element] ", Integers, " }"}]
        ]
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

printResultCholesky[data_Association, vars_List, st_, steps_] := Module[
  {solution, lMatrix, ltMatrix, yVector},

  solution = Which[
    AssociationQ[steps] && KeyExistsQ[steps, "Solution"], steps["Solution"],
    KeyExistsQ[data, "x"], data["x"],
    True, Missing["NotAvailable"]
  ];

  lMatrix = Which[
    AssociationQ[steps] && KeyExistsQ[steps, "L"], steps["L"],
    True, Missing["NotAvailable"]
  ];

  ltMatrix = If[MatrixQ[lMatrix], Transpose[lMatrix], Missing["NotAvailable"]];

  yVector = Which[
    AssociationQ[steps] && KeyExistsQ[steps, "Y"], steps["Y"],
    True, Missing["NotAvailable"]
  ];

  If[MatrixQ[lMatrix],
    printTextCell["Matica L:"];
    printFormulaCell[TraditionalForm[MatrixForm[lMatrix]]];
  ];

  If[MatrixQ[ltMatrix],
    printCellStyle[BoxData @ ToBoxes[Row[{"Matica ", transposeLSymbol[], ":"}], StandardForm], "Text"];
    printFormulaCell[TraditionalForm[MatrixForm[ltMatrix]]];
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
    plainEquationLine[cramerDetLabel[Style["A", Italic]], detA]
  ];

  printTextCell["Pomocné determinanty:"];
  printFormulaCell[
    Grid[
      Table[
        {
          plainEquationLine[
            cramerDetLabel[cramerMatrixLabel[vars[[i]]]],
            auxDeterminants[[i]]
          ]
        },
        {i, 1, Length[vars]}
      ],
      Alignment -> Left,
      Spacings -> {0, 0.8}
    ]
  ];

  If[ListQ[solution],
    printTextCell["Riešenie sústavy:"];
    printFormulaCell[
      Row[Flatten[{
        "(",
        Riffle[vars, ", "],
        ") = (",
        Riffle[TraditionalForm /@ solution, ", "],
        ")"
      }]]
    ];
  ];
];

(* ~-~-~ MAIN CONTROLLER ~-~-~ *)

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

  n = DimensionByMethodDifficulty[spec["DimKey"], diff];
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

  (*If[KeyExistsQ[data, "RetryCount"],*)
    (*printTextCell["Počet pregenerovaní: " <> ToString[data["RetryCount"]]];*)
  (*];*)

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

(* generátor Gaussovej eliminačnej metódy *)
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

(* generátor Gauss-Jordanovej metódy *)
GenGaussJordan[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenGaussJordan,
    "MsgPrefix" -> GenGaussJordan,
    "DimKey" -> "GaussJordan",
    "SectionTitle" -> "Gauss-Jordanova metóda",
    "ScrambleFn" -> genScrambleGauss,
    "StepsFn" -> (stepsGaussJordanShared[#, False, False] &),
    "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "UseForwardBoundRetry" -> True,
    "ForwardPivotMode" -> "ZERO"
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

(* generátor pivotovaného Gauss-Jordana *)
GenGaussJordanPivot[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenGaussJordanPivot,
    "MsgPrefix" -> GenGaussJordanPivot,
    "DimKey" -> "GaussJordanPivot",
    "SectionTitle" -> "Gauss-Jordanova metóda s pivotovaním",
    "ScrambleFn" -> genScrambleGaussJordanPivot,
    "StepsFn" -> (stepsGaussJordanShared[#, True, False] &),
    "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "UseForwardBoundRetry" -> True,
    "ForwardPivotMode" -> "MIN"
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

(* generátor Gauss-Jordana s elementárnymi maticami *)
GenElemGJ[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenElemGJ,
    "MsgPrefix" -> GenElemGJ,
    "DimKey" -> "ElemGaussJordan",
    "SectionTitle" -> "Gauss-Jordanova metóda pomocou elementárnych matíc",
    "ScrambleFn" -> genScrambleGauss,
    "StepsFn" -> (stepsGaussJordanShared[#, False, True] &),
    "ValidateExtra" -> validateOnlyOneSolutionType,
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "UseForwardBoundRetry" -> True,
    "ForwardPivotMode" -> "ZERO"
  |>;

  runMatrixGenerator[spec, diff, mode, opts]
];

(* generátor výpočtu inverznej matice *)
GenInverse[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenInverse,
    "MsgPrefix" -> GenInverse,
    "DimKey" -> "Inverse",
    "SectionTitle" -> "Výpočet inverznej matice",
    "ScrambleFn" -> genScrambleGauss,
    "StepsFn" -> stepsInverseMatrix,
    "ValidateExtra" -> validateOnlyOneSolutionType,
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "TaskPrinter" -> printTaskInverse,
    "ResultPrinter" -> printResultInverse,
    "UseForwardBoundRetry" -> True,
    "ForwardPivotMode" -> "MIN",
    "ForwardBoundAugFn" -> Function[data, Join[data["A"], IdentityMatrix[data["n"]], 2]],
    "ForwardBoundCheckFn" -> gaussJordanEliminationWithinBoundsQ
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
    "ValidateExtra" -> validateOnlyOneSolutionType,
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "TaskPrinter" -> printTaskLU,
    "ResultPrinter" -> printResultLU,
    "UseForwardBoundRetry" -> True,
    "ForwardBoundAugFn" -> Function[data, data],
    "ForwardBoundCheckFn" -> Function[{data, pivotMode}, luDecompositionWithinBoundsQ[data]]
  |>;
  runMatrixGenerator[spec, diff, mode, opts]
];

GenCholesky[diff_String, mode_String, opts : OptionsPattern[]] := Module[{spec},
  spec = <|
    "EntryFn" -> GenCholesky,
    "MsgPrefix" -> GenCholesky,
    "DimKey" -> "Cholesky",
    "SectionTitle" -> "Choleského rozklad",
    "ScrambleFn" -> genScrambleCholesky,
    "StepsFn" -> stepsCholesky,
    "ValidateExtra" -> validateOnlyOneSolutionType,
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"],
    "TaskPrinter" -> printTaskCholesky,
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
    "TaskPrinter" -> printTaskCramer,
    "ResultPrinter" -> printResultCramer
  |>;

  runMatrixGenerator[spec, diff, mode, opts]
];

End[];
EndPackage[];