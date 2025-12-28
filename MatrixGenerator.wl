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

(* definícia usage a chybových hlášok *)
GenTriangular::usage =
    "GenTriangular[diff, mode, opts] vygeneruje príklad riešenia trojuholníkovej sústavy pomocou augmentovanej matice
a dosadzovania po riadkoch.

diff: \"EASY\" (4x4), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\", \"TASK_RESULT\", \"TASK_STEPS_RESULT\"
opts: SolutionType -> Automatic|\"ONE\"|\"NONE\"|\"INFINITE\",
      TriangularType -> Automatic|\"L\"|\"U\"";

GenGauss::usage = "GenGauss[diff, mode, opts] (zatiaľ neimplementované).";
GenGaussJordan::usage = "GenGaussJordan[diff, mode, opts] (zatiaľ neimplementované).";
GenGaussJordanPivot::usage = "GenGaussJordanPivot[diff, mode, opts] (zatiaľ neimplementované).";


GenTriangular::baddiff  = "Neplatná obtiažnosť `1`. Použi \"EASY\"|\"MEDIUM\"|\"HARD\".";
GenTriangular::badmode  = "Neplatný režim `1`. Použi \"TASK\"|\"TASK_RESULT\"|\"TASK_STEPS_RESULT\".";
GenTriangular::badst    = "Neplatný typ riešenia `1`. Použi Automatic|\"ONE\"|\"NONE\"|\"INFINITE\".";
GenTriangular::badtri   = "Neplatný typ trojuholníkovej sústavy `1`. Použi Automatic|\"L\"|\"U\".";
GenTriangular::fail     = "Nepodarilo sa vygenerovať vhodný príklad.";

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

(* predvolené možnosti balíka *)
$CommonGeneratorOptions = {
  SolutionType -> Automatic,
  TriangularType -> Automatic
};

Options[GenTriangular] = $CommonGeneratorOptions;
Options[GenGauss] = $CommonGeneratorOptions;
Options[GenGaussJordan] = $CommonGeneratorOptions;
Options[GenGaussJordanPivot] = $CommonGeneratorOptions;

Begin["`Private`"];

(* ~-~-~ VALIDATION ~-~-~ *)

DimensionByDifficulty[generatorKey_String, diff_String] := Switch[
  diff, "EASY", 4, "MEDIUM", 5, "HARD", 6, _, 4
];


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
buildTaskEquations[A_, b_, vars_] := MapThread[HoldForm[#1 == #2] &, {A.vars, b}];
toAugmented[A_, b_] := Join[A, List /@ b, 2];

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
    If[IntegerQ[activeRow] && i === activeRow,
      bg = rowColor,
      If[MemberQ[sourceRows, i], bg = sourceColor]
    ];
    Item[expr, Background -> bg]
  ];

  makeCell[i_, j_, val_] := Module[
    {cell = TraditionalForm[val], isGreen, showPivotQ, isDiag},

    isGreen = MemberQ[greenCells, {i, j}];
    isDiag = boldDiagQ && (j <= nA) && (i === j);

    showPivotQ = ListQ[pivotPos] &&
        (
          (IntegerQ[activeRow] && activeRow === pivotPos[[1]]) ||
              MemberQ[sourceRows, pivotPos[[1]]]
        );

    If[isGreen,
      cell = Style[cell, Darker[Green], Bold],
      If[showPivotQ && pivotPos === {i, j},
        cell = Style[cell, Bold],
        If[isDiag,
          cell = Style[cell, Bold]
        ]
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
    ItemSize -> {
      ({#, Automatic} & /@ Join[{0.2}, ConstantArray[1.2, nA], {0.2, 1.2, 0.2}]),
      Automatic
    }
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

(* malé rozsahy pre generovanie – hranice riešime len výberom malých čísel *)
$bRange = {-10, 10};

(* koeficienty pre riadkové operácie *)
kSetByN[n_] := If[n <= 4, DeleteCases[Range[-5, 5], 0], DeleteCases[Range[-2, 2], 0]];
mSetByN[n_] := If[n <= 4, DeleteCases[Range[-4, 4], 0], {-2, -1, 1, 2}];

$rhsNonzeroRange = Join[Range[-10, -1], Range[1, 10]];
$paramSet = DeleteCases[Range[-3, 3], 0];

(* náhodný nenulový prvok z množiny *)
randomNonzeroFromSet[set_List] := RandomChoice[set];

(* vyrobí vyriešenú augmentovanú maticu [A|b] podľa prototypu *)
makeSolvedAugmented[n_Integer, solType_String, triType_String] := Module[
  {A, b, x, idx, paramIdx, badRow},

  A = IdentityMatrix[n];
  b = RandomInteger[$bRange, n];

  idx = RandomInteger[{1, n}];
  paramIdx = Missing["NotApplicable"];
  badRow = Missing["NotApplicable"];

  Switch[solType,
    "ONE",
    x = b;
    ,
    "INFINITE",
    A[[idx]] = ConstantArray[0, n];
    b[[idx]] = 0;

    If[triType === "U",
      Do[
        A[[i, idx]] = randomNonzeroFromSet[kSetByN[n]],
        {i, 1, idx - 1}
      ],
      (* triType === "L" *)
      Do[
        A[[i, idx]] = randomNonzeroFromSet[kSetByN[n]],
        {i, idx + 1, n}
      ]
    ];

    x = "INFINITE";
    paramIdx = idx;
    ,
    "NONE",
    A[[idx]] = ConstantArray[0, n];
    b[[idx]] = RandomChoice[$rhsNonzeroRange];

    x = "NONE";
    badRow = idx;
  ];

  <|
    "Aug" -> toAugmented[A, b],
    "x" -> x,
    "BadRow" -> badRow,
    "ParamIdx" -> paramIdx
  |>
];

(* generovanie dát aby postup bol bez zlomkov *)
generateData[n_, solType_, triType_, scrambleFn_] := Module[
  {solved, augSolved, augTask, A, b, vars},

  solved = makeSolvedAugmented[n, solType, triType];
  augSolved = solved["Aug"];

  augTask = scrambleFn[augSolved, triType];

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
    "Vars" -> vars,
    "n" -> n,
    "BadRow" -> solved["BadRow"],
    "ParamIdx" -> solved["ParamIdx"]
  |>
];

(* scrambling a škálovanie pivotu *)
scrambleAugmented[aug0_, triType_String] := Module[
  {aug = aug0, n, i, r, k, m},

  n = Length[aug];

  If[triType === "U",
    (* U: nechaj presne ako doteraz fungovalo *)
    Do[
      Do[
        k = randomNonzeroFromSet[kSetByN[n]];
        aug = rowAddMultiple[aug, r, i, k],
        {r, 1, i - 1}
      ];

      m = randomNonzeroFromSet[mSetByN[n]];
      aug = rowScale[aug, i, m];
      ,
      {i, 1, n}
    ],
    (* L: iba zmeň smer pivotov (zdola hore) *)
    Do[
      Do[
        k = randomNonzeroFromSet[kSetByN[n]];
        aug = rowAddMultiple[aug, r, i, k],
        {r, i + 1, n}
      ];

      m = randomNonzeroFromSet[mSetByN[n]];
      aug = rowScale[aug, i, m];
      ,
      {i, n, 1, -1}
    ]
  ];

  aug
];

scrambleAugmentedGauss[aug0_, triType_String] := Module[
  {aug, n, i, r, k},
  aug = scrambleAugmented[aug0, "U"];
  n = Length[aug];

  Do[
    Do[
      If[RandomReal[] < 0.65,
        k = randomNonzeroFromSet[kSetByN[n]];
        aug = rowAddMultiple[aug, r, i, k];
        aug = normalizeAugRow[aug, r];
      ],
      {r, i + 1, n}
    ],
    {i, 1, n - 1}
  ];

  aug
];

scrambleAugmentedGaussJordan[aug0_, triType_String] := scrambleAugmentedGauss[aug0, triType];


(* ~-~-~ ROW OPERATIONS - delenie, kombinácia ~-~-~ *)

(* poznámka pre delenie riadku *)
rowOpDivideNote[i_, p_] := Row[{"R", i, " \[LeftArrow] R", i, " / ", tf[p]}];
applyRowOpDivide[aug_, i_Integer, p_Integer] := ReplacePart[aug, i -> (aug[[i]]/p)];

(* poznámka pre kombináciu riadkov *)
rowOpCombineNote[i_, terms_List] := Module[
  {base = Row[{"R", i, " \[LeftArrow] R", i}]},
  Row @ Prepend[
    (Row[{If[#2 < 0, " - ", " + "], tf[Abs[#2]], "\[CenterDot]R", #1}] & @@@ terms),
    base
  ]
];

applyRowOpCombine[aug_, i_Integer, terms_List] := Module[
  {row = aug[[i]]},
  ReplacePart[
    aug,  i -> (row + Total[terms[[All, 2]] aug[[terms[[All, 1]]]]])
  ]
];

(* R_r <- R_r + k R_i *)
rowAddMultiple[aug_, r_Integer, i_Integer, k_Integer] := ReplacePart[aug, r -> (aug[[r]] + k*aug[[i]])];

(* R_i <- m R_i *)
rowScale[aug_, i_Integer, m_Integer] := ReplacePart[aug, i -> (m*aug[[i]])];

applyRowOpElim[aug_, r_Integer, i_Integer, p_Integer, a_Integer] := ReplacePart[aug, r -> (p*aug[[r]] - a*aug[[i]])];

(* zobrazenie matice pred a po úprave *)
renderBeforeAfter[before_, after_, notes_, hiBefore_, hiAfter_] := Grid[
  {{alignedAugmentedMatrix[before, notes, hiBefore], Spacer[18], alignedAugmentedMatrix[after, {}, hiAfter]}},
  Alignment -> {Left, Center, Left}, Spacings -> {0, 0}
];



(* ~-~-~ MATRIX ROW HELPERS ~-~-~ *)

rowAbsGCD[row_List] := Module[{g = Apply[GCD, Abs[row]]}, If[g === 0, 1, g]];

normalizeRowByGCD[row_List] := Module[{g = rowAbsGCD[row]}, If[g > 1, row/g, row]];

normalizeAugRow[aug_, r_Integer] := ReplacePart[aug, r -> normalizeRowByGCD[aug[[r]]]];

choosePivotRow[aug_, i_Integer] := Module[
  {n = Length[aug], candidates, best},
  candidates = Select[Range[i, n], aug[[#, i]] =!= 0 &];
  If[candidates === {},
    i,
    best = First @ MinimalBy[candidates, {Abs[aug[[#, i]]], Total[Abs[aug[[#]]]]} &];
    best
  ]
];

rowOpSwapNote[i_, k_] := Row[{"R", i, " \[LeftRightArrow] R", k}];

applyRowOpSwap[aug_, i_Integer, k_Integer] := ReplacePart[aug, {i -> aug[[k]], k -> aug[[i]]}];

rowOpElimNote[r_, i_, p2_, a2_, divG_] := Module[
  {leftPart, rightPart, op, base},

  (* ľavá časť: p2·Rr, ale ak p2==1 tak len Rr *)
  leftPart =
      If[p2 === 1,
        Row[{"R", r}],
        Row[{tf[p2], "\[CenterDot]", "R", r}]
      ];

  (* pravá časť: a2·Ri, ale ak |a2|==1 tak bez čísla a bez bodky *)
  rightPart =
      Which[
        Abs[a2] === 1, Row[{"R", i}],
        True,          Row[{tf[Abs[a2]], "\[CenterDot]", "R", i}]
      ];

  (* znamienko medzi nimi: a2<0 znamená + (lebo máme - a2·Ri), inak - *)
  op = If[a2 < 0, " + ", " - "];

  base = Row[{ "R", r, " \[LeftArrow] ", leftPart, op, rightPart }];

  If[divG > 1, Row[{base, " , potom / ", tf[divG]}], base]
];

applyRowOpElimStable[aug_, r_Integer, i_Integer] := Module[
  {p, a, g1, p2, a2, rowNew, g2},
  p = aug[[i, i]];
  a = aug[[r, i]];

  If[a === 0,
    <|"Aug" -> aug, "p2" -> 0, "a2" -> 0, "DivG" -> 1|>,
    g1 = GCD[p, a];
    p2 = p/g1;
    a2 = a/g1;

    rowNew = p2*aug[[r]] - a2*aug[[i]];
    g2 = rowAbsGCD[rowNew];
    If[g2 > 1, rowNew = rowNew/g2];

    <|"Aug" -> ReplacePart[aug, r -> rowNew], "p2" -> p2, "a2" -> a2, "DivG" -> g2|>
  ]
];




(* ~-~-~ STEP GENERATION ~-~-~ *)

stepsTriangular[data_Association] := Module[
  {content = {}, n, aug, vars, tri, st, order, addHeader, addText, addMatrix, addConclusion, addCheckHeader, notes, result, sol},

  n = data["n"]; aug = data["Aug"]; vars = data["Vars"]; tri = data["TriType"]; st = data["SolutionType"];
  order = If[tri === "U", Range[n, 1, -1], Range[1, n]];

  addHeader[text_] := AppendTo[content, makeStepHeader[text]];
  addText[text_] := AppendTo[content, text];
  addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] := AppendTo[content, alignedAugmentedMatrix[m, rowNotes, hi]];
  addConclusion[lines_List] := (addHeader["Záver"]; Scan[addText, lines]);
  addCheckHeader[extra_List : {}] := (addHeader["Skúška správnosti"]; Scan[addText, extra]);

  addHeader["Prepis sústavy do augmentovanej matice"];
  addText["Zapíšeme sústavu do augmentovanej matice a ďalej pracujeme už len s maticou."];
  addMatrix[aug];

  result = Switch[st,

    "ONE",
    Module[{before, after, terms, p, solLocal},
      solLocal = ConstantArray[None, n];

      addHeader["Riadkové úpravy"];
      addText["Postupne upravujeme riadky tak, aby sa dali premenné dopočítať dosadzovaním po riadkoch."];

      Do[
        terms = If[tri === "U",
          Select[Table[{j, -aug[[i, j]]}, {j, i + 1, n}], #[[2]] =!= 0 &],
          Select[Table[{j, -aug[[i, j]]}, {j, 1, i - 1}], #[[2]] =!= 0 &]
        ];

        If[terms =!= {},
          before = aug;
          after = applyRowOpCombine[before, i, terms];
          notes = ConstantArray["", n]; notes[[i]] = rowOpCombineNote[i, terms];
          AppendTo[content, renderBeforeAfter[
            before, after, notes,
            <|"ActiveRow" -> i, "SourceRows" -> terms[[All, 1]], "PivotPos" -> {i, i}|>,
            <|"ActiveRow" -> i, "SourceRows" -> terms[[All, 1]], "PivotPos" -> {i, i}|>
          ]];
          aug = after;
        ];

        p = aug[[i, i]];
        If[p =!= 1,
          before = aug;
          after = applyRowOpDivide[before, i, p];
          notes = ConstantArray["", n]; notes[[i]] = rowOpDivideNote[i, p];
          AppendTo[content, renderBeforeAfter[
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

      addCheckHeader[{"Výpočet overíme porovnaním A \[CenterDot] x s pravou stranou b (po riadkoch)."}];
      content = Join[content, verificationSteps[data, solLocal]];

      <|"Solution" -> solLocal|>
    ],

    "NONE",
    Module[{badIdx},
      badIdx = data["BadRow"];

      addHeader["Analýza riadkov"];
      addText["Hľadáme riadok, ktorý predstavuje spor (nulové koeficienty, ale nenulová pravá strana)."];

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
      addText["Nulový riadok znamená, že jedna premenná môže byť voľná (parameter)."];

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

      addHeader["Vyjadrenie ostatných premenných"];

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


      addCheckHeader[{"Dosadíme parametrické riešenie do pôvodných rovníc. Po úprave musí v každom riadku vyjsť identita (napr. 0 = 0) pre ľubovoľné \[FormalT] ∈ ℤ."}];
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

stepsGauss[data_Association] := Module[
  {content = {}, n, aug, vars, st, addHeader, addText, addMatrix, notes,
    before, after, i, r, kPivot, elimRes, pNow, solLocal, rhs, sum},

  n = data["n"]; aug = data["Aug"]; vars = data["Vars"]; st = data["SolutionType"];

  addHeader[text_] := AppendTo[content, makeStepHeader[text]];
  addText[text_] := AppendTo[content, text];
  addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] := AppendTo[content, alignedAugmentedMatrix[m, rowNotes, hi]];

  addHeader["Prepis sústavy do augmentovanej matice"];
  addText["Zapíšeme sústavu do augmentovanej matice a vykonáme doprednú elimináciu (Gauss)."];
  addMatrix[aug];

  If[st =!= "ONE",
    addHeader["Poznámka"];
    addText["V tejto verzii je pre Gauss metódu dopracovaný iba prípad SolutionType -> \"ONE\"."];
    Return[<|"Content" -> content, "Solution" -> "NOT_IMPLEMENTED"|>];
  ];

  addHeader["Dopredná eliminácia (na horný trojuholník)"];
  addText["Nulujeme prvky pod diagonálou celočíselne. Koeficienty redukujeme cez gcd a riadky priebežne normalizujeme."];

  Do[
    kPivot = choosePivotRow[aug, i];
    If[kPivot =!= i,
      before = aug;
      after = applyRowOpSwap[before, i, kPivot];
      notes = ConstantArray["", n]; notes[[i]] = rowOpSwapNote[i, kPivot];
      AppendTo[content, renderBeforeAfter[
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
        elimRes = applyRowOpElimStable[before, r, i];
        after = elimRes["Aug"];

        notes = ConstantArray["", n];
        notes[[r]] = rowOpElimNote[r, i, elimRes["p2"], elimRes["a2"], elimRes["DivG"]];

        AppendTo[content, renderBeforeAfter[
          before, after, notes,
          <|"ActiveRow" -> r, "SourceRows" -> {i}, "PivotPos" -> {i, i}|>,
          <|"ActiveRow" -> r, "SourceRows" -> {i}, "PivotPos" -> {i, i}, "GreenCells" -> {{r, i}}|>
        ]];

        aug = after;
      ],
      {r, i + 1, n}
    ],
    {i, 1, n - 1}
  ];

  addHeader["Tvar po Gaussovej eliminácii"];
  addText["Dostali sme hornú trojuholníkovú sústavu. Premenné dopočítame v rovniciach spätným dosadzovaním."];
  addMatrix[aug, {}, <|"BoldDiagonal" -> True|>];

  addHeader["Dopočet riešenia v rovniciach (spätné dosadzovanie)"];
  solLocal = ConstantArray[0, n];

  Do[ Module[{row, pivot, rhsVal, terms, symExpr, subExpr, sumProducts, numVal},

      row = aug[[i]];
      pivot = row[[i]];
      rhsVal = row[[n + 1]];
      terms = Select[Table[{row[[j]], vars[[j]], solLocal[[j]]}, {j, i + 1, n}], #[[1]] =!= 0 &];
      boldNum[val_] := Style[If[val < 0, Row[{"(", tft[val], ")"}], tft[val]], Bold];
      coeffTimes[a_, x_] := If[a === 1, x, Row[{tf[a], "\[CenterDot]", x}]];


      symExpr = Row @ Flatten @ Join[
        {tft[rhsVal]},
        Table[
          With[{a = terms[[k, 1]], v = Style[tf[terms[[k, 2]]], Bold]},
            {If[a > 0, " - ", " + "], coeffTimes[Abs[a], v]}
          ],
          {k, Length[terms]}
        ]
      ];

      If[i < n,

        AppendTo[content,
        Which[pivot === 1, Row[{tf[vars[[i]]], " = ", symExpr}],
          pivot === -1, Row[{tf[vars[[i]]], " = -(", symExpr, ")"}],
          True, Row[{tf[vars[[i]]], " = (", symExpr, ")/", tf[pivot]}]
        ]
      ];

      subExpr = Row @ Flatten @ Join[
        {tft[rhsVal]},
        Table[
          With[{a = terms[[k, 1]], val = boldNum[terms[[k, 3]]]},
            {If[a > 0, " - ", " + "], coeffTimes[Abs[a], val]}
          ], {k, Length[terms]}
        ]
      ];

      AppendTo[content,
        Which[pivot === 1, Row[{tf[vars[[i]]], " = ", subExpr}],
          pivot === -1, Row[{tf[vars[[i]]], " = -(", subExpr, ")"}],
          True, Row[{tf[vars[[i]]], " = (", subExpr, ")/", tf[pivot]}]
        ]
      ];
      ];

      sumProducts = Total[terms[[All, 1]]*terms[[All, 3]]];
      numVal = (rhsVal - sumProducts)/pivot;
      solLocal[[i]] = numVal;

      AppendTo[content, highlightGrid @ Grid[
        {{tf[vars[[i]]], "=", tft[numVal]}},
        Alignment -> {{Right, Center, Left}},
        BaseStyle -> {FontSize -> 16}
      ]];
    ],
    {i, n, 1, -1}
  ];


addHeader["Skúška správnosti"];
addText["Overíme porovnaním A \[CenterDot] x s pravou stranou b (po riadkoch)."];
content = Join[content, verificationSteps[data, solLocal]];

<|"Content" -> content, "Solution" -> solLocal|>
];

stepsGaussJordan[data_Association] := Module[
  {content = {}, n, aug, vars, st, addHeader, addText, addMatrix, notes,
    before, after, i, r, kPivot, elimRes, pNow, solLocal},

  n = data["n"]; aug = data["Aug"]; vars = data["Vars"]; st = data["SolutionType"];

  addHeader[text_] := AppendTo[content, makeStepHeader[text]];
  addText[text_] := AppendTo[content, text];
  addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] := AppendTo[content, alignedAugmentedMatrix[m, rowNotes, hi]];

  addHeader["Prepis sústavy do augmentovanej matice"];
  addText["Zapíšeme sústavu do augmentovanej matice a prevedieme ju na jednotkovú maticu (Gauss-Jordan)."];
  addMatrix[aug];

  If[st =!= "ONE",
    addHeader["Poznámka"];
    addText["V tejto verzii je pre Gauss-Jordan metódu dopracovaný iba prípad SolutionType -> \"ONE\"."];
    Return[<|"Content" -> content, "Solution" -> "NOT_IMPLEMENTED"|>];
  ];

  addHeader["Dopredná eliminácia (nulovanie pod diagonálou)"];
  addText["Nulujeme prvky pod diagonálou celočíselne, s redukciou cez gcd a normalizáciou riadkov."];

  Do[
    kPivot = choosePivotRow[aug, i];
    If[kPivot =!= i,
      before = aug;
      after = applyRowOpSwap[before, i, kPivot];
      notes = ConstantArray["", n]; notes[[i]] = rowOpSwapNote[i, kPivot];
      AppendTo[content, renderBeforeAfter[
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
        elimRes = applyRowOpElimStable[before, r, i];
        after = elimRes["Aug"];

        notes = ConstantArray["", n];
        notes[[r]] = rowOpElimNote[r, i, elimRes["p2"], elimRes["a2"], elimRes["DivG"]];

        AppendTo[content, renderBeforeAfter[
          before, after, notes,
          <|"ActiveRow" -> r, "SourceRows" -> {i}, "PivotPos" -> {i, i}|>,
          <|"ActiveRow" -> r, "SourceRows" -> {i}, "PivotPos" -> {i, i}, "GreenCells" -> {{r, i}}|>
        ]];


        aug = after;
      ],
      {r, i + 1, n}
    ],
    {i, 1, n - 1}
  ];

  addHeader["Spätná eliminácia (nulovanie nad diagonálou)"];
  addText["Nulujeme prvky nad diagonálou rovnakým stabilným celočíselným krokom."];

  Do[
    pNow = aug[[i, i]];
    If[pNow === 0, Continue[]];

    Do[
      If[aug[[r, i]] =!= 0,
        before = aug;
        elimRes = applyRowOpElimStable[before, r, i];
        after = elimRes["Aug"];

        notes = ConstantArray["", n];
        notes[[r]] = rowOpElimNote[r, i, elimRes["p2"], elimRes["a2"], elimRes["DivG"]];

        AppendTo[content, renderBeforeAfter[
          before, after, notes,
          <|"ActiveRow" -> r, "PivotPos" -> {i, i}|>,
          <|"ActiveRow" -> r, "PivotPos" -> {i, i}, "GreenCells" -> {{r, i}}|>
        ]];

        aug = after;
      ],
      {r, 1, i - 1}
    ],
    {i, n, 2, -1}
  ];

  addHeader["Normalizácia pivotov (na 1)"];
  addText["Každý pivotový riadok vydelíme pivotom, aby na diagonále vznikli jednotky. Pod maticou hneď zapíšeme zistenú premennú."];

  Do[
    pNow = aug[[i, i]];
    If[pNow === 0, Continue[]];

    If[pNow =!= 1,
      (* pivot nie je 1 -> spravíme delenie a ukážeme pred/po *)
      before = aug;
      after = applyRowOpDivide[before, i, pNow];
      notes = ConstantArray["", n]; notes[[i]] = rowOpDivideNote[i, pNow];

      AppendTo[content, renderBeforeAfter[
        before, after, notes,
        <|"ActiveRow" -> i, "PivotPos" -> {i, i}|>,
        <|"ActiveRow" -> i, "PivotPos" -> {i, i}, "GreenCells" -> {{i, i}, {i, n + 1}}|>
      ]];

      aug = after;
      ,
      (* pivot už je 1 -> nič nemeníme, ale ukážeme maticu s vyznačením *)
      addMatrix[
        aug,
        ConstantArray["", n],
        <|"ActiveRow" -> i, "PivotPos" -> {i, i}, "GreenCells" -> {{i, i}, {i, n + 1}}|>
      ];
    ];

    (* vždy hneď vypíšeme zistenú premennú *)
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

  solLocal = aug[[All, n + 1]];


  addHeader["Skúška správnosti"];
addText["Overíme porovnaním A \[CenterDot] x s pravou stranou b (po riadkoch)."];
content = Join[content, verificationSteps[data, solLocal]];

<|"Content" -> content, "Solution" -> solLocal|>
];


(* ~-~-~ SKÚŠKA SPRÁVNOSTI ~-~-~ *)

verificationStepsNone[data_Association] := Module[
  {content = {}, A = data["A"], b = data["b"], aug0, rA, rAug, n, badIdx, rhsVal},

  n = Length[b];
  aug0 = toAugmented[A, b];
  rA = MatrixRank[A];
  rAug = MatrixRank[aug0];

  AppendTo[content,
    Grid[
      {
        {Row[{"hodnosť(A) = ", rA}]},
        {Row[{"hodnosť([A|b]) = ", rAug}]},
        {If[rA < rAug,
          Style["hodnosť(A) < hodnosť([A|b])  \[Rule]  sústava nemá riešenie (OK)", Darker[Green]],
          Style["hodnosti sa nerovnajú tak, ako majú pre spor – over postup (CHYBA)", Red]
        ]}
      },
      Alignment -> Left,
      Spacings -> {0, 0.4},
      BaseStyle -> {FontSize -> 13}
    ]
  ];

  content
];
verificationStepsInfinite[data_Association, solExprs_List] := Module[
  {content = {}, A = data["A"], b = data["b"], n = data["n"], lhs, diff, okQ, coeffs},

  Do[
    lhs = Together[A[[i]].solExprs];
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
    ],
    {i, 1, n}
  ];

  content
];
verificationSteps[data_Association, sol_List] := Module[
  {content = {}, A = data["A"], b = data["b"], n = data["n"], lhs},

  Do[
    lhs = A[[i]].sol;
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
    ],
    {i, 1, n}
  ];

  content
];

(* ~-~-~ MAIN CONTROLLER ~-~-~ *)

runMatrixGenerator[spec_Association, diff_String, mode_String, opts : OptionsPattern[]] := Module[
  {n, vars, st, tri, data, steps, validateExtraQ, resolveExtra, sectionTitle, stepFn, scrambleFn, dimKey},

  (* spoločné validácie *)
  If[!TrueQ[ValidateDifficulty[diff]], Message[spec["MsgPrefix"]::baddiff, diff]; Return[]];
  If[!TrueQ[ValidateMode[mode]], Message[spec["MsgPrefix"]::badmode, mode]; Return[]];
  With[{stOpt = OptionValue[spec["EntryFn"], {opts}, SolutionType]},
    If[!TrueQ[ValidateSolutionType[stOpt]],
      Message[spec["MsgPrefix"]::badst, stOpt]; Return[]
    ];
  ];


  (* špecifické validácie (napr. TriangularType) *)
  validateExtraQ = Lookup[spec, "ValidateExtra", (True &)];
  If[!TrueQ[validateExtraQ[spec, opts]], Return[]];

  (* spoločné resolve *)
  st = ResolveSolutionType[OptionValue[spec["EntryFn"], {opts}, SolutionType]];

  (* špecifické resolve (napr. TriangularType) *)
  resolveExtra = Lookup[spec, "ResolveExtra", (Missing["NotUsed"] &)];
  tri = resolveExtra[spec, opts];

  (* rozmery *)
  dimKey = spec["DimKey"];
  n = DimensionByDifficulty[dimKey, diff];
  vars = buildVars[n];

  (* dáta *)
  scrambleFn = spec["ScrambleFn"];
  data = generateData[n, st, tri, scrambleFn];

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
      If[st === "INFINITE", printTextCell["Sústava má nekonečne veľa riešení."]];
    ];
  ];

];

GenTriangular[diff_String, mode_String, opts : OptionsPattern[]] := Module[
  {spec},

  spec = <|
    "EntryFn" -> GenTriangular,
    "MsgPrefix" -> GenTriangular,
    "DimKey" -> "Triangular",
    "SectionTitle" -> "Trojuholníková metóda",
    "ScrambleFn" -> scrambleAugmented,
    "StepsFn" -> stepsTriangular,

    "ValidateExtra" -> Function[{specLocal, passedOpts},
      With[{triOpt = OptionValue[specLocal["EntryFn"], {passedOpts}, TriangularType]},
        If[!TrueQ[validateTriangularType[triOpt]],
          Message[specLocal["MsgPrefix"]::badtri, triOpt];
          False,
          True
        ]
      ]
    ],


    "ResolveExtra" -> Function[{specLocal, passedOpts},
      resolveTriangularType[OptionValue[specLocal["EntryFn"], {passedOpts}, TriangularType]]
    ]

  |>;

  runMatrixGenerator[spec, diff, mode, opts]
];
GenGauss[diff_String, mode_String, opts : OptionsPattern[]] := Module[
  {spec},

  spec = <|
    "EntryFn" -> GenGauss,
    "MsgPrefix" -> GenGauss,
    "DimKey" -> "Gauss",
    "SectionTitle" -> "Gaussova eliminačná metóda",
    "ScrambleFn" -> scrambleAugmentedGauss,
    "StepsFn" -> stepsGauss,
    "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"]
  |>;

  runMatrixGenerator[spec, diff, mode, opts]
];
GenGaussJordan[diff_String, mode_String, opts : OptionsPattern[]] := Module[
  {spec},

  spec = <|
    "EntryFn" -> GenGaussJordan,
    "MsgPrefix" -> GenGaussJordan,
    "DimKey" -> "GaussJordan",
    "SectionTitle" -> "Gauss-Jordanova metóda",
    "ScrambleFn" -> scrambleAugmentedGaussJordan,
    "StepsFn" -> stepsGaussJordan,
    "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"]
  |>;

  runMatrixGenerator[spec, diff, mode, opts]
];
GenGaussJordanPivot[diff_String, mode_String, opts : OptionsPattern[]] := Module[
  {spec},

  spec = <|
    "EntryFn" -> GenGaussJordanPivot,
    "MsgPrefix" -> GenGaussJordanPivot,
    "DimKey" -> "GaussJordanPivot",
    "SectionTitle" -> "Gauss-Jordanova metóda s pivotovaním",
    "ScrambleFn" -> scrambleAugmentedGaussJordan,
    "StepsFn" -> stepsGaussJordan,
    "ValidateExtra" -> Function[{specLocal, passedOpts}, True],
    "ResolveExtra" -> Function[{specLocal, passedOpts}, "U"]
  |>;

  runMatrixGenerator[spec, diff, mode, opts]
];

End[];
EndPackage[];