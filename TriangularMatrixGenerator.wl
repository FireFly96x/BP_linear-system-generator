(* ::Package:: *)

(*
  Package: TriangularMatrixGenerator
  Description: Generates didactic materials for solving triangular linear systems using augmented matrices
               and pure row-based substitution (no equations after the conversion step).
  Guarantees: Integers only, coefficients and RHS always within bounds, no fractions anywhere.
  Updated: Dynamic step numbering added, fixed validation, strict integer rules, visual improvements, new bounds.
*)

BeginPackage["MojeGeneratory`TriangularMatrixGenerator`"];

$CharacterEncoding = "UTF-8";
Internal`$ContextMarks = False;

(* definícia usage a chybových hlášok *)
Gen01::usage =
    "Gen01[diff, mode, opts] vygeneruje príklad riešenia trojuholníkovej sústavy pomocou augmentovanej matice
a dosadzovania po riadkoch.

diff: \"EASY\" (3x3), \"MEDIUM\" (5x5), \"HARD\" (6x6)
mode: \"TASK\", \"TASK_RESULT\", \"TASK_STEPS_RESULT\"
opts: SolutionType -> Automatic|\"ONE\"|\"NONE\"|\"INFINITE\",
      TriangularType -> Automatic|\"L\"|\"U\"";

Gen01::baddiff  = "Neplatná obtiažnosť `1`. Použi \"EASY\"|\"MEDIUM\"|\"HARD\".";
Gen01::badmode  = "Neplatný režim `1`. Použi \"TASK\"|\"TASK_RESULT\"|\"TASK_STEPS_RESULT\".";
Gen01::badst    = "Neplatný typ riešenia `1`. Použi Automatic|\"ONE\"|\"NONE\"|\"INFINITE\".";
Gen01::badtri   = "Neplatný typ trojuholníkovej sústavy `1`. Použi Automatic|\"L\"|\"U\".";
Gen01::fail     = "Nepodarilo sa vygenerovať vhodný príklad.";

(* predvolené možnosti balíka *)
Options[Gen01] = {
  SolutionType -> Automatic,
  TriangularType -> Automatic
};

Begin["`Private`"];

(* ~-~-~ VALIDATION ~-~-~ *)

DimensionByDifficulty[generatorKey_String, diff_String] := Which[
  MemberQ[{"Elimination", "Substitution"}, generatorKey],
  Switch[diff, "EASY", 2, "MEDIUM", 3, "HARD", 3, _, 3], True,
  Switch[diff, "EASY", 4, "MEDIUM", 5, "HARD", 6, _, 4]
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
alignedAugmentedMatrix[aug_, notes_List : {}, hi_Association : <||>] := Module[
  {nRows, nCols, nA, notes2, pivotPos, activeRow, greenCells, bar, rowColor, wrapBg, makeCell, makeBar, leftBracketCell, rightBracketCell, rows, matrixGrid, notesGrid},
  {nRows, nCols} = Dimensions[aug];
  nA = nCols - 1;

  notes2 = If[notes === {}, ConstantArray["", nRows], PadRight[notes, nRows, ""]];
  pivotPos = Lookup[hi, "PivotPos", None];
  activeRow = Lookup[hi, "ActiveRow", None];
  greenCells = Lookup[hi, "GreenCells", {}];

  bar = Style["|", GrayLevel[.35], FontSize -> 16];
  rowColor = RGBColor[0.90, 0.95, 1];

  wrapBg[i_, expr_] := Item[expr, Background -> If[IntegerQ[activeRow] && i === activeRow, rowColor, None]];

  makeCell[i_, j_, val_] := Module[{cell = TraditionalForm[val], isGreen},
    isGreen = MemberQ[greenCells, {i, j}];
    If[isGreen, cell = Style[cell, Darker[Green], Bold], If[pivotPos === {i, j}, cell = Style[cell, Bold, RGBColor[0.8, 0, 0]]]];
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
    ], {i, 1, nRows}
  ];

  matrixGrid = Grid[
    rows,
    Alignment -> Join[{Center}, ConstantArray[Right, nA], {Center, Right, Center}],
    Spacings -> {1, 1},
    BaseStyle -> {FontSize -> 14},
    ItemSize -> {
      Join[{0.2}, ConstantArray[1.2, nA], {0.2, 1.2, 0.2}],
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
$kSet = DeleteCases[Range[-5, 5], 0];
$mSet = DeleteCases[Range[-4, 4], 0];
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
        A[[i, idx]] = randomNonzeroFromSet[$kSet],
        {i, 1, idx - 1}
      ],
      (* triType === "L" *)
      Do[
        A[[i, idx]] = randomNonzeroFromSet[$kSet],
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
generateData[n_, solType_, triType_] := Module[
  {solved, augSolved, augTask, A, b, vars},

  solved = makeSolvedAugmented[n, solType, triType];
  augSolved = solved["Aug"];

  augTask = scrambleTriangularAugmented[augSolved, triType];

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
scrambleTriangularAugmented[aug0_, triType_String] := Module[
  {aug = aug0, n, i, r, k, m},

  n = Length[aug];

  If[triType === "U",
    (* U: nechaj presne ako doteraz fungovalo *)
    Do[
      Do[
        k = randomNonzeroFromSet[$kSet];
        aug = rowAddMultiple[aug, r, i, k],
        {r, 1, i - 1}
      ];

      m = randomNonzeroFromSet[$mSet];
      aug = rowScale[aug, i, m];
      ,
      {i, 1, n}
    ],
    (* L: iba zmeň smer pivotov (zdola hore) *)
    Do[
      Do[
        k = randomNonzeroFromSet[$kSet];
        aug = rowAddMultiple[aug, r, i, k],
        {r, i + 1, n}
      ];

      m = randomNonzeroFromSet[$mSet];
      aug = rowScale[aug, i, m];
      ,
      {i, n, 1, -1}
    ]
  ];

  aug
];

(* ~-~-~ ROW OPERATIONS - delenie, kombinácia ~-~-~ *)

(* poznámka pre delenie riadku *)
rowOpDivideNote[i_, p_] := Row[{"R", i, " \[LeftArrow] R", i, " / ", tf[p]}];
applyRowOpDivide[aug_, i_Integer, p_Integer] := ReplacePart[aug, i -> (aug[[i]]/p)];

(* poznámka pre kombináciu riadkov *)
rowOpCombineNote[i_, terms_List] := Module[{base = Row[{"R", i, " \[LeftArrow] R", i}]},
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

(* zobrazenie matice pred a po úprave *)
renderBeforeAfter[before_, after_, notes_, hiBefore_, hiAfter_] := Grid[
  {{alignedAugmentedMatrix[before, notes, hiBefore], Spacer[18], alignedAugmentedMatrix[after, {}, hiAfter]}},
  Alignment -> {Left, Center, Left}, Spacings -> {0, 0}
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
            <|"ActiveRow" -> i, "PivotPos" -> {i, i}|>,
            <|"ActiveRow" -> i, "PivotPos" -> {i, i}|>
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
          {Row[{"Riadok ", i, ":  ", tf[A[[i]]], " \[CenterDot] ", tf[sol], " = ", tft[lhs]}]},
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

Gen01[diff_String, mode_String, opts : OptionsPattern[]] := Module[
  {n, vars, st, tri, data, steps},

  If[!TrueQ[ValidateDifficulty[diff]], Message[Gen01::baddiff, diff]; Return[$Failed]];
  If[!TrueQ[ValidateMode[mode]], Message[Gen01::badmode, mode]; Return[$Failed]];
  If[!TrueQ[ValidateSolutionType[OptionValue[SolutionType]]], Message[Gen01::badst, OptionValue[SolutionType]]; Return[$Failed]];
  If[!TrueQ[validateTriangularType[OptionValue[TriangularType]]], Message[Gen01::badtri, OptionValue[TriangularType]]; Return[$Failed]];

  st = ResolveSolutionType[OptionValue[SolutionType]];
  tri = resolveTriangularType[OptionValue[TriangularType]];

  n = DimensionByDifficulty["Triangular", diff];
  vars = buildVars[n];

  data = generateData[n, st, tri];

  printSectionCell["Trojuholníková metóda"];
  printSubsectionCell["Zadanie"];
  printTextCell["Riešte sústavu rovníc v množine celých čísel."];
  printFormulaCell @ Grid[List /@ (tf /@ buildTaskEquations[data["A"], data["b"], vars]), Alignment -> Left, Spacings -> {0, 0.8}];

  printTextCell["Riešte pomocou augmentovanej matice a dosadzovania po riadkoch."];

  If[mode === "TASK_STEPS_RESULT",
    withStepCounter @ Function[Null,
      printSubsectionCell["Postup"];
      steps = stepsTriangular[data];
      Scan[renderStepItem, steps["Content"]];
    ]
  ];

  If[mode =!= "TASK",
    printSubsectionCell["Výsledok"];
    If[st === "ONE", printFormulaCell[Row[Flatten[{"(", Riffle[vars, ", "], ") = (", Riffle[TraditionalForm /@ data["x"], ", "], ")"}]]]];
    If[st === "NONE", printTextCell["Sústava nemá riešenie."]];
    If[st === "INFINITE", printTextCell["Sústava má nekonečne veľa riešení."]];
  ];
];

End[];
EndPackage[];