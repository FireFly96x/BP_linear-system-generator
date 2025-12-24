(* ::Package:: *)

(*
  Package: TriangularMatrixGenerator
  Description: Generates didactic materials for solving triangular linear systems using augmented matrices
               and pure row-based substitution (no equations after the conversion step).
  Guarantees: Integers only, coefficients and RHS always within bounds, no fractions anywhere.
  Updated: Dynamic step numbering added, fixed validation, strict integer rules, visual improvements, new bounds.
*)

BeginPackage["MojeGeneratory`TriangularMatrixGenerator`", "MojeGeneratory`Common`"];

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

(* ~-~-~ CONSTANTS ~-~-~ *)

(* konštanty pre generovanie koeficientov *)
$CoeffMin = -10;
$CoeffMax = 10;
$DiagMin = -1; (* neskor vylepšiť generovanie aby mohlo byť viac *)
$DiagMax = 1;
rhsBound = 100;

(* ~-~-~ CELL PRINTING ~-~-~ *)

(* pomocné funkcie pre tlač buniek notebooku *)
printSectionCell[str_String] := CellSection[str];
printSubsectionCell[str_String] := CellSubsection[str];
printTextCell[str_String] := CellText[str];
printFormulaCell[expr_] := CellFormula[expr];

(* ~-~-~ FORMATTING ~-~-~ *)

(* štýlovanie mriežky *)
highlightGrid[grid_] := Style[grid, Background -> RGBColor[0.95, 0.95, 0.95], Frame -> True, FrameStyle -> None, FrameMargins -> 5];

(* zvýraznenie člena v rovnici *)
highlightTerm[term_] := Style[term, Bold, RGBColor[0.8, 0, 0]];

(* skratky pre TraditionalForm a úpravu výrazov *)
tf[val_] := TraditionalForm[val];
tft[val_] := tf[Together[val]];

(* ~-~-~ STEP RENDERER ~-~-~ *)

(* počítadlo krokov *)
stepsCounter = 0;

(* vytvorenie nadpisu kroku s číslovaním *)
makeStepHeader[text_String] := (stepsCounter++; Style[ToString[stepsCounter] <> ". " <> text, Bold]);

(* vykreslenie položky kroku podľa typu *)
renderStepItem[item_] := Which[
  StringQ[item], printTextCell[item],
  MatchQ[item, Style[_String, ___]], CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Text", ShowStringCharacters -> False],
  Head[item] === Cell, CellPrint[item],
  Head[item] === Graphics || Head[item] === Graphics3D, CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Graphics"],
  True, printFormulaCell[item]
];

(* ~-~-~ VALIDATION ~-~-~ *)

(* validácia typu trojuholníkovej matice *)
validateTriangularType[tri_] := TrueQ[tri === Automatic] || MemberQ[{"L", "U"}, tri];

(* výber typu matice ak je Automatic *)
resolveTriangularType[tri_] := If[tri === Automatic, RandomChoice[{"L", "U"}], tri];

(* kontrola rozsahu hodnôt vektora *)
vecInRangeQ[v_, n_] := Max[Abs @ Flatten[v]] <= rhsBound;

(* kontrola či výraz obsahuje len celé čísla *)
integersOnlyQ[expr_] := FreeQ[expr, _Rational | _Real];

(* ~-~-~ TASK EQUATIONS ~-~-~ *)

(* vytvorenie zoznamu premenných *)
buildVars[n_] := Take[{a, b, c, d, e, f}, n];

(* vytvorenie rovníc pre zadanie *)
buildTaskEquations[A_, b_, vars_] := MapThread[HoldForm[#1 == #2] &, {A.vars, b}];

(* vytvorenie augmentovanej matice *)
toAugmented[A_, b_] := Join[A, List /@ b, 2];

(* ~-~-~ MATRIX VISUALIZATION ~-~-~ *)

(* vizualizácia augmentovanej matice so zarovnaním *)
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
    isGreen = MemberQ[greenCells, {i, j}] || val === 1;
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

(* generovanie trojuholníkovej matice *)
buildTriangularMatrix[n_, triType_, diff_] := Module[{A, allowedPos, diagVals, offDiagVals},

  A = ConstantArray[0, {n, n}];

  diagVals = DeleteCases[Range[$DiagMin, $DiagMax], 0];
  offDiagVals = DeleteCases[Range[$CoeffMin, $CoeffMax], 0];

  Do[A[[i, i]] = RandomChoice[diagVals], {i, 1, n}];

  allowedPos = Position[
    If[triType === "L",
      LowerTriangularize[ConstantArray[1, {n, n}], -1],
      UpperTriangularize[ConstantArray[1, {n, n}], 1]
    ],
    1
  ];

  Scan[(A[[#[[1]], #[[2]]]] = RandomChoice[offDiagVals]) &, allowedPos];

  A
];

(* ~-~-~ DATA GENERATION ~-~-~ *)

(* generovanie dát pre zadanie a riešenie *)
generateData[n_, diff_, solType_, triType_] := Module[
  {A, b, x, idx, core, vars, aug},

  A = buildTriangularMatrix[n, triType, diff];

  core = Switch[solType,
    "ONE",
    x = RandomInteger[{-9, 9}, n];
    b = A.x;
    <|"b" -> b, "x" -> x|>
    ,
    "NONE",
    b = RandomInteger[{-9, 9}, n];
    idx = If[triType === "L", 1, n];
    A[[idx]] = ConstantArray[0, n];
    b[[idx]] = RandomChoice[Join[Range[-9, -1], Range[1, 9]]];
    <|"b" -> b, "x" -> "NONE", "BadRow" -> idx|>
    ,
    "INFINITE",
    idx = If[triType === "L", 1, n];
    A[[idx]] = ConstantArray[0, n];
    x = RandomInteger[{-3, 3}, n];
    x[[idx]] = 1;
    b = A.x;
    <|"b" -> b, "x" -> "INFINITE", "ParamIdx" -> idx|>
    ,
    _, $Failed
  ];

  If[core === $Failed, Return[$Failed]];
  If[!vecInRangeQ[core["b"], n], Return[$Failed]];

  vars = buildVars[n];
  aug = toAugmented[A, core["b"]];

  Join[<|"A" -> A, "TriType" -> triType, "SolutionType" -> solType|>, core, <|"Aug" -> aug, "Vars" -> vars, "n" -> n|>]
];

(* ~-~-~ ROW OPERATIONS ~-~-~ *)

(* poznámka pre násobenie riadku *)
rowOpScaleNote[i_, k_] := Row[{"R", i, " \[LeftArrow] ", tf[k], "\[CenterDot]R", i}];

(* poznámka pre delenie riadku *)
rowOpDivideNote[i_, p_] := Row[{"R", i, " \[LeftArrow] R", i, " / ", tf[p]}];

(* poznámka pre kombináciu riadkov *)
rowOpCombineNote[i_, terms_List] := Module[{base = Row[{"R", i, " \[LeftArrow] R", i}]},
  Row @ Prepend[
    (Row[{If[#2 < 0, " - ", " + "], tf[Abs[#2]], "\[CenterDot]R", #1}] & @@@ terms),
    base
  ]
];

(* aplikácia lineárnej kombinácie riadkov *)
applyRowOpCombine[aug_, i_Integer, terms_List] := Module[
  {row = aug[[i]]},
  ReplacePart[
    aug,  i -> (row + Total[terms[[All, 2]] aug[[terms[[All, 1]]]]])
  ]
];

(* aplikácia delenia riadku *)
applyRowOpDivide[aug_, i_Integer, p_Integer] := ReplacePart[aug, i -> Quotient[aug[[i]], p]];

(* zobrazenie matice pred a po úprave *)
renderBeforeAfter[before_, after_, notes_, hiBefore_, hiAfter_] := Grid[
  {{alignedAugmentedMatrix[before, notes, hiBefore], Spacer[18], alignedAugmentedMatrix[after, {}, hiAfter]}},
  Alignment -> {Left, Center, Left}, Spacings -> {0, 0}
];

(* ~-~-~ STEP GENERATION ~-~-~ *)

(* pomocné funkcie pre parametrické riešenie *)
getLinearCoeffsInT[expr_] := Module[{c},
  c = CoefficientList[Expand[expr], \[FormalT]];
  If[Length[c] > 2, $Failed, PadRight[c, 2, 0]]
];

safeDivideLinearCoeffs[{c0_, c1_}, p_] := If[
  Mod[c0, p] === 0 && Mod[c1, p] === 0,
  {Quotient[c0, p], Quotient[c1, p]},
  $Failed
];

(* generovanie krokov riešenia *)
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
        If[pivot === 0, Return[$Failed]];

        knownTerm = Total@Table[If[j === i, 0, row[[j]]*solExprs[[j]]], {j, 1, n}];

        With[
          {num = Expand[row[[n + 1]] - knownTerm]},
          Module[{coeffs, divCoeffs},
            coeffs = getLinearCoeffsInT[num];
            If[coeffs === $Failed, Return[$Failed]];

            divCoeffs = safeDivideLinearCoeffs[coeffs, pivot];
            If[divCoeffs === $Failed, Return[$Failed]];

            solExprs[[i]] = divCoeffs[[1]] + divCoeffs[[2]]*\[FormalT];
          ]
        ];

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
    ],
    _,  $Failed
  ];

  If[result === $Failed, Return[$Failed]];

  sol = result["Solution"];
  <|"Content" -> content, "Solution" -> sol|>
];

(* ~-~-~ VERIFICATION ~-~-~ *)

(* kroky overenia pre sústavu bez riešenia *)
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

  badIdx = Lookup[data, "BadRow", None];
  If[IntegerQ[badIdx],
    rhsVal = aug0[[badIdx, n + 1]];
    AppendTo[content, "V augmentovanej matici sa to prejaví aj priamo ako riadok tvaru (0 ... 0 | k), kde k \[NotEqual] 0, teda spor 0 = k:"];
    AppendTo[content,
      alignedAugmentedMatrix[
        aug0,
        ReplacePart[ConstantArray["", n], badIdx -> ("SPOR: 0 = " <> ToString[rhsVal])],
        <|"ActiveRow" -> badIdx|>
      ]
    ];
  ];

  content
];

(* kroky overenia pre nekonečne veľa riešení *)
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

(* štandardné kroky skúšky správnosti *)
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

  data = WithRetries[Function[Null, generateData[n, diff, st, tri]], 200];
  If[data === $Failed, Message[Gen01::fail]; Return[$Failed]];

  printSectionCell["Trojuholníková metóda"];
  printSubsectionCell["Zadanie"];
  printTextCell["Riešte sústavu rovníc v množine celých čísel."];
  printFormulaCell @ Grid[List /@ (tf /@ buildTaskEquations[data["A"], data["b"], vars]), Alignment -> Left, Spacings -> {0, 0.8}];

  printTextCell["Riešte pomocou augmentovanej matice a dosadzovania po riadkoch."];

  If[mode === "TASK_STEPS_RESULT",
    stepsCounter = 0;
    printSubsectionCell["Postup"];
    steps = stepsTriangular[data];
    If[steps === $Failed, Message[Gen01::fail]; Return[$Failed]];
    Scan[renderStepItem, steps["Content"]];
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