(* ::Package:: *)

(*
  Package: MatrixGenerator
  Description: Spoločné pomocné funkcie pre generátory lineárnych sústav tvaru A x = b.
*)

BeginPackage["MojeGeneratory`MatrixGenerator`"];

$CharacterEncoding = "UTF-8";
Internal`$ContextMarks = False;

GenTriangular::usage = "GenTriangular[diff, mode, opts] generuje trojuholníkovú metódu (base, nezmenené správanie).";
GenGauss::usage = "GenGauss[diff, mode, opts] generuje Gauss (REF + spätné dosadzovanie) v ℤ.";
GenGaussJordan::usage = "GenGaussJordan[diff, mode, opts] generuje Gauss–Jordan (RREF) v ℤ.";
GenGaussJordanPivot::usage = "GenGaussJordanPivot[diff, mode, opts] generuje Gauss–Jordan s čiastočným pivotovaním v ℤ.";

(* --- central message hub (single source of truth) --- *)
MatrixGenerator::baddiff = "Neplatná obtiažnosť `1`. Použi \"EASY\"|\"MEDIUM\"|\"HARD\".";
MatrixGenerator::badmode = "Neplatný režim `1`. Použi \"TASK\"|\"TASK_RESULT\"|\"TASK_STEPS_RESULT\".";
MatrixGenerator::badst   = "Neplatný typ riešenia `1`. Použi Automatic|\"ONE\"|\"NONE\"|\"INFINITE\".";
MatrixGenerator::badtri  = "Neplatný typ trojuholníkovej sústavy `1`. Použi Automatic|\"L\"|\"U\".";
MatrixGenerator::fail    = "Nepodarilo sa vygenerovať vhodný príklad.";
MatrixGenerator::badgen = "Neplatný typ generátora `1`.";

(* --- expose same messages on public symbols for compatibility --- *)
GenTriangular::baddiff = MatrixGenerator::baddiff;
GenTriangular::badmode = MatrixGenerator::badmode;
GenTriangular::badst   = MatrixGenerator::badst;
GenTriangular::badtri  = MatrixGenerator::badtri;
GenTriangular::fail    = MatrixGenerator::fail;

GenGauss::baddiff = MatrixGenerator::baddiff;
GenGauss::badmode = MatrixGenerator::badmode;
GenGauss::badst   = MatrixGenerator::badst;
GenGauss::fail    = MatrixGenerator::fail;
GenGauss::badgen = MatrixGenerator::badgen;

GenGaussJordan::baddiff = MatrixGenerator::baddiff;
GenGaussJordan::badmode = MatrixGenerator::badmode;
GenGaussJordan::badst   = MatrixGenerator::badst;
GenGaussJordan::fail    = MatrixGenerator::fail;
GenGaussJordan::badgen = MatrixGenerator::badgen;

GenGaussJordanPivot::baddiff = MatrixGenerator::baddiff;
GenGaussJordanPivot::badmode = MatrixGenerator::badmode;
GenGaussJordanPivot::badst   = MatrixGenerator::badst;
GenGaussJordanPivot::fail    = MatrixGenerator::fail;
GenGaussJordanPivot::badgen = MatrixGenerator::badgen;

(* --- options (public) --- *)
Options[GenTriangular]       = {SolutionType -> Automatic, TriangularType -> Automatic};
Options[GenGauss]            = {SolutionType -> Automatic};
Options[GenGaussJordan]      = {SolutionType -> Automatic};
Options[GenGaussJordanPivot] = {SolutionType -> Automatic};

Begin["`Private`"];

$AllowedDiffs = {"EASY", "MEDIUM", "HARD"};
$AllowedModes = {"TASK", "TASK_RESULT", "TASK_STEPS_RESULT"};
$AllowedSolutionTypes = {"ONE", "NONE", "INFINITE"};
$AllowedTriangularTypes = {"L", "U"};

mgValidateInputs[diff_, mode_, st_] := Module[{},
  If[!MemberQ[$AllowedDiffs, diff],
    Message[MatrixGenerator::baddiff, diff]; Return[$Failed]
  ];
  If[!MemberQ[$AllowedModes, mode],
    Message[MatrixGenerator::badmode, mode]; Return[$Failed]
  ];
  If[st =!= Automatic && !MemberQ[$AllowedSolutionTypes, st],
    Message[MatrixGenerator::badst, st]; Return[$Failed]
  ];
  True
];


(* ~-~-~ CONSTANTS ~-~-~ *)

(* konštanty pre generovanie koeficientov *)
$CoeffMin = -10;
$CoeffMax = 10;
$DiagMin  = -1;
$DiagMax  = 1;
rhsBound  = 900;

(* kontrola rozsahu vektora pravej strany *)
vecInRangeQ[v_] := Max[Abs @ Flatten[v]] <= rhsBound;

(* ~-~-~ COMMON.WL ~-~-~ *)

(* výber typu matice, ak je Automatic *)
resolveTriangularType[tri_] := If[tri === Automatic, RandomChoice[{"L", "U"}], tri];

mgValidateTriangularType[tri_] := Module[{},
  If[tri === Automatic || MemberQ[$AllowedTriangularTypes, tri], True,
    Message[MatrixGenerator::badtri, tri]; $Failed
  ]
];

ResolveSolutionType[st_, allowed_List : {"ONE", "NONE", "INFINITE"}] := Module[{choices},
  choices = Intersection[allowed, $AllowedSolutionTypes];
  If[st =!= Automatic, If[MemberQ[choices, st], Return[st], Return[First[choices]]]];
  If[choices === {}, "ONE", RandomChoice[choices]]
];


(* určenie rozmeru sústavy podľa obtiažnosti a typu generátora *)
DimensionByDifficulty[generatorKey_String, diff_String] := Which[
  MemberQ[{"Elimination", "Substitution"}, generatorKey],
  Switch[diff, "EASY", 2, "MEDIUM", 3, "HARD", 3],
  True,
  Switch[diff, "EASY", 4, "MEDIUM", 5, "HARD", 6]
];

(* opakované pokusy o generovanie, kým nevznikne platný príklad *)
WithRetries[f_, max_Integer : 200] := Module[{res, i},
  If[!MatchQ[f, _Function], Return[$Failed]];
  Do[
    res = f[];
    If[res =!= $Failed, Return[res]],
    {i, 1, max}
  ];
  $Failed
];

(* ~-~-~ CELL PRINTING ~-~-~ *)

inNotebookQ[] := Head @ Quiet[EvaluationNotebook[]] === NotebookObject;

printCellStyle[expr_, style_String] := If[inNotebookQ[], CellPrint @ Cell[expr, style, ShowStringCharacters -> False],  Print[expr]];
printTextCell[str_String] := printCellStyle[str, "Text"];
printExprCell[expr_] := Module[{boxes},
  boxes = Quiet @ Check[BoxData @ ToBoxes[expr, StandardForm], expr];
  printCellStyle[boxes, "Input"]
];
printSectionCell[str_String] := printCellStyle[str, "Section"];
printSubsectionCell[str_String] := printCellStyle[str, "Subsection"];
printTextExprCell[expr_] := Module[{boxes},
  boxes = Quiet @ Check[BoxData @ ToBoxes[expr, StandardForm], expr];
  printCellStyle[boxes, "Text"]
];
printFormulaCell[expr_] := Module[{boxes},
  boxes = Quiet @ Check[BoxData @ ToBoxes[expr, StandardForm], expr];
  printCellStyle[boxes, "DisplayFormula"]
];

(* ~-~-~ FORMATTING ~-~-~ *)

(* štýlovanie mriežky *)
highlightGrid[grid_] := Style[grid, Background -> RGBColor[0.95, 0.95, 0.95], Frame -> True, FrameStyle -> None, FrameMargins -> 5];

(* zvýraznenie člena v rovnici *)
highlightTerm[term_] := Style[term, Bold, RGBColor[0.8, 0, 0]];

(* skratky pre TraditionalForm a úpravu výrazov *)
tf[val_] := TraditionalForm[val];
tft[val_] := tf[Together[val]];

(* ~-~-~ STEP RENDERER ~-~-~ *)

(* číslovanie krokov bez globálneho stavu (lokalizované cez Block) *)
mgWithStepCounter[renderFn_] := Block[{stepsCounter = 0},
  (* vnútri renderFn sa volá mgStepHeader *)
  renderFn[]
];

mgStepHeader[text_String] := Module[{},
  stepsCounter++;
  Style[ToString[stepsCounter] <> ". " <> text, Bold]
];

renderStepItem[item_] := Which[
  StringQ[item], printTextCell[item],
  MatchQ[item, Style[_String, ___]], CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Text", ShowStringCharacters -> False],
  Head[item] === Cell, CellPrint[item],
  Head[item] === Graphics || Head[item] === Graphics3D, CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Graphics"],
  True, printFormulaCell[item]
];

(* ~-~-~ COMMON MATRIX HELPERS ~-~-~ *)

(* premenné pre sústavu *)
buildVars[n_] := Take[{a, b, c, d, e, f}, n];

(* zostavenie rovníc zo sústavy A x = b *)
buildTaskEquations[A_, b_, vars_] := MapThread[HoldForm[#1 == #2] &, {A.vars, b}];

(* vytvorenie augmentovanej matice z A a b *)
toAugmented[A_, b_] := Join[A, List /@ b, 2];

(* zobrazenie augmentovanej matice s poznámkami *)
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

(* zobrazenie pred a po úprave *)
renderBeforeAfter[before_, after_, notes_, hiBefore_, hiAfter_] := Grid[
  {{alignedAugmentedMatrix[before, notes, hiBefore], Spacer[18], alignedAugmentedMatrix[after, {}, hiAfter]}},
  Alignment -> {Left, Center, Left}, Spacings -> {0, 0}
];

(* ~-~-~ TRIANGULAR GENERATOR HELPERS ~-~-~ *)

buildUnitUpperTriangularMatrix[n_, diff_] := Module[{A, offDiagVals, allowedPos},
  A = IdentityMatrix[n];

  offDiagVals = DeleteCases[Range[$CoeffMin, $CoeffMax], 0];

  allowedPos = Position[
    UpperTriangularize[ConstantArray[1, {n, n}], 1],
    1
  ];

  Scan[(A[[#[[1]], #[[2]]]] = RandomChoice[offDiagVals]) &, allowedPos];
  A
];


(* generovanie trojuholníkovej matice (L/U) s celočíselnými koeficientmi *)
buildTriangularMatrix[n_, triType_, diff_] := Module[{A, allowedPos, diagVals, offDiagVals},
  A = ConstantArray[0, {n, n}];

  diagVals    = DeleteCases[Range[$DiagMin, $DiagMax], 0];
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

(* generovanie dát pre zadanie a riešenie (ONE/NONE/INFINITE) *)
generateData[n_, diff_, solType_, triType_] := Module[
  {A, b, x, idx, core, vars, aug},

  A = buildTriangularMatrix[n, triType, diff];

  core = Switch[solType,
    "ONE",
    x = RandomInteger[{-3, 3}, n];
    b = A.x;
    <|"b" -> b, "x" -> x|>,
    "NONE",
    b = RandomInteger[{-9, 9}, n];
    idx = If[triType === "L", 1, n];
    A[[idx]] = ConstantArray[0, n];
    b[[idx]] = RandomChoice[Join[Range[-9, -1], Range[1, 9]]];
    <|"b" -> b, "x" -> "NONE", "BadRow" -> idx|>,
    "INFINITE",
    idx = If[triType === "L", 1, n];
    A[[idx]] = ConstantArray[0, n];
    x = RandomInteger[{-3, 3}, n];
    x[[idx]] = 1;
    b = A.x;
    <|"b" -> b, "x" -> "INFINITE", "ParamIdx" -> idx|>,
    _,
    $Failed
  ];

  If[core === $Failed, Return[$Failed]];
  If[!vecInRangeQ[core["b"]], Return[$Failed]];

  vars = buildVars[n];
  aug  = toAugmented[A, core["b"]];

  Join[
    <|"A" -> A, "TriType" -> triType, "SolutionType" -> solType|>,
    core,
    <|"Aug" -> aug, "Vars" -> vars, "n" -> n|>
  ]
];

(* ~-~-~ ROW OPERATIONS ~-~-~ *)

rowOpScaleNote[i_, k_] := Row[{"R", i, " \[LeftArrow] ", tf[k], "\[CenterDot]R", i}];
rowOpDivideNote[i_, p_] := Row[{"R", i, " \[LeftArrow] R", i, " / ", tf[p]}];
rowOpCombineNote[i_, terms_List] := Module[{base = Row[{"R", i, " \[LeftArrow] R", i}]},
  Row @ Prepend[
    (Row[{If[#2 < 0, " - ", " + "], tf[Abs[#2]], "\[CenterDot]R", #1}] & @@@ terms),
    base
  ]
];
applyRowOpCombine[aug_, i_Integer, terms_List] := Module[{row = aug[[i]]},
  ReplacePart[aug, i -> (row + Total[terms[[All, 2]] aug[[terms[[All, 1]]]]])]
];
applyRowOpDivide[aug_, i_Integer, p_Integer] := ReplacePart[aug, i -> Quotient[aug[[i]], p]];

(* ~-~-~ STEP GENERATION ~-~-~ *)

(* bezpečné delenie koeficientov *)

(* generovanie krokov triangular metódy *)
stepsTriangular[data_Association] := Module[
  {content = {}, n, aug, vars, tri, st, order, addHeader, addText, addMatrix, addConclusion, addCheckHeader, notes, result, sol},

  n = data["n"]; aug = data["Aug"]; vars = data["Vars"]; tri = data["TriType"]; st = data["SolutionType"];
  order = If[tri === "U", Range[n, 1, -1], Range[1, n]];

  addHeader[text_] := AppendTo[content, mgStepHeader[text]];
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

        With[
          {num = Expand[row[[n + 1]] - knownTerm]},
          Module[{coeffs, divCoeffs},
            coeffs = getLinearCoeffsInT[num];
            solExprs[[i]] = coeffs[[1]]/pivot + (coeffs[[2]]/pivot)*\[FormalT];
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

(* ~-~-~ SKÚŠKA SPRÁVNOSTI ~-~-~ *)

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

(* interný generátor triangular base *)
Options[Gen01] = Options[GenTriangular];

Gen01[diff_String, mode_String, opts : OptionsPattern[]] := Module[
  {stOpt = OptionValue[SolutionType], triOpt = OptionValue[TriangularType],
    n, vars, st, tri, data, steps},

  If[mgValidateInputs[diff, mode, stOpt] === $Failed, Return[$Failed]];
  If[mgValidateTriangularType[triOpt] === $Failed, Return[$Failed]];

  st  = ResolveSolutionType[stOpt];
  tri = resolveTriangularType[triOpt];

  n = DimensionByDifficulty["Triangular", diff];

  vars = buildVars[n];

  data = WithRetries[Function[Null, generateData[n, diff, st, tri]], 200];
  If[data === $Failed, Return[$Failed]];

  (* render legacy output *)
  printSectionCell["Trojuholníková metóda"];
  printSubsectionCell["Zadanie"];
  printTextCell["Riešte sústavu rovníc v množine celých čísel."];
  printFormulaCell @ Grid[List /@ (tf /@ buildTaskEquations[data["A"], data["b"], vars]), Alignment -> Left, Spacings -> {0, 0.8}];
  printTextCell["Riešte pomocou augmentovanej matice a dosadzovania po riadkoch."];

  If[mode === "TASK_STEPS_RESULT",
    mgWithStepCounter @ Function[Null,
      printSubsectionCell["Postup"];
      steps = stepsTriangular[data];
      If[steps === $Failed, Message[MatrixGenerator::fail]; Return[$Failed]];
      Scan[renderStepItem, steps["Content"]];
    ]
  ];

  If[mode =!= "TASK",
    printSubsectionCell["Výsledok"];
    If[st === "ONE", printFormulaCell[Row[Flatten[{"(", Riffle[vars, ", "], ") = (", Riffle[TraditionalForm /@ data["x"], ", "], ")"}]]]];
    If[st === "NONE", printTextCell["Sústava nemá riešenie."]];
    If[st === "INFINITE", printTextCell["Sústava má nekonečne veľa riešení."]];
  ];

  (* return value: you can optionally return a structured association instead of Null *)
  data
];

(* ~-~-~ GAUSS / GJ VARIANTS ~-~-~ *)

mgVariantSpec[key_String] := Switch[key,
  "GAUSS",
  <|"Key" -> "GAUSS", "NeedUpElim" -> False, "NeedPivot" -> False,
    "SolvedAugFn" -> ( #["AugRefSolved"] & ),
    "ExtractFn" -> mgExtractSolutionGaussRef|>,
  "GJ",
  <|"Key" -> "GJ", "NeedUpElim" -> True, "NeedPivot" -> False,
    "SolvedAugFn" -> ( mgToRrefFromRef[#["AugRefSolved"], #] & ),
    "ExtractFn" -> mgExtractSolutionRref|>,
  "GJ_PIVOT",
  <|"Key" -> "GJ_PIVOT", "NeedUpElim" -> True, "NeedPivot" -> True,
    "SolvedAugFn" -> ( mgToRrefFromRef[#["AugRefSolved"], #] & ),
    "ExtractFn" -> mgExtractSolutionRref|>,
  _,
  $Failed
];

(* ~-~-~ ROW OPERATION APPLICATION  - výmena, kombinácia, delenie riadkov ~-~-~ *)

mgApplyOp[aug_, {"Swap", i_Integer, j_Integer}] := Module[{res = aug}, res[[{i, j}]] = res[[{j, i}]]; res];
mgApplyOp[aug_, {"Combine", t_Integer, s_Integer, k_Integer}] := ReplacePart[ aug, t -> (aug[[t]] + k aug[[s]])];
mgApplyOp[aug_, {"Divide", i_Integer, p_Integer}] := ReplacePart[aug, i -> Quotient[aug[[i]], p]];

mgNoteRow[{"Combine", t_, ___}] := t;
mgNoteRow[{"Divide",  i_, ___}] := i;
mgNoteRow[{"Swap",    i_, ___}] := i;

(* ~-~-~ ROW OPERATION TO TEXT ~-~-~ *)
mgOpToText[{"Combine", t_, s_, k_}] := If[k > 0,
  "R" <> ToString[t] <> " + " <> ToString[k] <> " R" <> ToString[s] <> " -> R" <> ToString[t],
  "R" <> ToString[t] <> " - " <> ToString[Abs[k]] <> " R" <> ToString[s] <> " -> R" <> ToString[t]
];

mgOpToText[{"Swap", i_, j_}] := "R" <> ToString[i] <> " <-> R" <> ToString[j];
mgOpToText[{"Divide", i_, p_}] := "R" <> ToString[i] <> " / " <> ToString[p] <> " -> R" <> ToString[i];
mgOpToText[_] := "";

(* ~-~-~ GAUSS: BASE GENERATION~-~-~ *)
mgGenerateGaussBaseData[n_Integer, diff_String, solType_String] := Module[
  {A, b, x, idx, vars, aug, core},

  (* slovenský komentár: pre GAUSS/GJ chceme jednotkovú diagonálu (pivoty 1) *)
  A = buildUnitUpperTriangularMatrix[n, diff];

  core = Switch[solType,
    "ONE",
    x = RandomInteger[{-9, 9}, n];
    b = A.x;
    <|"b" -> b, "x" -> x|>,
    "NONE",
    b = RandomInteger[{-9, 9}, n];
    idx = n;
    A[[idx]] = ConstantArray[0, n];
    b[[idx]] = RandomChoice[Join[Range[-9, -1], Range[1, 9]]];
    <|"b" -> b, "x" -> "NONE", "BadRow" -> idx|>,
    "INFINITE",
    Module[{p, rSet, cSet, rVals, cVals, augSolved},
      p = n;

      (* slovenský komentár: malé koeficienty pre väzbu na parameter *)
      rSet = {-3, -2, -1, 1, 2, 3};
      cSet = Range[-8, 8];

      rVals = RandomChoice[rSet, n - 1];
      cVals = RandomChoice[cSet, n - 1];

      augSolved = ConstantArray[0, {n, n + 1}];
      Do[
        augSolved[[i, i]] = 1;
        augSolved[[i, p]] = rVals[[i]];
        augSolved[[i, n + 1]] = cVals[[i]];
        ,
        {i, 1, n - 1}
      ];

      <|
        "b" -> augSolved[[All, n + 1]],
        "x" -> "INFINITE",
        "ParamIdx" -> p,
        "AugRefSolved" -> augSolved
      |>
    ],
    _,
    $Failed
  ];
  If[core === $Failed, Return[$Failed]];
  If[!vecInRangeQ[core["b"]], Return[$Failed]];

  vars = buildVars[n];

  (* pre ONE/NONE stále skladáme z A,b; pre INFINITE už máme AugRefSolved *)
  aug = If[solType === "INFINITE", core["AugRefSolved"], toAugmented[A, core["b"]]];

  Join[
    <|"A" -> If[solType === "INFINITE", aug[[All, 1 ;; n]], A], "TriType" -> "U", "SolutionType" -> solType|>,
    core,
    <|
      "Aug" -> aug,
      "Vars" -> vars,
      "n" -> n,
      "AugRefSolved" -> If[solType === "INFINITE", core["AugRefSolved"], toAugmented[A, core["b"]]]
    |>
  ]
];

(* prechod z REF na RREF (eliminácia nad pivotmi) *)
mgToRrefFromRef[augRef_, base_Association] := Module[{aug = augRef, n, i, r, after},
  n = base["n"];
  Do[
    Do[
      If[aug[[r, i]] =!= 0,
        aug = mgApplyOp[aug, {"Combine", r, i, -aug[[r, i]]}];
      ],
      {r, 1, i - 1}
    ],
    {i, 2, n}
  ];
  aug
];

(* ~-~-~ GAUSS: ELIMINATION PLAN ~-~-~ *)

(* výber eliminačných krokov pre GAUSS/GJ *)
mgEliminationPlan[n_Integer, needUpElim_] := Join[
  Flatten@Table[<|"Kind" -> "ElimDown", "PivotRow" -> i, "TargetRow" -> r|>, {i, 1, n - 1}, {r, i + 1, n}],
  If[TrueQ[needUpElim],
    Flatten@Table[<|"Kind" -> "ElimUp", "PivotRow" -> i, "TargetRow" -> r|>, {i, n, 2, -1}, {r, 1, i - 1}],
    {}
  ]
];

(* výber koeficientu k pre "Combine" operácie pri scramble *)
mgChooseScrambleK[diff_String, st_String : "ONE"] := Module[{kSet},
  kSet = If[st === "INFINITE",
    (* aby sme neprekročili hranice bez kontrol *)
    {-1, 1},
    Switch[diff,
      "EASY",   {-1, 1},
      "MEDIUM", {-2, -1, 1, 2},
      "HARD",   {-3, -2, -1, 1, 2, 3},
      _,        {-1, 1}
    ]
  ];
  RandomChoice[kSet]
];


(* vygenerovanie scramble operácií z eliminačného plánu *)
mgScrambleFromSolvedAugmented[augSolved_, plan_List, diff_String, st_String : "ONE"] := Module[
  {aug = augSolved, scrambleOps = {}, step, k, target, source, after, allowQ, i},

  Do[
    step   = plan[[i]];
    target = step["TargetRow"];
    source = step["PivotRow"];

    (* pri EASY povoliť len bezprostredný riadok pod pivotom (jednoduchšie stopy) *)
    allowQ = True;
    If[diff === "EASY", allowQ = (target === source + 1)];
    If[!TrueQ[allowQ], Continue[]];

    k = mgChooseScrambleK[diff, st];
    If[k === 0, Continue[]];

    aug = mgApplyOp[aug, {"Combine", target, source, k}];
    AppendTo[scrambleOps, {"Combine", target, source, k}];
    ,
    {i, Length[plan], 1, -1}
  ];

  <|"AugTask" -> aug, "ScrambleOps" -> scrambleOps|>
];

(* inverzná operácia pre generovanie solve operácií *)
mgInvertOp[{"Combine", t_, s_, k_}] := {"Combine", t, s, -k};
mgInvertOp[{"Swap", i_, j_}] := {"Swap", i, j};
mgInvertOp[{"Divide", __}] := $Failed;
mgInvertOp[_] := $Failed;

mgBuildSolveOps[scrambleOps_List] := Reverse[mgInvertOp /@ scrambleOps];

(* ~-~-~ GAUSS: SOLUTION EXTRACTION ~-~-~ *)
mgBackSubstitutionInfiniteFromRef[aug_, n_Integer, paramIdx_Integer] := Module[{x, i, rhs, sum},
  x = ConstantArray[0, n];
  x[[paramIdx]] = \[FormalT];
  Do[
    If[i === paramIdx, Continue[]];
    rhs = aug[[i, n + 1]];
    sum = Total@Table[If[j === i, 0, aug[[i, j]]*x[[j]]], {j, 1, n}];
    x[[i]] = Together[rhs - sum];
    ,
    {i, n, 1, -1}
  ];
  x
];


mgBackSubstitutionFromRef[aug_, n_Integer] := Module[{x, i, rhs, sum},
  x = ConstantArray[0, n];
  Do[
    rhs = aug[[i, n + 1]];
    sum = Total@Table[aug[[i, j]] * x[[j]], {j, i + 1, n}];
    x[[i]] = rhs - sum;
    ,
    {i, n, 1, -1}
  ];
  x
];

mgExtractSolutionGaussRef[augRef_, data_Association] := Module[{st = data["SolutionType"], n = data["n"], p},
  Switch[st,
    "ONE", mgBackSubstitutionFromRef[augRef, n],
    "NONE", "NONE",
    "INFINITE",
    p = Lookup[data, "ParamIdx", n];
    mgBackSubstitutionInfiniteFromRef[augRef, n, p],
    _, $Failed
  ]
];

mgExtractSolutionRref[augRref_, data_Association] := Module[{st = data["SolutionType"], n = data["n"], p, sol},
  Switch[st,
    "ONE", augRref[[All, n + 1]],
    "NONE", "NONE",
    "INFINITE",
    p = Lookup[data, "ParamIdx", n];
    sol = ConstantArray[0, n];
    sol[[p]] = \[FormalT];
    Do[
      If[i === p, Continue[]];
      sol[[i]] = Together[augRref[[i, n + 1]] - Total@Table[augRref[[i, j]]*sol[[j]], {j, 1, n}]];
      ,
      {i, 1, n}
    ];
    sol,
    _, $Failed
  ]
];


mgSolveAndRender[data_Association, solveOps_List, spec_Association, mode_String] := Module[
  {n, aug0, aug, content, finalAug, sol, steps},

  n = data["n"];
  aug0 = data["AugTask"];
  aug = aug0;
  content = {};

  If[mode === "TASK_STEPS_RESULT",
    steps = Catch[
      Reap[
        Sow[mgStepHeader["Prepis sústavy do augmentovanej matice"], "MGContent"];
        Sow["Zapíšeme sústavu do augmentovanej matice a ďalej pracujeme už len s maticou.", "MGContent"];
        Sow[alignedAugmentedMatrix[aug], "MGContent"];

        Sow[mgStepHeader["Riadkové úpravy"], "MGContent"];
        Do[
          Module[{before = aug, after, notes, noteRow},
            after = mgApplyOp[before, solveOps[[i]]];
            If[after === $Failed, Throw[$Failed, "MGFail"]];

            notes = ConstantArray["", n];
            noteRow = mgNoteRow[solveOps[[i]]];
            notes[[noteRow]] = mgOpToText[solveOps[[i]]];

            Sow[renderBeforeAfter[before, after, notes, <||>, <||>], "MGContent"];
            aug = after;
          ],
          {i, 1, Length[solveOps]}
        ];
        ,
        "MGContent"
      ],
      "MGFail"
    ];

    If[steps === $Failed, Return[$Failed]];
    content = steps[[2, 1]];
    finalAug = aug
    ,
    finalAug = Fold[mgApplyOp, aug0, solveOps];
    If[finalAug === $Failed, Return[$Failed]]
  ];

  sol = spec["ExtractFn"][finalAug, data];
  If[sol === $Failed, Return[$Failed]];

  <|"Content" -> content, "AugSolved" -> finalAug, "Solution" -> sol|>
];

(* ------------------------------------------------------- *)
(* GAUSS: PIVOTING LAYER (stubs / TODO)                     *)
(* ------------------------------------------------------- *)

(* kontrola potreby pivotovania v danom kroku *)
mgPivotRowByMaxAbs[aug_, pivotCol_Integer, startRow_Integer] := Module[
  {nRows, absVals, maxVal, relPos},
  nRows = Length[aug];
  If[startRow < 1 || startRow > nRows, Return[$Failed]];
  If[pivotCol < 1 || pivotCol > Length[aug[[1]]], Return[$Failed]];

  absVals = Abs @ aug[[startRow ;; nRows, pivotCol]];
  maxVal = Max[absVals];
  If[maxVal === 0, Return[$Failed]];

  relPos = First @ FirstPosition[absVals, maxVal];
  startRow + relPos - 1
];

mgInsertPivotSwapsValidated[augTask_, solveOps_List, n_Integer] := Module[
  {aug = augTask, newOps = {}, pivotDone, logicalAtPos, posOfLogical, op, i, t, s, k,
    pivotCol, pivotRow, after, swapPos, physT, physS, physI, physJ},

  (* slovenský komentár: mapovanie "logických" riadkov na fyzické pozície kvôli vloženým swapom *)
  logicalAtPos = Range[n];
  posOfLogical = Range[n];
  pivotDone = ConstantArray[False, n];

  (* slovenský komentár: pomocná aktualizácia mapovania pri fyzickom swape *)
  swapPos[i_Integer, j_Integer] := Module[{li, lj},
    li = logicalAtPos[[i]];
    lj = logicalAtPos[[j]];
    logicalAtPos[[i]] = lj; logicalAtPos[[j]] = li;
    posOfLogical[[li]] = j;
    posOfLogical[[lj]] = i;
  ];

  (* slovenský komentár: vloženie pivot swapu pred prvým "down" krokom pre daný pivot stĺpec *)
  Do[
    op = solveOps[[i]];

    If[ListQ[op] && Length[op] >= 4 && op[[1]] === "Combine",
      {t, s, k} = op[[2 ;; 4]];
      If[IntegerQ[t] && IntegerQ[s] && IntegerQ[k] && t > s && 1 <= s <= n && !TrueQ[pivotDone[[s]]],

        pivotCol = s;
        pivotRow = mgPivotRowByMaxAbs[aug, pivotCol, pivotCol];
        If[pivotRow === $Failed,
          pivotDone[[pivotCol]] = True;
          ,
          If[pivotRow =!= pivotCol,
            aug = mgApplyOp[aug, {"Swap", pivotCol, pivotRow}];
            AppendTo[newOps, {"Swap", pivotCol, pivotRow}];
            swapPos[pivotCol, pivotRow];
          ];
          pivotDone[[pivotCol]] = True;
        ];


        pivotDone[[pivotCol]] = True;
      ];
    ];

    (* slovenský komentár: premapovanie pôvodnej operácie cez aktuálnu permutáciu *)
    Switch[op[[1]],
      "Combine",
      {t, s, k} = op[[2 ;; 4]];
      physT = posOfLogical[[t]];
      physS = posOfLogical[[s]];
      aug = mgApplyOp[aug, {"Combine", physT, physS, k}];
      AppendTo[newOps, {"Combine", physT, physS, k}],
      "Swap",
      physI = posOfLogical[[op[[2]]]];
      physJ = posOfLogical[[op[[3]]]];
      aug = mgApplyOp[aug, {"Swap", physI, physJ}];
      AppendTo[newOps, {"Swap", physI, physJ}];
      swapPos[physI, physJ],
      "Divide",
      physI = posOfLogical[[op[[2]]]];
      aug = mgApplyOp[aug, {"Divide", physI, op[[3]]}];
      AppendTo[newOps, {"Divide", physI, op[[3]]}],
      _,
      Return[$Failed]
    ];
    ,
    {i, 1, Length[solveOps]}
  ];

  <|"AugTask" -> augTask, "SolveOps" -> newOps|>
];

mgMaybePivotify[augTask_, solveOps_, spec_, n_] :=
    If[TrueQ[spec["NeedPivot"]],
      Module[{res = mgInsertPivotSwapsValidated[augTask, solveOps, n]},
        If[res === $Failed, $Failed, res]
      ],
      <|"AugTask" -> augTask, "SolveOps" -> solveOps|>
    ];

Options[mgRunGenerator] = {SolutionType -> Automatic};

(* ~-~-~ GAUSS / GJ: MAIN GENERATOR ~-~-~ *)

mgRunGenerator[generatorKey_String, diff_String, mode_String, opts : OptionsPattern[{GenGauss, GenGaussJordan, GenGaussJordanPivot}]] := Module[
  {stOpt, st, n, spec, base, augSolved, plan, scr, solveOps, pivoted, data2, solvedPkg},
  stOpt = OptionValue[SolutionType];

  If[mgValidateInputs[diff, mode, stOpt] === $Failed, Return[$Failed]];

  spec = mgVariantSpec[generatorKey];
  If[spec === $Failed, Message[MatrixGenerator::badgen, generatorKey]; Return[$Failed]];

  st = ResolveSolutionType[stOpt];
  n = DimensionByDifficulty["Elimination", diff];

  (* 1) base data (solved REF with pivots 1 by construction) *)
  base = WithRetries[Function[Null, mgGenerateGaussBaseData[n, diff, st]], 200];
  If[base === $Failed, Return[$Failed]];

  (* 2) solved augmented for this variant *)
  augSolved = spec["SolvedAugFn"][base];
  If[augSolved === $Failed, Return[$Failed]];

  (* 3) elimination plan: down-only (GAUSS) or down+up (GJ variants) *)
  plan = mgEliminationPlan[n, spec["NeedUpElim"]];

  (* 4) scramble to create task (must retry due to bounds) *)
  scr = WithRetries[Function[Null, mgScrambleFromSolvedAugmented[augSolved, plan, diff, st]], 200];
  If[scr === $Failed, Return[$Failed]];

  data2 = Join[base, <|"AugTask" -> scr["AugTask"]|>];

  (* 5) solve ops are inverse of scramble ops *)
  solveOps = mgBuildSolveOps[scr["ScrambleOps"]];
  If[MemberQ[solveOps, $Failed], Return[$Failed]];

  (* 6) optional pivot layer (only if spec says so) *)
  pivoted = mgMaybePivotify[data2["AugTask"], solveOps, spec, n];
  If[pivoted === $Failed, Return[$Failed]];

  data2 = ReplacePart[data2, "AugTask" -> pivoted["AugTask"]];
  solveOps = pivoted["SolveOps"];

  (* 8) solve + (optional) render steps *)
  solvedPkg = mgWithStepCounter @ Function[Null, mgSolveAndRender[data2, solveOps, spec, mode]];
  If[solvedPkg === $Failed, Return[$Failed]];

  (* 9) print (public style) *)
  printSectionCell[
    Switch[generatorKey,
      "GAUSS", "Gaussova eliminácia",
      "GJ", "Gauss–Jordanova eliminácia",
      "GJ_PIVOT", "Gauss–Jordanova eliminácia (pivotovanie)",
      _, "Eliminácia"
    ]
  ];

  printSubsectionCell["Zadanie"];
  printTextCell["Riešte sústavu rovníc v množine celých čísel."];

  printFormulaCell @ Grid[
    List /@ (tf /@ buildTaskEquations[data2["A"], data2["b"], buildVars[n]]),
    Alignment -> Left, Spacings -> {0, 0.8}
  ];

  If[mode === "TASK_STEPS_RESULT",
    printSubsectionCell["Postup"];
    Scan[renderStepItem, solvedPkg["Content"]];
  ];

  If[mode =!= "TASK",
    printSubsectionCell["Výsledok"];
    Switch[st,
      "ONE", printFormulaCell[Row[{"x = ", TraditionalForm[solvedPkg["Solution"]]}]],
      "NONE", printTextCell["Sústava nemá riešenie."],
      "INFINITE", printTextCell["Sústava má nekonečne veľa riešení."],
      _, Null
    ];
  ];
];

(* ~-~-~ PACKAGE EXPORTS ~-~-~ *)

GenTriangular[diff_String, mode_String, opts : OptionsPattern[]] := Module[{res}, res = Gen01[diff, mode, FilterRules[{opts}, Options[Gen01]]];If[res === $Failed, Message[GenTriangular::fail]];res];
GenGauss[diff_String, mode_String, opts : OptionsPattern[]] := Module[{res}, res = mgRunGenerator["GAUSS", diff, mode, opts];If[res === $Failed, Message[GenGauss::fail]; Return[$Failed]];res];
GenGaussJordan[diff_String, mode_String, opts : OptionsPattern[]] := Module[{res}, res = mgRunGenerator["GJ", diff, mode, opts];If[res === $Failed, Message[GenGaussJordan::fail]; Return[$Failed]];res];
GenGaussJordanPivot[diff_String, mode_String, opts : OptionsPattern[]] := Module[{res}, res = mgRunGenerator["GJ_PIVOT", diff, mode, opts];If[res === $Failed, Message[GenGaussJordanPivot::fail]; Return[$Failed]];res];

End[];
EndPackage[];
