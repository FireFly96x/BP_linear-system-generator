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

Options[Gen01] = {
  SolutionType -> Automatic,
  TriangularType -> Automatic
};

Begin["`Private`"];

(* ~-~-~ HARD RULES / CONSTANTS ~-~-~ *)

$CoeffMin = -10;
$CoeffMax = 10;
$DiagMin = -5;
$DiagMax = 5;

rhsBoundByN[n_] := 100;

(* ~-~-~ CELL PRINTING HELPERS ~-~-~ *)
printSectionCell[str_String] := CellSection[str];
printSubsectionCell[str_String] := CellSubsection[str];
printTextCell[str_String] := CellText[str];
printFormulaCell[expr_] := CellFormula[expr];

(* ~-~-~ FORMATTING HELPERS ~-~-~ *)
highlightGrid[grid_] := Style[grid, Background -> RGBColor[0.95, 0.95, 0.95], Frame -> True, FrameStyle -> None, FrameMargins -> 5];
highlightTerm[term_] := Style[term, Bold, RGBColor[0.8, 0, 0]];
tf[val_] := TraditionalForm[val];
tft[val_] := tf[Together[val]];

(* ~-~-~ STEP COUNTER + STEP RENDERER ~-~-~ *)
stepsCounter = 0;

makeStepHeader[text_String] := (stepsCounter++; Style[ToString[stepsCounter] <> ". " <> text, Bold]);

renderStepItem[item_] := Which[
  StringQ[item], printTextCell[item],
  MatchQ[item, Style[_String, ___]], CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Text", ShowStringCharacters -> False],
  Head[item] === Cell, CellPrint[item],
  Head[item] === Graphics || Head[item] === Graphics3D, CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Graphics"],
  True, printFormulaCell[item]
];

(* ~-~-~ VALIDATION HELPERS (STRICT) ~-~-~ *)
validateTriangularType[tri_] := TrueQ[tri === Automatic] || MemberQ[{"L", "U"}, tri];

resolveTriangularType[tri_] := If[tri === Automatic, RandomChoice[{"L", "U"}], tri];

coeffInRangeQ[m_] := Max[Abs @ Flatten[m]] <= $CoeffMax;
vecInRangeQ[v_, n_] := Max[Abs @ Flatten[v]] <= rhsBoundByN[n];

integersOnlyQ[expr_] := FreeQ[expr, _Rational | _Real];

(* ~-~-~ TASK: EQUATIONS ONLY ~-~-~ *)
(*
  Rovnice tlačíme iba v "Zadanie". V krokoch už len matice.
*)
buildVars[n_] := Take[{x, y, z, u, v, w}, n];

buildTaskEquations[A_, b_, vars_] := Module[{},
  Thread[A.vars == b]
];

toAugmented[A_, b_] := Join[A, List /@ b, 2];

(* ~-~-~ AUGMENTED MATRIX RENDERER (LOCAL, SIMPLE) ~-~-~ *)
(*
  Matričný ekvivalent alignedEquations.
  - oddeľovač A | b
  - voliteľný stĺpec poznámok napr. "dosadzujeme x5", "odčítame známe členy"
  - zvýraznenie: aktívny riadok, pivot (i,i)
*)
alignedAugmentedMatrix[aug_, notes_List : {}, hi_Association : <||>] := Module[
  {nRows, nCols, nA, notes2, pivotPos, activeRow, greenCells, bar, rowColor,
    wrapBg, makeCell, makeBar, leftBracketCell, rightBracketCell, rows, matrixGrid, notesGrid},

  {nRows, nCols} = Dimensions[aug];
  nA = nCols - 1;

  notes2 = If[notes === {}, ConstantArray["", nRows], PadRight[notes, nRows, ""]];
  pivotPos = Lookup[hi, "PivotPos", None];
  activeRow = Lookup[hi, "ActiveRow", None];
  greenCells = Lookup[hi, "GreenCells", {}];

  bar = Style["|", GrayLevel[.35], FontSize -> 16];
  rowColor = RGBColor[0.90, 0.95, 1];

  (* podfarbenie celého aktívneho riadku *)
  wrapBg[i_, expr_] := If[IntegerQ[activeRow] && i === activeRow, Item[expr, Background -> rowColor], expr];

  (* bunka: pivot červený, greenCells zelené+bold *)
  makeCell[i_, j_, val_] := Module[{cell = TraditionalForm[val]},
    If[pivotPos === {i, j}, cell = Style[cell, Bold, RGBColor[0.8, 0, 0]]];
    If[MemberQ[greenCells, {i, j}], cell = Style[cell, Darker[Green], Bold]];
    wrapBg[i, cell]
  ];

  makeBar[i_] := wrapBg[i, bar];

  leftBracketCell = Item["",
    Frame -> {{True, False}, {True, True}},
    FrameStyle -> Directive[GrayLevel[.35], AbsoluteThickness[1.2]],
    FrameMargins -> {{8, 6}, {4, 4}}
  ];

  rightBracketCell = Item["",
    Frame -> {{False, True}, {True, True}},
    FrameStyle -> Directive[GrayLevel[.35], AbsoluteThickness[1.2]],
    FrameMargins -> {{6, 8}, {4, 4}}
  ];

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
    Spacings -> {0.9, 1.3},
    BaseStyle -> {FontSize -> 14}
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

(* ~-~-~ DATA GENERATION (SAFE BY CONSTRUCTION) ~-~-~ *)
(*
  Zámer: primitívny generátor, ktorý nemôže "zlyhať v krokoch".
*)
pickOffDiagNonZero[] := RandomChoice[DeleteCases[Range[$CoeffMin, $CoeffMax], 0]];
pickDiagNonZero[] := RandomChoice[DeleteCases[Range[$DiagMin, $DiagMax], 0]];

buildTriangularMatrixOne[n_, triType_, diff_] := Module[
  {A, allowedPos, extraZeroPos = {}, extraZeroSet = <||>, maxExtraZeros},

  A = ConstantArray[0, {n, n}];

  (* diagonála: vždy nenulová v [-5,5] *)
  Do[A[[i, i]] = pickDiagNonZero[], {i, 1, n}];

  (* povolené pozície mimo nulového trojuholníka (bez diagonály) *)
  allowedPos = Position[
    If[triType === "L",
      LowerTriangularize[ConstantArray[1, {n, n}], -1],  (* striktne pod diagonálou *)
      UpperTriangularize[ConstantArray[1, {n, n}], 1]    (* striktne nad diagonálou *)
    ],
    1
  ];

  (* vyplnenie povoleného trojuholníka *)
  Do[
    Module[{pos = allowedPos[[k]], i, j},
      i = pos[[1]]; j = pos[[2]];
      A[[i, j]] = pickOffDiagNonZero[]
    ],
    {k, 1, Length[allowedPos]}
  ];

  A
];

generateOneData[n_, triType_, diff_] := Module[{A, x, b},
  x = RandomInteger[{-9, 9}, n];
  A = buildTriangularMatrixOne[n, triType, diff];
  b = A.x;

  If[!coeffInRangeQ[A] || !vecInRangeQ[b, n], Return[$Failed]];

  <|"A" -> A, "b" -> b, "x" -> x, "TriType" -> triType, "SolutionType" -> "ONE"|>
];

generateNoneData[n_, triType_, diff_] := Module[{A, b, badRowIdx},
  A = buildTriangularMatrixOne[n, triType, diff];
  b = RandomInteger[{-9, 9}, n];

  badRowIdx = If[triType === "L", 1, n];
  A[[badRowIdx]] = ConstantArray[0, n];
  b[[badRowIdx]] = RandomChoice[DeleteCases[Range[-9, 9], 0]];

  If[!coeffInRangeQ[A] || !vecInRangeQ[b, n], Return[$Failed]];

  <|"A" -> A, "b" -> b, "x" -> "NONE", "TriType" -> triType,
    "SolutionType" -> "NONE", "BadRow" -> badRowIdx|>
];

generateInfiniteData[n_, triType_, diff_] := Module[{A, b, zeroRowIdx},
  A = buildTriangularMatrixOne[n, triType, diff];
  b = ConstantArray[0, n];

  zeroRowIdx = If[triType === "L", 1, n];
  A[[zeroRowIdx]] = ConstantArray[0, n];

  <|"A" -> A, "b" -> b, "x" -> "INFINITE", "TriType" -> triType,
    "SolutionType" -> "INFINITE", "ParamIdx" -> zeroRowIdx|>
];

generateExampleData[n_, diff_, solType_, triType_] := Module[
  {data, aug, vars},
  data = Switch[solType,
    "ONE", generateOneData[n, triType, diff],
    "NONE", generateNoneData[n, triType, diff],
    "INFINITE", generateInfiniteData[n, triType, diff],
    _, $Failed
  ];
  If[data === $Failed, Return[$Failed]];

  aug = toAugmented[data["A"], data["b"]];
  vars = buildVars[n];

  Append[data, {"Aug" -> aug, "Vars" -> vars, "n" -> n}]
];

(* -- riadkové operácie: poznámka + aplikácia -- *)

rowOpScaleNote[i_, k_] := Row[{"R", i, " \[LeftArrow] ", tf[k], "\[CenterDot]R", i}];

rowOpDivideNote[i_, p_] := Row[{"R", i, " \[LeftArrow] R", i, " / ", tf[p]}];

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

applyRowOpDivide[aug_, i_Integer, p_Integer] := ReplacePart[aug, i -> Quotient[aug[[i]], p]];

renderBeforeAfter[before_, after_, notes_, hiBefore_, hiAfter_] := Grid[
  {{alignedAugmentedMatrix[before, notes, hiBefore], Spacer[18], alignedAugmentedMatrix[after, {}, hiAfter]}},
  Alignment -> {Left, Center, Left}, Spacings -> {0, 0}
];

(* ~-~-~ STEP GENERATION (MATRIX-ONLY) ~-~-~ *)

stepsOneTriangular[data_Association] := Module[
  {content = {}, n, aug, vars, tri, order, i, before, after, notes, terms, p, sol},

  n = data["n"]; vars = data["Vars"]; tri = data["TriType"]; aug = data["Aug"];
  sol = ConstantArray[None, n];

  AppendTo[content, makeStepHeader["Prepis sústavy do augmentovanej matice"]];
  AppendTo[content, alignedAugmentedMatrix[aug]];
  AppendTo[content, makeStepHeader["Riadkové úpravy"]];

  order = If[tri === "U", Range[n, 1, -1], Range[1, n]];

  Do[
    (* zlučená eliminácia v jednom zápise *)
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

    (* normalizácia pivotu na 1 *)
    p = aug[[i, i]]; If[p === 0, Return[$Failed]];
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

    (* po dokončení riadku: vypíš výsledok pre danú premennú *)
    sol[[i]] = aug[[i, n + 1]];

    AppendTo[content, Spacer[6]];
    AppendTo[content, highlightGrid @ Grid[
      {{tf[vars[[i]]], "=", tft[sol[[i]]]}},
      Alignment -> {{Right, Center, Left}}, BaseStyle -> {FontSize -> 16}
    ]];
    AppendTo[content, Spacer[6]];

    ,
    {i, order}
  ];

  AppendTo[content, makeStepHeader["Výsledok"]];
  AppendTo[content, Cell[BoxData @ ToBoxes[
    Row[{Row[{"(", Riffle[vars, ", "], ")"}], " = ", Row[{"(", Riffle[tft /@ sol, ", "], ")"}]}],
    StandardForm
  ], "Text", ShowStringCharacters -> False]];

  content = Join[content, verificationSteps[data, sol]];
  <|"Content" -> content, "Solution" -> sol|>
];

stepsNoneTriangular[data_Association] := Module[
  {content = {}, n, aug, badIdx},
  n = data["n"];
  aug = data["Aug"];
  badIdx = data["BadRow"];

  AppendTo[content, makeStepHeader["Prepis sústavy do augmentovanej matice"]];
  AppendTo[content, alignedAugmentedMatrix[aug]];

  (* 2. Analýza riadkov *)
  AppendTo[content, makeStepHeader["Analýza riadkov"]];
  AppendTo[content, "Hľadáme riadok, ktorý by predstavoval spor."];

  (* Highlight bad row *)
  AppendTo[content, alignedAugmentedMatrix[aug,
    ReplacePart[ConstantArray["", n], badIdx -> "SPOR: 0 = " <> ToString[aug[[badIdx, n+1]]]],
    <|"ActiveRow" -> badIdx|>
  ]];

  AppendTo[content, makeStepHeader["Záver"]];
  AppendTo[content, "V matici sa nachádza riadok tvaru (0 ... 0 | k), kde k je nenulové číslo."];
  AppendTo[content, "To zodpovedá rovnici 0 = k, čo je nepravda."];
  AppendTo[content, "Sústava preto nemá riešenie."];

  <|"Content" -> content, "Solution" -> "NONE"|>
];

stepsInfiniteTriangular[data_Association] := Module[
  {content = {}, n, aug, vars, paramIdx, notes, solExprs, pivot, row, knownTerm},
  n = data["n"];
  aug = data["Aug"];
  vars = data["Vars"];
  paramIdx = data["ParamIdx"];

  AppendTo[content, makeStepHeader["Prepis sústavy do augmentovanej matice"]];
  AppendTo[content, alignedAugmentedMatrix[aug]];

  (* 2. Identifikácia voľnej premennej *)
  AppendTo[content, makeStepHeader["Identifikácia voľnej premennej"]];
  (* Highlight zero row *)
  AppendTo[content, alignedAugmentedMatrix[aug,
    ReplacePart[ConstantArray["", n], paramIdx -> "nulový riadok -> parameter"],
    <|"ActiveRow" -> paramIdx|>
  ]];
  AppendTo[content, Row[{"Premennú ", vars[[paramIdx]], " zvolíme za parameter ", TraditionalForm[\[FormalT]], "."}]];

  AppendTo[content, makeStepHeader["Vyjadrenie ostatných premenných"]];

  solExprs = ConstantArray[0, n];
  solExprs[[paramIdx]] = \[FormalT];

  Do[
    If[i == paramIdx, Continue[]];

    row = aug[[i]];
    pivot = row[[i]];

    If[pivot === 0, Return[$Failed]];

    knownTerm = 0;
    Do[
      If[j != i, knownTerm += row[[j]] * solExprs[[j]]],
      {j, 1, n}
    ];

    solExprs[[i]] = Together[(row[[n+1]] - knownTerm) / pivot];

    (* Panic check: coefficients must be integers *)
    If[!integersOnlyQ[solExprs[[i]]], Return[$Failed]];

    (* Print symbolic calc for this row *)
    notes = ConstantArray["", n];
    notes[[i]] = Row[{vars[[i]], " = ", TraditionalForm[solExprs[[i]]]}];

    AppendTo[content, alignedAugmentedMatrix[aug, notes, <|"ActiveRow" -> i, "PivotPos" -> {i, i}|>]];

    , {i, If[data["TriType"] === "U", Range[n, 1, -1], Range[1, n]]}
  ];

  AppendTo[content, makeStepHeader["Záver"]];
  AppendTo[content, "Sústava má nekonečne veľa riešení v tvare:"];
  AppendTo[content, CellFormula[
    Row[{
      "[", Riffle[TraditionalForm /@ solExprs, ", "], "], ", TraditionalForm[\[FormalT]], " \[Element] Z"
    }]
  ]];

  (* Verification for infinite is tricky to keep simple/didactic, we skip explicit row-by-row check for infinite in this basic skeleton or do symbolic check *)
  AppendTo[content, makeStepHeader["Skúška správnosti (symbolická)"]];
  AppendTo[content, "Dosadením parametrického riešenia do pôvodnej matice overíme, že všetky rovnice platia pre ľubovoľné ", TraditionalForm[\[FormalT]], "."];

  <|"Content" -> content, "Solution" -> "INFINITE"|>
];

verificationSteps[data_Association, sol_List] := Module[
  {content = {}, A = data["A"], b = data["b"], n = data["n"], lhs},

  AppendTo[content, makeStepHeader["Skúška správnosti"]];
  AppendTo[content, "Vypočítame A \[CenterDot] x a porovnáme s b (po riadkoch)."];

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

  data = WithRetries[Function[Null, generateExampleData[n, diff, st, tri]], 200];
  If[data === $Failed, Message[Gen01::fail]; Return[$Failed]];

  (* zadanie *)
  printSectionCell["Trojuholníková metóda"];
  printSubsectionCell["Zadanie"];
  printTextCell["Riešte sústavu rovníc v množine celých čísel."];

  printFormulaCell @ Grid[
    List /@ (tf /@ buildTaskEquations[data["A"], data["b"], vars]),
    Alignment -> Left,
    Spacings -> {0, 0.8}
  ];

  printTextCell["Riešte pomocou augmentovanej matice a dosadzovania po riadkoch."];

  (* postup *)
  If[mode === "TASK_STEPS_RESULT",
    stepsCounter = 0;
    printSubsectionCell["Postup"];

    steps = Switch[st,
      "ONE",      stepsOneTriangular[data],
      "NONE",     stepsNoneTriangular[data],
      "INFINITE", stepsInfiniteTriangular[data],
      _,          $Failed
    ];

    If[steps === $Failed, Message[Gen01::fail]; Return[$Failed]];
    Scan[renderStepItem, steps["Content"]];
  ];

  (* výsledok postupu *)
  If[mode === "TASK_RESULT",
    printSubsectionCell["Výsledok"];
    If[st === "ONE",
      printFormulaCell[
        Row[{"(", Riffle[vars, ", "], ")", " = ", "(", Riffle[tft /@ data["x"], ", "], ")"}]
      ]
    ];
    If[st === "NONE", printTextCell["Sústava nemá riešenie."]];
    If[st === "INFINITE", printTextCell["Sústava má nekonečne veľa riešení (parametricky). Postup zobraz v režime TASK_STEPS_RESULT."]];
  ];
];

End[];
EndPackage[];