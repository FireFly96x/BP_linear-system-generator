(* ::Package:: *)

(*
  Package: GaussianEliminationGenerator
  Description: Generates didactic materials for solving linear systems using Gauss, Gauss–Jordan, and Gauss–Jordan with partial pivoting.
  Guarantees: integer solutions; steps avoid fractions or allow only ±1/2, ±1/3; regenerates otherwise.
*)

BeginPackage["MojeGeneratory`GaussianEliminationGenerator`"];

$CharacterEncoding = "UTF-8";
Internal`$ContextMarks = False;

(* ~-~-~ PUBLIC USAGE ~-~-~ *)

Gen01::usage =
    "Gen01[diff, mode, opts] vygeneruje príklad na Gaussovu eliminačnú metódu (REF + spätné dosadzovanie).\n" <>
        "diff: \"EASY\" (4x4), \"MEDIUM\" (5x5), \"HARD\" (6x6)\n" <>
        "mode: \"TASK\", \"TASK_RESULT\", \"TASK_STEPS_RESULT\"\n" <>
        "opts: SolutionType -> Automatic|\"ONE\"|\"NONE\"|\"INFINITE\"";

Gen02::usage =
    "Gen02[diff, mode, opts] vygeneruje príklad na Gauss–Jordanovu metódu (RREF).\n" <>
        "diff: \"EASY\" (4x4), \"MEDIUM\" (5x5), \"HARD\" (6x6)\n" <>
        "mode: \"TASK\", \"TASK_RESULT\", \"TASK_STEPS_RESULT\"\n" <>
        "opts: SolutionType -> Automatic|\"ONE\"|\"NONE\"|\"INFINITE\"";

Gen03::usage =
    "Gen03[diff, mode, opts] vygeneruje príklad na Gauss–Jordanovu metódu s čiastočným pivotovaním (výber hlavného prvku).\n" <>
        "diff: \"EASY\" (4x4), \"MEDIUM\" (5x5), \"HARD\" (6x6)\n" <>
        "mode: \"TASK\", \"TASK_RESULT\", \"TASK_STEPS_RESULT\"\n" <>
        "opts: SolutionType -> Automatic|\"ONE\"|\"NONE\"|\"INFINITE\"";

(* chybové hlášky *)
Gen01::baddiff = "Neplatná obtiažnosť `1`. Použi \"EASY\"|\"MEDIUM\"|\"HARD\".";
Gen01::badmode = "Neplatný režim `1`. Použi \"TASK\"|\"TASK_RESULT\"|\"TASK_STEPS_RESULT\".";
Gen01::badst   = "Neplatný typ riešenia `1`. Použi Automatic|\"ONE\"|\"NONE\"|\"INFINITE\".";
Gen01::fail    = "Nepodarilo sa vygenerovať vhodný príklad.";

Gen02::baddiff = Gen01::baddiff; Gen02::badmode = Gen01::badmode; Gen02::badst = Gen01::badst; Gen02::fail = Gen01::fail;
Gen03::baddiff = Gen01::baddiff; Gen03::badmode = Gen01::badmode; Gen03::badst = Gen01::badst; Gen03::fail = Gen01::fail;

Options[Gen01] = {SolutionType -> Automatic};
Options[Gen02] = {SolutionType -> Automatic};
Options[Gen03] = {SolutionType -> Automatic};

Begin["`Private`"];

(* Z COMMON PACKAGE *)

ValidateDifficulty[diff_] := MemberQ[{"EASY", "MEDIUM", "HARD"}, diff];
ValidateMode[mode_] := MemberQ[{"TASK", "TASK_RESULT", "TASK_STEPS_RESULT"}, mode];
ValidateSolutionType[st_] := TrueQ[st === Automatic] || MemberQ[{"ONE", "NONE", "INFINITE"}, st];
ResolveSolutionType[st_] := If[st =!= Automatic, st, RandomChoice[{0.6, 0.2, 0.2} -> {"ONE", "NONE", "INFINITE"}]];

inNotebookQ[] := Head @ Quiet[EvaluationNotebook[]] === NotebookObject;
CellPrintStyle[expr_, style_String] := If[inNotebookQ[], CellPrint @ Cell[expr, style, ShowStringCharacters -> False], Print[expr]];
CellText[str_String] := CellPrintStyle[str, "Text"];
CellExpr[expr_] := Module[{boxes}, boxes = Quiet @ Check[BoxData @ ToBoxes[expr, StandardForm], expr]; CellPrintStyle[boxes, "Input"]];
CellSection[str_String] := CellPrintStyle[str, "Section"];
CellSubsection[str_String] := CellPrintStyle[str, "Subsection"];
CellTextExpr[expr_] := Module[{boxes}, boxes = Quiet @ Check[BoxData @ ToBoxes[expr, StandardForm], expr]; CellPrintStyle[boxes, "Text"]];
CellFormula[expr_] := Module[{boxes}, boxes = Quiet @ Check[BoxData @ ToBoxes[expr, StandardForm], expr]; CellPrintStyle[boxes, "DisplayFormula"]];

IsAllowedFraction[q_] := Module[{qq},
  qq = Quiet @ Check[Rationalize[q, 0], q];
  IntegerQ[qq] || MatchQ[qq, (1 | -1)/2 | (1 | -1)/3]
];

ValidateStepNumbers[expr_] := Module[{rats},
  rats = Cases[expr, _Rational, Infinity];
  AllTrue[rats, IsAllowedFraction]
];

WithRetries[f_, max_Integer : 200] := Module[{res = $Failed, i = 0},
  While[res === $Failed && i < max, i++; res = f[]];
  res
];

DimensionByDifficulty[generatorKey_String, diff_String] := Which[
  MemberQ[{"Elimination", "Substitution"}, generatorKey],
  Switch[diff, "EASY", 2, "MEDIUM", 3, "HARD", 3, _, 3],
  True,
  Switch[diff, "EASY", 4, "MEDIUM", 5, "HARD", 6, _, 4]
];


(* ~-~-~ CONSTANTS ~-~-~ *)

$CoeffMin = -9;
$CoeffMax = 9;
rhsBound = 100;

(* ~-~-~ CELL PRINTING ~-~-~ *)

printSectionCell[str_String] := CellSection[str];
printSubsectionCell[str_String] := CellSubsection[str];
printTextCell[str_String] := CellText[str];
printFormulaCell[expr_] := CellFormula[expr];

(* ~-~-~ FORMATTING ~-~-~ *)

highlightGrid[grid_] := Style[grid, Background -> RGBColor[0.95, 0.95, 0.95], Frame -> True, FrameStyle -> None, FrameMargins -> 5];
highlightTerm[term_] := Style[term, Bold, RGBColor[0.8, 0, 0]];

tf[val_] := TraditionalForm[val];
tft[val_] := tf[Together[val]];

(* ~-~-~ STEP RENDERER ~-~-~ *)

stepsCounter = 0;

makeStepHeader[text_String] := (stepsCounter++; Style[ToString[stepsCounter] <> ". " <> text, Bold]);

renderStepItem[item_] := Which[
  StringQ[item], printTextCell[item],
  MatchQ[item, Style[_String, ___]], CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Text", ShowStringCharacters -> False],
  Head[item] === Cell, CellPrint[item],
  Head[item] === Graphics || Head[item] === Graphics3D, CellPrint @ Cell[BoxData @ ToBoxes[item, StandardForm], "Graphics"],
  True, printFormulaCell[item]
];

(* ~-~-~ VALIDATION ~-~-~ *)

rhsInRangeQ[b_List] := Max[Abs @ Flatten[b]] <= rhsBound;
integersOnlyQ[expr_] := FreeQ[expr, _Rational | _Real];

(* celé čísla a jednoduché zlomky *)
IsAllowedFraction[x_] := IntegerQ[x] || (RationalQ[x] && MemberQ[{2, 3}, Denominator[x]] && Abs[Numerator[x]] <= 6);

ValidateStepNumbers[expr_] := Module[{vals = Flatten @ {expr}},
  AllTrue[vals, IsAllowedFraction] && Max[Abs[vals]] <= 300
];

safeFirstPosition[list_, pred_] := Module[{pos},
  pos = FirstPosition[list, _?(pred), Missing["NotFound"]];
  If[pos === Missing["NotFound"], None, pos[[1]]]
];


(* ~-~-~ TASK HELPERS ~-~-~ *)

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
    ],
    {i, 1, nRows}
  ];

  matrixGrid = Grid[
    rows,
    Alignment -> Join[{Center}, ConstantArray[Right, nA], {Center, Right, Center}],
    Spacings -> {1, 1},
    BaseStyle -> {FontSize -> 14},
    ItemSize -> {Join[{0.2}, ConstantArray[1.2, nA], {0.2, 1.2, 0.2}], Automatic}
  ];

  notesGrid = Grid[
    List /@ (Item[Pane[Style[#, GrayLevel[.35], FontSize -> 13], {220, Automatic}, Alignment -> Left], Background -> White] & /@ notes2),
    Alignment -> Left, Spacings -> {0, 1.15}, BaseStyle -> {FontSize -> 14}
  ];

  Grid[{{matrixGrid, Spacer[12], notesGrid}}, Alignment -> {Left, Center, Left}, Spacings -> {0, 0}]
];

rowOpSwapNote[i_, j_] := Row[{"R", i, " \[LeftRightArrow] R", j}];
rowOpDivideNote[i_, p_] := Row[{"R", i, " \[LeftArrow] R", i, " / ", tf[p]}];
rowOpCombineNote[i_, terms_List] := Module[{base = Row[{"R", i, " \[LeftArrow] R", i}]},
  Row @ Prepend[(Row[{If[#2 < 0, " - ", " + "], tf[Abs[#2]], "\[CenterDot]R", #1}] & @@@ terms), base]
];

applyRowOpSwap[aug_, i_Integer, j_Integer] := Permute[aug, Cycles[{{i, j}}]];
applyRowOpCombine1[aug_, targetRow_Integer, pivotRow_Integer, factor_] := ReplacePart[aug, targetRow -> (aug[[targetRow]] + factor*aug[[pivotRow]])];
applyRowOpDivideSafe[aug_, i_Integer, p_] := Module[{newRow},
  If[p === 0, Return[$Failed]];
  newRow = aug[[i]]/p;
  If[ValidateStepNumbers[newRow], ReplacePart[aug, i -> newRow], $Failed]
];

renderBeforeAfter[before_, after_, notes_, hiBefore_, hiAfter_] := Grid[
  {{alignedAugmentedMatrix[before, notes, hiBefore], Spacer[18], alignedAugmentedMatrix[after, {}, hiAfter]}},
  Alignment -> {Left, Center, Left}, Spacings -> {0, 0}
];

echelonPivotColumnsQ[aug_, n_Integer] := Module[{A, pivots, r, c, last = 0},
  A = aug[[All, 1 ;; n]];
  Do[
    If[rowIsAllZeroQ[A[[r]]], Continue[]];
    c = firstNonzeroIndex[A[[r]]];
    If[c === None, Continue[]];
    If[c <= last, Return[False]];
    last = c;
    , {r, 1, Length[A]}];
  True
];

findContradictionRow[aug_] := Module[{n = (Dimensions[aug][[2]] - 1), r},
  Do[If[aug[[r, 1 ;; n]] === ConstantArray[0, n] && aug[[r, n + 1]] =!= 0, Return[r]], {r, 1, Dimensions[aug][[1]]}];
  None
];

pivotColumns[aug_] := Module[{n = (Dimensions[aug][[2]] - 1), pivots = {}, r, c, row},
  Do[row = aug[[r, 1 ;; n]]; c = FirstPosition[row, x_ /; x =!= 0, Missing["NotFound"]]; If[c =!= Missing["NotFound"], AppendTo[pivots, c[[1]]]], {r, 1, Dimensions[aug][[1]]}];
  DeleteDuplicates[pivots]
];

freeColumns[aug_] := Module[{n = (Dimensions[aug][[2]] - 1), piv = pivotColumns[aug]}, Complement[Range[n], piv]];

dryRunSafeQ[A_, b_, method_String] := Module[{aug, nRows, nCols, nA, i, j, pivotRowIdx, pivotVal, factor, targetRow},
  aug = toAugmented[A, b];
  {nRows, nCols} = Dimensions[aug];
  nA = nCols - 1;
  If[!rhsInRangeQ[b], Return[False]];
  If[!TrueQ[ValidateStepNumbers[aug]], Return[False]];
  i = 1;
  Do[
    If[i > nRows, Break[]];
    pivotRowIdx = If[method === "GAUSS_JORDAN_PIVOT",
      Module[{candidates, bestK, maxAbs}, candidates = Table[{k, Abs[aug[[k, j]]]}, {k, i, nRows}]; {bestK, maxAbs} = MaximalBy[candidates, Last][[1]]; If[maxAbs == 0, 0, bestK]],
      Module[{k = i}, While[k <= nRows && aug[[k, j]] == 0, k++]; If[k > nRows, 0, k]]
    ];
    If[pivotRowIdx == 0, Continue[]];
    If[pivotRowIdx != i, aug = applyRowOpSwap[aug, i, pivotRowIdx]];
    pivotVal = aug[[i, j]];
    If[method === "GAUSS",
      Do[If[aug[[targetRow, j]] != 0, factor = -aug[[targetRow, j]] / pivotVal; If[!IsAllowedFraction[factor], Return[False]]; aug = applyRowOpCombine1[aug, targetRow, i, factor]; If[!TrueQ[ValidateStepNumbers[aug[[targetRow]]]], Return[False]]], {targetRow, i + 1, nRows}],
      If[pivotVal != 1, If[!IsAllowedFraction[1/pivotVal], Return[False]]; aug = applyRowOpDivideSafe[aug, i, pivotVal]; If[aug === $Failed, Return[False]]];
      Do[If[targetRow != i && aug[[targetRow, j]] != 0, factor = -aug[[targetRow, j]]; aug = applyRowOpCombine1[aug, targetRow, i, factor]; If[!TrueQ[ValidateStepNumbers[aug[[targetRow]]]], Return[False]]], {targetRow, 1, nRows}]
    ];
    If[Max[Abs[Flatten[aug]]] > 300, Return[False]];
    i++;
    , {j, 1, nA}];
  True
];

generateData[n_, diff_, solType_, method_String] := Module[
  {A, b, x, vars, aug, core, tries, U, L, P, perm, badRow, solLS, rA, rAug, rr, piv, free},

  core = $Failed;

  Do[
    Switch[solType,

      "ONE",
      U = IdentityMatrix[n];
      Do[U[[r, c]] = RandomInteger[{-2, 2}], {r, 1, n}, {c, r + 1, n}];

      L = IdentityMatrix[n];
      Do[L[[r, c]] = RandomInteger[{-2, 2}], {r, 1, n}, {c, 1, r - 1}];

      P = IdentityMatrix[n][[RandomSample[Range[n]]]];
      A = P . L . U;

      (* tvrdé bounds na koeficienty *)
      If[Max[Abs[Flatten[A]]] > $CoeffMax, Continue[]];

      x = RandomInteger[{$CoeffMin, $CoeffMax}, n];
      If[AllTrue[x, # == 0 &], Continue[]];

      b = A . x;
      If[!rhsInRangeQ[b], Continue[]]
      ,

      "NONE",
      A = RandomInteger[{-2, 2}, {n, n}];
      A[[-1]] = ConstantArray[0, n];

      b = RandomInteger[{$CoeffMin, $CoeffMax}, n];
      b[[-1]] = RandomChoice[Join[Range[$CoeffMin, -1], Range[1, $CoeffMax]]];

      perm = RandomSample[Range[n]];
      A = A[[perm]];
      b = b[[perm]];

      (* bounds *)
      If[Max[Abs[Flatten[A]]] > $CoeffMax, Continue[]];
      If[!rhsInRangeQ[b], Continue[]];

      badRow = First @ FirstPosition[A, ConstantArray[0, n]]
      ,

      "INFINITE",
      (* konštrukcia hodnosti n-1 (jedna voľná premenná) *)
      A = IdentityMatrix[n];
      A[[-1, -1]] = 0;

      Do[A[[r, c]] = RandomInteger[{-1, 1}], {r, 1, n - 1}, {c, r + 1, n}];

      L = IdentityMatrix[n];
      Do[L[[r, c]] = RandomInteger[{-1, 1}], {r, 1, n}, {c, 1, r - 1}];

      A = L . A;

      If[Max[Abs[Flatten[A]]] > $CoeffMax, Continue[]];

      x = RandomInteger[{$CoeffMin, $CoeffMax}, n];
      If[AllTrue[x, # == 0 &], Continue[]];

      b = A . x;
      If[!rhsInRangeQ[b], Continue[]]
    ];

    (* rýchla bezpečnostná kontrola krokov pre zvolenú metódu *)
    If[!dryRunSafeQ[A, b, method], Continue[]];

    aug = toAugmented[A, b];
    rA = MatrixRank[A];
    rAug = MatrixRank[aug];

    Switch[solType,

      "ONE",
      If[Det[A] === 0, Continue[]];
      solLS = Quiet @ Check[LinearSolve[A, b], $Failed];
      If[solLS === $Failed || !VectorQ[solLS, IntegerQ] || solLS =!= x, Continue[]];
      core = <|"A" -> A, "b" -> b, "x" -> x|>;
      Break[]
      ,

      "NONE",
      If[rA >= rAug, Continue[]];
      core = <|"A" -> A, "b" -> b, "BadRow" -> badRow|>;
      Break[]
      ,

      "INFINITE",
      If[!(rA == rAug && rA == n - 1), Continue[]];

      (* zisti voľný stĺpec konzistentne cez RowReduce *)
      rr = RowReduce[aug];
      piv = pivotColumns[rr];
      free = Complement[Range[n], piv];
      If[Length[free] =!= 1, Continue[]];

      core = <|"A" -> A, "b" -> b, "ParamIdx" -> free[[1]]|>;
      Break[]
    ];

    ,
    {tries, 1, 200}
  ];

  If[core === $Failed, Return[$Failed]];

  vars = buildVars[n];
  aug = toAugmented[core["A"], core["b"]];

  Join[core, <|"Aug" -> aug, "Vars" -> vars, "n" -> n, "SolutionType" -> solType|>]
];

(* ~-~-~ STEP HELPERS ~-~-~ *)

firstNonzeroIndex[row_List] := Module[{pos},
  pos = FirstPosition[row, x_ /; x =!= 0, Missing["NotFound"]];
  If[pos === Missing["NotFound"], None, pos[[1]]]];

rowIsAllZeroQ[row_List] := AllTrue[row, # === 0 &];

backSubstituteFromREF[aug_, n_Integer] := Module[
  {Aref, bref, x, row, pivotCol, pivotVal, rhs, knownSum, i, j},
  Aref = aug[[All, 1 ;; n]];
  bref = aug[[All, n + 1]];
  x = ConstantArray[0, n];
  Do[
    row = Aref[[i]];
    If[rowIsAllZeroQ[row], Continue[]];
    pivotCol = firstNonzeroIndex[row];
    If[pivotCol === None, Continue[]];
    pivotVal = row[[pivotCol]];
    If[pivotVal === 0, Return[$Failed]];
    rhs = bref[[i]];
    knownSum = 0;
    Do[If[j > pivotCol, knownSum += row[[j]] * x[[j]]], {j, pivotCol + 1, n}];
    x[[pivotCol]] = (rhs - knownSum)/pivotVal;
    If[!IntegerQ[x[[pivotCol]]], Return[$Failed]];
    , {i, Length[Aref], 1, -1}];
  x];

solutionFromRREF[aug_, n_Integer] := Module[
  {rref, A, b},
  rref = RowReduce[aug];
  A = rref[[All, 1 ;; n]];
  b = rref[[All, n + 1]];

  (* for ONE: must have full rank n and identity on first n rows *)
  If[Length[rref] < n, Return[$Failed]];
  If[A[[1 ;; n]] =!= IdentityMatrix[n], Return[$Failed]];
  If[!VectorQ[b[[1 ;; n]], IntegerQ], Return[$Failed]];

  <|"RREF" -> rref, "Solution" -> b[[1 ;; n]]|>
];



stepsGauss[data_Association] := Module[
  {
    content = {}, n, aug, vars, solType, sol, i, j, pivotVal, factor, pivotRowIdx,
    notes, nRows, nCols, nA, targetRow, before, after, k,
    pivots, freeCols, paramIdx, solExprs, pivotRowMap, r, rhs, coeff, addHeader, addText,
    addMatrix, addSpacer, leftPart, col
  },

  n = data["n"]; aug = data["Aug"]; vars = data["Vars"]; solType = data["SolutionType"];
  {nRows, nCols} = Dimensions[aug];
  nA = nCols - 1;

  addHeader[text_] := AppendTo[content, makeStepHeader[text]];
  addText[text_] := AppendTo[content, text];
  addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] := AppendTo[content, alignedAugmentedMatrix[m, rowNotes, hi]];
  addSpacer[] := AppendTo[content, Spacer[6]];

  addHeader["Prepis sústavy do augmentovanej matice"];
  addText["Sústavu zapíšeme do augmentovanej matice."];
  addMatrix[aug];

  addHeader["Riadkové úpravy (Gaussova eliminácia)"];
  addText["Upravujeme maticu na stupňovitý tvar (REF). Nulujeme prvky pod pivotmi."];

  i = 1; j = 1;
  While[i <= nRows && j <= nA,

    pivotRowIdx = i;
    While[pivotRowIdx <= nRows && aug[[pivotRowIdx, j]] == 0, pivotRowIdx++];

    If[pivotRowIdx > nRows, j++; Continue[]];

    If[pivotRowIdx != i,
      before = aug;
      after = applyRowOpSwap[before, i, pivotRowIdx];
      notes = ConstantArray["", nRows];
      notes[[i]] = rowOpSwapNote[i, pivotRowIdx];
      notes[[pivotRowIdx]] = rowOpSwapNote[pivotRowIdx, i];
      AppendTo[content, renderBeforeAfter[before, after, notes, <|"PivotPos" -> {pivotRowIdx, j}|>, <|"PivotPos" -> {i, j}|>]];
      aug = after;
    ];

    pivotVal = aug[[i, j]];
    If[pivotVal == 0, j++; Continue[]];

    Do[
      If[aug[[targetRow, j]] != 0,
        factor = -aug[[targetRow, j]]/pivotVal;

        (* povolené iba veľmi jednoduché faktory *)
        If[!IsAllowedFraction[factor], Return[$Failed]];

        before = aug;
        after = applyRowOpCombine1[before, targetRow, i, factor];

        If[after === $Failed || !ValidateStepNumbers[after], Return[$Failed]];

        notes = ConstantArray["", nRows];
        notes[[targetRow]] = rowOpCombineNote[targetRow, {{i, factor}}];
        AppendTo[content, renderBeforeAfter[before, after, notes,
          <|"PivotPos" -> {i, j}, "ActiveRow" -> targetRow|>,
          <|"PivotPos" -> {i, j}, "ActiveRow" -> targetRow|>
        ]];
        aug = after;
      ],
      {targetRow, i + 1, nRows}
    ];

    i++; j++;
  ];

  (* spoločné sanity checky *)
  If[!ValidateStepNumbers[aug], Return[$Failed]];

  Switch[solType,

    "NONE",
    k = findContradictionRow[aug];
    If[k === None, Return[$Failed]];
    addHeader["Analýza výsledku"];
    addText["V matici sa objavil riadok, ktorý reprezentuje spor (0 = k, k \[NotEqual] 0)."];
    addHeader["Skúška správnosti"];
    content = Join[content, verificationStepsNone[Join[data, <|"Aug" -> aug, "BadRow" -> k|>]]];
    sol = "NONE"
    ,

    "ONE",
    If[findContradictionRow[aug] =!= None, Return[$Failed]];

    pivots = pivotColumns[aug];
    If[Length[pivots] =!= n, Return[$Failed]];
    If[!echelonPivotColumnsQ[aug, n], Return[$Failed]];

    addHeader["Spätné dosadzovanie"];
    addText["Zo stupňovitého tvaru postupne určujeme neznáme od poslednej rovnice smerom nahor."];

    sol = backSubstituteFromREF[aug, n];
    If[sol === $Failed || !VectorQ[sol, IntegerQ], Return[$Failed]];

    addSpacer[];
    AppendTo[content, highlightGrid @ Grid[
      Table[{tf[vars[[ii]]], "=", tf[sol[[ii]]]}, {ii, 1, n}],
      Alignment -> {{Right, Center, Left}},
      BaseStyle -> {FontSize -> 16}
    ]];
    addSpacer[];

    addHeader["Skúška správnosti"];
    content = Join[content, verificationSteps[data, sol]]
    ,

    "INFINITE",
    If[findContradictionRow[aug] =!= None, Return[$Failed]];

    pivots = pivotColumns[aug];
    freeCols = freeColumns[aug];
    If[Length[freeCols] =!= 1, Return[$Failed]];
    paramIdx = Lookup[data, "ParamIdx", freeCols[[1]]];

    addHeader["Dopočítanie parametrického riešenia"];
    addText["Sústava má nekonečne veľa riešení. Pre prehľadnosť spravíme nad pivotmi nuly (RREF logika)."];

    (* pivot stĺpec -> pivot riadok podľa REF *)
    pivotRowMap = Association[];
    Do[
      leftPart = aug[[rr, 1 ;; n]];
      If[rowIsAllZeroQ[leftPart], Continue[]];
      col = firstNonzeroIndex[leftPart];
      If[col =!= None, pivotRowMap[col] = rr],
      {rr, 1, nRows}
    ];

    (* sprav pivoty 1 a nuly nad pivotmi *)
    Do[
      col = piv;
      If[!KeyExistsQ[pivotRowMap, col], Return[$Failed]];
      r = pivotRowMap[col];

      pivotVal = aug[[r, col]];
      If[pivotVal == 0, Return[$Failed]];

      If[pivotVal =!= 1,
        before = aug;
        after = applyRowOpDivideSafe[before, r, pivotVal];
        If[after === $Failed || !ValidateStepNumbers[after], Return[$Failed]];

        notes = ConstantArray["", nRows];
        notes[[r]] = rowOpDivideNote[r, pivotVal];
        AppendTo[content, renderBeforeAfter[before, after, notes, <|"PivotPos" -> {r, col}|>, <|"PivotPos" -> {r, col}|>]];
        aug = after;
      ];

      Do[
        If[targetRow != r && aug[[targetRow, col]] != 0,
          factor = -aug[[targetRow, col]];
          If[!IsAllowedFraction[factor], Return[$Failed]];

          before = aug;
          after = applyRowOpCombine1[before, targetRow, r, factor];
          If[after === $Failed || !ValidateStepNumbers[after], Return[$Failed]];

          notes = ConstantArray["", nRows];
          notes[[targetRow]] = rowOpCombineNote[targetRow, {{r, factor}}];
          AppendTo[content, renderBeforeAfter[before, after, notes, <|"PivotPos" -> {r, col}, "ActiveRow" -> targetRow|>, <|"PivotPos" -> {r, col}, "ActiveRow" -> targetRow|>]];
          aug = after;
        ],
        {targetRow, 1, nRows}
      ];
      ,
      {piv, Reverse[pivots]}
    ];

    (* parametrizácia – vyžaduj celé koeficienty *)
    solExprs = ConstantArray[0, n];
    solExprs[[paramIdx]] = \[FormalT];

    Do[
      col = piv;
      If[!KeyExistsQ[pivotRowMap, col], Return[$Failed]];
      r = pivotRowMap[col];

      rhs = aug[[r, n + 1]];
      coeff = aug[[r, paramIdx]];

      If[!IntegerQ[rhs] || !IntegerQ[coeff], Return[$Failed]];
      solExprs[[col]] = rhs - coeff*\[FormalT];
      ,
      {piv, pivots}
    ];

    addSpacer[];
    AppendTo[content, highlightGrid @ Grid[
      Table[{tf[vars[[ii]]], "=", tf[solExprs[[ii]]]}, {ii, 1, n}],
      Alignment -> {{Right, Center, Left}},
      BaseStyle -> {FontSize -> 16}
    ]];
    addSpacer[];

    addHeader["Skúška správnosti"];
    content = Join[content, verificationStepsInfinite[data, solExprs]];
    sol = "INFINITE"
  ];

  <|"Content" -> content, "Solution" -> sol|>
];

stepsGaussJordan[data_Association, pivotingQ_] := Module[
  {
    content = {}, n, aug, vars, solType, sol, i, j, k, pivotVal, factor, pivotRowIdx,
    notes, nRows, nCols, nA, targetRow, before, after,
    pivots, freeCols, paramIdx, solExprs, pivotRowMap, r, rhs, coeff, leftPart, col,
    addHeader, addText, addMatrix, addSpacer
  },

  n = data["n"]; aug = data["Aug"]; vars = data["Vars"]; solType = data["SolutionType"];
  {nRows, nCols} = Dimensions[aug];
  nA = nCols - 1;

  addHeader[text_] := AppendTo[content, makeStepHeader[text]];
  addText[text_] := AppendTo[content, text];
  addMatrix[m_, rowNotes_List : {}, hi_Association : <||>] := AppendTo[content, alignedAugmentedMatrix[m, rowNotes, hi]];
  addSpacer[] := AppendTo[content, Spacer[6]];

  addHeader["Prepis sústavy do augmentovanej matice"];
  addText["Sústavu zapíšeme do augmentovanej matice."];
  addMatrix[aug];

  addHeader[If[pivotingQ, "Úprava na RREF s pivotovaním", "Úprava na RREF (Gauss–Jordan)"]];
  addText["Pomocou riadkových úprav vytvoríme pivoty 1 a nuly nad aj pod pivotmi."];

  i = 1; j = 1;
  While[i <= nRows && j <= nA,

    pivotRowIdx = If[pivotingQ,
      Module[{candidates, bestK, maxAbs},
        candidates = Table[{kk, Abs[aug[[kk, j]]]}, {kk, i, nRows}];
        {bestK, maxAbs} = MaximalBy[candidates, Last][[1]];
        If[maxAbs == 0, 0, bestK]
      ],
      Module[{kk = i}, While[kk <= nRows && aug[[kk, j]] == 0, kk++]; If[kk > nRows, 0, kk]]
    ];

    If[pivotRowIdx == 0, j++; Continue[]];

    If[pivotRowIdx != i,
      before = aug;
      after = applyRowOpSwap[before, i, pivotRowIdx];
      notes = ConstantArray["", nRows];
      notes[[i]] = rowOpSwapNote[i, pivotRowIdx];
      notes[[pivotRowIdx]] = rowOpSwapNote[pivotRowIdx, i];
      AppendTo[content, renderBeforeAfter[before, after, notes, <|"PivotPos" -> {pivotRowIdx, j}|>, <|"PivotPos" -> {i, j}|>]];
      aug = after;
    ];

    pivotVal = aug[[i, j]];
    If[pivotVal == 0, j++; Continue[]];

    If[pivotVal != 1,
      before = aug;
      after = applyRowOpDivideSafe[before, i, pivotVal];
      If[after === $Failed || !ValidateStepNumbers[after], Return[$Failed]];
      notes = ConstantArray["", nRows];
      notes[[i]] = rowOpDivideNote[i, pivotVal];
      AppendTo[content, renderBeforeAfter[before, after, notes, <|"PivotPos" -> {i, j}|>, <|"PivotPos" -> {i, j}|>]];
      aug = after;
    ];

    Do[
      If[targetRow != i && aug[[targetRow, j]] != 0,
        factor = -aug[[targetRow, j]];
        If[!IsAllowedFraction[factor], Return[$Failed]];

        before = aug;
        after = applyRowOpCombine1[before, targetRow, i, factor];
        If[after === $Failed || !ValidateStepNumbers[after], Return[$Failed]];

        notes = ConstantArray["", nRows];
        notes[[targetRow]] = rowOpCombineNote[targetRow, {{i, factor}}];
        AppendTo[content, renderBeforeAfter[before, after, notes,
          <|"PivotPos" -> {i, j}, "ActiveRow" -> targetRow|>,
          <|"PivotPos" -> {i, j}, "ActiveRow" -> targetRow|>
        ]];
        aug = after;
      ],
      {targetRow, 1, nRows}
    ];

    i++; j++;
  ];

  If[!ValidateStepNumbers[aug], Return[$Failed]];

  Switch[solType,

    "ONE",
    (* pri jednoznačnom riešení musí byť ľavá časť identita a RHS celé *)
    If[findContradictionRow[aug] =!= None, Return[$Failed]];
    If[aug[[1 ;; n, 1 ;; n]] =!= IdentityMatrix[n], Return[$Failed]];
    sol = aug[[1 ;; n, n + 1]];
    If[!VectorQ[sol, IntegerQ], Return[$Failed]];

    addHeader["Výsledok"];
    addText["Z RREF matice priamo čítame riešenie:"];

    addSpacer[];
    AppendTo[content, highlightGrid @ Grid[
      Table[{tf[vars[[ii]]], "=", tf[sol[[ii]]]}, {ii, 1, n}],
      Alignment -> {{Right, Center, Left}},
      BaseStyle -> {FontSize -> 16}
    ]];
    addSpacer[];

    addHeader["Skúška správnosti"];
    content = Join[content, verificationSteps[data, sol]]
    ,

    "NONE",
    k = findContradictionRow[aug];
    If[k === None, Return[$Failed]];
    addHeader["Analýza výsledku"];
    addText["Nulový riadok na ľavej strane a nenulový na pravej znamená spor."];
    addHeader["Skúška správnosti"];
    content = Join[content, verificationStepsNone[Join[data, <|"Aug" -> aug, "BadRow" -> k|>]]];
    sol = "NONE"
    ,

    "INFINITE",
    If[findContradictionRow[aug] =!= None, Return[$Failed]];

    pivots = pivotColumns[aug];
    freeCols = freeColumns[aug];
    If[Length[freeCols] =!= 1, Return[$Failed]];
    paramIdx = Lookup[data, "ParamIdx", freeCols[[1]]];

    addHeader["Parametrické riešenie"];

    (* pivot col -> pivot row podľa RREF (pivot 1) *)
    pivotRowMap = Association[];
    Do[
      leftPart = aug[[rr, 1 ;; n]];
      If[rowIsAllZeroQ[leftPart], Continue[]];
      col = safeFirstPosition[leftPart, (# == 1 &)];
      If[col =!= None && leftPart[[col]] == 1, pivotRowMap[col] = rr],
      {rr, 1, nRows}
    ];

    solExprs = ConstantArray[0, n];
    solExprs[[paramIdx]] = \[FormalT];

    Do[
      col = piv;
      If[!KeyExistsQ[pivotRowMap, col], Return[$Failed]];
      r = pivotRowMap[col];

      rhs = aug[[r, n + 1]];
      coeff = aug[[r, paramIdx]];

      If[!IntegerQ[rhs] || !IntegerQ[coeff], Return[$Failed]];
      solExprs[[col]] = rhs - coeff*\[FormalT];
      ,
      {piv, pivots}
    ];

    addText[Row[{"Voľná premenná: ", vars[[paramIdx]], " = ", \[FormalT]}]];

    addSpacer[];
    AppendTo[content, highlightGrid @ Grid[
      Table[{tf[vars[[ii]]], "=", tf[solExprs[[ii]]]}, {ii, 1, n}],
      Alignment -> {{Right, Center, Left}},
      BaseStyle -> {FontSize -> 16}
    ]];
    addSpacer[];

    addHeader["Skúška správnosti"];
    content = Join[content, verificationStepsInfinite[data, solExprs]];
    sol = "INFINITE"
  ];

  <|"Content" -> content, "Solution" -> sol|>
];

verificationSteps[data_Association, sol_List] := Module[{content = {}, A = data["A"], b = data["b"], n = data["n"], lhs},
  Do[
    lhs = A[[i]].sol;
    AppendTo[content,
      Grid[
        {
          {Row[{"Riadok ", i, ":  ", tf[A[[i]]], " \[CenterDot] ", tf[sol], " = ", tft[lhs]}]},
          {Row[{"PS", i, " = ", tft[b[[i]]]}]},
          {If[lhs === b[[i]], Style["ĽS = PS (OK)", Darker[Green]], Style["ĽS \[NotEqual] PS (CHYBA)", Red]]}
        },
        Alignment -> Left, Spacings -> {0, 0.4}, BaseStyle -> {FontSize -> 13}
      ]
    ],
    {i, 1, n}
  ];
  content
];

verificationStepsNone[data_Association] := Module[{content = {}, A = data["A"], b = data["b"], aug0, rA, rAug, n, badIdx, rhsVal},
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
      Alignment -> Left, Spacings -> {0, 0.4}, BaseStyle -> {FontSize -> 13}
    ]
  ];

  badIdx = Lookup[data, "BadRow", None];
  If[IntegerQ[badIdx],
    rhsVal = aug0[[badIdx, n + 1]];
    AppendTo[content, "V augmentovanej matici sa to prejaví aj priamo ako riadok tvaru (0 ... 0 | k), kde k \[NotEqual] 0, teda spor 0 = k:"];
    AppendTo[content, alignedAugmentedMatrix[aug0, ReplacePart[ConstantArray["", n], badIdx -> ("SPOR: 0 = " <> ToString[rhsVal])], <|"ActiveRow" -> badIdx|>]];
  ];

  content
];

verificationStepsInfinite[data_Association, solExprs_List] := Module[{content = {}, A = data["A"], b = data["b"], n = data["n"], lhs, diff, okQ, coeffs},
  Do[
    lhs = Together[A[[i]].solExprs];
    diff = Together[lhs - b[[i]]];
    okQ = If[diff === 0, True, If[PolynomialQ[diff, \[FormalT]], coeffs = CoefficientList[Expand[diff], \[FormalT]]; AllTrue[coeffs, # === 0 &], False]];

    AppendTo[content,
      Grid[
        {
          {Row[{"Riadok ", i, ":  ", tf[A[[i]]], " \[CenterDot] ", TraditionalForm[solExprs], " = ", TraditionalForm[lhs]}]},
          {Row[{"PS", i, " = ", TraditionalForm[b[[i]]]}]},
          {Row[{"ĽS - PS = ", TraditionalForm[diff]}]},
          {If[okQ, Style["ĽS = PS (OK)", Darker[Green]], Style["ĽS \[NotEqual] PS (CHYBA)", Red]]}
        },
        Alignment -> Left, Spacings -> {0, 0.4}, BaseStyle -> {FontSize -> 13}
      ]
    ],
    {i, 1, n}
  ];
  content
];

printTheory[method_String] := Module[{},
  printTextCell @ Switch[method,
    "GAUSS", "Princíp: Pomocou elementárnych riadkových úprav prevedieme rozšírenú maticu sústavy na stupňovitý tvar a následne riešime spätným dosadzovaním.",
    "GAUSS_JORDAN", "Princíp: Pomocou elementárnych riadkových úprav prevedieme rozšírenú maticu na redukovaný stupňovitý tvar (RREF) a riešenie priamo vyčítame.",
    "GAUSS_JORDAN_PIVOT", "Princíp: Ako Gauss–Jordan, ale v každom kroku volíme pivot s najväčšou absolútnou hodnotou v stĺpci (čiastočné pivotovanie) kvôli stabilite a prehľadnosti.",
    _ , ""
  ];
];

runGen[diff_String, mode_String, stIn_, method_String] := Module[{n, st, data, steps, vars},
  If[!TrueQ[ValidateDifficulty[diff]], Message[Gen01::baddiff, diff]; Return[$Failed]];
  If[!TrueQ[ValidateMode[mode]], Message[Gen01::badmode, mode]; Return[$Failed]];
  If[!TrueQ[ValidateSolutionType[stIn]], Message[Gen01::badst, stIn]; Return[$Failed]];

  st = ResolveSolutionType[stIn];
  n = DimensionByDifficulty["Gaussian", diff];
  data = WithRetries[Function[Null, generateData[n, diff, st, method]]];
  If[data === $Failed, Message[Gen01::fail]; Return[$Failed]];

  printSectionCell @ Switch[method,
    "GAUSS", "Gaussova eliminačná metóda",
    "GAUSS_JORDAN", "Gauss–Jordanova metóda",
    "GAUSS_JORDAN_PIVOT", "Gauss–Jordanova metóda s pivotovaním",
    _, "Eliminačná metóda"
  ];
  printTheory[method];

  printSubsectionCell["Zadanie"];
  printTextCell["Riešte sústavu lineárnych rovníc v množine celých čísel."];
  vars = data["Vars"];
  printFormulaCell @ Grid[List /@ (tf /@ buildTaskEquations[data["A"], data["b"], vars]), Alignment -> Left, Spacings -> {0, 0.8}];

  If[mode === "TASK_STEPS_RESULT",
    stepsCounter = 0;
    printSubsectionCell["Postup"];
    steps = Switch[method,
      "GAUSS", stepsGauss[data],
      "GAUSS_JORDAN", stepsGaussJordan[data, False],
      "GAUSS_JORDAN_PIVOT", stepsGaussJordan[data, True],
      _, $Failed
    ];
    If[steps === $Failed, Message[Gen01::fail]; Return[$Failed]];
    Scan[renderStepItem, steps["Content"]];
    If[data["SolutionType"] === "ONE" && KeyExistsQ[steps, "Solution"],
      data = Join[data, <|"x" -> steps["Solution"]|>]];
  ];

  If[mode =!= "TASK",
    printSubsectionCell["Výsledok"];
    Switch[data["SolutionType"],
      "ONE", printFormulaCell[Row[Flatten[{"(", Riffle[vars, ", "], ") = (", Riffle[TraditionalForm /@ data["x"], ", "], ")"}]]],
      "NONE", printTextCell["Sústava nemá riešenie."],
      "INFINITE", printTextCell["Sústava má nekonečne veľa riešení."]
    ];
  ];
];

Gen01[diff_String, mode_String, opts : OptionsPattern[]] := runGen[diff, mode, OptionValue[SolutionType], "GAUSS"];
Gen02[diff_String, mode_String, opts : OptionsPattern[]] := runGen[diff, mode, OptionValue[SolutionType], "GAUSS_JORDAN"];
Gen03[diff_String, mode_String, opts : OptionsPattern[]] := runGen[diff, mode, OptionValue[SolutionType], "GAUSS_JORDAN_PIVOT"];

End[];
EndPackage[];