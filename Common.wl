(* ::Package:: *)

BeginPackage["MojeGeneratory`Common`"];
Internal`$ContextMarks = False;

ResolveSolutionType::usage =
  "ResolveSolutionType[st] vr\[AAcute]ti \"ONE\"|\"NONE\"|\"INFINITE\"; pri Automatic plat\[IAcute] 80% \"ONE\".";

CellText::usage =
  "CellText[str] vlo\[ZHacek]\[IAcute] textov\[UAcute] bunku do aktu\[AAcute]lneho notebooku.";

CellExpr::usage =
  "CellExpr[expr] vlo\[ZHacek]\[IAcute] v\[YAcute]po\[CHacek]tov\[UAcute] (Input) bunku do aktu\[AAcute]lneho notebooku.";

IsAllowedFraction::usage =
  "IsAllowedFraction[q] True, ak q je cel\[EAcute] \[CHacek]\[IAcute]slo alebo \[PlusMinus]1/2, \[PlusMinus]1/3, \[PlusMinus]1/4, \[PlusMinus]1/5.";

ValidateStepNumbers::usage =
  "ValidateStepNumbers[expr] True, ak expr neobsahuje nepr\[IAcute]pustn\[EAcute] zlomky.";

WithRetries::usage =
  "WithRetries[f, max] opakuje volanie f[], k\[YAcute]m nevr\[AAcute]ti nie\[CHacek]o in\[EAcute] ako $Failed (max pokusov).";

DimensionByDifficulty::usage =
  "DimensionByDifficulty[generatorKey, diff] vr\[AAcute]ti rozmer pod\:013ea pravidiel: \"Elimination\"/\"Substitution\" -> 2/3/3; inak -> 4/5/6.";

ResolveSolutionType;
CellText;
CellExpr;
IsAllowedFraction;
ValidateStepNumbers;
WithRetries;
DimensionByDifficulty;

Begin["`Private`"];

ResolveSolutionType[st_] := Module[{r},
  If[st =!= Automatic, Return[st]];
  r = RandomReal[];
  Which[r < 0.8, "ONE", r < 0.9, "NONE", True, "INFINITE"]
];

CellText[str_String] := CellPrint @ Cell[str, "Text"];
CellExpr[expr_] := CellPrint @ Cell[BoxData @ ToBoxes[expr], "Input"];

IsAllowedFraction[q_] := Module[{qq},
  qq = Quiet @ Check[Rationalize[q, 0], q];
  If[IntegerQ[qq], True,
    MatchQ[qq, (1|-1)/2 | (1|-1)/3 | (1|-1)/4 | (1|-1)/5]
  ]
];

ValidateStepNumbers[expr_] := Module[{rats},
  rats = Cases[expr, _Rational, Infinity];
  AllTrue[rats, IsAllowedFraction]
];

WithRetries[f_, max_Integer:200] := Module[{res = $Failed, i = 0},
  While[res === $Failed && i < max,
    i++;
    res = f[];
  ];
  res
];

DimensionByDifficulty[generatorKey_String, diff_String] := Switch[generatorKey,
  "Elimination" | "Substitution",
    Switch[diff, "EASY", 2, "MEDIUM", 3, "HARD", 3],
  _,
    Switch[diff, "EASY", 4, "MEDIUM", 5, "HARD", 6]
];

End[];
EndPackage[];
