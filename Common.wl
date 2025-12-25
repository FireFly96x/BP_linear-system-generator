(* ::Package:: *)

BeginPackage["MojeGeneratory`Common`"];

(*
  BALÍK: MojeGeneratory`Common`
  ÚČEL: Spoločné pomocné funkcie pre všetky generátory lineárnych sústav tvaru A x = b.
*)

Internal`$ContextMarks = False;

(* ~-~-~ PUBLIC API ~-~-~ *)

ResolveSolutionType; ValidateDifficulty; ValidateMode; ValidateSolutionType;
CellPrintStyle; CellText; CellExpr; CellSection; CellSubsection; CellTextExpr; CellFormula;
IsAllowedFraction; ValidateStepNumbers; WithRetries; DimensionByDifficulty;

Begin["`Private`"];

(* ~-~-~ COMMON VALIDATION HELPERS ~-~-~ *)

ValidateDifficulty[diff_] := MemberQ[{"EASY", "MEDIUM", "HARD"}, diff];

ValidateMode[mode_] := MemberQ[{"TASK", "TASK_RESULT", "TASK_STEPS_RESULT"}, mode];

ValidateSolutionType[st_] := TrueQ[st === Automatic] || MemberQ[{"ONE", "NONE", "INFINITE"}, st];

ResolveSolutionType[st_] := If[st =!= Automatic, st, RandomChoice[{0.6, 0.2, 0.2} -> {"ONE", "NONE", "INFINITE"}]];

(* ~-~-~ COMMON CELL PRINTING HELPERS ~-~-~ *)

inNotebookQ[] := Head @ Quiet[EvaluationNotebook[]] === NotebookObject;
CellPrintStyle[expr_, style_String] := If[inNotebookQ[], CellPrint @ Cell[expr, style, ShowStringCharacters -> False], Print[expr]];
CellText[str_String] := CellPrintStyle[str, "Text"];
CellExpr[expr_] := Module[{boxes}, boxes = Quiet @ Check[BoxData @ ToBoxes[expr, StandardForm], expr]; CellPrintStyle[boxes, "Input"]];
CellSection[str_String] := CellPrintStyle[str, "Section"];
CellSubsection[str_String] := CellPrintStyle[str, "Subsection"];
CellTextExpr[expr_] := Module[{boxes}, boxes = Quiet @ Check[BoxData @ ToBoxes[expr, StandardForm], expr]; CellPrintStyle[boxes, "Text"]];
CellFormula[expr_] := Module[{boxes}, boxes = Quiet @ Check[BoxData @ ToBoxes[expr, StandardForm], expr]; CellPrintStyle[boxes, "DisplayFormula"]];

(* ~-~-~ COMMON NUMBER VALIDATION (DIDACTIC RULES) ~-~-~ *)

IsAllowedFraction[q_] := Module[{qq},
  qq = Quiet @ Check[Rationalize[q, 0], q];
  IntegerQ[qq] || MatchQ[qq, (1 | -1)/2 | (1 | -1)/3]
];

ValidateStepNumbers[expr_] := Module[{rats},
  rats = Cases[expr, _Rational, Infinity];
  AllTrue[rats, IsAllowedFraction]
];

(* ~-~-~ COMMON RETRY / REGENERATION LOGIC ~-~-~ *)

WithRetries[f_, max_Integer : 200] := Module[{res = $Failed, i = 0},
  While[res === $Failed && i < max, i++; res = f[]];
  res
];

(* ~-~-~ COMMON DIMENSION DISPATCH ~-~-~ *)



End[];
EndPackage[];
