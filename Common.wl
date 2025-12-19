(* ::Package:: *)

BeginPackage["MojeGeneratory`Common`"];

(* =============================================================================
   BAL\[CapitalIAcute]K: MojeGeneratory`Common`
   POPIS:
     Tento bal\[IAcute]k obsahuje zdie\:013ean\[EAcute] pomocn\[EAcute] funkcie, kon\[SHacek]tanty a valid\[AAcute]tory, 
     ktor\[EAcute] vyu\[ZHacek]\[IAcute]vaj\[UAcute] v\[SHacek]etky gener\[AAcute]tory line\[AAcute]rnych s\[UAcute]stav (Elimination, Gauss, at\[DHacek].).
     
     Zabezpe\[CHacek]uje jednotn\[EAcute] spr\[AAcute]vanie pre:
       - V\[YAcute]ber typu rie\[SHacek]enia (jedno / \[ZHacek]iadne / nekone\[CHacek]ne ve\:013ea).
       - Valid\[AAcute]ciu vstupn\[YAcute]ch parametrov (obtia\[ZHacek]nos\[THacek], re\[ZHacek]im).
       - Form\[AAcute]tovanie v\[YAcute]stupu do notebooku (bunky Text, Input, Graphics).
       - Kontrolu "pekn\[YAcute]ch" \[CHacek]\[IAcute]sel (povolen\[EAcute] zlomky).
   =============================================================================
*)

Internal`$ContextMarks = False;

(* ======================== PUBLIC API (USAGE MESSAGES) ======================== *)

ResolveSolutionType::usage =
"ResolveSolutionType[st] rozhodne o type rie\[SHacek]enia s\[UAcute]stavy.
Vstup:
  st: Automatic | \"ONE\" | \"NONE\" | \"INFINITE\"
V\[YAcute]stup:
  Re\[THacek]azec \"ONE\", \"NONE\" alebo \"INFINITE\".
Pozn\[AAcute]mka:
  Ak je vstup Automatic, funkcia n\[AAcute]hodne vyberie typ s pravdepodobnos\[THacek]ou:
  - 80% pre pr\[AAcute]ve jedno rie\[SHacek]enie (ONE)
  - 10% pre \[ZHacek]iadne rie\[SHacek]enie (NONE)
  - 10% pre nekone\[CHacek]ne ve\:013ea rie\[SHacek]en\[IAcute] (INFINITE)";

ValidateDifficulty::usage =
"ValidateDifficulty[diff] over\[IAcute], \[CHacek]i je zadan\[AAcute] obtia\[ZHacek]nos\[THacek] platn\[AAcute].
Vstup:
  diff: Re\[THacek]azec (napr. \"EASY\")
V\[YAcute]stup:
  True, ak je diff jedno z {\"EASY\", \"MEDIUM\", \"HARD\"}, inak False.";

ValidateMode::usage =
"ValidateMode[mode] over\[IAcute], \[CHacek]i je zadan\[YAcute] re\[ZHacek]im v\[YAcute]stupu platn\[YAcute].
Vstup:
  mode: Re\[THacek]azec (napr. \"TASK\")
V\[YAcute]stup:
  True, ak je mode jedno z {\"TASK\", \"TASK_RESULT\", \"TASK_STEPS_RESULT\"}, inak False.";

ValidateSolutionType::usage =
"ValidateSolutionType[st] over\[IAcute], \[CHacek]i je parameter typu rie\[SHacek]enia platn\[YAcute].
Vstup:
  st: Symbol Automatic alebo re\[THacek]azec typu rie\[SHacek]enia.
V\[YAcute]stup:
  True, ak je st platn\[EAcute], inak False.";

CellText::usage =
"CellText[str] vyp\[IAcute]\[SHacek]e textov\[YAcute] re\[THacek]azec do aktu\[AAcute]lneho notebooku ako bunku \[SHacek]t\[YAcute]lu \"Text\".
Vstup:
  str: Re\[THacek]azec textu.
Pozn\[AAcute]mka:
  Ak k\[OAcute]d nebe\[ZHacek]\[IAcute] v notebooku (napr. konzola), pou\[ZHacek]ije sa oby\[CHacek]ajn\[YAcute] Print.";

CellExpr::usage =
"CellExpr[expr] vyp\[IAcute]\[SHacek]e v\[YAcute]raz (matematiku) do aktu\[AAcute]lneho notebooku ako bunku \[SHacek]t\[YAcute]lu \"Input\" (v\[YAcute]po\[CHacek]tov\[AAcute] bunka).
Vstup:
  expr: \:013dubovo\:013en\[YAcute] v\[YAcute]raz Mathematica.
Pozn\[AAcute]mka:
  Sl\[UAcute]\[ZHacek]i na zobrazovanie rovn\[IAcute]c a mat\[IAcute]c v \[SHacek]tandardnej form\[AAcute]tovanej podobe.";

CellPrintStyle::usage =
"CellPrintStyle[expr, style] je n\[IAcute]zko\[UAcute]rov\[NHacek]ov\[AAcute] funkcia na z\[AAcute]pis bunky s konkr\[EAcute]tnym \[SHacek]t\[YAcute]lom.
Vstup:
  expr: Obsah bunky.
  style: Re\[THacek]azec n\[AAcute]zvu \[SHacek]t\[YAcute]lu (napr. \"Section\", \"Text\", \"Input\").";

IsAllowedFraction::usage =
"IsAllowedFraction[q] skontroluje, \[CHacek]i je \[CHacek]\[IAcute]slo 'didakticky pekn\[EAcute]'.
Vstup:
  q: \[CapitalCHacek]\[IAcute]slo (Integer alebo Rational).
V\[YAcute]stup:
  True, ak je q cel\[EAcute] \[CHacek]\[IAcute]slo, alebo jednoduch\[YAcute] zlomok typu +/- 1/2, 1/3, 1/4, 1/5.
  False pre zlo\[ZHacek]itej\[SHacek]ie zlomky (napr. 3/7, 11/13).";

ValidateStepNumbers::usage =
"ValidateStepNumbers[expr] rekurz\[IAcute]vne skontroluje v\[SHacek]etky \[CHacek]\[IAcute]sla vo v\[YAcute]raze.
Vstup:
  expr: \:013dubovo\:013en\[YAcute] v\[YAcute]raz.
V\[YAcute]stup:
  True, ak v\[SHacek]etky racion\[AAcute]lne \[CHacek]\[IAcute]sla vo v\[YAcute]raze sp\:013a\[NHacek]aj\[UAcute] podmienku IsAllowedFraction.";

WithRetries::usage =
"WithRetries[f, max] sa pok\[UAcute]\[SHacek]a opakovane spusti\[THacek] funkciu f, k\[YAcute]m nevr\[AAcute]ti platn\[YAcute] v\[YAcute]sledok.
Vstup:
  f: Funkcia bez argumentov (Function[Null, ...]), ktor\[AAcute] vr\[AAcute]ti $Failed pri ne\[UAcute]spechu.
  max: Maxim\[AAcute]lny po\[CHacek]et pokusov (predvolen\[EAcute] 200).
V\[YAcute]stup:
  V\[YAcute]sledok funkcie f alebo $Failed, ak sa to nepodar\[IAcute] ani po max pokusoch.";

DimensionByDifficulty::usage =
"DimensionByDifficulty[generatorKey, diff] vr\[AAcute]ti rozmer matice (po\[CHacek]et premenn\[YAcute]ch) na z\[AAcute]klade typu gener\[AAcute]tora a obtia\[ZHacek]nosti.
Vstup:
  generatorKey: Re\[THacek]azec identifikuj\[UAcute]ci gener\[AAcute]tor (napr. \"Elimination\", \"Gauss\").
  diff: Obtia\[ZHacek]nos\[THacek] (\"EASY\", \"MEDIUM\", \"HARD\").
V\[YAcute]stup:
  Cel\[EAcute] \[CHacek]\[IAcute]slo (napr. 2, 3, 4...).";

(* Export symbolov pre pou\[ZHacek]itie v in\[YAcute]ch bal\[IAcute]koch *)
ResolveSolutionType;
ValidateDifficulty;
ValidateMode;
ValidateSolutionType;
CellText;
CellExpr;
CellPrintStyle;
IsAllowedFraction;
ValidateStepNumbers;
WithRetries;
DimensionByDifficulty;

Begin["`Private`"];

(* ======================== VALID\[CapitalAAcute]CIE ======================== *)

ValidateDifficulty[diff_] := MemberQ[{"EASY", "MEDIUM", "HARD"}, diff];

ValidateMode[mode_] := MemberQ[{"TASK", "TASK_RESULT", "TASK_STEPS_RESULT"}, mode];

ValidateSolutionType[st_] := (st === Automatic) || MemberQ[{"ONE", "NONE", "INFINITE"}, st];

ResolveSolutionType[st_] := Module[{r},
  If[st =!= Automatic, Return[st]];
  r = RandomReal[];
  Which[
    r < 0.8, "ONE",      (* 80% \[SHacek]anca *)
    r < 0.9, "NONE",     (* 10% \[SHacek]anca *)
    True,    "INFINITE"  (* 10% \[SHacek]anca *)
  ]
];

(* ======================== NOTEBOOK OUTPUT ======================== *)

CellPrintStyle[expr_, style_String] := Module[{},
  Quiet @ Check[
    If[Head[EvaluationNotebook[]] === NotebookObject,
      CellPrint @ Cell[expr, style],
      Print[expr] (* Fallback pre konzolu/skript *)
    ],
    Print[expr]
  ]
];

CellText[str_String] := CellPrintStyle[str, "Text"];

CellExpr[expr_] := Module[{boxes},
  (* Konverzia na boxy zabezpe\[CHacek]\[IAcute] zachovanie form\[AAcute]tovania (napr. 2D zlomky) *)
  boxes = Quiet @ Check[BoxData @ ToBoxes[expr], expr];
  CellPrintStyle[boxes, "Input"]
];

(* ======================== FRAKCIE / KONTROLY ======================== *)

IsAllowedFraction[q_] := Module[{qq},
  (* Rationalize zabezpe\[CHacek]\[IAcute], \[ZHacek]e ak pr\[IAcute]de 0.5, zmen\[IAcute] sa na 1/2 *)
  qq = Quiet @ Check[Rationalize[q, 0], q];
  
  If[IntegerQ[qq], True,
    (* Povolen\[EAcute] s\[UAcute] len menovatele 2, 3, 4, 5 a \[CHacek]itate\:013e +/- 1 *)
    MatchQ[qq, (1 | -1)/2 | (1 | -1)/3 | (1 | -1)/4 | (1 | -1)/5]
  ]
];

ValidateStepNumbers[expr_] := Module[{rats},
  (* N\[AAcute]jde v\[SHacek]etky racion\[AAcute]lne \[CHacek]\[IAcute]sla hlboko v \[SHacek]trukt\[UAcute]re v\[YAcute]razu *)
  rats = Cases[expr, _Rational, Infinity];
  AllTrue[rats, IsAllowedFraction]
];

(* ======================== RETRIES (Opakovanie generovania) ======================== *)

WithRetries[f_, max_Integer : 200] := Module[{res = $Failed, i = 0},
  While[res === $Failed && i < max,
    i++;
    res = f[];
  ];
  res
];

(* ======================== DIMENZIE (Logika ve\:013ekosti s\[UAcute]stavy) ======================== *)

DimensionByDifficulty[generatorKey_String, diff_String] := Switch[generatorKey,
  "Elimination" | "Substitution",
    (* Pre tieto met\[OAcute]dy (S\[CHacek]\[IAcute]tavacia/Dosadzovacia) s\[UAcute] rozmery men\[SHacek]ie *)
    Switch[diff, "EASY", 2, "MEDIUM", 3, "HARD", 3, _, 3],
  _,
    (* Pre Gaussovu a ostatn\[EAcute] maticov\[EAcute] met\[OAcute]dy s\[UAcute] rozmery v\[ADoubleDot]\[CHacek]\[SHacek]ie *)
    Switch[diff, "EASY", 4, "MEDIUM", 5, "HARD", 6, _, 4]
];

End[];
EndPackage[];
