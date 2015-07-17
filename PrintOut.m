(* ::Package:: *)

BeginPackage["PrintOut`"];


PrintOut::usage = "PrintOut[expr] prints expr to $PrintStream and returns erpr. Strings are printed without quotation marks.
PrintOut[list] prints elements of list concatinated.
PrintOut[list, \", \"] or list // PrintOut \", \" uses a separator.";
PrintLine::usage = "PrintLine[expr] works like PrintOut and prints \"\\n\" at end.";
$PrintStream::usage = "$PrintStream is a stream to print on with PrintOut and PrintLine.
Default is stdout.";
$PrePrintOut::usage = "$PrePrintOut is a function to be applied to string to be printed with PrintOut and PrintLine.";
$PrePrintOut = Identity;


$PrintStream = "stdout";


Begin["`Private`"];


(* SetOptions[$Output, FormatType->InputForm]; *)
PrintOut @ string_String := (WriteString[$PrintStream, $PrePrintOut @ string]; string);
PrintOut @ expr_ := (WriteString[$PrintStream, $PrePrintOut @ ToString @ InputForm[expr]]; expr);
PrintOut @ list_List := (PrintOut /@ list; list);
PrintOut[list_List, sep_] := (PrintOut /@ Riffle[list, sep]; list);
PrintOut @ Null = Null;
PrintLine[expr__] := {PrintOut[expr], PrintOut["\n"]}[[1]];
PrintLine[] := (PrintOut["\n"];);
PrintLine @ Null = Null;
Unprotect @ Times;
((pr:PrintOut|PrintLine) sep_)[list_List] := pr[list, sep];
PrintOut[list_List[sep_]] := PrintOut[list, sep];
Protect @ Times;

PrintOut[___] /; Message[PrintOut::args, PrintOut] = $Failed;
PrintLine[___] /; Message[PrintLine::args, PrintLine] = $Failed;


End[];

EndPackage[];
