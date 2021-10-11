program demo_04_Sort;
{$IFDEF FPC} {$MODE Delphi} {$ENDIF}
{$APPTYPE CONSOLE}

uses
  SysUtils, adot;

function Desc(a,b: integer):integer;
begin
  if a<b then
    result := 1
  else
    if a=b then
      result := 0
    else
      result := -1;
end;

procedure Demo1;
var
  n : TTInteger;
begin
  writeln;
  writeln('Vector of integers');
  n := TTInteger.create(TCVector);
  n.add([1,2,3,7,6,5]);
  n.Println(ffDefault, true);

  writeln('regular sort');
  n.Sort;
  n.Println(ffDefault, true);

  writeln('descendant sort');
  n.Sort(Desc);
  n.Println(ffDefault, true);

  writeln('sort first 3 items');
  n.Sort(0,2);
  n.Println(ffDefault, true);
  n.free;
end;

procedure Demo2;
var
  n : TTDouble;
begin
  writeln;
  writeln('Vector of doubles');
  n := TTDouble.create(TCVector);
  n.add([1.23,2.5,3.1415,7.62,9.11,5.45]);
  n.Println(ffDefault, true);
  n.Sort;
  n.Println(ffDefault, true);
  n.free;
end;

procedure Demo3;
var
  n : TTString;
begin
  writeln;
  writeln('Singly-linked list of strings');
  n := TTString.create(TCSList);
  n.add(['abc','acid','aim', 'abcde','acd']);
  n.Println(ffDefault, true);

  writeln;
  writeln('lexicographically ordered:');
  n.Sort;
  n.Println(ffDefault, true);

  writeln;
  writeln('simply ordered:');
  n.Lexicographic := false;
  n.Sort;
  n.Println(ffDefault, true);

  n.free;
end;

begin
  Demo1;
  Demo2;
  Demo3;
  writeln;
  writeln('Press Enter to exit...');
  readln;
end.
