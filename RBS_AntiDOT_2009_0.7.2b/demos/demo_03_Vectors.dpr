program demo_03_Vectors;
{$IFDEF FPC} {$MODE Delphi} {$ENDIF}
{$APPTYPE CONSOLE}

uses
  SysUtils, Adot;

procedure TestArrayOfPointers;
var
  p: TTPointer;
  i: integer;
  sum, n: double;
begin
  writeln('test vector of Pointer');
  p := TTPointer.create(TCVector);

  // alloc memory for items
  for i := 0 to 999 do
    p.Add(allocmem(SizeOF(double)));

  // fill allocated memory with random values and find SUM
  sum := 0;
  for i := 0 to p.count-1 do
  begin
    n := random*1000;
    sum := sum + n;
    double(p[i]^) := n;
  end;

  // check sum
  n := 0;
  for i := 0 to p.count-1 do
    n := n + double(p[i]^);
  assert(n=sum);

  // free allocated memory
  for i := 0 to p.count-1 do
    FreeMem(p[i]);

  p.free;
  writeln('ok');
end;

procedure TestArrayOfDoubles;
var
  p: TTDouble;
  i: integer;
begin
  writeln('test vector of Double');
  p := TTDouble.create(TCVector);

  for i := 0 to 19 do
    p.add(random*1000);
  writeln('unsorted:');
  for i := 0 to p.count-1 do
    writeln(format('%.3f', [p[i]]));
  p.Sort;
  writeln('sorted:');
  for i := 0 to p.count-1 do
    writeln(format('%.3f^2 = %.3f', [p[i], p[i]*p[i]]));

  p.free;
  writeln('ok');
end;

procedure TestArrayOfStrings;
var
  p: TTString;
  i: TCHandle;
begin
  writeln('test vector of String');
  p := TTString.create(TCVector);
  p.add(['it', 'is', 'fair', 'game']);
  writeln;
  writeln('Text:');
  writeln(p.Text);
  writeln;
  writeln('TextLine:');
  writeln(p.TextLine);

  writeln;
  writeln('Unsorted items:');

  // traverse container of any kind
  i := p.First;
  while i<>-1 do
  begin
    writeln(p[i]);
    p.Next(i);
  end;

  p.sort;
  writeln;
  writeln('Sorted items (Lexicographic=TRUE):');

  // another aproach to traverse container
  i := p.First;
  while i<>-1 do
    writeln(p[p.MoveNext(i)]);

  p.Lexicographic := false;
  p.sort;
  writeln;
  writeln('Sorted items (Lexicographic=FALSE):');

  // accessing items by direct index
  for i := 0 to p.count-1 do
    writeln(p[i]);

  writeln('ok');
  p.free;
end;

procedure main;
begin
  TestArrayOfPointers;
  TestArrayOfDoubles;
  TestArrayOfStrings;
end;

begin
  main;
  writeln('press Enter to exit...');
  readln;
end.
