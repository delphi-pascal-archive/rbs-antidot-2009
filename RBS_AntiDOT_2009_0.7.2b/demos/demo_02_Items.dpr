program demo_02_Items;
{$IFDEF FPC} {$MODE Delphi} {$ENDIF}
{$APPTYPE CONSOLE}

uses
  SysUtils, adot;

procedure Demo1;
var
  n: TTInteger;
  h: TCHandle;
  i,j: integer;
begin
  writeln;
  writeln('Vector of integers');
  n := TTInteger.create(TCVector);
  n.add([1,2,3,7,6,5]);

  // correct way to access items for any container
  h := n.First;
  for i:=0 to n.Count-1 do
  begin
    write(n[h], ' ');
    n.Next(h);
  end;
  writeln;

  // another way
  h := n.First;
  while h<>-1 do
  begin
    write(n[h], ' ');
    n.Next(h);
  end;
  writeln;

  // one more way
  h := n.First;
  while h<>-1 do
    write(n[n.MoveNext(h)], ' ');
  writeln;

  // accessing items by index (correct only for TCVector)
  for i := 0 to n.Count-1 do
    write(n[i], ' ');
  writeln;

  // accessing items by index (correct for all containers)
  for i := 0 to n.Count-1 do
    write(n[n.handles[i]], ' ');
  writeln;

  // find sum
  j := 0;
  for i := 0 to n.Count-1 do
    inc(j, n[n.handles[i]]);
  writeln('sum: ', j);

  n.free;
end;

procedure Demo2;
var
  n: TTInteger;
  i: integer;
begin
  writeln;
  writeln('Stack of integers');
  n := TTInteger.create(TCStack);
  for i := 0 to 6 do
    n.push(i);
  for i := 0 to n.count-1 do
    writeln(n.pop);
  n.free;
end;

begin
  Demo1;
  Demo2;
  writeln;
  writeln('Press Enter to exit...');
  readln;
end.
