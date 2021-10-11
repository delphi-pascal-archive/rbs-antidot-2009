program demo_06_Trees;
{$IFDEF FPC} {$MODE Delphi} {$ENDIF}
{$APPTYPE CONSOLE}

uses
  SysUtils, Adot;

procedure Demo;
var
  t: TTString;
  i,j: TCHandle;
begin
  t := TTString.create(TCTree);
  i := t.Add('item 1');
  t.AddChild(i, 'item 1.1');
  t.AddChild(i, 'item 1.2');
  t.AddChild(i, 'item 1.3');
  i := t.Add('item 2');
  t.AddChild(i, 'item 2.1');
  j := t.AddChild(i, 'item 2.2');
  t.AddChild(j, 'item 2.2.1');
  t.AddChild(i, 'item 2.3');
  t.Println;
  writeln;

  writeln('Move 2.2 -> 1.2');
  t.Move(t.Find('item 2.2'), t.Find('item 1.2'));
  t.Println;
  writeln;

  writeln('ChangeParent 2 -> 1.2');
  t.ChangeParent(t.Find('item 2'), t.Find('item 1.2'));
  t.Println;
  writeln;

  t.free;
end;

begin
  Demo;
  writeln('press Enter to exit...');
  readln;
end.
