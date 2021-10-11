program demo_05_StringTokens;
{$IFDEF FPC} {$MODE Delphi} {$ENDIF}
{$APPTYPE CONSOLE}

uses
  SysUtils, adot;

// demonstrates parsing of tokens from string
procedure Demo1;
var
  s : string;
  h : TStrToken;
  i,j: integer;
begin
  // extract english words from string
  s := 'A small box used by the ancient Greeks and Romans to hold medicines';
  writeln('Source string: ', s);
  writeln('Tokens:');
  STokOpen(s, ['a'..'z','A'..'Z'], h);
  while STokRead(h)=0 do
    writeln(h.Token);

  // extract int numbers from string
  s := '1;-2, +3 5 _7_ ~11';
  writeln;
  writeln('Source string: ', s);
  writeln('Tokens:');
  STokOpen(s, ['0'..'9','+','-'], h);
  while STokRead(h)=0 do
    writeln(h.Token);

  // extract floating numbers
  s := '1e3;2.5, 3.1415926';
  writeln;
  writeln('Source string: ', s);
  writeln('Tokens:');
  j := STokCountFloat(s);
  STokOpenFloat(s, h);
  for i := 0 to j-1 do
    writeln( format('%.3f', [STokReadFloat(h)]) );
end;

begin
  Demo1;
  writeln;
  writeln('Press Enter to exit...');
  readln;
end.
