program demo_01_Create;
{$IFDEF FPC} {$MODE Delphi} {$ENDIF}
{$APPTYPE CONSOLE}

uses
  SysUtils, adot;

{
  General format of ADOT type construction:
    <DataType>.Create(<ContainerType> [,Field1Type [,Field2Type ...]])
  where
    <DataType> is any data-type class (TTString, TTByte, TTPointer, TTInteger, ...)
    <ContainerType> is any container class (TCVector, TCList, TCMap, TCHeap, ...)

  Examples:
    TTInteger.Create(TCSet)                         // set of integers
    TTWideString.Create(TCMap, [TTInteger]);        // map of string keys with integer values
    TTDouble.Create(TCVector, [TTDouble, TTString]) // vector of doubles with two additional fields
    TTString.Create(TCSet, [TTByte])                // set of strings with one additional field
    ...

  You don't have to remember all data-type classes. ADOT supports ALL Delphi
  built-in data types. Just write TT<type> where <type> is what you need
  (TTByte, TTString, TTInteger, TTDouble, ...)

  IMPORTANT: DO NOT CALL DESTRUCTOR FOR ADDITIONAL FIELDS. THEY WILL BE
  DESTROYED AUTOMATICALLY FROM DESTRUCTOR OF MAIN FIELD.
}

// demonstrates creating of container with one field (TCVector, TCSList,
// TCDList, TCPriorityList, TCStack, TCDeque, ...)
procedure Demo1;
var
  n : TTInteger;
  m : TTDouble;
begin
  writeln;
  writeln('Vector of integers');
  n := TTInteger.create(TCVector);
  n.add([1,2,3,7,6,5]);
  n.Println(ffDefault, true);
  n.free;

  // Some containers MUST have two fields at least: TCMap, TCMultimap,
  // TCUnsortedMap. But we can add any number of additional fields to
  // any container!
  n := TTInteger.create(TCList, [TTDouble]);
  m := n.fields[1] as TTDouble;
  n.AddRecord([1, 0.999]);
  n.AddRecord([2, 1.785]);
  n.AddRecord([3, 3.1415926]);
  m[n.Add(4)] := 4.12;
  m[n.Add(5)] := 5.07;
  writeln;
  writeln('List with two fields');
  n.println;
end;

// demonstrates creating of container with two field (TCMap, TCMultimap,
// TCUnsortedmap)
// for mapping of doubles to strings
procedure Demo2;
var
  n : TTDouble;
  s : TTString;
begin
  writeln;
  writeln('Multimap(key: double; value: string)');
  n := TTDouble.create(TCMultimap, [TTString]);
  s := n.Fields[1] as TTString;

  // set some values with .Map property
  n.Map[3.14] := 'Pi';
  n.Map[2.72] := 'E';

  // another way to set values
  // now we have full checking of types in compile-time
  s[n.Add(1.618)] := 'Golden Ration';
  s[n.Add(1.618)] := 'Golden';

  // reassign
  n.Map[3.14] := 'pi';
  s[n.Find(2.72)] := 'e';

  n.Println;
  n.free;
end;

// demonstrates syngly-linked list
procedure Demo3;
var
  n : TTString;
begin
  writeln;
  writeln('Singly-linked list of strings');
  n := TTString.create(TCSList);
  n.add(['abc','acid','aim', 'abcde','acd']);
  n.Println;
  n.free;
end;

// demonstrates vector of doubles
procedure Demo4;
var
  n : TTDouble;
begin
  writeln;
  writeln('Vector of doubles');
  n := TTDouble.create(TCVector);
  n.add([1.23,2.5,3.1415,7.62,9.11,5.45]);
  n.Println(ffDefault, true);
  n.free;
end;

// demonstrates priority queue
procedure Demo5;
var
  n : TTInteger;
  f : TTString;
  h : TCHandle;
begin
  writeln;
  writeln('Priority queue');
  n := TTInteger.create(TCHeap, [TTString]);
  f := n.Fields[1] as TTString;
  f[n.Add(5)] := 'Five';
  f[n.Add(3)] := 'Three';
  f[n.Add(7)] := 'Seven';
  f[n.Add(0)] := 'Zero';
  f[n.Add(2)] := 'Two';
  f[n.Add(1)] := 'One';
  while n.Count>0 do
  begin
    h := n.FindMin;
    writeln(n[h], '->', f[h]);
    n.RemoveMinValue;
  end;
  n.Println;
  n.free;
end;

// demonstrates some basic set operations
procedure Demo6;
var
  a,b,c : TTString;
begin
  writeln;
  writeln('Set of strings');
  a := TTString.create(TCSet);
  b := TTString.create(TCSet);
  c := TTString.create(TCSet);
  a.Add(['a','b','c','ac','bc','ab']);
  b.Add(['ab','bc','b']);

  assert(b.SetIncludes(['b','ab']));
  assert(not b.SetIncludes(['b','a','ab']));

  // set will be displayed in sorted order!
  a.Println(ffDefault, true);
  b.Println(ffDefault, true);

  write('Union: ');
  a.SetUnion(b,c);
  c.Println(ffDefault, true);

  write('Intersection: ');
  c.Clear;
  a.SetIntersection(b,c);
  c.Println(ffDefault, true);

  write('Difference A-B: ');
  c.Clear;
  a.SetDifference(b,c);
  c.Println(ffDefault, true);

  a.free;
  b.free;
  c.free;
end;

// demonstrates creating of TTList-analog from ADOT
procedure Demo7;
var
  p : TTList;
begin
  p := TTList.create; // default container is TCVector
  p.add(p);
  p.add(nil);
  p.free;
end;

begin
  Demo1;
  Demo2;
  Demo3;
  Demo4;
  Demo5;
  Demo6;
  Demo7;
  writeln;
  writeln('Press Enter to exit...');
  readln;
end.
