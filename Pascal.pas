program CaesarCipher;

{$mode objfpc}{$H+}

uses
  SysUtils;

function ToUpper(str: string): string;
var
  i: Integer;
begin
  Result := str;
  for i := 1 to Length(str) do
    Result[i] := UpCase(str[i]);
end;

function Encrypt(str: string; shiftAmount: Integer): string;
var
  i: Integer;
  c: Char;
begin
  str := ToUpper(str);
  for i := 1 to Length(str) do
  begin
    if str[i] in ['A'..'Z'] then
    begin
      c := Chr(Ord('A') + (Ord(str[i]) - Ord('A') + shiftAmount) mod 26);
      str[i] := c;
    end;
  end;
  Result := str;
end;

function Decrypt(str: string; shiftAmount: Integer): string;
var
  i: Integer;
  c: Char;
begin
  str := ToUpper(str);
  for i := 1 to Length(str) do
  begin
    if str[i] in ['A'..'Z'] then
    begin
      c := Chr(Ord('A') + (Ord(str[i]) - Ord('A') - shiftAmount + 26) mod 26);
      str[i] := c;
    end;
  end;
  Result := str;
end;

procedure Solve(str: string; maxShiftValue: Integer);
var
  i, shift: Integer;
begin
  str := ToUpper(str);
  for shift := 1 to maxShiftValue do
    writeln('Caesar ', shift, ': ', Decrypt(str, shift));
end;

var
  originalText, encryptedText, decryptedText: string;
begin
  originalText := 'MINIME PLEASE DONT ENCRYPT ME';
  encryptedText := Encrypt(originalText, 3);
  writeln('Encrypted Text: ', encryptedText);

  decryptedText := Decrypt(encryptedText, 3);
  writeln('Decrypted Text: ', decryptedText);

  writeln('Solving Cipher...');
  Solve('MINIME PLEASE DONT ENCRYPT ME', 26);
end.
