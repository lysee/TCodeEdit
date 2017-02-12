{-------------------------------------------------------------------------------
 codesyns.pas
 ============
 Developing Language syntaxes for TCodeEdit defined in codeedit.pas.
 Create by Li Yunjie<718956073@qq.com> on Jan 26th, 2017.
 Last modified on Feb 11th, 2017.
 Hosted at https://github.com/lysee/TCodeEdit.git.
 Released on Feb 11th, 2017 under the MIT license:
 =================================================
 Copyright (c) 2017 Li Yunjie<718956073@qq.com>.

 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all
 copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.
-------------------------------------------------------------------------------}
unit codesyns;

interface

uses
  SysUtils, Classes, codeedit;

type

  { TLiCSyntax }

  TLiCSyntax = class(TPascalSyntax)
  protected
    function ParseComment: TToken;override;
    function ParseString: TToken;override;
    function ParseChar: TToken;override;
    function ParseHex: TToken;override;
    function ParseOctal: TToken;virtual;
    function ParseBinary: TToken;virtual;
  public
    class function Language: string;override;
    class function FileExts: string;override;
    class function DefFileExt: string;override;
    class function CaseSensitive: boolean;override;
    function ParseNextToken: TToken;override;
    function IsKeyword(const ID: TCodeString): boolean;override;
  end;

  { TLiCppSyntax }

  TLiCppSyntax = class(TLiCSyntax)
  public
    class function Language: string;override;
    class function FileExts: string;override;
    class function DefFileExt: string;override;
    function IsKeyword(const ID: TCodeString): boolean;override;
  end;

  { TLiJavaSyntax }

  TLiJavaSyntax = class(TLiCSyntax)
  public
    class function Language: string;override;
    class function FileExts: string;override;
    class function DefFileExt: string;override;
    function IsKeyword(const ID: TCodeString): boolean;override;
  end;

implementation

const

  { C }

  CKeywords: array[0..43] of TCodeString = (
    'auto', 'break', 'case', 'char', 'const', 'continue', 'default', 'do',
    'double', 'else', 'enum', 'extern', 'float', 'for', 'goto', 'if',
    'inline', 'int', 'long', 'register', 'restrict', 'return', 'short',
    'signed', 'sizeof', 'static', 'struct', 'switch', 'typedef', 'union',
    'unsigned', 'void', 'volatile', 'while', '_Alignas', '_Alignof',
    '_Atomic', '_Bool', '_Complex', '_Generic', '_Imaginary', '_Noreturn',
    '_Static_assert', '_Thread_local');

  { C++ }

  CppKeywords: array[0..94] of TCodeString = (
    'asm', 'auto', 'bool', 'break', 'case', 'catch', 'cdecl', 'char', 'class',
    'const', 'const_cast', 'continue', 'default', 'delete', 'do', 'double',
    'dynamic_cast', 'else', 'enum', 'explicit', 'extern', 'false', 'float',
    'for', 'friend', 'goto', 'if', 'inline', 'int', 'interface', 'long',
    'mutable', 'namespace', 'new', 'operator', 'pascal', 'private', 'protected',
    'public', 'register', 'reinterpret_cast', 'return', 'short', 'signed',
    'sizeof', 'static', 'static_cast', 'struct', 'switch', 'template', 'this',
    'throw', 'true', 'try', 'typedef', 'typeid', 'typename', 'union',
    'unsigned', 'using', 'virtual', 'void', 'volatile', 'wchar_t', 'while',
    '__asm', '__automated', '__cdecl', '__classid', '__closure', '__declspec',
    '__dispid', '__except', '__export', '__fastcall', '__finally', '__import',
    '__int16', '__int32', '__int64', '__int8', '__pascal', '__property',
    '__published', '__rtti', '__stdcall', '__thread', '__try', '_asm', '_cdecl',
    '_export', '_fastcall', '_import', '_pascal', '_stdcall');

  { Java }

  JavaKeywords: array[0..52] of TCodeString = (
    'abstract', 'assert', 'boolean', 'break', 'byte', 'case', 'catch', 'char',
    'class', 'const', 'continue', 'default', 'do', 'double', 'else', 'enum',
    'extends', 'false', 'final', 'finally', 'float', 'for', 'goto', 'if',
    'implements', 'import', 'instanceof', 'int', 'interface', 'long', 'native',
    'new', 'null', 'package', 'private', 'protected', 'public', 'return',
    'short', 'static', 'strictfp', 'super', 'switch', 'synchronized', 'this',
    'throw', 'throws', 'transient', 'true', 'try', 'void', 'volatile', 'while');

{ TLiCSyntax }

function TLiCSyntax.ParseString: TToken;
begin
  while GetNextChar and not MatchChar('"') do
    if MatchChar('\') then GetNextChar;
  if MatchChar('"') then
  begin
    GetNextChar;
    Result := TK_STRING;
  end
  else Result := TK_UNKNOWN;
end;

function TLiCSyntax.ParseBinary: TToken;
begin
  GetNextChar;
  if SkipNextChars(['0'..'1']) = 0 then
    Result := TK_UNKNOWN else
    Result := TK_NUMBER;
  if SkipChars(CS_ID) > 0 then
    Result := TK_UNKNOWN;
end;

function TLiCSyntax.ParseChar: TToken;
begin
  while GetNextChar and not MatchChar('''') do
    if MatchChar('\') then GetNextChar;
  if MatchChar('''') then
  begin
    GetNextChar;
    Result := TK_CHAR;
  end
  else Result := TK_UNKNOWN;
end;

function TLiCSyntax.ParseComment: TToken;
begin
  Result := ParseBlockComment('*/', TK_ECOMMENT);
end;

function TLiCSyntax.ParseHex: TToken;
begin
  GetNextChar;
  Result := inherited ParseHex;
end;

function TLiCSyntax.ParseNextToken: TToken;
begin
  if MatchPrior(TK_ECOMMENT) then Result := ParseComment else
  if MatchStr('/*')          then Result := ParseComment else
  if MatchStr('//')          then Result := ParseLineComment else
  if MatchChar(CS_SPACE)     then Result := ParseSpace else
  if MatchChar(CS_HEAD)      then Result := ParseKeywordID else
  if MatchChar('"')          then Result := ParseString else
  if MatchChar('''')         then Result := ParseChar else
  if MatchText('0x')         then Result := ParseHex else
  if MatchText('0o')         then Result := ParseOctal else
  if MatchText('0b')         then Result := ParseBinary else
  if MatchChar(CS_DIGIT)     then Result := ParseNumber else
  if MatchChar(CS_OPERATOR)  then Result := ParseOperator else
  if MatchChar(CS_DELIMITER) then Result := ParseDelimiter else
                                  Result := ParseUnknown;
end;

function TLiCSyntax.ParseOctal: TToken;
begin
  GetNextChar;
  if SkipNextChars(['0'..'7']) = 0 then
    Result := TK_UNKNOWN else
    Result := TK_NUMBER;
  if SkipChars(CS_ID) > 0 then
    Result := TK_UNKNOWN;
end;

class function TLiCSyntax.Language: string;
begin
  Result := 'C';
end;

class function TLiCSyntax.FileExts: string;
begin
  Result := '.c .h';
end;

class function TLiCSyntax.DefFileExt: string;
begin
  Result := '.c';
end;

class function TLiCSyntax.CaseSensitive: boolean;
begin
  Result := true;
end;

function TLiCSyntax.IsKeyword(const ID: TCodeString): boolean;
begin
  Result := MatchID(ID, CKeywords);
end;

{ TLiCppSyntax }

class function TLiCppSyntax.Language: string;
begin
  Result := 'C++';
end;

class function TLiCppSyntax.FileExts: string;
begin
  Result := '.c .h .cpp .hpp';
end;

class function TLiCppSyntax.DefFileExt: string;
begin
  Result := '.cpp';
end;

function TLiCppSyntax.IsKeyword(const ID: TCodeString): boolean;
begin
  Result := MatchID(ID, CppKeywords);
end;

{ TLiJavaSyntax }

class function TLiJavaSyntax.Language: string;
begin
  Result := 'Java';
end;

class function TLiJavaSyntax.FileExts: string;
begin
  Result := '.java';
end;

class function TLiJavaSyntax.DefFileExt: string;
begin
  Result := '.java';
end;

function TLiJavaSyntax.IsKeyword(const ID: TCodeString): boolean;
begin
  Result := MatchID(ID, JavaKeywords);
end;

initialization
begin
  SyntaxClasses.Add(TLiJavaSyntax);
  SyntaxClasses.Add(TLiCppSyntax);
  SyntaxClasses.Add(TLiCSyntax);
end;

end.
