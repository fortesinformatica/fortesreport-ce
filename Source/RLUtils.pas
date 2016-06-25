{ Projeto: FortesReport Community Edition                                      }
{ É um poderoso gerador de relatórios disponível como um pacote de componentes }
{ para Delphi. Em FortesReport, os relatórios são constituídos por bandas que  }
{ têm funções específicas no fluxo de impressão. Você definir agrupamentos     }
{ subníveis e totais simplesmente pela relação hierárquica entre as bandas.    }
{ Além disso possui uma rica paleta de Componentes                             }
{                                                                              }
{ Direitos Autorais Reservados(c) Copyright © 1999-2015 Fortes Informática     }
{                                                                              }
{ Colaboradores nesse arquivo: Ronaldo Moreira                                 }
{                              Márcio Martins                                  }
{                              Régys Borges da Silveira                        }
{                              Juliomar Marchetti                              }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto          }
{  localizado em                                                               }
{ https://github.com/fortesinformatica/fortesreport-ce                         }
{                                                                              }
{  Para mais informações você pode consultar o site www.fortesreport.com.br ou }
{  no Yahoo Groups https://groups.yahoo.com/neo/groups/fortesreport/info       }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* xx/xx/xxxx:  Autor...
|* - Descrição...
******************************************************************************}

{$I RLReport.inc}

{$IfDef FPC}
 {$IfNDef VER3}
  {$Define NO_CHARINSET}
 {$EndIf}
{$Else}
 {$IfNDef DELPHI12_UP}
  {$Define NO_CHARINSET}
 {$EndIf}
{$EndIf}

{@unit RLUtils - Rotinas de uso geral. }
unit RLUtils;

interface

uses
  {$IfDef MSWINDOWS}
   Windows,
  {$EndIf}
  SysUtils, Classes, DB,
  {$IfDef FPC}
   FileUtil, LazUTF8, LConvEncoding,
  {$EndIf}
  {$IfDef CLX}
   QTypes, QGraphics, QForms,
  {$Else}
   Types, Graphics, Forms,
  {$EndIf}
  Math;

{@var TempDir - Especifica aonde deverão ser criados os arquivos temporários.
 Na inicialização do sistema é atribuido um valor padrão a esta variável. Este valor pode ser alterado depois.
 No Windows o diretório padrão é "WINDOWS\TEMP", e no Linux é o "/tmp".
 @links GetTempFileName. :/}
var TempDir: String = '.';

{@proc FreeObj - Libera objeto se não for nil e em seguida limpa a variável.
 @links FreePtr. :/}
procedure FreeObj(var AObj);

{@proc FreePtr - Libera ponteiro se não for nil e em seguida limpa a variável.
 @links FreeObj. :/}
procedure FreePtr(var APtr);

{@func ByteToHex - Retorna o byte em notação hexadecimal de dois dígitos.
 @links HexToByte. :/}
function ByteToHex(const AByte: Byte): AnsiString;

{@func HexToByte - Retorna o valor hexadecimal como byte.
 @links ByteToHex. :/}
function HexToByte(const AHex: AnsiString): Byte;

{@func HexToBitmap - Cria bitmap a partir de uma cadeia hexadecimal.
 @links HexToGraphic, HexToByte. :/}
function HexToBitmap(const AHex: AnsiString): TBitmap;

{@func HexToGraphic - Cria um gráfico qualquer a partir de uma cadeia hexadecimal.
 @links HexToBitmap, HexToByte. :/}
function HexToGraphic(const AHex: AnsiString): TGraphic;

{@func NewComponentName - Cria um nome para um novo componente. :/}
function NewComponentName(AComponent: TComponent): String;

{@func GetTempFileName - Retorna nome de arquivo temporário.
 @links TempDir. :/}
function GetTempFileName: String;

function TokenListChanged(const ATokenList: String; ATokenNo: Integer;
  const ANewTokenValue: string; ATokenSeparator: Char = '|'): String;

{@func Token - Retorna a parte de número aIndex da string aTokenList cujas partes são separadas pelo caractere aTokenSeparator. :/}
function Token(const ATokenList: String; AIndex: Integer; ATokenSeparator: char = '|'): String;

{@func ThreadIt - Executa um método ou procedure em segundo plano. :}
function ThreadIt(AMethod: TThreadMethod; ALoop: Boolean = False): TThread; overload;
function ThreadIt(AProc: TProcedure; ALoop: Boolean = False): TThread; overload;
{/@func}

{@func FormatFileExt - Adiciona ponto a uma extensão, se não houver. :/}
function FormatFileExt(const AExt: String): String;

{@func AddFileFilter - Adiciona filtro de arquivos com nome aFilter, descrição aDescription e extensão padrão aExt. :/}
function AddFileFilter(const AFilter: String; const ADescription, AExt: String): String;

{@func GetFileFilterExt - Devolve a extensão padrão para arquivos correspondentes ao filtro aFilter. :/}
function GetFileFilterExt(const AFilter: String; AIndex: Integer): String;

{@func RotatePoints - Rotaciona os pontos aPoints em 2D de acordo com o ângulo aAngle.
 @links RotateBitmap. :/}
procedure RotatePoints(var APoints: array of TPoint; const AAngle: Double);

{@func RotateBitmap - Rotaciona o bitmap TBitmap em 2D de acordo com o ângulo aAngle e devolve em aDest.
 Nota: O bitmap aDest deve ter tamanho suficiente para a imagem rotacionada. Este cálculo pode ser feito
 previamente com a proc RotatePoints.
 @links RotatePoints, RotatedBitmap. :/}
procedure RotateBitmap(ASource, ADest: TBitmap; AAngle: Double; AAxis, AOffset: TPoint);

{@func RotatedBitmap - Cria e devolve um bitmap compatível com o bitmap aSource rotacionado em 2D de acordo com o ângulo aAngle com
 tamanho calculado.
 @links RotateBitmap. :/}
function RotatedBitmap(ASource: TBitmap; AAngle: Double): TBitmap;

{@func PointsRect - Retorna um retângulo delimitando a área definida pelos pontos aPoints.
 @links PointsSize. :/}
function PointsRect(const APoints: array of TPoint): TRect;

{@func PointsSize - Retorna o tamanho da área definida pelos pontos aPoints.
 @links PointsRect. :/}
function PointsSize(const APoints: array of TPoint): TPoint;

{@func ScalePoints - Modifica as dimensões dos pontos aPoints para que caibam no retângulo definido por aRect respeitando a proporção.
 @links PointsRect. :/}
procedure ScalePoints(var APoints: array of TPoint; const ARect: TRect);

{@func StretchPoints - Amplia ou reduz as dimensões dos pontos aPoints para que caibam no retângulo definido por aRect.
 @links PointsRect. :/}
procedure StretchPoints(var APoints: array of TPoint; const ARect: TRect);

{@func CenterPoints - Centraliza os pontos aPoints no retâgulo aRect.
 @links PointsRect. :/}
procedure CenterPoints(var APoints: array of TPoint; const ARect: TRect);

{@func TextBounds - Calcula as dimensões do texto aText de acordo com a fonte aFont e opcionalmente rotacionado em
 2D de acordo com o ângulo aAngle.
 @links PointsRect. :/}
function TextBounds(const AText: string; AFont: TFont; AAngle: Double): TPoint;

{@proc MoveRect - Desloca o retângulo horizontalmente de acordo com aX e verticalmente de acordo com aY.
 Nota: Valores positivos deslocam o retângulo para a direita ou abaixo. :/}
procedure MoveRect(var ARect: TRect; AX, AY: Integer);

{@func RectWidth - Retorna a largura do retângulo aRect.
 @links RectHeight. :/}
function RectWidth(const ARect: TRect): Integer;

{@func RectHeight - Retorna a largura do retângulo aRect.
 @links RectWidth. :/}
function RectHeight(const ARect: TRect): Integer;

{@func ReduceRect - Retorna o retângulo aRect reduzido de acordo com os decrementos especificados em aPixels. :/}
function ReduceRect(const ARect: TRect; APixels: TRect): TRect;

{@func IncreaseRect - Retorna o retângulo aRect ampliado de acordo com os incrementos especificados em aPixels. :/}
function IncreaseRect(const ARect: TRect; APixels: TRect): TRect;

{@func DiffRect - Retorna a diferença entre os retângulos aRectOut e aRectIn, desde que aRectIn esteja dentro
 de aRectOut. :/}
function DiffRect(const ARectOut, ARectIn: TRect): TRect;

{@func IterateJustification - Faz a justificação do texto distribuindo espaços. A função deve ser executada até
 se obter a largura total do texto. :/}
function IterateJustification(var AText: AnsiString; var AIndex: Integer): Boolean;

{@func ScaleRect - Calcula a maior amostra do retângulo aSource escalonado de modo a caber em aTarget. :/}
function ScaleRect(const ASource, ATarget: TRect; ACenter: Boolean): TRect;

procedure StreamWrite(AStream: TStream; const AStr: string);
procedure StreamWriteLn(AStream: TStream; const AStr: string = '');

{@proc RegisterTempFile - Registra um arquivo temporário para ser excluído na finalização. :/}
procedure RegisterTempFile(const AFileName: String);
{@proc UnregisterTempFile - Retira arquivo temporário da lista de arquivos a excluir na finalizacão. :/}
procedure UnregisterTempFile(const AFileName: String);
{@proc ClearTempFiles - Destroi arquivos temporários registrados pela proc RegisterTempFile. :/}
procedure ClearTempFiles;

{@proc SmartGetFieldDisplayText - Retorna a verdadeira intenção do texto de exibição do valor do campo. :/}
function SmartGetFieldDisplayText(Field: TField; const Mask: string = ''): String;

var
  LogFileName: String = 'rlib.log';

procedure LogClear;
procedure Log(const AMsg: String);

type
{$IfNDef MSWINDOWS}
  TRGBQuad = packed record
    rgbBlue: Byte;
    rgbGreen: Byte;
    rgbRed: Byte;
    rgbReserved: Byte;
  end;
{$endif}
  TRGBArray = array[0..0] of TRGBQuad;
  PRGBArray = ^TRGBArray;

{$IfNDef MSWINDOWS}
function RGB(R, G, B: Byte): TColor;
{$endif}

function NeedAuxBitmap: TBitmap;
function NewBitmap: TBitmap; overload;
function NewBitmap(Width, Height: Integer): TBitmap; overload;
{$IfDef NO_CHARINSET}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; overload;
function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean; overload;
{$EndIf}

function GetLocalizeStr(AString: AnsiString): String;
function GetAnsiStr(AString: String): AnsiString;

{/@unit}

type
  TRLBitmap = class(TBitmap)
  end;

implementation

{$IfDef NO_CHARINSET}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;

function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := (C < #$0100) and (AnsiChar(C) in CharSet);
end;
{$EndIf}

function GetLocalizeStr(AString: AnsiString): String;
begin
  {$IfDef FPC}
   Result := CP1252ToUTF8( AString );  // Fortes Report sources uses CP1252
  {$Else}
   Result := String(AString);
  {$EndIf}
end;

function GetAnsiStr(AString: String): AnsiString;
begin
  {$IfDef FPC}
   Result := UTF8ToCP1252( AString ) ;   // Fortes Report sources uses CP1252
  {$Else}
   {$IFDEF UNICODE}
    Result := AnsiString(AString);
   {$Else}
    Result := AString;
   {$EndIf}
  {$EndIf}
end;

function NewBitmap: TBitmap;
begin
  Result := NewBitmap(1, 1);
end;

function NewBitmap(Width, Height: Integer): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Width := Width;
  Result.Height := Height;
  Result.PixelFormat := pf32bit;
end;

var
  AuxBitmap: TBitmap;

function NeedAuxBitmap: TBitmap;
begin
  if AuxBitmap = nil then
  begin
    AuxBitmap := TRLBitmap.Create;
    AuxBitmap.Width := 1;
    AuxBitmap.Height := 1;
  end;
  Result := AuxBitmap;
end;

procedure LogClear;
begin
  if FileExists(LogFileName) then
    SysUtils.DeleteFile(LogFileName);
end;

procedure Log(const AMsg: String);
var
  loghandle: textfile;
begin
  AssignFile(loghandle, LogFileName);
  if FileExists(LogFileName) then
    Append(loghandle)
  else
    Rewrite(loghandle);
  Writeln(loghandle, TimeToStr(Time) + ': ' + AMsg);
  CloseFile(loghandle);
end;

type
  dw = record
    H, L: Word;
  end;

const
  //HEXDIGITS: String[16] = '0123456789ABCDEF';
  HEXDIGITS: AnsiString = '0123456789ABCDEF';

function ByteToHex(const AByte: Byte): AnsiString;
begin
  Result := HEXDIGITS[(AByte and $f0) shr 4 + 1] + HEXDIGITS[(AByte and $0f) + 1];
end;

function HexToByte(const AHex: AnsiString): Byte;
begin
  Result := (Pos(UpCase(AHex[1]), HEXDIGITS) - 1) * 16 + Pos(UpCase(AHex[2]), HEXDIGITS) - 1;
end;

procedure FreeObj(var AObj);
begin
  if Assigned(TObject(AObj)) then
    TObject(AObj).free;
  TObject(AObj) := nil;
end;

procedure FreePtr(var APtr);
begin
  if Assigned(pointer(APtr)) then
    FreeMem(pointer(APtr));
  pointer(APtr) := nil;
end;

{$IfNDef MSWINDOWS}
function RGB(R, G, B: Byte): TColor;
begin
  Result := (R or (G shl 8) or (B shl 16));
end;
{$endif}

type
  TPublicGraphic = class(TGraphic)
  end;

function HexToBitmap(const AHex: AnsiString): TBitmap;
var
  stream: TStringStream;
  I, L: Integer;
begin
  stream := TStringStream.Create('');
  try
    // traduz string hex em binária
    L := Length(AHex);
    I := 1;
    while I < L do
    begin
      stream.WriteString(AnsiChar(HexToByte(AHex[I] + AHex[I + 1])));
      Inc(I, 2);
    end;
    // procura referência para a classe
    Result := NeedAuxBitmap;
    stream.Seek(0, 0);
    TPublicGraphic(Result).ReadData(stream);
  finally
    FreeObj(stream);
  end;
end;

function HexToGraphic(const AHex: AnsiString): TGraphic;
var
  graphclassname: string;
  graphclass: TGraphicClass;
  stream: TStringStream;
  I, L: Integer;
  Size: Byte;
begin
  Result := nil;
  stream := TStringStream.Create('');
  try
    // traduz string hex em binária
    L := Length(AHex);
    I := 1;
    while I < L do
    begin
      stream.WriteString(AnsiChar(HexToByte(AHex[I] + AHex[I + 1])));
      Inc(I, 2);
    end;
    // pega o nome da classe
    stream.Seek(0, 0);
    stream.Read(Size, 1);
    SetLength(graphclassname, Size);
    stream.Read(graphclassname[1], Size);
    // procura referência para a classe
    graphclassname := UpperCase(graphclassname);
    if graphclassname = 'TBITMAP' then
      graphclass := TBitmap
    else if graphclassname = 'TICON' then
      graphclass := TIcon
    else
      graphclass := nil;
    // instancia e carrega o grafico
    if graphclass <> nil then
    begin
      Result := graphclass.Create;
      try
        TPublicGraphic(Result).ReadData(stream);
      except
        FreeObj(Result);
        raise;
      end;
    end;
  finally
    FreeObj(stream);
  end;
end;

// diretório temporário
function GetTempDir: String;
{$ifndef LINUX}
var
  P: array[0..255] of char;
  H: String;
{$endif}
begin
{$ifndef LINUX}
  GetDir(0, H); // salva diretório atual
  GetWindowsDirectory(@P, 256); // diretório do windows
  ChDir(strpas(P));
  try
    GetTempPath(256, @P);
    Result := strpas(P);
  finally
    ChDir(H);
  end;
{$else}
  TempDir := '/tmp';
{$endif}
end;

function NewComponentName(AComponent: TComponent): String;
var
  P, N: String;
  I, M: Integer;
begin
  P := AComponent.ClassName;
  if UpperCase(P[1]) = 'T' then
    Delete(P, 1, 1);
  M := 0;
  for I := 0 to AComponent.Owner.ComponentCount - 1 do
  begin
    N := AComponent.Owner.Components[I].Name;
    if AnsiSameText(Copy(N, 1, Length(P)), P) then
      M := Max(M, StrToIntDef(Copy(N, Length(P) + 1, Length(N)), 0));
  end;
  Result := P + IntToStr(M + 1);
end;

function GetTempFileName: String;
var
  tmppath: String;
begin
  Randomize;
  tmppath := TempDir;
  if tmppath <> '' then
    tmppath := IncludeTrailingPathDelimiter(tmppath);
  repeat
    Result := tmppath + '~fr' + IntToStr(Random(MaxInt)) + '.tmp';
  until not FileExists(Result);
end;

function TokenListChanged(const ATokenList: String; ATokenNo: Integer;
  const ANewTokenValue: string; ATokenSeparator: Char = '|'): String;
var
  I, P, ReadNo: Integer;
begin
  Result := ATokenList;
  ReadNo := 1;
  I := 1;
  while I <= Length(Result) do
  begin
    P := I;
    while (I <= Length(Result)) and (Result[I] <> ATokenSeparator) do
      Inc(I);
    if ReadNo = ATokenNo then
    begin
      Delete(Result, P, I - P);
      Insert(ANewTokenValue, Result, P);
      Exit;
    end;
    if I > Length(Result) then
      Break;
    Inc(I);
    Inc(ReadNo);
  end;
  while ReadNo < ATokenNo do
  begin
    Result := Result + '|';
    Inc(ReadNo);
  end;
  Result := Result + ANewTokenValue;
end;

function Token(const ATokenList: String; AIndex: Integer; ATokenSeparator: char = '|'): String;
var
  I, M, count: Integer;
begin
  Result := '';
  count := 0;
  I := 1;
  while I <= Length(ATokenList) do
  begin
    M := I;
    while (I <= Length(ATokenList)) and (ATokenList[I] <> ATokenSeparator) do
      Inc(I);
    Inc(count);
    if count = AIndex then
    begin
      Result := Copy(ATokenList, M, I - M);
      Break;
    end;
    Inc(I);
  end;
end;

type
  TInternalThread = class(TThread)
  protected
    FMethod: TThreadMethod;
    FProc: TProcedure;
    FLoop: Boolean;
    //
    procedure Execute; override;
    //
    procedure Call;
  public
    constructor Create(AMethod: TThreadMethod; ALoop: Boolean); overload;
    constructor Create(AProc: TProcedure; ALoop: Boolean); overload;
  end;

constructor TInternalThread.Create(AMethod: TThreadMethod; ALoop: Boolean);
begin
  FreeOnTerminate := True;
  FMethod := AMethod;
  FProc := nil;
  FLoop := ALoop;
  //
  inherited Create(False);
end;

constructor TInternalThread.Create(AProc: TProcedure; ALoop: Boolean);
begin
  FreeOnTerminate := True;
  FMethod := nil;
  FProc := AProc;
  FLoop := ALoop;
  //
  inherited Create(False);
end;

procedure TInternalThread.Call;
begin
  while FLoop and not Terminated do
  begin
    if Assigned(@FProc) then
      FProc;
    if Assigned(@FMethod) then
      FMethod;
  end;
end;

procedure TInternalThread.Execute;
begin
  Synchronize(Call);
end;

function ThreadIt(AMethod: TThreadMethod; ALoop: Boolean = False): TThread;
begin
  Result := TInternalThread.Create(AMethod, ALoop);
end;

function ThreadIt(AProc: TProcedure; ALoop: Boolean = False): TThread;
begin
  Result := TInternalThread.Create(AProc, ALoop);
end;

function FormatFileExt(const AExt: String): String;
begin
  if (AExt <> '') and (AExt[1] <> '.') then
    Result := '.' + AExt
  else
    Result := AExt;
end;

function AddFileFilter(const AFilter: String; const ADescription, AExt: String): String;
begin
  Result := AFilter;
  if Result <> '' then
    Result := Result + '|';
  Result := Result + ADescription + ' (*' + FormatFileExt(AExt) + ')';

  Result := Result + '|*' + FormatFileExt(AExt);
end;

function GetFileFilterExt(const AFilter: String; AIndex: Integer): String;
var
  P, I: Integer;
  M: String;
begin
  if AIndex = 0 then
    AIndex := 1;
  I := 1;
  while I <= AIndex do
  begin
    M := Token(AFilter, I, '|');
    P := Pos('(', M);
    if P > 0 then
      Delete(M, 1, P);
    P := Pos(')', M);
    if P > 0 then
      M := Copy(M, 1, P - 1);
    Inc(I);
  end;
  P := Pos('.', M);
  if P > 0 then
    Delete(M, 1, P);
  Result := FormatFileExt(M);
end;

procedure RotatePoints(var APoints: array of TPoint; const AAngle: Double);
var
  theta: Double;
  costheta: Double;
  sintheta: Double;
  center: TPoint;
  I, Q: Integer;
procedure RotatePoint(var APoint: TPoint);
var
  saved: TPoint;
begin
  saved := APoint;
  APoint.X := Round(saved.X * costheta - saved.Y * sintheta);
  APoint.Y := Round(saved.X * sintheta + saved.Y * costheta);
end;
begin
  theta := -AAngle * pi / 180; // radians
  sintheta := Sin(theta);
  costheta := Cos(theta);
  // calcula centro
  center.X := 0;
  center.Y := 0;
  Q := High(APoints) + 1;
  for I := 0 to Q - 1 do
  begin
    Inc(center.X, APoints[I].X);
    Inc(center.Y, APoints[I].Y);
  end;
  center.X := Round(center.X / Q);
  center.Y := Round(center.Y / Q);
  // roda
  for I := 0 to Q - 1 do
  begin
    Dec(APoints[I].X, center.X);
    Dec(APoints[I].Y, center.Y);
    RotatePoint(APoints[I]);
    Inc(APoints[I].X, center.X);
    Inc(APoints[I].Y, center.Y);
  end;
end;

procedure RotateBitmap(ASource, ADest: TBitmap; AAngle: Double; AAxis, AOffset: TPoint);
type
{$ifdef CLX}
  TRGBQuad = packed record
    rgbBlue: Byte;
    rgbGreen: Byte;
    rgbRed: Byte;
    rgbReserved: Byte;
  end;
{$endif}
  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..0] of TRGBQuad;
var
  I: Integer;
  iDest: Integer;
  iOriginal: Integer;
  iPrime: Integer;
  iPrimeRotated: Integer;
  //
  J: Integer;
  jDest: Integer;
  jOriginal: Integer;
  jPrime: Integer;
  jPrimeRotated: Integer;
  //
  RowSource: PRGBArray;
  RowDest: PRGBArray;
  //
  Radians: Double;
  RadiansCos: Double;
  RadiansSin: Double;
begin
  // Convert degrees to radians. Use minus sign to force clockwise rotation.
  Radians := AAngle * PI / 180;
  RadiansSin := Sin(Radians);
  RadiansCos := Cos(Radians);
  // Step through each row of rotated image.
  for J := 0 to ADest.Height - 1 do
  begin
    RowDest := ADest.ScanLine[J];
    jDest := J - AOffset.Y;
    jPrime := 2 * (jDest - AAxis.Y) + 1; // center y: -1,0,+1
    // Step through each col of rotated image.
    for I := 0 to ADest.Width - 1 do
    begin
      iDest := I - AOffset.X;
      iPrime := 2 * (iDest - AAxis.X) + 1; // center x: -1,0,+1
      // Rotate (iPrime, jPrime) to location of desired pixel
      // Note:  There is negligible difference between floating point and scaled integer arithmetic here, so keep the math simple (and readable).
      iPrimeRotated := Round(iPrime * RadiansCos - jPrime * RadiansSin);
      jPrimeRotated := Round(iPrime * RadiansSin + jPrime * RadiansCos);
      // Transform back to pixel coordinates of image, including translation
      // of origin from axis of rotation to origin of image.
      iOriginal := (iPrimeRotated - 1) div 2 + AAxis.X;
      jOriginal := (jPrimeRotated - 1) div 2 + AAxis.Y;
      // Make sure (iOriginal, jOriginal) is in aSource.  If not, assign blue color to corner points.
      if (iOriginal >= 0) and (iOriginal <= ASource.Width - 1) and (jOriginal >= 0) and (jOriginal <= ASource.Height - 1) then
      begin
        // Assign pixel from rotated space to current pixel in aDest
        RowSource := ASource.ScanLine[jOriginal];
        RowDest[I] := RowSource[iOriginal];
      end
      else
      begin
        RowSource := ASource.ScanLine[0];
        RowDest[I] := RowSource[0];
      end;
    end;
  end;
end;

function RotatedBitmap(ASource: TBitmap; AAngle: Double): TBitmap;
var
  P: array[0..3] of TPoint;
  R: TRect;
begin
  P[0] := Point(0, 0);
  P[1] := Point(ASource.Width - 1, 0);
  P[2] := Point(ASource.Width - 1, ASource.Height - 1);
  P[3] := Point(0, ASource.Height - 1);
  RotatePoints(P, AAngle);
  R := PointsRect(P);
  //
  Result := TRLBitmap.Create;
  try
    Result.PixelFormat := pf32bit;
    Result.Width := R.Right - R.Left;
    Result.Height := R.Bottom - R.Top;
    Result.Transparent := ASource.Transparent;
    Result.TransparentColor := ASource.TransparentColor;
    Result.TransparentMode := ASource.TransparentMode;
    RotateBitmap(ASource, Result, AAngle, Point(ASource.Width div 2, ASource.Height div 2), Point( - R.Left, - R.Top));
  except
    Result.free;
    raise;
  end;
end;

function PointsRect(const APoints: array of TPoint): TRect;
var
  I: Integer;
begin
  for I := 0 to High(APoints) do
    if I = 0 then
    begin
      Result.Left := APoints[I].X;
      Result.Top := APoints[I].Y;
      Result.Right := APoints[I].X;
      Result.Bottom := APoints[I].Y;
    end
    else
    begin
      Result.Left := Min(Result.Left, APoints[I].X);
      Result.Top := Min(Result.Top, APoints[I].Y);
      Result.Right := Max(Result.Right, APoints[I].X);
      Result.Bottom := Max(Result.Bottom, APoints[I].Y);
    end;
end;

function PointsSize(const APoints: array of TPoint): TPoint;
begin
  with PointsRect(APoints) do
  begin
    Result.X := Right - Left;
    Result.Y := Bottom - Top;
  end;
end;

procedure ScalePoints(var APoints: array of TPoint; const ARect: TRect);
var
  bounds: TRect;
  fx, fy: Double;
  I, len: Integer;
begin
  bounds := PointsRect(APoints);
  if RectWidth(bounds) <> 0 then
    fx := RectWidth(ARect) / RectWidth(bounds)
  else
    fx := 0;
  if RectHeight(bounds) <> 0 then
    fy := RectHeight(ARect) / RectHeight(bounds)
  else
    fy := 0;
  if fx = 0 then
    fx := fy;
  if fy = 0 then
    fy := fx;
  if (fx = 0) or (fy = 0) then
    Exit; 
  if fx < fy then
    fy := fx
  else
    fx := fy;
  len := High(APoints) + 1;
  for I := 0 to len - 1 do
    with APoints[I] do
    begin
      X := Round((X - bounds.Left) * fx) + ARect.Left;
      Y := Round((Y - bounds.Top) * fy) + ARect.Top;
    end;
end;

procedure StretchPoints(var APoints: array of TPoint; const ARect: TRect);
var
  bounds: TRect;
  fx, fy: Double;
  I, len: Integer;
begin
  bounds := PointsRect(APoints);
  if RectWidth(bounds) <> 0 then
    fx := RectWidth(ARect) / RectWidth(bounds)
  else
    fx := 0;
  if RectHeight(bounds) <> 0 then
    fy := RectHeight(ARect) / RectHeight(bounds)
  else
    fy := 0;
  if fx = 0 then
    fx := 1;
  if fy = 0 then
    fy := 1;
  if (fx = 0) or (fy = 0) then
    Exit; 
  len := High(APoints) + 1;
  for I := 0 to len - 1 do
    with APoints[I] do
    begin
      X := Round((X - bounds.Left) * fx) + ARect.Left;
      Y := Round((Y - bounds.Top) * fy) + ARect.Top;
    end;
end;

procedure CenterPoints(var APoints: array of TPoint; const ARect: TRect);
var
  bounds: TRect;
  ofx, ofy: Integer;
  I, len: Integer;
begin
  bounds := PointsRect(APoints);
  ofx := (RectWidth(ARect) - RectWidth(bounds)) div 2;
  ofy := (RectHeight(ARect) - RectHeight(bounds)) div 2;
  len := High(APoints) + 1;
  for I := 0 to len - 1 do
    with APoints[I] do
    begin
      X := X - bounds.Left + ARect.Left + ofx;
      Y := Y - bounds.Top + ARect.Top + ofy;
    end;
end;

function TextBounds(const AText: string; AFont: TFont; AAngle: Double): TPoint;
var
  B: TBitmap;
  P: array[0..3] of TPoint;
begin
  B := NeedAuxBitmap;
  B.Canvas.Font.Assign(AFont);
  Result.X := B.Canvas.TextWidth(AText);
  Result.Y := B.Canvas.TextHeight(AText);
  if AAngle <> 0 then
  begin
    P[0] := Point(0, 0);
    P[1] := Point(Result.X, 0);
    P[2] := Point(Result.X, Result.Y);
    P[3] := Point(0, Result.Y);
    RotatePoints(P, AAngle);
    Result := PointsSize(P);
  end;
end;

procedure MoveRect(var ARect: TRect; AX, AY: Integer);
begin
  OffsetRect(ARect, - ARect.Left + AX, - ARect.Top + AY);
end;

function RectWidth(const ARect: TRect): Integer;
begin
  Result := ARect.Right - ARect.Left;
end;

function RectHeight(const ARect: TRect): Integer;
begin
  Result := ARect.Bottom - ARect.Top;
end;

function ReduceRect(const ARect: TRect; APixels: TRect): TRect;
begin
  Result.Left := ARect.Left + APixels.Left;
  Result.Top := ARect.Top + APixels.Top;
  Result.Right := ARect.Right - APixels.Right;
  Result.Bottom := ARect.Bottom - APixels.Bottom;
end;

function IncreaseRect(const ARect: TRect; APixels: TRect): TRect;
begin
  Result.Left := ARect.Left - APixels.Left;
  Result.Top := ARect.Top - APixels.Top;
  Result.Right := ARect.Right + APixels.Right;
  Result.Bottom := ARect.Bottom + APixels.Bottom;
end;

function DiffRect(const ARectOut, ARectIn: TRect): TRect;
begin
  Result.Left := ARectIn.Left + ARectOut.Left;
  Result.Top := ARectIn.Top + ARectOut.Top;
  Result.Right := ARectOut.Right - ARectIn.Right;
  Result.Bottom := ARectOut.Bottom - ARectIn.Bottom;
end;

function IterateJustification(var AText: AnsiString; var AIndex: Integer): Boolean;
  function FindSpc: Boolean;
  const
    SPC = [#32, #9, #13, #10];
  begin
    Result := False;
    while (AIndex > 0) and CharInSet(AText[AIndex], SPC) do
      Dec(AIndex);
    while AIndex > 0 do
      if CharInSet(AText[AIndex], SPC) then
      begin
        while (AIndex > 0) and CharInSet(AText[AIndex], SPC) do
          Dec(AIndex);
        if AIndex > 0 then
        begin
          Insert(#32, AText, AIndex + 1);
          Result := True;
        end;
        Break;
      end
      else
        Dec(AIndex);
  end;
begin
  Result := FindSpc;
  if not Result then
  begin
    AIndex := Length(AText);
    Result := FindSpc;
  end;
end;

function ScaleRect(const ASource, ATarget: TRect; ACenter: Boolean): TRect;
var
  sw, sh, tw, th, W, H: Integer;
  fw, fh: Double;
begin
  sw := ASource.Right - ASource.Left;
  sh := ASource.Bottom - ASource.Top;
  tw := ATarget.Right - ATarget.Left;
  th := ATarget.Bottom - ATarget.Top;
  // calcula o maior dos fatores de proporção entre largura e altura
  if (sw = 0) then //Precaução para evitar divisão por zero
    sw := 1;
  if (sh = 0) then //Precaução para evitar divisão por zero
    sh := 1;
  fw := tw / sw;
  fh := th / sh;
  if fw > fh then
  begin
    H := th;
    W := Round(H * sw / sh);
  end
  else
  begin
    W := tw;
    H := Round(W * sh / sw);
  end;
  Result.Left := ATarget.Left;
  Result.Top := ATarget.Top;
  Result.Right := Result.Left + W;
  Result.Bottom := Result.Top + H;
  if ACenter then
    OffsetRect(Result, (tw - W) div 2, (th - H) div 2);
end;

procedure StreamWrite(AStream: TStream; const AStr: string);
var
  AnsiAux: AnsiString;
begin
  AnsiAux := AnsiString(AStr);
  if AnsiAux <> '' then
    AStream.Write(AnsiAux[1], Length(AnsiAux));
end;

procedure StreamWriteLn(AStream: TStream; const AStr: string = '');
begin
  StreamWrite(AStream, AStr);
  StreamWrite(AStream, sLineBreak);
end;

var
  TempFileNames: TStringList = nil;

procedure RegisterTempFile(const AFileName: String);
begin
  if not Assigned(TempFileNames) then
    TempFileNames := TStringList.Create;
  TempFileNames.Add(AFileName);
end;

procedure UnregisterTempFile(const AFileName: String);
var
  I: Integer;
begin
  if Assigned(TempFileNames) then
  begin
    I := TempFileNames.IndexOf(AFileName);
    if I <> -1 then
      TempFileNames.Delete(I);
  end;
end;

procedure ClearTempFiles;
var
  I: Integer;
begin
  if Assigned(TempFileNames) then
  begin
    for I := 0 to TempFileNames.Count - 1 do
      SysUtils.DeleteFile(TempFileNames[I]);
    TempFileNames.Free;
    TempFileNames := nil;
  end;
end;

function SmartGetFieldDisplayText(Field: TField; const Mask: string = ''): String;
begin
  if (Field is TBlobField) and not Assigned(Field.OnGetText) then
    Result := Field.AsString
  else if (Field is TFloatField) and (Mask <> '') then
    Result := FormatFloat(Mask, Field.AsFloat)
  else
    Result := Field.DisplayText;
end;

initialization
  TempDir := GetTempDir;
  AuxBitmap := nil;
  LogClear;

finalization
  ClearTempFiles;
  FreeObj(AuxBitmap);

end.  
