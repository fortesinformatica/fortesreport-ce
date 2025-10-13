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

{$IfNDef DELPHIXE_UP}
 {$Define NO_SPLITSTRING}
{$EndIf}

{$IfDef FPC}
 {$Define NO_SPLITSTRING}
{$EndIf}

unit RLXLSXFileFormat;

interface

uses
  {$IfDef MSWINDOWS}
   Windows,
  {$EndIf}
  Classes, SysUtils, Contnrs, StrUtils, Types, Variants,
  RLPkZip, RLTypes;

const
  RLXLSXDefaultFontName = 'Calibri';

type
  TRLXLSXCellValueType = (TRLXLSXCellValueTypeNull, TRLXLSXCellValueTypeString,
    TRLXLSXCellValueTypeInteger, TRLXLSXCellValueTypeFloat);

  TRLXLSXCellValue = record case TRLXLSXCellValueType of
    TRLXLSXCellValueTypeNull: (Fill: Integer);
    TRLXLSXCellValueTypeString: (StringIndex: Integer);
    TRLXLSXCellValueTypeInteger: (IntegerValue: Int64);
    TRLXLSXCellValueTypeFloat: (FloatValue: Double);
  end;
  PRLXLSXCellValue = ^TRLXLSXCellValue;

  TRLXLSXCellStyle = (TRLXLSXCellStyleBold);
  TRLXLSXCellStyles = set of TRLXLSXCellStyle;

  TRLXLSXCell = class
  private
    FColId: Integer;
    FValueType: TRLXLSXCellValueType;
    FValue: TRLXLSXCellValue;
    FFormat: Integer;
    function GetCellValue: PRLXLSXCellValue;
  public
    constructor Create(ColId: Integer);
    property ColId: Integer read FColId;
    property ValueType: TRLXLSXCellValueType read FValueType;
    property Value: PRLXLSXCellValue read GetCellValue;
    property Format: Integer read FFormat write FFormat;
  end;

  TRLXLSXRow = class
  private
    FCells: TObjectList;
    FId: Integer;
    function GetCellCount: Integer;
    function GetCells(CellIndex: Integer): TRLXLSXCell;
  public
    constructor Create(Id: Integer);
    destructor Destroy; override;
    function FindCell(ColId: Integer; CanCreate: Boolean = False): TRLXLSXCell;
    property Id: Integer read FId;
    property CellCount: Integer read GetCellCount;
    property Cells[CellIndex: Integer]: TRLXLSXCell read GetCells;
  end;

  TRLXLSXCol = class
  private
    FId: Integer;
    FWidth: Double;
  public
    constructor Create(Id: Integer);
    property Id: Integer read FId;
    property Width: Double read FWidth write FWidth;
  end;

  TRLXLSXFont = class
  private
    FName: string;
    FSize: Integer;
    FBold: Boolean;
    FItalic: Boolean;
  public
    property Name: string read FName;
    property Size: Integer read FSize;
    property Bold: Boolean read FBold;
    property Italic: Boolean read FItalic;
  end;

  TRLXLSXFormatHorzAlignment = (TRLXLSXFormatHorzAlignmentLeft, TRLXLSXFormatHorzAlignmentRight,
    TRLXLSXFormatHorzAlignmentCenter);
  TRLXLSXFormatVertAlignment = (TRLXLSXFormatVertAlignmentTop, TRLXLSXFormatVertAlignmentBottom,
    TRLXLSXFormatVertAlignmentCenter);
  TRLXLSXFormat = class
  private
    FNumberFormatId: Integer;
    FFontIndex: Integer;
    FHorzAlignment: TRLXLSXFormatHorzAlignment;
    FVertAlignment: TRLXLSXFormatVertAlignment;
    FFillIndex: Integer;
    FBorderIndex: Integer;
  public
    property NumberFormatId: Integer read FNumberFormatId;
    property FontIndex: Integer read FFontIndex;
    property FillIndex: Integer read FFillIndex;
    property BorderIndex: Integer read FBorderIndex;
    property HorzAlignment: TRLXLSXFormatHorzAlignment read FHorzAlignment;
    property VertAlignment: TRLXLSXFormatVertAlignment read FVertAlignment;
  end;

  TRLXLSXMerge = class
  private
    FRowId1: Integer;
    FColId1: Integer;
    FRowId2: Integer;
    FColId2: Integer;
  public
    property RowId1: Integer read FRowId1;
    property ColId1: Integer read FColId1;
    property RowId2: Integer read FRowId2;
    property ColId2: Integer read FColId2;
  end;

  TRLXLSXHashList = class
  private
    FHeapPtr: Pointer;
    FHeapSize: Integer;
    FHeapLength: Integer;
    FBuckets: array of TList;
    FItems: TObjectList;
    FOwnerData: Boolean;
    function ComputeHash(const AnsiKey: AnsiString): Cardinal;
    function GetCount: Integer;
    function GetItems(ItemIndex: Integer): TObject;
  public
    constructor Create(OwnerData: Boolean; BucketCount: Integer = 256);
    destructor Destroy; override;
    procedure Clear;
    function Add(const Key: string; Data: TObject): Integer;
    function Find(const Key: string): TObject; overload;
    function Find(const Key: string; var Index: Integer; var Data: TObject): Boolean; overload;
    property Count: Integer read GetCount;
    property Items[ItemIndex: Integer]: TObject read GetItems; default;
  end;

  TRLXLSXWorkbook = class;

  TRLXLSXWorksheet = class
  private
    FFile: TRLXLSXWorkbook;
    FName: string;
    FRows: TObjectList;
    FCols: TObjectList;
    FMerges: TObjectList;
    function MergeCells(RowId1, ColId1, RowId2, ColId2: Integer): TRLXLSXCell;
    function GetCols(ColIndex: Integer): TRLXLSXCol;
    function GetColCount: Integer;
    function GetMergeCount: Integer;
    function GetMerges(MergeIndex: Integer): TRLXLSXMerge;
    function GetRowCount: Integer;
    function GetRows(RowIndex: Integer): TRLXLSXRow;
  public
    constructor Create(Name: string);
    destructor Destroy; override;
    function SetString(RowId, ColId: Integer; const StringValue: string): TRLXLSXCell;
    function SetInteger(RowId, ColId: Integer; IntegerValue: Int64): TRLXLSXCell;
    function SetFloat(RowId, ColId: Integer; FloatValue: Double): TRLXLSXCell;
    function MergeString(RowId1, ColId1, RowId2, ColId2: Integer; const StringValue: string): TRLXLSXCell;
    function MergeInteger(RowId1, ColId1, RowId2, ColId2: Integer; IntegerValue: Int64): TRLXLSXCell;
    function MergeFloat(RowId1, ColId1, RowId2, ColId2: Integer; FloatValue: Double): TRLXLSXCell;
    function FindCell(RowId, ColId: Integer; CanCreate: Boolean = False): TRLXLSXCell;
    function FindCol(ColId: Integer; CanCreate: Boolean = False): TRLXLSXCol;
    function FindRow(RowId: Integer; CanCreate: Boolean = False): TRLXLSXRow;
    function FindMerge(RowId1, ColId1, RowId2, ColId2: Integer; CanCreate: Boolean = False): TRLXLSXMerge;
    property Name: string read FName;
    property ColCount: Integer read GetColCount;
    property Cols[ColIndex: Integer]: TRLXLSXCol read GetCols;
    property RowCount: Integer read GetRowCount;
    property Rows[RowIndex: Integer]: TRLXLSXRow read GetRows;
    property MergeCount: Integer read GetMergeCount;
    property Merges[MergeIndex: Integer]: TRLXLSXMerge read GetMerges;
  end;

  TRLXLSStreamMethod = procedure(Stream: TStream; Arg: TObject) of object;

  TRLXLSXWorkbook = class
  private
    FCreatorName: string;
    FCreatedTime: TDateTime;
    FSheets: TObjectList;
    FFonts: TRLXLSXHashList;
    FFormats: TRLXLSXHashList;

    FStringFileName: string;
    FStringStream: TStream;
    FStringOffsets: array of Int64;
    FStringSizes: array of Int64;
    FStringCount: Integer;

    procedure WriteUTF8(DestStream: TStream; const Data: string);
    procedure StreamMethodToFile(Proc: TRLXLSStreamMethod; FileName: string; Arg: TObject = nil);
    procedure StreamMethodToZip(Proc: TRLXLSStreamMethod; const ItemName: string; Archive: TRLPkZipArchive; Arg: TObject = nil);

    procedure Write_Content_Types_xml(DestStream: TStream; Arg: TObject);
    procedure Write_docProps_app_xml(DestStream: TStream; Arg: TObject);
    procedure Write_root_rels_xml(DestStream: TStream; Arg: TObject);
    procedure Write_docProps_core_xml(DestStream: TStream; Arg: TObject);
    procedure Write_xl_rels_workbook_xml_rels(DestStream: TStream; Arg: TObject);
    procedure Write_xl_theme_theme1_xml(DestStream: TStream; Arg: TObject);
    procedure Write_xl_sharedStrings_xml(DestStream: TStream; Arg: TObject);
    procedure Write_xl_styles_xml(DestStream: TStream; Arg: TObject);
    procedure Write_xl_workbook_xml(DestStream: TStream; Arg: TObject);
    procedure Write_xl_worksheets_sheet_xml(DestStream: TStream; Arg: TObject);

    function GetSheetCount: Integer;
    function GetSheets(SheetIndex: Integer): TRLXLSXWorksheet;
    function GetFontCount: Integer;
    function GetFonts(FontIndex: Integer): TRLXLSXFont;
    function GetFormatCount: Integer;
    function GetFormats(FormatIndex: Integer): TRLXLSXFormat;
    procedure AddDefaultFont;
    procedure NeedStringStream;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    
    procedure SaveToFolder(FolderName: string);
    procedure SaveToFile(const ArchiveFileName: string);

    property CreatorName: string read FCreatorName write FCreatorName;
    property CreatedTime: TDateTime read FCreatedTime write FCreatedTime;

    function AddString(AText: string): Integer;
    function GetString(Index: Integer): string;
    function Font(Size: Integer; Bold: Boolean = False): Integer; overload;
    function Font(const Name: string; Size: Integer; Bold, Italic: Boolean): Integer; overload;
    function Format(NumberFormatId, FontIndex: Integer;
      HorzAlignment: TRLXLSXFormatHorzAlignment;
      VertAlignment: TRLXLSXFormatVertAlignment;
      FillIndex: Integer; BorderIndex: Integer): Integer;
    function AddSheet(Name: string = ''): TRLXLSXWorksheet;

    property SheetCount: Integer read GetSheetCount;
    property Sheets[SheetIndex: Integer]: TRLXLSXWorksheet read GetSheets;
    property FontCount: Integer read GetFontCount;
    property Fonts[FontIndex: Integer]: TRLXLSXFont read GetFonts;
    property FormatCount: Integer read GetFormatCount;
    property Formats[FormatIndex: Integer]: TRLXLSXFormat read GetFormats;
  end;

implementation

const
  XmlHeader = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'#13#10;
  UrlOXml = 'http://schemas.openxmlformats.org';
  UrlOXmlSS06 = UrlOXml + '/spreadsheetml/2006';
  UrlOXmlDoc06 = UrlOXml + '/officeDocument/2006';
  UrlOXmlDocRels06 = UrlOXmlDoc06 + '/relationships';
  UrlOXmlPack06 = UrlOXml + '/package/2006';
  UrlOXmlRels06 = UrlOXmlPack06 + '/relationships';
  UrlMSOfficeSS = 'http://schemas.microsoft.com/office/spreadsheetml';

  CT_OXmlFmt = 'application/vnd.openxmlformats';

{$IfNDef FPC}
{$IfNDef DELPHI8_UP}
type RawByteString = UTF8String;

var UTF8ToUnicodeString: function(const RawBytes: RawByteString): WideString = UTF8Decode;
{$EndIf}
{$IfDef DELPHI2007}
type RawByteString = UTF8String;

var UTF8ToUnicodeString: function(const RawBytes: RawByteString): WideString = UTF8Decode;
{$EndIf}
{$EndIf}

{$ifdef NO_SPLITSTRING}
function SplitString(const S: string; Sep: Char): TStringDynArray;
var
  I, SepCount, SplitIndex, CopyStart: Integer;
begin
  if Length(S) = 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  SepCount := 0;
  for I := 1 to Length(S) do
    if S[I] = Sep then
      Inc(SepCount);
  SetLength(Result, SepCount + 1);
  CopyStart := 1;
  SplitIndex := 0;
  for I := 1 to Length(S) do
    if S[I] = Sep then
    begin
      Result[SplitIndex] := Copy(S, CopyStart, I - CopyStart);
      Inc(SplitIndex);
      CopyStart := I + 1;
    end;
  Result[SplitIndex] := Copy(S, CopyStart, MaxInt);
end;
{$endif}

function Tag(Name: string; Content: Variant): string;
begin
  Result := SysUtils.Format('<%0:s>%1:s</%2:s>', [Name, VarToStr(Content), SplitString(Name, ' ')[0]]);
end;

function EscapeXmlString(const S: string): string;
begin
	Result := StringReplace(StringReplace(StringReplace(StringReplace(StringReplace(S,
    '&', '&amp;', [rfReplaceAll]),
    '<', '&lt;', [rfReplaceAll]),
    '>', '&gt;', [rfReplaceAll]),
    '"', '&quot;', [rfReplaceAll]),
    '''', '&apos;', [rfReplaceAll]);
end;

function LatinToPlain(const S: string): string;
const
  //áéíóúàâêôüãõçÁÉÍÓÚÀÂÊÔÜÃÕÇ
  Latin = #225#233#237#243#250#224#226#234#244#252#227#245#231#193#201#205#211#218#192#194#202#212#220#195#213#199;
  Plain = 'aeiouaaeouaocAEIOUAAEOUAOC';
var
  ChIndex, LatinIndex: Integer;
  Ch: Char;
begin
  Result := S;
  for ChIndex := 1 to Length(Result) do
  begin
    Ch := Result[ChIndex];
    if AnsiChar(Ch) in ['A'..'Z', 'a'..'z', '0'..'9', '_'] then
    else
    begin
      LatinIndex := Pos(Ch, Latin);
      if LatinIndex <> 0 then
        Ch := Plain[LatinIndex]
      else
        Ch := '_';
      Result[ChIndex] := Ch;
    end;
  end;
end;

function FormatFloat(Value: Double): string;
var
  Aux: ShortString;
  Len: Integer;
begin
  Str(Value:9:2, Aux);
  Result := Trim(String(Aux));
  Len := Length(Result);
  while Result[Len] = '0' do
    Dec(Len);
  if Result[Len] = '.' then
    Dec(Len);
  if Len <> Length(Result) then
    SetLength(Result, Len);
end;

function CellId(RowId, ColId: Integer): string;
begin
  Result := Chr(ColId mod 26 + 65);
  repeat
    ColId := ColId div 26;
    if ColId <= 0 then
      Break;
    Result := Chr(ColId mod 26 + 65) + Result;
  until False;
  Result := Result + IntToStr(RowId + 1);
end;

type
  PRLXLSXHashItem = ^TRLXLSXHashItem;
  TRLXLSXHashItem = packed record
    KeyLength: Word;
    KeyOffset: Word;
    Data: PtrInt;
    Index: Integer;
  end;

constructor TRLXLSXHashList.Create(OwnerData: Boolean; BucketCount: Integer);
var
  BucketIndex: Integer;
begin
  inherited Create;
  FOwnerData := OwnerData;
  FHeapPtr := nil;
  FHeapSize := 0;
  FHeapLength := 0;
  FItems := TObjectList.Create(OwnerData);
  SetLength(FBuckets, BucketCount);
  for BucketIndex := 0 to BucketCount - 1 do
    FBuckets[BucketIndex] := TList.Create;
end;

destructor TRLXLSXHashList.Destroy;
var
  BucketIndex: Integer;
begin
  inherited;
  for BucketIndex := 0 to Length(FBuckets) - 1 do
    FBuckets[BucketIndex].Free;
  FItems.Free;
  if FHeapPtr <> nil then
    FreeMem(FHeapPtr);
end;

procedure TRLXLSXHashList.Clear;
var
  BucketIndex: Integer;
begin
  for BucketIndex := 0 to Length(FBuckets) - 1 do
    FBuckets[BucketIndex].Clear;
  FItems.Clear;
  if FHeapPtr <> nil then
    FreeMem(FHeapPtr);
  FHeapPtr := nil;
  FHeapSize := 0;
  FHeapLength := 0;
end;

function TRLXLSXHashList.Add(const Key: string; Data: TObject): Integer;
var
  BucketIndex, SizeNeeded: Integer;
  ItemPtr: PRLXLSXHashItem;
  AnsiKey: AnsiString;
begin
  AnsiKey := AnsiString(Key);
  BucketIndex := ComputeHash(AnsiKey) mod Cardinal(Length(FBuckets));
  if FHeapPtr = nil then
  begin
    FHeapSize := 1024;
    FHeapLength := 0;
    GetMem(FHeapPtr, FHeapSize);
  end;
  SizeNeeded := FHeapLength + SizeOf(TRLXLSXHashItem) + Length(AnsiKey);
  if FHeapSize < SizeNeeded then
  begin
    while FHeapSize < SizeNeeded do
      FHeapSize := FHeapSize * 2;
    ReallocMem(FHeapPtr, FHeapSize);
  end;
  PtrInt(ItemPtr) := PtrInt(FHeapPtr) + FHeapLength;
  ItemPtr.Data := PtrInt(Data);
  ItemPtr.KeyLength := Length(AnsiKey);
  Inc(FHeapLength, SizeOf(TRLXLSXHashItem));
  ItemPtr.KeyOffset := FHeapLength;
  Move(AnsiKey[1], Pointer(PtrInt(FHeapPtr) + ItemPtr.KeyOffset)^, Length(AnsiKey));
  Inc(FHeapLength, Length(AnsiKey));
  FBuckets[BucketIndex].Add(ItemPtr);
  ItemPtr.Index := FItems.Count;
  FItems.Add(Data);
  Result := ItemPtr.Index;
end;

function TRLXLSXHashList.ComputeHash(const AnsiKey: AnsiString): Cardinal;
var
  ChIndex: Integer;
begin
  Result := 0;
  for ChIndex := 1 to Length(AnsiKey) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor Ord(AnsiKey[ChIndex]);
end;

function TRLXLSXHashList.Find(const Key: string): TObject;
var
  Foo: Integer;
begin
  Find(Key, Foo, Result);
end;

function TRLXLSXHashList.Find(const Key: string; var Index: Integer; var Data: TObject): Boolean;
var
  BucketIndex: Integer;
  ItemPtr: PRLXLSXHashItem;
  Items: TList;
  StrPtr: Pointer;
  ItemIndex: Integer;
  AnsiKey: AnsiString;
begin
  AnsiKey := AnsiString(Key);
  BucketIndex := ComputeHash(AnsiKey) mod Cardinal(Length(FBuckets));
  Items := FBuckets[BucketIndex];
  for ItemIndex := 0 to Items.Count - 1 do
  begin
    ItemPtr := Items[ItemIndex];
    if ItemPtr.KeyLength = Length(AnsiKey) then
    begin
      PtrInt(StrPtr) := PtrInt(FHeapPtr) + ItemPtr.KeyOffset;

      if CompareMem(StrPtr, @AnsiKey[1], Length(AnsiKey)) then
      begin
        Data := TObject(ItemPtr.Data);
        Index := ItemPtr.Index;
        Result := True;
        Exit;
      end;
    end;
  end;
  Index := -1;
  Data := nil;
  Result := False;
end;

function TRLXLSXHashList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TRLXLSXHashList.GetItems(ItemIndex: Integer): TObject;
begin
  Result := FItems[ItemIndex];
end;

threadvar
  TempFileNameSeed: Integer;

{ TRLXLSXWorkbook }

constructor TRLXLSXWorkbook.Create;
begin
  inherited Create;
  FCreatorName := 'FortesReport';
  FCreatedTime := Now;
  FSheets := TObjectList.Create;
  FFonts := TRLXLSXHashList.Create(True);
  FFormats := TRLXLSXHashList.Create(True);
  SetLength(FStringOffsets, 0);
  SetLength(FStringSizes, 0);
  FStringCount := 0;
  FStringFileName := '';
  FStringStream := nil;
  //
  AddDefaultFont;
end;

destructor TRLXLSXWorkbook.Destroy;
begin
  inherited;
  Clear;
  FFonts.Free;
  FFormats.Free;
  FSheets.Free;
  FStringStream.Free;
end;

procedure TRLXLSXWorkbook.Clear;
begin
  FSheets.Clear;
  FFonts.Clear;
  FFormats.Clear;
  SetLength(FStringOffsets, 0);
  SetLength(FStringSizes, 0);
  FStringCount := 0;
  FStringStream.Free;
  FStringStream := nil;
  if FStringFileName <> '' then
    DeleteFile(FStringFileName);
  FStringFileName := '';  
end;

procedure TRLXLSXWorkbook.NeedStringStream;
begin
  if FStringStream = nil then
  begin
    SetLength(FStringOffsets, 0);
    SetLength(FStringSizes, 0);
    FStringCount := 0;
    Inc(TempFileNameSeed);
    {$IfDef FPC}
     FStringFileName := SysUtils.Format('RLXLSX[%d.%d.%d].~tmp', [GetProcessID, GetThreadID, TempFileNameSeed]);
    {$Else}
     FStringFileName := SysUtils.Format('RLXLSX[%d.%d.%d].~tmp', [GetCurrentProcessId, GetCurrentThreadId, TempFileNameSeed]);
    {$EndIf}
    FStringStream := TFileStream.Create(FStringFileName, fmCreate);
  end;
end;

procedure TRLXLSXWorkbook.AddDefaultFont;
begin
  Font(RLXLSXDefaultFontName, 10, False, False);
end;

function TRLXLSXWorkbook.AddSheet(Name: string = ''): TRLXLSXWorksheet;
begin
  if Name = '' then
    Name := 'Sheet' + IntToStr(SheetCount + 1);
  Result := TRLXLSXWorksheet.Create(Name);
  Result.FFile := Self;
  FSheets.Add(Result);
end;

function TRLXLSXWorkbook.GetFontCount: Integer;
begin
  Result := FFonts.Count;
end;

function TRLXLSXWorkbook.GetFonts(FontIndex: Integer): TRLXLSXFont;
begin
  Result := FFonts[FontIndex] as TRLXLSXFont;
end;

function TRLXLSXWorkbook.GetFormatCount: Integer;
begin
  Result := FFormats.Count;
end;

function TRLXLSXWorkbook.GetFormats(FormatIndex: Integer): TRLXLSXFormat;
begin
  Result := FFormats.FItems[FormatIndex] as TRLXLSXFormat;
end;

function TRLXLSXWorkbook.GetSheetCount: Integer;
begin
  Result := FSheets.Count;
end;

function TRLXLSXWorkbook.GetSheets(SheetIndex: Integer): TRLXLSXWorksheet;
begin
  Result := FSheets[SheetIndex] as TRLXLSXWorksheet;
end;

procedure TRLXLSXWorkbook.StreamMethodToZip(Proc: TRLXLSStreamMethod; const ItemName: string; Archive: TRLPkZipArchive; Arg: TObject = nil);
var
  TempStream: TMemoryStream;
begin
  TempStream := TMemoryStream.Create;
  try
    Proc(TempStream, Arg);
    TempStream.Position := 0;
    Archive.CompressItem(Archive.AddItem(ItemName), TempStream);
  finally
    TempStream.Free;
  end;
end;

procedure TRLXLSXWorkbook.SaveToFile(const ArchiveFileName: string);
var
  Archive: TRLPkZipArchive;
  Sheet: TRLXLSXWorksheet;
  SheetIndex: Integer;
begin
  Archive := TRLPkZipArchive.Create(ArchiveFileName, fmCreate);
  try
    StreamMethodToZip(Write_root_rels_xml, '_rels/.rels', Archive);
    StreamMethodToZip(Write_docProps_app_xml, 'docProps/app.xml', Archive);
    StreamMethodToZip(Write_docProps_core_xml, 'docProps/core.xml', Archive);
    StreamMethodToZip(Write_xl_sharedStrings_xml, 'xl/sharedStrings.xml', Archive);
    StreamMethodToZip(Write_xl_styles_xml, 'xl/styles.xml', Archive);
    StreamMethodToZip(Write_xl_workbook_xml, 'xl/workbook.xml', Archive);
    StreamMethodToZip(Write_xl_rels_workbook_xml_rels, 'xl/_rels/workbook.xml.rels', Archive);
    StreamMethodToZip(Write_xl_theme_theme1_xml, 'xl/theme/theme1.xml', Archive);
    for SheetIndex := 0 to SheetCount - 1 do
    begin
      Sheet := Sheets[SheetIndex];
      StreamMethodToZip(Write_xl_worksheets_sheet_xml, SysUtils.Format('xl/worksheets/%s.xml',
        [LatinToPlain(AnsiLowerCase(Sheet.Name))]), Archive, Sheet);
    end;
    StreamMethodToZip(Write_Content_Types_xml, '[Content_Types].xml', Archive);
  finally
    Archive.Free;
  end;
end;

procedure TRLXLSXWorkbook.StreamMethodToFile(Proc: TRLXLSStreamMethod; FileName: string; Arg: TObject = nil);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    Proc(FileStream, Arg);
  finally
    FileStream.Free;
  end;
end;

procedure TRLXLSXWorkbook.SaveToFolder(FolderName: string);
var
  Path: string;
  Sheet: TRLXLSXWorksheet;
  SheetIndex: Integer;
begin
  ForceDirectories(FolderName);

  Path := IncludeTrailingPathDelimiter(FolderName) + '_rels';
  ForceDirectories(Path);

  StreamMethodToFile(Write_root_rels_xml, IncludeTrailingPathDelimiter(Path) + '.rels');

  Path := IncludeTrailingPathDelimiter(FolderName) + 'docProps';
  ForceDirectories(Path);
  StreamMethodToFile(Write_docProps_app_xml, IncludeTrailingPathDelimiter(Path) + 'app.xml');
  StreamMethodToFile(Write_docProps_core_xml, IncludeTrailingPathDelimiter(Path) + 'core.xml');

  Path := IncludeTrailingPathDelimiter(FolderName) + 'xl';
  ForceDirectories(Path);
  StreamMethodToFile(Write_xl_sharedStrings_xml, IncludeTrailingPathDelimiter(Path) + 'sharedStrings.xml');
  StreamMethodToFile(Write_xl_styles_xml, IncludeTrailingPathDelimiter(Path) + 'styles.xml');
  StreamMethodToFile(Write_xl_workbook_xml, IncludeTrailingPathDelimiter(Path) + 'workbook.xml');

  Path := IncludeTrailingPathDelimiter(FolderName) + 'xl\_rels';
  ForceDirectories(Path);
  StreamMethodToFile(Write_xl_rels_workbook_xml_rels, IncludeTrailingPathDelimiter(Path) + 'workbook.xml.rels');

  Path := IncludeTrailingPathDelimiter(FolderName) + 'xl\theme';
  ForceDirectories(Path);
  StreamMethodToFile(Write_xl_theme_theme1_xml, IncludeTrailingPathDelimiter(Path) + 'theme1.xml');

  Path := IncludeTrailingPathDelimiter(FolderName) + 'xl\worksheets';
  ForceDirectories(Path);

  for SheetIndex := 0 to SheetCount - 1 do
  begin
    Sheet := Sheets[SheetIndex];
    StreamMethodToFile(Write_xl_worksheets_sheet_xml, IncludeTrailingPathDelimiter(Path) +
      LatinToPlain(AnsiLowerCase(Sheet.Name)) + '.xml', Sheet);
  end;

  Path := IncludeTrailingPathDelimiter(FolderName);
  StreamMethodToFile(Write_Content_Types_xml, Path + '[Content_Types].xml');
end;

procedure TRLXLSXWorkbook.WriteUTF8(DestStream: TStream; const Data: string);
var
  RawBytes: {$IfDef FPC}AnsiString{$Else}RawByteString{$EndIf};
begin
  RawBytes := UTF8Encode(Data);
  DestStream.Write(RawBytes[1], Length(RawBytes));
end;

procedure TRLXLSXWorkbook.Write_Content_Types_xml(DestStream: TStream; Arg: TObject);
var
  SheetIndex: Integer;
  Sheet: TRLXLSXWorksheet;
begin
  WriteUTF8(DestStream,
    XmlHeader +
    SysUtils.Format('<Types xmlns="%s/content-types">', [UrlOXmlPack06]) +
    SysUtils.Format('<Default Extension="rels" ContentType="%s-package.relationships+xml"/>', [CT_OXmlFmt]) +
    '<Default Extension="xml" ContentType="application/xml"/>' +
    SysUtils.Format('<Override PartName="/xl/workbook.xml" ContentType="%s-officedocument.spreadsheetml.sheet.main+xml"/>', [CT_OXmlFmt])
  );

  for SheetIndex := 0 to SheetCount - 1 do
  begin
    Sheet := Sheets[SheetIndex];
    WriteUTF8(DestStream,
      SysUtils.Format('<Override PartName="/xl/worksheets/%s.xml" ', [LatinToPlain(AnsiLowerCase(Sheet.Name))]) +
      SysUtils.Format('ContentType="%s-officedocument.spreadsheetml.worksheet+xml"/>', [CT_OXmlFmt]));
  end;

  WriteUTF8(DestStream,
    SysUtils.Format('<Override PartName="/xl/theme/theme1.xml" ContentType="%s-officedocument.theme+xml"/>', [CT_OXmlFmt]) +
    SysUtils.Format('<Override PartName="/xl/styles.xml" ContentType="%s-officedocument.spreadsheetml.styles+xml"/>', [CT_OXmlFmt]) +
    SysUtils.Format('<Override PartName="/xl/sharedStrings.xml" ContentType="%s-officedocument.spreadsheetml.sharedStrings+xml"/>', [CT_OXmlFmt]) +
    SysUtils.Format('<Override PartName="/docProps/core.xml" ContentType="%s-package.core-properties+xml"/>', [CT_OXmlFmt]) +
    SysUtils.Format('<Override PartName="/docProps/app.xml" ContentType="%s-officedocument.extended-properties+xml"/>', [CT_OXmlFmt]) +
    '</Types>'
  );
end;

procedure TRLXLSXWorkbook.Write_docProps_app_xml(DestStream: TStream; Arg: TObject);
var
  Sheet: TRLXLSXWorksheet;
  SheetIndex: Integer;
begin
  WriteUTF8(DestStream,
    XmlHeader +
    SysUtils.Format('<Properties xmlns="%s/extended-properties" ', [UrlOXmlDoc06]) +
    SysUtils.Format('xmlns:vt="%s/docPropsVTypes">', [UrlOXmlDoc06]) +
    '<Application>Microsoft Excel</Application>' +
    '<DocSecurity>0</DocSecurity>' +
    '<ScaleCrop>false</ScaleCrop>' +
    Tag('HeadingPairs',
      Tag('vt:vector size="2" baseType="variant"',
        Tag('vt:variant', Tag('vt:lpstr', 'Planilhas')) +
        Tag('vt:variant', Tag('vt:i4', SheetCount)))) +
    '<TitlesOfParts>' +
    SysUtils.Format('<vt:vector size="%d" baseType="lpstr">', [SheetCount])
  );
  for SheetIndex := 0 to SheetCount - 1 do
  begin
    Sheet := Sheets[SheetIndex];
    WriteUTF8(DestStream, Tag('vt:lpstr', EscapeXmlString(Sheet.Name)));
  end;
  WriteUTF8(DestStream,
    '</vt:vector>' +
    '</TitlesOfParts>' +
    '<Company></Company>' +
    '<LinksUpToDate>false</LinksUpToDate>' +
    '<SharedDoc>false</SharedDoc>' +
    '<HyperlinksChanged>false</HyperlinksChanged>' +
    '<AppVersion>15.0300</AppVersion>' +
    '</Properties>'
  );
end;

procedure TRLXLSXWorkbook.Write_docProps_core_xml(DestStream: TStream; Arg: TObject);
var
  CreatedTimeFmt: string;
begin
  CreatedTimeFmt := FormatDateTime('yyyy-mm-dd', FCreatedTime) + 'T' + FormatDateTime('hh:nn:ss', FCreatedTime) + 'Z';

  WriteUTF8(DestStream,
    XmlHeader +
    '<cp:coreProperties ' +
    SysUtils.Format('xmlns:cp="%s/metadata/core-properties" ', [UrlOXmlPack06]) +
    'xmlns:dc="http://purl.org/dc/elements/1.1/" ' +
    'xmlns:dcterms="http://purl.org/dc/terms/" ' +
    'xmlns:dcmitype="http://purl.org/dc/dcmitype/" ' +
    'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">' +
    Tag('dc:creator', EscapeXmlString(FCreatorName)) +
    Tag('cp:lastModifiedBy', EscapeXmlString(FCreatorName)) +
    Tag('dcterms:created xsi:type="dcterms:W3CDTF"', CreatedTimeFmt) +
    Tag('dcterms:modified xsi:type="dcterms:W3CDTF"', CreatedTimeFmt) +
    '</cp:coreProperties>'
  );
end;

procedure TRLXLSXWorkbook.Write_root_rels_xml(DestStream: TStream; Arg: TObject);
begin
  WriteUTF8(DestStream,
    XmlHeader +
    SysUtils.Format('<Relationships xmlns="%0:s">', [UrlOXmlRels06]) +
    SysUtils.Format('<Relationship Id="rId3" Type="%0:s/extended-properties" Target="docProps/app.xml"/>', [UrlOXmlDocRels06]) +
    SysUtils.Format('<Relationship Id="rId2" Type="%0:s/metadata/core-properties" Target="docProps/core.xml"/>', [UrlOXmlRels06]) +
    SysUtils.Format('<Relationship Id="rId1" Type="%0:s/officeDocument" Target="xl/workbook.xml"/>', [UrlOXmlDocRels06]) +
    '</Relationships>'
  );
end;

procedure TRLXLSXWorkbook.Write_xl_rels_workbook_xml_rels(DestStream: TStream; Arg: TObject);

  procedure WriteRelationship(Id: Integer; SubType, Target: string);
  begin
    WriteUTF8(DestStream, SysUtils.Format('<Relationship Id="rId%d" Type="%s/%s" Target="%s"/>',
      [Id, UrlOXmlDocRels06, SubType, Target]));
  end;

var
  Sheet: TRLXLSXWorksheet;
  SheetIndex: Integer;
begin
  WriteUTF8(DestStream, XmlHeader);
  WriteUTF8(DestStream, SysUtils.Format('<Relationships xmlns="%s">', [UrlOXmlRels06]));
  for SheetIndex := 0 to SheetCount - 1 do
  begin
    Sheet := Sheets[SheetIndex];
    WriteRelationship(SheetIndex + 1, 'worksheet', SysUtils.Format('worksheets/%s.xml',
      [LatinToPlain(AnsiLowerCase(Sheet.Name))]));
  end;
  WriteRelationship(SheetCount + 1, 'styles', 'styles.xml');
  WriteRelationship(SheetCount + 2, 'sharedStrings', 'sharedStrings.xml');
  WriteRelationship(SheetCount + 3, 'theme', 'theme/theme1.xml');
  WriteUTF8(DestStream, '</Relationships>');
end;

procedure TRLXLSXWorkbook.Write_xl_theme_theme1_xml(DestStream: TStream; Arg: TObject);

  function TypeFace(ScriptName, FaceName: string): string;
  begin
    Result := SysUtils.Format('<a:font script="%s" typeface="%s"/>', [ScriptName, FaceName]);
  end;

  function SRGB(Val: string): string; overload;
  begin
    Result := SysUtils.Format('<a:srgbClr val="%0:s"/>', [Val]);
  end;

  function SRGB(Val, Content: string): string; overload;
  begin
    Result := SysUtils.Format('<a:srgbClr val="%0:s">%1:s</a:srgbClr>', [Val, Content]);
  end;

  function Accent(N, Clr: string): string;
  begin
    Result := SysUtils.Format('<a:accent%0:s>%1:s</a:accent%0:s>', [N, SRGB(Clr)]);
  end;

  function Clr(Tint, Sat: string; Shade: string = ''): string; overload;
  begin
    Result := '<a:schemeClr val="phClr">' +
      IfThen(Tint <> '', '<a:tint val="' + Tint + '"/>') +
      IfThen(Shade <> '', '<a:shade val="' + Shade + '"/>') +
      IfThen(Sat <> '', '<a:satMod val="' + Sat + '"/>') +
      '</a:schemeClr>';
  end;

  function Clr(Val: string = 'phClr'): string; overload;
  begin
    Result := SysUtils.Format('<a:schemeClr val="%s"/>', [Val]);
  end;

  function GS(Pos, Tint, Sat: string; Shade: string = ''): string;
  begin
    Result := SysUtils.Format('<a:gs pos="%s">%s</a:gs>', [Pos, Clr(Tint, Sat, Shade)]);
  end;

  function Shdw(Rad, Dist, Dir, Shape, Content: string): string;
  begin
    Result := SysUtils.Format('<a:outerShdw blurRad="%0:s" dist="%1:s" dir="%2:s" rotWithShape="%3:s">%4:s</a:outerShdw>',
    [Rad, Dist, Dir, Shape, Content]);
  end;

begin
  WriteUTF8(DestStream,
    XmlHeader +
    SysUtils.Format('<a:theme xmlns:a="%s/drawingml/2006/main" name="Office Theme">', [UrlOXml]) +
    '<a:themeElements>' +
    '<a:clrScheme name="Office">' +
    Tag('a:dk1', '<a:sysClr val="windowText" lastClr="000000"/>') +
    Tag('a:lt1', '<a:sysClr val="window" lastClr="FFFFFF"/>') +
    Tag('a:dk2', SRGB('1F497D')) +
    Tag('a:lt2', SRGB('EEECE1')) +
    Accent('1', '4F81BD') +
    Accent('2', 'C0504D') +
    Accent('3', '9BBB59') +
    Accent('4', '8064A2') +
    Accent('5', '4BACC6') +
    Accent('6', 'F79646') +
    Tag('a:hlink', SRGB('0000FF')) +
    Tag('a:folHlink', SRGB('800080')) +
    '</a:clrScheme>' +
    '<a:fontScheme name="Office">' +
    '<a:majorFont>' +
    '<a:latin typeface="Cambria"/>' +
    '<a:ea typeface=""/>' +
    '<a:cs typeface=""/>' +
    TypeFace('Arab', 'Times New Roman') +
    TypeFace('Hebr', 'Times New Roman') +
    TypeFace('Thai', 'Tahoma') +
    TypeFace('Ethi', 'Nyala') +
    TypeFace('Beng', 'Vrinda') +
    TypeFace('Gujr', 'Shruti') +
    TypeFace('Khmr', 'MoolBoran') +
    TypeFace('Knda', 'Tunga') +
    TypeFace('Guru', 'Raavi') +
    TypeFace('Cans', 'Euphemia') +
    TypeFace('Cher', 'Plantagenet Cherokee') +
    TypeFace('Yiii', 'Microsoft Yi Baiti') +
    TypeFace('Tibt', 'Microsoft Himalaya') +
    TypeFace('Thaa', 'MV Boli') +
    TypeFace('Deva', 'Mangal') +
    TypeFace('Telu', 'Gautami') +
    TypeFace('Taml', 'Latha') +
    TypeFace('Syrc', 'Estrangelo Edessa') +
    TypeFace('Orya', 'Kalinga') +
    TypeFace('Mlym', 'Kartika') +
    TypeFace('Laoo', 'DokChampa') +
    TypeFace('Sinh', 'Iskoola Pota') +
    TypeFace('Mong', 'Mongolian Baiti') +
    TypeFace('Viet', 'Times New Roman') +
    TypeFace('Uigh', 'Microsoft Uighur') +
    TypeFace('Geor', 'Sylfaen') +
    '</a:majorFont>' +
    '<a:minorFont>' +
    '<a:latin typeface="Calibri"/>' +
    '<a:ea typeface=""/>' +
    '<a:cs typeface=""/>' +
    TypeFace('Arab', 'Arial') +
    TypeFace('Hebr', 'Arial') +
    TypeFace('Thai', 'Tahoma') +
    TypeFace('Ethi', 'Nyala') +
    TypeFace('Beng', 'Vrinda') +
    TypeFace('Gujr', 'Shruti') +
    TypeFace('Khmr', 'DaunPenh') +
    TypeFace('Knda', 'Tunga') +
    TypeFace('Guru', 'Raavi') +
    TypeFace('Cans', 'Euphemia') +
    TypeFace('Cher', 'Plantagenet Cherokee') +
    TypeFace('Yiii', 'Microsoft Yi Baiti') +
    TypeFace('Tibt', 'Microsoft Himalaya') +
    TypeFace('Thaa', 'MV Boli') +
    TypeFace('Deva', 'Mangal') +
    TypeFace('Telu', 'Gautami') +
    TypeFace('Taml', 'Latha') +
    TypeFace('Syrc', 'Estrangelo Edessa') +
    TypeFace('Orya', 'Kalinga') +
    TypeFace('Mlym', 'Kartika') +
    TypeFace('Laoo', 'DokChampa') +
    TypeFace('Sinh', 'Iskoola Pota') +
    TypeFace('Mong', 'Mongolian Baiti') +
    TypeFace('Viet', 'Arial') +
    TypeFace('Uigh', 'Microsoft Uighur') +
    TypeFace('Geor', 'Sylfaen') +
    '</a:minorFont>' +
    '</a:fontScheme>' +
    '<a:fmtScheme name="Office">' +
    '<a:fillStyleLst>' +
    Tag('a:solidFill', Clr) +
    Tag('a:gradFill rotWithShape="1"',
      Tag('a:gsLst',
        GS('0', '50000', '300000') +
        GS('35000', '37000', '300000') +
        GS('100000', '15000', '350000')) +
    '<a:lin ang="16200000" scaled="1"/>') +
    Tag('a:gradFill rotWithShape="1"',
      Tag('a:gsLst',
        GS('0', '100000', '130000', '100000') +
        GS('100000', '50000', '350000', '100000')) +
    '<a:lin ang="16200000" scaled="0"/>') +
    '</a:fillStyleLst>' +
    Tag('a:lnStyleLst',
      Tag('a:ln w="9525" cap="flat" cmpd="sng" algn="ctr"',
        Tag('a:solidFill',Clr('', '105000', '95000')) +
        '<a:prstDash val="solid"/>') +
      Tag('a:ln w="25400" cap="flat" cmpd="sng" algn="ctr"',
        Tag('a:solidFill', Clr) +
        '<a:prstDash val="solid"/>') +
      Tag('a:ln w="38100" cap="flat" cmpd="sng" algn="ctr"',
        Tag('a:solidFill', Clr) +
        '<a:prstDash val="solid"/>')) +
    Tag('a:effectStyleLst',
      Tag('a:effectStyle', Tag('a:effectLst', Shdw('40000', '20000', '5400000', '0', SRGB('000000', '<a:alpha val="38000"/>')))) +
      Tag('a:effectStyle', Tag('a:effectLst', Shdw('40000', '23000', '5400000', '0', SRGB('000000', '<a:alpha val="35000"/>')))) +
      Tag('a:effectStyle',
        Tag('a:effectLst', Shdw('40000', '23000', '5400000', '0', SRGB('000000', '<a:alpha val="35000"/>'))) +
        Tag('a:scene3d',
          Tag('a:camera prst="orthographicFront"', '<a:rot lat="0" lon="0" rev="0"/>') +
          Tag('a:lightRig rig="threePt" dir="t"', '<a:rot lat="0" lon="0" rev="1200000"/>')) +
        Tag('a:sp3d', '<a:bevelT w="63500" h="25400"/>'))) +
    '<a:bgFillStyleLst>' +
    Tag('a:solidFill', Clr) +
    Tag('a:gradFill rotWithShape="1"',
      Tag('a:gsLst',
        GS('0', '40000', '350000') +
        GS('40000', '45000', '350000', '99000') +
        GS('100000', '', '255000', '20000')) +
      Tag('a:path path="circle"', '<a:fillToRect l="50000" t="-80000" r="50000" b="180000"/>')) +
    Tag('a:gradFill rotWithShape="1"',
      Tag('a:gsLst',
        GS('0', '80000', '300000') +
        GS('100000', '', '200000', '30000')) +
      Tag('a:path path="circle"', '<a:fillToRect l="50000" t="50000" r="50000" b="50000"/>')) +
    '</a:bgFillStyleLst>' +
    '</a:fmtScheme>' +
    '</a:themeElements>' +
    '<a:objectDefaults>' +
    Tag('a:spDef',
      '<a:spPr/>' +
      '<a:bodyPr/>' +
      '<a:lstStyle/>' +
      Tag('a:style',
        Tag('a:lnRef idx="1"', Clr('accent1')) +
        Tag('a:fillRef idx="3"', Clr('accent1')) +
        Tag('a:effectRef idx="2"', Clr('accent1')) +
        Tag('a:fontRef idx="minor"', Clr('lt1')))) +
    Tag('a:lnDef',
      '<a:spPr/>' +
      '<a:bodyPr/>' +
      '<a:lstStyle/>' +
      Tag('a:style',
        Tag('a:lnRef idx="2"', Clr('accent1')) +
        Tag('a:fillRef idx="0"', Clr('accent1')) +
        Tag('a:effectRef idx="1"', Clr('accent1')) +
        Tag('a:fontRef idx="minor"', Clr('tx1')))) +
    '</a:objectDefaults>' +
    '<a:extraClrSchemeLst/>' +
    '</a:theme>'
  );
end;

procedure TRLXLSXWorkbook.Write_xl_sharedStrings_xml(DestStream: TStream; Arg: TObject);
var
  StringIndex: Integer;
  EscapedStr: string;
begin
  WriteUTF8(DestStream, XmlHeader +
    SysUtils.Format('<sst xmlns="%s/main" count="%d" uniqueCount="%d">',
    [UrlOXmlSS06, FStringCount, FStringCount]));
  for StringIndex := 0 to FStringCount - 1 do
  begin
    EscapedStr := EscapeXmlString(GetString(StringIndex));
    if Trim(EscapedStr) <> '' then
      WriteUTF8(DestStream, SysUtils.Format('<si><t>%s</t></si>', [EscapedStr]))
    else
      WriteUTF8(DestStream, SysUtils.Format('<si><t/></si>', [EscapedStr]));
  end;
  WriteUTF8(DestStream, '</sst>');
end;

procedure TRLXLSXWorkbook.Write_xl_styles_xml(DestStream: TStream; Arg: TObject);
var
  Font: TRLXLSXFont;
  Format: TRLXLSXFormat;
  FontIndex, FormatIndex: Integer;
  FontXml, InnerXml: string;
begin
  WriteUTF8(DestStream,
    XmlHeader +
    '<styleSheet ' +
    SysUtils.Format('xmlns="%s/main" ', [UrlOXmlSS06]) +
    SysUtils.Format('xmlns:mc="%s/markup-compatibility/2006" ', [UrlOXml]) +
    'mc:Ignorable="x14ac" ' +
    SysUtils.Format('xmlns:x14ac="%s/2009/9/ac">', [UrlMSOfficeSS]));

  WriteUTF8(DestStream, SysUtils.Format('<fonts count="%d" x14ac:knownFonts="1">', [FontCount]));
  for FontIndex := 0 to FontCount - 1 do
  begin
    Font := Fonts[FontIndex];
    FontXml := SysUtils.Format('<sz val="%d"/><color theme="1"/><name val="%s"/><family val="2"/><scheme val="minor"/>', [Font.Size, Font.Name]);
    if Font.Bold then
      FontXml := '<b/>' + FontXml;
    if Font.Italic then
      FontXml := '<i/>' + FontXml;
    WriteUTF8(DestStream, Tag('font', FontXml));
  end;
  WriteUTF8(DestStream, '</fonts>');

  WriteUTF8(DestStream,
    '<fills count="2">' +
    Tag('fill', '<patternFill patternType="none"/>') +
    Tag('fill', '<patternFill patternType="gray125"/>') +
    '</fills>' +
    Tag('borders count="1"', Tag('border', '<left/><right/><top/><bottom/><diagonal/>')) +
    '<cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>'
  );

  WriteUTF8(DestStream, SysUtils.Format('<cellXfs count="%d">', [FormatCount]));
  for FormatIndex := 0 to FormatCount - 1 do
  begin
    Format := Formats[FormatIndex];
    WriteUTF8(DestStream, SysUtils.Format('<xf numFmtId="%d" fontId="%d" fillId="%d" borderId="%d" xfId="0"',
      [Format.NumberFormatId, Format.FontIndex, Format.FillIndex, Format.BorderIndex]));
    InnerXml := '';
    if (Format.HorzAlignment <> TRLXLSXFormatHorzAlignmentLeft) or
      (Format.VertAlignment <> TRLXLSXFormatVertAlignmentTop) then
    begin
      WriteUTF8(DestStream, ' applyAlignment="1"');
      InnerXml := '<alignment';
      case Format.HorzAlignment of
        TRLXLSXFormatHorzAlignmentRight: InnerXml := InnerXml + ' horizontal="right"';
        TRLXLSXFormatHorzAlignmentCenter: InnerXml := InnerXml + ' horizontal="center"';
      end;
      case Format.VertAlignment of
        TRLXLSXFormatVertAlignmentBottom: InnerXml := InnerXml + ' vertical="bottom"';
        TRLXLSXFormatVertAlignmentCenter: InnerXml := InnerXml + ' vertical="center"';
      end;
      InnerXml := InnerXml + '/>';
    end;
    if InnerXml <> '' then
      WriteUTF8(DestStream, '>' + InnerXml + '</xf>')
    else
      WriteUTF8(DestStream, '/>');
  end;
  WriteUTF8(DestStream, '</cellXfs>');

  WriteUTF8(DestStream,
    '<cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>' +
    '<dxfs count="0"/>' +
    '<tableStyles count="0" defaultTableStyle="TableStyleMedium9" defaultPivotStyle="PivotStyleMedium4"/>' +
    '<extLst>' +
    SysUtils.Format('<ext uri="{EB79DEF2-80B8-43e5-95BD-54CBDDF9020C}" xmlns:x14="%s/2009/9/main">', [UrlMSOfficeSS]) +
    '<x14:slicerStyles defaultSlicerStyle="SlicerStyleLight1"/>' +
    '</ext>' +
    SysUtils.Format('<ext uri="{9260A510-F301-46a8-8635-F512D64BE5F5}" xmlns:x15="%s/2010/11/main">', [UrlMSOfficeSS]) +
    '<x15:timelineStyles defaultTimelineStyle="TimeSlicerStyleLight1"/>' +
    '</ext>' +
    '</extLst>' +
    '</styleSheet>'
  );
end;

procedure TRLXLSXWorkbook.Write_xl_workbook_xml(DestStream: TStream; Arg: TObject);
var
  Sheet: TRLXLSXWorksheet;
  SheetIndex: Integer;
begin
  WriteUTF8(DestStream,
    XmlHeader +
    SysUtils.Format('<workbook xmlns="%s/main" ', [UrlOXmlSS06]) +
    SysUtils.Format('xmlns:r="%s" ', [UrlOXmlDocRels06]) +
    SysUtils.Format('xmlns:mc="%s/markup-compatibility/2006" mc:Ignorable="x15" ', [UrlOXml]) +
    SysUtils.Format('xmlns:x15="%s/2010/11/main">', [UrlMSOfficeSS]) +
    '<fileVersion appName="xl" lastEdited="6" lowestEdited="5" rupBuild="14420"/>' +
    '<workbookPr showInkAnnotation="0" autoCompressPictures="0"/>' +
    '<bookViews>' +
    '<workbookView xWindow="0" yWindow="0" windowWidth="25605" windowHeight="17460" tabRatio="500"/>' +
    '</bookViews>' +
    '<sheets>'
  );
  for SheetIndex := 0 to SheetCount - 1 do
  begin
    Sheet := Sheets[SheetIndex];
    WriteUTF8(DestStream, SysUtils.Format('<sheet name="%0:s" sheetId="%1:d" r:id="rId%1:d"/>',
      [Sheet.Name, SheetIndex + 1]));
  end;
  WriteUTF8(DestStream,
    '</sheets>' +
    '<calcPr calcId="140000" concurrentCalc="0"/>' +
    '<extLst>' +
    '<ext uri="{7523E5D3-25F3-A5E0-1632-64F254C22452}" xmlns:mx="http://schemas.microsoft.com/office/mac/excel/2008/main">' +
    '<mx:ArchID Flags="2"/>' +
    '</ext>' +
    '</extLst>' +
    '</workbook>'
  );
end;

procedure TRLXLSXWorkbook.Write_xl_worksheets_sheet_xml(DestStream: TStream; Arg: TObject);

  procedure WriteSheetDataXml(Sheet: TRLXLSXWorksheet);
  var
    Row: TRLXLSXRow;
    RowIndex, CellIndex: Integer;
    Cell: TRLXLSXCell;
    IdStr, ValStr, TypeStr, CellXml: string;
  begin
    if Sheet.RowCount > 0 then
    begin
      WriteUTF8(DestStream, '<sheetData>');
      for RowIndex := 0 to Sheet.RowCount - 1 do
      begin
        Row := Sheet.Rows[RowIndex];
        //TODO 1:13 ???
        WriteUTF8(DestStream, '<row r="' + IntToStr(Row.Id + 1) + '" spans="1:13" x14ac:dyDescent="0.25">');
        for CellIndex := 0 to Row.CellCount - 1 do
        begin
          Cell := Row.Cells[CellIndex];
          IdStr := CellId(Row.Id, Cell.ColId);
          TypeStr := '';
          case Cell.ValueType of
            TRLXLSXCellValueTypeNull:
              ValStr := '';
            TRLXLSXCellValueTypeString:
            begin
              ValStr := IntToStr(Cell.Value.StringIndex);
              TypeStr := 's';
            end;
            TRLXLSXCellValueTypeInteger: ValStr := IntToStr(Cell.Value.IntegerValue);
            TRLXLSXCellValueTypeFloat: ValStr := FormatFloat(Cell.Value.FloatValue);
          else
            ValStr := 'ERROR';
          end;

          CellXml := '<c r="' + IdStr + '"' +
            IfThen(Cell.Format <> -1, ' s="' + IntToStr(Cell.Format) + '"') +
            IfThen(TypeStr <> '', ' t="' + TypeStr + '"');
          if ValStr <> '' then
            CellXml := CellXml + '><v>' + ValStr + '</v></c>'
          else
            CellXml := CellXml + '/>';
          WriteUTF8(DestStream, CellXml);
        end;
        WriteUTF8(DestStream, '</row>');
      end;
      WriteUTF8(DestStream, '</sheetData>');
    end
    else
      WriteUTF8(DestStream, '<sheetData/>');
  end;

  procedure WriteMergeDataXml(Sheet: TRLXLSXWorksheet);
  var
    Merge: TRLXLSXMerge;
    MergeIndex: Integer;
  begin
    if Sheet.MergeCount > 0 then
    begin
      WriteUTF8(DestStream, SysUtils.Format('<mergeCells count="%d">', [Sheet.MergeCount]));
      for MergeIndex := 0 to Sheet.MergeCount - 1 do
      begin
        Merge := Sheet.Merges[MergeIndex];
        WriteUTF8(DestStream, SysUtils.Format('<mergeCell ref="%s:%s"/>',
          [CellId(Merge.RowId1, Merge.ColId1), CellId(Merge.RowId2, Merge.ColId2)]));
      end;
      WriteUTF8(DestStream, '</mergeCells>');
    end;
  end;

  function CellIdRange(Sheet: TRLXLSXWorksheet): string;
  var
    MaxRowId, MaxColId, RowIndex, CoIndex: Integer;
    Row: TRLXLSXRow;
    Cell: TRLXLSXCell;
  begin
    MaxRowId := 0;
    MaxColId := 0;
    for RowIndex := 0 to Sheet.RowCount - 1 do
    begin
      Row := Sheet.Rows[RowIndex];
      if Row.Id > MaxRowId then
        MaxRowId := Row.Id;
      for CoIndex := 0 to Row.CellCount - 1 do
      begin
        Cell := Row.Cells[CoIndex];
        if Cell.ColId > MaxColId then
          MaxColId := Cell.ColId;
      end;
    end;
    if (MaxRowId = 0) and (MaxColId = 0) then
      Result := 'A1'
    else
      Result := 'A1:' + CellId(MaxRowId, MaxColId);
  end;

  procedure WriteSheetColsXml(Sheet: TRLXLSXWorksheet);
  var
    Col: TRLXLSXCol;
    ColIndex: Integer;
  begin
    if Sheet.ColCount > 0 then
    begin
      WriteUTF8(DestStream, '<cols>');
      for ColIndex := 0 to Sheet.ColCount - 1 do
      begin
        Col := Sheet.Cols[ColIndex];
        if Col.Width > 0 then
          WriteUTF8(DestStream, SysUtils.Format('<col min="%0:d" max="%0:d" width="%1:s" customWidth="1"/>',
            [Col.Id + 1, FormatFloat(Col.Width)]));
      end;
      WriteUTF8(DestStream, '</cols>');
    end;
  end;

var
  Sheet: TRLXLSXWorksheet;
begin
  Sheet := Arg as TRLXLSXWorksheet;
  WriteUTF8(DestStream,
    XmlHeader +
    SysUtils.Format('<worksheet xmlns="%s/main" ', [UrlOXmlSS06]) +
    SysUtils.Format('xmlns:r="%s" ', [UrlOXmlDocRels06]) +
    SysUtils.Format('xmlns:mc="%s/markup-compatibility/2006" ', [UrlOXml]) +
    'mc:Ignorable="x14ac" ' +
    SysUtils.Format('xmlns:x14ac="%s/2009/9/ac">', [UrlMSOfficeSS]) +
    SysUtils.Format('<dimension ref="%s"/>', [CellIdRange(Sheet)]) +
    '<sheetViews>' +
    '<sheetView workbookViewId="0"/>' +
    '</sheetViews>' +
    '<sheetFormatPr defaultColWidth="10" defaultRowHeight="15.75" x14ac:dyDescent="0.25"/>');
  WriteSheetColsXml(Sheet);
  WriteSheetDataXml(Sheet);
  WriteMergeDataXml(Sheet);
  WriteUTF8(DestStream,
    '<pageMargins left="0.75" right="0.75" top="1" bottom="1" header="0.5" footer="0.5"/>' +
    '</worksheet>'
  );
end;

function TRLXLSXWorkbook.AddString(AText: string): Integer;
var
  RawBytes: {$IfDef FPC}AnsiString{$Else}RawByteString{$EndIf};
  Capacity: Integer;
begin
  RawBytes := UTF8Encode(AText);
  if Length(RawBytes) = 0 then
  begin
    Result := -1;
    Exit;
  end;

  NeedStringStream;

  Inc(FStringCount);
  Capacity := Length(FStringOffsets);
  if FStringCount > Capacity then
  begin
    if Capacity < 1024 then
      Capacity := 1024;
    while Capacity < FStringCount do
      Capacity := Capacity * 2;
    SetLength(FStringOffsets, Capacity);
    SetLength(FStringSizes, Capacity);
  end;

  FStringStream.Position := FStringStream.Size;
  FStringOffsets[FStringCount - 1] := FStringStream.Position;
  FStringStream.Write(RawBytes[1], Length(RawBytes));
  FStringSizes[FStringCount - 1] := Length(RawBytes);

  Result := FStringCount - 1;
end;

function TRLXLSXWorkbook.GetString(Index: Integer): string;
var
  RawBytes: {$IfDef FPC}AnsiString{$Else}RawByteString{$EndIf};
begin
  if Index = -1 then
  begin
    Result := '';
    Exit;
  end;
  SetLength(RawBytes, FStringSizes[Index]);
  FStringStream.Position := FStringOffsets[Index];
  FStringStream.Read(RawBytes[1], FStringSizes[Index]);
  Result := {$IfDef FPC}String(RawBytes){$Else} UTF8ToUnicodeString(RawBytes){$EndIf};
end;

function TRLXLSXWorkbook.Font(Size: Integer; Bold: Boolean = False): Integer;
begin
  Result := Font(RLXLSXDefaultFontName, Size, Bold, False);
end;

function TRLXLSXWorkbook.Font(const Name: string; Size: Integer; Bold, Italic: Boolean): Integer;
var
  Font: TRLXLSXFont;
  FontKey: string;
  FontIndex: Integer;
begin
  FontKey := SysUtils.Format('%s,%d,%d,%d', [Name, Size, Byte(Bold), Byte(Italic)]);
  FFonts.Find(FontKey, FontIndex, TObject(Font));
  if Font <> nil then
  begin
    Result := FontIndex;
    Exit;
  end;
  Font := TRLXLSXFont.Create;
  Font.FName := Name;
  Font.FSize := Size;
  Font.FBold := Bold;
  Font.FItalic := Italic;
  Result := FFonts.Add(FontKey, Font);
end;

function TRLXLSXWorkbook.Format(NumberFormatId, FontIndex: Integer;
  HorzAlignment: TRLXLSXFormatHorzAlignment;
  VertAlignment: TRLXLSXFormatVertAlignment;
  FillIndex: Integer; BorderIndex: Integer): Integer;
var
  Format: TRLXLSXFormat;
  FormatKey: string;
  FormatIndex: Integer;
begin
  FormatKey := SysUtils.Format('%d,%d,%d,%d,%d,%d', [NumberFormatId, FontIndex,
    Byte(HorzAlignment), Byte(VertAlignment), FillIndex, BorderIndex]);
  FFormats.Find(FormatKey, FormatIndex, TObject(Format));
  if Format <> nil then
  begin
    Result := FormatIndex;
    Exit;
  end;
  Format := TRLXLSXFormat.Create;
  Format.FNumberFormatId := NumberFormatId;
  Format.FFontIndex := FontIndex;
  Format.FHorzAlignment := HorzAlignment;
  Format.FVertAlignment := VertAlignment;
  Format.FFillIndex := FillIndex;
  Format.FBorderIndex := BorderIndex;
  Result := FFormats.Add(FormatKey, Format);
end;

{ TRLXLSXWorksheet }

constructor TRLXLSXWorksheet.Create(Name: string);
begin
  inherited Create;
  FFile := nil;
  FName := Name;
  FRows := TObjectList.Create;
  FCols := TObjectList.Create;
  FMerges := TObjectList.Create;
end;

destructor TRLXLSXWorksheet.Destroy;
begin
  inherited;
  FRows.Free;
  FCols.Free;
  FMerges.Free;
end;

function TRLXLSXWorksheet.FindCell(RowId, ColId: Integer; CanCreate: Boolean = False): TRLXLSXCell;
var
  Row: TRLXLSXRow;
begin
  Row := FindRow(RowId, CanCreate);
  if Row <> nil then
    Result := Row.FindCell(ColId, CanCreate)
  else
    Result := nil;
end;

function TRLXLSXWorksheet.FindMerge(RowId1, ColId1, RowId2, ColId2: Integer; CanCreate: Boolean = False): TRLXLSXMerge;
var
  Merge: TRLXLSXMerge;
  MergeIndex, InsertAt: Integer;
begin
  InsertAt := MergeCount;
  for MergeIndex := MergeCount - 1 downto 0 do
  begin
    Merge := Merges[MergeIndex];
    if (Merge.RowId1 = RowId1) and (Merge.ColId1 = ColId1) and (Merge.RowId2 = RowId2) and (Merge.ColId2 = ColId2) then
    begin
      Result := Merge;
      Exit;
    end
    else if (Merge.RowId1 < RowId1) or (Merge.RowId1 = RowId1) and (Merge.ColId1 < ColId1) then
    begin
      InsertAt := MergeIndex + 1;
      Break;
    end;
  end;
  if CanCreate then
  begin
    Merge := TRLXLSXMerge.Create;
    FMerges.Insert(InsertAt, Merge);
    Merge.FRowId1 := RowId1;
    Merge.FColId1 := ColId1;
    Merge.FRowId2 := RowId2;
    Merge.FColId2 := ColId2;
    Result := Merge;
  end
  else
    Result := nil;
end;

function TRLXLSXWorksheet.FindRow(RowId: Integer; CanCreate: Boolean = False): TRLXLSXRow;
var
  Row: TRLXLSXRow;
  RowIndex, InsertAt: Integer;
begin
  InsertAt := RowCount;
  for RowIndex := RowCount - 1 downto 0 do
  begin
    Row := Rows[RowIndex];
    if Row.Id = RowId then
    begin
      Result := Row;
      Exit;
    end
    else if Row.Id < RowId then
    begin
      InsertAt := RowIndex + 1;
      Break;
    end;
  end;
  if CanCreate then
  begin
    Row := TRLXLSXRow.Create(RowId);
    FRows.Insert(InsertAt, Row);
    Result := Row;
  end
  else
    Result := nil;
end;

function TRLXLSXWorksheet.FindCol(ColId: Integer; CanCreate: Boolean = False): TRLXLSXCol;
var
  Col: TRLXLSXCol;
  ColIndex, InsertAt: Integer;
begin
  InsertAt := ColCount;
  for ColIndex := ColCount - 1 downto 0 do
  begin
    Col := Cols[ColIndex];
    if Col.Id = ColId then
    begin
      Result := Col;
      Exit;
    end
    else if Col.Id < ColId then
    begin
      InsertAt := ColIndex + 1;
      Break;
    end;
  end;
  if CanCreate then
  begin
    Col := TRLXLSXCol.Create(ColId);
    FCols.Insert(InsertAt, Col);
    Result := Col;
  end
  else
    Result := nil;
end;

function TRLXLSXWorksheet.GetColCount: Integer;
begin
  Result := FCols.Count;
end;

function TRLXLSXWorksheet.GetCols(ColIndex: Integer): TRLXLSXCol;
begin
  Result := FCols[ColIndex] as TRLXLSXCol;
end;

function TRLXLSXWorksheet.GetMergeCount: Integer;
begin
  Result := FMerges.Count;
end;

function TRLXLSXWorksheet.GetMerges(MergeIndex: Integer): TRLXLSXMerge;
begin
  Result := FMerges[MergeIndex] as TRLXLSXMerge;
end;

function TRLXLSXWorksheet.GetRowCount: Integer;
begin
  Result := FRows.Count;
end;

function TRLXLSXWorksheet.GetRows(RowIndex: Integer): TRLXLSXRow;
begin
  Result := FRows[RowIndex] as TRLXLSXRow;
end;

function TRLXLSXWorksheet.MergeCells(RowId1, ColId1, RowId2, ColId2: Integer): TRLXLSXCell;
var
  Cell: TRLXLSXCell;
  Row: TRLXLSXRow;
  RowId, ColId: Integer;
begin
  if (RowId1 = RowId2) and (ColId1 = ColId2) then
    Result := FindRow(RowId1, True).FindCell(ColId1, True)
  else
  begin
    Result := nil;
    for RowId := RowId1 to RowId2 do
    begin
      Row := FindRow(RowId, True);

      if ColId1 > ColId2 then
	    begin
	      ColId := ColId1;
    	  ColId1 := ColId2;
	      ColId2 := ColId;
	    end;
	  
      for ColId := ColId1 to ColId2 do
      begin
        Cell := Row.FindCell(ColId, True);
        if (RowId = RowId1) and (ColId = ColId1) then
          Result := Cell
        else
          Cell.FValueType := TRLXLSXCellValueTypeNull;
      end;
    end;
    FindMerge(RowId1, ColId1, RowId2, ColId2, True);
  end;
end;

function TRLXLSXWorksheet.MergeString(RowId1, ColId1, RowId2, ColId2: Integer; const StringValue: string): TRLXLSXCell;
begin
  Result := MergeCells(RowId1, ColId1, RowId2, ColId2);
  Result.FValueType := TRLXLSXCellValueTypeString;
  Result.FValue.StringIndex := FFile.AddString(StringValue);
end;

function TRLXLSXWorksheet.MergeInteger(RowId1, ColId1, RowId2, ColId2: Integer; IntegerValue: Int64): TRLXLSXCell;
begin
  Result := MergeCells(RowId1, ColId1, RowId2, ColId2);
  Result.FValueType := TRLXLSXCellValueTypeInteger;
  Result.FValue.IntegerValue := IntegerValue;
end;

function TRLXLSXWorksheet.MergeFloat(RowId1, ColId1, RowId2, ColId2: Integer; FloatValue: Double): TRLXLSXCell;
begin
  Result := MergeCells(RowId1, ColId1, RowId2, ColId2);
  Result.FValueType := TRLXLSXCellValueTypeFloat;
  Result.FValue.FloatValue := FloatValue;
end;

function TRLXLSXWorksheet.SetInteger(RowId, ColId: Integer; IntegerValue: Int64): TRLXLSXCell;
begin
  Result := FindCell(RowId, ColId, True);
  Result.FValueType := TRLXLSXCellValueTypeInteger;
  Result.FValue.IntegerValue := IntegerValue;
end;

function TRLXLSXWorksheet.SetFloat(RowId, ColId: Integer; FloatValue: Double): TRLXLSXCell;
begin
  Result := FindCell(RowId, ColId, True);
  Result.FValueType := TRLXLSXCellValueTypeFloat;
  Result.FValue.FloatValue := FloatValue;

end;

function TRLXLSXWorksheet.SetString(RowId, ColId: Integer; const StringValue: string): TRLXLSXCell;
begin
  Result := FindCell(RowId, ColId, True);
  Result.FValueType := TRLXLSXCellValueTypeString;
  Result.FValue.StringIndex := FFile.AddString(StringValue);
end;

{ TRLXLSXRow }

constructor TRLXLSXRow.Create(Id: Integer);
begin
  inherited Create;
  FId := Id;
  FCells := TObjectList.Create;
end;

destructor TRLXLSXRow.Destroy;
begin
  inherited;
  FCells.Free;
end;

function TRLXLSXRow.FindCell(ColId: Integer; CanCreate: Boolean = False): TRLXLSXCell;
var
  Cell: TRLXLSXCell;
  ColIndex, InsertAt: Integer;
begin
  InsertAt := CellCount;
  for ColIndex := CellCount - 1 downto 0 do
  begin
    Cell := Cells[ColIndex];
    if Cell.ColId = ColId then
    begin
      Result := Cell;
      Exit;
    end
    else if Cell.ColId < ColId then
    begin
      InsertAt := ColIndex + 1;
      Break;
    end;
  end;
  if CanCreate then
  begin
    Cell := TRLXLSXCell.Create(ColId);
    FCells.Insert(InsertAt, Cell);
    Result := Cell;
  end
  else
    Result := nil;
end;

function TRLXLSXRow.GetCellCount: Integer;
begin
  Result := FCells.Count;
end;

function TRLXLSXRow.GetCells(CellIndex: Integer): TRLXLSXCell;
begin
  Result := FCells[CellIndex] as TRLXLSXCell;
end;

{ TRLXLSXCell }

constructor TRLXLSXCell.Create(ColId: Integer);
begin
  inherited Create;
  FColId := ColId;
  FValueType := TRLXLSXCellValueTypeString;
  FValue.StringIndex := -1;
end;

function TRLXLSXCell.GetCellValue: PRLXLSXCellValue;
begin
  Result := @FValue;
end;

{ TRLXLSXCol }

constructor TRLXLSXCol.Create(Id: Integer);
begin
  inherited Create;
  FId := Id;
  FWidth := 0;
end;

end.

