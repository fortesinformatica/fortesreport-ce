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

{$Define SEARCHREC_USE_TIME}

{$IfDef DELPHIXE7_UP}
 {$UnDef SEARCHREC_USE_TIME}
{$EndIf}

unit RLPkZip;

interface

uses
  {$IfDef MSWINDOWS}
   Windows,
  {$EndIf}
  SysUtils, Classes, Contnrs, StrUtils, Types,
  {$IfDef FPC}
   zstream,
  {$EndIf}
  ZLib, RLCRC32;

type
  TRLPkZipCommonFileHeader = packed record
    VersionNeededToExtract: Word;
    GeneralPurposeBitFlag: Word;
    CompressionMethod: Word;
    LastModFileTimeDate: Cardinal;
    CRC32: Cardinal;
    CompressedSize: Cardinal;
    UncompressedSize: Cardinal;
    FileNameLength: Word;
    ExtraFieldLength: Word;
  end;

  TRLPkZipDataAddress = packed record
    DataStream: TStream;
    DataOffset: Int64;
  end;

  TRLPkZipFileAlloc = packed record
    LocalFileHeaderSignature: Cardinal;
    CommonFileHeader: TRLPkZipCommonFileHeader;
    FileName: AnsiString;
    ExtraField: AnsiString;
    CompressedDataAddress: TRLPkZipDataAddress;
  end;

  TRLPkZipFileHeader = packed record
    CentralFileHeaderSignature: Cardinal;
    VersionMadeBy: Word;
    CommonFileHeader: TRLPkZipCommonFileHeader;
    FileCommentLength: Word;
    DiskNumberStart: Word;
    InternalFileAttributes: Word;
    ExternalFileAttributes: Cardinal;
    RelativeOffsetOfLocalHeader: Cardinal;
    FileName: AnsiString;
    ExtraField: AnsiString;
    FileComment: AnsiString;
  end;

  TRLPkZipEndOfCentralDir = packed record
    EndOfCentralDirSignature: Cardinal;
    NumberOfThisDisk: Word;
    NumberOfTheDiskWithTheStart: Word;
    TotalNumberOfEntriesOnThisDisk: Word;
    TotalNumberOfEntries: Word;
    SizeOfTheCentralDirectory: Cardinal;
    OffsetOfStartOfCentralDirectory: Cardinal;
    ZipfileCommentLength: Word;
  end;

  TRLPkZipItem = class
  private
    FFileHeader: TRLPkZipFileHeader;
    FFileAlloc: TRLPkZipFileAlloc;
    function GetFileTime: TDateTime;
    function GetName: string;
    function GetFileSize: Int64;
    procedure SetFileTime(const Value: TDateTime);
    function GetFileAttributes: Cardinal;
    procedure SetFileAttributes(const Value: Cardinal);
  public
    procedure SetFileAge(const Value: Cardinal);
    function GetFileAge: Cardinal;
    property Name: string read GetName;
    property FileTime: TDateTime read GetFileTime write SetFileTime;
    property FileSize: Int64 read GetFileSize;
    property FileAttributes: Cardinal read GetFileAttributes write SetFileAttributes;
  end;
  
  TRLPkZipArchive = class
  private
    FArchiveFileName: string;
    FArchiveFileStream: TFileStream;
    FArchiveFileMode: Word;
    FSwapFileName: string;
    FSwapFileStream: TFileStream;
    FZippedItems: TObjectList;
    FArchiveModified: Integer;
    FEndOfCentralDirectory: TRLPkZipEndOfCentralDir;
    FZipFileComment: AnsiString;
    procedure ReadArchiveInfo;
    procedure CommitArchiveFileStream(Reopen: Boolean);
    procedure ReleaseArchiveFileStream;
    function SwapFileStreamNeeded: TFileStream;
    procedure ReleaseSwapFileStream;
    function GetFiles(FileIndex: Integer): TRLPkZipItem;
    function GetFileCount: Integer;
  public
    constructor Create(const ArchiveFileName: string; ArchiveFileMode: Word);
    destructor Destroy; override;
    function DecompressItem(ZipItem: TRLPkZipItem; OutputStream: TStream): Boolean;
    function CompressItem(ZipItem: TRLPkZipItem; InputStream: TStream):Boolean;
    function AddItem(const Name: string): TRLPkZipItem;
    function AddFile(const FileName: string; BaseDir: string = ''): TRLPkZipItem;
    procedure ExtractFile(ZipItem: TRLPkZipItem; const DestDir: string);
    procedure DeleteItem(ZipItem: TRLPkZipItem);
    function ItemByName(const Name: string):TRLPkZipItem;
    property Files[FileIndex: Integer]:TRLPkZipItem read GetFiles;
    property FileCount: Integer read GetFileCount;
  end;

implementation

{ TRLPkZipItem }

function TRLPkZipItem.GetFileTime: TDateTime;
begin
  Result := FileDateToDateTime(FFileAlloc.CommonFileHeader.LastModFileTimeDate);
end;

function TRLPkZipItem.GetName: string;
begin
  Result := string(FFileAlloc.FileName);
end;

function TRLPkZipItem.GetFileSize: Int64;
begin
  Result := FFileHeader.CommonFileHeader.UncompressedSize;
end;

procedure TRLPkZipItem.SetFileTime(const Value: TDateTime);
begin
  FFileAlloc.CommonFileHeader.LastModFileTimeDate := DateTimeToFileDate(Value);
end;

procedure TRLPkZipItem.SetFileAge(const Value: Cardinal);
begin
  FFileAlloc.CommonFileHeader.LastModFileTimeDate := Value;
end;

function TRLPkZipItem.GetFileAttributes: Cardinal;
begin
  Result := FFileHeader.ExternalFileAttributes;
end;

procedure TRLPkZipItem.SetFileAttributes(const Value: Cardinal);
begin
  FFileHeader.ExternalFileAttributes := Value;
end;

function TRLPkZipItem.GetFileAge: Cardinal;
begin
  Result := FFileHeader.CommonFileHeader.LastModFileTimeDate;
end;

{ TRLPkZipArchive }

constructor TRLPkZipArchive.Create(const ArchiveFileName: string; ArchiveFileMode: Word);
begin
  inherited Create;
  FArchiveFileName := ArchiveFileName;
  FArchiveFileMode := ArchiveFileMode;
  FArchiveFileStream := TFileStream.Create(FArchiveFileName, FArchiveFileMode);
  FSwapFileName := '';
  FSwapFileStream := nil;
  FZippedItems := TObjectList.Create;
  FArchiveModified := 0;
  if (ArchiveFileMode and fmCreate) <> fmCreate then
    ReadArchiveInfo;
end;

destructor TRLPkZipArchive.Destroy;
begin
  inherited;
  if FArchiveModified > 0 then
    CommitArchiveFileStream(False)
  else
    ReleaseArchiveFileStream;
  ReleaseSwapFileStream;
  FZippedItems.Free;
end;

procedure TRLPkZipArchive.ReleaseArchiveFileStream;
begin
  if FArchiveFileStream <> nil then
    FArchiveFileStream.Free;
  FArchiveFileStream := nil;
end;

procedure TRLPkZipArchive.CommitArchiveFileStream(Reopen: Boolean);
var
  TempZipStream: TFileStream;
  TempZipFileName: string;
  ZipItem: TRLPkZipItem;
  Allo: ^TRLPkZipFileAlloc;
  Head: ^TRLPkZipFileHeader;
  AuxPos: Int64;
  I: Integer;
begin
  TempZipFileName := ChangeFileExt(FArchiveFileName, '.$$$');
  TempZipStream := TFileStream.Create(TempZipFileName, fmCreate);
  try
    for I := 0 to FZippedItems.Count - 1 do
    begin
      ZipItem := FZippedItems[I] as TRLPkZipItem;
      Head := @ZipItem.FFileHeader;
      Allo := @ZipItem.FFileAlloc;
      Head^.RelativeOffsetOfLocalHeader := TempZipStream.Position;
      TempZipStream.Write(Allo^.LocalFileHeaderSignature, 4);
      if Allo^.LocalFileHeaderSignature = $04034B50 then
      begin
        TempZipStream.Write(Allo^.CommonFileHeader, SizeOf(Allo^.CommonFileHeader));
        TempZipStream.Write(PAnsiChar(Allo^.FileName)^, Allo^.CommonFileHeader.FileNameLength);
        TempZipStream.Write(PAnsiChar(Allo^.ExtraField)^, Allo^.CommonFileHeader.ExtraFieldLength);
        AuxPos := TempZipStream.Position;
        Allo^.CompressedDataAddress.DataStream.Position := Allo^.CompressedDataAddress.DataOffset;
        if Allo^.CommonFileHeader.CompressedSize > 0 then
          TempZipStream.CopyFrom(Allo^.CompressedDataAddress.DataStream, Allo^.CommonFileHeader.CompressedSize);
        Allo^.CompressedDataAddress.DataStream := TempZipStream;
        Allo^.CompressedDataAddress.DataOffset := AuxPos;
      end;
    end;
    FEndOfCentralDirectory.OffsetOfStartOfCentralDirectory := TempZipStream.Position;
    for I := 0 to FZippedItems.Count - 1 do
    begin
      ZipItem := FZippedItems[I] as TRLPkZipItem;
      Head := @ZipItem.FFileHeader;
      TempZipStream.Write(Head^.CentralFileHeaderSignature, 4);
      TempZipStream.Write(Head^.VersionMadeBy, 2);
      TempZipStream.Write(Head^.CommonFileHeader, SizeOf(Head^.CommonFileHeader));
      TempZipStream.Write(Head^.FileCommentLength, 2);
      TempZipStream.Write(Head^.DiskNumberStart, 2);
      TempZipStream.Write(Head^.InternalFileAttributes, 2);
      TempZipStream.Write(Head^.ExternalFileAttributes, 4);
      TempZipStream.Write(Head^.RelativeOffsetOfLocalHeader, 4);
      TempZipStream.Write(PAnsiChar(Head^.FileName)^, Length(Head^.FileName));
      TempZipStream.Write(PAnsiChar(Head^.ExtraField)^, Length(Head^.ExtraField));
      TempZipStream.Write(PAnsiChar(Head^.FileComment)^, Length(Head^.FileComment));
    end;
    with FEndOfCentralDirectory do
    begin
      EndOfCentralDirSignature := $06054B50;
      NumberOfThisDisk := 0;
      NumberOfTheDiskWithTheStart := 0;
      TotalNumberOfEntriesOnThisDisk := FZippedItems.Count;
      TotalNumberOfEntries := FZippedItems.Count;
      SizeOfTheCentralDirectory := TempZipStream.Position - OffsetOfStartOfCentralDirectory;
      ZipfileCommentLength := Length(FZipFileComment);
    end;
    TempZipStream.Write(FEndOfCentralDirectory, SizeOf(FEndOfCentralDirectory));
    TempZipStream.Write(PAnsiChar(FZipFileComment)^, Length(FZipFileComment));
  finally
    TempZipStream.Free;
  end;
  FArchiveFileStream.Free;
  FArchiveFileStream := nil;
  DeleteFile(FArchiveFileName);
  RenameFile(TempZipFileName, FArchiveFileName);
  if Reopen then
  begin
    FArchiveFileMode := fmOpenReadWrite or fmShareDenyNone;
    FArchiveFileStream := TFileStream.Create(FArchiveFileName, FArchiveFileMode);
    for I := 0 to FZippedItems.Count - 1 do
      (FZippedItems[I] as TRLPkZipItem).FFileAlloc.CompressedDataAddress.DataStream := FArchiveFileStream;
  end;
end;

procedure TRLPkZipArchive.ReadArchiveInfo;
var
  ZipItem: TRLPkZipItem;
  Signature: Cardinal;
  FileIndex: Integer;
  Allo: ^TRLPkZipFileAlloc;
  Head: ^TRLPkZipFileHeader;
begin
  FArchiveFileStream.Position := 0;
  repeat
    if FArchiveFileStream.Position = FArchiveFileStream.Size then
      raise Exception.Create('Arquivo zip está corrompido.');
    Signature := 0;
    FArchiveFileStream.Read(Signature, 4);
  until Signature = $04034B50;
  FZippedItems.Clear;
  repeat
    if FArchiveFileStream.Position = FArchiveFileStream.Size then
      raise Exception.Create('Arquivo zip está corrompido.');
    if Signature = $04034B50 then
    begin
      ZipItem := TRLPkZipItem.Create;
      FZippedItems.Add(ZipItem);
      Allo := @ZipItem.FFileAlloc;
      Allo^.LocalFileHeaderSignature := Signature;
      FArchiveFileStream.Read(Allo^.CommonFileHeader, SizeOf(Allo^.CommonFileHeader));
      SetLength(Allo^.FileName, Allo^.CommonFileHeader.FileNameLength);
      FArchiveFileStream.Read(PAnsiChar(Allo^.FileName)^, Allo^.CommonFileHeader.FileNameLength);
      SetLength(Allo^.ExtraField, Allo^.CommonFileHeader.ExtraFieldLength);
      FArchiveFileStream.Read(PAnsiChar(Allo^.ExtraField)^, Allo^.CommonFileHeader.ExtraFieldLength);
      Allo^.CompressedDataAddress.DataStream := FArchiveFileStream;
      Allo^.CompressedDataAddress.DataOffset := FArchiveFileStream.Position;
      FArchiveFileStream.Position := FArchiveFileStream.Position + Allo^.CommonFileHeader.CompressedSize;
    end;
    Signature := 0;
    FArchiveFileStream.Read(Signature, 4);
  until Signature <> $04034B50;
  FileIndex := 0;
  repeat
    if FArchiveFileStream.Position = FArchiveFileStream.Size then
      raise Exception.Create('Arquivo zip está corrompido.');
    if Signature = $02014B50 then
    begin
      ZipItem := FZippedItems[FileIndex] as TRLPkZipItem;
      Head := @ZipItem.FFileHeader;
      Head^.CentralFileHeaderSignature := Signature;
      FArchiveFileStream.Read(Head^.VersionMadeBy, 2);
      FArchiveFileStream.Read(Head^.CommonFileHeader, SizeOf(Head^.CommonFileHeader));
      FArchiveFileStream.Read(Head^.FileCommentLength, 2);
      FArchiveFileStream.Read(Head^.DiskNumberStart, 2);
      FArchiveFileStream.Read(Head^.InternalFileAttributes, 2);
      FArchiveFileStream.Read(Head^.ExternalFileAttributes, 4);
      FArchiveFileStream.Read(Head^.RelativeOffsetOfLocalHeader, 4);
      SetLength(Head^.FileName, Head^.CommonFileHeader.FileNameLength);
      FArchiveFileStream.Read(PAnsiChar(Head^.FileName)^, Head^.CommonFileHeader.FileNameLength);
      SetLength(Head^.ExtraField, Head^.CommonFileHeader.ExtraFieldLength);
      FArchiveFileStream.Read(PAnsiChar(Head^.ExtraField)^, Head^.CommonFileHeader.ExtraFieldLength);
      SetLength(Head^.FileComment, Head^.FileCommentLength);
      FArchiveFileStream.Read(PAnsiChar(Head^.FileComment)^, Head^.FileCommentLength);
      Inc(FileIndex);
    end;
    Signature := 0;
    FArchiveFileStream.Read(Signature, 4);
  until Signature <> $02014B50;
  if Signature = $06054B50 then
  begin
    FEndOfCentralDirectory.EndOfCentralDirSignature := Signature;
    FArchiveFileStream.Read(FEndOfCentralDirectory.NumberOfThisDisk, SizeOf(FEndOfCentralDirectory) - 4);
    SetLength(FZipFileComment, FEndOfCentralDirectory.ZipfileCommentLength);
    FArchiveFileStream.Read(PAnsiChar(FZipFileComment)^, FEndOfCentralDirectory.ZipfileCommentLength);
  end;
end;

function TRLPkZipArchive.DecompressItem(ZipItem: TRLPkZipItem; OutputStream: TStream): Boolean;
var
  Decompressor: TDecompressionStream;
  CompressedStream: TMemoryStream;
  HeaderBytes: AnsiString;
  ReadBytes: Int64;
  LoadedCRC32: Cardinal;
begin
  Result := False;
  CompressedStream := TMemoryStream.Create;
  try
    HeaderBytes := #120#156;
    CompressedStream.Write(HeaderBytes[1],Length(HeaderBytes));
    ZipItem.FFileAlloc.CompressedDataAddress.DataStream.Position := ZipItem.FFileAlloc.CompressedDataAddress.DataOffset;
    CompressedStream.CopyFrom(ZipItem.FFileAlloc.CompressedDataAddress.DataStream, ZipItem.FFileAlloc.CommonFileHeader.CompressedSize);
    CompressedStream.Position := 0;
    Decompressor := TDecompressionStream.Create(CompressedStream);
    try
      ReadBytes := OutputStream.Position;
      LoadedCRC32 := CRC32(Decompressor, ZipItem.FFileAlloc.CommonFileHeader.UncompressedSize, OutputStream);
      ReadBytes := OutputStream.Position - ReadBytes;
      if ReadBytes <> ZipItem.FFileAlloc.CommonFileHeader.UncompressedSize then
        raise Exception.CreateFmt('Unexpected end of file in "%s".',[ZipItem.Name]);
      Result := True;
    finally
      Decompressor.Free;
    end;
  finally
    CompressedStream.Free;
  end;
  if LoadedCRC32 <> ZipItem.FFileHeader.CommonFileHeader.CRC32 then
    raise Exception.CreateFmt('CRC error in "%s".', [ZipItem.Name]);
end;

function TRLPkZipArchive.ItemByName(const Name: string): TRLPkZipItem;
var
  I: Integer;
begin
  for I := 0 to FZippedItems.Count - 1 do
  begin
    Result := FZippedItems[I] as TRLPkZipItem;
    if SameText(Name, Result.Name) then
      Exit;
  end;
  Result := nil;
end;

function TRLPkZipArchive.GetFileCount: Integer;
begin
  Result := FZippedItems.Count;
end;

function TRLPkZipArchive.GetFiles(FileIndex: Integer): TRLPkZipItem;
begin
  Result := FZippedItems[FileIndex] as TRLPkZipItem;
end;

function TRLPkZipArchive.SwapFileStreamNeeded: TFileStream;
begin
  if FSwapFileStream = nil then
  begin
    FSwapFileName := ChangeFileExt(FArchiveFileName, '.S$$');
    FSwapFileStream := TFileStream.Create(FSwapFileName, fmCreate);
  end;
  Result := FSwapFileStream;
end;

procedure TRLPkZipArchive.ReleaseSwapFileStream;
begin
  if FSwapFileStream <> nil then
    FSwapFileStream.Free;
  FSwapFileStream := nil;
  if FSwapFileName <> '' then
    DeleteFile(FSwapFileName);
  FSwapFileName := '';
end;

function TRLPkZipArchive.CompressItem(ZipItem: TRLPkZipItem; InputStream: TStream):Boolean;
var
  Compressor: TCompressionStream;
  CompressedStream: TFileStream;
  Allo: ^TRLPkZipFileAlloc;
  Head: ^TRLPkZipFileHeader;
  CompressedFile: string;
begin
  CompressedFile := ChangeFileExt(FArchiveFileName, '.5$$');
  CompressedStream := TFileStream.Create(CompressedFile, fmCreate);
  try
    Head := @ZipItem.FFileHeader;
    Allo := @ZipItem.FFileAlloc;
    Compressor := TCompressionStream.Create(clDefault, CompressedStream);
    try
      Allo^.CommonFileHeader.CRC32 := CRC32(InputStream, InputStream.Size, Compressor);
    finally
      Compressor.Free;
    end;
    // pula os 2 bytes do header e deixa pra la os 4 byte do footer
    Allo^.CompressedDataAddress.DataStream := SwapFileStreamNeeded;
    Allo^.CompressedDataAddress.DataOffset := Allo^.CompressedDataAddress.DataStream.Size;
    Allo^.CompressedDataAddress.DataStream.Position := Allo^.CompressedDataAddress.DataOffset;
    CompressedStream.Position := 2;
    Allo^.CompressedDataAddress.DataStream.CopyFrom(CompressedStream, CompressedStream.Size-6);
    Allo^.LocalFileHeaderSignature := $04034B50;
    Allo^.CommonFileHeader.VersionNeededToExtract := 20;
    Allo^.CommonFileHeader.GeneralPurposeBitFlag := 0;
    Allo^.CommonFileHeader.CompressionMethod := 8;
    //Allo^.CommonFileHeader.LastModFileTimeDate := DateTimeToFileDate(Now);
    Allo^.CommonFileHeader.CompressedSize := CompressedStream.Size-6;
    Allo^.CommonFileHeader.UncompressedSize := InputStream.Size;
    Allo^.CommonFileHeader.FileNameLength := Length(Allo^.FileName);
    Allo^.CommonFileHeader.ExtraFieldLength := Length(Allo^.ExtraField);
    Head^.CentralFileHeaderSignature := $02014B50;
    Head^.VersionMadeBy := 20;
    Head^.CommonFileHeader := Allo^.CommonFileHeader;
    Head^.FileCommentLength := 0;
    Head^.DiskNumberStart := 0;
    Head^.InternalFileAttributes := 0;
    //Head^.ExternalFileAttributes := 0;
    Head^.RelativeOffsetOfLocalHeader := 0;
    Head^.FileName := Allo^.FileName;
    Head^.ExtraField := Allo^.ExtraField;
    Head^.FileComment := '';
  finally
    CompressedStream.Free;
    DeleteFile(CompressedFile);
  end;
  Result := True;
end;

function TRLPkZipArchive.AddItem(const Name: string):TRLPkZipItem;
var
  Allo: ^TRLPkZipFileAlloc;
  Head: ^TRLPkZipFileHeader;
begin
  Result := TRLPkZipItem.Create;
  FZippedItems.Add(Result);
  Head := @Result.FFileHeader;
  Allo := @Result.FFileAlloc;
  Allo^.FileName := AnsiString(Name);
  Allo^.CompressedDataAddress.DataStream := nil;
  Allo^.CompressedDataAddress.DataOffset := 0;
  Allo^.ExtraField := '';
  Allo^.LocalFileHeaderSignature := $04034B50;
  Allo^.CommonFileHeader.VersionNeededToExtract := 20;
  Allo^.CommonFileHeader.GeneralPurposeBitFlag := 0;
  Allo^.CommonFileHeader.CompressionMethod := 8;
  Allo^.CommonFileHeader.LastModFileTimeDate := DateTimeToFileDate(Now);
  Allo^.CommonFileHeader.CRC32 := 0;
  Allo^.CommonFileHeader.CompressedSize := 0;
  Allo^.CommonFileHeader.UncompressedSize := 0;
  Allo^.CommonFileHeader.FileNameLength := Length(Allo^.FileName);
  Allo^.CommonFileHeader.ExtraFieldLength := Length(Allo^.ExtraField);
  Head^.CentralFileHeaderSignature := $02014B50;
  Head^.VersionMadeBy := 20;
  Head^.CommonFileHeader := Allo^.CommonFileHeader;
  Head^.FileCommentLength := 0;
  Head^.DiskNumberStart := 0;
  Head^.InternalFileAttributes := 0;
  Head^.ExternalFileAttributes := 0;
  Head^.RelativeOffsetOfLocalHeader := 0;
  Head^.FileName := Allo^.FileName;
  Head^.ExtraField := Allo^.ExtraField;
  Head^.FileComment := '';
  Inc(FArchiveModified);
end;

procedure TRLPkZipArchive.DeleteItem(ZipItem: TRLPkZipItem);
begin
  FZippedItems.Extract(ZipItem);
  ZipItem.Free;
  Inc(FArchiveModified);
end;

function TRLPkZipArchive.AddFile(const FileName: string; BaseDir: string=''): TRLPkZipItem;
var
  FileStream: TFileStream;
  SearchRec: TSearchRec;
begin
  if BaseDir <> '' then
    BaseDir := IncludeTrailingPathDelimiter(BaseDir);
  if FindFirst(FileName, faAnyFile, SearchRec)=0 then
  begin
    FileStream := TFileStream.Create(FileName, fmOpenRead);
    try
      Result := AddItem(AnsiReplaceStr(Copy(FileName, Length(BaseDir)+1, MaxInt), PathDelim, '/'));
      {$IfDef SEARCHREC_USE_TIME}
       Result.SetFileAge(SearchRec.Time);
      {$Else}
       Result.SetFileAge(DateTimeToFileDate(SearchRec.TimeStamp));
      {$EndIf}
      Result.FileAttributes := SearchRec.Attr;
      CompressItem(Result, FileStream);
    finally
      FileStream.Free;
    end;
    FindClose(SearchRec);
  end
  else
    Result := nil;
end;

procedure TRLPkZipArchive.ExtractFile(ZipItem: TRLPkZipItem; const DestDir: string);
var
  FileStream: TFileStream;
  FileFolder: string;
  FilePath: string;
begin
  FilePath := AnsiReplaceStr(ZipItem.Name, '/', PathDelim);
  if DestDir <> '' then
    FilePath := IncludeTrailingPathDelimiter(DestDir) + FilePath;
  FileFolder := ExtractFilePath(FilePath);
  if FileFolder <> '' then
    ForceDirectories(FileFolder);
  if FileFolder <> FilePath then
  begin
    FileStream := TFileStream.Create(FilePath, fmCreate);
    try
      DecompressItem(ZipItem, FileStream);
    finally
      FileStream.Free;
    end;
  end;
  FileSetDate(FilePath, ZipItem.GetFileAge);
  {$IFDEF WINDOWS}
  FileSetAttr(FilePath, ZipItem.FileAttributes);
  {$ENDIF}
end;

end.

