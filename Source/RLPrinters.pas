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
{                              Luis Michel Silva Moreira                       }
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

{@unit RLPrinters - Implementação do wrapper para o objeto Printer. }

unit RLPrinters;

interface

uses
  {$IfDef MSWINDOWS}
   Windows, WinSpool,
   {$IfNDef FPC}
    Messages,
   {$EndIf}
  {$EndIf}
  Classes, SysUtils,
  {$IfDef CLX}
   QTypes, QGraphics, QDialogs, QPrinters,
  {$Else}
   Types, Graphics, Dialogs, Printers,
  {$EndIf}
  {$IfDef FPC}
   OSPrinters,
   {$IfDef MSWINDOWS} WinUtilPrn, {$Else} process, {$EndIf}
  {$Else}
    Math,
  {$EndIf}
  RLConsts, RLTypes, RLUtils;

type
  TRLPrintOddEvenPages = (odOddPagesOnly, odEvenPagesOnly, odAllPages);

  {$IfNDef FPC}
  TSavedPrinterSettings = record
    PrinterName: String;
    DevMode: PDeviceMode;
    Bytes: Integer;
  end;
  {$EndIf}

  TRLPrinterWrapper = class
  private
    FPrinters: TStrings;
    FCustomWidth: Double;
    FCustomHeight: Double;
    FOddEven: TRLPrintOddEvenPages;
    {$IfNDef FPC}
    SavedPrinterSettings: TSavedPrinterSettings;
    {$EndIf}
    //
    function GetPrinterIndex: Integer;
    procedure SetPrinterIndex(const Value: Integer);
    function GetCopies: Integer;
    procedure SetCopies(const Value: Integer);
    function GetDuplex: Boolean;
    procedure SetDuplex(const Value: Boolean);
    function GetOddEven: TRLPrintOddEvenPages;
    procedure SetOddEven(const Value: TRLPrintOddEvenPages);
    function GetPrinterName: string;
    procedure SetPrinterName(const APrinterName: string);
    function GetPrinterPort: string;
    procedure LoadPrintersList(APrinters: TStrings);
    procedure PrintersNeeded;
    function AnyPrinter: Boolean;
    function GetPrinterNames(AIndex: Integer): string;
    function GetPrinterPorts(AIndex: Integer): string;
    procedure SelectSystemPaperSize(APaperSize: TRLSystemPaperType; APaperWidthMM, APaperHeightMM: Double;
      AOrientation: TRLSystemOrientation; ASetPaperBin: Boolean);
    function GetCanvas: TCanvas;
    function GetPrinterDisplays(AIndex: Integer): string;
    procedure LoadDefaultMetrics(var APrinterMetrics: TRLPrinterMetrics);
    {$IfNDef FPC}
    procedure CreateDeviceMode(WindowHandle: HWND);
    procedure ApplyDeviceMode;
    procedure ClearSavedDevMode;
    {$EndIf}
  protected
    procedure GetBinNames(AStringList: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure BeginDoc(const ATitle: string = '');
    procedure EndDoc;
    //
    procedure NewPage;
    function Printers: TStrings;
    function ExecuteSetup(WindowHandle: THandle): Boolean;
    function SetupEnabled: Boolean;
    procedure Refresh;
    function SupportsDuplex: Boolean;
    //
    procedure SetPaperSize(APaperWidthMM, APaperHeightMM: Double; AOrientationLandscape: Boolean;
      AForceEmulation, ASetPaperBin: Boolean);
    procedure LoadMetrics(var APrinterMetrics: TRLPrinterMetrics);
    //
    property PrinterIndex: Integer read GetPrinterIndex write SetPrinterIndex;
    property PrinterName: string read GetPrinterName write SetPrinterName;
    property PrinterPort: string read GetPrinterPort;
    property Copies: Integer read GetCopies write SetCopies;
    property Duplex: Boolean read GetDuplex write SetDuplex;
    property OddEven: TRLPrintOddEvenPages read GetOddEven write SetOddEven;
    property Canvas: TCanvas read GetCanvas;
    //
    property PrinterNames[AIndex: Integer]: string read GetPrinterNames;
    property PrinterPorts[AIndex: Integer]: string read GetPrinterPorts;
    property PrinterDisplays[AIndex: Integer]: string read GetPrinterDisplays;
  end;

var
  AllowSetPaperBin: Boolean = False;

function RLPrinter: TRLPrinterWrapper;

implementation

var
  WarningDisplayed: Boolean;
  RLPrinterInstance: TRLPrinterWrapper;

// UTILS

function RLPrinter: TRLPrinterWrapper;
begin
  if not Assigned(RLPrinterInstance) then
    RLPrinterInstance := TRLPrinterWrapper.Create;
  Result := RLPrinterInstance;
end;

function TruePrinterName(const APrinterName: string): string;
var
  I: Integer;
begin
  I := Pos(' on ', APrinterName);
  if I > 0 then
    Result := Copy(APrinterName, 1, I - 1)
  else
    Result := APrinterName;
end;

function TruePrinterPort(const APrinterName: string): string;
var
  I: Integer;
begin
  I := Pos(' on ', APrinterName);
  if I > 0 then
    Result := Copy(APrinterName, I + 4, Length(APrinterName))
  else
    Result := APrinterName;
end;

procedure EWrapper(const AMessage: string = ' ');
begin
  raise Exception.Create(AMessage);
end;

{$IfNDef FPC}
type
  TPrinterEx = class(TPrinter) end;
{$EndIf}

{$IfDef FPC}
procedure ReloadCurrentPrinter;
begin
  Printer.Refresh;
end;
{$Else}
procedure ReloadCurrentPrinter;
var
  Device, Driver, Port: array[0..MAX_PATH] of char;
  hDeviceMode: THandle;
  P: TPrinterEx;
begin
  P := TPrinterEx(Printer);
  P.GetPrinter(Device, Driver, Port, hDeviceMode);
  hDeviceMode := 0;
  P.SetPrinter(Device, Driver, Port, hDeviceMode);
end;
{$endif}

{ TRLPrinterWrapper }

constructor TRLPrinterWrapper.Create;
begin
  FPrinters := nil;
  FOddEven := odAllPages;
  //
  inherited;

  {$IfNDef FPC}
  SavedPrinterSettings.PrinterName := '';
  SavedPrinterSettings.DevMode := nil;
  SavedPrinterSettings.Bytes := 0;
  {$EndIf}
end;

destructor TRLPrinterWrapper.Destroy;
begin
  {$IfNDef FPC}
  ClearSavedDevMode;
  {$EndIf}

  if Assigned(FPrinters) then
    FPrinters.free;
  //
  inherited;
end;

procedure TRLPrinterWrapper.PrintersNeeded;
begin
  if not Assigned(FPrinters) then
  begin
    FPrinters := TStringList.Create; 
    LoadPrintersList(FPrinters);
  end;
end;

procedure TRLPrinterWrapper.BeginDoc(const ATitle: string = '');
begin
  {$IfNDef FPC}
  ApplyDeviceMode;
  {$EndIf}
  Printer.Title := ATitle;
  Printer.BeginDoc;
  {$IfNDef FPC}
  Printer.Canvas.Font.PixelsPerInch := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
  {$Else}
  Printer.Canvas.Font.PixelsPerInch := Printer.YDPI;
  {$EndIf}
end;

procedure TRLPrinterWrapper.EndDoc;
var
  savedprinterindex: Integer;
begin
  // Na CLX o método EndDoc destroi o objeto PrinterAdapter.
  // Com isso se perde a referência para o OutputDevice escolhido. Nele se baseia o nosso PrinterIndex.
  // Temos que salvá-lo aqui e restaurar depois do EndDoc.
  savedprinterindex := PrinterIndex;
  Printer.EndDoc;
  PrinterIndex := savedprinterindex;
end;

procedure TRLPrinterWrapper.NewPage;
begin
  Printer.NewPage;
end;

function TRLPrinterWrapper.GetCanvas: TCanvas;
begin
  Result := Printer.Canvas;
end;

function ConvOrientation(AOrientation: TRLSystemOrientation): TPrinterOrientation;
begin
  if AOrientation = DMORIENT_PORTRAIT then
    Result := Printers.poPortrait
  else
    Result := Printers.poLandscape;
end;

{$IfDef MSWINDOWS}
procedure TRLPrinterWrapper.SelectSystemPaperSize(APaperSize: TRLSystemPaperType;
  APaperWidthMM, APaperHeightMM: Double; AOrientation: TRLSystemOrientation; ASetPaperBin: Boolean);
var
  {$IfDef FPC}
  FDeviceMode: PDeviceModeW;
  PDev: TPrinterDevice;
  {$Else}
  FDeviceMode: PDeviceMode;
  {$EndIf}
  FCapabilities, ALong: longint;
  FDriverHandle: THandle;
  FDevice, FDriver, FPort: PChar;
  FPrinterName: string;
  dmPaperWidthMax, dmPaperLengthMax: Short;

  function Able(Hability: Integer): Boolean;
  begin
    Result := (FCapabilities and Hability) > 0;
  end;

begin
  FDriverHandle := 0;
  FDevice := nil;
  FDriver := nil;
  FPort := nil;
  if AnyPrinter then
    try
      GetMem(FDevice, 255);
      GetMem(FDriver, 255);
      GetMem(FPort, 255);
      {$IFNDEF FPC}
      Printer.GetPrinter(FDevice, FDriver, FPort, FDriverHandle);
      if FDriverHandle = 0 then
      begin
        ReloadCurrentPrinter;
        Printer.GetPrinter(FDevice, FDriver, FPort, FDriverHandle);
      end;
      if FDriverHandle = 0 then
        Abort;
      FDeviceMode := GlobalLock(FDriverHandle);
      {$ELSE}
      PDev := TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
      FDeviceMode := PDev.DevModeW;
      {$ENDIF}
      if FDeviceMode = nil then
        Abort;
      try
        FCapabilities := FDeviceMode^.dmFields;
        //if Able(DM_PAPERSIZE) then // não funciona no Win2k/XP
        FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_PAPERSIZE;
        FDeviceMode^.dmPaperSize := APaperSize;
        // muda o tamanho do papel
        if APaperSize = DMPAPER_USER then
        begin
          dmPaperWidthMax := high(Smallint);
          dmPaperLengthMax := dmPaperWidthMax;
          {$IFNDEF FPC}
          ALong := DeviceCapabilities(FDevice, FPort, DC_MAXEXTENT, nil, FDeviceMode);
          {$Else}
          ALong := DeviceCapabilitiesW( FDeviceMode^.dmDeviceName, nil,
                                        DC_MAXEXTENT, nil, FDeviceMode);
          {$EndIf}
          if (ALong > 0) then
          begin
            //dmPaperLengthMax := HIWORD(ALong);  // Not Used, it will return Spooler Max Paper length. (we want try to override it)
            dmPaperWidthMax  := LOWORD(ALong);
          end;

          if (APaperWidthMM <> 0) then
          begin
            FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_PAPERWIDTH;
            FDeviceMode^.dmPaperWidth := min(Round(APaperWidthMM * 10), dmPaperWidthMax);
          end;
          if (APaperHeightMM <> 0) then
          begin
            FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_PAPERLENGTH;
            FDeviceMode^.dmPaperLength := min(Round(APaperHeightMM * 10), dmPaperLengthMax);
          end;
        end;
        // muda a orientacao do papel
        if Able(DM_ORIENTATION) then
        begin
          FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_ORIENTATION;
          FDeviceMode^.dmOrientation := AOrientation;
        end;
        // muda a alimentacao
        if AllowSetPaperBin and ASetPaperBin and Able(DM_DEFAULTSOURCE) then
        begin
          FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_DEFAULTSOURCE;
          FDeviceMode^.dmDefaultSource := DMBIN_MANUAL;
        end;
      finally
        GlobalUnlock(FDriverHandle);
      end;
    finally
      if FDevice <> nil then
        FreeMem(FDevice, 255);
      if FDriver <> nil then
        FreeMem(FDriver, 255);
      if FPort <> nil then
        FreeMem(FPort, 255);
    end;
  //
  Printer.Orientation := ConvOrientation(AOrientation);
  FPrinterName := PrinterName;
  PostMessage(HWND_BROADCAST, WM_DEVMODECHANGE, 0, PtrInt(@FPrinterName[1]));
end;
{$Else}
procedure TRLPrinterWrapper.SelectSystemPaperSize(APaperSize: TRLSystemPaperType;
  APaperWidthMM, APaperHeightMM: Double; AOrientation: TRLSystemOrientation; ASetPaperBin: Boolean);
begin
  //Printer.PrintAdapter.PageSize := APaperSize;
  // adaptação de dimensões para o componente
  // Na LCL não existe metodo portável para se definir o tamanho do papel personalizado
  if APaperSize = UserPaperCode then
  begin
    {
    if aPaperWidthMM<>0 then
      Printer.PrintAdapter.PageWidth:=aPaperWidthMM;
    if aPaperHeightMM<>0 then
      Printer.PrintAdapter.PageHeight:=aPaperHeightMM;
    }
  end;
  // orientação do papel
  Printer.Orientation := ConvOrientation(AOrientation);
end;
{$EndIf}

{$IfNDef FPC}
procedure TRLPrinterWrapper.GetBinNames(AStringList: TStrings);
type
  TBinName = array[0..23] of Char;
  TBinNameArray = array[1..99] of TBinName;
  PBinNameArray = ^TBinNameArray;
  TBinArray = array[1..99] of Word;
  PBinArray = ^TBinArray;
var
  FDevice: PChar;
  FDriver: PChar;
  FPort: PChar;
  FDriverHandle: THandle;
  I: Integer;
  iBinNames: Integer;
  iBins: Integer;
  pBinNames: PBinNameArray;
  pBins: PBinArray;
begin
  FDriverHandle := 0;
  FDevice := nil;
  FDriver := nil;
  FPort := nil;
  if AnyPrinter then
    try
      GetMem(FDevice, 255);
      GetMem(FDriver, 255);
      GetMem(FPort, 255);
      Printer.GetPrinter(FDevice, FDriver, FPort, FDriverHandle);
      if FDriverHandle = 0 then
      begin
        ReloadCurrentPrinter;
        Printer.GetPrinter(FDevice, FDriver, FPort, FDriverHandle);
      end;
      if FDriverHandle = 0 then
        Abort;
      //
      iBinNames := DeviceCapabilities(FDevice, FPort, DC_BINNAMES, nil, nil);
      if iBinNames > 0 then
      begin
        GetMem(pBinNames, iBinNames * SizeOf(TBinName));
        // descobre a quantidade de "bins" que existem e dimensiona o vetor de bins
        iBins := DeviceCapabilities(FDevice, FPort, DC_BINS, nil, nil);
        GetMem(pBins, iBins * SizeOf(Word));
        try
          DeviceCapabilities(FDevice, FPort, DC_BINNAMES, PChar(pBinNames), nil);
          DeviceCapabilities(FDevice, FPort, DC_BINS, PChar(pBins), nil);
          AStringList.Clear;
          for I := 1 to iBinNames do
            AStringList.Add(Format('%s (%d)', [pBinNames^[I], pBins^[I]]));
        finally
          FreeMem(pBinNames);
          if pBins <> nil then
            FreeMem(pBins);
        end;
      end;
      //
    finally
      if FDevice <> nil then
        FreeMem(FDevice, 255);
      if FDriver <> nil then
        FreeMem(FDriver, 255);
      if FPort <> nil then
        FreeMem(FPort, 255);
    end;
end;

{$Else}
procedure TRLPrinterWrapper.GetBinNames(AStringList: TStrings);
begin
  AStringList.Assign( Printer.SupportedBins );
end;
{$endif}

procedure TRLPrinterWrapper.SetPaperSize(APaperWidthMM, APaperHeightMM: Double; AOrientationLandscape: Boolean; AForceEmulation, ASetPaperBin: Boolean);
var
  ResultPaperSize: TRLSystemPaperType;
  ResultPaperWidth: Double;
  ResultPaperHeight: Double;
  ResultOrientation: TRLSystemOrientation;
begin
  DetectPaperSize(APaperWidthMM, APaperHeightMM, AOrientationLandscape, AForceEmulation, 
                  ResultPaperSize, ResultPaperWidth, ResultPaperHeight, ResultOrientation);
  SelectSystemPaperSize(ResultPaperSize, ResultPaperWidth, ResultPaperHeight, 
    ResultOrientation, ASetPaperBin);
  FCustomWidth := APaperWidthMM;
  FCustomHeight := APaperHeightMM;
end;

procedure TRLPrinterWrapper.LoadMetrics(var APrinterMetrics: TRLPrinterMetrics);
{$ifndef FPC}
var
  dc: HDC;
{$endif}
begin
  try
    if not AnyPrinter then
    begin
      LoadDefaultMetrics(APrinterMetrics);
      Exit;
    end;

{$ifndef FPC}
    dc := Printer.Handle;
    if dc = 0 then
    begin
      ReloadCurrentPrinter;
      dc := Printer.Handle;
      if dc = 0 then
        raise Exception.Create(GetLocalizeStr(LocaleStrings.LS_NoHandle));
    end;
    APrinterMetrics.PPIX := GetDeviceCaps(dc, LOGPIXELSX); // Number of pixels per logical inch along the page width
    APrinterMetrics.PPIY := GetDeviceCaps(dc, LOGPIXELSY); // Number of pixels per logical inch along the page height
    APrinterMetrics.PhysicalWidth := GetDeviceCaps(dc, PHYSICALWIDTH); // Width of the physical page, in device units
    APrinterMetrics.PhysicalHeight := GetDeviceCaps(dc, PHYSICALHEIGHT); // Height of the physical page, in device units
    APrinterMetrics.MarginLeft := GetDeviceCaps(dc, PHYSICALOFFSETX); // Distance from the left edge of the physical page to the left edge of the printable area, in device units
    APrinterMetrics.MarginTop := GetDeviceCaps(dc, PHYSICALOFFSETY); // Distance from the top edge of the physical page to the top edge of the printable area, in device units
    APrinterMetrics.ClientWidth := GetDeviceCaps(dc, HORZRES); // Width, in pixels, of the page
    APrinterMetrics.ClientHeight := GetDeviceCaps(dc, VERTRES); // Height, in raster lines, of the page
    APrinterMetrics.MarginRight := APrinterMetrics.PhysicalWidth - (APrinterMetrics.MarginLeft + APrinterMetrics.ClientWidth);
    APrinterMetrics.MarginBottom := APrinterMetrics.PhysicalHeight - (APrinterMetrics.MarginTop + APrinterMetrics.ClientHeight);
{$else}
    APrinterMetrics.PPIX := Printer.XDPI;
    APrinterMetrics.PPIY := Printer.YDPI;
    if Printer.PageWidth = 0 then
      APrinterMetrics.PhysicalWidth := Round(FCustomWidth * APrinterMetrics.PPIX / InchAsMM)
    else 
      APrinterMetrics.PhysicalWidth := Printer.PageWidth;
    if Printer.PageHeight = 0 then
      APrinterMetrics.PhysicalHeight := Round(FCustomHeight * APrinterMetrics.PPIY / InchAsMM)
    else 
      APrinterMetrics.PhysicalHeight := Printer.PageHeight;
    APrinterMetrics.MarginLeft := Printer.PaperSize.PaperRect.WorkRect.Left;
    APrinterMetrics.MarginTop := Printer.PaperSize.PaperRect.WorkRect.Top;
    APrinterMetrics.MarginRight := APrinterMetrics.MarginLeft;
    APrinterMetrics.MarginBottom := APrinterMetrics.MarginTop;
    APrinterMetrics.ClientWidth := APrinterMetrics.PhysicalWidth - (APrinterMetrics.MarginLeft + APrinterMetrics.MarginRight);
    APrinterMetrics.ClientHeight := APrinterMetrics.PhysicalHeight - (APrinterMetrics.MarginTop + APrinterMetrics.MarginBottom);
{$endif}
  except
    on E: Exception do
    begin
      LoadDefaultMetrics(APrinterMetrics);
      if not WarningDisplayed then
      begin
        WarningDisplayed := True;
      end;
    end;
  end;
end;

function TRLPrinterWrapper.Printers: TStrings;
begin
  PrintersNeeded;
  Result := FPrinters;
end;

function TRLPrinterWrapper.GetPrinterNames(AIndex: Integer): string;
begin
  Result := Token(Printers[AIndex], 1);
end;

function TRLPrinterWrapper.GetPrinterPorts(AIndex: Integer): string;
begin
  Result := Token(Printers[AIndex], 2);
end;

{$IfDef MSWINDOWS}
type
  TPrinterInfo = record
    PrinterName: string;
    PrinterPort: string;
    ServerName: string;
    ShareName: string;
  end;
 
function GetCurrentPrinterHandle: THandle;
const
  Defaults: TPrinterDefaults = (pDatatype: nil; pDevMode: nil; DesiredAccess: PRINTER_ACCESS_USE);
var
  ok: Boolean;
{$IfDef FPC}
  PDev: TPrinterDevice;
{$Else}
  hDeviceMode: THandle;
  Device, Driver, Port: array[0..255] of char;
{$EndIf}
begin
{$IfDef FPC}
  PDev := TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
  ok := OpenPrinter(PChar(PDev.Name), @Result, @Defaults);
{$Else}
  Printer.GetPrinter(Device, Driver, Port, hDeviceMode);
  ok := OpenPrinter(@Device, Result, @Defaults);
{$EndIf}

{$IfNDef DELPHI7_UP}
  if not ok then RaiseLastWin32Error;
{$Else}
  if not ok then RaiseLastOSError;
{$EndIf}
end;

function GetCurrentPrinterInfo: TPrinterInfo;
var
  pInfo: PPrinterInfo2;
  bytesNeeded: DWORD;
  hPrinter: THandle;
begin
  hPrinter := GetCurrentPrinterHandle;
  try
    GetPrinter(hPrinter, 2, nil, 0, @bytesNeeded);
    if bytesNeeded = 0 then
      bytesNeeded := 32768;
    pInfo := AllocMem(bytesNeeded);
    try
      GetPrinter(hPrinter, 2, {$IfDef FPC}PByte(pInfo){$Else}pInfo{$EndIf}, bytesNeeded, @bytesNeeded);
      Result.PrinterName := pInfo^.pPrinterName;
      Result.PrinterPort := pInfo^.pPortName;
      Result.ServerName := pInfo^.pServerName;
      Result.ShareName := pInfo^.pShareName;
    finally
      FreeMem(pInfo);
    end;
  finally
    ClosePrinter(hPrinter);
  end;
end;
{$endif}

procedure TRLPrinterWrapper.LoadPrintersList(APrinters: TStrings);
var
  I: Integer;
begin
  APrinters.Clear;
  for I := 0 to Printer.Printers.Count - 1 do
    APrinters.Add(Printer.Printers[I] + '|?');
end;

function TRLPrinterWrapper.AnyPrinter: Boolean;
begin
  if (Printer.PrinterIndex = -1) and (Printers.Count > 0) then
    Printer.PrinterIndex := 0;
  Result := (Printer.PrinterIndex <> -1);
end;

function TRLPrinterWrapper.GetPrinterPort: string;
{$IfDef MSWINDOWS}
var
  PInfo: TPrinterInfo;
{$EndIf}
begin
  //todo: verify the best way to get the printer port in LCL
  if not AnyPrinter then
    Result := ''
  else if (PrinterIndex < 0) or not (PrinterIndex < Printers.Count) then
    Result := ''
  else
  begin
    Result := Token(Printers[PrinterIndex], 2);

    {$IfDef MSWINDOWS}
    if Result = '?' then
    begin
      ReloadCurrentPrinter;
      PInfo := GetCurrentPrinterInfo;
      if PInfo.ServerName <> '' then
        Result := PInfo.ServerName + '\' + PInfo.ShareName
      else
        Result := PInfo.PrinterPort;
      Printers[PrinterIndex] := TokenListChanged(Printers[PrinterIndex], 2, Result);
    end;
    {$EndIf}
  end;  
end;

function TRLPrinterWrapper.GetPrinterName: string;
begin
  if not AnyPrinter then
    Result := ''
  else if (PrinterIndex < 0) or not (PrinterIndex < Printers.Count) then
    Result := ''
  else
    Result := Token(Printers[PrinterIndex], 1);
end;

procedure TRLPrinterWrapper.SetPrinterName(const APrinterName: string);
var
  N: string;
  I: Integer;
begin
  N := TruePrinterName(Token(APrinterName, 1));
  for I := 0 to Printers.Count - 1 do
    if AnsiSameText(TruePrinterName(Token(Printers[I], 1)), N) then
    begin
      SetPrinterIndex(I);
      Break;
    end;
end;

function TRLPrinterWrapper.GetPrinterIndex: Integer;
begin
  AnyPrinter;
  Result := Printer.PrinterIndex;
end;

procedure TRLPrinterWrapper.SetPrinterIndex(const Value: Integer);
begin
  PrintersNeeded;
  if (Value >= 0) and (Value < Printers.Count) then
    Printer.PrinterIndex := Value;
end;

procedure TRLPrinterWrapper.Refresh;
{$IfNDef FPC}
var
  savedprinterindex: Integer;
{$EndIf}
begin
  FreeAndNil(FPrinters);
{$IfDef FPC}
  Printer.Refresh;
{$Else}
  savedprinterindex := Printer.PrinterIndex;
  Printer.Refresh;
  Printer.PrinterIndex := -1;
  Printer.PrinterIndex := Min(savedprinterindex, Printer.Printers.Count - 1);
{$EndIf}
end;

function TRLPrinterWrapper.GetCopies: Integer;
begin
  Result := Printer.Copies;
end;

procedure TRLPrinterWrapper.SetCopies(const Value: Integer);
begin
  Printer.Copies := Value;
end;

function TRLPrinterWrapper.SupportsDuplex: Boolean;
{$IfNDef FPC}
var
  Device, Driver, Port: array[0..255] of Char;
  DriverHandle: THandle;
begin
  Printer.GetPrinter(Device, Driver, Port, DriverHandle);
  Result := WinSpool.DeviceCapabilities(Device, Port, DC_DUPLEX, nil, nil) <> 0;
end;
{$Else FPC}
{$IfDef MSWINDOWS}
var
  PDev: TPrinterDevice;
begin
  Result := Printer.PrinterIndex <> -1;
  if Result then
  begin
    PDev := TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
    Result := DeviceCapabilities(PChar(PDev.Device), PChar(PDev.Port), DC_DUPLEX, nil, nil) <> 0;
  end;
end;
{$Else MSWINDOWS}
begin
  //note: Printer Duplex nor supported in non windows
  Result := False;
end;
{$EndIf MSWINDOWS}
{$EndIf FPC}

function TRLPrinterWrapper.GetDuplex: Boolean;
{$IfNDef FPC}
var
  Device, Driver, Port: array[0..80] of Char;
  DriverHandle: THandle;
  DevModeRef: PDeviceMode;
begin
  Result := False;

  Printer.GetPrinter(Device, Driver, Port, DriverHandle);
  if DriverHandle <> 0 then
  begin
    DevModeRef := GlobalLock(DriverHandle);
    if DevModeRef <> nil then
    try
      Result := DevModeRef^.dmDuplex <> DMDUP_SIMPLEX;
    finally
      GlobalUnlock(DriverHandle);
    end;
  end;
end;
{$Else FPC}
{$IfDef MSWINDOWS}
var
  PDev: TPrinterDevice;
  DevModeRef: PDeviceModeW;
begin
  PDev := TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
  DevModeRef := PDev.DevModeW;
  if DevModeRef <> nil then
    Result := DevModeRef^.dmDuplex <> DMDUP_SIMPLEX;
end;
{$Else MSWINDOWS}
begin
  //note: Printer Duplex nor supported in non windows
  Result := False;
end;
{$EndIf MSWINDOWS}
{$EndIf FPC}

procedure TRLPrinterWrapper.SetDuplex(const Value: Boolean);
{$IfNDef FPC}
var
  Device, Driver, Port: array[0..80] of Char;
  DriverHandle: THandle;
  DevModeRef: PDeviceMode;
begin
  Printer.GetPrinter(Device, Driver, Port, DriverHandle);
  if DriverHandle <> 0 then
  begin
    DevModeRef := GlobalLock(DriverHandle);
    if DevModeRef <> nil then
    try
      if Value then
      begin
        DevModeRef^.dmDuplex := DMDUP_VERTICAL;
        DevModeRef^.dmFields := DevModeRef^.dmFields or DM_DUPLEX;
      end
      else
      begin
        DevModeRef^.dmDuplex := DMDUP_SIMPLEX;
        DevModeRef^.dmFields := DevModeRef^.dmFields and not DWORD(DM_DUPLEX);
      end;
    finally
      GlobalUnlock(DriverHandle);
    end;
  end;
end;
{$Else FPC}
{$IfDef MSWINDOWS}
var
  PDev: TPrinterDevice;
  DevModeRef: PDeviceModeW;
begin
  PDev := TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
  DevModeRef := PDev.DevModeW;

  if DevModeRef <> nil then
  begin
    if Value then
    begin
      DevModeRef^.dmDuplex := DMDUP_VERTICAL;
      DevModeRef^.dmFields := DevModeRef^.dmFields or DM_DUPLEX;
    end
    else
    begin
      DevModeRef^.dmDuplex := DMDUP_SIMPLEX;
      DevModeRef^.dmFields := DevModeRef^.dmFields and not DWORD(DM_DUPLEX);
    end;
  end;
end;
{$Else MSWINDOWS}
begin
  //note: Printer Duplex nor supported in non windows
end;
{$EndIf MSWINDOWS}
{$EndIf FPC}

function TRLPrinterWrapper.GetOddEven: TRLPrintOddEvenPages;
begin
  Result := FOddEven;
end;

procedure TRLPrinterWrapper.SetOddEven(const Value: TRLPrintOddEvenPages);
begin
  FOddEven := Value;
end;

function TRLPrinterWrapper.SetupEnabled: Boolean;
begin
{$ifdef MSWINDOWS}
  Result := True;
{$else}
  Result := False;
{$endif}
end;

function TRLPrinterWrapper.ExecuteSetup(WindowHandle: THandle): Boolean;
{$IfNDef FPC}
begin
  Result := false;
  CreateDeviceMode(WindowHandle);
  Result := true;
end;
{$else}
begin
  Result := False;
  {$IFDEF MSWINDOWS}
    TWinPrinter(Printer).AdvancedProperties;
    Result := True;
  {$ELSE}
	raise Exception.Create(Format(GetLocalizeStr(LocaleStrings.LS_NotImplemented), ['Printer.AdvancedProperties']));
  {$ENDIF}
end;
{$EndIf}

{$IfNDef FPC}
procedure TRLPrinterWrapper.ClearSavedDevMode;
begin
  if Assigned(SavedPrinterSettings.DevMode) then
    FreeMem(SavedPrinterSettings.DevMode);

  SavedPrinterSettings.PrinterName := '';
  SavedPrinterSettings.Bytes := 0;
end;

procedure TRLPrinterWrapper.CreateDeviceMode(WindowHandle: HWND);
var
  Device, Driver, Port: array[0..MAX_PATH] of char;
  PrinterHandle: THandle;
  OldModeHandle: THandle; //hDeviceMode
  OldDeviceMode: PDeviceMode; //PrinterDevMode
  NewModeHandle: THandle;
  NewDeviceMode: PDeviceMode;
  dummy: PDeviceMode;
  BytesNeeded: Integer;
begin
  Printer.GetPrinter(Device, Driver, Port, OldModeHandle);
  if WinSpool.OpenPrinter(@Device, PrinterHandle, nil) then
  try
    OldDeviceMode := GlobalLock(OldModeHandle);
    try
      dummy := nil;
      BytesNeeded := WinSpool.DocumentProperties(WindowHandle, PrinterHandle, Device, dummy^, dummy^, 0);
      if BytesNeeded < 0 then
        raise Exception.Create('The call to DocumentProperties failed.');
      NewModeHandle := GlobalAlloc(GHND, BytesNeeded);
      NewDeviceMode := GlobalLock(NewModeHandle);
      try
        case WinSpool.DocumentProperties(WindowHandle, PrinterHandle, Device, NewDeviceMode^, OldDeviceMode^, DM_OUT_BUFFER or DM_IN_PROMPT or DM_IN_BUFFER) of
          IDOK: begin
            Printer.SetPrinter(Device, Driver, Port, NewModeHandle);
            ClearSavedDevMode;
            SavedPrinterSettings.DevMode := GetMemory(BytesNeeded);
            SavedPrinterSettings.Bytes := BytesNeeded;
            SavedPrinterSettings.PrinterName := Printer.Printers[Printer.PrinterIndex];
            CopyMemory(SavedPrinterSettings.DevMode, NewDeviceMode, BytesNeeded);
          end;
          IDCANCEL: // do nothing
          else raise Exception.Create('Something went wrong with setting the new printer parameters.');
        end;
      finally
        GlobalUnlock(NewModeHandle);
      end;
    finally
      GlobalUnlock(OldModeHandle);
    end;
  finally
    WinSpool.ClosePrinter(PrinterHandle);
  end;
end;

procedure TRLPrinterWrapper.ApplyDeviceMode;
var
  Device, Driver, Port: array[0..MAX_PATH] of char;
  PrinterHandle: THandle;
  OldModeHandle: THandle; //hDeviceMode
  NewModeHandle: NativeUint;
  NewDeviceMode: PDeviceMode;
begin
  if Printer.Printers[Printer.PrinterIndex] = SavedPrinterSettings.PrinterName then
  begin
    Printer.GetPrinter(Device, Driver, Port, OldModeHandle);
    if WinSpool.OpenPrinter(@Device, PrinterHandle, nil) then
    try
      try
        NewModeHandle := GlobalAlloc(GHND, SavedPrinterSettings.Bytes);
        NewDeviceMode := GlobalLock(NewModeHandle);
        try
          case WinSpool.DocumentProperties(0, PrinterHandle, Device, NewDeviceMode^, SavedPrinterSettings.DevMode^, DM_OUT_BUFFER or DM_IN_BUFFER) of
            IDOK: begin
              Printer.SetPrinter(Device, Driver, Port, NewModeHandle);
            end;
            else raise Exception.Create('Something went wrong with setting the new printer parameters.');
          end;
        finally
          GlobalUnlock(NewModeHandle);
        end;
      finally
        GlobalUnlock(OldModeHandle);
      end;
    finally
      WinSpool.ClosePrinter(PrinterHandle);
    end;
  end;
end;
{$EndIf}

function TRLPrinterWrapper.GetPrinterDisplays(AIndex: Integer): string;
var
  Port: string;
begin
  if AIndex = -1 then
    Result := ''
  else
  begin
    Result := PrinterNames[AIndex];
    Port := PrinterPorts[AIndex];
    if (Port <> Result) and (Port <> '?') then
      Result := GetLocalizeStr(Result + ' ' + LocaleStrings.LS_AtStr + ' ' + Port);
  end;
end;

procedure TRLPrinterWrapper.LoadDefaultMetrics(
  var APrinterMetrics: TRLPrinterMetrics);
begin
  // Defaulting as "HP LaserJet Plus" metrics
  APrinterMetrics.PPIX := 300;
  APrinterMetrics.PPIY := 300;
  APrinterMetrics.PhysicalWidth := 2550;
  APrinterMetrics.PhysicalHeight := 3300;
  APrinterMetrics.MarginLeft := 75;
  APrinterMetrics.MarginTop := 75;
  APrinterMetrics.MarginRight := APrinterMetrics.MarginLeft;
  APrinterMetrics.MarginBottom := APrinterMetrics.MarginTop;
  APrinterMetrics.ClientWidth := APrinterMetrics.PhysicalWidth - (APrinterMetrics.MarginLeft + APrinterMetrics.MarginRight);
  APrinterMetrics.ClientHeight := APrinterMetrics.PhysicalHeight - (APrinterMetrics.MarginTop + APrinterMetrics.MarginBottom);
end;

initialization
  WarningDisplayed := False;
  RLPrinterInstance := nil;

finalization
  if Assigned(RLPrinterInstance) then
    RLPrinterInstance.free;

end.
