{ Projeto: FortesReport Community Edition                                      }
{ � um poderoso gerador de relat�rios dispon�vel como um pacote de componentes }
{ para Delphi. Em FortesReport, os relat�rios s�o constitu�dos por bandas que  }
{ t�m fun��es espec�ficas no fluxo de impress�o. Voc� definir agrupamentos     }
{ subn�veis e totais simplesmente pela rela��o hier�rquica entre as bandas.    }
{ Al�m disso possui uma rica paleta de Componentes                             }
{                                                                              }
{ Direitos Autorais Reservados(c) Copyright � 1999-2015 Fortes Inform�tica     }
{                                                                              }
{ Colaboradores nesse arquivo: Ronaldo Moreira                                 }
{                              M�rcio Martins                                  }
{                              R�gys Borges da Silveira                        }
{                              Juliomar Marchetti                              }
{                                                                              }
{  Voc� pode obter a �ltima vers�o desse arquivo na pagina do Projeto          }
{  localizado em                                                               }
{ https://github.com/fortesinformatica/fortesreport-ce                         }
{                                                                              }
{  Para mais informa��es voc� pode consultar o site www.fortesreport.com.br ou }
{  no Yahoo Groups https://groups.yahoo.com/neo/groups/fortesreport/info       }
{                                                                              }
{  Esta biblioteca � software livre; voc� pode redistribu�-la e/ou modific�-la }
{ sob os termos da Licen�a P�blica Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a vers�o 2.1 da Licen�a, ou (a seu crit�rio) }
{ qualquer vers�o posterior.                                                   }
{                                                                              }
{  Esta biblioteca � distribu�da na expectativa de que seja �til, por�m, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia impl�cita de COMERCIABILIDADE OU      }
{ ADEQUA��O A UMA FINALIDADE ESPEC�FICA. Consulte a Licen�a P�blica Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICEN�A.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Voc� deve ter recebido uma c�pia da Licen�a P�blica Geral Menor do GNU junto}
{ com esta biblioteca; se n�o, escreva para a Free Software Foundation, Inc.,  }
{ no endere�o 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Voc� tamb�m pode obter uma copia da licen�a em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* xx/xx/xxxx:  Autor...
|* - Descri��o...
******************************************************************************}

{$I RLReport.inc}

{@unit RLPrinters - Implementa��o do wrapper para o objeto Printer. }

unit RLPrinters;

interface

uses
  Classes, SysUtils, Math, 
{$ifndef LINUX}
  Windows, WinSpool, Graphics, Dialogs, Messages, Printers, 
{$else}
  QGraphics, QDialogs, QPrinters, Types, 
{$endif}
  RLConsts, RLTypes, RLUtils;

type
  TRLPrintOddEvenPages = (odOddPagesOnly, odEvenPagesOnly, odAllPages);

  TRLPrinterWrapper = class
  private
    FPrinters: TStrings;
    FCustomWidth: Double;
    FCustomHeight: Double;
    FOddEven: TRLPrintOddEvenPages;
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
    function ExecuteSetup: Boolean;
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

{$ifndef LINUX}
type
  TPrinterEx = class(TPrinter) end;
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
end;

destructor TRLPrinterWrapper.Destroy;
begin
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
  Printer.Title := ATitle;
  Printer.BeginDoc;
{$ifndef LINUX}
  Printer.Canvas.Font.PixelsPerInch := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
{$else}
  Printer.Canvas.Font.PixelsPerInch := Printer.YDPI;
{$endif}
end;

procedure TRLPrinterWrapper.EndDoc;
var
  savedprinterindex: Integer;
begin
  // Na CLX o m�todo EndDoc destroi o objeto PrinterAdapter.
  // Com isso se perde a refer�ncia para o OutputDevice escolhido. Nele se baseia o nosso PrinterIndex.
  // Temos que salv�-lo aqui e restaurar depois do EndDoc.
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

{$ifndef LINUX}
function ConvOrientation(AOrientation: TRLSystemOrientation): TPrinterOrientation;
begin
  if AOrientation = DMORIENT_PORTRAIT then
    Result := Printers.poPortrait
  else
    Result := Printers.poLandscape;
end; 
procedure TRLPrinterWrapper.SelectSystemPaperSize(APaperSize: TRLSystemPaperType;
  APaperWidthMM, APaperHeightMM: Double; AOrientation: TRLSystemOrientation; ASetPaperBin: Boolean);
var
  FCapabilities: longint;
  FDeviceMode: PDeviceMode;
  FDriverHandle: THandle;
  FDevice: PChar;
  FDriver: PChar;
  FPort: PChar;
  FPrinterName: string;
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
      Printer.GetPrinter(FDevice, FDriver, FPort, FDriverHandle);
      if FDriverHandle = 0 then
      begin
        ReloadCurrentPrinter;
        Printer.GetPrinter(FDevice, FDriver, FPort, FDriverHandle);
      end;
      if FDriverHandle = 0 then
        Abort;
      FDeviceMode := GlobalLock(FDriverHandle);
      if FDeviceMode = nil then
        Abort;
      try
        FCapabilities := FDeviceMode^.dmFields;
        //if Able(DM_PAPERSIZE) then // n�o funciona no Win2k/XP
        FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_PAPERSIZE;
        FDeviceMode^.dmPaperSize := APaperSize;
        // muda o tamanho do papel
        if APaperSize = DMPAPER_USER then
        begin
          if APaperWidthMM <> 0 then
          begin
            FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_PAPERWIDTH;
            FDeviceMode^.dmPaperWidth := Round(APaperWidthMM * 10);
          end;
          if APaperHeightMM <> 0 then
          begin
            FDeviceMode^.dmFields := FDeviceMode^.dmFields or DM_PAPERLENGTH;
            FDeviceMode^.dmPaperLength := Round(APaperHeightMM * 10);
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
  PostMessage(HWND_BROADCAST, WM_DEVMODECHANGE, 0, Integer(@FPrinterName[1]));
end;
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
{$else}
procedure TRLPrinterWrapper.SelectSystemPaperSize(APaperSize: TRLSystemPaperType;
  APaperWidthMM, APaperHeightMM: Double; AOrientation: TRLSystemOrientation; ASetPaperBin: Boolean);
begin
  Printer.PrintAdapter.PageSize := APaperSize;
  // adapta��o de dimens�es para o componente
  if APaperSize = UserPaperCode then
  begin
    {
    if aPaperWidthMM<>0 then
      Printer.PrintAdapter.PageWidth:=aPaperWidthMM;
    if aPaperHeightMM<>0 then
      Printer.PrintAdapter.PageHeight:=aPaperHeightMM;
    }
  end;
  // orienta��o do papel
  Printer.Orientation := AOrientation;
end;
procedure TRLPrinterWrapper.GetBinNames(AStringList: TStrings);
begin
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
{$ifndef LINUX}
var
  dc: HDC;
{$endif}
begin
  try
    if not AnyPrinter then
      raise Exception.Create('Nenhuma impressora selecionada.');
{$ifndef LINUX}
    dc := Printer.Handle;
    if dc = 0 then
    begin
      ReloadCurrentPrinter;
      dc := Printer.Handle;
      if dc = 0 then
        raise Exception.Create('Handle n�o dispon�vel.');
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
    APrinterMetrics.MarginLeft := Printer.Margins.CX;
    APrinterMetrics.MarginTop := Printer.Margins.CY;
    APrinterMetrics.MarginRight := APrinterMetrics.MarginLeft;
    APrinterMetrics.MarginBottom := APrinterMetrics.MarginTop;
    APrinterMetrics.ClientWidth := APrinterMetrics.PhysicalWidth - (APrinterMetrics.MarginLeft + APrinterMetrics.MarginRight);
    APrinterMetrics.ClientHeight := APrinterMetrics.PhysicalHeight - (APrinterMetrics.MarginTop + APrinterMetrics.MarginBottom);
{$endif}
  except
    on E: Exception do
    begin
      // configura��o padr�o da "HP LaserJet Plus"
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
      //
      if not WarningDisplayed then
      begin
        ///ShowMessage(LocaleStrings.LS_LoadDefaultConfigStr+#13#13+'Mensagem: '+e.Message);
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

{$ifndef LINUX}
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
  Device, Driver, Port: array[0..255] of char;
  hDeviceMode: THandle;
  ok: Boolean;
begin
  Printer.GetPrinter(Device, Driver, Port, hDeviceMode);
  ok := OpenPrinter(@Device, Result, @Defaults);
{$ifdef DELPHI7}
  if not ok then RaiseLastOSError;
{$else}
{$ifdef DELPHI6}
  if not ok then RaiseLastWin32Error;
{$else}
  if not ok then RaiseLastOSError;
{$endif}
{$endif}
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
      GetPrinter(hPrinter, 2, pInfo, bytesNeeded, @bytesNeeded);
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
{$ifdef LINUX}
  S: string;
{$endif}
begin
  APrinters.Clear;
{$ifdef LINUX}
  with TStringList.Create do
    try
      LoadFromFile('/etc/printcap');
      for I := 0 to Count - 1 do
      begin
        S := Trim(Strings[I]);
        if (S <> '') and not (S[1] in [':', '|', '#']) then
          if Pos('|', S) > 0 then
            APrinters.Add(Copy(S, 1, Pos('|', S) - 1))
          else if Pos(':', S) > 0 then
            APrinters.Add(Copy(S, 1, Pos(':', S) - 1))
          else
            APrinters.Add(S);
      end;
    finally
      Free;
    end;
{$else}
  for I := 0 to Printer.Printers.Count - 1 do
    APrinters.Add(Printer.Printers[I] + '|?');
{$endif}
end;

function TRLPrinterWrapper.AnyPrinter: Boolean;
begin
{$ifdef LINUX}
  if (Printer.PrintAdapter.OutputDevice = '') and (Printers.Count > 0) then
    Printer.SetPrinter(PrinterNames[0]);
  Result := (Printer.PrintAdapter.OutputDevice <> '');
{$else}
  if (Printer.PrinterIndex = -1) and (Printers.Count > 0) then
    Printer.PrinterIndex := 0;
  Result := (Printer.PrinterIndex <> -1);
{$endif}
end;

function TRLPrinterWrapper.GetPrinterPort: string;
{$ifndef LINUX}
var
  PInfo: TPrinterInfo;
{$endif}
begin
  if not AnyPrinter then
    Result := ''
  else if (PrinterIndex < 0) or not (PrinterIndex < Printers.Count) then
    Result := ''
  else
  begin
    Result := Token(Printers[PrinterIndex], 2);
{$ifndef LINUX}
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
{$endif}
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
{$ifdef LINUX}
var
  I: Integer;
{$endif}
begin
  AnyPrinter;
{$ifdef LINUX}
  Result := -1;
  for I := 0 to Printer.Printers.Count - 1 do
    if AnsiSameText(TruePrinterName(PrinterNames[I]), Printer.OutputDevice) then
    begin
      Result := I;
      Break;
    end;
{$else}
  Result := Printer.PrinterIndex;
{$endif}
end;

procedure TRLPrinterWrapper.SetPrinterIndex(const Value: Integer);
begin
  PrintersNeeded;
  if (Value >= 0) and (Value < Printers.Count) then
{$ifndef LINUX}
    Printer.PrinterIndex := Value;
{$else}
    Printer.SetPrinter(PrinterNames[Value]);
{$endif}
end;

procedure TRLPrinterWrapper.Refresh;
{$ifndef LINUX}
var
  savedprinterindex: Integer;
{$endif}
begin
  if Assigned(FPrinters) then
    FPrinters.Free;
  FPrinters := nil;
{$ifndef LINUX}
  savedprinterindex := Printer.PrinterIndex;
  Printer.Refresh;
  Printer.PrinterIndex := -1;
  Printer.PrinterIndex := Min(savedprinterindex, Printer.Printers.Count - 1);
{$endif}
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
var
  Device, Driver, Port: array[0..255] of Char;
  DriverHandle: THandle;
begin
  Printer.GetPrinter(Device, Driver, Port, DriverHandle);
  Result := WinSpool.DeviceCapabilities(Device, Port, DC_DUPLEX, nil, nil) <> 0;
end;

function TRLPrinterWrapper.GetDuplex: Boolean;
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

procedure TRLPrinterWrapper.SetDuplex(const Value: Boolean);
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
{$ifndef LINUX}
  Result := True;
{$else}
  Result := False;
{$endif}
end;

function TRLPrinterWrapper.ExecuteSetup: Boolean;
{$ifndef LINUX}
var
  PrinterHandle: THandle;
  PrinterInfo2: PPrinterInfo2;
  PrinterDefaults: TPrinterDefaults;
  PrinterResult: Boolean;
  BytesNeeded: DWORD;
  PrinterDevMode: PDeviceMode;
  DocFlags: Integer;
{$endif}
begin
  Result := False;
  Refresh;
{$ifndef LINUX}
  PrinterHandle := 0;
	BytesNeeded := 0;
  FillChar(PrinterDefaults, SizeOf(PrinterDefaults), 0);
  {Fred - 08/04/2008 - Corre��o de chamada de di�logo de impress�o quando a impressora
   est� em outro computador da rede }
  //Open printer handle (on Windows NT, you need full-access because you will eventually use SetPrinter)
  PrinterDefaults.DesiredAccess := PRINTER_ALL_ACCESS;
  PrinterResult := WinSpool.OpenPrinter(PChar(PrinterName), PrinterHandle, @PrinterDefaults);
  //Se o computador n�o permite acesso completo � impressora tento usar o acesso m�nimo 
  if GetLastError=ERROR_ACCESS_DENIED then
  begin
    PrinterDefaults.DesiredAccess := PRINTER_ACCESS_USE;
    PrinterResult := WinSpool.OpenPrinter(PChar(PrinterName), PrinterHandle, @PrinterDefaults);
  end;
  if not PrinterResult or (PrinterHandle = 0) then
    Exit;
  try
    // The first GetPrinter tells you how big the buffer should be in
    // order to hold all of PRINTER_INFO_2. Note that this should fail with
    // ERROR_INSUFFICIENT_BUFFER.  If GetPrinter fails for any other reason
    // or dwNeeded isn't set for some reason, then there is a problem...
    PrinterResult := WinSpool.GetPrinter(PrinterHandle, 2, nil, 0, @BytesNeeded);
    if not PrinterResult and ((GetLastError <> ERROR_INSUFFICIENT_BUFFER) or (BytesNeeded = 0)) then
      Exit;
    // Allocate enough space for PRINTER_INFO_2...
    GetMem(PrinterInfo2, BytesNeeded);
    if PrinterInfo2 = nil then
      Exit;
    try
      // The second GetPrinter fills in all the current settings, so all you
      // need to do is modify what you're interested in...
      PrinterResult := WinSpool.GetPrinter(PrinterHandle, 2, PrinterInfo2, BytesNeeded, @BytesNeeded);
      if not PrinterResult then
        Exit;
      // If GetPrinter didn't fill in the DEVMODE, try to get it by calling
      // DocumentProperties...
      PrinterDevMode := nil;
      try
        if PrinterInfo2.pDevMode = nil then
        begin
          BytesNeeded := WinSpool.AdvancedDocumentProperties(0, PrinterHandle, PChar(PrinterName), nil, nil);
          if BytesNeeded <= 0 then
            Exit;
          GetMem(PrinterDevMode, BytesNeeded);
          if PrinterDevMode = nil then
            Exit;
          DocFlags := AdvancedDocumentProperties(0, PrinterHandle, PChar(PrinterName), PrinterDevMode, nil);
          if (DocFlags <> IDOK) or (PrinterDevMode = nil) then
            Exit;
          PrinterInfo2.pDevMode := PrinterDevMode;
        end;
        // finalmente chama o di�logo...
        WinSpool.DocumentProperties(0, PrinterHandle, PChar(PrinterName), PrinterDevMode^,
          PrinterDevMode^, DM_PROMPT or DM_COPY);
        Result := True;
      finally
        if PrinterDevMode <> nil then
          FreeMem(PrinterDevMode);
      end;
    finally
      FreeMem(PrinterInfo2);
    end;
  finally
    WinSpool.ClosePrinter(PrinterHandle);
  end;
{$endif}
end;

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
      Result := Result + ' ' + LocaleStrings.LS_AtStr + ' ' + Port;
  end;
end;

initialization
  WarningDisplayed := False;
  RLPrinterInstance := nil;

finalization
  if Assigned(RLPrinterInstance) then
    RLPrinterInstance.free;

end.

