{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FortesReportCE_Lazarus;

interface

uses
  RLPrinters, RLReport, RLTypes, RLUtils, RLBarcode, RlCompilerConsts, 
  RLComponentFactory, RLConsts, RLFilters, RLMetaFile, RLParser, RLPreview, 
  RLRichText, RLMetaLCL, RLReg, RLSpoolFilter, RLAbout, RLFeedBack, 
  RLFindDialog, RLPreviewForm, RLPrintDialog, RLSaveDialog, RLRichFilter, 
  RLPDFFilter, RLXLSFilter, RLHTMLFilter, RLDraftFilter, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RLReg', @RLReg.Register);
end;

initialization
  RegisterPackage('FortesReportCE_Lazarus', @Register);
end.
