{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FortesReportCE_Lazarus;

interface

uses
  RLPrinters, RLReport, RLTypes, RLUtils, RLAbout, RLBarcode, 
  RlCompilerConsts, RLComponentFactory, RLConsts, RLFeedBack, RLFilters, 
  RLFindDialog, RLHTMLFilter, RLMetaFile, RLMetaVCL, RLParser, RLPreview, 
  RLPreviewForm, RLPrintDialog, RLRichFilter, RLRichText, RLSaveDialog, 
  RLSpoolFilter, RLXLSFilter, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('FortesReportCE_Lazarus', @Register);
end.
