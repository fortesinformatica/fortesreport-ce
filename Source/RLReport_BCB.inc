{******************************************************************************}
{ Projeto: FortesReport Community Edition                                      }
{ É um poderoso gerador de relatórios disponível como um pacote de componentes }
{ para Delphi. Em FortesReport, os relatórios são constituídos por bandas que  }
{ têm funções específicas no fluxo de impressão. Você definir agrupamentos     }
{ subníveis e totais simplesmente pela relação hierárquica entre as bandas.    }
{ Além disso possui uma rica paleta de Componentes                             }
{                                                                              }
{ Direitos Autorais Reservados(c) Copyright © 1999-2015 Fortes Informática     }
{                                                                              }
{ Colaboradores nesse arquivo: Márcio Martins                                  }
{                              Ronaldo Moreira                                 }
{                              Régys Borges da Silveira                        }
{                              Juliomar Marchetti                              }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto         }
{  localizado em                                                               }
{ https://github.com/fortesinformatica/fortesreport-ce                         }
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
|* 01/04/2021:  Marcelo Augusto Cicogna
|* - Diretivas para compilação em IDE C++Builder (sem Delphi personality).
|*   A ideia é atualizar as diretivas DELPHIX_UP em função de BCBX. Caso 
|*   contrário, seria necessário atualizar o código do FortesReport para 
|*   considerar DELPHIX_UP e BCBX_UP.
******************************************************************************}

{------------------------------------------------------------------------------}
{ DELPHIX_UP from BCBX mappings                                    }
{------------------------------------------------------------------------------}

{$IF Defined(DELPHI27) OR Defined (BCB27)} {$DEFINE DELPHI27_UP} {$IFEND}
{$IF Defined(DELPHI26) OR Defined (BCB26)} {$DEFINE DELPHI26_UP} {$IFEND}
{$IF Defined(DELPHI25) OR Defined (BCB25)} {$DEFINE DELPHI25_UP} {$IFEND}
{$IF Defined(DELPHI24) OR Defined (BCB24)} {$DEFINE DELPHI24_UP} {$IFEND}
{$IF Defined(DELPHI23) OR Defined (BCB23)} {$DEFINE DELPHI23_UP} {$IFEND}
{$IF Defined(DELPHI22) OR Defined (BCB22)} {$DEFINE DELPHI22_UP} {$IFEND}
{$IF Defined(DELPHI21) OR Defined (BCB21)} {$DEFINE DELPHI21_UP} {$IFEND}
{$IF Defined(DELPHI20) OR Defined (BCB20)} {$DEFINE DELPHI20_UP} {$IFEND}
{$IF Defined(DELPHI19) OR Defined (BCB19)} {$DEFINE DELPHI19_UP} {$IFEND}
{$IF Defined(DELPHI18) OR Defined (BCB18)} {$DEFINE DELPHI18_UP} {$IFEND}
{$IF Defined(DELPHI17) OR Defined (BCB17)} {$DEFINE DELPHI17_UP} {$IFEND}
{$IF Defined(DELPHI16) OR Defined (BCB16)} {$DEFINE DELPHI16_UP} {$IFEND}
{$IF Defined(DELPHI15) OR Defined (BCB15)} {$DEFINE DELPHI15_UP} {$IFEND}
{$IF Defined(DELPHI14) OR Defined (BCB14)} {$DEFINE DELPHI14_UP} {$IFEND}
{$IF Defined(DELPHI12) OR Defined (BCB12)} {$DEFINE DELPHI12_UP} {$IFEND}
{$IF Defined(DELPHI11) OR Defined (BCB11)} {$DEFINE DELPHI11_UP} {$IFEND}
{$IF Defined(DELPHI10) OR Defined (BCB10)} {$DEFINE DELPHI10_UP} {$IFEND}
{$IFDEF DELPHI9}  {$DEFINE DELPHI9_UP}  {$ENDIF}
{$IFDEF DELPHI8}  {$DEFINE DELPHI8_UP}  {$ENDIF}
{$IFDEF DELPHI7}  {$DEFINE DELPHI7_UP}  {$ENDIF}
{$IF Defined(DELPHI6) OR Defined (BCB6)}  {$DEFINE DELPHI6_UP}  {$IFEND}
{$IF Defined(DELPHI5) OR Defined (BCB5)}  {$DEFINE DELPHI5_UP}  {$IFEND}
{$IFDEF DELPHI4}  {$DEFINE DELPHI4_UP}  {$ENDIF}
{$IFDEF DELPHI3}  {$DEFINE DELPHI3_UP}  {$ENDIF}
{$IFDEF DELPHI2}  {$DEFINE DELPHI2_UP}  {$ENDIF}
{$IFDEF DELPHI1}  {$DEFINE DELPHI1_UP}  {$ENDIF}

{------------------------------------------------------------------------------}
{ DELPHIX_UP from DELPHIX_UP mappings                                          }
{------------------------------------------------------------------------------}

{$IFDEF DELPHI27_UP}
  {$DEFINE DELPHI26_UP}
{$ENDIF}

{$IFDEF DELPHI26_UP}
  {$DEFINE DELPHIX_RIO_UP} // synonym to DELPHI26_UP
  {$DEFINE DELPHI25_UP}
{$ENDIF}

{$IFDEF DELPHI25_UP}
  {$DEFINE DELPHIX_TOKYO_UP} // synonym to DELPHI25_UP
  {$DEFINE DELPHI24_UP}
{$ENDIF}

{$IFDEF DELPHI24_UP}
  {$DEFINE DELPHIX_BERLIN_UP} // synonym to DELPHI24_UP
  {$DEFINE DELPHI23_UP}
{$ENDIF}

{$IFDEF DELPHI23_UP}
  {$DEFINE DELPHIX_SEATTLE_UP} // synonym to DELPHI23_UP
  {$DEFINE DELPHI22_UP}
{$ENDIF}

{$IFDEF DELPHI22_UP}
  {$DEFINE DELPHIXE8_UP} // synonym to DELPHI22_UP
  {$DEFINE DELPHI21_UP}
{$ENDIF}

{$IFDEF DELPHI21_UP}
  {$DEFINE DELPHIXE7_UP} // synonym to DELPHI21_UP
  {$DEFINE DELPHI20_UP}
{$ENDIF}

{$IFDEF DELPHI20_UP}
  {$DEFINE DELPHIXE6_UP} // synonym to DELPHI20_UP
  {$DEFINE DELPHI19_UP}
{$ENDIF}

{$IFDEF DELPHI19_UP}
  {$DEFINE DELPHIXE5_UP} // synonym to DELPHI19_UP
  {$DEFINE DELPHI18_UP}
{$ENDIF}

{$IFDEF DELPHI18_UP}
  {$DEFINE DELPHIXE4_UP} // synonym to DELPHI18_UP
  {$DEFINE DELPHI17_UP}
{$ENDIF}

{$IFDEF DELPHI17_UP}
  {$DEFINE DELPHIXE3_UP} // synonym to DELPHI17_UP
  {$DEFINE DELPHI16_UP}
{$ENDIF}

{$IFDEF DELPHI16_UP}
  {$DEFINE DELPHIXE2_UP} // synonym to DELPHI16_UP
  {$DEFINE DELPHI15_UP}
{$ENDIF}

{$IFDEF DELPHI15_UP}
  {$DEFINE DELPHIXE_UP} // synonym to DELPHI15_UP
  {$DEFINE DELPHI14_UP}
{$ENDIF}

{$IFDEF DELPHI14_UP}
  {$DEFINE DELPHI2010_UP} // synonym to DELPHI14_UP
  {$DEFINE DELPHI12_UP}
{$ENDIF}

{$IFDEF DELPHI12_UP}
  {$DEFINE DELPHI2009_UP} // synonym to DELPHI12_UP
  {$DEFINE DELPHI11_UP}
{$ENDIF}

{$IFDEF DELPHI11_UP}
  {$DEFINE DELPHI2007_UP} // synonym to DELPHI11_UP
  {$DEFINE DELPHI10_UP}
{$ENDIF}

{$IFDEF DELPHI10_UP}
  {$DEFINE DELPHI2006_UP} // synonym to DELPHI10_UP
  {$DEFINE DELPHI9_UP}
{$ENDIF}

{$IFDEF DELPHI9_UP}
  {$DEFINE DELPHI2005_UP} // synonym to DELPHI9_UP
  {$DEFINE DELPHI8_UP}
{$ENDIF}

{$IFDEF DELPHI8_UP} {$DEFINE DELPHI7_UP} {$ENDIF}
{$IFDEF DELPHI7_UP} {$DEFINE DELPHI6_UP} {$ENDIF}
{$IFDEF DELPHI6_UP} {$DEFINE DELPHI5_UP} {$ENDIF}
{$IFDEF DELPHI5_UP} {$DEFINE DELPHI4_UP} {$ENDIF}
{$IFDEF DELPHI4_UP} {$DEFINE DELPHI3_UP} {$ENDIF}
{$IFDEF DELPHI3_UP} {$DEFINE DELPHI2_UP} {$ENDIF}
{$IFDEF DELPHI2_UP} {$DEFINE DELPHI1_UP} {$ENDIF}
