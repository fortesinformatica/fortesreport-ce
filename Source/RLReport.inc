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
|* xx/xx/xxxx:  Autor...
|* - Descrição...
******************************************************************************}

//Atualmente homologado somente para VCL, porém há uma trabalho sendo feito para
//que o mesmo código seja usado no Delphi ou no Lazarus
{$DEFINE VCL}
{.$DEFINE FMK}
{.$DEFINE KYLIX}
{.$DEFINE FREEPASCAL}

//O suporte ao Kylix foi decontinuado, porém teoricamente é suportado
{$IFDEF KYLIX1} {$DEFINE KYLIX} {$DEFINE KYLIX1_UP} {$DEFINE KYLIX1_DOWN} {$ENDIF}
{$IFDEF KYLIX2} {$DEFINE KYLIX} {$DEFINE KYLIX2_UP} {$DEFINE KYLIX2_DOWN} {$ENDIF}
{$IFDEF KYLIX3} {$DEFINE KYLIX} {$DEFINE KYLIX3_UP} {$DEFINE KYLIX3_DOWN} {$ENDIF}

//CPP Builder
{$IFDEF CBUILDER1} {$DEFINE CBUILDER} {$ENDIF}
{$IFDEF CBUILDER3} {$DEFINE CBUILDER} {$ENDIF}
{$IFDEF CBUILDER4} {$DEFINE CBUILDER} {$ENDIF}
{$IFDEF CBUILDER5} {$DEFINE CBUILDER} {$ENDIF}
{$IFDEF CBUILDER} {$DEFINE CPP} {$ENDIF}

// se é Linux, então é Kylix
{$IFDEF LINUX} {$DEFINE KYLIX} {$ENDIF}

{$IFDEF FREEPASCAL} {$DEFINE LAZARUS} {$ENDIF}
// se não é Kylix, entao é Delphi e Windows
{$IFNDEF KYLIX} {$DEFINE DELPHI} {$DEFINE WINDOWS} {$ENDIF}

// se é Kylix, então é CLX
{$IFDEF KYLIX}
  {$DEFINE CLX}
{$ENDIF}

// UPs compreendem versoes superiores (ex.: DELPHI6_UP = a partir do DELPHI6, inclusive)
// DOWNs compreendem versoes inferiores (DELPHI2007_DOWN = ate o DELPHI2007, inclusive)
{$IFDEF VER80} {$DEFINE DELPHI1} {$ENDIF}
{$IFDEF VER90} {$DEFINE DELPHI2} {$ENDIF}
{$IFDEF VER93} {$DEFINE CBUILDER1} {$ENDIF}
{$IFDEF VER100} {$DEFINE DELPHI3} {$ENDIF}
{$IFDEF VER110} {$DEFINE CBUILDER3} {$ENDIF}
{$IFDEF VER120} {$DEFINE DELPHI4} {$ENDIF}
{$IFDEF VER125} {$DEFINE CBUILDER4} {$ENDIF}
{$IFDEF VER130} {$DEFINE DELPHI5} {$ENDIF}
{$IFDEF VER135} {$DEFINE CBUILDER5} {$ENDIF}
{$IFDEF VER140} {$DEFINE DELPHI6} {$ENDIF}
{$IFDEF VER150} {$DEFINE DELPHI7} {$ENDIF}
{$IFDEF VER160} {$DEFINE DELPHI8} {$ENDIF}
{$IFDEF VER170} {$DEFINE DELPHI2005} {$ENDIF}
// confusao da borland: versoes 180, 185 e 190 representam o Delphi2007
// versoes 180 e 185 tambem sao Delphi6e 190 representam o Delphi2007
{$IFDEF VER180} {$DEFINE DELPHI2006} {$ENDIF}
{$IFDEF VER180} {$DEFINE DELPHI2007} {$ENDIF}
{$IFDEF VER185} {$UNDEF DELPHI2006} {$DEFINE DELPHI2007} {$ENDIF}
{$IFDEF VER190} {$DEFINE DELPHI2007} {$DEFINE DELPHI2007_DOTNET} {$DEFINE DOTNET} {$ENDIF}
{$IFDEF VER200} {$DEFINE DELPHI2009} {$ENDIF}
{$IFDEF VER210} {$DEFINE DELPHI2010} {$ENDIF}
{$IFDEF VER220} {$DEFINE DELPHIXE} {$ENDIF}
{$IFDEF VER230} {$DEFINE DELPHIXE2} {$ENDIF}
{$IFDEF VER240} {$DEFINE DELPHIXE3} {$ENDIF}
{$IFDEF VER250} {$DEFINE DELPHIXE4} {$ENDIF}
{$IFDEF VER260} {$DEFINE DELPHIXE5} {$ENDIF}
{$IFDEF VER270} {$DEFINE DELPHIXE6} {$ENDIF}
{$IFDEF VER280} {$DEFINE DELPHIXE7} {$ENDIF}
{$IFDEF VER290} {$DEFINE DELPHIXE8} {$ENDIF}
{$IFDEF VER300} {$DEFINE DELPHIXE9} {$ENDIF}

// UPs e DOWNs

{$IFDEF CBUILDER1} {$DEFINE CBUILDER} {$DEFINE CBUILDER1_UP} {$DEFINE CBUILDER1_DOWN} {$DEFINE DELPHI2_UP} {$DEFINE DELPHI2_DOWN} {$ENDIF}
{$IFDEF CBUILDER3} {$DEFINE CBUILDER} {$DEFINE CBUILDER3_UP} {$DEFINE CBUILDER3_DOWN} {$DEFINE DELPHI3_UP} {$DEFINE DELPHI3_DOWN} {$ENDIF}
{$IFDEF CBUILDER4} {$DEFINE CBUILDER} {$DEFINE DELPHI4_UP} {$DEFINE DELPHI4_DOWN} {$ENDIF}
{$IFDEF CBUILDER5} {$DEFINE CBUILDER} {$DEFINE DELPHI5_UP} {$DEFINE DELPHI5_DOWN} {$ENDIF}
{$IFDEF DELPHI1} {$DEFINE DELPHI1_UP} {$DEFINE DELPHI1_DOWN} {$ENDIF}
{$IFDEF DELPHI2} {$DEFINE DELPHI2_UP} {$DEFINE DELPHI2_DOWN} {$ENDIF}
{$IFDEF DELPHI3} {$DEFINE DELPHI3_UP} {$DEFINE DELPHI3_DOWN} {$ENDIF}
{$IFDEF DELPHI4} {$DEFINE DELPHI4_UP} {$DEFINE DELPHI4_DOWN} {$ENDIF}
{$IFDEF DELPHI5} {$DEFINE DELPHI5_UP} {$DEFINE DELPHI5_DOWN} {$ENDIF}
{$IFDEF DELPHI6} {$DEFINE DELPHI6_UP} {$DEFINE DELPHI6_DOWN} {$ENDIF}
{$IFDEF DELPHI7} {$DEFINE DELPHI7_UP} {$DEFINE DELPHI7_DOWN} {$ENDIF}
{$IFDEF DELPHI8} {$DEFINE DELPHI8_UP} {$DEFINE DELPHI8_DOWN} {$ENDIF}
{$IFDEF DELPHI2005} {$DEFINE DELPHI2005_UP} {$DEFINE DELPHI2005_DOWN} {$ENDIF}
{$IFDEF DELPHI2006} {$DEFINE DELPHI2006_UP} {$DEFINE DELPHI2006_DOWN} {$ENDIF}
{$IFDEF DELPHI2007} {$DEFINE DELPHI2007_UP} {$DEFINE DELPHI2007_DOWN} {$ENDIF}
{$IFDEF DELPHI2009} {$DEFINE DELPHI2009_UP} {$DEFINE DELPHI2009_DOWN} {$ENDIF}
{$IFDEF DELPHI2010} {$DEFINE DELPHI2010_UP} {$DEFINE DELPHI2010_DOWN} {$ENDIF}
{$IFDEF DELPHIXE} {$DEFINE DELPHIXE_UP} {$DEFINE DELPHIXE_DOWN} {$ENDIF}
{$IFDEF DELPHIXE2} {$DEFINE DELPHIXE2_UP} {$DEFINE DELPHIXE2_DOWN} {$ENDIF}
{$IFDEF DELPHIXE3} {$DEFINE DELPHIXE3_UP} {$DEFINE DELPHIXE3_DOWN} {$ENDIF}
{$IFDEF DELPHIXE4} {$DEFINE DELPHIXE4_UP} {$DEFINE DELPHIXE4_DOWN} {$ENDIF}
{$IFDEF DELPHIXE5} {$DEFINE DELPHIXE5_UP} {$DEFINE DELPHIXE5_DOWN} {$ENDIF}
{$IFDEF DELPHIXE6} {$DEFINE DELPHIXE6_UP} {$DEFINE DELPHIXE6_DOWN} {$ENDIF}
{$IFDEF DELPHIXE7} {$DEFINE DELPHIXE7_UP} {$DEFINE DELPHIXE7_DOWN} {$ENDIF}
{$IFDEF DELPHIXE8} {$DEFINE DELPHIXE8_UP} {$DEFINE DELPHIXE8_DOWN} {$ENDIF}
{$IFDEF DELPHIXE9} {$DEFINE DELPHIXE9_UP} {$DEFINE DELPHIXE9_DOWN} {$ENDIF}


// UP
{$IFDEF KYLIX3_UP} {$DEFINE KYLIX2_UP} {$ENDIF}
{$IFDEF KYLIX2_UP} {$DEFINE KYLIX1_UP} {$ENDIF}
{$IFDEF CBUILDER5_UP} {$DEFINE CBUILDER4_UP} {$ENDIF}
{$IFDEF CBUILDER4_UP} {$DEFINE CBUILDER3_UP} {$ENDIF}
{$IFDEF CBUILDER3_UP} {$DEFINE CBUILDER1_UP} {$ENDIF}

{$IFDEF DELPHIXE9_UP} {$DEFINE DELPHIXE8_UP} {$ENDIF}
{$IFDEF DELPHIXE8_UP} {$DEFINE DELPHIXE7_UP} {$ENDIF}
{$IFDEF DELPHIXE7_UP} {$DEFINE DELPHIXE6_UP} {$ENDIF}
{$IFDEF DELPHIXE6_UP} {$DEFINE DELPHIXE5_UP} {$ENDIF}
{$IFDEF DELPHIXE5_UP} {$DEFINE DELPHIXE4_UP} {$ENDIF}
{$IFDEF DELPHIXE4_UP} {$DEFINE DELPHIXE3_UP} {$ENDIF}
{$IFDEF DELPHIXE3_UP} {$DEFINE DELPHIXE2_UP} {$ENDIF}
{$IFDEF DELPHIXE2_UP} {$DEFINE DELPHIXE_UP} {$ENDIF}
{$IFDEF DELPHIXE_UP} {$DEFINE DELPHI2010_UP} {$ENDIF}
{$IFDEF DELPHI2010_UP} {$DEFINE DELPHI2009_UP} {$ENDIF}
{$IFDEF DELPHI2009_UP} {$DEFINE DELPHI2007_UP} {$ENDIF}
{$IFDEF DELPHI2007_UP} {$DEFINE DELPHI2006_UP} {$ENDIF}
{$IFDEF DELPHI2006_UP} {$DEFINE DELPHI2005_UP} {$ENDIF}
{$IFDEF DELPHI2005_UP} {$DEFINE DELPHI8_UP} {$ENDIF}
{$IFDEF DELPHI8_UP} {$DEFINE DELPHI7_UP} {$ENDIF}
{$IFDEF DELPHI7_UP} {$DEFINE DELPHI6_UP} {$ENDIF}
{$IFDEF DELPHI6_UP} {$DEFINE DELPHI5_UP} {$ENDIF}
{$IFDEF DELPHI5_UP} {$DEFINE DELPHI4_UP} {$ENDIF}
{$IFDEF DELPHI4_UP} {$DEFINE DELPHI3_UP} {$ENDIF}
{$IFDEF DELPHI3_UP} {$DEFINE DELPHI2_UP} {$ENDIF}
{$IFDEF DELPHI2_UP} {$DEFINE DELPHI1_UP} {$ENDIF}

// DOWN
{$IFDEF KYLIX1_DOWN} {$DEFINE KYLIX2_DOWN} {$ENDIF}
{$IFDEF KYLIX2_DOWN} {$DEFINE KYLIX3_DOWN} {$ENDIF}
{$IFDEF CBUILDER1_DOWN} {$DEFINE CBUILDER3_DOWN} {$ENDIF}
{$IFDEF CBUILDER3_DOWN} {$DEFINE CBUILDER4_DOWN} {$ENDIF}
{$IFDEF CBUILDER4_DOWN} {$DEFINE CBUILDER5_DOWN} {$ENDIF}

{$IFDEF DELPHI1_DOWN} {$DEFINE DELPHI2_DOWN} {$ENDIF}
{$IFDEF DELPHI2_DOWN} {$DEFINE DELPHI3_DOWN} {$ENDIF}
{$IFDEF DELPHI3_DOWN} {$DEFINE DELPHI4_DOWN} {$ENDIF}
{$IFDEF DELPHI4_DOWN} {$DEFINE DELPHI5_DOWN} {$ENDIF}
{$IFDEF DELPHI5_DOWN} {$DEFINE DELPHI6_DOWN} {$ENDIF}
{$IFDEF DELPHI6_DOWN} {$DEFINE DELPHI7_DOWN} {$ENDIF}
{$IFDEF DELPHI7_DOWN} {$DEFINE DELPHI8_DOWN} {$ENDIF}
{$IFDEF DELPHI2005_DOWN} {$DEFINE DELPHI2006_DOWN} {$ENDIF}
{$IFDEF DELPHI2006_DOWN} {$DEFINE DELPHI2007_DOWN} {$ENDIF}
{$IFDEF DELPHI2007_DOWN} {$DEFINE DELPHI2008_DOWN} {$ENDIF}
{$IFDEF DELPHI2008_DOWN} {$DEFINE DELPHI2009_DOWN} {$ENDIF}
{$IFDEF DELPHI2009_DOWN} {$DEFINE DELPHI2010_DOWN} {$ENDIF}
{$IFDEF DELPHI2010_DOWN} {$DEFINE DELPHIXE_DOWN} {$ENDIF}
{$IFDEF DELPHIXE_DOWN} {$DEFINE DELPHIXE2_DOWN} {$ENDIF}
{$IFDEF DELPHIXE2_DOWN} {$DEFINE DELPHIXE3_DOWN} {$ENDIF}
{$IFDEF DELPHIXE3_DOWN} {$DEFINE DELPHIXE4_DOWN} {$ENDIF}
{$IFDEF DELPHIXE4_DOWN} {$DEFINE DELPHIXE5_DOWN} {$ENDIF}
{$IFDEF DELPHIXE5_DOWN} {$DEFINE DELPHIXE6_DOWN} {$ENDIF}
{$IFDEF DELPHIXE6_DOWN} {$DEFINE DELPHIXE7_DOWN} {$ENDIF}
{$IFDEF DELPHIXE7_DOWN} {$DEFINE DELPHIXE8_DOWN} {$ENDIF}
{$IFDEF DELPHIXE8_DOWN} {$DEFINE DELPHIXE9_DOWN} {$ENDIF}

// Simbolos especiais
{$IFDEF DELPHI5_UP} {$DEFINE USE_VARIANTS} {$ENDIF}

