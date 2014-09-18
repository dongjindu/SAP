************************************************************************
* Program name : SAPMZEMMPM19E_CONTAINER_DISP
* Created by   : Min-su Park
* Created on   : 2003.10.06.
* Pattern      :
* Description  : Container content display(Graphical)
*
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.10.06.     Min-su Park      UD1K901873     Initial Coding       *
* 08.13.2014      Victor     T-code has been deleted for APM
************************************************************************
*eject
*-----------------------------------------------------------------------
*        Zentraler Modulpool LVS Stammdatenverwaltung
*-----------------------------------------------------------------------

*eject
*-----------------------------------------------------------------------
*        Report-Header / Tabellen / Daten / Field-Symbols
*-----------------------------------------------------------------------
INCLUDE MZEMMPM19E_CONTAINER_DISPTOP.
*         INCLUDE ML01STOP.

*eject
*-----------------------------------------------------------------------
*        Allgemein benützte Module
*-----------------------------------------------------------------------
INCLUDE ZMLLVSMOD.
*         INCLUDE MLLVSMOD.

*eject
*-----------------------------------------------------------------------
*        PBO - Module
*-----------------------------------------------------------------------
INCLUDE MZEMMPM19E_CONTAINER_DISPO00.
*         INCLUDE ML01SO00.

*eject
*-----------------------------------------------------------------------
*        PAI - Module
*-----------------------------------------------------------------------
INCLUDE MZEMMPM19E_CONTAINER_DISPI00.
*         INCLUDE ML01SI00.

*-----------------------------------------------------------------------
*        Help- Module
*-----------------------------------------------------------------------
INCLUDE MZEMMPM19E_CONTAINER_DISPH00.
*         INCLUDE ML01SH00.

*eject
*-----------------------------------------------------------------------
*        Allgemein benützte Formroutinen
*-----------------------------------------------------------------------
INCLUDE ZMLLVSFOR.
*         INCLUDE MLLVSFOR.

*eject
*-----------------------------------------------------------------------
*        FORM-Routinen ( alphabetisch )
*-----------------------------------------------------------------------
*eject
INCLUDE MZEMMPM19E_CONTAINER_DISPF10.
*         INCLUDE ML01SF10.             " Feldauswahl Basis-/Sonderregel
*eject
INCLUDE MZEMMPM19E_CONTAINER_DISPFS0.
*         INCLUDE ML01SFS0.             " EXEC-SQL
*eject
INCLUDE MZEMMPM19E_CONTAINER_DISPFS1.
*         INCLUDE ML01SFS1.             " Sperren
*eject
INCLUDE MZEMMPM19E_CONTAINER_DISPF00.
*         INCLUDE ML01SF00.             " Allgemeine Form-Routinen
*eject
INCLUDE MZEMMPM19E_CONTAINER_DISPFFC.
*         INCLUDE ML01SFFC.             " FCODE Bearbeitung
*{TC Begin} generation http://intranet.sap.com/materialversion
INCLUDE ZMGVTC_SAPML01S.
*  INCLUDE MGVTC_SAPML01S.
*{TC End} generation
