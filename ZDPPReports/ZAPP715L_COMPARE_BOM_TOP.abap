*&---------------------------------------------------------------------*
*& INCLUDE ZAPP715L_COMPARE_BOM_TOP                                    *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  ZAPP715M_COMPARE_BOM  NO STANDARD PAGE HEADING
                               LINE-SIZE 1023
                               MESSAGE-ID ZMPP.

DATA  WA_PSTTR_FLG.
INCLUDE <ICON>.
TYPE-POOLS: M61X.
*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : PLAF,      "Planned order
         CABN,      "Characteristic
         AUSP.      "Characteristic Values
TABLES : ZTPP_COMPPARTVIN,
         ZTPP_PARTHISTORY.

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
*DATA: IT_PLAF  LIKE  TABLE OF PLAF WITH HEADER LINE.
*--> Dispobereich
DATA: CM61B    TYPE M61X_CM61B.

*----> SCREEN 9000 ITAB
DATA : BEGIN OF IT_SCREEN OCCURS 0,
        CHK.
        INCLUDE STRUCTURE ZTPP_COMPPARTVIN.
DATA : CHANGED.
DATA : END OF IT_SCREEN.

*----> TEMPERARY ITAB
DATA : BEGIN OF IT_TEMP OCCURS 0,
         MATNR   LIKE  ZTPP_COMPPARTVIN-MATNR,
         VERID   LIKE  ZTPP_COMPPARTVIN-VERID.
DATA : END OF IT_TEMP.

*----> SCREEN 9100 ITAB
DATA : IT_SCREEN9100  LIKE TABLE OF IT_SCREEN WITH HEADER LINE.

DATA : WA_RSFLDESC        LIKE  RSFLDESC,
       WA_HELP_FIELD      LIKE  RSSCR-DBFIELD,
       WA_SEARCH_HELP     LIKE  DDSHDESCR-SHLPNAME,
       WA_TAB_AND_FIELD   LIKE  RSTABFIELD.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA: OK_CODE       TYPE  SY-UCOMM,
      SAVE_OK_CODE  TYPE  SY-UCOMM.

*-----> ICONS
DATA: WA_DATE_EXCL   LIKE  ICONS-TEXT,
      WA_DATE_MORE   LIKE  ICONS-TEXT,
      WA_MATNR_EXCL  LIKE  ICONS-TEXT,
      WA_MATNR_MORE  LIKE  ICONS-TEXT,
      WA_PSTTR_EXCL  LIKE  ICONS-TEXT,
      WA_PSTTR_MORE  LIKE  ICONS-TEXT.

*-----> Work Area for IT_SCREEN
DATA: WA_SCREEN_DS   LIKE  IT_SCREEN.

*-----> Checkbox
DATA: CHK.

DATA: WA_OBJEK      TYPE  AUSP-OBJEK,    "Vehicle Master
      WA_ATWRT      TYPE  AUSP-ATWRT,    "P_RP_STATUS
      WA_PARTNEW    TYPE  ZTPP_COMPPARTVIN-PARTNEW,
      WA_CHANGED.

*-----> SET/GET CURSOR HANDLING VARIABLE
DATA: WA_TXT9000(40) TYPE  C,         "SET/GET CURSOR FIELD
      WA_TXT9100(40) TYPE  C,
      WA_LINE9000    TYPE  SY-INDEX,  "SET/GET CURSOR LINE
      WA_LINE9100    TYPE  SY-INDEX.

*-----> BATCH JOB
DATA: WA_JOBCOUNT LIKE  TBTCJOB-JOBCOUNT ,
      WA_JOBNAME  LIKE  TBTCJOB-JOBNAME ,
      WA_REPORT   LIKE  SY-REPID.

*------> RANGES
RANGES: R_PSTTR     FOR   PLAF-PSTTR,
        R_DATUM     FOR   SY-DATUM,
        R_MATNR     FOR   MARA-MATNR.

CONTROLS : TC_9000 TYPE TABLEVIEW USING SCREEN 9000,
           TC_9100 TYPE TABLEVIEW USING SCREEN 9100.

*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: C_MARK        VALUE 'X',
           C_RP_STATUS   TYPE  CABN-ATNAM  VALUE 'P_RP_STATUS'.
