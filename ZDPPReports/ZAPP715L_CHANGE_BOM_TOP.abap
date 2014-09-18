*----------------------------------------------------------------------*
*   INCLUDE ZAPP715L_CHANGE_BOM_TOP                                    *
*----------------------------------------------------------------------*
PROGRAM ZAPP715M_CHANGE_BOM  NO STANDARD PAGE HEADING
                             LINE-SIZE 1023
                             MESSAGE-ID ZMPP.

*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : PLAF,      "Planned order
         CABN,      "Characteristic
         AUSP.      "Characteristic Values
TABLES : ZTPP_PLAN_COMP.    " History of Plan Order Component Change..

*----------------------------------------------------------------------*
* TYPE-POOLS Definition
*----------------------------------------------------------------------*
type-pools: CXTAB,
            vrm.

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
*----> SCREEN 9000 ITAB
DATA: BEGIN OF IT_SCREEN OCCURS 0,
        CHK,
*       INCLUDE STRUCTURE ZTPP_COMPPARTVIN.
        objek       LIKE  AUSP-OBJEK      ,
        WORDER      LIKE  AUSP-ATWRT      ,
        ECOLOR      LIKE  AUSP-ATWRT      ,
        ICOLOR      LIKE  AUSP-ATWRT      ,
        STATUS      LIKE  AUSP-ATWRT      ,
        QTY         LIKE  ZTPP_PLAN_COMP-QTY,
        SDATE       TYPE  D               ,
        plnum       LIKE  PLAF-plnum      ,
        RPDATE      TYPE  D               ,
        BODYDATE    TYPE  D               ,
        CHANGE,
        msg(100),
      END OF IT_SCREEN.

data: begin of st5000,
        aennr       like aenr-aennr,
        matnr       like mara-matnr,
      end of st5000.

DATA: IT_DISP          LIKE TABLE OF ZTPP_PLAN_COMP    WITH HEADER LINE,
      IT_AUSP          LIKE TABLE OF AUSP              WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA: OK_CODE       TYPE  SY-UCOMM,
      SAVE_OK_CODE  TYPE  SY-UCOMM.

*-----> Checkbox
DATA: WA_TOTAL TYPE I     ,
      wa_check type i     ,
      xname    type vrm_id,
      xlist    type vrm_values,
      xvalue   like line of xlist.

*-----> VARIABLE OF SCREEN9000
DATA: WA_CHANGED,
      WA_FLAG   ,
      wa_name       type vrm_id    ,    " Field Name..
      WA_RP         TYPE AUSP-ATWRT,    " List BOX - RP FROM
      WA_RP_TO      TYPE AUSP-ATWRT,    " List BOX - RP TO
      WA_PART_BOM   TYPE ZTPP_COMPPARTVIN-PARTNEW,
      WA_OBJEK      TYPE AUSP-OBJEK,    "Vehicle Master
      WA_ATWRT      TYPE AUSP-ATWRT.    "P_RP_STATUS

*-----> SET/GET CURSOR HANDLING VARIABLE
DATA: WA_TXT9000(40) TYPE  C,         "SET/GET CURSOR FIELD
      WA_TXT9100(40) TYPE  C,
      WA_LINE9000    TYPE  SY-INDEX,  "SET/GET CURSOR LINE
      WA_LINE9100    TYPE  SY-INDEX.

*-----> BATCH JOB
DATA: WA_JOBCOUNT LIKE  TBTCJOB-JOBCOUNT ,
      WA_JOBNAME  LIKE  TBTCJOB-JOBNAME ,
      WA_REPORT   LIKE  SY-REPID.

*-----> SCREEN 9000 VARIABLE
DATA: P_MATNR     LIKE  MARA-MATNR,    "FSC
      P_VERID     TYPE  ZTPP_COMPPARTVIN-VERID.  "PROD VERSION
DATA: WA_OLDNO    LIKE  MARA-MATNR,
      WA_NEWNO    LIKE  MARA-MATNR,
      WA_DIS1     TYPE  C         ,
      WA_DIS2     TYPE  C         ,
      WA_LINES    TYPE  I         ,
      WA_ECM_RESULT(10)           ,
      WA_ECM_NO   LIKE  RC29A-AENNR ,
      WA_QTY      TYPE  P DECIMALS 3,
      WA_MATNR    LIKE  MARA-MATNR,
      WA_VERID    LIKE  MKAL-VERID.

RANGES: R_PLNUM   FOR  PLAF-PLNUM,
        R_PSTTR   FOR  PLAF-PSTTR.

CONTROLS : TC_5000 TYPE TABLEVIEW USING SCREEN 5000,
           TC_9000 TYPE TABLEVIEW USING SCREEN 9000.

*----------------------------------------------------------------------*
* INTERNAL TABLE DECLARATION FOR POSSIBLE ENTRIES
*----------------------------------------------------------------------*
DATA  IT_DYNPREAD LIKE DYNPREAD OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF IT_VALUETAB OCCURS 0,
          VALUE(80).
DATA: END OF IT_VALUETAB.

DATA: BEGIN OF IT_FIELDS OCCURS 0.
        INCLUDE STRUCTURE HELP_VALUE.
DATA: END OF IT_FIELDS.

DATA: BEGIN OF IT_DYNPFIELDS  OCCURS 0.
        INCLUDE STRUCTURE DYNPREAD.
DATA: END OF IT_DYNPFIELDS.

DATA  WA_SELECT_IX LIKE SY-TABIX.

DATA: BEGIN OF IT_SELECT_VALUES OCCURS 0.
        INCLUDE STRUCTURE HELP_VTAB.
DATA: END OF IT_SELECT_VALUES.

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION (BDC DATA)
*----------------------------------------------------------------------*
DATA : IT_BDCDATA     LIKE TABLE OF BDCDATA  WITH HEADER LINE.
*---->
DATA : IT_SCREEN_BDC  LIKE TABLE OF IT_SCREEN WITH HEADER LINE.
*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION(BDC)
*----------------------------------------------------------------------*
*------> BDC MODE ('A' DISPLAY SCREEN, 'N' NO DISPLAY)
DATA: WA_MODE,                          "BDC MODE
      WA_ACTIVE(1).                     "ACTIVE CHECKBOX (Y:'X',N:'')
DATA: WA_OPTION_DS   LIKE CTU_PARAMS.   "BDC OPTION

*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: C_MARK        VALUE 'X',
           C_MODE        VALUE 'N',
           C_RP_STATUS   TYPE  CABN-ATNAM  VALUE 'P_RP_STATUS'.
