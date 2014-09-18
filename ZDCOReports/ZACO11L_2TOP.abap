*----------------------------------------------------------------------*
*   INCLUDE ZACO11L_2TOP                                               *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** Type-Pools
TYPE-POOLS : CKMV0, SYDES,  SLIST .

** Tables
TABLES : MARA, MACKU, T134, KKBML.
TABLES : CKMLCR, CKMLPP, CKMLPR, MLCD, CKMLHD.
TABLES : ZSCO_ML_REPORT, ZTCO_ML_MVT_GR, ZTCO_ML_MVT_GR_T,
         ZVCO_ML_REP_HIER.

** Internal table
* For ZTCO_MATLEDGER
DATA :  BEGIN OF IT_ZTCO_MATLEDGER OCCURS 500.
        INCLUDE STRUCTURE ZTCO_MATLEDGER.
DATA :  END OF   IT_ZTCO_MATLEDGER.
* For MLCD
DATA : IT_KALNR	 TYPE CKMV0_MATOBJ_TBL.
DATA : WA_KALNR	 TYPE CKMV0_MATOBJ_STR.
DATA :  IT_OT_MLCD LIKE TABLE OF MLCD
                   WITH HEADER LINE ,
        IT_OT_MLCD_NOT_ALLOC	
                   LIKE TABLE OF MLCD
                   WITH HEADER LINE .
DATA :  IT_MLCD    LIKE STANDARD TABLE OF MLCD
                   WITH HEADER LINE .
* For Material Information
DATA :  IT_MACKU   LIKE TABLE OF MACKU
                   WITH HEADER LINE .
* For Report Format
DATA :  BEGIN OF IT_ZSCO_ML_REPORT OCCURS 500.
        INCLUDE STRUCTURE  ZSCO_ML_REPORT.
DATA :   SUM_ZU_MENGE LIKE KKBML-MENGE,
         SUM_ZU_BEWER LIKE KKBML-BEWER,
         SUM_VN_MENGE LIKE KKBML-MENGE,
         SUM_VN_BEWER LIKE KKBML-BEWER,
         SUM_VP_MENGE LIKE KKBML-MENGE,
         SUM_VP_BEWER LIKE KKBML-BEWER.
DATA :  END OF IT_ZSCO_ML_REPORT.
* For Ending/Beginning Data
DATA :  IT_ZSCO_ML_REPORT_COL LIKE ZSCO_ML_REPORT_COL OCCURS 500
                              WITH HEADER LINE .
* For MVT. Gr. Linkage
DATA :  IT_ZTCO_ML_MVT_GR     LIKE STANDARD TABLE OF ZTCO_ML_MVT_GR
                              WITH HEADER LINE .
* Field List for ML report
DATA :  IT_DFIES_TAB          LIKE STANDARD TABLE OF DFIES
                              WITH HEADER LINE .
* Header texts
DATA : BEGIN OF IT_HD_TEXT OCCURS 0,
         FIELDNAME TYPE NAME_FELD,
         LV1_TEXT  TYPE TEXT20,
         LV2_TEXT  TYPE TEXT20,
       END OF  IT_HD_TEXT.


** Global Fields
DATA : GV_LINE_SIZE TYPE I .
* For List
CONSTANTS : C_KEY   TYPE WAERS VALUE 'WAERS'.
DATA : GV_KEY_L     TYPE I .
DATA : GV_SORT_KEY(30)         VALUE 'MTART'.
DATA : GV_COLLOCK(1).
DATA : GV_EXP(1).


*----------------------------------------------------------------------*
*   Macro
*----------------------------------------------------------------------*
DEFINE SUM_UP_BY_CAT.
* ZU, ZN, VP
*  Qty./ Amt.
  LOOP AT IT_DFIES_TAB WHERE FIELDNAME(8) = '&2'.
    CLEAR LV_FNAME.
    CONCATENATE '&1' '-' IT_DFIES_TAB-FIELDNAME
           INTO LV_FNAME.
    ASSIGN (LV_FNAME) TO  <FSFN>.
*     Adding
    IT_ZSCO_ML_REPORT-SUM_&2
    = IT_ZSCO_ML_REPORT-SUM_&2
      + <FSFN>.
  ENDLOOP.
END-OF-DEFINITION.

DEFINE KEY_SEPARATOR.
  WRITE :  / '|'               NO-GAP.
  CLEAR LV_BL_WRI.
  LOOP AT IT_DFIES_TAB.
    CHECK LV_BL_WRI EQ SPACE.
*   Key Part
    WRITE : AT (IT_DFIES_TAB-OUTPUTLEN) &1
              COLOR   1
                               NO-GAP,
              '|'              NO-GAP.
    IF  IT_DFIES_TAB-FIELDNAME = C_KEY.
      LV_BL_WRI = 'X'.
    ENDIF.
  ENDLOOP.
END-OF-DEFINITION.

DEFINE WRITE_DATA.
  WRITE :  / '|'               NO-GAP.
  LOOP AT &1.
    CLEAR L_FNAME.
    CONCATENATE  'IT_ZSCO_ML_REPORT' '-'
                 &1-FIELDNAME
           INTO  L_FNAME.
    ASSIGN (L_FNAME) TO <FS>.
    WRITE : <FS> NO-GAP, '|' NO-GAP.
  ENDLOOP.
END-OF-DEFINITION.


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_BDATJ LIKE MLCD_KEY-BDATJ MEMORY ID BDTJ OBLIGATORY,
             P_POPER LIKE MLCD_KEY-POPER MEMORY ID POPR OBLIGATORY,
             P_CURTP LIKE MLCD-CURTP     DEFAULT '10'   OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BL1.

SELECTION-SCREEN BEGIN OF BLOCK BL2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS : S_MATNR FOR MARA-MATNR  MEMORY ID MAT.
SELECT-OPTIONS : S_MTART FOR T134-MTART  MEMORY ID MTA.
SELECTION-SCREEN END OF BLOCK BL2.
