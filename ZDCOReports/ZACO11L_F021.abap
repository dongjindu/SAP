*----------------------------------------------------------------------*
*   INCLUDE ZACO11L_F021                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  READ_FR_ZTCO_MATLEDGER
*&---------------------------------------------------------------------*
*       Read Main Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_FR_ZTCO_MATLEDGER.
*
*             P_BDATJ LIKE MLCD_KEY-BDATJ MEMORY ID BDTJ OBLIGATORY,
*             P_POPER LIKE MLCD_KEY-POPER MEMORY ID POPR OBLIGATORY,
*             P_CURTP LIKE MLCD-CURTP     DEFAULT '10'   OBLIGATORY.
*             S_MATNR FOR MARA-MATNR.
*             S_MTART FOR T134-MTART.

  CLEAR : IT_ZTCO_MATLEDGER, IT_ZTCO_MATLEDGER[].

* Index ZD1(ZTCO_MATLEDGER), Key Index (MARA)
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTCO_MATLEDGER
           FROM ZTCO_MATLEDGER AS A
          WHERE BDATJ = P_BDATJ
            AND POPER = P_POPER
            AND CURTP = P_CURTP
            AND EXISTS   ( SELECT * FROM MARA
                                  WHERE MATNR = A~MATNR
                                    AND MATNR IN  S_MATNR
                                    AND MTART IN  S_MTART )
            AND ( LBKUM NE SPACE
               OR SALK3 NE SPACE
               OR PRDIF NE SPACE
               OR KRDIF NE SPACE ).
  IF  IT_ZTCO_MATLEDGER[] IS INITIAL .
*    MESSAGE E026.
  ENDIF.

ENDFORM.                    " READ_FR_ZTCO_MATLEDGER

*&---------------------------------------------------------------------*
*&      Form  READ_MLCD
*&---------------------------------------------------------------------*
*       Read MLCD data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_MLCD.

* Set KALNR (Cost Estimate Number)
  CLEAR : IT_KALNR,   IT_KALNR[].
  CLEAR : IT_OT_MLCD, IT_OT_MLCD[],
          IT_OT_MLCD_NOT_ALLOC,
          IT_OT_MLCD_NOT_ALLOC[].

  LOOP AT IT_ZTCO_MATLEDGER.
    CLEAR WA_KALNR.
    MOVE-CORRESPONDING IT_ZTCO_MATLEDGER TO WA_KALNR.
    APPEND WA_KALNR TO IT_KALNR .
    CLEAR IT_ZTCO_MATLEDGER.
  ENDLOOP.
  SORT IT_KALNR BY KALNR OBTYP BWKEY MATNR .
  DELETE ADJACENT DUPLICATES FROM IT_KALNR .

* Read data
  CALL FUNCTION 'CKMCD_MLCD_READ'
    EXPORTING
      I_FROM_BDATJ            = P_BDATJ
      I_FROM_POPER            = P_POPER
*     I_TO_BDATJ              =
*     I_TO_POPER              =
*     I_UNTPER                =
*     I_RUN_ID                =
*     I_NO_BUFFER             =
      I_REFRESH_BUFFER        = 'X'
      I_ONLINE                = 'X'
*     I_NO_MLCD_CREATE        =
    TABLES
      IT_KALNR                = IT_KALNR
      OT_MLCD                 = IT_OT_MLCD
      OT_MLCD_NOT_ALLOC       = IT_OT_MLCD_NOT_ALLOC
    EXCEPTIONS
      DATA_ERROR              = 1
      OTHERS                  = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID   SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " READ_MLCD

*&---------------------------------------------------------------------*
*&      Form  Col_MLCD_DATA
*&---------------------------------------------------------------------*
*       Re_Org MLCD DATA
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COL_MLCD_DATA.

  CLEAR : IT_MLCD, IT_MLCD[].

*IT_OT_MLCD : MLCD
*IT_OT_MLCD_NOT_ALLOC : Not Included
* There is no process definition about the data which were not allocated
* Temporarily, Ignore and sum up the data unable to be allocated.

* MLCD (Allocated)
  LOOP AT IT_OT_MLCD.
    MOVE-CORRESPONDING IT_OT_MLCD TO  IT_MLCD.
    CLEAR : IT_MLCD-MANDT, IT_MLCD-PTYP.
    COLLECT IT_MLCD.
    CLEAR   IT_MLCD.
    CLEAR   IT_OT_MLCD.
  ENDLOOP.

* MLCD (Not Allocated)
  LOOP AT IT_OT_MLCD_NOT_ALLOC.
    MOVE-CORRESPONDING IT_OT_MLCD_NOT_ALLOC TO  IT_MLCD.
    CLEAR : IT_MLCD-MANDT, IT_MLCD-PTYP.
    COLLECT IT_MLCD.
    CLEAR   IT_MLCD.
    CLEAR   IT_OT_MLCD_NOT_ALLOC.
  ENDLOOP.

  SORT IT_MLCD BY KALNR .
  CLEAR   IT_MLCD.

ENDFORM.                    " Col_MLCD_DATA

*&---------------------------------------------------------------------*
*&      Form  SET_DATA_INTO_REPTAB
*&---------------------------------------------------------------------*
*       Set data with report structure
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_DATA_INTO_REPTAB.

  CLEAR : IT_ZSCO_ML_REPORT, IT_ZSCO_ML_REPORT[].

* Read Beginning Inv./Val. and Ending Inv./Val.
  PERFORM READ_BEGIN_END_INV_VAL.
* Put  Beginning Inv./Val. and Ending Inv./Val.
  PERFORM PUT_BEGIN_END_INV_VAL_2_REPTAB.
* Re-orginze data by MVT. gr.
  PERFORM RE_ORG_DATA_BY_MVT_GR.

ENDFORM.                    " SET_DATA_INTO_REPTAB

*&---------------------------------------------------------------------*
*&      Form  READ_BEGIN_END_INV_VAL
*&---------------------------------------------------------------------*
*       Read Beginning Inv./Val. and Ending Inv./Val.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_BEGIN_END_INV_VAL.

* Local Data definition
  CLEAR : IT_ZSCO_ML_REPORT_COL,
          IT_ZSCO_ML_REPORT_COL[].
* Restric Value
  IF    S_MATNR[] IS INITIAL
   AND  S_MTART[] IS INITIAL.
* INV/Stock Information
* Valuation Information
* - Index CKMLPP, CKMLCR - table Key
    SELECT * INTO CORRESPONDING FIELDS OF  TABLE IT_ZSCO_ML_REPORT_COL
               FROM CKMLCR AS CR INNER JOIN CKMLPP AS PP
                 ON CR~KALNR  = PP~KALNR
                AND CR~BDATJ  = PP~BDATJ
                AND CR~POPER  = PP~POPER
                AND CR~UNTPER = PP~UNTPER
               WHERE
                      CR~BDATJ = P_BDATJ
                 AND  CR~POPER = P_POPER
                 AND  CR~CURTP = P_CURTP.
  ELSE.
* INV/Stock Information
* Valuation Information
* - Index CKMLPP, CKMLCR - table Key
    SELECT * INTO CORRESPONDING FIELDS OF  TABLE IT_ZSCO_ML_REPORT_COL
               FROM CKMLCR AS CR INNER JOIN CKMLPP AS PP
                 ON CR~KALNR  = PP~KALNR
                AND CR~BDATJ  = PP~BDATJ
                AND CR~POPER  = PP~POPER
                AND CR~UNTPER = PP~UNTPER
                FOR ALL ENTRIES IN IT_MACKU
               WHERE  CR~KALNR = IT_MACKU-KALN1
                 AND  CR~BDATJ = P_BDATJ
                 AND  CR~POPER = P_POPER
                 AND  CR~CURTP = P_CURTP.
  ENDIF.

  CLEAR : IT_ZSCO_ML_REPORT_COL.

ENDFORM.                    " READ_BEGIN_END_INV_VAL

*&---------------------------------------------------------------------*
*&      Form  PUT_BEGIN_END_INV_VAL_2_REPTAB
*&---------------------------------------------------------------------*
*       Put  Beginning Inv./Val. and Ending Inv./Val.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PUT_BEGIN_END_INV_VAL_2_REPTAB.

* Local Data definition
  DATA : LV_DF   LIKE MLCD-ESTPRD,
         LV_CLDF LIKE MLCD-ESTKDM,
         P_SH   TYPE I.

  CLEAR IT_ZSCO_ML_REPORT_COL.

  LOOP AT IT_ZSCO_ML_REPORT_COL.
    MOVE-CORRESPONDING IT_ZSCO_ML_REPORT_COL
                    TO IT_ZSCO_ML_REPORT.
    COLLECT IT_ZSCO_ML_REPORT.
    CLEAR   IT_ZSCO_ML_REPORT.
    CLEAR   IT_ZSCO_ML_REPORT_COL.
  ENDLOOP.

** PRD/ERD/End Inv.
  LOOP AT IT_ZSCO_ML_REPORT .
    CLEAR :  LV_DF,  LV_CLDF, P_SH.
    LOOP AT IT_MLCD
                WHERE KALNR = IT_ZSCO_ML_REPORT-KALNR
                   AND ( ESTPRD NE SPACE
                      OR ESTKDM NE SPACE
                      OR MSTPRD NE SPACE
                      OR MSTKDM NE SPACE
                      OR TPPRD  NE SPACE ).
* Check Consumption , 'VP' ?
      P_SH = '1'.
      IF IT_MLCD-CATEG = 'VN'.
        P_SH = '-1'.
      ENDIF.
* ERD/PRD
      LV_CLDF
       = P_SH * (
                  ( IT_MLCD-ESTPRD + IT_MLCD-MSTPRD + IT_MLCD-TPPRD )
                + ( IT_MLCD-ESTKDM + IT_MLCD-MSTKDM )
                ).
      LV_DF
       = LV_DF + LV_CLDF.

      CASE IT_MLCD-CATEG.
        WHEN 'ZU'.
* 'ZU'
          IT_ZSCO_ML_REPORT-ZU_BEWER_OTHER =
            IT_ZSCO_ML_REPORT-ZU_BEWER_OTHER + LV_CLDF.
        WHEN 'VN'.
* 'VN'
          IT_ZSCO_ML_REPORT-VN_BEWER_OTHER =
            IT_ZSCO_ML_REPORT-VN_BEWER_OTHER + LV_CLDF.
        WHEN 'VP'.
* 'VP'
          IT_ZSCO_ML_REPORT-VP_BEWER =
              IT_ZSCO_ML_REPORT-VP_BEWER + LV_CLDF.
        WHEN OTHERS.
          IT_ZSCO_ML_REPORT-BEWER_OTHER =
              IT_ZSCO_ML_REPORT-BEWER_OTHER + LV_CLDF.
      ENDCASE.

      CLEAR IT_MLCD.
    ENDLOOP.
*     Re-Cal. Ending Inv.
    IT_ZSCO_ML_REPORT-SALK3 = IT_ZSCO_ML_REPORT-SALK3
                            - LV_DF.
* Modify
    MODIFY IT_ZSCO_ML_REPORT.
    CLEAR IT_ZSCO_ML_REPORT.
  ENDLOOP.

  CLEAR   IT_ZSCO_ML_REPORT.

ENDFORM.                    " PUT_BEGIN_END_INV_VAL_2_REPTAB

*&---------------------------------------------------------------------*
*&      Form  RE_ORG_DATA_BY_MVT_GR
*&---------------------------------------------------------------------*
*       Re-orginze data by MVT. gr.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RE_ORG_DATA_BY_MVT_GR.
* Assign Amount/Quantity
  PERFORM ASSIGN_VALUES .
* PC (Price Change (Indp.))
  PERFORM ASSIGN_PC.
* Adjust 'ZU', 'VN', 'VP' (Other Values)
*  PERFORM ADJUST_OTHERS.
ENDFORM.                    " RE_ORG_DATA_BY_MVT_GR

*&---------------------------------------------------------------------*
*&      Form  READ_MAT_INF
*&---------------------------------------------------------------------*
*       Read Material Information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_MAT_INF.

  CLEAR : IT_MACKU, IT_MACKU[].

  SELECT * INTO TABLE IT_MACKU
           FROM MACKU
          WHERE MATNR IN S_MATNR
            AND MTART IN S_MTART.
  IF SY-SUBRC <> 0.
    MESSAGE E069.
  ENDIF.

ENDFORM.                    " READ_MAT_INF

*&---------------------------------------------------------------------*
*&      Form  READ_FIELD_LIST
*&---------------------------------------------------------------------*
*       Read Field-List
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_FIELD_LIST.

  CLEAR : IT_ZTCO_ML_MVT_GR, IT_ZTCO_ML_MVT_GR[].

* Read ALL (About 30 records)
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTCO_ML_MVT_GR
           FROM ZTCO_ML_MVT_GR.

  IF IT_ZTCO_ML_MVT_GR[] IS INITIAL.
    MESSAGE E071.
  ENDIF.

ENDFORM.                    " READ_FIELD_LIST

*&---------------------------------------------------------------------*
*&      Form  ASSIGN_VALUES
*&---------------------------------------------------------------------*
*       Assign Values
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSIGN_VALUES.

*IT_ZSCO_ML_REPORT
*IT_ZTCO_MATLEDGER
*IT_MACKU
*IT_MLCD

* Sort
  SORT IT_MLCD BY KALNR.
  SORT IT_ZTCO_MATLEDGER
        BY
        KALNR BDATJ POPER CURTP KATEGORIE BEWARTGRP FELDG VGART.
  SORT IT_MACKU           BY KALN1.

  LOOP AT IT_ZSCO_ML_REPORT.
* Set Material Information
    CLEAR IT_MACKU.
    READ TABLE IT_MACKU WITH KEY KALN1 = IT_ZSCO_ML_REPORT-KALNR
                        BINARY SEARCH.
    IT_ZSCO_ML_REPORT-MATNR = IT_MACKU-MATNR.
    IT_ZSCO_ML_REPORT-BWKEY = IT_MACKU-BWKEY.
    IT_ZSCO_ML_REPORT-BWTAR = IT_MACKU-BWTAR.
    IT_ZSCO_ML_REPORT-MTART = IT_MACKU-MTART.
** Assign Values by Movement Type Group (VP, VN, ZU)
    LOOP AT IT_ZTCO_MATLEDGER
                        WHERE KALNR = IT_ZSCO_ML_REPORT-KALNR.
      CLEAR IT_ZTCO_ML_MVT_GR.
      READ TABLE IT_ZTCO_ML_MVT_GR
      WITH KEY BEWARTGRP = IT_ZTCO_MATLEDGER-BEWARTGRP
               KATEGORIE = IT_ZTCO_MATLEDGER-KATEGORIE.
*   By MVT. gr.
      IF SY-SUBRC = 0.
*     Qty.
        PERFORM SET_VALUE
                    USING 'IT_ZSCO_ML_REPORT'
                           IT_ZTCO_ML_MVT_GR-FIELDNAME_MENGE
                           IT_ZTCO_MATLEDGER-LBKUM.
*     Amt.
        PERFORM SET_VALUE
                    USING 'IT_ZSCO_ML_REPORT'
                           IT_ZTCO_ML_MVT_GR-FIELDNAME_BEWER
                           IT_ZTCO_MATLEDGER-SALK3.
      ELSE.
*     (Do nothing)
*     Not yet defined - assumed "ZU"? PC? - >to 'ZU' other Field
        IF    IT_ZTCO_MATLEDGER-BEWARTGRP EQ SPACE
          AND IT_ZTCO_MATLEDGER-KATEGORIE EQ SPACE
          AND IT_ZTCO_MATLEDGER-VGART     EQ 'UP'
          AND IT_ZTCO_MATLEDGER-SALK3     NE SPACE.
          IT_ZSCO_ML_REPORT-ZU_BEWER_OTHER
        = IT_ZSCO_ML_REPORT-ZU_BEWER_OTHER + IT_ZTCO_MATLEDGER-SALK3 .
        ENDIF.
      ENDIF.
    ENDLOOP.
* Modify
    MODIFY IT_ZSCO_ML_REPORT.
    CLEAR IT_ZSCO_ML_REPORT.
  ENDLOOP.

ENDFORM.                    " ASSIGN_VALUES

*&---------------------------------------------------------------------*
*&      Form  SET_VALUE
*&---------------------------------------------------------------------*
*       Set Value
*----------------------------------------------------------------------*
*      -->P_ITBODY  The name of a specific internal table
*      -->P_FNAME   The name of field in a ITAB
*      -->P_VALUE   Value
*----------------------------------------------------------------------*
FORM SET_VALUE USING    P_ITBODY
                        P_FNAME
                        P_VALUE.

* Local Data Definition
  FIELD-SYMBOLS : <FS> TYPE ANY.
  DATA : LV_FNAME(80).

  CLEAR LV_FNAME.
  CONCATENATE  P_ITBODY
               '-'
               P_FNAME
               INTO LV_FNAME.

  ASSIGN (LV_FNAME) TO <FS>.
* Adding
  <FS> = <FS> + P_VALUE.

ENDFORM.                    " SET_VALUE

*&---------------------------------------------------------------------*
*&      Form  ADJUST_OTHERS
*&---------------------------------------------------------------------*
*       Adjustment (Other Values) - 'VP' 'ZU' 'VN'
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADJUST_OTHERS.

**IT_MLCD
**IT_ZSCO_ML_REPORT
**IT_ZTCO_MATLEDGER
*
*  DATA : WA_L_ZU LIKE IT_ZSCO_ML_REPORT-ZU.
*  DATA : WA_L_VN LIKE IT_ZSCO_ML_REPORT-VN.
*  DATA : WA_L_VP LIKE IT_ZSCO_ML_REPORT-VP.
*
*  DATA : LV_FNAME(60).
*  FIELD-SYMBOLS : <FSFN> TYPE ANY.
*
*
** Read Field List (DDIF)
*  PERFORM READ_FIELD_LIST_DDIF USING 'ZSCO_ML_REPORT'.
*
** Sum up values
*  LOOP AT IT_ZSCO_ML_REPORT.
** ZU
*    CLEAR WA_L_ZU.
*    MOVE-CORRESPONDING  IT_ZSCO_ML_REPORT TO WA_L_ZU.
**     (Qty)
*    SUM_UP_BY_CAT WA_L_ZU ZU_MENGE.
**     (AMT)
*    SUM_UP_BY_CAT WA_L_ZU ZU_BEWER.
*
** VN
*    CLEAR WA_L_VN.
*    MOVE-CORRESPONDING  IT_ZSCO_ML_REPORT TO WA_L_VN.
**     (Qty)
*    SUM_UP_BY_CAT WA_L_VN VN_MENGE.
**     (AMT)
*    SUM_UP_BY_CAT WA_L_VN VN_BEWER.
*
** VP
*    CLEAR WA_L_VP.
*    MOVE-CORRESPONDING  IT_ZSCO_ML_REPORT TO WA_L_VP.
**     (Qty)
*    SUM_UP_BY_CAT WA_L_VP VP_MENGE.
**     (AMT)
*    SUM_UP_BY_CAT WA_L_Vp VP_BEWER.
*
*    MODIFY IT_ZSCO_ML_REPORT.
*    CLEAR  IT_ZSCO_ML_REPORT.
*  ENDLOOP.

ENDFORM.                    " ADDJUST_OTHERS

*&---------------------------------------------------------------------*
*&      Form  ASSIGN_PC
*&---------------------------------------------------------------------*
*       Assign PC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSIGN_PC.

  LOOP AT IT_MLCD WHERE CATEG EQ 'PC'.
    LOOP AT IT_ZSCO_ML_REPORT WHERE KALNR = IT_MLCD-KALNR.
      IT_ZSCO_ML_REPORT-PABWE = IT_ZSCO_ML_REPORT-PABWE
                              + IT_MLCD-SALK3.
      MODIFY IT_ZSCO_ML_REPORT.
      CLEAR  IT_ZSCO_ML_REPORT.
    ENDLOOP.
    CLEAR IT_MLCD.
  ENDLOOP.

ENDFORM.                    " ASSIGN_PC

*&---------------------------------------------------------------------*
*&      Form  READ_FIELD_LIST_DDIF
*&---------------------------------------------------------------------*
*       Read Field List (DDIF)
*----------------------------------------------------------------------*
*  -->  P_TABNAME STR. Name
*  -->  IT_L_DFTAB     FIELD_LIST TAB
*----------------------------------------------------------------------*
FORM READ_FIELD_LIST_DDIF TABLES    IT_L_DFTAB STRUCTURE DFIES
                          USING     P_TABNAME  TYPE DDOBJNAME.

  CLEAR : IT_L_DFTAB, IT_L_DFTAB[].

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME              = P_TABNAME
*     FIELDNAME            = ' '
*     LANGU                = SY-LANGU
*     LFIELDNAME           = ' '
*     ALL_TYPES            = ' '
*   IMPORTING
*     X030L_WA             =
*     DDOBJTYPE            =
*     DFIES_WA             =
*     LINES_DESCR          =
    TABLES
      DFIES_TAB            = IT_L_DFTAB
*     FIXED_VALUES         =
    EXCEPTIONS
      NOT_FOUND            = 1
      INTERNAL_ERROR       = 2
      OTHERS               = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " READ_FIELD_LIST_DDIF

*&---------------------------------------------------------------------*
*&      Form  DIS_TEST_LIST
*&---------------------------------------------------------------------*
*       Test List
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DIS_TEST_LIST.
* -- test
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK              = ' '
*     I_BYPASSING_BUFFER             =
*     I_BUFFER_ACTIVE                = ' '
*     I_CALLBACK_PROGRAM             = ' '
*     I_CALLBACK_PF_STATUS_SET       = ' '
*     I_CALLBACK_USER_COMMAND        = ' '
      I_STRUCTURE_NAME               = 'ZSCO_ML_REPORT'
*     IS_LAYOUT                      =
*     IT_FIELDCAT                    =
*     IT_EXCLUDING                   =
*     IT_SPECIAL_GROUPS              =
*     IT_SORT                        =
*     IT_FILTER                      =
*     IS_SEL_HIDE                    =
*     I_DEFAULT                      = 'X'
*     I_SAVE                         = ' '
*     IS_VARIANT                     =
*     IT_EVENTS                      =
*     IT_EVENT_EXIT                  =
*     IS_PRINT                       =
*     IS_REPREP_ID                   =
*     I_SCREEN_START_COLUMN          = 0
*     I_SCREEN_START_LINE            = 0
*     I_SCREEN_END_COLUMN            = 0
*     I_SCREEN_END_LINE              = 0
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER        =
*     ES_EXIT_CAUSED_BY_USER         =
    TABLES
      T_OUTTAB                       = IT_ZSCO_ML_REPORT
*   EXCEPTIONS
*     PROGRAM_ERROR                  = 1
*     OTHERS                         = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " DIS_TEST_LIST

*&---------------------------------------------------------------------*
*&      Form  PF_STATUS
*&---------------------------------------------------------------------*
*       Status
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PF_STATUS.
  SET PF-STATUS 'BASLIST'.
ENDFORM.                    " PF_STATUS
