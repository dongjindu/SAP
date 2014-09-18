*----------------------------------------------------------------------*
***INCLUDE ZACO11L_F001 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CHECK_BATCH
*&---------------------------------------------------------------------*
*       Check Batch_Mode
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_BATCH.
  DATA : LV_ANSWER.
  IF SY-BATCH <> 'X'.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        DEFAULTOPTION        = 'N'
        TEXTLINE1            = TEXT-010
        TEXTLINE2            = TEXT-011
        TITEL                = TEXT-012
*       START_COLUMN         = 25
*       START_ROW            = 6
        CANCEL_DISPLAY       = SPACE
      IMPORTING
        ANSWER               = LV_ANSWER.
    IF LV_ANSWER <> 'J'.
      MESSAGE E043.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_BATCH

*&---------------------------------------------------------------------*
*&      Form  READ_GENERAL_INFORMATION
*&---------------------------------------------------------------------*
*       Read CCode/Chart of ACC./Plant/Valuation Area/
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_GENERAL_INFORMATION.
* Read Company Code/Chart-Of-account
  CLEAR   DB_TKA01_TKA02.
  CLEAR : IT_DB_TKA01_TKA02, IT_DB_TKA01_TKA02[].
  SELECT *  INTO CORRESPONDING FIELDS OF TABLE IT_DB_TKA01_TKA02
            FROM DB_TKA01_TKA02
           WHERE KOKRS = P_KOKRS.
  IF IT_DB_TKA01_TKA02[] IS INITIAL.
    MESSAGE E038 WITH P_KOKRS.
  ENDIF.
* Read Plant/Valuation Area
  CLEAR : IT_ORG, IT_ORG[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ORG
           FROM T001K INNER JOIN T001W
             ON T001K~BWKEY = T001W~BWKEY
            FOR ALL ENTRIES IN  IT_DB_TKA01_TKA02
          WHERE T001K~BUKRS = IT_DB_TKA01_TKA02-BUKRS.
  IF IT_ORG[] IS INITIAL .
    MESSAGE E063.
  ENDIF.

ENDFORM.                    " READ_GENERAL_INFORMATION

*&---------------------------------------------------------------------*
*&      Form  READ_MVT_GRP
*&---------------------------------------------------------------------*
*       Read MVT Group
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_MVT_GRP.
  CLEAR : IT_CKMLMV010, IT_CKMLMV010[].
  SELECT * INTO TABLE IT_CKMLMV010
           FROM CKMLMV010.
  IF  IT_CKMLMV010[] IS INITIAL.
    MESSAGE E064.
  ENDIF.
ENDFORM.                    " READ_MVT_GRP

*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS_CATAGORY
*&---------------------------------------------------------------------*
*       Read Process Catagory
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_PROCESS_CATAGORY.
  CLEAR : IT_CKMLMV009, IT_CKMLMV009[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_CKMLMV009
           FROM CKMLMV009 .
  IF IT_CKMLMV009[] IS INITIAL.
    MESSAGE E065.
  ENDIF.

ENDFORM.                    " READ_PROCESS_CATAGORY

*&---------------------------------------------------------------------*
*&      Form  READ_MAT_KALNR
*&---------------------------------------------------------------------*
*       Read Costestimate Number and Material Code
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM READ_MAT_KALNR.
*
*  CLEAR : IT_MATNR, IT_MATNR[].
*
** Read All data (Now, No way to improve the performance)
** No Index In table -but should read all data.
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_MATNR
*           FROM CKMLKALNR.
*
** Please, refer to the standard program 'SAPRCKM_KALNR_CONSISTENCY'
** For detail explanation
*  SORT IT_MATNR BY OTABLE.
*  LOOP AT  IT_MATNR.
*    CASE IT_MATNR-OTABLE.
*        READ_KALNR  MBEW  KALN1.
*        READ_KALNR  EBEW  KALN1.
*        READ_KALNR  QBEW  KALN1.
*        READ_KALNR  OBEW  KALN1.
*        READ_KALNR  CKMLLAHD   KALNR.
*        READ_KALNR  CKMLMV001  KALNR.
*        READ_KALNR  CKMLMV005  KALNR.
*    ENDCASE.
*    CLEAR IT_MATNR.
*  ENDLOOP.
** Delete records which have no matched materials
** ex> 'Procurement alternative/process'
*  DELETE   IT_MATNR  WHERE MATNR EQ SPACE.
*  CLEAR IT_MATNR.
** Set Plant key
*  CLEAR IT_ORG.
*  LOOP AT IT_ORG.
*    IT_MATNR-WERKS = IT_ORG-WERKS.
*    MODIFY IT_MATNR TRANSPORTING WERKS
*                    WHERE BWKEY = IT_ORG-BWKEY.
*
*  ENDLOOP.
*ENDFORM.                    " READ_MAT_KALNR

*&---------------------------------------------------------------------*
*&      Form  CHECK_OTHER_WAY
*&---------------------------------------------------------------------*
*       Check Costing Number
*----------------------------------------------------------------------*
*      -->P_IT_MATNR  text
*      <--P_NOT_FOUND  text
*      <--P_INCONSISTENT  text
*----------------------------------------------------------------------*
*FORM CHECK_OTHER_WAY USING    PLF_CKMLKALNR TYPE CKMLKALNR
*                     CHANGING NOT_FOUND     TYPE t_CKMLKALNR
*                              INCONSISTENT  TYPE T_INCONS.
*
*  DATA: LF_CKMLKALNR   TYPE CKMLKALNR,
*        LWA_INCONS     TYPE INCONS.
*
*  LF_CKMLKALNR = PLF_CKMLKALNR.
*  CLEAR: LF_CKMLKALNR-TYPE,
*         LF_CKMLKALNR-OTABLE.
*
*  CALL FUNCTION 'CKMS_KALNR_INFO_GET'
*       EXPORTING
**           REFRESH_BUFFER    = 'X'
*            IF_CKMLKALNR      = LF_CKMLKALNR
*            CONSISTENCY_CHECK = 'X'
*       IMPORTING
*            EF_CKMLKALNR      = LF_CKMLKALNR
*       EXCEPTIONS
*            INTERFACE_ERROR   = 1
*            OTHERS            = 2.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
*  IF LF_CKMLKALNR-OTABLE IS INITIAL.
*    APPEND PLF_CKMLKALNR TO NOT_FOUND.
*  ELSE.
*    LWA_INCONS = PLF_CKMLKALNR.
*    LWA_INCONS-TABLE = LF_CKMLKALNR-OTABLE.
*    APPEND LWA_INCONS TO INCONSISTENT.
*  ENDIF.
*
*ENDFORM.                               " CHECK_OTHER_WAY

*&---------------------------------------------------------------------*
*&      Form  CHECK_CUR_WPRO
*&---------------------------------------------------------------------*
*       Check Work Processes
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_CUR_WPRO.
* No 0 Process
  IF  P_PRONO < '3'.
    MESSAGE E068.
  ENDIF.

  DATA: LV_REASON_LOCAL_RESOURCES TYPE C,
        LV_OPCODE_LOCAL_RESOURCES TYPE X VALUE 13.

  DATA  : LV_CURRENT_RESOURCES  LIKE  SY-INDEX,
          LV_MAXIMAL_RESOURCES  LIKE  SY-INDEX,
          LV_RECOMMENDED_DELAY  LIKE  SY-INDEX.

* Get Sys. Info.
  CALL 'ThSysInfo' ID 'OPCODE'   FIELD LV_OPCODE_LOCAL_RESOURCES
                   ID 'NORES'    FIELD LV_CURRENT_RESOURCES
                   ID 'MAXRES'   FIELD LV_MAXIMAL_RESOURCES
                   ID 'WAIT'     FIELD LV_RECOMMENDED_DELAY
                   ID 'REASON'   FIELD LV_REASON_LOCAL_RESOURCES.
  IF SY-SUBRC <> 0.
*    MESSAGE E067 WITH P_PRONO LV_CURRENT_RESOURCES.
  ENDIF.

  IF P_PRONO > LV_CURRENT_RESOURCES.
    MESSAGE E067 WITH P_PRONO LV_CURRENT_RESOURCES.
  ENDIF.

ENDFORM.                    " CHECK_CUR_WPRO

*&---------------------------------------------------------------------*
*&      Form  ENQUEUE_ZTCO_MATLEDGER
*&---------------------------------------------------------------------*
*       Enqueue
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ENQUEUE_ZTCO_MATLEDGER.

  CALL FUNCTION 'ENQUEUE_EZCO_MATLEDGER'
    EXPORTING
      MODE_ZTCO_MATLEDGER       = 'E'
      MANDT                     = SY-MANDT
*      KALNR                     =
      BDATJ                     = P_BDATJ
      POPER                     = P_POPER
      CURTP                     = P_CURTP
*     KATEGORIE                 =
*     BEWARTGRP                 =
*     FELDG                     =
*     X_KALNR                   = ' '
*     X_BDATJ                   = ' '
*     X_POPER                   = ' '
*     X_CURTP                   = ' '
*     X_KATEGORIE               = ' '
*     X_BEWARTGRP               = ' '
*     X_FELDG                   = ' '
*     _SCOPE                    = '2'
*     _WAIT                     = ' '
*     _COLLECT                  = ' '
    EXCEPTIONS
      FOREIGN_LOCK              = 1
      SYSTEM_FAILURE            = 2
      OTHERS                    = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " ENQUEUE_ZTCO_MATLEDGER
