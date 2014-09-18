************************************************************************
* Program Name      : ZEMMPM46E_JIS_GR_NON_PARALLEL
* Author            :
* Creation Date     :
* Specifications By :
* Pattern           :
* Development Request: copy from ZEMMPM46E_JIS_GR for serious processing
* Addl Documentation:
* Description       : JIS GR Program
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 08.13.2014      Victor     T-code has been deleted for APM
************************************************************************

REPORT ZEMMPM46E_JIS_GR_NON_PARALLEL NO STANDARD PAGE HEADING
                        LINE-SIZE 132
                        LINE-COUNT 64(1)
                        MESSAGE-ID ZMMM.

**---
INCLUDE : ZRMMPMXXR_INCL.


**--- Internal Tables
DATA : BEGIN OF WA_ITAB ,
         LIFNR LIKE EKKO-LIFNR,     " vendor
         NAME1 LIKE LFA1-NAME1,     " vendor desc.
         EBELN LIKE EKKO-EBELN,     " scheduling agreement number
         MATNR LIKE EKPO-MATNR,     " material number
         TXZ01 LIKE EKPO-TXZ01,     " material desc.
         LABST LIKE MARD-LABST,     " quantity
         MEINS LIKE EKPO-MEINS,     " unit of measure
         WERKS LIKE EKPO-WERKS,
         LGORT LIKE EKPO-LGORT,
         EBELP LIKE EKPO-EBELP,
         MBLNR LIKE MKPF-MBLNR,     " GR document number
         MJAHR LIKE MKPF-MJAHR,
         MESSA(80),
         LINECOLOR(4),     " ALV Color
       END OF WA_ITAB.
DATA: BEGIN OF WA_BAPI_SUC,
         LIFNR LIKE EKKO-LIFNR,     " vendor
         EBELN LIKE EKPO-EBELN,
         EBELP LIKE EKPO-EBELP,
         MBLNR LIKE MKPF-MBLNR,     " GR document number
         MJAHR LIKE MKPF-MJAHR,
         SUMSG(80) TYPE C,
      END OF WA_BAPI_SUC.
DATA: BEGIN OF WA_BAPI_ERR,
        EBELN LIKE EKPO-EBELN,
        EBELP LIKE EKPO-EBELP,
        ERMSG LIKE BAPIRET2-MESSAGE,
       END OF WA_BAPI_ERR.
DATA: WA_PRINT TYPE SLIS_PRINT_ALV.
DATA : IT_ITAB LIKE TABLE OF WA_ITAB,
       IT_BAPI_SUC LIKE TABLE OF WA_BAPI_SUC,
       IT_BAPI_ERR LIKE TABLE OF WA_BAPI_ERR.
FIELD-SYMBOLS: <FS_ITAB> LIKE LINE OF IT_ITAB.

**--- BAPI
DATA : W_GOODSMVT_HEADER  LIKE BAPI2017_GM_HEAD_01,
       W_GOODSMVT_CODE    LIKE BAPI2017_GM_CODE,
       W_GOODSMVT_HEADRET LIKE BAPI2017_GM_HEAD_RET,
       W_MATERIALDOCUMENT LIKE BAPI2017_GM_HEAD_RET-MAT_DOC,
       W_MATDOCUMENTYEAR  LIKE BAPI2017_GM_HEAD_RET-DOC_YEAR,
       IT_GOODSMVT_ITEM
               LIKE TABLE OF BAPI2017_GM_ITEM_CREATE  WITH HEADER LINE,
       IT_GOODSMVT_SERIALNUMBER
               LIKE TABLE OF BAPI2017_GM_SERIALNUMBER WITH HEADER LINE,
       IT_RETURN LIKE TABLE OF BAPIRET2.

DATA : W_UZEIT TYPE T.
*&-----Parallel process
*DATA: W_TASKNAME(4) TYPE N VALUE '0001',
*      W_SND_JOBS    TYPE I VALUE 1,
*      W_RCV_JOBS    TYPE I VALUE 1,
*      W_EXCEP_FLAG  TYPE C.
*&---end

**---
CONSTANTS : C_BSTYP LIKE EKKO-BSTYP VALUE 'L',
            C_BSART LIKE EKKO-BSART VALUE 'JIS',
            C_MTART LIKE EKPO-MTART VALUE 'ROH',
            C_LGORT LIKE EKPO-LGORT VALUE 'P500',
            L_LGORT_499 LIKE EKPO-LGORT VALUE 'P499',
            C_BWART LIKE MSEG-BWART VALUE '101',
            C_GM_CODE LIKE W_GOODSMVT_CODE-GM_CODE VALUE '01',
            C_KZBEW LIKE BAPI2017_GM_ITEM_CREATE-MVT_IND VALUE 'B',
            C_UZEIT_000000 TYPE T VALUE '000000',
            C_UZEIT_055959 TYPE T VALUE '055959'.


**--- Macro
DEFINE APPEND_FIELDCAT.
  &1 = &1 + 1.
  W_FIELDCAT-COL_POS    = &1.
  W_FIELDCAT-FIELDNAME  = &2.
  W_FIELDCAT-OUTPUTLEN  = &3.
  W_FIELDCAT-SELTEXT_L  = &4.
  W_FIELDCAT-SELTEXT_M  = &4.
  W_FIELDCAT-SELTEXT_S  = &4.
  W_FIELDCAT-DATATYPE   = &5.
  W_FIELDCAT-KEY        = &6.
  W_FIELDCAT-QFIELDNAME = &7.
  W_FIELDCAT-CFIELDNAME = &8.
  APPEND W_FIELDCAT.
  CLEAR : W_FIELDCAT.
END-OF-DEFINITION.

DEFINE APPEND_TOP.
  CLEAR : W_LINE.
  IF NOT &3 IS INITIAL OR NOT &4 IS INITIAL.
    W_LINE-TYP   = &1.
    W_LINE-KEY   = &2.
    CONCATENATE &3 '~' &4 INTO W_LINE-INFO SEPARATED BY SPACE.
    APPEND W_LINE TO W_TOP_OF_PAGE.
  ENDIF.
END-OF-DEFINITION.

DEFINE APPEND_SORTCAT.
  W_SORTCAT-SPOS      = &1.
  W_SORTCAT-FIELDNAME = &2.
  W_SORTCAT-TABNAME   = &3.
  W_SORTCAT-UP        = &4.
  W_SORTCAT-SUBTOT    = &5.
  APPEND W_SORTCAT.
  CLEAR : W_SORTCAT.
END-OF-DEFINITION.

**---
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_EBELN FOR EKKO-EBELN,
                 S_MATNR FOR EKPO-MATNR.
SELECTION-SCREEN ULINE.
PARAMETERS : P_BUDAT LIKE MKPF-BUDAT,
             P_UZEIT LIKE SY-UZEIT.
SELECTION-SCREEN END OF BLOCK BLOCK1.

*PARAMETERS: P_SRVGRP LIKE RZLLITAB-CLASSNAME OBLIGATORY
*                     DEFAULT 'PG_JIS' ,
PARAMETERS: P_TEST AS CHECKBOX DEFAULT 'X',
            P_NOENT(3) TYPE N DEFAULT 30.
DATA: W_CLASSNAME LIKE RZLLITAB-CLASSNAME.

AT SELECTION-SCREEN.
*  SELECT SINGLE CLASSNAME INTO W_CLASSNAME
*                          FROM RZLLITAB
*                          WHERE CLASSNAME = P_SRVGRP.
*  IF SY-SUBRC NE 0.
*    MESSAGE ID 'ZMMM' TYPE 'I' NUMBER '009' WITH TEXT-003.
*    LEAVE PROGRAM.
*  ENDIF.
**---
INITIALIZATION.
  PERFORM EVENT_BUILD USING W_EVENTCAT[].

**---
TOP-OF-PAGE.
  PERFORM TOP_OF_PAGE.


**---
START-OF-SELECTION.
  PERFORM GET_DATA.


**---
END-OF-SELECTION.
  IF IT_ITAB[] IS INITIAL.
    MESSAGE S999 WITH TEXT-M01.
  ELSE.
    PERFORM POSTING_GR.
    FREE: IT_BAPI_ERR, IT_BAPI_SUC,IT_GOODSMVT_ITEM,
          IT_GOODSMVT_SERIALNUMBER, W_GOODSMVT_HEADER,
           W_GOODSMVT_CODE, W_GOODSMVT_HEADRET.
    PERFORM COMMENT_BUILD.     " USING w_top_of_page[].
    PERFORM MAKE_ALV_GRID.
  ENDIF.






*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  DATA: L_LABST LIKE MARD-LABST,
        L_INDEX LIKE SY-TABIX.
*---
*  CLEAR : it_temp, it_temp[], it_itab, it_itab[].
  CLEAR : IT_ITAB, IT_ITAB[].

  SELECT A~LIFNR NAME1 A~EBELN B~MATNR B~TXZ01
                 LABST B~MEINS B~WERKS B~LGORT B~EBELP
       INTO TABLE IT_ITAB
       FROM EKKO AS A
       INNER JOIN EKPO AS B
       ON A~MANDT EQ B~MANDT
       AND A~EBELN EQ B~EBELN
       INNER JOIN MARD AS C
       ON B~MANDT EQ C~MANDT
       AND B~MATNR EQ C~MATNR
       AND B~WERKS EQ C~WERKS
       INNER JOIN LFA1 AS D
       ON A~MANDT EQ D~MANDT
       AND A~LIFNR EQ D~LIFNR
       WHERE A~BSTYP EQ C_BSTYP     " document category : L
       AND A~BSART EQ C_BSART     " document type : JIS
       AND B~MTART EQ C_MTART     " material type : ROH
       AND B~LGORT EQ C_LGORT     " storage location : P500
       AND A~LOEKZ EQ SPACE       " Del. Ind.
       AND B~LOEKZ EQ SPACE       " Del. Ind.
       AND C~LABST LT 0
       AND C~MATNR IN S_MATNR     " material number
       AND C~LGORT EQ C_LGORT     " storage location
       AND A~EBELN IN S_EBELN     " scheduling agreement
       AND B~ELIKZ EQ SPACE.      " delivery compl. ind.

** Chnaged by Furong on 10/16/09
  LOOP AT IT_ITAB INTO WA_ITAB.
    SELECT SINGLE LABST INTO L_LABST
      FROM MARD
      WHERE MATNR = WA_ITAB-MATNR
        AND WERKS = 'P001'
        AND LGORT = L_LGORT_499.
    IF SY-SUBRC = 0.
      WA_ITAB-LABST = WA_ITAB-LABST + L_LABST.
      MODIFY IT_ITAB FROM WA_ITAB.
      CLEAR: WA_ITAB.
    ENDIF.
  ENDLOOP.
** End of change

** Chnaged by Furong on 02/09/10
  LOOP AT IT_ITAB INTO WA_ITAB.
    L_INDEX = SY-TABIX.
    IF WA_ITAB-LABST >= 0.
      DELETE IT_ITAB INDEX L_INDEX.
    ENDIF.
    CLEAR: WA_ITAB.
  ENDLOOP.
** End of change on 02/09/10


*---
*  LOOP AT it_temp.
*    MOVE-CORRESPONDING it_temp TO it_itab.
*    it_itab-labst = it_temp-labst * ( - 1 ).
*    APPEND it_itab.
*    CLEAR : it_temp, it_itab.
*  ENDLOOP.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COMMENT_BUILD.
*---
  CLEAR : W_LINE.
  W_LINE-TYP  = 'H'.
  W_LINE-INFO = TEXT-002.
  APPEND W_LINE TO W_TOP_OF_PAGE.

  CLEAR : W_LINE.
  APPEND INITIAL LINE TO W_TOP_OF_PAGE.
ENDFORM.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_ALV_GRID.
*---
  MOVE : 'LINECOLOR' TO W_LAYOUT-INFO_FIELDNAME,
         'X'         TO W_LAYOUT-COLWIDTH_OPTIMIZE.

  PERFORM BUILD_FIELDCAT.
  PERFORM BUILD_SORTCAT.

  CLEAR : W_PROGRAM.

  MOVE : SY-REPID TO W_PROGRAM.
*  wa_print-print = 'X'.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM = W_PROGRAM
            IS_LAYOUT          = W_LAYOUT
            IT_FIELDCAT        = W_FIELDCAT[]
            IT_EVENTS          = W_EVENTCAT[]
            IT_SORT            = W_SORTCAT[]
            I_SAVE             = 'A'
*            is_print           = wa_print
       TABLES
            T_OUTTAB           = IT_ITAB
       EXCEPTIONS
            PROGRAM_ERROR      = 1
            OTHERS             = 2.
ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  posting_gr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POSTING_GR.
*---
  DATA: BEGIN OF WA_VENDOR,
          LIFNR LIKE LFA1-LIFNR,
          COUNT TYPE I,
        END OF WA_VENDOR.
  DATA: IT_VENDOR LIKE TABLE OF WA_VENDOR.

  DATA: W_IDX TYPE I VALUE 1,
        W_CNT TYPE I,
        W_GRP_CNT TYPE I.

  CLEAR: W_UZEIT.

  MOVE: SY-UZEIT TO W_UZEIT.
*&---calculate no.of records for each vendor.
  LOOP AT IT_ITAB ASSIGNING <FS_ITAB>.
    WA_VENDOR-LIFNR = <FS_ITAB>-LIFNR.
    WA_VENDOR-COUNT = 1.
    COLLECT WA_VENDOR INTO IT_VENDOR.
  ENDLOOP.

  SORT: IT_VENDOR BY LIFNR,
        IT_ITAB BY LIFNR EBELN.
  LOOP AT IT_VENDOR INTO WA_VENDOR.
    LOOP AT IT_ITAB ASSIGNING <FS_ITAB> FROM W_IDX.
      IF WA_VENDOR-LIFNR NE <FS_ITAB>-LIFNR.
        W_IDX = SY-TABIX.
        CLEAR : W_CNT, W_GRP_CNT,
                W_GOODSMVT_HEADER, W_GOODSMVT_CODE,W_GOODSMVT_HEADRET,
                W_MATERIALDOCUMENT, W_MATDOCUMENTYEAR,
                IT_GOODSMVT_ITEM, IT_GOODSMVT_ITEM[],
                IT_GOODSMVT_SERIALNUMBER, IT_GOODSMVT_SERIALNUMBER[].
        EXIT.
      ELSE.
        W_CNT = W_CNT + 1.
        W_GRP_CNT = W_GRP_CNT + 1.
        PERFORM CLEAR_DATA_MOVE.
        IF W_CNT EQ P_NOENT OR W_GRP_CNT EQ WA_VENDOR-COUNT.
          PERFORM CALL_BAPI.
          CLEAR : W_CNT,W_GOODSMVT_HEADER,W_GOODSMVT_CODE,
                W_GOODSMVT_HEADRET,W_MATERIALDOCUMENT,W_MATDOCUMENTYEAR,
                  IT_GOODSMVT_ITEM, IT_GOODSMVT_ITEM[],
                  IT_GOODSMVT_SERIALNUMBER, IT_GOODSMVT_SERIALNUMBER[].
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  PERFORM MODIFY_ITAB.
ENDFORM.                    " posting_gr

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCAT.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : color
  APPEND_FIELDCAT :
    W_COL_POS 'LIFNR' 10 'Vendor'         'CHAR' 'X' ''      '',
    W_COL_POS 'NAME1' 20 'Vendor Name'    'CHAR' 'X' ''      '',
    W_COL_POS 'EBELN' 10 'SA Number'      'CHAR' 'X' ''      '',
    W_COL_POS 'MATNR' 18 'Material'       'CHAR' 'X' ''      '',
    W_COL_POS 'TXZ01' 20 'Material Name'  'CHAR' ''  ''      '',
    W_COL_POS 'LABST' 12 'Quantity'       'QUAN' ''  'MEINS' '',
    W_COL_POS 'MEINS'  3 'UoM'            'CHAR' ''  ''      '',
    W_COL_POS 'MBLNR' 10 'Document No'    'CHAR' ''  ''      '',
    W_COL_POS 'MESSA' 80 'Message'        'CHAR' ''  ''      ''.
ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_sortcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_SORTCAT.
**--- &1 : position     &2 : field name     &3 : tab name
**--- &4 : up           &5 : sub total
  APPEND_SORTCAT : '1' 'LIFNR' 'IT_ITAB' 'X' '',
                   '2' 'EBELN' 'IT_ITAB' 'X' ''.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  clear_data_move
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_DATA_MOVE.

*---
  MOVE : <FS_ITAB>-MATNR     TO IT_GOODSMVT_ITEM-MATERIAL,
         <FS_ITAB>-WERKS     TO IT_GOODSMVT_ITEM-PLANT,
         <FS_ITAB>-LGORT     TO IT_GOODSMVT_ITEM-STGE_LOC,
         C_BWART             TO IT_GOODSMVT_ITEM-MOVE_TYPE,
         <FS_ITAB>-LIFNR     TO IT_GOODSMVT_ITEM-VENDOR,
*         it_itab-labst     TO it_goodsmvt_item-entry_qnt,
         <FS_ITAB>-MEINS     TO IT_GOODSMVT_ITEM-ENTRY_UOM,
         <FS_ITAB>-MEINS     TO IT_GOODSMVT_ITEM-ENTRY_UOM_ISO,
         <FS_ITAB>-EBELN     TO IT_GOODSMVT_ITEM-PO_NUMBER,
         <FS_ITAB>-EBELP     TO IT_GOODSMVT_ITEM-PO_ITEM,
         C_KZBEW           TO IT_GOODSMVT_ITEM-MVT_IND.
  IT_GOODSMVT_ITEM-ENTRY_QNT = <FS_ITAB>-LABST * -1.

  APPEND IT_GOODSMVT_ITEM.
ENDFORM.                    " clear_data_move

*&---------------------------------------------------------------------*
*&      Form  call_bapi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_BAPI.
*---
  DATA : L_BUDAT TYPE D.

  DATA: WA_ITEM_INFO LIKE BAPI2017_GM_ITEM_CREATE.
  DATA: IT_ITEM_INFO LIKE TABLE OF WA_ITEM_INFO.

  FIELD-SYMBOLS: <FS_RET> LIKE LINE OF IT_RETURN.
*---
  IF P_BUDAT IS INITIAL.
** FURONG ON 06/13/12 for 3 shift
*    IF W_UZEIT GE C_UZEIT_000000 AND
*       W_UZEIT LE C_UZEIT_055959.
    IF W_UZEIT GE C_UZEIT_000000 AND
         W_UZEIT LE P_UZEIT.
** End on 06/13/12
      L_BUDAT = SY-DATUM - 1.
    ENDIF.
    MOVE : L_BUDAT   TO W_GOODSMVT_HEADER-PSTNG_DATE.   " posting date
*    MOVE : sy-datum  TO w_goodsmvt_header-pstng_date.   " posting date
  ELSEIF NOT P_BUDAT IS INITIAL.
    MOVE : P_BUDAT   TO W_GOODSMVT_HEADER-PSTNG_DATE.   " posting date
  ENDIF.

  CONCATENATE  'JIS-GR' W_GOODSMVT_HEADER-PSTNG_DATE
          INTO W_GOODSMVT_HEADER-REF_DOC_NO.

  MOVE : SY-DATUM  TO W_GOODSMVT_HEADER-DOC_DATE,
         C_GM_CODE TO W_GOODSMVT_CODE-GM_CODE.
** Change by Furong on 04/24/08

  CALL FUNCTION 'Z_FMM_JISGR_CREATE'
*       STARTING NEW TASK W_TASKNAME
*       DESTINATION IN GROUP P_SRVGRP
*       PERFORMING GR_INFO ON END OF TASK
   EXPORTING
     WA_GDSMVT_HDR  = W_GOODSMVT_HEADER
     WA_GDSMVT_CODE = W_GOODSMVT_CODE
     W_TSTRUN       = P_TEST
   IMPORTING
      W_MATDOC         = W_MATERIALDOCUMENT
      W_MDYEAR         = W_MATDOCUMENTYEAR
   TABLES
     IT_GDSMVT_ITM = IT_GOODSMVT_ITEM
     IT_GDSMVT_SNO = IT_GOODSMVT_SERIALNUMBER
     IT_RET        = IT_RETURN
   EXCEPTIONS
     COMMUNICATION_FAILURE = 1
     SYSTEM_FAILURE        = 2
     RESOURCE_FAILURE      = 3.


  READ TABLE IT_RETURN ASSIGNING <FS_RET> WITH KEY TYPE = 'E'.
  IF SY-SUBRC EQ 0.
    LOOP AT IT_RETURN ASSIGNING <FS_RET>.
      READ TABLE IT_GOODSMVT_ITEM INDEX <FS_RET>-ROW.
      IF SY-SUBRC NE 0.
      ELSE.
        WA_BAPI_ERR-EBELN = IT_GOODSMVT_ITEM-PO_NUMBER.
        WA_BAPI_ERR-EBELP = IT_GOODSMVT_ITEM-PO_ITEM.
        WA_BAPI_ERR-ERMSG = <FS_RET>-MESSAGE.
        APPEND WA_BAPI_ERR TO IT_BAPI_ERR.
      ENDIF.
      CLEAR: IT_GOODSMVT_ITEM, WA_BAPI_ERR.
    ENDLOOP.
  ELSE.
    IT_ITEM_INFO[] = IT_GOODSMVT_ITEM[].
    LOOP AT IT_ITEM_INFO INTO WA_ITEM_INFO.
      WA_BAPI_SUC-LIFNR = WA_ITEM_INFO-VENDOR.
      WA_BAPI_SUC-EBELN = WA_ITEM_INFO-PO_NUMBER.
      WA_BAPI_SUC-EBELP = WA_ITEM_INFO-PO_ITEM.
      WA_BAPI_SUC-MBLNR = W_MATERIALDOCUMENT.
      WA_BAPI_SUC-MJAHR = W_MATDOCUMENTYEAR.
      WA_BAPI_SUC-SUMSG = 'GR Document created'.
      APPEND WA_BAPI_SUC TO IT_BAPI_SUC.
      CLEAR: WA_BAPI_SUC.
    ENDLOOP.
  ENDIF.
  REFRESH: IT_RETURN, IT_ITEM_INFO.

ENDFORM.                    " call_bapi

*&---------------------------------------------------------------------*
*&      Form  modify_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_ITAB.

  CLEAR: WA_ITAB.

  LOOP AT IT_BAPI_SUC INTO WA_BAPI_SUC.
    WA_ITAB-MBLNR     = WA_BAPI_SUC-MBLNR.
    WA_ITAB-MJAHR     = WA_BAPI_SUC-MJAHR.
    WA_ITAB-MESSA     = WA_BAPI_SUC-SUMSG.
    WA_ITAB-LINECOLOR = C_GREEN.
    MODIFY IT_ITAB FROM WA_ITAB
                   TRANSPORTING LINECOLOR MESSA MBLNR MJAHR
                   WHERE LIFNR = WA_BAPI_SUC-LIFNR
                   AND   EBELN = WA_BAPI_SUC-EBELN
                   AND   EBELP = WA_BAPI_SUC-EBELP.
    CLEAR: WA_ITAB.
  ENDLOOP.
  LOOP AT IT_BAPI_ERR INTO WA_BAPI_ERR.
    WA_ITAB-MESSA     = WA_BAPI_ERR-ERMSG.
    WA_ITAB-LINECOLOR = C_RED.
    MODIFY IT_ITAB FROM WA_ITAB TRANSPORTING MESSA LINECOLOR
                                WHERE EBELN = WA_BAPI_ERR-EBELN
                                AND   EBELP = WA_BAPI_ERR-EBELP.
    CLEAR: WA_ITAB.
  ENDLOOP.

ENDFORM.                    " modify_itab
