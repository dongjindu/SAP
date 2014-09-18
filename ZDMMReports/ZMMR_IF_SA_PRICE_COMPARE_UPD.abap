************************************************************************
* Program Name      : ZMMR_IF_SA_PRICE_COMPARE_UPD
* Author            : Furong Wang
* Creation Date     : 05/2006
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Price comparison (SA & Info Record)
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 05/24/06        Furong Wang                     Condition ZPXX not
**                                                included
************************************************************************


REPORT ZEMMPM29E_CD_CHANGE_S NO STANDARD PAGE HEADING
                             LINE-SIZE 132
                             LINE-COUNT 64(1)
                             MESSAGE-ID ZMMM.
**---
INCLUDE : ZRMMPMXXR_INCL.

**--- Internal Tables
DATA : BEGIN OF IT_TEMP OCCURS 0,
         EBELN LIKE EKKO-EBELN,
         EBELP LIKE EKPO-EBELP,
         LIFNR LIKE EKKO-LIFNR,
         MATNR LIKE EKPO-MATNR,
         WERKS LIKE EKPO-WERKS,
         LGORT LIKE EKPO-LGORT,
         KSCHL LIKE KONP-KSCHL,
         KONWA LIKE KONP-KONWA,
         KZUST LIKE KONH-KZUST,
         KBETR_SA LIKE KONP-KBETR,
         KPEIN_SA LIKE KONP-KPEIN,
         DATAB_SA LIKE A018-DATAB,
         DATBI_SA LIKE A018-DATBI,
         DATAB LIKE A018-DATAB,
         DATBI LIKE A018-DATBI,
         KBETR LIKE KONP-KBETR,
         KPEIN LIKE KONP-KPEIN,
         LINECOLOR(4),     " ALV Color
         UPD_SA(1),
       END OF IT_TEMP.

DATA : IT_ITAB LIKE IT_TEMP OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF IT_SA_ALL OCCURS 0,
         EBELN LIKE EKPO-EBELN,
         EBELP LIKE EKPO-EBELP,
         LIFNR LIKE EKKO-LIFNR,
         MATNR LIKE EKPO-MATNR,
         WERKS LIKE EKPO-WERKS,
         LGORT LIKE EKPO-LGORT,
         ETFZ1 LIKE EKPO-ETFZ1,
         BSTYP LIKE EKKO-BSTYP,
         BUKRS LIKE EKKO-BUKRS,
         BSART LIKE EKKO-BSART,
         EKORG LIKE EKKO-EKORG,
         EKGRP LIKE EKKO-EKGRP,
         KDATB LIKE EKKO-KDATB,
         KDATE LIKE EKKO-KDATE,
       END OF IT_SA_ALL.

DATA : BEGIN OF IT_INFO_ITEM OCCURS 0,
         KAPPL LIKE KONP-KAPPL,
         KSCHL LIKE KONP-KSCHL,
         KBETR LIKE KONP-KBETR,
         KPEIN LIKE KONP-KPEIN,
         KONWA LIKE KONP-KONWA,
         LIFNR LIKE KONP-LIFNR,
         KZUST LIKE KONH-KZUST,
         DATAB LIKE KONH-DATAB,
         DATBI LIKE KONH-DATBI,
       END OF IT_INFO_ITEM.

DATA : IT_SACOND_ITEM LIKE IT_INFO_ITEM OCCURS 0 WITH HEADER LINE.

DATA: IT_MAIL TYPE STANDARD TABLE OF SOLISTI1 INITIAL SIZE 0
                  WITH HEADER LINE.

DATA: IT_T683S LIKE TABLE OF T683S WITH HEADER LINE.

** wokking variant
DATA : W_SUBRC LIKE SY-SUBRC.

DATA : W_KNUMH LIKE KONH-KNUMH.

** bdc
DATA : BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF IT_BDC.

DATA : BEGIN OF IT_MESS OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF IT_MESS.

DATA : IT_MESSAGE LIKE IT_MESS OCCURS 0 WITH HEADER LINE.

DATA : W_MODE LIKE CTU_PARAMS-DISMODE VALUE 'N'.  "'A'.

DATA: W_INFO_DATAB LIKE SY-DATUM,
      W_INFO_DATBI LIKE SY-DATUM.

DATA: W_UPD(1).
DATA: IT_ZTMM_IFSA_LOG LIKE TABLE OF ZTMM_IFSA_LOG WITH HEADER LINE.
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

SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_EKORG FOR EKKO-EKORG DEFAULT 'PU01'.
SELECT-OPTIONS : S_LIFNR FOR EKKO-LIFNR.
SELECT-OPTIONS : S_MATNR FOR EKPO-MATNR.
PARAMETERS : P_DATE LIKE EKKO-AEDAT OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETERS: P_PR RADIOBUTTON GROUP GRP.
PARAMETERS: P_PRDT RADIOBUTTON GROUP GRP DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) TEXT-081 FOR FIELD P_HIS.
PARAMETERS: P_HIS AS CHECKBOX.
SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN SKIP.
*PARAMETERS: p_test AS CHECKBOX DEFAULT 'X'.
*SELECT-OPTIONS : s_ebeln FOR ekko-ebeln.
SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-005.
PARAMETERS: P_SIM AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK BLOCK2.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK3 WITH FRAME TITLE TEXT-004.
PARAMETERS: P_SEND AS CHECKBOX DEFAULT 'X' USER-COMMAND UCOM.
PARAMETERS: P_EMAIL(40) DEFAULT 'SAIFVAL' MODIF ID MD3.
SELECTION-SCREEN END OF BLOCK BLOCK3.

**---
INITIALIZATION.
  PERFORM EVENT_BUILD USING W_EVENTCAT[].
  PERFORM SET_PARAMETERS.
**---
TOP-OF-PAGE.
  PERFORM TOP_OF_PAGE.

**---

START-OF-SELECTION.
  if P_PR = 'X'.
    MESSAGE S999 WITH 'Do not use this function'.
    exit.
  endif.
  PERFORM GET_SCHEDULING_AGREEMENT.

  IF IT_SA_ALL[] IS INITIAL.
    MESSAGE S999 WITH TEXT-M01.
  ELSE.
    PERFORM GET_COND_PRICE.
    PERFORM CHANGE_CONDITIONS.
    PERFORM COMMENT_BUILD.     " USING w_top_of_page[].
    PERFORM MAKE_ALV_GRID.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  change_conditions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_CONDITIONS.
  CLEAR : IT_ITAB, IT_ITAB[].
  LOOP AT IT_SA_ALL.
    PERFORM GET_INFO_ITEM_CONDITION.
    PERFORM GET_SA_ITEM_CONDITION.
    SORT IT_INFO_ITEM BY KSCHL DESCENDING.
    SORT IT_SACOND_ITEM BY KSCHL DESCENDING.
    IF P_HIS IS INITIAL.
      READ TABLE IT_INFO_ITEM WITH KEY KSCHL = 'PB00'.
      READ TABLE IT_SACOND_ITEM WITH KEY KSCHL = 'PB00'.
      IF   IT_SACOND_ITEM-KBETR NE IT_INFO_ITEM-KBETR OR
       IT_SACOND_ITEM-KPEIN ne IT_INFO_ITEM-KPEIN.
        IF P_SIM IS INITIAL.
          PERFORM CHANGE_SA_CONDITION.
        ENDIF.
        PERFORM FILL_TABLE.
        CLEAR: W_UPD.
      ENDIF.
    ELSE.
      CHECK IT_INFO_ITEM[] NE IT_SACOND_ITEM[].
      IF P_SIM IS INITIAL.
        PERFORM CHANGE_SA_CONDITION.
      ENDIF.
      PERFORM FILL_TABLE.
      CLEAR: W_UPD.
    ENDIF.
  ENDLOOP.

  IF NOT IT_ZTMM_IFSA_LOG[] IS INITIAL.
    MODIFY ZTMM_IFSA_LOG FROM TABLE IT_ZTMM_IFSA_LOG.
    IF SY-SUBRC EQ 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.
  IF P_SEND = 'X'.
    IF IT_ITAB[] IS INITIAL.
    ELSE.
      PERFORM SEND_EMAIL.
    ENDIF.
  ENDIF.
ENDFORM.                    " change_conditions

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
  CLEAR : W_LINE, W_TOP_OF_PAGE, W_TOP_OF_PAGE[].
  W_LINE-TYP  = 'H'.
  W_LINE-INFO = TEXT-002.
  APPEND W_LINE TO W_TOP_OF_PAGE.

*  CLEAR : w_line.
*  APPEND INITIAL LINE TO w_top_of_page.
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
  CLEAR: W_FIELDCAT, W_FIELDCAT[], W_SORTCAT, W_SORTCAT[].
  MOVE : 'LINECOLOR' TO W_LAYOUT-INFO_FIELDNAME,
         'X'         TO W_LAYOUT-COLWIDTH_OPTIMIZE.

  PERFORM BUILD_FIELDCAT.
  PERFORM BUILD_SORTCAT.

  CLEAR : W_PROGRAM.

  MOVE : SY-REPID TO W_PROGRAM.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = W_PROGRAM
      IS_LAYOUT          = W_LAYOUT
      IT_FIELDCAT        = W_FIELDCAT[]
      IT_EVENTS          = W_EVENTCAT[]
      IT_SORT            = W_SORTCAT[]
      I_SAVE             = 'A'
    TABLES
      T_OUTTAB           = IT_ITAB
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  get_scheduling_agreement
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SCHEDULING_AGREEMENT.
*---
  CLEAR: IT_SA_ALL, IT_SA_ALL[].
  SELECT B~EBELN
         EBELP
         MATNR
         WERKS
         LGORT
         LIFNR
         ETFZ1
         A~BSTYP
         A~BUKRS
         BSART
         EKORG
         EKGRP
         KDATB
         KDATE
               INTO CORRESPONDING FIELDS OF TABLE IT_SA_ALL
               FROM EKKO AS A INNER JOIN EKPO AS B
                 ON A~MANDT EQ B~MANDT
                AND A~EBELN EQ B~EBELN
               WHERE A~BSTYP EQ 'L'
                AND A~LIFNR IN S_LIFNR
                AND B~MATNR IN S_MATNR
                AND A~LOEKZ EQ SPACE
                AND B~LOEKZ EQ SPACE
                AND ELIKZ EQ SPACE.
*                AND ( kdatb LE sy-datum
*                  AND kdate GE sy-datum.
ENDFORM.                    " get_scheduling_agreement

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
    W_COL_POS 'EBELN' 10 'SA Number'      'CHAR' '' ''      '',
*    w_col_pos 'EBELP' 05 'Item Number'    'NUMC' '' ''      '',
    W_COL_POS 'MATNR' 18 'Material'       'CHAR' ''  ''      '',
    W_COL_POS 'WERKS' 04 'Plant'          'CHAR' ''  ''      '',
    W_COL_POS 'LIFNR' 04 'Vendor'         'CHAR' ''  ''      '',
    W_COL_POS 'KSCHL' 04 'Cond Record'    'CHAR' ''  ''      '',

    W_COL_POS 'DATAB_SA' 10 'SA From'     'DATS' ''  ''      '',
    W_COL_POS 'DATBI_SA' 10 'SA To'       'DATS' ''  ''      '',
    W_COL_POS 'KBETR_SA' 12 'Amount'      'CURR' ''  ''      '',
    W_COL_POS 'KPEIN_SA' 4  'Un/p'        'DEC' ''  ''      '',

    W_COL_POS 'DATAB' 10 'Info From'      'DATS' ''  ''      '',
    W_COL_POS 'DATBI' 10 'Info To'        'DATS' ''  ''      '',
    W_COL_POS 'KBETR' 12 'Amount'         'CURR' ''  ''      '',
    W_COL_POS 'KPEIN' 4  'Un/p'           'DEC' ''  ''      '',
    W_COL_POS 'UPD_SA' 2  'SA'            'CHAR' ''  ''      ''.

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
*  append_sortcat : '1' 'EBELN' 'IT_ITAB' 'X' '',
*                   '2' 'EBELP' 'IT_ITAB' 'X' '',
*                   '3' 'MATNR' 'IT_ITAB' 'X' '',
*                   '4' 'WERKS' 'IT_ITAB' 'X' '',
*                   '5' 'LGORT' 'IT_ITAB' 'X' ''.
ENDFORM.                    " build_sortcat

*---------------------------------------------------------------------*
*       FORM fill_table                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM FILL_TABLE.
  DATA: L_DIFF(1).

  READ TABLE IT_SACOND_ITEM WITH KEY KSCHL = 'PB00'.
  IF SY-SUBRC EQ 0.
    READ TABLE IT_INFO_ITEM WITH KEY KSCHL = 'PB00'.
    IF IT_SACOND_ITEM-KBETR = IT_INFO_ITEM-KBETR AND
       IT_SACOND_ITEM-KPEIN = IT_INFO_ITEM-KPEIN AND
       IT_SACOND_ITEM-DATBI = IT_INFO_ITEM-DATBI AND
       IT_SACOND_ITEM-DATAB = IT_INFO_ITEM-DATAB.
      DELETE IT_SACOND_ITEM WHERE KSCHL = 'PB00'.
    ELSE.
      L_DIFF = 'X'.
      MOVE : IT_SA_ALL-EBELN        TO IT_ITAB-EBELN,
             IT_SA_ALL-EBELP        TO IT_ITAB-EBELP,
             IT_SA_ALL-LIFNR        TO IT_ITAB-LIFNR,
             IT_SA_ALL-MATNR        TO IT_ITAB-MATNR,
             IT_SA_ALL-WERKS        TO IT_ITAB-WERKS,
             IT_SA_ALL-LGORT        TO IT_ITAB-LGORT,
             'PB00'                 TO IT_ITAB-KSCHL,
             IT_SACOND_ITEM-DATAB TO IT_ITAB-DATAB_SA,
             IT_SACOND_ITEM-DATBI TO IT_ITAB-DATBI_SA,
             IT_SACOND_ITEM-KBETR TO IT_ITAB-KBETR_SA,
             IT_SACOND_ITEM-KPEIN TO IT_ITAB-KPEIN_SA.
      DELETE IT_SACOND_ITEM WHERE KSCHL = 'PB00'.

      MOVE:  IT_INFO_ITEM-DATAB TO IT_ITAB-DATAB,
             IT_INFO_ITEM-DATBI TO IT_ITAB-DATBI,
             IT_INFO_ITEM-KBETR TO IT_ITAB-KBETR,
             IT_INFO_ITEM-KPEIN TO IT_ITAB-KPEIN.
      DELETE IT_INFO_ITEM WHERE KSCHL = 'PB00'.
      IT_ITAB-UPD_SA = W_UPD.
      APPEND IT_ITAB.
      CLEAR : IT_ITAB.
    ENDIF.
    LOOP AT IT_SACOND_ITEM.
      READ TABLE IT_INFO_ITEM WITH KEY KSCHL = IT_SACOND_ITEM-KSCHL.
      IF IT_SACOND_ITEM-KBETR = IT_INFO_ITEM-KBETR AND
       IT_SACOND_ITEM-KPEIN = IT_INFO_ITEM-KPEIN AND
       IT_SACOND_ITEM-DATBI = IT_INFO_ITEM-DATBI AND
       IT_SACOND_ITEM-DATAB = IT_INFO_ITEM-DATAB.
        DELETE IT_INFO_ITEM WHERE KSCHL = IT_SACOND_ITEM-KSCHL.
      ELSE.
        IF L_DIFF IS INITIAL.
          L_DIFF = 'X'.
          MOVE : IT_SA_ALL-EBELN        TO IT_ITAB-EBELN,
            IT_SA_ALL-EBELP        TO IT_ITAB-EBELP,
            IT_SA_ALL-LIFNR        TO IT_ITAB-LIFNR,
            IT_SA_ALL-MATNR        TO IT_ITAB-MATNR,
            IT_SA_ALL-WERKS        TO IT_ITAB-WERKS,
            IT_SA_ALL-LGORT        TO IT_ITAB-LGORT.
        ENDIF.
        MOVE: IT_SACOND_ITEM-KSCHL TO IT_ITAB-KSCHL,
              IT_SACOND_ITEM-DATAB TO IT_ITAB-DATAB_SA,
            IT_SACOND_ITEM-DATBI TO IT_ITAB-DATBI_SA,
            IT_SACOND_ITEM-KBETR TO IT_ITAB-KBETR_SA,
            IT_SACOND_ITEM-KPEIN TO IT_ITAB-KPEIN_SA.

        MOVE: IT_INFO_ITEM-DATAB TO IT_ITAB-DATAB,
              IT_INFO_ITEM-DATBI TO IT_ITAB-DATBI,
              IT_INFO_ITEM-KBETR TO IT_ITAB-KBETR,
              IT_INFO_ITEM-KPEIN TO IT_ITAB-KPEIN.
        DELETE IT_INFO_ITEM WHERE KSCHL = IT_SACOND_ITEM-KSCHL.
        APPEND IT_ITAB.
        CLEAR : IT_ITAB.
      ENDIF.
    ENDLOOP.
  ENDIF.
  LOOP AT IT_INFO_ITEM.
    IF L_DIFF IS INITIAL.
      L_DIFF = 'X'.
      MOVE : IT_SA_ALL-EBELN        TO IT_ITAB-EBELN,
        IT_SA_ALL-EBELP        TO IT_ITAB-EBELP,
        IT_SA_ALL-MATNR        TO IT_ITAB-MATNR,
        IT_SA_ALL-WERKS        TO IT_ITAB-WERKS,
        IT_SA_ALL-LGORT        TO IT_ITAB-LGORT.
    ENDIF.

    MOVE: IT_INFO_ITEM-KSCHL  TO IT_ITAB-KSCHL,
          IT_INFO_ITEM-DATAB TO IT_ITAB-DATAB,
          IT_INFO_ITEM-DATBI TO IT_ITAB-DATBI,
          IT_INFO_ITEM-KBETR TO IT_ITAB-KBETR,
          IT_INFO_ITEM-KPEIN TO IT_ITAB-KPEIN.
    APPEND IT_ITAB.
    CLEAR : IT_ITAB.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_info_item_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  GET_INFO_ITEM_CONDITION.

  DATA: L_DATAB LIKE A018-DATAB,
        L_DATBI LIKE A018-DATBI.

  CLEAR : IT_INFO_ITEM, IT_INFO_ITEM[].
  CLEAR : W_KNUMH.

  SELECT SINGLE KNUMH DATAB DATBI INTO (W_KNUMH, L_DATAB, L_DATBI)
                      FROM A018
                     WHERE KAPPL EQ 'M'
                       AND KSCHL EQ 'PB00'
                       AND LIFNR EQ IT_SA_ALL-LIFNR
                       AND MATNR EQ IT_SA_ALL-MATNR
                       AND DATAB <= P_DATE
                       AND DATBI >= P_DATE.

  W_INFO_DATAB = L_DATAB.
  W_INFO_DATBI = L_DATBI.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_INFO_ITEM
           FROM KONP
          WHERE KNUMH EQ W_KNUMH
            AND LOEVM_KO EQ SPACE
            AND KBETR > 0.

  LOOP AT IT_INFO_ITEM.
    READ TABLE IT_T683S WITH KEY
                      KSCHL = IT_INFO_ITEM-KSCHL
                      KALSM = 'RM0002'.
    IF IT_T683S-KSTAT = ' ' OR
        ( IT_T683S-KSTAT = 'X' AND IT_T683S-KVSL1 <> ' ' ).
      IF P_PR = 'X'.
      ELSE.
        IT_INFO_ITEM-DATAB = L_DATAB.
        IT_INFO_ITEM-DATBI = L_DATBI.
        MODIFY IT_INFO_ITEM.
      ENDIF.
      CONTINUE.
    ELSE.
      DELETE IT_INFO_ITEM.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " get_info_item_condition

*&---------------------------------------------------------------------*
*&      Form  get_sa_item_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SA_ITEM_CONDITION.

  DATA: L_DATAB LIKE A018-DATAB,
        L_DATBI LIKE A018-DATBI.

  CLEAR : IT_SACOND_ITEM, IT_SACOND_ITEM[], W_KNUMH.

  SELECT SINGLE KNUMH DATAB DATBI INTO (W_KNUMH, L_DATAB, L_DATBI)
                      FROM A016
                     WHERE KAPPL EQ 'M'
                       AND KSCHL EQ 'PB00'
                       AND EVRTN EQ IT_SA_ALL-EBELN
                       AND EVRTP EQ IT_SA_ALL-EBELP
                       AND DATAB <= P_DATE
                       AND DATBI >= P_DATE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_SACOND_ITEM
           FROM KONP
          WHERE KNUMH EQ W_KNUMH
            AND KBETR > 0.

  LOOP AT IT_SACOND_ITEM.
    READ TABLE IT_T683S WITH KEY
                KSCHL = IT_SACOND_ITEM-KSCHL
                KALSM = 'RM0000'.
    IF IT_T683S-KSTAT = ' ' OR
        ( IT_T683S-KSTAT = 'X' AND IT_T683S-KVSL1 <> ' ' ).
      IF P_PR = 'X'.
      ELSE.
*        IF P_HIS IS INITIAL.
*          IT_SACOND_ITEM-DATAB = W_INFO_DATAB.
*          IT_SACOND_ITEM-DATBI  = W_INFO_DATBI.
*        ELSE.
        IT_SACOND_ITEM-DATAB = L_DATAB.
        IT_SACOND_ITEM-DATBI = L_DATBI.
*        ENDIF.
        MODIFY IT_SACOND_ITEM.
      ENDIF.
      CONTINUE.
    ELSE.
      DELETE IT_SACOND_ITEM.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_sa_item_condition

*&---------------------------------------------------------------------*
*&      Form  send_email
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_EMAIL.

  DATA: L_SUBJECT(40) TYPE C VALUE 'SA Info Record Price Compare'.

  DATA:   IT_PACKING_LIST LIKE SOPCKLSTI1 OCCURS 0 WITH HEADER LINE,
          IT_CONTENTS LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
          IT_RECEIVERS LIKE SOMLRECI1 OCCURS 0 WITH HEADER LINE,
          IT_ATTACHMENT LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
          GD_CNT TYPE I,
          GD_SENT_ALL(1) TYPE C,
          GD_DOC_DATA LIKE SODOCCHGI1,
          GD_ERROR TYPE SY-SUBRC.

  PERFORM POPULATE_DATA_FOR_OUTPUT.

  GD_DOC_DATA-DOC_SIZE = 1.

* Populate the subject/generic message attributes
  GD_DOC_DATA-OBJ_LANGU = SY-LANGU.
  GD_DOC_DATA-OBJ_NAME  = SY-REPID.
  GD_DOC_DATA-OBJ_DESCR = L_SUBJECT.
  GD_DOC_DATA-SENSITIVTY = 'F'.

* Describe the body of the message
  CLEAR IT_PACKING_LIST.
  REFRESH IT_PACKING_LIST.
  IT_PACKING_LIST-TRANSF_BIN = SPACE.
  IT_PACKING_LIST-HEAD_START = 1.
  IT_PACKING_LIST-HEAD_NUM = 0.
  IT_PACKING_LIST-BODY_START = 1.
  DESCRIBE TABLE IT_MAIL LINES IT_PACKING_LIST-BODY_NUM.
  IT_PACKING_LIST-DOC_TYPE = 'RAW'.
  APPEND IT_PACKING_LIST.

* Add the recipients email address
  CLEAR IT_RECEIVERS.
  REFRESH IT_RECEIVERS.
  IT_RECEIVERS-RECEIVER = P_EMAIL.
*  it_receivers-rec_type = 'U'.  " internet email
  IT_RECEIVERS-REC_TYPE = 'C'.
  IT_RECEIVERS-COM_TYPE = 'INT'.
  IT_RECEIVERS-NOTIF_DEL = 'X'.
  IT_RECEIVERS-NOTIF_NDEL = 'X'.
  APPEND IT_RECEIVERS.

* Call the FM to post the message to SAPMAIL
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      DOCUMENT_DATA              = GD_DOC_DATA
*            PUT_IN_OUTBOX              = 'X'
            COMMIT_WORK                = 'X'
    IMPORTING
      SENT_TO_ALL                = GD_SENT_ALL
    TABLES
      PACKING_LIST               = IT_PACKING_LIST
      CONTENTS_TXT               = IT_MAIL
      RECEIVERS                  = IT_RECEIVERS
    EXCEPTIONS
      TOO_MANY_RECEIVERS         = 1
      DOCUMENT_NOT_SENT          = 2
      DOCUMENT_TYPE_NOT_EXIST    = 3
      OPERATION_NO_AUTHORIZATION = 4
      PARAMETER_ERROR            = 5
      X_ERROR                    = 6
      ENQUEUE_ERROR              = 7
      OTHERS                     = 8.

* Store function module return code
  IF sy-subrc = 0.
    SUBMIT rsconn01           "Start the sending process
          WITH mode   = 'INT'
          WITH output = ' '
          AND RETURN.

  ENDIF.

  GD_ERROR = SY-SUBRC.

ENDFORM.                    " send_email

*---------------------------------------------------------------------*
*       FORM populate_data_for_output                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM POPULATE_DATA_FOR_OUTPUT.
  DATA: L_MESSAGE TYPE SO_TEXT255,
        L_KPEIN(4),
        L_KBETR(12).

  CLEAR: IT_MAIL,IT_MAIL[].
**S> 08/04/11 Paul
*  APPEND 'SA Number Material        Plant Vend Cond SA Fr     SA To
*AMOUNT U/P  INFO FR  INFO TO    AMOUNT U/P' TO it_mail.
  APPEND text-m03 to it_mail.
**E<
  LOOP AT IT_ITAB.
    L_KBETR = IT_ITAB-KBETR_SA.
    L_KPEIN = IT_ITAB-KPEIN_SA.
    CONCATENATE IT_ITAB-EBELN IT_ITAB-MATNR IT_ITAB-WERKS
                IT_ITAB-LIFNR INTO L_MESSAGE SEPARATED BY SPACE.
    CONCATENATE L_MESSAGE IT_ITAB-KSCHL IT_ITAB-DATAB_SA
                IT_ITAB-DATBI_SA L_KBETR
                L_KPEIN INTO L_MESSAGE SEPARATED BY SPACE.
    L_KBETR = IT_ITAB-KBETR.
    L_KPEIN = IT_ITAB-KPEIN.
    CONCATENATE L_MESSAGE IT_ITAB-DATAB IT_ITAB-DATBI L_KBETR
                L_KPEIN INTO L_MESSAGE SEPARATED BY SPACE.

    APPEND L_MESSAGE TO IT_MAIL.
    CLEAR: IT_MAIL, L_MESSAGE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_cond_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_COND_PRICE.
  CLEAR: IT_T683S, IT_T683S[].
  SELECT * INTO TABLE IT_T683S FROM T683S
  WHERE KVEWE = 'A'
    AND KAPPL = 'M'
    AND ( KALSM = 'RM0000' OR KALSM = 'RM0002' ).
ENDFORM.   " get_cond_price

*** Change SA price update
FORM CHANGE_SA_CONDITION.
*---
  SORT IT_INFO_ITEM BY KSCHL DESCENDING.

  DATA : L_FIELD01(20),
         L_DATAB(10),
         L_DATBI(10),
         L_KBETR(13),
         L_KPEIN(5),
         L_KZUST LIKE KONH-KZUST,
         L_KBETR_TEMP LIKE KONP-KBETR.

*  DATA:  it_ztmm_if_price LIKE TABLE OF ztmm_if_price WITH HEADER LINE.
*
  CLEAR : IT_BDC, IT_BDC[], IT_MESS, IT_MESS[], L_FIELD01,
          L_DATAB, L_DATBI, L_KBETR.

  READ TABLE IT_INFO_ITEM WITH KEY KSCHL = 'PB00'.
*  IF IT_INFO_ITEM-DATAB = P_DATE.

  PERFORM DYNPRO USING : 'X'  'SAPMM06E'        '0205',
                         ' '  'RM06E-EVRTN'     IT_SA_ALL-EBELN,
                         ' '  'BDC_OKCODE'      '=AB'.

 CONCATENATE 'RM06E-TCSELFLAG(' IT_SA_ALL-EBELP+3(2) ')' INTO L_FIELD01
               .

  PERFORM DYNPRO USING : 'X'  'SAPMM06E'        '0220',
                         ' '  L_FIELD01         'X',
                         ' '  'BDC_OKCODE'      '=KO'.

  PERFORM DYNPRO USING : 'X'  'SAPLV14A'        '0102',
                         ' '  'BDC_OKCODE'      '=NEWD'.

  DATA : L_99991231 TYPE D VALUE '99991231'.

*    WRITE : IT_INFO_ITEM-DATAB TO L_DATAB,
  WRITE : P_DATE TO L_DATAB,
          IT_INFO_ITEM-DATBI TO L_DATBI.

  PERFORM DYNPRO USING : 'X'  'SAPMV13A'        '0201',
                         ' '  'RV13A-DATAB'     L_DATAB,
                         ' '  'RV13A-DATBI'     L_DATBI.
  IF SY-SUBRC EQ 0.
    CLEAR : L_KBETR, L_KPEIN.
    WRITE : IT_INFO_ITEM-KBETR TO L_KBETR
                               CURRENCY IT_INFO_ITEM-KONWA.
    WRITE : IT_INFO_ITEM-KPEIN TO L_KPEIN.
    PERFORM DYNPRO USING : ' '  'KONP-KBETR(01)'  L_KBETR,
                           ' '  'BDC_CURSOR'      'KONP-KPEIN(01)',
                           ' '  'KONP-KPEIN(01)'  L_KPEIN.
*    MOVE : it_sa_all-ebeln        TO it_itab-ebeln,
*           it_sa_all-ebelp        TO it_itab-ebelp,
*           it_sa_all-matnr        TO it_itab-matnr,
*           it_sa_all-werks        TO it_itab-werks,
*           it_sa_all-lgort        TO it_itab-lgort,
*           it_a018-datab TO it_itab-datab,
*           it_a018-datbi TO it_itab-datbi,
*           it_info_item-kschl TO it_itab-kschl,
*           it_info_item-kbetr TO it_itab-kbetr,
*           it_info_item-kpein TO it_itab-kpein,
*           it_info_item-konwa TO it_itab-konwa.
*    APPEND it_itab.
*    CLEAR : it_itab.
    DELETE IT_INFO_ITEM WHERE KSCHL EQ 'PB00'.
  ENDIF.

*---
  LOOP AT IT_INFO_ITEM.
    CLEAR : L_KBETR_TEMP.
    IF IT_INFO_ITEM-KONWA EQ '%'.
      L_KBETR_TEMP = IT_INFO_ITEM-KBETR / 10.
      MOVE : L_KBETR_TEMP TO L_KBETR.
    ELSE.
      WRITE : IT_INFO_ITEM-KBETR TO L_KBETR
                                 CURRENCY IT_INFO_ITEM-KONWA.
      WRITE : IT_INFO_ITEM-KPEIN TO L_KPEIN.
    ENDIF.
    IF SY-TABIX EQ 1.
      PERFORM DYNPRO USING : ' '  'KONP-KSCHL(02)'  IT_INFO_ITEM-KSCHL,
                                 ' '  'KONP-KBETR(02)'  L_KBETR,
                               ' '  'BDC_CURSOR'      'KONP-KPEIN(02)',
                                 ' '  'KONP-KPEIN(02)'  L_KPEIN,
                                 ' '  'BDC_OKCODE'      '=PDAT'.
    ELSE.
      PERFORM DYNPRO USING : 'X'  'SAPMV13A'        '0201',
                            ' '  'KONP-KSCHL(02)'  IT_INFO_ITEM-KSCHL,
                             ' '  'KONP-KBETR(02)'  L_KBETR,
                             ' '  'BDC_CURSOR'      'KONP-KSCHL(02)',
                             ' '  'BDC_OKCODE'      '=PDAT'.
    ENDIF.
    IF IT_INFO_ITEM-LIFNR <> ' '.
      PERFORM DYNPRO USING : 'X'  'SAPMV13A'        '0300',
                             ' '  'BDC_OKCODE'      '/00',
                            ' '  'KONP-LIFNR'      IT_INFO_ITEM-LIFNR,
                             ' '  'BDC_CURSOR'      'KONP-KBETR',
                              ' '  'BDC_OKCODE'      '=BACK'.
    ELSE.
      PERFORM DYNPRO USING : 'X'  'SAPMV13A'        '0300',
                              ' '  'BDC_OKCODE'      '=BACK'.

    ENDIF.
    PERFORM DYNPRO USING : 'X'  'SAPMV13A'        '0201',
                           ' '  'BDC_CURSOR'      'KONP-KSCHL(02)',
                           ' '  'BDC_OKCODE'      '=EINF'.

*    MOVE : it_sa-ebeln        TO it_itab-ebeln,
*           it_sa-ebelp        TO it_itab-ebelp,
*           it_sa-matnr        TO it_itab-matnr,
*           it_sa-werks        TO it_itab-werks,
*           it_sa-lgort        TO it_itab-lgort,
*           it_a018-datab TO it_itab-datab,
*           it_a018-datbi TO it_itab-datbi,
*           it_info_item-kschl TO it_itab-kschl,
*           it_info_item-kbetr TO it_itab-kbetr,
*           it_info_item-konwa TO it_itab-konwa.
*    IF it_itab-konwa EQ '%'.
*      it_itab-kbetr = it_itab-kbetr / 10.
*    ENDIF.
*    APPEND it_itab.
*    CLEAR : it_itab.
  ENDLOOP.

  PERFORM DYNPRO USING : 'X'  'SAPMV13A'        '0201',
                         ' '  'BDC_OKCODE'      '=KDAT'.

  CLEAR : L_KZUST.
  MOVE : IT_INFO_ITEM-KZUST TO L_KZUST.

  PERFORM DYNPRO USING : 'X'  'SAPMV13A'        '0200',
                         ' '  'KONH-KZUST'      L_KZUST,
                         ' '  'BDC_OKCODE'      '=BACK'.

  PERFORM DYNPRO USING : 'X'  'SAPMM06E'        '0220',
                         ' '  'BDC_OKCODE'      '=BU'.

  PERFORM DYNPRO USING : 'X'  'SAPLSPO1'        '0300',
                         ' '  'BDC_OKCODE'      '=YES'.

*---
  CALL TRANSACTION 'ME32L' USING IT_BDC
                           MODE W_MODE
                           UPDATE 'S'
                           MESSAGES INTO IT_MESS.

  APPEND LINES OF IT_MESS TO IT_MESSAGE.

  DATA : L_MESSA(80).

  CLEAR : IT_MESS, L_MESSA.

  READ TABLE IT_MESS WITH KEY MSGTYP = 'E'.

  IF SY-SUBRC EQ 0.
    MOVE : C_RED             TO IT_ITAB-LINECOLOR.
    PERFORM GET_MESSAGE USING    IT_MESS-MSGID
                                 IT_MESS-MSGNR
                                 IT_MESS-MSGV1
                                 IT_MESS-MSGV2
                                 IT_MESS-MSGV3
                                 IT_MESS-MSGV4
                        CHANGING L_MESSA.
** update ztmm_if_price table
    CLEAR: IT_ZTMM_IFSA_LOG.
    IT_ZTMM_IFSA_LOG-LIFNR = IT_SA_ALL-LIFNR.
    IT_ZTMM_IFSA_LOG-MATNR = IT_SA_ALL-MATNR.
    IT_ZTMM_IFSA_LOG-EKORG = IT_SA_ALL-EKORG.
    IT_ZTMM_IFSA_LOG-DATBI = L_DATBI.
    IT_ZTMM_IFSA_LOG-DATAB = L_DATAB.
* MOVE-CORRESPONDING it_a018 TO it_ztmm_ifsa_log.
    IT_ZTMM_IFSA_LOG-ZUSER = SY-UNAME.
    IT_ZTMM_IFSA_LOG-ZRESULT = 'E'.
    IT_ZTMM_IFSA_LOG-ZBDAT = SY-DATUM.
    IT_ZTMM_IFSA_LOG-ZTIME = SY-UZEIT.
    IT_ZTMM_IFSA_LOG-ZMSG = L_MESSA.
    APPEND IT_ZTMM_IFSA_LOG.

  ENDIF.

  READ TABLE IT_MESS WITH KEY MSGTYP = 'S'.

  IF SY-SUBRC EQ 0.
    W_UPD = 'X'.
  ELSE.
    CLEAR: W_UPD.
  ENDIF.
*  ELSE.
*    CLEAR: W_UPD.
*  ENDIF.
ENDFORM.                    " change_sa_condition



*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0955   text
*      -->P_0956   text
*      -->P_0957   text
*----------------------------------------------------------------------*
FORM DYNPRO USING    DYNBEGIN
                     NAME
                     VALUE.
*---
  IF DYNBEGIN = 'X'.
    CLEAR : IT_BDC.
    MOVE : NAME  TO IT_BDC-PROGRAM,
           VALUE TO IT_BDC-DYNPRO,
           'X'   TO IT_BDC-DYNBEGIN.
    APPEND IT_BDC.
  ELSE .
    CLEAR : IT_BDC.
    MOVE : NAME  TO IT_BDC-FNAM,
           VALUE TO IT_BDC-FVAL.
    APPEND IT_BDC.
  ENDIF.
ENDFORM.                    " dynpro

*&---------------------------------------------------------------------*
*&      Form  get_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MESS_MSGID  text
*      -->P_IT_MESS_MSGNR  text
*      -->P_IT_MESS_MSGV1  text
*      -->P_IT_MESS_MSGV2  text
*      -->P_IT_MESS_MSGV3  text
*      -->P_IT_MESS_MSGV4  text
*      <--P_L_MESSA  text
*----------------------------------------------------------------------*
FORM GET_MESSAGE USING    P_MSGID
                          P_MSGNR
                          P_MSGV1
                          P_MSGV2
                          P_MSGV3
                          P_MSGV4
                 CHANGING P_L_MESSA.
*---
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      MSGID               = P_MSGID
      MSGNR               = P_MSGNR
      MSGV1               = P_MSGV1
      MSGV2               = P_MSGV2
      MSGV3               = P_MSGV3
      MSGV4               = P_MSGV4
    IMPORTING
      MESSAGE_TEXT_OUTPUT = P_L_MESSA.
ENDFORM.                    " get_message
*&---------------------------------------------------------------------*
*&      Form  set_parameters
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_PARAMETERS.
  P_DATE = SY-DATUM + 1.
ENDFORM.                    " set_parameters
