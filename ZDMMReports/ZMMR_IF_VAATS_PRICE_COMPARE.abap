************************************************************************
* Program Name      : ZMMR_IF_VAATS_PRICE_COMPARE
* Author            : Furong Wang
* Creation Date     : 02/2008
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Price comparison (Vaats & Info Record)
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************


REPORT ZEMMPM29E_CD_CHANGE_S NO STANDARD PAGE HEADING
                             LINE-SIZE 132
                             LINE-COUNT 64(1)
                             MESSAGE-ID ZMMM.
**---
INCLUDE : ZRMMPMXXR_INCL.

**--- Internal Tables
DATA : BEGIN OF IT_TEMP OCCURS 0,
         LIFNR LIKE EKKO-LIFNR,
         MATNR LIKE EKPO-MATNR,
         INTF_D LIKE SY-DATUM,
         KSCHL LIKE KONP-KSCHL,
         KBETR_SA LIKE KONP-KBETR,
         DATAB_SA LIKE A018-DATAB,
         DATBI_SA LIKE A018-DATBI,
         DATAB LIKE A018-DATAB,
         DATBI LIKE A018-DATBI,
         KBETR LIKE KONP-KBETR,
         KPEIN LIKE KONP-KPEIN,
         REMARKS(160),
         DELETED(1),
         LINECOLOR(4),     " ALV Color
       END OF IT_TEMP.

DATA : IT_ITAB LIKE IT_TEMP OCCURS 0 WITH HEADER LINE.

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

DATA: IT_VAATS LIKE TABLE OF ZTMM_IF_PRICE WITH HEADER LINE.


DATA : BEGIN OF IT_VAATS_ERROR OCCURS 0,
       MATNR LIKE MARA-MATNR,
       LIFNR LIKE LFA1-LIFNR,
       ERR_MATNR(15),
       ERR_LIFNR(15),
       ERR_RESN(15),
       ERR_EKGRP(15),
       REMARKS(126),
       END OF IT_VAATS_ERROR .

DATA: IT_MAIL TYPE STANDARD TABLE OF SOLISTI1 INITIAL SIZE 0
                  WITH HEADER LINE.

DATA: IT_T683S LIKE TABLE OF T683S WITH HEADER LINE.

** wokking variant
DATA : W_SUBRC LIKE SY-SUBRC.

DATA : W_KNUMH LIKE KONH-KNUMH.
DATA: W_VAATS_ERROR(1).
CONSTANTS: P_PR VALUE IS INITIAL.

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
SELECT-OPTIONS : S_EKORG FOR EKKO-EKORG DEFAULT 'PU01'.
SELECT-OPTIONS : S_LIFNR FOR EKKO-LIFNR.
SELECT-OPTIONS : S_MATNR FOR EKPO-MATNR.
PARAMETERS : P_DATE LIKE EKKO-AEDAT DEFAULT SY-DATUM OBLIGATORY.
*SELECTION-SCREEN SKIP.
*PARAMETERS: P_PR RADIOBUTTON GROUP GRP.
*PARAMETERS: P_PRDT RADIOBUTTON GROUP GRP DEFAULT 'X'.
*SELECTION-SCREEN SKIP.
*PARAMETERS : p_bdc LIKE ekko-aedat DEFAULT sy-datum OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-004.
PARAMETERS: P_SEND AS CHECKBOX DEFAULT 'X' USER-COMMAND UCOM.
PARAMETERS: P_EMAIL(40) DEFAULT 'SAIFVAL' MODIF ID MD3.
SELECTION-SCREEN END OF BLOCK BLOCK2.

**---
INITIALIZATION.
  PERFORM EVENT_BUILD USING W_EVENTCAT[].

**---
TOP-OF-PAGE.
  PERFORM TOP_OF_PAGE.

**---
START-OF-SELECTION.
  PERFORM GET_VAATS_DATA.

  IF IT_VAATS[] IS INITIAL.
    MESSAGE S999 WITH TEXT-M01.
  ELSE.
    IF NOT IT_VAATS_ERROR[] IS INITIAL.
      W_VAATS_ERROR = 'X'.
      PERFORM SEND_EMAIL.
    ELSE.
      CLEAR W_VAATS_ERROR.
    ENDIF.
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
  LOOP AT IT_VAATS.
    PERFORM GET_INFO_ITEM_CONDITION.
    SORT IT_INFO_ITEM BY KSCHL DESCENDING.
    PERFORM FILL_TABLE.
  ENDLOOP.

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
FORM GET_VAATS_DATA.
  DATA: LT_VAATS LIKE TABLE OF ZTMM_IF_PRICE WITH HEADER LINE,
        L_FLAG(1),
        L_FIELD(20).

  CLEAR: IT_VAATS[], W_VAATS_ERROR.
  SELECT * INTO TABLE IT_VAATS
    FROM ZTMM_IF_PRICE
*    where not zresult EQ 'S'.
    WHERE MATNR IN S_MATNR
      AND LIFNR IN S_LIFNR
            AND INTF_D >= P_DATE
*      and APP_D >= P_DATE
    .
  SORT IT_VAATS BY MATNR LIFNR INTF_D ZSEQ DESCENDING APP_D.
  DELETE ADJACENT DUPLICATES FROM IT_VAATS COMPARING MATNR LIFNR INTF_D.

* CHECK vaatz data itself
*  LOOP AT IT_VAATS.
*    CLEAR: L_FLAG, IT_VAATS_ERROR.
*
*    SELECT SINGLE KZUST INTO L_FIELD
*     FROM T686C
*    WHERE KZUST = IT_VAATS-RESN_C.
*    IF SY-SUBRC <> 0.
*      L_FLAG = 'X'.
*      IT_VAATS_ERROR-ERR_RESN = IT_VAATS-RESN_C.
*    ENDIF.
*
*    SELECT SINGLE EKGRP INTO L_FIELD
*      FROM T024
*    WHERE EKGRP = IT_VAATS-PURCH_G.
*    IF SY-SUBRC <> 0.
*      L_FLAG = 'X'.
*      IT_VAATS_ERROR-ERR_EKGRP = IT_VAATS-PURCH_G.
*    ENDIF.
*
*    SELECT SINGLE MATNR INTO L_FIELD
*       FROM MARA
*     WHERE MATNR = IT_VAATS-MATNR.
*    IF SY-SUBRC <> 0.
*      L_FLAG = 'X'.
*      IT_VAATS_ERROR-ERR_MATNR = IT_VAATS-MATNR.
*    ENDIF.
*
*    SELECT SINGLE LIFNR INTO L_FIELD
*       FROM LFA1
*     WHERE LIFNR = IT_VAATS-LIFNR.
*    IF SY-SUBRC <> 0.
*      L_FLAG = 'X'.
*      IT_VAATS_ERROR-ERR_LIFNR = IT_VAATS-LIFNR.
*    ENDIF.
*    IF L_FLAG = 'X'.
*      IT_VAATS_ERROR-MATNR = IT_VAATS-MATNR.
*      IT_VAATS_ERROR-LIFNR = IT_VAATS-LIFNR.
*      APPEND IT_VAATS_ERROR.
*    ENDIF.
*  ENDLOOP.

* 1. reason code should be t686C
* 2. purchasing group : T024
* 3. material master not exist in SAP ; MARA
* 4. vendor master not exist in SAP ; LFA1

ENDFORM.

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
    W_COL_POS 'MATNR' 18 'Material'       'CHAR' ''  ''      '',
    W_COL_POS 'LIFNR' 04 'Vendor'         'CHAR' ''  ''      '',
    W_COL_POS 'INTF_D' 12 'Interface Date'  'DATS' ''  ''      '',
    W_COL_POS 'DATAB_SA' 12 'Vesting Date'     'DATS' ''  ''      '',
    W_COL_POS 'KBETR_SA' 14 'Vaats Amount'      'CURR' ''  ''      '',
    W_COL_POS 'DATAB' 10 'Info From'      'DATS' ''  ''      '',
    W_COL_POS 'DATBI' 10 'Info To'        'DATS' ''  ''      '',
    W_COL_POS 'KBETR' 14 'Inf0 Amount'         'CURR' ''  ''      '',
    W_COL_POS 'REMARKS' 40 'Reasons'        'CHAR' ''  ''      '',
    W_COL_POS 'DELETED' 11 'Inf Dltd'        'CHAR' ''  ''      ''.
*    w_col_pos 'KPEIN' 4  'Un/p'           'DEC' ''  ''      ''.

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
  DATA: L_DIFF(1),
         L_LOEKZ(1).

  CLEAR: IT_ITAB.
  SELECT SINGLE LOEKZ INTO L_LOEKZ
                         FROM EINA
                        WHERE LIFNR EQ IT_VAATS-LIFNR
                          AND MATNR EQ IT_VAATS-MATNR.
  IT_ITAB-DELETED = L_LOEKZ.

  READ TABLE IT_INFO_ITEM WITH KEY KSCHL = 'PB00'.
  IF SY-SUBRC = 0.
    IF IT_VAATS-PRICE = IT_INFO_ITEM-KBETR
       AND IT_VAATS-APP_D = IT_INFO_ITEM-DATAB.
    ELSE.
      IF IT_VAATS-PRICE = IT_INFO_ITEM-KBETR.
        MOVE:  'Validation period does not match' TO IT_ITAB-REMARKS.
      ELSEIF IT_VAATS-APP_D = IT_INFO_ITEM-DATAB.
        MOVE:  'Price is not same' TO IT_ITAB-REMARKS.
      ELSE.
        MOVE:  'Price & validation Period' TO IT_ITAB-REMARKS.
      ENDIF.

      IF IT_VAATS-ERR_C = 'E'.
        IF  IT_VAATS-ZRESULT = 'S'.
        ELSE.
          MOVE: IT_VAATS-MSG_C TO IT_ITAB-REMARKS.
        ENDIF.
      ELSEIF IT_VAATS-ZRESULT <> 'S'.
        MOVE: IT_VAATS-ZMSG TO IT_ITAB-REMARKS.
      ENDIF.

      L_DIFF = 'X'.
      MOVE : IT_VAATS-LIFNR        TO IT_ITAB-LIFNR,
             IT_VAATS-MATNR        TO IT_ITAB-MATNR,
             'PB00'                 TO IT_ITAB-KSCHL,
              IT_VAATS-APP_D TO IT_ITAB-DATAB_SA,
             IT_VAATS-INTF_D TO IT_ITAB-INTF_D,
             IT_VAATS-PRICE TO IT_ITAB-KBETR_SA.

      MOVE:  IT_INFO_ITEM-DATAB TO IT_ITAB-DATAB,
             IT_INFO_ITEM-DATBI TO IT_ITAB-DATBI,
             IT_INFO_ITEM-KBETR TO IT_ITAB-KBETR.
*             it_info_item-kpein TO it_itab-kpein.
*      DELETE it_info_item WHERE kschl = 'PB00'.

      APPEND IT_ITAB.
      CLEAR : IT_ITAB.
    ENDIF.
  ELSE.
*    SELECT SINGLE MATNR INTO L_MATNR
*                       FROM EINA
*                      WHERE KAPPL EQ 'M'
*                        AND KSCHL EQ 'PB00'
*                        AND LIFNR EQ IT_VAATS-LIFNR
*                        AND MATNR EQ IT_VAATS-MATNR.
    SELECT SINGLE LOEKZ INTO L_LOEKZ
                           FROM EINA
                          WHERE LIFNR EQ IT_VAATS-LIFNR
                            AND MATNR EQ IT_VAATS-MATNR
                            AND LOEKZ = ' '.
    IF SY-SUBRC = 0.
      MOVE:  'Validation period does not match' TO IT_ITAB-REMARKS.
    ELSE.
      MOVE:  'No Info Record found' TO IT_ITAB-REMARKS.
    ENDIF.

    IF IT_VAATS-ERR_C = 'E'.
      IF  IT_VAATS-ZRESULT = 'S'.
      ELSE.
        MOVE: IT_VAATS-MSG_C TO IT_ITAB-REMARKS.
      ENDIF.
    ELSEIF IT_VAATS-ZRESULT <> 'S'.
      MOVE: IT_VAATS-ZMSG TO IT_ITAB-REMARKS.
    ENDIF.


    MOVE:  IT_VAATS-LIFNR        TO IT_ITAB-LIFNR,
           IT_VAATS-MATNR        TO IT_ITAB-MATNR,
           'PB00'                 TO IT_ITAB-KSCHL,
           IT_VAATS-APP_D TO IT_ITAB-DATAB_SA,
           IT_VAATS-INTF_D TO IT_ITAB-INTF_D,
           IT_VAATS-PRICE TO IT_ITAB-KBETR_SA.

    APPEND IT_ITAB.
    CLEAR : IT_ITAB.
  ENDIF.

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
                       AND LIFNR EQ IT_VAATS-LIFNR
                       AND MATNR EQ IT_VAATS-MATNR
                       AND DATAB = IT_VAATS-APP_D.
*                       AND DATAB <= IT_VAATS-APP_D
*                       AND DATBI >= IT_VAATS-APP_D.
  IF SY-SUBRC <> 0.
    SELECT SINGLE KNUMH DATAB DATBI INTO (W_KNUMH, L_DATAB, L_DATBI)
                        FROM A017
                       WHERE KAPPL EQ 'M'
                         AND KSCHL EQ 'PB00'
                         AND LIFNR EQ IT_VAATS-LIFNR
                         AND MATNR EQ IT_VAATS-MATNR
                         AND DATAB = IT_VAATS-APP_D.
  ENDIF.
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
*&      Form  send_email
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_EMAIL.

  DATA: L_SUBJECT(40) TYPE C
        VALUE 'Vaats Interface & Info Rec Price COMP'.

  DATA:   IT_PACKING_LIST LIKE SOPCKLSTI1 OCCURS 0 WITH HEADER LINE,
          IT_CONTENTS LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
          IT_RECEIVERS LIKE SOMLRECI1 OCCURS 0 WITH HEADER LINE,
          IT_ATTACHMENT LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
          GD_CNT TYPE I,
          GD_SENT_ALL(1) TYPE C,
          GD_DOC_DATA LIKE SODOCCHGI1,
          GD_ERROR TYPE SY-SUBRC.

  IF W_VAATS_ERROR = 'X'.
    L_SUBJECT = 'VAATS DATA ERROR'.
    PERFORM POPULATE_DATA_VAATS_FOR_OUTPUT.
    CLEAR: W_VAATS_ERROR.
  ELSE.
    L_SUBJECT = 'Vaats Interface & Info Rec Price Comparison'.
    PERFORM POPULATE_DATA_FOR_OUTPUT.
  ENDIF.
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

**  APPEND 'SA Number Material        Plant Vend Cond SA Fr     SA To
**AMOUNT U/P  INFO FR  INFO TO    AMOUNT U/P' TO it_mail.
*  APPEND 'Material     Vend Vaats Fr Vaats To     AMT U/P INFO Fr  INFO
*TO     AMT U/P' TO it_mail.
*
*  LOOP AT IT_ITAB.
*    L_KBETR = IT_ITAB-KBETR_SA.
**    l_kpein = it_itab-kpein_sa.
*    CONCATENATE IT_ITAB-MATNR IT_ITAB-LIFNR
*    INTO L_MESSAGE SEPARATED BY SPACE.
*    CONCATENATE L_MESSAGE IT_ITAB-DATAB_SA
*                IT_ITAB-DATBI_SA L_KBETR
*                INTO L_MESSAGE SEPARATED BY SPACE.
*    L_KBETR = IT_ITAB-KBETR.
*    CONCATENATE L_MESSAGE IT_ITAB-DATAB IT_ITAB-DATBI L_KBETR
*                INTO L_MESSAGE SEPARATED BY SPACE.
*
*    APPEND L_MESSAGE TO IT_MAIL.
*    CLEAR: IT_MAIL, L_MESSAGE.
*  ENDLOOP.

** New format


  MOVE: '-------------------' TO IT_MAIL+0(19),
        '-------' TO IT_MAIL+19(7),
        '-----------' TO IT_MAIL+26(11),
        '-----------' TO IT_MAIL+37(11),
        '--------------' TO IT_MAIL+48(14),
        '-----------' TO IT_MAIL+62(11),
        '-----------' TO IT_MAIL+73(11),
        '--------------'  TO IT_MAIL+84(14),
        '---------------' TO  IT_MAIL+98(15),
        '--' TO IT_MAIL+113(2),
        '-' TO IT_MAIL+115(1),
        '------' TO IT_MAIL+116(6),
               '--------------------' to IT_MAIL+122(20),
          '--------------------' to IT_MAIL+142(20),
          '--------------------' to IT_MAIL+162(20),
          '--------------------' to IT_MAIL+182(20),
          '--------------------' to IT_MAIL+202(20).

  APPEND IT_MAIL.
  CLEAR: IT_MAIL.

  MOVE: '|          Material' TO IT_MAIL+0(19),
        '|Vendor' TO IT_MAIL+19(7),
        '| Inf. Date' TO IT_MAIL+26(11),
        '|Vest. Date' TO IT_MAIL+37(11),
        '|    Vaats Amt' TO IT_MAIL+48(14),
        '| Info From' TO IT_MAIL+62(11),
        '|   Info To' TO IT_MAIL+73(11),
        '|    Info Amnt'  TO IT_MAIL+84(14),
*        '|             Reasons' TO  IT_MAIL+98(21),
*        '|D' TO IT_MAIL+119(2),
*        '|' TO IT_MAIL+121(1).
       '|             Reasons' TO  IT_MAIL+98(121),
        '|D' TO IT_MAIL+219(2),
        '|' TO IT_MAIL+221(1).

  APPEND IT_MAIL.
  CLEAR: IT_MAIL.
  MOVE: '-------------------' TO IT_MAIL+0(19),
        '-------' TO IT_MAIL+19(7),
        '-----------' TO IT_MAIL+26(11),
        '-----------' TO IT_MAIL+37(11),
        '--------------' TO IT_MAIL+48(14),
        '-----------' TO IT_MAIL+62(11),
        '-----------' TO IT_MAIL+73(11),
        '--------------'  TO IT_MAIL+84(14),
        '---------------' TO  IT_MAIL+98(15),
        '--' TO IT_MAIL+113(2),
        '-' TO IT_MAIL+115(1),
        '------' TO IT_MAIL+116(6),
                  '--------------------' to IT_MAIL+122(20),
          '--------------------' to IT_MAIL+142(20),
          '--------------------' to IT_MAIL+162(20),
          '--------------------' to IT_MAIL+182(20),
          '--------------------' to IT_MAIL+202(20).

  APPEND IT_MAIL.
  CLEAR: IT_MAIL.

  LOOP AT IT_ITAB.
    L_KBETR = IT_ITAB-KBETR_SA.

    MOVE: '|' TO IT_MAIL+0(1),
          IT_ITAB-MATNR TO IT_MAIL+1(18),
          '|' TO IT_MAIL+19(1),
          IT_ITAB-LIFNR TO IT_MAIL+20(6),
          '|' TO IT_MAIL+26(1),
          IT_ITAB-INTF_D TO IT_MAIL+27(10),
            '|' TO IT_MAIL+37(1),
          IT_ITAB-DATAB_SA TO IT_MAIL+38(10),
            '|' TO IT_MAIL+48(1),
          IT_ITAB-KBETR_SA TO IT_MAIL+49(13),
            '|' TO IT_MAIL+62(1),
          IT_ITAB-DATAB TO IT_MAIL+63(10),
            '|' TO IT_MAIL+73(1),
          IT_ITAB-DATBI TO IT_MAIL+74(10),
            '|' TO IT_MAIL+84(1),
          IT_ITAB-KBETR TO IT_MAIL+85(13),
            '|' TO IT_MAIL+98(1),
*          IT_ITAB-REMARKS+0(20) TO IT_MAIL+99(20),
*            '|' TO IT_MAIL+119(1),
*          IT_ITAB-DELETED TO IT_MAIL+120(1),
*            '|' TO IT_MAIL+121(1).
        IT_ITAB-REMARKS+0(120) TO IT_MAIL+99(120),
            '|' TO IT_MAIL+219(1),
          IT_ITAB-DELETED TO IT_MAIL+220(1),
            '|' TO IT_MAIL+221(1).

    APPEND IT_MAIL.
    CLEAR: IT_MAIL, L_MESSAGE, IT_MAIL.
    MOVE: '-------------------' TO IT_MAIL+0(19),
         '-------' TO IT_MAIL+19(7),
         '-----------' TO IT_MAIL+26(11),
         '-----------' TO IT_MAIL+37(11),
         '--------------' TO IT_MAIL+48(14),
         '-----------' TO IT_MAIL+62(11),
         '-----------' TO IT_MAIL+73(11),
*         '--------------'  TO IT_MAIL+84(14),
*         '---------------' TO  IT_MAIL+98(15),
*         '--' TO IT_MAIL+113(2),
*         '-' TO IT_MAIL+115(1),
*         '------' TO IT_MAIL+116(6).
        '--------------'  TO IT_MAIL+84(14),
         '---------------' TO  IT_MAIL+98(15),
         '--' TO IT_MAIL+113(2),
         '-' TO IT_MAIL+115(1),
          '------' TO IT_MAIL+116(6),
          '--------------------' to IT_MAIL+122(20),
          '--------------------' to IT_MAIL+142(20),
          '--------------------' to IT_MAIL+162(20),
          '--------------------' to IT_MAIL+182(20),
          '--------------------' to IT_MAIL+202(20).
    APPEND IT_MAIL.
    CLEAR: IT_MAIL.

  ENDLOOP.

***
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
*&---------------------------------------------------------------------*
*&      Form  POPULATE_DATA_VAATS_FOR_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POPULATE_DATA_VAATS_FOR_OUTPUT.
  DATA: L_MESSAGE TYPE SO_TEXT255,
       L_KPEIN(4),
       L_KBETR(12).

  CLEAR: IT_MAIL,IT_MAIL[].

*  MOVE: '-------------------' TO IT_MAIL+0(19),
*        '-------' TO IT_MAIL+19(7),
*        '----------------------' TO IT_MAIL+26(22),
*        '----------------------' TO IT_MAIL+48(22),
*        '----------------------' TO IT_MAIL+70(22),
*        '----------------------' TO IT_MAIL+92(22),
*        '-' TO IT_MAIL+14(1).
*  APPEND IT_MAIL.
*  CLEAR: IT_MAIL.
*
*  MOVE: '|            Material' TO IT_MAIL+0(19),
*        '|Vend' TO IT_MAIL+19(4),
*        '| Error in Reason Code' TO IT_MAIL+26(22),
*        '|  Error in Pur. Group' TO IT_MAIL+48(22),
*        '|  Error in Masterial ' TO IT_MAIL+70(22),
*        '| Error in Vender Code' TO IT_MAIL+92(22),
*        '|' TO IT_MAIL+114(1).
*  APPEND IT_MAIL.
*  CLEAR: IT_MAIL.
*  MOVE: '-------------------' TO IT_MAIL+0(19),
*         '-----' TO IT_MAIL+19(5),
*         '----------------------' TO IT_MAIL+26(22),
*         '----------------------' TO IT_MAIL+48(22),
*         '----------------------' TO IT_MAIL+70(22),
*         '----------------------' TO IT_MAIL+92(22),
*         '-' TO IT_MAIL+14(1).
*  APPEND IT_MAIL.
*  CLEAR: IT_MAIL.
*
*  LOOP AT IT_VAATS_ERROR.
*
*    MOVE: '|' TO IT_MAIL+0(1),
*          IT_VAATS_ERROR-MATNR TO IT_MAIL+1(18),
*          '|' TO IT_MAIL+19(1),
*          IT_VAATS_ERROR-LIFNR TO IT_MAIL+20(4),
*          '|' TO IT_MAIL+24(1),
*          IT_VAATS_ERROR-ERR_RESN TO IT_MAIL+27(21),
*            '|' TO IT_MAIL+48(1),
*          IT_VAATS_ERROR-ERR_EKGRP TO IT_MAIL+49(21),
*            '|' TO IT_MAIL+70(1),
*          IT_VAATS_ERROR-ERR_MATNR TO IT_MAIL+71(21),
*            '|' TO IT_MAIL+92(1),
*          IT_VAATS_ERROR-ERR_LIFNR TO IT_MAIL+93(21),
*            '|' TO IT_MAIL+114(1).
*
*    APPEND IT_MAIL.
*    CLEAR: IT_MAIL, L_MESSAGE, IT_MAIL.
*    MOVE: '-------------------' TO IT_MAIL+0(19),
*         '-----' TO IT_MAIL+19(5),
*         '----------------------' TO IT_MAIL+26(22),
*         '----------------------' TO IT_MAIL+48(22),
*         '----------------------' TO IT_MAIL+70(22),
*         '----------------------' TO IT_MAIL+92(22),
*         '-' TO IT_MAIL+14(1).
*    APPEND IT_MAIL.
*    CLEAR: IT_MAIL.

*  MOVE: '---------------------' TO IT_MAIL+0(20),
*        '----------' TO IT_MAIL+20(10),
*        '---------------------' TO IT_MAIL+30(15),
*        '---------------------' TO IT_MAIL+45(15),
*        '---------------------' TO IT_MAIL+60(15),
*        '---------------------' TO IT_MAIL+75(15),
*        '---------------------' TO IT_MAIL+90(15).
**        '|' to IT_MAIL+90(1).
*  APPEND IT_MAIL.
*  CLEAR: IT_MAIL.

  MOVE: '---------------------' TO IT_MAIL+35(15),
          '---------------------' TO IT_MAIL+50(15),
                  'Error' TO IT_MAIL+70(10),
             '---------------------' TO IT_MAIL+80(15),
         '---------------------' TO IT_MAIL+95(15).

  APPEND IT_MAIL.
  CLEAR: IT_MAIL.


  MOVE: 'Material' TO IT_MAIL+0(20),
        'Vend' TO IT_MAIL+20(10),
        'RSN code' TO IT_MAIL+30(15),
        'Pur Grp' TO IT_MAIL+45(15),
        'Material ' TO IT_MAIL+60(15),
        'Vend Code' TO IT_MAIL+75(15).

  APPEND IT_MAIL.
  CLEAR: IT_MAIL.
  MOVE: '---------------------' TO IT_MAIL+0(20),
        '----------' TO IT_MAIL+20(10),
        '---------------------' TO IT_MAIL+30(15),
        '---------------------' TO IT_MAIL+45(15),
        '---------------------' TO IT_MAIL+60(15),
        '---------------------' TO IT_MAIL+75(15),
        '---------------------' TO IT_MAIL+90(23).
  APPEND IT_MAIL.
  CLEAR: IT_MAIL.

  LOOP AT IT_VAATS_ERROR.

*    MOVE: '|' TO IT_MAIL+0(1),
*          IT_VAATS_ERROR-MATNR TO IT_MAIL+1(19),
*          '|' TO IT_MAIL+20(1),
*          IT_VAATS_ERROR-LIFNR TO IT_MAIL+21(4),
*          '|' TO IT_MAIL+25(1),
*          IT_VAATS_ERROR-ERR_RESN TO IT_MAIL+26(14),
*            '|' TO IT_MAIL+40(1),
*          IT_VAATS_ERROR-ERR_EKGRP TO IT_MAIL+41(14),
*            '|' TO IT_MAIL+55(1),
*          IT_VAATS_ERROR-ERR_MATNR TO IT_MAIL+56(14),
*            '|' TO IT_MAIL+70(1),
*          IT_VAATS_ERROR-ERR_LIFNR TO IT_MAIL+71(14),
*            '|' TO IT_MAIL+90(1).

    MOVE: IT_VAATS_ERROR-MATNR TO IT_MAIL+0(20),
          IT_VAATS_ERROR-LIFNR TO IT_MAIL+20(10),
          IT_VAATS_ERROR-ERR_RESN TO IT_MAIL+30(15),
          IT_VAATS_ERROR-ERR_EKGRP TO IT_MAIL+45(15),
          IT_VAATS_ERROR-ERR_MATNR TO IT_MAIL+60(15),
          IT_VAATS_ERROR-ERR_LIFNR TO IT_MAIL+75(15).


    APPEND IT_MAIL.
    CLEAR: IT_MAIL, L_MESSAGE, IT_MAIL.

    MOVE: '---------------------' TO IT_MAIL+0(20),
            '----------' TO IT_MAIL+20(10),
            '---------------------' TO IT_MAIL+30(15),
            '---------------------' TO IT_MAIL+45(15),
            '---------------------' TO IT_MAIL+60(15),
            '---------------------' TO IT_MAIL+75(15),
            '---------------------' TO IT_MAIL+90(23).
    APPEND IT_MAIL.
    CLEAR: IT_MAIL.


  ENDLOOP.

ENDFORM.                    " POPULATE_DATA_VAATS_FOR_OUTPUT
