************************************************************************
* Program Name      : ZMMR_SA_REVAL
* Author            : Furong Wang
* Creation Date     : 03/2008
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZMMR_SA_REVAL NO STANDARD PAGE HEADING
                             LINE-SIZE 132
                             LINE-COUNT 64(1)
                             MESSAGE-ID ZMMM.

*INCLUDE : ZRMMPMXXR_INCL.
TYPE-POOLS : SLIS.

TABLES: EKKO, EKPO, A016.
DATA : BEGIN OF IT_EXCEL OCCURS 0,
       ITEM(3),
       MATNR(18),
       EXCPRICE(13),
       EXCFDATE(10),
       EXCTDATE(10),
       RSN(3),
       END OF IT_EXCEL.

DATA : BEGIN OF IT_EXCDATA OCCURS 0,
       ITEM(3),
       MATNR LIKE EKPO-MATNR,
       EXCPRICE LIKE KONP-KBETR,
       EXCFDATE LIKE EKKO-KDATB,
       EXCTDATE LIKE EKKO-KDATB,
       RSN LIKE T686C-KZUST,
       END OF IT_EXCDATA.

DATA : BEGIN OF IT_ITAB OCCURS 0,
       MATNR LIKE EKPO-MATNR,
       EXCPRICE(13), " LIKE KONP-KBETR,
       EXCFDATE LIKE EKKO-KDATB,
       EXCTDATE LIKE EKKO-KDATB,
       EBELN LIKE EKPO-EBELN,
       EBELP LIKE EKPO-EBELP,
       SAFDATE LIKE EKKO-KDATB,
       SATDATE LIKE EKKO-KDATB,
       SAPRICE  LIKE KONP-KBETR,
       DELETED(1),
       GRPRICE LIKE KONP-KBETR,
       GRQTY LIKE EKPO-MENGE,
       GRAMT LIKE KONP-KBETR,
       NEWAMT LIKE KONP-KBETR,
       DIFF  LIKE  KONP-KBETR,
       PEINH(5),
       RSN LIKE T686C-KZUST,
       END OF IT_ITAB.

DATA: IT_REVAL_RSN LIKE TABLE OF ZTMM_REVAL_RSN WITH HEADER LINE.

DATA : W_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
       W_EVENTCAT TYPE SLIS_T_EVENT WITH HEADER LINE,
       W_SELFIELD TYPE SLIS_SELFIELD,
       W_SORTCAT  TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
       W_COL_POS  TYPE I,
       W_PROGRAM  LIKE SY-REPID,
       W_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER,
       W_LINE TYPE SLIS_LISTHEADER,
       W_LAYOUT   TYPE SLIS_LAYOUT_ALV.

DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDCAT_FI  TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDCAT_CO  TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE,
       IT_FIELDCAT_DET TYPE LVC_T_FCAT WITH HEADER LINE. "/Detail

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
*      it_color type LVC_T_SCOL,
*      wa_color like line of it_color,
      W_FIELDNAME    LIKE LINE OF IT_FIELDCAT.


DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: OK_CODE      LIKE SY-UCOMM,
      W_REPID  LIKE SY-REPID,
      W_CNT       TYPE   I,
      W_NO_DATA(1).

DATA: W_VENAME LIKE LFA1-NAME1.

DATA : BEGIN OF IT_SA_ALL OCCURS 0,
         EBELN LIKE EKPO-EBELN,
         EBELP LIKE EKPO-EBELP,
         LIFNR LIKE EKKO-LIFNR,
         MATNR LIKE EKPO-MATNR,
         WERKS LIKE EKPO-WERKS,
         LGORT LIKE EKPO-LGORT,
         PEINH LIKE EKPO-PEINH,
         BSTYP LIKE EKKO-BSTYP,
         BUKRS LIKE EKKO-BUKRS,
         BSART LIKE EKKO-BSART,
         EKORG LIKE EKKO-EKORG,
         EKGRP LIKE EKKO-EKGRP,
         KDATB LIKE EKKO-KDATB,
         KDATE LIKE EKKO-KDATE,
         LOEKZ LIKE EKPO-LOEKZ,
       END OF IT_SA_ALL.
*DATA : BEGIN OF IT_INFO_ITEM OCCURS 0,
*         KAPPL LIKE KONP-KAPPL,
*         KSCHL LIKE KONP-KSCHL,
*         KBETR LIKE KONP-KBETR,
*         KPEIN LIKE KONP-KPEIN,
*         KONWA LIKE KONP-KONWA,
*         LIFNR LIKE KONP-LIFNR,
*         KZUST LIKE KONH-KZUST,
*         DATAB LIKE KONH-DATAB,
*         DATBI LIKE KONH-DATBI,
*       END OF IT_INFO_ITEM.
*
*DATA : IT_SACOND_ITEM LIKE IT_INFO_ITEM OCCURS 0 WITH HEADER LINE.

*DATA: IT_MAIL TYPE STANDARD TABLE OF SOLISTI1 INITIAL SIZE 0
*                  WITH HEADER LINE.

DATA: IT_T683S LIKE TABLE OF T683S WITH HEADER LINE.

** wokking variant
DATA : W_SUBRC LIKE SY-SUBRC.

DATA : W_KNUMH LIKE KONH-KNUMH.
DATA: W_ERROR(1).
** bdc
DATA : BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF IT_BDC.

DATA : BEGIN OF IT_MESS OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF IT_MESS.

DATA : IT_MESSAGE LIKE IT_MESS OCCURS 0 WITH HEADER LINE.

DATA : W_MODE LIKE CTU_PARAMS-DISMODE VALUE 'N'.  "'A'.

DATA : W_FILETY LIKE RLGRAP-FILETYPE VALUE 'DAT'.

DATA: W_UPD(1).
DATA: IT_ZTMM_IFSA_LOG LIKE TABLE OF ZTMM_IFSA_LOG WITH HEADER LINE.


DATA: BEGIN OF ALV_TAB OCCURS 1000, " WITH HEADER LINE,
        MATNR LIKE EKBE-MATNR,
        EBELN LIKE EKBE-EBELN,
        EBELP LIKE EKBE-EBELP,
        BEWTP LIKE EKBE-BEWTP,  "PO history category

        GJAHR LIKE EKBE-GJAHR,
        BELNR LIKE EKBE-BELNR,
        BUDAT LIKE EKBE-BUDAT,
        CPUDT LIKE EKBE-CPUDT,
        SHKZG LIKE EKBE-SHKZG,
        MENGE LIKE EKBE-MENGE,
        DMBTR LIKE EKBE-DMBTR,
        REEWR LIKE EKBE-REEWR,   "Inv.Value

        LFGJA LIKE EKBE-LFGJA,
        LFBNR LIKE EKBE-LFBNR,

        XBLNR LIKE EKBE-XBLNR,

        BSART LIKE EKKO-BSART,   "PO type
        BSTYP LIKE EKKO-BSTYP,   "PO category
        EKGRP LIKE EKKO-EKGRP,
        LIFNR LIKE EKKO-LIFNR,

        PEINH LIKE EKPO-PEINH,
        UEBTK LIKE EKPO-UEBTK,
        ELIKZ LIKE EKPO-ELIKZ,
        EREKZ LIKE EKPO-EREKZ,
        LOEKZ LIKE EKPO-LOEKZ,

        MTART LIKE MARA-MTART,
        PROFL LIKE MARA-PROFL,
        MAKTX LIKE MAKT-MAKTX,
        INFNR LIKE EINA-INFNR,

        REFDT     LIKE EKBE-BUDAT,
        YYYYMM(6) TYPE C,
        ZVBELN    LIKE LIKP-VBELN,
        ASN       LIKE LIKP-BORGR_GRP,
        ZBELNR    LIKE BSIS-BELNR,
        DUEDT     LIKE BSIS-ZFBDT,
        CLRDT     LIKE BSIS-AUGDT,
        AUGBL     LIKE BSIS-AUGBL,
        BLART     LIKE BSIS-BLART,

        ZMENGE LIKE EKBE-MENGE,  "sign
        ZDMBTR LIKE EKBE-DMBTR,  "sign
        IVPRC  LIKE EKBE-DMBTR,  "IV price

        SAPRC  LIKE EKBE-DMBTR,  "SA $
        SAUNT  LIKE KONP-KPEIN,  "SA unit
        SAVAL  LIKE EKBE-DMBTR,  "SA value
        SANO(1) TYPE C,          "No SA price

        IFPRC  LIKE EKBE-DMBTR,  "info $
        IFUNT  LIKE KONP-KPEIN,  "info unit
        IFVAL  LIKE EKBE-DMBTR,  "info value

        DIFFA  LIKE EKBE-DMBTR,  "SA-IV
        DIFFB  LIKE EKBE-DMBTR,  "Info-SA
END OF ALV_TAB.
DATA: BEGIN OF ITAB OCCURS 1000, " WITH HEADER LINE,
        MATNR LIKE EKBE-MATNR,
        EBELN LIKE EKBE-EBELN,
        EBELP LIKE EKBE-EBELP,
        BEWTP LIKE EKBE-BEWTP,  "PO history category

        GJAHR LIKE EKBE-GJAHR,
        BELNR LIKE EKBE-BELNR,
        BUDAT LIKE EKBE-BUDAT,
        CPUDT LIKE EKBE-CPUDT,
        SHKZG LIKE EKBE-SHKZG,
        MENGE LIKE EKBE-MENGE,
        DMBTR LIKE EKBE-DMBTR,
        REEWR LIKE EKBE-REEWR,   "Inv.Value

        LFGJA LIKE EKBE-LFGJA,
        LFBNR LIKE EKBE-LFBNR,

        XBLNR LIKE EKBE-XBLNR,

        BSART LIKE EKKO-BSART,   "PO type
        BSTYP LIKE EKKO-BSTYP,   "PO category
        EKGRP LIKE EKKO-EKGRP,
        LIFNR LIKE EKKO-LIFNR,

        PEINH LIKE EKPO-PEINH,
        UEBTK LIKE EKPO-UEBTK,
        ELIKZ LIKE EKPO-ELIKZ,
        EREKZ LIKE EKPO-EREKZ,
        LOEKZ LIKE EKPO-LOEKZ,

        MTART LIKE MARA-MTART,
        PROFL LIKE MARA-PROFL,
        MAKTX LIKE MAKT-MAKTX,
        INFNR LIKE EINA-INFNR,

        REFDT     LIKE EKBE-BUDAT,
        YYYYMM(6) TYPE C,
        ZVBELN    LIKE LIKP-VBELN,
        ASN       LIKE LIKP-BORGR_GRP,
        ZBELNR    LIKE BSIS-BELNR,
        DUEDT     LIKE BSIS-ZFBDT,
        CLRDT     LIKE BSIS-AUGDT,
        AUGBL     LIKE BSIS-AUGBL,
        BLART     LIKE BSIS-BLART,

        ZMENGE LIKE EKBE-MENGE,  "sign
        ZDMBTR LIKE EKBE-DMBTR,  "sign
        IVPRC  LIKE EKBE-DMBTR,  "IV price

        SAPRC  LIKE EKBE-DMBTR,  "SA $
        SAUNT  LIKE KONP-KPEIN,  "SA unit
        SAVAL  LIKE EKBE-DMBTR,  "SA value
        SANO(1) TYPE C,          "No SA price

        IFPRC  LIKE EKBE-DMBTR,  "info $
        IFUNT  LIKE KONP-KPEIN,  "info unit
        IFVAL  LIKE EKBE-DMBTR,  "info value

        DIFFA  LIKE EKBE-DMBTR,  "SA-IV
        DIFFB  LIKE EKBE-DMBTR,  "Info-SA
END OF ITAB.

RANGES: R_LIFNR FOR EKKO-LIFNR,
          R_MATNR FOR EKPO-MATNR,
          R_REFDT FOR SY-DATUM,
          R_EBELN FOR EKKO-EBELN,
          R_EBELP FOR EKPO-EBELP.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
PARAMETERS:
  P_FILE  LIKE RLGRAP-FILENAME OBLIGATORY,
  P_LIFNR LIKE EKKO-LIFNR  OBLIGATORY,
  P_DATE LIKE SY-DATUM DEFAULT SY-DATUM.
SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-007.
PARAMETERS: P_COM RADIOBUTTON GROUP GRP1 DEFAULT 'X',
            P_MOD RADIOBUTTON GROUP GRP1.
SELECTION-SCREEN END OF BLOCK BLOCK2.


SELECTION-SCREEN BEGIN OF BLOCK BLOCK3 WITH FRAME TITLE TEXT-006.
PARAMETERS:
  P_TEST AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK BLOCK3.


INITIALIZATION.
**---

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING P_FILE 'O'.

START-OF-SELECTION.
** Not used on 08/04/14 (
MESSAGE E009 WITH 'This program is no longer available'.
** )
  PERFORM UPLOAD.
  PERFORM CHECK_REASON_CODE.
  IF W_ERROR IS INITIAL.

    IF IT_EXCEL[] IS INITIAL.
      MESSAGE S999 WITH TEXT-M01.
    ELSE.
      PERFORM GET_SCHEDULING_AGREEMENT.
      PERFORM MAKE_DATA.
*   PERFORM GET_COND_PRICE.
*    PERFORM CHANGE_CONDITIONS.
      PERFORM DISPLAY_DATA.
*    PERFORM COMMENT_BUILD.     " USING w_top_of_page[].
*    PERFORM MAKE_ALV_GRID.
    ENDIF.
  ENDIF.

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
  SELECT SINGLE NAME1 INTO W_VENAME
     FROM LFA1
     WHERE LIFNR = P_LIFNR.

  CLEAR: IT_SA_ALL, IT_SA_ALL[].
  SELECT B~EBELN
         EBELP
         MATNR
         WERKS
         LGORT
         LIFNR
         PEINH
         A~BSTYP
         A~BUKRS
         BSART
         EKORG
         EKGRP
         KDATB
         KDATE
         B~LOEKZ
               INTO CORRESPONDING FIELDS OF TABLE IT_SA_ALL
               FROM EKKO AS A INNER JOIN EKPO AS B
                 ON A~MANDT EQ B~MANDT
                AND A~EBELN EQ B~EBELN
               FOR ALL ENTRIES IN IT_EXCEL
               WHERE A~BSTYP EQ 'L'
                AND A~LIFNR = P_LIFNR
                AND B~MATNR = IT_EXCEL-MATNR.
*                AND A~LOEKZ EQ SPACE
*                AND B~LOEKZ EQ SPACE.
*                AND ELIKZ EQ SPACE.
*                AND ( KDATB LE SY-DATUM
*                  AND KDATE GE SY-DATUM ).

ENDFORM.                    " get_scheduling_agreement


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
*&      Form  UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD.
  DATA: L_DATE_C(8).

  CALL FUNCTION 'UPLOAD'
  EXPORTING
*   CODEPAGE                      = ' '
    FILENAME                      = P_FILE
    FILETYPE                      = W_FILETY
*   ITEM                          = ' '
*   FILEMASK_MASK                 = ' '
*   FILEMASK_TEXT                 = ' '
*   FILETYPE_NO_CHANGE            = ' '
*   FILEMASK_ALL                  = ' '
*   FILETYPE_NO_SHOW              = ' '
*   LINE_EXIT                     = ' '
*   USER_FORM                     = ' '
*   USER_PROG                     = ' '
*   SILENT                        = 'S'
* IMPORTING
*   FILESIZE                      =
*   CANCEL                        =
*   ACT_FILENAME                  =
*   ACT_FILETYPE                  =
   TABLES
     DATA_TAB                      = IT_EXCEL.

  IF SY-SUBRC <> 0.
  ENDIF.


  CASE SY-SUBRC.
    WHEN 0.
    WHEN 2.
      MESSAGE E000 WITH 'File open error'.
    WHEN 3.
      MESSAGE E000 WITH 'File read error'.
    WHEN OTHERS.
      MESSAGE E000 WITH 'Upload error'.
  ENDCASE.
  DELETE IT_EXCEL INDEX 1.
  LOOP AT IT_EXCEL.
    IF IT_EXCEL-ITEM IS INITIAL.
      EXIT.
    ELSE.
      IT_EXCDATA = IT_EXCEL.
      IT_EXCDATA-EXCPRICE = IT_EXCEL-EXCPRICE.
      CONCATENATE '20' IT_EXCEL-EXCFDATE+6(2) IT_EXCEL-EXCFDATE+0(2)
      IT_EXCEL-EXCFDATE+3(2) INTO L_DATE_C.
      WRITE: L_DATE_C TO IT_EXCDATA-EXCFDATE.

      CONCATENATE '20' IT_EXCEL-EXCTDATE+6(2) IT_EXCEL-EXCTDATE+0(2)
      IT_EXCEL-EXCTDATE+3(2) INTO L_DATE_C.
      WRITE: L_DATE_C TO IT_EXCDATA-EXCTDATE.
      IT_EXCDATA-RSN = IT_EXCEL-RSN.
      APPEND IT_EXCDATA.
      CLEAR: IT_EXCDATA,IT_EXCEL.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " UPLOAD

*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
FORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING DEF_PATH LIKE RLGRAP-FILENAME
                                          MODE     TYPE C.

  DATA: TMP_FILENAME LIKE RLGRAP-FILENAME.
  DATA: TMP_MASK(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  DATA: FIELDLN TYPE I.
  FIELD-SYMBOLS: <TMP_SYM>.

  TMP_MASK = ',*.*,*.*.'.
  FIELDLN = STRLEN( DEF_PATH ) - 1.
  ASSIGN DEF_PATH+FIELDLN(1) TO <TMP_SYM>.
  IF <TMP_SYM> = '/' OR <TMP_SYM> = '\'.
    CLEAR <TMP_SYM>.
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            DEF_FILENAME     = P_FILE
            DEF_PATH         = DEF_PATH
*           MASK             = ',*.*,*.*.'
            MASK             = TMP_MASK
            MODE             = MODE
*           TITLE            = ' '
       IMPORTING
            FILENAME         = TMP_FILENAME
*         RC               =
       EXCEPTIONS
            INV_WINSYS       = 01
            NO_BATCH         = 02
            SELECTION_CANCEL = 03
            SELECTION_ERROR  = 04.

  IF SY-SUBRC = 0.
    P_FILE = TMP_FILENAME.
  ELSE.
* IF SY-SUBRC = 01.    "// Does not work, why ???
*   MESSAGELINE = 'Not supported'.
* ENDIF.
  ENDIF.

ENDFORM.                               " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  make_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_DATA.
  DATA: L_FLAG(1).
  DATA: L_DATAB LIKE A018-DATAB,
         L_DATBI LIKE A018-DATBI,
         L_GRQTY LIKE EKBE-MENGE,
         L_GRAMT LIKE EKBE-DMBTR,
         L_PRICE LIKE EKBE-DMBTR,
         L_NEWAMT LIKE EKBE-DMBTR,
         L_DIFF LIKE EKBE-DMBTR,
         L_RECNO TYPE I.

  DATA: LT_A016 LIKE TABLE OF A016 WITH HEADER LINE,
        LT_ITAB LIKE TABLE OF IT_ITAB WITH HEADER LINE,
        LT_EKBE LIKE TABLE OF EKBE WITH HEADER LINE.
  DATA: BEGIN OF LT_TEMP OCCURS 0,
        DMBTR LIKE EKBE-DMBTR,
        MENGE LIKE EKBE-MENGE,
        SHKZG LIKE EKBE-SHKZG,
        REEWR LIKE EKBE-REEWR,
        PRICE  LIKE EKBE-DMBTR,
        END OF LT_TEMP.

  R_LIFNR-SIGN  = 'I'.
  R_LIFNR-OPTION  = 'EQ'.
  R_LIFNR-LOW  = P_LIFNR.
  APPEND R_LIFNR.

  SORT IT_SA_ALL BY MATNR.
  LOOP AT IT_EXCDATA.
    CLEAR: L_FLAG.
*    CLEAR : IT_SACOND_ITEM, IT_SACOND_ITEM[]
    CLEAR: W_KNUMH.
    MOVE-CORRESPONDING IT_EXCDATA TO LT_ITAB.
    LOOP AT IT_SA_ALL WHERE MATNR = IT_EXCDATA-MATNR.
      SELECT * INTO TABLE LT_A016
                     FROM A016
                    WHERE KAPPL EQ 'M'
                      AND KSCHL EQ 'PB00'
                      AND EVRTN EQ IT_SA_ALL-EBELN
                      AND EVRTP EQ IT_SA_ALL-EBELP
                      AND DATAB <= IT_EXCDATA-EXCTDATE
                      AND DATBI >= IT_EXCDATA-EXCFDATE.

      LOOP AT LT_A016.
*        IF SY-SUBRC = 0.
        SELECT SINGLE KBETR INTO LT_ITAB-SAPRICE
          FROM KONP
          WHERE KNUMH = LT_A016-KNUMH
            AND KSCHL = 'PB00'.
        LT_ITAB-EBELN = IT_SA_ALL-EBELN.
        LT_ITAB-EBELP = IT_SA_ALL-EBELP.
        LT_ITAB-PEINH = IT_SA_ALL-PEINH.
        LT_ITAB-SAFDATE = LT_A016-DATAB.
        LT_ITAB-SATDATE = LT_A016-DATBI.
        LT_ITAB-DELETED = IT_SA_ALL-LOEKZ.
        APPEND LT_ITAB.
        L_FLAG = 'X'.
*        ENDIF.
      ENDLOOP.
      CLEAR: IT_SA_ALL, LT_A016[], LT_A016.
    ENDLOOP.
    IF L_FLAG IS INITIAL.
      APPEND LT_ITAB.
    ENDIF.
    CLEAR: LT_ITAB.
  ENDLOOP.

  LOOP AT LT_ITAB.
    REFRESH: R_MATNR, R_REFDT, R_EBELN, R_EBELP.

    IT_ITAB = LT_ITAB.
    IF NOT LT_ITAB-EBELN IS INITIAL.

      R_MATNR-SIGN  = 'I'.
      R_MATNR-OPTION  = 'EQ'.
      R_MATNR-LOW  = LT_ITAB-MATNR.
      APPEND R_MATNR.

      R_REFDT-SIGN  = 'I'.
      R_REFDT-OPTION  = 'BT'.
** Changed by Furong on 12/05/08
*** Changed by Furong on 05/07/09 comment following 4 statements

*      R_REFDT-LOW = LT_ITAB-SAFDATE.
*      R_REFDT-HIGH = LT_ITAB-SATDATE.
**      R_REFDT-LOW  = LT_ITAB-EXCFDATE.
**      R_REFDT-HIGH = LT_ITAB-EXCTDATE.

      IF IT_ITAB-SAFDATE = IT_ITAB-EXCFDATE.
        MOVE : IT_ITAB-EXCFDATE TO R_REFDT-LOW.
        IF IT_ITAB-SATDATE < IT_ITAB-EXCTDATE.
          MOVE IT_ITAB-SATDATE TO R_REFDT-HIGH.
        ELSE.
          MOVE IT_ITAB-EXCTDATE TO R_REFDT-HIGH.
        ENDIF.
      ENDIF.

      IF IT_ITAB-EXCFDATE > IT_ITAB-SAFDATE AND
         IT_ITAB-EXCFDATE <= IT_ITAB-SATDATE.
        MOVE IT_ITAB-EXCFDATE TO R_REFDT-LOW.
        IF IT_ITAB-EXCTDATE > IT_ITAB-SATDATE.
          MOVE IT_ITAB-SATDATE TO R_REFDT-HIGH.
        ELSE.
          MOVE IT_ITAB-EXCTDATE TO R_REFDT-HIGH.
        ENDIF.
      ENDIF.

      IF IT_ITAB-EXCFDATE < IT_ITAB-SAFDATE.
        MOVE IT_ITAB-SAFDATE TO R_REFDT-LOW.
        IF IT_ITAB-EXCTDATE > IT_ITAB-SATDATE.
          MOVE IT_ITAB-SATDATE TO R_REFDT-HIGH.
        ELSE.
          MOVE IT_ITAB-EXCTDATE TO R_REFDT-HIGH.
        ENDIF.
      ENDIF.

*** End of change on 05/07/09
** End of change
      APPEND R_REFDT.

      R_EBELN-SIGN  = 'I'.
      R_EBELN-OPTION  = 'EQ'.
      R_EBELN-LOW  = LT_ITAB-EBELN.
      APPEND R_EBELN.

      R_EBELP-SIGN  = 'I'.
      R_EBELP-OPTION  = 'EQ'.
      R_EBELP-LOW  = LT_ITAB-EBELP.
      APPEND R_EBELP.

      FREE  MEMORY ID 'SUBMFE'.

      SUBMIT ZMMR_SA_REVAL_GET_INV                          "ZRFI013
                WITH P_BUKRS  = 'H201'
                WITH S_LIFNR IN R_LIFNR
                WITH S_MATNR IN R_MATNR
                WITH S_REFDT IN R_REFDT
                WITH S_EBELN IN R_EBELN
                WITH S_EBELP IN R_EBELP
                WITH P_ALV = ' '
                EXPORTING LIST TO MEMORY
                AND RETURN.

      CLEAR: ALV_TAB, ALV_TAB[].
      CLEAR: L_GRQTY, L_GRAMT, L_PRICE, L_RECNO.

      IMPORT ALV_TAB FROM MEMORY ID 'SUBMFE'.

*    IMPORT ITAB FROM MEMORY ID 'SUBMFE'.
*    ALV_TAB[] = ITAB[].
      SORT ALV_TAB BY MATNR.

      LOOP AT ALV_TAB.
*      if LT_ITAB-EXCPRICE = ALV_TAB-IVPRC.
        L_PRICE = L_PRICE + ALV_TAB-IVPRC.
        L_GRQTY = L_GRQTY + ALV_TAB-ZMENGE.
        L_GRAMT = L_GRAMT + ALV_TAB-ZDMBTR.
        IF ALV_TAB-ZMENGE <> 0.
          L_RECNO = L_RECNO + 1.
        ENDIF.
        CLEAR: ALV_TAB.
      ENDLOOP.

      IT_ITAB-GRQTY = L_GRQTY.
      IT_ITAB-GRAMT =  L_GRAMT.

*    L_PRICE = L_PRICE / L_RECNO.
      IF L_GRQTY <> 0.
        L_PRICE = L_GRAMT / L_GRQTY.
      ENDIF.
      IT_ITAB-GRPRICE = L_PRICE.

** Changed by Furong on 09/18/08
      IF P_MOD = 'X'.
        IT_ITAB-EXCPRICE = IT_ITAB-SAPRICE + IT_ITAB-EXCPRICE.
      ENDIF.
** End of change on 09/18/08

      L_NEWAMT = IT_ITAB-EXCPRICE * L_GRQTY.
      IT_ITAB-NEWAMT = L_NEWAMT.
      L_DIFF =  L_GRAMT - L_NEWAMT.
      IT_ITAB-DIFF = L_DIFF.
    ENDIF.
    APPEND IT_ITAB.

  ENDLOOP.
ENDFORM.                    " make_data
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA.
  CALL SCREEN 200.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'T200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV_200 OUTPUT.
  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_ITAB'.
    PERFORM ASSIGN_ITAB_TO_ALV.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT.
  DATA:   W_REPID LIKE SY-REPID.
  CREATE OBJECT GRID_CONTAINER
          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL
          EXCEPTIONS
           CNTL_ERROR = 1
           CNTL_SYSTEM_ERROR = 2
           CREATE_ERROR = 3
           LIFETIME_ERROR = 4
           LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  W_REPID = SY-REPID.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT ALV_GRID
         EXPORTING I_PARENT = GRID_CONTAINER
                   I_APPL_EVENTS = 'X'.

ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
  WA_IS_LAYOUT-EDIT       = ' '.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
*  WA_IS_LAYOUT-INFO_FNAME = 'IF'.
*  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  WA_VARIANT-REPORT       = SY-REPID.
  WA_VARIANT-USERNAME     = SY-UNAME.

ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_SORTCAT_DISPLAY.
*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'LIFNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 2.
*  it_sort-fieldname      = 'MATNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
ENDFORM.                    " build_sortcat_display

*---------------------------------------------------------------------*
*       FORM setting_fieldcat                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDCAT                                                    *
*  -->  P_GUBUN                                                       *
*  -->  P_FIELD                                                       *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM SETTING_FIELDCAT TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : L_COL(40).

  FIELD-SYMBOLS <FS>.

* START - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.

    READ TABLE IT_FIELDNAME INTO W_FIELDNAME
                            WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH 'Check field catalog'.
    ENDIF.

    MOVE: W_FIELDNAME-FIELDNAME TO P_FIELDCAT-FIELDNAME.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' P_FIELD  INTO L_COL.
  ASSIGN (L_COL) TO <FS>.
  MOVE   P_VALUE TO <FS>.

* END - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'E'.
    ADD 1 TO W_CNT.
    P_FIELDCAT-COL_POS = W_CNT.
    APPEND P_FIELDCAT.
  ENDIF.
ENDFORM.                    " setting_fieldcat

*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV.

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY

   EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT
               I_SAVE           = WA_SAVE
               IS_VARIANT       = WA_VARIANT
*               i_default        = space
*               it_toolbar_excluding = it_toolbar_excluding[]
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = IT_ITAB[].
*               it_sort          = it_sort[].

ENDFORM.                    " assign_itab_to_alv

*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING P_ITAB.

  DATA: LW_ITAB TYPE SLIS_TABNAME,
        LW_WAERS LIKE T001-WAERS,
        L_RQTY(9),
        L_DATUM(8),
        L_CN(2) TYPE N.

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_CNT,W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = W_REPID
            I_INTERNAL_TABNAME = LW_ITAB
            I_INCLNAME         = W_REPID
       CHANGING
            CT_FIELDCAT        = IT_FIELDNAME.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
*
*                                 'S' 'LIFNR'       ' ',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'COLTEXT'     'Vendor',
*                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'EXCPRICE'       ' ',
                                  ' ' 'COLTEXT'     'New Price',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'CURR',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'EXCFDATE'    ' ',
                                  ' ' 'COLTEXT'     'Date From',
                                  ' ' 'DATATYPE'    'DATS',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'EXCTDATE'    ' ',
                                  ' ' 'COLTEXT'     'Date To',
                                  ' ' 'DATATYPE'    'DATS',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'EBELN'       ' ',
                                  ' ' 'COLTEXT'     'SA Number',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'EBELP'       ' ',
                                  ' ' 'COLTEXT'     'I/No',
                                  'E' 'OUTPUTLEN'   '3',

                                  'S' 'SAFDATE'    ' ',
                                  ' ' 'COLTEXT'     'SA Date From',
                                  ' ' 'DATATYPE'    'DATS',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'SATDATE'    ' ',
                                  ' ' 'COLTEXT'     'SA Date To',
                                  ' ' 'DATATYPE'    'DATS',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'DELETED'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Del',
                                  'E' 'OUTPUTLEN'   '3',

                                  'S' 'GRPRICE'       ' ',
                                  ' ' 'COLTEXT'     'Old Avg Price',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'CURR',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'GRQTY'       ' ',
                                  ' ' 'COLTEXT'     'GR Qty',
                                  ' ' 'DECIMALS_O'  '0',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'DEC',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'GRAMT'       ' ',
                                  ' ' 'COLTEXT'     'Old Payment',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'CURR',
                                  'E' 'OUTPUTLEN'   '13',


                                 'S' 'NEWAMT'       ' ',
                                  ' ' 'COLTEXT'     'New Payment',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'CURR',
                                  'E' 'OUTPUTLEN'   '13',

                                'S' 'DIFF'       ' ',
                                  ' ' 'COLTEXT'     'Difference',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'CURR',
                                  'E' 'OUTPUTLEN'   '14'.

ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  CASE OK_CODE.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
*    WHEN 'DOWNLOAD'.
*      PERFORM .
    WHEN 'PRINT'.
      PERFORM CALL_ZRFIG02.
    WHEN 'REEVAL'.
      PERFORM RE_EVAL.
    WHEN 'SAUPDATE'.
      PERFORM UPDATE_SA.
    WHEN 'DE-AGGRE'.
      PERFORM DE_AGGREGATE.
    WHEN 'DISP-SA'.
      PERFORM DISPLAY_SA.
    WHEN 'INFOREC'.
      PERFORM DISPLAY_INFO.
    WHEN '%_GC'.
      PERFORM DOUBLE_CLICK_RTN.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  CALL_ZRFIG02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_ZRFIG02.
  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
        LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I,
        L_BUKRS(4) VALUE 'H201'.

  RANGES: LR_MATNR FOR MARA-MATNR,
          LR_LIFNR FOR LFA1-LIFNR,
          LR_GJAHR FOR EKBE-GJAHR,
          LR_BUDAT FOR SY-DATUM.

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M12.
  ENDIF.
  READ TABLE IT_ITAB INDEX LT_ROWS-INDEX.
*  lr_matnr-sign = 'I'.
*  lr_matnr-option = 'EQ'.
*  lr_matnr-low = it_output-matnr.
*  APPEND lr_matnr.

  LR_GJAHR-SIGN = 'I'.
  LR_GJAHR-OPTION = 'EQ'.
  LR_GJAHR-LOW = IT_ITAB-EXCFDATE+0(4).
  APPEND LR_GJAHR.

  LR_LIFNR-SIGN = 'I'.
  LR_LIFNR-OPTION = 'EQ'.
  LR_LIFNR-LOW = P_LIFNR.
  APPEND LR_LIFNR.

  LR_BUDAT-SIGN = 'I'.
  LR_BUDAT-OPTION = 'BT'.
  LR_BUDAT-LOW = SY-DATUM.
  LR_BUDAT-HIGH = SY-DATUM.
*
*  LR_BUDAT-LOW = IT_ITAB-EXCFDATE.
*  LR_BUDAT-HIGH = IT_ITAB-EXCTDATE.
  APPEND LR_BUDAT.

*  SUBMIT ZRFIG02    "via selection-screen
*         WITH P_BUKRS = L_BUKRS
*         WITH S_GJAHR IN LR_GJAHR
*         WITH S_BUDAT IN LR_BUDAT
*         WITH S_LIFNR IN LR_LIFNR
*         AND RETURN.
  SET PARAMETER ID 'BUK' FIELD L_BUKRS.
  SET PARAMETER ID 'GJR' FIELD LR_GJAHR-LOW.
  CALL TRANSACTION 'ZRFIG02'.
ENDFORM.                    " CALL_ZRFIG02
*&---------------------------------------------------------------------*
*&      Form  RE_EVAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RE_EVAL.
  DATA: BEGIN OF LT_EKBE OCCURS 0,
    BELNR LIKE EKBE-BELNR,
    CPUDT LIKE EKBE-CPUDT,
    CPUTM LIKE EKBE-CPUTM,
    END OF LT_EKBE.

  DATA : BEGIN OF LT_REVAL OCCURS 0,
       MATNR LIKE EKPO-MATNR,
       EBELN LIKE EKPO-EBELN,
       EBELP LIKE EKPO-EBELP,
       EXCFDATE LIKE IT_ITAB-EXCFDATE,
       EXCTDATE LIKE IT_ITAB-EXCTDATE,
       END OF LT_REVAL.

  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
       LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I.
  RANGES: LR_WEDAT FOR SY-DATUM.

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  REFRESH  LT_REVAL.
  LOOP AT LT_ROWS.
    READ TABLE IT_ITAB INDEX LT_ROWS-INDEX.
    IF IT_ITAB-MATNR = ' '.
      MESSAGE E000(ZZ) WITH TEXT-M13.
    ENDIF.

    READ TABLE LT_REVAL WITH KEY
               MATNR = IT_ITAB-MATNR
               EBELN = IT_ITAB-EBELN
               EBELP = IT_ITAB-EBELP
               EXCFDATE = IT_ITAB-EXCFDATE
               EXCTDATE = IT_ITAB-EXCTDATE.
    .
    IF SY-SUBRC <> 0.
      LT_REVAL-MATNR = IT_ITAB-MATNR.
      LT_REVAL-EBELN = IT_ITAB-EBELN.
      LT_REVAL-EBELP = IT_ITAB-EBELP.
      LT_REVAL-EXCFDATE = IT_ITAB-EXCFDATE.
      LT_REVAL-EXCTDATE = IT_ITAB-EXCTDATE.

      APPEND LT_REVAL.

      LR_WEDAT-SIGN = 'I'.
      LR_WEDAT-OPTION = 'BT'.
      LR_WEDAT-LOW = IT_ITAB-EXCFDATE.
      LR_WEDAT-HIGH = IT_ITAB-EXCTDATE.
      APPEND LR_WEDAT.

      SUBMIT RMMR1MRB    "via selection-screen
             WITH PA_EBELN = IT_ITAB-EBELN
             WITH PA_EBELP = IT_ITAB-EBELP
             WITH SO_WEDAT IN LR_WEDAT
             WITH PA_NBWDT = P_DATE
             WITH PA_ZTERM = 'P030'
             WITH PA_GUZTE  = 'P030'
             WITH PA_XTEST = P_TEST
             AND RETURN.

*    SELECT SINGLE BELNR INTO IT_REVAL_RSN-BELNR
*      FROM EKBE
*      WHERE EBELN = IT_ITAB-EBELN
*        AND EBELP = IT_ITAB-EBELP
*        AND BUDAT = P_DATE
*        AND VGABE = '3'
*        AND BEWTP = 'W'
*        AND BWART = '  '
*        AND ERNAM = SY-UNAME.
      DO 3 TIMES.
        CLEAR: LT_EKBE, LT_EKBE[].
        SELECT BELNR CPUDT CPUTM INTO TABLE LT_EKBE
             FROM EKBE
             WHERE EBELN = IT_ITAB-EBELN
               AND EBELP = IT_ITAB-EBELP
*             AND BUDAT = P_DATE
               AND CPUDT = SY-DATUM
               AND VGABE = '3'
               AND BEWTP = 'W'
               AND BWART = '  '
               AND ERNAM = SY-UNAME.
        IF SY-SUBRC = 0.
          EXIT.
        ELSE.
          WAIT UP TO '0.1' SECONDS.
        ENDIF.
      ENDDO.

      SORT LT_EKBE DESCENDING BY CPUDT CPUTM.
      READ TABLE LT_EKBE INDEX 1.
      IT_REVAL_RSN-BELNR = LT_EKBE-BELNR.

      IF SY-SUBRC = 0 AND P_TEST IS INITIAL.
        IT_REVAL_RSN-KZUST = IT_ITAB-RSN.
        IT_REVAL_RSN-GJAHR = SY-DATUM+0(4).
        IT_REVAL_RSN-ZUSER = SY-UNAME.
        IT_REVAL_RSN-ZDATE = SY-DATUM.
        IT_REVAL_RSN-ZTIME = SY-UZEIT.
        APPEND IT_REVAL_RSN.
      ENDIF.
      CLEAR: IT_REVAL_RSN.
    ENDIF.
  ENDLOOP.
  IF IT_REVAL_RSN[] IS INITIAL.
  ELSE.
    MODIFY ZTMM_REVAL_RSN FROM TABLE IT_REVAL_RSN.
    IF SY-SUBRC = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.
ENDFORM.                    " RE_EVAL
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_SA.
  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
        LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I.
  DATA : L_MESSA(80).
  DATA : L_FIELD01(20),
        L_DATAB(10),
        L_DATBI(10),
        L_KBETR(13),
        L_KPEIN(5),
        L_KZUST LIKE KONH-KZUST,
        L_KBETR_TEMP LIKE KONP-KBETR,
        L_FLAG(1).
  DATA : L_99991231 TYPE D VALUE '99991231'.

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
            IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                      ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  LOOP AT LT_ROWS.
    CLEAR : IT_BDC, IT_BDC[], IT_MESS, IT_MESS[], L_FIELD01,
           L_DATAB, L_DATBI, L_KBETR .

    READ TABLE IT_ITAB INDEX LT_ROWS-INDEX.
*    IF NOT IT_ITAB-DELETED IS INITIAL or IT_ITAB-ebeln IS INITIAL.
    IF IT_ITAB-EBELN IS INITIAL.
      CLEAR: IT_ITAB.
      CONTINUE.
    ENDIF.

** Changed by Furong on 11/24/08
*    IF P_MOD = 'X'.
*    IF IT_ITAB-SAFDATE >= IT_ITAB-EXCFDATE AND
*       IT_ITAB-SAFDATE <= IT_ITAB-EXCTDATE.
*      IF IT_ITAB-SATDATE >= IT_ITAB-EXCFDATE AND
*         IT_ITAB-SATDATE <= IT_ITAB-EXCTDATE.
*        WRITE: IT_ITAB-EXCFDATE TO L_DATAB,
*               IT_ITAB-SATDATE TO L_DATBI.
*      ELSE.
*        WRITE: IT_ITAB-SAFDATE TO L_DATAB,
*               IT_ITAB-EXCTDATE TO L_DATBI.
*      ENDIF.
*    ELSE.
*      IF IT_ITAB-SATDATE >= IT_ITAB-EXCFDATE AND
*         IT_ITAB-SATDATE <= IT_ITAB-EXCTDATE.
*        WRITE : IT_ITAB-EXCFDATE TO L_DATAB,
*                IT_ITAB-SATDATE TO L_DATBI.
*      ELSE.
*        IF IT_ITAB-SATDATE > IT_ITAB-EXCTDATE.
*          WRITE: IT_ITAB-SAFDATE TO L_DATAB,
*                  IT_ITAB-SATDATE TO L_DATBI.
*        ELSE.
*          CLEAR: IT_ITAB.
*          CONTINUE.
*        ENDIF.
*      ENDIF.
*    ENDIF.

*    IF IT_ITAB-SAFDATE = IT_ITAB-EXCFDATE
*       AND IT_ITAB-SATDATE = IT_ITAB-EXCTDATE.
*      WRITE : IT_ITAB-EXCFDATE TO L_DATAB,
*              IT_ITAB-EXCTDATE TO L_DATBI.
*    ENDIF.

    IF IT_ITAB-SAFDATE = IT_ITAB-EXCFDATE.
      WRITE : IT_ITAB-EXCFDATE TO L_DATAB.
      IF IT_ITAB-SATDATE < IT_ITAB-EXCTDATE.
        WRITE: IT_ITAB-SATDATE TO L_DATBI.
      ELSE.
        WRITE: IT_ITAB-EXCTDATE TO L_DATBI.
      ENDIF.
    ENDIF.

    IF IT_ITAB-EXCFDATE > IT_ITAB-SAFDATE AND
       IT_ITAB-EXCFDATE <= IT_ITAB-SATDATE.
      WRITE : IT_ITAB-EXCFDATE TO L_DATAB.
      IF IT_ITAB-EXCTDATE > IT_ITAB-SATDATE.
        WRITE: IT_ITAB-SATDATE TO L_DATBI.
      ELSE.
        WRITE: IT_ITAB-EXCTDATE TO L_DATBI.
      ENDIF.
    ENDIF.

    IF IT_ITAB-EXCFDATE < IT_ITAB-SAFDATE.
      WRITE : IT_ITAB-SAFDATE TO L_DATAB.
      IF IT_ITAB-EXCTDATE > IT_ITAB-SATDATE.
        WRITE: IT_ITAB-SATDATE TO L_DATBI.
      ELSE.
        WRITE: IT_ITAB-EXCTDATE TO L_DATBI.
      ENDIF.
    ENDIF.


*    ELSE.
*      WRITE: IT_ITAB-EXCFDATE TO L_DATAB,
*             IT_ITAB-EXCTDATE TO L_DATBI.
*    ENDIF.
** End of change on 11/24/08

    PERFORM DYNPRO USING : 'X'  'SAPMM06E'        '0205',
                            ' '  'RM06E-EVRTN'     IT_ITAB-EBELN,
                            ' '  'BDC_OKCODE'      '=AB'.

   CONCATENATE 'RM06E-TCSELFLAG(' IT_ITAB-EBELP+3(2) ')' INTO L_FIELD01
                                                               .

    PERFORM DYNPRO USING : 'X'  'SAPMM06E'        '0220',
                           ' '  L_FIELD01         'X',
                           ' '  'BDC_OKCODE'      '=KO'.

    PERFORM DYNPRO USING : 'X'  'SAPLV14A'        '0102',
                           ' '  'BDC_OKCODE'      '=NEWD'.

    PERFORM DYNPRO USING : 'X'  'SAPMV13A'        '0201',
                           ' '  'RV13A-DATAB'     L_DATAB,
                           ' '  'RV13A-DATBI'     L_DATBI.
*    IF SY-SUBRC EQ 0.
    CLEAR : L_KBETR, L_KPEIN.
    WRITE : IT_ITAB-EXCPRICE TO L_KBETR.
*                               CURRENCY 'USD'.
    WRITE : '1' TO L_KPEIN.
    PERFORM DYNPRO USING : ' '  'KONP-KBETR(01)'  L_KBETR,
                           ' '  'BDC_CURSOR'      'KONP-KPEIN(01)',
                           ' '  'KONP-KPEIN(01)'  L_KPEIN.
*    ENDIF.

*---
*    LOOP AT IT_INFO_ITEM.
*      CLEAR : L_KBETR_TEMP.
*      IF IT_INFO_ITEM-KONWA EQ '%'.
*        L_KBETR_TEMP = IT_INFO_ITEM-KBETR / 10.
*        MOVE : L_KBETR_TEMP TO L_KBETR.
*      ELSE.
*        WRITE : IT_INFO_ITEM-KBETR TO L_KBETR
*                                   CURRENCY IT_INFO_ITEM-KONWA.
*        WRITE : IT_INFO_ITEM-KPEIN TO L_KPEIN.
*      ENDIF.
*      IF SY-TABIX EQ 1.
*       PERFORM DYNPRO USING : ' '  'KONP-KSCHL(02)'  IT_INFO_ITEM-KSCHL
*,
*                                  ' '  'KONP-KBETR(02)'  L_KBETR,
*                                ' '  'BDC_CURSOR'      'KONP-KPEIN(02)'
*,
*                                  ' '  'KONP-KPEIN(02)'  L_KPEIN,
*                                  ' '  'BDC_OKCODE'      '=PDAT'.
*      ELSE.
*        PERFORM DYNPRO USING : 'X'  'SAPMV13A'        '0201',
*                              ' '  'KONP-KSCHL(02)'  IT_INFO_ITEM-KSCHL
*,
*                               ' '  'KONP-KBETR(02)'  L_KBETR,
*                               ' '  'BDC_CURSOR'      'KONP-KSCHL(02)',
*                               ' '  'BDC_OKCODE'      '=PDAT'.
*      ENDIF.
*      IF IT_INFO_ITEM-LIFNR <> ' '.
*        PERFORM DYNPRO USING : 'X'  'SAPMV13A'        '0300',
*                               ' '  'BDC_OKCODE'      '/00',
*                              ' '  'KONP-LIFNR'      IT_INFO_ITEM-LIFNR
*,
*                               ' '  'BDC_CURSOR'      'KONP-KBETR',
*                                ' '  'BDC_OKCODE'      '=BACK'.
*      ELSE.
*        PERFORM DYNPRO USING : 'X'  'SAPMV13A'        '0300',
*                                ' '  'BDC_OKCODE'      '=BACK'.
*
*      ENDIF.
*      PERFORM DYNPRO USING : 'X'  'SAPMV13A'        '0201',
*                             ' '  'BDC_CURSOR'      'KONP-KSCHL(02)',
*                             ' '  'BDC_OKCODE'      '=EINF'.
*
*    ENDLOOP.
    PERFORM DYNPRO USING : 'X'  'SAPMV13A'        '0201',
                                ' '  'BDC_CURSOR'      'KONP-KSCHL(02)',
                                 ' '  'BDC_OKCODE'      '=EINF'.

    PERFORM DYNPRO USING : 'X'  'SAPMV13A'        '0201',
                           ' '  'BDC_OKCODE'      '=KDAT'.

    CLEAR : L_KZUST.
*    MOVE : IT_INFO_ITEM-KZUST TO L_KZUST.

    PERFORM DYNPRO USING : 'X'  'SAPMV13A'        '0200',
                           ' '  'KONH-KZUST'      L_KZUST,
                           ' '  'BDC_OKCODE'      '=BACK'.

    PERFORM DYNPRO USING : 'X'  'SAPMM06E'        '0220',
                           ' '  'BDC_OKCODE'      '=BU'.

    PERFORM DYNPRO USING : 'X'  'SAPLSPO1'        '0300',
                           ' '  'BDC_OKCODE'      '=YES'.

    CALL TRANSACTION 'ME32L' USING IT_BDC
                             MODE W_MODE
                             UPDATE 'S'
                             MESSAGES INTO IT_MESS.

    APPEND LINES OF IT_MESS TO IT_MESSAGE.

    CLEAR : IT_MESS, L_MESSA.

    READ TABLE IT_MESS WITH KEY MSGTYP = 'E'.

    IF SY-SUBRC EQ 0.
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
      IT_ZTMM_IFSA_LOG-ZUSER = SY-UNAME.
      IT_ZTMM_IFSA_LOG-ZRESULT = 'E'.
      IT_ZTMM_IFSA_LOG-ZBDAT = SY-DATUM.
      IT_ZTMM_IFSA_LOG-ZTIME = SY-UZEIT.
      IT_ZTMM_IFSA_LOG-ZMSG = L_MESSA.
      APPEND IT_ZTMM_IFSA_LOG.

      MODIFY ZTMM_IFSA_LOG FROM TABLE IT_ZTMM_IFSA_LOG.
      IF SY-SUBRC EQ 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
      MESSAGE I999 WITH 'Error - SA:'
                        IT_ITAB-EBELN ', Please check ZTMM_IFSA_LOG'.
      L_FLAG = 'X'.
    ELSE.
      READ TABLE IT_MESS WITH KEY MSGTYP = 'S'.
      IF SY-SUBRC EQ 0.
*        MESSAGE I999 WITH 'Successfully updated ' IT_ITAB-EBELN.
      ELSE.

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
        IT_ZTMM_IFSA_LOG-ZUSER = SY-UNAME.
        IT_ZTMM_IFSA_LOG-ZRESULT = 'E'.
        IT_ZTMM_IFSA_LOG-ZBDAT = SY-DATUM.
        IT_ZTMM_IFSA_LOG-ZTIME = SY-UZEIT.
        IT_ZTMM_IFSA_LOG-ZMSG = L_MESSA.
        APPEND IT_ZTMM_IFSA_LOG.

        MODIFY ZTMM_IFSA_LOG FROM TABLE IT_ZTMM_IFSA_LOG.
        IF SY-SUBRC EQ 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
        MESSAGE I999 WITH 'Error - SA:'
                       IT_ITAB-EBELN ', Please check ZTMM_IFSA_LOG'.
        L_FLAG = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF L_FLAG IS INITIAL.
    MESSAGE I999 WITH 'Successfully updated '.
  ENDIF.
ENDFORM.                    " UPDATE_SA
*&---------------------------------------------------------------------*
*&      Form  double_click_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOUBLE_CLICK_RTN.

  DATA: LW_FIELD(40),
        LW_LINE TYPE I..

  CHECK NOT IT_ITAB IS INITIAL.

  GET CURSOR FIELD LW_FIELD.
  GET CURSOR FIELD LW_FIELD LINE LW_LINE.

  IF LW_FIELD = 'IT_ITAB-GRPRICE'.
    READ TABLE IT_ITAB INDEX LW_LINE.

    R_MATNR-SIGN  = 'I'.
    R_MATNR-OPTION  = 'EQ'.
    R_MATNR-LOW  = IT_ITAB-MATNR.
    APPEND R_MATNR.

    R_REFDT-SIGN  = 'I'.
    R_REFDT-OPTION  = 'BT'.
    R_REFDT-LOW  = IT_ITAB-EXCFDATE.
    R_REFDT-HIGH = IT_ITAB-EXCTDATE.
    APPEND R_REFDT.

    R_EBELN-SIGN  = 'I'.
    R_EBELN-OPTION  = 'EQ'.
    R_EBELN-LOW  = IT_ITAB-EBELN.
    APPEND R_EBELN.


    SUBMIT ZMMR_SA_REVAL_GET_INV                            "ZRFI013
              WITH P_BUKRS  = 'H201'
              WITH S_LIFNR IN R_LIFNR
              WITH S_MATNR IN R_MATNR
              WITH S_BUDAT IN R_REFDT
              WITH S_EBELN IN R_EBELN
              EXPORTING LIST TO MEMORY
              AND RETURN.
  ELSE.
  ENDIF.
ENDFORM.                    " double_click_rtn
*&---------------------------------------------------------------------*
*&      Form  DE_AGGREgate
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DE_AGGREGATE.
  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
      LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I.
  RANGES: R_EBELN FOR EKKO-EBELN,
          R_WERKS FOR EKPO-WERKS.

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  LOOP AT LT_ROWS.
    READ TABLE IT_ITAB INDEX LT_ROWS-INDEX.
    IF IT_ITAB-MATNR = ' '.
      MESSAGE E000(ZZ) WITH TEXT-M13.
    ENDIF.

    R_EBELN-SIGN = 'I'.
    R_EBELN-OPTION = 'EQ'.
    R_EBELN-LOW = IT_ITAB-EBELN.
    APPEND R_EBELN.

  ENDLOOP.
  R_WERKS-SIGN = 'I'.
  R_WERKS-OPTION = 'EQ'.
  R_WERKS-LOW = 'P001'..
  APPEND R_WERKS.

  SUBMIT Z_EKBEAUFL    "via selection-screen
         WITH S_WERKS IN R_WERKS
         WITH S_EBELN IN R_EBELN
         WITH UPDATE = 'X'
         AND RETURN.
ENDFORM.                    " DE_AGGREgate
*&---------------------------------------------------------------------*
*&      Form  check_reason_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_REASON_CODE.
  DATA: L_KZUST LIKE T686C-KZUST.
  LOOP AT IT_EXCEL.
    IF IT_EXCEL-MATNR IS INITIAL.
    ELSE.
      SELECT SINGLE KZUST INTO L_KZUST
        FROM  T686C
        WHERE KZUST = IT_EXCEL-RSN.
      IF SY-SUBRC = 0.
      ELSE.
*        CALL FUNCTION 'POPUP_TO_INFORM'
*             EXPORTING
*                  TITEL = 'Error'
*                  TXT1  = 'Reason Code not found'
*                  TXT2  = 'Please modify input file and reload again!'
*                  TXT3  = IT_EXCEL-MATNR
*                  TXT4  = IT_EXCEL-RSN.
        MESSAGE I009 WITH  'Reason Code not found'.
        W_ERROR = 'X'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_reason_code
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_SA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_SA.
  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
        LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M12.
  ENDIF.
  READ TABLE IT_ITAB INDEX LT_ROWS-INDEX.

  SET PARAMETER ID 'VRT' FIELD IT_ITAB-EBELN.
  CALL TRANSACTION 'ME33' AND SKIP FIRST SCREEN.

ENDFORM.                    " DISPLAY_SA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_INFO.
  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
         LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LIFNR LIKE EKKO-LIFNR.

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M12.
  ENDIF.
  READ TABLE IT_ITAB INDEX LT_ROWS-INDEX.

  SELECT SINGLE LIFNR INTO L_LIFNR
    FROM EKKO
    WHERE EBELN = IT_ITAB-EBELN.

  SET PARAMETER ID 'LIF' FIELD L_LIFNR.
  SET PARAMETER ID 'MAT' FIELD IT_ITAB-MATNR.
  SET PARAMETER ID 'WRK' FIELD '    '.
  CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.

ENDFORM.                    " DISPLAY_info
