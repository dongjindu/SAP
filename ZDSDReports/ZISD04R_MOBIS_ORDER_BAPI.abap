************************************************************************
* Author                 : jun ho choi
* Creation Date          : 2003-10-06
* Specifications By      :
* Development Request No : UD1K901594
* Pattern                : 3-1
* Addl documentation     :
* Description            : Mobis Order Interface
*
*
*
* Modification Log
* Date       Developer    Request ID Description
*
************************************************************************
REPORT ZISD04R_MOBIS_ORDER NO STANDARD PAGE HEADING
                           MESSAGE-ID ZMSD
                           LINE-SIZE 137.


*
TABLES : ZTSD_MOBIS_OR,
         MARA, MARC, MVKE, KNA1, KOMK, KOMP, KOMV, USR01.

TABLES : AUSP, CABN, VEPVG.


*
DATA : BEGIN OF IT_IN OCCURS 0.
       INCLUDE STRUCTURE ZTSD_MOBIS_OR.
DATA : END OF IT_IN.

DATA : BEGIN OF IT_IN_DUPL OCCURS 0.
       INCLUDE STRUCTURE ZTSD_MOBIS_OR.
DATA : END OF IT_IN_DUPL.

DATA : BEGIN OF I_KOMK OCCURS 100.
       INCLUDE STRUCTURE KOMK.
DATA : END OF I_KOMK.

DATA : BEGIN OF I_KOMP OCCURS 100.
       INCLUDE STRUCTURE KOMP.
DATA : END OF I_KOMP.

DATA : BEGIN OF E_KOMK OCCURS 100.
       INCLUDE STRUCTURE KOMK.
DATA : END OF E_KOMK.

DATA : BEGIN OF E_KOMP OCCURS 100.
       INCLUDE STRUCTURE KOMP.
DATA : END OF E_KOMP.

DATA : BEGIN OF T_KOMV OCCURS 100.
       INCLUDE STRUCTURE KOMV.
DATA : END OF T_KOMV.

* BAPI
DATA : ORDER_HEADER_IN LIKE BAPISDHD1.

DATA : SALESDOCUMENT LIKE BAPIVBELN-VBELN.

DATA : BEGIN OF RETURN OCCURS 0.
       INCLUDE STRUCTURE BAPIRET2.
DATA : END OF RETURN.

DATA : BEGIN OF ORDER_ITEMS_IN OCCURS 0.
       INCLUDE STRUCTURE BAPISDITM.
DATA : END OF ORDER_ITEMS_IN.

DATA : BEGIN OF ORDER_PARTNERS OCCURS 0.
       INCLUDE STRUCTURE BAPIPARNR.
DATA : END OF ORDER_PARTNERS.

DATA : BEGIN OF ORDER_SCHEDULES_IN OCCURS 0.
       INCLUDE STRUCTURE BAPISCHDL.
DATA : END OF ORDER_SCHEDULES_IN.
* BAPI

DATA : BEGIN OF BDC_HEADER OCCURS 0,
       ZFPOTYPE LIKE ZTSD_MOBIS_OR-ZFPOTYPE,
       ZFHEADER LIKE ZTSD_MOBIS_OR-ZFHEADER,
       W_AUART  LIKE VBAK-AUART,
       W_VKORG  LIKE VBAK-VKORG,
       W_VTWEG  LIKE VBAK-VTWEG,
       W_SPART  LIKE VBAK-SPART,
       END OF BDC_HEADER.

DATA : BEGIN OF BDC_ITEM OCCURS 0,
       ZFPOTYPE LIKE ZTSD_MOBIS_OR-ZFPOTYPE,
       ZFHEADER LIKE ZTSD_MOBIS_OR-ZFHEADER,
       W_AUART  LIKE VBAK-AUART,
       W_VKORG  LIKE VBAK-VKORG,
       W_VTWEG  LIKE VBAK-VTWEG,
       W_SPART  LIKE VBAK-SPART,

       ZDATE    LIKE ZTSD_MOBIS_OR-ZDATE,
       ZFDLVDTE LIKE ZTSD_MOBIS_OR-ZFDLVDTE,
       ZFORDNO  LIKE ZTSD_MOBIS_OR-ZFORDNO,
       W_MATNR  LIKE MARA-MATNR,
       W_MATNR_ORG LIKE MARA-MATNR,
       ZWREQQTY(7),
       ZWHMCNO  LIKE ZTSD_MOBIS_OR-ZWHMCNO,
       W_ERROR(14),
       ZWFCD1(7) TYPE N,
       ZWFCD2(7) TYPE N,
       ZWFCD3(7) TYPE N,
       END OF BDC_ITEM.

DATA : BEGIN OF BDC_LIST OCCURS 0,
       GUBUN_S(1),
       ZFHEADER LIKE ZTSD_MOBIS_OR-ZFHEADER,
       ZFORDNO LIKE ZTSD_MOBIS_OR-ZFORDNO,
       VBELN LIKE VBAK-VBELN,
       POSNR(6) TYPE N,
       ZWREQQTY(7),

       MESSAGE(75),
       END OF BDC_LIST.

DATA : X_HEAD TYPE THEAD OCCURS 0 WITH HEADER LINE.
DATA : X_TEXT TYPE TLINE OCCURS 0 WITH HEADER LINE.

DATA : W_CNT TYPE I,
       W_CNT_S TYPE I,
       W_CNT_E TYPE I,
       W_CNT_P TYPE I,
       W_INDEX LIKE SY-TABIX,
       W_POSNR(6) TYPE N,
       W_LAST_DATE1 LIKE SY-DATUM,
       W_LAST_DATE2 LIKE SY-DATUM,
       W_DATE1 LIKE SY-DATUM,
       W_ERR(1),
       W_FCD(7) TYPE N,
       W_NUMC_13(13) TYPE N.


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_MODE(1) DEFAULT 'N'.
SELECTION-SCREEN END OF BLOCK B1.


*
TOP-OF-PAGE.
  PERFORM TOP_OF_PAGE.


*
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM TOP_OF_PAGE.


*
START-OF-SELECTION.
  SET PF-STATUS 'ISD04R'.
  PERFORM GET_DATA.
  PERFORM BDC_PROCESS.
  PERFORM DISPLAY_RESULT.


*
END-OF-SELECTION.


*
AT USER-COMMAND.
  PERFORM USER_COMMAND.


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
  REFRESH : IT_IN.
  CLEAR   : IT_IN.

  SELECT *
         INTO TABLE IT_IN
         FROM ZTSD_MOBIS_OR
        WHERE VBELN EQ SPACE.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.
  DESCRIBE TABLE IT_IN LINES W_CNT.
  IF W_CNT = 0.
    SKIP 5.
    WRITE:/ 'No Entry'.
    STOP.
  ENDIF.

  IT_IN_DUPL[] = IT_IN[].

  W_CNT_P = W_CNT.
  LOOP AT IT_IN.
    PERFORM SAPGUI_PROGRESS_INDICATOR USING 1.

    CLEAR BDC_ITEM-W_ERROR.

    PERFORM CHECK_AUART.

*   1 NF, 3 NA, 4 NL
    PERFORM CHECK_ITEM USING IT_IN-ZWPTNO.
    IF SY-SUBRC = 0.
      PERFORM CHECK_MIP USING IT_IN-ZWPTNO
                              IT_IN-ZVKORG
                              IT_IN-ZVTWEG
                              IT_IN-ZSPART.
      IF SY-SUBRC = 0.
        PERFORM CHECK_VALID USING IT_IN-ZWPTNO.
        IF SY-SUBRC <> 0.
          PERFORM CHECK_DUMMY.
          CONCATENATE BDC_ITEM-W_ERROR 'NL' INTO BDC_ITEM-W_ERROR.
        ENDIF.
      ELSE.
        PERFORM CHECK_DUMMY.
        CONCATENATE BDC_ITEM-W_ERROR 'NA' INTO BDC_ITEM-W_ERROR.
      ENDIF.
    ELSE.
      PERFORM CHECK_DUMMY.
      CONCATENATE BDC_ITEM-W_ERROR 'NF' INTO BDC_ITEM-W_ERROR.
    ENDIF.
*   2 QN
    IF IT_IN-ZWREQQTY = 0.
      CONCATENATE BDC_ITEM-W_ERROR 'QN' INTO BDC_ITEM-W_ERROR.
    ENDIF.
*   5 NE "HOLDING
*   6 CO
    CLEAR W_CNT.
    LOOP AT IT_IN_DUPL WHERE ZFHEADER EQ IT_IN-ZFHEADER
                       AND   ZFORDNO  EQ IT_IN-ZFORDNO
                       AND   ZFPOTYPE EQ IT_IN-ZFPOTYPE
                       AND   ZWPTNO   EQ IT_IN-ZWPTNO.
      W_CNT = W_CNT + 1.
    ENDLOOP.
    IF W_CNT <> 1.
      CONCATENATE BDC_ITEM-W_ERROR 'CO' INTO BDC_ITEM-W_ERROR.
    ENDIF.
*   9 UP
    REFRESH : I_KOMK, I_KOMP, E_KOMK, E_KOMP, T_KOMV.
    CLEAR   : I_KOMK, I_KOMP, E_KOMK, E_KOMP, T_KOMV.

    PERFORM FILL_I_KOMK.
    PERFORM FILL_I_KOMP.

    CALL FUNCTION 'PRICING'
         EXPORTING
              CALCULATION_TYPE = 'C'
              COMM_HEAD_I      = I_KOMK
              COMM_ITEM_I      = I_KOMP
         IMPORTING
              COMM_HEAD_E      = E_KOMK
              COMM_ITEM_E      = E_KOMP
         TABLES
              TKOMV            = T_KOMV.
    DESCRIBE TABLE T_KOMV LINES W_CNT.
    IF SY-SUBRC <> 0 OR W_CNT = 0.
      CONCATENATE BDC_ITEM-W_ERROR 'UP' INTO BDC_ITEM-W_ERROR.
    ELSE.
      READ TABLE T_KOMV WITH KEY KSCHL = 'ZP00'.
      IF SY-SUBRC <> 0.
        CONCATENATE BDC_ITEM-W_ERROR 'UP' INTO BDC_ITEM-W_ERROR.
      ENDIF.
    ENDIF.
*   10 SD
    SELECT SINGLE *
           FROM MVKE
          WHERE MATNR EQ BDC_ITEM-W_MATNR
          AND   VKORG EQ BDC_HEADER-W_VKORG
          AND   VTWEG EQ BDC_HEADER-W_VTWEG.
    IF SY-SUBRC <> 0.
      CONCATENATE BDC_ITEM-W_ERROR 'SD' INTO BDC_ITEM-W_ERROR.
    ENDIF.

    IF BDC_ITEM-W_ERROR IS INITIAL.
*   7 CQ
      IF BDC_HEADER-W_AUART = 'ZPSO'.
        SELECT SINGLE *
               FROM MVKE
              WHERE MATNR EQ BDC_ITEM-W_MATNR
              AND   VKORG EQ BDC_HEADER-W_VKORG
              AND   VTWEG EQ BDC_HEADER-W_VTWEG.
        IF SY-SUBRC <> 0 OR IT_IN-ZWREQQTY < MVKE-AUMNG.
          CONCATENATE BDC_ITEM-W_ERROR 'CQ' INTO BDC_ITEM-W_ERROR.
        ENDIF.
      ENDIF.
*   8 SI
    ENDIF.

    MOVE-CORRESPONDING IT_IN TO BDC_HEADER.
    COLLECT BDC_HEADER.

    MOVE-CORRESPONDING IT_IN TO BDC_ITEM.
    MOVE-CORRESPONDING BDC_HEADER TO BDC_ITEM.
    MOVE-CORRESPONDING BDC_ITEM TO BDC_ITEM.
    APPEND BDC_ITEM. CLEAR BDC_ITEM.
  ENDLOOP.

* BDC PROCESS
* MD61
*  W_CNT = 0.
*  LOOP AT BDC_ITEM WHERE W_MATNR NE 'DUMMY1'.
*    W_CNT = W_CNT + 1.
*  ENDLOOP.
*  W_CNT_P = W_CNT.
*  LOOP AT BDC_ITEM WHERE W_MATNR NE 'DUMMY1'.
*    PERFORM SAPGUI_PROGRESS_INDICATOR USING 2.
*
*    W_FCD = BDC_ITEM-ZWFCD1 + BDC_ITEM-ZWFCD2 + BDC_ITEM-ZWFCD3.
*    CHECK W_FCD GT 0.
*
*    PERFORM BDC_MD61.
*    IF W_CNT = 9.
*      W_ERR = 'X'.
*      EXIT.
*    ELSE.
*      W_ERR = ' '.
*    ENDIF.
*  ENDLOOP.
*  IF W_ERR = 'X'.
*    SY-SUBRC = 1.
*    EXIT.
*  ENDIF.
* VA01
  DESCRIBE TABLE BDC_HEADER LINES W_CNT.
  W_CNT_P = W_CNT.
  LOOP AT BDC_HEADER.
    PERFORM SAPGUI_PROGRESS_INDICATOR USING 3.

    CLEAR W_ERR.
    BDC_LIST-ZFHEADER = BDC_HEADER-ZFHEADER.
    PERFORM BDC_PROCESS_H.

    W_POSNR = 10.
    LOOP AT BDC_ITEM WHERE ZFPOTYPE EQ BDC_HEADER-ZFPOTYPE
                     AND   ZFHEADER EQ BDC_HEADER-ZFHEADER
                     AND   W_AUART  EQ BDC_HEADER-W_AUART
                     AND   W_SPART  EQ BDC_HEADER-W_SPART.
      BDC_LIST-ZFORDNO = BDC_ITEM-ZFORDNO.
      BDC_LIST-POSNR = W_POSNR.
      BDC_LIST-ZWREQQTY = BDC_ITEM-ZWREQQTY.
      APPEND BDC_LIST.

      PERFORM BDC_PROCESS_I.
**      IF BDC_ITEM-W_ERROR CA 'QN' OR "2
**         BDC_ITEM-W_ERROR CA 'UP'.   "9
**         W_ERR = 'X'.
**      ENDIF.
    ENDLOOP.

**    IF W_ERR = 'X'.
**      PERFORM BDC_PROCESS_S_ERR.
**    ELSE.
      PERFORM BDC_PROCESS_S.
**    ENDIF.
    PERFORM BDC_RESULT.
  ENDLOOP.
  " log
ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SAPGUI_PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
FORM SAPGUI_PROGRESS_INDICATOR USING GUBUN.
DATA : W_PERC TYPE P DECIMALS 2,
       W_TEXT(50).

  W_PERC = SY-TABIX / W_CNT_P * 100.
  WRITE W_PERC TO W_TEXT+0(7).
  CASE GUBUN.
  WHEN '1'.
    CONCATENATE W_TEXT 'Checking validation'
                INTO W_TEXT SEPARATED BY SPACE.
  WHEN '2'.
    CONCATENATE W_TEXT 'Creating planned indep. requirements'
                INTO W_TEXT SEPARATED BY SPACE.
  WHEN '3'.
    CONCATENATE W_TEXT 'Creating sales order'
                INTO W_TEXT SEPARATED BY SPACE.
  ENDCASE.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      PERCENTAGE       = W_PERC
      TEXT             = W_TEXT.
ENDFORM.                    " SAPGUI_PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS_REST
*&---------------------------------------------------------------------*
FORM BDC_PROCESS_REST.
* BDC PROCESS
* MD61
  LOOP AT BDC_ITEM WHERE ZFHEADER EQ BDC_LIST-ZFHEADER
                   AND   W_MATNR NE 'DUMMY1'.
    W_FCD = BDC_ITEM-ZWFCD1 + BDC_ITEM-ZWFCD2 + BDC_ITEM-ZWFCD3.
    CHECK W_FCD GT 0.

    PERFORM BDC_MD61.
    IF W_CNT <> 0.
      W_ERR = 'X'.
      EXIT.
    ELSE.
      W_ERR = ' '.
    ENDIF.
  ENDLOOP.
  IF W_ERR = 'X'.
    BDC_LIST-GUBUN_S = 'E'.
    BDC_LIST-MESSAGE = 'ERROR : Planned independent requirements(MD61)'.
    MODIFY BDC_LIST TRANSPORTING GUBUN_S MESSAGE
                    WHERE ZFORDNO = BDC_ITEM-ZFORDNO.
    EXIT.
  ENDIF.
* VA01
  LOOP AT BDC_HEADER WHERE ZFHEADER EQ BDC_LIST-ZFHEADER.
    CLEAR W_ERR.
    PERFORM BDC_PROCESS_H.

    W_POSNR = 10.
    LOOP AT BDC_ITEM WHERE ZFPOTYPE EQ BDC_HEADER-ZFPOTYPE
                     AND   ZFHEADER EQ BDC_HEADER-ZFHEADER
                     AND   W_AUART  EQ BDC_HEADER-W_AUART
                     AND   W_SPART  EQ BDC_HEADER-W_SPART.
      PERFORM BDC_PROCESS_I.
**      IF BDC_ITEM-W_ERROR CA 'QN' OR "2
**         BDC_ITEM-W_ERROR CA 'UP'.   "9
**         W_ERR = 'X'.
**      ENDIF.
    ENDLOOP.

**    IF W_ERR = 'X'.
**      PERFORM BDC_PROCESS_S_ERR.
**    ELSE.
      PERFORM BDC_PROCESS_S.
**    ENDIF.
    PERFORM BDC_RESULT.
  ENDLOOP.
  " log
ENDFORM.                    " BDC_PROCESS_REST
*&---------------------------------------------------------------------*
*&      Form  BDC_MD61
*&---------------------------------------------------------------------*
FORM BDC_MD61.
  TABLES : PBIM.

  DATA : BEGIN OF IT_BAPISSHDIN OCCURS 0.
         INCLUDE STRUCTURE BAPISSHDIN.
  DATA : END OF IT_BAPISSHDIN.

  DATA : BEGIN OF IT_BAPISITEMR OCCURS 0.
         INCLUDE STRUCTURE BAPISITEMR.
  DATA : END OF IT_BAPISITEMR.

  DATA : BEGIN OF IT_RETURN OCCURS 0.
         INCLUDE STRUCTURE BAPIRETURN1.
  DATA : END OF IT_RETURN.

  DATA : W_WERKS LIKE MARC-WERKS,
         W_DATES LIKE SY-DATUM.

  SELECT SINGLE *
         FROM MARC
        WHERE MATNR EQ BDC_ITEM-W_MATNR
        AND   WERKS EQ 'P001'.
  IF SY-SUBRC NE 0.
    W_WERKS = 'E001'.
  ELSE.
    IF MARC-BESKZ EQ 'E' OR MARC-BESKZ EQ 'X'. "MIP
      W_WERKS = 'P001'.
    ELSE.
      W_WERKS = 'E001'.
    ENDIF.
  ENDIF.

  REFRESH IT_BAPISSHDIN. CLEAR IT_BAPISSHDIN.
  REFRESH IT_BAPISITEMR. CLEAR IT_BAPISITEMR.

  CALL FUNCTION 'MONTH_PLUS_DETERMINE'
    EXPORTING
      MONTHS        = 2
      OLDDATE       = BDC_ITEM-ZDATE
    IMPORTING
      NEWDATE       = W_DATES.
  IT_BAPISSHDIN-DATE_TYPE = 3. "MONTH
  IT_BAPISSHDIN-REQ_DATE  = W_DATES.
  IT_BAPISSHDIN-REQ_QTY   = BDC_ITEM-ZWFCD1.
  APPEND IT_BAPISSHDIN. CLEAR IT_BAPISSHDIN.

  CALL FUNCTION 'MONTH_PLUS_DETERMINE'
    EXPORTING
      MONTHS        = 1
      OLDDATE       = W_DATES
    IMPORTING
      NEWDATE       = W_DATES.
  IT_BAPISSHDIN-DATE_TYPE = 3. "MONTH
  IT_BAPISSHDIN-REQ_DATE  = W_DATES.
  IT_BAPISSHDIN-REQ_QTY   = BDC_ITEM-ZWFCD2.
  APPEND IT_BAPISSHDIN. CLEAR IT_BAPISSHDIN.

  CALL FUNCTION 'MONTH_PLUS_DETERMINE'
    EXPORTING
      MONTHS        = 1
      OLDDATE       = W_DATES
    IMPORTING
      NEWDATE       = W_DATES.
  IT_BAPISSHDIN-DATE_TYPE = 3. "MONTH
  IT_BAPISSHDIN-REQ_DATE  = W_DATES.
  IT_BAPISSHDIN-REQ_QTY   = BDC_ITEM-ZWFCD3.
  APPEND IT_BAPISSHDIN. CLEAR IT_BAPISSHDIN.

  SELECT SINGLE *
         FROM PBIM
        WHERE MATNR EQ BDC_ITEM-W_MATNR
        AND   WERKS EQ W_WERKS
        AND   BEDAE EQ 'LSF'
        AND   VERSB EQ '00'.
  IF SY-SUBRC = 0.
  CALL FUNCTION 'BAPI_REQUIREMENTS_CHANGE'
    EXPORTING
      MATERIAL                       = BDC_ITEM-W_MATNR
      PLANT                          = W_WERKS
      REQUIREMENTSTYPE               = 'LSF'
      VERSION                        = '00'
      REQMTSPLANNUMBER               = ''
      VERS_ACTIV                     = 'X'
*     REQUIREMENT_PARAM              =
*     MATERIAL_EVG                   =
*   IMPORTING
*     REQUIREMENT_ITEM_OUT           =
    TABLES
      REQUIREMENTS_SCHEDULE_IN       = IT_BAPISSHDIN
*     REQUIREMENTS_CHAR_IN           =
      RETURN                         = IT_RETURN.
  ELSE.
  IT_BAPISITEMR-MATERIAL   = BDC_ITEM-W_MATNR.
  IT_BAPISITEMR-PLANT      = W_WERKS.
  IT_BAPISITEMR-REQU_TYPE  = 'LSF'.
  IT_BAPISITEMR-VERSION    = '00'.
  IT_BAPISITEMR-VERS_ACTIV = 'X'.
  IT_BAPISITEMR-REQ_NUMBER = ''.

  CALL FUNCTION 'BAPI_REQUIREMENTS_CREATE'
    EXPORTING
      REQUIREMENTS_ITEM              = IT_BAPISITEMR
*     REQUIREMENT_PARAM              =
*   IMPORTING
*     MATERIAL                       =
*     PLANT                          =
*     REQUIREMENTSTYPE               =
*     VERSION                        =
*     REQMTSPLANNUMBER               =
*     MATERIAL_EVG                   =
    TABLES
      REQUIREMENTS_SCHEDULE_IN       = IT_BAPISSHDIN
*     REQUIREMENTS_CHAR_IN           =
      RETURN                         = IT_RETURN.
  ENDIF.

  IF IT_RETURN-TYPE EQ 'E'. "[] IS INITIAL.
    W_CNT = 9.
  ELSE.
    W_CNT = 0.
  ENDIF.
ENDFORM.                    " BDC_MD61
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUART
*&---------------------------------------------------------------------*
FORM CHECK_AUART.
* ORDER TYPE
  CASE IT_IN-ZFPOTYPE.
    WHEN 'R'.
      BDC_HEADER-W_AUART = 'ZPSO'. "REGULAR
    WHEN 'U'.
      BDC_HEADER-W_AUART = 'ZPRO'. "RUSH
    WHEN 'V'.
      BDC_HEADER-W_AUART = 'ZPEO'. "VOR
    WHEN 'Z'.
      BDC_HEADER-W_AUART = 'ZOSO'. "
  ENDCASE.
ENDFORM.                    " CHECK_AUART
*&---------------------------------------------------------------------*
*&      Form  CHECK_DUMMY
*&---------------------------------------------------------------------*
FORM CHECK_DUMMY.
  BDC_ITEM-W_MATNR   = 'DUMMY1'.
  BDC_ITEM-W_MATNR_ORG = IT_IN-ZWPTNO.

  IF BDC_HEADER-W_AUART = 'ZOSO'.
    BDC_HEADER-W_VKORG = 'D100'.
    BDC_HEADER-W_VTWEG = '30'.
    BDC_HEADER-W_SPART = '40'.
  ELSE.
    BDC_HEADER-W_VKORG = 'D100'.
    BDC_HEADER-W_VTWEG = '30'.
    BDC_HEADER-W_SPART = '30'.
  ENDIF.
ENDFORM.                    " CHECK_DUMMY
*&---------------------------------------------------------------------*
*&      Form  CHECK_ITEM
*&---------------------------------------------------------------------*
FORM CHECK_ITEM USING PTNO.
  SELECT SINGLE *
         FROM MARA
        WHERE MATNR EQ PTNO
        AND   ( MTART EQ 'HALB' OR MTART EQ 'ROH1' ).
ENDFORM.                    " CHECK_ITEM
*&---------------------------------------------------------------------*
*&      Form  CHECK_MIP
*&---------------------------------------------------------------------*
FORM CHECK_MIP USING PTNO VKORG VTWEG SPART.
* CHECK MARC
  SELECT SINGLE *
         FROM MARC
        WHERE MATNR EQ PTNO
        AND   ( BESKZ EQ 'E' OR BESKZ EQ 'X' ). "MIP

* DIV
  BDC_HEADER-W_VKORG = VKORG.
  BDC_HEADER-W_VTWEG = VTWEG.
  BDC_HEADER-W_SPART = SPART.
ENDFORM.                    " CHECK_MIP
*&---------------------------------------------------------------------*
*&      Form  CHECK_VALID
*&---------------------------------------------------------------------*
FORM CHECK_VALID USING PTNO.
* CHECK PP-BOM(TBD)


* MATNR
  BDC_ITEM-W_MATNR = PTNO.
ENDFORM.                    " CHECK_VALID
*&---------------------------------------------------------------------*
*&      Form  FILL_I_KOMK
*&---------------------------------------------------------------------*
FORM FILL_I_KOMK.
  I_KOMK-VKORG = BDC_HEADER-W_VKORG.
  I_KOMK-VTWEG = BDC_HEADER-W_VTWEG.
  I_KOMK-SPART = BDC_HEADER-W_SPART.
  I_KOMK-KUNNR = 'MOBIS'. """"""TEMP(KNA1)
  I_KOMK-KAPPL = 'V'.
  I_KOMK-KALSM = 'ZPAJUS'.
  I_KOMK-PRSDT = SY-DATUM.
  I_KOMK-AUART = BDC_HEADER-W_AUART.
ENDFORM.                    " FILL_I_KOMK
*&---------------------------------------------------------------------*
*&      Form  FILL_I_KOMP
*&---------------------------------------------------------------------*
FORM FILL_I_KOMP.
  I_KOMP-KPOSN = '000010'.
  I_KOMP-MATNR = BDC_ITEM-W_MATNR.
  I_KOMP-PMATN = BDC_ITEM-W_MATNR.
  I_KOMP-PRSFD = 'X'.
ENDFORM.                    " FILL_I_KOMP
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS_H
*&---------------------------------------------------------------------*
FORM BDC_PROCESS_H.
  REFRESH ORDER_PARTNERS. CLEAR ORDER_PARTNERS.
  REFRESH ORDER_ITEMS_IN. CLEAR ORDER_ITEMS_IN.

  CALL FUNCTION 'MONTH_PLUS_DETERMINE'
    EXPORTING
      MONTHS        = 1
      OLDDATE       = SY-DATUM
    IMPORTING
      NEWDATE       = W_LAST_DATE1.

  CALL FUNCTION 'MM_LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = W_LAST_DATE1
       IMPORTING
            LAST_DAY_OF_MONTH = W_LAST_DATE1
       EXCEPTIONS
            DAY_IN_NO_DATE    = 1
            OTHERS            = 2.

  SELECT SINGLE *
         FROM USR01
        WHERE BNAME = SY-UNAME.
  CASE USR01-DATFM.
    WHEN '1'. "DD.MM.YYYY
      W_LAST_DATE2+4(4) = W_LAST_DATE1+0(4).
      W_LAST_DATE2+2(2) = W_LAST_DATE1+4(2).
      W_LAST_DATE2+0(2) = W_LAST_DATE1+6(2).
    WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
      W_LAST_DATE2+4(4) = W_LAST_DATE1+0(4).
      W_LAST_DATE2+0(2) = W_LAST_DATE1+4(2).
      W_LAST_DATE2+2(2) = W_LAST_DATE1+6(2).
    WHEN OTHERS.
      W_LAST_DATE2+0(4) = W_LAST_DATE1+0(4).
      W_LAST_DATE2+4(2) = W_LAST_DATE1+4(2).
      W_LAST_DATE2+6(2) = W_LAST_DATE1+6(2).
  ENDCASE.

  ORDER_HEADER_IN-DOC_TYPE   = BDC_HEADER-W_AUART.
  ORDER_HEADER_IN-SALES_ORG  = BDC_HEADER-W_VKORG.
  ORDER_HEADER_IN-DISTR_CHAN = BDC_HEADER-W_VTWEG.
  ORDER_HEADER_IN-DIVISION   = BDC_HEADER-W_SPART.
  ORDER_HEADER_IN-PURCH_NO_C = BDC_HEADER-ZFHEADER.
  ORDER_HEADER_IN-REQ_DATE_H = W_LAST_DATE2.

  ORDER_PARTNERS-PARTN_ROLE = 'AG'.
  ORDER_PARTNERS-PARTN_NUMB = 'MOBIS'. """""TEMP(KNA1)
  APPEND ORDER_PARTNERS. CLEAR ORDER_PARTNERS.
ENDFORM.                    " BDC_PROCESS_H
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS_I
*&---------------------------------------------------------------------*
FORM BDC_PROCESS_I.
  SELECT SINGLE *
         FROM USR01
        WHERE BNAME = SY-UNAME.
  CASE USR01-DATFM.
    WHEN '1'. "DD.MM.YYYY
      W_DATE1+2(2) = BDC_ITEM-ZFDLVDTE+0(2).
      W_DATE1+0(2) = BDC_ITEM-ZFDLVDTE+2(2).
      W_DATE1+4(4) = BDC_ITEM-ZFDLVDTE+4(4).
    WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
      W_DATE1+0(2) = BDC_ITEM-ZFDLVDTE+0(2).
      W_DATE1+2(2) = BDC_ITEM-ZFDLVDTE+2(2).
      W_DATE1+4(4) = BDC_ITEM-ZFDLVDTE+4(4).
    WHEN OTHERS.
      W_DATE1+4(2) = BDC_ITEM-ZFDLVDTE+0(2).
      W_DATE1+6(2) = BDC_ITEM-ZFDLVDTE+2(2).
      W_DATE1+0(4) = BDC_ITEM-ZFDLVDTE+4(4).
  ENDCASE.

  ORDER_ITEMS_IN-MATERIAL   = BDC_ITEM-W_MATNR.
*  ORDER_ITEMS_IN-CUST_MAT   = BDC_ITEM-ZWHMCNO.
*  ORDER_ITEMS_IN-REQ_DATE   = W_DATE1.
  ORDER_ITEMS_IN-ITM_NUMBER = W_POSNR.
*  ORDER_ITEMS_IN-PO_ITM_NO  = BDC_ITEM-ZFORDNO.
  APPEND ORDER_ITEMS_IN. CLEAR ORDER_ITEMS_IN.

  W_NUMC_13 = BDC_ITEM-ZWREQQTY.
  W_NUMC_13 = W_NUMC_13 * 1000.
  ORDER_SCHEDULES_IN-REQ_QTY    = W_NUMC_13.
  ORDER_SCHEDULES_IN-REQ_DATE   = W_DATE1.
  ORDER_SCHEDULES_IN-ITM_NUMBER = W_POSNR.
  APPEND ORDER_SCHEDULES_IN. CLEAR ORDER_SCHEDULES_IN.

  W_POSNR = W_POSNR + 10.
ENDFORM.                    " BDC_PROCESS_I
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS_S
*&---------------------------------------------------------------------*
FORM BDC_PROCESS_S.
CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
  EXPORTING
*   SALESDOCUMENTIN               =
    ORDER_HEADER_IN               = ORDER_HEADER_IN
*   ORDER_HEADER_INX              =
*   SENDER                        =
*   BINARY_RELATIONSHIPTYPE       =
*   INT_NUMBER_ASSIGNMENT         =
*   BEHAVE_WHEN_ERROR             =
*   LOGIC_SWITCH                  =
*   TESTRUN                       =
*   CONVERT                       = ' '
  IMPORTING
    SALESDOCUMENT                 = SALESDOCUMENT
  TABLES
    RETURN                        = RETURN
    ORDER_ITEMS_IN                = ORDER_ITEMS_IN
*   ORDER_ITEMS_INX               =
    ORDER_PARTNERS                = ORDER_PARTNERS
    ORDER_SCHEDULES_IN            = ORDER_SCHEDULES_IN
*   ORDER_SCHEDULES_INX           =
*   ORDER_CONDITIONS_IN           =
*   ORDER_CFGS_REF                =
*   ORDER_CFGS_INST               =
*   ORDER_CFGS_PART_OF            =
*   ORDER_CFGS_VALUE              =
*   ORDER_CFGS_BLOB               =
*   ORDER_CFGS_VK                 =
*   ORDER_CFGS_REFINST            =
*   ORDER_CCARD                   =
*   ORDER_TEXT                    =
*   ORDER_KEYS                    =
*   EXTENSIONIN                   =
*   PARTNERADDRESSES              =
          .
ENDFORM.                    " BDC_PROCESS_S
*&---------------------------------------------------------------------*
*&      Form  BDC_RESULT.
*&---------------------------------------------------------------------*
FORM BDC_RESULT.
  IF SALESDOCUMENT EQ SPACE.
    READ TABLE RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC <> 0.
      READ TABLE RETURN INDEX 1.
    ENDIF.
    BDC_LIST-MESSAGE = RETURN-MESSAGE.
    BDC_LIST-GUBUN_S = 'E'.
  ELSE.
    W_POSNR = 10.
    LOOP AT BDC_ITEM WHERE ZFPOTYPE EQ BDC_HEADER-ZFPOTYPE
                     AND   ZFHEADER EQ BDC_HEADER-ZFHEADER
                     AND   W_AUART  EQ BDC_HEADER-W_AUART
                     AND   W_SPART  EQ BDC_HEADER-W_SPART.
      PERFORM SAVE_TEXT.

      W_POSNR = W_POSNR + 10.
    ENDLOOP.

    BDC_LIST-GUBUN_S = 'S'.
    BDC_LIST-VBELN = SALESDOCUMENT.
    UPDATE ZTSD_MOBIS_OR SET : VBELN = SALESDOCUMENT
                         WHERE ZFPOTYPE = BDC_HEADER-ZFPOTYPE
                         AND   ZFHEADER = BDC_HEADER-ZFHEADER.
  ENDIF.

  MODIFY BDC_LIST TRANSPORTING GUBUN_S VBELN MESSAGE
                  WHERE ZFHEADER = BDC_HEADER-ZFHEADER.
ENDFORM.                    " BDC_RESULT
*&---------------------------------------------------------------------*
*&      Form  SAVE_TEXT
*&---------------------------------------------------------------------*
FORM SAVE_TEXT.
  DATA : POS(2) TYPE N.

  CHECK NOT BDC_ITEM-W_ERROR IS INITIAL.

  REFRESH : X_HEAD, X_TEXT.
  CLEAR   : X_HEAD, X_TEXT.

  X_HEAD-TDOBJECT = 'VBBP'.
  CONCATENATE SALESDOCUMENT W_POSNR INTO X_HEAD-TDNAME.
  X_HEAD-TDID     = '0002'.
  X_HEAD-TDSPRAS  = 'E'.
  APPEND X_HEAD.

  X_TEXT-TDFORMAT = '*'.
  X_TEXT-TDLINE = 'Origin. Material :'.
  APPEND X_TEXT. CLEAR X_TEXT.

  X_TEXT-TDLINE = BDC_ITEM-W_MATNR_ORG.
  APPEND X_TEXT. CLEAR X_TEXT.

  X_TEXT-TDFORMAT = '*'.
  APPEND X_TEXT. CLEAR X_TEXT.

  X_TEXT-TDFORMAT = '*'.
  X_TEXT-TDLINE = 'Error List'.
  APPEND X_TEXT. CLEAR X_TEXT.

  POS = 0.
  DO.
    IF BDC_ITEM-W_ERROR+POS(2) IS INITIAL.
      EXIT.
    ENDIF.

    CASE BDC_ITEM-W_ERROR+POS(2).
    WHEN 'NF'.
     X_TEXT-TDFORMAT = '*'.
     X_TEXT-TDLINE = 'NF : Not found order item'.
     APPEND X_TEXT. CLEAR X_TEXT.
    WHEN 'QN'.
     X_TEXT-TDFORMAT = '*'.
     X_TEXT-TDLINE = 'QN : Ordered Qty. = 0'.
     APPEND X_TEXT. CLEAR X_TEXT.
    WHEN 'NA'.
     X_TEXT-TDFORMAT = '*'.
     X_TEXT-TDLINE = 'NA : Not applied to vehicle nor sales item(LP)'.
     APPEND X_TEXT. CLEAR X_TEXT.
    WHEN 'NL'.
     X_TEXT-TDFORMAT = '*'.
     X_TEXT-TDLINE = 'NL : No longer service'.
     APPEND X_TEXT. CLEAR X_TEXT.
    WHEN 'NE'.
     X_TEXT-TDFORMAT = '*'.
     X_TEXT-TDLINE = 'NE :'.
     APPEND X_TEXT. CLEAR X_TEXT.
    WHEN 'CO'.
     X_TEXT-TDFORMAT = '*'.
     X_TEXT-TDLINE = 'CO : Duplicate order no(PO no)'.
     APPEND X_TEXT. CLEAR X_TEXT.
    WHEN 'CQ'.
     X_TEXT-TDFORMAT = '*'.
     X_TEXT-TDLINE = 'CQ : The minimum confirm qty by packing material'.
     APPEND X_TEXT. CLEAR X_TEXT.
    WHEN 'SI'.
     X_TEXT-TDFORMAT = '*'.
     X_TEXT-TDLINE = 'SI : Order confirmed supersession code'.
     APPEND X_TEXT. CLEAR X_TEXT.
    WHEN 'UP'.
     X_TEXT-TDFORMAT = '*'.
     X_TEXT-TDLINE = 'UP : Price not found'.
     APPEND X_TEXT. CLEAR X_TEXT.
    WHEN 'SD'.
     X_TEXT-TDFORMAT = '*'.
     X_TEXT-TDLINE = 'SD : Not define sales view'.
     APPEND X_TEXT. CLEAR X_TEXT.
    ENDCASE.
    POS = POS + 2.
  ENDDO.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      HEADER                = X_HEAD
      SAVEMODE_DIRECT       = 'X'
    TABLES
      LINES                 = X_TEXT
    EXCEPTIONS
      ID                    = 1
      LANGUAGE              = 2
      NAME                  = 3
      OBJECT                = 4
      OTHERS                = 5.
ENDFORM.                    " SAVE_TEXT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULT
*&---------------------------------------------------------------------*
FORM DISPLAY_RESULT.
  WRITE:/ ''.
  LOOP AT BDC_LIST.
    WRITE:/ SY-VLINE.
    CASE BDC_LIST-GUBUN_S.
      WHEN 'S'.
        FORMAT COLOR COL_POSITIVE.
        WRITE: (02) BDC_LIST-GUBUN_S.
        FORMAT COLOR COL_POSITIVE OFF.
      WHEN 'E'.
        FORMAT COLOR COL_NEGATIVE.
        WRITE: (02) BDC_LIST-GUBUN_S.
        FORMAT COLOR COL_NEGATIVE OFF.
      WHEN OTHERS.
        WRITE: (02) ''.
    ENDCASE.

    WRITE:  SY-VLINE, (10) BDC_LIST-ZFHEADER,
            SY-VLINE, (04) BDC_LIST-ZFORDNO,
            SY-VLINE, (10) BDC_LIST-VBELN,
            SY-VLINE, (06) BDC_LIST-POSNR NO-ZERO,
            SY-VLINE, (07) BDC_LIST-ZWREQQTY RIGHT-JUSTIFIED,
            SY-VLINE, (75) BDC_LIST-MESSAGE,
            SY-VLINE.

    W_INDEX = SY-TABIX.
    HIDE : W_INDEX.

    WRITE:/(136) SY-ULINE.
  ENDLOOP.
ENDFORM.                    " DISPLAY_RESULT
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  DESCRIBE TABLE BDC_LIST LINES W_CNT.
  WRITE:/ 'Total   records :', W_CNT.

  W_CNT_S = 0. W_CNT_E = 0.
  LOOP AT BDC_LIST.
    IF BDC_LIST-GUBUN_S = 'S'.
      W_CNT_S = W_CNT_S + 1.
    ENDIF.
    IF BDC_LIST-GUBUN_S <> 'S'.
      W_CNT_E = W_CNT_E + 1.
    ENDIF.
  ENDLOOP.
  WRITE:/ 'Success records :', W_CNT_S.
  WRITE:/ 'Error   records :', W_CNT_E.

  FORMAT COLOR COL_HEADING.
  WRITE:/(136) SY-ULINE.
  WRITE:/ SY-VLINE, (02) 'SO',
          SY-VLINE, (10) 'Header P.O',
          SY-VLINE, (04) 'Item',
          SY-VLINE, (10) 'Sales Or.',
          SY-VLINE, (06) 'Line',
          SY-VLINE, (07) 'Qty.',
          SY-VLINE, (75) 'Message',
          SY-VLINE.
  WRITE:/(136) SY-ULINE.
  FORMAT COLOR COL_HEADING OFF.
ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM USER_COMMAND.
  DATA : OK_CODE(4).
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'BACK'.
      SET SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'REST'.
      PERFORM RESTARTING.
      SY-LSIND = SY-LSIND - 1.
      PERFORM DISPLAY_RESULT.
  ENDCASE.
ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  RESTARTING
*&---------------------------------------------------------------------*
FORM RESTARTING.
  IF SY-LISEL+3(1) EQ ' '.
    READ TABLE BDC_LIST INDEX W_INDEX.
    IF SY-SUBRC = 0.
      IF BDC_LIST-GUBUN_S NE 'S'.
        PERFORM BDC_PROCESS_REST.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE I000 WITH TEXT-M02.
  ENDIF.
ENDFORM.                    " RESTARTING
