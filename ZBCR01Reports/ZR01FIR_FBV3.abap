*&---------------------------------------------------------------------*
* Provided by Andy Choi
*&---------------------------------------------------------------------*
REPORT  z_park          LINE-SIZE 90
                        NO STANDARD PAGE HEADING
                        MESSAGE-ID ZFI.
TABLES: vbkpf, vbsegs, vbsegd, vbsegk, skat, LFA1.

DATA: tvbkpf  LIKE vbkpf  OCCURS 0 WITH HEADER LINE,
      tvbsegs LIKE vbsegs OCCURS 0 WITH HEADER LINE,
      tvbsegd LIKE vbsegd OCCURS 0 WITH HEADER LINE,
      tvbsegk LIKE vbsegk OCCURS 0 WITH HEADER LINE,
      tskat   LIKE skat   OCCURS 0 WITH HEADER LINE.

DATA: g_flag   TYPE c,
      flag     TYPE c,
      D_AMOUNT LIKE bseg-wrbtr,
      C_AMOUNT LIKE bseg-wrbtr.

TYPEs: BEGIN OF it_bkpf,
         AUSBK LIKE vbkpf-AUSBK,
         BUKRS LIKE vbkpf-BUKRS,
         GJAHR LIKE vbkpf-GJAHR,
         bldat LIKE vbkpf-bldat,
         belnr LIKE vbkpf-belnr,
         bktxt LIKE vbkpf-bktxt,
         XBLNR LIKE vbkpf-XBLNR,
         BLART LIKE VBKPF-BLART,       "Document type
         XWFFR LIKE VBKPF-XWFFR,       "Enter release
         XPRFG LIKE VBKPF-XPRFG,       "Complete
         XFRGE LIKE VBKPF-XFRGE,       "Released
         USNAM LIKE vbkpf-USNAM,
       END OF it_bkpf.

DATA: it_tab_bkpf TYPE it_bkpf OCCURS 0 WITH HEADER LINE.
DATA: it_sel_bkpf TYPE it_bkpf OCCURS 0 WITH HEADER LINE.

DATA:  BEGIN OF it_tab_bseg OCCURS 0,
       belnr LIKE bkpf-belnr,
       pkey LIKE bseg-bschl,
       anom LIKE bseg-hkont,
       text LIKE bseg-sgtxt,
       vdat LIKE bseg-valut,
       amnt LIKE bseg-wrbtr,
       ccnt like bseg-kostl,
       saknr LIKE bseg-saknr.
DATA: END OF it_tab_bseg.

DATA: BEGIN OF it_tab_skat OCCURS 0,
      saknr LIKE skat-saknr,
      anam like skat-txt50.
DATA: END OF it_tab_skat.

DATA:  BEGIN OF it_tab OCCURS 0,
       date LIKE bkpf-bldat,
       doc  LIKE bkpf-belnr,
       head LIKE bkpf-bktxt,
       pkey LIKE bseg-bschl,
       anom LIKE bseg-hkont,
       amnt LIKE bseg-wrbtr,
       anam LIKE skat-txt50,
       ccnt LIKE cobl-kostl,
       text LIKE bseg-sgtxt,
       vdat LIKE bseg-valut,
       usnam LIKE vbkpf-usnam.
DATA: END OF it_tab.

DATA: BEGIN OF IT_SCREEN OCCURS 0,
        TCODE LIKE  SY-UCOMM,
      END OF   IT_SCREEN.

DEFINE __EXCLUDE.
  CLEAR IT_SCREEN.
  MOVE &1 TO IT_SCREEN-TCODE.
  APPEND IT_SCREEN.
END-OF-DEFINITION.
*
Parameters:    p_bukrs like vbkpf-bukrs memory id BUK.
SELECT-OPTIONS:p_belnr  FOR vbkpf-belnr,
               p_gjahr  FOR vbkpf-gjahr.
SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
*PARAMETERS: pa_empy LIKE vbkpf-usnam.
SELECT-OPTIONS:BLART   FOR VBKPF-BLART,       "Document type
               p_bldat FOR vbkpf-bldat,
               p_CPUDT for vbkpf-CPUDT,
               p_xblnr for vbkpf-xblnr.

SELECTION-SCREEN END OF BLOCK bl1.
*
SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.

*Complete
PARAMETERS: p_XPRFG as checkbox.
*No Release
*ELECT-OPTIONS: XWFFR FOR VBKPF-XWFFR NO INTERVALS NO-EXTENSION.
*Released
*ELECT-OPTIONS: XFRGE FOR VBKPF-XFRGE NO INTERVALS NO-EXTENSION.

*PARAMETERS: p_own TYPE C AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_own TYPE C DEFAULT 'X' no-display.

SELECTION-SCREEN END OF BLOCK bl2.

PARAMETERS: P_OLD TYPE C DEFAULT ' ' NO-DISPLAY.
*
*-----------------------------------------------------------------------
START-OF-SELECTION.
*-----------------------------------------------------------------------
  SET MARGIN 10.
* SET PF-STATUS 'CHOICE2'.

  PERFORM data_search.
  IF SY-SUBRC <> '0'.
    WRITE: / 'No Parked Document'.
  ENDIF.

************************************************************************
END-OF-SELECTION.
*-----------------------------------------------------------------------
  PERFORM data_output1.

************************************************************************
TOP-OF-PAGE.
  perform top_of_page.

************************************************************************
AT USER-COMMAND.
*---------------

************************************************************************
***  (SHIFT+PF1) Execute Download
************************************************************************

* create
AT pf13.
  CALL TRANSACTION 'F-63'.

  SY-LSIND = SY-LSIND - 1.
  PERFORM data_search.

  perform top_of_page.
  PERFORM data_output1.

* F4 - change
AT pf16.
  check it_tab_bkpf-BELNR <> space.
  PERFORM CALL_CHANGE_RTN.

* post
AT pf17.
  check it_tab_bkpf-BELNR <> space.
  PERFORM CALL_POST_RTN.

  exit.
  PERFORM CALL_DELETE_RTN.


************************************************************************
* AT LINE-SELECTION                                                    *
************************************************************************
AT LINE-SELECTION.

  __EXCLUDE 'DISPLAY'.
  __EXCLUDE 'CHANGE'.
  __EXCLUDE 'POST'.
  __EXCLUDE 'DELETE'.
  __EXCLUDE 'CREATE'.
*      SET PF-STATUS 'CHOICE2' EXCLUDING IT_SCREEN.
  REFRESH IT_SCREEN.

  if p_old = 'X'.
    PERFORM SINGLE_data_output2.
  else.
    SUBMIT ZR01FIR_FB03
    WITH P_BELNR = it_tab_bkpf-BELNR
            WITH P_BUKRS = p_bukrs
            WITH P_GJAHR = it_tab_bkpf-GJAHR
            WITH P_PARK  = 'X'
            WITH ACCSUM  = ' '
            with p_own   = ' '
           AND RETURN.

  endif.
*&---------------------------------------------------------------------*
*&      Form  data_search
*&---------------------------------------------------------------------*
FORM data_search.
  REFRESH it_tab_bkpf.

  IF p_own = 'X'.
    SELECT AUSBK BUKRS GJAHR belnr bldat bktxt XBLNR USNAM
             BLART XWFFR XPRFG XFRGE
        INTO CORRESPONDING FIELDS OF TABLE it_tab_bkpf
        FROM vbkpf
        WHERE bukrs =  p_bukrs
          and gjahr in p_gjahr
          and belnr IN p_belnr
          AND bldat IN p_bldat
          and xblnr in p_xblnr
          AND CPUDT in p_CPUDT
          AND BLART IN BLART
*       AND XWFFR IN XWFFR
*       AND XFRGE IN XFRGE
          AND XPRFG =  p_XPRFG
          AND usnam = sy-uname.
  ELSE.
    SELECT AUSBK BUKRS GJAHR belnr bldat bktxt XBLNR USNAM
           BLART XWFFR XPRFG XFRGE
      INTO CORRESPONDING FIELDS OF TABLE it_tab_bkpf
      FROM vbkpf
      WHERE bukrs =  p_bukrs
        and gjahr in p_gjahr
        and belnr IN p_belnr
        and xblnr in p_xblnr
        AND CPUDT in p_CPUDT
        AND bldat IN p_bldat
        AND BLART IN BLART
*       AND XWFFR IN XWFFR
*       AND XFRGE IN XFRGE.
        AND XPRFG = p_XPRFG.
  ENDIF.


ENDFORM.                    " data_search
*&---------------------------------------------------------------------*
*&      Form  data_output
*&---------------------------------------------------------------------*
FORM data_output1.
  format RESET.


  LOOP AT it_tab_bkpf.
    WRITE:/1  it_tab_bkpf-bldat,
           14 it_tab_bkpf-belnr,
           26 it_tab_bkpf-bktxt,
        52    it_tab_bkpf-XBLNR,
        70    it_tab_bkpf-BLART,
              it_tab_bkpf-XWFFR,
              it_tab_bkpf-XPRFG,
              it_tab_bkpf-XFRGE,
         (10) it_tab_bkpf-USNAM.

    HIDE: it_tab_bkpf-AUSBK, it_tab_bkpf-BUKRS, it_tab_bkpf-bktxt,
          it_tab_bkpf-BELNR, it_tab_bkpf-GJAHR, it_tab_bkpf-bldat,
          it_tab_bkpf-XBLNR, it_tab_bkpf-USNAM.

  ENDLOOP.

  CLEAR: it_tab_bkpf-AUSBK, it_tab_bkpf-BUKRS, it_tab_bkpf-bktxt,
         it_tab_bkpf-BELNR, it_tab_bkpf-GJAHR, it_tab_bkpf-bldat,
         it_tab_bkpf-XBLNR, it_tab_bkpf-USNAM.

ENDFORM.                    " data_output
*&---------------------------------------------------------------------*
*&      Form  mark_cb_all
*&---------------------------------------------------------------------*
FORM mark_cb_all.
  DO.
    READ LINE sy-index FIELD VALUE flag.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    MODIFY CURRENT LINE FIELD VALUE flag FROM g_flag.
  ENDDO.

ENDFORM.                    " mark_cb_all
*&---------------------------------------------------------------------*
*&      Form  scan_checkbox
*&---------------------------------------------------------------------*
FORM scan_checkbox.

  REFRESH it_sel_bkpf.

  DO.
    CLEAR flag.

    READ LINE sy-index FIELD VALUE flag.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.


    CHECK flag = 'X'.

    CLEAR it_sel_bkpf.

    MOVE-CORRESPONDING it_tab_bkpf TO it_sel_bkpf.

    APPEND it_sel_bkpf.

  ENDDO.

ENDFORM.                    " scan_checkbox
*&---------------------------------------------------------------------*
*&      Form  data_output2
*&---------------------------------------------------------------------*
FORM data_output2.

  SELECT * FROM skat INTO CORRESPONDING FIELDS OF TABLE tskat
    WHERE SPRAS = 'E'
      AND KTOPL = '2000'
    ORDER BY SAKNR.

  LOOP AT it_sel_bkpf.

*    AT NEW belnr.    "Document No
*      NEW-PAGE.
*      PERFORM PRINT_RTN.
*    ENDAT.

    REFRESH it_tab.
    D_AMOUNT = 0.
    C_AMOUNT = 0.
* VBSEGS
    SELECT * FROM VBSEGS
      WHERE AUSBK = it_sel_bkpf-AUSBK
        AND BELNR = it_sel_bkpf-BELNR
        AND GJAHR = it_sel_bkpf-GJAHR.

      MOVE it_tab_bkpf-bldat TO it_tab-date.
      MOVE it_tab_bkpf-belnr TO it_tab-doc.
      MOVE it_tab_bkpf-bktxt TO it_tab-head.
      MOVE VBSEGS-BSCHL      TO it_tab-pkey.
      MOVE VBSEGS-SAKNR      TO it_tab-anom.

      IF VBSEGS-SHKZG = 'H'.                          "Credit
        it_tab-amnt = -1 * VBSEGS-DMBTR.
      ELSEIF VBSEGS-SHKZG = 'S'.                      "Debit
        MOVE VBSEGS-DMBTR       TO it_tab-amnt.
      ENDIF.

      READ TABLE tskat WITH KEY SAKNR = VBSEGS-SAKNR  BINARY SEARCH.
      MOVE tskat-TXT50       TO it_tab-anam.
      MOVE VBSEGS-KOSTL      TO it_tab-ccnt.
      MOVE VBSEGS-SGTXT      TO it_tab-text.
      MOVE VBSEGS-VALUT      TO it_tab-vdat.

      IF VBSEGS-SHKZG = 'H'.
        C_AMOUNT = C_AMOUNT + it_tab-amnt.
      ELSEIF VBSEGS-SHKZG = 'S'.
        D_AMOUNT = D_AMOUNT + it_tab-amnt.
      ENDIF.

      APPEND it_tab.

    ENDSELECT.
* VBSEGD
    SELECT * FROM VBSEGD
      WHERE AUSBK = it_sel_bkpf-AUSBK
        AND BELNR = it_sel_bkpf-BELNR
        AND GJAHR = it_sel_bkpf-GJAHR.

      MOVE it_tab_bkpf-bldat TO it_tab-date.
      MOVE it_tab_bkpf-belnr TO it_tab-doc.
      MOVE it_tab_bkpf-bktxt TO it_tab-head.
      MOVE VBSEGD-BSCHL      TO it_tab-pkey.
*      MOVE VBSEGD-SAKNR      TO it_tab-anom.
      IF VBSEGD-SHKZG = 'H'.                          "Credit
        it_tab-amnt = -1 * VBSEGD-DMBTR.
      ELSEIF VBSEGD-SHKZG = 'S'.                      "Debit
        MOVE VBSEGD-DMBTR       TO it_tab-amnt.
      ENDIF.

*      MOVE                   TO it_tab-anam.
*      MOVE VBSEGD-KOSTL      TO it_tab-ccnt.
      MOVE VBSEGD-SGTXT      TO it_tab-text.
      MOVE VBSEGD-VALUT      TO it_tab-vdat.

      IF VBSEGD-SHKZG = 'H'.
        C_AMOUNT = C_AMOUNT + it_tab-amnt.
      ELSEIF VBSEGD-SHKZG = 'S'.
        D_AMOUNT = D_AMOUNT + it_tab-amnt.
      ENDIF.

      APPEND it_tab.

    ENDSELECT.
* VBSEGK
    SELECT * FROM VBSEGK
      WHERE AUSBK = it_sel_bkpf-AUSBK
        AND BELNR = it_sel_bkpf-BELNR
        AND GJAHR = it_sel_bkpf-GJAHR.

      MOVE it_tab_bkpf-bldat TO it_tab-date.
      MOVE it_tab_bkpf-belnr TO it_tab-doc.
      MOVE it_tab_bkpf-bktxt TO it_tab-head.
      MOVE VBSEGK-BSCHL      TO it_tab-pkey.
      MOVE VBSEGK-HKONT      TO it_tab-anom.

      IF VBSEGK-SHKZG = 'H'.                          "Credit
        it_tab-amnt = -1 * VBSEGK-DMBTR.
      ELSEIF VBSEGK-SHKZG = 'S'.                      "Debit
        MOVE VBSEGK-DMBTR       TO it_tab-amnt.
      ENDIF.

      READ TABLE tskat WITH KEY SAKNR = VBSEGK-HKONT BINARY SEARCH.
      MOVE tskat-TXT50       TO it_tab-anam.
*      MOVE VBSEGK-KOSTL      TO it_tab-ccnt.
      MOVE VBSEGK-SGTXT      TO it_tab-text.
      MOVE VBSEGK-VALUT      TO it_tab-vdat.

      APPEND it_tab.

      IF VBSEGK-SHKZG = 'H'.
        C_AMOUNT = C_AMOUNT + it_tab-amnt.
      ELSEIF VBSEGK-SHKZG = 'S'.
        D_AMOUNT = D_AMOUNT + it_tab-amnt.
      ENDIF.

    ENDSELECT.
*
    NEW-PAGE.
    PERFORM PRINT_RTN.

  ENDLOOP.

*  LOOP AT IT_TAB.
*    WRITE: / it_tab-date, it_tab-doc,
*             it_tab-head, it_tab-pkey, it_tab-anam,
*             it_tab-anom, it_tab-amnt, it_tab-ccnt,
*             it_tab-text, it_tab-vdat.
*
*  ENDLOOP.

ENDFORM.                    " data_output2

*&---------------------------------------------------------------------*
*&      Form  SINGLE_data_output2
*&---------------------------------------------------------------------*
FORM SINGLE_data_output2.

  SELECT * FROM skat INTO CORRESPONDING FIELDS OF TABLE tskat
    WHERE SPRAS = 'E'
      AND KTOPL = '2000'
    ORDER BY SAKNR.

  REFRESH it_tab.
  D_AMOUNT = 0.
  C_AMOUNT = 0.
* VBSEGS
  SELECT * FROM VBSEGS
    WHERE AUSBK = it_tab_bkpf-AUSBK
      AND BELNR = it_tab_bkpf-BELNR
      AND GJAHR = it_tab_bkpf-GJAHR.

    MOVE it_tab_bkpf-bldat TO it_tab-date.
    MOVE it_tab_bkpf-belnr TO it_tab-doc.
    MOVE it_tab_bkpf-bktxt TO it_tab-head.
    MOVE VBSEGS-BSCHL      TO it_tab-pkey.
    MOVE VBSEGS-SAKNR      TO it_tab-anom.

    IF VBSEGS-SHKZG = 'H'.                          "Credit
      it_tab-amnt = -1 * VBSEGS-DMBTR.
    ELSEIF VBSEGS-SHKZG = 'S'.                      "Debit
      MOVE VBSEGS-DMBTR       TO it_tab-amnt.
    ENDIF.

    READ TABLE tskat WITH KEY SAKNR = VBSEGS-SAKNR  BINARY SEARCH.
    MOVE tskat-TXT50       TO it_tab-anam.
    MOVE VBSEGS-KOSTL      TO it_tab-ccnt.
    MOVE VBSEGS-SGTXT      TO it_tab-text.
    MOVE VBSEGS-VALUT      TO it_tab-vdat.

    IF VBSEGS-SHKZG = 'H'.
      C_AMOUNT = C_AMOUNT + it_tab-amnt.
    ELSEIF VBSEGS-SHKZG = 'S'.
      D_AMOUNT = D_AMOUNT + it_tab-amnt.
    ENDIF.

    APPEND it_tab.

  ENDSELECT.
* VBSEGD
  SELECT * FROM VBSEGD
    WHERE AUSBK = it_tab_bkpf-AUSBK
      AND BELNR = it_tab_bkpf-BELNR
      AND GJAHR = it_tab_bkpf-GJAHR.

    MOVE it_tab_bkpf-bldat TO it_tab-date.
    MOVE it_tab_bkpf-belnr TO it_tab-doc.
    MOVE it_tab_bkpf-bktxt TO it_tab-head.
    MOVE VBSEGD-BSCHL      TO it_tab-pkey.
*      MOVE VBSEGD-SAKNR      TO it_tab-anom.
    IF VBSEGD-SHKZG = 'H'.                          "Credit
      it_tab-amnt = -1 * VBSEGD-DMBTR.
    ELSEIF VBSEGD-SHKZG = 'S'.                      "Debit
      MOVE VBSEGD-DMBTR       TO it_tab-amnt.
    ENDIF.

*      MOVE                   TO it_tab-anam.
*      MOVE VBSEGD-KOSTL      TO it_tab-ccnt.
    MOVE VBSEGD-SGTXT      TO it_tab-text.
    MOVE VBSEGD-VALUT      TO it_tab-vdat.

    IF VBSEGD-SHKZG = 'H'.
      C_AMOUNT = C_AMOUNT + it_tab-amnt.
    ELSEIF VBSEGD-SHKZG = 'S'.
      D_AMOUNT = D_AMOUNT + it_tab-amnt.
    ENDIF.

    APPEND it_tab.

  ENDSELECT.
* VBSEGK
  SELECT * FROM VBSEGK
    WHERE AUSBK = it_tab_bkpf-AUSBK
      AND BELNR = it_tab_bkpf-BELNR
      AND GJAHR = it_tab_bkpf-GJAHR.

    MOVE it_tab_bkpf-bldat TO it_tab-date.
    MOVE it_tab_bkpf-belnr TO it_tab-doc.
    MOVE it_tab_bkpf-bktxt TO it_tab-head.
    MOVE VBSEGK-BSCHL      TO it_tab-pkey.
    MOVE VBSEGK-HKONT      TO it_tab-anom.

    IF VBSEGK-SHKZG = 'H'.                          "Credit
      it_tab-amnt = -1 * VBSEGK-DMBTR.
    ELSEIF VBSEGK-SHKZG = 'S'.                      "Debit
      MOVE VBSEGK-DMBTR       TO it_tab-amnt.
    ENDIF.

    READ TABLE tskat WITH KEY SAKNR = VBSEGK-HKONT BINARY SEARCH.
    MOVE tskat-TXT50       TO it_tab-anam.
*      MOVE VBSEGK-KOSTL      TO it_tab-ccnt.
    MOVE VBSEGK-SGTXT      TO it_tab-text.
    MOVE VBSEGK-VALUT      TO it_tab-vdat.

    APPEND it_tab.

    IF VBSEGK-SHKZG = 'H'.
      C_AMOUNT = C_AMOUNT + it_tab-amnt.
    ELSEIF VBSEGK-SHKZG = 'S'.
      D_AMOUNT = D_AMOUNT + it_tab-amnt.
    ENDIF.

  ENDSELECT.
*
  SELECT SINGLE * FROM LFA1  WHERE LIFNR = it_tab_bkpf-XBLNR.
  IF SY-SUBRC <> 0.
    LFA1-NAME1 = ''.
  ENDIF.
*
  PERFORM PRINT_RTN.

ENDFORM.                    " SINGLE_data_output2

*&---------------------------------------------------------------------*
*&      Form  PRINT_RTN.
*&---------------------------------------------------------------------*
FORM PRINT_RTN.

*  PRINT-CONTROL SIZE '5' COLOR RED.

  SKIP 5.
  WRITE: /33 'EXPENSE REPORT'.
  WRITE: /33 '=============='.
  SKIP 1.
*  WRITE: /52 '-------- ', '-------- ', '--------'.
*  WRITE: /52 'Prepared ', 'Approved ', 'Approved'.
  WRITE: / 'Document no. :', it_tab_bkpf-BELNR,
           52 '-------- ', '-------- ', '--------'..
  hide it_tab_bkpf-belnr.
  WRITE: / 'Document date:', it_tab_bkpf-bldat,
           52 'Prepared ', 'Approved ', 'Approved'..
  WRITE: / 'Header text:',   it_tab_bkpf-bktxt.
  WRITE: / 'Reference:', (6) it_tab_bkpf-XBLNR.
  WRITE:  LFA1-NAME1.
  SKIP 1.
  ULINE /1(80).
  WRITE: / 'Pk', 'Account ', (40) 'Account name', 'Cost center',
           (14) 'Amount' RIGHT-JUSTIFIED.
  WRITE: /10(50) 'Text', 'Due date'.
  ULINE /1(80).

  LOOP AT it_tab.
    WRITE: / it_tab-pkey, (8) it_tab-anom, (40) it_tab-anam,
             it_tab-ccnt, it_tab-amnt.
    WRITE: /10 it_tab-text, it_tab-vdat.

  ENDLOOP.

  ULINE /1(80).
  WRITE: / 'Debit amount: ', D_AMOUNT.
  WRITE: / 'Credit amount:', C_AMOUNT.

*  WRITE: /53 'Prepared by : ', sy-uname.
  WRITE: /53 'Prepared by : ', it_tab_bkpf-USNAM.

  hide it_tab_bkpf-belnr.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALL_CHANGE_RTN
*&---------------------------------------------------------------------*
FORM CALL_CHANGE_RTN.

  SET PARAMETER ID 'BLP' FIELD it_tab_bkpf-BELNR.
  SET PARAMETER ID 'BUK' FIELD it_tab_bkpf-BUKRS.
  SET PARAMETER ID 'GJR' FIELD it_tab_bkpf-GJAHR.

*      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

  CALL TRANSACTION 'FBV2'  AND SKIP FIRST SCREEN.

ENDFORM.                    " CALL_CHANGE_RTN

*&---------------------------------------------------------------------*
*&      Form  CALL_POST_RTN
*&---------------------------------------------------------------------*
FORM CALL_POST_RTN.

  SET PARAMETER ID 'BLP' FIELD it_tab_bkpf-BELNR.
  SET PARAMETER ID 'BUK' FIELD it_tab_bkpf-BUKRS.
  SET PARAMETER ID 'GJR' FIELD it_tab_bkpf-GJAHR.

  CALL TRANSACTION 'FBV0' AND SKIP FIRST SCREEN.

*  CALL FUNCTION 'PRELIMINARY_POSTING_POST'
*       EXPORTING
*            BELNR   = it_tab_bkpf-BELNR
*            BUKRS   = it_tab_bkpf-BUKRS
*            GJAHR   = it_tab_bkpf-GJAHR.
**            NOCHECK = ' '.
*
*  IF SY-SUBRC <> 0.
*    MESSAGE W017 WITH 'POSTING ERROR...'.
*  ELSE.
*    MESSAGE I017 WITH 'SUCCESSFULLY POSTED...'.
*  ENDIF.

ENDFORM.                    " CALL_CHANGE_RTN

*&---------------------------------------------------------------------*
*&      Form  CALL_DELETE_RTN
*&---------------------------------------------------------------------*
FORM CALL_DELETE_RTN.

*    SET PARAMETER ID 'BLP' FIELD it_tab_bkpf-BELNR.
*    SET PARAMETER ID 'BUK' FIELD it_tab_bkpf-BUKRS.
*    SET PARAMETER ID 'GJR' FIELD it_tab_bkpf-GJAHR.
*
*      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

  CALL FUNCTION 'PRELIMINARY_POSTING_DOC_DELETE'
       EXPORTING
            BELNR = it_tab_bkpf-BELNR
            BUKRS = it_tab_bkpf-BUKRS
            GJAHR = it_tab_bkpf-GJAHR.
*     EXCEPTIONS
*       DOCUMENT_NOT_FOUND       = 1
*       UPDATE_ERROR             = 2
*       OTHERS                   = 3

  IF SY-SUBRC <> 0.
    MESSAGE W017 WITH 'DELETE ERROR...'.
  ELSE.
    MESSAGE I017 WITH 'SUCCESSFULLY DELETED..'.
  ENDIF.

ENDFORM.                    " CALL_CHANGE_RTN

*---------------------------------------------------------------------*
*       FORM CONTROL_TOOLBAR_MENU                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM CONTROL_TOOLBAR_MENU.

  __EXCLUDE 'DISPLAY'.
  __EXCLUDE 'CHANGE'.
  __EXCLUDE 'POST'.
  __EXCLUDE 'DELETE'.
  __EXCLUDE 'CREATE'.
*      SET PF-STATUS 'CHOICE2' EXCLUDING IT_SCREEN.
  REFRESH IT_SCREEN.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
FORM top_of_page.

  WRITE:/ 'User ID:', sy-uname,
          60 'F4-Change, F5-Post, F1-Create'.
  format color 4.
  ULINE.
  WRITE: /1(10)  'Doc. Date',
          14(10) 'Doc. No.' ,
          26 'Header Text' ,
          52 'Reference' ,
          70 'Ty', 'E', 'C', 'R',
          'UserID'.
  ULINE.
ENDFORM.                    " top_of_page
