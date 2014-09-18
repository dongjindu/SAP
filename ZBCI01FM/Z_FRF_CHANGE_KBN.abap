FUNCTION Z_FRF_CHANGE_KBN.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(P_PKKEY) TYPE  PKPS-PKKEY
*"     VALUE(P_MATNR) TYPE  MARA-MATNR
*"     VALUE(P_PRVBE) TYPE  PKHD-PRVBE
*"     VALUE(P_PKBST) TYPE  PKPS-PKBST
*"     VALUE(P_BEHMG) TYPE  CHAR13
*"     VALUE(P_FLAG) TYPE  CHAR1
*"     VALUE(P_EXIDV) TYPE  EXIDV
*"     VALUE(P_CAST) TYPE  CHAR1
*"     VALUE(P_WERKS) TYPE  PKHD-WERKS
*"     VALUE(P_ACSHOP) TYPE  ZACSHOP
*"     VALUE(P_USRID) TYPE  ZUSRID
*"  EXPORTING
*"     VALUE(P_SEMSG) TYPE  T100-TEXT
*"  TABLES
*"      ZTMMKRFK STRUCTURE  ZSMMKRFK
*"----------------------------------------------------------------------
**"     VALUE(P_STUZT) TYPE  LTAK-STUZT
*"  EXPORTING
*"     VALUE(P_RFSTA) TYPE  CHAR1
*"     VALUE(P_SEMSG) TYPE  T100-TEXT
*"  TABLES
*"      ZTMMKPI STRUCTURE  ZSMMKPI
*"----------------------------------------------------------------------
* *"     VALUE(P_STUZT) TYPE  LTAK-STUZT    "UD1K940229
*&--------------------------------------------------------------------&*
*&  Date         User     Transport       Description
*&  04/02/2007   Manju   UD1K940229    This is function module is copy
*&                                     of  ZFMMMKPI and ZFMMMRFK FMS.
*&                                     This function module gets list
*&                                     of Kanban status for the given
*&                                     input and changes Kanban status
*&                                     for the above selected BARCODES.
*&--------------------------------------------------------------------&*
  DATA: W_ABLAD1 LIKE LTAP-ABLAD,
        W_ABLAD2 LIKE LTAP-ABLAD,
        P_BARCO TYPE  CHAR11,
        P_QTY TYPE   CHAR13,
        PREV_STATUS(1) TYPE C,
        L_PKSAE TYPE TPK01-PKSAE,
        L_LABST LIKE MARD-LABST,
        LV_KANBAN LIKE SY-SUBRC.

  DATA: TEXIDV       LIKE ZMMT0042 OCCURS 0 WITH HEADER LINE.
  DATA: LS_ZMMT0041  LIKE ZMMT0041.
  CLEAR: W_MATNR, TEXIDV, TEXIDV[].
  CLEAR : P_BARCO, P_QTY, PREV_STATUS, LV_KANBAN.

*  IF p_matnr CA '*'.
*    REPLACE '*' WITH '%' INTO p_matnr.
  W_MATNR = P_MATNR.


*-- search available KANBAN ( 99999999 or 88888888 )

** Changed on 02/23/12 for wrong kanban pick

*  SELECT SINGLE t1~pknum t1~ablad t1~umlgo t1~pkumw t2~pkkey t1~BEHMG
**                INTO CORRESPONDING FIELDS OF TABLE it_kbn_matinfo1
*            INTO CORRESPONDING FIELDS OF wa_matinfo
*            FROM pkhd AS t1
*            INNER JOIN pkps AS t2
*              ON t2~pknum = t1~pknum
*         WHERE   t1~prvbe = p_prvbe
*            AND  t1~matnr = w_matnr
**                and   pkbst    = prev_status
*            AND  ( pkbst <> p_pkbst AND pkbst <> '9' )      "9-error
*            AND  pkkey <> p_pkkey .

  IF P_PKBST = '5'.
    SELECT SINGLE T1~PKNUM T1~ABLAD T1~UMLGO T1~PKUMW T2~PKKEY T1~BEHMG
*                INTO CORRESPONDING FIELDS OF TABLE it_kbn_matinfo1
             INTO CORRESPONDING FIELDS OF WA_MATINFO
             FROM PKHD AS T1
             INNER JOIN PKPS AS T2
               ON T2~PKNUM = T1~PKNUM
          WHERE   T1~PRVBE = P_PRVBE
             AND  T1~MATNR = W_MATNR
*                and   pkbst    = prev_status
             AND  ( PKBST <> P_PKBST AND PKBST <> '9'
                     AND PKBST <> '1')                      "9-error
             AND  PKKEY <> P_PKKEY .
  ELSE.
** Changed on 11/02/12 for casting p_cast = 'X'
    IF P_CAST IS INITIAL.
      SELECT SINGLE T1~PKNUM T1~ABLAD T1~UMLGO T1~PKUMW T2~PKKEY T1~BEHMG
*                INTO CORRESPONDING FIELDS OF TABLE it_kbn_matinfo1
                INTO CORRESPONDING FIELDS OF WA_MATINFO
                FROM PKHD AS T1
                INNER JOIN PKPS AS T2
                  ON T2~PKNUM = T1~PKNUM
             WHERE   T1~PRVBE = P_PRVBE
                AND  T1~MATNR = W_MATNR
*                and   pkbst    = prev_status
                AND  ( PKBST <> P_PKBST AND PKBST <> '9' )    "9-error
                AND  PKKEY <> P_PKKEY .
    ELSE.
      SELECT SINGLE T1~PKNUM T1~ABLAD T1~UMLGO T1~PKUMW T2~PKKEY T1~BEHMG
*                INTO CORRESPONDING FIELDS OF TABLE it_kbn_matinfo1
                INTO CORRESPONDING FIELDS OF WA_MATINFO
                FROM PKHD AS T1
                INNER JOIN PKPS AS T2
                  ON T2~PKNUM = T1~PKNUM
             WHERE   T1~PRVBE = P_PRVBE
** For casting on 11/02/12
                AND WERKS = P_WERKS
** End
                AND  T1~MATNR = W_MATNR
*                and   pkbst    = prev_status
                AND  ( PKBST <> P_PKBST AND PKBST <> '9' )    "9-error
                AND  PKKEY <> P_PKKEY .
    ENDIF.
  ENDIF.
** End on 02/23/12
  IF SY-SUBRC NE 0.
    IF P_PKBST EQ '5' .
      P_SEMSG = 'All Kanbans already FULL'.
    ELSEIF P_PKBST EQ '2' .
      P_SEMSG = 'All Kanbans already Empty'.
    ELSE.
      P_SEMSG = 'No Kanbans found '.
    ENDIF.
  ELSE.

*--- EMPTY
    LV_KANBAN = 1.
*---  ONESETP or FULL : check available stock
    IF ( P_PKBST = '2' AND P_FLAG = '1' ) OR P_PKBST = '5'.
** Changed on 11/02/12 for casting p_cast = 'X'
      IF P_CAST IS INITIAL.
        SELECT SINGLE LABST INTO L_LABST
          FROM MARD
          WHERE MATNR = P_MATNR
            AND WERKS = WA_MATINFO-PKUMW
            AND LGORT = WA_MATINFO-UMLGO.
      ELSE.
        SELECT SINGLE LABST INTO L_LABST
        FROM MARD
        WHERE MATNR = P_MATNR
          AND WERKS = P_WERKS
          AND LGORT = WA_MATINFO-UMLGO.
      ENDIF.
** End on 11/02/12
      IF L_LABST < WA_MATINFO-BEHMG.
        ZTMMKRFK-RFSTA = 'E'.
        CONCATENATE 'The Quantity available is less than'
        'the Kanban quantity. Please notify PC.' INTO  ZTMMKRFK-SEMSG
           SEPARATED BY SPACE.
        P_SEMSG = ZTMMKRFK-SEMSG.
        APPEND ZTMMKRFK.
      ELSE.  "OK available stock
        LV_KANBAN = 1.
      ENDIF.
    ENDIF.
  ENDIF.

*-now start KANBAN transaction....
  CHECK LV_KANBAN = 1.
*------ now we don't need looping...
  CLEAR P_BARCO.
  CONCATENATE WA_MATINFO-PKKEY P_PKBST INTO P_BARCO.
  WRITE P_BEHMG TO  P_QTY.

* Call Function Module to change Status
  CALL FUNCTION 'ZFMMMRFK_KBN'
    EXPORTING
      P_BARCO = P_BARCO
      P_MENGE = P_QTY
      P_UNAME = SY-UNAME
    IMPORTING
      P_PKKEY = ZTMMKRFK-PKKEY
      P_PKBST = ZTMMKRFK-PKBST
      P_RFSTA = ZTMMKRFK-RFSTA
      P_SEMSG = ZTMMKRFK-SEMSG.
  APPEND ZTMMKRFK.

  IF ZTMMKRFK-RFSTA EQ 'S'.
*S__ PAUL save into temporary table for later use.
    CLEAR : TEXIDV, TEXIDV[].
    TEXIDV-PKKEY  = WA_MATINFO-PKKEY.
    TEXIDV-EXIDV  = P_EXIDV.
** Change by Park on 11/20/13
    TEXIDV-ACSHOP = P_ACSHOP.
    TEXIDV-USRID  = P_USRID.
** End change.
    CONCATENATE SY-DATUM SY-UZEIT INTO TEXIDV-UPDATE_DATE.
    APPEND TEXIDV.
    MODIFY ZMMT0042 FROM TABLE TEXIDV.
*        commit work.  " to Release locks

**  Start 12/13/2013 Added by Park Requested by Kim
    IF P_PKBST = '2'.
      UPDATE ZMMT0041  SET ACSHOP = P_ACSHOP
                           USRID  = P_USRID
                     WHERE PKKEY EQ WA_MATINFO-PKKEY
                       AND EXIDV EQ ''
                       AND SAEDT EQ SY-DATUM.
    ENDIF.
**  End 12/13/2013 Added by Park Requested by Kim

  ELSE.
    CLEAR ZTMMKRFK.
  ENDIF.

* ONE STEP (EMPTY & FULL)
  IF P_FLAG = '1' AND P_PKBST = '2' .
*- CHECK!!!!
    CONCATENATE WA_MATINFO-PKKEY '5' INTO P_BARCO.

*-wait ????
    CALL FUNCTION 'ZFMMMRFK_KBN'
      EXPORTING
        P_BARCO = P_BARCO
        P_MENGE = P_QTY
        P_UNAME = SY-UNAME
      IMPORTING
        P_PKKEY = ZTMMKRFK-PKKEY
        P_PKBST = ZTMMKRFK-PKBST
        P_RFSTA = ZTMMKRFK-RFSTA
        P_SEMSG = ZTMMKRFK-SEMSG.
    APPEND ZTMMKRFK.

  ENDIF.


ENDFUNCTION.
