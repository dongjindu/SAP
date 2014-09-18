FUNCTION Z_FRF_CHANGE_KBN_OLD.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(P_PKKEY) TYPE  PKPS-PKKEY
*"             VALUE(P_MATNR) TYPE  MARA-MATNR
*"             VALUE(P_PRVBE) TYPE  PKHD-PRVBE
*"             VALUE(P_PKBST) TYPE  PKPS-PKBST
*"             VALUE(P_BEHMG) TYPE  CHAR13
*"             VALUE(P_FLAG) TYPE  CHAR1
*"             VALUE(P_EXIDV) TYPE  EXIDV
*"       EXPORTING
*"             VALUE(P_SEMSG) TYPE  T100-TEXT
*"       TABLES
*"              ZTMMKRFK STRUCTURE  ZSMMKRFK
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
        L_LABST LIKE MARD-LABST.

  DATA: TEXIDV LIKE ZMMT0042 OCCURS 0 WITH HEADER LINE.

  CLEAR: W_MATNR, TEXIDV, TEXIDV[].

  IF P_MATNR CA '*'.
    REPLACE '*' WITH '%' INTO P_MATNR.
    W_MATNR = P_MATNR.
  ELSE.
    W_MATNR = P_MATNR.
  ENDIF.

  CLEAR : P_BARCO, P_QTY,PREV_STATUS.

* Determine Next Status Automatically

* Barcode
  CONCATENATE P_PKKEY P_PKBST INTO P_BARCO.
  WRITE P_BEHMG TO  P_QTY.

**S__ PAUL Modify CBO Table
*  CLEAR : TEXIDV, TEXIDV[].
*  TEXIDV-EXIDV = P_EXIDV.
*  TEXIDV-PKKEY = P_PKKEY.
*  APPEND TEXIDV.
*  MODIFY ZMMT0042 FROM TABLE TEXIDV.

*
** Changed by Furong on 11/17/08
*  if  P_PRVBE eq 'RPK'.                                     "UD1K940325

*-one-step transaction; one scan -> two steps(empty & full)
  IF P_FLAG = '1'.
** End of change
* Call Function Module to change Status

*S__ PAUL Modify CBO Table
    CLEAR : TEXIDV, TEXIDV[].
    TEXIDV-EXIDV = P_EXIDV.
    TEXIDV-PKKEY = P_PKKEY.
    APPEND TEXIDV.
    MODIFY ZMMT0042 FROM TABLE TEXIDV.

*---try to change KANBAN using Barcode.
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
*        commit work.  " to Release locks
*  p_ret eq 1 then pick next Kanban card and set status
      EXIT.

*-- if fail to process Kanban - KANBAN ID is dummy # 999999,
*-- then search available KANBAN
    ELSE.
      SELECT SINGLE * FROM PKPS  WHERE PKKEY = P_PKKEY .
      IF SY-SUBRC EQ 0.
        PREV_STATUS =  PKPS-PKBSA.
      ENDIF.

      SELECT T1~PKNUM T1~ABLAD T2~PKKEY
                INTO CORRESPONDING FIELDS OF TABLE IT_KBN_MATINFO1
                FROM PKHD AS T1
                INNER JOIN PKPS AS T2
                  ON T2~PKNUM = T1~PKNUM
             WHERE T1~PRVBE = P_PRVBE
                AND   T1~MATNR LIKE W_MATNR
*                and   pkbst    = prev_status
                AND   PKBST <> P_PKBST
                AND   PKKEY <> P_PKKEY .
      IF SY-SUBRC NE 0.
        IF P_PKBST EQ '5' .
          P_SEMSG = 'All Kanbans already FULL'.
        ELSEIF P_PKBST EQ '2' .
          P_SEMSG = 'All Kanbans already Empty'.
        ELSE.
          P_SEMSG = 'No Kanbans found '.
        ENDIF.
      ELSE.
*------ now we don't need looping...
        LOOP AT IT_KBN_MATINFO1 INTO WA_MATINFO.
          CLEAR P_BARCO.
          CONCATENATE WA_MATINFO-PKKEY P_PKBST INTO P_BARCO.
          WRITE P_BEHMG TO  P_QTY.

*S__ PAUL Modify CBO Table
          CLEAR : TEXIDV, TEXIDV[].
          TEXIDV-EXIDV = P_EXIDV.
          TEXIDV-PKKEY = WA_MATINFO-PKKEY.
          APPEND TEXIDV.
          MODIFY ZMMT0042 FROM TABLE TEXIDV.

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
*        commit work.  " to Release locks
            EXIT.
          ELSE.
            CLEAR ZTMMKRFK.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*-two step: empty or full transaction
  ELSE.    "P_PRVBE EQ 'HDW'

* Prevous status _ > PKPS( Get Pkkey)  based on Matrial  ? Suplly Area
* PKHD - Get Previous status

    SELECT SINGLE * FROM PKHD WHERE MATNR = P_MATNR AND
                                    PRVBE = P_PRVBE.
*    if sy-subrc eq 0.
*
*      select single * from pkps  where PKNUM = pkhd-PKNUM and
*                                       PKBST <> P_PKBST .
*      if sy-subrc eq 0.
**                                 and PKBST = prev_status.
*        prev_status =  pkps-PKBST.
*      endif.
*
*    endif.


    IF  PKHD-PKSFG  EQ '0001'.

      SELECT T1~PKNUM T1~ABLAD T2~PKKEY T2~PKBSA
                INTO CORRESPONDING FIELDS OF TABLE IT_KBN_MATINFO1
                FROM PKHD AS T1
                INNER JOIN PKPS AS T2
                  ON T2~PKNUM = T1~PKNUM
             WHERE T1~PRVBE = P_PRVBE
                AND   T1~MATNR LIKE W_MATNR
                AND   PKBST    <>  P_PKBST    " Current Status
*                   and   PKBSA    =  P_PKBST    " Previous Status
                AND   PKKEY <> P_PKKEY .
      LOOP AT   IT_KBN_MATINFO1 INTO WA_MATINFO.
        IF NOT ( WA_MATINFO-PKBSA IS INITIAL OR
               WA_MATINFO-PKBSA EQ '1' ).
          DELETE IT_KBN_MATINFO1 WHERE PKKEY EQ WA_MATINFO-PKKEY AND
                                       PKBSA <>  P_PKBST .
        ENDIF.
      ENDLOOP.
    ELSE.
      SELECT T1~PKNUM T1~ABLAD T2~PKKEY
                INTO CORRESPONDING FIELDS OF TABLE IT_KBN_MATINFO1
                FROM PKHD AS T1
                INNER JOIN PKPS AS T2
                  ON T2~PKNUM = T1~PKNUM
             WHERE T1~PRVBE = P_PRVBE
                AND   T1~MATNR LIKE W_MATNR
                AND   PKBST    <>  P_PKBST    " Current Status
*                  and   PKBSA    <>  P_PKBST    " Previous Status
                AND   PKKEY <> P_PKKEY .
      LOOP AT   IT_KBN_MATINFO1 INTO WA_MATINFO.
        IF NOT ( WA_MATINFO-PKBSA IS INITIAL OR
                WA_MATINFO-PKBSA EQ '1' ).

          DELETE IT_KBN_MATINFO1 WHERE PKKEY EQ WA_MATINFO-PKKEY AND
                                       PKBSA <>  P_PKBST .
        ENDIF.
      ENDLOOP.
*      delete it_kbn_matinfo1 where PKBSA <> P_PKBST .
    ENDIF.

    LOOP AT IT_KBN_MATINFO1 INTO WA_MATINFO.
      CLEAR P_BARCO.
      CONCATENATE WA_MATINFO-PKKEY P_PKBST INTO P_BARCO.
      WRITE P_BEHMG TO  P_QTY.

** Changed by Furong on 05/19/09

      SELECT SINGLE LABST INTO L_LABST
        FROM MARD
        WHERE MATNR = P_MATNR
          AND WERKS = PKHD-PKUMW
          AND LGORT = PKHD-UMLGO.

      IF L_LABST < PKHD-BEHMG.
        ZTMMKRFK-RFSTA = 'E'.
        concatenate 'The Quantity available is less than'
           'the Kanban quantity. Please notify PC.' into  ZTMMKRFK-SEMSG
           separated by space.
        P_SEMSG = ZTMMKRFK-SEMSG.
        APPEND ZTMMKRFK.
        EXIT.
      ENDIF.
** End of change


*S__ PAUL Modify CBO Table
      CLEAR : TEXIDV, TEXIDV[].
      TEXIDV-EXIDV = P_EXIDV.
      TEXIDV-PKKEY = WA_MATINFO-PKKEY.
      APPEND TEXIDV.
      MODIFY ZMMT0042 FROM TABLE TEXIDV.

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
*        commit work.  " to Release locks
        EXIT.
      ELSE.
        CLEAR ZTMMKRFK.
      ENDIF.
    ENDLOOP.


  ENDIF.

ENDFUNCTION.
