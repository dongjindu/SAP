FUNCTION ZFMMMKPI_FK_OLD.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(P_PKKEY) TYPE  PKPS-PKKEY
*"             VALUE(P_MATNR) TYPE  MARA-MATNR
*"             VALUE(P_PRVBE) TYPE  PKHD-PRVBE
*"             VALUE(P_PKBST) TYPE  PKPS-PKBST
*"             VALUE(P_BEHMG) TYPE  CHAR13
*"             VALUE(P_FLAG) TYPE  CHAR1
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

  DATA: w_ablad1 LIKE ltap-ablad,
        w_ablad2 LIKE ltap-ablad,
        p_barco TYPE  char11,
        p_qty TYPE   char13,
        prev_status(1) TYPE c,
        l_pksae TYPE tpk01-pksae,
        l_labst LIKE mard-labst.

  CLEAR: w_matnr.

  IF p_matnr CA '*'.
    REPLACE '*' WITH '%' INTO p_matnr.
    w_matnr = p_matnr.
  ELSE.
    w_matnr = p_matnr.
  ENDIF.

** Changed on 02/03/10
  CALL FUNCTION 'ENQUEUE_EMMBEWE'
   EXPORTING
     mode_mbew            = 'E'
     mandt                = sy-mandt
     matnr                = w_matnr
*   BWKEY                =
*   BWTAR                =
*   X_MATNR              = ' '
*   X_BWKEY              = ' '
*   X_BWTAR              = ' '
*   _SCOPE               = '2'
*   _WAIT                = ' '
*   _COLLECT             = ' '
   EXCEPTIONS
     foreign_lock         = 1
     system_failure       = 2
     OTHERS               = 3
            .
  IF sy-subrc <> 0.
    ztmmkrfk-rfsta = 'E'.
    CONCATENATE 'Valuation data locked' ' ' INTO  ztmmkrfk-semsg
       SEPARATED BY space.
    p_semsg = ztmmkrfk-semsg.
    APPEND ztmmkrfk.
    EXIT.
  ELSE.
    CALL FUNCTION 'DEQUEUE_EMMBEWE'
     EXPORTING
       mode_mbew       = 'E'
       mandt           = sy-mandt
       matnr           = w_matnr
*   BWKEY           =
*   BWTAR           =
*   X_MATNR         = ' '
*   X_BWKEY         = ' '
*   X_BWTAR         = ' '
*   _SCOPE          = '3'
*   _SYNCHRON       = ' '
*   _COLLECT        = ' '
              .

  ENDIF.

** End of change
  CLEAR : p_barco, p_qty,prev_status.

* Determine Next Status Automatically
*CALL FUNCTION 'PK_GET_STATUS'
*  EXPORTING
*    PKSAE_IV            = l_pksae
*    PKHD_IS             = PKHD
*    PKPS_IS             = PKPS
*    TPK00_IS            = TPK00
* IMPORTING
*   NEXTSTATUS_EV        =  next_status.

* Barcode
  CONCATENATE p_pkkey p_pkbst INTO p_barco.
  WRITE p_behmg TO  p_qty.

** Changed by Furong on 11/17/08
*  if  P_PRVBE eq 'RPK'.                                     "UD1K940325
  IF p_flag = '1'.
** End of change

** Changed by Furong on 02/01//11
    IF p_pkbst = '2'.
      SELECT SINGLE * FROM pkhd WHERE matnr = p_matnr AND
                                        prvbe = p_prvbe.
      SELECT SINGLE labst INTO l_labst
           FROM mard
           WHERE matnr = p_matnr
             AND werks = pkhd-pkumw
             AND lgort = pkhd-umlgo.

      IF l_labst < pkhd-behmg.
        ztmmkrfk-rfsta = 'E'.
        CONCATENATE 'The Quantity available is less than'
           'the Kanban quantity. Please notify PC.' INTO  ztmmkrfk-semsg
           SEPARATED BY space.
        p_semsg = ztmmkrfk-semsg.
        APPEND ztmmkrfk.
        EXIT.
      ENDIF.
    ENDIF.
** End of changeon 02/01//11

* Call Function Module to change Status
    CALL FUNCTION 'ZFMMMRFK'
         EXPORTING
              p_barco = p_barco
              p_menge = p_qty
              p_uname = sy-uname
         IMPORTING
              p_pkkey = ztmmkrfk-pkkey
              p_pkbst = ztmmkrfk-pkbst
              p_rfsta = ztmmkrfk-rfsta
              p_semsg = ztmmkrfk-semsg.

    APPEND ztmmkrfk.

    IF ztmmkrfk-rfsta EQ 'S'.
*        commit work.  " to Release locks

*  p_ret eq 1 then pick next Kanban card and set status
      EXIT.
    ELSE.

      SELECT SINGLE * FROM pkps  WHERE pkkey = p_pkkey .
      IF sy-subrc EQ 0.
        prev_status =  pkps-pkbsa.
      ENDIF.

      SELECT t1~pknum t1~ablad t2~pkkey
                INTO CORRESPONDING FIELDS OF TABLE it_kbn_matinfo1
                FROM pkhd AS t1
                INNER JOIN pkps AS t2
                  ON t2~pknum = t1~pknum
             WHERE t1~prvbe = p_prvbe
                AND   t1~matnr LIKE w_matnr
*                and   pkbst    = prev_status
                AND   pkbst    <> p_pkbst
                AND   pkkey <> p_pkkey .
      IF sy-subrc NE 0.
        IF p_pkbst EQ '5' .
          p_semsg = 'All Kanbans already FULL'.
        ELSEIF p_pkbst EQ '2' .
          p_semsg = 'All Kanbans already Empty'.
        ELSE.
          p_semsg = 'No Kanbans found '.
        ENDIF.
      ELSE.
        LOOP AT it_kbn_matinfo1 INTO wa_matinfo.
          CLEAR p_barco.
          CONCATENATE wa_matinfo-pkkey p_pkbst INTO p_barco.
          WRITE p_behmg TO  p_qty.

* Call Function Module to change Status
          CALL FUNCTION 'ZFMMMRFK'
               EXPORTING
                    p_barco = p_barco
                    p_menge = p_qty
                    p_uname = sy-uname
               IMPORTING
                    p_pkkey = ztmmkrfk-pkkey
                    p_pkbst = ztmmkrfk-pkbst
                    p_rfsta = ztmmkrfk-rfsta
                    p_semsg = ztmmkrfk-semsg.
          APPEND ztmmkrfk.
          IF ztmmkrfk-rfsta EQ 'S'.
*        commit work.  " to Release locks
            EXIT.
          ELSE.
            CLEAR ztmmkrfk.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ELSE.    "P_PRVBE EQ 'HDW'

* Prevous status _ > PKPS( Get Pkkey)  based on Matrial  ? Suplly Area
* PKHD - Get Previous status

    SELECT SINGLE * FROM pkhd WHERE matnr = p_matnr AND
                                    prvbe = p_prvbe.
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


    IF  pkhd-pksfg  EQ '0001'.

      SELECT t1~pknum t1~ablad t2~pkkey t2~pkbsa
                INTO CORRESPONDING FIELDS OF TABLE it_kbn_matinfo1
                FROM pkhd AS t1
                INNER JOIN pkps AS t2
                  ON t2~pknum = t1~pknum
             WHERE t1~prvbe = p_prvbe
                AND   t1~matnr LIKE w_matnr
                AND   pkbst    <>  p_pkbst    " Current Status
*                   and   PKBSA    =  P_PKBST    " Previous Status
                AND   pkkey <> p_pkkey .
      LOOP AT   it_kbn_matinfo1 INTO wa_matinfo.
        IF NOT ( wa_matinfo-pkbsa IS INITIAL OR
               wa_matinfo-pkbsa EQ '1' ).
          DELETE it_kbn_matinfo1 WHERE pkkey EQ wa_matinfo-pkkey AND
                                       pkbsa <>  p_pkbst .
        ENDIF.
      ENDLOOP.
    ELSE.
      SELECT t1~pknum t1~ablad t2~pkkey
                INTO CORRESPONDING FIELDS OF TABLE it_kbn_matinfo1
                FROM pkhd AS t1
                INNER JOIN pkps AS t2
                  ON t2~pknum = t1~pknum
             WHERE t1~prvbe = p_prvbe
                AND   t1~matnr LIKE w_matnr
                AND   pkbst    <>  p_pkbst    " Current Status
*                  and   PKBSA    <>  P_PKBST    " Previous Status
                AND   pkkey <> p_pkkey .
      LOOP AT   it_kbn_matinfo1 INTO wa_matinfo.
        IF NOT ( wa_matinfo-pkbsa IS INITIAL OR
                wa_matinfo-pkbsa EQ '1' ).

          DELETE it_kbn_matinfo1 WHERE pkkey EQ wa_matinfo-pkkey AND
                                       pkbsa <>  p_pkbst .
        ENDIF.
      ENDLOOP.
*      delete it_kbn_matinfo1 where PKBSA <> P_PKBST .
    ENDIF.

    LOOP AT it_kbn_matinfo1 INTO wa_matinfo.
      CLEAR p_barco.
      CONCATENATE wa_matinfo-pkkey p_pkbst INTO p_barco.
      WRITE p_behmg TO  p_qty.

** Changed by Furong on 05/19/09

      SELECT SINGLE labst INTO l_labst
        FROM mard
        WHERE matnr = p_matnr
          AND werks = pkhd-pkumw
          AND lgort = pkhd-umlgo.

      IF l_labst < pkhd-behmg.
        ztmmkrfk-rfsta = 'E'.
        CONCATENATE 'The Quantity available is less than'
           'the Kanban quantity. Please notify PC.' INTO  ztmmkrfk-semsg
           SEPARATED BY space.
        p_semsg = ztmmkrfk-semsg.
        APPEND ztmmkrfk.
        EXIT.
      ENDIF.
** End of change

* Call Function Module to change Status

      CALL FUNCTION 'ZFMMMRFK'
           EXPORTING
                p_barco = p_barco
                p_menge = p_qty
                p_uname = sy-uname
           IMPORTING
                p_pkkey = ztmmkrfk-pkkey
                p_pkbst = ztmmkrfk-pkbst
                p_rfsta = ztmmkrfk-rfsta
                p_semsg = ztmmkrfk-semsg.
      APPEND ztmmkrfk.
      IF ztmmkrfk-rfsta EQ 'S'.
*        commit work.  " to Release locks
        EXIT.
      ELSE.
        CLEAR ztmmkrfk.
      ENDIF.
    ENDLOOP.


  ENDIF.

ENDFUNCTION.
