*&-------------------------------------------------------------------*
*& REPORT Z_DISSAGG_EKBE
*&-------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
*&---------------------------------------------------------------------*
*& Report  Z_EKBEAUFL                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*& disaggregates EKBE-records for a range of PO-history records,       *
*& which is not possible in transaction ME87                           *
*&---------------------------------------------------------------------*

REPORT  z_ekbeaufl                   .

TABLES: ekbe, ekpo, ekko, ekbz, ekbeh, ekbzh.

SELECT-OPTIONS:
  s_werks   FOR  ekpo-werks,           "plant
  s_ekorg   FOR  ekko-ekorg,           "purch.org
  s_ekgrp   FOR  ekko-ekgrp.           "purch.group
SELECTION-SCREEN BEGIN OF BLOCK ss1 WITH FRAME.

SELECT-OPTIONS:
  s_lifnr   FOR  ekko-lifnr,           "vendor
  s_matnr   FOR  ekpo-matnr,           "material
  s_ebeln   FOR  ekko-ebeln.           "doc-no
PARAMETERS: update AS CHECKBOX DEFAULT ' '.
PARAMETERS: P_ALV  AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK ss1.



*- table of the condensed EKBE-records ---------------------------*
DATA: BEGIN OF xekbe OCCURS 5.
        INCLUDE STRUCTURE ekbe.
DATA: END OF xekbe.
*- table of the condensed EKBZ-records ---------------------------*
DATA: BEGIN OF xekbz OCCURS 5.
        INCLUDE STRUCTURE ekbz.
DATA: END OF xekbz.

*- table of documents which are aggregated successfully --------*
DATA: BEGIN OF it_agg OCCURS 0,
      ebeln LIKE ekpo-ebeln,
      ebelp LIKE ekpo-ebeln.
DATA: END OF it_agg.

*- table of documents which have no aggregated PO-history --------*
DATA: BEGIN OF it_no_agg OCCURS 0,
      ebeln LIKE ekpo-ebeln,
      ebelp LIKE ekpo-ebeln.
DATA: END OF it_no_agg.

*- table of documents which have no PO-history -------------------*
DATA: BEGIN OF it_no_ekbe OCCURS 0,
      ebeln LIKE ekpo-ebeln,
      ebelp LIKE ekpo-ebeln.
DATA: END OF it_no_ekbe.

*- table of documents which are blocked --------------------------*
DATA: BEGIN OF it_po_blocked OCCURS 0,
      ebeln LIKE ekpo-ebeln,
      ebelp LIKE ekpo-ebeln.
DATA: END OF it_po_blocked.


DATA: l_ekkopo LIKE v_ekkopo OCCURS 0 WITH HEADER LINE.

DATA: xactvt LIKE tact-actvt,          "Hilfsfeld Aktivitat
      xactxt(10),                      "Hilfsfeld Aktivitatstext
      xobjekt(10),                     "Hilfsfeld Objekt
      xobjtxt(15),                     "Hilfsfeld Objekttext
      xfldtxt(15).                     "Hilfsfeld Feldtext

* relevant for material ledger
DATA: xmlmaa TYPE ck_ml_maac.
DATA: wwo TYPE cki_wwo_ml.

START-OF-SELECTION.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE l_ekkopo
    FROM ekko INNER JOIN ekpo
           ON ekko~ebeln = ekpo~ebeln
    WHERE ekko~ebeln IN s_ebeln
      AND ekko~lifnr IN s_lifnr
      AND ekpo~matnr IN s_matnr
      AND ekko~ekorg IN s_ekorg
      AND ekko~ekgrp IN s_ekgrp
      AND ekpo~werks IN s_werks.



  REFRESH: it_agg, it_no_ekbe, it_no_agg, it_po_blocked.

* loop over selected documents
  LOOP AT l_ekkopo.
    CLEAR: ekbe.
    SELECT ebeln ebelp
           INTO (ekbe-ebeln, ekbe-ebelp)
           FROM ekbe
          WHERE ebeln  EQ l_ekkopo-ebeln
          AND   ebelp  EQ l_ekkopo-ebelp
    GROUP by ebeln ebelp.
    ENDSELECT.

    IF sy-subrc NE 0.
      it_no_ekbe-ebeln = l_ekkopo-ebeln.
      it_no_ekbe-ebelp = l_ekkopo-ebelp.
      APPEND it_no_ekbe.
      CONTINUE.
    ENDIF.

* authority check
    xobjekt = 'M_LPET_WRK'.
    xfldtxt = text-004.
    xactvt  = '65'.
    xactxt  = text-005.
    xobjtxt = text-006.
    AUTHORITY-CHECK OBJECT xobjekt
           ID 'ACTVT' FIELD xactvt
           ID 'WERKS' FIELD l_ekkopo-werks.
    IF sy-subrc <> 0.
      MESSAGE e199(06) WITH xobjtxt xactxt xfldtxt ekpo-werks.
    ENDIF.
    REFRESH: xekbe, xekbz.


    SELECT SINGLE * FROM ekbe                     "check if
                     WHERE ebeln EQ ekbe-ebeln    "deletion possible
                     AND   ebelp EQ ekbe-ebelp
                     AND   belnr EQ space.

    IF sy-subrc NE 0.
      it_no_agg-ebeln = ekbe-ebeln.
      it_no_agg-ebelp = ekbe-ebelp.
      APPEND it_no_agg.
      PERFORM unblock USING ekbe-ebeln ekbe-ebelp.
      CONTINUE.
    ENDIF.


* beginning update
    IF update NE space.

* block document
      CALL FUNCTION 'ENQUEUE_EMEKPOE'
           EXPORTING
                ebeln          = ekbe-ebeln
                ebelp          = ekbe-ebelp
           EXCEPTIONS
                foreign_lock   = 2
                system_failure = 3.

      IF sy-subrc NE 0.
        it_po_blocked-ebeln = ekbe-ebeln.
        it_po_blocked-ebelp = ekbe-ebelp.
        APPEND it_po_blocked.
        PERFORM unblock USING ekbe-ebeln ekbe-ebelp.
        CONTINUE.
      ENDIF.


      DELETE FROM ekbe WHERE ebeln EQ ekbe-ebeln    "delete sum
                       AND   ebelp EQ ekbe-ebelp
                       AND   belnr EQ space.

      DELETE FROM ekbz WHERE ebeln EQ ekbe-ebeln
                    AND   ebelp EQ ekbe-ebelp
                    AND   belnr EQ space.

      SELECT * FROM ekbeh APPENDING TABLE xekbe
                 WHERE ebeln EQ ekbe-ebeln
                   AND ebelp EQ ekbe-ebelp.


      SELECT * FROM ekbzh APPENDING TABLE xekbz
                 WHERE ebeln EQ ekbe-ebeln
                   AND ebelp EQ ekbe-ebelp.



* move data from history table into table
      INSERT ekbe  FROM TABLE xekbe.
      INSERT ekbz  FROM TABLE xekbz.
      DELETE ekbeh FROM TABLE xekbe.
      DELETE ekbzh FROM TABLE xekbz.

* part for material ledger
      SELECT SINGLE * FROM ekko WHERE ebeln EQ ekbe-ebeln.
      SELECT SINGLE * FROM ekpo WHERE ebeln EQ ekbe-ebeln
                              AND ebelp EQ ekbe-ebelp.

      IF ekpo-werks IS INITIAL.
        CLEAR xmlmaa.
      ELSE.
        CALL FUNCTION 'CKML_F_SET_PLANT'
             EXPORTING
                  plant = ekpo-werks.

        CALL FUNCTION 'CKML_F_GET_WWO'
             IMPORTING
                  wwo = wwo.

        IF NOT wwo-mlbwa IS INITIAL.
          xmlmaa = 'X'.
        ELSE.
          CLEAR xmlmaa.
        ENDIF.
      ENDIF.

      IF NOT xmlmaa IS INITIAL.
        CALL FUNCTION 'CKMW_REPLACE_AND_POST_POH'
             EXPORTING
                  i_aufl  = 'X'
                  i_ebeln = ekbe-ebeln
                  i_ebelp = ekbe-ebelp.
      ENDIF.
* end of material ledger

* unblock document
      PERFORM unblock USING ekbe-ebeln ekbe-ebelp.

    ENDIF.        "end of update

* store disaggregated/selected document in internal table
    it_agg-ebeln = ekbe-ebeln.
    it_agg-ebelp = ekbe-ebelp.
    APPEND it_agg.

  ENDLOOP.       "l_ekkopo


CHECK P_ALV IS NOT INITIAL.
* list-output header
  IF update NE space.
    ULINE.
    WRITE: / 'DISAGGREGATION WRITTEN TO DATABASE' COLOR 5.
    ULINE.
  ELSE.
    ULINE.
    WRITE: / 'DISAGGREGATION' COLOR 5, 'NOT' COLOR 6,
             'WRITTEN TO DATABASE' COLOR 5.
    ULINE.
  ENDIF.


* list-output no PO-history
  FORMAT COLOR 1 ON.
  WRITE: /,/,/ 'The following Purchase Orders or Scheduling Agreements',
            'have no PO-history'.
  FORMAT COLOR 1 OFF.
  ULINE.
  WRITE: / 'document     item:'.
  ULINE.
  LOOP AT it_no_ekbe.
    WRITE: / it_no_ekbe-ebeln, '   ',it_no_ekbe-ebelp.
  ENDLOOP.

* list-output no aggregated PO-history
  FORMAT COLOR 1 ON.
  WRITE: /,/,/ 'The following Purchase Orders or Scheduling',
               'Agreements have no aggregated PO-history'.
  FORMAT COLOR 1 OFF.
  ULINE.
  WRITE: / 'document     item: ' .
  ULINE.
  LOOP AT it_no_agg.
    WRITE: / it_no_agg-ebeln, '   ',it_no_agg-ebelp.
  ENDLOOP.

* list-output blocked documents
  FORMAT COLOR 1 ON.
  WRITE: / 'The following Purchase Orders or Scheduling',
           'Agreements are blocked'.
  FORMAT COLOR 1 OFF.
  ULINE.
  WRITE: / 'document     item: ' .
  ULINE.
  LOOP AT it_po_blocked.
    WRITE: / it_po_blocked-ebeln, '   ',it_po_blocked-ebelp.
  ENDLOOP.

* list-output disaggregated documents
  FORMAT COLOR 1 ON.
  IF update NE space.
    WRITE: /,/,/ 'The following Purchase Orders or Scheduling',
                 'Agreements have been disaggregated'.
  ELSE.
    WRITE: /,/,/ 'The following Purchase Orders or Scheduling',
               'Agreements can be disaggregated'.
  ENDIF.
  FORMAT COLOR 1 OFF.
  ULINE.
  WRITE: / 'document     item: ' .
  ULINE.
  LOOP AT it_agg.
    WRITE: / it_agg-ebeln, '   ',it_agg-ebelp.
  ENDLOOP.






*---------------------------------------------------------------------*
*       FORM unblock                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_EBELN                                                       *
*  -->  P_EBELP                                                       *
*---------------------------------------------------------------------*
FORM unblock USING p_ebeln LIKE ekpo-ebeln
                   p_ebelp LIKE ekpo-ebelp.

  CALL FUNCTION 'DEQUEUE_EMEKPOE'
       EXPORTING
            ebeln  = p_ebeln
            ebelp  = p_ebelp
       EXCEPTIONS
            OTHERS = 1.

ENDFORM.
