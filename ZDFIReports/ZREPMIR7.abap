*&---------------------------------------------------------------------*
*&                                                                     *
*& Report  ZREPMIR7                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
* Change history
* 15.02.01: Version 0, Responsible: KlMe -> EnGe, note 382797
* 12.12.02: Version 1, Changed by : SvSi, add document summarization
* 02.04.03: Version 2, Changed by : EnGe, insert c_rbstat_rele_pa
* 13.01.04: Version 3, Changed by : SvSi, no PO check for zerolines
* 07.12.04: Version 4, Changed by : InTo, add case RBKP_BLOCKED

REPORT  zrepmir7.

TABLES: rbkp.

SELECT-OPTIONS:

  so_belnr FOR rbkp-belnr,
  so_gjahr FOR rbkp-gjahr.

IF sy-saprl GE '46C'.

  PARAMETERS: p_update TYPE c.

  TYPES: BEGIN OF header,
          belnr         TYPE belnr_d,
          gjahr         TYPE gjahr,
          rbstat        TYPE rbstat,
          x_no_po       TYPE c,              " no PO history
          x_no_fi       TYPE c,              " no FI follow-on docs
          x_blocked_found TYPE c,
          x_doc_sum     TYPE c,
          text(40)      TYPE c,
         END OF   header.

  TYPES: trbkp          TYPE TABLE OF rbkp,
         trseg          TYPE TABLE OF rseg.

  DATA: tab_rbkp        TYPE TABLE OF rbkp,
        tab_rbkp_blocked TYPE TABLE OF rbkp_blocked,
        tab_rbkp_blocked_delete TYPE TABLE OF rbkp_blocked,
        tab_rseg        TYPE TABLE OF rseg,
        tab_rbco        TYPE TABLE OF rbco,
        tab_ekbe        TYPE TABLE OF ekbe,
        tab_ekbz        TYPE TABLE OF ekbz,
        tab_bkpf        TYPE TABLE OF bkpf,
        tab_header      TYPE TABLE OF header,
        tab_ekbeh       TYPE TABLE OF ekbeh,
        tab_ekbzh       TYPE TABLE OF ekbzh.

  DATA: s_rbkp          TYPE rbkp,
        s_rbkp_blocked  TYPE rbkp_blocked,
        s_rbkpb         TYPE rbkpb,
        s_rseg          TYPE rseg,
        s_rbco          TYPE rbco,
        s_ekbe          TYPE ekbe,
        s_ekbz          TYPE ekbz,
        s_bkpf          TYPE bkpf,
        s_header        TYPE header,
        s_ekbeh         TYPE ekbeh,
        s_ekbzh         TYPE ekbzh.

  DATA: f_lines_rbkp    TYPE i,
        f_lines_rseg    TYPE i,
        f_lines_delete  TYPE i,
        f_awkey         TYPE awkey,
        f_new_rbstat    TYPE rbstat,
        f_no_po_item_expected TYPE c,
        f_do_not_check TYPE c.

  DATA: c_rbstat_parked TYPE rbstat VALUE 'A',
        c_rbstat_comple TYPE rbstat VALUE 'B',
        c_rbstat_saved_pa TYPE rbstat VALUE 'C',
        c_rbstat_saved_po TYPE rbstat VALUE 'D',
        c_rbstat_rele_pa TYPE rbstat VALUE 'E',
        c_rbstat_posted TYPE rbstat VALUE '5',
        c_vgabe_parked  TYPE vgabe  VALUE 'P',
        c_bewtp_parked  TYPE bewtp  VALUE 'T',
        c_koart_initial TYPE koart  VALUE ' ',
        c_awtyp_rmrp    TYPE awtyp  VALUE 'RMRP'.

  SELECT * FROM  rbkp INTO TABLE tab_rbkp
         WHERE   belnr IN so_belnr AND
                 gjahr IN so_gjahr AND
               ( rbstat = c_rbstat_posted OR
                 rbstat = c_rbstat_rele_pa OR
                 rbstat = c_rbstat_parked OR
                 rbstat = c_rbstat_comple ).

  DESCRIBE TABLE tab_rbkp LINES f_lines_rbkp.
  IF sy-subrc = 0.
    SELECT * FROM  rseg INTO TABLE tab_rseg
           FOR ALL ENTRIES IN tab_rbkp
           WHERE ( gjahr = tab_rbkp-gjahr AND
                   belnr = tab_rbkp-belnr ).
  ENDIF.

  DESCRIBE TABLE tab_rseg LINES f_lines_rseg.

  IF sy-subrc = 0.
    SELECT * FROM  rbco INTO TABLE tab_rbco
           FOR ALL ENTRIES IN tab_rseg
           WHERE ( gjahr = tab_rseg-gjahr AND
                   belnr = tab_rseg-belnr AND
                   koart = c_koart_initial ).

    SELECT * FROM rbkp_blocked INTO TABLE tab_rbkp_blocked
           FOR ALL ENTRIES IN tab_rbkp
           WHERE belnr = tab_rbkp-belnr AND
                 gjahr = tab_rbkp-gjahr.

    SELECT * FROM  ekbe INTO TABLE tab_ekbe
           FOR ALL ENTRIES IN tab_rseg
           WHERE ( ebeln = tab_rseg-ebeln AND
                   ebelp = tab_rseg-ebelp AND
                   gjahr = tab_rseg-gjahr AND
                   belnr = tab_rseg-belnr ).

    SELECT * FROM  ekbz INTO TABLE tab_ekbz
           FOR ALL ENTRIES IN tab_rseg
           WHERE ( ebeln = tab_rseg-ebeln AND
                   ebelp = tab_rseg-ebelp AND
                   gjahr = tab_rseg-gjahr AND
                   belnr = tab_rseg-belnr ).

    SELECT * FROM  ekbeh INTO TABLE tab_ekbeh
          FOR ALL ENTRIES IN tab_rseg
          WHERE ( ebeln = tab_rseg-ebeln AND
                  ebelp = tab_rseg-ebelp AND
                  gjahr = tab_rseg-gjahr AND
                  belnr = tab_rseg-belnr ).

    SELECT * FROM  ekbzh INTO TABLE tab_ekbzh
          FOR ALL ENTRIES IN tab_rseg
          WHERE ( ebeln = tab_rseg-ebeln AND
                  ebelp = tab_rseg-ebelp AND
                  gjahr = tab_rseg-gjahr AND
                  belnr = tab_rseg-belnr ).
  ENDIF.

* For better performance: sort all tables
  SORT tab_rbkp_blocked BY gjahr belnr.
  SORT tab_rseg BY gjahr belnr.
  SORT tab_ekbe BY gjahr belnr.
  SORT tab_ekbz BY gjahr belnr.
  SORT tab_ekbeh BY gjahr belnr.
  SORT tab_ekbzh BY gjahr belnr.
* sort tab_rbco by belnr gjahr buzei.

*    Check: follow-on documents missing?
  LOOP AT tab_rbkp INTO s_rbkp.
    CLEAR s_header.
    READ TABLE tab_rseg INTO s_rseg WITH KEY
                 gjahr = s_rbkp-gjahr
                 belnr = s_rbkp-belnr
                 BINARY SEARCH.
    CHECK sy-subrc = 0.
* do not check PO history for posted documents with zerolines
    IF s_rseg-menge = 0 AND
       s_rseg-wrbtr = 0 AND
       s_rbkp-rbstat = c_rbstat_posted.
      f_do_not_check = 'X'.
    ENDIF.

    IF f_do_not_check EQ space.

*    1. PO summarization existing?
      READ TABLE tab_ekbeh INTO s_ekbeh WITH KEY
                     gjahr = s_rbkp-gjahr
                     belnr = s_rbkp-belnr
                     BINARY SEARCH.
      IF sy-subrc EQ 0.
*    ... Document summarization in EKBEH
        MOVE-CORRESPONDING s_rbkp TO s_header.
        s_header-x_doc_sum = 'X'.
      ELSE.
        READ TABLE tab_ekbzh INTO s_ekbzh WITH KEY
                     gjahr = s_rbkp-gjahr
                     belnr = s_rbkp-belnr
                     BINARY SEARCH.
        IF sy-subrc EQ 0.

*    ... Document summarization in EKBZH
          MOVE-CORRESPONDING s_rbkp TO s_header.
          s_header-x_doc_sum = 'X'.
        ENDIF.
      ENDIF.
*    PO history existing?
      IF s_header-x_doc_sum IS INITIAL.
        READ TABLE tab_ekbe INTO s_ekbe WITH KEY
                  gjahr = s_rbkp-gjahr
                  belnr = s_rbkp-belnr
                  BINARY SEARCH.
        IF sy-subrc NE 0.
          READ TABLE tab_ekbz INTO s_ekbz WITH KEY
                       gjahr = s_rbkp-gjahr
                       belnr = s_rbkp-belnr
                       BINARY SEARCH.
          IF sy-subrc NE 0.
            MOVE-CORRESPONDING s_rbkp TO s_header.
            s_header-x_no_po = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR f_do_not_check.

*    Check FI Document only if there are real PO items
    LOOP AT tab_rseg INTO s_rseg WHERE
                 gjahr = s_rbkp-gjahr AND
                 belnr = s_rbkp-belnr AND
                 wrbtr NE 0.
      EXIT.
    ENDLOOP.

    IF sy-subrc NE 0.
      CLEAR s_header.
    ELSE.
      MOVE: s_rbkp-belnr TO f_awkey(10),
            s_rbkp-gjahr TO f_awkey+10(4).

      SELECT SINGLE * FROM bkpf INTO s_bkpf
        WHERE awtyp = 'RMRP '
          AND awkey = f_awkey.

      IF sy-subrc <> 0.
        MOVE-CORRESPONDING s_rbkp TO s_header.
        s_header-x_no_fi = 'X'.
      ENDIF.
    ENDIF.

* Entry in RBKP_BLOCKED existing?
    IF s_header-x_no_fi = 'X'.
      READ TABLE tab_rbkp_blocked INTO s_rbkp_blocked
                               WITH KEY gjahr = s_rbkp-gjahr
                                        belnr = s_rbkp-belnr
                               BINARY SEARCH.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING s_rbkp TO s_header.
        s_header-x_blocked_found = 'X'.
      ENDIF.
    ENDIF.

    IF NOT s_header IS INITIAL.
      APPEND s_header TO tab_header.
    ENDIF.

  ENDLOOP.

*    Update
  IF p_update = 'X'.
    LOOP AT tab_header INTO s_header.
      CASE s_header-x_no_po.
        WHEN 'X'.
          CASE s_header-x_no_fi.
            WHEN 'X'.
*    PO history not existing, Acc. follow-on not existing
*      > Invoice blocking reason has to be deleted, otherwise
*        short dump occurs
              REFRESH tab_rbkp_blocked_delete.
              READ TABLE tab_rbkp_blocked INTO s_rbkp_blocked
                        WITH KEY gjahr = s_header-gjahr
                                 belnr = s_header-belnr
                        BINARY SEARCH.
              IF sy-subrc = 0.
                APPEND s_rbkp_blocked TO tab_rbkp_blocked_delete.
              ENDIF.
              DESCRIBE TABLE tab_rbkp_blocked_delete
                LINES f_lines_delete.
              IF f_lines_delete > 0.
                DELETE rbkp_blocked FROM TABLE tab_rbkp_blocked_delete.
                IF sy-subrc NE 0.
                  s_header-text = 'Error when updating RBKP_BLOCKED'.
                  MODIFY tab_header FROM s_header.
                  EXIT.
                ENDIF.
              ENDIF.
*      > Set status to Hold (C or D)
              IF s_header-rbstat = c_rbstat_parked OR
                 s_header-rbstat = c_rbstat_comple OR
                 s_header-rbstat = c_rbstat_rele_pa.
                .
                f_new_rbstat = c_rbstat_saved_pa.
              ELSEIF s_header-rbstat = c_rbstat_posted.
                f_new_rbstat = c_rbstat_saved_po.
              ENDIF.
*      > Create RBKPB-record if old status is 'Posted'
              PERFORM rbkpb_create USING     s_header
                                             f_new_rbstat
                                             tab_rbkp
                                             tab_rseg
                                   CHANGING  s_rbkpb
                                             s_header-text.
              IF NOT s_header-text IS INITIAL.
                MODIFY tab_header FROM s_header.
                EXIT.
              ENDIF.
              UPDATE rbkp SET rbstat = f_new_rbstat
                WHERE belnr = s_header-belnr AND
                      gjahr = s_header-gjahr.
              IF sy-subrc NE 0.
                s_header-text = 'Error when updating RBKP'.
                MODIFY tab_header FROM s_header.
                EXIT.
              ENDIF.
              IF f_new_rbstat = c_rbstat_saved_po.
                INSERT INTO rbkpb VALUES s_rbkpb.
                IF sy-subrc NE 0.
                  s_header-text = 'Error when inserting RBKPB'.
                  MODIFY tab_header FROM s_header.
                  EXIT.
                ENDIF.
              ENDIF.
              CONCATENATE
                '... updated, new status '
                f_new_rbstat
                INTO s_header-text.
              MODIFY tab_header FROM s_header.

            WHEN OTHERS.
*    PO history not existing, Acc. follow-on existing
*      > Recreate EKBE or EKBZ - only for parked documents
              IF s_header-rbstat = c_rbstat_parked OR
                  s_header-rbstat = c_rbstat_comple.
                READ TABLE tab_rbkp INTO s_rbkp
                    WITH KEY belnr = s_header-belnr
                             gjahr = s_header-gjahr.
                LOOP AT tab_rseg INTO s_rseg
                    WHERE belnr = s_header-belnr AND
                          gjahr = s_header-gjahr.
                  CLEAR: s_ekbe,
                         s_ekbz.
                  IF s_rseg-exkbe = 'X'.
                    MOVE-CORRESPONDING s_rbkp TO s_ekbe.
                    MOVE-CORRESPONDING s_rseg TO s_ekbe.
                    s_ekbe-ernam = s_rbkp-usnam.


                    s_ekbe-vgabe = c_vgabe_parked.
                    s_ekbe-bewtp = c_bewtp_parked.
                    IF s_rseg-knttp IS INITIAL.
                      INSERT INTO ekbe VALUES s_ekbe.
                    ELSE.
                      LOOP AT tab_rbco INTO s_rbco
                          WHERE belnr = s_rseg-belnr AND
                                gjahr = s_rseg-gjahr AND
                                buzei = s_rseg-buzei.
                        MOVE-CORRESPONDING s_rbco TO s_ekbe.
                        INSERT INTO ekbe VALUES s_ekbe.
                      ENDLOOP.
                    ENDIF.
                  ELSE.
                    MOVE-CORRESPONDING s_rbkp TO s_ekbz.
                    MOVE-CORRESPONDING s_rseg TO s_ekbz.
                    s_ekbz-vgabe = c_vgabe_parked.
                    s_ekbz-bewtp = c_bewtp_parked.
                    INSERT INTO ekbz VALUES s_ekbz.
                  ENDIF.
                ENDLOOP.
                s_header-text = '... updated'.
                MODIFY tab_header FROM s_header.
              ENDIF.
          ENDCASE.

        WHEN OTHERS.
          CASE s_header-x_no_fi.
            WHEN 'X'.
*    PO history existing, Acc. follow-on not existing
*      > Invoice blocking reason has to be deleted, otherwise
*        short dump occurs
              REFRESH tab_rbkp_blocked_delete.
              READ TABLE tab_rbkp_blocked INTO s_rbkp_blocked
                        WITH KEY gjahr = s_header-gjahr
                                 belnr = s_header-belnr
                                 BINARY SEARCH.
              IF sy-subrc = 0.
                APPEND s_rbkp_blocked TO tab_rbkp_blocked_delete.
              ENDIF.
              DESCRIBE TABLE tab_rbkp_blocked_delete
                LINES f_lines_delete.
              IF f_lines_delete > 0.
                DELETE rbkp_blocked FROM TABLE tab_rbkp_blocked_delete.
                IF sy-subrc NE 0.
                  s_header-text = 'Error when updating RBKP_BLOCKED'.
                  MODIFY tab_header FROM s_header.
                  EXIT.
                ENDIF.
              ENDIF.
*      > Set status to Hold (C or D) if
*        there is no document summarization
              IF s_header-x_doc_sum IS INITIAL.
                IF s_header-rbstat = c_rbstat_parked OR
                   s_header-rbstat = c_rbstat_comple OR
                   s_header-rbstat = c_rbstat_rele_pa.
                  f_new_rbstat = c_rbstat_saved_pa.
                ELSEIF s_header-rbstat = c_rbstat_posted.
                  f_new_rbstat = c_rbstat_saved_po.
                ENDIF.
*      > Create RBKPB-record if old status is 'Posted'
                PERFORM rbkpb_create USING     s_header
                                               f_new_rbstat
                                               tab_rbkp
                                               tab_rseg
                                     CHANGING  s_rbkpb
                                               s_header-text.
                IF NOT s_header-text IS INITIAL.
                  MODIFY tab_header FROM s_header.
                  EXIT.
                ENDIF.
                UPDATE rbkp SET rbstat = f_new_rbstat
                  WHERE belnr = s_header-belnr AND
                        gjahr = s_header-gjahr.
                IF sy-subrc NE 0.
                  s_header-text = 'Error when updating RBKP'.
                  MODIFY tab_header FROM s_header.
                  EXIT.


                ENDIF.
                IF f_new_rbstat = c_rbstat_saved_po.
                  INSERT INTO rbkpb VALUES s_rbkpb.
                  IF sy-subrc NE 0.
                    s_header-text = 'Error when inserting RBKPB'.
                    MODIFY tab_header FROM s_header.
                    EXIT.
                  ENDIF.
                ENDIF.
*      > Delete EKBE/EKBZ
                LOOP AT tab_rseg INTO s_rseg
                    WHERE belnr = s_header-belnr AND
                          gjahr = s_header-gjahr.
                  DELETE FROM ekbe WHERE ebeln = s_rseg-ebeln AND
                                         ebeln = s_rseg-ebeln AND
                                         belnr = s_rseg-belnr AND
                                         gjahr = s_rseg-gjahr AND
                                         buzei = s_rseg-buzei.
                ENDLOOP.
                CONCATENATE
                       '... updated, new status '
                       f_new_rbstat
                  INTO s_header-text.
                MODIFY tab_header FROM s_header.
              ENDIF.
            WHEN OTHERS.
*    PO history existing, Acc. follow-on existing   > no update
          ENDCASE.
      ENDCASE.
    ENDLOOP.
  ENDIF.

*    Log output
  WRITE: / 'BELNR      GJAHR  RBSTAT  No PO  No FI  BLOCKED  DOC SUM'.
  SORT tab_header BY gjahr belnr.
  LOOP AT tab_header INTO s_header.
    WRITE:
      / s_header-belnr,
      s_header-gjahr,
      ' ',
      s_header-rbstat,
      '     ',
      s_header-x_no_po,

      '    ',
      s_header-x_no_fi,
      '    ',
      s_header-x_blocked_found,
      '    ',
      s_header-x_doc_sum,
      '       ',
      s_header-text.
  ENDLOOP.
  IF p_update = ' '.
    READ TABLE tab_header INTO s_header
       WITH KEY x_no_fi = 'X'.
    IF sy-subrc = 0.
      WRITE: /, /,
           / 'Attention: ',
           / 'This report checks only missing FI documents, not other',
             'accounting follow-on documents. If there is a missing ',
             'FI document please check if there are other Accounting',
             'follow-on documents. If you find one please do not run ',
             'this report in update mode for this invoice.'.
    ENDIF.
  ENDIF.
ELSE.
  WRITE: 'This report is only for R/3 Release above 4.6B'.
  EXIT.
ENDIF.

*&                                                                     *
*&      Form  rbkpb_create
*&                                                                     *
*       text
*                                                                      *

FORM rbkpb_create USING    i_header      TYPE header
                           i_new_rbstat  TYPE rbstat
                           ti_rbkp       TYPE trbkp
                           ti_rseg       TYPE trseg
                  CHANGING e_rbkpb       TYPE rbkpb
                           e_text        TYPE text40.

  DATA: s_rbkp TYPE rbkp,
        s_rseg TYPE rseg.

  CHECK i_header-rbstat = c_rbstat_posted.
  CLEAR e_rbkpb.
  READ TABLE ti_rbkp INTO s_rbkp
      WITH KEY belnr = s_header-belnr
               gjahr = s_header-gjahr.

  IF sy-subrc NE 0.
    s_header-text = 'Error when creating RBKPB record'.
    MODIFY tab_header FROM s_header.
    EXIT.
  ENDIF.
  MOVE-CORRESPONDING s_header TO e_rbkpb.
  LOOP AT tab_rseg INTO s_rseg
      WHERE belnr = s_rbkp-belnr AND
            gjahr = s_rbkp-gjahr.
    IF ( s_rbkp-xrech = 'X' AND s_rseg-shkzg = 'S' ) OR
       ( s_rbkp-xrech = space AND s_rseg-shkzg = 'H' ).
      e_rbkpb-rpzieln = e_rbkpb-rpzieln + s_rseg-wrbtr.
    ELSE.
      e_rbkpb-rpzieln = e_rbkpb-rpzieln - s_rseg-wrbtr.
    ENDIF.
    IF s_rseg-exkbe = 'X'.
      e_rbkpb-xware = 'X'.
    ELSEIF s_rseg-xekbz = 'X'.
      e_rbkpb-xbnk = 'X'.
    ENDIF.
  ENDLOOP.
  e_rbkpb-rbstat = i_new_rbstat.
  e_rbkpb-anzrpo = 1.
  e_rbkpb-xkorrekt = 'X'.
  e_rbkpb-xzuordli = 'X'.
  e_rbkpb-xzuordrt = 'X'.
  e_rbkpb-xrechl = 'S'.
  e_rbkpb-xrechr = 'H'.

ENDFORM.                    " rbkpb_create
