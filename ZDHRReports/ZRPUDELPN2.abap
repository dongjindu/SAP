* LCP
* XREN631092 062003 PU00/RPUDELPN: IT 0121 Einträge werden nicht
*                   mitgelöscht
* XREN545903 082002 RPDUDELPN/PU00: Pernr. werden nicht aus T5D2M
*                   gelöscht
* YCTAL0K032145 300701 Arbeitszeitflexibilisierung löschen
* ( = L9CK051812 )
* XWSN369825  281200 RPUDELPN: Fehler bei der Auswahl der zu
*                    löschenden Personalnummern
* XWSN362696  301100 RPUDELPN löscht im Testmodus IT 1001-Sätze
* XDPN0358008   1100    Delete CP
* XWSN355855  081100 RPUDELPN / PU00: Altdatenübernahme nicht gelöscht
* XWSN331992  110900 Sicherheitsabfrage nicht in Batchmodus
* XWSN329054  290800 ALE data
* XWSN0212720 060400 Note #0212720
* XWSL9CK003489 310300 DELETE_BAV erweitern
* 4.6C
* XWSL9CK005028 100400 FB RH_DELETE_INFTY ausgetauscht gegen
*                      FB RH_DELETE_INFTY_DIRECT in
*                      FORM UPDATE_INTEGRATION
* XWSPH0K006897 270100 Abfrage auf Original/Replikat beim Löschen pernr
* XWSPH0K005619 190100 P593R-Daten löschen
* XWSN0191305 221299 Note #0191305
* XWSAHRK063486 181199 DEÜV löschen
* XWSAHRK063353 171199 HRMDORIGIN löschen
* YAYAHRK059890 151099 PTQUODED löschen
* 4.6A
* XWSAHRK055570 250899 Personalnummern in der T77INT löschen
* XWSPH9K011503 300699 Note 0159824
* XWSPH9K011363 290699 Note 0159744
* QNZAHRK049811 090499  delete HRPY_RGDIR and HRPY_WPBP
* QNOAHRK036732 15Feb99 neue MC-W-Tab. mitloeschen
* XWSAHRK033018 080199
* usability modifications for test and confirmation
* 4.6A
* QICAHRK029467 19Nov98 ROLLBACK und WRITE im Batch
* 4.0
* QNOP40K025951 07Okt97 TEVEN/TEVEN_MORE loeschen, nie 00000000 setzen
*                       "order by pernr"
* QNOALRK034383 5Jul97 Deco: AFRU -> TEVEN_MORE
* QNOAKRK026276 23Jul97 T582A -> T777D
* 3.0E
* QNOP30K080558 250696 Auch DB in T582A mit "BA.." erkennen;
*  Uebergabe-Tab. für PD leeren; im PD SCHNELL löschen & Sperrproblem
*  TEVEN (qualifiziert) loeschen
* 3.0D
* QNOP30K57428 190396 Keine Daten auf Bewerber-Clustern loeschen
* 3.0C
* QNO 120995 Vollst. neu geschrieben, da ITs in 30 auf PAxxxx statt PREL
* Komplettes vollstaendiges Loeschen der selektierten PerNr's
REPORT rpudelpn MESSAGE-ID rp.
INCLUDE mppref00.                      "Konstanten fuer ReferenzPerNr
INCLUDE ZMPPSEL00.
*INCLUDE mppsel00.                      "first etc.

*
TABLES: rpureoxx,    "Struktur fuer Reportparameter RPUREO*
        t582a,                         "Infotypen "QNOK026276 "N0191305
        t5d46,                                              "YCTK032145
        t5d48,                                              "YCTK032145
*       t777d,                         "Infotypen           "QNOK026276
*       t52b5,       "Welche Cluster muessen mitgeloescht werden
        asshr, assob,
*       afru,  "QNOK034383
*       teven,                                              "QNOK080558
*       pdsnr,       "laufende Nummer für BDE-Meldungen
        prel_db,
*       pskey,
        t000,                                            "QICAHRK029467
        pspar,                                              "XWSK063353
        pcl1.
*       pcalac, ppoix, ppopx.          "Buchungen - RPCIPO00
*       pa0003.                        ", pa1001.

TYPES t_error TYPE i.                  "Error-codes

CONSTANTS: error_lock TYPE t_error VALUE 1,
           error_integration TYPE t_error VALUE 2,
           infty_reference          LIKE pskey-infty VALUE '0031',
           infty_reference_priority LIKE pskey-infty VALUE '0121',
           infty_integration        LIKE pskey-infty VALUE '1001'.

DATA: number_at_a_time TYPE i VALUE 100,"number of PerNrs in 1 comit wrk
      max_selected_pernr LIKE pa0003-pernr,
      number_looped TYPE i,
      lin TYPE i.
*
*
DATA: BEGIN OF all_infotypes OCCURS 50,
        infty LIKE t777d-infty,
        dbtab LIKE t777d-dbtab,                             "QNOK026276
      END OF all_infotypes.

DATA: BEGIN OF all_pernrs OCCURS 100,
        pernr LIKE pernr-pernr,
      END OF all_pernrs.
DATA: pernr_tab TYPE TABLE OF hrpernr,                      "XWSK006897
      number_really_looped TYPE i.                          "XWSK006897

DATA: BEGIN OF pernrs_with_errors OCCURS 10,
        pernr LIKE pernr-pernr,
        error_code TYPE t_error,
      END OF pernrs_with_errors.
* for pafru/asshr
DATA: BEGIN OF asshr_key OCCURS 30.
        INCLUDE STRUCTURE pskey.
DATA END OF asshr_key.

DATA: BEGIN OF pafru_key OCCURS 30.
        INCLUDE STRUCTURE pskey.       "pakey + infty
DATA: END OF pafru_key.
* all cluster
DATA: my_t52b5 LIKE t52b5 OCCURS 10 WITH HEADER LINE.

*ATA: dbtabname(6) TYPE c VALUE 'PA0000'.
DATA: dbtabname LIKE t777d-dbtab.
DATA: test_mode(1) TYPE c.
DATA: numb TYPE i VALUE 300.

DATA: test LIKE rpureoxx-test.                              "XWSK033018
DATA: popup_answer.                                         "XWSK033018
DATA: number_currently_looped TYPE i.                       "XWSN369825
*- selection-screen: ---------------------------------------------------
* Selektion:
SELECTION-SCREEN BEGIN OF BLOCK sel_blck WITH FRAME TITLE f_title.

SELECT-OPTIONS selpernr FOR rpureoxx-lowpernr.
SELECTION-SCREEN END OF BLOCK sel_blck.

* Test-Options:
SELECTION-SCREEN BEGIN OF BLOCK par_block WITH FRAME TITLE f_title2.
PARAMETERS: protocol LIKE rpuxxxxx-kr_feld1 DEFAULT 'X',
            testx LIKE rpuxxxxx-kr_feld1 DEFAULT 'X'.       "XWSK033018
*            test LIKE rpureoxx-test DEFAULT 'ON'.          "XWSK033018
SELECTION-SCREEN END OF BLOCK par_block.

*arameters: numb type i default 30.
*arameters: test_out as checkbox default 'X'.                      "test

* selection-screen end of block sel_blck.
*-----------------------------------------------------------------------
* Main:
*-----------------------------------------------------------------------
INITIALIZATION.

* run program in test system only
*  SELECT SINGLE cccategory FROM t000                     "QICAHRK029467
*                INTO t000-cccategory                     "QICAHRK029467
*                WHERE mandt EQ sy-mandt.                 "QICAHRK029467
*  IF t000-cccategory EQ 'P'.                             "QICAHRK029467
*    MESSAGE e637.                                        "QICAHRK029467
*  ENDIF.                                                 "QICAHRK029467

* authorization check
  CALL FUNCTION 'HR_CHECK_AUTHORITY_INFTY'               "QICAHRK029467
       EXPORTING                                         "QICAHRK029467
          pernr            = '00000000'                  "QICAHRK029467
          infty            = '*'                         "QICAHRK029467
          subty            = '*'                         "QICAHRK029467
          level            = 'W'                         "QICAHRK029467
     EXCEPTIONS                                          "QICAHRK029467
          no_authorization = 1                           "QICAHRK029467
          internal_error   = 2                           "QICAHRK029467
          OTHERS           = 3.                          "QICAHRK029467
  IF sy-subrc NE 0.                                      "QICAHRK029467
    MESSAGE e638.                                        "QICAHRK029467
  ENDIF.                                                 "QICAHRK029467

  f_title  = 'Selektion   '(bp1).
  f_title2 = 'Ablauf      '(bp2).
*
START-OF-SELECTION.
* initialize test according to the checkbox testx.
  CASE testx.                                               "XWSK033018
    WHEN 'X'.                                               "XWSK033018
      test = 'ON'.                                          "XWSK033018
    WHEN ' '.                                               "XWSK033018
      test = 'OFF'.                                         "XWSK033018
  ENDCASE.                                                  "XWSK033018

  IF test EQ 'OFF'.
    IF sy-batch IS INITIAL.                                 "XWSN331992
*   rp-check-password. "rp-check.. contains parameter statement
                                                            "XWSK033018
      CALL FUNCTION 'POPUP_TO_CONFIRM'                      "XWSK033018
           EXPORTING                                        "XWSK033018
                text_question         = text-pqu            "XWSK033018
                text_button_1         = 'Ja'(001)           "XWSK033018
                icon_button_1         = 'ICON_OKAY'         "XWSK033018
                text_button_2         = 'Nein'(002)         "XWSK033018
                icon_button_2         = 'ICON_CANCEL'       "XWSK033018
                default_button        = '2'                 "XWSK033018
                display_cancel_button = ' '                 "XWSK033018
           IMPORTING                                        "XWSK033018
                answer                = popup_answer        "XWSK033018
           EXCEPTIONS                                       "XWSK033018
                text_not_found        = 1                   "XWSK033018
                OTHERS                = 2.                  "XWSK033018
      CASE popup_answer.                                    "XWSK033018
        WHEN '1'.                                           "XWSK033018
          test_mode = ' '.                                  "XWSK033018
        WHEN '2'.                                           "XWSK033018
*        Report wurde abgebrochen                           "XWSK033018
          MESSAGE i016 WITH text-abr.                       "XWSK033018
          EXIT.                                             "XWSK033018
      ENDCASE.                                              "XWSK033018
    ENDIF.                                                  "XWSN331992
  ELSE.
    test_mode = 'X'.
  ENDIF.
*
  number_at_a_time = numb.
  PERFORM init_program.

* get all pernrs to be deleted: number_at_a_time at a time;
* we use a current (sub-)set of PreNrs to be deleted
* the subset is found in all_pernrs
  PERFORM get_more_pernrs TABLES all_pernrs
                           USING number_looped max_selected_pernr.
  number_currently_looped = sy-dbcnt.                       "XWSN369825
  IF sy-dbcnt > 0.
    IF protocol = 'X'.
      FORMAT COLOR COL_HEADING.
      WRITE: /21(60) 'Liste der bearbeiteten Personalnummern:    '(lst).
      FORMAT RESET.
    ENDIF.
  ELSE.
* no PerNrs found:
    IF sy-binpt EQ space.              "for CATT
*      message i016 with text-non.                          "QNOK080558
      MESSAGE i050(pn).                                     "QNOK080558
      STOP.
    ELSE.                              "for ONLINE
      MESSAGE e050(pn).                                     "QNOK080558
*      message e016 with                                    "QNOK080558
*           'Es wurden keine Personalnummern selektiert   '(non).
    ENDIF.
  ENDIF.

* start insert block ------------------------------------>  "XWSK006897
  REFRESH pernr_tab.
  APPEND LINES OF all_pernrs TO pernr_tab.
  CALL FUNCTION 'RH_CHECK_PERNRTAB_ORIG_DIALOG'
*   EXPORTING
*     OTYPE                    = 'P'
*     MAX_LIST                 = 100
    TABLES
      pernr_tab                = pernr_tab
*     REJECTED_PERNR_TAB       =
    EXCEPTIONS
      cancel                   = 1
      OTHERS                   = 2.
  CASE sy-subrc.
    WHEN 1.
      MESSAGE i016 WITH text-abr.
      EXIT.
    WHEN 2.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDCASE.
  REFRESH all_pernrs.
  APPEND LINES OF pernr_tab TO all_pernrs.
* end insert block -------------------------------------->  "XWSK006897

* loop while there still are PerNrs:
* WHILE SY-DBCNT > 0.                  "at all PerNr's      "XWSN369825
  WHILE number_currently_looped > 0.                        "XWSN369825
    PERFORM enqueue_pernr TABLES all_pernrs.
    DESCRIBE TABLE all_pernrs LINES lin.
    IF lin > 0.
      PERFORM update_reference TABLES all_pernrs.
      PERFORM update_reference_priority TABLES all_pernrs.
*     cp must be delete after  update_reference and         "XDPN0358008
*                       before integration                  "XDPN0358008
      PERFORM delete_cp TABLES all_pernrs.                  "XDPN0358008
      PERFORM delete_cluster TABLES all_pernrs.
      PERFORM delete_postings TABLES all_pernrs.
      PERFORM delete_ptrv TABLES all_pernrs.                "N0159744
      PERFORM delete_bav TABLES all_pernrs.                 "N0159824
      PERFORM update_integration TABLES all_pernrs.
      PERFORM delete_teven TABLES all_pernrs.               "QNOK080558
      PERFORM delete_t77int TABLES all_pernrs.           "XWSAHRK055570
      PERFORM delete_ptquoded TABLES all_pernrs.         "YAYAHRK059890
      PERFORM delete_deuv TABLES all_pernrs.                "XWSK063486
      PERFORM delete_skv TABLES all_pernrs.              "QCMPH0K000688
      PERFORM delete_p593r TABLES all_pernrs.               "XWSK005619
      PERFORM change_pointers TABLES all_pernrs.            "XWSN329054
      PERFORM delete_payroll_legacy_data TABLES all_pernrs. "XWSN355855
      PERFORM delete_svwg TABLES all_pernrs.                "YCTK032145
      PERFORM delete_tax_certi TABLES all_pernrs.           "XREN545903
*
      LOOP AT all_infotypes.
*        dbtabname+2(4) = all_infotypes-infty.
        dbtabname = all_infotypes-dbtab.
*       if test_out = 'X'. write: / dbtabname. endif.         "test

        PERFORM del_assob_pafru TABLES all_pernrs
                                USING  dbtabname all_infotypes-infty.
*
        LOOP AT all_pernrs.
          IF protocol = 'X' AND all_infotypes-infty = '0003'.
            WRITE: / all_pernrs.
          ENDIF.
          IF test_mode = 'X'.
            SELECT * FROM (dbtabname) INTO prel_db
                   WHERE pernr = all_pernrs-pernr.
*             if test_out = 'X'. write prel_db. endif.          "test
            ENDSELECT.
          ELSE.                        "delete infotype
            DELETE FROM (dbtabname) WHERE pernr = all_pernrs-pernr.
          ENDIF.
        ENDLOOP.                       "at all_pernrs
      ENDLOOP.
      PERFORM delete_hrmdorigin TABLES all_pernrs.          "XWSK063353
      IF NOT test_mode = 'X'.
        COMMIT WORK.
*     else.                                         (del) QICAHRK029467
*       rollback work.                              (del) QICAHRK029467
      ENDIF.
    ENDIF.
    PERFORM dequeue_pernr TABLES all_pernrs.
* fill all_pernrs
    PERFORM get_more_pernrs TABLES all_pernrs
                            USING number_looped max_selected_pernr.
    number_currently_looped = sy-dbcnt.                     "XWSN369825
  ENDWHILE.

*  PERFORM END USING NUMBER_LOOPED.                          "XWSK006897
  PERFORM end USING number_really_looped.                   "XWSK006897

************************************************************************
* Forms:
************************************************************************
*---------------------------------------------------------------------*
* INIT_PROGRAM
*---------------------------------------------------------------------*
* set initial values; called once
*---------------------------------------------------------------------*
FORM init_program.
  DATA: no_int(1) TYPE c.                                   "QNOK080558
*
  number_looped = 0.                   "number of PerNrs deleted
* get all infotypes for employees (NOT applicants)
* SELECT infty INTO TABLE all_infotypes FROM t582a          "QNOK026276
*               where dbidn like 'A%'                       "QNOK080558
*              WHERE dbidn LIKE '%A%'                       "QNOK026276
  SELECT infty dbtab                                        "QNOK026276
         INTO CORRESPONDING FIELDS OF TABLE all_infotypes   "QNOK026276
               FROM t777d                                   "QNOK026276
               WHERE dbtab <> space                         "QNOK026276
               AND   papd = 'X'                             "QNOK026276
* these inftypes are handled separately:
               AND   infty <> infty_reference
               AND   infty <> infty_reference_priority
               AND   infty <> infty_integration.
* get all clusters
  SELECT * INTO TABLE my_t52b5 FROM t52b5
           WHERE potyp = 'CLST'        "all cluster
           AND   pattr = 'PDP'         "Personalnummerabhaengig
           AND   pwert = '1'           "have key at front of cluster
           AND   NOT ponam LIKE 'C3%'  "no applicants         "QNOK57428
           AND   NOT ponam = 'C1TY'    "applicant-text, not in tab."...
           AND   NOT ponam = 'C4LB'                         "QNOK57428
           AND   NOT ponam = 'C4SB'.                        "QNOK57428
* the same for Cluster TX in PCL1, which is not in t52b5 since it is
* not supposed to be deleted by itself
  my_t52b5-potyp = 'CLST'.
  my_t52b5-pattr = 'PDP'.
  my_t52b5-pwert = '1'.
  my_t52b5-ponam = 'C1TX'.
  APPEND my_t52b5.
* set switch for integration:
*   this is to inform PD that no enqueue is necessary
  no_int = 'X'.                                             "QNOK080558
  EXPORT no_int TO MEMORY ID 'INTERPRH'.                    "QNOK080558
ENDFORM.                               " init_program.

*---------------------------------------------------------------------*
*       FORM GET_MORE_PERNRS                                          *
*---------------------------------------------------------------------*
*       read next max_selected_pernr Pernrs into p_pernrs_tab         *
*---------------------------------------------------------------------*
*  <--  P_NUMBER       # of PerNrs read
*  <--  p_pernrs_tab   table with new PerNrs
*  <->  p_max_pernr    highest PerNr selected
*---------------------------------------------------------------------*
FORM get_more_pernrs TABLES p_pernrs STRUCTURE all_pernrs
                     USING p_number TYPE i
                           p_max_pernr LIKE pernr-pernr.

  DATA : selpernr_lines TYPE i.                           "VWMRANGE
  DATA : eq_only(1).                                      "VWMRANGE
  DATA : loc_p_pernrs LIKE p_pernrs OCCURS 0 WITH HEADER LINE. "VWMRANGE

  DESCRIBE TABLE selpernr LINES selpernr_lines.  "VWMRANGE

  IF selpernr_lines LE 200."ARRAY-FETCH MAX. 8K  "VWMRANGE
* Catch - endcatch?
    SELECT pernr INTO TABLE p_pernrs FROM pa0003
                UP TO number_at_a_time ROWS
                WHERE pernr IN selpernr
                  AND pernr > p_max_pernr
                  ORDER BY pernr.                           "QNOK025952
  ELSE.                                           "VWMRANGE
    eq_only = on.
    LOOP AT selpernr.
      IF selpernr-sign NE 'I' OR selpernr-option NE 'EQ'.
        eq_only = off.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF eq_only EQ on.
      SELECT pernr INTO TABLE p_pernrs FROM pa0003
             UP TO number_at_a_time ROWS
             FOR ALL ENTRIES IN selpernr  "VWMRANGE
             WHERE pernr = selpernr-low
                   AND pernr > p_max_pernr.
      SORT p_pernrs BY pernr.
    ELSE.
      SELECT pernr INTO TABLE loc_p_pernrs FROM pa0003
             WHERE pernr > p_max_pernr.
      LOOP AT loc_p_pernrs.
        CHECK loc_p_pernrs-pernr IN selpernr.
        APPEND loc_p_pernrs TO p_pernrs.
      ENDLOOP.
    ENDIF.
  ENDIF.                                           "VWMRANGE
  DESCRIBE TABLE p_pernrs LINES sy-dbcnt.          "VWMRANGE
* IF SY-SUBRC <> 0.                                "VWMRANGE
*    SY-DBCNT = 0.                                 "VWMRANGE
  IF sy-dbcnt = 0.                                 "VWMRANGE
    EXIT.
  ENDIF.

  p_number = p_number + sy-dbcnt.


  READ TABLE p_pernrs INDEX sy-dbcnt.
  IF sy-subrc = 0. p_max_pernr = p_pernrs-pernr. ENDIF.

ENDFORM.                               "get_more_pernrs
*---------------------------------------------------------------------*
*       FORM UPDATE_REFERENZ                                          *
*---------------------------------------------------------------------*
*       InfTy 0031   ReferenzPerNrs                                   *
*---------------------------------------------------------------------*
* Kopiert aus sapfp50p
FORM update_reference TABLES p_pernrs STRUCTURE all_pernrs.
*  local: pskey.
  TABLES pa0031.
*
  DATA BEGIN OF del0031 OCCURS 5.    "entries to be deleted from PA0031
          INCLUDE STRUCTURE pa0031.
  DATA END OF del0031.
  DATA BEGIN OF upd0031 OCCURS 5.      "entries to be updated in PA0031
          INCLUDE STRUCTURE pa0031.
  DATA END OF upd0031.
  DATA: l0031 LIKE del0031 OCCURS 5 WITH HEADER LINE.
  DATA: ref_pernr LIKE pa0031-rfp01,
        num TYPE i.
* start: fetch all IT0031 into del0031 (to be deleted)
  SELECT * FROM pa0031 INTO TABLE del0031
      FOR ALL ENTRIES IN p_pernrs WHERE pernr = p_pernrs-pernr.
  CHECK sy-subrc = 0. "continue only if there are IT0031's
  l0031[] = del0031[].
  LOOP AT l0031.  "for all entries that do have a reference
*begda = lowdate & endda = highdate

    DO nrfpn31 TIMES VARYING ref_pernr "nrfpn31=20, max.# of ref. in 31
                       FROM l0031-rfp01 NEXT l0031-rfp02.
*     if test_out = 'X'. write: / ref_pernr, l0031. endif.     "test
      IF ref_pernr = '00000000'. EXIT. ENDIF.     "leave: DO nrfpn31...
* is the reference to a pernr that will be deleted? if so, take the next
      READ TABLE del0031 WITH KEY pernr = ref_pernr.
      IF sy-subrc = 0. CONTINUE. ENDIF.
* is the reference to a pernr that we work with already?
      READ TABLE upd0031 WITH KEY pernr = ref_pernr.
      IF sy-subrc = 0.
        PERFORM remove_one_entry_in_it0031 USING l0031-pernr
                                                 upd0031 num.
        IF num > 0.  "other reference(s) from that PerNr
          MODIFY upd0031 INDEX sy-tabix.
        ELSE.        "there was only one reference; delete this too
          APPEND upd0031 TO del0031.   "num=0, no entry left->del
          DELETE upd0031 INDEX sy-tabix.
        ENDIF.
      ELSE.                            "reference is to "new" PerNr
        SELECT * FROM pa0031 WHERE pernr = ref_pernr.
          EXIT.                        "there should be exactly one
        ENDSELECT.
        IF sy-subrc <> 0.              "error: none found
          CONTINUE.                    "just disregard
        ENDIF.
        PERFORM remove_one_entry_in_it0031 USING l0031-pernr pa0031 num.
        IF num > 0.   "other reference(s) in from that PerNr
          APPEND pa0031 TO upd0031.
        ELSE.                          "there was only one reference
          APPEND pa0031 TO del0031.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDLOOP.

  CHECK test_mode NE 'X'.
* now write the changes to the DB:
  DELETE pa0031 FROM TABLE del0031.
  IF sy-subrc NE 0. WRITE: / 'Fehler bei DELETE IT0031'(u05). ENDIF.
  DESCRIBE TABLE upd0031 LINES num.
  IF num > 0.
    UPDATE pa0031 FROM TABLE upd0031.
    IF sy-subrc NE 0. WRITE: / 'Fehler bei UPDATE IT0031'(d05). ENDIF.
  ENDIF.
*    LOOP AT P_PERNRS.
** get IT0031 of current PerNr into s0031 (stupid)
*      IF PA0031-PERNR EQ P_PERNRS-PERNR.
*        MOVE PA0031 TO S0031.
*      ELSE.
*        PERFORM READ_INFOTYPE(SAPFP50P) USING
*          P_PERNRS-PERNR INFTY_REFERENZ SPACE SPACE SPACE
*          LOW_DATE HIGH_DATE FIRST NOP S0031.
*        IF SY-SUBRC NE 0.
*          EXIT.
*        ENDIF.
*      ENDIF.
** Ueber alle Referenz-PerNrs...
*      DO NRFPN31 TIMES VARYING REF_PERNR
*                       FROM S0031-RFP01 NEXT S0031-RFP02.
** in RefPerNr IT0031 lesen
*        CHECK REF_PERNR NE '00000000'.
*        PERFORM READ_INFOTYPE(SAPFP50P) USING
*           REF_PERNR INFTY_REFERENZ SPACE SPACE SPACE
*           LOW_DATE HIGH_DATE FIRST NOP PA0031.
*        CHECK SY-SUBRC EQ 0.
*        RFP_FOUND = NO.
*        RFP31IDX = NRFPN31 - 1.
*        DO RFP31IDX TIMES VARYING RFPNR1
*           FROM PA0031-RFP01 NEXT PA0031-RFP02
*           VARYING RFPNR2 FROM PA0031-RFP02 NEXT PA0031-RFP03.
*          IF RFPNR1 EQ P_PERNRS-PERNR.
*            RFP_FOUND = YES.
*          ENDIF.
*          CHECK RFP_FOUND EQ YES.
*          MOVE RFPNR2 TO RFPNR1.
*          CLEAR RFPNR2.
*        ENDDO.
*        IF PA0031-RFP01 NE 0.
**        perform update_infotyp() using p0031.
*        ELSE.
*          MOVE PA0031 TO PSKEY.
**          perform delete_ps using pskey.
*        ENDIF.
*      ENDDO.
*    ENDLOOP.
ENDFORM.                               "update_reference
*---------------------------------------------------------------------*
*       FORM REMOVE_ONE_ENTRY_IN_IT0031                               *
*---------------------------------------------------------------------*
*       remove from p_pa0031 entry p_pernr; return number of referenc.*
*       left in return_number (-1 -> none left & no one in before)    *
*---------------------------------------------------------------------*
FORM remove_one_entry_in_it0031 USING value(p_pernr) LIKE pa0031-rfp01
                   p_pa0031 STRUCTURE pa0031
                   number_left TYPE i. "# of references left in IT31
  DATA: found LIKE yes,
        max_minus_one LIKE nrfpn31,
        ref_pernr_1 LIKE p0031-rfp01,
        ref_pernr_2 LIKE p0031-rfp01.
*
  found = no.
  max_minus_one = nrfpn31 - 1.
  number_left = 0.
  DO max_minus_one TIMES VARYING ref_pernr_1
     FROM p_pa0031-rfp01 NEXT p_pa0031-rfp02
     VARYING ref_pernr_2 FROM p_pa0031-rfp02 NEXT p_pa0031-rfp03.
    number_left = number_left + 1.
    IF ref_pernr_1 EQ p_pernr. found = yes. ENDIF.
    IF found = yes. MOVE ref_pernr_2 TO ref_pernr_1. ENDIF.
    IF ref_pernr_1 = '00000000'. EXIT. ENDIF. "end of list reached
  ENDDO.
  IF found = yes.
    number_left = number_left - 1.
    ref_pernr_2 = '00000000'.
  ENDIF.
ENDFORM.                               "remove_one_entry_in_it0031
*---------------------------------------------------------------------*
*       FORM UPDATE_REFERENCE_PRIORITY                                *
*---------------------------------------------------------------------*
*       InfTy 0121   ReferenzPerNrsPriority                           *
*---------------------------------------------------------------------*
* Kopiert aus sapfp50p
FORM update_reference_priority TABLES p_pernrs STRUCTURE all_pernrs.
*  local: pskey.
  TABLES pa0121.
*
  DATA BEGIN OF del0121 OCCURS 5.    "entries to be deleted from PA0121
          INCLUDE STRUCTURE pa0121.
  DATA END OF del0121.
  DATA BEGIN OF upd0121 OCCURS 5.      "entries to be updated in PA0121
          INCLUDE STRUCTURE pa0121.
  DATA END OF upd0121.
  DATA: l0121 LIKE del0121 OCCURS 5 WITH HEADER LINE.
  DATA: ref_pernr LIKE pa0121-rfp01,
        num TYPE i.
* start: fetch all IT0031 into del0031 (to be deleted)
  SELECT * FROM pa0121 INTO TABLE del0121
      FOR ALL ENTRIES IN p_pernrs WHERE pernr = p_pernrs-pernr.
  CHECK sy-subrc = 0. "continue only if there are IT0121's
  l0121[] = del0121[].
  LOOP AT l0121.  "for all entries that do have a reference
*begda = lowdate & endda = highdate
    num = nrfpn31 + 1.    "nrfpn31=20, max.# of references in 31
    DO num TIMES VARYING ref_pernr
                       FROM l0121-rfp01 NEXT l0121-rfp02.
*     if test_out = 'X'. write: / ref_pernr, l0121. endif.     "test
      IF ref_pernr = '00000000'. EXIT. ENDIF.     "leave: DO nrfpn31...
* is the reference to a pernr that will be deleted? if so, take the next
      READ TABLE del0121 WITH KEY pernr = ref_pernr.
      IF sy-subrc = 0. CONTINUE. ENDIF.
* is the reference to a pernr that we work with already?
      READ TABLE upd0121 WITH KEY pernr = ref_pernr.
      IF sy-subrc = 0.
        PERFORM remove_one_entry_in_it0121 USING l0121-pernr
                                                 upd0121 num.
        IF num > 0.  "other reference(s) from that PerNr
          MODIFY upd0121 INDEX sy-tabix.
        ELSE.        "there was only one reference; delete this too
          APPEND upd0121 TO del0121.   "num=0, no entry left->del
          DELETE upd0121 INDEX sy-tabix.
        ENDIF.
      ELSE.                            "reference is to "new" PerNr
        SELECT * FROM pa0121 WHERE pernr = ref_pernr.
          EXIT.                        "there should be exactly one
        ENDSELECT.
        IF sy-subrc NE 0.              "error: none found
          CONTINUE.                    "just disregard
        ENDIF.
        PERFORM remove_one_entry_in_it0121 USING l0121-pernr pa0121 num.
        IF num > 0.   "other reference(s) in from that PerNr
          APPEND pa0121 TO upd0121.
        ELSE.                          "there was only one reference
          APPEND pa0121 TO del0121.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDLOOP.

  CHECK test_mode NE 'X'.
* now write the changes to the DB:
  DELETE pa0121 FROM TABLE del0121.
  IF sy-subrc NE 0. WRITE: / 'Fehler bei DELETE IT0121'(d04). ENDIF.
  DESCRIBE TABLE upd0121 LINES num.
  IF num > 0.
    UPDATE pa0121 FROM TABLE upd0121.
    IF sy-subrc NE 0. WRITE: / 'Fehler bei UPDATE IT0121'(u04). ENDIF.
  ENDIF.
ENDFORM.                               "UPDATE_REFERENCE_PRIORITY

*---------------------------------------------------------------------*
*       FORM REMOVE_ONE_ENTRY_IN_IT0121                               *
*---------------------------------------------------------------------*
*       remove from p_pa0031 entry p_pernr; return number of referenc.*
*       left in return_number (-1 if empty & none in it before        *
*---------------------------------------------------------------------*
FORM remove_one_entry_in_it0121 USING value(p_pernr) LIKE pa0121-rfp01
                   p_pa0121     STRUCTURE pa0121
                   number_left  TYPE i."# of references left in IT31
  DATA: found LIKE yes,
        ref_pernr_1 LIKE p0121-rfp01,
        ref_pernr_2 LIKE p0121-rfp01.
*
  found = no.
  number_left = 0.
  DO nrfpn31 TIMES VARYING ref_pernr_1
     FROM p_pa0121-rfp01 NEXT p_pa0121-rfp02
     VARYING ref_pernr_2 FROM p_pa0121-rfp02 NEXT p_pa0121-rfp03.
    number_left = number_left + 1.
    IF ref_pernr_1 EQ p_pernr. found = yes. ENDIF.
    IF found = yes. MOVE ref_pernr_2 TO ref_pernr_1. ENDIF.
    IF ref_pernr_1 = '00000000'. EXIT. ENDIF. "end of list reached
  ENDDO.
  IF found = yes.
    number_left = number_left - 1.
    ref_pernr_2 = '00000000'.
  ENDIF.
ENDFORM.                               "remove_one_entry_in_it0031
*---------------------------------------------------------------------*
*       FORM UPDATE_INFTY1001                                         *
*---------------------------------------------------------------------*
*       InfTyp 1001                                                   *
*---------------------------------------------------------------------*
FORM update_integration TABLES p_pernrs STRUCTURE all_pernrs.
*  data: begin of itab occurs 3. include structure plog.    "QNOK080558
*  data: end of itab.                                       "QNOK080558
*
* DATA: PLOG_TAB LIKE PLOG OCCURS 3 WITH HEADER LINE.       "QNOK080558
                                                            "XWSK005028
  DATA: i1001     LIKE p1001 OCCURS 0 WITH HEADER LINE.     "XWSK005028
  DATA: i1001_inv LIKE p1001 OCCURS 0 WITH HEADER LINE.     "XWSK005028
* data: plog_tab_inv LIKE PLOG OCCURS 3 WITH HEADER LINE.   "XWSK005028
  DATA: objects LIKE hrobject OCCURS 0 WITH HEADER LINE.    "XWSK005028
*
  LOOP AT p_pernrs.
*    REFRESH PLOG_TAB.                          "QNOK080558 "XWSK005028
*    CALL FUNCTION 'RH_FETCH_PLOG'                          "XWSK005028
*         EXPORTING                                         "XWSK005028
*              INFTY   = '1001'                             "XWSK005028
*              OBJID   = P_PERNRS-PERNR                     "XWSK005028
*              OTYPE   = 'P'                                "XWSK005028
*              PLVAR   = SPACE                              "XWSK005028
**           SUBTY   = ' '                                  "XWSK005028
*         TABLES                                            "XWSK005028
*              PLOGTAB = PLOG_TAB                           "XWSK005028
*         EXCEPTIONS                                        "XWSK005028
*              OTHERS  = 1.                                 "XWSK005028
*    CHECK SY-SUBRC = 0 AND TEST_MODE = ' '.                "XWSK005028
*----------- Start of insert block -------------------------- XWSK005028
    REFRESH objects.
    CLEAR objects.
    CALL FUNCTION 'RH_INTEGRATION_CHECK'
         IMPORTING
              integrated_plvar = objects-plvar.
    objects-otype = 'P'.
    objects-objid = p_pernrs.
    APPEND objects.
    CALL FUNCTION 'RH_READ_INFTY'
      EXPORTING
*       AUTHORITY                  = 'DISP'
*       WITH_STRU_AUTH             = 'X'
        infty                      = '1001'
*       ISTAT                      = ' '
*       EXTEND                     = 'X'
*       SUBTY                      = ' '
*       BEGDA                      = '19000101'
*       ENDDA                      = '99991231'
*       CONDITION                  = '00000'
*       INFTB                      = '1'
*       SORT                       = 'X'
*       VIA_T777D                  = ' '
      TABLES
        innnn                      = i1001
        objects                    = objects
      EXCEPTIONS
        all_infty_with_subty       = 1
        nothing_found              = 2
        no_objects                 = 3
        wrong_condition            = 4
        OTHERS                     = 5.
    CHECK sy-subrc = 0 AND test_mode = ' '.
    CALL FUNCTION 'RH_DELETE_INFTY_DIRECT'
         EXPORTING
              vtask               = 'D'  "DIALOG
         TABLES
              innnn               = i1001
         EXCEPTIONS
              no_authorization    = 1
              error_during_delete = 2
              corr_exit           = 3
              OTHERS              = 4.
    IF sy-subrc = 0.
      LOOP AT i1001.
        CALL FUNCTION 'RH_INVERT_RELA_INFTY'
             EXPORTING
                  p1001_imp               = i1001
             IMPORTING
                  p1001_exp               = i1001_inv
             EXCEPTIONS
                  relation_not_reversible = 1
                  OTHERS                  = 2.
        IF sy-subrc = 0.
          APPEND i1001_inv.
        ENDIF.
      ENDLOOP.
      CLEAR sy-subrc.                                       "XWSN362696
      IF test_mode = ' '.                                   "XWSN362696
        CALL FUNCTION 'RH_DELETE_INFTY_DIRECT'
             EXPORTING
                  vtask               = 'D'  "DIALOG
             TABLES
                  innnn               = i1001_inv
             EXCEPTIONS
                  no_authorization    = 1
                  error_during_delete = 2
                  corr_exit           = 3
                  OTHERS              = 4.
      ENDIF.                                                "XWSN362696
    ENDIF.
*----------- End of insert block ---------------------------- XWSK005028
*----------- Start of delete block -------------------------- XWSK005028
*    CALL FUNCTION 'RH_DELETE_INFTY'
*         EXPORTING
*              VTASK               = 'D'"DIALOG
**             ORDER_FLG           = 'X'
**             COMMIT_FLG          = 'X'
**             AUTHY               = 'X'
**             PPPAR_IMP           =
*         TABLES
*              INNNN               = PLOG_TAB
*         EXCEPTIONS
**              error_during_delete = 1
**              no_authorization    = 2
**              delete_first_record = 3
**              corr_exit           = 4
*              OTHERS              = 5.
*----------- End of delete block ---------------------------- XWSK005028
    IF sy-subrc <> 0.
      MOVE p_pernrs-pernr TO pernrs_with_errors-pernr.
      MOVE error_integration TO pernrs_with_errors-error_code.
      APPEND pernrs_with_errors.
    ENDIF.
  ENDLOOP.
ENDFORM.                               "update_integration.

*---------------------------------------------------------------------*
*       FORM UPDATE_CLUSTER                                           *
*---------------------------------------------------------------------*
*       delete everything in any cluster                              *
*---------------------------------------------------------------------*
FORM delete_cluster TABLES p_pernrs STRUCTURE all_pernrs.
  DATA: pclx(4) TYPE c VALUE 'PCLx',
        l_relid LIKE pcl1-relid,
        l_srtfd LIKE pcl1-srtfd.
*
  LOOP AT my_t52b5.
    CHECK my_t52b5-pwert = 1.
    pclx+3(1) = my_t52b5-ponam+1(1).   "ponam-Format:CxYZ fuer PCLx,
    l_relid = my_t52b5-ponam+2(2).     "Cluster YZ
    LOOP AT p_pernrs.
      CONCATENATE p_pernrs-pernr '%' INTO l_srtfd.
      IF test_mode = 'X'.
        SELECT * FROM (pclx) INTO pcl1 WHERE relid = l_relid
                                       AND   srtfd LIKE l_srtfd.
*         if test_out = 'X'. write: / pclx, ':', pcl1. endif.  "test
        ENDSELECT.
      ELSE.                            "no testmode:
        DELETE FROM (pclx) WHERE relid = l_relid AND srtfd LIKE l_srtfd.
      ENDIF.
    ENDLOOP.                           "at all pernrs
  ENDLOOP.                             "at all clusters

  LOOP AT p_pernrs.                                       "QNZAHRK049811
    CALL FUNCTION 'HRPY_DELETE_RGDIR_WPBP'                        "!
        EXPORTING                                                 "!
             employeenumber = p_pernrs-pernr                      "!
             test           = test_mode.                          "!
  ENDLOOP.                                                "QNZAHRK049811
ENDFORM.

*---------------------------------------------------------------------*
*       FORM DELETE_TEVEN                      "QNOK080558 - new form
*---------------------------------------------------------------------*
*       TEVEN loeschen; alle Eintraege in TEVEN_MORE loeschen
*---------------------------------------------------------------------*
*  -->  P_PERNRS   table of all pernrs to be deleted
*---------------------------------------------------------------------*
FORM delete_teven TABLES p_pernrs STRUCTURE all_pernrs.
  DATA: l_teven LIKE teven OCCURS 10 WITH HEADER LINE.
*
  SELECT * FROM teven INTO TABLE l_teven
*           for all entries in all_pernrs where            "XWSN0212720
*              pernr = all_pernrs-pernr and                "XWSN0212720
            FOR ALL ENTRIES IN p_pernrs WHERE              "XWSN0212720
               pernr = p_pernrs-pernr AND                  "XWSN0212720
               ordex = 'X'.
  LOOP AT l_teven.
    IF test_mode IS INITIAL.
*     UPDATE afru SET pernr = '00000000' WHERE pdsnr = l_teven-pdsnr
*                                                   ...     "QNOK034383
*     update teven_more set pernr = '00000000'  "QNOK034383 "QNOK025951
*                       where pdsnr = l_teven-pdsnr         "QNOK025951
*                       and   pernr = l_teven-pernr.        "QNOK025951
      DELETE FROM teven_more WHERE pdsnr = l_teven-pdsnr.   "QNOK025951
    ENDIF.
    WRITE: /
     'TEVEN/_MORE: Personalnummer auf "00000000" gesetzt für:   '(379),
      l_teven-pernr, l_teven-pdsnr.
  ENDLOOP.
  CHECK test_mode IS INITIAL.          "don't delete in TEST-MODE
* loop at all_pernrs.                                      "XWSN0212720
  LOOP AT p_pernrs.                                        "XWSN0212720
*   delete from teven where pernr = all_pernrs-pernr        "QNOK025951
*                                         and ordex = ' '.  "QNOK025951
*   update teven set pernr = '00000000' where ordex = 'X'   "QNOK025951
*                                         and pernr = all_pernrs-pernr.
*   delete from teven where pernr = all_pernrs-pernr.       "QNOK025951
    "XWSN0212720
    DELETE FROM teven WHERE pernr = p_pernrs-pernr.         "QNOK025951
    "XWSN0212720
  ENDLOOP.
ENDFORM.                               "delete_teven

*---------------------------------------------------------------------*
*       FORM DELETE_POSTINGS                                          *
*---------------------------------------------------------------------*
*       Tabellen fuer Buchungen (RPCIPO00) und
*                Matchcode W (T52MCW) loeschen
*---------------------------------------------------------------------*
*  -->  P_PERNRS    table of all pernrs to be deleted
*---------------------------------------------------------------------*
FORM delete_postings TABLES p_pernrs STRUCTURE all_pernrs.
  CHECK test_mode IS INITIAL.          "don't delete in TEST-MODE
* LOOP AT ALL_PERNRS.                                     "XWSN0212720
*   DELETE FROM PCALAC WHERE PERNR = ALL_PERNRS-PERNR.    "XWSN0212720
*   DELETE FROM PPOIX WHERE PERNR = ALL_PERNRS-PERNR.     "XWSN0212720
*   DELETE FROM PPOPX WHERE PERNR = ALL_PERNRS-PERNR.     "XWSN0212720
*   DELETE FROM T52MCW WHERE PERNR = ALL_PERNRS-PERNR.   "QNOK036732
  "XWSN0212720
  LOOP AT p_pernrs.                                       "XWSN0212720
    DELETE FROM pcalac WHERE pernr = p_pernrs-pernr.      "XWSN0212720
    DELETE FROM ppoix WHERE pernr = p_pernrs-pernr.       "XWSN0212720
    DELETE FROM ppopx WHERE pernr = p_pernrs-pernr.       "XWSN0212720
    DELETE FROM t52mcw WHERE pernr = p_pernrs-pernr.        "QNOK036732
    "XWSN0212720
  ENDLOOP.
ENDFORM.                               "delete_teven
*&---------------------------------------------------------------------*
*&      Form  LOCK_PERNR
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  pernrs_to_lock: table with all PerNrs to be locked
*----------------------------------------------------------------------*
FORM enqueue_pernr TABLES pernrs_to_lock STRUCTURE all_pernrs.
  LOOP AT pernrs_to_lock.                                   "XWSK006897
    ADD 1 TO number_really_looped.                          "XWSK006897
  ENDLOOP.                                                  "XWSK006897
  CHECK test_mode IS INITIAL.          "don't lock in TEST_MODE
  LOOP AT pernrs_to_lock.
* if lock fails sy-subrc is set but no warning/error/etc issued...
    PERFORM enqueue_pernr(sapfp50g) USING pernrs_to_lock-pernr ' '.
    IF sy-subrc NE 0.  "couldn't lock pernr - take out of list!
* ...we do this here (if necessary)
      IF protocol = 'X'.
        WRITE: / 'Sperren nicht möglich für PerNr    '(er0),
                 pernrs_to_lock-pernr.
* ...store the troublemaker away...
      ENDIF.
      MOVE pernrs_to_lock-pernr TO pernrs_with_errors-pernr.
      MOVE error_lock TO pernrs_with_errors-error_code.
      APPEND pernrs_with_errors.
* ...and take it out of our work-list
      DELETE pernrs_to_lock.
    ENDIF.
  ENDLOOP.
ENDFORM.                               " ENQUEUE_PERNR

*&---------------------------------------------------------------------*
*&      Form  DEQUEUE_PERNR
*&---------------------------------------------------------------------*
*       dequeue all pernrs from table pernrs_to_dequeue                *
*----------------------------------------------------------------------*
*  -->  pernrs_to_dequeue: list of pernrs to dequeue
*----------------------------------------------------------------------*
FORM dequeue_pernr TABLES pernrs_to_dequeue STRUCTURE all_pernrs.
  LOOP AT pernrs_to_dequeue.
    PERFORM dequeue_pernr(sapfp50g) USING pernrs_to_dequeue-pernr.
    IF sy-subrc NE 0.
      WRITE: / 'Entsperren nicht möglich! PerNr'(er1),
               pernrs_to_dequeue-pernr.
    ENDIF.
  ENDLOOP.
ENDFORM.                               " DEQUEUE_PERNR

*---------------------------------------------------------------------*
*       FORM DEL_ASSOB_PAFRU                                          *
*---------------------------------------------------------------------*
*       delete entries in assob (pafru not used - yet)                *
*---------------------------------------------------------------------*
*  -->  p_pernrs                                                      *
*---------------------------------------------------------------------*
FORM del_assob_pafru TABLES p_pernrs STRUCTURE all_pernrs
                     USING  value(p_dbtabname) LIKE dbtabname
                            value(p_infty) LIKE t777d-infty.
  DATA: l_infty LIKE t777d-infty.      "Syntax-restriction ABAP
*
  l_infty = p_infty.                   "Syntax-restriction ABAP
* check p_infty = '0014' or p_infty = '0015' or l_infty(1) = '2'.
                                                            "N0191305
  SELECT SINGLE * FROM t582a                                "N0191305
         WHERE  infty = l_infty.                            "N0191305
  CHECK sy-subrc = 0 AND NOT t582a-pzint IS INITIAL.        "N0191305
* get assob-entries: fill tab. asshr_key      (pafru/ordex not used)
  REFRESH asshr_key.
  SELECT * FROM (p_dbtabname) INTO prel_db
    FOR ALL ENTRIES IN p_pernrs
           WHERE pernr = p_pernrs-pernr
           AND   refex = 'X'.
*           and  ( ordex = 'X' or refex = 'X' ).    "ordex not used
*   if test_out = 'X'.                                         "test
*     write: / p_dbtabname, 'ordex=', prel_db-ordex, 'refex=',
*                          prel_db-refex.
*   endif.
*    IF PREL_DB-ORDEX = 'X'.
*      MOVE-CORRESPONDING PREL_DB TO PAFRU_KEY.
*      MOVE ALL_INFOTYPES-INFTY TO PAFRU_KEY-INFTY.
*      APPEND PAFRU_KEY.
*    ENDIF.
*    if prel_db-refex = 'X'.
    MOVE-CORRESPONDING prel_db TO asshr_key.
    MOVE p_infty TO asshr_key-infty.
    APPEND asshr_key.
*    endif.
  ENDSELECT.

  CHECK test_mode IS INITIAL.
* assob_hr
  LOOP AT asshr_key.
    SELECT * FROM asshr WHERE pernr = asshr_key-pernr
                        AND   infty = asshr_key-infty
                        AND   subty = asshr_key-subty
                        AND   objps = asshr_key-objps
                        AND   sprps = asshr_key-sprps
                        AND   endda = asshr_key-endda
                        AND   begda = asshr_key-begda
                        AND   seqnr = asshr_key-seqnr.
      EXIT.   "we will find only one entry anyway
    ENDSELECT.
    IF sy-subrc <> 0.                  "delete the line just read
      WRITE: / 'Kein Eintrag in asshr obwohl refex = "X"'(ord).
    ELSE.
      DELETE FROM assob WHERE pdsnr = asshr-pdsnr.
      IF sy-subrc <> 0.
        WRITE: / 'Fehler bei DELETE assob'(d01), asshr.
      ENDIF.
      DELETE FROM pdsnr WHERE pdsnr = asshr-pdsnr.
      IF sy-subrc <> 0.
        WRITE: / 'Fehler bei DELETE pdsnr'(d02).
      ENDIF.
      DELETE asshr.
      IF sy-subrc <> 0.
        WRITE: / 'Fehler bei DELETE asshr'(d03).
      ENDIF.
    ENDIF.
  ENDLOOP.                             "at asshr_key

  EXIT.                                "pafru will not be used....

** pafru
*  LOOP AT PAFRU_KEY.
*    SELECT * FROM ASSHR WHERE PERNR = PAFRU_KEY-PERNR
*                        AND   INFTY = PAFRU_KEY-INFTY
*                        AND   SUBTY = PAFRU_KEY-SUBTY
*                        AND   OBJPS = PAFRU_KEY-OBJPS
*                        AND   SPRPS = PAFRU_KEY-SPRPS
*                        AND   ENDDA = PAFRU_KEY-ENDDA
*                        AND   BEGDA = PAFRU_KEY-BEGDA
*                        AND   SEQNR = PAFRU_KEY-SEQNR.
*      EXIT.   "we will find only one entry anyway
*    ENDSELECT.
*    IF SY-SUBRC NE 0.
** well, since the infty may be marked with "X" even though there is no
**  entry in pafru, we can't rely on it... that's a bug, but not mine
**  so we just skip this error-msg, since it doesn't have to be an error
**     write: / 'Kein Eintrag in asshr obwohl ordex = "X"'(ord). "QNO...
*    ELSE.
*      DELETE FROM AFRU WHERE PDSNR = ASSHR-PDSNR.
*      DELETE FROM PDSNR WHERE PDSNR = ASSHR-PDSNR.
*      DELETE ASSHR.
*    ENDIF.
*  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  END
*&---------------------------------------------------------------------*
*       finish delete, write protocol                                  *
*----------------------------------------------------------------------*
*  -->  number_looped: Anzahl der bearbeiteten Personalnummern
*----------------------------------------------------------------------*
FORM end USING number_looped TYPE i.
  DATA: tmp TYPE i.
*
  IF test_mode = 'X'.
    FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
    WRITE: / ,
     /21(60) 'Dies war nur ein Testlauf.'(te2),
     /21(60) 'Es wurden keine Daten gelöscht.'(te3),
     /21(60) 'Zum Löschen Testparameter = OFF setzen.'(te4).
    FORMAT RESET.
  ENDIF.
  SKIP 1.
  WRITE: / 'Protokoll des Reports     '(pro), sy-repid.
  SKIP 1.
  FORMAT COLOR COL_TOTAL.
  WRITE: 'Anzahl bearbeiteter Personalnummern:     '(te1),
         number_looped.
  SKIP 1.
  FORMAT RESET.
  DESCRIBE TABLE pernrs_with_errors LINES tmp.
  IF tmp > 0.
    FORMAT COLOR COL_HEADING.
    WRITE: / 'Fehlerhafte Personalnummer:   '(te5), tmp.
    FORMAT RESET.
    LOOP AT pernrs_with_errors.
      WRITE: / pernrs_with_errors-pernr, '   '.
      CASE pernrs_with_errors-error_code.
        WHEN error_lock. WRITE: 'Personalnummer gesperrt   '(er3).
        WHEN error_integration.
          WRITE 'Integration nicht löschbar  '(er2).
        WHEN OTHERS.
          WRITE:  'Fehler-Code '(er9), pernrs_with_errors-error_code.
      ENDCASE.
    ENDLOOP.
  ENDIF.
ENDFORM.                               " END
* N0159744 ------------> start of insert block
*&---------------------------------------------------------------------*
*&      Form  DELETE_PTRV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ALL_PERNRS  text
*----------------------------------------------------------------------*
FORM delete_ptrv TABLES   p_all_pernrs STRUCTURE all_pernrs.
  CHECK test_mode IS INITIAL.
  LOOP AT p_all_pernrs.
    CALL FUNCTION 'PERNR_TRIPS_DELETE_ALL'
         EXPORTING
              employeenumber = p_all_pernrs-pernr
              check_auth     = space.
  ENDLOOP.
ENDFORM.                    " DELETE_PTRV
* N0159744 ------------> end of insert block
* N0159824 ------------> start of insert block
*&---------------------------------------------------------------------*
*&      Form  DELETE_BAV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ALL_PERNRS  text
*----------------------------------------------------------------------*
FORM delete_bav TABLES   p_all_pernrs STRUCTURE all_pernrs.
  CHECK test_mode IS INITIAL.
  LOOP AT p_all_pernrs.
    CALL FUNCTION 'RP_01C_DA_DELETE'
         EXPORTING
              pernr = p_all_pernrs-pernr.                "#EC DOM_EQUAL
    CALL FUNCTION 'RP_01C_DBDELPCL2'                        "XWSK003489
      EXPORTING i10_pernr = p_all_pernrs-pernr."#EC DOM_EQUAL"XWSK003489
  ENDLOOP.
ENDFORM.                    " DELETE_BAV
* N0159824 ------------> end of insert block
* XWSAHRK055570 ------------> start of insert block
*&---------------------------------------------------------------------*
*&      Form  DELETE_T77INT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ALL_PERNRS  text
*----------------------------------------------------------------------*
FORM delete_t77int TABLES   p_all_pernrs STRUCTURE all_pernrs.
  DATA: l_return LIKE sy-subrc.
  CHECK test_mode IS INITIAL.
  LOOP AT p_all_pernrs.
* Pruefen ob erweiterte Integration aktiv:
    CALL FUNCTION 'HR_CHECK_MARKED_EVENT'
         EXPORTING
              act_pernr = p_all_pernrs-pernr
         IMPORTING
              return    = l_return.
    CHECK l_return = 8.
* Loeschen:
    CALL FUNCTION 'RH_DEL_DATA_AFTER_INTEGRATION'
         EXPORTING
              act_pernr     = p_all_pernrs-pernr
         EXCEPTIONS
              no_data_found = 1
              OTHERS        = 2.
* der sy-subrc ist uns egal, das ist nur fuer SLIN
    CHECK sy-subrc = 0.
  ENDLOOP.
ENDFORM.                    " DELETE_T77INT
* XWSAHRK055570 ------------> end of insert block
*YAYAHRK059890 New form
*&---------------------------------------------------------------------*
*&      Form  DELETE_PTQUODED
*&---------------------------------------------------------------------*
*       Löschen der PTQUODED (Zeitkontingentabtragung)
*----------------------------------------------------------------------*
FORM delete_ptquoded
     TABLES
            p_all_pernrs STRUCTURE all_pernrs.
  DATA: wper LIKE LINE OF all_pernrs.

  CHECK test_mode IS INITIAL.
  LOOP AT p_all_pernrs INTO wper.
    DELETE FROM ptquoded WHERE pernr = wper-pernr.
  ENDLOOP.
ENDFORM.                    " DELETE_PTQUODED

* XWSK063353 ------------> start of insert block
*&---------------------------------------------------------------------*
*&      Form  DELETE_HRMDORIGIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_hrmdorigin
     TABLES
            p_all_pernrs STRUCTURE all_pernrs.
  DATA: wper LIKE LINE OF all_pernrs,
        otype TYPE otype.

  CHECK test_mode IS INITIAL.
  otype = 'P'.
  LOOP AT p_all_pernrs INTO wper.
    CALL FUNCTION 'RH_PERSON_CLEAR_ORIGSYSTEM'
         EXPORTING
              otype = otype
              pernr = wper-pernr.
  ENDLOOP.
ENDFORM.                    " DELETE_HRMDORIGIN
* XWSK063353 ------------> end of insert block

* XWSK063486 ------------> start of insert block
*&---------------------------------------------------------------------*
*&      Form  DELETE_DEUV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ALL_PERNRS  text
*----------------------------------------------------------------------*
FORM delete_deuv
     TABLES
            p_all_pernrs STRUCTURE all_pernrs.
  DATA: wper LIKE LINE OF all_pernrs.

  CHECK test_mode IS INITIAL.
  LOOP AT p_all_pernrs INTO wper.
    CALL FUNCTION 'HR_DELETE_D3_ALL'
         EXPORTING
              pernr  = wper-pernr
         EXCEPTIONS
              OTHERS = 1.
    CHECK sy-subrc = 0.
  ENDLOOP.
ENDFORM.                    " DELETE_DEUV
* XWSK063486 ------------> end of insert block

* QCMPH0K000688 ---------> start of insert block
*&---------------------------------------------------------------------*
*&      Form  DELETE_SKV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ALL_PERNRS  text
*----------------------------------------------------------------------*
FORM delete_skv
     TABLES p_all_pernrs STRUCTURE all_pernrs.
  DATA: wper LIKE LINE OF all_pernrs.

  CHECK test_mode IS INITIAL.
  LOOP AT p_all_pernrs INTO wper.
    CALL FUNCTION 'HR_DE_BK_DELETE_ALL'
         EXPORTING
              pnr                = wper-pernr
         EXCEPTIONS
              no_entries_deleted = 1
              OTHERS             = 2.
    CHECK sy-subrc = 0 OR sy-subrc = 1.
  ENDLOOP.

ENDFORM.                    " DELETE_SKV
* QCMPH0K000688 --------> end of insert block
* XWSK005619 ------------> start of insert block
*&---------------------------------------------------------------------*
*&      Form  delete_p593r
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ALL_PERNRS  text
*----------------------------------------------------------------------*
FORM delete_p593r
     TABLES p_all_pernrs STRUCTURE all_pernrs.
  DATA: wper LIKE LINE OF all_pernrs.

  CHECK test_mode IS INITIAL.
  LOOP AT p_all_pernrs INTO wper.
    CALL FUNCTION 'HR_DE_PS_DEL_P593R'
         EXPORTING
              pernr          = wper-pernr
         EXCEPTIONS
              no_entry_found = 1
              OTHERS         = 2.
    CHECK sy-subrc = 0 OR sy-subrc = 1.
  ENDLOOP.

ENDFORM.                    " delete_p593r
* XWSK005619 ------------> end of insert block
*&---------------------------------------------------------------------*
*&      Form  change_pointers                            new XWSN329054
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ALL_PERNRS  text
*----------------------------------------------------------------------*
FORM change_pointers TABLES   p_pernrs STRUCTURE all_pernrs.
  DATA: t_change_pointers TYPE hrobjinfty OCCURS 0 WITH HEADER LINE.

  CHECK test_mode IS INITIAL.
  CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
       IMPORTING
            act_plvar       = t_change_pointers-plvar
       EXCEPTIONS
            no_active_plvar = 1
            OTHERS          = 2.
  IF sy-subrc = 0.
    t_change_pointers-otype = 'P'.
    t_change_pointers-infty = '0000'.
    t_change_pointers-begda = sy-datum.
    t_change_pointers-endda = sy-datum.
    LOOP AT p_pernrs.
      t_change_pointers-objid = p_pernrs-pernr.
      APPEND t_change_pointers.
    ENDLOOP.
    CALL FUNCTION 'RH_INFTY_CHANGE_PROT'
         TABLES
              changed_objects = t_change_pointers.
  ENDIF.
ENDFORM.                    " CHANGE_POINTERS
*&---------------------------------------------------------------------*
*&      Form  delete_PAYROLL_LEGACY_DATA                 new XWSN355855
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ALL_PERNRS  text
*----------------------------------------------------------------------*
FORM delete_payroll_legacy_data TABLES   p_all_pernrs
                                         STRUCTURE all_pernrs.
  DATA: wper LIKE LINE OF all_pernrs.

  CHECK test_mode IS INITIAL.
  LOOP AT p_all_pernrs INTO wper.
    CALL FUNCTION 'HR_DELETE_LEGACY_DATA_PAYROLL'
         EXPORTING
              pernr = wper-pernr.
  ENDLOOP.
ENDFORM.                    " delete_PAYROLL_LEGACY_DATA

*begin XDPN0358008
*---------------------------------------------------------------------*
*       FORM delete_cp                                                *
*---------------------------------------------------------------------*
FORM delete_cp TABLES p_pernrs STRUCTURE all_pernrs.

  DATA current_pernr LIKE LINE OF all_pernrs.

  DATA ls_hrpersonee TYPE hrpersonee_s.

  DATA lt_relation TYPE p1001 OCCURS 0 WITH HEADER LINE.
  DATA lt_object TYPE hrobject OCCURS 0 WITH HEADER LINE.

  DATA lv_related_any TYPE p.
  DATA l_plvar TYPE plvar.

  CHECK test_mode IS INITIAL.

  CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
       EXPORTING
            set_default_plvar = 'X'
       IMPORTING
            act_plvar         = l_plvar
       EXCEPTIONS
            no_active_plvar   = 0
            OTHERS            = 0.

  LOOP AT p_pernrs INTO current_pernr.

    CLEAR ls_hrpersonee.

    CALL FUNCTION 'HR_PERSONEE_GETPERSON'
         EXPORTING
              iv_employee_id    = current_pernr-pernr
              iv_with_authority = space
         IMPORTING
              es_hrpersonee     = ls_hrpersonee.

    CHECK ls_hrpersonee-personid CN ' 0'.

    CLEAR lt_object.
    REFRESH lt_object.

    lt_object-plvar = l_plvar.
    lt_object-otype = 'CP'.
    lt_object-objid = ls_hrpersonee-personid.
    APPEND lt_object.

    CLEAR lt_relation.
    REFRESH lt_relation.

    CALL FUNCTION 'RH_READ_INFTY'
         EXPORTING
              authority            = space
              infty                = '1001'
         TABLES
              innnn                = lt_relation
              objects              = lt_object
         EXCEPTIONS
              all_infty_with_subty = 0
              nothing_found        = 0
              no_objects           = 0
              wrong_condition      = 0
              OTHERS               = 0.

    DELETE lt_relation WHERE sclas = 'P'
                         AND sobid = current_pernr-pernr.

    DESCRIBE TABLE lt_relation LINES lv_related_any.

    IF lv_related_any GT 0.   "additional references are existing

* - delete relation to current cp
      CALL FUNCTION 'HR_PERSONEE_DELETE'
           EXPORTING
                is_hrpersonee     = ls_hrpersonee
                iv_with_authority = space
           EXCEPTIONS
                no_authorization  = 0
                invalid_data      = 0
                OTHERS            = 0.

    ELSE. " no additional refrences

* - delete             current cp
      CALL FUNCTION 'HR_CENTRALPERSON_DELETE'
           EXPORTING
                iv_person_id      = ls_hrpersonee-personid
                iv_with_authority = space
           EXCEPTIONS
                no_authorization  = 0
                invalid_data      = 0
                OTHERS            = 0.

    ENDIF. " additional refrences

  ENDLOOP.

  CHECK ls_hrpersonee-personid CN ' 0'.                     "XREN631092
  PERFORM delete_prio_rfpnr TABLES all_pernrs.              "XREN631092
ENDFORM.                                                    " delete_cp
*end   XDPN0358008
*&---------------------------------------------------------------------*
*&      Form  DELETE_SVWG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ALL_PERNRS  text
*----------------------------------------------------------------------*
FORM delete_svwg                                            "YCTK032145
     TABLES
            p_all_pernrs STRUCTURE all_pernrs.

  CHECK test_mode IS INITIAL.
  LOOP AT p_all_pernrs.
    DELETE FROM t5d46 WHERE pernr = p_all_pernrs-pernr.
    DELETE FROM t5d48 WHERE pernr = p_all_pernrs-pernr.
  ENDLOOP.

ENDFORM.                    " DELETE_SVWG
*&---------------------------------------------------------------------*
*&      Form  delete_tax_certi                           new XREN545903
*&---------------------------------------------------------------------*
FORM delete_tax_certi
            TABLES p_all_pernrs STRUCTURE all_pernrs.
  CHECK test_mode IS INITIAL.
  LOOP AT p_all_pernrs.
    DELETE FROM t5d2m WHERE pernr = p_all_pernrs-pernr.
  ENDLOOP.
ENDFORM.                    " delete_tax_certi
*&---------------------------------------------------------------------*
*&      Form  delete_prio_rfpnr                          new XREN631092
*&---------------------------------------------------------------------*
FORM delete_prio_rfpnr
          TABLES p_all_pernrs STRUCTURE all_pernrs.

  DATA p0121        LIKE p0121 OCCURS 0 WITH HEADER LINE.
  DATA s0121        LIKE pa0121 OCCURS 0 WITH HEADER LINE.
  DATA rfpnr        LIKE p0121-pernr.
  DATA tab_lines(2) TYPE c.
  DATA number(20).
  DATA index(4).
  DATA: BEGIN OF rfpnr_tab OCCURS 0,
          pernr LIKE pernr-pernr,
        END OF rfpnr_tab.
  DATA adjust_tab LIKE rfpnr_tab OCCURS 0 WITH HEADER LINE.
  FIELD-SYMBOLS <number>.

  CHECK test_mode IS INITIAL.

  LOOP AT p_all_pernrs.
    CALL FUNCTION 'HR_READ_INFOTYPE'
         EXPORTING
              pernr     = p_all_pernrs-pernr
              infty     = '0121'
         TABLES
              infty_tab = p0121.

*     Alle involvierte PERNR besorgen
    LOOP AT p0121.
      DO nrfpn121 TIMES VARYING rfpnr
                        FROM p0121-rfp01 NEXT p0121-rfp02.
        IF rfpnr CN ' 0' AND rfpnr <> p_all_pernrs-pernr.
          rfpnr_tab-pernr = rfpnr.
          APPEND rfpnr_tab.
        ENDIF.
      ENDDO.
    ENDLOOP.
    SORT rfpnr_tab ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rfpnr_tab.
    CLEAR rfpnr.
    DESCRIBE TABLE rfpnr_tab LINES tab_lines.

    IF tab_lines <= 1.
*     Fall 1: Zu CP besteht nur noch eine weitere P-Verknüpfung
*             => löschen des IT 121 der anderen PERNR
      READ TABLE rfpnr_tab INDEX 1.
      SELECT * FROM pa0121 INTO s0121 WHERE pernr = rfpnr_tab-pernr.
        APPEND s0121.
      ENDSELECT.
      LOOP AT s0121.
        DO nrfpn121 TIMES VARYING rfpnr
                          FROM s0121-rfp01 NEXT s0121-rfp02.
          IF rfpnr = p_all_pernrs-pernr.
            DELETE pa0121 FROM TABLE s0121.
            EXIT.
          ENDIF.
        ENDDO.
      ENDLOOP.
    ELSE.
*     Fall 2: Zu CP besteht mehr als eine P-Verknüpfung
*             => IT 121 der weiteren Pernr anpassen
      LOOP AT rfpnr_tab.
        SELECT * FROM pa0121 INTO s0121 WHERE pernr = rfpnr_tab-pernr.
          APPEND s0121.
        ENDSELECT.
        LOOP AT s0121.
*         die aktuellen RFPNRs in adjust_tab sichern und die
*         Wiederholstruktur von s0121 komplett löschen
          DO nrfpn121 TIMES VARYING rfpnr
                            FROM s0121-rfp01 NEXT s0121-rfp02.
            IF rfpnr CN ' 0' AND rfpnr <> p_all_pernrs-pernr.
              adjust_tab-pernr = rfpnr.
              APPEND adjust_tab.
            ENDIF.
          ENDDO.
*           ggf. Hauptpersonalnummer löschen
          IF s0121-hpern = p_all_pernrs-pernr.
            CLEAR s0121-hpern.
            MODIFY s0121.
          ENDIF.
          CLEAR: number, index.
          DO nrfpn121 TIMES.
            index = sy-index.
            SHIFT index LEFT DELETING LEADING space.
            IF index+1(1) IS INITIAL.
              CONCATENATE 'S0121-RFP' '0' index INTO number.
            ELSE.
              CONCATENATE 'S0121-RFP' index INTO number.
            ENDIF.
            ASSIGN (number) TO <number>.
            CLEAR <number>.
            MODIFY s0121.
          ENDDO.
*           die aktuellen RFPNRs wieder einfügen
          CLEAR: number, index.
          LOOP AT adjust_tab.
            index = sy-tabix.
            SHIFT index LEFT DELETING LEADING space.
            IF index+1(1) IS INITIAL.
              CONCATENATE 'S0121-RFP' '0' index INTO number.
            ELSE.
              CONCATENATE 'S0121-RFP' index INTO number.
            ENDIF.
            ASSIGN (number) TO <number>.
            <number> = adjust_tab-pernr.
            MODIFY s0121.
          ENDLOOP.
*           DB-Tabelle anpassen
          MODIFY pa0121 FROM TABLE s0121.
          CLEAR adjust_tab.
          REFRESH adjust_tab.
        ENDLOOP.
        CLEAR   s0121.
        REFRESH s0121.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.                                      " delete_prio_rfpnr
