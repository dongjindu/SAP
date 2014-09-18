*XZQ 171002 Hinweis 563592 Dump bei Zeilensummen > 999.99
*XZQ 240800 Hinweis 327806 mit P9CK066380
*XZQ 050500 Hinweis 300987 mit P9CK028499
*4.6C
***INCLUDE LCATSFM1.
*---------------------------------------------------------------------*
*       FORM MODIFY_ICATSD_AFTER_CHANGE                               *
*---------------------------------------------------------------------*
*       modify internal table icatsd after change on dynpro          *
*---------------------------------------------------------------------*
*  -->  EXT_CALL                                                      *
*  -->  EXT_LINE                                                      *
*  -->  FLG_MARK                                                      *
*  -->  FLG_ENRICH                                                    *
*  -->  UCATSD                                                        *
*---------------------------------------------------------------------*
FORM modify_icatsd_after_change  USING external_call   TYPE c
                                       external_line   LIKE sy-tabix
                                       flag_mark       TYPE c
                                       flag_enrich     TYPE c
                                       catsd_new       LIKE catsd.

  DATA: catsd_comp LIKE catsd.
  DATA: t_catsd LIKE catsd OCCURS 0 WITH HEADER LINE.
  DATA: readindex LIKE sy-tabix.
  DATA: added_index LIKE sy-tabix.
  DATA: insert_index LIKE sy-tabix.
  DATA: modify_index LIKE sy-tabix.
  DATA: uenrich_flag_in(1) TYPE c.
  DATA: uenrich_flag_out(1) TYPE c.
  DATA: ucats_popup  LIKE cats_popup.
  DATA: ucats_techn  LIKE cats_techn.
  DATA: uci_catsdb LIKE cats_ci_ca.
  DATA: exit_flag(1) TYPE c.
*-------------Hinweis 327806 Beginn----------------
  DATA: beguhr_alt LIKE catsdb-beguz.  "Hinweis 327806
  DATA: enduhr_alt LIKE catsdb-enduz.  "Hinweis 327806
  DATA: beguhr_neu LIKE catsdb-beguz.  "Hinweis 327806
  DATA: enduhr_neu LIKE catsdb-enduz.  "Hinweis 327806
*-------------Hinweis 327806 Ende--------------------


* set the input enrich flag
  uenrich_flag_in = flag_enrich.
* clear the output enrich flag
  CLEAR uenrich_flag_out.

* treat different scenarios
  IF external_call = space.            "called directly from dynpro
* read the original
    readindex = tc_catsd-current_line + lines_added_index.
    READ TABLE icatsd INDEX readindex.
    IF sy-subrc = 0 AND NOT ( icatsd IS INITIAL ).
* get popup information to catsd_new
      MOVE-CORRESPONDING icatsd TO ucats_popup.
      MOVE-CORRESPONDING ucats_popup TO catsd_new.
* get technical information to catsd_new
      MOVE-CORRESPONDING icatsd TO ucats_techn.
      MOVE-CORRESPONDING ucats_techn TO catsd_new.
* get customer information to catsd_new
      MOVE-CORRESPONDING icatsd TO uci_catsdb.
* there could be fields in catsd_new that have to be saved
      PERFORM move_additional_fields USING catsd_new uci_catsdb.
      MOVE-CORRESPONDING uci_catsdb TO catsd_new.
* get controlling szenario flag to catsd_new                 "XQPK084279
      MOVE icatsd-hrcostasg TO catsd_new-hrcostasg.         "XQPK084279
* enrich data only once
      CLEAR uenrich_flag_in.
    ELSE.
      PERFORM initialize_catsd_records USING icatsd catsd_new
                                                exit_flag.
      IF exit_flag = yx.               "initial record given
        EXIT.
      ENDIF.
    ENDIF.
  ELSE.                                "called via form routine
* is an external line given
    IF NOT external_line IS INITIAL.
      readindex = external_line + lines_added_index.
      READ TABLE icatsd INDEX readindex.
      IF sy-subrc = 0 AND NOT ( icatsd IS INITIAL ).
* bring technical information to given line
*       no change if the co szenario doesn't fit to the profile  "XQP
        IF  ( icatsd-hrcostasg <> tcats-hrcostasg AND       "XQPK084279
              icatsd-hrcostasg <> szenario_0                "XQPK084279
              OR NOT icatsd-tasktype IS INITIAL )           "YIK
           AND   icatsd-enrich    <> yx.                    "XQPK084279
          MESSAGE e319(lr).                                 "XQPK084279
*         Function not possible with data sets that are display only
        ENDIF.                                              "XQPK084279
        MOVE-CORRESPONDING icatsd TO ucats_techn.
        MOVE-CORRESPONDING ucats_techn TO catsd_new.
      ELSE.
        PERFORM initialize_catsd_records USING icatsd catsd_new
                                                  exit_flag.
        IF exit_flag = yx.             "initial record given
          EXIT.
        ENDIF.
      ENDIF.
    ELSE.
      PERFORM initialize_catsd_records USING icatsd catsd_new
                                                exit_flag.
      IF exit_flag = yx.               "initial record given
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  action_done = yx.

  MOVE-CORRESPONDING icatsd TO catsd_comp.
  IF catsd_new NE catsd_comp.          "something changed at all?

* -------Hinweis 327806 Beginn----------------------------

* Falls in der Monatssicht eine Zeile geändert wird,
* die für die Tage 15 bis 31 Uhrzeiten enthält, so gehen
* diese Uhrzeiten verloren (die Felder fehlen im TC).
* Da in einem Monatsprofil keine Uhrzeiten geändert
* werden können, die Uhrzeiten aber dominant sind,
* ist es sinnvoll, die Uhrzeiten aus dem "alten Satz"
* in den "neuen" Satz zu übernehmen.

    IF tcats-pertype = 4.

      DO days_on_screen TIMES
      VARYING beguhr_alt FROM catsd_comp-beguz1
      NEXT catsd_comp-beguz2
      VARYING enduhr_alt FROM catsd_comp-enduz1
      NEXT catsd_comp-enduz2
      VARYING beguhr_neu FROM catsd_new-beguz1
      NEXT catsd_new-beguz2
      VARYING enduhr_neu FROM catsd_new-enduz1
      NEXT catsd_new-enduz2.

        beguhr_neu = beguhr_alt.
        enduhr_neu = enduhr_alt.

      ENDDO.

    ENDIF.                             "if tcats-pertype = 4
*--------Hinweis 327806 Ende------------------------------

    REFRESH t_catsd.
* check entries of record
    IF icatsd-enrich = yx.
      uenrich_flag_in = yx.
    ENDIF.
    PERFORM enrich_and_check_entries TABLES t_catsd
                                     USING catsd_new icatsd
                                           uenrich_flag_in
                                           uenrich_flag_out
                                         external_call.     "note325504
* check against required fields
*   PERFORM CHECK_AGAINST_REQUIRED_FIELDS TABLES T_CATSD.

* bring checked values back to dynpro
    CLEAR icatsd.
    CLEAR added_index.
    LOOP AT t_catsd.
      added_index = added_index + 1.
      IF added_index GT 1.
* a line was added through exit.
        ADD 1 TO lines_added_index.
      ENDIF.
      MOVE t_catsd TO catsd_new.
* Calculate sum of hours
      PERFORM calculate_sum_of_hours USING catsd_new-sumdays
*              CATSD_NEW.              "Hinweis 563592
               catsd_new external_call."Hinweis 563592
      MOVE-CORRESPONDING catsd_new TO icatsd.
      IF uenrich_flag_out = yx.
        icatsd-enrich = yx.
      ENDIF.
      IF NOT flag_mark IS INITIAL.
        icatsd-mark = yx.
      ENDIF.
      icatsd-modified = yx.
      IF icatsd-enrich = yx.                                "note300987
        icatsd-hrcostasg = szenario_0.                      "note300987
      ENDIF.                                                "note300987



* has split in line taken place ??
      IF added_index GT 1.
        IF external_call = space.
          insert_index = tc_catsd-current_line + lines_added_index.
        ELSE.
          insert_index = external_line + lines_added_index.
        ENDIF.
        INSERT icatsd INDEX insert_index.
        IF sy-subrc NE 0.              "only the case at end of list
          APPEND icatsd.
        ENDIF.
      ELSE.                            "standard case
        IF external_call = space.
          MODIFY icatsd INDEX readindex.
          IF sy-subrc NE 0.
            APPEND icatsd.
          ENDIF.
        ELSE.
          IF external_line IS INITIAL.
            APPEND icatsd.
          ELSE.
            modify_index = external_line + lines_added_index.
            MODIFY icatsd INDEX modify_index.
            IF sy-subrc NE 0.
              MESSAGE x030.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

* initialize catsd_new in order to prevent wrong field transport to the
* next record with fields that are not on the dynp (KOKRS, MEINH etc.)
  CLEAR catsd_new.
  IF tcats-begend = yx.                "initialize beguz and enduz
    MOVE-CORRESPONDING icatsbegend TO catsd_new.
  ENDIF.

ENDFORM.                               " MODIFY_ICATSD_AFTER_CHANGE
