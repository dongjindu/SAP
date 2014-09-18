*&---------------------------------------------------------------------*
*& Report  ZMDF_GM_AFFWRESB                                            *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zmdf_gm_affwresb LINE-SIZE 255.
*&---------------------------------------------------------------------*
*& PLEASE START THE CORRECTION REPORT Z_PPC_CORRECT_AFFW (note 730504) *
*& BEFORE YOU START THIS REPORT                                        *
*&---------------------------------------------------------------------*

TYPE-POOLS slis.
TYPES:
  BEGIN OF ty_affwresb,
    weblnr  TYPE affw-weblnr,
    weblpos TYPE affw-weblpos,
    rsnum   TYPE affw-rsnum,
    rspos   TYPE affw-rspos,
    c_matnr TYPE affw-matnr,
    c_werks TYPE affw-werks,
    c_lgort TYPE affw-lgort,
    c_charg TYPE affw-charg,
    c_sobkz TYPE affw-sobkz,
    c_bwart TYPE affw-bwart,
    c_erfme TYPE affw-erfme,
    r_matnr TYPE affw-matnr,
    r_werks TYPE affw-werks,
    r_lgort TYPE affw-lgort,
    r_charg TYPE affw-charg,
    r_sobkz TYPE affw-sobkz,
    r_bwart TYPE affw-bwart,
    r_erfme TYPE affw-erfme,
  END OF ty_affwresb,
  ty_tab_affwresb TYPE TABLE OF ty_affwresb.
DATA:
  gf_matnr  TYPE matnr,
  gf_werks  TYPE werks_d,
  gf_rsnum  TYPE rsnum.
*-------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS:
  so_matnr  FOR gf_matnr,
  so_werks  FOR gf_werks.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS:
  p_check1 AS CHECKBOX DEFAULT 'X',
  p_check2 AS CHECKBOX DEFAULT 'X',
  p_check3 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME.
PARAMETERS:
  pa_test1 AS CHECKBOX DEFAULT 'X',
  pa_test2 AS CHECKBOX DEFAULT 'X',
  pa_test3 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b3.
*-------------------------------------------

FIELD-SYMBOLS:
  <fs_affw> TYPE affw,
  <fs_resb> TYPE resb.
RANGES:
  so_rsnum  FOR gf_rsnum.
DATA:
  ls_resb   TYPE resb,
  lt_affw   TYPE TABLE OF affw,
  lt_affw_u TYPE TABLE OF affw,
  lt_resb   TYPE TABLE OF resb,
  lt_resb_i TYPE TABLE OF resb,
  lt_resb_u TYPE TABLE OF resb,
  lt_resb_d TYPE TABLE OF resb,
  lt_output TYPE ty_tab_affwresb,
  lt_resco LIKE TABLE OF resco WITH HEADER LINE,
  l_bigselect(1) TYPE c.

INITIALIZATION.
  CLEAR: so_rsnum, lt_resco.
  REFRESH: lt_resb_d,lt_resb_i,lt_resb_u,lt_resco,lt_affw,so_rsnum,
           lt_resb, lt_affw_u.

START-OF-SELECTION.
 IF p_check1 IS INITIAL AND p_check2 IS INITIAL AND p_check3 IS INITIAL.
*   in case background start we have to see the message in the log
    WRITE: /1 'No check was selected'.
    EXIT.
  ENDIF.
  IF pa_test1 IS INITIAL OR pa_test2 IS INITIAL OR pa_test3 IS INITIAL .
*   question?
    DATA l_answer(1).
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
         EXPORTING
              titel          = 'Confirmation'
              textline1      = 'This is not a test run'
              textline2      = 'Do you want to continue?'
              cancel_display = space
         IMPORTING
              answer         = l_answer.
    IF l_answer = 'N' OR
       l_answer = 'A'.
      EXIT.
    ENDIF.
  ENDIF.
  IF so_matnr[] IS INITIAL AND so_werks[] IS INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
         EXPORTING
              titel          = 'Confirmation'
              textline1      =
'Without restrictions, a very long runtime expected for this selection.'
              textline2      = 'Do you want to continue?'
              cancel_display = space
         IMPORTING
              answer         = l_answer.
    IF l_answer = 'N' OR
       l_answer = 'A'.
      EXIT.
    ENDIF.
    l_bigselect = 'X'.
  ENDIF.

* lock the objects
  IF pa_test1 IS INITIAL OR pa_test2 IS INITIAL OR pa_test3 IS INITIAL .
* lock the entries
    CALL FUNCTION 'ENQUEUE_ESAFFW'
         EXPORTING
              mode_affw      = 'E'
              mandt          = sy-mandt
              _scope         = '3'
              _wait          = 'X'
         EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
    IF sy-subrc NE 0.
      MESSAGE e167(rm).
    ENDIF.
* LOCK PPCGO RUN
    CALL FUNCTION 'ENQUEUE_E_PPC_CONF_MAT'
         EXPORTING
              mode_ppc_conf_mat = 'E'
              mandt             = sy-mandt
              _scope            = '3'
              _wait             = 'X'
         EXCEPTIONS
              foreign_lock      = 1
              system_failure    = 2
              OTHERS            = 3.
    IF sy-subrc NE 0.
      MESSAGE e020(ppc1pr) WITH sy-msgv1 RAISING enqueue_error.
    ENDIF.
    CALL FUNCTION 'ENQUEUE_E_PPC_RES_HDR'
         EXPORTING
              mode_ppc_res_hdr = 'E'
              mandt            = sy-mandt
              _scope           = '3'
              _wait            = 'X'
         EXCEPTIONS
              foreign_lock     = 1
              system_failure   = 2
              OTHERS           = 3.
    IF sy-subrc NE 0.
      MESSAGE e020(ppc1pr) WITH sy-msgv1
      RAISING enqueue_error.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* PART 1:                                                              *
*         -Check the wrong reservations without reprocessing records   *
*           RESB entry without AFFW -> delete them                     *
*----------------------------------------------------------------------*
  IF NOT p_check1 IS INITIAL.
    WRITE: /1 'Part 1. Check wrong reservations'.

    SELECT * INTO TABLE lt_resb
             FROM resb
             WHERE matnr IN so_matnr
               AND werks IN so_werks
               AND bdart = 'SB'
               AND plnum = '          '
             ORDER BY rsnum rspos.
    IF sy-subrc IS INITIAL.
      IF NOT lt_resb[] IS INITIAL.
*   records against run schedule headers = records from <= 4.0B
        SELECT rsnum FROM resco
               INTO CORRESPONDING FIELDS OF TABLE lt_resco
               FOR ALL ENTRIES IN lt_resb
               WHERE rsnum = lt_resb-rsnum.
      ENDIF.
      IF sy-subrc IS INITIAL.
        LOOP AT lt_resco.
          READ TABLE lt_resb WITH KEY rsnum = lt_resco-rsnum
               BINARY SEARCH TRANSPORTING NO FIELDS.
          CHECK sy-subrc IS INITIAL.
          DELETE lt_resb FROM sy-tabix WHERE rsnum = lt_resco-rsnum.
        ENDLOOP.
      ENDIF.

      IF NOT lt_resb[] IS INITIAL.
*   check the entries corresponding entries in the table AFFW
        SELECT * FROM affw INTO TABLE lt_affw
                 FOR ALL ENTRIES IN lt_resb
                 WHERE rsnum EQ lt_resb-rsnum
                 AND rspos EQ lt_resb-rspos.
      ENDIF.
      SORT lt_affw BY rsnum rspos.
      LOOP AT lt_resb ASSIGNING <fs_resb>.
        READ TABLE lt_affw TRANSPORTING NO FIELDS
                           WITH KEY rsnum = <fs_resb>-rsnum
                                    rspos = <fs_resb>-rspos
                           BINARY SEARCH.
        IF NOT sy-subrc IS INITIAL.
*       for the reservation there is no AFFW entry -> wrong reservation
          WRITE: /1 'RESB w/o AFFW entry:',
                    <fs_resb>-rsnum,
                    <fs_resb>-rspos,
                    <fs_resb>-matnr,
                    <fs_resb>-werks.
          APPEND <fs_resb> TO lt_resb_d.
        ENDIF.
      ENDLOOP.

      IF lt_resb_d[] IS INITIAL.
        WRITE: /1 'No inconsistency detected.'.
      ELSEIF pa_test1 IS INITIAL.
        DELETE resb FROM TABLE lt_resb_d.
        PERFORM send_reservation USING 'D'
                                       lt_resb_d.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDIF. " if not p_check1 is initial.

*----------------------------------------------------------------------*
* PART 2:                                                              *
*         -Compare the key fields of the tables AFFW-RESB              *
*           AFFW entry should be the right one                         *
*           do not check the quantity                                  *
*----------------------------------------------------------------------*
  IF NOT p_check2 IS INITIAL.
    SKIP.
    WRITE: /1 'Part 2. Check the key fields of AFFW/RESB'.

    REFRESH: lt_resb_d, lt_resb, lt_resco, lt_affw.
    FREE:  lt_resb_d, lt_resb, lt_resco, lt_affw.

* Get affw entries for selected materials
    SELECT * FROM affw
      INTO TABLE lt_affw
      WHERE matnr IN so_matnr
        AND werks IN so_werks
        AND flg_orig = '1'.

* Get referrenced resb entries
    so_rsnum-sign = 'I'.
    so_rsnum-option = 'EQ'.
    LOOP AT lt_affw ASSIGNING <fs_affw>.
      so_rsnum-low = <fs_affw>-rsnum.
      COLLECT so_rsnum.
    ENDLOOP.

  IF NOT so_rsnum[] IS INITIAL. "otherwise the system selects everything
* select the reservations
      SELECT * FROM resb
        INTO TABLE lt_resb
        WHERE rsnum IN so_rsnum AND
              bdart EQ 'SB'.
      SORT lt_resb BY rsnum rspos.
    ENDIF.

* Start comparing entries
    LOOP AT lt_affw ASSIGNING <fs_affw>.

*   ... get referenced resb line
      READ TABLE lt_resb WITH KEY rsnum = <fs_affw>-rsnum
                                  rspos = <fs_affw>-rspos
                                  ASSIGNING <fs_resb>
                                  BINARY SEARCH.
      IF NOT sy-subrc IS INITIAL.
*     no resb entry for this affw!!!
*     create the missing entry
        PERFORM create_new_resb USING    <fs_affw>
                                CHANGING lt_resb_i
                                         lt_affw_u
                                         lt_output.
        CONTINUE. "to next pass
      ENDIF.

*   .. do the comparison of the key fields
      IF NOT ( <fs_affw>-matnr = <fs_resb>-matnr AND
               <fs_affw>-werks = <fs_resb>-werks AND
               <fs_affw>-lgort = <fs_resb>-lgort AND
               <fs_affw>-charg = <fs_resb>-charg AND
               <fs_affw>-sobkz = <fs_resb>-sobkz AND
               <fs_affw>-bwart = <fs_resb>-bwart AND
               <fs_affw>-erfme = <fs_resb>-erfme ).
*     entries do not match! change the RESB entry!
        PERFORM modif_wrong_resb USING    lt_affw
                                 CHANGING <fs_resb>
                                          <fs_affw>
                                          lt_resb_i
                                          lt_resb_u
                                          lt_affw_u
                                          lt_output.
      ENDIF.

    ENDLOOP. "end of comparison


    IF lt_resb_i IS INITIAL AND lt_resb_u IS INITIAL.

      WRITE: /1 'No inconsistency detected.'.

    ELSEIF pa_test2 IS INITIAL.

*   Now we have to modify database
      IF NOT lt_resb_i IS INITIAL.
*     check whether RSPOS are used. shouldn't be because we locked
        SELECT * FROM resb INTO TABLE lt_resb
                 FOR ALL ENTRIES IN lt_resb_i
                 WHERE rsnum EQ lt_resb_i-rsnum
                   AND rspos EQ lt_resb_i-rspos.
        LOOP AT lt_resb ASSIGNING <fs_resb>.
          DELETE lt_resb_i WHERE rsnum EQ <fs_resb>-rsnum
                             AND rspos EQ <fs_resb>-rspos.
          WRITE: /1 'Please check RESB entry manually',
                    <fs_resb>-rsnum,
                    <fs_resb>-rspos,
                    <fs_resb>-matnr.
        ENDLOOP.
*     INSERT THE RESB ENTRIES
        INSERT resb FROM TABLE lt_resb_i.
        PERFORM send_reservation USING 'I'
                                       lt_resb_i.
      ENDIF.
      IF NOT lt_resb_u IS INITIAL.
*     UPDATE THE RESB ENTRIES
        UPDATE resb FROM TABLE lt_resb_u.
        PERFORM send_reservation USING 'U'
                                       lt_resb_u.
      ENDIF.
      IF NOT lt_affw_u[] IS INITIAL.
*     update AFFW entries with new RSPOS
        UPDATE affw FROM TABLE lt_affw_u.
      ENDIF.
      COMMIT WORK.

    ENDIF.

* do protocol:
    IF NOT lt_resb_i[] IS INITIAL.
      SKIP.
      WRITE: /1 'Following entries were inserted into table RESB'.
      LOOP AT lt_resb_i ASSIGNING <fs_resb>.
        PERFORM write_resb USING lt_affw
                                 <fs_resb>.
      ENDLOOP.
    ENDIF.

    IF NOT lt_resb_u[] IS INITIAL.
      SKIP.
      WRITE: /1 'Following entries were updated in table RESB'.
      LOOP AT lt_resb_u ASSIGNING <fs_resb>.
        PERFORM write_resb USING lt_affw
                                 <fs_resb>.
      ENDLOOP.

      IF NOT lt_affw_u[] IS INITIAL.
        SKIP.
        WRITE: /1 'Following RSNUMs were updated in table AFFW'.
        LOOP AT lt_affw_u ASSIGNING <fs_affw>.
          WRITE: /1 <fs_affw>-rsnum,
                    <fs_affw>-rspos,
                    <fs_affw>-matnr,
                    <fs_affw>-werks.
        ENDLOOP.

      ENDIF.
    ENDIF.
  ENDIF. " if not p_check2 is initial.
*----------------------------------------------------------------------*
* PART 3:                                                              *
*         -Check the quantity between the tables AFFW-RESB             *
*           AFFW entry should be the right one                         *
*----------------------------------------------------------------------*
  IF NOT p_check3 IS INITIAL.
    SKIP.
    WRITE: /1 'Part 3. Check quantity'.

    REFRESH: lt_resb_d, lt_resb_i, lt_resb_u, lt_resb, lt_affw.
    FREE: lt_resb_d, lt_resb_i, lt_resb_u, lt_resb, lt_affw.

    DATA: lf_quant   TYPE erfmg,
          lf_redflag TYPE c.

* Get affw entries for selected materials
    SELECT * FROM affw
      INTO TABLE lt_affw
      WHERE matnr IN so_matnr
        AND werks IN so_werks
        AND flg_orig = '1'.

    IF NOT lt_affw[] IS INITIAL.
      SELECT * FROM resb
        INTO TABLE lt_resb
        FOR ALL ENTRIES IN lt_affw
        WHERE rsnum EQ lt_affw-rsnum
         AND  rspos EQ lt_affw-rspos
         AND  bdart EQ 'SB'.
    ENDIF.
    SORT lt_resb BY rsnum rspos.
    SORT lt_affw BY rsnum rspos.

* for each resb entry (individual component...)
    LOOP AT lt_resb ASSIGNING <fs_resb>.

      CLEAR: lf_quant, lf_redflag.
*   1. get individual affw entries
      LOOP AT lt_affw ASSIGNING <fs_affw>
                WHERE rsnum = <fs_resb>-rsnum
                  AND rspos = <fs_resb>-rspos.
        ADD <fs_affw>-erfmg TO lf_quant.
*     double check consistency affw-resb
        IF NOT ( <fs_affw>-matnr = <fs_resb>-matnr AND
                 <fs_affw>-werks = <fs_resb>-werks AND
                 <fs_affw>-lgort = <fs_resb>-lgort AND
                 <fs_affw>-charg = <fs_resb>-charg AND
                 <fs_affw>-sobkz = <fs_resb>-sobkz AND
                 <fs_affw>-bwart = <fs_resb>-bwart AND
                 <fs_affw>-erfme = <fs_resb>-erfme ).
*       key fields should have been corrected!
          lf_redflag = 'X'.
        ENDIF.
      ENDLOOP. "loop at referenced affw entries
*   2. check errors
      IF NOT lf_redflag IS INITIAL.
*    consistency error (wrong key fields)
        WRITE: /1 'CHECK ERROR MANUALLY',
                   <fs_resb>-rsnum,
                   <fs_resb>-rspos,
                   <fs_resb>-matnr.
      ELSEIF NOT ( lf_quant = <fs_resb>-bdmng AND
                   lf_quant = <fs_resb>-erfmg ).
*     quantity error
        PERFORM modif_wrong_resb_with_q USING    lf_quant
                                        CHANGING <fs_resb>
                                                 lt_resb_u.
      ENDIF.

    ENDLOOP. "loop at resb entries


    IF lt_resb_u IS INITIAL.

      WRITE: /1 'No inconsistency detected.'.

    ELSEIF pa_test3 IS INITIAL.

*   ... correct wrong stuff
      IF NOT lt_resb_u IS INITIAL.
*     UPDATE THE RESB ENTRIES with quantity
        UPDATE resb FROM TABLE lt_resb_u.
        PERFORM send_reservation USING 'U'
                                       lt_resb_u.
      ENDIF.
      COMMIT WORK.

    ENDIF.

* do protocol:
    IF NOT lt_resb_u[] IS INITIAL.
      SKIP.
    WRITE: /1 'Following entries were updated in table RESB (quantity)'.
      LOOP AT lt_resb_u ASSIGNING <fs_resb>.
        PERFORM write_resb USING lt_affw
                                 <fs_resb>.
      ENDLOOP.
    ENDIF.

  ENDIF. " if not p_check3 is initial.


  IF pa_test1 IS INITIAL OR pa_test2 IS INITIAL OR pa_test3 IS INITIAL.
*   unlock the locking objects
    CALL FUNCTION 'DEQUEUE_ESAFFW'
         EXPORTING
              mode_affw = 'E'
              mandt     = sy-mandt.
    CALL FUNCTION 'DEQUEUE_ESAFFW'
         EXPORTING
              mode_affw = 'E'
              mandt     = sy-mandt
              _scope    = '3'.
    CALL FUNCTION 'DEQUEUE_E_PPC_CONF_MAT'
         EXPORTING
              mode_ppc_conf_mat = 'E'
              mandt             = sy-mandt
              _scope            = '3'
              _synchron         = ' '
              _collect          = ' '.
    CALL FUNCTION 'DEQUEUE_E_PPC_RES_HDR'
         EXPORTING
              mode_ppc_res_hdr = 'E'
              mandt            = sy-mandt
              _scope           = '3'.
  ELSE.
    MESSAGE s999(pp) WITH 'Nothing has been posted, as requested.'.
  ENDIF.


*---------------------------------------------------------------------*
*       FORM create_new_resb                                          *
*---------------------------------------------------------------------*
FORM create_new_resb  USING    is_affw   TYPE affw
                      CHANGING et_resb_i LIKE lt_resb
                               et_affw_u LIKE lt_affw
                               et_output TYPE ty_tab_affwresb.
  DATA:
    ls_resb TYPE resb.

  CLEAR ls_resb.

* basically we have to create a new entry for is_affw-rsnum and -rspos
* but we have to check whether this entry has been already created
* by a previous affw entry -> to avoid duplicate_insert dumps
  READ TABLE et_resb_i
      INTO ls_resb
      WITH KEY rsnum = is_affw-rsnum
               rspos = is_affw-rspos.
  IF sy-subrc GT 0.

*   it is safe to create new resb entry
*   it does not exist in database, nor in resb_i
    CLEAR ls_resb.
    MOVE is_affw-rsnum TO ls_resb-rsnum.
    MOVE is_affw-rspos TO ls_resb-rspos.
    MOVE 'SB'          TO ls_resb-bdart.
    MOVE is_affw-matnr TO ls_resb-matnr.
    MOVE is_affw-werks TO ls_resb-werks.
    MOVE is_affw-lgort TO ls_resb-lgort.
    MOVE is_affw-prvbe TO ls_resb-prvbe.
    MOVE is_affw-charg TO ls_resb-charg.
    MOVE is_affw-ersda TO ls_resb-bdter.
    MOVE is_affw-ersda TO ls_resb-sbter.
    MOVE is_affw-erfmg TO ls_resb-bdmng.
    MOVE is_affw-erfme TO ls_resb-meins.
    MOVE is_affw-erfmg TO ls_resb-erfmg.
    MOVE is_affw-erfme TO ls_resb-erfme.
    MOVE is_affw-shkzg TO ls_resb-shkzg.
    MOVE is_affw-aufnr TO ls_resb-aufnr.
    MOVE is_affw-sobkz TO ls_resb-bwart.
    MOVE is_affw-bwart TO ls_resb-bwart.
*   and insert it into resb_i
    APPEND ls_resb TO et_resb_i.

  ELSE.

*   check if this new resb entry is consistent with the affw
    IF is_affw-matnr = ls_resb-matnr AND
           is_affw-werks = ls_resb-werks AND
           is_affw-lgort = ls_resb-lgort AND
           is_affw-charg = ls_resb-charg AND
           is_affw-sobkz = ls_resb-sobkz AND
           is_affw-bwart = ls_resb-bwart AND
           is_affw-erfme = ls_resb-erfme.
*      yes, consistent, then add the qty
      ADD is_affw-erfmg TO ls_resb-bdmng.
      ADD is_affw-erfmg TO ls_resb-erfmg.
      MODIFY et_resb_i INDEX sy-tabix FROM ls_resb.

    ELSE.

*     no, this refer to different material
*     CHANGE ALSO THE AFFW RSPOS because it used already by an other
*     AFFW item!!!
      MOVE 9999 TO is_affw-rspos.
*     check whether this entry is used (hope only by us)
      READ TABLE et_resb_i TRANSPORTING NO FIELDS
                         WITH KEY rsnum = is_affw-rsnum
                                  rspos = is_affw-rspos.
      WHILE sy-subrc IS INITIAL AND
          is_affw-rspos NE 1.
        is_affw-rspos = is_affw-rspos - 1.
        READ TABLE et_resb_i TRANSPORTING NO FIELDS
                             WITH KEY rsnum = is_affw-rsnum
                                      rspos = is_affw-rspos.
      ENDWHILE.
*     please process this entry asap with COGI
      IF is_affw-rspos NE 9999.
        IF is_affw-rspos EQ 1.
*         do not update RESB
          WRITE: /1 'RESB should be checked manually',
                    ls_resb-matnr,
                    ls_resb-rsnum,
                    ls_resb-rspos.
          EXIT.
        ENDIF.
        WRITE: /1 'Please process this entry in COGI asap:',
                  ls_resb-matnr,
                  ls_resb-rsnum,
                  ls_resb-rspos.
      ENDIF.
      PERFORM create_new_resb USING is_affw
                           CHANGING et_resb_i
                                    et_affw_u
                                    et_output.
*     update AFFW rspos
      APPEND is_affw TO et_affw_u.

    ENDIF.  "if the resb entry does not match

  ENDIF. "if the

ENDFORM.                    "create_new_resb

*---------------------------------------------------------------------*
*       FORM modif_wrong_resb                                         *
*---------------------------------------------------------------------*
FORM modif_wrong_resb USING    it_affw   LIKE lt_affw
                      CHANGING cs_resb   TYPE resb
                               cs_affw   TYPE affw
                               et_resb_i LIKE lt_resb
                               et_resb_u LIKE lt_resb
                               et_affw_u LIKE lt_affw
                               et_output TYPE ty_tab_affwresb.

  DATA:
    lf_weblnr TYPE affw-weblnr,
    lf_subrc  TYPE sysubrc.

* basically, we have resb which does not match affw
* we should modify it.
* before that, we must check whether or not this actual entry
* is used in any affw entry (in this case, we shouldn't modify it
* - instead, create a new matching entry.

  SELECT SINGLE weblnr FROM affw INTO lf_weblnr
      WHERE rsnum = cs_resb-rsnum AND
            rspos = cs_resb-rspos AND
            matnr = cs_resb-matnr AND
            werks = cs_resb-werks AND
            lgort = cs_resb-lgort AND
            charg = cs_resb-charg AND
            sobkz = cs_resb-sobkz AND
            bwart = cs_resb-bwart AND
            erfme = cs_resb-erfme AND
            flg_orig = '1'.
  MOVE sy-subrc TO lf_subrc.
  READ TABLE et_affw_u TRANSPORTING NO FIELDS
         WITH KEY rsnum = cs_resb-rsnum
                  rspos = cs_resb-rspos
                  matnr = cs_resb-matnr
                  werks = cs_resb-werks
                  lgort = cs_resb-lgort
                  charg = cs_resb-charg
                  sobkz = cs_resb-sobkz
                  bwart = cs_resb-bwart
                  erfme = cs_resb-erfme.
  IF sy-subrc LT lf_subrc.
    MOVE sy-subrc TO lf_subrc.
  ENDIF.

  IF lf_subrc EQ 0. "or in database, or in affw_u

*   CHANGE ALSO THE AFFW RSPOS because it used already by an other
*   AFFW item!!!
    MOVE 9999 TO cs_affw-rspos.
*   check whether this entry is used (hope only by us)
    READ TABLE et_resb_i TRANSPORTING NO FIELDS
                         WITH KEY rsnum = cs_affw-rsnum
                                  rspos = cs_affw-rspos.
    WHILE sy-subrc IS INITIAL AND
          cs_affw-rspos NE 1.
      cs_affw-rspos = cs_affw-rspos - 1.
      READ TABLE et_resb_i TRANSPORTING NO FIELDS
                           WITH KEY rsnum = cs_affw-rsnum
                                    rspos = cs_affw-rspos.
    ENDWHILE.
*   please process this entry asap with COGI
    IF cs_affw-rspos NE 9999.
      IF cs_affw-rspos EQ 1.
*       do not update RESB
        WRITE: /1 'RESB should be checked manually',
                  cs_resb-matnr,
                  cs_resb-rsnum,
                  cs_resb-rspos.
        EXIT.
      ENDIF.
      WRITE: /1 'Please process this entry in COGI asap:',
                cs_resb-matnr,
                cs_resb-rsnum,
                cs_resb-rspos.
    ENDIF.
    PERFORM create_new_resb USING    cs_affw
                            CHANGING et_resb_i
                                     et_affw_u
                                     et_output.
*   update AFFW rspos
    APPEND cs_affw TO et_affw_u.

  ELSE.

*   this resb entry is wrong, but not used anywhere else
*   then correct the entry!!
    MOVE cs_affw-matnr TO cs_resb-matnr.
    MOVE cs_affw-werks TO cs_resb-werks.
    MOVE cs_affw-lgort TO cs_resb-lgort.
    MOVE cs_affw-charg TO cs_resb-charg.
    MOVE cs_affw-sobkz TO cs_resb-sobkz.
    MOVE cs_affw-bwart TO cs_resb-bwart.
    MOVE cs_affw-erfme TO cs_resb-erfme.
    MOVE cs_affw-erfme TO cs_resb-meins.
    APPEND cs_resb TO et_resb_u.

  ENDIF.

ENDFORM.                    "modif_wrong_resb



*&---------------------------------------------------------------------*
*&      Form  write_resb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_RESB>  text
*----------------------------------------------------------------------*
FORM write_resb  USING it_affw  LIKE lt_affw
                       cs_resb  TYPE resb.

  WRITE: /1 cs_resb-rsnum,
            cs_resb-rspos,
            cs_resb-matnr,
            cs_resb-werks,
            cs_resb-lgort,
            cs_resb-charg,
            cs_resb-sobkz,
            cs_resb-erfmg,
            cs_resb-erfme.

ENDFORM.                    " write_resb
*---------------------------------------------------------------------*
*       FORM modif_wrong_resb_with_q
*
*---------------------------------------------------------------------*
FORM modif_wrong_resb_with_q USING    if_quant  TYPE erfmg
                             CHANGING cs_resb   TYPE resb
                                      et_resb_u LIKE lt_resb.

* just take the sum of quantity of the corresponding AFFW
  cs_resb-erfmg = if_quant.
  cs_resb-bdmng = if_quant.

  APPEND cs_resb TO et_resb_u.

ENDFORM.                    "modif_wrong_resb_with_q
*&---------------------------------------------------------------------*
*&      Form  send_reservation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0581   text
*      -->P_LT_RESB_D  text
*----------------------------------------------------------------------*
FORM send_reservation USING    i_action TYPE char01
                               it_resb LIKE lt_resb.
  DATA: lt_res_hdr LIKE ppc_res_hdr OCCURS 0,
        ls_res_hdr LIKE ppc_res_hdr,
        ls_resb    LIKE resb,
        l_tabix    TYPE sytabix,
        BEGIN OF lt_tab OCCURS 0,
         conflogsys  TYPE ppc_conflogsys,
         flg_info_dest TYPE  ppc_flg_info_dest,
         resb LIKE lt_resb,
       END OF lt_tab.

  SELECT * INTO TABLE lt_res_hdr
           FROM ppc_res_hdr FOR ALL ENTRIES IN it_resb
           WHERE rsnum = it_resb-rsnum.

  LOOP AT lt_res_hdr INTO ls_res_hdr.
    READ TABLE lt_tab WITH KEY conflogsys = ls_res_hdr-conflogsys
                               flg_info_dest = ls_res_hdr-flg_info_dest.
    IF sy-subrc EQ 0.
      l_tabix = sy-tabix.
      LOOP AT it_resb INTO ls_resb.
        IF i_action = 'D'.
          CLEAR: ls_resb-bdmng, ls_resb-erfmg, ls_resb-enmng.
        ENDIF.
        APPEND ls_resb TO lt_tab-resb[].
      ENDLOOP.
      MODIFY lt_tab INDEX l_tabix.
    ELSE.
      lt_tab-conflogsys = ls_res_hdr-conflogsys.
      lt_tab-flg_info_dest = ls_res_hdr-flg_info_dest.
      REFRESH lt_tab-resb.
      LOOP AT it_resb INTO ls_resb.
        IF i_action = 'D'.
          CLEAR: ls_resb-bdmng, ls_resb-erfmg.
        ENDIF.
        APPEND ls_resb TO lt_tab-resb[].
      ENDLOOP.
      APPEND lt_tab.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_tab.
    IF i_action = 'D'.
      CALL FUNCTION 'PPC1IF_RESERV_DISTRIB'
           EXPORTING
                if_flt_val            = lt_tab-flg_info_dest
                if_logdestsys         = lt_tab-conflogsys
           TABLES
                it_dep_require_delete = lt_tab-resb[].
    ELSEIF i_action = 'U'.
      CALL FUNCTION 'PPC1IF_RESERV_DISTRIB'
           EXPORTING
                if_flt_val            = lt_tab-flg_info_dest
                if_logdestsys         = lt_tab-conflogsys
           TABLES
                it_dep_require_change = lt_tab-resb[].
    ELSEIF i_action = 'I'.
      CALL FUNCTION 'PPC1IF_RESERV_DISTRIB'
           EXPORTING
                if_flt_val            = lt_tab-flg_info_dest
                if_logdestsys         = lt_tab-conflogsys
           TABLES
                it_dep_require_create = lt_tab-resb[].
    ENDIF.
  ENDLOOP.

ENDFORM.                    " send_reservation
