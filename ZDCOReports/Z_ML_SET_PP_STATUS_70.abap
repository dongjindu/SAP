*&---------------------------------------------------------------------*
*& Report  Z_ML_SET_PP_STATUS_70                                       *
*&                                                                     *
*&---------------------------------------------------------------------*
*&  check MLHELP_DELETE_UMBEW before run.
*&---------------------------------------------------------------------*
* ANDY
REPORT  Z_ML_SET_PP_STATUS_70 LINE-SIZE 173 MESSAGE-ID c+.

TYPE-POOLS: ckmh1, slis.

TABLES:
  ckmlhd,
  sscrfields,
  mara,
  ckmlpp.

*DATA: stattext(30) type C.

SELECT-OPTIONS:
  r_matnr FOR ckmlhd-matnr MEMORY ID mat MODIF ID man.


PARAMETERS:
  p_bwkey LIKE t001k-bwkey MEMORY ID wrk MODIF ID man OBLIGATORY.
SELECT-OPTIONS:
  r_bwtar FOR ckmlhd-bwtar MEMORY ID bwt MODIF ID man,
  r_sobkz FOR ckmlhd-sobkz MEMORY ID sob MODIF ID man.

SELECTION-SCREEN SKIP.

PARAMETERS:
  p_bdatj LIKE ckmlpp-bdatj MEMORY ID mlj MODIF ID man OBLIGATORY,
  p_poper LIKE ckmlpp-poper MEMORY ID mlp MODIF ID man OBLIGATORY.

SELECTION-SCREEN SKIP.

SELECT-OPTIONS:
  r_mtart FOR mara-mtart MEMORY ID mta MODIF ID man.
SELECT-OPTIONS:
  r_status FOR ckmlpp-status MEMORY ID ast.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(17) text-003.
CONSTANTS:
  p_status LIKE ckmlpp-status VALUE '70'.
SELECTION-SCREEN COMMENT 19(2) t_status.
SELECTION-SCREEN COMMENT 23(1) text-023.
SELECTION-SCREEN COMMENT 25(23) stattext.
SELECTION-SCREEN COMMENT 50(1) text-024.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

PARAMETERS:
  p_list AS CHECKBOX DEFAULT 'X',
  p_test AS CHECKBOX DEFAULT 'X'.

TYPE-POOLS:
  ckmv0.

DATA:
  lt_kalnr_all TYPE ckmv0_matobj_str OCCURS 0 WITH HEADER LINE,
  lt_kalnr_pack TYPE ckmv0_matobj_str OCCURS 0 WITH HEADER LINE,
  lt_ckmlpp LIKE ckmlpp OCCURS 0,
  lt_old_ckmlpp LIKE ckmlpp OCCURS 0 WITH HEADER LINE,
  lt_new_ckmlpp LIKE ckmlpp OCCURS 0 WITH HEADER LINE,
  lt_ckmlcr LIKE ckmlcr OCCURS 0,
  lt_old_ckmlcr LIKE ckmlcr OCCURS 0 WITH HEADER LINE,
  lt_new_ckmlcr LIKE ckmlcr OCCURS 0 WITH HEADER LINE,
  gd_mara_tbl   LIKE mara   OCCURS 0.

DATA:
  lt_dd07v LIKE dd07v OCCURS 0 WITH HEADER LINE.

DATA:  gd_master TYPE boole-boole.
DATA:  gd_dontpanic LIKE sy-datlo.

DATA:  pp_count_changed TYPE i,
       pp_count_created TYPE i,
       cr_count_created TYPE i.

DATA:  lh_counter TYPE i,
       lh_total_lines TYPE i,
       cr_count_old TYPE i,
       cr_count_new TYPE i.

DATA:  ausgz TYPE boole-boole,
       corrected type boole-boole.

DATA:  ckmlpp_old LIKE ckmlpp,
       ckmlpp_new LIKE ckmlpp.

FIELD-SYMBOLS:
  <kalnr> TYPE ckmv0_matobj_str,
  <ckmlpp> LIKE ckmlpp.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*

  CALL FUNCTION 'DD_DD07V_GET'
       EXPORTING
            domain_name    = 'CK_MLSTAT'
            langu          = sy-langu
            withtext       = 'X'
       TABLES
            dd07v_tab      = lt_dd07v
       EXCEPTIONS
            access_failure = 1
            OTHERS         = 2.
  READ TABLE lt_dd07v WITH KEY domvalue_l = p_status.
  stattext = lt_dd07v-ddtext.
  t_status = p_status.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

  CLEAR: pp_count_changed, pp_count_created, cr_count_created,
         corrected.

    IF r_matnr[] IS INITIAL.
*   if no materials selected: select materials generically
      r_matnr-sign = 'I'.
      r_matnr-option = 'CP'.
      r_matnr-low = '*'.
      APPEND r_matnr.
    ENDIF.

* Pre-Selection: Nur Materialien mit den angegebenen Select-Options
    SELECT * FROM mara
             INTO CORRESPONDING FIELDS OF TABLE gd_mara_tbl
             WHERE mtart IN r_mtart
             AND   matnr IN r_matnr.

* Selektion der zu bearbeitenden Materialien
    IF NOT gd_mara_tbl[] IS INITIAL.
      SELECT * FROM ckmlhd
               INTO CORRESPONDING FIELDS OF TABLE lt_kalnr_all
               FOR ALL ENTRIES IN gd_mara_tbl
               WHERE matnr EQ gd_mara_tbl-matnr
               AND   bwkey EQ p_bwkey
               AND   bwtar IN r_bwtar
               AND   sobkz IN r_sobkz.
    ENDIF.


  SORT lt_kalnr_all by kalnr.

  DESCRIBE TABLE lt_kalnr_all LINES lh_total_lines.

  CHECK lh_total_lines GT 0.

  IF NOT lh_total_lines GT 100.
    APPEND LINES OF lt_kalnr_all TO lt_kalnr_pack.
    PERFORM process_kalnr_package.
  ELSE.
    CLEAR lh_counter. REFRESH lt_kalnr_pack.
    LOOP AT lt_kalnr_all.
      APPEND lt_kalnr_all TO lt_kalnr_pack.
      lh_counter = lh_counter + 1.
      IF lh_counter GE 100.
        PERFORM process_kalnr_package.
        CLEAR lh_counter. REFRESH lt_kalnr_pack.
      ENDIF.
    ENDLOOP.
    PERFORM process_kalnr_package.
  ENDIF.

  PERFORM write_statistics.


************************************************************************
TOP-OF-PAGE.
************************************************************************
If not p_list is initial.

WRITE: /30 'MATNR',
        49 'BWKEY',
        55 'POPER',
        61 'BDATJ',
        67 'UNTPER',
        74 'STATUS/CURTP',
       102 'BWTAR',
       113 'SOBKZ',
       119 'VBELN',
       130 'POSNR',
       137 'PSPNR',
       162 'LIFNR'.
ENDIF.

*---------------------------------------------------------------------*
*       FORM PROCESS_KALNR_PACKAGE                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM process_kalnr_package.

  CLEAR: cr_count_old, cr_count_new.

* Periodens?ze lesen
  CALL FUNCTION 'CKMS_BUFFER_REFRESH_COMPLETE'.
  CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
       EXPORTING
            i_bdatj_1                 = p_bdatj
            i_poper_1                 = p_poper
            i_no_chk_periods_complete = 'X'
       TABLES
            t_kalnr                   = lt_kalnr_pack
            t_ckmlpp                  = lt_ckmlpp
            t_ckmlcr                  = lt_ckmlcr
       EXCEPTIONS
            no_data_found             = 1
            input_data_inconsistent   = 2
            buffer_inconsistent       = 3
            OTHERS                    = 4.
  IF sy-subrc <> 0.
    MESSAGE e101 WITH text-009.
    " Error while reading data
    STOP.
  ENDIF.

* Puffer auslesen
  CALL FUNCTION 'CKMS_BUFFER_GET'
       TABLES
            ot_old_ckmlpp = lt_old_ckmlpp
            ot_new_ckmlpp = lt_new_ckmlpp
            ot_old_ckmlcr = lt_old_ckmlcr
            ot_new_ckmlcr = lt_new_ckmlcr.

  IF r_status[] IS INITIAL.
* Status umschie?n f? alle
    LOOP AT lt_ckmlpp ASSIGNING <ckmlpp>.
      PERFORM set_stat_write_correction_prot.
    ENDLOOP.
    UNASSIGN <ckmlpp>.
  ELSE.
* Status umschie?n f? bestimten aktuellen Status
    LOOP AT lt_ckmlpp ASSIGNING <ckmlpp> WHERE status IN r_status.
      PERFORM set_stat_write_correction_prot.
    ENDLOOP.
    UNASSIGN <ckmlpp>.
  ENDIF.


  DESCRIBE TABLE lt_old_ckmlcr LINES cr_count_old.
  DESCRIBE TABLE lt_new_ckmlcr LINES cr_count_new.
* Abgleich Anzahl S?ze lt_old_ckmlcr <-> lt_new_ckmlcr
  IF cr_count_new > cr_count_old.
    LOOP AT lt_new_ckmlcr.
      READ TABLE lt_old_ckmlcr WITH KEY kalnr  = lt_new_ckmlcr-kalnr
                                        poper  = lt_new_ckmlcr-poper
                                        bdatj  = lt_new_ckmlcr-bdatj
                                        untper = lt_new_ckmlcr-untper
                                        curtp  = lt_new_ckmlcr-curtp
                               BINARY SEARCH.
      IF sy-subrc <> 0.
        cr_count_created = cr_count_created + 1.
        IF p_test IS INITIAL.
          corrected = 'X'.
          IF p_list = 'X'.
            READ TABLE lt_kalnr_all
                 with key kalnr  = lt_new_ckmlcr-kalnr
                 BINARY SEARCH.
            WRITE: / 'CKMLCR', text-021,      "record created:
                   30 lt_kalnr_all-matnr,
                   49 lt_kalnr_all-bwkey,
                   55 lt_new_ckmlcr-poper,
                   61 lt_new_ckmlcr-bdatj,
                   67 lt_new_ckmlcr-untper,
                   74 lt_new_ckmlcr-curtp,
                  102 lt_kalnr_all-bwtar,
                  113 lt_kalnr_all-sobkz,
                  119 lt_kalnr_all-vbeln,
                  130 lt_kalnr_all-posnr,
                  137 lt_kalnr_all-pspnr,
                  162 lt_kalnr_all-lifnr.

          endif.
        ELSE.
          IF p_list = 'X'.
            READ TABLE lt_kalnr_all
                 with key kalnr  = lt_new_ckmlcr-kalnr
                 BINARY SEARCH.
            WRITE: / 'CKMLCR', text-022,      "record to be created:
                   30 lt_kalnr_all-matnr,
                   49 lt_kalnr_all-bwkey,
                   55 lt_new_ckmlcr-poper,
                   61 lt_new_ckmlcr-bdatj,
                   67 lt_new_ckmlcr-untper,
                   74 lt_new_ckmlcr-curtp,
                  102 lt_kalnr_all-bwtar,
                  113 lt_kalnr_all-sobkz,
                  119 lt_kalnr_all-vbeln,
                  130 lt_kalnr_all-posnr,
                  137 lt_kalnr_all-pspnr,
                  162 lt_kalnr_all-lifnr.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF p_test IS INITIAL.
* In Puffer schreiben
    CALL FUNCTION 'CKMS_BUFFER_WRITE'
         EXPORTING
              write_mode            = 'M'
              build_smbew           = space
         TABLES
              t_ckmlpp              = lt_ckmlpp
         EXCEPTIONS
              buffer_data_not_found = 1
              entry_already_there   = 2
              write_not_successful  = 3
              OTHERS                = 4.
    IF sy-subrc <> 0.
      MESSAGE e101 WITH text-010.
      "Error while writing buffer
      STOP.
    ENDIF.

* Puffer auslesen
    CALL FUNCTION 'CKMS_BUFFER_GET'
         TABLES
              ot_old_ckmlpp = lt_old_ckmlpp
              ot_new_ckmlpp = lt_new_ckmlpp
              ot_old_ckmlcr = lt_old_ckmlcr
              ot_new_ckmlcr = lt_new_ckmlcr.

* Periodens?ze verbuchen
    CALL FUNCTION 'CKMS_BUFFER_POST_PERIODS'
         EXPORTING
              in_update_task = 'X'
         TABLES
              in_old_ckmlpp  = lt_old_ckmlpp
              in_new_ckmlpp  = lt_new_ckmlpp
              in_old_ckmlcr  = lt_old_ckmlcr
              in_new_ckmlcr  = lt_new_ckmlcr.

**** Ohne Exceptions kein sy-subrc!
*    IF sy-subrc <> 0.
*      MESSAGE e101 WITH text-011.
*      "Error while posting changes
*      STOP.
*    ENDIF.
     CLEAR: corrected.

* Verbuchung antriggern
    COMMIT WORK.

  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM WRITE_STATISTICS                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM write_statistics.
  SKIP.
  WRITE: / lh_total_lines, text-004.
  " KALNRs have been selected.
  IF p_test IS INITIAL.
    WRITE: / pp_count_changed,  'CKMLPP', text-005.
    "records have been changed.
    HIDE ausgz.
    WRITE: / pp_count_created,  'CKMLPP', text-006.
    "records have been created.
    HIDE ausgz.
    WRITE: / cr_count_created,  'CKMLCR', text-006.
    "records have been created.
  ELSE.
    WRITE: / pp_count_changed,  'CKMLPP', text-007.
    "records would be changed.
    HIDE ausgz.
    WRITE: / pp_count_created,  'CKMLPP', text-008.
    "records would be created.
    HIDE ausgz.
    WRITE: / cr_count_created,  'CKMLCR', text-008.
    "records would be created.
    HIDE ausgz.
  ENDIF.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM SET_STAT_WRITE_CORRECTION_PROT                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set_stat_write_correction_prot.


  <ckmlpp>-status = p_status.

  READ TABLE lt_old_ckmlpp WITH KEY kalnr  = <ckmlpp>-kalnr
                                    poper  = <ckmlpp>-poper
                                    bdatj  = <ckmlpp>-bdatj
                                    untper = <ckmlpp>-untper
                                    BINARY SEARCH.
  IF sy-subrc = 0.
    IF p_test IS INITIAL.
      corrected = 'X'.
      IF p_list = 'X'.
        READ TABLE lt_kalnr_all
             with key kalnr  = lt_old_ckmlpp-kalnr
             BINARY SEARCH.
        WRITE: / 'CKMLPP', text-019, "record changed:
               30 lt_kalnr_all-matnr,
               49 lt_kalnr_all-bwkey,
               55 lt_old_ckmlpp-poper,
               61 lt_old_ckmlpp-bdatj,
               67 lt_old_ckmlpp-untper,
               74 lt_old_ckmlpp-status,
                  lt_old_ckmlpp-lbkum,
                  lt_old_ckmlpp-meins,
              102 lt_kalnr_all-bwtar,
              113 lt_kalnr_all-sobkz,
              119 lt_kalnr_all-vbeln,
              130 lt_kalnr_all-posnr,
              137 lt_kalnr_all-pspnr,
              162 lt_kalnr_all-lifnr.

      ENDIF.
    ELSE.
      IF p_list = 'X'.
        READ TABLE lt_kalnr_all
             with key kalnr  = lt_old_ckmlpp-kalnr
             BINARY SEARCH.
        WRITE: / 'CKMLPP', text-020,  "record to be changed:
               30 lt_kalnr_all-matnr,
               49 lt_kalnr_all-bwkey,
               55 lt_old_ckmlpp-poper,
               61 lt_old_ckmlpp-bdatj,
               67 lt_old_ckmlpp-untper,
               74 lt_old_ckmlpp-status,
                  lt_old_ckmlpp-lbkum,
                  lt_old_ckmlpp-meins,
              102 lt_kalnr_all-bwtar,
              113 lt_kalnr_all-sobkz,
              119 lt_kalnr_all-vbeln,
              130 lt_kalnr_all-posnr,
              137 lt_kalnr_all-pspnr,
              162 lt_kalnr_all-lifnr.

      ENDIF.
    ENDIF.
    pp_count_changed = pp_count_changed + 1.
  ELSE.
    IF p_test IS INITIAL.
      corrected = 'X'.
      IF p_list = 'X'.
        READ TABLE lt_kalnr_all
             with key kalnr  = <ckmlpp>-kalnr
             BINARY SEARCH.
        WRITE: / 'CKMLPP', text-021,  "record created:
               30 lt_kalnr_all-matnr,
               49 lt_kalnr_all-bwkey,
               55 <ckmlpp>-poper,
               61 <ckmlpp>-bdatj,
               67 <ckmlpp>-untper,
               74 <ckmlpp>-status,
                  <ckmlpp>-lbkum,
                  <ckmlpp>-meins,
              102 lt_kalnr_all-bwtar,
              113 lt_kalnr_all-sobkz,
              119 lt_kalnr_all-vbeln,
              130 lt_kalnr_all-posnr,
              137 lt_kalnr_all-pspnr,
              162 lt_kalnr_all-lifnr.

      endif.
    ELSE.
      IF p_list = 'X'.
        READ TABLE lt_kalnr_all
             with key kalnr  = <ckmlpp>-kalnr
             BINARY SEARCH.
        WRITE: / 'CKMLPP', text-022,  "record to be created:
               30 lt_kalnr_all-matnr,
               49 lt_kalnr_all-bwkey,
               55 <ckmlpp>-poper,
               61 <ckmlpp>-bdatj,
               67 <ckmlpp>-untper,
               74 <ckmlpp>-status,
                  <ckmlpp>-lbkum,
                  <ckmlpp>-meins,
              102 lt_kalnr_all-bwtar,
              113 lt_kalnr_all-sobkz,
              119 lt_kalnr_all-vbeln,
              130 lt_kalnr_all-posnr,
              137 lt_kalnr_all-pspnr,
              162 lt_kalnr_all-lifnr.

      ENDIF.
    ENDIF.
    pp_count_created  = pp_count_created + 1.
  ENDIF.

ENDFORM.
