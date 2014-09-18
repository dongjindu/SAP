*&---------------------------------------------------------------------*
*& Report  Z_CORR_PURCOND                                              *
*&                                                                     *
*&---------------------------------------------------------------------*
*& Find inconsistencies between Axxx and KONH-VAKEY (see also SD_CMDC0)*
*& and repair them                                                     *
*&---------------------------------------------------------------------*

REPORT z_corr_purcond.

TABLES: eina,
        eine,
        t024e,
        konh,
        rm06k.

PARAMETERS: p_lifnr LIKE eina-lifnr OBLIGATORY,
            p_ekorg LIKE eine-ekorg OBLIGATORY,
            p_datum LIKE vake-datab DEFAULT sy-datlo,
            p_simu  LIKE rm06k-simuk DEFAULT 'X'.

CONSTANTS: lc_report_name(20) TYPE c VALUE 'Z_CORR_PURCOND',
           lc_dat_0           TYPE d VALUE '00000000',
           lc_dat_f           TYPE d VALUE '99991231'.

* tricky hex2char conversion
DATA: BEGIN OF con_high.
DATA:   from  TYPE c,              " always initial like vakey
          to  TYPE x VALUE 'FF'.
DATA: END OF con_high.

DATA: BEGIN OF con_low.
DATA:   from  TYPE c,              " always initial like vakey
          to  TYPE x VALUE '00'.
DATA: END OF con_low.

DATA: gt_vake  LIKE vake OCCURS 0 WITH HEADER LINE,
      lt_t681  LIKE t681 OCCURS 0 WITH HEADER LINE,
      lt_konhu LIKE konh OCCURS 0 WITH HEADER LINE,
      lf_kalsm LIKE tmks-kalsm.




START-OF-SELECTION.

* get the relevant condition-data
  PERFORM cond_access USING p_ekorg
                            p_lifnr
                            p_datum.

  REFRESH: lt_konhu.
  LOOP AT gt_vake.
    SELECT SINGLE * FROM konh WHERE knumh EQ gt_vake-knumh.
    IF sy-subrc EQ 0.
* compare the VAKEY in the KONH with the VAKEY from GT_VAKE
      IF gt_vake-vakey NE konh-vakey.
        WRITE: / gt_vake-knumh,
                 gt_vake-vakey,
                 konh-vakey.
        MOVE konh TO lt_konhu.
        MOVE gt_vake-vakey TO lt_konhu-vakey.
        APPEND lt_konhu.
      ENDIF.
    ENDIF.

  ENDLOOP. " gt_vake

  IF p_simu EQ space.
    IF NOT lt_konhu IS INITIAL.
      UPDATE konh FROM TABLE lt_konhu.
      COMMIT WORK.
    ENDIF.
  ENDIF.


*---------------------------------------------------------------------*
*       FORM GET_SCHEME                                               *
*---------------------------------------------------------------------*
* Get the calculation scheme for the selected data                    *
*---------------------------------------------------------------------*
*  -->  IF_EKORG                                                      *
*  -->  IF_LIFNR                                                      *
*  <--  EF_KALSM                                                      *
*---------------------------------------------------------------------*
FORM get_scheme USING    value(if_ekorg) LIKE eine-ekorg
                         value(if_lifnr) LIKE eina-lifnr
                CHANGING       ef_kalsm  LIKE tmks-kalsm.

  DATA: ls_t024e LIKE t024e,
        ls_lfm1  LIKE lfm1.

  CLEAR: ef_kalsm.

* get all the data we need to determine the calculation scheme
  CALL FUNCTION 'T024E_SINGLE_READ'
       EXPORTING
            t024e_ekorg = if_ekorg
       IMPORTING
            wt024e      = ls_t024e
       EXCEPTIONS
            not_found   = 1
            OTHERS      = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    CALL FUNCTION 'VENDOR_MASTER_DATA_SELECT_00'
         EXPORTING
              i_lfa1_lifnr     = if_lifnr
              i_lfm1_ekorg     = if_ekorg
              i_data           = 'X'
         IMPORTING
              a_lfm1           = ls_lfm1
         EXCEPTIONS
              vendor_not_found = 1
              OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      CALL FUNCTION 'ME_GET_PRICING_SCHEME'
           EXPORTING
                groupe = ls_t024e-kalse
                groupk = ls_lfm1-kalsk
           IMPORTING
                scheme = ef_kalsm.
    ENDIF.
  ENDIF.

ENDFORM. " get_scheme



*---------------------------------------------------------------------*
*       FORM GET_CONDITIONS                                           *
*---------------------------------------------------------------------*
* Get all the relevant condition-tables                               *
*---------------------------------------------------------------------*
*  -->  IF_KALSM                                                      *
*  <--  ET_T681                                                       *
*---------------------------------------------------------------------*
FORM get_conditions TABLES et_t681         STRUCTURE t681
                    USING  value(if_kalsm) LIKE      tmks-kalsm.

  DATA: lt_t685  LIKE t685  OCCURS 0 WITH HEADER LINE,
        lt_t682i LIKE t682i OCCURS 0 WITH HEADER LINE,
        lt_t681  LIKE t681  OCCURS 0 WITH HEADER LINE,
        ls_t681  LIKE t681.

  REFRESH: lt_t685, lt_t681, et_t681.

  CALL FUNCTION 'ME_GET_CONDITIONS_TO_SEARCH'
       EXPORTING
            application        = 'M'
            pricing_scheme     = if_kalsm
       TABLES
            koart              = lt_t685
       EXCEPTIONS
            no_scheme          = 1
            no_success         = 2
            terminated_by_user = 3
            OTHERS             = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.

* we are only interested in pricing conditions
    DELETE lt_t685 WHERE kvewe NE 'A'.
    SORT lt_t685 BY kvewe kappl kozgf.
    DELETE ADJACENT DUPLICATES FROM lt_t685
                               COMPARING kvewe kappl kozgf.
    LOOP AT lt_t685.
      REFRESH: lt_t682i.
      CALL FUNCTION 'SD_T682I_SINGLE_READ'
           EXPORTING
                kvewe_i      = lt_t685-kvewe
                kappl_i      = lt_t685-kappl
                kozgf_i      = lt_t685-kozgf
           TABLES
                t682i_tab_io = lt_t682i.
      SORT lt_t682i BY kvewe kotabnr kappl.
      DELETE ADJACENT DUPLICATES FROM lt_t682i
                                 COMPARING kvewe kotabnr kappl.
      LOOP AT lt_t682i.

        CALL FUNCTION 'SD_T681_SINGLE_READ'
             EXPORTING
                  kvewe_i        = lt_t682i-kvewe
                  kotabnr_i      = lt_t682i-kotabnr
             IMPORTING
                  t681_o         = ls_t681
             EXCEPTIONS
                  no_entry_found = 1
                  OTHERS         = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          lt_t681 = ls_t681.
          APPEND lt_t681.
        ENDIF.
      ENDLOOP.                                              " lt_t682i
    ENDLOOP.                                                " lt_t685

    SORT lt_t681 BY kotab.
    DELETE ADJACENT DUPLICATES FROM lt_t681
                               COMPARING kotab.
    et_t681[] = lt_t681[].
  ENDIF.

ENDFORM. " get_conditions



*---------------------------------------------------------------------*
*       FORM COND_ACCESS                                              *
*---------------------------------------------------------------------*
* Determine all the affected condition-records in all the possible    *
* condition-tables and return their key-values in the global table    *
* GT_VAKE                                                             *
*---------------------------------------------------------------------*
*  -->  VALUE(IF_EKORG)                                               *
*  -->  VALUE(IF_LIFNR)                                               *
*  -->  VALUE(IF_DATE)                                                *
*---------------------------------------------------------------------*
FORM cond_access USING   value(if_ekorg) LIKE eine-ekorg
                         value(if_lifnr) LIKE eina-lifnr
                         value(if_date)  LIKE sy-datum.

  DATA: lt_t681           LIKE t681 OCCURS 0 WITH HEADER LINE,
        lf_kalsm          LIKE tmks-kalsm,
        lf_access_program LIKE trdir-name,
        lf_vake_low       LIKE vake,
        lf_vake_high      LIKE vake,
        lf_subrc          LIKE sy-subrc.

  PERFORM get_scheme USING    if_ekorg
                              if_lifnr
                     CHANGING lf_kalsm.

  PERFORM get_conditions TABLES lt_t681
                         USING  lf_kalsm.

  REFRESH: gt_vake.

  TRANSLATE lf_vake_low  USING con_low.
  TRANSLATE lf_vake_high USING con_high.
  lf_vake_low-datab = lc_dat_0.
  lf_vake_high-datab = if_date.
  lf_vake_low-datbi = if_date.
  lf_vake_high-datbi = lc_dat_f.
* for all possible condition-tables
  LOOP AT lt_t681.
    PERFORM set_access_program(sapmv130) USING lt_t681-kvewe
                                               lt_t681-kotabnr
                                               lf_access_program.
    lf_vake_low-kappl = lt_t681-kappl.
    lf_vake_high-kappl = lt_t681-kappl.
* determine the affected records and return them in GT_VAKE
    PERFORM access_using IN PROGRAM (lf_access_program)
                         USING 'RECEIVE_DATA'
                               lc_report_name
                               lf_vake_low
                               lf_vake_high
                               lf_subrc.
  ENDLOOP.                                                  " lt_t681

ENDFORM. " cond_access



*---------------------------------------------------------------------*
*       FORM RECEIVE_DATA                                             *
*---------------------------------------------------------------------*
* Write a data-record to the global table gt_vake                     *
* This routine is called from the routine ACCESS_USING in programs    *
* RV13Axyz                                                            *
*---------------------------------------------------------------------*
*  -->  IS_VAKE                                                       *
*  <--  EF_SUBRC                                                      *
*---------------------------------------------------------------------*
FORM receive_data USING value(is_vake) LIKE vake
                        ef_subrc       LIKE sy-subrc.
  ef_subrc = 4.
  gt_vake = is_vake.
  APPEND gt_vake.
ENDFORM. " receive_data
