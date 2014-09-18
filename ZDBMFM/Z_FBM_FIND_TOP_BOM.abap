FUNCTION z_fbm_find_top_bom.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(PA_MATNR) LIKE  MARA-MATNR
*"     REFERENCE(PA_DATUM) LIKE  SY-DATUM OPTIONAL
*"     REFERENCE(PA_WERKS) LIKE  MARC-WERKS DEFAULT 'P001'
*"     REFERENCE(PA_STLAN) LIKE  STZU-STLAN DEFAULT '1'
*"     REFERENCE(PA_MEHRS) LIKE  CSDATA-XFELD DEFAULT 'X'
*"  TABLES
*"      T_UPPER STRUCTURE  STPOV
*"      T_TOP STRUCTURE  STPOV
*"  EXCEPTIONS
*"      MAT_NOT_FOUND
*"      BOM_NOT_FOUND
*"      INVALID_PERIOD
*"      NO_SELECTION
*"----------------------------------------------------------------------

  PM_STLAN = PA_STLAN .
  PM_WERKS = PA_WERKS .
  PM_IDNRK = PA_MATNR .
  PM_MEHRS = PA_MEHRS .

  cal_aclas = csbomex-aclas.                                "HGD072824
  tcs03-agb29 = '29'.
  READ TABLE tcs03.

  PERFORM tcspr_lesen.

  PERFORM tcc08_lesen.                                      "YHG087082

  PERFORM set_schalter.                                   "YHG077295
  IF PA_DATUM IS INITIAL.
     pm_datuv = syst-datum.
  ELSE.
     pm_datuv = PA_DATUM.
  ENDIF.


* ---------------------------------
**AT SELECTION-SCREEN.                                        "YHG078090
* ---------------------------------
  PERFORM prep_chk_types.                                   "YHG110068
  PERFORM prep_multilv.                                     "YHG125492

  CHECK NOT pm_idnrk IS INITIAL.

  PERFORM get_revl_sdatu.                                   "YHG083168

  PERFORM get_wu_recs                                       "YHG125492
     USING pm_idnrk                                         "YHG125492
           pm_werks                                         "YHG125492
           pm_stlan.                                        "YHG125492

  CASE sy-subrc.
    WHEN 2.
      raise MAT_NOT_FOUND.
    WHEN 3.
      raise BOM_NOT_FOUND.
    WHEN 4.
      raise NO_SELECTION .
    WHEN 5.
      raise INVALID_PERIOD.
  ENDCASE.

  READ TABLE ltb INDEX 1.
  IF sy-subrc = 0 .
    IF NOT pm_mehrs IS INITIAL.
      sav_selpool = selpool.

      PERFORM exp_wutab
         USING pm_idnrk
               pm_werks
               pm_stlan.

      PERFORM reg_wures
         USING pm_idnrk
               pm_werks
               pm_stlan
               'x' 'x' ' '.

      LOOP AT wu_ctab.
        wu_ctab_loopx = sy-tabix.
        CHECK NOT wu_ctab-isusd IS INITIAL.
        CHECK wu_ctab-wutck IS INITIAL.

        wu_memid-tabid = 'LT'.
        wu_memid-matnr = wu_ctab-matnr.
        wu_memid-werks = wu_ctab-werks.
        wu_memid-stlan = pm_stlan.                          "HGA114476

        CLEAR: lltb. REFRESH: lltb.
        IMPORT ltb TO lltb
           FROM MEMORY ID wu_memid.

        wu_memid-tabid = 'MT'.
        CLEAR: lmatcat. REFRESH: lmatcat.
        IMPORT matcat TO lmatcat
           FROM MEMORY ID wu_memid.

        LOOP AT lltb                                        "HGC154389
           WHERE     bmtyp EQ typ_mat                       "HGC154389
                 AND rekri IS initial                       "HGC154389
                 AND rekrs IS initial           "HGC154389 MBA148624
                 AND kzkup IS initial                       "MBC167558
                 AND excpt NE 'CONV'.                       "MBA148624

          READ TABLE lmatcat INDEX lltb-ttidx.

          wu_ctab_key-matnr = lmatcat-matnr.
          wu_ctab_key-werks = lltb-werks.
          wu_ctab_key-stlan = pm_stlan.                     "HGA114476
          READ TABLE wu_ctab WITH KEY wu_ctab_key.
          CHECK sy-subrc <> 0.

          PERFORM get_wu_recs
             USING lmatcat-matnr
                   lltb-werks
                   pm_stlan.                                "HGA114476

          IF sy-subrc = 0.
            PERFORM exp_wutab
               USING lmatcat-matnr
                     lltb-werks
                     pm_stlan.                              "HGA114476

            PERFORM reg_wures
               USING lmatcat-matnr
                     lltb-werks
                     pm_stlan                               "HGA114476
                     'x' 'x' ' '.
          ELSE.
            T_TOP = LLTB .   APPEND T_TOP.
            PERFORM reg_wures
               USING lmatcat-matnr
                     lltb-werks
                     pm_stlan                               "HGA114476
                     'x' ' ' ' '.
          ENDIF.
        ENDLOOP.

        READ TABLE wu_ctab INDEX wu_ctab_loopx.
        wu_ctab-wutck = 'x'.
        MODIFY wu_ctab.
      ENDLOOP.

      selpool = sav_selpool.
      CLEAR: ltb. REFRESH: ltb.
      CLEAR: matcat. REFRESH: matcat.

      READ TABLE wu_ctab INDEX 1.
      wu_memid-tabid = 'LT'.
      wu_memid-matnr = wu_ctab-matnr.
      wu_memid-werks = wu_ctab-werks.
      wu_memid-stlan = wu_ctab-stlan.

      CLEAR: lltb. REFRESH: lltb.
      IMPORT ltb TO lltb
         FROM MEMORY ID wu_memid.

      wu_memid-tabid = 'MT'.
      CLEAR: lmatcat. REFRESH: lmatcat.
      IMPORT matcat TO lmatcat
         FROM MEMORY ID wu_memid.

      act_level = 1 .
      wu_ml_stack-stufe = act_level.
      wu_ml_stack-loopx = 1 .
      wu_ml_stack-matnr = wu_ctab-matnr.
      wu_ml_stack-werks = wu_ctab-werks.
      wu_ml_stack-stlan = wu_ctab-stlan.
      APPEND wu_ml_stack.
      ltb_strtx = 1 .

      WHILE end_flag IS INITIAL.
        LOOP AT lltb FROM ltb_strtx                         "HGC154389
           WHERE bmtyp EQ typ_mat.                          "HGC154389

          ltb_loopx = sy-tabix.

          IF     pop_flag IS INITIAL
             OR  lltb-sumfg EQ 'x'.

            READ TABLE lmatcat INDEX lltb-ttidx.
            matcat = lmatcat. APPEND matcat.

            lltb-ttidx = sy-tabix.
            lltb-level = act_level.
            ltb = lltb.
            APPEND ltb.

            CHECK lltb-sumfg NE 'x'.
            CHECK lltb-rekri IS INITIAL
               AND lltb-excpt NE 'CONV'                     "MBB167558
               AND lltb-kzkup IS INITIAL              "note 593874
               AND lltb-rekrs IS INITIAL.

            wu_ctab_key-matnr = lmatcat-matnr.
            wu_ctab_key-werks = lltb-werks.
            wu_ctab_key-stlan = pm_stlan.                   "HGA114476
            READ TABLE wu_ctab WITH KEY wu_ctab_key.

            IF NOT wu_ctab-isusd IS INITIAL.
              wu_ml_stack-stufe = act_level + 1 .
              wu_ml_stack-loopx = 1 .
              wu_ml_stack-matnr = lmatcat-matnr.
              wu_ml_stack-werks = lltb-werks.
              wu_ml_stack-stlan = pm_stlan.                 "HGA114476
              APPEND wu_ml_stack.
              stack_prevx = sy-tabix - 1 .
              pop_flag = 'x'.
            ENDIF.
          ELSE.
            READ TABLE wu_ml_stack
               WITH KEY act_level.
            wu_ml_stack-loopx = ltb_loopx.
            MODIFY wu_ml_stack INDEX sy-tabix.
            stack_flag = 'x'.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF NOT pop_flag IS INITIAL.
          CLEAR: pop_flag.

          IF stack_flag IS INITIAL.
            READ TABLE wu_ml_stack INDEX stack_prevx.
            DESCRIBE TABLE lltb LINES sy-tabix.
            wu_ml_stack-loopx = sy-tabix + 1 .
            MODIFY wu_ml_stack INDEX stack_prevx.
          ELSE.
            CLEAR: stack_flag.
          ENDIF.

          act_level = act_level + 1 .
          READ TABLE wu_ml_stack
             WITH KEY act_level.
        ELSE.
          IF act_level = 1 .
            EXIT.
            end_flag = 'x'.
          ENDIF.

          DESCRIBE TABLE wu_ml_stack LINES sy-tabix.
          DELETE wu_ml_stack INDEX sy-tabix.
          sy-tabix = sy-tabix - 1.
          READ TABLE wu_ml_stack INDEX sy-tabix.
        ENDIF.

        wu_memid-tabid = 'LT'.
        wu_memid-matnr = wu_ml_stack-matnr.
        wu_memid-werks = wu_ml_stack-werks.
        wu_memid-stlan = wu_ml_stack-stlan.

        CLEAR: lltb. REFRESH: lltb.
        IMPORT ltb TO lltb
          FROM MEMORY ID wu_memid.

        wu_memid-tabid = 'MT'.
        CLEAR: lmatcat. REFRESH: lmatcat.
        IMPORT matcat TO lmatcat
           FROM MEMORY ID wu_memid.

        act_level = wu_ml_stack-stufe.
        ltb_strtx = wu_ml_stack-loopx.
      ENDWHILE.
    ENDIF.                                                  "YHG125492

    IF NOT tcc08-ccrvl IS INITIAL.                          "YHG087082
      IF NOT selpool-kzrev IS INITIAL.                      "YHG083168
        PERFORM get_revlv.                                  "YHG083168
      ENDIF.                                                "YHG083168
    ENDIF.                                                  "YHG087082
  ELSE.
    MESSAGE s507(29) WITH 'E: ' pm_idnrk.
  ENDIF.

  T_UPPER[] = LTB[].
ENDFUNCTION.
