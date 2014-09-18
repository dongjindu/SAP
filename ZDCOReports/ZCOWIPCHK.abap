REPORT z_ppc_wip_correct_oss133983 LINE-SIZE 255.
TABLES: ppc_head, ppc_conf_act_var,ppc_ord_inf,qrp002,
        ppc_act.
DATA:
lt_c_act LIKE ppc_conf_act OCCURS 0,
lt_c_act_var LIKE ppc_conf_act_var OCCURS 0,

lt_ord LIKE ppc_ord_inf OCCURS 0,
lt_act LIKE ppc_act OCCURS 0,
ls_act LIKE ppc_act,
ls_ord LIKE ppc_ord_inf,
lt_head LIKE ppc_head OCCURS 0,
ls_head LIKE ppc_head,
lt_qrp002 LIKE qrp002 OCCURS 0,
ls_qrp002 LIKE qrp002,
ls_c_act LIKE ppc_conf_act,
ls_c_act_var LIKE ppc_conf_act_var,

*
l_dur_var_temp TYPE ppc_duration_var,
lf_aufnr TYPE aufnr,
lf_pkosa_plant TYPE werks_d,
lf_objnr TYPE j_objnr,
lf_f_objnr TYPE f_objnr,
lf_pkosa_error TYPE c,
lf_kokrs TYPE kokrs,
lf_gjper_post TYPE co_gjper,
lf_gjper_curr TYPE co_gjper,
lf_gjper_prev TYPE co_gjper,
lf_zaehl TYPE count_zp,
ls_cpzp LIKE cpzp,
ls_cpzp_3 LIKE cpzp,
lt_cpzp LIKE cpzp OCCURS 0,
l_answer(1),
l_text1(70),
l_text2(70).

DATA:count TYPE i.
count = 0.

SELECT-OPTIONS: so_h_id FOR ppc_head-headid.
PARAMETERS: p_test(1) DEFAULT 'X'.

START-OF-SELECTION.
* This report makes really changes in the system =>
* Check that the current user has debug authorization.
  AUTHORITY-CHECK OBJECT 'S_DEVELOP'
    ID 'DEVCLASS' DUMMY
    ID 'OBJTYPE'  FIELD 'DEBUG'
    ID 'OBJNAME'  DUMMY
    ID 'P_GROUP'  DUMMY
    ID 'ACTVT'    FIELD '03'.
  IF  sy-subrc <> 0.
    MESSAGE e895(m7) WITH 'Sorry, no authorization'.
  ENDIF.

  l_text1  =
  'Please make sure that the report will not be started twice'.
  l_text2  = 'Do you want to continue?'.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            titel     = 'Do not start the report twice'
            textline1 = l_text1
            textline2 = l_text2
       IMPORTING
            answer    = l_answer.
  IF l_answer = 'A' OR
     l_answer = 'N'.
    MESSAGE e895(m7) WITH 'Ok, abort the program'.
  ENDIF.


*--> Set lock to prevent ppc postings
  PERFORM conf_mat_enqueue(saplppc1pr).
*  PERFORM ENQUEUE_PPCGO(SAPLPPC1PR).


  REFRESH: lt_cpzp, lt_c_act, lt_c_act_var, lt_head, lt_act.

*Therefore, we may loop at PPC_HEAD and find the confirmations posted
*with respect to dummy orders, get the corresponding activities from
*PPC_CONF_ACT (PPC_CONF_ACT_VAR), and for each of them:
*  -if the confirmation is "normal", then get the value of field
*  DELTA_DUR_VAR and add it to the ISTMN and GMPER of the corresponding
*  CPZP lines in all subsequent periods, (i.e, if the posting was in
*  March, we have to add for the CPZP lines of March and April and May
*  if open)
*
*  -if the confirmation is reversal, then decrease the double of
*   DELTA_DUR_VAR from VARMN of the corresponding CPZP line in the
*   posting period only (if the posting was in March, we have to add
*   only for the CPZP line of March) and finally, we have to update the
*   value of field DURATION_VAR of the corresponding line of
*   PPC_CONF_ACT.

  SELECT * INTO TABLE lt_ord FROM ppc_ord_inf
           WHERE dummy_order = 'X'.

  IF NOT lt_ord[] IS INITIAL.
    SELECT * INTO TABLE lt_head FROM ppc_head
             FOR ALL ENTRIES IN lt_ord
             WHERE orderid = lt_ord-orderid.
    SORT lt_head BY postdate.

    SELECT * INTO TABLE lt_c_act FROM ppc_conf_act
                                FOR ALL ENTRIES IN lt_head
                                 WHERE headid EQ lt_head-headid AND
                                 duration_var EQ 0.

    SELECT * INTO TABLE lt_c_act_var FROM ppc_conf_act_var
                                FOR ALL ENTRIES IN lt_head
                                 WHERE headid EQ lt_head-headid.

* read the ressource GUID
    SELECT * FROM ppc_act INTO TABLE lt_act
    FOR ALL ENTRIES IN lt_c_act
    WHERE actid = lt_c_act-actid.
  ENDIF.

  LOOP AT lt_head INTO ls_head.

    LOOP AT lt_c_act_var INTO ls_c_act_var
                         WHERE headid = ls_head-headid.
      SKIP.
      CHECK NOT ls_c_act_var-delta_dur_var IS INITIAL OR
            NOT ls_c_act_var-delta_dur_fix IS INITIAL.

      READ TABLE lt_act INTO ls_act
                        WITH KEY actid = ls_c_act_var-actid.
      IF sy-subrc NE 0.
        BREAK-POINT.
        MESSAGE e001(00) WITH 'Missing ACTID' ls_c_act-actid.
      ENDIF.
      CHECK sy-subrc EQ 0.

      PERFORM get_co_fields USING ls_head-accassobj
                                  ls_head-postdate
                                  ls_act-resource_guid
                            CHANGING
                            lf_aufnr       lf_objnr      lf_pkosa_error
                             lf_pkosa_plant lf_gjper_post lf_gjper_curr
                                lf_gjper_prev  lf_kokrs      lf_f_objnr.

*     select the CPZP entry
      IF p_test IS INITIAL.
        SELECT SINGLE FOR UPDATE * FROM cpzp INTO ls_cpzp
                                         WHERE objnr EQ lf_objnr
                                           AND f_objnr  EQ lf_f_objnr
                                           AND gjper EQ lf_gjper_post
                                           AND zaehl EQ lf_zaehl.
      ELSE.
        SELECT SINGLE * FROM cpzp INTO ls_cpzp
                                         WHERE objnr EQ lf_objnr
                                           AND f_objnr  EQ lf_f_objnr
                                           AND gjper EQ lf_gjper_post
                                           AND zaehl EQ lf_zaehl.
      ENDIF.
      IF sy-subrc NE 0.
*       ??? what to do in this case ???
        BREAK-POINT.
        MESSAGE e001(00) WITH 'Unexpected error'.
      ENDIF.

      WRITE: /1 'HEADID',  ls_head-headid,
                'OBJNR',   lf_objnr,
                'F_OBJNR', lf_f_objnr,
                'gjper',   lf_gjper_post,
                'ZAEHL',   lf_zaehl,
                'GMPER',   ls_cpzp-gmper,
                'ISTMN',   ls_cpzp-istmn,
                'VARMN',   ls_cpzp-varmn,
                'delta',   ls_c_act_var-delta_dur_var.

      IF ls_head-flg_reversal IS INITIAL.
*     -if the confirmation is "normal", then get the value of field
*   DELTA_DUR_VAR and add it to the ISTMN and GMPER of the corresponding
*   CPZP lines in all subsequent periods, (i.e, if the posting was in
*   March, we have to add for the CPZP lines of March and April and May
*   if open)
        DO.
          IF ls_cpzp-meinh NE ls_c_act_var-durunit.
            MESSAGE i001(00) WITH 'Uom was converted!'.
            CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
                 EXPORTING
                      input                = ls_c_act_var-delta_dur_var
                      unit_in              = ls_c_act_var-durunit
                      unit_out             = ls_cpzp-meinh
                 IMPORTING
                      output               = ls_c_act_var-delta_dur_var
                 EXCEPTIONS
                      conversion_not_found = 1
                      division_by_zero     = 2
                      input_invalid        = 3
                      output_invalid       = 4
                      overflow             = 5
                      type_invalid         = 6
                      units_missing        = 7
                      unit_in_not_found    = 8
                      unit_out_not_found   = 9
                      OTHERS               = 10.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
          ENDIF.
*         ??? SHALL WE DO ABS VALUE ???
          ls_cpzp-gmper  = ls_cpzp-gmper +
                             ABS( ls_c_act_var-delta_dur_var ).
          ls_cpzp-istmn  = ls_cpzp-istmn +
                             ABS( ls_c_act_var-delta_dur_var ).
          IF p_test IS INITIAL.
            UPDATE cpzp FROM ls_cpzp.
          ENDIF.

*         check the next period whether we have already posting there
          lf_gjper_post = lf_gjper_post + 1.
          IF p_test IS INITIAL.
            SELECT SINGLE FOR UPDATE * FROM cpzp INTO ls_cpzp
                                             WHERE objnr EQ lf_objnr
                                             AND f_objnr  EQ lf_f_objnr
                                             AND gjper EQ lf_gjper_post
                                               AND zaehl EQ lf_zaehl.
          ELSE.
            SELECT SINGLE * FROM cpzp INTO ls_cpzp
                                             WHERE objnr EQ lf_objnr
                                             AND f_objnr  EQ lf_f_objnr
                                             AND gjper EQ lf_gjper_post
                                               AND zaehl EQ lf_zaehl.
          ENDIF.
          IF sy-subrc NE 0.
*           exit do loop! => no posting in the period
            EXIT.
          ENDIF.
*         do again the CPZP update in the next period
          WRITE: /2 'HEADID',  ls_head-headid,
                    'OBJNR',   lf_objnr,
                    'F_OBJNR', lf_f_objnr,
                    'gjper',   lf_gjper_post,
                    'ZAEHL',   lf_zaehl,
                    'GMPER',   ls_cpzp-gmper,
                    'ISTMN',   ls_cpzp-istmn,
                    'VARMN',   ls_cpzp-varmn,
                    'delta',   ls_c_act_var-delta_dur_var.
        ENDDO.

      ELSE.
*     FLG_REVERSAL = 'X'.
*  -if the confirmation is reversal, then decrease the double of
*   DELTA_DUR_VAR from VARMN of the corresponding CPZP line in the
*   posting period only (if the posting was in March, we have to add
*   only for the CPZP line of March) and finally, we have to update the
*   value of field DURATION_VAR of the corresponding line of
*   PPC_CONF_ACT.
*       ??? shall we take ABS value ???
        l_dur_var_temp = ls_c_act_var-delta_dur_var.
        IF ls_cpzp-meinh NE ls_c_act_var-durunit.
          MESSAGE i001(00) WITH 'Uom was converted!'.
          CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
               EXPORTING
                    input                = ls_c_act_var-delta_dur_var
                    unit_in              = ls_c_act_var-durunit
                    unit_out             = ls_cpzp-meinh
               IMPORTING
                    output               = ls_c_act_var-delta_dur_var
               EXCEPTIONS
                    conversion_not_found = 1
                    division_by_zero     = 2
                    input_invalid        = 3
                    output_invalid       = 4
                    overflow             = 5
                    type_invalid         = 6
                    units_missing        = 7
                    unit_in_not_found    = 8
                    unit_out_not_found   = 9
                    OTHERS               = 10.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.

*       modify the CPZP-VARMN
        ls_cpzp-varmn = ls_cpzp-varmn -
                         ( ABS( ls_c_act_var-delta_dur_var ) * 2 ).
        IF p_test IS INITIAL.
          UPDATE cpzp FROM ls_cpzp.
        ENDIF.

        CLEAR ls_c_act.
        READ TABLE lt_c_act INTO ls_c_act
                            WITH KEY headid = ls_head-headid
                                     actid  = ls_c_act_var-actid.
        IF sy-subrc NE 0.
*         fill key values from conf_act_var => INSERT
          MOVE-CORRESPONDING ls_c_act_var TO ls_c_act.
          ls_c_act-duration_var = l_dur_var_temp.
        ELSE.
*         if it is there => UPDATE
          ls_c_act-duration_var = ls_c_act-duration_var +
                                               l_dur_var_temp.
        ENDIF.
        IF p_test IS INITIAL.
          MODIFY ppc_conf_act FROM ls_c_act.
        ENDIF.

      ENDIF. " FLG_REVERSAL = 'X'.

      IF p_test IS INITIAL.
        COMMIT WORK.
      ENDIF.
    ENDLOOP. " PPC_CONF_ACT_VAR
  ENDLOOP. " PPC_HEAD

*--> unlock the ppc postings
  PERFORM conf_mat_dequeue(saplppc1pr).
*  PERFORM DEQUEUE_PPCGO(SAPLPPC1PR).

*&---------------------------------------------------------------------*
*&      Form  get_co_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_HEAD_ACCASSOBJ  text
*      -->P_LS_HEAD_POSTDATE  text
*      -->P_LS_ACT_RESOURCE_GUID  text
*      <--P_LF_AUFNR  text
*----------------------------------------------------------------------*
FORM get_co_fields USING    i_accassobj TYPE ppc_accassobj_int
                            i_postdate TYPE budat
                            i_act_resource_guid TYPE ppc_resguid_int
                   CHANGING ef_aufnr TYPE aufnr
                            ef_objnr TYPE j_objnr
                            ef_pkosa_error TYPE c
                            ef_pkosa_plant TYPE werks_d
                            ef_gjper_post TYPE co_gjper
                            ef_gjper_curr TYPE co_gjper
                            ef_gjper_prev TYPE co_gjper
                            ef_kokrs TYPE kokrs
                            ef_f_objnr TYPE f_objnr.

  CLEAR: ef_aufnr,ef_objnr,ef_pkosa_error,
         ef_pkosa_plant,ef_gjper_post,ef_gjper_curr,ef_gjper_prev,
         ef_kokrs,ef_f_objnr.

*   get the OBJNR
  PERFORM objnr_get(saplqrprp) USING    i_accassobj
                    CHANGING ef_aufnr
                             ef_objnr
                             ef_pkosa_error.
  IF NOT ef_pkosa_error IS INITIAL.
    MESSAGE e001(qrprp) WITH 'QRP_APO_REPORTINGPOINT_POST'
                      ls_head-accassobj space space.

  ENDIF.

*   Werk zum Produktkostensammler lesen und Buchungsperiode bestimmen
*   read plant to pcc and determine posting period
  PERFORM pkosa_plant_get(saplqrprp) USING ef_aufnr
                                     CHANGING ef_pkosa_plant
                                              ef_kokrs.
  PERFORM periods_get(saplqrprp) USING    ef_pkosa_plant
                                          i_postdate
                                 CHANGING ef_gjper_post
                                          ef_gjper_curr
                                          ef_gjper_prev.
  CALL FUNCTION 'KCR01_GET_COST_RESOURCE'
       EXPORTING
            i_resource_guid    = i_act_resource_guid
            i_controlling_area = ef_kokrs
            i_key_date         = i_postdate
       IMPORTING
            e_object_number    = ef_f_objnr
       EXCEPTIONS
            OTHERS             = 1.



ENDFORM.                    " get_co_fields
