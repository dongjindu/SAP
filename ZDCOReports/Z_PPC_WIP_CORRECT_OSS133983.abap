REPORT z_ppc_wip_correct_oss133983 LINE-SIZE 200.

* Usefull macros
DEFINE macro_change_unit.
  if ls_cpzp-meinh ne ls_c_act_var-durunit.
    "MESSAGE i001(00) WITH 'Uom was converted!'.
    call function 'UNIT_CONVERSION_SIMPLE'
         exporting
              input    = ls_c_act_var-delta_dur_var
              unit_in  = ls_c_act_var-durunit
              unit_out = ls_cpzp-meinh
         importing
              output   = ls_c_act_var-delta_dur_var
         exceptions
              others   = 10.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
             with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endif.
END-OF-DEFINITION.


DEFINE macro_write_screen.
  if &1 = 'OLD'.
    write: /1 ls_head-headid,
           35 ls_head-postdate,
           50 lf_objnr(14),
           65 lf_f_objnr(14),
           80 lf_gjper_post,
           90 ls_cpzp-gmper,
           110 ls_cpzp-istmn,
           130 ls_cpzp-varmn,
           150 ls_cpzp-meinh,
           155 ls_c_act_var-delta_dur_var,
               ls_c_act_var-durunit,
               ls_cpzp-gmsum.
  else.
    write: /90 ls_cpzp-gmper color 5,
           110 ls_cpzp-istmn color 5,
           130 ls_cpzp-varmn color 5.
  endif.

END-OF-DEFINITION.

TOP-OF-PAGE.
  SKIP.
  WRITE: /1 'HEADID',
         35 'POSTDATE',
         50 'OBJNR',
         65 'F_OBJNR',
         80 'gjper',
         95 'GMPER',
         120 'ISTMN',
         140 'VARMN',
         165 'delta'.
  ULINE.


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
    l_text2(70),
    count TYPE i.

  count = 0.

  PARAMETERS:
    p_date TYPE budat DEFAULT '20050430',
    p_test AS CHECKBOX DEFAULT 'X'.

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

  IF p_test IS INITIAL.
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
  ENDIF.


*--> Set lock to prevent ppc postings
  PERFORM conf_mat_enqueue(saplppc1pr).


  REFRESH: lt_cpzp, lt_c_act, lt_c_act_var, lt_head, lt_act.

  SELECT * INTO TABLE lt_ord FROM ppc_ord_inf
           WHERE dummy_order = 'X'.

  IF NOT lt_ord[] IS INITIAL.
    SELECT * INTO TABLE lt_head FROM ppc_head
             FOR ALL ENTRIES IN lt_ord
             WHERE orderid EQ lt_ord-orderid
               AND postdate EQ p_date
               AND flg_reversal EQ 'X'.
    SORT lt_head BY headid.

    IF NOT lt_head[] IS INITIAL.
      SELECT * INTO TABLE lt_c_act FROM ppc_conf_act
                                   FOR ALL ENTRIES IN lt_head
                                   WHERE headid EQ lt_head-headid.
      SELECT * INTO TABLE lt_c_act_var FROM ppc_conf_act_var
                                   FOR ALL ENTRIES IN lt_head
                                   WHERE headid EQ lt_head-headid
                                     AND delta_dur_var LT 0.
      IF NOT lt_c_act[] IS INITIAL.
        SELECT * INTO TABLE lt_act FROM ppc_act
                                     FOR ALL ENTRIES IN lt_c_act
                                     WHERE actid EQ lt_c_act-actid.
      ENDIF.
      IF NOT lt_c_act_var[] IS INITIAL.
        SELECT * APPENDING TABLE lt_act FROM ppc_act
                                     FOR ALL ENTRIES IN lt_c_act_var
                                     WHERE actid EQ lt_c_act_var-actid.
      ENDIF.
      SORT lt_act BY actid.

    ENDIF.
  ENDIF.


* Take each wrong variance
  LOOP AT lt_c_act_var INTO ls_c_act_var.

    SKIP 2.

    l_dur_var_temp = abs( ls_c_act_var-delta_dur_var ).

    READ TABLE lt_head INTO ls_head
        WITH KEY headid = ls_c_act_var-headid
        BINARY SEARCH.
    IF sy-subrc NE 0.
      MESSAGE e001(00) WITH 'Unexpected error'.
    ENDIF.
    READ TABLE lt_act INTO ls_act
        WITH KEY actid = ls_c_act_var-actid
        BINARY SEARCH.
    IF sy-subrc NE 0.
      MESSAGE e001(00) WITH 'Unexpected error'.
    ENDIF.
    PERFORM get_co_fields USING ls_head-accassobj
                                ls_head-postdate
                                ls_act-resource_guid
                          CHANGING
                            lf_aufnr
                            lf_objnr
                            lf_pkosa_error
                            lf_pkosa_plant
                            lf_gjper_post
                            lf_gjper_curr
                            lf_gjper_prev
                            lf_kokrs
                            lf_f_objnr.

*   select the CPZP entry
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
      MESSAGE e001(00) WITH 'Unexpected error'.
    ENDIF.


*   Step 1. Adjust the posting period: GMPER, ISTMN, VARMN
    macro_write_screen 'OLD'.
    macro_change_unit.
    ls_cpzp-gmper  = ls_cpzp-gmper + ( 2 * l_dur_var_temp ).
    ls_cpzp-istmn  = ls_cpzp-istmn + ( 2 * l_dur_var_temp ).
    ls_cpzp-varmn  = ls_cpzp-varmn - ( 2 * l_dur_var_temp ).
    macro_write_screen 'NEW'.

    IF p_test IS INITIAL.
      UPDATE cpzp FROM ls_cpzp.
    ENDIF.

*   Step2. Check the subsequant periods
    DO.
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
*       exit do loop! => no posting in this period
        EXIT.
      ENDIF.
*     do again the CPZP update; this time only ISTMN
      macro_write_screen 'OLD'.
      macro_change_unit.
      ls_cpzp-istmn  = ls_cpzp-istmn + ( 2 * l_dur_var_temp ).
      macro_write_screen 'NEW'.
      IF p_test IS INITIAL.
        UPDATE cpzp FROM ls_cpzp.
      ENDIF.
    ENDDO.


*   Step 3. Update the CONF_ACT_VAR entry (change sign)
    IF p_test IS INITIAL.
      UPDATE ppc_conf_act_var
          SET   delta_dur_var = l_dur_var_temp
          WHERE headid = ls_c_act_var-headid
            AND actid  = ls_c_act_var-actid.
    ENDIF.


    IF p_test IS INITIAL.
      COMMIT WORK.
    ENDIF.

  ENDLOOP. " PPC_CONF_ACT_VAR



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
