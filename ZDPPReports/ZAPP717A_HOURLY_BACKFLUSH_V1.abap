************************************************************************
* Program Name      : ZAPP717A_HOURLY_BACKFLUSH
* Author            : Won-seob Kim
* Creation Date     : 2003.12.10.
* Specifications By :
* Pattern           : Report 1-1
* Development Request No : UD1K903764,UD1K904914
* Addl Documentation:
* Description       : Hourly BackFlush Reporting Point
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************

INCLUDE ZAPP717A_HOURLY_TOP_1.
*INCLUDE zapp717a_hourly_top.

INCLUDE ZAPP_717A_HOURLY_PARA_1.
*INCLUDE zapp_717a_hourly_para.

INITIALIZATION.

AT SELECTION-SCREEN.
  PERFORM screen_input_check.

START-OF-SELECTION.
  PERFORM get_bf_objects.

END-OF-SELECTION.
  PERFORM write_list.

*&---------------------------------------------------------------------*
*&      Form  get_bf_objects
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bf_objects.
*Get data : BackFlush status table ZTPP_BFST
  CLEAR : it_bfst[],it_bfst,w_int,it_resb[],it_resbt[].
  IF p_a EQ 'X'.
    SELECT * INTO TABLE it_bfst FROM ztpp_bfst
          WHERE plant EQ p_plant
*           AND plan_del_flg EQ space
            AND plan_ord IN s_plnum
            AND fin_bf_flg NE 'Y'
            AND bfp18_flg <> 'Y' ORDER BY model body_ser plan_ord.

  ELSEIF p_b EQ 'X'.
    SELECT * INTO TABLE it_bfst FROM ztpp_bfst
         WHERE plant EQ p_plant
*          AND plan_del_flg EQ space
           AND fin_bf_flg NE 'Y'
           AND bfp18_flg <> 'Y' ORDER BY model body_ser plan_ord.
  ENDIF.
  DESCRIBE TABLE it_bfst LINES w_int.
  IF w_int <> 0.
    PERFORM gathering_components TABLES it_bfst.
  ELSE.
    MESSAGE i005. "WITH text-002.
    EXIT.
  ENDIF.
ENDFORM.                    " get_bf_objects
*&---------------------------------------------------------------------*
*&      Form  write_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_list.
  LOOP AT it_bfst.
    WRITE : / it_bfst-plant,it_bfst-model,it_bfst-body_ser,
              it_bfst-plan_ord,
              it_bfst-bfp01_flg,it_bfst-bfp02_flg,it_bfst-bfp03_flg,
              it_bfst-bfp04_flg,it_bfst-bfp05_flg,it_bfst-bfp06_flg,
              it_bfst-bfp07_flg,it_bfst-bfp08_flg,it_bfst-bfp09_flg,
              it_bfst-bfp10_flg,it_bfst-bfp11_flg,it_bfst-bfp12_flg,
              it_bfst-bfp13_flg,it_bfst-bfp14_flg,it_bfst-bfp15_flg,
              it_bfst-bfp16_flg,it_bfst-bfp17_flg,it_bfst-bfp18_flg.
  ENDLOOP.
*Parallel processing : Trigger SD GOOD Issue program :
*by swkim 02.14.2004
  IF  NOT p_d IS INITIAL.
    PERFORM sd_trigger_event.
  ENDIF.
ENDFORM.                    " write_list
*&---------------------------------------------------------------------*
*&      Form  gathering_components
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BFST  text
*----------------------------------------------------------------------*
FORM gathering_components TABLES lp_bfst STRUCTURE it_bfst.
  CLEAR : it_plpo[],t_num,r_num.
  t_num = 1.
  LOOP AT lp_bfst.
*Accorting to valus of each field.
    PERFORM collect_bfst USING lp_bfst .
    MODIFY lp_bfst FROM lp_bfst.
*Update at Backflush status table
    MODIFY ztpp_bfst FROM lp_bfst.
    COMMIT WORK.
    CLEAR lp_bfst.
  ENDLOOP.

*Update at Vehicle master table : request by hur(01/27/2004)
  DATA : l_dtime(14).CLEAR l_dtime.
  LOOP AT lp_bfst WHERE bfp18_flg = 'Y'
                    AND fin_bf_flg = 'Y'.
    CONCATENATE  lp_bfst-bfp18_dat lp_bfst-bfp18_tim
                    INTO l_dtime.
    PERFORM update_vehicle_master USING 'P_FINAL_BF_DATE'
                                        lp_bfst-vin_num
                                        l_dtime.
    CLEAR : lp_bfst,l_dtime.
  ENDLOOP.

*Create log file
*  PERFORM create_interface_log TABLES lp_bfst.

ENDFORM.                    " gathering_components
*&---------------------------------------------------------------------*
*&      Form  input_di_backflush
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BFST  text
*----------------------------------------------------------------------*
FORM input_di_backflush TABLES lt_act STRUCTURE zppc_act_conf
                        USING ls_ppc_comp_conf TYPE
                                      zppc_comp_conf
                              ls_mode TYPE c
                              del_flag TYPE c
                        CHANGING l_plag.

  IF ls_mode = 'W'.        "Components and Activites
    CALL FUNCTION 'ZPPC1TP_COMP_CONF_DATA_WRITE'
         EXPORTING
              is_ppc_comp_conf  = ls_ppc_comp_conf
              if_skipsync       = ' '
              if_compsync       = ' '
         TABLES
*              it_conf_mats      = lt_mat
              it_conf_acts      = lt_act
         EXCEPTIONS
              order_error       = 1
              line_error        = 2
              bapi_error        = 3
              duplicate_posting = 4
              OTHERS            = 5.

    IF sy-subrc IS INITIAL.
      MESSAGE s008(ppc1pr).
      l_plag = 'Y'.
    ELSE.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      l_plag = 'EB'.

    ENDIF.
  ELSEIF ls_mode EQ 'R'.             "Reverse

    CALL FUNCTION 'ZPPC1TP_COMP_CONF_DATA_REVERSE'
         EXPORTING
              is_ppc_comp_conf = ls_ppc_comp_conf
              if_skipsync      = ' '
         TABLES
*              it_conf_mats     = lt_mat
              it_conf_acts     = lt_act
         EXCEPTIONS
              order_error      = 1
              line_error       = 2
              bapi_error       = 3
              run_ppcgo        = 4
              OTHERS           = 5.
    IF sy-subrc IS INITIAL.
      MESSAGE s008(ppc1pr).
      l_plag = '01'.
*In Case that Plan order is Deleted
      IF NOT del_flag IS INITIAL.
        l_plag = '00'.
      ENDIF.
    ELSE.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      l_plag = 'ER'.

    ENDIF.
  ENDIF.
ENDFORM.                    " input_di_backflush
*&---------------------------------------------------------------------*
*&      Form  collect_bfst
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LP_BFST  text
*----------------------------------------------------------------------*
FORM collect_bfst USING lpp_bfst STRUCTURE it_bfst.
*Check
  DATA : z_field(18),z_dat(18),z_tim(18).
  FIELD-SYMBOLS : <field> TYPE ANY,
                  <date> TYPE ANY,
                  <time> TYPE ANY.
  t_num = 1.
*First step
*Check backflush status : case of reversal
  IF lpp_bfst-bfp18_flg EQ '09'.
*DI Backflush
    CONCATENATE 'LPP_BFST' '-' 'BFP' '18' '_FLG' INTO z_field.
    ASSIGN (z_field) TO <field>.
*Conversion characteristic
    PERFORM conversion_characteristic USING '18'
                                       CHANGING p_char.

    PERFORM collect_di USING lpp_bfst p_char '18' 'R'
                         CHANGING p_flg.

    ASSIGN (z_field) TO <field>.
    MOVE p_flg TO <field>.

    CONCATENATE 'LPP_BFST' '-' 'BFP' '18' '_DAT' INTO z_dat.
    ASSIGN (z_dat) TO <date>.
    MOVE sy-datum TO  <date>.

    CONCATENATE 'LPP_BFST' '-' 'BFP' '18' '_TIM' INTO z_tim.
    ASSIGN (z_tim) TO <time>.
    MOVE sy-timlo  TO <time>.
  ENDIF.

*Second step
*CHECK : Each filed of status
  CLEAR num.
  num = '01'.
  DO.
    CONCATENATE 'LPP_BFST' '-' 'BFP' num '_FLG' INTO z_field.
    ASSIGN (z_field) TO <field>.
    IF num NE '18' AND <field> EQ '01'.
*Conversion characteristic
      PERFORM conversion_characteristic USING num
                                         CHANGING p_char.
*check status
      PERFORM check_components_status USING p_char lpp_bfst num.

      IF NOT ausp-atflv IS INITIAL.
*DI Backflush
        PERFORM collect_di USING lpp_bfst p_char num 'W'
                             CHANGING p_flg.
        ASSIGN (z_field) TO <field>.
        MOVE p_flg TO <field>.

        CONCATENATE 'LPP_BFST' '-' 'BFP' num '_DAT' INTO z_dat.
        ASSIGN (z_dat) TO <date>.
        MOVE sy-datum TO  <date>.

        CONCATENATE 'LPP_BFST' '-' 'BFP' num '_TIM' INTO z_tim.
        ASSIGN (z_tim) TO <time>.
        MOVE sy-timlo  TO <time>.
        num = num + 1.
      ELSE.
        EXIT.
      ENDIF.
    ELSEIF num EQ '18' AND <field> EQ '01'.
*Conversion characteristic
      PERFORM conversion_characteristic USING num
                                        CHANGING p_char.
*check status
      PERFORM check_components_status USING p_char lpp_bfst num.

      IF NOT ausp-atflv IS INITIAL.
*DI Backflush
        PERFORM collect_di USING lpp_bfst p_char num 'W'
                             CHANGING p_flg.
        MOVE p_flg TO <field>.
        IF p_flg EQ 'Y'.
          MOVE 'Y'   TO lpp_bfst-fin_bf_flg.
        ENDIF.

        CONCATENATE 'LPP_BFST' '-' 'BFP' num '_DAT' INTO z_dat.
        ASSIGN (z_dat) TO <date>.
        MOVE sy-datum TO  <date>.

        CONCATENATE 'LPP_BFST' '-' 'BFP' num '_TIM' INTO z_tim.
        ASSIGN (z_tim) TO <time>.
        MOVE sy-timlo  TO <time>.
        num = num + 1.
        EXIT.
      ELSE.
        EXIT.
      ENDIF.
    ELSEIF <field> EQ '09'.  " Reversal
*Conversion characteristic
      PERFORM conversion_characteristic USING num
                                        CHANGING p_char.
*DI Backflush
      PERFORM collect_di USING lpp_bfst p_char num 'R'
                           CHANGING p_flg.
      MOVE p_flg TO <field>.
      CONCATENATE 'LPP_BFST' '-' 'BFP' num '_DAT' INTO z_dat.
      ASSIGN (z_dat) TO <date>.
      MOVE sy-datum TO  <date>.

      CONCATENATE 'LPP_BFST' '-' 'BFP' num '_TIM' INTO z_tim.
      ASSIGN (z_tim) TO <time>.
      MOVE sy-timlo  TO <time>.
      num = num + 1.
    ELSEIF num EQ '19'.
      EXIT.
    ELSE.
      num = num + 1.
    ENDIF.
  ENDDO.
*DELETE CHECK
  IF NOT lpp_bfst-plan_del_flg IS INITIAL.
    CLEAR z_num.
    z_num = '01'.
    DO 18 TIMES.
      CONCATENATE 'LPP_BFST' '-' 'BFP' num '_FLG' INTO z_field.
      ASSIGN (z_field) TO <field>.
      IF  <field> EQ '00'.
        z_num = z_num + 1.
      ENDIF.
      IF z_num = 18.
        MOVE 'Y'   TO lpp_bfst-fin_bf_flg.
        CONCATENATE 'LPP_BFST' '-' 'BFP' z_num '_DAT' INTO z_dat.
        ASSIGN (z_dat) TO <date>.
        MOVE sy-datum TO  <date>.
        CONCATENATE 'LPP_BFST' '-' 'BFP' z_num '_TIM' INTO z_tim.
        ASSIGN (z_tim) TO <time>.
        MOVE sy-timlo  TO <time>.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    " collect_bfst
*&---------------------------------------------------------------------*
*&      Form  collect_resb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LPP_BFST  text
*      -->P_0198   text
*      -->P_0199   text
*      <--P_P_FLG  text
*----------------------------------------------------------------------*
FORM collect_di USING    pp_bfst  STRUCTURE it_bfst
                           pp_char  pp_num p_mode
                  CHANGING  p_flag.

*Collect components
  PERFORM collect_components_move USING pp_char pp_bfst
                                        pp_num p_mode
                             CHANGING lt_ppc_comp_conf
                                      g_sortb.
*Collect Activities & IPPE
  PERFORM collect_activites TABLES lt_act
                            USING pp_bfst pp_num g_sortb.

*Call input DI Backflush bapi function
  PERFORM input_di_backflush  TABLES lt_act
                              USING lt_ppc_comp_conf p_mode
                                    pp_bfst-plan_del_flg
                              CHANGING p_flag.

ENDFORM.                    " collect_resb
*&---------------------------------------------------------------------*
*&      Form  conversion_characteristic
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NUM  text
*----------------------------------------------------------------------*
FORM conversion_characteristic USING p_num
                               CHANGING p_char.

  CONCATENATE 'P_RP' p_num '_SHOP_DATE' INTO p_char.

ENDFORM.                    " conversion_characteristic
*&---------------------------------------------------------------------*
*&      Form  collect_components
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PP_CHAR  text
*      -->P_PP_BFST  text
*----------------------------------------------------------------------*
FORM check_components_status USING lp_char
                              lp_bfst STRUCTURE it_bfst
                              lp_num.
  CLEAR l_atinn.
*Read vehicle master status
  PERFORM get_vehicle_number USING lp_char
                                   lp_bfst-vin_num.

ENDFORM.                    " collect_components
*&---------------------------------------------------------------------*
*&      Form  collect_activites
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PP_NUM  text
*----------------------------------------------------------------------*
FORM collect_activites   TABLES ls_act STRUCTURE it_act
                         USING  lp_bfst STRUCTURE it_bfst
                              ls_num f_sortb.

*read routing : work center & value
  PERFORM routing_workcenter TABLES ls_act
                             USING lp_bfst
                                   ls_num f_sortb
                             CHANGING p_flg.

ENDFORM.                    " collect_activites
*&---------------------------------------------------------------------*
*&      Form  routing_workcenter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LP_BFST  text
*      -->P_LS_NUM  text
*      <--P_P_FLG  text
*----------------------------------------------------------------------*
FORM routing_workcenter TABLES ls_act STRUCTURE it_act
                       USING    lpp_bfst STRUCTURE it_bfst
                                 cc g_sortb
                        CHANGING pp_flg TYPE c.

  DATA : l_plnnr LIKE mapl-plnnr,
         l_plnal LIKE mapl-plnal.

  CLEAR : it_routing[],l_matnr,l_verid,l_dispo,l_plnnr,l_plnal.

  SELECT SINGLE  matnr verid dispo INTO (l_matnr,l_verid,l_dispo)
       FROM plaf
        WHERE plnum EQ lpp_bfst-plan_ord.

  SELECT SINGLE  plnng alnag INTO (l_plnnr,l_plnal)
       FROM mkal
        WHERE matnr EQ l_matnr
          AND verid EQ l_verid
          AND werks EQ p_plant.

  SELECT   ma~matnr  mr~dispo pk~plnnr
           pk~plnal  pp~vornr pp~lar02 pp~vgw02 pp~vge02
           pp~lar03  pp~vgw03 pp~vge03 ch~arbpl ch~sortb
    INTO CORRESPONDING FIELDS OF TABLE it_routing
      FROM ( ( ( ( ( ( mara AS ma
        INNER JOIN marc AS mr ON ma~matnr = mr~matnr )
        INNER JOIN mapl AS mp ON mr~matnr = mp~matnr )
        INNER JOIN plko AS pk ON mp~plnnr = pk~plnnr AND
                                 mp~plnal = pk~plnal )
        INNER JOIN plas AS pa ON mp~plnty = pa~plnty AND
                                 mp~plnnr = pa~plnnr AND
                                 mp~plnal = pa~plnal )
        INNER JOIN plpo AS pp ON pk~plnnr = pp~plnnr AND
                                 pa~plnty = pp~plnty AND
                                 pa~plnnr = pp~plnnr AND
                                 pa~plnkn = pp~plnkn AND
                                 pa~zaehl = pp~zaehl )
        INNER JOIN crhd AS ch ON pp~arbid = ch~objid )

      WHERE mp~plnty = 'R' AND
            mp~loekz = ' ' AND
            pa~loekz = ' ' AND
            ma~matnr EQ l_matnr  AND
            mr~werks EQ p_plant  AND
            mr~dispo EQ l_dispo  AND
            pk~plnnr EQ l_plnnr  AND
            pk~plnal EQ l_plnal  AND
            pa~plnfl EQ '000000' AND
            ch~objty EQ 'A' AND
            ch~sortb EQ g_sortb.

  SORT it_routing BY matnr arbpl sortb
                      ASCENDING.

  DELETE ADJACENT DUPLICATES FROM it_routing
           COMPARING matnr arbpl sortb vornr.

  CLEAR : w_int,ls_act[].
  DESCRIBE TABLE it_routing LINES w_int.
  IF w_int <> 0.
    CLEAR : it_routing.
*modify it_avt
    LOOP AT it_routing.
*HMMA_MAN
*Cost center
      CONCATENATE it_routing-arbpl '_MAN' INTO g_arbpl.
*IPPE
      PERFORM get_activities_ippe USING g_mname g_arbpl
                                  CHANGING mod_id
                                           re_id.

      MOVE it_routing-vgw03 TO ls_act-duration_var. "man value
      MOVE it_routing-vge03 TO ls_act-durunit. "man unit
      MOVE mod_id  TO ls_act-mode_guid.
      MOVE re_id   TO ls_act-resource_guid.
      APPEND ls_act. CLEAR ls_act.
*HMMA_MCH
*Cost center
      CONCATENATE it_routing-arbpl '_MCH' INTO g_arbpl.
*IPPE
      PERFORM get_activities_ippe USING g_cname g_arbpl
                                  CHANGING mod_id
                                           re_id.

      MOVE it_routing-vgw02 TO ls_act-duration_var. "man value
      MOVE it_routing-vge02 TO ls_act-durunit. "man unit
      MOVE mod_id  TO ls_act-mode_guid.
      MOVE re_id   TO ls_act-resource_guid.
      APPEND ls_act.CLEAR ls_act.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " routing_workcenter
*&---------------------------------------------------------------------*
*&      Form  get_activities_ippe
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_F_SORTB  text
*----------------------------------------------------------------------*
FORM get_activities_ippe USING  f_pname f_arbpl
                         CHANGING f_mode_id f_resource_id.
  CLEAR : f_mode_id,f_resource_id.

  CALL FUNCTION 'ZPPC1TP_GET_ACT_GUID_1'
    EXPORTING
      activity          = f_pname
      resource          = f_arbpl
   IMPORTING
*     ACTIVITY_ID       =
      mode_id           = f_mode_id
      resource_id       = f_resource_id.


ENDFORM.                    " get_activities_ippe
*&---------------------------------------------------------------------*
*&      Form  CREATE_INTERFACE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_interface_log TABLES lp_bfst.
  CLEAR w_int.
  DESCRIBE TABLE lp_bfst LINES w_int.
  CHECK w_int <> 0.
*  I_ZTCA_IF_LOG-TCODE    = 'ZPPI501'.
*  I_ZTCA_IF_LOG-ZSLNO    = WA_JOB-SLNO.
*  I_ZTCA_IF_LOG-JOBCOUNT = WA_JOB-INT.
  i_ztca_if_log-total    = w_int.
*  I_ZTCA_IF_LOG-ZSUCC    = Z_SUCC.
*  I_ZTCA_IF_LOG-ERROR    = Z_TOTAL - Z_SUCC.
  i_ztca_if_log-erdat    = sy-datum. "Created on.
  i_ztca_if_log-erzet    = sy-uzeit. "Created time.
  i_ztca_if_log-ernam    = sy-uname. "Created by.

  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      i_ztca_if_log              = i_ztca_if_log
*   IMPORTING
*     E_ZTCA_IF_LOG              =
   EXCEPTIONS
     update_failed              = 1
     number_range_error         = 2
     tcode_does_not_exist       = 3
     OTHERS                     = 4
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CREATE_INTERFACE_LOG
*&---------------------------------------------------------------------*
*&      Form  GET_VEHICLE_NUMBER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LP_BFST_PLAN_ORD  text
*----------------------------------------------------------------------*
FORM get_vehicle_number USING ls_char  ls_vin_num.
  CLEAR ausp.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = ls_char
       IMPORTING
            output = l_atinn.

*Compare with vin_mater status table
  SELECT SINGLE * FROM ausp
        WHERE objek  EQ ls_vin_num
          AND klart  EQ '002'
          AND atinn  EQ l_atinn.


ENDFORM.                    " GET_VEHICLE_NUMBER
*&---------------------------------------------------------------------*
*&      Form  collect_components_move
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PP_CHAR  text
*      -->P_PP_BFST  text
*      -->P_PP_NUM  text
*      <--P_LT_PPC_COMP_CONF  text
*      <--P_G_SORTB  text
*----------------------------------------------------------------------*
FORM collect_components_move USING lp_char
                              lp_bfst STRUCTURE it_bfst
                              lp_num lp_mode TYPE c
                        CHANGING lt_ppc_comp_conf
                                 STRUCTURE zppc_comp_conf
                              lp_usr01.

  DATA : ls_plnkn LIKE plpo-plnkn,
         ls_sortb(10),
         f_num(2) TYPE n,
         w_int2 TYPE i.
  CLEAR : l_steus,l_usr01,ls_plnkn,lt_ppc_comp_conf,ls_sortb.
*check ? node in sequence
*Reference Rate Routing
  ls_plnkn = lp_num.
  DESCRIBE TABLE it_plpo LINES w_int2.
  IF w_int2 = 0.
    SELECT plnkn INTO CORRESPONDING FIELDS OF TABLE it_plpo
            FROM plpo
             WHERE  plnty EQ 'M'
                AND plnnr EQ p_rp
                AND werks EQ p_plant
                AND nvadd NE 'X'.
  ENDIF.

  SORT it_plpo BY plnkn ASCENDING.
  CLEAR : it_plpo,f_num.
  f_num = 01.
  LOOP AT it_plpo.
    MOVE f_num TO it_plpo-num.
    MODIFY it_plpo FROM it_plpo.
    f_num = f_num + 1.
  ENDLOOP.

  READ TABLE it_plpo WITH  KEY num = ls_plnkn.

  SELECT  SINGLE ch~sortb INTO ls_sortb
     FROM plpo AS pp INNER JOIN crhd AS ch
                    ON pp~arbid = ch~objid
                     WHERE pp~plnty EQ 'M'
                       AND pp~plnnr EQ p_rp
                       AND pp~nvadd NE 'X'
                       AND pp~werks EQ p_plant
                       AND ch~objty EQ 'A'
                       AND pp~plnkn EQ it_plpo-plnkn.

  CLEAR : g_date, lt_ppc_comp_conf.
*conversion from fltp type to char
  PERFORM convsersion_flat USING ausp-atflv
                           CHANGING f_date.

  lt_ppc_comp_conf-plnum  = lp_bfst-plan_ord.
  lt_ppc_comp_conf-stsor  = ls_sortb.

  IF lp_mode  = 'W'.
    IF p_budat IS INITIAL.
*if value of posting in initial screen is not exist, use shop date
      lt_ppc_comp_conf-budat  = f_date.
      lt_ppc_comp_conf-bldat  = f_date.
    ELSE.
*if value of posting in initial screen is exist, use date
*of parameters
      READ TABLE p_budat INDEX 1.
      lt_ppc_comp_conf-budat  = p_budat-low.
      lt_ppc_comp_conf-bldat  = p_budat-low.
    ENDIF.
*In case of Reversal for reporting point : use system date instead of
*shop date
  ELSEIF lp_mode EQ 'R'.
    lt_ppc_comp_conf-budat  = sy-datum.
    lt_ppc_comp_conf-bldat  = sy-datum.
    lt_ppc_comp_conf-erfmg  = '1'.
    lt_ppc_comp_conf-erfme  = 'EA'.
  ENDIF.

  IF lp_num = '18'.
    lt_ppc_comp_conf-gr_ind = 'X'.
    lt_ppc_comp_conf-gi_ind = 'X'.
    lt_ppc_comp_conf-erfmg  = '1'.
    lt_ppc_comp_conf-erfme  = 'EA'.
  ELSE.
    lt_ppc_comp_conf-gr_ind = ' '.
    lt_ppc_comp_conf-gi_ind = 'X'.
  ENDIF.
  lp_usr01 = ls_sortb.

ENDFORM.                    " collect_components_move
*&---------------------------------------------------------------------*
*&      Form  convsersion_flat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_AUSP_ATWRT  text
*----------------------------------------------------------------------*
FORM convsersion_flat USING    p_atflv
                      CHANGING l_text2.

  DATA : l_text3(9).
  CLEAR : l_text,l_text1,l_text2,l_text3.
  MOVE p_atflv TO l_text.

  CALL FUNCTION 'CHAR_FLTP_CONVERSION'
       EXPORTING
            string = l_text
       IMPORTING
            flstr  = l_text1.

  MOVE l_text1(9) TO l_text3.
  REPLACE '.' WITH ' ' INTO l_text3.
  CONDENSE l_text3 NO-GAPS.
  MOVE l_text3 TO l_text2.

ENDFORM.                    " convsersion_flat
*&---------------------------------------------------------------------*
*&      Form  SCREEN_INPUT_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_input_check.
  CHECK NOT p_a IS INITIAL.
  IF s_plnum-low IS INITIAL.
    MESSAGE e001 WITH 'Plan order is a required field'.
  ENDIF.

ENDFORM.                    " SCREEN_INPUT_CHECK
*&---------------------------------------------------------------------*
*&      Form  UPDATE_vehicle_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0216   text
*      -->P_LP_BFST_VIN_NUM  text
*----------------------------------------------------------------------*
FORM update_vehicle_master USING    p_char
                                    p_vin_num
                                    p_dtime.

  CLEAR : val_table[],val_table.
  val_table-atnam = p_char.
  val_table-atwrt = p_dtime.
  APPEND val_table.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
          object             = p_vin_num
          mode               = 'W'
          ctype              = '002'
*           DISPLAY            = 'D'
       TABLES
          val_table          = val_table
*         EXCEPTIONS
*           NO_DATA            = 1
*           ERROR_MODE         = 2
*           ERROR_OBJECT       = 3
*           OTHERS             = 4
       .

ENDFORM.                    " UPDATE_vehicle_MASTER
*&---------------------------------------------------------------------*
*&      Form  sd_trigger_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sd_trigger_event.

  CALL FUNCTION 'BP_EVENT_RAISE'
           EXPORTING
                eventid                = 'ZESD02_01'
*                eventparm              = 'SD_DEL01'
           EXCEPTIONS
                bad_eventid            = 1
                eventid_does_not_exist = 2
                eventid_missing        = 3
                raise_failed           = 4
                OTHERS                 = 99.

  CASE sy-subrc.
    WHEN 0.
      MESSAGE s250 WITH 'ZESD02_01'.
    WHEN OTHERS.
      MESSAGE e042 WITH 'ZESD02_01'.
  ENDCASE.

ENDFORM.                    " sd_trigger_event
