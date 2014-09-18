*&--------------------------------------------------------------------
*& REPORT                 : ZACO92A_CREAT_ROUTING
*& Author                 : HS Jung
*& Creation Date          : 09/27/2006
*& Specification By       : Andy Choi
*& Development Request No :
*& Addl documentation     :
*& Description            : Creating Routing
*& Modification Log
*& Date     Developer      Request ID      Description
*&
*&--------------------------------------------------------------------

REPORT zaco92a_creat_routing MESSAGE-ID zmco
        NO STANDARD PAGE HEADING LINE-SIZE 118
                                 LINE-COUNT 87.

* For TOP include
INCLUDE zaco92a_creat_routing_top .
INCLUDE zco_alv_top.
INCLUDE zco_alv_form .

TABLES: tc31a.
*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
* Planing Year
PARAMETERS : p_kokrs LIKE keko-kokrs MEMORY ID cac OBLIGATORY,
             p_bdatj LIKE keko-bdatj MEMORY ID bdtj OBLIGATORY,
             p_versn LIKE cosl-versn OBLIGATORY DEFAULT '311',
             p_plscn LIKE plaf-plscn DEFAULT '901' OBLIGATORY,
             p_new(2) TYPE c DEFAULT 'EL'.
SELECT-OPTIONS :
             p_eff   FOR tc31a-zgkal NO INTERVALS OBLIGATORY ,
             s_matnr FOR mara-matnr .
SELECTION-SCREEN END OF BLOCK bl1.

PARAMETERS : p_mode(1) DEFAULT 'E',
             p_vers LIKE ztco_routing-versn OBLIGATORY .

*ABP routing usg
PARAMETERS: p_usg LIKE plkod-verwe DEFAULT '10' NO-DISPLAY.

INITIALIZATION.
*monthly labor performance rate
  p_eff-low = '97'. APPEND p_eff.
  p_eff-low = '97'. APPEND p_eff.
  p_eff-low = '97'. APPEND p_eff.
  p_eff-low = '97'. APPEND p_eff.
  p_eff-low = '97'. APPEND p_eff.
  p_eff-low = '97'. APPEND p_eff.
  p_eff-low = '97'. APPEND p_eff.
  p_eff-low = '97'. APPEND p_eff.
  p_eff-low = '97'. APPEND p_eff.
  p_eff-low = '97'. APPEND p_eff.
  p_eff-low = '97'. APPEND p_eff.
  p_eff-low = '97'. APPEND p_eff.
*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Avaialable M/H : by Cost center
  PERFORM select_cosl.

* Product Manufacturing : by Material
  PERFORM select_plaf.

* Rounting M/H(Labor) : by Cost center
  PERFORM select_routing_info.

  PERFORM calculate_abp_mh.

  PERFORM calculate_current_routing .

  PERFORM display_data.


*&---------------------------------------------------------------------*
*&      Form  select_cosl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_cosl.
  DATA : l_cnt(3) TYPE n,
         l_field(20),
         l_field2(20).

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_cosl_temp
     FROM cosl
     WHERE gjahr = p_bdatj
       AND ( wrttp = '01' AND versn = p_versn )
       AND objnr LIKE '%MAN_HR%'.

  LOOP AT it_cosl_temp.
    MOVE-CORRESPONDING it_cosl_temp TO it_cosl.
    it_cosl-kostl = it_cosl_temp-objnr+6(6).
    CLEAR : l_cnt.
    DO 12 TIMES.
      l_cnt = l_cnt + 1.
*-----capacity
      CONCATENATE 'IT_COSL_TEMP-KAP' l_cnt INTO l_field.
      ASSIGN  (l_field)    TO   <f_field> .
      IF <f_field> =  0 .
*-------plan qty
        CONCATENATE 'IT_COSL_TEMP-LST' l_cnt INTO l_field.
        ASSIGN  (l_field)    TO   <f_field> .
      ENDIF.
      CONCATENATE 'IT_COSL-MH' l_cnt INTO l_field2.
      ASSIGN  (l_field2)  TO   <f_field2> .

      READ TABLE p_eff INDEX l_cnt.
      IF sy-subrc <> 0.
        READ TABLE p_eff INDEX 1.
      ENDIF.

      <f_field2> = <f_field> * p_eff-low / 100.
    ENDDO.
    COLLECT  it_cosl.
    CLEAR it_cosl.
  ENDLOOP.
ENDFORM.                    " select_cosl
*&---------------------------------------------------------------------*
*&      Form  SELECT_ROUTING_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_routing_info.
  CLEAR g_startdt.
  CONCATENATE p_bdatj '0101' INTO g_startdt.
  CHECK NOT it_plaf[] IS INITIAL.

  SELECT matnr mtart INTO CORRESPONDING FIELDS OF TABLE it_mara
    FROM mara
     FOR ALL ENTRIES IN it_plaf
   WHERE matnr = it_plaf-matnr.

  LOOP AT it_mara .
*   If new car material code 7th digit 2 are 'EL' .
*   => do like MIP
    IF it_mara-mtart = 'FERT' AND it_mara-matnr+6(2) = p_new.
      it_mip-matnr = it_mara-matnr.
      APPEND it_mip. CLEAR it_mip.
*   FERT
    ELSEIF  it_mara-mtart = 'FERT' .
      it_mi-matnr = it_mara-matnr.
      it_mi-plnnr = it_mara-matnr+6(7) .
      APPEND it_mi. CLEAR it_mi.
*   MIP
    ELSE.
      it_mip-matnr = it_mara-matnr.
      APPEND it_mip. CLEAR it_mip.
    ENDIF.
  ENDLOOP.

* 1) MI Rounting (FERT)
  IF NOT it_mi[]  IS INITIAL.
    PERFORM make_mi_routing.
  ENDIF.

* 2) MIP Rounting (HALB)
  IF NOT it_mip[]  IS INITIAL.
    PERFORM select_mip_matnr.
* 2-1) MIP :Rate routing
    IF NOT it_rate[] IS INITIAL.
      PERFORM make_mip_routing TABLES it_rate
                               USING  'R'.
    ENDIF.
* 2-2) MIP:Product routing
    IF NOT it_product[] IS INITIAL.
      PERFORM make_mip_routing TABLES it_product
                               USING  'N'.
    ENDIF.
  ENDIF.

* For get work center mapping
  SELECT objid arbpl INTO CORRESPONDING FIELDS OF TABLE it_crhd
     FROM crhd
     FOR ALL ENTRIES IN it_plpo
     WHERE objty = 'A'
       AND objid = it_plpo-arbid.

ENDFORM.                    " SELECT_ROUTING_INFO
*&---------------------------------------------------------------------*
*&      Form  SELECT_PLAF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_plaf.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_plaf_temp
     FROM plaf
     WHERE plscn = p_plscn              "Scenario
       AND sobes = 'E'                  "Special procurement
*       AND stlan = '6'                  "BOM usage
       AND matnr IN s_matnr.


  LOOP AT it_plaf_temp.
    MOVE-CORRESPONDING it_plaf_temp TO it_plaf.
    it_plaf-perio(6) = it_plaf_temp-pedtr.
    COLLECT it_plaf. CLEAR it_plaf.
  ENDLOOP.


  SORT it_plaf BY plwrk matnr.
ENDFORM.                    " SELECT_PLAF
*&---------------------------------------------------------------------*
*&      Form  calculate_abp_mh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_abp_mh.
  DATA : l_cnt(3) TYPE n,
         l_field(40).
  DATA : BEGIN OF tmpt OCCURS 0,
       perio(6),
       kostl TYPE kostl,
       arbpl  LIKE crhd-arbpl,
       eng_mh LIKE it_plaf-gsmng,
       END OF tmpt .

  SORT it_plaf.
  LOOP AT it_plaf.
    LOOP AT it_plpo WHERE matnr = it_plaf-matnr .
      tmpt-perio   = it_plaf-perio.
      CLEAR it_crhd .
      READ TABLE it_crhd WITH KEY objid = it_plpo-arbid.
      tmpt-kostl   = it_crhd-arbpl.
      tmpt-arbpl   = it_plpo-arbid.
      tmpt-eng_mh  = it_plaf-gsmng * it_plpo-vgw03.
      CHECK tmpt-eng_mh <>  0 .
      COLLECT tmpt. CLEAR tmpt.
    ENDLOOP.
  ENDLOOP.


  LOOP AT it_plaf WHERE matnr IN s_matnr.
    LOOP AT it_plpo WHERE matnr = it_plaf-matnr .
      itab-werks   = it_plaf-plwrk.
      itab-matnr   = it_plpo-matnr.
      itab-perio   = it_plaf-perio.
      itab-type    = it_plpo-type.
      itab-vgw01   = it_plpo-vgw01.    "Set
      itab-vgw02   = it_plpo-vgw02.    "Machine

      itab-vge01   = it_plpo-vge01.    "unit Set
      itab-vge02   = it_plpo-vge02.    "unit Machine
      itab-vge03   = it_plpo-vge03.    "unit MH

      CLEAR tmpt.
      READ TABLE tmpt WITH KEY arbpl = it_plpo-arbid
                               perio = it_plaf-perio.
      CHECK sy-subrc = 0 .
      itab-kostl = tmpt-kostl.
      CLEAR it_cosl.
      READ TABLE it_cosl WITH KEY kostl = itab-kostl.
      l_cnt = itab-perio+4(2).
      CONCATENATE 'IT_COSL-MH' l_cnt INTO l_field.
      ASSIGN  (l_field)    TO   <f_field> .
      itab-cc_rate =  <f_field>     / tmpt-eng_mh .
      itab-abp_mh  =  itab-cc_rate  * it_plpo-vgw03.
      IF itab-cc_rate <> 0 AND it_plpo-vgw03 <> 0 AND
         itab-abp_mh = 0 .
        itab-abp_mh = '0.001' .
      ENDIF.
      CHECK NOT itab-abp_mh IS INITIAL.
      COLLECT itab. CLEAR itab.
    ENDLOOP.
  ENDLOOP.


  PERFORM make_actual_display_data.

  PERFORM make_product_display_data.

  SORT it_rout.
  SORT it_display.


ENDFORM.                    " calculate_abp_mh
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
*-- ALV layout
  PERFORM alv_set_layout   USING  space  space
                                  space  space.
  gs_layout-box_fieldname          = 'CHK'.

*-- Event
  PERFORM alv_get_event    USING  gt_events.

*-- Fieldcategory
  PERFORM alv_get_fieldcat TABLES gt_fieldcat USING 'DISPLAY'.
  PERFORM alv_chg_fieldcat TABLES gt_fieldcat.

*-- Sort
* perform set_sort         tables gt_alv_sort.

*-- Top of page
*  PERFORM SET_TOP_PAGE.

*-- Display
  PERFORM alv_grid_display TABLES it_display.


ENDFORM.                    " display_data

*&---------------------------------------------------------------------*
*&      Form  alv_chg_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM alv_chg_fieldcat TABLES pt_fieldcat TYPE slis_t_fieldcat_alv.
  READ TABLE it_display INDEX 1.

  LOOP AT pt_fieldcat INTO gs_fieldcat.
    CLEAR :  gs_fieldcat-key, gs_fieldcat-no_out.

    IF gs_fieldcat-col_pos < 5.
      gs_fieldcat-key     = 'X'.
    ENDIF.

    CASE gs_fieldcat-fieldname.
      WHEN 'MATNR'.
        set_fieldcat gs_fieldcat 'Product'.
      WHEN 'WERKS'.
        set_fieldcat gs_fieldcat 'Plant'.
      WHEN 'TYPE'.
        set_fieldcat gs_fieldcat 'Type'.
      WHEN 'KOSTL'.
        set_fieldcat gs_fieldcat 'CC'.
      WHEN 'ABPTOT'.
        set_fieldcat gs_fieldcat 'Total'.
      WHEN 'ABP001'.
        set_fieldcat gs_fieldcat '1'.
        set_fieldcat_sum gs_fieldcat 'X'.
      WHEN 'ABP002'.
        set_fieldcat gs_fieldcat '2' .
        set_fieldcat_sum gs_fieldcat 'X'.
      WHEN 'ABP003'.
        set_fieldcat gs_fieldcat '3' .
        set_fieldcat_sum gs_fieldcat 'X'.
      WHEN 'ABP004'.
        set_fieldcat gs_fieldcat '4'.
        set_fieldcat_sum gs_fieldcat 'X'.
      WHEN 'ABP005'.
        set_fieldcat gs_fieldcat '5' .
        set_fieldcat_sum gs_fieldcat 'X'.
      WHEN 'ABP006'.
        set_fieldcat gs_fieldcat '6'.
        set_fieldcat_sum gs_fieldcat 'X'.
      WHEN 'ABP007'.
        set_fieldcat gs_fieldcat '7'.
        set_fieldcat_sum gs_fieldcat 'X'.
      WHEN 'ABP008'.
        set_fieldcat gs_fieldcat '8'.
        set_fieldcat_sum gs_fieldcat 'X'.
      WHEN 'ABP009'.
        set_fieldcat gs_fieldcat '9'.
        set_fieldcat_sum gs_fieldcat 'X'.
      WHEN 'ABP010'.
        set_fieldcat gs_fieldcat '10'.
        set_fieldcat_sum gs_fieldcat 'X'.
      WHEN 'ABP011'.
        set_fieldcat gs_fieldcat '11'.
        set_fieldcat_sum gs_fieldcat 'X'.
      WHEN 'ABP012'.
        set_fieldcat gs_fieldcat '12'.
        set_fieldcat_sum gs_fieldcat 'X'.
    ENDCASE.
    CLEAR: gs_fieldcat-cfieldname,
           gs_fieldcat-ctabname.
    MODIFY pt_fieldcat FROM gs_fieldcat.
  ENDLOOP.
ENDFORM.                    " alv_chg_fieldcat
*&---------------------------------------------------------------------*
*&      Form  make_mip_routing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_RATE  text
*----------------------------------------------------------------------*
FORM make_mip_routing TABLES   p_tab  STRUCTURE it_rate
                       USING   p_plnty.

  DATA : BEGIN OF it_plpo_temp2 OCCURS 0,
          werks  LIKE plpo-werks,
          matnr   LIKE mara-matnr,
          plnty   LIKE plpo-plnty,
          plnnr   LIKE plpo-plnnr,
          plnkn   LIKE plpo-plnkn,
          arbid   LIKE plpo-arbid,
          zaehl   LIKE plpo-zaehl,
          datuv   LIKE plpo-datuv,
          vgw01   LIKE plpo-vgw01,    "Set
          vgw02   LIKE plpo-vgw02,    "Machine
          vgw03   LIKE plpo-vgw03,    "Labor
          vge01   LIKE plpo-vge01,    "Set
          vge02   LIKE plpo-vge02,    "Machine
          vge03   LIKE plpo-vge03,    "Labor
        END OF it_plpo_temp2 .

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_plpo_temp2
     FROM mapl AS a
    INNER JOIN plko AS b
      ON  a~plnty = b~plnty
     AND  a~plnnr = b~plnnr
     AND  a~plnal = b~plnal
    INNER JOIN plas AS c
       ON b~plnty = c~plnty
      AND b~plnnr = c~plnnr
     AND  b~plnal = c~plnal
    INNER JOIN plpo AS d
       ON c~plnty = d~plnty
      AND c~plnnr = d~plnnr
      AND c~plnkn = d~plnkn
    FOR ALL ENTRIES IN p_tab
    WHERE a~plnty = p_plnty         "R:Rate routing N:Product
      AND a~matnr = p_tab-matnr
      AND a~loekz = ''
      AND b~verwe = '1'             "Usage
      AND b~datuv <= g_startdt      "Valid from
      AND b~delkz = ''              "Delete indicator
      AND c~loekz = ''              "Delete indicator
      AND d~loekz = ''.

* delete old data; change number
  SORT it_plpo_temp2 BY plnnr arbid ASCENDING
                  datuv       DESCENDING.
  DATA: w_plpo_temp LIKE it_plpo_temp2.
  LOOP AT it_plpo_temp2.
    IF  it_plpo_temp2-plnnr = w_plpo_temp-plnnr
    AND it_plpo_temp2-arbid = w_plpo_temp-arbid.
      DELETE it_plpo_temp2.
    ENDIF.
    w_plpo_temp = it_plpo_temp2.
  ENDLOOP.

  LOOP AT it_plpo_temp2.
    MOVE-CORRESPONDING it_plpo_temp2 TO it_plpo.
    IF p_plnty = 'R'.
      it_plpo-type = 'R'.
    ELSE.
      it_plpo-type = 'N'.
    ENDIF.
    COLLECT it_plpo. CLEAR it_plpo.
  ENDLOOP.


ENDFORM.                    " make_mip_routing
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
FORM pf_status_set USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD'.
ENDFORM.                    " PF_STATUS_SET

*---------------------------------------------------------------------*
*      Form USER_COMMAND
*---------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                                  rs_selfield TYPE slis_selfield.


  CASE r_ucomm.
    WHEN 'CRE_ROU'.
      PERFORM make_bdc_rout.
      PERFORM get_routing_info.
      PERFORM call_bdc_for_routing.
      rs_selfield-refresh = 'X'.

    WHEN 'DEL_ROU'.
      PERFORM make_bdc_rout.
      PERFORM get_routing_info.
      IF it_chg_rout[] IS INITIAL.
        MESSAGE e000 WITH 'Please select at least 1 product!!!'.
      ENDIF.

      PERFORM call_bdc_for_routing_del.
      rs_selfield-refresh = 'X'.

    WHEN 'SAVE'.
      PERFORM save_ztco_routing.
  ENDCASE.

ENDFORM.                    "ALV_EVENT_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_FOR_ROUTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bdc_for_routing.
  DATA : l_month_cnt(3) TYPE n,
         l_cnt(5) TYPE n.
  DATA : l_type(10),
         l_new(1).

  DO 12 TIMES.
    l_month_cnt = l_month_cnt + 1.
    LOOP AT it_bdc_rout .
      AT NEW matnr.
        l_new = 'X'.
        CLEAR l_cnt.
      ENDAT.
      l_cnt = l_cnt + 1.
*     Header
      IF l_new = 'X'.
        CLEAR l_new.
        PERFORM make_bdc_header  USING     l_month_cnt
                                 CHANGING  l_type.
      ENDIF.
*     Detail
      PERFORM make_bdc_detail USING l_cnt
                                    l_month_cnt
                                    l_type.

      AT END OF matnr.
        IF l_type = 'CREATE'.
          PERFORM bdc_dynpro  USING   'SAPLCPDI' g_screen.
        ELSE.
          PERFORM bdc_dynpro  USING   'SAPLCPDO' '1200'.
        ENDIF.
        PERFORM bdc_field USING 'BDC_OKCODE' '=BU'.
        CLEAR l_new.
        wa_opt-dismode = p_mode.
        wa_opt-updmode = 'S'.
        wa_opt-defsize = 'X'.
        CALL TRANSACTION g_code USING it_bdcdata
                                OPTIONS FROM wa_opt
                                MESSAGES INTO it_msg.
        PERFORM get_detail_msg.
        REFRESH: it_bdcdata, it_msg.
        CLEAR : g_code, l_type.
      ENDAT.
    ENDLOOP.
  ENDDO.
ENDFORM.                    " CALL_BDC_FOR_ROUTING
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1258   text
*      -->P_1259   text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING p_program p_dynpro.

  CLEAR wa_bdcdata.

  wa_bdcdata-program = p_program.
  wa_bdcdata-dynpro = p_dynpro.
  wa_bdcdata-dynbegin = 'X'.
  APPEND wa_bdcdata TO it_bdcdata.
ENDFORM.                    " bdc_dynpro
*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1267   text
*      -->P_IT_DISPLAY_KOSTL  text
*      -->P_1269   text
*      -->P_IT_DISPLAY_KOSTL  text
*      -->P_1271   text
*      -->P_IT_DISPLAY_KOSTL  text
*      -->P_1273   text
*      -->P_IT_DISPLAY_KOSTL  text
*      -->P_1275   text
*      -->P_1276   text
*----------------------------------------------------------------------*
FORM bdc_field USING    p_fnam
                        p_fval.
  CLEAR wa_bdcdata.
  wa_bdcdata-fnam = p_fnam.
  wa_bdcdata-fval = p_fval.
  APPEND wa_bdcdata TO it_bdcdata.

ENDFORM.                    " bdc_field
*&---------------------------------------------------------------------*
*&      Form  MAKE_BDC_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_bdc_header  USING p_month_cnt
                            p_type.

  DATA : l_aennr(15).
  DATA : l_datuv(8).
  CONCATENATE p_bdatj p_month_cnt+1(2) '01' '-001' INTO    l_aennr.
  CLEAR it_chg_rout.
  READ TABLE it_chg_rout WITH KEY matnr = it_bdc_rout-matnr
                                  aennr = l_aennr.
  IF sy-subrc <> 0 .
    p_type = 'CREATE'.
  ELSE.
    p_type = 'CHANGE'.
  ENDIF.

  IF it_bdc_rout-bdc_type  = 'R'.
    g_screen = '5400'.
    IF p_type = 'CREATE'.
      g_code = 'CA21'.
    ELSE.
      g_code = 'CA22'.
    ENDIF.
  ELSE.
    g_screen = '1400'.
    IF p_type = 'CREATE'.
      g_code = 'CA01'.
    ELSE.
      g_code = 'CA02'.
    ENDIF.

  ENDIF.

  IF p_type = 'CREATE'.
    PERFORM bdc_dynpro  USING   'SAPLCPDI' '1010'.
    PERFORM bdc_field   USING : 'RC27M-MATNR' it_bdc_rout-matnr ,
                                'RC27M-WERKS' it_bdc_rout-werks,
                                'RC271-AENNR' l_aennr,
                                'BDC_OKCODE' '=ANLG'.

    PERFORM bdc_dynpro  USING   'SAPLCPDA' '1010'.
    PERFORM bdc_field   USING : 'PLKOD-VERWE' p_usg,
                                'PLKOD-STATU' '4',
                                'BDC_OKCODE' '=VOUE'.
  ELSE.

    CONCATENATE it_chg_rout-datuv+4(2) it_chg_rout-datuv+6(2)
                it_chg_rout-datuv(4) INTO l_datuv.
    PERFORM bdc_dynpro  USING   'SAPLCPDI' '1010'.
    PERFORM bdc_field   USING : 'RC27M-MATNR' it_bdc_rout-matnr ,
                                'RC27M-WERKS' it_bdc_rout-werks,
                                'RC271-STTAG' l_datuv,
                                'RC271-PLNAL' it_chg_rout-plnal,
                                'BDC_OKCODE' '=VOUE'.

    PERFORM bdc_dynpro USING 'SAPLCPDI' g_screen.
    PERFORM bdc_field  USING : 'BDC_OKCODE'   '=MAAL'.

    PERFORM bdc_dynpro USING 'SAPLCPDI' g_screen.
    PERFORM bdc_field  USING : 'BDC_CURSOR'   'PLPOD-VORNR(01)',
                               'BDC_OKCODE'   '=PICK'.
  ENDIF.
ENDFORM.                    " MAKE_BDC_ITAB
*&---------------------------------------------------------------------*
*&      Form  get_detail_msg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_detail_msg.
  DATA: w_numb LIKE bapiret2-number,
        w_msgv1 LIKE bapiret2-message_v1,
        w_msgv2 LIKE bapiret2-message_v2,
        w_msgv3 LIKE bapiret2-message_v3,
        w_msgv4 LIKE bapiret2-message_v4,
        w_messa LIKE bapiret2-message.

  LOOP AT it_msg INTO wa_msg.
    w_numb = wa_msg-msgnr.
    w_msgv1 = wa_msg-msgv1.
    w_msgv2 = wa_msg-msgv2.
    w_msgv3 = wa_msg-msgv3.
    w_msgv4 = wa_msg-msgv4.

    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id         = wa_msg-msgid
        number     = w_numb
*       LANGUAGE   = SY-LANGU
        textformat = 'ASC'
*   LINKPATTERN       =
        message_v1 = w_msgv1
        message_v2 = w_msgv2
   message_v3      = w_msgv3
   message_v4      = w_msgv4
     IMPORTING
       message     = w_messa
*   RETURN            =
* TABLES
*   TEXT              =
              .
    wa_result-matnr = it_rout-matnr.
    wa_result-messa = w_messa.
    APPEND wa_result TO it_result.
    CLEAR wa_result.
  ENDLOOP.

ENDFORM.                    " get_detail_msg
*&---------------------------------------------------------------------*
*&      Form  make_bdc_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_CNT  text
*      -->P_L_MONTH_CNT  text
*----------------------------------------------------------------------*
FORM make_bdc_detail USING    p_cnt
                              p_month_cnt
                              p_type.

  DATA : l_field1(20),
         l_field2(20),
         l_field3(20).

  DATA : l_vgw01(15), " like plpo-vgw01,
         l_vgw02(15), " like plpo-vgw02,
         l_vgw03(15). " like plpo-vgw03

  FIELD-SYMBOLS: <f_field1>,
                 <f_field2>,
                 <f_field3>.

* Rate routing
  IF it_bdc_rout-bdc_type  = 'R'.
    g_screen = '5400'.
* Production routing
  ELSE.
    g_screen = '1400'.
  ENDIF.

  CONCATENATE 'IT_BDC_ROUT-SET' p_month_cnt INTO l_field1.
  ASSIGN  (l_field1)    TO   <f_field1> .
  CONCATENATE 'IT_BDC_ROUT-MCH' p_month_cnt INTO l_field2.
  ASSIGN  (l_field2)    TO   <f_field2> .
  CONCATENATE 'IT_BDC_ROUT-ABP' p_month_cnt INTO l_field3.
  ASSIGN  (l_field3)    TO   <f_field3> .

  l_vgw01 =   <f_field1> .
  l_vgw02 =   <f_field2> .
  l_vgw03 =   <f_field3> .

  CONDENSE : l_vgw01,l_vgw02,l_vgw03.

* Unit conversion
  IF it_bdc_rout-vge01 = 'STD'.
    it_bdc_rout-vge01 = 'HR'.
  ENDIF.

  IF it_bdc_rout-vge02 = 'STD'.
    it_bdc_rout-vge02 = 'HR'.
  ENDIF.

  IF it_bdc_rout-vge03 = 'STD'.
    it_bdc_rout-vge03 = 'HR'.
  ENDIF.


* Create routing
  IF p_type = 'CREATE'.
    IF p_cnt = 1.
      PERFORM bdc_dynpro USING 'SAPLCPDI' g_screen.
      PERFORM bdc_field   USING : 'PLPOD-ARBPL(01)' it_bdc_rout-kostl,
                                  'BDC_OKCODE'      '/00'.
      PERFORM bdc_dynpro USING    'SAPLCPDI' g_screen.
      PERFORM bdc_field  USING :  'RC27X-FLG_SEL(01)'  'X',
                                  'BDC_OKCODE'         '=VOD1'.
    ELSE.
      PERFORM bdc_dynpro USING 'SAPLCPDI' g_screen.
      PERFORM bdc_dynpro USING 'SAPLCPDI' g_screen.
      PERFORM bdc_field   USING : 'PLPOD-ARBPL(02)' it_bdc_rout-kostl,
                                  'BDC_OKCODE'      '/00'.
      PERFORM bdc_dynpro USING    'SAPLCPDI' g_screen.
      PERFORM bdc_field  USING :  'RC27X-FLG_SEL(01)'  ' ',
                                  'RC27X-FLG_SEL(02)'  'X',
                                  'BDC_OKCODE'         '=VOD1'.
    ENDIF.

*   Routing Operation detail
    PERFORM bdc_dynpro USING 'SAPLCPDO' '1200'.
    PERFORM bdc_field  USING : 'PLPOD-VGW01'   l_vgw01,
                               'PLPOD-VGW02'   l_vgw02,
                               'PLPOD-VGW03'   l_vgw03,
                               'PLPOD-VGE01'   it_bdc_rout-vge01,
                               'PLPOD-VGE02'   it_bdc_rout-vge02,
                               'PLPOD-VGE03'   it_bdc_rout-vge03,
                               'BDC_OKCODE'    '=BACK'.

    PERFORM bdc_dynpro USING 'SAPLCPDI' g_screen.
    PERFORM bdc_field  USING : 'BDC_OKCODE'         '=P+'.

* change routing
  ELSE.
*   Routing Operation detail
    PERFORM bdc_dynpro USING 'SAPLCPDO' '1200'.
    PERFORM bdc_field  USING : 'PLPOD-VGW01'   l_vgw01,
                               'PLPOD-VGW02'   l_vgw02,
                               'PLPOD-VGW03'   l_vgw03,
                               'PLPOD-VGE01'   it_bdc_rout-vge01,
                               'PLPOD-VGE02'   it_bdc_rout-vge02,
                               'PLPOD-VGE03'   it_bdc_rout-vge03,
                               'BDC_OKCODE'    '=OD+'.
  ENDIF.

ENDFORM.                    " make_bdc_detail
*&---------------------------------------------------------------------*
*&      Form  call_bdc_for_routing_del
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bdc_for_routing_del.
  DATA : l_month_cnt(2) TYPE n,
         l_cnt(5) TYPE n.
  DATA : l_new(1).


*  l_month_cnt = l_month_cnt + 1.
  LOOP AT it_chg_rout.

    CLEAR it_bdc_rout.
    READ TABLE it_bdc_rout WITH KEY matnr = it_chg_rout-matnr
                                    werks = it_chg_rout-werks.
    IF it_bdc_rout-bdc_type  = 'R'.
      g_code = 'CA22'.
    ELSE.
      g_code = 'CA02'.
    ENDIF.
    l_month_cnt = it_chg_rout-aennr+4(2).
    PERFORM make_bdc_header_del USING l_month_cnt.

    wa_opt-dismode = p_mode.
    wa_opt-updmode = 'S'.
    wa_opt-defsize = 'X'.
    CALL TRANSACTION g_code USING it_bdcdata
                            OPTIONS FROM wa_opt
                            MESSAGES INTO it_msg.
    PERFORM get_detail_msg.
    REFRESH: it_bdcdata, it_msg.
    CLEAR : g_code.
  ENDLOOP.


ENDFORM.                    " call_bdc_for_routing_del
*&---------------------------------------------------------------------*
*&      Form  make_bdc_header_del
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MONTH_CNT  text
*----------------------------------------------------------------------*
FORM make_bdc_header_del USING    p_month_cnt.
  DATA : l_aennr(15).
  DATA : l_pre_month_cnt(3) TYPE n.
  DATA : l_aennr2(15).


  IF it_bdc_rout-bdc_type  = 'R'.
    g_screen = '5200'.
  ELSE.
    g_screen = '1200'.
  ENDIF.


*   Creat Change no
  IF p_month_cnt = '12'.
    CONCATENATE p_bdatj '12' '31' '-001' INTO    l_aennr.
  ELSE.
    p_month_cnt = p_month_cnt + 1 .
    CONCATENATE p_bdatj p_month_cnt(2) '01' '-001' INTO    l_aennr.
  ENDIF.

  PERFORM bdc_dynpro  USING   'SAPLCPDI' '1010'.
  PERFORM bdc_field   USING : 'RC27M-MATNR' it_chg_rout-matnr ,
                              'RC27M-WERKS' it_chg_rout-werks,
                              'RC271-AENNR' l_aennr,
                              'RC271-PLNNR' it_chg_rout-plnnr,
                              'RC271-PLNAL' it_chg_rout-plnal,
                              'BDC_OKCODE' '=ALUE'.

  PERFORM bdc_dynpro  USING   'SAPLCPDI' g_screen .
  PERFORM bdc_field   USING :
                              'BDC_OKCODE' '=LOE'.

  PERFORM bdc_dynpro  USING   'SAPLSPO1' '0100'.
  PERFORM bdc_field   USING :
                              'BDC_OKCODE' '=YES'.

  PERFORM bdc_dynpro  USING   'SAPLCPDI' g_screen .
  PERFORM bdc_field   USING :
                              'BDC_OKCODE' '=BACK'.

  PERFORM bdc_dynpro  USING   'SAPLSPO1' '0100'.
  PERFORM bdc_field   USING :
                              'BDC_OKCODE' '=YES'.



ENDFORM.                    " make_bdc_header_del
*&---------------------------------------------------------------------*
*&      Form  GET_ROUTING_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_routing_info.
  DATA : BEGIN OF it_del_rout OCCURS 0 ,
          plnty LIKE plko-plnty,
          plnnr LIKE plko-plnnr,
          plnal LIKE plko-plnal,
         END OF it_del_rout.

* UD1K941578 - by IG.MOON 9/21/2007 {
*  CHECK NOT it_bdc_rout IS INITIAL.
  CHECK NOT it_bdc_rout[] IS INITIAL.
* }

  CLEAR : it_chg_rout, it_chg_rout[].


  SELECT matnr a~werks b~plnty b~plnnr b~plnal b~aennr b~datuv
    INTO CORRESPONDING FIELDS OF TABLE it_chg_rout
    FROM mapl AS a
   INNER JOIN plko AS b
      ON a~plnty = b~plnty
     AND a~plnnr = b~plnnr
     AND a~plnal = b~plnal
     FOR ALL ENTRIES IN it_bdc_rout
   WHERE a~plnty = it_bdc_rout-bdc_type   "R:Rate routing N:Product
     AND a~matnr = it_bdc_rout-matnr
     AND a~loekz = ''
     AND b~verwe = '10'            "Usage
     AND b~loekz = ''    .         "Delete indicator


ENDFORM.                    " GET_ROUTING_INFO
*&---------------------------------------------------------------------*
*&      Form  make_bdc_rout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_bdc_rout.
  CLEAR : it_bdc_rout[], it_bdc_rout.
  CLEAR it_rout.
  LOOP AT it_rout WHERE type = 'A'.
    CLEAR it_display.
    READ TABLE it_display WITH KEY matnr = it_rout-matnr.
    IF it_display-chk = 'X'.
      MOVE-CORRESPONDING it_rout TO it_bdc_rout.
      APPEND it_bdc_rout. CLEAR it_bdc_rout.
    ENDIF.
  ENDLOOP.


  IF it_bdc_rout[] IS INITIAL.
    MESSAGE e000 WITH 'Please select at least 1 product!!!'.
  ENDIF.
ENDFORM.                    " make_bdc_rout
*&---------------------------------------------------------------------*
*&      Form  save_ztco_routing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_ztco_routing.
  DATA : BEGIN OF it_routing OCCURS 0,
         matnr  LIKE ztco_routing-matnr,
         END OF it_routing.
  DATA : it_ztco_rout LIKE ztco_routing OCCURS 0 WITH HEADER LINE.

  DATA : l_title(50),
         l_text1(50),
         l_text2(50),
         l_answer .


*  SELECT BDATJ POPER VERSN WERKS MATNR KOSTL
  SELECT matnr
   INTO CORRESPONDING FIELDS OF TABLE it_routing
   FROM ztco_routing
    FOR ALL ENTRIES IN it_display
  WHERE bdatj   = p_bdatj
    AND versn   = p_vers
    AND werks   = it_display-werks
    AND matnr   = it_display-matnr
    AND kostl   = it_display-kostl .


  IF NOT it_routing[] IS INITIAL.
    l_title  = 'Warning TABLE : ZTCO_ROUTING' .
    l_text1  = 'The key data will be overwritten in Table' .
    l_text2  = 'Do you want to save? '.
  ELSE.
    l_title  = 'TABLE : ZTCO_ROUTING' .
    l_text1  = 'The new routing data' .
    l_text2  = 'Do you want to save? '.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1      = l_text1
            textline2      = l_text2
            titel          = l_title
            cancel_display = ''
       IMPORTING
            answer         = l_answer.

  CHECK l_answer = 'J'.

  LOOP AT it_display.
    MOVE-CORRESPONDING it_display TO it_ztco_rout.
    it_ztco_rout-kokrs   = p_kokrs.
    it_ztco_rout-bdatj   = p_bdatj.
    it_ztco_rout-versn   = p_vers.
    it_ztco_rout-erdat   = sy-datum.
    it_ztco_rout-erzet   = sy-uzeit.
    it_ztco_rout-ernam   = sy-uname.
    APPEND it_ztco_rout. CLEAR it_ztco_rout.
  ENDLOOP.

  MODIFY ztco_routing FROM TABLE it_ztco_rout.

  IF sy-subrc = 0 .
    COMMIT WORK.
    MESSAGE s000 WITH 'Save sucessfully'.
  ENDIF.

ENDFORM.                    " save_ztco_routing
*&---------------------------------------------------------------------*
*&      Form  calculate_current_routing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_current_routing.

  DATA : it_tc31a LIKE v_tc31a OCCURS 0 WITH HEADER LINE.
  DATA : l_zgkal LIKE v_tc31a-zgkal,
         l_month(3) TYPE n,
         l_date TYPE datum,
         l_last_date TYPE datum,
         l_field(20),
         l_total LIKE it_display-abptot.


* Calculate Efficiency : by month
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_tc31a
     FROM tc31a
     FOR ALL ENTRIES IN it_plpo
     WHERE zgrad = it_plpo-zgr03.

  SORT it_tc31a.
  LOOP AT it_plpo.
    CLEAR : g_val_cnt , l_total.
    MOVE-CORRESPONDING it_plpo TO it_display.
    CLEAR it_crhd .
    READ TABLE it_crhd WITH KEY objid = it_plpo-arbid.
    it_display-kostl   = it_crhd-arbpl.
*   if it_display-kostl = 'MXBXB1'. break-point. endif.
    it_display-type = 'C'.
    CLEAR l_month.
    DO 12 TIMES.
      l_month = l_month + 1.
      CLEAR : l_date, l_last_date.
      CONCATENATE p_bdatj l_month+1(2) '01' INTO l_date.

      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
           EXPORTING
                day_in            = l_date
           IMPORTING
                last_day_of_month = l_last_date.

      CLEAR l_zgkal.
*     Check last day of every month &
*     Get matching data
      LOOP AT it_tc31a WHERE zgrad = it_plpo-zgr03
                         AND datub >= l_last_date.
        l_zgkal =     it_tc31a-zgkal.
        EXIT.
      ENDLOOP.

      CONCATENATE 'IT_DISPLAY-ABP' l_month INTO l_field.
      ASSIGN  (l_field)    TO   <f_field> .
      IF l_zgkal <>  0 .
        <f_field> = it_plpo-vgw03 / l_zgkal * 100.
      ELSE.
        <f_field> = it_plpo-vgw03.
      ENDIF.
      IF <f_field> <> 0 .
        g_val_cnt = g_val_cnt + 1.
      ENDIF.
      l_total =  <f_field>  + l_total.
    ENDDO.
    IF g_val_cnt <>  0 .
      it_display-abptot = l_total / g_val_cnt.
    ENDIF.
    COLLECT it_display. CLEAR it_display.
  ENDLOOP.


ENDFORM.                    " calculate_current_routing
*&---------------------------------------------------------------------*
*&      Form  MAKE_ACTUAL_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_actual_display_data.
  DATA : l_cnt(3) TYPE n,
         l_field(40),
         l_total LIKE it_display-abptot.

  SORT itab.
  LOOP AT itab.
    AT NEW kostl .
      CLEAR : g_val_cnt, l_total.
    ENDAT.

    CLEAR l_field.
    MOVE-CORRESPONDING itab TO it_rout.
    it_rout-type = 'A'.
    CLEAR it_mara.
    READ TABLE it_mara WITH KEY matnr = itab-matnr.
    IF it_mara-mtart = 'FERT'.
      it_rout-bdc_type = 'R'.
    ELSE.
      CLEAR it_rate.
      READ TABLE it_rate WITH KEY matnr = itab-matnr.
      IF sy-subrc = 0 .
        it_rout-bdc_type = 'R'.
      ELSE.
        CLEAR it_product.
        READ TABLE it_product WITH KEY matnr = itab-matnr.
        IF sy-subrc = 0 .
          it_rout-bdc_type = 'N'.
        ENDIF.
      ENDIF.
    ENDIF.
    l_cnt = itab-perio+4(2).
    CONCATENATE 'IT_ROUT-ABP' l_cnt INTO l_field.
    ASSIGN (l_field)    TO   <f_field> .
    <f_field> = itab-abp_mh .
    IF  <f_field> <>  0 .
      g_val_cnt = g_val_cnt + 1.
    ENDIF.
*   MH Average
*   FIXME (count if not ZERO)
    IF g_val_cnt <>  0 .
      l_total  = <f_field> + l_total.
    ENDIF.

    CONCATENATE 'IT_ROUT-SET' l_cnt INTO l_field.
    ASSIGN (l_field)    TO   <f_field> .
    <f_field> = itab-vgw01 .

    CONCATENATE 'IT_ROUT-MCH' l_cnt INTO l_field.
    ASSIGN (l_field)    TO   <f_field> .
    <f_field> = itab-vgw02 .
    AT END OF kostl.
      IF g_val_cnt <> 0 .
        it_rout-abptot = l_total / g_val_cnt .
      ENDIF.
    ENDAT.
    COLLECT  it_rout.
    MOVE-CORRESPONDING it_rout TO it_display.
    COLLECT it_display. CLEAR it_display.
    CLEAR it_rout.
  ENDLOOP.


ENDFORM.                    " MAKE_ACTUAL_DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  MAKE_PRODUCT_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_product_display_data.
  DATA : l_cnt(3) TYPE n,
         l_field(40).

  SORT it_plaf.
  LOOP AT it_plaf WHERE matnr IN s_matnr.
    CLEAR l_field.
    it_rout-werks = it_plaf-plwrk.
    it_rout-matnr = it_plaf-matnr.
    it_rout-type  = 'P'.
    CLEAR it_mara.
    READ TABLE it_mara WITH KEY matnr = it_rout-matnr.
    IF it_mara-mtart = 'FERT'.
      it_rout-bdc_type = 'R'.
    ELSE.
      CLEAR it_rate.
      READ TABLE it_rate WITH KEY matnr = it_rout-matnr.
      IF sy-subrc = 0 .
        it_rout-bdc_type = 'R'.
      ELSE.
        CLEAR it_product.
        READ TABLE it_product WITH KEY matnr = it_rout-matnr.
        IF sy-subrc = 0 .
          it_rout-bdc_type = 'N'.
        ENDIF.
      ENDIF.
    ENDIF.
    l_cnt = it_plaf-perio+4(2).
    CONCATENATE 'IT_ROUT-ABP' l_cnt INTO l_field.
    ASSIGN  (l_field)  TO   <f_field> .
    <f_field> = it_plaf-gsmng .
    it_rout-abptot = it_plaf-gsmng .
    COLLECT  it_rout.
    MOVE-CORRESPONDING it_rout TO it_display.
    COLLECT it_display. CLEAR it_display.
    CLEAR it_rout.
  ENDLOOP.

ENDFORM.                    " MAKE_PRODUCT_DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  make_mi_routing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_mi_routing.

* MI Rounting (FERT)
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_plpo_temp
     FROM plko AS a
    INNER JOIN plas AS b
       ON a~plnty = b~plnty
      AND a~plnnr = b~plnnr
      AND a~plnal = b~plnal
    INNER JOIN plpo AS c
       ON b~plnty = c~plnty
      AND b~plnnr = c~plnnr
      AND b~plnkn = c~plnkn
      FOR ALL ENTRIES IN it_mi
     WHERE a~plnty = 'M'
       AND a~plnnr = it_mi-plnnr
       AND a~verwe = '1'             "Usage
       AND a~statu IN ('3', '4')     "Status
       AND a~datuv <= g_startdt      "Valid from
       AND a~delkz = ''              "Delete indicator
       AND b~loekz = ''              "Delete indicator
       AND c~loekz = '' .            "Delete indicator

* delete old data; change number
  SORT it_plpo_temp BY plnnr arbid ASCENDING
                       datuv       DESCENDING.
  DATA: w_plpo_temp LIKE it_plpo_temp.
  LOOP AT it_plpo_temp.
    IF  it_plpo_temp-plnnr = w_plpo_temp-plnnr
    AND it_plpo_temp-arbid = w_plpo_temp-arbid.
      DELETE it_plpo_temp.
    ENDIF.
    w_plpo_temp = it_plpo_temp.
  ENDLOOP.

  LOOP AT it_mi.
    LOOP AT it_plpo_temp WHERE plnnr = it_mi-plnnr.
      MOVE-CORRESPONDING it_plpo_temp TO it_plpo.
      it_plpo-matnr = it_mi-matnr.
      it_plpo-type = 'F'.
      COLLECT it_plpo.  CLEAR it_plpo.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " make_mi_routing
*&---------------------------------------------------------------------*
*&      Form  select_mip_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_mip_matnr.
* MI Rounting (FERT)
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_marc
     FROM marc
     FOR ALL ENTRIES IN it_mip
     WHERE matnr = it_mip-matnr.

  LOOP AT it_marc.
    CLEAR it_plaf.
    READ TABLE it_plaf WITH KEY plwrk = it_marc-werks
                                matnr = it_marc-matnr
                       BINARY SEARCH.
    IF sy-subrc <>  0 .
      DELETE it_marc.
      CONTINUE.
    ELSE.
      IF it_marc-matnr+6(2) = p_new.
        it_rate-matnr = it_marc-matnr.
        APPEND it_rate. CLEAR it_rate.
*     Repeatitive mfg
      ELSEIF it_marc-sauft = 'X'.
        it_rate-matnr = it_marc-matnr.
        APPEND it_rate. CLEAR it_rate.
      ELSE.
        it_product-matnr = it_marc-matnr.
        APPEND it_product. CLEAR it_product.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " select_mip_matnr
