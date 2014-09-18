*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZMMI01120T
*& Program Name   : Module Price Interface to Vaatz
*& Created by     : Victor Park
*& Created on     : 08.10.2011
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& RFC func. : Z_MM_IF_OB_01_007
*& Stru.     : ZMMS_VZ020 , ZMMT_VZ020
*&----------------------------------------------------------------------
*& Desc.     : price : per 1 unit
*&
*&----------------------------------------------------------------------

REPORT zmmi01120t MESSAGE-ID zmpp.

TABLES : mseg, mara, s021, mast, ztpp_wosum,  ztmm_mod_pri_bk,
         ztpp_vm, ztpp_rpid.

*- ALV
TYPE-POOLS: slis.
DATA: gt_fieldcat         TYPE slis_t_fieldcat_alv,
      gs_layout           TYPE slis_layout_alv,
      gs_sort             TYPE slis_sortinfo_alv,
      gt_sort             TYPE slis_t_sortinfo_alv,
      gs_light            TYPE lvc_s_layo,
      gs_print            TYPE slis_print_alv,
      gt_sp_group         TYPE slis_t_sp_group_alv,
      gt_events           TYPE slis_t_event,
      gs_events           LIKE  LINE OF gt_events,
      g_save              VALUE 'A',
      gx_variant          LIKE disvariant,
      g_variant           LIKE disvariant.

DATA : ls_title         TYPE slis_listheader, "alv header
       alv_t_listheader TYPE slis_t_listheader.

DATA : g_extab          TYPE slis_t_extab,
       g_extab_ln       LIKE   LINE  OF  g_extab.

DATA : g_user_command  TYPE slis_formname VALUE 'USER_COMMAND'.
DATA : t_colinfo_table TYPE slis_t_specialcol_alv WITH HEADER LINE.
DATA : g_repid         LIKE sy-repid.


DATA : lv_sdate TYPE sy-datum.
DATA : lv_edate TYPE sy-datum.

DATA : BEGIN OF it_vehicle  OCCURS 0,
        erp_fsc  LIKE materialid-matnr_ext,
        model_code LIKE ztpp_vm-model_code,
        body_no    LIKE ztpp_vm-body_no,
        ver        LIKE ztpp_vm-ver,
        rp18_sdate LIKE ztpp_rpid-rp18_sdate,
        intc       LIKE ztpp_vm-intc,
       END OF it_vehicle.

DATA : BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE zmms_vz020.
*DATA :
*       matkl  LIKE mara-matkl,
*       mtart  LIKE mara-mtart,
*       zbudat LIKE mseg-zbudat.
DATA : END OF it_data.

DATA : it_vz021 LIKE zmmt_vz021 OCCURS 0 WITH HEADER LINE.
DATA : l_zdate  LIKE zmmt_vz021-zdate,
       l_ztime  LIKE zmmt_vz021-ztime.

DATA : it_upgvc      LIKE it_data OCCURS 0 WITH HEADER LINE.
DATA : it_component  LIKE it_data OCCURS 0 WITH HEADER LINE.
DATA : it_mast       LIKE mast OCCURS  0 WITH HEADER LINE.
DATA : it_materialid LIKE materialid OCCURS 0 WITH HEADER LINE.
DATA : it_unique     LIKE it_data OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_mbew  OCCURS 0.
        INCLUDE STRUCTURE mbew .
DATA : END OF it_mbew.

DATA : it_interface  LIKE zmms_vz020 OCCURS 0 WITH HEADER LINE.

DATA : wa_save TYPE  zmmt_vz020.
DATA : it_save TYPE STANDARD TABLE OF  zmmt_vz020 WITH HEADER LINE.

DATA : wa_module TYPE   ztmm_mod_pri_bk.
DATA : it_module LIKE   ztmm_mod_pri_bk OCCURS 0 WITH HEADER LINE.
DATA : lv_chk(1),
       lv_digit TYPE i.

*- RETURN MESSAGE
DATA : e_return TYPE zmms0053.
DATA : l_msgtxt(200).

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.

PARAMETERS : p_bukrs     LIKE mseg-bukrs DEFAULT 'H201' OBLIGATORY.
SELECT-OPTIONS : s_werks FOR  mseg-werks DEFAULT 'P001' OBLIGATORY,
*                 s_mtart FOR mara-mtart,
*                                "    DEFAULT 'PART' OPTION EQ SIGN I,
                 s_spmon FOR s021-spmon  NO-EXTENSION   OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) text-t01.
PARAMETERS : p_send RADIOBUTTON GROUP r1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(5) text-t02.
PARAMETERS : p_alv  RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b2.
*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM init.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_spmon-low.
  PERFORM pov_month USING s_spmon-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_spmon-high.
  PERFORM pov_month USING s_spmon-high.

*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM select_data.
  PERFORM modify_data.

*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF p_send = 'X'.
    PERFORM  pro_batch.
  ELSE.
    PERFORM pro_alv.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM select_data .
*-Read data from I/F 6 (Spec Vs Module)

  DATA : lv_zzcdate TYPE   ztimestamp.

  CLEAR : it_data[], it_data.

  CONCATENATE sy-datum sy-uzeit INTO lv_zzcdate.

  SELECT zdate ztime INTO (l_zdate, l_ztime)
  FROM zmmt_vz021
    UP TO 1 ROWS
  WHERE spmon = s_spmon-low
    ORDER BY zdate DESCENDING
             ztime DESCENDING.
  ENDSELECT.

  IF sy-subrc <> 0.
    MESSAGE s000 WITH 'Please execute ZMMI01130T program fist'.
    STOP.
  ENDIF.

  SELECT * INTO TABLE it_vz021
  FROM zmmt_vz021
  WHERE   zdate = l_zdate
    AND   ztime = l_ztime.

  SORT it_vz021 BY zmodule.
  DELETE ADJACENT DUPLICATES FROM it_vz021 COMPARING zmodule.

  LOOP AT it_vz021.

    it_data-bukrs = 'HMMA'.
    it_data-matnr  = it_vz021-zmodule.
    it_data-spmon  = s_spmon-low.
    it_data-zzcdate = lv_zzcdate.
    APPEND it_data.
  ENDLOOP.

  CONCATENATE s_spmon-low '01' INTO lv_sdate.
  IF s_spmon-high IS INITIAL.
    PERFORM cal_month_lastdate USING s_spmon-low  lv_edate.
  ELSE.
    PERFORM cal_month_lastdate USING s_spmon-high lv_edate.
  ENDIF.
*
*  SELECT  c~fsc AS erp_fsc  a~model_code a~body_no  a~ver b~rp18_sdate
*          a~intc
*    INTO CORRESPONDING FIELDS OF TABLE it_vehicle
*  FROM ztpp_vm AS a INNER JOIN ztpp_rpid AS b
*                ON a~model_code = b~model_code
*               AND a~body_no    = b~body_no
*                    INNER JOIN ztpp_wosum AS c
*                ON a~wo_serial = c~wo_ser
*               AND a~wo_nation = c~nation
*               AND a~wo_dealer = c~dealer
*               AND a~extc      = c~extc
*               AND a~intc      = c~intc
*  WHERE b~rp18_sdate  >= lv_sdate
*    AND b~rp18_sdate  <= lv_edate
*    AND a~wo_dealer   LIKE 'A%'
*    AND a~usg_car     = 'P' .          "Production
*
*  IF it_vehicle[] IS INITIAL.
*    MESSAGE s005.
*    STOP.
*  ENDIF.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .
  DATA : lt_stb LIKE stpox OCCURS 0 WITH HEADER LINE.
  DATA : lv_lines   TYPE i.
  DATA : lv_zzcdate TYPE   ztimestamp.
  DATA : lv_menge   LIKE stko-bmeng.
  DATA : lv_upgvc   LIKE mara-matnr.
  DATA : lv_matnr   LIKE mara-matnr.
  DATA : lv_date LIKE sy-datum,
         lv_time LIKE sy-uzeit.

  DATA : BEGIN OF it_mara  OCCURS 0,
            matnr LIKE mara-matnr,
            mtart LIKE mara-mtart,
            beskz LIKE marc-beskz,
            profl LIKE mara-profl,
         END OF it_mara.

  DATA : it_a018 LIKE a018 OCCURS 0 WITH HEADER LINE,
         it_konh LIKE konh OCCURS 0 WITH HEADER LINE,
         it_konp LIKE konp OCCURS 0 WITH HEADER LINE.

*  CLEAR : it_data[], it_upgvc[], it_component[].
  CLEAR : it_upgvc[], it_component[].

*  CONCATENATE sy-datum sy-uzeit INTO lv_zzcdate.
*
*  SORT it_vehicle BY erp_fsc ver.
*  DELETE ADJACENT DUPLICATES FROM it_vehicle COMPARING erp_fsc ver.
*
*  DESCRIBE TABLE it_vehicle LINES lv_lines.
*
*
*  LOOP AT it_vehicle.
*
*    PERFORM progress_bar USING  sy-tabix lv_lines.
*
*    lv_matnr  =  it_vehicle-erp_fsc.
*
**-  Module Part
*    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
*      EXPORTING
*        capid                 = 'PP01'
*        datuv                 = lv_edate
*        ehndl                 = '1'
*        cuobj                 = '999999999999'
*        mktls                 = 'X'
**       mehrs                 = 'X'
*        mehrs                 = ''                         "One level
*        mmory                 = '1'
*        mtnrv                 = lv_matnr
*        emeng                 = 1
*        svwvo                 = 'X'
*        werks                 = s_werks-low
*        stlal                 = it_vehicle-ver     " alternative no.
*        vrsvo                 = 'X'
*      TABLES
*        stb                   = lt_stb
*      EXCEPTIONS
*        alt_not_found         = 1
*        call_invalid          = 2
*        material_not_found    = 3
*        missing_authorization = 4
*        no_bom_found          = 5
*        no_plant_data         = 6
*        no_suitable_bom_found = 7
*        conversion_error      = 8.
*
*    IF   NOT lt_stb[] IS INITIAL.
*      LOOP AT lt_stb.
*        IF lt_stb-posnr = '7000'.
*          CLEAR : it_data.
*          MOVE-CORRESPONDING lt_stb TO it_data.
*          it_data-bukrs = 'HMMA'.
*          it_data-matnr  = lt_stb-idnrk.
*          it_data-spmon  = it_vehicle-rp18_sdate+0(6).
*          it_data-menge  = lt_stb-mnglg.
*          it_data-zzcdate = lv_zzcdate.
*
*          lv_chk  = 'X'.
*          lv_digit  = strlen( lt_stb-idnrk ).
*
*          IF lv_digit > 12.
*            PERFORM get_color_from_od USING lt_stb-knobj
*                                            lt_stb-idnrk
*                                            lv_edate
*                                            it_vehicle-intc
*                                            lv_chk .
*            IF lv_chk <> 'X'.
**              CONTINUE.
*            ENDIF.
*          ENDIF.
*
*          APPEND it_data.
*        ENDIF.
*      ENDLOOP.
*    ELSE.
*    ENDIF.
*
*  ENDLOOP.
*
*  SORT it_data BY matnr.
*  DELETE ADJACENT DUPLICATES FROM it_data COMPARING matnr.


*-Price and etc
  LOOP AT it_data.


    CLEAR : lv_date, it_module[].

    SELECT run_date run_time INTO  (lv_date, lv_time)
    FROM ztmm_mod_pri_bk
    UP TO 1 ROWS
    WHERE matnr     =  it_data-matnr
*      AND run_date  <= lv_edate
      AND input_date  <= lv_edate
    ORDER BY run_date DESCENDING
             run_time DESCENDING.
    ENDSELECT.

    IF NOT lv_date IS INITIAL.

      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_module
      FROM ztmm_mod_pri_bk
      WHERE matnr  =  it_data-matnr
        AND run_date  =  lv_date
        AND run_time  =  lv_time.

      LOOP AT it_module.
        CLEAR : it_component.
        MOVE-CORRESPONDING it_data TO it_component.
        it_component-upgvc  = it_module-upgvc.
        it_component-mcomp  = it_module-comp.
        it_component-menge  = it_module-qnty.
        it_component-lifnr  = it_module-lifnr.
        IF it_module-peinh > 0.
          it_component-netpr  = it_module-netpr / it_module-peinh.
        ELSE.
          it_component-netpr  = it_module-netpr.
        ENDIF.
*        it_component-netpr  = it_module-dmbtr.
        it_component-resn_c  = it_module-kzust.
        it_component-app_t   = it_module-datab.
        it_component-waers   = 'USD'.
        APPEND it_component.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  CHECK NOT it_component[] IS INITIAL.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_mara
  FROM mara AS a INNER JOIN marc AS b
              ON a~matnr  =  b~matnr
    FOR ALL ENTRIES IN it_component
  WHERE a~matnr  = it_component-mcomp.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_a018
  FROM a018 AS a
    FOR ALL ENTRIES IN it_component
  WHERE a~matnr  = it_component-mcomp
    AND a~lifnr  = it_component-lifnr
    AND a~kappl  =  'M'
    AND a~kschl  =  'PB00'
    AND a~ekorg  =  'PU01'
    AND a~esokz  =  '0'
    AND a~datab  <= lv_edate
    AND a~datbi  >= lv_edate.

  SORT it_a018 BY matnr.
  DELETE ADJACENT DUPLICATES FROM it_a018 COMPARING matnr.

  IF NOT it_a018[] IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_konh
    FROM konh AS a
      FOR ALL ENTRIES IN it_a018
    WHERE a~knumh  = it_a018-knumh.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_konp
    FROM konp AS a
      FOR ALL ENTRIES IN it_konh
    WHERE a~knumh  = it_konh-knumh.
  ENDIF.

  SORT it_konh BY knumh.
  SORT it_mara BY matnr.
  SORT it_konp BY knumh kopos DESCENDING.

  DELETE ADJACENT DUPLICATES FROM it_konp COMPARING knumh.

  LOOP AT it_component.

    CLEAR : it_mara, it_a018, it_konh.
    READ TABLE it_mara WITH KEY matnr   = it_component-mcomp
                                          BINARY SEARCH.
    READ TABLE it_a018 WITH KEY matnr   = it_component-mcomp
                                          BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE it_konh WITH KEY  knumh  = it_a018-knumh
                                          BINARY SEARCH.
      IF sy-subrc = 0.
        it_component-pum_n   =   it_konh-kosrt.
        it_component-resn_c  =   it_konh-kzust.  "Update reason code

        READ TABLE it_konp WITH KEY knumh = it_konh-knumh
                                            BINARY SEARCH.
        IF sy-subrc = 0.
          it_component-txbhw  =  it_component-netpr *
                                            it_konp-kbetr / 1000.
        ENDIF.
      ENDIF.
    ENDIF.


    it_component-labor  = it_mara-profl.
    it_component-zzeitm = 'M'.            "TBD

    MODIFY it_component.
  ENDLOOP.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  PRO_BATCH
*&---------------------------------------------------------------------*
FORM pro_batch .
  DATA : v_dest(30) VALUE 'WMHR01'.   "Interface Destination.

  CLEAR : e_return.

  CHECK NOT it_component[] IS INITIAL.

  CALL FUNCTION 'Z_MM_IF_OB_01_007' DESTINATION v_dest
    IMPORTING
      e_return              = e_return
    TABLES
      t_data                = it_component
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF e_return-type = 'S' AND  sy-subrc = 0.   "Success
    PERFORM save_log  USING 'S' 'Success'    ''.
    MESSAGE s000 WITH 'Interface : Success'.
  ELSE.
    PERFORM save_log  USING 'E' e_return-message l_msgtxt.
    MESSAGE e000 WITH  e_return-message l_msgtxt.
  ENDIF.

ENDFORM.                    " PRO_BATCH

*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG
*&---------------------------------------------------------------------*
FORM save_log  USING    p_type p_msg1 p_msg2.
  DATA : l_zseq(10) TYPE n.

  CLEAR : it_save[], it_save.

  SELECT zseq INTO l_zseq
    FROM zmmt_vz020
    UP TO 1 ROWS
    WHERE zdate = sy-datum
    ORDER BY zseq DESCENDING.
  ENDSELECT.

  LOOP AT it_component.
    MOVE-CORRESPONDING it_component TO it_save.

    it_save-zdate = sy-datum.
    it_save-zseq  = l_zseq + sy-tabix.
    it_save-ztime = sy-uzeit.
    it_save-ernam = sy-uname.
    it_save-zrslt = p_type.
    IF p_type = 'E'.
      IF NOT  p_msg1 IS INITIAL.
        it_save-zmsg  = p_msg1.
      ELSE.
        it_save-zmsg  = p_msg2.
      ENDIF.
    ENDIF.

    APPEND it_save.
    CLEAR : it_save.

  ENDLOOP.

  INSERT zmmt_vz020 FROM TABLE it_save
                             ACCEPTING DUPLICATE KEYS .
  COMMIT WORK AND WAIT.

ENDFORM.                    " SAVE_LOG
*&---------------------------------------------------------------------*
*&      Form  PRO_ALV
*&---------------------------------------------------------------------*
FORM pro_alv .


  PERFORM layout_build       USING   gs_layout.
  PERFORM sorttab_build      USING   gt_sort.
  PERFORM fieldcat           TABLES  gt_fieldcat
                             USING   'IT_DATA'.

  PERFORM list_header_write USING alv_t_listheader[].
  PERFORM append_alv_event  CHANGING   gt_events.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = g_repid
*     i_callback_pf_status_set = 'PF_STATUS'
      i_callback_user_command  = g_user_command
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
      it_sort                  = gt_sort
      i_save                   = g_save
*     is_variant               = g_variant
      it_events                = gt_events[]
    TABLES
      t_outtab                 = it_component[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.                    " PRO_ALV
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_BUILD
*&---------------------------------------------------------------------*
FORM layout_build  USING  p_layout TYPE slis_layout_alv.

  p_layout-zebra             = 'X'.
  p_layout-colwidth_optimize = 'X'.
*  p_layout-key_hotspot = 'X'.
*  p_layout-box_fieldname  =    'CHK'.  "SELECTION FIELD
*  p_layout-coltab_fieldname = 'COL_COLOR'. "color field of itabe
*  p_layout-cell_merge        = 'X'.
*  p_layout-detail_popup      = 'X'.
*  p_layout-detail_titlebar   = sy-title.
*  p_layout-no_subtotals      = ''.

ENDFORM.                    " LAYOUT_BUILD
*&---------------------------------------------------------------------*
*&      Form  SORTTAB_BUILD
*&---------------------------------------------------------------------*
FORM sorttab_build  USING   p_sort TYPE slis_t_sortinfo_alv.

  CLEAR: gs_sort, p_sort[].

  gs_sort-spos      = '1'.
  gs_sort-tabname   = 'IT_COMPONENT'.
  gs_sort-fieldname = 'MATNR'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

  gs_sort-spos      = '2'.
  gs_sort-tabname   = 'IT_COMPONENT'.
  gs_sort-fieldname = 'UPGVC'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

  gs_sort-spos      = '3'.
  gs_sort-tabname   = 'IT_COMPONENT'.
  gs_sort-fieldname = 'MCOMP'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

ENDFORM.                    " SORTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
FORM fieldcat  TABLES   pt_fieldcat TYPE  slis_t_fieldcat_alv
               USING    p_name      TYPE  slis_tabname.

  DATA: l_datum(08).

  sy-datum = sy-datum + 1.
  MOVE sy-datum TO l_datum.
  SET PARAMETER ID 'ALVBUFFER' FIELD l_datum.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = g_repid
*     i_structure_name   = p_name
      i_internal_tabname = p_name
      i_inclname         = g_repid
    CHANGING
      ct_fieldcat        = pt_fieldcat[].

  LOOP AT pt_fieldcat.
    CASE pt_fieldcat-fieldname.
      WHEN 'BUKRS'.
        pt_fieldcat-seltext_m    = 'Company Code'.
        pt_fieldcat-no_out       = 'X'.
      WHEN 'LABOR'.
        pt_fieldcat-seltext_m    = 'Source.'.
      WHEN 'UPG_G'.
        pt_fieldcat-seltext_m    = 'UPG Group'.
      WHEN 'SEQ'.
        pt_fieldcat-seltext_m    = 'Price Seq.'.
      WHEN OTHERS.

    ENDCASE.
    pt_fieldcat-reptext_ddic =
    pt_fieldcat-seltext_s    =
    pt_fieldcat-seltext_l    =
    pt_fieldcat-seltext_m.

    MODIFY pt_fieldcat.

  ENDLOOP.

ENDFORM.                    " FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  LIST_HEADER_WRITE
*&---------------------------------------------------------------------*
FORM list_header_write  USING alv_t_listheader TYPE slis_t_listheader.

  CLEAR   : ls_title, alv_t_listheader.
  REFRESH : alv_t_listheader.

  DATA : h_title(30), s_title(60),  a_title(60).

  ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
  CONCATENATE '*Company Code : '  p_bukrs INTO ls_title-info.
  APPEND ls_title TO alv_t_listheader.

  ls_title-typ = 'S'.
  CONCATENATE '*Month : ' s_spmon-low   INTO ls_title-info.
  APPEND ls_title TO alv_t_listheader.

ENDFORM.                    " LIST_HEADER_WRITE
*&---------------------------------------------------------------------*
*&      Form  APPEND_ALV_EVENT
*&---------------------------------------------------------------------*
FORM append_alv_event  CHANGING p_alv_event TYPE slis_t_event.
* TOP-OF-PAGE Event

  DATA ls_events TYPE slis_alv_event.
  ls_events-name  =  'TOP_OF_PAGE'.
  ls_events-form  =  'TOP_OF_PAGE'.
  APPEND ls_events TO p_alv_event.

ENDFORM.                    " APPEND_ALV_EVENT

*&---------------------------------------------------------------------
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = alv_t_listheader.

ENDFORM. " TOP_OF_PAGE

*&--------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&--------------------------------------------------------------------*
FORM  user_command USING ucomm    LIKE sy-ucomm
                    p_selfield    TYPE slis_selfield.
* double click : UCOMM = &IC1
  CASE ucomm.

  ENDCASE.

  p_selfield-refresh = 'X'.
ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  POV_MONTH
*&---------------------------------------------------------------------*
FORM pov_month  USING pv_spmon.
  DATA: lv_spmon TYPE spmon.

  MOVE: sy-datum(6) TO lv_spmon.

  CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
    EXPORTING
      actual_month               = lv_spmon
    IMPORTING
      selected_month             = pv_spmon
    EXCEPTIONS
      factory_calendar_not_found = 1
      holiday_calendar_not_found = 2
      month_not_found            = 3
      OTHERS                     = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " POV_MONTH
*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
FORM init .
  DATA : lv_datum TYPE sy-datum.

  g_repid  = sy-repid.

  CONCATENATE sy-datum+0(6) '01' INTO lv_datum.
  lv_datum  = lv_datum - 1.

  s_spmon-low    = lv_datum+0(6).
  s_spmon-sign   = 'I'.
  s_spmon-option = 'EQ'.
  APPEND s_spmon.

ENDFORM.                    " INIT
*&---------------------------------------------------------------------*
*&      Form  CAL_MONTH_LASTDATE
*&---------------------------------------------------------------------*
FORM cal_month_lastdate  USING    p_spmon
                                  p_edate.
  DATA : lv_date TYPE sy-datum.
  CONCATENATE p_spmon '01' INTO lv_date.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_date
    IMPORTING
      last_day_of_month = p_edate
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CAL_MONTH_LASTDATE
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_BAR
*&---------------------------------------------------------------------*
FORM progress_bar  USING    p_tabix
                            p_lines.
  DATA : lv_per TYPE i .
  DATA : lv_text(50).
  DATA : lv_mode TYPE i.
  DATA : lv_lines(7) TYPE n,
         lv_tabix(7)  TYPE n.

  lv_lines  = p_lines.
  lv_tabix  = p_tabix.

  lv_per = ( p_tabix * 100 / p_lines ) .
  CONCATENATE 'Processing : ' lv_tabix ' / ' lv_lines INTO lv_text.
  lv_mode =  lv_per MOD 5.
  IF lv_mode   =  0.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_per
        text       = lv_text.
  ENDIF.
ENDFORM.                    " PROGRESS_BAR
*&---------------------------------------------------------------------*
*&      Form  GET_COLOR_FROM_OD
*&---------------------------------------------------------------------*
FORM get_color_from_od  USING    p_knobj
                                 p_idnrk
                                 p_edate
                                 p_intc
                                 p_chk .

  DATA: lt_cuob LIKE cuob OCCURS 0 WITH HEADER LINE.
  DATA: lt_cukb LIKE cukb OCCURS 0 WITH HEADER LINE.
  DATA : BEGIN OF it_ktab OCCURS 0,
          text(100),
         END OF it_ktab.

  SELECT * INTO TABLE lt_cuob
    FROM cuob
   WHERE kntab =  'STPO'
     AND knobj =  p_knobj
     AND datuv <= p_edate.

  CHECK lt_cuob[] IS NOT INITIAL.

  SELECT * INTO TABLE lt_cukb
    FROM cukb
     FOR ALL ENTRIES IN lt_cuob
   WHERE knnum =  lt_cuob-knnum
     AND adzhl =  lt_cuob-adzhl
     AND datuv <= p_edate.

  LOOP AT lt_cukb.
    CALL FUNCTION 'CUKD_GET_KNOWLEDGE'
      EXPORTING
        knowledge_type     = 'S'
        relation           = lt_cukb-knnam
*       RELATION_NR        = ' '
        date               = sy-datum
      TABLES
        knowledge_tab      = it_ktab
      EXCEPTIONS
        no_knowledge_found = 1
        no_relation_found  = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
    ELSE.
      READ TABLE it_ktab INDEX 1.
      SHIFT it_ktab UP TO `'` LEFT.
      REPLACE `'` WITH '' INTO it_ktab.
      REPLACE `'` WITH '' INTO it_ktab.
      CONDENSE it_ktab.

      IF it_ktab = p_intc.
        p_chk = 'X'.
      ELSE.
        p_chk = ''.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_COLOR_FROM_OD
