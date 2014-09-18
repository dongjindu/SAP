*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZMMI01110T
*& Program Name   : Sub materials Interface to Vaatz
*& Created by     : Victor Park
*& Created on     : 08.10.2011
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& RFC func. : Z_MM_IF_OB_01_006
*& Stru.     : ZMMS_VZ019 , ZMMT_VZ019
*&----------------------------------------------------------------------

REPORT zmmi01110t MESSAGE-ID zmpp.

TABLES : mseg, mara, s021, mast,
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
       END OF it_vehicle.

DATA : BEGIN OF it_data OCCURS 0.
DATA : chk(1).
        INCLUDE STRUCTURE zmms_vz019.
*DATA :
*       matkl  LIKE mara-matkl,
*       mtart  LIKE mara-mtart,
*       zbudat LIKE mseg-zbudat.
DATA : END OF it_data.

*DATA : it_materialid LIKE materialid OCCURS 0 WITH HEADER LINE.
DATA : it_unique     LIKE it_data OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_mbew  OCCURS 0.
        INCLUDE STRUCTURE mbew .
DATA : END OF it_mbew.

DATA : it_interface  LIKE zmms_vz019 OCCURS 0 WITH HEADER LINE.

DATA : wa_save TYPE  zmmt_vz019.
DATA : it_save TYPE STANDARD TABLE OF  zmmt_vz019 WITH HEADER LINE.

*- RETURN MESSAGE
DATA : e_return TYPE zmms0053.
DATA : l_msgtxt(200).

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.

PARAMETERS : p_bukrs     LIKE mseg-bukrs DEFAULT 'H201' OBLIGATORY.
SELECT-OPTIONS : s_werks FOR  mseg-werks DEFAULT 'E001' TO 'P001'
                                                       OBLIGATORY,
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

  CONCATENATE s_spmon-low '01' INTO lv_sdate.
  IF s_spmon-high IS INITIAL.
    PERFORM cal_month_lastdate USING s_spmon-low  lv_edate.
  ELSE.
    PERFORM cal_month_lastdate USING s_spmon-high lv_edate.
  ENDIF.

  SELECT  a~model_code a~body_no  c~fsc AS erp_fsc  a~ver b~rp18_sdate
      INTO CORRESPONDING FIELDS OF TABLE it_vehicle
  FROM ztpp_vm AS a INNER JOIN ztpp_rpid AS b
                ON a~model_code = b~model_code
               AND a~body_no    = b~body_no
                    INNER JOIN ztpp_wosum AS c
                ON a~wo_serial = c~wo_ser
               AND a~wo_nation = c~nation
               AND a~wo_dealer = c~dealer
               AND a~extc      = c~extc
               AND a~intc      = c~intc
  WHERE  b~rp18_sdate  >= lv_sdate
    AND b~rp18_sdate  <= lv_edate
    AND a~wo_dealer   LIKE 'A%'
    AND a~usg_car     = 'P' .                      "Production


  IF it_vehicle[] IS INITIAL.
    MESSAGE s005.
    STOP.
  ENDIF.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .
  DATA : lt_stb LIKE stpox OCCURS 0 WITH HEADER LINE.
  DATA : lv_lines   TYPE i.
  DATA : lv_zzcdate TYPE  ztimestamp.
  DATA : lv_matnr   LIKE mara-matnr.
  DATA : lv_b(4).

  CLEAR : it_data[], it_interface[].

  CONCATENATE sy-datum sy-uzeit INTO lv_zzcdate.

  SORT it_vehicle BY erp_fsc ver.
  DELETE ADJACENT DUPLICATES FROM it_vehicle COMPARING erp_fsc ver.

*  SELECT *
*    INTO CORRESPONDING FIELDS OF TABLE it_materialid
*  FROM materialid
*    FOR ALL ENTRIES IN it_vehicle
*  WHERE matnr_ext   = it_vehicle-erp_fsc.
*  SORT it_materialid BY matnr_ext.

  DESCRIBE TABLE it_vehicle LINES lv_lines.


  LOOP AT it_vehicle.

    PERFORM progress_bar USING  sy-tabix lv_lines.

    lv_matnr  =  it_vehicle-erp_fsc.

    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
         EXPORTING
              capid                 = 'PP01'
              datuv                 = lv_edate
              ehndl                 = '1'
              mktls                 = 'X'
              mehrs                 = 'X'
              mmory                 = '1'
              mtnrv                 = lv_matnr
              emeng                 = 1
              svwvo                 = 'X'
              werks                 = 'P001'
              stlal                 = it_vehicle-ver  " alternative no.
              vrsvo                 = 'X'
         TABLES
              stb                   = lt_stb
         EXCEPTIONS
              alt_not_found         = 1
              call_invalid          = 2
              material_not_found    = 3
              missing_authorization = 4
              no_bom_found          = 5
              no_plant_data         = 6
              no_suitable_bom_found = 7
              conversion_error      = 8.

    IF  NOT lt_stb[] IS INITIAL.
      LOOP AT lt_stb.
        IF lt_stb-mtart = 'ROH1' AND lt_stb-idnrk+0(1)  = 'G'.
          CLEAR : it_data.
          MOVE-CORRESPONDING lt_stb TO it_data.
          it_data-bukrs  = 'HMMA'.
          it_data-werks  = 'HVA1'.
          it_data-matnr  = it_vehicle-erp_fsc.

          lv_b  =  it_data-matnr+14(4).
          it_data-matnr+14(3)  =  '   '.
          it_data-matnr+17(4)  =  lv_b.
          IF it_vehicle-ver  =  '00'.
            it_data-matnr+21(2)  = '  '.
          ELSE.
            it_data-matnr+21(2)  = it_vehicle-ver.
          ENDIF.


          it_data-spmon  = it_vehicle-rp18_sdate+0(6).
          it_data-mcomp  = lt_stb-idnrk.
          it_data-menge  = lt_stb-mnglg.
          it_data-meins  = lt_stb-meins.
          it_data-zzcdate = lv_zzcdate.
          COLLECT it_data.    "Collect with same bom compont
        ENDIF.
      ENDLOOP.
    ELSE.
    ENDIF.

  ENDLOOP.

  it_unique[] = it_data[].

  SORT it_unique BY mcomp.
  DELETE ADJACENT DUPLICATES FROM it_unique COMPARING  mcomp.

  IF NOT it_unique[] IS INITIAL.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE it_mbew
    FROM mbew
    FOR ALL ENTRIES IN it_unique
    WHERE matnr =  it_unique-mcomp.
  ENDIF.
  SORT it_mbew BY matnr.


  LOOP AT it_data.
    CLEAR : it_interface.

    CLEAR : it_mbew.
    READ TABLE it_mbew WITH KEY matnr =  it_data-mcomp
                                     BINARY SEARCH.
    IF it_mbew-vprsv = 'S'.
      IF it_mbew-peinh <> 0.
        it_data-stprs =  ( it_mbew-stprs * it_data-menge )
                           / it_mbew-peinh.
      ELSE.
        it_data-stprs = it_mbew-stprs.
      ENDIF.
    ELSE.
      IF it_mbew-peinh <> 0.
        it_data-stprs = ( it_mbew-verpr * it_data-menge )
                                      / it_mbew-peinh.
      ELSE.
        it_data-stprs = it_mbew-verpr.
      ENDIF.
    ENDIF.

    it_data-waers = 'USD'.
    MOVE-CORRESPONDING it_data TO it_interface.

    MODIFY it_data.
    APPEND it_interface.
  ENDLOOP.
ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  PRO_BATCH
*&---------------------------------------------------------------------*
FORM pro_batch .
  DATA : v_dest(30) VALUE 'WMHR01'.   "Interface Destination.

  CLEAR : e_return.

  CHECK NOT it_interface[] IS INITIAL.

  CALL FUNCTION 'Z_MM_IF_OB_01_006' DESTINATION v_dest
    IMPORTING
      e_return              = e_return
    TABLES
      t_data                = it_interface
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
    FROM zmmt_vz019
    UP TO 1 ROWS
    WHERE zdate = sy-datum
    ORDER BY zseq DESCENDING.
  ENDSELECT.

  LOOP AT it_interface.
    MOVE-CORRESPONDING it_interface TO it_save.

    it_save-zdate = sy-datum.
    it_save-zseq  = l_zseq + sy-tabix.
    it_save-ztime = sy-uzeit.
    it_save-ernam = sy-uname.
    it_save-zrslt = p_type.
    IF p_type = 'E'.
      IF  NOT p_msg1 IS INITIAL.
        it_save-zmsg  = p_msg1.
      ELSE.
        it_save-zmsg  = p_msg2.
      ENDIF.
    ENDIF.

    APPEND it_save.
    CLEAR : it_save.

  ENDLOOP.

  INSERT zmmt_vz019 FROM TABLE it_save
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
      i_callback_program = g_repid
*      i_callback_pf_status_set = 'PF_STATUS'
      i_callback_user_command = g_user_command
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat
      it_sort            = gt_sort
      i_save             = g_save
*      is_variant         = g_variant
      it_events          = gt_events[]
    TABLES
      t_outtab           = it_data[]
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.                    " PRO_ALV
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_BUILD
*&---------------------------------------------------------------------*
FORM layout_build  USING  p_layout TYPE slis_layout_alv.

  p_layout-zebra             = 'X'.
  p_layout-colwidth_optimize = 'X'.
*  p_layout-key_hotspot = 'X'.
  p_layout-box_fieldname  =    'CHK'.  "SELECTION FIELD
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

*  gs_sort-spos      = '1'.
*  gs_sort-tabname   = 'IT_DATA'.
*  gs_sort-fieldname = 'SPMON'.
*  gs_sort-up        = 'X'.
*  gs_sort-group     = 'BL'.
*  gs_sort-subtot    = ''.
*  APPEND gs_sort TO p_sort.

  gs_sort-spos      = '2'.
  gs_sort-tabname   = 'IT_DATA'.
  gs_sort-fieldname = 'MATNR'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

  gs_sort-spos      = '2'.
  gs_sort-tabname   = 'IT_DATA'.
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
*      i_structure_name      = p_name
      i_internal_tabname = p_name
      i_inclname         = g_repid
    CHANGING
      ct_fieldcat        = pt_fieldcat[].

  LOOP AT pt_fieldcat.
    CASE pt_fieldcat-fieldname.
      WHEN 'BUKRS'.
        pt_fieldcat-seltext_m    = 'Company Code'.
      WHEN 'LABOR'.
        pt_fieldcat-seltext_m    = 'Source.'.
      WHEN 'FLAG'.
        pt_fieldcat-seltext_m    = 'Work Flag'.
      WHEN 'STPRS'.
        pt_fieldcat-seltext_m    = 'Price'.
      WHEN 'MENGE'.
        pt_fieldcat-seltext_m    = 'Quantity'.
*        pt_fieldcat-do_sum       = 'X'.
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
