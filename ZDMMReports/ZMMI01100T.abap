*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZMMI01100T
*& Program Name   : GR Interface(Parts) to Vaatz
*& Created by     : Victor Park
*& Created on     : 07.07.2011
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& RFC func. : Z_MM_IF_OB_01_005
*& Stru.     : ZMMS_VZ018 , ZMMT_VZ018
*&----------------------------------------------------------------------

REPORT zmmi01100t MESSAGE-ID zmpp.

TABLES : mseg, mara, s021, mast, a018, eord.

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


DATA : BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE zmms_vz018.
DATA : bwart  LIKE mseg-bwart,
       shkzg  LIKE mseg-shkzg,
       matkl  LIKE mara-matkl,
       mtart  LIKE mara-mtart,
       zbudat LIKE mseg-zbudat.
DATA : END OF it_data.

DATA : it_modify LIKE it_data OCCURS 0 WITH HEADER LINE.
DATA : it_modify1 LIKE it_data OCCURS 0 WITH HEADER LINE.
DATA : it_eord    LIKE eord  OCCURS 0 WITH HEADER LINE. "Soruce list
DATA : it_collect LIKE zmms_vz018 OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_marc OCCURS 0,
       matnr LIKE marc-matnr,
       werks LIKE marc-werks,
       dispo LIKE marc-dispo.
DATA : END OF it_marc.

DATA : wa_save TYPE  zmmt_vz018.
DATA : it_save TYPE STANDARD TABLE OF  zmmt_vz018 WITH HEADER LINE.

*- RETURN MESSAGE
DATA : e_return TYPE zmms0053.
DATA : l_msgtxt(200).

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.

PARAMETERS : p_bukrs     LIKE mseg-bukrs DEFAULT 'H201' OBLIGATORY.
SELECT-OPTIONS : s_werks FOR  mseg-werks DEFAULT 'E001' TO 'P001'
                                                    OBLIGATORY,
                 s_mtart FOR mara-mtart,
                            "       DEFAULT 'PART' OPTION EQ SIGN I,
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
  DATA : lv_sdate TYPE sy-datum.
  DATA : lv_edate TYPE sy-datum.

  CONCATENATE s_spmon-low '01' INTO lv_sdate.
  IF s_spmon-high IS INITIAL.
    PERFORM cal_month_lastdate USING s_spmon-low  lv_edate.
  ELSE.
    PERFORM cal_month_lastdate USING s_spmon-high lv_edate.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_data
  FROM mseg AS a  INNER JOIN mara AS b
                 ON a~matnr = b~matnr
  WHERE a~zbudat        >= lv_sdate
    AND a~zbudat        <= lv_edate
    AND a~werks         IN s_werks
    AND b~mtart         IN s_mtart
    AND ( a~bwart = '101' OR a~bwart = '102' OR a~bwart = '121' OR
          a~bwart = '122' OR a~bwart = '161' OR a~bwart = '162' OR
          a~bwart = '411' OR a~bwart = '412' )
    %_HINTS ORACLE 'ORDERED USE_NL(T_00 T_01) INDEX (T_00 "MSEG~Z03")'.


ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .
  DATA : lt_stb LIKE stpox OCCURS 0 WITH HEADER LINE.
  DATA : lv_menge LIKE stko-bmeng.
  DATA : lv_lastday LIKE mkpf-budat.
  DATA : lv_spmon   LIKE s021-spmon.
  DATA : lv_lines   TYPE i.

  CLEAR : it_modify[], it_modify1[], it_marc[].

  DELETE it_data WHERE  menge = 0.

  SORT it_data BY matnr zbudat lifnr shkzg.

  LOOP AT it_data.
    MOVE-CORRESPONDING it_data TO it_modify.
    it_modify-spmon  = it_data-zbudat+0(6).
    IF it_data-shkzg  = 'H'.
      it_modify-menge = -1 * it_modify-menge.
    ENDIF.
    CLEAR : it_modify-bwart, it_modify-shkzg, it_modify-zbudat.

    COLLECT it_modify.
  ENDLOOP.

  SORT it_modify BY spmon matnr.
  DESCRIBE TABLE it_modify LINES lv_lines.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_marc
  FROM marc
    FOR ALL ENTRIES IN it_modify
  WHERE matnr  =  it_modify-matnr
    AND werks  =  it_modify-werks.

  SORT it_marc BY matnr werks.

  LOOP AT it_modify.

    PERFORM progress_bar USING  sy-tabix lv_lines.

    IF it_modify-spmon <> lv_spmon.
      CONCATENATE it_modify-spmon '01' INTO lv_lastday.

      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
        EXPORTING
          day_in            = lv_lastday
        IMPORTING
          last_day_of_month = lv_lastday
        EXCEPTIONS
          day_in_no_date    = 1
          OTHERS            = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

*-  explode BOM
    CLEAR : it_marc.
    READ TABLE it_marc WITH KEY matnr  =  it_modify-matnr
                                werks  =  it_modify-werks
                                BINARY SEARCH.
    IF it_marc-dispo  =  'M01'.                     "Module Code
*    IF it_modify-matkl  = 'MODP'.

      CLEAR : lt_stb, lt_stb[].

      SELECT SINGLE * FROM mast
      WHERE matnr = it_modify-matnr
        AND werks = it_modify-werks.

      IF sy-subrc = 0.

*-< Victor 01.31.2012   add Module-Assy part
        CLEAR : it_modify1.
        MOVE-CORRESPONDING it_modify TO it_modify1.
        APPEND it_modify1. CLEAR : it_modify1.
*->

        lv_menge  = it_modify-menge.
        CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
          EXPORTING
            capid                 = 'MD01'
            datuv                 = lv_lastday
            ehndl                 = '1'
            mktls                 = 'X'
            mehrs                 = 'X'
            mmory                 = '1'
            mtnrv                 = it_modify-matnr
            emeng                 = lv_menge
            svwvo                 = 'X'
            werks                 = it_modify-werks
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

        IF  NOT  lt_stb[] IS INITIAL.
          LOOP AT lt_stb.
*            IF lt_stb-mtart = 'PART' OR lt_stb-mtart = 'SUBP'.
            IF lt_stb-mtart = 'ROH'.  "HMMA
              CLEAR : it_modify1.
              MOVE-CORRESPONDING lt_stb TO it_modify1.
              it_modify1-spmon  = it_modify-spmon.
              it_modify1-matnr  = lt_stb-idnrk.
*              it_modify1-menge  = lt_stb-mnglg.
              it_modify1-menge  = lt_stb-mngko.
              it_modify1-flag_gbn = 'W'. "If mother is Module,then 'W'

*-            Vendor of Module sub material : from zmmt0104
              CLEAR : it_eord.
              READ TABLE it_eord WITH KEY  matnr  = lt_stb-idnrk.
              IF sy-subrc = 0.
                it_modify1-lifnr  =  it_eord-lifnr.
              ELSE.
                CLEAR : eord.
                SELECT   *
                 FROM eord
                  UP TO 1 ROWS
                WHERE matnr  = lt_stb-idnrk
                  AND vdatu  <=  lv_lastday
                  AND bdatu  >=  lv_lastday
                  ORDER BY autet DESCENDING.
                ENDSELECT.
                IF sy-subrc = 0.
                  it_modify1-lifnr  =  eord-lifnr.
                  MOVE-CORRESPONDING eord TO it_eord.
                  APPEND it_eord.
                ELSE.             "if No source list, check Info record
                  SELECT SINGLE lifnr INTO it_modify1-lifnr
                  FROM a018
                  WHERE matnr = lt_stb-idnrk
                    AND  datab <= lv_lastday
                    AND  datbi >= lv_lastday.
                ENDIF.
              ENDIF.

              APPEND it_modify1.
            ENDIF.
          ENDLOOP.
        ELSE.
        ENDIF.
      ELSE.
        CLEAR : it_modify1.
        MOVE-CORRESPONDING it_modify TO it_modify1.
        APPEND it_modify1.
      ENDIF.
    ELSE.
      CLEAR : it_modify1.
      MOVE-CORRESPONDING it_modify TO it_modify1.
      APPEND it_modify1.
    ENDIF.

    lv_spmon  = it_modify-spmon.
  ENDLOOP.


**-create interface data
  LOOP AT it_modify1.
    CLEAR : it_collect.

    it_modify1-bukrs  =  'HMMA'.

    IF it_modify1-werks  =  'P001'.
      it_modify1-werks  =  'HVA1'.
    ELSEIF it_modify1-werks  =  'E001'.
      it_modify1-werks  =  'HEA1'.
** furong on 01/17/13
  ELSEIF it_modify1-werks  =  'E002'.
      it_modify1-werks  =  'HEA2'.
** end on 01/17/13
    ENDIF.
    IF it_modify1-flag_gbn IS INITIAL.
      it_modify1-flag_gbn =  'P'.
    ENDIF.

    IF it_modify1-profl IS INITIAL.
      it_modify1-profl  = 'V'.
    ENDIF.

    MOVE-CORRESPONDING it_modify1 TO it_collect.
    it_collect-flag   = 'D'.
*    IF it_modify1-mtart = 'SUBP'.    "Victor 03.27.2012
*      it_collect-flag_gbn = 'W'.
*    ENDIF.



    COLLECT it_collect .
  ENDLOOP.
ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  PRO_BATCH
*&---------------------------------------------------------------------*
FORM pro_batch .

  DATA : v_dest(30) VALUE 'WMHR01'.   "Interface Destination.

  CLEAR : e_return.

  CHECK NOT it_collect[] IS INITIAL.

  CALL FUNCTION 'Z_MM_IF_OB_01_005' DESTINATION v_dest
    IMPORTING
      e_return              = e_return
    TABLES
      t_data                = it_collect
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
    FROM zmmt_vz018
    UP TO 1 ROWS
    WHERE zdate = sy-datum
    ORDER BY zseq DESCENDING.
  ENDSELECT.

  LOOP AT it_collect.
    MOVE-CORRESPONDING it_collect TO it_save.

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

  INSERT zmmt_vz018 FROM TABLE it_save
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
*                             USING   'IT_DATA'.
                             USING   'IT_COLLECT'.

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
      t_outtab                 = it_collect[]
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
  gs_sort-tabname   = 'IT_COLLECT'.
  gs_sort-fieldname = 'SPMON'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

  gs_sort-spos      = '2'.
  gs_sort-tabname   = 'IT_COLLECT'.
  gs_sort-fieldname = 'MATNR'.
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
*        pt_fieldcat-no_out       = 'X'.
      WHEN 'PROFL'.
        pt_fieldcat-seltext_m    = 'LP/KD/MIP.'.
      WHEN 'FLAG'.
        pt_fieldcat-seltext_m    = 'Flag(Ex/Do)'.
      WHEN 'FLAG_GBN'.
        pt_fieldcat-seltext_m    = 'Flag(Module Sub_Part)'.
      WHEN 'MENGE'.
        pt_fieldcat-seltext_m    = 'Quantity(GR)'.
        pt_fieldcat-do_sum       = 'X'.   "Sub Total
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
  CLEAR : s_spmon[], s_mtart[].

  g_repid  = sy-repid.

  CONCATENATE sy-datum+0(6) '01' INTO lv_datum.
  lv_datum  = lv_datum - 1.

  s_spmon-low    = lv_datum+0(6).
  s_spmon-sign   = 'I'.
  s_spmon-option = 'EQ'.
  APPEND s_spmon.

  s_mtart-low    = 'ROH'.
  s_mtart-sign   = 'I'.
  s_mtart-option = 'EQ'.
  APPEND s_mtart.

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
