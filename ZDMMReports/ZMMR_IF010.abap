*&---------------------------------------------------------------------*
*& Report  ZMMR_IF009                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zmmr_if009 MESSAGE-ID 00
        NO STANDARD PAGE HEADING.
************************************************************************
*                       TABLE & VARIANTS                               *
************************************************************************
TYPE-POOLS : slis.

CONTROLS: tc1100 TYPE TABLEVIEW USING SCREEN '1100',
          tc1110 TYPE TABLEVIEW USING SCREEN '1100'.

TABLES: adrp,
        lfa1,
        t001,
        t024,
        t024e,
        t052u,
        t685t,
        tinct,
        usr21,
        ztmm_if004,           " Purchasing Order Log Table
        ztmm_if009,           " Purchase Order Item Log Table
        ztmm_if010,           " Purchase Order Item Detail Log Table
        ztmm_if016.           " Purchase Order Log Message Table

RANGES: ra_zflag FOR ztmm_if004-zflag,
        ra_ztype FOR ztmm_if016-type.

DATA: gt_if004 LIKE ztmm_if004 OCCURS 0 WITH HEADER LINE,
      gt_if016 LIKE ztmm_if016 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF gt_if009 OCCURS 0.
        INCLUDE STRUCTURE ztmm_if009.
DATA: waers    LIKE t001-waers,
      END OF gt_if009.

DATA: BEGIN OF gt_if010 OCCURS 0,
      kschl    LIKE ztmm_if010-kschl,
      vtext    LIKE t685t-vtext,
      kwert    LIKE ztmm_if010-kwert,
      waerk    LIKE ztmm_if010-waerk.
DATA: END OF gt_if010.

DATA: BEGIN OF gt_maind OCCURS 0,
      icon(4),
      ebeln    LIKE ztmm_if004-ebeln,
      erdat    LIKE ztmm_if004-erdat,
      erzet    LIKE ztmm_if004-erzet,
      ntext    LIKE adrp-name_text,
      zseq     LIKE ztmm_if004-zseq,
      bsart    LIKE ztmm_if004-bsart,
      loekz    LIKE ztmm_if004-loekz,
      lifnr    LIKE ztmm_if004-lifnr,
      name1    LIKE lfa1-name1,
      zterm    LIKE ztmm_if004-zterm,
      text1    LIKE t052u-text1,
      ekorg    LIKE ztmm_if004-ekorg,
      ekotx    LIKE t024e-ekotx,
      ekgrp    LIKE ztmm_if004-ekgrp,
      eknam    LIKE t024-eknam,
      bukrs    LIKE ztmm_if004-bukrs,
      butxt    LIKE t001-butxt,
      waers    LIKE ztmm_if004-waers,
      wkurs    LIKE ztmm_if004-wkurs,
      bedat    LIKE ztmm_if004-bedat,
      inco1    LIKE ztmm_if004-inco1,
      bezei    LIKE tinct-bezei,
      inco2    LIKE ztmm_if004-inco2,
      verkf    LIKE ztmm_if004-verkf,
      type     LIKE ztmm_if004-type,
      zflag    LIKE ztmm_if004-zflag,
      zr2pro   LIKE ztmm_if004-zr2pro,
      zredoc   LIKE ztmm_if004-zredoc,
      chkbx.
DATA: END OF gt_maind.

DATA: BEGIN OF gt_ntext OCCURS 0,
      ernam    LIKE ztmm_if004-ernam,
      persnumber  LIKE usr21-persnumber,
      name_text   LIKE adrp-name_text.
DATA: END OF gt_ntext.

DATA: BEGIN OF gt_lifnr OCCURS 0,
      lifnr    LIKE lfa1-lifnr,
      name1    LIKE lfa1-name1.
DATA: END OF gt_lifnr.

DATA: BEGIN OF st_if004,
      icon(4),
      zseq     LIKE ztmm_if004-zseq,
      zflag    LIKE ztmm_if004-zflag.
        INCLUDE STRUCTURE zsmm_if002.
DATA: END OF st_if004.

*... ALV parameter
DATA: g_fieldcat_t        TYPE slis_t_fieldcat_alv,
      g_events_t          TYPE slis_t_event,
      g_keyinfo_s         TYPE slis_keyinfo_alv,
      g_evts_exit_s       TYPE slis_t_event_exit,
      gt_list_top_of_page TYPE slis_t_listheader,
      gt_list_top_of_list TYPE slis_t_listheader,
      g_sort_t            TYPE slis_t_sortinfo_alv,
      g_sort_s            TYPE slis_sortinfo_alv,
      g_layout_s          TYPE slis_layout_alv,
      g_vari              TYPE disvariant,
      gs_gridset          TYPE lvc_s_glay,
      gt_exclude          TYPE slis_t_extab.

DATA: g_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE',
      g_top_of_list  TYPE slis_formname,
      g_status       TYPE slis_formname VALUE 'SET_PF_STATUS',
      g_user_command TYPE slis_formname VALUE 'USER-COMMAND',
      g_data_changed TYPE slis_formname VALUE 'DATA_CHANGED'.

DATA: g_repid LIKE sy-repid,
      g_tabname TYPE slis_tabname,
      pos       TYPE i VALUE 1.

DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
      alv_grid           TYPE REF TO cl_gui_alv_grid.

DATA: g_st_detail_layout TYPE lvc_s_layo,
      g_st_detail_fdcat  TYPE lvc_s_fcat,
      g_it_detail_fdcat  TYPE lvc_t_fcat.

DATA: fieldname(30),
      fieldvalu(10).

DATA : ok_code LIKE sy-ucomm.
************************************************************************
*                        SELECT OPTION                                 *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: sa_ebeln FOR ztmm_if004-ebeln,
                sa_zzseq FOR ztmm_if004-zseq NO-EXTENSION NO INTERVALS,
                sa_matnr FOR ztmm_if009-matnr NO-EXTENSION NO INTERVALS,
                sa_erdat FOR ztmm_if004-erdat.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: pa_rbtn1 RADIOBUTTON GROUP gr1,
            pa_rbtn2 RADIOBUTTON GROUP gr1,
            pa_rbtn3 RADIOBUTTON GROUP gr1,
            pa_rbtn4 RADIOBUTTON GROUP gr1.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS: pa_rbtn5 RADIOBUTTON GROUP gr2,
            pa_rbtn6 RADIOBUTTON GROUP gr2,
            pa_rbtn7 RADIOBUTTON GROUP gr2,
            pa_rbtn8 RADIOBUTTON GROUP gr2.
SELECTION-SCREEN END OF BLOCK b3.

AT SELECTION-SCREEN.
  PERFORM check_select_option.

************************************************************************
*                         MAIN PROGRAM                                 *
************************************************************************
START-OF-SELECTION.
*  SET PF-STATUS 'STANDARD_FULLSCREEN'.
*..1 select data
  PERFORM select_data.
  IF gt_if004[] IS INITIAL.
    MESSAGE s325.
  ENDIF.
*..2 make ALV list data
  PERFORM set_main_data.
*..3 call ALV function
  PERFORM alv_grid_list.

************************************************************************
*                         SUB ROUTINE                                  *
************************************************************************

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM select_data .
  CLEAR: gt_if004, gt_if004[].
*
*  SELECT DISTINCT a~ebeln a~ernam a~erdat a~zseq  a~erzet a~bsart
*         a~loekz a~lifnr a~zterm a~ekorg a~ekgrp a~bukrs
*         a~waers a~wkurs a~bedat a~inco1 a~inco2 a~zflag
*         a~verkf b~type
*    INTO CORRESPONDING FIELDS OF TABLE gt_if004
*    FROM ztmm_if004 AS a JOIN ztmm_if016 AS b
*      ON a~zseq = b~zseq
*      INNER JOIN ztmm_if009 AS c
*      ON a~ebeln = c~ebeln
*      AND a~zseq = c~zseq
*    WHERE a~ebeln IN sa_ebeln
*     AND a~erdat IN sa_erdat
*     AND a~zseq  IN sa_zzseq
*     AND a~zflag IN ra_zflag
*     AND b~type  IN ra_ztype
*     AND c~matnr IN sa_matnr.

  SELECT DISTINCT a~ebeln a~ernam a~erdat a~zseq  a~erzet a~bsart
         a~loekz a~lifnr a~zterm a~ekorg a~ekgrp a~bukrs
         a~waers a~wkurs a~bedat a~inco1 a~inco2 a~zflag
         a~verkf a~zr2pro a~zredoc b~type
    INTO CORRESPONDING FIELDS OF TABLE gt_if004
    FROM ztmm_if004 AS a JOIN ztmm_if016 AS b
      ON a~zseq = b~zseq
      INNER JOIN ztmm_if009 AS c
      ON a~ebeln = c~ebeln
      AND a~zseq = c~zseq
    WHERE a~ebeln IN sa_ebeln
     AND a~erdat IN sa_erdat
     AND a~zseq  IN sa_zzseq
     AND a~zflag IN ra_zflag
     AND b~type  IN ra_ztype
     AND c~matnr IN sa_matnr.
*
  IF pa_rbtn6 = 'X'.                        "success
    DELETE gt_if004 WHERE type = 'E'.
    DELETE gt_if004 WHERE zredoc <> space.
  ELSEIF pa_rbtn7 = 'X'.                    "Error
    DELETE gt_if004 WHERE type = 'S'.
    DELETE gt_if004 WHERE type = 'E'
                    AND zredoc <> space.
  ELSEIF pa_rbtn8 = 'X'.                    "Re-Processing
    DELETE gt_if004 WHERE zredoc = space.
    LOOP AT gt_if004 WHERE type <> 'E'.
      gt_if004-type = 'R'.
      MODIFY gt_if004.
      CLEAR gt_if004.
    ENDLOOP.
  ELSEIF pa_rbtn5 = 'X'.
    LOOP AT gt_if004 WHERE zredoc <> space.
      gt_if004-type = 'R'.
      MODIFY gt_if004.
      CLEAR gt_if004.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_SELECT_OPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_select_option .
  CLEAR: ra_zflag, ra_zflag[], ra_ztype, ra_ztype[].
*
  IF pa_rbtn2 = 'X'.
    ra_zflag-sign = 'I'. ra_zflag-option = 'EQ'.
    ra_zflag-low = 'C'. APPEND ra_zflag.
  ELSEIF pa_rbtn3 = 'X'.
    ra_zflag-sign = 'I'. ra_zflag-option = 'EQ'.
    ra_zflag-low = 'R'. APPEND ra_zflag.
  ELSEIF pa_rbtn4 = 'X'.
    ra_zflag-sign = 'I'. ra_zflag-option = 'EQ'.
    ra_zflag-low = 'D'. APPEND ra_zflag.
  ENDIF.
*
  IF pa_rbtn6 = 'X'.
    ra_ztype-sign = 'I'. ra_ztype-option = 'EQ'.
    ra_ztype-low = 'S'. APPEND ra_ztype.
  ELSEIF pa_rbtn7 = 'X'.
    ra_ztype-sign = 'I'. ra_ztype-option = 'EQ'.
    ra_ztype-low = 'E'. APPEND ra_ztype.
  ENDIF.
ENDFORM.                    " CHECK_SELECT_OPTION
*&---------------------------------------------------------------------*
*&      Form  SET_MAIN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_main_data .
  CLEAR: gt_maind, gt_maind[].
*
  LOOP AT gt_if004.
    MOVE-CORRESPONDING gt_if004 TO gt_maind.
*-- ICON
    CASE gt_maind-type.
      WHEN 'S'.
        MOVE : '@08@' TO gt_maind-icon.
      WHEN 'E'.
        IF gt_maind-zr2pro EQ 'S'.
          MOVE : '@09@' TO gt_maind-icon.
        ELSE.
          MOVE : '@0A@' TO gt_maind-icon.
        ENDIF.
      WHEN 'R'.
        MOVE : '@09@' TO gt_maind-icon.
    ENDCASE.
*-- ICON
    CLEAR gt_ntext.
    READ TABLE gt_ntext WITH KEY ernam = gt_if004-ernam.
    IF sy-subrc = 0.
      gt_maind-ntext = gt_ntext-name_text.
    ELSE.
      CLEAR: usr21, adrp.
      SELECT SINGLE persnumber INTO usr21-persnumber
        FROM usr21 WHERE bname = gt_if004-ernam.
      SELECT SINGLE name_text INTO adrp-name_text
        FROM adrp WHERE persnumber = usr21-persnumber.
      gt_ntext-ernam = gt_if004-ernam.
      gt_ntext-persnumber = usr21-persnumber.
      gt_ntext-name_text  = adrp-name_text.
      gt_maind-ntext      = adrp-name_text.
      APPEND gt_ntext. CLEAR gt_ntext.
    ENDIF.

    CLEAR gt_lifnr.
    READ TABLE gt_lifnr WITH KEY lifnr = gt_if004-lifnr.
    IF sy-subrc = 0.
      gt_maind-name1 = gt_lifnr-name1.
    ELSE.
      CLEAR lfa1.
      SELECT SINGLE name1 INTO lfa1-name1
        FROM lfa1 WHERE lifnr = gt_if004-lifnr.
      gt_lifnr-lifnr = gt_if004-lifnr.
      gt_lifnr-name1 = lfa1-name1.
      gt_maind-name1 = lfa1-name1.
      APPEND gt_lifnr. CLEAR gt_lifnr.
    ENDIF.

    CLEAR t052u.
    SELECT SINGLE text1 INTO t052u-text1
      FROM t052u WHERE spras = sy-langu
                   AND zterm = gt_if004-zterm.
    gt_maind-text1 = t052u-text1.

    CLEAR: t024, t024e.
    SELECT SINGLE ekotx INTO t024e-ekotx
      FROM t024e WHERE ekorg = gt_if004-ekorg.
    gt_maind-ekotx = t024e-ekotx.
    SELECT SINGLE eknam INTO t024-eknam
      FROM t024 WHERE ekgrp = gt_if004-ekgrp.
    gt_maind-eknam = t024-eknam.

    CLEAR t001.
    SELECT SINGLE butxt INTO t001-butxt
      FROM t001 WHERE bukrs = gt_if004-bukrs.
    gt_maind-butxt = t001-butxt.

    CLEAR tinct.
    SELECT SINGLE bezei INTO tinct-bezei
      FROM tinct WHERE spras = sy-langu
                   AND inco1 = gt_if004-inco1.
    gt_maind-bezei = tinct-bezei.
    APPEND gt_maind. CLEAR gt_maind.
  ENDLOOP.
*
*  SORT gt_maind BY erdat erzet.
  SORT gt_maind BY zseq zredoc.

ENDFORM.                    " SET_MAIN_DATA
*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_grid_list .
  CLEAR: g_layout_s, g_vari.
  CLEAR: g_events_t[], g_fieldcat_t[].
  CLEAR: gt_list_top_of_page[].
  CLEAR  g_tabname.
*
  g_repid = sy-repid.
  g_tabname = 'GT_MAIND'.
*.. Field Catalog
  PERFORM fieldcat_build.
*.. Event INFO
  PERFORM eventtab_build.
*.. Layout INFO
  PERFORM layout_build USING g_layout_s.
*  PERFORM SET_FIELD_COLOR.
*.. Sort Catalog
  PERFORM sortcat_build.
*.. ALV Call
  PERFORM call_display_function.
ENDFORM.                    " ALV_GRID_LIST
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fieldcat_build .
  PERFORM reuse_alv USING :
  'ICON'  'Status'            '04' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
  'ZSEQ'  'Seq.No.'           '10' '' 'X' 'R' 'A' 'ALPHA' '' 'GT_MAIND',
  'EBELN' 'PO Doc.No.'        '10' '' 'X' 'C' ' ' ''      '' 'GT_MAIND',
  'ZFLAG' 'Division'          '1'  '' 'X' 'C' ' ' ''      '' 'GT_MAIND',
  'TYPE'   'Type'             '1'  '' 'X' 'C' ' ' ''      '' 'GT_MAIND',
  'ERDAT'  'Created Date'     '10' '' 'X' 'C' ' ' ''      '' 'GT_MAIND',
   'ERZET'  'Created Time'    '8'  '' 'X' 'C' ' ' ''      '' 'GT_MAIND',
   'ZR2PRO' 'ReProcessingInd' '10' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
   'ZREDOC' 'ReProcessingDoc' '10' '' ' ' 'C' 'A' 'ALPHA' '' 'GT_MAIND',
   'BSART'  'Doc. Type'       '4'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
   'LOEKZ'  'Deletion Ind.'   '1'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
   'LIFNR'  'Vendor Code'     '10' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
   'NAME1'  'Vendor Name'     '20' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
   'ZTERM'  'Payment Code'    '4'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
   'TEXT1'  'Payment Name'    '20' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
   'EKOTX'  'Purchasing Org.' '20' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
   'EKNAM'  'Purchasing Grp.' '20' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
   'BUTXT'  'Company'         '20' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
   'WAERS'  'Currency Key'    '5'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
   'WKURS'  'Exchange rage'   '9'  '' ' ' 'R' ' ' ''      '' 'GT_MAIND',
   'BEDAT'  'Doc. date'       '10' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
   'INCO1'  'Incoterms(pt1)'  '3'  '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
   'BEZEI'  'Inco1 descrip.'  '20' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
   'INCO2'  'Incoterms(pt2)'  '20' '' ' ' 'C' ' ' ''      '' 'GT_MAIND',
   'VERKF'  'VAATZ PO Number' '30' '' ' ' 'C' ' ' ''      '' 'GT_MAIND'.
ENDFORM.                    " FIELDCAT_BUILD
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM reuse_alv  USING fieldname
                      fieldtext
                      outputlen
                      key
                      color
                      just
                      ref_fld
                      ref_fld2
                      no_out
                      p_tabname.
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
*
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = fieldname.
  ls_fieldcat-reptext_ddic   = fieldtext.
  ls_fieldcat-outputlen      = outputlen.
  ls_fieldcat-fix_column     = key.
  ls_fieldcat-key            = color.
  ls_fieldcat-just           = just.
  ls_fieldcat-no_out         = no_out.
*
  CASE ref_fld.
    WHEN 'C'. ls_fieldcat-cfieldname = ref_fld2.
    WHEN 'Q'. ls_fieldcat-qfieldname = ref_fld2.
    WHEN 'A'. ls_fieldcat-edit_mask  = '==ALPHA'.
    WHEN 'E'. ls_fieldcat-edit_mask  = '==EXCRT'.
  ENDCASE.
*
  IF fieldname = 'EBELN'.
    ls_fieldcat-hotspot = 'X'.
  ENDIF.
*
  APPEND ls_fieldcat TO g_fieldcat_t.
  CLEAR ls_fieldcat.
  pos = pos + 1.
ENDFORM.                    " REUSE_ALV
*&---------------------------------------------------------------------*
*&      Form  EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM eventtab_build .
  DATA: ls_event  TYPE slis_alv_event.
*
*  LS_EVENT-NAME = 'TOP_OF_PAGE'.
*  LS_EVENT-FORM = 'TOP_OF_PAGE'.
*  APPEND LS_EVENT TO G_EVENTS_T.
*
  ls_event-name = 'USER_COMMAND'.
  ls_event-form = 'USER_COMMAND'.
  APPEND ls_event TO g_events_t.
*
  ls_event-name = 'PF_STATUS_SET'.
  ls_event-form = 'PF_STATUS_SET'.
  APPEND ls_event TO g_events_t.
ENDFORM.                    " EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM layout_build  USING l_layout_s TYPE slis_layout_alv.
  l_layout_s-colwidth_optimize = 'X'.
  l_layout_s-zebra             = 'X'.
  l_layout_s-box_fieldname     = 'CHKBX'.
ENDFORM.                    " LAYOUT_BUILD
*&---------------------------------------------------------------------*
*&      Form  CALL_DISPLAY_FUNCTION
*&---------------------------------------------------------------------*
FORM call_display_function .
  SET SCREEN 0.
  SET PARAMETER ID 'ALVBUFFER' FIELD sy-uzeit.
  gs_gridset-edt_cll_cb = 'X'.
*
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program      = g_repid
            i_callback_user_command = g_user_command
            it_fieldcat             = g_fieldcat_t[]
            it_excluding            = gt_exclude[]
            it_events               = g_events_t[]
            it_sort                 = g_sort_t[]
            is_layout               = g_layout_s
            is_variant              = g_vari
            i_save                  = 'X'
            i_grid_settings         = gs_gridset
       TABLES
            t_outtab                = gt_maind
       EXCEPTIONS
            program_error           = 1
            OTHERS                  = 2.
*
  CASE sy-subrc.
    WHEN '1'.
      MESSAGE e007 WITH 'PROGRAM_ERROR'.
    WHEN '2'.
      MESSAGE e007 WITH 'OTHERS'.
  ENDCASE.
ENDFORM.                    " CALL_DISPLAY_FUNCTION
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
FORM top_of_page.

ENDFORM.                    "top_of_page
*---------------------------------------------------------------------*
*       FORM USER-COMMAND                                             *
*---------------------------------------------------------------------*
FORM user_command  USING  rf_ucomm  LIKE  sy-ucomm
                          rs        TYPE  slis_selfield .
  rs-refresh = 'X'.
  CASE rf_ucomm.
    WHEN 'REFR'.
      PERFORM select_data.
      PERFORM set_main_data.
    WHEN '&IC1'.
      READ TABLE gt_maind INDEX rs-tabindex.
      IF sy-subrc = 0 AND gt_maind-ebeln NE space.
        SET PARAMETER ID 'BES' FIELD gt_maind-ebeln.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.
*    when 'DETA'.
*      read table gt_maind with key chkbx = 'X'.
*      if sy-subrc = 0.
*        perform get_condition_info.
*        perform get_items_info.
*        perform get_log_message.
*        describe table gt_if010 lines tc1100-lines.
*        describe table gt_if009 lines tc1110-lines.
*        call screen 1100.
*      endif.
    WHEN 'REPV'.
      READ TABLE gt_maind WITH KEY chkbx = 'X'.
      IF sy-subrc EQ 0.
        PERFORM get_header_info.
        PERFORM get_condition_info.
        PERFORM get_items_info.
        PERFORM get_log_message.
        DESCRIBE TABLE gt_if010 LINES tc1100-lines.
        DESCRIBE TABLE gt_if009 LINES tc1110-lines.
        CALL SCREEN 1100.
      ENDIF.
  ENDCASE.
ENDFORM.                    "USER-COMMAND
*&---------------------------------------------------------------------*
*&      Form  pf_status_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pf_status_set  USING    p_rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULL'.
ENDFORM.                    " pf_status_set
*&---------------------------------------------------------------------*
*&      Form  GET_CONDITION_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_condition_info .
  CLEAR: gt_if010, gt_if010[].
*
  SELECT kschl kwert waerk
    INTO CORRESPONDING FIELDS OF TABLE gt_if010
    FROM ztmm_if010 WHERE zseq = gt_maind-zseq.
*
  LOOP AT gt_if010.
    CLEAR t685t.
    SELECT SINGLE vtext INTO t685t-vtext
      FROM t685t WHERE spras = sy-langu
                   AND kvewe = 'A'
                   AND kappl = 'M'
                   AND kschl = gt_if010-kschl.
    gt_if010-vtext = t685t-vtext.
    MODIFY gt_if010. CLEAR gt_if010.
  ENDLOOP.
ENDFORM.                    " GET_CONDITION_INFO
*&---------------------------------------------------------------------*
*&      Form  GET_ITEMS_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_items_info .
  CLEAR: gt_if009, gt_if009[].
*
  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_if009
    FROM ztmm_if009 WHERE zseq = gt_maind-zseq.
*
  gt_if009-waers = gt_maind-waers.
  MODIFY gt_if009 TRANSPORTING waers WHERE waers = space.
ENDFORM.                    " GET_ITEMS_INFO
*&---------------------------------------------------------------------*
*&      Form  GET_LOG_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_log_message .
  CLEAR: gt_if016, gt_if016[].
*
  SELECT type message INTO CORRESPONDING FIELDS OF TABLE gt_if016
    FROM ztmm_if016 WHERE zseq = gt_maind-zseq
                      AND type IN ra_ztype.
ENDFORM.                    " GET_LOG_MESSAGE
*&---------------------------------------------------------------------*
*&      Module  STATUS_1100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1100 OUTPUT.
*  SET PF-STATUS 'PS1100'.
*  SET TITLEBAR '110'.
  IF gt_maind-icon NE '@0A@'.
    SET PF-STATUS 'PS1100' EXCLUDING 'REP'.
  ELSE.
    SET PF-STATUS 'PS1100'.
  ENDIF.
  SET TITLEBAR '110'.
ENDMODULE.                 " STATUS_1100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE back_exit INPUT.
  SET SCREEN 0. LEAVE SCREEN.
ENDMODULE.                 " BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_object OUTPUT.
  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING container_name = 'CC_MSG'.
    CREATE OBJECT alv_grid
      EXPORTING i_parent = g_custom_container.
  ENDIF.
ENDMODULE.                 " CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_grid OUTPUT.
  PERFORM detail_layout.
  PERFORM detail_field_cat.
*
  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      is_layout       = g_st_detail_layout
    CHANGING
      it_fieldcatalog = g_it_detail_fdcat
      it_outtab       = gt_if016[].

ENDMODULE.                 " ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  DETAIL_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM detail_layout .
  CLEAR g_st_detail_layout.
  g_st_detail_layout-cwidth_opt = 'X'.
  g_st_detail_layout-zebra      = 'X'.
ENDFORM.                    " DETAIL_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  DETAIL_FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM detail_field_cat .
  REFRESH g_it_detail_fdcat.
*
  g_st_detail_fdcat-fieldname = 'TYPE'.
  g_st_detail_fdcat-coltext   = 'Type'.
  APPEND g_st_detail_fdcat TO g_it_detail_fdcat.
*
  g_st_detail_fdcat-fieldname = 'MESSAGE'.
  g_st_detail_fdcat-coltext   = 'Message'.
  APPEND g_st_detail_fdcat TO g_it_detail_fdcat.
ENDFORM.                    " DETAIL_FIELD_CAT
*&---------------------------------------------------------------------*
*&      Form  get_header_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_header_info.
  CLEAR st_if004.
  MOVE-CORRESPONDING gt_maind TO st_if004.
  st_if004-cuky = gt_maind-waers.
ENDFORM.                    " get_header_info
*&---------------------------------------------------------------------*
*&      Module  screen_control  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_control OUTPUT.
  CHECK st_if004-icon EQ '@0A@'.  " error
*  CHECK st_if004-zflag NE 'D'.    " delete

  LOOP AT SCREEN.
    screen-input = 1.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " screen_control  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_1100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1100 INPUT.
  DATA : answer,
         save_okcode LIKE ok_code.

  MOVE : ok_code TO save_okcode.

  CLEAR : ok_code.

  CASE save_okcode.
    WHEN 'REP'.
      CLEAR : save_okcode.
      PERFORM popup_to_confirm USING    text-p01
                                        text-p02
                               CHANGING answer.
      CHECK answer = 'J'.
*--ABAP Memory
DATA L_FLAG.
  L_FLAG = 'X'.
  EXPORT L_FLAG TO MEMORY ID 'FLAG'.
*----------------------------------------------
      PERFORM re_process_po.
  ENDCASE.

ENDMODULE.                 " user_command_1100  INPUT
*&---------------------------------------------------------------------*
*&      Form  popup_to_confirm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_P01  text
*      -->P_TEXT_P02  text
*      <--P_ANSWER  text
*----------------------------------------------------------------------*
FORM popup_to_confirm USING    p_text_p01
                               p_text_p02
                      CHANGING p_answer.
*----------------------------------------------------------------------*
*  MESSAGE POPUP
*----------------------------------------------------------------------*
  DATA: BEGIN OF pop,
        titel     LIKE spop-titel,
        diagnose1 LIKE spop-diagnose1,
        diagnose2 LIKE spop-diagnose2,
        diagnose3 LIKE spop-diagnose3,
        textline1 LIKE spop-textline1,
        textline2 LIKE spop-textline2,
        textline3 LIKE spop-textline3,
        option1   LIKE spop-varoption1,
        option2   LIKE spop-varoption2,
        default,
        answer,
        END OF pop.

  DATA: cancel_display.

  MOVE: p_text_p01 TO pop-textline1,
        p_text_p02 TO pop-titel.

  CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
    EXPORTING
*     DEFAULTOPTION        = 'Y'
      diagnosetext1        = pop-diagnose1
*     DIAGNOSETEXT2        = ' '
*     DIAGNOSETEXT3        = ' '
      textline1            = pop-textline1
      textline2            = pop-textline2
      titel                = pop-titel
*     START_COLUMN         = 25
*     START_ROW            = 6
      cancel_display       = cancel_display
    IMPORTING
      answer               = pop-answer
    EXCEPTIONS
      othsers              = 1.

  p_answer = pop-answer.

ENDFORM.                    " popup_to_confirm
*&---------------------------------------------------------------------*
*&      Form  re_process_po
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM re_process_po.
  DATA: wa_poheader LIKE zsmm_if002.
  DATA: it_poitem LIKE zsmm_if009 OCCURS 0 WITH HEADER LINE.
  DATA: it_condition LIKE zsmm_if010 OCCURS 0 WITH HEADER LINE.
  DATA: it_return LIKE bapireturn OCCURS 0 WITH HEADER LINE.
  DATA: it_service LIKE zsmm_if011 OCCURS 0 WITH HEADER LINE.
  DATA: l_check LIKE st_if004-zflag.
  DATA: v_num LIKE ztmm_if004-zseq.

  CLEAR: v_num.
  CLEAR: wa_poheader, l_check, it_poitem,
         it_condition, it_return, it_service.

  REFRESH: it_poitem, it_condition, it_return, it_service.

  MOVE-CORRESPONDING st_if004 TO wa_poheader.
  MOVE: st_if004-zflag TO l_check.

  LOOP AT gt_if009.
    MOVE-CORRESPONDING gt_if009 TO it_poitem.
    APPEND it_poitem.
    CLEAR: gt_if009, it_poitem.
  ENDLOOP.

  LOOP AT gt_if010.
    MOVE-CORRESPONDING gt_if010 TO it_condition.
    APPEND it_condition.
    CLEAR: gt_if010, it_condition.
  ENDLOOP.

  CALL FUNCTION 'ZMMF_IF_PO'
       EXPORTING
            i_zsmm_if002 = wa_poheader
            i_check      = l_check
       TABLES
            t_item       = it_poitem
            t_condition  = it_condition
            t_service    = it_service
            e_return     = it_return.

  IF sy-subrc = 0.
    SELECT MAX( zseq ) INTO v_num
    FROM ztmm_if004
    WHERE ebeln = wa_poheader-ebeln.

    READ TABLE it_return WITH KEY type = 'S'.
    IF sy-subrc = 0.
      UPDATE ztmm_if004 SET zr2pro = 'S'
                            zredoc = v_num
         WHERE zseq = st_if004-zseq.
      COMMIT WORK AND WAIT.

      UPDATE ztmm_if004 SET zredoc = st_if004-zseq
                            type = 'R'
         WHERE zseq = v_num.
      COMMIT WORK AND WAIT.
      MESSAGE i028(zmmm).
    ELSE.
      DELETE FROM ztmm_if004 WHERE zseq = v_num.
      DELETE FROM ztmm_if009 WHERE zseq = v_num.
      DELETE FROM ztmm_if010 WHERE zseq = v_num.
      COMMIT WORK AND WAIT.
      DATA : it_xmsg LIKE ztismessage OCCURS 0 WITH HEADER LINE.
      LOOP AT it_return.
        MOVE: it_return-type    TO it_xmsg-msgty,
              it_return-message TO it_xmsg-msgtx.
        APPEND it_xmsg.
        CLEAR it_return.
      ENDLOOP.
*      MESSAGE i000(zmmm) WITH 'Error!!'.
      CALL FUNCTION 'ZMM_IF_POPUP_TO_ERROR_MESSAGE'
           EXPORTING
                xdocno_show = 'X'
           TABLES
                xmsg        = it_xmsg.
    ENDIF.
  ENDIF.
  PERFORM select_data.
  PERFORM set_main_data.
  LEAVE TO SCREEN 0.
ENDFORM.                    " re_process_po
*&---------------------------------------------------------------------*
*&      Module  read_condition  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE read_condition INPUT.
  MODIFY gt_if010 INDEX tc1100-current_line.
ENDMODULE.                 " read_condition  INPUT
*&---------------------------------------------------------------------*
*&      Module  read_item  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE read_item INPUT.
  MODIFY gt_if009 INDEX tc1110-current_line.
ENDMODULE.                 " read_item  INPUT

*&---------------------------------------------------------------------*
*&      Form  sortcat_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sortcat_build.
*---
  DATA : l_sort LIKE LINE OF g_sort_t.

  CLEAR : l_sort, g_sort_t[].

  l_sort-spos = 1.
  l_sort-fieldname = 'EBELN'.
  l_sort-up = 'X'.

  APPEND l_sort TO g_sort_t.
ENDFORM.                    " sortcat_build
