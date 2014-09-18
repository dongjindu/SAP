*&---------------------------------------------------------------------*
*&  Include           ZTRR01200_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECT_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form SELECT_ORDER .

  CLEAR : it_aufk, it_aufk[], it_coas, it_coas[],
          r_aufnr, r_aufnr[].

*// === Order master data === //*
  SELECT aufnr auart autyp ktext
         INTO CORRESPONDING FIELDS OF TABLE it_aufk
    FROM aufk
   WHERE aufnr IN s_aufnr
     and bukrs  = p_bukrs.

*// === Order Master for Controlling(all Fields in AUFK Tbl) === //*
  if it_aufk[] is not initial.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_coas
      FROM coas for all entries in it_aufk
     WHERE aufnr = it_aufk-aufnr
       AND auart = it_aufk-auart
       and bukrs = p_bukrs.

  endif.

  check  s_aufnr[] is initial.

  loop at it_aufk.
    make_ranges: it_aufk-aufnr  r_aufnr.
  endloop.


endform.                    " SELECT_ORDER
*&---------------------------------------------------------------------*
*&      Form  SELECT_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form SELECT_TEXT .

  clear: gt_t087j, gt_t087j[], gt_imzo, gt_imzo[], gt_impr, gt_impr[],
         gt_impu, gt_impu[], gt_t035t, gt_t035t[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_t087j
    FROM t087j
   WHERE spras = sy-langu.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_imzo
    FROM imzo for all entries in it_coas
   WHERE objnr = it_coas-objnr.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_impr
    FROM impr for all entries in it_coas
   WHERE objnr = it_coas-objnr.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_impu
    FROM impu for all entries in gt_imzo
   WHERE posnr = gt_imzo-posnr.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_t035t
    FROM t035t
   WHERE spras = sy-langu.

endform.                    " SELECT_TEXT

*---------------------------------------------------------------------*
*      Form  select_data
*---------------------------------------------------------------------*
FORM select_data.

  CLEAR : it_list[], it_list, it_zttr0009, it_zttr0009[],
          it_ztim9000, it_ztim9000[].


  if s_aufnr[] is initial.

*// === [TR-CM] I/O Planning Group === //*
    SELECT * INTO TABLE it_zttr0009
      FROM zttr0009
     WHERE aufnr IN r_aufnr.

*// === [IM]Invest. category & Std.items for P_IO === //*
    SELECT * INTO TABLE it_ztim9000
      FROM ztim9000
     WHERE aufnr IN r_aufnr.

  else.

*// === [TR-CM] I/O Planning Group === //*
    SELECT * INTO TABLE it_zttr0009
      FROM zttr0009
     WHERE aufnr IN s_aufnr.

*// === [IM]Invest. category & Std.items for P_IO === //*
    SELECT * INTO TABLE it_ztim9000
      FROM ztim9000
     WHERE aufnr IN s_aufnr.

  endif.


  LOOP AT it_coas.
    MOVE-CORRESPONDING it_coas TO it_list.

    perform find_text using     it_coas-izwek
                                it_coas-objnr
                      changing  it_list-txt50
                                it_list-gjahr
                                it_list-posid
                                it_list-prnam
                                it_list-post1.


    CLEAR it_zttr0009.
    READ TABLE it_zttr0009 WITH KEY aufnr = it_list-aufnr.

    it_list-fdgrv = it_zttr0009-fdgrv.

    perform find_text2 using     it_zttr0009-fdgrv
                       changing  it_list-textl.


    it_list-count = 1.

    IF it_list-posid = '' OR it_list-izwek = ''
       OR it_list-fdgrv = ''.
      it_list-icon = gc_led_red.
    ELSE.
      it_list-icon = gc_led_green.
    ENDIF.

    IF it_list-icon = gc_led_green.
      IF it_list-izwek(1) <> it_list-fdgrv+4(1).
        it_list-icon = gc_led_red.
      ENDIF.
    ENDIF.

    APPEND it_list. CLEAR it_list.

  ENDLOOP.

  SORT it_list BY icon posid izwek aufnr.

ENDFORM.                    " select_data
*---------------------------------------------------------------------*
*      Form  DISPLAY_LIST
*---------------------------------------------------------------------*
FORM display_list.

  g_repid = sy-repid.
  alv_layout-zebra             = char_x.
  alv_layout-colwidth_optimize = 'X'.

  PERFORM alv_field_catalog.
  PERFORM alv_events.
  PERFORM alv_sort.
  PERFORM alv_comment.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = g_repid
      i_callback_pf_status_set = 'STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      it_sort                  = alv_sort[]
      is_layout                = alv_layout
      it_fieldcat              = alv_fieldcat[]
      i_save                   = 'A'
      it_events                = alv_event[]
    TABLES
      t_outtab                 = it_list.

ENDFORM.                    " write_data
*---------------------------------------------------------------------*
*      Form  build_field_catalog
*---------------------------------------------------------------------*
FORM alv_field_catalog.

  DATA: ls_fieldcat  TYPE  slis_fieldcat_alv.
  DATA: lt_fieldcat  TYPE  slis_t_fieldcat_alv.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = g_repid
      i_internal_tabname = 'IT_LIST'
      i_inclname         = g_repid
    CHANGING
      ct_fieldcat        = alv_fieldcat[].

  LOOP AT alv_fieldcat INTO ls_fieldcat.

    CASE ls_fieldcat-fieldname.
      WHEN 'AUFNR'. ls_fieldcat-hotspot   = 'X'.
      WHEN 'FDGRV'. ls_fieldcat-edit      = 'X'.
      WHEN 'COUNT'. ls_fieldcat-do_sum    = 'X'.
      WHEN 'IZWEK'. ls_fieldcat-emphasize = 'C500'.
      WHEN 'POSID'. ls_fieldcat-emphasize = 'C410'.
    ENDCASE.

    MODIFY alv_fieldcat FROM ls_fieldcat.
  ENDLOOP.
ENDFORM.                    " build_field_catalog
*---------------------------------------------------------------------*
*      Form  build_events
*---------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM alv_events.

*  DATA ll_events TYPE slis_alv_event.
*
*  CLEAR alv_event[].
*
*  ll_events-name   =  'TOP_OF_PAGE'.
*  ll_events-form   =  'TOP_OF_PAGE'.
*  APPEND ll_events TO alv_event.

ENDFORM.                    " build_events
*---------------------------------------------------------------------*
*      Form  build_comment
*---------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM alv_comment.

ENDFORM.                    " build_comment
*---------------------------------------------------------------------*
*      Form  TOP_OF_PAGE
*---------------------------------------------------------------------*

FORM top_of_page.
*
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
*      i_logo             = 'ENJOY_SAP'
*      it_list_commentary = top_of_page.

ENDFORM.                    " TOP_OF_PAGE
*---------------------------------------------------------------------*
*      Form  user_command
*---------------------------------------------------------------------*
FORM user_command   USING       r_ucomm     LIKE  sy-ucomm
                                rs_selfield TYPE  slis_selfield.

  CASE r_ucomm.

    WHEN '&IC1'.
      READ TABLE it_list INDEX rs_selfield-tabindex.

      CASE rs_selfield-sel_tab_field.      "
        WHEN 'IT_LIST-AUFNR'.
          SET PARAMETER ID : 'ANR' FIELD it_list-aufnr.

          CALL TRANSACTION 'KO03' AND SKIP FIRST SCREEN .
      ENDCASE.

    WHEN 'KO02'.
      READ TABLE it_list INDEX rs_selfield-tabindex.

      SET PARAMETER ID 'ANR' FIELD it_list-aufnr.
      CALL TRANSACTION 'KO02' AND SKIP FIRST SCREEN.

    WHEN '&DATA_SAVE'.
      LOOP AT it_list.
*// === [TR-CM] I/O Planning Group
        READ TABLE it_zttr0009 WITH KEY aufnr = it_list-aufnr.

        IF sy-subrc = 0.
          CHECK it_list-fdgrv <> it_zttr0009-fdgrv.

          UPDATE zttr0009
          SET   fdgrv = it_list-fdgrv
                 aenam = sy-uname
                 aedat = sy-datum
                 aezet = sy-uzeit
          WHERE  aufnr = it_list-aufnr.

        ELSE.
          zttr0009-aufnr = it_list-aufnr.
          zttr0009-fdgrv = it_list-fdgrv.
          zttr0009-ernam = sy-uname.
          zttr0009-erdat = sy-datum.
          zttr0009-erzet = sy-uzeit.

          INSERT zttr0009.
        ENDIF.
      ENDLOOP.

      IF sy-subrc = 0.
        COMMIT WORK.
        MESSAGE s007.   "Saved successfully
      ENDIF.

    WHEN 'REFR'.
      PERFORM select_data.
  ENDCASE.
  rs_selfield-refresh    = 'X'.

ENDFORM.                    " user_command
*---------------------------------------------------------------------*
*      Form  status
*---------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM status   USING extab TYPE slis_t_extab.

  SET PF-STATUS 'STANDARD'.  " EXCLUDING tab.

ENDFORM.                    " status
*---------------------------------------------------------------------*
*      Form  alv_sort
*---------------------------------------------------------------------*
FORM alv_sort.
  DATA ls_sort TYPE slis_t_sortinfo_alv WITH HEADER LINE.

  CLEAR alv_sort[].

  ls_sort-spos      = 1.

  ls_sort-fieldname = 'ICON'.      "
  ls_sort-up        = 'X'.
  ls_sort-subtot    = 'X'.
  ls_sort-comp      = 'X'.

  APPEND ls_sort TO alv_sort.

  ls_sort-spos      = 2.

  ls_sort-fieldname = 'POSID'.      "
  ls_sort-up        = 'X'.
  ls_sort-subtot    = 'X'.
  ls_sort-comp      = 'X'.

  APPEND ls_sort TO alv_sort.
ENDFORM.                    " alv_sort
*&---------------------------------------------------------------------*
*&      Form  INIT_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form INIT_SCREEN .

  loop at screen.
    if screen-name = 'P_BUTXT'.
      screen-input  = 0.
      screen-intensified = '0'.
      screen-display_3d  = '0'.
      modify screen.
    endif.
    if screen-name = 'P_BUKRS'.
      screen-input = ' '.
      modify screen.
    endif.
  endloop.


* & find text.
  perform fi_wt_read_t001 using    p_bukrs
                          changing p_butxt.


endform.                    " INIT_SCREEN
*&---------------------------------------------------------------------*
*&      Form  FI_WT_READ_T001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BUKRS  text
*      <--P_P_BUTXT  text
*----------------------------------------------------------------------*
form FI_WT_READ_T001  using    pa_bukrs
                      changing pa_butxt.

  data : it_t001 like t001.

  call function 'FI_WT_READ_T001'
    exporting
      i_bukrs   = pa_bukrs
    importing
      t_t001    = it_t001
    exceptions
      not_found = 1.

  case sy-subrc.
    when 0.
      pa_butxt = it_t001-butxt.
    when 1.
      message s101(f5).
    when others.
  endcase.


endform.                    " FI_WT_READ_T001
*&---------------------------------------------------------------------*
*&      Form  FIND_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_COAS_IZWEK  text
*      -->P_IT_COAS_OBJNR  text
*      <--P_IT_LIST_TXT50  text
*      <--P_IT_LIST_GJAHR  text
*      <--P_IT_LIST_POSID  text
*      <--P_IT_LIST_PRNAM  text
*      <--P_IT_LIST_POST1  text
*----------------------------------------------------------------------*
form FIND_TEXT  using    pa_izwek
                         pa_objnr
                changing pa_txt50
                         pa_gjahr
                         pa_posid
                         pa_prnam
                         pa_post1.


  clear: gt_t087j, gt_imzo, gt_impr, gt_impu, gt_t035t.

*
  READ TABLE gt_t087j with key izwek = pa_izwek.
  if sy-subrc = 0.
    pa_txt50 = gt_t087j-txt50.
  endif.
*
  READ TABLE gt_imzo with key objnr = pa_objnr.
  if sy-subrc = 0.
    pa_gjahr = gt_imzo-gjahr.
  endif.
*
  READ TABLE gt_impr with key posnr = gt_imzo-posnr.
  if sy-subrc = 0.
    pa_posid = gt_impr-posid.
    pa_prnam = gt_impr-prnam.
  endif.

*
  READ TABLE gt_impu with key posnr = gt_imzo-posnr.
  if sy-subrc = 0.
    pa_post1 = gt_impu-post1.
  endif.


endform.                    " FIND_TEXT
*&---------------------------------------------------------------------*
*&      Form  FIND_TEXT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZTTR0009_FDGRV  text
*      <--P_IT_LIST_TEXTL  text
*----------------------------------------------------------------------*
form FIND_TEXT2  using    pa_fdgrv
                 changing pa_textl.

  READ TABLE gt_t035t with key grupp = pa_fdgrv.
  if sy-subrc = 0.
    pa_textl = gt_t035t-textl.
  endif.

endform.                    " FIND_TEXT2
