REPORT zrmmpm28r_undetermin_price_tmp NO STANDARD PAGE HEADING
                                      LINE-SIZE 255.
*----- Type
TYPE-POOLS : slis, sp01r.

TABLES: ekko,
        t024,
        t024e,
        t134t.

*----- Internal Tables
DATA: BEGIN OF it_price_info OCCURS 0,
        vakey LIKE konh-vakey,
        datab LIKE konh-datab,
        datbi LIKE konh-datbi,
        lifnr LIKE lfa1-lifnr,
        matnr LIKE mara-matnr,
        maktx LIKE makt-maktx,
        ekorg LIKE ekko-ekorg,
        ekgrp LIKE ekko-ekgrp,
        eknam LIKE t024-eknam,
        kzust LIKE konh-kzust,
        kschl LIKE konh-kschl,
        kbetr LIKE konp-kbetr,
        kpein LIKE konp-kpein,
        kmein LIKE konp-kmein,
        waers LIKE ekko-waers,
        name1 LIKE lfa1-name1,
      END   OF it_price_info.

DATA: BEGIN OF it_display OCCURS 0,
        ekorg    LIKE ekko-ekorg,
        ekgrp    LIKE ekko-ekgrp,
        eknam    LIKE t024-eknam,
        1mcnt TYPE i,
        2mcnt TYPE i,
        3mcnt TYPE i,
        sum   TYPE i,
      END   OF it_display.

DATA: it_detail LIKE it_price_info OCCURS 0 WITH HEADER LINE.

*----- Working area
DATA: wa_master LIKE it_price_info.

DATA: wa_progress_idx TYPE   i,                "Progress bar index
      wa_ekgrp_f      LIKE   ekko-ekgrp,       "Purchase group From
      wa_ekgrp_t      LIKE   ekko-ekgrp,       "Purchase group To
      wa_vakey        LIKE   konh-vakey,       "Value Key
      wa_knumh        LIKE   konh-knumh,       "Condition record No.
      wa_matnr        LIKE   mara-matnr,       "Material
      wa_count        TYPE   i,                "Count of Material
      wa_datab        TYPE   d.                "Valid from

*----- Define variable for ALV
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_top_of_page_detail TYPE slis_t_listheader,
       w_status_flg(6).
*----- Constants
CONSTANTS : c_formname_top_of_page TYPE slis_formname
                                        VALUE 'TOP_OF_PAGE'.

*----- Macro
DEFINE append_fieldcat.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : cur field      &8 : no zero          &9 : just

  &1 = &1 + 1.
  w_fieldcat-col_pos    = &1.
  w_fieldcat-fieldname  = &2.
  w_fieldcat-outputlen  = &3.
  w_fieldcat-seltext_l  = &4.
  w_fieldcat-seltext_m  = &4.
  w_fieldcat-seltext_s  = &4.
  w_fieldcat-datatype   = &5.
  w_fieldcat-key        = &6.
  w_fieldcat-cfieldname = &7.
  w_fieldcat-no_zero    = &8.
  w_fieldcat-just       = &9.
  append w_fieldcat.
  clear : w_fieldcat.

END-OF-DEFINITION.

*----- Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS:     p_ekorg LIKE ekko-ekorg  DEFAULT 'PU01'    OBLIGATORY.
SELECT-OPTIONS: s_ekgrp FOR ekko-ekgrp NO-EXTENSION.
PARAMETERS:     p_mtart LIKE mara-mtart  DEFAULT 'ROH'     OBLIGATORY.
PARAMETERS:     p_datum LIKE sy-datum    DEFAULT sy-datum  OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK bl1.

*----- Initialization
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].

*----- At selection-screen
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_rtn.
  PERFORM get_data.

*----- Top-of-page
TOP-OF-PAGE.
  PERFORM top_of_page.

*----- Start of selection
START-OF-SELECTION.
  PERFORM calculate_data.
  PERFORM display_data.
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: wa_matnr LIKE mara-matnr. "Material


  EXEC SQL PERFORMING APPEND_IT_PRICE_INFO.
    SELECT A.MATNR, D.MAKTX, B.LIFNR, C.EKORG, C.EKGRP,
           C.WAERS, E.EKNAM, F.NAME1
      INTO :WA_MASTER-MATNR, :WA_MASTER-MAKTX, :WA_MASTER-LIFNR,
           :WA_MASTER-EKORG, :WA_MASTER-EKGRP, :WA_MASTER-WAERS,
           :WA_MASTER-EKNAM, :WA_MASTER-NAME1
      FROM MARA A, EINA B, EINE C, MAKT D, T024 E, LFA1 F
     WHERE A.MANDT = :SY-MANDT
       AND A.MTART = :P_MTART
       AND A.LVORM = ' '
       AND B.MANDT = A.MANDT
       AND B.MATNR = A.MATNR
       AND B.LOEKZ = ' '
       AND C.MANDT = B.MANDT
       AND C.INFNR = B.INFNR
       AND C.EKORG = :P_EKORG
       AND C.ESOKZ = '0'
       AND C.WERKS = ' '
       AND C.LOEKZ = ' '
       AND C.EKGRP BETWEEN :WA_EKGRP_F AND :WA_EKGRP_T
       AND D.MANDT = B.MANDT
       AND D.MATNR = B.MATNR
       AND D.SPRAS = :SY-LANGU
       AND E.MANDT = C.MANDT
       AND E.EKGRP = C.EKGRP
       AND F.MANDT(+) = B.MANDT
       AND F.LIFNR(+) = B.LIFNR
  ENDEXEC.

  READ TABLE it_price_info INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  APPEND_IT_PRICE_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_it_price_info.
*  PERFORM display_progress_bar.

  CLEAR: wa_vakey.

  MOVE: wa_master-lifnr TO wa_vakey,
        wa_master-matnr TO wa_vakey+10,
        wa_master-ekorg TO wa_vakey+28,
        '0'             TO wa_vakey+32.

*----- Read the lastest Value key
  CLEAR: wa_datab.
  SELECT SINGLE knumh datab
    INTO (wa_knumh, wa_datab)
    FROM a018
   WHERE kappl = 'M'
     AND kschl = 'PB00'
     AND lifnr = wa_master-lifnr
     AND matnr = wa_master-matnr
     AND ekorg = wa_master-ekorg
     AND esokz = '0'
     AND datab <= p_datum
     AND datbi >=  p_datum.


  CHECK NOT wa_datab IS INITIAL.

  SELECT *
         APPENDING CORRESPONDING FIELDS OF TABLE it_price_info
    FROM konh AS a INNER JOIN konp AS b
      ON a~knumh    = b~knumh
   WHERE a~knumh    =    wa_knumh
     AND a~datab    =    wa_datab
     AND a~kzust    LIKE 'X%'
     AND a~kschl    =    'PB00'
     AND b~kschl    =    'PB00'
     AND b~loevm_ko =    ' '.

  CHECK sy-subrc EQ 0.

  MOVE: wa_master-lifnr TO it_price_info-lifnr,
        wa_master-matnr TO it_price_info-matnr,
        wa_master-maktx TO it_price_info-maktx,
        wa_master-ekorg TO it_price_info-ekorg,
        wa_master-ekgrp TO it_price_info-ekgrp,
        wa_master-eknam TO it_price_info-eknam,
        wa_master-waers TO it_price_info-waers,
        wa_master-name1 TO it_price_info-name1.

  MODIFY it_price_info
         TRANSPORTING lifnr matnr maktx ekorg ekgrp eknam waers name1
   WHERE vakey = wa_vakey.

  CLEAR: wa_master.
ENDFORM.                    " APPEND_IT_PRICE_INFO
*&---------------------------------------------------------------------*
*&      Form  display_progress_bar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_progress_bar.
  DATA: lw_percentage(3) TYPE c,
        lw_mod TYPE i,
        lw_text(50).

  wa_progress_idx = wa_progress_idx + 1.

  lw_percentage = wa_progress_idx / wa_count * 100.

  CONCATENATE text-b01 lw_percentage '%' INTO lw_text.

  lw_mod = lw_percentage MOD 6.

  CHECK lw_mod EQ 5.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = lw_percentage
            text       = lw_text.
ENDFORM.                    " display_progress_bar
*&---------------------------------------------------------------------*
*&      Form  calculate_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_data.
  DATA: lw_day TYPE i.

  LOOP AT it_price_info.
    READ TABLE it_display WITH KEY ekorg = it_price_info-ekorg
                                   ekgrp = it_price_info-ekgrp.
    IF sy-subrc EQ 0.
      lw_day = sy-datum - it_price_info-datab.
      IF     lw_day <= 30.
        it_display-1mcnt = it_display-1mcnt + 1.
      ELSEIF lw_day > 30 AND lw_day <= 60.
        it_display-2mcnt = it_display-2mcnt + 1.
      ELSEIF lw_day > 60.
        it_display-3mcnt = it_display-3mcnt + 1.
      ENDIF.

      it_display-sum = it_display-sum + 1.

      MODIFY it_display INDEX sy-tabix.
    ELSE.
      CLEAR: it_display.

      MOVE: it_price_info-ekorg TO it_display-ekorg,
            it_price_info-ekgrp TO it_display-ekgrp,
            it_price_info-eknam TO it_display-eknam.

      lw_day = sy-datum - it_price_info-datab.
      IF     lw_day <= 30.
        it_display-1mcnt = it_display-1mcnt + 1.
      ELSEIF lw_day > 30 AND lw_day <= 60.
        it_display-2mcnt = it_display-2mcnt + 1.
      ELSEIF lw_day > 60.
        it_display-3mcnt = it_display-3mcnt + 1.
      ENDIF.

      it_display-sum = it_display-sum + 1.

      APPEND it_display.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " calculate_data
*&---------------------------------------------------------------------*
*&      Form  event_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_EVENTCAT[]  text
*----------------------------------------------------------------------*
FORM event_build USING p_w_eventcat TYPE slis_t_event.
  DATA : l_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = p_w_eventcat.

  READ TABLE p_w_eventcat WITH KEY name = slis_ev_top_of_page
                          INTO l_event.

  IF sy-subrc EQ 0.
    MOVE c_formname_top_of_page TO l_event-form.
    APPEND l_event TO p_w_eventcat.
  ENDIF.
ENDFORM.                    " event_build
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_of_page.
  CASE   w_status_flg.
    WHEN 'BASE'.
      CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
           EXPORTING
                it_list_commentary = w_top_of_page.
    WHEN OTHERS.
      CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
           EXPORTING
                it_list_commentary = w_top_of_page_detail.
  ENDCASE.
ENDFORM.                    " top_of_page
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  PERFORM build_fieldcat.
  PERFORM build_event.
  PERFORM build_sort.
  PERFORM comment_build USING  w_top_of_page[].
  PERFORM alv_function.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : cur field      &8 : no zero          &9 : just

  append_fieldcat :
    w_col_pos 'EKGRP' 10 text-002 'CHAR' 'X' ''      '' '',
    w_col_pos 'EKNAM' 10 text-003 'CHAR' 'X' ''      '' '',
    w_col_pos '1MCNT' 20 text-004 'INT4'  ''  ''      '' '',
    w_col_pos '2MCNT' 20 text-005 'INT4'  ''  ''      '' '',
    w_col_pos '3MCNT' 20 text-006 'INT4'  ''  ''      '' '',
    w_col_pos 'SUM'   20 text-007 'INT4'  ''  ''      '' ''.
ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  build_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_event.
  w_eventcat-name = 'TOP_OF_PAGE'.
  w_eventcat-form = 'TOP_OF_PAGE'.

  APPEND w_eventcat.
ENDFORM.                    " build_event
*&---------------------------------------------------------------------*
*&      Form  build_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sort.
  w_sortcat-spos           = 1.
  w_sortcat-fieldname      = 'EKGRP'.
  w_sortcat-tabname        = 'IT_DISPLAY'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.
ENDFORM.                    " build_sort
*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM comment_build USING  lt_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
        l_manager(50),
        l_date(50),
        l_list(50),
        l_dsnam LIKE t024d-dsnam,
        l_h_dsnam LIKE t024d-dsnam,
        l_ldate(10),
        l_hdate(10).

*----- Title
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-h01.
  APPEND ls_line TO lt_top_of_page.

*----- Purchase Org.
  ls_line-typ  = 'S'.
  ls_line-key  = text-h02.
  CONCATENATE p_ekorg t024e-ekotx INTO ls_line-info
    SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.

*----- Material Type
  ls_line-typ  = 'S'.
  ls_line-key  = text-h03.
  ls_line-info = p_mtart.
  APPEND ls_line TO lt_top_of_page.

*----- Base date
  ls_line-typ  = 'S'.
  ls_line-key  = text-h04.
  ls_line-info = p_datum.
  APPEND ls_line TO lt_top_of_page.
ENDFORM.                    " comment_build
*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rtn.
  PERFORM check_ekorg.
  PERFORM check_ekgrp.
  PERFORM check_mtart.
ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  alv_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_function.
  DATA:   l_print_p TYPE slis_print_alv.  " print setting

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

*** print paramter   ****************************************
  l_print_p-no_coverpage = 'X'.
  l_print_p-no_print_listinfos = 'X'.
  l_print_p-no_change_print_params = 'X'.
  l_print_p-no_print_selinfos = 'X'.
*************************************************************

  w_status_flg = 'BASE'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_bypassing_buffer       = 'X'
            i_callback_program       = w_program
            i_callback_pf_status_set = 'SET_STATUS'
            i_callback_user_command  = 'USER_COMMAND'
            it_fieldcat              = w_fieldcat[]
            it_sort                  = w_sortcat[]
            i_save                   = 'A'
            it_events                = w_eventcat[]
            is_print                 = l_print_p
       TABLES
            t_outtab                 = it_display
       EXCEPTIONS
            program_error            = 1
            OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " alv_function
*&---------------------------------------------------------------------*
*&      Form  check_ekorg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_ekorg.
  SELECT SINGLE * FROM t024e WHERE ekorg = p_ekorg.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m04.
  ENDIF.
ENDFORM.                    " check_ekorg
*&---------------------------------------------------------------------*
*&      Form  check_ekgrp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_ekgrp.
  SELECT SINGLE * FROM t024 WHERE ekgrp IN s_ekgrp.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m05.
  ENDIF.

  IF     s_ekgrp-low EQ '' AND s_ekgrp-high EQ ''.
    wa_ekgrp_t = 'ZZZ'.
  ELSEIF s_ekgrp-low EQ '' AND s_ekgrp-high NE ''.
    wa_ekgrp_t = s_ekgrp-high.
  ELSEIF s_ekgrp-low NE '' AND s_ekgrp-high EQ ''.
    wa_ekgrp_f = wa_ekgrp_t = s_ekgrp-low.
  ELSEIF s_ekgrp-low EQ '' AND s_ekgrp-high NE ''.
    wa_ekgrp_f = s_ekgrp-low.
    wa_ekgrp_t = s_ekgrp-high.
  ENDIF.
ENDFORM.                    " check_ekgrp
*&---------------------------------------------------------------------*
*&      Form  check_mtart
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_mtart.
  SELECT SINGLE * FROM t134t
                 WHERE mtart = p_mtart
                   AND spras = sy-langu.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m03.
  ENDIF.
ENDFORM.                    " check_mtart
*---------------------------------------------------------------------*
*       FORM SET_STATUS                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RT_EXTAB                                                      *
*---------------------------------------------------------------------*
FORM set_status USING rt_extab TYPE slis_t_extab.
  CASE w_status_flg.
    WHEN 'BASE'.
      SET PF-STATUS 'BASE'.
    WHEN 'DETAIL'.
      SET PF-STATUS 'DETAIL'.
  ENDCASE.
ENDFORM.                    "
*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  SELFIELD                                                      *
*---------------------------------------------------------------------*
FORM user_command USING ucomm LIKE sy-ucomm
                       selfield TYPE slis_selfield.
  CASE ucomm.
    WHEN '&DETAIL'.
      READ TABLE it_display INDEX selfield-tabindex.
      PERFORM display_detail_rtn.
    WHEN '&HISTORY'.
      READ TABLE it_detail INDEX selfield-tabindex.
      PERFORM display_gr_history.
  ENDCASE.
ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  display_DETAIL_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_detail_rtn.
  CLEAR: w_eventcat,    w_eventcat,
         w_sortcat,     w_sortcat[],
         w_fieldcat,    w_fieldcat[],
         w_top_of_page_detail, w_top_of_page_detail[].

  w_status_flg = 'DETAIL'.

  PERFORM build_fieldcat_detail.
  PERFORM build_event.
  PERFORM build_sort_detail.
  PERFORM comment_build_detail USING  w_top_of_page_detail[].
  PERFORM alv_function_detail.

  w_status_flg = 'BASE'.
ENDFORM.                    " display_DETAIL_rtn
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat_detail.
  CLEAR: w_col_pos.

**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : cur field      &8 : no zero          &9 : just

  append_fieldcat :
    w_col_pos 'DATAB' 10 text-008 'CHAR' 'X' ''      '' '',
    w_col_pos 'MATNR' 18 text-011 'CHAR' 'X' ''      '' '',
    w_col_pos 'MAKTX' 20 text-012 'CHAR' ''  ''      '' '',
    w_col_pos 'LIFNR' 10 text-009 'CHAR' ''  ''      '' '',
    w_col_pos 'NAME1' 20 text-010 'CHAR' ''  ''      '' '',
    w_col_pos 'KZUST'  5 text-013 'CHAR' ''  ''      '' '',
    w_col_pos 'WAERS'  4 text-014 'CUKY' ''  ''      '' '',
    w_col_pos 'KBETR' 15 text-015 'CURR' ''  'WAERS' '' '',
    w_col_pos 'KPEIN'  5 text-016 'DEC'  ''  ''      '' '',
    w_col_pos 'KMEIN'  3 text-017 'UNIT' ''  ''      '' ''.
ENDFORM.                    " build_fieldcat_detail
*&---------------------------------------------------------------------*
*&      Form  build_sort_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sort_detail.
  w_sortcat-spos           = 1.
  w_sortcat-fieldname      = 'DATAB'.
  w_sortcat-tabname        = 'IT_PRICE_INFO'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.
ENDFORM.                    " build_sort_DETAIL
*&---------------------------------------------------------------------*
*&      Form  comment_build_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM comment_build_detail USING  lt_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
        l_manager(50),
        l_date(50),
        l_list(50),
        l_dsnam LIKE t024d-dsnam,
        l_h_dsnam LIKE t024d-dsnam,
        l_ldate(10),
        l_hdate(10).

*----- Title
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-h05.
  APPEND ls_line TO lt_top_of_page.

*----- Purchase Org.
  ls_line-typ  = 'S'.
  ls_line-key  = text-h02.
  CONCATENATE p_ekorg t024e-ekotx INTO ls_line-info
    SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.

*----- Material Type
  ls_line-typ  = 'S'.
  ls_line-key  = text-h03.
  ls_line-info = p_mtart.
  APPEND ls_line TO lt_top_of_page.

*----- Base date
  ls_line-typ  = 'S'.
  ls_line-key  = text-h04.
  ls_line-info = p_datum.
  APPEND ls_line TO lt_top_of_page.

*----- Purchase Grp.
  ls_line-typ  = 'S'.
  ls_line-key  = text-h06.
  CONCATENATE it_display-ekgrp it_display-eknam INTO ls_line-info
    SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.
ENDFORM.                    " comment_build_DETAIL
*&---------------------------------------------------------------------*
*&      Form  ALV_FUNCTION_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_function_detail.
  DATA: l_print_p TYPE slis_print_alv.  " print setting

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

*** print paramter   ****************************************
  l_print_p-no_coverpage = 'X'.
  l_print_p-no_print_listinfos = 'X'.
  l_print_p-no_change_print_params = 'X'.
  l_print_p-no_print_selinfos = 'X'.
*************************************************************

  CLEAR: it_detail, it_detail[].
  LOOP AT it_price_info WHERE ekgrp = it_display-ekgrp.
    MOVE it_price_info TO it_detail.
    APPEND it_detail.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_bypassing_buffer       = 'X'
            i_callback_program       = w_program
            i_callback_pf_status_set = 'SET_STATUS'
            i_callback_user_command  = 'USER_COMMAND'
            it_fieldcat              = w_fieldcat[]
            it_sort                  = w_sortcat[]
            i_save                   = 'A'
            it_events                = w_eventcat[]
            is_print                 = l_print_p
       TABLES
            t_outtab                 = it_detail
       EXCEPTIONS
            program_error            = 1
            OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " ALV_FUNCTION_DETAIL
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_GR_HISTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_gr_history.
  SUBMIT zrmmpm29r_undetermin_price
    WITH s_werks BETWEEN '0000' AND 'ZZZZ' SIGN 'I'
    WITH s_lifnr = it_detail-lifnr
    WITH s_ekgrp = it_display-ekgrp
    WITH s_matnr = it_detail-matnr
     AND RETURN.
ENDFORM.                    " DISPLAY_GR_HISTORY
