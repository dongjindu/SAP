*----------------------------------------------------------------------
* Program ID        : ZACOU137
* Title             : [CO] Daily update for source info.(mat-vendor)
* Created on        : 12/17/2007
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : Daily update for source info.(mat-vendor)
*----------------------------------------------------------------------
REPORT zacou137 MESSAGE-ID zmco.

TABLES : t001k, ztcou137 ,sscrfields , *ztcou137.

INCLUDE : z_moon_alv_top,
          z_moon_alv_fnc.
*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
DEFINE __define_not_important.
* { not important
* Total Doc. Count to be created.
  data  : total_doc_cnt type i,
          current_doc_cnt type i.
  data : percentage type p,$mod type i,
         $current_cnt(10),$total_cnt(10),$text(30) .
  clear : total_doc_cnt,current_doc_cnt.
* }
END-OF-DEFINITION.

DEFINE __not_important.
  add 1 to current_doc_cnt.
  $mod = current_doc_cnt mod 100.
  if $mod eq 0.
    $current_cnt = current_doc_cnt.
    concatenate $current_cnt '/' $total_cnt
    into $text.
    condense $text.
    percentage = current_doc_cnt / total_doc_cnt * 100.
    perform show_progress using $text percentage.
  endif.
END-OF-DEFINITION.

DEFINE __not_important_pre.
  add 1 to current_doc_cnt.
  $current_cnt = current_doc_cnt.
  concatenate 'Get PO' $current_cnt '/' $total_cnt
  into $text.
  condense $text.
  percentage = current_doc_cnt / total_doc_cnt * 100.
  perform show_progress using $text percentage.
END-OF-DEFINITION.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.

DEFINE __message.
  call function 'POPUP_TO_INFORM'
       exporting
            titel = &1
            txt1  = &2
            txt2  = sy-subrc.
END-OF-DEFINITION.

DEFINE __u_break.
  if p_debug eq true and p_matnr ne space.
    if gt_ekbe-matnr eq p_matnr.
      if not p_date is initial.
        if gt_ekbe-budat eq p_date.
          break-point.
        endif.
      else.
        break-point.
      endif.
    endif.
  endif.
END-OF-DEFINITION.

DEFINE __u__simple_break.
  if p_debug eq true.
    break-point.
  endif.
END-OF-DEFINITION.

****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.
*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS     : p_bukrs LIKE t001k-bukrs OBLIGATORY MEMORY ID buk.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-003.
SELECT-OPTIONS : s_matnr FOR ztcou137-matnr.
SELECTION-SCREEN END OF BLOCK bl3.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.
SELECT-OPTIONS : s_zgrdt FOR sy-datum.
SELECTION-SCREEN END OF BLOCK bl2.

SELECTION-SCREEN BEGIN OF BLOCK bl4 WITH FRAME TITLE text-004.
PARAMETER: p_upd AS CHECKBOX DEFAULT false,
           p_hst AS CHECKBOX DEFAULT false.

SELECTION-SCREEN END OF BLOCK bl4.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.
PARAMETER :    p_dsp AS CHECKBOX DEFAULT true,
               p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK view-result WITH FRAME TITLE text-t03.
SELECTION-SCREEN PUSHBUTTON  1(24) vslt USER-COMMAND vslt.
SELECTION-SCREEN END OF BLOCK view-result.

PARAMETER : p_debug AS CHECKBOX.
PARAMETER : p_matnr LIKE ztcou137-matnr.
PARAMETER : p_date LIKE sy-datum.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-022.
PARAMETER p_mx_r TYPE i DEFAULT '50' NO-DISPLAY. " obligatory.
SELECTION-SCREEN END OF BLOCK b5.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
RANGES : gr_matnr FOR ekbe-matnr.

TYPES: BEGIN OF ty_row_tab.
        INCLUDE STRUCTURE ztcou137.
TYPES: END OF ty_row_tab.

TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES: END OF ty_out.

DATA  : it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE,
        i_ztcou137 TYPE TABLE OF ztcou137    WITH HEADER LINE.

TYPES: BEGIN OF ty_t001k,
         bwkey TYPE bwkey,                " Plant
       END OF ty_t001k.

DATA : BEGIN OF gt_ekbe OCCURS 0,

        matnr TYPE matnr,
        budat TYPE budat,
        lifnr TYPE lifnr,
        menge TYPE menge_d,   " G/R Qty
        shkzg TYPE shkzg,

        ebeln TYPE ebeln,
        ebelp TYPE ebelp,
        bwart TYPE bwart,
        vgabe TYPE vgabe,
        bewtp TYPE bewtp,
        zekkn TYPE dzekkn,
        knttp TYPE knttp,

       END OF gt_ekbe.

DATA $gt_ekbe LIKE gt_ekbe OCCURS 0 WITH HEADER LINE.

DATA: g_error(1),
      g_repid  LIKE sy-repid.

RANGES: r_bwkey FOR t001k-bwkey.           " Plant

INCLUDE <icon>.                        " icon
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  sy-title = '[CO] Daily update for source info.(mat-vendor)'.
  PERFORM default_.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  PERFORM :
            initialize,
            get_row_data,
            refine_row_itab.

  IF p_dsp EQ true.
    PERFORM view_from_memory.
  ENDIF.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
  CASE sscrfields-ucomm.
    WHEN 'VSLT'.
      PERFORM view_.
  ENDCASE.

*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_S01  text
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  SET_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_output.

  CHECK : p_dsp EQ true,
          g_error IS INITIAL.

  PERFORM show_progress     USING 'Preparing screen...' '95'.
  PERFORM init_alv_parm.
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM sort_build        USING gt_sort[].
  PERFORM alv_events_get    USING:  'P', 'T'.
  PERFORM alv_grid_display  TABLES  gt_out USING ''.

ENDFORM.                    " SET_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_PARM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv_parm.

  __cls   :  gt_fieldcat, gt_sort, gt_events, gt_listheader,
             gt_sp_group.

  CLEAR   :  gs_layout.

  gs_layout-colwidth_optimize = 'X'.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

ENDFORM.                    " INIT_ALV_PARM
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv .

  DATA: l_pos TYPE i.

  __cls ft_fieldcat.

  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-emphasize     = &6.
    gs_fieldcat-cfieldname    = &7.
    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.

  __catalog :
    'X'  'MATNR'    'Material'         18  'CHAR' '' '',
    ' '  'ZDTFR'    'From'              8  'DATS' '' '',
    ' '  'ZDTTO'    'To'                8  'DATS' '' '',
    ' '  'LIFNR'    'Vendor'           10  'CHAR' '' ''.
  PERFORM change_fieldcat USING ft_fieldcat[] .

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build USING    ft_sort TYPE slis_t_sortinfo_alv.

  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-comp      = &5.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

  sort_tab :
             'MATNR'        ' ' 'X' 'X' 'X',
             'ZDTFR'        ' ' 'X' 'X' 'X',
             'ZDTTO'        ' ' 'X' 'X' 'X',
             'LIFNR'        ' ' 'X' 'X' 'X'.
ENDFORM.                    " SORT_BUILD

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  DATA l_text(60).
  REFRESH gt_listheader.

  l_text = 'Daily update for source info.(mat-vendor)'.
  PERFORM set_header_line USING:
          'P' 'H' ''                 l_text       '',
          'P' 'S' 'Company Code' p_bukrs      '',
          'D' 'S' 'Run Date'     s_zgrdt-low s_zgrdt-high.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = gt_listheader.

ENDFORM.                    "top_of_page
*---------------------------------------------------------------------*
*       FORM PF_STATUS_SET
*---------------------------------------------------------------------*
FORM pf_status_set USING  ft_extab TYPE slis_t_extab.
  IF p_upd EQ true.
    SET PF-STATUS '100' EXCLUDING 'SAVE'.
  ELSE.
    SET PF-STATUS '100' EXCLUDING ft_extab.
  ENDIF.
ENDFORM.                    "PF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM user_command USING fp_ucomm LIKE sy-ucomm
                        fs       TYPE slis_selfield.
  CLEAR : g_error.

  CASE fp_ucomm.
    WHEN 'SAVE'.
      CHECK g_error NE true.
  ENDCASE.

ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  CHANGE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FT_FIELDCAT[]  text
*      -->P_ENDFORM  text
*----------------------------------------------------------------------*
FORM change_fieldcat USING    pt_fieldcat TYPE slis_t_fieldcat_alv.

  LOOP AT pt_fieldcat INTO gs_fieldcat.
    gs_fieldcat-ref_tabname = 'ZTCOU137'.
    gs_fieldcat-ref_fieldname = gs_fieldcat-fieldname.
    MODIFY pt_fieldcat FROM gs_fieldcat.
  ENDLOOP.

ENDFORM.                    " CHANGE_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  default_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM default_.

  WRITE:
          icon_biw_report_view AS ICON TO vslt,
         'View saved data' TO vslt+4(21).

ENDFORM.                    " default_
*&---------------------------------------------------------------------*
*&      Form  refine_row_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refine_row_itab.

  CHECK g_error EQ space.

  __define_not_important.

  IF p_upd EQ true.
    IF s_zgrdt[] IS INITIAL.
** Furong on 08/05/14 requested by Pascal (
*      PERFORM delete_data_normal.
** )
    ELSE.
      PERFORM delete_data_period.
    ENDIF.
    COMMIT WORK.
  ENDIF.

  DATA $gt_ekbe  LIKE gt_ekbe OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF i_ztcou137 OCCURS 0,
         lifnr TYPE lifnr,
         zdtfr TYPE ad_date_fr,
         zdtto TYPE ad_date_to,
         menge TYPE menge_d,
        END OF i_ztcou137.

  DATA $i_ztcou137 LIKE i_ztcou137 OCCURS 0 WITH HEADER LINE.
  DATA $exists.
  DATA : $from LIKE sy-datum,
         $to   LIKE sy-datum,
         $flag_start, $flag_end.
  DATA $ix LIKE sy-tabix.
  DATA $flag.

  SORT gt_ekbe BY matnr budat lifnr.
  $gt_ekbe[] = gt_ekbe[].

  DELETE ADJACENT DUPLICATES FROM $gt_ekbe
      COMPARING matnr.

  DESCRIBE TABLE $gt_ekbe LINES total_doc_cnt.

  $total_cnt = total_doc_cnt.

  __cls it_row_tab.

  LOOP AT $gt_ekbe.

    AT NEW matnr.
      $flag = true.
    ENDAT.

    CHECK $flag EQ true.
    CLEAR $flag.

* {
    __not_important.
    __u_break.
* }

    READ TABLE gt_ekbe WITH KEY matnr = $gt_ekbe-matnr
                                BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    __cls : i_ztcou137, $i_ztcou137.

    LOOP AT gt_ekbe FROM sy-tabix.

      IF gt_ekbe-matnr NE $gt_ekbe-matnr.
        EXIT.
      ENDIF.

      MOVE-CORRESPONDING gt_ekbe TO i_ztcou137.
      i_ztcou137-zdtfr = gt_ekbe-budat.
      APPEND i_ztcou137.

    ENDLOOP.

    SORT i_ztcou137 BY zdtfr lifnr .

    CLEAR : $from, $to, $flag_start, $flag_end.

    LOOP AT i_ztcou137.
      AT NEW lifnr.
        $flag_start = true.
      ENDAT.

      IF $flag_start EQ true.
        CLEAR $flag_start.
        $from = i_ztcou137-zdtfr.
      ENDIF.
      AT END OF lifnr.
        $flag_end = true.
      ENDAT.
      AT LAST.
        $flag_end = true.
      ENDAT.

      IF $flag_end EQ true.
        CLEAR $flag.
        $to = i_ztcou137-zdtfr.
      ENDIF.

      IF NOT $from IS INITIAL AND NOT $to IS INITIAL.
        $i_ztcou137-lifnr = i_ztcou137-lifnr.
        $i_ztcou137-zdtfr = $from.
        $i_ztcou137-zdtto = $to.
        APPEND $i_ztcou137.
        CLEAR : $from,$to.
      ENDIF.
    ENDLOOP.

    SORT $i_ztcou137 BY zdtfr zdtto.

    CLEAR $flag_end.

    LOOP AT $i_ztcou137.
      $ix = sy-tabix - 1.
      CHECK  $ix > 0.
      $i_ztcou137-zdtto = $i_ztcou137-zdtfr - 1.
      MODIFY $i_ztcou137 INDEX $ix TRANSPORTING zdtto.
      AT LAST.
        $flag_end = true.
      ENDAT.
      CHECK $flag_end = true.
      CLEAR $flag_end.
      $i_ztcou137-zdtto = '99991231'.
      MODIFY $i_ztcou137 INDEX sy-tabix TRANSPORTING zdtto.
    ENDLOOP.

    IF $ix EQ 0.
      $i_ztcou137-zdtto = '99991231'.
      MODIFY $i_ztcou137 INDEX 1 TRANSPORTING zdtto.
    ENDIF.

    LOOP AT $i_ztcou137.
      IF $i_ztcou137-zdtfr > $i_ztcou137-zdtto.
        CONTINUE.
      ENDIF.
      it_row_tab-bukrs = p_bukrs.
      it_row_tab-matnr = $gt_ekbe-matnr.
      it_row_tab-zdtfr = $i_ztcou137-zdtfr.
      it_row_tab-zdtto = $i_ztcou137-zdtto.
      it_row_tab-lifnr = $i_ztcou137-lifnr.
      APPEND it_row_tab.
    ENDLOOP.
  ENDLOOP.

  IF p_upd EQ true.
    CLEAR $flag.
    CHECK NOT it_row_tab[] IS INITIAL.
    PERFORM save_z_table  .                                 " ZTCOU137
  ENDIF.

ENDFORM.                    " refine_row_itab
*&---------------------------------------------------------------------*
*&      Form  MOVE_OUT_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_out.

  __cls gt_out.
  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
    APPEND gt_out.
  ENDLOOP.

ENDFORM.                    " MOVE_OUT_
*&---------------------------------------------------------------------*
*&      Form  VIEW_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM view_.

  PERFORM  initialize            .
  PERFORM view_from_table.

ENDFORM.                    " VIEW_
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0811   text
*      -->P_0812   text
*      -->P_0813   text
*      <--P_L_ANSWER  text
*----------------------------------------------------------------------*
FORM pop_up USING    p_text p_text2 p_canc
            CHANGING p_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1      = p_text
            textline2      = p_text2
            titel          = 'Check!'
            cancel_display = p_canc
       IMPORTING
            answer         = p_answer.

ENDFORM.                    " POP_UP
*&---------------------------------------------------------------------*
*&      Form  initialize
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize.
  CLEAR g_error.
  PERFORM get_r_bwkey.
ENDFORM.                    " initialize

*---------------------------------------------------------------------*
*       FORM get_r_bwkey                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM get_r_bwkey.

  DATA  lt_plant TYPE TABLE OF ty_t001k     WITH HEADER LINE.

  __cls : r_bwkey.

  SELECT bwkey INTO TABLE lt_plant FROM t001k
  WHERE bukrs EQ p_bukrs.

  r_bwkey-sign   = 'I'.
  r_bwkey-option = 'EQ'.

  LOOP AT lt_plant.
    r_bwkey-low = lt_plant-bwkey.
    COLLECT r_bwkey.
  ENDLOOP.

ENDFORM.                    " get_r_bwkey
*&---------------------------------------------------------------------*
*&      Form  get_row_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_row_data.

  DATA $ix LIKE sy-tabix.

  PERFORM show_progress USING 'Gather PO...' '5'.

  __cls : gt_ekbe.

  __u__simple_break.

  IF s_zgrdt[] IS INITIAL.
    PERFORM get_data_normal.
  ELSE.
* perform get_data_period.
* It is faster than above. {
    PERFORM get_data_period_new.
* }
  ENDIF.

  CHECK g_error EQ false.
  __cls $gt_ekbe.

  __u__simple_break.

*  perform gr_first_date .

  LOOP AT gt_ekbe.
    $gt_ekbe = gt_ekbe.
    COLLECT $gt_ekbe.
  ENDLOOP.

  __cls gt_ekbe.
  gt_ekbe[] = $gt_ekbe[].

  __cls $gt_ekbe.
  LOOP AT gt_ekbe.
    __u_break.
    $gt_ekbe = gt_ekbe.
    IF $gt_ekbe-shkzg NE 'S'.
      $gt_ekbe-menge = -1 * $gt_ekbe-menge.
    ENDIF.
    CLEAR : $gt_ekbe-shkzg,
            $gt_ekbe-ebeln,
            $gt_ekbe-ebelp,
            $gt_ekbe-bwart,
            $gt_ekbe-vgabe,
            $gt_ekbe-bewtp,
            $gt_ekbe-zekkn,
            $gt_ekbe-knttp.
    COLLECT $gt_ekbe.
    CLEAR $gt_ekbe.
  ENDLOOP.

  __cls gt_ekbe.
  gt_ekbe[] = $gt_ekbe[].

  DELETE gt_ekbe WHERE menge EQ 0.

  READ TABLE gt_ekbe INDEX 1.

  CHECK sy-subrc EQ 0.

  SORT gt_ekbe BY matnr budat lifnr.

  DELETE ADJACENT DUPLICATES FROM gt_ekbe
  COMPARING matnr budat .

ENDFORM.                    " get_row_data
*&---------------------------------------------------------------------*
*&      Form  save_z_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_z_table.

  CHECK g_error EQ false.

  DATA $lines LIKE sy-tabix.
  DATA $flag.

  __cls i_ztcou137.

  SORT it_row_tab BY bukrs matnr zdtfr.

  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO i_ztcou137.
    i_ztcou137-bukrs = p_bukrs.
    i_ztcou137-aedat = sy-datum.
    i_ztcou137-aenam = sy-uname.
    APPEND i_ztcou137.CLEAR i_ztcou137.
  ENDLOOP.

  DESCRIBE TABLE i_ztcou137 LINES $lines.

  LOOP AT i_ztcou137.
    SELECT SINGLE * FROM ztcou137
    WHERE bukrs EQ i_ztcou137-bukrs
      AND matnr EQ i_ztcou137-matnr
      AND zdtfr EQ  i_ztcou137-zdtfr
** Fuorng on 08/06/14
*     AND manupd EQ true.
      AND lifnr EQ  i_ztcou137-lifnr.
** )
    IF sy-subrc EQ 0.
    ELSE.
      ztcou137 = i_ztcou137.
      MODIFY ztcou137. " from table i_ztcou137.
    ENDIF.
  ENDLOOP.

  IF sy-subrc EQ 0 AND $lines > 0.
    MESSAGE s000 WITH 'Data has been saved successfully.'. " $lines .
  ENDIF..

  COMMIT WORK.

ENDFORM.                    " save_z_table
*&---------------------------------------------------------------------*
*&      Form  view_from_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM view_from_table.
  __cls it_row_tab.

  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE it_row_tab
  FROM ztcou137
      WHERE bukrs EQ p_bukrs
        AND matnr IN s_matnr.

  CHECK sy-subrc EQ 0.

  DATA : $p_upd LIKE p_upd,
         $p_dsp LIKE p_dsp.

  $p_upd = p_upd.
  $p_dsp = p_dsp.

  p_upd = true.
  p_dsp = true.

  PERFORM  : move_out ,
             set_output .

  p_upd = $p_upd.
  p_dsp = $p_dsp.

ENDFORM.                    " view_from_table

*---------------------------------------------------------------------*
*       FORM view_from_memory                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM view_from_memory.

  DATA : $p_upd LIKE p_upd,
         $p_dsp LIKE p_dsp.

  $p_upd = p_upd.
  $p_dsp = p_dsp.

  p_upd = true.
  p_dsp = true.

  PERFORM : move_out ,
            set_output .

  p_upd = $p_upd.
  p_dsp = $p_dsp.

ENDFORM.                    " view_from_table

*&---------------------------------------------------------------------*
*&      Form  one_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gr_first_date.

  DATA : $ix LIKE sy-tabix.

  DATA $gt_ekbe LIKE gt_ekbe OCCURS 0 WITH HEADER LINE.
  $gt_ekbe[] = gt_ekbe[].

  SORT $gt_ekbe BY matnr budat ebeln.

  DELETE ADJACENT DUPLICATES FROM $gt_ekbe
      COMPARING matnr budat ebeln.

  SORT gt_ekbe BY matnr budat ebeln.

  LOOP AT gt_ekbe.
    $ix = sy-tabix.
    READ TABLE $gt_ekbe WITH KEY matnr = gt_ekbe-matnr
                                 ebeln = gt_ekbe-ebeln
                          BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_ekbe-budat =  $gt_ekbe-budat.
      MODIFY gt_ekbe INDEX $ix TRANSPORTING budat.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " one_date
*&---------------------------------------------------------------------*
*&      Form  get_data_normal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_normal.

  SELECT a~ebeln a~ebelp a~bwart a~vgabe a~bewtp a~zekkn b~knttp
         a~budat a~matnr
         c~lifnr
         a~shkzg
         SUM( a~menge ) AS menge
         FROM ekbe AS a
         JOIN ekpo AS b
         ON b~ebeln EQ a~ebeln
        AND b~ebelp EQ a~ebelp
         JOIN ekko AS c
         ON c~ebeln EQ b~ebeln
        INTO CORRESPONDING FIELDS OF TABLE gt_ekbe
      WHERE a~werks IN r_bwkey
        AND a~bwart NE space
        AND a~matnr IN s_matnr
        AND a~vgabe EQ '1'
        AND a~bewtp EQ 'E'
        AND b~knttp EQ space
     GROUP by a~ebeln a~ebelp a~bwart a~vgabe a~bewtp a~zekkn b~knttp
         a~budat a~matnr
         c~lifnr
         a~shkzg  .

  IF p_hst EQ true.

    SELECT a~ebeln a~ebelp a~bwart a~vgabe a~bewtp a~zekkn b~knttp
           a~budat a~matnr
           c~lifnr
           a~shkzg
           SUM( a~menge ) AS menge
           FROM ekbeh AS a
           JOIN ekpo AS b
           ON b~ebeln EQ a~ebeln
          AND b~ebelp EQ a~ebelp
           JOIN ekko AS c
           ON c~ebeln EQ b~ebeln
          APPENDING CORRESPONDING FIELDS OF TABLE gt_ekbe
        WHERE a~werks IN r_bwkey
          AND a~bwart NE space
          AND a~matnr IN s_matnr
          AND a~vgabe EQ '1'
          AND a~bewtp EQ 'E'
          AND b~knttp EQ space
       GROUP by a~ebeln a~ebelp a~bwart a~vgabe a~bewtp a~zekkn b~knttp
           a~budat a~matnr
           c~lifnr
           a~shkzg  .

  ENDIF.

ENDFORM.                    " get_data_normal
*&---------------------------------------------------------------------*
*&      Form  get_data_normal_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_period.

  DATA $gr_matnr LIKE gr_matnr OCCURS 0  WITH HEADER LINE.

  DATA: BEGIN OF $matnr OCCURS 0,
          matnr   LIKE   resb-matnr,
        END   OF $matnr.

  __define_not_important.

  SELECT DISTINCT matnr INTO TABLE $matnr
          FROM ekbe
          WHERE werks IN r_bwkey
            AND budat IN s_zgrdt
            AND bwart NE space
            AND matnr IN s_matnr
            AND vgabe EQ '1'
            AND bewtp EQ 'E'
         %_HINTS ORACLE 'FIRST_ROWS(1)'.

  IF p_hst EQ true.

    SELECT DISTINCT matnr APPENDING TABLE $matnr
            FROM ekbeh
            WHERE werks IN r_bwkey
              AND budat IN s_zgrdt
              AND bwart NE space
              AND matnr IN s_matnr
              AND vgabe EQ '1'
              AND bewtp EQ 'E'
           %_HINTS ORACLE 'FIRST_ROWS(1)'.

  ENDIF.

  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'Could not find data.'.
    g_error = true.
    EXIT.
  ENDIF.

  SORT $matnr.
  DELETE ADJACENT DUPLICATES FROM $matnr.
  DELETE $matnr WHERE matnr EQ space.
  __cls gr_matnr.

  LOOP AT $matnr.
    CHECK $matnr-matnr NE space.
    gr_matnr = 'IEQ'.
    gr_matnr-low = $matnr-matnr.
    APPEND gr_matnr.
  ENDLOOP.

  CLEAR gr_matnr.

  DATA max_range TYPE i.
  DATA line1 LIKE sy-tabix.
  DESCRIBE TABLE gr_matnr LINES line1.

  IF p_mx_r IS INITIAL.
    max_range = 50.
  ELSE.
    max_range = p_mx_r.
  ENDIF.

  IF line1 < max_range.
    $gr_matnr[] = gr_matnr[].
    PERFORM get_po_one_time TABLES $gr_matnr.
  ELSE.

    DATA : $times TYPE i,
           $ix LIKE sy-tabix,
           $line LIKE sy-tabix.

    $times = line1 / max_range.
    ADD 1 TO $times.
    $ix = 1.

    total_doc_cnt = $times.
    $total_cnt = total_doc_cnt.

    DO $times TIMES.

      __not_important_pre.

      __cls $gr_matnr.
      $line = 1.

      LOOP AT gr_matnr FROM $ix.
        $ix = sy-tabix.
        $gr_matnr = gr_matnr.
        APPEND $gr_matnr.
        CLEAR $gr_matnr.
        ADD 1 TO $line.
        IF $line > max_range.
          EXIT.
        ENDIF.
      ENDLOOP.

      ADD 1 TO $ix.

      IF NOT $gr_matnr[] IS INITIAL.
        PERFORM get_po_several_times TABLES $gr_matnr.
      ENDIF.

      IF $ix > line1.
        EXIT.
      ENDIF.

    ENDDO.

  ENDIF.

ENDFORM.                    " get_data_normal_period
*&---------------------------------------------------------------------*
*&      Form  get_po_one_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GR_MATNR  text
*----------------------------------------------------------------------*
FORM get_po_one_time TABLES $gr_matnr STRUCTURE gr_matnr.

  SELECT a~ebeln a~ebelp a~bwart a~vgabe a~bewtp a~zekkn b~knttp
         a~budat a~matnr
         c~lifnr
         a~shkzg
         SUM( a~menge ) AS menge
         FROM ekbe AS a
         JOIN ekpo AS b
         ON b~ebeln EQ a~ebeln
        AND b~ebelp EQ a~ebelp
         JOIN ekko AS c
         ON c~ebeln EQ b~ebeln
        INTO CORRESPONDING FIELDS OF TABLE gt_ekbe
      WHERE a~werks IN r_bwkey
        AND a~bwart NE space
        AND a~matnr IN $gr_matnr
        AND a~vgabe EQ '1'
        AND a~bewtp EQ 'E'
        AND b~knttp EQ space
  GROUP by a~ebeln a~ebelp a~bwart a~vgabe a~bewtp a~zekkn b~knttp
      a~budat a~matnr
      c~lifnr
      a~shkzg  .

  IF p_hst EQ true.

    SELECT a~ebeln a~ebelp a~bwart a~vgabe a~bewtp a~zekkn b~knttp
           a~budat a~matnr
           c~lifnr
           a~shkzg
           SUM( a~menge ) AS menge
           FROM ekbeh AS a
           JOIN ekpo AS b
           ON b~ebeln EQ a~ebeln
          AND b~ebelp EQ a~ebelp
           JOIN ekko AS c
           ON c~ebeln EQ b~ebeln
          APPENDING CORRESPONDING FIELDS OF TABLE gt_ekbe
        WHERE a~werks IN r_bwkey
          AND a~bwart NE space
          AND a~matnr IN $gr_matnr
          AND a~vgabe EQ '1'
          AND a~bewtp EQ 'E'
          AND b~knttp EQ space
    GROUP by a~ebeln a~ebelp a~bwart a~vgabe a~bewtp a~zekkn b~knttp
        a~budat a~matnr
        c~lifnr
        a~shkzg  .
  ENDIF.
ENDFORM.                    " get_po_one_time
*&---------------------------------------------------------------------*
*&      Form  get_po_several_times
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GR_MATNR  text
*----------------------------------------------------------------------*
FORM get_po_several_times TABLES $gr_matnr STRUCTURE gr_matnr.

  SELECT a~ebeln a~ebelp a~bwart a~vgabe a~bewtp a~zekkn b~knttp
         a~budat a~matnr
         c~lifnr
         a~shkzg
         SUM( a~menge ) AS menge
         FROM ekbe AS a
         JOIN ekpo AS b
         ON b~ebeln EQ a~ebeln
        AND b~ebelp EQ a~ebelp
         JOIN ekko AS c
         ON c~ebeln EQ b~ebeln
        APPENDING CORRESPONDING FIELDS OF TABLE gt_ekbe
      WHERE a~werks IN r_bwkey
        AND a~bwart NE space
        AND a~matnr IN $gr_matnr
        AND a~vgabe EQ '1'
        AND a~bewtp EQ 'E'
        AND b~knttp EQ space
  GROUP by a~ebeln a~ebelp a~bwart a~vgabe a~bewtp a~zekkn b~knttp
      a~budat a~matnr
      c~lifnr
      a~shkzg  .

  IF p_hst EQ true.

    SELECT a~ebeln a~ebelp a~bwart a~vgabe a~bewtp a~zekkn b~knttp
           a~budat a~matnr
           c~lifnr
           a~shkzg
           SUM( a~menge ) AS menge
           FROM ekbeh AS a
           JOIN ekpo AS b
           ON b~ebeln EQ a~ebeln
          AND b~ebelp EQ a~ebelp
           JOIN ekko AS c
           ON c~ebeln EQ b~ebeln
          APPENDING CORRESPONDING FIELDS OF TABLE gt_ekbe
        WHERE a~werks IN r_bwkey
          AND a~bwart NE space
          AND a~matnr IN $gr_matnr
          AND a~vgabe EQ '1'
          AND a~bewtp EQ 'E'
          AND b~knttp EQ space
    GROUP by a~ebeln a~ebelp a~bwart a~vgabe a~bewtp a~zekkn b~knttp
        a~budat a~matnr
        c~lifnr
        a~shkzg  .
  ENDIF.
ENDFORM.                    " get_po_several_times
*&---------------------------------------------------------------------*
*&      Form  delete_data_normal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data_normal.

  DELETE FROM ztcou137
              WHERE bukrs =  p_bukrs
                AND matnr IN s_matnr
                AND manupd NE true.

ENDFORM.                    " delete_data_normal
*&---------------------------------------------------------------------*
*&      Form  delete_data_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data_period.

  __define_not_important.

  DATA $gr_matnr LIKE gr_matnr OCCURS 0  WITH HEADER LINE.
  DATA max_range TYPE i.
  DATA line1 LIKE sy-tabix.
  DESCRIBE TABLE gr_matnr LINES line1.

  IF p_mx_r IS INITIAL.
    max_range = 50.
  ELSE.
    max_range = p_mx_r.
  ENDIF.

  IF line1 < max_range.
    $gr_matnr[] = gr_matnr[].

    DELETE FROM ztcou137
                WHERE bukrs =  p_bukrs
                  AND matnr IN $gr_matnr.

  ELSE.

    DATA : $times TYPE i,
           $ix LIKE sy-tabix,
           $line LIKE sy-tabix.

    $times = line1 / max_range.
    ADD 1 TO $times.
    $ix = 1.

    total_doc_cnt = $times.
    $total_cnt = total_doc_cnt.

    DO $times TIMES.

      __not_important_pre.

      __cls $gr_matnr.
      $line = 1.

      LOOP AT gr_matnr FROM $ix.
        $ix = sy-tabix.
        $gr_matnr = gr_matnr.
        APPEND $gr_matnr.
        CLEAR $gr_matnr.
        ADD 1 TO $line.
        IF $line > max_range.
          EXIT.
        ENDIF.
      ENDLOOP.

      ADD 1 TO $ix.

      IF NOT $gr_matnr[] IS INITIAL.
        DELETE FROM ztcou137
            WHERE bukrs =  p_bukrs
              AND matnr IN $gr_matnr
              AND manupd NE true.
      ENDIF.

      IF $ix > line1.
        EXIT.
      ENDIF.

    ENDDO.

  ENDIF.

ENDFORM.                    " delete_data_period
*&---------------------------------------------------------------------*
*&      Form  get_data_period_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_period_new.

  DATA $gr_matnr LIKE gr_matnr OCCURS 0  WITH HEADER LINE.

  DATA: BEGIN OF $matnr OCCURS 0,
          matnr   LIKE   resb-matnr,
        END   OF $matnr.

  __define_not_important.

  SELECT DISTINCT matnr INTO TABLE $matnr
          FROM ekbe
          WHERE werks IN r_bwkey
            AND budat IN s_zgrdt
            AND bwart NE space
            AND matnr IN s_matnr
            AND vgabe EQ '1'
            AND bewtp EQ 'E'
         %_HINTS ORACLE 'FIRST_ROWS(1)'.

  IF p_hst EQ true.

    SELECT DISTINCT matnr APPENDING TABLE $matnr
            FROM ekbeh
            WHERE werks IN r_bwkey
              AND budat IN s_zgrdt
              AND bwart NE space
              AND matnr IN s_matnr
              AND vgabe EQ '1'
              AND bewtp EQ 'E'
           %_HINTS ORACLE 'FIRST_ROWS(1)'.

  ENDIF.

  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'Could not find data.'.
    g_error = true.
    EXIT.
  ENDIF.

  SORT $matnr.
  DELETE ADJACENT DUPLICATES FROM $matnr.
  DELETE $matnr WHERE matnr EQ space.

  __cls gr_matnr.

  LOOP AT $matnr.
    CHECK $matnr-matnr NE space.
    gr_matnr = 'IEQ'.
    gr_matnr-low = $matnr-matnr.
    APPEND gr_matnr.
  ENDLOOP.

  CLEAR gr_matnr.

  PERFORM get_data_normal.

  __cls $gt_ekbe.

  LOOP AT gt_ekbe.
    READ TABLE $matnr WITH KEY matnr = gt_ekbe-matnr
                      BINARY SEARCH.
    CHECK sy-subrc EQ 0.
    $gt_ekbe =  gt_ekbe.
    APPEND $gt_ekbe.
  ENDLOOP.

  __cls gt_ekbe.
  gt_ekbe[] = $gt_ekbe[].

ENDFORM.                    " get_data_period_new
