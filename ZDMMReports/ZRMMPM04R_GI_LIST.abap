************************************************************************
* Program Name           : ZRMMPM04R_GI_LIST
* Author                 : Sung-Tae, Lim
* Creation Date          : 2003.10.14.
* Specifications By      : Sung-Tae, Lim
* Pattern                : Report 1-1
* Development Request No : UD1K902070
* Addl Documentation     :
* Description            : GI(normal) List
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.12.11.     Sung-Tae Lim     UD1K904909     Initial Coding
************************************************************************

REPORT  zrmmpm04r_gi_list  NO STANDARD PAGE HEADING
                           LINE-SIZE 132
                           LINE-COUNT 64(1)
                           MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

*------ Type
TYPE-POOLS : slis.               "Abap List View Type

**---
DATA : BEGIN OF it_temp OCCURS 0,
         matnr LIKE mseg-matnr,     " material number
         maktx LIKE makt-maktx,     " material description
         dispo LIKE marc-dispo,     " person of contact(mrp controller)
         bwart LIKE mseg-bwart,     " movement type
         menge LIKE mseg-menge,     " quantity
         meins LIKE mseg-meins,     " unit of measure
       END OF it_temp.

DATA : BEGIN OF it_itab OCCURS 0,
         matnr LIKE it_temp-matnr,     " material number
         maktx LIKE it_temp-maktx,     " material description
         dispo LIKE it_temp-dispo,     " person of contact
         mengt LIKE it_temp-menge,     " total quantity
         meng1 LIKE it_temp-menge,     " backflush qty.
         meng2 LIKE it_temp-menge,     " qty. for scrapped vehicle
         meng3 LIKE it_temp-menge,     " component scrap GI
*         meng4 like it_temp-menge,
         meins LIKE it_temp-meins,
       END OF it_itab.

RANGES : r_bwart FOR mseg-bwart.

**--- Macro
DEFINE append_fieldcat.
  &1 = &1 + 1.
  w_fieldcat-col_pos    = &1.
  w_fieldcat-fieldname  = &2.
  w_fieldcat-outputlen  = &3.
  w_fieldcat-seltext_l  = &4.
  w_fieldcat-seltext_m  = &4.
  w_fieldcat-seltext_s  = &4.
  w_fieldcat-datatype   = &5.
  w_fieldcat-key        = &6.
*  w_fieldcat-do_sum     = &6.
  w_fieldcat-qfieldname = &7.
  w_fieldcat-cfieldname = &8.
  w_fieldcat-no_out     = &9.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.

DEFINE append_mvt_type.
  clear : r_bwart.
  move : 'I'     to r_bwart-sign,
         'EQ'    to r_bwart-option,
         &1      to r_bwart-low.
  append r_bwart.
END-OF-DEFINITION.

DEFINE append_top.
  clear : w_line.
  if not &3 is initial or not &4 is initial.
    w_line-typ   = &1.
    w_line-key   = &2.
    concatenate &3 '~' &4 into w_line-info separated by space.
    append w_line to w_top_of_page.
  endif.
END-OF-DEFINITION.


*------ Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS : s_werks FOR mseg-werks OBLIGATORY, "Plant
                 s_dispo FOR marc-dispo,            "MRP controller
                 s_mtart FOR mara-mtart DEFAULT 'ROH',
                 s_matnr FOR mseg-matnr,            "Material No
                 s_budat FOR mkpf-budat OBLIGATORY. "Posting date
SELECTION-SCREEN END OF BLOCK block1.

*-------At Selection Screen
AT SELECTION-SCREEN.
  PERFORM check_rtn.

**---
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].


**---
TOP-OF-PAGE.
  PERFORM top_of_page.

*------ Start of selection
START-OF-SELECTION.
  PERFORM get_data.

*---
END-OF-SELECTION.
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m05.
  ELSE.
    PERFORM display_data.
  ENDIF.





*&---------------------------------------------------------------------*
*&      Form  check_rtn
*----------------------------------------------------------------------*
FORM check_rtn.
*  PERFORM check_werks_rtn.       "Plant Check
*  PERFORM check_dispo_rtn.       "Person of contact check
*  PERFORM check_matnr_rtn.       "Material No. Check
ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  check_werks_rtn
*----------------------------------------------------------------------*
FORM check_werks_rtn.
  CHECK s_werks-low NE space.
  SELECT SINGLE * FROM t001w WHERE werks EQ s_werks-low.

  IF sy-subrc NE 0.
    MESSAGE e000(zmmm) WITH text-m01.
  ENDIF.
ENDFORM.                    " check_werks_rtn
*&---------------------------------------------------------------------*
*&      Form  check_matnr_rtn
*----------------------------------------------------------------------*
FORM check_matnr_rtn.
  CHECK s_matnr-low NE space.
  SELECT SINGLE * FROM mara WHERE matnr EQ s_matnr-low.

  IF sy-subrc NE 0.
    MESSAGE e000(zmmm) WITH text-m02.
  ENDIF.
ENDFORM.                    " check_matnr_rtn
*&---------------------------------------------------------------------*
*&      Form  check_dispo_rtn
*----------------------------------------------------------------------*
FORM check_dispo_rtn.
  CHECK s_dispo-low NE space.
  SELECT SINGLE * FROM marc WHERE dispo EQ s_dispo-low.

  IF sy-subrc NE 0.
    MESSAGE e000(zmmm) WITH text-m03.
  ENDIF.
ENDFORM.                    " check_dispo_rtn
*&---------------------------------------------------------------------*
*&      Form  get_data
*----------------------------------------------------------------------*
FORM get_data.
*---
  CLEAR : it_temp, it_temp[], it_itab, it_itab[].

*--- set movement type
  PERFORM set_movement_type.

*-- get data (mseg+marc+makt+mkpf+mara)
  SELECT b~matnr
         b~menge
         b~meins
         b~bwart
         c~dispo
         e~maktx
                 INTO CORRESPONDING FIELDS OF TABLE it_temp
                 FROM mkpf AS a INNER JOIN mseg AS b
                   ON a~mandt EQ b~mandt
                  AND a~mblnr EQ b~mblnr
                  AND a~mjahr EQ b~mjahr
                      INNER JOIN marc AS c
                         ON b~mandt EQ c~mandt
                        AND b~matnr EQ c~matnr
                        AND b~werks EQ c~werks
                            INNER JOIN mara AS d
                               ON c~mandt EQ d~mandt
                              AND c~matnr EQ d~matnr
                                  INNER JOIN makt AS e
                                     ON d~mandt EQ e~mandt
                                    AND d~matnr EQ e~matnr
               WHERE b~werks IN s_werks
                 AND c~dispo IN s_dispo
                 AND d~matnr IN s_matnr
                 AND a~budat IN s_budat
*                 AND d~mtart IN ('ROH', 'HALB')
                 AND d~mtart IN s_mtart
                 AND b~bwart IN r_bwart
                 AND e~spras EQ sy-langu.

*---
  LOOP AT it_temp.
    MOVE-CORRESPONDING it_temp TO it_itab.
    CASE it_temp-bwart.
      WHEN '261'.
        it_itab-meng1 = it_temp-menge.
      WHEN '262'.
        it_itab-meng1 = it_temp-menge * -1.
      WHEN '903'.
        it_itab-meng2 = it_temp-menge.
      WHEN '904'.
        it_itab-meng2 = it_temp-menge * -1.
      WHEN '907'.
        it_itab-meng3 = it_temp-menge.
      WHEN '908'.
        it_itab-meng3 = it_temp-menge * -1.
    ENDCASE.
    it_itab-mengt = it_itab-meng1 + it_itab-meng2 + it_itab-meng3.
    COLLECT it_itab.
    CLEAR : it_temp, it_itab.
  ENDLOOP.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  display_data
*----------------------------------------------------------------------*
FORM display_data.
  PERFORM build_fieldcat.                       "build  fieldcat
*  PERFORM build_event.                          "build  event
  PERFORM build_sort.                           "build  sort
  PERFORM comment_build USING  w_top_of_page[]. "Header Title
  PERFORM alv_function.                         "ALV function
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*----------------------------------------------------------------------*
FORM build_fieldcat.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : cur field        &9 : no out
  append_fieldcat :
   w_col_pos 'MATNR' 18 'Material'        'CHAR' 'X'  ''      '' '',
   w_col_pos 'MAKTX' 30 'Material Desc'   'CHAR' ' '  ''      '' '',
   w_col_pos 'DISPO'  4 'PoC'             'CHAR' ' '  ''      '' '',
   w_col_pos 'MENGT' 12 'Total Qty'       'QUAN' ' '  'MEINS' '' '',
   w_col_pos 'MENG1' 12 'B/F   Qty'       'QUAN' ' '  'MEINS' '' '',
   w_col_pos 'MENG2' 12 'Scrap Veh Qty'   'QUAN' ' '  'MEINS' '' '',
   w_col_pos 'MENG3' 12 'Component Scrap' 'QUAN' ' '  'MEINS' '' '',
   w_col_pos 'MEINS'  3 'UoM'             'UNIT' ' '  ''      '' ''.
ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  build_event
*----------------------------------------------------------------------*
FORM build_event.
  w_eventcat-name = 'TOP_OF_PAGE'.
  w_eventcat-form = 'TOP_OF_PAGE'.

  APPEND w_eventcat.
ENDFORM.                    " build_event
*&---------------------------------------------------------------------*
*&      Form  build_sort
*----------------------------------------------------------------------*
FORM build_sort.
*---
  w_sortcat-spos           = 1.
  w_sortcat-fieldname      = 'DISPO'.     "Person of Contact Sort
  w_sortcat-tabname        = 'IT_ITAB'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.

  w_sortcat-spos           = 2.
  w_sortcat-fieldname      = 'MATNR'.     "Material No. Sort
  w_sortcat-tabname        = 'IT_ITAB'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.
ENDFORM.                    " build_sort
*&---------------------------------------------------------------------*
*&      Form  comment_build
*----------------------------------------------------------------------*
FORM comment_build USING it_top_of_page TYPE slis_t_listheader.
*---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-002.
  APPEND w_line TO w_top_of_page.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page.

  append_top :
      'S' text-003 s_werks-low s_werks-high,
      'S' text-004 s_dispo-low s_dispo-high,
      'S' text-005 s_matnr-low s_matnr-high,
      'S' text-006 s_budat-low s_budat-high.
ENDFORM.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  alv_function
*----------------------------------------------------------------------*
FORM alv_function.
  DATA : l_print_p TYPE slis_print_alv.  " print setting

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

  MOVE : 'X' TO w_layout-colwidth_optimize.

*** print paramter   ****************************************
  l_print_p-no_coverpage = 'X'.
  l_print_p-no_print_listinfos = 'X'.
  l_print_p-no_change_print_params = 'X'.
  l_print_p-no_print_selinfos = 'X'.
*************************************************************
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_bypassing_buffer = 'X'
            i_callback_program = w_program
            is_layout          = w_layout
            it_fieldcat        = w_fieldcat[]
            it_sort            = w_sortcat[]
            i_save             = 'A'
            it_events          = w_eventcat[]
            is_print           = l_print_p
       TABLES
            t_outtab           = it_itab
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.
ENDFORM.                    " alv_function

*&---------------------------------------------------------------------*
*&      Form  set_movement_type
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_movement_type.
*---
  CLEAR : r_bwart, r_bwart[].

  append_mvt_type : '261',
                    '262',
                    '903',
                    '904',
                    '907',
                    '908'.
ENDFORM.                    " set_movement_type
