************************************************************************
* Program Name           : ZRMMPM04R_GI_LIST
* Author                 : EunKyung Chun
* Creation Date          : 2003.10.14.
* Specifications By      : EunKyung Chun
* Pattern                : Report 1-1
* Development Request No : UD1K902070
* Addl Documentation     :
* Description            : GI(normal) List
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
**&---------------------------------------------------------------------
*& Report  ZRMMPM04R_GI_LIST
*&---------------------------------------------------------------------*
REPORT  zrmmpm04r_gi_list  NO STANDARD PAGE HEADING
                           MESSAGE-ID zmmm.
*------ Type
TYPE-POOLS : slis.               "Abap List View Type

*------ Tables
TABLES : mseg,                   "Document Segment: Material
         mkpf,                   "Header: Material Document
         mara,                   "General Material Data
         makt,                   "Material Descriptions
         marc,                   "Plant Data for Material
         t024d,                  "MRP controllers
         t001w.                  "Plants/Branches

*------ Interal Table
DATA : BEGIN OF it_gi_01 OCCURS 0,
         matnr LIKE mseg-matnr,  "Material number
         maktx LIKE makt-maktx,  "Material Description
         dispo LIKE marc-dispo,  "MRP controller
         bwart LIKE mseg-bwart,  "Movement type
         meins LIKE mseg-meins,  "Base unit of measure
         mtart LIKE mara-mtart,  "Material type

        mengea LIKE mseg-menge,  "Total GI Quantity (261+901..905)
        mengeb LIKE mseg-menge,  "Total GI Quantity (262+902..906)

        menge1 LIKE mseg-menge,  "B/F(GI) Quantity (261)
        menge2 LIKE mseg-menge,  "B/F(GI) Quantity (262)
        mengec LIKE mseg-menge,  "Total Gi Quantity (261-262)

        menge3 LIKE mseg-menge,  "A/S Quantity  (901)
        menge4 LIKE mseg-menge,  "A/S Quantity  (902)
        menged LIKE mseg-menge,  "Total Gi Quantity (901-902)

        menge5 LIKE mseg-menge,  "Quantity  (903)
        menge6 LIKE mseg-menge,  "Quantity  (904)
        mengee LIKE mseg-menge,  "Total Gi Quantity (903-904)

        menge7 LIKE mseg-menge,  "Component Scrap Quantity (905)
        menge8 LIKE mseg-menge,  "Component Scrap Quantity (906)
        mengef LIKE mseg-menge,  "Total Gi Quantity (905-906)

       END OF it_gi_01.

DATA : BEGIN OF it_gi_02 OCCURS 0,
         matnr LIKE mseg-matnr,  "Material number
         menge LIKE mseg-menge,  "Quantity
         meins LIKE mseg-meins,  "Base unit of measure
         maktx LIKE makt-maktx,  "Material Description
         dispo LIKE marc-dispo,  "MRP controller
         bwart LIKE mseg-bwart,  "Movement type
       END OF it_gi_02.

*--- ALV
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line TYPE slis_listheader.

************************************************************************
* macro
************************************************************************
DEFINE append_fieldcat.
  &1 = &1 + 1.
  w_fieldcat-col_pos    = &1.
  w_fieldcat-fieldname  = &2.
  w_fieldcat-ref_fieldname   = &3.
  w_fieldcat-key        = &4.
  w_fieldcat-qfieldname = &5.
  w_fieldcat-cfieldname = &6.
  w_fieldcat-seltext_l  = &7.
  w_fieldcat-seltext_m  = &7.
  w_fieldcat-seltext_s  = &7.
  w_fieldcat-outputlen  = &8.
  w_fieldcat-do_sum    = &9 .
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.

*------ Selection Screen
SELECTION-SCREEN : BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS : s_werks FOR mseg-werks OBLIGATORY, "Plant
                 s_dispo FOR marc-dispo,            "MRP controller
                 s_matnr FOR mseg-matnr,            "Material No
                 s_budat FOR mkpf-budat OBLIGATORY. "Posting date
SELECTION-SCREEN : END OF BLOCK bl1.

*-------At Selection Screen
AT SELECTION-SCREEN.
  CHECK sy-ucomm = 'ONLI'.
  PERFORM check_rtn.
  PERFORM get_data.

*------ Start of selection
START-OF-SELECTION.
  PERFORM display_data.






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
  CLEAR : it_gi_01, it_gi_01[].

*-- get data (mseg+marc+makt+mkpf+mara)
  SELECT b~matnr
         b~menge
         b~meins
         b~bwart
         c~dispo
         e~maktx
                 INTO CORRESPONDING FIELDS OF TABLE it_gi_02
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
                 AND d~mtart IN ('ROH', 'HALB')
                 AND b~bwart IN ('261', '262', '901', '902',
                                 '903', '904', '905', '906')
                 AND e~spras EQ sy-langu.

*          INTO CORRESPONDING FIELDS OF TABLE it_gi_02
*           FROM ( ( ( mseg AS a INNER JOIN marc AS b
*             ON a~matnr EQ b~matnr AND
*                a~werks EQ b~werks ) INNER JOIN makt AS c
*             ON a~matnr EQ c~matnr ) INNER JOIN mkpf AS d
*             ON a~mblnr EQ d~mblnr AND
*                a~mjahr EQ d~mjahr ) INNER JOIN mara AS e
*             ON a~matnr EQ e~matnr
*          WHERE a~werks IN s_werks          "Plant
*            AND b~dispo IN s_dispo          "Person of Contact
*            AND a~matnr IN s_matnr          "Material No
*            AND d~budat IN s_budat          "Date
*            AND e~mtart EQ 'ROH'            "Material Type
*            AND a~bwart IN ('261', '262', '901', '902', '903',
*                            '904', '905', '906'). "Movement Type

  IF sy-subrc NE 0.
    MESSAGE e000(zmmm) WITH text-m04.
  ENDIF.

  PERFORM get_quantity.

ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  get_Quantity
*----------------------------------------------------------------------*
FORM get_quantity.
*---
  DATA : t_quantity_01 LIKE it_gi_01-mengea,
         t_quantity_02 LIKE it_gi_01-mengeb,
         t_quantity_03 LIKE it_gi_01-mengec,
         t_quantity_04 LIKE it_gi_01-menged,
         t_quantity_05 LIKE it_gi_01-mengee,
         t_quantity_06 LIKE it_gi_01-mengef.

  LOOP AT it_gi_02.
    CLEAR : it_gi_01.

*-- move it_gi_02 to it_gi_01

    MOVE : it_gi_02-maktx TO it_gi_01-maktx,
           it_gi_02-dispo TO it_gi_01-dispo,
           it_gi_02-meins TO it_gi_01-meins.

    MOVE : it_gi_02-matnr TO it_gi_01-matnr.

    CASE it_gi_02-bwart.

      WHEN '261'. "Movement Type GI for order
        MOVE : it_gi_02-menge TO it_gi_01-menge1.

      WHEN '262'.    "Movement Type : RE for order
        MOVE : it_gi_02-menge TO it_gi_01-menge2.

      WHEN '901'.    "Movement Type : Component Scrap GI
        MOVE : it_gi_02-menge TO it_gi_01-menge3.

      WHEN '902'.
        MOVE : it_gi_02-menge TO it_gi_01-menge4.

      WHEN '903'.    "Movement Type : Discrepancy GI
        MOVE : it_gi_02-menge TO it_gi_01-menge5.

      WHEN '904'.
        MOVE : it_gi_02-menge TO it_gi_01-menge6.

      WHEN '905'.
        MOVE : it_gi_02-menge TO it_gi_01-menge7.

      WHEN '906'.
        MOVE : it_gi_02-menge TO it_gi_01-menge8.
    ENDCASE.


*-- Quantity Sum (Movement Type 261,-262)
    t_quantity_03 = it_gi_01-menge1 - it_gi_01-menge2.

*-- Quantity Sum (Movement Type 261,-262)
    t_quantity_04 = it_gi_01-menge3 - it_gi_01-menge4.

*-- Quantity Sum (Movement Type 901,-902)
    t_quantity_05 = it_gi_01-menge5 - it_gi_01-menge6.

*-- Quantity Sum (Movement Type 903,-904)
    t_quantity_06 = it_gi_01-menge7 - it_gi_01-menge8.


*-- Quantity Sum (Movement Type 261,..,906)
    t_quantity_01 = t_quantity_03 + t_quantity_04
                  + t_quantity_05 + t_quantity_06.

    MOVE : t_quantity_01 TO it_gi_01-mengea,
           t_quantity_02 TO it_gi_01-mengeb,
           t_quantity_03 TO it_gi_01-mengec,
           t_quantity_04 TO it_gi_01-menged,
           t_quantity_05 TO it_gi_01-mengee,
           t_quantity_06 TO it_gi_01-mengef.
    COLLECT it_gi_01.

  ENDLOOP.
ENDFORM.                    " get_Quantity
*&---------------------------------------------------------------------*
*&      Form  display_data
*----------------------------------------------------------------------*
FORM display_data.
  PERFORM build_fieldcat.                       "build  fieldcat
  PERFORM build_event.                          "build  event
  PERFORM build_sort.                           "build  sort
  PERFORM comment_build USING  w_top_of_page[]. "Header Title
  PERFORM alv_function.                         "ALV function
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*----------------------------------------------------------------------*
FORM build_fieldcat.
  append_fieldcat :
*   postion    field    ref field      key      q-ref    c-ref
*   Text                               leg      SUM

    w_col_pos  'MATNR'  'MATNR'       'X'       ''      ''
    'Material  No'                    '15'      '' ,

    w_col_pos  'MAKTX'  'MAKTX'       'X'       ''      ''
    'Material description'            '30'      '',

    w_col_pos  'DISPO'  'DISPO'       'X'       ''      ''
    'Poc'                             '4'       '' ,

    w_col_pos  'MENGEA' 'MENGE_D'     ''        'MEINS' ''
    'Total GI Qty'                    '10'      '' ,

    w_col_pos  'MENGEC' 'MENGE_D'     ''        'MEINS' ''
    'B/F(GI) Qty'                     '9'       '' ,

    w_col_pos  'MENGED' 'MENGE_D'     ''        'MEINS' ''
    'A/S Qty'                         '9'       '' ,

    w_col_pos  'MENGEE' 'MENGE_D'     ''        'MEINS' ''
    'Qty for Scrapped Vehicle'        '9'       '' ,

    w_col_pos  'MENGEF' 'MENGE_D'     ''        'MEINS' ''
    'Component Scrap'                 '9'       '' .
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
  w_sortcat-spos           = 1.
  w_sortcat-fieldname      = 'DISPO'.     "Person of Contact Sort
  w_sortcat-tabname        = 'IT_GI_01'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.

  w_sortcat-spos           = 2.
  w_sortcat-fieldname      = 'MATNR'.     "Material No. Sort
  w_sortcat-tabname        = 'IT_GI_01'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.


ENDFORM.                    " build_sort
*&---------------------------------------------------------------------*
*&      Form  comment_build
*----------------------------------------------------------------------*
FORM comment_build USING it_top_of_page TYPE slis_t_listheader.

  DATA: ls_line TYPE slis_listheader,
         l_manager(50),
         l_date(50),
         l_list(50),
         l_dsnam LIKE t024d-dsnam,
         l_h_dsnam LIKE t024d-dsnam,
         l_ldate(10),
         l_hdate(10).

*-------------- HEADER Title
* Listen?erschrift: Typ H
  CLEAR ls_line.
  ls_line-typ  = 'H'.
* LS_LINE-KEY:  not used for this type
  ls_line-info = text-h01.
  APPEND ls_line TO it_top_of_page.

*--------------- HEADER Title (Parameter & Select-Option)
*-- S_werks (Plant)
  ls_line-typ  = 'S'.
  ls_line-key  = 'Plant :'.
  CONCATENATE  'From :' s_werks-low  'TO :' s_werks-high INTO l_list.
  ls_line-info = l_list.
  APPEND ls_line TO it_top_of_page.

*-- S_dispo (Person of Contact)
  ls_line-typ  = 'S'.
  ls_line-key  = 'Person of contact :'.
  CONCATENATE  'From :' s_dispo-low  'TO :' s_dispo-high INTO l_list.
  ls_line-info = l_list.
  APPEND ls_line TO it_top_of_page.

*-- S_MATNR (Material number)
  ls_line-typ  = 'S'.
  ls_line-key  = 'Material No : '.
  CONCATENATE  'From :' s_matnr-low  'TO :' s_matnr-high INTO l_list.
  ls_line-info = l_list.
  APPEND ls_line TO it_top_of_page.

*-- S_budat (Date)
  ls_line-typ  = 'S'.
  ls_line-key  = 'Date :'.
  CONCATENATE  'From :' s_budat-low  'TO :' s_budat-high INTO l_list.
  ls_line-info = l_list.
  APPEND ls_line TO it_top_of_page.
ENDFORM.                    " comment_build
*&---------------------------------------------------------------------*
*&      Form  alv_function
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
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_bypassing_buffer      = 'X'
            i_callback_program      = w_program

*            i_callback_user_command = 'USER_COMMAND'
            it_fieldcat             = w_fieldcat[]
            it_sort                 = w_sortcat[]
            i_save                  = 'A'
            it_events               = w_eventcat[]
            is_print                = l_print_p
       TABLES
            t_outtab                = it_gi_01
       EXCEPTIONS
            program_error           = 1
            OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " alv_function
*&---------------------------------------------------------------------*
*&      Form  TOP-OF-PAGE
*----------------------------------------------------------------------*
FORM top_of_page .
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'Z_HYUNDAI_LOGO'
*            i_logo             = 'ZZZZZ'
            it_list_commentary = w_top_of_page.

ENDFORM.                    " TOP-OF-PAGE
