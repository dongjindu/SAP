
************************************************************************
* Program Name      : ZRMMPM17_LINE_INVENTORY
* Author            : Jaesung, Lee
* Creation Date     : 2003.08.25.
* Specifications By : Jaesung, Lee
* Pattern           : Report 1-1
* Development Request No : UD1K901849
* Addl Documentation:
* Description       : Line Inventory Report
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 2003.10.16 Hakchin Kim  UD1K901849
*
*
************************************************************************

REPORT  zrmmpm16_line_inventory       .

************************************************************************
* Data
************************************************************************
TYPE-POOLS : slis.

TABLES : mara,
         marc,       "Plant Data for Material
         mard,       "Storage Location Data for Material
         ekpo,
         ztmm_mast,  "Supply to Line Master Table
         stpo,       "BOM item
         lqua,       "Quants
         eket.       "Scheduling Agreement Schedule Lines


DATA: BEGIN OF it_tab OCCURS 10,
      matnr  LIKE eban-matnr, " Material number
      maktx  LIKE makt-maktx, " Material description
      bismt  LIKE mara-bismt, " Old material number(ALC code)
      werks  LIKE mard-werks, " Plant
      lgort  LIKE mard-lgort, " Storage location
      lgbzo  LIKE ekpo-lgbzo, " group gate(Automotive) Unloading Point
      feedr  LIKE ztmm_mast-feedr, " Feeder
      zline  LIKE ztmm_mast-zline, " Line
      works  LIKE ztmm_mast-works, " WorkStation
      rh_lh  LIKE ztmm_mast-rh_lh, " RH/LH


      menge  LIKE stpo-menge, " Component quantity
      meins  LIKE stpo-meins, " unit
      profl  LIKE mara-profl, " surce (LP/KD)
      bstmi  LIKE marc-bstmi, " Qty/con
      plan(10)              , " plan
      diff(10)              , " differce
      labst  LIKE mard-labst, " inventory
      gesme  LIKE lqua-gesme, " line stock
      wstok  LIKE lqua-gesme, " Warehouse stock
      ccbin  LIKE lqua-gesme, " cc-bin stock
      ccrack LIKE lqua-gesme, " cc-rack stock
      cystok LIKE lqua-gesme, " cy stock
      mobile LIKE lqua-gesme, " mobile stock
      duin   LIKE eket-menge, " Due in
      dispo  LIKE marc-dispo, " mrp controler
      END OF it_tab.

DATA: BEGIN OF it_lp OCCURS 10,

      vbeln LIKE likp-vbeln,
      posnr LIKE lips-posnr,
      tduhr LIKE likp-tduhr,
      lfuhr LIKE likp-lfuhr,
      traid LIKE likp-traid,
      lgtor LIKE likp-lgtor,
      duin  LIKE eket-menge,
      MEINS LIKE MARA-MEINS,
      MATNR LIKE LIPS-MATNR,

      END OF it_lp .


DATA : w_fieldcat    TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat    TYPE slis_t_event WITH HEADER LINE,
       w_selfield    TYPE slis_selfield,
       w_sortcat     TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos     TYPE i,
       w_program     LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line        TYPE slis_listheader.





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
  w_fieldcat-no_out     = &9.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.


************************************************************************
* SELECTION SCREEN
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
PARAMETERS     : p_werks LIKE mard-werks OBLIGATORY
                                         DEFAULT 'P001'.     " plant
SELECT-OPTIONS : s_lgort FOR mard-lgort,       " Storage Location
                 s_lgtyp FOR lqua-lgtyp,       " storage type
                 s_feedr FOR ztmm_mast-feedr,  " feeder
                 s_zline FOR ztmm_mast-zline,  " line
                 s_works FOR ztmm_mast-works,  " w/s.
                 s_profl FOR mara-profl.


PARAMETERS :    p_inven(1) TYPE c  DEFAULT '7'.



SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002
                                    NO INTERVALS.
SELECTION-SCREEN COMMENT /6(20) text-t01.
SELECTION-SCREEN COMMENT /6(20) text-t02.
SELECTION-SCREEN COMMENT /6(20) text-t03.
SELECTION-SCREEN COMMENT /6(20) text-t04.
SELECTION-SCREEN COMMENT /6(20) text-t05.
SELECTION-SCREEN COMMENT /6(20) text-t06.
SELECTION-SCREEN COMMENT /6(20) text-t07.

SELECTION-SCREEN END OF BLOCK block2.

************************************************************************
* INITIALIZATION
************************************************************************

************************************************************************
* AT SELECTION-SCREEN
************************************************************************

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_inven.
  PERFORM f4_help.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.

  PERFORM data_selection.
  PERFORM data_calculation.

  PERFORM list_write.
*&---------------------------------------------------------------------*
*&      Form  data_selection
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
* data selection mard,mara,ztmm_mast, etc
*----------------------------------------------------------------------*
FORM data_selection.

  DATA : BEGIN OF it_po OCCURS 10,
         matnr LIKE mara-matnr,
         werks LIKE mard-werks,
         lgort LIKE mard-lgort,
         lgbzo LIKE ekpo-lgbzo,
         END OF it_po.

  DATA : BEGIN OF it_mast OCCURS 10,
         matnr  LIKE ztmm_mast-matnr,
         feedr  LIKE ztmm_mast-feedr, " Feeder
         zline  LIKE ztmm_mast-zline, " Line
         works  LIKE ztmm_mast-works, " WorkStation
         rh_lh  LIKE ztmm_mast-rh_lh, " RH/LH
         END OF it_mast.

  DATA : BEGIN OF st_open ,
*          matnr LIKE mdbs-matnr,
*          werks LIKE mdbs-werks,
*          lgort LIKE mdbs-lgort,
          menge LIKE mdbs-menge,
          wemng LIKE mdbs-wemng,
          END OF st_open.


  SELECT d~matnr t~maktx a~bismt a~profl c~bstmi a~meins d~labst
         c~dispo d~werks d~lgort

  INTO  CORRESPONDING FIELDS OF TABLE it_tab

  FROM ( ( ( mard AS d INNER JOIN mara AS a
             ON d~matnr = a~matnr   )
                                        INNER JOIN makt AS t ON
                                        t~matnr = d~matnr AND
                                        t~spras = sy-langu   )

                                        INNER JOIN marc AS c ON
                                        d~matnr = c~matnr AND
                                        d~werks = c~werks )

          WHERE d~werks =  p_werks
            AND d~lgort IN s_lgort
            AND a~profl IN s_profl.


*--- Group gate read

  SELECT p~matnr p~werks p~lgort p~lgbzo
         INTO CORRESPONDING FIELDS OF TABLE it_po
         FROM ekko AS k INNER JOIN ekpo AS p
               ON k~ebeln = p~ebeln
         FOR ALL ENTRIES IN it_tab
         WHERE k~bstyp = 'L'
           AND p~matnr = it_tab-matnr.

*--- Supply to Line Master Table read

  SELECT matnr feedr zline works rh_lh INTO TABLE it_mast
         FROM ztmm_mast
         FOR ALL ENTRIES IN it_tab
         WHERE matnr = it_tab-matnr
           and werks = it_tab-werks.


*--- line stock



  LOOP AT it_tab .


    SELECT  SUM( menge ) AS menge SUM( wemng ) AS wemng
         INTO   st_open
         FROM mdbs
         WHERE matnr = it_tab-matnr
           AND werks = it_tab-werks
*          and lgort = it_tab-lgort
           AND loekz = ' '
           AND elikz = ' '
*           GROUP by matnr werks
           .

    IF sy-subrc EQ 0.
      it_tab-duin = st_open-menge - st_open-wemng.
      CLEAR st_open.
    ENDIF.

    READ TABLE it_po WITH KEY werks = it_tab-werks
                              matnr = it_tab-matnr.

    IF sy-subrc EQ 0.
      it_tab-lgbzo = it_po-lgbzo.

    ENDIF.

    READ TABLE it_mast WITH KEY matnr = it_tab-matnr.

    IF sy-subrc EQ 0.
      MOVE : it_mast-feedr TO it_tab-feedr,
             it_mast-zline TO it_tab-zline,
             it_mast-works TO it_tab-works,
             it_mast-rh_lh TO it_tab-rh_lh.

    ENDIF.

*      wstok
*      ccbin  LIKE lqua-gesme, " cc-bin stock
*      ccrack LIKE lqua-gesme, " cc-rack stock
*      cystok LIKE lqua-gesme, " cy stock


    IF it_tab-lgort = 'P400'.

      SELECT SUM( gesme ) INTO it_tab-wstok
           FROM lqua
           WHERE matnr = it_tab-matnr
             AND werks = p_werks
             AND lgort IN s_lgort
             AND lgtyp IN ('500','510','520').


      SELECT SUM( gesme ) INTO it_tab-ccbin
           FROM lqua
           WHERE matnr = it_tab-matnr
             AND werks = p_werks
             AND lgort IN s_lgort
             AND lgtyp IN ('300').


      SELECT SUM( gesme ) INTO it_tab-ccrack
           FROM lqua
           WHERE matnr = it_tab-matnr
             AND werks = p_werks
             AND lgort IN s_lgort
             AND lgtyp IN ('200').



      SELECT SUM( gesme ) INTO it_tab-cystok
           FROM lqua
           WHERE matnr = it_tab-matnr
             AND werks = p_werks
             AND lgort IN s_lgort
             AND lgtyp IN ('100').

      PERFORM line_inventory_cal.


    ENDIF.


*if p_int


    MODIFY it_tab.

  ENDLOOP.


ENDFORM.                    " data_selection
*&---------------------------------------------------------------------*
*&      Form  data_calculation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_calculation.

ENDFORM.                    " data_calculation
*&---------------------------------------------------------------------*
*&      Form  f4_help
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_help.

  DATA : BEGIN OF it_dynpfields OCCURS 0.
          INCLUDE STRUCTURE dynpread.
  DATA : END OF it_dynpfields.

  DATA : BEGIN OF value_tab OCCURS 0,
           inven TYPE aankz,
           name LIKE lfa1-name1,
         END OF value_tab.

  CLEAR : it_dynpfields, value_tab.
  REFRESH : it_dynpfields, value_tab.


  value_tab-inven = '1'.
  value_tab-name  = '1)'.
  APPEND value_tab.

  value_tab-inven = '2'.
  value_tab-name  = '1) + 2)'.
  APPEND value_tab.

  value_tab-inven = '3'.
  value_tab-name  = '1) + 2) + 3)'.
  APPEND value_tab.


  value_tab-inven = '4'.
  value_tab-name  = '1) + 2) + 3) + 4)'.
  APPEND value_tab.

  value_tab-inven = '5'.
  value_tab-name  = '1) + 2) + 3) + 4) + 5)'.
  APPEND value_tab.

  value_tab-inven = '6'.
  value_tab-name  = '1) + 2) + 3) + 4) + 5) + 6)'.
  APPEND value_tab.

  value_tab-inven = '7'.
  value_tab-name  = '1) + 2) + 3) + 4) + 5) + 6) + 7)'.
  APPEND value_tab.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            retfield        = 'INVEN'
            dynpprog =
               'ZRMMPM16R_LINE_INVENTORY' " <- Report Name
            dynpnr          = '1000'  "<- Screen Number
            dynprofield     = 'P_INVEN'  "<- Input Field Name
            window_title    = 'Line Stock'  "<- Description
            value_org       = 'S'
       TABLES
            value_tab       = value_tab
       EXCEPTIONS
            parameter_error = 1.

ENDFORM.                                                    " f4_help
*&---------------------------------------------------------------------*
*&      Form  line_inventory_cal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM line_inventory_cal.
  CLEAR : it_tab-labst.

  CASE p_inven.
    WHEN '1'.
      it_tab-labst = it_tab-gesme.
    WHEN '2'.
      it_tab-labst = it_tab-gesme + it_tab-wstok .
    WHEN '3'.
      it_tab-labst = it_tab-gesme + it_tab-wstok + it_tab-ccbin.
    WHEN '4'.
      it_tab-labst = it_tab-gesme + it_tab-wstok + it_tab-ccbin +
                     it_tab-ccrack.

    WHEN '5'.
      it_tab-labst = it_tab-gesme  + it_tab-wstok + it_tab-ccbin +
                     it_tab-ccrack + it_tab-cystok.

    WHEN '6'.
      it_tab-labst = it_tab-gesme  + it_tab-wstok   + it_tab-ccbin +
                     it_tab-ccrack + it_tab-cystok + it_tab-mobile.
    WHEN '7'.
      it_tab-labst = it_tab-gesme  + it_tab-wstok   + it_tab-ccbin +
                     it_tab-ccrack + it_tab-cystok + it_tab-mobile
                      + it_tab-duin.


  ENDCASE.
ENDFORM.                    " line_inventory_cal
*&---------------------------------------------------------------------*
*&      Form  list_write
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list_write.
  DATA : l_count TYPE i.
  CLEAR l_count.
  DESCRIBE TABLE it_tab LINES l_count .
  IF l_count = 0.
    MESSAGE s420(me) ." with text-002.
    STOP.
  ENDIF.

  PERFORM build_fieldcat.
  PERFORM build_event.
*  PERFORM build_sort.
  PERFORM comment_build USING  w_top_of_page[].
  PERFORM alv_function.

ENDFORM.                    " list_write
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat.

  append_fieldcat :
*   postion    field    ref field     key q-ref   c-ref
*   Text                leg

    w_col_pos 'MATNR'  'MATNR'        'X' ''      ''
    'Material'                        '18' '',
    w_col_pos 'MAKTX'  'MAKTX'        '' ''      ''
    'Material description'            '38' '',
    w_col_pos 'BISMT'  'BISMT'        '' ''      ''
    'ALC code'                        '18' '',
    w_col_pos 'WERKS'  'WERKS_D'      '' ''      ''
    'Plant'                           '4' '',
    w_col_pos 'LGORT'  'LGORT_D'      '' ''      ''
    'SLoc'                            '4' '',
    w_col_pos 'LGBZO'  'UNLPT'        '' ''      ''
    'Group gate'                      '10' '',
    w_col_pos 'FEEDR'  'ZFEEDER'      '' ''      ''
    'Feeder'                          '5' '',

    w_col_pos 'ZLINE'  'ZDLINE'       '' ''      ''
    'Line'                            '1' '',
    w_col_pos 'WORKS'  'ZWORKST'      '' ''      ''
    'W/S'                             '5' '',
    w_col_pos 'RH_LH'  'ZRH_LH'       '' ''      ''
    'RH/LH'                           '2' '',
    w_col_pos 'MEINS'  'MEINS'        '' ''      ''
    'Un'                              '3' '',
    w_col_pos 'MENGE'  'MENGE'        '' 'MEINS'      ''
    'Usage'                           '3' '',
    w_col_pos 'PROFL'  'ADGE_PROFL'   '' 'MEINS '      ''
    'Source'                          '3' '',
    w_col_pos 'BSTMI'  'BSTMI'        '' 'MEINS'      ''
    'Qty/con'                         '18' '',
    w_col_pos 'LABST'  'LABST'        '' 'MEINS'      ''
    'Inventory'                       '18' '',
    w_col_pos 'GESME'  'GESME'        '' 'MEINS'      ''
    'Line Stock'                      '18' '',
    w_col_pos 'WSTOK'  'GESME'        '' 'MEINS'      ''
    'Warehouse stock'                 '18' '',
    w_col_pos 'CCBIN'  'GESME'        '' 'MEINS'      ''
    'CC-bin stock'                    '18' '',
    w_col_pos 'CCRACK'  'GESME'       '' 'MEINS'      ''
    'CC-rack stock'                   '18' '',
    w_col_pos 'CYSTOK'  'GESME'       '' 'MEINS'      ''
    'CY stock'                        '18' '',
    w_col_pos 'MOBILE'  'GESME'       '' 'MEINS'      ''
    'Mobile stock'                    '18' '',
    w_col_pos 'DUIN'  'GESME'         '' 'MEINS'      ''
    'Due in'                          '18' '',
    w_col_pos 'DISPO'  'GESME'        '' 'MEINS'      ''
    'Manager'                         '8' ''.


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
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
        l_manager(50),
        l_date(50),
        l_list(50),
        l_dsnam LIKE t024d-dsnam,
        l_h_dsnam LIKE t024d-dsnam,
        l_ldate(10),
        l_hdate(10),
        l_name1 LIKE t001w-name1.


*SELECT-OPTIONS : s_lgort FOR mard-lgort,       " Storage Location
*                 s_lgtyp FOR lqua-lgtyp,       " storage type
*                 s_feedr FOR ztmm_mast-feedr,  " feeder
*                 s_zline FOR ztmm_mast-zline,  " line
*                 s_works FOR ztmm_mast-works,  " w/s.
*                 S_PROFL FOR MARA-PROFL.
*
*

*
* Listenüberschrift: Typ H
  CLEAR ls_line.
  ls_line-typ  = 'H'.
* LS_LINE-KEY:  not used for this type
  ls_line-info = text-100.
  APPEND ls_line TO lt_top_of_page.

* Kopfinfo: Typ S



* Material Type
  ls_line-key  = 'Plant: '.
  ls_line-typ  = 'S'.
  SELECT SINGLE name1 INTO l_name1 FROM t001w
         WHERE werks = p_werks.

  WRITE : p_werks   TO l_list(6),
          l_name1    TO l_list+7(30).
  ls_line-info = l_list.
  APPEND ls_line TO lt_top_of_page.

* Storage Location
  ls_line-key  = 'Storage Location: '.
  READ TABLE s_lgort INDEX 1.
  IF sy-subrc EQ 0.
    ls_line-typ  = 'S'.

    CONCATENATE 'FROM: ' s_lgort-low '  TO: ' s_lgort-high INTO l_list.
    ls_line-info = l_list.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

* storage type
  ls_line-key  = 'Storage type: '.
  READ TABLE s_lgtyp INDEX 1.
  IF sy-subrc EQ 0.
    ls_line-typ  = 'S'.

    CONCATENATE 'FROM: ' s_lgtyp-low '  TO: ' s_lgtyp-high INTO l_list.
    ls_line-info = l_list.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

* storage feeder
  ls_line-key  = 'Feeder   : '.
  READ TABLE s_feedr INDEX 1.
  IF sy-subrc EQ 0.
    ls_line-typ  = 'S'.

    CONCATENATE 'FROM: ' s_feedr-low '  TO: ' s_feedr-high INTO l_list.
    ls_line-info = l_list.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

* line
  ls_line-key  = 'Line     : '.
  READ TABLE s_zline INDEX 1.
  IF sy-subrc EQ 0.
    ls_line-typ  = 'S'.

    CONCATENATE 'FROM: ' s_zline-low '  TO: ' s_zline-high INTO l_list.
    ls_line-info = l_list.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

* workstation
  ls_line-key  = 'W/S       : '.
  READ TABLE s_works INDEX 1.
  IF sy-subrc EQ 0.
    ls_line-typ  = 'S'.

    CONCATENATE 'FROM: ' s_works-low '  TO: ' s_works-high INTO l_list.
    ls_line-info = l_list.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

* workstation
  ls_line-key  = 'Source    : '.
  READ TABLE s_profl INDEX 1.
  IF sy-subrc EQ 0.
    ls_line-typ  = 'S'.

    CONCATENATE 'FROM: ' s_profl-low '  TO: ' s_profl-high INTO l_list.
    ls_line-info = l_list.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

  ls_line-key  = 'Storage Type: '.
  IF NOT p_inven IS INITIAL.
    ls_line-typ  = 'S'.

    ls_line-info = p_inven .
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

  CLEAR ls_line.
*
ENDFORM.                    " comment_build
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
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
     i_bypassing_buffer                = 'X'
*   I_BUFFER_ACTIVE                   = ' '
     i_callback_program                = w_program
*   I_CALLBACK_PF_STATUS_SET          = ' '
     i_callback_user_command           = 'USER_COMMAND'
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*  I_GRID_TITLE                      = text-001
*   I_GRID_SETTINGS                   =
*   IS_LAYOUT                         =
     it_fieldcat                       = w_fieldcat[]
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
     it_sort                           = w_sortcat[]
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
      i_save                            = 'A'
*   IS_VARIANT                        =
     it_events                         = w_eventcat[]
*   IT_EVENT_EXIT                     =
     is_print                          = l_print_p
*   IS_REPREP_ID                      =
*   I_SCREEN_START_COLUMN             = 0
*   I_SCREEN_START_LINE               = 0
*   I_SCREEN_END_COLUMN               = 0
*   I_SCREEN_END_LINE                 = 0
*   IT_ALV_GRAPHICS                   =
*   IT_ADD_FIELDCAT                   =
*   IT_HYPERLINK                      =
*   I_HTML_HEIGHT_TOP                 =
*   I_HTML_HEIGHT_END                 =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab                          = it_tab
   EXCEPTIONS
     program_error                     = 1
     OTHERS                            = 2
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " alv_function

*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
*
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'Z_HYUNDAI_LOGO'
*            i_logo             = 'ENJOYSAP_LOGO'
            it_list_commentary = w_top_of_page.
ENDFORM.
*&---------------------------------------------------------------------*
*&      user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM user_command USING ucomm LIKE sy-ucomm
                       selfield TYPE slis_selfield.
  CASE ucomm.
    WHEN 'REGIST'.
      CALL FUNCTION 'REUSE_ALV_LIST_LAYOUT_INFO_SET'
        EXPORTING
*         IS_LAYOUT            =
          it_fieldcat          = w_fieldcat[]
*         IT_SORT              =
*         IT_FILTER            =
*         IS_LIST_SCROLL       =
                .

    WHEN '&IC1'. " Double Click

      PERFORM detail_list USING selfield.
*      CALL TRANSACTION 'ME21N' .
*       CALL DIALOG 'ME_CREATE_PO'

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  detail_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SELFIELD  text
*----------------------------------------------------------------------*
FORM detail_list USING  selfield TYPE slis_selfield.

  READ TABLE it_tab INDEX selfield-tabindex.
  CHECK sy-subrc EQ 0.

  SELECT p~vbeln k~posnr p~tduhr p~lfuhr p~traid p~lgtor
         INTO TABLE it_lp
         FROM likp AS p  INNER JOIN lips AS k
              ON  k~vbeln = p~vbeln

         WHERE p~lfart = 'EL'
*           AND p~lfdat = sy-datum
           AND k~matnr = it_tab-matnr .


  it_lp-duin = it_tab-duin.

  MODIFY TABLE it_lp  TRANSPORTING duin MEINS MATNR .


  PERFORM detail_list_write.


ENDFORM.                    " detail_list
*&---------------------------------------------------------------------*
*&      Form  detail_list_write
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM detail_list_write.

  PERFORM build_fieldcat_detail.
  PERFORM list_write_detail.


ENDFORM.                    " detail_list_write
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat_detail.

  CLEAR : w_fieldcat[].

  append_fieldcat :
*   postion    field    ref field     key q-ref   c-ref
*   Text                leg

    w_col_pos 'VBELN'  'VBELN'        'X' ''      ''
    'ASN num'                         '10' '',
    w_col_pos 'POSNR'  'POSNR'        '' ''      ''
    'Item'                            '6' '',
    w_col_pos 'TDUHR'  'TDUHR'        '' ''      ''
    'ETD'                             '8' '',
    w_col_pos 'LFUHR'  'LFUHR'        '' ''      ''
    'ETA'                             '8' '',

    w_col_pos 'TRAID'  'TRAID'        '' ''      ''
    'Trailer No'                      '20' '',
    w_col_pos 'LGTOR'  'LGTOR'        '' ''      ''
    'Gate No'                         '3' '',
    w_col_pos 'MATNR'  'MATNR'        '' ''      ''
    'Material NO'                     '18' '',
    w_col_pos 'MEINS'  'MEINS'        '' ''      ''
    'Um'                          '3' '',
    w_col_pos 'DUEIN'  'MANGE'        '' 'MEINS'      ''
    'Due-in'                          '20' ''.

ENDFORM.                    " build_fieldcat_detail
*&---------------------------------------------------------------------*
*&      Form  list_write_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list_write_detail.
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
            i_bypassing_buffer = 'X'
            i_callback_program = w_program
            i_grid_title       = text-004
            it_fieldcat        = w_fieldcat[]
            i_save             = 'A'
            it_events          = w_eventcat[]
            is_print           = l_print_p
       TABLES
            t_outtab           = it_LP
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " list_write_detail
