************************************************************************
* Program Name      : ZRNNON18R_PDFR
* Author            : SeungJae, Lee
* Creation Date     : 2003.08.25.
* Specifications By : SeungJae, Lee
* Development Request No : UD1K901842
* Addl Documentation:
* Description       : Production & Feeding Record
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT zrmmpm18r_pdfr NO STANDARD PAGE HEADING  MESSAGE-ID zmmm.

TYPE-POOLS: slis.

TABLES: ztmm_mast, " Supply to Line Master Table
        mara,      " Material Master
        makt,      " Material Decsriptions
        marc,      " Plant Data for Material
        mard,      " Storage Location Data for Material
        t001w,     " Plant
        t001l,     " Storage location
        t301,      " WM Storage Types
        t024d,     " Manager(MRP Controller)
        tdg41,     " DG indicator profiles for material master
        lqua,      " Quants
        mlgt,      " Material Data for Each Storage Type
        stpo,      " BOM Item
        mlgn.      " Material Data for Each Warehouse Number

********************INTERNAL TABLE*******************************
*     MAIN TABLE
DATA: BEGIN OF it_main OCCURS 0,
         matnr     LIKE mara-matnr,      " Material No.
         werks     LIKE mard-werks,      " Plant
         lgort     LIKE mard-lgort,      " Storage Location
         bismt     LIKE mara-bismt,      " ALC Code
         profl     LIKE mara-profl,      " Source(KD/LP)
         maktx     LIKE makt-maktx,      " Material Desc.
         meins     LIKE lqua-meins,      " Base unit Of Measure
         bstma     LIKE marc-bstma,      " QTY/Container
         lvsme     LIKE mlgn-lvsme,      " Warehouse Unit
         fstma     LIKE marc-bstma,      " Feeding QTY
         dispo     LIKE marc-dispo,      " Manager
         feedr     LIKE ztmm_mast-feedr, " Feeder
         zline     LIKE ztmm_mast-zline, " Line
         works     LIKE ztmm_mast-works, " Workstation
         rh_lh     LIKE ztmm_mast-rh_lh, " LH/RH
         lgtyp     LIKE lqua-lgtyp,      " Warehouse
         lgpla     LIKE lqua-lgpla,      " Bin Location
         ls_gesme  TYPE summe_mng,       " Line Stock
         ws_gesme  TYPE summe_mng,       " Warehouse Stock
         cb_gesme  TYPE summe_mng,       " CC-Bin Stock
         cr_gesme  TYPE summe_mng,       " CC-Rack Stock
         cy_gesme  TYPE summe_mng,       " SY Stock
         sum_gesme TYPE summe_mng,       " Total Stock
      END   OF it_main.

*     Material Master
DATA: BEGIN OF it_mara OCCURS 0,
         matnr LIKE mara-matnr,       " Material No.
         werks LIKE mard-werks,       " Plant
         lgort LIKE mard-lgort,       " Storage location
         labst LIKE mard-labst,       " Valuated stock
         bismt LIKE mara-bismt,       " ALC Code
         profl LIKE mara-profl,       " Source
         maktx LIKE makt-maktx,       " Material Desc.
         bstma LIKE marc-bstma,       " Maximum lot size(QTY/Container)
         dispo LIKE marc-dispo,       " Manager
         feedr LIKE ztmm_mast-feedr,  " Feeder
         zline LIKE ztmm_mast-zline,  " Line
         works LIKE ztmm_mast-works,  " Workstation
         rh_lh LIKE ztmm_mast-rh_lh,  " LH/RH
      END   OF it_mara.

*     Material number
DATA: BEGIN OF it_matnr OCCURS 0,
        matnr LIKE mara-matnr,        " Material No.
        werks LIKE mard-werks,        " Plant
      END   OF it_matnr.

*     Material for Each Storage Types
DATA: BEGIN OF it_mlgt OCCURS 0,
        matnr LIKE mlgt-matnr,        " Material No.
        lgnum LIKE mlgt-lgnum,        " Warehouse No.
        lgtyp LIKE mlgt-lgtyp,        " Storage Types
        lgpla LIKE mlgt-lgpla,        " Storage Bin
      END   OF it_mlgt.

*     Warehouse Unit
DATA: BEGIN OF it_mlgn OCCURS 0,
        matnr LIKE mlgn-matnr,        " Material No.
        lgnum LIKE mlgn-lgnum,        " Warehouse No.
        lvsme LIKE mlgn-lvsme,        " Warehouse Unit
        umrez LIKE marm-umrez,
        umren LIKE marm-umren,
      END   OF it_mlgn.

*     Warehouse Stock
DATA: BEGIN OF it_lqua OCCURS 0,
        werks LIKE lqua-werks,        " Plant
        matnr LIKE lqua-matnr,        " Material no
        lgtyp LIKE lqua-lgtyp,        " Storage Type
        lgpla LIKE lqua-lgpla,        " Bin Location
        meins LIKE lqua-meins,        " Base unit Of Measure
        gesme LIKE lqua-gesme,        " Total Quantity
      END   OF it_lqua.

****************************Others***********************************
*    ALV Structure
DATA: rep         LIKE sy-repid,
      wa_layout   TYPE slis_layout_alv,
      it_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      it_sort     TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      wa_print    TYPE  slis_print_alv.

*    Top-Of-Page
DATA: gt_list_top_of_page TYPE slis_t_listheader WITH HEADER LINE,
      p_text(255).

CONSTANTS: c_p400 LIKE t001l-lgort VALUE 'P400',
           c_100(3) VALUE '100',
           c_200(3) VALUE '200',
           c_300(3) VALUE '300',
           c_400(3) VALUE '400',
           c_410(3) VALUE '410',
           c_420(3) VALUE '420',
           c_430(3) VALUE '430',
           c_500(3) VALUE '500',
           c_510(3) VALUE '510',
           c_520(3) VALUE '520'.

*********Selection Screen***********************************************
SELECTION-SCREEN BEGIN OF BLOCK sel1 WITH FRAME TITLE text-t01.
*SELECT-OPTIONS: s_matnr FOR mara-matnr. "for test
*PARAMETERS     : p_date  LIKE sy-datum MODIF ID g1.
SELECT-OPTIONS : s_werks FOR t001w-werks OBLIGATORY, " Plant
                 s_lgort FOR t001l-lgort OBLIGATORY, " Storage Location
                 s_lgtyp FOR t301-lgtyp  OBLIGATORY, " Storage Type
                 s_dispo FOR t024d-dispo,     " Manager(MRP Controller)
                 s_feedr FOR ztmm_mast-feedr, " Feeder
                 s_zline FOR ztmm_mast-zline, " Line
                 s_works FOR ztmm_mast-works, " Workstation
                 s_profl FOR tdg41-profl.
"Source(Dangerous goods indicator profile)
SELECTION-SCREEN END   OF BLOCK sel1.

*********AT SELECTION-SCREEN OUTPUT*************************************
*AT SELECTION-SCREEN OUTPUT.
*  p_date = sy-datum.
*  LOOP AT screen.
*    check screen-group1 = 'G1'.
*    screen-input = '0'.
*    MODIFY SCREEN.
*  ENDLOOP.

*********At Selection Screen********************************************
AT SELECTION-SCREEN.
  PERFORM check_select-options.
  PERFORM get_data.

*********Start Of Selection*********************************************
START-OF-SELECTION.
  PERFORM build_data.

*********End Of Selection***********************************************
END-OF-SELECTION.
  PERFORM display_list_first.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

  PERFORM get_material_master.
  CHECK NOT it_mara[] IS INITIAL.
  PERFORM get_warehouse_unit.
  PERFORM get_material_storagetype.
  PERFORM get_warehouse_stock.

*  DATA: BEGIN OF IT_STPO OCCURS 0,
*          MATNR LIKE MAST-MATNR,
*          WERKS LIKE MAST-WERKS,
*          STPOZ LIKE STPO-STPOZ,
*          MEINS LIKE STPO-MEINS,
*          MENGE LIKE STPO-MENGE,
*        END   OF IT_STPO.
*  SELECT *
*    FROM MAST AS A INNER JOIN STPO AS P
*      ON A~STLNR = P~STLNR "AND
**         A~STLKN = P~STLKN
*     FOR ALL ENTRIES IN IT_MATNR
*   WHERE A~MATNR = IT_MATNR-MATNR
*     AND A~WERKS = IT_MATNR-WERKS.

ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  display_list_first
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_list_first.
  CLEAR: rep.
  rep = sy-repid.

  wa_layout-colwidth_optimize = 'X'.
  wa_print-no_print_selinfos  = 'X'.
  wa_print-no_coverpage       = 'X'.
  wa_print-no_new_page        = 'X'.
  wa_print-reserve_lines      = 2.
  wa_print-no_print_listinfos = 'X'.

  PERFORM build_fieldcat.
  PERFORM build_top_of_page.
  PERFORM build_sort.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK                 = ' '
      i_bypassing_buffer                = 'X'
      i_buffer_active                   = 'X'
      i_callback_program                = rep
*     I_CALLBACK_PF_STATUS_SET          = ' '
      i_callback_user_command           = 'USER_COMMAND'
      i_callback_top_of_page            = 'TOP_OF_PAGE'
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME                  =
*     I_BACKGROUND_ID                   = ' '
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
      is_layout                         = wa_layout
      it_fieldcat                       = it_fieldcat[]
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
      it_sort                           = it_sort[]
*     IT_FILTER                         =
*     IS_SEL_HIDE                       =
      i_default                         = 'X'
      i_save                            = 'A'
*     IS_VARIANT                        =
*     IT_EVENTS                         =
*     IT_EVENT_EXIT                     =
      is_print                          = wa_print
*     IS_REPREP_ID                      =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE                 = 0
*     IT_ALV_GRAPHICS                   =
*     IT_ADD_FIELDCAT                   =
*     IT_HYPERLINK                      =
*     I_HTML_HEIGHT_TOP                 =
*     I_HTML_HEIGHT_END                 =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab                          = it_main
   EXCEPTIONS
     program_error                     = 1
     OTHERS                            = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " display_list_first
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*            i_logo             = 'IDES_LOGO'
            it_list_commentary = gt_list_top_of_page[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat.
  CLEAR   it_fieldcat.
  REFRESH it_fieldcat.
  CHECK it_fieldcat[] IS INITIAL.
*PERFORM append_fieldcat USING  p_fieldname
*                               p_tabname
*                               p_outputlen
*                               p_text_l
*                               p_text_m
*                               p_text_s
*                               p_datatype
*                               p_key
*                               p_no_out
*                               p_unit
*                               p_currency
*                               p_text_field
*                               p_do_sum.
  PERFORM append_fieldcat USING 'MATNR'
                                'IT_MAIN'
                                18
                                'Material No'
                                'Material No'
                                'Material No'
                                'CHAR'
                                'X'
                                ''
                                ''
                                ''
                                'MAKTX'
                                ''.
  PERFORM append_fieldcat USING 'BISMT'
                                'IT_MAIN'
                                18
                                'ALC Code'
                                'ALC Code'
                                'ALC Code'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''
                                ''.
  PERFORM append_fieldcat USING 'MAKTX'
                                'IT_MAIN'
                                40
                                'Material description'
                                'Material description'
                                'Material desc'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''
                                ''.
  PERFORM append_fieldcat USING 'WERKS'
                                'IT_MAIN'
                                4
                                'Plant'
                                'Plant'
                                'Plant'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''
                                ''.
  PERFORM append_fieldcat USING 'LGORT'
                                'IT_MAIN'
                                4
                                'Storage Location'
                                'Storage Location'
                                'St.Location'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''
                                ''.
  PERFORM append_fieldcat USING 'DISPO'
                                'IT_MAIN'
                                3
                                'Manager'
                                'Manager'
                                'Manager'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''
                                ''.
  PERFORM append_fieldcat USING 'FEEDR'
                                'IT_MAIN'
                                5
                                'Feeder'
                                'Feeder'
                                'Feeder'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''
                                ''.
  PERFORM append_fieldcat USING 'LGTYP'
                                'IT_MAIN'
                                3
                                'Warehouse'
                                'Warehouse'
                                'Warehouse'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''
                                ''.
  PERFORM append_fieldcat USING 'LGPLA'
                                'IT_MAIN'
                                10
                                'Bin Location'
                                'Bin Location'
                                'Bin Location'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''
                                ''.
  PERFORM append_fieldcat USING 'ZLINE'
                                'IT_MAIN'
                                1
                                'Line'
                                'Line'
                                'Line'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''
                                ''.
  PERFORM append_fieldcat USING 'WORKS'
                                'IT_MAIN'
                                5
                                'Workstation'
                                'Workstation'
                                'Workstation'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''
                                ''.
  PERFORM append_fieldcat USING 'RH_LH'
                                'IT_MAIN'
                                2
                                'RH/LH'
                                'RH/LH'
                                'RH/LH'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''
                                ''.
  PERFORM append_fieldcat USING 'PROFL'
                                'IT_MAIN'
                                3
                                'Source'
                                'Source'
                                'Source'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''
                                ''.
  PERFORM append_fieldcat USING 'MEINS'
                                'IT_MAIN'
                                3
                                'Base Unit'
                                'Base Unit'
                                'Base Unit'
                                'UNIT'
                                ''
                                ''
                                ''
                                ''
                                ''
                                ''.
  PERFORM append_fieldcat USING 'BSTMA'
                                'IT_MAIN'
                                18
                                'QTY/Container'
                                'QTY/Container'
                                'QTY/Container'
                                'QUAN'
                                ''
                                ''
                                'MEINS'
                                ''
                                ''
                                ''.
  PERFORM append_fieldcat USING 'LVSME'
                                'IT_MAIN'
                                3
                                'WMoM'
                                'WMoM'
                                'WMoM'
                                'UNIT'
                                ''
                                ''
                                ''
                                ''
                                ''
                                ''.
  PERFORM append_fieldcat USING 'FSTMA'
                                'IT_MAIN'
                                18
                                'Feeding QTY'
                                'Feeding QTY'
                                'Feeding QTY'
                                'QUAN'
                                ''
                                ''
                                'LVSME'
                                ''
                                ''
                                ''.
  PERFORM append_fieldcat USING 'LS_GESME'
                                'IT_MAIN'
                                19
                                'Line Stock'
                                'Line Stock'
                                'Line Stock'
                                'QUAN'
                                ''
                                ''
                                'MEINS'
                                ''
                                ''
                                'X'.
  PERFORM append_fieldcat USING 'WS_GESME'
                                'IT_MAIN'
                                19
                                'Warehouse Stock'
                                'Warehouse Stock'
                                'Warehouse Stock'
                                'QUAN'
                                ''
                                ''
                                'MEINS'
                                ''
                                ''
                                'X'.
  PERFORM append_fieldcat USING 'CB_GESME'
                                'IT_MAIN'
                                19
                                'CC-Bin Stock'
                                'CC-Bin Stock'
                                'CC-Bin Stock'
                                'QUAN'
                                ''
                                ''
                                'MEINS'
                                ''
                                ''
                                'X'.
  PERFORM append_fieldcat USING 'CR_GESME'
                                'IT_MAIN'
                                19
                                'CC-Rack Stock'
                                'CC-Rack Stock'
                                'CC-Rack Stock'
                                'QUAN'
                                ''
                                ''
                                'MEINS'
                                ''
                                ''
                                'X'.
  PERFORM append_fieldcat USING 'CY_GESME'
                                'IT_MAIN'
                                19
                                'CY Stock'
                                'CY Stock'
                                'CY Stock'
                                'QUAN'
                                ''
                                ''
                                'MEINS'
                                ''
                                ''
                                'X'.
  PERFORM append_fieldcat USING 'SUM_GESME'
                                'IT_MAIN'
                                19
                                'Total Stock'
                                'Total Stock'
                                'Total Stock'
                                'QUAN'
                                ''
                                ''
                                'MEINS'
                                ''
                                ''
                                'X'.
ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  append_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0255   text
*      -->P_0256   text
*      -->P_18     text
*      -->P_0258   text
*      -->P_0259   text
*      -->P_0260   text
*      -->P_0261   text
*      -->P_0262   text
*      -->P_0263   text
*      -->P_0264   text
*      -->P_0265   text
*      -->P_0266   text
*----------------------------------------------------------------------*
FORM append_fieldcat USING    p_fieldname
                              p_tabname
                              p_outputlen
                              p_text_l
                              p_text_m
                              p_text_s
                              p_datatype
                              p_key
                              p_no_out
                              p_unit
                              p_currency
                              p_text_field
                              p_do_sum.
  it_fieldcat-fieldname      = p_fieldname.
  it_fieldcat-tabname        = p_tabname.
  it_fieldcat-outputlen      = p_outputlen.
  it_fieldcat-seltext_l      = p_text_l.
  it_fieldcat-seltext_m      = p_text_m.
  it_fieldcat-seltext_s      = p_text_s.
  it_fieldcat-datatype       = p_datatype.
  it_fieldcat-key            = p_key.
  it_fieldcat-no_out         = p_no_out.
  it_fieldcat-qfieldname     = p_unit.
  it_fieldcat-cfieldname     = p_currency.
  it_fieldcat-text_fieldname = p_text_field.
  it_fieldcat-do_sum         = p_do_sum.
  APPEND it_fieldcat. CLEAR it_fieldcat.
ENDFORM.                    " append_fieldcat
*&---------------------------------------------------------------------*
*&      Form  build_top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_top_of_page.
* LIST HEADING LINE: TYPE H
  CLEAR gt_list_top_of_page.
  gt_list_top_of_page-typ  = 'H'.
  gt_list_top_of_page-info = text-t01.
  APPEND gt_list_top_of_page.
* STATUS LINE: TYPE S
  CLEAR gt_list_top_of_page.
* PLANT
  IF s_werks-high IS INITIAL.
    p_text = s_werks-low.
  ELSE.
    CONCATENATE s_werks-low '~' s_werks-high INTO p_text
      SEPARATED BY space.
  ENDIF.
  gt_list_top_of_page-typ  = 'S'.
  gt_list_top_of_page-key  = text-001. "Plant
  gt_list_top_of_page-info = p_text.
  APPEND gt_list_top_of_page.
*STORAGE LOCATION
  IF s_lgort-high IS INITIAL.
    p_text = s_lgort-low.
  ELSE.
    CONCATENATE s_lgort-low '~' s_lgort-high INTO p_text
      SEPARATED BY space.
  ENDIF.
  gt_list_top_of_page-typ  = 'S'.
  gt_list_top_of_page-key  = text-002. "Storage location
  gt_list_top_of_page-info = p_text.
  APPEND gt_list_top_of_page.
* Storage Type
  IF s_lgtyp-high IS INITIAL.
    p_text = s_lgtyp-low.
  ELSE.
    CONCATENATE s_lgtyp-low '~' s_lgtyp-high INTO p_text
      SEPARATED BY space.
  ENDIF.
  gt_list_top_of_page-typ  = 'S'.
  gt_list_top_of_page-key  = text-003. "Storage Type
  gt_list_top_of_page-info = p_text.
  APPEND gt_list_top_of_page.
* Manager
  IF s_dispo-high IS INITIAL.
    p_text = s_dispo-low.
  ELSE.
    CONCATENATE s_dispo-low '~' s_dispo-high INTO p_text
      SEPARATED BY space.
  ENDIF.
  gt_list_top_of_page-typ  = 'S'.
  gt_list_top_of_page-key  = text-004. "Manager
  gt_list_top_of_page-info = p_text.
  APPEND gt_list_top_of_page.
* Feeder
  IF s_feedr-high IS INITIAL.
    p_text = s_feedr-low.
  ELSE.
    CONCATENATE s_feedr-low '~' s_feedr-high INTO p_text
      SEPARATED BY space.
  ENDIF.
  gt_list_top_of_page-typ  = 'S'.
  gt_list_top_of_page-key  = text-005. "Feeder
  gt_list_top_of_page-info = p_text.
  APPEND gt_list_top_of_page.
* Line
  IF s_zline-high IS INITIAL.
    p_text = s_zline-low.
  ELSE.
    CONCATENATE s_zline-low '~' s_zline-high INTO p_text
      SEPARATED BY space.
  ENDIF.
  gt_list_top_of_page-typ  = 'S'.
  gt_list_top_of_page-key  = text-006. "Line
  gt_list_top_of_page-info = p_text.
  APPEND gt_list_top_of_page.
* Workstation
  IF s_works-high IS INITIAL.
    p_text = s_works-low.
  ELSE.
    CONCATENATE s_works-low '~' s_works-high INTO p_text
      SEPARATED BY space.
  ENDIF.
  gt_list_top_of_page-typ  = 'S'.
  gt_list_top_of_page-key  = text-007. "Workstation
  gt_list_top_of_page-info = p_text.
  APPEND gt_list_top_of_page.
* Source
  IF s_profl-high IS INITIAL.
    p_text = s_profl-low.
  ELSE.
    CONCATENATE s_profl-low '~' s_profl-high INTO p_text
      SEPARATED BY space.
  ENDIF.
  gt_list_top_of_page-typ  = 'S'.
  gt_list_top_of_page-key  = text-008. "Source
  gt_list_top_of_page-info = p_text.
  APPEND gt_list_top_of_page.
ENDFORM.                    " build_top_of_page
*&---------------------------------------------------------------------*
*&      Form  build_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sort.

  CHECK it_sort[] IS INITIAL.

  it_sort-fieldname = 'MATNR'.
  it_sort-tabname   = 'IT_MAIN'.
  it_sort-up        = 'X'.
  it_sort-group     = 'G1'.
  APPEND it_sort. CLEAR it_sort.
  it_sort-fieldname = 'BISMT'.
  it_sort-tabname   = 'IT_MAIN'.
  it_sort-up        = 'X'.
  it_sort-group     = 'G1'.
  APPEND it_sort. CLEAR it_sort.
  it_sort-fieldname = 'MAKTX'.
  it_sort-tabname   = 'IT_MAIN'.
  it_sort-up        = 'X'.
  it_sort-group     = 'G1'.
  APPEND it_sort. CLEAR it_sort.


ENDFORM.                    " build_sort
*&---------------------------------------------------------------------*
*&      Form  check_select-options
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_select-options.

* Check Plant
  IF NOT s_werks-low IS INITIAL.
    SELECT SINGLE * FROM t001w WHERE werks = s_werks-low.
    IF sy-subrc NE 0.
      MESSAGE e999 WITH text-010 'Plant'.
    ENDIF.
  ENDIF.
* Check Storage Location
  IF NOT s_lgort-low IS INITIAL.
    SELECT SINGLE * FROM t001l WHERE werks IN s_werks
                                 AND lgort = s_lgort-low.
    IF sy-subrc NE 0.
      MESSAGE e999 WITH text-010 'Storage location'.
    ENDIF.
  ENDIF.
* Check Storage Type
  IF NOT s_lgtyp-low IS INITIAL.
    SELECT SINGLE * FROM t301 WHERE lgtyp = s_lgtyp-low.
    IF sy-subrc NE 0.
      MESSAGE e999 WITH text-010 'Storage Type'.
    ENDIF.
  ENDIF.
* Check Manager(MRP Controller)
  IF NOT s_dispo-low IS INITIAL.
    SELECT SINGLE * FROM t024d WHERE werks IN s_werks
                                 AND dispo = s_dispo-low.
    IF sy-subrc NE 0.
      MESSAGE e999 WITH text-010 'Manager'.
    ENDIF.
  ENDIF.
* Check Feeder, Line, Workstation
  IF NOT ( s_feedr[] IS INITIAL AND s_zline[] IS INITIAL AND
           s_works[] IS INITIAL ).
    SELECT SINGLE * FROM ztmm_mast WHERE feedr IN s_feedr
                                     AND zline IN s_zline
                                     AND works IN s_works.
    IF sy-subrc NE 0.
      MESSAGE e999 WITH text-010 'Supply to Line Master Parameter'.
    ENDIF.
  ENDIF.
* Check Source
  IF NOT s_profl-low IS INITIAL.
    SELECT SINGLE * FROM tdg41 WHERE profl = s_profl-low.
    IF sy-subrc NE 0.
      MESSAGE e999 WITH text-010 'Source'.
    ENDIF.
  ENDIF.
ENDFORM.                    " check_select-options
*&---------------------------------------------------------------------*
*&      Form  build_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_data.

  CLEAR it_main. REFRESH it_main.
  LOOP AT it_mara.
    MOVE-CORRESPONDING it_mara TO it_main.
*   Unit of Warehouse
    READ TABLE it_mlgn WITH KEY matnr = it_mara-matnr.
    IF sy-subrc = 0.
      it_main-fstma = it_mara-bstma * it_mlgn-umrez / it_mlgn-umren.
      it_main-lvsme = it_mlgn-lvsme.
    ENDIF.
*   Stck
    LOOP AT it_lqua WHERE werks = it_mara-werks
                      AND matnr = it_mara-matnr.
      it_main-meins = it_lqua-meins.
      CASE it_lqua-lgtyp.
*       Line Stock
        WHEN c_500 OR c_510 OR c_520.
          ADD it_lqua-gesme TO it_main-ls_gesme.
*       Warehouse Stock
        WHEN c_400 OR c_410 OR c_420 OR c_430.
          ADD it_lqua-gesme TO it_main-ws_gesme.
*       CC Bin Stock
        WHEN c_300.
          ADD it_lqua-gesme TO it_main-cb_gesme.
*       CC Rack Stock
        WHEN c_200.
          ADD it_lqua-gesme TO it_main-cr_gesme.
*       CY Stock
        WHEN c_100.
          ADD it_lqua-gesme TO it_main-cy_gesme.
      ENDCASE.
      CLEAR it_lqua.
    ENDLOOP.
*   Total Stock
    IF sy-subrc = 0.
      it_main-sum_gesme = it_main-ls_gesme + it_main-ws_gesme +
                          it_main-cb_gesme + it_main-cr_gesme +
                          it_main-cy_gesme.
    ELSE.
      it_main-sum_gesme = it_mara-labst.
    ENDIF.
*   Storage Type
    READ TABLE it_mlgt WITH KEY matnr = it_mara-matnr.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING it_mlgt TO it_main.
    ENDIF.
    APPEND it_main. CLEAR it_main.
  ENDLOOP.
ENDFORM.                    " build_data
*&---------------------------------------------------------------------*
*&      Form  get_Material_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_material_master.
  CLEAR: it_mara, it_matnr.
  REFRESH: it_mara, it_matnr.
  SELECT a~matnr d~werks d~lgort d~labst
         a~bismt a~profl t~maktx c~bstma c~dispo
         z~feedr z~zline z~works z~rh_lh
    INTO TABLE it_mara
    FROM mara AS a
   INNER JOIN marc AS c
      ON a~matnr = c~matnr
   INNER JOIN ztmm_mast AS z
      ON c~werks = z~werks AND
         c~matnr = z~matnr
   INNER JOIN mard AS d
      ON z~matnr = d~matnr AND
         z~werks = d~werks
   INNER JOIN makt AS t
      ON a~matnr = t~matnr AND
         t~spras = sy-langu
  WHERE a~profl IN s_profl
    AND c~dispo IN s_dispo
    AND z~feedr IN s_feedr
    AND z~zline IN s_zline
    AND z~works IN s_works
    AND d~werks IN s_werks
    AND d~lgort IN s_lgort.
  IF sy-subrc NE 0.
    MESSAGE e999 WITH text-009.
  ENDIF.
  SORT it_mara BY werks.
  LOOP AT it_mara.
    it_matnr-werks = it_mara-werks.
    it_matnr-matnr = it_mara-matnr.
    COLLECT it_matnr. CLEAR it_matnr.
  ENDLOOP.
ENDFORM.                    " get_Material_master
*&---------------------------------------------------------------------*
*&      Form  get_Warehouse
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_warehouse_unit.
  CLEAR it_mlgn.
  REFRESH it_mlgn.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_mlgn
    FROM mlgn AS a INNER JOIN marm AS m
      ON a~matnr = m~matnr AND
         a~lvsme = m~meinh
     FOR ALL ENTRIES IN it_matnr
   WHERE a~matnr = it_matnr-matnr.
ENDFORM.                    " get_Warehouse_unit
*&---------------------------------------------------------------------*
*&      Form  get_material_Storagetype
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_material_storagetype.
  CLEAR it_mlgt.
  REFRESH it_mlgt.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_mlgt
    FROM mlgt
     FOR ALL ENTRIES IN it_matnr
   WHERE matnr = it_matnr-matnr.
ENDFORM.                    " get_material_Storagetype
*&---------------------------------------------------------------------*
*&      Form  get_Warehouse_stock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_warehouse_stock.
  CLEAR it_lqua.
  REFRESH it_lqua.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_lqua
    FROM lqua
     FOR ALL ENTRIES IN it_matnr
   WHERE matnr = it_matnr-matnr
     AND werks = it_matnr-werks.
ENDFORM.                    " get_Warehouse_stock
*---------------------------------------------------------------------*
*       FORM USER_COMMAND
*---------------------------------------------------------------------*
*       ........
*---------------------------------------------------------------------*
*  -->  UCOMM
*  -->  SELFIELD
*---------------------------------------------------------------------*
FORM user_command USING ucomm LIKE sy-ucomm
                       selfield TYPE slis_selfield.
  CASE ucomm.
    WHEN '&IC1'. " Double Click
      BREAK-POINT.
  ENDCASE.

ENDFORM.
