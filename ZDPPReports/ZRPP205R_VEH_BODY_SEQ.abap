************************************************************************
* Program Name      : ZRPP205R_VEH_BODY_SEQ
* Author            : BOBBY
* Creation Date     : 2004.02.10.
* Specifications By : MH Moon
* Pattern           : 2.1
* Development Request No : UD1K907187
* Addl Documentation:
* Description       : NFA Sequence List
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  zrpp205r_veh_body_seq         NO STANDARD PAGE HEADING
                                      MESSAGE-ID zmpp.

TABLES: ztpp_common_vals, "[PP] COMMON Information Table
        ztpp_seq_sum02 ,  "ALC Code Summary - Long Term Plan Table
        AUSP            .

INCLUDE <icon>.
INCLUDE <list>.

*****// ALV //**********************************************************
TYPE-POOLS: slis.
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gt_fc       TYPE slis_t_fieldcat_alv,
      g_fieldcat_s LIKE LINE OF gt_fieldcat,
      gs_layout   TYPE slis_layout_alv,
      gs_print    TYPE slis_print_alv,
      gt_sort     TYPE slis_t_sortinfo_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_header   TYPE slis_t_listheader,
*      gt_header1  TYPE slis_t_listheader,
      gt_colinfo_table TYPE slis_t_specialcol_alv. "line color.

* FIELD SYMBOLS
FIELD-SYMBOLS: <fs> TYPE ANY.

* hierarchy(simple)
DATA : g_tabname_header       TYPE slis_tabname,       "header part
       g_tabname_item         TYPE slis_tabname,       "detail list
       gs_keyinfo             TYPE slis_keyinfo_alv.   "relation key

* return
DATA : g_exit_caused_by_caller  TYPE c,
       gs_exit_caused_by_user   TYPE slis_exit_by_user.

DATA: col_pos TYPE i,
      cnt     TYPE i,
      g_save  TYPE c,
      g_repid LIKE sy-repid,
      g_variant LIKE disvariant.
DATA: gt_extab TYPE slis_t_extab WITH HEADER LINE.

****************************************************
* Definition of Internal Tables
****************************************************
DATA: BEGIN OF it_sap        OCCURS 0,
         seq                 TYPE i ,              " SERIAL
         mitu                TYPE c ,              " MITU
         fl(30)              TYPE c ,              " FACE LIFT
         modl(30)            TYPE c ,              " MODEL
         vin(17)             TYPE c ,              " VIN
         style(30)           TYPE c ,              " STYLE
         tm(30)              TYPE c ,              " TRANS MISSION
         drv(30)             TYPE c ,              " DRIVE
         reg(30)             TYPE c ,              " REGION
         nat(05)             TYPE c ,              " NATION + DEALER
         pas(30)             TYPE c ,              " PERSONS PER CAR
         sunr(30)            TYPE c ,              " SUNROOF
         space1(30)          TYPE c ,                       " SPACE1
         space2(30)          TYPE c ,                       " SPACE2
         space3(30)          TYPE c ,                       " SPACE3
         space4(30)          TYPE c ,                       " SPACE4
         space5(30)          TYPE c ,                       " SPACE5
         worder              LIKE mara-matnr,      " WORK ORDER(HEADRE)
         wcolor(05)          TYPE c ,              " EXTC + INTC
         epi(5)              TYPE c ,              " EPI-CODE
         MODE(04)            TYPE C ,              " Ex) EMF
         mast                LIKE mara-matnr,      " Ex) EMF000001
         SQDT                TYPE D ,
         SSR1(5)             TYPE C ,
       END OF it_sap .

DATA: BEGIN OF it_data OCCURS 0,
         seq                 TYPE i ,              " SERIAL
         mitu                TYPE c ,              " MITU
         fl(30)              TYPE c ,              " FACE LIFT
         modl(30)            TYPE c ,              " MODEL
         vin(17)             TYPE c ,              " VIN
         style(30)           TYPE c ,              " STYLE
         tm(30)              TYPE c ,              " TRANS MISSION
         drv(30)             TYPE c ,              " DRIVE
         reg(30)             TYPE c ,              " REGION
         nat(05)             TYPE c ,              " NATION + DEALER
         pas(30)             TYPE c ,              " PERSONS PER CAR
         sunr(30)            TYPE c ,              " SUNROOF
         space1(30)          TYPE c ,                       " SPACE1
         space2(30)          TYPE c ,                       " SPACE2
         space3(30)          TYPE c ,                       " SPACE3
         space4(30)          TYPE c ,                       " SPACE4
         space5(30)          TYPE c ,                       " SPACE5
         worder(18)          TYPE c ,              " WORK ORDER(HEADRE)
         wcolor(05)          TYPE c ,              " EXTC + INTC
         epi(5)              TYPE c ,              " EPI-CODE
         SQDT                TYPE D ,
         SSR1(5)             TYPE C ,
      END OF it_data.

DATA: BEGIN OF it_cuvtab      OCCURS 0.
        INCLUDE STRUCTURE     cuvtab_FLD.
DATA:   t_name(18),                       " Table Name
      END OF it_cuvtab.


****************************************************
* Work-Area Variables Definition
****************************************************
DATA: alv_grid               TYPE REF TO cl_gui_alv_grid,
      gs_custom_container    TYPE REF TO cl_gui_custom_container,
      wa_container           TYPE scrfname VALUE 'CONTAINER'.

*----------------------------------------------------------------------
* CONSTANTS DECLARATION
*----------------------------------------------------------------------


****************************************************
* Selection-Screen
****************************************************
SELECTION-SCREEN  BEGIN OF BLOCK blk1 WITH FRAME TITLE text-002.
PARAMETERS: p_date  TYPE d DEFAULT SY-DATUM OBLIGATORY ,
            p_model TYPE zmodel                          .
SELECTION-SCREEN  END OF BLOCK blk1.

***************************************************
INITIALIZATION.
***************************************************
  DATA: L_ATWRT         LIKE AUSP-ATWRT.

  G_REPID = SY-REPID.
*  SELECT A~ATWRT INTO L_ATWRT
*    FROM AUSP AS A INNER JOIN CABN AS C
*      ON A~ATINN = C~ATINN   UP TO 1 ROWS
*   WHERE C~ATNAM = 'P_VM_DATE'
*     AND A~KLART = '002'
*    ORDER BY A~ATWRT DESCENDING .
*  ENDSELECT.
*
*  P_DATE = L_ATWRT(8).

*************************************************
*AT SELECTION-SCREEN ON p_rp.
*************************************************


*************************************************
START-OF-SELECTION.
*************************************************
  PERFORM initial_data.
  PERFORM make_data_for_display.

*************************************************
END-OF-SELECTION.
*************************************************
  PERFORM  build_events.
  PERFORM  build_fieldcat    USING  'IT_DATA'.
* PERFORM  build_layout      USING  'X'   space   space.
  PERFORM  build_layout      USING  space space   space.
  PERFORM  build_comment     USING  gt_header[].
* ALV FUNCTION CALL
  PERFORM  start_grid_viewer TABLES  it_data.

*************************************************
TOP-OF-PAGE.
*************************************************
  PERFORM top_of_page.


*&---------------------------------------------------------------------*
*&      Form  initial_data
*&---------------------------------------------------------------------*
*       Searching Raw Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initial_data.
  DATA: l_ausp                LIKE TABLE OF ausp       WITH HEADER LINE,
        l_EQUI                LIKE TABLE OF EQUI       WITH HEADER LINE,
        l_worder              LIKE mara-matnr,
        l_color(3)            TYPE c         ,
        L_TABLE(40)           TYPE C         ,
        L_DATE                TYPE D         ,
        S_DATE(09)            TYPE C         ,
        L_ATFLV               LIKE AUSP-ATFLV,
        L_DATE_CHAR(10),
        l_atinn               LIKE ausp-atinn.

*  PERFORM get_atinn    USING 'P_VM_DATE'  l_atinn.
*  CONCATENATE P_DATE '%' INTO S_DATE.

  PERFORM get_atinn    USING 'P_SEQUENCE_DATE'  l_atinn.
  L_ATFLV = L_DATE_CHAR = P_DATE.

  SELECT * INTO TABLE L_EQUI
    FROM EQUI
   WHERE ERDAT = P_DATE
     AND EQTYP = 'V'    .

  SELECT objek  INTO CORRESPONDING FIELDS OF TABLE l_ausp
    FROM ausp
   WHERE atinn = l_atinn
     AND klart = '002'
*     AND ATWRT LIKE S_DATE .
     AND ATFLV = L_ATFLV.

  LOOP AT L_EQUI.
    SELECT SINGLE *
      FROM AUSP
     WHERE OBJEK = L_EQUI-EQUNR
       AND ATINN = L_ATINN
       AND klart = '002'   .

    IF SY-SUBRC NE 0.
       L_AUSP-OBJEK = L_EQUI-EQUNR.
       APPEND L_AUSP.
    ENDIF.
  ENDLOOP.

  LOOP AT l_ausp.
    PERFORM get_val_atwrt USING l_ausp-objek 'P_MODEL'      it_sap-MODE.
    PERFORM get_val_atwrt USING l_ausp-objek 'P_MITU'       it_sap-mitu.
    PERFORM get_val_atwrt USING l_ausp-objek 'P_VIN'        it_sap-vin .
    PERFORM get_val_atwrt USING l_ausp-objek 'P_WORK_ORDER' l_worder.
    PERFORM get_val_atwrt USING l_ausp-objek 'P_EPI_CODE'   it_sap-epi .
    PERFORM get_val_atwrt USING l_ausp-objek 'P_EXT_COLOR'  l_color    .
    PERFORM get_val_atFLV USING l_ausp-objek 'P_SEQUENCE_DATE'
                                                            IT_SAP-SQDT.
    PERFORM get_val_atwrt USING l_ausp-objek 'P_SEQUENCE_SERIAL'
                                                            IT_SAP-SSR1.
    it_sap-nat    = l_worder+9(5).
    it_sap-worder = l_worder(09) .
    it_sap-wcolor = l_color      .
    PERFORM get_val_atwrt USING l_ausp-objek 'P_INT_COLOR'  l_color    .
    CONCATENATE it_sap-wcolor '/' l_color     INTO  it_sap-wcolor      .
    CONDENSE it_sap-wcolor.
    it_sap-mast   = l_ausp-objek .
    CONCATENATE it_sap-MODE '_SEQ_003'   INTO L_TABLE .
    PERFORM get_table_val USING  L_TABLE      it_sap-mast  it_sap-fl   .
    CONCATENATE it_sap-MODE '_SEQ_004'   INTO L_TABLE .
    PERFORM get_table_val USING  L_TABLE      it_sap-mast  it_sap-modl .
    CONCATENATE it_sap-MODE '_SEQ_006'   INTO L_TABLE .
    PERFORM get_table_val USING  L_TABLE      it_sap-mast  it_sap-style.
    CONCATENATE it_sap-MODE '_SEQ_007'   INTO L_TABLE .
    PERFORM get_table_val USING  L_TABLE      it_sap-mast  it_sap-tm   .
    CONCATENATE it_sap-MODE '_SEQ_008'   INTO L_TABLE .
    PERFORM get_table_val USING  L_TABLE      it_sap-mast  it_sap-drv  .
    CONCATENATE it_sap-MODE '_SEQ_009'   INTO L_TABLE .
    PERFORM get_table_val USING  L_TABLE      it_sap-mast  it_sap-reg  .
    CONCATENATE it_sap-MODE '_SEQ_011'   INTO L_TABLE .
    PERFORM get_table_val USING  L_TABLE      it_sap-mast  it_sap-pas  .
    CONCATENATE it_sap-MODE '_SEQ_012'   INTO L_TABLE .
    PERFORM get_table_val USING  L_TABLE      it_sap-mast  it_sap-sunr .
    APPEND it_sap.   CLEAR: IT_SAP .
  ENDLOOP.
ENDFORM.                    " initial_data

*&---------------------------------------------------------------------*
*&      Form  make_data_for_display
*&---------------------------------------------------------------------*
*       Modification of Data For Display
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_data_for_display.
  DATA: l_ser            TYPE i,
        l_no(3)          TYPE n.

* Reporting Point, Column, Model, Code .
  IF P_MODEL NE SPACE.
     DELETE IT_SAP WHERE MODE NE P_MODEL .
  ENDIF.

* SORT IT_SAP BY MODE MAST.
  SORT IT_SAP BY SQDT SSR1.

  LOOP AT it_sap .
    L_SER = L_SER + 1.
    MOVE-CORRESPONDING it_sap        TO it_data.
    IT_DATA-SEQ = L_SER .
    APPEND IT_DATA.
  ENDLOOP.
ENDFORM.                    " make_data_for_display

*&---------------------------------------------------------------------*
*&      Form  build_events
*&---------------------------------------------------------------------*
*       Building Events For ALV
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_events.
  CONSTANTS : c_pss TYPE slis_formname VALUE 'PF_STATUS_SET',
              c_uc  TYPE slis_formname VALUE 'USER_COMMAND',
              c_top TYPE slis_formname VALUE 'TOP_OF_PAGE'.
  REFRESH gt_events.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = gt_events.

  PERFORM modify_gt_events
          TABLES  gt_events
          USING :
*            slis_ev_pf_status_set c_pss,
*            slis_ev_user_command  c_uc,
            slis_ev_top_of_page   c_top.

ENDFORM.                    " build_events
*&---------------------------------------------------------------------*
*&      Form  modify_gt_events
*&---------------------------------------------------------------------*
*       Modification of Events For ALV
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*      -->P_SLIS_EV_TOP_OF_PAGE  text
*      -->P_C_TOP  text
*----------------------------------------------------------------------*
FORM modify_gt_events TABLES p_events_t LIKE gt_events
                      USING  p_form p_value.

  DATA: ls_event TYPE slis_alv_event.

  READ TABLE  p_events_t  WITH KEY  name = p_form
                          INTO ls_event.
  IF sy-subrc EQ 0.
    MOVE     p_value     TO   ls_event-form.
    MODIFY   p_events_t  FROM ls_event INDEX sy-tabix.
  ENDIF.
ENDFORM.                    " modify_gt_events

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       Building Field Categories For ALV
*----------------------------------------------------------------------*
*      -->P_0441   text
*----------------------------------------------------------------------*
FORM build_fieldcat USING p_intab.
  CLEAR   : gt_fieldcat, gt_fc.
  REFRESH : gt_fieldcat, gt_fc.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = g_repid
            i_internal_tabname = p_intab
            i_inclname         = g_repid
       CHANGING
            ct_fieldcat        = gt_fc.

  PERFORM setting_fieldcat TABLES gt_fieldcat USING :
*          Start of Setting The Field's Attributes
                                  'S' 'SEQ'         ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'SEQ',

                                  'S' 'MITU'        ' ',
                                  ' ' 'JUST'        'C',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '03',
                                  'E' 'SELTEXT_L'   'MIT' ,

                                  'S' 'FL'          ' ',
                                  ' ' 'JUST'        'C',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'F / L' ,

                                  'S' 'MODL'        ' ',
                                  ' ' 'JUST'        'C',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'MODEL' ,

                                  'S' 'VIN'         ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '17',
                                  'E' 'SELTEXT_L'   'VIN'     ,

                                  'S' 'STYLE'       ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'STYLE'  ,

                                  'S' 'TM'          ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '03',
                                  'E' 'SELTEXT_L'   'T/M'       ,

                                  'S' 'DRV'         ' ',
                                  ' ' 'JUST'        'C',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'DRV',

                                  'S' 'REG'         ' ',
                                  ' ' 'JUST'        'C',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '10',
                                  'E' 'SELTEXT_L'   'Reg'   ,

                                  'S' 'NAT'         ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'NAT'   ,

                                  'S' 'PAS'         ' ',
                                  ' ' 'JUST'        'C',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '10',
                                  'E' 'SELTEXT_L'   'PAS'   ,

                                  'S' 'SUNR'        ' ',
                                  ' ' 'JUST'        'C',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   'SUNROOF',

                                  'S' 'SPACE1'      ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '03',
                                  'E' 'SELTEXT_L'   'SP1'   ,

                                  'S' 'SPACE2'      ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '03',
                                  'E' 'SELTEXT_L'   'SP2'   ,

                                  'S' 'SPACE3'      ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '03',
                                  'E' 'SELTEXT_L'   'SP3'   ,

                                  'S' 'SPACE4'      ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '03',
                                  'E' 'SELTEXT_L'   'SP4'   ,

                                  'S' 'SPACE5'      ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '03',
                                  'E' 'SELTEXT_L'   'SP5'   ,

                                  'S' 'WORDER'      ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '10',
                                  'E' 'SELTEXT_L'   'WORK-ORDER',

                                  'S' 'WCOLOR'      ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'COLOR' ,

                                  'S' 'EPI'         ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'EPI' ,

                                  'S' 'SQDT'        ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '10',
                                  'E' 'SELTEXT_L'   'SEQ_DATE',

                                  'S' 'SSR1'        ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'SERIAL' .
ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       Setting Field Category For ALV
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT  text
*      -->P_1259   text
*      -->P_1260   text
*      -->P_1261   text
*----------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat LIKE gt_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.

  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR : g_fieldcat_s.
    READ TABLE gt_fc INTO g_fieldcat_s
                     WITH KEY fieldname  = p_field.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'G_FIELDCAT_S-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO cnt.
    g_fieldcat_s-col_pos = cnt.
    APPEND g_fieldcat_s TO p_fieldcat.
  ENDIF.

ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       Building Layout For ALV
*----------------------------------------------------------------------*
*      -->P_0445   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM build_layout USING p_cb p_color p_sum.
  CLEAR gs_layout.

  gs_layout-zebra             = 'X'.
  gs_layout-cell_merge        = space.
  gs_layout-colwidth_optimize = ' '.
  gs_layout-default_item      = 'X'.
* check box
  IF p_cb = 'X'.
    gs_layout-box_fieldname    = 'CHKBOX'.
  ENDIF.
* line color
  IF p_color = 'X'.
    gs_layout-coltab_fieldname = 'COLOR'.
  ENDIF.
* sum
  IF p_sum = 'X'.
    gs_layout-totals_text       = 'TOT'.
  ENDIF.
*
ENDFORM.                    " build_layout
*&---------------------------------------------------------------------*
*&      Form  build_comment
*&---------------------------------------------------------------------*
*       Building Comments For ALV
*----------------------------------------------------------------------*
*      -->P_GT_HEADER[]  text
*----------------------------------------------------------------------*
FORM build_comment USING    p_gt_header TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
        l_date(10),
        l_SPACE(50) VALUE '                                           ',
        l_dsnam LIKE t024d-dsnam,
        l_h_dsnam LIKE t024d-dsnam,
        l_time(8),
*        l_succ(5) TYPE i,
        l_ldate(10),
        l_hdate(10).

* Title
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-001.
  APPEND ls_line TO p_gt_header.

* Sub-title of HEADER
  ls_line-typ  = 'S'.
  WRITE  P_DATE    TO  L_DATE   MM/DD/YYYY .
  CONCATENATE TEXT-030 L_DATE TEXT-040
         INTO ls_line-info SEPARATED BY ' '.
  APPEND ls_line TO p_gt_header.

* Head-Line 1 Format
  ls_line-typ  = 'A'.
  ls_line-key  = 'HMMA: '.
  WRITE  SY-DATUM  TO  L_DATE   MM/DD/YYYY .
  CONCATENATE TEXT-010 L_DATE   INTO ls_line-info SEPARATED BY ' '.
  APPEND ls_line TO p_gt_header.
  ls_line-typ  = 'A'.
  ls_line-key  = 'HMMA: '.
  CONCATENATE TEXT-003 P_MODEL  INTO ls_line-info SEPARATED BY ' '.
  APPEND ls_line TO p_gt_header.
ENDFORM.                    " build_comment

*&---------------------------------------------------------------------*
*&      Form  start_grid_viewer
*&---------------------------------------------------------------------*
*       Running GRID Viewer
*----------------------------------------------------------------------*
*      -->P_IT_DATA  text
*----------------------------------------------------------------------*
FORM start_grid_viewer TABLES p_intab.

*** print paramter   ****************************************
  gs_print-no_coverpage = 'X'.
  gs_print-no_print_listinfos = 'X'.
  gs_print-no_change_print_params = 'X'.
  gs_print-no_print_selinfos = 'X'.
*************************************************************

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
*            i_background_id          = 'ALV_BACKGROUND' "HEADER? ??
*            i_bypassing_buffer       = 'X'
            i_callback_program       = g_repid
            is_layout                = gs_layout
            it_fieldcat              = gt_fieldcat[]
            i_callback_top_of_page   = 'TOP_OF_PAGE'
*            i_callback_pf_status_set = 'SET_STATUS'
*            I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*            IT_SORT                  = GT_SORT[]
            i_save                   = 'A'
            is_variant               = g_variant
            it_events                = gt_events[]
            is_print                 = gs_print
            it_list_commentary       = gt_header
       IMPORTING
            e_exit_caused_by_caller  = g_exit_caused_by_caller
            es_exit_caused_by_user   = gs_exit_caused_by_user
       TABLES
            t_outtab                 = p_intab.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " start_grid_viewer
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'Z_HYUNDAI_LOGO'
*           i_logo             = 'ENJOYSAP_LOGO'
            it_list_commentary = gt_header.
ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  call_workday
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DATE  text
*----------------------------------------------------------------------*
FORM call_workday USING    pa_date.
  DATA: l_ident             LIKE t001w-fabkl.

  SELECT SINGLE fabkl  INTO l_ident
    FROM t001w
   WHERE werks = 'P001'   .

  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            correct_option               = '+'
            date                         = pa_date
            factory_calendar_id          = l_ident
       IMPORTING
            date                         = pa_date
       EXCEPTIONS
            calendar_buffer_not_loadable = 1
            correct_option_invalid       = 2
            date_after_range             = 3
            date_before_range            = 4
            date_invalid                 = 5
            factory_calendar_not_found   = 6
            OTHERS                       = 7.
ENDFORM.                    " call_workday

*&---------------------------------------------------------------------*
*&      Form  GET_ATINN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0380   text
*      -->P_L_ATINN  text
*----------------------------------------------------------------------*
FORM get_atinn USING    pa_char  pa_atinn.
  CLEAR: pa_atinn .

  SELECT SINGLE atinn INTO pa_atinn
    FROM cabn
   WHERE atnam = pa_char .
ENDFORM.                    " GET_ATINN

*&---------------------------------------------------------------------*
*&      Form  GET_VAL_ATWRT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_AUSP_OBJEK  text
*      -->P_0423   text
*      -->P_IT_SAP_MITU  text
*----------------------------------------------------------------------*
FORM get_val_atwrt USING    pa_objek  pa_char  pa_atwrt.
  DATA: l_atinn    LIKE ausp-atinn.

  PERFORM get_atinn    USING  pa_char     l_atinn.

  SELECT SINGLE atwrt INTO pa_atwrt
    FROM ausp
   WHERE objek = pa_objek
     AND atinn = l_atinn
     AND klart = '002'    .
ENDFORM.                    " GET_VAL_ATWRT

*&---------------------------------------------------------------------*
*&      Form  GET_VAL_ATFLV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_AUSP_OBJEK  text
*      -->P_0423   text
*      -->P_IT_SAP_MITU  text
*----------------------------------------------------------------------*
FORM get_val_ATFLV USING    pa_objek  pa_char  pa_atwrt.
  DATA: L_VALS(8)  TYPE N         ,
        L_ATFLV    LIKE AUSP-ATFLV,
        l_atinn    LIKE ausp-atinn.

  PERFORM get_atinn    USING  pa_char     l_atinn.

  SELECT SINGLE ATFLV INTO L_ATFLV
    FROM ausp
   WHERE objek = pa_objek
     AND atinn = l_atinn
     AND klart = '002'    .

  PA_ATWRT = L_VALS = L_ATFLV .
ENDFORM.                    " GET_VAL_ATFLV

*&---------------------------------------------------------------------*
*&      Form  GET_TABLE_VAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0490   text
*      -->P_IT_SAP_MAST  text
*      -->P_IT_SAP_FL  text
*----------------------------------------------------------------------*
FORM get_table_val USING    pa_table  pa_key  pa_val .
  DATA: L_VALS               LIKE TABLE OF CUVTAB_VALC WITH HEADER LINE,
        L_VALN               LIKE TABLE OF CUVTAB_VALN WITH HEADER LINE,
        L_ATINN              LIKE AUSP-ATINN  ,
        L_CHAR(30)           TYPE C           ,
        l_vtint              LIKE cuvtab-vtint.

  CONCATENATE 'P_SEQ_' PA_TABLE+8(3)     INTO L_CHAR.
  PERFORM get_atinn    USING  L_char     l_atinn.

  SELECT SINGLE vtint INTO l_vtint
    FROM cuvtab
    WHERE vtnam = pa_table.

  CALL FUNCTION 'CUTQ_SELECT_CUVTAB_VALUES'
    EXPORTING
      table_number           = L_vtint
    tables
      entries_char           = L_VALS
      ENTRIES_NON_CHAR       = L_VALN.

  SORT L_VALS BY SLNID.

  LOOP AT L_VALS.
    IF L_VALS-ATINN = L_ATINN.  CONTINUE. ENDIF.
    SELECT SINGLE *
      FROM AUSP
     WHERE OBJEK = PA_KEY
       AND ATINN = L_VALS-ATINN
       AND KLART = '002'
       AND ATWRT = L_VALS-VALC .

    IF SY-SUBRC NE 0.
       DELETE L_VALS WHERE SLNID = L_VALS-SLNID.
    ENDIF.
  ENDLOOP.

  clear: l_vals.
  READ TABLE L_VALS WITH KEY ATINN = l_atinn.
  IF L_VALS-VALC = 'SPACE'.
    CLEAR: PA_VAL.
  ELSE.
    PA_VAL = L_VALS-VALC.
  ENDIF.
ENDFORM.                    " GET_TABLE_VAL
