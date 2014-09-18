*&---------------------------------------------------------------------*
*& Program ID     : ZFMC0004                                           *
*& Program Name   : Release operating budget periodically              *
*& Created by     : YN.Kim                                             *
*& Created on     : 08/19/2011                                         *
*& Reference Pgm  :                                                    *
*&                                                                     *
*& Modification Log                                                    *
*----------------------------------------------------------------------*
* DATE      |  NAME          |Transport | Issue #  |      DESC         *
*----------------------------------------------------------------------*
*                                                                      *
*&=====================================================================*
REPORT  zfmc0004 NO STANDARD PAGE HEADING.

TABLES: fmfctr,               " Funds Center Master Record
        fmhisv,               " Hierarchy table of funds center
        fmci,                 " Commitment items master data
        fmhici,               " Commitment items hierarchy
        fm01,                 " Financial Management Areas
        bppe.                 " Totals Record for Period Values
type-pools zfmcm.
INCLUDE <icon>.
DATA: zsfm0008 LIKE zsfm0008.

*---// Internal tables
DATA: it_row TYPE STANDARD TABLE OF zsfm0008
                                     WITH HEADER LINE.
DATA: BEGIN OF it_sorc OCCURS 0,
*        objnr  LIKE bppe-objnr,
*        posit  LIKE bppe-posit,
*        fistl  LIKE fmhisv-fistl,
        fictr  LIKE fmfctr-fictr,
        fipex  LIKE fmhici-fipex,
        gjahr  LIKE bppe-gjahr,
        geber  LIKE bppe-geber,
        profil LIKE tbpfmx-profil,
        icon(04),
        waers  LIKE bppe-twaer,
        farea  LIKE bppe-farea,
        wlp01  LIKE bppe-wlp01,
        wlp02  LIKE bppe-wlp01,
        wlp03  LIKE bppe-wlp01,
        wlp04  LIKE bppe-wlp01,
        wlp05  LIKE bppe-wlp01,
        wlp06  LIKE bppe-wlp01,
        wlp07  LIKE bppe-wlp01,
        wlp08  LIKE bppe-wlp01,
        wlp09  LIKE bppe-wlp01,
        wlp10  LIKE bppe-wlp01,
        wlp11  LIKE bppe-wlp01,
        wlp12  LIKE bppe-wlp01,
        wltot  LIKE bppe-wlp01,
        docnr  LIKE tbpbwret21-entry_docnr,
        zmsg(100),
      END   OF it_sorc.

DATA: it_sorc_tar LIKE it_sorc OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.

DATA: BEGIN OF wa_opt OCCURS 0.
        INCLUDE STRUCTURE ctu_params.
DATA: END OF wa_opt.

*---// Global Variables
*DATA: v_fname  LIKE rlgrap-filename VALUE 'c:\temp\fm_actual.xls'.
*---// Global variables
DATA: v_total(5)    TYPE n,
      v_ready(5)    TYPE n,
      v_error(5)    TYPE n,
      v_success(5)  TYPE n,
      v_selected(5) TYPE n,
      v_ans.

*---// Constants
CONSTANTS: c_versn      LIKE bppe-versn     VALUE zfmcm_versn_0,      "Version
           c_hivarnt    LIKE fmhisv-hivarnt VALUE zfmcm_hivarnt,      "Hierarchy
           c_fikrs      LIKE fmci-fikrs     VALUE zfmcm_fm_area,
           c_geber      LIKE bppe-geber     VALUE '    ',
           c_eur        LIKE bppe-twaer     VALUE zfmcm_euro,
           c_yes                            VALUE 'J',
           c_original   TYPE i              VALUE  1,
           c_supplement TYPE i              VALUE  2,
           c_transfer   TYPE i              VALUE  3,
           c_return     TYPE i              VALUE  4,
           c_current    TYPE i              VALUE  5,
           c_released   TYPE i              VALUE  6,
           c_commitment TYPE i              VALUE  7,
           c_invoice    TYPE i              VALUE  8,
           c_payments   TYPE i              VALUE  9,
           c_available  TYPE i              VALUE 10,
           c_unknown    TYPE i              VALUE 11.


*-----/// ALV Control : START
* Control Framework Basic Class
CLASS cl_gui_cfw      DEFINITION LOAD.

* Declare reference variables, the container and internal table
DATA: wc_control_9000   TYPE        scrfname VALUE 'CC_9000_ALV',
      wc_alv_9000       TYPE REF TO cl_gui_alv_grid,
      wc_container_9000 TYPE REF TO cl_gui_custom_container.

DATA: v_container(50),
      v_control(50),
      v_alv(50),
      v_itab(50),
      v_structure LIKE dd02l-tabname.

FIELD-SYMBOLS: <container> TYPE REF TO cl_gui_custom_container,
               <control>   TYPE        scrfname,
               <alv>       TYPE REF TO cl_gui_alv_grid,
               <itab>      TYPE STANDARD TABLE.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS lcl_event_receiver DEFINITION DEFERRED. "/ALV Event Handling

DATA : event_receiver TYPE REF TO lcl_event_receiver.

* Interal tables for ALV GRID
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

* Global variable for ALV GRID
DATA : v_is_layout TYPE lvc_s_layo,
       v_variant   TYPE disvariant,          "for parameter IS_VARIANT
       v_fieldname LIKE LINE OF it_fieldname,
       v_repid     LIKE sy-repid,
       v_cnt       TYPE i,                   "Field count
       v_save      TYPE c   VALUE 'A'.   "for Parameter I_SAVE
*-----/// ALV Control : END

****************************************************************
* LOCAL CLASSES: Definition for Event Handling
****************************************************************
* class lcl_event_receiver: local class to handle event DOUBLE_CLICK
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
ENDCLASS.                    "lcl_event_receiver DEFINITION

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
* class lcl_event_receiver (Implementation)
CLASS lcl_event_receiver IMPLEMENTATION.
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*---// Selection screen
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS: p_fikrs LIKE fmci-fikrs MEMORY ID fik OBLIGATORY
                                    DEFAULT c_fikrs,
            p_gjahr LIKE fmci-gjahr MEMORY ID gjr OBLIGATORY
                                    DEFAULT sy-datum(4),
            p_perio TYPE fc_tperi   OBLIGATORY
                                    DEFAULT sy-datum+4(2).

SELECT-OPTIONS: s_fictr  FOR fmfctr-fictr,
                s_fipex  FOR fmci-fipex.
SELECTION-SCREEN END OF BLOCK bl1.

INITIALIZATION.
  PERFORM initialization.
  SET PARAMETER ID 'FIK' FIELD zfmcm_fm_area.
*---// Input value check & Read data
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_rtn.
  PERFORM read_data.

START-OF-SELECTION.
  PERFORM it_sorc_delete.
  READ TABLE it_sorc INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE s000(zz) WITH text-m27.
    EXIT.
  ENDIF.

  CALL SCREEN 9000.

AT USER-COMMAND.
*  CASE sy-ucomm.
*    WHEN 'DOWNLOAD'.
*      CLEAR: sy-ucomm.
*      PERFORM download_rtn.
*  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
FORM check_rtn .

  PERFORM check_fikrs.
  PERFORM check_fictr.
  PERFORM check_fipex.

ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  check_fikrs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_fikrs.

  SELECT SINGLE * FROM fm01 WHERE fikrs = p_fikrs.
  IF sy-subrc NE 0.
    MESSAGE e001(zz) WITH text-m01.
  ENDIF.

ENDFORM.                    " check_fikrs
*&---------------------------------------------------------------------*
*&      Form  check_fictr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_fictr .

  SELECT SINGLE * FROM fmfctr WHERE fikrs EQ p_fikrs
                                AND fictr IN s_fictr.
  IF sy-subrc NE 0.
    MESSAGE e001(zz) WITH text-m02.
  ENDIF.

ENDFORM.                    " check_fictr
*&---------------------------------------------------------------------*
*&      Form  check_fipex
*&---------------------------------------------------------------------*
FORM check_fipex .

  SELECT SINGLE * FROM fmci WHERE fikrs EQ p_fikrs
                              AND gjahr EQ '0000'
                              AND fipex IN s_fipex.
  IF sy-subrc NE 0.
    MESSAGE e001(zz) WITH text-m03.
  ENDIF.

ENDFORM.                    " check_fipex
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_data .

  DATA: lt_sorc LIKE it_sorc OCCURS 0 WITH HEADER LINE.
  DATA: l_fld_cnt    TYPE i,
        l_fr_fld_cnt TYPE i,
        l_month      TYPE i,
        l_left       TYPE i.

  FIELD-SYMBOLS: <fs1>, <fs2>.

  CLEAR: it_sorc, it_sorc[].

  PERFORM call_function_monthly_budget TABLES lt_sorc.

*  SELECT a~fictr b~fistl d~fipex d~potyp
*         e~bezei c~objnr c~posit c~gjahr
*         c~geber c~farea
**         c~versn c~vorga c~twaer
*         c~wlp01 c~wlp02 c~wlp03 c~wlp04
*         c~wlp05 c~wlp06 c~wlp07 c~wlp08
*         c~wlp09 c~wlp10 c~wlp11 c~wlp12
*    INTO CORRESPONDING FIELDS OF TABLE lt_sorc
*    FROM fmfctr AS a INNER JOIN fmhisv AS b
*                        ON a~fikrs   = b~fikrs
*                       AND a~fictr   = b~fistl
*                       AND b~hivarnt = c_hivarnt
*                     INNER JOIN bppe AS c
*                        ON a~ctr_objnr = c~objnr
*                     INNER JOIN fmci AS d
*                        ON c~posit = d~posit
*                       AND a~fikrs = d~fikrs
*                     INNER JOIN fmhici AS f
*                        ON d~fikrs = f~fikrs
*                       AND d~gjahr = f~gjahr
*                       AND d~fipex = f~fipex
*                       AND f~varnt = '000'
*                     INNER JOIN fmcit AS e
*                        ON d~fikrs = e~fikrs
*                       AND d~fipex = e~fipex
*                       AND e~spras = sy-langu
*                       AND e~gjahr = '0000'
*   WHERE a~fikrs     EQ c_fikrs
*     AND a~fictr     IN s_fictr
*     AND b~child_st  EQ space
*     AND c~lednr     EQ '0003'
*     AND c~trgkz     EQ 'N'
*     AND c~wrttp     EQ '43'
*     AND c~gjahr     EQ p_gjahr
*     AND c~geber     EQ c_geber
*     AND c~versn     EQ c_versn
*     AND c~vorga     EQ 'KBUD'
*     AND c~twaer     EQ c_eur
*     AND c~subvo     EQ space
*     AND c~gnjhr     EQ '0000'
*     AND c~farea     EQ space
*     AND c~perbl     EQ '016'
*     AND d~fipex     IN s_fipex
**     AND d~potyp     IN r_potyp
*     AND f~child_fip EQ space.

  l_month = p_perio.

  LOOP AT lt_sorc.

*    PERFORM get_budget_profile USING   c_fikrs       lt_sorc-posit
*                                       lt_sorc-objnr lt_sorc-gjahr
*                                       lt_sorc-geber lt_sorc-farea
*                              CHANGING lt_sorc-profil.

    CLEAR: l_left.
    CASE lt_sorc-profil.
      WHEN 'M'.
        l_left = 1.
      WHEN 'Q'.
        l_left = l_month MOD 3.
      WHEN 'H'.
        l_left = l_month MOD 6.
      WHEN 'Y'.
        l_left = l_month MOD 12.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    CHECK l_left = 1.

    l_fld_cnt = 8.
    DO l_fld_cnt TIMES.
      ASSIGN COMPONENT sy-index OF
             STRUCTURE lt_sorc TO <fs1>.
      IF sy-subrc <> 0. EXIT. ENDIF.
      ASSIGN COMPONENT sy-index OF
             STRUCTURE it_sorc TO <fs2>.
      <fs2> = <fs1>.
    ENDDO.

    CASE it_sorc-profil.
      WHEN 'M'.
        l_fr_fld_cnt = l_fld_cnt + l_month.
        ASSIGN COMPONENT l_fr_fld_cnt OF
               STRUCTURE lt_sorc TO <fs1>.
        ASSIGN COMPONENT l_fr_fld_cnt OF
               STRUCTURE it_sorc TO <fs2>.
        <fs2> = <fs1>.
        ADD <fs1> TO it_sorc-wltot.
      WHEN 'Q'.
        DO 3 TIMES.
          IF sy-index = 1.
            l_fr_fld_cnt = l_fld_cnt + l_month.
          ELSE.
            l_fr_fld_cnt = l_fr_fld_cnt + 1.
          ENDIF.
          ASSIGN COMPONENT l_fr_fld_cnt OF
                 STRUCTURE lt_sorc TO <fs1>.
          ASSIGN COMPONENT l_fr_fld_cnt OF
                 STRUCTURE it_sorc TO <fs2>.
          <fs2> = <fs1>.
          ADD <fs1> TO it_sorc-wltot.
        ENDDO.
      WHEN 'H'.
        DO 6 TIMES.
          IF sy-index = 1.
            l_fr_fld_cnt = l_fld_cnt + l_month.
          ELSE.
            l_fr_fld_cnt = l_fr_fld_cnt + 1.
          ENDIF.
          ASSIGN COMPONENT l_fr_fld_cnt OF
                 STRUCTURE lt_sorc TO <fs1>.
          ASSIGN COMPONENT l_fr_fld_cnt OF
                 STRUCTURE it_sorc TO <fs2>.
          <fs2> = <fs1>.
          ADD <fs1> TO it_sorc-wltot.
        ENDDO.
      WHEN 'Y'.
        DO 12 TIMES.
          IF sy-index = 1.
            l_fr_fld_cnt = l_fld_cnt + l_month.
          ELSE.
            l_fr_fld_cnt = l_fr_fld_cnt + 1.
          ENDIF.
          ASSIGN COMPONENT l_fr_fld_cnt OF
                 STRUCTURE lt_sorc TO <fs1>.
          ASSIGN COMPONENT l_fr_fld_cnt OF
                 STRUCTURE it_sorc TO <fs2>.
          <fs2> = <fs1>.
          ADD <fs1> TO it_sorc-wltot.
        ENDDO.
    ENDCASE.

    IF it_sorc-wltot IS INITIAL.
*      MOVE: icon_led_green  TO it_sorc-icon.
*      v_success = v_success + 1.
      CLEAR it_sorc.
      CONTINUE.
    ELSE.
      MOVE: icon_led_yellow TO it_sorc-icon.
      v_ready = v_ready + 1.
    ENDIF.

    v_total = v_total + 1.

    APPEND it_sorc.  CLEAR it_sorc.

  ENDLOOP.

  IF sy-subrc NE 0.
    MESSAGE e001(zz) WITH text-m04.
  ENDIF.

  it_sorc-waers = c_eur.
  MODIFY it_sorc TRANSPORTING waers WHERE waers <> c_eur.

  SORT it_sorc BY fictr fipex.

ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  get_budget_profile
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM get_budget_profile USING   pv_fikrs pv_posit pv_objnr
                                pv_gjahr pv_geber pv_farea
                       CHANGING pv_profil.

  CALL FUNCTION 'KBPA_FIFM_GET_PROFIL'
    EXPORTING
      i_posit         = pv_posit
      i_objnr         = pv_objnr
      i_gjahr         = pv_gjahr
      i_geber         = pv_geber
      i_farea         = pv_farea
    IMPORTING
      e_profil        = pv_profil
    EXCEPTIONS
      no_profil_found = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    CALL FUNCTION 'FM5B_GET_PROFILE'
      EXPORTING
        i_fikrs           = pv_fikrs
        i_fincode         = pv_geber
      IMPORTING
        e_profil          = pv_profil
      EXCEPTIONS
        fm_area_not_found = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      RAISE no_profile.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_budget_profile
*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.

  CASE sy-dynnr.
    WHEN 9000.
      SET PF-STATUS '9000'.
      SET TITLEBAR  '9000'.
  ENDCASE.

ENDMODULE.                 " status  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_alv_object OUTPUT.

  PERFORM create_alv_object USING sy-dynnr.

ENDMODULE.                 " create_alv_object  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  create_alv_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DYNNR  text
*----------------------------------------------------------------------*
FORM create_alv_object USING p_dynnr.

  CONCATENATE: 'WC_CONTAINER_' p_dynnr INTO v_container.
  ASSIGN:      (v_container)           TO   <container>.

  IF <container> IS INITIAL.          "/Not Created Control for ALV GRID
    PERFORM create_container_n_object USING p_dynnr.
    PERFORM set_attributes_alv_grid USING p_dynnr.
    PERFORM build_field_catalog USING p_dynnr.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM assign_itab_to_alv USING p_dynnr.
    PERFORM sssign_event USING p_dynnr.
  ENDIF.

ENDFORM.                    " create_alv_object

*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM create_container_n_object USING p_dynnr.

*- Create Container('GRID_CONTAINER') with Custom Control on screen

  CONCATENATE: 'WC_CONTAINER_' p_dynnr INTO v_container,
               'WC_CONTROL_'   p_dynnr INTO v_control,
               'WC_ALV_'       p_dynnr INTO v_alv.

  ASSIGN: (v_container) TO <container>,
          (v_control)   TO <control>,
          (v_alv)       TO <alv>.

  CREATE OBJECT <container>
         EXPORTING container_name = <control>
         EXCEPTIONS
          cntl_error = 1
          cntl_system_error = 2
          create_error = 3
          lifetime_error = 4
          lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc NE 0.
    v_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = v_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT <alv>
         EXPORTING i_parent      = <container>
                   i_appl_events = 'X'.

ENDFORM.                    " create_container_n_object

*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid USING p_dynnr.

  CASE p_dynnr.
    WHEN '9000'.
      PERFORM set_attributes_alv_9000.
  ENDCASE.

ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_attributes_alv_9000.
  CLEAR : v_is_layout, v_variant.

  v_is_layout-edit       = ' '.      "/Edit Mode Enable
  v_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  v_is_layout-language   = sy-langu. "/Language Key
  v_is_layout-cwidth_opt = 'X'.      "/optimizes the column width
  v_is_layout-no_merging = 'X'.      "/Disable cell merging
  v_variant-report       = sy-repid.
  v_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_9000

*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM build_field_catalog USING p_dynnr.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  PERFORM set_fieldname USING p_dynnr.
  PERFORM set_screen_fields USING p_dynnr.
ENDFORM.                    " build_field_catalog

*&---------------------------------------------------------------------*
*&      Form  set_fieldname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_fieldname USING p_dynnr.
*  DATA: lw_itab TYPE slis_tabname.
*
*  CLEAR: it_fieldcat,  it_fieldcat[],
*         it_fieldname, it_fieldname[].
*
*  MOVE: sy-repid TO v_repid.
*  CONCATENATE c_structure p_dynnr INTO lw_itab.
*
*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_program_name     = v_repid
*      i_internal_tabname = lw_itab
*      i_inclname         = v_repid
*    CHANGING
*      ct_fieldcat        = it_fieldname.


ENDFORM.                    " set_fieldname
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_screen_fields USING p_dynnr.

  CLEAR: it_fieldcat,  it_fieldcat[].

  CASE p_dynnr.
    WHEN '9000'.
      PERFORM set_screen_fields_9000.
  ENDCASE.
ENDFORM.                    " set_screen_fields
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen_fields_9000.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :
*                    'S' 'FIELDNAME'   'OBJNR',
*                    ' ' 'REF_FIELD'   'OBJNR',
*                    ' ' 'REF_TABLE'   'BPPE',
*                    'E' 'KEY'         'X',
*
*                    'S' 'FIELDNAME'   'POSIT',
*                    ' ' 'REF_FIELD'   'POSIT',
*                    ' ' 'REF_TABLE'   'FMCI',
*                    ' ' 'KEY'         'X',
*                    'E' 'COLTEXT'     'Commit item',

                    'S' 'FIELDNAME'   'FICTR',
                    ' ' 'REF_FIELD'   'FICTR',
                    ' ' 'REF_TABLE'   'FMFCTR',
                    'E' 'KEY'         'X',

                    'S' 'FIELDNAME'   'FIPEX',
                    ' ' 'REF_FIELD'   'FIPEX',
                    ' ' 'REF_TABLE'   'FMHICI',
                    'E' 'KEY'         'X',

                    'S' 'FIELDNAME'   'ICON',
                    ' ' 'KEY'         'X',
                    'E' 'COLTEXT'     'Icon',

                    'S' 'FIELDNAME'   'PROFIL',
                    ' ' 'REF_FIELD'   'PROFIL',
                    ' ' 'REF_TABLE'   'TBPFMX',
                    'E' 'KEY'         'X',

                    'S' 'FIELDNAME'   'WLP01',
                    ' ' 'REF_FIELD'   'WLP01',
                    ' ' 'REF_TABLE'   'BPPE',
                    ' ' 'CFIELDNAME'  'WAERS',
                    'E' 'COLTEXT'     'Mon. 01',

                    'S' 'FIELDNAME'   'WLP02',
                    ' ' 'REF_FIELD'   'WLP02',
                    ' ' 'REF_TABLE'   'BPPE',
                    ' ' 'CFIELDNAME'  'WAERS',
                    'E' 'COLTEXT'     'Mon. 02',

                    'S' 'FIELDNAME'   'WLP03',
                    ' ' 'REF_FIELD'   'WLP03',
                    ' ' 'REF_TABLE'   'BPPE',
                    ' ' 'CFIELDNAME'  'WAERS',
                    'E' 'COLTEXT'     'Mon. 03',

                    'S' 'FIELDNAME'   'WLP04',
                    ' ' 'REF_FIELD'   'WLP04',
                    ' ' 'REF_TABLE'   'BPPE',
                    ' ' 'CFIELDNAME'  'WAERS',
                    'E' 'COLTEXT'     'Mon. 04',

                    'S' 'FIELDNAME'   'WLP05',
                    ' ' 'REF_FIELD'   'WLP05',
                    ' ' 'REF_TABLE'   'BPPE',
                    ' ' 'CFIELDNAME'  'WAERS',
                    'E' 'COLTEXT'     'Mon. 05',

                    'S' 'FIELDNAME'   'WLP06',
                    ' ' 'REF_FIELD'   'WLP06',
                    ' ' 'REF_TABLE'   'BPPE',
                    ' ' 'CFIELDNAME'  'WAERS',
                    'E' 'COLTEXT'     'Mon. 06',

                    'S' 'FIELDNAME'   'WLP07',
                    ' ' 'REF_FIELD'   'WLP07',
                    ' ' 'REF_TABLE'   'BPPE',
                    ' ' 'CFIELDNAME'  'WAERS',
                    'E' 'COLTEXT'     'Mon. 07',

                    'S' 'FIELDNAME'   'WLP08',
                    ' ' 'REF_FIELD'   'WLP08',
                    ' ' 'REF_TABLE'   'BPPE',
                    ' ' 'CFIELDNAME'  'WAERS',
                    'E' 'COLTEXT'     'Mon. 08',

                    'S' 'FIELDNAME'   'WLP09',
                    ' ' 'REF_FIELD'   'WLP09',
                    ' ' 'REF_TABLE'   'BPPE',
                    ' ' 'CFIELDNAME'  'WAERS',
                    'E' 'COLTEXT'     'Mon. 09',

                    'S' 'FIELDNAME'   'WLP10',
                    ' ' 'REF_FIELD'   'WLP10',
                    ' ' 'REF_TABLE'   'BPPE',
                    ' ' 'CFIELDNAME'  'WAERS',
                    'E' 'COLTEXT'     'Mon. 10',

                    'S' 'FIELDNAME'   'WLP11',
                    ' ' 'REF_FIELD'   'WLP11',
                    ' ' 'REF_TABLE'   'BPPE',
                    ' ' 'CFIELDNAME'  'WAERS',
                    'E' 'COLTEXT'     'Mon. 11',

                    'S' 'FIELDNAME'   'WLP12',
                    ' ' 'REF_FIELD'   'WLP12',
                    ' ' 'REF_TABLE'   'BPPE',
                    ' ' 'CFIELDNAME'  'WAERS',
                    'E' 'COLTEXT'     'Mon. 12',

                    'S' 'FIELDNAME'   'WLTOT',
                    ' ' 'REF_FIELD'   'WLP12',
                    ' ' 'REF_TABLE'   'BPPE',
                    ' ' 'CFIELDNAME'  'WAERS',
                    'E' 'COLTEXT'     'Total',

                    'S' 'FIELDNAME'   'DOCNR',
                    ' ' 'REF_FIELD'   'BELNR',
                    ' ' 'REF_TABLE'   'BPPK',
                    ' ' 'NO_ZERO'     'X',
                    'E' 'COLTEXT'     'Doc. No',

                    'S' 'FIELDNAME'   'ZMSG',
                    'E' 'COLTEXT'     'Message'.

ENDFORM.                    " set_screen_fields_9000

*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING p_gub  p_fname  p_value.
*  DATA : lv_col(40).
*
*  FIELD-SYMBOLS <fs>.
*
** START - FIELD ATTRIBUTE SETTING
*  IF p_gubun = 'S'.
*    CLEAR: p_fieldcat.
*
*    READ TABLE it_fieldname INTO v_fieldname
*                            WITH KEY fieldname  = p_field.
*    IF sy-subrc NE 0.
*      MESSAGE e000(zz) WITH 'Check filed catalog'.
*    ENDIF.
*
*    MOVE: v_fieldname-fieldname TO p_fieldcat-fieldname.
*    EXIT.
*  ENDIF.
*
** Setting The Field's Attributes
*  CONCATENATE 'P_FIELDCAT-' p_field  INTO lv_col.
*  ASSIGN (lv_col) TO <fs>.
*  MOVE   p_value  TO <fs>.
*
** END - FIELD ATTRIBUTE SETTING
*  IF p_gubun = 'E'.
*    IF p_fieldcat-col_pos IS INITIAL.
*      ADD 1 TO v_cnt.
*      p_fieldcat-col_pos = v_cnt.
*    ENDIF.
*    APPEND p_fieldcat.
*  ENDIF.

  IF p_gub = 'S'.
    CLEAR p_fieldcat.
  ENDIF.

  DATA l_fname(40).
  FIELD-SYMBOLS <fs> TYPE ANY.
  CONCATENATE 'p_fieldcat-' p_fname INTO l_fname.

  ASSIGN (l_fname) TO <fs>.
  <fs> = p_value.

  IF p_gub = 'E'.
    APPEND p_fieldcat.
  ENDIF.

ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv USING p_dynnr.
  DATA: lv_dynnr   LIKE   sy-dynnr.

  CONCATENATE: 'WC_ALV_'    p_dynnr      INTO v_alv.
*               c_structure  p_dynnr      INTO v_structure,
*               'IT_'        p_dynnr '[]' INTO v_itab.

  MOVE 'IT_SORC'        TO v_itab.
  ASSIGN: (v_alv)       TO <alv>.
*          (v_itab)      TO <itab>.

  CALL METHOD <alv>->set_table_for_first_display
    EXPORTING
*      i_structure_name = v_structure
      is_layout        = v_is_layout
      i_save           = v_save
      is_variant       = v_variant
      i_default        = space
    CHANGING
      it_fieldcatalog  = it_fieldcat[]
      it_outtab        = it_sorc[].
*      IT_OUTTAB        = <itab>.
ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  sssign_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sssign_event USING p_dynnr.
  DATA: lv_dynnr   LIKE   sy-dynnr.

  CONCATENATE: 'WC_ALV_'    p_dynnr      INTO v_alv.
  ASSIGN: (v_alv)       TO <alv>.

*--  Regist event for Edit
  IF sy-batch IS INITIAL.
    CALL METHOD <alv>->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
  ENDIF.
ENDFORM.                    " sssign_event
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.

  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANC'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'EXECUTE'.
      CLEAR: sy-ucomm.
      PERFORM execute_rtn.
  ENDCASE.

ENDMODULE.                 " user_command_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  FILL_FIELDNAME
*&---------------------------------------------------------------------*
FORM fill_fieldname  USING    value(p_tab)
                              value(p_fld).

  CLEAR v_fieldname.
  v_fieldname-tabname      = p_tab.
  v_fieldname-fieldname    = p_fld.
  APPEND v_fieldname TO it_fieldname.

ENDFORM.                    " FILL_FIELDNAME
*&---------------------------------------------------------------------*
*&      Form  execute_rtn
*&---------------------------------------------------------------------*
FORM execute_rtn .

  PERFORM get_target_data USING sy-dynnr.
  PERFORM execute_fr51.
  PERFORM assign_itab_to_alv USING sy-dynnr.

ENDFORM.                    " execute_rtn
*&---------------------------------------------------------------------*
*&      Form  get_target_data
*&---------------------------------------------------------------------*
FORM get_target_data  USING    pv_dynnr.

  "/Indexes of Selected Rows
  DATA: lt_rows   TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows

  CONCATENATE: 'WC_ALV_' pv_dynnr INTO v_alv.
  ASSIGN: (v_alv) TO <alv>.

  CALL METHOD <alv>->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m10.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m11.
  ENDIF.

  PERFORM set_target_data TABLES lt_rows.

ENDFORM.                    " get_target_data

*&---------------------------------------------------------------------*
*&      Form  set_target_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ROWS  text
*----------------------------------------------------------------------*
FORM set_target_data TABLES pt_rows STRUCTURE lvc_s_row.
  CLEAR: v_selected.
  CLEAR: it_sorc_tar, it_sorc_tar[].

  LOOP AT pt_rows WHERE index NE 0.
    CLEAR it_sorc.
    READ TABLE it_sorc INDEX pt_rows-index.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m05.
    ENDIF.

    v_selected = v_selected + 1.

    CHECK it_sorc-icon EQ icon_led_yellow OR
          it_sorc-icon EQ icon_led_red.

    MOVE it_sorc TO it_sorc_tar.
    APPEND it_sorc_tar.  CLEAR it_sorc_tar.

  ENDLOOP.
ENDFORM.                    " set_target_data

*&---------------------------------------------------------------------*
*&      Form  execute_FR51
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM execute_fr51 .

  DATA: text1(60),
        text2(60),
        titl(60).

  text1 = text-m06.
*  text2 = text-m07.
  titl  = text-m08.
  PERFORM popup_confirm   USING text1 text2 titl
                                'X'   'A'   v_ans.
  CHECK v_ans = c_yes.

  PERFORM generate_bdc.
*  PERFORM posting_rtn.

  PERFORM update_it_9000.
  PERFORM count_status.

  MESSAGE s000(zz) WITH v_selected text-m12.
ENDFORM.                    " execute_FR51
*&---------------------------------------------------------------------*
*&      Form  generate_bdc
*&---------------------------------------------------------------------*
FORM generate_bdc .

  DATA: lv_peri1_01(21), lv_peri1_02(21), lv_peri1_03(21),
        lv_peri1_04(21), lv_peri1_05(21), lv_peri1_06(21),
        lv_peri1_07(21), lv_peri1_08(21), lv_peri1_09(21),
        lv_peri1_10(21), lv_peri1_11(21), lv_peri1_12(21),
        l_sgtxt(40).

  CONCATENATE p_gjahr '.' p_perio ':' 'Budget Release'
              INTO l_sgtxt.

  LOOP AT it_sorc_tar.

    CLEAR: it_bdc, it_bdc[].

    WRITE: it_sorc_tar-wlp01 CURRENCY it_sorc_tar-waers
                             TO lv_peri1_01(21),
           it_sorc_tar-wlp02 CURRENCY it_sorc_tar-waers
                             TO lv_peri1_02(21),
           it_sorc_tar-wlp03 CURRENCY it_sorc_tar-waers
                             TO lv_peri1_03(21),
           it_sorc_tar-wlp04 CURRENCY it_sorc_tar-waers
                             TO lv_peri1_04(21),
           it_sorc_tar-wlp05 CURRENCY it_sorc_tar-waers
                             TO lv_peri1_05(21),
           it_sorc_tar-wlp06 CURRENCY it_sorc_tar-waers
                             TO lv_peri1_06(21),
           it_sorc_tar-wlp07 CURRENCY it_sorc_tar-waers
                             TO lv_peri1_07(21),
           it_sorc_tar-wlp08 CURRENCY it_sorc_tar-waers
                             TO lv_peri1_08(21),
           it_sorc_tar-wlp09 CURRENCY it_sorc_tar-waers
                             TO lv_peri1_09(21),
           it_sorc_tar-wlp10 CURRENCY it_sorc_tar-waers
                             TO lv_peri1_10(21),
           it_sorc_tar-wlp11 CURRENCY it_sorc_tar-waers
                             TO lv_peri1_11(21),
           it_sorc_tar-wlp12 CURRENCY it_sorc_tar-waers
                             TO lv_peri1_12(21).

    PERFORM dynpro USING:
       'X' 'SAPLKBPB'     '0200',
       ' ' 'FMDY-FIKRS'   p_fikrs,              "FM Area
       ' ' 'BPDY-GJAHR'   p_gjahr,              "Fiscal year
       ' ' 'BPDY-VERSN'   c_versn,              "Version
       ' ' 'FMDY-FINCODE' c_geber,       "Fund
       ' ' 'BDC_OKCODE'   '/00',

       'X' 'SAPLKBPB'     '0400',               "Detail screen
       ' ' 'BDC_OKCODE'   '=DOCH',              "Header

       'X' 'SAPLKBPB'     '0150',               "Header
       ' ' 'BPDY-SGTXT'   l_sgtxt,              "Text
       ' ' 'BDC_OKCODE'   '/00'.

    PERFORM dynpro USING:
       'X' 'SAPLKBPB'         '0400',               "Detail Screen
       ' ' 'FMDY-FICTR(01)'   it_sorc_tar-fictr,    "Fund Center
       ' ' 'FMDY-FIPEX(01)'   it_sorc_tar-fipex,    "Commitment Item
       ' ' 'BPDY-SPRED1(01)'  '0',                  "DK
       ' ' 'G_TABLECON_INFO-MARK(01)' 'X',          "Check
       ' ' 'BDC_OKCODE'       '=PERI',

       'X' 'SAPLKBPP'         '0600',               "Period Screen
       ' ' 'BPDY-PERI1(01)'   lv_peri1_01,          " 01
       ' ' 'BPDY-PERI1(02)'   lv_peri1_02,          " 02
       ' ' 'BPDY-PERI1(03)'   lv_peri1_03,          " 03
       ' ' 'BPDY-PERI1(04)'   lv_peri1_04,          " 04
       ' ' 'BPDY-PERI1(05)'   lv_peri1_05,          " 05
       ' ' 'BPDY-PERI1(06)'   lv_peri1_06,          " 06
       ' ' 'BPDY-PERI1(07)'   lv_peri1_07,          " 07
       ' ' 'BPDY-PERI1(08)'   lv_peri1_08,          " 08
       ' ' 'BPDY-PERI1(09)'   lv_peri1_09,          " 09
       ' ' 'BPDY-PERI1(10)'   lv_peri1_10,          " 10
       ' ' 'BPDY-PERI1(11)'   lv_peri1_11,          " 11
       ' ' 'BPDY-PERI1(12)'   lv_peri1_12,          " 12
       ' ' 'G_SCREEN_0600-DK' '0',                  "DK
       ' ' 'BDC_OKCODE'  '=CLOS'.

    PERFORM dynpro USING:
       'X' 'SAPLKBPB'         '0400',                 "Detail Screen
       ' ' 'BDC_OKCODE'       '=POST'.                "Save

    PERFORM posting_rtn.

  ENDLOOP.

ENDFORM.                    " generate_bdc

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
FORM dynpro USING dynbegin name value.
  IF dynbegin = 'X'.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-program,
          value TO it_bdc-dynpro,
          dynbegin TO it_bdc-dynbegin.
    APPEND it_bdc.
  ELSE.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-fnam,
          value TO it_bdc-fval.
    APPEND it_bdc.
  ENDIF.
ENDFORM.                    " dynpro
*&---------------------------------------------------------------------*
*&      Form  posting_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posting_rtn .

  CALL TRANSACTION 'FR51'  USING it_bdc
                           OPTIONS FROM wa_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '043'.
    MOVE: icon_led_red TO it_sorc_tar-icon.
    PERFORM get_err_msg USING it_sorc_tar-zmsg.

    MODIFY it_sorc_tar.
  ELSE.
    MOVE: icon_led_green TO it_sorc_tar-icon,
          sy-msgv1       TO it_sorc_tar-docnr.
    MODIFY it_sorc_tar.
  ENDIF.

ENDFORM.                    " posting_rtn
*&---------------------------------------------------------------------*
*&      Form  count_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM count_status .

  CLEAR: v_total, v_ready, v_error, v_success.

  LOOP AT it_sorc.
    CASE it_sorc-icon.
      WHEN icon_led_yellow.
        v_ready   = v_ready   + 1.
      WHEN icon_led_red.
        v_error   = v_error   + 1.
      WHEN icon_led_green.
        v_success = v_success + 1.
    ENDCASE.

    v_total = v_total + 1.
  ENDLOOP.

ENDFORM.                    " count_status
*&---------------------------------------------------------------------*
*&      Form  initialization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization .

*---// BDC MODE, DEFAULT SIZE, UPDATE MODE
  wa_opt-defsize = 'X'.
  wa_opt-updmode = 'S'.
  wa_opt-dismode = 'N'.
*  wa_opt-dismode = 'A'.

ENDFORM.                    " initialization

*&---------------------------------------------------------------------*
*&      Form  get_err_msg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_9000_ZMSG  text
*----------------------------------------------------------------------*
FORM get_err_msg USING pw_msg.
  DATA: lw_msg LIKE cfgnl-msglin.

  CALL FUNCTION 'RKC_MSG_STRING'
    EXPORTING
      id      = sy-msgid
      mtype   = sy-msgty
      number  = sy-msgno
      par1    = sy-msgv1
      par2    = sy-msgv2
      par3    = sy-msgv3
      par4    = sy-msgv4
    IMPORTING
      msg_lin = lw_msg.

  MOVE: lw_msg TO pw_msg.
ENDFORM.                    " get_err_msg
*&---------------------------------------------------------------------*
*&      Form  update_it_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM update_it_9000 .

  SORT it_sorc BY fictr fipex.

  LOOP AT it_sorc_tar.
    READ TABLE it_sorc WITH KEY fictr = it_sorc_tar-fictr
                                fipex = it_sorc_tar-fipex
                                BINARY SEARCH.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m05.
    ENDIF.

    MOVE: it_sorc_tar-icon  TO it_sorc-icon,
          it_sorc_tar-zmsg  TO it_sorc-zmsg,
          it_sorc_tar-docnr TO it_sorc-docnr.

    MODIFY it_sorc INDEX sy-tabix.
  ENDLOOP.

ENDFORM.                    " update_it_9000

*&---------------------------------------------------------------------*
*&      Form  popup_confirm
*&---------------------------------------------------------------------*
FORM popup_confirm USING   value(p_txt1)
                           value(p_txt2)
                           value(p_titl)
                           value(p_cancel_flg)
                           value(p_def_val)
                           p_ans.

  CLEAR p_ans.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      defaultoption  = p_def_val
      textline1      = p_txt1
      textline2      = p_txt2
      titel          = p_titl
      cancel_display = p_cancel_flg
    IMPORTING
      answer         = p_ans.

ENDFORM.                    " POPUP_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  call_function_monthly_budget
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_SORC  text
*----------------------------------------------------------------------*
FORM call_function_monthly_budget  TABLES pt_sorc STRUCTURE it_sorc.

  RANGES: lr_geber FOR ifmfincode-fincode.

  lr_geber = 'IEQ'. lr_geber-low = c_geber.
  APPEND lr_geber.

  CALL FUNCTION 'Z_FM_GET_MONTHLY_BUDGET'
    EXPORTING
      i_fikrs            = p_fikrs
      i_gjahr            = p_gjahr
    TABLES
      t_geber            = lr_geber
      t_fictr            = s_fictr
      t_fipex            = s_fipex
      t_itab             = it_row
    EXCEPTIONS
      no_fm_area         = 1
      no_fund            = 2
      no_funds_center    = 3
      no_commitment_item = 4
      no_profile         = 5
      no_category        = 6
      no_original        = 7
      OTHERS             = 8.

  CASE sy-subrc.
    WHEN 1.
      MESSAGE e000(zz) WITH text-m21.
    WHEN 2.
      MESSAGE e000(zz) WITH text-m22.
    WHEN 3.
      MESSAGE e000(zz) WITH text-m23.
    WHEN 4.
      MESSAGE e000(zz) WITH text-m24.
    WHEN 5.
      MESSAGE e000(zz) WITH text-m25.
    WHEN 6.
      MESSAGE e000(zz) WITH text-m26.
    WHEN 7.
      MESSAGE e000(zz) WITH text-m27.
    WHEN 8.
      MESSAGE e000(zz) WITH text-m28.
  ENDCASE.

  LOOP AT it_row WHERE ctgry = c_current
                    OR ctgry = c_released.

    IF it_row-ctgry = c_released.
      it_row-wtp01   =     it_row-wtp01  * -1.
      it_row-wtp02   =     it_row-wtp02  * -1.
      it_row-wtp03   =     it_row-wtp03  * -1.
      it_row-wtp04   =     it_row-wtp04  * -1.
      it_row-wtp05   =     it_row-wtp05  * -1.
      it_row-wtp06   =     it_row-wtp06  * -1.
      it_row-wtp07   =     it_row-wtp07  * -1.
      it_row-wtp08   =     it_row-wtp08  * -1.
      it_row-wtp09   =     it_row-wtp09  * -1.
      it_row-wtp10   =     it_row-wtp10  * -1.
      it_row-wtp11   =     it_row-wtp11  * -1.
      it_row-wtp12   =     it_row-wtp12  * -1.
    ENDIF.

*    MOVE: it_row-geber       TO pt_sorc-geber,
    MOVE: it_row-fictr       TO pt_sorc-fictr,
          it_row-fipex       TO pt_sorc-fipex,
          it_row-profil      TO pt_sorc-profil,
          it_row-wtp01       TO pt_sorc-wlp01,
          it_row-wtp02       TO pt_sorc-wlp02,
          it_row-wtp03       TO pt_sorc-wlp03,
          it_row-wtp04       TO pt_sorc-wlp04,
          it_row-wtp05       TO pt_sorc-wlp05,
          it_row-wtp06       TO pt_sorc-wlp06,
          it_row-wtp07       TO pt_sorc-wlp07,
          it_row-wtp08       TO pt_sorc-wlp08,
          it_row-wtp09       TO pt_sorc-wlp09,
          it_row-wtp10       TO pt_sorc-wlp10,
          it_row-wtp11       TO pt_sorc-wlp11,
          it_row-wtp12       TO pt_sorc-wlp12.

    COLLECT pt_sorc.  CLEAR pt_sorc.
    CLEAR it_row.
  ENDLOOP.

ENDFORM.                    " call_function_monthly_budget
*&---------------------------------------------------------------------*
*&      Form  IT_SORC_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form IT_SORC_DELETE .
 DELETE it_sorc where wlp01  = 0 and
                      wlp02  = 0 and
                      wlp03  = 0 and
                      wlp04  = 0 and
                      wlp05  = 0 and
                      wlp06  = 0 and
                      wlp07  = 0 and
                      wlp08  = 0 and
                      wlp09  = 0 and
                      wlp10  = 0 and
                      wlp11  = 0 and
                      wlp12  = 0.

endform.                    " IT_SORC_DELETE
