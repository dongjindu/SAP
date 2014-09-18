*&--------------------------------------------------------------------
*& REPORT                 : ZRFIG05_MIT
*& Author                 : WSKIM
*& Creation Date          : 01/04/2005
*& Specification By       : Andy Choi
*& Pattern                : Report 1-1
*& Development Request No :
*& Addl documentation     :
*& Description            :  [CO] Material In Transit Report
*& Modification Log
*& Date       Developer    Request ID      Description
*& #1 01/19/2005 wskim        UD1K913889      Import data check
*& #2 02/22/2005 wskim        UD1K914557      New requirements
*&    05/23/2006 Manju        UD1K920829      performance fine tuning
*&    04/09/2010 Valerian                    *Add logic to get field
*&               HIS20094                     from House B/L No.
*&                                           *Add report header
*&                                           *Correct Drill down logic
*&                                            to display the PO
*&                                           *Fix print preview's
*&                                            short dump
*&                                           *Enhance Report to prevent
*&                                            error when tot. amount
*&--------------------------------------------------------------------
*
* useful t-code: MB5S, FBL3N
*
*
*   INDEX;
*     BSIS ; BUDAT / HKONT / BUDAT / ZUONR  - 125 sec to build index
*     BSAS ; BUDAT / HKONT / AUGDT / BUDAT / ZUONR
*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer  CTS No.    Description
*======================================================================
*& 06/21/2013  T00303    UD1K957474  U1: Apply Archiving
*----------------------------------------------------------------------
REPORT zaco91r_mit   MESSAGE-ID  zmco.

TYPE-POOLS: slis, vrm.
TABLES: bsis,bseg,mseg,bkpf,eket,ekpo,ztbl,ekko,lfa1, t001, ska1,
        ztfi_mit,*rseg.
TABLES sscrfields.

DATA: gc LIKE fdes-dispw.

INCLUDE <icon>.
INCLUDE <symbol>.
CLASS cl_gui_resources DEFINITION LOAD.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv.


DATA: wa_repid LIKE sy-repid,
      wa_var_save(1) TYPE c             VALUE  'A',
      wa_default(1)  TYPE c,
      wa_exit(1) TYPE c,
      wa_variant LIKE disvariant,
      wa_var LIKE disvariant,
      wa_alv_function_name(30) TYPE c VALUE 'REUSE_ALV_GRID_LIST',
      wa_alv_get_info_name(40) TYPE c.
*--- ALV
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

*----------------------------------------------------------------------
* define tables and internal structure
*----------------------------------------------------------------------
DATA : BEGIN OF gt_bsis OCCURS 0,
         zuonr  LIKE bsis-zuonr,
         matnr  LIKE bseg-matnr,
         ebeln  LIKE bseg-ebeln,
         ebelp  LIKE bseg-ebelp,
         hkont  LIKE bsis-hkont,
*         bukrs  like bsis-bukrs,
         gjahr  LIKE bsis-gjahr,
         belnr  LIKE bsis-belnr,
         buzei  LIKE bsis-buzei,
         shkzg  LIKE bsis-shkzg,
         budat  LIKE bsis-budat,
         bldat  LIKE bsis-bldat,
         blart  LIKE bsis-blart,
         augbl  LIKE bsis-augbl,
         augdt  LIKE bsis-augdt,
         dmbtr  LIKE bsis-dmbtr,
         menge  LIKE bseg-menge,
        awkey  LIKE bkpf-awkey,
        awtyp  LIKE bkpf-awtyp,
        tcode  LIKE bkpf-tcode,
        chk(1) TYPE c,
        xblnr   TYPE bsis-xblnr,                            "HIS20094
       END OF gt_bsis.

RANGES: r_zuonr FOR bsis-zuonr.

DATA: BEGIN OF i_bkpf OCCURS 0,
        gjahr  LIKE bkpf-gjahr,
        belnr  LIKE bkpf-belnr,
        awkey  LIKE bkpf-awkey,
        awtyp  LIKE bkpf-awtyp,
        tcode  LIKE bkpf-tcode,
      END OF i_bkpf.

DATA: BEGIN OF gt_pohd OCCURS 0,
       ebeln  LIKE ekpo-ebeln,
      END OF gt_pohd.

DATA: BEGIN OF gt_pohd_item OCCURS 0,
       ebeln  LIKE ekpo-ebeln,
       ebelp  LIKE ekpo-ebelp,
       bstyp  LIKE ekko-bstyp,
       bedat  LIKE ekko-bedat,
       bsart  LIKE ekko-bsart,
       lifnr  LIKE ekko-lifnr,
       elikz  LIKE ekpo-elikz,
       matnr  LIKE ekpo-matnr,
       menge  LIKE ekpo-menge,
       xersy  LIKE ekpo-xersy,
       ktmng  LIKE ekpo-ktmng,
       stawn  LIKE marc-stawn,
      END OF gt_pohd_item.

DATA: BEGIN OF gt_bol OCCURS 0,
        zfblno   LIKE ztbl-zfblno,   "B/L Document No
        ebeln    LIKE ztblit-ebeln,
        ebelp    LIKE ztblit-ebelp,
        zfrebeln LIKE ztbl-zfrebeln, "Representative P/O No
        zfbldt   LIKE ztbl-zfbldt,   "B/L issuing date
        zfetd    LIKE ztbl-zfetd,    "ETD
        zfeta    LIKE ztbl-zfeta,    "ETA
      END OF gt_bol.

DATA: BEGIN OF gt_data OCCURS 0,
* Key
       zuonr  LIKE bsis-zuonr,
       ebeln  LIKE eket-ebeln,
       ebelp  LIKE eket-ebelp,
       lifnr  LIKE bseg-lifnr,
       matnr  LIKE bsis-sgtxt,

       zfblno LIKE ztbl-zfblno,   "B/L#
* Misc Attribute
       stawn  LIKE marc-stawn,    "Duty
       hkont  LIKE bseg-hkont,
       bsart  LIKE ekko-bsart,
       xersy  LIKE ekpo-xersy,    "ERS mark in PO
       icon(4) TYPE c,
* Date...
       bedat   LIKE ekko-bedat,   " PO date
       budat   LIKE bsis-budat,   " POSTING DATE
       op_dat  LIKE   ekbz-budat, " FIRST POSTING DATE
       gr_date LIKE bsis-budat,   " GR DATE
       eindt   LIKE eket-eindt,   " delivery date
       zfbldt  LIKE ztbl-zfbldt,  " last B/L date
       zfeta   LIKE ztbl-zfeta,   " last ETA
       bldat   LIKE bsis-bldat,   " Doc Date (FI)

* Key figures..
       poqty   LIKE eket-menge,   " PO Quantity
       inbqt   LIKE lips-lfimg,   " Inb.Qty
       obmng   LIKE eket-menge,   " open PO quantity

       gr_qty     LIKE bseg-menge,   " GR Qty
       a_menge    LIKE bseg-menge,   " A IV
       m_menge    LIKE bseg-menge,   " M IV
       o_menge    LIKE bseg-menge,   " Other
       b_menge    LIKE bseg-menge,   " Open Qty

       gr_amt    LIKE bsis-dmbtr,   " GR Amt
       a_dmbtr   LIKE bsis-dmbtr,   " A IV
       m_dmbtr   LIKE bsis-dmbtr,   " M IV
       o_dmbtr   LIKE bsis-dmbtr,   " Other account amount
       w_dmbtr   LIKE bsis-dmbtr,   " Write off account
       b_dmbtr   LIKE bsis-dmbtr,   " Balance Amt
       act_amt   LIKE bsis-dmbtr,   " Actual Amount
       chkbox  TYPE c,
       light   TYPE c,
*       tabcolor     type slis_t_specialcol_alv,
       chk(1)  TYPE c,
       xblnr   TYPE bsis-xblnr,                             "HIS20094
      END OF gt_data.

* Begin of HIS20094
DATA: BEGIN OF gt_data_t OCCURS 0.
        INCLUDE STRUCTURE gt_data.
DATA: END OF gt_data_t.

DATA:   BEGIN OF gt_housebl OCCURS 0,
         hkont TYPE bsis-hkont,
         zuonr TYPE bsis-zuonr,
         xblnr TYPE bsis-xblnr,
         menge TYPE bseg-menge,
         dmbtr TYPE bsis-dmbtr,
        END OF gt_housebl.
* End of HIS20094

DATA: BEGIN OF gt_data_d OCCURS 0.
        INCLUDE STRUCTURE ztfi_mit.
DATA: END OF gt_data_d.

*data : begin of it_ekbe occurs 0,
*       ebeln   like   ekbz-ebeln,
*       ebelp   like   ekbz-ebelp,
*       bewtp   like   ekbe-bewtp,
*       budat   like   ekbz-budat,
*
*       shkzg   like   ekbe-shkzg,
*       menge   like   ekbe-menge,
*       end   of  it_ekbe.

DATA : BEGIN OF it_t030 OCCURS 0,
        konts LIKE t030-konts,
       END OF it_t030.

DATA :  BEGIN OF gt_glpoitm OCCURS 0,
         hkont LIKE bseg-hkont,
         zuonr LIKE bseg-zuonr,
         ebeln LIKE ekpo-ebeln,
         ebelp LIKE ekpo-ebelp,
         dmbtr LIKE bseg-dmbtr,
        END OF gt_glpoitm.

DATA :  BEGIN OF gt_poitm OCCURS 0,
         ebeln LIKE ekpo-ebeln,
         ebelp LIKE ekpo-ebelp,
        END OF gt_poitm.

*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
DATA:   bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
*       Nodata-Character
DATA:   nodata_character TYPE c VALUE '/'.
DATA    mode TYPE c VALUE 'E'.

**---WORK AREA
DATA :w_int TYPE i.
DATA : z_shkzg LIKE bsas-shkzg,
       z_dmbtr LIKE bsas-dmbtr,
       z_zfbldt LIKE ztbl-zfbldt.
DATA : p_signal(4).
DATA : z_awkey LIKE bkpf-awkey,
       z_awtyp LIKE bkpf-awtyp.

*---// Constants
CONSTANTS: c_icon_equal(4)          VALUE icon_led_green,
           c_icon_diff(4)           VALUE icon_led_yellow,
           c_icon_err(4)            VALUE icon_led_red,
           c_icon_none(4)           VALUE icon_dummy.
CONSTANTS: c_f2code    LIKE sy-ucomm       VALUE '&ETA'.
*---start#2  wskim
RANGES: s_eindt FOR eket-eindt.
*r_hkont for bsis-hkont,
*---end

*- U1 start
DATA: gt_ekko_a TYPE TABLE OF ekko WITH HEADER LINE,
      gt_ekpo_a TYPE TABLE OF ekpo WITH HEADER LINE,
      gt_eket_a TYPE TABLE OF eket WITH HEADER LINE,
      gt_bkpf_a TYPE TABLE OF bkpf WITH HEADER LINE,
      gt_bseg_a TYPE TABLE OF bseg WITH HEADER LINE.
*- U1 End

**----------------------------------------------------------------------
* SELECTION-SCREEN
*----------------------------------------------------------------------

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-016.
PARAMETERS : p_bukrs LIKE bsis-bukrs MEMORY ID buk.
PARAMETERS : p_hkont LIKE bsas-hkont MEMORY ID sak.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  (10) text-013.
SELECTION-SCREEN POSITION 12.
PARAMETERS  p_open  TYPE c  RADIOBUTTON GROUP ra01
                    USER-COMMAND ucom.
SELECTION-SCREEN COMMENT  20(10) text-014.
SELECTION-SCREEN POSITION 32.
PARAMETERS  p_post TYPE c  RADIOBUTTON GROUP ra01.
SELECTION-SCREEN END OF LINE.
PARAMETERS : p_augdt LIKE bsas-augdt DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-002.
SELECT-OPTIONS :
             s_budat   FOR bsis-budat MODIF ID sbu,
             s_zuonr   FOR bsis-zuonr,
             s_matnr2  FOR mseg-matnr.
SELECTION-SCREEN END OF BLOCK bl3.


PARAMETERS: p_read  RADIOBUTTON GROUP r2 DEFAULT 'X',
            p_upd   RADIOBUTTON GROUP r2,
            p_del   RADIOBUTTON GROUP r2,
            p_dir   RADIOBUTTON GROUP r2.

PARAMETERS: p_det AS CHECKBOX DEFAULT 'X'.

*selection-screen begin of block bl3 with frame title text-002.
*selection-screen begin of line.
*selection-screen comment  18(5) text-003.
*selection-screen position 25.
*parameters : pi_a type c as checkbox.
*selection-screen comment  33(4) text-004.
*selection-screen position 38.
*parameters :pi_b type c as checkbox.
*selection-screen comment  42(7) text-005.
*selection-screen position 55.
*parameters : pi_c type c as checkbox.
*selection-screen comment  58(12) text-006.
*selection-screen position 75.
*parameters :  pi_d type c as checkbox.
*selection-screen end of line.
*selection-screen begin of line.
*selection-screen comment  18(5) text-007.
*selection-screen position 25.
*parameters  pl_a type c   as checkbox.
*selection-screen comment  33(4) text-008.
*selection-screen position 38.
*parameters  pl_b type c   as checkbox.
*selection-screen comment  42(12) text-009.
*selection-screen position 55.
*parameters  pl_c type c   as checkbox.
*selection-screen comment  58(16) text-010.
*selection-screen position 75.
*parameters  pl_d type c  as checkbox.
*selection-screen end of line.
*select-options: s_hkont for ska1-saknr NO INTERVALS .
*selection-screen end of block bl3.


*selection-screen begin of block bl4 with frame title text-015.
*    s_eindt FOR eket-eindt NO INTERVALS NO-EXTENSION.
*             s_grdate FOR bsis-budat MODIF ID sgr.
*selection-screen end of block bl4.
*selection-screen skip 1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-000.
SELECT-OPTIONS :
             s_lifnr FOR ekko-lifnr,
             s_matnr FOR mseg-matnr,
             s_ebeln FOR eket-ebeln,
             s_ebelp FOR eket-ebelp,
             s_mtart FOR ekpo-mtart DEFAULT 'ROH'.
SELECTION-SCREEN END OF BLOCK b1.


*PARAMETERS: p_import  AS CHECKBOX default 'X',
*            p_zero    AS CHECKBOX,
*            p_idate   AS CHECKBOX,  "show date information
*            p_ipo     AS CHECKBOX,  "show po data.
*            p_bseg    AS CHECKBOX.
PARAMETERS: p_layout LIKE ltdx-variant.

*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End
*----------------------------------------------------------------------
* INITIALIZATION
*----------------------------------------------------------------------
INITIALIZATION.
  PERFORM initial.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_hkont.
  PERFORM get_help_p_hkont.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM selection_output.

AT SELECTION-SCREEN ON RADIOBUTTON GROUP ra01.
  PERFORM selection_on_radiobutton.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN .
  PERFORM selection_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM search_help_layout.

AT USER-COMMAND.

*---------------------------------------------------------------------
*    M   A   I   N
*---------------------------------------------------------------------
START-OF-SELECTION.
  SELECT SINGLE * FROM t001 WHERE bukrs = p_bukrs.
  gc = t001-waers.
*READ MIT Account
  PERFORM get_mit_account.

  IF p_upd EQ 'X' .
    PERFORM select_data.
    IF gt_data[] IS INITIAL.
      MESSAGE s000(zmfi) WITH 'No data '.
      EXIT.
    ELSE.
      PERFORM save_to_buffer.
    ENDIF.

  ELSEIF p_del EQ 'X' .
    PERFORM delete_buffer.

  ELSEIF p_read EQ 'X'.
    PERFORM get_from_buffer.
    PERFORM show_records.

  ELSE.

    PERFORM select_data.
    PERFORM show_records.
  ENDIF.


*END-OF-SELECTION.

*&---------------------------------------------------------------------
*&      Form  select_data
*&---------------------------------------------------------------------
FORM select_data.
  REFRESH: gt_bsis,gt_data.

  PERFORM get_data_bsis_bsas.   " USING l_hkont.
  PERFORM get_bseg_data.

  PERFORM get_po_item_list.
  PERFORM process_open_only.

  DESCRIBE TABLE gt_data LINES sy-index.
  CHECK sy-index > 0.

  PERFORM get_all_po.

  PERFORM put_other_info.

ENDFORM.                    " select_data
*---------------------------------------------------------------------*
*  FORM alv_event_pf_status_set
*---------------------------------------------------------------------*
FORM alv_event_pf_status_set USING rt_extab TYPE slis_t_extab.
                                                            "#EC *
  IF wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
    SET PF-STATUS 'STANDARD_GRID' ."EXCLUDING rt_extab.
  ELSE.
    SET PF-STATUS 'STANDARD' ."EXCLUDING rt_extab.
  ENDIF.
  SET TITLEBAR  'STANDARD'.
ENDFORM.                    "alv_event_pf_status_set
*---------------------------------------------------------------------*
*  FORM alv_event_user_command
*---------------------------------------------------------------------*
FORM alv_event_user_command USING r_ucomm     LIKE sy-ucomm
                                      rs_selfield TYPE slis_selfield.
                                                            "#EC *
  DATA : seltab LIKE rsparams OCCURS 0 WITH HEADER LINE.
  DATA : messtab LIKE  bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  REFRESH seltab.
  CASE r_ucomm.
*   ---------------------------------- processing on double click.
    WHEN '&IC1'.
      READ TABLE gt_data INDEX rs_selfield-tabindex.
      CASE rs_selfield-fieldname.
        WHEN 'EBELN'
          OR 'EBELP'                                        "HIS20094
          OR 'MATNR'.                                       "HIS20094

*          seltab-selname = 'EN_EBELN'.
*          seltab-sign =  'I'.
*          seltab-option = 'EQ'.
*          seltab-low = gt_data-ebeln.
*          APPEND seltab.
*
***         PERFORM bdc_nodata  USING nodata.
* Begin of HIS20094
*          refresh bdcdata.
*          perform bdc_dynpro      using 'RM06EN00' '1000'.
*          perform bdc_field       using 'BDC_OKCODE'
*                                        '/EGET'.
*
*          perform bdc_dynpro      using 'SAPMSSY0' '0120'.
*          perform bdc_field       using 'BDC_CURSOR'
*                                        '04/09'.
*          perform bdc_field       using 'BDC_OKCODE'
*                                        '=PICK'.
*
*          perform bdc_dynpro      using 'RM06EN00' '1000'.
*          perform bdc_field       using 'BDC_OKCODE'
*                                        '=DYNS'.
*          perform bdc_field       using 'EN_EBELN-LOW'
*                                        gt_data-ebeln.
*          perform bdc_field       using 'LISTU'
*                                        'ALLES'.
*
*          perform bdc_dynpro      using 'RM06EN00' '1000'.
*          perform bdc_field       using 'BDC_OKCODE'
*                                        '=ONLI'.
*          perform bdc_field       using 'EN_EBELN-LOW'
*                                        gt_data-ebeln.
*          perform bdc_field       using 'LISTU'
*                                        'ALLES'.
*          perform bdc_field     using '%%DYN001-LOW'
*                                       gt_data-ebelp.
*
**          PERFORM bdc_dynpro      USING 'SAPMSSY0' '0120'.
**          PERFORM bdc_field       USING 'BDC_OKCODE'
**                                        '/00'.
*
*          perform bdc_transaction tables messtab
*                                   using 'ME2N'
*                                          mode.

          CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
            EXPORTING
              i_ebeln = gt_data-ebeln
              i_ebelp = gt_data-ebelp
              i_enjoy = 'X'
            EXCEPTIONS
              OTHERS  = 1.
          IF sy-subrc NE 0.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

* End of HIS20094

      ENDCASE.
*---------------------------------- switching view type grid or list
    WHEN 'LIST' OR 'GRID'.
      PERFORM switch_list_or_grid USING r_ucomm.
  ENDCASE.

  CHECK r_ucomm EQ 'LIST' OR
        r_ucomm EQ 'GRID'.

  rs_selfield-exit = 'X'.

ENDFORM.                    "alv_event_user_command
*&---------------------------------------------------------------------
*&      Form  set_variant
*&---------------------------------------------------------------------
FORM set_variant CHANGING cs_vari TYPE disvariant.

  CHECK p_layout NE space.

  cs_vari-report      = sy-repid.
  cs_vari-handle      = space.
  cs_vari-log_group   = space.
  cs_vari-username    = space.
  cs_vari-variant     = p_layout.
  cs_vari-text        = space.
  cs_vari-dependvars  = space.

ENDFORM.                    " set_variant

*&---------------------------------------------------------------------
*&      Form  set_events
*&---------------------------------------------------------------------
FORM set_events CHANGING ct_events TYPE slis_t_event.

  FIELD-SYMBOLS: <ls_event> TYPE slis_alv_event.

  DATA: l_event TYPE lvc_fname.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 0
    IMPORTING
      et_events       = ct_events
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    DELETE ct_events WHERE name NE 'END_OF_PAGE'
                       AND name NE 'TOP_OF_PAGE'
                       AND name NE 'TOP_OF_LIST'
                       AND name NE 'END_OF_LIST'.
    LOOP AT ct_events ASSIGNING <ls_event>.
      CONCATENATE 'ALV_EVENT_'
                  <ls_event>-name
                  INTO <ls_event>-form.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " f01_set_evts


*&---------------------------------------------------------------------
*&      Form  set_layout
*&---------------------------------------------------------------------
FORM set_layout CHANGING cs_layo TYPE slis_layout_alv.

*... Display options
  cs_layo-colwidth_optimize      = space.
  cs_layo-no_colhead             = space.
  cs_layo-no_hotspot             = space.
  cs_layo-zebra                  = ' '.
  cs_layo-no_vline               = space.
  cs_layo-cell_merge             = space.
  cs_layo-no_min_linesize        = space.
  cs_layo-min_linesize           = space.
  cs_layo-max_linesize           = space.
  cs_layo-window_titlebar        = space.
  cs_layo-no_uline_hs            = space.
*... Edit
  cs_layo-edit                   = ' '."space.
  cs_layo-edit_mode              = ' '."space.
*... Exceptions
  cs_layo-lights_fieldname       = ' '.
  "=> ??? ??? ???
  cs_layo-lights_tabname         = space.
  cs_layo-lights_rollname        = space.
  cs_layo-lights_condense        = space.
*... Sums
  cs_layo-no_sumchoice           = space.
  cs_layo-no_totalline           = space.
  cs_layo-totals_before_items    = space.
  cs_layo-totals_only            = space.
  cs_layo-totals_text            = space.
  cs_layo-no_subchoice           = space.
  cs_layo-no_subtotals           = space.
  cs_layo-subtotals_text         = space.
  cs_layo-numc_sum               = 'X'.
  cs_layo-no_unit_splitting      = space.
*... Interaction
  cs_layo-box_fieldname          = 'CHKBOX'.
  cs_layo-box_tabname            = space.
  cs_layo-box_rollname           = space.
  cs_layo-expand_fieldname       = space.
  cs_layo-hotspot_fieldname      = space.
  cs_layo-no_input               = ' '.
  cs_layo-f2code                 = space.
  cs_layo-confirmation_prompt    = space.
  cs_layo-key_hotspot            = space.
  cs_layo-flexible_key           = space.
  cs_layo-reprep                 = space.
  cs_layo-group_buttons          = 'X'.
  cs_layo-no_keyfix              = space.
  cs_layo-get_selinfos           = space.
  cs_layo-group_change_edit      = 'X'.
  cs_layo-no_scrolling           = space.
  cs_layo-expand_all             = space.
  cs_layo-no_author              = space.
*... Detailed screen
  cs_layo-detail_popup           = 'X'.
  cs_layo-detail_initial_lines   = space.
  cs_layo-detail_titlebar        = space.
*... PF-status
  cs_layo-def_status             = space.
*... Display variants
  cs_layo-header_text            = space.
  cs_layo-item_text              = space.
  cs_layo-default_item           = space.
*... colour
  cs_layo-info_fieldname         = space.
*  cs_layo-coltab_fieldname       = 'TABCOLOR'.            "HIS20094
*... others
  cs_layo-list_append            = space.

ENDFORM.                    " set_layout
*---------------------------------------------------------------------*
*  FORM f01_alv_event_top_of_page
*---------------------------------------------------------------------*
FORM alv_event_top_of_page.                                 "#EC CALLED
*  WRITE : /(10) 'nvestment Program' , p_prnam.
*          /(10) 'BBBBBBB',  BKPF-BUKRS INVERSE COLOR 1 INPUT ON,
*           (20) 'CCCCCCC',  BKPF-BELNR INPUT ON.
ENDFORM.                    "alv_event_top_of_page

*---------------------------------------------------------------------*
*       FORM alv_event_top_of_LIST                                    *
*---------------------------------------------------------------------*
FORM alv_event_top_of_list.                                 "#EC CALLED


ENDFORM.                    "alv_event_top_of_page

*---------------------------------------------------------------------*
*  FORM f01_alv_event_end_of_page
*---------------------------------------------------------------------*
FORM alv_event_end_of_page.
*
ENDFORM.                    "alv_event_end_of_page

*---------------------------------------------------------------------*
*  FORM f01_alv_event_end_of_list
*---------------------------------------------------------------------*
FORM alv_event_end_of_list.


ENDFORM.                    "alv_event_end_of_list
*&---------------------------------------------------------------------*
*&      Form  dispaly_heager
*----------------------------------------------------------------------*
FORM display_header.
* Begin of HIS20094
  DATA: w_top TYPE slis_listheader.

  REFRESH w_top_of_page.
  w_top-typ  = 'S'.
  w_top-key  = 'Company code:'.
  WRITE p_bukrs TO w_top-info.
  APPEND w_top TO w_top_of_page.

  w_top-typ  = 'S'.
  w_top-key  = 'G/L account:'.
  WRITE p_hkont TO w_top-info.
  APPEND w_top TO w_top_of_page.

  w_top-typ  = 'S'.
  w_top-key  = 'Open at key date:'.
  WRITE p_augdt TO w_top-info.
  APPEND w_top TO w_top_of_page.

* End of HIS20094

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
*     i_logo             = 'Z_HYUNDAI_LOGO'
*     i_logo             = 'ENJOYSAP_LOGO'
      it_list_commentary = w_top_of_page.
ENDFORM.                    " top_of_page

*&---------------------------------------------------------------------
*&      Form  switch_list_or_grid
*&---------------------------------------------------------------------
FORM switch_list_or_grid USING r_ucomm.

  DATA: ls_vari      TYPE disvariant,
       ls_slis_layo TYPE slis_layout_alv,
       lt_slis_fcat TYPE slis_t_fieldcat_alv,
       lt_slis_sort TYPE slis_t_sortinfo_alv,
       lt_slis_filt TYPE slis_t_filter_alv,
       ls_slis_prnt TYPE slis_print_alv.


  IF r_ucomm = 'LIST' AND
     wa_alv_function_name = 'REUSE_ALV_LIST_DISPLY'.
    EXIT.
  ENDIF.
  IF r_ucomm = 'GRID' AND
     wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
    EXIT.
  ENDIF.
  CASE wa_alv_function_name.
    WHEN 'REUSE_ALV_LIST_DISPLAY'.
      wa_alv_get_info_name = 'REUSE_ALV_LIST_LAYOUT_INFO_GET'.
    WHEN 'REUSE_ALV_GRID_DISPLAY'.
      wa_alv_get_info_name = 'REUSE_ALV_GRID_LAYOUT_INFO_GET'.

  ENDCASE.

  CALL FUNCTION wa_alv_get_info_name
    IMPORTING
      es_layout     = ls_slis_layo
      et_fieldcat   = lt_slis_fcat
      et_sort       = lt_slis_sort
      et_filter     = lt_slis_filt
      es_variant    = ls_vari
    EXCEPTIONS
      no_infos      = 1
      program_error = 2
      OTHERS        = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  IF r_ucomm = 'LIST'.
    wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
    CALL FUNCTION wa_alv_function_name
      EXPORTING
        i_callback_program       = wa_repid
        i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
        i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
        is_layout                = ls_slis_layo
        it_fieldcat              = lt_slis_fcat
        it_sort                  = lt_slis_sort
        it_filter                = lt_slis_filt
        i_default                = ' '  "gs_test-vari_default
        i_save                   = wa_var_save
        is_variant               = ls_vari
        is_print                 = ls_slis_prnt
        it_events                = gt_events[]
      TABLES
        t_outtab                 = gt_data
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
  ENDIF.
  IF r_ucomm = 'GRID'.
    wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
    CALL FUNCTION wa_alv_function_name
      EXPORTING
        i_callback_program       = wa_repid
        i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
        i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
        is_layout                = ls_slis_layo
        it_fieldcat              = lt_slis_fcat
        it_sort                  = lt_slis_sort
        it_filter                = lt_slis_filt
        i_default                = ' '  "gs_test-vari_default
        i_save                   = wa_var_save
        is_variant               = ls_vari
        is_print                 = ls_slis_prnt
*       it_events                = gt_events[]
      TABLES
        t_outtab                 = gt_data
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.

  ENDIF.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " switch_list_or_grid
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM field_setting TABLES p_fieldcat_t LIKE gt_fieldcat USING
                                  p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO gt_fieldcat.


ENDFORM.                    " fill_field_category

*&---------------------------------------------------------------------*
*&      Form  f4_variant
*&---------------------------------------------------------------------*
FORM f4_variant CHANGING c_variant TYPE disvariant-variant.

  DATA: ls_variant TYPE disvariant,
        l_exit     TYPE char1.

  ls_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant          = ls_variant
      i_save              = 'A'
*     it_default_fieldcat =
    IMPORTING
      e_exit              = l_exit
      es_variant          = ls_variant
    EXCEPTIONS
      not_found           = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF l_exit EQ space.
      c_variant = ls_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " f4_variant
*&---------------------------------------------------------------------*
*&      Form  build_sort_table
*&---------------------------------------------------------------------*
FORM build_sort_table USING  p_spos
                             p_fieldname
                             p_up
                             p_subtot
                             p_group.
  DATA: ls_sort TYPE slis_sortinfo_alv.

  ls_sort-spos      = p_spos.
  ls_sort-fieldname = p_fieldname.
  ls_sort-up        = p_up.
  ls_sort-subtot    = p_subtot.
  ls_sort-group     = p_group.
  APPEND ls_sort TO gt_sorts.
ENDFORM.                    " build_sort_table
*&---------------------------------------------------------------------*
*&      Form  set_line_color
*&---------------------------------------------------------------------*
FORM set_line_color USING    p_color.
  DATA: ls_fieldcat   TYPE slis_fieldcat_alv,
        lt_color      TYPE slis_t_specialcol_alv,
        ls_color      TYPE slis_specialcol_alv.

  REFRESH lt_color.
  CLEAR   lt_color.
  LOOP AT gt_fieldcat INTO ls_fieldcat.
    ls_color-fieldname = ls_fieldcat-fieldname.
    ls_color-color-col = p_color.
*    "cl_gui_resources=>list_col_positive.
    ls_color-color-int = cl_gui_resources=>list_intensified.
    ls_color-color-inv = 0.
    ls_color-nokeycol  = 'X'.
    APPEND ls_color TO lt_color.
*    gt_data-tabcolor = lt_color.
  ENDLOOP.

ENDFORM.                    " set_line_color
*&---------------------------------------------------------------------*
*&      Form  build_field_category1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0634   text
*      -->P_0635   text
*      -->P_0636   text
*      -->P_0637   text
*      -->P_0638   text
*      -->P_0639   text
*      -->P_0640   text
*      -->P_0641   text
*      -->P_0642   text
*----------------------------------------------------------------------*
*FORM build_field_category1 USING
*                                  p_fieldname       " field name
*                                  p_title           " field title
*                                  p_outputlen       " length
*                                  p_key             "
*                                  p_just            "
*                                  p_noout           "
*                                  p_edit            "
*                                  p_cfield          " currency field
*                                  p_qfield          " quantity field
*                                  .
*
*  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
*  CLEAR ls_fieldcat.
*  ls_fieldcat-fieldname = p_fieldname.
*  ls_fieldcat-seltext_l = p_title.
*  ls_fieldcat-outputlen = p_outputlen.
*  ls_fieldcat-key       = p_key.
*  ls_fieldcat-just      = p_just.
*  ls_fieldcat-edit      = p_edit.
*  ls_fieldcat-no_out     = p_noout.
*  ls_fieldcat-cfieldname = p_cfield.
*  ls_fieldcat-qfieldname = p_qfield.
**  if p_fieldname = 'KUNNR'.
**    ls_fieldcat-emphasize = 'C100'.
**  endif.
*  APPEND ls_fieldcat TO gt_fieldcat.
*ENDFORM.                    " build_field_category1
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
*-------------- HEADER
*  CLEAR ls_line.
*  ls_line-typ  = 'H'.
*  ls_line-info = text-h01.     "HEADER TITLE (H001)
*  APPEND ls_line TO lt_top_of_page.
*
*  ls_line-typ  = 'S'.
*  ls_line-key  = 'Investment program : '.
*  ls_line-info = p_prnam.
*  APPEND ls_line TO lt_top_of_page.
**--
*  ls_line-typ  = 'S'.
*  ls_line-key  = 'Approval Year : '.
**  ls_line-info = p_ayear.
*  APPEND ls_line TO lt_top_of_page.
**--
*  ls_line-typ  = 'S'.
*  ls_line-key  = 'Order no : '.
*  CONCATENATE   s_aufnr-low  ' ~'  s_aufnr-high INTO l_list.
*  ls_line-info = l_list.
*  APPEND ls_line TO lt_top_of_page.
*
*
ENDFORM.                    " comment_build
*&---------------------------------------------------------------------*
*&      Form  set_build_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_build_event.
  w_eventcat-name = 'TOP_OF_PAGE'.
  w_eventcat-form = 'DISPLAY_HEADER'.
  APPEND w_eventcat.
ENDFORM.                    " set_build_event
*&---------------------------------------------------------------------*
*&      Form  selection_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selection_screen.
*---start #2 wskim
*  perform ranges_account.
*---end

  IF s_lifnr-low <> space.
    SELECT SINGLE * FROM lfa1
     WHERE lifnr EQ s_lifnr-low.
    IF sy-subrc <> 0.
      MESSAGE i000 WITH 'Check Vendor number' s_lifnr-low.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " selection_screen
*&---------------------------------------------------------------------*
*&      Form  initial
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initial.
* ==> Change Variant saving type
  wa_var_save = 'A'.
* ==> Change first mode   GRID or LIST
  wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
* wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
  REFRESH : gt_fieldcat.
  CLEAR   : gs_layout.
  wa_repid = sy-repid.

  DATA: l_gjahr TYPE t009b-bdatj,
        l_poper TYPE t009b-poper.

  l_gjahr = sy-datum(4).
  l_poper = sy-datum+4(2).

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr = l_gjahr
      i_poper = l_poper
      i_periv = 'K0'
    IMPORTING
      e_date  = p_augdt.

ENDFORM.                    " initial
*&---------------------------------------------------------------------*
*&      Form  selection_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selection_output.
  EXIT.

  CASE 'X'.
    WHEN p_open.
      LOOP AT SCREEN.
        IF screen-name EQ 'S_BUDAT-LOW' OR
           screen-name EQ 'S_BUDAT-HIGH'.
          screen-input = '0'.
        ELSEIF screen-name EQ 'S_GRDATE-LOW' OR
         screen-name EQ 'S_GRDATE-HIGH'.
          screen-input = '0'.
        ELSEIF screen-name EQ 'P_AUGDT'.
          screen-input = '1'.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN p_post.
      LOOP AT SCREEN.
        IF screen-name EQ 'S_BUDAT-LOW' OR
           screen-name EQ 'S_BUDAT-HIGH'.
          screen-input = '1'.
        ELSEIF screen-name EQ 'S_GRDATE-LOW' OR
         screen-name EQ 'S_GRDATE-HIGH'.
          screen-input = '1'.
        ELSEIF screen-name EQ 'P_AUGDT'.
          screen-input = '0'.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
  ENDCASE.


ENDFORM.                    " selection_output
*&---------------------------------------------------------------------*
*&      Form  GET DATA BSIS BSAS
*&---------------------------------------------------------------------*
FORM get_data_bsis_bsas ."USING l_hkont.
  DATA: l_ebeln LIKE ekpo-ebeln,
        l_ebelp LIKE ekpo-ebelp.
  IF NOT s_matnr2[] IS INITIAL.
    s_zuonr-sign = 'I'.
    s_zuonr-option = 'EQ'.
    SELECT ebeln ebelp INTO (l_ebeln, l_ebelp)
      FROM ekpo
      WHERE matnr IN s_matnr2.
      CONCATENATE l_ebeln l_ebelp INTO s_zuonr-low.
      APPEND s_zuonr.
    ENDSELECT.
  ENDIF.

  REFRESH gt_bsis.

  SELECT *
     INTO CORRESPONDING FIELDS OF TABLE gt_bsis
     FROM bsis AS b
       INNER JOIN bkpf AS a
       ON  a~bukrs = b~bukrs
       AND a~belnr = b~belnr
       AND a~gjahr = b~gjahr
   WHERE b~bukrs =  p_bukrs
     AND b~hkont =  p_hkont
     AND b~budat =< p_augdt
     AND b~budat IN s_budat
     AND b~zuonr IN s_zuonr.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_bkpf_2.
  ENDIF.
*- U1 End

  IF p_open = 'X'.
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE gt_bsis
        FROM bsas AS b
          INNER JOIN bkpf AS a
         ON  a~bukrs = b~bukrs
         AND a~belnr = b~belnr
         AND a~gjahr = b~gjahr
          WHERE b~bukrs =  p_bukrs
            AND b~hkont =  p_hkont
            AND b~augdt >  p_augdt
            AND b~budat <= p_augdt
            AND b~budat <= p_augdt
            AND b~budat IN s_budat
            AND b~zuonr IN s_zuonr.
*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_bkpf_3 USING p_open.
    ENDIF.
*- U1 End
  ELSE.
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE gt_bsis
        FROM bsas AS b
          INNER JOIN bkpf AS a
         ON  a~bukrs = b~bukrs
         AND a~belnr = b~belnr
         AND a~gjahr = b~gjahr
          WHERE b~bukrs =  p_bukrs
            AND b~hkont =  p_hkont
            AND b~budat <= p_augdt
            AND b~budat <= p_augdt
            AND b~budat IN s_budat
            AND b~zuonr IN s_zuonr.
*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_bkpf_3 USING p_open.
    ENDIF.
*- U1 End
  ENDIF.
ENDFORM.                    "get_data_bsis_bsas
*&---------------------------------------------------------------------*
*&      Form  GET_BSIS_BSEG
*&---------------------------------------------------------------------*
FORM get_bseg_data.
  DATA: l_idx LIKE sy-tabix.
  CLEAR : z_awtyp.

  LOOP AT gt_bsis.
    l_idx = sy-tabix.

    IF gt_bsis-zuonr+10(5) = '00000' OR gt_bsis-zuonr+10(5) IS INITIAL.
      MESSAGE s000 WITH 'ZUONR is blank:' gt_bsis-gjahr gt_bsis-belnr.
      CONTINUE.
    ELSE.
      gt_data-hkont = gt_bsis-hkont.
      gt_data-ebeln = gt_bsis-zuonr(10).
      gt_data-ebelp = gt_bsis-zuonr+10(5).
      gt_data-zuonr = gt_bsis-zuonr.
    ENDIF.

    SELECT SINGLE menge INTO (gt_bsis-menge)
      FROM bseg
       WHERE bukrs EQ p_bukrs  AND
             belnr EQ gt_bsis-belnr AND
             gjahr EQ gt_bsis-gjahr AND
             buzei EQ gt_bsis-buzei.
*- U1 Start
    IF p_arch EQ 'X' AND sy-subrc <> 0.
      PERFORM archive_read_bseg.
    ENDIF.
*- U1 End

    IF gt_bsis-shkzg = 'H'.
      gt_bsis-dmbtr = gt_bsis-dmbtr * -1 .
      gt_bsis-menge = gt_bsis-menge * -1 .
    ENDIF.
*    clear gt_bsis-shkzg.
*    modify gt_bsis  index l_idx transporting ebeln ebelp shkzg dmbtr.


    CASE gt_bsis-awtyp.
      WHEN 'MKPF'. "GR
        gt_data-gr_qty = gt_bsis-menge .
        gt_data-gr_amt = gt_bsis-dmbtr.

        MOVE-CORRESPONDING gt_bsis TO gt_housebl.           "HIS20094
        COLLECT gt_housebl. CLEAR gt_housebl.               "HIS20094

      WHEN 'RMRP'.     "invoice
        IF gt_bsis-tcode EQ 'MRER'.
          gt_data-a_menge = gt_bsis-menge.
          gt_data-a_dmbtr = gt_bsis-dmbtr.
        ELSEIF gt_bsis-tcode EQ 'MRNB'.  "revaluation
          CLEAR gt_bsis-menge.
          gt_data-a_dmbtr = gt_bsis-dmbtr.
        ELSEIF gt_bsis-tcode = 'MR11'.   "GRIR maintain -> BalanceQty
          gt_data-o_menge = gt_bsis-menge.
          gt_data-o_dmbtr = gt_bsis-dmbtr.
        ELSE.
          gt_data-m_menge = gt_bsis-menge.
          gt_data-m_dmbtr = gt_bsis-dmbtr.
        ENDIF.

      WHEN OTHERS.
        gt_data-o_dmbtr = gt_bsis-dmbtr.
    ENDCASE.

* Balance
    gt_data-b_menge = gt_bsis-menge.
    gt_data-b_dmbtr = gt_bsis-dmbtr.

    COLLECT gt_data. CLEAR gt_data.
  ENDLOOP.

ENDFORM.                    " GET_BSIS_BSEG
*&---------------------------------------------------------------------*
*&      Form  set_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TEMP  text
*----------------------------------------------------------------------*
*form set_status tables  it_temp structure it_ekbe
*                          changing c_signal.
*  data : f_sub_sum like it_temp-menge.
*
**Compare
*  sort it_temp by ebeln ebelp.
*  data : it_temp_po like it_temp occurs 0 with header line,
*         iv_qty like it_temp-menge,
*         gr_qty like it_temp-menge.
*  refresh it_temp_po. clear:iv_qty,gr_qty,f_sub_sum.
*  it_temp_po[] = it_temp[].
*  delete adjacent duplicates from it_temp_po comparing ebeln ebelp.
*
*  loop at it_temp_po.
*    loop at it_temp where ebeln = it_temp_po-ebeln
*                      and ebelp = it_temp_po-ebelp.
*      if it_temp-vgabe = '2'.
*        if it_temp-shkzg eq 'S'.
*          it_temp-menge = it_temp-menge * -1.
*        endif.
*        iv_qty = iv_qty + it_temp-menge.
*      elseif it_temp-vgabe = '1'.
*        if it_temp-shkzg eq 'S'.
*          it_temp-menge = it_temp-menge * -1.
*        endif.
*        gr_qty = gr_qty + it_temp-menge.
*      endif.
*    endloop.
*
*    if  p_det eq 'X'.
*      if iv_qty =< gr_qty.
*        c_signal =  c_icon_err.
*      else.
*        c_signal =  c_icon_equal.
*      endif.
*    else. " p_local EQ 'X'.
*      if iv_qty = gr_qty.
**        c_signal =  c_icon_err.
*        c_signal =  c_icon_equal.
*      else.
**        c_signal =  c_icon_equal.
*        c_signal =  c_icon_err.
*
*      endif.
*    endif.
*    clear :iv_qty,gr_qty,f_sub_sum.
*  endloop.
*
*
*endform.                    " set_status
*&---------------------------------------------------------------------*
*&      Form  BDC_NODATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODATA  text
*----------------------------------------------------------------------*
FORM bdc_nodata USING p_nodata.

  nodata_character = p_nodata.

ENDFORM.                    " BDC_NODATA
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0420   text
*      -->P_0421   text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    " bdc_dynpro
*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0425   text
*      -->P_0426   text
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
*  IF fval <> nodata_character.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
*  ENDIF.

ENDFORM.                    " bdc_field
*&---------------------------------------------------------------------*
*&      Form  bdc_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MESSTAB  text
*      -->P_0494   text
*      -->P_0495   text
*      -->P_MODE  text
*      -->P_0497   text
*----------------------------------------------------------------------*
FORM bdc_transaction TABLES p_messtab
                     USING  p_tcode
                            p_mode.

  DATA: l_subrc LIKE sy-subrc.

  CALL TRANSACTION p_tcode USING bdcdata
                   MODE   p_mode
*                     UPDATE p_update
                   MESSAGES INTO p_messtab.
  l_subrc = sy-subrc.
  REFRESH bdcdata.
  sy-subrc = l_subrc.

ENDFORM.                    " bdc_transaction
*&---------------------------------------------------------------------*
*&      Form  initial_build_field_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initial_build_field_category.
  PERFORM field_setting TABLES gt_fieldcat USING :
   'LIFNR'     'Vendor'         '10' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
*  'HKONT'     'Account'        '10' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
   'EBELN'     'PO Number'      '10' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
   'EBELP'     'Item No'        '5'  'X' 'L'  ' '  ' '  '  ' ' '  ' ',
   'MATNR'     'Material'       '10' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
   'ICON'      'Clr'            '3'  'X' 'C'  ' '  ' '  '  ' ' '  ' ',
   'STAWN'     'Duty code'      '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'ZFBLNO'    'B/L Doc'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',

* Begin of HIS20094
   'XBLNR'     'House B/L No'   '16' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
* End of HIS20094

   'POQTY'     'PO Quantity'    '09' ' ' 'R'  ' '  '0'  '  ' ' '  ' ',
   'INBQT'     'INB.QTY'        '09' ' ' 'R'  ' '  '0'  '  ' ' '  ' ',
   'OBMNG'     'OpenDel.'       '09' ' ' 'R'  ' '  '0'  '  ' ' '  ' ',
   'GR_QTY'    'GR qty'         '09' ' ' 'R'  ' '  '0'  '  ' ' '  'X',
   'A_MENGE'   'A.IV qty'       '09' ' ' 'R'  ' '  '0'  '  ' ' '  'X',
   'M_MENGE'   'M.IV qty'       '09' ' ' 'R'  ' '  '0'  '  ' ' '  'X',
   'O_MENGE'   'OtherQty'       '09' ' ' 'R'  ' '  '0'  '  ' ' '  'X',
   'B_MENGE'   'OpenInvQty'     '09' ' ' 'R'  ' '  '0'  '  ' ' '  'X',
   'GR_AMT'    'GR Amount'      '09' ' ' 'R'  ' '  ' '  gc   ' '  'X',
   'A_DMBTR'   'A.IV Amt'       '09' ' ' 'R'  ' '  ' '  gc   ' '  'X',
   'M_DMBTR'   'M.IV Amt'       '09' ' ' 'R'  ' '  ' '  gc   ' '  'X',
   'O_DMBTR'   'Oth.Acct'       '09' ' ' 'R'  ' '  ' '  gc   ' '  'X',
   'W_DMBTR'   'Write off'      '09' ' ' 'R'  ' '  ' '  gc   ' '  'X',
   'B_DMBTR'   'Balance Amt'    '09' ' ' 'R'  ' '  ' '  gc   ' '  'X',
   'ACT_AMT'   'Actual  Amt'    '09' ' ' 'R'  ' '  ' '  gc   ' '  'X',
   'BEDAT'     'PO Date'        '09' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'EINDT'     'LstDlvSch'      '10' ' ' 'L'  ' '  '0'  '  ' ' '  'X',
   'GR_DATE'   'LstGR Dt'       '10' ' ' 'L'  ' '  '0'  '  ' ' '  'X',
   'BUDAT'     'LstPstDt'       '10' ' ' 'L'  ' '  '0'  '  ' ' '  'X',
   'ZFBLDT'    'LstB/LDt'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'ZFETA'     'Lst ETA'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
*  'BLART'     'Document type'   '3' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'OP_DAT'    'Fst OpenDt'     '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'BSART'     'POTy'           '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'XERSY'     'ERS'            '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.

ENDFORM.                    " initial_build_field_category
*&---------------------------------------------------------------------*
*&      Form  ranges_account
*&---------------------------------------------------------------------*
*form ranges_account.
*  data: i_ska1 like ska1 occurs 0 with header line.
*  refresh r_hkont.clear r_hkont.
*
*  r_hkont-sign = 'I'.
*  r_hkont-option = 'EQ'.
*
*  if pi_a eq 'X' or  pi_b eq 'X' or pi_c eq 'X' or pi_d eq 'X'
*       or pl_a eq 'X' or pl_b eq 'X' or pl_c eq 'X' or pl_d eq 'X'.
*
*    if pi_a eq 'X'. " KD
*      r_hkont-low = '0000138000'.
*      append r_hkont.
*    endif.
*    if pi_b eq 'X'. "Duty
*      r_hkont-low = '0000215010'.
*      append r_hkont.
*    endif.
*    if pi_c eq 'X'. "Freight
*      r_hkont-low = '0000138200'.
*      append r_hkont.
*    endif.
*    if pi_d eq 'X'. "Other Charge
*      r_hkont-low = '0000138400'.
*      append r_hkont.
*    endif.
*    if pl_a eq 'X'. "JIT
*      r_hkont-low = '0000211100'.
*      append r_hkont.
*    endif.
*    if pl_b eq 'X'. "JIS GRIR
*      r_hkont-low = '0000211200'.
*      append r_hkont.
*    endif.
*    if  pl_c eq 'X'. "LocalFreight
*      r_hkont-low = '0000211110'.
*      append r_hkont.
*    endif.
*    if pl_d eq 'X'. "Non Pro.Material
*      r_hkont-low  = '0000215070'.
*      append r_hkont.
*    endif.
*  else.
*    describe table p_hkont lines sy-tabix.
*    if sy-tabix = 0.
*      message e000 with 'Check account checkbox'.
*      stop.
*    else.
*      perform get_mit_account.
*      select * from ska1  where saknr in p_hkont.
*        read table it_t030 with key konts = ska1-saknr.
*        if sy-subrc = 0.
*          r_hkont-low = ska1-saknr.  append r_hkont.
*        endif.
*      endselect.
*    endif.
*  endif.
*endform.                    " ranges_account

*&---------------------------------------------------------------------*
*&      Form  SELECTION_ON_RADIOBUTTON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selection_on_radiobutton.

ENDFORM.                    " SELECTION_ON_RADIOBUTTON
*&---------------------------------------------------------------------*
*&      Form  GET_BSAS_DETAIL_ABNORMAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*form get_bsas_detail_abnormal using z_awkey gt_data-awtyp.
*  refresh it_bsas.
*
*  select * into table it_bsas
*       from bsas
*        where augdt eq gt_bsis-augdt
*          and augbl eq gt_bsis-augbl
*          and belnr ne gt_bsis-augbl.
*
*  loop at it_bsas.
*    perform get_bseg_detail using it_bsas
*                                        z_awkey gt_data-awtyp 'A'.
*  endloop.
*
*endform.                    " GET_BSAS_DETAIL_ABNORMAL
*&---------------------------------------------------------------------*
*&      Form  get_other_account
*&---------------------------------------------------------------------*
FORM get_other_account.
  DATA  :ok_flag.
  DATA : it_clea LIKE bseg OCCURS 0 WITH HEADER LINE,
         c_ponum(15).
  CLEAR ok_flag.

*When clearing date of All of clearing document is during posting date
* period
  c_ponum(10) = gt_data-ebeln.
  c_ponum+10(5) = gt_data-ebelp.


  LOOP AT gt_bsis WHERE zuonr EQ c_ponum.
    IF gt_bsis-augdt BETWEEN s_budat-low AND s_budat-high.
      ok_flag = 'X'.
    ELSE.
      CLEAR ok_flag.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF ok_flag EQ 'X'.
    SELECT * INTO TABLE it_clea FROM bseg
     WHERE bukrs EQ p_bukrs
       AND belnr EQ gt_bsis-augbl
       AND gjahr EQ gt_bsis-augdt(4)..

*   read table r_hkont index 1.
    DELETE it_clea WHERE hkont = p_hkont. "eq r_hkont-low.

    LOOP AT it_clea.
      READ TABLE it_t030 WITH KEY konts = it_clea-hkont.
      IF sy-subrc <> 0.
        gt_data-w_dmbtr = gt_data-w_dmbtr + ( it_clea-dmbtr * -1 ).
      ELSE.
        IF gt_data-gr_amt = 0.
          gt_data-o_dmbtr = gt_data-gr_amt * -1.
        ELSEIF gt_data-gr_amt = 0.
          gt_data-o_dmbtr = gt_data-gr_amt * -1.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ELSE.
    EXIT.
  ENDIF.

ENDFORM.                    " get_other_account
*&---------------------------------------------------------------------*
*&      Form  mit_account
*&---------------------------------------------------------------------*
FORM get_mit_account.

  REFRESH it_t030.

  SELECT konts INTO TABLE it_t030
    FROM t030
      WHERE ktopl EQ 'HNA1'
      AND ktosl IN ('WRX', 'FR1', 'FR2', 'FR3','ZR3', 'ZR4').

ENDFORM.                    " mit_account
*&---------------------------------------------------------------------*
*&      Form  search_help_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM search_help_layout.
  DATA : BEGIN OF value_variant OCCURS 0,
          variant LIKE ltdx-variant,
        END OF value_variant.
*
  DATA: scr_fields LIKE dynpread OCCURS 0 WITH HEADER LINE.
  DATA : selectfield   LIKE  help_info-fieldname,
         it_fields     LIKE  help_value OCCURS 0 WITH HEADER LINE,
         it_fields1    LIKE  help_value OCCURS 0 WITH HEADER LINE,
         select_value  LIKE  help_info-fldvalue,
         ld_tabix      LIKE  sy-tabix.

  CLEAR: selectfield, it_fields, select_value, ld_tabix.
  REFRESH: it_fields.
*
  CLEAR: scr_fields.
  REFRESH: scr_fields.
  DATA  : BEGIN OF f4hlp OCCURS 0.
          INCLUDE STRUCTURE dynpread.
  DATA  : END   OF f4hlp.
  REFRESH : f4hlp.  CLEAR: f4hlp.

  f4hlp-fieldname = 'P_LAYOUT'.
  f4hlp-fieldinp  = 'X'.
  APPEND f4hlp.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = 'ZRFIG05_MIT'
      dynumb     = '1000'
    TABLES
      dynpfields = f4hlp
    EXCEPTIONS
      OTHERS     = 01.

  READ TABLE f4hlp INDEX 1.
  MOVE f4hlp-fieldvalue TO p_layout.

* Help List
  SELECT variant
       INTO CORRESPONDING FIELDS OF TABLE value_variant
        FROM ltdx
          WHERE relid EQ 'LT'
            AND report EQ sy-cprog.

  IF sy-dbcnt EQ 0.
    MESSAGE s001 WITH '???? ????.'.
    EXIT.
  ENDIF.
* Hit List
  it_fields-tabname = 'LTDX'.
  it_fields-fieldname = 'VARIANT'.
  it_fields-selectflag = 'X'.
  APPEND it_fields.


  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
    EXPORTING
      selectfield                  = selectfield
    IMPORTING
      ind                          = ld_tabix
      select_value                 = select_value
    TABLES
      fields                       = it_fields
      full_table                   = value_variant
    EXCEPTIONS
      full_table_empty             = 1
      no_tablestructure_given      = 2
      no_tablefields_in_dictionary = 3
      more_then_one_selectfield    = 4
      no_selectfield               = 5
      OTHERS                       = 6.

  CHECK NOT ld_tabix IS INITIAL.
  READ TABLE value_variant INDEX ld_tabix.
  value_variant-variant = select_value.
*
  scr_fields-fieldname  = 'P_LAYOUT'.
  scr_fields-fieldvalue = value_variant-variant.
  APPEND scr_fields.
  MOVE value_variant-variant TO p_layout.

  CLEAR :  value_variant[],it_fields[].
ENDFORM.                    " search_help_layout
*&---------------------------------------------------------------------*
*&      Form  put_po_detail_info
*&---------------------------------------------------------------------*
FORM put_po_detail_info.
  DATA: l_menge LIKE ekbe-menge.

  PERFORM get_po_qty.

  CHECK p_det = 'X'.

  PERFORM get_open_po_qty.

  PERFORM get_po_dates.

* Last Delivery Date
  SELECT MAX( eindt ) INTO gt_data-eindt
          FROM eket
          WHERE ebeln EQ gt_data-ebeln
            AND ebelp EQ gt_data-ebelp.
*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_eket.
  ENDIF.
*- U1 End
* Inboun delivery qty
  IF p_open = 'X'.
    SELECT SUM( lfimg ) INTO gt_data-inbqt
        FROM lips AS a INNER JOIN likp AS b
          ON a~vbeln = b~vbeln
        WHERE a~vgbel = gt_data-ebeln
          AND a~vgpos = gt_data-ebelp
          AND b~lfdat <= p_augdt
          AND b~lfdat IN s_budat.
*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_lips USING 'X'.
    ENDIF.
*- U1 End
  ELSE.
    SELECT SUM( lfimg ) INTO gt_data-inbqt
        FROM lips AS a INNER JOIN likp AS b
          ON a~vbeln = b~vbeln
        WHERE a~vgbel = gt_data-ebeln
          AND a~vgpos = gt_data-ebelp
          AND b~lfdat IN s_budat.
*- U1 Start
    IF p_arch EQ 'X'.
      PERFORM archive_read_lips USING ''.
    ENDIF.
*- U1 End
  ENDIF.

ENDFORM.                    " put_po_detail_info
*&---------------------------------------------------------------------*
*&      Form  get_po_material
*&---------------------------------------------------------------------*
*FORM get_po_material.
*
*  DATA: l_ebeln LIKE ekpo-ebeln,
*        l_ebelp LIKE ekpo-ebelp.
*
**search by vendor
*  IF s_lifnr-low <> space AND s_matnr-low <> space.
*    REFRESH r_zuonr.
*    r_zuonr-option = 'EQ'.
*    r_zuonr-sign   = 'I'.
*
** Begin of changes - UD1K920829
*
**    SELECT * INTO CORRESPONDING FIELDS OF gt_pohd_item
***      a~ebeln b~ebelp into (l_ebeln, l_ebelp)
**      FROM ekko AS a INNER JOIN ekpo AS b
**        ON a~ebeln = b~ebeln
**      WHERE a~lifnr = s_lifnr-low
**        AND b~matnr IN s_matnr
**        AND b~mtart IN s_mtart
**        AND b~wepos = 'X'  "GR
**        AND b~weunb = ' '  "GR, non-valuated
**        AND b~knttp = ' '. "Account assignment category
**      APPEND gt_pohd_item.
**      CONCATENATE gt_pohd_item-ebeln i_po_item-ebelp INTO r_zuonr-low.
**      APPEND r_zuonr.
**    ENDSELECT.
*
*    SELECT * INTO CORRESPONDING FIELDS OF table gt_pohd_item
*      FROM ekko AS a INNER JOIN ekpo AS b
*        ON a~ebeln = b~ebeln
*      WHERE a~lifnr = s_lifnr-low
*        AND b~matnr IN s_matnr
*        AND b~mtart IN s_mtart
*        AND b~wepos = 'X'  "GR
*        AND b~weunb = ' '  "GR, non-valuated
*        AND b~knttp = ' '. "Account assignment category
*
*    loop at gt_pohd_item.
*      CONCATENATE gt_pohd_item-ebeln i_po_item-ebelp INTO r_zuonr-low.
*      APPEND r_zuonr.
*    ENDloop.
*
** End  of changes - UD1K920829
*
** too many...
*    DESCRIBE TABLE r_zuonr LINES sy-index.
*    IF sy-index > 2000.
*      REFRESH r_zuonr.
*    ENDIF.
*  ENDIF.
*ENDFORM.                    " get_po_data
*&---------------------------------------------------------------------*
*&      Form  get_bkpf
*&---------------------------------------------------------------------*
FORM get_bkpf.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE i_bkpf
      FROM bkpf
      FOR ALL ENTRIES IN gt_bsis
       WHERE bukrs EQ p_bukrs
         AND gjahr EQ gt_bsis-gjahr
         AND belnr EQ gt_bsis-belnr.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_bkpf.
  ENDIF.
*- U1 End

ENDFORM.                    " get_bkpf
*&---------------------------------------------------------------------*
*&      Form  get_po_detail
*&---------------------------------------------------------------------*
FORM get_po_detail.

  READ TABLE gt_pohd_item WITH KEY ebeln = gt_data-ebeln
                                   ebelp = gt_data-ebelp BINARY SEARCH.
  IF sy-subrc <> 0.
    READ TABLE gt_pohd_item WITH KEY ebeln = gt_data-ebeln.
  ENDIF.
  IF sy-subrc = 0.
    gt_data-bedat = gt_pohd_item-bedat.
    gt_data-bsart = gt_pohd_item-bsart.
    gt_data-lifnr = gt_pohd_item-lifnr.

    gt_data-matnr = gt_pohd_item-matnr.
    gt_data-xersy = gt_pohd_item-xersy.
    gt_data-stawn = gt_pohd_item-stawn.

    IF ekko-bstyp <> 'L'.
      CLEAR: gt_data-poqty.
    ELSE.
      gt_data-poqty = gt_pohd_item-menge.
    ENDIF.
  ENDIF.


ENDFORM.                    " get_po_detail
*&---------------------------------------------------------------------*
*&      Form  get_po_item_list
*&---------------------------------------------------------------------*
FORM get_po_item_list.
*if assignment is blank..check bseg.
  DATA: l_idx TYPE i.
  LOOP AT gt_data.
    l_idx = sy-tabix.


    gt_pohd-ebeln  = gt_data-ebeln.
    APPEND gt_pohd.

    gt_poitm-ebeln = gt_data-ebeln.
    gt_poitm-ebelp = gt_data-ebelp.
    APPEND gt_poitm.

    gt_glpoitm-hkont = gt_data-hkont.
    gt_glpoitm-zuonr = gt_data-zuonr.
    gt_glpoitm-ebeln = gt_data-ebeln.
    gt_glpoitm-ebelp = gt_data-ebelp.
    gt_glpoitm-dmbtr = gt_data-b_dmbtr.
    COLLECT gt_glpoitm.
  ENDLOOP.

  SORT gt_pohd  BY ebeln.
  DELETE ADJACENT DUPLICATES FROM gt_pohd.
  SORT gt_poitm BY ebeln ebelp.
  DELETE ADJACENT DUPLICATES FROM gt_poitm.



ENDFORM.                    " get_po_item_list
*&---------------------------------------------------------------------*
*&      Form  get_all_po
*&---------------------------------------------------------------------*
FORM get_all_po.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_pohd_item
    FROM ekko AS a
      INNER JOIN ekpo AS b
         ON a~ebeln = b~ebeln
      INNER JOIN marc AS c
         ON b~werks = c~werks
        AND b~matnr = c~matnr
    FOR ALL ENTRIES IN gt_poitm
    WHERE a~ebeln = gt_poitm-ebeln
      AND b~ebelp = gt_poitm-ebelp.
*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_ekko.
  ENDIF.
*- U1 End
*      and a~lifnr in s_lifnr
*      and b~matnr in s_matnr
*      and b~ebeln in s_ebeln
*      and b~ebelp in s_ebelp
*      and b~mtart in s_mtart
*      and b~wepos = 'X'  "GR
*      and b~weunb = ' '  "GR, non-valuated
*      and b~knttp = ' '. "Account assignment category

*Issusing date get from import data ( t-code : zim23)
  SELECT a~zfblno
         b~ebeln
         b~ebelp
         a~zfrebeln
         a~zfbldt
         a~zfetd
         a~zfeta
    INTO TABLE gt_bol
    FROM ztbl AS a INNER JOIN ztblit AS b
      ON a~zfblno = b~zfblno
    FOR ALL ENTRIES IN gt_poitm
    WHERE b~ebeln = gt_poitm-ebeln
      AND b~ebelp = gt_poitm-ebelp
      AND a~zfbldt <= p_augdt.

*1 PO = N B/L
  SORT gt_bol BY ebeln ebelp ASCENDING
                 zfbldt      DESCENDING.

  SORT  gt_pohd_item BY ebeln ebelp.

ENDFORM.                    " get_all_po

*&---------------------------------------------------------------------*
*&      Form  process_filter
*&---------------------------------------------------------------------*
FORM process_filter.
  DATA: l_idx LIKE sy-tabix,
        f_idx LIKE sy-tabix,
        flag TYPE c.

  SORT gt_bsis BY zuonr.
  EXPORT gt_bsis TO MEMORY ID 'man'.
  IMPORT gt_bsis FROM MEMORY ID 'man'.

  LOOP AT gt_bsis WHERE ebeln <> space.
    AT NEW zuonr.
      f_idx = sy-tabix.
    ENDAT.
    l_idx = sy-tabix.
    IF   flag = ''.
      READ TABLE gt_pohd_item WITH KEY ebeln = gt_bsis-ebeln.
      IF sy-subrc <> 0.
        flag = 'X'.
      ENDIF.
    ENDIF.
*    IF sy-subrc <> 0.
*      DELETE gt_bsis INDEX l_idx.
*    ENDIF.
    AT END OF zuonr.
      IF flag = 'X'.
        DELETE gt_bsis  FROM f_idx TO  l_idx.
        CLEAR flag.
      ENDIF.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " process_filter
*&---------------------------------------------------------------------*
*&      Form  put_other_info
*&---------------------------------------------------------------------*
FORM put_other_info.
  DATA: l_idx TYPE i.

  SORT gt_bsis BY zuonr.

  LOOP AT gt_data.
    l_idx = sy-tabix.

* get PO detail
    PERFORM get_po_detail.

* get import data
    READ TABLE gt_bol WITH KEY ebeln = gt_data-ebeln
                               ebelp = gt_data-ebelp.
    IF sy-subrc <> 0.
      READ TABLE gt_bol WITH KEY ebeln = gt_data-ebeln.
    ENDIF.
    IF sy-subrc = 0.
      gt_data-zfblno = gt_bol-zfblno.
      gt_data-zfbldt = gt_bol-zfbldt.
    ENDIF.

*other account, write off
*FIXME....LATER
*     perform get_other_account .


*PO history
    PERFORM put_po_detail_info.

*Status
    IF gt_data-b_dmbtr  = 0.
      gt_data-icon =  c_icon_equal.
    ELSE.
      gt_data-icon =  c_icon_err.
    ENDIF.

**Actual Amount by IG.MOON 6/5/2007
*    perform get_actual_amount using gt_data-ebeln
*                                    gt_data-ebelp
*                                    gt_data-poqty
*                           changing gt_data-act_amt.
*
    MODIFY gt_data INDEX l_idx.
  ENDLOOP.

* Begin of HIS20094
  SORT gt_housebl BY hkont zuonr.

  LOOP AT gt_data.
    LOOP AT gt_housebl WHERE hkont = gt_data-hkont
                         AND zuonr = gt_data-zuonr.

      gt_data-xblnr  = gt_housebl-xblnr.
      gt_data-gr_qty = gt_housebl-menge.
      gt_data-gr_amt = gt_housebl-dmbtr.

      APPEND gt_data TO gt_data_t.

      CLEAR: gt_data-poqty,
             gt_data-inbqt,
             gt_data-obmng,

             gt_data-gr_qty,
             gt_data-a_menge,
             gt_data-m_menge,
             gt_data-o_menge,
             gt_data-b_menge,

             gt_data-gr_amt,
             gt_data-a_dmbtr,
             gt_data-m_dmbtr,
             gt_data-o_dmbtr,
             gt_data-w_dmbtr,
             gt_data-b_dmbtr,
             gt_data-act_amt.

    ENDLOOP.

    IF sy-subrc NE 0.
      APPEND gt_data TO gt_data_t.
    ENDIF.

  ENDLOOP.

  gt_data[] = gt_data_t[].
  FREE gt_data_t.

* End of HIS20094

ENDFORM.                    " put_other_info
*&---------------------------------------------------------------------*
*&      Form  process_open_only
*&---------------------------------------------------------------------*
FORM process_open_only.
  DATA: l_idx  TYPE i.
  DATA: l_idx2 TYPE i.

  IF p_open = 'X'.
    SORT gt_glpoitm BY hkont zuonr.  "by dmbtr.
    SORT gt_data    BY hkont zuonr.

    LOOP AT gt_data.
      l_idx = sy-tabix.

*      delete gt_bsis where hkont = gt_glpoitm-hkont
*                       and zuonr = gt_glpoitm-zuonr.
      READ TABLE gt_glpoitm WITH KEY hkont = gt_data-hkont
                                     zuonr = gt_data-zuonr
                                 BINARY SEARCH.
      IF gt_glpoitm-dmbtr = 0.
        DELETE gt_glpoitm INDEX sy-tabix.
        gt_data-chk = 'D'.
        MODIFY gt_data INDEX l_idx TRANSPORTING chk.
      ENDIF.

    ENDLOOP.

    SORT gt_data BY chk.
    DELETE gt_data WHERE chk = 'D'.


    SORT gt_glpoitm BY ebeln ebelp.
    LOOP AT gt_poitm.
      l_idx = sy-tabix.
      READ TABLE gt_glpoitm WITH KEY ebeln = gt_poitm-ebeln
                                     ebelp = gt_poitm-ebelp
                                 BINARY SEARCH.
      IF sy-subrc <> 0.
        DELETE gt_poitm INDEX l_idx.
      ENDIF.
    ENDLOOP.
    LOOP AT gt_pohd.
      l_idx = sy-tabix.
      READ TABLE gt_glpoitm WITH KEY ebeln = gt_pohd-ebeln
                                 BINARY SEARCH.
      IF sy-subrc <> 0.
        DELETE gt_pohd INDEX l_idx.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " process_open_only
*&---------------------------------------------------------------------*
*&      Form  get_open_po_qty
*&---------------------------------------------------------------------*
FORM get_open_po_qty.
  DATA: l_menge LIKE ekbe-menge.

  gt_data-obmng = gt_data-poqty.

*1  Goods receipt
*2  Invoice receipt
*3  Subsequent debit
  DATA : BEGIN OF lt_ekbe OCCURS 0,
         shkzg   LIKE   ekbe-shkzg,
         menge   LIKE   ekbe-menge,
        END OF lt_ekbe.

  REFRESH lt_ekbe.
  SELECT  shkzg SUM( menge ) INTO TABLE lt_ekbe
      FROM    ekbe
          WHERE ebeln = gt_data-ebeln
            AND ebelp = gt_data-ebelp
            AND zekkn = space
            AND vgabe = '1'
            AND bewtp = 'E'
            AND budat <= p_augdt
      GROUP BY shkzg.
  LOOP AT lt_ekbe.
    IF lt_ekbe-shkzg = 'H'. "(-)
      gt_data-obmng = gt_data-obmng + lt_ekbe-menge.
    ELSE.
      gt_data-obmng = gt_data-obmng - lt_ekbe-menge.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_open_po_qty
*&---------------------------------------------------------------------*
*&      Form  get_po_qty
*&---------------------------------------------------------------------*
FORM get_po_qty.

*CHECK : "Delivery completed" indicator  "open qty
  READ TABLE gt_pohd_item WITH KEY ebeln = gt_data-ebeln
                                   ebelp = gt_data-ebelp
                               BINARY SEARCH.

*In case  schedule line category is 'L'
*PO qty = Total schedule line qty
  IF gt_pohd_item-bstyp = 'L'.
    IF p_open = 'X'.
      SELECT SUM( menge ) INTO gt_data-poqty
         FROM eket
         WHERE ebeln = gt_data-ebeln
           AND ebelp = gt_data-ebelp
           AND eindt <= p_augdt
           AND eindt IN s_budat.
*- U1 Start
      IF p_arch EQ 'X'.
        PERFORM archive_read_eket_2 USING 'X'.
      ENDIF.
*- U1 End
    ELSE.
      SELECT SUM( menge ) INTO gt_data-poqty
         FROM eket
         WHERE ebeln = gt_data-ebeln
           AND ebelp = gt_data-ebelp
           AND eindt IN s_budat.
*- U1 Start
      IF p_arch EQ 'X'.
        PERFORM archive_read_eket_2 USING ''.
      ENDIF.
*- U1 End
    ENDIF.
  ELSE.
    gt_data-poqty = gt_pohd_item-menge.
  ENDIF.

ENDFORM.                    " get_po_qty
*&---------------------------------------------------------------------*
*&      Form  get_po_dates
*&---------------------------------------------------------------------*
FORM get_po_dates.
  DATA: l_date1 LIKE sy-datum.
  DATA: l_date2 LIKE sy-datum.

  CLEAR: l_date1, l_date2.

* First Open Date / Last posting date
  SELECT  MIN( budat ) MAX( budat ) INTO (l_date1, l_date2)
    FROM  ekbe
          WHERE ebeln = gt_data-ebeln
            AND ebelp = gt_data-ebelp
            AND budat <= p_augdt.

  gt_data-op_dat = l_date1.
  gt_data-budat  = l_date2.

*1  Goods receipt
*2  Invoice receipt
*3  Subsequent debit

* Last GR date / Posting Date
  SELECT  MAX( budat ) INTO l_date1
    FROM  ekbe
          WHERE ebeln = gt_data-ebeln
            AND ebelp = gt_data-ebelp
            AND vgabe = '1'
            AND bewtp = 'E'
            AND budat <= p_augdt.

  gt_data-gr_date = l_date1.

ENDFORM.                    " get_po_dates
*&---------------------------------------------------------------------*
*&      Form  get_from_buffer
*&---------------------------------------------------------------------*
FORM get_from_buffer.

  SELECT *   INTO CORRESPONDING FIELDS OF TABLE gt_data
              FROM ztfi_mit WHERE bukrs     = p_bukrs
                              AND open_date = p_augdt
                              AND hkont     = p_hkont
                              AND lifnr     IN s_lifnr
                              AND matnr     IN s_matnr2
                              AND matnr     IN s_matnr
                              AND ebeln     IN s_ebeln
                              AND ebelp     IN s_ebelp.
*                             and budat     in s_budat.    "HIS20094

*Status
  DATA: l_idx LIKE sy-tabix.
  LOOP AT gt_data.
    l_idx = sy-tabix.
    IF gt_data-b_dmbtr  = 0.
      gt_data-icon =  c_icon_equal.
    ELSE.
      gt_data-icon =  c_icon_err.
    ENDIF.

    MODIFY gt_data INDEX l_idx TRANSPORTING icon.
  ENDLOOP.


ENDFORM.                    " get_from_buffer
*&---------------------------------------------------------------------*
*&      Form  save_to_buffer
*&---------------------------------------------------------------------*
FORM save_to_buffer.
  LOOP AT gt_data.
    MOVE-CORRESPONDING gt_data TO gt_data_d.
    gt_data_d-bukrs     = p_bukrs.
    gt_data_d-open_date = p_augdt.
    gt_data_d-erdat =  sy-datum.
    gt_data_d-erzet =  sy-uzeit.
    gt_data_d-ernam =  sy-uname.
    APPEND gt_data_d.
  ENDLOOP.

  DELETE FROM ztfi_mit WHERE bukrs = p_bukrs
                         AND open_date EQ p_augdt
                         AND hkont = p_hkont.
  IF sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
    WRITE :/ 'No of Records Deleted', sy-dbcnt.
  ENDIF.

  INSERT  ztfi_mit FROM TABLE gt_data_d.

  IF sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
    WRITE :/ 'No of Records inserted ', sy-dbcnt.
  ELSE.
    ROLLBACK WORK.
    WRITE :/ 'Error in Inserting  Records '.
  ENDIF.

ENDFORM.                    " save_to_buffer
*&---------------------------------------------------------------------*
*&      Form  delete_buffer
*&---------------------------------------------------------------------*
FORM delete_buffer.

  DELETE FROM ztfi_mit WHERE bukrs = p_bukrs
                         AND open_date EQ p_augdt
                         AND hkont = p_hkont.

  IF sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
    WRITE :/ 'No of Records Deleted', sy-dbcnt.
  ELSE.
    ROLLBACK WORK .
    WRITE :/ 'Error in deleting  Records'.
  ENDIF.

ENDFORM.                    " delete_buffer
*&---------------------------------------------------------------------*
*&      Form  show_records
*&---------------------------------------------------------------------*
FORM show_records.

* ==> 5. build field category
  PERFORM initial_build_field_category.
* ==> 6. build sorts info
*  REFRESH gt_sorts.
  PERFORM build_sort_table
    USING :
       '1'    'LIFNR'   'X'   'X'   '*',
       '2'    'BSART'   'X'   'X'   '*',
       '3'    'EBELN'   'X'   ' '   '*',
       '4'    'EBELP'   'X'   ' '   '*'.
* ==> 2. set variant default
  PERFORM set_variant CHANGING wa_var.
* ==> 3. set layout for alv style
  PERFORM set_layout CHANGING gs_layout.
* ==> 4. set events for alv
  PERFORM set_events CHANGING gt_events.
*===> 5. set event for top-of-page grid.
  PERFORM set_build_event.
*===>
  PERFORM comment_build USING  w_top_of_page[].

* ==> 7. call function display alv.
  CALL FUNCTION wa_alv_function_name
    EXPORTING
      i_callback_program       = wa_repid
      i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
      i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat[]
      it_special_groups        = gt_sp_group[]
      it_sort                  = gt_sorts[]
*     IT_FILTER                =
      i_default                = wa_default
      i_save                   = wa_var_save
      is_variant               = wa_var
*     it_events                = gt_events[]
      it_events                = w_eventcat[]
      is_print                 = gs_prnt
*     IT_EVENT_EXIT            =
*     I_SCREEN_START_COLUMN    = 10
*     I_SCREEN_START_LINE      = 2
*     I_SCREEN_END_COLUMN      = 80
*     I_SCREEN_END_LINE        = 23
    TABLES
      t_outtab                 = gt_data.

ENDFORM.                    " show_records
*&---------------------------------------------------------------------*
*&      Form  get_help_p_hkont
*&---------------------------------------------------------------------*
FORM get_help_p_hkont.
  DATA: BEGIN OF value_tab OCCURS 0,
          txt50 LIKE skat-txt50,
          ktopl LIKE t030-ktopl,
          konts LIKE t030-konts,
        END OF value_tab.

  DATA: l_bukrs LIKE t001-bukrs.
  GET PARAMETER ID 'BUK' FIELD l_bukrs.
  SELECT SINGLE * FROM t001 WHERE bukrs = l_bukrs.
  IF sy-subrc = 0.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE value_tab
      FROM t030
      WHERE ktopl = t001-ktopl
        AND ( ktosl LIKE 'FR%' OR
              ktosl = 'WRX' ).
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE value_tab
      FROM t030
      WHERE ( ktosl LIKE 'FR%' OR
              ktosl = 'WRX' ).
  ENDIF.

  SORT value_tab BY ktopl konts.
  DELETE ADJACENT DUPLICATES FROM value_tab COMPARING ktopl konts.

  LOOP AT value_tab.
    SELECT SINGLE txt50 INTO value_tab-txt50
     FROM skat
    WHERE spras = sy-langu
      AND ktopl = value_tab-ktopl
      AND saknr = value_tab-konts.
    MODIFY value_tab.
  ENDLOOP.


* Set F4 values for pdate

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'S_MCODE'
      dynpprog        = 'DYNPPROG'  "sy-repid "l_dyname
      dynpnr          = '1000'
      dynprofield     = 'KONTS'
      window_title    = 'MIT Account'
      value_org       = 'S'
    TABLES
      value_tab       = value_tab
    EXCEPTIONS
      parameter_error = 1.

ENDFORM.                    " get_help_p_hkont
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_EKKO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_ekko .

  TYPES: BEGIN OF ty_ekko,
         ebeln TYPE ebeln,
         ebelp TYPE ebelp,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_ekko.

  DATA: l_handle    TYPE sytabix,
        lt_ekko     TYPE TABLE OF ekko WITH HEADER LINE,
        lt_ekpo     TYPE TABLE OF ekpo WITH HEADER LINE,
        lt_marc     TYPE TABLE OF marc WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_ekko TYPE TABLE OF ty_ekko,
        ls_inx_ekko TYPE ty_ekko.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZEKPO_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

  CHECK NOT gt_poitm[] IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_ekko[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_ekko
    FROM (l_gentab)
    FOR ALL ENTRIES IN gt_poitm
   WHERE ebeln = gt_poitm-ebeln
     AND ebelp = gt_poitm-ebelp.

  CHECK NOT lt_inx_ekko[] IS INITIAL.

*  SORT lt_inx_ekko BY ebeln.
*  DELETE ADJACENT DUPLICATES FROM lt_inx_ekko COMPARING ebeln.

* 4. Get more archived data looping structure table
  CLEAR: gt_ekko_a, gt_ekko_a[], gt_ekpo_a, gt_ekpo_a[].
  LOOP AT lt_inx_ekko INTO ls_inx_ekko.
*  4.1 Read information from archivekey & offset
    CLEAR: lt_ekko, lt_ekko[], lt_ekpo, lt_ekpo[].
    CALL FUNCTION 'ASH_MM_EKKO_READ'
      EXPORTING
        i_archivekey           = ls_inx_ekko-archivekey
        i_offset               = ls_inx_ekko-archiveofs
      TABLES
        et_ekko                = lt_ekko
        et_ekpo                = lt_ekpo
      EXCEPTIONS
        not_in_infostructure   = 1
        not_in_archive         = 2
        no_instructure_defined = 3
        OTHERS                 = 4.

    CHECK sy-subrc = 0.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_ekko INTO TABLE gt_ekko_a.
    INSERT LINES OF lt_ekpo INTO TABLE gt_ekpo_a.
  ENDLOOP.

  SORT: gt_ekko_a, gt_ekpo_a.
  DELETE ADJACENT DUPLICATES FROM: gt_ekko_a COMPARING ALL FIELDS,
                                   gt_ekpo_a COMPARING ALL FIELDS.

  CLEAR: lt_marc, lt_marc[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_marc
    FROM marc
    FOR ALL ENTRIES IN gt_ekpo_a
   WHERE matnr = gt_ekpo_a-matnr
     AND werks = gt_ekpo_a-werks.

  LOOP AT gt_ekpo_a.
    CLEAR gt_pohd_item.
    MOVE-CORRESPONDING gt_ekpo_a TO gt_poitm.

    CLEAR lt_marc.
    READ TABLE lt_marc WITH KEY matnr = gt_ekpo_a-matnr
                                werks = gt_ekpo_a-werks.
    CHECK sy-subrc = 0.
    MOVE-CORRESPONDING lt_marc   TO gt_poitm.

    CLEAR gt_ekko_a.
    READ TABLE gt_ekko_a WITH KEY ebeln = gt_ekpo_a-ebeln.
    CHECK sy-subrc = 0.
    MOVE-CORRESPONDING gt_ekko_a TO gt_poitm.

    APPEND gt_poitm.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_EKKO
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_LIPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3549   text
*----------------------------------------------------------------------*
FORM archive_read_lips  USING  p_open.

  TYPES: BEGIN OF ty_lips,
         vbeln TYPE vbeln_vl,
         posnr TYPE posnr_vl,
         lfimg TYPE lfimg,
         lfdat TYPE lfdat_v,
         vgbel TYPE vgbel,
         vgpos TYPE vgpos,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_lips.

  DATA: l_handle    TYPE sytabix,
        lt_lips     TYPE TABLE OF lips WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_lips TYPE TABLE OF ty_lips,
        ls_inx_lips TYPE ty_lips.

  DATA: l_lfimg LIKE lips-lfimg.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZLIPS_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  IF p_open = 'X'.
    CLEAR l_lfimg.
    SELECT SUM( lfimg ) INTO l_lfimg
      FROM (l_gentab)
     WHERE vgbel = gt_data-ebeln
       AND vgpos = gt_data-ebelp
       AND lfdat <= p_augdt
       AND lfdat IN s_budat.
  ELSE.
    CLEAR l_lfimg.
    SELECT SUM( lfimg ) INTO l_lfimg
      FROM (l_gentab)
     WHERE vgbel = gt_data-ebeln
       AND vgpos = gt_data-ebelp
       AND lfdat IN s_budat.
  ENDIF.

  gt_data-inbqt = gt_data-inbqt + l_lfimg.

ENDFORM.                    " ARCHIVE_READ_LIPS
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_EKET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_eket .

  TYPES: BEGIN OF ty_eket,
         ebeln TYPE ebeln,
         ebelp TYPE ebelp,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_eket.

  DATA: l_handle    TYPE sytabix,
        lt_eket     TYPE TABLE OF eket WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_eket TYPE TABLE OF ty_eket,
        ls_inx_eket TYPE ty_eket.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZEKPO_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_eket[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_eket
    FROM (l_gentab)
   WHERE ebeln EQ gt_data-ebeln
     AND ebelp EQ gt_data-ebelp.

  CHECK NOT lt_inx_eket[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_eket_a, gt_eket_a[].
  LOOP AT lt_inx_eket INTO ls_inx_eket.
*  4.1 Read information from archivekey & offset
    CLEAR: lt_eket, lt_eket[].
    CALL FUNCTION 'ASH_MM_EKKO_READ'
      EXPORTING
        i_archivekey           = ls_inx_eket-archivekey
        i_offset               = ls_inx_eket-archiveofs
      TABLES
        et_eket                = lt_eket
      EXCEPTIONS
        not_in_infostructure   = 1
        not_in_archive         = 2
        no_instructure_defined = 3
        OTHERS                 = 4.

    CHECK sy-subrc = 0.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_eket INTO TABLE gt_eket_a.
  ENDLOOP.

  CHECK NOT gt_eket_a[] IS INITIAL.

  SORT gt_eket_a BY ebeln ebelp etenr.
  DELETE ADJACENT DUPLICATES FROM gt_eket_a COMPARING ebeln ebelp etenr.

  SORT gt_eket_a BY eindt DESCENDING.

  READ TABLE gt_eket_a INDEX 1.
  IF gt_data-eindt IS INITIAL.
    gt_data-eindt = gt_eket_a-eindt.
  ELSEIF gt_eket_a-eindt > gt_data-eindt.
    gt_data-eindt = gt_eket_a-eindt.
  ENDIF.

ENDFORM.                    " ARCHIVE_READ_EKET
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_EKET_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4391   text
*----------------------------------------------------------------------*
FORM archive_read_eket_2  USING  p_open.

  TYPES: BEGIN OF ty_eket,
         ebeln TYPE ebeln,
         ebelp TYPE ebelp,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_eket.

  DATA: l_handle    TYPE sytabix,
        lt_eket     TYPE TABLE OF eket WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_eket TYPE TABLE OF ty_eket,
        ls_inx_eket TYPE ty_eket.

  DATA: l_menge LIKE eket-menge.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZEKPO_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_eket[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_eket
    FROM (l_gentab)
   WHERE ebeln EQ gt_data-ebeln
     AND ebelp EQ gt_data-ebelp.

  CHECK NOT lt_inx_eket[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_eket_a, gt_eket_a[].
  LOOP AT lt_inx_eket INTO ls_inx_eket.
*  4.1 Read information from archivekey & offset
    CLEAR: lt_eket, lt_eket[].
    CALL FUNCTION 'ASH_MM_EKKO_READ'
      EXPORTING
        i_archivekey           = ls_inx_eket-archivekey
        i_offset               = ls_inx_eket-archiveofs
      TABLES
        et_eket                = lt_eket
      EXCEPTIONS
        not_in_infostructure   = 1
        not_in_archive         = 2
        no_instructure_defined = 3
        OTHERS                 = 4.

    CHECK sy-subrc = 0.

    IF p_open = 'X'.
      DELETE lt_eket WHERE eindt > p_augdt
                        OR NOT eindt IN s_budat.
    ELSE.
      DELETE lt_eket WHERE NOT eindt IN s_budat.
    ENDIF.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_eket INTO TABLE gt_eket_a.
  ENDLOOP.

  CHECK NOT gt_eket_a[] IS INITIAL.

  SORT gt_eket_a BY ebeln ebelp etenr.
  DELETE ADJACENT DUPLICATES FROM gt_eket_a COMPARING ebeln ebelp etenr.

  CLEAR l_menge.
  LOOP AT gt_eket_a.
    l_menge = l_menge + gt_eket_a-menge.
  ENDLOOP.

  gt_data-poqty = l_menge.

ENDFORM.                    " ARCHIVE_READ_EKET_2
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BKPF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_bkpf .

  TYPES: BEGIN OF ty_bkpf,
         bukrs TYPE bukrs,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
         blart TYPE blart,
         bldat TYPE bldat,
         budat TYPE budat,
         awkey TYPE awkey,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_bkpf.

  DATA: l_handle    TYPE sytabix,
        lt_bkpf     TYPE TABLE OF bkpf WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_bkpf TYPE TABLE OF ty_bkpf,
        ls_inx_bkpf TYPE ty_bkpf.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZBKPF_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

  CHECK NOT gt_bsis[] IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_bkpf[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_bkpf
    FROM (l_gentab)
    FOR ALL ENTRIES IN gt_bsis
   WHERE bukrs EQ p_bukrs
     AND gjahr EQ gt_bsis-gjahr
     AND belnr EQ gt_bsis-belnr.

  CHECK NOT lt_inx_bkpf[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_bkpf_a, gt_bkpf_a[].
  LOOP AT lt_inx_bkpf INTO ls_inx_bkpf.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'FI_DOCUMNT'
        archivkey                 = ls_inx_bkpf-archivekey
        offset                    = ls_inx_bkpf-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_bkpf, lt_bkpf[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'BKPF'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_bkpf
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_bkpf[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_bkpf INTO TABLE gt_bkpf_a.
  ENDLOOP.

  CHECK NOT gt_bkpf_a[] IS INITIAL.

  SORT gt_bkpf_a.
  DELETE ADJACENT DUPLICATES FROM gt_bkpf_a COMPARING ALL FIELDS.

  LOOP AT gt_bkpf_a.
    CLEAR i_bkpf.
    MOVE-CORRESPONDING gt_bkpf_a TO i_bkpf.
    APPEND i_bkpf.  CLEAR i_bkpf.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_BKPF
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BSEG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_bseg .

  TYPES: BEGIN OF ty_bseg,
         bukrs TYPE bukrs,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
         buzei TYPE buzei,
         koart TYPE koart,
         gsber TYPE gsber,
         lifnr TYPE lifnr,
         kunnr TYPE kunnr,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_bseg.

  DATA: l_handle    TYPE sytabix,
        lt_bseg     TYPE TABLE OF bseg WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_bseg TYPE TABLE OF ty_bseg,
        ls_inx_bseg TYPE ty_bseg.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZBSEG_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_bseg[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_bseg
    FROM (l_gentab)
   WHERE bukrs EQ p_bukrs
     AND belnr EQ gt_bsis-belnr
     AND gjahr EQ gt_bsis-gjahr
     AND buzei EQ gt_bsis-buzei.

  CHECK NOT lt_inx_bseg[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_bseg_a, gt_bseg_a[].
  LOOP AT lt_inx_bseg INTO ls_inx_bseg.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'FI_DOCUMNT'
        archivkey                 = ls_inx_bseg-archivekey
        offset                    = ls_inx_bseg-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_bseg, lt_bseg[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'BSEG'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_bseg
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_bseg[] IS INITIAL.

    CLEAR lt_bseg.
    READ TABLE lt_bseg INDEX 1.

    gt_bsis-menge = lt_bseg-menge.

* 5. Append archived data table to finally interal table
*    INSERT LINES OF lt_bkpf INTO TABLE gt_bkpf_a.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_BSEG
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BKPF_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_bkpf_2 .

  TYPES: BEGIN OF ty_bkpf,
         bukrs TYPE bukrs,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
         blart TYPE blart,
         bldat TYPE bldat,
         budat TYPE budat,
         awkey TYPE awkey,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_bkpf.

  DATA: l_handle    TYPE sytabix,
        lt_bkpf     TYPE TABLE OF bkpf WITH HEADER LINE,
        lt_bsis     TYPE TABLE OF bsis WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_bkpf TYPE TABLE OF ty_bkpf,
        ls_inx_bkpf TYPE ty_bkpf.

  CLEAR: lt_bsis, lt_bsis[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_bsis
    FROM bsis
   WHERE bukrs =  p_bukrs
     AND hkont =  p_hkont
     AND budat =< p_augdt
     AND budat IN s_budat
     AND zuonr IN s_zuonr.

  CHECK NOT lt_bsis[] IS INITIAL.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZBKPF_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_bkpf[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_bkpf
    FROM (l_gentab)
    FOR ALL ENTRIES IN lt_bsis
   WHERE bukrs EQ p_bukrs
     AND gjahr EQ lt_bsis-gjahr
     AND belnr EQ lt_bsis-belnr.

  CHECK NOT lt_inx_bkpf[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_bkpf_a, gt_bkpf_a[].
  LOOP AT lt_inx_bkpf INTO ls_inx_bkpf.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'FI_DOCUMNT'
        archivkey                 = ls_inx_bkpf-archivekey
        offset                    = ls_inx_bkpf-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_bkpf, lt_bkpf[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'BKPF'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_bkpf
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_bkpf[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_bkpf INTO TABLE gt_bkpf_a.
  ENDLOOP.

  CHECK NOT gt_bkpf_a[] IS INITIAL.

  SORT gt_bkpf_a.
  DELETE ADJACENT DUPLICATES FROM gt_bkpf_a COMPARING ALL FIELDS.

  LOOP AT lt_bsis.
    CLEAR gt_bkpf_a.
    READ TABLE gt_bkpf_a WITH KEY bukrs = gt_bkpf_a-bukrs
                                  belnr = gt_bkpf_a-belnr
                                  gjahr = gt_bkpf_a-gjahr.
    CHECK sy-subrc = 0.
    gt_bsis-awkey = gt_bkpf_a-awkey.
    gt_bsis-awtyp = gt_bkpf_a-awtyp.
    gt_bsis-tcode = gt_bkpf_a-tcode.
    MOVE-CORRESPONDING lt_bsis TO gt_bsis.
    APPEND gt_bsis. CLEAR gt_bsis.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_BKPF_2
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BKPF_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_bkpf_3 USING p_open.

  TYPES: BEGIN OF ty_bkpf,
         bukrs TYPE bukrs,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
         blart TYPE blart,
         bldat TYPE bldat,
         budat TYPE budat,
         awkey TYPE awkey,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_bkpf.

  DATA: l_handle    TYPE sytabix,
        lt_bkpf     TYPE TABLE OF bkpf WITH HEADER LINE,
        lt_bsas     TYPE TABLE OF bsas WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_bkpf TYPE TABLE OF ty_bkpf,
        ls_inx_bkpf TYPE ty_bkpf.

  IF p_open = 'X'.
    CLEAR: lt_bsas, lt_bsas[].
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_bsas
      FROM bsas
     WHERE bukrs =  p_bukrs
       AND hkont =  p_hkont
       AND augdt >  p_augdt
       AND budat <= p_augdt
       AND budat <= p_augdt
       AND budat IN s_budat
       AND zuonr IN s_zuonr.
  ELSE.
    CLEAR: lt_bsas, lt_bsas[].
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_bsas
      FROM bsas
     WHERE bukrs =  p_bukrs
       AND hkont =  p_hkont
       AND budat <= p_augdt
       AND budat <= p_augdt
       AND budat IN s_budat
       AND zuonr IN s_zuonr.
  ENDIF.

  CHECK NOT lt_bsas[] IS INITIAL.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZBKPF_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_bkpf[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_bkpf
    FROM (l_gentab)
    FOR ALL ENTRIES IN lt_bsas
   WHERE bukrs EQ p_bukrs
     AND gjahr EQ lt_bsas-gjahr
     AND belnr EQ lt_bsas-belnr.

  CHECK NOT lt_inx_bkpf[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_bkpf_a, gt_bkpf_a[].
  LOOP AT lt_inx_bkpf INTO ls_inx_bkpf.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'FI_DOCUMNT'
        archivkey                 = ls_inx_bkpf-archivekey
        offset                    = ls_inx_bkpf-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_bkpf, lt_bkpf[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'BKPF'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_bkpf
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_bkpf[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_bkpf INTO TABLE gt_bkpf_a.
  ENDLOOP.

  CHECK NOT gt_bkpf_a[] IS INITIAL.

  SORT gt_bkpf_a.
  DELETE ADJACENT DUPLICATES FROM gt_bkpf_a COMPARING ALL FIELDS.

  LOOP AT lt_bsas.
    CLEAR gt_bkpf_a.
    READ TABLE gt_bkpf_a WITH KEY bukrs = gt_bkpf_a-bukrs
                                  belnr = gt_bkpf_a-belnr
                                  gjahr = gt_bkpf_a-gjahr.
    CHECK sy-subrc = 0.
    gt_bsis-awkey = gt_bkpf_a-awkey.
    gt_bsis-awtyp = gt_bkpf_a-awtyp.
    gt_bsis-tcode = gt_bkpf_a-tcode.
    MOVE-CORRESPONDING lt_bsas TO gt_bsis.
    APPEND gt_bsis. CLEAR gt_bsis.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_BKPF_3
