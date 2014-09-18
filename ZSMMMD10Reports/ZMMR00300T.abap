*&--------------------------------------------------------------------
*& Program ID     : ZMMR00300T
*& Program Name   : Master Overview for Materials Management
*& Created by     : hmoon
*& Created on     : 02.02.2009
*& Development ID :
*& Reference Pgm. : ZMMR4001
*& Description    :
*&
*& Modification Log
*&====================================================================
*& Date        Developer    Request ID    Description
*& 02.02.2009  hmoon                      First development
*& 07.06.2010  Victor       lgort(ekpo-lgort) added in ZMMS0001
*&--------------------------------------------------------------------
REPORT zmmr00300t  NO STANDARD PAGE HEADING
                   LINE-SIZE 191.

TABLES : ekpo.

INCLUDE <list>.

* ALV
TYPE-POOLS: slis,pdt.

*"Callback
DATA: gt_list_top_of_page TYPE slis_t_listheader,
      g_status            TYPE slis_formname VALUE 'PF_STATUS_SET',
      g_user_command      TYPE slis_formname VALUE 'USER_COMMAND',
      g_top_of_page       TYPE slis_formname VALUE 'TOP_OF_PAGE',
      g_top_of_list       TYPE slis_formname VALUE 'TOP_OF_LIST',
      g_end_of_list       TYPE slis_formname VALUE 'END_OF_LIST'.
DATA: gt_fieldcat         TYPE slis_t_fieldcat_alv,
      gt_popupfc          TYPE slis_t_fieldcat_alv,
      gs_popup_layout     TYPE slis_layout_alv,
      gs_grid_settings    TYPE lvc_s_glay,
      gs_filter           TYPE slis_filter_alv,
      gt_filter           TYPE slis_t_filter_alv,
      gs_layout           TYPE slis_layout_alv,
      gs_light            TYPE lvc_s_layo,
      gs_print            TYPE slis_print_alv,
      gt_sort             TYPE slis_t_sortinfo_alv,
      gt_sp_group         TYPE slis_t_sp_group_alv,
      gt_events           TYPE slis_t_event,
      g_save                                 VALUE 'A',
      gx_variant          LIKE disvariant,
      g_variant           LIKE disvariant.
DATA: g_lights_fieldname  TYPE slis_fieldname VALUE 'LIGHT'.
* Data to be displayed.
DATA: g_repid             LIKE sy-repid,
      g_rc,
      g_matnr             LIKE mara-matnr.

* BDC Detail Message.
DATA:
BEGIN OF gt_bdcmsg OCCURS 0,
  updkz TYPE updkz_d,
  matnr LIKE mara-matnr.
        INCLUDE STRUCTURE bapiret2.
DATA:
END OF gt_bdcmsg.

*DATA:
*  BEGIN OF xpopseq OCCURS 0.
*        INCLUDE STRUCTURE ztbm_hkon.
*DATA:
*    color    TYPE slis_t_specialcol_alv,
*  END OF xpopseq.



*======================================================================*
*    Global Data Definition
*======================================================================*
TABLES:
  t134,                       "Material Types
  t001w,                      "Plant Data.
  mara,                       "General Material Data.
  marc,                       "Plant Data for material.
  mard,                       "Storage Location Data for Material.
  makt,                       "Material Descriptions.
  mbew,                       "Material Valuation.
  t460a,                      "Special procurement key.
  lfa1,                       "Vendor Master
  eord.                       "Source List.

DATA: x001w LIKE TABLE OF t001w WITH HEADER LINE,
      x159l LIKE TABLE OF t159l WITH HEADER LINE,
      x134  LIKE TABLE OF t134  WITH HEADER LINE,
      xlfa1 TYPE TABLE OF lfa1  WITH HEADER LINE,
      xmast TYPE HASHED TABLE OF mast
            WITH UNIQUE KEY matnr werks stlan stlnr stlal
            WITH HEADER LINE,
      xmard TYPE HASHED TABLE OF mard
            WITH UNIQUE KEY matnr werks lgort
            WITH HEADER LINE,
      xeord TYPE HASHED TABLE OF eord
            WITH UNIQUE KEY matnr werks zeord
            WITH HEADER LINE,
      xpkhd TYPE HASHED TABLE OF pkhd
            WITH UNIQUE KEY matnr werks prvbe
            WITH HEADER LINE.
DATA:
  BEGIN OF xinfo OCCURS 0,
    matnr TYPE eina-matnr,
    esokz TYPE eine-esokz,
    infnr TYPE eina-infnr,
    lifnr TYPE eina-lifnr,
  END OF xinfo.

DATA: BEGIN OF xlist OCCURS 0.
       INCLUDE STRUCTURE zmms0001.
DATA:
		kalkz TYPE mbew-kalkz,
		kalkl TYPE mbew-kalkl,
		kalkv TYPE mbew-kalkv,
		mark,
		light,
*		coltab  TYPE lvc_t_scol,
  END OF xlist.

DATA: BEGIN OF bdcdata OCCURS 500.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.
DATA: ctu_params LIKE ctu_params.

DATA:
  it_wultb   TYPE TABLE OF stpov   WITH HEADER LINE.

* Constants.
CONSTANTS:
  co_client_level  TYPE mara-pstat  VALUE 'K',
  co_plant_level   TYPE marc-pstat  VALUE 'ABDEGLPQSV',
  co_status_space  TYPE iconshort   VALUE '@5F\QNot Relevant@',
  co_status_red    TYPE iconshort   VALUE '@5C\QNot yet processed@',
  co_status_yellow TYPE iconshort   VALUE '@5D\QNot yet,no difficult@',
  co_status_green  TYPE iconshort   VALUE '@5B\QCompletely Processed@',
  co_basic         TYPE t133a-auswg VALUE '07',   "K
  co_sales         TYPE t133a-auswg VALUE '09',   "V
  co_purchasing    TYPE t133a-auswg VALUE '14',   "E
  co_export        TYPE t133a-auswg VALUE '12',   "E
  co_import        TYPE t133a-auswg VALUE '15',   "V
  co_mrp1          TYPE t133a-auswg VALUE '26',   "D
  co_mrp2          TYPE t133a-auswg VALUE '27',   "D
  co_mrp3          TYPE t133a-auswg VALUE '28',   "D
  co_mrp4          TYPE t133a-auswg VALUE '29',   "D
  co_quality       TYPE t133a-auswg VALUE '20',   "Q
  co_accounting1   TYPE t133a-auswg VALUE '21',   "B
  co_accounting2   TYPE t133a-auswg VALUE '22',   "B
  co_costing1      TYPE t133a-auswg VALUE '34',   "G
  co_costing2      TYPE t133a-auswg VALUE '35',   "G
  co_storage       TYPE t133a-auswg VALUE '18',   "L
  co_storage2      TYPE t133a-auswg VALUE '19',   "L
  co_class         TYPE t133a-auswg VALUE '23',   "C
  co_forecast      TYPE t133a-auswg VALUE '30',   "P
  co_tools         TYPE t133a-auswg VALUE '17',   "F
  co_work          TYPE t133a-auswg VALUE '31',   "A
  co_backgrnd      TYPE i VALUE 0,
  co_heading       TYPE i VALUE 1,
  co_normal        TYPE i VALUE 2,
  co_total         TYPE i VALUE 3,
  co_key           TYPE i VALUE 4,
  co_positive      TYPE i VALUE 5,
  co_negative      TYPE i VALUE 6,
  co_group         TYPE i VALUE 7.

DATA: gt_color  TYPE lvc_t_scol.
DATA: first_act VALUE 'X',
      act_butt  VALUE 'X'.
*======================================================================*
*    Selection Screen.
*======================================================================*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
SELECT-OPTIONS:
  so_werks FOR  t001w-werks MEMORY ID wrk," DEFAULT 'KVA1',
  so_matnr FOR  mara-matnr,
  so_mtart FOR  mara-mtart  OBLIGATORY DEFAULT 'ROH',
  so_ferth FOR  mara-ferth,
  so_formt FOR  mara-formt,
  so_bklas FOR  mbew-bklas,
  so_labor FOR  mara-labor,
  so_matkl FOR  mara-matkl,
  so_kzkri FOR  marc-kzkri,
  so_beskz FOR  marc-beskz,
  so_sobsl FOR  marc-sobsl,
  so_ersda FOR  mara-ersda,
  so_laeda FOR  mara-laeda,
  so_lvorm FOR  marc-lvorm,
  so_fabkz FOR  marc-fabkz,
  so_tempb FOR  mara-tempb,
  so_dispo FOR  marc-dispo,
  so_ekgrp FOR  marc-ekgrp,
  so_aeszn FOR  mara-aeszn,
  so_mstae FOR  mara-mstae,
  so_lifnr FOR  eord-lifnr MODIF ID noi.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK bom WITH FRAME TITLE text-bom.
PARAMETERS:
  pa_bom   AS CHECKBOX,
  pa_datuv LIKE stko-datuv DEFAULT sy-datum OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bom.

SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE text-lis.
PARAMETERS:
  pa_all   RADIOBUTTON GROUP rend,
  pa_error RADIOBUTTON GROUP rend,
  pa_norm  RADIOBUTTON GROUP rend.
SELECTION-SCREEN END OF BLOCK blk.
*
SELECTION-SCREEN BEGIN OF BLOCK blkcd WITH FRAME TITLE text-cdm.
PARAMETERS:
  pref_3_o RADIOBUTTON GROUP cd23,
  pref_3   RADIOBUTTON GROUP cd23,
  pref_2   RADIOBUTTON GROUP cd23.
SELECTION-SCREEN END OF BLOCK blkcd.
*======================================================================*
*    Initialization.
*======================================================================*
INITIALIZATION.
  PERFORM check_tcode_authority USING sy-tcode.
  g_repid = sy-repid.
  PERFORM variant_init.


  IF sy-tcode EQ 'ZMMR00301T'.
    PERFORM read_vendor_of_userid .
    g_variant-variant = '/VENDOR'.
  ENDIF.


* Get default variant
  gx_variant = g_variant.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
       EXPORTING
            i_save     = g_save
       CHANGING
            cs_variant = gx_variant
       EXCEPTIONS
            not_found  = 2.
  IF sy-subrc = 0.

  ENDIF.
  PERFORM layout_init     USING gs_layout.
  PERFORM set_default_value.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT                                           *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT .
  CASE sy-tcode.
    WHEN 'ZMMR00301T'.
      PERFORM modify_screen.
    WHEN OTHERS.

  ENDCASE.

*======================================================================*
*    Start of Selection.
*======================================================================*
START-OF-SELECTION.
  CASE sy-tcode.
    WHEN 'ZMMR00301T'.
      IF so_lifnr[] IS INITIAL.
        MESSAGE i999(zmmm) WITH 'Please Input the Vendor Code'.
        EXIT.
      ENDIF.
  ENDCASE.

  PERFORM read_main_data.

*======================================================================*
*    End of Selection.
*======================================================================*
END-OF-SELECTION.
  PERFORM list_viewer.

*======================================================================*
*    Subroutines.
*======================================================================*
*&---------------------------------------------------------------------*
*&      Form  read_material_master
*&---------------------------------------------------------------------*
*       Read Material Master.
*----------------------------------------------------------------------*
FORM read_material_master.

  IF so_lifnr[] IS INITIAL.
    SELECT c~matnr c~werks k~maktx m~mtart m~meins
           m~matkl m~bismt m~labor m~mstae m~rmatp
           m~brgew m~ntgew m~gewei m~mbrsh m~zeinr
           m~laeda m~vpsta m~tempb m~raube
           m~ferth m~formt m~normt m~aeszn
           c~kzkri c~ekgrp c~kautb c~kordb c~webaz
           c~fabkz c~disgr c~maabc c~dismm
           c~minbe c~mrppp c~eprio AS inslc
           c~dispo c~disls c~bstrf c~beskz c~sobsl
           c~lgpro c~vspvb c~lgfsb c~plifz c~fhori
           c~shzet c~eisbe c~xmcng c~rdprf c~shflg
           c~mtvfp c~sauft c~sbdkz c~kzbed c~loggr
           c~prctr c~pstat c~stawn w~bklas m~ersda
           c~lvorm c~ncost w~kalkz w~kalkl w~kalkv
      INTO CORRESPONDING FIELDS OF TABLE xlist
      FROM marc AS c LEFT JOIN mbew AS w
                            ON w~matnr = c~matnr AND
                               w~bwkey = c~werks
                    INNER JOIN mara AS m
                            ON c~matnr = m~matnr
                    INNER JOIN makt AS k
                            ON m~matnr = k~matnr AND
                               k~spras = sy-langu
     WHERE c~werks IN so_werks
       AND c~matnr IN so_matnr
       AND m~mtart IN so_mtart
       AND m~ferth IN so_ferth
       AND m~formt IN so_formt
       AND m~labor IN so_labor
       AND m~matkl IN so_matkl
       AND m~ersda IN so_ersda
       AND m~laeda IN so_laeda
       AND c~kzkri IN so_kzkri
       AND c~beskz IN so_beskz
       AND c~sobsl IN so_sobsl
       AND c~lvorm IN so_lvorm
       AND c~fabkz IN so_fabkz
       AND c~dispo IN so_dispo
       AND c~ekgrp IN so_ekgrp
       AND m~tempb IN so_tempb
       AND m~mstae IN so_mstae
       AND m~aeszn IN so_aeszn.


  ELSE.

    SELECT c~matnr c~werks k~maktx m~mtart m~meins
           m~matkl m~bismt m~labor m~mstae m~rmatp
           m~brgew m~ntgew m~gewei m~mbrsh
           m~laeda m~vpsta m~tempb m~raube
           m~ferth m~formt m~normt m~aeszn
           c~kzkri c~ekgrp c~kautb c~kordb c~webaz
           c~fabkz c~disgr c~maabc c~dismm
           c~minbe c~mrppp c~eprio AS inslc
           c~dispo c~disls c~bstrf c~beskz c~sobsl
           c~lgpro c~vspvb c~lgfsb c~plifz c~fhori
           c~shzet c~eisbe c~xmcng c~rdprf
           c~mtvfp c~sauft c~sbdkz c~kzbed c~loggr
           c~prctr c~pstat c~stawn w~bklas m~ersda
           c~lvorm c~ncost w~kalkz w~kalkl w~kalkv
      INTO CORRESPONDING FIELDS OF TABLE xlist
      FROM marc AS c LEFT JOIN mbew AS w
                            ON w~matnr = c~matnr AND
                               w~bwkey = c~werks
                    INNER JOIN mara AS m
                            ON c~matnr = m~matnr
                    INNER JOIN makt AS k
                            ON m~matnr = k~matnr AND
                               k~spras = sy-langu
     WHERE c~werks IN so_werks
       AND c~matnr IN so_matnr
       AND m~mtart IN so_mtart
       AND m~ferth IN so_ferth
       AND m~formt IN so_formt
       AND m~labor IN so_labor
       AND m~matkl IN so_matkl
       AND m~ersda IN so_ersda
       AND m~laeda IN so_laeda
       AND c~kzkri IN so_kzkri
       AND c~beskz IN so_beskz
       AND c~sobsl IN so_sobsl
       AND c~lvorm IN so_lvorm
       AND c~fabkz IN so_fabkz
       AND c~dispo IN so_dispo
       AND c~ekgrp IN so_ekgrp
       AND m~tempb IN so_tempb
       AND m~mstae IN so_mstae
       AND m~aeszn IN so_aeszn
       AND exists ( SELECT *
                      FROM eord AS d
                  WHERE d~matnr  EQ c~matnr
                    AND d~werks  EQ c~werks
                    AND d~lifnr  IN so_lifnr
                    AND d~vdatu  <= sy-datum
                    AND d~bdatu  >= sy-datum
                    AND d~autet  NE space
                   ).

  ENDIF.

  DELETE xlist WHERE NOT bklas IN so_bklas.


ENDFORM.                    " read_material_master
*&---------------------------------------------------------------------*
*&      Form  CHECK_MATERIAL_STATUS
*&---------------------------------------------------------------------*
*       Check Material Status.
*----------------------------------------------------------------------*
FORM check_material_status.
  DATA: lv_len    TYPE i,
        lv_pos    TYPE i,
        lv_view.
  DATA: lv_mtart_view TYPE t134-pstat.
  LOOP AT xlist.
    IF NOT xlist-normt IS INITIAL.
      PERFORM read_text  USING xlist-normt
                      CHANGING xlist-pgntx.
    ENDIF.

*WB : WJIT
*EX : DJIT
*RO : RPCB
*MB : MJIT
    IF NOT xlist-disls IS INITIAL.
      CASE xlist-disls.
        WHEN 'WB'.
          xlist-disdc = 'WJIT'.
        WHEN 'EX' OR 'PK'.
          xlist-disdc = 'DJIT'.
        WHEN 'RO'.
          xlist-disdc = 'RPCB'.
        WHEN 'MB'.
          xlist-disdc = 'MJIT'.
      ENDCASE.
    ENDIF.

    IF pa_bom EQ 'X'.
*     Check BOM Existence
      PERFORM check_bom_existence.
    ELSE.
      xlist-status_bom1 = co_status_space.
      xlist-status_bomm = co_status_space.
    ENDIF.
*   Get Maintenance View for Material Type.
    PERFORM get_mtart_pstat USING    xlist-mtart
                            CHANGING lv_mtart_view.
    lv_len = strlen( lv_mtart_view ).
    CLEAR lv_pos.
    DO.
      IF lv_len = lv_pos. EXIT. ENDIF.
      lv_pos = sy-index - 1.
      lv_view = lv_mtart_view+lv_pos(1).
      IF lv_view CA xlist-vpsta.             "Maintenance status.
        IF lv_view CA co_client_level.
          PERFORM update_maintained_view USING lv_view.
        ELSE.
          IF lv_view CA co_plant_level AND
             lv_view CA xlist-pstat.
            PERFORM update_maintained_view USING lv_view.
          ELSE.
            PERFORM update_omitted_view USING lv_view.
          ENDIF.
        ENDIF.
      ELSE.
        PERFORM update_omitted_view USING lv_view.
      ENDIF.
    ENDDO.
    PERFORM get_storage_bin USING    xlist-matnr
                                     xlist-werks
                                     xlist-lgfsb
                                     xlist-inslc
                                     xlist-lgpro
                                     xlist-tempb
                            CHANGING xlist-lgpbe
                                     xlist-umlgpbe
                                     xlist-vbrst.
*   Check Purchasing Master Data
    PERFORM check_purchasing_master.
*   Check Control Cycle
    PERFORM check_control_cycle.
*   Check Packing Instruction.
    PERFORM check_packing_instruction.
*   Check Specific Data.
    PERFORM check_specific_data.
*   Check Storage loc in mrp2 view and Storage loc in SA 07/06/2010
    PERFORM check_storage_loc.

    PERFORM check_released_costings.
    PERFORM update_all_view_status.
    IF xlist-beskz EQ 'E' OR
       xlist-beskz EQ 'X'.
      PERFORM get_production_version USING    xlist-matnr
                                              xlist-werks
                                              pa_datuv
                                     CHANGING xlist-verid.
    ENDIF.
    MODIFY xlist.
  ENDLOOP.
ENDFORM.                    " CHECK_MATERIAL_STATUS
*&---------------------------------------------------------------------*
*&      Form  read_material_types
*&---------------------------------------------------------------------*
*       Read Material Types.
*----------------------------------------------------------------------*
FORM read_material_types.
  REFRESH x134.
  SELECT *
    INTO TABLE x134
    FROM t134
   WHERE mtart IN so_mtart.
ENDFORM.                    " read_material_types
*&---------------------------------------------------------------------*
*&      Form  GET_MTART_PSTAT
*&---------------------------------------------------------------------*
*       Get Maintenance Status for Material Type.
*----------------------------------------------------------------------*
*      -->I_MTART  Material Type.
*      <--E_PSTAT  Maintenance Status.
*----------------------------------------------------------------------*
FORM get_mtart_pstat USING    i_mtart
                     CHANGING e_pstat.
  CLEAR e_pstat.
  READ TABLE x134 WITH KEY mtart = i_mtart
                  BINARY SEARCH.
  IF sy-subrc EQ 0.
    e_pstat = x134-pstat.
  ENDIF.
ENDFORM.                    " get_mtart_pstat
*&---------------------------------------------------------------------*
*&      Form  UPDATE_MAINTAINED_VIEW
*&---------------------------------------------------------------------*
*       Update Maintained View.
*----------------------------------------------------------------------*
*      -->I_VIEW  View.
*----------------------------------------------------------------------*
FORM update_maintained_view USING    i_view.
  DATA : l_land1 TYPE lfa1-land1,
         l_cnt   TYPE i,
         l_lifnr TYPE lfa1-lifnr.

  CASE i_view.
    WHEN 'A'.   "Work Scheduling
      xlist-status_work     = co_status_green.
    WHEN 'B'.   "Accounting
      xlist-status_account  = co_status_green.
    WHEN 'C'.   "Classification
      xlist-status_class    = co_status_green.
    WHEN 'D'.   "MRP
*   1. MRP 1view? ??? ?? ???.
*   1.1 MRP group : R*??
*   1.2 Material Type : PART??
*       ??, MRP Group = RM10? ??? ????? ??
*
*   1.3 Material Type = RAW1??
*       MRP Group = RM10? ??? ???
*   1.4 Material Type = RAW2??
*       MRP Group = BM10? ??? ???

      IF xlist-disgr NE 'RM10'.
        IF xlist-mtart EQ 'RAW1' OR
           xlist-mtart EQ 'RAW2'.
          xlist-status_mrp1     = co_status_yellow.
        ELSE.
          xlist-status_mrp1     = co_status_green.
        ENDIF.
      ELSE.
        xlist-status_mrp1     = co_status_green.
      ENDIF.

      xlist-status_mrp2     = co_status_green.
      IF xlist-mtvfp IS INITIAL.
        IF xlist-sobsl(1) EQ '5'.
          xlist-status_mrp3     = co_status_yellow.
        ELSE.
          xlist-status_mrp3     = co_status_red.
        ENDIF.
      ELSE.
        xlist-status_mrp3     = co_status_green.
      ENDIF.
      IF xlist-sauft IS INITIAL.
        CASE xlist-beskz.
          WHEN 'F'.
            xlist-status_mrp4     = co_status_green.
          WHEN 'E'.
            IF xlist-sobsl(1) EQ '5'.
              xlist-status_mrp4     = co_status_green.
            ELSE.
              xlist-status_mrp4     = co_status_red.
            ENDIF.
          WHEN 'X'.
            CASE xlist-sobsl(1).
              WHEN '1' OR '2' OR '3' OR '4' OR '5' OR space.
                xlist-status_mrp4     = co_status_green.
              WHEN OTHERS.
                xlist-status_mrp4     = co_status_red.
            ENDCASE.
          WHEN OTHERS.
            xlist-status_mrp4     = co_status_green.
        ENDCASE.
      ELSE.
        xlist-status_mrp4     = co_status_green.
      ENDIF.
    WHEN 'E'.   "Purchasing
      IF xlist-mtart EQ 'SEMI'.
        xlist-status_purchase = co_status_green.
      ELSE.
        IF xlist-ekgrp IS INITIAL.
          xlist-status_purchase = co_status_red.
        ELSE.
          xlist-status_purchase = co_status_green.
        ENDIF.
      ENDIF.
      IF xlist-labor EQ 'K'.
        IF xlist-mtart EQ 'PART' OR
           xlist-mtart EQ 'PGN'.
          CLEAR : l_lifnr, l_land1.
          LOOP AT xeord WHERE matnr = xlist-matnr
                          AND werks = xlist-werks
                          AND autet <> space.
            l_lifnr = xeord-lifnr.
            EXIT.
          ENDLOOP.

          IF l_lifnr IS INITIAL.
            xlist-status_import = co_status_space.
          ELSE.
            SELECT SINGLE land1
                   INTO l_land1
                   FROM lfa1
                   WHERE lifnr = l_lifnr.
            IF l_land1 EQ 'US'.
              xlist-status_import = co_status_space.
            ELSE.
              IF xlist-stawn IS INITIAL.
                xlist-status_import   = co_status_red.
              ELSE.
                xlist-status_import = co_status_green.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          xlist-status_import = co_status_space.
        ENDIF.
      ELSE.
        xlist-status_import = co_status_space.
      ENDIF.
    WHEN 'F'.   "Production resources/tools
      xlist-status_tool     = co_status_green.
    WHEN 'G'.   "Costing
*      IF XLIST-PRCTR IS INITIAL.
*        XLIST-STATUS_COST = CO_STATUS_RED.
*      ELSE.
      xlist-status_cost = co_status_green.
*      ENDIF.
    WHEN 'K'.   "Basic
      xlist-status_basic    = co_status_green.
    WHEN 'L'.   "Storage
      CASE xlist-mtart.
        WHEN 'PGN'.
          xlist-status_storage  = co_status_green.
        WHEN OTHERS.
          READ TABLE x159l WITH KEY werks = xlist-werks.
          IF NOT xlist-lgpro IS INITIAL.
            CLEAR mard.
            SELECT SINGLE *
              FROM mard
             WHERE matnr = xlist-matnr
               AND werks = xlist-werks
               AND lgort = xlist-lgpro.
            IF sy-subrc <> 0.
*          IF X159L-XLAUT EQ 'X'.
*            XLIST-STATUS_STORAGE = CO_STATUS_YELLOW.
*          ELSE.
              xlist-status_storage = co_status_red.
*          ENDIF.
            ELSE.
              xlist-status_storage  = co_status_green.
            ENDIF.
          ENDIF.
          IF NOT xlist-lgfsb IS INITIAL.
            CLEAR mard.
            SELECT SINGLE *
              FROM mard
             WHERE matnr = xlist-matnr
               AND werks = xlist-werks
               AND lgort = xlist-lgfsb.
            IF sy-subrc <> 0.
*          IF X159L-XLAUT EQ 'X'.
*            XLIST-STATUS_STORAGE = CO_STATUS_YELLOW.
*          ELSE.
              xlist-status_storage = co_status_red.
*          ENDIF.
            ELSE.
              IF xlist-status_storage EQ co_status_green OR
                 xlist-status_storage EQ co_status_space.
                xlist-status_storage  = co_status_green.
              ENDIF.
            ENDIF.
          ENDIF.
          IF NOT xlist-inslc IS INITIAL.
            CLEAR mard.
            SELECT SINGLE *
              FROM mard
             WHERE matnr = xlist-matnr
               AND werks = xlist-werks
               AND lgort = xlist-inslc.
            IF sy-subrc <> 0.
*          IF X159L-XLAUT EQ 'X'.
*            XLIST-STATUS_STORAGE = CO_STATUS_YELLOW.
*          ELSE.
              xlist-status_storage = co_status_red.
*          ENDIF.
            ELSE.
              xlist-status_storage  = co_status_green.
            ENDIF.
          ENDIF.

      ENDCASE.
    WHEN 'P'.   "Forecasting
      xlist-status_forecast = co_status_green.
    WHEN 'Q'.   "Quality Management
      xlist-status_qm       = co_status_green.
    WHEN 'V'.   "Sales
      xlist-status_sales    = co_status_green.
  ENDCASE.
ENDFORM.                    " UPDATE_MAINTAINED_VIEW
*&---------------------------------------------------------------------*
*&      Form  UPDATE_OMITTED_VIEW
*&---------------------------------------------------------------------*
*       Update Omitted View.
*----------------------------------------------------------------------*
*      -->I_VIEW  View.
*----------------------------------------------------------------------*
FORM update_omitted_view USING    i_view.
  CASE i_view.
    WHEN 'A'.   "Work Scheduling
      xlist-status_work     = co_status_yellow.
    WHEN 'B'.   "Accounting
      IF xlist-sobsl(1) EQ '5'.
        xlist-status_account  = co_status_yellow.
      ELSE.
        xlist-status_account  = co_status_red.
      ENDIF.
    WHEN 'C'.   "Classification
      xlist-status_class    = co_status_yellow.
    WHEN 'D'.   "MRP
      IF xlist-sobsl(1) EQ '5'.
        xlist-status_mrp1     = co_status_yellow.
        xlist-status_mrp2     = co_status_yellow.
        xlist-status_mrp3     = co_status_yellow.
        xlist-status_mrp4     = co_status_yellow.
      ELSE.
        xlist-status_mrp1     = co_status_red.
        xlist-status_mrp2     = co_status_red.
        xlist-status_mrp3     = co_status_red.
        xlist-status_mrp4     = co_status_red.
      ENDIF.
    WHEN 'E'.   "Purchasing
      IF xlist-mtart EQ 'SEMI'.
        xlist-status_purchase = co_status_green.
      ELSE.
        CASE xlist-beskz.
          WHEN 'F'.
            IF xlist-sobsl(1) CA '5'.
              xlist-status_purchase = co_status_yellow.
            ELSE.
              xlist-status_purchase = co_status_red.
            ENDIF.
          WHEN 'X'.
            CASE xlist-sobsl(1).
              WHEN '1' OR '2' OR '3' OR '4'.
                xlist-status_purchase = co_status_red.
              WHEN OTHERS.
                xlist-status_purchase = co_status_yellow.
            ENDCASE.
          WHEN 'E'.
            xlist-status_purchase = co_status_space.
          WHEN OTHERS.
            xlist-status_purchase = co_status_yellow.
        ENDCASE.
      ENDIF.
    WHEN 'F'.   "Production resources/tools
      xlist-status_tool     = co_status_yellow.
    WHEN 'G'.   "Costing
      IF xlist-sobsl(1) EQ '5'.
        xlist-status_cost     = co_status_yellow.
      ELSE.
        IF xlist-mtart EQ 'SUBP'.
          xlist-status_cost     = co_status_yellow.
        ELSE.
          xlist-status_cost     = co_status_red.
        ENDIF.
      ENDIF.
    WHEN 'K'.   "Basic
      xlist-status_basic    = co_status_red.
    WHEN 'L'.   "Storage
      xlist-status_storage  = co_status_yellow.
    WHEN 'P'.   "Forecasting
      xlist-status_forecast = co_status_yellow.
    WHEN 'Q'.   "Quality Management
      xlist-status_qm       = co_status_yellow.
    WHEN 'V'.   "Sales
      CASE xlist-mtart.
        WHEN 'FSC'.
          xlist-status_sales    = co_status_red.
        WHEN OTHERS.
          IF xlist-beikz EQ 'C'.
            xlist-status_sales    = co_status_red.
          ELSE.
            xlist-status_sales    = co_status_yellow.
          ENDIF.
      ENDCASE.
  ENDCASE.
ENDFORM.                    " UPDATE_OMITTED_VIEW

*&---------------------------------------------------------------------*
*&      Form  CHECK_PURCHASING_MASTER
*&---------------------------------------------------------------------*
*       Check Purchasing Master
*----------------------------------------------------------------------*
FORM check_purchasing_master.
  DATA: lv_fixed TYPE i,
        lv_count TYPE i,
        l_rksta  TYPE pkhd-rksta.
  DATA: it_quota  LIKE TABLE OF mdrp_quota WITH HEADER LINE,
        it_eine   LIKE TABLE OF bapieine   WITH HEADER LINE,
        it_return LIKE TABLE OF bapireturn WITH HEADER LINE.
  DATA: lv_info_type TYPE eine-esokz.

  CHECK xlist-mtart EQ 'SUBP' OR
        xlist-mtart EQ 'PART' OR
        xlist-mtart EQ 'PGN'  OR
        xlist-mtart EQ 'RAW1' OR
        xlist-mtart EQ 'RAW2'.

  CHECK xlist-sobsl(1) NE '5'.

  IF xlist-mtart EQ 'PGN'.
    PERFORM get_related_material USING xlist-ferth
                              CHANGING xlist-lifnr.
    PERFORM read_vendor_name USING    xlist-lifnr
                             CHANGING xlist-name1.
    EXIT.
  ENDIF.

  CASE xlist-sobsl.
    WHEN '10'.       "Consignment
      lv_info_type = '0'.
    WHEN '30'.       "Subcontracting
      lv_info_type = '3'.
    WHEN OTHERS.     "Standard
      lv_info_type = '0'.
  ENDCASE.
  IF xlist-sobsl(1) EQ '4'.
    CLEAR: xlist-status_inforec,
           xlist-status_sa.
    CLEAR t460a.
*---Check Special Procurement Type.
    CALL FUNCTION 'T460A_SINGLE_READ'
         EXPORTING
              t460a_werks = xlist-werks
              t460a_sobsl = xlist-sobsl
         IMPORTING
              wt460a      = t460a
         EXCEPTIONS
              not_found   = 1
              OTHERS      = 2.
*---Check Source List for Supplying plant.
    READ TABLE xeord WITH KEY matnr = xlist-matnr
                              werks = xlist-werks.
    IF sy-subrc EQ 0.
      xlist-eortp = xeord-eortp.
      xlist-status_source = co_status_green.
    ELSE.
      IF xlist-kordb EQ 'X'.
        xlist-status_source = 'A'.
      ELSE.
        xlist-status_source = space.
      ENDIF.
    ENDIF.
  ELSE.
* Check Info.Record.
    CLEAR xinfo.
    READ TABLE xinfo WITH KEY matnr = xlist-matnr
                              esokz = lv_info_type.
    IF sy-subrc EQ 0.
      xlist-status_inforec = co_status_green.
    ELSE.
      IF xlist-beskz EQ 'X'.
        xlist-status_inforec = co_status_yellow.
      ELSE.
        xlist-status_inforec = co_status_red.
      ENDIF.
    ENDIF.


    CHECK xlist-status_inforec = co_status_green.

    IF  xlist-mtart EQ 'RAW2'.
      xlist-status_source = co_status_green.
      xlist-status_sa     = co_status_green.
    ELSE.
* Check Source List.
      READ TABLE xeord WITH KEY matnr = xlist-matnr
                                werks = xlist-werks.
      IF sy-subrc EQ 0.
        xlist-eortp = xeord-eortp.
        xlist-ebeln = xeord-ebeln.
        xlist-ebelp = xeord-ebelp.
        xlist-lifnr = xeord-lifnr.
        PERFORM read_vendor_name USING    xlist-lifnr
                                 CHANGING xlist-name1.
        IF xeord-vrtyp EQ 'L'.
          CASE xlist-sobsl.
            WHEN '30' OR '31'.
              IF xlist-eortp EQ '3'.
                xlist-status_source = co_status_green.
                xlist-status_sa     = co_status_green.
              ELSE.
                IF xlist-mtart EQ 'PART'.
                  xlist-status_source = co_status_red.
                ELSE.
                  xlist-status_source = co_status_yellow.
                ENDIF.
                xlist-status_sa     = co_status_green.
              ENDIF.
            WHEN '10'.
              IF xlist-eortp EQ '2'.
                xlist-status_source = co_status_green.
                xlist-status_sa     = co_status_green.
              ELSE.
                IF xlist-mtart EQ 'PART'.
                  xlist-status_source = co_status_red.
                ELSE.
                  xlist-status_source = co_status_yellow.
                ENDIF.
                xlist-status_sa     = co_status_green.
              ENDIF.
            WHEN OTHERS.
              IF xlist-eortp EQ '0'.
                xlist-status_source = co_status_green.
                xlist-status_sa     = co_status_green.
              ELSE.
                IF xlist-mtart EQ 'PART'.
                  xlist-status_source = co_status_red.
                ELSE.
                  xlist-status_source = co_status_yellow.
                ENDIF.
                xlist-status_sa     = co_status_green.
              ENDIF.
          ENDCASE.
        ELSE.
          IF xlist-fabkz = '1'.        "JIT Indicator
            IF xlist-mtart EQ 'PART'.
              xlist-status_source = co_status_red.
              xlist-status_sa     = co_status_red.
            ELSE.
              xlist-status_source = co_status_yellow.
              xlist-status_sa     = co_status_yellow.
            ENDIF.
          ELSE.
            xlist-status_source = co_status_green.
          ENDIF.
        ENDIF.

      ELSE.
        IF xinfo-lifnr <> space. "IS NOT INITIAL.
          xlist-lifnr = xinfo-lifnr.
          PERFORM read_vendor_name USING    xlist-lifnr
                                   CHANGING xlist-name1.
        ENDIF.
        IF xlist-kordb EQ 'X'.
          xlist-status_source = co_status_red.
        ELSE.
          IF xlist-fabkz = '1'.        "JIT Indicator
            xlist-status_source = co_status_red.
            xlist-status_sa     = co_status_yellow.
          ELSE.
            xlist-status_source = co_status_red.
            xlist-status_sa     = co_status_space.
          ENDIF.
        ENDIF.
        SELECT SINGLE ebeln ebelp
               INTO (xlist-ebeln, xlist-ebelp)
               FROM ekpo
               WHERE matnr = xlist-matnr
                 AND loekz = space.
        IF sy-subrc EQ 0.
          xlist-status_sa     = co_status_green.
        ELSE.
          xlist-status_sa     = co_status_red.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  IF xlist-mtart         EQ 'SUBP' AND
     xlist-status_sa     EQ co_status_red.
    xlist-status_sa     = co_status_yellow.
  ENDIF.
  IF xlist-mtart         EQ 'SUBP' AND
     xlist-status_source EQ co_status_red.
    xlist-status_source = co_status_yellow.
  ENDIF.
* Check Quota Arrangement.
*    LOOP AT IT_EORD WHERE NOTKZ EQ CO_STATUS_SPACE
*                      AND VDATU <= SY-DATUM
*                      AND BDATU >= SY-DATUM.
*      CHECK IT_EORD-EBELN IS INITIAL.
*      LV_COUNT = LV_COUNT + 1.
*    ENDLOOP.
*    CHECK LV_COUNT > 1.
*    CALL FUNCTION 'MDRP_QUOTA_READ'
*      EXPORTING
*        I_MATNR    = XLIST-MATNR
*        I_WERKS    = XLIST-WERKS
*      TABLES
*        T_QUOTA    = IT_QUOTA
*      EXCEPTIONS
*        READ_ERROR = 1
*        OTHERS     = 2.
*    CLEAR LV_COUNT.
*    DESCRIBE TABLE IT_QUOTA LINES LV_COUNT.
*    IF LV_COUNT > 0.
*      XLIST-STATUS_SA = CO_STATUS_GREEN.
*    ELSE.
*      XLIST-STATUS_SA = CO_STATUS_RED.
*    ENDIF.
*  ENDIF.
ENDFORM.                    " CHECK_PURCHASING_MASTER
*&---------------------------------------------------------------------*
*&      Form  CHECK_BOM_EXISTENCE
*&---------------------------------------------------------------------*
*       Check BOM Exist or not.
*----------------------------------------------------------------------*
FORM check_bom_existence.
  DATA it_return TYPE TABLE OF bapiret2 WITH HEADER LINE.
  DATA:
    equicat TYPE TABLE OF cscequi WITH HEADER LINE,
    kndcat  TYPE TABLE OF cscknd  WITH HEADER LINE,
    matcat  TYPE TABLE OF cscmat  WITH HEADER LINE,
    stdcat  TYPE TABLE OF cscstd  WITH HEADER LINE,
    tplcat  TYPE TABLE OF csctpl  WITH HEADER LINE,
    stpox   TYPE TABLE OF stpox   WITH HEADER LINE.
  DATA lv_whereused LIKE sy-subrc.
  DATA: lv_mehrs TYPE csdata-xfeld,
        lv_beikz TYPE stpo-beikz.
  REFRESH: it_wultb,
           stpox.
  CASE xlist-beskz.
    WHEN 'E'.
*-- Production BOM
      READ TABLE xmast WITH KEY matnr = xlist-matnr
                                werks = xlist-werks
                                stlan = '1'.
      IF sy-subrc EQ 0.
        xlist-status_bom1 = co_status_green.
      ELSE.
        xlist-status_bom1 = co_status_yellow.
      ENDIF.
*-- Module BOM
      READ TABLE xmast WITH KEY matnr = xlist-matnr
                                werks = xlist-werks
                                stlan = 'M'.
      IF sy-subrc EQ 0.
        xlist-status_bomm = co_status_green.
      ELSE.
        xlist-status_bomm = co_status_yellow.
      ENDIF.
    WHEN 'F' OR ' '.
*-- Production BOM
      READ TABLE xmast WITH KEY matnr = xlist-matnr
                                werks = xlist-werks
                                stlan = '1'.
      IF sy-subrc EQ 0.
        xlist-status_bom1 = co_status_green.
        lv_mehrs = 'X'.
        CASE xlist-sobsl.
          WHEN '30'.
            lv_beikz = 'F'.
          WHEN '31'.
            lv_beikz = 'C'.
          WHEN OTHERS.
            CLEAR lv_beikz.
        ENDCASE.
        IF xlist-sobsl EQ '30' OR
           xlist-sobsl EQ '31'.
*==== Production BOM
          CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
               EXPORTING
                    beikz                 = lv_beikz
                    capid                 = 'PP01'
                    datuv                 = pa_datuv
                    mtnrv                 = xlist-matnr
                    stlan                 = '1'
                    mehrs                 = lv_mehrs
                    werks                 = xlist-werks
               TABLES
                    stb                   = stpox
               EXCEPTIONS
                    alt_not_found         = 1
                    call_invalid          = 2
                    material_not_found    = 3
                    missing_authorization = 4
                    no_bom_found          = 5
                    no_plant_data         = 6
                    no_suitable_bom_found = 7
                    conversion_error      = 8
                    OTHERS                = 9.
          IF stpox[] IS INITIAL.
            xlist-status_bom1 = co_status_red.
          ELSE.
            READ TABLE stpox WITH KEY beikz = lv_beikz.
            IF sy-subrc EQ 0.
              xlist-status_bom1 = co_status_green.
            ELSE.
              xlist-status_bom1 = co_status_red.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        REFRESH: it_wultb,
                 stpox.
        CALL FUNCTION 'CS_WHERE_USED_MAT'
             EXPORTING
                  datub                      = '99991231'
                  datuv                      = pa_datuv
                  matnr                      = xlist-matnr
                  stlan                      = '1'
                  werks                      = xlist-werks
             TABLES
                  wultb                      = it_wultb
                  equicat                    = equicat
                  kndcat                     = kndcat
                  matcat                     = matcat
                  stdcat                     = stdcat
                  tplcat                     = tplcat
             EXCEPTIONS
                  call_invalid               = 1
                  material_not_found         = 2
                  no_where_used_rec_found    = 3
                  no_where_used_rec_selected = 4
                  no_where_used_rec_valid    = 5
                  OTHERS                     = 6.
        lv_whereused = sy-subrc.
        IF lv_whereused EQ 0.
          IF xlist-status_bom1 <> co_status_red.
            xlist-status_bom1 = co_status_green.
          ENDIF.
*-----Material Provision Indicator.( F / C )
*-----Material provided by vendor
          READ TABLE it_wultb WITH KEY beikz = 'C'.
          IF sy-subrc EQ 0.
            xlist-beikz = 'C'.
          ELSE.
            READ TABLE it_wultb WITH KEY beikz = 'F'.
            IF sy-subrc EQ 0.
              xlist-beikz = 'F'.
            ENDIF.
          ENDIF.
        ELSE.
          IF xlist-status_bom1 <> co_status_red.
            xlist-status_bom1 = co_status_yellow.
          ENDIF.
        ENDIF.
      ENDIF.

*-- Module BOM
      READ TABLE xmast WITH KEY matnr = xlist-matnr
                                werks = xlist-werks
                                stlan = 'M'.
      IF sy-subrc EQ 0.
        xlist-status_bomm = co_status_green.
      ELSE.
        REFRESH: it_wultb,
                 stpox.
        CALL FUNCTION 'CS_WHERE_USED_MAT'
             EXPORTING
                  datub                      = '99991231'
                  datuv                      = pa_datuv
                  matnr                      = xlist-matnr
                  stlan                      = 'M'
                  werks                      = xlist-werks
             TABLES
                  wultb                      = it_wultb
                  equicat                    = equicat
                  kndcat                     = kndcat
                  matcat                     = matcat
                  stdcat                     = stdcat
                  tplcat                     = tplcat
             EXCEPTIONS
                  call_invalid               = 1
                  material_not_found         = 2
                  no_where_used_rec_found    = 3
                  no_where_used_rec_selected = 4
                  no_where_used_rec_valid    = 5
                  OTHERS                     = 6.
        lv_whereused = sy-subrc.
        IF lv_whereused EQ 0.
          xlist-status_bomm = co_status_green.
        ELSE.
          xlist-status_bomm = co_status_yellow.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
      xlist-status_bom1 = co_status_yellow.
      xlist-status_bomm = co_status_yellow.
  ENDCASE.
ENDFORM.                    " CHECK_BOM_EXISTENCE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ALL_VIEW_STATUS
*&---------------------------------------------------------------------*
*       Update All view Status.
*----------------------------------------------------------------------*
FORM update_all_view_status.
  IF xlist-mtart EQ 'RAW1' OR
     xlist-mtart EQ 'RAW2'.
    IF xlist-status_basic    EQ co_status_red OR
       xlist-status_class    EQ co_status_red OR
       xlist-status_sales    EQ co_status_red OR
       xlist-status_export   EQ co_status_red OR
       xlist-status_purchase EQ co_status_red OR
       xlist-status_import   EQ co_status_red OR
       xlist-status_mrp1     EQ co_status_red OR
       xlist-status_mrp2     EQ co_status_red OR
       xlist-status_mrp3     EQ co_status_red OR
       xlist-status_mrp4     EQ co_status_red OR
       xlist-status_storage  EQ co_status_red OR
       xlist-status_qm       EQ co_status_red OR
       xlist-status_account  EQ co_status_red OR
       xlist-status_cost     EQ co_status_red OR
       xlist-status_inforec  EQ co_status_red OR
       xlist-status_source   EQ co_status_red OR
       xlist-status_sa       EQ co_status_red OR
       xlist-status_pi       EQ co_status_red.
      xlist-light = '1'.
    ELSE.
      xlist-light = '3'.
    ENDIF.

  ELSE.

    IF xlist-status_basic    EQ co_status_red OR
       xlist-status_class    EQ co_status_red OR
       xlist-status_sales    EQ co_status_red OR
       xlist-status_export   EQ co_status_red OR
       xlist-status_purchase EQ co_status_red OR
       xlist-status_import   EQ co_status_red OR
       xlist-status_mrp1     EQ co_status_red OR
       xlist-status_mrp2     EQ co_status_red OR
       xlist-status_mrp3     EQ co_status_red OR
       xlist-status_mrp4     EQ co_status_red OR
       xlist-status_storage  EQ co_status_red OR
       xlist-status_qm       EQ co_status_red OR
       xlist-status_account  EQ co_status_red OR
       xlist-status_cost     EQ co_status_red OR
       xlist-status_inforec  EQ co_status_red OR
       xlist-status_source   EQ co_status_red OR
       xlist-status_sa       EQ co_status_red OR
       xlist-status_pi       EQ co_status_red OR
       xlist-status_kanban   EQ co_status_red OR
       xlist-status_bom1     EQ co_status_red OR
       xlist-status_jitcall  EQ co_status_red OR
       xlist-status_repl     EQ co_status_red.
      xlist-light = '1'.
    ELSE.
      xlist-light = '3'.
    ENDIF.
  ENDIF.
ENDFORM.                    " UPDATE_ALL_VIEW_STATUS
*&---------------------------------------------------------------------*
*&      Form  CHECK_TCODE_AUTHORITY
*&---------------------------------------------------------------------*
*       Check TCode Authority.
*----------------------------------------------------------------------*
FORM check_tcode_authority USING u_tcode .
* Authority Check
  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
       EXPORTING
            tcode  = u_tcode
       EXCEPTIONS
            ok     = 0
            not_ok = 1
            OTHERS = 2.
  IF sy-subrc NE 0.
    MESSAGE e172(00) WITH u_tcode RAISING no_authority.
  ENDIF.
ENDFORM.                    " CHECK_TCODE_AUTHORITY
*&---------------------------------------------------------------------*
*&      Form  READ_DEFAULT_VALUES_FOR_IM
*&---------------------------------------------------------------------*
*       Read Default values for Inventory Management.
*----------------------------------------------------------------------*
FORM read_default_values_for_im .
  SELECT *
    INTO TABLE x159l
    FROM t159l
   WHERE werks IN so_werks.
ENDFORM.                    " READ_DEFAULT_VALUES_FOR_IM
*&---------------------------------------------------------------------*
*&      Form  READ_PLANT_DATA
*&---------------------------------------------------------------------*
*       Read Plant Data.
*----------------------------------------------------------------------*
FORM read_plant_data .
  SELECT *
    INTO TABLE x001w
    FROM t001w
   WHERE werks IN so_werks.
ENDFORM.                    " READ_PLANT_DATA
*&---------------------------------------------------------------------*
*&      Form  FILTER_MATERIAL_STATUS
*&---------------------------------------------------------------------*
*       Set Filter Material Status.
*----------------------------------------------------------------------*
FORM filter_material_status.
  IF     pa_error EQ 'X'.
    DELETE xlist WHERE light NE '1'.
  ELSEIF pa_norm  EQ 'X'.
    DELETE xlist WHERE light EQ '1'.
  ENDIF.
ENDFORM.                    " FILTER_MATERIAL_STATUS
*&---------------------------------------------------------------------*
*&      Form  VARIANT_INIT
*&---------------------------------------------------------------------*
*       Variant Initialize.
*----------------------------------------------------------------------*
FORM variant_init.
  CLEAR g_variant.
  g_variant-report = g_repid.
ENDFORM.                               " VARIANT_INIT
*-----------------------------------------------------------------------
*       FORM PF_STATUS_SET
*-----------------------------------------------------------------------
*       Set Status.
*----------------------------------------------------------------------*
FORM pf_status_set USING  extab TYPE slis_t_extab.
  DATA:BEGIN OF ftab OCCURS 0,
       fcode LIKE rsmpe-func,
      END   OF ftab.
* clear extab.refresh extab.
  IF pref_3_o <> 'X' OR act_butt <> 'X'.
    ftab-fcode = 'ACMD'.
    APPEND ftab.
  ENDIF.
  IF pref_2 = 'X'.
    ftab-fcode = 'AEN2'.
  ELSE.
    ftab-fcode = 'ANZ2'.
  ENDIF.
  APPEND ftab.
  IF pref_3_o = 'X'.
    ftab-fcode = 'AEN2'.
    APPEND ftab.
  ENDIF.

  IF sy-tcode EQ 'ZMMR00300T'.
    ftab-fcode = 'BACK'.
    APPEND ftab.
  ENDIF.

  SET PF-STATUS 'G0500'  EXCLUDING ftab.
* endif.
ENDFORM.                               " PF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM user_command USING r_ucomm     LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  DATA:
    lf_tcode  TYPE sy-tcode VALUE 'SLIS_DUMMY',
    ls_status LIKE LINE OF xlist,
    lf_matnr  LIKE mara-matnr,
    change_m,
    tr_cd LIKE sy-tcode,
    l_chk.
  DATA:l_extab TYPE slis_t_extab.

  CASE r_ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'MESA'.
      PERFORM change_sched_agreement.
      rs_selfield-refresh    = 'X'.
      rs_selfield-col_stable = 'X'.
      rs_selfield-row_stable = 'X'.
    WHEN 'MMSC'.
      PERFORM extend_storagelocation.
      PERFORM read_main_data.
      rs_selfield-refresh    = 'X'.
      rs_selfield-col_stable = 'X'.
      rs_selfield-row_stable = 'X'.
    WHEN 'COPY'.
      PERFORM copy_and_paste_material USING l_chk.
      IF l_chk EQ ' '.
        PERFORM read_main_data .
        rs_selfield-refresh   = 'X'.
        rs_selfield-col_stable = 'X'.
        rs_selfield-row_stable = 'X'.
      ENDIF.
    WHEN 'REFR'.
      PERFORM read_main_data .
      rs_selfield-refresh   = 'X'.
      rs_selfield-col_stable = 'X'.
      rs_selfield-row_stable = 'X'.

    WHEN 'ACMD'.
      CLEAR:pref_3_o,act_butt.
      pref_3 = 'X'.
      PERFORM pf_status_set USING l_extab.
    WHEN '&IC1' OR 'AEN2' OR 'ANZ2'.    "change or doubleclick
      PERFORM check_vendor_user USING g_rc.

      CHECK g_rc IS INITIAL.

      CLEAR act_butt.
      IF r_ucomm = 'AEN2' OR ( r_ucomm = '&IC1' AND pref_2 = 'X' ).
        change_m = 'X'.
      ELSE.
        CLEAR change_m.
      ENDIF.
      PERFORM pf_status_set USING l_extab.
      READ TABLE xlist INTO ls_status INDEX rs_selfield-tabindex.

*
      IF change_m IS INITIAL AND first_act = 'X' AND pref_3_o <> 'X'.
        MESSAGE  ID 'VL' TYPE 'S' NUMBER '001' WITH
          'Change-mode First,Please close and go to Display'.
        PERFORM maintain_display USING 'X' ls_status-matnr
                                           ls_status-werks
                                           co_basic
                                           'MM02' 'MM03'.
        CLEAR first_act.
      ENDIF.
*
      IF sy-subrc EQ 0.
        CASE rs_selfield-fieldname.
          WHEN 'STATUS_BASIC' OR 'MATKL' OR 'RMATP' OR 'MSTAE' OR
               'LABOR' OR 'MEINS' OR 'MATNR' OR 'MAKTX' OR
               'FERTH' OR 'FORMT'.
*-----------Material Basic View.
            IF ls_status-vpsta CA 'K'.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_basic
                                             'MM02' 'MM03'.
            ELSE.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_basic
                                            'MM01' 'MM03'.
            ENDIF.
          WHEN 'STATUS_CLASS'.
*-----------Material Classification View.
            IF ls_status-vpsta CA 'C'.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_class
                                             'MM02' 'MM03'.
            ELSE.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_class
                                             'MM01' 'MM03'.
            ENDIF.
          WHEN 'STATUS_WORK'.
*-----------Material Work Scheduling View.
            IF ls_status-pstat CA 'A'.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_work
                                             'MM02' 'MM03'.
            ELSE.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_work
                                             'MM01' 'MM03'.
            ENDIF.
          WHEN 'STATUS_FORECAST'.
*-----------Material Forecast View.
            IF ls_status-pstat CA 'P'.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_forecast
                                             'MM02' 'MM03'.
            ELSE.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_forecast
                                             'MM01' 'MM03'.
            ENDIF.
          WHEN 'STATUS_TOOL'.
*-----------Material Tools View.
            IF ls_status-pstat CA 'F'.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_tools
                                             'MM02' 'MM03'.
            ELSE.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_tools
                                             'MM01' 'MM03'.
            ENDIF.
          WHEN 'STATUS_PURCHASE' OR 'EKGRP' OR 'KAUTB' OR 'KORDB' OR
               'FABKZ' OR 'WEBAZ' OR 'KZKRI'.
*-----------Material Purchasing View.
            IF ls_status-pstat CA 'E'.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_purchasing
                                             'MM02' 'MM03'.
            ELSE.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_purchasing
                                             'MM01' 'MM03'.
            ENDIF.
          WHEN 'STAWN'.
*-----------Material Foreign Trage : Import View.
            IF ls_status-pstat CA 'E'.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_import
                                             'MM02' 'MM03'.
            ELSE.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_import
                                             'MM01' 'MM03'.
            ENDIF.
          WHEN 'STATUS_EXPORT'.
*-----------Material Foreign Trage : Export View.
            IF ls_status-pstat CA 'V'.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_export
                                             'MM02' 'MM03'.
            ELSE.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_export
                                             'MM01' 'MM03'.
            ENDIF.
          WHEN 'STATUS_IMPORT'.
*-----------Material Foreign Trage : Import View.
            IF ls_status-pstat CA 'E'.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_import
                                             'MM02' 'MM03'.
            ELSE.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_import
                                             'MM01' 'MM03'.
            ENDIF.
          WHEN 'STATUS_MRP1' OR 'DISGR' OR 'MAABC' OR 'DISMM' OR
               'DISPO' OR 'DISLS' OR 'BSTRF'.
*-----------Material MRP1 View.
            IF ls_status-pstat CA 'D'.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_mrp1
                                             'MM02' 'MM03'.
            ELSE.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_mrp1
                                             'MM01' 'MM03'.
            ENDIF.
          WHEN 'STATUS_MRP2' OR 'BESKZ' OR 'SOBSL' OR 'LGPRO' OR
                     'LGFSB' OR 'PLIFZ' OR 'FHORI' OR 'VSPVB' OR
                     'SHZET' OR 'EISBE' OR 'INSLC'.
*-----------Material MRP2 View.
            IF ls_status-pstat CA 'D'.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_mrp2
                                             'MM02' 'MM03'.
            ELSE.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_mrp2
                                             'MM01' 'MM03'.
            ENDIF.
          WHEN 'STATUS_MRP3' OR 'MTVFP'.
*-----------Material MRP3 View.
            IF ls_status-pstat CA 'D'.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_mrp3
                                             'MM02' 'MM03'.
            ELSE.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_mrp3
                                             'MM01' 'MM03'.
            ENDIF.
          WHEN 'STATUS_MRP4' OR 'SAUFT' OR 'SBDKZ' OR 'KZBED'.
*-----------Material MRP4 View.
            IF ls_status-pstat CA 'D'.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_mrp4
                                             'MM02' 'MM03'.
            ELSE.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_mrp4
                                             'MM01' 'MM03'.
            ENDIF.
          WHEN 'STATUS_QM'.
*-----------Material Quality View.
            IF ls_status-pstat CA 'Q'.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_quality
                                             'MM02' 'MM03'.
            ELSE.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_quality
                                             'MM01' 'MM03'.
            ENDIF.
          WHEN 'STATUS_STORAGE' OR 'TEMPB' OR 'LOGGR'.
*-----------Material Storage View.
            IF ls_status-pstat CA 'L'.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_storage
                                             'MM02' 'MM03'.
            ELSE.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_storage
                                             'MM01' 'MM03'.
            ENDIF.
          WHEN 'XMCNG'.
            PERFORM maintain_display USING change_m
                                           ls_status-matnr
                                           ls_status-werks
                                           co_storage2
                                           'MM02' 'MM03'.
          WHEN 'VBRST'.
            PERFORM enter_storage_locations  USING ls_status-matnr
                                                   ls_status-werks
                                                   ls_status-lgpro
                                                   change_m.
          WHEN 'LGPBE'.
            PERFORM enter_storage_locations  USING ls_status-matnr
                                                   ls_status-werks
                                                   ls_status-lgfsb
                                                   change_m.
          WHEN 'UMLGPBE'.
            PERFORM enter_storage_locations  USING ls_status-matnr
                                                   ls_status-werks
                                                   ls_status-inslc
                                                   change_m.
          WHEN 'ZEINR'.
            PERFORM popup_get_runningno USING 'ALL'
                                              ls_status-matnr
                                              ls_status-werks
                                              ls_status-zeinr.


          WHEN 'STATUS_ACCOUNT' OR 'BKLAS'.
*-----------Material Accounting View.
            IF ls_status-pstat CA 'B'.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_accounting1
                                             'MM02' 'MM03'.
            ELSE.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_accounting1
                                             'MM01' 'MM03'.
            ENDIF.
          WHEN 'STATUS_COST' OR 'NCOST'.
*-----------Material Costing View.
            IF ls_status-pstat CA 'G'.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_costing1
                                             'MM02' 'MM03'.
            ELSE.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_costing1
                                             'MM01' 'MM03'.
            ENDIF.
          WHEN 'FREIG'.
*-----------Material Costing View.
            IF ls_status-pstat CA 'G'.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_costing2
                                             'MM02' 'MM03'.
            ELSE.
              PERFORM maintain_display USING change_m
                                             ls_status-matnr
                                             ls_status-werks
                                             co_costing2
                                             'MM01' 'MM03'.
            ENDIF.
          WHEN 'STATUS_SOURCE'.
*-----------Source List.
            CASE ls_status-status_source.
              WHEN co_status_red.
                PERFORM maintain_display USING change_m
                                               ls_status-matnr
                                               ls_status-werks
                                               ''
                                               'ME01' 'ME03'.
              WHEN OTHERS.
                PERFORM maintain_display USING change_m
                                               ls_status-matnr
                                               ls_status-werks
                                               ''
                                               'ME03' 'ME03'.
            ENDCASE.
          WHEN 'STATUS_INFOREC'.
*-----------Info.Record.
            READ TABLE x001w WITH KEY werks = ls_status-werks.
            IF sy-subrc EQ 0.
              SET PARAMETER ID 'EKO' FIELD x001w-ekorg.
            ENDIF.
            CASE ls_status-status_inforec.
              WHEN co_status_red.
                PERFORM maintain_display USING change_m
                                               ls_status-matnr
                                               ls_status-werks
                                               ''
                                               'ME11' 'ME13'.

              WHEN OTHERS.
                PERFORM maintain_display USING change_m
                                               ls_status-matnr
                                               ls_status-werks
                                               ''
                                               'ME1M' 'ME1M'.
            ENDCASE.
          WHEN 'STATUS_SA' OR 'EBELN' OR 'EBELP' OR 'KANBA' OR 'LGBZO'.
            IF ls_status-ebeln IS INITIAL.
              PERFORM maintain_display USING change_m
                                            ls_status-matnr
                                            ls_status-werks
                                            ''
                                            'ME31L' 'M33L'.
            ELSE.
              CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
                   EXPORTING
                        i_ebeln              = ls_status-ebeln
                        i_ebelp              = ls_status-ebelp
                        i_edit               = change_m
                   EXCEPTIONS
                        not_found            = 1
                        no_authority         = 2
                        invalid_call         = 3
                        preview_not_possible = 4
                        OTHERS               = 5.
            ENDIF.
          WHEN 'STATUS_PI' OR 'PIID' OR 'LOADCARR' OR 'TRGQTY'.
*-----------Packing Instruction.
            IF change_m = 'X'.
              CASE ls_status-status_pi.
                WHEN co_status_red OR co_status_yellow.
                  PERFORM maintain_display USING change_m
                                                ls_status-matnr
                                                ls_status-werks
                                                ''
                                                'POP1' 'POP3'.

                WHEN OTHERS.
*                PERFORM check_tcode_authority USING 'POP2'.
                  PERFORM check_tcode_auth_nw USING 'POP2' CHANGING
tr_cd.
                  SET PARAMETER ID 'PIID' FIELD ls_status-piid.
                  CALL TRANSACTION tr_cd AND SKIP FIRST SCREEN .
              ENDCASE.
            ELSE.
              SET PARAMETER ID 'PIID' FIELD ls_status-piid.
              CALL TRANSACTION 'POP3' AND SKIP FIRST SCREEN .
            ENDIF.
          WHEN 'PKNUM' OR 'STATUS_KANBAN'.
            IF change_m EQ 'X'.
              CALL FUNCTION 'PK_CHANGE_SINGLE_CCY'
                   EXPORTING
                        pknum_iv      = ls_status-pknum
                   EXCEPTIONS
                        ccy_not_exist = 1
                        OTHERS        = 2.
            ELSE.
              CALL FUNCTION 'PK_DISPLAY_CCY'
                   EXPORTING
                        pknum_iv = ls_status-pknum.
            ENDIF.
        ENDCASE.
      ENDIF.
  ENDCASE.
ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG_BUILD
*&---------------------------------------------------------------------*
*       Build Field Catalog.
*----------------------------------------------------------------------*
*      -->T_FIELDCAT  Field Catalog Table.
*----------------------------------------------------------------------*
FORM field_catalog_build TABLES   et_fieldcat TYPE slis_t_fieldcat_alv.
*-- Fieldcatalog create automatically --------------------------------*

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name   = g_repid
            i_structure_name = 'ZMMS0001'
       CHANGING
            ct_fieldcat      = et_fieldcat[].

  LOOP AT et_fieldcat.
    CASE et_fieldcat-fieldname.
      WHEN 'MATNR'.
        et_fieldcat-key     = 'X'.
      WHEN 'WERKS'.
        et_fieldcat-key     = 'X'.
      WHEN 'PRCTR' OR 'MTVFP' OR 'SAUFT' OR 'VPSTA' OR
           'PSTAT' OR 'BISMT' OR 'LOGGR' OR 'SBDKZ' OR
           'KZBED' OR 'RDPRF' OR 'PIID'  OR 'MBRSH' OR
           'STATUS_QM' OR 'STATUS_WORK' OR 'STATUS_CLASS' OR
           'STATUS_FORECAST' OR 'STATUS_TOOL'.
        et_fieldcat-no_out  = 'X'.
      WHEN OTHERS.
    ENDCASE.
    MODIFY et_fieldcat.
  ENDLOOP.
ENDFORM.                    " FIELD_CATALOG_BUILD

*---------------------------------------------------------------------*
*       FORM LAYOUT_INIT                                              *
*---------------------------------------------------------------------*
*       Layout Initialize.                                            *
*---------------------------------------------------------------------*
*  -->  RS_LAYOUT                                                     *
*---------------------------------------------------------------------*
FORM layout_init USING rs_layout TYPE slis_layout_alv.
*"Build layout for list display
*  rs_layout-detail_popup      = 'X'.
  rs_layout-lights_fieldname  = g_lights_fieldname.
  rs_layout-box_fieldname     = 'MARK'.
  rs_layout-zebra             = 'X'.
  rs_layout-colwidth_optimize = 'X'.
*  RS_LAYOUT-INFO_FIELDNAME    = 'COLOR'.
  rs_layout-coltab_fieldname  = 'COLTAB'. "Cell Color
ENDFORM.                    "LAYOUT_INIT
*&---------------------------------------------------------------------*
*&      Form  LIST_VIEWER
*&---------------------------------------------------------------------*
*       List Viewer Call.
*----------------------------------------------------------------------*
FORM list_viewer.
  DATA: lf_tfill     TYPE sy-tfill,
        lf_count(10) TYPE c.
  DESCRIBE TABLE xlist LINES lf_tfill.
  WRITE lf_tfill TO lf_count.
  CONCATENATE sy-title '(' lf_count ')' INTO sy-title.
  PERFORM field_catalog_build TABLES gt_fieldcat.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program       = g_repid
            i_callback_pf_status_set = g_status
            i_callback_user_command  = g_user_command
            i_structure_name         = 'ZMMS0001'
            it_fieldcat              = gt_fieldcat
            is_layout                = gs_layout
            it_filter                = gt_filter[]
            i_save                   = g_save
            is_variant               = g_variant
       TABLES
            t_outtab                 = xlist[].
ENDFORM.                    " LIST_VIEWER
*&---------------------------------------------------------------------*
*&      Form  MAINTAIN_MATERIAL_VIEW
*&---------------------------------------------------------------------*
*       Maintain Material View
*----------------------------------------------------------------------*
*      -->IV_MATNR  Material
*      -->IV_WERKS  Plant
*      -->IV_AUSWG  Screen Sequence
*      -->IV_TCODE  Transaction Code
*----------------------------------------------------------------------*
FORM maintain_material_view_old  USING    iv_matnr
                                          iv_werks
                                          iv_auswg
                                          iv_tcode.
  DATA: lv_bilds LIKE t133a-bilds,
        ls_t130m LIKE t130m,
        is_rmmg1 LIKE rmmg1,
        es_rmmg1 LIKE rmmg1,
        l_pstat  LIKE t130m-pstat,
        lwa_view TYPE mbildtab,
        lwa_auswg TYPE mgauswg,
        lf_updateok TYPE t130f-kzref,
        lt_views TYPE STANDARD TABLE OF mbildtab INITIAL SIZE 0,
        lt_auswg TYPE STANDARD TABLE OF mgauswg INITIAL SIZE 0.

  CLEAR : ls_t130m, lv_bilds, lt_views[], lt_views,
          is_rmmg1, es_rmmg1, lt_auswg, lt_auswg[].

  is_rmmg1-matnr = iv_matnr.
  is_rmmg1-werks = iv_werks.
  is_rmmg1-spras = sy-langu.

  CALL FUNCTION 'MATERIAL_READ_ALL'
       EXPORTING
            herkunft      = 'Z'
            aktyp         = 'A'
            wrmmg1        = is_rmmg1
            tcode         = 'MM03'
            kz_berprf     = 'X'
            kz_status_det = 'X'
            kzrfb         = 'X'
       IMPORTING
            aktvstatus    = l_pstat
            wrmmg1        = es_rmmg1
            tmara         = mara.


*  READ TABLE lt_views INTO lwa_view WITH KEY dytxt = p_dytxt.
*  CHECK sy-subrc EQ 0.
  lwa_auswg-auswg = iv_auswg.
  APPEND lwa_auswg TO lt_auswg.

  CALL FUNCTION 'MATERIAL_MAINTAIN_DIALOGUE'
      EXPORTING
          irmmg1             = es_rmmg1
*         IRMMG1_REF = ' '
          kz_ein_dark        = 'X'
          t_tcode            = iv_tcode
*         FLG_MATNR_RES = ' '
          p_pstat            = l_pstat
*         FLG_STAT_ALL = ' '
*         CALL_MODE2 = ' '
      IMPORTING
*         MATERIAL_NO =
         update_ok           = lf_updateok
      TABLES
          iauswg             = lt_auswg[]
      EXCEPTIONS
          no_authority = 1
          OTHERS       = 2.

  IF sy-subrc  EQ 0.
    IF lf_updateok EQ 'X'.
      COMMIT WORK.
    ENDIF.
  ENDIF.
ENDFORM.                    " MAINTAIN_MATERIAL_VIEW_OLD
*&---------------------------------------------------------------------*
*&      Form  MAINTAIN_MATERIAL_VIEW
*&---------------------------------------------------------------------*
*       Maintain Material View
*----------------------------------------------------------------------*
*      -->IV_MATNR  Material
*      -->IV_WERKS  Plant
*      -->IV_AUSWG  Screen Sequence
*      -->IV_TCODE  Transaction Code
*----------------------------------------------------------------------*
FORM maintain_material_view  USING    iv_matnr
                                      iv_werks
                                      iv_auswg
                                      iv_tcode.
  DATA lf_view(15).

  CASE iv_auswg.
    WHEN co_basic.
      lf_view = 'K'.
    WHEN co_sales OR co_export.
      lf_view = 'V'.
    WHEN co_purchasing OR co_import.
      lf_view = 'E'.
    WHEN co_mrp1 OR co_mrp2 OR co_mrp3 OR co_mrp4.
      lf_view = 'D'.
    WHEN co_quality.
      lf_view = 'Q'.
    WHEN co_accounting1 OR co_accounting2.
      lf_view = 'B'.
    WHEN co_costing1 OR co_costing2.
      lf_view = 'G'.
    WHEN co_storage  OR co_storage2.
      lf_view = 'L'.
    WHEN co_class.
      lf_view = 'C'.
    WHEN co_forecast.
      lf_view = 'P'.
    WHEN co_tools.
      lf_view = 'F'.
    WHEN co_work.
      lf_view = 'A'.
  ENDCASE.

  SET PARAMETER ID 'MXX' FIELD lf_view.
  SET PARAMETER ID 'MAT' FIELD iv_matnr.
  SET PARAMETER ID 'WRK' FIELD iv_werks.
  SET PARAMETER ID 'VZW' FIELD iv_werks.
*  get parameter id 'LAG' field lv_lgort.
*  set parameter id 'LAG' field l_lgort_leer.

  CASE iv_tcode.
    WHEN 'MM01'.
      AUTHORITY-CHECK OBJECT 'S_TCODE'
               ID 'TCD' FIELD 'MM01'.
      IF sy-subrc NE 0.
        MESSAGE e172(00) WITH 'MM01'.
      ENDIF.
      CALL TRANSACTION 'MM01' AND SKIP FIRST SCREEN.
    WHEN 'MM02'.
      AUTHORITY-CHECK OBJECT 'S_TCODE'
               ID 'TCD' FIELD 'MM02'.
      IF sy-subrc NE 0.
        MESSAGE e172(00) WITH 'MM02'.
      ENDIF.
      CALL TRANSACTION 'MM02' AND SKIP FIRST SCREEN.
    WHEN 'MM03'.
      AUTHORITY-CHECK OBJECT 'S_TCODE'
             ID 'TCD' FIELD 'MM03'.
      IF sy-subrc NE 0.
        MESSAGE e172(00) WITH 'MM03'.
      ENDIF.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDFORM.                    " MAINTAIN_MATERIAL_VIEW
*&---------------------------------------------------------------------*
*&      Form  CHECK_PACKING_INSTRUCTION
*&---------------------------------------------------------------------*
*       Check Packing Instruction.
*----------------------------------------------------------------------*
FORM check_packing_instruction.
  DATA: ls_pibasedata    LIKE pibasedata.
  DATA: lt_pinst         TYPE pdt_t_packobj_found,
        ls_pinst         TYPE pdt_s_packobj_found,
        ls_paitemtype    TYPE pdt_ra_paitemtype,
        lt_ra_paitemtype TYPE pdt_t_ra_paitemtype.
  CHECK xlist-piid IS INITIAL.
  CLEAR ls_paitemtype.
  ls_paitemtype-sign   = 'I'.
  ls_paitemtype-option = 'EQ'.
  ls_paitemtype-low    = 'R'.         "reference material
  APPEND ls_paitemtype TO lt_ra_paitemtype.
  IF xlist-rmatp EQ space.
    xlist-status_pi = co_status_yellow.
  ELSE.
* Find Packing Object
    CALL FUNCTION 'VHUPODB_PACKOBJ_FIND'
         EXPORTING
              packtyp_imp          = 'P'
              range_paitemtype_imp = lt_ra_paitemtype
              matnr_imp            = xlist-rmatp
              maxrecords_imp       = 1
         CHANGING
              packobj_tab          = lt_pinst
         EXCEPTIONS
              pos_without_head     = 1
              OTHERS               = 2.
    READ TABLE lt_pinst INTO ls_pinst INDEX 1.
    IF sy-subrc EQ 0.
*   Get Packing Instruction Information.
      CALL FUNCTION 'VHUPIAP_GET_PI_INFO'
           EXPORTING
                ip_packnr           = ls_pinst-packnr
           IMPORTING
                es_basedata         = ls_pibasedata
           EXCEPTIONS
                packinstr_not_found = 1
                packinstr_deleted   = 2
                OTHERS              = 3.
      xlist-piid      = ls_pibasedata-piid.
      xlist-loadcarr  = ls_pibasedata-loadcarr.
      xlist-trgqty    = ls_pibasedata-t_trgqty.
      xlist-status_pi = co_status_green.
    ELSE.
      xlist-status_pi = co_status_yellow.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_PACKING_INSTRUCTION
*&---------------------------------------------------------------------*
*&      Form  check_control_cycle
*&---------------------------------------------------------------------*
*       Check KANBAN Control Cycle
*----------------------------------------------------------------------*
FORM check_control_cycle.
*  DATA: ls_pibasedata    LIKE pibasedata.
*  CHECK xlist-mtart EQ 'PART'.
*  CASE xlist-tempb.
*    WHEN '3'.
*      READ TABLE xpkhd WITH KEY matnr = xlist-matnr
*                                werks = xlist-werks
*                                prvbe = xlist-vspvb.
*      IF sy-subrc EQ 0.
**--- Checking Kanban Source Storage Location
*        IF xlist-inslc IS INITIAL.
*          IF xpkhd-umlgo <> xlist-lgfsb.
*            xlist-status_kanban = co_status_red.
*          ELSE.
*            xlist-status_kanban = co_status_green.
*          ENDIF.
*        ELSE.
*          IF xpkhd-umlgo <> xlist-inslc.
*            xlist-status_kanban = co_status_red.
*          ELSE.
*            xlist-status_kanban = co_status_green.
*          ENDIF.
*        ENDIF.
*
*        xlist-pknum         = xpkhd-pknum.
*        xlist-zfeeder       = xpkhd-zfeeder.
*      ELSE.
*        xlist-status_kanban = co_status_red.
*      ENDIF.
*    WHEN '4'.
*      READ TABLE xpkhd WITH KEY matnr = xlist-matnr
*                                werks = xlist-werks.
*      IF sy-subrc EQ 0.
*        xlist-pknum         = xpkhd-pknum.
*        xlist-zfeeder       = xpkhd-zfeeder.
*
*        IF xpkhd-rksta EQ 'I'.
*          xlist-status_kanban = co_status_red.
*        ELSE.
**--- Checking Kanban Source Storage Location
*          IF xlist-inslc IS INITIAL.
*            IF xpkhd-umlgo <> xlist-lgfsb.
*              xlist-status_kanban = co_status_red.
*            ELSE.
*              xlist-status_kanban = co_status_green.
*            ENDIF.
*          ELSE.
*            IF xpkhd-umlgo <> xlist-inslc.
*              xlist-status_kanban = co_status_red.
*            ELSE.
*              xlist-status_kanban = co_status_green.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ELSE.
*        xlist-status_kanban = co_status_red.
*      ENDIF.
*    WHEN OTHERS.
*
*  ENDCASE.
*
**-- Control Cycle for JIT call
*  IF NOT xlist-ebeln IS INITIAL.
*    SELECT SINGLE kanba lgbzo
*      INTO (xlist-kanba,xlist-lgbzo)
*      FROM ekpo
*     WHERE ebeln = xlist-ebeln
*       AND ebelp = xlist-ebelp.
*
*    CASE xlist-kanba.
*      WHEN 'Z'. "Scheduling Agreement for JIT Call
*        READ TABLE xpkhd WITH KEY matnr = xlist-matnr
*                                  werks = xlist-werks
*                                  ebeln = xlist-ebeln
*                                  ebelp = xlist-ebelp
*                                  rksta = 'M'.
*        IF sy-subrc EQ 0.
*          xlist-status_jitcall     = co_status_green.
*        ELSE.
*          xlist-status_jitcall     = co_status_red.
*        ENDIF.
*      WHEN OTHERS.
*        xlist-status_jitcall     = co_status_space.
*    ENDCASE.
*  ENDIF.
*
**-Control Cycle for Replenishment.
*  IF xlist-inslc IS NOT INITIAL.
*    READ TABLE xpkhd WITH KEY matnr = xlist-matnr
*                              werks = xlist-werks
*                              prvbe = xlist-inslc
*                              rksta = 'A'.
*    IF sy-subrc EQ 0.
*      xlist-status_repl = co_status_green.
*    ELSE.
*      xlist-status_repl = co_status_red.
*    ENDIF.
*  ENDIF.
ENDFORM.                    " check_control_cycle
*&---------------------------------------------------------------------*
*&      Form  ENTER_STORAGE_LOCATIONS
*&---------------------------------------------------------------------*
*       Enter Storage Locations.
*----------------------------------------------------------------------*
*      -->IF_MATNR  Material.
*      -->IF_WERKS  Plant
*----------------------------------------------------------------------*
FORM enter_storage_locations  USING    if_matnr
                                       if_werks
                                       if_lgort
                                       if_chgmd.
  DATA: lf_tcode TYPE syst-tcode,
        lv_bilds LIKE t133a-bilds,
        ls_t130m LIKE t130m,
        ls_rmmg1 LIKE rmmg1,
        lwa_view TYPE mbildtab,
        lwa_auswg TYPE mgauswg,
        lf_updateok TYPE t130f-kzref,
        lt_views TYPE STANDARD TABLE OF mbildtab INITIAL SIZE 0,
        lt_auswg TYPE STANDARD TABLE OF mgauswg INITIAL SIZE 0.

  IF if_chgmd  = 'X'.
    PERFORM check_tcode_auth_nw USING 'MM02' CHANGING lf_tcode.
  ELSE.
    lf_tcode = 'MM03'.
  ENDIF.
  CLEAR : ls_t130m, lv_bilds, lt_views[], lt_views,
          ls_rmmg1, lt_auswg, lt_auswg[].

  SELECT SINGLE * FROM mara WHERE matnr EQ if_matnr.
  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'T130M_SINGLE_READ'
       EXPORTING
            tcode      = lf_tcode
            kzrfb      = 'X'
       IMPORTING
            wt130m     = ls_t130m
       EXCEPTIONS
            not_found  = 1
            wrong_call = 2
            OTHERS     = 3.

  CALL FUNCTION 'BILDSEQUENZ_IDENTIFY'
      EXPORTING
          branche     = mara-mbrsh
          materialart = mara-mtart
          tcode_ref   = ls_t130m-trref
*         KZRFB = ' '
      IMPORTING
          bildsequenz = lv_bilds
*         KZ_BILDS_CHANGED =
      EXCEPTIONS
          wrong_call = 1
          not_found  = 2
          OTHERS     = 3.

  CALL FUNCTION 'SELECTION_VIEWS_FIND'
       EXPORTING
            bildsequenz     = lv_bilds
            pflegestatus    = mara-pstat
       TABLES
            bildtab         = lt_views[]
       EXCEPTIONS
            call_wrong      = 1
            empty_selection = 2
            OTHERS          = 3.

  ls_rmmg1-matnr = mara-matnr.
  ls_rmmg1-werks = if_werks.
  ls_rmmg1-lgort = if_lgort.

  lwa_auswg-auswg = co_storage.
  APPEND lwa_auswg TO lt_auswg.

  CALL FUNCTION 'MATERIAL_MAINTAIN_DIALOGUE'
      EXPORTING
          irmmg1             = ls_rmmg1
*         IRMMG1_REF = ' '
          kz_ein_dark        = 'X'
          t_tcode            = lf_tcode
*         FLG_MATNR_RES = ' '
          p_pstat            = mara-pstat
*         FLG_STAT_ALL = ' '
*         CALL_MODE2 = ' '
      IMPORTING
*         MATERIAL_NO =
         update_ok           = lf_updateok
      TABLES
          iauswg             = lt_auswg[]
      EXCEPTIONS
          no_authority = 1
          OTHERS       = 2.

  IF sy-subrc  EQ 0.
    IF lf_updateok EQ 'X'.
      COMMIT WORK.
    ENDIF.
  ENDIF.
ENDFORM.                    " ENTER_STORAGE_LOCATIONS
*&---------------------------------------------------------------------*
*&      Form  FILL_CELL_COLOR
*&---------------------------------------------------------------------*
*       Fill Color Table.
*----------------------------------------------------------------------*
*      -->IF_FNAME   Field Name.
*      -->IF_COLOR   Color.
*      <--ET_COLOR   Color Table.
*----------------------------------------------------------------------*
FORM fill_cell_color  USING    if_fname TYPE lvc_fname
                               if_color
                      CHANGING et_color TYPE lvc_t_scol.
  DATA: ls_color    TYPE lvc_s_scol.
  ls_color-color-col = if_color.
  ls_color-color-int = 0.
  ls_color-color-inv = 0.
  ls_color-fname     = if_fname.
  INSERT ls_color INTO TABLE et_color.
ENDFORM.                    " FILL_CELL_COLOR
*&---------------------------------------------------------------------*
*&      Form  check_specific_data
*&---------------------------------------------------------------------*
*       Check KMMG Specific Data
*----------------------------------------------------------------------*
FORM check_specific_data.
  DATA: lf_matnr5    TYPE mara-matnr,
        lf_pgn       TYPE mara-ferth,
        lt_color     TYPE lvc_t_scol,
        l_cnt        TYPE i,
        lf_remainder TYPE marc-bstrf.

  CASE xlist-mtart.
    WHEN 'SEMI'.
*---- Procurement Type
      IF xlist-beskz NE 'E'.
        PERFORM fill_cell_color USING    'BESKZ'
                                         co_negative
                                CHANGING lt_color.
        xlist-status_mrp2 = co_status_red.
      ENDIF.
    WHEN 'RAW1'.
*---- Commodity Code.
      IF xlist-labor EQ 'K' AND
         xlist-stawn IS INITIAL.
        PERFORM fill_cell_color USING    'STAWN'
                                         co_negative
                                CHANGING lt_color.
      ENDIF.
    WHEN 'RAW2'.
*---- Commodity Code.
      IF xlist-labor EQ 'K' AND
         xlist-stawn IS INITIAL.
        PERFORM fill_cell_color USING    'STAWN'
                                         co_negative
                                CHANGING lt_color.
      ENDIF.
    WHEN 'PART' OR 'PGN'.
      IF xlist-maabc IS INITIAL.
        PERFORM fill_cell_color USING    'MAABC'
                                         co_negative
                                CHANGING lt_color.
      ENDIF.
*---- Weight
      IF xlist-mtart EQ 'PART' AND
         xlist-dispo <> 'R10'.
        IF xlist-brgew IS INITIAL OR
           xlist-ntgew IS INITIAL OR
           xlist-gewei IS INITIAL.
          xlist-status_basic = co_status_yellow.
        ENDIF.
      ENDIF.
*---- Source
      IF xlist-labor NA 'KV'.
        PERFORM fill_cell_color USING    'LABOR'
                                         co_negative
                                CHANGING lt_color.
        xlist-status_basic = co_status_red.
      ENDIF.
*---- Material Group.
      IF xlist-matkl IS INITIAL.
        PERFORM fill_cell_color USING    'MATKL'
                                         co_negative
                                CHANGING lt_color.
        xlist-status_basic = co_status_red.
      ENDIF.
*---- Purchasing Group.
      IF xlist-ekgrp IS INITIAL.
        PERFORM fill_cell_color USING    'EKGRP'
                                         co_negative
                                CHANGING lt_color.
      ENDIF.
*---- Planning Calendar in case of Lot Size 'PK'.
      IF xlist-disls EQ 'PK' AND
         xlist-mrppp IS INITIAL.
        PERFORM fill_cell_color USING    'MRPPP'
                                         co_negative
                                CHANGING lt_color.
      ENDIF.
*---- Rounding Value.
      IF xlist-labor EQ 'K'     AND
         xlist-bstrf IS INITIAL AND
         xlist-rdprf IS INITIAL.
        PERFORM fill_cell_color USING    'BSTRF'
                                         co_negative
                                CHANGING lt_color.
      ENDIF.
*---- Commodity Code.
      IF xlist-mtart EQ 'PART'  AND
         xlist-labor EQ 'K'     AND
         xlist-stawn IS INITIAL.
        PERFORM fill_cell_color USING    'STAWN'
                                         co_negative
                                CHANGING lt_color.
        xlist-status_import = co_status_red.
      ENDIF.
*---- Source vs. Valuation Class
      IF xlist-mtart EQ 'PART'.
        IF ( xlist-labor EQ 'K' AND
             xlist-bklas NE '3000' ) OR
           ( xlist-labor EQ 'V' AND
             xlist-bklas NE '3001' AND
             xlist-bklas NE '7900' ).
          PERFORM fill_cell_color USING    'LABOR'
                                           co_negative
                                  CHANGING lt_color.
          PERFORM fill_cell_color USING    'BKLAS'
                                           co_negative
                                  CHANGING lt_color.
          xlist-status_account = co_status_yellow.
        ENDIF.
        IF xlist-labor EQ 'V'  AND
           xlist-sobsl EQ '30' AND
           xlist-bklas NE '7900'.
          PERFORM fill_cell_color USING    'LABOR'
                                           co_negative
                                  CHANGING lt_color.
          PERFORM fill_cell_color USING    'BKLAS'
                                           co_negative
                                  CHANGING lt_color.
          xlist-status_account = co_status_red.
        ENDIF.
        IF xlist-sobsl NE '30' AND
           xlist-bklas EQ '7900'.
          PERFORM fill_cell_color USING    'BKLAS'
                                           co_negative
                                  CHANGING lt_color.
          xlist-status_account = co_status_red.
        ENDIF.
      ENDIF.
*---- Source vs. JIT indicator
      IF ( xlist-labor EQ 'K' AND
           xlist-fabkz EQ '1' ).
        PERFORM fill_cell_color USING    'LABOR'
                                         co_negative
                                CHANGING lt_color.
        PERFORM fill_cell_color USING    'FABKZ'
                                         co_negative
                                CHANGING lt_color.
        xlist-status_purchase = co_status_red.
      ENDIF.
*---- JIT indicator vs. Replenishment Strategy
*      IF xlist-fabkz EQ '1' AND
*         xlist-tempb EQ '1'.
*        PERFORM fill_cell_color USING    'FABKZ'
*                                         co_negative
*                                CHANGING lt_color.
*        PERFORM fill_cell_color USING    'TEMPB'
*                                         co_negative
*                                CHANGING lt_color.
*        xlist-status_purchase = co_status_red.
*      ENDIF.
*---- Prod.St.Loc vs. GR St.Loc
      IF xlist-lgpro IS INITIAL.
        IF xlist-beikz EQ 'C' OR
           xlist-beikz EQ 'F'.
        ELSE.
          PERFORM fill_cell_color USING    'LGPRO'
                                           co_negative
                                  CHANGING lt_color.
          xlist-status_mrp2 = co_status_red.
        ENDIF.
      ENDIF.
      IF xlist-lgfsb IS INITIAL.
        PERFORM fill_cell_color USING    'LGFSB'
                                         co_negative
                                CHANGING lt_color.
        xlist-status_mrp2 = co_status_red.
      ENDIF.
*---- Supply Area
      IF xlist-vspvb IS INITIAL.
        IF xlist-beikz EQ 'C' OR
           xlist-beikz EQ 'F'.
        ELSE.
          PERFORM fill_cell_color USING    'VSPVB'
                                           co_negative
                                  CHANGING lt_color.
        ENDIF.
      ENDIF.
*---- Packing Insturction Required
      IF xlist-tempb CA '349' OR
         xlist-disls EQ 'RO'.
        IF xlist-piid IS INITIAL.
          xlist-status_pi = co_status_red.
          PERFORM fill_cell_color USING    'PIID'
                                           co_negative
                                  CHANGING lt_color.
        ENDIF.
      ENDIF.
*---- Rounding Value vs. PI target Q'ty
      IF xlist-trgqty > 0 AND
         xlist-rdprf IS INITIAL.
        lf_remainder = xlist-bstrf MOD xlist-trgqty.
        IF lf_remainder <> 0.
          PERFORM fill_cell_color USING    'BSTRF'
                                           co_negative
                                  CHANGING lt_color.
          PERFORM fill_cell_color USING    'TRGQTY'
                                           co_negative
                                  CHANGING lt_color.
        ENDIF.
      ENDIF.
*---- Replenishment Strategy vs. Negative stock allowed
      IF xlist-tempb EQ '1'.
        IF xlist-xmcng EQ space.
          PERFORM fill_cell_color USING    'XMCNG'
                                           co_negative
                                  CHANGING lt_color.
          xlist-status_storage = co_status_red.
        ENDIF.
      ELSE.
        IF xlist-sobsl EQ '10'.
          IF xlist-xmcng EQ space.
            PERFORM fill_cell_color USING    'XMCNG'
                                             co_negative
                                    CHANGING lt_color.
            xlist-status_storage = co_status_red.
          ENDIF.
        ELSE.
          IF xlist-xmcng EQ 'X'.
            PERFORM fill_cell_color USING    'XMCNG'
                                             co_negative
                                    CHANGING lt_color.
            xlist-status_storage = co_status_red.
          ENDIF.
        ENDIF.
      ENDIF.
*---- Lot Size vs. KANBAN Indicator
      IF xlist-mtart EQ 'PART'.
        IF xlist-disls EQ 'RO' AND
           xlist-kanba <> 'Z'.
          xlist-status_jitcall     = co_status_red.
          PERFORM fill_cell_color USING    'KANBA'
                                           co_negative
                                  CHANGING lt_color.
        ELSEIF xlist-disls <> 'RO' AND
           xlist-kanba = 'Z'.
          xlist-status_jitcall     = co_status_red.
          PERFORM fill_cell_color USING    'DISLS'
                                           co_negative
                                  CHANGING lt_color.
        ENDIF.
      ENDIF.
      IF xlist-mtart EQ 'PART'.
*---- Special Procurement type vs. Item category in SA
        CASE xlist-sobsl.
          WHEN '30'.
            IF xlist-eortp EQ '0'.
              PERFORM fill_cell_color USING    'EORTP'
                                               co_negative
                                      CHANGING lt_color.
              xlist-status_mrp2 = co_status_red.
            ENDIF.
          WHEN '31'.
            IF xlist-eortp EQ '0'.
              PERFORM fill_cell_color USING    'EORTP'
                                               co_negative
                                      CHANGING lt_color.
              xlist-status_mrp2 = co_status_red.
            ENDIF.
          WHEN '10'.
            IF xlist-eortp EQ '0' OR
               xlist-eortp EQ '3'.
              PERFORM fill_cell_color USING    'EORTP'
                                               co_negative
                                      CHANGING lt_color.
              xlist-status_mrp2 = co_status_red.
            ENDIF.
          WHEN OTHERS.
            IF xlist-eortp EQ '3'.
              PERFORM fill_cell_color USING    'EORTP'
                                               co_negative
                                      CHANGING lt_color.
              xlist-status_mrp2 = co_status_red.
            ENDIF.
        ENDCASE.
      ENDIF.

*---- Check Duplicate PGN/PAC Code
      IF NOT xlist-ferth IS INITIAL AND
         NOT xlist-formt IS INITIAL.
        CLEAR : l_cnt.
        SELECT COUNT( * ) INTO l_cnt
               FROM mara
               WHERE ferth = xlist-ferth
                 AND formt = xlist-formt.
        IF l_cnt > 1.
          xlist-status_basic    = co_status_red.
          PERFORM fill_cell_color USING    'FORMT'
                                           co_negative
                                  CHANGING lt_color.
        ELSE.
          xlist-status_basic    = co_status_green.
        ENDIF.
      ELSE.
        xlist-status_basic    = co_status_green.
      ENDIF.
*---- Check missing PGN
      IF xlist-ferth IS INITIAL.
        CLEAR lf_pgn.
        CONCATENATE xlist-matnr(5) '%' INTO lf_matnr5.
        SELECT SINGLE ferth INTO lf_pgn
               FROM mara
               WHERE matnr LIKE lf_matnr5
                 AND mtart EQ 'PART'
                 AND ferth <> space.
        IF sy-subrc EQ 0.
          xlist-status_basic    = co_status_yellow.
          PERFORM fill_cell_color USING    'FERTH'
                                           co_negative
                                  CHANGING lt_color.
        ENDIF.
      ENDIF.

    WHEN 'FSC'.
*---- Commodity Code.
      IF xlist-stawn IS INITIAL.
        PERFORM fill_cell_color USING    'STAWN'
                                         co_negative
                                CHANGING lt_color.
        xlist-status_export = co_status_red.
      ENDIF.
  ENDCASE.
  READ TABLE lt_color INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
*    INSERT LINES OF lt_color INTO TABLE xlist-coltab.
  ENDIF.
ENDFORM.                    " check_specific_data
*&---------------------------------------------------------------------*
*&      Form  get_storage_bin
*&---------------------------------------------------------------------*
*       Get Storage Bin for GR/GI
*----------------------------------------------------------------------*
*      -->IF_MATNR  Material
*      -->IF_WERKS  Plant
*      -->IF_LGFSB  Storage Loc. for Receipt
*      -->IF_INSLC  Storage Loc. for Transit
*      -->IF_LGPRO  Storage Loc. for Issue
*      -->IF_TEMPB  Replenishment Strategy
*      <--EF_LGPBE  Storage Bin for Receipt
*      <--EF_UMLGPBE  Storage Bin for Transit
*      <--EF_VBRST  Point of Consumption
*----------------------------------------------------------------------*
FORM get_storage_bin  USING    if_matnr
                               if_werks
                               if_lgfsb
                               if_inslc
                               if_lgpro
                               if_tempb
                      CHANGING ef_lgpbe
                               ef_umlgpbe
                               ef_vbrst.
  DATA lf_lgort TYPE mard-lgort.
  IF if_lgfsb EQ 'C001'.
    IF if_tempb EQ '2'.
      lf_lgort = 'C004'.
    ELSE.
      IF if_lgpro EQ 'L501'.
        lf_lgort = 'P501'.
      ELSEIF if_lgpro EQ 'L201'.
        lf_lgort = 'P201'.
      ELSE.
        lf_lgort = 'C002'.
      ENDIF.
    ENDIF.
  ELSE.
    lf_lgort = if_lgfsb.
  ENDIF.
* Storage Bin for Receipt
  READ TABLE xmard WITH KEY matnr = if_matnr
                            werks = if_werks
                            lgort = lf_lgort.
  IF sy-subrc EQ 0.
    ef_lgpbe = xmard-lgpbe.
  ENDIF.
* Point of Consumption(Work Station)
  READ TABLE xmard WITH KEY matnr = if_matnr
                            werks = if_werks
                            lgort = if_inslc.
  IF sy-subrc EQ 0.
    ef_umlgpbe = xmard-lgpbe.
  ENDIF.

* Point of Consumption(Work Station)
  READ TABLE xmard WITH KEY matnr = if_matnr
                            werks = if_werks
                            lgort = if_lgpro.
  IF sy-subrc EQ 0.
    ef_vbrst = xmard-lgpbe.
  ENDIF.
ENDFORM.                    " get_storage_bin
*&---------------------------------------------------------------------*
*&      Form  set_default_value
*&---------------------------------------------------------------------*
*       Set Default Value
*----------------------------------------------------------------------*
FORM set_default_value .
  REFRESH: so_mstae, so_aeszn.
*  so_mstae-sign   = 'E'.
*  so_mstae-option = 'EQ'.
*  so_mstae-low    = '13'.
*  APPEND so_mstae.
*  so_mstae-sign   = 'E'.
*  so_mstae-option = 'EQ'.
*  so_mstae-low    = '14'.
*  APPEND so_mstae.

*  so_aeszn-sign   = 'I'.
*  so_aeszn-option = 'CP'.
*  so_aeszn-low    = '*2'.
*  APPEND so_aeszn.
*
*  so_aeszn-sign   = 'I'.
*  so_aeszn-option = 'EQ'.
*  so_aeszn-low    = '11'.
*  APPEND so_aeszn.
*
*  so_aeszn-sign   = 'I'.
*  so_aeszn-option = 'EQ'.
*  so_aeszn-low    = '13'.
*  APPEND so_aeszn.
*
*  so_aeszn-sign   = 'I'.
*  so_aeszn-option = 'EQ'.
*  so_aeszn-low    = '14'.
*  APPEND so_aeszn.

ENDFORM.                    " set_default_value
*&---------------------------------------------------------------------*
*&      Form  maintain_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM maintain_display  USING  change_m
                              iv_matnr
                              iv_werks
                              iv_auswg
                              iv_tcode_m
                              iv_tcode_d.
  DATA:tcod LIKE sy-tcode,
       l_tcode LIKE sy-tcode VALUE 'ME12'.

  IF change_m  = 'X'.
    PERFORM check_tcode_auth_nw USING iv_tcode_m CHANGING tcod.
    CASE tcod.
      WHEN 'MM01' OR 'MM02' OR 'MM03'.
        IF tcod = 'MM02'.
          CLEAR first_act.
        ENDIF.
        PERFORM maintain_material_view USING iv_matnr
                                             iv_werks
                                             iv_auswg
                                             tcod.
      WHEN 'ME11' OR 'ME12' OR 'ME13'.
        SET PARAMETER ID 'MAT' FIELD iv_matnr.
        SET PARAMETER ID 'WRK' FIELD space.
        IF iv_tcode_m = 'ME13'.
          CALL TRANSACTION tcod AND SKIP FIRST SCREEN.
        ELSE.
          CALL TRANSACTION tcod.
        ENDIF.
      WHEN 'ME01' OR 'ME03'.
        SET PARAMETER ID 'MAT' FIELD iv_matnr.
        SET PARAMETER ID 'WRK' FIELD iv_werks.
        CALL TRANSACTION tcod AND SKIP FIRST SCREEN.
      WHEN 'ME1M'.
        SUBMIT rm06im00 WITH if_matnr EQ iv_matnr AND RETURN.
      WHEN 'ME31L'.
        CALL TRANSACTION 'ME31L'.
      WHEN 'POP1'.
        CALL TRANSACTION 'POP1'.
    ENDCASE.
  ELSE.
    CASE iv_tcode_d.
      WHEN 'MM03'.
        PERFORM maintain_material_view USING iv_matnr
                                             iv_werks
                                             iv_auswg
                                             iv_tcode_d.
      WHEN 'ME03'.
        SET PARAMETER ID 'MAT' FIELD iv_matnr.
        SET PARAMETER ID 'WRK' FIELD iv_werks.
        CALL TRANSACTION iv_tcode_d AND SKIP FIRST SCREEN.
      WHEN 'ME1M'.
        SUBMIT rm06im00 WITH if_matnr EQ iv_matnr AND RETURN.
    ENDCASE.
  ENDIF.
ENDFORM.                    " maintain_display
*&---------------------------------------------------------------------*
*&      Form  check_tcode_auth_nw
*&---------------------------------------------------------------------*
FORM check_tcode_auth_nw USING t_code CHANGING tc.
* Authority Check
  tc = t_code.
  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
       EXPORTING
            tcode  = t_code
       EXCEPTIONS
            ok     = 0
            not_ok = 1
            OTHERS = 2.
  IF sy-subrc NE 0.
    MESSAGE s172(00) WITH t_code." RAISING no_authority.
    CASE t_code.
      WHEN 'MM01' OR 'MM02' OR 'MMSC' OR 'ME03'.
        tc = 'MM03'.
      WHEN 'ME01' OR 'ME1M'.
        tc = 'ME03'.
      WHEN 'POP1' OR 'POP2'.
        tc = 'POP3'.
    ENDCASE.
  ENDIF.
ENDFORM.                    " check_tcode_auth_nw
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bdc_dynpro  USING program dynpro. "New dynpro
  CLEAR bdcdata.
  bdcdata-program = program.
  bdcdata-dynpro = dynpro.
  bdcdata-dynbegin = 'x'.
  APPEND bdcdata.
ENDFORM.                    " BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM bdc_field  USING  fnam fval. "New Field
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    " BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  READ_MAIN_DATA
*&---------------------------------------------------------------------*
FORM read_main_data .
  PERFORM read_material_types.
  PERFORM read_plant_data.
  PERFORM read_default_values_for_im.
  PERFORM read_material_master.
  PERFORM read_storage_data.
  PERFORM read_info_records.
  PERFORM read_source_list.
  PERFORM read_control_cycles.
  PERFORM read_bom.
  PERFORM check_material_status.
  PERFORM filter_material_status.
ENDFORM.                    " READ_MAIN_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_PRODUCTION_VERSION
*&---------------------------------------------------------------------*
*       Get Production Version
*----------------------------------------------------------------------*
*      -->IF_MATNR  Material
*      -->IF_WERKS  Plant
*      -->IF_DATUV  Valid Date
*      <--EF_VERID  Production Version
*----------------------------------------------------------------------*
FORM get_production_version  USING    if_matnr
                                      if_werks
                                      if_datuv
                             CHANGING ef_verid.
  CLEAR ef_verid.
  SELECT SINGLE verid
    INTO ef_verid
    FROM mkal
   WHERE matnr EQ if_matnr
     AND werks EQ if_werks
     AND adatu <= if_datuv
     AND bdatu >= if_datuv.
ENDFORM.                    " GET_PRODUCTION_VERSION
*&---------------------------------------------------------------------*
*&      Module  STATUS_0050  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0050 OUTPUT.
  SET PF-STATUS '0050'.
  SET TITLEBAR '0050'.

ENDMODULE.                 " STATUS_0050  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit_command  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command2 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " exit_command2  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0050  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0050 INPUT.

  CASE sy-ucomm.
    WHEN 'OKAY'.
      g_rc = 'X'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      g_rc = ' '.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0050  INPUT
*&---------------------------------------------------------------------*
*&      Form  COPY_AND_PASTE_MATERIAL
*&---------------------------------------------------------------------*
FORM copy_and_paste_material USING p_chk .
  DATA : BEGIN OF lt_matnr OCCURS 0, " WITH HEADER LINE,
        matnr TYPE matnr,
        mtart TYPE mtart,
         END OF lt_matnr.
  data: lt_return like bapiret2 occurs 0 with header line.
  DATA : l_chk.
  DATA : lf_mline TYPE sy-index,
         lf_first,
         lf_pgn   TYPE mara-ferth,
         lf_group TYPE mara-matnr.
  DATA : lf_text(400),
         lf_answer.
  CLEAR : lt_matnr, lt_matnr[],
          g_matnr,  l_chk,
          lt_return, lt_return[],
          gt_bdcmsg, gt_bdcmsg[].

  READ TABLE xlist WITH KEY mark = 'X'.
  IF sy-subrc NE 0.
    MESSAGE s074(0d).
    EXIT.
  ELSE.
*-- Check if the same PGN.
    LOOP AT xlist WHERE mark = 'X'.
      IF lf_first IS INITIAL.
        lf_pgn  = xlist-normt.
        lf_first = 'X'.
      ELSE.
        IF lf_pgn <> xlist-normt.
          lf_text = text-pgn.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
               EXPORTING
                    titlebar              = text-tit
                    text_question         = lf_text
                    default_button        = '2'
                    display_cancel_button = space
               IMPORTING
                    answer                = lf_answer
               EXCEPTIONS
                    text_not_found        = 1
                    OTHERS                = 2.
        ENDIF.
      ENDIF.
    ENDLOOP.
    CHECK lf_answer <> '2'.
    CLEAR lf_answer.
    g_matnr = lf_pgn.
    CALL SCREEN 0050 STARTING AT 10 10.
    CHECK NOT g_rc IS INITIAL.
    LOOP AT xlist WHERE mark = 'X'.
      MOVE-CORRESPONDING xlist TO lt_matnr.
      APPEND lt_matnr. CLEAR lt_matnr.
      IF lf_pgn IS INITIAL.
        IF g_matnr(5) NE xlist-matnr(5).
          l_chk = 'X'.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF l_chk = 'X'.
      p_chk = ' '.
      lf_text = text-ref.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
           EXPORTING
                titlebar              = text-tit
                text_question         = lf_text
                default_button        = '2'
                display_cancel_button = space
           IMPORTING
                answer                = lf_answer
           EXCEPTIONS
                text_not_found        = 1
                OTHERS                = 2.
      CHECK lf_answer <> '2'.
    ELSE.
      p_chk = ' '.
    ENDIF.

    CALL FUNCTION 'Z_MM_MATERIAL_MASTER_COPY'
         EXPORTING
              c_matnr  = g_matnr
         TABLES
              t_matnr  = lt_matnr
              t_return = lt_return.


    DATA:
          ls_header TYPE bapimathead,
          ls_mard   TYPE bapi_mard,
          ls_mardx  TYPE bapi_mardx,
          ls_return TYPE bapiret2,
          lt_mard   LIKE mard OCCURS 0 WITH HEADER LINE.


    CLEAR : lt_mard, lt_mard[].
    SELECT *
          INTO CORRESPONDING FIELDS OF TABLE lt_mard
          FROM mard
          WHERE matnr = g_matnr
            AND lgort <> 'C002'
            AND werks = so_werks-low.

    LOOP AT lt_matnr.
      ls_header-material     = lt_matnr-matnr.
      ls_header-ind_sector   = 'A'.
      ls_header-matl_type    = lt_matnr-mtart.
      ls_header-storage_view = 'X'.

      LOOP AT lt_mard.
        ls_mard-plant     = lt_mard-werks.
        ls_mard-stge_loc  = lt_mard-lgort.
        ls_mard-stge_bin  = lt_mard-lgpbe.
        ls_mardx-plant    = lt_mard-werks.
        ls_mardx-stge_loc = lt_mard-lgort.
        ls_mardx-stge_bin = 'X'.
        IF NOT lt_mard-lgort IS INITIAL.
          CLEAR ls_return.
          CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
               EXPORTING
                    headdata             = ls_header
                    storagelocationdata  = ls_mard
                    storagelocationdatax = ls_mardx
               IMPORTING
                    return               = ls_return.
          APPEND ls_return TO lt_return.
        ENDIF.
      ENDLOOP.
    ENDLOOP.


    CALL FUNCTION 'MESSAGES_INITIALIZE'.
    LOOP AT lt_return.
      ADD 1 TO lf_mline.
      CALL FUNCTION 'MESSAGE_STORE'
           EXPORTING
                arbgb                  = lt_return-id
                msgty                  = lt_return-type
                msgv1                  = lt_return-message_v1
                msgv2                  = lt_return-message_v2
                msgv3                  = lt_return-message_v3
                msgv4                  = lt_return-message_v4
                txtnr                  = lt_return-number
                zeile                  = lf_mline
           EXCEPTIONS
                message_type_not_valid = 1
                not_active             = 2.
    ENDLOOP.

    IF NOT lt_return[] IS INITIAL.
      PERFORM show_messages.
    ENDIF.
  ENDIF.
ENDFORM.                    " COPY_AND_PASTE_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  SHOW_MESSAGES
*&---------------------------------------------------------------------*
*       Show Messages.
*----------------------------------------------------------------------*
FORM show_messages.
  CALL FUNCTION 'MESSAGES_SHOW'
       EXPORTING
            object             = text-log
            show_linno         = space
       EXCEPTIONS
            inconsistent_range = 1
            no_messages        = 2.
  IF sy-subrc NE 0.
  ENDIF.
ENDFORM.                    " SHOW_MESSAGES
*&---------------------------------------------------------------------*
*&      Form  EXTEND_STORAGELOCATION
*&---------------------------------------------------------------------*
*       Extend Storage Location
*----------------------------------------------------------------------*
FORM extend_storagelocation .
  DATA: lf_mline  TYPE sy-tabix,
        lf_lgort  TYPE mard-lgort,
        ls_header TYPE bapimathead,
        ls_mard   TYPE bapi_mard,
        ls_mardx  TYPE bapi_mardx,
        ls_return TYPE bapiret2,
        lt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.
  LOOP AT xlist WHERE mark = 'X'.
    ls_header-material     = xlist-matnr.
    ls_header-ind_sector   = 'A'.
    ls_header-matl_type    = xlist-mtart.
    ls_header-storage_view = 'X'.
    IF xlist-lgpro <> space. "IS NOT INITIAL.
      SELECT SINGLE lgort
        INTO lf_lgort
        FROM mard
       WHERE matnr EQ xlist-matnr
         AND werks EQ xlist-werks
         AND lgort EQ xlist-lgpro.
      IF sy-subrc <> 0.
        ls_mard-plant     = xlist-werks.
        ls_mard-stge_loc  = xlist-lgpro.
        ls_mardx-plant    = xlist-werks.
        ls_mardx-stge_loc = xlist-lgpro.
        CLEAR ls_return.
        CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
             EXPORTING
                  headdata             = ls_header
                  storagelocationdata  = ls_mard
                  storagelocationdatax = ls_mardx
             IMPORTING
                  return               = ls_return.
        APPEND ls_return TO lt_return.
      ENDIF.
    ENDIF.
*-- Transit Storage Location
    IF xlist-inslc <> space. "IS NOT INITIAL.
      SELECT SINGLE lgort
        INTO lf_lgort
        FROM mard
       WHERE matnr EQ xlist-matnr
         AND werks EQ xlist-werks
         AND lgort EQ xlist-inslc.
      IF sy-subrc <> 0.
        ls_mard-plant     = xlist-werks.
        ls_mard-stge_loc  = xlist-inslc.
        ls_mardx-plant    = xlist-werks.
        ls_mardx-stge_loc = xlist-inslc.
        CLEAR ls_return.
        CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
             EXPORTING
                  headdata             = ls_header
                  storagelocationdata  = ls_mard
                  storagelocationdatax = ls_mardx
             IMPORTING
                  return               = ls_return.
        APPEND ls_return TO lt_return.
      ENDIF.
    ENDIF.

    IF xlist-lgfsb <> space. "IS NOT INITIAL.
      SELECT SINGLE lgort
        INTO lf_lgort
        FROM mard
       WHERE matnr EQ xlist-matnr
         AND werks EQ xlist-werks
         AND lgort EQ xlist-lgfsb.
      IF sy-subrc <> 0.
        ls_mard-plant     = xlist-werks.
        ls_mard-stge_loc  = xlist-lgfsb.
        ls_mardx-plant    = xlist-werks.
        ls_mardx-stge_loc = xlist-lgfsb.
        CLEAR ls_return.
        CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
             EXPORTING
                  headdata             = ls_header
                  storagelocationdata  = ls_mard
                  storagelocationdatax = ls_mardx
             IMPORTING
                  return               = ls_return.
        APPEND ls_return TO lt_return.
      ENDIF.
    ENDIF.


  ENDLOOP.



  CALL FUNCTION 'MESSAGES_INITIALIZE'.
  LOOP AT lt_return.
    ADD 1 TO lf_mline.
    CALL FUNCTION 'MESSAGE_STORE'
         EXPORTING
              arbgb                  = lt_return-id
              msgty                  = lt_return-type
              msgv1                  = lt_return-message_v1
              msgv2                  = lt_return-message_v2
              msgv3                  = lt_return-message_v3
              msgv4                  = lt_return-message_v4
              txtnr                  = lt_return-number
              zeile                  = lf_mline
         EXCEPTIONS
              message_type_not_valid = 1
              not_active             = 2.
  ENDLOOP.

  IF NOT lt_return[] IS INITIAL.
    PERFORM show_messages.
  ENDIF.

ENDFORM.                    " EXTEND_STORAGELOCATION
*&---------------------------------------------------------------------*
*&      Form  check_released_costings
*&---------------------------------------------------------------------*
*       Check Released Cost Estimation
*----------------------------------------------------------------------*
FORM check_released_costings.
  IF xlist-kalkz EQ 'X' OR
     xlist-kalkl EQ 'X' OR
     xlist-kalkv EQ 'X'.
    xlist-freig = 'X'.
  ELSE.
    xlist-freig = space.
  ENDIF.
ENDFORM.                    " check_released_costings
*&---------------------------------------------------------------------*
*&      Form  READ_INFO_RECORDS
*&---------------------------------------------------------------------*
*       Read Info.Records
*----------------------------------------------------------------------*
FORM read_info_records .
  REFRESH xinfo.
  SELECT a~matnr e~esokz a~infnr a~lifnr
    INTO CORRESPONDING FIELDS OF TABLE xinfo
    FROM eina AS a INNER JOIN eine AS e
                      ON a~infnr = e~infnr
   WHERE a~matnr IN so_matnr
     AND a~loekz EQ space
     AND e~loekz EQ space
     AND exists ( SELECT *
                    FROM mara AS m INNER JOIN marc AS c
                                      ON m~matnr = c~matnr
                   WHERE m~matnr EQ a~matnr
                     AND c~werks IN so_werks
                     AND c~matnr IN so_matnr
                     AND m~mtart IN so_mtart
                     AND m~labor IN so_labor
                     AND m~matkl IN so_matkl
                     AND m~ersda IN so_ersda
                     AND m~laeda IN so_laeda
                     AND c~kzkri IN so_kzkri
                     AND c~beskz IN so_beskz
                     AND c~sobsl IN so_sobsl
                     AND c~lvorm IN so_lvorm
                     AND c~fabkz IN so_fabkz
                     AND c~dispo IN so_dispo
                     AND c~ekgrp IN so_ekgrp
                     AND m~tempb IN so_tempb
                     AND m~mstae IN so_mstae ).
ENDFORM.                    " READ_INFO_RECORDS
*&---------------------------------------------------------------------*
*&      Form  READ_SOURCE_LIST
*&---------------------------------------------------------------------*
*       Read Source List
*----------------------------------------------------------------------*
FORM read_source_list .
  REFRESH xeord.
  SELECT *
    INTO TABLE xeord
    FROM eord AS e
   WHERE e~matnr IN so_matnr
     AND e~werks IN so_werks
     AND e~vdatu <= sy-datum
     AND e~bdatu >= sy-datum
     AND e~autet IN ('1','2')
     AND exists ( SELECT *
                    FROM mara AS m INNER JOIN marc AS c
                                      ON m~matnr = c~matnr
                   WHERE m~matnr EQ e~matnr
                     AND c~werks EQ e~werks
                     AND c~werks IN so_werks
                     AND c~matnr IN so_matnr
                     AND m~mtart IN so_mtart
                     AND m~labor IN so_labor
                     AND m~matkl IN so_matkl
                     AND m~ersda IN so_ersda
                     AND m~laeda IN so_laeda
                     AND c~kzkri IN so_kzkri
                     AND c~beskz IN so_beskz
                     AND c~sobsl IN so_sobsl
                     AND c~lvorm IN so_lvorm
                     AND c~fabkz IN so_fabkz
                     AND c~dispo IN so_dispo
                     AND c~ekgrp IN so_ekgrp
                     AND m~tempb IN so_tempb
                     AND m~mstae IN so_mstae ).
ENDFORM.                    " READ_SOURCE_LIST
*&---------------------------------------------------------------------*
*&      Form  READ_CONTROL_CYCLES
*&---------------------------------------------------------------------*
*       Read Control Cycles.
*----------------------------------------------------------------------*
FORM read_control_cycles.
  REFRESH xpkhd.
  SELECT *
    INTO TABLE xpkhd
    FROM pkhd AS p
   WHERE p~werks IN so_werks
     AND p~matnr IN so_matnr
     AND exists ( SELECT *
                    FROM mara AS m INNER JOIN marc AS c
                                      ON m~matnr = c~matnr
                   WHERE m~matnr EQ p~matnr
                     AND c~werks EQ p~werks
                     AND c~werks IN so_werks
                     AND c~matnr IN so_matnr
                     AND m~mtart IN so_mtart
                     AND m~labor IN so_labor
                     AND m~matkl IN so_matkl
                     AND m~ersda IN so_ersda
                     AND m~laeda IN so_laeda
                     AND c~kzkri IN so_kzkri
                     AND c~beskz IN so_beskz
                     AND c~sobsl IN so_sobsl
                     AND c~lvorm IN so_lvorm
                     AND c~fabkz IN so_fabkz
                     AND c~dispo IN so_dispo
                     AND c~ekgrp IN so_ekgrp
                     AND m~tempb IN so_tempb
                     AND m~mstae IN so_mstae ).
ENDFORM.                    " READ_CONTROL_CYCLES
*&---------------------------------------------------------------------*
*&      Form  READ_VENDOR_NAME
*&---------------------------------------------------------------------*
*       Read Vendor Name
*----------------------------------------------------------------------*
*      -->IF_LIFNR  Vendor
*      <--EF_NAME1  Name
*----------------------------------------------------------------------*
FORM read_vendor_name  USING    if_lifnr
                       CHANGING ef_name1.
  CLEAR ef_name1.
  READ TABLE xlfa1 WITH KEY lifnr = if_lifnr.
  IF sy-subrc EQ 0.
    ef_name1 = xlfa1-name1.
  ELSE.
    SELECT SINGLE *
      FROM lfa1
     WHERE lifnr EQ if_lifnr.
    IF sy-subrc EQ 0.
      APPEND lfa1 TO xlfa1.
      ef_name1 = lfa1-name1.
    ENDIF.
  ENDIF.
ENDFORM.                    " READ_VENDOR_NAME
*&---------------------------------------------------------------------*
*&      Form  READ_STORAGE_DATA
*&---------------------------------------------------------------------*
*       Read Storage Data
*----------------------------------------------------------------------*
FORM read_storage_data .
  REFRESH xmard.
  SELECT *
    INTO TABLE xmard
    FROM mard AS d
   WHERE d~werks IN so_werks
     AND d~matnr IN so_matnr
     AND exists ( SELECT *
                    FROM mara AS m INNER JOIN marc AS c
                                      ON m~matnr = c~matnr
                   WHERE m~matnr EQ d~matnr
                     AND c~werks EQ d~werks
                     AND c~werks IN so_werks
                     AND c~matnr IN so_matnr
                     AND m~mtart IN so_mtart
                     AND m~labor IN so_labor
                     AND m~matkl IN so_matkl
                     AND m~ersda IN so_ersda
                     AND m~laeda IN so_laeda
                     AND c~kzkri IN so_kzkri
                     AND c~beskz IN so_beskz
                     AND c~sobsl IN so_sobsl
                     AND c~lvorm IN so_lvorm
                     AND c~fabkz IN so_fabkz
                     AND c~dispo IN so_dispo
                     AND c~ekgrp IN so_ekgrp
                     AND m~tempb IN so_tempb
                     AND m~mstae IN so_mstae ).
ENDFORM.                    " READ_STORAGE_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_BOM
*&---------------------------------------------------------------------*
*       Read BOM Header
*----------------------------------------------------------------------*
FORM read_bom.
  REFRESH xmast.
  CHECK pa_bom EQ 'X'.
  SELECT *
    INTO TABLE xmast
    FROM mast AS b
   WHERE b~werks IN so_werks
     AND b~matnr IN so_matnr
     AND b~stlan IN ('1','M')
     AND exists ( SELECT *
                    FROM mara AS m INNER JOIN marc AS c
                                      ON m~matnr = c~matnr
                   WHERE m~matnr EQ b~matnr
                     AND c~werks EQ b~werks
                     AND c~werks IN so_werks
                     AND c~matnr IN so_matnr
                     AND m~mtart IN so_mtart
                     AND m~labor IN so_labor
                     AND m~matkl IN so_matkl
                     AND m~ersda IN so_ersda
                     AND m~laeda IN so_laeda
                     AND c~kzkri IN so_kzkri
                     AND c~beskz IN so_beskz
                     AND c~sobsl IN so_sobsl
                     AND c~lvorm IN so_lvorm
                     AND c~fabkz IN so_fabkz
                     AND c~dispo IN so_dispo
                     AND c~ekgrp IN so_ekgrp
                     AND m~tempb IN so_tempb
                     AND m~mstae IN so_mstae ).
ENDFORM.                    " READ_BOM
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM modify_screen .

  SELECT SINGLE lifnr
         INTO so_lifnr-low
         FROM lfa1
        WHERE lifnr = sy-uname.
  IF sy-subrc EQ 0.
    LOOP AT SCREEN .
      IF screen-group1 EQ 'NOI' .
        screen-input        = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  read_vendor_of_userid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_vendor_of_userid .

  SELECT vendor
  INTO so_lifnr-low
  FROM isautosicvendtab
  WHERE sic_user = sy-uname.

    so_lifnr-sign   = 'I'.
    so_lifnr-option = 'EQ'.
    APPEND so_lifnr. CLEAR so_lifnr.
  ENDSELECT.

ENDFORM.                    " read_vendor_of_userid
*&---------------------------------------------------------------------*
*&      Form  GET_RELATED_MATERIAL
*&---------------------------------------------------------------------*
FORM get_related_material USING p_ferth
                       CHANGING p_lifnr.

  SELECT SINGLE e~lifnr
    INTO p_lifnr
    FROM eord AS e INNER JOIN mara AS a
                 ON e~matnr = a~matnr
   WHERE e~werks IN so_werks
     AND e~vdatu <= sy-datum
     AND e~bdatu >= sy-datum
     AND e~autet IN ('1','2')
     AND a~ferth = p_ferth
     AND a~mtart = 'PART'
     AND a~mstae IN so_mstae.


ENDFORM.                    " GET_RELATED_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FERTH  PGN No
*      <--P_PGNTX  PGN text
*----------------------------------------------------------------------*
FORM read_text  USING    p_ferth
                CHANGING p_pgntx.

  SELECT SINGLE maktx
         INTO p_pgntx
         FROM makt
         WHERE spras = sy-langu
           AND matnr = p_ferth.

ENDFORM.                    " READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  CHECK_VENDOR_USER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_RC  text
*----------------------------------------------------------------------*
FORM check_vendor_user  USING    p_rc.

  SELECT SINGLE *
         FROM lfa1
         WHERE lifnr = sy-uname.
  IF sy-subrc EQ 0.
    p_rc = 'X'.
  ELSE.
    p_rc = ' '.
  ENDIF.

ENDFORM.                    " CHECK_VENDOR_USER
*&---------------------------------------------------------------------*
*&      Form  CHANGE_SCHED_AGREEMENT
*&---------------------------------------------------------------------*
*       Change Scheduling Agreement
*----------------------------------------------------------------------*
FORM change_sched_agreement.
*  DATA:
*    ls_seldata    TYPE mass_wa_tabdata,
*    lt_seldata    TYPE mass_tabdata,
*    ls_keyfield   TYPE ddobjname,
*    ls_fieldname  TYPE ddobjname,
*    lt_keyfields  TYPE mass_fieldtab,
*    lt_fieldnames TYPE mass_fieldtab,
*    ls_tabname    TYPE masstablestab,
*    lt_massmsg    TYPE massmsg OCCURS 0,
*    ls_massmsg    TYPE massmsg,
*    ls_ekpochange TYPE massekposchagree,
*    lt_ekpochange TYPE massekposchagree OCCURS 0.
*  DATA: lf_mline TYPE sy-index.
** Table Name
*  ls_tabname-name    = 'MASSEKPOSCHAGREE'.
*  ls_tabname-name_db = 'MASSEKPOSCHAGREE'.
*  ls_seldata-tabname = ls_tabname.
** Key fields
*  ls_keyfield = 'EBELN'.
*  APPEND ls_keyfield TO lt_keyfields.
*  ls_keyfield = 'EBELP'.
*  APPEND ls_keyfield TO lt_keyfields.
*  ls_seldata-keyfieldnames[] = lt_keyfields[].
** Field Name
*  ls_fieldname = 'LGORT'.
*  APPEND ls_fieldname TO lt_fieldnames.
*  ls_fieldname = 'LGBZO'.
*  APPEND ls_fieldname TO lt_fieldnames.
*  ls_fieldname = 'KANBA'.
*  APPEND ls_fieldname TO lt_fieldnames.
*  ls_seldata-fieldnames[] = lt_fieldnames[].
*
*  APPEND ls_seldata TO lt_seldata.
*
*
*  LOOP AT xlist WHERE mark = 'X'.
*    SELECT SINGLE *
*      INTO CORRESPONDING FIELDS OF ls_ekpochange
*      FROM ekpo
*     WHERE ebeln = xlist-ebeln
*       AND ebelp = xlist-ebelp.
*    IF sy-subrc EQ 0.
*      ls_ekpochange-lgort = xlist-lgfsb.
*      ls_ekpochange-lgbzo = xlist-loggr.
*      IF xlist-disls EQ 'RO'.
*        ls_ekpochange-kanba = 'Z'.
*      ELSE.
*        ls_ekpochange-kanba = space.
*      ENDIF.
*      APPEND ls_ekpochange TO lt_ekpochange.
*    ENDIF.
*  ENDLOOP.
*  CALL FUNCTION 'MASS_CHANGE_SCHED_AGREEMENT'
*       EXPORTING
*            seldata           = lt_seldata
*            testmode          = space
*       IMPORTING
*            msg               = lt_massmsg
*       TABLES
*            smassekposchagree = lt_ekpochange.
*  CALL FUNCTION 'MESSAGES_INITIALIZE'.
*  LOOP AT lt_massmsg INTO ls_massmsg.
*    ADD 1 TO lf_mline.
*    CALL FUNCTION 'MESSAGE_STORE'
*         EXPORTING
*              arbgb                  = ls_massmsg-msgid
*              msgty                  = ls_massmsg-msgty
*              msgv1                  = ls_massmsg-msgv1
*              msgv2                  = ls_massmsg-msgv2
*              msgv3                  = ls_massmsg-msgv3
*              msgv4                  = ls_massmsg-msgv4
*              txtnr                  = ls_massmsg-msgno
*              zeile                  = lf_mline
*         EXCEPTIONS
*              message_type_not_valid = 1
*              not_active             = 2.
*  ENDLOOP.
*  IF NOT lt_massmsg[] IS INITIAL.
*    PERFORM show_messages.
*  ENDIF.
ENDFORM.                    " CHANGE_SCHED_AGREEMENT

*&---------------------------------------------------------------------*
*&      Form  popup_get_runningno
*&---------------------------------------------------------------------*
*       popup_get_runningno
*----------------------------------------------------------------------*
FORM popup_get_runningno USING if_field
                               if_matnr
                               if_werks
                               if_zeinr.
*  DATA: lf_title TYPE sy-title,
*        lf_material(40),
*        lf_count TYPE sy-tabix,
*        lf_hours TYPE numc2.
*  REFRESH xpopseq.
*  CASE if_field.
*    WHEN 'ALL'.
*      SELECT * INTO CORRESPONDING FIELDS OF TABLE xpopseq
*        FROM ztbm_hkon
*        WHERE rno = if_zeinr.
*  ENDCASE.
*
*  DESCRIBE TABLE xpopseq LINES lf_count.
*  CHECK lf_count > 0.
*
*  SORT xpopseq DESCENDING BY wkdt rno.
*  DELETE ADJACENT DUPLICATES FROM xpopseq
*                  COMPARING rno.
*  PERFORM make_popup_field_cat TABLES gt_popupfc
*                               USING  if_field.
*  IF if_matnr IS INITIAL.
*    lf_title = text-pop.
*  ELSE.
*    WRITE if_matnr TO lf_material USING EDIT MASK '==MATN2'.
*    CONCATENATE text-pop 'for' lf_material
*           INTO lf_title
*      SEPARATED BY space.
*  ENDIF.
*  PERFORM popup_display USING 'XPOPSEQ[]'
*                              lf_title.
ENDFORM.                    " popup_seq_vehicles
*&---------------------------------------------------------------------*
*&      Form  make_popup_field_cat
*&---------------------------------------------------------------------*
*       Make Popup Field Category
*----------------------------------------------------------------------*
FORM make_popup_field_cat TABLES et_fieldcat TYPE slis_t_fieldcat_alv
                          USING  if_field.
*-- Fieldcatalog create automatically --------------------------------*
  DATA : ls_fieldcat  TYPE slis_fieldcat_alv,
         lt_fieldcat  TYPE slis_t_fieldcat_alv.
  DATA : col_pos(2) TYPE n.
  REFRESH et_fieldcat.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_internal_tabname     = 'XPOPSEQ'
            i_structure_name       = 'ZMMS0159'
       CHANGING
            ct_fieldcat            = lt_fieldcat[]
       EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    LOOP AT lt_fieldcat INTO ls_fieldcat.
      CASE ls_fieldcat-fieldname.
        WHEN 'WKDT' OR 'BODY_NO'.
          CLEAR et_fieldcat.
          MOVE-CORRESPONDING ls_fieldcat TO et_fieldcat.
          et_fieldcat-key                   = 'X'.
          col_pos = col_pos + 1.
          et_fieldcat-col_pos = col_pos .
          APPEND et_fieldcat.
        WHEN 'TIMESTAMP'.
          CASE if_field.
            WHEN space OR 'ALL' OR 'ASSMB' OR 'QUIME'.
              "Hide
            WHEN OTHERS.
              CLEAR et_fieldcat.
              MOVE-CORRESPONDING ls_fieldcat TO et_fieldcat.
              col_pos = col_pos + 1.
              et_fieldcat-col_pos = col_pos .
              APPEND et_fieldcat.
          ENDCASE.
        WHEN OTHERS.
          CLEAR et_fieldcat.
          MOVE-CORRESPONDING ls_fieldcat TO et_fieldcat.
          col_pos = col_pos + 1.
          et_fieldcat-col_pos = col_pos .
          APPEND et_fieldcat.
      ENDCASE.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " make_popup_field_cat
*&---------------------------------------------------------------------*
*&      Form  popup_display
*&---------------------------------------------------------------------*
*       POPUP Display
*----------------------------------------------------------------------*
FORM popup_display  USING    u_tabname
                             u_title.
  gs_popup_layout-zebra             = 'X'.
  gs_popup_layout-colwidth_optimize = 'X'.
  gs_popup_layout-coltab_fieldname  = 'COLOR'.

  FIELD-SYMBOLS <ft1> TYPE table.
  ASSIGN (u_tabname) TO <ft1>.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program    = g_repid
            i_grid_title          = u_title
            i_grid_settings       = gs_grid_settings
            is_layout             = gs_popup_layout
            it_fieldcat           = gt_popupfc
            i_default             = space
            i_screen_start_column = 1
            i_screen_start_line   = 1
            i_screen_end_column   = 100
            i_screen_end_line     = 20
       TABLES
            t_outtab              = <ft1>
       EXCEPTIONS
            program_error         = 1
            OTHERS                = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " popup_display
*&---------------------------------------------------------------------*
*&      Form  CHECK_STORAGE_LOC
*&---------------------------------------------------------------------*
FORM check_storage_loc .
  DATA :  lt_color     TYPE lvc_t_scol.

  IF xlist-labor = 'V' AND xlist-ebeln <> space. "IS NOT INITIAL.
    SELECT SINGLE *
    FROM ekpo
    WHERE ebeln = xlist-ebeln
      AND ebelp = xlist-ebelp.

    IF sy-subrc = 0 .
      xlist-lgort = ekpo-lgort.

      IF ekpo-lgort <> xlist-lgfsb.  "if different,make column with red
        PERFORM fill_cell_color USING    'LGORT'
                                co_negative
                                CHANGING lt_color.

        READ TABLE lt_color INDEX 1 TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
*          INSERT LINES OF lt_color INTO TABLE xlist-coltab.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_STORAGE_LOC
