*&------------------------------------------------------------------
*& Program ID     : ZMMR60200T
*& Profram Name   : Control cycle mass maintenance
*& Created by     : Yang
*& Created on     : 04.29.2009
*& Development ID : MM-008
*& Reference Pgm. : ZMMR1360
*& Description    : Control cycle mass maintenance
*&
*& Modification Log
*&====================================================================
*& Date        Developer      Request ID   Description
*& 04.29.2009  Yang                        First development
*&--------------------------------------------------------------------

REPORT  zmmr60200t                                .

*===============================================================*
* Data definition                                               *
*===============================================================*
*-----------------------------------------*
* Include                                 *
*-----------------------------------------*
INCLUDE zmmitop01.                             "Global TOP
INCLUDE zmmr60200_cls.                       "MM module local class
INCLUDE zmmr60200_cls2.                      "Additional local class
INCLUDE <icon>.
TYPE-POOLS: pdt.
*-----------------------------------------*
* Local class                             *
*-----------------------------------------*
*CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA : event_receiver TYPE REF TO lcl_event_receiver.
*CLASS lcl_event_receiver2 DEFINITION DEFERRED.
DATA : event_receiver2 TYPE REF TO lcl_event_receiver2.
*-----------------------------------------*
* Table definition                        *
*-----------------------------------------*
TABLES:
    mara,
    marc,
    mard,
    makt,
    pkhd,
    pvkt,
    zmms0155,
    rmpkr,
    zmmt0087.

*-----------------------------------------*
* data processing                         *
*-----------------------------------------*
* temp
DATA : BEGIN OF it_pkhd OCCURS 0.
        INCLUDE STRUCTURE pkhd.
DATA :  ferth      TYPE mara-ferth,
        formt      TYPE mara-formt,
        maktx      TYPE makt-maktx,
        matkl      TYPE mara-matkl,
        dispo      TYPE marc-dispo,
        pvbtx      TYPE pvkt-pvbtx,    "supply area text
        piid       TYPE pl_piid,
        rmatp      TYPE mara-rmatp,
        vspvb      TYPE marc-vspvb,
        lgfsb      TYPE marc-lgfsb,
        eprio      TYPE marc-eprio,
        lgpro      TYPE marc-lgpro,
        lgpbe      TYPE mard-lgpbe,
        aeszn      TYPE mara-aeszn,
        mstae      TYPE mara-mstae,
        line,
        mark,
        icon       TYPE icon-id,
        msg(100),
        pkkla,                       "Classic KANBAN
        pkimp,                       "Event-driven KANBAN
        pksum,
"Control Cycle for Digital Bucket Order to Vendor
        pkasm,
"Control Cycle for Digital Bucket Order to C.C
        zfeednm    TYPE zefeednm,
       END OF it_pkhd.

* main list
DATA : BEGIN OF it_list OCCURS 0.
        INCLUDE STRUCTURE it_pkhd.
DATA :  celltab    TYPE lvc_t_styl,
        f_col      TYPE lvc_t_scol,
       END OF it_list.

* copy line
DATA : it_copy LIKE TABLE OF it_pkhd WITH HEADER LINE.


* f4 entry variant
TABLES : cuvtab, cuvtab_valc, cabn, cabnt, cawn, cawnt, tsrtb.
DATA : table_fields  LIKE TABLE OF cuvtab_fld  WITH HEADER LINE.
DATA : entries_char  LIKE TABLE OF cuvtab_valc WITH HEADER LINE.
DATA : entries_non_char LIKE TABLE OF cuvtab_valn.
DATA : field_tab     LIKE TABLE OF dfies WITH HEADER LINE.
DATA : dynpfld_mapping LIKE TABLE OF dselc WITH HEADER LINE.
DATA : return_tab  LIKE TABLE OF ddshretval WITH HEADER LINE.

DATA : BEGIN OF f4_tab OCCURS 0,
        f001   TYPE char20,
        f002   TYPE char20,
        f003   TYPE char20,
        f004   TYPE char20,
        f005   TYPE char20,
        f006   TYPE char20,
        f007   TYPE char20,
        f008   TYPE char20,
        f009   TYPE char20,
        f010   TYPE char20,
        f011   TYPE char20,
        f012   TYPE char20,
        f013   TYPE char20,
        f014   TYPE char20,
        f015   TYPE char20,
       END OF f4_tab.

DATA: f4_atwrt LIKE TABLE OF f4_tab WITH HEADER LINE.
DATA: f4_kcprf LIKE TABLE OF tpkpt  WITH HEADER LINE.

* upload file info
DATA : filename         TYPE rlgrap-filename,
       filefilter       TYPE string,
       path             TYPE string,
       fullpath         TYPE string,
       user_action      TYPE i.
*       file_encoding    TYPE abap_encoding. NO EXIST 4.6
DATA : xlsdata LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_excel OCCURS 0,
         matnr      TYPE  pkhd-matnr,      "Material
         werks      TYPE  pkhd-werks,      "Plant
         prvbe      TYPE  pkhd-prvbe,      "Supply Area
         rksta      TYPE  pkhd-rksta,      "Control Cycle Category
         behaz      TYPE  pkhd-behaz,      "Number of kanban containers
         ablad      TYPE  pkhd-ablad,      "Storing position
         sigaz      TYPE  pkhd-sigaz,      "maximum empty
         umlgo      TYPE  pkhd-umlgo,      "Storage Location
         kwbzm      TYPE  pkhd-kwbzm,      "Replenishment Lead Time
         PKSFG      type  pkhd-PKSFG,      "KANBAN status seq
         zRHLH      type  pkhd-zRHLH,      "left/right
         zfeeder    TYPE  pkhd-zfeeder,    " feeder
         zzeisbe    TYPE  pkhd-zzeisbe,    "Safety Stock
         zztim      TYPE  pkhd-zztim,      "Safety Time
         anzlt      TYPE  pkhd-anzlt,      " Number of Load Carriers
        END OF it_excel.

DATA : BEGIN OF excel_down OCCURS 0,
         matnr(20),     "TYPE  pkhd-matnr,       "Material
         werks(20),     "TYPE  pkhd-werks,       "Plant
         prvbe(20),     "TYPE  pkhd-prvbe,       "Supply Area
         rksta(20),     "TYPE  pkhd-rksta,       "Control Cycle Category
         behaz(20),     "TYPE  pkhd-behaz,
"Number of kanban containers
         ablad(20),     "TYPE  pkhd-ablad,       "Storing position
         sigaz(20),     "TYPE  pkhd-sigaz,       "maximum empty
         umlgo(20),     "TYPE  pkhd-umlgo,       "Storage Location
         kwbzm(20),     "type  pkhd-kwbzm, "Replenishment Lead Time
         PKSFG(4),      "type  pkhd-PKSFG,      "KANBAN status seq
         zRHLH(2),      "type  pkhd-zRHLH,      "left/right
         zzeisbe(20),   "type  pkhd-zzeisbe,     "Safety Stock
         zztim(20),     "type  pkhd-zztim,       "Safety Time
         zfeeder(5) ,    " feeder
         anzlt(5),   " Number of Load Carriers
        END OF excel_down.

data: it_pvkt like pvkt     occurs 0 with header line,
      it_87   like zmmt0087 occurs 0 with header line.


*** message in screen
DATA : gs_class_msg    TYPE lvc_s_msg1.
DATA : error_in_data ,
      l_scroll     TYPE lvc_s_stbl.

* constants
DATA : icon_gre TYPE icon-id VALUE '@5B@',
       icon_red TYPE icon-id VALUE '@5C@',
       icon_del TYPE icon-id VALUE '@11@',  "dummy icon
       icon_pos TYPE icon-id VALUE '@P8@'.  "ICON_RATING_POSITIVE

CONSTANTS :
    c_datum  TYPE datum VALUE '99991231'.


data: p_werks  TYPE pkhd-werks.
*===============================================================*
* Selection screen                                              *
*===============================================================*
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-b01.
PARAMETERS :
*    p_werks  TYPE pkhd-werks OBLIGATORY MEMORY ID wrk DEFAULT 'KVA1'.
  p_p001 RADIOBUTTON GROUP PLNT DEFAULT 'X',
  p_e001 RADIOBUTTON GROUP PLNT,

* by ig.moon - Engine Plant Split {
  P_E002 RADIOBUTTON GROUP PLNT.
* }

SELECT-OPTIONS :
*    s_werks  FOR pkhd-werks,
    s_pknum  FOR pkhd-pknum ,
    s_matnr  FOR pkhd-matnr,
    s_prvbe  FOR pkhd-prvbe,
    s_tempb  FOR mara-tempb,
    s_rksta  FOR pkhd-rksta,
    s_matkl  FOR mara-matkl,
    s_dispo  FOR marc-dispo,
    s_ferth  FOR mara-ferth,
    s_formt  FOR mara-formt.
PARAMETERS :
    p_mode  TYPE ctu_mode DEFAULT 'N'.
SELECTION-SCREEN END OF BLOCK block1.
*===============================================================*
* Events                                                        *
*===============================================================*
INITIALIZATION.
  g_repid = sy-repid.
  g_mode  = 'D'.        "display
  s_rksta-option = 'EQ'. s_rksta-sign = 'I'.
  s_rksta-low = 'I'.   append s_rksta.
  s_rksta-low = 'K'.   append s_rksta.


AT SELECTION-SCREEN.


START-OF-SELECTION.
  IF P_P001 = 'X'.
    P_WERKS = 'P001'.
* by ig.moon - Engine Plant Split {
*  ELSE.
   ELSEIF P_E001 = 'X'.
    P_WERKS = 'E001'.
   ELSE.
    P_WERKS = 'E002'.
* }
  ENDIF.

  perform read_key_master.

  PERFORM run.

END-OF-SELECTION.
  CALL SCREEN 100.


*===============================================================*
* Subroutine                                                    *
*===============================================================*
*&---------------------------------------------------------------------*
*&      Form  run
*&---------------------------------------------------------------------*
*       program run~
*----------------------------------------------------------------------*
FORM run .
  DATA :
         l_tabix LIKE sy-tabix,
         l_lgpbe LIKE mard-lgpbe.
  DATA : lt_pkhd TYPE TABLE OF pkhd WITH HEADER LINE.
  DATA : lt_color   TYPE lvc_t_scol.

  REFRESH : it_pkhd, it_list, lt_color.
  CLEAR   : it_pkhd, it_list.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_pkhd
    FROM pkhd AS k INNER JOIN mara AS m
                   ON k~matnr = m~matnr
                   INNER JOIN marc AS c
                   ON k~matnr = c~matnr
                  AND k~werks = c~werks
   WHERE k~matnr IN s_matnr
     AND k~werks =  p_werks
     AND k~prvbe IN s_prvbe
     AND k~pknum IN s_pknum
     AND k~rksta IN s_rksta
     AND m~matkl IN s_matkl
     AND m~tempb IN s_tempb
     AND m~ferth IN s_ferth
     AND m~formt IN s_formt
     AND c~dispo IN s_dispo
     AND m~mstae NOT IN ('13', '14')
   ORDER BY k~pknum.



  LOOP AT it_pkhd.
    MOVE-CORRESPONDING it_pkhd TO it_list.
*    IF it_pkhd-packv IS NOT INITIAL.
    IF it_pkhd-packv <> ''.

      PERFORM get_pi_info USING    it_pkhd-packv
                          CHANGING it_list-behmg
                                   it_list-pkbht.
      PERFORM read_packobj_ident USING    it_pkhd-packv
                                 CHANGING it_list-piid.
    ENDIF.

* text
    PERFORM read_text USING     'ZFEEDNM'
                                it_pkhd-zfeeder
                      CHANGING  it_list-zfeednm.

    PERFORM read_text USING     'MAKTX'
                                it_pkhd-matnr
                      CHANGING  it_list-maktx.

    PERFORM read_text USING     'PVBTX'
                                it_pkhd-prvbe
                      CHANGING  it_list-pvbtx.


    SELECT SINGLE lgpbe
           INTO it_list-lgpbe
          FROM mard
     WHERE matnr EQ it_pkhd-matnr
       AND werks EQ it_pkhd-werks
       AND lgort EQ it_pkhd-lgpro.

    CLEAR : lt_color[].

    IF it_pkhd-rksta EQ 'I'.
*      IF it_list-aeszn EQ '12' OR
*         it_list-aeszn EQ '22' OR
*         it_list-aeszn EQ '32'.
*      ELSE.
*        PERFORM fill_color  USING    'ZZEISBE'
*                            CHANGING lt_color.
*        it_list-icon = icon_red_light.
*      ENDIF.

*      IF it_list-piid IS INITIAL.
*        PERFORM fill_color  USING    'PIID'
*                            CHANGING lt_color.
*        it_list-icon = icon_red_light.
*      ENDIF.

      IF it_pkhd-eprio NE space.
        IF it_pkhd-umlgo <> it_pkhd-eprio.
          PERFORM fill_color  USING    'UMLGO'
                              CHANGING lt_color.
          it_list-icon = icon_red_light.
        ENDIF.
      ELSE.
        IF it_pkhd-umlgo <> it_pkhd-lgfsb.
          PERFORM fill_color  USING    'UMLGO'
                              CHANGING lt_color.
          it_list-icon = icon_red_light.

        ENDIF.
      ENDIF.

      IF it_pkhd-ablad <> it_list-lgpbe.
        PERFORM fill_color  USING    'ABLAD'
                            CHANGING lt_color.
        it_list-icon = icon_red_light.
      ENDIF.

      IF it_pkhd-prvbe <> it_pkhd-vspvb.
        PERFORM fill_color  USING    'PRVBE'
                            CHANGING lt_color.
        it_list-icon = icon_red_light.
      ENDIF.

    ENDIF.


    IF it_pkhd-zfeeder is initial.
      PERFORM fill_color  USING    'ZFEEDER'
                          CHANGING lt_color.
      it_list-icon = icon_red_light.
    ENDIF.
    IF it_pkhd-PKSFG is initial.
      PERFORM fill_color  USING    'PKSFG'
                          CHANGING lt_color.
      it_list-icon = icon_red_light.
    ENDIF.

    INSERT LINES OF lt_color   INTO TABLE it_list-f_col.
    IF it_list-icon IS INITIAL.
      it_list-icon = icon_green_light.
    ENDIF.

    APPEND it_list. CLEAR it_list.
  ENDLOOP.


  LOOP AT it_list WHERE ferth NE space.
    l_tabix = sy-tabix.
    CLEAR : lt_color[].

    LOOP AT it_pkhd WHERE ferth EQ it_list-ferth
                      AND rksta EQ it_list-rksta.
      IF it_list-kwbzm  <> it_pkhd-kwbzm.   " RLT
        PERFORM fill_color  USING    'KWBZM'
                            CHANGING lt_color.
        it_list-icon = icon_red_light.
        EXIT.
      ENDIF.
    ENDLOOP.
    LOOP AT it_pkhd WHERE ferth EQ it_list-ferth
                      AND rksta EQ it_list-rksta.
      IF it_list-zfeeder  <> it_pkhd-zfeeder. "Feeder
        PERFORM fill_color  USING    'ZFEEDER'
                            CHANGING lt_color.
        it_list-icon = icon_red_light.
        EXIT.
      ENDIF.
    ENDLOOP.
    LOOP AT it_pkhd WHERE ferth EQ it_list-ferth
                      AND rksta EQ it_list-rksta.
      IF it_list-zztim    <> it_pkhd-zztim.   "
        PERFORM fill_color  USING    'ZZTIM'
                            CHANGING lt_color.
        it_list-icon = icon_red_light.
        EXIT.
      ENDIF.
    ENDLOOP.

    INSERT LINES OF lt_color   INTO TABLE it_list-f_col.

    IF it_list-icon IS INITIAL.
      it_list-icon = icon_green_light.
    ENDIF.

    MODIFY it_list INDEX l_tabix.
  ENDLOOP.


ENDFORM.                    " run
*&---------------------------------------------------------------------*
*&      Form  read_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_text  USING    u_id
                         u_val
                CHANGING c_txt.

  CASE u_id.
    WHEN 'MAKTX'.
      SELECT SINGLE maktx
        FROM makt
        INTO c_txt
       WHERE matnr = u_val
         AND spras = sy-langu.

    WHEN 'PVBTX'.
      read table it_pvkt with key prvbe = u_val binary search.
      c_txt = it_pvkt-pvbtx.
*      SELECT SINGLE pvbtx
*        FROM pvkt
*        INTO c_txt
*       WHERE werks = p_werks
*         AND prvbe = u_val
*         AND spras = sy-langu.

*    WHEN 'CABNT'.
*      SELECT SINGLE atbez
*        FROM cabnt
*        INTO c_txt
*       WHERE atinn = u_val
*         AND spras = sy-langu.

    WHEN 'ZFEEDNM'.
      read table it_87 with key zfeeder = u_val.
      c_txt = it_87-zfeednm.
*      SELECT SINGLE zfeednm
*        FROM zmmt0087
*        INTO c_txt
*       WHERE zfeeder = u_val.

  ENDCASE.

ENDFORM.                    " read_text
*&---------------------------------------------------------------------*
*&      Form  handle_data_changed
*&---------------------------------------------------------------------*
*       check changed cell
*----------------------------------------------------------------------*
FORM handle_data_changed  USING  u_changed
                          TYPE REF TO cl_alv_changed_data_protocol.

  DATA : ls_mod_cells TYPE lvc_s_modi,
         ls_cells     TYPE lvc_s_modi,
         l_line       TYPE sy-tfill.

  LOOP AT u_changed->mt_good_cells INTO ls_mod_cells.
    PERFORM assign_changed_field USING u_changed
                                       ls_mod_cells-fieldname
                                       ls_mod_cells-row_id.
    CLEAR : gs_class_msg, error_in_data .

    CASE ls_mod_cells-fieldname.
      WHEN 'MATNR'.
        PERFORM input_check_material.
      WHEN 'PRVBE'.
        PERFORM input_check_supply_area.

      WHEN 'UMLGO'.
        PERFORM input_check_location.

      WHEN 'KWBZM'.      "Replenishment Lead Time in Hours:Minutes

      WHEN 'LOGGR'.

      WHEN 'ZFEEDER'.

        IF NOT it_list-zfeeder IS INITIAL.
          PERFORM input_check_feeder.
          PERFORM read_text USING     'ZFEEDNM'
                             it_list-zfeeder
                   CHANGING  it_list-zfeednm.
        ENDIF.

    ENDCASE.
    IF error_in_data EQ 'X'.
      CALL METHOD u_changed->add_protocol_entry
        EXPORTING
          i_msgid     = gs_class_msg-msgid
          i_msgno     = gs_class_msg-msgno
          i_msgty     = 'E'
          i_msgv1     = gs_class_msg-msgv1
          i_msgv2     = gs_class_msg-msgv2
          i_msgv3     = gs_class_msg-msgv3
          i_fieldname = ls_mod_cells-fieldname
          i_row_id    = ls_mod_cells-row_id.
    ELSE.
      IF it_list-line = 'C'.
        it_list-line = 'S'.
      ENDIF.
      MODIFY it_list INDEX ls_mod_cells-row_id.
    ENDIF.
  ENDLOOP.
  IF error_in_data EQ 'X'.
    CALL METHOD u_changed->display_protocol.
  ELSE.
*    PERFORM gui_alv_refresh.
  ENDIF.
ENDFORM.                    " handle_data_changed
*&---------------------------------------------------------------------*
*&      Form  handle_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_user_command  USING    p_e_ucomm.

ENDFORM.                    " handle_user_command
*&---------------------------------------------------------------------*
*&      Form  handle_toolbar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_toolbar  USING    u_object
                              u_interactive.


ENDFORM.                    " handle_toolbar
*&---------------------------------------------------------------------*
*&      Form  HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM hotspot_click  USING    u_row
                             u_column.
  READ TABLE it_list INDEX u_row.
  CASE u_column.
    WHEN 'PKNUM'.
*      CHECK it_list-pknum IS NOT INITIAL. "INCORRECT 4.6C
      CHECK it_list-pknum <> ''.
      PERFORM link_control_cycle USING it_list-pknum.
  ENDCASE.
ENDFORM.                    " HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Module  status_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA : l_tfill(5).

  DESCRIBE TABLE it_list LINES l_tfill.
  SET TITLEBAR '100' WITH l_tfill.

  SET PF-STATUS '100'.
ENDMODULE.                 " status_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  init_screen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_screen OUTPUT.
  IF g_custom_container1 IS INITIAL.
    PERFORM gui_alv_create.
    PERFORM gui_alv_content.
*    PERFORM gui_alv_sort.
    PERFORM gui_alv_cell_control.
    PERFORM gui_alv_display.
    PERFORM gui_alv_event.
  ELSE.
    PERFORM gui_alv_refresh.
  ENDIF.
  PERFORM set_focus.

ENDMODULE.                 " init_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  gui_alv_create
*&---------------------------------------------------------------------*
*       create screen object
*----------------------------------------------------------------------*
FORM gui_alv_create .
  CREATE OBJECT g_custom_container1
    EXPORTING
      container_name = g_container1.

  CREATE OBJECT g_grid1
    EXPORTING
      i_parent = g_custom_container1.
ENDFORM.                    " gui_alv_create
*&---------------------------------------------------------------------*
*&      Form  gui_alv_content
*&---------------------------------------------------------------------*
*       fill screen fieldcatalog
*
*  Selection modes for SEL_MODE
* 'A' : Column and row selection
* 'B' : Simple selection, list box
* 'C' : Multiple selection, list box
* 'D' : Cell selection
*----------------------------------------------------------------------*
FORM gui_alv_content .
**** layout ****
  CLEAR gs_fcatlayo.
*  gs_fcatlayo-zebra                = 'X'.
  gs_fcatlayo-box_fname            = 'MARK'.
  gs_fcatlayo-sel_mode             = 'A'.
  gs_fcatlayo-no_toolbar           = 'X'.
  gs_fcatlayo-stylefname           = 'CELLTAB'.
  gs_fcatlayo-edit                 = 'X'.
  gs_fcatlayo-ctab_fname           = 'F_COL'.

**** FIELD CATALOG ****
  DATA : col_pos TYPE i.
  DATA : l_fieldcatalog  TYPE lvc_s_fcat.
  DATA : l_txt(20).
  REFRESH it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'ICON'.
  l_fieldcatalog-icon              = 'X'.
  l_fieldcatalog-reptext           = 'St.'.
  l_fieldcatalog-outputlen         = 5.
  l_fieldcatalog-key               = 'X'.
  l_fieldcatalog-just              = 'C'.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'PKNUM'.
  l_fieldcatalog-ref_table         = 'PKHD'.
  l_fieldcatalog-key               = 'X'.
  l_fieldcatalog-hotspot           = 'X'.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'RKSTA'.
  l_fieldcatalog-ref_table         = 'PKHD'.
  l_fieldcatalog-key               = 'X'.
  l_fieldcatalog-outputlen         = 2.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'MATNR'.
  l_fieldcatalog-ref_table         = 'MARA'.
  l_fieldcatalog-outputlen         = 16.
  l_fieldcatalog-key               = 'X'.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'MAKTX'.
  l_fieldcatalog-ref_table         = 'MAKT'.
  l_fieldcatalog-outputlen         = 26.
  APPEND l_fieldcatalog TO it_fieldcatalog.
  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'MATKL'.
  l_fieldcatalog-ref_table         = 'MARA'.
  APPEND l_fieldcatalog TO it_fieldcatalog.
  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'DISPO'.
  l_fieldcatalog-ref_table         = 'MARC'.
  APPEND l_fieldcatalog TO it_fieldcatalog.
  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'WERKS'.
  l_fieldcatalog-ref_table         = 'PKHD'.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'PRVBE'.
  l_fieldcatalog-ref_table         = 'PKHD'.
  l_fieldcatalog-checktable        = '!'.  "do not check foreign keys
  l_fieldcatalog-outputlen         = 5.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'PVBTX'.
  l_fieldcatalog-ref_table         = 'PVKT'.
  l_fieldcatalog-outputlen         = 16.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'ZRHLH'. "Left/Right
  l_fieldcatalog-ref_table         = 'PKHD'.
  l_fieldcatalog-outputlen         = 05.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'ZZFSTP'. "FEEDER STOP
  l_fieldcatalog-ref_table         = 'PKHD'.
  l_fieldcatalog-outputlen         = 05.
  APPEND l_fieldcatalog TO it_fieldcatalog.

*For KMMG
*  CLEAR l_fieldcatalog.
*  col_pos = col_pos + 1.
*  l_fieldcatalog-col_pos           = col_pos.
*  l_fieldcatalog-fieldname         = 'FERTH'. "Part Group Number
*  l_fieldcatalog-ref_table         = 'MARA'.
*  l_fieldcatalog-outputlen         = 18.
*  APPEND l_fieldcatalog TO it_fieldcatalog.
*
*  CLEAR l_fieldcatalog.
*  col_pos = col_pos + 1.
*  l_fieldcatalog-col_pos           = col_pos.
*  l_fieldcatalog-fieldname         = 'FORMT'. "Part Assembly Code
*  l_fieldcatalog-ref_table         = 'MARA'.
*  l_fieldcatalog-outputlen         = 4.
*  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'ANZLT'.
  l_fieldcatalog-ref_table         = 'PKHD'.
  l_fieldcatalog-outputlen         = 7.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'ABLAD'.   "Storing position
  l_fieldcatalog-ref_table         = 'PKHD'.
  l_fieldcatalog-outputlen         = 16.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'PKSFG'. "Status seq
  l_fieldcatalog-ref_table         = 'PKHD'.
  l_fieldcatalog-outputlen         = 04.
  APPEND l_fieldcatalog TO it_fieldcatalog.


  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'BEHAZ'.   "# of kanban containers
  l_fieldcatalog-ref_table         = 'PKHD'.
  l_fieldcatalog-outputlen         = 3.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'SIGAZ'.      "Maximum Number
  l_fieldcatalog-ref_table         = 'PKHD'.       "Empty Containers
  l_fieldcatalog-outputlen         = 3.
  APPEND l_fieldcatalog TO it_fieldcatalog.


  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'KWBZM'.      "RLT
  l_fieldcatalog-ref_table         = 'PKHD'.
  l_fieldcatalog-outputlen         = 8.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'ZZTIM'.      "Safety Time
  l_fieldcatalog-ref_table         = 'PKHD'.
  l_fieldcatalog-outputlen         = 8.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'BEHMG'.
  l_fieldcatalog-ref_table         = 'PKHD'.
  l_fieldcatalog-qfieldname        = 'MEINS'.
  l_fieldcatalog-no_zero           = 'X'.
  l_fieldcatalog-outputlen         = 10.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'ZZEISBE'.
  l_fieldcatalog-ref_table         = 'PKHD'.
  l_fieldcatalog-qfieldname        = 'MEINS'.
  l_fieldcatalog-no_zero           = 'X'.
  l_fieldcatalog-outputlen         = 10.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'ZFEEDER'.
*  l_fieldcatalog-ref_table         = 'PKHD'.
  l_fieldcatalog-ref_table         = 'ZMMT0087'.
  l_fieldcatalog-qfieldname        = 'ZFEEDER'.
  l_fieldcatalog-f4availabl        = 'X'.
  l_fieldcatalog-outputlen         = 5.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'ZFEEDNM'.
  l_fieldcatalog-ref_field         = 'ZFEEDNM'.
  l_fieldcatalog-ref_table         = 'ZMMT0087'.
  l_fieldcatalog-outputlen         = 20.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'MEINS'.
  l_fieldcatalog-ref_table         = 'MARA'.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'PKBHT'.
  l_fieldcatalog-ref_table         = 'PKHD'.
  l_fieldcatalog-outputlen         = 5.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'UMLGO'.
  l_fieldcatalog-ref_table         = 'PKHD'.
*  l_fieldcatalog-checktable        = '!'.  "do not check foreign keys
  APPEND l_fieldcatalog TO it_fieldcatalog.


*  CLEAR l_fieldcatalog.
*  col_pos = col_pos + 1.
*  l_fieldcatalog-col_pos           = col_pos.
*  l_fieldcatalog-fieldname         = 'PIID'.
*  l_fieldcatalog-ref_field         = 'PACKV'.
*  l_fieldcatalog-ref_table         = 'PKHD'.
*  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'EBELN'.
  l_fieldcatalog-ref_field         = 'EBELN'.
  l_fieldcatalog-ref_table         = 'PKHD'.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'EBELP'.
  l_fieldcatalog-ref_field         = 'EBELP'.
  l_fieldcatalog-ref_table         = 'PKHD'.
  APPEND l_fieldcatalog TO it_fieldcatalog.

  CLEAR l_fieldcatalog.
  col_pos = col_pos + 1.
  l_fieldcatalog-col_pos           = col_pos.
  l_fieldcatalog-fieldname         = 'MSG'.
  l_fieldcatalog-reptext           = 'Result'.
  l_fieldcatalog-outputlen         = 45.
  APPEND l_fieldcatalog TO it_fieldcatalog.

ENDFORM.                    " gui_alv_content
*&---------------------------------------------------------------------*
*&      Form  gui_alv_display
*&---------------------------------------------------------------------*
*       screen display
*----------------------------------------------------------------------*
FORM gui_alv_display .
  CALL METHOD g_grid1->set_table_for_first_display
    EXPORTING
      is_layout            = gs_fcatlayo
      it_toolbar_excluding = gt_exclude
    CHANGING
      it_outtab            = it_list[]
      it_fieldcatalog      = it_fieldcatalog
      it_sort              = gt_sort.
ENDFORM.                    " gui_alv_display
*&---------------------------------------------------------------------*
*&      Form  gui_alv_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gui_alv_event .
  CALL METHOD g_grid1->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CREATE OBJECT event_receiver.
  SET HANDLER event_receiver->handle_hotspot_click FOR g_grid1.
  SET HANDLER event_receiver->handle_data_changed  FOR g_grid1.
  CREATE OBJECT event_receiver2.
  SET HANDLER event_receiver2->handle_f4            FOR g_grid1.

  PERFORM f4_field_assign.
ENDFORM.                    " gui_alv_event
*&---------------------------------------------------------------------*
*&      Form  gui_alv_refresh
*&---------------------------------------------------------------------*
*       screen data refresh
*----------------------------------------------------------------------*
FORM gui_alv_refresh .
  DATA : row_no      TYPE lvc_s_roid,
         row_info    TYPE lvc_s_row,
         col_info    TYPE lvc_s_col.

  CHECK NOT g_grid1 IS INITIAL.

  l_scroll-row = 'X'.
  l_scroll-col = 'X'.

  CALL METHOD g_grid1->refresh_table_display
    EXPORTING
      i_soft_refresh = 'X'
      is_stable      = l_scroll.     "?? ??? ??? refresh


*  CALL METHOD g_grid1->get_scroll_info_via_id
*    IMPORTING
*      es_row_no   = row_no
*      es_row_info = row_info
*      es_col_info = col_info.
*
*  CALL METHOD g_grid1->refresh_table_display.
*
*  CALL METHOD g_grid1->set_scroll_info_via_id
*    EXPORTING
*      is_row_info = row_info
*      is_col_info = col_info
*      is_row_no   = row_no.

ENDFORM.                    " gui_alv_refresh
*&---------------------------------------------------------------------*
*&      Form  set_focus
*&---------------------------------------------------------------------*
*       screen focus
*----------------------------------------------------------------------*
FORM set_focus .

  CHECK NOT g_grid1 IS INITIAL.

  CALL METHOD cl_gui_control=>set_focus
    EXPORTING
      control = g_grid1.
ENDFORM.                    " set_focus
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command INPUT.
  PERFORM user_command.
ENDMODULE.                 " USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command .
  CALL METHOD g_grid1->check_changed_data.  "changed data

  DATA : l_code TYPE sy-ucomm.
  l_code = ok_code.
  CLEAR ok_code.
  CASE l_code.
    WHEN 'ADJU'.
      PERFORM get_selected_rows.
      PERFORM adjust_control_cycle.
    WHEN 'SAVE'.
      PERFORM user_command_save.
    WHEN 'REFR'.
      g_mode = 'D'.
      PERFORM run.
      PERFORM gui_alv_cell_control.
*      PERFORM gui_alv_refresh.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'DELE'.              "delete line
      PERFORM user_command_delete.
    WHEN 'COPY'.              "copy line
      PERFORM user_command_copy.
      PERFORM remove_mark_in_screen.
    WHEN 'TOGL'.              "display<->change
      PERFORM user_command_toggle.
      PERFORM gui_alv_cell_control.
    WHEN 'NEW'.               "new line
      PERFORM user_command_new.
      PERFORM gui_alv_cell_control.
    WHEN 'PASTE'.
      PERFORM user_command_paste.
    WHEN 'ENTR'.
      PERFORM user_command_enter.
      LEAVE TO SCREEN 0.
    WHEN 'CONT'.
      PERFORM user_command_cont.
      LEAVE TO SCREEN 0.
    WHEN 'STUP'.              "sort up
      PERFORM user_command_sort USING 'U'.
    WHEN 'STDN'.              "sort down
      PERFORM user_command_sort USING 'D'.
    WHEN 'UPLOAD'.
      PERFORM user_command_upload.
      PERFORM gui_alv_cell_control.
    WHEN 'DOWN'.               "templete download
      PERFORM user_command_download USING 'T'.
    WHEN 'DNLOAD'.             "screen data download
      PERFORM user_command_download USING 'S'.
    WHEN 'MASS'.
      PERFORM get_selected_rows.
      PERFORM user_command_mass .
    WHEN OTHERS.

  ENDCASE.
ENDFORM.                    " user_command
*&---------------------------------------------------------------------*
*&      Module  exit_commnad  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_commnad INPUT.
  CASE ok_code.
    WHEN 'EXIT' OR 'CANC'.
      IF sy-dynnr = '0110' OR
         sy-dynnr = '0120'.
        LEAVE TO SCREEN 0.
      ELSE.
        LEAVE PROGRAM.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " exit_commnad  INPUT
*&---------------------------------------------------------------------*
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_selected_rows.
  DATA : lt_rows TYPE lvc_t_row.
  DATA : l_row   TYPE lvc_s_row.

* Get Selected Rows
  CALL METHOD g_grid1->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  DESCRIBE TABLE lt_rows.
  IF sy-tfill = 0.
    MESSAGE e800(13) .
  ENDIF.


  LOOP AT lt_rows INTO l_row.
    READ TABLE it_list INDEX l_row-index.
    it_list-mark = 'X'.
    MODIFY it_list INDEX l_row-index.
  ENDLOOP.

ENDFORM.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  user_command_delete
*&---------------------------------------------------------------------*
*       delete selected line
*----------------------------------------------------------------------*
FORM user_command_delete .
  DATA : lt_list LIKE TABLE OF it_pkhd WITH HEADER LINE.
  DATA : l_answer.
  PERFORM get_selected_rows.
  PERFORM func_confirm_data_loss CHANGING l_answer.
  CHECK l_answer = 'J'.

  LOOP AT it_list WHERE mark = 'X'.
    IF it_list-pknum IS INITIAL.          "delete from itab.
      DELETE it_list.
    ELSE.                                 "delete from DB
      REFRESH : bdcdata, it_message.
      CLEAR : bdcdata, it_message.

      PERFORM bdc_delete USING it_list-werks
                               it_list-pknum.
      PERFORM bdc_return CHANGING it_list-icon
                                  it_list-msg.
      IF it_list-icon = icon_red.
        it_list-icon = it_list-icon.
        it_list-msg  = it_list-msg.
      ELSE.
        it_list-line = 'D'.
        it_list-icon = icon_del.
      ENDIF.
      MODIFY it_list .
    ENDIF.
  ENDLOOP.
ENDFORM.                    " user_command_delete
*&---------------------------------------------------------------------*
*&      Form  func_confirm_data_loss
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM func_confirm_data_loss  CHANGING c_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM_DATA_LOSS'
       EXPORTING
            defaultoption = 'J'
            titel         = text-101
       IMPORTING
            answer        = c_answer.
ENDFORM.                    " func_confirm_data_loss
*&---------------------------------------------------------------------*
*&      Form  gui_alv_cell_control
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gui_alv_cell_control .
  LOOP AT it_list.
    CLEAR it_list-celltab.
    PERFORM fill_celltab USING    it_list-line
                                  it_list-rksta
                         CHANGING it_list-celltab[].
    MODIFY it_list.
  ENDLOOP.

ENDFORM.                    " gui_alv_cell_control
*&---------------------------------------------------------------------*
*&      Form  fill_celltab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_celltab  USING    u_line
                            u_rksta
                   CHANGING c_celltab  TYPE lvc_t_styl.

  DATA : l_fieldcatalog  TYPE lvc_s_fcat.
  DATA : l_celltab       TYPE lvc_s_styl.

  LOOP AT it_fieldcatalog INTO l_fieldcatalog.
    l_celltab-fieldname = l_fieldcatalog-fieldname.
    CASE u_line.
      WHEN 'N'.                       "New line
        CASE l_celltab-fieldname.
          WHEN 'MATNR' OR 'WERKS' OR 'PRVBE' OR 'ABLAD' OR 'ANZLT' OR
               'KWBZM' OR 'ZFEEDER' OR 'ZRHLH' or 'PKSFG'.
            l_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
          WHEN 'UMLGO'.
            IF u_rksta = 'M'.
              l_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
            ELSE.
              l_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
            ENDIF.
          WHEN 'SIGAZ' OR 'BEHAZ'.
            IF u_rksta = 'K'.
              l_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
            ELSE.
              l_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
            ENDIF.
          WHEN 'ZZEISBE' OR 'ZZTIM'.
            IF u_rksta = 'I' OR
               u_rksta = 'M' OR
               u_rksta = 'A'.
              l_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
            ELSE.
              l_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
            ENDIF.
          WHEN OTHERS.
            l_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
        ENDCASE.
      WHEN 'C'.                       "change line
        CASE l_celltab-fieldname.
          WHEN 'PRVBE' OR 'ABLAD' OR 'KWBZM' OR 'ZFEEDER' OR 'ANZLT'
            OR 'ZRHLH' or 'PKSFG'.
            l_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
          WHEN 'UMLGO'.
            IF u_rksta = 'M'.
              l_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
            ELSE.
              l_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
            ENDIF.
          WHEN 'SIGAZ' OR 'BEHAZ'.
            IF u_rksta = 'K'.
              l_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
            ELSE.
              l_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
            ENDIF.
          WHEN 'ZZEISBE' OR 'ZZTIM'.
            IF u_rksta = 'I' OR
               u_rksta = 'M' OR
               u_rksta = 'A'.
              l_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
            ELSE.     "classical
              l_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
            ENDIF.
          WHEN OTHERS.
            l_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
        ENDCASE.

      WHEN OTHERS.
        l_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDCASE.
    INSERT l_celltab INTO TABLE c_celltab.
  ENDLOOP.
ENDFORM.                    " fill_celltab
*&---------------------------------------------------------------------*
*&      Form  user_command_toggle
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command_toggle .

  IF g_mode = 'D'.
    g_mode = 'M'.
    it_list-line = 'C'.
    MODIFY it_list TRANSPORTING LINE where LINE = space.
  ELSEIF g_mode = 'M'.
    g_mode = 'D'.
    CLEAR it_list-line.
    MODIFY it_list TRANSPORTING LINE where LINE = 'C'.
  ENDIF.

ENDFORM.                    " user_command_toggle
*&---------------------------------------------------------------------*
*&      Form  assign_changed_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM assign_changed_field  USING  u_changed
                              TYPE REF TO cl_alv_changed_data_protocol
                                  u_fieldname
                                  u_row_id.
  DATA : l_field(20),
         l_val(20).
  CONCATENATE 'IT_LIST-' u_fieldname INTO l_field.
  FIELD-SYMBOLS : <fs1> .
  ASSIGN (l_field)  TO <fs1>.

  READ TABLE it_list INDEX u_row_id.

* get cell input value
  CALL METHOD u_changed->get_cell_value
    EXPORTING
      i_row_id    = u_row_id
      i_fieldname = u_fieldname
    IMPORTING
      e_value     = <fs1>.

ENDFORM.                    " assign_changed_field
*&---------------------------------------------------------------------*
*&      Form  user_command_save
*&---------------------------------------------------------------------*
*       save changed data
*****  mode (it_list-line)
* S = changed control cycle
* N = new control cycle
* D = delete control cycle
* A = add ALC at existed KANBAN
*----------------------------------------------------------------------*
FORM user_command_save .
** changed data / inserted data
  LOOP AT it_list WHERE LINE NE 'C'
                    AND LINE NE space.
    REFRESH : bdcdata, it_message.
    CLEAR : bdcdata, it_message, it_list-icon, it_list-msg.
    CASE it_list-line.
      WHEN 'S'.                   "changed data
        PERFORM bdc_change.
      WHEN 'N'.                   "inserted data
        PERFORM bdc_insert_fieldcheck.
        PERFORM bdc_insert.
      WHEN 'A'.                   "add ALC data

    ENDCASE.
    PERFORM bdc_return CHANGING it_list-icon
                                it_list-msg.
    IF it_list-icon = icon_gre.
      CLEAR : it_list-line, it_list-mark.
    ENDIF.
    MODIFY it_list.
  ENDLOOP.

  IF sy-subrc NE 0.
    MESSAGE s314(sls).
  ELSE.
    g_mode = 'D'.
    PERFORM clear_table_after_save.
    PERFORM gui_alv_cell_control.
  ENDIF.
ENDFORM.                    " user_command_save
*&---------------------------------------------------------------------*
*&      Form  bdc_delete
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bdc_delete USING u_werks
                      u_pknum.
*# initial screen
  PERFORM bdc_dynpro_processing USING:
                      'X' 'SAPLMPK_CCY_UI'           '0110',
                      ' ' 'BDC_OKCODE'               '=GETD' ,
                      ' ' 'RMPKR-WERKS'              u_werks,
                      ' ' 'RMPKR-PRVBE'              '',
                      ' ' 'RMPKR-RGVER'              '',
                      ' ' 'RMPKR-PKNUM'              u_pknum.
*# toggle
  PERFORM bdc_dynpro_processing USING:
                      'X' 'SAPLMPK_CCY_UI'           '0110',
                      ' ' 'BDC_OKCODE'               '=TOGG' .

*# delete
  PERFORM bdc_dynpro_processing USING:
                      'X' 'SAPLMPK_CCY_UI'           '0110',
                      ' ' 'BDC_OKCODE'               '=DELE' .
*#. confirm
  PERFORM bdc_dynpro_processing USING:
                      'X' 'SAPLSPO1'                 '0300',
                      ' ' 'BDC_OKCODE'               '=YES' .
*#. save
  PERFORM bdc_dynpro_processing USING:
                      'X' 'SAPLMPK_CCY_UI'           '0110',
                      ' ' 'BDC_OKCODE'               '=SAVE' .

  CALL TRANSACTION 'PKMC' USING bdcdata MODE p_mode
                   MESSAGES INTO it_message.

ENDFORM.                    " bdc_delete
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bdc_dynpro_processing  USING   dy_begin  pg_name   sc_no.
  IF dy_begin = 'X'.
    CLEAR bdcdata.
    MOVE  pg_name  TO bdcdata-program.
    MOVE  sc_no    TO bdcdata-dynpro.
    MOVE  'X'      TO bdcdata-dynbegin.
    APPEND bdcdata.
  ELSE.
    CLEAR bdcdata.
    MOVE  pg_name  TO bdcdata-fnam.
    MOVE  sc_no    TO bdcdata-fval.
    APPEND bdcdata.
  ENDIF.
ENDFORM.                    " bdc_dynpro_processing
*&---------------------------------------------------------------------*
*&      Form  bdc_return
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bdc_return CHANGING c_icon
                         c_msg.
  READ TABLE it_message WITH KEY msgtyp = 'E'.  "error
  IF sy-subrc = 0.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              msgid               = it_message-msgid
              msgnr               = it_message-msgnr
              msgv1               = it_message-msgv1
              msgv2               = it_message-msgv2
              msgv3               = it_message-msgv3
              msgv4               = it_message-msgv4
         IMPORTING
              message_text_output = c_msg.

    c_icon  = icon_red.

  ELSE.
*    READ TABLE it_message WITH KEY msgtyp = 'S'.
*    IF sy-subrc = 0 AND it_list-line NE 'A'.
*      SELECT SINGLE pknum
*        FROM pkhd
*        INTO it_list-pknum
*       WHERE matnr = it_list-matnr
*         AND werks = it_list-werks
*         AND prvbe = it_list-prvbe.
*    ENDIF.
*    IF it_message[] IS INITIAL.
*      it_list-line = 'D'.
*      c_icon  = icon_gre.
*    ENDIF.
    c_icon  = icon_gre.

  ENDIF.
ENDFORM.                    " bdc_return
*&---------------------------------------------------------------------*
*&      Form  bdc_change
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bdc_change .
  DATA : l_kwbzm(20),
         l_behaz(20),
         l_safety(10),
         l_safetytime(20).
  WRITE: it_list-kwbzm   TO l_kwbzm NO-ZERO,
         it_list-behaz   TO l_behaz NO-ZERO,
         it_list-zzeisbe TO l_safety,
         it_list-zztim   TO l_safetytime NO-ZERO.

*# initial screen
  PERFORM bdc_dynpro_processing USING:
           'X' 'SAPLMPK_CCY_UI'       '0110',
           ' ' 'BDC_OKCODE'           '=TOGG',
           ' ' 'RMPKR-WERKS'          it_list-werks,
           ' ' 'RMPKR-PRVBE'          ' ',
           ' ' 'RMPKR-RGVER'          ' ',
           ' ' 'RMPKR-PKNUM'          it_list-pknum.
  PERFORM bdc_dynpro_processing USING:
           'X' 'SAPLMPK_CCY_UI'       '0110',
           ' ' 'BDC_OKCODE'           '=GETD'.
*# change data
  PERFORM bdc_dynpro_processing USING:
           'X' 'SAPLMPK_CCY_UI'       '0110',
           ' ' 'BDC_OKCODE'           '=OK',
           ' ' 'PKHD-PRVBE'           it_list-prvbe,
           ' ' 'PKHD-ABLAD'           it_list-ablad,
           ' ' 'PKHD-KWBZM'           l_kwbzm, "Replenishment Lead Time
           ' ' 'PKHD-ANZLT'           it_list-anzlt.

  IF it_list-rksta = 'K'.
    PERFORM bdc_dynpro_processing USING:
             ' ' 'PKHD-UMLGO'         it_list-umlgo,
             ' ' 'PKHD-BEHAZ'         l_behaz,         "Number of KANBAN
             ' ' 'PKHD-SIGAZ'         it_list-sigaz,   "Maximum Empty
             ' ' 'PKHD-KWBZM'         l_kwbzm,
"Replenishment Lead Time
             ' ' 'PKHD-ZFEEDER'       it_list-zfeeder,
             ' ' 'PKHD-ZRHLH'         it_list-zrhlh,
             ' ' 'PKHD-ZZFSTP'        it_list-ZZFSTP.
*  ELSEIF it_list-rksta = 'M'.
*    PERFORM bdc_dynpro_processing USING:
*             ' ' 'PKHD-ZZEISBE'       l_safety,        "Safety Stock
*             ' ' 'PKHD-ZZTIM'         l_safetytime.    "Safety Time
  ELSE.
    PERFORM bdc_dynpro_processing USING:
             ' ' 'PKHD-UMLGO'         it_list-umlgo,
             ' ' 'PKHD-ZZEISBE'       l_safety,        "Safety Stock
             ' ' 'PKHD-ZZTIM'         l_safetytime,    "Safety Time
             ' ' 'PKHD-ZFEEDER'       it_list-zfeeder,
             ' ' 'PKHD-ZRHLH'         it_list-zrhlh,
             ' ' 'PKHD-ZZFSTP'        it_list-ZZFSTP.
  ENDIF.

*flow sequence
  PERFORM bdc_dynpro_processing USING:
           ' ' 'PKHD-PKSFG'         it_list-pksfg.

  PERFORM bdc_dynpro_processing USING:
           'X' 'SAPLMPK_CCY_UI'       '0110',
           ' ' 'BDC_OKCODE'           '=SAVE'.
  CALL TRANSACTION 'PKMC' USING bdcdata MODE p_mode
                   MESSAGES INTO it_message.
ENDFORM.                    " bdc_change
*&---------------------------------------------------------------------*
*&      Form  user_command_copy
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command_copy .

  PERFORM get_selected_rows.
  REFRESH it_copy.
  LOOP AT it_list WHERE mark = 'X'.
    CLEAR it_copy.
    MOVE-CORRESPONDING it_list TO it_copy.
* Control Cycle Category
    IF it_list-rksta   = 'K'.      "Classic KANBAN
      it_copy-pkkla = 'X'.
    ELSEIF it_list-rksta   = 'I'.  "Event-driven KANBAN
      it_copy-pkimp = 'X'.
    ELSEIF it_list-rksta   = 'M'.  "SumJIT
      it_list-pksum  = 'X'.
    ENDIF.

    APPEND it_copy.
  ENDLOOP.
  IF sy-subrc = 0.
    MESSAGE s060(e2).
  ENDIF.

ENDFORM.                    " user_command_copy
*&---------------------------------------------------------------------*
*&      Form  bdc_insert
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bdc_insert .
  DATA: l_kwbzm(20),
        l_behaz(20),
        l_safety(10),
        l_safetytime(20).
  WRITE: it_list-kwbzm   TO l_kwbzm NO-ZERO,
         it_list-behaz   TO l_behaz NO-ZERO,
         it_list-zzeisbe TO l_safety,
         it_list-zztim   TO l_safetytime NO-ZERO.
  PERFORM bdc_dynpro_processing USING:
           'X' 'SAPLMPK_CCY_UI'       '0110',
           ' ' 'BDC_OKCODE'           '=TOGG',
           ' ' 'RMPKR-WERKS'          it_list-werks.
  PERFORM bdc_dynpro_processing USING:
           'X' 'SAPLMPK_CCY_UI'       '0110',
           ' ' 'BDC_OKCODE'           '=CCY_CRE'.
  PERFORM bdc_dynpro_processing USING:
           'X' 'SAPLMPK_CCY_UI'       '0120',
           ' ' 'BDC_OKCODE'           '=WEIT',
           ' ' 'RMPKR-MATNR'          it_list-matnr,
           ' ' 'RMPKR-PRVBE'          it_list-prvbe,
           ' ' 'RMPKR-PKKLA'          it_list-pkkla,  "classic
           ' ' 'RMPKR-PKIMP'          it_list-pkimp,  "event
           ' ' 'RMPKR-PKSUM'          it_list-pksum,
 "Manual Summarized JIT Calls
           ' ' 'RMPKR-PKASM'          it_list-pkasm.  "

  PERFORM bdc_dynpro_processing USING:
           'X' 'SAPLMPK_CCY_UI'       '0110',
           ' ' 'BDC_OKCODE'           '/00',
           ' ' 'RMPKR-STUML'          'X',             "stock transport
           ' ' 'PKHD-ABLAD'           it_list-ablad,   "Storing position
           ' ' 'PKHD-ANZLT'           it_list-anzlt.

  CASE it_list-rksta.
    WHEN 'M'.
      PERFORM bdc_dynpro_processing USING:
               ' ' 'RMPKR-STFRD'          'X',
"stock transport
               ' ' 'PKHD-SUMRST2'         '0001',
"Replenishment Strategy
               ' ' 'PKHD-PABPRF'          'JC01',
"JIT Call Profile
               ' ' 'PKHD-JITSCPRF'        'SP01'.
      "Sched.Profile
    WHEN 'A'.
      PERFORM bdc_dynpro_processing USING:
               ' ' 'RMPKR-STUML'          'X',
"stock transport
               ' ' 'PKHD-SUMRST3'         '0001',
"Replenishment Strategy
               ' ' 'PKHD-PABPRF'          'JC01',
"JIT Call Profile
               ' ' 'PKHD-JITSCPRF'        'SP01'.
      "Sched.Profile

    WHEN OTHERS.
      PERFORM bdc_dynpro_processing USING:
               ' ' 'RMPKR-STUML'          'X',
"stock transport
               ' ' 'PKHD-PKSTU'           '0001'.
      "Replenishment Strategy
  ENDCASE.
  IF it_list-rksta EQ 'K'.
    PERFORM bdc_dynpro_processing USING:
           ' ' 'PKHD-BEHAZ'           '2'. "Number of kanban containers
  ENDIF.

  PERFORM bdc_dynpro_processing USING:
         ' ' 'PKHD-ZFEEDER'       it_list-zfeeder,
         ' ' 'PKHD-ZRHLH'         it_list-zrhlh,
         ' ' 'PKHD-ZZFSTP'        it_list-ZZFSTP.

  PERFORM bdc_dynpro_processing USING:
           'X' 'SAPLMPK_CCY_UI'       '0110',
           ' ' 'BDC_OKCODE'           '/00',
           ' ' 'RMPKR-PACKV'          it_list-piid,    "Packing Object
           ' ' 'PKHD-KWBZM'           l_kwbzm.
  "Replenishment Lead Time

  CASE it_list-rksta.
    WHEN 'M'.
      PERFORM bdc_dynpro_processing USING:
               ' ' 'PKHD-EBELN'           it_list-ebeln,   "Agreement
               ' ' 'PKHD-EBELP'           it_list-ebelp.
      "Agreement Item

    WHEN 'A'.
      PERFORM bdc_dynpro_processing USING:
               ' ' 'PKHD-UMLGO'           it_list-umlgo.
    WHEN OTHERS.
      PERFORM bdc_dynpro_processing USING:
               ' ' 'PKHD-UMLGO'           it_list-umlgo,
"StorageLocation
               ' ' 'PKHD-KCPRF'           '0001'.
      "Kanban Calculation Profile
  ENDCASE.


  PERFORM bdc_dynpro_processing USING:
           'X' 'SAPLMPK_CCY_UI'       '0110',
           ' ' 'BDC_OKCODE'           '=SAVE'.

* pkkla,                       "Classic KANBAN
* pkimp,                       "Event-driven KANBAN
* pksum,                       "Control Cycle for Digital Bucket Order
  "to Vendor
* PKASM,                       "Control Cycle for Digital Bucket Order
  "to C.C
  IF it_list-pkkla = 'X'.
    PERFORM bdc_dynpro_processing USING:
           ' ' 'PKHD-BEHAZ'           l_behaz,
           "Number of kanban containers

           ' ' 'PKHD-SIGAZ'           it_list-sigaz,   "Maximum Empty
           ' ' 'PKHD-ZFEEDER'         it_list-zfeeder.

  ELSEIF it_list-pkimp = 'X'.
    PERFORM bdc_dynpro_processing USING:
           ' ' 'PKHD-ZZEISBE'         l_safety,       "Safety Stock
           ' ' 'PKHD-ZZTIM'           l_safetytime,   "Safety Time
           ' ' 'PKHD-ZFEEDER'         it_list-zfeeder.

  ELSE.
    PERFORM bdc_dynpro_processing USING:
           ' ' 'PKHD-ZZEISBE'         l_safety,       "Safety Stock
           ' ' 'PKHD-ZZTIM'           l_safetytime,   "Safety Time
           ' ' 'PKHD-ZFEEDER'         it_list-zfeeder.


  ENDIF.

  CALL TRANSACTION 'PKMC' USING bdcdata MODE p_mode
                   MESSAGES INTO it_message.

ENDFORM.                    " bdc_insert
*&---------------------------------------------------------------------*
*&      Form  find_packobj
*&---------------------------------------------------------------------*
*       Find Packing Instruction
*----------------------------------------------------------------------*
FORM find_packobj  USING    if_matnr
                   CHANGING ef_packnr
                            ef_pobjid.

  DATA: lf_rmatp         TYPE mara-rmatp.
  DATA: lt_pinst         TYPE pdt_t_packobj_found,
        ls_pinst         TYPE pdt_s_packobj_found,
        ls_paitemtype    TYPE pdt_ra_paitemtype,
        lt_ra_paitemtype TYPE pdt_t_ra_paitemtype.

  CLEAR: ef_packnr, ef_pobjid.
  CLEAR ls_paitemtype.
  ls_paitemtype-sign   = 'I'.
  ls_paitemtype-option = 'EQ'.
  ls_paitemtype-low    = 'R'.         "reference material
  APPEND ls_paitemtype TO lt_ra_paitemtype.

  SELECT SINGLE rmatp
    INTO lf_rmatp
    FROM mara
   WHERE matnr EQ if_matnr.

* Find Packing Object
  CALL FUNCTION 'VHUPODB_PACKOBJ_FIND'
       EXPORTING
            packtyp_imp          = 'P'
            range_paitemtype_imp = lt_ra_paitemtype
            matnr_imp            = lf_rmatp
            maxrecords_imp       = 1
       CHANGING
            packobj_tab          = lt_pinst
       EXCEPTIONS
            pos_without_head     = 1
            OTHERS               = 2.
  READ TABLE lt_pinst INTO ls_pinst INDEX 1.
  IF sy-subrc EQ 0.
    ef_packnr = ls_pinst-packnr.
    ef_pobjid = ls_pinst-pobjid.
  ENDIF.
ENDFORM.                    " find_packobj
*&---------------------------------------------------------------------*
*&      Form  get_pi_info
*&---------------------------------------------------------------------*
*       Get Packing Instruction Info.
*----------------------------------------------------------------------*
FORM get_pi_info USING    u_packv
                 CHANGING c_behmg
                          c_pkbht.
  DATA: ls_pibasedata LIKE pibasedata.

  CALL FUNCTION 'VHUPIAP_GET_PI_INFO'
       EXPORTING
            ip_packnr           = u_packv
       IMPORTING
            es_basedata         = ls_pibasedata
       EXCEPTIONS
            packinstr_not_found = 1
            packinstr_deleted   = 2
            OTHERS              = 3.

  c_behmg  = ls_pibasedata-t_trgqty.
  c_pkbht  = ls_pibasedata-loadcarr.

ENDFORM.                    " get_pi_info
*&---------------------------------------------------------------------*
*&      Form  read_packobj_ident
*&---------------------------------------------------------------------*
*       Read Packing Object Identification
*----------------------------------------------------------------------*
FORM read_packobj_ident  USING    if_packnr
                         CHANGING ef_piid.
  DATA : pobjid_exp   LIKE  packkp-pobjid,
         packtyp_exp  LIKE  packkp-packtyp,
         content_exp  LIKE  packkps-content.

  CALL FUNCTION 'VHUPODB_PACKOBJ_READ_IDENT'
       EXPORTING
            packnr_imp  = if_packnr
       IMPORTING
            pobjid_exp  = pobjid_exp
            packtyp_exp = packtyp_exp
            content_exp = content_exp.

  IF packtyp_exp = 'P'.
    ef_piid = pobjid_exp.
  ENDIF.
ENDFORM.                    " read_packobj_ident
*&---------------------------------------------------------------------*
*&      Form  user_command_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command_new .
  CLEAR rmpkr.
  rmpkr-werks = p_werks.
  CALL SCREEN 110 STARTING AT 5 5.
ENDFORM.                    " user_command_new
*&---------------------------------------------------------------------*
*&      Form  user_command_mass
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command_mass .
  CLEAR zmms0155.

  IF g_mode = 'M'.
    READ TABLE it_list WITH KEY mark = 'X'.

    IF sy-subrc EQ 0.
      CALL SCREEN 120 STARTING AT 5 5.
    ELSE.
      MESSAGE i009(zmmm) WITH text-m10.
    ENDIF.
  ELSE.
    MESSAGE i009(zmmm) WITH text-m20.
  ENDIF.

ENDFORM.                    " user_command_mass

*&---------------------------------------------------------------------*
*&      Form  handle_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_f4  USING    u_fieldname
                         u_fieldvalue
                         u_row_no
                         u_event_data
                         u_bad_cells
                         u_display.
  CASE u_fieldname.
    WHEN 'KCPRF'.
      PERFORM f4_kcprf.
  ENDCASE.
ENDFORM.                                                    " handle_f4
*&---------------------------------------------------------------------*
*&      Form  func_conversion_exit_matn1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM func_conversion_exit_matn1  USING    u_in
                                 CHANGING c_out.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
       EXPORTING
            input  = u_in
       IMPORTING
            output = c_out.

ENDFORM.                    " func_conversion_exit_matn1
*&---------------------------------------------------------------------*
*&      Form  f4_field_assign
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f4_field_assign .
  CLEAR gs_f4.
  gs_f4-fieldname  = 'KCPRF'.
  gs_f4-register   = 'X'.
  APPEND gs_f4 TO gt_f4.

  CLEAR gs_f4.
  gs_f4-fieldname  = 'ZFEEDER'.
  gs_f4-register   = 'X'.
  APPEND gs_f4 TO gt_f4.

  CALL METHOD g_grid1->register_f4_for_fields
    EXPORTING
      it_f4 = gt_f4.
ENDFORM.                    " f4_field_assign
*&---------------------------------------------------------------------*
*&      Module  status_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0120 OUTPUT.
  SET PF-STATUS '120'.
  SET TITLEBAR '100' WITH text-102.
ENDMODULE.                 " status_0200  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  status_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS '110'.
  SET TITLEBAR '100' WITH text-102.
ENDMODULE.                 " status_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  user_command_enter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command_enter .
  CLEAR it_list.
  it_list-werks   = rmpkr-werks.
  it_list-matnr   = rmpkr-matnr.
  it_list-maktx   = rmpkr-maktx.
  it_list-prvbe   = rmpkr-prvbe.
  it_list-pvbtx   = rmpkr-pvbtx.
  it_list-pkstu   = '0001'.
  it_list-line    = 'N'.

  PERFORM find_packobj USING    it_list-matnr
                       CHANGING it_list-packv
                                it_list-piid.
  PERFORM get_pi_info USING    it_list-packv
                      CHANGING it_list-behmg
                               it_list-pkbht.

  SELECT SINGLE meins
    FROM mara
    INTO it_list-meins
   WHERE matnr = it_list-matnr.

* Control Cycle Category
  CASE 'X'.
    WHEN rmpkr-pkkla.
      it_list-rksta   = 'K'.  "Classic KANBAN
      it_list-pkkla   = 'X'.
    WHEN rmpkr-pkimp.
      it_list-rksta   = 'I'.  "Event-driven KANBAN
      it_list-pkimp   = 'X'.
    WHEN rmpkr-pksum.
      it_list-rksta   = 'M'.
      " Manual Generation of Summarized JIT Calls
      it_list-pksum   = 'X'.
    WHEN rmpkr-pkasm.
      it_list-rksta   = 'A'.
      " Control Cycle for Digital Bucket Order to Vendor
      it_list-pkasm   = 'X'.
  ENDCASE.

*   Find Scheduling Agreement
  IF it_list-rksta EQ 'M'.
    PERFORM find_sa(zmmr61700t)
                    USING    it_list-matnr
                             it_list-werks
                    CHANGING it_list-ebeln
                             it_list-ebelp.
  ENDIF.

  INSERT it_list INDEX 1.
ENDFORM.                    " user_command_enter
*&---------------------------------------------------------------------*
*&      Module  check_matnr  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_matnr INPUT.
  PERFORM read_text USING    'MAKTX'
                             rmpkr-matnr
                    CHANGING rmpkr-maktx.
  CHECK rmpkr-maktx IS INITIAL.
  SET CURSOR FIELD 'RMPKR-MATNR'.
  MESSAGE e002(pk) WITH rmpkr-matnr rmpkr-werks.
ENDMODULE.                 " check_matnr  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_prvbe  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_prvbe INPUT.
  PERFORM read_text USING    'PVBTX'
                             rmpkr-prvbe
                    CHANGING rmpkr-pvbtx.
  CHECK rmpkr-pvbtx IS INITIAL.
  SET CURSOR FIELD 'RMPKR-PRVBE'.
  MESSAGE e003(pk) WITH rmpkr-prvbe.
ENDMODULE.                 " check_prvbe  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_duplication  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_duplication INPUT.

  SELECT SINGLE *
    FROM pkhd
   WHERE matnr = rmpkr-matnr
     AND werks = rmpkr-werks
     AND prvbe = rmpkr-prvbe.
  CHECK sy-subrc = 0.
  SET CURSOR FIELD 'RMPKR-MATNR'.
  MESSAGE e009(pk) WITH rmpkr-matnr rmpkr-werks rmpkr-prvbe.

ENDMODULE.                 " check_duplication  INPUT
*&---------------------------------------------------------------------*
*&      Form  input_check_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM input_check_material .
  SELECT SINGLE meins
    FROM mara
    INTO it_list-meins
   WHERE matnr = it_list-matnr.
  IF sy-subrc = 0.
    PERFORM read_text USING    'MAKTX'
                                it_list-matnr
                      CHANGING  it_list-maktx.
    PERFORM find_packobj USING    it_list-matnr
                         CHANGING it_list-packv
                                  it_list-piid.
    PERFORM get_pi_info USING    it_list-packv
                        CHANGING it_list-behmg
                                 it_list-pkbht.
  ELSE.
*  MESSAGE e002(pk) WITH it_list-matnr it_list-werks.
    gs_class_msg-msgid = 'PK'.
    gs_class_msg-msgno = '002'.
    gs_class_msg-msgv1 = it_list-matnr .
    gs_class_msg-msgv2 = it_list-werks.
    error_in_data = 'X'.
  ENDIF.
ENDFORM.                    " input_check_material
*&---------------------------------------------------------------------*
*&      Form  input_check_supply_area
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM input_check_supply_area .
  PERFORM read_text USING    'PVBTX'
                             it_list-prvbe
                    CHANGING it_list-pvbtx.
  CHECK it_list-pvbtx IS INITIAL.
*  MESSAGE e003(pk) WITH it_list-prvbe.
  gs_class_msg-msgid = 'PK'.
  gs_class_msg-msgno = '003'.
  gs_class_msg-msgv1 = it_list-prvbe .
  error_in_data = 'X'.
ENDFORM.                    " input_check_supply_area
*&---------------------------------------------------------------------*
*&      Form  input_check_location
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM input_check_location .

  SELECT SINGLE *
    FROM mard
   WHERE matnr = it_list-matnr
     AND werks = it_list-werks
     AND lgort = it_list-umlgo.

  CHECK sy-subrc NE 0.
*  MESSAGE e040(pk) WITH it_list-matnr it_list-umlgo.
  gs_class_msg-msgid = 'PK'.
  gs_class_msg-msgno = '040'.
  gs_class_msg-msgv1 = it_list-matnr  .
  gs_class_msg-msgv2 = it_list-umlgo  .
  error_in_data = 'X'.

ENDFORM.                    " input_check_location
*&---------------------------------------------------------------------*
*&      Form  input_check_feeder
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM input_check_feeder .

  SELECT SINGLE *
    FROM zmmt0087
   WHERE zfeeder = it_list-zfeeder.

  CHECK sy-subrc NE 0.
*  MESSAGE e040(pk) WITH it_list-matnr it_list-umlgo.
  gs_class_msg-msgid = '/SAPNEA/J_SC'.
  gs_class_msg-msgno = '037'.
  gs_class_msg-msgv1 = it_list-matnr  .
  gs_class_msg-msgv2 = it_list-zfeeder  .
  error_in_data = 'X'.

ENDFORM.                    " input_check_location
*&---------------------------------------------------------------------*
*&      Form  bdc_insert_fieldcheck
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bdc_insert_fieldcheck .

  IF it_list-matnr = space OR it_list-prvbe = space.
    it_message-msgtyp  = 'E'.
    it_message-msgid   = 'PK'.
    it_message-msgnr   = '274'.
    APPEND it_message.
  ENDIF.
ENDFORM.                    " bdc_insert_fieldcheck
*&---------------------------------------------------------------------*
*&      Form  user_command_paste
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command_paste .

  DATA : lt_rows TYPE lvc_t_row,
         l_row   TYPE lvc_s_row,
         l_index     TYPE sy-tabix.

* get select line
  CALL METHOD g_grid1->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  READ TABLE lt_rows INTO l_row INDEX 1.
  l_index = l_row-index.
  LOOP AT it_copy.
    l_index = l_index + 1.
    CLEAR it_list.
    MOVE-CORRESPONDING it_copy TO it_list.
    CLEAR it_list-pknum.
    it_list-line = 'N'.
    CLEAR it_list-celltab.
    PERFORM fill_celltab USING    it_list-line
                                  it_list-rksta
                         CHANGING it_list-celltab[].

    INSERT it_list INDEX l_index.
  ENDLOOP.

ENDFORM.                    " user_command_paste
*&---------------------------------------------------------------------*
*&      Form  user_command_sortup
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command_sort USING u_updown.
  DATA : et_index_columns  TYPE  lvc_t_col,
         l_columns         TYPE  lvc_s_col.


  CALL METHOD g_grid1->get_selected_columns
    IMPORTING
      et_index_columns = et_index_columns.

  READ TABLE et_index_columns INDEX 1 INTO l_columns.
  CHECK sy-subrc = 0.
  IF u_updown = 'U'.
    SORT it_list BY (l_columns-fieldname) ASCENDING.
  ELSEIF u_updown = 'D'.
    SORT it_list BY (l_columns-fieldname) DESCENDING.
  ENDIF.

ENDFORM.                    " user_command_sortup
*&---------------------------------------------------------------------*
*&      Form  user_command_upload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command_upload .
  DATA : LV_LINE TYPE I.

  PERFORM get_file_path.
  CHECK filename NE space.
  PERFORM upload_local_file.
*  CHECK it_excel[] IS NOT INITIAL.
*  READ TABLE it_excel[] INDEX 1.
  DESCRIBE TABLE it_excel LINES LV_LINE.
  IF LV_LINE <> 0.
    PERFORM uploaded_data_input_check.
  ENDIF.
ENDFORM.                    " user_command_upload
*&---------------------------------------------------------------------*
*&      Form  get_file_path
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_file_path .
  CLEAR : filename, filefilter, path , fullpath , user_action.
  "file_encoding .

  CALL FUNCTION 'F4_FILENAME'
   EXPORTING
     program_name        = sy-cprog
     dynpro_number       = sy-dynnr
*   FIELD_NAME          = ' '
   IMPORTING
     file_name           = filename .

ENDFORM.                    " get_upload_file
*&---------------------------------------------------------------------*
*&      Form  upload_local_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM upload_local_file .
  REFRESH it_excel.
  CLEAR it_excel.

  CALL FUNCTION 'Z_MM_EXCEL_UPLOAD'
       EXPORTING
            filename   = filename
            itab       = 'IT_EXCEL'
            begin_line = 2
       TABLES
            outab      = it_excel.

ENDFORM.                    " upload_local_file
*&---------------------------------------------------------------------*
*&      Form  uploaded_data_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM uploaded_data_input_check .
  DATA : wa_list LIKE it_list .
  DATA : l_index  TYPE sy-tabix,
         l_kwbzm(11)  TYPE n .

  LOOP AT it_excel.
    CLEAR : it_list, wa_list.
    MOVE-CORRESPONDING it_excel TO wa_list.


*--> check control cycle category
    PERFORM check_category USING    wa_list-rksta
                           CHANGING wa_list-icon
                                    wa_list-msg.

    IF wa_list-icon = space.

      PERFORM func_conv_matn1 USING    it_excel-matnr
                              CHANGING wa_list-matnr.

*--> check existing KANBAN master.
      SELECT SINGLE pknum
        FROM pkhd
        INTO wa_list-pknum
       WHERE matnr = wa_list-matnr
         AND werks = wa_list-werks
         AND prvbe = wa_list-prvbe.

      IF sy-subrc = 0.      "========duplicated KANBAN
        wa_list-icon   = icon_red.
        CLEAR wa_list-line.
        PERFORM func_message_text_build USING    'PK'
                                                 '009'
                                                 wa_list-matnr
                                                 wa_list-werks
                                                 wa_list-prvbe
                                                 ''
                                        CHANGING wa_list-msg.

      ELSE.       "===================New KANBAN
        READ TABLE it_list WITH KEY matnr = wa_list-matnr
                                    werks = wa_list-werks
                                    prvbe = wa_list-prvbe
                                    line  = 'N'.
        IF sy-subrc NE 0.   "==============Insert KANBAN
* Control Cycle Category
          IF it_excel-rksta = 'I'.      "event
            wa_list-pkimp   = 'X'.      "event
          ELSEIF it_excel-rksta = 'K'.  "Classic
            wa_list-pkkla   = 'X'.      "Classic
          ELSEIF it_excel-rksta = 'M'.  " Mannual Sumjit
            wa_list-pksum   = 'X'.
          ELSEIF it_excel-rksta = 'A'.  " Auto. Sumjit
            wa_list-pkasm   = 'X'.
          ENDIF.

          wa_list-icon = icon_pos.
          wa_list-line = 'N'.
        ELSE.   "===================== duplication KANBAN
          wa_list-icon   = icon_red.
          CLEAR wa_list-line.
          PERFORM func_message_text_build USING    'PK'
                                                    '009'
                                                    wa_list-matnr
                                                    wa_list-werks
                                                    wa_list-prvbe
                                                    ''
                                           CHANGING wa_list-msg.
        ENDIF.
      ENDIF.

      IF wa_list-line NE space.
        PERFORM find_packobj USING    wa_list-matnr
                             CHANGING wa_list-packv
                                      wa_list-piid.
        PERFORM get_pi_info  USING    wa_list-packv
                             CHANGING wa_list-behmg
                                      wa_list-pkbht.

        SELECT SINGLE meins INTO wa_list-meins
          FROM mara
         WHERE matnr = wa_list-matnr.

* text
        PERFORM read_text USING     'MAKTX'
                                    wa_list-matnr
                          CHANGING  wa_list-maktx.
        PERFORM read_text USING     'PVBTX'
                                    wa_list-prvbe
                          CHANGING  wa_list-pvbtx.

* Replenishment Lead Time in Hours:Minutes
        IF it_excel-kwbzm NE SPACE. "IS NOT INITIAL.
          CLEAR wa_list-kwbzm.
          PERFORM func_conversion_tstrn_input USING    it_excel-kwbzm
                                              CHANGING wa_list-kwbzm.
        ENDIF.
      ENDIF.
    ENDIF.
    wa_list-pkstu = '0001'.
    wa_list-kcprf = '0001'.
    APPEND wa_list TO it_list.
  ENDLOOP.

ENDFORM.                    " uploaded_data_input
*&---------------------------------------------------------------------*
*&      Form  func_MESSAGE_TEXT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM func_message_text_build  USING    u_msgid
                                       u_msgnr
                                       u_msgv1
                                       u_msgv2
                                       u_msgv3
                                       u_msgv4
                              CHANGING c_msg.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            msgid               = u_msgid
            msgnr               = u_msgnr
            msgv1               = u_msgv1
            msgv2               = u_msgv2
            msgv3               = u_msgv3
            msgv4               = u_msgv4
       IMPORTING
            message_text_output = c_msg.

ENDFORM.                    " func_MESSAGE_TEXT_BUILD
*&---------------------------------------------------------------------*
*&      Form  func_conv_matn1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM func_conv_matn1  USING    u_matnr
                      CHANGING c_matnr.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
       EXPORTING
            input  = u_matnr
       IMPORTING
            output = c_matnr.

ENDFORM.                    " func_conv_matn1
*&---------------------------------------------------------------------*
*&      Form  link_control_cycle
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM link_control_cycle USING u_pknum.
  CALL FUNCTION 'PK_DISPLAY_CCY'
       EXPORTING
            pknum_iv = u_pknum.

ENDFORM.                    " link_control_cycle
*&---------------------------------------------------------------------*
*&      Form  clear_table_after_save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM clear_table_after_save .
*  CLEAR : it_list-mark, it_list-line.
*  MODIFY it_list TRANSPORTING mark LINE where mark = 'X'
*                                           OR LINE ne space.
  REFRESH : it_copy, it_excel.
  CLEAR   : it_copy, it_excel.
ENDFORM.                    " clear_table_after_save
*&---------------------------------------------------------------------*
*&      Form  f4_KCPRF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f4_kcprf .
  DATA : l_line TYPE sy-tabix.
  DATA : ct_cells    TYPE lvc_t_cell,
          l_cell     TYPE lvc_s_cell.

  REFRESH : table_fields, entries_char, f4_kcprf, field_tab.

*--> Read Dynpro Value from Screen.
  CALL METHOD g_grid1->get_selected_cells
    IMPORTING
      et_cell = ct_cells.

  READ TABLE ct_cells INTO l_cell INDEX 1.
  READ TABLE it_list INDEX l_cell-row_id-index.

  SELECT *
    FROM tpkpt
    INTO TABLE f4_kcprf
   WHERE spras = sy-langu
     AND werks = it_list-werks.

  IF sy-subrc = 0.
*--> pop up
    PERFORM f4_kcprf_fieldcat.
    PERFORM f4_kcprf_popup USING l_cell-row_id-index.

  ELSE.
*    MESSAGE i009(zz) WITH text-001.
  ENDIF.

ENDFORM.                                                    " f4_KCPRF
*&---------------------------------------------------------------------*
*&      Form  f4_kcprf_popup
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f4_kcprf_popup  USING   i_row_id.
  DATA : es_selfield   TYPE slis_selfield,
         data_protocol TYPE REF TO cl_alv_changed_data_protocol.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
       EXPORTING
            i_selection = 'X'
            i_tabname   = 'F4_KCPRF'
            it_fieldcat = it_fieldcat2
       IMPORTING
            es_selfield = es_selfield
       TABLES
            t_outtab    = f4_kcprf.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
** selecting value in pop-up screen
    READ TABLE f4_kcprf INDEX es_selfield-tabindex.

* modify cell
    CHECK f4_kcprf-kcprf NE SPACE. "IS NOT INITIAL.
    CHECK it_list-line NE space .
    it_list-kcprf = f4_kcprf-kcprf.
    IF it_list-line = 'C'.
      it_list-line  = 'S'.
    ENDIF.
    CHECK sy-subrc = 0.
    MODIFY it_list INDEX i_row_id.
    PERFORM gui_alv_refresh.
  ENDIF.
ENDFORM.                    " f4_kcprf_popup
*&---------------------------------------------------------------------*
*&      Form  f4_kcprf_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f4_kcprf_fieldcat .
  DATA : l_fieldcat TYPE slis_fieldcat_alv.

  REFRESH it_fieldcat2.

  l_fieldcat-fieldname             = 'KCPRF'.
  l_fieldcat-ref_tabname           = 'TPKPT'.
  APPEND l_fieldcat TO it_fieldcat2.

  l_fieldcat-fieldname             = 'KCPRT'.
  l_fieldcat-ref_tabname           = 'TPKPT'.
  APPEND l_fieldcat TO it_fieldcat2.

ENDFORM.                    " f4_kcprf_fieldcat
*&---------------------------------------------------------------------*
*&      Form  user_command_download
*&---------------------------------------------------------------------*
*       case u_table
*       'T' = template
*       'S' = screen data
*----------------------------------------------------------------------*
FORM user_command_download USING u_table.
  PERFORM get_file_path.
  CHECK filename NE space.
  PERFORM make_excel_head.
  IF u_table = 'S'.
    PERFORM make_excel_item.
  ENDIF.
  PERFORM download_excel.
ENDFORM.                    " user_command_download
*&---------------------------------------------------------------------*
*&      Form  make_excel_head
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM make_excel_head  .
  REFRESH excel_down.
  CLEAR excel_down.

  excel_down-matnr      = 'Material'.
  excel_down-werks      = 'Plant'.
  excel_down-prvbe      = 'Supply Area'.
  excel_down-rksta      = 'Category'.
  excel_down-behaz      = 'Number of kanban'.
  excel_down-ablad      = 'Storing position'.
  excel_down-sigaz      = 'Maximum empty'.
  excel_down-umlgo      = 'Storage Location'.
  excel_down-kwbzm      = 'Rep. Lead Time'.
  excel_down-zzeisbe    = 'Safety Stock'.
  excel_down-zztim      = 'Safety Time'.
  excel_down-zfeeder    = 'Feeder'.
  excel_down-anzlt      = 'Number of Load Carriers'.

  APPEND excel_down.

ENDFORM.                    " make_excel_head
*&---------------------------------------------------------------------*
*&      Form  download_excel
*&---------------------------------------------------------------------*
*       download excel
*----------------------------------------------------------------------*
FORM download_excel .
  DATA : l_filename   TYPE  string.
  l_filename = filename.
  CALL FUNCTION 'GUI_DOWNLOAD'
       EXPORTING
            filename                = l_filename
            filetype                = 'DAT'
       TABLES
            data_tab                = excel_down
       EXCEPTIONS
            file_write_error        = 1
            no_batch                = 2
            gui_refuse_filetransfer = 3
            invalid_type            = 4
            no_authority            = 5
            unknown_error           = 6
            header_not_allowed      = 7
            separator_not_allowed   = 8
            filesize_not_allowed    = 9
            header_too_long         = 10
            dp_error_create         = 11
            dp_error_send           = 12
            dp_error_write          = 13
            unknown_dp_error        = 14
            access_denied           = 15
            dp_out_of_memory        = 16
            disk_full               = 17
            dp_timeout              = 18
            file_not_found          = 19
            dataprovider_exception  = 20
            control_flush_error     = 21
            OTHERS                  = 22.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " download_excel
*&---------------------------------------------------------------------*
*&      Form  func_conversion_tstrn_input
*&---------------------------------------------------------------------*
*       CALL FUNCTION 'CONVERSION_EXIT_TSTRN_INPUT'
*----------------------------------------------------------------------*
FORM func_conversion_tstrn_input  USING    u_kwbzm
                                  CHANGING c_kwbzm.
  CALL FUNCTION 'CONVERSION_EXIT_TSTRN_INPUT'
       EXPORTING
            input          = u_kwbzm
       IMPORTING
            output         = c_kwbzm
       EXCEPTIONS
            invalid_format = 1
            OTHERS         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " func_conversion_tstrn_input
*&---------------------------------------------------------------------*
*&      Form  handle_after_user_command
*&---------------------------------------------------------------------*
*       Not used in this program
*----------------------------------------------------------------------*
FORM handle_after_user_command  USING    p_e_ucomm
                                         p_e_not_processed.

ENDFORM.                    " handle_after_user_command
*&---------------------------------------------------------------------*
*&      Form  make_excel_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM make_excel_item .
  CLEAR excel_down.

  LOOP AT it_list.
    CLEAR excel_down.
    MOVE-CORRESPONDING it_list TO excel_down.
    WRITE it_list-kwbzm TO excel_down-kwbzm NO-ZERO.
    APPEND excel_down.
  ENDLOOP.

ENDFORM.                    " make_excel_item
*&---------------------------------------------------------------------*
*&      Form  gui_alv_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gui_alv_sort .
  DATA : l_sort TYPE lvc_s_sort.

  REFRESH gt_sort.
  CLEAR l_sort.
  l_sort-spos       = 1.
  l_sort-fieldname  = 'PKNUM'.
  l_sort-up         = 'X'.
  l_sort-group      = 'X'.
  APPEND l_sort TO gt_sort.

ENDFORM.                    " gui_alv_sort
*&---------------------------------------------------------------------*
*&      Form  remove_mark_in_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM remove_mark_in_screen .
  LOOP AT it_list WHERE mark = 'X'.
    CLEAR it_list-mark.
    MODIFY it_list.
  ENDLOOP.
ENDFORM.                    " remove_mark_in_screen
*&---------------------------------------------------------------------*
*&      Form  check_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_category  USING    u_rksta
                     CHANGING c_icon
                              c_msg.
  DATA : i_domname    LIKE  dd07v-domname,
         i_domvalue   LIKE  dd07v-domvalue_l.

  i_domname   = 'RKSTA'.
  i_domvalue  = u_rksta.

  CALL FUNCTION 'DOMAIN_VALUE_GET'
    EXPORTING
      i_domname        = i_domname
      i_domvalue       = i_domvalue
* IMPORTING
*   E_DDTEXT         =
   EXCEPTIONS
     not_exist        = 1
     OTHERS           = 2
            .
  IF sy-subrc <> 0.
    c_icon = icon_red.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO c_msg.
  ENDIF.


ENDFORM.                    " check_category
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_CONT
*&---------------------------------------------------------------------*
FORM user_command_cont .

  it_list-line = 'S'.

  IF NOT zmms0155-prvbe IS INITIAL.
    it_list-prvbe   = zmms0155-prvbe.
    MODIFY it_list TRANSPORTING prvbe
                                LINE
                   where werks = p_werks
                     AND mark  = 'X'.
  ENDIF.
  IF NOT zmms0155-ablad IS INITIAL.
    it_list-ablad   = zmms0155-ablad.
    MODIFY it_list TRANSPORTING ablad
                                LINE
                   where werks = p_werks
                     AND mark  = 'X'.
  ENDIF.
  IF NOT zmms0155-kwbzm IS INITIAL.
    it_list-kwbzm   = zmms0155-kwbzm.
    MODIFY it_list TRANSPORTING kwbzm
                                LINE
                   where werks = p_werks
                     AND mark  = 'X'.
  ENDIF.
  IF NOT zmms0155-umlgo IS INITIAL.
    it_list-umlgo   = zmms0155-umlgo.
    MODIFY it_list TRANSPORTING umlgo
                                LINE
                   where werks = p_werks
                     AND mark  = 'X'.
  ENDIF.
  IF NOT zmms0155-sigaz IS INITIAL.
    it_list-sigaz   = zmms0155-sigaz.
    MODIFY it_list TRANSPORTING sigaz
                                LINE
                   where werks = p_werks
                     AND mark  = 'X'.
  ENDIF.
  IF NOT zmms0155-behaz IS INITIAL.
    it_list-behaz   = zmms0155-behaz.
    MODIFY it_list TRANSPORTING behaz
                                LINE
                   where werks = p_werks
                     AND mark  = 'X'.
  ENDIF.
  IF NOT zmms0155-zzeisbe IS INITIAL.
    it_list-zzeisbe = zmms0155-zzeisbe.
    MODIFY it_list TRANSPORTING zzeisbe
                                LINE
                   where werks = p_werks
                     AND mark  = 'X'.
  ENDIF.
  IF NOT zmms0155-zztim IS INITIAL.
    it_list-zztim   = zmms0155-zztim.
    MODIFY it_list TRANSPORTING zztim
                                LINE
                   where werks = p_werks
                     AND mark  = 'X'.
  ENDIF.
  IF NOT zmms0155-zfeeder IS INITIAL.
    it_list-zfeeder = zmms0155-zfeeder.
    MODIFY it_list TRANSPORTING zfeeder
                                LINE
                   where werks = p_werks
                     AND mark  = 'X'.
  ENDIF.


ENDFORM.                    " USER_COMMAND_CONT
*&---------------------------------------------------------------------*
*&      Module  CHECK_CHANGE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_change INPUT.


ENDMODULE.                 " CHECK_CHANGE  INPUT


*&---------------------------------------------------------------------*
*&      Form  fill_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_color   USING p_fieldname
               CHANGING pt_color  TYPE lvc_t_scol.

  DATA :  ls_color   TYPE lvc_s_scol.

  ls_color-fname     = p_fieldname.
  ls_color-color-col = 6.
  ls_color-color-int = 0.
  INSERT ls_color INTO TABLE pt_color.

ENDFORM.                    " fill_color_200
*&---------------------------------------------------------------------*
*&      Form  ADJUST_CONTROL_CYCLE
*&---------------------------------------------------------------------*
FORM adjust_control_cycle .

** changed data / inserted data
  LOOP AT it_list WHERE mark = 'X'
                    AND icon = icon_red_light.

    it_list-prvbe = it_list-vspvb.
    it_list-piid  = it_list-rmatp.

    IF NOT it_list-eprio IS INITIAL.
      it_list-umlgo  = it_list-eprio.
    ELSE.
      it_list-umlgo  = it_list-lgfsb.
    ENDIF.

    it_list-ablad = it_list-lgpbe.

    REFRESH : bdcdata, it_message.
    CLEAR : bdcdata, it_message, it_list-icon, it_list-msg.

    PERFORM bdc_change.

    PERFORM bdc_return CHANGING it_list-icon
                                it_list-msg.
    IF it_list-icon = icon_gre.
      CLEAR : it_list-line, it_list-mark.
    ENDIF.
    MODIFY it_list.
  ENDLOOP.

  IF sy-subrc NE 0.
    MESSAGE s314(sls).
  ELSE.
    g_mode = 'D'.
    PERFORM clear_table_after_save.
    PERFORM gui_alv_cell_control.
  ENDIF.
ENDFORM.                    " ADJUST_CONTROL_CYCLE
*&---------------------------------------------------------------------*
*&      Form  read_key_master
*&---------------------------------------------------------------------*
FORM read_key_master.

  SELECT * into table it_pvkt
    FROM pvkt
   WHERE werks = p_werks
     AND spras = sy-langu.
  sort it_pvkt by prvbe werks.

  SELECT * into table it_87
    FROM zmmt0087.
  sort it_87 by ZFEEDER.


ENDFORM.                    " read_key_master
