************************************************************************
* Program Name      : ZRMMPM27_MATERIAL_PRICE
* Author            : hj.song
* Creation Date     : 2003.11.19
* Specifications By : hj.song
* Development Request No : UD1K902172
* Addl Documentation:
* Description       : Material Price status and GR qty
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.19.     hj.song          UD1K902172     Initial Coding
*
*
************************************************************************

*&---------------------------------------------------------------------*
*& Report  ZRMMPM27_MATERIAL_PRICE                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT  zrmmpm27_material_price MESSAGE-ID zmmm.

*** include
INCLUDE zrmmpm27_material_price_top.

*** start
START-OF-SELECTION.
* read data
  PERFORM select_data.
  PERFORM modify_data.
* set alv parameters
  PERFORM alv_field_build.

*** end
END-OF-SELECTION.
  PERFORM list_display.
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       read basic data
*----------------------------------------------------------------------*
FORM select_data.
*!!! display by material number level(no plant level)!!!*
* source of data ->  eord(Purchasing Source List)
*                    a018(Material Info Record)
* 1. vendor code space     :
*      ascending by lifnr,
*      first vendor display on list and next vendor display on reference
* 2. vendor code not space :
*      input vendor display on list
*      and next vendor display on reference except input vendor

  DATA  : lw_lines  TYPE sy-index,
          lw_index  TYPE sy-index.
  CLEAR : it_a018[], it_a018,
          it_list[], it_list,
          it_mseg[], it_mseg,
          it_eord[], it_eord,
          lw_index,  lw_lines.

* read source list
  SELECT * INTO TABLE it_eord
         FROM eord
        WHERE matnr  EQ  p_matnr
        AND   lifnr  IN  s_lifnr
        AND   ekorg  EQ  p_ekorg.
*        AND   ebeln  NE  space.     " <= add by stlim (2004/04/01)
  IF   sy-subrc EQ 0.
    SORT it_eord BY lifnr.
    DESCRIBE TABLE it_eord LINES lw_lines.
    lw_index = 2.
    DELETE it_eord FROM lw_index TO lw_lines.
* read info record
    SELECT * INTO TABLE it_a018
           FROM a018
           FOR ALL ENTRIES IN it_eord
          WHERE kappl  EQ  'M'     "Purchasing
          AND   kschl  EQ  'PB00'  "Gross price
          AND   lifnr  EQ  it_eord-lifnr
          AND   matnr  EQ  it_eord-matnr
          AND   ekorg  EQ  it_eord-ekorg.
    IF sy-subrc NE 0.
      MESSAGE s027. STOP.
    ENDIF.
  ELSE.
    MESSAGE s015. STOP.
  ENDIF.
  LOOP AT it_a018 INTO wa_a018.
    MOVE-CORRESPONDING wa_a018 TO it_list.

* get condition
    SELECT SINGLE a~kzust
                  a~kosrt
                  a~erdat
                  b~kbetr
                  b~konwa
           INTO CORRESPONDING FIELDS OF it_list
           FROM konh AS a INNER JOIN konp AS b
             ON a~mandt  EQ  b~mandt
            AND a~knumh  EQ  b~knumh
        WHERE   a~knumh  EQ  wa_a018-knumh
          AND   a~kappl  EQ  wa_a018-kappl
          AND   b~kschl  EQ  'PB00'.  "Gross price

* get gr qty
    PERFORM get_gr_qty USING wa_a018.
    READ TABLE it_mseg WITH KEY matnr = wa_a018-matnr
                                datab = wa_a018-datab.
    IF sy-subrc EQ 0.
      it_list-erfmg_v = it_mseg-erfmg_v.
      it_list-erfmg_k = it_mseg-erfmg_k.
    ENDIF.
* read vendor text
    PERFORM read_vendor_text USING wa_a018-lifnr it_list-name1.
    COLLECT it_list. CLEAR it_list.
  ENDLOOP.

* get next vendor
  PERFORM get_next_vendor.

ENDFORM.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  read_lgort_text
*&---------------------------------------------------------------------*
*       read storage location text
*----------------------------------------------------------------------*
FORM read_lgort_text USING    p_werks    p_lgort    p_lgobe.

  SELECT SINGLE lgobe INTO p_lgobe
         FROM t001l
        WHERE  werks = p_werks
        AND    lgort = p_lgort.
  IF sy-subrc NE 0.
    CLEAR p_lgobe.
  ENDIF.

ENDFORM.                    " read_lgort_text
*&---------------------------------------------------------------------*
*&      Form  read_lifnr_text
*&---------------------------------------------------------------------*
*        read vendor text
*----------------------------------------------------------------------*
FORM read_lifnr_text USING    p_lifnr   p_name1.

  SELECT SINGLE name1 INTO p_name1
         FROM lfa1
        WHERE lifnr = p_lifnr.
  IF sy-subrc NE 0.
    CLEAR p_name1.
  ENDIF.

ENDFORM.                    " read_lifnr_text
*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo OUTPUT.

*  SET PF-STATUS 'MAIN100'.
*  IF G_CUSTOM_CONTAINER IS INITIAL.
*    CREATE OBJECT G_CUSTOM_CONTAINER
*           EXPORTING CONTAINER_NAME = G_CONTAINER.
*
*    CREATE OBJECT GRID1
*           EXPORTING I_PARENT = G_CUSTOM_CONTAINER.
*
*    PERFORM mask_columns TABLES it_fieldcat.
*
*    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
*         EXPORTING I_STRUCTURE_NAME = 'ZSMM_ZRMMPM12'
*         CHANGING  it_fieldcatalog  = it_fieldcat[]
*                   IT_OUTTAB        = it_list1.
*
*  ENDIF.

ENDMODULE.                 " PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai INPUT.

*   to react on oi_custom_events:
  CALL METHOD cl_gui_cfw=>dispatch.
  CASE ok_code.
    WHEN 'EXIT'.
      PERFORM exit_program.
    WHEN OTHERS.
*     do nothing
  ENDCASE.
  CLEAR ok_code.

ENDMODULE.                 " PAI  INPUT
*&---------------------------------------------------------------------*
*&      Form  EXIT_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM exit_program.

*  CALL METHOD G_CUSTOM_CONTAINER->FREE.
*  CALL METHOD CL_GUI_CFW=>FLUSH.
  LEAVE PROGRAM.

ENDFORM.                    " EXIT_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  mask_columns
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_it_fieldcat  text
*----------------------------------------------------------------------*
FORM mask_columns TABLES p_gt_fieldcat TYPE lvc_t_fcat.

* Build the fieldcat according to DDIC structure SBOOK:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name = 'ZSMM_ZRMMPM12'
       CHANGING
            ct_fieldcat      = p_gt_fieldcat[].

  LOOP AT p_gt_fieldcat.
    IF  p_gt_fieldcat-fieldname = 'WERKS'
    OR  p_gt_fieldcat-fieldname = 'LGORT'
    OR  p_gt_fieldcat-fieldname = 'LGOBE'
    OR  p_gt_fieldcat-fieldname = 'MATNR'.
      p_gt_fieldcat-fix_column = 'X'.
      MODIFY p_gt_fieldcat.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " mask_columns
*&---------------------------------------------------------------------*
*&      Form  list_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM list_display.

  CHECK NOT it_list[] IS INITIAL.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program           = w_repid
      it_events                    = wa_events[]
      it_fieldcat                  = it_fieldcat[]
      i_callback_user_command      = 'USER_COMMAND'
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    TABLES
      t_outtab                     = it_list
   EXCEPTIONS
      program_error                  = 1
      OTHERS                         = 2
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " list_display
*&---------------------------------------------------------------------*
*&      Form  alv_field_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_field_build.

  w_repid = sy-repid.
  CLEAR : it_fieldcat[], wa_events[],
          it_list_top_of_page[]     .
* set fields
  PERFORM fieldcat_init.
* set event
  PERFORM eventtab_build USING wa_events[].
* set list heading
  PERFORM comment_build  USING it_list_top_of_page[].

ENDFORM.                    " alv_field_build
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
FORM fieldcat_init.

  build_fieldcat  'INDEX'  'INDEX'  'X'  space  space   'SEQ'  'SEQ'
                  'SEQ'  '2'.
  build_fieldcat  'DATAB'  'DATAB'  'X'  space  space  'Validity On'
                  'Validity On'  'Validity On'  '10'.
  build_fieldcat  'KBETR'  'KONWA'  ''  space  space  'Current Price'
                  'Current Price'  'Current Price'  '10'.
  build_fieldcat  'KZUST'  'KZUST'  ''  ''  ''  'Reason Code'
                  'Reason Code' 'Reason Code'  '3'.
  build_fieldcat  'LIFNR'  'LIFNR'  ''  ''  ''  'Vendor'
                  'Vendor' 'Vendor'  '10'.
  build_fieldcat  'NAME1'  'NAME1'  ''  ''  ''  'Vendor Name'
                  'Vendor Name' 'Vendor Name'  '25'.
  build_fieldcat  'ERFMG_V'  'ERFME'  ''  ''  ''  'LP GR'
                  'LP GR' 'LP GR'  '13'.
  build_fieldcat  'ERFMG_K'  'ERFME'  ''  ''  ''  'KD GR'
                  'KD GR' 'KD GR'  '13'.
  build_fieldcat  'KOSRT'  'KOSRT'  ''  ''  ''  'Approval No'
                  'Approval No' 'Approval No'  '10'.
  build_fieldcat  'ERDAT'  'ERDAT'  ''  ''  ''  'Create date'
                  'Create date' 'Create date'  '10'.

*  build_fieldcat_qty  'PREQT'  'MEINS'  ''  ''  ''  'Pre.GI qty'
*                  'Pre.GI qty' 'Pre.GI qty'  '10'.
*  build_fieldcat  'FD_03'  'MEINS'  ''  ''  ''  'D+2'
*                  'D+2' 'D+2'  '6'.

ENDFORM.                    " FIELDCAT_INIT
*&---------------------------------------------------------------------*
*&      Form  EVENTTAB_BUILD
*&---------------------------------------------------------------------*
FORM eventtab_build
                   USING e03_lt_events TYPE slis_t_event.

  DATA: ls_event TYPE slis_alv_event.
*
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = e03_lt_events.
  READ TABLE e03_lt_events WITH KEY name =  slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE c_formname_top_of_page TO ls_event-form.
    APPEND ls_event TO e03_lt_events.
  ENDIF.

ENDFORM.                    " EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD
*&---------------------------------------------------------------------*
FORM comment_build
               USING lt_top_of_page TYPE slis_t_listheader.

  DATA: ls_line  TYPE slis_listheader.
  DATA: lw_info  TYPE slis_entry,
        lw_name1 LIKE t001w-name1,
        lw_maktx LIKE makt-maktx,
        lw_ekotx LIKE t024e-ekotx,
        lw_eknam LIKE t024-eknam.
* read header text..
  PERFORM read_pruchasing_group USING p_matnr  lw_eknam.
  PERFORM read_material_text    USING p_matnr  lw_maktx.
  PERFORM read_pur_org_text     USING p_ekorg  lw_ekotx.
* title
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-h01.
  APPEND ls_line TO lt_top_of_page.
* plant
*  CLEAR: ls_line, lw_info.
*  CONCATENATE p_werks '/' lw_name1
*              INTO lw_info SEPARATED BY space.
*  ls_line-typ  = 'S'.
*  ls_line-key  = text-h02.
*  ls_line-info = lw_info.
*  APPEND ls_line TO lt_top_of_page.
* material number
  CLEAR: ls_line, lw_info.
  CONCATENATE p_matnr '/' lw_maktx
              INTO lw_info SEPARATED BY space.
  ls_line-typ  = 'S'.
  ls_line-key  = text-h03.
  ls_line-info = lw_info.
  APPEND ls_line TO lt_top_of_page.
* Purchasing org
  CLEAR: ls_line, lw_info.
  CONCATENATE p_ekorg '/' lw_ekotx
              INTO lw_info SEPARATED BY space.
  ls_line-typ  = 'S'.
  ls_line-key  = text-h04.
  ls_line-info = lw_info.
  APPEND ls_line TO lt_top_of_page.
* Reference(Purchasing group)
  CLEAR: ls_line, lw_info.
  ls_line-typ  = 'S'.
  ls_line-key  = text-h05.
  ls_line-info = lw_eknam.
  APPEND ls_line TO lt_top_of_page.
* Reference(Vendor)
  CLEAR: ls_line, lw_info.
  ls_line-typ  = 'S'.
  ls_line-key  = text-h06.
  APPEND ls_line TO lt_top_of_page.

  LOOP AT it_nextvd.
    CLEAR: ls_line, lw_info.
    CONCATENATE it_nextvd-lifnr '/' it_nextvd-name1
                INTO lw_info SEPARATED BY space.
    ls_line-typ  = 'S'.
    ls_line-info = lw_info.
    APPEND ls_line TO lt_top_of_page.
  ENDLOOP.

ENDFORM.                    " COMMENT_BUILD
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
*           I_LOGO             = 'ENJOYSAP_LOGO'
            it_list_commentary = it_list_top_of_page.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  calculate_date
*&---------------------------------------------------------------------*
*       get date until firm jone
*----------------------------------------------------------------------*
FORM calculate_date USING    p_date  p_firmjone.

  DATA : lw_firmjone  LIKE  t5a4a-dlydy.
  CLEAR: lw_firmjone.

  MOVE  p_firmjone  TO  lw_firmjone.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            date      = sy-datum
            days      = lw_firmjone
            months    = '00'
            signum    = '+'
            years     = '00'
       IMPORTING
            calc_date = p_date.

ENDFORM.                    " calculate_date
*&---------------------------------------------------------------------*
*&      Form  get_gr_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_gr_qty  USING pa_a018  LIKE  it_a018.

* get gr qty
  DATA  : BEGIN OF lt_mseg OCCURS 0,
            werks  LIKE  mseg-werks,
            matnr  LIKE  mseg-matnr,
            bwart  LIKE  mseg-bwart,
            profl  LIKE  mara-profl,
            erfme  LIKE  mseg-erfme,
            erfmg  LIKE  mseg-erfmg,
          END   OF lt_mseg.
  CLEAR : lt_mseg[], lt_mseg.

  SELECT werks matnr bwart profl erfme erfmg
        INTO TABLE lt_mseg
        FROM zvmm_gr_detail
       WHERE matnr  EQ      pa_a018-matnr
*       AND   werks  EQ      pa_a017-werks
       AND   budat  BETWEEN pa_a018-datab and pa_a018-datbi
       and   LIFNR  eq      PA_A018-LIFNR
       AND   bwart  IN      ('101', '102', '122', '932',
                             '511', '512', '561', '562').

  LOOP AT lt_mseg.
    MOVE : lt_mseg-werks  TO  it_mseg-werks,
           lt_mseg-matnr  TO  it_mseg-matnr,
           pa_a018-datab  TO  it_mseg-datab,
           lt_mseg-erfme  TO  it_mseg-erfme.
    IF  lt_mseg-bwart  EQ  '102'
    OR  lt_mseg-bwart  EQ  '122'
    OR  lt_mseg-bwart  EQ  '932'
    OR  lt_mseg-bwart  EQ  '512'
    OR  lt_mseg-bwart  EQ  '562'.
      lt_mseg-erfmg  =  lt_mseg-erfmg  *  -1.
    ENDIF.

    IF     lt_mseg-profl  EQ  'V'.
      it_mseg-erfmg_v  = lt_mseg-erfmg.
    ELSEIF lt_mseg-profl  EQ  'K'.
      it_mseg-erfmg_k  = lt_mseg-erfmg.
    ENDIF.

    COLLECT it_mseg.  CLEAR it_mseg.
  ENDLOOP.


ENDFORM.                    " get_gr_qty
*&---------------------------------------------------------------------*
*&      Form  modify_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM modify_data.

  DATA : lw_index  TYPE  i.
  CLEAR: lw_index.

  LOOP AT it_list.
    ADD 1 TO lw_index.
    it_list-index  =  lw_index.
* modify gr qty
    CLEAR wa_mseg.
    READ TABLE it_mseg INTO wa_mseg
                       WITH KEY werks  = it_list-werks
                                matnr  = it_list-matnr
                                datab  = it_list-datab.
    IF sy-subrc EQ 0.
      MOVE : wa_mseg-erfme   TO it_list-erfme,
             wa_mseg-erfmg_v TO it_list-erfmg_v,
             wa_mseg-erfmg_k TO it_list-erfmg_k.
    ENDIF.
* read reason code text
    SELECT SINGLE vtext INTO it_list-vtext
           FROM t686d
          WHERE spras  EQ  sy-langu
          AND   kzust  EQ  it_list-kzust.

    MODIFY it_list.
  ENDLOOP.

ENDFORM.                    " modify_data
*&---------------------------------------------------------------------*
*&      Form  read_plant_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_plant_text USING p_werks p_name1.

  SELECT SINGLE name1  INTO p_name1
         FROM t001w
        WHERE werks = p_werks.

ENDFORM.                    " read_plant_text
*&---------------------------------------------------------------------*
*&      Form  read_material_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_material_text USING    p_matnr  p_maktx.

  SELECT SINGLE maktx INTO p_maktx
         FROM makt
        WHERE matnr  = p_matnr
        AND   spras  = sy-langu.

ENDFORM.                    " read_material_text
*&---------------------------------------------------------------------*
*&      Form  read_pur_org_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_pur_org_text USING    p_ekorg  p_ekotx.

  SELECT SINGLE ekotx INTO p_ekotx
         FROM t024e
        WHERE ekorg  = p_ekorg.

ENDFORM.                    " read_pur_org_text
*&---------------------------------------------------------------------*
*&      Form  read_pruchasing_group
*&---------------------------------------------------------------------*
*       read purchasing group(reference person)
*----------------------------------------------------------------------*
FORM read_pruchasing_group USING    p_matnr  p_eknam.

* Person(purchasing group text)
  SELECT SINGLE b~eknam INTO p_eknam
         FROM marc AS a INNER JOIN t024 AS b
           ON a~mandt  EQ  b~mandt
          AND a~ekgrp  EQ  b~ekgrp
        WHERE matnr    EQ  p_matnr.

ENDFORM.                    " read_pruchasing_group
*&---------------------------------------------------------------------*
*&      Form  read_vendor_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_vendor_text USING    p_lifnr  p_name1.

  SELECT SINGLE name1 INTO p_name1
         FROM lfa1
        WHERE lifnr  EQ  p_lifnr.

ENDFORM.                    " read_vendor_text
*&---------------------------------------------------------------------*
*&      Form  get_next_vendor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_next_vendor.

  CLEAR : it_nextvd[], it_nextvd.
* next vendor
  SELECT p~lifnr  q~name1 INTO wa_nextvd
         FROM eord AS p INNER JOIN lfa1 AS q
           ON p~mandt  EQ  q~mandt
          AND p~lifnr  EQ  q~lifnr
        WHERE p~matnr  EQ  p_matnr.
*          AND p~ebeln  NE  space.     " <= add by stlim (2004/04/01)
    SELECT SINGLE *
           FROM a018
          WHERE kappl  EQ  'M'
          AND   kschl  EQ  'PB00'
          AND   lifnr  EQ  wa_nextvd-lifnr
          AND   matnr  EQ  p_matnr
          AND   ekorg  EQ  p_ekorg.
    IF sy-subrc EQ 0.
      APPEND wa_nextvd TO it_nextvd.
    ENDIF.
    CLEAR: it_nextvd, wa_nextvd.
  ENDSELECT.

**--- add by stlim (2004/04/07)
  DELETE ADJACENT DUPLICATES FROM it_nextvd.
**---

  SORT it_nextvd BY lifnr.
  IF s_lifnr-low  EQ  ''.
    DELETE it_nextvd INDEX 1.
  ELSE.
    LOOP AT it_nextvd WHERE lifnr = s_lifnr-low.
      DELETE it_nextvd.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " get_next_vendor
