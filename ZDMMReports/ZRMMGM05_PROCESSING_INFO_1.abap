************************************************************************
* Program name: ZRMMGM05_PROCESSING_INFO
* Created by  : hj.song
* Created on  : 2003.11.11.
* Pattern     :
* Description : PROCESSING INFO
*
* Modification Log
* Date            Developer        Request No.    Description
* 2003.11.11.     hj.song          UD1K902172     Initial Coding
*
************************************************************************
*&---------------------------------------------------------------------*
*& Report  ZRMMGM05_PROCESSING_INFO                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT  zrmmgm05_processing_info "NO STANDARD PAGE HEADING LINE-SIZE 132
                              MESSAGE-ID zmmm.

TYPE-POOLS: slis.

DEFINE build_fieldcat.
  add 1 to col_pos.
  wa_fieldcat-col_pos           = col_pos.
  wa_fieldcat-fieldname         = &1.
  wa_fieldcat-ref_fieldname     = &2.
  wa_fieldcat-key               = &3.
  wa_fieldcat-qfieldname        = &4.
  wa_fieldcat-cfieldname        = &5.
  wa_fieldcat-seltext_l         = &6.
  wa_fieldcat-seltext_m         = &7.
  wa_fieldcat-seltext_s         = &8.
  wa_fieldcat-outputlen         = &9.
  append wa_fieldcat to it_fieldcat.
  clear  wa_fieldcat.
END-OF-DEFINITION.


* tables
TABLES : ekko,
         ekpo,
         eket,
         ekes,
         resb,
         makt,
         lfa1,
         t163y, *t163y,
         ztmm_pro_info.

* itab
DATA :                                "basic data
       BEGIN  OF it_header  OCCURS 0.
        INCLUDE STRUCTURE ztmm_pro_info.
DATA :   bsart    LIKE    ekko-bsart,
         pstyp    LIKE    ekpo-pstyp,
         lgort    LIKE    ekpo-lgort,
       END    OF it_header,
                                       "component
       BEGIN  OF it_comp  OCCURS 0,
         matnr    LIKE    ekpo-matnr,
         nomng    LIKE    resb-nomng,
       END    OF it_comp,
* wa
       wa_header  LIKE it_header.

*ALV Definition.
DATA:   wa_events      TYPE  slis_t_event,
        it_fieldcat    TYPE  slis_t_fieldcat_alv WITH HEADER LINE,
        wa_list_top_of_page
                       TYPE  slis_t_listheader,
        wa_fieldcat    LIKE  LINE OF it_fieldcat,
        wa_layo        TYPE  slis_layout_alv,
        c_formname_top_of_page
                       TYPE  slis_formname VALUE 'TOP_OF_PAGE',
        w_repid        LIKE  sy-repid,
        col_pos        TYPE  i.

* gv data
DATA : w_total        LIKE ztca_if_log-total,  "total cnt in interface
       w_succ         LIKE ztca_if_log-zsucc,  "success cnt in interface
       wa_ztca_if_log LIKE ztca_if_log.


* selection screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS :    s_eindt  FOR   eket-eindt,
                    s_bedat  FOR   ekko-bedat.
PARAMETERS     :    p_lifnr  LIKE  ekko-lifnr,
                    p_matnr  LIKE  ekpo-matnr.
SELECTION-SCREEN END OF BLOCK b1.

* start
START-OF-SELECTION.
  PERFORM set_parameters.
  PERFORM read_data.

* end
END-OF-SELECTION.
  PERFORM display_data.

*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------
FORM display_data.

  w_repid = sy-repid.
  CLEAR : it_fieldcat[], wa_events[],
          wa_list_top_of_page[],
          wa_layo.
* set fields
  PERFORM fieldcat_init.
* set event
  PERFORM eventtab_build USING wa_events[].
* set list heading
  PERFORM comment_build  USING wa_list_top_of_page[].
* set layout
  PERFORM layout_build   CHANGING wa_layo.
* call fun
  PERFORM alv_display.


ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  fieldcat_init
*&---------------------------------------------------------------------*
FORM fieldcat_init.

  build_fieldcat  'DOC_TYPE'  'DOC_TYPE'  'X'  space  space
                  'Document Type'  'Document Type'  'Document Type'
                  '10'.
  build_fieldcat  'BUKRS'  'BUKRS'  'X'  space  space  'Company'
                  'Company'  'Company'  '4'.
  build_fieldcat  'WERKS'  'WERKS'  'X'  space  space  'Plant'
                  'Plant'  'Plant'  '4'.
  build_fieldcat  'EBELN'  'EBELN'  'X'  space  space  'PO doc'
                  'PO doc'  'PO doc'  '10'.
  build_fieldcat  'EBELP'  'EBELP'  'X'  space  space  'PO doc item'
                  'PO doc item'  'PO doc item'  '5'.
  build_fieldcat  'MATNR'  'MATNR'  'X'  space  space  'Material'
                  'Material'  'Material'  '18'.
  build_fieldcat  'MAKTX'  'MAKTX'  ''  ''  ''  'Material desc'
                  'Material desc' 'Material desc'  '40'.
  build_fieldcat  'PROP'  'PROP'  ''  ''  ''  'Material property'
                  'Material property' 'Material property'  '20'.
  build_fieldcat  'COATING'  'COATING'  ''  ''  ''  'Coating'
                  'Coating' 'Coating'  '6'.
  build_fieldcat  'THICK'  'THICK'  ''  ''  ''  'Thick'
                  'Thick' 'Thick'  '8'.
  build_fieldcat  'WIDTH'  'WIDTH'  ''  ''  ''  'WIDTH'
                  'WIDTH' 'WIDTH'  '5'.
  build_fieldcat  'LENGTH'  'LENGTH'  ''  ''  ''  'Length'
                  'Length' 'Length'  '5'.
  build_fieldcat  'LIFNR'  'LIFNR'  ''  ''  ''  'vendor'
                  'vendor' 'vendor'  '10'.
  build_fieldcat  'NAME1'  'NAME1'  ''  ''  ''  'vendor desc'
                  'vendor desc' 'vendor desc'  '35'.
  build_fieldcat  'MENGE'  'MEINS'  ''  ''  ''  'PO quantity'
                  'PO quantity' 'PO quantity'  '13'.
  build_fieldcat  'MEINS'  'MEINS'  ''  ''  ''  'Order unit'
                  'Order unit' 'Order unit'  '3'.
  build_fieldcat  'EINDT'  'EINDT'  ''  ''  ''  'delivery date'
                  'delivery date' 'delivery date'  '8'.
  build_fieldcat  'NETPR'  'WAERS'  ''  ''  ''  'Net price'
                  'Net price' 'Net price'  '10'.
  build_fieldcat  'WAERS'  'WAERS'  ''  ''  ''  'Currency'
                  'Currency' 'Currency'  '5'.
  build_fieldcat  'CHARG'  'CHARG'  ''  ''  ''  'Batch number'
                  'Batch number' 'Batch number'  '10'.
  build_fieldcat  'LGPRO_F'  'LGPRO_F'  ''  ''  ''  'From stor.loc'
                  'From stor.loc' 'From stor.loc'  '4'.
  build_fieldcat  'LGPRO_E'  'LGPRO_E'  ''  ''  ''  'To stor.loc'
                  'To stor.loc' 'To stor.loc'  '4'.
  build_fieldcat  'MATNR_C1'  'MATNR_C1'  ''  ''  ''  'Material'
                  'Material' 'Material'  '10'.
  build_fieldcat  'MAKTX_C1'  'MAKTX_C1'  ''  ''  ''  'desc'
                  'desc' 'desc'  '40'.
  build_fieldcat  'ERFMG_C1'  'MEINS'  ''  ''  ''  'Require qty'
                  'Require qty' 'Require qty'  '13'.
  build_fieldcat  'MATNR_C2'  'MATNR_C1'  ''  ''  ''  'Material'
                  'Material' 'Material'  '10'.
  build_fieldcat  'MAKTX_C2'  'MAKTX_C1'  ''  ''  ''  'desc'
                  'desc' 'desc'  '40'.
  build_fieldcat  'ERFMG_C2'  'MEINS'  ''  ''  ''  'Require qty'
                  'Require qty' 'Require qty'  '13'.
  build_fieldcat  'MATNR_C3'  'MATNR_C1'  ''  ''  ''  'Material'
                  'Material' 'Material'  '10'.
  build_fieldcat  'MAKTX_C3'  'MAKTX_C1'  ''  ''  ''  'desc'
                  'desc' 'desc'  '40'.
  build_fieldcat  'ERFMG_C3'  'MEINS'  ''  ''  ''  'Require qty'
                  'Require qty' 'Require qty'  '13'.
  build_fieldcat  'MATNR_C4'  'MATNR_C1'  ''  ''  ''  'Material'
                  'Material' 'Material'  '10'.
  build_fieldcat  'MAKTX_C4'  'MAKTX_C1'  ''  ''  ''  'desc'
                  'desc' 'desc'  '40'.
  build_fieldcat  'ERFMG_C4'  'MEINS'  ''  ''  ''  'Require qty'
                  'Require qty' 'Require qty'  '13'.
  build_fieldcat  'MATNR_C5'  'MATNR_C1'  ''  ''  ''  'Material'
                  'Material' 'Material'  '10'.
  build_fieldcat  'MAKTX_C5'  'MAKTX_C1'  ''  ''  ''  'desc'
                  'desc' 'desc'  '40'.
  build_fieldcat  'ERFMG_C5'  'MEINS'  ''  ''  ''  'Require qty'
                  'Require qty' 'Require qty'  '13'.


ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  eventtab_build
*&---------------------------------------------------------------------*
FORM eventtab_build  USING e03_lt_events TYPE slis_t_event.

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

ENDFORM.                    " eventtab_build
*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE slis_t_listheader.

  DATA: ls_line TYPE slis_listheader.
  DATA: info_txt(50).

  CLEAR ls_line.
  ls_line-typ  = 'H'.
* LS_LINE-KEY:  not used for this type
  ls_line-info = text-h01.
  APPEND ls_line TO lt_top_of_page.

*Date Selection Range Display
  CLEAR info_txt.
*  INFO_TXT+0(4)   = 'Date'    .
  info_txt+5(10)  = sy-datum.
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Date:'.
  ls_line-info = info_txt.
  APPEND ls_line TO lt_top_of_page.

ENDFORM.                    " comment_build
*&---------------------------------------------------------------------*
*&      Form  alv_display
*&---------------------------------------------------------------------*
FORM alv_display.

  CHECK NOT it_header[] IS INITIAL.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program           = w_repid
      it_events                    = wa_events[]
      it_fieldcat                  = it_fieldcat[]
      i_callback_pf_status_set     = 'PF_STATUS'
      i_callback_user_command      = 'USER_COMMAND'
      is_layout                    = wa_layo
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    TABLES
      t_outtab                     = it_header
 EXCEPTIONS
   program_error                  = 1
   OTHERS                         = 2
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " alv_display
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_data.

  CLEAR : it_header[], it_header.

  SELECT  a~bsart  b~pstyp  a~bukrs
          b~werks  b~ebeln  b~ebelp
          b~matnr  a~lifnr  b~menge
          b~meins  c~eindt  b~netpr
          a~waers  b~lgort  d~charg
          b~ebeln  b~ebelp

        INTO CORRESPONDING FIELDS OF TABLE it_header
        FROM ekko AS a INNER JOIN ekpo AS b
          ON a~mandt  EQ  b~mandt
         AND a~ebeln  EQ  b~ebeln
           INNER JOIN eket AS c
              ON b~mandt  EQ  c~mandt
             AND b~ebeln  EQ  c~ebeln
             AND b~ebelp  EQ  c~ebelp
               INNER JOIN eket AS d
                  ON b~mandt  EQ  d~mandt
                 AND b~ebeln  EQ  d~ebeln
                 AND b~ebelp  EQ  d~ebelp
       WHERE a~bedat  IN      s_bedat
       AND   a~lifnr  LIKE    p_lifnr
       AND   b~matnr  LIKE    p_matnr
       AND   c~eindt  IN      s_eindt
       AND   b~loekz  EQ      ''
       AND   a~bsart  EQ      'NB'    "po
       AND   b~pstyp  EQ      '3'.    "subcontracting
*  if sy-subrc eq 0.
*  endif.
  SORT it_header BY ebeln ebelp.


  LOOP AT it_header INTO wa_header.
* get material text
    PERFORM read_material_text  USING wa_header-matnr
                                      wa_header-maktx.
* get vendor text
    PERFORM read_vendor_text    USING wa_header-lifnr
                                      wa_header-name1.
* get characteristic
    PERFORM read_characteristic USING wa_header.
* get component
    PERFORM read_componenet     USING wa_header.
* others
    CLEAR *t163y.
    SELECT SINGLE epstp  INTO *t163y-epstp
                  FROM  t163y
                 WHERE spras  EQ  sy-langu
                 AND   pstyp  EQ  wa_header-pstyp.
    CONCATENATE wa_header-bsart  *t163y-epstp
                    INTO wa_header-doc_type. "doc type
    wa_header-lgpro_f  =  wa_header-lgort.   "from storg.loc
    wa_header-lgpro_e  =  wa_header-lifnr.   "to storg.loc

    MODIFY it_header FROM wa_header.
  ENDLOOP.



ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  read_material_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_material_text USING    p_matnr  p_maktx.

  SELECT SINGLE maktx INTO p_maktx
         FROM makt
        WHERE matnr EQ p_matnr
        AND   spras EQ sy-langu.

ENDFORM.                    " read_material_text
*&---------------------------------------------------------------------*
*&      Form  read_vendor_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_vendor_text USING    p_lifnr  p_name1.

  SELECT SINGLE name1 INTO p_name1
         FROM lfa1
        WHERE lifnr EQ p_lifnr.

ENDFORM.                    " read_vendor_text
*&---------------------------------------------------------------------*
*&      Form  read_characteristic
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_characteristic
                USING  pa_header  LIKE  it_header.

  DATA : lt_allocvalueschar  LIKE TABLE OF bapi1003_alloc_values_char
                                  WITH HEADER LINE.
  CLEAR: lt_allocvalueschar[],  lt_allocvalueschar.

  CALL FUNCTION 'Z_FMM_GET_CHARACT_CONV_CHAR'
       EXPORTING
            i_matnr            = pa_header-matnr
       TABLES
            it_allocvalueschar = lt_allocvalueschar.

* charact code는 나중에 변경가능성 있음
  LOOP AT lt_allocvalueschar.
    CASE  lt_allocvalueschar-charact.
      WHEN 'ZSTEEL_MATPROPERTY'.
        pa_header-prop  = lt_allocvalueschar-value_char.
      WHEN 'ZSTEEL_COATING'.
        pa_header-coating  = lt_allocvalueschar-value_char.
      WHEN 'ZSPEC_THICK'.
        pa_header-thick  = lt_allocvalueschar-value_char.
      WHEN 'ZSPEC_WIDTH'.
        pa_header-width  = lt_allocvalueschar-value_char.
      WHEN 'ZSPEC_LENGTH'.
        pa_header-length  = lt_allocvalueschar-value_char.
    ENDCASE.
  ENDLOOP.


ENDFORM.                    " read_characteristic
*&---------------------------------------------------------------------*
*&      Form  read_componenet
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_componenet  USING  pa_header LIKE it_header.

  DATA  : lt_vlcporder_it  LIKE TABLE OF vlcporder
                                WITH HEADER LINE,
          lt_resb_et       LIKE TABLE OF resb
                                WITH HEADER LINE.
  CLEAR : it_comp[], it_comp,
          lt_vlcporder_it[], lt_vlcporder_it,
          lt_resb_et[], lt_resb_et.

*  SELECT  matnr  nomng
*         INTO TABLE it_comp
*         FROM resb
*        WHERE ebeln  EQ  pa_header-ebeln
*        AND   ebelp  EQ  pa_header-ebelp.

* modify by tuning--> but not improve

  lt_vlcporder_it-ebeln  =  pa_header-ebeln.
  lt_vlcporder_it-ebelp  =  pa_header-ebelp.
  APPEND  lt_vlcporder_it.

  CALL FUNCTION 'VELO14_READ_COMPONENTS_OF_PO'
       TABLES
            vlcporder_it          = lt_vlcporder_it
            resb_et               = lt_resb_et
       EXCEPTIONS
            no_data_received      = 1
            nothing_found_in_eket = 2
            nothing_found_in_resb = 3
            OTHERS                = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT  lt_resb_et.
    it_comp-matnr  =  lt_resb_et-matnr.
    it_comp-nomng  =  lt_resb_et-nomng.
    APPEND it_comp.
  ENDLOOP.

  LOOP AT it_comp.
    CASE sy-tabix.
      WHEN 1.
        pa_header-matnr_c1  = it_comp-matnr.
        pa_header-erfmg_c1  = it_comp-nomng.
        PERFORM read_material_text USING pa_header-matnr_c1
                                         pa_header-maktx_c1.
      WHEN 2.
        pa_header-matnr_c2  = it_comp-matnr.
        pa_header-erfmg_c2  = it_comp-nomng.
        PERFORM read_material_text USING pa_header-matnr_c2
                                         pa_header-maktx_c2.
      WHEN 3.
        pa_header-matnr_c3  = it_comp-matnr.
        pa_header-erfmg_c3  = it_comp-nomng.
        PERFORM read_material_text USING pa_header-matnr_c3
                                         pa_header-maktx_c3.
      WHEN 4.
        pa_header-matnr_c4  = it_comp-matnr.
        pa_header-erfmg_c4  = it_comp-nomng.
        PERFORM read_material_text USING pa_header-matnr_c4
                                         pa_header-maktx_c4.
      WHEN 5.
        pa_header-matnr_c5  = it_comp-matnr.
        pa_header-erfmg_c5  = it_comp-nomng.
        PERFORM read_material_text USING pa_header-matnr_c5
                                         pa_header-maktx_c5.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " read_componenet
*&---------------------------------------------------------------------*
*&      Form  set_parameters
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_parameters.

  IF    p_lifnr  EQ  ''.  p_lifnr  =  '%'.  ENDIF.
  IF    p_matnr  EQ  ''.  p_matnr  =  '%'.  ENDIF.

ENDFORM.                    " set_parameters
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
FORM user_command USING p_ucomm          LIKE sy-ucomm
                        wa_slis_selfield TYPE slis_selfield.
  data: ET_MARKED_COLUMNS TYPE  SLIS_T_FIELDCAT_ALV,
CS_FIELDCAT TYPE  LVC_S_FCAT.
  IF p_ucomm = 'INTF'.

  ENDIF.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS
*&---------------------------------------------------------------------*
FORM pf_status  USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'ZRMMGM05' EXCLUDING rt_extab[].

ENDFORM.                    "pf_status
*&---------------------------------------------------------------------*
*&      Form  layout_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM layout_build CHANGING pa_layo TYPE slis_layout_alv.

  pa_layo-box_fieldname          = 'BOX'.

ENDFORM.                    " layout_build
