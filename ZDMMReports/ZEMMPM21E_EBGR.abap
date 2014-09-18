************************************************************************
* Program Name      : ZEMMPM21E_EBGR
* Author            : Sung-Tae, Lim
* Creation Date     : 2003.09.03.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K901864
* Addl Documentation:
* Description       : Empty Box GR Quantity
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.09.03.     Sung-Tae Lim     UD1K901864     Initial Coding
* 08.13.2014      Victor     T-code has been deleted for APM
************************************************************************

report zemmpm21e_ebgr no standard page heading
                      line-size 132
                      line-count 64(1)
                      message-id zmmm.

**---
include : zrmmpmxxr_incl.

**--- Tables, Views & Structures


**--- Internal Tables
data : begin of it_itab occurs 0,
         lifnr like mseg-lifnr,
         matnr like mseg-matnr,
         werks like mseg-werks,
         lgort like mseg-lgort,
         erfmg like mseg-erfmg,
         meins like mseg-meins,
         mblnr like mseg-mblnr,
         messa(80),
         linecolor(4),     " ALV Color
       end of it_itab.

data : begin of it_marm occurs 0,
         matnr like marm-matnr,
         meinh like marm-meinh,
         umrez like marm-umrez,
       end of it_marm.

data : begin of it_temp occurs 0,
         matnr like marm-matnr,
         menge like mseg-menge,
         bwart like mseg-bwart,
         werks like mseg-werks,
         lgort like mseg-lgort,
       end of it_temp.

*----- BDC
data : begin of it_bdc occurs 0.
        include structure bdcdata.
data : end of it_bdc.

data : begin of it_mess occurs 0.
        include structure bdcmsgcoll.
data : end of it_mess.

data : it_message like it_mess occurs 0 with header line.

**--- BAPI
data : wa_goodsmvt_header  like bapi2017_gm_head_01,
       wa_goodsmvt_headret like bapi2017_gm_head_ret.

data : begin of it_goodsmvt_item occurs 0.
        include structure bapi2017_gm_item_create.
data : end of it_goodsmvt_item.

data : begin of it_return occurs 0.
        include structure bapiret2.
data : end of it_return.

data : w_mblnr like mkpf-mblnr,
       w_mjahr like mkpf-mjahr.

constants : c_goodsmvt_code like bapi2017_gm_code value '05'.

**--- Variables
data : w_mode like ctu_params-dismode value 'A'.

**--- Constants
constants : c_bwart like mseg-bwart value '501'.

**--- Macro
define append_fieldcat.
  &1 = &1 + 1.
  w_fieldcat-col_pos    = &1.
  w_fieldcat-fieldname  = &2.
  w_fieldcat-outputlen  = &3.
  w_fieldcat-seltext_l  = &4.
  w_fieldcat-seltext_m  = &4.
  w_fieldcat-seltext_s  = &4.
  w_fieldcat-datatype   = &5.
  w_fieldcat-key        = &6.
  w_fieldcat-qfieldname = &7.
  w_fieldcat-cfieldname = &8.
  append w_fieldcat.
  clear : w_fieldcat.
end-of-definition.

define append_sortcat.
  w_sortcat-spos      = &1.
  w_sortcat-fieldname = &2.
  w_sortcat-tabname   = &3.
  w_sortcat-up        = &4.
  w_sortcat-subtot    = &5.
  append w_sortcat.
  clear : w_sortcat.
end-of-definition.

**---
selection-screen begin of block block1 with frame title text-001.
select-options : s_matnr for mara-matnr.
parameters : p_budat like mkpf-budat obligatory.
selection-screen end of block block1.

**---
initialization.
  perform event_build using w_eventcat[].


**---
top-of-page.
  perform top_of_page.

**---
start-of-selection.
  perform get_data.

**---
end-of-selection.
  if it_itab[] is initial.
    message s999 with text-m01.
  else.
    perform posting_document.
    perform comment_build.     " USING w_top_of_page[].
    perform make_alv_grid.
  endif.


**---





*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data.
**---
  clear : it_temp, it_temp[], it_marm, it_marm[], it_itab, it_itab[].

*---
  select matnr
         meinh
         umrez
               into corresponding fields of table it_marm
               from marm
              where matnr in s_matnr
                and meinh eq 'BOX'.

*---
  select matnr
         bwart
         werks
         lgort
         menge
               into corresponding fields of table it_temp
               from mkpf as a inner join mseg as b
                 on a~mandt eq b~mandt
                and a~mblnr eq b~mblnr
                and a~mjahr eq b~mjahr
                for all entries in it_marm
              where budat eq p_budat
                and bwart in ('501', '502')
                and werks eq 'P001'
                and lgort eq 'P500'
                and matnr eq it_marm-matnr.

*---
  loop at it_temp.
    move-corresponding it_temp to it_itab.
*---
    clear : it_marm.
    read table it_marm with key matnr = it_temp-matnr.
    move : it_marm-meinh       to it_itab-meins.
*---
    perform get_vendor using it_temp-matnr it_temp-werks.
    check sy-subrc eq 0.
    if it_temp-bwart eq '502'.
      it_itab-erfmg = it_temp-menge * -1.
    elseif it_temp-bwart eq '501'.
      it_itab-erfmg = it_temp-menge.
    endif.
    collect it_itab.
    clear : it_temp, it_itab.
  endloop.

  sort it_itab by lifnr matnr.

*---
  data : l_erfmg like it_itab-erfmg.

  loop at it_itab.
    clear : l_erfmg.
    read table it_marm with key matnr = it_itab-matnr.
    l_erfmg = it_itab-erfmg / it_marm-umrez.
    call function 'FIMA_NUMERICAL_VALUE_ROUND'
         exporting
              i_rtype     = '+'
              i_runit     = 1
              i_value     = l_erfmg
         importing
              e_value_rnd = it_itab-erfmg.
    modify it_itab.
  endloop.
endform.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form comment_build.
**---
  clear : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-002.
  append w_line to w_top_of_page.

  clear : w_line.
  append initial line to w_top_of_page.
endform.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_alv_grid.
**---
  move : 'LINECOLOR' to w_layout-info_fieldname.

  perform build_fieldcat.
  perform build_sortcat.

  clear : w_program.

  move : sy-repid to w_program.

  call function 'REUSE_ALV_GRID_DISPLAY'
       exporting
            i_callback_program      = w_program
*            i_callback_user_command = 'USER_COMMAND'
*            i_structure_name        = 'ZSMM_GR_LIST'
            is_layout               = w_layout
            it_fieldcat             = w_fieldcat[]
            it_events               = w_eventcat[]
            it_sort                 = w_sortcat[]
            i_save                  = 'A'
       tables
            t_outtab                = it_itab
       exceptions
            program_error           = 1
            others                  = 2.
endform.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0599   text
*      -->P_0600   text
*      -->P_0601   text
*----------------------------------------------------------------------*
form dynpro using    dynbegin
                     name
                     value.
  if dynbegin = 'X'.
    clear : it_bdc.
    move : name  to it_bdc-program,
           value to it_bdc-dynpro,
           'X'   to it_bdc-dynbegin.
    append it_bdc.
  else .
    clear : it_bdc.
    move : name  to it_bdc-fnam,
           value to it_bdc-fval.
    append it_bdc.
  endif.
endform.                    " dynpro

*&---------------------------------------------------------------------*
*&      Form  get_vendor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TEMP_MATNR  text
*      -->P_IT_TEMP_WERKS  text
*----------------------------------------------------------------------*
form get_vendor using    p_matnr
                         p_werks.
*---
  clear : eord.

  select single lifnr into it_itab-lifnr
                      from eord
                     where matnr eq p_matnr
                       and werks eq p_werks
                       and vdatu le sy-datum
                       and bdatu ge sy-datum.
endform.                    " get_vendor

*&---------------------------------------------------------------------*
*&      Form  posting_document
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form posting_document.
**--- call BAPI Function
  loop at it_itab.
    at new lifnr.
      perform make_bapi_header.
    endat.
    perform make_bapi_item.
    at end of lifnr.
      perform call_bapi_function.
    endat.
  endloop.
endform.                    " posting_document

*&---------------------------------------------------------------------*
*&      Form  make_bapi_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_bapi_header.
*---
  clear : wa_goodsmvt_header, it_goodsmvt_item, it_goodsmvt_item[].

  move : sy-datum to wa_goodsmvt_header-pstng_date,
         sy-datum to wa_goodsmvt_header-doc_date.
endform.                    " make_bapi_header

*&---------------------------------------------------------------------*
*&      Form  call_bapi_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_bapi_function.
*---
  clear : w_mblnr, w_mjahr, it_return, it_return[].

  call function 'BAPI_GOODSMVT_CREATE'
       exporting
            goodsmvt_header  = wa_goodsmvt_header
            goodsmvt_code    = c_goodsmvt_code
       importing
            goodsmvt_headret = wa_goodsmvt_headret
            materialdocument = w_mblnr
            matdocumentyear  = w_mjahr
       tables
            goodsmvt_item    = it_goodsmvt_item
            return           = it_return.

  if it_return[] is initial and w_mblnr ne space.
    call function 'BAPI_TRANSACTION_COMMIT'
         exporting
              wait = 'X'.
    move : w_mblnr to it_itab-mblnr,
           space   to it_itab-messa,
           c_green to it_itab-linecolor.
  else.
    read table it_return index 1.
    move : it_return-message to it_itab-messa,
           space             to it_itab-mblnr,
           c_red             to it_itab-linecolor.
  endif.

  modify it_itab transporting mblnr
                              messa
                              linecolor where lifnr eq it_itab-lifnr.
endform.                    " call_bapi_function

*&---------------------------------------------------------------------*
*&      Form  make_bapi_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_bapi_item.
*---
  clear : it_goodsmvt_item.

  move : it_itab-matnr to it_goodsmvt_item-material,
         it_itab-werks to it_goodsmvt_item-plant,
         it_itab-lgort to it_goodsmvt_item-stge_loc,
         c_bwart       to it_goodsmvt_item-move_type,
         it_itab-lifnr to it_goodsmvt_item-vendor,
         it_itab-erfmg to it_goodsmvt_item-entry_qnt,
         it_itab-meins to it_goodsmvt_item-entry_uom.

  append it_goodsmvt_item.
endform.                    " make_bapi_item

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_fieldcat.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : color
  append_fieldcat :
    w_col_pos 'LIFNR' 10 'Vendor'         'CHAR' 'X' ''      '',
    w_col_pos 'MATNR' 18 'Material'       'CHAR' ''  ''      '',
    w_col_pos 'WERKS'  4 'Plant'          'CHAR' ''  ''      '',
    w_col_pos 'LGORT'  4 'Storage Loc.'   'CHAR' ''  ''      '',
    w_col_pos 'ERFMG' 12 'Quantity'       'QUAN' ''  'MEINS' '',
    w_col_pos 'MEINS'  3 'UoM'            'CHAR' ''  ''      '',
    w_col_pos 'MBLNR' 10 'Document No'    'CHAR' ''  ''      '',
    w_col_pos 'MESSA' 80 'Message'        'CHAR' ''  ''      ''.
endform.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_sortcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_sortcat.
**--- &1 : position     &2 : field name     &3 : tab name
**--- &4 : up           &5 : sub total
  append_sortcat : '1' 'LIFNR' 'IT_ITAB' 'X' '',
                   '2' 'MATNR' 'IT_ITAB' 'X' ''.
endform.                    " build_sortcat
