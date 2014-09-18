************************************************************************
* Program Name      : ZACOU117
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.08.13.
* Specifications By : Andy Choi
* Development Request No :
* Addl Documentation:
* Description       : LDC Creation
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
* ZEMMPM49E_LDC
************************************************************************

report zacou117 no standard page heading
                     line-size 132
                     line-count 64(1)
                     message-id zmmm.
tables eina.
**---
*INCLUDE : zrmmpmxxr_incl.
type-pools : slis,
             cxtab.

*--- ALV
data : w_fieldcat type slis_t_fieldcat_alv with header line,
       w_eventcat type slis_t_event with header line,
       w_selfield type slis_selfield,
       w_sortcat  type slis_t_sortinfo_alv with header line,
       w_col_pos  type i,
       w_program  like sy-repid,
       w_top_of_page type slis_t_listheader,
       w_line type slis_listheader,
       w_layout   type slis_layout_alv.

data : w_fieldcat02 type slis_t_fieldcat_alv with header line,
       w_eventcat02 type slis_t_event with header line,
       w_selfield02 type slis_selfield,
       w_sortcat02  type slis_t_sortinfo_alv with header line,
       w_col_pos02  type i,
       w_program02  like sy-repid,
       w_top_of_page02 type slis_t_listheader,
       w_line02 type slis_listheader,
       w_layout02   type slis_layout_alv.

data : w_fieldcat03 type slis_t_fieldcat_alv with header line,
       w_eventcat03 type slis_t_event with header line,
       w_selfield03 type slis_selfield,
       w_sortcat03  type slis_t_sortinfo_alv with header line,
       w_col_pos03  type i,
       w_program03  like sy-repid,
       w_top_of_page03 type slis_t_listheader,
       w_line03 type slis_listheader,
       w_layout03   type slis_layout_alv.

**--- Constants
constants : c_formname_top_of_page   type slis_formname
                                        value 'TOP_OF_PAGE',
            c_formname_top_of_page02 type slis_formname
                                        value 'TOP_OF_PAGE02',
            c_formname_top_of_page03 type slis_formname
                                        value 'TOP_OF_PAGE03',
            c_yell(4)  value 'C310',
            c_green(4) value 'C510',
            c_red(4)   value 'C610'.
*&---------------------------------------------------------------------*
*&      Form  event_build
*&---------------------------------------------------------------------*
form event_build using    p_w_eventcat type slis_t_event.
  data : l_event type slis_alv_event.

  call function 'REUSE_ALV_EVENTS_GET'
       exporting
            i_list_type = 0
       importing
            et_events   = p_w_eventcat.

  read table p_w_eventcat with key name = slis_ev_top_of_page
                          into l_event.

  if sy-subrc eq 0.
    move c_formname_top_of_page to l_event-form.
    append l_event to p_w_eventcat.
  endif.
endform.                    " event_build

*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
form top_of_page.
  call function 'REUSE_ALV_COMMENTARY_WRITE'
       exporting
            it_list_commentary = w_top_of_page.
endform.                    " top_of_page




*tables : ztmm_ldc_log.

**--- Internal Tables
data : it_itab like a018 occurs 0 with header line.

data : begin of it_info_item occurs 0,
         kappl like konp-kappl,
         kschl like konp-kschl,
         kbetr like konp-kbetr,
         kpein like konp-kpein,
         konwa like konp-konwa,
         kzust like konh-kzust,  "reason
         kosrt like konh-kosrt,
       end of it_info_item.

data : begin of gt_log occurs 0,
         lifnr like a018-lifnr,
         matnr like a018-matnr,
         datab like a018-datab,
         datbi like a018-datbi,
         messa(80),
         linecolor(4),     " ALV Color
       end of gt_log.

*data : it_ztmm_ldc_log like ztmm_ldc_log occurs 0 with header line.


**--- Variables


**--- Constants
constants : c_kappl like a018-kappl value 'M',
            c_kschl_pb00 like a018-kschl value 'PB00',
            c_kschl_fra1 like a018-kschl value 'FRA1',
            c_kschl_zoth like a018-kschl value 'ZOTH',
            c_kschl_zoti like a018-kschl value 'ZOTI',
            c_kschl_fra2 like a018-kschl value 'FRA2',
            c_esokz like a018-esokz value '0'.



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

define append_top.
  clear : w_line.
  if not &3 is initial or not &4 is initial.
    w_line-typ   = &1.
    w_line-key   = &2.
    concatenate &3 '~' &4 into w_line-info separated by space.
    append w_line to w_top_of_page.
  endif.
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


tables: lfa1, mara, konh, konp, a018.

types: begin of ty_a902,
         stawn type stawn,
         kbetr type kbetr,
       end of ty_a902.
data: gt_ldc type table of ztcou116 with header line.
data gt_a902 type table of ty_a902 with header line.
data: gv_land1 like lfa1-land1.

data: g_fra1 type zfra1,
      g_zoth type zoth,
      g_zoti type zoti.
ranges: s_budat for konh-datab.
data: g_date_start like konh-datab.
**---
selection-screen begin of block block1 with frame title text-001.

parameters : p_ekorg like t024e-ekorg default 'PU01',
             p_kokrs like ztcou116-kokrs memory id cac,
             p_bdatj like ztcou116-bdatj memory id bdtj.
select-options : s_lifnr for lfa1-lifnr obligatory,
                 s_mtart for mara-mtart obligatory,
                 s_matnr for mara-matnr,
                 s_profl for mara-profl default 'K'.

selection-screen skip.

selection-screen begin of block block2 with frame title text-002.
parameters: p_budat like konh-datab obligatory.
parameters: p_test as checkbox default 'X'.
select-options: s_kd  for a018-kschl.
*               s_lp  for a018-kschl.

*selection-screen skip.
*parameters : p_kzust like konh-kzust.
*selection-screen skip.
*parameters : p_fra1  like konp-kbetr,
*             p_zoth  like konp-kbetr,
*             p_zoti  like konp-kbetr.
selection-screen end of block block2.

selection-screen end of block block1.

include zcoi_bdc_inc.


data : messtabage like messtab occurs 0 with header line.


**---
initialization.
  group = 'LDC INPUT'.
  session = ' '.

  perform event_build using w_eventcat[].
  s_kd-option = 'EQ'.  s_kd-sign = 'I'.

*KD condition
  s_kd-low    = 'PB00'. append s_kd.
  s_kd-low    = 'FRA1'. append s_kd.
*  s_Kd-low    = 'ZOA1'. append s_kd.
  s_kd-low    = 'ZOTH'. append s_kd.
  s_kd-low    = 'ZOTI'. append s_kd.

  s_kd-low    = 'ZP16'. append s_kd.
  s_kd-low    = 'ZP17'. append s_kd.
  s_kd-low    = 'ZP18'. append s_kd.

*LP condition
  s_kd-low    = 'ZP01'. append s_kd.
  s_kd-low    = 'ZP02'. append s_kd.
  s_kd-low    = 'ZP03'. append s_kd.
  s_kd-low    = 'ZP04'. append s_kd.
  s_kd-low    = 'ZP05'. append s_kd.
  s_kd-low    = 'ZP06'. append s_kd.
  s_kd-low    = 'ZP07'. append s_kd.
  s_kd-low    = 'ZP08'. append s_kd.
  s_kd-low    = 'ZP09'. append s_kd.
  s_kd-low    = 'ZP10'. append s_kd.
  s_kd-low    = 'ZP11'. append s_kd.
  s_kd-low    = 'ZP12'. append s_kd.
  s_kd-low    = 'ZP13'. append s_kd.
  s_kd-low    = 'ZP14'. append s_kd.
  s_kd-low    = 'ZP15'. append s_kd.
  s_kd-low    = 'ZP20'. append s_kd.
  s_kd-low    = 'ZP21'. append s_kd.
  s_kd-low    = 'ZP22'. append s_kd.
  s_kd-low    = 'ZP23'. append s_kd.
  s_kd-low    = 'ZP24'. append s_kd.


**---
top-of-page.
  perform top_of_page.


**---
start-of-selection.
  concatenate p_bdatj '0101' into g_date_start.
  check p_budat >= g_date_start.
  s_budat-option  = 'BT'.
  s_budat-sign    = 'I'.
  s_budat-low  = p_budat.
  s_budat-high = '99991231'.
  append s_budat.

  perform get_ldc_info.
  perform get_data.


**---
end-of-selection.
  if it_itab[] is initial.
    message s999 with text-m01.
  else.
    if p_test = space.
      perform create_info_record.
    endif.

    perform comment_build.
    perform make_alv_grid.
  endif.



**---

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
form get_data.
*---
  clear : it_itab, it_itab[].

  if not s_budat-high is initial.
    select * into corresponding fields of table it_itab
             from a018
            where kappl eq c_kappl
              and kschl eq c_kschl_pb00
              and lifnr in s_lifnr
              and matnr in s_matnr
              and ekorg eq p_ekorg
              and esokz eq c_esokz
              and ( datbi in s_budat
                 or datab in s_budat
                 or datab le s_budat-low and
                    datbi ge s_budat-high ).
*              AND datbi GE s_budat-low.    " valid end
**            AND datab LE s_budat-low.    " valid from
  else.
    select * into corresponding fields of table it_itab
             from a018
            where kappl eq c_kappl
              and kschl eq c_kschl_pb00
              and lifnr in s_lifnr
              and matnr in s_matnr
              and ekorg eq p_ekorg
              and esokz eq c_esokz
              and datbi ge s_budat-low    " valid end
              and datab le s_budat-low.    " valid from
  endif.

*---
  data $ix like sy-tabix.
  loop at it_itab.
    $ix = sy-tabix.
    clear : mara.
    select single matnr into mara-matnr
                        from mara
                       where matnr eq it_itab-matnr
                         and mtart in s_mtart
                         and profl in s_profl.
    if sy-subrc ne 0.
      delete it_itab index $ix.
    endif.
* UD1K940999 by IG.MOON 7/11/2007 {
    select single * from eina where matnr eq it_itab-matnr
                                and lifnr eq it_itab-lifnr
                                and loekz eq 'X'.

    if sy-subrc eq 0.
      delete it_itab index $ix.
    endif.
* }
  endloop.
endform.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  create_info_record
*&---------------------------------------------------------------------*
form create_info_record.
*---
  clear : messtabage, messtabage[], gt_log, gt_log[].
*         it_ztmm_ldc_log, it_ztmm_ldc_log[].
  data : l_cnt type i.

  loop at it_itab.
    at first.
      perform open_group.
    endat.
    add 1 to l_cnt.

    clear : gt_log. "it_ztmm_ldc_log.
    perform get_info_item_condition.
    describe table it_info_item lines sy-tabix.
    if sy-tabix > 0.
      perform change_info_condition.
    endif.

    perform append_to_log_itab.
*    perform append_to_log_db.

    if l_cnt = p_scount.
      perform close_group.
      perform open_group.
      clear l_cnt.
    endif.
  endloop.

  perform close_group.

*---
*  modify ztmm_ldc_log from table it_ztmm_ldc_log.
endform.                    " create_info_record

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
form comment_build.
*---
  clear : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-003.
  append w_line to w_top_of_page.

  clear : w_line.
  append initial line to w_top_of_page.

*---
  append_top :
      'S' text-004 p_ekorg ' ',
      'S' text-005 s_lifnr-low s_lifnr-high,
      'S' text-006 s_mtart-low s_mtart-high,
      'S' text-007 s_matnr-low s_matnr-high,
      'S' text-008 s_profl-low s_profl-high.

  clear : w_line.
  append initial line to w_top_of_page.

  append_top :
      'S' text-009 s_budat-low s_budat-high.
*      'S' text-010 p_kzust ' '.

  clear : w_line.
*  move : 'S'      to w_line-typ,
*         text-010 to w_line-key,
*         p_kzust  to w_line-info.
*  append w_line to w_top_of_page.

  data : l_text_temp(20).

*  clear : w_line.
*  move : 'S'      to w_line-typ,
*         text-011 to w_line-key.
*  write p_fra1    to l_text_temp.
*  concatenate l_text_temp '%' into w_line-info.
*  append w_line to w_top_of_page.
*
*  clear : w_line.
*  move : 'S'      to w_line-typ,
*         text-012 to w_line-key.
*  write p_zoth    to l_text_temp.
*  concatenate l_text_temp '%' into w_line-info.
*  append w_line to w_top_of_page.
*
*  clear : w_line.
*  move : 'S'      to w_line-typ,
*         text-013 to w_line-key.
*  write p_zoti    to l_text_temp.
*  concatenate l_text_temp '%' into w_line-info.
*  append w_line to w_top_of_page.
endform.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
form make_alv_grid.
*---
  move : 'LINECOLOR' to w_layout-info_fieldname,
         'X'         to w_layout-colwidth_optimize.

  perform build_fieldcat.
  perform build_sortcat.

  clear : w_program.

  move : sy-repid to w_program.

  if p_test = 'X'.
    call function 'REUSE_ALV_LIST_DISPLAY'
         exporting
              i_callback_program = w_program
              is_layout          = w_layout
              it_fieldcat        = w_fieldcat[]
              it_events          = w_eventcat[]
              it_sort            = w_sortcat[]
              i_save             = 'A'
         tables
              t_outtab           = it_itab
         exceptions
              program_error      = 1
              others             = 2.
  else.
    call function 'REUSE_ALV_LIST_DISPLAY'
         exporting
              i_callback_program = w_program
              is_layout          = w_layout
              it_fieldcat        = w_fieldcat[]
              it_events          = w_eventcat[]
              it_sort            = w_sortcat[]
              i_save             = 'A'
         tables
              t_outtab           = gt_log
         exceptions
              program_error      = 1
              others             = 2.
  endif.
endform.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  get_info_item_condition
*&---------------------------------------------------------------------*
form get_info_item_condition.
*---
  clear : it_info_item.
  refresh: it_info_item.

  select konp~kappl konp~kschl konp~kbetr konp~kpein konp~konwa
         konh~kzust konh~kosrt
           into table it_info_item
           from konp
           inner join konh
              on konp~knumh = konh~knumh
          where konp~knumh eq it_itab-knumh
            and konp~loevm_ko eq space
            and konp~kschl in s_kd.

  select single land1 into gv_land1 from lfa1
     where lifnr = it_itab-lifnr.

endform.                    " get_info_item_condition

*&---------------------------------------------------------------------*
*&      Form  change_info_condition
*&---------------------------------------------------------------------*
form change_info_condition.
*---
  data : l_field01(20),
         l_datab(10),
         l_datbi(10),
         l_kbetr(13),
         l_kpein(05),
         l_kzust like konh-kzust,
         l_kbetr_temp like konp-kbetr,
         lv_ldc(1)   type c,
         w_lineid(2) type n,
         w_field1(14) type c,
         w_field2(14) type c,
         w_field3(14) type c,
         line_count type i.

  refresh : bdcdata.
  clear : bdcdata, messtab, messtab[], l_field01,
          l_datab, l_datbi, l_kbetr.

*---
  perform bdc_field using : 'X'  'SAPMM06I'        '0100',
                            ' '  'EINA-LIFNR'      it_itab-lifnr,
                            ' '  'EINA-MATNR'      it_itab-matnr,
                            ' '  'EINE-EKORG'      it_itab-ekorg,
                            ' '  'EINE-WERKS'      space,
                            ' '  'EINA-INFNR'      space,
                            ' '  'BDC_OKCODE'      '/00'.

  perform bdc_field using : 'X'  'SAPMM06I'        '0101',
                            ' '  'BDC_OKCODE'      '=KO'.

  perform bdc_field using : 'X'  'SAPLV14A'        '0102',
                            ' '  'BDC_OKCODE'      '=NEWD'.

  if ( s_budat-low ge it_itab-datab and
       s_budat-low le it_itab-datbi ).
    write : s_budat-low   to l_datab.
  else.
    write : it_itab-datab to l_datab.
  endif.

  if not s_budat-high is initial.
    if ( s_budat-low  ge it_itab-datab and
         s_budat-high le it_itab-datbi ).
      write : s_budat-high  to l_datbi.
    else.
      write : it_itab-datbi to l_datbi.
    endif.
  else.
    write : it_itab-datbi to l_datbi.
  endif.

* The validity period of the condition created overlaps
* with conditions with shorter validity periods. These
* conditions will be deleted when you save.
* if condition type is changed....popup screen to confirm
  if it_itab-datbi = l_datbi and it_itab-datab = l_datab.
*FIXME - ANDY
  endif.


*-basic price condition
  sort it_info_item by kschl.
  perform bdc_field using : 'X'  'SAPMV13A'        '0201',
                            ' '  'RV13A-DATAB'     l_datab,
                            ' '  'RV13A-DATBI'     l_datbi.

  read table it_info_item with key kschl = c_kschl_pb00.

  if sy-subrc eq 0.
    clear : l_kbetr.
    write : it_info_item-kbetr to l_kbetr
                               currency it_info_item-konwa.
    move: it_info_item-kpein to l_kpein.

    perform bdc_field using : ' '  'KONP-KBETR(01)'  l_kbetr,
                              ' '  'KONP-KPEIN(01)'  l_kpein.
    delete it_info_item where kschl eq c_kschl_pb00.
*---
*    move : it_info_item-kbetr to it_ztmm_ldc_log-kbetr,
*           it_info_item-konwa to it_ztmm_ldc_log-waers.
  endif.

*-other conditions
*  delete it_info_item where kschl = c_kschl_fra1
*                         or kschl = c_kschl_zoth
*                         or kschl = c_kschl_zoti.

* { by IG.MOON 10/2007

  perform get_ldc_rate   using gv_land1
                               it_itab-matnr
                     changing  lv_ldc.

*  READ TABLE GT_LDC WITH KEY LAND1 = GV_LAND1
*                             MATNR = IT_ITAB-MATNR.
*  IF SY-SUBRC <> 0.
*    READ TABLE GT_LDC WITH KEY LAND1 = GV_LAND1.
*  ENDIF.
*
*  IF SY-SUBRC = 0.
*    LV_LDC = 'X'.
*  ENDIF.

* }

*-fill other condition
  w_lineid = 1.
  loop at it_info_item.

    case it_info_item-kschl.
      when c_kschl_fra1 or c_kschl_zoth or c_kschl_zoti
        or c_kschl_fra2.

      when others.
        clear : l_kbetr_temp.
        if it_info_item-konwa eq '%'.
          l_kbetr_temp = it_info_item-kbetr / 10.
          move : l_kbetr_temp to l_kbetr.
        else.
          write : it_info_item-kbetr to l_kbetr
                                     currency it_info_item-konwa.
        endif.
        move: it_info_item-kpein to l_kpein.

**  changed >> UD1K940042
        if   line_count  > 7.
          perform bdc_field    using 'X' 'SAPMV13A'  '0201'.
          perform bdc_field    using ' ' 'BDC_CURSOR' 'RV13A-DATAB'.
          perform bdc_field    using ' ' 'BDC_OKCODE' '=P+'.
          clear line_count.
          w_lineid = 1.
        endif.
        line_count = line_count + 1.

** end of change
*    if sy-tabix ne 1.
*      perform bdc_field using : 'X'  'SAPMV13A'        '0201'.
*    endif.

        w_lineid = w_lineid + 1.
        concatenate 'KONP-KSCHL(' w_lineid ')' into w_field1.
        concatenate 'KONP-KBETR(' w_lineid ')' into w_field2.
        concatenate 'KONP-KPEIN(' w_lineid ')' into w_field3.
        perform fill_condn_table using w_field1 w_field2 w_field3
                                 it_info_item-kschl l_kbetr l_kpein.
    endcase.
  endloop.
**  changed >> UD1K940042

  if   line_count  > 7.
    perform bdc_field    using 'X' 'SAPMV13A'  '0201'.
    perform bdc_field    using ' ' 'BDC_CURSOR' 'RV13A-DATAB'.
    perform bdc_field    using ' ' 'BDC_OKCODE' '=P+'.
    clear line_count.
    w_lineid = 1.
  endif.

** end of change
*LDC rate : yes -> update, no -> if condition exist, delete old one
  clear l_kpein.
  read table it_info_item with key kschl = c_kschl_fra1.
  if lv_ldc = 'X' or sy-subrc = 0.
    l_kbetr = gt_ldc-fra1.
    w_lineid = w_lineid + 1.
    concatenate 'KONP-KSCHL(' w_lineid ')' into w_field1.
    concatenate 'KONP-KBETR(' w_lineid ')' into w_field2.
    concatenate 'KONP-KPEIN(' w_lineid ')' into w_field3.
    perform fill_condn_table using w_field1 w_field2 w_field3
                             c_kschl_fra1 l_kbetr l_kpein.
  endif.

  read table it_info_item with key kschl = c_kschl_zoth.
  if lv_ldc = 'X' or sy-subrc = 0.
    l_kbetr = gt_ldc-zoth.
    w_lineid = w_lineid + 1.
    concatenate 'KONP-KSCHL(' w_lineid ')' into w_field1.
    concatenate 'KONP-KBETR(' w_lineid ')' into w_field2.
    concatenate 'KONP-KPEIN(' w_lineid ')' into w_field3.
    perform fill_condn_table using w_field1 w_field2 w_field3
                             c_kschl_zoth l_kbetr l_kpein.
  endif.

  read table it_info_item with key kschl = c_kschl_zoti.
  if lv_ldc = 'X' or sy-subrc = 0.
    l_kbetr = gt_ldc-zoti.
    w_lineid = w_lineid + 1.
    concatenate 'KONP-KSCHL(' w_lineid ')' into w_field1.
    concatenate 'KONP-KBETR(' w_lineid ')' into w_field2.
    concatenate 'KONP-KPEIN(' w_lineid ')' into w_field3.
    perform fill_condn_table using w_field1 w_field2 w_field3
                             c_kschl_zoti l_kbetr l_kpein.
  endif.

  perform bdc_field using : 'X'  'SAPMV13A'        '0201',
                            ' '  'BDC_OKCODE'      '=KDAT'.

  perform bdc_field using : 'X'  'SAPMV13A'        '0200',
                            ' '  'KONH-KZUST'      it_info_item-kzust,
                            ' '  'KONH-KOSRT'      it_info_item-kosrt,
                            ' '  'BDC_OKCODE'      '=SICH'.

*---
  perform bdc_transaction using 'ME12'.

*---
  append lines of messtab to messtabage.

  data : l_messa(80).

  clear : messtab, l_messa.

  read table messtab with key msgtyp = 'E'.

  if sy-subrc eq 0.
    move : c_red             to gt_log-linecolor.
*           messtab-msgtyp    to it_ztmm_ldc_log-msgty,
*           c_red             to it_ztmm_ldc_log-linec.
    perform get_message using    messtab-msgid
                                 messtab-msgnr
                                 messtab-msgv1
                                 messtab-msgv2
                                 messtab-msgv3
                                 messtab-msgv4
                        changing l_messa.
  endif.

  read table messtab with key msgtyp = 'S'.

  if sy-subrc eq 0.
    move : c_green           to gt_log-linecolor.
*           messtab-msgtyp    to it_ztmm_ldc_log-msgty,
*           c_green           to it_ztmm_ldc_log-linec.
    perform get_message using    messtab-msgid
                                 messtab-msgnr
                                 messtab-msgv1
                                 messtab-msgv2
                                 messtab-msgv3
                                 messtab-msgv4
                        changing l_messa.
  endif.

  move : l_messa to gt_log-messa.
*         l_messa to it_ztmm_ldc_log-messa.
endform.                    " change_info_condition

*&---------------------------------------------------------------------*
*&      Form  get_message
*&---------------------------------------------------------------------*
form get_message using    p_msgid
                          p_msgnr
                          p_msgv1
                          p_msgv2
                          p_msgv3
                          p_msgv4
                 changing p_l_messa.
*---
  call function 'MESSAGE_TEXT_BUILD'
       exporting
            msgid               = p_msgid
            msgnr               = p_msgnr
            msgv1               = p_msgv1
            msgv2               = p_msgv2
            msgv3               = p_msgv3
            msgv4               = p_msgv4
       importing
            message_text_output = p_l_messa.
endform.                    " get_message

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
form build_fieldcat.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : color
  append_fieldcat :
    w_col_pos 'LIFNR' 10 'Vendor'         'CHAR' 'X' ''      '',
    w_col_pos 'MATNR' 18 'Material'       'CHAR' 'X' ''      '',
    w_col_pos 'DATAB' 10 'Valid On'       'DATS' ''  ''      '',
    w_col_pos 'DATBI' 10 'Valid To'       'DATS' ''  ''      ''.
*    w_col_pos 'KSCHL' 04 'Cond Record'    'CHAR' ''  ''      '',
**    w_col_pos 'KBETR' 12 'Amount'         'CURR' ''  ''      'KONWA',
*    w_col_pos 'KBETR' 12 'Amount'         'CURR' ''  ''      '',
*    w_col_pos 'KONWA' 05 'Currency'       'CUKY' ''  ''      '',
*    w_col_pos 'KZUST' 03 'Reason Code'    'CHAR' ''  ''      '',
  if p_test = 'X'.

  else.
    append_fieldcat :
    w_col_pos 'MESSA' 80 'Message'        'CHAR' ''  ''      ''.
  endif.
endform.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_sortcat
*&---------------------------------------------------------------------*
form build_sortcat.

endform.                    " build_sortcat
*&---------------------------------------------------------------------*
*&      Form  append_to_log_db
*&---------------------------------------------------------------------*
*form append_to_log_db.
*  move : p_ekorg       to it_ztmm_ldc_log-ekorg,
*         it_itab-lifnr to it_ztmm_ldc_log-lifnr,
*         it_itab-matnr to it_ztmm_ldc_log-matnr.
*  if ( s_budat-low ge it_itab-datab and
*       s_budat-low le it_itab-datbi ).
*    move : s_budat-low   to it_ztmm_ldc_log-datab.
*  else.
*    move : it_itab-datab to it_ztmm_ldc_log-datab.
*  endif.
*
*  if not s_budat-high is initial.
*    if ( s_budat-low  ge it_itab-datab and
*         s_budat-high le it_itab-datbi ).
*      move : s_budat-high to it_ztmm_ldc_log-datbi.
**        WRITE : s_budat-high  TO l_datbi.
*    else.
*      move : it_itab-datbi to it_ztmm_ldc_log-datbi.
**        WRITE : it_itab-datbi TO l_datbi.
*    endif.
*  else.
*    move : it_itab-datbi to it_ztmm_ldc_log-datbi.
**      WRITE : it_itab-datbi TO l_datbi.
*  endif.
*
**    MOVE : it_itab-datbi TO it_ztmm_ldc_log-datbi.
*
*  move : it_info_item-kzust to it_ztmm_ldc_log-kzust,
*         g_fra1        to it_ztmm_ldc_log-zfra1,
*         g_zoth        to it_ztmm_ldc_log-zzoth,
*         g_zoti        to it_ztmm_ldc_log-zzoti,
*         '%'           to it_ztmm_ldc_log-konwa.
*  it_ztmm_ldc_log-erdat = it_ztmm_ldc_log-aedat = sy-datum.
*  it_ztmm_ldc_log-erzet = it_ztmm_ldc_log-aezet = sy-uzeit.
*  it_ztmm_ldc_log-ernam = it_ztmm_ldc_log-aenam = sy-uname.
*  append it_ztmm_ldc_log.
*endform.                    " append_to_log_db
*&---------------------------------------------------------------------*
*&      Form  append_to_log_itab
*&---------------------------------------------------------------------*
form append_to_log_itab.

  move : it_itab-lifnr to gt_log-lifnr,
         it_itab-matnr to gt_log-matnr.
  if ( s_budat-low ge it_itab-datab and
       s_budat-low le it_itab-datbi ).
    move : s_budat-low   to gt_log-datab.
  else.
    move : it_itab-datab to gt_log-datab.
  endif.

  if not s_budat-high is initial.
    if ( s_budat-low  ge it_itab-datab and
         s_budat-high le it_itab-datbi ).
      move : s_budat-high to gt_log-datbi.
*        WRITE : s_budat-high  TO l_datbi.
    else.
      move : it_itab-datbi to gt_log-datbi.
*        WRITE : it_itab-datbi TO l_datbi.
    endif.
  else.
    move : it_itab-datbi to gt_log-datbi.
*      WRITE : it_itab-datbi TO l_datbi.
  endif.

*    MOVE : it_itab-datbi TO gt_log-datbi.
  append gt_log.

endform.                    " append_to_log_itab
*&---------------------------------------------------------------------*
*&      Form  get_ldc_info
*&---------------------------------------------------------------------*
form get_ldc_info.
  clear: gt_ldc.
  refresh: gt_ldc.

  select * into table gt_ldc
    from ztcou116
   where kokrs = p_kokrs
     and bdatj = p_bdatj
     and ver   = 0.

  sort gt_ldc by land1 matnr.

*  select stawn kbetr into table gt_a902
*    from a902 as a
*    join konp as b
*      on a~knumh = b~knumh
*   where a~kappl = 'M'
*     and a~kschl = 'ZOA1'.
*
*  sort gt_a902 by stawn.

endform.                    " get_ldc_info
*&---------------------------------------------------------------------*
*&      Form  fill_condn_table
*&---------------------------------------------------------------------*
form fill_condn_table using p_fnam1
                            p_fnam2
                            p_fnam3
                            p_kschl
                            p_kbetr
                            p_kpein.

  perform bdc_field    using 'X' 'SAPMV13A'   '0201'.
  perform bdc_field    using ' ' 'BDC_CURSOR' p_fnam1.
  perform bdc_field    using ' ' 'BDC_OKCODE' '/00'.
  perform bdc_field    using ' ' p_fnam1      p_kschl.         "KSCHL.
  perform bdc_field    using ' ' p_fnam2      p_kbetr.         "KBETR.
  perform bdc_field    using ' ' p_fnam3      p_kpein.         "KPEIN.

endform.                    " fill_condn_table
*&---------------------------------------------------------------------*
*&      Form  GET_LDC_RATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_LAND1  text
*      -->P_IT_ITAB_MATNR  text
*      <--P_LV_LDC  text
*----------------------------------------------------------------------*
form get_ldc_rate using    p_land1
                           p_matnr
                  changing p_lv_ldc.

*-get ldc%, if no ldc maintained.. condition=ZERO
  clear: gt_ldc, p_lv_ldc.

  read table gt_ldc with key land1 = gv_land1
                             matnr = it_itab-matnr
                             binary search.
  if sy-subrc ne 0.
    read table gt_ldc with key land1 = gv_land1.
    if sy-subrc ne 0.
      read table gt_ldc with key land1 = '*'.
    endif.
  endif.

  if sy-subrc = 0.
    p_lv_ldc = 'X'.
  endif.

endform.                    " GET_LDC_RATE
