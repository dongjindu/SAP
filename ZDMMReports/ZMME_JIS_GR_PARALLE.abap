************************************************************************
* Program Name      : ZMME_JIS_GR_PARALLE
* Author            :
* Creation Date     :
* Specifications By :
* Pattern           : Report 1-1
* Development Request No :
* Addl Documentation:
* Description       : JIS GR Program
*
* Modification Logs
*
************************************************************************

REPORT zmme_jis_gr_paralle NO STANDARD PAGE HEADING
                        LINE-SIZE 132
                        LINE-COUNT 64(1)
                        MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

DATA: it_mseg LIKE TABLE OF ztmm_jisgr_his WITH HEADER LINE,
      it_tran LIKE TABLE OF ztmm_jisgr_tran WITH HEADER LINE.


**--- Internal Tables
DATA : BEGIN OF wa_itab ,
         budat LIKE mkpf-budat,
         lifnr LIKE ekko-lifnr,     " vendor
         matnr LIKE ekpo-matnr,     " material number
         ebeln LIKE ekko-ebeln,     " scheduling agreement number
         name1 LIKE lfa1-name1,     " vendor desc.
         txz01 LIKE ekpo-txz01,     " material desc.
         BWART LIKE MSEG-BWART,
*         labst LIKE mard-labst,     " quantity
         menge LIKE ekpo-menge,
         meins LIKE ekpo-meins,     " unit of measure
         werks LIKE ekpo-werks,
*         lgort LIKE ekpo-lgort,
         ebelp LIKE ekpo-ebelp,
         mblnr LIKE mkpf-mblnr,     " GR document number
         mjahr LIKE mkpf-mjahr,
         erdat LIKE sy-datum,
         erzet LIKE sy-uzeit,
         type(1),
         message(80),
         linecolor(4),     " ALV Color
       END OF wa_itab.

DATA: BEGIN OF wa_bapi_suc,
         lifnr LIKE ekko-lifnr,     " vendor
         matnr LIKE ekpo-matnr,
         ebeln LIKE ekpo-ebeln,
         ebelp LIKE ekpo-ebelp,
         mblnr LIKE mkpf-mblnr,     " GR document number
         mjahr LIKE mkpf-mjahr,
         sumsg(80) TYPE c,
        END OF wa_bapi_suc.

DATA: BEGIN OF wa_bapi_err,
        lifnr LIKE ekko-lifnr,
        matnr LIKE ekpo-matnr,
        ebeln LIKE ekpo-ebeln,
        ebelp LIKE ekpo-ebelp,
        ermsg LIKE bapiret2-message,
       END OF wa_bapi_err.

DATA: wa_print TYPE slis_print_alv.
DATA : it_itab LIKE TABLE OF wa_itab  WITH HEADER LINE,
       it_itab_122 LIKE TABLE OF wa_itab WITH HEADER LINE,
       it_itab_log LIKE TABLE OF wa_itab  WITH HEADER LINE,
       it_itab_bk LIKE TABLE OF wa_itab  WITH HEADER LINE,
       it_itab_temp LIKE TABLE OF wa_itab  WITH HEADER LINE.

DATA:  it_bapi_suc LIKE TABLE OF wa_bapi_suc WITH HEADER LINE,
       it_bapi_err LIKE TABLE OF wa_bapi_err WITH HEADER LINE.
FIELD-SYMBOLS: <fs_itab> LIKE LINE OF it_itab.

**--- BAPI
DATA : w_goodsmvt_header  LIKE bapi2017_gm_head_01,
       w_goodsmvt_code    LIKE bapi2017_gm_code,
       w_goodsmvt_headret LIKE bapi2017_gm_head_ret,
       w_materialdocument LIKE bapi2017_gm_head_ret-mat_doc,
       w_matdocumentyear  LIKE bapi2017_gm_head_ret-doc_year,
       it_goodsmvt_item
               LIKE TABLE OF bapi2017_gm_item_create  WITH HEADER LINE,
       it_goodsmvt_serialnumber
               LIKE TABLE OF bapi2017_gm_serialnumber WITH HEADER LINE,
       it_return LIKE TABLE OF bapiret2.

DATA : w_uzeit TYPE t.
DATA: w_post_date LIKE sy-datum.

*&-----Parallel process
DATA: w_taskname(4) TYPE n VALUE '0001',
      w_snd_jobs    TYPE i VALUE 1,
      w_rcv_jobs    TYPE i VALUE 1,
      w_excep_flag  TYPE c.
*&---end

CONSTANTS : c_bstyp LIKE ekko-bstyp VALUE 'L',
            c_bsart LIKE ekko-bsart VALUE 'JIS',
            c_mtart LIKE ekpo-mtart VALUE 'ROH',
            c_lgort LIKE ekpo-lgort VALUE 'P500',
            l_lgort_499 LIKE ekpo-lgort VALUE 'P499',
            c_bwart LIKE mseg-bwart VALUE '101',
            c_bwart_102 LIKE mseg-bwart VALUE '102',
            c_gm_code LIKE w_goodsmvt_code-gm_code VALUE '01',
            c_kzbew LIKE bapi2017_gm_item_create-mvt_ind VALUE 'B'.

**--- Macro
DEFINE append_fieldcat.
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
END-OF-DEFINITION.

DEFINE append_top.
  clear : w_line.
  if not &3 is initial or not &4 is initial.
    w_line-typ   = &1.
    w_line-key   = &2.
    concatenate &3 '~' &4 into w_line-info separated by space.
    append w_line to w_top_of_page.
  endif.
END-OF-DEFINITION.

DEFINE append_sortcat.
  w_sortcat-spos      = &1.
  w_sortcat-fieldname = &2.
  w_sortcat-tabname   = &3.
  w_sortcat-up        = &4.
  w_sortcat-subtot    = &5.
  append w_sortcat.
  clear : w_sortcat.
END-OF-DEFINITION.

**---

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
*SELECT-OPTIONS : s_ebeln FOR ekko-ebeln,
*                 s_matnr FOR ekpo-matnr.
*SELECTION-SCREEN ULINE.
SELECT-OPTIONS : s_mblnr FOR mkpf-mblnr,
      s_matnr FOR mara-matnr.
*      s_ebeln for ekko-ebeln no-DISPLAY.
PARAMETERS : p_budat LIKE mkpf-budat DEFAULT sy-datum,
** On 08/14/13 by Furong
*            p_fstime like sy-uzeit DEFAULT '070000',
*             p_setime like sy-uzeit DEFAULT '065959'.
             p_fstime like sy-uzeit DEFAULT '071500',
             p_setime like sy-uzeit DEFAULT '071459'.
** End on 08/14/13

*SELECT-OPTIONS : s_UZEIT for SY-UZEIT.

*PARAMETERS: P_SRVGRP LIKE RZLLITAB-CLASSNAME OBLIGATORY
*                     DEFAULT 'PG_JIS' ,
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X',
            p_noent(3) TYPE n DEFAULT 10.

SELECTION-SCREEN SKIP.
PARAMETERS: p_rver LIKE somlreci1-receiver
            DEFAULT 'HISNA_V_STEE' OBLIGATORY.
SELECTION-SCREEN SKIP.

PARAMETERS: p_srvgrp LIKE rzllitab-classname OBLIGATORY
                     DEFAULT 'PG_JIS' .
SELECTION-SCREEN END OF BLOCK block1.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-004.
SELECTION-SCREEN COMMENT 1(10) text-c01.
SELECTION-SCREEN COMMENT 12(10) text-s01.
SELECTION-SCREEN COMMENT /1(10) text-c02.
SELECTION-SCREEN COMMENT 12(10) text-s02.
SELECTION-SCREEN COMMENT /1(10) text-c03.
SELECTION-SCREEN COMMENT 12(10) text-s03.
SELECTION-SCREEN COMMENT /1(10) text-c04.
SELECTION-SCREEN COMMENT 12(10) text-s04.
SELECTION-SCREEN COMMENT /1(10) text-c05.
SELECTION-SCREEN COMMENT 12(10) text-s05.
SELECTION-SCREEN COMMENT /1(10) text-c06.
SELECTION-SCREEN COMMENT 12(10) text-s06.
SELECTION-SCREEN COMMENT /1(10) text-c07.
SELECTION-SCREEN COMMENT 12(10) text-s07.
SELECTION-SCREEN COMMENT /1(10) text-c08.
SELECTION-SCREEN COMMENT 12(10) text-s08.
SELECTION-SCREEN COMMENT /1(10) text-c09.
SELECTION-SCREEN COMMENT 12(10) text-s09.
SELECTION-SCREEN COMMENT /1(10) text-c10.
SELECTION-SCREEN COMMENT 12(10) text-s10.
SELECTION-SCREEN COMMENT /1(10) text-c11.
SELECTION-SCREEN COMMENT 12(10) text-s11.
SELECTION-SCREEN COMMENT /1(10) text-c12.
SELECTION-SCREEN COMMENT 12(10) text-s12.
SELECTION-SCREEN COMMENT /1(10) text-c13.
SELECTION-SCREEN COMMENT 12(10) text-s13.
SELECTION-SCREEN COMMENT /1(10) text-c14.
SELECTION-SCREEN COMMENT 12(10) text-s14.
SELECTION-SCREEN END OF BLOCK block2.

DATA: w_classname LIKE rzllitab-classname.

AT SELECTION-SCREEN.
  SELECT SINGLE classname INTO w_classname
                          FROM rzllitab
                          WHERE classname = p_srvgrp.
  IF sy-subrc NE 0.
    MESSAGE ID 'ZMMM' TYPE 'I' NUMBER '009' WITH text-003.
    LEAVE PROGRAM.
  ENDIF.
**---
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].

**---
TOP-OF-PAGE.
  PERFORM top_of_page.

**---
START-OF-SELECTION.

  WAIT UP TO 300 SECONDS.

  PERFORM get_data_new.

**---
END-OF-SELECTION.
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    it_itab_bk[] = it_itab[].

    PERFORM posting_gr_new using c_bwart.

** Negtive qty
    IF NOT it_itab_122[] IS INITIAL.
** On 03/13/14 posting 102 - requested by Mr. Lee
      it_itab_temp[] = it_itab[].
      it_itab_bk[] = it_itab_122[].
      PERFORM posting_gr_new using c_bwart_102.
      it_itab[] = it_itab_temp[].
*      PERFORM update_and_send_email.
** End
    ENDIF.
    IF p_test IS INITIAL.
      PERFORM save_to_table.
    ENDIF.

    PERFORM comment_build.     " USING w_top_of_page[].
    PERFORM make_alv_grid.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM comment_build.
*---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-002.
  APPEND w_line TO w_top_of_page.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page.
ENDFORM.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_alv_grid.

  APPEND LINES OF it_itab_122 TO it_itab .

  MOVE : 'LINECOLOR' TO w_layout-info_fieldname,
         'X'         TO w_layout-colwidth_optimize.

  PERFORM build_fieldcat.
  PERFORM build_sortcat.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.
*  wa_print-print = 'X'.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program = w_program
      is_layout          = w_layout
      it_fieldcat        = w_fieldcat[]
      it_events          = w_eventcat[]
      it_sort            = w_sortcat[]
      i_save             = 'A'
*     is_print           = wa_print
    TABLES
      t_outtab           = it_itab
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  posting_gr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posting_gr using p_bwart.
*---
  DATA: BEGIN OF wa_vendor,
          lifnr LIKE lfa1-lifnr,
          count TYPE i,
        END OF wa_vendor.
  DATA: it_vendor LIKE TABLE OF wa_vendor.

  DATA: w_idx TYPE i VALUE 1,
        w_cnt TYPE i,
        w_grp_cnt TYPE i.

*&---calculate no.of records for each vendor.
  LOOP AT it_itab ASSIGNING <fs_itab>.
    wa_vendor-lifnr = <fs_itab>-lifnr.
    wa_vendor-count = 1.
    COLLECT wa_vendor INTO it_vendor.
  ENDLOOP.

  SORT: it_vendor BY lifnr,
        it_itab BY lifnr ebeln.
  LOOP AT it_vendor INTO wa_vendor.
    LOOP AT it_itab ASSIGNING <fs_itab> FROM w_idx.
      IF wa_vendor-lifnr NE <fs_itab>-lifnr.
        w_idx = sy-tabix.
        CLEAR : w_cnt, w_grp_cnt,
                w_goodsmvt_header, w_goodsmvt_code,w_goodsmvt_headret,
                w_materialdocument, w_matdocumentyear,
                it_goodsmvt_item, it_goodsmvt_item[],
                it_goodsmvt_serialnumber, it_goodsmvt_serialnumber[].
        EXIT.
      ELSE.
        w_cnt = w_cnt + 1.
        w_grp_cnt = w_grp_cnt + 1.
        PERFORM clear_data_move using p_bwart.
        IF w_cnt EQ p_noent OR w_grp_cnt EQ wa_vendor-count.
          PERFORM call_bapi.
          CLEAR : w_cnt,w_goodsmvt_header,w_goodsmvt_code,
                w_goodsmvt_headret,w_materialdocument,w_matdocumentyear,
                  it_goodsmvt_item, it_goodsmvt_item[],
                  it_goodsmvt_serialnumber, it_goodsmvt_serialnumber[].
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  WHILE w_rcv_jobs < w_snd_jobs.
    WAIT UNTIL w_rcv_jobs >= w_snd_jobs.
  ENDWHILE.

  PERFORM modify_itab.
ENDFORM.                    " posting_gr

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : color
  append_fieldcat :
   w_col_pos 'LIFNR' 10 'Vendor'         'CHAR' 'X' ''      '',
   w_col_pos 'BUDAT' 10 'Posting Date'   'CHAR' 'X' ''      '',
   w_col_pos 'EBELN' 10 'SA Number'      'CHAR' 'X' ''      '',
   w_col_pos 'MATNR' 18 'Material'       'CHAR' 'X' ''      '',
*   w_col_pos 'TXZ01' 20 'Material Name'  'CHAR' ''  ''      '',
*    w_col_pos 'LABST' 12 'Quantity'       'QUAN' ''  'MEINS' '',
   w_col_pos 'MENGE' 12 'Quantity'       'QUAN' ''  'MEINS' '',
   w_col_pos 'MEINS'  3 'UoM'            'CHAR' ''  ''      '',
*   w_col_pos 'BWART' 4 'Mvt Type'        'CHAR' ''  ''      '',
   w_col_pos 'MBLNR' 10 'Document No'    'CHAR' ''  ''      '',
   w_col_pos 'TYPE' 2 'Ty'               'CHAR' ''  ''      '',
   w_col_pos 'MESSAGE' 80 'Message'        'CHAR' ''  ''      ''.

ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_sortcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat.
**--- &1 : position     &2 : field name     &3 : tab name
**--- &4 : up           &5 : sub total
*  append_sortcat : '1' 'LIFNR' 'IT_ITAB' 'X' '',
*                   '2' 'EBELN' 'IT_ITAB' 'X' ''.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  clear_data_move
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_data_move using p_bwart.
  DATA: l_belnr LIKE ekbe-belnr,
         l_gjahr LIKE ekbe-gjahr,
         l_buzei LIKE ekbe-buzei,
         l_budat LIKE ekbe-budat.


  MOVE : <fs_itab>-matnr TO it_goodsmvt_item-material,
         <fs_itab>-werks TO it_goodsmvt_item-plant,
*         <fs_itab>-lgort TO it_goodsmvt_item-stge_loc,
         c_lgort TO it_goodsmvt_item-stge_loc,
** On 03/18/14
         p_bwart TO it_goodsmvt_item-move_type,
*         c_bwart             TO it_goodsmvt_item-move_type,
** End
         <fs_itab>-lifnr TO it_goodsmvt_item-vendor,
*         it_itab-labst TO it_goodsmvt_item-entry_qnt,
         <fs_itab>-meins TO it_goodsmvt_item-entry_uom,
         <fs_itab>-meins TO it_goodsmvt_item-entry_uom_iso,
         <fs_itab>-ebeln TO it_goodsmvt_item-po_number,
         <fs_itab>-ebelp TO it_goodsmvt_item-po_item,
         c_kzbew TO it_goodsmvt_item-mvt_ind.

** On 03/18/14
*    it_goodsmvt_item-entry_qnt = <fs_itab>-menge.
  case  p_bwart.
    when '101'.
      it_goodsmvt_item-entry_qnt = <fs_itab>-menge.
      APPEND it_goodsmvt_item.
    when '102'.
      data: lt_ekbe like TABLE OF ekbe with HEADER LINE.
      data: begin of lt_temp OCCURS 0,
*           BUDAT like ekbe-budat,
*           mblnr like ekbe-matnr,
*           mjahr like ekbe-mjahr,
            LFGJA like ekbe-LFGJA,
            LFbnr like ekbe-LFbnr,
            lfpos like ekbe-lfpos,
            MENGE like ekbe-menge,
        end of lt_temp.
      data: begin of lt_matdoc OCCURS 0,
            BUDAT like ekbe-budat,
            LFGJA like ekbe-LFGJA,
            LFbnr like ekbe-LFbnr,
            lfpos like ekbe-lfpos,
            MENGE like ekbe-menge,
        end of lt_matdoc.

      data: l_frdate like sy-datum,
            l_qty like ekbe-menge.

      l_frdate = sy-datum - 60.
      select * into TABLE lt_ekbe
        from ekbe
        where ebeln = <fs_itab>-ebeln
          and ebelp = <fs_itab>-ebelp
          and BEWTP = 'E'
          and BUDAT BETWEEN l_frdate AND sy-datum.
      if sy-subrc <> 0.
        l_frdate = sy-datum - 120.
        select * into TABLE lt_ekbe
          from ekbe
          where ebeln = <fs_itab>-ebeln
            and ebelp = <fs_itab>-ebelp
            and BEWTP = 'E'
            and BUDAT BETWEEN l_frdate AND sy-datum.
        if sy-subrc <> 0.
          clear: wa_bapi_err.
          wa_bapi_err-lifnr = it_goodsmvt_item-vendor.
          wa_bapi_err-matnr = it_goodsmvt_item-material.
          wa_bapi_err-ebeln = it_goodsmvt_item-po_number.
          wa_bapi_err-ebelp = it_goodsmvt_item-po_item.
          wa_bapi_err-ermsg = 'No GR Document found for reversal'.
          APPEND wa_bapi_err TO it_bapi_err.
          clear: wa_bapi_err.
          exit.
        endif.
      else.
        sort lt_ekbe by LFGJA LFBNR LFPOS.
        loop at lt_ekbe.
          MOVE-CORRESPONDING lt_ekbe to lt_temp.
          if  lt_ekbe-bwart = '102' or  lt_ekbe-bwart = '122'.
            lt_temp-menge =  lt_temp-menge * -1.
          endif.
          collect lt_temp.
        endloop.

        delete lt_temp WHERE menge <= 0.
        loop at lt_temp.
          MOVE-CORRESPONDING lt_temp to lt_matdoc.
          read table lt_ekbe WITH key BELNR = lt_temp-lfbnr
                                      BUZEI = lt_temp-lfpos
                                      GJAHR = lt_temp-lfgja.

          lt_matdoc-budat = lt_ekbe-budat.
** Furong on 04/29/14  102 posting date check (
          IF lt_matdoc-budat >  <fs_itab>-BUDAT.
          ELSE.
** End  )
          append lt_matdoc.
          ENDIF.
        endloop.
        sort lt_matdoc by budat DESCENDING.
        l_qty = <fs_itab>-menge.
        loop at lt_matdoc.
          if lt_matdoc-menge >= l_qty.
            it_goodsmvt_item-entry_qnt = l_qty.
            it_goodsmvt_item-REF_DOC = lt_matdoc-lfbnr.
            it_goodsmvt_item-REF_DOC_it = lt_matdoc-lfpos.
            it_goodsmvt_item-REF_DOC_YR = lt_matdoc-lfgja.
            APPEND it_goodsmvt_item.
            exit.
          else.
            it_goodsmvt_item-entry_qnt = lt_matdoc-menge.
            it_goodsmvt_item-REF_DOC = lt_matdoc-lfbnr.
            it_goodsmvt_item-REF_DOC_it = lt_matdoc-lfpos.
            it_goodsmvt_item-REF_DOC_YR = lt_matdoc-lfgja.
            APPEND it_goodsmvt_item.
            l_qty = l_qty - lt_matdoc-menge.
          endif.
        endloop.
      endif.
  endcase.
ENDFORM.                    " clear_data_move

*&---------------------------------------------------------------------*
*&      Form  call_bapi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bapi.
*---
  DATA : l_budat TYPE d.

  w_goodsmvt_header-pstng_date = w_post_date.
  CONCATENATE  'JIS-GR' w_goodsmvt_header-pstng_date
          INTO w_goodsmvt_header-ref_doc_no.

  MOVE : sy-datum  TO w_goodsmvt_header-doc_date,
         c_gm_code TO w_goodsmvt_code-gm_code.

  DO.
    CALL FUNCTION 'Z_FMM_JISGR_CREATE'
      STARTING NEW TASK w_taskname
      DESTINATION IN GROUP p_srvgrp
      PERFORMING gr_info ON END OF TASK
      EXPORTING
        wa_gdsmvt_hdr         = w_goodsmvt_header
        wa_gdsmvt_code        = w_goodsmvt_code
        w_tstrun              = p_test
      TABLES
        it_gdsmvt_itm         = it_goodsmvt_item
        it_gdsmvt_sno         = it_goodsmvt_serialnumber
        it_ret                = it_return
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        resource_failure      = 3.

    CASE sy-subrc.
      WHEN 0.
        w_taskname = w_taskname + 1.
        w_snd_jobs = w_snd_jobs + 1.
        CLEAR w_excep_flag.
        EXIT.
      WHEN 1 OR 2.
        w_excep_flag = 'X'.
      WHEN 3.
        IF w_excep_flag = space.
          w_excep_flag = 'X'.
          WAIT UNTIL w_rcv_jobs >= w_snd_jobs UP TO '0.01' SECONDS.
        ELSE.
          WAIT UNTIL w_rcv_jobs >= w_snd_jobs UP TO '0.1' SECONDS.
        ENDIF.
*      if sy-subrc eq 0.
*        clear w_excep_flag.
*      else.
*        exit.
*      endif.
    ENDCASE.
  ENDDO.

ENDFORM.                    " call_bapi

*&---------------------------------------------------------------------*
*&      Form  modify_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_itab.

  CLEAR: wa_itab.

  LOOP AT it_bapi_suc INTO wa_bapi_suc.
    wa_itab-mblnr     = wa_bapi_suc-mblnr.
    wa_itab-mjahr     = wa_bapi_suc-mjahr.
    wa_itab-message    = wa_bapi_suc-sumsg.
    wa_itab-linecolor = c_green.
    wa_itab-type = 'S'.
    MODIFY it_itab FROM wa_itab
                   TRANSPORTING linecolor message mblnr mjahr type
                   WHERE lifnr = wa_bapi_suc-lifnr
                   AND budat = w_post_date
                   AND   ebeln = wa_bapi_suc-ebeln
                   AND   ebelp = wa_bapi_suc-ebelp.
    CLEAR: wa_itab.

    it_mseg-type = 'S'.
    it_mseg-message = wa_bapi_suc-sumsg..

    MODIFY it_mseg TRANSPORTING type message
                   WHERE lifnr = wa_bapi_suc-lifnr
                   AND budat = w_post_date
                   AND ebeln = wa_bapi_suc-ebeln
                   AND matnr = wa_bapi_suc-matnr.

  ENDLOOP.
  LOOP AT it_bapi_err INTO wa_bapi_err.
    wa_itab-message     = wa_bapi_err-ermsg.
    wa_itab-linecolor = c_red.
    wa_itab-type = 'E'.
    MODIFY it_itab FROM wa_itab TRANSPORTING message linecolor type
                                WHERE ebeln = wa_bapi_err-ebeln
                                AND   ebelp = wa_bapi_err-ebelp.
    CLEAR: wa_itab.
    it_mseg-type = 'E'.
    it_mseg-message = wa_bapi_suc-sumsg..

    MODIFY it_mseg TRANSPORTING type message
                   WHERE lifnr = wa_bapi_err-lifnr
                   AND budat = w_post_date
                   AND ebeln = wa_bapi_err-ebeln
                   AND matnr = wa_bapi_err-matnr.
  ENDLOOP.
  APPEND LINES OF it_itab TO it_itab_log.
ENDFORM.                    " modify_itab
*&---------------------------------------------------------------------*
*&      Form  gr_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gr_info USING w_taskname.

  DATA: wa_item_info LIKE bapi2017_gm_item_create.
  DATA: it_item_info LIKE TABLE OF wa_item_info.
  DATA:   it_goodsmvt_item_ret
               LIKE TABLE OF bapi2017_gm_item_create  WITH HEADER LINE.

  FIELD-SYMBOLS: <fs_ret> LIKE LINE OF it_return.

  RECEIVE RESULTS FROM FUNCTION 'Z_FMM_JISGR_CREATE'
            IMPORTING
              wa_gdsmvt_hdrrtn = w_goodsmvt_headret
              w_matdoc         = w_materialdocument
              w_mdyear         = w_matdocumentyear
           TABLES
             it_gdsmvt_itm = it_goodsmvt_item_ret
             it_gdsmvt_sno = it_goodsmvt_serialnumber
             it_ret        = it_return
           EXCEPTIONS
             communication_failure = 1
             system_failure        = 2.

  IF sy-subrc NE 0.
    w_excep_flag = 'X'.
    EXIT.
  ENDIF.

  w_rcv_jobs = w_rcv_jobs + 1.

  READ TABLE it_return ASSIGNING <fs_ret> WITH KEY type = 'E'.
  IF sy-subrc EQ 0.
    LOOP AT it_return ASSIGNING <fs_ret>.
      READ TABLE it_goodsmvt_item_ret INDEX <fs_ret>-row.
      IF sy-subrc NE 0.
      ELSE.
        wa_bapi_err-lifnr = it_goodsmvt_item-vendor.
        wa_bapi_err-matnr = it_goodsmvt_item-material.
        wa_bapi_err-ebeln = it_goodsmvt_item_ret-po_number.
        wa_bapi_err-ebelp = it_goodsmvt_item_ret-po_item.
        wa_bapi_err-ermsg = <fs_ret>-message.
        APPEND wa_bapi_err TO it_bapi_err.
      ENDIF.
      CLEAR: it_goodsmvt_item_ret, wa_bapi_err.
    ENDLOOP.
  ELSE.
    it_item_info[] = it_goodsmvt_item_ret[].
    LOOP AT it_item_info INTO wa_item_info.
      wa_bapi_suc-lifnr = wa_item_info-vendor.
      wa_bapi_suc-matnr = wa_item_info-material.
      wa_bapi_suc-ebeln = wa_item_info-po_number.
      wa_bapi_suc-ebelp = wa_item_info-po_item.
      wa_bapi_suc-mblnr = w_materialdocument.
      wa_bapi_suc-mjahr = w_matdocumentyear.
      IF wa_item_infO-MOVE_TYPE = '102'.
        wa_bapi_suc-sumsg = 'Reversal Document created'.
      ELSE.
        wa_bapi_suc-sumsg = 'GR Document created'.
      ENDIF.
      APPEND wa_bapi_suc TO it_bapi_suc.
      CLEAR: wa_bapi_suc.
    ENDLOOP.
  ENDIF.
  REFRESH: it_return, it_item_info.
ENDFORM.                    " gr_info
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_new .
  DATA: l_labst LIKE mard-labst,
       l_index LIKE sy-tabix,
       l_date LIKE sy-datum,
       l_message(80),
       l_erdat LIKE sy-datum,
       l_erzet LIKE sy-uzeit,
       l_ernam LIKE sy-uname,
       l_string(10).

  DATA: lt_zhis LIKE TABLE OF ztmm_jisgr_his  WITH HEADER LINE.

  l_date = p_budat - 1.

  CLEAR : it_itab, it_itab[],
          it_mseg, it_mseg[].

  l_string = '465%'.

  SELECT a~mblnr zeile a~mjahr budat cpudt cputm b~matnr
         b~werks b~lgort bwart b~menge b~meins c~ebeln c~ebelp
         shkzg c~lifnr
         INTO CORRESPONDING FIELDS OF TABLE it_mseg
         FROM mkpf AS a
       INNER JOIN mseg AS b
         ON a~mblnr = b~mblnr
         AND a~mjahr = b~mjahr
        INNER JOIN eord AS c
        ON b~matnr = c~matnr
        INNER JOIN ekko AS d
         ON c~ebeln = d~ebeln
    AND c~ebeln = d~ebeln
         WHERE bstyp = c_bstyp
          AND bsart = c_bsart
** For testing
**          AND b~matnr IN s_matnr
         AND cpudt = l_date
         AND cputm BETWEEN p_fstime AND '235959'
*         AND cputm BETWEEN '063000' AND '235959'
         AND b~werks = c~werks
** For testing
*         AND a~mblnr IN s_mblnr
         AND c~notkz = space
         AND c~ebeln  LIKE l_string "in s_ebeln
         AND ( ( b~lgort = '9999'
                 AND b~bwart IN ('261', '262')
                 AND b~sakto = '0000540020' )
            OR  b~lgort = 'P500'
            OR  b~lgort = 'P499' ).

  SELECT a~mblnr zeile a~mjahr budat cpudt cputm b~matnr
         b~werks b~lgort bwart b~menge b~meins c~ebeln c~ebelp
         shkzg c~lifnr
         APPENDING CORRESPONDING FIELDS OF TABLE it_mseg
         FROM mkpf AS a
       INNER JOIN mseg AS b
         ON a~mblnr = b~mblnr
         AND a~mjahr = b~mjahr
        INNER JOIN eord AS c
        ON b~matnr = c~matnr
        INNER JOIN ekko AS d
         ON c~ebeln = d~ebeln
    AND c~ebeln = d~ebeln
         WHERE bstyp = c_bstyp
          AND bsart = c_bsart
** For testing
*             AND b~matnr IN s_matnr
          AND cpudt = p_budat
          AND cputm BETWEEN '000000' AND p_setime
*           AND cputm BETWEEN '000000' AND  '062959'
           AND b~werks = c~werks
** For testing
*         AND a~mblnr IN s_mblnr
         AND c~notkz = space
        AND c~ebeln  LIKE l_string "in s_ebeln
            AND ( ( b~lgort = '9999'
                 AND b~bwart IN ('261', '262')
                 AND b~sakto = '0000540020' )
            OR  b~lgort = 'P500'
            OR  b~lgort = 'P499' ).

  LOOP AT it_mseg.
    CASE it_mseg-lgort.
      WHEN 'P500'.
        IF it_mseg-bwart = '261' OR
           it_mseg-bwart = '262' OR
          it_mseg-bwart =  '201' OR
          it_mseg-bwart = '202' OR
          it_mseg-bwart = '551' OR
          it_mseg-bwart = '552' OR
          it_mseg-bwart = '122' OR
          it_mseg-bwart = '123'.
        ELSE.
          DELETE it_mseg.
        ENDIF.
      WHEN 'P499'.
        IF it_mseg-bwart = '551' OR
        it_mseg-bwart = '552' OR
        it_mseg-bwart = '122' OR
        it_mseg-bwart = '123'.
        ELSE.
          DELETE it_mseg.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  CHECK NOT it_mseg[] IS INITIAL.

** Delete deplicated data
  SORT it_mseg BY mblnr mjahr budat zeile.   "matnr.
  DELETE ADJACENT DUPLICATES FROM it_mseg COMPARING
     mblnr mjahr budat zeile.

*  SORT it_mseg BY mblnr mjahr budat matnr.
*  DELETE ADJACENT DUPLICATES FROM it_mseg COMPARING
*     mblnr mjahr budat matnr.

** Check multi-run
  SELECT * INTO TABLE lt_zhis
    FROM ztmm_jisgr_his
    FOR ALL ENTRIES IN it_mseg
    WHERE mblnr = it_mseg-mblnr
      AND zeile = it_mseg-zeile
      AND mjahr = it_mseg-mjahr.
  IF sy-subrc = 0.
    READ TABLE lt_zhis INDEX 1.
    CONCATENATE 'Data already processed: '
       lt_zhis-mblnr lt_zhis-matnr  " LT_ZHIS-BUDAT
      INTO l_message SEPARATED BY space.
    MESSAGE e009 WITH l_message.
    EXIT.
  ENDIF.

  l_erdat = sy-datum.
  l_erzet = sy-uzeit.
  l_ernam = sy-uname.

** Set negtive sign
  LOOP AT it_mseg.
    l_index = sy-tabix.
    it_mseg-erdat = l_erdat.
    it_mseg-erzet = l_erzet.
    it_mseg-ernam = l_ernam.
    MODIFY it_mseg INDEX l_index TRANSPORTING erdat erzet ernam.

*    MOVE-CORRESPONDING it_mseg TO wa_itab.
*    MOVE: it_mseg-mblnr TO wa_itab-mblnr,
*          it_mseg-mjahr TO wa_itab-mjahr,
    MOVE: it_mseg-budat TO wa_itab-budat,
         it_mseg-matnr TO wa_itab-matnr,
         it_mseg-ebeln TO wa_itab-ebeln,
         it_mseg-ebelp TO wa_itab-ebelp,
         it_mseg-werks TO wa_itab-werks,
         it_mseg-lifnr TO wa_itab-lifnr,
         it_mseg-meins TO wa_itab-meins,
         it_mseg-menge TO wa_itab-menge.
    wa_itab-erdat = l_erdat.
    wa_itab-erzet = l_erzet.

    IF it_mseg-shkzg = 'S'.
      wa_itab-menge = wa_itab-menge * -1.
    ENDIF.
*    CLEAR: wa_itab-mblnr, wa_itab-mjahr.

    COLLECT wa_itab INTO it_itab.

  ENDLOOP.

** Summrize data to it_itab/it_itab_122
  LOOP AT it_itab INTO wa_itab.
    l_index = sy-tabix.
    IF wa_itab-menge > 0.
** On 03/13/14  - add bwart
      wa_itab-bwart = c_bwart.
      modify it_itab from wa_itab index l_index
        TRANSPORTING bwart.
** End
    ELSE.
*     DELETE it_itab INDEX l_index.
      IF wa_itab-menge < 0.
** On 03/13/14  - posting 102
        wa_itab-menge = wa_itab-menge * -1.
        wa_itab-bwart = c_bwart_102.
*        wa_itab-type = 'E'.
*        wa_itab-message = 'Not get posted due to negtive QTY'.
** End on 03/13/14
        APPEND wa_itab TO it_itab_122.
        delete it_itab index l_index.
      ENDIF.
    ENDIF.
    CLEAR: wa_itab.
  ENDLOOP.

  SORT it_itab BY  budat lifnr matnr.

ENDFORM.                    " GET_DATA_NEW
*&---------------------------------------------------------------------*
*&      Form  POSTING_GR_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posting_gr_new using p_bwart.

  READ TABLE it_itab_bk INDEX 1.
  w_post_date = it_itab_bk-budat.
  REFRESH it_itab.
  LOOP AT it_itab_bk.
    IF w_post_date NE it_itab_bk-budat.
      FREE: it_bapi_err, it_bapi_suc,it_goodsmvt_item,
      it_goodsmvt_serialnumber, w_goodsmvt_header,
       w_goodsmvt_code, w_goodsmvt_headret.
      CLEAR : w_goodsmvt_header, w_goodsmvt_code,
              w_goodsmvt_headret,
              w_materialdocument, w_matdocumentyear,
              it_goodsmvt_item,
              it_goodsmvt_serialnumber,
              w_rcv_jobs, w_snd_jobs, w_excep_flag.
      PERFORM posting_gr using p_bwart.
      w_post_date = it_itab_bk-budat.
      REFRESH: it_itab.
      CLEAR: it_itab.
      MOVE-CORRESPONDING it_itab_bk TO it_itab.
      it_itab-bwart = p_bwart.
      APPEND it_itab.
      WAIT UP TO 60 SECONDS.
    ELSE.
      CLEAR: it_itab.
      MOVE-CORRESPONDING it_itab_bk TO it_itab.
             it_itab-bwart = p_bwart.
      APPEND it_itab.

    ENDIF.
  ENDLOOP.
  IF NOT it_itab[] IS INITIAL.
    FREE: it_bapi_err, it_bapi_suc,it_goodsmvt_item,
          it_goodsmvt_serialnumber, w_goodsmvt_header,
          w_goodsmvt_code, w_goodsmvt_headret.
    CLEAR : w_goodsmvt_header, w_goodsmvt_code,
            w_goodsmvt_headret,
            w_materialdocument, w_matdocumentyear,
            it_goodsmvt_item,
            it_goodsmvt_serialnumber,
            w_rcv_jobs, w_snd_jobs, w_excep_flag..
    PERFORM posting_gr using p_bwart.
  ENDIF.
** On 03/13/14
  if p_bwart = '102'.
    it_itab_122[] = it_itab_log[].
  else.
** End
    it_itab[] = it_itab_log[].
  endif.
  FREE: it_itab_bk, it_itab_log.
ENDFORM.                    " POSTING_GR_NEW
*&---------------------------------------------------------------------*
*&      Form  SAVE_TO_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_to_table .
  DATA: l_date LIKE sy-datum.

  CLEAR: it_tran, it_tran[].
  LOOP AT it_itab INTO wa_itab.
    MOVE-CORRESPONDING wa_itab TO it_tran.
    APPEND it_tran.
    CLEAR: it_tran.
  ENDLOOP.
  LOOP AT it_itab_122 INTO wa_itab.
    MOVE-CORRESPONDING wa_itab TO it_tran.
    APPEND it_tran.
    CLEAR: it_tran.
  ENDLOOP.

  l_date = sy-datum - 90.
  DELETE FROM ztmm_jisgr_his WHERE erdat < l_date.
  INSERT ztmm_jisgr_his FROM TABLE it_mseg
                        ACCEPTING DUPLICATE KEYS .
  IF sy-subrc = 0 OR sy-subrc = 4.
    COMMIT WORK AND WAIT.
    l_date = sy-datum - 180.
    DELETE FROM ztmm_jisgr_tran WHERE erdat < l_date.

    INSERT ztmm_jisgr_tran FROM TABLE it_tran
                        ACCEPTING DUPLICATE KEYS .
    IF sy-subrc = 0 OR sy-subrc = 4.
      COMMIT WORK.
    ELSE.
      MESSAGE e009 WITH 'Update failed for log table'.
      ROLLBACK WORK.
    ENDIF.

  ELSE.
    MESSAGE e009 WITH 'Update failed for history table'.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " SAVE_TO_TABLE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_AND_SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_and_send_email .
  PERFORM update_status.
  PERFORM send_email.
ENDFORM.                    " UPDATE_AND_SEND_EMAIL
*&---------------------------------------------------------------------*
*&      Form  UPDATE_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_status .
  LOOP AT it_itab_122 INTO wa_itab.
    it_mseg-type = wa_itab-type.
    it_mseg-message = wa_itab-message.
    MODIFY it_mseg TRANSPORTING type message
                      WHERE lifnr = wa_itab-lifnr
                      AND budat = wa_itab-budat
                      AND ebeln = wa_itab-ebeln
                      AND matnr = wa_itab-matnr.
  ENDLOOP.
ENDFORM.                    " UPDATE_STATUS
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email .

  DATA: lt_body LIKE TABLE OF solisti1 WITH HEADER LINE.

  MOVE 'Following items with negtive quantity' TO lt_body.

  APPEND lt_body.
  CLEAR: lt_body.
  MOVE '=====================================' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: 'Material No' TO lt_body+0(20),
        'Vendor' TO lt_body+20(15),
        'Post Date' TO lt_body+35(10),
        'Quantity' TO lt_body+45(15).

  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: '------------------------' TO  lt_body+0(20),
        '---------------' TO  lt_body+20(15),
        '----------' TO  lt_body+35(10),
         '---------------' TO  lt_body+45(15).
  APPEND lt_body.
  CLEAR: lt_body.

  LOOP AT it_itab_122 INTO wa_itab.
    MOVE: wa_itab-matnr TO lt_body+0(20),
          wa_itab-lifnr TO lt_body+20(15),
          wa_itab-budat TO lt_body+35(10),
          wa_itab-menge TO lt_body+45(15).
    APPEND lt_body.
  ENDLOOP.

  CALL FUNCTION 'ZCAF_SEND_EMAIL'
    EXPORTING
      p_subject  = 'JIS-GR Posting Error with negtive QTY'
      p_rec_type = 'C'
      p_receiver = p_rver
    TABLES
      pt_body    = lt_body.
ENDFORM.                    " SEND_EMAIL
