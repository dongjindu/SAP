*&---------------------------------------------------------------------*
*& Report  ZRIMTCHGLST                                                 *
*&---------------------------------------------------------------------*
*&  프로그램명 : Term Charge 만기일 조회를 위한 Report Program.        *
*&      작성자 : 나신호 INFOLINK Ltd.                                  *
*&      작성일 : 2002.11.25                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : L/C 원장관리 현황리스트를 조회하고 세부화면에서 확인. *
*&---------------------------------------------------------------------*
*& [변경내용]
*&---------------------------------------------------------------------*
report  zrimtchglst  message-id  zim
                     line-size   126
                     no standard page heading.
type-pools : slis.
*-----------------------------------------------------------------------
* Tables 및 변수 Definition.
*-----------------------------------------------------------------------
include   zrimtchglsttop.
include   zrimutil01.             " Utility function 모음.

*-----------------------------------------------------------------------
* Selection Screen Clause.
*-----------------------------------------------------------------------
selection-screen skip 1.                           " 2 LINE SKIP
selection-screen begin of block b1 with frame title text-001.
parameters :    p_bukrs   like ztreqhd-bukrs      " 회사코드.
                          obligatory default 'PSC'.
select-options: s_werks   for ztreqhd-zfwerks,   " Plant..
                s_reqed   for ztreqhd-zfreqed,   " 의뢰 유효일..
                s_pwdt    for ztpmthd-zfpwdt,    " Usance 만기일..
*       s_tcedt   for ztbseg-zftcedt,    " Term Charge 종료일.. NCW 막
                s_ebeln   for ztreqhd-ebeln,     " Purchasing document..
                s_reqno   for ztreqhd-zfreqno,   " 수입의뢰 관리번호..
                s_reqty   for ztreqhd-zfreqty,   " 수입의뢰 Type..
                s_opnno   for ztreqhd-zfopnno,   " 신용장 승인번호..
                s_opbn    for ztreqhd-zfopbn,    " 개설은행.
                s_opndt   for ztreqst-zfopndt.   " 신용장 개설일..
selection-screen end of block b1.

selection-screen begin of block b2 with frame title text-002.
parameters:     p_alv     as checkbox.
selection-screen end of block b2.
* Parameter 초기값 Setting.
initialization.                          " 초기값 SETTING
  perform   p2000_set_parameter.

*----------------------------------------------------------------------*
* Top of Page.
*----------------------------------------------------------------------*
top-of-page.
  perform p3000_title_write.

*-----------------------------------------------------------------------
* Start of Selection.
*-----------------------------------------------------------------------
start-of-selection.
* Import System Config Check
  perform   p2000_config_check        using   w_err_chk.

  perform  p2000_read_data        using  w_err_chk.
  if w_err_chk = 'Y'.  exit.   endif.

  perform p3000_write_data.

*-----------------------------------------------------------------------
* User Command.
*-----------------------------------------------------------------------
at user-command.
  w_ok_code = sy-ucomm.
  case sy-ucomm.
    when 'LCGL'. " L/C 원장 상세 조회.
      set parameter id 'ZPREQNO' field it_tab-zfreqno.
      set parameter id 'ZPAMDNO' field it_tab-zfamdno.
      call transaction 'ZIMGL1' and skip first screen.
    when 'SHLC'.
      set parameter id 'ZPOPNNO' field ''.
      set parameter id 'BES'     field ''.
      set parameter id 'ZPREQNO' field it_tab-zfreqno.
      call transaction 'ZIM03' and skip first screen.
    when 'SHPO'.
      set parameter id 'BES'     field it_tab-ebeln.
      call transaction 'ME23N' and skip first screen.
    when others.

  endcase.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
form p2000_set_parameter.
  set  titlebar 'ZIMR24C'.          " TITLE BAR
endform.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P2000_READ_DATA
*&---------------------------------------------------------------------*
form p2000_read_data using    p_w_err_chk.

  w_err_chk = 'N'.                     " Error Bit Setting

  select *
    from ztreqhd
   where bukrs   eq p_bukrs
     and zfwerks in s_werks
     and zfreqno in s_reqno
     and zfreqty in s_reqty
     and zfreqed in s_reqed
     and ebeln   in s_ebeln
     and zfopbn  in s_opbn
     and zfopnno in s_opnno
     and zflckn  in ('2', '3')
     and zfclose eq space.
    if sy-subrc eq 0.
      move-corresponding ztreqhd to it_tab.

      select single *
               from lfa1
              where lifnr = it_tab-zfopbn. " ztreqhd-zfopbn.

      if sy-subrc eq 0.
* 수입의뢰 상태 Table Select.
        select single *
                 from ztreqst
                where zfreqno eq it_tab-zfreqno "ztreqhd-zfreqno
                  and zfdocst eq 'O'
                  and zfopndt in s_opndt.
*              and zfamdno eq ( select max( zfamdno ) from ztreqst ).
        if sy-subrc eq 0.
* 수입의뢰 Release 상태를 Check.
          if ztreqst-zfamdno is initial.
            if ztimimg00-zfrelyn1 eq 'X'.
              if ztreqst-zfrlst1 ne 'R'. continue. endif.
            endif.
            if ztimimg00-zfrelyn2 eq 'X'.
              if ztreqst-zfrlst2 ne 'R'. continue. endif.
            endif.
          else.
            if ztimimg00-zfrelyn3 eq 'X'.
              if ztreqst-zfrlst1 ne 'R'. continue. endif.
            endif.
            if ztimimg00-zfrelyn4 eq 'X'.
              if ztreqst-zfrlst2 ne 'R'. continue. endif.
            endif.
          endif.

* Payment Notice Table Select.
          select single *
                   from ztpmthd
                  where zfreqno eq it_tab-zfreqno "ztreqhd-zfreqno.
                    and zfpwdt  in s_pwdt.

* 비용 Item Table Select.
            select *
              from ztbseg
             where zfimdno eq it_tab-zfreqno
*               and zftcedt in s_tcedt           "NCW 막음
               and zfcd    eq '04'.

*              if sy-subrc eq 0 and not ztbseg-zftcedt is initial. "NCW
*                move lfa1-name1            to it_tab-name1.
*                move ztreqst-zfopndt       to it_tab-zfopndt.
*                move ztreqst-zfamdno       to it_tab-zfamdno.
*                move ztpmthd-zfpnam        to it_tab-zfpnam.
*                move ztpmthd-zfpnamc       to it_tab-zfpnamc.
*                move ztpmthd-zfpwdt        to it_tab-zfpwdt.
*                move ztbseg-zftcedt        to it_tab-zftcedt.
*                append it_tab.
*              endif.
            endselect.
        endif.
      endif.
    endif.
  endselect.

  describe table it_tab lines w_line.
  if w_line eq 0.
    w_err_chk = 'Y'.  message s009.    exit.
  endif.

  sort it_tab by zfreqno ascending.
  loop at it_tab.
*    if not it_tab-zftcedt is initial.        "NCW
*      if not it_tab-zfreqed is initial.
*        it_tab-period1 = it_tab-zfreqed - it_tab-zfpwdt.
*      endif.
*      if not it_tab-zfpwdt is initial.
**        it_tab-period2 = it_tab-zftcedt - it_tab-zfpwdt."NCW 막음
*      endif.
*      modify it_tab index sy-tabix.
*    endif.
  endloop.
endform.                    " P2000_READ_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_DATA
*&---------------------------------------------------------------------*
form p3000_write_data.

  set pf-status 'ZIMR24C'.               " GUI STATUS SETTING
  set  titlebar 'ZIMR24C'.               " GUI TITLE SETTING..

  w_page = 1.     w_line = 0.     w_count = 0.
  if p_alv eq 'X'.
    perform p3000_alv_write.
  else.
    loop at it_tab.
      perform p3000_line_write.

      at last.
        perform p3000_last_write.
      endat.
    endloop.
  endif.
endform.                    " P3000_WRITE_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
form p3000_title_write.

  skip 2.

  format color col_background intensified off.
  write : /45  '[ L/C Term Charge 만기일 List ]'
               color col_heading intensified off.
  write : / 'Date : ', sy-datum, 97 'Page : ', w_page.
  write : / sy-uline.
  format color col_heading intensified on.
  write : / sy-vline no-gap,
        (13)'개설일'        no-gap,   sy-vline no-gap,
        (10)'수입의뢰'      no-gap,   sy-vline no-gap,
        (05)'Amend'         no-gap,   sy-vline no-gap,
        (02)'Ty'            no-gap,   sy-vline no-gap,
        (40)'Material'      no-gap,   sy-vline no-gap,
        (25)'Amount'        no-gap,   sy-vline no-gap,
        (13)'Usanc 만기(B)' no-gap,   sy-vline no-gap,
        (09)'일자(A-B)'     no-gap,   sy-vline no-gap.
  write : / sy-vline no-gap,
        (13)'L/C 만기일(A)' no-gap,   sy-vline no-gap,
        (10)'P/O No.'       no-gap,   sy-vline no-gap,
        (25)'L/C No.'       no-gap,   sy-vline no-gap,
        (23)'개설은행'      no-gap,   sy-vline no-gap,
        (25)'Usance 금액'   no-gap,   sy-vline no-gap,
        (13)'Term 만기 (C)' no-gap,   sy-vline no-gap,
        (09)'일자(C-B)'     no-gap,   sy-vline no-gap.
  write : / sy-uline.
  format color col_background intensified off.

endform.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
form p2000_config_check           using   w_err_chk.

  w_err_chk = 'N'.
* Import Config Select.
  select single * from ztimimg00.

* if Not Found.
  if sy-subrc ne 0.
    w_err_chk = 'Y'.   message s961.   leave to screen 0.
  endif.

  set parameter id 'BUK'  field  p_bukrs.

endform.                               " P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
form p3000_line_write.
  format reset.
  format color col_normal intensified on.

  write: / sy-vline no-gap,
       (13)it_tab-zfopndt no-gap,          sy-vline no-gap,
       (10)it_tab-zfreqno no-gap,          sy-vline no-gap,
       (05)it_tab-zfamdno no-gap,          sy-vline no-gap,
*       (25)it_tab-zfopnno no-gap,          sy-vline no-gap,
*       (23)it_tab-name1   no-gap,          sy-vline no-gap,
       (02)it_tab-zfreqty no-gap,          sy-vline no-gap,
       (40)it_tab-maktx   no-gap,          sy-vline no-gap,
       (19)it_tab-zflastam currency it_tab-waers,
       (05)it_tab-waers   no-gap,          sy-vline no-gap,
       (13)it_tab-zfpwdt  no-gap,          sy-vline no-gap.
  if it_tab-period1 lt 0.
    format color col_negative intensified on.
    write: (09)it_tab-period1 no-gap,      sy-vline no-gap.
    format color col_normal intensified on.
  else.
    write: (09)it_tab-period1 no-gap,      sy-vline no-gap.
  endif.

  write: / sy-vline no-gap,
       (13)it_tab-zfreqed no-gap,          sy-vline no-gap,
       (10)it_tab-ebeln   no-gap,          sy-vline no-gap,
       (25)it_tab-zfopnno no-gap,          sy-vline no-gap,
       (23)it_tab-name1   no-gap,          sy-vline no-gap,
       (19)it_tab-zfpnam currency it_tab-zfpnamc,
       (05)it_tab-waers   no-gap,          sy-vline no-gap.
*       (13)it_tab-zftcedt no-gap,          sy-vline no-gap.
  if it_tab-period2 lt 0.
    format color col_negative intensified on.
      write: (09)it_tab-period2 no-gap,    sy-vline no-gap.
    format color col_normal intensified on.
  else.
    write: (09)it_tab-period2 no-gap,    sy-vline no-gap.
  endif.

  hide: it_tab.
  format color col_normal intensified on.

  w_count = w_count + 1.

  write : / sy-uline.
endform.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
form p3000_last_write.
  if w_count gt 0.
    format reset.
    write : / '총', w_count, '건'.
  endif.

endform.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
form reset_list.

  move 0 to sy-lsind.

  w_page = 1.
  w_line = 1.
  w_count = 0.
  perform   p3000_title_write.                  " 해더 출력...
* 레포트 Write
  perform   p3000_write_data.

endform.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P3000_ALV_WRITE
*&---------------------------------------------------------------------*
form p3000_alv_write.

  perform p3000_append_fieldcat.      " ALV Report TiTle.

  g_repid = sy-repid.
  data: slis_formname(30)  type c.
  call function 'REUSE_ALV_GRID_DISPLAY'
       exporting
            i_callback_program = g_repid
            is_layout          = g_layout
            it_fieldcat        = gt_fieldcat[]
            i_grid_title       = 'Term Charge 만기일 현황'
            i_save             = g_save
            is_variant         = g_variant
       tables
            t_outtab           = it_tab
       exceptions
            program_error      = 1
            others             = 2.
  if sy-subrc <> 0.
    message e977 with 'Grid Dispaly 도중 오류가 발생하였습니다.'.
  endif.

endform.                    " P3000_ALV_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_APPEND_FIELDCAT
*&---------------------------------------------------------------------*
form p3000_append_fieldcat.

  clear: gt_fieldcat, pos.
  perform p3000_fmake using 'ZFOPNDT' '개설일'             10 'C200'.
  perform p3000_fmake using 'ZFREQNO' '수입의뢰 관리번호'  10 'C200'.
*  perform p3000_fmake using 'ZFAMDNO' 'Amend'              5  'C200'.
  perform p3000_fmake using 'ZFREQTY' 'Type'               2  'C200'.
  perform p3000_fmake using 'EBELN'   'P/O No.'            10 'C200'.
  perform p3000_fmake using 'ZFOPNNO' 'L/C No.'            18 'C200'.
*  perform p3000_fmake using 'MATNR'   'Material No.'       18 'C200'.
  perform p3000_fmake using 'MAKTX'   'Description'        20 'C200'.
*  perform p3000_fmake using 'ZFOPBN'  '개설은행'           10 'C200'.
  perform p3000_fmake using 'NAME1'   '개설은행명'         20 'C200'.
  perform p3000_fmake using 'ZFLASTAM' 'Amount'            19 'C200'.
  perform p3000_fmake using 'WAERS'   '통화'               5  'C200'.
  perform p3000_fmake using 'ZFREQED' 'L/C 만기일(A)'      13 'C200'.
  perform p3000_fmake using 'ZFPNAM'  'Usance 금액'        19 'C200'.
  perform p3000_fmake using 'ZFPNAMC' '통화'               5  'C200'.
  perform p3000_fmake using 'ZFPWDT'  'Usance 만기일(B)'   16 'C200'.
  perform p3000_fmake using 'ZFTCEDT' 'Term   만기일(C)'   16 'C200'.
  perform p3000_fmake using 'PERIOD1' '일자(A-B)'          9  'C500'.
  perform p3000_fmake using 'PERIOD2' '일자(C-B)'          9  'C300'.

endform.                    " P3000_APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  P2000_ALV_PF_STATUS
*&---------------------------------------------------------------------*
form p2000_alv_pf_status using  extab type slis_t_extab.
  set pf-status 'ZIMR24C'.
endform.                    " P2000_ALV_PF_STATUS

*&---------------------------------------------------------------------*
*&      Form  P2000_ALV_COMMAND
*&---------------------------------------------------------------------*
form p2000_alv_command using r_ucomm      like sy-ucomm
                              rs_selfield type slis_selfield.
  case r_ucomm.
    when 'LCGL'. " L/C 원장 상세 조회.
      set parameter id 'ZPREQNO' field it_zvreq-zfreqno.
      set parameter id 'ZPAMDNO' field it_zvreq-zfamdno.
      call transaction 'ZIMGL1' and skip first screen.
    when 'SHLC'.
      set parameter id 'ZPOPNNO' field ''.
      set parameter id 'BES'     field ''.
      set parameter id 'ZPREQNO' field it_zvreq-zfreqno.
      call transaction 'ZIM03' and skip first screen.
    when 'SHPO'.
      set parameter id 'BES'     field it_zvreq-ebeln.
      call transaction 'ME23N' and skip first screen.
    when others.
  endcase.

endform.                    " P2000_ALV_COMMAND
*&---------------------------------------------------------------------*
*&      Form  p3000_field_make
*&---------------------------------------------------------------------*
form p3000_fmake using    field text len emp.

  clear ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = field.
  ls_fieldcat-seltext_m      = text.
  ls_fieldcat-outputlen      = len.
  ls_fieldcat-emphasize      = emp.
  append ls_fieldcat to gt_fieldcat.

endform.                    " p3000_field_make
