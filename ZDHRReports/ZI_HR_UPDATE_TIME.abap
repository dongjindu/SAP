*----------------------------------------------------------------------
* Program ID        : ZI_HR_UPDATE_TIME
* Title             : HR Timesheet Update
* Created on        : 11/01/2007
* Created by        : Rakesh Gandhi
* Specifications By : Hu,J.T.
* Description       : Program to upload HR time into system. This
*                     uses BDC on standard transaction CAT2 to update
*                     timesheet.
*----------------------------------------------------------------------
*----------------------------------------------------------------------
* CHANGE LOG
*-----------------------------------------------------------------------
* DATE      |  NAME       |      DESC
*-----------------------------------------------------------------------
*01/24/08      IG.MOON      Merge weekly,daily into one template

report zi_hr_update_time no standard page heading
                         line-size  100
                         line-count 65
                         message-id zmhr.

tables: t100      .
data  g_error.
data  num(12) value ' 0123456789'.

*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
define __cls.                          " clear & refresh
  clear &1.refresh &1.
end-of-definition.
constants:  false value ' ',
            true  value 'X'.
define u_break.
  if p_debug eq true.
    break-point.
  endif.
end-of-definition.

define __ap_lt_type.
  if not &3 is initial.
    lt_type-awart = &1(4).
    lt_type-day = &2.
    lt_type-value = &3.
    append lt_type.
    clear lt_type.
  endif.
end-of-definition.

define __move_dayx.

  if not &1 is initial.
    &2 = &1.
  endif.

end-of-definition.
*--------------------------------------------------------------------*
* DATA DECLARATION
*--------------------------------------------------------------------*
*data: begin of it_file occurs 0,
*        name(20)    type c     ,
*        pernr(8)    type c     ,
*        awart(10)   type c     ,
*        day1(8)     type c     ,
*        day2(8)     type c     ,
*        day3(8)     type c     ,
*        day4(8)     type c     ,
*        day5(8)     type c     ,
*        day6(8)     type c     ,
*        day7(8)     type c     ,
*        total(8)    type c     ,
*        remarks(40) type c     ,
*      end of it_file           ,

data: begin of it_file occurs 0,
      name(20),
      pernr(8),
      shftcd(7),
      ot1(8),
      day11(8),
      day12(8),
      day13(8),
      day14(8),
      ot2(8),
      day21(8),
      day22(8),
      day23(8),
      day24(8),
      ot3(8),
      day31(8),
      day32(8),
      day33(8),
      day34(8),
      ot4(8),
      day41(8),
      day42(8),
      day43(8),
      day44(8),
      ot5(8),
      day51(8),
      day52(8),
      day53(8),
      day54(8),
      ot6(8),
      day61(8),
      day62(8),
      day63(8),
      day64(8),
      ot7(8),
      day71(8),
      day72(8),
      day73(8),
      day74(8),
      total(8),
      remarks(40),
      end of it_file.

data: begin of it_file_key,
      name(20),
      pernr(8),
      end of it_file_key.

data: begin of it_cat2 occurs 0,
          pernr like catsd-pernr ,
          key_date type sy-datum ,
          awart like catsd-awart ,  " Attendance or Absence Type
          day1(7) type c        ,
          day2(7) type c        ,
          day3(7) type c        ,
          day4(7) type c        ,
          day5(7) type c        ,
          day6(7) type c        ,
          day7(7) type c        ,
          status(1) type c       ,
          desc(80) type c       ,
          name(20),
end of it_cat2           .


data: it_excl like table of it_file    with header line.

data : begin of it_bdc occurs 0  .
        include structure bdcdata.
data : end of it_bdc             .
data : begin of it_mess occurs 0    .
        include structure bdcmsgcoll.
data : end of it_mess               .

data : w_update like ctu_params-updmode value 'L'.

data: w_cat2 like it_cat2                      ,
      w_flag(1)                                ,
      w_day_in_week type p                     ,
      w_mstring(480) type c                    ,
      w_dw(1)                                  .

data : w_mode like ctu_params-dismode value 'E', "'E'. "A-display 'A'
       w_fr_date like sy-datum,
       w_filety like rlgrap-filetype value 'DAT',
       w_admin like catsfields-sachz,
       l_otcode(4),
       l_sachz like pa0001-sachz,
       l_date_c(8).

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
selection-screen begin of block b1 with frame.
parameters: p_file  like rlgrap-filename obligatory.
selection-screen end   of block b1.
parameters p_debug no-display. " as checkbox.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
initialization.

  perform set_title.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
at selection-screen on value-request for p_file.
  perform f4_p_upfile using p_file.

*----------------------------------------------------------------------*
* START OF SELECTION.
*----------------------------------------------------------------------*
start-of-selection.
  message e000 with 'Program no more available'.

  if w_dw eq 'W'.
    perform upload_file.
  else.
    perform upload_file_daily.
  endif.

  if it_excl[] is initial.
    exit.
  endif.

  if g_error ne space.
    message s001 with 'The template is invalid.'.
    stop.
  endif.

* perform read_excel_old.
  perform read_excel.

  if w_flag = 'X'.
    clear:  w_flag.
    exit.
  endif.

  perform update_time.
  perform display_report.

*&---------------------------------------------------------------------*
*&      Form  f4_p_upfile
*&---------------------------------------------------------------------*
*       Subroutine to provide F4 help for file
*----------------------------------------------------------------------*
*      -->P_FILE Local file name
*----------------------------------------------------------------------*
form f4_p_upfile using    p_file.
  call function 'WS_FILENAME_GET'
       exporting
            def_path         = p_file  "* File Name
            mask             = ',*.*,*.*.'
            mode             = 'O'
       importing
            filename         = p_file
       exceptions
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            others           = 5.

endform.                    " f4_p_upfile
*&---------------------------------------------------------------------*
*&      Form  upload_file
*&---------------------------------------------------------------------*
*       Subroutine to upload file contents into internal table
*----------------------------------------------------------------------*
form upload_file.
  clear g_error.

  data: it_itab like standard table of alsmex_tabline with header line.
  field-symbols : <fs>.
  data : v_index type i.
  data : $sht_value(10).

  call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       exporting
            filename                = p_file
            i_begin_col             = 1
            i_begin_row             = 2
            i_end_col               = 38
            i_end_row               = 86
       tables
            intern                  = it_itab
       exceptions
            inconsistent_parameters = 1
            upload_ole              = 2
            others                  = 3.

  clear l_date_c.
  read table it_itab index 2.
  if sy-subrc eq 0.
    concatenate '20' it_itab-value+6(2) it_itab-value+0(2)
                it_itab-value+3(2) into l_date_c.
  else.
    g_error = true.
    exit.
  endif.

*  clear l_otcode.

*  read table it_itab index 7.
*  if sy-subrc eq 0.
*    $sht_value = it_itab-value.
*    condense $sht_value.
*    case $sht_value.
*      when '1st'.
*        l_otcode = '1001'.
*      when '2nd'.
*        l_otcode = '1003'.
*      when '3rd'.
*        l_otcode = '1004'.
*      when others.
*        g_error = true.
*        exit.
*    endcase.
*
*  else.
*    g_error = true.
*    exit.
*  endif.

  read table it_itab index 7.
  if sy-subrc eq 0.
    l_sachz = it_itab-value(3).
  else.
    g_error = true.
    exit.
  endif.

  delete it_itab where row < 6.

  if it_itab[] is initial.
    message s003(zz) with 'No Data was uploaded'.
    exit.
  else.
    sort it_itab by row col.
    loop at it_itab.
      move : it_itab-col to v_index.
      assign component v_index of structure it_excl to <fs>.
      move : it_itab-value to <fs>.
      at end of row.
        move-corresponding it_excl to it_file_key.
        append it_excl.
        move-corresponding it_file_key to it_excl.
        clear it_excl.
      endat.
    endloop.
  endif.
  data $line type i.

  describe table it_excl lines $line.

  read table it_excl with key name = 'Totals'.

  if sy-subrc eq 0.
    delete it_excl from sy-tabix to $line.
  endif.

endform.                    " upload_file
*&---------------------------------------------------------------------*
*&      Form  read_excel
*&---------------------------------------------------------------------*
*       Subroutine to read data from Excel file
*----------------------------------------------------------------------*
form read_excel_old.
*  data: l_key_date type sy-datum,
*        l_date_c(8),
*        l_to_dat like sy-datum,
*        w_pernr like it_excl-pernr,
*        w_total like it_cat2-day1,
*        l_to_weekday like dtresr-weekday,
*        l_ans(1) type c.
*
*  read table it_excl index 2.
*  concatenate '20' it_excl-pernr+6(2) it_excl-pernr+0(2)
*                   it_excl-pernr+3(2) into l_date_c.
*
*  l_key_date = l_date_c.
*  write: l_date_c to w_fr_date.
*
*  w_dw = it_excl-day1+0(1).
*  if w_dw <> 'D' and w_dw <> 'W'.
*    w_dw = it_excl-day2+0(1).
*    if w_dw <> 'D' and w_dw <> 'W'.
*      w_flag = 'X'.
*      message i001 with 'Error in excel file'.
*      exit.
*    endif.
*  endif.
*
*  call function 'HR_GB_DAY_RELATIVE_TO_DATE'
*   exporting
*     date                  = sy-datum
*     day_in_week           = '7'
*     before_or_after       = '-'
*   importing
*     new_date              = l_to_dat
**   DAY_TEXT              =
*            .
*  l_to_dat = l_to_dat - 7.
*  if w_fr_date <= l_to_dat.
*    message i001 with 'Date entry error'.
*    w_flag = 'X'.
*    exit.
*  endif.
*
*  if w_dw = 'W'.
*    l_to_dat = sy-datum - 7.
*    if w_fr_date <> l_to_dat.
*      call function 'POPUP_TO_DECIDE'
*        exporting
*          textline1               = 'Entry date should be 7 days ago'
*          text_option1            = 'Continue to process'
*          text_option2            = 'Back'
*          titel                   = 'Warning'
**   START_COLUMN            = 25
**   START_ROW               = 6
**   CANCEL_DISPLAY          = 'X'
*          importing
*          answer                  = l_ans
*              .
*      if l_ans <> '1'.
*        w_flag = 'X'.
*        exit.
*      endif.
*    endif.
*  else.
*    call function 'DATE_TO_DAY'
*         exporting
*              date    = sy-datum
*         importing
*              weekday = l_to_weekday.
*
*    if l_to_weekday = 'Monday'.
*      l_to_dat = sy-datum - 3.
*      if w_fr_date <> l_to_dat.
*        call function 'POPUP_TO_DECIDE'
*           exporting
**   DEFAULTOPTION           = '1'
*           textline1               = 'Enter date should be Friday data'
**   TEXTLINE2               = ' '
**   TEXTLINE3               = ' '
*           text_option1            = 'Continue to process'
*           text_option2            = 'Back'
**   ICON_TEXT_OPTION1       = ' '
**   ICON_TEXT_OPTION2       = ' '
*           titel                   = 'Warning'
**   START_COLUMN            = 25
**   START_ROW               = 6
**   CANCEL_DISPLAY          = 'X'
*           importing
*           answer                  = l_ans.
*        if l_ans <> '1'.
*          w_flag = 'X'.
*          exit.
*        endif.
*      endif.
*    else.
*      l_to_dat = sy-datum - 1.
*      if w_fr_date <> l_to_dat.
*        call function 'POPUP_TO_DECIDE'
*            exporting
**   DEFAULTOPTION           = '1'
*            textline1               = 'Entry date should be yesterday'
**   TEXTLINE2               = ' '
**   TEXTLINE3               = ' '
*            text_option1            = 'Continue to process'
*            text_option2            = 'Back'
**   ICON_TEXT_OPTION1       = ' '
**   ICON_TEXT_OPTION2       = ' '
*            titel                   = 'Warning'
**   START_COLUMN            = 25
**   START_ROW               = 6
**   CANCEL_DISPLAY          = 'X'
*            importing
*            answer                  = l_ans.
*        if l_ans <> '1'.
*          w_flag = 'X'.
*          exit.
*        endif.
*      endif.
*    endif.
*  endif.
*  call function 'DAY_IN_WEEK'
*       exporting
*            datum = l_key_date
*       importing
*            wotnr = w_day_in_week.
*
*  read table it_excl index 8.
*  w_pernr = it_excl-pernr.
*  loop at it_excl from 7.
*    if it_excl-awart is initial.
*      continue.
*    endif.
*    if it_excl-pernr is initial.
*      it_cat2-pernr = w_pernr.
*    else.
*      it_cat2-pernr = it_excl-pernr.
*      w_pernr = it_excl-pernr.
*    endif.
*    it_cat2-awart = it_excl-awart+0(4).
*    it_cat2-key_date = l_key_date.
*    if w_dw = 'D'.
*      if w_day_in_week = '1'.
*        it_cat2-day1 = it_excl-day1.
*      elseif w_day_in_week = '2'.
*        it_cat2-day2 = it_excl-day1.
*      elseif w_day_in_week = '3'.
*        it_cat2-day3 = it_excl-day1.
*      elseif w_day_in_week = '4'.
*        it_cat2-day4 = it_excl-day1.
*      elseif w_day_in_week = '5'.
*        it_cat2-day5 = it_excl-day1.
*      elseif w_day_in_week = '6'.
*        it_cat2-day6 = it_excl-day1.
*      elseif w_day_in_week = '7'.
*        it_cat2-day7 = it_excl-day1.
*      endif.
*    elseif w_dw = 'W'.
*      it_cat2-day1 = it_excl-day1.
*      it_cat2-day2 = it_excl-day2.
*      it_cat2-day3 = it_excl-day3.
*      it_cat2-day4 = it_excl-day4.
*      it_cat2-day5 = it_excl-day5.
*      it_cat2-day6 = it_excl-day6.
*      it_cat2-day7 = it_excl-day7.
*    endif.
*    w_total = it_cat2-day1 + it_cat2-day2 + it_cat2-day3 +
*              it_cat2-day4 + it_cat2-day5 + it_cat2-day6 +
*              it_cat2-day7.
*    w_total = w_total * 100.
*    if w_total >= 0.
*      append it_cat2.
*    endif.
*    clear: it_cat2, it_excl, w_total.
*  endloop.
*
endform.                    " read_excel
*&---------------------------------------------------------------------*
*&      Form  update_time
*&---------------------------------------------------------------------*
*       BDC to update time entry
*----------------------------------------------------------------------*
form update_time.
  data: l_extdate(12) type c,
        l_week like scal-week,
        l_week1(7) type c    ,
        l_cnt(2) type n,
        l_string(20) type c.

  sort it_cat2 by pernr key_date awart.

  read table it_cat2 index 1.
  if sy-subrc = 0.
    call function 'GET_WEEK_INFO_BASED_ON_DATE'
   exporting
     date          = it_cat2-key_date
   importing
     week          = l_week
*   MONDAY        =
*   SUNDAY        =
              .
  endif.
  concatenate l_week+4(2) '.' l_week(4) into l_week1.
  loop at it_cat2.
    clear w_cat2.
    w_cat2 = it_cat2.
    at new pernr.
      clear:   it_bdc,
               it_mess,
               l_extdate.
      refresh: it_bdc,
               it_mess.

      perform user_specific_date using w_cat2-key_date
                                 changing l_extdate.

      perform bdc_dynpro      using 'SAPLCATS' '1000'.

*      perform bdc_field       using 'BDC_CURSOR'
*                                    'TCATST-VARIANT'.
*      perform bdc_field       using 'BDC_OKCODE'
*                                    '/00'.

      perform bdc_field       using 'TCATST-VARIANT'
                                    'TIMEADMN'.

*      perform bdc_dynpro      using 'SAPLCATS' '1000'.
*      perform bdc_field       using 'BDC_CURSOR'
*                                    'CATSFIELDS-INPUTDATE'.
*      perform bdc_field       using 'BDC_OKCODE'
*                                    '/00'.
*
*      perform bdc_field       using 'TCATST-VARIANT'
*                                    'TIMEADMN'.

      perform bdc_field       using 'CATSFIELDS-INPUTDATE'
                                     l_extdate.

      perform bdc_field       using 'BDC_OKCODE'
                                    '/00'.

      perform bdc_field       using 'CATSFIELDS-PERNR'
                                    w_cat2-pernr.
      perform bdc_dynpro      using 'SAPLCATS' '1000'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'CATSFIELDS-PERNR'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=TIME'.
      perform bdc_field       using 'TCATST-VARIANT'
                                    'TIMEADMN'.
      perform bdc_field       using 'CATSFIELDS-INPUTDATE'
                                     l_extdate.
      perform bdc_field       using 'CATSFIELDS-PERNR'
                                    w_cat2-pernr.

      perform bdc_dynpro      using 'SAPLCATS' '2002'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'CATSFIELDS-CATSWEEKEX'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '/00'.
      perform bdc_field       using 'CATSFIELDS-CATSWEEKEX'
                                    l_week1.
      perform bdc_dynpro      using 'SAPLCATS' '2002'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=SAVE'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '/00'.
      l_cnt = '01'.
    endat.
    clear l_string.
    concatenate 'CATSD-AWART(' l_cnt ')' into l_string.
    perform bdc_field       using l_string
                                  it_cat2-awart.
    if w_dw = 'D'.
      if w_day_in_week = '1'.
        if it_cat2-day1 <> space.
          clear l_string.
          concatenate 'CATSD-DAY1(' l_cnt ')' into l_string.
          perform bdc_field       using l_string
                                        it_cat2-day1.
        endif.
      elseif w_day_in_week = '2'.
        if it_cat2-day2 <> space.
          clear l_string.
          concatenate 'CATSD-DAY2(' l_cnt ')' into l_string.
          perform bdc_field       using l_string
                                        it_cat2-day2.
        endif.
      elseif w_day_in_week = '3'.
        if it_cat2-day3 <> space.
          clear l_string.
          concatenate 'CATSD-DAY3(' l_cnt ')' into l_string.
          perform bdc_field       using l_string
                                        it_cat2-day3.
        endif.
      elseif w_day_in_week = '4'.
        if it_cat2-day4 <> space.
          clear l_string.
          concatenate 'CATSD-DAY4(' l_cnt ')' into l_string.
          perform bdc_field       using l_string
                                        it_cat2-day4.
        endif.
      elseif w_day_in_week = '5'.
        if it_cat2-day5 <> space.
          clear l_string.
          concatenate 'CATSD-DAY5(' l_cnt ')' into l_string.
          perform bdc_field       using l_string
                                        it_cat2-day5.
        endif.
      elseif w_day_in_week = '6'.
        clear l_string.
        concatenate 'CATSD-DAY6(' l_cnt ')' into l_string.
        perform bdc_field       using l_string
                                      it_cat2-day6.
      elseif w_day_in_week = '7'.
        clear l_string.
        concatenate 'CATSD-DAY7(' l_cnt ')' into l_string.
        perform bdc_field       using l_string
                                      it_cat2-day7.
      endif.
    elseif w_dw = 'W'.
      if it_cat2-day1 <> space.
        clear l_string.
        concatenate 'CATSD-DAY1(' l_cnt ')' into l_string.
        perform bdc_field       using l_string
                                    it_cat2-day1.
      endif.
      if it_cat2-day2 <> space.
        clear l_string.
        concatenate 'CATSD-DAY2(' l_cnt ')' into l_string.
        perform bdc_field       using l_string
                                    it_cat2-day2.
      endif.
      if it_cat2-day3 <> space.
        clear l_string.
        concatenate 'CATSD-DAY3(' l_cnt ')' into l_string.
        perform bdc_field       using l_string
                                    it_cat2-day3.
      endif.
      if it_cat2-day4 <> space.
        clear l_string.
        concatenate 'CATSD-DAY4(' l_cnt ')' into l_string.
        perform bdc_field       using l_string
                                    it_cat2-day4.
      endif.
      if it_cat2-day5 <> space.
        clear l_string.
        concatenate 'CATSD-DAY5(' l_cnt ')' into l_string.
        perform bdc_field       using l_string
                                    it_cat2-day5.
      endif.
      if it_cat2-day6 <> space.
        clear l_string.
        concatenate 'CATSD-DAY6(' l_cnt ')' into l_string.
        perform bdc_field       using l_string
                                    it_cat2-day6.
      endif.
      if it_cat2-day7 <> space.
        clear l_string.
        concatenate 'CATSD-DAY7(' l_cnt ')' into l_string.
        perform bdc_field       using l_string
                                    it_cat2-day7.
      endif.
    endif.
    l_cnt = l_cnt + 1.

    at end of pernr.
      perform bdc_dynpro      using 'SAPLCATS' '2002'.

      perform bdc_field       using 'BDC_OKCODE'
                                    '=SAVE'.

      refresh it_mess.

      if p_debug eq 'X'.
        w_mode = 'A'.
      endif.

      call transaction 'CAT2' using it_bdc
                       mode   w_mode
                       update w_update
                       messages into it_mess.
      if sy-subrc = 0.
        w_cat2-status = 'S'.
        modify it_cat2 from w_cat2 transporting status
                                   where pernr = w_cat2-pernr.
      else.
        loop at it_mess. " WHERE msgtyp = 'E' OR msgtyp = 'W'.
          select single * from t100 where sprsl = it_mess-msgspra
                                    and   arbgb = it_mess-msgid
                                    and   msgnr = it_mess-msgnr.
          if sy-subrc = 0.
            clear w_mstring.
            w_mstring = t100-text.
            if w_mstring cs '&1'.
              replace '&1' with it_mess-msgv1 into w_mstring.
              replace '&2' with it_mess-msgv2 into w_mstring.
              replace '&3' with it_mess-msgv3 into w_mstring.
              replace '&4' with it_mess-msgv4 into w_mstring.
            else.
              replace '&' with it_mess-msgv1 into w_mstring.
              replace '&' with it_mess-msgv2 into w_mstring.
              replace '&' with it_mess-msgv3 into w_mstring.
              replace '&' with it_mess-msgv4 into w_mstring.
            endif.
            condense w_mstring.
            w_cat2-status = 'F'.
            w_cat2-desc = w_mstring.
            modify it_cat2 from w_cat2 transporting status desc
                                       where pernr = w_cat2-pernr.
          endif.
        endloop.
      endif.
    endat.
  endloop.
endform.                    " update_time
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
form bdc_dynpro using program dynpro.
  clear it_bdc.
  it_bdc-program  = program.
  it_bdc-dynpro   = dynpro.
  it_bdc-dynbegin = 'X'.
  append it_bdc.
endform.
*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
form bdc_field using fnam fval.
  clear it_bdc.
  it_bdc-fnam = fnam.
  it_bdc-fval = fval.
  append it_bdc.
endform.
*&---------------------------------------------------------------------*
*&      Form  user_specific_date
*&---------------------------------------------------------------------*
*       Subroutine to get user specific date format
*----------------------------------------------------------------------*
*      -->P_DATE    Date in any date format
*      <--P_EXTDATE Date in user specific date format
*----------------------------------------------------------------------*
form user_specific_date using    p_pdate
                        changing p_extdate.
  data: w_datfm(1) type c.
  call function 'ITS_GET_USER_DEFAULTS'
       exporting
            bname = sy-uname
       importing
            datfm = w_datfm.

  case w_datfm.
    when 1.
      concatenate p_pdate+6(2) p_pdate+4(2) p_pdate+0(4) into p_extdate
      separated by '.'.

    when 2.
      concatenate p_pdate+4(2) p_pdate+6(2) p_pdate+0(4) into p_extdate
      separated by '/'.

    when 3.
      concatenate p_pdate+4(2) p_pdate+6(2) p_pdate+0(4) into p_extdate
      separated by '-'.

    when 4.
      concatenate p_pdate+0(4) p_pdate+4(2) p_pdate+6(2) into p_extdate
      separated by '.'.

    when 5.
      concatenate p_pdate+0(4) p_pdate+4(2) p_pdate+6(2) into p_extdate
      separated by '/'.

    when 6.
      concatenate p_pdate+0(4) p_pdate+4(2) p_pdate+6(2) into p_extdate
      separated by '-'.

  endcase.
endform.                    " user_specific_date
*&---------------------------------------------------------------------*
*&      Form  display_report
*&---------------------------------------------------------------------*
*       Subroutine to display status report
*----------------------------------------------------------------------*
form display_report.

  perform set_title.
  data: l_success type i,
        l_fail    type i,
        l_total   type i.
  clear: l_success,
         l_fail   .
  loop at it_cat2.
    if it_cat2-status = 'S'.
      l_success = l_success + 1.
    elseif it_cat2-status = 'F'.
      l_fail = l_fail + 1.
    endif.
  endloop.
  l_total = l_success + l_fail.
  new-page line-size 198.
  write : /(23) 'Total Records'.
  write:  (8) l_total.
  skip.

*-Successful records
  if l_success > 0.
    write : /(40) 'Number of Records updated successfully'.
    write:  (8) l_success.
    skip.

    write : /(99) sy-uline.
    write : /   sy-vline no-gap,   (8) 'Per No'            no-gap,
                sy-vline no-gap,  (10) ' KEY DATE'         no-gap,
                sy-vline no-gap,  (21) ' Att. or Absence Type'   no-gap,
                sy-vline no-gap,   (7) ' DAY1'             no-gap,
                sy-vline no-gap,   (7) ' DAY2'             no-gap,
                sy-vline no-gap,   (7) ' DAY3'             no-gap,
                sy-vline no-gap,   (7) ' DAY4'             no-gap,
                sy-vline no-gap,   (7) ' DAY5'             no-gap,
                sy-vline no-gap,   (7) ' DAY6'             no-gap,
                sy-vline no-gap,   (7) ' DAY7'             no-gap,
                sy-vline no-gap.
    write : /(99) sy-uline.
    loop at it_cat2 where status <> 'F'.
      write : /   sy-vline no-gap,   (8) it_cat2-pernr    no-gap,
                  sy-vline no-gap,  (10) it_cat2-key_date no-gap,
                  sy-vline no-gap,  (21) it_cat2-awart    no-gap,
                  sy-vline no-gap,   (7) it_cat2-day1     no-gap,
                  sy-vline no-gap,   (7) it_cat2-day2     no-gap,
                  sy-vline no-gap,   (7) it_cat2-day3     no-gap,
                  sy-vline no-gap,   (7) it_cat2-day4     no-gap,
                  sy-vline no-gap,   (7) it_cat2-day5     no-gap,
                  sy-vline no-gap,   (7) it_cat2-day6     no-gap,
                  sy-vline no-gap,   (7) it_cat2-day7     no-gap,
                  sy-vline no-gap.
      write : /(99) sy-uline.
    endloop.
  endif.

*-Erroneous records
  if l_fail > 0.
    write : /(26) 'Number of Records failed'.
    write:  (8) l_fail.
    skip.

    write : /(180) sy-uline.
    write : /   sy-vline no-gap,   (8) 'Per No'            no-gap,
                sy-vline no-gap,  (10) ' KEY DATE'         no-gap,
                sy-vline no-gap,  (21) ' Att. or Absence Type'   no-gap,
                sy-vline no-gap,   (7) ' DAY1'             no-gap,
                sy-vline no-gap,   (7) ' DAY2'             no-gap,
                sy-vline no-gap,   (7) ' DAY3'             no-gap,
                sy-vline no-gap,   (7) ' DAY4'             no-gap,
                sy-vline no-gap,   (7) ' DAY5'             no-gap,
                sy-vline no-gap,   (7) ' DAY6'             no-gap,
                sy-vline no-gap,   (7) ' DAY7'             no-gap,
                sy-vline no-gap,  (80) 'ERROR DESCRIPTION' no-gap,
                sy-vline no-gap.
    write : /(180) sy-uline.
    loop at it_cat2 where status = 'F'.
      write : /   sy-vline no-gap,   (8) it_cat2-pernr    no-gap,
                  sy-vline no-gap,  (10) it_cat2-key_date no-gap,
                  sy-vline no-gap,  (21) it_cat2-awart    no-gap,
                  sy-vline no-gap,   (7) it_cat2-day1     no-gap,
                  sy-vline no-gap,   (7) it_cat2-day2     no-gap,
                  sy-vline no-gap,   (7) it_cat2-day3     no-gap,
                  sy-vline no-gap,   (7) it_cat2-day4     no-gap,
                  sy-vline no-gap,   (7) it_cat2-day5     no-gap,
                  sy-vline no-gap,   (7) it_cat2-day6     no-gap,
                  sy-vline no-gap,   (7) it_cat2-day7     no-gap,
                  sy-vline no-gap,  (80) it_cat2-desc     no-gap,
                  sy-vline no-gap.
      write : /(180) sy-uline.
    endloop.
  endif.
endform.                    " display_report
*&---------------------------------------------------------------------*
*&      Form  read_excel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_excel.
  data: l_endda like pa0001-endda,
        l_to_dat like sy-datum,
        w_pernr like it_excl-pernr,
        w_name like it_excl-name,
        w_total like it_cat2-day1,
        l_sachz like pa0001-sachz,
        l_agr_name like agr_users-agr_name,
        l_dw(1),
        l_to_weekday like dtresr-weekday,
        l_ans(1) type c,
        l_no(1) type n,
        l_text(30),
        l_first_line(1),
        l_shift(1).

  data: l_awart11 like it_excl-day11,
        l_awart12 like it_excl-day11,
        l_awart13 like it_excl-day11,
        l_awart14 like it_excl-day11,
        l_awart1x like it_excl-day11,

        l_awart21 like it_excl-day11,
        l_awart22 like it_excl-day11,
        l_awart23 like it_excl-day11,
        l_awart24 like it_excl-day11,
        l_awart2x like it_excl-day11,

        l_awart31 like it_excl-day11,
        l_awart32 like it_excl-day11,
        l_awart33 like it_excl-day11,
        l_awart34 like it_excl-day11,
        l_awart3x like it_excl-day11,

        l_awart41 like it_excl-day11,
        l_awart42 like it_excl-day11,
        l_awart43 like it_excl-day11,
        l_awart44 like it_excl-day11,
        l_awart4x like it_excl-day11,

        l_awart51 like it_excl-day11,
        l_awart52 like it_excl-day11,
        l_awart53 like it_excl-day11,
        l_awart54 like it_excl-day11,
        l_awart5x like it_excl-day11,

        l_awart61 like it_excl-day11,
        l_awart62 like it_excl-day11,
        l_awart63 like it_excl-day11,
        l_awart64 like it_excl-day11,
        l_awart6x like it_excl-day11,

        l_awart71 like it_excl-day11,
        l_awart72 like it_excl-day11,
        l_awart73 like it_excl-day11,
        l_awart74 like it_excl-day11,
        l_awart7x like it_excl-day11.

  data: begin of lt_type occurs 0,
        awart like it_cat2-awart,
        day(1),
        value like it_excl-day11,
        end of lt_type.

  field-symbols: <fs>.
  data: lt_pa0001 like table of pa0001 with header line.


  write: l_date_c to w_fr_date.

  call function 'HR_GB_DAY_RELATIVE_TO_DATE'
       exporting
            date            = sy-datum
            day_in_week     = '7'
            before_or_after = '-'
       importing
            new_date        = l_to_dat.

  l_to_dat = l_to_dat - 7.

  if w_fr_date <= l_to_dat.
    message i001 with 'Date entry error'.
    w_flag = 'X'.
    exit.
  endif.

  l_to_dat = sy-datum - 7.
  if w_fr_date <> l_to_dat.
    call function 'POPUP_TO_DECIDE'
         exporting
              textline1    = 'Entry date should be 7 days ago'
              text_option1 = 'Continue to process'
              text_option2 = 'Back'
              titel        = 'Warning'
         importing
              answer       = l_ans.
    if l_ans <> '1'.
      w_flag = 'X'.
      exit.
    endif.
  endif.

  if sy-uname = '103569' or sy-uname = '100553'.
  else.
    concatenate 'Z:HR_TIME_CATS_' l_sachz into l_agr_name.
    select single to_dat into l_to_dat
      from agr_users
      where agr_name = l_agr_name.
    if sy-subrc <> 0.
      message e000 with 'No authorization to upload data'.
      exit.
    endif.
    if l_to_dat < sy-datum.
      message e000 with 'Adminstrator not valid '.
      exit.
    endif.
  endif.

  select * into table lt_pa0001
    from pa0001
    where sachz = l_sachz
      and endda >= sy-datum.
  sort lt_pa0001 by pernr.
  read table it_excl index 1.
  w_pernr = it_excl-pernr.
  w_name = it_excl-name.
  l_first_line = 'X'.

  data : otx like it_excl-ot1,
         dayx like it_cat2-day1,
         day1x like it_cat2-day1,
         day2x like it_cat2-day1,
         day3x like it_cat2-day1,
         day4x like it_cat2-day1,
         day5x like it_cat2-day1,
         day6x like it_cat2-day1,
         day7x like it_cat2-day1.

  data : $sftcd(4),$sft(1), n_sftcd(4) type n.

  loop at it_excl.

*    if it_excl-pernr is initial and l_first_line = 'X'.
*      continue.
*    endif.
*
*    if not it_excl-pernr is initial.
*      l_first_line = 'X'.
*    endif.

* { logic for shift code
    clear l_otcode.
    split it_excl-shftcd at '#' into $sftcd $sft.
    n_sftcd = $sftcd.
    case $sft.
      when '1'.
        l_otcode = '1001'.
      when '2'.
        l_otcode = '1003'.
      when '3'.
        l_otcode = '1004'.
      when others.
    endcase .
* }

    it_cat2-key_date = l_date_c.

*    if l_first_line = 'X'.
    if not it_excl-pernr is initial.
      it_cat2-pernr = it_excl-pernr.
      it_cat2-name = it_excl-name.
      w_pernr = it_excl-pernr.
      w_name = it_excl-name.
      clear: l_first_line.

      read table lt_pa0001 with key pernr = it_cat2-pernr binary search.

      if sy-subrc = 0
            or sy-uname = '103569' or sy-uname = '100553'.

        do 7 times varying otx  from it_excl-ot1 next it_excl-ot2
                   varying dayx from it_cat2-day1 next it_cat2-day2 .

          if otx > 0.
            it_cat2-awart = l_otcode.
            dayx = otx.
          endif.

        enddo.

        if not it_cat2-awart is initial.
          append it_cat2.
          clear: it_cat2.
          it_cat2-pernr = it_excl-pernr.
          it_cat2-name = it_excl-name.
        endif.

        do 4 times varying day1x from it_excl-day11 next it_excl-day12
                   varying day2x from it_excl-day21 next it_excl-day22
                   varying day3x from it_excl-day31 next it_excl-day32
                   varying day4x from it_excl-day41 next it_excl-day42
                   varying day5x from it_excl-day51 next it_excl-day52
                   varying day6x from it_excl-day61 next it_excl-day62
                   varying day7x from it_excl-day71 next it_excl-day72
                   varying l_awart1x from l_awart11 next l_awart12
                   varying l_awart2x from l_awart21 next l_awart22
                   varying l_awart3x from l_awart31 next l_awart32
                   varying l_awart4x from l_awart41 next l_awart42
                   varying l_awart5x from l_awart51 next l_awart52
                   varying l_awart6x from l_awart61 next l_awart62
                   varying l_awart7x from l_awart71 next l_awart72.


          __move_dayx : day1x l_awart1x,
                        day2x l_awart2x,
                        day3x l_awart3x,
                        day4x l_awart4x,
                        day5x l_awart5x,
                        day6x l_awart6x,
                        day7x l_awart7x.

        enddo.

      else.
        call function 'POPUP_TO_INFORM'
           exporting
             titel = 'Time Sheet'
          txt1  = 'The employee does not report to the administrator'
             txt2  = it_cat2-pernr
             txt3  = l_sachz
             txt4  = it_excl-ot1.
        w_flag = 'X'.
        exit.
      endif.
    else.

      it_cat2-pernr = w_pernr.
      it_cat2-name = w_name.
      l_first_line = 'X'.

      do 4 times varying day1x from it_excl-day11 next it_excl-day12
                 varying day2x from it_excl-day21 next it_excl-day22
                 varying day3x from it_excl-day31 next it_excl-day32
                 varying day4x from it_excl-day41 next it_excl-day42
                 varying day5x from it_excl-day51 next it_excl-day52
                 varying day6x from it_excl-day61 next it_excl-day62
                 varying day7x from it_excl-day71 next it_excl-day72
                 varying l_awart1x from l_awart11 next l_awart12
                 varying l_awart2x from l_awart21 next l_awart22
                 varying l_awart3x from l_awart31 next l_awart32
                 varying l_awart4x from l_awart41 next l_awart42
                 varying l_awart5x from l_awart51 next l_awart52
                 varying l_awart6x from l_awart61 next l_awart62
                 varying l_awart7x from l_awart71 next l_awart72.

        __ap_lt_type : l_awart1x '1' day1x,
                       l_awart2x '2' day2x,
                       l_awart3x '3' day3x,
                       l_awart4x '4' day4x,
                       l_awart5x '5' day5x,
                       l_awart6x '6' day6x,
                       l_awart7x '7' day7x.

      enddo.

      sort lt_type by awart day.
      loop at lt_type.

        if lt_type-awart eq '#REF'.
          continue.
        endif.

        it_cat2-awart = lt_type-awart.

        case lt_type-day.
          when '1'.
            it_cat2-day1 = lt_type-value.
          when '2'.
            it_cat2-day2 = lt_type-value.
          when '3'.
            it_cat2-day3 = lt_type-value.
          when '4'.
            it_cat2-day4 = lt_type-value.
          when '5'.
            it_cat2-day5 = lt_type-value.
          when '6'.
            it_cat2-day6 = lt_type-value.
          when '7'.
            it_cat2-day7 = lt_type-value.
        endcase.

        at end of awart.
          append it_cat2.
          clear: it_cat2-day1,it_cat2-day2,it_cat2-day3,it_cat2-day4,
                it_cat2-day5,it_cat2-day6,it_cat2-day7.
        endat.

      endloop.
    endif.

    clear: l_endda, it_cat2, it_excl, w_total, lt_type[].

  endloop.

  u_break.

endform.                    " read_excel
*&---------------------------------------------------------------------*
*&      Form  upload_file_d
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form upload_file_daily.
  clear g_error.
  data l_bs_date like sy-datum.

  data: it_itab like standard table of alsmex_tabline with header line.
  field-symbols : <fs>.
  data : v_index type i.
  data : $sht_value(10).

  call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       exporting
            filename                = p_file
            i_begin_col             = 1
            i_begin_row             = 2
            i_end_col               = 8
            i_end_row               = 89
       tables
            intern                  = it_itab
       exceptions
            inconsistent_parameters = 1
            upload_ole              = 2
            others                  = 3.


  data : $y(2),
         $m(2),
         $d(2).
  data : ny(2) type n,
         nm(2) type n,
         nd(2) type n.

  clear l_date_c.

  read table it_itab index 14.

  if sy-subrc eq 0.
    split it_itab-value at '/' into $m $d $y .

    if $y cn num or  $m cn num or $d cn num.
      message s001 with 'Invalid Date!' .
      g_error = true.
      exit.
    endif.

    ny = $y.
    nm = $m.
    nd = $d.
    concatenate '20' ny nm nd into l_date_c.

    write l_date_c to l_bs_date.

    call function 'DAY_IN_WEEK'
         exporting
              datum = l_bs_date
         importing
              wotnr = w_day_in_week.

  else.
    g_error = true.
    exit.
  endif.


  read table it_itab index 2.

  if sy-subrc eq 0.
    split it_itab-value at '/' into $m $d $y .

    if $y cn num or  $m cn num or $d cn num.
      message s001 with 'Invalid Date!' .
      g_error = true.
      exit.
    endif.

    ny = $y.
    nm = $m.
    nd = $d.
    concatenate '20' ny nm nd into l_date_c.

  else.
    g_error = true.
    exit.
  endif.

  clear l_otcode.

*  read table it_itab index 8.
*  if sy-subrc eq 0.
*    $sht_value = it_itab-value.
*    condense $sht_value.
*    case $sht_value.
*      when '1st'.
*        l_otcode = '1001'.
*      when '2nd'.
*        l_otcode = '1003'.
*      when '3rd'.
*        l_otcode = '1004'.
*      when others.
*        message s001 with 'Invalid Shift Code!' .
*        g_error = true.
*        exit.
*    endcase.
*
*  else.
*    g_error = true.
*    exit.
*  endif.

  read table it_itab index 10.
  if sy-subrc eq 0.
    l_sachz = it_itab-value(3).
  else.
    message s001 with 'Invalid Admin. Code!' .
    g_error = true.
    exit.
  endif.

  delete it_itab where row < 9.

  if it_itab[] is initial.
    message s003(zz) with 'No Data was uploaded'.
    exit.
  else.
    sort it_itab by row col.

    loop at it_itab.
      if it_itab-col > 3.
        v_index = ( ( w_day_in_week - 1 ) * 5 ) + it_itab-col.
      else.
        move : it_itab-col to v_index.
      endif.

      assign component v_index of structure it_excl to <fs>.
      move : it_itab-value to <fs>.
      at end of row.
        move-corresponding it_excl to it_file_key.
        append it_excl.
        move-corresponding it_file_key to it_excl.
        clear it_excl.
      endat.
    endloop.
  endif.
  data $line type i.

  describe table it_excl lines $line.

endform.                    " upload_file_d
*&---------------------------------------------------------------------*
*&      Form  set_title
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_title.
  if sy-tcode eq 'ZHR_TM_UPDATE_WEEKLY'.
    w_dw = 'W'.
    sy-title = 'HR Time Sheet Update - Weekly'.
  else.
    w_dw = 'D'.
    sy-title = 'HR Time Sheet Update - Daily'.
  endif.


endform.                    " set_title
