report zh_if_eyemed message-id zmco.

*-----------------------------------------------------------------------
* Name: ZH_IF_EYEMED
* Tech. Resource: Euna Lee
* Desc: Interface to EyeMed
*----------------------------------------------------------------------
*----------------------------------------------------------------------*
*  Title          : ZH_IF_EYEMED
*  Author         : ig.moon
*  Creation Data  : 10/27/2008
*  Requirements by: Euna Lee
*  Description    : Interface to EyeMed for Vision Plan.

************************************************************************
* CHANGE LOG
*-----------------------------------------------------------------------
* DATE      |  NAME          |Transport | Issue #  |      DESC
*-----------------------------------------------------------------------
* 12/06/2011  Valerian        UD1K953401  FIX the ABAP Dump
* 06/05/2012  Valerian        UD1K954974  Apply authorization to call
*                                         RFC by TCode
***********************************************************************
*                      --- TABLES ---
*----------------------------------------------------------------------
tables: zshreyemed,pa0167,pa0021,pa0106,*pa0167,*pa0021,
        *rfcdes.

data : begin of itab occurs 0,
        pernr like pa0000-pernr,
        seqno(2) type n,
        subty like pa0021-subty,
        objps like pa0021-objps,
        vorna like pa0002-vorna,
        midnm like pa0002-midnm,
        nachn like pa0002-nachn,
        stras like pa0006-stras,
        locat like pa0006-locat,
        ort01 like pa0006-ort01,

        state like pa0006-state,
        pstlz like pa0006-pstlz,
        gbdat like pa0002-gbdat,
        perid like pa0002-perid,
        telnr like q0006-telnr,

        com01 type pa0006-com01,
        num01 type pa0006-num01,
        com02 type pa0006-com02,
        num02 type pa0006-num02,
        com03 type pa0006-com03,
        num03 type pa0006-num03,
        com04 type pa0006-com04,
        num04 type pa0006-num04,
        com05 type pa0006-com05,
        num05 type pa0006-num05,
        com06 type pa0006-com06,
        num06 type pa0006-num06,

        gesch like pa0002-gesch,
        begda like pa0167-begda,
        endda like pa0167-endda,
        dtyxx like pa0167-dty01,
        depcv like pa0167-depcv,
        anzkd like pa0002-anzkd,
        trdat like pa0167-endda,
        begda_d like pa0021-begda,
        endda_d like pa0021-endda,
        fnmzu like pa0021-fnmzu,
        fgbdt like pa0021-fgbdt,
        flag,
       end of itab.
data g_itab like itab occurs 0  with header line.

data tmp_itab like itab occurs 10 with header line.

data : begin of i_pa0021 occurs 0,
        pernr like pa0021-pernr,
        subty like pa0021-subty,
        objps like pa0021-objps,
        begda like pa0021-begda,
        endda like pa0021-endda,
        fgbdt like pa0021-fgbdt,
        aedtm like pa0021-aedtm,
        fasex like pa0021-fasex,
        favor like pa0021-favor,
        fanam like pa0021-fanam,
        perid like pa0106-perid,
        erbnr like pa0021-erbnr,
        fnmzu like pa0021-fnmzu,
        finit like pa0021-finit,
     end of i_pa0021.


data : begin of i_pa0167 occurs 0,
        pernr like pa0167-pernr,
        begda like pa0167-begda,
        endda like pa0167-endda,
        depcv like pa0167-depcv,
        dty01 like pa0167-dty01,
        dty02 like pa0167-dty02,
        dty03 like pa0167-dty03,
        dty04 like pa0167-dty04,
        dty05 like pa0167-dty05,
        dty06 like pa0167-dty06,
        dty07 like pa0167-dty07,
        dty08 like pa0167-dty08,
        dty09 like pa0167-dty09,
        dty10 like pa0167-dty10,
        dty11 like pa0167-dty11,
        dty12 like pa0167-dty12,
        dty13 like pa0167-dty13,
        dty14 like pa0167-dty14,
        dty15 like pa0167-dty15,
        dty16 like pa0167-dty16,
        dty17 like pa0167-dty17,
        dty18 like pa0167-dty18,
        dt119 like pa0167-dty19,
        dty20 like pa0167-dty20,

        did01 like pa0167-did01,
        did02 like pa0167-did02,
        did03 like pa0167-did03,
        did04 like pa0167-did04,
        did05 like pa0167-did05,
        did06 like pa0167-did06,
        did07 like pa0167-did07,
        did08 like pa0167-did08,
        did09 like pa0167-did09,
        did10 like pa0167-did10,
        did11 like pa0167-did11,
        did12 like pa0167-did12,
        did13 like pa0167-did13,
        did14 like pa0167-did14,
        did15 like pa0167-did15,
        did16 like pa0167-did16,
        did17 like pa0167-did17,
        did18 like pa0167-did18,
        did19 like pa0167-did19,
        did20 like pa0167-did20,

        vorna like pa0002-vorna,
        midnm like pa0002-midnm,
        nachn like pa0002-nachn,
        gbdat like pa0002-gbdat,
        perid like pa0002-perid,
        gesch like pa0002-gesch,
        anzkd like pa0002-anzkd,
        stras like pa0006-stras,
        locat like pa0006-locat,
        ort01 like pa0006-ort01,
        state like pa0006-state,
        pstlz like pa0006-pstlz,
        telnr like pa0006-telnr,

        com01 type pa0006-com01,
        num01 type pa0006-num01,
        com02 type pa0006-com02,
        num02 type pa0006-num02,
        com03 type pa0006-com03,
        num03 type pa0006-num03,
        com04 type pa0006-com04,
        num04 type pa0006-num04,
        com05 type pa0006-com05,
        num05 type pa0006-num05,
        com06 type pa0006-com06,
        num06 type pa0006-num06,

        trdat like pa0167-endda,
        pltyp like pa0167-pltyp,

       end of i_pa0167.

data : begin of i_aedtm occurs 0,
        pernr like pa0167-pernr,
        begda like pa0167-begda,
        endda like pa0167-endda,
       end of i_aedtm.

data tmp_pa0167 like i_pa0167 occurs 1 with header line.

data itab_new like itab occurs 0 with header line.
data i_pa0167_new like i_pa0167 occurs 0 with header line.

data : begin of i_pernr occurs 0,
        pernr like pa0167-pernr,
       end of i_pernr.

data : begin of i_avail occurs 0,
        pernr like pa0167-pernr,
        begda like pa0167-begda,
        endda like pa0167-endda,
       end of i_avail.

data : begin of term_emp occurs 0,
        pernr like pa0167-pernr,
       end of term_emp.

data: begin of it_status occurs 0,
        pernr like pa0000-pernr,
        begda like pa0000-begda,
        massn like pa0000-massn,
        massg like pa0000-massg,
        stat2 like pa0000-stat2,
      end of it_status           .


data: begin of it_withdr occurs 0,
        pernr like pa0000-pernr,
        begda like pa0000-begda,
        massn like pa0000-massn,
        massg like pa0000-massg,
        stat2 like pa0000-stat2,
      end of it_withdr.

data: begin of it_zshreyemed occurs 0.
        include structure zshreyemed.
data    sort_k.
data: end of it_zshreyemed.

data: begin of it_zshreyemed_final occurs 0.
        include structure zshreyemed.
data: end of it_zshreyemed_final.

data   num(12) value ' 0123456789'.

data  $ix type i.
*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*

selection-screen begin of block 0 with frame title text-100.
select-options s_pernr for pa0167-pernr.
parameters : par_date like sy-datum default sy-datum no-display.

select-options : s_date for sy-datum no-extension.

parameters : p_pltyp like pa0167-pltyp default 'VISN'.

selection-screen end   of block 0.

selection-screen begin of block 1 with frame title text-101.
selection-screen begin of line.
parameters:
  par_r1 radiobutton group 1.
selection-screen:
  comment 03(29) text-003 for field par_r1.
parameters:
  par_file like rlgrap-filename.                            "UD1K953401
* par_file(50).                                             "UD1K953401

selection-screen:
  end of line,
  begin of line.
selection-screen:
  end of line,
  begin of line.
parameters:
  par_r2 radiobutton group 1 default 'X'.
selection-screen:
  comment 03(50) text-005 for field par_r2,
  end of line.
parameters : par_dest like rfcdes-rfcdest
                       default 'WMHR01'.
selection-screen end   of block 1.

parameters p_debug as checkbox.

define __cls.                          " clear & refresh
  clear &1.refresh &1.
end-of-definition.
define u_break.
  if not p_debug is initial.
    break-point.
  endif.
end-of-definition.
define __process.
  perform show_progress using &1 &2.
end-of-definition.

constants:  false value ' ',
            true  value 'X'.

****************************** Global Data *****************************

initialization.

  par_file    = 'c:\temp\HR_EYEMED'.
  par_file+17  = '-'.
  write sy-datlo to par_file+18(6) yymmdd.
  par_file+25 = '-'.
  par_file+26 = sy-timlo.
  concatenate par_file '.txt' into par_file.
  condense par_file no-gaps.

  s_date = 'IBT'.
  s_date-low = sy-datum - 13.
  s_date-high = sy-datum.
  append s_date.

* BEGIN OF UD1K954974
at selection-screen.
  if sy-tcode = 'ZHR_EYEMED_DATA' and not par_r2 is initial.
    message e000 with text-m01 text-m02.
  endif.
* END OF UD1K954974
*--------------------------------------------------------------------*
start-of-selection.
*--------------------------------------------------------------------*

  data $text(50).

  __cls i_aedtm.

  select a~pernr a~begda a~endda
    into corresponding fields of table i_aedtm
         from pa0167 as a
         where a~pernr in s_pernr
           and a~aedtm in s_date
           and a~pltyp = p_pltyp.

*  SORT i_aedtm BY pernr ascending " begda.
*                  endda descending.
*
*  DELETE ADJACENT DUPLICATES FROM i_aedtm
*      COMPARING pernr." begda.


  sort i_aedtm by pernr begda.

  delete adjacent duplicates from i_aedtm
      comparing pernr begda.

  ranges r_pernr for pa0167-pernr.
  ranges r_date  for sy-datum.

  __cls g_itab.

  sort i_aedtm by pernr.


  loop at i_aedtm.

    __cls : r_pernr, r_date .

    r_pernr = 'IEQ'.
    r_pernr-low = i_aedtm-pernr.
    r_pernr-high = i_aedtm-pernr.
    append r_pernr.

    r_date = 'IBT'.
    r_date-low = i_aedtm-begda.
    r_date-high = i_aedtm-endda.
    append r_date.

    par_date = r_date-low - 1.

    do 365 times.

      add 1 to par_date.
      check par_date in r_date.

      perform get_g_itab tables r_pernr
                          using par_date
                                true.
      exit.
    enddo.

  endloop.


  __cls term_emp.


  perform get_g_itab_all tables s_pernr.

  __cls it_zshreyemed.

  sort g_itab by pernr seqno.

  u_break.

  data $flag.
  data $cnt(2) type n.

  data: $_comxx type p0006-com01,
        $_numxx type p0006-num01.

  loop at g_itab.
    add 1 to $cnt.
    at end of pernr.
      $flag = true.
    endat.
    check $flag eq true.
    clear $flag.
    g_itab-anzkd = $cnt - 1.
    modify g_itab transporting anzkd where pernr eq g_itab-pernr
                                     and seqno eq '00'.
    clear $cnt.
  endloop.


  loop at g_itab.

    it_zshreyemed-grpcd = '9733478'.
    if g_itab-seqno eq '00'.
      move g_itab-pernr to it_zshreyemed-memid.
      it_zshreyemed-membtype = 'S'.
    else.
      concatenate g_itab-pernr g_itab-seqno into it_zshreyemed-memid.
      it_zshreyemed-membtype = 'D'.
    endif.

    it_zshreyemed-memfname = g_itab-vorna.
    it_zshreyemed-memmname = g_itab-midnm.                  "UD1K953401
*   it_zshreyemed-memmname = g_itab-midnm(0).               "UD1K953401
    it_zshreyemed-memlname = g_itab-nachn.
    it_zshreyemed-adrline1 = g_itab-stras.
    it_zshreyemed-adrline2 = g_itab-locat.
    it_zshreyemed-city     = g_itab-ort01.
    it_zshreyemed-state    = g_itab-state.
    it_zshreyemed-zip      = g_itab-pstlz.

    write g_itab-gbdat to it_zshreyemed-datebirth.

    it_zshreyemed-benefitopt = 'LEV1'.
    it_zshreyemed-divisioncd = ' '.
    it_zshreyemed-primarysubid = g_itab-pernr.
    it_zshreyemed-ssn = g_itab-perid.
*    it_zshreyemed-telno = g_itab-telnr.

    do 6 times varying $_comxx from g_itab-com01
                              next g_itab-com02

            varying $_numxx from g_itab-num01
                            next g_itab-num02.
      case $_comxx.
        when 'HOME'.
          it_zshreyemed-telno = $_numxx.
      endcase.
    enddo.

    if g_itab-gesch eq '1'.
      it_zshreyemed-gender = 'M'.
    else.
      it_zshreyemed-gender = 'F'.
    endif.
    it_zshreyemed-rsvforeyemed = ' '.

    write g_itab-begda to it_zshreyemed-effdate.

    if not g_itab-trdat is initial.
      write g_itab-trdat to it_zshreyemed-termdate.
    endif.

    case g_itab-dtyxx.
      when 'H'.
        it_zshreyemed-relcode = 'H'.
        it_zshreyemed-sort_k = '1'.
      when '1'.
        it_zshreyemed-relcode = 'S'.
        it_zshreyemed-sort_k = '2'.
      when '2' or '6' or '9' or '13'.
        it_zshreyemed-relcode = 'C'.
        it_zshreyemed-sort_k = '3'.
      when others.
        it_zshreyemed-relcode = 'O'.
        it_zshreyemed-sort_k = '4'.
    endcase.

    case g_itab-depcv.
      when 'EE'.
        it_zshreyemed-tier = '1'.
      when 'EE+1'.
        it_zshreyemed-tier = '2'.
      when 'EE+F'.
        it_zshreyemed-tier = '3'.
      when others.
        it_zshreyemed-tier = ' '.
    endcase.

    it_zshreyemed-updact = ' '.
    it_zshreyemed-loc_code = ' '.

    if g_itab-anzkd is initial.
      it_zshreyemed-num_of_depf = '0'.
    else.
      it_zshreyemed-num_of_depf = g_itab-anzkd.
    endif.
    if g_itab-depcv ne 'WAIV'.
      append it_zshreyemed.
    endif.
    clear it_zshreyemed.
  endloop.

  sort it_zshreyemed by grpcd memid sort_k.
  u_break.
  __cls it_zshreyemed_final.
  loop at it_zshreyemed.
    move-corresponding it_zshreyemed to it_zshreyemed_final.
    append it_zshreyemed_final.
  endloop.


*--------------------------------------------------------------------*
end-of-selection.
*--------------------------------------------------------------------*

  read table it_zshreyemed_final index 1.
  if sy-subrc ne 0.
    message s000 with 'Data not found!'.
    exit.
  endif.

  if par_r1 eq true.
    perform create_file.
  else.
    perform eyemed_eai.
  endif.

*&---------------------------------------------------------------------*
*&      Form  gqms_eai
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form eyemed_eai.

  select single * from rfcdes  into *rfcdes
                where rfcdest eq par_dest.
  if sy-subrc ne 0.
    message s000 with 'No RFC Destination was found!'.
    exit.
  endif.
  call function 'Z_HR_EYEMED_IF'
    destination par_dest
    tables
      zshreyemed = it_zshreyemed_final.

  if sy-subrc ne 0.
    message s000 with 'I/F error was occured!'.
  endif.

endform.                    " gqms_eai
*&---------------------------------------------------------------------*
*&      Form  create_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_file.

  if par_file eq space.
    par_file    = 'HR_EYEM'.
    par_file+8  = '-'.
    write sy-datlo to par_file+9(6) yymmdd.
    par_file+15 = '-'.
    par_file+16 = sy-timlo.
    concatenate par_file '.txt' into par_file.
    condense par_file no-gaps.
  endif.
  call function 'WS_DOWNLOAD'
    exporting
      filename        = par_file
    tables
      data_tab        = it_zshreyemed_final
    exceptions
      file_open_error = 1
      others          = 2.

  if sy-subrc <> 0.
    message s000 with 'ERROR OPENING/DOWNLOADING TO PC FILE.'.
  else.
    message s000 with 'File has been created successfully!:' par_file.
  endif.

endform.                    " create_file
**&---------------------------------------------------------------------
**
**&      Form  get_dep_code
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM get_dep_code.
*
*
*  CHECK NOT $job_pos[] IS INITIAL.
*
*  DATA : BEGIN OF $sobid OCCURS 0,
*          objid LIKE hrp1000-objid,
*         END OF $sobid.
*
*  LOOP AT $job_pos.
*    $sobid-objid = $job_pos-$sobid.
*    APPEND $sobid.
*  ENDLOOP.
*
*  SORT $sobid.
*  DELETE ADJACENT DUPLICATES FROM $sobid.
*
*  CHECK NOT $sobid[] IS INITIAL.
*
*  SELECT objid sobid INTO TABLE $dep_code
*      FROM hrp1001
*      FOR ALL ENTRIES IN $sobid
*                    WHERE otype EQ 'S'
*                      AND objid EQ $sobid-objid
*                      AND rsign EQ 'A'
*                      AND relat = '003'
*                      AND endda >= par_date
*                      AND begda <= par_date.
*
*  LOOP AT $dep_code.
*    $dep_code-$sobid = $dep_code-sobid(8).
*    MODIFY $dep_code INDEX sy-tabix TRANSPORTING $sobid.
*  ENDLOOP.
*  SORT  $dep_code BY  objid $sobid.
*
*ENDFORM.                    " get_dep_code
*---------------------------------------------------------------------*
*       FORM show_progress                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PF_TEXT                                                       *
*  -->  VALUE(PF_VAL)                                                 *
*---------------------------------------------------------------------*
form show_progress using    pf_text
                            value(pf_val).

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = pf_val
      text       = pf_text.

endform.                    " SHOW_PROGRESS
**&---------------------------------------------------------------------
**
**&      Form  get_itab
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**      -->P_I_PA0167  text
**      -->P_ITAB  text
**----------------------------------------------------------------------
**
*FORM get_itab TABLES   p_i_pa0167 STRUCTURE i_pa0167
*                       p_itab STRUCTURE itab.
*
*  __cls p_itab.
*
*  DATA : dtyxx TYPE ben_deptyp,
*         didxx TYPE ben_depid,
*         seqno(2) TYPE n.
*
*  LOOP AT p_i_pa0167.
*    CLEAR p_itab.
*    MOVE-CORRESPONDING p_i_pa0167 TO p_itab.
*
*    CLEAR seqno.
*    p_itab-dtyxx = 'H'.
*    APPEND p_itab.
*
*    DO 20 TIMES VARYING dtyxx FROM p_i_pa0167-dty01
*                              NEXT p_i_pa0167-dty02
*                VARYING didxx FROM p_i_pa0167-did01
*                              NEXT p_i_pa0167-did02.
*      IF dtyxx IS INITIAL.
*        EXIT.
*      ELSE.
*
*        SELECT SINGLE * FROM pa0021
*         WHERE pernr EQ p_i_pa0167-pernr
*           AND subty EQ dtyxx
*           AND objps EQ didxx
*           AND begda <= par_date
*           AND endda >= par_date.
*
*        IF sy-subrc EQ 0.
*
*          p_itab-vorna = pa0021-favor.
*          p_itab-midnm = space.
*          p_itab-nachn = pa0021-fanam.
*          p_itab-gbdat = pa0021-fgbdt.
*          p_itab-dtyxx = dtyxx.
*          p_itab-gesch = pa0021-fasex.
*          p_itab-objps = pa0021-objps.
*          p_itab-begda_d = pa0021-begda.
*          p_itab-endda_d = pa0021-endda.
*          p_itab-fnmzu = pa0021-fnmzu.
*          SELECT SINGLE perid INTO p_itab-perid
*          FROM pa0106
*           WHERE pernr EQ p_i_pa0167-pernr
*             AND subty EQ dtyxx.
*
*          ADD 1 TO seqno.
*          p_itab-seqno = seqno.
*          CLEAR p_itab-anzkd.
*
*          APPEND p_itab.
*
*        ENDIF.
*
*      ENDIF.
*
*    ENDDO.
*
*  ENDLOOP.
*
*ENDFORM.                    " get_itab
*&---------------------------------------------------------------------*
*&      Form  get_itab_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_PA0167  text
*      -->P_ITAB  text
*----------------------------------------------------------------------*
form get_itab_new tables p_i_pa0021 structure i_pa0021
                         p_i_pa0167 structure i_pa0167
                         p_itab structure itab.

  __cls p_itab.

  data : dtyxx type ben_deptyp,
         didxx type ben_depid,
         seqno(2) type n,
         $fr type i,
         $ix  type i.

  loop at p_i_pa0167 .

    read table term_emp with key pernr = p_i_pa0167-pernr
    binary search.

    if sy-subrc eq 0.
      continue.
    endif.

    if not p_i_pa0167-trdat is initial and
           p_i_pa0167-endda <= p_i_pa0167-trdat .

      read table i_aedtm with key pernr = p_i_pa0167-pernr
      binary search.
      if sy-subrc eq 0.
      else.
        if p_i_pa0167-trdat in s_date.
        else.
          term_emp-pernr =  p_i_pa0167-pernr.
          append term_emp.
          collect term_emp.
          sort term_emp.
          continue.
        endif.
      endif.
    endif.

    __cls tmp_itab.

    move-corresponding p_i_pa0167 to tmp_itab.

    clear seqno.
    tmp_itab-dtyxx = 'H'.
    tmp_itab-flag = true.
    append tmp_itab.
    clear tmp_itab.
    read table p_i_pa0021 with key pernr = p_i_pa0167-pernr
                                   binary search.
    $fr = sy-tabix.
    if sy-subrc eq 0.

      loop at  p_i_pa0021 from $fr.
        if p_i_pa0021-pernr ne p_i_pa0167-pernr.
          exit.
        endif.
        move-corresponding p_i_pa0167 to tmp_itab.
        tmp_itab-pernr = p_i_pa0021-pernr.
        tmp_itab-seqno = p_i_pa0021-erbnr(2).
        tmp_itab-subty = p_i_pa0021-subty.
        tmp_itab-objps = p_i_pa0021-objps.
        tmp_itab-begda = p_i_pa0021-begda.
        tmp_itab-endda = p_i_pa0021-endda.
        tmp_itab-vorna = p_i_pa0021-favor.
        tmp_itab-midnm = space.
        tmp_itab-nachn = p_i_pa0021-fanam.
        tmp_itab-gbdat = p_i_pa0021-fgbdt.
        tmp_itab-dtyxx = p_i_pa0021-subty.
        tmp_itab-gesch = p_i_pa0021-fasex.
        tmp_itab-begda_d = p_i_pa0021-begda.
        tmp_itab-endda_d = p_i_pa0021-endda.
        tmp_itab-perid = p_i_pa0021-perid.
        tmp_itab-fnmzu = p_i_pa0021-fnmzu.
        tmp_itab-fgbdt = p_i_pa0021-fgbdt.
        tmp_itab-midnm = p_i_pa0021-finit.
        clear tmp_itab-anzkd.
        append tmp_itab.
      endloop.

      sort tmp_itab by begda subty objps.

*    DELETE tmp_itab WHERE endda < s_date-low.
*
*    DELETE p_i_pa0021 WHERE  pernr = p_i_pa0167-pernr
*                             AND endda < par_date.

      sort tmp_itab by subty objps.


      do 20 times varying dtyxx from p_i_pa0167-dty01
                                next p_i_pa0167-dty02
                  varying didxx from p_i_pa0167-did01
                                next p_i_pa0167-did02.
        if dtyxx is initial.
          exit.
        else.

          read table p_i_pa0021 with key pernr = p_i_pa0167-pernr
                                         subty = dtyxx
                                         objps = didxx
                                         binary search.
          if sy-subrc eq 0.
            read table tmp_itab with key subty = dtyxx
                                         objps = didxx
                                         binary search.
            if sy-subrc eq 0.
              tmp_itab-flag = true.
* {
              tmp_itab-begda = p_i_pa0167-begda.
              tmp_itab-endda = p_i_pa0167-endda.
* }
              modify tmp_itab index sy-tabix transporting flag begda endda.
            endif.
          endif.
        endif.
      enddo.

      delete tmp_itab where flag ne true.

    endif.

    append lines of tmp_itab to p_itab.
  endloop.

endform.                    " get_itab
*&---------------------------------------------------------------------*
*&      Form  get_g_itab_all
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_g_itab_all tables p_s_pernr.

  data: l_comxx type p0006-com01,
        l_numxx type p0006-num01.

  data before_30d like par_date.
  before_30d = s_date-low - 30.


  __cls : i_pa0167, i_pa0167_new .



  par_date = s_date-low - 1.

  __cls i_avail.

  do 1000 times.

    add 1 to par_date.
    check par_date in s_date.
    select pernr begda endda
    into (i_avail-pernr,i_avail-begda,i_avail-endda)
          from pa0167
           where pernr in p_s_pernr
             and begda <= par_date
             and endda >= par_date
             and pltyp = p_pltyp.

      read table i_aedtm with key pernr = i_avail-pernr
                         binary search.
      if sy-subrc ne 0.
        collect i_avail.
      endif.
    endselect.

  enddo.

  sort i_avail.

  if not i_avail[] is initial.
    select a~pernr a~begda a~endda

           a~dty01 a~dty02 a~dty03
           a~dty04 a~dty05 a~dty06
           a~dty07 a~dty08 a~dty09
           a~dty10 a~dty11 a~dty12
           a~dty13 a~dty14 a~dty15
           a~dty16 a~dty17 a~dty18
           a~dty19 a~dty20

           a~did01 a~did02 a~did03
           a~did04 a~did05 a~did06
           a~did07 a~did08 a~did09
           a~did10 a~did11 a~did12
           a~did13 a~did14 a~did15
           a~did16 a~did17 a~did18
           a~did19 a~did20

           a~depcv
           b~vorna b~midnm b~nachn
           b~gbdat b~perid b~gesch
           b~anzkd
           c~stras c~locat c~ort01
           c~state c~pstlz c~telnr
* 09/18/2013 - T00306 Start
           c~com01 c~num01 c~com02
           c~num02 c~com03 c~num03
           c~com04 c~num04 c~com05
           c~num05 c~com06 c~num06
* 09/18/2013 - T00306 End
           a~pltyp

      into corresponding fields of table i_pa0167
            from pa0167 as a
           inner join pa0002 as b
              on b~pernr eq a~pernr
             and b~endda eq '99991231'
           inner join pa0006 as c
              on c~pernr eq b~pernr
             and c~endda eq '99991231'
          for all entries in i_avail
           where a~pernr eq i_avail-pernr
             and a~begda eq i_avail-begda
             and a~endda eq i_avail-endda
             and c~subty = '5'
             and a~pltyp = p_pltyp.

    sort i_pa0167 by  pernr.
  endif.


* 09/18/2013 - T00306 Start
  data: l_tabix type stabix.
  loop at i_pa0167.
    l_tabix = sy-tabix.
    do 6 times varying l_comxx from i_pa0167-com01
                               next i_pa0167-com02

             varying l_numxx from i_pa0167-num01
                             next i_pa0167-num02.
      case l_comxx.
        when 'HOME'.
          i_pa0167-telnr = l_numxx.
          modify i_pa0167 index l_tabix.
      endcase.
    enddo.
  endloop.
* 09/18/2013 - T00306 End


  __cls : it_status, it_withdr.
  select pernr  begda massn massg stat2 into table it_status
  from pa0000
  where
    begda >= before_30d
    and stat2 eq '0'.

  sort it_status by pernr ascending
                    begda descending .

  delete adjacent duplicates from it_status
      comparing pernr.

  sort it_status by pernr.

  select pernr  begda massn massg stat2 into table it_withdr
  from pa0000
  where aedtm >= before_30d
    and pernr in p_s_pernr
    and stat2 eq '0'.

  sort it_withdr by pernr.

  loop at it_withdr.
    read table i_pa0167 with key pernr = it_withdr-pernr binary search.
    if sy-subrc ne 0.

      perform get_add_i167 tables p_s_pernr
                          using before_30d.

    endif.
  endloop.

  loop at i_pa0167.
    clear i_pa0167-trdat.
    $ix = sy-tabix.
    read table it_status with key pernr = i_pa0167-pernr binary search.
    if sy-subrc eq 0.
* Withdrawn{
      i_pa0167-trdat = it_status-begda - 1.
      modify i_pa0167 index $ix transporting trdat.
* }
    endif.

    read table it_withdr with key pernr = i_pa0167-pernr binary search.
    if sy-subrc eq 0.
* Withdrawn{
      if i_pa0167-endda ne '99991231'.
        i_pa0167-trdat = i_pa0167-endda.
        modify i_pa0167 index $ix transporting trdat.
      endif.
* }
    endif.

  endloop.

  __cls i_pernr.

  loop at i_pa0167.
    $ix = sy-tabix.

    if i_pa0167-endda ne '99991231'.
      clear *pa0167.
      select * into *pa0167
         from pa0167 where pernr eq i_pa0167-pernr
         and subty eq p_pltyp
         order by endda.
        if *pa0167-endda > i_pa0167-endda.
          exit.
        endif.
      endselect.

      if *pa0167-endda eq i_pa0167-endda or
         *pa0167-endda is initial.
        if i_pa0167-endda in s_date.
* VISN status was terminated for this employee {
          i_pa0167-trdat = i_pa0167-endda.
          modify i_pa0167 index $ix transporting trdat.
        endif.
* }
      else.

* VISN status was changed for this employee {
        i_pernr-pernr = i_pa0167-pernr.
        append i_pernr.
* }
      endif.
    endif.

  endloop.

  if not i_pernr[] is initial.

    select a~pernr a~begda a~endda

           a~dty01 a~dty02 a~dty03
           a~dty04 a~dty05 a~dty06
           a~dty07 a~dty08 a~dty09
           a~dty10 a~dty11 a~dty12
           a~dty13 a~dty14 a~dty15
           a~dty16 a~dty17 a~dty18
           a~dty19 a~dty20

           a~did01 a~did02 a~did03
           a~did04 a~did05 a~did06
           a~did07 a~did08 a~did09
           a~did10 a~did11 a~did12
           a~did13 a~did14 a~did15
           a~did16 a~did17 a~did18
           a~did19 a~did20 a~depcv
           a~pltyp

     into corresponding fields of table i_pa0167_new
           from pa0167 as a
          for all entries in i_pernr
           where a~pernr eq i_pernr-pernr
             and a~endda >= s_date-high
             and a~pltyp = p_pltyp
             and a~pernr in p_s_pernr.

    sort i_pa0167_new by pernr endda.

    delete adjacent duplicates from i_pa0167_new
    comparing pernr.

    loop at i_pa0167_new.
      $ix = sy-tabix.
      read table i_pa0167 with key pernr = i_pa0167_new-pernr
                          binary search.
      if sy-subrc eq 0.
        i_pa0167_new-vorna = i_pa0167-vorna.
        i_pa0167_new-midnm = i_pa0167-midnm.
        i_pa0167_new-nachn = i_pa0167-nachn.
        i_pa0167_new-gbdat = i_pa0167-gbdat.
        i_pa0167_new-perid = i_pa0167-perid.
        i_pa0167_new-gesch = i_pa0167-gesch.
        i_pa0167_new-anzkd = i_pa0167-anzkd.
        i_pa0167_new-stras = i_pa0167-stras.
        i_pa0167_new-locat = i_pa0167-locat.
        i_pa0167_new-ort01 = i_pa0167-ort01.
        i_pa0167_new-state = i_pa0167-state.
        i_pa0167_new-pstlz = i_pa0167-pstlz.
        i_pa0167_new-telnr = i_pa0167-telnr.
        modify i_pa0167_new index $ix.
      endif.

    endloop.

  endif.

  if i_pa0021[] is initial.

    select a~pernr a~subty a~objps
           a~begda a~endda a~fgbdt
           a~aedtm a~fasex
           a~favor a~fanam a~erbnr a~fnmzu a~finit
           b~perid
    into corresponding fields of table i_pa0021
      from pa0021 as a
      inner join pa0106 as b
         on b~pernr eq a~pernr
        and b~subty eq a~subty
        and b~objps eq a~objps
        and b~sprps eq a~sprps
        and b~endda eq a~endda
        and b~begda eq a~begda
        and b~seqnr eq a~seqnr
        where a~pernr in s_pernr
          and a~subty ne '7'
          and a~endda >= s_date-low.

  endif.

  sort i_pa0021 by pernr subty objps begda .

  perform get_itab_new tables i_pa0021
                              i_pa0167
                              itab.
  perform get_itab_new tables i_pa0021
                              i_pa0167_new
                              itab_new.

  sort : i_pernr,
         itab_new by pernr objps,
         i_pa0167_new by pernr.

  delete itab_new where seqno eq '00'.

  sort : itab by pernr seqno.

  loop at itab.

    $ix = sy-tabix.
    check itab-seqno ne '00'. " exclude self.

    read table i_pernr with key pernr = itab-pernr binary search.
    check sy-subrc eq 0.

    read table itab_new with key pernr = itab-pernr
                                 objps = itab-objps
                                 binary search.

    if sy-subrc ne 0.
      read table i_pa0167_new with key pernr = itab-pernr
                                   binary search.
      if sy-subrc eq 0.
        itab-trdat = i_pa0167_new-begda - 1.
        modify itab index $ix transporting trdat.
      endif.
    endif.

  endloop.

  append lines of itab to g_itab.

  sort g_itab by pernr vorna midnm nachn fnmzu fgbdt ascending
                 begda descending.

  delete adjacent duplicates from g_itab
      comparing pernr vorna midnm nachn fnmzu fgbdt.


  __cls itab.



endform.                    " get_g_itab

*---------------------------------------------------------------------*
*       FORM get_g_itab                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_S_PERNR                                                     *
*  -->  P_P_DATE                                                      *
*  -->  P_FORCE                                                       *
*---------------------------------------------------------------------*
form get_g_itab tables p_s_pernr
                 using p_p_date p_force.

  data before_30d like par_date.
  data: l_comxx type p0006-com01,
        l_numxx type p0006-num01.

  __cls : i_pa0167, i_pa0167_new .

  select a~pernr a~begda a~endda

         a~dty01 a~dty02 a~dty03
         a~dty04 a~dty05 a~dty06
         a~dty07 a~dty08 a~dty09
         a~dty10 a~dty11 a~dty12
         a~dty13 a~dty14 a~dty15
         a~dty16 a~dty17 a~dty18
         a~dty19 a~dty20

         a~did01 a~did02 a~did03
         a~did04 a~did05 a~did06
         a~did07 a~did08 a~did09
         a~did10 a~did11 a~did12
         a~did13 a~did14 a~did15
         a~did16 a~did17 a~did18
         a~did19 a~did20

         a~depcv
         b~vorna b~midnm b~nachn
         b~gbdat b~perid b~gesch
         b~anzkd
         c~stras c~locat c~ort01
         c~state c~pstlz c~telnr
* 09/18/2013 - T00306 Start
         c~com01 c~num01 c~com02
         c~num02 c~com03 c~num03
         c~com04 c~num04 c~com05
         c~num05 c~com06 c~num06
* 09/18/2013 - T00306 End
         a~pltyp

    into corresponding fields of table i_pa0167
         from pa0167 as a
         inner join pa0002 as b
            on b~pernr eq a~pernr
           and b~endda eq '99991231'
         inner join pa0006 as c
            on c~pernr eq b~pernr
           and c~endda eq '99991231'
         where a~pernr in p_s_pernr
           and a~begda <= p_p_date
           and a~endda >= p_p_date
           and c~subty = '5'
           and a~pltyp = p_pltyp.

  sort i_pa0167 by  pernr.



* 09/18/2013 - T00306 Start
  data: l_tabix type stabix.
  loop at i_pa0167.
    l_tabix = sy-tabix.
    do 6 times varying l_comxx from i_pa0167-com01
                               next i_pa0167-com02

             varying l_numxx from i_pa0167-num01
                             next i_pa0167-num02.
      case l_comxx.
        when 'HOME'.
          i_pa0167-telnr = l_numxx.
          modify i_pa0167 index l_tabix.
      endcase.
    enddo.
  endloop.
* 09/18/2013 - T00306 End



  __cls : it_status, it_withdr.
  select pernr  begda massn massg stat2 into table it_status
  from pa0000
  where
    begda <= p_p_date
    and endda >= p_p_date
    and stat2 eq '0'.

  sort it_status by pernr ascending
                    begda descending .
  delete adjacent duplicates from it_status
      comparing pernr.

  sort it_status by pernr.
  before_30d = p_p_date - 30.

  select pernr  begda massn massg stat2 into table it_withdr
  from pa0000
  where aedtm >= before_30d
    and pernr in p_s_pernr
    and stat2 eq '0'.

  sort it_withdr by pernr.

  loop at it_withdr.
    if p_force eq true.

      perform get_add_i167 tables p_s_pernr
                          using p_p_date.

    else.
      read table i_pa0167 with key pernr = it_withdr-pernr binary search.
      if sy-subrc ne 0.

        perform get_add_i167 tables p_s_pernr
                            using p_p_date.

      endif.
    endif.
  endloop.

  loop at i_pa0167.
    clear i_pa0167-trdat.
    $ix = sy-tabix.
    read table it_status with key pernr = i_pa0167-pernr binary search.
    if sy-subrc eq 0.
* Withdrawn{
      i_pa0167-trdat = it_status-begda - 1.
      modify i_pa0167 index $ix transporting trdat.
* }
    endif.

    read table it_withdr with key pernr = i_pa0167-pernr binary search.
    if sy-subrc eq 0.
* Withdrawn{
      if i_pa0167-endda ne '99991231'.
        i_pa0167-trdat = i_pa0167-endda.
        modify i_pa0167 index $ix transporting trdat.
      endif.
* }
    endif.

  endloop.

  __cls i_pernr.

  loop at i_pa0167.
    $ix = sy-tabix.

    if i_pa0167-endda ne '99991231'.
      clear *pa0167.
      select * into *pa0167
         from pa0167 where pernr eq i_pa0167-pernr
         and subty eq p_pltyp
         order by endda.
        if *pa0167-endda > i_pa0167-endda.
          exit.
        endif.
      endselect.

      if *pa0167-endda eq i_pa0167-endda or
         *pa0167-endda is initial.
        if i_pa0167-endda in s_date.
* VISN status was terminated for this employee {
          i_pa0167-trdat = i_pa0167-endda.
          modify i_pa0167 index $ix transporting trdat.
        endif.
* }
      else.

* VISN status was changed for this employee {
        i_pernr-pernr = i_pa0167-pernr.
        append i_pernr.
* }
      endif.
    endif.

  endloop.

  if not i_pernr[] is initial.

    select a~pernr a~begda a~endda

           a~dty01 a~dty02 a~dty03
           a~dty04 a~dty05 a~dty06
           a~dty07 a~dty08 a~dty09
           a~dty10 a~dty11 a~dty12
           a~dty13 a~dty14 a~dty15
           a~dty16 a~dty17 a~dty18
           a~dty19 a~dty20

           a~did01 a~did02 a~did03
           a~did04 a~did05 a~did06
           a~did07 a~did08 a~did09
           a~did10 a~did11 a~did12
           a~did13 a~did14 a~did15
           a~did16 a~did17 a~did18
           a~did19 a~did20 a~depcv
           a~pltyp

     into corresponding fields of table i_pa0167_new
           from pa0167 as a
          for all entries in i_pernr
           where a~pernr eq i_pernr-pernr
             and a~endda >= s_date-high
             and a~pltyp = p_pltyp
             and a~pernr in p_s_pernr.

    sort i_pa0167_new by pernr endda.

    delete adjacent duplicates from i_pa0167_new
    comparing pernr.

    loop at i_pa0167_new.
      $ix = sy-tabix.
      read table i_pa0167 with key pernr = i_pa0167_new-pernr
                          binary search.
      if sy-subrc eq 0.
        i_pa0167_new-vorna = i_pa0167-vorna.
        i_pa0167_new-midnm = i_pa0167-midnm.
        i_pa0167_new-nachn = i_pa0167-nachn.
        i_pa0167_new-gbdat = i_pa0167-gbdat.
        i_pa0167_new-perid = i_pa0167-perid.
        i_pa0167_new-gesch = i_pa0167-gesch.
        i_pa0167_new-anzkd = i_pa0167-anzkd.
        i_pa0167_new-stras = i_pa0167-stras.
        i_pa0167_new-locat = i_pa0167-locat.
        i_pa0167_new-ort01 = i_pa0167-ort01.
        i_pa0167_new-state = i_pa0167-state.
        i_pa0167_new-pstlz = i_pa0167-pstlz.
        i_pa0167_new-telnr = i_pa0167-telnr.
        modify i_pa0167_new index $ix.
      endif.

    endloop.

  endif.

  if i_pa0021[] is initial.

    select a~pernr a~subty a~objps
           a~begda a~endda a~fgbdt
           a~aedtm a~fasex
           a~favor a~fanam a~erbnr a~fnmzu a~finit
           b~perid
    into corresponding fields of table i_pa0021
      from pa0021 as a
      inner join pa0106 as b
         on b~pernr eq a~pernr
        and b~subty eq a~subty
        and b~objps eq a~objps
        and b~sprps eq a~sprps
        and b~endda eq a~endda
        and b~begda eq a~begda
        and b~seqnr eq a~seqnr
        where a~pernr in s_pernr
          and a~subty ne '7'
          and a~endda >= s_date-low.
  endif.

  sort i_pa0021 by pernr subty objps begda .

  perform get_itab_new tables i_pa0021
                              i_pa0167
                              itab.
  perform get_itab_new tables i_pa0021
                              i_pa0167_new
                              itab_new.

  sort : i_pernr,
         itab_new by pernr objps,
         i_pa0167_new by pernr.

  delete itab_new where seqno eq '00'.

  sort : itab by pernr seqno.

  loop at itab.

    $ix = sy-tabix.
    check itab-seqno ne '00'. " exclude self.

    read table i_pernr with key pernr = itab-pernr binary search.
    check sy-subrc eq 0.

    read table itab_new with key pernr = itab-pernr
                                 objps = itab-objps
                                 binary search.

    if sy-subrc ne 0.
      read table i_pa0167_new with key pernr = itab-pernr
                                   binary search.
      if sy-subrc eq 0.
        itab-trdat = i_pa0167_new-begda - 1.
        modify itab index $ix transporting trdat.
      endif.
    endif.

  endloop.

  append lines of itab to g_itab.

  sort g_itab by pernr vorna midnm nachn fnmzu fgbdt ascending
                 begda descending.

  delete adjacent duplicates from g_itab
      comparing pernr vorna midnm nachn fnmzu fgbdt.

  __cls itab.



endform.                    " get_g_itab

*&---------------------------------------------------------------------*
*&      Form  get_add_i167
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_add_i167 tables p_s_pernr
                            using p_p_date.

  __cls tmp_pa0167.


  select a~pernr a~begda a~endda

         a~dty01 a~dty02 a~dty03
         a~dty04 a~dty05 a~dty06
         a~dty07 a~dty08 a~dty09
         a~dty10 a~dty11 a~dty12
         a~dty13 a~dty14 a~dty15
         a~dty16 a~dty17 a~dty18
         a~dty19 a~dty20

         a~did01 a~did02 a~did03
         a~did04 a~did05 a~did06
         a~did07 a~did08 a~did09
         a~did10 a~did11 a~did12
         a~did13 a~did14 a~did15
         a~did16 a~did17 a~did18
         a~did19 a~did20

         a~depcv
         b~vorna b~midnm b~nachn
         b~gbdat b~perid b~gesch
         b~anzkd
         c~stras c~locat c~ort01
         c~state c~pstlz c~telnr
* 09/18/2013 - T00306 Start
         c~com01 c~num01 c~com02
         c~num02 c~com03 c~num03
         c~com04 c~num04 c~com05
         c~num05 c~com06 c~num06
* 09/18/2013 - T00306 End
         a~pltyp
   into corresponding fields of table tmp_pa0167
         from pa0167 as a
         inner join pa0002 as b
            on b~pernr eq a~pernr
           and b~begda <= p_p_date
         inner join pa0006 as c
            on c~pernr eq b~pernr
           and c~begda >= p_p_date
         where a~pernr eq it_withdr-pernr
           and a~begda >= p_p_date
           and c~subty = '5'
           and a~pltyp = p_pltyp
           and a~pernr in p_s_pernr.

  sort tmp_pa0167 by pernr ascending
                   endda descending .

  delete adjacent duplicates from tmp_pa0167
    comparing pernr.

  append lines of tmp_pa0167 to i_pa0167.

endform.                    " get_add_i167
