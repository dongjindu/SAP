************************************************************************
* Program Name      : ZIPP111I_APS_7GB1
* Author            : JongOh, Kim
* Creation Date     : 2003.08.22
* Specifications By : JongOh, Kim
* Pattern           : 5.2.3
* Development Request No : UD1K901977
* Addl Documentation:
* Description       : Workorder Information Interface
*                     - ALC and HPCS Code (7GB)
* Modification Logs
* Date       Developer    RequestNo    Description
* 8/23/07    IG.MOON      UD1K941473   Performance Tuning
************************************************************************
report zipp111i_aps_7gb1 no standard page heading
                          line-size 1023
                          message-id zmpp.

*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
tables : mara,            "General Material Data
         ztpp_pmt07gb,    "ALC and HPCS Code
         ztpp_pmt07ob,    "Selected ALC Code (Inbound Interface)
         ztpp_wosum.      "ERP_WO QTY SUMMARY

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
data : it_ztpp_pmt07ob  like table of ztpp_pmt07ob with header line,
       it_ztpp_wosum    like table of ztpp_wosum   with header line,
       it_ztpp_pmt07gb  like table of ztpp_pmt07gb with header line,
       it_wohd          like table of zspp_vin_value with header line,
       it_wocl          like table of zspp_vin_value with header line.

data : begin of it_error occurs 0,
         wohd   like  mara-matnr,
         extc   like  ztpp_wosum-extc,
         intc   like  ztpp_wosum-intc,
         type,
         zmsg   like  ztpp_pmt07ob-zmsg.
data : end of it_error.

* UD1K941430 by IG.MOON 8/23/2007 {

data: begin of it_wosum_n  occurs 0,
 $wa_wocl     like equi-equnr,
  wo_ser	like ztpp_wosum-wo_ser,	
  nation	like ztpp_wosum-nation,	
  dealer	like ztpp_wosum-dealer,	
  extc		like ztpp_wosum-extc,		
  intc		like ztpp_wosum-intc,		
  wa_wocl     like mara-matnr,
end of it_wosum_n.

data :
    begin of l_ausp occurs 0,
        objek like ausp-objek,
        atnam like cabn-atnam,
        atwrt like ausp-atwrt,
    end of l_ausp.

data :
    begin of l_auspl occurs 0,
        objek like ausp-objek,
        atnam like cabn-atnam,
        atwrt like ausp-atwrt,
    end of l_auspl.

define __cls.                          " clear & refresh
  clear &1.refresh &1.
end-of-definition.

* }

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
data : wa_line_nb       like sy-tabix.

data : wa_error_flg,
       wa_error_ix      type i,
       wa_wosum_ix      type i.
*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
constants : c_plnt      like ztpp_pmt07gb-plnt  value '1'.

************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
selection-screen: begin of block b1 with frame .
selection-screen begin of line.
parameters: p_run          type c as checkbox  default 'X'.
selection-screen comment  (55) text-001 for field p_run.
selection-screen end of line.
selection-screen: end of block b1.

************************************************************************
* INITIALIZAION
************************************************************************
initialization.
*  PERFORM INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
at selection-screen.
*  PERFORM AT_SELECTION-SCREEN.

************************************************************************
* START-OF-SELECTION
************************************************************************
start-of-selection.
  check p_run = 'X'  .
  perform excute_process.

************************************************************************
* END-OF-SELECTION
************************************************************************
end-of-selection.
  perform list_process.


*&---------------------------------------------------------------------*
*&      Form  EXCUTE_PROCESS
*&---------------------------------------------------------------------*
form excute_process.

*  PERFORM START_TIME.
*-----> Selected ALC Code (Inbound Interface)
  clear : it_ztpp_pmt07ob, it_ztpp_pmt07ob[].
  select *
         into table it_ztpp_pmt07ob
         from ztpp_pmt07ob.
  if sy-subrc eq 0.
* UD1K941473 by IG.MOON 8/25/2007 {
  if '<run fast'>'!'.
    perform append_pmt07gb_n. " tunned
  else.
    perform append_pmt07gb.   " old
  endif.
* }
  else.
    wa_error_flg = 'X'.
    message w000 with 'No Data in The Table - 07OB'.
  endif.

endform.                    " EXCUTE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  WOCL_TO_PMT07GB
*&---------------------------------------------------------------------*
form wocl_to_pmt07gb using p_ind p_atnam  p_field.
  case p_ind.
    when 'ALC'.
      data : l_fname(40).
      field-symbols: <fs_pmt07gb>.
      clear it_wocl.
      read table it_wocl with key atnam = p_atnam.
      if sy-subrc eq 0.
        concatenate 'IT_ZTPP_PMT07GB' '-' p_field into l_fname.
        assign (l_fname)    to  <fs_pmt07gb>.
        move it_wocl-atwrt  to  <fs_pmt07gb>.
      endif.

    when others.
      clear it_wocl.
      read table it_wocl with key atnam = p_atnam.
      if sy-subrc eq 0.
        move it_wocl-atwrt  to  p_field    .
      endif.
  endcase.
endform.                    " WOCL_TO_PMT07GB
*&---------------------------------------------------------------------*
*&      Form  WOHD_TO_PMT07GB
*&---------------------------------------------------------------------*
form wohd_to_pmt07gb using p_atnam  p_field.
  data : l_fname(40).
  field-symbols: <fs_pmt07gb>.
  clear it_wohd.
  read table it_wohd with key atnam = p_atnam.
  if sy-subrc eq 0.
    concatenate 'IT_ZTPP_PMT07GB' '-' p_field into l_fname.
    assign (l_fname)    to  <fs_pmt07gb>.
    move it_wohd-atwrt  to  <fs_pmt07gb>.
  endif.
endform.                    " WOHD_TO_PMT07GB

*// performance tuning

form wohd_to_pmt07gb_n using p_atnam  p_field.
  data : l_fname(40).
  field-symbols: <fs_pmt07gb>.
  clear l_ausp.
  read table l_ausp with key  objek = it_wosum_n-$wa_wocl
                              atnam = p_atnam binary search.
  if sy-subrc eq 0.
    concatenate 'IT_ZTPP_PMT07GB' '-' p_field into l_fname.
    assign (l_fname)    to  <fs_pmt07gb>.
    move l_ausp-atwrt  to  <fs_pmt07gb>.
  endif.
endform.                    " WOHD_TO_PMT07GB

*&---------------------------------------------------------------------*
*&      Form  PART_VALUE
*&---------------------------------------------------------------------*
form part_value using   p_ind.
  data : l_index_2(2)   type   n,
         l_index_1(1)   type   n,
         l_index(2)     type   n.
  data : l_fname_pmt07ob(40),
         l_fname_pmt07ob1(40),
         l_fname_pmt07gb(40),
         l_field_wo(40),
         l_field_pmt07gb(40).
  data : l_field_1a(10),  "PMT07OB UPxx, CPxx FIELD
         l_field_1b(10),  "PMT07GB USERxx, CSERxx FIELD
         l_field_2a(10),  "WOHD, WOCL Charicteristic name
         l_field_2b(10),  "PMT07GB UVALxx, CVALxx FIELD
         l_val1(1)     type    n,
         l_val2(2)     type    n,
         l_val3(3)     type    n.

  field-symbols : <fs_pmt07ob>,
                  <fs_pmt07gb>.

  case p_ind.
    when 'WOHD'.
      move : 'UP'        to   l_field_1a,
             'USER'      to   l_field_1b,
             'P_ALC_U_'  to   l_field_2a,
             'UVAL'      to   l_field_2b.
    when 'WOCL'.
      move : 'CP'        to   l_field_1a,
             'CSER'      to   l_field_1b,
             'P_ALC_C_'  to   l_field_2a,
             'CVAL'      to   l_field_2b.
  endcase.

  clear l_index.
  do.
    l_index_2 = sy-index.
    concatenate 'IT_ZTPP_PMT07OB' '-' l_field_1a l_index_2
                 into l_fname_pmt07ob.
    concatenate 'IT_ZTPP_PMT07GB' '-' l_field_1b l_index_2
                 into l_fname_pmt07gb.
    assign (l_fname_pmt07ob)  to <fs_pmt07ob>.
    assign (l_fname_pmt07gb)  to <fs_pmt07gb>.
    if <fs_pmt07ob> le 0.
      exit.
    else.
      if l_index eq '20' and p_ind eq 'WOHD'.
        exit.
      elseif l_index eq '10' and p_ind eq 'WOCL'.
        exit.
      endif.
      <fs_pmt07gb> = <fs_pmt07ob>.
      if <fs_pmt07gb> le '009'.
        l_val1 = <fs_pmt07gb>.
        concatenate l_field_2a l_val1 into l_field_wo.
      elseif <fs_pmt07gb> ge '100'.
        l_val3 = <fs_pmt07gb>.
        concatenate l_field_2a l_val3 into l_field_wo.
      else.
        l_val2 = <fs_pmt07gb>.
        concatenate l_field_2a l_val2 into l_field_wo.
      endif.
      concatenate l_field_2b l_index_2  into  l_field_pmt07gb.

      case p_ind.
        when 'WOHD'.
          perform wohd_to_pmt07gb
                         using l_field_wo l_field_pmt07gb.
          if not it_wohd-atwrt is initial.
            l_index = l_index + 1.
          endif.
        when 'WOCL'.
          perform wocl_to_pmt07gb
                         using 'ALC' l_field_wo l_field_pmt07gb.
          if not it_wocl-atwrt is initial.
            l_index = l_index + 1.
          endif.
      endcase.
    endif.
  enddo.
endform.                    " UNIQUE_PART_VALUE
*&---------------------------------------------------------------------*
*&      Form  PART_VALUE_WITHOUT
*&---------------------------------------------------------------------*
form part_value_without using  p_ind.
  data : l_index(2)    type    n,
         l_no          type    i,
         l_val1(1)     type    n,
         l_val2(2)     type    n,
         l_val3(3)     type    n,
         l_index_ix(3) type    n.
  data : l_fname_pmt07gb(40),
         l_field_wo(40),
         l_field_pmt07gb(40).
  data : l_field_1a(10),  "PMT07OB UPxx, CPxx FIELD
         l_field_1b(10),  "PMT07GB USERxx, CSERxx FIELD
         l_field_2a(10),  "WOHD, WOCL Charicteristic name
         l_field_2b(10).  "PMT07GB UVALxx, CVALxx FIELD
  field-symbols : <fs_pmt07gb>.

  case p_ind.
    when 'WOHD'.
      l_no = 200.
      move : 'USER'      to   l_field_1b,
             'P_ALC_U_'  to   l_field_2a,
             'UVAL'      to   l_field_2b.
    when 'WOCL'.
      l_no = 50.
      move : 'CSER'      to   l_field_1b,
             'P_ALC_C_'  to   l_field_2a,
             'CVAL'      to   l_field_2b.
  endcase.

  l_index = '01'.
  do 200 times.
    l_index_ix = sy-index.
    concatenate 'IT_ZTPP_PMT07GB' '-' l_field_1b l_index
                 into l_fname_pmt07gb.
    assign (l_fname_pmt07gb)  to <fs_pmt07gb>.
    <fs_pmt07gb> = l_index_ix.
    if <fs_pmt07gb> le '009'.
      l_val1 = <fs_pmt07gb>.
      concatenate l_field_2a l_val1 into l_field_wo.
    elseif <fs_pmt07gb> ge '100'.
      l_val3 = <fs_pmt07gb>.
      concatenate l_field_2a l_val3 into l_field_wo.
    else.
      l_val2 = <fs_pmt07gb>.
      concatenate l_field_2a l_val2 into l_field_wo.
    endif.
    concatenate l_field_2b l_index  into  l_field_pmt07gb.

    case p_ind.
      when 'WOHD'.
        perform wohd_to_pmt07gb
                       using l_field_wo l_field_pmt07gb.
        if not it_wohd-atwrt is initial.
          l_index = l_index + 1.
        endif.
      when 'WOCL'.
        perform wocl_to_pmt07gb
                       using 'ALC' l_field_wo l_field_pmt07gb.
        if not it_wocl-atwrt is initial.
          l_index = l_index + 1.
        endif.
    endcase.

    if l_index eq '20' and p_ind eq 'WOHD'.
      exit.
    elseif l_index eq '10' and p_ind eq 'WOCL'.
      exit.
    endif.
  enddo.
endform.                    " PART_VALUE_WITHOUT
*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form list_process.
  data: l_text(60) type c.

  if wa_error_flg eq 'X'.
*    WRITE :/ TEXT-303.
    message w001 with text-303.
  else.
    describe table it_ztpp_pmt07gb lines wa_line_nb.
    write wa_line_nb to l_text left-justified.
    if wa_line_nb ne 0.
*-------> DELETE ZTPP_PMT07GB & INSERT ZTPP_PMT07GB
      delete from ztpp_pmt07gb
             client specified
             where mandt eq sy-mandt.
      insert ztpp_pmt07gb from table it_ztpp_pmt07gb.
      if sy-subrc eq 0.
        commit work.
        concatenate 'Created Record Count :' l_text
          into l_text.
        message s001 with l_text .
*        FORMAT COLOR COL_HEADING.
*        WRITE :/ TEXT-311.
        if sy-batch = ' '.
          message s001 with text-311.
        endif.
        format reset intensified on.
*        WRITE :/5 'Created by' , SY-UNAME .
*        WRITE :/5 'Total Counted: ' , WA_WOSUM_IX COLOR COL_KEY.
*        WRITE :/5 'Created on' , SY-DATUM, SY-UZEIT.
*        WRITE :/5 'Created Data : ' , WA_LINE_NB  COLOR COL_POSITIVE.
*        WRITE :/5 'Errored Data : ' , WA_ERROR_IX COLOR COL_NEGATIVE.
*        SKIP 2.
**-----> Detailed ERROR List
*        PERFORM DETAILED_ERROR.

      else.
        rollback work.
        if sy-batch = ' '.
          message w001 with text-303.
        endif.
      endif.
    endif.
  endif.
*  ELSE.
*    MESSAGE E001 WITH 'Relevant Work Order Summary not exist!!'.
*  ENDIF.
*  PERFORM END_TIME.
endform.                    " LIST_PROCESS
*&---------------------------------------------------------------------*
*&      Form  APPEND_PMT07GB
*&---------------------------------------------------------------------*
form append_pmt07gb.
  data : l_tabix    like   sy-tabix,
         l_idnum    like   sy-tabix,
         l_matnr_cl like   mara-matnr,
         l_matnr_hd like   mara-matnr.
*-----> WORK ORDER SUMMARY TABLE (ZTPP_WOSUM)
  clear : it_ztpp_wosum, it_ztpp_wosum[],
          it_error, it_error[].

  select * into table it_ztpp_wosum
    from ztpp_wosum as za
   where za~modqty > 0
     and za~modqty > za~rp09tq .

  sort it_ztpp_wosum.
  describe table it_ztpp_wosum lines wa_wosum_ix.

  loop at it_ztpp_wosum.
*-----> CREATE ZTPP_PMT07GB TABLE
    at new dealer.
      clear : l_matnr_hd, it_wohd, it_wohd[].
      concatenate it_ztpp_wosum-wo_ser
                  it_ztpp_wosum-nation
                  it_ztpp_wosum-dealer  into  l_matnr_hd.

      call function 'Z_FPP_HANDLING_MASTER'
           exporting
                object       = l_matnr_hd
                ctype        = '001'
                display      = 'X'
           tables
                val_table    = it_wohd
           exceptions
                no_data      = 1
                error_mode   = 2
                error_object = 3
                others       = 4.

    endat.
    clear : it_wohd.
*---> CHECK P_PERF_YN
    read table it_wohd with key atnam = 'P_PERF_YN'.
    if it_wohd-atwrt ne 'Y'.
      wa_error_ix = wa_error_ix + 1.
      it_error-wohd = l_matnr_hd.
      it_error-extc = it_ztpp_wosum-extc.
      it_error-intc = it_ztpp_wosum-intc.
      it_error-type = 'E'.
      it_error-zmsg = text-301.
      append it_error.
    else.
      clear : l_matnr_cl, it_wocl, it_wocl[].
      concatenate it_ztpp_wosum-wo_ser  it_ztpp_wosum-nation
                  it_ztpp_wosum-dealer  it_ztpp_wosum-extc
                  it_ztpp_wosum-intc    into  l_matnr_cl.

      call function 'Z_FPP_HANDLING_MASTER'
           exporting
                object       = l_matnr_cl
                ctype        = '001'
                display      = 'X'
           tables
                val_table    = it_wocl
           exceptions
                no_data      = 1
                error_mode   = 2
                error_object = 3
                others       = 4.

      if sy-subrc eq 0.
        clear it_ztpp_pmt07gb.
        move c_plnt  to    it_ztpp_pmt07gb-plnt. "PLANT
        concatenate it_ztpp_wosum-nation  it_ztpp_wosum-dealer
               into it_ztpp_pmt07gb-dist. "DIST(NATION+DEALER)

*------> Exterior Color , Interior Color
        move : it_ztpp_wosum-extc     to   it_ztpp_pmt07gb-extc,
               it_ztpp_wosum-intc     to   it_ztpp_pmt07gb-intc.
        perform  wocl_to_pmt07gb
                 using : ' ' 'P_MI'      it_ztpp_pmt07gb-mo01,  "MODEL
                         ' ' 'P_WO_SER'  it_ztpp_pmt07gb-ordr,  "WORDER
                         ' ' 'P_MI'      it_ztpp_pmt07gb-bmdl,  "MI
                         ' ' 'P_OCN'     it_ztpp_pmt07gb-ocnn,  "OCN
                         ' ' 'P_VERSION' it_ztpp_pmt07gb-vers.  "VERSION

        read table it_ztpp_pmt07ob
                       with key plnt = c_plnt   "M
                                modl = it_ztpp_pmt07gb-mo01.
        if sy-subrc eq 0.
*------>    UNIQUE PART SERIAL FROM P_WOHD
          perform part_value  using 'WOHD'.
*------>    COLOR PART SERIAL FROM P_WOCL
          perform part_value  using 'WOCL'.
          move : sy-uname   to  it_ztpp_pmt07gb-zuser,
                 sy-datum   to  it_ztpp_pmt07gb-zsdat,
                 sy-uzeit   to  it_ztpp_pmt07gb-zstim.
          append it_ztpp_pmt07gb.
          clear it_ztpp_pmt07gb.

        else.
          read table it_ztpp_pmt07ob
                         with key plnt = c_plnt "M
                                  modl = '*'.
          if sy-subrc eq 0.
*------>    UNIQUE PART SERIAL FROM P_WOHD
            perform part_value  using 'WOHD'.
*------>    COLOR PART SERIAL FROM P_WOCL
            perform part_value  using 'WOCL'.
            move : sy-uname   to  it_ztpp_pmt07gb-zuser,
                   sy-datum   to  it_ztpp_pmt07gb-zsdat,
                   sy-uzeit   to  it_ztpp_pmt07gb-zstim.
            append it_ztpp_pmt07gb.
            clear it_ztpp_pmt07gb.
          else.
**------>    UNIQUE PART SERIAL FROM P_WOHD WITHOUT ZTPP_PMT07OB
*          PERFORM PART_VALUE_WITHOUT USING 'WOHD'.
**------>    COLOR PART SERIAL FROM P_WOCL WITHOUT ZTPP_PMT07OB
*          PERFORM PART_VALUE_WITHOUT USING 'WOCL'.
            wa_error_ix = wa_error_ix + 1.
            it_error-wohd = l_matnr_hd.
            it_error-extc = it_ztpp_wosum-extc.
            it_error-intc = it_ztpp_wosum-intc.
            it_error-type = 'E'.
            it_error-zmsg = text-302.
            append it_error.
          endif.
        endif.
      endif.
    endif.
  endloop.

endform.                    " APPEND_PMT07GB
*&---------------------------------------------------------------------*
*&      Form  DETAILED_ERROR
*&---------------------------------------------------------------------*
form detailed_error.
  data : l_tabix    type   sy-tabix.
  loop at it_error.
    at first.
      format intensified off.
      write:/ '********* BEGIN of Detailed Error List **********'.
    endat.
    l_tabix = sy-tabix mod 2.
    if l_tabix eq 0.
      format intensified on.
    else.
      format intensified off.
    endif.
    write:/ it_error-wohd color col_key,
           it_error-extc color col_key,
           it_error-intc color col_key,
           it_error-type color col_negative,
           it_error-zmsg color col_normal.
    at last.
      format intensified off.
      write:/ '*********  END of Detailed Error List  **********'.
    endat.
  endloop.
endform.                    " DETAILED_ERROR
*&---------------------------------------------------------------------*
*&      Form  START_TIME
*&---------------------------------------------------------------------*
form start_time.
  write :/ '********** BEGIN OF PROCESS ***********'.
  write at: /001(030) 'Processing Time...(Start)' ,
             031(010) sy-datum                    ,
             042(010) sy-uzeit                    .
  write :/ 'ALC and HPCS Code (7GB)'.
  write :/ '********** BEGIN OF PROCESS ***********'.
  skip 2.
endform.                    " START_TIME
*&---------------------------------------------------------------------*
*&      Form  END_TIME
*&---------------------------------------------------------------------*
form end_time.
  skip 2.
  write :/ '********** END OF PROCESS ***********'.
  write at: /001(030) 'Processing Time...(End)' ,
           031(010) sy-datum                    ,
           042(010) sy-uzeit                    .
  write :/ '********** END OF PROCESS ***********'.
endform.                    " END_TIME
*&---------------------------------------------------------------------*
*&      Form  append_pmt07gb_n
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form append_pmt07gb_n.

  data : l_matnr_hd like   mara-matnr,
         $flag(1).

*-----> WORK ORDER SUMMARY TABLE (ZTPP_WOSUM)

  sort it_ztpp_pmt07ob by plnt modl.

  __cls : it_wosum_n, it_error, l_ausp, l_auspl .

  select wo_ser nation dealer extc intc
  into corresponding fields of table it_wosum_n
    from ztpp_wosum as za
   where za~modqty > 0
     and za~modqty > za~rp09tq .

  data: begin of l_object occurs 0,
          objek like ausp-objek,
        end of l_object.

  data: begin of l_objectl occurs 0,
          objek like ausp-objek,
        end of l_objectl.

  data $ix like sy-tabix.

  loop at it_wosum_n.
    $ix = sy-tabix.
    concatenate it_wosum_n-wo_ser it_wosum_n-nation it_wosum_n-dealer
                it_wosum_n-extc it_wosum_n-intc into it_wosum_n-wa_wocl.
    l_object-objek = it_wosum_n-$wa_wocl = it_wosum_n-wa_wocl(14).
    l_objectl-objek = it_wosum_n-wa_wocl.
    modify it_wosum_n index $ix transporting $wa_wocl wa_wocl.
    append : l_object, l_objectl.
  endloop.

  sort : l_object, l_objectl.
  delete adjacent duplicates from : l_object, l_objectl.

  if not l_object[] is initial.
    select a~objek
           b~atnam
           a~atwrt
    into table l_ausp
      from ausp as a
      join cabn as b
     on b~atinn eq a~atinn
     for all entries in l_object
     where a~objek = l_object-objek
       and a~klart = '001'
      %_hints oracle 'FIRST_ROWS(10)'.
  endif.

  if not l_objectl[] is initial.
    select a~objek
           b~atnam
           a~atwrt
    into table l_auspl
      from ausp as a
      join cabn as b
     on b~atinn eq a~atinn
     for all entries in l_objectl
     where a~objek = l_objectl-objek
       and a~klart = '001'
      %_hints oracle 'FIRST_ROWS(10)'.
  endif.

  describe table it_wosum_n lines wa_wosum_ix.

  sort l_ausp by objek atnam.
  sort l_auspl by objek atnam.

  sort it_wosum_n.

  loop at it_wosum_n.
    clear l_ausp.
    read table l_ausp with key objek = it_wosum_n-$wa_wocl
                               atnam = 'P_PERF_YN' binary search.
    if l_ausp-atwrt ne 'Y'.
      wa_error_ix = wa_error_ix + 1.
      it_error-wohd = l_matnr_hd.
      it_error-extc = it_wosum_n-extc.
      it_error-intc = it_wosum_n-intc.
      it_error-type = 'E'.
      it_error-zmsg = text-301.
      append it_error.
      continue.
    endif.

    clear it_ztpp_pmt07gb.
    move c_plnt  to    it_ztpp_pmt07gb-plnt. "PLANT

    concatenate it_wosum_n-nation  it_wosum_n-dealer
           into it_ztpp_pmt07gb-dist. "DIST(NATION+DEALER)

*------> Exterior Color , Interior Color
    move : it_wosum_n-extc     to   it_ztpp_pmt07gb-extc,
           it_wosum_n-intc     to   it_ztpp_pmt07gb-intc.

    perform  wocl_to_pmt07gb_n
             using : ' ' 'P_MI'      it_ztpp_pmt07gb-mo01,  "MODEL
                     ' ' 'P_WO_SER'  it_ztpp_pmt07gb-ordr,  "WORDER
                     ' ' 'P_MI'      it_ztpp_pmt07gb-bmdl,  "MI
                     ' ' 'P_OCN'     it_ztpp_pmt07gb-ocnn,  "OCN
                     ' ' 'P_VERSION' it_ztpp_pmt07gb-vers.  "VERSION

    read table it_ztpp_pmt07ob
                   with key plnt = c_plnt   "M
                            modl = it_ztpp_pmt07gb-mo01
                            binary search.
    if sy-subrc eq 0.
*------>    UNIQUE PART SERIAL FROM P_WOHD
      perform part_value_n  using 'WOHD'.
*------>    COLOR PART SERIAL FROM P_WOCL
      perform part_value_n  using 'WOCL'.
      move : sy-uname   to  it_ztpp_pmt07gb-zuser,
             sy-datum   to  it_ztpp_pmt07gb-zsdat,
             sy-uzeit   to  it_ztpp_pmt07gb-zstim.
      append it_ztpp_pmt07gb.
      clear it_ztpp_pmt07gb.

    else.
      read table it_ztpp_pmt07ob
                     with key plnt = c_plnt "M
                              modl = '*' binary search.
      if sy-subrc eq 0.
*------>    UNIQUE PART SERIAL FROM P_WOHD
        perform part_value_n  using 'WOHD'.
*------>    COLOR PART SERIAL FROM P_WOCL
        perform part_value_n  using 'WOCL'.
        move : sy-uname   to  it_ztpp_pmt07gb-zuser,
               sy-datum   to  it_ztpp_pmt07gb-zsdat,
               sy-uzeit   to  it_ztpp_pmt07gb-zstim.
        append it_ztpp_pmt07gb.
        clear it_ztpp_pmt07gb.
      else.
**------>    UNIQUE PART SERIAL FROM P_WOHD WITHOUT ZTPP_PMT07OB
**------>    COLOR PART SERIAL FROM P_WOCL WITHOUT ZTPP_PMT07OB
        wa_error_ix = wa_error_ix + 1.
        it_error-wohd = l_matnr_hd.
        it_error-extc = it_wosum_n-extc.
        it_error-intc = it_wosum_n-intc.
        it_error-type = 'E'.
        it_error-zmsg = text-302.
        append it_error.
      endif.
    endif.

  endloop.

endform.                    " append_pmt07gb_n
*&---------------------------------------------------------------------*
*&      Form  wocl_to_pmt07gb_n
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1515   text
*      -->P_1516   text
*      -->P_IT_ZTPP_PMT07GB_MO01  text
*----------------------------------------------------------------------*
form wocl_to_pmt07gb_n using p_ind p_atnam  p_field.
  case p_ind.
    when 'ALC'.
      data : l_fname(40).
      field-symbols: <fs_pmt07gb>.
      clear l_auspl.
      read table l_auspl with key objek = it_wosum_n-wa_wocl
                                  atnam = p_atnam binary search.
      if sy-subrc eq 0.
        concatenate 'IT_ZTPP_PMT07GB' '-' p_field into l_fname.
        assign (l_fname)    to  <fs_pmt07gb>.
        move l_auspl-atwrt  to  <fs_pmt07gb>.
      endif.
    when others.
      clear l_auspl.
      read table l_auspl with key objek = it_wosum_n-wa_wocl
                                  atnam = p_atnam binary search.

      if sy-subrc eq 0.
        move l_auspl-atwrt  to  p_field    .
      endif.
  endcase.
endform.                    " WOCL_TO_PMT07GB
*&---------------------------------------------------------------------*
*&      Form  part_value_n
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1567   text
*----------------------------------------------------------------------*
form part_value_n using   p_ind.

  data : l_index_2(2)   type   n,
         l_index_1(1)   type   n,
         l_index(2)     type   n.
  data : l_fname_pmt07ob(40),
         l_fname_pmt07ob1(40),
         l_fname_pmt07gb(40),
         l_field_wo(40),
         l_field_pmt07gb(40).
  data : l_field_1a(10),  "PMT07OB UPxx, CPxx FIELD
         l_field_1b(10),  "PMT07GB USERxx, CSERxx FIELD
         l_field_2a(10),  "WOHD, WOCL Charicteristic name
         l_field_2b(10),  "PMT07GB UVALxx, CVALxx FIELD
         l_val1(1)     type    n,
         l_val2(2)     type    n,
         l_val3(3)     type    n.

  field-symbols : <fs_pmt07ob>,
                  <fs_pmt07gb>.

  case p_ind.
    when 'WOHD'.
      move : 'UP'        to   l_field_1a,
             'USER'      to   l_field_1b,
             'P_ALC_U_'  to   l_field_2a,
             'UVAL'      to   l_field_2b.
    when 'WOCL'.
      move : 'CP'        to   l_field_1a,
             'CSER'      to   l_field_1b,
             'P_ALC_C_'  to   l_field_2a,
             'CVAL'      to   l_field_2b.
  endcase.

  clear l_index.
  do.
    l_index_2 = sy-index.
    concatenate 'IT_ZTPP_PMT07OB' '-' l_field_1a l_index_2
                 into l_fname_pmt07ob.
    concatenate 'IT_ZTPP_PMT07GB' '-' l_field_1b l_index_2
                 into l_fname_pmt07gb.
    assign (l_fname_pmt07ob)  to <fs_pmt07ob>.
    assign (l_fname_pmt07gb)  to <fs_pmt07gb>.
    if <fs_pmt07ob> le 0.
      exit.
    else.
      if l_index eq '20' and p_ind eq 'WOHD'.
        exit.
      elseif l_index eq '10' and p_ind eq 'WOCL'.
        exit.
      endif.
      <fs_pmt07gb> = <fs_pmt07ob>.
      if <fs_pmt07gb> le '009'.
        l_val1 = <fs_pmt07gb>.
        concatenate l_field_2a l_val1 into l_field_wo.
      elseif <fs_pmt07gb> ge '100'.
        l_val3 = <fs_pmt07gb>.
        concatenate l_field_2a l_val3 into l_field_wo.
      else.
        l_val2 = <fs_pmt07gb>.
        concatenate l_field_2a l_val2 into l_field_wo.
      endif.
      concatenate l_field_2b l_index_2  into  l_field_pmt07gb.

      case p_ind.
        when 'WOHD'.
          perform wohd_to_pmt07gb_n
                         using l_field_wo l_field_pmt07gb.
          if not l_ausp-atwrt is initial.
            l_index = l_index + 1.
          endif.
        when 'WOCL'.
          perform wocl_to_pmt07gb_n
                         using 'ALC' l_field_wo l_field_pmt07gb.
          if not l_auspl-atwrt is initial.
            l_index = l_index + 1.
          endif.
      endcase.
    endif.
  enddo.
endform.                    " UNIQUE_PART_VALUE
