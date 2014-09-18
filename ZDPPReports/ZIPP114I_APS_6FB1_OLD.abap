************************************************************************
* Program Name           : ZIPP114I_APS_6FB1
* Author                 : Kim Gil Hyun(Tonkey)
* Creation Date          : 2003.12.24.
* Specifications By      : Bobby, Choi
* Pattern           : 1.1
* Development Request No :
* Addl Documentation: Reference to "ZIPP101U_PMT07JB_A"
* Description       : Plan Quantity Per ALC Code.
*----------------------------------------------------------------------
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
report  zipp114i_aps_6fb1 message-id zmpp no standard page heading .

tables: ztpp_pmt06fb,    "Plan Quantity Per ALC Code
        ztpp_pmt06gb,    "Sequence Plan per WorkOrder(PMM06GB)
        ztpp_wosum2,     "Work Order Summary Monthly
        ztpp_pmt07gb,    "ALC and HPCS Code (PP->APS)
        ztpp_pmt07jb_a,  "SUMMARIZED PMT07JB
        equi,
        cabn,
        ausp.            "Characteristic Values
* Internal Table For 'ZTPP_PMT06FB' Data Creation.
data: begin of it_pmt06fb occurs 0.
        include structure ztpp_pmt06fb.
data:   equnr type equi-equnr,
      end of it_pmt06fb .

* Internal Table For reading 'ZTPP_PMT06GB'.
data: it_pmt06gb like table of ztpp_pmt06gb with header line .

data: begin of it_equip occurs 0,
        equnr type ausp-objek,
        wo_no type mara-matnr,
      end of it_equip .

************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
selection-screen: begin of block b1 with frame .
selection-screen begin of line.
parameters: p_run          type c as checkbox  default 'X'.
selection-screen comment  (75) text-001 for field p_run.
selection-screen end of line.
selection-screen: end of block b1.

*********************************************
initialization.
*********************************************

*********************************************
start-of-selection.
*********************************************
  check p_run = 'X'  .
  delete from ztpp_pmt06fb where plnt <> space.
  perform read_ztpp_pmt06gb .
  perform make_data_of_ztpp_pmt06fb .
  perform insert_ztpp_pmt06fb .


*********************************************
end-of-selection.
*********************************************


*&---------------------------------------------------------------------*
*&      Form  read_ztpp_pmt06gb
*&---------------------------------------------------------------------*
*       Searching Data From ZTPP_PMT06GB
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_ztpp_pmt06gb.
  select *
    into corresponding fields of table it_pmt06gb
    from ztpp_pmt06gb
    where gubb =    'S' .

endform.                    " read_ztpp_pmt06gb

*&---------------------------------------------------------------------*
*&      Form  make_data_of_ztpp_pmt06fb
*&---------------------------------------------------------------------*
*       Setting Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_data_of_ztpp_pmt06fb.
  data: l_woc_no type mara-matnr ,
        l_num03(03) type n .
  data: l_it_wo like table of zspp_vin_value with header line .
  clear: it_pmt06fb, it_pmt06fb[].
  loop at it_pmt06gb .
    clear it_pmt06fb .
*   Plant, Line, Car Type, Confirm Indicator, PBS QTY,
*   Paint Reject QTY, WBS QTY, Ds's QTY.
    move-corresponding it_pmt06gb to it_pmt06fb .
*   Trim Input QTY .
    concatenate it_pmt06gb-ordr
                it_pmt06gb-extc
                it_pmt06gb-intc
      into l_woc_no .
    clear it_pmt06fb-trim_qty .
    perform search_trim_qty using    l_woc_no
                            changing it_pmt06fb-trim_qty .
    if it_pmt06gb-lcgu = 'O'.
*   Planned QTY including LC .
      it_pmt06fb-lcok01 = it_pmt06gb-bukt01 + it_pmt06gb-bukt02 +
                          it_pmt06gb-bukt03 + it_pmt06gb-bukt04 +
                          it_pmt06gb-bukt05 + it_pmt06gb-bukt06 +
                          it_pmt06gb-bukt07 + it_pmt06gb-bukt08 +
                          it_pmt06gb-bukt09 + it_pmt06gb-bukt10 +
                          it_pmt06gb-bukt11 + it_pmt06gb-bukt12 +
                          it_pmt06gb-bukt13 + it_pmt06gb-bukt14 +
                          it_pmt06gb-bukt15 + it_pmt06gb-bukt16 +
                          it_pmt06gb-bukt17 + it_pmt06gb-bukt18 +
                          it_pmt06gb-bukt19 + it_pmt06gb-bukt20 +
                          it_pmt06gb-bukt21 + it_pmt06gb-bukt22 +
                          it_pmt06gb-bukt23 + it_pmt06gb-bukt24 +
                          it_pmt06gb-bukt25 + it_pmt06gb-bukt26 +
                          it_pmt06gb-bukt27 + it_pmt06gb-bukt28 +
                          it_pmt06gb-bukt29 + it_pmt06gb-bukt30 +
                          it_pmt06gb-bukt31 .
*   Extra QTY including LC .
      it_pmt06fb-lcok02 = it_pmt06gb-remq .
    else.  "it_pmt06gb-lcgu = 'X' .
*   Planned QTY excluding LC .
      it_pmt06fb-lcng01 = it_pmt06gb-bukt01 + it_pmt06gb-bukt02 +
                          it_pmt06gb-bukt03 + it_pmt06gb-bukt04 +
                          it_pmt06gb-bukt05 + it_pmt06gb-bukt06 +
                          it_pmt06gb-bukt07 + it_pmt06gb-bukt08 +
                          it_pmt06gb-bukt09 + it_pmt06gb-bukt10 +
                          it_pmt06gb-bukt11 + it_pmt06gb-bukt12 +
                          it_pmt06gb-bukt13 + it_pmt06gb-bukt14 +
                          it_pmt06gb-bukt15 + it_pmt06gb-bukt16 +
                          it_pmt06gb-bukt17 + it_pmt06gb-bukt18 +
                          it_pmt06gb-bukt19 + it_pmt06gb-bukt20 +
                          it_pmt06gb-bukt21 + it_pmt06gb-bukt22 +
                          it_pmt06gb-bukt23 + it_pmt06gb-bukt24 +
                          it_pmt06gb-bukt25 + it_pmt06gb-bukt26 +
                          it_pmt06gb-bukt27 + it_pmt06gb-bukt28 +
                          it_pmt06gb-bukt29 + it_pmt06gb-bukt30 +
                          it_pmt06gb-bukt31 .
*   Extra QTY excluding LC .
      it_pmt06fb-lcng02 = it_pmt06gb-remq .
    endif.
**  Code Column, ALC or HPC's Code of Work Order Color
    clear: l_it_wo, l_it_wo[] .

    perform make_classification_data tables l_it_wo
                                     using  l_woc_no.

*   Work Order Color's Data
    loop at l_it_wo where atnam+00(05) = 'P_ALC' or
                          atnam+00(08) = 'P_WO_HPC' .
      clear: it_pmt06fb-colm, it_pmt06fb-code .
      if l_it_wo-atnam+00(05) = 'P_ALC' .
        " For ALC .
        it_pmt06fb-gubb = 'A'.
        if l_it_wo-atwrt <> space.
          move l_it_wo-atnam+08 to l_num03 .
          concatenate l_it_wo-atnam+06(01)
                      l_num03
*   Code Column .
            into it_pmt06fb-colm .
*   ALC Code
          move l_it_wo-atwrt to it_pmt06fb-code .
          move sy-datum      to it_pmt06fb-zdate .
          move sy-uname      to it_pmt06fb-user_01 .
*
          append it_pmt06fb .
*
        else.
          continue .
        endif.
      else.
        " For HPC .
        it_pmt06fb-gubb = 'H'.
        if l_it_wo-atwrt <> space.
*   Code Column .
          move l_it_wo-atnam+09(04) to it_pmt06fb-colm .
*   HPC Code
          move l_it_wo-atwrt to it_pmt06fb-code .
          move sy-datum      to it_pmt06fb-zdate .
          move sy-uname      to it_pmt06fb-user_01 .
*
          append it_pmt06fb .
*
        else.
          continue .
        endif.
      endif.
    endloop.

**  Code Column, ALC or HPC's Code of Work Order Header
    clear: l_it_wo, l_it_wo[] .
    move it_pmt06gb-ordr to l_woc_no .
    perform make_classification_data tables l_it_wo
                                     using  l_woc_no .
*   Work Order Header's Data
    loop at l_it_wo where atnam+00(05) = 'P_ALC' or
                          atnam+00(08) = 'P_WO_HPC' .
      clear: it_pmt06fb-colm, it_pmt06fb-code .
      if l_it_wo-atnam+00(05) = 'P_ALC' .
        " For ALC .
        it_pmt06fb-gubb = 'A'.
        if l_it_wo-atwrt <> space.
          move l_it_wo-atnam+08 to l_num03 .
          concatenate l_it_wo-atnam+06(01)
                      l_num03
*   Code Column .
            into it_pmt06fb-colm .
*   ALC Code
          move l_it_wo-atwrt to it_pmt06fb-code .
          move sy-datum      to it_pmt06fb-zdate .
          move sy-uname      to it_pmt06fb-user_01 .
*
          append it_pmt06fb .
*
        else.
          continue .
        endif.
      else.
        " For HPC .
        it_pmt06fb-gubb = 'H'.
        if l_it_wo-atwrt <> space.
*   Code Column .
          move l_it_wo-atnam+09(04) to it_pmt06fb-colm .
*   HPC Code
          move l_it_wo-atwrt to it_pmt06fb-code .
          move sy-datum      to it_pmt06fb-zdate .
          move sy-uname      to it_pmt06fb-user_01 .
*
          append it_pmt06fb .
*
        else.
          continue .
        endif.
      endif.
    endloop.
  endloop.
endform.                    " make_data_of_ztpp_pmt06fb
*&---------------------------------------------------------------------*
*&      Form  search_trim_qty
*&---------------------------------------------------------------------*
*       Getting a Trim Quantity
*----------------------------------------------------------------------*
*      -->P_L_WOC_NO  text
*      <--P_IT_PMT06FB_TRIM_QTY  text
*----------------------------------------------------------------------*
form search_trim_qty using    p_woc_no
                     changing p_trim_qty.
  data: l_rp06cq   type ztpp_wosum2-rp06cq,
        l_rp07cq   type ztpp_wosum2-rp07cq.
  data: l_wo_ser(09), l_nation(03),
        l_dealer(02),
        l_extc(03), l_intc(03).
  move p_woc_no+00(09) to l_wo_ser.
  move p_woc_no+09(03) to l_nation.
  move p_woc_no+12(02) to l_dealer.
  move p_woc_no+14(02) to l_extc .
  move p_woc_no+16(02) to l_intc .
  select single rp06cq rp07cq
    into (l_rp06cq, l_rp07cq)
    from ztpp_wosum2
    where wo_ser = l_wo_ser and
          nation = l_nation and
          dealer = l_dealer and
          extc   = l_extc   and
          intc   = l_intc   and
          cr_date = ( select max( cr_date )
                        from ztpp_wosum2
                        where wo_ser = l_wo_ser and
                              nation = l_nation and
                              dealer = l_dealer and
                              extc   = l_extc   and
                              intc   = l_intc   ).
  p_trim_qty = l_rp06cq + l_rp07cq .

endform.                    " search_trim_qty
*&---------------------------------------------------------------------*
*&      Form  make_classification_data
*&---------------------------------------------------------------------*
*       Calling a Function For W/O's Information
*----------------------------------------------------------------------*
*      -->P_L_WOC_NO  text
*      -->P_LI_DATA  text
*----------------------------------------------------------------------*
form make_classification_data tables   p_it_data
                                         structure zspp_vin_value
                              using    p_woc_no          .

  call function 'Z_FPP_HANDLING_MASTER'
    exporting
      object             = p_woc_no
*     MODE               = 'R'
      ctype              = '001'
*     DISPLAY            = 'D'
    tables
      val_table          = p_it_data
    exceptions
      no_data            = 1
      error_mode         = 2
      error_object       = 3
    others               = 4
    .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

endform.                    " make_classification_data
*&---------------------------------------------------------------------*
*&      Form  insert_ztpp_pmt06fb
*&---------------------------------------------------------------------*
*       Insert New Data Into ZTPP_PMT06FB
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form insert_ztpp_pmt06fb.
  data: l_it_pmt06fb like table of ztpp_pmt06fb with header line .
  data: l_count type i,
        l_grup  type ztpp_pmt06fb-grup ,
        l_bukt21 type d,
        l_text(60) type c,
        l_int type i .

  sort it_pmt06fb by gubb plnt line modl plgu colm code .
  loop at it_pmt06fb .
    at new code.
      clear: l_it_pmt06fb .
      sum.
      move-corresponding it_pmt06fb to l_it_pmt06fb.
      move sy-datum      to l_it_pmt06fb-zdate .
      move sy-uname      to l_it_pmt06fb-user_01 .
      clear: l_it_pmt06fb-grup ,
             l_it_pmt06fb-zuser  , l_it_pmt06fb-zsdat,
             l_it_pmt06fb-zstim  , l_it_pmt06fb-zedat,
             l_it_pmt06fb-zetim  , l_it_pmt06fb-zmode,
             l_it_pmt06fb-zresult, l_it_pmt06fb-zmsg.
      append l_it_pmt06fb.
      l_count = l_count + 1 .
    endat.
************************************************************
*    l_bukt21 = l_bukt21 + it_pmt06fb-bukt21 .
*      WRITE:/ l_bukt21, it_pmt06fb-plgu,
*              it_pmt06fb-code, it_pmt06fb-grup ,
*              it_pmt06fb-bukt22 .
*
*    AT END OF code.
*      WRITE:/ l_bukt21 ,
*              l_it_pmt06fb-code, l_it_pmt06fb-grup .
*      CLEAR l_bukt21.
*    ENDAT.
************************************************************
  endloop.

  insert ztpp_pmt06fb from table l_it_pmt06fb.
  if sy-subrc <> 0.
    rollback work .
    if sy-batch = 'X'.
      message w001 with text-102.
    endif.
  else.
    describe table l_it_pmt06fb lines l_int.
    write l_int to l_text left-justified.
    concatenate 'Created Record Count :' l_text
      into l_text .
    commit work .
    message s001 with l_text .
    if sy-batch = 'X'.
      message s001 with text-101.
    endif.
  endif.

endform.                    " insert_ztpp_pmt06fb
