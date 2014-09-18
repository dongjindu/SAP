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
* 8/23/07    IG.MOON      UD1K941473   Performance Tuning
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
*data: it_pmt06gb like table of ztpp_pmt06gb with header line .

data: begin of it_equip occurs 0,
        equnr type ausp-objek,
        wo_no type mara-matnr,
      end of it_equip .

* UD1K941473 by IG.MOON Performance Tuning {
define __cls.                          " clear & refresh
  clear &1.refresh &1.
end-of-definition.

types: begin of ty_pmt06gb.
types: $key(20).
include  structure ztpp_pmt06gb.
types: end of ty_pmt06gb.

data: it_pmt06gb  type table of ty_pmt06gb     with header line.

data: begin of it_wosum2  occurs 0,
  wo_ser	like ztpp_wosum2-wo_ser,	
  nation	like ztpp_wosum2-nation,	
  dealer	like ztpp_wosum2-dealer,	
  extc		like ztpp_wosum2-extc,		
  intc		like ztpp_wosum2-intc,		
  rp06cq	like ztpp_wosum2-rp06cq,
  rp07cq	like ztpp_wosum2-rp07cq,	
  cr_date	like ztpp_wosum2-cr_date,	
  $dist       like ztpp_pmt07jb_a-dist,
end of it_wosum2.

data :
    begin of l_ausp occurs 0,
        objek like ausp-objek,
        atnam like cabn-atnam,
        atwrt like ausp-atwrt,
    end of l_ausp,
    begin of l_auspl occurs 0,
        objek like ausp-objek,
        atnam like cabn-atnam,
        atwrt like ausp-atwrt,
    end of l_auspl.

field-symbols <s_qty>.

* }

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

*  delete from ztpp_pmt06fb where plnt <> space.
  perform read_ztpp_pmt06gb .


* UD1K941473 by IG.MOON 8/25/2007 {
  if '<run fast'>'!'.
    perform make_data_of_ztpp_pmt06fb_n .  " tunned
  else.
    perform make_data_of_ztpp_pmt06fb .    " old
  endif.
* }

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
form make_data_of_ztpp_pmt06fb_n.

  __cls : it_pmt06fb, l_ausp.

  perform make_wosum2.
  perform make_order_char.

  loop at it_pmt06gb .

    clear it_pmt06fb .
*   Plant, Line, Car Type, Confirm Indicator, PBS QTY,
*   Paint Reject QTY, WBS QTY, Ds's QTY.
    move-corresponding it_pmt06gb to it_pmt06fb .
*   Trim Input QTY .

    clear it_pmt06fb-trim_qty .

    read table it_wosum2 with key wo_ser = it_pmt06gb-ordr(9)
                                  nation = it_pmt06gb-ordr+9(3)
                                  dealer = it_pmt06gb-ordr+12(2)
                                  extc   = it_pmt06gb-extc
                                  intc   = it_pmt06gb-intc
                                  binary search.
    if sy-subrc eq 0.
      it_pmt06fb-trim_qty = it_wosum2-rp06cq + it_wosum2-rp07cq .
    endif.


    if it_pmt06gb-lcgu = 'O'.
*   Planned QTY including LC .
      perform sum_qty changing it_pmt06fb-lcok01.

*   Extra QTY including LC .
      it_pmt06fb-lcok02 = it_pmt06gb-remq .
    else.  "it_pmt06gb-lcgu = 'X' .
*   Planned QTY excluding LC .
      perform sum_qty changing it_pmt06fb-lcng01.

*   Extra QTY excluding LC .
      it_pmt06fb-lcng02 = it_pmt06gb-remq .
    endif.

**  Code Column, ALC or HPC's Code of Work Order Color
    perform append_table  tables l_auspl
                          using it_pmt06gb-$key.

**  Code Column, ALC or HPC's Code of Work Order Header
    perform append_table  tables l_ausp
                          using it_pmt06gb-ordr.

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

  delete from ztpp_pmt06fb where plnt <> space.

  data $flag(1).

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

  __cls it_pmt06fb.

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
*&---------------------------------------------------------------------*
*&      Form  make_wosum2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_wosum2.

  data : $ix like sy-tabix,
         $flag(1).

  __cls it_wosum2.

  select wo_ser nation dealer extc intc
         rp06cq rp07cq cr_date
    into corresponding fields of table it_wosum2
    from ztpp_wosum2
    for all entries in it_pmt06gb
   where wo_ser = it_pmt06gb-ordr(9)
     and nation = it_pmt06gb-ordr+9(3)
     and dealer = it_pmt06gb-ordr+12(2)
     and extc   = it_pmt06gb-extc
     and intc   = it_pmt06gb-intc .

  sort it_wosum2 by wo_ser nation dealer extc intc ascending
                    cr_date descending.


  loop at it_wosum2.
    $ix =  sy-tabix.
    at new intc.
      $flag = 'X'.
    endat.
    if $flag eq 'X'.
      concatenate it_wosum2-nation it_wosum2-dealer
        into  it_wosum2-$dist.
      modify it_wosum2 index $ix transporting $dist.
      clear $flag.
    else.
      delete it_wosum2 index $ix.
    endif.
  endloop.

  sort it_wosum2 by wo_ser nation dealer extc intc.

endform.                    " make_wosum2
*&---------------------------------------------------------------------*
*&      Form  append_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_AUSPL  text
*      -->P_IT_PMT06GB_$KEY  text
*----------------------------------------------------------------------*
form append_table tables   p_o_table structure l_ausp
                  using    p_object.


  data : $ix like sy-tabix,
         l_num03(03) type n .

  read table p_o_table with key objek =  p_object binary search.
  if sy-subrc eq 0.
    $ix = sy-tabix.
*   Work Order Color's Data

    loop at p_o_table from $ix.
      if p_o_table-objek ne p_object.
        exit.
      endif.
      clear: it_pmt06fb-colm, it_pmt06fb-code .
      if p_o_table-atnam(05) = 'P_ALC' .
        " For ALC .
        it_pmt06fb-gubb = 'A'.
        if p_o_table-atwrt <> space.
          move p_o_table-atnam+08 to l_num03 .
          concatenate p_o_table-atnam+06(01) l_num03
*   Code Column .
            into it_pmt06fb-colm .
*   ALC Code

          move p_o_table-atwrt to it_pmt06fb-code .
          move sy-datum     to it_pmt06fb-zdate .
          move sy-uname     to it_pmt06fb-user_01 .
          append it_pmt06fb .
        endif.
      else.
        " For HPC .
        it_pmt06fb-gubb = 'H'.
        if p_o_table-atwrt <> space.
*   Code Column .
          move p_o_table-atnam+09(04) to it_pmt06fb-colm .
*   HPC Code
          move p_o_table-atwrt to it_pmt06fb-code .
          move sy-datum      to it_pmt06fb-zdate .
          move sy-uname      to it_pmt06fb-user_01 .

          append it_pmt06fb .
        endif.
      endif.
    endloop.
  endif.


endform.                    " append_table
*&---------------------------------------------------------------------*
*&      Form  make_order_char
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_order_char.
  data $ix like sy-tabix.

  data: begin of l_object occurs 0,
          objek like ausp-objek,
        end of l_object.

  data: begin of l_objectl occurs 0,
          objek like ausp-objek,
        end of l_objectl.


  loop at it_pmt06gb.
    $ix = sy-tabix.
    concatenate it_pmt06gb-ordr
                it_pmt06gb-extc
                it_pmt06gb-intc into it_pmt06gb-$key .

    l_object-objek = it_pmt06gb-ordr.
    l_objectl-objek = it_pmt06gb-$key.

    modify it_pmt06gb index $ix transporting $key.
    append : l_object,l_objectl.
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
       and ( b~atnam like 'P_ALC%' or b~atnam like 'P_WO_HPC%' )
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
       and ( b~atnam like 'P_ALC%' or b~atnam like 'P_WO_HPC%' )
      %_hints oracle 'FIRST_ROWS(10)'.
  endif.

  sort : l_ausp  by objek atnam,
         l_auspl by objek atnam.

endform.                    " make_order_char
*&---------------------------------------------------------------------*
*&      Form  sum_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_PMT06FB_LCOK01  text
*----------------------------------------------------------------------*
form sum_qty changing p_qty.

  data : $field(20),
         $no(2) type n.

  clear p_qty.

  do 31 times.
    $no = sy-index.
    concatenate 'IT_PMT06GB-BUKT' $no into $field.
    assign ($field) to <s_qty>.
    add <s_qty> to p_qty .
  enddo.

endform.                    " sum_qty
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
