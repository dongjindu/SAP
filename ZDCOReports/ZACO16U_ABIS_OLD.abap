************************************************************************
* Program Name      : ZACO16U_ABIS
* Author            : Hyung Jin Youn
* Creation Date     : 04/11/2003
* Specifications By : Deok-Kie Lee
* Pattern           : Report 1-1
* Development Request No: UD1K903655
* Add documentation :
* Description       : Allocate the costs in Internal order- P001, E001
*                     to PCC by the rate of PCC cost -> Changed
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date       Developer       Request ID
* 16/04/2004 Hyung Jin Youn  UD1K909644
* Description
* Cost By Shop report requires the Additional Issue Data, so this
* program should be revised and changed to be fit to the change of
* Business Process - To PCC by the rate of PCC Quantity Base
* Date      Developer      Request            Description
* 06/06/06  Manju          UD1K920991         Rounding off changes
************************************************************************
report zaco16u_abis message-id zmco.

include : z_moon_alv_top,
          z_moon_alv_fnc.
include <icon>.                        " icon
* For TOP include
INCLUDE ZACO16L_1TOP_OLD.
*INCLUDE ZACO16L_1TOP_BACK.
*include zaco16l_1top.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
selection-screen begin of block bl1 with frame title text-001.
parameters : p_kokrs like csks-kokrs memory id cac obligatory,
             p_gjahr like coep-gjahr memory id gjr default sy-datum(4)
                                                   obligatory,
             p_perio like coep-perio memory id bpe obligatory,
             p_versn like coep-versn  obligatory default '000',
             p_wrttp like coep-wrttp  obligatory default '04' ,
             p_conf  as checkbox user-command pchkbox.
" Confirmation Ind.

parameters   p_test  as checkbox default 'X'.

select-options : s_mtart for t134-mtart memory id mta
                         obligatory
                         no intervals,
                 s_aufnr for aufk-aufnr memory id anr
                         obligatory
                         no intervals
                         matchcode object zsh_co_io,
                 s_kstar for cska-kstar memory id kat
                         obligatory
                         no intervals,
                 s_matnr for coep-matnr.

selection-screen end of block bl1.
selection-screen begin of block bl2 with frame title text-002.

parameters :
    p_report radiobutton group ra01 modif id zpa, " Report
    p_frpost radiobutton group ra01 modif id zpa. " Report + Save Result

parameters : p_post   as checkbox  modif id zpa. " Posting

selection-screen end of block bl2.

* Layout
selection-screen begin of block b4 with frame title text-01s.
parameter p_vari type slis_vari.
selection-screen end of block b4.

selection-screen begin of block view-result with frame title text-t03.
selection-screen pushbutton  1(24) vslt user-command vslt.
selection-screen end of block view-result.

*&----------------------------------------------------------------------

* For Sub-Routine
INCLUDE ZACO16L_F001_OLD.
*INCLUDE ZACO16L_F001_BACK.
*include zaco16l_f001.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
initialization.

**// Mod. By Hyung Jin Youn 2004.08.05
* Do not use Default Values
* KSTAR, I/O, Material Type
*  PERFORM SELECT_INIT.
**// End of Mod.
* Set Material Type for Semi-Finished product and FSC
  perform set_mtype_fk.

* 11/06/2007 by IG.MOON {
  perform default_.
* }
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
at selection-screen.
* information
  perform confirm_message.

at selection-screen output.
* Check Post+Report/Report
*  PERFORM IND_POST_N_REPORT.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
start-of-selection.
* Controlling Area Information
  perform read_tka01.
* Enqueue
*  PERFORM ENQUEUE.

  if p_post = space.

* Confirmation

* commentted by IG.MOON
    PERFORM CONF_REPLACE_DATA.

* KSTAR - ALPHA_NUMERIC
    perform alpha_kstar.
* Read I/O data from CO document
    perform read_io_data.
* Read Costs from B/F
    perform read_b_f_data.

*// Mod. by Hyung Jin Youn 2004.04.19
* Cal. ratio by Qty. Base not By Cost Base
* Cal. Total material cost and the ratio for parents materials,
*      The cost ratio of child materials
* PERFORM CAL_COST_RATIO.
    perform cal_cost_ratio_02.
    perform cal_qty_ratio_02.
*// End of Mod.

* Making itab to be posted
    perform make_pcc_post_table.
* Adjustment
    perform adjustment_data.

    clear p_frpost. " by IG.MOON
    if p_frpost = 'X'.
* Update result to CBO table
      perform update_data_to_table .
    endif.

  endif.

* Posting
  clear p_post. " by IG.MOON
  if p_post = 'X'.
    perform post_data.
  endif.
* Let system Dequeue


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
end-of-selection.
  loop at gt_return.
    write:/ gt_return-id,
            gt_return-number,
            gt_return-message_v1.

  endloop.
*&---------------------------------------------------------------------*
*&      Form  DEFAULT_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form default_.
  write:
          icon_biw_report_view as icon to vslt,
         'View saved data' to vslt+4(21).

endform.                    " DEFAULT_
