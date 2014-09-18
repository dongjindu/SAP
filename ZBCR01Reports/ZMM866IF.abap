REPORT ZMM866IF.

*&--------------------------------------------------------------------&*
*  Program: ZMM866IF.
*  Author: Shiva Gnanaguru
*  Specification: Extract the data from the tables ztpp_seq_sum01 and
*                 ztpp_seq_sum02 and send to vendor.
*&--------------------------------------------------------------------&*
*  Date         User Id  Transport    Description
* 05/16/2004    100471   UD1K910664  initial program.
* 01/18/2004    100471   UD1K913877  check for deletion indicator.
*&--------------------------------------------------------------------&*

include ZMM866TP.

parameters: p_fname like rlgrap-filename
                    default '/usr/sap/EDI_SAP/HMMA866/hmma866'.

select * from ztpp_seq_sum01
          into table it_seqsum1.

select * from ztpp_seq_sum02
          into table it_seqsum2.

describe table it_seqsum1 lines no_lines.
if no_lines eq 0.
  describe table it_seqsum2 lines no_lines.
  if no_lines eq 0.
    perform write_err_log using '1'.
    exit.
  endif.
endif.

select t1~matnr bismt lifnr
                    into table it_mat_vend
                    from mara as t1
                    inner join eina as t2
                    on t2~matnr = t1~matnr
                    and t2~loekz = t1~lvorm
                    where lvorm = space.

delete it_mat_vend where bismt is initial.
delete adjacent duplicates from it_mat_vend comparing matnr bismt.
describe table it_mat_vend lines no_lines.
if no_lines eq 0.
  perform write_err_log using '2'.
  exit.
endif.

sort: it_mat_vend by bismt,
      it_seqsum1 by alc_vals,
      it_seqsum2 by alc_vals.

concatenate p_fname sy-datum sy-uzeit into w_fname.
open dataset w_fname for output in text mode.
if sy-subrc ne 0.
  MESSAGE e999(zmmm) WITH 'File could not be open to write!'(001).
  exit.
endif.
loop at it_mat_vend into wa_mat_vend.
*&---- 2 Hour bucket .
  read table it_seqsum1 into wa_seqsum1
                       with key alc_vals = wa_mat_vend-bismt
                                          binary search.
  if sy-subrc eq 0.
    w_flag = 'X'.
    wa_866_info-matnr = wa_mat_vend-matnr.
    wa_866_info-lifnr = wa_mat_vend-lifnr.
    wa_866_info-ind_01 = 'H'.
    wa_866_info-alc_code_01 = wa_seqsum1-alc_code.
    wa_866_info-serial_01   = wa_seqsum1-serial.
    wa_866_info-rp_01       = wa_seqsum1-rp.
    wa_866_info-model_01    = wa_seqsum1-model.
    wa_866_info-alc_vals_01 = wa_seqsum1-alc_vals.
    wa_866_info-d_1_01      = wa_seqsum1-d_1.
    wa_866_info-seq_01      = wa_seqsum1-seq.
    wa_866_info-bodyin_01   = wa_seqsum1-bodyin.
    wa_866_info-wbs_01      = wa_seqsum1-wbs.
    wa_866_info-paint_01    = wa_seqsum1-paint.
    wa_866_info-prj_01      = wa_seqsum1-prj.
    wa_866_info-pbs_01      = wa_seqsum1-pbs.
    wa_866_info-mitu_01     = wa_seqsum1-mitu.
    wa_866_info-stot_01     = wa_seqsum1-stot.
    wa_866_info-fore_01     = wa_seqsum1-fore.
    move-corresponding wa_seqsum1 to wa_866_info.
  endif.
*&--------21 days.
  read table it_seqsum2 into wa_seqsum2
                       with key alc_vals = wa_mat_vend-bismt
                                          binary search.
  if sy-subrc eq 0.
    w_flag = 'X'.
    if wa_866_info-matnr is initial.
      wa_866_info-matnr = wa_mat_vend-matnr.
      wa_866_info-lifnr = wa_mat_vend-lifnr.
    endif.
    wa_866_info-ind_02 = 'D'.
    wa_866_info-alc_code_02 = wa_seqsum2-alc_code.
    wa_866_info-serial_02   = wa_seqsum2-serial.
    wa_866_info-rp_02       = wa_seqsum2-rp.
    wa_866_info-model_02    = wa_seqsum2-model.
    wa_866_info-alc_vals_02 = wa_seqsum2-alc_vals.
    wa_866_info-d_1_02      = wa_seqsum2-d_1.
    wa_866_info-seq_02      = wa_seqsum2-seq.
    wa_866_info-bodyin_02   = wa_seqsum2-bodyin.
    wa_866_info-wbs_02      = wa_seqsum2-wbs.
    wa_866_info-paint_02    = wa_seqsum2-paint.
    wa_866_info-prj_02      = wa_seqsum2-prj.
    wa_866_info-pbs_02      = wa_seqsum2-pbs.
    wa_866_info-mitu_02     = wa_seqsum2-mitu.
    wa_866_info-stot_02     = wa_seqsum2-stot.
    wa_866_info-fore_02     = wa_seqsum2-fore.
    move-corresponding wa_seqsum2 to wa_866_info.
  endif.
  perform format_866_info using wa_866_info changing wa_866_result.
  transfer wa_866_result to w_fname.
  clear: wa_866_info, wa_866_result.
endloop.

close dataset w_fname.

if w_flag = space.
else.
  message i999(zmmm) with 'EDI866 File transfered !'.
endif.
*&---------------------------------------------------------------------*
*&      Form  format_866_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_866_INFO  text
*      <--P_WA_866_RESULT  text
*----------------------------------------------------------------------*
FORM format_866_info USING    p_866_info structure wa_866_info
                     CHANGING p_866_result structure wa_866_result.

  move: p_866_info-lifnr    to p_866_result-lifnr,
        p_866_info-matnr    to p_866_result-matnr,
        p_866_info-ind_01    to p_866_result-ind_01,
        p_866_info-alc_code_01 to p_866_result-alc_vals_01,
        p_866_info-serial_01   to p_866_result-serial_01,
        p_866_info-rp_01       to p_866_result-rp_01,
        p_866_info-model_01    to p_866_result-model_01,
        p_866_info-alc_vals_01 to p_866_result-alc_code_01.
  unpack: p_866_info-d_1_01    to p_866_result-d_1_01,
          p_866_info-seq_01    to p_866_result-seq_01,
          p_866_info-bodyin_01 to p_866_result-bodyin_01,
          p_866_info-wbs_01    to p_866_result-wbs_01,
          p_866_info-paint_01  to p_866_result-paint_01,
          p_866_info-prj_01    to p_866_result-prj_01,
          p_866_info-pbs_01    to p_866_result-pbs_01,
          p_866_info-H02    to p_866_result-H02,
          p_866_info-H04    to p_866_result-H04,
          p_866_info-H06    to p_866_result-H06,
          p_866_info-H08    to p_866_result-H08,
          p_866_info-H10    to p_866_result-H10,
          p_866_info-H12    to p_866_result-H12,
          p_866_info-H14    to p_866_result-H14,
          p_866_info-H16    to p_866_result-H16,
          p_866_info-H18    to p_866_result-H18,
          p_866_info-H20    to p_866_result-H20,
          p_866_info-H22    to p_866_result-H22,
          p_866_info-H24    to p_866_result-H24,
          p_866_info-H26    to p_866_result-H26,
          p_866_info-H28    to p_866_result-H28,
          p_866_info-H30    to p_866_result-H30,
          p_866_info-H32    to p_866_result-H32,
          p_866_info-H34    to p_866_result-H34,
          p_866_info-H36    to p_866_result-H36,
          p_866_info-H38    to p_866_result-H38,
          p_866_info-H40    to p_866_result-H40,
          p_866_info-H42    to p_866_result-H42,
          p_866_info-H44    to p_866_result-H44,
          p_866_info-H46    to p_866_result-H46,
          p_866_info-H48    to p_866_result-H48,
          p_866_info-H50    to p_866_result-H50,
          p_866_info-H52    to p_866_result-H52,
          p_866_info-H54    to p_866_result-H54,
          p_866_info-H56    to p_866_result-H56,
          p_866_info-H58    to p_866_result-H58,
          p_866_info-H60    to p_866_result-H60,
          p_866_info-MITU_01   to p_866_result-MITU_01,
          p_866_info-STOT_01   to p_866_result-STOT_01,
          p_866_info-FORE_01   to p_866_result-FORE_01.

  move:   p_866_info-ind_02      to p_866_result-ind_02,
          p_866_info-alc_code_02 to p_866_result-alc_vals_02,
          p_866_info-serial_02   to p_866_result-serial_02,
          p_866_info-rp_02       to p_866_result-rp_02,
          p_866_info-model_02    to p_866_result-model_02,
          p_866_info-alc_vals_02 to p_866_result-alc_code_02.
  unpack: p_866_info-d_1_02    to p_866_result-d_1_02,
          p_866_info-seq_02    to p_866_result-seq_01,
          p_866_info-bodyin_02 to p_866_result-bodyin_01,
          p_866_info-wbs_02    to p_866_result-wbs_01,
          p_866_info-paint_02  to p_866_result-paint_01,
          p_866_info-prj_02    to p_866_result-prj_01,
          p_866_info-pbs_02    to p_866_result-pbs_01,
          p_866_info-D01    to p_866_result-D01,
          p_866_info-D02    to p_866_result-D02,
          p_866_info-D03    to p_866_result-D03,
          p_866_info-D04    to p_866_result-D04,
          p_866_info-D05    to p_866_result-D05,
          p_866_info-D06    to p_866_result-D06,
          p_866_info-D07    to p_866_result-D07,
          p_866_info-D08    to p_866_result-D08,
          p_866_info-D09    to p_866_result-D09,
          p_866_info-D10    to p_866_result-D10,
          p_866_info-D11    to p_866_result-D11,
          p_866_info-D12    to p_866_result-D12,
          p_866_info-D13    to p_866_result-D13,
          p_866_info-D14    to p_866_result-D14,
          p_866_info-D15    to p_866_result-D15,
          p_866_info-D16    to p_866_result-D16,
          p_866_info-D17    to p_866_result-D17,
          p_866_info-D18    to p_866_result-D18,
          p_866_info-D19    to p_866_result-D19,
          p_866_info-D20    to p_866_result-D20,
          p_866_info-D21    to p_866_result-D21,
          p_866_info-MITU_02   to p_866_result-MITU_02,
          p_866_info-STOT_02   to p_866_result-STOT_02,
          p_866_info-FORE_02   to p_866_result-FORE_02.

ENDFORM.                    " format_866_info
*&---------------------------------------------------------------------*
*&      Form  write_err_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0030   text
*----------------------------------------------------------------------*
FORM write_err_log USING p_ertyp.

  clear wa_866_log.
  case p_ertyp.
    when '1'.
      wa_866_log-result = 'No data in table ZTPP_SEQ_SUM01 & 02'.
    when '2'.
      wa_866_log-result = 'Vendor material Info record not available'.
  endcase.

  concatenate p_fname sy-datum sy-uzeit into w_fname.
  open dataset w_fname for output in text mode.
  transfer wa_866_log to w_fname.
  close dataset w_fname.

ENDFORM.                    " write_err_log
