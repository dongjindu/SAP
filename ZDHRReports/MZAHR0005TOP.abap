*&---------------------------------------------------------------------*
*& Include MZAHR0005TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  SAPMZAHR0005 MESSAGE-ID ZMHR.

*... tables
  tables: zthr_pcp01,          " Personal Cost Plan - module, group
          zthr_pcp02.          " Personal Cost Plan - code

  controls: tc9000 type tableview using screen 9000,
            tc9100 type tableview using screen 9100.

*... internal tables
  data: begin of it_pcp01 occurs 0.
        include structure zthr_pcp01.
  data: chkbx,
        tabix    like sy-tabix,
        end of it_pcp01.

  data: del_pcp01 like zthr_pcp01 occurs 0 with header line.

  data: begin of it_pcp02 occurs 0.
        include structure zthr_pcp02.
  data: chkbx,
        tabix    like sy-tabix,
        end of it_pcp02.

  data: del_pcp02 like zthr_pcp02 occurs 0 with header line.

  data: begin of it_stats occurs 1,
        fcode    like rsmpe-func,
        end of it_stats.

  data: begin of it_moval occurs 1,
        zmodl    like zthr_pcp01-zmodl,
        zmtxt    like zthr_pcp01-zmtxt.
  data: end of it_moval.

  data: begin of it_grval occurs 1,
        zgrup    like zthr_pcp01-zgrup,
        zgtxt    like zthr_pcp01-zgtxt.
  data: end of it_grval.

  data: it_field     like help_value occurs 1 with header line,
        dynpfields   like standard table of dynpread with header line.

*... variants
  data: w_text1(50),
        w_text2(50),
        w_zmodl      like zthr_pcp01-zmodl,
        w_zmtxt      like zthr_pcp01-zmtxt,
        w_zgrup      like zthr_pcp01-zgrup,
        w_zgtxt      like zthr_pcp01-zgtxt.

  data: w_count      like sy-tabix,
        w_modes(1)   type n.

  data: w_fname      like  help_info-fieldname,
        w_tabix      like  sy-tabix,
        w_fldvl      like  help_info-fldvalue.

  data: w_field(20).
  data: w_tc_index type i.
