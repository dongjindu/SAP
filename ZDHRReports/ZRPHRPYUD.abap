*----------------------------------------------------------------------*
*   INCLUDE RPHRPYUD
*----------------------------------------------------------------------*
* ERP2005
* =======
* SAM850091 06/01/2005  MSC: Payroll reconciliation shows incorrect
*                       values
*----------------------------------------------------------------------*
DATA: BEGIN OF s_itab OCCURS 30,       "Range of valid Eval Class
        sign(1),
        option(2),
        low  like t512w-aklas,
        high like t512w-aklas,
      end   of s_itab.

data: begin of tmp_itab occurs 30,     "Structure of Range Table
        sign(1),
        option(2),
        low(4),
        high(4),
      end   of tmp_itab.

data: begin of params.
        include structure pri_params.
data: end of params.

DATA: BEGIN OF ITRDT OCCURS 1.
        include structure v_5uxy_a.
data:   last_asofd like v_5uxy_a-asofd.
DATA:   LAST_GENTM LIKE V_5UXY_A-GENTM.
DATA:   LAST_GENDA LIKE V_5UXY_A-GENDA.
DATA: END OF ITRDT.

types: begin of tinper_directory_entry,
        iabkrs like pc261-iabkrs,
        iperm like pc261-iperm,
        inper like pc261-inper,
        inpty like pc261-inpty,
        inpid like pc261-inpid,
        ipend like pc261-ipend,
        rundt like pc261-rundt,
        runtm like pc261-runtm,
        ippay like pc261-paydt,
      end of tinper_directory_entry.
types: tinper_directory type tinper_directory_entry occurs 0.

data: ip_beg   like sy-datum,          "IN Period Begin Date
      ip_end   like sy-datum,          "IN Period End Date
      ip_mod   like pc261-iperm.       "IN Period Modifier

data: org_rgdir like pc261 occurs 1 with header line,
      evp like pc261 occurs 0 with header line,
      evp_number_lines type i,
      evp_cp like pc261 occurs 0 with header line,
      evp_ppadj like pc261 occurs 0 with header line,
      evp_cpadj like pc261 occurs 0 with header line,
      inper_directory type tinper_directory,
      inper_directory_entry type tinper_directory_entry.

data: pyarea         like t549a-abkrs, "Current Payroll Area
*<SAM850091>
*      pyprd          LIKE qppnp-pabrp, "Payroll Period
*      pyyr           LIKE qppnp-pabrj, "Payroll Year
*<SAM850091>
      begda          like qppnp-begda, "Period Begin Date
      endda          like qppnp-endda, "Period end Date
      co_code        like pc205-bukrs, "Current Company Code
      rp_molga       like t512w-molga, "User's Country
      pagno          like sy-pagno,    "Page number for ALV header
      line_number    like sy-linno,
      tx_imp_dt      like t511k-begda,
      retro_dt       like pnpbegda.

data: datkey(16)  type c.              "DATE KEY
data: select_mode type c,
      special_pay(1) TYPE c.
*<SAM850091>
*      ee_er_ind(2) type c.
*</SAM850091>

data: p_interval  like select_mode value '1',
      p_period    like select_mode value '2',
      p_special   like select_mode value '3'.

data: per_beg_str(10) type c,
      per_end_str(10) type c.

data: rfeld type p decimals 2,
      vargt(8),
      lgart_kl(2),
      char2(2),
      char4(4),
      pack type p,
      xfeld type x,
      xfeld2 type x.

data: q_beg like pc261-fpbeg,
      q_end like pc261-fpend,
      p_beg like pc261-fpbeg,
      p_end like pc261-fpend,
      P_PER LIKE PC261-FPPER,
      P_ASOP LIKE PC261-FPBEG,
      P_ASOC LIKE PC261-FPEND.

data: btype_flag,
      atype_flag.

data:   empl_name(30)  type c,         "Formated employee name
        repnm(35)      type c,         "Report name
        repid(30)      type c,         "Report ID
        date_id        type i,         "date identifier
        crep_name(35)  type c,         "Centered report name
        rpt_type(35)   type c,         "Report Type
        period(25)     type c,         "Period dates
        rp_date(10)    type c,         "System Date
        rp_time(8)     type c,         "System Time
        rp_dt_tm(19)   type c,         "System Date & Time
        rp_pypr(8)     type c,         "Pay period & Year
        rp_begda(10)   type c,         "Pay Period Begda
        rp_endda(10)   type c,         "Pay Period Endda
        rp_prd_dt(21)  type c,         "Pay Period Beg/End Dates
        slgart_lines   type i,         "Number of lines in s_lgart
        dtable_lines   TYPE i.         "Number of lines in data tables
*<SAM850091>
*        CONSIDER_OUTFLOW.
*</SAM850091>

data: begin of ee_table occurs 500,
         pernr like p0000-pernr,
         status(1) type c,
         lstrec like pc261-paydt,
      end of ee_table.

data: begin of swgtyp occurs 30,       "All valid Wage Types
            lgart like t512w-lgart,
            lgtxt like t512t-lgtxt,
            sumlg like t596i-sumlg,
            rechz like t596i-rechz,
            type(1) type c,
            eree(2),
*<SAM850091>
            adjtax TYPE xflag,
*</SAM850091>
      END OF swgtyp.

*<SAM850091>
*ranges: lgart for t596i-lgart.
*</SAM850091>

data: begin of p_range occurs 17.      "Period date ranges
        include structure range_prds.
data: end of p_range.

data: begin of i_range occurs 17.      "Selected Period date ranges
        include structure range_prds.
data: end of i_range.

data: begin of data_table occurs 0,
        pernr    like p0001-pernr,
        btype(1) type c,          "R=Retro P=PrPerAdj C=CurrPer N=Nrmal
        box      like t596i-sumlg,
        lgart    like rt-lgart,
        iabkrs   like pc261-iabkrs,
        inper    like pc261-inper,
        ipydt    like pc261-paydt,
        inpty    like pc261-inpty,
        inpid    like pc261-inpid,
        abkrs    like pc261-abkrs,
        fpper    like pc261-fpper,
        paydt    like pc261-paydt,
        payty    like pc261-payty,
        payid    like pc261-payid,
        bukrs    like pc205-bukrs,
        werks    like pc205-werks,
        btrtl    like pc205-btrtl,
        kostl    like pc205-kostl,
        persg    like pc205-persg,
        persk    like pc205-persk,
        vdsk1    like pc205-vdsk1,
        stat2    like pc205-stat2,
        txcmp    like t5utl-txcmp,
        taxau    like pc22t-taxau,
        taxty    like t5utt-taxty,
        anzhl    like pc207-anzhl,
        betrg    like pc207-betrg,
        perid_m  like range_prds-per_id,
        betrg_m  like pc207-betrg,
        anzhl_m  like pc207-anzhl,
        perid_q  like range_prds-per_id,
        betrg_q  like pc207-betrg,
        anzhl_q  like pc207-anzhl,
        perid_y  like range_prds-per_id,
        betrg_y  like pc207-betrg,
        anzhl_y  like pc207-anzhl,
        amt_curr like pc207-amt_curr,
        ATYPE(4) TYPE C,
        PRCL(1) TYPE C,
        sort_strng(100) type c,
     end of data_table.

data: data_table_write like data_table occurs 100 with header line.

data: begin of totals_table occurs 0,
        box like t596i-sumlg,
        iabkrs like pc261-iabkrs,
        bukrs like pc205-bukrs,
        werks like pc205-werks,
        btrtl like pc205-btrtl,
        kostl like pc205-kostl,
        persg like pc205-persg,
        persk like pc205-persk,
        vdsk1 like pc205-vdsk1,
        stat2 like pc205-stat2,
        txcmp like t5utl-txcmp,
        taxau like pc22t-taxau,
        taxty like t5utt-taxty,
        PERNR LIKE P0001-PERNR,
        anzhl like pc207-anzhl,
        betrg like pc207-betrg,
        atype(4) type c,               " 'P' = period, W2, 941, SUI
        btype(1) type c,        " 'R' = retro 'A' = prior prd adjst
                                " 'C  = current prd adjst N = normal
        sort_strng(100) type c,
     end of totals_table.

data: data_table_tmp   like data_table occurs 0 with header line,
      data_table_cp    like data_table occurs 0 with header line,
      data_table_retro like data_table occurs 0 with header line,
      data_table_ppadj like data_table occurs 0 with header line,
      data_table_cpadj like data_table occurs 0 with header line,
      data_table_accum like data_table occurs 0 with header line,
      data_table_o_acc like data_table occurs 0 with header line.

data: begin of sort_table occurs 1,
        pernr    like p0001-pernr,
        lgart    like pc207-lgart,
        iabkrs   like pc261-iabkrs,
        inper    like pc261-inper,
        ipydt    like pc261-paydt,
        inpty    like pc261-inpty,
        inpid    like pc261-inpid,
        abkrs    like pc261-abkrs,
        fpper    like pc261-fpper,
        paydt    like pc261-paydt,
        payty    like pc261-payty,
        payid    like pc261-payid,
        anzhl    like pc207-anzhl,
        betrg    like pc207-betrg,
        perid_m  like range_prds-per_id,
        betrg_m  like pc207-betrg,
        anzhl_m  like pc207-anzhl,
        perid_q  like range_prds-per_id,
        betrg_q  like pc207-betrg,
        anzhl_q  like pc207-anzhl,
        perid_y  like range_prds-per_id,
        betrg_y  like pc207-betrg,
        anzhl_y  like pc207-anzhl,
        taxty    like t5utt-taxty,
        taxau    like pc22t-taxau,
        amt_curr like pc207-amt_curr,
        atype(4) type c,
        btype(1) type c,
        box like t596g-sumlg,
        sort_strng(100) type c,
     end of sort_table.

data: begin of total_srt_table occurs 0,
        box like t596g-sumlg,
        pernr like p0001-pernr,
        anzhl like pc207-anzhl,
        betrg like pc207-betrg,
        BUKRS LIKE P0001-BUKRS,
        WERKS LIKE P0001-WERKS,
        BTRTL LIKE P0001-BTRTL,
        KOSTL LIKE P0001-KOSTL,
        PERSG LIKE P0001-PERSG,
        PERSK LIKE P0001-PERSK,
        taxty like t5utt-taxty,
        taxau like pc22t-taxau,
        TXCMP LIKE PC22T-TAXAU,
        amt_curr like pc207-amt_curr,
        atype(4) type c,
        btype(1) type c,
        sort_strng(100) type c,
     end of total_srt_table.
data: sort_tmp_1       like data_table occurs 0 with header line,
      sort_tmp_2       like data_table occurs 0 with header line,
      sort_table_tmp   like data_table occurs 0 with header line,
      sort_table_accum like data_table occurs 0 with header line,
      sort_tmp_cp      like data_table occurs 0 with header line,
      sort_tmp_ppadj   like data_table occurs 0 with header line,
      sort_tmp_cpadj   like data_table occurs 0 with header line,
      sort_tmp_retro   like data_table occurs 0 with header line,
      sort_accum_cp    like data_table occurs 0 with header line,
      sort_table_ppadj like data_table occurs 0 with header line,
      sort_table_cpadj like data_table occurs 0 with header line,
      sort_table_retro like data_table occurs 0 with header line,
      sort_table_cp    like data_table occurs 0 with header line,
      total_srt_ppadj  like total_srt_table occurs 0 with header line,
      total_srt_cpadj  like total_srt_table occurs 0 with header line,
      total_srt_retro  like total_srt_table occurs 0 with header line,
      total_srt_cp     like total_srt_table occurs 0 with header line.

data:  begin of i_box occurs 0,
         box like data_table-box,
       end of i_box.

data:  begin of i_btype occurs 0,
         btype like data_table-btype,
      end of i_btype.

data: begin of i_pernr occurs 0,
         pernr  like data_table-pernr,
      end of i_pernr.

*<SAM850091>
*data: begin of i_seqnr occurs 0,
*         seqnr like rgdir-seqnr,
*      end of i_seqnr.
*
*DATA: BEGIN OF TR_TXCMP OCCURS 0,
*         TXCMP LIKE T5UTL-TXCMP,
*         BEGDA LIKE PNPBEGDA,
*         ENDDA LIKE PNPENDDA,
*         ASOFD LIKE T5UXY-ASOFD,
*         GENDT LIKE PC261-RUNDT,
*         GENTM LIKE PC261-RUNTM,
*      END OF TR_TXCMP.
*
*data: begin of it512w occurs 0,
*        LGART LIKE T512W-LGART,
*        VKLAS LIKE T512W-VKLAS,
*        KUMUL LIKE T512W-KUMUL,
*        ENDDA LIKE T512W-ENDDA,
*        BEGDA LIKE T512W-BEGDA,
*      end of it512w.
*
*data: begin of i_wagety occurs 100,
*        LGART LIKE T512W-LGART,
*      END OF I_WAGETY.
*</SAM850091>
