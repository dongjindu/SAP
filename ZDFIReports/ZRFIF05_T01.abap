*&---------------------------------------------------------------------*
*&  Include           ZRFIF05_T01
*&---------------------------------------------------------------------*

INCLUDE <icon>.

************************************************************************
*     DATA DECLARATION
************************************************************************
TABLES: t001,
        bkpf,
        bseg,
        vdarl,     "loans
        vtbfhapo,  "transaction flow
        ztfi_cmal, "actual line item
        ztfi_cmac, "control for collecting actuals
*       ztfi_cmas, "actuals summary
        t036v,     "Alloc. of planning levels for Financial Assets Mgmt
        vdbeki,    "Flow data actual document header
        vdbepi,    "Posted line items for document header.
        vtbfha,    "transaction.
        bsad,bsak, "Accounting:SecondIdx for Customers(Cleared Items)
        knb1,      "Customer Master (Company Code)
        t035,      "Planning groups
        sscrfields.

DATA: BEGIN OF it_zcmal OCCURS 0,
        ssign    LIKE vtbfhapo-ssign.
        INCLUDE STRUCTURE ztfi_cmal.
DATA:   awtyp    LIKE bkpf-awtyp,
        ssolhab  LIKE vdbepi-ssolhab,
        rbelkpfd LIKE vdbeki-rbelkpfd,
        stblg    LIKE bkpf-stblg,
        stjah    LIKE bkpf-stjah,
        gubun(1) TYPE c.  " N : New, M : Update, D : Delete
DATA: END OF it_zcmal.
DATA: it_list  LIKE it_zcmal OCCURS 0 WITH HEADER LINE.
DATA: wk_zcmal LIKE it_zcmal OCCURS 0 WITH HEADER LINE.

*DATA: it_zcmas   LIKE ztfi_cmas OCCURS 0 WITH HEADER LINE.

* plan data control
DATA: BEGIN OF i_zcmad OCCURS 0,
        zeilt  LIKE ztfi_cmad-zeilt,
        hkont  LIKE ztfi_cmad-hkont,
        umskz  LIKE ztfi_cmad-umskz,
        shkzg  LIKE ztfi_cmad-shkzg,
        grupp  LIKE ztfi_cmad-grupp,
        ebene  LIKE ztfi_cmad-ebene,
      END OF i_zcmad.

* Special GL
DATA: BEGIN OF i_t074 OCCURS 0,
        koart  LIKE t074-koart,
        umskz  LIKE t074-umskz,
        skont  LIKE t074-hkont,
        ebene  LIKE t074-ebene,
      END OF i_t074.

*plan level
DATA: BEGIN OF i_skb1 OCCURS 0,
*       bukrs LIKE skb1-bukrs,
        saknr LIKE skb1-saknr,
        fdlev LIKE skb1-fdlev,
        xgkon LIKE skb1-xgkon,
      END OF i_skb1.

*plan group
DATA: BEGIN OF i_t035 OCCURS 0,
        grupp  LIKE t035-grupp,
        ebene  LIKE t035-ebene,
      END OF i_t035.

*Account Assignment References acc. to Transaction Type
DATA: BEGIN OF i_t037s OCCURS 0,
        rantyp   LIKE t037s-rantyp,
        rrefkont LIKE t037s-rrefkont,
        hkont    LIKE t037s-hkont,
      END OF i_t037s.

DATA: BEGIN OF it_bkpf OCCURS 0,
        gubun(1) TYPE c,
        bukrs  LIKE bkpf-bukrs,
        belnr  LIKE bkpf-belnr,
        gjahr  LIKE bkpf-gjahr,
        xblnr  LIKE bkpf-xblnr,  "check number
        cpudt  LIKE bkpf-cpudt,
        cputm  LIKE bkpf-cputm,
        budat  LIKE bkpf-budat,
        aedat  LIKE bkpf-aedat,
        waers  LIKE bkpf-waers,
        bstat  LIKE bkpf-bstat,
        blart  LIKE bkpf-blart,
        stblg  LIKE bkpf-stblg,
        stjah  LIKE bkpf-stjah,
        awtyp  LIKE bkpf-awtyp,  "Reference Transaction
        awkey  LIKE bkpf-awkey,
        tcode  LIKE bkpf-tcode, "Loan..reverse:FNM3
        ssign  LIKE vtbfhapo-ssign,   "(+/-)
     END OF it_bkpf.

DATA: BEGIN OF it_bseg OCCURS 0,
*       BUKRS  LIKE BSEG-BUKRS,
*       GJAHR  LIKE BSEG-GJAHR,
        belnr  LIKE bseg-belnr,
        buzei  LIKE bseg-buzei,

        shkzg  LIKE bseg-shkzg,
        fdgrp  LIKE bseg-fdgrp,
        fdlev  LIKE bseg-fdlev,
        fdtag  LIKE bseg-fdtag,
        gsber  LIKE bseg-gsber,
        pswsl  LIKE bseg-pswsl,
        augdt  LIKE bseg-augdt,
        augbl  LIKE bseg-augbl,

        saknr  LIKE bseg-hkont,

        koart  LIKE bseg-koart,
        hkont  LIKE bseg-hkont,
        lifnr  LIKE bseg-lifnr,
        kunnr  LIKE bseg-kunnr,
        nebtr  LIKE bseg-nebtr,

        wrbtr  LIKE bseg-wrbtr,
        dmbtr  LIKE bseg-dmbtr,

        xopvw  LIKE bseg-xopvw,
        umskz  LIKE bseg-umskz,
        valut  LIKE bseg-valut,

        stblg  LIKE bkpf-stblg,
        bstat  LIKE bkpf-bstat,

*        xref1  LIKE bseg-xref1,
*        xref3  LIKE bseg-xref2,
*        sgtxt  LIKE bseg-sgtxt,
*        zuonr  LIKE bseg-zuonr,
*        aufnr  LIKE bseg-aufnr, "order number
*       rposnr LIKE ztfi_cmal-rposnr,  "to save multi-lines(bkpf)
      END   OF it_bseg.

DATA: BEGIN OF it_apar OCCURS 0,                  "sort by amount
        buzei    LIKE bseg-buzei,
        wrbtr    LIKE bseg-wrbtr,
        dmbtr    LIKE bseg-dmbtr,
        nebtr    LIKE bseg-nebtr, "Payment Amt

        shkzg    LIKE bseg-shkzg,
        koart    LIKE bseg-koart,
        lifnr    LIKE bseg-lifnr,
        kunnr    LIKE bseg-kunnr,
        umskz    LIKE bseg-umskz,
        hkont    LIKE bseg-hkont,

        rebzt    LIKE bseg-rebzt,  "Z-partial, V-residual
        gsber    LIKE bseg-gsber,
        augbl    LIKE bseg-augbl,

        zuonr    LIKE bseg-zuonr,
        aufnr    LIKE bseg-aufnr,
        skfbt    like bseg-skfbt,
      END OF it_apar.

DATA: BEGIN OF pftab  OCCURS 0,
        fcode LIKE  rsmpe-func,
      END   OF pftab.

RANGES:r_cmacct  FOR skb1-saknr OCCURS 0.
RANGES:r_hkont   FOR skb1-saknr OCCURS 0.
RANGES:r_tracct  FOR skb1-saknr OCCURS 0.
*RANGES:r_hkont9a FOR skb1-saknr OCCURS 0.
*RANGES:r_hkont91 FOR skb1-saknr OCCURS 0.
*RANGES:r_hkont92 FOR skb1-saknr OCCURS 0.
*RANGES:r_hkont93 FOR skb1-saknr OCCURS 0.
*RANGES:r_cash   FOR skb1-saknr OCCURS 0.
*RANGES:r_imsi   FOR skb1-saknr OCCURS 0.
RANGES:r_tcodel FOR bkpf-tcode OCCURS 0.
RANGES:r_tcodet FOR bkpf-tcode OCCURS 0.


* $OLD_TSTLO = '19980101000000'.
DATA:  $old_tstlo   LIKE ztfi_cmac-tstlo,
       $tstlo       LIKE ztfi_cmac-tstlo,
       g_date_from  LIKE bkpf-cpudt,
       g_time_from  LIKE bkpf-cputm,
       g_date_to    LIKE bkpf-cpudt,
       g_time_to    LIKE bkpf-cputm,
*      g_trade      LIKE vdbeki-ranl,
       it_zcmal_lin TYPE i,
       g_nrgnr      TYPE i,
       c_waers      LIKE t001-waers.

DATA: p_ftstlo LIKE ztfi_cmac-tstlo,
      p_ttstlo LIKE ztfi_cmac-tstlo.

*------ CONSTANTS
*CONSTANTS: c_leve1 TYPE skb1-fdlev VALUE '90',
*           c_leve2 TYPE skb1-fdlev VALUE '91',
*           c_leve3 TYPE skb1-fdlev VALUE '92',
*           c_leve4 TYPE skb1-fdlev VALUE '93',
*           c_leve5 TYPE skb1-fdlev VALUE '94',
*           c_leve6 TYPE skb1-fdlev VALUE '95',
*           c_leve7 TYPE skb1-fdlev VALUE '96',
*           c_leve8 TYPE skb1-fdlev VALUE '97',
*           c_leve9 TYPE skb1-fdlev VALUE '98',
*           c_lev10 TYPE skb1-fdlev VALUE '99',
*           c_lev11 TYPE skb1-fdlev VALUE '9A',
*           c_lev9x TYPE skb1-fdlev VALUE '9X'.

*..For Display
CONSTANTS: v VALUE '|',
           l_yea TYPE i VALUE 04,
           l_ste TYPE i VALUE 08,
           l_bel TYPE i VALUE 10,
           l_tra TYPE i VALUE 13,
           l_amt TYPE i VALUE 16,
           l_txt TYPE i VALUE 16.

*HARD CODING
CONSTANTS: "c_devlp TYPE bseg-fdlev VALUE '19',
           "c_devlm TYPE bseg-fdlev VALUE '29',
           c_bukrs TYPE t001-bukrs VALUE 'H201'.
*          c_waers TYPE t001-waers VALUE 'USD'.
"g_xx999 LIKE bseg-xref1 VALUE 'XX999'.  "except collecting

* USING ALV REPORTING..
TYPE-POOLS : slis.

INCLUDE rvreuse_global_data.
INCLUDE rvreuse_local_data.
INCLUDE rvreuse_forms.

DATA : gs_layout    TYPE slis_layout_alv,
       gt_fieldcat  TYPE slis_t_fieldcat_alv,
       gt_field     TYPE slis_t_fieldcat_alv,
       g_fieldcat_s TYPE slis_fieldcat_alv,  " ?? ??? ??.
       gt_events    TYPE slis_t_event,
       it_sort      TYPE slis_t_sortinfo_alv,
       g_save(1)    TYPE c,
       g_exit(1)    TYPE c,
       gx_variant   LIKE disvariant,
       g_variant    LIKE disvariant,
       g_repid      LIKE sy-repid,
       g_cnt(2)     TYPE n.

CONSTANTS : c_status_set   TYPE slis_formname
                           VALUE 'PF_STATUS_SET',
            c_user_command TYPE slis_formname
                           VALUE 'USER_COMMAND',
            c_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE',
            c_top_of_list  TYPE slis_formname VALUE 'TOP_OF_LIST',
            c_end_of_list  TYPE slis_formname VALUE 'END_OF_LIST'.

CONSTANTS: char_x   type c  value 'X'.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DATA: BEGIN OF log_tbl OCCURS 0,
        zeile(256) TYPE c.
DATA: END OF log_tbl.


************************************************************************
*     SELECTION-SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-h01.
PARAMETERS: p_bukrs  LIKE t001-bukrs   DEFAULT c_bukrs OBLIGATORY
                                       MEMORY ID buk.
SELECTION-SCREEN COMMENT 52(40) p_butxt.
SELECTION-SCREEN END OF LINE.

PARAMETERS: p_date  AS CHECKBOX.
SELECTION-SCREEN SKIP 1.

PARAMETERS: p_test  AS CHECKBOX DEFAULT 'X' USER-COMMAND ucom .
SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-01s.
PARAMETERS: p_run_p AS CHECKBOX MODIF ID rpa.
SELECTION-SCREEN END OF BLOCK bl3.

PARAMETERS: p_list AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK bl1.

*..(Data Conversion)
SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_gjahr FOR bkpf-gjahr DEFAULT sy-datum(4) NO INTERVALS,
                s_belnr FOR bkpf-belnr, "NO INTERVALS,
                s_budat FOR bkpf-budat  .
SELECT-OPTIONS: s_blart  FOR bkpf-blart.
PARAMETERS:p_conv  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK bl2.


SELECTION-SCREEN BEGIN OF BLOCK bls WITH FRAME TITLE text-007.
*DB read
PARAMETERS: p_dbread AS CHECKBOX.
*                                             "Actual Line Item
SELECT-OPTIONS: s_grupp FOR ztfi_cmal-grupp,  "Planning Group
                s_ebene FOR ztfi_cmal-ebene,  "Planning Level
                s_gsart FOR vdarl-gsart,      "Product Type
                s_datum FOR ztfi_cmal-datum,  "Date
                s_saknr FOR ztfi_cmal-saknr,  "G/L Account Number
                s_lifnr FOR ztfi_cmal-lifnr,  "Vendor or Creditor
                s_kunnr FOR ztfi_cmal-kunnr.  "Customer Number
PARAMETERS: p_out AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK bls.

SELECTION-SCREEN PUSHBUTTON  1(24) runp USER-COMMAND runp.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN PUSHBUTTON  1(24) vslt USER-COMMAND vslt.

*--delete from screen field, but keep logic..
*SELECTION-SCREEN BEGIN OF BLOCK bl4 WITH FRAME TITLE text-003.
*PARAMETERS:p_ftstlo  LIKE ztfi_cmac-tstlo.
*PARAMETERS:p_ttstlo  LIKE ztfi_cmac-tstlo.
*SELECTION-SCREEN END OF BLOCK bl4.
*---

*SELECTION-SCREEN BEGIN OF BLOCK bl5 WITH FRAME TITLE text-002.
*SELECT-OPTIONS: s_blart  FOR bkpf-blart.
*SELECTION-SCREEN END OF BLOCK bl5.

************************************************************************
*     AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN .
  CASE sscrfields-ucomm.
    WHEN 'UCOM'.
    WHEN 'RUNP'.
      PERFORM run_fm_payment.
    WHEN 'VSLT'.
      PERFORM view_log.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

AT SELECTION-SCREEN ON p_bukrs.
  PERFORM get_bukrs_entry.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_blart-low.
  PERFORM help_blart USING s_blart-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_blart-high.
  PERFORM help_blart USING s_blart-high.
