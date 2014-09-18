*&--------------------------------------------------------------------
*& Author                 : HS.Jeong
*& Creation Date          : 10/02/2004
*& Specification By       : hs.jeong
*& Pattern                : Report 1-1
*& Development Request No : UD1K903288
*& Addl documentation     :1
*& Description  : FM Budget/Actual Report
*&                FM actual total : FMIT
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*& 02/25/2005 wskim        UD1K914649     budget report problem
*& 11/26/2011 KDM          UQ1K900271     Fund Center Hierachy not use
*&--------------------------------------------------------------------

REPORT zrfif02 LINE-SIZE 170
             LINE-COUNT 58
             NO STANDARD PAGE HEADING.

INCLUDE <icon>.
INCLUDE <symbol>.


*copy program : WISP_LOESCHE_LAYOUT

TABLES: fmci, fmcit,    "commitment
        fmfctr, fmfctrt,  "fund center
        fmhictr,          "fund center hier
        bppe.
TABLES: fmep,fmsu, fmit.
TABLES: fkrs, fpos, tbpfm, sscrfields.
DATA: ibppe LIKE bppe OCCURS 0 WITH HEADER LINE.

*actual
DATA: ifmit LIKE fmit OCCURS 0 WITH HEADER LINE.


DATA: BEGIN OF itab OCCURS 0,
         key(15) TYPE c,
         fictr   LIKE fmfctr-fictr,  "fundcenter
         geber   LIKE bppe-geber,    "fund
         potyp   LIKE fmci-potyp, "category
         fipos   LIKE fmci-fipos,   "commitment
         tslvt   LIKE fmit-tslvt,    "carryfoward
         oamt    LIKE bppe-wtp01,   "original
         samt    LIKE bppe-wtp01,   "supplement
         tamt    LIKE bppe-wtp01,   "transfer
         ramt    LIKE bppe-wtp01,   "return
         camt    LIKE bppe-wtp01,   "current
         lamt    LIKE bppe-wtp01,   "released
         camtc    LIKE bppe-wtp01,   "cumulative budget
         mamt    LIKE bppe-wtp02,   "commitment
         aamt    LIKE bppe-wtp02,   "actual
         aamts   LIKE bppe-wtp02,   "actual(statistical)
         pamt    LIKE bppe-wtp02,   "payment
         damt    LIKE bppe-wtp02,   "downpay
         aamtc   LIKE bppe-wtp02,   "cumulative actual (prev)
         zamt    LIKE bppe-wtp02,   "residual
         profil  LIKE tbpfm-profil,
         mark(1) TYPE c,
      END OF itab.


DATA : BEGIN OF ifmfctr OCCURS 0,
         ctr_objnr  LIKE fmfctr-ctr_objnr,
         fictr      LIKE fmfctr-fictr,
         parent_obj LIKE fmhictr-parent_obj,
       END OF ifmfctr.

DATA : BEGIN OF ifmci OCCURS 0,
         fipos    LIKE fmci-fipos,
         bezei   LIKE fmcit-bezei ,
         posit    LIKE fmep-posit,
         potyp  LIKE fmci-potyp,
       END OF ifmci.

* Sam file Layout
DATA : BEGIN OF rec OCCURS 10,
             geber LIKE fmps-geber,   "fund
             fistl LIKE bpfmps-fistl, "fund center
             fipos LIKE bpfmps-fipos, "commitment
             wert(15) TYPE c,         "amt
        END OF rec.


* Active availability control on commitment budget
DATA: BEGIN OF fmctl OCCURS 0,
         fictr     LIKE fmfctr-fictr,
         fipos     LIKE fmci-fipos,
         geber     LIKE bppe-geber,
         profil    LIKE tbpfm-profil,
      END OF fmctl.

* for combobox
TYPE-POOLS: vrm.

DATA: it_rt TYPE vrm_values,
      w_rt_line LIKE LINE OF it_rt.

DATA: it_val TYPE vrm_values,
      w_line LIKE LINE OF it_val.

DATA: g_repid             LIKE sy-repid,           " Name of the report
      g_cursor_field(20)  TYPE c VALUE 'P_OUTG', " default cursor pos.
      g_sscr_ucomm        TYPE sscrfields-ucomm.   " PAI function code
DATA :  wa_bu_chk.

RANGES : r_date FOR sy-datum.
SELECTION-SCREEN BEGIN OF BLOCK sb WITH FRAME TITLE c010.
*p_buk  like fmit-rbukrs memory id BUK obligatory,
PARAMETERS :
             p_fik  LIKE fmps-fikrs  DEFAULT 'H201',
*             MEMORY ID FIK OBLIGATORY,
             p_gjr  LIKE bpdy-jahr DEFAULT sy-datum+0(4),
*             MEMORY ID GJR OBLIGATORY,
             p_per  LIKE bpdy-perio DEFAULT sy-datum+4(2).
*             memory id per obligatory.
SELECTION-SCREEN END OF BLOCK sb.
* WRTTP: 43 - current, 46 - release
* VORGA: kbud - origin, kbn0 - supp, kbr0 - return, kbus - transfer
*        kbfr - release


SELECTION-SCREEN BEGIN OF BLOCK sl WITH FRAME TITLE c020.
SELECT-OPTIONS: p_fictr FOR fmfctr-fictr MEMORY ID fis,
                p_fipos FOR fmci-fipos,   "default '600000' option GE,
                p_geber FOR bppe-geber  MEMORY ID fic,
                p_prof  FOR tbpfm-profil,  "default 'B' option NE
                p_knz   FOR fmci-potyp DEFAULT '3'.  " item category

PARAMETERS: p_ver  LIKE bppe-versn DEFAULT '000' NO-DISPLAY.
PARAMETERS: p_type(1) TYPE c AS LISTBOX VISIBLE LENGTH 25 OBLIGATORY.

* ' ' - available.    'X' - released

SELECTION-SCREEN END OF BLOCK sl.


SELECTION-SCREEN BEGIN OF BLOCK s3 WITH FRAME TITLE c030.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) c_scale.
SELECTION-SCREEN POSITION 33.
PARAMETERS: p_r LIKE rfpdo1-ts70skal  DEFAULT '0'.
SELECTION-SCREEN COMMENT 35(1) c_slash.
SELECTION-SCREEN POSITION 37.
PARAMETERS: p_d LIKE rfpdo1-ts70skal  DEFAULT '0'.
*SELECTION-SCREEN COMMENT 39(1) c_slash2.
*selection-screen position 41.
*parameters: p_amtype(1) type c default ' '.
SELECTION-SCREEN END OF LINE.

PARAMETERS: p_act(1) TYPE c AS LISTBOX VISIBLE LENGTH 25 OBLIGATORY.

PARAMETERS: p_cumm AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK s3.
**********************************
***************** Toggle block 01 : general selections

SELECTION-SCREEN: PUSHBUTTON /1(30) pushb_o1         " Open Block 01
                    USER-COMMAND ucomm_o1 MODIF ID mo1,
                  PUSHBUTTON /1(30) pushb_c1         " Close Block 01
                    USER-COMMAND ucomm_c1 MODIF ID mc1.
*----- MODIF ID mc1
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (40) c_cum1_h MODIF ID mc1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
SELECTION-SCREEN COMMENT (83) c_cum11 MODIF ID mc1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
SELECTION-SCREEN COMMENT (83) c_cum12 MODIF ID mc1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
SELECTION-SCREEN COMMENT (83) c_cum13 MODIF ID mc1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (40) c_cum2_h MODIF ID mc1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
SELECTION-SCREEN COMMENT (83) c_cum21 MODIF ID mc1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
SELECTION-SCREEN COMMENT (83) c_cum22 MODIF ID mc1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
SELECTION-SCREEN COMMENT (83) c_cum23 MODIF ID mc1.
SELECTION-SCREEN END OF LINE.
*************************************************

SELECTION-SCREEN BEGIN OF BLOCK s4 WITH FRAME TITLE c040.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) c_thred.
SELECTION-SCREEN POSITION 33.
PARAMETERS: p_thred LIKE raip1-prozu DEFAULT 90.
SELECTION-SCREEN POSITION 41.
PARAMETERS: p_thonly AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK s4.



PARAMETERS : p_file LIKE rlgrap-filename DEFAULT
   'c:\temp\fmrpt.xls'.

DATA: g_per(2)  TYPE n.
DATA: g_bldat   LIKE bpdy-bldat,
      g_subrc   LIKE sy-subrc.

TABLES: t100.

* for function
DATA: g_func(1) TYPE c.

* Variables
DATA: v_field(20) TYPE c,
      g_history   TYPE c,
      g_pfr       LIKE fmto-perio,
      g_pto       LIKE fmto-perio.
RANGES: r_perio   FOR  fmfi-perio.

CONSTANTS: gv(1) TYPE c VALUE '|'.
PARAMETERS: p_avpn(1) TYPE c NO-DISPLAY,  " display variant number
            p_cb1(1) TYPE c NO-DISPLAY," Close Block 1
            p_cb2(1) TYPE c NO-DISPLAY." Close Block 2
*---------------------------------------------------------------------*
* Initialization.
*---------------------------------------------------------------------*
INITIALIZATION.
* for combo box
  w_rt_line-key  = '1'.
  w_rt_line-text = 'Current Budget'.
  APPEND w_rt_line TO it_rt.
  w_rt_line-key  = '2'.
  w_rt_line-text = 'Budget Activity'.
  APPEND w_rt_line TO it_rt.
  w_rt_line-key  = '3'.
  w_rt_line-text = 'Payment/Down Payment'.
  APPEND w_rt_line TO it_rt.
  p_type = '1'.

  w_line-key  = '1'.
  w_line-text = 'Sort by Fund Center'.
  APPEND w_line TO it_val.
  w_line-key  = '2'.
  w_line-text = 'Sort by Commitment Item'.
  APPEND w_line TO it_val.
  w_line-key  = '3'.
  w_line-text = 'Sort by Fund'.
  APPEND w_line TO it_val.
  p_act = '1'.

* initial screen text
  c_scale  = 'Scale/Decimal'.
  c_slash  = '/'.
* c_slash2 = '/'.
  c_thred  = 'Threshold'.
  c010 = 'Run Parameter'.
  c020 = 'Select option'.
  c030 = 'Display'.
  c040 = 'Threshold'.
  c_cum1_h = 'In Case of Cummlative = '' '' '.
  c_cum11 = 'Regardless of control term ''residual'' Column represents'.
  c_cum12 = 'residual amount up to prior month'.
  c_cum13 = 'Other column represents just the amount of entered month'.
  c_cum2_h = 'In Case of Cummlative = V '.
c_cum21 = 'Regardless of control term ''Cummlative'' Column represents'.

  c_cum22 = 'cummlative budget  up to prior month'.
  c_cum23 =
   'Other column represents own amount up to relevent controlterm'.

*****************************
  PERFORM i01_initialization.



*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*-------------------------------------*
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = 'P_TYPE'
            values = it_rt.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = 'P_ACT'
            values = it_val.
******************************
  PERFORM i01_control_sel_screen_pbo.
********************************************
AT SELECTION-SCREEN.
*********************************************
  PERFORM i01_control_sel_screen_pai.

*---------------------------------------------------------------------
*  S T A R T - O F - S E L E C T I O N
*---------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM get_master_info.
* read database
  REFRESH itab.

  PERFORM get_bbpe.

  PERFORM get_fmit.

  PERFORM sort_itab.

*---------------------------------------------------------------------
*  END-OF-SELECTION.
*---------------------------------------------------------------------
END-OF-SELECTION.
  PERFORM display_data.

*----------------------------------------------------------------------*
* AT LINE SELECTION
*----------------------------------------------------------------------*
AT LINE-SELECTION.

  GET CURSOR FIELD v_field.
  CASE v_field.
*{ 09/21/11 PaUL iNSERT Ifmci-bezei
    WHEN 'Ifmci-bezei '.
*}
    WHEN 'ITAB-GEBER'.
* Drill down to master data
      SET PARAMETER ID 'FIK' FIELD p_fik.
      SET PARAMETER ID 'FIC' FIELD itab-geber.
      CALL TRANSACTION 'FM5S' AND SKIP FIRST SCREEN.
    WHEN 'ITAB-FICTR'.
* Drill down to master data
      SET PARAMETER ID 'FIK' FIELD p_fik.
      SET PARAMETER ID 'FIS' FIELD itab-fictr.
*{ 09/21/11 PaUL FM2S -> FMSC AFTER UPGRADE
*      CALL TRANSACTION 'FM2S' AND SKIP FIRST SCREEN.
      CALL TRANSACTION 'FMSC' AND SKIP FIRST SCREEN.

    WHEN 'ITAB-FIPOS'.
* Drill down to actual summary
      IF p_cumm = 'X'.
        g_pfr = '01'.
      ELSE.
        g_pfr = p_per.
      ENDIF.
      g_pto = p_per.
      CHECK itab-fipos <> space.
      r_perio-sign = 'I'.
      r_perio-low = g_pfr.
      r_perio-high = g_pto.
      r_perio-option = 'BT'.
      APPEND r_perio.
*{ 09/21/11 PuAL CHANGE
*      SUBMIT rffmep1a
      SUBMIT RFFMEPGAX
        WITH s_fictr = itab-fictr
        WITH s_fikrs = p_fik
*        WITH s_fincd = itab-geber
*        WITH s_fipos = itab-fipos
        WITH s_FONDS = itab-geber
        WITH s_fipEX = itab-fipos
        WITH s_gjahr = p_gjr
        WITH s_perio IN r_perio
        WITH P_MAXSEL = ''
      AND RETURN.
*---

*         OAMT    like BPPE-WTP01,   "original
*         SAMT    like BPPE-WTP01,   "supplement
*         TAMT    like BPPE-WTP01,   "transfer
*         RAMT    like BPPE-WTP01,   "return
    WHEN 'ITAB-OAMT' OR 'ITAB-SAMT' OR 'ITAB-RAMT'.
      MOVE 'I'  TO r_date-sign.
      MOVE 'BT' TO r_date-option.
*      concatenate  '01' p_per+1(2) p_gjr  into r_date-low.
*      concatenate  '31' p_per+1(2) p_gjr  into r_date-high.
      CONCATENATE  p_gjr p_per+1(2) '01'    INTO r_date-low.
      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
        EXPORTING
          day_in                  = r_date-low
       IMPORTING
         last_day_of_month       = r_date-high
*       EXCEPTIONS
*         DAY_IN_NO_DATE          = 1
*         OTHERS                  = 2
                .
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      APPEND r_date.

      SUBMIT zrfii18
        WITH r_3      = 'X'
        WITH p_bukrs = 'H201'
        WITH s_fifctr-low = itab-fictr
        WITH s_fipos-low = itab-fipos
*       WITH s_cpudt IN  r_date
      AND RETURN.
    WHEN 'ITAB-WTP01'.
      PERFORM display_fm_doc.


    WHEN OTHERS.
* Drill down to actual summary
      IF p_cumm = 'X'.
        g_pfr = '01'.
      ELSE.
        g_pfr = p_per.
      ENDIF.
      g_pto = p_per.
      CHECK itab-fictr <> space.

*  SET PARAMETER ID 'VPE' FIELD g_pfr.
*  SET PARAMETER ID 'BPE' FIELD g_pto.
*  SET PARAMETER ID 'VPF' FIELD g_pfr.
*  SET PARAMETER ID 'BPF' FIELD g_pto.
*{ 09/21/11 Paul rffmto10->rffmto10X after Upgrade
*                 fincd -> fonds
*                 fipos -> fipex
      SUBMIT rffmto10X
              WITH p_fyr_fr = p_gjr
              WITH p_fyr_to = p_gjr
              WITH p_per_fr = g_pfr
              WITH p_per_to = g_pto
*             WITH P_FYF_FR = p_gjr
*             WITH P_FYF_TO = p_gjr
*             WITH P_PEF_FR = g_pfr
*             WITH P_PEF_TO = g_pto
              WITH s_fictr = itab-fictr
              WITH s_fikrs = p_fik
*              WITH s_fincd = itab-geber
*              WITH s_fipos = itab-fipos
              WITH s_fonds = itab-geber
              WITH s_fipex = itab-fipos
            AND RETURN.

  ENDCASE.
************************************************************************
TOP-OF-PAGE.
  PERFORM top_of_page.

************************************************************************
* TOP-OF-PAGE DURING LINE-SELECTION
************************************************************************
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM top_of_page.

************************************************************************
* Line selection                                                       *
************************************************************************
AT LINE-SELECTION.
  PERFORM pick.

************************************************************************
***  (SHIFT+PF1) Execute Download
************************************************************************
AT PF13.
  PERFORM data_download.

************************************************************************
***  (SHIFT+PF4) Release simulation
************************************************************************
AT PF16.

************************************************************************
***  (SHIFT+PF5) Release
************************************************************************
AT PF17.

  INCLUDE lfmauequ.
*&---------------------------------------------------------------------*
*&      Form  get_master_info
*&---------------------------------------------------------------------*
FORM get_master_info.
  DATA: l_berec.

* commitment Item
  SELECT * INTO CORRESPONDING FIELDS OF TABLE ifmci
     FROM fmci
     WHERE fikrs   =  p_fik
       AND fipos   IN p_fipos
       AND potyp IN p_knz.

* commitment item text
  LOOP AT ifmci.
    SELECT SINGLE bezei  INTO ifmci-bezei
       FROM fmcit
       WHERE spras = sy-langu
         AND fikrs = p_fik
         AND fipex = ifmci-fipos.
    MODIFY ifmci.
  ENDLOOP.

* Fund Center
  SELECT * INTO CORRESPONDING FIELDS OF TABLE ifmfctr
     FROM fmfctr
     WHERE fikrs =  p_fik
       AND fictr IN p_fictr.

*{ 09/19/11 Paul delete
** Fund Center Hiearchy (select end node only)
  LOOP AT ifmfctr.
*    SELECT SINGLE * FROM fmhictr
*       WHERE ctr_objnr = ifmfctr-ctr_objnr.
*    IF fmhictr-parent_obj = space.
*      DELETE ifmfctr.
*    ELSE.
**-- end node...

**-->Fund Center 'HMMA' Delete manually(11/26/2011 BY KDM)
**-->Because Fund Center hierachy does not use -- UQ1K900271
    IF ifmfctr-fictr = 'HMMA'.
      DELETE ifmfctr.
    ELSE.
*--- check FundCenter Auth.
      SELECT SINGLE * FROM fmfctr
          WHERE fikrs = p_fik
            AND fictr = ifmfctr-fictr
            AND datbis >= sy-datum.

      AUTHORITY-CHECK OBJECT 'Z_FICTR'
               ID 'FM_FIKRS'   FIELD p_fik
               ID 'FM_FICTR'   FIELD fmfctr-fictr.

      IF sy-subrc <> 0.
        DELETE ifmfctr.
      ENDIF.
    ENDIF.
  ENDLOOP.
*}


* Budget Profile
  SELECT * INTO CORRESPONDING FIELDS OF TABLE fmctl
    FROM tbpfm
    WHERE fikrs = p_fik
      AND gjahr = p_gjr.

ENDFORM.                    " get_master_info
*&---------------------------------------------------------------------*
*&      Form  data_download
*&---------------------------------------------------------------------*
FORM data_download.
* EDIT_TABLE_WITH_EXCEL
* SEND_TABLE_TO_EXCEL

  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename = p_file
            filetype = 'WK1'
       TABLES
            data_tab = itab.

  WRITE:/ p_file, ' is created...'.
ENDFORM.                    " data_download
*&---------------------------------------------------------------------*
*&      Form  get_bbpe
*&---------------------------------------------------------------------*
FORM get_bbpe.
  DATA: l_amt LIKE bppe-wtp01,
        lcamt LIKE bppe-wtp01.

  SELECT * FROM bppe
    INTO TABLE ibppe
    FOR ALL ENTRIES IN ifmfctr
    WHERE objnr =  ifmfctr-ctr_objnr
      AND gjahr =  p_gjr
      AND versn =  p_ver
      AND geber IN p_geber.

  LOOP AT ibppe.
    CLEAR itab.

    READ TABLE ifmci  WITH KEY posit = ibppe-posit.
    CHECK sy-subrc = 0.
    CHECK ifmci-fipos IN p_fipos.

    READ TABLE ifmfctr WITH KEY ctr_objnr = ibppe-objnr.
    CHECK sy-subrc = 0.
    CHECK ifmfctr-fictr IN p_fictr.

    MOVE-CORRESPONDING ibppe TO itab.
    itab-fipos   = ifmci-fipos.
    itab-potyp = ifmci-potyp.
    itab-fictr   = ifmfctr-fictr.

    PERFORM get_budget_period USING itab-fictr itab-fipos itab-geber
                              CHANGING itab-profil.
* filter profile
    CHECK itab-profil IN p_prof.

* Cumulative Sum until previous period
    PERFORM cal_cum_budget USING    'X'
                           CHANGING lcamt.

* available budget = orgin - released
    IF p_cumm = ' '.
      CASE p_per.  " Period
        WHEN 1.     l_amt = ibppe-wtp01.
        WHEN 2.     l_amt = ibppe-wtp02.
        WHEN 3.     l_amt = ibppe-wtp03.
        WHEN 4.     l_amt = ibppe-wtp04.
        WHEN 5.     l_amt = ibppe-wtp05.
        WHEN 6.     l_amt = ibppe-wtp06.
        WHEN 7.     l_amt = ibppe-wtp07.
        WHEN 8.     l_amt = ibppe-wtp08.
        WHEN 9.     l_amt = ibppe-wtp09.
        WHEN 10.    l_amt = ibppe-wtp10.
        WHEN 11.    l_amt = ibppe-wtp11.
        WHEN 12.    l_amt = ibppe-wtp12.
      ENDCASE.
    ELSE.
* Cumulative Sum by Period Control
      CASE itab-profil.
        WHEN 'Y'.
          l_amt = ibppe-wtp01 + ibppe-wtp02 + ibppe-wtp03
                + ibppe-wtp04 + ibppe-wtp05 + ibppe-wtp06
                + ibppe-wtp07 + ibppe-wtp08 + ibppe-wtp09
                + ibppe-wtp10 + ibppe-wtp11 + ibppe-wtp12.

        WHEN 'H'.
          IF p_per <= 6.
            l_amt = ibppe-wtp01 + ibppe-wtp02 + ibppe-wtp03
                  + ibppe-wtp04 + ibppe-wtp05 + ibppe-wtp06.
          ELSE.
            l_amt = ibppe-wtp01 + ibppe-wtp02 + ibppe-wtp03
                  + ibppe-wtp04 + ibppe-wtp05 + ibppe-wtp06
                  + ibppe-wtp07 + ibppe-wtp08 + ibppe-wtp09
                  + ibppe-wtp10 + ibppe-wtp11 + ibppe-wtp12.
          ENDIF.
        WHEN 'Q'.
          IF p_per <= 3.
            l_amt = ibppe-wtp01 + ibppe-wtp02 + ibppe-wtp03.
          ELSEIF p_per <= 6.
            l_amt = ibppe-wtp01 + ibppe-wtp02 + ibppe-wtp03
                  + ibppe-wtp04 + ibppe-wtp05 + ibppe-wtp06.
          ELSEIF p_per <= 9.
            l_amt = ibppe-wtp01 + ibppe-wtp02 + ibppe-wtp03
                  + ibppe-wtp04 + ibppe-wtp05 + ibppe-wtp06
                  + ibppe-wtp07 + ibppe-wtp08 + ibppe-wtp09.
          ELSE. " p_per >= 12.
            l_amt = ibppe-wtp01 + ibppe-wtp02 + ibppe-wtp03
                  + ibppe-wtp04 + ibppe-wtp05 + ibppe-wtp06
                  + ibppe-wtp07 + ibppe-wtp08 + ibppe-wtp09
                  + ibppe-wtp10 + ibppe-wtp11 + ibppe-wtp12.
          ENDIF.
        WHEN OTHERS.
          l_amt = lcamt.   " use lcamt
      ENDCASE.
    ENDIF.
* if not controlling budget, do not use released budget
    IF itab-profil NA 'MQHY'.
      CHECK ibppe-wrttp <> '46'.

      itab-camtc = lcamt.
      itab-lamt = l_amt.
    ELSE.
* Released
      IF ibppe-wrttp = '46'.
        itab-lamt  = l_amt.    "current month release
        itab-camtc  = lcamt.
      ELSE.
        CASE ibppe-vorga.
          WHEN 'KBUD'.  itab-oamt = l_amt. "origin
          WHEN 'KBR0'.  itab-ramt = l_amt. "return
          WHEN 'KBN0'.  itab-samt = l_amt. "supplement
          WHEN 'KBUE'.  itab-tamt = l_amt. "receiver
          WHEN 'KBUS'.  itab-tamt = l_amt. "sender
        ENDCASE.
        itab-camt  = l_amt.  " current
      ENDIF.
    ENDIF.

    COLLECT itab.
    CLEAR   itab.
  ENDLOOP.

  if itab[] is initial.
    message s000(zmfi) with 'Data Not exist ...'.
  endif.

ENDFORM.                    " get_bbpe
*&---------------------------------------------------------------------*
*&      Form  get_fmit
*&---------------------------------------------------------------------*
FORM get_fmit.
  DATA: l_amt LIKE bppe-wtp01.

  SELECT * FROM fmit
    INTO TABLE ifmit
    WHERE rfistl IN p_fictr
      AND rfonds IN p_geber
      AND ryear  =  p_gjr.

  LOOP AT ifmit.
* check auth
    READ TABLE ifmfctr WITH KEY fictr = ifmit-rfistl.
    CHECK sy-subrc = 0.

* filter for commitment item
    CHECK ifmit-rfipex IN p_fipos.
    READ TABLE ifmci  WITH KEY fipos = ifmit-rfipex.
    CHECK sy-subrc = 0.

    CLEAR itab.

    itab-fipos   = ifmit-rfipex.
    itab-potyp = ifmci-potyp.
    itab-fictr   = ifmit-rfistl.
    itab-geber   = ifmit-rfonds.

    PERFORM get_budget_period USING itab-fictr itab-fipos itab-geber
                              CHANGING itab-profil.
* filter profile
    CHECK itab-profil IN p_prof.

*   perform switch_sign.

    CLEAR: l_amt.
    IF p_cumm = ' '.
      CASE p_per.  " Period
        WHEN 1.     l_amt = ifmit-hsl01.
        WHEN 2.     l_amt = ifmit-hsl02.
        WHEN 3.     l_amt = ifmit-hsl03.
        WHEN 4.     l_amt = ifmit-hsl04.
        WHEN 5.     l_amt = ifmit-hsl05.
        WHEN 6.     l_amt = ifmit-hsl06.
        WHEN 7.     l_amt = ifmit-hsl07.
        WHEN 8.     l_amt = ifmit-hsl08.
        WHEN 9.     l_amt = ifmit-hsl09.
        WHEN 10.    l_amt = ifmit-hsl10.
        WHEN 11.    l_amt = ifmit-hsl11.
        WHEN 12.    l_amt = ifmit-hsl12.
      ENDCASE.
    ELSE.
*     if p_amtype = '9'.
      IF p_per >=  1.  l_amt = l_amt + ifmit-hsl01.  ENDIF.
      IF p_per >=  2.  l_amt = l_amt + ifmit-hsl02.  ENDIF.
      IF p_per >=  3.  l_amt = l_amt + ifmit-hsl03.  ENDIF.
      IF p_per >=  4.  l_amt = l_amt + ifmit-hsl04.  ENDIF.
      IF p_per >=  5.  l_amt = l_amt + ifmit-hsl05.  ENDIF.
      IF p_per >=  6.  l_amt = l_amt + ifmit-hsl06.  ENDIF.
      IF p_per >=  7.  l_amt = l_amt + ifmit-hsl07.  ENDIF.
      IF p_per >=  8.  l_amt = l_amt + ifmit-hsl08.  ENDIF.
      IF p_per >=  9.  l_amt = l_amt + ifmit-hsl09.  ENDIF.
      IF p_per >= 10.  l_amt = l_amt + ifmit-hsl10.  ENDIF.
      IF p_per >= 11.  l_amt = l_amt + ifmit-hsl11.  ENDIF.
      IF p_per >= 12.  l_amt = l_amt + ifmit-hsl12.  ENDIF.
*     endif.
    ENDIF.

* 50 - p/r, 51 - p/o, 54 - invoice, 57 - payment, 58 - d/p req
* 60 -parked, 61 - downpayment
* 95 - co posting (secondary cost posting)
    IF ifmit-rwrttp = '50' OR ifmit-rwrttp = '51'
    OR ifmit-rwrttp = '60' OR ifmit-rwrttp = '61'
    OR ifmit-rwrttp = '81'.

      PERFORM cumm_commitment.
      PERFORM cumm_used.
      IF ifmit-rwrttp = '61'.
        itab-damt = l_amt.   " downpayment
      ENDIF.
      COLLECT itab.
      CLEAR : itab-mamt, itab-aamtc.
* Original Only for invoice
    ELSEIF (  ifmit-rwrttp = '54'
           OR ifmit-rwrttp = '66'
           OR ifmit-rwrttp = '95'
           OR ifmit-rwrttp = '64' )  "fund trf.
        AND ifmit-rbtart = '0100'.
      PERFORM cumm_used.

*statistical update (just information)
*      if ifmit-rwrttp = '66' and ifmit-RSTATS = 'X'.
*        itab-aamtc = l_amt.
*      else.
*      if p_cumm = 'X'.
*        itab-aamt = l_amt + itab-aamtc.  "Cumulative
*      else.
      itab-aamt = l_amt.
*      endif.

      COLLECT itab.

* Payment
    ELSEIF ifmit-rbtart = '0250'.  " Paid
      itab-pamt = l_amt.
      COLLECT itab.
    ELSEIF ifmit-rwrttp = '61'.  " down payment
      itab-damt = l_amt.
      COLLECT itab.
      CLEAR itab-damt.
    ENDIF.
*================2004/02/10
    IF ifmit-rwrttp = '50' OR ifmit-rwrttp = '51' ."OR
*---start #1 wskim
*      ifmit-rwrttp = '54'.
*      IF ifmit-rbtart = '0350'.
     IF ifmit-rbtart = '0350' OR ifmit-rbtart = '0150' OR
        ifmit-rbtart = '0500'.
*---end

*---2004/04/20
        ifmit-hslvt = ifmit-hslvt * -1.
        itab-tslvt = ifmit-hslvt .
*       itab-camtc = ifmit-hslvt.
*       itab-lamt  = ifmit-hslvt.    "release
*         itab-mamt  = ifmit-hslvt.
*        itab-aamt  = ifmit-hslvt.
        COLLECT itab.
        CLEAR itab-aamt.
      ENDIF.
    ENDIF.
    CLEAR itab.
  ENDLOOP.
ENDFORM.                    " get_fmit
*&---------------------------------------------------------------------*
*&      Form  cumm_used
*&---------------------------------------------------------------------*
FORM cumm_used.
  CLEAR itab-aamtc.
* for calc auto c/f budget amount
  IF p_per >  1.  itab-aamtc = itab-aamtc + ifmit-hsl01.  ENDIF.
  IF p_per >  2.  itab-aamtc = itab-aamtc + ifmit-hsl02.  ENDIF.
  IF p_per >  3.  itab-aamtc = itab-aamtc + ifmit-hsl03.  ENDIF.
  IF p_per >  4.  itab-aamtc = itab-aamtc + ifmit-hsl04.  ENDIF.
  IF p_per >  5.  itab-aamtc = itab-aamtc + ifmit-hsl05.  ENDIF.
  IF p_per >  6.  itab-aamtc = itab-aamtc + ifmit-hsl06.  ENDIF.
  IF p_per >  7.  itab-aamtc = itab-aamtc + ifmit-hsl07.  ENDIF.
  IF p_per >  8.  itab-aamtc = itab-aamtc + ifmit-hsl08.  ENDIF.
  IF p_per >  9.  itab-aamtc = itab-aamtc + ifmit-hsl09.  ENDIF.
  IF p_per > 10.  itab-aamtc = itab-aamtc + ifmit-hsl10.  ENDIF.
  IF p_per > 11.  itab-aamtc = itab-aamtc + ifmit-hsl11.  ENDIF.
  IF p_per > 12.  itab-aamtc = itab-aamtc + ifmit-hsl12.  ENDIF.
ENDFORM.                    " cumm_used
*&---------------------------------------------------------------------*
*&      Form  cumm_commitment
*&---------------------------------------------------------------------*
FORM cumm_commitment.
* commitment is Cumulative (include year c/f)
* C/F - Amt Type : 0350
  itab-mamt = itab-mamt + ifmit-tslvt.
  IF p_per >=  1.  itab-mamt = itab-mamt + ifmit-hsl01.  ENDIF.
  IF p_per >=  2.  itab-mamt = itab-mamt + ifmit-hsl02.  ENDIF.
  IF p_per >=  3.  itab-mamt = itab-mamt + ifmit-hsl03.  ENDIF.
  IF p_per >=  4.  itab-mamt = itab-mamt + ifmit-hsl04.  ENDIF.
  IF p_per >=  5.  itab-mamt = itab-mamt + ifmit-hsl05.  ENDIF.
  IF p_per >=  6.  itab-mamt = itab-mamt + ifmit-hsl06.  ENDIF.
  IF p_per >=  7.  itab-mamt = itab-mamt + ifmit-hsl07.  ENDIF.
  IF p_per >=  8.  itab-mamt = itab-mamt + ifmit-hsl08.  ENDIF.
  IF p_per >=  9.  itab-mamt = itab-mamt + ifmit-hsl09.  ENDIF.
  IF p_per >= 10.  itab-mamt = itab-mamt + ifmit-hsl10.  ENDIF.
  IF p_per >= 11.  itab-mamt = itab-mamt + ifmit-hsl11.  ENDIF.
  IF p_per >= 12.  itab-mamt = itab-mamt + ifmit-hsl12.  ENDIF.
ENDFORM.                    " cumm_commitment

*&---------------------------------------------------------------------*
*&      Form  fill_budget
*&---------------------------------------------------------------------*
FORM fill_budget.
  REFRESH rec.
  LOOP AT itab.

* release - actual
    rec-wert = itab-lamt - itab-aamt.

    rec-fistl = itab-fictr. "fundcenter
    rec-fipos = itab-fipos. "commitment item
    rec-geber = itab-geber. "fund

    IF rec-wert <> 0.
      APPEND rec.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " fill_budget
*&---------------------------------------------------------------------*
*&      Form  sort_itab
*&---------------------------------------------------------------------*
FORM sort_itab..
* delete zero amount
  LOOP AT itab.
    IF  itab-oamt  = 0   "original
    AND itab-samt  = 0   "supplement
    AND itab-tamt  = 0   "transfer
    AND itab-ramt  = 0   "return
    AND itab-camt  = 0   "current
    AND itab-lamt  = 0   "released
    AND itab-mamt  = 0   "commitment
    AND itab-aamt  = 0   "actual
    AND itab-pamt  = 0   "payment
    AND itab-damt  = 0   "downpay
    AND itab-camtc = 0   "release sum
    AND itab-aamtc = 0.  "actual sum
      DELETE itab.
    ENDIF.
  ENDLOOP.

* fill key field
  LOOP AT itab.
    CASE p_act.
      WHEN '1'.
        itab-key(10) = itab-fictr.  itab-key+10(1) = itab-potyp.
      WHEN '2'.
        itab-key(1) = itab-potyp. itab-key+1(10) = itab-fipos.
      WHEN '3'.
        itab-key(10) = itab-geber.  itab-key+10(1) = itab-potyp.
    ENDCASE.

* residual amount = cum budget - current budget - actual - commitment
    itab-zamt = itab-camtc - itab-lamt + itab-aamtc.
    MODIFY itab.
  ENDLOOP.
  CASE p_act.
    WHEN '1'. SORT itab BY key fipos.
    WHEN '2'. SORT itab BY key fictr geber.
    WHEN '3'. SORT itab BY key fipos fictr.
  ENDCASE.


ENDFORM.                    " sort_itab
*&---------------------------------------------------------------------*
*&      Form  get_budget_period
*&---------------------------------------------------------------------*
FORM get_budget_period USING    f_fictr f_fipos f_geber
                       CHANGING f_profil.
  READ TABLE ifmci  WITH KEY fipos = f_fipos.
  IF sy-subrc = 0.
    PERFORM determine_profile_fs USING    p_fik
                                          f_fictr
                                          ifmci-posit
                                          f_geber
                                          p_gjr
                                 CHANGING f_profil.
  ELSE.
    f_profil = ' '.
  ENDIF.
ENDFORM.                    " get_budget_period
*---------------------------------------------------------------------*
*       FORM DETERMINE_PROFILE_FS                                     *
*---------------------------------------------------------------------*
FORM determine_profile_fs USING    l_fikrs
                                   l_fictr
                                   l_posit
                                   l_geber
                                   l_gjahr
                          CHANGING l_bprof.
  DATA: l_objnr LIKE fmfctr-ctr_objnr.

  l_objnr(2) = 'FS'.
  l_objnr+2(4) = l_fikrs.
  l_objnr+6  = l_fictr.

* Profile from TBPFM table.
  CALL FUNCTION 'KBPA_FIFM_GET_PROFIL'
       EXPORTING
            i_objnr         = l_objnr
            i_posit         = l_posit
            i_geber         = l_geber
            i_gjahr         = l_gjahr
*{ 09/19/11 Paul insert I_FAREA
            I_FAREA         = ''
*}
       IMPORTING
            e_profil        = l_bprof
       EXCEPTIONS
            no_profil_found = 01.


  IF NOT sy-subrc IS INITIAL.
*   Profile from FundMgt Area
    CALL FUNCTION 'FM5B_GET_PROFILE'
         EXPORTING
              i_fikrs           = l_fikrs
              i_fincode         = l_geber
         IMPORTING
              e_profil          = l_bprof
         EXCEPTIONS
              fm_area_not_found = 1
              OTHERS            = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  pick
*&---------------------------------------------------------------------*
FORM pick.
  CHECK g_func = 'X'.

  IF sy-cucol < 15.     " download
    PERFORM data_download.
  ELSEIF sy-cucol < 30. " test

  ELSE.
    READ LINE 1 FIELD VALUE p_r.
    READ LINE 1 FIELD VALUE p_d.
    sy-lsind = sy-lsind - 1.
    PERFORM display_data.

  ENDIF.

* icon_refresh AS ICON HOTSPOT
* READ CURRENT LINE
*           FIELD VALUE report_lst-optres INTO report_lst-optres.
ENDFORM.                    " pick
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
FORM top_of_page.
  WRITE:/ 'Year/Period:',
          p_gjr NO-GAP, '/' NO-GAP, p_per.
  IF p_cumm = 'X'.
    WRITE: '- Cumulative'.
  ELSE.
    WRITE: '- Current Month'.
  ENDIF.
  WRITE: ' (Round:',   p_r NO-GAP ,
         ', Decimal:', p_d NO-GAP , ') '.
*  uline.
*  g_func = 'X'.
*  write: icon_execute_object AS ICON HOTSPOT, 'Download(S_F1) ',
*         icon_execute_object AS ICON HOTSPOT, 'Test(S_F4)     ',
*         icon_execute_object AS ICON HOTSPOT, 'Run(S_F5)      ',
*         icon_refresh        AS ICON HOTSPOT, 'Refresh        '.
*
*  hide: g_func.
*  clear: g_func.
*  uline.

  FORMAT COLOR COL_HEADING.
  PERFORM write_uline.
  WRITE:/ gv NO-GAP, ' ' NO-GAP, gv NO-GAP, 'FndCtr' NO-GAP,
          gv NO-GAP, 'Commitment Item      ' NO-GAP,
          gv NO-GAP, 'Fund      '   NO-GAP,
          gv NO-GAP, 'P'            NO-GAP.
*===2004/02/10
  WRITE:  gv NO-GAP, 'Carry foward' NO-GAP.
*-----
  IF p_cumm = 'X'.
    WRITE: gv NO-GAP, ' Cumulative ' NO-GAP.
  ELSE.
    WRITE: gv NO-GAP, '   Residual ' NO-GAP.
  ENDIF.

  WRITE:  gv NO-GAP, 'Release/Plan' NO-GAP,
          gv NO-GAP, '     Actual ' NO-GAP,
          gv NO-GAP, ' Commitment ' NO-GAP,
          gv NO-GAP, ' Var(%)'      NO-GAP,
          gv NO-GAP.
*---2004/02/10
*  if p_amtype = '1'.
  IF p_type = '1'.
    WRITE:             '    Current ' NO-GAP,
            gv NO-GAP.
*  elseif p_amtype = '2'.
  ELSEIF p_type = '2'.
    WRITE:             '   Original ' NO-GAP,
            gv NO-GAP, ' Supplement ' NO-GAP,
            gv NO-GAP, '   Transfer ' NO-GAP,
            gv NO-GAP, '     Return ' NO-GAP,
            gv NO-GAP.
*  elseif p_amtype = '9'.
  ELSEIF p_type = '3'.
    WRITE: '    Payment ' NO-GAP, gv NO-GAP.
    WRITE: 'DownPayment ' NO-GAP, gv NO-GAP.
  ENDIF.
  PERFORM write_uline.
  FORMAT COLOR COL_NORMAL.


ENDFORM.                    " top_of_page
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
FORM display_data.

  LOOP AT itab.
    PERFORM display_line USING 'L'.

    AT END OF key.
      IF p_thonly = space.
        SUM.
        PERFORM display_line USING 'S'.
        PERFORM write_uline.
      ENDIF.
    ENDAT.

    AT LAST.
      IF p_thonly = space.
        SUM.
        PERFORM write_uline.
        PERFORM display_line  USING 'S'.
      ENDIF.
      PERFORM write_uline.
    ENDAT.
  ENDLOOP.

  CLEAR itab.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  display_line
*&---------------------------------------------------------------------*
FORM display_line USING l_type.
  DATA: c_amt  LIKE itab-lamt,
        ccamt  LIKE itab-lamt,
        bbamt  LIKE itab-lamt,
        l_pct  TYPE i,
        l_pcts LIKE raip1-prozu.

* current or c/f budget amount (just sum)
  IF p_cumm = 'X'.
    ccamt = itab-camtc.  "cumulative
    bbamt = itab-lamt + itab-tslvt.
  ELSE.
    IF itab-profil CA 'MQHY'.
      ccamt = itab-zamt.  "residual
    ELSE.
      CLEAR ccamt.
    ENDIF.
    bbamt = itab-lamt + ccamt + itab-tslvt.
  ENDIF.

* variance %
  IF bbamt <= 0.
    l_pct = 0.
  ELSE.
    l_pct = -100 * ( itab-aamt + itab-mamt ) / bbamt.

* for protect dump...
    IF l_pct  > 999.
      l_pcts = 999.
    ELSEIF l_pct < -999.
      l_pcts = -999.
    ELSE.
      l_pcts = l_pct.
    ENDIF.
  ENDIF.

  IF p_thonly = 'X' AND l_pct < p_thred.
    EXIT.
  ENDIF.

* commitment item text
  READ TABLE ifmci  WITH KEY fipos = itab-fipos.
  IF sy-subrc <> 0. CLEAR ifmci-bezei . ENDIF.

* switch amount sign (actual, commitment)
*    itab-aamt   = itab-aamt * -1.
*    itab-mamt   = itab-mamt * -1.

  IF l_type = 'S'.
    WRITE:/ gv NO-GAP, ' ' NO-GAP.
    FORMAT INTENSIFIED ON.
  ELSE.
    WRITE:/ gv NO-GAP, itab-mark AS CHECKBOX NO-GAP.
    FORMAT INTENSIFIED OFF.
    HIDE: itab-fictr, itab-fipos, itab-geber, itab-profil.
  ENDIF.

  FORMAT COLOR COL_KEY.
  WRITE:  gv NO-GAP, itab-fictr(6)       NO-GAP,
          gv NO-GAP, itab-fipos(6)       NO-GAP,
          gv NO-GAP, ifmci-bezei(14)     NO-GAP,
          gv NO-GAP, itab-geber(10)      NO-GAP,
          gv NO-GAP, itab-profil(1)      NO-GAP.

*==2004/02/10
  FORMAT COLOR COL_NORMAL.
  WRITE:
    gv NO-GAP, (12) itab-tslvt       ROUND p_r DECIMALS p_d NO-GAP.
*----------*
  WRITE:
    gv NO-GAP, (12) ccamt       ROUND p_r DECIMALS p_d NO-GAP,
    gv NO-GAP, (12) itab-lamt   ROUND p_r DECIMALS p_d NO-GAP.

* actual
  FORMAT COLOR COL_GROUP.
  WRITE:
    gv NO-GAP, (12) itab-aamt   ROUND p_r DECIMALS p_d NO-GAP.

* commitment
  IF itab-mamt = 0.
    FORMAT COLOR COL_BACKGROUND.
  ELSE.
    FORMAT COLOR COL_POSITIVE.
  ENDIF.
  WRITE:
    gv NO-GAP, (12) itab-mamt   ROUND p_r DECIMALS p_d NO-GAP.

* variance %
*  bbamt = itab-lamt + ccamt.
*  if bbamt <= 0.
*    write: gv no-gap, (7) '-' right-justified no-gap, gv no-gap.
*  else.
*    l_pct = -100 * ( itab-aamt + itab-mamt ) / bbamt.

  IF l_pct > p_thred.
    FORMAT COLOR COL_NEGATIVE.
  ELSE.
    FORMAT COLOR COL_BACKGROUND.
  ENDIF.
  IF bbamt <= 0.
    WRITE: gv NO-GAP, (7) '-' RIGHT-JUSTIFIED NO-GAP, gv NO-GAP.
  ELSE.
    WRITE: gv NO-GAP, l_pcts DECIMALS 0 NO-GAP, '%' NO-GAP,
           gv NO-GAP.
  ENDIF.

*---2004/02/10
*  if p_amtype = '1'.
  IF p_type = '1'.
    FORMAT COLOR COL_BACKGROUND.
    WRITE:       (12) itab-camt   ROUND p_r DECIMALS p_d NO-GAP,
      gv NO-GAP.

*  elseif p_amtype = '2'.
  ELSEIF p_type = '2'.
    FORMAT COLOR COL_BACKGROUND.
    WRITE:
                 (12) itab-oamt   ROUND p_r DECIMALS p_d NO-GAP,
      gv NO-GAP, (12) itab-samt   ROUND p_r DECIMALS p_d NO-GAP,
      gv NO-GAP, (12) itab-tamt   ROUND p_r DECIMALS p_d NO-GAP,
      gv NO-GAP, (12) itab-ramt   ROUND p_r DECIMALS p_d NO-GAP,
      gv NO-GAP.

* payment
*  elseif p_amtype = '9'.
  ELSEIF p_type = '3'.
    FORMAT COLOR COL_BACKGROUND.
    WRITE: (12) itab-pamt   ROUND p_r DECIMALS p_d NO-GAP,
           gv NO-GAP, (12) itab-damt   ROUND p_r DECIMALS p_d NO-GAP,
           gv NO-GAP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
FORM read_data.
*  DATA: $IX    LIKE SY-TABIX VALUE 3,             "..Data Display Line
*        $INDEX LIKE SY-INDEX.                     "..Table Index
*
*  CLEAR:WK_SUTAK[],WK_SUTAK.
*  DESCRIBE TABLE itab LINES SY-TFILL.
*
*  DO SY-TFILL TIMES.
*    ADD 1 TO: $IX, $INDEX.
*
*    CLEAR:itab.
*    READ LINE $IX  FIELD VALUE itab-MARK.
*    CHECK SY-SUBRC EQ 0.
*    CHECK itab-MARK EQ 'X'.
*
*    READ TABLE itab INDEX $INDEX.
*    CHECK SY-SUBRC EQ 0 AND itab-Z_CHKFLAG EQ SPACE. "..????
*
*    MOVE-CORRESPONDING itab TO WK_SUTAK.
*    APPEND WK_SUTAK. CLEAR:WK_SUTAK.
*  ENDDO.
*
*  IF WK_SUTAK[] IS INITIAL.
*    MESSAGE E006.
*  ENDIF.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM popup_to_confirm CHANGING fp_answer.
  DATA: l_defaultoption, l_textline1(70),  l_textline2(70).

  CLEAR fp_answer.
  l_defaultoption = 'N'.
  CASE sy-ucomm.
    WHEN 'EXEC'.
      l_textline1     = text-002.
      l_textline2     = text-003.
  ENDCASE.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            defaultoption = l_defaultoption
            textline1     = l_textline1
            textline2     = l_textline2
            titel         = sy-title
       IMPORTING
            answer        = fp_answer.

ENDFORM.                    " POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  EXEC_DATA
*&---------------------------------------------------------------------*
FORM exec_data.
  DATA: answer.

  PERFORM read_data.
  PERFORM popup_to_confirm CHANGING answer.
  CHECK answer EQ 'J'.

*  PERFORM EXEC_BDC.

*  PERFORM REFRESH_DATA.
ENDFORM.                    " EXEC_DATA

*    WHEN 'EXEC'. PERFORM EXEC_DATA.          "..??
*    WHEN 'SALL'. PERFORM CHECK_MARK_FIELD USING 'X'.
*    WHEN 'DALL'. PERFORM CHECK_MARK_FIELD USING ' '.
*&---------------------------------------------------------------------*
*&      Form  check_mark_field
*&---------------------------------------------------------------------*
FORM check_mark_field USING    p_mark.
  DATA: $ix LIKE sy-tabix.

  LOOP AT itab.
    $ix = sy-tabix.
    itab-mark = p_mark.
    MODIFY itab INDEX $ix.
  ENDLOOP.

  sy-lsind = sy-lsind - 1.
  PERFORM display_data.
ENDFORM.                    " check_mark_field
*&---------------------------------------------------------------------*
*&      Form  write_uline
*&---------------------------------------------------------------------*
FORM write_uline.
*--2004/02/10
*  case p_amtype.
  CASE p_type.
    WHEN '1'.
*      WRITE:/(118) SY-ULINE.
      WRITE:/(131) sy-uline.
    WHEN '2'.
*      WRITE:/(157) SY-ULINE.
      WRITE:/(170) sy-uline.
    WHEN '3'.
*      WRITE:/(131) SY-ULINE.
      WRITE:/(144) sy-uline.
    WHEN OTHERS.
*      WRITE:/(105) SY-ULINE.
      WRITE:/(118) sy-uline.
  ENDCASE.
ENDFORM.                    " write_uline
*&---------------------------------------------------------------------*
*&      Form  switch_sign
*&---------------------------------------------------------------------*
FORM switch_sign.
  ifmit-tslvt = -1 * ifmit-tslvt.
  ifmit-tsl01 = -1 * ifmit-tsl01.
  ifmit-tsl02 = -1 * ifmit-tsl02.
  ifmit-tsl03 = -1 * ifmit-tsl03.
  ifmit-tsl04 = -1 * ifmit-tsl04.
  ifmit-tsl05 = -1 * ifmit-tsl05.
  ifmit-tsl06 = -1 * ifmit-tsl06.
  ifmit-tsl07 = -1 * ifmit-tsl07.
  ifmit-tsl08 = -1 * ifmit-tsl08.
  ifmit-tsl09 = -1 * ifmit-tsl09.
  ifmit-tsl10 = -1 * ifmit-tsl10.
  ifmit-tsl11 = -1 * ifmit-tsl11.
  ifmit-tsl12 = -1 * ifmit-tsl12.
ENDFORM.                    " switch_sign
*&---------------------------------------------------------------------*
*&      Form  cal_cum_budget
*&---------------------------------------------------------------------*
FORM cal_cum_budget USING    pl_current
                    CHANGING pl_amt.

  DATA: lcamt LIKE bppe-wtp01.

  CLEAR lcamt.
  IF pl_current = 'X'.
    IF p_per >=  1. lcamt = lcamt + ibppe-wtp01.  ENDIF.
    IF p_per >=  2. lcamt = lcamt + ibppe-wtp02.  ENDIF.
    IF p_per >=  3. lcamt = lcamt + ibppe-wtp03.  ENDIF.
    IF p_per >=  4. lcamt = lcamt + ibppe-wtp04.  ENDIF.
    IF p_per >=  5. lcamt = lcamt + ibppe-wtp05.  ENDIF.
    IF p_per >=  6. lcamt = lcamt + ibppe-wtp06.  ENDIF.
    IF p_per >=  7. lcamt = lcamt + ibppe-wtp07.  ENDIF.
    IF p_per >=  8. lcamt = lcamt + ibppe-wtp08.  ENDIF.
    IF p_per >=  9. lcamt = lcamt + ibppe-wtp09.  ENDIF.
    IF p_per >= 10. lcamt = lcamt + ibppe-wtp10.  ENDIF.
    IF p_per >= 11. lcamt = lcamt + ibppe-wtp11.  ENDIF.
    IF p_per >= 12. lcamt = lcamt + ibppe-wtp12.  ENDIF.
  ELSE.
    IF p_per >  1. lcamt = lcamt + ibppe-wtp01.  ENDIF.
    IF p_per >  2. lcamt = lcamt + ibppe-wtp02.  ENDIF.
    IF p_per >  3. lcamt = lcamt + ibppe-wtp03.  ENDIF.
    IF p_per >  4. lcamt = lcamt + ibppe-wtp04.  ENDIF.
    IF p_per >  5. lcamt = lcamt + ibppe-wtp05.  ENDIF.
    IF p_per >  6. lcamt = lcamt + ibppe-wtp06.  ENDIF.
    IF p_per >  7. lcamt = lcamt + ibppe-wtp07.  ENDIF.
    IF p_per >  8. lcamt = lcamt + ibppe-wtp08.  ENDIF.
    IF p_per >  9. lcamt = lcamt + ibppe-wtp09.  ENDIF.
    IF p_per > 10. lcamt = lcamt + ibppe-wtp10.  ENDIF.
    IF p_per > 11. lcamt = lcamt + ibppe-wtp11.  ENDIF.
  ENDIF.

  pl_amt = lcamt.
ENDFORM.                    " cal_cum_budget
*&---------------------------------------------------------------------*
*&      Form  display_fm_doc
*&---------------------------------------------------------------------*
FORM display_fm_doc.

*  SELECT l~belnr l~buzei l~wtjhr l~objnr l~posit l~geber
*         h~sgtext l~vorga
*                     INTO TABLE it_bpej
*                     FROM bpej as l inner join bpbk as h
*                       ON l~belnr  = h~belnr
*                     WHERE l~gjahr = p_gjahr
*                       and h~bldat in p_datum
*                       and l~WRTTP = '43'      "no budget release
*                       AND l~posit > 'FP000001'
*                       AND l~vorga IN ('KBUD', 'KBN0', 'KBR0',
*                                       'KBUE', 'KBUS').

ENDFORM.                    " display_fm_doc
*&---------------------------------------------------------------------*
*&      Form  i01_initialization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM i01_initialization.
  p_cb1 =  'X'.
*   P_CB2 =  'X'.

  CONCATENATE icon_expand:   text-001 INTO pushb_o1.
  CONCATENATE icon_collapse: text-001 INTO pushb_c1.

ENDFORM.                    " i01_initialization
*&---------------------------------------------------------------------*
*&      Form  i01_control_sel_screen_pbo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM i01_control_sel_screen_pbo.
  LOOP AT SCREEN.
    PERFORM close_block USING p_cb1 'MC1' space.
    PERFORM close_block USING p_cb1 'MO1' 'X'.
    PERFORM close_block USING p_cb2 'MC2' space.
    PERFORM close_block USING p_cb2 'MO2' 'X'.

  ENDLOOP.


  CASE g_sscr_ucomm.

    WHEN 'UCOMM_O1'.                   "Open Block 1
      SET CURSOR FIELD 'P_comm1'.

    WHEN 'UCOMM_O2'.                   "Open Block 2
      SET CURSOR FIELD 'P_comm2'.

    WHEN 'UCOMM_O3'.                   "Open Block 3
      SET CURSOR FIELD 'P_TEST'.

    WHEN 'UCOMM_O4'.                   "Open Block 4
      SET CURSOR FIELD 'P_ALCUR'.

    WHEN 'UCOMM_O5'.                   "Open Block 5
      SET CURSOR FIELD 'P_SENDER'.

    WHEN 'UCOMM_O6'.                   "Open Block 6
      SET CURSOR FIELD 'P_FILE'.

    WHEN 'UCOMM_O7'.                   "Open Block 7
      SET CURSOR FIELD 'P_UPGM'.

    WHEN OTHERS.
      SET CURSOR FIELD g_cursor_field.
  ENDCASE.

ENDFORM.                    " i01_control_sel_screen_pbo
*&---------------------------------------------------------------------*
*&      Form  close_block
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_CB1  text
*      -->P_3542   text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM close_block USING    value(u_close_block) LIKE p_cb1
                          u_modify_id LIKE screen-group1
                          u_convert.

  IF NOT u_convert IS INITIAL.
    IF  u_close_block IS INITIAL.
      u_close_block = 'X'.
    ELSE.
      CLEAR u_close_block.
    ENDIF.
  ENDIF.

  IF ( screen-group1 = u_modify_id ) AND
     ( NOT u_close_block IS INITIAL ).
    screen-active = '0'.
    MODIFY SCREEN.
  ENDIF.
ENDFORM.                    " close_block
*&---------------------------------------------------------------------*
*&      Form  i01_control_sel_screen_pai
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM i01_control_sel_screen_pai.
  GET CURSOR FIELD g_cursor_field.

  g_sscr_ucomm = sscrfields-ucomm.


* react to User-commands :
  CASE sscrfields-ucomm.

    WHEN 'UCOMM_O1'.                   "Open Block 1
      wa_bu_chk = 'X'.
      CLEAR p_cb1.
    WHEN 'UCOMM_C1'.                   "Close Block 1
      wa_bu_chk = ' '.
      p_cb1 = 'X'.
    WHEN 'UCOMM_O2'.                   "Open Block 2
      CLEAR p_cb2.
    WHEN 'UCOMM_C2'.                   "Close Block 2
      p_cb2 = 'X'.

  ENDCASE.

ENDFORM.                    " i01_control_sel_screen_pai
