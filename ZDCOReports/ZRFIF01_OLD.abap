*&--------------------------------------------------------------------
*& Author                 : Andy Choi
*& Creation Date          : 23/10/2003
*& Specification By       : Andy Choi
*& Pattern                : Report 1-1
*& Development Request No : UD1K903288
*& Addl documentation     :1
*& Description  : list for print out slip
*&                This is developed use ALV.(Not Yet)
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*& 02/25/2005 wskim        UD1K914649     budget report problem
*& 03/04/2005 andy         UD1K914803     budget report problem
*& 01/11/2005 Manjunath    UD1K914803     Program corrections
*& 01/17/2006 Manjunath    UD1K918947     Change Current calculation
*&                                        current=Carryforward+orginal
*&                                        +transfer+Supplement+Return
*&                                        / change display order
*& 01/20/2006 Manjunath    UD1K918978     Program changes to provide
*&                                        option not display any
*&                                        values under C/F column
*&                                        expect C/F,if budget is C/F.
*&--------------------------------------------------------------------
*
* Maintain user own data (SU3)
*   ZCOLV1	USR, MGR, ADM
*
*
REPORT zrfif01 MESSAGE-ID zmfi
               LINE-SIZE 220
               LINE-COUNT 65
               NO STANDARD PAGE HEADING.

INCLUDE <icon>.

*copy program : WISP_LOESCHE_LAYOUT

TABLES: fmfpo, fmfpot,    "commitment
        fmfctr, fmfctrt,  "fund center
        fmhictr,          "fund center hier
        bppe.
TABLES: fmep,fmsu, aufk.
TABLES: fkrs, fpos, tbpfm.
DATA: it_bppe LIKE bppe OCCURS 0 WITH HEADER LINE.

*actual
TABLES: fmit.
DATA: it_fmit LIKE fmit OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_tab OCCURS 0,
         key(17) TYPE c,
         fictr   LIKE fmfctr-fictr, "Fund Cneter
         knzaepo LIKE fmfpo-knzaepo, "category
         fipos   LIKE fmfpo-fipos,  "Commitment Item
         geber   LIKE bppe-geber,   "Fund
         bezeich LIKE fmfpot-bezeich,
         vor(2)  TYPE c,             "budget activity
         tslvt   LIKE bppe-wtp01,    "carryfoward ?
         wtp00   LIKE bppe-wtp01,
         wtp01   LIKE bppe-wtp01,
         wtp02   LIKE bppe-wtp02,
         wtp03   LIKE bppe-wtp03,
         wtp04   LIKE bppe-wtp04,
         wtp05   LIKE bppe-wtp05,
         wtp06   LIKE bppe-wtp06,
         wtp07   LIKE bppe-wtp07,
         wtp08   LIKE bppe-wtp08,
         wtp09   LIKE bppe-wtp09,
         wtp10   LIKE bppe-wtp10,
         wtp11   LIKE bppe-wtp11,
         wtp12   LIKE bppe-wtp12,
         profil  LIKE tbpfm-profil,
*         posit   LIKE fmfpo-posit,
      END OF it_tab.

DATA: BEGIN OF it_temp OCCURS 0,
         key(17) TYPE c,
         fictr   LIKE fmfctr-fictr, "Fund Cneter
         knzaepo LIKE fmfpo-knzaepo, "category
         fipos   LIKE fmfpo-fipos,  "Commitment Item
         geber   LIKE bppe-geber,   "Fund
         bezeich LIKE fmfpot-bezeich,
      END OF it_temp.

DATA: BEGIN OF it_sort1 OCCURS 0,
         fictr   LIKE fmfctr-fictr,  "Fund Cneter
         knzaepo LIKE fmfpo-knzaepo, "category
         fipos   LIKE fmfpo-fipos,
         vor(2)  TYPE c,             "budget activity
         tslvt   LIKE bppe-wtp01,
         wtp00   LIKE bppe-wtp01,
         wtp01   LIKE bppe-wtp01,
         wtp02   LIKE bppe-wtp02,
         wtp03   LIKE bppe-wtp03,
         wtp04   LIKE bppe-wtp04,
         wtp05   LIKE bppe-wtp05,
         wtp06   LIKE bppe-wtp06,
         wtp07   LIKE bppe-wtp07,
         wtp08   LIKE bppe-wtp08,
         wtp09   LIKE bppe-wtp09,
         wtp10   LIKE bppe-wtp10,
         wtp11   LIKE bppe-wtp11,
         wtp12   LIKE bppe-wtp12,
         profil  LIKE tbpfm-profil,
         posit   LIKE fmfpo-posit,
      END OF it_sort1.

DATA: BEGIN OF it_sort2 OCCURS 0,
         key(17) TYPE c,
         fipos   LIKE fmfpo-fipos,  "Commitment Item
         fictr   LIKE fmfctr-fictr, "Fund Cneter
         knzaepo LIKE fmfpo-knzaepo, "category
         vor(2)  TYPE c,             "budget activity
         tslvt   LIKE bppe-wtp01,
         wtp00   LIKE bppe-wtp01,
         wtp01   LIKE bppe-wtp01,
         wtp02   LIKE bppe-wtp02,
         wtp03   LIKE bppe-wtp03,
         wtp04   LIKE bppe-wtp04,
         wtp05   LIKE bppe-wtp05,
         wtp06   LIKE bppe-wtp06,
         wtp07   LIKE bppe-wtp07,
         wtp08   LIKE bppe-wtp08,
         wtp09   LIKE bppe-wtp09,
         wtp10   LIKE bppe-wtp10,
         wtp11   LIKE bppe-wtp11,
         wtp12   LIKE bppe-wtp12,
         profil  LIKE tbpfm-profil,
*         posit   LIKE fmfpo-posit,
      END OF it_sort2.

DATA: BEGIN OF it_sort3 OCCURS 0,
         key(17) TYPE c,
         geber   LIKE bppe-geber,   "Fund
         fictr   LIKE fmfctr-fictr, "Fund Cneter
         vor(2)  TYPE c,             "budget activity
         tslvt   LIKE bppe-wtp01,
         wtp00   LIKE bppe-wtp01,
         wtp01   LIKE bppe-wtp01,
         wtp02   LIKE bppe-wtp02,
         wtp03   LIKE bppe-wtp03,
         wtp04   LIKE bppe-wtp04,
         wtp05   LIKE bppe-wtp05,
         wtp06   LIKE bppe-wtp06,
         wtp07   LIKE bppe-wtp07,
         wtp08   LIKE bppe-wtp08,
         wtp09   LIKE bppe-wtp09,
         wtp10   LIKE bppe-wtp10,
         wtp11   LIKE bppe-wtp11,
         wtp12   LIKE bppe-wtp12,
         profil  LIKE tbpfm-profil,
*         posit   LIKE fmfpo-posit,
      END OF it_sort3.


DATA: BEGIN OF it_dum OCCURS 0,
         key(17) TYPE c,
         fictr   LIKE fmfctr-fictr,
         knzaepo LIKE fmfpo-knzaepo, "category
         fipos   LIKE fmfpo-fipos,
         geber   LIKE bppe-geber,
         bezeich LIKE fmfpot-bezeich,
         vor(2)  TYPE c,             "budget activity
         tslvt   LIKE bppe-wtp01,
         wtp00   LIKE bppe-wtp01,
         wtp01   LIKE bppe-wtp01,
         wtp02   LIKE bppe-wtp02,
         wtp03   LIKE bppe-wtp03,
         wtp04   LIKE bppe-wtp04,
         wtp05   LIKE bppe-wtp05,
         wtp06   LIKE bppe-wtp06,
         wtp07   LIKE bppe-wtp07,
         wtp08   LIKE bppe-wtp08,
         wtp09   LIKE bppe-wtp09,
         wtp10   LIKE bppe-wtp10,
         wtp11   LIKE bppe-wtp11,
         wtp12   LIKE bppe-wtp12,
         profil  LIKE tbpfm-profil,
*         posit   LIKE fmfpo-posit,
      END OF it_dum.

DATA: BEGIN OF it_down OCCURS 0,
         fictr(6), "LIKE fmfctr-fictr,
         fipos(10), "   LIKE fmfpo-fipos,
         bezeich(20), " LIKE fmfpot-bezeich,
         geber(10), "   LIKE bppe-geber,
         profil(1),
         act(20),
*        vor(2)  TYPE c,             "budget activity
         wtp00 LIKE bppe-wtp01, "E P,
         tslvt LIKE bppe-wtp01, "E P,
         wtp01 LIKE bppe-wtp01, "E P,
         wtp02(15) TYPE p,
         wtp03(15) TYPE p,
         wtp04(15) TYPE p,
         wtp05(15) TYPE p,
         wtp06(15) TYPE p,
         wtp07(15) TYPE p,
         wtp08(15) TYPE p,
         wtp09(15) TYPE p,
         wtp10(15) TYPE p,
         wtp11(15) TYPE p,
         wtp12(15) TYPE p,
*         profil  LIKE tbpfm-profil,
*         posit   LIKE fmfpo-posit,
      END OF it_down.

DATA: BEGIN OF it_head OCCURS 0,
         text(16),
      END OF it_head.

DATA : BEGIN OF it_fmfctr OCCURS 0,
         ctr_objnr  LIKE fmfctr-ctr_objnr,
         fictr      LIKE fmfctr-fictr,
         parent_obj LIKE fmhictr-parent_obj,
       END OF it_fmfctr.

DATA : BEGIN OF it_fmfpo OCCURS 0,
         fipos    LIKE fmfpo-fipos,
         bezeich  LIKE fmfpot-bezeich,
         posit    LIKE fmep-posit,
       END OF it_fmfpo.

* Sam file Layout
DATA : BEGIN OF rec OCCURS 10,
             geber LIKE fmps-geber,   "fund
             fistl LIKE bpfmps-fistl, "fund center
             fipos LIKE bpfmps-fipos, "commitment
             wert(15) TYPE c,         "amt
        END OF rec.
DATA: BEGIN OF iftab OCCURS 0,
         fictr     LIKE fmfctr-fictr,
         fipos     LIKE fmfpo-fipos,
         bezeich   LIKE fmfpot-bezeich,
         geber     LIKE bppe-geber,
         wtp01(15) TYPE c,
         wtp02(15) TYPE c,
         wtp03(15) TYPE c,
         wtp04(15) TYPE c,
         wtp05(15) TYPE c,
         wtp06(15) TYPE c,
         wtp07(15) TYPE c,
         wtp08(15) TYPE c,
         wtp09(15) TYPE c,
         wtp10(15) TYPE c,
         wtp11(15) TYPE c,
         wtp12(15) TYPE c,
      END OF iftab.

* Active availability control on commitment budget
DATA: BEGIN OF fmctl OCCURS 0,
         fictr     LIKE fmfctr-fictr,
         fipos     LIKE fmfpo-fipos,
         geber     LIKE bppe-geber,
         profil    LIKE tbpfm-profil,
      END OF fmctl.
DATA : it_bdc      LIKE bdcdata OCCURS 0 WITH HEADER LINE.
*---work.
DATA : wa_wtp00   LIKE bppe-wtp01,
       wa_tslvt   LIKE bppe-wtp01,
       wa_wtp01   LIKE bppe-wtp01,
       wa_wtp02   LIKE bppe-wtp01,
       wa_wtp03   LIKE bppe-wtp01,
       wa_wtp04   LIKE bppe-wtp01,
       wa_wtp05   LIKE bppe-wtp01,
       wa_wtp06   LIKE bppe-wtp01,
       wa_wtp07   LIKE bppe-wtp01,
       wa_wtp08   LIKE bppe-wtp01,
       wa_wtp09   LIKE bppe-wtp01,
       wa_wtp10   LIKE bppe-wtp01,
       wa_wtp11   LIKE bppe-wtp01,
       wa_wtp12   LIKE bppe-wtp01,
       wa_key(17) TYPE c,
       wa_fictr   LIKE fmfctr-fictr,
       wa_knzaepo LIKE fmfpo-knzaepo, "category
       wa_fipos   LIKE fmfpo-fipos,
       wa_geber   LIKE bppe-geber,
       wa_bezeich LIKE fmfpot-bezeich,
       wa_vor(2)  TYPE c.             "budget activity

* for combobox
TYPE-POOLS: vrm.
DATA: it_rt TYPE vrm_values,
      w_rt_line LIKE LINE OF it_rt.

DATA: it_val TYPE vrm_values,
      w_line LIKE LINE OF it_val.

RANGES : r_fictr     FOR fmfctr-fictr,
         r_fipos     FOR fmfpo-fipos.

SELECTION-SCREEN BEGIN OF BLOCK sb WITH FRAME TITLE c010.
*p_buk  like fmit-rbukrs memory id BUK obligatory,
PARAMETERS :
             p_fik  LIKE fmps-fikrs  MEMORY ID fik OBLIGATORY
                                       DEFAULT 'H201',  "FM area
             p_gjr  LIKE bpdy-jahr   MEMORY ID gjr OBLIGATORY
                                       DEFAULT sy-datum+0(4).
SELECTION-SCREEN END OF BLOCK sb.
* WRTTP: 43 - current, 46 - release
* VORGA: kbud - origin, kbn0 - supp, kbr0 - return, kbus - transfer
*        kbfr - release
SELECTION-SCREEN BEGIN OF BLOCK sl WITH FRAME TITLE c020.
SELECT-OPTIONS: p_fictr FOR fmfctr-fictr,  "Fund Center
                p_fipos FOR fmfpo-fipos,   "Commitment Item
                p_geber FOR bppe-geber,    "Fund
                p_prof  FOR tbpfm-profil,  "default 'B' option NE
                p_knz   FOR fmfpo-knzaepo DEFAULT '3'.  " item category

SELECT-OPTIONS: p_auf   FOR aufk-aufnr.    "Order
*----
PARAMETERS: p_rt(1) TYPE c AS LISTBOX VISIBLE LENGTH 25 OBLIGATORY,

*parameters: p_bdt   as checkbox, "budget
*            p_bdact as checkbox, "budget activity
            p_ver  LIKE bppe-versn DEFAULT '000' NO-DISPLAY.
*
SELECTION-SCREEN END OF BLOCK sl.

SELECTION-SCREEN BEGIN OF BLOCK dl WITH FRAME TITLE c030.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) c_scale.
SELECTION-SCREEN POSITION 33.
PARAMETERS: p_r LIKE rfpdo1-ts70skal  DEFAULT '0'.
SELECTION-SCREEN COMMENT 35(1) c_slash.
*selection-screen position 37.
*--decimals
PARAMETERS: p_d LIKE rfpdo1-ts70skal  DEFAULT '0'.
*SELECTION-SCREEN COMMENT 39(1) c_slash2.
*selection-screen position 41.
** 1 - commitment
** 2 - invoice
** 3 - commitment + invoice
** 9 - payment
*parameters: p_amtype(1) type c default '3'.
SELECTION-SCREEN END OF LINE.
PARAMETERS: p_act(1) TYPE c AS LISTBOX VISIBLE LENGTH 25 OBLIGATORY,
            P_CF as checkbox default 'X'.

SELECTION-SCREEN END OF BLOCK dl.



PARAMETERS : p_file LIKE rlgrap-filename DEFAULT
   'c:\temp\fmactual.xls'.

DATA: g_per(2)  TYPE n.
DATA: g_bldat LIKE bpdy-bldat,
      g_subrc LIKE sy-subrc.

TABLES: t100.

* for function
DATA: g_func(1) TYPE c.

* Variables
DATA: v_field(20) TYPE c,
      g_history   TYPE c,
      g_pfr       LIKE fmto-perio,
      g_pto       LIKE fmto-perio.

*user run level
DATA: e_level(3) TYPE c.

*---------------------------------------------------------------------*
* Initialization.
*---------------------------------------------------------------------*
INITIALIZATION.
* for combo box
*---report type
  w_rt_line-key  = '1'.
  w_rt_line-text = 'Budget'.
  APPEND w_rt_line TO it_rt.
  w_rt_line-key  = '2'.
  w_rt_line-text = 'Commitment'.
  APPEND w_rt_line TO it_rt.
  w_rt_line-key  = '3'.
  w_rt_line-text = 'Invoice'.
  APPEND w_rt_line TO it_rt.
  w_rt_line-key  = '4'.
  w_rt_line-text = 'Invoice+Commitment'.
  APPEND w_rt_line TO it_rt.
  w_rt_line-key  = '5'.
  w_rt_line-text = 'Payment'.
  APPEND w_rt_line TO it_rt.
*--All
  w_rt_line-key  = '6'.
  w_rt_line-text = 'All'.
  APPEND w_rt_line TO it_rt.
  p_rt = '6'.
*------sort
  w_line-key  = '1'.
  w_line-text = 'Sort by Fund Center'.
  APPEND w_line TO it_val.
  w_line-key  = '2'.
  w_line-text = 'Sort by Commitment Item'.
  APPEND w_line TO it_val.
  w_line-key  = '3'.
  w_line-text = 'Sort by Fund'.
  APPEND w_line TO it_val.
  p_act = '3'.

  w_line-key  = '4'.
  w_line-text = 'Sort by Item'.
  APPEND w_line TO it_val.
  p_act = '4'.
* Begin of changes - UD1K918947
  w_line-key  = '5'.
  w_line-text = 'Sort by Activity'.
  APPEND w_line TO it_val.
  p_act = '5'.
* End of changes - UD1K918947


* initial screen text
  c_scale  = 'Scal/Decimal'.
*  c_slash = '/'.
*  c_slash2 = '/'.

*  c_text = 'Carry Forward Budget'.

  c010 = 'Run Parameter'.
  c020 = 'Selection Option'.
  c030 = 'Display'.

  DATA: l_cousertype(3) TYPE c.
  GET PARAMETER ID 'ZCOLV1' FIELD l_cousertype.
  e_level = l_cousertype.

*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT. "on p_rt. "Report type.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = 'P_RT'
            values = it_rt.

*at selection-screen on output.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = 'P_ACT'
            values = it_val.

AT SELECTION-SCREEN.
*---------------------------------------------------------------------
*  S T A R T - O F - S E L E C T I O N
*---------------------------------------------------------------------
START-OF-SELECTION.
  REFRESH it_tab.

* read master
  PERFORM get_master_info.

* read budget
*  if p_bdact = 'X'.
*     p_bdt = 'X'.
*  endif.
*  if p_bdt = 'X'.
*    perform get_bbpe.
*  endif.
  IF p_rt = '1'  OR p_rt = '6'.
    PERFORM get_bbpe.
  ENDIF.
* read actuals
  PERFORM get_fmit.
*==JHS ADD 2003.10.30
  IF p_rt = '6'.
    PERFORM make_tab.
  ENDIF.

  PERFORM sort_it_tab.

*---------------------------------------------------------------------
*  END-OF-SELECTION.
*---------------------------------------------------------------------
END-OF-SELECTION.

  PERFORM display_data.

  SET PF-STATUS 'PF1000'.
*---------------------------------------------------------------------
AT USER-COMMAND.
*---------------------------------------------------------------
  CASE sy-ucomm.
    WHEN 'DOWN'.
      PERFORM data_download.
  ENDCASE.
*----------------------------------------------------------------------*
* AT LINE SELECTION
*----------------------------------------------------------------------*
AT LINE-SELECTION.
  DATA: v_value(100).

  GET CURSOR FIELD v_field  VALUE v_value.
  g_history = 'X'.

  g_pfr = '01'. g_pto = '12'.
  CASE v_field.
*---2004/05/27
    WHEN 'IT_TAB-WTP00'.
      REFRESH : it_bdc.
      CLEAR   : it_bdc.
      PERFORM make_bdc_rtn USING :
                         'X'  'ZRFII18'        '1000',
                         ' '  'R_3'            'X',
                         ' '  'S_FIFCTR-LOW'   it_tab-fictr,
                         ' '  'S_FIPOS-LOW'    it_tab-fipos,
                         ' '  'BDC_OKCODE'      '=ONLI'.
      CALL TRANSACTION 'ZRFII18'  USING   it_bdc
                                  MODE   'E'
                                  UPDATE 'S'.
      EXIT.
*
*      CLEAR : r_fictr[], r_fipos[].
*      MOVE 'I'          TO r_fictr-sign.
*      MOVE 'EQ'         TO r_fictr-option.
*      MOVE it_tab-fictr TO r_fictr-low.
*      APPEND r_fictr.
*
*      MOVE 'I'          TO r_fipos-sign.
*      MOVE 'EQ'         TO r_fipos-option.
*      MOVE it_tab-fipos TO r_fipos-low.
*      APPEND r_fipos.
*
*      SUBMIT zrfii18
*           WITH r_3 = 'X'
*           WITH p_bukrs  = p_fik
**              WITH p_per_fr = g_pfr
**              WITH p_per_to = g_pto
*           WITH s_fictr IN  r_fictr
**              WITH s_fikrs = p_fik
**              WITH s_fincd = it_tab-geber
*           WITH s_fipos IN  r_fipos
*         AND RETURN.
    WHEN 'IT_TAB-WTP01'.  g_pfr = '01'. g_pto = '01'.
    WHEN 'IT_TAB-WTP02'.  g_pfr = '02'. g_pto = '02'.
    WHEN 'IT_TAB-WTP03'.  g_pfr = '03'. g_pto = '03'.
    WHEN 'IT_TAB-WTP04'.  g_pfr = '04'. g_pto = '04'.
    WHEN 'IT_TAB-WTP05'.  g_pfr = '05'. g_pto = '05'.
    WHEN 'IT_TAB-WTP06'.  g_pfr = '06'. g_pto = '06'.
    WHEN 'IT_TAB-WTP07'.  g_pfr = '07'. g_pto = '07'.
    WHEN 'IT_TAB-WTP08'.  g_pfr = '08'. g_pto = '08'.
    WHEN 'IT_TAB-WTP09'.  g_pfr = '09'. g_pto = '09'.
    WHEN 'IT_TAB-WTP10'.  g_pfr = '10'. g_pto = '10'.
    WHEN 'IT_TAB-WTP11'.  g_pfr = '11'. g_pto = '11'.
    WHEN 'IT_TAB-WTP12'.  g_pfr = '12'. g_pto = '12'.
    WHEN 'IT_TAB-GEBER'.
      SET PARAMETER ID 'FIK' FIELD 'H201'.
      SET PARAMETER ID 'FIC' FIELD it_tab-geber.
      CALL TRANSACTION 'FM5S' AND  SKIP FIRST SCREEN.
      EXIT.
  ENDCASE.

  CASE v_value.
    WHEN 'Commitment'.
  ENDCASE.

  CHECK it_tab-fictr <> space.
*  GET PARAMETER ID 'VPE' FIELD l_sav_per.
*  GET PARAMETER ID 'BPE' FIELD l_sav_per.
*  GET PARAMETER ID 'VPF' FIELD l_sav_per.
*  GET PARAMETER ID 'BPF' FIELD l_sav_per.
  SUBMIT rffmto10
          WITH p_fyr_fr = p_gjr
          WITH p_fyr_to = p_gjr
          WITH p_per_fr = g_pfr
          WITH p_per_to = g_pto
          WITH s_fictr = it_tab-fictr
          WITH s_fikrs = p_fik
          WITH s_fincd = it_tab-geber
          WITH s_fipos = it_tab-fipos
        AND RETURN.

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
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_fmfpo
     FROM fmfpo
     WHERE fikrs =  p_fik
       AND fipos IN p_fipos
       AND knzaepo IN p_knz.

* commitment item text
  LOOP AT it_fmfpo.
    SELECT SINGLE bezeich INTO it_fmfpo-bezeich
       FROM fmfpot
       WHERE spras = sy-langu
         AND fikrs = p_fik
         AND fipos = it_fmfpo-fipos.
    MODIFY it_fmfpo.
  ENDLOOP.

* Fund Center
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_fmfctr
     FROM fmfctr
     WHERE fikrs =  p_fik
       AND fictr IN p_fictr.
* Fund Center Hiearchy (select end node only)
  LOOP AT it_fmfctr.
    SELECT SINGLE * FROM fmhictr
       WHERE ctr_objnr = it_fmfctr-ctr_objnr.

    IF fmhictr-parent_obj = space.
      if e_level <> 'ADM'.
        DELETE it_fmfctr.
      endif.
    ELSE.
*-- end node...
*--- check FundCenter Auth.
      SELECT SINGLE * FROM fmfctr
          WHERE fikrs = p_fik
            AND fictr = it_fmfctr-fictr
            AND datbis >= sy-datum.

*      CALL FUNCTION 'FMAU_AUTHORITY_FIFM'
*           EXPORTING
*                I_ACTVT       = FMAU_DISPLAY
*                I_AUTH_OBJECT = 'F_FICA_CTR'
*                I_FIKRS       = p_FIK
*                I_FMFCTR      = fmfctr
*                I_DATE        = SY-DATLO
*           IMPORTING
*                EX_AUTH       = L_BEREC.
*      IF L_BEREC = SPACE.
*        delete it_fmfctr.
*      endif.
      AUTHORITY-CHECK OBJECT 'Z_FICTR'
               ID 'FM_FIKRS'   FIELD p_fik
               ID 'FM_FICTR'   FIELD fmfctr-fictr.
      IF sy-subrc <> 0.
        DELETE it_fmfctr.
      ENDIF.

    ENDIF.
  ENDLOOP.

* Budget Profile
  SELECT * INTO CORRESPONDING FIELDS OF TABLE fmctl
    FROM tbpfm
    WHERE fikrs = p_fik
      AND gjahr = p_gjr.


* Fund Assignment
  DATA: iaufk LIKE aufk OCCURS   0 WITH HEADER LINE.
  DATA: l_t_fmzuob LIKE fmzuob OCCURS   0 WITH HEADER LINE.
  DATA: l_objnr LIKE fmii1-objnr,
        l_line  TYPE i.

  DESCRIBE TABLE p_auf LINES l_line.
  CHECK l_line > 0.
  SELECT * FROM aufk INTO TABLE iaufk
         WHERE aufnr IN p_auf
           AND autyp = '01'.  " internal order

  LOOP AT iaufk.
    CALL FUNCTION 'K_ORDER_READ'
         EXPORTING
              aufnr     = iaufk-aufnr
         IMPORTING
              objnr     = l_objnr
         EXCEPTIONS
              not_found = 1.

    SELECT * FROM fmzuob
        APPENDING TABLE l_t_fmzuob
        WHERE objnr     = l_objnr.
  ENDLOOP.

  p_geber-sign   = 'I'.
  p_geber-option = 'EQ'.
  LOOP AT l_t_fmzuob.
    p_geber-low = l_t_fmzuob-fonds.
    APPEND p_geber.
  ENDLOOP.
ENDFORM.                    " get_master_info
*&---------------------------------------------------------------------*
*&      Form  data_download
*&---------------------------------------------------------------------*
FORM data_download.
* EDIT_TABLE_WITH_EXCEL
* SEND_TABLE_TO_EXCEL
  CLEAR : it_head, it_head[].
  CLEAR : it_down, it_down[].
  it_head-text  = 'FnCtr'.
  APPEND it_head.
  CLEAR  it_head.
  it_head-text = 'Comm item'.
  APPEND it_head.
  CLEAR  it_head.
  it_head-text = 'Description'.
  APPEND it_head.
  CLEAR  it_head.
  it_head-text  = 'Fund'.
  APPEND it_head.
  CLEAR  it_head.
  it_head-text = 'P'.
  APPEND it_head.
  CLEAR  it_head.
  it_head-text  = 'Category'.
  APPEND it_head.
  CLEAR  it_head.
  it_head-text = 'Total'.
  APPEND it_head.
  CLEAR  it_head.
  it_head-text  = 'C/F'.
  APPEND it_head.
  CLEAR  it_head.
  it_head-text  = 'Jan'.
  APPEND it_head.
  CLEAR  it_head.
  it_head-text  = 'Feb'.
  APPEND it_head.
  CLEAR  it_head.
  it_head-text  = 'Mar'.
  APPEND it_head.
  CLEAR  it_head.
  it_head-text  = 'Apr'.
  APPEND it_head.
  CLEAR  it_head.
  it_head-text  = 'May'.
  APPEND it_head.
  CLEAR  it_head.
  it_head-text  = 'Jun'.
  APPEND it_head.
  CLEAR  it_head.
  it_head-text  = 'Jul'.
  APPEND it_head.
  CLEAR  it_head.
  it_head-text  = 'Aug'.
  APPEND it_head.
  CLEAR  it_head.
  it_head-text  = 'Sep'.
  APPEND it_head.
  CLEAR  it_head.
  it_head-text  = 'Oct'.
  APPEND it_head.
  CLEAR  it_head.
  it_head-text  = 'Nov'.
  APPEND it_head.
  CLEAR  it_head.
  it_head-text  = 'Dec'.
  APPEND it_head.
  CLEAR it_head.

  LOOP AT it_tab.
    MOVE-CORRESPONDING it_tab TO it_down.

    CASE it_tab-vor+1(1).
* Begin of changes - UD1K914803
      WHEN 'F'.    WRITE: 'Carry/Forward'  TO it_down-act.
* End of changes - UD1K914803
      WHEN 'O'.    MOVE   'Original     '  TO it_down-act.
      WHEN 'T'.    WRITE: 'Transfer     '  TO it_down-act.
      WHEN 'S'.    WRITE: 'Supplement   '  TO it_down-act.
      WHEN 'R'.    WRITE: 'Return       '  TO it_down-act.
      WHEN 'C'.    WRITE: 'Current      '  TO it_down-act.
      WHEN 'X'.    WRITE: 'Released     '  TO it_down-act.
      WHEN 'Q'.    WRITE: 'Available    '  TO it_down-act.
      WHEN '1'.    WRITE: 'Commitment   '  TO it_down-act.
      WHEN 'I'.    WRITE: 'Invoice      '  TO it_down-act.
      WHEN 'A'.    WRITE: 'Cmmt+Invoice '  TO it_down-act.
      WHEN 'P'.    WRITE: 'Payments     '  TO it_down-act.
      WHEN OTHERS. WRITE: 'Unknown      '  TO it_down-act.
    ENDCASE.
    APPEND it_down.
    CLEAR  it_down.
  ENDLOOP.

  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename   = p_file
            filetype   = 'DAT'
       TABLES
            data_tab   = it_down
            fieldnames = it_head.
*  sy-lisel = sy-lisel - 1.
  WRITE:/ p_file, ' is created...'.
ENDFORM.                    " data_download
*&---------------------------------------------------------------------*
*&      Form  get_fmit
*&---------------------------------------------------------------------*
FORM get_fmit.

  SELECT * FROM fmit
    INTO TABLE it_fmit
    WHERE rfistl IN p_fictr
      AND rfonds IN p_geber
*      AND RFONDS <> ' '
      AND ryear  =  p_gjr
      AND rfipex IN p_fipos.

  LOOP AT it_fmit.
* check auth
    READ TABLE it_fmfctr WITH KEY fictr = it_fmit-rfistl.
    CHECK sy-subrc = 0.

    CLEAR it_tab.

* filter for commitment item
    CHECK it_fmit-rfipex IN p_fipos.
    READ TABLE it_fmfpo  WITH KEY fipos = it_fmit-rfipex.
    CHECK sy-subrc = 0.

    it_tab-fipos = it_fmit-rfipex.
    it_tab-fictr = it_fmit-rfistl.
    it_tab-geber = it_fmit-rfonds.
    it_tab-bezeich = it_fmfpo-bezeich.


    PERFORM get_budget_period USING it_tab-fictr
                                    it_tab-fipos
                                    it_tab-geber
                              CHANGING it_tab-profil.
    CHECK it_tab-profil IN p_prof.

* 50 - p/r, 51 - p/o, 54 - invoice, 57 - payment, 58 - d/p req
* 61 - downpayment
* 95 - co posting (secondary cost posting)
    IF p_rt     = '2'.
      CHECK it_fmit-rwrttp = '50' OR it_fmit-rwrttp = '51'
         OR it_fmit-rwrttp = '60' OR it_fmit-rwrttp = '61'.
      it_tab-vor = 'G1'.  "commitment

      PERFORM move_amount.
*--invoice
    ELSEIF p_rt = '3'.
      CHECK ( it_fmit-rwrttp = '54' OR it_fmit-rwrttp = '66'
* Original
         OR it_fmit-rwrttp = '95' ) AND it_fmit-rbtart = '0100'.
      it_tab-vor = 'HI'.  "invoice
      PERFORM move_amount.
*---comm + invo
    ELSEIF p_rt = '4'.
      IF ( it_fmit-rwrttp = '50' OR it_fmit-rwrttp = '51'
        OR it_fmit-rwrttp = '60' OR it_fmit-rwrttp = '61' )
      OR ( ( it_fmit-rwrttp = '54' OR it_fmit-rwrttp = '66'
          OR it_fmit-rwrttp = '95' ) AND it_fmit-rbtart = '0100' ).
        it_tab-vor = 'IA'.  "actuals
        PERFORM move_amount.
      ENDIF.

** payment
    ELSEIF p_rt = '5'.
      IF ( it_fmit-rbtart = '0250' OR it_fmit-rwrttp = '61' ).  " Paid
*     check it_fmit-RWRTTP = '57' or it_fmit-RWRTTP = '58'.
        it_tab-vor = 'JP'.  "payment
        PERFORM move_amount.
      ENDIF.
*====JHS ADD 2003.10.29
    ELSEIF p_rt = '6'.                       "ALL
      IF it_fmit-rwrttp = '50' OR it_fmit-rwrttp = '51'
         OR it_fmit-rwrttp = '60' OR it_fmit-rwrttp = '61'.
        it_tab-vor = 'G1'.  "commitment
        PERFORM move_amount.
      ELSEIF ( it_fmit-rwrttp = '54' OR it_fmit-rwrttp = '66'
* Original
         OR it_fmit-rwrttp = '95' ) AND it_fmit-rbtart = '0100'.
        it_tab-vor = 'HI'.  "invoice
        PERFORM move_amount.
      ELSEIF ( it_fmit-rbtart = '0250' OR it_fmit-rwrttp = '61' ).
*     check it_fmit-RWRTTP = '57' or it_fmit-RWRTTP = '58'.
        it_tab-vor = 'JP'.  "payment
        PERFORM move_amount.
      ENDIF.
    ENDIF.
*=====JHS ADD 2003.10.31   Carry Foward
    IF it_fmit-rwrttp = '50' OR it_fmit-rwrttp = '51'."
*---2004/0210   invoice except
*---start #1 wskim
*      OR it_fmit-rwrttp = '54'.
*      IF it_fmit-rbtart = '0350'.
      IF it_fmit-rbtart = '0350' OR it_fmit-rbtart = '0150' OR
         it_fmit-rbtart = '0500'.
*---end
        IF p_rt = '1' OR p_rt = '6'.

* Begin of changes -  UD1K914803
          it_tab-vor = 'AO'.                                "UD1K918978
          if P_CF eq ''   .
            MOVE it_fmit-hslvt TO it_tab-tslvt.             "UD1K918978
            it_tab-tslvt =  it_tab-tslvt * -1.              "UD1K918978
          endif.
          MOVE  it_tab-tslvt TO it_tab-wtp00.
          COLLECT it_tab.
* End of changes -    UD1K914803
*======jhs modify 2004/02/07
          it_tab-vor = 'EC'.
          if P_CF eq ''  .
            MOVE it_fmit-hslvt TO it_tab-tslvt.             "UD1K918978
            it_tab-tslvt =  it_tab-tslvt * -1.              "UD1K918978
          endif.
          MOVE  it_tab-tslvt TO it_tab-wtp00.               "UD1K918978
          COLLECT it_tab.
* Begin of changes -  UD1K914803
          it_tab-vor = 'FX'.
          if P_CF eq '' .
            MOVE it_fmit-hslvt TO it_tab-tslvt.             "UD1K918978
            it_tab-tslvt =  it_tab-tslvt * -1.              "UD1K918978
          endif.
          MOVE  it_tab-tslvt TO it_tab-wtp00.
          COLLECT it_tab.
* End  of changes -   UD1K914803
          it_tab-vor = 'G1'.
* Set Commitment Amount as zero for C/F column
          if P_CF eq ''  .
            MOVE it_fmit-hslvt TO it_tab-tslvt.             "UD1K918978
          endif.
          MOVE  it_fmit-tslvt TO it_tab-wtp00.
          COLLECT it_tab.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR it_tab.
  ENDLOOP.
ENDFORM.                    " get_fmit
*&---------------------------------------------------------------------*
*&      Form  get_bbpe
*&---------------------------------------------------------------------*
FORM get_bbpe.
  REFRESH it_tab.

  SELECT * FROM bppe
    INTO TABLE it_bppe
    FOR ALL ENTRIES IN it_fmfctr
    WHERE objnr =  it_fmfctr-ctr_objnr
      AND gjahr =  p_gjr
      AND versn =  p_ver
      AND geber IN p_geber.
*      AND GEBER <> ' '.

  LOOP AT it_bppe.
* check auth
    READ TABLE it_fmfctr WITH KEY ctr_objnr = it_bppe-objnr.
    CHECK sy-subrc = 0.

    it_tab-fictr = it_fmfctr-fictr.
    it_tab-geber = it_bppe-geber.


* filter for commitment item
    READ TABLE it_fmfpo  WITH KEY posit = it_bppe-posit.
    CHECK sy-subrc = 0.
*   check it_tab-fipos in p_fipos.

    it_tab-fipos   = it_fmfpo-fipos.
    it_tab-bezeich = it_fmfpo-bezeich.

    PERFORM get_budget_period USING it_tab-fictr
                                    it_tab-fipos
                                    it_tab-geber
                              CHANGING it_tab-profil.
    CHECK it_tab-profil IN p_prof.


    MOVE-CORRESPONDING it_bppe TO it_tab.

* display budget activity (sort key + activity)
    CASE it_bppe-vorga.
      WHEN 'KBUD'.
        it_tab-vor = 'AO'.  "orgin
        PERFORM cal_tot.
        it_tab-vor = 'EC'.  "Current
        MOVE-CORRESPONDING it_bppe TO it_tab.
        PERFORM cal_tot.
      WHEN 'KBN0'.
        it_tab-vor = 'BS'.  "supp
        PERFORM cal_tot.
* Begin of changes - UD1K914803 / UD1K918947
        it_tab-vor = 'EC'.     "Current
        MOVE-CORRESPONDING it_bppe TO it_tab.
        PERFORM cal_tot.
* End of changes - UD1K914803 / UD1K918947
      WHEN 'KBR0'.
        it_tab-vor = 'DR'.   "return
        PERFORM cal_tot.
* Begin of changes - UD1K914803 / UD1K918947
        it_tab-vor = 'EC'.   "Current
        MOVE-CORRESPONDING it_bppe TO it_tab.
        PERFORM cal_tot.
* End of changes - UD1K914803 / UD1K918947
      WHEN 'KBUS'.
        it_tab-vor = 'CT'.   "transfer
        PERFORM cal_tot.
        it_tab-vor = 'EC'.   "Current
        MOVE-CORRESPONDING it_bppe TO it_tab.
        PERFORM cal_tot.
      WHEN 'KBUE'.
        it_tab-vor = 'CT'.   "transfer
        PERFORM cal_tot.
        it_tab-vor = 'EC'.    "Current
        MOVE-CORRESPONDING it_bppe TO it_tab.
        PERFORM cal_tot.
      WHEN 'KBFR'.
        it_tab-vor = 'FX'.  "release
        PERFORM cal_tot.
* Begin of changes - UD1K914803 / UD1K918947
*        it_tab-vor = 'EC'.
*        MOVE-CORRESPONDING it_bppe TO it_tab.
*        PERFORM cal_tot.
      WHEN 'KBW1' or 'KBW2'.
        it_tab-vor = 'CF'.     "Carry Forward
        if P_CF eq 'X' .
          it_tab-tslvt = it_tab-wtp01.                      "UD1K918978
        endif.
        PERFORM cal_tot.
        it_tab-vor = 'EC'.     "Current
        MOVE-CORRESPONDING it_bppe TO it_tab.
        if P_CF eq 'X' .
          it_tab-tslvt = it_tab-wtp01.                      "UD1K918978
        endif.
        PERFORM cal_tot.
* End  of changes - UD1K914803 / UD1K918947
      WHEN OTHERS.
        it_tab-vor = 'XZ'.
        PERFORM cal_tot.
    ENDCASE.

  ENDLOOP.
****===JHS CURRENTY
*  IF p_rt = '2' OR p_rt = '7'.
*    LOOP AT it_tab.
*      MOVE-CORRESPONDING it_tab TO it_dum.
*      APPEND it_dum.
*      CLEAR  it_dum.
*    ENDLOOP.
*    REFRESH : it_tab.
*    CLEAR   : it_tab.
*
*    SORT it_dum BY key fictr knzaepo fipos geber ASCENDING.
*    LOOP AT it_dum.
*      MOVE-CORRESPONDING it_dum TO it_tab.
*      IF it_dum-vor+1(1) <> 'O'.
*        ADD it_dum-wtp00 TO wa_wtp00.
*        ADD it_dum-wtp01 TO wa_wtp01.
*        ADD it_dum-wtp02 TO wa_wtp02.
*        ADD it_dum-wtp03 TO wa_wtp03.
*        ADD it_dum-wtp04 TO wa_wtp04.
*        ADD it_dum-wtp05 TO wa_wtp05.
*        ADD it_dum-wtp06 TO wa_wtp06.
*        ADD it_dum-wtp07 TO wa_wtp07.
*        ADD it_dum-wtp08 TO wa_wtp08.
*        ADD it_dum-wtp09 TO wa_wtp09.
*        ADD it_dum-wtp10 TO wa_wtp10.
*        ADD it_dum-wtp11 TO wa_wtp11.
*        ADD it_dum-wtp12 TO wa_wtp12.
*      ENDIF.
*      wa_key =    it_dum-key.
*      wa_fictr  = it_dum-fictr.
*      wa_knzaepo = it_dum-knzaepo.
*      wa_fipos   = it_dum-fipos.
*      wa_geber   = it_dum-geber.
*      wa_bezeich = it_dum-bezeich.
*
*      COLLECT it_tab.
*      CLEAR   it_tab.
*      AT END OF geber.
*        it_tab-vor = 'GC'.   "CURR
*        it_tab-key =  wa_key.
*        it_tab-fictr = wa_fictr.
*        it_tab-knzaepo = wa_knzaepo.
*        it_tab-fipos   =  wa_fipos.
*        it_tab-geber      =  wa_geber.
*        it_tab-bezeich  =  wa_bezeich.
*        it_tab-wtp00  = wa_wtp00.
*        it_tab-wtp01  = wa_wtp01.
*        it_tab-wtp02  = wa_wtp02.
*        it_tab-wtp03  = wa_wtp03.
*        it_tab-wtp04  = wa_wtp04.
*        it_tab-wtp05  = wa_wtp05.
*        it_tab-wtp06  = wa_wtp06.
*        it_tab-wtp07  = wa_wtp07.
*        it_tab-wtp08  = wa_wtp08.
*        it_tab-wtp09  = wa_wtp09.
*        it_tab-wtp10  = wa_wtp10.
*        it_tab-wtp11  = wa_wtp11.
*        it_tab-wtp12  = wa_wtp12.
**    it_tab-wtp00 = it_tab-wtp01 + it_tab-wtp02 + it_tab-wtp03
**               + it_tab-wtp04 + it_tab-wtp05 + it_tab-wtp06
**               + it_tab-wtp07 + it_tab-wtp08 + it_tab-wtp09
**               + it_tab-wtp10 + it_tab-wtp11 + it_tab-wtp12.
**
*        COLLECT it_tab.
*        CLEAR   it_tab.
*        CLEAR : wa_wtp00, wa_wtp01, wa_wtp02, wa_wtp03, wa_wtp04.
*        CLEAR : wa_wtp05, wa_wtp06, wa_wtp07, wa_wtp08, wa_wtp09.
*        CLEAR : wa_wtp10, wa_wtp11, wa_wtp12.
*      ENDAT.
*    ENDLOOP.
*  ENDIF.
ENDFORM.                    " get_bbpe
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
FORM display_data.
  DATA: l_key1(40) TYPE c,
        l_key2(40) TYPE c,
        l_swch TYPE i.
  l_swch = 0.

* Begin of changes - UD1K918947


* End of changes - UD1K918947

  LOOP AT it_tab.
**---start #1 wskim
*    IF it_tab-vor+1(1) = 'O' OR  it_tab-vor+1(1) ='C'  OR
*          it_tab-vor+1(1) = 'P'.
*      CONTINUE.
*    ENDIF.
**---end
    IF p_act = '3'.   " Fund
*====2004/02/10
*      IF it_tab-geber = ' '.
*        CONTINUE.
*      ELSE.
      READ TABLE it_temp WITH KEY fictr = it_tab-fictr
                                  geber = it_tab-geber.
      IF sy-subrc = 0.
        MOVE it_temp-fipos   TO it_tab-fipos.
        MOVE it_temp-bezeich TO it_tab-bezeich.
      ENDIF.
*      ENDIF.
    ENDIF.

    IF p_act = '1' OR  p_act = '2'.
      READ TABLE it_temp WITH KEY fictr = it_tab-fictr
                                  fipos = it_tab-fipos.
      IF sy-subrc = 0.
        MOVE it_temp-bezeich TO it_tab-bezeich.
      ENDIF.
    ENDIF.

    l_key1(10)    = it_tab-fictr.
    l_key1+10(10) = it_tab-fipos.
    l_key1+20(10) = it_tab-geber.
*    l_key1+30(10) = it_tab-PROFIL.


    IF l_key1 <> l_key2.
      IF p_rt = '1'.
        ULINE.
      ENDIF.

      l_key2 = l_key1.
      CLEAR l_swch.
      PERFORM turn_switch USING l_swch.
      PERFORM display_line USING ' ' 'X'.
    ELSE.
      PERFORM turn_switch USING l_swch.
      PERFORM display_line USING ' ' ' '.
    ENDIF.

*FIX IT LATER
*    at end of key.
*      if p_amtype <> '8' and p_bdact = ' '.
*        sum.
*        perform display_line using 'X' ' '.
*        uline.
*      endif.
*    endat.
*
*    at last.
*      if p_bdact = ' '.
*        sum.
*        uline.
*        perform display_line using 'X' ' '.
*      endif.
*    endat.
  ENDLOOP.

  SET LEFT SCROLL-BOUNDARY COLUMN 74.

  CLEAR: it_tab-fictr, it_tab-fipos, it_tab-geber.

  ULINE.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  fill_budget
*&---------------------------------------------------------------------*
FORM fill_budget.
  REFRESH rec.
  LOOP AT it_tab.

* check release period
    IF     it_tab-profil = 'M'.   "Month
*     ok
      CASE g_per.
        WHEN 1.   rec-wert = it_tab-wtp01.
        WHEN 2.   rec-wert = it_tab-wtp02.
        WHEN 3.   rec-wert = it_tab-wtp03.
        WHEN 4.   rec-wert = it_tab-wtp04.
        WHEN 5.   rec-wert = it_tab-wtp05.
        WHEN 6.   rec-wert = it_tab-wtp06.
        WHEN 7.   rec-wert = it_tab-wtp07.
        WHEN 8.   rec-wert = it_tab-wtp08.
        WHEN 9.   rec-wert = it_tab-wtp09.
        WHEN 10.  rec-wert = it_tab-wtp10.
        WHEN 11.  rec-wert = it_tab-wtp11.
        WHEN 12.  rec-wert = it_tab-wtp12.
      ENDCASE.
    ELSEIF it_tab-profil = 'Q'.   "Quaterly
      CHECK g_per = 1 OR g_per = 4 OR g_per = 7 OR g_per = 10.
      IF g_per = 1.
        rec-wert = it_tab-wtp01 + it_tab-wtp02 + it_tab-wtp03.
      ELSEIF g_per = 4.
        rec-wert = it_tab-wtp04 + it_tab-wtp05 + it_tab-wtp06.
      ELSEIF g_per = 7.
        rec-wert = it_tab-wtp07 + it_tab-wtp08 + it_tab-wtp09.
      ELSE.
        rec-wert = it_tab-wtp10 + it_tab-wtp11 + it_tab-wtp12.
      ENDIF.
    ELSEIF it_tab-profil = 'H'.   "Half
      CHECK g_per = 1 OR g_per = 7.
      IF g_per = 1.
        rec-wert = it_tab-wtp01 + it_tab-wtp02 + it_tab-wtp03
                 + it_tab-wtp04 + it_tab-wtp05 + it_tab-wtp06.
      ELSE.
        rec-wert = it_tab-wtp07 + it_tab-wtp08 + it_tab-wtp09
                 + it_tab-wtp10 + it_tab-wtp11 + it_tab-wtp12.
      ENDIF.
    ELSEIF it_tab-profil = 'Y'.   "Year
      CHECK g_per = 1.
      rec-wert = it_tab-wtp01 + it_tab-wtp02 + it_tab-wtp03
               + it_tab-wtp04 + it_tab-wtp05 + it_tab-wtp06
               + it_tab-wtp07 + it_tab-wtp08 + it_tab-wtp09
               + it_tab-wtp10 + it_tab-wtp11 + it_tab-wtp12.
    ELSE.
      CONTINUE.
    ENDIF.

    rec-fistl = it_tab-fictr. "fundcenter
    rec-fipos = it_tab-fipos. "commitment item
    rec-geber = it_tab-geber. "fund

    IF rec-wert <> 0.
      APPEND rec.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " fill_budget
*&---------------------------------------------------------------------*
*&      Form  sort_it_tab
*&---------------------------------------------------------------------*
FORM sort_it_tab.
*======================2004/02/07
*  LOOP AT it_tab.
*    IF  it_tab-wtp01 = 0
*    AND it_tab-tslvt = 0
*    AND it_tab-wtp02 = 0
*    AND it_tab-wtp03 = 0
*    AND it_tab-wtp04 = 0
*    AND it_tab-wtp05 = 0
*    AND it_tab-wtp06 = 0
*    AND it_tab-wtp07 = 0
*    AND it_tab-wtp08 = 0
*    AND it_tab-wtp09 = 0
*    AND it_tab-wtp10 = 0
*    AND it_tab-wtp11 = 0
*    AND it_tab-wtp12 = 0.
*      DELETE it_tab.
*    ENDIF.
*  ENDLOOP.
*=================2004/02/07
** fill key field
**===JHS ADD 2003.10.31
*  REFRESH : it_sort1, it_sort2, it_sort3.
*  CLEAR   : it_sort1, it_sort2, it_sort3.
**---GET TEXT
*  CLEAR : it_temp, it_temp[].
*  LOOP AT it_tab.
*    MOVE-CORRESPONDING it_tab TO it_temp.
*    APPEND it_temp.
*    CLEAR  it_temp.
*  ENDLOOP.
*
*  IF p_act = '1'.
*    LOOP AT it_tab.
*      MOVE-CORRESPONDING it_tab TO it_sort1.
*      COLLECT it_sort1.
*      CLEAR  : it_sort1.
*    ENDLOOP.
*    REFRESH : it_tab.
*    CLEAR   : it_tab.
*    LOOP AT it_sort1.
*      MOVE-CORRESPONDING it_sort1 TO it_tab.
*      APPEND it_tab.
*      CLEAR  it_tab.
*    ENDLOOP.
**---2
*  ELSEIF p_act = '2'.
*    LOOP AT it_tab.
*      MOVE-CORRESPONDING it_tab TO it_sort2.
*      COLLECT it_sort2.
*      CLEAR  : it_sort2.
*    ENDLOOP.
*    REFRESH : it_tab.
*    CLEAR   : it_tab.
*    LOOP AT it_sort2.
*      MOVE-CORRESPONDING it_sort2 TO it_tab.
*      APPEND it_tab.
*      CLEAR  it_tab.
*    ENDLOOP.
**---3
*  ELSEIF p_act = '3'.
*    LOOP AT it_tab.
*      MOVE-CORRESPONDING it_tab TO it_sort3.
*      COLLECT it_sort3.
*      CLEAR  : it_sort3.
*    ENDLOOP.
*    REFRESH : it_tab.
*    CLEAR   : it_tab.
*    LOOP AT it_sort3.
*      MOVE-CORRESPONDING it_sort3 TO it_tab.
*      APPEND it_tab.
*      CLEAR  it_tab.
*    ENDLOOP.
*  ENDIF.
*
  LOOP AT it_tab.
    CASE p_act.
      WHEN '1'.
        it_tab-key(10) = it_tab-fictr.
        it_tab-key+10(1) = it_tab-knzaepo.
      WHEN '2'.
        it_tab-key(1) = it_tab-knzaepo.
        it_tab-key+1(10) = it_tab-fipos.
      WHEN '3'.
        it_tab-key(10) = it_tab-geber.
        it_tab-key+10(1) = it_tab-knzaepo.
* Begin of changes - UD1K918947
      when '5'.
        it_tab-key(10) = it_tab-fipos.
        case it_tab-vor.
          when 'CF'.
            it_tab-key+10(1) = '1'.
          when 'AO'.
            it_tab-key+10(1) = '2'.
          when 'CT'.
            it_tab-key+10(1) = '3'.
          when 'BS'.
            it_tab-key+10(1) = '4'.
          when 'DR'.
            it_tab-key+10(1) = '5'.
          when 'EC'.
            it_tab-key+10(1) = '6'.
          when 'FX'.
            it_tab-key+10(1) = '7'.
          when others.
            it_tab-key+10(1) = '99'.
        endcase.
* End changes - UD1K918947
    ENDCASE.

* residual amount = budget - actual - commitment
    MODIFY it_tab.
  ENDLOOP.
  CASE p_act.
    WHEN '1'. SORT it_tab BY key fipos       vor.
    WHEN '2'. SORT it_tab BY key fictr geber vor.
    WHEN '3'. SORT it_tab BY key  geber fictr fipos vor.
    WHEN '4'. SORT it_tab BY key fipos fictr geber vor.
    when '5'. SORT it_tab by key.                           "UD1K918947
  ENDCASE.


ENDFORM.                    " sort_it_tab
*&---------------------------------------------------------------------*
*&      Form  get_budget_period
*&---------------------------------------------------------------------*
FORM get_budget_period USING    f_fictr f_fipos f_geber
                       CHANGING f_profil.
  READ TABLE it_fmfpo  WITH KEY fipos = f_fipos.
  IF sy-subrc = 0.
    PERFORM determine_profile_fs USING    p_fik
                                          f_fictr
                                          it_fmfpo-posit
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
  WRITE:/ 'Year:', p_gjr .
  CASE p_rt.
    WHEN '3'. WRITE: '- Commitment'.
    WHEN '4'. WRITE: '- Invoice   '.
    WHEN '5'. WRITE: '- Invoice+Commitment'.
    WHEN '6'. WRITE: '- Payment'.
  ENDCASE.
  WRITE: ' (Round:',   p_r NO-GAP ,
         ', Decimal:', p_d NO-GAP , ') '.

  FORMAT COLOR COL_HEADING.
  WRITE:/ 'FnCtr  Commitment Item            Fund       P Category  '.
  WRITE:  '         Total       C/F  '.
  WRITE:  '     Jan       Feb       Mar       Apr '.
  WRITE:  '     May       Jun       Juy       Aug '.
  WRITE:  '     Sep       Oct       Nov       Dec '.
* uline
  FORMAT COLOR COL_NORMAL.

ENDFORM.                    " top_of_page
*&---------------------------------------------------------------------*
*&      Form  display_line
*&---------------------------------------------------------------------*
FORM display_line USING l_sum  l_key.

  IF l_sum = 'X'.
    FORMAT COLOR COL_GROUP.
  ELSE.
    FORMAT COLOR COL_KEY.
*   HIDE: it_tab-fictr, it_tab-fipos, it_tab-geber.
  ENDIF.

*  read table it_fmfpo  with key fipos = it_tab-fipos.
*  if sy-subrc <> 0. clear it_fmfpo-bezeich. endif.
  IF l_key = 'X'.
    WRITE:/   it_tab-fictr(6),  " no-gap,
              it_tab-fipos(6),  "  no-gap,
              it_tab-bezeich(20), "no-gap,
              it_tab-geber(10)  ,
              it_tab-profil(1)  . " no-gap.
    HIDE: it_tab-fictr, it_tab-fipos, it_tab-geber.
  ELSE.
    WRITE:/47 ' '. "it_tab-profil(1)  . " no-gap.
  ENDIF.

  CASE it_tab-vor+1(1).
* Begin of changes - UD1K914803
    WHEN 'F'.
      WRITE: 'Carry/Forward ' COLOR 5 INTENSIFIED OFF,
        62(12) it_tab-wtp00  ROUND p_r DECIMALS p_d NO-GAP
         COLOR 5 INTENSIFIED OFF.
* End of changes - UD1K914803
* Begin of changes - UD1K918947
    WHEN 'O'.
      WRITE: 'Original     ' COLOR 5 INTENSIFIED OFF,
        62(12) it_tab-wtp00  ROUND p_r DECIMALS p_d NO-GAP
         COLOR 5 INTENSIFIED OFF.
    WHEN 'T'.
      WRITE: 'Transfer     ' COLOR 5 INTENSIFIED OFF,
       62(12) it_tab-wtp00  ROUND p_r DECIMALS p_d NO-GAP
        COLOR 5 INTENSIFIED OFF.
    WHEN 'S'.
      WRITE: 'Supplement   ' COLOR 5 INTENSIFIED OFF,
        62(12) it_tab-wtp00  ROUND p_r DECIMALS p_d NO-GAP
         COLOR 5 INTENSIFIED OFF.
    WHEN 'R'.
      WRITE: 'Return       ' COLOR 5 INTENSIFIED OFF,
        62(12) it_tab-wtp00  ROUND p_r DECIMALS p_d NO-GAP
         COLOR 5 INTENSIFIED OFF.
    WHEN 'C'.
      WRITE: 'Current      ' COLOR 1 INTENSIFIED OFF,
        62(12) it_tab-wtp00  ROUND p_r DECIMALS p_d NO-GAP
         COLOR 1 INTENSIFIED OFF.
    WHEN 'X'.
      WRITE: 'Released     ' COLOR 1 INTENSIFIED OFF,
        62(12) it_tab-wtp00  ROUND p_r DECIMALS p_d NO-GAP
         COLOR 1 INTENSIFIED OFF.
* End of changes -   UD1K918947
    WHEN 'Q'.
      WRITE: 'Available     ' COLOR 3 INTENSIFIED OFF,
         62(12) it_tab-wtp00  ROUND p_r DECIMALS p_d NO-GAP
          COLOR 3 INTENSIFIED OFF.
    WHEN '1'.
      WRITE: 'Commitment   '  COLOR 2 INTENSIFIED OFF,
         62(12) it_tab-wtp00  ROUND p_r DECIMALS p_d NO-GAP
          COLOR 2 INTENSIFIED OFF.
    WHEN 'I'.
      WRITE: 'Invoice      '  COLOR 2 INTENSIFIED OFF,
         62(12) it_tab-wtp00  ROUND p_r DECIMALS p_d NO-GAP
          COLOR 2 INTENSIFIED OFF.
    WHEN 'A'.
      WRITE: 'Cmmt+Invoice '  COLOR 2 INTENSIFIED OFF,
         62(12) it_tab-wtp00  ROUND p_r DECIMALS p_d NO-GAP
          COLOR 2 INTENSIFIED OFF.
    WHEN 'P'.
      WRITE: 'Payments     '  COLOR 2 INTENSIFIED OFF,
         62(12) it_tab-wtp00  ROUND p_r DECIMALS p_d NO-GAP
          COLOR 2 INTENSIFIED OFF.
    WHEN OTHERS.
      WRITE: 'Unknown      '  COLOR 5 INTENSIFIED OFF,
         62(12) it_tab-wtp00  ROUND p_r DECIMALS p_d NO-GAP
          COLOR 5 INTENSIFIED OFF.
  ENDCASE.

*  FORMAT COLOR COL_NORMAL.
*  WRITE:
*    62(12) it_tab-wtp00  ROUND p_r DECIMALS p_d NO-GAP COLOR 3.
      WRITE:
        (10) it_tab-tslvt  ROUND p_r DECIMALS p_d NO-GAP,
        (10) it_tab-wtp01  ROUND p_r DECIMALS p_d NO-GAP,
        (10) it_tab-wtp02  ROUND p_r DECIMALS p_d NO-GAP,
        (10) it_tab-wtp03  ROUND p_r DECIMALS p_d NO-GAP,
        (10) it_tab-wtp04  ROUND p_r DECIMALS p_d NO-GAP,
        (10) it_tab-wtp05  ROUND p_r DECIMALS p_d NO-GAP,
        (10) it_tab-wtp06  ROUND p_r DECIMALS p_d NO-GAP,
        (10) it_tab-wtp07  ROUND p_r DECIMALS p_d NO-GAP,
        (10) it_tab-wtp08  ROUND p_r DECIMALS p_d NO-GAP,
        (10) it_tab-wtp09  ROUND p_r DECIMALS p_d NO-GAP,
        (10) it_tab-wtp10  ROUND p_r DECIMALS p_d NO-GAP,
        (10) it_tab-wtp11  ROUND p_r DECIMALS p_d NO-GAP,
        (10) it_tab-wtp12  ROUND p_r DECIMALS p_d NO-GAP.
*  else.
**   format color col_total.
*    write:
*      62(12) it_tab-pTP00  round p_r decimals p_d no-gap color 3.
*    write:
*      (10) it_tab-pTP01  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP02  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP03  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP04  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP05  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP06  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP07  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP08  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP09  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP10  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP11  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP12  round p_r decimals p_d no-gap.
*  endif.

*  if p_amtype = '8'.
**   format color col_background.
*    write:/62(12) it_tab-pTP00  round p_r decimals p_d no-gap  color 3.
*    write:
*      (10) it_tab-pTP01  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP02  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP03  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP04  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP05  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP06  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP07  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP08  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP09  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP10  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP11  round p_r decimals p_d no-gap,
*      (10) it_tab-pTP12  round p_r decimals p_d no-gap.
*  endif.

*  if p_bdact = 'X' and it_tab-vor+1(1) = 'X'.  "budget activity
*    uline.
*  endif.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  move_amount
*&---------------------------------------------------------------------*
FORM move_amount.
*---2004/02/11
*  it_tab-wtp01 =  it_fmit-tsl01.
*  it_tab-wtp02 =  it_fmit-tsl02.
*  it_tab-wtp03 =  it_fmit-tsl03.
*  it_tab-wtp04 =  it_fmit-tsl04.
*  it_tab-wtp05 =  it_fmit-tsl05.
*  it_tab-wtp06 =  it_fmit-tsl06.
*  it_tab-wtp07 =  it_fmit-tsl07.
*  it_tab-wtp08 =  it_fmit-tsl08.
*  it_tab-wtp09 =  it_fmit-tsl09.
*  it_tab-wtp10 =  it_fmit-tsl10.
*  it_tab-wtp11 =  it_fmit-tsl11.
*  it_tab-wtp12 =  it_fmit-tsl12.
*
  it_tab-wtp01 =  it_fmit-hsl01.
  it_tab-wtp02 =  it_fmit-hsl02.
  it_tab-wtp03 =  it_fmit-hsl03.
  it_tab-wtp04 =  it_fmit-hsl04.
  it_tab-wtp05 =  it_fmit-hsl05.
  it_tab-wtp06 =  it_fmit-hsl06.
  it_tab-wtp07 =  it_fmit-hsl07.
  it_tab-wtp08 =  it_fmit-hsl08.
  it_tab-wtp09 =  it_fmit-hsl09.
  it_tab-wtp10 =  it_fmit-hsl10.
  it_tab-wtp11 =  it_fmit-hsl11.
  it_tab-wtp12 =  it_fmit-hsl12.

  it_tab-wtp00 = it_tab-wtp01 + it_tab-wtp02 + it_tab-wtp03
             + it_tab-wtp04 + it_tab-wtp05 + it_tab-wtp06
             + it_tab-wtp07 + it_tab-wtp08 + it_tab-wtp09
             + it_tab-wtp10 + it_tab-wtp11 + it_tab-wtp12.
  COLLECT it_tab.
ENDFORM.                    " move_amount
*&---------------------------------------------------------------------*
*&      Form  move_payment
*&---------------------------------------------------------------------*
*FORM move_payment.
**  it_tab-pTP01 = -1 * it_fmit-TSL01.
**  it_tab-pTP02 = -1 * it_fmit-TSL02.
**  it_tab-pTP03 = -1 * it_fmit-TSL03.
**  it_tab-pTP04 = -1 * it_fmit-TSL04.
**  it_tab-pTP05 = -1 * it_fmit-TSL05.
**  it_tab-pTP06 = -1 * it_fmit-TSL06.
**  it_tab-pTP07 = -1 * it_fmit-TSL07.
**  it_tab-pTP08 = -1 * it_fmit-TSL08.
**  it_tab-pTP09 = -1 * it_fmit-TSL09.
**  it_tab-pTP10 = -1 * it_fmit-TSL10.
**  it_tab-pTP11 = -1 * it_fmit-TSL11.
**  it_tab-pTP12 = -1 * it_fmit-TSL12.
*
*  it_tab-pTP01 =  it_fmit-TSL01.
*  it_tab-pTP02 =  it_fmit-TSL02.
*  it_tab-pTP03 =  it_fmit-TSL03.
*  it_tab-pTP04 =  it_fmit-TSL04.
*  it_tab-pTP05 =  it_fmit-TSL05.
*  it_tab-pTP06 =  it_fmit-TSL06.
*  it_tab-pTP07 =  it_fmit-TSL07.
*  it_tab-pTP08 =  it_fmit-TSL08.
*  it_tab-pTP09 =  it_fmit-TSL09.
*  it_tab-pTP10 =  it_fmit-TSL10.
*  it_tab-pTP11 =  it_fmit-TSL11.
*  it_tab-pTP12 =  it_fmit-TSL12.
*
*  it_tab-ptp00 = it_tab-pTP01 + it_tab-pTP02 + it_tab-pTP03
*             + it_tab-pTP04 + it_tab-pTP05 + it_tab-pTP06
*             + it_tab-pTP07 + it_tab-pTP08 + it_tab-pTP09
*             + it_tab-pTP10 + it_tab-pTP11 + it_tab-pTP12.
*ENDFORM.                    " move_payment
*&---------------------------------------------------------------------*
*&      Form  turn_switch
*&---------------------------------------------------------------------*
FORM turn_switch USING    p_l_swch.
  IF p_l_swch = 0.
    FORMAT INTENSIFIED ON.
    p_l_swch = 1.
  ELSE.
    FORMAT INTENSIFIED OFF.
    CLEAR p_l_swch.
  ENDIF.
ENDFORM.                    " turn_switch
*&---------------------------------------------------------------------*
*&      Form  CAL_TOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_tot.
  it_tab-wtp00 = it_tab-wtp01 + it_tab-wtp02 + it_tab-wtp03
             + it_tab-wtp04 + it_tab-wtp05 + it_tab-wtp06
             + it_tab-wtp07 + it_tab-wtp08 + it_tab-wtp09
             + it_tab-wtp10 + it_tab-wtp11 + it_tab-wtp12.

  COLLECT it_tab.

ENDFORM.                    " CAL_TOT
*&---------------------------------------------------------------------*
*&      Form  MAKE_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_tab.
***===JHS CURRENTY
  IF  p_rt = '6'.
    REFRESH : it_dum.
    CLEAR   : it_dum.

    LOOP AT it_tab.
      MOVE-CORRESPONDING it_tab TO it_dum.
      APPEND it_dum.
      CLEAR  it_dum.
    ENDLOOP.

    REFRESH : it_tab.
    CLEAR   : it_tab.

    SORT it_dum BY key fictr knzaepo fipos geber ASCENDING.
    LOOP AT it_dum.
      MOVE-CORRESPONDING it_dum TO it_tab.
      IF it_dum-profil = 'B'.
        IF it_dum-vor =  'EC'.   "Current
          ADD it_dum-wtp00 TO wa_wtp00.
          ADD it_dum-wtp01 TO wa_wtp01.
          ADD it_dum-wtp02 TO wa_wtp02.
          ADD it_dum-wtp03 TO wa_wtp03.
          ADD it_dum-wtp04 TO wa_wtp04.
          ADD it_dum-wtp05 TO wa_wtp05.
          ADD it_dum-wtp06 TO wa_wtp06.
          ADD it_dum-wtp07 TO wa_wtp07.
          ADD it_dum-wtp08 TO wa_wtp08.
          ADD it_dum-wtp09 TO wa_wtp09.
          ADD it_dum-wtp10 TO wa_wtp10.
          ADD it_dum-wtp11 TO wa_wtp11.
          ADD it_dum-wtp12 TO wa_wtp12.
        ENDIF.
      ELSE.
        IF it_dum-vor =  'FX'.    "Release
          ADD it_dum-wtp00 TO wa_wtp00.
          ADD it_dum-wtp01 TO wa_wtp01.
          ADD it_dum-wtp02 TO wa_wtp02.
          ADD it_dum-wtp03 TO wa_wtp03.
          ADD it_dum-wtp04 TO wa_wtp04.
          ADD it_dum-wtp05 TO wa_wtp05.
          ADD it_dum-wtp06 TO wa_wtp06.
          ADD it_dum-wtp07 TO wa_wtp07.
          ADD it_dum-wtp08 TO wa_wtp08.
          ADD it_dum-wtp09 TO wa_wtp09.
          ADD it_dum-wtp10 TO wa_wtp10.
          ADD it_dum-wtp11 TO wa_wtp11.
          ADD it_dum-wtp12 TO wa_wtp12.
        ENDIF.
      ENDIF.
*--
      IF it_dum-vor =  'HI'.
        ADD it_dum-wtp00 TO   wa_wtp00.
        ADD it_dum-wtp01 TO   wa_wtp01.
        ADD it_dum-wtp02 TO   wa_wtp02.
        ADD it_dum-wtp03 TO   wa_wtp03.
        ADD it_dum-wtp04 TO   wa_wtp04.
        ADD it_dum-wtp05 TO   wa_wtp05.
        ADD it_dum-wtp06 TO   wa_wtp06.
        ADD it_dum-wtp07 TO   wa_wtp07.
        ADD it_dum-wtp08 TO   wa_wtp08.
        ADD it_dum-wtp09 TO   wa_wtp09.
        ADD it_dum-wtp10 TO   wa_wtp10.
        ADD it_dum-wtp11 TO   wa_wtp11.
        ADD it_dum-wtp12 TO   wa_wtp12.
      ENDIF.

      IF it_dum-vor =  'G1'.
        ADD it_dum-wtp00 TO   wa_wtp00.
        ADD it_dum-wtp01 TO   wa_wtp01.
        ADD it_dum-wtp02 TO   wa_wtp02.
        ADD it_dum-wtp03 TO   wa_wtp03.
        ADD it_dum-wtp04 TO   wa_wtp04.
        ADD it_dum-wtp05 TO   wa_wtp05.
        ADD it_dum-wtp06 TO   wa_wtp06.
        ADD it_dum-wtp07 TO   wa_wtp07.
        ADD it_dum-wtp08 TO   wa_wtp08.
        ADD it_dum-wtp09 TO   wa_wtp09.
        ADD it_dum-wtp10 TO   wa_wtp10.
        ADD it_dum-wtp11 TO   wa_wtp11.
        ADD it_dum-wtp12 TO   wa_wtp12.
      ENDIF.

      wa_key =    it_dum-key.
      wa_fictr  = it_dum-fictr.
      wa_knzaepo = it_dum-knzaepo.
      wa_fipos   = it_dum-fipos.
      wa_geber   = it_dum-geber.
      wa_bezeich = it_dum-bezeich.

      COLLECT it_tab.
      CLEAR   it_tab.
      AT END OF geber.
        it_tab-vor = 'KQ'.   "Available
        it_tab-key =  wa_key.
        it_tab-fictr = wa_fictr.
        it_tab-knzaepo = wa_knzaepo.
        it_tab-fipos   =  wa_fipos.
        it_tab-geber      =  wa_geber.
        it_tab-bezeich  =  wa_bezeich.
        it_tab-wtp00  = wa_wtp00.
        it_tab-wtp01  = wa_wtp01.
        it_tab-wtp02  = wa_wtp02.
        it_tab-wtp03  = wa_wtp03.
        it_tab-wtp04  = wa_wtp04.
        it_tab-wtp05  = wa_wtp05.
        it_tab-wtp06  = wa_wtp06.
        it_tab-wtp07  = wa_wtp07.
        it_tab-wtp08  = wa_wtp08.
        it_tab-wtp09  = wa_wtp09.
        it_tab-wtp10  = wa_wtp10.
        it_tab-wtp11  = wa_wtp11.
        it_tab-wtp12  = wa_wtp12.
*    it_tab-wtp00 = it_tab-wtp01 + it_tab-wtp02 + it_tab-wtp03
*               + it_tab-wtp04 + it_tab-wtp05 + it_tab-wtp06
*               + it_tab-wtp07 + it_tab-wtp08 + it_tab-wtp09
*               + it_tab-wtp10 + it_tab-wtp11 + it_tab-wtp12.
*
        COLLECT it_tab.
        CLEAR   it_tab.
        CLEAR : wa_wtp00, wa_wtp01, wa_wtp02, wa_wtp03, wa_wtp04.
        CLEAR : wa_wtp05, wa_wtp06, wa_wtp07, wa_wtp08, wa_wtp09.
        CLEAR : wa_wtp10, wa_wtp11, wa_wtp12.
      ENDAT.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " MAKE_TAB
*&---------------------------------------------------------------------*
*&      Form  make_bdc_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1339   text
*      -->P_1340   text
*      -->P_1341   text
*----------------------------------------------------------------------*

FORM make_bdc_rtn USING   dynbegin program dynpro.
  CLEAR it_bdc.

  IF dynbegin = 'X'.
    it_bdc-program  = program.
    it_bdc-dynpro   = dynpro.
    it_bdc-dynbegin = 'X'.
  ELSE.
    it_bdc-fnam     = program.
    it_bdc-fval     = dynpro.
  ENDIF.

  APPEND it_bdc.
ENDFORM.                    " make_bdc_rtn
