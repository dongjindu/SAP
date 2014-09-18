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
*& 10/05/2011 KDM                         FM 'KBPA_FIFM_GET_PROFIL fmit'
*&                                        one parameter(Functional Area) add
*&                                        --> From fmit table(RFAREA)
*&                                           relational logic change(KDM01)
*&--------------------------------------------------------------------
*
* Maintain user own data (SU3)
*   ZCOLV1  USR, MGR, ADM
*
* budget/commitment c/f - RFFMEP1G
*
* need enhancement
*
* - RFFMEP1B for budget(yearly), RFFMEP4B (monthly)
* - RFFMTO10 for actual
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
         fictr   LIKE fmfctr-fictr, "Fund Cneter
         knzaepo LIKE fmfpo-knzaepo, "category
         fipos   LIKE fmfpo-fipos,  "Commitment Item
         geber   LIKE bppe-geber,   "Fund
         profil  LIKE tbpfm-profil,
         bezeich LIKE fmfpot-bezeich,
         vor(2)  TYPE c,             "budget activity
         wtp00   LIKE bppe-wtp01,    "total
         tslvt   LIKE bppe-wtp01,    "carryfoward
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

DATA: BEGIN OF it_output OCCURS 0,
         fictr   LIKE fmfctr-fictr,
         profil  LIKE tbpfm-profil,
         fipos   LIKE fmfpo-fipos,
         geber   LIKE bppe-geber,
         bezeich LIKE fmfpot-bezeich,
         vor(2)  TYPE c,             "budget activity
         act(20) TYPE c,
         wtp00 LIKE bppe-wtp01, "E P,
         tslvt LIKE bppe-wtp01, "E P,
         wtp01 LIKE bppe-wtp01, "E P,
         wtp02 LIKE bppe-wtp01,
         wtp03 LIKE bppe-wtp01,
         wtp04 LIKE bppe-wtp01,
         wtp05 LIKE bppe-wtp01,
         wtp06 LIKE bppe-wtp01,
         wtp07 LIKE bppe-wtp01,
         wtp08 LIKE bppe-wtp01,
         wtp09 LIKE bppe-wtp01,
         wtp10 LIKE bppe-wtp01,
         wtp11 LIKE bppe-wtp01,
         wtp12 LIKE bppe-wtp01,
*         posit   LIKE fmfpo-posit,
      END OF it_output.

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


*--- ALV
TYPE-POOLS: slis.
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv,
      g_repid     LIKE sy-repid.
*---- ALV

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
PARAMETERS: p_cf AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK dl.

PARAMETERS: p_disp(4) TYPE c  DEFAULT 'LIST'. " no-display.

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
  w_rt_line-key  = '0'.  w_rt_line-text = 'Released+Actual'.
  APPEND w_rt_line TO it_rt.
  w_rt_line-key  = '1'.  w_rt_line-text = 'Budget'.
  APPEND w_rt_line TO it_rt.
  w_rt_line-key  = '2'.  w_rt_line-text = 'Commitment'.
  APPEND w_rt_line TO it_rt.
  w_rt_line-key  = '3'.  w_rt_line-text = 'Invoice'.
  APPEND w_rt_line TO it_rt.
  w_rt_line-key  = '4'.  w_rt_line-text = 'Invoice+Commitment'.
  APPEND w_rt_line TO it_rt.
  w_rt_line-key  = '5'.  w_rt_line-text = 'Payment'.
  APPEND w_rt_line TO it_rt.
  w_rt_line-key  = '9'.  w_rt_line-text = 'All'.
  APPEND w_rt_line TO it_rt.

  p_rt = '0'.

**------sort
*  w_line-key  = '1'.
*  w_line-text = 'Sort by Fund Center'.
*  APPEND w_line TO it_val.
*  w_line-key  = '2'.
*  w_line-text = 'Sort by Commitment Item'.
*  APPEND w_line TO it_val.
*  w_line-key  = '3'.
*  w_line-text = 'Sort by Fund'.
*  APPEND w_line TO it_val.
*  p_act = '3'.
*
*  w_line-key  = '4'.
*  w_line-text = 'Sort by Item'.
*  APPEND w_line TO it_val.
*  p_act = '4'.
** Begin of changes - UD1K918947
*  w_line-key  = '5'.
*  w_line-text = 'Sort by Activity'.
*  APPEND w_line TO it_val.
*  p_act = '5'.
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

* default budget profile
  REFRESH p_prof.
  p_prof-option = 'EQ'. p_prof-sign = 'I'.
  p_prof-low = 'M'. APPEND p_prof.
  p_prof-low = 'Q'. APPEND p_prof.
  p_prof-low = 'H'. APPEND p_prof.
  p_prof-low = 'Y'. APPEND p_prof.

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
  PERFORM get_bbpe.
* read actuals
  PERFORM get_fmit.
  PERFORM calculate_available_budget.

*---------------------------------------------------------------------
*  END-OF-SELECTION.
*---------------------------------------------------------------------
END-OF-SELECTION.


  PERFORM fill_output.
  SORT it_output BY fictr fipos geber vor.
  PERFORM display_alv.

*---------------------------------------------------------------------
AT USER-COMMAND.
*---------------------------------------------------------------
  CASE sy-ucomm.
    WHEN 'DOWN'.
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
*  PERFORM top_of_page.

************************************************************************
* TOP-OF-PAGE DURING LINE-SELECTION
************************************************************************
TOP-OF-PAGE DURING LINE-SELECTION.
*  PERFORM top_of_page.

************************************************************************
* Line selection                                                       *
************************************************************************
AT LINE-SELECTION.
  PERFORM pick.

************************************************************************
***  (SHIFT+PF1) Execute Download
************************************************************************
AT PF13.
*  PERFORM data_download.

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


** : Not use Parent_obj, So need to change logic.
** :
*    IF fmhictr-parent_obj = space.
*      IF e_level <> 'ADM'.
*        DELETE it_fmfctr.
*      ENDIF.
*    ELSE.
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

*    ENDIF.
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
                                    it_fmit-rfarea   "" KDM01
                              CHANGING it_tab-profil.
    CHECK it_tab-profil IN p_prof.

* 50 - p/r, 51 - p/o, 54 - invoice, 57 - payment, 58 - d/p req
* 61 - downpayment
* 95 - co posting (secondary cost posting)

    IF it_fmit-rwrttp = '50' OR it_fmit-rwrttp = '51'
    OR it_fmit-rwrttp = '60' OR it_fmit-rwrttp = '61'.
      it_tab-vor = 'C1'.  "commitment
      PERFORM move_amount.

*--invoice
    ELSEIF ( ( it_fmit-rwrttp = '54' OR it_fmit-rwrttp = '66'
        OR it_fmit-rwrttp = '95' ) AND it_fmit-rbtart = '0100' ).
      it_tab-vor = 'C3'.  "invoice
      PERFORM move_amount.

** payment
    ELSEIF ( it_fmit-rbtart = '0250' OR it_fmit-rwrttp = '61' ).  " Paid
*       or it_fmit-RWRTTP = '57' or it_fmit-RWRTTP = '58'.

      it_tab-vor = 'PY'.  "payment
      PERFORM move_amount.
    ENDIF.

*=====JHS ADD 2003.10.31   Carry Foward (commitment)
    IF  ( it_fmit-rwrttp = '50' OR it_fmit-rwrttp = '51' )
    AND ( it_fmit-rbtart = '0350' OR it_fmit-rbtart = '0150' OR
          it_fmit-rbtart = '0500' ).

      it_tab-tslvt =  it_fmit-hslvt.
      it_tab-wtp00 =  it_tab-tslvt.
      it_tab-vor = 'C1'.  COLLECT it_tab.

      IF p_cf = space.  "No commitment budget c/f done
        it_tab-tslvt =  it_fmit-hslvt * -1.
        it_tab-wtp00 =  it_tab-tslvt.

        it_tab-vor = 'B0'.  COLLECT it_tab.  "add up to original
        it_tab-vor = 'B9'.  COLLECT it_tab.  "add up to current
        it_tab-vor = 'BX'.  COLLECT it_tab.  "add up to released
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
                                    it_bppe-farea    "" KDM01
                              CHANGING it_tab-profil.
    CHECK it_tab-profil IN p_prof.



* display budget activity (sort key + activity)
    CASE it_bppe-vorga.
      WHEN 'KBUD'.
        MOVE-CORRESPONDING it_bppe TO it_tab.
        PERFORM collect_tot USING 'B0'.  "orgin

        MOVE-CORRESPONDING it_bppe TO it_tab.
        PERFORM collect_tot USING 'B9'.  "Current
      WHEN 'KBN0'.
        MOVE-CORRESPONDING it_bppe TO it_tab.
        PERFORM collect_tot USING 'B1'.  "supp

        MOVE-CORRESPONDING it_bppe TO it_tab.
        PERFORM collect_tot USING 'B9'.  "Current
      WHEN 'KBR0'.
        MOVE-CORRESPONDING it_bppe TO it_tab.
        PERFORM collect_tot USING 'B2'.   "return

        MOVE-CORRESPONDING it_bppe TO it_tab.
        PERFORM collect_tot USING 'B9'.   "Current
      WHEN 'KBUS'.
*Trf=KBUS, Trf C/F(KBU2), Trf Adv (KBU3)
        MOVE-CORRESPONDING it_bppe TO it_tab.
        PERFORM collect_tot USING 'B3'.

        MOVE-CORRESPONDING it_bppe TO it_tab.
        PERFORM collect_tot USING 'B9'.   "Current
      WHEN 'KBUE'.
        MOVE-CORRESPONDING it_bppe TO it_tab.
        PERFORM collect_tot USING 'B3'.   "transfer

        MOVE-CORRESPONDING it_bppe TO it_tab.
        PERFORM collect_tot USING 'B9'.    "Current

*budget c/f (sender,receiver), Trf c/f=KBU2, Trf Adv=KBU3
      WHEN 'KBW1' OR 'KBW2' OR 'KBU2' OR 'KBU3'.
        IF it_bppe-vorga = 'KBW1'. "Sender year

        ELSE.
          MOVE-CORRESPONDING it_bppe TO it_tab.
          it_tab-tslvt = it_tab-wtp01.
          CLEAR          it_tab-wtp01.
          PERFORM collect_tot USING 'AC'.  "C/F

          MOVE-CORRESPONDING it_bppe TO it_tab.
          it_tab-tslvt = it_tab-wtp01.
          CLEAR          it_tab-wtp01.
          PERFORM collect_tot USING 'B9'.  "current

        ENDIF.

      WHEN 'KBFR'.  "Amt Type = 46
        MOVE-CORRESPONDING it_bppe TO it_tab.
        PERFORM collect_tot USING 'BX'.  "release

      WHEN OTHERS.
        IF it_bppe-wrttp = '43'.  "Budget
          MOVE-CORRESPONDING it_bppe TO it_tab.
          PERFORM collect_tot USING 'XZ'.
        ENDIF.
    ENDCASE.

  ENDLOOP.
ENDFORM.                    " get_bbpe
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
*&      Form  get_budget_period
*&---------------------------------------------------------------------*
FORM get_budget_period USING    f_fictr f_fipos f_geber f_farea  ""KDM01
                       CHANGING f_profil.
  READ TABLE it_fmfpo  WITH KEY fipos = f_fipos.
  IF sy-subrc = 0.
    PERFORM determine_profile_fs USING    p_fik
                                          f_fictr
                                          it_fmfpo-posit
                                          f_geber
                                          p_gjr
                                          f_farea    ""KDM01
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
                                   l_farea   ""KDM01
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
            i_farea         = l_farea   "" KDM01
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
*  CHECK g_func = 'X'.
*
*  IF sy-cucol < 15.     " download
*    PERFORM data_download.
*  ELSEIF sy-cucol < 30. " test
*
*  ELSE.
*    READ LINE 1 FIELD VALUE p_r.
*    READ LINE 1 FIELD VALUE p_d.
*    sy-lsind = sy-lsind - 1.
*    PERFORM display_data.
*
*  ENDIF.
*
* icon_refresh AS ICON HOTSPOT
* READ CURRENT LINE
*           FIELD VALUE report_lst-optres INTO report_lst-optres.
ENDFORM.                    " pick
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
*&      Form  collect_tot
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_tot  USING f_activity.
  it_tab-wtp00 = it_tab-tslvt
             + it_tab-wtp01 + it_tab-wtp02 + it_tab-wtp03
             + it_tab-wtp04 + it_tab-wtp05 + it_tab-wtp06
             + it_tab-wtp07 + it_tab-wtp08 + it_tab-wtp09
             + it_tab-wtp10 + it_tab-wtp11 + it_tab-wtp12.

  it_tab-vor = f_activity.

  COLLECT it_tab.
  CLEAR: it_tab-wtp00,
         it_tab-wtp01,  it_tab-wtp02,  it_tab-wtp03,
         it_tab-wtp04,  it_tab-wtp05,  it_tab-wtp06,
         it_tab-wtp07,  it_tab-wtp08,  it_tab-wtp09,
         it_tab-wtp10,  it_tab-wtp11,  it_tab-wtp12.


ENDFORM.                    " collect_tot
*&---------------------------------------------------------------------*
*&      Form  calculate_available_budget
*&---------------------------------------------------------------------*
FORM calculate_available_budget.
  LOOP AT it_tab
     WHERE vor = 'BX'  "released
*         OR vor = 'AC'  "c/f
        OR vor = 'C1'  "commitment
        OR vor = 'C3'. "invoice
    it_tab-vor = 'D1'.
    COLLECT it_tab.
  ENDLOOP.

ENDFORM.                    " calculate_available_budget
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
*&---------------------------------------------------------------------*
*&      Form  fill_output
*&---------------------------------------------------------------------*
FORM fill_output.
  CLEAR : it_output, it_output[].

  IF p_rt = '0'.
    DELETE it_tab
       WHERE vor = 'AC'
          OR vor = 'B0' OR vor = 'B1' OR vor = 'B2' OR vor = 'B3'
          OR vor = 'B9' OR vor = 'PY'.
  ENDIF.
  IF p_rt = '1'.
    DELETE it_tab
       WHERE vor = 'C1' OR vor = 'C3' OR vor = 'C9' OR vor = 'D1'
          OR vor = 'PY'.
  ENDIF.
  IF p_rt = '2'.
    DELETE it_tab WHERE vor <> 'C1'.
  ENDIF.
  IF p_rt = '3'.
    DELETE it_tab WHERE vor <> 'C3'.
  ENDIF.
  IF p_rt = '4'.
    DELETE it_tab WHERE vor <> 'C1' AND vor <> 'C3'.
  ENDIF.
  IF p_rt = '5'.
    DELETE it_tab WHERE vor <> 'PY'.
  ENDIF.

  LOOP AT it_tab.
    MOVE-CORRESPONDING it_tab TO it_output.

    CASE it_tab-vor.
      WHEN 'AC'.   WRITE: 'C/F         '  TO it_output-act.
      WHEN 'B0'.   MOVE   'Original    '  TO it_output-act.
      WHEN 'B1'.   WRITE: 'Supplement  '  TO it_output-act.
      WHEN 'B2'.   WRITE: 'Return      '  TO it_output-act.
      WHEN 'B3'.   WRITE: 'Transfer    '  TO it_output-act.
      WHEN 'B9'.   WRITE: 'Current     '  TO it_output-act.
      WHEN 'BX'.   WRITE: 'Released    '  TO it_output-act.
      WHEN 'C1'.   WRITE: 'Commitment  '  TO it_output-act.
      WHEN 'C3'.   WRITE: 'Invoice     '  TO it_output-act.
      WHEN 'C9'.   WRITE: 'Cmmt+Invoice'  TO it_output-act.
      WHEN 'D1'.   WRITE: 'Available   '  TO it_output-act.
      WHEN 'PY'.   WRITE: 'Payment     '  TO it_output-act.
      WHEN OTHERS. WRITE: 'Unknown     '  TO it_output-act.
    ENDCASE.
    APPEND it_output.
    CLEAR  it_output.
  ENDLOOP.
ENDFORM.                    " fill_output
*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
FORM display_alv.

  PERFORM field_setting TABLES gt_fieldcat USING :
  'FICTR'     'FundCtr'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'FIPOS'     'CommitItm'     '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'BEZEICH'   'Description'   '20' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'GEBER'     'Fund'          '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'PROFIL'    'Profile'       '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'VOR'       'Sort'          '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'ACT'       'Category'      '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'WTP00'     'Total'         '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'TSLVT'     'C/F'           '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'WTP01'     'JAN'           '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'WTP02'     'FEB'           '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'WTP03'     'MAR'           '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'WTP04'     'APR'           '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'WTP05'     'MAY'           '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'WTP06'     'JUN'           '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'WTP07'     'JUL'           '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'WTP08'     'AUG'           '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'WTP09'     'SEP'           '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'WTP10'     'OCT'           '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'WTP11'     'NOV'           '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'WTP12'     'DEC'           '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' '.

  g_repid = sy-repid.

  IF p_disp = 'GRID'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
         EXPORTING
              i_callback_program = g_repid
              it_fieldcat        = gt_fieldcat
              i_save             = 'A'
         TABLES
              t_outtab           = it_output
         EXCEPTIONS
              program_error      = 1
              OTHERS             = 2.
  ELSE.
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
         EXPORTING
              i_callback_program = g_repid
              it_fieldcat        = gt_fieldcat
              i_save             = 'A'
         TABLES
              t_outtab           = it_output
         EXCEPTIONS
              program_error      = 1
              OTHERS             = 2.

  ENDIF.


ENDFORM.                    " display_alv
*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM field_setting TABLES p_fieldcat_t LIKE gt_fieldcat USING
                                  p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO gt_fieldcat.

ENDFORM.                    " fill_field_category
*ALV
