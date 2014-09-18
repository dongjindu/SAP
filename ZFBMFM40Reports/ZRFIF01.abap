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
*& 09/29/2006 Manju        UD1K922250     Call ZRFIF07 Program instead
*&                                        of RFFMEP1O program
*&--------------------------------------------------------------------
*
* Maintain user own data (SU3)
*   ZCOLV1  USR, MGR, ADM
*
* budget/commitment c/f - RFFMEP1G
*
*
REPORT zrfif01 MESSAGE-ID zmfi
               LINE-SIZE 220
               LINE-COUNT 65
               NO STANDARD PAGE HEADING.

INCLUDE <icon>.

*copy program : WISP_LOESCHE_LAYOUT

TABLES: fmci, fmcit,    "commitment
        fmfctr, fmfctrt,  "fund center
        fmhictr,          "fund center hier
        bppe.
TABLES: fmep,fmsu, aufk.
TABLES: fkrs, fpos, tbpfm.
DATA: it_bppe LIKE bppe OCCURS 0 WITH HEADER LINE.

*actual
*TABLES: fmit.
DATA: it_fmit LIKE fmit OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_tab OCCURS 0,
         fictr   LIKE fmfctr-fictr, "Fund Cneter
         potyp   LIKE fmci-potyp, "category
         fipos   LIKE fmci-fipos,  "Commitment Item
         geber   LIKE bppe-geber,   "Fund
         profil  LIKE tbpfm-profil,
         bezei   LIKE fmcit-bezei ,
         vor(2)  TYPE c,             "budget activity
         act(20) TYPE c,
         wtp99   LIKE bppe-wtp01,    "total
         tslvt   LIKE bppe-wtp01,    "carryfoward
         wtp00   LIKE bppe-wtp01,    "SUM(JAN-DEC)
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
         wtp13   LIKE bppe-wtp13,
*         posit   LIKE fmci-posit,


*         counter(4)   TYPE n,
         color_line(4) TYPE c,           " Line color
*        color_cell    TYPE lvc_t_scol,  " Cell color
      END OF it_tab.

DATA: BEGIN OF it_temp OCCURS 0,
         key(17) TYPE c,
         fictr   LIKE fmfctr-fictr, "Fund Cneter
         potyp   LIKE fmci-potyp, "category
         fipos   LIKE fmci-fipos,  "Commitment Item
         geber   LIKE bppe-geber,   "Fund
         bezei  LIKE fmcit-bezei ,
      END OF it_temp.

DATA: it_output LIKE it_tab OCCURS 0 WITH HEADER LINE.

*DATA: BEGIN OF it_output OCCURS 0,
*         fictr   LIKE fmfctr-fictr,
*         profil  LIKE tbpfm-profil,
*         fipos   LIKE fmci-fipos,
*         geber   LIKE bppe-geber,
*         bezei  LIKE fmcit-bezei ,
*         vor(2)  TYPE c,             "budget activity
*         act(20) TYPE c,
*         wtp99 LIKE bppe-wtp01, "E P,
*         wtp00 LIKE bppe-wtp01, "E P,
*         tslvt LIKE bppe-wtp01, "E P,
*         wtp01 LIKE bppe-wtp01, "E P,
*         wtp02 LIKE bppe-wtp01,
*         wtp03 LIKE bppe-wtp01,
*         wtp04 LIKE bppe-wtp01,
*         wtp05 LIKE bppe-wtp01,
*         wtp06 LIKE bppe-wtp01,
*         wtp07 LIKE bppe-wtp01,
*         wtp08 LIKE bppe-wtp01,
*         wtp09 LIKE bppe-wtp01,
*         wtp10 LIKE bppe-wtp01,
*         wtp11 LIKE bppe-wtp01,
*         wtp12 LIKE bppe-wtp01,
*         wtp13 LIKE bppe-wtp01,
**         posit   LIKE fmci-posit,
*      END OF it_output.

DATA: g_output LIKE it_output.

DATA: BEGIN OF it_head OCCURS 0,
         text(16),
      END OF it_head.

DATA : BEGIN OF it_fmfctr OCCURS 0,
         ctr_objnr  LIKE fmfctr-ctr_objnr,
         fictr      LIKE fmfctr-fictr,
         parent_obj LIKE fmhictr-parent_obj,
       END OF it_fmfctr.

DATA : BEGIN OF it_fmci OCCURS 0,
         fipos    LIKE fmci-fipos,
         bezei    LIKE fmcit-bezei ,
         posit    LIKE fmep-posit,
       END OF it_fmci.

* Active availability control on commitment budget
DATA: BEGIN OF fmctl OCCURS 0,
         fictr     LIKE fmfctr-fictr,
         fipos     LIKE fmci-fipos,
         geber     LIKE bppe-geber,
         profil    LIKE tbpfm-profil,
      END OF fmctl.

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
       wa_potyp LIKE fmci-potyp, "category
       wa_fipos   LIKE fmci-fipos,
       wa_geber   LIKE bppe-geber,
       wa_bezei  LIKE fmcit-bezei ,
       wa_vor(2)  TYPE c.             "budget activity

* for combobox
TYPE-POOLS: vrm.
DATA: it_rt TYPE vrm_values,
      w_rt_line LIKE LINE OF it_rt.

DATA: it_val TYPE vrm_values,
      w_line LIKE LINE OF it_val.

RANGES : r_fictr     FOR fmfctr-fictr,
         r_fipos     FOR fmci-fipos,
         r_vtci      FOR fmit-rwrttp,  "V.Type for commitment
*         r_vtpr      FOR fmit-rwrttp,  "V.Type for PR
*         r_vtpo      FOR fmit-rwrttp,  "V.Type for PO
         r_vtiv      FOR fmit-rwrttp,  "V.Type for invoice
         r_vtpy      FOR fmit-rwrttp,  "V.Type for payment
         r_atcf      FOR fmit-rbtart.  "c/f amt type


*--- ALV
TYPE-POOLS: slis.

CONSTANTS:
  c_fnam_cos_pf_status
         TYPE  slis_formname VALUE 'ALV_SET_PF_STATUS',
  c_fnam_cos_user_command
         TYPE slis_formname  VALUE 'ALV_USER_COMMAND',
  c_f2code
         LIKE sy-ucomm       VALUE '&ETA'.

DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event, "WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv, "WITH HEADER LINE,
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

DATA: it_color TYPE TABLE OF lvc_s_scol.

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
                p_fipos FOR fmci-fipos,   "Commitment Item
                p_geber FOR bppe-geber,    "Fund
                p_prof  FOR tbpfm-profil,  "default 'B' option NE
                p_knz   FOR fmci-potyp DEFAULT '3'.  " item category

SELECT-OPTIONS: p_auf   FOR aufk-aufnr.    "Order
*----
PARAMETERS: p_ver  LIKE bppe-versn DEFAULT '000' NO-DISPLAY.
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
*PARAMETERS: p_cf AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK dl.
PARAMETERS: p_rt(1) TYPE c AS LISTBOX VISIBLE LENGTH 25 OBLIGATORY.

PARAMETERS: p_disp(4) TYPE c  DEFAULT 'GRID' NO-DISPLAY.

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

*drill-down
RANGES: r_gjahr FOR fmfi-gjahr,
        r_perio FOR fmfi-perio.

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

* value type
  REFRESH: r_vtci, r_vtiv, r_vtpy.
  r_vtci-option = 'EQ'. r_vtci-sign = 'I'.
  r_vtiv-option = 'EQ'. r_vtiv-sign = 'I'.
  r_vtpy-option = 'EQ'. r_vtpy-sign = 'I'.

  r_vtci-low = '50'. APPEND r_vtci.   "PR
  r_vtci-low = '51'. APPEND r_vtci.   "PO
  r_vtci-low = '60'. APPEND r_vtci.   "Park
*  r_vtci-low = '58'. APPEND r_vtci.   "DP Req.(ZERO amt)

  r_vtiv-low = '54'. APPEND r_vtiv.   "invoice
  r_vtiv-low = '66'. APPEND r_vtiv.   "transfer
  r_vtiv-low = '95'. APPEND r_vtiv.   "co posting (secondary)

  r_vtpy-low = '57'. APPEND r_vtpy.   "Pay
  r_vtpy-low = '61'. APPEND r_vtpy.   "DP

* amt type
*0100     Original
*0110     Approved in Workflow        (Workflow
*0120     Not approved in Workflow    (Workflow
*0150     Change
*0200     Reduce
*0250     Paid
*0300     Previous year commt carryforward
*0350     Following year commt carryforward
*0360     Following year actual carryforward
*0400     Lock entry
*0500     Adjustment by follow-on document

  REFRESH: r_atcf.
  r_atcf-option = 'EQ'. r_atcf-sign = 'I'.
  r_atcf-low = '0300'.  APPEND r_atcf.
  r_atcf-low = '0350'.  APPEND r_atcf.
  r_atcf-low = '0360'.  APPEND r_atcf.


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


  INCLUDE lfmauequ.
*&---------------------------------------------------------------------*
*&      Form  get_master_info
*&---------------------------------------------------------------------*
FORM get_master_info.
  DATA: l_berec.
  DATA : l_index LIKE sy-tabix.

* commitment Item
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_fmci
     FROM fmci
     WHERE fikrs =  p_fik
       AND fipos IN p_fipos
       AND potyp IN p_knz.

* commitment item text
  LOOP AT it_fmci.
    SELECT SINGLE bezei  INTO it_fmci-bezei
       FROM fmcit
       WHERE spras = sy-langu
         AND fikrs = p_fik
         AND fipex = it_fmci-fipos.
    MODIFY it_fmci.
  ENDLOOP.

* Fund Center
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_fmfctr
     FROM fmfctr
     WHERE fikrs =  p_fik
       AND fictr IN p_fictr.
*{ 09/22/11 Paul delete it_fmfctr logic
* Fund Center Hiearchy (select end node only)

  LOOP AT it_fmfctr.

    l_index = sy-tabix.
*    SELECT SINGLE * FROM fmhictr
*       WHERE ctr_objnr = it_fmfctr-ctr_objnr.
*
*    IF fmhictr-parent_obj = space.
*      IF e_level <> 'ADM'.
*        DELETE it_fmfctr.
*      ENDIF.
*    ELSE.
*-- end node...
*--- check FundCenter Auth.

**--> Fund Center hierachy structure change
**--> 'HMMA' manually delete.(11/26/2011 BY KDM)
    IF it_fmfctr-fictr = 'HMMA'.
      DELETE it_fmfctr INDEX l_index.
    ENDIF.

      SELECT SINGLE * FROM fmfctr
          WHERE fikrs = p_fik
            AND fictr = it_fmfctr-fictr
            AND datbis >= sy-datum.

      CALL FUNCTION 'FMAU_AUTHORITY_FIFM'
           EXPORTING
                I_ACTVT       = FMAU_DISPLAY
                I_AUTH_OBJECT = 'F_FICA_CTR'
                I_FIKRS       = p_FIK
                I_FMFCTR      = fmfctr
                I_DATE        = SY-DATLO
           IMPORTING
                EX_AUTH       = L_BEREC.
      IF L_BEREC = SPACE.
        delete it_fmfctr  INDEX l_index.
      endif.
      AUTHORITY-CHECK OBJECT 'Z_FICTR'
               ID 'FM_FIKRS'   FIELD p_fik
               ID 'FM_FICTR'   FIELD fmfctr-fictr.
      IF sy-subrc <> 0.
        DELETE it_fmfctr  INDEX l_index.
      ENDIF.

*    ENDIF.
  ENDLOOP.
*}

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
    READ TABLE it_fmci  WITH KEY fipos = it_fmit-rfipex.
    CHECK sy-subrc = 0.

    it_tab-fipos = it_fmit-rfipex.
    it_tab-fictr = it_fmit-rfistl.
    it_tab-geber = it_fmit-rfonds.
    it_tab-bezei  = it_fmci-bezei .

* get budget profile
    PERFORM get_budget_period USING it_tab-fictr
                                    it_tab-fipos
                                    it_tab-geber
                              CHANGING it_tab-profil.
    CHECK it_tab-profil IN p_prof.


* Amount Type
*0100     Original
*0110     Approved in Workflow        (Workflow
*0120     Not approved in Workflow    (Workflow
*0150     Change
*0200     Reduce
*0250     Paid
*0300     Previous year commt carryforward
*0350     Following year commt carryforward
*0360     Following year actual carryforward
*0400     Lock entry
*0500     Adjustment by follow-on document

*--carryforward: commitment -> tslvt

* commitment
*    IF     it_fmit-rwrttp IN r_vtci.
    IF     it_fmit-rwrttp = '50'.
      PERFORM move_amount USING 'C1' '+'.
      PERFORM move_amount USING 'C9' '+'.
      PERFORM move_amount USING 'E1' '+'.
    ELSEIF it_fmit-rwrttp = '51'.

      PERFORM move_amount USING 'C2' '+'.
      PERFORM move_amount USING 'C9' '+'.
      PERFORM move_amount USING 'E1' '+'.

    ELSEIF it_fmit-rwrttp = '60'.
      PERFORM move_amount USING 'C3' '+'.
      PERFORM move_amount USING 'C9' '+'.
      PERFORM move_amount USING 'E1' '+'.

* invoice
*  r_vtiv-low = '54'. APPEND r_vtiv.   "invoice
*  r_vtiv-low = '66'. APPEND r_vtiv.   "transfer
*  r_vtiv-low = '95'. APPEND r_vtiv.   "co posting (secondary)

*    ELSEIF it_fmit-rwrttp IN r_vtiv
    ELSEIF it_fmit-rwrttp = '54'
    AND  ( it_fmit-rbtart <> '0200' ).  "reduce
      PERFORM move_amount USING 'D1' '+'.
      PERFORM move_amount USING 'D9' '+'.
      PERFORM move_amount USING 'E1' '+'.

    ELSEIF it_fmit-rwrttp = '66'        "Transfer postings
    AND  ( it_fmit-rbtart <> '0200' ).  "reduce
      PERFORM move_amount USING 'D2' '+'.
      PERFORM move_amount USING 'D9' '+'.
      PERFORM move_amount USING 'E1' '+'.

    ELSEIF it_fmit-rwrttp = '95'       "CO repost
    AND  ( it_fmit-rbtart <> '0200' ).  "reduce
      PERFORM move_amount USING 'D3' '+'.
      PERFORM move_amount USING 'D9' '+'.
      PERFORM move_amount USING 'E1' '+'.

*FM posting
    ELSEIF it_fmit-rwrttp = '81'        "FM reservation(commitment)
    AND  ( it_fmit-rbtart <> '0200' ).  "reduce
      PERFORM move_amount USING 'C5' '+'.
      PERFORM move_amount USING 'D9' '+'.
      PERFORM move_amount USING 'E1' '+'.

    ELSEIF it_fmit-rwrttp = '64'        "Fund Trf (actual)
    AND  ( it_fmit-rbtart <> '0200' ).  "reduce
      PERFORM move_amount USING 'D5' '-'.
      PERFORM move_amount USING 'D9' '-'.
      PERFORM move_amount USING 'E1' '-'.

* Payment
    ELSEIF ( it_fmit-rwrttp IN r_vtpy OR  it_fmit-rbtart = '0250' ).
*       or it_fmit-RWRTTP = '57' or it_fmit-RWRTTP = '58'.
      PERFORM move_amount USING 'PY' '+'.

    ENDIF.

    CLEAR it_tab.
  ENDLOOP.
ENDFORM.                    " get_fmit
*&---------------------------------------------------------------------*
*&      Form  get_bbpe
*&---------------------------------------------------------------------*
FORM get_bbpe.
  DATA: w_itab LIKE it_tab.

  check not it_fmfctr[] is initial.
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
    CLEAR: w_itab.
* check auth
    READ TABLE it_fmfctr WITH KEY ctr_objnr = it_bppe-objnr.
    CHECK sy-subrc = 0.

    w_itab-fictr = it_fmfctr-fictr.
    w_itab-geber = it_bppe-geber.


* filter for commitment item
    READ TABLE it_fmci  WITH KEY posit = it_bppe-posit.
    CHECK sy-subrc = 0.
*   check it_tab-fipos in p_fipos.

    w_itab-fipos   = it_fmci-fipos.
    w_itab-bezei  = it_fmci-bezei .

    PERFORM get_budget_period USING w_itab-fictr
                                    w_itab-fipos
                                    w_itab-geber
                              CHANGING w_itab-profil.
    CHECK w_itab-profil IN p_prof.

    MOVE-CORRESPONDING it_bppe TO w_itab.


* display budget activity (sort key + activity)
    CASE it_bppe-vorga.
      WHEN 'KBUD'.
        it_tab = w_itab.
        PERFORM collect_tot USING 'B0'.  "orgin
        PERFORM collect_tot USING 'B9'.  "Current
        PERFORM clear_tot.
      WHEN 'KBN0'.
        it_tab = w_itab.
        PERFORM collect_tot USING 'B1'.  "supp
        PERFORM collect_tot USING 'B9'.  "Current
        PERFORM clear_tot.
      WHEN 'KBR0'.
        it_tab = w_itab.
        PERFORM collect_tot USING 'B2'.   "return
        PERFORM collect_tot USING 'B9'.   "Current
        PERFORM clear_tot.
      WHEN 'KBUS' OR 'KBUE'.
*Trf=KBUS, Trf C/F(KBU2), Trf Adv (KBU3)
        it_tab = w_itab.
        PERFORM collect_tot USING 'B3'.   "TRF
        PERFORM collect_tot USING 'B9'.   "Current
        PERFORM clear_tot.

*budget c/f (sender,receiver), Trf c/f=KBU2, Trf Adv=KBU3
      WHEN 'KBW1' OR 'KBW2' OR 'KBU2' OR 'KBU3'.
        it_tab = w_itab.

*FIXME - ANDY
        CASE it_bppe-vorga.
          WHEN 'KBW1'.  it_tab-wtp13 = it_tab-wtp01.  "Sending
          WHEN 'KBU3'.  it_tab-wtp13 = it_tab-wtp01.  "Sending

          WHEN 'KBW2'.  it_tab-tslvt = it_tab-wtp01.  "Receiving
          WHEN 'KBU2'.  it_tab-tslvt = it_tab-wtp01.  "Receiving
        ENDCASE.


        IF it_bppe-vorga = 'KBW1'. "Sender year
          it_tab-wtp01 = - it_tab-wtp01.
          PERFORM collect_tot USING 'BX'.  "back to release
        ENDIF.

        CLEAR: it_tab-wtp01.
        PERFORM collect_tot USING 'AC'.  "C/F
        PERFORM collect_tot USING 'B9'.  "current

        PERFORM clear_tot.

*.... release
      WHEN 'KBFR'.  "Amt Type = 46
        it_tab = w_itab.
        PERFORM collect_tot USING 'BX'.  "release
        PERFORM clear_tot.

      WHEN OTHERS.
        IF it_bppe-wrttp = '43'.  "Budget
          it_tab = w_itab.
          PERFORM collect_tot USING 'XZ'.
          PERFORM clear_tot.
        ENDIF.
    ENDCASE.

  ENDLOOP.
ENDFORM.                    " get_bbpe
*&---------------------------------------------------------------------*
*&      Form  get_budget_period
*&---------------------------------------------------------------------*
FORM get_budget_period USING    f_fictr f_fipos f_geber
                       CHANGING f_profil.
  READ TABLE it_fmci  WITH KEY fipos = f_fipos.
  IF sy-subrc = 0.
    PERFORM determine_profile_fs USING    p_fik
                                          f_fictr
                                          it_fmci-posit
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
*{ 09/22/11 Paul insert I_FAREA
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
*&      Form  move_amount
*&---------------------------------------------------------------------*
FORM move_amount USING f_vor
                       f_sign.
  DATA: l_sn TYPE i.
  IF f_sign = '-'. l_sn = -1. ELSE. l_sn = 1. ENDIF.

  it_tab-vor = f_vor.

*FIXME...
* it_tab-tslvt =  it_fmit-tslvt + it_fmit-hsl13.

  it_tab-wtp01 =  it_fmit-hsl01 * l_sn.
  it_tab-wtp02 =  it_fmit-hsl02 * l_sn.
  it_tab-wtp03 =  it_fmit-hsl03 * l_sn.
  it_tab-wtp04 =  it_fmit-hsl04 * l_sn.
  it_tab-wtp05 =  it_fmit-hsl05 * l_sn.
  it_tab-wtp06 =  it_fmit-hsl06 * l_sn.
  it_tab-wtp07 =  it_fmit-hsl07 * l_sn.
  it_tab-wtp08 =  it_fmit-hsl08 * l_sn.
  it_tab-wtp09 =  it_fmit-hsl09 * l_sn.
  it_tab-wtp10 =  it_fmit-hsl10 * l_sn.
  it_tab-wtp11 =  it_fmit-hsl11 * l_sn.
  it_tab-wtp12 =  it_fmit-hsl12 * l_sn.

  it_tab-wtp13 =  it_fmit-hsl13 * l_sn.  "GO to Next year

  IF it_fmit-rbtart EQ '0350'.
    it_tab-tslvt =  it_fmit-tslvt * l_sn.
  ELSE.
    it_tab-wtp01 =  it_tab-wtp01 + it_fmit-tslvt * l_sn.
  ENDIF.


*TEMP
*  if it_tab-tslvt <> 0.
*    break-point.
*  endif.
  it_tab-wtp00 =
               + it_tab-wtp01 + it_tab-wtp02 + it_tab-wtp03
               + it_tab-wtp04 + it_tab-wtp05 + it_tab-wtp06
               + it_tab-wtp07 + it_tab-wtp08 + it_tab-wtp09
               + it_tab-wtp10 + it_tab-wtp11 + it_tab-wtp12.

  it_tab-wtp99 = it_tab-wtp00 + it_tab-tslvt.
  COLLECT it_tab.
ENDFORM.                    " move_amount
*&---------------------------------------------------------------------*
*&      Form  collect_tot
*&---------------------------------------------------------------------*
FORM collect_tot  USING f_activity.
  it_tab-wtp00 =
             + it_tab-wtp01 + it_tab-wtp02 + it_tab-wtp03
             + it_tab-wtp04 + it_tab-wtp05 + it_tab-wtp06
             + it_tab-wtp07 + it_tab-wtp08 + it_tab-wtp09
             + it_tab-wtp10 + it_tab-wtp11 + it_tab-wtp12.

  it_tab-wtp99 = it_tab-wtp00 + it_tab-tslvt.
  it_tab-vor   = f_activity.

  COLLECT it_tab.

ENDFORM.                    " collect_tot
*&---------------------------------------------------------------------*
*&      Form  clear_tot
*&---------------------------------------------------------------------*
FORM clear_tot.
  CLEAR: it_tab-wtp99,  it_tab-wtp00,  it_tab-wtp13,
         it_tab-wtp01,  it_tab-wtp02,  it_tab-wtp03,
         it_tab-wtp04,  it_tab-wtp05,  it_tab-wtp06,
         it_tab-wtp07,  it_tab-wtp08,  it_tab-wtp09,
         it_tab-wtp10,  it_tab-wtp11,  it_tab-wtp12.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  calculate_available_budget
*&---------------------------------------------------------------------*
FORM calculate_available_budget.
  LOOP AT it_tab
     WHERE vor = 'BX'  "released
        OR vor = 'E1'.                                      "C1 + D1.

    CLEAR: it_tab-wtp13.

    it_tab-vor = 'EE'.
    COLLECT it_tab.
  ENDLOOP.

ENDFORM.                    " calculate_available_budget
*&---------------------------------------------------------------------*
*&      Form  fill_output
*&---------------------------------------------------------------------*
FORM fill_output.
  CLEAR : it_output, it_output[].

*  w_rt_line-key  = '0'.  w_rt_line-text = 'Released+Actual'.
*  w_rt_line-key  = '1'.  w_rt_line-text = 'Budget'.
*  w_rt_line-key  = '2'.  w_rt_line-text = 'Commitment'.
*  w_rt_line-key  = '3'.  w_rt_line-text = 'Invoice'.
*  w_rt_line-key  = '4'.  w_rt_line-text = 'Invoice+Commitment'.
*  w_rt_line-key  = '5'.  w_rt_line-text = 'Payment'.
*  w_rt_line-key  = '9'.  w_rt_line-text = 'All'.

  IF p_rt = '0'.
    DELETE it_tab
       WHERE vor <> 'BX' AND vor(1) <> 'E'.
  ENDIF.
  IF p_rt = '1'.
    DELETE it_tab
       WHERE vor <> 'AC' AND vor(1) <> 'B'.
  ENDIF.
  IF p_rt = '2'.
    DELETE it_tab WHERE vor(1) <> 'C'.
  ENDIF.
  IF p_rt = '3'.
    DELETE it_tab WHERE vor(1) <> 'D'.
  ENDIF.
  IF p_rt = '4'.
    DELETE it_tab WHERE vor(1) <> 'C' AND vor(1) <> 'D'.
  ENDIF.
  IF p_rt = '5'.
    DELETE it_tab WHERE vor <> 'PY'.
  ENDIF.

  LOOP AT it_tab.
    MOVE-CORRESPONDING it_tab TO it_output.

    CASE it_tab-vor.
      WHEN 'AC'.   WRITE: '..C/F       '  TO it_output-act.
      WHEN 'B0'.   MOVE   '..Original  '  TO it_output-act.
      WHEN 'B1'.   WRITE: '..Supplement'  TO it_output-act.
      WHEN 'B2'.   WRITE: '..Return    '  TO it_output-act.
      WHEN 'B3'.   WRITE: '..Transfer  '  TO it_output-act.
      WHEN 'B9'.   WRITE: '.Current    '  TO it_output-act.
      WHEN 'BX'.
        WRITE: 'Released    '  TO it_output-act.
        MOVE 'C410'            TO it_output-color_line.
      WHEN 'C1'.   WRITE: '..PR        '  TO it_output-act.
      WHEN 'C2'.   WRITE: '..PO        '  TO it_output-act.
      WHEN 'C3'.   WRITE: '..FI Park   '  TO it_output-act.
      WHEN 'C5'.   WRITE: '..FM Reserv '  TO it_output-act.
      WHEN 'C9'.   WRITE: '.Commitment '  TO it_output-act.
      WHEN 'D1'.   WRITE: '..Invoice   '  TO it_output-act.
      WHEN 'D2'.   WRITE: '..Transfer  '  TO it_output-act.
      WHEN 'D3'.   WRITE: '..Repost    '  TO it_output-act.
      WHEN 'D5'.   WRITE: '..Fund Trf  '  TO it_output-act.
      WHEN 'D9'.   WRITE: '.Actual     '  TO it_output-act.
      WHEN 'E1'.
        WRITE: 'Assigned    '  TO it_output-act.
** On 08/30/13
        MOVE 'C110'            TO it_output-color_line.
*        MOVE 'C410'            TO it_output-color_line.
** End on 08/30/13
      WHEN 'EE'.
        WRITE: 'Available   '  TO it_output-act.
        MOVE 'C410'            TO it_output-color_line.
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
  'BEZEI'     'Description'   '20' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'GEBER'     'Fund'          '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'PROFIL'    'Profile'       '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'VOR'       'Sort'          '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'ACT'       'Category'      '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'TSLVT'     'C/F'           '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'WTP00'     'JAN-DEC'       '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
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
  'WTP12'     'DEC'           '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'WTP13'     'NextYR'        '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'WTP99'     'Total'         '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' '.

* layout
  PERFORM fill_layout.
* build event table.
  PERFORM alv_build_eventtab CHANGING w_eventcat.
* build table of sort information.
  PERFORM alv_build_sorttab CHANGING w_sortcat.
* set callback program
  g_repid = sy-repid.

  IF p_disp = 'GRID'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = g_repid
        it_fieldcat              = gt_fieldcat
        is_layout                = gs_layout
        it_events                = w_eventcat
        it_sort                  = w_sortcat
        i_callback_user_command  = c_fnam_cos_user_command
        i_callback_pf_status_set = c_fnam_cos_pf_status
        i_save                   = 'A'
      TABLES
        t_outtab                 = it_output
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
  ELSE.
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program       = g_repid
        it_fieldcat              = gt_fieldcat
        is_layout                = gs_layout
        it_events                = w_eventcat
        it_sort                  = w_sortcat
        i_callback_user_command  = c_fnam_cos_user_command
        i_callback_pf_status_set = c_fnam_cos_pf_status
        i_save                   = 'A'
      TABLES
        t_outtab                 = it_output
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.

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
*&--------------------------------------------------------------*
*& Form fill_layout
*&--------------------------------------------------------------*
FORM fill_layout.
* Field that identify color line in internal table
*  MOVE 'COLOR_LINE' TO gs_layout-info_fieldname.

* Field that identify cell color in inetrnal table
*  MOVE 'COLOR_CELL' TO gs_layout-coltab_fieldname.
ENDFORM. " fill_layout
*&---------------------------------------------------------------------*
*&      Form  ALV_BUILD_EVENTTAB
*&---------------------------------------------------------------------*
FORM alv_build_eventtab CHANGING et_alv_events TYPE slis_t_event.

  DATA: ls_alv_events    TYPE slis_alv_event,
        l_dummy_ucomm    LIKE sy-ucomm,
        l_dummy_selfield TYPE slis_selfield,
        l_dummy_excl_tab TYPE slis_t_extab.


  REFRESH: et_alv_events.

** event 'TOP_OF_LIST'.
*  CLEAR ls_alv_events.
*  ls_alv_events-name = slis_ev_top_of_list.
*  ls_alv_events-form = 'ALV_TOP_OF_LIST'.
*  APPEND ls_alv_events TO et_alv_events.
*
** event 'END_OF_LIST'.
*  CLEAR ls_alv_events.
*  ls_alv_events-name = slis_ev_end_of_list.
*  ls_alv_events-form = 'ALV_END_OF_LIST'.
*  APPEND ls_alv_events TO et_alv_events.

* event 'PF_STATUS_SET'.
  CLEAR ls_alv_events.
  ls_alv_events-name = slis_ev_pf_status_set.
  ls_alv_events-form = 'ALV_SET_PF_STATUS'.
  APPEND ls_alv_events TO et_alv_events.

* event 'USER_COMMAND'.
  CLEAR ls_alv_events.
  ls_alv_events-name = slis_ev_user_command.
  ls_alv_events-form = 'ALV_USER_COMMAND'.
  APPEND ls_alv_events TO et_alv_events.

* callback forms.
  IF 1 = 0.
*    PERFORM alv_top_of_list.
*    PERFORM alv_end_of_list.
    PERFORM alv_set_pf_status USING l_dummy_excl_tab.
    PERFORM alv_user_command  USING l_dummy_ucomm
                                    l_dummy_selfield.

  ENDIF.
ENDFORM.                               " ALV_BUILD_EVENTTAB
*&---------------------------------------------------------------------*
*&      Form  ALV_BUILD_SORTTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_ALV_SORT  text
*----------------------------------------------------------------------*
FORM alv_build_sorttab CHANGING et_alv_sort TYPE slis_t_sortinfo_alv.

  DATA: ls_alv_sort LIKE LINE OF et_alv_sort.

  REFRESH: et_alv_sort[].

  CLEAR: ls_alv_sort.
  ls_alv_sort-up        = 'X'.

  ls_alv_sort-spos      = '01'.
  ls_alv_sort-fieldname = 'FICTR'.
  INSERT ls_alv_sort INTO TABLE et_alv_sort.

  ls_alv_sort-spos      = '02'.
  ls_alv_sort-fieldname = 'FIPOS'.
  INSERT ls_alv_sort INTO TABLE et_alv_sort.

  ls_alv_sort-spos      = '03'.
  ls_alv_sort-fieldname = 'BEZEI'.
  INSERT ls_alv_sort INTO TABLE et_alv_sort.

  ls_alv_sort-spos      = '04'.
  ls_alv_sort-fieldname = 'PROFIL'.
  INSERT ls_alv_sort INTO TABLE et_alv_sort.

  ls_alv_sort-spos      = '05'.
  ls_alv_sort-fieldname = 'VOR'.
  INSERT ls_alv_sort INTO TABLE et_alv_sort.

  ls_alv_sort-spos      = '06'.
  ls_alv_sort-fieldname = 'ACT'.
  INSERT ls_alv_sort INTO TABLE et_alv_sort.


ENDFORM.                               " ALV_BUILD_SORTTAB
*---------------------------------------------------------------------*
*       FORM ALV_SET_PF_STATUS                                        *
*---------------------------------------------------------------------*
*       for ALV processing                                            *
*---------------------------------------------------------------------*
FORM alv_set_pf_status USING excl_tab TYPE slis_t_extab.

  DATA: l_excl_tab TYPE slis_extab.

*  IF number_of_messages IS INITIAL.
**   No messages reported -> deactivate message button
*    CLEAR: l_excl_tab.
*    l_excl_tab-fcode = c_fcode_error_button.
*    INSERT l_excl_tab INTO TABLE excl_tab.
*  ENDIF.

  SET PF-STATUS 'ALV_STATUS' EXCLUDING excl_tab.

ENDFORM.                               " alv_set_pf_status
*---------------------------------------------------------------------*
*       FORM ALV_USER_COMMAND                                         *
*---------------------------------------------------------------------*
*       user command for ALV processing                               *
*---------------------------------------------------------------------*
FORM alv_user_command USING r_ucomm     LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.

  CHECK r_ucomm = '&IC1'.                       "Pick

  READ TABLE it_output INDEX rs_selfield-tabindex INTO g_output.
  CHECK sy-subrc = 0.

  CASE g_output-vor.

* budget report
    WHEN 'AC'.
      REFRESH: r_gjahr.
      r_gjahr-sign = 'I'.   r_gjahr-option = 'EQ'.
      r_gjahr-low  = p_gjr - 1. APPEND r_gjahr.    "Closing year
*{ 09/21/11 Paul rffmep1g -> rffmep1gx
*                fincd -> fonds // fipos -> fipex
      SUBMIT rffmep1gx   "Commit/Budget C/F
*              WITH p_cf_gja IN r_gjahr
              WITH p_cf_gja eq p_gjr
              WITH s_fictr = g_output-fictr
              WITH s_fikrs = p_fik
*              WITH s_fincd = g_output-geber
*              WITH s_fipos = g_output-fipos
              WITH s_fonds = g_output-geber
              WITH s_fipex = g_output-fipos
      AND RETURN.
    WHEN 'BX'.
      REFRESH: r_gjahr.
      r_gjahr-sign = 'I'.   r_gjahr-option = 'EQ'.
      r_gjahr-low  = p_gjr. APPEND r_gjahr.    "Closing year
      SUBMIT rffmep1bX  "annual budget lineitem
              WITH s_fictr = g_output-fictr
              WITH s_fikrs = p_fik
*              WITH s_fincd = g_output-geber
*              WITH s_fipos = g_output-fipos
              WITH s_fonds = g_output-geber
              WITH s_fipex = g_output-fipos
              WITH s_gjahr IN r_gjahr
       AND RETURN.

    WHEN 'B9'.
      REFRESH: r_gjahr.
      r_gjahr-sign = 'I'.   r_gjahr-option = 'EQ'.
      r_gjahr-low  = p_gjr. APPEND r_gjahr.    "Closing year
      SUBMIT rffmep4bx  "Period budget
              WITH s_fictr = g_output-fictr
              WITH s_fikrs = p_fik
*              WITH s_fincd = g_output-geber
*              WITH s_fipos = g_output-fipos
              WITH s_fonds = g_output-geber
              WITH s_fipex = g_output-fipos
              WITH s_gjahr IN r_gjahr
       AND RETURN.

* Actual/Commitment
*    WHEN 'E9' OR 'D9' OR 'C9'.     "" UD1K953437
    WHEN 'E9' .                     "" UD1K953437
*Begin of Changes UD1K922250
*      SUBMIT rffmto10  "commit/actual total
*              WITH p_fyr_fr = p_gjr
*              WITH p_fyr_to = p_gjr
*              WITH p_per_fr = '01'
*              WITH p_per_to = '13'    "C/F
*              WITH s_fictr = g_output-fictr
*              WITH s_fikrs = p_fik
*              WITH s_fincd = g_output-geber
*              WITH s_fipos = g_output-fipos
*      AND RETURN.
*      Call ZRFIF07
*      REFRESH: r_gjahr.
*      r_gjahr-sign = 'I'.   r_gjahr-option = 'EQ'.
*      r_gjahr-low  = p_gjr. APPEND r_gjahr.    " year
*      SUBMIT zrfif07  "commit/actual total
*              WITH s_gjahr in r_gjahr
*              WITH p_fikrs = p_fik
*              WITH s_fonds = g_output-geber
*              WITH s_fipex = g_output-fipos
*              WITH s_fistl = g_output-fictr
*              WITH p_p1 EQ 'X'
*      AND RETURN.

* refer : RFFMEPGAX

**{// 11/23/11 Shin Change rffmep1a->ZRFIF07N
      REFRESH: r_gjahr.
      r_gjahr-sign = 'I'.   r_gjahr-option = 'EQ'.
      r_gjahr-low  = p_gjr. APPEND r_gjahr.    " year
      SUBMIT ZRFIF07N  "commit/actual lineitem
*            WITH s_gjahr  IN r_gjahr
*             WITH s_perio  IN r_perio
             WITH s_fistl  = g_output-fictr
             WITH p_fikrs  = p_fik
             WITH p_gjahr  = p_gjr
*              WITH s_fincd  = g_output-geber
*              WITH s_fipos  = g_output-fipos
              WITH s_fonds = g_output-geber
              WITH s_fipex = g_output-fipos
      AND RETURN.

* End of  changes  UD1K922250

    WHEN OTHERS.
* show detail actual
      DATA: l_fr LIKE fmfi-perio.
      CASE rs_selfield-fieldname.
        WHEN 'WTP01'. PERFORM call_assigned USING '01'.
        WHEN 'WTP02'. PERFORM call_assigned USING '02'.
        WHEN 'WTP03'. PERFORM call_assigned USING '03'.
        WHEN 'WTP04'. PERFORM call_assigned USING '04'.
        WHEN 'WTP05'. PERFORM call_assigned USING '05'.
        WHEN 'WTP06'. PERFORM call_assigned USING '06'.
        WHEN 'WTP07'. PERFORM call_assigned USING '07'.
        WHEN 'WTP08'. PERFORM call_assigned USING '08'.
        WHEN 'WTP09'. PERFORM call_assigned USING '09'.
        WHEN 'WTP10'. PERFORM call_assigned USING '10'.
        WHEN 'WTP11'. PERFORM call_assigned USING '11'.
        WHEN 'WTP12'. PERFORM call_assigned USING '12'.
        WHEN OTHERS.  PERFORM call_assigned USING '13'.
      ENDCASE.
  ENDCASE.

ENDFORM.                               "ALV_USER_COMMAND

*ALV
*&---------------------------------------------------------------------*
*&      Form  call_assigned
*&---------------------------------------------------------------------*
FORM call_assigned USING    value(f_fr).

  RANGES : r_wrttp FOR FMEP-WRTTP.

  REFRESH: r_gjahr, r_perio.
  r_gjahr-sign = 'I'.   r_gjahr-option = 'EQ'.
  r_perio-sign = 'I'.   r_perio-option = 'EQ'.

  r_gjahr-low  = p_gjr. APPEND r_gjahr.
  r_perio-low  = f_fr.  APPEND r_perio.

  CASE g_output-vor.
* commitment
** Furong on 07/15/14 (
*    WHEN 'C1' OR 'C2' OR 'C3' OR 'C9'.
    WHEN 'C1' OR 'C2' OR 'C3' OR 'C9' or 'C5'.
** )
**{ 09/21/11 Paul RFFMEP1B -> RFFMEP1BX
**                fincd -> fonds // fipos -> fipex
*      SUBMIT RFFMEP1BX
*        WITH s_gjahr  IN r_gjahr
*        WITH s_perio  IN r_perio
*        WITH s_fictr  = g_output-fictr
*        WITH s_fikrs  = p_fik
**              WITH s_fincd  = g_output-geber
**              WITH s_fipos  = g_output-fipos
*        WITH s_fonds = g_output-geber
*        WITH s_fipex = g_output-fipos
*        AND RETURN.

**{ 12/07/11 KDM RFFMEP1B -> RFFMEP1BX -> RFFMEPGAX
*      IF g_output-geber <> space.
*        r_funds+0(3) = 'IEQ'.
*        r_funds-low = g_output-geber.
*        APPEND r_funds.
*      ENDIF.
      IF g_output-vor = 'C1'.
        r_wrttp+0(3) = 'IEQ'.
        r_wrttp-low  = '50'.
        APPEND r_wrttp.
      ELSEIF g_output-vor = 'C2'.
        r_wrttp+0(3) = 'IEQ'.
        r_wrttp-low  = '51'.
        APPEND r_wrttp.
      ELSEIF g_output-vor = 'C3'.
        r_wrttp+0(3) = 'IEQ'.
        r_wrttp-low  = '60'.
        APPEND r_wrttp.
** Furong on 07/15/14 (
      ELSEIF g_output-vor = 'C5'.
        r_wrttp+0(3) = 'IEQ'.
        r_wrttp-low  = '81'.
        APPEND r_wrttp.
** )
      ELSEIF g_output-vor = 'C9'.
        r_wrttp+0(3) = 'IEQ'.
        r_wrttp-low  = '50'.
        APPEND r_wrttp.
        r_wrttp-low  = '51'.
        APPEND r_wrttp.
        r_wrttp-low  = '60'.
        APPEND r_wrttp.
      ENDIF.

      SUBMIT RFFMEPGAX  "annual budget lineitem
              WITH s_fictr = g_output-fictr
              WITH s_fikrs-low = p_fik
              WITH s_fonds = g_output-geber
              WITH s_fipex = g_output-fipos
              WITH p_fyr_fr = p_gjr
              WITH p_per_fr = '01'
              WITH p_fyr_t0 = p_gjr
              WITH p_per_to = '12'
              WITH p_maxsel = space
              WITH s_wrttp in r_wrttp
       AND RETURN.

* invoice
    WHEN 'D1'.
**{// 12/08/11 KDM  Change Parameter

      r_wrttp+0(3) = 'IEQ'.
      r_wrttp-low  = '54'.
      APPEND r_wrttp.

      SUBMIT RFFMEPGAX  "annual budget lineitem
              WITH s_fictr = g_output-fictr
              WITH s_fikrs-low = p_fik
              WITH s_fonds = g_output-geber
              WITH s_fipex = g_output-fipos
              WITH p_fyr_fr = p_gjr
              WITH p_per_fr = '01'
              WITH p_fyr_t0 = p_gjr
              WITH p_per_to = '12'
              WITH p_maxsel = space
              WITH s_wrttp in r_wrttp
       AND RETURN.

    WHEN 'D2'.
**{// 09/23/11 PuAL Change rffmep1f->RFFMEPGAX
**{// 12/08/11 KDM  Change Parameter

        r_wrttp+0(3) = 'IEQ'.
        r_wrttp-low  = '66'.
        APPEND r_wrttp.

      SUBMIT RFFMEPGAX  "annual budget lineitem
              WITH s_fictr = g_output-fictr
              WITH s_fikrs-low = p_fik
              WITH s_fonds = g_output-geber
              WITH s_fipex = g_output-fipos
              WITH p_fyr_fr = p_gjr
              WITH p_per_fr = '01'
              WITH p_fyr_t0 = p_gjr
              WITH p_per_to = '12'
              WITH p_maxsel = space
              WITH s_wrttp in r_wrttp
       AND RETURN.

    WHEN 'D9'.
**{// 12/08/11 KDM  Change Parameter

        r_wrttp+0(3) = 'IEQ'.
        r_wrttp-low  = '54'.   ""Invoice
        APPEND r_wrttp.
        r_wrttp-low  = '66'.   ""Transfer Postings
        APPEND r_wrttp.

      SUBMIT RFFMEPGAX  "annual budget lineitem
              WITH s_fictr = g_output-fictr
              WITH s_fikrs-low = p_fik
              WITH s_fonds = g_output-geber
              WITH s_fipex = g_output-fipos
              WITH p_fyr_fr = p_gjr
              WITH p_per_fr = '01'
              WITH p_fyr_t0 = p_gjr
              WITH p_per_to = '12'
              WITH p_maxsel = space
              WITH s_wrttp in r_wrttp
       AND RETURN.

* commitment/actual
    WHEN 'E1'.
**{// 09/23/11 PuAL Change rffmep1a->RFFMAV01X
*      SUBMIT RFFMAV01X  "commit/actual lineitem(too long run)
*             WITH S_GJ_BUD  IN r_gjahr
**             WITH s_perio  IN r_perio
*             WITH s_fictr  = g_output-fictr
*             WITH s_fikrs  = p_fik
**              WITH s_fincd  = g_output-geber
**              WITH s_fipos  = g_output-fipos
*              WITH s_fonds = g_output-geber
*              WITH s_fipex = g_output-fipos
*      AND RETURN.
**{// 11/23/11 Shin Change rffmep1a->ZRFIF07N
      SUBMIT ZRFIF07N  "commit/actual lineitem
             WITH p_gjahr  = p_gjr
*            WITH s_gjahr  IN r_gjahr
*             WITH s_perio  IN r_perio
             WITH s_fistl  = g_output-fictr
             WITH p_fikrs  = p_fik
*              WITH s_fincd  = g_output-geber
*              WITH s_fipos  = g_output-fipos
              WITH s_fonds = g_output-geber
              WITH s_fipex = g_output-fipos
      AND RETURN.
  ENDCASE.

ENDFORM.                    " call_assigned
