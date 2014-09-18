*&---------------------------------------------------------------------*
*& Program ID     : ZFMR0003                                           *
*& Program Name   : Budget Comp. Orig/Releas/Inv...                    *
*& Created by     : YN.Kim                                             *
*& Created on     : 08/18/2011                                         *
*& Reference Pgm  : ZFMR0001                                           *
*&                                                                     *
*& Modification Log                                                    *
*----------------------------------------------------------------------*
* DATE      |  NAME          |Transport | Issue #  |      DESC         *
*----------------------------------------------------------------------*
*                                                                      *
*&=====================================================================*
REPORT  zfmr0003 NO STANDARD PAGE HEADING.

INCLUDE <icon>.
INCLUDE zfm_auth_form.

TYPE-POOLS: slis.
type-pools zfmcm.

DATA:repname  LIKE sy-repid,"flg_stp,
     fct TYPE slis_t_fieldcat_alv,
     fctd TYPE slis_t_fieldcat_alv,
     alv_variant LIKE disvariant,
     events  TYPE slis_t_event.
DATA:t99(99),
     fm_curr LIKE fm01-waers,fm_currt LIKE fm_curr,
     flg_ittd,lnl TYPE i,flg_cnt,
     itname TYPE slis_tabname VALUE 'IT_9000',"Letters must be Upper
     rg_am VALUE 'X',rg_om,tx_am(5),tx_om(2),
     it_fictr LIKE fmci-fictr OCCURS 1 WITH HEADER LINE,
     BEGIN OF it_gl OCCURS 1,
      saknr LIKE skb1-saknr,tx LIKE skat-txt20,
     END OF it_gl.
TABLES:ltdx,disvariant,bkpf,fm01,skb1,ska1,skat,fmcit.
TABLES: fmci, fmfctr, bppe, tbpfe, aufk, zsfm0008.
*---// Internal tables
DATA:BEGIN OF ittd OCCURS 1,
      t(99),
     END OF ittd,
     it_9000 LIKE zsfm0008 OCCURS 1 WITH HEADER LINE,
     BEGIN OF ita OCCURS 1,
      geber LIKE bppe-geber," fmit-RFONDS,  "zsfm0008-geber,
      fictr LIKE fmci-fictr,
      potyp LIKE fmci-potyp,
      fipex LIKE fmci-fipex,
      bezei LIKE zsfm0008-bezei,
*      profil like zsfm0008-profil, "polrok H,M-mesacne
      org_tot LIKE zsfm0008-total,
      cfd_tot like zsfm0008-total, "Carry forword
      rel_tot LIKE zsfm0008-total,
      com_tot LIKE zsfm0008-total,
      inv_tot LIKE zsfm0008-total,
      pay_tot LIKE zsfm0008-total,
      ava_tot LIKE zsfm0008-total,
*      waeras like zsfm0008-waers,
     END OF ita,
     ita_c like ita,
     BEGIN OF itag OCCURS 1,
      geber LIKE bppe-geber,
      fictr LIKE fmci-fictr,
      potyp LIKE fmci-potyp,
      fipex LIKE fmci-fipex,
      bezei LIKE zsfm0008-bezei,
      glac  LIKE skb1-saknr,
      gltx  LIKE skat-txt20,
      org_tot LIKE zsfm0008-total,
      cfd_tot like zsfm0008-total, "Carry forword
      rel_tot LIKE zsfm0008-total,
      com_tot LIKE zsfm0008-total,
      inv_tot LIKE zsfm0008-total,
      pay_tot LIKE zsfm0008-total,
      ava_tot LIKE zsfm0008-total,
     END OF itag,
     itag_c like itag,
     BEGIN OF itaa OCCURS 1,
      fipex LIKE fmci-fipex,
      ac1fc LIKE zsfm0008-total,
      or1fc LIKE zsfm0008-total,
      us1fc LIKE zsfm0008-total,
      df1fc LIKE zsfm0008-total,
      acnfc LIKE zsfm0008-total,
      ornfc LIKE zsfm0008-total,
      usnfc LIKE zsfm0008-total,
      dfnfc LIKE zsfm0008-total,
     END OF itaa,
     itaa_c like itaa,
     BEGIN OF itaag OCCURS 1,
      fipex LIKE fmci-fipex,
      glac  LIKE skb1-saknr,
      ac1fc LIKE zsfm0008-total,
      or1fc LIKE zsfm0008-total,
      us1fc LIKE zsfm0008-total,
      df1fc LIKE zsfm0008-total,
      acnfc LIKE zsfm0008-total,
      ornfc LIKE zsfm0008-total,
      usnfc LIKE zsfm0008-total,
      dfnfc LIKE zsfm0008-total,
     END OF itaag,
     itaag_c like itaag,
     BEGIN OF itaa6 OCCURS 1,
      fictr LIKE fmci-fictr,
      fipex LIKE fmci-fipex,
      ac1fc LIKE zsfm0008-total,
      or1fc LIKE zsfm0008-total,
      us1fc LIKE zsfm0008-total,
      df1fc LIKE zsfm0008-total,
     END OF itaa6,
     itaa6_c like itaa6,
     BEGIN OF itaa6g OCCURS 1,
      fictr LIKE fmci-fictr,
      fipex LIKE fmci-fipex,
      glac  LIKE skb1-saknr,
      ac1fc LIKE zsfm0008-total,
      or1fc LIKE zsfm0008-total,
      us1fc LIKE zsfm0008-total,
      df1fc LIKE zsfm0008-total,
     END OF itaa6g,
     itaa6g_c like itaa6g,
     BEGIN OF it3 OCCURS 1,
      geber LIKE bppe-geber,
      fictr LIKE fmci-fictr,
      fipex_gr3(3),
      bezei LIKE zsfm0008-bezei,
      org_tot LIKE zsfm0008-total,
      cfd_tot like zsfm0008-total, "Carry forword
      rel_tot LIKE zsfm0008-total,
      com_tot LIKE zsfm0008-total,
      inv_tot LIKE zsfm0008-total,
      pay_tot LIKE zsfm0008-total,
      ava_tot LIKE zsfm0008-total,
     END OF it3,
     it3_c like it3,
     BEGIN OF it3a OCCURS 1,
      fipex_gr3(3),
      ac1fc LIKE zsfm0008-total,
      or1fc LIKE zsfm0008-total,
      us1fc LIKE zsfm0008-total,
      df1fc LIKE zsfm0008-total,
      acnfc LIKE zsfm0008-total,
      ornfc LIKE zsfm0008-total,
      usnfc LIKE zsfm0008-total,
      dfnfc LIKE zsfm0008-total,
     END OF it3a,
     it3a_c like it3a,
     BEGIN OF it3a6 OCCURS 1,
      fictr LIKE fmci-fictr,
      fipex_gr3(3),
      ac1fc LIKE zsfm0008-total,
      or1fc LIKE zsfm0008-total,
      us1fc LIKE zsfm0008-total,
      df1fc LIKE zsfm0008-total,
     END OF it3a6,
     it3a6_c like it3a6,
     BEGIN OF itb OCCURS 1,
      geber LIKE bppe-geber,
      fictr LIKE fmci-fictr,
      potyp LIKE fmci-potyp,
      org_tot LIKE ita-org_tot,
      cfd_tot like zsfm0008-total, "Carry forword
      rel_tot LIKE ita-rel_tot,
      com_tot LIKE zsfm0008-total,
      inv_tot LIKE ita-inv_tot,
      pay_tot LIKE zsfm0008-total,
      ava_tot LIKE ita-ava_tot,
     END OF itb,
     itb_c like itb,
     BEGIN OF itba OCCURS 1,
      potyp LIKE fmci-potyp,
      ac1fc LIKE zsfm0008-total,
      or1fc LIKE zsfm0008-total,
      us1fc LIKE zsfm0008-total,
      df1fc LIKE zsfm0008-total,
      acnfc LIKE zsfm0008-total,
      ornfc LIKE zsfm0008-total,
      usnfc LIKE zsfm0008-total,
      dfnfc LIKE zsfm0008-total,
     END OF itba,
     itba_c like itba,
     BEGIN OF itba6 OCCURS 1,
      fictr LIKE fmci-fictr,
      potyp LIKE fmci-potyp,
      ac1fc LIKE zsfm0008-total,
      or1fc LIKE zsfm0008-total,
      us1fc LIKE zsfm0008-total,
      df1fc LIKE zsfm0008-total,
     END OF itba6,
     itba6_c like itba6,
*-
     BEGIN OF itc OCCURS 1,
      geber LIKE bppe-geber,
      fictr LIKE fmci-fictr,
      org_tot LIKE ita-org_tot,
      cfd_tot like zsfm0008-total, "Carry forword
      rel_tot LIKE ita-rel_tot,
      com_tot LIKE ita-com_tot,
      inv_tot LIKE ita-inv_tot,
      pay_tot LIKE zsfm0008-total,
      ava_tot LIKE ita-ava_tot,
     END OF itc,
     itc_c like itc,
     BEGIN OF itd OCCURS 1,
      geber LIKE bppe-geber,
      org_tot LIKE ita-org_tot,
      cfd_tot like zsfm0008-total, "Carry forword
      rel_tot LIKE ita-rel_tot,
      com_tot LIKE ita-com_tot,
      inv_tot LIKE ita-inv_tot,
      pay_tot LIKE zsfm0008-total,
      ava_tot LIKE ita-ava_tot,
     END OF itd,
     itd_c like itd.

DATA : l_fund TYPE bp_geber VALUE '',
       l_fictr TYPE fistl VALUE '',
       l_fipex TYPE fm_fipex VALUE ''.

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS: p_fikrs LIKE fmci-fikrs MEMORY ID fik OBLIGATORY
                                    DEFAULT zfmcm_fm_area,
            p_gjahr LIKE fmci-gjahr MEMORY ID gjr OBLIGATORY
                                    DEFAULT sy-datum(4).
SELECT-OPTIONS: s_geber  FOR bppe-geber,
                s_fictr  FOR fmfctr-fictr," OBLIGATORY,
                s_fipex FOR fmci-fipex,
                s_profil FOR tbpfe-profil NO-DISPLAY,
                s_potyp  FOR fmci-potyp.
SELECT-OPTIONS:period FOR bkpf-monat OBLIGATORY NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK bl1.
SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-f02.
SELECTION-SCREEN ULINE.
*
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(29) text-t02.
PARAMETERS:rg_a RADIOBUTTON GROUP p DEFAULT 'X'.
SELECTION-SCREEN POSITION 36.
PARAMETERS:fgl AS CHECKBOX.
SELECTION-SCREEN COMMENT 40(36) text-t03.
SELECTION-SCREEN COMMENT 76(17) text-t04.
PARAMETERS p_ktopl LIKE ska1-ktopl DEFAULT zfmcm_ktopl.
SELECTION-SCREEN END OF LINE.
*
*parameters:rg_a RADIOBUTTON GROUP P default 'X'.
PARAMETERS:rg_3 RADIOBUTTON GROUP p,
           rg_b RADIOBUTTON GROUP p.
SELECTION-SCREEN BEGIN OF BLOCK bl21 WITH FRAME TITLE text-f03.
PARAMETERS:acf AS CHECKBOX.
SELECT-OPTIONS:per_acf FOR bkpf-monat OBLIGATORY NO-EXTENSION.
SELECTION-SCREEN BEGIN OF BLOCK bl31 WITH FRAME TITLE text-f04.
SELECT-OPTIONS:s_fictr2  FOR fmfctr-fictr.
SELECTION-SCREEN END OF BLOCK bl31.
PARAMETERS:col6 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK bl21.
PARAMETERS:rg_c RADIOBUTTON GROUP p,
           rg_d RADIOBUTTON GROUP p.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN END OF BLOCK bl2.
DATA:akm(2) TYPE n.
**********************************************
INITIALIZATION.
  SET PARAMETER ID 'FIC' FIELD l_fund.
  SET PARAMETER ID 'FIS' FIELD l_fictr.
  SET PARAMETER ID 'FPS' FIELD l_fipex.

  MOVE:'I' TO period-sign,'EQ' TO period-option,
        1 TO period-low,12 TO period-high.
  APPEND period.
  MOVE sy-datum+4(2) TO akm.
  MOVE:'I' TO per_acf-sign,'EQ' TO per_acf-option,
        1 TO per_acf-low,akm TO per_acf-high.
  APPEND per_acf.
**********************************************
AT SELECTION-SCREEN.
  PERFORM check_inputs.

START-OF-SELECTION.
*  PERFORM user_auth_check TABLES s_fictr[]
*                          USING p_fikrs.
  PERFORM user_auth_check_geber TABLES s_geber[]
                                       s_fictr[]
                                USING p_fikrs.
  if not g_auth_check is INITIAL.
   LEAVE TO SCREEN 0.
  endif.
  PERFORM define_itname.
  PERFORM read_data.
  PERFORM transf_data.
  PERFORM create_catalog.
  PERFORM show_alv_l.
*******************************END PGM*****************
FORM user_command USING f_ucomm LIKE sy-ucomm
                  i_selfield TYPE slis_selfield.            "#EC CALLED
  DATA:sypf LIKE sy-pfkey.
  CASE f_ucomm.
    WHEN 'GLLI'.
      IF rg_a = 'X'.
        PERFORM ci_ass_gl USING i_selfield-tabindex.
        PERFORM show_sel_condit.
      ENDIF.
*   FMRP_RFFMEP1AX
    WHEN 'ITLP'.PERFORM sbmt_lid USING 'RFFMEPGAX' i_selfield-tabindex.
    WHEN 'ITML'.PERFORM sbmt_lid USING 'RFFMEP4BX' i_selfield-tabindex.
    WHEN 'FB03' OR '&IC1'.
      GET PF-STATUS sypf.
      IF sypf = 'ALV_L_RECURS' AND f_ucomm = '&IC1'.
        PERFORM sbmt_dr_dw USING 'ZFMR0003' i_selfield-tabindex.
      ELSE.
        PERFORM sbmt_dr_dw USING 'ZFMR0001' i_selfield-tabindex.
      ENDIF.
    WHEN 'DDRE'.
      PERFORM sbmt_dr_dw USING 'ZFMR0003' i_selfield-tabindex.
    WHEN 'PSEL'.
      PERFORM sel_condition.
      PERFORM show_sel_condit.
  ENDCASE.
ENDFORM.                               "USER_COMMAND
************************************************************************
FORM is_there_budget CHANGING f_bdg.
  DATA:cnnt TYPE i.
  DATA: c_hivarnt    LIKE fmhisv-hivarnt VALUE zfmcm_hivarnt.      "Hierarchy
  CLEAR f_bdg.
  SELECT COUNT( * ) INTO cnnt
      FROM fmfctr AS a INNER JOIN fmhisv AS b
                          ON a~fikrs   = b~fikrs
                         AND a~fictr   = b~fistl
                         AND b~hivarnt = c_hivarnt
                       INNER JOIN bppe AS c
                          ON a~ctr_objnr = c~objnr
                       INNER JOIN fmci AS d
                          ON c~posit = d~posit
                         AND a~fikrs = d~fikrs
              WHERE a~fikrs  EQ p_fikrs
       AND a~fictr     IN s_fictr
       AND c~gjahr     EQ p_gjahr
       AND c~versn     EQ zfmcm_versn_0
       AND c~geber     IN s_geber
       AND d~fipex     IN s_fipex
       AND d~potyp     IN s_potyp.
  IF sy-subrc = 0 AND cnnt > 0.
    f_bdg = 'Y'.
  ENDIF.
ENDFORM.                    "is_there_budget
************************************************************************
FORM read_data.
  DATA:flg_bdgt.
  RANGES:r_fipex FOR fmci-fipex, "Mod#1 display also lines with budget 0
         r_fictr  FOR fmfctr-fictr,r_geber FOR bppe-geber,
         r_profil FOR tbpfe-profil,r_potyp  FOR fmci-potyp.
  PERFORM is_there_budget CHANGING flg_bdgt.
  IF flg_bdgt = 'Y'.
    APPEND LINES OF s_geber TO r_geber.
    APPEND LINES OF s_fictr TO r_fictr.
    APPEND LINES OF s_fipex TO r_fipex.
    APPEND LINES OF s_profil TO r_profil.
    APPEND LINES OF s_potyp TO r_potyp.
  ENDIF.
  FREE it_9000.
  CALL FUNCTION 'Z_FM_GET_MONTHLY_BUDGET_OTH1'
    EXPORTING
      i_fikrs            = p_fikrs
      i_gjahr            = p_gjahr
    TABLES
      t_geber            = r_geber
      t_fictr            = r_fictr
*      t_fipex            = s_fipex "mod#1
      t_fipex            = r_fipex                          "mod#1
      t_profil           = r_profil
      t_potyp            = r_potyp
      t_itab             = it_9000
    EXCEPTIONS
      no_fm_area         = 1
      no_fund            = 2
      no_funds_center    = 3
      no_commitment_item = 4
      no_profile         = 5
      no_category        = 6
      no_original        = 7
      OTHERS             = 8.
  CASE sy-subrc.
    WHEN 1.
      MESSAGE i000(zz) WITH text-m03.
      LEAVE LIST-PROCESSING.
    WHEN 2.
      MESSAGE i000(zz) WITH text-m05.
      LEAVE LIST-PROCESSING.
    WHEN 3.
      MESSAGE i000(zz) WITH text-m04.
      LEAVE LIST-PROCESSING.
    WHEN 4.
      MESSAGE i000(zz) WITH text-m08.
      LEAVE LIST-PROCESSING.
    WHEN 5.
      MESSAGE i000(zz) WITH text-m09.
      LEAVE LIST-PROCESSING.
    WHEN 6.
      MESSAGE i000(zz) WITH text-m10.
      LEAVE LIST-PROCESSING.
*    WHEN 7.    "Mod#1 rem by 5001130
*      MESSAGE I000(ZZ) WITH TEXT-M02.
*      leave list-processing.
    WHEN 7.    "Mod#1 rem by 5001130
      MESSAGE s000(zz) WITH text-m02.
    WHEN 8.
      MESSAGE i000(zz) WITH text-m11.
  ENDCASE.
  IF flg_bdgt = 'Y'.EXIT.ENDIF.
  LOOP AT it_9000."Mod#1 check in S-fipex must be outside FM
    IF NOT it_9000-fipex IN s_fipex OR NOT it_9000-fictr IN s_fictr
       OR NOT it_9000-geber IN s_geber OR NOT it_9000-profil IN s_profil
       OR NOT it_9000-potyp IN r_potyp.
      DELETE it_9000.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " READ_DATA
**************************************************************
FORM check_inputs.
  DATA:wrg_per,dmz TYPE i,hmz TYPE i,dma TYPE i,hma TYPE i,
       itfc LIKE fmfctr-fictr OCCURS 1 WITH HEADER LINE,
       itfc2 LIKE fmfctr-fictr OCCURS 1 WITH HEADER LINE.
  SELECT SINGLE * FROM fm01 WHERE fikrs = p_fikrs.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m03.
  ELSE.
    fm_curr = fm01-waers.fm_currt = fm_curr.
  ENDIF.
  LOOP AT period.
    dmz = period-low.hmz = period-high.
    IF period-sign <> 'I' OR period-option <> 'BT' OR
       period-low < 1 OR period-high > 12.
      wrg_per = 'Y'.EXIT.
    ENDIF.
  ENDLOOP.
  IF wrg_per = 'Y'.
    MESSAGE  ID 'VL' TYPE 'E' NUMBER '001' WITH
      'Period, month(1-12)  must be define as interval'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  IF acf IS INITIAL.EXIT.ENDIF.
  CLEAR wrg_per.
  LOOP AT per_acf.
    dma = per_acf-low.hma = per_acf-high.
    WRITE hma TO tx_om.WRITE dma TO tx_am.
    SHIFT tx_am LEFT DELETING LEADING space.
    SHIFT tx_om LEFT DELETING LEADING space.
    CONCATENATE tx_am '-' tx_om INTO tx_am.
    IF per_acf-sign <> 'I' OR per_acf-option <> 'BT' OR
       per_acf-low < 1 OR per_acf-high > 12.
      wrg_per = 'Y'.EXIT.
    ENDIF.
  ENDLOOP.
  IF wrg_per = 'Y'.
    MESSAGE  ID 'VL' TYPE 'E' NUMBER '001' WITH
      'Period for accumulation  must be define as interval'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  IF ( dma < dmz OR hma > hmz ).
    MESSAGE  ID 'VL' TYPE 'E' NUMBER '001' WITH
      'Period for accumulation exceeds period for selection'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  IF col6 = 'X' OR acf <> 'X'. EXIT. ENDIF.
  SELECT DISTINCT fictr FROM fmfctr INTO TABLE itfc
      WHERE fikrs = p_fikrs AND fictr IN s_fictr.
  SELECT DISTINCT fictr FROM fmfctr INTO TABLE itfc2
      WHERE fikrs = p_fikrs AND fictr IN s_fictr2.
  LOOP AT itfc2.
    READ TABLE itfc WITH KEY = itfc2.
    IF sy-subrc <> 0.
      MOVE itfc2 TO t99.
      CONCATENATE 'F.center:' t99
      ',from select.2 isn''t in Sel.conditions' INTO t99.
      MESSAGE  ID 'VL' TYPE 'E' NUMBER '001' WITH t99.
*    'Fund center from sel2 isn''t in sel1'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_inputs
************************************************************************
FORM define_itname.
  IF rg_a = 'X'. "split on level of C/I
    IF acf = 'X'.
      IF col6 = 'X'.itname ='ITAA6'.ELSE.itname = 'ITAA'.ENDIF.
    ELSE.
      itname = 'ITA'.
    ENDIF.
  ENDIF.
  IF rg_3 = 'X'. "split on level of 3 digit of C/I
    IF acf = 'X'.
      IF col6 = 'X'.itname = 'IT3A6'.ELSE.itname = 'IT3A'.ENDIF.
    ELSE.
      itname = 'IT3'.
    ENDIF.
  ENDIF.
  IF rg_b = 'X'.
    IF acf = 'X'.
      IF col6 = 'X'.itname = 'ITBA6'.ELSE.itname = 'ITBA'.ENDIF.
    ELSE.
      itname = 'ITB'.
    ENDIF.
  ENDIF.
  IF rg_c = 'X'.itname = 'ITC'.ENDIF.
  IF rg_d = 'X'.itname = 'ITD'.ENDIF.
  IF fgl = 'X' AND rg_a = 'X'.
    CONCATENATE itname 'G' INTO itname.
  ENDIF.
ENDFORM.                    "define_itname
************************************************************************
FORM pf_status_set USING rt_extab TYPE slis_t_extab.
  CLEAR rt_extab.
  CASE itname.
    WHEN 'ITD' OR 'ITAA' OR 'ITAAG' OR 'IT3A'
      OR 'ITBA' OR 'ITB' OR 'ITBA6'.
      SET PF-STATUS 'ALV_L_RECURS' EXCLUDING rt_extab.
    WHEN OTHERS.
      IF rg_a = 'X'.
        SET PF-STATUS 'ALV_LIST_FULL' EXCLUDING rt_extab.
      ELSE.
        SET PF-STATUS 'ALV_LIST' EXCLUDING rt_extab.
      ENDIF.
  ENDCASE.
ENDFORM.                    "pf_status_set
***************************************************************
FORM std_p_up USING rt_extab TYPE slis_t_extab. "mod#1 Ins. 5001130
*calling from form show_sel_condit
  CLEAR rt_extab.
  SET PF-STATUS 'STD_P_UP' EXCLUDING rt_extab.
*GUI STD_P_UP is copy from SAPLSLVC_FULLSCREEN GUI STDPOPUP_FULLSCREEN
  IF flg_ittd = 'Y'.
    SET TITLEBAR 'P_SEL' WITH 'Select conditions'.
  ELSE.
    SET TITLEBAR 'P_SEL' WITH 'Assign G/L accounts - C/I (Commitm.Item)'.
  ENDIF.
ENDFORM.                    "STD_P_UP
************************************************************************
FORM sel_condition.  "mod#1   form inserted by 5001130
  IF flg_ittd = 'Y'.EXIT.ENDIF.
  CLEAR ittd.FREE ittd.
  PERFORM create_det_catal.
  WRITE lnl TO t99.SHIFT t99 LEFT DELETING LEADING space.
  IF t99 = ''.t99 = '0'.ENDIF.
  CONCATENATE 'Number of items:' t99 INTO ittd-t.
  CONCATENATE ittd-t 'For next select conditions:' INTO ittd-t
              SEPARATED BY ' | '.
  APPEND ittd.
  CLEAR ittd.
  DO 80 TIMES.
    CONCATENATE '_' ittd-t INTO ittd-t.
  ENDDO.
  APPEND ittd.
  CONCATENATE 'Financ. Managm. area:' p_fikrs INTO ittd-t.
  APPEND ittd.
  CONCATENATE 'Selection Fiscal Year:' p_gjahr INTO ittd-t.
  APPEND ittd.
  LOOP AT s_geber.
    CONCATENATE 'Fund:' s_geber INTO ittd-t.
    APPEND ittd.
  ENDLOOP.
  LOOP AT s_fictr.
    CONCATENATE 'Funds center:' s_fictr INTO ittd-t.
    APPEND ittd.
  ENDLOOP.
  LOOP AT s_fipex.
    CONCATENATE 'Commitment item:' s_fipex INTO ittd-t.
    APPEND ittd.
  ENDLOOP.
  LOOP AT s_profil.
    CONCATENATE 'Commitm. item type:' s_profil INTO ittd-t.
    APPEND ittd.
  ENDLOOP.
  LOOP AT period.
    CONCATENATE 'Selected period (Month):' period-low '-' period-high
                INTO ittd-t.
    APPEND ittd.
  ENDLOOP.
  IF acf = 'X' AND ( rg_a = 'X' OR rg_3 = 'X' OR rg_b = 'X' ).
    CLEAR ittd.
    DO 80 TIMES.
      CONCATENATE '_' ittd-t INTO ittd-t.
    ENDDO.
    APPEND ittd.
    MOVE 'Format with accumulated values' TO ittd-t.
    APPEND ittd.
    LOOP AT per_acf.
      CONCATENATE 'Accumul.period (Month):' per_acf-low '-' per_acf-high
                 INTO ittd-t.
      CONCATENATE ittd-t 'Actual month' per_acf-high
                 INTO ittd-t SEPARATED BY space.
      APPEND ittd.
    ENDLOOP.
    IF col6 <> 'X'.
      LOOP AT s_fictr2.
        CONCATENATE 'Separated Fund center(for accumulation):' s_fictr2 ''
         INTO ittd-t SEPARATED BY space.
        APPEND ittd.
      ENDLOOP.
    ENDIF.
  ENDIF.
  DO 80 TIMES.
    CONCATENATE '_' ittd-t INTO ittd-t.
  ENDDO.
  APPEND ittd.
  IF rg_a = 'X'.
    ittd-t = 'Split on level of C/I'.
  ENDIF.
  IF rg_3 = 'X'.
    ittd-t = 'Split on level of 3 dig. of C/I-group'.
  ENDIF.
  IF rg_b = 'X'.
    ittd-t = 'Split on level of C/I-type'.
  ENDIF.
  IF rg_c = 'X'.
    ittd-t = 'Split on level of Funds center'.
  ENDIF.
  IF rg_d = 'X'.
    ittd-t = 'Split on level of Fund'.
  ENDIF.
  APPEND ittd.
  flg_ittd = 'Y'.
ENDFORM.   "sel_condition
************************************************************************
FORM show_sel_condit.
  DATA:pl TYPE i.
  CONSTANTS:c_pf_st TYPE slis_formname VALUE 'STD_P_UP'."form std_p_UP
  DESCRIBE TABLE ittd LINES pl.
  pl = pl + 7.
  IF pl > 20.pl = 21.ENDIF.
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
*CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
              i_callback_program = repname
              i_callback_pf_status_set = c_pf_st
*            i_callback_user_command  = c_user_command
              i_structure_name   = 'ZSFM0008_ITTD'
              it_fieldcat        = fctd
              i_default          = 'X'
*            i_save             = 'A'        "user specific
*            is_variant         = alv_variant
              it_events          = events
*            is_layout          = layout
     i_screen_start_column             = 5
     i_screen_start_line               = 5
     i_screen_end_column               = 80
     i_screen_end_line                 = pl
         TABLES  t_outtab           = ittd
         EXCEPTIONS program_error = 1 OTHERS = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.    "show_sel_condit
************************************************************************
FORM create_det_catal. "mod#1   form inserted by 5001130
  DATA:it_nm TYPE slis_tabname VALUE 'ITTD'.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = repname
*            i_internal_tabname = 'ITTD' "Letters must be Upper case
*            i_internal_tabname = it_nm "must be Upper case
            I_STRUCTURE_NAME = 'ZSFM0008_ITTD'
            i_inclname         = repname
       CHANGING
            ct_fieldcat        = fctd
       EXCEPTIONS
        inconsistent_interface       = 1
        program_error                = 2
        OTHERS                       = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM. "create_det_catal
************************************************************************
FORM create_catalog.
    data: lw_itab type slis_tabname.

  DATA:rfct TYPE slis_fieldcat_alv,
       it_fct TYPE slis_fieldcat_alv OCCURS 1 WITH HEADER LINE,
       sep_fc(20),acc_mes(7),akt_m(2).
  LOOP AT s_fictr2.
    CONCATENATE s_fictr2-sign s_fictr2-option s_fictr2-low(5) INTO sep_fc.
    EXIT.
  ENDLOOP.
  LOOP AT per_acf.
    CONCATENATE per_acf-low '-' per_acf-high INTO acc_mes.
    MOVE per_acf-high TO akt_m.
    EXIT.
  ENDLOOP.
  repname = sy-repid.
  lw_itab = itname.
  data : iw_tablename(30) type c.
*  if itname = 'ITA'.
*    iw_tablename = 'ZSFM0008_ITA'.
    CONCATENATE 'ZSFM0008_' itname into iw_tablename.
*  endif.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = repname
*      i_internal_tabname     = lw_itab "itname "must be Upper case
     i_structure_name        = iw_tablename
*      i_inclname             = repname
    CHANGING
      ct_fieldcat            = fct
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    LOOP AT fct INTO rfct.
      IF rfct-fieldname = 'FIPEX_GR3'.
        MOVE '3 Dig. C/I' TO rfct-seltext_s.
        MOVE '3 First Dig.of C/I' TO rfct-seltext_m.
        rfct-seltext_l = rfct-seltext_m.
      ENDIF.
      IF rfct-fieldname = 'AC1FC'.
        IF col6 = 'X'.
          CONCATENATE acc_mes 'Acum.' INTO rfct-seltext_s.
          CONCATENATE acc_mes 'Act.Accumulated' INTO rfct-seltext_m
                                SEPARATED BY space.
        ELSE.
          CONCATENATE acc_mes 'AcFc:' INTO rfct-seltext_s.
          CONCATENATE rfct-seltext_s sep_fc INTO rfct-seltext_m.
        ENDIF.
        rfct-seltext_l = rfct-seltext_m.
      ENDIF.
      IF rfct-fieldname = 'OR1FC'.
        IF col6 = 'X'.
          CONCATENATE akt_m 'Orig.' INTO rfct-seltext_s.
          CONCATENATE akt_m 'Original' INTO rfct-seltext_m
                                       SEPARATED BY space.
        ELSE.
          CONCATENATE akt_m 'Orig.Fc:' INTO rfct-seltext_s.
          CONCATENATE rfct-seltext_s sep_fc INTO rfct-seltext_m.
        ENDIF.
        rfct-seltext_l = rfct-seltext_m.
      ENDIF.
      IF rfct-fieldname = 'US1FC'.
        IF col6 = 'X'.
          CONCATENATE akt_m 'Actual' INTO rfct-seltext_s.
          CONCATENATE akt_m 'Actual' INTO rfct-seltext_m
                                      SEPARATED BY space.
        ELSE.
          CONCATENATE akt_m 'Actu.Fc' INTO rfct-seltext_s.
          CONCATENATE rfct-seltext_s sep_fc INTO rfct-seltext_m.
        ENDIF.
        rfct-seltext_l = rfct-seltext_m.
      ENDIF.
      IF rfct-fieldname = 'DF1FC'.
        IF col6 = 'X'.
          CONCATENATE akt_m 'Difer' INTO rfct-seltext_s.
          CONCATENATE akt_m 'Diference' INTO rfct-seltext_m
                                       SEPARATED BY space.
        ELSE.
          CONCATENATE akt_m 'Difer.Fc' INTO rfct-seltext_s.
          CONCATENATE rfct-seltext_s sep_fc INTO rfct-seltext_m.
        ENDIF.
        rfct-seltext_l = rfct-seltext_m.
      ENDIF.
**--
      IF rfct-fieldname = 'ACNFC'.
        CONCATENATE acc_mes 'Ac-Su' INTO rfct-seltext_s.
        CONCATENATE acc_mes 'Acum-Sum-Fc' INTO rfct-seltext_m.
        rfct-seltext_l = rfct-seltext_m.
      ENDIF.
      IF rfct-fieldname = 'ORNFC'.
        CONCATENATE akt_m 'Or.SumFc' INTO rfct-seltext_s.
        rfct-seltext_m = rfct-seltext_s.
        rfct-seltext_l = rfct-seltext_m.
      ENDIF.
      IF rfct-fieldname = 'USNFC'.
        CONCATENATE akt_m 'Act.Sum' INTO rfct-seltext_s.
        CONCATENATE akt_m 'Act.Sum-Fc' INTO rfct-seltext_m.
        rfct-seltext_l = rfct-seltext_m.
      ENDIF.
      IF rfct-fieldname = 'DFNFC'.
        CONCATENATE akt_m 'Dif.SuFC' INTO rfct-seltext_s.
        CONCATENATE akt_m 'Dif.Sum-FC' INTO rfct-seltext_l.
        rfct-seltext_l = rfct-seltext_m.
      ENDIF.
*--
      IF rfct-fieldname = 'ORG_TOT'.
        rfct-seltext_s = 'Original'.rfct-seltext_m = rfct-seltext_s.
        rfct-seltext_l = rfct-seltext_m.
      ENDIF.
      IF rfct-fieldname = 'CFD_TOT'.
        rfct-seltext_s = 'Carryforwd'. rfct-seltext_m = rfct-seltext_s.
        rfct-seltext_l = rfct-seltext_m.
      ENDIF.
      IF rfct-fieldname = 'REL_TOT'.
        rfct-seltext_s = 'Released'.rfct-seltext_m = rfct-seltext_s.
        rfct-seltext_l = rfct-seltext_m.
      ENDIF.
      IF rfct-fieldname = 'INV_TOT'.
        rfct-seltext_s = 'Invoice'.rfct-seltext_m = rfct-seltext_s.
        rfct-seltext_l = rfct-seltext_m.
      ENDIF.
      IF rfct-fieldname = 'COM_TOT'.
        rfct-seltext_s = 'Commitment'.rfct-seltext_m = rfct-seltext_s.
        rfct-seltext_l = rfct-seltext_s.
      ENDIF.
      IF rfct-fieldname = 'PAY_TOT'.
        rfct-seltext_s = 'Payments'.rfct-seltext_m = 'Payments'.
        rfct-seltext_l = 'Payments'.
      ENDIF.

      IF rfct-fieldname = 'AVA_TOT'.
        rfct-seltext_s = 'Available'.rfct-seltext_m = 'Available'.
        rfct-seltext_l = 'Available'.
      ENDIF.
      rfct-reptext_ddic = rfct-seltext_l.
      APPEND rfct TO it_fct.
    ENDLOOP.
    FREE fct.
    LOOP AT it_fct.
      APPEND it_fct TO fct.
    ENDLOOP.
  ENDIF.
ENDFORM. "create_catalog
************************************************************************
FORM show_alv_l.
  CONSTANTS:c_pf_status_set TYPE slis_formname VALUE 'PF_STATUS_SET',
            c_user_command TYPE slis_formname VALUE 'USER_COMMAND'.
  CASE itname.
    WHEN 'ITAA6'.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
             i_callback_program = repname
             i_callback_pf_status_set = c_pf_status_set "FORM pf_status_set
             i_callback_user_command  = c_user_command
             i_structure_name   = itname
             it_fieldcat        = fct
             i_default          = 'X'
             i_save             = 'A'        "user specific
             is_variant         = alv_variant
             it_events          = events
*        is_layout          = layout
           TABLES  t_outtab           = itaa6
           EXCEPTIONS program_error = 1 OTHERS = 2.
    WHEN 'ITAA6G'.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
             i_callback_program = repname
             i_callback_pf_status_set = c_pf_status_set "FORM pf_status_set
             i_callback_user_command  = c_user_command
             i_structure_name   = itname
             it_fieldcat        = fct
             i_default          = 'X'
             i_save             = 'A'        "user specific
             is_variant         = alv_variant
             it_events          = events
*        is_layout          = layout
           TABLES  t_outtab           = itaa6g
           EXCEPTIONS program_error = 1 OTHERS = 2.
    WHEN 'ITAA'.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
             i_callback_program = repname
             i_callback_pf_status_set = c_pf_status_set "FORM pf_status_set
             i_callback_user_command  = c_user_command
             i_structure_name   = itname
             it_fieldcat        = fct
             i_default          = 'X'
             i_save             = 'A'        "user specific
             is_variant         = alv_variant
             it_events          = events
*        is_layout          = layout
           TABLES  t_outtab           = itaa
           EXCEPTIONS program_error = 1 OTHERS = 2.
    WHEN 'ITAAG'.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
             i_callback_program = repname
             i_callback_pf_status_set = c_pf_status_set "FORM pf_status_set
             i_callback_user_command  = c_user_command
             i_structure_name   = itname
             it_fieldcat        = fct
             i_default          = 'X'
             i_save             = 'A'        "user specific
             is_variant         = alv_variant
             it_events          = events
*        is_layout          = layout
           TABLES  t_outtab           = itaag
           EXCEPTIONS program_error = 1 OTHERS = 2.
    WHEN 'ITA'.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
             i_callback_program = repname
             i_callback_pf_status_set = c_pf_status_set "FORM pf_status_set
             i_callback_user_command  = c_user_command
             i_structure_name   = itname
             it_fieldcat        = fct
             i_default          = 'X'
             i_save             = 'A'        "user specific
             is_variant         = alv_variant
             it_events          = events
*        is_layout          = layout
           TABLES  t_outtab           = ita
           EXCEPTIONS program_error = 1 OTHERS = 2.
    WHEN 'ITAG'.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
             i_callback_program = repname
             i_callback_pf_status_set = c_pf_status_set "FORM pf_status_set
             i_callback_user_command  = c_user_command
             i_structure_name   = itname
             it_fieldcat        = fct
             i_default          = 'X'
             i_save             = 'A'        "user specific
             is_variant         = alv_variant
             it_events          = events
*        is_layout          = layout
           TABLES  t_outtab           = itag
           EXCEPTIONS program_error = 1 OTHERS = 2.
    WHEN 'IT3A6'.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
             i_callback_program = repname
             i_callback_pf_status_set = c_pf_status_set "FORM pf_status_set
             i_callback_user_command  = c_user_command
             i_structure_name   = itname
             it_fieldcat        = fct
             i_default          = 'X'
             i_save             = 'A'        "user specific
             is_variant         = alv_variant
             it_events          = events
*        is_layout          = layout
           TABLES  t_outtab           = it3a6
           EXCEPTIONS program_error = 1 OTHERS = 2.
    WHEN 'IT3A'.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
             i_callback_program = repname
             i_callback_pf_status_set = c_pf_status_set "FORM pf_status_set
             i_callback_user_command  = c_user_command
             i_structure_name   = itname
             it_fieldcat        = fct
             i_default          = 'X'
             i_save             = 'A'        "user specific
             is_variant         = alv_variant
             it_events          = events
*        is_layout          = layout
           TABLES  t_outtab           = it3a
           EXCEPTIONS program_error = 1 OTHERS = 2.
    WHEN 'IT3'.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
             i_callback_program = repname
             i_callback_pf_status_set = c_pf_status_set "FORM pf_status_set
             i_callback_user_command  = c_user_command
             i_structure_name   = itname
             it_fieldcat        = fct
             i_default          = 'X'
             i_save             = 'A'        "user specific
             is_variant         = alv_variant
             it_events          = events
*        is_layout          = layout
           TABLES  t_outtab           = it3
           EXCEPTIONS program_error = 1 OTHERS = 2.
    WHEN 'ITBA6'.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
             i_callback_program = repname
             i_callback_pf_status_set = c_pf_status_set "FORM pf_status_set
             i_callback_user_command  = c_user_command
             i_structure_name   = itname
             it_fieldcat        = fct
             i_default          = 'X'
             i_save             = 'A'        "user specific
             is_variant         = alv_variant
             it_events          = events
*        is_layout          = layout
           TABLES  t_outtab           = itba6
        EXCEPTIONS program_error = 1 OTHERS = 2.
    WHEN 'ITBA'.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
             i_callback_program = repname
             i_callback_pf_status_set = c_pf_status_set "FORM pf_status_set
             i_callback_user_command  = c_user_command
             i_structure_name   = itname
             it_fieldcat        = fct
             i_default          = 'X'
             i_save             = 'A'        "user specific
             is_variant         = alv_variant
             it_events          = events
*        is_layout          = layout
           TABLES  t_outtab           = itba
           EXCEPTIONS program_error = 1 OTHERS = 2.
    WHEN 'ITB'.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
             i_callback_program = repname
             i_callback_pf_status_set = c_pf_status_set "FORM pf_status_set
             i_callback_user_command  = c_user_command
             i_structure_name   = itname
             it_fieldcat        = fct
             i_default          = 'X'
             i_save             = 'A'        "user specific
             is_variant         = alv_variant
             it_events          = events
*        is_layout          = layout
           TABLES  t_outtab           = itb
           EXCEPTIONS program_error = 1 OTHERS = 2.
    WHEN 'ITC'.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
             i_callback_program = repname
             i_callback_pf_status_set = c_pf_status_set "FORM pf_status_set
             i_callback_user_command  = c_user_command
             i_structure_name   = itname
             it_fieldcat        = fct
             i_default          = 'X'
             i_save             = 'A'        "user specific
             is_variant         = alv_variant
             it_events          = events
*        is_layout          = layout
           TABLES  t_outtab           = itc
           EXCEPTIONS program_error = 1 OTHERS = 2.
    WHEN 'ITD'.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
             i_callback_program = repname
             i_callback_pf_status_set = c_pf_status_set "FORM pf_status_set
             i_callback_user_command  = c_user_command
             i_structure_name   = itname
             it_fieldcat        = fct
             i_default          = 'X'
             i_save             = 'A'        "user specific
             is_variant         = alv_variant
             it_events          = events
*        is_layout          = layout
           TABLES  t_outtab           = itd
           EXCEPTIONS program_error = 1 OTHERS = 2.
  ENDCASE.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM."show_alv_L
************************************************************************
FORM transf_data.
  DATA:val LIKE it_9000-total,
       BEGIN OF rit,
        geber LIKE ita-geber,fictr LIKE ita-fictr,
        potyp LIKE ita-potyp,fipex LIKE ita-fipex,
        fipex_gr3(3),
        bezei LIKE ita-bezei,
        org_tot LIKE ita-org_tot,
        cfd_tot LIKE ita-cfd_tot,
        rel_tot LIKE ita-rel_tot,
        com_tot LIKE ita-com_tot,
        inv_tot LIKE ita-inv_tot,
        ava_tot LIKE ita-ava_tot,
        pay_tot LIKE ita-pay_tot,
        ac1fc LIKE zsfm0008-total,
        or1fc LIKE zsfm0008-total,
        us1fc LIKE zsfm0008-total,
        df1fc LIKE zsfm0008-total,
        acnfc LIKE zsfm0008-total,
        ornfc LIKE zsfm0008-total,
        usnfc LIKE zsfm0008-total,
        dfnfc LIKE zsfm0008-total,
       END OF rit,
       t10(10),t24(24).
  FREE:ita,itaa,itaa6,it3,it3a,it3a6,itb,itba,itba6,itc,itd,it_fictr.
  LOOP AT it_9000 WHERE ctgry ='1' OR ctgry = '2' OR ctgry = '7' OR ctgry = '8'
                     OR ctgry = '9' OR ctgry = '10' OR ctgry = '11'.
    MOVE it_9000-fictr TO it_fictr.COLLECT it_fictr.
    CLEAR:rit,ita,itaa,itaa6,it3,it3a,it3a6,itb,itba,itba6,itc,itd.
    MOVE-CORRESPONDING it_9000 TO rit.
    MOVE it_9000-fipex TO t24.
    SHIFT t24 LEFT DELETING LEADING space.MOVE t24(3) TO rit-fipex_gr3.
    PERFORM period_val CHANGING val rit-ac1fc rit-or1fc rit-us1fc rit-df1fc
                                   rit-acnfc rit-ornfc rit-usnfc rit-dfnfc.
*data : c_original_oth1   type i              value  1,
*       c_carryfd_oth1    type i              value  2,
*       c_supplement_oth1 type i              value  3,
*       c_transfer_oth1   type i              value  4,
*       c_return_oth1     type i              value  5,
*       c_current_oth1    type i              value  6,
*       c_released_oth1   type i              value  7,
*       c_commitment_oth1 type i              value  8,
*       c_invoice_oth1    type i              value  9,
*       c_payments_oth1   type i              value 10,
*       c_available_oth1  type i              value 11,
*       c_unknown_oth1    type i              value 12.
    CASE it_9000-ctgry.
      WHEN '1'.MOVE val TO rit-org_tot.
      WHEN '2'.MOVE val TO rit-cfd_tot.
      WHEN '7'.MOVE val TO rit-rel_tot.
      WHEN '8'.MOVE val TO rit-com_tot.
      WHEN '9'.MOVE val TO rit-inv_tot.
      WHEN '10'.MOVE val TO rit-pay_tot.
      WHEN '11'.MOVE val TO rit-ava_tot.
    ENDCASE.
    CASE itname.
      WHEN 'ITA'.
        MOVE-CORRESPONDING rit TO ita.COLLECT ita.
      WHEN 'ITAA'.
        MOVE-CORRESPONDING rit TO itaa.COLLECT itaa.
      WHEN 'ITAA6'.
        MOVE-CORRESPONDING rit TO itaa6.COLLECT itaa6.
      WHEN 'ITAG'.
        MOVE-CORRESPONDING rit TO itag.COLLECT itag.
      WHEN 'ITAAG'.
        MOVE-CORRESPONDING rit TO itaag.COLLECT itaag.
      WHEN 'ITAA6G'.
        MOVE-CORRESPONDING rit TO itaa6g.COLLECT itaa6g.
      WHEN 'IT3A6'.
        CONCATENATE 'C/I group:' rit-fipex_gr3 'xxx' INTO rit-bezei.
        MOVE-CORRESPONDING rit TO it3a6.COLLECT it3a6.
      WHEN 'IT3A'.
        CONCATENATE 'C/I group:' rit-fipex_gr3 'xxx' INTO rit-bezei.
        MOVE-CORRESPONDING rit TO it3a.COLLECT it3a.
      WHEN 'IT3'.
        CONCATENATE 'C/I group:' rit-fipex_gr3 'xxx' INTO rit-bezei.
        MOVE-CORRESPONDING rit TO it3.COLLECT it3.
      WHEN 'ITBA6'.
        MOVE-CORRESPONDING rit TO itba6.COLLECT itba6.
      WHEN 'ITBA'.
        MOVE-CORRESPONDING rit TO itba.COLLECT itba.
      WHEN 'ITB'.
        MOVE-CORRESPONDING rit TO itb.COLLECT itb.
      WHEN 'ITC'.
        MOVE-CORRESPONDING rit TO itc.COLLECT itc.
      WHEN 'ITD'.
        MOVE-CORRESPONDING rit TO itd.COLLECT itd.
    ENDCASE.
  ENDLOOP.
  PERFORM fill_gl.
  CASE itname.
    WHEN 'ITA'.DESCRIBE TABLE ita LINES lnl.
    WHEN 'ITAA'.DESCRIBE TABLE itaa LINES lnl.
    WHEN 'ITAA6'.DESCRIBE TABLE itaa6 LINES lnl.
    WHEN 'ITAG'.DESCRIBE TABLE itag LINES lnl.
    WHEN 'ITAAG'.DESCRIBE TABLE itaag LINES lnl.
    WHEN 'ITAA6G'.DESCRIBE TABLE itaa6g LINES lnl.
    WHEN 'IT3'.DESCRIBE TABLE it3 LINES lnl.
    WHEN 'IT3A'.DESCRIBE TABLE it3a LINES lnl.
    WHEN 'IT3A6'.DESCRIBE TABLE it3a6 LINES lnl.
    WHEN 'ITB'.DESCRIBE TABLE itb LINES lnl.
    WHEN 'ITBA'.DESCRIBE TABLE itba LINES lnl.
    WHEN 'ITBA6'.DESCRIBE TABLE itba6 LINES lnl.
    WHEN 'ITC'.DESCRIBE TABLE itc LINES lnl.
    WHEN 'ITD'.DESCRIBE TABLE itd LINES lnl.
  ENDCASE.
  WRITE lnl TO t10.SHIFT t10 LEFT DELETING LEADING space.
  IF t10 = ''.t10 = '0'.ENDIF.
  CONCATENATE sy-title t10 'Lines Curr:' fm_curr INTO sy-title
              SEPARATED BY space.
ENDFORM." transf_data.
***********************************************************************
FORM period_val CHANGING p_val ac1fc or1fc us1fc df1fc
                               acnfc ornfc usnfc dfnfc.
  DATA:acval LIKE it_9000-wtp01,msval LIKE acval,mxms TYPE i.
  CLEAR p_val.
  DO 12 TIMES.
    CHECK sy-index IN period.
    CASE sy-index.
      WHEN 1.p_val = p_val + it_9000-wtp01.
      WHEN 2.p_val = p_val + it_9000-wtp02.
      WHEN 3.p_val = p_val + it_9000-wtp03.
      WHEN 4.p_val = p_val + it_9000-wtp04.
      WHEN 5.p_val = p_val + it_9000-wtp05.
      WHEN 6.p_val = p_val + it_9000-wtp06.
      WHEN 7.p_val = p_val + it_9000-wtp07.
      WHEN 8.p_val = p_val + it_9000-wtp08.
      WHEN 9.p_val = p_val + it_9000-wtp09.
      WHEN 10.p_val = p_val + it_9000-wtp10.
      WHEN 11.p_val = p_val + it_9000-wtp11.
      WHEN 12.p_val = p_val + it_9000-wtp12.
    ENDCASE.
  ENDDO.
  IF it_9000-waers <> fm_currt AND fm_currt <> ''.
    CONCATENATE it_9000-fictr '/' it_9000-fipex '/' it_9000-waers
                '<>' fm_curr ', FM-currency' INTO t99.
    MESSAGE  ID 'VL' TYPE 'I' NUMBER '001' WITH t99.
    CLEAR fm_currt.
  ENDIF.
  IF acf <> 'X'.EXIT.ENDIF.
  LOOP AT per_acf.
    mxms = per_acf-high.EXIT.
  ENDLOOP.
  CLEAR acval.
  DO 12 TIMES.
    CHECK sy-index IN per_acf.
    CLEAR msval.
    CASE sy-index.
      WHEN 1.msval = it_9000-wtp01.
      WHEN 2.msval = it_9000-wtp02.
      WHEN 3.msval = it_9000-wtp03.
      WHEN 4.msval = it_9000-wtp04.
      WHEN 5.msval = it_9000-wtp05.
      WHEN 6.msval = it_9000-wtp06.
      WHEN 7.msval = it_9000-wtp07.
      WHEN 8.msval = it_9000-wtp08.
      WHEN 9.msval = it_9000-wtp09.
      WHEN 10.msval = it_9000-wtp10.
      WHEN 11.msval = it_9000-wtp11.
      WHEN 12.msval = it_9000-wtp12.
    ENDCASE.
* ****** upravit tak aby plnilo aj itaa6... atd
    IF sy-index = mxms.
      CASE it_9000-ctgry.
        WHEN '1' or '2'. MOVE msval TO ornfc.
        WHEN '8' OR '9' OR '10'. MOVE msval TO usnfc.
        WHEN '11'.MOVE msval TO dfnfc.
      ENDCASE.
    ENDIF.
    IF it_9000-ctgry = '8' OR it_9000-ctgry = '9' OR it_9000-ctgry = '10'.
      acval = acval + msval."actual = 7.Commm+8.Invoic+9.Paym
    ENDIF.
  ENDDO.
  IF col6 = 'X'.
    MOVE:acval TO ac1fc,ornfc TO or1fc,usnfc TO us1fc,dfnfc TO df1fc.
    CLEAR:acnfc,ornfc,usnfc,dfnfc.
    EXIT.
  ENDIF.
  CASE it_9000-ctgry.
    WHEN '8' OR '9' OR '10'.MOVE acval TO acnfc.
  ENDCASE.
  IF it_9000-fictr IN s_fictr2.
    MOVE:acnfc TO ac1fc,ornfc TO or1fc,usnfc TO us1fc,dfnfc TO df1fc.
  ENDIF.
ENDFORM.                    "period_val
**************************************************
FORM sbmt_lid USING rp_name lix.
  DATA:mo_st LIKE bkpf-monat,mo_end LIKE bkpf-monat,
       pfst TYPE sy-pfkey.
  DATA:ageber LIKE bppe-geber,afictr LIKE fmci-fictr.
  RANGES:rfipex FOR fmci-fipex.
  CASE itname.
    WHEN 'ITA'.
      READ TABLE ita INDEX lix.
      MOVE:ita-fictr TO afictr,ita-geber TO ageber.
      MOVE:'I' TO rfipex-sign,'EQ' TO rfipex-option,
           ita-fipex TO rfipex-low.
      APPEND rfipex.
    WHEN 'ITAA6'.
      READ TABLE itaa6 INDEX lix.
      MOVE:itaa6-fictr TO afictr.
      MOVE:'I' TO rfipex-sign,'EQ' TO rfipex-option,
           itaa6-fipex TO rfipex-low.
      APPEND rfipex.
    WHEN 'ITAG'.
      READ TABLE itag INDEX lix.
      MOVE:itag-fictr TO afictr,itag-geber TO ageber.
      MOVE:'I' TO rfipex-sign,'EQ' TO rfipex-option,
           itag-fipex TO rfipex-low.
      APPEND rfipex.
    WHEN 'ITAA6G'.
      READ TABLE itaa6g INDEX lix.
      MOVE:itaa6g-fictr TO afictr.
      MOVE:'I' TO rfipex-sign,'EQ' TO rfipex-option,
           itaa6g-fipex TO rfipex-low.
      APPEND rfipex.
    WHEN 'IT3A6'.
      READ TABLE it3a6 INDEX lix.
      MOVE:it3a6-fictr TO afictr.
      MOVE:'I' TO rfipex-sign,'CP' TO rfipex-option.
      CONCATENATE it3a6-fipex_gr3 '*' INTO rfipex-low.
      APPEND rfipex.

    WHEN 'IT3'.
      READ TABLE it3 INDEX lix.
      MOVE:it3-fictr TO afictr,it3-geber TO ageber.
      MOVE:'I' TO rfipex-sign,'CP' TO rfipex-option.
      CONCATENATE it3-fipex_gr3 '*' INTO rfipex-low.
      APPEND rfipex.

    WHEN 'ITC'.
      READ TABLE itc INDEX lix.
      MOVE:itc-fictr TO afictr,itc-geber TO ageber.
  ENDCASE.

  IF sy-subrc <> 0.
    MESSAGE  ID 'VL' TYPE 'I' NUMBER '001' WITH
                'Position the cursor on a line in the list'.
    EXIT.
  ENDIF.
*
  IF acf = 'X'.
    mo_st = per_acf-low.mo_end = per_acf-high.
  ELSE.
    mo_st = period-low.mo_end = period-high.
  ENDIF.
  IF rp_name = 'RFFMEPGAX' AND acf = 'X' AND rg_c <> 'X'."C/I line doc.
    CALL SCREEN '0100' STARTING AT 9 8 ENDING AT 43 10.
    IF flg_cnt = ''.EXIT.ENDIF.
    IF rg_om = 'X' AND acf = 'X'.mo_st = mo_end.ENDIF.
  ENDIF.

  SUBMIT (rp_name) WITH s_fikrs = p_fikrs WITH s_gjahr = p_gjahr
    WITH s_fonds = ageber WITH s_fictr = afictr
    WITH s_fipex IN rfipex
    WITH p_varnt = zfmcm_versn_0
    WITH s_perio BETWEEN mo_st AND mo_end SIGN 'I'
*via selection-screen
    AND RETURN.
  GET PF-STATUS pfst.
  IF pfst(4) <> 'ALV_'.
    CASE itname.
      WHEN 'ITD' OR 'ITAA' OR 'ITAAG' OR 'IT3A'
        OR 'ITBA' OR 'ITBA6' OR 'ITB'.
        SET PF-STATUS 'ALV_L_RECURS'.
      WHEN OTHERS.
        IF rg_a = 'X'.
          SET PF-STATUS 'ALV_LIST_FULL'.
        ELSE.
          SET PF-STATUS 'ALV_LIST'.
        ENDIF.
    ENDCASE.
  ENDIF.
ENDFORM.                    "sbmt_LID
********************************************************
FORM sbmt_dr_dw USING pg_nm lix.
  DATA:nwcol6.
  RANGES:r_geber FOR bppe-geber,r_fictr FOR fmfctr-fictr,
         r_fictr2 FOR fmfctr-fictr,
         r_fipex FOR fmci-fipex,r_potyp FOR fmci-potyp.
  IF itname = 'ITAA' OR itname = 'ITAAG'.
    nwcol6 = 'X'.
  ELSE.
    nwcol6 = col6.
    IF rg_a = 'X' AND pg_nm = sy-repid.
      MESSAGE  ID 'VL' TYPE 'S' NUMBER '001' WITH
        'You are already on lowest level(split on L. of C/I)'.
      EXIT.
    ENDIF.
  ENDIF.
  APPEND LINES OF s_geber TO r_geber.
  APPEND LINES OF s_fictr TO r_fictr.
  APPEND LINES OF s_fictr2 TO r_fictr2.
  APPEND LINES OF s_fipex TO r_fipex.
  APPEND LINES OF s_potyp TO r_potyp.
  CASE itname.
    WHEN 'ITAA6'.
      READ TABLE itaa6 INDEX lix.
      FREE:r_fictr,r_fipex.
      CLEAR:r_fictr,r_fipex.
      MOVE:'I' TO r_fictr-sign,'EQ' TO r_fictr-option,
            itaa6-fictr TO r_fictr-low.
      APPEND r_fictr.
      MOVE:'I' TO r_fipex-sign,'EQ' TO r_fipex-option,
            itaa6-fipex TO r_fipex-low.
      APPEND r_fipex.
*
    WHEN 'ITAA6G'.
      READ TABLE itaa6g INDEX lix.
      FREE:r_fictr,r_fipex.
      CLEAR:r_fictr,r_fipex.
      MOVE:'I' TO r_fictr-sign,'EQ' TO r_fictr-option,
            itaa6g-fictr TO r_fictr-low.
      APPEND r_fictr.
      MOVE:'I' TO r_fipex-sign,'EQ' TO r_fipex-option,
            itaa6g-fipex TO r_fipex-low.
      APPEND r_fipex.
*
    WHEN 'ITAA'. "recursive submit of ZFMR003
      READ TABLE itaa INDEX lix.
      FREE:r_fipex,r_fictr,r_fictr2.CLEAR:r_fipex,r_fictr,r_fictr2.
      MOVE:'I' TO r_fipex-sign,'EQ' TO r_fipex-option,
            itaa-fipex TO r_fipex-low.
      APPEND r_fipex.
      LOOP AT it_fictr.
        MOVE:'I' TO r_fictr-sign,'EQ' TO r_fictr-option,
             it_fictr TO r_fictr-low.
        APPEND r_fictr.
        MOVE r_fictr TO r_fictr2.APPEND r_fictr2.
      ENDLOOP.
*
    WHEN 'ITAAG'. "recursive submit of ZFMR003
      READ TABLE itaag INDEX lix.
      FREE:r_fipex,r_fictr,r_fictr2.CLEAR:r_fipex,r_fictr,r_fictr2.
      MOVE:'I' TO r_fictr-sign,'EQ' TO r_fictr-option,
           it_fictr TO r_fictr-low.
      APPEND r_fictr.
      MOVE:'I' TO r_fipex-sign,'EQ' TO r_fipex-option,
            itaag-fipex TO r_fipex-low.
      APPEND r_fipex.
      LOOP AT it_fictr.
        MOVE:'I' TO r_fictr-sign,'EQ' TO r_fictr-option,
             it_fictr TO r_fictr-low.
        APPEND r_fictr.
        MOVE r_fictr TO r_fictr2.APPEND r_fictr2.
      ENDLOOP.
*
    WHEN 'ITA'.
      READ TABLE ita INDEX lix.
      FREE:r_geber,r_fictr,r_fipex,r_potyp.
      CLEAR:r_geber,r_fictr,r_fipex,r_potyp.
      MOVE:'I' TO r_geber-sign,'EQ' TO r_geber-option,
            ita-geber TO r_geber-low.
      APPEND r_geber.
      MOVE:'I' TO r_fictr-sign,'EQ' TO r_fictr-option,
            ita-fictr TO r_fictr-low.
      APPEND r_fictr.
      MOVE:'I' TO r_fipex-sign,'EQ' TO r_fipex-option,
            ita-fipex TO r_fipex-low.
      APPEND r_fipex.
      MOVE:'I' TO r_potyp-sign,'EQ' TO r_potyp-option,
            ita-potyp TO r_potyp-low.
      APPEND r_potyp.
*
    WHEN 'ITAG'.
      READ TABLE itag INDEX lix.
      FREE:r_geber,r_fictr,r_fipex,r_potyp.
      CLEAR:r_geber,r_fictr,r_fipex,r_potyp.
      MOVE:'I' TO r_geber-sign,'EQ' TO r_geber-option,
            itag-geber TO r_geber-low.
      APPEND r_geber.
      MOVE:'I' TO r_fictr-sign,'EQ' TO r_fictr-option,
            itag-fictr TO r_fictr-low.
      APPEND r_fictr.
      MOVE:'I' TO r_fipex-sign,'EQ' TO r_fipex-option,
            itag-fipex TO r_fipex-low.
      APPEND r_fipex.
      MOVE:'I' TO r_potyp-sign,'EQ' TO r_potyp-option,
            itag-potyp TO r_potyp-low.
      APPEND r_potyp.
*
    WHEN 'IT3A6'.
      READ TABLE it3a6 INDEX lix.
      FREE:r_fictr,r_fipex.
      CLEAR:r_fictr,r_fipex.
      MOVE:'I' TO r_fictr-sign,'EQ' TO r_fictr-option,
            it3a6-fictr TO r_fictr-low.
      APPEND r_fictr.
      MOVE:'I' TO r_fipex-sign,'CP' TO r_fipex-option.
      CONCATENATE it3a6-fipex_gr3 '*' INTO r_fipex-low.
      APPEND r_fipex.
*
    WHEN 'IT3A'.  "recursive submit of ZFMR003
      READ TABLE it3a INDEX lix.
      FREE r_fipex.CLEAR r_fipex.
      MOVE:'I' TO r_fipex-sign,'CP' TO r_fipex-option.
      CONCATENATE it3a-fipex_gr3 '*' INTO r_fipex-low.
      APPEND r_fipex.
*
    WHEN 'IT3'.
      READ TABLE it3 INDEX lix.
      FREE:r_geber,r_fictr,r_fipex.
      CLEAR:r_geber,r_fictr,r_fipex.
      MOVE:'I' TO r_geber-sign,'EQ' TO r_geber-option,
            it3-geber TO r_geber-low.
      APPEND r_geber.
      MOVE:'I' TO r_fictr-sign,'EQ' TO r_fictr-option,
            it3-fictr TO r_fictr-low.
      APPEND r_fictr.
      MOVE:'I' TO r_fipex-sign,'CP' TO r_fipex-option.
      CONCATENATE it3-fipex_gr3 '*' INTO r_fipex-low.
      APPEND r_fipex.
*
    WHEN 'ITBA6'. "recursive submit of ZFMR003
      READ TABLE itba6 INDEX lix.
      FREE:r_fictr,r_potyp.
      CLEAR:r_fictr,r_potyp.
      MOVE:'I' TO r_fictr-sign,'EQ' TO r_fictr-option,
            itba6-fictr TO r_fictr-low.
      APPEND r_fictr.
      MOVE:'I' TO r_potyp-sign,'EQ' TO r_potyp-option,
            itba6-potyp TO r_potyp-low.
      APPEND r_potyp.
*
    WHEN 'ITBA'. "recursive submit of ZFMR003
      READ TABLE itba INDEX lix.
      FREE r_potyp.CLEAR r_potyp.
      MOVE:'I' TO r_potyp-sign,'EQ' TO r_potyp-option,
            itba-potyp TO r_potyp-low.
      APPEND r_potyp.
*
    WHEN 'ITB'. "recursive submit of ZFMR003
      READ TABLE itb INDEX lix.
      FREE:r_geber,r_fictr,r_potyp.
      CLEAR:r_geber,r_fictr,r_potyp.
      MOVE:'I' TO r_geber-sign,'EQ' TO r_geber-option,
            itb-geber TO r_geber-low.
      APPEND r_geber.
      MOVE:'I' TO r_fictr-sign,'EQ' TO r_fictr-option,
            itb-fictr TO r_fictr-low.
      APPEND r_fictr.
      MOVE:'I' TO r_potyp-sign,'EQ' TO r_potyp-option,
            itb-potyp TO r_potyp-low.
      APPEND r_potyp.
*
    WHEN 'ITC'.
      READ TABLE itc INDEX lix.
      FREE:r_geber,r_fictr.
      CLEAR:r_geber,r_fictr.
      MOVE:'I' TO r_geber-sign,'EQ' TO r_geber-option,
            itc-geber TO r_geber-low.
      APPEND r_geber.
      MOVE:'I' TO r_fictr-sign,'EQ' TO r_fictr-option,
            itc-fictr TO r_fictr-low.
      APPEND r_fictr.
*
    WHEN 'ITD'.
      READ TABLE itd INDEX lix.
      FREE:r_geber.CLEAR:r_geber.
      MOVE:'I' TO r_geber-sign,'EQ' TO r_geber-option,
            itd-geber TO r_geber-low.
      APPEND r_geber.
  ENDCASE.
  IF pg_nm = sy-repid. "recursive calling.
    SUBMIT (pg_nm) WITH p_fikrs = p_fikrs WITH p_gjahr = p_gjahr
         WITH s_geber IN r_geber
         WITH s_fictr IN r_fictr WITH s_fipex IN r_fipex
         WITH s_fictr2 IN r_fictr2
         WITH s_profil IN s_profil WITH s_potyp IN r_potyp
         WITH period IN period
         WITH rg_a = 'X' WITH fgl = fgl
         WITH acf = acf WITH col6 = nwcol6
         WITH per_cf IN per_acf
*via selection-screen
         AND RETURN.
  ELSE.
    SUBMIT (pg_nm) WITH p_fikrs = p_fikrs WITH p_gjahr = p_gjahr
         WITH s_geber IN r_geber
         WITH s_fictr IN r_fictr WITH s_fipex IN r_fipex
         WITH s_profil IN s_profil WITH s_potyp IN r_potyp
*via selection-screen
         AND RETURN.
  ENDIF.
ENDFORM. "sbmt_dr_dw
************************************************************************
FORM fill_gl.
  DATA:form_par LIKE itag-gltx.
  CASE itname.
    WHEN 'ITAG'.
      LOOP AT itag.
        PERFORM find_gl USING itag-fipex CHANGING itag-glac itag-gltx.
        MODIFY itag.
      ENDLOOP.
    WHEN 'ITAAG'.
      LOOP AT itaag.
        PERFORM find_gl USING itaag-fipex CHANGING itaag-glac form_par.
        MODIFY itaag.
      ENDLOOP.
    WHEN 'ITAA6G'.
      LOOP AT itaa6g.
        PERFORM find_gl USING itaa6g-fipex CHANGING itaa6g-glac form_par.
        MODIFY itaa6g.
      ENDLOOP.
  ENDCASE.
ENDFORM. "fill_gl
************************************************************************
FORM ci_ass_gl USING lix.
  DATA:comi LIKE ita-fipex,lns TYPE i,
       ci_txt LIKE fmcit-bezei,
       f_ac LIKE skb1-saknr,f_txt LIKE skat-txt20.
  CASE itname.
    WHEN 'ITA'.
      READ TABLE ita INDEX lix.
      MOVE:ita-fipex TO comi.
    WHEN 'ITAA6'.
      READ TABLE itaa6 INDEX lix.
      MOVE:itaa6-fipex TO comi.
    WHEN 'ITAG'.
      READ TABLE itag INDEX lix.
      MOVE:itag-fipex TO comi.
    WHEN 'ITAA6G'.
      READ TABLE itaa6g INDEX lix.
      MOVE:itaa6g-fipex TO comi.
  ENDCASE.
  IF comi IS INITIAL.EXIT.ENDIF.
  PERFORM find_gl USING comi CHANGING f_ac f_txt.
  SELECT * FROM fmcit WHERE spras = sy-langu AND fikrs = p_fikrs
           AND fipex = comi.
    ci_txt = fmcit-bezei.EXIT.
  ENDSELECT.
*
  CLEAR flg_ittd.
  CLEAR ittd.FREE ittd.
  PERFORM create_det_catal.
  CONCATENATE 'C/I:' comi ci_txt INTO ittd-t SEPARATED BY space.
  APPEND ittd.
  CLEAR ittd.
  DO 80 TIMES.
    CONCATENATE '_' ittd-t INTO ittd-t.
  ENDDO.
  APPEND ittd.
  DESCRIBE TABLE it_gl LINES lns.
  WRITE lns TO t99.SHIFT t99 LEFT DELETING LEADING space.
  IF t99 = ''.t99 = '0'.ENDIF.
  CONCATENATE 'Number of assign G/L accounts:' t99
               INTO ittd-t SEPARATED BY space.
  APPEND ittd.
  CLEAR ittd.
  DO 80 TIMES.
    CONCATENATE '_' ittd-t INTO ittd-t.
  ENDDO.
  APPEND ittd.
  LOOP AT it_gl.
    MOVE it_gl-saknr TO t99.SHIFT t99 LEFT DELETING LEADING '0'.
    CONCATENATE 'G/L' t99 it_gl-tx INTO ittd-t SEPARATED BY ' : '.
    APPEND ittd.
  ENDLOOP.
ENDFORM. "CI_ASS_GL
************************************************************************
FORM find_gl USING com_it CHANGING gl_ac gl_txt.
  DATA:cnnt TYPE i,t12(12).
  CLEAR:it_gl,gl_ac,gl_txt.
  FREE it_gl.
  SELECT * FROM skb1 WHERE fipos = com_it.
    SELECT COUNT( * ) INTO cnnt FROM ska1 WHERE ktopl = p_ktopl
                               AND saknr = skb1-saknr.
    CHECK cnnt > 0.
    CLEAR it_gl.
    MOVE skb1-saknr TO it_gl-saknr.
    COLLECT it_gl.
  ENDSELECT.
  DESCRIBE TABLE it_gl LINES cnnt.
  IF cnnt  = 0.EXIT.ENDIF.
  LOOP AT it_gl.
    CLEAR gl_txt.
    gl_ac = it_gl-saknr.
    SELECT SINGLE * FROM skat WHERE spras = sy-langu
                 AND ktopl = p_ktopl AND saknr = it_gl-saknr.
    IF sy-subrc = 0.
      gl_txt = skat-txt20.
    ENDIF.
    it_gl-tx = gl_txt.
    MODIFY it_gl.
  ENDLOOP.
  IF cnnt > 1.
    CONCATENATE '++' gl_ac INTO t12.MOVE t12(10) TO gl_ac.
    gl_txt = '++More G/L acc.:'.
    WRITE cnnt TO t12.SHIFT t12 LEFT DELETING LEADING space.
    CONCATENATE gl_txt t12 INTO gl_txt.
  ENDIF.
ENDFORM. "find_gl
************************************************************************
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
************************************************************************
MODULE pbo_0100 OUTPUT.
  CLEAR flg_cnt.
  SET PF-STATUS 'ENT_EXT'.
  SET TITLEBAR 'P_SEL' WITH 'Accumulated months or One month'.
ENDMODULE.                 " PBO_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
************************************************************************
MODULE pai_0100 INPUT.
  IF sy-ucomm = 'ENTE'.
    flg_cnt = 'Y'.
  ENDIF.
ENDMODULE.                 " PAI_0100  INPUT
