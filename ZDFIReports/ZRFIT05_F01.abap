*&---------------------------------------------------------------------*
*&  Include           ZRFIT05_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  INIT_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_SCREEN .

  loop at screen.
    if screen-name = 'P_BUTXT'.
      screen-input  = 0.
      screen-intensified = '0'.
      screen-display_3d  = '0'.
      modify screen.
    endif.
    if screen-name = 'P_BUKRS'.
      screen-input = ' '.
      modify screen.
    endif.
  endloop.


* & c????
  perform fi_wt_read_t001 using    p_bukrs
                          changing p_butxt.

ENDFORM.                    " INIT_SCREEN
*&---------------------------------------------------------------------*
*&      Form  FI_WT_READ_T001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BUKRS  text
*      <--P_P_BUTXT  text
*----------------------------------------------------------------------*
FORM fi_wt_read_t001  using    pa_bukrs
                      changing pa_butxt.

  data : it_t001 like t001.

  call function 'FI_WT_READ_T001'
    exporting
      i_bukrs   = pa_bukrs
    importing
      t_t001    = it_t001
    exceptions
      not_found = 1.

  case sy-subrc.
    when 0.
      pa_butxt = it_t001-butxt.
    when 1.
      message s101(f5).
    when others.
  endcase.


ENDFORM.                    " FI_WT_READ_T001
*&---------------------------------------------------------------------*
*&      Form  INIT_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form INIT_PROC .

  CLEAR: pftab[], pftab.

  IF p_test EQ 'X'.  "test run
    CLEAR: pftab[], pftab.
  ELSE.
    pftab-fcode = '&DATA_SAVE'.
    APPEND pftab.
  ENDIF.

  SELECT single waers INTO (c_waers)
    FROM t001
   WHERE bukrs EQ p_bukrs.


endform.                    " INIT_PROC

************************************************************************
* AT USER-COMMAND                                                     *
************************************************************************
FORM user_command USING p_ucomm    LIKE sy-ucomm
                        p_selfield TYPE slis_selfield.

  CASE p_ucomm.
    WHEN '&IC1' OR '&ETA'.  "PICK.."
      READ TABLE it_list INDEX p_selfield-tabindex.
      IF sy-subrc = 0.
        SET PARAMETER ID:'BLN' FIELD it_list-belnr,
                         'BUK' FIELD it_list-bukrs,
                         'GJR' FIELD it_list-gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN '&DATA_SAVE'.
      PERFORM exec_save.
  ENDCASE.

ENDFORM.
************************************************************************
* Form  PF_STATUS_SET
************************************************************************
FORM  pf_status_set USING p_rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'MENU'.
*  SET PF-STATUS 'STANDARD' EXCLUDING pftab.

ENDFORM.

******************************
* FORM END_OF_LIST
******************************
FORM end_of_list.

  DESCRIBE TABLE it_zcmal LINES it_zcmal_lin.

  SKIP 1.
  WRITE:/ '*** Processed Document total : ', it_zcmal_lin.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_BUKRS_ENTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bukrs_entry.
  SELECT SINGLE * FROM t001
                  WHERE bukrs EQ p_bukrs.
  IF sy-subrc NE 0.
    MESSAGE e002 WITH 'Company Code' p_bukrs.
  ENDIF.

ENDFORM.                    " GET_BUKRS_ENTRY
*&---------------------------------------------------------------------*
*&      Form  get_ztfi_cmal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ztfi_cmal.   "actual line item.


*it_zcmal

  SELECT *  INTO CORRESPONDING FIELDS OF it_zcmal
            FROM ztfi_cmal
            WHERE bukrs = p_bukrs
            AND   gjahr IN s_gjahr
            AND   saknr IN s_saknr
            AND   lifnr IN s_lifnr
            AND   kunnr IN s_kunnr
            AND   belnr IN s_belnr
            AND   budat IN s_budat
            AND   datum IN s_datum
            AND   grupp IN s_grupp
            AND   ebene IN s_ebene
            AND   gsart IN s_gsart.

    IF it_zcmal-dmshb > 0.
      IF p_out = 'X'.  CONTINUE.  ENDIF.
      it_zcmal-ssign = '+'.
    ELSE.
      IF p_out = 'X' AND it_zcmal-grupp(1) = 'W'.
        CONTINUE.
      ENDIF.
      it_zcmal-ssign = '-'.
    ENDIF.
    APPEND it_zcmal.
  ENDSELECT.

ENDFORM.                    " get_ztfi_cmal
*&---------------------------------------------------------------------*
*&      Form  read_setting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_setting.
  SELECT SINGLE * INTO t001 FROM t001
                  WHERE bukrs EQ p_bukrs.

  SELECT * FROM ztfi_cmad  "collecting actual settings for g/l acct.
           INTO CORRESPONDING FIELDS OF TABLE i_zcmad
           WHERE bukrs = p_bukrs
           ORDER BY zeilt
                    umskz
                    shkzg
                    hkont DESCENDING.

  SELECT *  FROM t074     "special g/l account
            INTO CORRESPONDING FIELDS OF TABLE i_t074
            WHERE ktopl EQ t001-ktopl.   "Chart of Accounts

  SELECT *  FROM t035 INTO CORRESPONDING FIELDS OF TABLE i_t035.
*//  planning groups

*  Account Assignment References acc. to Transaction Type
  SELECT *  FROM t037s INTO CORRESPONDING FIELDS OF TABLE i_t037s
                      WHERE bukrs = p_bukrs.

  SELECT saknr fdlev xgkon
         FROM skb1   "G/L account master (company code)
         INTO CORRESPONDING FIELDS OF TABLE i_skb1
         WHERE bukrs = p_bukrs
           AND xgkon = 'X'  "Cash receipt(disbursement) account
           AND fdlev <> space.  "Planning Level
  SORT i_skb1 BY saknr.

ENDFORM.                    " read_setting
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.
  PERFORM get_skb1.           "..???? G/L ??

*// Conversion Only
  IF p_conv = 'X'.
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_bkpf
             FROM bkpf CLIENT SPECIFIED
             WHERE mandt = sy-mandt
             AND   bukrs EQ p_bukrs
             AND   gjahr IN s_gjahr
             AND   belnr IN s_belnr
             AND   budat IN s_budat
             AND   blart IN s_blart
             AND   bstat NE 'V'.
  ELSE.
    PERFORM get_ztfi_cmac.      "collect actuals time stamp
    PERFORM get_bkpf.           "Header Info.
  ENDIF.

  PERFORM get_item.           "..Item Info.

ENDFORM.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  get_skb1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_skb1.
  CLEAR: r_hkont[],  r_hkont,
         r_cmacct[], r_cmacct.

  r_hkont-sign   = 'I'.     r_hkont-option   = 'EQ'.
  r_cmacct-sign  = 'I'.     r_cmacct-option  = 'EQ'.

*... logic ??,?? BY JIPARK.
*  loop at i_skb1 where xgkon eq 'X'
*                 and ( fdlev cp 'B*' or  "bank account
*                       fdlev cp 'C*' ).  "cash account

* UD1K941019 by IG.MOON 7/16/2007 {
  LOOP AT i_skb1 WHERE xgkon EQ 'X'
                 AND ( fdlev CP 'B*' OR  "bank account
                       fdlev CP 'C*' OR  "cash account
                       fdlev EQ 'TH' ).  "Interest Incom

* }


    r_hkont-low = i_skb1-saknr.
    APPEND r_hkont.

    IF i_skb1-fdlev+1(1) = '0'.
      r_cmacct-low = i_skb1-saknr.
      APPEND r_cmacct.
    ENDIF.

*    CASE l_ebene.
*      WHEN '93'.  " ?????
*        r_hkont93-sign = 'E'.     r_hkont93-option = 'EQ'.  "Exclude
*        r_imsi-sign    = 'I'.     r_imsi-option    = 'EQ'.
*        r_hkont93-low = r_hkont-low. APPEND r_hkont93.
*        r_imsi-low    = r_hkont-low. APPEND r_imsi.
*      WHEN '92'.  " ????? ?? ??
*        r_hkont92-sign = 'I'.     r_hkont92-option = 'EQ'.
*        r_imsi-sign    = 'I'.     r_imsi-option    = 'EQ'.
*        r_hkont92-low = r_hkont-low. APPEND r_hkont92.
*        r_imsi-low    = r_hkont-low. APPEND r_imsi.
*      WHEN '91'.    " ???? ??
*        r_hkont91-sign = 'I'.     r_hkont91-option = 'EQ'.
*        r_imsi-sign    = 'I'.     r_imsi-option    = 'EQ'.
*        r_hkont91-low = r_hkont-low. APPEND r_hkont91.
*        r_imsi-low    = r_hkont-low. APPEND r_imsi.
*      WHEN '90' OR '9X'.  " ??/????? ??
*        r_cash-sign    = 'I'.     r_cash-option    = 'EQ'.
*        r_cash-low    = r_hkont-low. APPEND r_cash.
*        APPEND r_hkont.
*      WHEN '9A'.    " ????
*        r_hkont9a-sign = 'I'.     r_hkont9a-option = 'EQ'.
*        r_imsi-sign    = 'I'.     r_imsi-option    = 'EQ'.
*        r_hkont9a-low = r_hkont-low. APPEND r_hkont9a.
*        r_imsi-low    = r_hkont-low. APPEND r_imsi.
*        APPEND r_hkont.
*      WHEN OTHERS.
*        r_imsi-sign    = 'I'.     r_imsi-option    = 'EQ'.
*        r_imsi-low    = r_hkont-low. APPEND r_imsi.
*        APPEND r_hkont.
*    ENDCASE.
  ENDLOOP.
*  SELECT saknr fdlev INTO (r_hkont-low, l_ebene)
*                     FROM skb1 "G/L account master (company code)
*                     WHERE bukrs EQ p_bukrs
*                     AND   xgkon EQ 'X'
*                     AND   fdlev IN (c_leve1,c_leve2,c_leve3,c_leve4,
*                                     c_leve5,c_leve6,c_leve7,c_leve8,
*                                     c_leve9,c_lev10,c_lev11,c_lev9x ).
*    r_cmacct-sign  = 'I'.     r_cmacct-option  = 'EQ'.
*    r_cmacct-low = r_hkont-low. APPEND r_cmacct.
*
*    CASE l_ebene.
*      WHEN '93'.  " ?????
*        r_hkont93-sign = 'E'.     r_hkont93-option = 'EQ'.  "Exclude
*        r_imsi-sign    = 'I'.     r_imsi-option    = 'EQ'.
*        r_hkont93-low = r_hkont-low. APPEND r_hkont93.
*        r_imsi-low    = r_hkont-low. APPEND r_imsi.
*      WHEN '92'.  " ????? ?? ??
*        r_hkont92-sign = 'I'.     r_hkont92-option = 'EQ'.
*        r_imsi-sign    = 'I'.     r_imsi-option    = 'EQ'.
*        r_hkont92-low = r_hkont-low. APPEND r_hkont92.
*        r_imsi-low    = r_hkont-low. APPEND r_imsi.
*      WHEN '91'.    " ???? ??
*        r_hkont91-sign = 'I'.     r_hkont91-option = 'EQ'.
*        r_imsi-sign    = 'I'.     r_imsi-option    = 'EQ'.
*        r_hkont91-low = r_hkont-low. APPEND r_hkont91.
*        r_imsi-low    = r_hkont-low. APPEND r_imsi.
*      WHEN '90' OR '9X'.  " ??/????? ??
*        r_cash-sign    = 'I'.     r_cash-option    = 'EQ'.
*        r_cash-low    = r_hkont-low. APPEND r_cash.
*        APPEND r_hkont.
*      WHEN '9A'.    " ????
*        r_hkont9a-sign = 'I'.     r_hkont9a-option = 'EQ'.
*        r_imsi-sign    = 'I'.     r_imsi-option    = 'EQ'.
*        r_hkont9a-low = r_hkont-low. APPEND r_hkont9a.
*        r_imsi-low    = r_hkont-low. APPEND r_imsi.
*        APPEND r_hkont.
*      WHEN OTHERS.
*        r_imsi-sign    = 'I'.     r_imsi-option    = 'EQ'.
*        r_imsi-low    = r_hkont-low. APPEND r_imsi.
*        APPEND r_hkont.
*    ENDCASE.
*  ENDSELECT.

ENDFORM.                                                    " get_skb1
*&--------------------------------------------------------------------*
*&      Form  get_ztfi_cmac
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ztfi_cmac.

  CLEAR:$tstlo.
* from
  SELECT MAX( tstlo ) INTO $old_tstlo  "tstlo: time stamp
                      FROM ztfi_cmac
                      WHERE bukrs EQ p_bukrs.

* to : exe.date/exe.time
  IF p_ftstlo <> space.
    $old_tstlo = p_ftstlo.
  ENDIF.
  IF p_ttstlo = space.
    CONCATENATE sy-datum sy-uzeit INTO $tstlo.    " system date
  ELSE.
    $tstlo = p_ttstlo.                            " input date
  ENDIF.

* from,to date/time
  g_date_from = $old_tstlo(8).
  g_time_from = $old_tstlo+8(6).
  g_date_to   = $tstlo(8).
  g_time_to   = $tstlo+8(6).
ENDFORM.                    " get_ztfi_cmac
*&---------------------------------------------------------------------*
*&      Form  exec_save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exec_save. "usage2. at user-command
  DATA:answer.
  DATA:$subrc(10).

  CHECK p_dbread = space.

  LOOP AT it_zcmal WHERE grupp = space.
    WRITE:/ '*** Error Log ***'.
    WRITE:/ 'Plan Group not assigned', it_zcmal-belnr, it_zcmal-gjahr.
  ENDLOOP.

  IF p_test = 'X'.
    PERFORM popup_to_confirm CHANGING answer.
    CHECK answer EQ 'J'.
  ENDIF.

  IF p_conv = space.
    PERFORM insert_ztfi_cmac CHANGING $subrc.
    IF $subrc = 'E'.
      ROLLBACK WORK. EXIT.
    ENDIF.
  ELSE.
*---conversion only
    DELETE FROM ztfi_cmal WHERE bukrs = p_bukrs
                          AND   gjahr IN s_gjahr
                          AND   budat IN s_budat
                          AND   belnr IN s_belnr
                          AND   blart IN s_blart.
  ENDIF.


  PERFORM insert_ztfi_cmal CHANGING $subrc.
  IF $subrc = 'E'.
    ROLLBACK WORK. EXIT.
  ENDIF.

*  PERFORM make_it_zcmas.
*  PERFORM update_ztfi_cmas CHANGING $subrc.
*  IF $subrc = 'E'.
*    ROLLBACK WORK. EXIT.
*  ENDIF.
  IF $subrc EQ space.
    MESSAGE s007.
    IF p_test EQ 'X'.  LEAVE TO SCREEN 0. ENDIF.
  ENDIF.

ENDFORM.                    " exec_save
*&---------------------------------------------------------------------*
*&      Form  get_bkpf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bkpf.
  CLEAR:it_bkpf[],it_bkpf.

  SELECT * INTO CORRESPONDING FIELDS OF it_bkpf
           FROM bkpf  CLIENT SPECIFIED
           WHERE mandt = sy-mandt
           AND bukrs EQ p_bukrs
           AND ( ( cpudt > g_date_from AND cpudt <= g_date_to )
           OR  ( cpudt = g_date_from   AND cputm > g_time_from )
           OR  ( aedat >= g_date_from  AND aedat <= g_date_to ) )
           AND bstat NE 'V'                    "except temp.doc
           AND gjahr IN s_gjahr
           AND belnr IN s_belnr
           AND budat IN s_budat
           AND blart IN s_blart.

*   adjust reverse doc.
    IF it_bkpf-stjah <> space OR it_bkpf-stblg <> space.
      SELECT SINGLE * FROM ztfi_cmal
                      WHERE bukrs EQ it_bkpf-bukrs
                      AND   gjahr EQ it_bkpf-stjah
                      AND   belnr EQ it_bkpf-stblg.
      IF sy-subrc <> 0. CONTINUE. ENDIF.
      it_bkpf-bstat = 'R'.
    ELSE.
      it_bkpf-bstat = ' '.
      IF it_bkpf-aedat IS INITIAL.
        it_bkpf-gubun = 'A'. "new
      ELSE.
        it_bkpf-gubun = 'M'. "modify
      ENDIF.
    ENDIF.

    APPEND it_bkpf.
  ENDSELECT.
ENDFORM.                    " get_bkpf
*&---------------------------------------------------------------------*
*&      Form  get_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_item.

  DATA: $subrc.
  DATA: $ix LIKE sy-tabix.
  CLEAR:it_zcmal[],it_zcmal.

  LOOP AT it_bkpf.
    $ix = sy-tabix.

*    PERFORM check_ztfi_cmal CHANGING $subrc.

*    CHECK $subrc EQ space.

*- by ig.moon 5/12/2008 {

*    if not it_bkpf-stjah is initial.
*      continue.
*    endif.

* }

*+ by ig.moon 5/12/2008 {
    IF NOT it_bkpf-stjah IS INITIAL.
      SELECT SINGLE budat INTO bkpf-budat
        FROM bkpf WHERE bukrs EQ it_bkpf-bukrs
                    AND belnr EQ it_bkpf-stblg
                    AND gjahr EQ it_bkpf-stjah.
      IF sy-subrc EQ 0.
        IF bkpf-budat EQ it_bkpf-budat.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.
* }
    PERFORM get_bank_inout. "CHANGING sy-tabix.

    CHECK sy-subrc <> space.
*//             Determine Document Type.
    CALL FUNCTION 'ZFFI_DETERMINE_DOCTYPE'
         EXPORTING
              tcode = it_bkpf-tcode
         IMPORTING
              awtyp = it_bkpf-awtyp. "Reference Transaction

*   CLEAR g_trade.
    CASE it_bkpf-awtyp.  "reference procedure
      WHEN 'TR-TM'.
        PERFORM get_vtbfha.
      WHEN 'LOANS'.
        PERFORM get_vdarl.
      WHEN OTHERS.
        PERFORM get_bseg.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " get_item
*&---------------------------------------------------------------------*
*&      Form  check_ztfi_cmal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_$SUBRC  text
*----------------------------------------------------------------------*
*FORM check_ztfi_cmal CHANGING $subrc.
*  CLEAR:it_zcmal, $subrc.
*
** normal document
*  IF it_bkpf-stjah IS INITIAL.
*    SELECT * FROM ztfi_cmal
*             WHERE bukrs EQ it_bkpf-bukrs
*             AND   gjahr EQ it_bkpf-gjahr
*             AND   belnr EQ it_bkpf-belnr.
*
*      MOVE-CORRESPONDING ztfi_cmal TO it_zcmal.
*      it_zcmal-wrshb = - it_zcmal-wrshb.
*      it_zcmal-dmshb = - it_zcmal-dmshb.
*      it_zcmal-gubun = 'D'.  " delete...
*      CLEAR: it_zcmal-mandt.
*
*      APPEND it_zcmal.
*    ENDSELECT.
*
** reverse document
*  ELSE.
*    SELECT * FROM ztfi_cmal
*             WHERE bukrs EQ it_bkpf-bukrs
*             AND   gjahr EQ it_bkpf-stjah  "reverse year
*             AND   belnr EQ it_bkpf-stblg. "reverse doc.
*
*      MOVE-CORRESPONDING ztfi_cmal TO it_zcmal.
*      it_zcmal-wrshb = - it_zcmal-wrshb.
*      it_zcmal-dmshb = - it_zcmal-dmshb.
*      it_zcmal-gubun = 'D'.  " delete...
*      CLEAR: it_zcmal-mandt.
*
*      APPEND it_zcmal.
*    ENDSELECT.
*
*    $subrc = 'R'.
*  ENDIF.
*ENDFORM.                    " check_ztfi_cmal
*&---------------------------------------------------------------------*
*&      Form  get_bank_inout
*&---------------------------------------------------------------------*
FORM get_bank_inout. "CHANGING $subrc.
  CLEAR:it_bseg[],it_bseg. ",$subrc.
  DATA: l_acct  LIKE ska1-saknr.
  DATA: BEGIN OF lt_bseg OCCURS 0,
         belnr LIKE bseg-belnr,
         hkont LIKE bseg-hkont,
         shkzg LIKE bseg-shkzg,
         wrbtr LIKE bseg-wrbtr,
         dmbtr LIKE bseg-dmbtr,
         fdtag LIKE bseg-fdtag,
         valut LIKE bseg-valut,
         augdt LIKE bseg-augdt,
         augbl LIKE bseg-augbl,
        END OF lt_bseg.

*--> bank account
  REFRESH lt_bseg.
  SELECT belnr hkont shkzg
         wrbtr dmbtr
         fdtag valut augdt augbl
     INTO TABLE lt_bseg
     FROM bseg
     WHERE bukrs EQ it_bkpf-bukrs
       AND gjahr EQ it_bkpf-gjahr
       AND belnr EQ it_bkpf-belnr
       AND hkont IN r_hkont
       AND xref3 <> 'REVERSE'.      "Manual filter

  LOOP AT lt_bseg.
    MOVE-CORRESPONDING lt_bseg TO it_bseg.

    IF lt_bseg-shkzg = 'H'.
      it_bseg-dmbtr = - lt_bseg-dmbtr.
      it_bseg-wrbtr = - lt_bseg-wrbtr.
    ENDIF.

    IF p_date = space.
      CLEAR: it_bseg-valut, it_bseg-fdtag.
    ENDIF.

    CLEAR: it_bseg-shkzg.
    COLLECT it_bseg. CLEAR:it_bseg.
  ENDLOOP.

  LOOP AT it_bseg.
    IF it_bseg-dmbtr < 0.
      it_bseg-shkzg = 'H'.
    ELSE.
      it_bseg-shkzg = 'S'.
    ENDIF.
    MODIFY it_bseg INDEX sy-tabix TRANSPORTING shkzg.
  ENDLOOP.

  DESCRIBE TABLE it_bseg LINES sy-subrc.
ENDFORM.                    " get_bank_inout
*&---------------------------------------------------------------------*
*&      Form  get_vtbfha
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_vtbfha.
  DATA : l_hkont  LIKE bseg-hkont,
         l_idx   TYPE i,
         $subrc  TYPE sy-subrc.

  CLEAR:it_apar[],it_apar.
  CLEAR:it_zcmal.

  READ TABLE it_bseg INDEX 1.
*--> g/l account
*  IF it_bseg-xref3 = space.   "!!!
  SELECT wrbtr dmbtr zuonr koart kunnr lifnr
         umskz hkont gsber sgtxt aufnr buzei "augbl
         INTO CORRESPONDING FIELDS OF TABLE it_apar
         FROM bseg
         WHERE bukrs EQ it_bkpf-bukrs
           AND gjahr =  it_bkpf-gjahr
           AND belnr =  it_bkpf-belnr
           AND hkont <> it_bseg-hkont
*            AND hkont IN r_hkont93
           AND buzid <> 'T'.

*  ELSE.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*         EXPORTING
*              input  = it_bseg-xref3
*         IMPORTING
*              output = l_hkont.
*
*    SELECT wrbtr dmbtr zuonr koart kunnr lifnr
*           umskz hkont gsber sgtxt aufnr buzei "augbl
*           INTO CORRESPONDING FIELDS OF it_apar
*           FROM bseg    CLIENT SPECIFIED
*           WHERE mandt = sy-mandt
*             AND bukrs EQ it_bkpf-bukrs
*             AND gjahr =  it_bkpf-gjahr
*             AND belnr =  it_bkpf-belnr
*             AND hkont =  l_hkont.
*      COLLECT it_apar.
*    ENDSELECT.
*  ENDIF.

  LOOP AT it_apar.
*...modify by JIPARK 2004/01/15 (Using bank account)
    PERFORM check_hkont USING it_bkpf-bukrs
                             it_bseg-hkont. "it_apar-hkont.

    MOVE-CORRESPONDING it_bkpf TO it_zcmal.
    MOVE-CORRESPONDING it_apar  TO it_zcmal.

    it_zcmal-saknr  = it_bseg-hkont.

    it_zcmal-buzei  = it_apar-buzei.   "bank acct. buzei update
* vtbfhapo : Transaction Flow => Using index 3 : awkey, gjahr, bukrs
    SELECT SINGLE rfha sfhazba wzbetr bzbetr bhwbetr ssign "rrefkont
             INTO (it_zcmal-trade, it_zcmal-sbewart, it_zcmal-dispw,
                   it_zcmal-wrshb, it_zcmal-dmshb, it_zcmal-ssign)
                   ", it_zcmal-hkont)
                  FROM vtbfhapo
                  WHERE awkey EQ it_bkpf-awkey
                  AND   gjahr EQ it_bkpf-gjahr
                  AND   bukrs EQ it_bkpf-bukrs
                  AND   belnr EQ it_bkpf-belnr.

    CHECK sy-subrc EQ 0.

    PERFORM move_vtbfhapo_to_zcmal.
  ENDLOOP.

ENDFORM.                    " get_vtbfha
*&---------------------------------------------------------------------*
*&      Form  move_vtbfhapo_to_zcmal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_vtbfhapo_to_zcmal.
  IF it_zcmal-ssign EQ '-'.                   "c/d
    it_zcmal-wrshb =  it_zcmal-wrshb * ( - 1 ).
    it_zcmal-dmshb =  it_zcmal-dmshb * ( - 1 ).
  ENDIF.

  PERFORM get_grupp_ebene.
* product type
  SELECT SINGLE rantyp sgsart kontrh
         INTO (it_zcmal-rantyp, it_zcmal-gsart, it_zcmal-kontrh)
         FROM vtbfha  "Transaction
         WHERE bukrs EQ it_bkpf-bukrs
         AND   rfha  EQ it_zcmal-trade.

  IF it_zcmal-grupp EQ space.
*   PERFORM get_tm_acct.
    PERFORM get_t036v.                          "set plan group/level
  ENDIF.

  it_zcmal-bukrs = p_bukrs.

* plan date
  IF p_date = 'X' AND NOT it_bseg-valut IS INITIAL.
    it_zcmal-datum = it_bseg-valut.  "value date
  ELSE.
    it_zcmal-datum = it_bkpf-budat.  "post date
  ENDIF.

  it_zcmal-budat = it_bkpf-budat.
*----clearing date
  it_zcmal-augdt  = it_bseg-augdt.

* B/A LOGIC
  SELECT SINGLE gsber INTO it_zcmal-gsber
                FROM bseg
                WHERE bukrs EQ it_bkpf-bukrs
                AND   gjahr EQ it_bkpf-gjahr
                AND   belnr EQ it_bkpf-belnr
                AND   hkont NE it_bseg-saknr.

  it_zcmal-gubun = it_bkpf-gubun.
  it_zcmal-stblg = it_bkpf-stblg.          "..Original doc.
  it_zcmal-stjah = it_bkpf-stjah.
  it_zcmal-bstat = it_bkpf-bstat.

  APPEND it_zcmal. CLEAR it_zcmal.
ENDFORM.                    " move_vtbfhapo_to_zcmal
*&---------------------------------------------------------------------*
*&      Form  get_vdarl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_vdarl.
  DATA: l_sstorno LIKE vdbeki-sstorno. "reverse
  CLEAR: it_zcmal.

  READ TABLE it_bseg INDEX 1.
  MOVE-CORRESPONDING it_bkpf TO it_zcmal.

  it_zcmal-saknr = it_bseg-hkont.

* => using index 2 : bukrs, dgjahr, rvzblg
  SELECT SINGLE sstorno ranl rbelkpfd
                INTO (l_sstorno, it_zcmal-trade, it_zcmal-rbelkpfd)
                FROM vdbeki
                WHERE bukrs  EQ it_bkpf-bukrs
                AND   dgjahr EQ it_bkpf-gjahr
                AND   rvzblg EQ it_bkpf-belnr.

* except reverse doc.
  CHECK l_sstorno = space.
  CHECK sy-subrc EQ 0.

  SELECT SINGLE gsart rdarnehm rrefkont
                INTO (it_zcmal-gsart, it_zcmal-kontrh, it_zcmal-hkont)
                FROM vdarl
                WHERE bukrs EQ it_bkpf-bukrs
                AND   ranl  EQ it_zcmal-trade.
  it_zcmal-rantyp = '1'.  "Loan

  PERFORM get_tm_acct.

  SELECT * INTO vdbepi
           FROM vdbepi
           WHERE bukrs    EQ it_bkpf-bukrs
           AND   rbelkpfd EQ it_zcmal-rbelkpfd.

    PERFORM move_vdbepi_to_zcmal.

    IF it_bseg-dmbtr > 0.              "debit
      it_zcmal-hkont = vdbepi-rsoll.
    ELSE.                                  "credit
      it_zcmal-hkont = vdbepi-rhaben.
      it_zcmal-wrshb = it_zcmal-wrshb * ( -1 ).
      it_zcmal-dmshb = it_zcmal-dmshb * ( -1 ).
    ENDIF.

    PERFORM get_t036v.

    it_zcmal-gubun = it_bkpf-gubun.
    it_zcmal-stblg = it_bkpf-stblg.          "..Original doc.
    it_zcmal-stjah = it_bkpf-stjah.
    it_zcmal-bstat = it_bkpf-bstat.
    it_zcmal-blart = it_bkpf-blart.

    APPEND it_zcmal. CLEAR vdbepi.
  ENDSELECT.

ENDFORM.                    " get_vdarl
*&---------------------------------------------------------------------*
*&      Form  get_tm_acct
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_tm_acct.
  READ TABLE i_t037s WITH KEY rantyp    = it_zcmal-rantyp
                              rrefkont  = it_zcmal-hkont.
  it_zcmal-hkont = i_t037s-hkont.
ENDFORM.                    " get_tm_acct
*&---------------------------------------------------------------------*
*&      Form  get_t036v
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_t036v.
  DATA: l_grupp LIKE ztfi_cmad-grupp,
        l_ebene LIKE ztfi_cmad-ebene.

  SELECT SINGLE fdlevsk INTO it_zcmal-ebene   "plan level
                FROM t036v
                WHERE bukrs    EQ it_bkpf-bukrs
                AND   gsart    EQ it_zcmal-gsart  "product type
                AND   sfgzustt EQ '20'.           "activity category

  READ TABLE i_zcmad WITH KEY zeilt = 'E'  " Level
                              hkont = it_zcmal-ebene.
  it_zcmal-grupp = i_zcmad-grupp.
ENDFORM.                                                    " get_t036v
*&---------------------------------------------------------------------*
*&      Form  move_vdbepi_to_zcmal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_vdbepi_to_zcmal.
  it_zcmal-bukrs   = p_bukrs.
  it_zcmal-sbewart = vdbepi-sbewart.
* it_zcmal-rposnr  = vdbepi-rposnr.
  it_zcmal-ssolhab = vdbepi-ssolhab.
  it_zcmal-dispw   = vdbepi-scwhr.
  it_zcmal-wrshb   = vdbepi-bcwhr.
  it_zcmal-dmshb   = vdbepi-bhwhr.
  it_zcmal-gsber   = vdbepi-gsber.
  it_zcmal-datum   = vdbepi-dfaell.
  it_zcmal-budat   = it_bkpf-budat.
ENDFORM.                    " move_vdbepi_to_zcmal
*&---------------------------------------------------------------------*
*&      Form  get_bseg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bseg.
  DATA: $subrc,
        $zcmal LIKE it_zcmal.

  CLEAR:it_zcmal, $subrc.

  LOOP AT it_bseg.
    CLEAR:it_apar[],it_apar.
    READ TABLE i_skb1 WITH KEY saknr = it_bseg-hkont BINARY SEARCH.


    PERFORM make_it_apar.

    LOOP AT it_apar.
*     PERFORM check_hkont USING it_bkpf-bukrs it_apar-hkont.
      MOVE-CORRESPONDING it_apar TO it_zcmal.

      it_zcmal-saknr  = it_bseg-hkont.  "bank accout

      it_zcmal-buzei  = it_apar-buzei.   "bank acct. buzei update
      it_zcmal-ssign  = it_bkpf-ssign.
*     it_zcmal-rposnr = it_bseg-rposnr.

*-- clearing date/document
      it_zcmal-augbl = it_bseg-augbl.
      it_zcmal-augdt = it_bseg-augdt.

      PERFORM move_bseg_to_zcmal.

* multiple cash account  (FIXME)
      READ TABLE it_zcmal INTO $zcmal WITH KEY belnr = it_bseg-belnr
                                               buzei = it_apar-buzei.
      IF sy-subrc = 0.
        IF it_bseg-hkont IN r_cmacct.
          it_zcmal-grupp = 'W0'.
        ELSE.
          it_zcmal-grupp = 'W1'.
        ENDIF.
      ENDIF.

      IF i_skb1-fdlev <> 'TH'.
        APPEND it_zcmal.

      ELSE.
        IF it_zcmal-grupp+(1) EQ 'W'.

        ELSE.
          APPEND it_zcmal.
*       ENDIF.
* UD1K941019 by IG.MOON 7/16/2007 {
* check bank account = 'TH',
* if so -> after appending, create additional line (Hkont=bank account)
* {
*      IF I_SKB1-FDLEV = 'TH' AND IT_ZCMAL-GRUPP+(1) NE 'W' .
          it_zcmal-hkont = it_zcmal-saknr.
          it_zcmal-wrshb = - it_zcmal-wrshb.
          it_zcmal-dmshb = - it_zcmal-dmshb.
          PERFORM get_grupp_ebene.
          APPEND it_zcmal.
        ENDIF.
      ENDIF.
* }
      CLEAR: it_zcmal.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " get_bseg
*&---------------------------------------------------------------------*
*&      Form  check_hkont
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_$SUBRC  text
*----------------------------------------------------------------------*
FORM check_hkont USING p_bukrs p_hkont.
  READ TABLE i_skb1 WITH KEY saknr = p_hkont. "it_bseg-hkont.
  CHECK sy-subrc = 0.

  IF i_skb1-fdlev CP 'B+' OR i_skb1-fdlev CP 'C+'
  OR i_skb1-fdlev EQ 'TH'.
    it_zcmal-ebene = i_skb1-fdlev.
  ELSE.
*    $subrc = 'D'.
  ENDIF.

ENDFORM.                    " check_hkont
*&---------------------------------------------------------------------*
*&      Form  make_it_apar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_it_apar.
  DATA: lt_apar LIKE it_apar OCCURS 0  with header line.
  REFRESH lt_apar.
*--g/l account
* IF it_bseg-xref3 = space.   "!!!
  SELECT wrbtr dmbtr nebtr zuonr buzei "augbl
         shkzg koart kunnr lifnr umskz hkont rebzt gsber augbl skfbt
         INTO CORRESPONDING FIELDS OF TABLE lt_apar
         FROM bseg
         WHERE bukrs =  it_bkpf-bukrs
           AND gjahr =  it_bkpf-gjahr
           AND belnr =  it_bkpf-belnr
           AND hkont <> it_bseg-hkont
*          AND shkzg <> it_bseg-shkzg
*          AND hkont IN r_hkont93
           AND buzid <> 'T'.

  data $lt_apar like lt_apar occurs 0 with header line.
  data $ix type i.

  LOOP AT lt_apar INTO it_apar.
    IF it_apar-shkzg = 'S'.
      it_apar-wrbtr = - it_apar-wrbtr.
      it_apar-dmbtr = - it_apar-dmbtr.
    ENDIF.
    CLEAR it_apar-shkzg.

    IF it_apar-koart = 'S'.
      CLEAR: it_apar-rebzt, it_apar-augbl, it_apar-zuonr.
    ENDIF.
    COLLECT it_apar.

*    if it_apar-skfbt is initial.
*      $lt_apar = it_apar.
*      append $lt_apar.
*    endif.

  ENDLOOP.
  LOOP AT it_apar.
    IF it_apar-dmbtr < 0.
      it_apar-shkzg = 'H'.
    ELSE.
      it_apar-shkzg = 'S'.
    ENDIF.
    MODIFY it_apar INDEX sy-tabix TRANSPORTING shkzg.
  ENDLOOP.

*"Residual Item
  READ TABLE it_apar WITH KEY rebzt = 'V'.   "Payment Diff.
  IF sy-subrc = 0.
*    DELETE it_apar INDEX sy-tabix.

    LOOP AT it_apar where rebzt = 'V'.
      $ix = sy-tabix.
*-----FIXME is it OK?
*      read table $lt_apar index 1.
*      if sy-subrc eq 0.
*
*       IF it_apar-shkzg = 'H'.
*        it_apar-wrbtr = it_apar-wrbtr + $lt_apar-skfbt.
*        it_apar-dmbtr = it_apar-wrbtr + $lt_apar-skfbt.
*      else.
*        it_apar-wrbtr = it_apar-wrbtr - $lt_apar-skfbt.
*        it_apar-dmbtr = it_apar-wrbtr - $lt_apar-skfbt.
*      endif.
*      it_apar-wrbtr =
*        abs( it_apar-wrbtr * it_apar-nebtr / it_apar-dmbtr ).
*      it_apar-dmbtr =
*        abs( it_apar-nebtr ).

*        IF it_apar-shkzg = 'H'.
*          it_apar-dmbtr = - it_apar-dmbtr.
*          it_apar-wrbtr = - it_apar-wrbtr.
*        ENDIF.
*
      it_apar-rebzt = 'V'.
      MODIFY it_apar INDEX $ix.
*        delete $lt_apar index 1.
*    endif.

  ENDLOOP.
ENDIF.
ENDFORM.                    " make_it_apar
*&---------------------------------------------------------------------*
*&      Form  popup_to_confirm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_ANSWER  text
*----------------------------------------------------------------------*
FORM popup_to_confirm CHANGING fp_answer.
  DATA: l_defaultoption, l_textline1(70),  l_textline2(70).

  CLEAR fp_answer.
  l_defaultoption = 'N'.
  l_textline1     = text-005.
  l_textline2     = text-006.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            defaultoption = l_defaultoption
            textline1     = l_textline1
            textline2     = l_textline2
            titel         = sy-title
       IMPORTING
            answer        = fp_answer.

ENDFORM.                    " popup_to_confirm
*&---------------------------------------------------------------------*
*&      Form  insert_ztfi_cmac
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_$SUBRC  text
*----------------------------------------------------------------------*
FORM insert_ztfi_cmac CHANGING $subrc.
  CLEAR: ztfi_cmac, $subrc.

  ztfi_cmac-bukrs = p_bukrs.
  ztfi_cmac-tstlo = $tstlo.
  ztfi_cmac-uname = sy-uname.

  INSERT ztfi_cmac.

  IF sy-subrc NE 0.
    WRITE:/ 'Already Processed(ZTFI_CMAC).'.
    $subrc = 'E'.
  ENDIF.
ENDFORM.                    " insert_ztfi_cmac
*&---------------------------------------------------------------------*
*&      Form  insert_ztfi_cmal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_$SUBRC  text
*----------------------------------------------------------------------*
FORM insert_ztfi_cmal CHANGING $subrc.
  DATA: $ix LIKE sy-tabix.

  SORT it_zcmal BY gubun DESCENDING.

  LOOP AT it_zcmal.
    $ix = sy-tabix.

    PERFORM process_reverse_doc USING $subrc.
    IF it_zcmal-bstat = 'R'.
      DELETE it_zcmal INDEX $ix.
    ENDIF.

    CHECK it_zcmal-bstat <> 'R'.
*   compute week..
    CALL FUNCTION 'DATE_GET_WEEK'
         EXPORTING
              date = it_zcmal-budat  "posting date ??
         IMPORTING
              week = it_zcmal-week.

*   get number rage
*    PERFORM get_number_range USING it_zcmal-gjahr.

    CASE it_zcmal-gubun.
*      WHEN 'D'.  " Delete
*        DELETE FROM ztfi_cmal WHERE bukrs EQ it_zcmal-bukrs
*                              AND   gjahr EQ it_zcmal-gjahr
*                              AND   nrgnr EQ it_zcmal-nrgnr.
      WHEN 'A'.    "NEW
        CLEAR:ztfi_cmal.
        MOVE-CORRESPONDING it_zcmal TO ztfi_cmal.
        MOVE  $ix  TO  ztfi_cmal-lines.
        INSERT ztfi_cmal.
      WHEN OTHERS. "MODIFY
        CLEAR:ztfi_cmal.
        MOVE-CORRESPONDING it_zcmal TO ztfi_cmal.
        MOVE  $ix  TO  ztfi_cmal-lines.
        MODIFY ztfi_cmal.
    ENDCASE.

    IF sy-subrc <> 0.
      WRITE:/ '*** Error in Update Lineitem Table',
              ztfi_cmal-belnr, ztfi_cmal-gjahr.
      $subrc = 'E'.
    ENDIF.
  ENDLOOP.

  INSERT LINES OF wk_zcmal INTO TABLE it_zcmal.
ENDFORM.                    " insert_ztfi_cmal
*&---------------------------------------------------------------------*
*&      Form  process_reverse_doc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$SUBRC  text
*----------------------------------------------------------------------*
FORM process_reverse_doc USING    $subrc.
  DATA:$zcmal LIKE it_zcmal.

  CLEAR $subrc.
  CHECK it_zcmal-bstat EQ 'R'.              "reverse

  READ TABLE wk_zcmal WITH KEY bukrs = it_zcmal-bukrs
                               gjahr = it_zcmal-stjah
                               belnr = it_zcmal-stblg.
  CHECK sy-subrc <> 0.

  SELECT * INTO CORRESPONDING FIELDS OF wk_zcmal
           FROM ztfi_cmal
           WHERE bukrs EQ it_zcmal-bukrs
           AND gjahr EQ it_zcmal-stjah
           AND belnr EQ it_zcmal-stblg.

    wk_zcmal-wrshb = - wk_zcmal-wrshb.
    wk_zcmal-dmshb = - wk_zcmal-dmshb.
    wk_zcmal-gubun = 'R'.
    APPEND wk_zcmal.
  ENDSELECT.

  IF sy-subrc EQ 0.
    DELETE FROM ztfi_cmal
           WHERE bukrs EQ it_zcmal-bukrs
           AND   gjahr EQ it_zcmal-stjah
           AND   belnr EQ it_zcmal-stblg.
  ENDIF.
ENDFORM.                    " process_reverse_doc
*&---------------------------------------------------------------------*
*&      Form  move_bseg_to_zcmal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_bseg_to_zcmal.
* IF it_zcmal-bukrs = 'TPYJ'.
*   PERFORM check_clearing.
* ENDIF.

  PERFORM get_grupp_ebene.

  it_zcmal-wrshb = it_apar-wrbtr.
  it_zcmal-dmshb = it_apar-dmbtr.

  it_zcmal-rebzt  = it_apar-rebzt.  "V-residual, Z-partial

  it_zcmal-bukrs = p_bukrs.
  it_zcmal-gjahr = it_bkpf-gjahr.
  it_zcmal-belnr = it_bkpf-belnr.
  it_zcmal-dispw = it_bkpf-waers.
  it_zcmal-budat = it_bkpf-budat.
  it_zcmal-blart = it_bkpf-blart.

  IF p_date = 'X' AND NOT it_bseg-valut IS INITIAL.
    it_zcmal-datum = it_bseg-valut.
  ELSE.
    it_zcmal-datum = it_bkpf-budat.
  ENDIF.

  it_zcmal-awtyp  = it_bkpf-awtyp.

  it_zcmal-gubun = it_bkpf-gubun.
  it_zcmal-stblg = it_bkpf-stblg.   "..Original ??
  it_zcmal-stjah = it_bkpf-stjah.
  it_zcmal-bstat = it_bkpf-bstat.
  it_zcmal-xblnr = it_bkpf-xblnr.   "check number


ENDFORM.                    " move_bseg_to_zcmal
*&---------------------------------------------------------------------*
*&      Form  check_clearing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_clearing.
  DATA: l_zuonr LIKE bsad-zuonr.

  CHECK it_apar-koart = 'D' AND it_apar-umskz = space.
  SELECT SINGLE  zuonr INTO l_zuonr
         FROM bsad   CLIENT SPECIFIED
         WHERE mandt = sy-mandt
           AND bukrs = p_bukrs
           AND kunnr = it_apar-kunnr
           AND augbl = it_apar-augbl
           AND belnr <> it_bkpf-belnr.

  CHECK l_zuonr <> space.
  SELECT SINGLE * FROM knb1
         WHERE bukrs = p_bukrs
           AND kunnr = l_zuonr.
  IF sy-subrc = 0.
    it_zcmal-kunnr = knb1-kunnr.
  ENDIF.

ENDFORM.                    " check_clearing
*&---------------------------------------------------------------------*
*&      Form  get_grupp_ebene
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_grupp_ebene.
  DATA: l_koart LIKE bseg-koart.

  l_koart = it_zcmal-koart.
  IF ( l_koart = 'K' AND it_zcmal-dmshb > 0 ).
    SELECT SINGLE kunnr FROM lfa1 INTO it_zcmal-kunnr
                        WHERE lifnr = it_zcmal-lifnr.
    IF it_zcmal-kunnr <> space.
      l_koart = 'D'.   " OK!!!
    ENDIF.
  ENDIF.
  IF ( l_koart = 'D' AND it_zcmal-dmshb < 0 ).
    SELECT SINGLE lifnr FROM kna1 INTO it_zcmal-lifnr
                 WHERE kunnr = it_zcmal-kunnr.
    IF it_zcmal-lifnr <> space.
      l_koart = 'K'.   " OK!!!
    ENDIF.
  ENDIF.


* Get Group / Level
  CASE l_koart.
    WHEN 'K'.                                  "vendor
      PERFORM case_lifnr.
      PERFORM case_shkzg USING 'SL'.           "Credit/Debit
    WHEN 'D'.                                  "customer
      IF it_bseg-dmbtr < 0.  "outgoing pay
        SELECT SINGLE fdgrv INTO it_zcmal-grupp
          FROM lfb1
         WHERE bukrs EQ p_bukrs
           AND lifnr EQ it_zcmal-kunnr.
      ENDIF.

      IF it_zcmal-grupp = space.
        PERFORM case_kunnr.
        PERFORM case_shkzg USING 'SL'.           "C/D
      ENDIF.

*   WHEN 'S'.                                  "G/L Account
    WHEN OTHERS.
      PERFORM case_shkzg USING 'GL'.           "C/D
  ENDCASE.

  CHECK it_bkpf-awtyp NE 'TR-TM'. "add by JIPARK
  IF it_zcmal-grupp EQ space.
    PERFORM default_group.
  ELSE.
    CHECK it_bkpf-bstat <> 'R'.
    IF it_zcmal-grupp+0(1) <> 'W'.
      IF ( it_bseg-dmbtr < 0 AND it_zcmal-grupp+0(1) EQ 'A' )
      OR ( it_bseg-dmbtr > 0 AND it_zcmal-grupp+0(1) EQ 'B' ).
        PERFORM default_group.
      ENDIF.
    ENDIF.
  ENDIF.
*Issue Number : FI-20041111-007, Requested by GHLEE
*Changed on 2004/11/30, by WSKIM
*uncomment
*---Start
  IF it_zcmal-ebene EQ space.  "add by JIPARK
    PERFORM set_ebene.
  ENDIF.
*  IF it_zcmal-ebene EQ space.  "add by JIPARK
*    PERFORM set_ebene.
*  ENDIF.
*---End
* if user want to use xref3 key for planning group, then use it.
*  IF  it_bseg-xref3 <> space.
*    PERFORM read_t035.
*  ENDIF.

ENDFORM.                    " get_grupp_ebene
*&---------------------------------------------------------------------*
*&      Form  CASE_SHKZG
*&---------------------------------------------------------------------*
FORM case_shkzg USING gubun.
  DATA: l_grupp LIKE ztfi_cmad-grupp,
        l_ebene LIKE ztfi_cmad-ebene,
        l_acct  LIKE skb1-saknr.
  CLEAR l_grupp.

*  IF ( it_zcmal-saknr IN r_cmacct AND it_zcmal-hkont IN r_cmacct ).
*    l_acct = it_zcmal-saknr.
*  ELSE.
  l_acct = it_zcmal-hkont.

  CASE gubun.
    WHEN 'SL'.                           "..(AR/AP)
* only account
      LOOP AT i_zcmad WHERE zeilt = 'G'
                        AND umskz = space
                        AND shkzg = space.
        CHECK l_acct CP i_zcmad-hkont.
        l_grupp = i_zcmad-grupp.
        EXIT.
*       l_ebene = i_zcmad-ebene.  "add by JIPARK
      ENDLOOP.
* look if special gl
      LOOP AT i_zcmad WHERE zeilt = 'G'
                        AND umskz = it_zcmal-umskz
                        AND shkzg = space.
        CHECK l_acct CP i_zcmad-hkont.
        l_grupp = i_zcmad-grupp.
        EXIT.
*       l_ebene = i_zcmad-ebene.
      ENDLOOP.
* look if special gl + debit/credit
      LOOP AT i_zcmad WHERE zeilt = 'G'
                        AND umskz = it_zcmal-umskz
                        AND shkzg = it_bseg-shkzg.
        CHECK l_acct CP i_zcmad-hkont.

        l_grupp = i_zcmad-grupp.
        EXIT.
*       l_ebene = i_zcmad-ebene.
      ENDLOOP.

* look if debit/credit
      LOOP AT i_zcmad WHERE zeilt = 'G'
                        AND umskz = space
                        AND shkzg = it_bseg-shkzg.
        CHECK l_acct CP i_zcmad-hkont.
        l_grupp = i_zcmad-grupp.
        EXIT.
*       l_ebene = i_zcmad-ebene.
      ENDLOOP.

    WHEN 'GL'.                           "..Normal (G/L)
* only account
      LOOP AT i_zcmad WHERE zeilt = 'G'
                        AND umskz = space
                        AND shkzg = it_bseg-shkzg.
        CHECK l_acct CP i_zcmad-hkont.
        l_grupp = i_zcmad-grupp.
        EXIT.
      ENDLOOP.
      IF l_grupp = space.
        LOOP AT i_zcmad WHERE zeilt = 'G'
                          AND umskz = space.
          CHECK l_acct CP i_zcmad-hkont.
          l_grupp = i_zcmad-grupp.
          EXIT.
        ENDLOOP.
      ENDIF.
  ENDCASE.

* Modify by JIPARK 2004/01/15 (Using bank account)
  PERFORM check_hkont USING it_bkpf-bukrs it_zcmal-saknr. "l_acct.

  CHECK l_grupp <> space.
  it_zcmal-grupp = l_grupp.

* CHECK l_ebene <> space.   "add by JIPARK
* it_zcmal-ebene = l_ebene.
ENDFORM.                    " CASE_SHKZG
*&---------------------------------------------------------------------*
*&      Form  CASE_LIFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM case_lifnr.
  SELECT SINGLE fdgrv INTO it_zcmal-grupp
    FROM lfb1
   WHERE bukrs EQ p_bukrs
     AND lifnr EQ it_zcmal-lifnr.

ENDFORM.                    " CASE_LIFNR
*&---------------------------------------------------------------------*
*&      Form  CASE_KUNNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM case_kunnr.
  SELECT SINGLE fdgrv INTO it_zcmal-grupp
    FROM knb1
   WHERE bukrs EQ p_bukrs
     AND kunnr EQ it_zcmal-kunnr.

ENDFORM.                    " CASE_KUNNR
*&---------------------------------------------------------------------*
*&      Form  DEFAULT_GROUP
*&---------------------------------------------------------------------*
FORM default_group.
  LOOP AT i_zcmad WHERE zeilt = space.
    IF it_bseg-dmbtr > 0 AND i_zcmad-shkzg = 'H'
    OR it_bseg-dmbtr < 0 AND i_zcmad-shkzg = 'S'.
      CONTINUE.
    ELSE.
      it_zcmal-grupp = i_zcmad-grupp.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " DEFAULT_GROUP
*&---------------------------------------------------------------------*
*&      Form  READ_T035
*&---------------------------------------------------------------------*
*FORM read_t035.
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*       EXPORTING
*            input  = it_bseg-xref3
*       IMPORTING
*            output = it_bseg-xref3.
*
*  SELECT SINGLE * FROM t035
*                  WHERE grupp = it_bseg-xref3.
*  CHECK sy-subrc = 0.
*  it_zcmal-grupp  = t035-grupp.
*ENDFORM.                                                    " READ_T035
*&---------------------------------------------------------------------*
*&      Form  SET_EBENE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_ebene.
  CLEAR: it_zcmal-ebene.

* if R+++/W+++  => fix later!
*  IF it_zcmal-grupp+0(1) = 'R'.
*    it_zcmal-ebene = it_zcmal-grupp+0(2).
*    EXIT.
*  ENDIF.

* confirm special g/l
  READ TABLE i_t074 WITH KEY koart = it_zcmal-koart
                             skont = it_zcmal-hkont
                             umskz = it_zcmal-umskz.
  IF sy-subrc = 0.
    it_zcmal-ebene = i_t074-ebene.
  ELSE.
*   use plan level
    READ TABLE i_t035 WITH KEY grupp = it_zcmal-grupp.
    IF sy-subrc = 0.
      it_zcmal-ebene = i_t035-ebene.
    ENDIF.

    IF it_zcmal-ebene = space.
      READ TABLE i_zcmad WITH KEY zeilt = 'E'
                                  grupp = it_zcmal-grupp.
      it_zcmal-ebene = i_zcmad-hkont.
    ENDIF.
  ENDIF.

* fix later
*  IF ( it_zcmal-dmshb < 0 AND it_zcmal-ebene+0(1) EQ '1' ) OR
*     ( it_zcmal-dmshb > 0 AND it_zcmal-ebene+0(1) EQ '2' ).
*    CLEAR it_zcmal-ebene.
*  ENDIF.

*  CHECK it_zcmal-ebene = space.
*  IF it_zcmal-dmshb < 0.
*    it_zcmal-ebene = c_devlm.  "use Default Level
*  ELSE.
*    it_zcmal-ebene = c_devlm.  "use Default Level
*  ENDIF.

ENDFORM.                    " SET_EBENE
*&---------------------------------------------------------------------*
*&      Form  make_it_zcmas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM make_it_zcmas.
*  LOOP AT it_zcmal.
*    PERFORM summary_zcmal.
*    COLLECT it_zcmas.
*  ENDLOOP.
*
*ENDFORM.                    " make_it_zcmas
*&---------------------------------------------------------------------*
*&      Form  summary_zcmal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM summary_zcmal.
*  CLEAR: it_zcmas.
*
*  CHECK it_zcmal-grupp+0(2) <> 'WW'.
*
*  MOVE-CORRESPONDING it_zcmal TO it_zcmas.
** it_zcmas-versn = '0'.
** CLEAR:it_zcmas-datum.
*
*  it_zcmas-gjahr = it_zcmal-datum+0(04).
*  CASE it_zcmal-datum+4(02).
*    WHEN '01'.
*      it_zcmas-wtp01 = it_zcmal-wrshb.
*      it_zcmas-wlp01 = it_zcmal-dmshb.
*    WHEN '02'.
*      it_zcmas-wtp02 = it_zcmal-wrshb.
*      it_zcmas-wlp02 = it_zcmal-dmshb.
*    WHEN '03'.
*      it_zcmas-wtp03 = it_zcmal-wrshb.
*      it_zcmas-wlp03 = it_zcmal-dmshb.
*    WHEN '04'.
*      it_zcmas-wtp04 = it_zcmal-wrshb.
*      it_zcmas-wlp04 = it_zcmal-dmshb.
*    WHEN '05'.
*      it_zcmas-wtp05 = it_zcmal-wrshb.
*      it_zcmas-wlp05 = it_zcmal-dmshb.
*    WHEN '06'.
*      it_zcmas-wtp06 = it_zcmal-wrshb.
*      it_zcmas-wlp06 = it_zcmal-dmshb.
*    WHEN '07'.
*      it_zcmas-wtp07 = it_zcmal-wrshb.
*      it_zcmas-wlp07 = it_zcmal-dmshb.
*    WHEN '08'.
*      it_zcmas-wtp08 = it_zcmal-wrshb.
*      it_zcmas-wlp08 = it_zcmal-dmshb.
*    WHEN '09'.
*      it_zcmas-wtp09 = it_zcmal-wrshb.
*      it_zcmas-wlp09 = it_zcmal-dmshb.
*    WHEN '10'.
*      it_zcmas-wtp10 = it_zcmal-wrshb.
*      it_zcmas-wlp10 = it_zcmal-dmshb.
*    WHEN '11'.
*      it_zcmas-wtp11 = it_zcmal-wrshb.
*      it_zcmas-wlp11 = it_zcmal-dmshb.
*    WHEN '12'.
*      it_zcmas-wtp12 = it_zcmal-wrshb.
*      it_zcmas-wlp12 = it_zcmal-dmshb.
*  ENDCASE.
*
*ENDFORM.                    " summary_zcmal
*&---------------------------------------------------------------------*
*&      Form  update_ztfi_cmas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_$SUBRC  text
*----------------------------------------------------------------------*
*FORM update_ztfi_cmas CHANGING $subrc.
*  CLEAR: $subrc.
*
*  LOOP AT it_zcmas.
*    CHECK it_zcmas-grupp+0(2) <> 'WW'.
*    UPDATE ztfi_cmas
*      SET wtp01 = wtp01 + it_zcmas-wtp01
*          wlp01 = wlp01 + it_zcmas-wlp01
*          wtp02 = wtp02 + it_zcmas-wtp02
*          wlp02 = wlp02 + it_zcmas-wlp02
*          wtp03 = wtp03 + it_zcmas-wtp03
*          wlp03 = wlp03 + it_zcmas-wlp03
*          wtp04 = wtp04 + it_zcmas-wtp04
*          wlp04 = wlp04 + it_zcmas-wlp04
*          wtp05 = wtp05 + it_zcmas-wtp05
*          wlp05 = wlp05 + it_zcmas-wlp05
*          wtp06 = wtp06 + it_zcmas-wtp06
*          wlp06 = wlp06 + it_zcmas-wlp06
*          wtp07 = wtp07 + it_zcmas-wtp07
*          wlp07 = wlp07 + it_zcmas-wlp07
*          wtp08 = wtp08 + it_zcmas-wtp08
*          wlp08 = wlp08 + it_zcmas-wlp08
*          wtp09 = wtp09 + it_zcmas-wtp09
*          wlp09 = wlp09 + it_zcmas-wlp09
*          wtp10 = wtp10 + it_zcmas-wtp10
*          wlp10 = wlp10 + it_zcmas-wlp10
*          wtp11 = wtp11 + it_zcmas-wtp11
*          wlp11 = wlp11 + it_zcmas-wlp11
*          wtp12 = wtp12 + it_zcmas-wtp12
*          wlp12 = wlp12 + it_zcmas-wlp12
*      WHERE
*        bukrs = it_zcmas-bukrs AND
*        gjahr = it_zcmas-gjahr AND
**       versn = 0             AND
*        grupp = it_zcmas-grupp AND
*        ebene = it_zcmas-ebene AND
*        dispw = it_zcmas-dispw AND
*        gsber = it_zcmas-gsber.
*
*    IF sy-subrc <> 0.
*      MOVE-CORRESPONDING it_zcmas TO ztfi_cmas.
*      INSERT ztfi_cmas.
*    ENDIF.
*    IF sy-subrc <> 0.
*      WRITE:/ '*** Error in Update Summary Table',
*              it_zcmas-grupp, it_zcmas-ebene.
*      $subrc = 'E'.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.                    " update_ztfi_cmas
*&-------------------------------------------------------------------
*&      Form  alvprn_basic01
*&-------------------------------------------------------------------
FORM alvprn_basic01.
  FIELD-SYMBOLS: <ls_event> TYPE slis_alv_event.

  CLEAR   : gt_events, gs_layout.
  REFRESH : gt_events.

  gs_layout-header_text      = 'HEADER'.
  gs_layout-item_text        = 'item_text'.
  gs_layout-default_item     = 'X'.
* gs_layout-box_fieldname    = 'CHKBOX'.
  gs_layout-zebra            = 'X'.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = gt_events.

  DELETE gt_events WHERE name NE 'END_OF_PAGE'
                     AND name NE 'TOP_OF_PAGE'
                     AND name NE 'TOP_OF_LIST'
                     AND name NE 'END_OF_LIST'.
  LOOP AT gt_events ASSIGNING <ls_event>.
    CONCATENATE 'ALV_EVENT_'
                <ls_event>-name
                INTO <ls_event>-form.
  ENDLOOP.
*  PERFORM   form_setting
*   TABLES   gt_events
*    USING : slis_ev_pf_status_set  c_status_set,
*            slis_ev_user_command   c_user_command,
*            slis_ev_end_of_list    c_end_of_list.

* PROGRAM  ID
  g_repid = sy-repid.
ENDFORM.                    " alvprn_basic01
*&-------------------------------------------------------------------
*&      Form  form_setting
*&-------------------------------------------------------------------
FORM form_setting TABLES p_events_t LIKE gt_events
                   USING p_com   p_form.

  DATA : l_event_s    TYPE  slis_alv_event.

  READ TABLE  p_events_t  WITH KEY  name = p_com
                            INTO l_event_s.
  IF   sy-subrc EQ 0.
    MOVE     p_form      TO   l_event_s-form.
    MODIFY   p_events_t  FROM l_event_s INDEX sy-tabix.
  ENDIF.
ENDFORM.                    " form_setting
*&-------------------------------------------------------------------
*&      Form  alvprn_field01
*&-------------------------------------------------------------------
FORM alvprn_field01 USING p_intab.
  CLEAR   : gt_field, gt_fieldcat.
  REFRESH : gt_field, gt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = g_repid
            i_internal_tabname = p_intab
            i_inclname         = g_repid
       CHANGING
            ct_fieldcat        = gt_field.

* FIELD SETTING
  CLEAR g_cnt.
  PERFORM field_setting TABLES gt_fieldcat USING :
                                'S' 'AWTYP'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '08',
                                ' ' 'JUST'        'C',
                                ' ' 'FIX_COLUMN'  'X',
                                ' ' 'SELTEXT_M'   'Ref.',
                                'E' 'KEY'         ' ',
*                               'E' 'COL_POS'     '01',

                                'S' 'GSBER'       ' ',
                                ' ' 'DDICTXT'     'S',
                                ' ' 'OUTPUTLEN'   '02',
                                ' ' 'JUST'        'C',
                                'E' 'SELTEXT_S'   'BA',
*                               'E' 'COL_POS'     '10',

                                'S' 'GJAHR'       ' ',
                                ' ' 'DDICTXT'     'S',
                                ' ' 'JUST'        'C',
                                ' ' 'OUTPUTLEN'   '04',
                                ' ' 'FIX_COLUMN'  'X',
                                ' ' 'SELTEXT_S'   'Year',
                                'E' 'KEY'         ' ',
*                               'E' 'COL_POS'     '02',

                                'S' 'BELNR'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'FIX_COLUMN'  'X',
                                ' ' 'SELTEXT_M'   'PayDoc#',
*                               ' ' 'COL_POS'     '03',
                                'E' 'KEY'         ' ',

                                'S' 'BUZEI'       ' ',
                                ' ' 'DDICTXT'     'S',
                                ' ' 'OUTPUTLEN'   '04',
                                ' ' 'JUST'        'C',
                                'E' 'SELTEXT_S'   'Item',
*                               'E' 'COL_POS'     '04',

                                'S' 'AUGBL'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'JUST'        'C',
                                'E' 'SELTEXT_M'   'Clr.Doc',
*                               'E' 'COL_POS'     '23',

                                'S' 'SAKNR'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                'E' 'SELTEXT_M'   'Bank Acct.',
*                               'E' 'COL_POS'     '05',

                                'S' 'GRUPP'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '08',
                                'E' 'SELTEXT_M'   'CashGrp.',
*                               'E' 'COL_POS'     '06',

                                'S' 'EBENE'       ' ',
                                ' ' 'DDICTXT'     'S',
                                ' ' 'OUTPUTLEN'   '04',
                                'E' 'SELTEXT_S'   'Lvl.',
*                               'E' 'COL_POS'     '07',

                                'S' 'DATUM'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'JUST'        'C',
                                'E' 'SELTEXT_M'   'CashDate',
*                               'E' 'COL_POS'     '09',

                                'S' 'DISPW'       ' ',
                                ' ' 'DDICTXT'     'S',
                                ' ' 'OUTPUTLEN'   '04',
                                'E' 'SELTEXT_S'   'Curr.',
*                               'E' 'COL_POS'     '08',

                                'S' 'WRSHB'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '15',
                                ' ' 'CURRENCY'    it_zcmal-dispw,
                                ' ' 'SELTEXT_M'   'Doc.Amt.',
                                'E' 'DO_SUM'      'X',
*                               'E' 'COL_POS'     '11',

                                'S' 'DMSHB'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '15',
                                ' ' 'CURRENCY'    c_waers,
                                ' ' 'SELTEXT_M'   'Loc.Amt.',
                                'E' 'DO_SUM'      'X',
*                               'E' 'COL_POS'     '12',

                                'S' 'KOART'       ' ',
                                ' ' 'DDICTXT'     'S',
                                ' ' 'OUTPUTLEN'   '01',
                                ' ' 'JUST'        'C',
                                'E' 'SELTEXT_S'   'A',
*                               'E' 'COL_POS'     '14',

                                'S' 'BSTAT'       ' ',
                                ' ' 'DDICTXT'     'S',
                                ' ' 'OUTPUTLEN'   '04',
                                ' ' 'JUST'        'C',
                                'E' 'SELTEXT_S'   'Status',
*                               'E' 'COL_POS'     '15',

                                'S' 'UMSKZ'       ' ',
                                ' ' 'DDICTXT'     'S',
                                ' ' 'OUTPUTLEN'   '04',
                                ' ' 'JUST'        'C',
                                'E' 'SELTEXT_S'   'S/GL',
*                               'E' 'COL_POS'     '16',

                                'S' 'XBLNR'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                'E' 'SELTEXT_M'   'Reference',
*                               'E' 'COL_POS'     '17',

                                'S' 'HKONT'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '08',
                                'E' 'SELTEXT_M'   'G/LAcct',
*                               'E' 'COL_POS'     '18',

                                'S' 'LIFNR'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                'E' 'SELTEXT_M'   'Vendor',
*                               'E' 'COL_POS'     '19',

                                'S' 'KUNNR'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                'E' 'SELTEXT_M'   'Customer',
*                               'E' 'COL_POS'     '20',

                                'S' 'SBEWART'     ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '08',
                                'E' 'SELTEXT_M'   'Flw.Typ.',
*                               'E' 'COL_POS'     '21',

                                'S' 'GSART'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '08',
                                'E' 'SELTEXT_M'   'Prd.Typ.',
*                               'E' 'COL_POS'     '22',

                                'S' 'BUDAT'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'JUST'        'C',
                                'E' 'SELTEXT_M'   'Pst.Date',
*                               'E' 'COL_POS'     '23',


                                'S' 'AUGDT'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'JUST'        'C',
                                'E' 'SELTEXT_M'   'Clr.Date',
*                               'E' 'COL_POS'     '23',


                                'S' 'DMBTR2'      ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '15',
                                ' ' 'CURRENCY'    c_waers,
                                ' ' 'SELTEXT_M'   'Total Amt.'.
*                               'E' 'COL_POS'     '13',

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  display_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_list TABLES p_it_list.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
*         I_INTERFACE_CHECK        = ' '
          i_callback_program       =  g_repid
          i_callback_pf_status_set = 'PF_STATUS_SET'
          i_callback_user_command  = 'USER_COMMAND'
          is_layout                =  gs_layout
          it_fieldcat              =  gt_fieldcat[]
*         IT_EXCLUDING             =
*         IT_SPECIAL_GROUPS        =  GT_SLIS_SP_GROUP_ALV
*         IT_SORT                  =  IT_SORT[]
*         IT_FILTER                =
*         IS_SEL_HIDE              =
*         I_DEFAULT                = 'X'
          i_save                   =  g_save
          is_variant               =  g_variant
          it_events                =  gt_events[]
       TABLES
          t_outtab                 =  p_it_list.

ENDFORM.                    " display_list
*&-------------------------------------------------------------------
*&      Form  field_setting
*&-------------------------------------------------------------------
FORM field_setting TABLES p_fieldcat_t LIKE gt_fieldcat
                    USING p_gub p_fname p_con.

  DATA: l_col(40).

  IF p_gub = 'S'.
    CLEAR g_fieldcat_s.
    READ TABLE gt_field INTO g_fieldcat_s
                        WITH KEY fieldname  = p_fname.
    EXIT.
  ENDIF.

  FIELD-SYMBOLS <fs>.
  CONCATENATE 'G_FIELDCAT_S-' p_fname  INTO l_col.
  ASSIGN      (l_col)         TO       <fs>.
  MOVE         p_con          TO       <fs>.


* DATA  APPEND
  CHECK  p_gub = 'E'.

  g_cnt = g_cnt + 1.
  g_fieldcat_s-col_pos = g_cnt.

*  g_fieldcat_s-seltext_l = g_fieldcat_s-seltext_s
*                         = g_fieldcat_s-seltext_m.
  APPEND g_fieldcat_s TO p_fieldcat_t.
ENDFORM.                    " field_setting
*&---------------------------------------------------------------------*
*&      Form  get_number_range
*&---------------------------------------------------------------------*
*FORM get_number_range USING p_gjahr.
*  CLEAR g_nrgnr.
*
*  CALL FUNCTION 'NUMBER_GET_NEXT'
*       EXPORTING
*            nr_range_nr             = '01'
*            object                  = 'ZNFI_CMAL'
*            quantity                = '1'
*            toyear                  = p_gjahr  "sy-datum(4)
*       IMPORTING
*            number                  = g_nrgnr
*       EXCEPTIONS
*            interval_not_found      = 1
*            number_range_not_intern = 2
*            object_not_found        = 3
*            quantity_is_0           = 4
*            quantity_is_not_1       = 5
*            interval_overflow       = 6
*            OTHERS                  = 7.
*
*ENDFORM.                    " get_number_range
*&---------------------------------------------------------------------*
*&      Form  help_blart
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_BLART_LOW  text
*----------------------------------------------------------------------*
FORM help_blart USING    p_blart.
ENDFORM.                    " help_blart
*&---------------------------------------------------------------------*
*&      Form  delete_setoff
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_setoff.
  DATA : it_zcmal_temp LIKE it_zcmal OCCURS 0 WITH HEADER LINE.
  DATA : l_wrshb LIKE it_zcmal-wrshb,
         l_del(1).

* Delete : IF Document Type = ZR, AB
*          Same  DATUM, SAKNR, LIFNR and WRSHB(-1)

*  it_zcmal_temp[] = it_zcmal[].
*  LOOP AT it_zcmal WHERE blart = 'AB' OR  blart = 'ZR'.
*    CLEAR : l_wrshb , l_del.
*    l_wrshb = it_zcmal-wrshb * -1.
*
*    LOOP AT it_zcmal_temp WHERE datum  =  it_zcmal-datum
*                            AND saknr  =  it_zcmal-saknr
*                            AND lifnr  =  it_zcmal-lifnr
*                            AND ( blart  =  'AB' OR blart  = 'ZR' )
*                            AND wrshb  =  l_wrshb.
*      l_del = 'X'.
*      EXIT.
*    ENDLOOP.
*    IF l_del = 'X'.
*      DELETE it_zcmal FROM IT_ZCMAL.
*      CONTINUE.
*    ENDIF.
*  ENDLOOP.



ENDFORM.                    " delete_setoff
*&---------------------------------------------------------------------*
*&      Form  modify_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen.

  LOOP AT SCREEN.

    IF screen-group1 = 'RPA'.
      IF p_test EQ 'X'. " test mode
        screen-input = 0.
        screen-invisible  = 1.
      ELSE.
        screen-input = 1.
        screen-invisible = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " modify_screen
*&---------------------------------------------------------------------*
*&      Form  DEFAULT_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM default_.
  WRITE:
          icon_payment AS ICON TO runp,
         'Run FM Pymt Only' TO runp+4(21),
          icon_protocol AS ICON TO vslt,
         'View payment log' TO vslt+4(21).

ENDFORM.                    " DEFAULT_
*&---------------------------------------------------------------------*
*&      Form  run_fm_payment
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM run_fm_payment.

  DATA $line TYPE i.

  IF p_test EQ 'X'.
    MESSAGE s000 WITH 'You can not run FM payment on Test Mode.'.
    EXIT.
  ENDIF.

  IF sscrfields-ucomm EQ 'RUNP'.
    __cls it_zcmal.

    PERFORM get_ztfi_cmal.
  ENDIF.

  DESCRIBE TABLE it_zcmal LINES $line .

  IF $line > 0.
    PERFORM run_payment TABLES it_zcmal.
  ELSE.
    MESSAGE s000 WITH 'Please Run CM Collection first.'.
    EXIT.
  ENDIF.


ENDFORM.                    " run_fm_payment
*&---------------------------------------------------------------------*
*&      Form  run_payument
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZCMAL  text
*----------------------------------------------------------------------*
FORM run_payment TABLES   p_it_zcmal STRUCTURE it_zcmal.


  DATA it_fm_tab LIKE it_zcmal OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF lt_belnr OCCURS 0,
         bukrs LIKE ztfi_cmal-bukrs,
         belnr LIKE ztfi_cmal-belnr,
         datum LIKE ztfi_cmal-datum,
         gjahr LIKE ztfi_cmal-gjahr,
        END OF lt_belnr.

  DATA $log_tbl LIKE log_tbl OCCURS 0 WITH HEADER LINE.

  it_fm_tab[] = p_it_zcmal[].

  DELETE it_fm_tab  WHERE koart NE 'K' AND koart NE 'D' .

  LOOP AT it_fm_tab.
    lt_belnr-bukrs = it_fm_tab-bukrs.
    lt_belnr-datum = it_fm_tab-datum.
    IF it_fm_tab-koart EQ 'K'.
      SELECT SINGLE belnr gjahr
      INTO (lt_belnr-belnr,lt_belnr-gjahr)

      FROM bsak  WHERE bukrs EQ it_fm_tab-bukrs
                   AND lifnr EQ it_fm_tab-lifnr
                   AND augdt EQ it_fm_tab-datum
                   AND augbl EQ it_fm_tab-belnr
                   AND belnr NE it_fm_tab-belnr
                   AND umsks EQ space.
      IF sy-subrc EQ 0.
        APPEND lt_belnr.
      ENDIF.
    ELSE.
      SELECT SINGLE belnr gjahr
      INTO (lt_belnr-belnr,lt_belnr-gjahr)
      FROM bsad  WHERE bukrs EQ it_fm_tab-bukrs
                   AND augdt EQ it_fm_tab-datum
                   AND augbl EQ it_fm_tab-belnr
                   AND belnr NE it_fm_tab-belnr
                   AND umsks EQ space.
      IF sy-subrc EQ 0.
        APPEND lt_belnr.
      ENDIF.
    ENDIF.

  ENDLOOP.

  RANGES: r_belnr FOR ztfi_cmal-belnr.
  DATA %abaplist LIKE abaplist OCCURS 0 WITH HEADER LINE.

  __cls log_tbl.
  SORT lt_belnr BY bukrs belnr.
  DATA $flag.

* { not important
* Total Doc. Count to be created.
  DATA  : total_doc_cnt TYPE i,
          current_doc_cnt TYPE i.
  DATA : percentage TYPE p,$mod TYPE i,
         $prog_text(50),$current_cnt(10),$total_cnt(10),$text(30) .
* }

  LOOP AT lt_belnr.
    AT NEW belnr.
      $flag = 'X'.
    ENDAT.

    CHECK $flag EQ 'X'.
    CLEAR $flag.

    ADD 1 TO total_doc_cnt.
  ENDLOOP.
  $total_cnt = total_doc_cnt.

  LOOP AT lt_belnr.

    AT NEW belnr.
      $flag = 'X'.
    ENDAT.

    CHECK $flag EQ 'X'.
    CLEAR $flag.


    ADD 1 TO current_doc_cnt.
    $current_cnt = current_doc_cnt.
    CONCATENATE $current_cnt '/' $total_cnt
    INTO $text.
    CONDENSE $text.
    CONCATENATE $text ':' lt_belnr-belnr INTO $prog_text SEPARATED BY
    space.
    percentage = current_doc_cnt / total_doc_cnt * 100.
    PERFORM show_progress USING $prog_text percentage.

    REFRESH r_belnr.

    r_belnr-sign = 'I'.
    r_belnr-option = 'EQ'.
    r_belnr-low = lt_belnr-belnr.
    APPEND r_belnr.

*// 2011.09.13 change by yn.kim
    SUBMIT rffms200 AND RETURN
            EXPORTING LIST TO MEMORY
            WITH p_bukrs  = lt_belnr-bukrs
            WITH p_gjahr  = lt_belnr-gjahr
            WITH s_belnr  IN r_belnr
            WITH s_erfdt  in s_datum
            WITH s_bns201 IN r_belnr
            WITH s_budat  in s_datum
            WITH s_cpudt  in s_datum
            WITH p_open   =  char_x
            WITH p_clear  =  char_x
            WITH p_cc     =  char_x
            WITH p_dlopen =  char_x
            WITH p_dlclr  =  char_x
            WITH p_dcc    =  char_x
            WITH p_rcdocs =  char_x

            WITH p_pstdat = lt_belnr-datum
            WITH p_test  = ' '
            WITH p_err   = ' '
            WITH p_liste = 'X'
            WITH p_lopen = ' '
            WITH p_no_pl = 'X' .

***            WITH p_bukrs = lt_belnr-bukrs
***            WITH p_gjahr = lt_belnr-gjahr
***            WITH s_belnr IN r_belnr
***            WITH p_pstdat = lt_belnr-datum
***            WITH p_test  = ' '
***            WITH p_err   = ' '
***            WITH p_liste = 'X'
***            WITH p_lopen = ' '
***            WITH p_no_pl = 'X' .
*//  change end. ============================== //*

    IF sy-batch IS INITIAL.
      __cls %abaplist.

      CALL FUNCTION 'LIST_FROM_MEMORY'
           TABLES
                listobject = %abaplist
           EXCEPTIONS
                not_found  = 1
                OTHERS     = 2.

      IF sy-subrc = 0.
        __cls $log_tbl.

        CALL FUNCTION 'LIST_TO_ASCI'
             TABLES
                  listasci   = $log_tbl
                  listobject = %abaplist
             EXCEPTIONS
                  OTHERS     = 99.
        IF sy-subrc NE 0.
        ENDIF.

      ENDIF.

      APPEND LINES OF $log_tbl TO log_tbl.
    ENDIF.

  ENDLOOP.


ENDFORM.                    " run_payument
*&---------------------------------------------------------------------*
*&      Form  view_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM view_log.
  CALL SCREEN 9000.
ENDFORM.                    " view_log
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'ZLOG'.
  sy-title = 'Error log...'.
  SUPPRESS DIALOG.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  PERFORM error_list.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ERROR_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM error_list.
  LOOP AT log_tbl.
    WRITE:/ log_tbl-zeile.
  ENDLOOP.

ENDFORM.                    " ERROR_LIST
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$PROG_TEXT  text
*      -->P_PERCENTAGE  text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
