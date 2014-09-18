REPORT zituseru .

TABLES: ztitbpml, zthrappusge, ztitdevc, ztitbph,
        usr02,
        tadir, tdevc, *tstc, agr_tcodes, agr_1251, agr_users, tbtcp.

DATA: gt_tdevc2   TYPE ztitdevc OCCURS 0 WITH HEADER LINE.
DATA: it_bpm LIKE ztitbpml OCCURS 0 WITH HEADER LINE.


DATA: BEGIN OF it_usr OCCURS 0,
       bname    LIKE usr02-bname,
       accnt    LIKE usr02-accnt,
       GLTGB    like usr02-GLTGB,
       uflag    like usr02-uflag,
       lic_type like usr06-lic_type,
       used     type char01,
      END OF it_usr.

DATA: BEGIN OF itab OCCURS 0,
        account LIKE zthrappusge-account,
        tcode   LIKE zthrappusge-tcode,
        usage   LIKE zthrappusge-diastepcnt,
        cinfo   TYPE syhex01,  "tcode type
        devclass LIKE tadir-devclass,
      END OF itab.

DATA: BEGIN OF it_out OCCURS 0,
        lic     like usr06-lic_type,
        accnt   TYPE xuaccnt,
        account  LIKE zthrappusge-account,
        locked   type char01,
        GLTGB   like usr02-GLTGB,

        tcode    LIKE zthrappusge-tcode,
        devclass LIKE tadir-devclass,
        ttype    TYPE char05,    "tcode type
        usage    LIKE zthrappusge-diastepcnt,

        usaget   LIKE zthrappusge-diastepcnt,
        usager   LIKE zthrappusge-diastepcnt,
        usagep   LIKE zthrappusge-diastepcnt,
        usagec   LIKE zthrappusge-diastepcnt,
*       usagee   LIKE zthrappusge-diastepcnt,
*       usagez   LIKE zthrappusge-diastepcnt,
        usagew   LIKE zthrappusge-diastepcnt,
        usageq   LIKE zthrappusge-diastepcnt,
        usageo   LIKE zthrappusge-diastepcnt,

        cnt    LIKE zthrappusge-diastepcnt,
        cntt   LIKE zthrappusge-diastepcnt,
        cntr   LIKE zthrappusge-diastepcnt,
        cntp   LIKE zthrappusge-diastepcnt,
        cntc   LIKE zthrappusge-diastepcnt,
*       cnte   LIKE zthrappusge-diastepcnt,
*       cntz   LIKE zthrappusge-diastepcnt,
        cntw   LIKE zthrappusge-diastepcnt,
        cntq   LIKE zthrappusge-diastepcnt,
        cnto   LIKE zthrappusge-diastepcnt,
      END OF it_out.


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


SELECT-OPTIONS: p_usgdt   FOR sy-datum OBLIGATORY.
SELECT-OPTIONS: s_users   FOR usr02-bname. "zthrappusge-account.
SELECT-OPTIONS: s_tcode   FOR zthrappusge-tcode.
PARAMETER  p_detail AS CHECKBOX.

INITIALIZATION.
  DATA: lv_date LIKE sy-datum.
  REFRESH p_usgdt.
  p_usgdt-high = sy-datum.
  CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
    EXPORTING
      months  = -13
      olddate = p_usgdt-high
    IMPORTING
      newdate = p_usgdt-low.
  p_usgdt-sign = 'I'.
  p_usgdt-option = 'BT'.
  APPEND p_usgdt.

START-OF-SELECTION.
  PERFORM read_data.
  PERFORM display_alv.

*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM field_setting TABLES         p_fieldcat_t LIKE gt_fieldcat
                   USING          p_fieldname       " FIELD name
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

  APPEND ls_fieldcat TO p_fieldcat_t.

ENDFORM.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
FORM display_alv.
  PERFORM field_setting TABLES gt_fieldcat USING :
 'LIC'       'License'        '05' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'ACCNT'     'Dept.'          '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'ACCOUNT'   'User'           '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'LOCKED'    'Locked'         '05' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'GLTGB'     'Valid To'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'CNTT'      'Cnt-Tran'       '07' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'CNTR'      'Cnt-Rept'       '07' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'CNTP'      'Cnt-P.Tn'       '07' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'CNTC'      'Cnt-Chk'        '07' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
*'CNTE'      'Cnt-Lock'       '07' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
*'CNTZ'      'Cnt-Ztc'        '07' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'CNTW'      'Cnt-Web'        '07' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'CNTQ'      'Cnt-Qry'        '07' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'CNTO'      'Cnt-Oth'        '07' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'CNT'       'Count'          '07' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'USAGET'    'Usg-Tran'       '12' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'USAGER'    'Usg-Rept'       '12' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'USAGEP'    'Usg-P.Tn'       '12' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'USAGEC'    'Usg-Chk'        '12' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
*'USAGEE'    'Usg-Lock'       '12' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
*'USAGEZ'    'Usg-Ztc'        '12' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'USAGEW'    'Usg-Web'        '12' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'USAGEQ'    'Usg-Query'      '12' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'USAGEO'    'Usg-Other'      '12' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'USAGE'     'Usage'          '12' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'TTYPE'     'Ttype'          '05' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'TCODE'     'Tcode'          '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'DEVCLASS'  'DevClass'       '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.

  g_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = g_repid
      it_fieldcat        = gt_fieldcat
      i_save             = 'A'
    TABLES
      t_outtab           = it_out
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.                    " display_alv
*---------------------------------------------------------------------*
*       FORM FILL_TSTC_CINFO                                          *
*---------------------------------------------------------------------*
*       Fill transaction list                                         *
*---------------------------------------------------------------------*
*       called from Form SELECT_TSTC                                  *
*---------------------------------------------------------------------*
FORM fill_tstc_cinfo.
* refer: SAPMS921
* T: Transaction
* R: Report
* C: with Check-Objekt in TSTCA
* P: Parameter transaction
* E: locked in SM01
* M: Bereichsmenue (werden hier nicht angezeigt)
  DATA: hex_tra TYPE x VALUE '00',              " Transaktion         T
        hex_men TYPE x VALUE '01',              " Area menu           -
        hex_par TYPE x VALUE '02',              " Parametertrans.     P
        hex_rep TYPE x VALUE '80',              " Report              R
        hex_rpv TYPE x VALUE '10',              " Report  w Variante  V
        hex_obj TYPE x VALUE '08',              " Objekttransaktionen
        hex_chk TYPE x VALUE '04',              " mit Prüfobjekt
        hex_enq TYPE x VALUE '20'.              " Gesperrt über SM01

*  IF itab-tcode(1) CA 'ZY'.
*    it_out-ttype = 'Z'.
*  ENDIF.

  IF     itab-cinfo O hex_rep.             " Report
    it_out-ttype = 'R'.
  ELSEIF itab-cinfo O hex_par.
    it_out-ttype = 'P'.            " Trans w. param inc.view maintenance
  ELSEIF itab-cinfo O hex_men.         " Menü
    it_out-ttype = ' '.
  ELSEIF itab-cinfo O hex_chk.         " with check object
    it_out-ttype = 'C'.
*  ELSEIF itab-cinfo O hex_enq.         " in SM01 locked
*    it_out-ttype = 'E'.
  ELSE.                             " Transaktion
    it_out-ttype = 'T'.
  ENDIF.


*Misc...
  IF itab-tcode(1) = '{'. " {ZWHR
    it_out-ttype = 'W'.
  ENDIF.
  IF itab-tcode EQ 'SAPMHTTP' OR itab-tcode EQ 'RFC'.
    it_out-ttype = 'W'.  "Web
  ENDIF.

*???
  IF itab-tcode EQ 'RSM13000'.
    it_out-ttype = 'O'.
  ENDIF.


  IF itab-tcode(2) = 'AQ' OR itab-tcode(2) = 'GP' .
*   it_out-tcode    = 'Query'.
    it_out-ttype    = 'Q'.
  ENDIF.

  IF itab-tcode = 'ZVEMAIL'. it_out-ttype = 'R'.  ENDIF.


ENDFORM.                               " FILL_TSTC_CINFO
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
FORM read_data .
  data: l_idx like sy-tabix.

*  SELECT * INTO TABLE gt_tdevc2 FROM ztitdevc.
*  SORT gt_tdevc2 BY devclass.
*
*  SELECT * INTO TABLE it_bpm FROM ztitbpml.
*  SORT it_bpm BY tcode tctyp.

  SELECT usr02~bname usr02~accnt GLTGB uflag usr06~lic_type
    INTO corresponding fields of TABLE it_usr
    FROM usr02
    left outer join usr06
       on usr02~bname = usr06~bname
    where usr02~bname IN s_users.

  SORT it_usr BY bname.

  SELECT u~account u~tcode SUM( u~diastepcnt ) t~cinfo d~devclass
    INTO TABLE itab
    FROM zthrappusge AS u
    LEFT OUTER JOIN tstc AS t
      ON u~tcode = t~tcode
    LEFT OUTER JOIN tadir AS d
      ON d~pgmid = 'R3TR'
     AND d~object = 'TRAN'
     AND d~obj_name = u~tcode
    WHERE u~ldate   IN p_usgdt
      AND u~account IN s_users
      AND u~tcode   IN s_tcode
*      and u~tcode not like '<%'
    GROUP BY u~account u~tcode t~cinfo d~devclass
    ORDER BY u~account u~tcode.

  LOOP AT itab.
    CLEAR it_out.
    IF itab-tcode CA '(/<'
    OR itab-tcode EQ 'ALE'
    OR itab-tcode EQ 'RFC'
    OR itab-tcode EQ 'RSM13000'   "update process
    OR itab-tcode EQ 'Aborted'
    OR itab-tcode EQ 'SAPMHTTP'
    OR itab-tcode EQ 'AutoABAP'
    OR itab-tcode EQ 'Buf.Sync'
    OR itab-tcode EQ 'Login_Pw'
    OR itab-tcode EQ 'Logoff'.
      CONTINUE.
    ENDIF.

    PERFORM fill_tstc_cinfo.
*    IF it_out-ttype = ' '.   " Menu, ...
    IF it_out-ttype is initial.
      CONTINUE.
    ENDIF.

    CASE it_out-ttype.
      WHEN 'T'.    it_out-usaget = itab-usage.  it_out-cntt = 1.
      WHEN 'R'.    it_out-usager = itab-usage.  it_out-cntr = 1.
      WHEN 'P'.    it_out-usagep = itab-usage.  it_out-cntp = 1.
      WHEN 'C'.    it_out-usagec = itab-usage.  it_out-cntc = 1.
*     WHEN 'E'.    it_out-usagee = itab-usage.  it_out-cnte = 1.
*     WHEN 'Z'.    it_out-usagez = itab-usage.  it_out-cntz = 1.
      WHEN 'W'.    it_out-usagew = itab-usage.  it_out-cntw = 1.
      WHEN 'Q'.    it_out-usageq = itab-usage.  it_out-cntq = 1.
      WHEN 'O'.    it_out-usageo = itab-usage.  it_out-cnto = 1.
      WHEN OTHERS. it_out-usageo = itab-usage.  it_out-cnto = 1.
    ENDCASE.

    it_out-cnt     = 1.
    it_out-usage   = itab-usage.

    IF p_detail = 'X'.
      it_out-tcode    = itab-tcode.
      it_out-devclass = itab-devclass.
    ELSE.
      CLEAR it_out-ttype.
    ENDIF.
    it_out-account = itab-account.

    READ TABLE it_usr WITH KEY bname = it_out-account BINARY SEARCH.
    IF sy-subrc = 0.
      l_idx = sy-tabix.
      it_out-accnt = it_usr-accnt.
      it_out-lic   = it_usr-lic_type.
      it_out-GLTGB = it_usr-GLTGB.
      if it_usr-uflag = 0.
        it_out-locked = ''.
      else.
        it_out-locked = 'X'.
      endif.
      it_usr-used = 'X'.
      modify it_usr index l_idx transporting used.
    ELSE.
      it_out-accnt  = '*DELETED*'.
      it_out-locked = 'D'.
    ENDIF.

    COLLECT it_out.
  ENDLOOP.

*No usage
  loop at it_usr where used = space.
* by IG Moon 3-7-2014 {
      clear it_out.
* }
      it_out-account = it_usr-bname.
      it_out-accnt   = it_usr-accnt.
      it_out-lic     = it_usr-lic_type.
      it_out-GLTGB   = it_usr-GLTGB.
      if it_usr-uflag = 0.
        it_out-locked = ''.
      else.
        it_out-locked = 'X'.
      endif.
      append it_out.
  endloop.


ENDFORM.                    " READ_DATA
