*&--------------------------------------------------------------------
*& Author                 : JIPARK
*& Creation Date          : 01/09/2004
*& Specification By       : JIPARK
*& Pattern                : Report 1-11
*& Development Request No : UD1K905610
*& Addl documentation     :
*& Description  : This is developed use ALV.
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------
*  1. Calculation Fraction (option: 05/01/03)
*     ( 12/31/03 ~ 05/01/03 ) / ( 12/31/03 ~ 01/01/03 )
*  2. Calculation AAE
*     Fraction * amount.
*&--------------------------------------------------------------------

REPORT zrfif11 MESSAGE-ID zmfi
               LINE-SIZE 200
               LINE-COUNT 65
               NO STANDARD PAGE HEADING.

************************************************************************
*     DATA DECLARATION
************************************************************************
TABLES : ztfi_fmal, fmfint, t001, covp, fmfincode, aufk.
TABLES : anlk, "yearly asset amount
         anli, anep.

DATA : it_fmal LIKE ztfi_fmal OCCURS 0 WITH HEADER LINE.
DATA : it_aufk LIKE aufk OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_list OCCURS 0,
         fincode(11), "LIKE ztfi_fmal-fincode,  "fund
         fintext(40),                      "fund desc.
         aufnr    LIKE ztfi_fmal-aufnr,    "internal order
         aftext(40),                       "order desc.
         astkz    LIKE aufk-astkz,         "statistical
         ivpro    LIKE aufk-ivpro,         "investment profile
         datum    LIKE sy-datum,
         kstar    LIKE covp-kstar,         "cost element
*        atype(15),                        "amount type(payment/capi.)
         dmshb    LIKE ztfi_fmal-dmshb,    "amount
         fract    LIKE vtmhptkond-pkond,   "fraction
         framt_py LIKE ztfi_fmal-dmshb,    "paymemtn AAE
         capamt   LIKE ztfi_fmal-dmshb,    "capitalizable amt
         KOSTV    LIKE aufk-KOSTV,         "resp.cc
         AKSTL    LIKE aufk-AKSTL,         "req.cc
         LIFNR    LIKE ztfi_fmal-LIFNR,    "vendor
         IZWEK    LIKE aufk-IZWEK,         "inv.reason
         gjahr    LIKE ztfi_fmal-gjahr,
         belnr    LIKE ztfi_fmal-belnr,
         ORAMT    LIKE COSP-WTG001,        "order actuals...
       END OF it_list.

DATA : sv_spras LIKE t001-spras,
       sv_waers LIKE t001-waers.
DATA : v_first_date LIKE sy-datum,
       v_last_date  LIKE sy-datum,
       v_frac_day1  LIKE vtbbewe-atage,
       v_frac_day2  LIKE vtbbewe-atage.
DATA : l_datum LIKE sy-datum,
       l_fonds LIKE fmzuob-fonds.

RANGES : r_kstar FOR covp-kstar,
         r_datum FOR sy-datum.

CONSTANTS : c_ptype(15) VALUE 'Payment',
            c_ctype(15) VALUE 'Capitalize',
            c_01typ(2)  VALUE '01',
            c_20typ(2)  VALUE '20',
            c_30typ(2)  VALUE '30'.

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
       ls_sort      TYPE slis_sortinfo_alv,
       gt_sort      TYPE slis_t_sortinfo_alv,
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

************************************************************************
*     SELECTION-SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS : p_bukrs LIKE glt0-bukrs OBLIGATORY MEMORY ID buk
                                     DEFAULT 'H201'.
SELECT-OPTIONS : p_datum for sy-datum OBLIGATORY NO-EXTENSION.
*Average interest rate on general debts (if no specific loan)
parameters : p_capint like vtmhptkond-pkond.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-006.
SELECT-OPTIONS : s_datum FOR sy-datum NO-EXTENSION ,
                 s_fund  FOR ztfi_fmal-fincode, "fund
                 s_auart FOR aufk-auart,        "order type
                 s_aufnr FOR ztfi_fmal-aufnr,   "order
                 s_KOSTV for aufk-KOSTV,
                 s_AKSTL for aufk-AKSTL,
                 s_LIFNR for ztfi_fmal-LIFNR,
                 s_kstar FOR covp-kstar.        "cost element
*-- tcode : ko8g (SAPLKASS - 0100)
*PARAMETERS : p_vart LIKE codia-variant.      "selection-varient
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-005.
PARAMETERS : p_fund  AS CHECKBOX DEFAULT 'X'.   "fund fraction
SELECTION-SCREEN END OF BLOCK b3.

*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS : p_pydate RADIOBUTTON GROUP rd DEFAULT 'X'.
*SELECTION-SCREEN COMMENT 5(26) text-003.
*PARAMETERS : p_cldate RADIOBUTTON GROUP rd.
*SELECTION-SCREEN COMMENT 36(36) text-004.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-007.
PARAMETERS : p_mterm AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b5.

***********************************************************************
* AT SELECTION-SCREEN
***********************************************************************
AT SELECTION-SCREEN.
  READ TABLE p_datum INDEX 1.
  IF p_datum-high IS INITIAL.
    MESSAGE e000 WITH 'To-Date is required field!'.
  ENDIF.
***********************************************************************
* START-OF-SELECTION
***********************************************************************
START-OF-SELECTION.
  CLEAR: it_fmal[], it_fmal, it_aufk[], it_aufk, it_list[], it_list.
  SELECT SINGLE spras waers INTO (sv_spras, sv_waers)
                FROM t001
                WHERE bukrs EQ p_bukrs.

* CONCATENATE p_datum(4) '0101' INTO v_first_date.
* v_last_date = p_datum.

  READ TABLE p_datum INDEX 1.

  CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
       EXPORTING
            i_date_from = p_datum-low
            i_date_to   = p_datum-high
       IMPORTING
            e_days      = v_frac_day2
       EXCEPTIONS
            OTHERS      = 1.

  IF p_fund EQ 'X'.
    PERFORM get_fund_data.
    PERFORM make_fund_list.
  ELSE.
    PERFORM get_order_data.
    PERFORM make_order_list.
  ENDIF.

  SORT it_list BY fincode aufnr datum.
***********************************************************************
* END-OF-SELECTION
***********************************************************************
END-OF-SELECTION.
  g_repid = sy-repid.

* ALV HEADER & FIELD SETTING
  PERFORM : alvprn_basic01,
            alvprn_field01 USING 'IT_LIST'.

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
*         it_sort                  =  gt_sort[]
*         IT_FILTER                =
*         IS_SEL_HIDE              =
*         I_DEFAULT                = 'X'
          i_save                   =  g_save
          is_variant               =  g_variant
          it_events                =  gt_events[]
       TABLES
          t_outtab                 =  it_list.

************************************************************************
* Form  PF_STATUS_SET
************************************************************************
FORM  pf_status_set USING p_rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD'.
ENDFORM.

************************************************************************
* AT USER-COMMAND                                                     *
************************************************************************
FORM user_command USING p_ucomm    LIKE sy-ucomm
                        p_selfield TYPE slis_selfield.
  CASE p_ucomm.
    WHEN '&IC1' OR '&ETA'.  "PICK..
      READ TABLE it_list INDEX p_selfield-tabindex.
      IF sy-subrc = 0.
*.......FUND : fi document
        IF NOT it_list-framt_py IS INITIAL.
          SET PARAMETER ID:'BLN' FIELD it_list-belnr,
                           'BUK' FIELD p_bukrs,
                           'GJR' FIELD it_list-gjahr.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ELSE.
          SET PARAMETER ID:'BLN' FIELD it_list-belnr,
                           'CAC' FIELD p_bukrs,
                           'GJR' FIELD it_list-gjahr.
          CALL TRANSACTION 'KSB5' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_fund_data
*&---------------------------------------------------------------------*
FORM get_fund_data.
* IF p_pydate EQ 'X'.
  IF s_datum[] IS INITIAL.
    SELECT * FROM ztfi_fmal
             INTO TABLE it_fmal
             WHERE bukrs   EQ p_bukrs
             AND   datum   <= p_datum-high  "fraction date
             AND   fincode NE space
             AND   fincode IN s_fund
             AND   aufnr   IN s_aufnr
             AND   lifnr   IN s_lifnr.
  ELSE.
    SELECT * FROM ztfi_fmal
             INTO TABLE it_fmal
             WHERE bukrs   EQ p_bukrs
             AND   datum   IN s_datum  "payment date
             AND   fincode NE space
             AND   fincode IN s_fund
             AND   aufnr   IN s_aufnr
             AND   lifnr   IN s_lifnr.
  ENDIF.
*  ELSEIF p_cldate EQ 'X'.
*    IF s_datum[] IS INITIAL.
*      SELECT * FROM ztfi_fmal
*               INTO TABLE it_fmal
*               WHERE bukrs   EQ p_bukrs
*               AND   augdt   NE '00000000'
*               AND   datum   <= p_datum-high  "fraction date
*               AND   fincode NE space
*               AND   fincode IN s_fund
*               AND   aufnr   IN s_aufnr.
*    ELSE.
*      SELECT * FROM ztfi_fmal
*               INTO TABLE it_fmal
*               WHERE bukrs   EQ p_bukrs
*               AND   augdt   NE '00000000'
*               AND   augdt   IN s_datum  "cleared date
*               AND   fincode NE space
*               AND   fincode IN s_fund
*               AND   aufnr   IN s_aufnr.
*    ENDIF.
*  ENDIF.
ENDFORM.                    " get_fund_data
*&---------------------------------------------------------------------*
*&      Form  make_fund_list
*&---------------------------------------------------------------------*
FORM make_fund_list.
  LOOP AT it_fmal.
    SELECT SINGLE aufnr objnr kostv akstl IZWEK
      INTO (aufk-aufnr, aufk-objnr, aufk-kostv, aufk-akstl, aufk-IZWEK)
      FROM aufk
      WHERE aufnr EQ it_fmal-aufnr
        AND   auart IN s_auart  "order type
        AND   autyp IN (c_01typ, c_20typ, c_30typ)
        AND   kostv   IN s_KOSTV
        AND   akstl   IN s_akstl.

    CHECK sy-subrc EQ 0.

*...check AuC balance is exist...
    select single * from anli
       where objnr = aufk-objnr.
    check sy-subrc = 0.

    select sum( ANBTR ) into anep-ANBTR
       from anep
       where bukrs = p_bukrs
         and anln1 = anli-anln1
         and anln2 = anli-anln2
         and BZDAT <= p_datum-high.

    if anep-ANBTR = 0.
      select sum( ANBTR ) into anep-ANBTR
             from anep
             where bukrs = p_bukrs
               and anln1 = anli-anln1
               and anln2 = anli-anln2
               and BWASL like '33%'           "acq.transfer
               and BZDAT between p_datum-low and p_datum-high.
      if anep-anbtr = 0.
        continue.
      endif.
    endif.

*    select sum( WAHKK ) into anlk-WAHKK
*       from anlk
*       where bukrs = p_bukrs
*         and anln1 = anli-anln1
*         and anln2 = anli-anln2
*         and gjahr <= p_datum-low(4).
*    check anlk-wahkk > 0.

    MOVE-CORRESPONDING it_fmal TO it_list.
    it_list-kstar  = it_fmal-fipos.
    it_list-kostv  = aufk-kostv.
    it_list-akstl  = aufk-akstl.
    it_list-IZWEK  = aufk-IZWEK.

* order actual.
    perform get_io_actual using aufk-aufnr
                          changing it_list-oramt.



*...fund description
    SELECT SINGLE beschr INTO it_list-fintext
                  FROM fmfint
                  WHERE spras EQ sv_spras
                  AND   fikrs EQ p_bukrs
                  AND   fincode EQ it_fmal-fincode.
*...fund type + fund.
    SELECT SINGLE type INTO fmfincode-type
                  FROM fmfincode
                  WHERE fikrs   EQ p_bukrs
                  AND   fincode EQ it_list-fincode.
    CONCATENATE fmfincode-type it_list-fincode INTO it_list-fincode.
*...order description
    SELECT SINGLE ktext INTO it_list-aftext
                  FROM coas
                  WHERE aufnr EQ it_list-aufnr.

*--->FUND : payment AAE
*.....calculate fraction
    CLEAR l_datum.
    IF NOT it_fmal-datum IN p_datum[].
      l_datum = p_datum-low.
    ELSE.
*     IF p_pydate EQ 'X'.
      l_datum = it_fmal-datum.  "payment date
*     ELSE.
*       l_datum = it_fmal-augdt.  "cleared date
*     ENDIF.
    ENDIF.

    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
         EXPORTING
              i_date_from = l_datum
              i_date_to   = p_datum-high
         IMPORTING
              e_days      = v_frac_day1
         EXCEPTIONS
              OTHERS      = 1.

    it_list-fract = v_frac_day1 / v_frac_day2.

    it_list-framt_py = it_list-dmshb * it_list-fract.
*Issue  : Requested by andy, changed by wskim,on 20041108
*----Start
*     it_list-capamt   = it_list-framt_py * p_capint.
    it_list-capamt   = it_list-framt_py * p_capint / 100.
*----End
    APPEND it_list. "CLEAR it_list.
    CLEAR it_list.
  ENDLOOP.
ENDFORM.                    " make_fund_list
*&---------------------------------------------------------------------*
*&      Form  alvprn_basic01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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

  PERFORM   form_setting
   TABLES   gt_events
    USING : slis_ev_pf_status_set  c_status_set,
            slis_ev_user_command   c_user_command,
            slis_ev_end_of_list    c_end_of_list.

  g_repid = sy-repid.

ENDFORM.                    " alvprn_basic01
*&---------------------------------------------------------------------*
*&      Form  alvprn_field01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0293   text
*----------------------------------------------------------------------*
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
                                'S' 'FINCODE'     ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '11',
                                'E' 'SELTEXT_M'   'Fund',

                                'S' 'FINTEXT'     ' ',
                                ' ' 'DDICTXT'     'L',
                                ' ' 'OUTPUTLEN'   '25',
                                'E' 'SELTEXT_L'   'Description',

                                'S' 'KOSTV'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '06',
                                'E' 'SELTEXT_M'   'RespCC',

                                'S' 'AKSTL'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '06',
                                'E' 'SELTEXT_M'   'Req.CC',

                                'S' 'LIFNR'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '06',
                                'E' 'SELTEXT_M'   'Vendor',

                                'S' 'AUFNR'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '07',
                                'E' 'SELTEXT_M'   'Order',

                                'S' 'AFTEXT'      ' ',
                                ' ' 'DDICTXT'     'L',
                                ' ' 'OUTPUTLEN'   '25',
                                'E' 'SELTEXT_L'   'Description',

                                'S' 'DATUM'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                'E' 'SELTEXT_M'   'Date',

                                'S' 'DMSHB'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '16',
                                ' ' 'DO_SUM'      'X',
                                ' ' 'CURRENCY'    sv_waers,
                                'E' 'SELTEXT_M'   'Amount',

                                'S' 'FRACT'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '13',
                                'E' 'SELTEXT_M'   'Fraction',

                                'S' 'FRAMT_PY'    ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '16',
                                ' ' 'DO_SUM'      'X',
                                ' ' 'CURRENCY'    sv_waers,
                                'E' 'SELTEXT_M'   'AvgExpenditure',

                                'S' 'CAPAMT'      ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '16',
                                ' ' 'DO_SUM'      'X',
                                ' ' 'CURRENCY'    sv_waers,
                                'E' 'SELTEXT_M'   'Cap.Int',


                                'S' 'IZWEK'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '02',
                                'E' 'SELTEXT_M'   'RC',

                                'S' 'KSTAR'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '06',
                                'E' 'SELTEXT_M'   'Cst.El',

                                'S' 'ORAMT'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '15',
                                ' ' 'CURRENCY'    sv_waers,
                                'E' 'SELTEXT_M'   'OrdAct',


                                'S' 'ASTKZ'       ' ',
                                ' ' 'DDICTXT'     'S',
                                ' ' 'OUTPUTLEN'   '01',
                                'E' 'SELTEXT_S'   'S',

                                'S' 'IVPRO'       ' ',
                                ' ' 'DDICTXT'     'S',
                                ' ' 'OUTPUTLEN'   '03',
                                'E' 'SELTEXT_S'   'Prf',


                                'S' 'GJAHR'       ' ',
                                ' ' 'DDICTXT'     'S',
                                ' ' 'OUTPUTLEN'   '04',
                                ' ' 'KEY'         ' ',
                                'E' 'SELTEXT_S'   'Year',

                                'S' 'BELNR'       ' ',
                                ' ' 'DDICTXT'     'M',
                                ' ' 'OUTPUTLEN'   '10',
                                ' ' 'KEY'         ' ',
                                'E' 'SELTEXT_M'   'Document'.
ENDFORM.                    " alvprn_field01
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
*&---------------------------------------------------------------------*
*&      Form  get_order_data
*&---------------------------------------------------------------------*
FORM get_order_data.
  SELECT * FROM aufk
           INTO TABLE it_aufk
           WHERE aufnr IN s_aufnr
           AND   auart IN s_auart
           AND   autyp IN (c_01typ, c_20typ, c_30typ).
*          AND   kstar IN s_kstar.
ENDFORM.                    " get_order_data
*&---------------------------------------------------------------------*
*&      Form  make_order_list
*&---------------------------------------------------------------------*
FORM make_order_list.
*--->Cost Element : capitalize AAE
  LOOP AT it_aufk.
    MOVE : it_aufk-aufnr  TO  it_list-aufnr,
           it_aufk-astkz  TO  it_list-astkz,
           it_aufk-ivpro  TO  it_list-ivpro.

    CALL FUNCTION 'FM_CO_ASSIGNMENT_READ_OBJECT'
        EXPORTING
             i_kokrs            = p_bukrs
             i_fikrs            = p_bukrs
             i_objnr            = it_aufk-objnr
             i_kstar            = ''
*             I_DATUM            = I_COBK-BUDAT
*             I_GJAHR            = I_COBK-GJAHR
        IMPORTING
             e_fonds            = l_fonds   "fund
*             E_FICTR            = c_fistl
*             E_fipex            = c_fipex
*             E_FLG_FOUND_OBJECT = l_xfeld
        EXCEPTIONS
             OTHERS             = 1.

    CHECK l_fonds IN s_fund[].  "fund

*...fund description
    SELECT SINGLE beschr INTO it_list-fintext
                  FROM fmfint
                  WHERE spras EQ sv_spras
                  AND   fikrs EQ p_bukrs
                  AND   fincode EQ l_fonds.
*...fund type + fund.
    SELECT SINGLE type INTO fmfincode-type
                  FROM fmfincode
                  WHERE fikrs   EQ p_bukrs
                  AND   fincode EQ l_fonds.
    CONCATENATE fmfincode-type l_fonds INTO it_list-fincode.
*...order description
    SELECT SINGLE ktext INTO it_list-aftext
                  FROM coas
                  WHERE aufnr EQ it_aufk-aufnr.

    IF s_datum[] IS INITIAL.
      LOOP AT p_datum.
        MOVE-CORRESPONDING p_datum TO r_datum.
        CLEAR r_datum-low.
        APPEND r_datum.
      ENDLOOP.
*     r_datum[] = p_datum[].  "to-date
    ELSE.
      r_datum[] = s_datum[].  "searching date
    ENDIF.

    SELECT gjahr belnr wtgbtr kstar budat
           FROM covp
           INTO (it_list-gjahr, it_list-belnr, it_list-dmshb,
                 it_list-kstar, it_list-datum)
           WHERE kokrs EQ p_bukrs
           AND   objnr EQ it_aufk-objnr
           AND   budat IN r_datum
           AND   stokz EQ space
           AND   gkoar EQ 'A'  "offsetting account type : asset
*          and   GJAHR eq p_datum(4)
           AND   kstar IN s_kstar.  "cost element

*.....calculate fraction
      CLEAR l_datum.
*-----> mid-month convension
      IF p_mterm EQ 'X'.
        CONCATENATE p_datum-low(6) '15' INTO l_datum.
        it_list-datum = l_datum.
      ELSE.
        IF NOT it_list-datum IN p_datum[].
          l_datum = p_datum-low.
        ELSE.
          l_datum = it_list-datum.
        ENDIF.
      ENDIF.

      CLEAR: it_list-fract, it_list-framt_py.
      CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
           EXPORTING
                i_date_from = l_datum
                i_date_to   = p_datum-high
           IMPORTING
                e_days      = v_frac_day1
           EXCEPTIONS
                OTHERS      = 1.

      it_list-fract     = v_frac_day1 / v_frac_day2.
      it_list-framt_py  = it_list-dmshb * it_list-fract.
*     CLEAR it_list-framt_py.
      APPEND it_list.  "CLEAR it_list.
    ENDSELECT.

    CLEAR it_list.
  ENDLOOP.
ENDFORM.                    " make_order_list
*&---------------------------------------------------------------------*
*&      Form  get_io_actual
*&---------------------------------------------------------------------*
FORM get_io_actual USING    fp_aufnr
                   changing fp_amt.

  DATA : lc_actual LIKE zfi_io_actual OCCURS 0 WITH HEADER LINE.
  DATA : lc_list like it_list.

  read table it_list into lc_list with key aufnr = fp_aufnr.
  if sy-subrc = 0.
    fp_amt = lc_list-oramt.
  else.
    CALL FUNCTION 'Z_FFI_GET_IO_ACTUAL'
         EXPORTING
              aufnr = fp_aufnr
         IMPORTING
              AMT   = fp_amt
         TABLES
              out   = lc_actual.
  endif.
ENDFORM.                    " get_io_actual
