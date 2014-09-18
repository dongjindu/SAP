*----------------------------------------------------------------------
* Program ID        : ZAFIU127
* Title             : [FI] Check Extract Creation
* Created on        : 08/10/2007
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : Check Extract Creation
*----------------------------------------------------------------------
* Modification Logs
* Date       Developer  Request    Description
* 10/20/2010 VALERIAN   UD1K949978 Include house bank 'WA2' in current
*                                  check printing format
*----------------------------------------------------------------------
REPORT zacou126
  LINE-COUNT (1)
  LINE-SIZE 132
  NO STANDARD PAGE HEADING
  MESSAGE-ID fibl.

TABLES:
  bnka,                                "Bankenstamm - Tabelle
  dtachkh,                             "inttab: Daten zu Header und
  dtachkp,                             "Posten bei Extrakterstellung
  payr,                                "Scheckdatei
  ifibl_aux_fields,                    "Select-Options
  t001,                                "für das Land des Buchungskreises
  t005,                                "für den Bankschlüssel
  t012,                                "Banken
  t012k.                               "Bankenkonten

INCLUDE : z_moon_alv_top,
          z_moon_alv_fnc.
INCLUDE zafiu127_top.
*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
INCLUDE: rfchki99.

PARAMETERS:
  par_zbuk LIKE payr-zbukr OBLIGATORY MEMORY ID buk,
  par_hbki LIKE payr-hbkid OBLIGATORY MEMORY ID hbk,
  par_waer LIKE payr-waers OBLIGATORY MEMORY ID whr DEFAULT 'USD'.
PARAMETERS par_void LIKE ifibl_aux_fields-chkexlst+ DEFAULT 'X'.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK 0 WITH FRAME TITLE text-100.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:
  par_xzhl LIKE ifibl_aux_fields-chkexzhl RADIOBUTTON GROUP 1.
SELECTION-SCREEN:
  COMMENT 03(29) text-003 FOR FIELD par_xzhl.
PARAMETERS:
  zw_laufd LIKE payr-laufd.
SELECTION-SCREEN POSITION 46.
PARAMETERS:
  zw_laufi LIKE payr-laufi,
  zw_xvorl LIKE boole-boole NO-DISPLAY.

SELECTION-SCREEN:
  END OF LINE,
  BEGIN OF LINE.
SELECTION-SCREEN:
  END OF LINE,
  BEGIN OF LINE.
PARAMETERS:
  par_xels LIKE ifibl_aux_fields-chkexels RADIOBUTTON GROUP 1.
SELECTION-SCREEN:
  COMMENT 03(50) text-005 FOR FIELD par_xels,
  END OF LINE.
SELECT-OPTIONS sel_zald FOR ifibl_aux_fields-chkladat.

SELECTION-SCREEN END   OF BLOCK 0.

SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE text-101.
SELECT-OPTIONS:
  sel_hkti FOR payr-hktid.
select_option_scheck.
SELECT-OPTIONS:
  sel_rwbt FOR payr-rwbtr,
  sel_cpud FOR ifibl_aux_fields-chkledat,
  sel_zawe FOR payr-rzawe,
  sel_void FOR payr-voidr.

SELECT-OPTIONS s_laufi FOR payr-laufi NO INTERVALS.


SELECTION-SCREEN END   OF BLOCK 1.

SELECTION-SCREEN BEGIN OF BLOCK 2 WITH FRAME TITLE text-102.
PARAMETERS :
      par_file(50),
      " LIKE IFIBL_AUX_FIELDS-ALLGUNIX MEMORY ID GLX, "Filename
      par_xlst LIKE ifibl_aux_fields-chkexlst NO-DISPLAY.
SELECTION-SCREEN END   OF BLOCK 2.

* <<<<<<<<<<<<<< Layout >>>>>>>>>>>>>>
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

INITIALIZATION.
  g_repid = sy-repid.
  get_text: 0,1.
  PERFORM get_default_variant_get CHANGING p_vari.

  __cls sel_zawe.

  sel_zawe = 'IEQC'.
  APPEND sel_zawe.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  PERFORM bukrs_check.

  PERFORM bank_check.

  IF par_chkf GT par_chkt.                                "#EC PORTABLE
    payr-chect = par_chkt.
    par_chkt   = par_chkf.
    par_chkf   = payr-chect.
  ENDIF.
  IF par_chkf EQ space.
    par_chkf = par_chkt.
  ENDIF.
  IF par_chkt EQ space.
    par_chkt = par_chkf.
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

AT SELECTION-SCREEN ON par_waer.
  payr-waers = par_waer.

AT SELECTION-SCREEN ON BLOCK 0.

  hlp_mode = 0.

  IF par_xzhl NE space.
    hlp_mode = 2.
  ENDIF.

*--------------------------------------------------------------------*
START-OF-SELECTION.
*--------------------------------------------------------------------*

  PERFORM initialize_value.

  IF sy-batch NE space.
    PERFORM bukrs_check.
    PERFORM bank_check.
  ENDIF.
  CHECK g_error EQ space.

  PERFORM get_check.
  CHECK g_error EQ space.
  __process 'Prepare screen...' '70'.

  PERFORM get_output.     " prepare the data to ALV display

*--------------------------------------------------------------------*
END-OF-SELECTION.
  CHECK g_error EQ space .

  PERFORM set_list .

*--------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_S01  text
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  GET_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_output.

  DATA : $chect(15),
         $rwbtr(15),
         $laufd(8).

  __cls gt_out.

  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.

    IF gt_out-voidr EQ space.
      gt_out-ind_c = 'I'.
    ELSE.
      gt_out-ind_c = 'V'.
    ENDIF.
*           $CHECT(10) TYPE N,
*           $RWBTR(10) TYPE N,
*           $LAUFD(8),

    gt_out-rwbtr = - gt_out-rwbtr.
    $chect = gt_out-chect.
    $rwbtr = gt_out-rwbtr.

    PERFORM check_pure_num CHANGING :
        $chect,
        $rwbtr.

    gt_out-$chect = $chect .
    gt_out-$rwbtr = $rwbtr .

    $laufd = gt_out-laufd.

    PERFORM change_date_format CHANGING $laufd .

    gt_out-$laufd = $laufd .
    APPEND gt_out.

  ENDLOOP.

ENDFORM.                    " GET_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_list.

  CHECK g_error IS INITIAL.

  PERFORM init_alv_parm.

***   Initialization fieldcatalog   ***
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM sort_build        USING gt_sort[].

  PERFORM alv_events_get    USING:  'P', 'U', 'T', ''.
  PERFORM alv_grid_display  TABLES  gt_out USING ''.

ENDFORM.                    " SET_LIST
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_PARM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv_parm.

  __cls   :  gt_fieldcat, gt_sort, gt_events, gt_listheader,
             gt_sp_group.

  CLEAR   :  gs_layout.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-box_fieldname = 'SELECT'.
  gs_layout-box_tabname = 'GT_OUT'.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

ENDFORM.                    " INIT_ALV_PARM
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv .

  DATA: l_pos       TYPE i.

  __cls ft_fieldcat.

  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-emphasize     = &6.
    gs_fieldcat-cfieldname    = &7.
    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.

  __catalog :

    'X'  'HBKID'   'House bank'      5  'CHAR' '' '',
    'X'  'HKTID'   'ID/Acct.'        5  'CHAR' '' '',
    ' '  'LAUFI'   'Ident.'          6  'CHAR' '' '',
    ' '  'RZAWE'   'P'               1  'CHAR' '' '',
    ' '  'CHECT'  'Check Number'    13  'CHAR' '' '',
    ' '  'RWBTR'  'Dollar Amount'   13  'CURR' '' 'WAERS',
    ' '  'LAUFD'  'Issue Date'       8  'DATS' '' '',
    ' '  'IND_C'   'Ind'             1  'CHAR' 'C50' '',
    ' '  'BANKN'   'Acct.Number'    18  'CHAR' 'C50' '',
    ' '  '$CHECT'  'Check Number'   10  'CHAR' 'C50' '',
    ' '  '$RWBTR'  'Dollar Amount'  10  'CHAR' 'C50' '',
    ' '  '$LAUFD'  'Issue Date'      8  'CHAR' 'C50' '',
    ' '  'ZNME1'   'Payee'          35  'CHAR' 'C50' ''.


  PERFORM change_fieldcat USING ft_fieldcat[] .

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build USING    ft_sort TYPE slis_t_sortinfo_alv.

  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-comp      = &5.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

  sort_tab :
             'HBKID'      ' ' 'X' 'X' 'X',
             'HKTID'      ' ' 'X' 'X' 'X',
             'LAUFI'      ' ' 'X' 'X' 'X',
             'RZAWE'      ' ' 'X' 'X' 'X',
             'CHECT'      ' ' 'X' 'X' 'X'.
ENDFORM.                    " SORT_BUILD

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  DATA l_text(60).
  DATA s_text(60).

  REFRESH gt_listheader.
*  PAR_ZBUK LIKE PAYR-ZBUKR OBLIGATORY MEMORY ID BUK,
*  PAR_HBKI LIKE PAYR-HBKID OBLIGATORY MEMORY ID HBK,

  l_text = 'Check List to Extract.'.
  PERFORM set_header_line USING:
           'P' 'H' ''                 l_text       '',
           'P' 'S' 'Paying company code' par_zbuk      '',
           'P' 'S' 'House Bank'     par_hbki      ''.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = gt_listheader.

ENDFORM.                    "top_of_page
*---------------------------------------------------------------------*
*       FORM PF_STATUS_SET
*---------------------------------------------------------------------*
FORM pf_status_set USING  ft_extab TYPE slis_t_extab.
  SET PF-STATUS '100' EXCLUDING ft_extab.
ENDFORM.                    "PF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM user_command USING fp_ucomm LIKE sy-ucomm
                        fs       TYPE slis_selfield.
  CLEAR : g_error.
*  FIELD-SYMBOLS : <LS_LINE>, <MATNR>.
*
  CASE fp_ucomm.
    WHEN 'CSAM'.
      PERFORM browser CHANGING par_file.
      PERFORM create_bank_if_file USING par_file.

  ENDCASE.
*
*  PERFORM TOP_OF_PAGE.
*  CALL FUNCTION 'REUSE_ALV_GRID_LAYOUT_INFO_SET'
*       EXPORTING
*            IS_LAYOUT   = GS_LAYOUT
*            IT_FIELDCAT = GT_FIELDCAT
*            IT_SORT     = GT_SORT.
*
*  FS-REFRESH    = 'X'.
*  FS-ROW_STABLE = 'X'.
*  FS-COL_STABLE = 'X'.

ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  CHANGE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FT_FIELDCAT[]  text
*      -->P_ENDFORM  text
*----------------------------------------------------------------------*
FORM change_fieldcat USING    pt_fieldcat TYPE slis_t_fieldcat_alv.

*  LOOP AT PT_FIELDCAT INTO GS_FIELDCAT.
*    CASE GS_FIELDCAT-FIELDNAME.
*
*      WHEN 'RWBTR'.
*        GS_FIELDCAT-CFIELDNAME  = 'WAERS'.
*    ENDCASE.
*
*    MODIFY PT_FIELDCAT FROM GS_FIELDCAT.
*
*  ENDLOOP.

ENDFORM.                    " CHANGE_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize_value.
  CLEAR g_error.
  __cls it_color.

ENDFORM.                    " INITIALIZE_VALUE
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_auth.
  AUTHORITY-CHECK OBJECT 'F_PAYR_BUK'
    ID 'BUKRS' FIELD par_zbuk
    ID 'ACTVT' FIELD '03'.
  IF sy-subrc NE 0.
    MESSAGE i515(fibl) WITH par_zbuk.
    g_error = 'X'.
  ENDIF.
ENDFORM.                    " CHECK_AUTH
*&---------------------------------------------------------------------*
*&      Form  GET_DEFAULT_VARIANT_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_default_variant_get  CHANGING fp_listv.

  PERFORM variant_init.
* Get default variant
  gs_variant = g_variant.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
       EXPORTING
            i_save     = g_save
       CHANGING
            cs_variant = gs_variant
       EXCEPTIONS
            not_found  = 2.
  IF sy-subrc = 0.
    fp_listv = gs_variant-variant.
  ENDIF.

ENDFORM.                    " GET_DEFAULT_VARIANT_GET
*&---------------------------------------------------------------------*
*&      Form  VARIANT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM variant_init.
  CLEAR g_variant.
  g_variant-report = sy-repid.
ENDFORM.                               " VARIANT_INIT
*&---------------------------------------------------------------------*
*&      Form  bukrs_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bukrs_check.

  SELECT SINGLE * FROM t001
    WHERE bukrs EQ par_zbuk.
  IF sy-subrc NE 0.
    IF sy-batch EQ space.
      SET CURSOR FIELD 'PAR_ZBUK'.
      MESSAGE s511 WITH par_zbuk.
      STOP.
    ELSE.
      MESSAGE s511 WITH par_zbuk.
      STOP.
    ENDIF.
  ENDIF.
  dtachkh-name = t001-butxt.

  AUTHORITY-CHECK OBJECT 'F_PAYR_BUK'
    ID 'BUKRS' FIELD par_zbuk
    ID 'ACTVT' FIELD '03'.
  IF sy-subrc NE 0.
    g_error = 'X'.
    IF sy-batch EQ space.
      SET CURSOR FIELD 'PAR_ZBUK'.
      MESSAGE s515 WITH par_zbuk.
      STOP.
    ELSE.
      MESSAGE s515 WITH par_zbuk.
      STOP.
    ENDIF.
  ENDIF.

  SELECT SINGLE * FROM t005 WHERE land1 EQ t001-land1.

ENDFORM.                    " bukrs_check
*&---------------------------------------------------------------------*
*&      Form  bank_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bank_check.
  SELECT SINGLE * FROM t012
    WHERE bukrs EQ par_zbuk
      AND hbkid EQ par_hbki.
  IF sy-subrc NE 0.                    "Bank nicht gefunden
    g_error = 'X'.
    IF sy-batch EQ space.
      SET CURSOR FIELD 'PAR_HBKI'.
      MESSAGE e514 WITH par_hbki.
    ELSE.
      MESSAGE s514 WITH par_hbki.
      STOP.
    ENDIF.
  ENDIF.

  SELECT * FROM t012k
    WHERE bukrs EQ par_zbuk
      AND hbkid EQ par_hbki
      AND hktid IN sel_hkti.
    EXIT.
  ENDSELECT.
  IF sy-subrc NE 0.                    "keine Konten gefunden
    g_error = 'X'.

    IF sy-batch EQ space.
      SET CURSOR FIELD 'SEL_HKTI-LOW'.
      MESSAGE e619.
    ELSE.
      MESSAGE s619.
      STOP.
    ENDIF.
  ENDIF.

  IF t005-bnkey EQ '2'.
    t012-bankl = t012k-bankn.
  ENDIF.

  SELECT SINGLE * FROM bnka
    WHERE banks EQ t012-banks
      AND bankl EQ t012-bankl.
  IF sy-subrc NE 0.                    "Bank nicht gefunden
    g_error = 'X'.
    IF sy-batch EQ space.
      SET CURSOR FIELD 'PAR_HBKI'.
      MESSAGE e617 WITH t012-banks t012-bankl.
    ELSE.
      MESSAGE s617 WITH t012-banks t012-bankl.
      STOP.
    ENDIF.
  ENDIF.
  dtachkh-bankl = bnka-bnklz.          "Bankleitzahl sichern

ENDFORM.                    " bank_check
*&---------------------------------------------------------------------*
*&      Form  get_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_check.

  sel_waer-low    = par_waer.          "Eingetragene Währung übernehmen
  sel_waer-high   = space.
  sel_waer-sign   = 'I'.
  sel_waer-option = 'EQ'.
  APPEND sel_waer.

  sel_waer-low    = '   '.             "zusätzlich leeres Währungsfeld
  APPEND sel_waer.                     "bei voided-checks akzeptieren

  dtachkp-filler  = space.             "Als leer initialisieren

  INCLUDE rfchki10.

  IF par_xzhl NE space.                "Zahllauf wurde selektiert
    IF ( zw_laufd IS INITIAL OR        "Kein Datum angegeben oder
         zw_laufi IS INITIAL ).        "keine Identifikation angegeben
      IF sy-batch EQ space.
        MESSAGE s610.
        g_error = 'X'.
        STOP.
      ELSE.
        g_error = 'X'.
        MESSAGE s610.
        g_error = 'X'.
        STOP.
      ENDIF.
    ENDIF.
  ENDIF.

  IF par_file EQ space.                "Kein Dateiname wurde angegeben
    par_file    = 'CHK'.
    par_file+8  = '-'.
    WRITE sy-datlo TO par_file+9(6) YYMMDD.
    par_file+15 = '-'.
    par_file+16 = sy-timlo.
    CONCATENATE par_file '.txt' INTO par_file.
    CONDENSE par_file NO-GAPS.
  ENDIF.

  REFRESH : sel_laud,
            sel_laui.

  CLEAR   : sel_laud,
            sel_laui.

  CASE hlp_mode.

    WHEN 0.

    WHEN 2.                            "Schecks eines Zahllaufs
      IF zw_xvorl EQ space.            "Echtlauf
        sel_laud-low    = zw_laufd.
        sel_laud-option = 'EQ'.
        sel_laud-sign   = 'I'.
        APPEND sel_laud.
        sel_laui-low    = zw_laufi.
        sel_laui-option = 'EQ'.
        sel_laui-sign   = 'I'.
        APPEND sel_laui.
      ELSE.                            "Vorschlagslauf
        MESSAGE s615 WITH sy-repid.    "ZW_XVORL als NO-DISPLAY hat 'X'
        STOP.
      ENDIF.

  ENDCASE.

*  SELECT * FROM PAYR
*   WHERE ICHEC EQ SPACE
*   AND ZBUKR EQ PAR_ZBUK
*   AND HBKID EQ PAR_HBKI
*   AND HKTID IN SEL_HKTI
*   AND RZAWE IN SEL_ZAWE
*   AND CHECF LE PAR_CHKT
*   AND CHECT GE PAR_CHKF
*   AND LAUFD IN SEL_LAUD
*   AND LAUFI IN SEL_LAUI
*   AND ZALDT IN SEL_ZALD
*   AND WAERS IN SEL_WAER
*   AND RWBTR IN SEL_RWBT
*   AND VOIDR IN SEL_VOID
*   AND ( PRIDT IN SEL_CPUD OR VOIDD IN SEL_CPUD )
*   AND XBANC NE 'X'.
*
*    SELECT SINGLE * FROM T012K WHERE BUKRS EQ PAYR-ZBUKR
*                                 AND HBKID EQ PAYR-HBKID
*                                 AND HKTID EQ PAYR-HKTID.
*
*    WRITE: / PAYR-LAUFI,
*             T012K-BANKN,
*             PAYR-CHECT,
*             PAYR-RWBTR,
*             PAYR-LAUFD,
*             PAYR-ZNME1.
*
*  ENDSELECT.

  IF par_void EQ space.

    SELECT
      a~zbukr a~hbkid a~hktid a~rzawe a~laufi
      a~chect a~laufd b~bankn a~waers a~rwbtr
      a~znme1 a~voidr
      INTO CORRESPONDING FIELDS OF TABLE it_row_tab
      FROM payr AS a
      JOIN t012k AS b
      ON b~bukrs EQ a~zbukr
      AND b~hbkid EQ a~hbkid
      AND b~hktid EQ a~hktid
       WHERE a~ichec EQ space
       AND a~zbukr EQ par_zbuk
       AND a~hbkid EQ par_hbki
       AND a~hktid IN sel_hkti
       AND a~rzawe IN sel_zawe
       AND a~checf LE par_chkt
       AND a~chect GE par_chkf
       AND a~laufd IN sel_laud
       AND a~laufi IN sel_laui
       AND a~zaldt IN sel_zald
       AND a~waers IN sel_waer
       AND a~rwbtr IN sel_rwbt
       AND a~voidr IN sel_void
       AND ( a~pridt IN sel_cpud OR a~voidd IN sel_cpud )
       and A~XBANC ne 'X'
       AND a~voidr EQ space
       AND a~laufi IN s_laufi.

  ELSE.
    SELECT
    a~zbukr a~hbkid a~hktid a~rzawe a~laufi
    a~chect a~laufd b~bankn a~waers a~rwbtr
    a~znme1 a~voidr
    INTO CORRESPONDING FIELDS OF TABLE it_row_tab
    FROM payr AS a
    JOIN t012k AS b
    ON b~bukrs EQ a~zbukr
    AND b~hbkid EQ a~hbkid
    AND b~hktid EQ a~hktid
     WHERE a~ichec EQ space
     AND a~zbukr EQ par_zbuk
     AND a~hbkid EQ par_hbki
     AND a~hktid IN sel_hkti
     AND a~rzawe IN sel_zawe
     AND a~checf LE par_chkt
     AND a~chect GE par_chkf
     AND a~laufd IN sel_laud
     AND a~laufi IN sel_laui
     AND a~zaldt IN sel_zald
     AND a~waers IN sel_waer
     AND a~rwbtr IN sel_rwbt
     AND a~voidr IN sel_void
     AND ( a~pridt IN sel_cpud OR a~voidd IN sel_cpud )
     and A~XBANC ne 'X'
     AND a~laufi IN s_laufi.


  ENDIF.

  IF sy-subrc NE 0.
    MESSAGE s509.
    STOP.
  ENDIF.


ENDFORM.                    " get_check

*---------------------------------------------------------------------*
*       FORM check_pure_num                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  N_VALUE                                                       *
*---------------------------------------------------------------------*
FORM check_pure_num CHANGING n_value.
  DATA num(12) VALUE ' 0123456789'.
  DATA $char(1).
  DATA $strlen TYPE i.
  DATA $strlen2 TYPE i.
  DATA $offset TYPE i.

  CONDENSE n_value NO-GAPS.
  $strlen = strlen( n_value ).

  DO $strlen TIMES.
    $offset = sy-index - 1.
    $strlen2 =  strlen( n_value ).
    IF $strlen2 <= $offset.
      EXIT.
    ENDIF.
    $char = n_value+$offset(1).
    IF $char CN num.
      REPLACE $char WITH '' INTO n_value.
    ENDIF.
  ENDDO.

  CONDENSE n_value NO-GAPS.

ENDFORM.                    " CHECK_NUM
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DATE_FORMAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_$LAUFD  text
*      <--P_GT_OUT_$LAUFD  text
*      <--P_=  text
*      <--P_$LAUFD  text
*----------------------------------------------------------------------*
FORM change_date_format CHANGING p_$laufd.

  SHIFT p_$laufd BY 4 PLACES CIRCULAR.

ENDFORM.                    " CHANGE_DATE_FORMAT
*&---------------------------------------------------------------------*
*&      Form  create_bank_if_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_bank_if_file USING filename.

  DATA: $filename TYPE string.

  DATA: BEGIN OF it_datatab OCCURS 0,
          col1(100) TYPE c,
        END OF it_datatab.

  $filename = filename.

  DATA : $i TYPE i,
         $ii(10),
         $c,
         $rwbtr(30),
         $znme1(40).

  LOOP AT gt_out WHERE select = 'X'.
    IF par_hbki EQ 'WA1' or par_hbki EQ 'WA2'.              "UD1K949978
*   IF par_hbki EQ 'WA1'.                                   "UD1K949978
      IF gt_out-ind_c EQ 'V'.
        $c = 'C'.
      ELSE.
        $c = 'I'.
      ENDIF.
      WRITE gt_out-rwbtr TO $rwbtr CURRENCY gt_out-waers.

      PERFORM filter_number CHANGING $rwbtr.
      $znme1 = gt_out-znme1.

      WRITE sy-tabix TO $ii.
      CONDENSE $ii.
      DO 10 TIMES.
        REPLACE ',' WITH '' INTO $znme1.
      ENDDO.

      CONCATENATE
          $ii ','
          'WBCS' gt_out-bankn ','
          gt_out-$chect ','
          $rwbtr ','
          gt_out-$laufd(2) '/'  gt_out-$laufd+2(2) '/'gt_out-$laufd+4
          ','
          $c ','
          $znme1 ','
          '                               '
          INTO it_datatab-col1.
    ELSE.
      CONCATENATE
          gt_out-ind_c ','
          gt_out-bankn ','
          gt_out-$chect ','
          gt_out-$chect ','
          gt_out-$rwbtr ','
          gt_out-$laufd ','
          gt_out-znme1 INTO it_datatab-col1.
    ENDIF.
    APPEND it_datatab.
    CLEAR it_datatab.
  ENDLOOP.

  READ TABLE it_datatab INDEX 1.
  IF sy-subrc NE 0.
    LOOP AT gt_out.
      IF par_hbki EQ 'WA1' or par_hbki EQ 'WA2'.            "UD1K949978
*     IF par_hbki EQ 'WA1'.                                 "UD1K949978
        IF gt_out-ind_c EQ 'V'.
          $c = 'C'.
        ELSE.
          $c = 'I'.
        ENDIF.

        WRITE sy-tabix TO $ii.
        CONDENSE $ii.
        CONCATENATE
            $ii ','
            'WBCS' gt_out-bankn ','
            gt_out-$chect ','
            gt_out-$rwbtr ','
            gt_out-$laufd ','
            $c ','
            gt_out-znme1 ','
            '                               '
            INTO it_datatab-col1.
      ELSE.
        CONCATENATE
            gt_out-ind_c ','
            gt_out-bankn ','
            gt_out-$chect ','
            gt_out-$chect ','
            gt_out-$rwbtr ','
            gt_out-$laufd ','
            gt_out-znme1 INTO it_datatab-col1.
      ENDIF.
      APPEND it_datatab.
      CLEAR it_datatab.
    ENDLOOP.
  ENDIF.

  IF par_hbki EQ 'WA1' OR par_hbki EQ 'WA2'.                "UD1K949978
* IF par_hbki EQ 'WA1'.                                     "UD1K949978
    CLEAR it_datatab-col1.
    DESCRIBE TABLE it_datatab LINES $i.
    WRITE $i TO it_datatab-col1.
    CONCATENATE it_datatab-col1 ',' INTO it_datatab-col1.
    CONDENSE it_datatab-col1.
    IF $i > 0.
      INSERT it_datatab INDEX 1.
    ENDIF.
  ENDIF.


  CALL FUNCTION 'GUI_DOWNLOAD'
   EXPORTING
        filename         = $filename
        filetype         = 'ASC'
*       APPEND           = 'X'
        write_field_separator = 'X'
*       CONFIRM_OVERWRITE = 'X'
   TABLES
        data_tab         = it_datatab[]
   EXCEPTIONS
        file_open_error  = 1
        file_write_error = 2
        OTHERS           = 3.

ENDFORM.                    " create_bank_if_file
*&---------------------------------------------------------------------*
*&      Form  BROWSER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_PAR_FILE  text
*----------------------------------------------------------------------*
FORM browser CHANGING filename.

  DATA: it_tfile TYPE filetable ,
        gd_subrc TYPE i.

  DATA  $filename TYPE string.

  $filename = filename.

  CALL  METHOD cl_gui_frontend_services=>file_open_dialog
        EXPORTING
          window_title = 'Select File Name'
          default_extension = '*.txt'
          default_filename = $filename
          file_filter = '*.*'
          initial_directory = 'c:\temp\'
*         MULTISELECTION =
*         WITH_ENCODING =
        CHANGING
          file_table = it_tfile
          rc = gd_subrc.
*         USER_ACTION =
*         FILE_ENCODING =
*         EXCEPTIONS
*         FILE_OPEN_DIALOG_FAILED = 1
*         CNTL_ERROR = 2
*         ERROR_NO_GUI = 3
*         NOT_SUPPORTED_BY_GUI = 4
*         others = 5
  .
  IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    READ TABLE it_tfile INTO filename INDEX 1.
  ENDIF.

ENDFORM.                    " BROWSER
*&---------------------------------------------------------------------*
*&      Form  FILTER_NUMBER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_$RWBTR  text
*----------------------------------------------------------------------*
FORM filter_number CHANGING p_number.

  DATA: output(10) TYPE c,
        f2(10) TYPE c,
        f3 TYPE c.
  DATA: len TYPE i,
        i TYPE i.

  len = strlen( p_number ).

  DO len TIMES.
    f3 = p_number+i(1).
    IF f3 CA '0123456789.'.
      CONCATENATE f2 f3 INTO f2.
      len = len - 1.
    ENDIF.
    i = i + 1.
  ENDDO.
  CONDENSE f2.

  p_number = f2.

ENDFORM.                    " FILTER_NUMBER
