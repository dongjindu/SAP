*&--------------------------------------------------------------------
*& Author                 : Andy Choi
*& Creation Date          : 09/20/2003
*& Specification By       : Andy Choi
*& Addl documentation     :
*& Description  : Update Depreciation Data to Appr.Req Variants
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------
REPORT  zcfii11 NO STANDARD PAGE HEADING
                LINE-SIZE 132
                LINE-COUNT 65
                MESSAGE-ID zmfi.

TABLES: imak, imav, ania, anib, imavz.

* by ig.moon 11/24/2008
*////////////////////////////////////////////////////////////////////
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

INCLUDE : z_moon_alv_top,
          z_moon_alv_fnc.

INCLUDE <icon>.                        " icon

TYPES: BEGIN OF ty_row_tab,
        posnr  LIKE imak-posnr,
        txt50  LIKE imakt-txt50,
        objnr  LIKE anib-objnr,
        afabe  LIKE anib-afabe,
        afasl  LIKE anib-afasl,
        ndjar  LIKE anib-ndjar,
        anlkl  LIKE ania-anlkl,
        aktiv  LIKE ania-aktiv,
        kostl  LIKE ania-kostl,
        caufn  LIKE ania-caufn,
        gplab  LIKE ania-gplab,
        icon   TYPE icon_d,
        err,
        varnt  LIKE imavz-varnt,
        msg(60),
        investment_costs TYPE bapicurr_d,
        investment_year TYPE bapicurr_d,
 END OF ty_row_tab.

TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES: END OF ty_out.

DATA  : it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DATA: g_error(1),
      g_repid  LIKE sy-repid.

*////////////////////////////////////////////////////////////////////

DATA gv_chk.

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-010.
PARAMETERS: p_gjahr LIKE imavz-gjahr DEFAULT sy-datum(4) OBLIGATORY,
            p_versi LIKE imavz-versi DEFAULT 'ID' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b0.

SELECT-OPTIONS:
  s_ivart   FOR   imak-ivart,
  s_posnr   FOR   imak-posnr,
  s_vkostl  FOR   imak-vkostl,
* AR approval year
  s_gjahr   FOR   imak-gjahr,
  s_usr03   FOR   imak-usr03,
  s_wdatu   FOR   imak-wdatu,
  s_usr09   FOR   imak-usr09.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-01t.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

*////////////////////////////////////////////////////////////////////

DATA: c_flg_change  LIKE  t020-aktyp.

DATA: i_imak   LIKE imak OCCURS 0 WITH HEADER LINE.
DATA: i_imakpa LIKE imakpa OCCURS 0 WITH HEADER LINE.

DATA:
    lt_ania  LIKE rania OCCURS 0 WITH HEADER LINE,
    lt_anib  LIKE STANDARD TABLE OF ranib
                  INITIAL SIZE 10 WITH HEADER LINE,
    lt_anib1 LIKE STANDARD TABLE OF ranib
                  INITIAL SIZE 10 WITH HEADER LINE.

DATA: BEGIN OF i_plan_tot OCCURS 0.
        INCLUDE STRUCTURE bapiappreqplantotalmulti.
DATA: END OF i_plan_tot.

DATA: BEGIN OF i_plan_year OCCURS 0.
        INCLUDE STRUCTURE bapiappreqplanyearmulti.
DATA: END OF i_plan_year.
DATA: BEGIN OF i_plan_year_wa OCCURS 1.
        INCLUDE STRUCTURE bapiappreqplanyearmulti.
DATA: END OF i_plan_year_wa.

*&--------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

START-OF-SELECTION.

*  PERFORM get_ar_master.

  SELECT * FROM imak INTO TABLE i_imak
    WHERE ivart  IN s_ivart
      AND posnr  IN s_posnr
      AND vkostl IN s_vkostl
      AND gjahr  IN s_gjahr
      AND usr03  IN s_usr03
      AND wdatu  IN s_wdatu
      AND usr09  IN s_usr09.


  DATA $posid(24).
  DATA $investment_costs TYPE bapicurr_d.
  __cls it_row_tab.

  IF sy-subrc EQ 0.

    LOOP AT i_imak.

      CLEAR it_row_tab.
      MOVE-CORRESPONDING i_imak TO it_row_tab.
      SELECT SINGLE txt50 INTO it_row_tab-txt50
        FROM imakt WHERE spras EQ sy-langu
                     AND posnr EQ i_imak-posnr.

      SELECT SINGLE posnr varnt
            INTO (it_row_tab-posnr,
                  it_row_tab-varnt)
            FROM imavz
           WHERE gjahr EQ p_gjahr
             AND posnr EQ i_imak-posnr
             AND versi EQ p_versi.

      IF sy-subrc EQ 0.

        SELECT SINGLE objnr INTO it_row_tab-objnr
          FROM imav WHERE posnr EQ it_row_tab-posnr
             AND varnt EQ it_row_tab-varnt.

        IF sy-subrc EQ 0.
          SELECT SINGLE
               a~afabe a~afasl a~ndjar
               b~anlkl b~aktiv
               b~kostl  b~caufn b~gplab
               INTO (it_row_tab-afabe,
                     it_row_tab-afasl,
                     it_row_tab-ndjar,
                     it_row_tab-anlkl,
                     it_row_tab-aktiv,
                     it_row_tab-kostl,
                     it_row_tab-caufn,
                     it_row_tab-gplab)
                    FROM anib AS a
             INNER JOIN ania AS b
               ON  b~objnr EQ a~objnr
               AND b~lfdnr EQ a~lfdnr
             WHERE a~objnr EQ it_row_tab-objnr
               AND a~afabe EQ '20'.
        ENDIF.
      ELSE.
        IF i_imak-gjahr NE p_gjahr.
          CONTINUE.
        ENDIF.

        it_row_tab-icon = icon_led_red.
        it_row_tab-err = true.

      ENDIF.

      IF NOT it_row_tab-varnt IS INITIAL.
        $posid = it_row_tab-posnr.
        __cls : i_plan_tot, i_plan_year.
        CALL FUNCTION 'BAPI_APPREQUEST_GETDETAIL'
             EXPORTING
                  externalnumber = $posid
             TABLES
                  plan_total     = i_plan_tot
                  plan_year      = i_plan_year.

        IF sy-subrc = 0.
          READ TABLE i_plan_tot WITH KEY appreqvrnt = it_row_tab-varnt.
          IF sy-subrc EQ 0.
            it_row_tab-investment_costs = i_plan_tot-investment_costs.
          ENDIF.
          CLEAR  $investment_costs.
          LOOP AT i_plan_year INTO i_plan_year_wa
                WHERE appreqvrnt = it_row_tab-varnt.
            ADD i_plan_year_wa-investment_costs TO $investment_costs.
          ENDLOOP.
          it_row_tab-investment_year = $investment_costs.
        ENDIF.

      ENDIF.
      APPEND it_row_tab.
    ENDLOOP.

  ELSE.
    MESSAGE s000 WITH 'No data was found!'.
    STOP.
  ENDIF.

  SORT it_row_tab BY posnr.
  __cls gt_out.

  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
    APPEND gt_out.
  ENDLOOP.

END-OF-SELECTION.
  PERFORM set_output .


*&---------------------------------------------------------------------*
*&      Form  simulate_depr
*&---------------------------------------------------------------------*
FORM simulate_depr.
  CLEAR gv_chk.

  DATA:
    ls_rania LIKE rania,
    ls_anib  LIKE ranib,
    l_subrc  LIKE sy-subrc,
    l_nodep  LIKE sy-subrc.

  DATA:
        i_objnr LIKE  ania-objnr,
        i_anlkl LIKE  ania-anlkl,
        i_aktiv LIKE  ania-aktiv,
        i_bukrs LIKE  t001-bukrs,
        i_kokrs LIKE  tka01-kokrs,
        i_ivpro LIKE  taif1-ivpro,
        i_repid LIKE  sy-repid,
        i_dynnr LIKE  sy-dynnr,
        i_aktyp LIKE  t020-aktyp.

  DATA: it_afasplit LIKE afasplit OCCURS 0.

  i_bukrs = i_imak-vbukrs.  "responsible cc
*asset class
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = i_imak-usr03
       IMPORTING
            output = i_anlkl.
*cap.date
  i_aktiv = i_imak-usr09.
  i_objnr = gt_out-objnr.

*investment profile
  SELECT SINGLE ivpro INTO i_ivpro
     FROM taprf
     WHERE anlkl_s = i_anlkl.
  IF sy-subrc = 0.
    l_nodep = 0.
  ELSE.
* no depr.asset... delete..simulation data.
    l_nodep = 1.
  ENDIF.

  i_dynnr = sy-dynnr.
  i_repid = sy-repid.

* controlling area
  SELECT SINGLE kokrs INTO i_kokrs
    FROM tka02
    WHERE bukrs = i_bukrs.

  CLEAR: lt_anib, lt_anib1.
  REFRESH: lt_anib, lt_anib1.

  CALL FUNCTION 'AIPS_SIMUL_CHECK'
       EXPORTING
            i_kokrs             = i_kokrs
            i_bukrs             = i_bukrs
            i_anlkl             = i_anlkl
            i_aktiv             = i_aktiv
            i_objnr             = i_objnr
            i_ivpro             = i_ivpro
            i_dynnr             = i_dynnr
            i_repid             = i_repid
       IMPORTING
            es_ania             = ls_rania
            e_subrc             = l_subrc
       TABLES
            et_anib             = lt_anib
       EXCEPTIONS
            kein_bukrs          = 1
            bukrs_kein_am_bukrs = 2
            perioden_nicht_def  = 3.

  CHECK l_subrc IS INITIAL.


*IOPAAA0YR0001 0005    |0000022001|     |100.00
*FUNCTION DEPREC_SIMUL_MODIFIKATION.
*
*      CON_INS           LIKE T020-AKTYP  VALUE 'I',
*                                      - Update
*      CON_UPD           LIKE T020-AKTYP  VALUE 'U',
*                                      - Update set XUNVL
*      CON_UPD_SET       LIKE T020-AKTYP  VALUE 'S',
*                                      - delete
*      CON_DEL           LIKE T020-AKTYP  VALUE 'D',

  CLEAR i_imakpa.
  REFRESH i_imakpa.

  SELECT * INTO TABLE i_imakpa
    FROM imakpa WHERE posnr = i_imak-posnr.

  CLEAR lt_ania.
  REFRESH lt_ania.
  MOVE-CORRESPONDING ls_rania TO lt_ania.

*change user, date
  lt_ania-ernam = sy-uname.
  lt_ania-erdat = sy-datum.

* set insert/update/...
*  read table lt_anib into ls_anib index 1.
*  lt_ania-KZ    = ls_anib-KZ.
  SELECT SINGLE * FROM ania WHERE objnr = i_objnr.

* << Start of modification on 10.17.2006 by Michelle
*  IF SY-SUBRC = 0.
*    IF L_NODEP = 1.
*      LT_ANIA-KZ    = 'D'.
*      LT_ANIB-KZ    = 'D'.
*    ELSE.
*      LT_ANIA-KZ    = 'U'.
*      LT_ANIB-KZ    = 'U'.
*    ENDIF.
*    MODIFY LT_ANIB TRANSPORTING KZ WHERE OBJNR = I_OBJNR.
*  ELSE.
*    LT_ANIA-KZ    = 'I'.
*  ENDIF.
*
*  SELECT * INTO TABLE I_IMAKPA
*      FROM IMAKPA WHERE POSNR = I_IMAK-POSNR.
*  LOOP AT I_IMAKPA.
*    LT_ANIA-KOSTL = I_IMAKPA-AKOSTL.
*    LT_ANIA-AUFPR = I_IMAKPA-APROZ.
*    APPEND LT_ANIA.
*  ENDLOOP.

  IF sy-subrc = 0.
    PERFORM insert_appr_req USING i_objnr.
    PERFORM delete_appr_req USING i_objnr.

  ELSE.
    PERFORM insert_appr_req USING i_objnr.
  ENDIF.
* End of modification on 10.17.2006 by Michelle >>


*  CALL FUNCTION 'AIPS_SIMUL_CREATE'
*       EXPORTING
*            is_ania     = ls_rania
*            i_objnr     = i_objnr
*            i_ivpro     = i_ivpro
*            i_kokrs     = i_kokrs
*       IMPORTING
*            e_parm_flag = c_flg_change  "return
*       TABLES
*            it_afasplit = it_afasplit  "input
*            it_anib     = lt_anib. "input


ENDFORM.                    " update_depr
*&---------------------------------------------------------------------*
*&      Form  get_ar_master
*&---------------------------------------------------------------------*
FORM get_ar_master.

  SELECT * FROM imak INTO TABLE i_imak
    WHERE ivart  IN s_ivart
      AND posnr  IN s_posnr
      AND vkostl IN s_vkostl
      AND gjahr  IN s_gjahr
      AND usr03  IN s_usr03
      AND wdatu  IN s_wdatu
      AND usr09  IN s_usr09.

ENDFORM.                    " get_ar_master
*&---------------------------------------------------------------------*
*&      Form  reset_depr
*&---------------------------------------------------------------------*
FORM reset_depr USING f_objnr.
  REFRESH: lt_ania, lt_anib.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ania
      FROM ania WHERE objnr = f_objnr.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_anib
      FROM anib WHERE objnr = f_objnr.

  IF sy-subrc = 0.
    lt_ania-kz    = 'D'.
    lt_anib-kz    = 'D'.

    MODIFY lt_ania TRANSPORTING kz WHERE objnr = f_objnr.
    MODIFY lt_anib TRANSPORTING kz WHERE objnr = f_objnr.
  ENDIF.

ENDFORM.                    " reset_depr
*&---------------------------------------------------------------------*
*&      Form  INSERT_APPR_REQ
*&---------------------------------------------------------------------*
*       Get Data for Insertion of Appropriation req.
*----------------------------------------------------------------------*
FORM insert_appr_req USING p_objnr TYPE j_objnr.
  lt_ania-kz = 'I'.
  PERFORM append_lt_ania USING 'I'.

  lt_anib-lfdnr = lt_ania-lfdnr.
  lt_anib-kz = 'I'.
  PERFORM append_lt_anib1 USING p_objnr.

ENDFORM.                    " INSERT_APPR_REQ
*&---------------------------------------------------------------------*
*&      Form  DELETE_APPR_REQ
*&---------------------------------------------------------------------*
*       Get Data for deletion of Appropriation req.
*----------------------------------------------------------------------*
FORM delete_appr_req USING p_objnr TYPE j_objnr.
  lt_ania-kz = 'D'.
  PERFORM append_lt_ania USING 'D'.

  lt_anib-lfdnr = lt_ania-lfdnr.
  lt_anib-kz = 'D'.
  PERFORM append_lt_anib1 USING p_objnr.

ENDFORM.                    " DELETE_APPR_REQ
*&---------------------------------------------------------------------*
*&      Form  APPEND_LT_ANIA
*&---------------------------------------------------------------------*
FORM append_lt_ania USING p_kz.
  LOOP AT i_imakpa.
    IF p_kz = 'I'.
      IF ania IS INITIAL.
        CLEAR lt_ania-lfdnr.
      ELSE.
        lt_ania-lfdnr = ania-lfdnr + 1.
      ENDIF.

    ELSEIF p_kz = 'D'.
      MOVE-CORRESPONDING ania TO lt_ania.
    ENDIF.

    lt_ania-kostl = i_imakpa-akostl.
    lt_ania-aufpr = i_imakpa-aproz.
    APPEND lt_ania.
  ENDLOOP.

ENDFORM.                    " APPEND_LT_ANIA
*&---------------------------------------------------------------------*
*&      Form  APPEND_LT_ANIB1
*&---------------------------------------------------------------------*
FORM append_lt_anib1 USING p_objnr TYPE j_objnr.
  MODIFY lt_anib TRANSPORTING lfdnr kz WHERE objnr = p_objnr.

  LOOP AT lt_anib.
    MOVE-CORRESPONDING lt_anib TO lt_anib1.
    APPEND lt_anib1.
    CLEAR lt_anib1.
  ENDLOOP.

ENDFORM.                    " APPEND_LT_ANIB1
*&---------------------------------------------------------------------*
*&      Form  set_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_output.
  CHECK g_error IS INITIAL.

  PERFORM show_progress     USING 'Preparing screen...' '95'.
  PERFORM init_alv_parm.
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM sort_build        USING gt_sort[].
  PERFORM alv_events_get    USING:  'P', 'T'.
  PERFORM alv_grid_display  TABLES  gt_out USING ''.

ENDFORM.                    " set_output

*---------------------------------------------------------------------*
*       FORM show_progress                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PF_TEXT                                                       *
*  -->  VALUE(PF_VAL)                                                 *
*---------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  init_alv_parm
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

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

ENDFORM.                    " INIT_ALV_PARM
*&---------------------------------------------------------------------*
*&      Form  fieldcat_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv .

  DATA: l_pos TYPE i.

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
    gs_fieldcat-no_zero       = &8.
    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.

  __catalog :
    'X'  'POSNR'    'Int.Number'        12  'CHAR' '' '' '',
    'X'  'TXT50'    'Description'       50  'CHAR' '' '' '',
*    'X'  'OBJNR'    'Obj.Number'        22  'CHAR' '' '' '',
    ' '  'AFABE'    'Depr.Area'          8  'NUMC' '' '' '',
    ' '  'VARNT'    'Varint'            10  'CHAR' '' '' '',
    ' '  'INVESTMENT_COSTS' 'Total $'  18  'DEC' '' '' '',
    ' '  'INVESTMENT_YEAR' 'Yearly $'  18  'DEC' '' '' '',
    ' '  'AFASL'    'Dep. key'          10  'CHAR' '' '' '',
    ' '  'NDJAR'    'Usefl.life'        10  'CHAR' '' '' '',
    ' '  'ANLKL'    'Asset Class'       10  'CHAR' '' '' '',
    ' '  'AKTIV'    'Cap. date'          8  'DATS' '' '' '',
    ' '  'KOSTL'    'Cost ctr'          10  'CHAR' '' '' '',
    ' '  'CAUFN'    'Order'             10  'CHAR' '' '' '',
    ' '  'GPLAB'    'Pl.ret.dat'         8  'DATS' '' '' '',
    ' '  'ICON'     'err'                4  'ICON' '' '' '',
    ' '  'MSG'      'Remarks'           40  'CHAR' '' '' ''.

  LOOP AT gt_fieldcat INTO gs_fieldcat.
    IF gs_fieldcat-fieldname EQ 'INVESTMENT_COSTS'
      OR gs_fieldcat-fieldname EQ 'INVESTMENT_YEAR'.
      gs_fieldcat-decimals_out  = '2'.
    ENDIF.
    MODIFY gt_fieldcat FROM gs_fieldcat.
  ENDLOOP.

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  sort_build
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
     'POSNR'        ' ' 'X' 'X' 'X'.
*     'OBJNR'        ' ' 'X' 'X' 'X'.
ENDFORM.                    " SORT_BUILD


*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FP_UCOMM                                                      *
*  -->  FS                                                            *
*---------------------------------------------------------------------*
FORM user_command USING fp_ucomm LIKE sy-ucomm
                        fs       TYPE slis_selfield.
  CLEAR : g_error.

  CASE fp_ucomm.
    WHEN 'PROC1'.
      CHECK g_error NE true.
      PERFORM data_confirm.
      CHECK g_error NE true.
      PERFORM proc USING false.

      fs-refresh    = 'X'.
      fs-row_stable = 'X'.
      fs-col_stable = 'X'.

    WHEN 'PROC2'.
      CHECK g_error NE true.
      PERFORM data_confirm.
      CHECK g_error NE true.
      PERFORM proc USING true.

      fs-refresh    = 'X'.
      fs-row_stable = 'X'.
      fs-col_stable = 'X'.

  ENDCASE.

ENDFORM.                    "USER_COMMAND

*---------------------------------------------------------------------*
*       FORM pf_status_set                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FT_EXTAB                                                      *
*---------------------------------------------------------------------*
FORM pf_status_set USING  ft_extab TYPE slis_t_extab.
  SET PF-STATUS '100'.
ENDFORM.                    "PF_STATUS_SET

*---------------------------------------------------------------------*
*       FORM top_of_page                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
  DATA l_text(60).
  REFRESH gt_listheader.

  l_text = 'Create Depreciation Simulation Data to Appr.Req Variants'.
  PERFORM set_header_line USING:
          'P' 'H' ''      l_text       '',
          'P' 'S' 'IM Approval year'   p_gjahr '',
          'P' 'S' 'IM Version'   p_versi ''.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = gt_listheader.

ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  proc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM proc USING p_reset.
  DATA $ix TYPE i.
  LOOP AT i_imak.

    READ TABLE gt_out WITH KEY posnr = i_imak-posnr BINARY SEARCH.
    IF sy-subrc = 0.
      $ix = sy-tabix.
    ELSE.
      CONTINUE.
    ENDIF.
    CHECK gt_out-err IS INITIAL.

    IF p_reset = 'X'.
      PERFORM reset_depr USING gt_out-objnr.

      CALL FUNCTION 'DEPREC_SIMUL_MODIFIKATION'
           TABLES
                t_ania = lt_ania
                t_anib = lt_anib.

    ELSE.
      PERFORM simulate_depr.

      CALL FUNCTION 'DEPREC_SIMUL_MODIFIKATION'
           TABLES
                t_ania = lt_ania
                t_anib = lt_anib1.

    ENDIF.
    IF sy-subrc EQ 0.
      CONCATENATE i_imak-posnr ' is updated!' INTO gt_out-msg.
      MODIFY gt_out INDEX $ix.
    ENDIF.
  ENDLOOP.



ENDFORM.                    " proc
*&---------------------------------------------------------------------*
*&      Form  data_confirm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_confirm.

  DATA l_answer(1).

  PERFORM pop_up USING
      'Simulation Data will be created'
      'Do you want to proceed it?' ' '
                 CHANGING l_answer.

  IF l_answer NE 'J'.
    g_error = true.
    MESSAGE s000 WITH 'Processing was canceled by user.'.
  ENDIF.

ENDFORM.                    " data_confirm

*---------------------------------------------------------------------*
*       FORM pop_up                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_TEXT                                                        *
*  -->  P_TEXT2                                                       *
*  -->  P_CANC                                                        *
*  -->  P_ANSWER                                                      *
*---------------------------------------------------------------------*
FORM pop_up USING    p_text p_text2 p_canc
            CHANGING p_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1      = p_text
            textline2      = p_text2
            titel          = 'Check!'
            cancel_display = p_canc
       IMPORTING
            answer         = p_answer.


ENDFORM.                    " POP_UP
