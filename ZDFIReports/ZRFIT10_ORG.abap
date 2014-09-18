*&--------------------------------------------------------------------
*& Author                 : JIPARK
*& Creation Date          : 12/05/2003
*& Specification By       : JIPARK
*& Pattern                : Report 1-10
*& Development Request No : UD1K903862
*& Addl documentation     :
*& Description  : Cash Actuals Drill-Down
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------
REPORT  zrfit10  MESSAGE-ID zmfi NO STANDARD PAGE HEADING
                                 LINE-SIZE 136.
**********************************************************************
* Data Declaration
**********************************************************************
TABLES: ztfi_cmal, ztfi_pi, fmfpot.

DATA: BEGIN OF it_ddwn OCCURS 0,
        budat   LIKE sy-datum,          "(posting)date
        dispw   LIKE ztfi_cmal-dispw,   "currency
        wrshb   LIKE ztfi_cmal-wrshb,   "amt
        dmshb   LIKE ztfi_cmal-dmshb,   "local amt
        saknr   LIKE ztfi_cmal-saknr,   "bank acct
        grupp   LIKE ztfi_cmal-grupp,   "group
        grptt(30),                      "group desc.
        koart   LIKE ztfi_cmal-koart,   "account type
        kacct   LIKE ztfi_cmal-hkont,   "vnd/cust/account
        fipos   LIKE ztfi_fmal-fipos,   "cmltm
        rftext(25),                     "cmltm desc.
        fincode LIKE ztfi_fmal-fincode, "fund
        fistl   LIKE ztfi_fmal-fistl,   "fctr
        sbewart LIKE ztfi_cmal-sbewart, "flow type
        belnr   LIKE ztfi_cmal-belnr,   "document
        finacct(1)  TYPE c,             "fund acct
        findesc(40) TYPE c,             "fund desc.
        aufnr   like ztfi_fmal-aufnr,   "order
        bukrs   LIKE ztfi_cmal-bukrs,
        gjahr   LIKE ztfi_cmal-gjahr,
        knbelnr LIKE ztfi_cmal-belnr,   "invoice doc.
        kngjahr LIKE ztfi_cmal-gjahr,   "invoice year
      END OF it_ddwn.

DATA: sv_waers LIKE t001-waers.

* using ALV reporting..
* USING ALV REPORTING..
type-pools : slis.

include rvreuse_global_data.
include rvreuse_local_data.
include rvreuse_forms.

data : gs_layout    type slis_layout_alv,
       gt_fieldcat  type slis_t_fieldcat_alv,
       gt_field     type slis_t_fieldcat_alv,
       g_fieldcat_s type slis_fieldcat_alv,  " ?? ??? ??.
       gt_events    type slis_t_event,
       it_sort      type slis_t_sortinfo_alv,
       g_save(1)    type c,
       g_exit(1)    type c,
       gx_variant   like disvariant,
       g_variant    like disvariant,
       g_repid      like sy-repid,
       g_cnt(2)     type n,
       ls_sort      type slis_sortinfo_alv,
       gt_sort      type slis_t_sortinfo_alv.

constants:
  c_fnam_cos_pf_status
         type  slis_formname value 'ALV_SET_PF_STATUS',
  c_fnam_cos_user_command
         type slis_formname  value 'ALV_USER_COMMAND',
  c_f2code
         like sy-ucomm       value '&ETA'.

data: g_exit_caused_by_caller,
      gt_list_top_of_page type slis_t_listheader,
      g_user_command type slis_formname value 'USER_COMMAND',
      g_top_of_page  type slis_formname value 'TOP_OF_PAGE',
      g_status_set   type slis_formname value 'PF_STATUS_SET',
      gs_exit_caused_by_user type slis_exit_by_user,
      g_tabname type slis_tabname value 'ITAB',
      g_boxname type slis_fieldname value 'BOX'.


***********************************************************************
* START-OF-SELECTION
***********************************************************************
START-OF-SELECTION.
  IMPORT it_ddwn sv_waers FROM MEMORY ID 'Z10'.

* get commitment item & fund acct.
  LOOP AT it_ddwn.
    SELECT SINGLE beschr INTO it_ddwn-rftext
                  FROM fmfpot
                  WHERE fikrs EQ it_ddwn-bukrs
                  AND   fipos EQ it_ddwn-fipos.
    SELECT SINGLE code descr
                  INTO (it_ddwn-finacct, it_ddwn-findesc )
                  FROM ztfi_pi
                  WHERE type EQ '5'  "account
                  AND   code EQ it_ddwn-fincode+5(1).

    MODIFY it_ddwn. CLEAR it_ddwn.
  ENDLOOP.


* ALV DISPLAY
  PERFORM display_list.

************************************************************************
* AT USER-COMMAND                                                     *
************************************************************************
form alv_user_command using r_ucomm     like sy-ucomm
                            rs_selfield type slis_selfield.
  CASE r_ucomm.
    WHEN '&IC1' OR '&ETA'.  "PICK.."
      READ TABLE it_ddwn INDEX rs_selfield-tabindex.
      IF sy-subrc = 0.
        CASE rs_selfield-fieldname.
          WHEN 'KACCT'.
            SET PARAMETER ID 'BUK' FIELD it_ddwn-bukrs.
            IF it_ddwn-koart EQ 'D'.  "customer
              SET PARAMETER ID 'KUN' FIELD it_ddwn-kacct.
              CALL TRANSACTION 'FD03' AND SKIP FIRST SCREEN.
            ELSEIF it_ddwn-koart EQ 'K'.  "vendor
              SET PARAMETER ID 'LIF' FIELD it_ddwn-kacct.
              CALL TRANSACTION 'FK03' AND SKIP FIRST SCREEN.
            ELSE.  "g/l account
              SET PARAMETER ID 'SAK' FIELD it_ddwn-kacct.
              CALL TRANSACTION 'FS03' AND SKIP FIRST SCREEN.
            ENDIF.

          WHEN 'RFIPEX'.  "commitment item
            SET PARAMETER ID:'FIK' FIELD it_ddwn-bukrs,
                             'FIP' FIELD it_ddwn-fipos.
            CALL TRANSACTION 'FM3S' AND SKIP FIRST SCREEN.

          WHEN 'FINACCT'.
            SET PARAMETER ID:'FIK' FIELD it_ddwn-bukrs,
                             'FIC' FIELD it_ddwn-fincode.
            CALL TRANSACTION 'FM5S' AND SKIP FIRST SCREEN.

          WHEN 'FISTL'.

          WHEN 'BELNR'.
            SET PARAMETER ID:'BLN' FIELD it_ddwn-belnr,
                             'BUK' FIELD it_ddwn-bukrs,
                             'GJR' FIELD it_ddwn-gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

          WHEN 'KNBELNR'.
            SET PARAMETER ID:'BLN' FIELD it_ddwn-knbelnr,
                             'BUK' FIELD it_ddwn-bukrs,
*Issue Number : FI-20041111-009, Requested by GHLEE
*Changed on 2004/12/01,changed by WSKIM
*---Start
*                             'GJR' FIELD it_ddwn-gjahr.
                              'GJR' FIELD it_ddwn-kngjahr.
*---End

            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

        ENDCASE.
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  display_list
*&---------------------------------------------------------------------*
FORM display_list.
  data: l_cu like t001-waers.
* FIELD SETTING
  l_cu = 'USD'.

  perform field_setting tables gt_fieldcat using :
   'BUDAT'    'Date'          '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'DISPW'    'CURR'          '04' ' ' 'L'  ' '  '  ' ' ' ' ',
   'WRSHB'    'Amount'        '16' ' ' 'R'  ' '  '  ' ' ' 'X',
   'DMSHB'    'Lc.Amt'        '16' ' ' 'R'  ' '  '  ' ' ' 'X',
   'SAKNR'    'BANK GL'       '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'GRUPP'    'GROUP'         '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'GRPTT'    'GROUP DESC'    '15' ' ' 'L'  ' '  '  ' ' ' ' ',
   'KOART'    'AT'            '01' ' ' 'L'  ' '  '  ' ' ' ' ',
   'KACCT'    'VND/CUST'      '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'FIPOS'    'CMLTM'         '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'RFTEXT'   'CMLTM DESC'    '20' ' ' 'L'  ' '  '  ' ' ' ' ',
   'FINCODE'  'FUND'          '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'FISTL'    'FundCtr'       '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'SBEWART'  'FLOW'          '05' ' ' 'L'  ' '  '  ' ' ' ' ',
   'BELNR'    'PayDoc#'       '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'FINACCT'  'FC'            '01' ' ' 'L'  ' '  '  ' ' ' ' ',
   'FINDESC'  'FUND DESC.'    '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'AUFNR'    'ORDER'         '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'BUKRS'    'PayYr'         '04' ' ' 'L'  ' '  '  ' ' ' ' ',
   'KNBELNR'  'INVOICE'       '10' ' ' 'L'  ' '  '  ' ' ' ' ',
   'KNGJAHR'  'InvYr'         '04' ' ' 'L'  ' '  '  ' ' ' ' '.


* build event table.
  perform alv_build_eventtab changing gt_events.
  g_repid = sy-repid.

* PERFORM display_alv.
  call function 'REUSE_ALV_GRID_DISPLAY'
       exporting
            i_callback_program       = g_repid
            i_callback_user_command  = c_fnam_cos_user_command
            i_callback_pf_status_set = c_fnam_cos_pf_status
            it_events                = gt_events
            is_layout                = gs_layout
            is_variant               = g_variant
            it_fieldcat              = gt_fieldcat[]
            i_save                   = 'A'
       tables
            t_outtab                 = it_ddwn
       exceptions
            program_error            = 1
            others                   = 2.


ENDFORM.                    " display_list
*&-------------------------------------------------------------------
*&      Form  field_setting
*&-------------------------------------------------------------------
form field_setting tables p_fieldcat_t like gt_fieldcat using
                                  p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
*                                  p_edit            "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum
                                  .

  data: ls_fieldcat type slis_fieldcat_alv.
  clear ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.

  ls_fieldcat-do_sum     = p_dosum.

  append ls_fieldcat to gt_fieldcat.
endform.                    " field_setting
*&---------------------------------------------------------------------*
*&      Form  ALV_BUILD_EVENTTAB
*&---------------------------------------------------------------------*
form alv_build_eventtab changing et_alv_events type slis_t_event.

  data: ls_alv_events    type slis_alv_event,
        l_dummy_ucomm    like sy-ucomm,
        l_dummy_selfield type slis_selfield,
        l_dummy_excl_tab type slis_t_extab.


  refresh: et_alv_events.

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
  clear ls_alv_events.
  ls_alv_events-name = slis_ev_pf_status_set.
  ls_alv_events-form = 'ALV_SET_PF_STATUS'.
  append ls_alv_events to et_alv_events.

* event 'USER_COMMAND'.
  clear ls_alv_events.
  ls_alv_events-name = slis_ev_user_command.
  ls_alv_events-form = 'ALV_USER_COMMAND'.
  append ls_alv_events to et_alv_events.

* callback forms.
  if 1 = 0.
*    PERFORM alv_top_of_list.
*    PERFORM alv_end_of_list.
    perform alv_set_pf_status using l_dummy_excl_tab.
    perform alv_user_command  using l_dummy_ucomm
                                    l_dummy_selfield.

  endif.
endform.                               " ALV_BUILD_EVENTTAB
*---------------------------------------------------------------------*
*       FORM ALV_SET_PF_STATUS                                        *
*---------------------------------------------------------------------*
*       for ALV processing                                            *
*---------------------------------------------------------------------*
form alv_set_pf_status using excl_tab type slis_t_extab.

  data: l_excl_tab type slis_extab.

  set pf-status 'STANDARD' excluding excl_tab.

endform.                               " alv_set_pf_status
