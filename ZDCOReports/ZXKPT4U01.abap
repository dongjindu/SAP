*----------------------------------------------------------------------*
*   INCLUDE ZXKPT4U01                                                  *
*----------------------------------------------------------------------*
*"       IMPORTING
*"             VALUE(IS_PARX) LIKE  RKPLV_PARX STRUCTURE  RKPLV_PARX
*"             VALUE(ID_CURSW_C) LIKE  RKPLV_FLGX-FLG_CURSW_C
*"       TABLES
*"              CT_RKPLV1 STRUCTURE  RKPLV1
*"              CT_RKU0RJA STRUCTURE  RKU0RJA OPTIONAL

************************************************************************
** Which transaction is calling up (KP97/KP98 only)                   **
** ------------------------                                           **
** Copy Planning  (kp97)  -> it_parx-flg_modus = 'P'
** Copy Act->Plan (kp98)  -> it_parx-flg_modus = 'A'
************************************************************************

*  CHECK Is_PARX-FLG_MODUS = 'A'.  "Execute module for actual->plan only
*

CHECK is_parx-tversn = '600'. "NAFTA version only

*+ by igmoon 4/4/2008 {
DATA: BEGIN OF g_t_rate_e OCCURS 0.
        INCLUDE STRUCTURE zco140_gen.
DATA: END OF g_t_rate_e.

LOOP AT ct_rkplv1.
  CLEAR : g_t_rate_e, g_t_rate_e[].

* only primary cost element... not working...
*  select count( * ) into sy-index from cskb
*      where KOKRS = is_parx-kokrs
*        and kstar = ct_rkplv1-kstar
*        and KATYP = '01'.    "Primary costs/cost-reducing revenues
*  if sy-subrc <> 0.
*    delete ct_rkplv1 index sy-tabix.
*    continue.
*  endif.

  CALL FUNCTION 'Z_CO_RATE_GENERIC_GET'
       EXPORTING
            i_kokrs = is_parx-kokrs
            i_kostl = ct_rkplv1-kostl
            i_kstar = ct_rkplv1-kstar
       TABLES
            t_rate  = g_t_rate_e.

  READ TABLE g_t_rate_e INDEX 1.
  IF sy-subrc EQ 0.
    g_t_rate_e-rate_e = 1 - ( g_t_rate_e-rate_e / 100  ).
    MULTIPLY :
* planning currency: co area currency
        ct_rkplv1-wtg001 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtg002 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtg003 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtg004 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtg005 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtg006 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtg007 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtg008 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtg009 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtg010 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtg011 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtg012 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtf001 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtf002 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtf003 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtf004 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtf005 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtf006 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtf007 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtf008 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtf009 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtf010 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtf011 BY g_t_rate_e-rate_e,
        ct_rkplv1-wtf012 BY g_t_rate_e-rate_e.

*        ct_rkplv1-wkg001 by g_t_rate_e-rate_e,
*        ct_rkplv1-wkf001 by g_t_rate_e-rate_e,
*        ct_rkplv1-wog001 by g_t_rate_e-rate_e,
*        ct_rkplv1-wof001 by g_t_rate_e-rate_e,


  ENDIF.

  ct_rkplv1-meg001 = 0.
  ct_rkplv1-meg002 = 0.
  ct_rkplv1-meg003 = 0.
  ct_rkplv1-meg004 = 0.
  ct_rkplv1-meg005 = 0.
  ct_rkplv1-meg006 = 0.
  ct_rkplv1-meg007 = 0.
  ct_rkplv1-meg008 = 0.
  ct_rkplv1-meg009 = 0.
  ct_rkplv1-meg010 = 0.
  ct_rkplv1-meg011 = 0.
  ct_rkplv1-meg012 = 0.

  MODIFY ct_rkplv1.

ENDLOOP.
* }

***  loop at ct_rkplv1 where kstar+4(6) = '836001'
***                       or kstar+4(6) = '836002'.
***
***
**** LSTAR = 'MAN_HR' or LSTAR = 'MCH_HR'.
***    ct_rkplv1-meg001 = 0.
***    ct_rkplv1-meg002 = 0.
***    ct_rkplv1-meg003 = 0.
***    ct_rkplv1-meg004 = 0.
***    ct_rkplv1-meg005 = 0.
***    ct_rkplv1-meg006 = 0.
***    ct_rkplv1-meg007 = 0.
***    ct_rkplv1-meg008 = 0.
***    ct_rkplv1-meg009 = 0.
***    ct_rkplv1-meg010 = 0.
***    ct_rkplv1-meg011 = 0.
***    ct_rkplv1-meg012 = 0.
***    modify ct_rkplv1.
***  endloop.
****
****
****  CHECK ( Is_PARX-VRGNG  = 'RKP2' ).
