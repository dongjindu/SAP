*&---------------------------------------------------------------------*
*&  Include           ZSAPBF_CPZP_CORRECTION_REPSEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK general WITH FRAME TITLE text-041.

PARAMETERS: p_year TYPE gjahr OBLIGATORY DEFAULT sy-datlo(4)," NO-DISPLAY DEFAULT sy-datlo(4),
            p_month TYPE monat OBLIGATORY DEFAULT sy-datlo+4(2).

PARAMETERS: p_aufnr TYPE aufnr OBLIGATORY MATCHCODE OBJECT orde MODIF ID 10.
SELECTION-SCREEN END OF BLOCK general.

SELECTION-SCREEN BEGIN OF BLOCK processing WITH FRAME TITLE text-037.
PARAMETERS: p_test AS CHECKBOX TYPE flag DEFAULT 'X'.
PARAMETERS: p_error AS CHECKBOX TYPE zsapbf_cpzp_error DEFAULT ' '.
PARAMETERS: p_updcur TYPE flag NO-DISPLAY .
SELECTION-SCREEN END OF BLOCK processing.

**** Start; Added by James Sung-Kon Kim 2011/03/02
SELECTION-SCREEN BEGIN OF BLOCK parallel WITH FRAME TITLE text-003.
PARAMETERS: p_par  TYPE ppc_para DEFAULT '' AS CHECKBOX USER-COMMAND u2 MODIF ID 50.
PARAMETERS: p_sgr  TYPE wsl_ctrl-serv_grp  MODIF ID 40 .
PARAMETERS: p_wps     TYPE zsapbf_ppc_wps_req DEFAULT 1 MODIF ID 50.
PARAMETERS: p_wpssub  TYPE zsapbf_ppc_wps_req DEFAULT 1 MODIF ID 40.
PARAMETERS: p_wpstot  TYPE zsapbf_ppc_wps_req DEFAULT 1 MODIF ID 50.

PARAMETERS: p_del_hl TYPE ppc_mwdel_th DEFAULT 'X'  MODIF ID 50.
PARAMETERS: p_wttime TYPE ppc_mwwaittime DEFAULT 20  MODIF ID 50.
PARAMETERS: p_retryc TYPE ppc_retry_com DEFAULT 3  MODIF ID 50.
PARAMETERS: p_retrys TYPE ppc_retry_sys DEFAULT 3  MODIF ID 50.
PARAMETERS: p_retryr TYPE ppc_retry_res DEFAULT 3  MODIF ID 50.

SELECTION-SCREEN END OF BLOCK parallel.
**** End; Added by James Sung-Kon Kim 2011/03/02
