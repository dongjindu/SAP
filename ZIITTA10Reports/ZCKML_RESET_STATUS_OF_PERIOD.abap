REPORT  zckml_reset_status_of_period.
*&---------------------------------------------------------------------*
*& Report  zckml_reset_status_of_period
*&---------------------------------------------------------------------*
*& Resets the ML period status to 'period opened' if the circumstances
*& allow that. Additional SAP-Program from note 574930                 *
*&---------------------------------------------------------------------*

*&--------------------------------------------------------------------&*
*&   Program: ZCKML_RESET_STATUS_OF_PERIOD.                           &*
*&   Author: Shiva.                                                   &*
*&   Specification: Apply OSS Note 574930.                            &*
*&                                                                    &*
*&--------------------------------------------------------------------&*
*& Date         User   Transport   Description
*& 09/10/2004   Shiva  UD1K912190  Ossnote: 574930 & commented[turnoff]
*&                          selection parameters that HMMA doesn't need.
*&--------------------------------------------------------------------&*

INCLUDE lckm0top_status.

TABLES:
        ckmlhd,
        ckmlpp,
        marv,
        mara,
        sscrfields.

TYPE-POOLS:
        slis, ckmv0.

TYPES:                      " allow input range bwkey
      BEGIN OF ty_bwkey,
         bwkey LIKE ckmlhd-bwkey,
      END OF ty_bwkey.

DATA  : prev_status LIKE ckmlpp-status.

DATA  : t_bwkey             TYPE ty_bwkey OCCURS 0 WITH HEADER LINE,
        t_kalnr_all         TYPE ckmv0_matobj_tbl WITH HEADER LINE,
        f_ckmlpp type ckmlpp,
        f_ckmlcr type ckmlcr.

* Fieldcatalogs
DATA:
        gd_fieldcat    TYPE slis_t_fieldcat_alv,

* ALV
        gd_program          LIKE sy-repid,
        gd_tabname          TYPE slis_tabname,
        gd_top_of_list      TYPE slis_t_listheader.

DATA:
      BEGIN OF gd_tbl OCCURS 0,
        kalnr        LIKE ckmlhd-kalnr,    "Kalnr des Material-Objekts
        bwkey        LIKE ckmlhd-bwkey,    "Bewertungskreis
        matnr        LIKE ckmlhd-matnr,    "Materialnummer
        bwtar        LIKE ckmlhd-bwtar,    "Bewertungsart
        vbeln        LIKE ckmlhd-vbeln,    "Kundenauftragsnummer
        posnr        LIKE ckmlhd-posnr,    "Kundenauftragsposition
        pspnr        LIKE ckmlhd-pspnr,    "PSP-Element
        flg_lock     LIKE boole-boole,     "Kz: Objekt ist gesperrt
        mlast        like ckmlhd-mlast,
        prev_status  LIKE ckmlpp-status,   "Previous status of CKMLPP
        status       LIKE ckmlpp-status,   "Updated or current status
        changed      LIKE boole-boole,     "Set if record is changed
      END OF gd_tbl.

data: lf_tbl like line of gd_tbl.
data: h_change_allowed type boole.
DATA: BWTAR_EXIST TYPE XFELD,
      VBELN_EXIST TYPE XFELD,
      PSPNR_EXIST TYPE XFELD.

SELECT-OPTIONS: r_matnr FOR  ckmlhd-matnr  MEMORY ID mat.
SELECT-OPTIONS: r_bwkey FOR  ckmlhd-bwkey  OBLIGATORY MEMORY ID bwk.
SELECT-OPTIONS: r_bwtar FOR  ckmlhd-bwtar  MEMORY ID bwt.
PARAMETERS:     P_MBEW  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN SKIP.
*PARAMETERS:     P_EBEW AS CHECKBOX DEFAULT 'X'.
*SELECT-OPTIONS: R_VBELN FOR  CKMLHD-VBELN.
*SELECT-OPTIONS: R_POSNR FOR  CKMLHD-POSNR.
*SELECTION-SCREEN SKIP.
*PARAMETERS:     P_QBEW AS CHECKBOX DEFAULT 'X'.
*SELECT-OPTIONS: R_PSPNR FOR  CKMLHD-PSPNR.
PARAMETERS:     P_EBEW no-display.
SELECT-OPTIONS: R_VBELN FOR  CKMLHD-VBELN no-display.
SELECT-OPTIONS: R_POSNR FOR  CKMLHD-POSNR no-display.
PARAMETERS:     P_QBEW no-display.
SELECT-OPTIONS: R_PSPNR FOR  CKMLHD-PSPNR no-display.

PARAMETERS:     p_bdatj LIKE ckmlpp-bdatj  OBLIGATORY MEMORY ID bdtj,
                P_POPER LIKE MARV-LFMON  OBLIGATORY MEMORY ID POPR.
SELECTION-SCREEN SKIP.
*SELECT-OPTIONS: r_mtart FOR  mara-mtart    MEMORY ID mta MODIF ID puk,
*                r_matkl FOR  mara-matkl    MEMORY ID mtl MODIF ID puk,
*                r_spart FOR  mara-spart    MEMORY ID spa MODIF ID puk.
*SELECTION-SCREEN SKIP.
SELECT-OPTIONS: r_mtart FOR  mara-mtart no-display
                                        MEMORY ID mta MODIF ID puk,
                r_matkl FOR  mara-matkl no-display
                                        MEMORY ID mtl MODIF ID puk,
                r_spart FOR  mara-spart no-display
                                        MEMORY ID spa MODIF ID puk.

SELECTION-SCREEN BEGIN OF BLOCK processing WITH FRAME TITLE text-001.
PARAMETERS:     p_test LIKE cki_doc_ml-test DEFAULT 'X'.
SELECTION-SCREEN END   OF BLOCK processing.

DATA: PREV_POPER LIKE CKMLPP-POPER,
      PREV_BDATJ LIKE CKMLPP-BDATJ.
DATA PREV_PP_STATUS LIKE CKMLPP-STATUS.

********************
AT SELECTION-SCREEN.
********************

  DATA: lf_t001k LIKE t001k.

  DATA:
     ft_t001k LIKE t001k OCCURS 0 WITH HEADER LINE.
  CLEAR:
     ft_t001k.

  REFRESH:
     ft_t001k,
     t_bwkey.

* Use range so_bwkey to fill internal table ft_t001k with a list of
* BWKEY BUKRS MLBWA
  SELECT * FROM t001k
           INTO TABLE ft_t001k
           WHERE BWKEY IN R_BWKEY
           AND   MLBWA = 'X'.

  IF sy-subrc <> 0.
*     No BWKEY found for this input (range)
    MESSAGE s101(C+)
    WITH 'No valuation area found for this input (range)'.
    EXIT.
  ENDIF.

* Fill internal table lt_bwkey with bwkey from ft_t001k
  LOOP AT ft_t001k.
    t_bwkey-bwkey = ft_t001k-bwkey.

    CALL FUNCTION 'MARV_SINGLE_READ'
         EXPORTING
              bukrs = ft_t001k-bukrs
         IMPORTING
              wmarv = marv.

    IF P_BDATJ NE MARV-LFGJA OR P_POPER NE MARV-LFMON.
      MESSAGE W101(C+) WITH
               MARV-LFMON
               MARV-LFGJA
               'is the current period in valuation area'
               FT_T001K-BWKEY.
      CONTINUE.
    ENDIF.

    APPEND t_bwkey.
  ENDLOOP.

*******************
START-OF-SELECTION.
*******************
* determine period and year of previous period.
  IF P_POPER GT 1  AND
     P_POPER LE 12.
    PREV_POPER = P_POPER - 1.
    PREV_BDATJ = P_BDATJ.
  ELSE.
    PREV_POPER = '12'.
    PREV_BDATJ = P_BDATJ - 1.
  ENDIF.

  REFRESH: gd_tbl.

  LOOP AT t_bwkey.
    REFRESH: t_kalnr_all.

    PERFORM XBEW_SELECTION  TABLES R_MATNR
                                   R_BWTAR
                                   R_VBELN
                                   R_POSNR
                                   R_PSPNR
                                   R_MTART
                                   R_MATKL
                                   R_SPART
                                   T_KALNR_ALL
                            USING T_BWKEY-BWKEY.

    LOOP AT t_kalnr_all.
      clear h_change_allowed.

*       write into output table
      clear lf_tbl.
      MOVE-CORRESPONDING t_kalnr_all TO lf_tbl.

**      Read ML data and check if status reset is allowed
      SELECT SINGLE MLAST FROM CKMLHD INTO LF_TBL-MLAST
      WHERE KALNR  = T_KALNR_ALL-KALNR.

      IF SY-SUBRC = 0.
        IF LF_TBL-MLAST = '3'.
*      Read ML data and check if status reset is allowed
          SELECT single * FROM ckmlpp into f_ckmlpp
          WHERE kalnr  = t_kalnr_all-kalnr
          AND   poper  = p_poper
          AND   bdatj  = p_bdatj
          AND   untper = '000'.

          IF sy-subrc ne 0.
*       period not yet shifted...
            lf_tbl-prev_status = y_periode_eroeffnet.
            lf_tbl-status = y_periode_eroeffnet.
          else.
            MOVE-CORRESPONDING f_ckmlpp TO lf_tbl.
            lf_tbl-prev_status = f_ckmlpp-status.
            clear lf_tbl-changed.
            if f_ckmlpp-status ne y_neu_angelegt
            and f_ckmlpp-status ne y_periode_eroeffnet.

*       check that PP-data allow resetting status
              if f_ckmlpp-status = y_abschlussbuchung_erfolgt
              or f_ckmlpp-zukumo ne 0
              or f_ckmlpp-umkumo ne 0
              or f_ckmlpp-szkumo ne 0
              or f_ckmlpp-vpkumo ne 0
              or f_ckmlpp-vnkumo ne 0.
                clear h_change_allowed.
              else.
*         check that no collected values in any currencies exist
                select * from ckmlcr into f_ckmlcr
                          WHERE kalnr  = f_ckmlpp-kalnr
                          AND   poper  = f_ckmlpp-poper
                          AND   bdatj  = f_ckmlpp-bdatj
                          AND   untper = f_ckmlpp-untper.
                  if f_ckmlcr-ZUUMB_O ne 0
                  or f_ckmlcr-abprd_o ne 0
                  or f_ckmlcr-abkdm_o ne 0
                  or f_ckmlcr-zuprd_o ne 0
                  or f_ckmlcr-zukdm_o ne 0
                  or f_ckmlcr-vpprd_o ne 0
                  or f_ckmlcr-vpkdm_o ne 0
                  or f_ckmlcr-vnprd_o ne 0
                  or f_ckmlcr-vnkdm_o ne 0.
                    clear h_change_allowed.
                    exit.
                  else.
*       now check that previous period is not already posted
*      would lead to problems with zuumb generated by price change
                   SELECT SINGLE STATUS FROM CKMLPP INTO PREV_PP_STATUS
                       WHERE KALNR  = T_KALNR_ALL-KALNR
                       AND   POPER  = PREV_POPER
                       AND   BDATJ  = PREV_BDATJ
                       AND   UNTPER = '000'.
                    IF SY-SUBRC = 0 AND
                    PREV_PP_STATUS EQ Y_ABSCHLUSSBUCHUNG_ERFOLGT.
                      CLEAR H_CHANGE_ALLOWED.
                    ELSE.
                      h_change_allowed = 'X'.
*         show new status on listing
                      lf_tbl-status = y_periode_eroeffnet.
                    ENDIF.
                  endif.
                endselect.
              endif.
              if h_change_allowed = 'X'.
*         changes are allowed, reset the status now
                prev_status = f_ckmlpp-status.
                IF p_test IS INITIAL.
*              try to block meaterial
                  CALL FUNCTION 'ENQUEUE_EMMBEWE'
                       EXPORTING
                            MATNR  = t_kalnr_all-MATNR
                            BWKEY  = t_kalnr_all-BWKEY
                            BWTAR  = t_kalnr_all-BWTAR
                       EXCEPTIONS
                            OTHERS = 3.
                  if sy-subrc = 0.
                    UPDATE ckmlpp
                    SET status = y_periode_eroeffnet
                             WHERE kalnr  = f_ckmlpp-kalnr
                             AND   poper  = f_ckmlpp-poper
                             AND   bdatj  = f_ckmlpp-bdatj
                             AND   untper = f_ckmlpp-untper.
                    COMMIT WORK.
                    lf_tbl-status = y_periode_eroeffnet.
                  else.
                    lf_tbl-status = lf_tbl-prev_status.
                    clear h_change_allowed.
                    lf_tbl-flg_lock = 'X'.
                  ENDIF.
                endif.
              else.
                lf_tbl-status = lf_tbl-prev_status.
              endif.
              lf_tbl-changed = h_change_allowed.
            ENDIF.
          endif.
        ENDIF.
      ELSE.        "keine bew. Material z.B. UNBW / NLAG
        CONTINUE.   "übergehen
      ENDIF.
      append lf_tbl to gd_tbl.
    ENDLOOP.
  ENDLOOP.

  PERFORM display_alv.

*----------------------------------------------------------------------*
*       FORM DISPLAY_ALV
*----------------------------------------------------------------------*
*       Display of all events of one protocoll
*----------------------------------------------------------------------*
FORM display_alv.

  PERFORM alv_ge_top_of_page.
  PERFORM alv_ge_fieldcat.
  PERFORM alv_display.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  alv_ge_top_of_page
*&---------------------------------------------------------------------*
FORM alv_ge_top_of_page.
  DATA: wa_line        TYPE slis_listheader.

  CLEAR wa_line.
  REFRESH gd_top_of_list.

* Test Run
  IF p_test IS INITIAL.
    wa_line-typ  = 'H'.
    wa_line-key  = 'Period status overview'.
    wa_line-info = 'Period status overview'.
    APPEND wa_line TO gd_top_of_list.
  ELSE.
    wa_line-typ  = 'H'.
    wa_line-key  = 'Period status overview  (Test Run)'.
    wa_line-info = 'Period status overview  (Test Run)'.
    APPEND wa_line TO gd_top_of_list.
  ENDIF.

* Jahr
  wa_line-typ  = 'S'.
  wa_line-key  = 'Year'.
  wa_line-info = p_bdatj.
  APPEND wa_line TO gd_top_of_list.

* Period
  wa_line-typ  = 'S'.
  wa_line-key  = 'Posting Period'.
  wa_line-info = p_poper.
  APPEND wa_line TO gd_top_of_list.

ENDFORM.                    " alv_ge_top_of_page

*----------------------------------------------------------------------*
*       FORM ALV_GE_EVENT_FIELDCAT
*----------------------------------------------------------------------*
FORM alv_ge_fieldcat.
  DATA:
   ld_wa_fieldcat       LIKE LINE OF gd_fieldcat,
   COLPOS               LIKE LD_WA_FIELDCAT-COL_POS,
   ld_tabix             LIKE sy-tabix.

* Initialization of the fieldcat
  gd_program = sy-repid.
  gd_tabname = 'GD_TBL'.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = gd_program
            i_internal_tabname = gd_tabname
            i_inclname         = gd_program
            i_bypassing_buffer = 'X'
       CHANGING
            ct_fieldcat        = gd_fieldcat.

* Modification of the fieldcat
  CLEAR: COLPOS.
  LOOP AT gd_fieldcat INTO ld_wa_fieldcat.

    ld_tabix = sy-tabix.
    CASE ld_wa_fieldcat-fieldname.
      WHEN 'BWKEY'.
        COLPOS = COLPOS + 1.
        LD_WA_FIELDCAT-NO_OUT = SPACE.    "ausgeben
        LD_WA_FIELDCAT-COL_POS = COLPOS.
        LD_WA_FIELDCAT-KEY     = 'X'.     "Schluesselspalte
        LD_WA_FIELDCAT-KEY_SEL = 'X'.     "Spalte ausblendbar
        LD_WA_FIELDCAT-FIX_COLUMN = 'X'.  "Spalte fixieren
        MODIFY GD_FIELDCAT FROM LD_WA_FIELDCAT.
      WHEN 'MATNR'.
        COLPOS = COLPOS + 1.
        LD_WA_FIELDCAT-NO_OUT = SPACE.    "ausgeben
        LD_WA_FIELDCAT-COL_POS = COLPOS.
        LD_WA_FIELDCAT-KEY     = 'X'.     "Schluesselspalte
        LD_WA_FIELDCAT-KEY_SEL = 'X'.     "Spalte ausblendbar
        LD_WA_FIELDCAT-FIX_COLUMN = 'X'.  "Spalte fixieren
        MODIFY GD_FIELDCAT FROM LD_WA_FIELDCAT.
      WHEN 'BWTAR'.
        IF BWTAR_EXIST = 'X'.
          COLPOS = COLPOS + 1.
          LD_WA_FIELDCAT-NO_OUT = SPACE.    "ausgeben
          LD_WA_FIELDCAT-COL_POS = COLPOS.
          LD_WA_FIELDCAT-KEY     = 'X'.     "Schluesselspalte
          LD_WA_FIELDCAT-KEY_SEL = 'X'.     "Spalte ausblendbar
          LD_WA_FIELDCAT-FIX_COLUMN = 'X'.  "Spalte fixieren
          MODIFY GD_FIELDCAT FROM LD_WA_FIELDCAT.
        ELSE.
          LD_WA_FIELDCAT-NO_OUT = 'X'.
          LD_WA_FIELDCAT-KEY    = SPACE.
          MODIFY GD_FIELDCAT FROM LD_WA_FIELDCAT
               TRANSPORTING NO_OUT
                            KEY.
        ENDIF.
      WHEN 'VBELN'.
        IF VBELN_EXIST = 'X'.
          COLPOS = COLPOS + 1.
          LD_WA_FIELDCAT-NO_OUT = SPACE.    "ausgeben
          LD_WA_FIELDCAT-COL_POS = COLPOS.
          LD_WA_FIELDCAT-KEY     = 'X'.     "Schluesselspalte
          LD_WA_FIELDCAT-KEY_SEL = 'X'.     "Spalte ausblendbar
          LD_WA_FIELDCAT-FIX_COLUMN = 'X'.  "Spalte fixieren
          MODIFY GD_FIELDCAT FROM LD_WA_FIELDCAT.
        ELSE.
          LD_WA_FIELDCAT-NO_OUT = 'X'.
          LD_WA_FIELDCAT-KEY    = SPACE.
          MODIFY GD_FIELDCAT FROM LD_WA_FIELDCAT
               TRANSPORTING NO_OUT
                            KEY.
        ENDIF.
      WHEN 'POSNR'.
        IF VBELN_EXIST = 'X'.
          COLPOS = COLPOS + 1.
          LD_WA_FIELDCAT-NO_OUT = SPACE.    "ausgeben
          LD_WA_FIELDCAT-COL_POS = COLPOS.
          LD_WA_FIELDCAT-KEY     = 'X'.     "Schluesselspalte
          LD_WA_FIELDCAT-KEY_SEL = 'X'.     "Spalte ausblendbar
          LD_WA_FIELDCAT-FIX_COLUMN = 'X'.  "Spalte fixieren
          MODIFY GD_FIELDCAT FROM LD_WA_FIELDCAT.
        ELSE.
          LD_WA_FIELDCAT-NO_OUT = 'X'.
          LD_WA_FIELDCAT-KEY    = SPACE.
          MODIFY GD_FIELDCAT FROM LD_WA_FIELDCAT
               TRANSPORTING NO_OUT
                            KEY.
        ENDIF.
      WHEN 'PSPNR'.
        IF PSPNR_EXIST = 'X'.
          COLPOS = COLPOS + 1.
          LD_WA_FIELDCAT-NO_OUT = SPACE.    "ausgeben
          LD_WA_FIELDCAT-COL_POS = COLPOS.
          LD_WA_FIELDCAT-KEY     = 'X'.     "Schluesselspalte
          LD_WA_FIELDCAT-KEY_SEL = 'X'.     "Spalte ausblendbar
          LD_WA_FIELDCAT-FIX_COLUMN = 'X'.  "Spalte fixieren
          MODIFY GD_FIELDCAT FROM LD_WA_FIELDCAT.
        ELSE.
          LD_WA_FIELDCAT-NO_OUT = 'X'.
          LD_WA_FIELDCAT-KEY    = SPACE.
          MODIFY GD_FIELDCAT FROM LD_WA_FIELDCAT
               TRANSPORTING NO_OUT
                            KEY.
        ENDIF.
      WHEN 'MLAST'.
        COLPOS = COLPOS + 1.
        LD_WA_FIELDCAT-NO_OUT = SPACE.    "ausgeben
        LD_WA_FIELDCAT-COL_POS = COLPOS.
        MODIFY GD_FIELDCAT FROM LD_WA_FIELDCAT.
      WHEN 'FLG_LOCK'.
        COLPOS = COLPOS + 1.
        LD_WA_FIELDCAT-NO_OUT = SPACE.    "ausgeben
        LD_WA_FIELDCAT-COL_POS = COLPOS.
        LD_WA_FIELDCAT-SELTEXT_S = 'Lock Flag'.
        LD_WA_FIELDCAT-SELTEXT_M = 'Lock Flag'.
        LD_WA_FIELDCAT-SELTEXT_L = 'Lock Flag'.
        LD_WA_FIELDCAT-OUTPUTLEN = 9.
        MODIFY GD_FIELDCAT FROM LD_WA_FIELDCAT.
      WHEN 'PREV_STATUS'.
        COLPOS = COLPOS + 1.
        LD_WA_FIELDCAT-NO_OUT = SPACE.    "ausgeben
        LD_WA_FIELDCAT-COL_POS = COLPOS.
        LD_WA_FIELDCAT-SELTEXT_S = 'Prev. Status'.
        LD_WA_FIELDCAT-SELTEXT_M = 'Previous Status'.
        LD_WA_FIELDCAT-SELTEXT_L = 'Previous Status'.
        LD_WA_FIELDCAT-OUTPUTLEN = 15.
        MODIFY GD_FIELDCAT FROM LD_WA_FIELDCAT.
      WHEN 'STATUS'.
        COLPOS = COLPOS + 1.
        LD_WA_FIELDCAT-NO_OUT = SPACE.    "ausgeben
        LD_WA_FIELDCAT-COL_POS = COLPOS.
        LD_WA_FIELDCAT-SELTEXT_S = 'New Status'.
        LD_WA_FIELDCAT-SELTEXT_M = 'New Status'.
        LD_WA_FIELDCAT-SELTEXT_L = 'New Status'.
        LD_WA_FIELDCAT-OUTPUTLEN = 15.
        MODIFY GD_FIELDCAT FROM LD_WA_FIELDCAT.
      WHEN 'CHANGED'.
        COLPOS = COLPOS + 1.
        LD_WA_FIELDCAT-NO_OUT = SPACE.    "ausgeben
        LD_WA_FIELDCAT-COL_POS = COLPOS.
        IF P_TEST IS INITIAL.
          LD_WA_FIELDCAT-SELTEXT_S = 'Changed'.
          LD_WA_FIELDCAT-SELTEXT_M = 'Changed'.
          LD_WA_FIELDCAT-SELTEXT_L = 'Status changed'.
        ELSE.
          LD_WA_FIELDCAT-SELTEXT_S = 'poss.'.
          LD_WA_FIELDCAT-SELTEXT_M = 'Change poss.'.
          LD_WA_FIELDCAT-SELTEXT_L = 'Change possible'.
        ENDIF.
        MODIFY GD_FIELDCAT FROM LD_WA_FIELDCAT.

      WHEN OTHERS.
        LD_WA_FIELDCAT-NO_OUT = 'X'.
        LD_WA_FIELDCAT-KEY    = SPACE.
        MODIFY GD_FIELDCAT FROM LD_WA_FIELDCAT
             TRANSPORTING NO_OUT
                          KEY.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
*       FORM ALV_EVENT_DISPLAY
*----------------------------------------------------------------------*
FORM alv_display.

* Lokale Daten
  DATA:
    t_events      TYPE slis_t_event,        " User-Events
    wa_event      LIKE LINE OF t_events,
    s_layout      TYPE slis_layout_alv,     " Layout der ALV-Tabelle
    ld_callb_prog LIKE sy-repid,            " Callback Programm
    ld_cb_status  LIKE sy-xform,            " Callback Form: Status
    ld_cb_ucom    LIKE sy-xform,            " Callback Form: Usercommand
    ld_cb_top     LIKE sy-xform.            " Callback Form: Top of Page

* Layout-Angaben
  CLEAR s_layout.
  s_layout-min_linesize = 100.
*  s_layout-f2code = 'BACK'.

* Callback Programm definieren
  ld_callb_prog = sy-repid.

* Callback-Routinen definieren
  ld_cb_ucom   = 'ALV_CB_XLIST_USER_COMMAND'.
  ld_cb_top    = 'ALV_CB_TOP_OF_PAGE'.

* Eventtabelle zusammenstellen
  READ TABLE t_events
             WITH KEY name = slis_ev_top_of_page
             INTO wa_event.
  IF sy-subrc NE 0.
    MOVE ld_cb_top           TO wa_event-form.
    MOVE slis_ev_top_of_page TO wa_event-name.
    APPEND wa_event TO t_events.
  ENDIF.

  s_layout-colwidth_optimize = ' '.
  ld_callb_prog = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
*           I_INTERFACE_CHECK        = ' '
            I_CALLBACK_PROGRAM       = LD_CALLB_PROG
*           I_CALLBACK_PF_STATUS_SET = ' '
*           I_CALLBACK_USER_COMMAND  = ' '
*           I_STRUCTURE_NAME         =
            IS_LAYOUT                = S_LAYOUT
            IT_FIELDCAT              = GD_FIELDCAT
*           IT_EXCLUDING             =
*           IT_SPECIAL_GROUPS        =
*           IT_SORT                  =
*           IT_FILTER                =
*           IS_SEL_HIDE              =
*           I_DEFAULT                = 'X'
*           I_SAVE                   = ' '
*           IS_VARIANT               = ' '
            IT_EVENTS                = T_EVENTS
*           IT_EVENT_EXIT            =
*           IS_PRINT                 =
*           IS_REPREP_ID             =
*           I_SCREEN_START_COLUMN    = 0
*           I_SCREEN_START_LINE      = 0
*           I_SCREEN_END_COLUMN      = 0
*           I_SCREEN_END_LINE        = 0
*      IMPORTING
*           E_EXIT_CAUSED_BY_CALLER  =
*           ES_EXIT_CAUSED_BY_USER   =
       TABLES
            T_OUTTAB                 = GD_TBL
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM alv_cb_top_of_page                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM alv_cb_top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = gd_top_of_list.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  XBEW_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->t_MATNR  text
*      -->t_BWTAR  text
*      -->t_MTART  text
*      -->t_MATKL  text
*      -->t_SPART  text
*      -->t_KALNR_ALL  text
*      -->t_BWKEY-BWKEY  text
*----------------------------------------------------------------------*
FORM XBEW_SELECTION TABLES   T_MATNR STRUCTURE R_MATNR
                             T_BWTAR STRUCTURE R_BWTAR
                             T_VBELN STRUCTURE R_VBELN
                             T_POSNR STRUCTURE R_POSNR
                             T_PSPNR STRUCTURE R_PSPNR
                             T_MTART STRUCTURE R_MTART
                             T_MATKL STRUCTURE R_MATKL
                             T_SPART STRUCTURE R_SPART
                             T_KALNR_ALL STRUCTURE T_KALNR_ALL
                    USING    T_BWKEY-BWKEY TYPE BWKEY.

  DATA: BEGIN OF MBEW_WA,
              MATNR   LIKE MBEW-MATNR,
              BWKEY   LIKE MBEW-BWKEY,
              BWTAR   LIKE MBEW-BWTAR,
              KALN1   LIKE MBEW-KALN1,
        END OF MBEW_WA.
  DATA: BEGIN OF EBEW_WA,
              MATNR   LIKE EBEW-MATNR,
              BWKEY   LIKE EBEW-BWKEY,
              BWTAR   LIKE EBEW-BWTAR,
              VBELN   LIKE EBEW-VBELN,
              POSNR   LIKE EBEW-POSNR,
              KALN1   LIKE EBEW-KALN1,
        END OF EBEW_WA.
  DATA: BEGIN OF QBEW_WA,
              MATNR   LIKE QBEW-MATNR,
              BWKEY   LIKE QBEW-BWKEY,
              BWTAR   LIKE QBEW-BWTAR,
              PSPNR   LIKE QBEW-PSPNR,
              KALN1   LIKE QBEW-KALN1,
        END OF QBEW_WA.

  DATA: CKMLHD_WA LIKE CKMLHD.

  CLEAR:    T_KALNR_ALL.
  REFRESH:  T_KALNR_ALL.
* selection of MBEW
  IF P_MBEW = 'X'.
    SELECT MBEW~MATNR
           MBEW~BWKEY
           MBEW~BWTAR
           MBEW~KALN1
      INTO MBEW_WA
      FROM MBEW   JOIN MARA
      ON   MBEW~MATNR = MARA~MATNR
      WHERE MBEW~MATNR IN T_MATNR
      AND   MBEW~BWKEY = T_BWKEY-BWKEY
      AND   MBEW~BWTAR IN T_BWTAR
      AND   MARA~MTART IN T_MTART
      AND   MARA~MATKL IN T_MATKL
      AND   MARA~SPART IN T_SPART.

      CLEAR:  T_KALNR_ALL.
      MOVE-CORRESPONDING MBEW_WA TO T_KALNR_ALL.
      MOVE: MBEW_WA-KALN1        TO T_KALNR_ALL-KALNR.
      APPEND T_KALNR_ALL.

      CLEAR: MBEW_WA.                     "for next select
    ENDSELECT.
  ENDIF.
* selection of EBEW
  IF P_EBEW = 'X'.
    SELECT EBEW~MATNR
           EBEW~BWKEY
           EBEW~BWTAR
           EBEW~VBELN
           EBEW~POSNR
           EBEW~KALN1
      INTO EBEW_WA
      FROM EBEW   JOIN MARA
      ON   EBEW~MATNR = MARA~MATNR
      WHERE EBEW~MATNR IN T_MATNR
      AND   EBEW~BWKEY = T_BWKEY-BWKEY
      AND   EBEW~BWTAR IN T_BWTAR
      AND   EBEW~VBELN IN T_VBELN
      AND   EBEW~POSNR IN T_POSNR
      AND   MARA~MTART IN T_MTART
      AND   MARA~MATKL IN T_MATKL
      AND   MARA~SPART IN T_SPART.

      CLEAR:  T_KALNR_ALL.
      MOVE-CORRESPONDING EBEW_WA TO T_KALNR_ALL.
      MOVE: EBEW_WA-KALN1        TO T_KALNR_ALL-KALNR.
      APPEND T_KALNR_ALL.

      CLEAR: EBEW_WA.                     "for next select
    ENDSELECT.
  ENDIF.
* selection of QBEW
  IF P_QBEW = 'X'.
    SELECT QBEW~MATNR
           QBEW~BWKEY
           QBEW~BWTAR
           QBEW~PSPNR
           QBEW~KALN1
      INTO QBEW_WA
      FROM QBEW   JOIN MARA
      ON   QBEW~MATNR = MARA~MATNR
      WHERE QBEW~MATNR IN T_MATNR
      AND   QBEW~BWKEY = T_BWKEY-BWKEY
      AND   QBEW~BWTAR IN T_BWTAR
      AND   QBEW~PSPNR IN T_PSPNR
      AND   MARA~MTART IN T_MTART
      AND   MARA~MATKL IN T_MATKL
      AND   MARA~SPART IN T_SPART.

      CLEAR:  T_KALNR_ALL.
      MOVE-CORRESPONDING QBEW_WA TO T_KALNR_ALL.
      MOVE: QBEW_WA-KALN1        TO T_KALNR_ALL-KALNR.
      APPEND T_KALNR_ALL.

      CLEAR: QBEW_WA.                     "for next select
    ENDSELECT.
  ENDIF.

*Sortieren der Ausgabetabelle
  SORT T_KALNR_ALL BY BWKEY
                      MATNR
                      BWTAR
                      VBELN
                      POSNR
                      PSPNR.
*Pruefen, ob Ausgabespalten initial.
  CLEAR: BWTAR_EXIST,
         VBELN_EXIST,
         PSPNR_EXIST.
  LOOP AT T_KALNR_ALL.
    IF NOT T_KALNR_ALL-BWTAR IS INITIAL.
      BWTAR_EXIST = 'X'.
    ENDIF.
    IF NOT T_KALNR_ALL-VBELN IS INITIAL.
      VBELN_EXIST = 'X'.
    ENDIF.
    IF NOT T_KALNR_ALL-PSPNR IS INITIAL.
      PSPNR_EXIST = 'X'.
    ENDIF.
* Abbruchkriterium
    IF BWTAR_EXIST = 'X'  AND
       VBELN_EXIST = 'X'  AND
       PSPNR_EXIST = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " XBEW_SELECTION
