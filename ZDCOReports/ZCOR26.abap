  REPORT zcor701 .

  TABLES: mbewh, mbew, t030.


  PARAMETERS:
         p_lfgja LIKE mbewh-lfgja OBLIGATORY,
         p_lfmon LIKE mbewh-lfmon OBLIGATORY.

  SELECT-OPTIONS:
         s_bklas FOR mbewh-bklas,
         s_bwkey FOR mbewh-bwkey,
         s_matnr FOR mbewh-matnr.

  DATA : BEGIN OF it_t030 OCCURS 0,
          bklas LIKE t030-bklas,
          konts LIKE t030-konts,
         END OF it_t030.

  DATA : BEGIN OF t_mats OCCURS 0,
           kalnr  LIKE ckmlhd-kalnr ,
           matnr  LIKE ckmlhd-matnr ,
           bwkey  LIKE ckmlhd-bwkey ,
           lbkum  LIKE ckmlpp-lbkum ,
           status LIKE ckmlpp-status,
           peinh  LIKE ckmlcr-peinh ,
           vprsv  LIKE ckmlcr-vprsv ,
           stprs  LIKE ckmlcr-stprs ,
           pvprs  LIKE ckmlcr-pvprs,
           salk3  LIKE ckmlcr-salk3 ,
           salkv  LIKE ckmlcr-salkv ,
           lfgja  LIKE mbew-lfgja,
           lfmon  LIKE mbew-lfmon,
           bklas  LIKE mbew-bklas,
         END OF t_mats.

  DATA: BEGIN OF itab OCCURS 0.
          INCLUDE STRUCTURE t_mats.
  DATA:   konts  LIKE t030-konts,
        END OF itab.


*----------------------------------------------------------------------*
*   Start-of-selection
*----------------------------------------------------------------------*
  START-OF-SELECTION.

*   Select Standard Accounts Table (T001 table; company code)
    SELECT bklas konts INTO CORRESPONDING FIELDS OF TABLE it_t030
      FROM t030
      WHERE ktopl = 'HNA1'
        AND ktosl = 'BSX'.
    SORT it_t030 BY bklas.

* The RM07MMFI report is started to check the time "Previous year". The
*  report selects the data in the following steps:
*- All entries with period 12/2005 are read from the MBEWH history table
* - All entries from the MBEW table are read that have a period smaller
*  or equal to 12/2005.
*  - Then, the balances of the stock accounts for period 12/2005 are
*  determined.

    SELECT ckmlhd~kalnr ckmlhd~matnr ckmlhd~bwkey
           ckmlpp~lbkum ckmlpp~status
           ckmlcr~peinh ckmlcr~vprsv ckmlcr~stprs ckmlcr~pvprs
           ckmlcr~salk3 ckmlcr~salkv
           mbew~lfgja   mbew~lfmon   mbew~bklas
      INTO CORRESPONDING FIELDS OF TABLE t_mats
      FROM ckmlhd
      INNER JOIN mbew
         ON mbew~kaln1   = ckmlhd~kalnr
      INNER JOIN ckmlpp
         ON ckmlpp~kalnr = ckmlhd~kalnr
      INNER JOIN ckmlcr
         ON ckmlcr~kalnr = ckmlpp~kalnr
        AND ckmlcr~bdatj = ckmlpp~bdatj
        AND ckmlcr~poper = ckmlpp~poper
     WHERE ckmlhd~matnr IN s_matnr
       AND ckmlhd~bwkey IN s_bwkey
       AND ckmlpp~bdatj = p_lfgja
       AND ckmlpp~poper = p_lfmon
       AND ckmlpp~lbkum <> 0.

    SORT t_mats BY kalnr.
*Add old balance w/o new transaction (period: open)
* - No CKMLPP, CKMLCR
    SELECT * FROM mbew
     WHERE matnr IN s_matnr
       AND bwkey IN s_bwkey
       AND lfgja = p_lfgja
       AND lfmon < p_lfmon
       AND lbkum <> 0.

      READ TABLE t_mats WITH KEY kalnr = mbew-kaln1.
      IF sy-subrc <> 0.
        MOVE-CORRESPONDING mbew TO t_mats.
        t_mats-pvprs = mbew-verpr.
        t_mats-kalnr = mbew-kaln1.
        APPEND t_mats.
      ENDIF.
    ENDSELECT.
    SELECT * FROM mbew
     WHERE matnr IN s_matnr
       AND bwkey IN s_bwkey
       AND lfgja < p_lfgja
       AND lbkum <> 0.
      READ TABLE t_mats WITH KEY kalnr = mbew-kaln1.
      IF sy-subrc <> 0.
        MOVE-CORRESPONDING mbew TO t_mats.
        t_mats-pvprs = mbew-verpr.
        t_mats-kalnr = mbew-kaln1.
        APPEND t_mats.
      ENDIF.
    ENDSELECT.

    LOOP AT t_mats.
      MOVE-CORRESPONDING t_mats TO itab.

*----- get from current value
      IF t_mats-lfgja < p_lfgja
      OR ( t_mats-lfgja = p_lfgja AND t_mats-lfmon <= p_lfmon ).
        IF itab-bklas IN s_bklas.
          PERFORM append_to_itab.
        ENDIF.

*----- get from history
      ELSE.
        PERFORM get_from_hist.
        IF itab-bklas IN s_bklas.
          PERFORM append_to_itab.
        ENDIF.
      ENDIF.
    ENDLOOP.

*----------------------------------------------------------------------*
*   END-of-selection
*----------------------------------------------------------------------*
  END-OF-SELECTION.

    PERFORM call_alv_list.


*&---------------------------------------------------------------------*
*  &      Form  call_alv_list
*&---------------------------------------------------------------------*
  FORM call_alv_list.

*  --- ALV
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
*  ---- ALV

    PERFORM field_setting(zcogsrev) TABLES gt_fieldcat USING :
   'KONTS'       'Account'  '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'BKLAS'       'BKLAS'    '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'BWKEY'       'BWKEY'    '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'MATNR'       'MATNR'    '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'LBKUM'       'LBKUM'    '15' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
   'SALK3'       'SALK3'    '16' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'SALKV'       'SALKV'    '16' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'VPRSV'       'VPRSV'    '01' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
   'STPRS'       'STPRS'    '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
   'PVPRS'       'PVPRS'    '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
   'PEINH'       'PEINH'    '05' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'LFGJA'       'LFGJA'    '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'LFMON'       'LFMON'    '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.

    g_repid = sy-repid.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
         EXPORTING
              i_callback_program = g_repid
              it_fieldcat        = gt_fieldcat
              i_save             = 'A'
         TABLES
              t_outtab           = itab
         EXCEPTIONS
              program_error      = 1
              OTHERS             = 2.


  ENDFORM.                    " call_alv_list
*&---------------------------------------------------------------------*
*&      Form  get_from_hist
*&---------------------------------------------------------------------*
  FORM get_from_hist.
    DATA: lv_mbewh LIKE mbewh.

*select last record by sorting.
    CLEAR lv_mbewh.

    SELECT * INTO lv_mbewh
     FROM mbewh
        WHERE matnr = t_mats-matnr
          AND bwkey = t_mats-bwkey
*         AND bwtar = t_mats-bwtar
          AND lfgja = p_lfgja
          AND lfmon <= p_lfmon
     ORDER BY lfgja DESCENDING
              lfmon DESCENDING.
      EXIT.
    ENDSELECT.
    IF sy-subrc <> 0.
      SELECT * INTO lv_mbewh
       FROM mbewh
          WHERE matnr = t_mats-matnr
            AND bwkey = t_mats-bwkey
*           AND bwtar = t_mats-bwtar
           AND lfgja < p_lfgja
       ORDER BY lfgja DESCENDING
                lfmon DESCENDING.
        EXIT.
      ENDSELECT.
    ENDIF.

    IF sy-subrc = 0.
      itab-bklas = lv_mbewh-bklas.
    ENDIF.

  ENDFORM.                    " get_from_hist
*&---------------------------------------------------------------------*
*&      Form  append_to_itab
*&---------------------------------------------------------------------*
  FORM append_to_itab.
    READ TABLE it_t030 WITH KEY bklas = itab-bklas.
    itab-konts = it_t030-konts.
    APPEND itab.
  ENDFORM.                    " append_to_itab
