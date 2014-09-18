REPORT ztcou_bom_trace .

TABLES: aenr, stko, stas, stpo, stzu.

constants:
         typ_mat        like stzu-stlty       value 'M',
         max_grg        like sy-datum         value '99991231',
         min_grg        like sy-datum         value '19000101'.

DATA: BEGIN OF i_aenr OCCURS 0,
         aennr LIKE aenr-aennr,
      END OF i_aenr.

DATA: BEGIN OF stasx OCCURS 0,
         aennr LIKE stas-aennr,
         datuv LIKE aenr-datuv,
         datub LIKE stasb-datub,
         stlty LIKE stas-stlty, "M-category
         stlnr LIKE stas-stlnr, "BoM
         stlal LIKE stas-stlal, "Alt
         stlkn LIKE stas-stlkn, "node no
         stasz LIKE stas-stasz, "counter
         lkenz LIKE stas-lkenz, "deletion
         stvkn LIKE stas-stvkn, "Item#
      END OF stasx.

DATA: BEGIN OF stltab OCCURS 0,
         stlty          LIKE stzu-stlty,  "Category
         stlnr          LIKE stzu-stlnr,  "Bill of material
         stlan          LIKE stzu-stlan,  "BOM usage
      END OF stltab.

DATA: stpox LIKE stpo OCCURS 0 WITH HEADER LINE,
      stpob LIKE stpob OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF key_stern,
         stlkn          LIKE stpo-stlkn,
         stpoz          LIKE stpo-stpoz,
         stlal          LIKE stas-stlal,
      END OF key_stern.
data: sterntab like cszalt occurs 0 with header line.

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

SELECT-OPTIONS: s_datum FOR sy-datum,
                s_aennr FOR aenr-aennr,
                s_stlty FOR stko-stlty,
                s_stlnr FOR stko-stlnr.


SELECT *
    INTO CORRESPONDING FIELDS OF TABLE stasx
    FROM stas
    INNER JOIN stko
       ON stas~stlty = stko~stlty
      AND stas~stlnr = stko~stlnr
      AND stas~stlal = stko~stlal
    INNER JOIN aenr
       ON aenr~aennr = stas~aennr
    WHERE aenr~aenst = '01'        "active
      AND aenr~datuv IN s_datum    "valid from
      AND aenr~fluse = 'X'        "for data mgt
      AND aenr~aennr IN s_aennr
      AND stko~stlty IN s_stlty
      AND stko~stlnr IN s_stlnr.


SORT stasx BY stlal datuv DESCENDING.

LOOP AT stasx.
  stltab-stlty = stasx-stlty.
  stltab-stlnr = stasx-stlnr.
  COLLECT stltab.
ENDLOOP.

SELECT * FROM stpo INTO CORRESPONDING FIELDS OF TABLE stpob
   WHERE stlty = stltab-stlty
     AND stlnr = stltab-stlnr.

SORT stasx BY stlkn datuv DESCENDING.
LOOP AT stpob.
  MOVE-CORRESPONDING stpob TO key_stern.
  READ TABLE stasx WITH KEY stlkn = stpob-stlkn BINARY SEARCH.
  LOOP AT stasx FROM sy-tabix.
    IF stasx-stlkn NE stpob-stlkn.
      EXIT.
    ENDIF.
    key_stern-stlal = stasx-stlal.
    READ TABLE sterntab WITH KEY key_stern BINARY SEARCH.
    IF sy-subrc = 0.
      IF stasx-lkenz IS INITIAL.
        IF stasx-datuv GE sterntab-datuv.
          sterntab-aenv2 = stasx-aennr.
          sterntab-datuv = stasx-datuv.
        ENDIF.
      ELSE.
        IF stasx-datuv LT sterntab-datub.
          sterntab-datub = stasx-datuv.
        ENDIF.
      ENDIF.
      MODIFY sterntab INDEX sy-tabix.

    ELSE.
      MOVE-CORRESPONDING key_stern TO sterntab.
      sterntab-aenv1 = stpob-aennr.
      sterntab-datuv = stpob-datuv.
      sterntab-datub = max_grg.
      IF stasx-lkenz IS INITIAL.
        sterntab-aenv2 = stasx-aennr.
        IF stasx-datuv GT sterntab-datuv.
          sterntab-datuv = stasx-datuv.
        ENDIF.
*---------- Auslauf
      ELSE.
        sterntab-datub = stasx-datuv.
      ENDIF.
      INSERT sterntab INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
ENDLOOP.
CLEAR key_stern.

  loop at stasx.
    read table sterntab with key stasx-stlkn binary search.
    loop at sterntab from sy-tabix.
      if sterntab-stlkn <> stasx-stlkn.
        exit.
      endif.
      check stasx-stlal = sterntab-stlal.
      check stasx-datuv = sterntab-datub.
      sterntab-aenb2 = stasx-aennr.
      modify sterntab.
    endloop.
  endloop.



PERFORM field_setting(zcogsrev) TABLES gt_fieldcat USING :
'AENNR'       'AENNR'       '10' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
'DATUV'       'DATUV'       '10' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
'STLTY'       'STLTY'       '10' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
'STLNR'       'STLNR'       '10' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
'STLAL'       'STLAL'       '10' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
'STLKN'       'STLKN'       '10' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
'STASZ'       'STASZ'       '10' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
'LKENZ'       'LKENZ'       '10' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
'STVKN'       'STVKN'       '10' ' ' 'L' ' ' ' ' ' ' ' ' ' '.

g_repid = sy-repid.
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
          i_callback_program = g_repid
          it_fieldcat        = gt_fieldcat
          i_save             = 'A'
     TABLES
          t_outtab           = stasx
     EXCEPTIONS
          program_error      = 1
          OTHERS             = 2.
