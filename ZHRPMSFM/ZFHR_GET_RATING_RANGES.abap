FUNCTION zfhr_get_rating_ranges.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(ES_TITLE) TYPE  ZSHR_RATING_TITLE
*"     REFERENCE(ES_RATING) TYPE  ZSHR_RATING_RANGES
*"----------------------------------------------------------------------


  DATA: lt_rtrg           TYPE TABLE OF zthr_rtrg,
        lt_t77tp          TYPE TABLE OF t77tp.

  DATA: ls_rtrg           TYPE zthr_rtrg,
        ls_t77s0          TYPE t77s0,
        ls_t77tp          TYPE t77tp.

  DATA: l_index           TYPE n LENGTH 2,
        l_fieldname       TYPE string,
        l_from            TYPE string,
        l_to              TYPE string,
        l_rating          TYPE string.

  FIELD-SYMBOLS: <fs>     TYPE any.


  " get Rating Range
  CLEAR lt_rtrg.
  SELECT * FROM zthr_rtrg INTO TABLE lt_rtrg.
  CHECK lines( lt_rtrg ) > 0.

  " get Scale ID
  CLEAR ls_t77s0.
  SELECT SINGLE * FROM t77s0
    INTO ls_t77s0
    WHERE grpid = 'ZPMS'
      AND semid = 'RATNG'.
  CHECK ls_t77s0 IS NOT INITIAL.

  " get Rating Text
  CLEAR lt_t77tp.
  SELECT * FROM t77tp
    INTO TABLE lt_t77tp
    WHERE langu = sy-langu
      AND scale_id = ls_t77s0-gsval.
  CHECK lines( lt_t77tp ) > 0.

************************************
*   set Title
************************************
  CLEAR: ls_t77tp, l_index.
  LOOP AT lt_t77tp INTO ls_t77tp.
    l_index = ls_t77tp-rating.
    CONCATENATE 'ES_TITLE-TITLE_' l_index INTO l_fieldname.
    ASSIGN (l_fieldname) TO <fs>.
    <fs> = ls_t77tp-pstext.

    CLEAR: ls_t77s0, l_fieldname.
  ENDLOOP.

************************************
*   set Rating Range
************************************
  CLEAR: ls_rtrg, l_index.
  LOOP AT lt_rtrg INTO ls_rtrg.
    l_index = ls_rtrg-zdhrid.
    CONCATENATE 'ES_RATING-RATING_' l_index INTO l_fieldname.
    ASSIGN (l_fieldname) TO <fs>.

    l_from = ls_rtrg-zdhrfr.
    l_to = ls_rtrg-zdhrto.
    CONCATENATE l_from '~' l_to INTO l_rating SEPARATED BY space.

    <fs> = l_rating.

    CLEAR: ls_rtrg, l_fieldname.
  ENDLOOP.


ENDFUNCTION.
