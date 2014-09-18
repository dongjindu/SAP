FUNCTION Z_CO_GET_MI_CODE_BATCH.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      IT_FSC_MI STRUCTURE  ZSFSC_MI
*"----------------------------------------------------------------------

  DATA : BEGIN OF IT_CODE OCCURS 0,
           CODE(2),
         END   OF  IT_CODE.

  DATA : BEGIN OF IT_VALUE OCCURS 0,
           CODE(2),
           VALUE(1),
         END   OF  IT_VALUE.

  DATA $IX LIKE SY-TABIX.

  LOOP AT IT_FSC_MI.
    IF IT_FSC_MI-FSC+13(1) NE SPACE.
      IT_CODE-CODE = IT_FSC_MI-FSC+5(2).
      COLLECT IT_CODE.CLEAR IT_CODE.
    ENDIF.
  ENDLOOP.

  IF NOT IT_CODE[] IS INITIAL.
    SELECT CARX VALU INTO TABLE IT_VALUE
                   FROM ZTBM_ABXOPVDT
                   FOR ALL ENTRIES IN IT_CODE
                   WHERE CARX EQ IT_CODE-CODE
                     AND CLNO EQ '002'.
    SORT IT_VALUE BY CODE.
  ENDIF.

  LOOP AT IT_FSC_MI.
    $IX = SY-TABIX.
    IF IT_FSC_MI-FSC+13(1) EQ SPACE. " old
      IT_FSC_MI-MI = IT_FSC_MI-FSC+6(8).
    ELSE.
      READ TABLE IT_VALUE WITH KEY CODE = IT_FSC_MI-FSC+5(2)
                                   BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        CONCATENATE IT_VALUE-VALUE IT_FSC_MI-FSC+7(7) INTO IT_FSC_MI-MI.
      ENDIF.
    ENDIF.
    MODIFY IT_FSC_MI INDEX $IX TRANSPORTING MI.
  ENDLOOP.

  SORT IT_FSC_MI BY FSC.

ENDFUNCTION.
