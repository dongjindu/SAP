*----------------------------------------------------------------------*
*   INCLUDE ZXM61U01                                                   *
*----------------------------------------------------------------------*
  CLEAR: NO_PLANNING, STOP_PLANNING.
  CASE USER_KEY.
*----------------------------------------------------------------------*
* select materials for one MRP controller (specified in user_par)
*----------------------------------------------------------------------*
    WHEN '001'.
* 1. PARAMETER: USER_PAR(dispo) = SPACE (EXIT)
* 2. PARAMETER: Run particular MRP Controller(dispo)
      UXPAR = USER_PAR.
      CONDENSE UXPAR.
      WRITE UXPAR+0(3) TO DISPO.
      IF DISPO IS INITIAL.
        EXIT.
      ENDIF.
      IF MT61D-DISPO <> DISPO.
        NO_PLANNING = 'X'.
      ENDIF.
  ENDCASE.
