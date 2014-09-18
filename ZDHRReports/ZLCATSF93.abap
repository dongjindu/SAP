*----------------------------------------------------------------------*
*   INCLUDE LCATSF93                                                   *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  set_tc_width
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--KEEP_SCOLS  text
*----------------------------------------------------------------------*
FORM SET_TC_WIDTH CHANGING KEEP_SCOLS.

  DATA:    IT_FIELDLIST  TYPE STANDARD TABLE OF D021S,
           SCR_WIDTH     LIKE SY-SCOLS,
           TC_FIXED_COLS LIKE TC_CATSD-FIXED_COLS,
           lincount type i,
           lincount1 type i.

  STATICS: TC_WIDTH   LIKE TC_CATSD-FIXED_COLS,
           FIRST_TIME TYPE C.

  FIELD-SYMBOLS: <FS_FIELDLIST> TYPE D021S.


  IF FIRST_TIME IS INITIAL.

    CALL FUNCTION 'RS_SCRP_GET_SCREEN_INFOS'
         EXPORTING
              DYNNR     = SY-DYNNR
              PROGNAME  = 'SAPLCATS'
         TABLES
              FIELDLIST = IT_FIELDLIST
         EXCEPTIONS
              OTHERS    = 1.

    IF SY-SUBRC = 0.

      READ TABLE IT_FIELDLIST ASSIGNING <FS_FIELDLIST>
        WITH KEY FNAM = 'TC_CATSD'
                 FILL = 'T'.

      TC_WIDTH = <FS_FIELDLIST>-LENG.

      IF NOT TC_FIXED_COLS IS INITIAL.

        TC_CATSD-FIXED_COLS = TC_FIXED_COLS.

      ENDIF.

    ENDIF.


    FIRST_TIME = 'X'.

  ENDIF.

* move the selected employees to the table control
  DESCRIBE TABLE ICATSD LINES  lincount.
  DESCRIBE TABLE allowed_pernr LINES  lincount1.
  if lincount < lincount1.
    loop at pernr_list where mark eq 'X'.
* move all the selected pernrs to the internal table.
      READ TABLE allowed_pernr
                         WITH KEY pernr = pernr_list-pernr
                         BINARY SEARCH.
      if sy-subrc eq 0.
        READ TABLE ICATSD
                         WITH KEY pernr = pernr_list-pernr
                         BINARY SEARCH.
        if sy-subrc <> 0.
          clear:icatsd, pa0007-schkz.
          move-corresponding pernr_list to ICATSD.
          select single schkz into pa0007-schkz from pa0007
                                  where pernr eq pernr_list-pernr and
                                        begda <= CATSFIELDS-DATEFROM and
                                        endda >= CATSFIELDS-DATETO.
          case pa0007-schkz.
          when '1000' or '1001' or '1004' or '1009' or '1010' or '2001'
                    or '2004' or '3001' or '3003' or '4001'.
              icatsd-AWART = '1001'.
          when '1002' or '1003' or '1007' or '1008' or '2002' or '2005'
                    or '2006' or '3002'.
              icatsd-AWART = '1003'.
            when '1006' or '2003'.
              icatsd-AWART = '1004'.
          endcase.
          clear icatsd-mark.
          append ICATSD.
        endif.
      endif.
    endloop.
  endif.

  SCR_WIDTH = SY-SCOLS.

  IF NOT TC_WIDTH IS INITIAL.

    IF SCR_WIDTH < TC_WIDTH.

      KEEP_SCOLS = SCR_WIDTH.

    ELSE.

      KEEP_SCOLS = TC_WIDTH.

    ENDIF.

  ELSE.

    KEEP_SCOLS = SCR_WIDTH.

  ENDIF.

ENDFORM.                               " set_tc_width
