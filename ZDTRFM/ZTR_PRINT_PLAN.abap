FUNCTION ZTR_PRINT_PLAN.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(HIER) TYPE  CFHID2
*"     VALUE(SKALV) TYPE  TS70SKAL_VOR DEFAULT '0'
*"     VALUE(DECIM) TYPE  TS70SKAL_NACH DEFAULT '0'
*"     VALUE(P_DATE) TYPE  SY-DATUM OPTIONAL
*"  TABLES
*"      TITLE
*"      DATE
*"      BEGINNING STRUCTURE  FDSR OPTIONAL
*"      PLAN STRUCTURE  FDSR
*"      ENDING STRUCTURE  FDSR OPTIONAL
*"----------------------------------------------------------------------

  TABLES : T035T.

  DATA : COL_COUNT(2) TYPE N,
         LINE_SIZE TYPE I,
         COL_WID   TYPE I,
         PRINT_SET(1),
         PAGE_COL TYPE I.

  DATA : LT_DATE LIKE SY-DATUM OCCURS 0 WITH HEADER LINE.
  DATA : LT_TITLE(30) OCCURS 0 WITH HEADER LINE.

  LEAVE TO LIST-PROCESSING.

  PAGE_COL = 15 + SKALV - DECIM.
  LT_DATE[]  = DATE[].
  LT_TITLE[] = TITLE[].

  DO.
    IF LT_DATE[] IS INITIAL.
      EXIT.
    ENDIF.
    REFRESH : DATE, TITLE.
    LOOP AT LT_DATE.
      APPEND LT_DATE TO DATE.
      DELETE LT_DATE.
      IF LINES( DATE ) > PAGE_COL.
        EXIT.
      ENDIF.
    ENDLOOP.

    LOOP AT LT_TITLE.
      APPEND LT_TITLE TO TITLE.
      DELETE LT_TITLE.
      IF LINES( TITLE ) > PAGE_COL.
        EXIT.
      ENDIF.
    ENDLOOP.

    COL_COUNT = LINES( DATE ).
    COL_WID = SKALV / 3.
    COL_WID = 16 - SKALV + DECIM - COL_WID.
    LINE_SIZE = COL_COUNT * ( COL_WID + 1 ) + 65.

    IF PRINT_SET IS INITIAL.
      PERFORM PRINT_SET USING LINE_SIZE.
      PRINT_SET = 'X'.
    ENDIF.
    NEW-PAGE NO-HEADING NO-TITLE LINE-SIZE LINE_SIZE.

    SET PF-STATUS 'MAIN'.
    PERFORM WRITE_TITLE TABLES TITLE
                         USING LINE_SIZE COL_WID SKALV P_DATE ' '.

    PERFORM WRITE_BALANCE TABLES DATE
                                 BEGINNING
                           USING 'Beginning Balance'
                                 LINE_SIZE
                                 COL_WID
                                 SKALV
                                 DECIM.

    PERFORM GET_HIERARCHY USING HIER.

    PERFORM WRITE_PLAN TABLES DATE
                              PLAN
                              TITLE
                       USING  LINE_SIZE
                              COL_WID
                              SKALV
                              DECIM
                              P_DATE.

* Ending Balance
    PERFORM WRITE_BALANCE TABLES DATE
                                 ENDING
                          USING 'Ending Balance'
                                 LINE_SIZE
                                 COL_WID
                                 SKALV
                                 DECIM.
  ENDDO.




ENDFUNCTION.
