FUNCTION Z_FRF_ORDER_SEARCH.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(S_OD_SERC) LIKE  ZSRF_OD_SERC STRUCTURE  ZSRF_OD_SERC
*"       OPTIONAL
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_OD_DISP STRUCTURE  ZSRF_OD_DISP
*"----------------------------------------------------------------------
* TABLES
  TABLES: AFKO,   "Order header data PP orders
          RESB,   "Reservation/dependent requirements
          AUFK,   "Order master data
          AFIH.   "Maintenance order header
* DATA
  DATA: BEGIN OF LT_OD_DISP OCCURS 0,
          AUFNR TYPE ZSRF_OD_DISP-AUFNR,
          AUART TYPE ZSRF_OD_DISP-AUART,
          GSTRI TYPE AFKO-GSTRI,
          GETRI TYPE AFKO-GETRI,
          DATUB TYPE SY-DATUM,     "ZSRF_OD_DISP-DATUB,
          INGPR TYPE ZSRF_OD_DISP-INGPR,
          KTEXT TYPE ZSRF_OD_DISP-KTEXT,
        END OF LT_OD_DISP.

  DATA: BEGIN OF LT_DISP OCCURS 0,
          AUFNR TYPE ZSRF_OD_DISP-AUFNR,
          AUART TYPE ZSRF_OD_DISP-AUART,
          GSTRI TYPE AFKO-GSTRI,
          GETRI TYPE AFKO-GETRI,
          DATUB TYPE SY-DATUM,     "ZSRF_OD_DISP-DATUB,
          INGPR TYPE ZSRF_OD_DISP-INGPR,
          KTEXT TYPE ZSRF_OD_DISP-KTEXT,
          KZEAR TYPE RESB-KZEAR,
        END OF LT_DISP.
  DATA: L_TABIX LIKE SY-TABIX,
        L_LINES LIKE SY-TABIX.
* RANGES
  RANGES: R_DATUV FOR ZSRF_OD_SERC-DATUV, "Valid-from date
          R_LGORT FOR ZSRF_OD_SERC-LGORT. "Storage location
* CHECK
  CHECK NOT S_OD_SERC IS INITIAL.
* Period
  IF NOT S_OD_SERC-DATUV IS INITIAL.
    R_DATUV-LOW    = S_OD_SERC-DATUV.
    R_DATUV-SIGN   = 'I'.
    R_DATUV-OPTION = 'EQ'.
    APPEND R_DATUV.
  ENDIF.
* Group
  IF NOT S_OD_SERC-LGORT IS INITIAL.
    R_LGORT-LOW    = S_OD_SERC-LGORT.
    R_LGORT-SIGN   = 'I'.
    R_LGORT-OPTION = 'EQ'.
    APPEND R_LGORT.
  ENDIF.
  IF S_OD_SERC-COMPL EQ 'N'.
    SELECT A~AUFNR
           C~AUART
           A~GSTRI "added 12/16/04
           A~GETRI "added 12/16/04
           A~GSTRP
           D~INGPR
           C~KTEXT
         FROM AFKO AS A INNER JOIN RESB AS B
                          ON  A~RSNUM EQ B~RSNUM
                          AND B~XWAOK EQ 'X'
* Changed by 100565 on 12/16/2004 as Order Search for Not completed
*order was not functioning
*                          AND B~KZEAR EQ SPACE
* END CHANGE BY 100565 12/16/2004
                        INNER JOIN AUFK AS C
                          ON  A~AUFNR EQ C~AUFNR
                        INNER JOIN AFIH AS D
                          ON A~AUFNR EQ D~AUFNR
         INTO TABLE LT_OD_DISP
         WHERE A~GSTRP IN R_DATUV
         AND A~GSTRI EQ 0  "ADDED 12/16/04
         AND A~GETRI EQ 0  "ADDED 12/16/04
         AND   B~LGORT IN R_LGORT.
    IF SY-SUBRC EQ 0.
*      T_OD_DISP[] = LT_OD_DISP[].
*A__PAUL CHANGE
      SORT LT_DISP BY AUFNR.
      LOOP AT LT_OD_DISP.
        MOVE-CORRESPONDING LT_OD_DISP TO T_OD_DISP.
        APPEND T_OD_DISP.
        CLEAR T_OD_DISP.
      ENDLOOP.
*E__<*
      SORT T_OD_DISP BY AUFNR.
      DELETE ADJACENT DUPLICATES FROM T_OD_DISP COMPARING AUFNR.
      ZRESULT = TEXT-M03.
      E_MESS	= TEXT-M01.
    ELSE.
      ZRESULT = TEXT-M04.
      E_MESS	= TEXT-M02.
    ENDIF.
  ELSE.
    SELECT A~AUFNR
           C~AUART
           A~GSTRI
           A~GETRI
           A~GSTRP
           D~INGPR
           C~KTEXT
           B~KZEAR
         FROM AFKO AS A INNER JOIN RESB AS B
                          ON  A~RSNUM EQ B~RSNUM
                          AND B~XWAOK EQ 'X'
*                          AND B~KZEAR EQ SPACE
                          AND B~XLOEK NE 'X'
                        INNER JOIN AUFK AS C
                          ON  A~AUFNR EQ C~AUFNR
                        INNER JOIN AFIH AS D
                          ON A~AUFNR EQ D~AUFNR
         INTO TABLE LT_DISP
         WHERE A~GSTRP IN R_DATUV
         AND A~GSTRI NE 0 "ADDED 12/16/04
         AND A~GETRI NE 0 "ADDED 12/16/04
         AND   B~LGORT IN R_LGORT.
    IF SY-SUBRC EQ 0.
      SORT LT_DISP BY AUFNR.
      LOOP AT LT_DISP.
        MOVE-CORRESPONDING LT_DISP TO T_OD_DISP.
        APPEND T_OD_DISP.
        CLEAR T_OD_DISP.
      ENDLOOP.
* Changed by 100565
* Order list was not being displayed correctly. The Select seems to be
*functioning correctly. Problem is in the loop statement when all the
*enteries are getting deleted.
*      LOOP AT LT_DISP.
*        IF LT_DISP-KZEAR IS INITIAL.
*          LOOP AT T_OD_DISP WHERE AUFNR EQ LT_DISP-AUFNR.
*            L_TABIX = SY-TABIX.
*            DELETE T_OD_DISP INDEX L_TABIX.
*            CLEAR T_OD_DISP.
*          ENDLOOP.
*        ENDIF.
*      ENDLOOP.
*sort T_OD_DISP by aufnr.
*       LOOP AT T_OD_DISP.
*        delete adjacent duplicates from T_OD_DISP comparing aufnr.
*      ENDLOOP.


      DESCRIBE TABLE T_OD_DISP LINES L_LINES.
      IF L_LINES GE '1'.
        SORT T_OD_DISP BY AUFNR.
        DELETE ADJACENT DUPLICATES FROM T_OD_DISP COMPARING AUFNR.
        ZRESULT   = TEXT-M03.
        E_MESS	= TEXT-M01.
      ELSE.
        ZRESULT   = TEXT-M04.
        E_MESS	= TEXT-M02.
      ENDIF.
    ELSE.
      ZRESULT = TEXT-M04.
      E_MESS	= TEXT-M02.
    ENDIF.
  ENDIF.
ENDFUNCTION.
