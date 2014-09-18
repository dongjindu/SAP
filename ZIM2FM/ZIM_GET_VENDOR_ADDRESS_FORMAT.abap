FUNCTION ZIM_GET_VENDOR_ADDRESS_FORMAT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(LIFNR) LIKE  LFA1-LIFNR
*"  EXPORTING
*"     REFERENCE(NAME1)
*"     REFERENCE(NAME2)
*"     REFERENCE(NAME3)
*"     REFERENCE(NAME4)
*"     VALUE(P_LFA1) LIKE  LFA1 STRUCTURE  LFA1
*"     VALUE(P_ADRC) LIKE  ADRC STRUCTURE  ADRC
*"  EXCEPTIONS
*"      NO_INPUT
*"      NOT_FOUND
*"----------------------------------------------------------------------

*"----------------------------------------------------------------------

*"----------------------------------------------------------------------

  CLEAR : NAME1, NAME2, NAME3, NAME4, ADRC, LFA1, P_ADRC, P_LFA1.
  IF LIFNR IS INITIAL.
     RAISE NO_INPUT.
  ENDIF.

  SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ LIFNR.
  IF SY-SUBRC NE 0.
     RAISE NOT_FOUND.
  ENDIF.

  SELECT * INTO ADRC FROM ADRC UP TO 1 ROWS
                     WHERE ADDRNUMBER EQ LFA1-ADRNR
                     AND   DATE_FROM  LE SY-DATUM.
*                     AND   NATION     EQ SPACE.
  ENDSELECT.
  IF SY-SUBRC NE 0.
     RAISE NOT_FOUND.
  ENDIF.

  NAME1 = ADRC-NAME1.

* 내자/외자 구분
  IF LFA1-LAND1 EQ 'KR'.   " 내자
     IF NOT ADRC-NAME2 IS INITIAL.
        NAME2 = ADRC-NAME2.
        CONCATENATE ADRC-CITY1 ADRC-CITY2 INTO NAME3
                                                SEPARATED BY SPACE.
        IF NOT ADRC-STREET IS INITIAL AND
           NOT ADRC-STR_SUPPL3 IS INITIAL.
           CONCATENATE ADRC-STREET ',' INTO NAME4.
           CONCATENATE NAME4 ADRC-STR_SUPPL3 INTO NAME4
                       SEPARATED BY SPACE.
        ELSE.
           CONCATENATE ADRC-STREET  ADRC-STR_SUPPL3 INTO NAME4.
        ENDIF.
     ELSE.
        CONCATENATE ADRC-CITY1 ADRC-CITY2 INTO NAME2
                                               SEPARATED BY SPACE.
        NAME3 = ADRC-STREET.
        NAME4 = ADRC-STR_SUPPL3.
     ENDIF.
     PERFORM   P2000_ADDRESS_CONCATENATE
                             USING   NAME4  ADRC-POST_CODE1.
  ELSE.                     " 외자
     SELECT SINGLE * FROM  T005T
*                    WHERE SPRAS EQ SY-LANGU
                     WHERE SPRAS EQ 'E'
                     AND   LAND1 EQ LFA1-LAND1.

     IF NOT ADRC-NAME2 IS INITIAL.
        NAME2 = ADRC-NAME2.
        NAME3 = ADRC-STREET.

        IF NOT ADRC-STR_SUPPL3 IS INITIAL.
           MOVE ADRC-STR_SUPPL3 TO NAME4.
        ENDIF.
        IF NOT ADRC-CITY2 IS INITIAL.
           PERFORM   P2000_ADDRESS_CONCATENATE
                                  USING   NAME4  ADRC-CITY2.
        ENDIF.
        IF NOT ADRC-CITY1 IS INITIAL.
           PERFORM   P2000_ADDRESS_CONCATENATE
                                  USING   NAME4  ADRC-CITY1.
        ENDIF.
        IF NOT T005T-LANDX IS INITIAL.
           PERFORM   P2000_ADDRESS_CONCATENATE
                                  USING   NAME4   T005T-LANDX.
        ENDIF.
        IF NOT ADRC-POST_CODE1 IS INITIAL.
           PERFORM   P2000_ADDRESS_CONCATENATE
                                  USING   NAME4  ADRC-POST_CODE1.
        ENDIF.
     ELSE.
        NAME2 = ADRC-STREET.
        IF NOT ADRC-STR_SUPPL3 IS INITIAL.
           NAME3 = ADRC-STR_SUPPL3.
           IF NOT ADRC-CITY2 IS INITIAL.
              PERFORM   P2000_ADDRESS_CONCATENATE
                                     USING   NAME4  ADRC-CITY2.
           ENDIF.
           IF NOT ADRC-CITY1 IS INITIAL.
              PERFORM   P2000_ADDRESS_CONCATENATE
                                     USING   NAME4  ADRC-CITY1.
           ENDIF.
           IF NOT T005T-LANDX IS INITIAL.
              PERFORM   P2000_ADDRESS_CONCATENATE
                                     USING   NAME4   T005T-LANDX.
           ENDIF.
           IF NOT ADRC-POST_CODE1 IS INITIAL.
              PERFORM   P2000_ADDRESS_CONCATENATE
                                  USING   NAME4  ADRC-POST_CODE1.
           ENDIF.

        ELSE.
           IF NOT ADRC-CITY2 IS INITIAL.
              PERFORM   P2000_ADDRESS_CONCATENATE
                                     USING   NAME3  ADRC-CITY2.
           ENDIF.
           IF NOT ADRC-CITY1 IS INITIAL.
              PERFORM   P2000_ADDRESS_CONCATENATE
                                     USING   NAME3  ADRC-CITY1.
           ENDIF.

           NAME4 = T005T-LANDX.
           PERFORM   P2000_ADDRESS_CONCATENATE
                                  USING   NAME4  ADRC-POST_CODE1.
        ENDIF.
     ENDIF.
  ENDIF.

  P_ADRC = ADRC.
  P_LFA1 = LFA1.

ENDFUNCTION.
