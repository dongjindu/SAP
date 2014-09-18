FUNCTION z_mm_if_ob_02_002_db.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"  TABLES
*"      IT_BODY STRUCTURE  ZMMT0036
*"----------------------------------------------------------------------

  DATA : l_dispo LIKE marc-dispo.

  CLEAR : it_m036, it_m036[].

***"Movement Type
**  LOOP AT IT_BODY.
**    CLEAR : L_DISPO.
**    SELECT SINGLE DISPO INTO L_DISPO
**           FROM MARC
**          WHERE MATNR = IT_BODY-MATNR
***            AND werks = 'KVA1'.
***s__Paul Changed 21/04/11
**            AND WERKS = IT_BODY-WERKS.
**
***    IF l_dispo = 'R10'.
**    IF L_DISPO = 'P01'.
**      CONTINUE.
**    ENDIF.
**
**    CASE IT_BODY-BWART.
**      WHEN '122' OR '161' OR
**           '123' OR '162' OR
**           '344' OR '343' OR
**           '501' OR '502' OR
**           '511' OR '512' OR
**           '541' OR '542' OR
**           '551' OR '552' OR
**           '555' OR '556' OR
**           '201' OR '202' OR
**           '701' OR '702' OR
**           '707' OR '708' OR
**           '309' OR '310'.
**
**      WHEN '961' OR '962' OR
**           '963' OR '964' OR
**           '561' OR '562'.
***        IF it_body-lgort EQ 'L401'.
**        IF IT_BODY-LGORT EQ 'P400'.
**
**          CONTINUE.
**        ENDIF.
***s__Paul Changed 05/05/11 : 311-> 301 or 311
**      WHEN '311'.
***s__Paul Changed 06/02/11 : 'S' is ignore
**        IF IT_BODY-SHKZG = 'S'.
**          CONTINUE.
**        ENDIF.
***s__Paul Changed 06/22/11 : 311-> 301 or 311
**        IF ( IT_BODY-LGORT(2) EQ 'G1' AND IT_BODY-UMLGO(2) EQ 'G1' )
**        OR ( IT_BODY-LGORT EQ 'G100' AND IT_BODY-UMLGO EQ 'G999' )
**        OR ( IT_BODY-LGORT EQ 'G999' AND IT_BODY-UMLGO EQ 'G100' )
**        OR ( IT_BODY-LGORT EQ 'P400' AND IT_BODY-UMLGO(1) NE 'G' )
**        OR ( IT_BODY-LGORT EQ 'G100' AND IT_BODY-UMLGO EQ 'P400' ).
**          CONTINUE.
**        ENDIF.
**        IF IT_BODY-SOBKZ  NE 'K'.
**          IF IT_BODY-LGORT    EQ 'P400' OR
***s__Paul Changed 04/29/11 :
***             IT_BODY-LGORT    EQ 'G100'.
**             IT_BODY-LGORT(1) EQ 'G'.
**          ELSE.
**            CONTINUE.
**          ENDIF.
**        ENDIF.
***s__Paul Changed 07/06/11 : 301
**      WHEN '301'.
**        IF ( IT_BODY-LGORT EQ 'G100' AND IT_BODY-UMLGO EQ 'E200' ).
**          CONTINUE.
**        ENDIF.
**
**      WHEN '291' OR '292'.
***        IF it_body-lgort(1) EQ 'L' OR
***           it_body-lgort(1) EQ 'R'.
***S__MODIFY BY PAUL 04/29/11.
***        IF IT_BODY-LGORT NE 'G100'.
**        IF IT_BODY-LGORT(1) NE 'G'.
**          CONTINUE.
**        ENDIF.
**      WHEN OTHERS.
**        CONTINUE.
**    ENDCASE.
**
***e__Paul Changed 21/04/11
**
**    MOVE-CORRESPONDING IT_BODY TO IT_M036.
**
**    PERFORM FIND_MVT_TEXT USING    IT_M036-BWART
**                          CHANGING IT_M036-BTEXT.
**
**    IF NOT IT_M036-GRUND IS INITIAL.
**      PERFORM FIND_REASON_TEXT  USING IT_M036-BWART
**                                      IT_M036-GRUND
**                             CHANGING IT_M036-GRTXT.
**    ENDIF.
**
**    APPEND IT_M036. CLEAR : IT_M036.
**  ENDLOOP.

** new logic by IG.Moon 7/29/2011

  DATA $bwart TYPE bwart.
  DATA $rsnum TYPE rsnum.
  DATA $shkzg TYPE shkzg.

  LOOP AT it_body.

    IF ( sy-uname CP '1*' OR sy-uname CP 'HIS*' ).

*      SELECT SINGLE rsnum shkzg INTO ($rsnum,$shkzg)
*        FROM mseg
*        WHERE mblnr EQ it_body-mblnr
*          AND rsnum EQ space.
*      IF sy-subrc NE 0.
*        CONTINUE.
*      ENDIF.

      SELECT SINGLE bwart INTO $bwart FROM zmmt0092
        WHERE bwart EQ it_body-bwart
          AND werks EQ it_body-werks
          AND lgort EQ it_body-lgort
          AND umwrk EQ it_body-umwrk
          AND umlgo EQ it_body-umlgo
          AND intyn EQ 'Y'
          AND shkzg EQ it_body-shkzg.

      IF sy-subrc EQ 0.

        MOVE-CORRESPONDING it_body TO it_m036.

        PERFORM find_mvt_text USING    it_m036-bwart
                              CHANGING it_m036-btext.

        IF NOT it_m036-grund IS INITIAL.
          PERFORM find_reason_text  USING it_m036-bwart
                                          it_m036-grund
                                 CHANGING it_m036-grtxt.
        ENDIF.

        APPEND it_m036. CLEAR it_m036.

      ENDIF.

    ELSE.
      CONTINUE.
    ENDIF.

  ENDLOOP.

  MODIFY zmmt0036 FROM TABLE it_m036.


ENDFUNCTION.
