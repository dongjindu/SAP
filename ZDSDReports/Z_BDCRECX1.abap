DATA:   BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.

DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.

DATA:   E_GROUP_OPENED.

data: CTUMODE LIKE CTU_PARAMS-DISMODE value 'N'.
*                                      "A: show all dynpros
*                                      "E: show dynpro on error only
*                                      "N: do not display dynpro,
data: CUPDATE LIKE CTU_PARAMS-UPDMODE value  'L'.
*                                      "S: synchronously
*                                      "A: asynchronously
*                                      "L: local.

TABLES: T100.

*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
FORM BDC_TRANSACTION USING TCODE doc.
  DATA: L_MSTRING(480),
        l_len type i.
  DATA: L_SUBRC LIKE SY-SUBRC.
  REFRESH MESSTAB.
  CALL TRANSACTION TCODE USING BDCDATA
                   MODE   CTUMODE
                   UPDATE CUPDATE
                   MESSAGES INTO MESSTAB.
  L_SUBRC = SY-SUBRC.
*  WRITE: / 'CALL_TRANSACTION',
*           TCODE,
*           'returncode:'(I05),
*           L_SUBRC,
*           'RECORD:',
*           SY-INDEX.
*  read table MESSTAB with key MSGTYP = 'E'.  "OR key MSGTYP = 'A'.
  LOOP AT MESSTAB.
    IF MESSTAB-MSGTYP = 'E' OR MESSTAB-MSGTYP = 'A'.
      SELECT SINGLE * FROM T100 WHERE SPRSL = MESSTAB-MSGSPRA
                                AND   ARBGB = MESSTAB-MSGID
                                AND   MSGNR = MESSTAB-MSGNR.
      IF SY-SUBRC = 0.
        L_MSTRING = T100-TEXT.
        IF L_MSTRING CS '&1'.
          REPLACE '&1' WITH MESSTAB-MSGV1 INTO L_MSTRING.
          REPLACE '&2' WITH MESSTAB-MSGV2 INTO L_MSTRING.
          REPLACE '&3' WITH MESSTAB-MSGV3 INTO L_MSTRING.
          REPLACE '&4' WITH MESSTAB-MSGV4 INTO L_MSTRING.
        ELSE.
          REPLACE '&' WITH MESSTAB-MSGV1 INTO L_MSTRING.
          REPLACE '&' WITH MESSTAB-MSGV2 INTO L_MSTRING.
          REPLACE '&' WITH MESSTAB-MSGV3 INTO L_MSTRING.
          REPLACE '&' WITH MESSTAB-MSGV4 INTO L_MSTRING.
        ENDIF.
        CONDENSE L_MSTRING.
        WRITE: / MESSTAB-MSGTYP, L_MSTRING(250).
*      write: MESSTAB-MSGV2. " MESSTAB-MSGV2.  " MESSTAB-MSGV3,
      ELSE.
        WRITE: / MESSTAB.
      ENDIF.
    ELSE.
      read table MESSTAB with key MSGTYP = 'S'
                                  MSGID = 'V1'
                                  MSGNR = '311'.
      if sy-subrc = 0.
        WRITE: / 'Sales order has been changed ', MESSTAB-MSGV2.
        exit.
      else.
        read table MESSTAB with key MSGTYP = 'S'
                                  MSGID = '00'
                                  MSGNR = '343'.
      if sy-subrc = 0.
        WRITE: / 'Sales order has more than 2 items ', doc.
        exit.
      else.
*        WRITE: / 'Sales order update error ', MESSTAB-MSGV2.
*        write: / MESSTAB-MSGID , MESSTAB-MSGnr. .
        LOOP AT MESSTAB.
          SELECT SINGLE * FROM T100 WHERE SPRSL = MESSTAB-MSGSPRA
                                    AND   ARBGB = MESSTAB-MSGID
                                    AND   MSGNR = MESSTAB-MSGNR.
          IF SY-SUBRC = 0.
            L_MSTRING = T100-TEXT.
            IF L_MSTRING CS '&1'.
              REPLACE '&1' WITH MESSTAB-MSGV1 INTO L_MSTRING.
              REPLACE '&2' WITH MESSTAB-MSGV2 INTO L_MSTRING.
              REPLACE '&3' WITH MESSTAB-MSGV3 INTO L_MSTRING.
              REPLACE '&4' WITH MESSTAB-MSGV4 INTO L_MSTRING.
            ELSE.
              REPLACE '&' WITH MESSTAB-MSGV1 INTO L_MSTRING.
              REPLACE '&' WITH MESSTAB-MSGV2 INTO L_MSTRING.
              REPLACE '&' WITH MESSTAB-MSGV3 INTO L_MSTRING.
              REPLACE '&' WITH MESSTAB-MSGV4 INTO L_MSTRING.
            ENDIF.
            CONDENSE L_MSTRING.
            l_len = strlen( L_MSTRING ).
            WRITE: / L_MSTRING(l_len), doc.
*            write: / MESSTAB-MSGID , MESSTAB-MSGnr.
          endif.
        ENDLOOP.
      endif.
      endif.
    ENDIF.
  ENDLOOP.
  REFRESH BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
enDFORM.
