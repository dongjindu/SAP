*----------------------------------------------------------------------*
*   INCLUDE ZRMMGM01R_6005BDCREC                                       *
*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
DATA:   bdcdata LIKE bdcdata        OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   messtab LIKE bdcmsgcoll     OCCURS 0 WITH HEADER LINE.
*       error session opened (' ' or 'X')
***
DATA:   tmp_messtab LIKE zbdcmsgcoll OCCURS 0 WITH HEADER LINE.
*****
DATA:   e_group_opened.
*       message texts
TABLES: t100.

*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
FORM bdc_transaction USING tcode.
*MODE
*'A' Display screen
*'E' Display only if an error occurs
*'N' Do not display
*'P' Do not display; debugging possible
  DATA: l_mstring(480).
  DATA: lv_subrc LIKE sy-subrc.
  REFRESH messtab.
  DATA: mode.
    mode = 'N'.
*    mode = 'A'.
  CALL TRANSACTION tcode USING bdcdata
                   MODE   mode
*                   MODE   'N'
                   UPDATE 'S'
                   MESSAGES INTO messtab.

  lv_subrc = sy-subrc.
  w_subrc = lv_subrc.
******Catch System Message (Doc No etc.)
  IF tcode = 'ME11'. "Create Info Record
    IF lv_subrc = 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      MOVE sy-msgv1 TO ztqesr-belnr.   "ME11ÀÇ DOC NO.
** To return to calling program's display mode
*      SET PARAMETER ID 'ZBLN_FB01' FIELD ztqesr-belnr.
    ENDIF.
*  Pass Success/Failure of ME11 bdc to calling program.
    EXPORT lv_subrc TO MEMORY ID 'LV_SUBRC'.
  ELSEIF tcode = 'ME12'.  "Change Info Record
    IF lv_subrc = 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      MOVE sy-msgv1 TO ztqesr-belnr.   "ME12's DOC NO.
** To return to calling program's display mode
*      SET PARAMETER ID 'ZBLN_FB01' FIELD ztqesr-belnr.
    ENDIF.
*  Pass Success/Failure of ME11 bdc to calling program.
    EXPORT lv_subrc TO MEMORY ID 'LV_SUBRC'.
  ENDIF.

****** To display log....
*/If you don't view the log, then don't comment below 4 lines.
  WRITE: / 'CALL_TRANSACTION',
         tcode,
         'Return code ='(i05),
         lv_subrc.

  LOOP AT messtab.
    SELECT SINGLE * FROM t100 WHERE sprsl = messtab-msgspra
                              AND   arbgb = messtab-msgid
                              AND   msgnr = messtab-msgnr.
    IF sy-subrc = 0.
      l_mstring = t100-text.
      IF l_mstring CS '&1'.
        REPLACE '&1' WITH messtab-msgv1 INTO l_mstring.
        REPLACE '&2' WITH messtab-msgv2 INTO l_mstring.
        REPLACE '&3' WITH messtab-msgv3 INTO l_mstring.
        REPLACE '&4' WITH messtab-msgv4 INTO l_mstring.
      ELSE.
        REPLACE '&' WITH messtab-msgv1 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv2 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv3 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv4 INTO l_mstring.
      ENDIF.
      CONDENSE l_mstring.
*/If you don't view the log, then don't comment below 4 lines.
      WRITE: / messtab-msgtyp, l_mstring(250).
    ELSE.
      WRITE: / messtab.
    ENDIF.
  ENDLOOP.

*  PERFORM make_tmp_messtab.         "To Save log...
  REFRESH bdcdata.
ENDFORM.                    "BDC_TRANSACTION

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "BDC_FIELD
