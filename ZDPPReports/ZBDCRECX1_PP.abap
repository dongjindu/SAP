***INCLUDE zBDCRECX1_pp.
*  for programs doing a data transfer by creating a batch-input session
*  and
*  for programs doing a data transfer by CALL TRANSACTION USING

*SELECTION-SCREEN BEGIN OF LINE.
*  PARAMETERS SESSION RADIOBUTTON GROUP CTU.  "create session
*  SELECTION-SCREEN COMMENT 3(20) TEXT-S07 FOR FIELD SESSION.
*  selection-screen position 45.
*  PARAMETERS CTU RADIOBUTTON GROUP  CTU.     "call transaction
*  SELECTION-SCREEN COMMENT 48(20) TEXT-S08 FOR FIELD CTU.
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN BEGIN OF LINE.
*  SELECTION-SCREEN COMMENT 3(20) TEXT-S01 FOR FIELD GROUP.
*  selection-screen position 25.
*  PARAMETERS GROUP(12).                      "group name of session
*  SELECTION-SCREEN COMMENT 48(20) TEXT-S05 FOR FIELD CTUMODE.
*  selection-screen position 70.
*  PARAMETERS CTUMODE LIKE CTU_PARAMS-DISMODE DEFAULT 'N'.
*                                      "A: show all dynpros
*                                      "E: show dynpro on error only
*                                      "N: do not display dynpro
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN BEGIN OF LINE.
*  SELECTION-SCREEN COMMENT 3(20) TEXT-S02 FOR FIELD USER.
*  selection-screen position 25.
*  PARAMETERS: USER(12) DEFAULT SY-UNAME.     "user for session in batch
*  SELECTION-SCREEN COMMENT 48(20) TEXT-S06 FOR FIELD CUPDATE.
*  selection-screen position 70.
*  PARAMETERS CUPDATE LIKE CTU_PARAMS-UPDMODE DEFAULT 'L'.
*                                      "S: synchronously
*                                      "A: asynchronously
*                                      "L: local
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN BEGIN OF LINE.
*  SELECTION-SCREEN COMMENT 3(20) TEXT-S03 FOR FIELD KEEP.
*  selection-screen position 25.
*  PARAMETERS: KEEP AS CHECKBOX.       "' ' = delete session if finished
*                                      "'X' = keep   session if finished
*  SELECTION-SCREEN COMMENT 48(20) TEXT-S09 FOR FIELD E_GROUP.
*  selection-screen position 70.
*  parameters E_GROUP(12).             "group name of error-session
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN BEGIN OF LINE.
*  SELECTION-SCREEN COMMENT 3(20) TEXT-S04 FOR FIELD HOLDDATE.
*  selection-screen position 25.
*  PARAMETERS: HOLDDATE LIKE SY-DATUM.
*  SELECTION-SCREEN COMMENT 51(17) TEXT-S02 FOR FIELD E_USER.
*  selection-screen position 70.
*  PARAMETERS: E_USER(12) DEFAULT SY-UNAME.    "user for error-session
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN BEGIN OF LINE.
*  SELECTION-SCREEN COMMENT 51(17) TEXT-S03 FOR FIELD E_KEEP.
*  selection-screen position 70.
*  PARAMETERS: E_KEEP AS CHECKBOX.     "' ' = delete session if finished
*                                      "'X' = keep   session if finished
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN BEGIN OF LINE.
*  SELECTION-SCREEN COMMENT 51(17) TEXT-S04 FOR FIELD E_HDATE.
*  selection-screen position 70.
*  PARAMETERS: E_HDATE LIKE SY-DATUM.
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN SKIP.
*
*SELECTION-SCREEN BEGIN OF LINE.
*  SELECTION-SCREEN COMMENT 1(33) TEXT-S10 FOR FIELD NODATA.
*  PARAMETERS: NODATA DEFAULT '/' LOWER CASE.          "nodata
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN BEGIN OF LINE.
*  SELECTION-SCREEN COMMENT 1(33) FOR FIELD SMALLLOG.
*  PARAMETERS: SMALLLOG as checkbox.  "' ' = log all transactions
*                                     "'X' = no transaction logging
*SELECTION-SCREEN END OF LINE.

*----------------------------------------------------------------------*
*   data definition
*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
DATA:   bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
*       error session opened (' ' or 'X')
DATA:   e_group_opened.
DATA:   group(12),                      "group name of session
        user(12),
        keep(1) VALUE 'X',
        e_group(12),
        nodata VALUE '/'.
*       message texts
TABLES: t100, ctu_params.


*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
*AT SELECTION-SCREEN.
** group and user must be filled for create session
*  IF SESSION = 'X' AND
*     GROUP = SPACE OR USER = SPACE.
*    MESSAGE E613(MS).
*  ENDIF.
*
*----------------------------------------------------------------------*
*   open dataset                                                       *
*----------------------------------------------------------------------*
FORM open_dataset USING p_dataset.
  OPEN DATASET p_dataset IN TEXT MODE.
  IF sy-subrc <> 0.
    WRITE: / text-e00, sy-subrc.
    STOP.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*   close dataset                                                      *
*----------------------------------------------------------------------*
FORM close_dataset USING p_dataset.
  CLOSE DATASET p_dataset.
ENDFORM.

*----------------------------------------------------------------------*
*   create batchinput session                                          *
*   (not for call transaction using...)                                *
*----------------------------------------------------------------------*
FORM open_group.
  IF session = 'X'.
    SKIP.
    WRITE: /(20) 'Create group'(i01), group.
    SKIP.
*   open batchinput group
    CALL FUNCTION 'BDC_OPEN_GROUP'
         EXPORTING
              client = sy-mandt
              group  = group
              user   = user
              keep   = keep.
*                    HOLDDATE = HOLDDATE.
    WRITE: /(30) 'BDC_OPEN_GROUP'(i02),
            (12) 'returncode:'(i05),
                 sy-subrc.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*   end batchinput session                                             *
*   (call transaction using...: error session)                         *
*----------------------------------------------------------------------*
FORM close_group.
  IF session = 'X'.
*   close batchinput group
    CALL FUNCTION 'BDC_CLOSE_GROUP'.
    WRITE: /(30) 'BDC_CLOSE_GROUP'(i04),
            (12) 'returncode:'(i05),
                 sy-subrc.
  ELSE.
    IF e_group_opened = 'X'.
      CALL FUNCTION 'BDC_CLOSE_GROUP'.
      WRITE: /.
      WRITE: /(30) 'Fehlermappe wurde erzeugt'(i06).
      e_group_opened = ' '.
    ENDIF.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
FORM bdc_transaction USING tcode.
  DATA: l_mstring(480).
  DATA: l_subrc LIKE sy-subrc.
  DATA: ctumode LIKE ctu_params-dismode VALUE 'N'.
  DATA: cupdate LIKE ctu_params-updmode VALUE 'L'.
  DATA: smalllog(1).  "' ' = log all transactions
  "'X' = no transaction logging

* batch input session
  IF session = 'X'.
    CALL FUNCTION 'BDC_INSERT'
         EXPORTING
              tcode     = tcode
         TABLES
              dynprotab = bdcdata.
    IF smalllog <> 'X'.
      WRITE: / 'BDC_INSERT'(i03),
               tcode,
               'returncode:'(i05),
               sy-subrc,
               'RECORD:',
               sy-index.
    ENDIF.
* call transaction using
  ELSE.
    REFRESH messtab.
    CALL TRANSACTION tcode USING bdcdata
                     MODE   ctumode
                     UPDATE cupdate
                     MESSAGES INTO messtab.
    l_subrc = sy-subrc.
*    IF SMALLLOG <> 'X'.
*      WRITE: / 'CALL_TRANSACTION',
*               TCODE,
*               'returncode:'(I05),
*               L_SUBRC,
*               'RECORD:',
*               SY-INDEX.
*      LOOP AT MESSTAB.
*        SELECT SINGLE * FROM T100 WHERE SPRSL = MESSTAB-MSGSPRA
*                                  AND   ARBGB = MESSTAB-MSGID
*                                  AND   MSGNR = MESSTAB-MSGNR.
*        IF SY-SUBRC = 0.
*          L_MSTRING = T100-TEXT.
*          IF L_MSTRING CS '&1'.
*            REPLACE '&1' WITH MESSTAB-MSGV1 INTO L_MSTRING.
*            REPLACE '&2' WITH MESSTAB-MSGV2 INTO L_MSTRING.
*            REPLACE '&3' WITH MESSTAB-MSGV3 INTO L_MSTRING.
*            REPLACE '&4' WITH MESSTAB-MSGV4 INTO L_MSTRING.
*          ELSE.
*            REPLACE '&' WITH MESSTAB-MSGV1 INTO L_MSTRING.
*            REPLACE '&' WITH MESSTAB-MSGV2 INTO L_MSTRING.
*            REPLACE '&' WITH MESSTAB-MSGV3 INTO L_MSTRING.
*            REPLACE '&' WITH MESSTAB-MSGV4 INTO L_MSTRING.
*          ENDIF.
*          CONDENSE L_MSTRING.
*          WRITE: / MESSTAB-MSGTYP, L_MSTRING(250).
*        ELSE.
*          WRITE: / MESSTAB.
*        ENDIF.
*      ENDLOOP.
*      SKIP.
*    ENDIF.
** Erzeugen fehlermappe ************************************************
    IF l_subrc <> 0 AND e_group <> space.
      IF e_group_opened = ' '.
        CALL FUNCTION 'BDC_OPEN_GROUP'
             EXPORTING  client   = sy-mandt
                        group    = e_group
                        user     = user
                        keep     = keep
*                        HOLDDATE = E_HDATE.
         e_group_opened = 'X'.
      ENDIF.
      CALL FUNCTION 'BDC_INSERT'
           EXPORTING
                tcode     = tcode
           TABLES
                dynprotab = bdcdata.
    ENDIF.
  ENDIF.
  REFRESH bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  IF fval <> nodata.
    CLEAR bdcdata.
    bdcdata-fnam = fnam.
    bdcdata-fval = fval.
    APPEND bdcdata.
  ENDIF.
ENDFORM.
