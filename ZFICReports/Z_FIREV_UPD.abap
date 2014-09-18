*
* Update text field of document
*
* Andy Choi
*
REPORT Z_FIREV_UPD no standard page heading line-size 255.

*** End generated data section ***
TABLES :GLT0,BSIS,BKPF,T001,SKAT,BSEG,LFA1,SKA1,KNA1,SKB1.
DATA : IBSIS  LIKE BSIS OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF itab OCCURS 0,
         BELNR  LIKE  Bkpf-BELNR,
       END OF itab.

DATA : BEGIN OF ibseg OCCURS 0,
         buzei  like bseg-buzei,
         koart  like bseg-koart,
         UMSKZ  like bseg-UMSKZ,
         sgtxt  like bseg-sgtxt,
       END OF ibseg.

PARAMETERS : P_BUKRS LIKE BSIS-BUKRS memory id BUK,
             P_GJAHR(4) type c.
*             P_HKONT LIKE BSIS-HKONT OBLIGATORY.
SELECT-OPTIONS : S_BELNR FOR BSIS-BELNR,
                 S_BUDAT FOR BSIS-BUDAT.
* comments/Notes
selection-screen begin of block blk2 with frame title text-003.
selection-screen begin of line.
selection-screen comment  1(60) cmt1.
selection-screen end of line.
*selection-screen begin of line.
*selection-screen comment  1(60) cmt2.
*selection-screen end of line.
*selection-screen begin of line.
*selection-screen comment  1(60) cmt3.
*selection-screen end of line.
selection-screen end of block blk2.


parameters: p_run as checkbox.

* for BDC
***INCLUDE BDCRECX1.
*  for programs doing a data transfer by creating a batch-input
*  for programs doing a data transfer by CALL TRANSACTION USING
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS SESSION RADIOBUTTON GROUP CTU.  "create session
SELECTION-SCREEN COMMENT 3(20) S07 FOR FIELD SESSION.
selection-screen position 45.
PARAMETERS CTU     RADIOBUTTON GROUP  CTU.  "call transaction
SELECTION-SCREEN COMMENT 48(20) S08 FOR FIELD CTU.
SELECTION-SCREEN END OF LINE.

PARAMETERS GROUP(12) no-display.    "group name of session
* run-mode
* A: show all dynpros
* E: show dynpro on error only
* N: do not display dynpro
PARAMETERS CTUMODE LIKE CTU_PARAMS-DISMODE DEFAULT 'N'. " no-display.

* user for session in batch
PARAMETERS USER(12) DEFAULT SY-UNAME no-display.
PARAMETERS CUPDATE LIKE CTU_PARAMS-UPDMODE DEFAULT 'L' no-display.
"S: synchronously
"A: asynchronously
"L: local
PARAMETERS HOLDDATE LIKE SY-DATUM no-display.
* 'X' = keep   session if finished
PARAMETERS KEEP(1) TYPE C default ' ' no-display.
"' ' = delete session if finished
PARAMETERS NODATA DEFAULT '/' LOWER CASE no-display.          "nodata
* 'X' = no transaction logging
PARAMETERS SMALLLOG(1) TYPE C default ' ' no-display.

*----------------------------------------------------------------------*
*   data definition
*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
DATA:   BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
*       error session opened (' ' or 'X')
DATA:   E_GROUP_OPENED.
*       message texts
TABLES: T100.

data: l_sgtxt like bseg-sgtxt.

*---------------------------------------------------------------------*
* Initialization.
*---------------------------------------------------------------------*
Initialization.
  ctu = 'X'.
*  session = ' '.
  GROUP = 'BKPF-ReverseDocUpdate'.
  s07 = 'BDC Session'.
  s08 = 'Call Transaction'.

  cmt1 = 'This program update reversed document text field'.

*---------------------------------------------------------------------*
start-of-selection.

  PERFORM GET_BELNR.

  loop at itab.

    refresh ibseg.
    select buzei koart UMSKZ sgtxt from bseg
       into table ibseg
       where bukrs = p_bukrs
         and gjahr = p_gjahr
         and belnr = itab-belnr.

    loop at ibseg.
      check ibseg-sgtxt(4) <> 'Rev.'.

      l_sgtxt(4) = 'Rev.'.
      l_sgtxt+4(45) = ibseg-sgtxt(45).
      write:/ itab-belnr, '-->', l_sgtxt.

      if p_run = 'X'.
        perform update_doc.
      endif.

    endloop.
  endloop.
*  PERFORM GET_BSIS.  "G.L??


*&---------------------------------------------------------------------*
*&      Form  GET_BELNR
*&---------------------------------------------------------------------*
FORM GET_BELNR.

  SELECT BELNR INTO TABLE itab
    from bkpf
   WHERE BUKRS = P_BUKRS
     AND GJAHR = P_GJAHR
     AND BUDAT IN S_BUDAT
     AND BELNR IN S_BELNR
     AND STBLG <> SPACE.

ENDFORM.                    " GET_BELNR
*&---------------------------------------------------------------------*
*&      Form  update_doc
*&---------------------------------------------------------------------*
FORM update_doc.
  perform bdc_dynpro      using 'SAPMF05L' '0102'.
  perform bdc_field       using 'BDC_CURSOR'    'RF05L-BUZEI'.
  perform bdc_field       using 'BDC_OKCODE'    '/00'.
  perform bdc_field       using 'RF05L-BELNR'     itab-BELNR.
  perform bdc_field       using 'RF05L-BUKRS'     p_BUKRS.
  perform bdc_fld         using 'RF05L-GJAHR'     p_GJAHR.
  perform bdc_fld         using 'RF05L-BUZEI'     ibseg-BUZEI.

  if ibseg-koart = 'S'.
    perform bdc_dynpro      using 'SAPMF05L' '0300'.
    perform bdc_field       using 'BDC_CURSOR'    'BSEG-SGTXT'.
    perform bdc_field       using 'BDC_OKCODE'    '=AE'.
    perform bdc_field       using 'BSEG-SGTXT'      l_sgtxt.

    perform bdc_field       using 'DKACB-FMORE' 'X'.
    perform bdc_dynpro      using 'SAPLKACB' '0002'.
    perform bdc_field       using 'BDC_OKCODE'     '=ENTE'.
  elseif ibseg-UMSKZ <> space.  "special gl
    perform bdc_dynpro      using 'SAPMF05L' '0303'.
    perform bdc_field       using 'BDC_CURSOR'    'BSEG-SGTXT'.
    perform bdc_field       using 'BDC_OKCODE'    '=AE'.
    perform bdc_field       using 'BSEG-SGTXT'      l_sgtxt.
  elseif ibseg-koart = 'D'.  "vendor
    perform bdc_dynpro      using 'SAPMF05L' '0301'.
    perform bdc_field       using 'BDC_CURSOR'    'BSEG-SGTXT'.
    perform bdc_field       using 'BDC_OKCODE'    '=AE'.
    perform bdc_field       using 'BSEG-SGTXT'      l_sgtxt.
  elseif ibseg-koart = 'K'.  "vendor
    perform bdc_dynpro      using 'SAPMF05L' '0302'.
    perform bdc_field       using 'BDC_CURSOR'    'BSEG-SGTXT'.
    perform bdc_field       using 'BDC_OKCODE'    '=AE'.
    perform bdc_field       using 'BSEG-SGTXT'      l_sgtxt.
  elseif ibseg-koart = 'A'.  "asset
    perform bdc_dynpro      using 'SAPMF05L' '0305'.
    perform bdc_field       using 'BDC_CURSOR'    'BSEG-SGTXT'.
    perform bdc_field       using 'BDC_OKCODE'    '=AE'.
    perform bdc_field       using 'BSEG-SGTXT'      l_sgtxt.
  endif.
  perform bdc_transaction using 'FB09'.

ENDFORM.                    " update_doc

*---------------------------------------------------------------------*
*       FORM BDC_FLD                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FNAM                                                          *
*  -->  FVAL                                                          *
*---------------------------------------------------------------------*
FORM BDC_FLD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.
*----------------------------------------------------------------------*
*   open dataset                                                       *
*----------------------------------------------------------------------*
FORM OPEN_DATASET USING P_DATASET.
  OPEN DATASET P_DATASET IN TEXT MODE.
  IF SY-SUBRC <> 0.
    WRITE: / TEXT-E00, SY-SUBRC.
    STOP.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*   close dataset                                                      *
*----------------------------------------------------------------------*
FORM CLOSE_DATASET USING P_DATASET.
  CLOSE DATASET P_DATASET.
ENDFORM.

*----------------------------------------------------------------------*
*   create batchinput session                                          *
*   (not for call transaction using...)                                *
*----------------------------------------------------------------------*
FORM OPEN_GROUP.
  IF SESSION = 'X'.
    SKIP.
    WRITE: /(20) 'Create group'(I01), GROUP.
    SKIP.
*   open batchinput group
    CALL FUNCTION 'BDC_OPEN_GROUP'
         EXPORTING
              CLIENT   = SY-MANDT
              GROUP    = GROUP
              USER     = USER
              KEEP     = KEEP
              HOLDDATE = HOLDDATE.
    WRITE: /(30) 'BDC_OPEN_GROUP'(I02),
            (12) 'returncode:'(I05),
                 SY-SUBRC.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*   end batchinput session                                             *
*   (call transaction using...: error session)                         *
*----------------------------------------------------------------------*
FORM CLOSE_GROUP.
  IF SESSION = 'X'.
*   close batchinput group
    CALL FUNCTION 'BDC_CLOSE_GROUP'.
    WRITE: /(30) 'Close session',
            (12) 'Return code =',
                 SY-SUBRC.
  ELSE.
    IF E_GROUP_OPENED = 'X'.
      CALL FUNCTION 'BDC_CLOSE_GROUP'.
      WRITE: /.
      WRITE: /(30) 'Error session created'.
    ENDIF.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
FORM BDC_TRANSACTION USING TCODE.
  DATA: L_MSTRING(480).
  DATA: L_SUBRC LIKE SY-SUBRC.
* batch input session
  IF SESSION = 'X'.
    CALL FUNCTION 'BDC_INSERT'
         EXPORTING
              TCODE     = TCODE
         TABLES
              DYNPROTAB = BDCDATA.
    IF SMALLLOG <> 'X'.
      WRITE: / 'Insert transaction',
               TCODE,
               'Return code =',
               SY-SUBRC,
               'RECORD:',
               SY-INDEX.
    ENDIF.
* call transaction using
  ELSE.
    REFRESH MESSTAB.
    CALL TRANSACTION TCODE USING BDCDATA
                     MODE   CTUMODE
                     UPDATE CUPDATE
                     MESSAGES INTO MESSTAB.
    L_SUBRC = SY-SUBRC.
    IF SMALLLOG <> 'X'.
      format color COL_KEY.
      WRITE: / 'Return code =',
               L_SUBRC,
               'RECORD:',
               SY-INDEX.
      format color COL_NORMAL.
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
          WRITE: / MESSTAB-MSGTYP, L_MSTRING(150).
        ELSE.
          WRITE: / MESSTAB.
        ENDIF.
      ENDLOOP.
      SKIP.
    ENDIF.
** Erzeugen fehlermappe ************************************************
    IF L_SUBRC <> 0 AND GROUP <> SPACE.
      IF E_GROUP_OPENED = ' '.
        CALL FUNCTION 'BDC_OPEN_GROUP'
             EXPORTING
                  CLIENT   = SY-MANDT
                  GROUP    = GROUP
                  USER     = USER
                  KEEP     = KEEP
                  HOLDDATE = HOLDDATE.
        E_GROUP_OPENED = 'X'.
      ENDIF.
      CALL FUNCTION 'BDC_INSERT'
           EXPORTING
                TCODE     = TCODE
           TABLES
                DYNPROTAB = BDCDATA.
    ENDIF.
  ENDIF.
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
  IF FVAL <> NODATA.
    CLEAR BDCDATA.
    BDCDATA-FNAM = FNAM.
    BDCDATA-FVAL = FVAL.
    APPEND BDCDATA.
  ENDIF.
ENDFORM.
