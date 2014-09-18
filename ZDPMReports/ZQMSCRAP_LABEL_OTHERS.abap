REPORT ZQMSCRAP_LABEL.
TABLES: BDCMSGCOLL, MARA,QMEL,MAKT, QMFE, QPCT.
*
*-----------------------------------------------------------------*
* Date         Request      Developer    Description
* 05/08/2007   UD1K940504   Manju        change work center
*                                        names to new scrap table names
*------------------------------------------------------------------*
*
*DATA: BEGIN OF IT_QMNUM   OCCURS 0,
*      QMNUM LIKE  BDCMSGCOLL-MSGV1,
*      matnr like mara-matnr,
*      END OF IT_QMNUM.
*

DATA: BEGIN OF IT_QMNUM OCCURS 0,
      QMNUM LIKE  BDCMSGCOLL-MSGV1,
      MATNR LIKE MARA-MATNR,
      ZERDAT LIKE QMEL-ERDAT,
      MAKTX LIKE MAKT-MAKTX,
      MATKL LIKE MARA-MATKL,
      ZMEINS LIKE MARA-MEINS,
      RKMNG LIKE QMEL-RKMNG,
      ZREASON LIKE QPCT-KURZTEXT,
      OTGRP LIKE QMFE-OTGRP,
      TXTCDUR LIKE QPCT-KURZTEXT,
      ZROOTWC LIKE QPCT-KURZTEXT,
      PDFBC(250) TYPE C,
      FLAG(1),
      END OF IT_QMNUM.

DATA: ZZRKMNG LIKE QMEL-RKMNG,
      ZZERDAT LIKE QMEL-ERDAT,
      ZZMEINS LIKE MARA-MEINS,
      ZZMATKL LIKE MARA-MATKL,
      ZZMAKTX LIKE MAKT-MAKTX.
DATA: ZZCODE LIKE VIQMFE-FECOD,
      ZZROOTWC LIKE VIQMFE-OTEIL,
      ZZTXTCDUR LIKE VIQMUR-URCOD.
DATA: ZQMNUM LIKE QMEL-QMNUM.
DATA : ZOPTIONS LIKE	ITCPO OCCURS 0 WITH HEADER LINE.
DATA: ZQTY(13) TYPE C.
DATA: ZQMCOD LIKE QMEL-QMCOD.
DATA: ZPRINTER(4).
*DATA: BEGIN OF LT_MESS OCCURS 5,
*      MESS(255),
*      END OF LT_MESS.
*DATA: L_MESS LIKE LT_MESS.
DATA: L_MESS(255),
      W_CALL(4).

*SELECT-OPTIONS: QMNUM FOR QMEL-QMNUM MEMORY ID IQM.
SELECT-OPTIONS: MATNR FOR MARA-MATNR MEMORY ID MAT.

IMPORT IT_QMNUM FROM MEMORY ID 'M2'.

LOOP AT IT_QMNUM.
*  CLEAR ZQMNUM.
*  CLEAR: IT_QMNUM-RKMNG.
  ZQMNUM = IT_QMNUM-QMNUM.
  W_CALL = IT_QMNUM-OTGRP.
*  CONCATENATE '00' ZQMNUM INTO ZQMNUM.
*
*  SELECT SINGLE RKMNG ERDAT QMCOD INTO
*  (IT_QMNUM-RKMNG,IT_QMNUM-ZERDAT, ZQMCOD)
*   FROM QMEL
*  WHERE QMNUM = ZQMNUM.

  ZQMCOD = IT_QMNUM-MAKTX.
  SELECT SINGLE MEINS MATKL INTO (IT_QMNUM-ZMEINS, IT_QMNUM-MATKL)
  FROM MARA WHERE
     MATNR = IT_QMNUM-MATNR.

  SELECT SINGLE MAKTX INTO (IT_QMNUM-MAKTX) FROM MAKT WHERE
     MATNR = IT_QMNUM-MATNR.

  SELECT SINGLE GRTXT INTO IT_QMNUM-ZREASON
    FROM T157E
    WHERE SPRAS ='EN'
      AND BWART ='201'
      AND GRUND = IT_QMNUM-ZREASON.

*** To select the text for Reason and Root cause dept
*  SELECT SINGLE FECOD OTEIL INTO (ZZCODE,ZZROOTWC)
*   FROM VIQMFE
*  WHERE QMNUM = ZQMNUM.
*
*  SELECT SINGLE KURZTEXT INTO (IT_QMNUM-ZREASON)
*   FROM QPCT
*  WHERE KATALOGART = 'W'
*  AND CODE = ZZCODE.
*
*  SELECT SINGLE KURZTEXT INTO (IT_QMNUM-ZROOTWC)
*   FROM QPCT
*  WHERE KATALOGART = 'V'
*  AND CODE = ZZROOTWC.
*
**** To select the text for Cause
*  SELECT SINGLE URCOD INTO (ZZTXTCDUR)
*   FROM VIQMUR
*  WHERE QMNUM = ZQMNUM.
*
  SELECT SINGLE KURZTEXT INTO (IT_QMNUM-TXTCDUR)
   FROM QPcT
  WHERE KATALOGART = 'V'
  AND CODE = ZQMCOD.

*** Add Barcode

*Concatenate these texts
  ZQTY    = IT_QMNUM-RKMNG.

  CONDENSE ZQTY NO-GAPS.
  CONCATENATE '[)>*06:ZQM' IT_QMNUM-QMNUM ':P' IT_QMNUM-MATNR
  ':7Q' ZQTY IT_QMNUM-ZMEINS '*' INTO IT_QMNUM-PDFBC.

  MODIFY IT_QMNUM.
ENDLOOP.


MOVE '1' TO ZOPTIONS-TDCOPIES.
*move zqmnum  to zoptions-TDDATASET.     "UD1K940504
* Move last 6 characters
MOVE ZQMNUM+6(6)  TO ZOPTIONS-TDDATASET.                    "UD1K940504
MOVE 'X' TO ZOPTIONS-TDIMMED.
MOVE 'X' TO ZOPTIONS-TDNEWID.

*move 'Z4M' to zoptions-tddest.
*concatenate 'Z' zqmcod+0(1) zqmcod+2(2) into zprinter.  "UD1K940504
ZPRINTER = ZQMCOD.

MOVE ZPRINTER TO ZOPTIONS-TDDEST.

** for test only **
*zPRINTER = 'RFL'.
*MOVE ZPRINTER TO ZOPTIONS-TDDEST.
** end of test

APPEND ZOPTIONS.


CALL FUNCTION 'OPEN_FORM'
 EXPORTING
   DEVICE                            = 'PRINTER'
   DIALOG                            = ' '
*   FORM                              = 'ZQM_SCRAP_NOTI'
   FORM                              = 'ZQM_SCRAP_OTHERS'

   LANGUAGE                          = SY-LANGU
    OPTIONS                           = ZOPTIONS
*   MAIL_SENDER                       =
*   MAIL_RECIPIENT                    =
*   MAIL_APPL_OBJECT                  =
   RAW_DATA_INTERFACE                = '*'
* IMPORTING
*   LANGUAGE                          =
*   NEW_ARCHIVE_PARAMS                =
*   RESULT                            =
 EXCEPTIONS
   CANCELED                          = 1
   DEVICE                            = 2
   FORM                              = 3
   OPTIONS                           = 4
   UNCLOSED                          = 5
   MAIL_OPTIONS                      = 6
   ARCHIVE_ERROR                     = 7
   INVALID_FAX_NUMBER                = 8
   MORE_PARAMS_NEEDED_IN_BATCH       = 9
   SPOOL_ERROR                       = 10
   OTHERS                            = 11
          .
IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            MSGID               = SY-MSGID
            MSGNR               = SY-MSGNO
            MSGV1               = SY-MSGV1
            MSGV2               = SY-MSGV2
            MSGV3               = SY-MSGV3
            MSGV4               = SY-MSGV4
       IMPORTING
            MESSAGE_TEXT_OUTPUT = L_MESS.

  EXPORT L_MESS TO MEMORY ID 'ME'.
  EXIT.
ENDIF.

*import it_qmnum from memory id 'M1'.
*
*loop at it_qmnum.
*clear zqmnum.
*clear: it_qmnum-rkmng.
*zqmnum = it_qmnum-qmnum.
*
*concatenate '00' zqmnum into zqmnum.
*
*select single rkmng erdat into (it_qmnum-rkmng,it_qmnum-zerdat)
* from qmel
*where qmnum = zqmnum.
*
*select single meins matkl into (it_qmnum-zmeins, it_qmnum-matkl)
*from mara where
*   matnr = it_qmnum-matnr.
*
*select single maktx into (it_qmnum-maktx) from makt where
*   matnr = it_qmnum-matnr.
*modify it_qmnum.
*endloop.


LOOP AT IT_QMNUM.
  CALL FUNCTION 'WRITE_FORM'
   EXPORTING
     ELEMENT                        = 'PRINT_LABEL'
     FUNCTION                       = 'SET'
     TYPE                           = 'BODY'
     WINDOW                         = 'MAIN'
* IMPORTING
*   PENDING_LINES                  =
   EXCEPTIONS
     ELEMENT                        = 1
     FUNCTION                       = 2
     TYPE                           = 3
     UNOPENED                       = 4
     UNSTARTED                      = 5
     WINDOW                         = 6
     BAD_PAGEFORMAT_FOR_PRINT       = 7
     SPOOL_ERROR                    = 8
     OTHERS                         = 9
           .

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              MSGID               = SY-MSGID
              MSGNR               = SY-MSGNO
              MSGV1               = SY-MSGV1
              MSGV2               = SY-MSGV2
              MSGV3               = SY-MSGV3
              MSGV4               = SY-MSGV4
         IMPORTING
              MESSAGE_TEXT_OUTPUT = L_MESS.
    EXPORT L_MESS TO MEMORY ID 'ME'.
  ENDIF.
ENDLOOP.
CALL FUNCTION 'CLOSE_FORM'
* IMPORTING
*   RESULT                         =
*   RDI_RESULT                     =
* TABLES
*   OTFDATA                        =
 EXCEPTIONS
   UNOPENED                       = 1
   BAD_PAGEFORMAT_FOR_PRINT       = 2
   SEND_ERROR                     = 3
   SPOOL_ERROR                    = 4
   OTHERS                         = 5
          .
IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            MSGID               = SY-MSGID
            MSGNR               = SY-MSGNO
            MSGV1               = SY-MSGV1
            MSGV2               = SY-MSGV2
            MSGV3               = SY-MSGV3
            MSGV4               = SY-MSGV4
       IMPORTING
            MESSAGE_TEXT_OUTPUT = L_MESS.
  EXPORT L_MESS TO MEMORY ID 'ME'.

ENDIF.
