*  (c) Copyright 1998 Conversion Sciences, Inc.
*  Version 1.0 - December 1998
REPORT ZPPA023546C MESSAGE-ID ZP.
*
* Internal table declaration for reading the source data
* Field Perid added - Mrudula.
* BDC OKCODE changed - Mrudula.
*
* Indiv. infotype program specifically developed - Hemang
* form in ZPEEMAIN46B doesn't handle steploop properly.
* Aug 9 99

DATA: BEGIN OF _PA0235 OCCURS 0.
        INCLUDE STRUCTURE PA0235.
DATA: PERID(20),TAXLV(1),TAXTY(2),FRMNR(1),EXMPT(1),BEGDA1(10),
                                       ENDDA1(10).
DATA: END OF _PA0235.


DATA: BEGIN OF BDC_DATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_DATA.

DATA: DELIMITER TYPE X  VALUE '09'.
DATA  SSN(13).
DATA: CNT1 TYPE I.

*PARAMETER : FILE0032 LIKE RLGRAP-FILENAME
*                      DEFAULT 'F:\WINDOW95\CSI\INFO\TXT\0032.TXT' .

PARAMETER : FILE0235 LIKE RLGRAP-FILENAME
                      DEFAULT 'C:\WINDOWS\SAP\0235.TXT' .
*parameter : pmaxrows(2) type n default 09.
* Source Code

CNT1 = 0.
PERFORM INIT_BDC USING 'HRPA0235' SY-UNAME.
PERFORM UPLOAD_235 USING FILE0235 'Error'.
PERFORM POPULATE_BDC.
PERFORM CLOSE_PROGRAM.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_0235
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM UPLOAD_235 USING F ERR.
  DATA: BEGIN OF ITAB OCCURS 0,
  FILE1(65535),
   END OF ITAB.
  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            FILENAME      = F
            FILETYPE      = 'ASC'
       TABLES
            DATA_TAB      = ITAB
       EXCEPTIONS
            UNKNOWN_ERROR = 7
            OTHERS        = 8.
  PERFORM CHECK_ERROR USING SY-SUBRC ERR.
  DATA T.
  LOOP AT ITAB.
    SPLIT ITAB-FILE1 AT DELIMITER INTO
 _PA0235-PERID _PA0235-BEGDA1 _PA0235-ENDDA1
               _PA0235-TAURT _PA0235-TAXLV _PA0235-TAXTY _PA0235-FRMNR
 _PA0235-EXMPT T.
  CONCATENATE _PA0235-BEGDA1+6(4) _PA0235-BEGDA1+0(2)
              _PA0235-BEGDA1+3(2) INTO _PA0235-BEGDA.

  CONCATENATE _PA0235-ENDDA1+6(4) _PA0235-ENDDA1+0(2)
              _PA0235-ENDDA1+3(2) INTO _PA0235-ENDDA.

 if _PA0235-PERID ne space.
    MOVE  _PA0235-PERID TO _PA0235-PERNR.
    APPEND _PA0235.
 endif.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_SUBRC  text                                             *
*      -->P_ERR  text                                                  *
*----------------------------------------------------------------------*
FORM CHECK_ERROR USING ERR_CD STAGE.
  CASE ERR_CD.
    WHEN 0.
    WHEN OTHERS.
*      write:/ 'Error in the process ', stage, '. Error -', err_cd.
      STOP.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POPULATE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POPULATE_BDC.
DATA : CNT235(2) TYPE N, CNT235NXT(2) TYPE N, CURRENTPG(2) TYPE N.
DATA : LASTPERID LIKE _PA0235-PERID.
DATA : LASTTAURT LIKE _PA0235-TAURT.


LOOP AT _Pa0235.

 CLEAR SSN. CONCATENATE '=c..' _Pa0235-PERID INTO SSN.

* find loop index for the taxtype
 CLEAR CNT235.
 perform findloopidx using _pa0235-taurt
                           _pa0235-taxty
                           _PA0235-BEGDA
                           _PA0235-ENDDA
                     CHANGING cnt235.
 IF CNT235 EQ 0 AND _PA0235-TAURT NE SPACE .
   WRITE : / 'Error. Missing entry int table T5UTD'
                  , _PA0235-TAURT, _PA0235-TAXTY.
 ENDIF.

* New 235 transaction ( change in SSN or Tax authority)
  IF LASTPERID NE _PA0235-PERID OR LASTTAURT NE _PA0235-TAURT.
* Insert last transaction in BDC
 IF LASTPERID NE SPACE.
 PERFORM INSERT_BDC TABLES BDC_DATA USING 'PA30'.
 CLEAR : CURRENTPG, CNT235NXT.
 ENDIF.
*      IF CNT235 EQ 1.
        PERFORM DYNPRO TABLES BDC_DATA USING:

                            'X' 'SAPMP50A' '1000' ' ',
                            ' ' 'rp50g-pernr' SSN ' ',
                            ' ' 'rp50g-choic' '0235' ' ',
                            ' ' 'RP50G-SUBTY' _PA0235-TAURT ' ',
                            ' ' 'BDC_OKCODE' '/05' ' ',
                            'X' 'MP023500' '2000' ' ',
                        ' ' 'p0235-begda' _PA0235-BEGDA1 ' ',
                        ' ' 'p0235-endda' _PA0235-ENDDA1 ' ',
                         ' ' 'Q0235-TAXTY' _PA0235-TAXTY CNT235 ,
                        ' ' 'Q0235-FRMNR' _PA0235-FRMNR CNT235 ,
                        ' ' 'Q0235-EXMPT' _PA0235-EXMPT CNT235 ,
                            ' ' 'BDC_OKCODE' '/11' ' '.
  ELSE. " next texttype for current employee and tax authority.
* Find out if pgdown needed
   IF CNT235 > 9.
     CNT235NXT = CNT235 - 9.
     IF CNT235NXT > CURRENTPG.  " Needs pagedown
        PERFORM DYNPRO TABLES BDC_DATA USING :
                            ' ' 'BDC_OKCODE' 'P+' ' ',  " Pagedown
                           'X' 'MP023500' '2000' ' '.
       CURRENTPG = CURRENTPG + 9.
      ENDIF.                  " cnt235nxt > currentpg
      CNT235 = CNT235NXT .
     ENDIF.                   " cnt235 > 9

        PERFORM DYNPRO TABLES BDC_DATA USING :
                        ' ' 'Q0235-TAXTY' _PA0235-TAXTY CNT235 ,
                        ' ' 'Q0235-FRMNR' _PA0235-FRMNR CNT235 ,
                        ' ' 'Q0235-EXMPT' _PA0235-EXMPT CNT235 ,
                        ' ' 'BDC_OKCODE' '/11' ' '.
 ENDIF.

 lastperid = _pa0235-perid.
 LASTTAURT = _PA0235-TAURT.

ENDLOOP.
* Insert  last transaction in BDC
 PERFORM INSERT_BDC TABLES BDC_DATA USING 'PA30'.

ENDFORM.                    " POPULATE_BDC

*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BDC_DATA  text                                             *
*      -->P_0153   text                                                *
*      -->P_0154   text                                                *
*      -->P_0155   text                                                *
*      -->P_0156   text                                                *
*----------------------------------------------------------------------*
FORM DYNPRO TABLES BDC_DATA STRUCTURE BDCDATA
           USING  DYNBEGIN NAME VALUE IDX.
 IF DYNBEGIN = 'X'.
   CLEAR   BDC_DATA.
   BDC_DATA-PROGRAM = NAME.
   BDC_DATA-DYNPRO = VALUE.
   BDC_DATA-DYNBEGIN = 'X'.
   APPEND BDC_DATA.
 ELSE.
   CLEAR   BDC_DATA.
   IF IDX = ' '.
     BDC_DATA-FNAM = NAME.
   ELSE.
     CONCATENATE NAME '(' IDX ')' INTO BDC_DATA-FNAM.
   ENDIF.
   BDC_DATA-FVAL = VALUE.
   APPEND BDC_DATA.
 ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INSERT_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BDC_DATA  text                                             *
*      -->P_0297   text                                                *
*----------------------------------------------------------------------*
FORM INSERT_BDC TABLES BTAB USING TRCD.
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE     = TRCD
       TABLES
            DYNPROTAB = BTAB.
  REFRESH BTAB.
  CLEAR BTAB.
  CNT1 = CNT1 + 1.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0044   text                                                *
*      -->P_SY_UNAME  text                                             *
*----------------------------------------------------------------------*
FORM INIT_BDC USING SES_NAME SAP_USER.

  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT = SY-MANDT
            GROUP  = SES_NAME
            USER   = SAP_USER.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CLOSE_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM CLOSE_PROGRAM.

  CALL FUNCTION 'BDC_CLOSE_GROUP'.
*  write:/ 'No. of transaction: ',cnt1.
*  write:/ ' BDC session created ' . , cnt2, 'documents.' .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FINDLOOPIDX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P__PA0235_TAURT  text                                        *
*      -->P__PA0235_TAXTY  text                                        *
*      <--P_CNT235  text                                               *
*----------------------------------------------------------------------*
FORM FINDLOOPIDX USING    PTAURT
                          PTAXTY
                          PBEGDA
                          PENDDA
                 CHANGING PCNT235.
TABLES: T5UTD.
DATA : CNT(2) TYPE N.

  CNT = 0.
  SELECT * FROM T5UTD WHERE TAXAU EQ PTAURT
                       AND BEGDA LE PBEGDA
                       AND ENDDA GE PBEGDA
                      ORDER BY PRIMARY KEY.
* Don't consider W/H tax
   CHECK T5UTD-TAXTY <> '01'.
* Increase counter
   CNT = CNT + 1.
* If desired tax type then set pcnt235.
  IF T5UTD-TAXTY = PTAXTY.  PCNT235 = CNT. ENDIF.

 ENDSELECT.


ENDFORM.                    " FINDLOOPIDX
