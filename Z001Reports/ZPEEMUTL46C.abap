* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Conversion Employee Main Load
* Include:  ZPEEMUTL46C
* Version 1.0  - August 2000

* Utility forms for BDC Programs.
* This include has the utilities generally required to write a BDC prog.

* Authors : Mrudula - Annance Consulting
* Modified by Mrudula Patel - June 1999.
* Modified By Mrudula Patel - Dec 1999.
* Modified By Sunil Dandekar- March 2000.
* Modified By Mrudula Patel - April 2000.
* Modified By Mrudula Patel - ADDED INFO-0379 - OCTOBER 2000.
*----------------------------------------------------------------------*
*   INCLUDE ZPEEMUTL46C                       *
*----------------------------------------------------------------------*

FORM GET_FNAME CHANGING F.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            DEF_FILENAME     = 'P'
            DEF_PATH         = 'c:\'
            MASK             = ',*.*,*.*.'
            MODE             = 'O'
            TITLE            = 'Select  Folder'
       IMPORTING
            FILENAME         = F
       EXCEPTIONS
            INV_WINSYS       = 01
            NO_BATCH         = 02
            SELECTION_CANCEL = 03
            SELECTION_ERROR  = 04.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM UPLOAD_01                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_01 USING F ERR.

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
  DATA T(2) TYPE N.
  LOOP AT ITAB.
    SPLIT ITAB-FILE1 AT DELIMITER INTO
 _PA0001-SSN _PA0001-BEGDA1
_PA0001-ENDDA1 _PA0001-PERNR1 _PA0001-WERKS _PA0001-PERSG _PA0001-PERSK
               _PA0001-KOSTL _PA0001-BTRTL _PA0001-GSBER _PA0001-ABKRS
 _PA0001-ANSVH _PA0001-PLANS _PA0001-STELL _PA0001-EXMPT _PA0001-PROZT
 _PA0001-ORGEH _PA0001-MSTBR _PA0001-VDSK1 _PA0001-SACHP _PA0001-SACHZ
 _PA0001-SACHA _PA0001-MASSG _PA0001-RFPNR T.
    T = STRLEN( _PA0001-SSN ) .
    IF T = 9.
      MOVE _PA0001-SSN TO _PA0001-PERNR.
      APPEND _PA0001.
    ENDIF.
  ENDLOOP.
ENDFORM.



*---------------------------------------------------------------------*
*       FORM UPLOAD_2776DATA                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_2776DATA USING F ERR.

  DATA: BEGIN OF ITAB OCCURS 0,
  FILE1(65535),
   END OF ITAB.



  CALL FUNCTION 'WS_UPLOAD'
      EXPORTING
*         CODEPAGE                = ' '
           FILENAME                = F
           FILETYPE                = 'ASC'
*         HEADLEN                 = ' '
*         LINE_EXIT               = ' '
*         TRUNCLEN                = ' '
*         USER_FORM               = ' '
*         USER_PROG               = ' '
*   importing
*         FILELENGTH              =
       TABLES
            DATA_TAB                = ITAB
       EXCEPTIONS
            UNKNOWN_ERROR = 7
            OTHERS        = 8.
  PERFORM CHECK_ERROR USING SY-SUBRC ERR.


  DATA T(9).

  LOOP AT ITAB.
    SPLIT ITAB-FILE1 AT DELIMITER INTO

 _PA0002-PERID _PA0002-BEGDA1 _PA0002-ENDDA1 _PA0002-ANREX _PA0002-NACHN
 _PA0002-VORNA _PA0002-MIDNM _PA0002-TITL2  _PA0002-NAMZU _PA0002-ENAME
 _PA0002-KNZNM _PA0002-NAME2 _PA0002-INITS _PA0002-RUFNM _PA0002-GBDAT1
 _PA0002-SPRSL _PA0002-NATIO _PA0002-FATXT _PA0002-FAMDT _PA0002-GESCH
 _PA0002-NATI2 _PA0002-NATI3 _PA0002-ANZKD1

 _PA0077-RACKY _PA0077-MILSA _PA0077-MEDIC _PA0077-EEOEX _PA0077-VETS2
_PA0077-VETS3  _PA0077-VETS1 _PA0077-VETS4 _PA0077-DISAB _PA0077-DISDT
_PA0077-DISLE

 _PA0006-ANSSA _PA0006-NAME2  _PA0006-STRAS _PA0006-LOCAT _PA0006-ORT01
 _PA0006-STATE _PA0006-PSTLZ _PA0006-LAND1 _PA0006-AREAC _PA0006-TELNR
 _PA0006-COM01 _PA0006-AREA1 _PA0006-TELN1 _PA0006-COM02 _PA0006-AREA2
 _PA0006-TELN2 _PA0006-COM03 _PA0006-AREA3 _PA0006-TELN3
 _PA0006-COM04 _PA0006-AREA4 _PA0006-TELN4 T.
    T = STRLEN( _PA0002-PERID ) .
    IF T = 9.
      MOVE   _PA0002-PERID TO _PA0002-PERNR.
      MOVE   _PA0002-PERID TO _PA0077-PERNR.
      MOVE   _PA0002-PERID TO _PA0006-PERNR.
      APPEND _PA0002.
      IF _PA0077-RACKY NE SPACE.
      APPEND _PA0077.
      ENDIF.
      MOVE   _PA0002-BEGDA1 TO _PA0006-BEGDA1.
      MOVE   _PA0002-ENDDA1 TO _PA0006-ENDDA1.
      IF _PA0006-STATE NE SPACE.
      APPEND _PA0006.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM UPLOAD_7DATA                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  _PA0007                                                       *
*                                                                     *
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_7DATA  USING F ERR.

  DATA: BEGIN OF ITAB OCCURS 0,
  FILE1(65535),
   END OF ITAB.

  CALL FUNCTION 'WS_UPLOAD'
      EXPORTING
*         CODEPAGE                = ' '
           FILENAME                = F
           FILETYPE                = 'ASC'
*         HEADLEN                 = ' '
*         LINE_EXIT               = ' '
*         TRUNCLEN                = ' '
*         USER_FORM               = ' '
*         USER_PROG               = ' '
*   importing
*         FILELENGTH              =
       TABLES
            DATA_TAB                = ITAB
       EXCEPTIONS
            UNKNOWN_ERROR = 7
            OTHERS        = 8.
  PERFORM CHECK_ERROR USING SY-SUBRC ERR.
  DATA T(3).
  LOOP AT ITAB.
    SPLIT ITAB-FILE1 AT DELIMITER INTO
_PA0007-PERID _PA0007-BEGDA1 _PA0007-ENDDA1 _PA0007-SCHKZ _PA0007-ZTERF
   _PA0007-WWEEK _PA0007-TEILK _PA0007-EMPCT1 _PA0007-ARBST1
           _PA0007-WOSTD1
   _PA0007-MOSTD1 _PA0007-JRSTD1 _PA0007-WKWDY1 T.
    MOVE  _PA0007-PERID TO _PA0007-PERNR.
    T = STRLEN( _PA0007-PERID ) .
    IF T = 9.
      APPEND _PA0007.
    ENDIF.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM UPLOAD_50DATA                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  _PA0050                                                       *
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_50DATA  USING F ERR.

  DATA: BEGIN OF ITAB OCCURS 0,
  FILE1(65535),
   END OF ITAB.

  CALL FUNCTION 'WS_UPLOAD'
      EXPORTING
*         CODEPAGE                = ' '
           FILENAME                = F
           FILETYPE                = 'ASC'
*         HEADLEN                 = ' '
*         LINE_EXIT               = ' '
*         TRUNCLEN                = ' '
*         USER_FORM               = ' '
*         USER_PROG               = ' '
*   importing
*         FILELENGTH              =
       TABLES
            DATA_TAB                = ITAB
       EXCEPTIONS
            UNKNOWN_ERROR = 7
            OTHERS        = 8.
  PERFORM CHECK_ERROR USING SY-SUBRC ERR.
  DATA T(3).
  LOOP AT ITAB.
    SPLIT ITAB-FILE1 AT DELIMITER INTO
  _PA0050-PERID _PA0050-BEGDA1 _PA0050-ENDDA1
 _PA0050-ZAUSW _PA0050-ZAUVE _PA0050-PMBDE _PA0050-BDEGR _PA0050-ZANBE
 _PA0050-ZMAIL _PA0050-ZPINC _PA0050-ZDGBE _PA0050-ZABAR _PA0050-GLMAX1
 _PA0050-GLMIN1 _PA0050-ZTZUA1 _PA0050-ZMGEN _PA0050-ZUSKZ
 _PA0050-GRAWG _PA0050-GRELG T.
    MOVE  _PA0050-PERID TO _PA0050-PERNR .
    T = STRLEN( _PA0050-PERID ) .
    IF T = 9.

      APPEND _PA0050.
    ENDIF.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM UPLOAD_2078910DATA                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  _PA0207                                                       *
*  -->  _PA0208                                                       *
*  -->  _PA0209                                                       *
*  -->  _PA0210                                                       *
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_2078910DATA USING F ERR.

  DATA: BEGIN OF ITAB OCCURS 0,
  FILE1(65535),
   END OF ITAB.

  CALL FUNCTION 'WS_UPLOAD'
      EXPORTING
*         CODEPAGE                = ' '
           FILENAME                = F
           FILETYPE                = 'ASC'
*         HEADLEN                 = ' '
*         LINE_EXIT               = ' '
*         TRUNCLEN                = ' '
*         USER_FORM               = ' '
*         USER_PROG               = ' '
*   importing
*         FILELENGTH              =
       TABLES
            DATA_TAB                = ITAB
       EXCEPTIONS
            UNKNOWN_ERROR = 7
            OTHERS        = 8.
  PERFORM CHECK_ERROR USING SY-SUBRC ERR.
  DATA T(3).
  LOOP AT ITAB.
    SPLIT ITAB-FILE1 AT DELIMITER INTO
_PA0207-PERID  _PA0207-BEGDA1 _PA0207-ENDDA1 _PA0207-TAXAR _PA0207-TAXAU
   _PA0207-TAXLV

   _PA0208-WTART _PA0208-ALLPC1 _PA0208-TAXAU _PA0208-TAXLV

   _PA0209-TAXAU _PA0209-WKSIT
 _PA0210-TAURT _PA0210-TAXLV _PA0210-TXSTA _PA0210-NBREX
_PA0210-ADEXN _PA0210-PEREX _PA0210-DEPEX _PA0210-EXIND _PA0210-AMTEX1
_PA0210-ADEXA1 _PA0210-IRLSI _PA0210-EXAMT1 _PA0210-FRMND _PA0210-FRMNR

 _PAT210-TAURT _PAT210-TAXLV _PAT210-TXSTA _PAT210-NBREX
 _PAT210-EXIND _PAT210-IRLSI _PAT210-EXAMT1 _PAT210-FRMND _PAT210-FRMNR
 _PAT210-EICST  .


    MOVE  _PA0207-PERID TO _PA0207-PERNR.
    MOVE  _PA0207-PERID TO _PA0208-PERNR.
    MOVE  _PA0207-PERID TO _PA0209-PERNR.
    MOVE  _PA0207-PERID TO _PA0210-PERNR.
    MOVE  _PA0207-PERID TO _PAT210-PERNR.
    T = STRLEN( _PA0207-PERID ) .
    IF T = 9.

      APPEND _PA0207.
      APPEND _PA0208.
      APPEND _PA0209.
      IF _PA0210-TAURT NE SPACE.
        APPEND _PA0210.
      ENDIF.

      MOVE-CORRESPONDING _PAT210 TO _PA0210.
      APPEND _PA0210.
    ENDIF.
  ENDLOOP.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM UPLOAD_8                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_8 USING F ERR.

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

  LOOP AT ITAB.
    DATA : T(3).
    CLEAR _PA0008.
    SPLIT ITAB-FILE1 AT DELIMITER INTO
_PA0008-PERID _PA0008-BEGDA1 _PA0008-ENDDA1 _PA0008-PREAS _PA0008-TRFAR
 _PA0008-TRFGB _PA0008-TRFGR _PA0008-TRFST _PA0008-BSGRD1 _PA0008-DIVGV1
 _PA0008-STVOR _PA0008-ANSAL1 _PA0008-LGART _PA0008-WAERS _PA0008-BETRG
 _PA0008-ANZHL _PA0008-EITXT T .
    SHIFT _PA0008-BETRG RIGHT DELETING TRAILING SPACE.
    MOVE  _PA0008-PERID TO _PA0008-PERNR.
    T = STRLEN( _PA0008-PERID ) .
    IF T = 9.

      APPEND _PA0008.
    ENDIF.
  ENDLOOP.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM UPLOAD_41                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_41 USING F ERR.

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
_PA0041-PERID _PA0041-BEGDA1 _PA0041-ENDDA1 _PA0041-DAR01 _PA0041-DAT001
_PA0041-DAR02 _PA0041-DAT002 _PA0041-DAR03 _PA0041-DAT003
_PA0041-DAR04 _PA0041-DAT004
_PA0041-DAR05 _PA0041-DAT005 _PA0041-DAR06 _PA0041-DAT006
_PA0041-DAR07 _PA0041-DAT007
_PA0041-DAR08 _PA0041-DAT008 _PA0041-DAR09 _PA0041-DAT009
_PA0041-DAR10 _PA0041-DAT010
_PA0041-DAR11 _PA0041-DAT011 _PA0041-DAR12 _PA0041-DAT012   T.
 MOVE  _PA0041-PERID TO _PA0041-PERNR.
    APPEND _PA0041.
  ENDLOOP.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM UPLOAD_94                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_94 USING F ERR.

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

  LOOP AT ITAB.
    DATA : T .                                              "
    SPLIT ITAB-FILE1 AT DELIMITER INTO
_PA0094-PERID _PA0094-BEGDA1 _PA0094-ENDDA1 _PA0094-RESIS _PA0094-FPNCD
 _PA0094-AUTH1 _PA0094-DOCN1 _PA0094-DATE11 _PA0094-EXPID _PA0094-FWOCD
 _PA0094-AUTH2 _PA0094-DOCN2 _PA0094-DATE21 _PA0094-EXPDT1 T.
    MOVE  _PA0094-PERID TO _PA0094-PERNR.

    APPEND _PA0094.
  ENDLOOP.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM UPLOAD_09                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_09 USING F ERR.

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
_PA0009-PERID _PA0009-BEGDA1 _PA0009-ENDDA1 _PA0009-BNKSA _PA0009-EMFTX
 _PA0009-BKPLZ _PA0009-BKORT _PA0009-BANKS _PA0009-BANKL _PA0009-BANKN
 _PA0009-BKONT _PA0009-ZLSCH _PA0009-ZWECK _PA0009-WAERS _PA0009-BTTYP
 _PA0009-BETRG1 _PA0009-ANZHL1 T.
    MOVE  _PA0009-PERID TO _PA0009-PERNR.

    APPEND _PA0009.
  ENDLOOP.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM UPLOAD_14                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_14 USING F ERR.

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
_PA0014-PERID _PA0014-BEGDA1 _PA0014-ENDDA1 _PA0014-LGART _PA0014-BETRG1
 _PA0014-WAERS _PA0014-ANZHL1 _PA0014-EITXT _PA0014-ZUORD _PA0014-PREAS
 _PA0014-MODEL _PA0014-ZFPER _PA0014-ZNPER _PA0014-ZDATE1 _PA0014-ZANZL1
 _PA0014-ZEITX _PA0014-UWDAT T.
    MOVE  _PA0014-PERID TO _PA0014-PERNR.
    APPEND _PA0014.
  ENDLOOP.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM UPLOAD_15                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_15 USING F ERR.
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
 _PA0015-PERID _PA0015-LGART _PA0015-BETRG1 _PA0015-WAERS _PA0015-ANZHL1
 _PA0015-EITXT _PA0015-BEGDA1 _PA0015-PABRP _PA0015-PABRJ _PA0015-ZUORD
 _PA0015-PREAS T.
    MOVE  _PA0015-PERID TO _PA0015-PERNR.
    APPEND _PA0015.
  ENDLOOP.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM UPLOAD_167                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_167 USING F ERR.
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
 _PA0167-PERID _PA0167-BEGDA1 _PA0167-ENDDA1 _PA0167-BAREA _PA0167-PLTYP
 _PA0167-BPLAN _PA0167-BOPTI _PA0167-DEPCV _PA0167-PRETX _PA0167-COORD
 _PA0167-PROVI _PA0167-POLNR _PA0167-ELIDT _PA0167-ELDTO1 _PA0167-PARDT1
 _PA0167-ENRTX _PA0167-EOIRQ _PA0167-EOIPR _PA0167-EOGRP _PA0167-CSTDT
 _PA0167-EECST _PA0167-PERTX T.
    MOVE  _PA0167-PERID TO _PA0167-PERNR.
    APPEND _PA0167.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM UPLOAD_168                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_168 USING F ERR.
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
 _PA0168-PERID _PA0168-BEGDA1 _PA0168-ENDDA1
 _PA0168-BPLAN _PA0168-BOPTI _PA0168-ADDNO1 _PA0168-PRETX "bcovr<-bopti
 _PA0168-BENSL _PA0168-COVAM _PA0168-ELIDT _PA0168-ELDTO1 _PA0168-PARDT1
 _PA0168-ENRTX _PA0168-EOIRQ _PA0168-EOIPR _PA0168-EOGRP _PA0168-CSTDT
 _PA0168-EECST _PA0168-PERTX T.       "Changed pretx > Pertx
    MOVE  _PA0168-PERID TO _PA0168-PERNR.
    APPEND _PA0168.
  ENDLOOP.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM UPLOAD_169                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_169 USING F ERR.
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
 _PA0169-PERID _PA0169-BEGDA1 _PA0169-ENDDA1
               _PA0169-BAREA _PA0169-PLTYP _PA0169-BPLAN _PA0169-PERTX
 _PA0169-EEPCT1 _PA0169-EEAMT1 _PA0169-PTPCT1
      _PA0169-PTAMT1 _PA0169-PSTTX
 _PA0169-ELIDT _PA0169-ELDTO1 _PA0169-PARDT1 _PA0169-ENRTX _PA0169-ACCTN
 _PA0169-PAAMT1
 _PA0169-BCAMT1 _PA0169-BCPCT1 _PA0169-BCUNT1
 _PA0169-BPAMT1 _PA0169-BPPCT1 _PA0169-BPUNT1 T.
    MOVE  _PA0169-PERID TO _PA0169-PERNR.
    APPEND _PA0169.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM UPLOAD_17                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_17 USING F ERR.
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
 _PA0017-PERID _PA0017-BEGDA1 _PA0017-ENDDA1 _PA0017-ERKLA _PA0017-ERGRU
 _PA0017-PKWRG _PA0017-PKWKL _PA0017-PEKEZ _PA0017-SPEBE _PA0017-PTZUO
 _PA0017-TRVFL _PA0017-BWAEN _PA0017-BUKRS _PA0017-GSBER _PA0017-KOSTL
 _PA0017-FISTL _PA0017-GEBER T.
    MOVE  _PA0017-PERID TO _PA0017-PERNR.
    APPEND _PA0017.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM UPLOAD_170                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_170 USING F ERR.
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
 _PA0170-PERID _PA0170-BEGDA1 _PA0170-ENDDA1 _PA0170-BAREA _PA0170-PLTYP
_PA0170-BPLAN _PA0170-CAMNT1 _PA0170-ELIDT _PA0170-ELDTO1 _PA0170-PARDT1
 _PA0170-ENRTX1 T.
    MOVE  _PA0170-PERID TO _PA0170-PERNR.
    APPEND _PA0170.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM UPLOAD_171                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_171 USING F ERR.
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
 _PA0171-PERID _PA0171-BEGDA1 _PA0171-ENDDA1
               _PA0171-BAREA _PA0171-BENGR _PA0171-BSTAT T.
    MOVE  _PA0171-PERID TO _PA0171-PERNR.
    APPEND _PA0171.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM UPLOAD_172                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_172 USING F ERR.
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
_PA0172-PERID _PA0172-PLTYP _PA0172-BPLAN _PA0172-CLDAT1 _PA0172-CODAT1
_PA0172-FSACL1 _PA0172-CLSTA1 _PA0172-RCPTY _PA0172-RECNR _PA0172-PROVN
T.
    MOVE  _PA0172-PERID TO _PA0172-PERNR.
    APPEND _PA0172.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM UPLOAD_194                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_194 USING F ERR.
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
 _PA0194-PERID _PA0194-BEGDA1 _PA0194-ENDDA1 _PA0194-GCASE _PA0194-GSTAT
 _PA0194-GPRIO _PA0194-RCVDD1 _PA0194-ORIGN _PA0194-GRNUM _PA0194-RLSDD
 _PA0194-GCATE _PA0194-LIFNR _PA0194-PLAIN _PA0194-RULNR
 _PA0194-SCHID  _PA0194-ORCOD _PA0194-ORNAM _PA0194-ORSTR
 _PA0194-ORORT _PA0194-ORPLZ _PA0194-ORREG _PA0194-ORCNT _PA0194-ANSWR
 _PA0194-SCRUL _PA0194-LAPDY _PA0194-LAPDT T.
    MOVE  _PA0194-PERID TO _PA0194-PERNR.
    APPEND _PA0194.
  ENDLOOP.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM UPLOAD_195                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_195 USING F ERR.
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
_PA0195-PERID  _PA0195-GCASE _PA0195-OBJPS _PA0195-GRNUM _PA0195-ORDCD
_PA0195-RULID _PA0195-IBALN1 _PA0195-DEDUC1 _PA0195-DEDUT
                                            _PA0195-LI1NO _PA0195-LI1EX
 _PA0195-NVAL11 _PA0195-NUNI1 _PA0195-LI2NO _PA0195-LI2EX _PA0195-NVAL21
 _PA0195-NUNI2 _PA0195-ADDNO _PA0195-ADDEX _PA0195-ADDVL1 _PA0195-ADDUN
                                                                    T .
    MOVE  _PA0195-PERID TO _PA0195-PERNR.
    APPEND _PA0195.
  ENDLOOP.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM UPLOAD_21                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_21 USING F ERR.
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
 _PA0021-PERID _PA0021-BEGDA1 _PA0021-ENDDA1 _PA0021-FAMSA
  _PA0021-FANAM _PA0021-FAVOR
 _PA0021-GESC2 _PA0021-GESC1
 _PA0021-FGBNA _PA0021-FGBDT1 _PA0021-ERBNR
 _PA0021-SSN1 _PA0021-AREAC _PA0021-TELNR _PA0021-STRAS _PA0021-LOCAT
 _PA0021-SEPDT1 _PA0021-ORT01 _PA0021-STATE _PA0021-PSTLZ _PA0021-LAND1
 _PA0021-DOCNM _PA0021-DOCID _PA0021-DOCN2 _PA0021-DOCI2
 _PA0021-BEN03 _PA0021-BEN04 _PA0021-MEDIC _PA0021-SMOKE
 _PA0021-DISAB _PA0021-DISDT1 _PA0021-DISLE1
   T .
    MOVE  _PA0021-PERID TO _PA0021-PERNR.
    APPEND _PA0021.
  ENDLOOP.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM UPLOAD_234                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_234 USING F ERR.
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
 _PA0234-PERID _PA0234-BEGDA1 _PA0234-ENDDA1
               _PA0234-TAURT _PA0234-TAXLV _PA0234-SUPCD _PA0234-CERTF
 _PA0234-OVAMT1 _PA0234-COVGR T.
    MOVE  _PA0234-PERID TO _PA0234-PERNR.
    APPEND _PA0234.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM UPLOAD_235                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
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
    MOVE  _PA0235-PERID TO _PA0235-PERNR.
    APPEND _PA0235.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM UPLOAD_236                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_236 USING F ERR.
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
 _PA0236-PERID _PA0236-BEGDA1 _PA0236-ENDDA1 _PA0236-BAREA _PA0236-PLTYP
 _PA0236-BPLAN _PA0236-ELIDT _PA0236-ELDTO1 _PA0236-PARDT1 _PA0236-ENRTX
 _PA0236-CRAMT _PA0236-PERT1 _PA0236-CRAMO1 _PA0236-PERT2 _PA0236-CRDDT
 _PA0236-PCRED _PA0236-PERT3 T.
    MOVE  _PA0236-PERID TO _PA0236-PERNR.
    APPEND _PA0236.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM UPLOAD_27                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_27 USING F ERR.
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
 _PA0027-PERID _PA0027-BEGDA1 _PA0027-ENDDA1 _PA0027-KSTAR _PA0027-KBU
 _PA0027-KSI _PA0027-KPR T.
    MOVE  _PA0027-PERID TO _PA0027-PERNR.
    APPEND _PA0027.
  ENDLOOP.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM UPLOAD_377                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_377 USING F ERR.
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
 _PA0377-PERID _PA0377-BEGDA1 _PA0377-ENDDA1 _PA0377-BPLAN _PA0377-LEVL1
_PA0377-ELIDT _PA0377-ELDTO1 _PA0377-PARDT1 _PA0377-ENRTX _PA0377-ADDNO1
 _PA0377-PRETX  _PA0377-SALOV1  _PA0377-COVOV1
 _PA0377-CRAMT1 _PA0377-CRAMO1 _PA0377-CSTDT _PA0377-EECST
 _PA0377-EEPCT1 _PA0377-EEAMT1 _PA0377-PTPCT1
_PA0377-PTAMT1 _PA0377-PSTTX T.

    MOVE  _PA0377-PERID TO _PA0377-PERNR.
    APPEND _PA0377.
  ENDLOOP.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM UPLOAD_378                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_378 USING F ERR.
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
_PA0378-PERID _PA0378-BEGDA1 _PA0378-ENDDA1  _PA0378-EVENT
                                                                   T.
    MOVE  _PA0378-PERID TO _PA0378-PERNR .
    APPEND _PA0378.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM UPLOAD_379                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F                                                             *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_379 USING F ERR.
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
_PA0379-PERID _PA0379-BEGDA1 _PA0379-ENDDA1 _PA0379-BPLAN1
_PA0379-ELIDT1 _PA0379-ELDTO1 _PA0379-PARDT1 _PA0379-ENRTX1
_PA0379-EEAMT1 _PA0379-EEPCT1 _PA0379-EEUNT1 _PA0379-PTAMT1
_PA0379-PTPCT1 _PA0379-PTUNT1 _PA0379-PSTTX1 _PA0379-PDFIX1
_PA0379-PDREG1 _PA0379-PDBEG1 _PA0379-PDEND1 _PA0379-PERIO1
_PA0379-BCAMT1 _PA0379-BCPCT1 _PA0379-BCUNT1 _PA0379-BPAMT1
_PA0379-BPPCT1 _PA0379-BPUNT1 _PA0379-BPTTX  T.

    MOVE  _PA0379-PERID TO _PA0379-PERNR .
    APPEND _PA0379.
  ENDLOOP.
ENDFORM.




*---------------------------------------------------------------------*
*       FORM init_bdc                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM init_bdc using ses_name sap_user.

  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT = SY-MANDT
            GROUP  = ses_name
            USER   = sap_user.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM INSERT_BDC                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM INSERT_BDC TABLES BTAB USING TRCD.
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE     = TRCD
       TABLES
            DYNPROTAB = BTAB.
  REFRESH BTAB.
  CLEAR BTAB.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM DYNPRO                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  DYNBEGIN                                                      *
*  -->  NAME                                                          *
*  -->  VALUE                                                         *
*  -->  IDX                                                           *
*---------------------------------------------------------------------*
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

*---------------------------------------------------------------------*
*       FORM CHECK_ERROR                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  ERR_CD                                                        *
*  -->  STAGE                                                         *
*---------------------------------------------------------------------*
FORM CHECK_ERROR USING ERR_CD STAGE.
  CASE ERR_CD.
    WHEN 0.
      WRITE:/ 'Upload process ', STAGE,'uploaded'.
    WHEN OTHERS.
      WRITE:/ 'Upload process ', STAGE,'not present'.
  ENDCASE.
ENDFORM.
