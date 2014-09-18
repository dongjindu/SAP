************************************************************************
* Created by  : JS LEE
* Created on  : 2003.08.05
* Description : Scheduling agreement creation
*
*
* Modification Log
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZEMMPM05E_SA_CR  no standard page heading LINE-SIZE 260
.


************************************************************************
* Data
************************************************************************
*     LOADING FILE TABLE

data : begin of it_FILE OCCURS 10,
       lifnr(10) ,    " Vendor
       evart(4),      " Agreement Type
       vedat(10),      " Agreement Date
       ekorg(4),      " Pur. Org
       ekgrp(3),      " Pur. Group
       kdate(10),      " Vaildate end
       zterm(4),      " Payment Tems
       ematn(18),     " Material No
       ktmng(16),     " Target Qty
       werks(4),      " Plant
       lgort(4),      " Storage location
       fipos(10),     " Commitment item
       bstae(10),     " Confirmation control key
       abueb(4),      " Creation Profile
       etfz1(3),      " Firm Zone
       lgbzo(10),     " Unloading Point
       end of it_FILE.

*       ERROR DATA .

data : begin of it_ERROR OCCURS 10,
       lifnr(10) ,    " Vendor
       evart(4),      " Agreement Type
       vedat(10),      " Agreement Date
       ekorg(4),      " Pur. Org
       ekgrp(3),      " Pur. Group
       kdate(10),      " Vaildate end
       zterm(4),      " Payment Tems
       ematn(18),     " Material No
       ktmng(16),     " Target Qty
       werks(4),      " Plant
       lgort(4),      " Storage location
       fipos(10),     " Commitment item
       bstae(10),     " Confirmation control key
       abueb(4),      " Creation Profile
       etfz1(3),      " Firm Zone
       lgbzo(10),     " Unloading Point
       end of it_ERROR.


*      ITEM TABLE
data : begin of it_sa OCCURS 10,
       lifnr like ekko-lifnr ,    " Vendor
       evart like rm06e-evart,    " Agreement Type
       vedat(10),                 " Agreement Date
       ekorg like ekko-ekorg,     " Pur. Org
       ekgrp like ekko-ekgrp,     " Pur. Group
       kdate(10),                 " Vaildate end
       zterm like ekko-zterm,     " Payment Tems
       ematn like ekpo-ematn,     " Material No
       ktmng(15),                 " Target Qty
       werks like ekpo-werks,     " Plant
       lgort like ekpo-lgort,     " Storage location
       fipos like cobl-fipos,     " Commitment item
       bstae like ekpo-bstae,     " Confirmation control key
       abueb like ekpo-abueb,     " Creation Profile
       etfz1(3) ,                 " Firm Zone
       lgbzo like ekpo-lgbzo,     " Unloading Point
       end of it_sa.

*      HEADER TABLE
data : begin of it_HEADER OCCURS 10,
       lifnr like ekko-lifnr ,    " Vendor
       evart like rm06e-evart,    " Agreement Type
       vedat like rm06e-vedat,    " Agreement Date
       ekorg like ekko-ekorg,     " Pur. Org
       ekgrp like ekko-ekgrp,     " Pur. Group
       kdate(10),                 " Vaildate end
       zterm like ekko-zterm,     " Payment Tems
       END OF IT_HEADER.

*       Batchinputdata of single transaction
DATA: BEGIN OF BDC_TAB OCCURS 10.      " BDC STRUCTURE
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_TAB.


*       messages of call transaction
DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.

DATA : G_CANCEL ,
       G_SUBRC  LIKE SY-SUBRC .

*       message texts
TABLES: T100.

************************************************************************
* SELECTION SCREEN
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK bK WITH FRAME TITLE TEXT-001.
PARAMETERS : P_FNAME LIKE RLGRAP-FILENAME
                     DEFAULT 'C:\TEMP\SA_DATA\SA_CREATE.TXT'. " FILENAME

PARAMETERS : P_CMODE LIKE CTU_PARAMS-DISMODE DEFAULT 'N'.
"A: show all dynpros
"E: show dynpro on error only
"N: do not display dynpro

SELECTION-SCREEN END OF BLOCK Bk .

************************************************************************
* START-OF-SELECTION
************************************************************************


start-of-selection.

  PERFORM UPLOADING.

  CHECK G_CANCEL EQ SPACE .

  PERFORM TABLE_ASSIGN.
  PERFORM DATA_CACULATION.
  PERFORM BDC_DATA_GENERATION .
  PERFORM ERROR_DATA_DOWN.


*&---------------------------------------------------------------------*
*&      Form  UPLOADING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  FILE UPLOADING  FLAT FILE
*----------------------------------------------------------------------*

FORM UPLOADING.

  CALL FUNCTION 'UPLOAD'
       EXPORTING
            FILENAME                = P_FNAME
            FILETYPE                = 'DAT'
       IMPORTING
            CANCEL                  = G_CANCEL
       TABLES
            DATA_TAB                = IT_FILE
       EXCEPTIONS
            CONVERSION_ERROR        = 1
            INVALID_TABLE_WIDTH     = 2
            INVALID_TYPE            = 3
            NO_BATCH                = 4
            UNKNOWN_ERROR           = 5
            GUI_REFUSE_FILETRANSFER = 6
            OTHERS                  = 7.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*--- DELET HADER TEXT

READ TABLE IT_FILE INDEX 1.

IF IT_FILE-ktmng  CO ' 0123456789'.
ELSE.
  DELETE IT_FILE INDEX 1.
ENDIF.



ENDFORM.                    " UPLOADING
*&---------------------------------------------------------------------*
*&      Form  TABLE_ASSIGN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TABLE_ASSIGN.

  CLEAR: IT_SA, it_sa[].
  CLEAR : IT_ERROR , IT_ERROR[].

  LOOP AT IT_FILE.

    MOVE-CORRESPONDING IT_FILE TO IT_SA.
    APPEND IT_SA.
    CLEAR IT_SA.

  ENDLOOP.

  FREE IT_FILE.    " MEMORY FREE




ENDFORM.                    " TABLE_ASSIGN
*&---------------------------------------------------------------------*
*&      Form  DATA_CACULATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_CACULATION.

* SA HEADER DATA COLLECT .

  LOOP AT IT_SA.
    MOVE-CORRESPONDING IT_SA TO IT_HEADER.
    COLLECT IT_HEADER.
    CLEAR IT_HEADER.
  ENDLOOP.

* empty line delete .
  delete IT_HEADER where lifnr = ''.

ENDFORM.                    " DATA_CACULATION
*&---------------------------------------------------------------------*
*&      Form  BDC_DATA_GENERATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_DATA_GENERATION.

  DATA : L_FIRST,
         L_EBELP LIKE RM06E-EBELP.



  LOOP AT it_header.

    REFRESH:  BDC_TAB.


* HEADER DATA
    PERFORM BDC_DATA USING : 'X' 'SAPMM06E'      '0200',
                             ' ' 'EKKO-LIFNR'    IT_HEADER-LIFNR,
                             ' ' 'RM06E-EVART'   IT_HEADER-EVART,
                             ' ' 'RM06E-VEDAT'   IT_HEADER-VEDAT,
                             ' ' 'EKKO-EKORG'    IT_HEADER-EKORG,
                             ' ' 'EKKO-EKGRP'    IT_HEADER-EKGRP,
                             ' ' 'BDC_OKCODE'    '/00',

                             'X' 'SAPMM06E'      '0201',
                             ' ' 'EKKO-KDATE'    IT_HEADER-KDATE,
                             ' ' 'EKKO-ZTERM'    IT_HEADER-ZTERM,
                             ' ' 'BDC_OKCODE'    '/00'.


*-- ITEM DATA
    LOOP AT IT_SA WHERE  LIFNR = IT_HEADER-LIFNR
                    AND  EVART = IT_HEADER-EVART
                    AND  VEDAT = IT_HEADER-VEDAT
                    AND  EKORG = IT_HEADER-EKORG
                    AND  EKGRP = IT_HEADER-EKGRP
                    AND  KDATE = IT_HEADER-KDATE
                    AND  ZTERM = IT_HEADER-ZTERM .

      L_EBELP = L_EBELP + 1.   " POSTION SELECT.


      if l_first <> 'X'.
        PERFORM BDC_DATA USING : 'X' 'SAPMM06E'      '0220',
                                 ' ' 'EKPO-EMATN(01)' IT_SA-ematn,
                                 ' ' 'EKPO-KTMNG(01)' IT_SA-KTMNG,
                                 ' ' 'EKPO-WERKS(01)' IT_SA-WERKS,
                                 ' ' 'EKPO-LGORT(01)' IT_SA-LGORT,
                                 ' ' 'BDC_OKCODE'     '/00',

                                 'X' 'SAPLFMSV'       '0100',
                                 ' ' 'COBL-FIPOS'     IT_SA-FIPOS,
                                 ' ' 'BDC_OKCODE'     '=ENTR',

                                 'X' 'SAPMM06E'       '0220',
                                 ' ' 'RM06E-TCSELFLAG(01)' 'X',
                                 ' ' 'BDC_OKCODE'     '=DETZ',


                                 'X' 'SAPMM06E'       '0212',
                                 ' ' 'EKPO-ABUEB'     IT_SA-ABUEB,
                                 ' ' 'EKPO-ETFZ1'     IT_SA-ETFZ1,
                                 ' ' 'EKPO-LGBZO'     IT_SA-LGBZO,
                                 ' ' 'BDC_OKCODE'     '=DETA',

                                 'X' 'SAPMM06E'       '0211',
                                 ' ' 'EKPO-BSTAE'     IT_SA-BSTAE,
                                 ' ' 'BDC_OKCODE'     '=BACK'.
        l_first = 'X'.
      else.

        PERFORM BDC_DATA USING : 'X' 'SAPMM06E'      '0220',
                                 ' ' 'EKPO-EMATN(02)' IT_SA-ematn,
                                 ' ' 'EKPO-KTMNG(02)' IT_SA-KTMNG,
                                 ' ' 'EKPO-WERKS(02)' IT_SA-WERKS,
                                 ' ' 'EKPO-LGORT(02)' IT_SA-LGORT,
                                 ' ' 'BDC_OKCODE'     '/00',

                                 'X' 'SAPLFMSV'       '0100',
                                 ' ' 'COBL-FIPOS'     IT_SA-FIPOS,
                                 ' ' 'BDC_OKCODE'     '=ENTR',

                                 'X' 'SAPMM06E'       '0220',
                                 ' ' 'RM06E-TCSELFLAG(02)' 'X',
                                 ' ' 'BDC_OKCODE'     '=DETZ',

                                 'X' 'SAPMM06E'       '0212',
                                 ' ' 'EKPO-ABUEB'     IT_SA-ABUEB,
                                 ' ' 'EKPO-ETFZ1'     IT_SA-ETFZ1,
                                 ' ' 'EKPO-LGBZO'     IT_SA-LGBZO,
                                 ' ' 'BDC_OKCODE'     '=DETA',

                                 'X' 'SAPMM06E'       '0211',
                                 ' ' 'EKPO-BSTAE'     IT_SA-BSTAE,
                                 ' ' 'BDC_OKCODE'     '=BACK'.

        PERFORM BDC_DATA USING : 'X' 'SAPMM06E'      '0220',
                                 ' ' 'RM06E-EBELP'    L_EBELP,
                                 ' ' 'BDC_OKCODE'     '/00'.

      endif.

    ENDLOOP.

* call transation .

    CLEAR : L_EBELP, L_FIRST .

    PERFORM BDC_DATA USING : 'X' 'SAPMM06E'      '0220',
                             ' ' 'BDC_OKCODE'     '=BU',
                             'X' 'SAPLSPO1'      '0300',
                             ' ' 'BDC_OKCODE'     '=YES'.




    PERFORM BDC_TRANSACTION .



  ENDLOOP.
ENDFORM.                    " BDC_DATA_GENERATION


*&---------------------------------------------------------------------*
*&      Form  BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0357   text
*      -->P_0358   text
*      -->P_0359   text
*----------------------------------------------------------------------*
FORM BDC_DATA USING DYNBEGIN NAME VALUE.

  IF DYNBEGIN = 'X'.
    CLEAR BDC_TAB.
    MOVE: NAME TO BDC_TAB-PROGRAM,
          VALUE TO BDC_TAB-DYNPRO,
          'X'   TO BDC_TAB-DYNBEGIN.
    APPEND BDC_TAB.
  ELSE.
    CLEAR BDC_TAB.
    MOVE: NAME TO BDC_TAB-FNAM,
          VALUE TO BDC_TAB-FVAL.
    APPEND BDC_TAB.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BDC_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_TRANSACTION.
** error handing and call transation

  DATA: L_MSTRING(480).

  REFRESH MESSTAB.

  CALL TRANSACTION 'ME31L'
                 USING    BDC_TAB
                 MODE     P_CMODE
                 UPDATE  'A'
                 messageS INTO MESSTAB.

  G_SUBRC = SY-SUBRC.

  WRITE: / '      ',
           'S/A Create','Vendor: [', it_header-lifnr, '] ',
           'returncode:'(I05),
           G_SUBRC.

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
      WRITE: / MESSTAB-MSGTYP, L_MSTRING(250).
    ELSE.
      WRITE: / MESSTAB.
    ENDIF.
  ENDLOOP.


*-- ERROR DATA.
  if G_SUBRC ne 0.

    LOOP AT IT_SA WHERE  LIFNR = IT_HEADER-LIFNR
                    AND  EVART = IT_HEADER-EVART
                    AND  VEDAT = IT_HEADER-VEDAT
                    AND  EKORG = IT_HEADER-EKORG
                    AND  EKGRP = IT_HEADER-EKGRP
                    AND  KDATE = IT_HEADER-KDATE
                    AND  ZTERM = IT_HEADER-ZTERM .

      move-corresponding IT_SA TO IT_ERROR.

      APPEND IT_ERROR.
      CLEAR  IT_ERROR.



    endloop.
  endif.


ENDFORM.                    " BDC_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  ERROR_DATA_DOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ERROR_DATA_DOWN.

  data : l_count type i .

  clear l_count.

  describe table it_error lines l_count.

  check l_count > 0.


  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            FILENAME                = 'C:\TEMP\SA_DATA\SA_ERROR.TXT'
            FILETYPE                = 'DAT'
       TABLES
            DATA_TAB                = IT_ERROR
       EXCEPTIONS
            INVALID_FILESIZE        = 1
            INVALID_TABLE_WIDTH     = 2
            INVALID_TYPE            = 3
            NO_BATCH                = 4
            UNKNOWN_ERROR           = 5
            GUI_REFUSE_FILETRANSFER = 6
            CUSTOMER_ERROR          = 7
            OTHERS                  = 8.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.




ENDFORM.                    " ERROR_DATA_DOWN
