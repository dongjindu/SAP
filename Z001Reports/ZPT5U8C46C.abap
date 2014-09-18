* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Conversion Payroll Results Load
* Version 1.0  - August 2000

* Payroll results - table T5U8C direct load
* Authors : Hemang / Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPT5U8C46C MESSAGE-ID ZP.

TABLES :T5U8C .    " payroll periods for each personnal no
TABLES : PA0002.   " HR Master load - it 0002 -

PARAMETERS : FT5U8C LIKE RLGRAP-FILENAME DEFAULT'C:\5U8C.TXT'.

DATA: BEGIN OF ITAB5U OCCURS 0,
   PERNR1(9),                   " like   t5u8c-pernr,
   SEQNR LIKE   T5U8C-SEQNR,
   TBLTY LIKE   T5U8C-TBLTY,
   MOLGA LIKE   T5U8C-MOLGA,
   LGART LIKE   T5U8C-LGART,
   TAXAU LIKE   T5U8C-TAXAU,
   KEYDATE LIKE T5U8C-KEYDATE,
   KEYDATE1(10),
   BETPE(15) TYPE C,
   ANZHL(15) TYPE C,
   BETRG(15) TYPE C.
*include structure T5U8C.
DATA :END OF ITAB5U.

DATA: DELIMITER TYPE X VALUE'09', ERR(50).

PERFORM UPLOAD_T5U8C USING FT5U8C ERR.
PERFORM APPEND.

*---------------------------------------------------------------------*
*       FORM UPLOAD_T5U8C                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FT5U8C                                                        *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_T5U8C USING FT5U8C ERR.

  DATA: BEGIN OF ITAB OCCURS 0,
  FILE1(8192),
   END OF ITAB.
  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            FILENAME      = FT5U8C
            FILETYPE      = 'ASC'
       TABLES
            DATA_TAB      = ITAB
       EXCEPTIONS
            UNKNOWN_ERROR = 7
            OTHERS        = 8.
  PERFORM CHECK_ERROR USING SY-SUBRC ERR.
  DATA : T.  " Used to store junk (tab) char if any ...
  LOOP AT ITAB.
    SPLIT ITAB-FILE1 AT DELIMITER INTO
ITAB5U-PERNR1 ITAB5U-SEQNR ITAB5U-TBLTY ITAB5U-MOLGA
ITAB5U-LGART ITAB5U-TAXAU ITAB5U-KEYDATE1 ITAB5U-BETPE
ITAB5U-ANZHL ITAB5U-BETRG T.

CONCATENATE ITAB5U-KEYDATE1+6(4)
           ITAB5U-KEYDATE1+0(2)
           ITAB5U-KEYDATE1+3(2) INTO ITAB5U-KEYDATE.
IF ITAB5U-PERNR1 NE SPACE.
  APPEND ITAB5U.
ENDIF.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM APPEND                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM APPEND.
  LOOP AT ITAB5U.

   SELECT SINGLE * FROM PA0002 WHERE PERID =  ITAB5U-PERNR1.
  IF SY-SUBRC NE 0.
     WRITE :/ 'No employee with SSN:',  ITAB5U-PERNR1.
  ELSE.
    T5U8C-PERNR =  PA0002-PERNR.  "tab5u-pernr1.
    T5U8C-SEQNR =  ITAB5U-SEQNR.
    T5U8C-TBLTY =  ITAB5U-TBLTY.
    T5U8C-MOLGA =  ITAB5U-MOLGA.
    T5U8C-LGART =  ITAB5U-LGART.
    T5U8C-TAXAU = ITAB5U-TAXAU.
    T5U8C-KEYDATE =  ITAB5U-KEYDATE.
    T5U8C-BETPE =  ITAB5U-BETPE.
    T5U8C-ANZHL =  ITAB5U-ANZHL.
    T5U8C-BETRG =  ITAB5U-BETRG.
IF T5U8C-PERNR NE '0'.
    INSERT  T5U8C.
    ENDIF.
    IF SY-SUBRC NE 0.
      WRITE:/ 'unable to insert into table T5U8C'.
      WRITE T5U8C-PERNR.
      CONTINUE.
    ENDIF.
   ENDIF.    " select on pa0002 for ssn
  ENDLOOP.

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
    WHEN OTHERS.
      WRITE:/ 'Error in the process ', STAGE, '. Error -', ERR_CD.
      STOP.
  ENDCASE.
ENDFORM.
