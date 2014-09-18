* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Conversion Payroll Results Load
* Version 1.0  - August 2000

* Payroll results - table T558C direct load
* Authors : Hemang / Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPT558C46C MESSAGE-ID ZP.

* changed by Hemang to use SSN instead of PERNR
TABLES :T558C .    " payroll periods for each personnal no
TABLES :PA0002.    " HR master rec it 00002 - 2 find pernr based on ssn
PARAMETERS : FT558C LIKE RLGRAP-FILENAME DEFAULT'C:\558C.TXT'.

DATA: BEGIN OF ITABTT5C OCCURS 0,
   PERNR1(9),                  "like   t558c-pernr,
   SEQNR LIKE   T558C-SEQNR,
   MOLGA LIKE   T558C-MOLGA,
   LGART LIKE   T558C-LGART,
   KEYDATE LIKE T558C-KEYDATE,
   KEYDATE1(10),
   BETPE(15) TYPE C,
   ANZHL(15) TYPE C,
   BETRG(15) TYPE C.
*include structure t558c.
DATA :END OF ITABTT5C.

DATA: DELIMITER TYPE X VALUE'09', ERR(50).

PERFORM UPLOAD_T558C USING FT558C ERR.
PERFORM APPEND.

*---------------------------------------------------------------------*
*       FORM UPLOAD_T558C                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FT558C                                                        *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_T558C USING FT558C ERR.

  DATA: BEGIN OF ITAB OCCURS 0,
  FILE1(8192),
   END OF ITAB.
  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            FILENAME      = FT558C
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
ITABTT5C-PERNR1 ITABTT5C-SEQNR ITABTT5C-MOLGA ITABTT5C-LGART
ITABTT5C-KEYDATE1 ITABTT5C-BETPE ITABTT5C-ANZHL ITABTT5C-BETRG  T.

CONCATENATE ITABTT5C-KEYDATE1+6(4)
           ITABTT5C-KEYDATE1+0(2)
           ITABTT5C-KEYDATE1+3(2) INTO ITABTT5C-KEYDATE.
IF ITABTT5C-PERNR1 NE SPACE.
  APPEND ITABTT5C.
ENDIF.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM APPEND                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM APPEND.


  LOOP AT ITABTT5C.
   SELECT SINGLE * FROM PA0002 WHERE PERID =  ITABTT5C-PERNR1.
  IF SY-SUBRC NE 0.
     WRITE :/ 'No employee with SSN:',  ITABTT5C-PERNR1.
  ELSE.
    T558C-PERNR =  PA0002-PERNR.     " itabtt5c-pernr1.
    T558C-SEQNR =  ITABTT5C-SEQNR.
    T558C-MOLGA =  ITABTT5C-MOLGA.
    T558C-LGART =  ITABTT5C-LGART.
    T558C-KEYDATE =  ITABTT5C-KEYDATE .
    T558C-BETPE =  ITABTT5C-BETPE.
    T558C-ANZHL =  ITABTT5C-ANZHL.
    T558C-BETRG =  ITABTT5C-BETRG.
   IF T558C-PERNR NE '0'.
    INSERT  T558C.
    ENDIF.
    IF SY-SUBRC NE 0.
      WRITE:/ 'unable to insert into table T558C'.
      WRITE T558C-PERNR.
      CONTINUE.
    ENDIF.
   ENDIF.   " select on pa0002 for ssn
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
