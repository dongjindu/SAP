* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Conversion Payroll Results Load
* Version 1.0  - August 2000

* Payroll results - table T558B direct load
* Authors : Hemang / Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZP558B46C MESSAGE-ID ZP.

* changed by Hemang to use SSN instead of PERNR
TABLES :T558B .    " payroll periods for each personnal no

TABLES :PA0002.    " HR master rec it 00002 - 2 find pernr based on ssn

PARAMETERS : FT558B LIKE RLGRAP-FILENAME DEFAULT'C:\558b.TXT'.

DATA: BEGIN OF ITABTT5B OCCURS 0,
   PERNR1(9),    "SSN
   PAYDT1(10) , FPBEG1(10), FPEND1(10).
   INCLUDE STRUCTURE T558B.

DATA :END OF ITABTT5B.

DATA: DELIMITER TYPE X VALUE'09', ERR(50).

PERFORM UPLOAD_T558B USING FT558B ERR.
PERFORM APPEND.

*---------------------------------------------------------------------*
*       FORM UPLOAD_T558B                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FT558B                                                        *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_T558B USING FT558B ERR.

  DATA: BEGIN OF ITAB OCCURS 0,
  FILE1(8192),
   END OF ITAB.
  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            FILENAME      = FT558B
            FILETYPE      = 'ASC'
       TABLES
            DATA_TAB      = ITAB
       EXCEPTIONS
            UNKNOWN_ERROR = 7
            OTHERS        = 8.
  PERFORM CHECK_ERROR USING SY-SUBRC ERR.

  DATA : T." Used to store junk (tab) char if any ...
  LOOP AT ITAB.

    SPLIT ITAB-FILE1 AT DELIMITER INTO
ITABTT5B-PERNR1 ITABTT5B-SEQNR ITABTT5B-PAYTY ITABTT5B-PAYID
ITABTT5B-PAYDT1 ITABTT5B-PERMO ITABTT5B-PABRJ ITABTT5B-PABRP
ITABTT5B-FPBEG1 ITABTT5B-FPEND1 T.

CONCATENATE ITABTT5B-PAYDT1+6(4)
            ITABTT5B-PAYDT1+0(2)
            ITABTT5B-PAYDT1+3(2) INTO ITABTT5B-PAYDT.
CONCATENATE ITABTT5B-FPBEG1+6(4)
            ITABTT5B-FPBEG1+0(2)
            ITABTT5B-FPBEG1+3(2) INTO ITABTT5B-FPBEG.
CONCATENATE ITABTT5B-FPEND1+6(4)
            ITABTT5B-FPEND1+0(2)
            ITABTT5B-FPEND1+3(2) INTO ITABTT5B-FPEND.
IF ITABTT5B-PERNR1 NE SPACE.
    APPEND ITABTT5B.
ENDIF.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM APPEND                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM APPEND.


  LOOP AT ITABTT5B.
    SELECT SINGLE * FROM PA0002 WHERE PERID =  ITABTT5B-PERNR1.
   IF SY-SUBRC NE 0.
      WRITE :/ 'No employee with SSN:',  ITABTT5B-PERNR.
   ELSE.
    T558B-PERNR =  PA0002-PERNR . "  itabtt5b-pernr.
    T558B-SEQNR =  ITABTT5B-SEQNR .
    T558B-PAYTY =  ITABTT5B-PAYTY .
    T558B-PAYID =  ITABTT5B-PAYID.
    T558B-PAYDT =  ITABTT5B-PAYDT .
    T558B-PERMO =  ITABTT5B-PERMO .
    T558B-PABRJ =  ITABTT5B-PABRJ .
    T558B-PABRP =  ITABTT5B-PABRP .
    T558B-FPBEG =  ITABTT5B-FPBEG .
    T558B-FPEND =  ITABTT5B-FPEND .

   IF T558B-PERNR NE '0'.
      INSERT  T558B.
   ENDIF.

   IF SY-SUBRC NE 0.
      WRITE:/ 'unable to insert into table t558b'.
      WRITE T558B-PERNR.
      CONTINUE.
   ENDIF.
  ENDIF.   "select on pa0002 for ssn
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
