************************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
*&---------------------------------------------------------------------*
*& Program: ZCORRDABMG
*& Type   : Correction Report                                          *
*& Author : Manju                                                      *
*& Title  : OSS Note : 202875                                          *
*&---------------------------------------------------------------------*
* Help Desk Request No  : 68IG261745                                   *
*   Requested by:         Richard Davis                                *
*   Assigned to:                                                       *
*   Original Request #:                                                *
*   ABAP Analyst:                                                      *
*                                                                      *
* Business Users:                                                      *
*                                                                      *
* Business Requirement Description:                                    *
*                                                                      *
*                                                                      *
* Processing Logic:                                                    *
*     < Outline the flow of the main processing logic >                *
*                                                                      *
* Configuration Requirements:                                          *
*     < Document any special config requirements that must exist for   *
*       this program to work correctly >                               *
*                                                                      *
* Program Inputs:                                                      *
*     < Input File Path & Name >                                       *
*     < Any variants program would be typically run with >             *
*                                                                      *
* Program Outputs:                                                     *
*       Online Report                                                  *
*                                                                      *
* Authorization Checks:                                                *
*     < Document if any authorization objects are checked >            *
*                                                                      *
* Direct Update Database Tables:                                       *
*   < No direct updates to SAP tables are allowed.List custom tables > *
*                                                                      *
* Outstanding Issues:                                                  *
*     < If the program is being delivered with any known open issues   *
*       document them here; they could be planned for next release >   *
*                                                                      *
* Instructions on how to test this program:                            *
*     < If this program needs any special inputs like an inbound file  *
*       from an EDI subsystem for testing, document it here >          *
*                                                                      *
* Instructions on how to re-start this program:                        *
*                                                                      *
* Volume Estimates:                                                    *
*                                                                      *
* Frequency of Execution:                                              *
*   o On demand                                                        *
*                                                                      *
* Execution Mode:                                                      *
*   o Online      - Transaction Code -                                 *
*                                                                      *
* Other Comments:                                                      *
*                                                                      *
*&----------------------------------------------------------------------
* Modification Logs
************************************************************************
* Date        Developer    RequestNo    Description
* 08/22/06    Manju          UD1K921827   Initial Coding
************************************************************************
REPORT ZCORRDABMG  LINE-SIZE 132 LINE-COUNT 65
                        NO STANDARD PAGE HEADING message-id db .


TABLES: EKKO, EKPO, EKET, EKES.

DATA: T_EKPO LIKE EKPO OCCURS 0 WITH HEADER LINE.
DATA:    BEGIN OF ETT OCCURS 50.
        INCLUDE STRUCTURE BEKET.
DATA:    END OF ETT.
DATA: BEGIN OF YEKET OCCURS 20.
        INCLUDE STRUCTURE UEKET                         .
DATA: END OF YEKET                          .
DATA: BEGIN OF T_EKET OCCURS 20.
        INCLUDE STRUCTURE EKET                         .
DATA: END OF T_EKET                          .

DATA: EKESOK.              "filled from Import-parameter E_EKESOK
DATA: H_FUNKT(2).          "function parameter for confirmation program

DATA:  BEGIN OF L_EKPODATA OCCURS 0,   "Schedul. Agreements
          EBELN LIKE EKPO-EBELN,
          EBELP LIKE EKPO-EBELP,
       END OF L_EKPODATA.

DATA:  BEGIN OF F_EKPODATA OCCURS 0,   "Purchase Orders
          EBELN LIKE EKPO-EBELN,
          EBELP LIKE EKPO-EBELP,
       END OF F_EKPODATA.

* select-options: s_ebeln  for ekes-ebeln.
SELECT-OPTIONS: PO_NR FOR EKPO-EBELN.
SELECT-OPTIONS: PO_ITEM FOR EKPO-EBELP.
PARAMETERS ONLYLST AS CHECKBOX DEFAULT 'X'. "Only list, no batch Input
PARAMETERS:
      GROUP1(12) DEFAULT 'znv_sched.agr.' OBLIGATORY,
      GROUP2(12) DEFAULT 'znv_POs' OBLIGATORY,
      USER(12)     OBLIGATORY,         "user for start session in batch
      KEEP AS CHECKBOX,                "' ' = delete session if finished
                                       "'X' = keep   session if finished
      HOLDDATE LIKE SY-DATUM.

*     Batchinputdata of single transaction
DATA: BEGIN OF BDCDATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.


*----------------------------------------------------------------------*
*   create batchinput session                                          *
*----------------------------------------------------------------------*
FORM OPEN_GROUP USING GROUP.
  SKIP.
  WRITE: /(20) 'Create group'(I01), GROUP.
  SKIP.
* open batchinput group.
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
ENDFORM.

*----------------------------------------------------------------------*
*   end batchinput session                                             *
*----------------------------------------------------------------------*
FORM CLOSE_GROUP.
* close batchinput group
  CALL FUNCTION 'BDC_CLOSE_GROUP'.
  WRITE: /(30) 'BDC_CLOSE_GROUP'(I04),
          (12) 'returncode:'(I05),
               SY-SUBRC.
ENDFORM.

*----------------------------------------------------------------------*
*        start new transaction                                         *
*----------------------------------------------------------------------*
FORM BDC_TRANSACTION USING TCODE.
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE     = TCODE
       TABLES
            DYNPROTAB = BDCDATA.
  WRITE: /(25) 'BDC_INSERT'(I03),
               TCODE,
          (12) 'returncode:'(I05),
               SY-SUBRC.
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
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM ENTER_DOCUMENT_L                                         *
*---------------------------------------------------------------------*
FORM ENTER_DOCUMENT_L USING EBELN LIKE EKPO-EBELN.   "sched. agr.

  DATA: FVAL LIKE BDCDATA-FVAL.

  FVAL = EBELN.

  PERFORM BDC_DYNPRO      USING 'SAPMM06E' '0205'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RM06E-EVRTN'.
  PERFORM BDC_FIELD       USING 'RM06E-EVRTN'
                                FVAL.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM SELECT_ITEM_L                                            *
*---------------------------------------------------------------------*
FORM SELECT_ITEM_L    USING ITEM LIKE EKPO-EBELP.

  DATA: FVAL LIKE BDCDATA-FVAL.

  FVAL = ITEM.

  PERFORM BDC_DYNPRO      USING 'SAPMM06E' '0222'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RM06E-EBELP'.
  PERFORM BDC_FIELD       USING 'RM06E-EBELP'
                                FVAL.
  PERFORM BDC_FIELD       USING 'RM06E-TCSELFLAG(01)'
                                ''.    "'X'.
  PERFORM BDC_DYNPRO      USING 'SAPMM06E' '0222'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BSTA'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RM06E-EVRTP(01)'.
  PERFORM BDC_FIELD       USING 'RM06E-TCSELFLAG(01)'
                                 'X'.
ENDFORM.


*---------------------------------------------------------------------*
*     FORM REDISTRIBUTION                                             *
*---------------------------------------------------------------------*
FORM REDISTRIBUTION.

  PERFORM BDC_DYNPRO      USING 'SAPLEINB' '0200'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=NEUV'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RM06E-LPEIN(01)'.
  PERFORM BDC_DYNPRO      USING 'SAPLEINB' '0200'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BACK'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RM06E-LPEIN(01)'.

ENDFORM.

*----------------------------------------------------------------------*
*   form save_document_l                                               *
*----------------------------------------------------------------------*
FORM SAVE_DOCUMENT_L.

  PERFORM BDC_DYNPRO      USING 'SAPMM06E' '0222'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BU'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                 'RM06E-TCSELFLAG(01)'.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM DO_TRANSACTION_ME38                                      *
*---------------------------------------------------------------------*
FORM DO_TRANSACTION_ME38.

  PERFORM BDC_TRANSACTION USING 'ME38'.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM REPAIR_L                                                 *
*---------------------------------------------------------------------*
FORM REPAIR_L USING DOCNR LIKE EKPO-EBELN.

  LOOP AT L_EKPODATA WHERE EBELN = DOCNR.

    REFRESH BDCDATA.

    PERFORM ENTER_DOCUMENT_L USING DOCNR.

    SKIP.

    WRITE: /(32) 'REPAIRING',
            (10) 'document:', 44 DOCNR .
    WRITE: /38   'item:', 44  L_EKPODATA-EBELP.

    PERFORM SELECT_ITEM_L           USING L_EKPODATA-EBELP.
    PERFORM REDISTRIBUTION.
    PERFORM SAVE_DOCUMENT_L.
    PERFORM DO_TRANSACTION_ME38.

  ENDLOOP.

  SKIP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM ENTER_DOCUMENT_F                                         *
*---------------------------------------------------------------------*
FORM ENTER_DOCUMENT_F USING EBELN LIKE EKPO-EBELN.          "POs

  DATA: FVAL LIKE BDCDATA-FVAL.

  FVAL = EBELN.

  PERFORM BDC_DYNPRO      USING 'SAPMM06E' '0105'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RM06E-BSTNR'.
  PERFORM BDC_FIELD       USING 'RM06E-BSTNR'
                                FVAL.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM SELECT_ITEM_F                                            *
*---------------------------------------------------------------------*
FORM SELECT_ITEM_F    USING ITEM LIKE EKPO-EBELP.

  DATA: FVAL LIKE BDCDATA-FVAL.

  FVAL = ITEM.

  PERFORM BDC_DYNPRO      USING 'SAPMM06E' '0120'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RM06E-EBELP'.
  PERFORM BDC_FIELD       USING 'RM06E-EBELP'
                                FVAL.
  PERFORM BDC_FIELD       USING 'RM06E-TCSELFLAG(01)'
                                ''.    "'X'.
  PERFORM BDC_DYNPRO      USING 'SAPMM06E' '0120'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BSTA'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RM06E-BSTPO(01)'.
  PERFORM BDC_FIELD       USING 'RM06E-TCSELFLAG(01)'
                                 'X'.
ENDFORM.

*----------------------------------------------------------------------*
*   form save_document_f                                               *
*----------------------------------------------------------------------*
FORM SAVE_DOCUMENT_F.

  PERFORM BDC_DYNPRO      USING 'SAPMM06E' '0120'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BU'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                 'RM06E-BSTPO(01)'.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM DO_TRANSACTION_ME22                                      *
*---------------------------------------------------------------------*
FORM DO_TRANSACTION_ME22.

  PERFORM BDC_TRANSACTION USING 'ME22'.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM REPAIR_F                                                 *
*---------------------------------------------------------------------*
FORM REPAIR_F USING DOCNR LIKE EKPO-EBELN.

  LOOP AT F_EKPODATA WHERE EBELN = DOCNR.

    REFRESH BDCDATA.

    PERFORM ENTER_DOCUMENT_F USING DOCNR.

    SKIP.

    WRITE: /(32) 'REPAIRING',
            (10) 'document:', 44 DOCNR .
    WRITE: /38   'item:', 44 F_EKPODATA-EBELP.

    PERFORM SELECT_ITEM_F   USING F_EKPODATA-EBELP.
    PERFORM REDISTRIBUTION.
    PERFORM SAVE_DOCUMENT_F.
    PERFORM DO_TRANSACTION_ME22.

  ENDLOOP.

  SKIP.

ENDFORM.

*----------- START-OF-SELECTION --------------------------------------*
START-OF-SELECTION.

  H_FUNKT = 'P1'.
  REFRESH: F_EKPODATA, L_EKPODATA.
  REFRESH T_EKPO.
  SELECT * FROM EKPO INTO TABLE T_EKPO WHERE
                          EBELN IN PO_NR   AND
                          EBELP IN PO_ITEM AND
                          BSTAE NE SPACE   AND
                          ELIKZ EQ SPACE   AND
                          LOEKZ EQ SPACE.
  LOOP AT T_EKPO.
    MOVE T_EKPO TO EKPO.
*check: confirmaton records exists !
      SELECT SINGLE * FROM EKES WHERE EBELN EQ EKPO-EBELN
                                AND   EBELP EQ EKPO-EBELP.
      IF SY-SUBRC = 0.
* check: candidate for redistribution
        REFRESH ETT.
        REFRESH T_EKET.
        SELECT * FROM EKET INTO TABLE T_EKET
                         WHERE EBELN = EKPO-EBELN
                          AND  EBELP = EKPO-EBELP.
        LOOP AT T_EKET.
          MOVE-CORRESPONDING T_EKET TO ETT.
          APPEND ETT.
        ENDLOOP.
        CLEAR SY-MSGID.
        CLEAR SY-MSGNO.
        CALL FUNCTION 'ME_CONFIRMATION_MAINTAIN'
             EXPORTING
                  I_BSTAE  = EKPO-BSTAE
                  I_EBELN  = EKPO-EBELN
                  I_EBELP  = EKPO-EBELP
                  I_FUNKT  = H_FUNKT
                  I_WERKS  = EKPO-WERKS
             IMPORTING
                  E_EKESOK = EKESOK
             TABLES
                  XEKET    = ETT
                  YEKET    = YEKET.
* record for redistribution is found
        IF EKESOK  = 'Z'.
          CASE EKPO-BSTYP.
            WHEN 'F'.
              F_EKPODATA-EBELN = EKPO-EBELN.
              F_EKPODATA-EBELP = EKPO-EBELP.
              APPEND F_EKPODATA.
            WHEN 'L'.
              L_EKPODATA-EBELN = EKPO-EBELN.
              L_EKPODATA-EBELP = EKPO-EBELP.
              APPEND L_EKPODATA.
            WHEN OTHERS.
              WRITE: / 'Wrong BSTYP',
                     EKPO-BSTYP, EKPO-EBELN, EKPO-EBELP.
          ENDCASE.
        ENDIF.
      ENDIF.
  ENDLOOP.
  DATA H_COUNTER LIKE SY-TABIX.

  DESCRIBE TABLE L_EKPODATA LINES H_COUNTER.
  WRITE: / H_COUNTER ,'Schedule Agreement Items found'.
  LOOP AT L_EKPODATA.
    WRITE: / L_EKPODATA-EBELN, L_EKPODATA-EBELP.
  ENDLOOP.
  DESCRIBE TABLE F_EKPODATA LINES H_COUNTER.
  WRITE: / H_COUNTER ,'Purchase Order Items found'.
  LOOP AT F_EKPODATA.
    WRITE: / F_EKPODATA-EBELN, F_EKPODATA-EBELP.
  ENDLOOP.
  CHECK ONLYLST IS INITIAL.


  CLEAR L_EKPODATA.
  SORT L_EKPODATA.
  PERFORM OPEN_GROUP USING GROUP1.
  LOOP AT L_EKPODATA.
    AT NEW EBELN.
      PERFORM REPAIR_L USING L_EKPODATA-EBELN.
    ENDAT.
  ENDLOOP.
  PERFORM CLOSE_GROUP.

  CLEAR F_EKPODATA.
  SORT F_EKPODATA.
  PERFORM OPEN_GROUP USING GROUP2.
  LOOP AT F_EKPODATA.
    AT NEW EBELN.
      PERFORM REPAIR_F USING F_EKPODATA-EBELN.
    ENDAT.
  ENDLOOP.
  PERFORM CLOSE_GROUP.
