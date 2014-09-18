*----------------------------------------------------------------------*
*   INCLUDE ZIPP503L_ENGIN_PR_F                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  CLEAR: IT_ZTPPER, IT_ERROR .

  REFRESH: IT_ZTPPER , IT_ERROR .

  IF R1 EQ 'X'.     "PROCESSING
    SELECT * FROM ZTPPER
             APPENDING TABLE IT_ZTPPER
** changed bymFurong on 10/31/2006
*             WHERE ZRESULT EQ 'I'
             WHERE ( zresult EQ 'I'
                  OR zresult EQ 'E'
                  OR zresult EQ 'C' )
** end of change
*               AND ZSLNO   IS NOT NULL
               AND ZSLNO   IN S_ZSLNO
**changed by Furong on 09/10/2005
               AND RDATE  IN S_ZSDAT.
  ELSEIF R2 EQ 'X'.  "REPROCESSING FOR ERROR
    SELECT * FROM ZTPPER
             INTO TABLE IT_ZTPPER
             WHERE ZRESULT EQ 'E'
               AND ZSLNO IN S_ZSLNO
               AND RDATE IN S_ZSDAT.

  ELSEIF R3 EQ 'X'.  "DISPLAY
    SELECT * FROM ZTPPER
             INTO TABLE IT_ZTPPER
*             WHERE ZRESULT EQ 'S'
             WHERE ZSLNO IN S_ZSLNO
               AND RDATE IN S_ZSDAT.
** END OF CHANGE
  ENDIF.
  SORT IT_ZTPPER BY TSEQ ERPID RSEQ.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
  DATA: L_TABIX LIKE SY-TABIX,
        L_MSG LIKE CFGNL-MSGLIN,
        L_TSEQ LIKE ZTPPER-TSEQ.

  LOOP AT IT_ZTPPER.
** Added by Furong on 01/09/08 for database update delay
*    IF wa_erpid = 'RUN' AND wa_matnr = it_ztpper-eitem.
*      WAIT UP TO 3 SECONDS .
*    ENDIF.
    IF wa_matnr = it_ztpper-eitem.
      WAIT UP TO 3 SECONDS .
    ENDIF.
** End of addition
    L_TABIX = SY-TABIX.

    CLEAR : DATA_GENERAL  , DATA_SPECIFIC  ,
            DATA_GENERALX , DATA_SPECIFICX ,
            DATA_INSTALL  , RETURN         ,
            WA_DATUM .

*----> CHECK Material Master
    SELECT SINGLE *
               FROM MARA
               WHERE MATNR EQ IT_ZTPPER-EITEM .
    IF SY-SUBRC EQ 0.
*----> Convert Actual Date
      CALL FUNCTION 'Z_FPP_CHANGE_DATE'
           EXPORTING
                IWERKS                     = 'E001'
                IDATE                      = IT_ZTPPER-RDATE
                ITIME                      = IT_ZTPPER-RTIME
           IMPORTING
                ODATE                      = WA_DATUM
           EXCEPTIONS
                FACTORY_CALENDAR_NOT_FOUND = 1
                HOLIDAY_CALENDAR_NOT_FOUND = 2
                DATE_HAS_INVALID_FORMAT    = 3
                DATE_INCONSISTENCY         = 4
                ERROR_TIME                 = 5
                OTHERS                     = 6.

      IF SY-SUBRC NE 0.
        WA_FLAG  = 'E'.
        WA_MSGID = 'SF'  .
        WA_MSGTY = 'E'   .
        WA_MSGNO = '999' .
        WA_MSGV1 = TEXT-202.  "Posting date is invalid
      ELSE.
*----> PROCESS Report Point
        CLEAR: wa_Erpid .
        PERFORM PROCESS_REPORT_POINT  .
      ENDIF.
    ELSE.
      WA_FLAG  = 'E'.
      WA_MSGID = 'SF'.
      WA_MSGTY = 'E'.
      WA_MSGNO = '999'.
      WA_MSGV1 = TEXT-201.   "Material Code is Invalid!!
    ENDIF.

    IF WA_FLAG EQ SPACE .
      IF RETURN-TYPE EQ SPACE . "       AND RETURN-MESSAGE+0(1) EQ 'S'.
        PERFORM IT_ZTPPER_MODIFY USING L_MSG
                                       'S'
                                       L_TABIX.
      ELSE.
        PERFORM IT_ZTPPER_MODIFY USING RETURN-MESSAGE
                                       RETURN-TYPE
                                       L_TABIX.
      ENDIF.
    ELSE.
      PERFORM RKC_MSG_STRING1 USING WA_MSGID
                                    WA_MSGTY
                                    WA_MSGNO
                                    WA_MSGV1
                                    WA_MSGV2
                                    WA_MSGV3
                                    WA_MSGV4
                              CHANGING L_MSG  .

      IF WA_MSGTY EQ SPACE.  "AND RETURN-MESSAGE+0(1) EQ 'S'.
        PERFORM IT_ZTPPER_MODIFY USING L_MSG
                                       'S'
                                       L_TABIX.
      ELSE.
        PERFORM IT_ZTPPER_MODIFY USING L_MSG
                                       WA_MSGTY
                                       L_TABIX.
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING IT_ZTPPER TO WA_ZTPPER.
    IF IT_ZTPPER-ZRESULT EQ 'E' .
      MOVE-CORRESPONDING IT_ZTPPER TO IT_ERROR .
      APPEND IT_ERROR .
    ENDIF.

    UPDATE ZTPPER FROM WA_ZTPPER.
    IF SY-SUBRC EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    ENDIF.
    CLEAR: WA_EQUNR   ,
           WA_ZTPPER  ,
           WA_FLAG    ,
           WA_MSGID   ,
           WA_MSGTY   ,
           WA_MSGNO   ,
           WA_MSGV1   ,
           WA_MSGV2   ,
           WA_MSGV3   ,
           WA_MSGV4   ,
           IT_ERROR   .
** Added by Furong on 01/09/08 for database update delay
    wa_matnr = it_ztpper-eitem.
** End of change
  ENDLOOP.
ENDFORM.                    " DATA_PROCESS

*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING
*&---------------------------------------------------------------------*
FORM RKC_MSG_STRING CHANGING P_MSG.
  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            ID      = SY-MSGID
            MTYPE   = SY-MSGTY
            NUMBER  = SY-MSGNO
            PAR1    = SY-MSGV1
            PAR2    = SY-MSGV2
            PAR3    = SY-MSGV3
            PAR4    = SY-MSGV4
       IMPORTING
            MSG_LIN = P_MSG
       EXCEPTIONS
            OTHERS  = 1.
ENDFORM.                    " RKC_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING1
*&---------------------------------------------------------------------*
FORM RKC_MSG_STRING1 USING P_MSGID
                           P_MSGTY
                           P_MSGNO
                           P_MSGV1
                           P_MSGV2
                           P_MSGV3
                           P_MSGV4
                     CHANGING P_MSG.

  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            ID      = P_MSGID
            MTYPE   = P_MSGTY
            NUMBER  = P_MSGNO
            PAR1    = P_MSGV1
            PAR2    = P_MSGV2
            PAR3    = P_MSGV3
            PAR4    = P_MSGV4
       IMPORTING
            MSG_LIN = P_MSG
       EXCEPTIONS
            OTHERS  = 1.

ENDFORM.                    " RKC_MSG_STRING1
*&---------------------------------------------------------------------*
*&      Form  IT_ZTPPER_MODIFY
*&---------------------------------------------------------------------*
FORM IT_ZTPPER_MODIFY USING P_MSG
                            P_MSGTY
                            P_TABIX.

  CASE P_MSGTY.
    WHEN 'S'.
      IT_ZTPPER-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
      IT_ZTPPER-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ZTPPER-ZBNAM = SY-UNAME.  "BDC User ID
*     C: Create U: Update D: Delete
      IT_ZTPPER-ZMODE    = 'C'.       "C:CREATE
      IT_ZTPPER-ZRESULT  = 'S'.       "SUCCESS
      IT_ZTPPER-ZMSG     = P_MSG.
      IT_ZTPPER-ZRESULT  = P_MSGTY.

    WHEN 'I'.
      IT_ZTPPER-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
      IT_ZTPPER-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ZTPPER-ZBNAM = SY-UNAME.  "BDC User ID
*     C: Create U: Update D: Delete
      IT_ZTPPER-ZMODE   = 'C'.       "C:CREATE
      IT_ZTPPER-ZRESULT = 'S'.       "P_MSGTY.
      IT_ZTPPER-ZMSG    = 'Classification Update successfully!!'.

    WHEN 'E'.
      IT_ZTPPER-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
      IT_ZTPPER-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ZTPPER-ZBNAM = SY-UNAME.  "BDC User ID
      IT_ZTPPER-ZMODE = 'C'.       "C:CREATE
*      IT_ZTPPER-FLAG  = 'E'.       "ERROR
      IT_ZTPPER-ZMSG    = P_MSG.
      IT_ZTPPER-ZRESULT = 'E' .

    WHEN OTHERS.
      IT_ZTPPER-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
      IT_ZTPPER-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ZTPPER-ZBNAM = SY-UNAME.  "BDC User ID
      IT_ZTPPER-ZMODE = 'C'.       "C:CREATE
      IT_ZTPPER-ZMSG    = P_MSG.
      IT_ZTPPER-ZRESULT = 'S' .
  ENDCASE.

  MODIFY IT_ZTPPER INDEX P_TABIX TRANSPORTING ZBDAT
                                              ZBTIM
                                              ZBNAM
                                              ZMODE
                                              ZRESULT
                                              ZMSG.

ENDFORM.                    " IT_ZTPPER_MODIFY
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E00
*&---------------------------------------------------------------------*
FORM EN_CID_E00.

*  check assy ID.
  SELECT SINGLE * FROM EQUI
         WHERE EQUNR EQ WA_EQUNR .  "IT_ZTPPER-EASSYID.
  IF SY-SUBRC NE 0.
    CLEAR ZTPPER_MAPPING .
    SELECT SINGLE * FROM ZTPPER_MAPPING.

    EXTERNAL_NUMBER         = IT_ZTPPER-EASSYID.      "Equipment Master
* General Tab
    DATA_GENERAL-OBJECTTYPE = ZTPPER_MAPPING-EQART.	"Object Type
    DATA_GENERAL-DESCRIPT   = ZTPPER_MAPPING-SHTXT.	"Description

* Location Tab
    DATA_GENERAL-MAINTPLANT = ZTPPER_MAPPING-SWERK.   "Maint Plant
    DATA_GENERAL-MAINTLOC   = ZTPPER_MAPPING-STORT.   "Maint Location
    DATA_GENERAL-MAINTROOM  = ZTPPER_MAPPING-MSGRP.	"Room
    DATA_GENERAL-PLSECTN    = ZTPPER_MAPPING-BEBER.	"Plant Section

*    DATA_GENERAL-PP_WKCTR   = ZTPPER_MAPPING-ARBPL.	"Object ID (W/C)
    SELECT SINGLE OBJID
           INTO DATA_GENERAL-PP_WKCTR
           FROM CRHD
           WHERE ARBPL EQ ZTPPER_MAPPING-ARBPL
             AND WERKS EQ 'E001' .  "ZTPPER_MAPPING-WERGW .

* Organization Tab
    DATA_GENERAL-COMP_CODE  = ZTPPER_MAPPING-BUKRS.	"Company Code
    DATA_GENERAL-COSTCENTER = ZTPPER_MAPPING-KOSTL.	"Cost Center
    DATA_GENERAL-PLANPLANT  = ZTPPER_MAPPING-IWERK.	"Planning Plant
    DATA_GENERAL-PLANGROUP  = ZTPPER_MAPPING-INGRP.   "Planner Group

*    DATA_GENERAL-WORK_CTR   = ZTPPER_MAPPING-ARBPL.  "Work Center
    SELECT SINGLE OBJID
           INTO DATA_GENERAL-WORK_CTR                "Work Center
           FROM CRHD
           WHERE ARBPL EQ ZTPPER_MAPPING-GEWRK      "Work Center
             AND WERKS EQ 'E001' .  "ZTPPER_MAPPING-WERGW  "Maint W/C

* Sales and Distribution Tab
    DATA_GENERAL-SALES_ORG  = ZTPPER_MAPPING-VKORG.   "Sales Org
    DATA_GENERAL-DISTR_CHAN = ZTPPER_MAPPING-VTWEG.   "Distribution
    DATA_GENERAL-DIVISION   = ZTPPER_MAPPING-SPART.   "Division

* Ser Data Tab
    DATA_SPECIFIC-MATERIAL   = IT_ZTPPER-EITEM.       "ITEM
    DATA_SPECIFIC-EQUICATGRY = ZTPPER_MAPPING-EQTYP.  "Equi Category 'E'

    CALL FUNCTION 'BAPI_EQUI_CREATE'
         EXPORTING
              EXTERNAL_NUMBER   = EXTERNAL_NUMBER
              DATA_GENERAL      = DATA_GENERAL
              DATA_SPECIFIC     = DATA_SPECIFIC
              VALID_DATE        = WA_DATUM
              DATA_INSTALL      = DATA_INSTALL
         IMPORTING
              EQUIPMENT         = EXTERNAL_NUMBER
              DATA_GENERAL_EXP  = DATA_GENERAL
              DATA_SPECIFIC_EXP = DATA_SPECIFIC
              RETURN            = RETURN.

*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*    WAIT UP TO 1 SECONDS.

    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
         EXPORTING
              TYPE   = RETURN-TYPE
              CL     = RETURN-ID
              NUMBER = RETURN-NUMBER
              PAR1   = RETURN-MESSAGE_V1
              PAR2   = RETURN-MESSAGE_V2
              PAR3   = RETURN-MESSAGE_V3
              PAR4   = RETURN-MESSAGE_V4
         IMPORTING
              RETURN = RETURN.

    IF RETURN-TYPE  = 'E'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
*----> Assign class to Engine Master
      PERFORM ASSIGN_CLASS USING IT_ZTPPER-EASSYID.
    ENDIF.
  ELSE.
    WA_FLAG = 'X'.
    WA_MSGID = 'SF'.
    WA_MSGTY = 'E'.
    WA_MSGNO = '999'.
    WA_MSGV1 = TEXT-203.
  ENDIF.

ENDFORM.                    " EN_CID_E00
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E04
*&---------------------------------------------------------------------*
FORM EN_CID_E04.

  EXTERNAL_NUMBER           = IT_ZTPPER-EASSYID.    "Equipment number
* General Tab
*  DATA_GENERAL-START_FROM   = IT_ZTPPER-RDATE.      "Start-up date
*  DATA_GENERALX-START_FROM  = 'X'.
*  DATA_GENERAL-CONSTYEAR    = IT_ZTPPER-RDATE+0(4). "Year construction
*  DATA_GENERALX-CONSTYEAR   = 'X'.
*  DATA_GENERAL-CONSTMONTH   = IT_ZTPPER-RDATE+4(2). "Month constr.
*  DATA_GENERALX-CONSTMONTH  = 'X'.
  DATA_GENERAL-START_FROM   = WA_DATUM       .      "Start-up date
  DATA_GENERALX-START_FROM  = 'X'.
  DATA_GENERAL-CONSTYEAR    = WA_DATUM+0(4)  .      "Year construction
  DATA_GENERALX-CONSTYEAR   = 'X'.
  DATA_GENERAL-CONSTMONTH   = WA_DATUM+4(2)  .      "Month constr.
  DATA_GENERALX-CONSTMONTH  = 'X'.
* Specific
  DATA_SPECIFIC-EQUICATGRY  = 'E'.                  "Equipment category
  DATA_SPECIFICX-EQUICATGRY = 'X'.

  CALL FUNCTION 'BAPI_EQUI_CHANGE'
       EXPORTING
            EQUIPMENT         = EXTERNAL_NUMBER
            DATA_GENERAL      = DATA_GENERAL
            DATA_GENERALX     = DATA_GENERALX
            DATA_SPECIFIC     = DATA_SPECIFIC
            DATA_SPECIFICX    = DATA_SPECIFICX
       IMPORTING
            DATA_GENERAL_EXP  = DATA_GENERAL
            DATA_SPECIFIC_EXP = DATA_SPECIFIC
            RETURN            = RETURN
       EXCEPTIONS
            OTHERS            = 1.
*  ENDIF.

  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
       EXPORTING
            TYPE   = RETURN-TYPE
            CL     = RETURN-ID
            NUMBER = RETURN-NUMBER
            PAR1   = RETURN-MESSAGE_V1
            PAR2   = RETURN-MESSAGE_V2
            PAR3   = RETURN-MESSAGE_V3
            PAR4   = RETURN-MESSAGE_V4
       IMPORTING
            RETURN = RETURN.

  IF RETURN-TYPE  = 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
    PERFORM UPDATE_CLASSIFICATION_E04.
  ENDIF.

ENDFORM.                    " EN_CID_E04
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E05
*&---------------------------------------------------------------------*
FORM EN_CID_E05.
*  check assy ID.
  SELECT SINGLE * FROM EQUI
         WHERE EQUNR EQ WA_EQUNR .  "IT_ZTPPER-EASSYID.
  IF SY-SUBRC EQ 0 .
*    PERFORM UPDATE_CLASSIFICATION_E06.
    PERFORM EN_CID_E01 .
  ELSE.
    CLEAR ZTPPER_MAPPING .
    SELECT SINGLE * FROM ZTPPER_MAPPING.

    EXTERNAL_NUMBER         = IT_ZTPPER-EASSYID.      "Equipment Master
* General Tab
    DATA_GENERAL-OBJECTTYPE = ZTPPER_MAPPING-EQART.	"Object Type
    DATA_GENERAL-DESCRIPT   = ZTPPER_MAPPING-SHTXT.	"Description

* Location Tab
    DATA_GENERAL-MAINTPLANT = ZTPPER_MAPPING-SWERK.   "Maint Plant
    DATA_GENERAL-MAINTLOC   = ZTPPER_MAPPING-STORT.   "Maint Location
    DATA_GENERAL-MAINTROOM  = ZTPPER_MAPPING-MSGRP.	"Room
    DATA_GENERAL-PLSECTN    = ZTPPER_MAPPING-BEBER.	"Plant Section

*    DATA_GENERAL-PP_WKCTR   = ZTPPER_MAPPING-ARBPL.	"Object ID (W/C)
    SELECT SINGLE OBJID
           INTO DATA_GENERAL-PP_WKCTR
           FROM CRHD
           WHERE ARBPL EQ ZTPPER_MAPPING-ARBPL
             AND WERKS EQ 'E001' .  "ZTPPER_MAPPING-WERGW .

* Organization Tab
    DATA_GENERAL-COMP_CODE  = ZTPPER_MAPPING-BUKRS.	"Company Code
    DATA_GENERAL-COSTCENTER = ZTPPER_MAPPING-KOSTL.	"Cost Center
    DATA_GENERAL-PLANPLANT  = ZTPPER_MAPPING-IWERK.	"Planning Plant
    DATA_GENERAL-PLANGROUP  = ZTPPER_MAPPING-INGRP.   "Planner Group

*    DATA_GENERAL-WORK_CTR   = ZTPPER_MAPPING-ARBPL.  "Work Center
    SELECT SINGLE OBJID
           INTO DATA_GENERAL-WORK_CTR                "Work Center
           FROM CRHD
           WHERE ARBPL EQ ZTPPER_MAPPING-GEWRK       "Work Center
             AND WERKS EQ 'E001' .  "ZTPPER_MAPPING-WERGW  "Maint W/C

* Sales and Distribution Tab
    DATA_GENERAL-SALES_ORG  = ZTPPER_MAPPING-VKORG.   "Sales Org
    DATA_GENERAL-DISTR_CHAN = ZTPPER_MAPPING-VTWEG.   "Distribution
    DATA_GENERAL-DIVISION   = ZTPPER_MAPPING-SPART.   "Division

* Ser Data Tab
    DATA_SPECIFIC-MATERIAL   = IT_ZTPPER-EITEM.       "ITEM
    DATA_SPECIFIC-EQUICATGRY = ZTPPER_MAPPING-EQTYP.  "Equi Category 'E'

    CALL FUNCTION 'BAPI_EQUI_CREATE'
         EXPORTING
              EXTERNAL_NUMBER   = EXTERNAL_NUMBER
              DATA_GENERAL      = DATA_GENERAL
              DATA_SPECIFIC     = DATA_SPECIFIC
              VALID_DATE        = WA_DATUM
              DATA_INSTALL      = DATA_INSTALL
         IMPORTING
              EQUIPMENT         = EXTERNAL_NUMBER
              DATA_GENERAL_EXP  = DATA_GENERAL
              DATA_SPECIFIC_EXP = DATA_SPECIFIC
              RETURN            = RETURN.

*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*    WAIT UP TO 1 SECONDS.

    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
         EXPORTING
              TYPE   = RETURN-TYPE
              CL     = RETURN-ID
              NUMBER = RETURN-NUMBER
              PAR1   = RETURN-MESSAGE_V1
              PAR2   = RETURN-MESSAGE_V2
              PAR3   = RETURN-MESSAGE_V3
              PAR4   = RETURN-MESSAGE_V4
         IMPORTING
              RETURN = RETURN.

    IF RETURN-TYPE  = 'E'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
*----> Assign class to Engine Master
      PERFORM ASSIGN_CLASS USING IT_ZTPPER-EASSYID.

    ENDIF.
  ENDIF.

ENDFORM.                    " EN_CID_E05
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E06
*&---------------------------------------------------------------------*
FORM EN_CID_E06 USING P_CID .
*---> KD Engine Assy : P_CID = 'KD'
*---> not KD Engine Assy : P_CID = SPACE
  PERFORM UPDATE_CLASSIFICATION_E06 USING P_CID .
ENDFORM.                    " EN_CID_E06

*&---------------------------------------------------------------------*
*&      Form  EN_CID_E07
*&---------------------------------------------------------------------*
FORM EN_CID_E07.
  DATA: GOODSMVT_HEADER   LIKE  BAPI2017_GM_HEAD_01,
        GOODSMVT_CODE     LIKE  BAPI2017_GM_CODE,
        IT_GOODSMVT_ITEM
        LIKE TABLE OF BAPI2017_GM_ITEM_CREATE WITH HEADER LINE,
        IT_RETURN  LIKE TABLE OF BAPIRET2 WITH HEADER LINE.
  DATA: L_MBLNR    TYPE  BAPI2017_GM_HEAD_RET-MAT_DOC,
        L_MJAHR    TYPE  BAPI2017_GM_HEAD_RET-DOC_YEAR.

  IF IT_ZTPPER-EUSAGE NE SPACE.   "A/S Stock Transfer
    CLEAR : ZTPP_MIP_STK_TRA.
    SELECT SINGLE *
           FROM ZTPP_MIP_STK_TRA
           WHERE WERKS  EQ 'E001'
             AND EUSAGE EQ IT_ZTPPER-EUSAGE.

    IF SY-SUBRC EQ 0.
      GOODSMVT_HEADER-PSTNG_DATE = WA_DATUM.
      GOODSMVT_HEADER-DOC_DATE   = IT_ZTPPER-RDATE.
      GOODSMVT_HEADER-HEADER_TXT = IT_ZTPPER-RTIME.
*  GM_Code 01: Goods receipt for purchase order
*  GM_Code 02: Goods receipt for production order
*  GM_Code 03: Goods issue
*  GM_Code 04: Transfer posting
*  GM_Code 05: Other goods receipts
*  GM_Code 06: Reversal of goods movements
      GOODSMVT_CODE = '04'.
      IT_GOODSMVT_ITEM-MATERIAL  = IT_ZTPPER-EITEM.   "MATERIAL
      IT_GOODSMVT_ITEM-PLANT     = ZTPP_MIP_STK_TRA-WERKS. "Engine PLANT
      CLEAR : MARC.
      SELECT SINGLE * FROM MARC
             WHERE MATNR EQ IT_ZTPPER-EITEM          "ENGINE MATERIAL
               AND WERKS EQ ZTPP_MIP_STK_TRA-WERKS.  "ENGINE PLANT
      IT_GOODSMVT_ITEM-STGE_LOC   = MARC-LGPRO.      "FROM
      IT_GOODSMVT_ITEM-MOVE_TYPE  = ZTPP_MIP_STK_TRA-BWART. "MVT type
      IT_GOODSMVT_ITEM-ENTRY_QNT  = IT_ZTPPER-EQTY.
      IT_GOODSMVT_ITEM-ENTRY_UOM  = IT_ZTPPER-MEINS.

      IT_GOODSMVT_ITEM-MOVE_STLOC = ZTPP_MIP_STK_TRA-LGORT. "A/S Stock
      APPEND IT_GOODSMVT_ITEM.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          GOODSMVT_HEADER             = GOODSMVT_HEADER
          GOODSMVT_CODE               = GOODSMVT_CODE
        IMPORTING
          MATERIALDOCUMENT            = L_MBLNR
          MATDOCUMENTYEAR             = L_MJAHR
        TABLES
          GOODSMVT_ITEM               = IT_GOODSMVT_ITEM
*      GOODSMVT_SERIALNUMBER       =
          RETURN                      = IT_RETURN.

      READ TABLE IT_RETURN INDEX 1.
      MOVE-CORRESPONDING IT_RETURN TO RETURN.
    ENDIF.
  ENDIF.

*-----> Change Engine Master
  CLEAR ZTPPER_MAPPING .
  SELECT SINGLE * FROM ZTPPER_MAPPING.

  EXTERNAL_NUMBER          = IT_ZTPPER-EASSYID.    "Equipment number
  DATA_GENERAL-CONSTYEAR   = WA_DATUM+0(4) .   "IT_ZTPPER-RDATE+0(4).
  DATA_GENERALX-CONSTYEAR  = 'X'.
  DATA_GENERAL-CONSTMONTH  = WA_DATUM+4(2) .   "IT_ZTPPER-RDATE+4(2).
  DATA_GENERALX-CONSTMONTH = 'X'.
  DATA_GENERAL-SALES_ORG  = ZTPPER_MAPPING-VKORG.
  DATA_GENERALX-SALES_ORG  = 'X' .
  DATA_GENERAL-DISTR_CHAN = ZTPPER_MAPPING-VTWEG.
  DATA_GENERALX-DISTR_CHAN = 'X' .
  DATA_GENERAL-DIVISION   = ZTPPER_MAPPING-SPART.
  DATA_GENERALX-DIVISION   = 'X' .

  IF IT_ZTPPER-EUSAGE EQ '01'.
    DATA_SPECIFIC-SERIALNO  = IT_ZTPPER-EASSYID.
    DATA_SPECIFICX-SERIALNO = 'X'.
  ENDIF.
  DATA_SPECIFIC-EQUICATGRY = 'E'.
  DATA_SPECIFICX-EQUICATGRY = 'X'.

  CALL FUNCTION 'BAPI_EQUI_CHANGE'
       EXPORTING
            EQUIPMENT         = EXTERNAL_NUMBER
            DATA_GENERAL      = DATA_GENERAL
            DATA_GENERALX     = DATA_GENERALX
            DATA_SPECIFIC     = DATA_SPECIFIC
            DATA_SPECIFICX    = DATA_SPECIFICX
       IMPORTING
            DATA_GENERAL_EXP  = DATA_GENERAL
            DATA_SPECIFIC_EXP = DATA_SPECIFIC
            RETURN            = RETURN
       EXCEPTIONS
            OTHERS            = 1.

  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
       EXPORTING
            TYPE   = RETURN-TYPE
            CL     = RETURN-ID
            NUMBER = RETURN-NUMBER
            PAR1   = RETURN-MESSAGE_V1
            PAR2   = RETURN-MESSAGE_V2
            PAR3   = RETURN-MESSAGE_V3
            PAR4   = RETURN-MESSAGE_V4
       IMPORTING
            RETURN = RETURN.

  IF RETURN-TYPE  = 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
    PERFORM UPDATE_CLASSIFICATION_E07 .
  ENDIF.
ENDFORM.                    " EN_CID_E07

*&---------------------------------------------------------------------*
*&      Form  EN_CID_T04
*&---------------------------------------------------------------------*
FORM EN_CID_T04.
*  EXTERNAL_NUMBER          = IT_ZTPPER-EASSYID.    "Equipment number
*    DATA_GENERAL-SALES_ORG  = ZTPPER_MAPPING-VKORG.
*    DATA_GENERALX-SALES_ORG  = 'X' .
*    DATA_GENERAL-DISTR_CHAN = ZTPPER_MAPPING-VTWEG.
*    DATA_GENERALX-DISTR_CHAN = 'X' .
*    DATA_GENERAL-DIVISION   = ZTPPER_MAPPING-SPART.
*    DATA_GENERALX-DIVISION   = 'X' .

**  IF IT_ZTPPER-EUSAGE EQ '01'.
**    DATA_SPECIFIC-SERIALNO  = IT_ZTPPER-EASSYID.
**    DATA_SPECIFICX-SERIALNO = 'X'.
**  ENDIF.
*  DATA_SPECIFIC-EQUICATGRY = 'E'.
*  DATA_SPECIFICX-EQUICATGRY = 'X'.
*
*  CALL FUNCTION 'BAPI_EQUI_CHANGE'
*       EXPORTING
*            EQUIPMENT         = EXTERNAL_NUMBER
*            DATA_GENERAL      = DATA_GENERAL
*            DATA_GENERALX     = DATA_GENERALX
*            DATA_SPECIFIC     = DATA_SPECIFIC
*            DATA_SPECIFICX    = DATA_SPECIFICX
*       IMPORTING
*            DATA_GENERAL_EXP  = DATA_GENERAL
*            DATA_SPECIFIC_EXP = DATA_SPECIFIC
*            RETURN            = RETURN
*       EXCEPTIONS
*            OTHERS            = 1.
*
*  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
*       EXPORTING
*            TYPE   = RETURN-TYPE
*            CL     = RETURN-ID
*            NUMBER = RETURN-NUMBER
*            PAR1   = RETURN-MESSAGE_V1
*            PAR2   = RETURN-MESSAGE_V2
*            PAR3   = RETURN-MESSAGE_V3
*            PAR4   = RETURN-MESSAGE_V4
*       IMPORTING
*            RETURN = RETURN.
  SELECT SINGLE * FROM MARA
         WHERE MATNR EQ IT_ZTPPER-EITEM.
  IF SY-SUBRC EQ 0.
    PERFORM UPDATE_CLASSIFICATION_T04 .
  ELSE.
    WA_FLAG = 'X'.
    WA_MSGID = 'SF'.
    WA_MSGTY = 'E'.
    WA_MSGNO = '999'.
    WA_MSGV1 = 'Material Code is Invalid!!'.
  ENDIF.
ENDFORM.                    " EN_CID_T04

*&---------------------------------------------------------------------*
*&      Form  EN_CID_E98
*&---------------------------------------------------------------------*
FORM EN_CID_E98.
   SELECT SINGLE *
     FROM MARA
    WHERE MATNR EQ IT_ZTPPER-EITEM.

   IF SY-SUBRC EQ 0.
     PERFORM UPDATE_CLASSIFICATION_T04 .
   ELSE.
     WA_FLAG = 'X'.
     WA_MSGID = 'SF'.
     WA_MSGTY = 'E'.
     WA_MSGNO = '999'.
     WA_MSGV1 = 'Material Code is Invalid!!'.
   ENDIF.
ENDFORM.                    " EN_CID_E98

*&---------------------------------------------------------------------*
*&      Form  EN_CID_E99
*&---------------------------------------------------------------------*
FORM EN_CID_E99.
   DATA: L_ENCID  LIKE ZSPP_VIN_VALUE-ATWRT,
         L_MATNR  LIKE ZSPP_VIN_VALUE-ATWRT.

  CLEAR: IT_VMASTER,  IT_VMASTER[].

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT        = WA_EQUNR
            MODE          = 'R'
            DISPLAY       = 'X'
       TABLES
            VAL_TABLE     = IT_VMASTER
       EXCEPTIONS
            NO_DATA       = 1
            ERROR_MODE    = 2
            ERROR_OBJECT  = 3
            ERROR_VALUE   = 4
            OTHERS        = 5.

  READ TABLE IT_VMASTER WITH KEY ATNAM = 'EN_CID'.
  IF SY-SUBRC = 0.
    L_ENCID = IT_VMASTER-ATWRT.
  ELSE.
    " Error...
    WA_FLAG  = 'X' .
    EXIT.
  ENDIF.

  SELECT SINGLE MATNR
    FROM EQUI
    INTO L_MATNR
   WHERE EQUNR EQ WA_EQUNR.

  IF SY-SUBRC NE 0.   WA_FLAG  = 'X' .   EXIT.   ENDIF.

  CLEAR: IT_VALS, IT_VALS[], IT_VMASTER.
  CASE L_ENCID .
    WHEN 'E00' .
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_ORD_SEQ'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_OSEQ'.     IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_ORD_SEQ'.  IT_VALS-ATWRT = IT_ZTPPER-RSEQ  .
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_ORD_DATE'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_ODATE'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_ORD_DATE'. IT_VALS-ATWRT = IT_ZTPPER-RDATE .
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_ORD_TIME'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_OTIME'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_ORD_TIME'. IT_VALS-ATWRT = IT_ZTPPER-RTIME .
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
    WHEN 'E04' .
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_IN_SEQ'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_OSEQ'.     IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_IN_SEQ'.   IT_VALS-ATWRT = IT_ZTPPER-RSEQ  .
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_IN_DATE'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_ODATE'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_IN_DATE'.  IT_VALS-ATWRT = IT_ZTPPER-RDATE .
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_IN_TIME'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_OTIME'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_IN_TIME'.  IT_VALS-ATWRT = IT_ZTPPER-RTIME .
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
    WHEN 'E05' .
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_COMPL_SEQ'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_OSEQ'.     IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_COMPL_SEQ'.
                                       IT_VALS-ATWRT = IT_ZTPPER-RSEQ  .
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_COMPL_DATE'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_ODATE'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_COMPL_DATE'.
                                       IT_VALS-ATWRT = IT_ZTPPER-RDATE .
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_COMPL_TIME'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_OTIME'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_COMPL_TIME'.
                                       IT_VALS-ATWRT = IT_ZTPPER-RTIME .
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
    WHEN 'E07' .
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_OUT_SEQ'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_OSEQ'.     IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_OUT_SEQ'. IT_VALS-ATWRT = IT_ZTPPER-RSEQ  .
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_OUT_DATE'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_ODATE'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_OUT_DATE'. IT_VALS-ATWRT = IT_ZTPPER-RDATE .
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_OUT_TIME'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_OTIME'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_OUT_TIME'. IT_VALS-ATWRT = IT_ZTPPER-RTIME .
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
    WHEN 'T04' .
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_MOUNT_SEQ'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_OSEQ'.     IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_MOUNT_SEQ'.
                                       IT_VALS-ATWRT = IT_ZTPPER-RSEQ .
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_MOUNT_DATE'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_ODATE'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_MOUNT_DATE'.
                                       IT_VALS-ATWRT = IT_ZTPPER-RDATE.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_MOUNT_TIME'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_OTIME'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_MOUNT_TIME'.
                                       IT_VALS-ATWRT = IT_ZTPPER-RTIME.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
  ENDCASE.

  READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_ITEM_CODE'.
  IF SY-SUBRC = 0.
    IT_VALS-ATNAM = 'EN_OITEM_CODE'.   IT_VALS-ATWRT = IT_VMASTER-ATWRT.
    APPEND IT_VALS.                    CLEAR: IT_VALS, IT_VMASTER.
    IT_VALS-ATNAM = 'EN_ITEM_CODE'.    IT_VALS-ATWRT = IT_ZTPPER-EITEM.
    APPEND IT_VALS.                    CLEAR: IT_VALS, IT_VMASTER.
  ENDIF.

  READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_CHG_CNT'.
  IF SY-SUBRC = 0.
    IT_VMASTER-ATWRT = IT_VMASTER-ATWRT + 1 .
    IT_VALS-ATNAM = 'EN_CHG_CNT'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
    APPEND IT_VALS.                  CLEAR: IT_VALS, IT_VMASTER.
  ELSE.
    IT_VALS-ATNAM = 'EN_CHG_CNT'.    IT_VALS-ATWRT = 1 .
    APPEND IT_VALS.                  CLEAR: IT_VALS, IT_VMASTER.
  ENDIF.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object             = WA_EQUNR
      MODE               = 'W'
    tables
      val_table          = IT_VALS
    EXCEPTIONS
      NO_DATA            = 1
      ERROR_MODE         = 2
      ERROR_OBJECT       = 3
      ERROR_VALUE        = 4
      OTHERS             = 5 .

  IF sy-subrc <> 0.
     WA_FLAG  = 'X' .
  ENDIF.
ENDFORM.                    " EN_CID_E99

*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_E00
*&---------------------------------------------------------------------*
FORM UPDATE_CLASSIFICATION_E00.
  DATA: L_CNT LIKE ZSPP_VIN_VALUE-ATWRT.

  L_CNT = '0' .
  CLEAR : IT_VMASTER,  IT_VMASTER[] .
  PERFORM APPEND_VMASTER USING 'EN_CID'        IT_ZTPPER-ERPID  .
  PERFORM APPEND_VMASTER USING 'EN_ORD_DATE'   IT_ZTPPER-RDATE  .
*  PERFORM APPEND_VMASTER USING 'EN_ORD_DATE'   WA_DATUM         .
  PERFORM APPEND_VMASTER USING 'EN_ORD_TIME'   IT_ZTPPER-RTIME  .
  PERFORM APPEND_VMASTER USING 'EN_ORD_SEQ'    IT_ZTPPER-RSEQ   .
  PERFORM APPEND_VMASTER USING 'EN_CHG_CNT'    L_CNT            .
  PERFORM APPEND_VMASTER USING 'EN_PLD_NO'     IT_ZTPPER-EPONO  .
  PERFORM APPEND_VMASTER USING 'EN_ITEM_CODE'  IT_ZTPPER-EITEM  .

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT     = WA_EQUNR
            MODE       = 'W'
            CTYPE      = '002'
       TABLES
            VAL_TABLE  = IT_VMASTER
       EXCEPTIONS
            NO_DATA    = 1
            ERROR_MODE = 2
            OTHERS     = 3.
  IF SY-SUBRC NE 0.
    WA_FLAG  = 'X' .
    WA_MSGID = SY-MSGID .
    WA_MSGTY = SY-MSGTY .
    WA_MSGNO = SY-MSGNO .
    WA_MSGV1 = SY-MSGV1 .
  ENDIF.
ENDFORM.                    " UPDATE_CLASSIFICATION_E00


*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_E04
*&---------------------------------------------------------------------*
FORM UPDATE_CLASSIFICATION_E04 .

  CLEAR: IT_VMASTER,
         IT_VMASTER[].
  PERFORM APPEND_VMASTER USING 'EN_CID'        IT_ZTPPER-ERPID  .
  PERFORM APPEND_VMASTER USING 'EN_IN_DATE'    IT_ZTPPER-RDATE  .
*  PERFORM APPEND_VMASTER USING 'EN_IN_DATE'    WA_DATUM         .
  PERFORM APPEND_VMASTER USING 'EN_IN_TIME'    IT_ZTPPER-RTIME  .
  PERFORM APPEND_VMASTER USING 'EN_IN_SEQ'     IT_ZTPPER-RSEQ   .

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT     = WA_EQUNR
            MODE       = 'W'
            CMODE      = '002'
       TABLES
            VAL_TABLE  = IT_VMASTER
       EXCEPTIONS
            NO_DATA    = 1
            ERROR_MODE = 2
            OTHERS     = 3.
  IF SY-SUBRC NE 0.
    WA_FLAG  = 'X' .
    WA_MSGID = SY-MSGID .
    WA_MSGTY = SY-MSGTY .
    WA_MSGNO = SY-MSGNO .
    WA_MSGV1 = SY-MSGV1 .
  ENDIF.
ENDFORM.                    " UPDATE_CLASSIFICATION_E04

*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_E06
*&---------------------------------------------------------------------*
FORM UPDATE_CLASSIFICATION_E06 USING P_CID .
*---> KD Engine Assy : P_CID = 'KD'
*---> not KD Engine Assy : P_CID = SPACE
  CLEAR: IT_VMASTER,
         IT_VMASTER[].
  PERFORM APPEND_VMASTER USING 'EN_CID'        IT_ZTPPER-ERPID  .
  PERFORM APPEND_VMASTER USING 'EN_ITEM_CODE'  IT_ZTPPER-EITEM  .
  PERFORM APPEND_VMASTER USING 'EN_COMPL_DATE' IT_ZTPPER-RDATE  .
*  PERFORM APPEND_VMASTER USING 'EN_COMPL_DATE' WA_DATUM         .
  PERFORM APPEND_VMASTER USING 'EN_COMPL_TIME' IT_ZTPPER-RTIME  .
  PERFORM APPEND_VMASTER USING 'EN_COMPL_SEQ'  IT_ZTPPER-RSEQ   .

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT     = WA_EQUNR
            MODE       = 'W'
            CMODE      = '002'
       TABLES
            VAL_TABLE  = IT_VMASTER
       EXCEPTIONS
            NO_DATA    = 1
            ERROR_MODE = 2
            OTHERS     = 3.
  IF SY-SUBRC NE 0.
    WA_FLAG  = 'X' .
    WA_MSGID = SY-MSGID .
    WA_MSGTY = SY-MSGTY .
    WA_MSGNO = SY-MSGNO .
    WA_MSGV1 = SY-MSGV1 .
  ELSE.
    IF P_CID NE 'KD' .
     PERFORM SELECT_MAT_DOC .
    ENDIF.
  ENDIF.
ENDFORM.                    " UPDATE_CLASSIFICATION_E06

*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_E07
*&---------------------------------------------------------------------*
FORM UPDATE_CLASSIFICATION_E07.

  CLEAR: IT_VMASTER,
         IT_VMASTER[].

  PERFORM APPEND_VMASTER USING 'EN_CID'        IT_ZTPPER-ERPID  .
  PERFORM APPEND_VMASTER USING 'EN_OUT_DATE'   IT_ZTPPER-RDATE  .
*  PERFORM APPEND_VMASTER USING 'EN_OUT_DATE'   WA_DATUM         .
  PERFORM APPEND_VMASTER USING 'EN_OUT_TIME'   IT_ZTPPER-RTIME  .
  PERFORM APPEND_VMASTER USING 'EN_OUT_SEQ'    IT_ZTPPER-RSEQ   .

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT     = WA_EQUNR
            MODE       = 'W'
            CMODE      = '002'
       TABLES
            VAL_TABLE  = IT_VMASTER
       EXCEPTIONS
            NO_DATA    = 1
            ERROR_MODE = 2
            OTHERS     = 3.
  IF SY-SUBRC NE 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    WA_FLAG  = 'X' .
    WA_MSGID = SY-MSGID .
    WA_MSGTY = SY-MSGTY .
    WA_MSGNO = SY-MSGNO .
    WA_MSGV1 = SY-MSGV1 .
  ENDIF.
ENDFORM.                    " UPDATE_CLASSIFICATION_E07

*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_T04
*&---------------------------------------------------------------------*
FORM UPDATE_CLASSIFICATION_T04.
  CLEAR: IT_VMASTER,  IT_VMASTER[].

  PERFORM APPEND_VMASTER USING 'EN_CID'          IT_ZTPPER-ERPID  .
  PERFORM APPEND_VMASTER USING 'EN_COMPL_DATE'   IT_ZTPPER-RDATE  .
  PERFORM APPEND_VMASTER USING 'EN_COMPL_TIME'   IT_ZTPPER-RTIME  .
* PERFORM APPEND_VMASTER USING 'EN_MOUNT_SEQ'    IT_ZTPPER-RSEQ   .

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT     = WA_EQUNR
            MODE       = 'W'
            CMODE      = '002'
       TABLES
            VAL_TABLE     = IT_VMASTER
       EXCEPTIONS
            NO_DATA       = 1
            ERROR_MODE    = 2
            ERROR_OBJECT  = 3
            ERROR_VALUE   = 4
            OTHERS        = 5.

  IF SY-SUBRC NE 0.
    WA_FLAG  = 'X' .
    WA_MSGID = SY-MSGID .
    WA_MSGTY = SY-MSGTY .
    WA_MSGNO = SY-MSGNO .
    WA_MSGV1 = SY-MSGV1 .
  ENDIF.
ENDFORM.                    " UPDATE_CLASSIFICATION_T04

*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_E98
*&---------------------------------------------------------------------*
FORM UPDATE_CLASSIFICATION_E98.

  CLEAR: IT_VMASTER,
         IT_VMASTER[].

  PERFORM APPEND_VMASTER USING 'EN_CID'          IT_ZTPPER-ERPID  .
  PERFORM APPEND_VMASTER USING 'EN_COMPL_DATE'   IT_ZTPPER-RDATE  .
  PERFORM APPEND_VMASTER USING 'EN_COMPL_TIME'   IT_ZTPPER-RTIME  .
  PERFORM APPEND_VMASTER USING 'EN_KKK_SEQ'      IT_ZTPPER-RSEQ   .

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT     = WA_EQUNR
            MODE       = 'W'
            CMODE      = '002'
       TABLES
            VAL_TABLE  = IT_VMASTER
       EXCEPTIONS
            NO_DATA    = 1
            ERROR_MODE = 2
            OTHERS     = 3.
  IF SY-SUBRC NE 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    WA_FLAG  = 'X' .
    WA_MSGID = SY-MSGID .
    WA_MSGTY = SY-MSGTY .
    WA_MSGNO = SY-MSGNO .
    WA_MSGV1 = SY-MSGV1 .
  ENDIF.
ENDFORM.                    " UPDATE_CLASSIFICATION_E98

*&---------------------------------------------------------------------*
*&      Form  ASSIGN_CLASS
*&---------------------------------------------------------------------*
FORM ASSIGN_CLASS USING P_EASSYID.
  DATA: L_EQUI    LIKE    V_EQUI       ,
        L_CLASS   TYPE    KLAH-CLASS   ,
        L_KLART   TYPE    KSSK-KLART   .

  L_EQUI-EQUNR = P_EASSYID.
  L_CLASS      = ZTPPER_MAPPING-CLASS       .
  L_KLART      = ZTPPER_MAPPING-KLART       .

  CALL FUNCTION 'EQUIPMENT_CLASS_ALLOCATE'
     EXPORTING
       EQ_CLASS                 =  L_CLASS   " ENG_ASSY_MASTER
       EQ_CLASS_TYPE            =  L_KLART                  " '002'
*       IS_STANDARD              = ' '
       INIT_NEW                 = 'X'
       LOCK_NEW                 = 'X'
       UPDATE_NEW               = 'X'
       COMMIT_NEW               = 'X'
     CHANGING
       S_EQUI                   = L_EQUI
     EXCEPTIONS
       ERR_CLASS_ALLOCATE       = 1
       OTHERS                   = 2          .

  IF SY-SUBRC <> 0.
    WA_FLAG  = 'X' .
    WA_MSGID = SY-MSGID .
    WA_MSGTY = SY-MSGTY .
    WA_MSGNO = SY-MSGNO .
    WA_MSGV1 = SY-MSGV1 .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .
*----> Update Classification
    CASE IT_ZTPPER-ERPID .
      WHEN 'E00' .
        PERFORM UPDATE_CLASSIFICATION_E00 .
      WHEN 'E05' .
        PERFORM UPDATE_CLASSIFICATION_E06 USING 'KD' .
    ENDCASE .
  ENDIF.
ENDFORM.                    " ASSIGN_CLASS
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E01
*&---------------------------------------------------------------------*
FORM EN_CID_E01.

*-----> CANCEL PROCESS
  IF NOT IT_ZTPPER-PRTNR IS INITIAL.
    CALL FUNCTION 'BAPI_REPMANCONF1_CANCEL'
         EXPORTING
              CONFIRMATION     = IT_ZTPPER-PRTNR
              POSTDATE         = WA_DATUM
         IMPORTING
              CANCCONFIRMATION = WA_CANCCO
              RETURN           = RETURN.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
    IT_ZTPPER-CANC_PRTNR = WA_CANCCO.
  ENDIF.

*& Ref. BFLUSHFLAG-BACKFLTYPE = '01'
*&                              '02' Z : Reporting Point Backflush
*&                                     : Goods issue for reporting point
*&                              '10' A : Only activity posting
*&                              '11' U : Unplanned consumption message
*&                              '12' V : Scrap Message
*&                              '20'
*------> YIELD QTY
  CLEAR : BFLUSHDATAGEN,
          BFLUSHFLAGS.
  BFLUSHFLAGS-BCKFLTYPE       = '01'            .   "Backflushing type

  BFLUSHDATAGEN-MATERIALNR    = IT_ZTPPER-EITEM .   "Material number
  BFLUSHDATAGEN-PRODPLANT     = 'E001'          .   "Plant
  BFLUSHDATAGEN-PRODVERSION   = '01'            .   "Production version

*  BFLUSHDATAGEN-POSTDATE      = IT_ZTPPER-RDATE .   "Posting date
  BFLUSHDATAGEN-POSTDATE      = WA_DATUM .          "Posting date
  BFLUSHDATAGEN-DOCDATE       = SY-DATUM        .   "Document date
  BFLUSHDATAGEN-DOCHEADERTXT  = IT_ZTPPER-RTIME .   "Doc header text
  BFLUSHDATAGEN-UNITOFMEASURE = IT_ZTPPER-MEINS .   "UoM

  IF IT_ZTPPER-EQTY > 0.
    BFLUSHDATAGEN-BACKFLQUANT   = IT_ZTPPER-EQTY.   "Yield Qty
  ENDIF.

  IF IT_ZTPPER-SQTY > 0.
    BFLUSHDATAGEN-SCRAPQUANT    = IT_ZTPPER-SQTY.   "Scrap qty
  ENDIF.

  IF IT_ZTPPER-RQTY > 0.
    BFLUSHDATAGEN-BACKFLQUANT   = IT_ZTPPER-RQTY .  "Rework Qty
    BFLUSHDATAGEN-STORAGELOC    = 'E128'         .  "Rework SLoc
  ENDIF.

*-----> BAPI FOR BACKFLUSH
  CALL FUNCTION 'BAPI_REPMANCONF1_CREATE_MTS'
       EXPORTING
            BFLUSHFLAGS   = BFLUSHFLAGS
            BFLUSHDATAGEN = BFLUSHDATAGEN
            BFLUSHDATAMTS = BFLUSHDATAMTS
       IMPORTING
            CONFIRMATION  = WA_CONFIRMATION
            RETURN        = RETURN
       TABLES
            SERIALNR      = IT_SERIALNR.

  PERFORM CALL_RETURN_MESSAGE .   "  USING L_CONFIRMATION .

ENDFORM.                    " EN_CID_E01

*&---------------------------------------------------------------------*
*&      Form  call_return_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_RETURN_MESSAGE. "  USING PA_CONFIRM.
  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
       EXPORTING
            TYPE   = RETURN-TYPE
            CL     = RETURN-ID
            NUMBER = RETURN-NUMBER
            PAR1   = RETURN-MESSAGE_V1
            PAR2   = RETURN-MESSAGE_V2
            PAR3   = RETURN-MESSAGE_V3
            PAR4   = RETURN-MESSAGE_V4
       IMPORTING
            RETURN = RETURN.

*  IF NOT PA_CONFIRM IS INITIAL.
  IF NOT WA_CONFIRMATION IS INITIAL.
    IT_ZTPPER-PRTNR = WA_CONFIRMATION . "PA_CONFIRM.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
*-----> Classification Update
    IF IT_ZTPPER-ERPID EQ 'E05' .
      PERFORM EN_CID_E06 USING SPACE .
    ELSE.
      PERFORM SELECT_MAT_DOC .
    ENDIF.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
  ENDIF.
ENDFORM.                    " call_return_message

*&---------------------------------------------------------------------*
*&      Form  APPEND_VMASTER
*&---------------------------------------------------------------------*
FORM APPEND_VMASTER USING P_ATNAM  P_ATWRT .
  CLEAR IT_VMASTER .
  IT_VMASTER-ATNAM = P_ATNAM    .
  IT_VMASTER-ATWRT = P_ATWRT    .
  APPEND IT_VMASTER.
ENDFORM.                    " APPEND_VMASTER
*&---------------------------------------------------------------------*
*&      Form  PROCESS_REPORT_POINT
*&---------------------------------------------------------------------*
FORM PROCESS_REPORT_POINT.
  CLEAR : WA_CANCCO , WA_CONFIRMATION .
  WA_EQUNR = IT_ZTPPER-EASSYID.
  CASE IT_ZTPPER-ERPID.
    WHEN 'E00'.    "Process order
      PERFORM EN_CID_E00.
    WHEN 'E01'.    "Cylinder Block of Process Line
      PERFORM EN_CID_E01.
    WHEN 'E02'.    "Crank Shaft of Process Line
      PERFORM EN_CID_E01.
    WHEN 'E03'.    "Cylinder Head of Process Line
      PERFORM EN_CID_E01.
    WHEN 'E04'.    "Engine Assembly Line
      PERFORM EN_CID_E04.
    WHEN 'E05'.    "Engine Line Out
*-----> Create Engine Assy Master for KD Engine Assy ID
      PERFORM EN_CID_E05.
*    WHEN 'E06'.    "Storage In
*      PERFORM EN_CID_E06.
    WHEN 'E07'.    "Storage Out
      PERFORM EN_CID_E07.
    WHEN 'T04'.    "
*        PERFORM EN_CID_T04.
    WHEN 'E98'.    "
       PERFORM EN_CID_E98.
    WHEN 'E99'.    "
       PERFORM EN_CID_E99.
  ENDCASE.
ENDFORM.                    " PROCESS_REPORT_POINT

*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM AT_SELECTION_SCREEN.
  DATA L_DAYS    TYPE   SY-TABIX .
*---> When displaying, only 1 day display is possible
  IF R3 EQ C_MARK.
    IF S_ZSDAT[] IS INITIAL.
      MESSAGE E001 WITH TEXT-101.
    ELSE.
      L_DAYS = S_ZSDAT-HIGH - S_ZSDAT-LOW .
      IF L_DAYS GT 0.
        MESSAGE E001 WITH TEXT-101.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " AT_SELECTION_SCREEN

*&---------------------------------------------------------------------*
*&      Form  WRITE_OPEN
*&---------------------------------------------------------------------*
FORM WRITE_OPEN.

  WRITE :/ '********** BEGIN OF PROCESS ***********'.
  WRITE AT: /001(030) 'Processing Time...(Start)' ,
             031(010) SY-DATUM                    ,
             042(010) SY-UZEIT                    .
  CASE C_MARK.
    WHEN R1.
      WRITE :/ TEXT-301.
    WHEN R2.
      WRITE :/ TEXT-302.
  ENDCASE.
  WRITE :/ '********** BEGIN OF PROCESS ***********'.
  SKIP 2.
ENDFORM.                    " WRITE_OPEN
*&---------------------------------------------------------------------*
*&      Form  WRITE_RESULT
*&---------------------------------------------------------------------*
FORM WRITE_RESULT.
  DATA : L_LINE          TYPE    SY-INDEX ,
         L_ZTPPER_IX     TYPE    SY-TABIX ,
         L_ERROR_IX      TYPE    SY-TABIX ,
         L_SUCCESS_IX    TYPE    SY-TABIX .

  DESCRIBE TABLE IT_ZTPPER  LINES   L_ZTPPER_IX .
  DESCRIBE TABLE IT_ERROR   LINES   L_ERROR_IX  .
  L_SUCCESS_IX   =  L_ZTPPER_IX  -  L_ERROR_IX  .
  WRITE:/ TEXT-311 , L_ZTPPER_IX  .
  WRITE:/ TEXT-312 , L_SUCCESS_IX .
  WRITE:/ TEXT-313 , L_ERROR_IX   .
  SKIP 2.
*  LOOP AT IT_ERROR.
*    AT FIRST.
*      FORMAT RESET INTENSIFIED ON.
*      WRITE :/ '********** BEGIN OF ERROR Detail List ***********'.
*    ENDAT.
*    L_LINE = SY-TABIX MOD 2.
*    IF L_LINE EQ 1.
*      FORMAT COLOR COL_NORMAL INTENSIFIED ON.
*    ELSE.
*      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
*    ENDIF.
*      WRITE :/ IT_ERROR-TSEQ      COLOR COL_KEY ,
*               IT_ERROR-ERPID     COLOR COL_KEY ,
*               IT_ERROR-RSEQ      COLOR COL_KEY ,
*               IT_ERROR-EITEM     COLOR COL_KEY ,
*               IT_ERROR-EASSYID   COLOR COL_KEY ,
*               IT_ERROR-EPONO     COLOR COL_KEY ,
*
*               COLOR COL_NEGATIVE,COLOR COL_NORMAL.
*    AT LAST.
*      FORMAT RESET INTENSIFIED ON.
*      WRITE :/ '********** END OF ERROR Detail List ***********'.
*    ENDAT.
*  ENDLOOP.
ENDFORM.                    " WRITE_RESULT
*&---------------------------------------------------------------------*
*&      Form  WRITE_CLOSE
*&---------------------------------------------------------------------*
FORM WRITE_CLOSE.
  SKIP 2.
  WRITE :/ '********** END OF PROCESS ***********'.
  WRITE AT: /001(030) 'Processing Time...(End)' ,
           031(010) SY-DATUM                    ,
           042(010) SY-UZEIT                    .
  WRITE :/ '********** END OF PROCESS ***********'.
ENDFORM.                    " WRITE_CLOSE
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_PROCESS
*&---------------------------------------------------------------------*
FORM EXECUTE_PROCESS.

  CASE C_MARK.
    WHEN R1 OR R2.
      PERFORM WRITE_OPEN.      "Write Header for Batch job
      PERFORM READ_PROCESS.    "Read from ZTPPER Where condition
      PERFORM DATA_PROCESS.    "Data processing for Report Point
      PERFORM WRITE_RESULT.    "Write Result for Batch job
      PERFORM WRITE_CLOSE.     "Write Botton for Batch job
    WHEN R3.
      PERFORM READ_PROCESS.    "Read from ZTPPER Where condition
      CALL SCREEN 9000 .
  ENDCASE.
ENDFORM.                    " EXECUTE_PROCESS
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET TITLEBAR '9000'.
  SET PF-STATUS 'MAIN'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_ALV_GRID OUTPUT.
  IF GS_CUSTOM_CONTAINER IS INITIAL.
*-----> CREATE OBJECT
*    CREATE OBJECT GS_APPLICATION.

    CREATE OBJECT GS_CUSTOM_CONTAINER
        EXPORTING CONTAINER_NAME = WA_CONTAINER.

    CREATE OBJECT ALV_GRID
        EXPORTING I_PARENT = GS_CUSTOM_CONTAINER.

    PERFORM  BUILD_VARIANT.
    PERFORM  BUILD_LAYOUT.
    PERFORM  BUILD_FIELDCAT.

*-----> SET OBJECT
    CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
        I_STRUCTURE_NAME              = 'ZTPPER'
        IS_VARIANT                    = GS_VARIANT
        I_SAVE                        = 'A'
*        I_DEFAULT                     = 'X'
        IS_LAYOUT                     = GS_LAYOUT
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
      CHANGING
        IT_OUTTAB                     = IT_ZTPPER[]
        IT_FIELDCATALOG               = GT_FIELDCAT[]
*        IT_SORT                       =
*        IT_FILTER                     =
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4  .

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_VARIANT
*&---------------------------------------------------------------------*
FORM BUILD_VARIANT.
  GS_VARIANT-REPORT = SY-REPID.
ENDFORM.                    " BUILD_VARIANT
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
FORM BUILD_LAYOUT.
  GS_LAYOUT-ZEBRA  = 'X'.       "ZEBRA
  GS_LAYOUT-CWIDTH_OPT = 'X'.   "OPTIMIZE COLUMN WIDTH
  GS_LAYOUT-DETAILINIT = 'X'.   "DISPLAY INITIAL VALUES ON DETAIL SCREEN
ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
FORM BUILD_FIELDCAT.
  DATA: L_STRUCT    LIKE DD02L-TABNAME.

  DATA: ZERO_FNAME1(20),
        ZERO_FNAME2(20),
        ZERO_CNT TYPE I.

  L_STRUCT = 'ZTPPER'.
  CLEAR : WA_FIELDCAT, GT_FIELDCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_BUFFER_ACTIVE        = 'X'
            I_STRUCTURE_NAME       = L_STRUCT
       CHANGING
            CT_FIELDCAT            = GT_FIELDCAT[]
       EXCEPTIONS
            INCONSISTENT_INTERFACE = 1
            PROGRAM_ERROR          = 2
            OTHERS                 = 3.

*  DELETE GT_FIELDCAT  WHERE FIELDNAME = 'MANDT' OR
*                            FIELDNAME = 'ZUSER' OR
*                            FIELDNAME = 'ZSDAT' OR
*                            FIELDNAME = 'ZSTIM' OR
*                            FIELDNAME = '' OR
*                            FIELDNAME = ''.

  LOOP AT GT_FIELDCAT INTO WA_FIELDCAT.
    PERFORM SET_FIELD_INFO USING WA_FIELDCAT.
    MODIFY GT_FIELDCAT FROM WA_FIELDCAT.
    CLEAR WA_FIELDCAT.
  ENDLOOP.

ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_INFO
*&---------------------------------------------------------------------*
FORM SET_FIELD_INFO USING L_FIELDCAT STRUCTURE LVC_S_FCAT.

*  CASE L_FIELDCAT-FIELDNAME.
*    WHEN 'FLG'.
*      SET_FIELDCAT  'FLG' 3.
*      L_FIELDCAT-KEY = 'X'.
*    WHEN 'MODL'.
*      SET_FIELDCAT 'MODEL' 3.
*      L_FIELDCAT-KEY = 'X'.
*    WHEN 'VHNO'.
*      SET_FIELDCAT 'BODY NO.' 7.
*      L_FIELDCAT-KEY = 'X'.
*    WHEN 'ORDR'.
*      SET_FIELDCAT 'WORK ORDER' 9.
*    WHEN 'DIST'.
*      SET_FIELDCAT 'DIST.' 5.
*    WHEN 'INTC'.
**      SET_FIELDCAT 'INT'  3.
*    WHEN 'EXTC'.
**      SET_FIELDCAT 'EXT'  3.
*    WHEN 'VINN'.
*      SET_FIELDCAT 'VIN' 17.
*    WHEN 'K01PNO'.
**      SET_FIELDCAT 'Packing No' 8.
*    WHEN 'PLNT'.
**      SET_FIELDCAT 'Plant' 2.
*    WHEN 'LINE'.
**      SET_FIELDCAT 'Line'  2.
*    WHEN 'SSR1'.
*      SET_FIELDCAT 'SEQ' 4.
*    WHEN 'P_EMMISSION'.
*      SET_FIELDCAT 'CAL.' 2.
*    WHEN 'EVL1'.
*      SET_FIELDCAT 'VAL1' 4.
*    WHEN 'EVL2'.
*      SET_FIELDCAT 'VAL2' 4.
*    WHEN 'EVL3'.
*      SET_FIELDCAT 'VAL3' 4.
*    WHEN 'EVL4'.
*      SET_FIELDCAT 'VAL4' 4.
*    WHEN 'EVL5'.
*      SET_FIELDCAT 'VAL5' 4.
**   WHEN 'P_SEQ_DATE'.                " Sequence Date
**     SET_FIELDCAT 'SEQ Date' 10.
*    WHEN 'P_ENGINE_NO'.
*      SET_FIELDCAT 'ENGINE ASSY ID' 15.
*    WHEN 'P_KEY_NO'.
*      SET_FIELDCAT 'KEY' 6.
*    WHEN 'P_TM_NO'.
*      SET_FIELDCAT 'KEY' 15.
*    WHEN 'CDAT'.
*      SET_FIELDCAT 'CREATE DATE' 10.
*    WHEN 'CTIM'.
*      SET_FIELDCAT 'CREATE TIME' 10.
*    WHEN 'ZEDAT'.
*      SET_FIELDCAT 'I/F DATE' 10.
*  ENDCASE.

ENDFORM.                    " SET_FIELD_INFO
*&---------------------------------------------------------------------*
*&      Module  SET_CURSOR_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_CURSOR_FIELD OUTPUT.
  SET CURSOR FIELD WA_FNAME_TX LINE WA_SAVELINE_IX.
ENDMODULE.                 " SET_CURSOR_FIELD  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  CASE OK_CODE.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_CURSOR_FIELD INPUT.
  CLEAR: WA_FNAME_TX, WA_SAVELINE_IX.
  GET CURSOR FIELD WA_FNAME_TX LINE WA_SAVELINE_IX.
ENDMODULE.                 " GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*&      Form  SELECT_MAT_DOC
*&---------------------------------------------------------------------*
FORM SELECT_MAT_DOC.
  DATA : L_BELNR     TYPE   BLPP-BELNR  .  "Material Doc#
*---->
  CLEAR : BLPK, BLPP.
  SELECT SINGLE *
              FROM BLPK
              WHERE PRTNR EQ WA_CONFIRMATION
                AND MATNR EQ IT_ZTPPER-EITEM
                AND WERKS EQ 'E001'
                AND VERID EQ '01'      .
  IF SY-SUBRC NE 0.

  ELSE.
    SELECT SINGLE BELNR
                  INTO L_BELNR
                  FROM BLPP
                  WHERE PRTNR  EQ  BLPK-PRTNR
                    AND PRTPS  EQ  '0001' .
    IF SY-SUBRC NE 0.
    ELSE.
      CLEAR : RETURN-MESSAGE, RETURN-TYPE .
      MOVE 'S'      TO   RETURN-TYPE .
      CONCATENATE 'GR:' L_BELNR INTO RETURN-MESSAGE
                               SEPARATED BY SPACE .
    ENDIF.
  ENDIF.

ENDFORM.                    " SELECT_MAT_DOC
