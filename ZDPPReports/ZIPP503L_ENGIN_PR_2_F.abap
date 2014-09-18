*----------------------------------------------------------------------*
*   INCLUDE ZIPP503L_ENGIN_PR_F                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  CLEAR: IT_ZTPPERM, IT_ERROR .

  REFRESH: IT_ZTPPERM, IT_ERROR .
** Added by Furong on 01/11/10
  UPDATE ZTPPERM SET: ZRESULT = 'S'
          ZBDAT = SY-DATUM
          ZBTIM = SY-UZEIT
          ZBNAM = SY-UNAME
         WHERE ( ERPID = 'E91'
           OR  ERPID = 'E92'
           OR  ERPID = 'E93' )
           AND ZRESULT = 'I'.
  IF SY-SUBRC = 0.
    COMMIT WORK.
  ENDIF.
** End of change
  IF R1 EQ 'X'.     "PROCESSING
    SELECT * FROM ZTPPERM
             APPENDING TABLE IT_ZTPPERM
** changed by Furong on 10/31/2006
*             WHERE ZRESULT EQ 'I'
             WHERE ( ZRESULT EQ 'I'
                  OR ZRESULT EQ 'E'
                  OR ZRESULT EQ 'C' )
** end of change
*               AND ZSLNO   IS NOT NULL
               AND ZSLNO   IN S_ZSLNO
**changed by Furong on 09/10/2005
               AND RDATE  IN S_ZSDAT.
  ELSEIF R2 EQ 'X'.  "REPROCESSING FOR ERROR
    SELECT * FROM ZTPPERM
             INTO TABLE IT_ZTPPERM
             WHERE ZRESULT EQ 'E'
               AND ZSLNO IN S_ZSLNO
               AND RDATE IN S_ZSDAT.

  ELSEIF R3 EQ 'X'.  "DISPLAY
    SELECT * FROM ZTPPERM
             INTO TABLE IT_ZTPPERM
*             WHERE ZRESULT EQ 'S'
             WHERE ZSLNO IN S_ZSLNO
               AND RDATE IN S_ZSDAT.
** END OF CHANGE
  ENDIF.
** Changed by Furong on 12/17/09
*  SORT IT_ZTPPERM BY TSEQ ERPID RSEQ.
** Changed by Furong on 04/26/10
  SORT IT_ZTPPERM BY  RDATE TSEQ ERPID.
*  SORT IT_ZTPPERM BY ERPID RDATE RSEQ.
** End of change on 04/26/10
** End of change
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
  DATA: L_TABIX LIKE SY-TABIX,
        L_MSG LIKE CFGNL-MSGLIN,
        L_TSEQ LIKE ZTPPERM-TSEQ.

  LOOP AT IT_ZTPPERM.
** Added by Furong on 01/09/08 for database update delay
*    IF wa_erpid = 'RUN' AND wa_matnr = it_ztpperM-eitem.
*      WAIT UP TO 3 SECONDS .
*    ENDIF.
    IF WA_MATNR = IT_ZTPPERM-EITEM.
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
               WHERE MATNR EQ IT_ZTPPERM-EITEM .
    IF SY-SUBRC EQ 0.
*----> Convert Actual Date
** Changed by Furong Wang on 06/19/08
*      IF  IT_ZTPPERM-PROD_DT IS INITIAL.
*        WA_FLAG  = 'E'.
*        WA_MSGID = 'SF'  .
*        WA_MSGTY = 'E'   .
*        WA_MSGNO = '999' .
*        WA_MSGV1 = TEXT-202.  "Posting date is invalid
*      ELSE.
*        WA_DATUM = IT_ZTPPERM-PROD_DT.
**----> PROCESS REPORT POINT
*           CLEAR: WA_ERPID .
*        PERFORM PROCESS_REPORT_POINT  .
*      ENDIF.
** End of change
** Changed on 07/17/08 by Furong: production date

*      CALL FUNCTION 'Z_FPP_CHANGE_DATE'
*           EXPORTING
*                IWERKS                     = 'E001'
*                IDATE                      = IT_ZTPPERM-RDATE
*                ITIME                      = IT_ZTPPERM-RTIME
*           IMPORTING
*                ODATE                      = WA_DATUM
*           EXCEPTIONS
*                FACTORY_CALENDAR_NOT_FOUND = 1
*                HOLIDAY_CALENDAR_NOT_FOUND = 2
*                DATE_HAS_INVALID_FORMAT    = 3
*                DATE_INCONSISTENCY         = 4
*                ERROR_TIME                 = 5
*                OTHERS                     = 6.
*
*      IF SY-SUBRC NE 0.
*        WA_FLAG  = 'E'.
*        WA_MSGID = 'SF'  .
*        WA_MSGTY = 'E'   .
*        WA_MSGNO = '999' .
*        WA_MSGV1 = TEXT-202.  "Posting date is invalid
*      ELSE.
      WA_DATUM = IT_ZTPPERM-PROD_DT.
*----> PROCESS Report Point
      CLEAR: WA_ERPID .
      PERFORM PROCESS_REPORT_POINT  .
*      ENDIF.

** End of cahnge on 07/14/08
    ELSE.
      WA_FLAG  = 'E'.
      WA_MSGID = 'SF'.
      WA_MSGTY = 'E'.
      WA_MSGNO = '999'.
      WA_MSGV1 = TEXT-201.   "Material Code is Invalid!!
    ENDIF.

    IF WA_FLAG EQ SPACE .
      IF IT_ZTPPERM-ERPID = 'E51'.

      ELSE.
       IF RETURN-TYPE EQ SPACE . "       AND RETURN-MESSAGE+0(1) EQ 'S'.
          PERFORM IT_ZTPPERM_MODIFY USING L_MSG
                                         'S'
                                         L_TABIX.
        ELSE.
          PERFORM IT_ZTPPERM_MODIFY USING RETURN-MESSAGE
                                         RETURN-TYPE
                                         L_TABIX.
        ENDIF.
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
        PERFORM IT_ZTPPERM_MODIFY USING L_MSG
                                       'S'
                                       L_TABIX.
      ELSE.
        PERFORM IT_ZTPPERM_MODIFY USING L_MSG
                                       WA_MSGTY
                                       L_TABIX.
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING IT_ZTPPERM TO WA_ZTPPERM.
    IF IT_ZTPPERM-ZRESULT EQ 'E' .
      MOVE-CORRESPONDING IT_ZTPPERM TO IT_ERROR .
      APPEND IT_ERROR .
    ENDIF.

    UPDATE ZTPPERM FROM WA_ZTPPERM.
    IF SY-SUBRC EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                WAIT = 'X'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    ENDIF.
    CLEAR: WA_EQUNR   ,
           WA_ZTPPERM ,
           WA_FLAG    ,
           WA_MSGID   ,
           WA_MSGTY   ,
           WA_MSGNO   ,
           WA_MSGV1   ,
           WA_MSGV2   ,
           WA_MSGV3   ,
           WA_MSGV4   ,
           IT_ERROR   ,
** Changed by Furong on 06/04/08
           L_MSG.
** End of change
** Added by Furong on 01/09/08 for database update delay
    WA_MATNR = IT_ZTPPERM-EITEM.
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
FORM IT_ZTPPERM_MODIFY USING P_MSG
                            P_MSGTY
                            P_TABIX.

  CASE P_MSGTY.
    WHEN 'S'.
      IT_ZTPPERM-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
      IT_ZTPPERM-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ZTPPERM-ZBNAM = SY-UNAME.  "BDC User ID
*     C: Create U: Update D: Delete
      IT_ZTPPERM-ZMODE    = 'C'.       "C:CREATE
      IT_ZTPPERM-ZRESULT  = 'S'.       "SUCCESS
      IT_ZTPPERM-ZMSG     = P_MSG.
      IT_ZTPPERM-ZRESULT  = P_MSGTY.

    WHEN 'I'.
      IT_ZTPPERM-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
      IT_ZTPPERM-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ZTPPERM-ZBNAM = SY-UNAME.  "BDC User ID
*     C: Create U: Update D: Delete
      IT_ZTPPERM-ZMODE   = 'C'.       "C:CREATE
      IT_ZTPPERM-ZRESULT = 'S'.       "P_MSGTY.
      IT_ZTPPERM-ZMSG    = 'Classification Update successfully!!'.

    WHEN 'E'.
      IT_ZTPPERM-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
      IT_ZTPPERM-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ZTPPERM-ZBNAM = SY-UNAME.  "BDC User ID
      IT_ZTPPERM-ZMODE = 'C'.       "C:CREATE
*      IT_ZTPPERM-FLAG  = 'E'.       "ERROR
      IT_ZTPPERM-ZMSG    = P_MSG.
      IT_ZTPPERM-ZRESULT = 'E' .

    WHEN OTHERS.
      IT_ZTPPERM-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
      IT_ZTPPERM-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ZTPPERM-ZBNAM = SY-UNAME.  "BDC User ID
      IT_ZTPPERM-ZMODE = 'C'.       "C:CREATE
      IT_ZTPPERM-ZMSG    = P_MSG.
** Changed by Furong on 06/04/08
*      IT_ZTPPERM-ZRESULT = 'S'.
      IT_ZTPPERM-ZRESULT = P_MSGTY.
** End of change
  ENDCASE.

  MODIFY IT_ZTPPERM INDEX P_TABIX TRANSPORTING ZBDAT
                                              ZBTIM
                                              ZBNAM
                                              ZMODE
                                              ZRESULT
                                              ZMSG.

ENDFORM.                    " IT_ZTPPERM_MODIFY
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E00
*&---------------------------------------------------------------------*
FORM EN_CID_E00.

*  check assy ID.
  SELECT SINGLE * FROM EQUI
         WHERE EQUNR EQ WA_EQUNR .  "IT_ZTPPERM-EASSYID.
  IF SY-SUBRC NE 0.
    CLEAR ZTPPER_MAPPING .
    SELECT SINGLE * FROM ZTPPER_MAPPING.

    EXTERNAL_NUMBER         = IT_ZTPPERM-EASSYID.      "Equipment Master
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
    DATA_SPECIFIC-MATERIAL   = IT_ZTPPERM-EITEM.       "ITEM
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
                WAIT = 'X'.
*----> Assign class to Engine Master
      PERFORM ASSIGN_CLASS USING IT_ZTPPERM-EASSYID.
    ENDIF.
  ELSE.
** changed by Furong on 12/16/09
    PERFORM UPDATE_CLASSIFICATION_E00 .
** end of change
    WA_FLAG = 'X'.
    WA_MSGID = 'SF'.
** changed by Furong on 12/16/09
*    WA_MSGTY = 'E'.
    WA_MSGTY = 'S'.
** end of change
    WA_MSGNO = '999'.
    WA_MSGV1 = TEXT-203.
  ENDIF.

ENDFORM.                    " EN_CID_E00
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E04
*&---------------------------------------------------------------------*
FORM EN_CID_E04.

  EXTERNAL_NUMBER           = IT_ZTPPERM-EASSYID.    "Equipment number
* General Tab
*  DATA_GENERAL-START_FROM   = IT_ZTPPERM-RDATE.      "Start-up date
*  DATA_GENERALX-START_FROM  = 'X'.
*  DATA_GENERAL-CONSTYEAR    = IT_ZTPPERM-RDATE+0(4). "Year construction
*  DATA_GENERALX-CONSTYEAR   = 'X'.
*  DATA_GENERAL-CONSTMONTH   = IT_ZTPPERM-RDATE+4(2). "Month constr.
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
              WAIT = 'X'.
    PERFORM UPDATE_CLASSIFICATION_E04.
  ENDIF.

ENDFORM.                    " EN_CID_E04
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E05
*&---------------------------------------------------------------------*
FORM EN_CID_E05.

** Changed by Furong on 06/12/08
  IF NOT IT_ZTPPERM-EASSYID IS INITIAL.
    SELECT SINGLE *
      FROM ZTPPERM
       WHERE ERPID = IT_ZTPPERM-ERPID
         AND EASSYID = IT_ZTPPERM-EASSYID
         AND ZRESULT = 'S'.
    IF SY-SUBRC = 0.
      PERFORM UPDATE_CLASSIFICATION_E06 USING 'KD' .
      IF WA_FLAG IS INITIAL.
        WA_FLAG  = 'X'.
        WA_MSGID = 'SF'  .
        WA_MSGTY = 'S'   .
        WA_MSGNO = '999' .
        WA_MSGV1 = TEXT-801.  "Duplicated record
      ENDIF.
      EXIT.
    ENDIF.
  ENDIF.
** End of change

*  check assy ID.
  SELECT SINGLE * FROM EQUI
         WHERE EQUNR EQ WA_EQUNR .  "IT_ZTPPERM-EASSYID.
  IF SY-SUBRC EQ 0 .
*    PERFORM UPDATE_CLASSIFICATION_E06.
    PERFORM EN_CID_E01 .
  ELSE.
    CLEAR ZTPPER_MAPPING .
    SELECT SINGLE * FROM ZTPPER_MAPPING.

    EXTERNAL_NUMBER         = IT_ZTPPERM-EASSYID.      "Equipment Master
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
    DATA_SPECIFIC-MATERIAL   = IT_ZTPPERM-EITEM.       "ITEM
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
                WAIT = 'X'.
*----> Assign class to Engine Master
      PERFORM ASSIGN_CLASS USING IT_ZTPPERM-EASSYID.

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

  IF IT_ZTPPERM-EUSAGE NE SPACE.   "A/S Stock Transfer
    CLEAR : ZTPP_MIP_STK_TRA.
    SELECT SINGLE *
           FROM ZTPP_MIP_STK_TRA
           WHERE WERKS  EQ 'E001'
             AND EUSAGE EQ IT_ZTPPERM-EUSAGE.

    IF SY-SUBRC EQ 0.
      GOODSMVT_HEADER-PSTNG_DATE = WA_DATUM.
      GOODSMVT_HEADER-DOC_DATE   = IT_ZTPPERM-RDATE.
      GOODSMVT_HEADER-HEADER_TXT = IT_ZTPPERM-RTIME.
*  GM_Code 01: Goods receipt for purchase order
*  GM_Code 02: Goods receipt for production order
*  GM_Code 03: Goods issue
*  GM_Code 04: Transfer posting
*  GM_Code 05: Other goods receipts
*  GM_Code 06: Reversal of goods movements
      GOODSMVT_CODE = '04'.
      IT_GOODSMVT_ITEM-MATERIAL  = IT_ZTPPERM-EITEM.   "MATERIAL
      IT_GOODSMVT_ITEM-PLANT     = ZTPP_MIP_STK_TRA-WERKS. "Engine PLANT
      CLEAR : MARC.
      SELECT SINGLE * FROM MARC
             WHERE MATNR EQ IT_ZTPPERM-EITEM          "ENGINE MATERIAL
               AND WERKS EQ ZTPP_MIP_STK_TRA-WERKS.  "ENGINE PLANT
      IT_GOODSMVT_ITEM-STGE_LOC   = MARC-LGPRO.      "FROM
      IT_GOODSMVT_ITEM-MOVE_TYPE  = '311'. "MVT type
      IT_GOODSMVT_ITEM-ENTRY_QNT  = IT_ZTPPERM-EQTY.
      IT_GOODSMVT_ITEM-ENTRY_UOM  = IT_ZTPPERM-MEINS.

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

  EXTERNAL_NUMBER          = IT_ZTPPERM-EASSYID.    "Equipment number
  DATA_GENERAL-CONSTYEAR   = WA_DATUM+0(4) .   "IT_ZTPPERM-RDATE+0(4).
  DATA_GENERALX-CONSTYEAR  = 'X'.
  DATA_GENERAL-CONSTMONTH  = WA_DATUM+4(2) .   "IT_ZTPPERM-RDATE+4(2).
  DATA_GENERALX-CONSTMONTH = 'X'.
  DATA_GENERAL-SALES_ORG  = ZTPPER_MAPPING-VKORG.
  DATA_GENERALX-SALES_ORG  = 'X' .
  DATA_GENERAL-DISTR_CHAN = ZTPPER_MAPPING-VTWEG.
  DATA_GENERALX-DISTR_CHAN = 'X' .
  DATA_GENERAL-DIVISION   = ZTPPER_MAPPING-SPART.
  DATA_GENERALX-DIVISION   = 'X' .

  IF IT_ZTPPERM-EUSAGE EQ '01'.
    DATA_SPECIFIC-SERIALNO  = IT_ZTPPERM-EASSYID.
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
              WAIT = 'X'.
    PERFORM UPDATE_CLASSIFICATION_E07 .
  ENDIF.
ENDFORM.                    " EN_CID_E07

*&---------------------------------------------------------------------*
*&      Form  EN_CID_T04
*&---------------------------------------------------------------------*
FORM EN_CID_T04.
*  EXTERNAL_NUMBER          = IT_ZTPPERM-EASSYID.    "Equipment number
*    DATA_GENERAL-SALES_ORG  = ZTPPER_MAPPING-VKORG.
*    DATA_GENERALX-SALES_ORG  = 'X' .
*    DATA_GENERAL-DISTR_CHAN = ZTPPER_MAPPING-VTWEG.
*    DATA_GENERALX-DISTR_CHAN = 'X' .
*    DATA_GENERAL-DIVISION   = ZTPPER_MAPPING-SPART.
*    DATA_GENERALX-DIVISION   = 'X' .

**  IF IT_ZTPPERM-EUSAGE EQ '01'.
**    DATA_SPECIFIC-SERIALNO  = IT_ZTPPERM-EASSYID.
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
         WHERE MATNR EQ IT_ZTPPERM-EITEM.
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
*  SELECT SINGLE *
*    FROM MARA
*   WHERE MATNR EQ IT_ZTPPERM-EITEM.
*
*  IF SY-SUBRC EQ 0.
*    PERFORM UPDATE_CLASSIFICATION_T04 .
*  ELSE.
*    WA_FLAG = 'X'.
*    WA_MSGID = 'SF'.
*    WA_MSGTY = 'E'.
*    WA_MSGNO = '999'.
*    WA_MSGV1 = 'Material Code is Invalid!!'.
*  ENDIF.

  DATA: GOODSMVT_HEADER   LIKE  BAPI2017_GM_HEAD_01,
         GOODSMVT_CODE     LIKE  BAPI2017_GM_CODE,
         IT_GOODSMVT_ITEM
         LIKE TABLE OF BAPI2017_GM_ITEM_CREATE WITH HEADER LINE,
         IT_RETURN  LIKE TABLE OF BAPIRET2 WITH HEADER LINE.
  DATA: L_MBLNR    TYPE  BAPI2017_GM_HEAD_RET-MAT_DOC,
        L_MJAHR    TYPE  BAPI2017_GM_HEAD_RET-DOC_YEAR.
  DATA: L_CNT LIKE IT_VMASTER-ATWRT.
*  IF IT_ZTPPERM-EUSAGE NE SPACE.   "A/S Stock Transfer
*  CLEAR : ZTPP_MIP_STK_TRA.
*  SELECT SINGLE MATNR INTO W_OITEM
*      FROM EQUI
*      WHERE EQUNR = IT_ZTPPERM-EASSYID.

  CLEAR: IT_VMASTER,  IT_VMASTER[].

  WA_EQUNR = IT_ZTPPERM-EASSYID.
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = WA_EQUNR
            MODE         = 'R'
            DISPLAY      = 'X'
       TABLES
            VAL_TABLE    = IT_VMASTER
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  READ TABLE IT_VMASTER WITH KEY ATNAM = 'EN_ITEM_CODE'.
  IF SY-SUBRC = 0.
    W_OITEM = IT_VMASTER-ATWRT.
    READ TABLE IT_VMASTER WITH KEY ATNAM = 'EN_CHG_CNT'.
    IF SY-SUBRC = 0.
      L_CNT = IT_VMASTER-ATWRT.
    ENDIF.
    READ TABLE IT_VMASTER WITH KEY ATNAM = 'EN_CID'.
    IF SY-SUBRC = 0 AND
         ( IT_VMASTER-ATWRT = 'E05' OR IT_VMASTER-ATWRT = 'E09' ).
      IF IT_VMASTER-ATWRT = 'E05'.
        IF  IT_ZTPPERM-EITEM+0(2) = 'AU'.
          IT_GOODSMVT_ITEM-STGE_LOC   = 'E110'.      "FROM
          IT_GOODSMVT_ITEM-MOVE_STLOC = 'E110'.      "TO
        ELSE.
          IT_GOODSMVT_ITEM-STGE_LOC   = 'E210'.      "FROM
          IT_GOODSMVT_ITEM-MOVE_STLOC = 'E210'.      "TO
        ENDIF.
      ELSE.
        IT_GOODSMVT_ITEM-STGE_LOC   = 'E301'.      "FROM
        IT_GOODSMVT_ITEM-MOVE_STLOC = 'E301'.      "TO
      ENDIF.
      GOODSMVT_HEADER-PSTNG_DATE = IT_ZTPPERM-RDATE.
      GOODSMVT_HEADER-DOC_DATE   = IT_ZTPPERM-RDATE.
      GOODSMVT_HEADER-HEADER_TXT = IT_ZTPPERM-EASSYID.
      GOODSMVT_CODE = '04'.

      IT_GOODSMVT_ITEM-MATERIAL = W_OITEM.
      IT_GOODSMVT_ITEM-MOVE_MAT = IT_ZTPPERM-EITEM.
*      IT_GOODSMVT_ITEM-MATERIAL  = IT_ZTPPERM-EITEM.   "MATERIAL
*      IT_GOODSMVT_ITEM-MOVE_MAT = W_OITEM.
      IT_GOODSMVT_ITEM-PLANT     = 'E001'. "Engine PLANT
      IT_GOODSMVT_ITEM-MOVE_PLANT = 'E001'.
*      CLEAR : MARC.
*      SELECT SINGLE * FROM MARC
*             WHERE MATNR EQ IT_ZTPPERM-EITEM          "ENGINE MATERIAL
*               AND WERKS EQ 'E001'.  "ENGINE PLANT
*      IT_GOODSMVT_ITEM-STGE_LOC   = MARC-LGPRO.      "FROM
*      IT_GOODSMVT_ITEM-MOVE_STLOC = MARC-LGPRO.      "TO

      IT_GOODSMVT_ITEM-MOVE_TYPE  = '309'. "MVT type
      IT_GOODSMVT_ITEM-ENTRY_QNT  = IT_ZTPPERM-EQTY.
      IT_GOODSMVT_ITEM-ENTRY_UOM  = IT_ZTPPERM-MEINS.

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


      IF SY-SUBRC = 0 AND NOT L_MBLNR IS INITIAL.

        CLEAR : RETURN-MESSAGE, RETURN-TYPE .
        MOVE 'S'      TO   RETURN-TYPE .
        CONCATENATE 'TR:' L_MBLNR INTO RETURN-MESSAGE
                                 SEPARATED BY SPACE .

        L_CNT = L_CNT + 1.
        CLEAR : IT_VMASTER,  IT_VMASTER[] .
        PERFORM APPEND_VMASTER USING 'EN_OITEM_CODE'  W_OITEM.
        PERFORM APPEND_VMASTER USING 'EN_ITEM_CODE' IT_ZTPPERM-EITEM  .
        PERFORM APPEND_VMASTER USING 'EN_ODATE'   IT_ZTPPERM-RDATE  .
        PERFORM APPEND_VMASTER USING 'EN_OTIME'   IT_ZTPPERM-RTIME  .
        PERFORM APPEND_VMASTER USING 'EN_OSEQ'    IT_ZTPPERM-RSEQ .
        PERFORM APPEND_VMASTER USING 'EN_CHG_CNT' L_CNT .
        PERFORM APPEND_VMASTER USING 'EN_MDOC_SPECCHANGE'  L_MBLNR.

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
      ENDIF.
    ELSE.
      READ TABLE IT_RETURN INDEX 1.
      MOVE-CORRESPONDING IT_RETURN TO RETURN.
*  PERFORM UPDATE_CLASSIFICATION_E98A.
    ENDIF.
  ELSE.
    CLEAR : RETURN-MESSAGE, RETURN-TYPE .
    MOVE 'E'      TO   RETURN-TYPE .
    RETURN-MESSAGE = 'Invalid current RP for SPEC changes'.
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
            OBJECT       = WA_EQUNR
            MODE         = 'R'
            DISPLAY      = 'X'
       TABLES
            VAL_TABLE    = IT_VMASTER
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

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
        IT_VALS-ATNAM = 'EN_ORD_SEQ'.  IT_VALS-ATWRT = IT_ZTPPERM-RSEQ.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_ORD_DATE'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_ODATE'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_ORD_DATE'. IT_VALS-ATWRT = IT_ZTPPERM-RDATE.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_ORD_TIME'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_OTIME'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_ORD_TIME'. IT_VALS-ATWRT = IT_ZTPPERM-RTIME.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
    WHEN 'E04' .
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_IN_SEQ'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_OSEQ'.     IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_IN_SEQ'.   IT_VALS-ATWRT = IT_ZTPPERM-RSEQ.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_IN_DATE'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_ODATE'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_IN_DATE'.  IT_VALS-ATWRT = IT_ZTPPERM-RDATE.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_IN_TIME'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_OTIME'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_IN_TIME'.  IT_VALS-ATWRT = IT_ZTPPERM-RTIME.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
    WHEN 'E05' .
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_COMPL_SEQ'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_OSEQ'.     IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_COMPL_SEQ'.
        IT_VALS-ATWRT = IT_ZTPPERM-RSEQ.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_COMPL_DATE'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_ODATE'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_COMPL_DATE'.
        IT_VALS-ATWRT = IT_ZTPPERM-RDATE.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_COMPL_TIME'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_OTIME'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_COMPL_TIME'.
        IT_VALS-ATWRT = IT_ZTPPERM-RTIME.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
    WHEN 'E07' .
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_OUT_SEQ'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_OSEQ'.     IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_OUT_SEQ'. IT_VALS-ATWRT = IT_ZTPPERM-RSEQ.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_OUT_DATE'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_ODATE'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_OUT_DATE'. IT_VALS-ATWRT = IT_ZTPPERM-RDATE.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_OUT_TIME'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_OTIME'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_OUT_TIME'. IT_VALS-ATWRT = IT_ZTPPERM-RTIME.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
    WHEN 'T04' .
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_MOUNT_SEQ'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_OSEQ'.     IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_MOUNT_SEQ'.
        IT_VALS-ATWRT = IT_ZTPPERM-RSEQ.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_MOUNT_DATE'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_ODATE'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_MOUNT_DATE'.
        IT_VALS-ATWRT = IT_ZTPPERM-RDATE.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
      READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_MOUNT_TIME'.
      IF SY-SUBRC = 0.
        IT_VALS-ATNAM = 'EN_OTIME'.    IT_VALS-ATWRT = IT_VMASTER-ATWRT.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
        IT_VALS-ATNAM = 'EN_MOUNT_TIME'.
        IT_VALS-ATWRT = IT_ZTPPERM-RTIME.
        APPEND IT_VALS.                CLEAR: IT_VALS, IT_VMASTER.
      ENDIF.
  ENDCASE.

  READ TABLE IT_VMASTER WITH KEY ATNAM =  'EN_ITEM_CODE'.
  IF SY-SUBRC = 0.
    IT_VALS-ATNAM = 'EN_OITEM_CODE'.   IT_VALS-ATWRT = IT_VMASTER-ATWRT.
    APPEND IT_VALS.                    CLEAR: IT_VALS, IT_VMASTER.
    IT_VALS-ATNAM = 'EN_ITEM_CODE'.    IT_VALS-ATWRT = IT_ZTPPERM-EITEM.
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
            OBJECT       = WA_EQUNR
            MODE         = 'W'
       TABLES
            VAL_TABLE    = IT_VALS
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  IF SY-SUBRC <> 0.
    WA_FLAG  = 'X' .
  ENDIF.
ENDFORM.                    " EN_CID_E99

*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_E00
*&---------------------------------------------------------------------*
FORM UPDATE_CLASSIFICATION_E00.
  DATA: L_CNT LIKE ZSPP_VIN_VALUE-ATWRT.
  L_CNT = '0' .
  CLEAR : IT_VMASTER,  IT_VMASTER[].
** Changed by Furong on 12/01/09
  IF WA_OLD_CID < IT_ZTPPERM-ERPID.
    PERFORM APPEND_VMASTER USING 'EN_CID'        IT_ZTPPERM-ERPID  .
  ENDIF.
** End of change on 12/01/09
  PERFORM APPEND_VMASTER USING 'EN_ORD_DATE'   IT_ZTPPERM-RDATE  .
*  PERFORM APPEND_VMASTER USING 'EN_ORD_DATE'   WA_DATUM         .
  PERFORM APPEND_VMASTER USING 'EN_ORD_TIME'   IT_ZTPPERM-RTIME  .
  PERFORM APPEND_VMASTER USING 'EN_ORD_SEQ'    IT_ZTPPERM-RSEQ   .
  PERFORM APPEND_VMASTER USING 'EN_CHG_CNT'    L_CNT            .
  PERFORM APPEND_VMASTER USING 'EN_PLD_NO'     IT_ZTPPERM-EPONO  .
  PERFORM APPEND_VMASTER USING 'EN_ITEM_CODE'  IT_ZTPPERM-EITEM  .

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
** Changed by Furong on 12/01/09
  IF WA_OLD_CID < IT_ZTPPERM-ERPID.
    PERFORM APPEND_VMASTER USING 'EN_CID'        IT_ZTPPERM-ERPID  .
  ENDIF.
** End of change
  PERFORM APPEND_VMASTER USING 'EN_IN_DATE'    IT_ZTPPERM-RDATE  .
*  PERFORM APPEND_VMASTER USING 'EN_IN_DATE'    WA_DATUM         .
  PERFORM APPEND_VMASTER USING 'EN_IN_TIME'    IT_ZTPPERM-RTIME  .
  PERFORM APPEND_VMASTER USING 'EN_IN_SEQ'     IT_ZTPPERM-RSEQ   .

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
** Changed by Furong on 12/01/09
  IF WA_OLD_CID < IT_ZTPPERM-ERPID.
    PERFORM APPEND_VMASTER USING 'EN_CID'        IT_ZTPPERM-ERPID  .
  ENDIF.
** End of change
  PERFORM APPEND_VMASTER USING 'EN_ITEM_CODE'  IT_ZTPPERM-EITEM  .
  PERFORM APPEND_VMASTER USING 'EN_COMPL_DATE' IT_ZTPPERM-RDATE  .
*  PERFORM APPEND_VMASTER USING 'EN_COMPL_DATE' WA_DATUM         .
  PERFORM APPEND_VMASTER USING 'EN_COMPL_TIME' IT_ZTPPERM-RTIME  .
  PERFORM APPEND_VMASTER USING 'EN_COMPL_SEQ'  IT_ZTPPERM-RSEQ   .
  PERFORM MORE_NEW_CHAR.

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
** Changed by Furong on 12/01/09
  IF WA_OLD_CID < IT_ZTPPERM-ERPID.
    PERFORM APPEND_VMASTER USING 'EN_CID'        IT_ZTPPERM-ERPID  .
  ENDIF.
** End of change
  PERFORM APPEND_VMASTER USING 'EN_OUT_DATE'   IT_ZTPPERM-RDATE  .
*  PERFORM APPEND_VMASTER USING 'EN_OUT_DATE'   WA_DATUM         .
  PERFORM APPEND_VMASTER USING 'EN_OUT_TIME'   IT_ZTPPERM-RTIME  .
  PERFORM APPEND_VMASTER USING 'EN_OUT_SEQ'    IT_ZTPPERM-RSEQ   .

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
** Changed by Furong on 12/01/09
  IF WA_OLD_CID < IT_ZTPPERM-ERPID.
    PERFORM APPEND_VMASTER USING 'EN_CID'          IT_ZTPPERM-ERPID  .
  ENDIF.
** End of change
  PERFORM APPEND_VMASTER USING 'EN_COMPL_DATE'   IT_ZTPPERM-RDATE  .
  PERFORM APPEND_VMASTER USING 'EN_COMPL_TIME'   IT_ZTPPERM-RTIME  .
* PERFORM APPEND_VMASTER USING 'EN_MOUNT_SEQ'    IT_ZTPPERM-RSEQ   .

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = WA_EQUNR
            MODE         = 'W'
            CMODE        = '002'
       TABLES
            VAL_TABLE    = IT_VMASTER
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

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
** Changed by Furong on 12/01/09
  IF WA_OLD_CID < IT_ZTPPERM-ERPID.
    PERFORM APPEND_VMASTER USING 'EN_CID'          IT_ZTPPERM-ERPID  .
  ENDIF.
** End of change
  PERFORM APPEND_VMASTER USING 'EN_COMPL_DATE'   IT_ZTPPERM-RDATE  .
  PERFORM APPEND_VMASTER USING 'EN_COMPL_TIME'   IT_ZTPPERM-RTIME  .
  PERFORM APPEND_VMASTER USING 'EN_KKK_SEQ'      IT_ZTPPERM-RSEQ   .

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
    CASE IT_ZTPPERM-ERPID .
      WHEN 'E00' .
        PERFORM UPDATE_CLASSIFICATION_E00 .
      WHEN 'E05' .
        PERFORM UPDATE_CLASSIFICATION_E06 USING 'KD' .
      WHEN 'E06' .
        PERFORM UPDATE_CLASSIFICATION_E06_CKD USING 'KD' .
    ENDCASE .
  ENDIF.
ENDFORM.                    " ASSIGN_CLASS
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E01
*&---------------------------------------------------------------------*
FORM EN_CID_E01.

** Changed by Furong on 06/12/08
  IF NOT IT_ZTPPERM-EASSYID IS INITIAL.
    SELECT SINGLE *
      FROM ZTPPERM
       WHERE ERPID = IT_ZTPPERM-ERPID
         AND EASSYID = IT_ZTPPERM-EASSYID
         AND ZRESULT = 'S'.
    IF SY-SUBRC = 0.
      WA_FLAG  = 'X'.
      WA_MSGID = 'SF'  .
      WA_MSGTY = 'S'   .
      WA_MSGNO = '999' .
      WA_MSGV1 = TEXT-801.  "Duplicated record
      EXIT.
    ENDIF.
  ENDIF.
** End of change

*-----> CANCEL PROCESS
  IF NOT IT_ZTPPERM-PRTNR IS INITIAL.
    CALL FUNCTION 'BAPI_REPMANCONF1_CANCEL'
         EXPORTING
              CONFIRMATION     = IT_ZTPPERM-PRTNR
              POSTDATE         = WA_DATUM
         IMPORTING
              CANCCONFIRMATION = WA_CANCCO
              RETURN           = RETURN.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              WAIT = 'X'.
    IT_ZTPPERM-CANC_PRTNR = WA_CANCCO.
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

  BFLUSHDATAGEN-MATERIALNR    = IT_ZTPPERM-EITEM .   "Material number
  BFLUSHDATAGEN-PRODPLANT     = 'E001'          .   "Plant
  BFLUSHDATAGEN-PRODVERSION   = '01'            .   "Production version
*  BFLUSHDATAGEN-POSTDATE      = IT_ZTPPERM-RDATE .   "Posting date
  BFLUSHDATAGEN-POSTDATE      = WA_DATUM .          "Posting date
  BFLUSHDATAGEN-DOCDATE       = SY-DATUM        .   "Document date

** Changed by Furong on 06/13/08, requested by MY Hur
*    BFLUSHDATAGEN-DOCHEADERTXT  = IT_ZTPPERM-RTIME .   "Doc header text
  BFLUSHDATAGEN-DOCHEADERTXT  = IT_ZTPPERM-EASSYID.  "Engine serial no
** end of change

  BFLUSHDATAGEN-UNITOFMEASURE = IT_ZTPPERM-MEINS .   "UoM

  IF IT_ZTPPERM-EQTY > 0.
    BFLUSHDATAGEN-BACKFLQUANT   = IT_ZTPPERM-EQTY.   "Yield Qty
  ENDIF.

  IF IT_ZTPPERM-SQTY > 0.
    BFLUSHDATAGEN-SCRAPQUANT    = IT_ZTPPERM-SQTY.   "Scrap qty
  ENDIF.

  IF IT_ZTPPERM-RQTY > 0.
    BFLUSHDATAGEN-BACKFLQUANT   = IT_ZTPPERM-RQTY .  "Rework Qty
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
    IT_ZTPPERM-PRTNR = WA_CONFIRMATION . "PA_CONFIRM.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              WAIT = 'X'.
*-----> Classification Update
    IF IT_ZTPPERM-ERPID EQ 'E05' .
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
  WA_EQUNR = IT_ZTPPERM-EASSYID.

** Changed by Furong on 12/01/09
  CLEAR : IT_VMASTER,  IT_VMASTER[], WA_OLD_CID.
  PERFORM APPEND_VMASTER USING 'EN_CID'   ' '.
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT     = WA_EQUNR
            MODE       = 'R'
            CTYPE      = '002'
       TABLES
            VAL_TABLE  = IT_VMASTER
       EXCEPTIONS
            NO_DATA    = 1
            ERROR_MODE = 2
            OTHERS     = 3.
  READ TABLE IT_VMASTER INDEX 1.
  WA_OLD_CID = IT_VMASTER-ATWRT.

  CASE IT_ZTPPERM-ERPID.
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
** Added by Furong Wang on 04/15/09
    WHEN 'E06'.    "Storage In
      PERFORM EN_CID_E06_CKD.
** End of addition
    WHEN 'E07'.    "Storage Out
      PERFORM EN_CID_E07.

** Changed by Furong Wang on 08/18/09
** Added by Furong Wang on 07/22/09
*    WHEN 'E09'.
*      PERFORM EN_CID_E09.
*    WHEN 'E10'.
*      PERFORM EN_CID_E10.
** end of addtion by Furong Wang
** end of addtion by Furong Wang on 08/18/09
    WHEN 'T04'.    "
*        PERFORM EN_CID_T04.
** Changed by Furong Wang on 08/18/09
*    WHEN 'E98'.    "
*      PERFORM EN_CID_E98.
** end of addtion by Furong Wang on 08/18/09
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
         L_ZTPPERM_IX     TYPE    SY-TABIX ,
         L_ERROR_IX      TYPE    SY-TABIX ,
         L_SUCCESS_IX    TYPE    SY-TABIX .

  DESCRIBE TABLE IT_ZTPPERM  LINES   L_ZTPPERM_IX .
  DESCRIBE TABLE IT_ERROR   LINES   L_ERROR_IX  .
  L_SUCCESS_IX   =  L_ZTPPERM_IX  -  L_ERROR_IX  .
  WRITE:/ TEXT-311 , L_ZTPPERM_IX  .
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
      PERFORM READ_PROCESS.    "Read from ZTPPERM Where condition
      PERFORM DATA_PROCESS.    "Data processing for Report Point
      PERFORM WRITE_RESULT.    "Write Result for Batch job
      PERFORM WRITE_CLOSE.     "Write Botton for Batch job
    WHEN R3.
      PERFORM READ_PROCESS.    "Read from ZTPPERM Where condition
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
        I_STRUCTURE_NAME              = 'ZTPPERM'
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
        IT_OUTTAB                     = IT_ZTPPERM[]
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

  L_STRUCT = 'ZTPPERM'.
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
                AND MATNR EQ IT_ZTPPERM-EITEM
                AND WERKS EQ 'E001'
                AND VERID EQ '01'      .
  IF SY-SUBRC NE 0.
** Added by Furong on 07/09/08
    CLEAR : RETURN-MESSAGE, RETURN-TYPE .
    MOVE 'E'      TO   RETURN-TYPE .
    MOVE 'NO MAT DOC CREATED' TO RETURN-MESSAGE.
** end of addition
  ELSE.
    SELECT SINGLE BELNR
                  INTO L_BELNR
                  FROM BLPP
                  WHERE PRTNR  EQ  BLPK-PRTNR
                    AND PRTPS  EQ  '0001' .
    IF SY-SUBRC NE 0.
** Added by Furong on 07/09/08
      CLEAR : RETURN-MESSAGE, RETURN-TYPE .
      MOVE 'E'      TO   RETURN-TYPE .
      MOVE 'NO MAT DOC CREATED' TO RETURN-MESSAGE.
** end of addition
    ELSE.
      CLEAR : RETURN-MESSAGE, RETURN-TYPE .
      MOVE 'S'      TO   RETURN-TYPE .
      CONCATENATE 'GR:' L_BELNR INTO RETURN-MESSAGE
                               SEPARATED BY SPACE .
** Changed by Furong on 07/08/08
      IF IT_ZTPPERM-ERPID EQ 'E05'.
        CLEAR: IT_VMASTER,
               IT_VMASTER[].
        PERFORM APPEND_VMASTER USING 'EN_GR_DOCU_NO'  L_BELNR .

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
      ENDIF.
** End of change
    ENDIF.
  ENDIF.

ENDFORM.                    " SELECT_MAT_DOC
*&---------------------------------------------------------------------*
*&      Form  COPY_TO_ZTPPERM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COPY_TO_ZTPPERM.
  DATA: L_RDATE LIKE ZTPPER2-RDATE,
        L_TSEQ LIKE ZTPPER2-TSEQ,
        L_INDEX LIKE SY-TABIX.
  DATA: LT_ZTPPER_O LIKE TABLE OF ZTPPER WITH HEADER LINE,
        LT_ZTPPER2 LIKE TABLE OF ZTPPER2 WITH HEADER LINE,
        LT_ZTPPERM LIKE TABLE OF ZTPPERM WITH HEADER LINE.

** copy ztpper table to ztpperm

*  SELECT MAX( RDATE ) MAX( TSEQ ) INTO (L_RDATE, L_TSEQ )
*    FROM ZTPPERM
*   WHERE STABLE = 'R'.
*  IF L_RDATE IS INITIAL.
*    SELECT * INTO TABLE LT_ZTPPER_O
*      FROM ZTPPER
*     WHERE RDATE = SY-DATUM.
*  ELSE.
*    SELECT * INTO TABLE LT_ZTPPER_O
*       FROM ZTPPER
*      WHERE ( TSEQ > L_TSEQ AND RDATE = L_RDATE )
*         OR ( RDATE > L_RDATE ).
*  ENDIF.
  SELECT * INTO TABLE LT_ZTPPER_O
         FROM ZTPPER
        WHERE ZRESULT = 'I'.
  IF SY-SUBRC = 0.
    LOOP AT LT_ZTPPER_O.
      MOVE-CORRESPONDING LT_ZTPPER_O TO LT_ZTPPERM.
      LT_ZTPPERM-STABLE = 'R'.

** Changed by Furong Wang on 06/19/08

** Changed by Furong on 07/16/08

*      CALL FUNCTION 'Z_FPP_CHANGE_DATE'
*           EXPORTING
*                IWERKS                     = 'E001'
*                IDATE                      = LT_ZTPPERM-RDATE
*                ITIME                      = LT_ZTPPERM-RTIME
*           IMPORTING
*                ODATE                      = LT_ZTPPERM-PROD_DT
*           EXCEPTIONS
*                FACTORY_CALENDAR_NOT_FOUND = 1
*                HOLIDAY_CALENDAR_NOT_FOUND = 2
*                DATE_HAS_INVALID_FORMAT    = 3
*                DATE_INCONSISTENCY         = 4
*                ERROR_TIME                 = 5
*                OTHERS                     = 6.

** End of change

      IF LT_ZTPPERM-RTIME GE '051500' AND LT_ZTPPERM-RTIME LE '240000' .
        LT_ZTPPERM-PROD_DT  =  LT_ZTPPERM-RDATE.
      ELSEIF LT_ZTPPERM-RTIME GE '000000' AND
             LT_ZTPPERM-RTIME LT '051500'.
        LT_ZTPPERM-PROD_DT  =  LT_ZTPPERM-RDATE - 1.
      ENDIF.

** End of change on 07/16/08


      APPEND LT_ZTPPERM.
      CLEAR: LT_ZTPPER_O,LT_ZTPPERM.
    ENDLOOP.
  ENDIF.

  INSERT ZTPPERM FROM TABLE LT_ZTPPERM.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
    LOOP AT LT_ZTPPER_O.
      L_INDEX = SY-TABIX.
      LT_ZTPPER_O-ZRESULT = 'P'.
      MODIFY LT_ZTPPER_O INDEX L_INDEX TRANSPORTING ZRESULT.
    ENDLOOP.

    MODIFY ZTPPER FROM TABLE LT_ZTPPER_O.

    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
  REFRESH LT_ZTPPER_O.
  REFRESH LT_ZTPPERM.

** copy ztpper2 table to ztpperm
  CLEAR: L_RDATE, L_TSEQ.
*  SELECT MAX( RDATE ) MAX( TSEQ ) INTO (L_RDATE, L_TSEQ )
*   FROM ZTPPERM
*  WHERE STABLE = 'M'.
*  IF L_RDATE IS INITIAL.
*    SELECT * INTO TABLE LT_ZTPPER2
*      FROM ZTPPER2
*     WHERE RDATE = SY-DATUM.
*  ELSE.
*    SELECT * INTO TABLE LT_ZTPPER2
*       FROM ZTPPER2
*      WHERE ( TSEQ > L_TSEQ AND RDATE = L_RDATE )
*         OR ( RDATE > L_RDATE ).
*  ENDIF.
  SELECT * INTO TABLE LT_ZTPPER2
        FROM ZTPPER2
         WHERE ZRESULT = 'I'.
** Changed by Furong on 11/30/09
  LOOP AT LT_ZTPPER2.
    IF LT_ZTPPER2-ERPID = 'E09' OR
       LT_ZTPPER2-ERPID = 'E10' OR
       LT_ZTPPER2-ERPID = 'E98'.
      DELETE LT_ZTPPER2.
    ENDIF.
  ENDLOOP.
** End of change on 11/30/09

  IF SY-SUBRC = 0.
    LOOP AT LT_ZTPPER2.
      MOVE-CORRESPONDING LT_ZTPPER2 TO LT_ZTPPERM.
      LT_ZTPPERM-STABLE = 'M'.
      APPEND LT_ZTPPERM.
      CLEAR: LT_ZTPPER2,LT_ZTPPERM.
    ENDLOOP.

    INSERT ZTPPERM FROM TABLE LT_ZTPPERM.
    IF SY-SUBRC EQ 0.
      COMMIT WORK.
      LOOP AT LT_ZTPPER2.
        L_INDEX = SY-TABIX.
        LT_ZTPPER2-ZRESULT = 'P'.
        MODIFY LT_ZTPPER2 INDEX L_INDEX TRANSPORTING ZRESULT.
      ENDLOOP.
      MODIFY ZTPPER2 FROM TABLE LT_ZTPPER2.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.
  REFRESH LT_ZTPPER2.
ENDFORM.                    " COPY_TO_ZTPPERM

*---------------------------------------------------------------------*
*       FORM more_new_char                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM MORE_NEW_CHAR.

** Changed by Furong on 12/17/09
* PERFORM APPEND_VMASTER USING 'EN_HEAD_LH_SERIAL' IT_ZTPPERM-HEAD_SRL_N
*.
* PERFORM APPEND_VMASTER USING 'EN_HEAD_RH_SERIAL'
*                                IT_ZTPPERM-HEAD_SRL_N_RH.
*
* PERFORM APPEND_VMASTER USING 'EN_BLOCK_SERIAL' IT_ZTPPERM-BLOCK_SRL_NO
*.
* PERFORM APPEND_VMASTER USING 'EN_CRANK_SERIAL' IT_ZTPPERM-CRANK_SRL_NO
*.
  IF IT_ZTPPERM-HEAD_SRL_N IS INITIAL.
  ELSE.
    PERFORM APPEND_VMASTER USING 'EN_HEAD_LH_SERIAL'
                                  IT_ZTPPERM-HEAD_SRL_N.
  ENDIF.
  IF IT_ZTPPERM-HEAD_SRL_N_RH IS INITIAL.
  ELSE.
    PERFORM APPEND_VMASTER USING 'EN_HEAD_RH_SERIAL'
                                  IT_ZTPPERM-HEAD_SRL_N_RH.
  ENDIF.
  IF IT_ZTPPERM-BLOCK_SRL_NO IS INITIAL.
  ELSE.
    PERFORM APPEND_VMASTER USING 'EN_BLOCK_SERIAL'
                                  IT_ZTPPERM-BLOCK_SRL_NO.
  ENDIF.
  IF IT_ZTPPERM-CRANK_SRL_NO IS INITIAL.
  ELSE.
    PERFORM APPEND_VMASTER USING 'EN_CRANK_SERIAL'
                                  IT_ZTPPERM-CRANK_SRL_NO.
  ENDIF.
** End of change on 12/17/09
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E06_ckd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EN_CID_E06_CKD.

*  check assy ID.
  SELECT SINGLE * FROM EQUI
         WHERE EQUNR EQ WA_EQUNR .  "IT_ZTPPERM-EASSYID.
  IF SY-SUBRC EQ 0 .
    PERFORM UPDATE_CLASSIFICATION_E06_CKD  USING 'KD'.
  ELSE.
    CLEAR ZTPPER_MAPPING .
    SELECT SINGLE * FROM ZTPPER_MAPPING.

    EXTERNAL_NUMBER         = IT_ZTPPERM-EASSYID.      "Equipment Master
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
    DATA_SPECIFIC-MATERIAL   = IT_ZTPPERM-EITEM.       "ITEM
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
                WAIT = 'X'.
*----> Assign class to Engine Master
      PERFORM ASSIGN_CLASS USING IT_ZTPPERM-EASSYID.

    ENDIF.
  ENDIF.

ENDFORM.                    " EN_CID_E06_ckd
*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_E06_ckd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_CLASSIFICATION_E06_CKD  USING P_CID.
  CLEAR: IT_VMASTER,
       IT_VMASTER[].
** Changed by Furong on 12/01/09
  IF WA_OLD_CID < IT_ZTPPERM-ERPID.
    PERFORM APPEND_VMASTER USING 'EN_CID'        IT_ZTPPERM-ERPID  .
  ENDIF.
** End of change
  PERFORM APPEND_VMASTER USING 'EN_ITEM_CODE'  IT_ZTPPERM-EITEM  .
  PERFORM APPEND_VMASTER USING 'EN_ASRS_IN_DATE' IT_ZTPPERM-RDATE  .
*  PERFORM APPEND_VMASTER USING 'EN_COMPL_DATE' WA_DATUM         .
  PERFORM APPEND_VMASTER USING 'EN_ASRS_IN_TIME' IT_ZTPPERM-RTIME  .
  PERFORM APPEND_VMASTER USING 'EN_ASRS_IN_SEQ'  IT_ZTPPERM-RSEQ   .

  PERFORM MORE_NEW_CHAR.

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
*    IF P_CID NE 'KD' .
*      PERFORM SELECT_MAT_DOC .
*    ENDIF.
  ENDIF.

ENDFORM.                    " UPDATE_CLASSIFICATION_E06_ckd
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E09
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EN_CID_E09.
  PERFORM UPDATE_CLASSIFICATION_E09 .
  PERFORM POST_MAT_MB1B.
ENDFORM.                    " EN_CID_E09
*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_E09
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_CLASSIFICATION_E09.
  DATA: L_CNT LIKE ZSPP_VIN_VALUE-ATWRT.

  CLEAR : IT_VMASTER,  IT_VMASTER[] .
*  PERFORM APPEND_VMASTER USING 'EN_ITEM_CODE'  IT_ZTPPERM-EITEM  .
** Changed by Furong on 12/01/09
  IF WA_OLD_CID < IT_ZTPPERM-ERPID.
    PERFORM APPEND_VMASTER USING 'EN_CID'        IT_ZTPPERM-ERPID  .
  ENDIF.
** End of change
  PERFORM APPEND_VMASTER USING 'EN_RACK_DATE'   IT_ZTPPERM-RDATE  .
  PERFORM APPEND_VMASTER USING 'EN_RACK_TIME'   IT_ZTPPERM-RTIME  .
  PERFORM APPEND_VMASTER USING 'EN_RACK_SEQ'    IT_ZTPPERM-RSEQ   .
  PERFORM APPEND_VMASTER USING 'EN_RACK_NO'     IT_ZTPPERM-EN_RACK_NO.
  PERFORM APPEND_VMASTER USING 'EN_RACK_WORKER'   IT_ZTPPERM-EN_WORKER.

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

ENDFORM.                    " UPDATE_CLASSIFICATION_E09
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E10
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EN_CID_E10.
  PERFORM UPDATE_CLASSIFICATION_E10.
ENDFORM.                    " EN_CID_E10
*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_E10
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_CLASSIFICATION_E10.

  CLEAR : IT_VMASTER,  IT_VMASTER[] .
*  PERFORM APPEND_VMASTER USING 'EN_ITEM_CODE'  IT_ZTPPERM-EITEM  .
** Changed by Furong on 12/01/09
  IF WA_OLD_CID < IT_ZTPPERM-ERPID.
    PERFORM APPEND_VMASTER USING 'EN_CID'        IT_ZTPPERM-ERPID  .
  ENDIF.
** End of change
  PERFORM APPEND_VMASTER USING 'EN_TRUCK_DATE'   IT_ZTPPERM-RDATE  .
  PERFORM APPEND_VMASTER USING 'EN_TRUCK_TIME'   IT_ZTPPERM-RTIME  .
  PERFORM APPEND_VMASTER USING 'EN_TRUCK_SEQ'    IT_ZTPPERM-RSEQ   .
  PERFORM APPEND_VMASTER USING 'EN_RACK_NO'     IT_ZTPPERM-EN_RACK_NO.
  PERFORM APPEND_VMASTER USING 'EN_TRUCK_NO'  IT_ZTPPERM-EN_TRUCK_NO.
  PERFORM APPEND_VMASTER USING 'EN_WORK_FLAG' IT_ZTPPERM-EN_WORK_FLAG.
  PERFORM APPEND_VMASTER USING 'EN_TRUCK_WORKER'   IT_ZTPPERM-EN_WORKER.

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

ENDFORM.                    " UPDATE_CLASSIFICATION_E10
*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_E98A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_CLASSIFICATION_E98A.

  CLEAR : IT_VMASTER,  IT_VMASTER[] .
  PERFORM APPEND_VMASTER USING 'EN_OITEM_CODE'  W_OITEM.

*  PERFORM APPEND_VMASTER USING 'EN_GR_DOCU_NO'  L_BELNR .
*  IT_ZTPPERM-EITEM  .
*  PERFORM APPEND_VMASTER USING 'EN_CID'        IT_ZTPPERM-ERPID  .
*  PERFORM APPEND_VMASTER USING 'EN_TRUCK_DATE'   IT_ZTPPERM-RDATE  .
*  PERFORM APPEND_VMASTER USING 'EN_TRUCK_TIME'   IT_ZTPPERM-RTIME  .
**  PERFORM APPEND_VMASTER USING 'EN_TRUCK_SEQ'    IT_ZTPPERM-RSEQ   .
*  PERFORM APPEND_VMASTER USING 'EN_RACK_NO'     IT_ZTPPERM-EN_RACK_NO.
*  PERFORM APPEND_VMASTER USING 'EN_TRUCK_NO'  IT_ZTPPERM-EN_TRUCK_NO.
*  PERFORM APPEND_VMASTER USING 'EN_WORK_FLAG' IT_ZTPPERM-EN_WORK_FLAG.
*  PERFORM APPEND_VMASTER USING 'EN_TRUCK_WORKER'  IT_ZTPPERM-EN_WORKER.


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

ENDFORM.                    " UPDATE_CLASSIFICATION_E98A
*&---------------------------------------------------------------------*
*&      Form  post_mat_mb1b
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POST_MAT_MB1B.
  DATA: GOODSMVT_HEADER   LIKE  BAPI2017_GM_HEAD_01,
       GOODSMVT_CODE     LIKE  BAPI2017_GM_CODE,
       IT_GOODSMVT_ITEM
       LIKE TABLE OF BAPI2017_GM_ITEM_CREATE WITH HEADER LINE,
       IT_RETURN  LIKE TABLE OF BAPIRET2 WITH HEADER LINE.
  DATA: L_MBLNR    TYPE  BAPI2017_GM_HEAD_RET-MAT_DOC,
        L_MJAHR    TYPE  BAPI2017_GM_HEAD_RET-DOC_YEAR.

  SELECT SINGLE *
      FROM ZTPPERM
       WHERE ERPID = IT_ZTPPERM-ERPID
         AND EASSYID = IT_ZTPPERM-EASSYID
         AND ZRESULT = 'S'
         AND ZMSG <> ' '.

  IF SY-SUBRC = 0.
** this is duplicate data
    EXIT.
  ENDIF.

*  IF IT_ZTPPERM-EUSAGE NE SPACE.
*    CLEAR : ZTPP_MIP_STK_TRA.
*    SELECT SINGLE *
*           FROM ZTPP_MIP_STK_TRA
*           WHERE WERKS  EQ 'E001'
*             AND EUSAGE EQ IT_ZTPPERM-EUSAGE.
*
*    IF SY-SUBRC EQ 0.
  GOODSMVT_HEADER-PSTNG_DATE = WA_DATUM.
  GOODSMVT_HEADER-DOC_DATE   = IT_ZTPPERM-RDATE.
  GOODSMVT_HEADER-HEADER_TXT = IT_ZTPPERM-EASSYID.
*  GM_Code 01: Goods receipt for purchase order
*  GM_Code 02: Goods receipt for production order
*  GM_Code 03: Goods issue
*  GM_Code 04: Transfer posting
*  GM_Code 05: Other goods receipts
*  GM_Code 06: Reversal of goods movements
  GOODSMVT_CODE = '04'.
  IT_GOODSMVT_ITEM-MATERIAL  = IT_ZTPPERM-EITEM.   "MATERIAL
  IT_GOODSMVT_ITEM-PLANT     = 'E001' . "Engine PLANT
  CLEAR : MARC.
  SELECT SINGLE * FROM MARC
         WHERE MATNR EQ IT_ZTPPERM-EITEM          "ENGINE MATERIAL
           AND WERKS EQ ZTPP_MIP_STK_TRA-WERKS.  "ENGINE PLANT
  IF IT_ZTPPERM-EITEM+0(2) = 'AU'.
    IT_GOODSMVT_ITEM-STGE_LOC   = 'E110'.      "FROM
  ELSE.
    IT_GOODSMVT_ITEM-STGE_LOC   = 'E210'.      "FROM
  ENDIF.
  IT_GOODSMVT_ITEM-MOVE_TYPE  = '311' . "MVT type
  IT_GOODSMVT_ITEM-ENTRY_QNT  = IT_ZTPPERM-EQTY.
  IT_GOODSMVT_ITEM-ENTRY_UOM  = IT_ZTPPERM-MEINS.

  IT_GOODSMVT_ITEM-MOVE_STLOC = 'E301'.  "TO Stock
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

  IF L_MBLNR IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
** Update engine master
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

    CLEAR: IT_VMASTER,
           IT_VMASTER[].
    PERFORM APPEND_VMASTER USING 'EN_MDOC_STORAGE'  L_MBLNR.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT     = IT_ZTPPERM-EASSYID
              MODE       = 'W'
              CMODE      = '002'
         TABLES
              VAL_TABLE  = IT_VMASTER
         EXCEPTIONS
              NO_DATA    = 1
              ERROR_MODE = 2
              OTHERS     = 3.

    CLEAR : RETURN-MESSAGE, RETURN-TYPE .
    MOVE 'S'      TO   RETURN-TYPE .
    CONCATENATE 'TR:' L_MBLNR INTO RETURN-MESSAGE
                             SEPARATED BY SPACE .
  ENDIF.
ENDFORM.                    " post_mat_mb1b
*&---------------------------------------------------------------------*
*&      Form  check_batch_job
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_BATCH_JOB.
** Added by Furong Wang on 03/12/10
  DATA: LT_STATUS LIKE TABLE OF TBTCO WITH HEADER LINE,
        L_CN TYPE I,
*        L_JOBNAME LIKE TBTCO-JOBNAME default ',
        L_FLAG TYPE CHAR1.

  DATA: LT_CHECK_LOG LIKE TABLE OF ZTPP_PMT07JB_B WITH HEADER LINE.

  SELECT * INTO TABLE LT_STATUS
    FROM TBTCO
    WHERE JOBNAME LIKE 'ZPP_ENG_SBF%'
      AND STATUS = 'R'.
*      AND ( STATUS = 'P'
*         OR STATUS = 'S'
*         OR STATUS = 'Y'
*         OR STATUS = 'R' ).
  DESCRIBE TABLE LT_STATUS LINES L_CN.
  IF L_CN > 1.
    W_BATCH_JOB = 'X'.
    MESSAGE E001 WITH 'Please check Job log for same job'.
  ELSE.
    CLEAR: W_BATCH_JOB.
  ENDIF.
** End of addition

ENDFORM.                    " check_batch_job
