************************************************************************
* Program Name      : ZIPP102I_SEQ_MAIN
* Author            : Bobby
* Creation Date     : 2004.02.08.
* Specifications By : Bobby
* Pattern           : 5.2.2
* Development Request No :
* Addl Documentation:
* Description       : Vehicle Order(Planned Order) Creation
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 10/11/05   Furong       UD1K917863   continue to run if error occurs
*                         UD1K917912   in characteristic creation;
*                                      create issue log ZTPP_SEQ_LOG
*                                      which will be reprocessed by
*                                      other program
************************************************************************
REPORT  ZIPP102I_SEQ_MAIN   MESSAGE-ID ZMPP  .

*----------------------------------------------------------------------*
* TABLES DECLARATION
*----------------------------------------------------------------------*
TABLES: ZTPP_SEQ_BACKUP ,    " Sequence Table's Information for the Reco
        ZTPP_COMMON_VALS,    " PP: Common Values
        EQUI ,               " Vehicle Master(Header)
        AUSP .               " Vehicle Master(Detail-Characteristic)

*----------------------------------------------------------------------*
* INTERNAL TABLES DECLARATION
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_VIN         OCCURS 0                        .
        INCLUDE STRUCTURE     ZTPP_PMT07JB_B .
DATA:   MATNR                 LIKE MARA-MATNR,
      END OF IT_VIN                          ,
      IT_7JB              LIKE TABLE OF IT_VIN         WITH HEADER LINE,
      IT_MSG              LIKE TABLE OF BDCMSGCOLL     WITH HEADER LINE,
      IT_VMASTER          LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.

*----------------------------------------------------------------------*
* WORKING-AREA VARIABLES DECLARATION
*----------------------------------------------------------------------*
DATA: WA_MATERIAL             LIKE MARA-MATNR                 ,
      WA_7JB                  LIKE IT_VIN                     ,
      WA_7JB_LOG              LIKE IT_VIN                     ,
      WA_MAXDAY               LIKE SY-DATUM                   ,
      WA_MINDAY               LIKE SY-DATUM                   ,
      WA_LINES                TYPE I                          ,
      WA_MSG(70)              TYPE C                          ,
      WA_MNG                  TYPE I                          ,
      WA_SND_JOBS             TYPE I                          ,
      WA_RCV_JOBS             TYPE I                          ,
      WA_TASKNAME(4)          TYPE N VALUE '0001'             ,
      WA_EXCP_FLAG            TYPE C                          ,
      WA_ERROR                TYPE C                          ,
      WA_FLAG                 TYPE C                          ,
      WA_DATE                 TYPE D                          ,
      WA_ERR_HD               TYPE C                          ,
      WA_SUBRC                LIKE SY-SUBRC                   ,
      C_PROG                  LIKE SY-REPID                   .

*----------------------------------------------------------------------*
* Field-Symbols VARIABLES DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS   P_RUN                            DEFAULT 'X'.
SELECTION-SCREEN COMMENT  (55) TEXT-102 FOR FIELD P_RUN  .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------
TOP-OF-PAGE.
*----------------------------------------------------------------------
  IF WA_ERR_HD = 'E'     .
    WRITE AT: /001(50)  TEXT-001.
    SKIP 1 .
    ULINE AT: /(50)             .
    WRITE AT: /001(09)  TEXT-201,     " l_ordr ,
               011(05)  TEXT-202,     " l_dist ,
               017(03)  TEXT-203,     " l_extc ,
               021(03)  TEXT-204.     " l_intc .
    ULINE AT: /(50)             .
  ENDIF.

*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------
  GET TIME.
  WA_DATE = SY-DATUM.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------

** Added by Furong Wang on 05/12/09
  DATA: LT_STATUS LIKE TABLE OF TBTCO WITH HEADER LINE,
        L_CN TYPE I,
        L_FLAG TYPE CHAR1.
  DATA: LT_CHECK_LOG LIKE TABLE OF ZTPP_PMT07JB_B WITH HEADER LINE.

  SELECT * INTO TABLE LT_STATUS
    FROM TBTCO
    WHERE JOBNAME = 'ZPP_SEQ'
      AND STATUS = 'R'.
*      AND ( STATUS = 'P'
*         OR STATUS = 'S'
*         OR STATUS = 'Y'
*         OR STATUS = 'R' ).
  DESCRIBE TABLE LT_STATUS LINES L_CN.
  IF L_CN > 1.
    MESSAGE I001 WITH 'Please check Job log for same job'.
    EXIT.
  ENDIF.
** End of addition
  DELETE FROM ZTPP_REP_SEQ CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
  DELETE FROM ZTPP_SEQ_LOG CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            WAIT = 'X'.

  IF P_RUN = 'X'.
    " Normal Processing...
    IF SY-BATCH = ' '.
** for testing only
*      PERFORM exit_message  .
*      EXIT.
    ENDIF.
** changed by Furong on 02/23/10
   CALL FUNCTION 'Z_FPP_SEQ_MAIN_PRE_CHECK'
         IMPORTING
              O_FLAG = L_FLAG.

    CHECK L_FLAG = 'S'.
** End of change

    PERFORM GET_DATA        .
    CHECK WA_FLAG IS INITIAL.
    PERFORM CHECK_DATA .
    CHECK WA_FLAG IS INITIAL.
    PERFORM BACK_UP_DATA1   .         " BACK-UP ROUTINE FOR THE RECOVERY
*workorder color update
    PERFORM BDC_WOCL_SUMMARY .        " Step 1: Work Order Color
    IF WA_ERROR = 'X'        .
      MESSAGE I001 WITH TEXT-905 '#7 Rollback STEP 1-1 W.Color'.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .

      " WA_7JB_LOG is Null Values..   ( For the Structure Parameter..)
      PERFORM CREATE_LOG USING '1' WA_7JB_LOG. " Log Create in STEP 1 .
      EXIT .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                WAIT = 'X'.
    ENDIF.
*workorder header update
    PERFORM BDC_WOHD_SUMMARY .
    IF WA_ERROR = 'X'        .
      MESSAGE I001 WITH TEXT-905 '#10 Rollback STEP 1-2'.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      " WA_7JB_LOG is Null Values..   ( For the Structure Parameter..)
      PERFORM CREATE_LOG USING '2' WA_7JB_LOG. " Log Create in STEP 2 .
      EXIT .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                WAIT = 'X'.
    ENDIF.
*vehicle
    PERFORM CLEAR_VINN       .
    PERFORM RECORD_PROCESSING.           " Log Create in Step 3 & 4 .
  ELSE.
    " Reprocessing..
  ENDIF.

  CHECK WA_ERROR = SPACE   .
  PERFORM CHECK_RESULT     .
  PERFORM WRITE_RESULT     .
  PERFORM WRITE_TIMESTAMP  USING  TEXT-011 .

END-OF-SELECTION.
  PERFORM CHECK_PCC_ROUTINE.
  PERFORM CALL_PLAN_SAVE   .


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  DATA: COM_DATE             TYPE D,
        CHK_DATE             TYPE D.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_7JB
    FROM ZTPP_PMT07JB_B .

  DESCRIBE TABLE IT_7JB LINES WA_LINES .
  READ TABLE IT_7JB INDEX 1.
  CHK_DATE = IT_7JB-SQDT   .

  " Data Check for Re-Running..
  C_PROG = 'ZIPP101U_PMT07JB_A'.
  SELECT SINGLE *
    FROM ZTPP_COMMON_VALS
   WHERE JOBS  = C_PROG
     AND KEY2  = IT_7JB-MODL .

  COM_DATE = ZTPP_COMMON_VALS-ITEM1.   " Previous Max Sequenced Date..
  IF CHK_DATE <= COM_DATE .
    WA_FLAG  =  'X'      .            " Already Sequenced Data..
    MESSAGE I001 WITH TEXT-012 .
  ENDIF.
ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  RECORD_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RECORD_PROCESSING.
  " Check the process about the followings:
  " 1. Sequence date is diffrent. and same record is exist each days.
  " 2. early day's record is sequencing. and late day's record is MITU
  "    the record's updating time is delay. data will be a unmatch!!
  DATA: L_LEN TYPE I,
        L_NEW_DEALER(1).

  SORT IT_7JB BY SQDT SSR1.
  LOOP AT IT_7JB WHERE MTGU NE 'M'.
    PERFORM CHECK_VIN         .
    PERFORM VIN_CODE_GEN      .
    IF WA_ERROR = 'X'.
      MESSAGE I001 WITH TEXT-905 '#14 Error VIN_CODE_GEN'.
      EXIT.
    ENDIF.
    MODIFY IT_7JB.
  ENDLOOP.

  IF WA_ERROR = 'X'.
    MESSAGE I001 WITH TEXT-905 '#15 Error Rollback in STEP 3-1'.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    " VIN Code Generation Error...
    PERFORM CREATE_LOG USING '3' IT_7JB.   " Log Create in STEP 3    .
    EXIT .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              WAIT = 'X'.
  ENDIF.

  CLEAR: WA_RCV_JOBS, WA_SND_JOBS.
  LOOP AT IT_7JB .
** Changed by Furong on 10/09/07 for EBOM
    L_LEN = STRLEN( IT_7JB-BMDL ).
    IF L_LEN = 7.
     CONCATENATE IT_7JB-MOYE  IT_7JB-DIST IT_7JB-BMDL INTO IT_7JB-MATNR.
     CONCATENATE IT_7JB-MATNR IT_7JB-OCNN             INTO IT_7JB-MATNR
             SEPARATED BY SPACE.
    ELSE.
      CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
           EXPORTING
                OLD_DEALER = IT_7JB-DIST+3(2)
           IMPORTING
                NEW_DEALER = L_NEW_DEALER.
      CONCATENATE IT_7JB-MOYE IT_7JB-DIST+0(3) L_NEW_DEALER IT_7JB-BMDL
                    INTO IT_7JB-MATNR.
      CONCATENATE IT_7JB-MATNR IT_7JB-OCNN INTO IT_7JB-MATNR.
    ENDIF.
** End of change
    WA_7JB = IT_7JB .
    " Error Log Step 4 : Call the Job-Processor.....
    CASE IT_7JB-MTGU .
      WHEN 'M'      .
        PERFORM PROCESS_MITU_VALUE .
      WHEN OTHERS.
        PERFORM SELECT_VIN        .
        PERFORM JOB_CREATE  USING  '1'           .
    ENDCASE.
    MODIFY IT_7JB.
  ENDLOOP.

  WAIT UNTIL WA_RCV_JOBS >= WA_SND_JOBS.


  CLEAR: WA_RCV_JOBS, WA_SND_JOBS, WA_TASKNAME.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            WAIT = 'X'.

  IF SY-SUBRC = 0  AND  WA_EXCP_FLAG = SPACE.
  ELSE.
*    MESSAGE i001 WITH text-905 '#16 Error Rollback  Vehicle Master'.
    MESSAGE W001 WITH TEXT-300 .
** added by furong on 11/16/05
    WRITE: 'SY:', SY-SUBRC,SY-MSGID,SY-MSGTY,SY-MSGV1,SY-MSGV2,SY-MSGV3.
    WRITE: / 'wa_excp_flag =', WA_EXCP_FLAG.
    WRITE: / 'Received jobs =', WA_RCV_JOBS.
    WRITE: / 'Sent jobs =', WA_SND_JOBS.
** end of change

*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .


    WRITE AT: /001(010) 'RT-Code: '   ,
               012(003) SY-SUBRC      ,
               016(013) 'Send Count: ',
               030(003) WA_SND_JOBS   ,
               035(013) 'Recv Count: ',
               069(003) WA_RCV_JOBS   .
    ULINE.                                " TO-BE DELETE LINE.

    PERFORM CREATE_LOG USING '6' IT_7JB.   " Log Create in STEP 4    .
* changed by furong on 11/15/05
*   EXIT.
* end of change
  ENDIF.

  CLEAR: WA_RCV_JOBS, WA_SND_JOBS.
  LOOP AT IT_7JB WHERE MTGU NE 'M' .
    " Error Log Step 5 : Call the Job-Processor.....
    PERFORM SELECT_VIN        .
    PERFORM JOB_CREATE USING '2'         .
  ENDLOOP.

  WAIT UNTIL WA_RCV_JOBS >= WA_SND_JOBS.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            WAIT = 'X'.
  IF SY-SUBRC = 0  AND  WA_EXCP_FLAG = SPACE.

  ELSE.
*    MESSAGE i001 WITH text-905 '#17 Error Rollback Planned Order'.
** andded by furong on 11/16/2005
    MESSAGE W001 WITH TEXT-400 .

    WRITE: 'sy:', SY-SUBRC,SY-MSGID,SY-MSGTY,SY-MSGV1,SY-MSGV2,SY-MSGV3.
    WRITE: / 'wa_excp_flag =', WA_EXCP_FLAG.
    WRITE: / 'Received jobs =', WA_RCV_JOBS.
    WRITE: / 'Sent jobs =', WA_SND_JOBS.
** end of change

*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    WRITE AT: /001(010) 'RT-Code: '   ,
               012(003) SY-SUBRC      ,
               016(013) 'Send Count: ',
               030(003) WA_SND_JOBS   ,
               035(013) 'Recv Count: ',
               069(003) WA_RCV_JOBS   .
    ULINE.                                " TO-BE DELETE LINE.
*    MESSAGE w001 WITH text-400 .
    PERFORM CREATE_LOG USING '9' IT_7JB.   " Log Create in STEP 5    .
* changed by furong on 11/15/05
*   EXIT.
* end of change
  ENDIF.

  PERFORM UPDATE_COMMONVLAS    .
ENDFORM.                    " RECORD_PROCESSING

*&---------------------------------------------------------------------*
*&      Form  process_mitu_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_MITU_VALUE.
  DATA: L_EQUNR              LIKE EQUI-EQUNR ,
        L_ATINN              LIKE AUSP-ATINN ,
        L_PORDER             LIKE PLAF-PLNUM ,
        L_VIN                LIKE MARA-MATNR ,
        L_OBJEK              LIKE AUSP-OBJEK ,
        L_AUSP               LIKE AUSP       ,
        L_MITUQTY            LIKE ZTPP_WOSUM-MITUQTY,
        L_DATA               LIKE TABLE OF ZSPP_VIN_VALUE
                                         WITH HEADER LINE,
        L_PLNMG              LIKE ZTPP_WOSUM-SEQQTY .


  CLEAR: WA_MATERIAL.
  CONCATENATE IT_7JB-ORDR IT_7JB-DIST INTO WA_MATERIAL ."workorder h
  CONCATENATE IT_7JB-MODL IT_7JB-VHNO INTO L_OBJEK     ."vehicle num

  " Check the Vehicle for the MITU...
  SELECT SINGLE ATINN INTO L_ATINN
    FROM CABN
   WHERE ATNAM = 'P_MITU'      .

  SELECT SINGLE *  INTO L_AUSP
    FROM AUSP
   WHERE OBJEK = L_OBJEK
     AND KLART = '002'
     AND ATINN = L_ATINN .
*requested by hur,changed by wskim,on 2004.11.05
*When Mitu vehicle is entered in BODY-IN Point without resequencing
*-----start
*  CHECK sy-subrc = 0 AND l_ausp-atwrt = 'Y' .
  IF SY-SUBRC = 0 AND L_AUSP-ATWRT = 'Y' .
*-----end
    " Work Order Summary Table Update.....
    SELECT SINGLE MITUQTY INTO L_MITUQTY
      FROM ZTPP_WOSUM
     WHERE WO_SER = IT_7JB-ORDR
       AND NATION = IT_7JB-DIST(3)
       AND DEALER = IT_7JB-DIST+3(2)
       AND EXTC   = IT_7JB-EXTC
       AND INTC   = IT_7JB-INTC     .

    L_MITUQTY = L_MITUQTY - IT_7JB-PQTY  .
    UPDATE ZTPP_WOSUM   SET MITUQTY = L_MITUQTY
                      WHERE WO_SER = IT_7JB-ORDR
                        AND NATION = IT_7JB-DIST(3)
                        AND DEALER = IT_7JB-DIST+3(2)
                        AND EXTC   = IT_7JB-EXTC
                        AND INTC   = IT_7JB-INTC     .

    " Vehicle Master Update..
* By Daniel on 02/10/11 {
*    CONCATENATE  IT_7JB-BMDL(3)  IT_7JB-VHNO  INTO  L_EQUNR .
    CONCATENATE  IT_7JB-MODL  IT_7JB-VHNO  INTO  L_EQUNR.
*}
    CLEAR: IT_VMASTER, IT_VMASTER[] .
    IT_VMASTER-ATNAM = 'P_SEQUENCE_DATE' .
    IT_VMASTER-ATWRT = IT_7JB-SQDT       .
    APPEND IT_VMASTER.
*add sequence serial & vm_date
*requested by hur,changed by wskim,on 2004.11.05
*-----start
    IT_VMASTER-ATNAM = 'P_SEQUENCE_SERIAL' .
    IT_VMASTER-ATWRT = IT_7JB-SSR1 .
    APPEND IT_VMASTER.
    DATA :LT_ATWRT(10) TYPE N.
    CLEAR LT_ATWRT.

* By Daniel on 02/10/11 {
*    IT_VMASTER-ATNAM = 'P_VM_DATE' .
*    CONCATENATE SY-DATUM SY-UZEIT   INTO  LT_ATWRT .
*    IT_VMASTER-ATWRT =  LT_ATWRT.
*    APPEND IT_VMASTER.
* }
*-----end
    IT_VMASTER-ATNAM = 'P_MITU'          .
    IT_VMASTER-ATWRT = ' '               .
    APPEND IT_VMASTER.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT       = L_EQUNR
              MODE         = 'W'
         TABLES
              VAL_TABLE    = IT_VMASTER
         EXCEPTIONS
              NO_DATA      = 1
              ERROR_MODE   = 2
              ERROR_OBJECT = 3
              ERROR_VALUE  = 4
              OTHERS       = 5.

    " Plan Order Number Search and Saving...
    SELECT SINGLE ATINN INTO L_ATINN
      FROM CABN
     WHERE ATNAM = 'P_PLAN_ORDER'.

    SELECT SINGLE ATWRT INTO L_PORDER
      FROM AUSP
     WHERE OBJEK  = L_EQUNR
       AND ATINN  = L_ATINN .

    SELECT SINGLE ATINN INTO L_ATINN
      FROM CABN
     WHERE ATNAM = 'P_VIN'       .

    SELECT SINGLE ATWRT INTO L_VIN
      FROM AUSP
     WHERE OBJEK  = L_EQUNR
       AND ATINN  = L_ATINN .

    IF SY-SUBRC = 0.
      UPDATE ZTPP_PMT07JB_B    SET: PLNUM = L_PORDER
                                    VINN  = L_VIN
                                    AEDAT = SY-DATUM
                                    AEZET = SY-UZEIT
                                    AENAM = SY-UNAME
                             WHERE SQDT  = IT_7JB-SQDT
                               AND PLNT  = IT_7JB-PLNT
                               AND LINE  = IT_7JB-LINE
                               AND MODL  = IT_7JB-MODL
                               AND MTGU  = IT_7JB-MTGU
                               AND SSR1  = IT_7JB-SSR1
                               AND SSR2  = IT_7JB-SSR2  .

    ENDIF.
*requested by hur,changed by wskim,on 2004.11.05
*When Mitu vehicle is entered in BODY-IN Point
*-----start
*update planned order
*change  order_start_date & order_fin_date & plan_open_date for MRP
    PERFORM CHANGE_PLANNEDORDER USING IT_7JB L_PORDER.
*Check : WORKORDER, EXT_COLOR,INT_COLOR
    PERFORM CHECK_VEHICLE_INFO USING IT_7JB L_EQUNR.
  ELSE.
*Workorder color mitu quty should be restorated
    CLEAR: WA_MATERIAL,L_DATA,L_DATA[].
    CONCATENATE IT_7JB-ORDR IT_7JB-DIST   INTO WA_MATERIAL .
    CONCATENATE WA_MATERIAL IT_7JB-EXTC  IT_7JB-INTC INTO WA_MATERIAL.

    CLEAR: L_PLNMG, L_DATA-ATWRT, AUSP.
    L_DATA-ATNAM = 'P_MITU_QTY'.    APPEND L_DATA.
*     l_data-atnam = 'P_SEQ_QTY'.     APPEND l_data.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT       = WA_MATERIAL
              CTYPE        = '001'
         TABLES
              VAL_TABLE    = L_DATA
         EXCEPTIONS
              NO_DATA      = 1
              ERROR_MODE   = 2
              ERROR_OBJECT = 3
              ERROR_VALUE  = 4
              OTHERS       = 5.

    " MITU Qauntity is add  : Work color
    READ TABLE L_DATA INDEX 1.
    L_PLNMG = L_DATA-ATWRT   .
    L_PLNMG =  L_PLNMG + 1.
    PERFORM WORKORDER_MITUQUTY_RESTORATION USING WA_MATERIAL
                                                 L_PLNMG.
*Workorder header mitu quty should be restorated
    CLEAR: WA_MATERIAL,L_DATA,L_DATA[].
    CONCATENATE IT_7JB-ORDR IT_7JB-DIST   INTO WA_MATERIAL .

    CLEAR: L_PLNMG, L_DATA-ATWRT, AUSP.
    L_DATA-ATNAM = 'P_MITU_QTY'.    APPEND L_DATA.
*     l_data-atnam = 'P_SEQ_QTY'.     APPEND l_data.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT       = WA_MATERIAL
              CTYPE        = '001'
         TABLES
              VAL_TABLE    = L_DATA
         EXCEPTIONS
              NO_DATA      = 1
              ERROR_MODE   = 2
              ERROR_OBJECT = 3
              ERROR_VALUE  = 4
              OTHERS       = 5.

    " MITU Qauntity is add  : Workorder header
    READ TABLE L_DATA INDEX 1.
    L_PLNMG = L_DATA-ATWRT   .
    L_PLNMG =  L_PLNMG + 1.
    PERFORM WORKORDER_MITUQUTY_RESTORATION USING WA_MATERIAL
                                                 L_PLNMG.
  ENDIF.
*-----end
ENDFORM.                    " process_mitu_value

*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_WORKORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*      -->P_L_PLNMG  text
*----------------------------------------------------------------------*
FORM CALL_BDC_WORKORDER USING    PA_MATERIAL  PA_PLNMG  PA_PQTY
                                 PA_FQTY      PA_MITU .
  DATA: L_VARS                LIKE TABLE OF ZSPP_VIN_VALUE
                                                  WITH HEADER LINE,
        L_DEC                 TYPE I,
        L_DECP                TYPE P DECIMALS 0,
        L_DEC1(6)             TYPE C,
        L_DEC2(6)             TYPE C.

  L_DEC1 = PA_PLNMG.
  L_VARS-ATNAM = 'P_SEQ_QTY' .     L_VARS-ATWRT = L_DEC1. APPEND L_VARS.
  CLEAR: L_VARS              .     L_DEC1 = PA_PQTY     .
  L_VARS-ATNAM = 'P_PLAN_QTY'.     L_VARS-ATWRT = L_DEC1. APPEND L_VARS.
  CLEAR: L_VARS              .     L_DEC1 = PA_FQTY     .
  L_VARS-ATNAM = 'P_FORECAST_QTY'. L_VARS-ATWRT = L_DEC1. APPEND L_VARS.
  CLEAR: L_VARS              .     L_DEC1 = PA_MITU     .
  L_VARS-ATNAM = 'P_MITU_QTY' .    L_VARS-ATWRT = L_DEC1. APPEND L_VARS.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = PA_MATERIAL
            MODE         = 'W'
            CTYPE        = '001'
       TABLES
            VAL_TABLE    = L_VARS
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  IF SY-SUBRC NE 0.
    WA_ERROR = 'X'.
  ENDIF.
ENDFORM.                    " CALL_BDC_WORKORDER

*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_SALES_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_BDC_SALES_ORDER   USING PA_SORDER PA_VAL2  PA_VAL10  PA_VAL20.
  DATA: L_VAL10(6)          TYPE N           ,
        L_VAL20(6)          TYPE N           ,
        L_FLAG              TYPE C           ,   " Roll-Back Flag..
        L_KWMENG            LIKE VBAP-KWMENG .

  " Change the Sales Order
  L_VAL10 =  PA_VAL10 .
  L_VAL20 =  PA_VAL20 .
  L_VAL20 = L_VAL20 - L_VAL10.

  " Change the Coding for the Re-Processing (BDC --> BAPI)
  PERFORM CALL_BAPI_SALESORDER USING PA_SORDER L_VAL10 L_VAL20 .
ENDFORM.                    " CALL_BDC_SALES_ORDER

*&---------------------------------------------------------------------*
*&      Form  BDC_WOHD_SUMMARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_WOHD_SUMMARY.
  " Sum the MITU Qty and SEQ Qty ...  (Each Others..)
  DATA: L_PLNMG(4)           TYPE N          ,
        L_ORDR               LIKE IT_7JB-ORDR,
        L_DIST               LIKE IT_7JB-DIST,
        L_MQTY               LIKE ZTPP_WOSUM-SEQQTY ,
        L_SEQQTY             LIKE ZTPP_WOSUM-SEQQTY ,
        L_TSEQ               LIKE ZTPP_WOSUM-SEQQTY ,
        L_SQTY               LIKE ZTPP_WOSUM-SEQQTY ,
        L_PQTY               LIKE ZTPP_WOSUM-SEQQTY ,
        L_FQTY               LIKE ZTPP_WOSUM-SEQQTY ,
        L_MITUQTY            LIKE ZTPP_WOSUM-MITUQTY,
        L_MITU               LIKE ZTPP_WOSUM-MITUQTY,
        L_DATA               LIKE TABLE OF ZSPP_VIN_VALUE
                                                       WITH HEADER LINE.

  SORT IT_7JB BY ORDR DIST EXTC INTC SSR1.
  CLEAR: L_SQTY  , L_MITUQTY.
  READ TABLE IT_7JB INDEX 1.
  L_ORDR = IT_7JB-ORDR.  L_DIST = IT_7JB-DIST.
  MOVE-CORRESPONDING IT_7JB  TO   IT_VIN     .

  LOOP AT IT_7JB .
    IF IT_7JB-ORDR = L_ORDR  AND  IT_7JB-DIST = L_DIST .
*      IF it_7jb-mtgu = space .
*        l_sqty      = l_sqty   + 1.
*      ELSE.
*        l_mituqty   = l_mituqty + 1 .
*      ENDIF.
*requested by hur,changed by wskim,on 2004.11.05
*logic add : Mitu quty
*-----Start
      IF IT_7JB-MTGU <> SPACE.
        L_MITUQTY   = L_MITUQTY + 1 .
      ENDIF.
*-----End
    ELSE.
      " Process.
      CLEAR: WA_MATERIAL,  L_DATA, L_DATA[], L_PLNMG, AUSP.
      CONCATENATE L_ORDR      L_DIST      INTO WA_MATERIAL .

      SELECT SUM( MODQTY )  SUM( SEQQTY ) SUM( MITUQTY )
        INTO (L_MQTY, L_TSEQ, L_MITU)
        FROM ZTPP_WOSUM
       WHERE WO_SER = L_ORDR
         AND NATION = L_DIST(3)
         AND DEALER = L_DIST+3(2) .

      L_SEQQTY  = L_SQTY    + L_TSEQ  .      " Total SEQ-Qty..

      " Work Order Header's SEQ  Qty Change......
      L_DATA-ATNAM = 'P_VIN_SPEC'.        APPEND L_DATA.
      L_DATA-ATNAM = 'P_MITU_QTY'.        APPEND L_DATA.
*      l_data-atnam = 'P_SEQ_QTY'.         APPEND l_data.
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                OBJECT       = WA_MATERIAL
                CTYPE        = '001'
           TABLES
                VAL_TABLE    = L_DATA
           EXCEPTIONS
                NO_DATA      = 1
                ERROR_MODE   = 2
                ERROR_OBJECT = 3
                ERROR_VALUE  = 4
                OTHERS       = 5.

*      READ TABLE l_data INDEX 3.
*      l_plnmg = l_data-atwrt   .         CLEAR: l_data.
*      l_plnmg = l_sqty .                  " L_plnmg + l_seqqty.
      READ TABLE L_DATA INDEX 1.
      IT_VIN-VINN = L_DATA-ATWRT.        CLEAR: L_DATA.
*     it_7JB-vinn = l_data-atwrt.        CLEAR: l_data.
      READ TABLE L_DATA INDEX 2.
      L_PLNMG = L_DATA-ATWRT   .         CLEAR: L_DATA.
      L_PLNMG  = L_PLNMG - L_MITUQTY.

      PERFORM PLAN_QUANTITY USING L_ORDR   L_DIST   ' '    ' '
                                  L_MQTY   L_SEQQTY L_PQTY L_FQTY 'H' .

      PERFORM CALL_BDC_WORKORDER USING WA_MATERIAL L_SEQQTY
                                       L_PQTY      L_FQTY    L_PLNMG .

      IF WA_ERROR = 'X'.
        MESSAGE I003 WITH TEXT-905 '#8 Workorder Header Update error'
                     WA_MATERIAL.
** Changed on 07/19/2006 Requested by Hur
        CLEAR: WA_ERROR.
*        EXIT.
** end of change
      ENDIF.
      APPEND IT_VIN .     CLEAR: IT_VIN.
      L_ORDR = IT_7JB-ORDR.  L_DIST = IT_7JB-DIST.
      IT_VIN = IT_7JB     .
*      IF it_7jb-mtgu = space .
*        l_sqty      = 1.
*      ELSE.
*        l_mituqty   = 1 .
*      ENDIF.
*requested by hur,changed by wskim,on 2004.11.05
*logic add : Mitu quty
*-----Start
      IF IT_7JB-MTGU <> SPACE.
        L_MITUQTY   = 1 .
      ENDIF.
*-----End

    ENDIF.
  ENDLOOP.

  IF WA_ERROR = 'X'.  EXIT.  ENDIF.

  CHECK WA_ERROR = SPACE.

  IF WA_LINES > 0 .
    CLEAR: WA_MATERIAL, L_DATA, L_DATA[], L_PLNMG, L_MQTY, L_TSEQ.
    CONCATENATE L_ORDR      L_DIST      INTO WA_MATERIAL .

    SELECT SUM( MODQTY )  SUM( SEQQTY ) SUM( MITUQTY )
      INTO (L_MQTY, L_TSEQ, L_MITU)
      FROM ZTPP_WOSUM
     WHERE WO_SER = L_ORDR
       AND NATION = L_DIST(3)
       AND DEALER = L_DIST+3(2) .

    L_SEQQTY  = L_SQTY    + L_TSEQ  .      " Total SEQ-Qty..

    " Work Order Header's SEQ  Qty Change......
    L_DATA-ATNAM = 'P_VIN_SPEC'.        APPEND L_DATA.
    L_DATA-ATNAM = 'P_MITU_QTY'.        APPEND L_DATA.
*      l_data-atnam = 'P_SEQ_QTY'.         APPEND l_data.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT       = WA_MATERIAL
              CTYPE        = '001'
         TABLES
              VAL_TABLE    = L_DATA
         EXCEPTIONS
              NO_DATA      = 1
              ERROR_MODE   = 2
              ERROR_OBJECT = 3
              ERROR_VALUE  = 4
              OTHERS       = 5.

*      READ TABLE l_data INDEX 3.
*      l_plnmg = l_data-atwrt   .         CLEAR: l_data.
*      l_plnmg = l_sqty .                  " L_plnmg + l_seqqty.
    READ TABLE L_DATA INDEX 1.
    IT_VIN-VINN = L_DATA-ATWRT.        CLEAR: L_DATA.
    READ TABLE L_DATA INDEX 2.
    L_PLNMG = L_DATA-ATWRT   .         CLEAR: L_DATA.
    L_PLNMG  = L_PLNMG - L_MITUQTY.

    PERFORM PLAN_QUANTITY USING L_ORDR   L_DIST   ' '    ' '
                                L_MQTY   L_SEQQTY L_PQTY L_FQTY 'H' .

    PERFORM CALL_BDC_WORKORDER USING WA_MATERIAL L_SEQQTY
                                     L_PQTY      L_FQTY    L_PLNMG .

    IF WA_ERROR = 'X'.
      MESSAGE I002 WITH TEXT-905 '#9 Workorder Header Update error'
WA_MATERIAL.
      EXIT.
    ENDIF.
    APPEND IT_VIN .
  ENDIF.
ENDFORM.                    " BDC_WOHD_SUMMARY

*&---------------------------------------------------------------------*
*&      Form  bdc_wocl_summary
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_WOCL_SUMMARY.
  DATA: L_PLNMG              LIKE ZTPP_WOSUM-SEQQTY ,
        L_MODQTY             LIKE ZTPP_WOSUM-MODQTY ,
        L_SALESORDER         LIKE ZTPP_WOSUM-SALES,
        L_ORDR               LIKE IT_7JB-ORDR,
        L_DIST               LIKE IT_7JB-DIST,
        L_EXTC               LIKE IT_7JB-EXTC,
        L_INTC               LIKE IT_7JB-INTC,
        L_WOSUM(20),
        L_SEQ                LIKE ZTPP_WOSUM-SEQQTY ,
        L_SEQQTY             LIKE ZTPP_WOSUM-SEQQTY ,
        L_TSEQ               LIKE ZTPP_WOSUM-SEQQTY ,
        L_PQTY               LIKE ZTPP_WOSUM-SEQQTY ,
        L_FQTY               LIKE ZTPP_WOSUM-SEQQTY ,
        L_MITUQTY            LIKE ZTPP_WOSUM-MITUQTY,
        L_DATA               LIKE TABLE OF ZSPP_VIN_VALUE
                                                       WITH HEADER LINE.

  DATA: L_COUNT              TYPE I.

  PERFORM WRITE_TIMESTAMP  USING  TEXT-010 .
  PERFORM WRITE_HEAD               .         " TO-BE DELETE LINE...

  SORT IT_7JB BY ORDR DIST EXTC INTC SSR1 SSR2.
  CLEAR: L_SEQQTY, L_MITUQTY, L_SEQ .
  READ TABLE IT_7JB INDEX 1.
  L_ORDR = IT_7JB-ORDR.  L_DIST = IT_7JB-DIST.
  L_EXTC = IT_7JB-EXTC.  L_INTC = IT_7JB-INTC.

  LOOP AT IT_7JB .
    IF IT_7JB-ORDR = L_ORDR  AND  IT_7JB-DIST = L_DIST AND
       IT_7JB-EXTC = L_EXTC  AND  IT_7JB-INTC = L_INTC .
      IF IT_7JB-MTGU = SPACE .
        L_SEQ       = L_SEQ    + 1.
      ELSE.
        L_MITUQTY   = L_MITUQTY + 1 .
      ENDIF.
    ELSE.
      " Work Order Summary Table Update.....
      SELECT SINGLE MODQTY SEQQTY SALES
               INTO (L_MODQTY, L_TSEQ, L_SALESORDER)
        FROM ZTPP_WOSUM
       WHERE WO_SER = L_ORDR
         AND NATION = L_DIST(3)
         AND DEALER = L_DIST+3(2)
         AND EXTC   = L_EXTC
         AND INTC   = L_INTC     .

      L_SEQQTY  = L_SEQ     + L_TSEQ  .      " Total SEQ-Qty..
      PERFORM PLAN_QUANTITY USING L_ORDR   L_DIST  L_EXTC  L_INTC
                         L_MODQTY L_SEQQTY L_PQTY  L_FQTY  'C' .
      UPDATE ZTPP_WOSUM  SET: SEQQTY  = L_SEQQTY
                              PLANQTY = L_PQTY
                          FORECASTQTY = L_FQTY
                        WHERE WO_SER = L_ORDR
                          AND NATION = L_DIST(3)
                          AND DEALER = L_DIST+3(2)
                          AND EXTC   = L_EXTC
                          AND INTC   = L_INTC     .

      CONCATENATE L_ORDR L_DIST L_EXTC L_INTC INTO L_WOSUM.
      IF SY-SUBRC NE 0.
        WA_ERROR = 'X'.
        MESSAGE I002 WITH TEXT-905 '#1 Update error ZTPP_WOSUM' L_WOSUM.

        EXIT.
      ELSE.                                " TO-BE DELETE LINE.
        WRITE AT: /001(010) L_ORDR ,       " TO-BE DELETE LINE.
                   012(005) L_DIST ,       " TO-BE DELETE LINE.
                   018(003) L_EXTC ,       " TO-BE DELETE LINE.
                   022(003) L_INTC ,       " TO-BE DELETE LINE.
                   026(004) SY-SUBRC,      " TO-BE DELETE LINE.
                   031(020) L_SEQQTY .     " TO-BE DELETE LINE.
      ENDIF.

      CLEAR: WA_MATERIAL,  L_DATA, L_DATA[].
      CONCATENATE L_ORDR       L_DIST         INTO WA_MATERIAL .
      CONCATENATE WA_MATERIAL  L_EXTC  L_INTC INTO WA_MATERIAL.

      CLEAR: L_PLNMG, L_TSEQ, L_DATA-ATWRT, AUSP.
      L_DATA-ATNAM = 'P_MITU_QTY'.    APPEND L_DATA.
*     l_data-atnam = 'P_SEQ_QTY'.     APPEND l_data.
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                OBJECT       = WA_MATERIAL
                CTYPE        = '001'
           TABLES
                VAL_TABLE    = L_DATA
           EXCEPTIONS
                NO_DATA      = 1
                ERROR_MODE   = 2
                ERROR_OBJECT = 3
                ERROR_VALUE  = 4
                OTHERS       = 5.

*     READ TABLE l_data INDEX 2.
*     l_plnmg = l_data-atwrt   .
*     l_plnmg = l_plnmg + l_seq.        CLEAR: l_data.
      " MITU Qauntity is MUNIS.. => MITU Sequence reduced the MITU-Qty..
      READ TABLE L_DATA INDEX 1.
      L_PLNMG = L_DATA-ATWRT   .
      L_PLNMG = L_PLNMG - L_MITUQTY.

      PERFORM CALL_BDC_WORKORDER USING WA_MATERIAL L_SEQQTY
                                       L_PQTY      L_FQTY    L_PLNMG.

      IF WA_ERROR = 'X'.
        MESSAGE I002 WITH TEXT-905  '#2 Workorder Color Update error'
WA_MATERIAL.
        EXIT.
      ENDIF.
      WRITE AT: /001(018)  WA_MATERIAL,     " TO-BE DELETE LINE.
                 020(005) 'WO-CL'     ,     " TO-BE DELETE LINE.
                 025(001) SY-VLINE    ,     " TO-BE DELETE LINE.
                 026(004) WA_ERROR    ,     " TO-BE DELETE LINE.
                 030(001) SY-VLINE    ,     " TO-BE DELETE LINE.
                 031(020) L_SEQQTY    .     " TO-BE DELETE LINE.

      " Sales Order Master Change...
      PERFORM CALL_BDC_SALES_ORDER USING L_SALESORDER L_MITUQTY
                                         L_SEQQTY     L_MODQTY  .
      IF WA_ERROR = 'X'.
        MESSAGE I003 WITH TEXT-905 '#3 Sales Order Update error'
                          L_SALESORDER WA_MATERIAL.
        EXIT.
      ENDIF.
      WRITE AT: /001(017) L_SALESORDER,     " TO-BE DELETE LINE.
                 018(007) 'S-ORDER'   ,     " TO-BE DELETE LINE.
                 025(001) SY-VLINE    ,     " TO-BE DELETE LINE.
                 026(004) WA_ERROR    ,     " TO-BE DELETE LINE.
                 030(001) SY-VLINE    ,     " TO-BE DELETE LINE.
                 031(020) L_SEQQTY    .     " TO-BE DELETE LINE.
      ULINE.                                " TO-BE DELETE LINE.
      CLEAR: L_SEQ, L_MITUQTY.
      L_ORDR = IT_7JB-ORDR.  L_DIST = IT_7JB-DIST.
      L_EXTC = IT_7JB-EXTC.  L_INTC = IT_7JB-INTC.
      IF IT_7JB-MTGU = SPACE .
        L_SEQ       = 1.
      ELSE.
        L_MITUQTY   = 1 .
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF WA_ERROR = 'X'.   EXIT.  ENDIF.

  IF WA_LINES > 0 .
    " Work Order Summary Table Update.....

    SELECT SINGLE MODQTY SEQQTY SALES
            INTO (L_MODQTY, L_TSEQ, L_SALESORDER)
      FROM ZTPP_WOSUM
     WHERE WO_SER = L_ORDR            " it_7jb-ordr
       AND NATION = L_DIST(3)         " it_7jb-dist(3)
       AND DEALER = L_DIST+3(2)       " it_7jb-dist+3(2)
       AND EXTC   = L_EXTC            " it_7jb-extc
       AND INTC   = L_INTC     .      " it_7jb-intc     .

    L_SEQQTY  = L_SEQ     + L_TSEQ  .
    PERFORM PLAN_QUANTITY USING L_ORDR   L_DIST   L_EXTC L_INTC
                                L_MODQTY L_SEQQTY L_PQTY L_FQTY 'C' .
    UPDATE ZTPP_WOSUM  SET: SEQQTY  = L_SEQQTY
                            PLANQTY = L_PQTY
                        FORECASTQTY = L_FQTY
                      WHERE WO_SER  = L_ORDR          " it_7jb-ordr
                        AND NATION  = L_DIST(3)       " it_7jb-dist(3)
                        AND DEALER  = L_DIST+3(2)     " it_7jb-dist+3(2)
                        AND EXTC    = L_EXTC          " it_7jb-extc
                        AND INTC    = L_INTC     .    " it_7jb-intc  .

    CLEAR: WA_MATERIAL,  L_DATA, L_DATA[].
    CONCATENATE L_ORDR       L_DIST         INTO WA_MATERIAL .
    CONCATENATE WA_MATERIAL  L_EXTC  L_INTC INTO WA_MATERIAL.

    IF SY-SUBRC NE 0.

      " Step 1: Update Fail - Reason: Data not found!!!
      "         Check the Working data.
      WA_ERROR = 'X'.
      MESSAGE I002 WITH TEXT-905  '#4 Update error ZTPP_WOSUM'
WA_MATERIAL.
      EXIT.
    ELSE.                                  " TO-BE DELETE LINE.

      WRITE AT: /001(010) L_ORDR ,       " TO-BE DELETE LINE.
                 012(005) L_DIST ,       " TO-BE DELETE LINE.
                 018(003) L_EXTC ,       " TO-BE DELETE LINE.
                 022(003) L_INTC ,       " TO-BE DELETE LINE.
                 026(004) SY-SUBRC,      " TO-BE DELETE LINE.
                 031(020) L_SEQQTY .     " TO-BE DELETE LINE.
    ENDIF.


    CLEAR: L_PLNMG, L_TSEQ, L_DATA-ATWRT, AUSP.
    L_DATA-ATNAM = 'P_MITU_QTY'.    APPEND L_DATA.
*   l_data-atnam = 'P_SEQ_QTY'.     APPEND l_data.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT       = WA_MATERIAL
              CTYPE        = '001'
         TABLES
              VAL_TABLE    = L_DATA
         EXCEPTIONS
              NO_DATA      = 1
              ERROR_MODE   = 2
              ERROR_OBJECT = 3
              ERROR_VALUE  = 4
              OTHERS       = 5.

*   READ TABLE l_data INDEX 2.
*   l_plnmg = l_data-atwrt   .
*   l_plnmg = l_plnmg + l_seq.        CLEAR: l_data.
    READ TABLE L_DATA INDEX 1.
    L_PLNMG = L_DATA-ATWRT   .
    L_PLNMG = L_PLNMG - L_MITUQTY.    CLEAR: L_DATA.

    PERFORM CALL_BDC_WORKORDER USING WA_MATERIAL L_SEQQTY
                                     L_PQTY      L_FQTY    L_PLNMG .

    IF WA_ERROR = 'X'.
      MESSAGE I002 WITH TEXT-905  '#5 Workorder Color Update error'
       WA_MATERIAL.
      EXIT.
    ENDIF.
    WRITE AT: /001(018)  WA_MATERIAL,     " TO-BE DELETE LINE.
               020(005) 'WO-CL'     ,     " TO-BE DELETE LINE.
               025(001) SY-VLINE    ,     " TO-BE DELETE LINE.
               026(004) WA_ERROR    ,     " TO-BE DELETE LINE.
               030(001) SY-VLINE    ,     " TO-BE DELETE LINE.
               031(020) L_SEQQTY    .     " TO-BE DELETE LINE.

    " Sales Order Master Change...
    PERFORM CALL_BDC_SALES_ORDER USING L_SALESORDER L_MITUQTY
                                       L_SEQQTY     L_MODQTY  .
    IF WA_ERROR = 'X'.
      MESSAGE I002 WITH TEXT-905 '#6 Sales Order Update error'
              L_SALESORDER.
      EXIT.
    ENDIF.
    WRITE AT: /001(017) L_SALESORDER,     " TO-BE DELETE LINE.
                   018(007) 'S-ORDER'   ,     " TO-BE DELETE LINE.
                   025(001) SY-VLINE    ,     " TO-BE DELETE LINE.
                   026(004) WA_ERROR    ,     " TO-BE DELETE LINE.
                   030(001) SY-VLINE    ,     " TO-BE DELETE LINE.
                   031(020) L_SEQQTY    .     " TO-BE DELETE LINE.
  ENDIF.
ENDFORM.                    " bdc_wocl_summary

*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_DATA.
  DATA: L_PLNMG(4)           TYPE N          ,
        L_SALESORDER         LIKE ZTPP_WOSUM-SALES,
        L_ORDR               LIKE IT_7JB-ORDR,
        L_DIST               LIKE IT_7JB-DIST,
        L_EXTC               LIKE IT_7JB-EXTC,
        L_INTC               LIKE IT_7JB-INTC,
        L_SEQQTY             LIKE ZTPP_WOSUM-SEQQTY ,
        L_MODQTY             LIKE ZTPP_WOSUM-MODQTY ,
        L_PLNQTY             LIKE ZTPP_WOSUM-PLANQTY,
        L_CHKQTY             TYPE I,
        L_COUNT              TYPE I,
        L_CHKPLNQTY          TYPE I,
        L_WOSUM(20),
        L_DATA               LIKE TABLE OF CONF_OUT    WITH HEADER LINE.

  SORT IT_7JB BY ORDR DIST EXTC INTC SSR1.
  CLEAR: L_SEQQTY, L_CHKPLNQTY .
  READ TABLE IT_7JB INDEX 1.
  L_ORDR = IT_7JB-ORDR.  L_DIST = IT_7JB-DIST.
  L_EXTC = IT_7JB-EXTC.  L_INTC = IT_7JB-INTC.

  LOOP AT IT_7JB .
    IF IT_7JB-ORDR = L_ORDR  AND  IT_7JB-DIST = L_DIST AND
       IT_7JB-EXTC = L_EXTC  AND  IT_7JB-INTC = L_INTC .
      IF IT_7JB-MTGU = SPACE .
        L_COUNT      = L_COUNT  + 1.
      ENDIF.
    ELSE.
      " Processing Data Check with ZTPP_WOSUM Table...
      SELECT SINGLE MODQTY SEQQTY INTO (L_MODQTY, L_SEQQTY)
        FROM ZTPP_WOSUM
       WHERE WO_SER = L_ORDR
         AND NATION = L_DIST(3)
         AND DEALER = L_DIST+3(2)
         AND EXTC   = L_EXTC
         AND INTC   = L_INTC     .

      L_CHKQTY  = L_MODQTY - L_SEQQTY .
*      IF l_chkqty < l_count .
*        wa_err_hd = wa_flag  = 'E'    .
*        WRITE AT: /001(20)  text-902,
*                   021(09)  l_ordr ,
*                   031(05)  l_dist ,
*                   037(03)  l_extc ,
*                   041(03)  l_intc .
*        ULINE AT: /(60)            .
*      ENDIF.
      " Check Data for the Plan Quantity...
      SELECT SUM( PQTY ) INTO L_PLNQTY
        FROM ZTPP_PMT07JB_A
       WHERE ORDR = L_ORDR
         AND DIST = L_DIST
         AND EXTC = L_EXTC
         AND INTC = L_INTC
         AND GUBB = 'A'
** Changed by Furong on 10/29/08
         AND GUB1 = '1'.
** End of change on 10/29/08
      L_CHKPLNQTY  = L_COUNT + L_PLNQTY.

      IF L_CHKQTY < L_CHKPLNQTY ."l_plnqty.
        WA_ERR_HD = WA_FLAG  = 'E'    .
        CONCATENATE L_ORDR L_DIST L_EXTC L_INTC INTO L_WOSUM.
        MESSAGE I001 WITH TEXT-904 L_WOSUM.
        WRITE AT: /001(20)  TEXT-904,
                   021(09)  L_ORDR ,
                   031(05)  L_DIST ,
                   037(03)  L_EXTC ,
                   041(03)  L_INTC ,
                   045(10)  L_CHKQTY,
                   056(3)   '< (',
                   060(10)  L_COUNT ,
                   071(1)   '+',
                   073(10)  L_PLNQTY,
                   084(1)   ')'.
        ULINE AT: /(85)            .
      ENDIF.
      IF IT_7JB-MTGU = SPACE .
        L_COUNT = 1 .
      ELSE.
        CLEAR: L_COUNT.
      ENDIF.
      L_ORDR = IT_7JB-ORDR.  L_DIST = IT_7JB-DIST.
      L_EXTC = IT_7JB-EXTC.  L_INTC = IT_7JB-INTC.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_DATA

*&---------------------------------------------------------------------*
*&      Form  update_commonvlas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_COMMONVLAS.
  C_PROG = 'ZIPP101U_PMT07JB_A'.
  SELECT MAX( SQDT ) MIN( SQDT ) INTO (WA_MAXDAY, WA_MINDAY)
    FROM ZTPP_PMT07JB_B .

  UPDATE ZTPP_COMMON_VALS SET: DATES = WA_MINDAY
                               ITEM1 = WA_MAXDAY
                        WHERE JOBS  = C_PROG
                          AND KEY2  = IT_7JB-MODL.
ENDFORM.                    " update_commonvlas

*&---------------------------------------------------------------------*
*&      Form  job_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM JOB_CREATE   USING PA_TYPE.
  DATA: L_7JB     LIKE ZTPP_PMT07JB_B   .

  CLEAR: L_7JB.
  MOVE-CORRESPONDING WA_7JB TO L_7JB .

  DO .
    CASE PA_TYPE.
      WHEN '1'  .
        CALL FUNCTION 'Z_FPP_VEHICLE_CREATION'
            STARTING NEW TASK WA_TASKNAME DESTINATION IN GROUP 'PG_SEQ'
             PERFORMING RETURN_STEP1 ON END OF TASK
             EXPORTING
               P_7JB                       = L_7JB
               P_DATE                      = SY-DATUM
             EXCEPTIONS
               COMMUNICATION_FAILURE       = 1
               SYSTEM_FAILURE              = 2
               RESOURCE_FAILURE            = 3.
*         OTHERS                      = 4.
      WHEN '2' .
        CALL FUNCTION 'Z_FPP_PLANORDER_CREATION'
            STARTING NEW TASK WA_TASKNAME DESTINATION IN GROUP 'PG_SEQ'
             PERFORMING RETURN_STEP2 ON END OF TASK
             EXPORTING
               P_7JB                       = L_7JB
               P_DATE                      = SY-DATUM
             EXCEPTIONS
               COMMUNICATION_FAILURE       = 1
               SYSTEM_FAILURE              = 2
               RESOURCE_FAILURE            = 3.
*         OTHERS                      = 4.
    ENDCASE.

    CASE SY-SUBRC.
      WHEN 0.
        WA_TASKNAME = WA_TASKNAME  + 1.
        WA_SND_JOBS = WA_SND_JOBS  + 1.
        CLEAR: WA_EXCP_FLAG .
        EXIT.
      WHEN 1 OR 2.
        WA_EXCP_FLAG = 'X'.
      WHEN 3.
*Receive reply to asynchronous RFC calls
        IF WA_EXCP_FLAG = SPACE.
          WA_EXCP_FLAG = 'X'.
*First attempt for RESOURCE_Failure handling
          WAIT UNTIL WA_RCV_JOBS >= WA_SND_JOBS UP TO '0.01' SECONDS.
        ELSE.
*Second attempt for RESOURCE_Failure handling
          WAIT UNTIL WA_RCV_JOBS >= WA_SND_JOBS UP TO '0.1' SECONDS.
        ENDIF.
        IF SY-SUBRC = 0.
          CLEAR WA_EXCP_FLAG. " Reset flag
*        ELSE.
*          EXIT.
        ENDIF.
    ENDCASE.
  ENDDO.
ENDFORM.                    " job_create

*&---------------------------------------------------------------------*
*&      Form  check_vin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_VIN.
  CLEAR: IT_VIN.
  WA_7JB = IT_7JB.
  READ TABLE IT_VIN WITH KEY ORDR = WA_7JB-ORDR
                             DIST = WA_7JB-DIST .
ENDFORM.                    " check_vin

*&---------------------------------------------------------------------*
*&      Form  WRITE_TIMESTAMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_011  text
*----------------------------------------------------------------------*
FORM WRITE_TIMESTAMP USING    PA_TEXT.
  GET TIME.
  WRITE AT: /001(030)  PA_TEXT,
             031(015)  SY-DATUM,
             047(015)  SY-UZEIT.
ENDFORM.                    " WRITE_TIMESTAMP

*&---------------------------------------------------------------------*
*&      Form  WRITE_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_RESULT.
  DATA: L_LINES             TYPE I.

  DESCRIBE TABLE IT_7JB     LINES L_LINES.
  WRITE AT:/001(020) TEXT-015 ,
            022(010) L_LINES  .
ENDFORM.                    " WRITE_RESULT

*&---------------------------------------------------------------------*
*&      Form  VIN_CODE_GEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VIN_CODE_GEN.
  DATA: L_VIN                LIKE MARA-MATNR ,
        L_MODE               LIKE ZTPP_COMMON_VALS-KEY2.

  " Call the function for the Last Data...
* l_mode = 'EMF'.      " wa_7jb-modl .
  L_MODE = IT_7JB-MODL .
  L_VIN = IT_VIN-VINN  .

  CALL FUNCTION 'Z_FPP_VIN_GENERATION'
       EXPORTING
            W_ORDER   = L_VIN
            MODE      = L_MODE
       IMPORTING
            P_LASTID  = L_VIN
       EXCEPTIONS
            NOT_FOUND = 1
            OTHERS    = 2.

  IF SY-SUBRC <> 0.
    WA_ERROR = 'X'.
    MESSAGE I002 WITH TEXT-905 '#12 Error at VIN GENERATION ' L_VIN.

    WRITE AT: /001(025) 'Error of VIN-Generation: ',
               027(003) SY-SUBRC                   ,
               031(004) L_MODE                     .
    EXIT.
  ENDIF.

  IT_7JB-VINN = WA_7JB-VINN = L_VIN     .

  " Update the ZTPP_PMT07JB_B..
  UPDATE ZTPP_PMT07JB_B   SET  VINN  = L_VIN
                        WHERE SQDT  = WA_7JB-SQDT
                          AND MODL  = WA_7JB-MODL
                          AND MTGU  = WA_7JB-MTGU
                          AND SSR1  = WA_7JB-SSR1
                          AND SSR2  = WA_7JB-SSR2 .

  IF SY-SUBRC NE 0  OR L_VIN = SPACE .
    "Error Data
    WA_ERROR = 'X' .
    MESSAGE I002 WITH TEXT-905 '#13 Error Update ZTPP_PMT07JB_B' L_VIN.
  ENDIF.
ENDFORM.                    " VIN_CODE_GEN

*&---------------------------------------------------------------------*
*&      Form  select_vin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_VIN.
  SELECT SINGLE * INTO WA_7JB
    FROM ZTPP_PMT07JB_B
   WHERE SQDT  = IT_7JB-SQDT
     AND MODL  = IT_7JB-MODL
     AND MTGU  = IT_7JB-MTGU
     AND SSR1  = IT_7JB-SSR1 .
ENDFORM.                    " select_vin

*&---------------------------------------------------------------------*
*&      Form  clear_vinn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_VINN.
  DATA: L_PORDER            LIKE ZTPP_PMT07JB_B-PLNUM,
        L_VINN              LIKE ZTPP_PMT07JB_B-VINN.

  CLEAR: L_VINN, L_PORDER.
  UPDATE ZTPP_PMT07JB_B   SET VINN  = L_VINN
                        WHERE VINN NE L_VINN .
  COMMIT WORK.

  UPDATE ZTPP_PMT07JB_B   SET PLNUM = L_PORDER
                        WHERE PLNUM NE L_PORDER.
  COMMIT WORK.

  MESSAGE I001 WITH TEXT-905 '#11 Update ZTPP_WOSUM In clear vinn'.

ENDFORM.                    " clear_vinn

*&---------------------------------------------------------------------*
*&      Form  check_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_RESULT.
  " Check the result of the Plan Order & Vehicle Master.
ENDFORM.                    " check_result

*&---------------------------------------------------------------------*
*&      Form  call_bapi_salesorder
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PA_SORDER  text
*      -->P_L_VAL10  text
*      -->P_L_VAL20  text
*----------------------------------------------------------------------*
FORM CALL_BAPI_SALESORDER USING    PA_ORDER  PA_VAL10  PA_VAL20.
  DATA : L_IT_ORD_HEADER_INX LIKE TABLE OF BAPISDH1X  WITH HEADER LINE,
         L_BAPISDLS          LIKE BAPISDLS ,
         L_IT_RETURN         LIKE TABLE OF BAPIRET2   WITH HEADER LINE,
         L_IT_ITM            LIKE TABLE OF BAPISDITM  WITH HEADER LINE,
         L_IT_ITMX           LIKE TABLE OF BAPISDITMX WITH HEADER LINE,
         L_IT_LINES          LIKE TABLE OF BAPISCHDL  WITH HEADER LINE,
         L_IT_LINESX         LIKE TABLE OF BAPISCHDLX WITH HEADER LINE.

  DATA : P_ITEM10_ORG         LIKE VBAP-KWMENG,
         P_ITEM20_ORG         LIKE VBAP-KWMENG,
         P_ITEM20_QTY         LIKE ZTPP_WOSUM-MODQTY,
         P_ITEM10_QTY         LIKE ZTPP_WOSUM-SEQQTY,
         L_ITEM10_FLG(01),
         L_ITEM20_FLG(01),
         L_ITEM10_QTY_FLG(01),
         L_ITEM20_QTY_FLG(01).

  DATA: LW-MESSAGE LIKE L_IT_RETURN-MESSAGE.

  P_ITEM10_QTY = PA_VAL10 .
  P_ITEM20_QTY = PA_VAL20 .

  SELECT SINGLE KWMENG INTO P_ITEM10_ORG
    FROM VBAP
   WHERE VBELN = PA_ORDER
     AND POSNR = '000010' .

  SELECT SINGLE KWMENG INTO P_ITEM20_ORG
    FROM VBAP
   WHERE VBELN = PA_ORDER
     AND POSNR = '000020' .

  " Check Logic..
  "   1) p_item10_org =  0,     " Seq. Qty before sequence..
  "      l_item10_flg --> 'I'   l_item10_qty_flg --> 'I'
  "      l_item20_flg --> 'I'   l_item20_qty_flg --> 'I'
  "   2) p_item10_QTY =  0,     " Seq. Qty after sequence..
  "      l_item10_flg --> 'D'   l_item10_qty_flg --> 'D'
  "      l_item20_flg --> 'D'   l_item20_qty_flg --> 'D'
  "   3) OTHERS.
  "      l_item10_flg --> 'U'   l_item10_qty_flg --> 'U'
  "      l_item20_flg --> 'U'   l_item20_qty_flg --> 'U'

  IF P_ITEM10_ORG =  0 AND P_ITEM10_QTY =  0.
    L_IT_RETURN-TYPE = 'A'   .
    WA_ERROR = 'X'           .
    EXIT.
  ELSE.
    IF P_ITEM10_ORG =  0     .
      L_ITEM10_FLG     = 'I'   .
      L_ITEM10_QTY_FLG = 'I'   .
      IF P_ITEM20_QTY = 0   .
        L_ITEM20_FLG     = 'U'   .
        L_ITEM20_QTY_FLG = 'D'   .
      ELSE.
        L_ITEM20_FLG     = 'U'   .
        L_ITEM20_QTY_FLG = 'U'   .
      ENDIF.
    ELSE.
      L_ITEM10_FLG     = 'U'   .
      L_ITEM10_QTY_FLG = 'U'   .
      IF P_ITEM20_QTY = 0   .
        L_ITEM20_FLG     = 'U'   .
        L_ITEM20_QTY_FLG = 'D'   .
      ELSE.
        L_ITEM20_FLG     = 'U'   .
        L_ITEM20_QTY_FLG = 'U'   .
      ENDIF.
    ENDIF.
  ENDIF.

  L_BAPISDLS-SCHEDULING = 'X'.

  L_IT_ORD_HEADER_INX-UPDATEFLAG = 'U'.
  APPEND L_IT_ORD_HEADER_INX.

  L_IT_ITM-ITM_NUMBER = '000010'.
  APPEND L_IT_ITM.
  L_IT_ITM-ITM_NUMBER = '000020'.
  APPEND L_IT_ITM.

  L_IT_ITMX-UPDATEFLAG = L_ITEM10_FLG.
  L_IT_ITMX-ITM_NUMBER = '000010'.
  APPEND L_IT_ITMX.
  L_IT_ITMX-UPDATEFLAG = L_ITEM20_FLG.
  L_IT_ITMX-ITM_NUMBER = '000020'.
  APPEND L_IT_ITMX.

  P_ITEM10_ORG = PA_VAL10 .
  P_ITEM20_ORG = PA_VAL20 .

  L_IT_LINES-ITM_NUMBER = '000010'.
  IF L_ITEM10_QTY_FLG = 'I'       .
    L_IT_LINES-SCHED_LINE = '0001'.
  ELSE.
    SELECT SINGLE ETENR INTO L_IT_LINES-SCHED_LINE
      FROM VBEP
     WHERE VBELN = PA_ORDER
       AND POSNR = L_IT_LINES-ITM_NUMBER .
  ENDIF.
  L_IT_LINES-REQ_QTY = P_ITEM10_ORG.
  APPEND L_IT_LINES.

  L_IT_LINESX-UPDATEFLAG = L_ITEM10_QTY_FLG.
  L_IT_LINESX-ITM_NUMBER = '000010'.
  L_IT_LINESX-SCHED_LINE = L_IT_LINES-SCHED_LINE .
  L_IT_LINESX-REQ_QTY = 'X'.
  APPEND L_IT_LINESX.

  L_IT_LINES-ITM_NUMBER = '000020'.
  IF L_ITEM20_QTY_FLG = 'I'       .
    L_IT_LINES-SCHED_LINE = '0001'.
  ELSE.
    SELECT SINGLE ETENR INTO L_IT_LINES-SCHED_LINE
      FROM VBEP
     WHERE VBELN = PA_ORDER
       AND POSNR = L_IT_LINES-ITM_NUMBER .
  ENDIF.
  L_IT_LINES-REQ_QTY = P_ITEM20_ORG.
  APPEND L_IT_LINES.

  L_IT_LINESX-UPDATEFLAG = L_ITEM20_QTY_FLG.
  L_IT_LINESX-ITM_NUMBER = '000020'.
  L_IT_LINESX-SCHED_LINE = L_IT_LINES-SCHED_LINE .
  L_IT_LINESX-REQ_QTY = 'X'.
  APPEND L_IT_LINESX.

  DO.
    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
         EXPORTING
              SALESDOCUMENT    = PA_ORDER
              ORDER_HEADER_INX = L_IT_ORD_HEADER_INX
              LOGIC_SWITCH     = L_BAPISDLS
         TABLES
              RETURN           = L_IT_RETURN
              ORDER_ITEM_IN    = L_IT_ITM
              ORDER_ITEM_INX   = L_IT_ITMX
              SCHEDULE_LINES   = L_IT_LINES
              SCHEDULE_LINESX  = L_IT_LINESX.
** changed by Furong on 11/22/2006
    READ TABLE L_IT_RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0.
      IF LW-MESSAGE <> L_IT_RETURN-MESSAGE.
        LW-MESSAGE = L_IT_RETURN-MESSAGE.
        WRITE: / PA_ORDER, ':', L_IT_RETURN-TYPE, L_IT_RETURN-MESSAGE.
      ENDIF.
      WAIT UP TO 2 SECONDS.
      CLEAR: L_IT_RETURN, L_IT_RETURN[].
      CONTINUE.
    ELSE.
      READ TABLE L_IT_RETURN WITH KEY TYPE = 'A'.
      IF SY-SUBRC = 0.
        IF LW-MESSAGE <> L_IT_RETURN-MESSAGE.
          LW-MESSAGE = L_IT_RETURN-MESSAGE.
          WRITE: / PA_ORDER, ':', L_IT_RETURN-TYPE, L_IT_RETURN-MESSAGE.
        ENDIF.
        WAIT UP TO 2 SECONDS.
        CLEAR: L_IT_RETURN, L_IT_RETURN[].
        CONTINUE.
      ELSE.
        LOOP AT L_IT_RETURN .   " WHERE type = 'E' OR type = 'A' .
         WRITE: / PA_ORDER, ':', L_IT_RETURN-TYPE, L_IT_RETURN-MESSAGE .
        ENDLOOP.
        EXIT.
      ENDIF.
    ENDIF.
  ENDDO.

*  LOOP AT l_it_return .   " WHERE type = 'E' OR type = 'A' .
*    WRITE: / pa_order, ':', l_it_return-type, l_it_return-message .
*    IF l_it_return-type = 'E' OR
*       l_it_return-type = 'A'   .
*      wa_error = 'X'           .
*      EXIT.
*    ENDIF.
*  ENDLOOP.
** end of change
ENDFORM.                    " call_bapi_salesorder

*&---------------------------------------------------------------------*
*&      Form  create_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0346   text
*----------------------------------------------------------------------*
FORM CREATE_LOG USING    PA_STEP  PA_DATA LIKE IT_7JB .
  DATA: L_LOG                LIKE ZTPP_REP_SEQ .

  CLEAR: L_LOG.
  SELECT MAX( SEQUENCE ) INTO L_LOG-SEQUENCE
    FROM ZTPP_REP_SEQ
   WHERE WK_DATE  = WA_DATE .

  L_LOG-WK_DATE   = WA_DATE            .
  L_LOG-SEQUENCE  = L_LOG-SEQUENCE + 1 .
  L_LOG-STEP      = PA_STEP            .
  L_LOG-STATUS    = 'E'                .
  L_LOG-LOGTYPE   = 'E'                .

  CASE PA_STEP.
    WHEN '1'  .
    WHEN '2'  .
    WHEN '3'  .
      MOVE-CORRESPONDING PA_DATA  TO L_LOG .
      L_LOG-MSG = IT_7JB-VINN              .
    WHEN '4'  .
      MOVE-CORRESPONDING PA_DATA  TO L_LOG .
    WHEN '5'  .
      MOVE-CORRESPONDING PA_DATA  TO L_LOG .
  ENDCASE.
  INSERT INTO ZTPP_REP_SEQ VALUES L_LOG    .
ENDFORM.                    " create_log

*&---------------------------------------------------------------------*
*&      Form  return_STEP1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RETURN_STEP1  USING P_TASKNAME.
  RECEIVE RESULTS FROM FUNCTION 'Z_FPP_VEHICLE_CREATION'
         EXCEPTIONS
         COMMUNICATION_FAILURE       = 1
         SYSTEM_FAILURE              = 2
         RESOURCE_FAILURE            = 3
         OTHERS                      = 4.

  CHECK SY-SUBRC = 0.
  WA_RCV_JOBS  = WA_RCV_JOBS + 1.
ENDFORM.                    " return_STEP1

*&---------------------------------------------------------------------*
*&      Form  return_STEP2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RETURN_STEP2  USING P_TASKNAME.
  RECEIVE RESULTS FROM FUNCTION 'Z_FPP_PLANORDER_CREATION'
         EXCEPTIONS
         COMMUNICATION_FAILURE       = 1
         SYSTEM_FAILURE              = 2
         RESOURCE_FAILURE            = 3
         OTHERS                      = 4.

  CHECK SY-SUBRC = 0.
  WA_RCV_JOBS  = WA_RCV_JOBS + 1.
ENDFORM.                    " return_STEP2

*&---------------------------------------------------------------------*
*&      Form  CREATE_PCC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_PCC USING PA_MATNR  PA_WERKS  PA_TEXT  PA_VERID  PA_FLAG .
  DATA: L_MATNR  LIKE BDCDATA-FVAL,
        L_WERKS  LIKE BDCDATA-FVAL,
        L_KTEXT  LIKE BDCDATA-FVAL,
        L_VERID  LIKE BDCDATA-FVAL,
        L_FLAG   LIKE BDCDATA-FVAL.

  L_MATNR = PA_MATNR.   L_WERKS = PA_WERKS.
  L_KTEXT = PA_TEXT .   L_VERID = PA_VERID.

  CLEAR: WA_SUBRC, IT_MSG, IT_MSG[].
  CALL FUNCTION 'Z_FCO_PCC_ORDER_CRE_WITH_PDV'
       EXPORTING
            MATNR_001 = L_MATNR
            WERKS_002 = L_WERKS
            KTEXT_004 = L_KTEXT
            VERID_007 = L_VERID
            P_FIRST   = PA_FLAG
       IMPORTING
            SUBRC     = WA_SUBRC
       TABLES
            MESSTAB   = IT_MSG.

  READ TABLE IT_MSG WITH KEY MSGTYP = 'E'.
  IF SY-SUBRC = 0.
    LOOP AT IT_MSG WHERE MSGTYP = 'E'.
      MESSAGE I002 WITH TEXT-905 '#18 PCC UPDATE ERROR' L_MATNR.
      WRITE: / IT_MSG                .
      EXIT.
    ENDLOOP.
  ELSE.
    WRITE: / 'Success: ', L_KTEXT .
  ENDIF.
ENDFORM.                    " CREATE_PCC

*&---------------------------------------------------------------------*
*&      Form  CHECK_PCC_ROUTINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_PCC_ROUTINE.
  DATA: LT_7JB               LIKE TABLE OF IT_7JB      WITH HEADER LINE,
        L_MATNR              LIKE MARA-MATNR,
        L_VERID              LIKE MKAL-VERID ,
        L_TEXT               LIKE MAKT-MAKTX ,
        L_FLAG               TYPE C          .
  DATA: L_LEN TYPE I,
        L_NEW_DEALER(1).

  LT_7JB[] = IT_7JB[]       .
  LOOP AT LT_7JB.
** Changed by Furong on 10/09/07 for EBOM
*    CONCATENATE LT_7JB-MOYE  LT_7JB-DIST LT_7JB-BMDL INTO LT_7JB-MATNR
*.
*    CONCATENATE LT_7JB-MATNR LT_7JB-OCNN             INTO LT_7JB-MATNR
*      SEPARATED BY SPACE.
    L_LEN = STRLEN( IT_7JB-BMDL ).
    IF L_LEN = 7.
     CONCATENATE IT_7JB-MOYE  IT_7JB-DIST IT_7JB-BMDL INTO IT_7JB-MATNR.
     CONCATENATE IT_7JB-MATNR IT_7JB-OCNN             INTO IT_7JB-MATNR
             SEPARATED BY SPACE.
    ELSE.
      CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
           EXPORTING
                OLD_DEALER = IT_7JB-DIST+3(2)
           IMPORTING
                NEW_DEALER = L_NEW_DEALER.
      CONCATENATE IT_7JB-MOYE IT_7JB-DIST+0(3) L_NEW_DEALER IT_7JB-BMDL
                    INTO IT_7JB-MATNR.
      CONCATENATE IT_7JB-MATNR IT_7JB-OCNN INTO IT_7JB-MATNR.
    ENDIF.
** End of change
    MODIFY LT_7JB .
  ENDLOOP.

  SORT IT_7JB BY MATNR VERS .
  DELETE ADJACENT DUPLICATES FROM LT_7JB COMPARING MATNR VERS .

  LOOP AT LT_7JB            .
    " Function Call for the PCC check...
    CLEAR: L_FLAG.
    L_VERID = LT_7JB-VERS+1(2) .
    PERFORM CHECK_PCC_FUNCTION   USING  LT_7JB-MATNR   L_VERID  L_FLAG .
    CHECK L_FLAG = SPACE OR L_FLAG = 'X' .
    CLEAR: L_TEXT .
    CONCATENATE LT_7JB-MATNR L_VERID                 INTO L_TEXT .
    PERFORM CREATE_PCC USING LT_7JB-MATNR   'P001'
                             L_TEXT   L_VERID  L_FLAG .
  ENDLOOP.
ENDFORM.                    " CHECK_PCC_ROUTINE

*&---------------------------------------------------------------------*
*&      Form  CHECK_PCC_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_7JB_MATNR  text
*----------------------------------------------------------------------*
FORM CHECK_PCC_FUNCTION USING    PA_MATNR  PA_VERID  PA_FLAG .
  DATA: LP_PROCNR        LIKE AUFK-PROCNR,
        LP_VERID         LIKE AFPO-VERID ,
        LP_STLAN         LIKE MKAL-STLAN ,
        LP_STLAL         LIKE MKAL-STLAL ,
        LP_PLNTY         LIKE PLKO-PLNTY ,
        LP_PLNNR         LIKE PLKO-PLNNR ,
        LP_PLNAL         LIKE PLKO-PLNAL ,
        LP_AUFNR         LIKE AUFK-AUFNR ,
        LW_KEKO          LIKE KEKO       ,
        LT_AFKO          LIKE TABLE OF AFKO            WITH HEADER LINE,
        LT_VKKS0         LIKE TABLE OF VKKS0           WITH HEADER LINE,
        LT_PKOSA         LIKE TABLE OF PKOSA           WITH HEADER LINE.

  PA_FLAG = 'X'.
  CALL FUNCTION 'KK_F_PKOSA_FIND'
       EXPORTING
            I_MATNR               = PA_MATNR
            I_WERKS               = 'P001'
            I_PWERK               = 'P001'
            I_VERID               = PA_VERID
       IMPORTING
            E_PROCNR              = LP_PROCNR
            E_VERID               = LP_VERID
            E_STLAN               = LP_STLAN
            E_STLAL               = LP_STLAL
            E_PLNTY               = LP_PLNTY
            E_PLNNR               = LP_PLNNR
            E_PLNAL               = LP_PLNAL
            E_AUFNR               = LP_AUFNR
       TABLES
            E_VKKS0               = LT_VKKS0
            E_PKOSA               = LT_PKOSA
       EXCEPTIONS
            NONE_FOUND            = 1
            WRONG_INPUT           = 2
            NONE_PICKED           = 3
            WRONG_RULE            = 4
            RSH_NOT_VALID         = 5
            WRONG_CHARACTERISTICS = 6
            NO_RULE               = 7
            VERSION_NOT_VALID     = 8
            OTHERS                = 9.

  CASE SY-SUBRC  .
    WHEN 0.
      PA_FLAG = 'S' .     " Can not call the PCC Function..
    WHEN 1.
      " Check the Standard Costing Estimate REsult...
      " If the Data is make without error... Continue...
      " Else Error...
      SELECT SINGLE * INTO LW_KEKO
        FROM KEKO
       WHERE MATNR = PA_MATNR
         AND TVERS = '01'
         AND WERKS = 'P001'
         AND KOKRS = 'H201'
         AND FEH_STA = 'FR'
         AND KLVAR = 'PPC1'
         AND KADAT <= SY-DATUM
         AND BIDAT >= SY-DATUM .

      IF SY-SUBRC NE 0.
        " Display the Message...
        WRITE: / PA_MATNR, '(', PA_VERID, ')',  TEXT-016.
        PA_FLAG = 'S'.  EXIT.
      ENDIF.
    WHEN 8.
      PA_FLAG = 'E' .     " Error - Un-Respected Scenario.
    WHEN OTHERS.
      PA_FLAG = 'E' .     " Error - Un-Respected Scenario.
  ENDCASE.
ENDFORM.                    " CHECK_PCC_FUNCTION

*&---------------------------------------------------------------------*
*&      Form  PLAN_QUANTITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ORDR  text
*      -->P_L_DIST  text
*      -->P_L_EXTC  text
*      -->P_L_INTC  text
*      -->P_L_SEQQTY  text
*      -->P_L_PQTY  text
*      -->P_L_FQTY  text
*      -->P_1528   text
*----------------------------------------------------------------------*
FORM PLAN_QUANTITY USING    PA_ORDR    PA_DIST  PA_EXTC  PA_INTC
                   PA_MQTY  PA_SQTY    PA_PQTY  PA_FQTY  PA_TYPE .
  DATA: L_QTY      TYPE I .

  IF PA_TYPE = 'C' .
    SELECT SUM( PQTY ) INTO  L_QTY
      FROM ZTPP_PMT07JB_A
     WHERE ORDR = PA_ORDR
       AND DIST = PA_DIST
       AND EXTC = PA_EXTC
       AND INTC = PA_INTC
       AND GUBB = 'A'    .
*      AND gub1 = '1'    .

    PA_PQTY = L_QTY      .
    PA_FQTY = PA_MQTY - PA_SQTY - L_QTY .
    IF PA_FQTY LT 0.
      PA_FQTY = 0.
    ENDIF.
  ELSE.
    SELECT SUM( PQTY ) INTO  L_QTY
      FROM ZTPP_PMT07JB_A
     WHERE ORDR = PA_ORDR
       AND DIST = PA_DIST
       AND GUBB = 'A'    .
*      AND gub1 = '1'    .

    PA_PQTY = L_QTY      .
    PA_FQTY = PA_MQTY - PA_SQTY - L_QTY .
  ENDIF.
ENDFORM.                    " PLAN_QUANTITY

*&---------------------------------------------------------------------*
*&      Form  WRITE_HEAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_HEAD.                          " TO-BE DELETE LINE.
  WRITE AT: /001(010) 'WO_SER' ,          " TO-BE DELETE LINE.
             011(001) SY-VLINE ,          " TO-BE DELETE LINE.
             012(005) 'DEST'   ,          " TO-BE DELETE LINE.
             017(001) SY-VLINE ,          " TO-BE DELETE LINE.
             018(003) 'EXT'    ,          " TO-BE DELETE LINE.
             021(001) SY-VLINE ,          " TO-BE DELETE LINE.
             022(003) 'INT'    ,          " TO-BE DELETE LINE.
             025(001) SY-VLINE ,          " TO-BE DELETE LINE.
             026(004) 'RTC'    ,          " TO-BE DELETE LINE.
             030(001) SY-VLINE ,          " TO-BE DELETE LINE.
             031(020) 'SEQ_QTY'.          " TO-BE DELETE LINE.
  ULINE .                                 " TO-BE DELETE LINE.
  SKIP 2.                                 " TO-BE DELETE LINE.
  ULINE .                                 " TO-BE DELETE LINE.
ENDFORM.                    " WRITE_HEAD

*&---------------------------------------------------------------------*
*&      Form  CALL_PLAN_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_PLAN_SAVE.
  DATA: LT_7JB               LIKE TABLE OF IT_7JB      WITH HEADER LINE,
        L_MATNR              LIKE MARA-MATNR,
        L_VERID              LIKE MKAL-VERID ,
        L_TEXT               LIKE MAKT-MAKTX ,
        L_FLAG               TYPE C          .

  " Save the Daily Plan..
  LT_7JB[] = IT_7JB[]  .
  DELETE ADJACENT DUPLICATES FROM LT_7JB COMPARING SQDT.

  LOOP AT LT_7JB       .
    DELETE FROM ZTPP_DAY_PLAN WHERE SQDT = LT_7JB-SQDT .
  ENDLOOP.

  MODIFY ZTPP_DAY_PLAN FROM TABLE IT_7JB .

  MESSAGE I001 WITH TEXT-905 '#19 Successful Processing'.

ENDFORM.                    " CALL_PLAN_SAVE

*&---------------------------------------------------------------------*
*&      Form  BACK_UP_DATA1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BACK_UP_DATA1 .
  DATA: L_MODQTY             LIKE ZTPP_WOSUM-MODQTY ,
        L_SALESORDER         LIKE ZTPP_WOSUM-SALES,
        L_ORDR               LIKE IT_7JB-ORDR,
        L_DIST               LIKE IT_7JB-DIST,
        L_EXTC               LIKE IT_7JB-EXTC,
        L_INTC               LIKE IT_7JB-INTC,
        L_QTY                TYPE P DECIMALS 0,
        L_SEQ                LIKE ZTPP_WOSUM-SEQQTY ,
        L_SEQQTY             LIKE ZTPP_WOSUM-SEQQTY ,
        L_TSEQ               LIKE ZTPP_WOSUM-SEQQTY ,
        L_PQTY               LIKE ZTPP_WOSUM-SEQQTY ,
        L_FQTY               LIKE ZTPP_WOSUM-SEQQTY ,
        L_MITUQTY            LIKE ZTPP_WOSUM-MITUQTY,
        L_DATA               LIKE TABLE OF ZSPP_VIN_VALUE
                                                       WITH HEADER LINE.

  " Processing Process:  1. Sales Order's Quantity
  "                      2. Work Order Header's SEQ/PLAN/FORECASR/MITU
  "                      3. Work Order Color's SEQ/PLAN/FORECASR/MITU
  "                      4. Work Order Summary's SEQ/PLAN/FORECASR/MITU
  DELETE FROM ZTPP_SEQ_BACKUP CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
  SORT IT_7JB BY ORDR DIST EXTC INTC SSR1.
  CLEAR: L_SEQQTY, L_MITUQTY, L_SEQ .
  READ TABLE IT_7JB INDEX 1.
  L_ORDR = IT_7JB-ORDR.  L_DIST = IT_7JB-DIST.
  L_EXTC = IT_7JB-EXTC.  L_INTC = IT_7JB-INTC.

  LOOP AT IT_7JB .
    IF IT_7JB-ORDR = L_ORDR  AND  IT_7JB-DIST = L_DIST AND
       IT_7JB-EXTC = L_EXTC  AND  IT_7JB-INTC = L_INTC .
      CONTINUE.
    ELSE.
      CLEAR: ZTPP_SEQ_BACKUP.
** changed by furong on 10/31/05
      SELECT SINGLE MODQTY SEQQTY PLANQTY FORECASTQTY SALES MITUQTY
        INTO (L_MODQTY, L_SEQQTY, L_PQTY, L_FQTY, L_SALESORDER,
              L_MITUQTY)
        FROM ZTPP_WOSUM
       WHERE WO_SER = L_ORDR
         AND NATION = L_DIST(3)
         AND DEALER = L_DIST+3(2)
         AND EXTC   = L_EXTC
         AND INTC   = L_INTC     .

*      SELECT SINGLE modqty seqqty planqty forecastqty sales mituqty
*        INTO (l_modqty, l_tseq, l_pqty, l_fqty, l_salesorder,
*              l_mituqty)
*        FROM ztpp_wosum
*       WHERE wo_ser = l_ordr
*         AND nation = l_dist(3)
*         AND dealer = l_dist+3(2)
*         AND extc   = l_extc
*         AND intc   = l_intc     .
*
** end of change
      CLEAR: WA_MATERIAL,  L_DATA, L_DATA[].
      CONCATENATE L_ORDR       L_DIST         INTO WA_MATERIAL .
      CONCATENATE WA_MATERIAL  L_EXTC  L_INTC INTO WA_MATERIAL.

      ZTPP_SEQ_BACKUP-TABLES = 'ZTPP_WOSUM'      .
      ZTPP_SEQ_BACKUP-WORDER = WA_MATERIAL       .
      ZTPP_SEQ_BACKUP-FNAME  = 'SEQQTY'          .
      ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_SEQQTY  .
** Changed by Furong on 10/29/08
      ZTPP_SEQ_BACKUP-CUSR = SY-UNAME.
      ZTPP_SEQ_BACKUP-CTIM = SY-UZEIT.
      ZTPP_SEQ_BACKUP-CDAT = SY-DATUM.
** End of change on 10/29/08
      MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.
      ZTPP_SEQ_BACKUP-FNAME  = 'PLANQTY'         .
      ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_PQTY    .
      MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.
      ZTPP_SEQ_BACKUP-FNAME  = 'FORECASTQTY'     .
      ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_FQTY    .
      MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.
      ZTPP_SEQ_BACKUP-FNAME  = 'MITUQTY'         .
      ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_MITUQTY .
      MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.

      CLEAR: L_TSEQ, L_DATA-ATWRT, AUSP, ZTPP_SEQ_BACKUP.
      L_DATA-ATNAM = 'P_SEQ_QTY'.        APPEND L_DATA.
      L_DATA-ATNAM = 'P_PLAN_QTY'.       APPEND L_DATA.
      L_DATA-ATNAM = 'P_FORECAST_QTY'.   APPEND L_DATA.
      L_DATA-ATNAM = 'P_MITU_QTY'.       APPEND L_DATA.
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                OBJECT       = WA_MATERIAL
                CTYPE        = '001'
           TABLES
                VAL_TABLE    = L_DATA
           EXCEPTIONS
                NO_DATA      = 1
                ERROR_MODE   = 2
                ERROR_OBJECT = 3
                ERROR_VALUE  = 4
                OTHERS       = 5.

      READ TABLE L_DATA INDEX 1.
      L_SEQQTY   = L_DATA-ATWRT   .        CLEAR: L_DATA.
      READ TABLE L_DATA INDEX 2.
      L_PQTY     = L_DATA-ATWRT   .        CLEAR: L_DATA.
      READ TABLE L_DATA INDEX 3.
      L_FQTY     = L_DATA-ATWRT   .        CLEAR: L_DATA.
      READ TABLE L_DATA INDEX 4.
      L_MITUQTY  = L_DATA-ATWRT   .        CLEAR: L_DATA.

      ZTPP_SEQ_BACKUP-TABLES = 'WOCL'          .
      ZTPP_SEQ_BACKUP-WORDER = WA_MATERIAL     .
      ZTPP_SEQ_BACKUP-FNAME  = 'P_SEQ_QTY'       .
      ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_SEQQTY  .
** Changed by Furong on 10/29/08
      ZTPP_SEQ_BACKUP-CUSR = SY-UNAME.
      ZTPP_SEQ_BACKUP-CTIM = SY-UZEIT.
      ZTPP_SEQ_BACKUP-CDAT = SY-DATUM.
** End of change on 10/29/08
      MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.
      ZTPP_SEQ_BACKUP-FNAME  = 'P_PLAN_QTY'      .
      ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_PQTY    .
      MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.
      ZTPP_SEQ_BACKUP-FNAME  = 'P_FORECAST_QTY'  .
      ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_FQTY    .
      MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.
      ZTPP_SEQ_BACKUP-FNAME  = 'P_MITU_QTY'      .
      ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_MITUQTY .
      MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.

      " Sales Order Master Change...
      CLEAR: ZTPP_SEQ_BACKUP, L_QTY.
      ZTPP_SEQ_BACKUP-TABLES = 'VBAP'          .
      ZTPP_SEQ_BACKUP-WORDER = L_SALESORDER    .
      SELECT SINGLE KWMENG INTO L_QTY
        FROM VBAP
       WHERE VBELN = L_SALESORDER
         AND POSNR = '000010' .

      ZTPP_SEQ_BACKUP-FNAME  = '000010'        .
      ZTPP_SEQ_BACKUP-VALS   = L_QTY           .
** Changed by Furong on 10/29/08
      ZTPP_SEQ_BACKUP-CUSR = SY-UNAME.
      ZTPP_SEQ_BACKUP-CTIM = SY-UZEIT.
      ZTPP_SEQ_BACKUP-CDAT = SY-DATUM.
** End of change on 10/29/08

      MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.   CLEAR: L_QTY.

      SELECT SINGLE KWMENG INTO L_QTY
        FROM VBAP
       WHERE VBELN = L_SALESORDER
         AND POSNR = '000020' .

      ZTPP_SEQ_BACKUP-FNAME  = '000020'        .
      ZTPP_SEQ_BACKUP-VALS   = L_QTY           .
** Changed by Furong on 10/29/08
      ZTPP_SEQ_BACKUP-CUSR = SY-UNAME.
      ZTPP_SEQ_BACKUP-CTIM = SY-UZEIT.
      ZTPP_SEQ_BACKUP-CDAT = SY-DATUM.
** End of change on 10/29/08

      MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.   CLEAR: L_QTY.

      CLEAR: L_SEQ, L_MITUQTY.
      L_ORDR = IT_7JB-ORDR.  L_DIST = IT_7JB-DIST.
      L_EXTC = IT_7JB-EXTC.  L_INTC = IT_7JB-INTC.
    ENDIF.
  ENDLOOP.

  IF WA_LINES > 0 .
    CLEAR: ZTPP_SEQ_BACKUP.
    SELECT SINGLE MODQTY SEQQTY PLANQTY FORECASTQTY SALES MITUQTY
      INTO (L_MODQTY, L_SEQQTY, L_PQTY, L_FQTY, L_SALESORDER, L_MITUQTY)
      FROM ZTPP_WOSUM
     WHERE WO_SER = L_ORDR
       AND NATION = L_DIST(3)
       AND DEALER = L_DIST+3(2)
       AND EXTC   = L_EXTC
       AND INTC   = L_INTC     .

    CLEAR: WA_MATERIAL,  L_DATA, L_DATA[].
    CONCATENATE L_ORDR       L_DIST         INTO WA_MATERIAL .
    CONCATENATE WA_MATERIAL  L_EXTC  L_INTC INTO WA_MATERIAL.

    ZTPP_SEQ_BACKUP-TABLES = 'ZTPP_WOSUM'      .
    ZTPP_SEQ_BACKUP-WORDER = WA_MATERIAL       .
    ZTPP_SEQ_BACKUP-FNAME  = 'SEQQTY'          .
    ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_SEQQTY  .
** Changed by Furong on 10/29/08
    ZTPP_SEQ_BACKUP-CUSR = SY-UNAME.
    ZTPP_SEQ_BACKUP-CTIM = SY-UZEIT.
    ZTPP_SEQ_BACKUP-CDAT = SY-DATUM.
** End of change on 10/29/08

    MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.
    ZTPP_SEQ_BACKUP-FNAME  = 'PLANQTY'         .
    ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_PQTY    .
    MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.
    ZTPP_SEQ_BACKUP-FNAME  = 'FORECASTQTY'     .
    ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_FQTY    .
    MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.

    CLEAR: L_TSEQ, L_DATA-ATWRT, AUSP, ZTPP_SEQ_BACKUP.
    L_DATA-ATNAM = 'P_SEQ_QTY'.        APPEND L_DATA.
    L_DATA-ATNAM = 'P_PLAN_QTY'.       APPEND L_DATA.
    L_DATA-ATNAM = 'P_FORECAST_QTY'.   APPEND L_DATA.
    L_DATA-ATNAM = 'P_MITU_QTY'.       APPEND L_DATA.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT       = WA_MATERIAL
              CTYPE        = '001'
         TABLES
              VAL_TABLE    = L_DATA
         EXCEPTIONS
              NO_DATA      = 1
              ERROR_MODE   = 2
              ERROR_OBJECT = 3
              ERROR_VALUE  = 4
              OTHERS       = 5.

    READ TABLE L_DATA INDEX 1.
    L_SEQQTY   = L_DATA-ATWRT   .        CLEAR: L_DATA.
    READ TABLE L_DATA INDEX 2.
    L_PQTY     = L_DATA-ATWRT   .        CLEAR: L_DATA.
    READ TABLE L_DATA INDEX 3.
    L_FQTY     = L_DATA-ATWRT   .        CLEAR: L_DATA.
    READ TABLE L_DATA INDEX 4.
    L_MITUQTY  = L_DATA-ATWRT   .        CLEAR: L_DATA.

    ZTPP_SEQ_BACKUP-TABLES = 'WOCL'          .
    ZTPP_SEQ_BACKUP-WORDER = WA_MATERIAL     .
    ZTPP_SEQ_BACKUP-FNAME  = 'P_SEQ_QTY'       .
    ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_SEQQTY  .
** Changed by Furong on 10/29/08
    ZTPP_SEQ_BACKUP-CUSR = SY-UNAME.
    ZTPP_SEQ_BACKUP-CTIM = SY-UZEIT.
    ZTPP_SEQ_BACKUP-CDAT = SY-DATUM.
** End of change on 10/29/08

    MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.
    ZTPP_SEQ_BACKUP-FNAME  = 'P_PLAN_QTY'      .
    ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_PQTY    .
    MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.
    ZTPP_SEQ_BACKUP-FNAME  = 'P_FORECAST_QTY'  .
    ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_FQTY    .
    MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.
    ZTPP_SEQ_BACKUP-FNAME  = 'P_MITU_QTY'      .
    ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_MITUQTY .
    MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.

    " Sales Order Master Change...
    CLEAR: ZTPP_SEQ_BACKUP, L_QTY.
    ZTPP_SEQ_BACKUP-TABLES = 'VBAP'          .
    ZTPP_SEQ_BACKUP-WORDER = L_SALESORDER    .
    SELECT SINGLE KWMENG INTO L_QTY
      FROM VBAP
     WHERE VBELN = L_SALESORDER
       AND POSNR = '000010' .

    ZTPP_SEQ_BACKUP-FNAME  = '000010'        .
    ZTPP_SEQ_BACKUP-VALS   = L_QTY           .
** Changed by Furong on 10/29/08
    ZTPP_SEQ_BACKUP-CUSR = SY-UNAME.
    ZTPP_SEQ_BACKUP-CTIM = SY-UZEIT.
    ZTPP_SEQ_BACKUP-CDAT = SY-DATUM.
** End of change on 10/29/08

    MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.   CLEAR: L_QTY.

    SELECT SINGLE KWMENG INTO L_QTY
      FROM VBAP
     WHERE VBELN = L_SALESORDER
       AND POSNR = '000020' .

    ZTPP_SEQ_BACKUP-FNAME  = '000020'        .
    ZTPP_SEQ_BACKUP-VALS   = L_QTY           .
** Changed by Furong on 10/29/08
    ZTPP_SEQ_BACKUP-CUSR = SY-UNAME.
    ZTPP_SEQ_BACKUP-CTIM = SY-UZEIT.
    ZTPP_SEQ_BACKUP-CDAT = SY-DATUM.
** End of change on 10/29/08

    MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.   CLEAR: L_QTY.
  ENDIF.

  DATA: LT_7JB              LIKE TABLE OF IT_7JB  WITH HEADER LINE,
        L_MODL              LIKE IT_7JB-MODL,
        L_CNT               TYPE I.

  LT_7JB[] = IT_7JB[].
  SORT LT_7JB BY ORDR DIST EXTC INTC SSR1.
  DELETE ADJACENT DUPLICATES FROM LT_7JB COMPARING  ORDR DIST .

  READ TABLE LT_7JB INDEX 1.
  L_ORDR = LT_7JB-ORDR.  L_DIST = LT_7JB-DIST.

  LOOP AT LT_7JB .
    IF LT_7JB-ORDR = L_ORDR  AND  LT_7JB-DIST = L_DIST .
      CONTINUE.
    ELSE.
      CLEAR: ZTPP_SEQ_BACKUP, WA_MATERIAL,  L_DATA, L_DATA[].

      CONCATENATE L_ORDR       L_DIST         INTO WA_MATERIAL .

      L_DATA-ATNAM = 'P_SEQ_QTY'.        APPEND L_DATA.
      L_DATA-ATNAM = 'P_PLAN_QTY'.       APPEND L_DATA.
      L_DATA-ATNAM = 'P_FORECAST_QTY'.   APPEND L_DATA.
      L_DATA-ATNAM = 'P_MITU_QTY'.       APPEND L_DATA.
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                OBJECT       = WA_MATERIAL
                CTYPE        = '001'
           TABLES
                VAL_TABLE    = L_DATA
           EXCEPTIONS
                NO_DATA      = 1
                ERROR_MODE   = 2
                ERROR_OBJECT = 3
                ERROR_VALUE  = 4
                OTHERS       = 5.

      READ TABLE L_DATA INDEX 1.
      L_SEQQTY   = L_DATA-ATWRT   .        CLEAR: L_DATA.
      READ TABLE L_DATA INDEX 2.
      L_PQTY     = L_DATA-ATWRT   .        CLEAR: L_DATA.
      READ TABLE L_DATA INDEX 3.
      L_FQTY     = L_DATA-ATWRT   .        CLEAR: L_DATA.
      READ TABLE L_DATA INDEX 4.
      L_MITUQTY  = L_DATA-ATWRT   .        CLEAR: L_DATA.

      ZTPP_SEQ_BACKUP-TABLES = 'WOHD'          .
      ZTPP_SEQ_BACKUP-WORDER = WA_MATERIAL     .
      ZTPP_SEQ_BACKUP-FNAME  = 'P_SEQ_QTY'       .
      ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_SEQQTY  .
** Changed by Furong on 10/29/08
      ZTPP_SEQ_BACKUP-CUSR = SY-UNAME.
      ZTPP_SEQ_BACKUP-CTIM = SY-UZEIT.
      ZTPP_SEQ_BACKUP-CDAT = SY-DATUM.
** End of change on 10/29/08

      MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.
      ZTPP_SEQ_BACKUP-FNAME  = 'P_PLAN_QTY'      .
      ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_PQTY    .
      MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.
      ZTPP_SEQ_BACKUP-FNAME  = 'P_FORECAST_QTY'  .
      ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_FQTY    .
      MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.
      ZTPP_SEQ_BACKUP-FNAME  = 'P_MITU_QTY'      .
      ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_MITUQTY .
      MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.

      L_ORDR = LT_7JB-ORDR.  L_DIST = LT_7JB-DIST.
    ENDIF.
  ENDLOOP.

  IF WA_LINES > 0 .
    CLEAR: ZTPP_SEQ_BACKUP, WA_MATERIAL,  L_DATA, L_DATA[].

    CONCATENATE L_ORDR       L_DIST         INTO WA_MATERIAL .

    L_DATA-ATNAM = 'P_SEQ_QTY'.        APPEND L_DATA.
    L_DATA-ATNAM = 'P_PLAN_QTY'.       APPEND L_DATA.
    L_DATA-ATNAM = 'P_FORECAST_QTY'.   APPEND L_DATA.
    L_DATA-ATNAM = 'P_MITU_QTY'.       APPEND L_DATA.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT       = WA_MATERIAL
              CTYPE        = '001'
         TABLES
              VAL_TABLE    = L_DATA
         EXCEPTIONS
              NO_DATA      = 1
              ERROR_MODE   = 2
              ERROR_OBJECT = 3
              ERROR_VALUE  = 4
              OTHERS       = 5.

    READ TABLE L_DATA INDEX 1.
    L_SEQQTY   = L_DATA-ATWRT   .        CLEAR: L_DATA.
    READ TABLE L_DATA INDEX 2.
    L_PQTY     = L_DATA-ATWRT   .        CLEAR: L_DATA.
    READ TABLE L_DATA INDEX 3.
    L_FQTY     = L_DATA-ATWRT   .        CLEAR: L_DATA.
    READ TABLE L_DATA INDEX 4.
    L_MITUQTY  = L_DATA-ATWRT   .        CLEAR: L_DATA.

    ZTPP_SEQ_BACKUP-TABLES = 'WOHD'          .
    ZTPP_SEQ_BACKUP-WORDER = WA_MATERIAL     .
    ZTPP_SEQ_BACKUP-FNAME  = 'P_SEQ_QTY'       .
    ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_SEQQTY  .
** Changed by Furong on 10/29/08
    ZTPP_SEQ_BACKUP-CUSR = SY-UNAME.
    ZTPP_SEQ_BACKUP-CTIM = SY-UZEIT.
    ZTPP_SEQ_BACKUP-CDAT = SY-DATUM.
** End of change on 10/29/08

    MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.
    ZTPP_SEQ_BACKUP-FNAME  = 'P_PLAN_QTY'      .
    ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_PQTY    .
    MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.
    ZTPP_SEQ_BACKUP-FNAME  = 'P_FORECAST_QTY'  .
    ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_FQTY    .
    MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.
    ZTPP_SEQ_BACKUP-FNAME  = 'P_MITU_QTY'      .
    ZTPP_SEQ_BACKUP-VALS   = L_QTY = L_MITUQTY .
    MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.
  ENDIF.

  LT_7JB[] = IT_7JB[].
  SORT LT_7JB BY MODL .
  DELETE ADJACENT DUPLICATES FROM LT_7JB COMPARING MODL .
  C_PROG = 'Z_FPP_VIN_GENERATION'.

  LOOP AT LT_7JB .
    SELECT SINGLE *
      FROM ZTPP_COMMON_VALS
     WHERE JOBS = C_PROG
       AND KEY2 = LT_7JB-MODL .
*      AND key3 = '******************'.

    ZTPP_SEQ_BACKUP-TABLES = 'VIN_GEN'         .
    ZTPP_SEQ_BACKUP-WORDER = LT_7JB-MODL       .
    ZTPP_SEQ_BACKUP-FNAME  = 'ITEM4'           .
    ZTPP_SEQ_BACKUP-VALS   = ZTPP_COMMON_VALS-ITEM4+14(6).
** Changed by Furong on 10/29/08
    ZTPP_SEQ_BACKUP-CUSR = SY-UNAME.
    ZTPP_SEQ_BACKUP-CTIM = SY-UZEIT.
    ZTPP_SEQ_BACKUP-CDAT = SY-DATUM.
** End of change on 10/29/08

    MODIFY ZTPP_SEQ_BACKUP FROM ZTPP_SEQ_BACKUP.
  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            WAIT = 'X'.

ENDFORM.                    " BACK_UP_DATA1

*&---------------------------------------------------------------------*
*&      Form  EXIT_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXIT_MESSAGE.
  MESSAGE I001 WITH TEXT-900 TEXT-901." TEXT-902 TEXT-903 .
ENDFORM.                    " EXIT_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  workorder_mituquty_restoration
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MATERIAL  text
*      -->P_L_PLNMG  text
*----------------------------------------------------------------------*
FORM WORKORDER_MITUQUTY_RESTORATION USING    PA_MATERIAL
                                             PA_MITU .
  DATA: L_VARS                LIKE TABLE OF ZSPP_VIN_VALUE
                                                  WITH HEADER LINE,
        L_DEC1(6)             TYPE C.
  REFRESH L_VARS.
  CLEAR: L_VARS              .     L_DEC1 = PA_MITU     .
  L_VARS-ATNAM = 'P_MITU_QTY' .    L_VARS-ATWRT = L_DEC1. APPEND L_VARS.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = PA_MATERIAL
            MODE         = 'W'
            CTYPE        = '001'
       TABLES
            VAL_TABLE    = L_VARS
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  IF SY-SUBRC NE 0.
    WA_ERROR = 'X'.
  ENDIF.
ENDFORM.                    " workorder_mituquty_restoration
*&---------------------------------------------------------------------*
*&      Form  check_vehicle_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_7JB  text
*----------------------------------------------------------------------*
FORM CHECK_VEHICLE_INFO USING    P_7JB LIKE IT_7JB
                                 P_EQUNR.

  CLEAR: IT_VMASTER, IT_VMASTER[] .
  IT_VMASTER-ATNAM = 'P_WORK_ORDER' .
  APPEND IT_VMASTER.
  IT_VMASTER-ATNAM = 'P_EXT_COLOR' .
  APPEND IT_VMASTER.
  IT_VMASTER-ATNAM = 'P_INT_COLOR' .
  APPEND IT_VMASTER.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = P_EQUNR
       TABLES
            VAL_TABLE    = IT_VMASTER
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  CLEAR WA_MATERIAL.
  CONCATENATE IT_7JB-ORDR IT_7JB-DIST INTO WA_MATERIAL ."workorder h

  READ TABLE IT_VMASTER WITH KEY ATNAM = 'P_WORK_ORDER'
                                 ATWRT =  WA_MATERIAL.
  IF SY-SUBRC <> 0.
    MESSAGE I001 WITH TEXT-906 WA_MATERIAL.
  ENDIF.

  READ TABLE IT_VMASTER WITH KEY ATNAM = 'P_EXT_COLOR'
                                 ATWRT =  P_7JB-EXTC.
  IF SY-SUBRC <> 0.
    MESSAGE I001 WITH TEXT-906 P_7JB-EXTC.
  ENDIF.

  READ TABLE IT_VMASTER WITH KEY ATNAM = 'P_INT_COLOR'
                                 ATWRT =  P_7JB-INTC.
  IF SY-SUBRC <> 0.
    MESSAGE I001 WITH TEXT-906 P_7JB-INTC.
  ENDIF.

ENDFORM.                    " check_vehicle_info
*&---------------------------------------------------------------------*
*&      Form  change_plannedorder
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_7JB  text
*      -->P_L_PORDER  text
*----------------------------------------------------------------------*
FORM CHANGE_PLANNEDORDER USING    P_7JB LIKE IT_7JB
                                  P_PORDER.
  DATA :LS_HEADERDATA    LIKE  BAPIPLAF_I2,
        LS_HEADERDATAX   LIKE  BAPIPLAF_I2X.


  LS_HEADERDATA-ORDER_START_DATE  = P_7JB-SQDT.
  LS_HEADERDATA-ORDER_FIN_DATE    = P_7JB-SQDT.
  LS_HEADERDATA-PLAN_OPEN_DATE    = P_7JB-SQDT.
  LS_HEADERDATAX-ORDER_START_DATE = 'X'.
  LS_HEADERDATAX-ORDER_FIN_DATE   = 'X'.
  LS_HEADERDATAX-PLAN_OPEN_DATE   = 'X'.

  CALL FUNCTION 'BAPI_PLANNEDORDER_CHANGE'
       EXPORTING
            PLANNEDORDER = P_PORDER
            HEADERDATA   = LS_HEADERDATA
            HEADERDATAX  = LS_HEADERDATAX.


ENDFORM.                    " change_plannedorder
