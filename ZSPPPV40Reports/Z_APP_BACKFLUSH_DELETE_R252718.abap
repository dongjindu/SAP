************************************************************************
* Program Name      : Z_APP_BACKFLUSH_DELETE_R252718
* Author            : Won-seob Kim
* Creation Date     : 2003.12.30.
* Specifications By :
* Pattern           : Report 1-1
* Development Request No : UD1K904914
* Addl Documentation:
* Description       : BackFlush DELETE RESB & PLAF
*
* Modification Logs
* Date       Developer    RequestNo    Description
*11/16/2004  chris        UD1K913015   cancel the deletion logic of * *
*                                      table ZTPP_BFST.
*03/08/2005  chris        UD1K914825   backup the deleted records and
*                                      cancel the last time change
* 09/20/2006 Haseeb       UD1K922203    HD#66TI672577
*                                       Change the logic from deleting
*planorders which has passed RP18, to RP25/RP17 (V05/V07). delete from
*RESB, PLAF tables using the BAPI. after this program execution one can
*check the planned order non existance using MD13 transaction.
*
************************************************************************
*

INCLUDE Z_APP_DELETE_TOP_R252718.

INCLUDE Z_APP_DELETE_PARA_R252718.

INITIALIZATION.

START-OF-SELECTION.
  PERFORM GET_BF_OBJECTS.

END-OF-SELECTION.
  PERFORM WRITE_LIST.
*&---------------------------------------------------------------------*
*&      Form  get_bf_objects
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_BF_OBJECTS.

  PERFORM GET_DATA_DELETE.
*Delete ztpp_bfst :
  CHECK NOT P_D IS INITIAL.
  PERFORM DELETION_ZTPP_BFST.


ENDFORM.                    " get_bf_objects
*&---------------------------------------------------------------------*
*&      Form  write_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_LIST.
  DATA : W_INT2 TYPE I,W_INT3 TYPE I.
  CLEAR : W_INT,W_INT2,W_INT3.
*Deleted planned order
  DESCRIBE TABLE IT_RETURN LINES W_INT3.
  IF W_INT3 <> 0.
    WRITE : / '***************************************'.
    WRITE : / 'Deleted Planned order : ex) Spec change'.
    LOOP AT IT_RETURN.
      WRITE : / IT_RETURN-TYPE,IT_RETURN-MESSAGE,IT_RETURN-MESSAGE_V1  .
    ENDLOOP.
  ENDIF.
*Delete
  DESCRIBE TABLE IT_DELETE LINES W_INT.
  IF W_INT <> 0.
    WRITE : / '***************************************'.
    WRITE : / 'Deleted Planned order : ex)Duration'.

    LOOP AT IT_DELETE.
      WRITE : / 'Deleted planned order number: ', IT_DELETE-PLNUM,
                'PLAF Count :', IT_DELETE-PLAF_COUNT,
                'RESB Count :', IT_DELETE-RESB_COUNT.
    ENDLOOP.
  ENDIF.

  CHECK NOT P_D IS INITIAL.
*Missing  Plan order
  DESCRIBE TABLE IT_DELETE_S LINES W_INT2.
  IF W_INT2 <> 0.
    WRITE : / '***************************************'.
    WRITE : / 'Missing Planned order '.

    WRITE : / 'ZTPP_BFST', ' :' , W_INT2.
    LOOP AT IT_DELETE_S.
      WRITE : / IT_DELETE_S-PLAN_ORD.
    ENDLOOP.
  ENDIF.

* BACKUPED TOTAL ENTRIES
  SKIP.
  IF W_BACKUP NE 0.
    WRITE:/ 'Total Backup Entries :',W_BACKUP.
  ENDIF.
ENDFORM.                    " write_list
*&---------------------------------------------------------------------*
*&      Form  get_data_delete
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_DELETE.
  CLEAR : IT_BFST[],IT_BFST,IT_RETURN[].


  DATA :  WA_DATE LIKE SY-DATUM.
  DATA : WA_DAY LIKE T5A4A-DLYDY.
  CLEAR WA_DAY.
  MOVE P_RTIME TO WA_DAY.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            DATE      = SY-DATUM
            DAYS      = WA_DAY
            MONTHS    = '00'
            SIGNUM    = '-'
            YEARS     = '00'
       IMPORTING
            CALC_DATE = WA_DATE.

  IF R_SP EQ 'X'.
    SELECT * INTO TABLE IT_BFST FROM ZTPP_BFST
          WHERE PLAN_ORD IN S_PLNUM
            AND PLANT EQ P_PLANT
            AND PLAN_DEL_FLG EQ 'X'.

  ELSEIF R_SI EQ 'X'.
    SELECT * INTO TABLE IT_BFST FROM ZTPP_BFST
          WHERE PLAN_ORD IN S_PLNUM
            AND PLANT EQ P_PLANT
            AND SD_DELI_FLG EQ 'Y'.

  ENDIF.


*  CALL FUNCTION 'MONTH_PLUS_DETERMINE'
*       EXPORTING
*            MONTHS  = -1
*            OLDDATE = SY-DATUM
*       IMPORTING
*            NEWDATE = WA_DATE.
*  IF R_SP EQ 'X'.
*    SELECT * INTO TABLE IT_BFST FROM ZTPP_BFST
*          WHERE PLANT EQ P_PLANT
*            AND PLAN_ORD IN S_PLNUM
*            AND PLAN_DEL_FLG EQ 'X'.
*
*  ELSEIF R_SI EQ 'X'.
*    SELECT * INTO TABLE IT_BFST FROM ZTPP_BFST
*          WHERE PLANT EQ P_PLANT
*            AND PLAN_ORD IN S_PLNUM
*            AND FIN_BF_FLG EQ 'Y' AND BFP18_DAT LE WA_DATE.
*  ENDIF.

  DESCRIBE TABLE IT_BFST LINES W_INT.
  IF W_INT <> 0.
    PERFORM GATHERING_COMPONENTS TABLES IT_BFST
                                 USING R_SP.
  ELSE.
    MESSAGE I004 ."WITH text-002.    "i004.
    EXIT.
  ENDIF.

* BACKUP THE DELETED ENTRIES--ADDED BY CHRIS
  PERFORM SAVE_DELETED_ENTRIES.

ENDFORM.                    " get_data_delete
*&---------------------------------------------------------------------*
*&      Form  gathering_components
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BFST  text
*----------------------------------------------------------------------*
FORM GATHERING_COMPONENTS TABLES LP_BFST STRUCTURE IT_BFST
                          USING PR_SP.

**HASEEB COMMENTED   UD1K922203

*  IF PR_SP EQ 'X'.
*    PERFORM CHECK_DELETED_PLANNEDORDER TABLES LP_BFST .
*  ELSE.
* Control comes here always for second option.



  PERFORM CHECK_DATE TABLES LP_BFST .
*  ENDIF.
ENDFORM.                    " gathering_components
*&---------------------------------------------------------------------*
*&      Form  DELETION_PLAF_RESB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LP_BFST  text
*----------------------------------------------------------------------*
FORM DELETION_PLAF_RESB using LPP_BFST structure IT_BFST.
  DATA : Z_FIELD(18) , NUM(2) TYPE N,P_FLG,P_SP_FLG.
  FIELD-SYMBOLS : <FIELD> TYPE ANY,
                  <DATE> TYPE ANY,
                  <TIME> TYPE ANY.
  CLEAR : NUM,P_FLG,P_SP_FLG.

**The below logic shows the deletion of planned order from table
*ZPP_BFST table after RP18. so commented this logic to implement the new
* one to delete RP25/RP17.
**Issue PP-20040729-002

*Delete marked Planorder delete
*-----start
  IF LPP_BFST-PLAN_DEL_FLG EQ 'X'.
    NUM = '01'.  CLEAR : P_SP_FLG.
    DO 18 TIMES.
      CONCATENATE 'LPP_BFST' '-' 'BFP' NUM '_FLG' INTO Z_FIELD.
      ASSIGN (Z_FIELD) TO <FIELD>.
*No '09' status on any backflush point
      IF LPP_BFST-PLAN_DEL_FLG EQ 'X' AND <FIELD> <> '00'.
        EXIT.
      ENDIF.
      IF NUM = 18.
        P_SP_FLG  = 'X'.
      ENDIF.
      NUM = NUM + 1.
    ENDDO.
*-----end
  ELSE.
    NUM = '01'.
    DO 18 TIMES.
      CONCATENATE 'LPP_BFST' '-' 'BFP' NUM '_FLG' INTO Z_FIELD.
      ASSIGN (Z_FIELD) TO <FIELD>.
*No '09' status on any backflush point
      IF LPP_BFST-PLAN_DEL_FLG EQ 'X' AND <FIELD> EQ '09'.
        EXIT.
      ENDIF.
*Final Backflush is 'Y', and Final point date is less than the
*selection date and NO '01' or '09' on any backflush point and
*SD delivery flag is 'Y'
     IF  LPP_BFST-FIN_BF_FLG EQ 'Y' AND LPP_BFST-SD_DELI_FLG EQ 'Y' AND
                                ( <FIELD> EQ '09' OR  <FIELD> EQ '01' )
 .
        EXIT.
      ENDIF.
      IF NUM = 18.
        P_FLG = 'X'.
      ENDIF.
      NUM = NUM + 1.
    ENDDO.
  ENDIF.
*Issue PP-20040729-002
*Delete marked Planorder delete
*-----start
*  IF P_SP_FLG EQ 'X'.
*    PERFORM CALL_DELETE_BAPI USING LPP_BFST
*                                   LPP_BFST-PLAN_ORD.
*  ENDIF.
**-----end
*  IF P_FLG EQ 'X'.

  PERFORM CALL_DELETE_FUNCTION USING LPP_BFST
                                     LPP_BFST-PLAN_ORD.
*  ENDIF.

ENDFORM.                    " DELETION_PLAF_RESB
*&---------------------------------------------------------------------*
*&      Form  SERCH_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_RTIME  text
*----------------------------------------------------------------------*
FORM SEARCH_DAY USING    PP_RTIME
               CHANGING TO_DATE.

  DATA : P_DAY LIKE T5A4A-DLYDY.
  CLEAR P_DAY.
  MOVE PP_RTIME TO P_DAY.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            DATE      = SY-DATUM
            DAYS      = P_DAY
            MONTHS    = '00'
            SIGNUM    = '-'
            YEARS     = '00'
       IMPORTING
            CALC_DATE = BE_DATE.
*check holiday
  PERFORM GET_HOILIDAY USING BE_DATE
                       CHANGING TO_DATE.

ENDFORM.                    " SERCH_DAY
*&---------------------------------------------------------------------*
*&      Form  CALL_DELETE_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0152   text
*----------------------------------------------------------------------*
FORM CALL_DELETE_FUNCTION USING LT_BFST STRUCTURE IT_BFST
                                LS_PLNUM.
  CLEAR : PLAF_COUNT,RESB_COUNT.

*****COMMENTED BY HASEEB.  UD1K922203
*  IF P_E IS INITIAL. "Check RESB
*    PERFORM CALL_DELETE_FUNCTION_PLAN USING LT_BFST
*                                            LS_PLNUM.
*
*  ELSE.
  PERFORM CHECK_RESB_TABLE USING LT_BFST LS_PLNUM.
  PERFORM CALL_DELETE_FUNCTION_PLAN USING LT_BFST
                                          LS_PLNUM.

*  ENDIF.
ENDFORM.                    " CALL_DELETE_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  DELETION_ZTPP_BFST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETION_ZTPP_BFST.
  REFRESH : IT_DELETE_C,IT_DELETE_S.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DELETE_C
          FROM ZTPP_BFST   .

  DESCRIBE TABLE IT_DELETE_C LINES W_INT.
  IF W_INT <> 0.
    LOOP AT IT_DELETE_C.
      SELECT SINGLE  * FROM PLAF WHERE PLNUM EQ IT_DELETE_C-PLAN_ORD.
      CHECK SY-SUBRC <> 0.
      DELETE ZTPP_BFST FROM IT_DELETE_C.
*     requested by catherine s. changed by chris
*     backup the deleted records for future reference
      APPEND IT_DELETE_C TO IT_BFST_BK.
*     end of change on 03/09/2005
      MOVE-CORRESPONDING IT_DELETE_C TO IT_DELETE_S.
      APPEND IT_DELETE_S.
    ENDLOOP.
*   REQUESTED BY CATHERINE S. CHANGED BY CHRIS
*   SAVE THE DELETED ENTIES
    PERFORM SAVE_DELETED_ENTRIES.
*   END OF CHANGE ON 03/09/2005

*
  ENDIF.
ENDFORM.                    " DELETION_ZTPP_BFST
*&---------------------------------------------------------------------*
*&      Form  CALL_DELETE_FUNCTION_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_BFST  text
*      -->P_LS_PLNUM  text
*----------------------------------------------------------------------*
FORM CALL_DELETE_FUNCTION_PLAN USING    P_LT_BFST
                                        P_LS_PLNUM.
  CALL FUNCTION 'ZPPC1TP_ORDERS_DELETE_PL'
       EXPORTING
            IF_RESTIME    = P_RTIME
            IF_DELCONF    = 'X'
            IF_COMMIT     = 'X'
            S_LOW         = P_LS_PLNUM
       IMPORTING
            EF_PLAF_COUNT = PLAF_COUNT
            EF_RESB_COUNT = RESB_COUNT
       EXCEPTIONS
            UPDATE_ERROR  = 1
            OTHERS        = 2.

  IF SY-SUBRC <> 0.
** Furong on 04/02/12
   message s003 with sy-msgv1 '. Plan order:'
                     P_LS_PLNUM.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
** end
  ELSE.
    IF NOT PLAF_COUNT IS INITIAL.
      DELETE ZTPP_BFST FROM P_LT_BFST.
*     requested by catherine s. changed by chris
*     backup the deleted records for future reference
      APPEND P_LT_BFST TO IT_BFST_BK.
*     end of change on 03/09/2005

      COMMIT WORK.
      MOVE : P_LS_PLNUM    TO IT_DELETE-PLNUM,
             PLAF_COUNT  TO IT_DELETE-PLAF_COUNT,
             RESB_COUNT  TO IT_DELETE-RESB_COUNT.
      APPEND IT_DELETE.
    ENDIF.
  ENDIF.
ENDFORM.                    " CALL_DELETE_FUNCTION_PLAN
*&---------------------------------------------------------------------*
*&      Form  CHECK_RESB_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_BFST  text
*      -->P_LS_PLNUM  text
*----------------------------------------------------------------------*
FORM CHECK_RESB_TABLE USING    P_LT_BFST
                               P_LS_PLNUM.
  DATA : IT_RESB LIKE RESB OCCURS 0 WITH HEADER LINE,
         Z_ENMNG LIKE RESB-ENMNG.
  CLEAR : W_INT,Z_ENMNG,IT_RESB[].

  SELECT RSNUM RSPOS RSART BDMNG ERFMG ENMNG SORTF
        INTO CORRESPONDING FIELDS OF TABLE IT_RESB
             FROM RESB
                 WHERE WERKS EQ P_PLANT
                 AND PLNUM EQ P_LS_PLNUM.

  DESCRIBE TABLE IT_RESB LINES W_INT.
  CHECK NOT W_INT IS INITIAL.
  LOOP AT IT_RESB WHERE ENMNG EQ SPACE  " Quantity withdrawn
                    AND SORTF NE SPACE. " Sort string
    MOVE IT_RESB-BDMNG TO Z_ENMNG.

    UPDATE RESB SET ENMNG = Z_ENMNG
       WHERE RSNUM EQ  IT_RESB-RSNUM
         AND RSPOS EQ  IT_RESB-RSPOS
         AND RSART EQ  IT_RESB-RSART.
    CLEAR Z_ENMNG.
  ENDLOOP.
  COMMIT WORK.
ENDFORM.                    " CHECK_RESB_TABLE
*&---------------------------------------------------------------------*
*&      Form  get_hoiliday
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DAY  text
*----------------------------------------------------------------------*
FORM GET_HOILIDAY USING  BE_DATE
                  CHANGING TO_DATE.

  PERFORM GET_SHOP_RP_INFO.
  PERFORM GET_CALENDAR_ID.
  PERFORM GET_WORK_DAY USING BE_DATE
                       CHANGING TO_DATE.

ENDFORM.                    " get_hoiliday
*&---------------------------------------------------------------------*
*&      Form  get_shop_rp_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SHOP_RP_INFO.
*----- Read Shop<->RP

  SELECT SINGLE SHOPNAME INTO W_ARBPL
    FROM ZTPP_STATUS
      WHERE RP_POINT = W_RP_POINT.
  IF SY-SUBRC NE 0.
    RAISE ETC_EXCEPTION.
  ENDIF.

ENDFORM.                    " get_shop_rp_info
*&---------------------------------------------------------------------*
*&      Form  get_calendar_id
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_CALENDAR_ID.
*----- Read Shop Calendar ID
  SELECT SINGLE C~KALID C~MOSID B~KAPID
    INTO (W_KALID, W_MOSID, W_KAPID)
    FROM CRHD AS A INNER JOIN CRCA AS B
                      ON A~OBJTY = B~OBJTY
                     AND A~OBJID = B~OBJID
                   INNER JOIN KAKO AS C
                      ON B~KAPID = C~KAPID
   WHERE A~WERKS =  W_WERKS
     AND A~ARBPL =  W_ARBPL
     AND B~FORK2 =  'SAP006'.

ENDFORM.                    " get_calendar_id
*&---------------------------------------------------------------------*
*&      Form  get_work_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_WORK_DAY USING BE_DATE
                  CHANGING TO_DATE.
  DATA: LW_DATE  TYPE D,
        LW_LINES TYPE I,
        LW_DAYNR LIKE HRVSCHED-DAYNR,
        LW_DAYFREE LIKE HRVSCHED-NODAY.

  LW_DATE = BE_DATE.

  DO.
    CALL FUNCTION 'RH_GET_DATE_DAYNAME'
         EXPORTING
              LANGU               = SY-LANGU
              DATE                = LW_DATE
              CALID               = W_KALID
         IMPORTING
              DAYNR               = LW_DAYNR
              DAYFREE             = LW_DAYFREE
         EXCEPTIONS
              NO_LANGU            = 1
              NO_DATE             = 2
              NO_DAYTXT_FOR_LANGU = 3
              INVALID_DATE        = 4
              OTHERS              = 5.
    IF SY-SUBRC <> 0.
      RAISE ETC_EXCEPTION.
    ENDIF.

    IF LW_DAYFREE EQ 'X'.
      LW_DATE = LW_DATE - 1.
      CONTINUE.
    ELSE.
      TO_DATE = LW_DATE .
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " get_work_day
*&---------------------------------------------------------------------*
*&      Form  call_delete_bapi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LPP_BFST  text
*      -->P_LPP_BFST_PLAN_ORD  text
*----------------------------------------------------------------------*
FORM CALL_DELETE_BAPI USING LT_BFST STRUCTURE IT_BFST
                                LS_PLNUM.
  DATA : RETURN LIKE BAPIRETURN1.
  CLEAR RETURN.
  CALL FUNCTION 'BAPI_PLANNEDORDER_DELETE'
       EXPORTING
            PLANNEDORDER = LS_PLNUM
*   USE_COLL_UPDATE       = ' '
*   LAST_ORDER            = ' '
       IMPORTING
            RETURN       = RETURN.

  IF SY-SUBRC = 0.
    MOVE-CORRESPONDING  RETURN TO IT_RETURN.
    APPEND IT_RETURN.

    DELETE ZTPP_BFST FROM LT_BFST.
*   requested by catherine s. changed by chris
*   backup the deleted records for future reference
    APPEND LT_BFST TO IT_BFST_BK.
*   end of change on 03/09/2005

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

  ENDIF.
ENDFORM.                    " call_delete_bapi
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LP_BFST  text
*----------------------------------------------------------------------*
FORM CHECK_DATE TABLES LP_BFST STRUCTURE IT_BFST.
  DATA : Z_TOT TYPE I,
         Z_NUM TYPE I.
  DATA : BEGIN OF IT_AUSPFINAL OCCURS 0,
           OBJEK LIKE AUSP-OBJEK,
           PLAN_OR LIKE  ZTPP_BFST-PLAN_ORD,
         END OF IT_AUSPFINAL.

  DATA : ZRP25 like AUSP-ATINN.
  DATA : ZRP27 like AUSP-ATINN.

  CLEAR : IT_DELETE[],LP_BFST,Z_TOT,Z_NUM,W_INT.


*search hoiliday
  PERFORM SEARCH_DAY USING P_25TIME
                    CHANGING TO_DATE.

  DESCRIBE TABLE LP_BFST LINES Z_TOT.

*** BELOW is the logic to delete the Planned order which are 7 days and
*above older. here are the steps,## UD1K922203.  HD#66TI672577
*    1. Read plan order from ZTPP_BFST table with SD_DELI_FLG = 'Y'
*     2. get object from ASUP using step 1
*     3. get P_STATUS = 'RP25/RP27/V05/V07' and P_RP25_ACTUAL_DATE,
*P_RP27_ACTUAL_DATE from AUSP using step 2.
*     4. Compare the P_RP*_ACTUAL_DATE to see if it is 7 days or older ,
* if older delete it using Planned order delete BAPI, which deletes
*entries from RESB and PLAF.


  LOOP AT IT_BFST.
    IT_ATWRT-PLN_ORD = IT_BFST-PLAN_ORD.
*    CONCATENATE IT_BFST-MODEL IT_BFST-BODY_SER INTO IT_ATWRT-OBJEK.
    APPEND IT_ATWRT.
  ENDLOOP.

  PERFORM make_atnam USING 'P_PLAN_ORDER'.
  REFRESH : it_cabn,r_atinn.
  CLEAR   : it_cabn,r_atinn.
* find the internal characteristic number from CABN table.
  SELECT atinn atnam INTO TABLE it_cabn
     FROM cabn
     WHERE atnam IN r_atnam.

  LOOP AT it_cabn.
    r_atinn-sign = 'I'.
    r_atinn-option = 'EQ'.
    r_atinn-low = it_cabn-atinn.
    APPEND r_atinn.
  ENDLOOP.


  IF NOT it_atwrt[] IS INITIAL.

    SELECT OBJEK ATINN ATWRT INTO table IT_AUSP FROM AUSP
      for all entries in it_atwrt
     WHERE KLART = '002' AND ATINN IN r_atinn and
       ATWRT eq  it_atwrt-PLN_ORD.
* REFRESH : it_cabn,r_atinn.
*  CLEAR   : it_cabn,r_atinn.
*  it_cabn = ''.
*  r_atinn = ''.
*

*  PERFORM make_atnam USING 'P_STATUS'.
*  PERFORM make_atnam USING 'P_RP25_ACTUAL_DATE'.
*  PERFORM make_atnam USING 'P_RP27_ACTUAL_DATE'.
*
*
*  SELECT atinn atnam INTO TABLE it_cabn
*       FROM cabn
*       WHERE atnam IN r_atnam.
*
*  LOOP AT it_cabn.
*    r_atinn-sign = 'I'.
*    r_atinn-option = 'EQ'.
*    r_atinn-low = it_cabn-atinn.
*    APPEND r_atinn.
*  ENDLOOP.


* SELECT OBJEK ATINN ATWRT INTO table IT_AUSPSTATUS FROM AUSP
*       FOR ALL ENTRIES IN IT_ATWRT
*       WHERE  OBJEK EQ IT_ATWRT-OBJEK AND KLART = '002'
*              AND ATINN IN R_ATINN.

  ENDIF.

  REFRESH : it_cabn,r_atinn.
  CLEAR   : it_cabn,r_atinn.
  it_cabn = ''.
  r_atinn = ''.


  PERFORM make_atnam USING 'P_STATUS'.
  PERFORM make_atnam USING 'P_RP25_ACTUAL_DATE'.
  PERFORM make_atnam USING 'P_RP27_ACTUAL_DATE'.


  SELECT atinn atnam INTO TABLE it_cabn
       FROM cabn
       WHERE atnam IN r_atnam.

  LOOP AT it_cabn.
    r_atinn-sign = 'I'.
    r_atinn-option = 'EQ'.
    r_atinn-low = it_cabn-atinn.
    APPEND r_atinn.
  ENDLOOP.

  IF NOT it_ausp[] IS INITIAL.

    SELECT OBJEK ATINN ATWRT INTO table IT_AUSPSTATUS FROM AUSP
      for all entries in it_AUSP
     WHERE KLART = '002' AND ATINN IN r_atinn
       AND OBJEK eq  IT_AUSP-OBJEK.
  ENDIF.

  SORT IT_AUSP by OBJEK.
  CLEAR IT_AUSPFINAL.
  REFRESH IT_AUSPFINAL.
*  SORT IT_AUSPSTATUS BY OBJEK.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = 'P_RP25_ACTUAL_DATE'
       IMPORTING
            OUTPUT = ZRP25.


  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = 'P_RP27_ACTUAL_DATE'
       IMPORTING
            OUTPUT = ZRP27.

*IF NOT it_auspstatus[] IS INITIAL.
*    LOOP AT it_auspstatus.
*
*      CHECK it_auspstatus-atwrt = 'V05' or it_auspstatus-atwrt = 'V07'
*or it_auspstatus-atwrt = 'T25' or it_auspstatus-atwrt = 'T27'.
*      CASE it_auspstatus-atwrt.
*        WHEN  'V05' or 'T25'.
*          READ table it_auspstatus into wa_auspdate with  KEY
*          OBJEK = it_auspstatus-objek  ATINN = ZRP25. " 0000003425.
*          if  sy-subrc eq 0 and wa_auspdate-ATWRT+0(8) <= TO_DATE.
*            READ TABLE IT_AUSPSTATUS with Key OBJEK  =
*wa_auspdate-objek
*BINARY SEARCH.
*            if sy-subrc eq 0.
*              it_auspfinal-objek = it_auspSTATUS-objek.
*              it_auspfinal-plan_or = it_auspSTATUS-atwrt.
*              APPEND IT_AUSPFINAL.
*            endif.
*          endif.
*        WHEN  'V07' or 'T27'.
*          READ table it_auspstatus into wa_auspdate with KEY
*          OBJEK = it_auspstatus-objek  ATINN = ZRP27. " 0000003424.
*          if sy-subrc eq 0 and wa_auspdate-ATWRT+0(8) <= TO_DATE.
*            READ TABLE IT_AUSPSTATUS with Key OBJEK  =
*wa_auspdate-objek
*BINARY SEARCH.
*            if sy-subrc eq 0.
*              it_auspfinal-objek = it_auspSTATUS-objek.
*              it_auspfinal-plan_or = it_auspSTATUS-atwrt.
*              APPEND IT_AUSPFINAL.
*            endif.
*          endif.
*      ENDCASE.
*      CLEAR IT_AUSPFINAL.
*    ENDLOOP.

  IF NOT it_auspstatus[] IS INITIAL.
    LOOP AT it_auspstatus.

      CHECK it_auspstatus-atwrt = 'V05' or it_auspstatus-atwrt = 'V07'
or it_auspstatus-atwrt = 'T25' or it_auspstatus-atwrt = 'T27'.
      CASE it_auspstatus-atwrt.
        WHEN  'V05' or 'T25'.
          READ table it_auspstatus into wa_auspdate with  KEY
          OBJEK = it_auspstatus-objek  ATINN = ZRP25. " 0000003425.
          if  sy-subrc eq 0 and wa_auspdate-ATWRT+0(8) <= TO_DATE.
            READ TABLE IT_AUSP with Key OBJEK  = wa_auspdate-objek
BINARY SEARCH.
            if sy-subrc eq 0.
              it_auspfinal-objek = it_ausp-objek.
              it_auspfinal-plan_or = it_ausp-atwrt.
              APPEND IT_AUSPFINAL.
            endif.
          endif.
        WHEN  'V07' or 'T27'.
          READ table it_auspstatus into wa_auspdate with KEY
          OBJEK = it_auspstatus-objek  ATINN = ZRP27. " 0000003424.
          if sy-subrc eq 0 and wa_auspdate-ATWRT+0(8) <= TO_DATE.
            READ TABLE IT_AUSP with Key OBJEK  = wa_auspdate-objek
BINARY SEARCH.
            if sy-subrc eq 0.
              it_auspfinal-objek = it_ausp-objek.
              it_auspfinal-plan_or = it_ausp-atwrt.
              APPEND IT_AUSPFINAL.
            endif.
          endif.
      ENDCASE.
      CLEAR IT_AUSPFINAL.
    ENDLOOP.

  ENDIF.

  SORT IT_BFST by PLAN_ORD.
  LOOP AT IT_AUSPFINAL.
    READ TABLE IT_BFST with KEY PLAN_ORD = IT_AUSPFINAL-plan_or BINARY
  SEARCH.
    if sy-subrc eq 0.
      MOVE-CORRESPONDING IT_BFST TO WA_BFST.
       APPEND WA_BFST.
       Clear wa_bfst.
    endif.
  ENDLOOP.

  LOOP AT WA_BFST.
    PERFORM DELETION_PLAF_RESB USING WA_BFST .
  ENDLOOP.

********END CODE OF HASEEB.


*  LOOP AT LP_BFST.
*    IF LP_BFST-PLAN_DEL_DAT LT TO_DATE OR
*       LP_BFST-BFP18_DAT LT TO_DATE.
*      Z_NUM = ( SY-TABIX / Z_TOT ) * 100.
*      WK_GUI_MSG(3) = Z_NUM.
*      WK_GUI_MSG+3 = '%'.
*      WK_GUI_MSG+5(30) = 'Delete Planned order'.
*
*      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*           EXPORTING
*                PERCENTAGE = SY-INDEX
*                TEXT       = WK_GUI_MSG.
*
*      PERFORM DELETION_PLAF_RESB USING LP_BFST .
*    ENDIF.
*    CLEAR LP_BFST.
*  ENDLOOP.
ENDFORM.                    " CHECK_DATE
*&---------------------------------------------------------------------*
*&      Form  check_deleted_plannedorder
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LP_BFST  text
*----------------------------------------------------------------------*
FORM CHECK_DELETED_PLANNEDORDER TABLES   DP_BFST STRUCTURE  IT_BFST.
  LOOP AT DP_BFST.
    PERFORM DELETION_PLAF_RESB USING DP_BFST .
  ENDLOOP.
ENDFORM.                    " check_deleted_plannedorder
*&---------------------------------------------------------------------*
*&      Form  SAVE_DELETED_ENTRIES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DELETED_ENTRIES.

  SORT IT_BFST_BK BY PLANT MODEL BODY_SER PLAN_ORD.
  DELETE ADJACENT DUPLICATES FROM IT_BFST_BK
     COMPARING PLANT MODEL BODY_SER PLAN_ORD.

  DESCRIBE TABLE IT_BFST_BK LINES W_BACKUP.

  IF W_BACKUP NE 0.
    MODIFY ZTPP_BFST_BK FROM TABLE IT_BFST_BK.
    IF SY-SUBRC NE 0.
      MESSAGE I000 WITH 'BACKUP OF DELETED ENTRIES FAILED!'.
    ELSE.
      COMMIT WORK .
      IF SY-SUBRC EQ 0.
        CLEAR: IT_BFST_BK, IT_BFST_BK[].
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " SAVE_DELETED_ENTRIES
*&---------------------------------------------------------------------*
*&      Form  MAKE_atnam  BY HASEEB.  UD1K922203   HD#66TI672577
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM MAKE_atnam USING  p_atnam.
  r_atnam-sign = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low = p_atnam.
  APPEND r_atnam.
endform.                   "MAKE_ATNAM END.
