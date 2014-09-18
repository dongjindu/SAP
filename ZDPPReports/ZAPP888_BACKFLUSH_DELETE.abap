************************************************************************
* Program Name      : ZAPP888_BACKFLUSH_DELETE
* Author            :
* Creation Date     : 12/04/2006
* Specifications By :
* Pattern           : Report 1-1
* Development Request No :
* Addl Documentation:
* Description       : BackFlush DELETE RESB & PLAF
*
* Modification Logs : COopy from ZAPP718_BACKFLUSH_DELETE
* for keeping planned order not deleted if not shipped out
* (rp25, rp27)
* Date       Developer    RequestNo    Description
*
*
************************************************************************
*

INCLUDE z_app_delete_top_r252718.

INCLUDE z_app_delete_para_r252718.

INITIALIZATION.

START-OF-SELECTION.
  PERFORM get_bf_objects.

END-OF-SELECTION.
  PERFORM write_list.
*&---------------------------------------------------------------------*
*&      Form  get_bf_objects
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bf_objects.

  PERFORM get_data_delete.
*Delete ztpp_bfst :
  CHECK NOT p_d IS INITIAL.
  PERFORM deletion_ztpp_bfst.


ENDFORM.                    " get_bf_objects
*&---------------------------------------------------------------------*
*&      Form  write_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_list.
  DATA : w_int2 TYPE i,w_int3 TYPE i.
  CLEAR : w_int,w_int2,w_int3.
*Deleted planned order
  DESCRIBE TABLE it_return LINES w_int3.
  IF w_int3 <> 0.
    WRITE : / '***************************************'.
    WRITE : / 'Deleted Planned order : ex) Spec change'.
    LOOP AT it_return.
      WRITE : / it_return-type,it_return-message,it_return-message_v1  .
    ENDLOOP.
  ENDIF.
*Delete
  DESCRIBE TABLE it_delete LINES w_int.
  IF w_int <> 0.
    WRITE : / '***************************************'.
    WRITE : / 'Deleted Planned order : ex)Duration'.

    LOOP AT it_delete.
      WRITE : / 'Deleted planned order number: ', it_delete-plnum,
                'PLAF Count :', it_delete-plaf_count,
                'RESB Count :', it_delete-resb_count.
    ENDLOOP.
  ENDIF.

  CHECK NOT p_d IS INITIAL.
*Missing  Plan order
  DESCRIBE TABLE it_delete_s LINES w_int2.
  IF w_int2 <> 0.
    WRITE : / '***************************************'.
    WRITE : / 'Missing Planned order '.

    WRITE : / 'ZTPP_BFST', ' :' , w_int2.
    LOOP AT it_delete_s.
      WRITE : / it_delete_s-plan_ord.
    ENDLOOP.
  ENDIF.

* BACKUPED TOTAL ENTRIES
  SKIP.
  IF w_backup NE 0.
    WRITE:/ 'Total Backup Entries :',w_backup.
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
FORM get_data_delete.
  CLEAR : it_bfst[],it_bfst,it_return[].


  DATA :  wa_date LIKE sy-datum.
  DATA : wa_day LIKE t5a4a-dlydy.
  CLEAR wa_day.
  MOVE p_rtime TO wa_day.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            date      = sy-datum
            days      = wa_day
            months    = '00'
            signum    = '-'
            years     = '00'
       IMPORTING
            calc_date = wa_date.

  IF r_sp EQ 'X'.
    SELECT * INTO TABLE it_bfst FROM ztpp_bfst
          WHERE plant EQ p_plant
            AND plan_ord IN s_plnum
            AND plan_del_flg EQ 'X'.

  ELSEIF r_si EQ 'X'.
    SELECT * INTO TABLE it_bfst FROM ztpp_bfst
          WHERE plant EQ p_plant
            AND plan_ord IN s_plnum
            AND sd_deli_flg EQ 'Y'.

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

  DESCRIBE TABLE it_bfst LINES w_int.
  IF w_int <> 0.
    PERFORM gathering_components TABLES it_bfst
                                 USING r_sp.
  ELSE.
    MESSAGE i004 ."WITH text-002.    "i004.
    EXIT.
  ENDIF.

* BACKUP THE DELETED ENTRIES--ADDED BY CHRIS
  PERFORM save_deleted_entries.

ENDFORM.                    " get_data_delete
*&---------------------------------------------------------------------*
*&      Form  gathering_components
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BFST  text
*----------------------------------------------------------------------*
FORM gathering_components TABLES lp_bfst STRUCTURE it_bfst
                          USING pr_sp.

**HASEEB COMMENTED   UD1K922203

*  IF PR_SP EQ 'X'.
*    PERFORM CHECK_DELETED_PLANNEDORDER TABLES LP_BFST .
*  ELSE.
* Control comes here always for second option.



  PERFORM check_date TABLES lp_bfst .
*  ENDIF.
ENDFORM.                    " gathering_components
*&---------------------------------------------------------------------*
*&      Form  DELETION_PLAF_RESB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LP_BFST  text
*----------------------------------------------------------------------*
FORM deletion_plaf_resb USING lpp_bfst STRUCTURE it_bfst.
  DATA : z_field(18) , num(2) TYPE n,p_flg,p_sp_flg.
  FIELD-SYMBOLS : <field> TYPE ANY,
                  <date> TYPE ANY,
                  <time> TYPE ANY.
  CLEAR : num,p_flg,p_sp_flg.

**The below logic shows the deletion of planned order from table
*ZPP_BFST table after RP18. so commented this logic to implement the new
* one to delete RP25/RP17.
**Issue PP-20040729-002

*Delete marked Planorder delete
*-----start
  IF lpp_bfst-plan_del_flg EQ 'X'.
    num = '01'.  CLEAR : p_sp_flg.
    DO 18 TIMES.
      CONCATENATE 'LPP_BFST' '-' 'BFP' num '_FLG' INTO z_field.
      ASSIGN (z_field) TO <field>.
*No '09' status on any backflush point
      IF lpp_bfst-plan_del_flg EQ 'X' AND <field> <> '00'.
        EXIT.
      ENDIF.
      IF num = 18.
        p_sp_flg  = 'X'.
      ENDIF.
      num = num + 1.
    ENDDO.
*-----end
  ELSE.
    num = '01'.
    DO 18 TIMES.
      CONCATENATE 'LPP_BFST' '-' 'BFP' num '_FLG' INTO z_field.
      ASSIGN (z_field) TO <field>.
*No '09' status on any backflush point
      IF lpp_bfst-plan_del_flg EQ 'X' AND <field> EQ '09'.
        EXIT.
      ENDIF.
*Final Backflush is 'Y', and Final point date is less than the
*selection date and NO '01' or '09' on any backflush point and
*SD delivery flag is 'Y'
     IF  lpp_bfst-fin_bf_flg EQ 'Y' AND lpp_bfst-sd_deli_flg EQ 'Y' AND
                                ( <field> EQ '09' OR  <field> EQ '01' )
    .
        EXIT.
      ENDIF.
      IF num = 18.
        p_flg = 'X'.
      ENDIF.
      num = num + 1.
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

  PERFORM call_delete_function USING lpp_bfst
                                     lpp_bfst-plan_ord.
*  ENDIF.

ENDFORM.                    " DELETION_PLAF_RESB
*&---------------------------------------------------------------------*
*&      Form  SERCH_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_RTIME  text
*----------------------------------------------------------------------*
FORM search_day USING    pp_rtime
               CHANGING to_date.

  DATA : p_day LIKE t5a4a-dlydy.
  CLEAR p_day.
  MOVE pp_rtime TO p_day.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            date      = sy-datum
            days      = p_day
            months    = '00'
            signum    = '-'
            years     = '00'
       IMPORTING
            calc_date = be_date.
*check holiday
  PERFORM get_hoiliday USING be_date
                       CHANGING to_date.

ENDFORM.                    " SERCH_DAY
*&---------------------------------------------------------------------*
*&      Form  CALL_DELETE_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0152   text
*----------------------------------------------------------------------*
FORM call_delete_function USING lt_bfst STRUCTURE it_bfst
                                ls_plnum.
  CLEAR : plaf_count,resb_count.

*****COMMENTED BY HASEEB.  UD1K922203
*  IF P_E IS INITIAL. "Check RESB
*    PERFORM CALL_DELETE_FUNCTION_PLAN USING LT_BFST
*                                            LS_PLNUM.
*
*  ELSE.
  PERFORM check_resb_table USING lt_bfst ls_plnum.
  PERFORM call_delete_function_plan USING lt_bfst
                                          ls_plnum.

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
FORM deletion_ztpp_bfst.
  REFRESH : it_delete_c,it_delete_s.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_delete_c
          FROM ztpp_bfst   .

  DESCRIBE TABLE it_delete_c LINES w_int.
  IF w_int <> 0.
    LOOP AT it_delete_c.
      SELECT SINGLE  * FROM plaf WHERE plnum EQ it_delete_c-plan_ord.
      CHECK sy-subrc <> 0.
      DELETE ztpp_bfst FROM it_delete_c.
*     requested by catherine s. changed by chris
*     backup the deleted records for future reference
      APPEND it_delete_c TO it_bfst_bk.
*     end of change on 03/09/2005
      MOVE-CORRESPONDING it_delete_c TO it_delete_s.
      APPEND it_delete_s.
    ENDLOOP.
*   REQUESTED BY CATHERINE S. CHANGED BY CHRIS
*   SAVE THE DELETED ENTIES
    PERFORM save_deleted_entries.
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
FORM call_delete_function_plan USING    p_lt_bfst
                                        p_ls_plnum.
  CALL FUNCTION 'ZPPC1TP_ORDERS_DELETE_PL'
       EXPORTING
            if_restime    = p_rtime
            if_delconf    = 'X'
            if_commit     = 'X'
            s_low         = p_ls_plnum
       IMPORTING
            ef_plaf_count = plaf_count
            ef_resb_count = resb_count
       EXCEPTIONS
            update_error  = 1
            OTHERS        = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF NOT plaf_count IS INITIAL.
      DELETE ztpp_bfst FROM p_lt_bfst.
*     requested by catherine s. changed by chris
*     backup the deleted records for future reference
      APPEND p_lt_bfst TO it_bfst_bk.
*     end of change on 03/09/2005

      COMMIT WORK.
      MOVE : p_ls_plnum    TO it_delete-plnum,
             plaf_count  TO it_delete-plaf_count,
             resb_count  TO it_delete-resb_count.
      APPEND it_delete.
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
FORM check_resb_table USING    p_lt_bfst
                               p_ls_plnum.
  DATA : it_resb LIKE resb OCCURS 0 WITH HEADER LINE,
         z_enmng LIKE resb-enmng.
  CLEAR : w_int,z_enmng,it_resb[].

  SELECT rsnum rspos rsart bdmng erfmg enmng sortf
        INTO CORRESPONDING FIELDS OF TABLE it_resb
             FROM resb
                 WHERE werks EQ p_plant
                 AND plnum EQ p_ls_plnum.

  DESCRIBE TABLE it_resb LINES w_int.
  CHECK NOT w_int IS INITIAL.
  LOOP AT it_resb WHERE enmng EQ space  " Quantity withdrawn
                    AND sortf NE space. " Sort string
    MOVE it_resb-bdmng TO z_enmng.

    UPDATE resb SET enmng = z_enmng
       WHERE rsnum EQ  it_resb-rsnum
         AND rspos EQ  it_resb-rspos
         AND rsart EQ  it_resb-rsart.
    CLEAR z_enmng.
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
FORM get_hoiliday USING  be_date
                  CHANGING to_date.

  PERFORM get_shop_rp_info.
  PERFORM get_calendar_id.
  PERFORM get_work_day USING be_date
                       CHANGING to_date.

ENDFORM.                    " get_hoiliday
*&---------------------------------------------------------------------*
*&      Form  get_shop_rp_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_shop_rp_info.
*----- Read Shop<->RP

  SELECT SINGLE shopname INTO w_arbpl
    FROM ztpp_status
      WHERE rp_point = w_rp_point.
  IF sy-subrc NE 0.
    RAISE etc_exception.
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
FORM get_calendar_id.
*----- Read Shop Calendar ID
  SELECT SINGLE c~kalid c~mosid b~kapid
    INTO (w_kalid, w_mosid, w_kapid)
    FROM crhd AS a INNER JOIN crca AS b
                      ON a~objty = b~objty
                     AND a~objid = b~objid
                   INNER JOIN kako AS c
                      ON b~kapid = c~kapid
   WHERE a~werks =  w_werks
     AND a~arbpl =  w_arbpl
     AND b~fork2 =  'SAP006'.

ENDFORM.                    " get_calendar_id
*&---------------------------------------------------------------------*
*&      Form  get_work_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_work_day USING be_date
                  CHANGING to_date.
  DATA: lw_date  TYPE d,
        lw_lines TYPE i,
        lw_daynr LIKE hrvsched-daynr,
        lw_dayfree LIKE hrvsched-noday.

  lw_date = be_date.

  DO.
    CALL FUNCTION 'RH_GET_DATE_DAYNAME'
         EXPORTING
              langu               = sy-langu
              date                = lw_date
              calid               = w_kalid
         IMPORTING
              daynr               = lw_daynr
              dayfree             = lw_dayfree
         EXCEPTIONS
              no_langu            = 1
              no_date             = 2
              no_daytxt_for_langu = 3
              invalid_date        = 4
              OTHERS              = 5.
    IF sy-subrc <> 0.
      RAISE etc_exception.
    ENDIF.

    IF lw_dayfree EQ 'X'.
      lw_date = lw_date - 1.
      CONTINUE.
    ELSE.
      to_date = lw_date .
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
FORM call_delete_bapi USING lt_bfst STRUCTURE it_bfst
                                ls_plnum.
  DATA : return LIKE bapireturn1.
  CLEAR return.
  CALL FUNCTION 'BAPI_PLANNEDORDER_DELETE'
       EXPORTING
            plannedorder = ls_plnum
*   USE_COLL_UPDATE       = ' '
*   LAST_ORDER            = ' '
       IMPORTING
            return       = return.

  IF sy-subrc = 0.
    MOVE-CORRESPONDING  return TO it_return.
    APPEND it_return.

    DELETE ztpp_bfst FROM lt_bfst.
*   requested by catherine s. changed by chris
*   backup the deleted records for future reference
    APPEND lt_bfst TO it_bfst_bk.
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
FORM check_date TABLES lp_bfst STRUCTURE it_bfst.

  DATA: lw_atinn LIKE cabn-atinn,
        lw_atinn1 LIKE cabn-atinn,
        lw_atnam LIKE cabn-atnam,
        lw_atwrt LIKE ausp-atwrt,
        lw_char_2(2).

  DATA : BEGIN OF lt_atwrt OCCURS 0,
         objek  LIKE ausp-objek,
*        OBJEK LIKE AUSP-OBJEK,
        END OF lt_atwrt.

  DATA : z_tot TYPE i,
         z_num TYPE i.
*  DATA : BEGIN OF it_auspfinal OCCURS 0,
*           objek LIKE ausp-objek,
*           plan_or LIKE  ztpp_bfst-plan_ord,
*         END OF it_auspfinal.
*
*  DATA : zrp25 LIKE ausp-atinn.
*  DATA : zrp27 LIKE ausp-atinn.

  CLEAR : it_delete[],lp_bfst,z_tot,z_num,w_int.


*search hoiliday
  PERFORM search_day USING p_25time
                    CHANGING to_date.

  DESCRIBE TABLE lp_bfst LINES z_tot.

*** BELOW is the logic to delete the Planned order which are 7 days and
*above older. here are the steps,## UD1K922203.  HD#66TI672577
*    1. Read plan order from ZTPP_BFST table with SD_DELI_FLG = 'Y'
*     2. get object from ASUP using step 1
*     3. get P_STATUS = 'RP25/RP27/V05/V07' and P_RP25_ACTUAL_DATE,
*P_RP27_ACTUAL_DATE from AUSP using step 2.
*     4. Compare the P_RP*_ACTUAL_DATE to see if it is 7 days or older ,
* if older delete it using Planned order delete BAPI, which deletes
*entries from RESB and PLAF.

  SELECT SINGLE atinn INTO lw_atinn
      FROM cabn
      WHERE atnam = 'P_RP_STATUS'.

  LOOP AT it_bfst.
    lt_atwrt-objek = it_bfst-vin_num.
*    CONCATENATE IT_BFST-MODEL IT_BFST-BODY_SER INTO IT_ATWRT-OBJEK.
    COLLECT lt_atwrt.
  ENDLOOP.

  SELECT objek atinn atwrt INTO TABLE it_ausp FROM ausp
       FOR ALL ENTRIES IN lt_atwrt
      WHERE klart = '002'
        AND objek = lt_atwrt-objek
        AND atinn = lw_atinn
        AND ( atwrt = '25' OR atwrt = '27' ).

  SELECT SINGLE atinn INTO lw_atinn
       FROM cabn
       WHERE atnam = 'P_RP25_ACTUAL_DATE'.

  SELECT SINGLE atinn INTO lw_atinn1
       FROM cabn
       WHERE atnam = 'P_RP27_ACTUAL_DATE'.

  SELECT objek atinn atwrt INTO TABLE it_ausp_actural FROM ausp
         FOR ALL ENTRIES IN lt_atwrt
        WHERE klart = '002'
          AND objek = lt_atwrt-objek
          AND ( atinn = lw_atinn OR atinn = lw_atinn1 ).

  LOOP AT it_ausp.
*    IF it_ausp-atwrt = '25' OR it_ausp-atwrt = '27'.
    lw_char_2 = it_ausp-atwrt.
    CONCATENATE 'P_RP' lw_char_2 '_ACTUAL_DATE' INTO lw_atnam.

    SELECT SINGLE atinn INTO lw_atinn
    FROM cabn
    WHERE atnam = lw_atnam.

    READ TABLE it_ausp_actural WITH KEY objek = it_ausp-objek
                                       atinn = lw_atinn.

*      SELECT SINGLE atwrt INTO lw_atwrt
*      FROM ausp
*      WHERE klart = '002'
*        and objek = it_ausp-objek
*        AND atinn = lw_atinn.

    IF sy-subrc = 0 AND lw_atwrt+0(8) <= to_date.

      READ TABLE it_bfst WITH KEY vin_num = it_ausp-objek.
*    BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING it_bfst TO wa_bfst.
        APPEND wa_bfst.
        CLEAR wa_bfst.
      ENDIF.
    ENDIF.
*    ENDIF.
  ENDLOOP.

*  LOOP AT IT_BFST.
*    IT_ATWRT-PLN_ORD = IT_BFST-PLAN_ORD.
**    CONCATENATE IT_BFST-MODEL IT_BFST-BODY_SER INTO IT_ATWRT-OBJEK.
*    APPEND IT_ATWRT.
*  ENDLOOP.

*  PERFORM make_atnam USING 'P_PLAN_ORDER'.
*  REFRESH : it_cabn,r_atinn.
*  CLEAR   : it_cabn,r_atinn.
** find the internal characteristic number from CABN table.
*  SELECT atinn atnam INTO TABLE it_cabn
*     FROM cabn
*     WHERE atnam IN r_atnam.
*
*  LOOP AT it_cabn.
*    r_atinn-sign = 'I'.
*    r_atinn-option = 'EQ'.
*    r_atinn-low = it_cabn-atinn.
*    APPEND r_atinn.
*  ENDLOOP.
*
*
*  IF NOT it_atwrt[] IS INITIAL.
*
*    SELECT objek atinn atwrt INTO TABLE it_ausp FROM ausp
*      FOR ALL ENTRIES IN it_atwrt
*     WHERE klart = '002' AND atinn IN r_atinn AND
*       atwrt EQ  it_atwrt-pln_ord.
** REFRESH : it_cabn,r_atinn.
**  CLEAR   : it_cabn,r_atinn.
**  it_cabn = ''.
**  r_atinn = ''.
**
*
**  PERFORM make_atnam USING 'P_STATUS'.
**  PERFORM make_atnam USING 'P_RP25_ACTUAL_DATE'.
**  PERFORM make_atnam USING 'P_RP27_ACTUAL_DATE'.
**
**
**  SELECT atinn atnam INTO TABLE it_cabn
**       FROM cabn
**       WHERE atnam IN r_atnam.
**
**  LOOP AT it_cabn.
**    r_atinn-sign = 'I'.
**    r_atinn-option = 'EQ'.
**    r_atinn-low = it_cabn-atinn.
**    APPEND r_atinn.
**  ENDLOOP.
*
*
** SELECT OBJEK ATINN ATWRT INTO table IT_AUSPSTATUS FROM AUSP
**       FOR ALL ENTRIES IN IT_ATWRT
**       WHERE  OBJEK EQ IT_ATWRT-OBJEK AND KLART = '002'
**              AND ATINN IN R_ATINN.
*
*  ENDIF.
*
*  REFRESH : it_cabn,r_atinn.
*  CLEAR   : it_cabn,r_atinn.
*  it_cabn = ''.
*  r_atinn = ''.
*
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
*
*  IF NOT it_ausp[] IS INITIAL.
*
*    SELECT objek atinn atwrt INTO TABLE it_auspstatus FROM ausp
*      FOR ALL ENTRIES IN it_ausp
*     WHERE klart = '002' AND atinn IN r_atinn
*       AND objek EQ  it_ausp-objek.
*  ENDIF.
*
*  SORT it_ausp BY objek.
*  CLEAR it_auspfinal.
*  REFRESH it_auspfinal.
**  SORT IT_AUSPSTATUS BY OBJEK.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
*       EXPORTING
*            input  = 'P_RP25_ACTUAL_DATE'
*       IMPORTING
*            output = zrp25.
*
*
*  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
*       EXPORTING
*            input  = 'P_RP27_ACTUAL_DATE'
*       IMPORTING
*            output = zrp27.
*
**IF NOT it_auspstatus[] IS INITIAL.
**    LOOP AT it_auspstatus.
**
**      CHECK it_auspstatus-atwrt = 'V05' or it_auspstatus-atwrt = 'V07'
**or it_auspstatus-atwrt = 'T25' or it_auspstatus-atwrt = 'T27'.
**      CASE it_auspstatus-atwrt.
**        WHEN  'V05' or 'T25'.
**          READ table it_auspstatus into wa_auspdate with  KEY
**          OBJEK = it_auspstatus-objek  ATINN = ZRP25. " 0000003425.
**          if  sy-subrc eq 0 and wa_auspdate-ATWRT+0(8) <= TO_DATE.
**            READ TABLE IT_AUSPSTATUS with Key OBJEK  =
**wa_auspdate-objek
**BINARY SEARCH.
**            if sy-subrc eq 0.
**              it_auspfinal-objek = it_auspSTATUS-objek.
**              it_auspfinal-plan_or = it_auspSTATUS-atwrt.
**              APPEND IT_AUSPFINAL.
**            endif.
**          endif.
**        WHEN  'V07' or 'T27'.
**          READ table it_auspstatus into wa_auspdate with KEY
**          OBJEK = it_auspstatus-objek  ATINN = ZRP27. " 0000003424.
**          if sy-subrc eq 0 and wa_auspdate-ATWRT+0(8) <= TO_DATE.
**            READ TABLE IT_AUSPSTATUS with Key OBJEK  =
**wa_auspdate-objek
**BINARY SEARCH.
**            if sy-subrc eq 0.
**              it_auspfinal-objek = it_auspSTATUS-objek.
**              it_auspfinal-plan_or = it_auspSTATUS-atwrt.
**              APPEND IT_AUSPFINAL.
**            endif.
**          endif.
**      ENDCASE.
**      CLEAR IT_AUSPFINAL.
**    ENDLOOP.
*
*  IF NOT it_auspstatus[] IS INITIAL.
*    LOOP AT it_auspstatus.
*
*      CHECK it_auspstatus-atwrt = 'V05' OR it_auspstatus-atwrt = 'V07'
* OR it_auspstatus-atwrt = 'T25' OR it_auspstatus-atwrt = 'T27'.
*      CASE it_auspstatus-atwrt.
*        WHEN  'V05' OR 'T25'.
*          READ TABLE it_auspstatus INTO wa_auspdate WITH  KEY
*          objek = it_auspstatus-objek  atinn = zrp25. " 0000003425.
*          IF  sy-subrc EQ 0 AND wa_auspdate-atwrt+0(8) <= to_date.
*            READ TABLE it_ausp WITH KEY objek  = wa_auspdate-objek
*BINARY SEARCH.
*            IF sy-subrc EQ 0.
*              it_auspfinal-objek = it_ausp-objek.
*              it_auspfinal-plan_or = it_ausp-atwrt.
*              APPEND it_auspfinal.
*            ENDIF.
*          ENDIF.
*        WHEN  'V07' OR 'T27'.
*          READ TABLE it_auspstatus INTO wa_auspdate WITH KEY
*          objek = it_auspstatus-objek  atinn = zrp27. " 0000003424.
*          IF sy-subrc EQ 0 AND wa_auspdate-atwrt+0(8) <= to_date.
*            READ TABLE it_ausp WITH KEY objek  = wa_auspdate-objek
*BINARY SEARCH.
*            IF sy-subrc EQ 0.
*              it_auspfinal-objek = it_ausp-objek.
*              it_auspfinal-plan_or = it_ausp-atwrt.
*              APPEND it_auspfinal.
*            ENDIF.
*          ENDIF.
*      ENDCASE.
*      CLEAR it_auspfinal.
*    ENDLOOP.
*
*  ENDIF.
*
*  SORT it_bfst BY plan_ord.
*  LOOP AT it_auspfinal.
*    READ TABLE it_bfst WITH KEY plan_ord = it_auspfinal-plan_or BINARY
*   SEARCH.
*    IF sy-subrc EQ 0.
*      MOVE-CORRESPONDING it_bfst TO wa_bfst.
*      APPEND wa_bfst.
*      CLEAR wa_bfst.
*    ENDIF.
*  ENDLOOP.

  LOOP AT wa_bfst.
    PERFORM deletion_plaf_resb USING wa_bfst .
  ENDLOOP.

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
FORM check_deleted_plannedorder TABLES   dp_bfst STRUCTURE  it_bfst.
  LOOP AT dp_bfst.
    PERFORM deletion_plaf_resb USING dp_bfst .
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
FORM save_deleted_entries.

  SORT it_bfst_bk BY plant model body_ser plan_ord.
  DELETE ADJACENT DUPLICATES FROM it_bfst_bk
     COMPARING plant model body_ser plan_ord.

  DESCRIBE TABLE it_bfst_bk LINES w_backup.

  IF w_backup NE 0.
    MODIFY ztpp_bfst_bk FROM TABLE it_bfst_bk.
    IF sy-subrc NE 0.
      MESSAGE i000 WITH 'BACKUP OF DELETED ENTRIES FAILED!'.
    ELSE.
      COMMIT WORK .
      IF sy-subrc EQ 0.
        CLEAR: it_bfst_bk, it_bfst_bk[].
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " SAVE_DELETED_ENTRIES
*&---------------------------------------------------------------------*
*&      Form  MAKE_atnam  BY HASEEB.  UD1K922203   HD#66TI672577
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM make_atnam USING  p_atnam.
  r_atnam-sign = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low = p_atnam.
  APPEND r_atnam.
ENDFORM.                   "MAKE_ATNAM END.
