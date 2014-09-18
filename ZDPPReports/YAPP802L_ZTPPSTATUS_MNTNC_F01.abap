*----------------------------------------------------------------------*
*   INCLUDE YAPP_ROUTING_F01                                           *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  set_field_wc
*&---------------------------------------------------------------------*
*       Getting Data For Dropdown ListBox - arbpl
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_wc.
  SELECT DISTINCT arbpl
    INTO wc_value-key
    FROM ztpp_status.
    APPEND wc_value TO wc_list.
  ENDSELECT.
ENDFORM.                    " set_field_wc
*&---------------------------------------------------------------------*
*&      Form  set_field_ck
*&---------------------------------------------------------------------*
*       Getting Data For Dropdown ListBox - bf_usage
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_ck.
  SELECT DISTINCT bf_usage
    INTO ck_value-key
    FROM ztpp_status.
    APPEND ck_value TO ck_list.
  ENDSELECT.
ENDFORM.                    " set_field_ck
*&---------------------------------------------------------------------*
*&      Form  set_field_sa
*&---------------------------------------------------------------------*
*       Getting Data For Dropdown ListBox - prvbe
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_sa.
  SELECT DISTINCT prvbe
    INTO sa_value-key
    FROM ztpp_status.
    APPEND sa_value TO sa_list.
  ENDSELECT.
ENDFORM.                    " set_field_sa
*&---------------------------------------------------------------------*
*&      Form  set_field_ss
*&---------------------------------------------------------------------*
*       Getting Data For Dropdown ListBox - rp_point
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_ss.
*DATA: p_wc(08),  "Work Center(arbpl)
*      p_ck(01),  "Control Key(bf_usage)
*      p_sa(10),  "Supply Area(prvbe)
*      p_ss(02),  "Sort String(rp_point)
*      p_bf(20).  "Backflush point(usr01)
  SELECT DISTINCT rp_point
    INTO ss_value-key
    FROM ztpp_status.
    APPEND ss_value TO ss_list.
  ENDSELECT.
ENDFORM.                    " set_field_ss
*&---------------------------------------------------------------------*
*&      Form  set_field_bf
*&---------------------------------------------------------------------*
*       Getting Data For Dropdown ListBox - usr01
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_bf.
  SELECT DISTINCT usr01
    INTO bf_value-key
    FROM ztpp_status.
    APPEND bf_value TO bf_list.
  ENDSELECT.
ENDFORM.                    " set_field_bf
*&---------------------------------------------------------------------*
*&      Form  call_function_vrm
*&---------------------------------------------------------------------*
*       Calling a Func. For DropDown ListBox
*----------------------------------------------------------------------*
*      -->P_BF_LIST  text
*----------------------------------------------------------------------*
FORM call_function_vrm USING    p_list.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = name
            values = p_list.
ENDFORM.                    " call_function_vrm
*&---------------------------------------------------------------------*
*&      Form  search_data
*&---------------------------------------------------------------------*
*       Searching Data From ZTPP_STATUS
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM search_data.
  DATA: l_id(05),
        l_wc(09),  "Work Center(arbpl)
        l_ck(02),  "Control Key(bf_usage)
        l_sa(11),  "Supply Area(prvbe)
        l_ss(03),  "Sort String(rp_point)
        l_bf(21).  "Backflush point(usr01)
  CONCATENATE p_id '%'
    INTO l_id.
  CONCATENATE p_wc '%'
    INTO l_wc.
  CONCATENATE p_ck '%'
    INTO l_ck.
  CONCATENATE p_sa '%'
    INTO l_sa.
  CONCATENATE p_ss '%'
    INTO l_ss.
  CONCATENATE p_bf '%'
    INTO l_bf.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_app802
    FROM ztpp_status
    WHERE id LIKE l_id AND
          arbpl LIKE l_wc AND
          bf_usage LIKE l_ck AND
          prvbe LIKE l_sa AND
          rp_point LIKE l_ss AND
          usr01 LIKE l_bf .

  SORT IT_app802 BY id .
  tc_app802-top_line = 1.
ENDFORM.                    " search_data
*&---------------------------------------------------------------------*
*&      Form  insert_new_line
*&---------------------------------------------------------------------*
*       Calculation of Line of Internal Table & Table Control's
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_new_line.
  DATA: l_curr_line TYPE i,
        l_data_lines TYPE i.
* Reading Table Control's Current Line on the Screen
  GET CURSOR LINE l_curr_line.
  IF sy-subrc <> 0.
    MESSAGE s000 WITH 'Set the cursor correctly!!!'.
    EXIT.
  ENDIF.
* Calculating The Current Line in the Internal Table
  l_curr_line = l_curr_line + tc_app802-top_line - 1.
* Getting The Number of Data of Internal Table
  DESCRIBE TABLE IT_app802 LINES l_data_lines.
  "If There isn't data in The Internal Table
  IF l_data_lines = 0.
    CLEAR IT_app802.
    IT_app802-new = 'X'.
    IT_app802-mark = 'X'.
    APPEND IT_app802.
    "If There is data in The Internal Table
  ELSE.
*   Inserting a New Line Into The Internal Table at The Current Line
*   with Check Fields - New, Mark
    INSERT INITIAL LINE INTO IT_app802 INDEX l_curr_line.
    CLEAR IT_app802.
    IT_app802-new = 'X'.
    IT_app802-mark = 'X'.
    MODIFY IT_app802 INDEX l_curr_line.
*   Deleting a Mark of The Previous' Record of Internal Table
    l_curr_line = l_curr_line + 1.
    READ TABLE IT_app802 INDEX l_curr_line.
    CLEAR IT_app802-mark.
    MODIFY IT_app802 INDEX l_curr_line.

  ENDIF.
* Calculating The Current Line on the Screen to Set Curror Field
  wa_current_line = l_curr_line - tc_app802-top_line .

ENDFORM.                    " insert_new_line
*&---------------------------------------------------------------------*
*&      Form  delete_data
*&---------------------------------------------------------------------*
*       The Process of Data Deletion
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data.
  DATA: l_count TYPE i.
* Reading The Number of The Internal Table's Data
  DESCRIBE TABLE IT_app802 LINES l_count.
  IF l_count < 1.
    MESSAGE s000 WITH 'There is no data to delete!!!'.
    EXIT.
  ENDIF.
* Getting The Table Control's Current Line
  GET CURSOR LINE l_count.
  IF sy-subrc <> 0.
    MESSAGE s000 WITH 'Set Cursor Correctly!!'.
  ENDIF.
* Copying Data To be Deleted into IT_DEL.
  LOOP AT IT_app802 WHERE  mark = 'X'.
    MOVE-CORRESPONDING IT_app802 TO it_del .
    APPEND it_del.
*   Deleting Data to be deleted from IT_app802.
    DELETE IT_app802.
  ENDLOOP.
ENDFORM.                    " delete_data
*&---------------------------------------------------------------------*
*&      Form  mark_upd_flag
*&---------------------------------------------------------------------*
*       The Process of Update
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mark_upd_flag.
  DATA l_line TYPE sy-tabix.
  READ TABLE IT_app802 WITH KEY mark = 'X'.
  l_line = sy-tabix.
  IF sy-subrc = 0.
*   First: Check If there is data in IT_UPD with IT_APP's key fields .
    READ TABLE it_upd WITH KEY id = IT_app802-id .
    IF sy-subrc <> 0.
*     If there is no same data in IT_UPD,
*     Second: Check ZTPP_STATUS with IT_APP's Key Field .
      CLEAR it_upd.
      SELECT SINGLE *
        INTO CORRESPONDING FIELDS OF it_upd
        FROM ztpp_status
        WHERE id       = IT_app802-id .
      IF sy-subrc = 0.
*       If there is a same data in ZTPP_STATUS,
*       Mark The Update Flag in IT_app802.
        APPEND it_upd .
        IT_app802-upd = 'X'.
        MODIFY IT_app802 INDEX l_line .
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE s000 WITH 'There is no data to update!!'.
  ENDIF.
ENDFORM.                    " mark_upd_flag
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       Data Deletion of ZTPP_STATUS with The Internal Table
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.
  COMMIT WORK.
* Data Deletion
  DELETE ztpp_status FROM TABLE it_del .
  IF sy-subrc <> 0.
    ROLLBACK WORK.
    MESSAGE s000 WITH 'Deletion Process Failed!!!'.
  ELSE.
    MESSAGE s000 WITH 'Deletion Process has been done.'.
  ENDIF.
* Data Update and Insert
  LOOP AT IT_app802 WHERE new = 'X' OR upd = 'X' .
    CLEAR ztpp_status.
    IF IT_app802-upd = 'X'.  "For Update
      IT_app802-aedat = sy-datum .
      IT_app802-aezet = sy-uzeit .
      IT_app802-aenam = sy-uname .

      UPDATE ztpp_status FROM IT_app802 .
    ELSE.                 "For Insert
      IT_app802-erdat = sy-datum .
      IT_app802-erzet = sy-uzeit .
      IT_app802-ernam = sy-uname .
      IT_app802-aedat = sy-datum .
      IT_app802-aezet = sy-uzeit .
      IT_app802-aenam = sy-uname .

      INSERT INTO ztpp_status VALUES IT_app802.
    ENDIF.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      MESSAGE s001 WITH IT_app802-id
                        ' : Updating ZTPP_STATUS Failed!!!'.
    ELSE.
      COMMIT WORK.
      MESSAGE s001 WITH IT_app802-id
                    ' : Updating ZTPP_STATUS has been well done.'.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " update_table
*&---------------------------------------------------------------------*
*&      Form  modify_sap
*&---------------------------------------------------------------------*
*       The Process of Data Mapping between SAP and ZTPP_STATUS
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_sap USING p_flg.
  PERFORM bdc_open_group.
  COMMIT WORK.
* Deletion Process.
  PERFORM deletion_process CHANGING p_flg.
  IF p_flg <> space.
    ROLLBACK WORK.
    EXIT.
  ENDIF.

* Update Process.
  PERFORM update_process CHANGING p_flg.
  IF p_flg <> space.
    ROLLBACK WORK.
    EXIT.
  ENDIF.

* Insert Process.
  PERFORM insert_process CHANGING p_flg.
  IF p_flg <> space.
    ROLLBACK WORK.
    EXIT.
  ENDIF.
  PERFORM bdc_close_group.

ENDFORM.                    " modify_sap
*&---------------------------------------------------------------------*
*&      Form  bdc_open_group
*&---------------------------------------------------------------------*
*       Starting BDC Process
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_open_group.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            client              = sy-mandt
            group               = sy-uname
            keep                = 'X'
            user                = sy-uname
       EXCEPTIONS
            client_invalid      = 1
            destination_invalid = 2
            group_invalid       = 3
            group_is_locked     = 4
            holddate_invalid    = 5
            internal_error      = 6
            queue_error         = 7
            running             = 8
            system_lock_error   = 9
            user_invalid        = 10
            OTHERS              = 11.

  IF sy-subrc <> 0.
*    write:   /, 'ERROR-BDC Opening Group : ', sy-uzeit,
*             /, 'OPEN_GROUP RETURN CODE  : ', sy-subrc.
*    exit.
  ELSE.
*    write: /, 'BDC Opened....'.
  ENDIF.
ENDFORM.                    " open_group
*&---------------------------------------------------------------------*
*&      Form  bdc_close_group
*&---------------------------------------------------------------------*
*       Ending BDC Process
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_close_group.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            not_open    = 1
            queue_error = 2
            OTHERS      = 3.

  IF sy-subrc <> 0.
*    write: /, ' BDC Close Error : ', sy-uzeit,
*           /, ' Return Code     : ', sy-subrc.
    EXIT.
  ELSE.
*    write: /, 'BDC Closed....'.
  ENDIF.
ENDFORM.                    " close_group
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       Setting Data For BDC
*----------------------------------------------------------------------*
*      -->P_0805   text
*      -->P_0806   text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR it_fbdcdata.
  it_fbdcdata-program  = program.
  it_fbdcdata-dynpro   = dynpro.
  it_fbdcdata-dynbegin = 'X'.
  APPEND it_fbdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*       Setting Data For BDC
*----------------------------------------------------------------------*
*      -->P_0810   text
*      -->P_0811   text
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  IF fval <> nodata.
    CLEAR it_fbdcdata.
    it_fbdcdata-fnam = fnam.
    it_fbdcdata-fval = fval.
    APPEND it_fbdcdata.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  check_data
*&---------------------------------------------------------------------*
*       Checking Data From SAP - Work Center & Control Key
*----------------------------------------------------------------------*
*      -->P_IT_DEL_ARBPL  text
*      -->P_IT_DEL_BF_USAGE  text
*----------------------------------------------------------------------*
FORM check_data USING    p_arbpl   "Work Center
                         p_steus   "Control Key
                CHANGING p_flag .
  DATA: l_text(200) TYPE c,
        l_text_2(200) TYPE c,
        l_temp TYPE crhd-arbpl.

  SELECT SINGLE arbpl
    INTO l_temp
    FROM ( ( crhd AS ch
         INNER JOIN plpo AS pp ON ch~objid = pp~arbid )
         INNER JOIN plas AS pa ON pp~plnty = pa~plnty AND
                                  pp~plnnr = pa~plnnr AND
                                  pp~plnkn = pa~plnkn AND
                                  pp~zaehl = pa~zaehl )
    WHERE ch~arbpl = p_arbpl AND
          pp~steus = p_steus AND
          pp~plnty = 'M'     AND  "Reference Rate Routing
          pp~plnnr = 'RP'    AND  "Group Key
          ch~objty = 'A'     AND  "Work Center
          pa~loekz <> 'X'.  "Deletion Flag
  IF sy-subrc <> 0.
    p_flag = 'X'.
    CONCATENATE 'The Process Failed!! -'
                'Work Center :'
                p_arbpl ','
      INTO l_text.
    CONCATENATE ' Control Key :'
                p_steus
                ' - That isn''t in SAP.'
      INTO l_text_2.
    MESSAGE s001 WITH l_text l_text_2.
  ENDIF.
ENDFORM.                    " check_data
*&---------------------------------------------------------------------*
*&      Form  make_fbdcdata_for_del
*&---------------------------------------------------------------------*
*       Making BDC Data For Data Deletion
*----------------------------------------------------------------------*
*      -->P_IT_DEL_ARBPL  text
*      -->P_IT_DEL_BF_USAGE  text
*----------------------------------------------------------------------*
FORM make_fbdcdata_for_del USING    p_arbpl  "Work Center
                                    p_steus. "Control Key
  DATA: BEGIN OF l_it_temp OCCURS 0.
          INCLUDE STRUCTURE plpo.
  DATA:   arbpl TYPE crhd-arbpl,
        END OF l_it_temp .
  DATA: l_count TYPE i,
        l_count_c(10),
        l_date_c(10).

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE l_it_temp
    FROM ( ( crhd AS ch
         INNER JOIN plpo AS pp ON ch~objid = pp~arbid )
         INNER JOIN plas AS pa ON pp~plnty = pa~plnty AND
                                  pp~plnnr = pa~plnnr AND
                                  pp~plnkn = pa~plnkn AND
                                  pp~zaehl = pa~zaehl )
    WHERE pp~plnty = 'M'     AND  "Reference Rate Routing
          pp~plnnr = 'RP'    AND  "Group Key
          ch~objty = 'A'     AND  "Work Center
          pa~loekz <> 'X' .       "Delete Flag

  SORT l_it_temp BY vornr .  "Sorting By Operation Number.
  READ TABLE l_it_temp WITH KEY arbpl = p_arbpl
                                steus = p_steus .
  l_count = sy-tabix.
  WRITE l_count TO l_count_c LEFT-JUSTIFIED.
  WRITE: sy-datum TO l_date_c.
*******************************************************
** Setting For Data Deletion.
*******************************************************
  PERFORM bdc_dynpro      USING 'SAPLCPDI' '1001'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RC271-STTAG'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VOUE'.
  PERFORM bdc_field       USING 'RC271-PLNNR'
                                'RP'.
  PERFORM bdc_field       USING 'RC271-STTAG'
                                l_date_c.
*perform bdc_field       using 'RC271-WERKS'
*                              record-WERKS_003.
*perform bdc_field       using 'RC271-PLNAL'
*                              record-PLNAL_004.

  PERFORM bdc_dynpro      USING 'SAPLCPDI' '5400'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RC27X-ENTRY_ACT'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RC27X-ENTRY_ACT'
                                l_count_c.

  PERFORM bdc_dynpro      USING 'SAPLCPDI' '5400'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'PLPOD-VORNR(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=LOE'.
  PERFORM bdc_field       USING 'RC27X-ENTRY_ACT'
                                l_count_c.
  PERFORM bdc_field       USING 'RC27X-FLG_SEL(01)'
                                'X'.

  PERFORM bdc_dynpro      USING 'SAPLSPO1' '0100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=YES'.

  PERFORM bdc_dynpro      USING 'SAPLCPDI' '5400'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RC27X-ENTRY_ACT'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.
  PERFORM bdc_field       USING 'RC27X-ENTRY_ACT'
                                l_count_c.
*******************************************************

ENDFORM.                    " make_fbdcdata_for_del
*&---------------------------------------------------------------------*
*&      Form  DELETION_PROCESS
*&---------------------------------------------------------------------*
*       Deletion Process
*----------------------------------------------------------------------*
*      <--P_P_FLG  text
*----------------------------------------------------------------------*
FORM deletion_process CHANGING p_flg.
  DATA: l_steus TYPE plpo-steus .
  DATA: l_text(100),
      l_text_2(100).
  LOOP AT it_del.
    CLEAR l_steus.
    IF it_del-bf_usage = 'X'.
      IF it_del-prvbe = 'SOFF'.
        l_steus = 'PP99'.  "Sign Off
      ELSE.
        l_steus = 'PP97'.  "Confirmation
      ENDIF.
    ELSE.
      l_steus = 'PP90'.    "No Confirmation
    ENDIF.
    PERFORM check_data USING it_del-id l_steus
                       CHANGING p_flg .
    IF p_flg <> space.
      EXIT.
    ENDIF.
    CLEAR: it_fbdcdata, it_fbdcdata[].
**  Set BDC Data.
    CLEAR: it_msg_tab,  it_msg_tab[],
           it_fbdcdata, it_fbdcdata[].
    PERFORM make_fbdcdata_for_del USING it_del-id
                                        l_steus .
**  Run BDC For Data Deletion.
    CALL TRANSACTION 'CA32'  USING it_fbdcdata
                             UPDATE 'S'
                             MODE   'A'
                             MESSAGES INTO it_msg_tab.
    IF sy-subrc <> 0.
      p_flg = 'X' .
      CONCATENATE 'Deletion Process Failed!! -'
                  'Work Center :'
                  it_del-id
        INTO l_text.
      CONCATENATE ',' ' Control Key :'
                  l_steus
                  ' - There is a Problem in SAP.'
        INTO l_text_2.
      MESSAGE s001 WITH l_text l_text_2.
    ENDIF.

  ENDLOOP.
  IF p_flg <> space.
    EXIT.
  ENDIF.

ENDFORM.                    " DELETION_PROCESS
*&---------------------------------------------------------------------*
*&      Form  update_process
*&---------------------------------------------------------------------*
*       Update Process
*----------------------------------------------------------------------*
*      <--P_P_FLG  text
*----------------------------------------------------------------------*
FORM update_process CHANGING p_flg.
  DATA: l_steus TYPE plpo-steus,
        l_werks TYPE crhd-werks.
  LOOP AT IT_app802 WHERE upd = 'X'.
    CLEAR l_steus.
    READ TABLE it_upd WITH KEY id = IT_app802-id .
    IF sy-subrc <> 0.
      p_flg = 'X'.
      EXIT.
    ENDIF.

    IF it_upd-bf_usage = 'X'.
      IF it_upd-prvbe = 'SOFF'.
        l_steus = 'PP99'.  "Sign Off
      ELSE.
        l_steus = 'PP97'.  "Confirmation
      ENDIF.
    ELSE.
      l_steus = 'PP90'.    "No Confirmation
    ENDIF.
    PERFORM check_data USING it_upd-id l_steus
                       CHANGING p_flg .
    IF p_flg <> space.
      EXIT.
    ENDIF.

    IF IT_app802-bf_usage = 'X'.
      IF IT_app802-prvbe = 'SOFF'.
        l_steus = 'PP99'.  "Sign Off
      ELSE.
        l_steus = 'PP97'.  "Confirmation
      ENDIF.
    ELSE.
      l_steus = 'PP90'.    "No Confirmation
    ENDIF.

    PERFORM update_bdc USING IT_app802-id
                             l_steus
                             IT_app802-rp_point
                             IT_app802-prvbe
                             IT_app802-usr01
                             IT_app802-usr02
                       CHANGING p_flg.
    IF p_flg <> space.
      EXIT.
    ENDIF.

  ENDLOOP.
  IF p_flg <> space.
    EXIT.
  ENDIF.

ENDFORM.                    " update_process
*&---------------------------------------------------------------------*
*&      Form  make_fbdcdata_for_upd
*&---------------------------------------------------------------------*
*       Making Data For Data Update
*----------------------------------------------------------------------*
*      -->P_IT_app802_ID  text
*      -->P_L_STEUS  text
*      -->P_IT_app802_RP_POINT  text
*      -->P_IT_app802_PRVBE  text
*      -->P_USR01  text
*      -->P_USR02  text
*----------------------------------------------------------------------*
FORM make_fbdcdata_for_upd USING    p_arbpl  "Work Center
                                    p_steus  "Control Key
*                                    p_sortb  "Sort String
                                    p_usr00  "Supply Area
                                    p_usr01  "B/F Point
                                    p_usr02  " It isn't defined yet...
                           CHANGING p_werks .
  DATA: BEGIN OF l_it_temp OCCURS 0.
          INCLUDE STRUCTURE plpo.
  DATA:   arbpl TYPE crhd-arbpl,
        END OF l_it_temp .
  DATA: l_steus TYPE plpo-steus.
  DATA: l_count TYPE i,
        l_count_c(10),
        l_date_c(10).

*************>>>>>>>> Check <<<<<<<<****************
****     How can I get the recent data???       ****
****************************************************
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE l_it_temp
    FROM ( ( crhd AS ch
         INNER JOIN plpo AS pp ON ch~objid = pp~arbid )
         INNER JOIN plas AS pa ON pp~plnty = pa~plnty AND
                                  pp~plnnr = pa~plnnr AND
                                  pp~plnkn = pa~plnkn AND
                                  pp~zaehl = pa~zaehl )
    WHERE pp~plnty = 'M'     AND  "Reference Rate Routing
          pp~plnnr = 'RP'    AND  "Group Key
          ch~objty = 'A'     AND  "Work Center
          pa~loekz <> 'X' .       "Delete Flag

***************************************************
* Check The Present Line Number.
***************************************************
  READ TABLE it_upd WITH KEY id = p_arbpl .
  IF it_upd-bf_usage = 'X'.
    IF it_upd-prvbe = 'SOFF'.
      l_steus = 'PP99'.
    ELSE.
      l_steus = 'PP97'.
    ENDIF.
  ELSE.
    l_steus = 'PP90'.
  ENDIF.
  SORT l_it_temp BY vornr .  "Sorting By Operation Number
  READ TABLE l_it_temp WITH KEY arbpl = it_upd-id
                                steus = l_steus .
  l_count = sy-tabix.
  MOVE l_it_temp-werks TO p_werks.
  WRITE: l_count  TO l_count_c LEFT-JUSTIFIED,
         sy-datum TO l_date_c.

*******************************************************
** Setting For Data Update.
*******************************************************
  PERFORM bdc_dynpro      USING 'SAPLCPDI' '1001'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RC271-STTAG'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VOUE'.
  PERFORM bdc_field       USING 'RC271-PLNNR'
                                'RP'.
  PERFORM bdc_field       USING 'RC271-STTAG'
                                l_date_c.
  PERFORM bdc_field       USING 'RC271-WERKS'
                                l_it_temp-werks.
*perform bdc_field       using 'RC271-PLNAL'
*                              record-PLNAL_004.

  PERFORM bdc_dynpro      USING 'SAPLCPDI' '5400'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RC27X-ENTRY_ACT'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RC27X-ENTRY_ACT'
                                l_count_c.

  PERFORM bdc_dynpro      USING 'SAPLCPDI' '5400'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'PLPOD-STEUS(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VOD1'.
  PERFORM bdc_field       USING 'RC27X-ENTRY_ACT'
                                l_count_c.
  PERFORM bdc_field       USING 'RC27X-FLG_SEL(01)'
                                'X'.
  PERFORM bdc_field       USING 'PLPOD-ARBPL(01)'
                                p_arbpl.
  PERFORM bdc_field       USING 'PLPOD-STEUS(01)'
                                p_steus.

  PERFORM bdc_dynpro      USING 'SAPLCPDO' '1200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'PLPOD-SLWID'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'PLPOD-SLWID'
                                'SA-BF'.

  PERFORM bdc_dynpro      USING 'SAPLCPDO' '1200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'PLPOD-USR01'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.
  PERFORM bdc_field       USING 'PLPOD-USR00'
                                p_usr00.
  PERFORM bdc_field       USING 'PLPOD-USR01'
                                p_usr01.


ENDFORM.                    " make_fbdcdata_for_upd
*&---------------------------------------------------------------------*
*&      Form  make_fbdcdata_for_upd_wc
*&---------------------------------------------------------------------*
*       Making Data For Data Update - Work Center
*----------------------------------------------------------------------*
*      -->P_L_WERKS  text
*      -->P_IT_app802_RP_POINT  text
*----------------------------------------------------------------------*
FORM make_fbdcdata_for_upd_wc USING    p_werks
                                       p_arbpl
                                       p_sortb .
*******************************************************
** Setting For Data Update.
*******************************************************
  PERFORM bdc_dynpro      USING 'SAPLCRA0' '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RC68A-ARBPL'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=TECH'.
  PERFORM bdc_field       USING 'RC68A-WERKS'
                                p_werks.
  PERFORM bdc_field       USING 'RC68A-ARBPL'
                                p_arbpl.

  PERFORM bdc_dynpro      USING 'SAPLCRA0' '4000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'P3007-SORTB'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=UPD'.
  PERFORM bdc_field       USING 'P3007-SORTB'
                                p_sortb.

ENDFORM.                    " make_fbdcdata_for_upd_wc
*&---------------------------------------------------------------------*
*&      Form  insert_process
*&---------------------------------------------------------------------*
*       Insert Process
*----------------------------------------------------------------------*
*      <--P_P_FLG  text
*----------------------------------------------------------------------*
FORM insert_process CHANGING p_flg.
  DATA: l_steus TYPE plpo-steus,
        l_werks TYPE crhd-werks,
        l_status,
        l_temp TYPE ztpp_status-id.
  DATA: l_text(100),
        l_text_2(100).
  LOOP AT IT_app802 WHERE new = 'X'.
    CLEAR l_steus.
**************************************
**  Checking Unique Data.
**************************************
    SELECT SINGLE id
      INTO l_temp
      FROM ztpp_status
      WHERE id = IT_app802-id .
    IF sy-subrc = 0.
      p_flg = 'X'.
      MESSAGE s000 WITH 'Violation of Unique Key!!'.
      EXIT.
    ENDIF.
**************************************
    IF IT_app802-bf_usage = 'X'.
      IF IT_app802-prvbe = 'SOFF'.
        l_steus = 'PP99'.  "Sign Off
      ELSE.
        l_steus = 'PP97'.  "Confirmation
      ENDIF.
    ELSE.
      l_steus = 'PP90'.    "No Confirmation
    ENDIF.
    PERFORM check_data_for_ins USING IT_app802-id
                                     l_steus
                               CHANGING l_status .
    IF l_status = 'I'.  "Insert
      PERFORM create_data USING IT_app802-id
                                l_steus
                                IT_app802-rp_point
                                IT_app802-prvbe
                                IT_app802-usr01
                                IT_app802-usr02
                          CHANGING p_flg .
    ELSE.               "Update
*     Usually, It will not work.
*     But, If there is a condition change,
*     It will be necessary .
      PERFORM update_bdc USING IT_app802-id
                               l_steus
                               IT_app802-rp_point
                               IT_app802-prvbe
                               IT_app802-usr01
                               IT_app802-usr02
                         CHANGING p_flg.
    ENDIF.
    IF p_flg <> space.
      EXIT.
    ENDIF.

  ENDLOOP.
  IF p_flg <> space.
    EXIT.
  ENDIF.

ENDFORM.                    " insert_process
*&---------------------------------------------------------------------*
*&      Form  check_data_for_ins
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_app802_ID  text
*      -->P_L_STEUS  text
*      <--P_L_STATUS  text
*----------------------------------------------------------------------*
FORM check_data_for_ins USING    p_id
                                 p_steus
                        CHANGING p_flg.
  DATA: l_text(200) TYPE c,
        l_text_2(200) TYPE c,
        l_temp TYPE ztpp_status,
        l_bf_usage TYPE ztpp_status-bf_usage.
  RANGES: l_prvbe FOR ztpp_status-prvbe.

  CASE p_steus.
    WHEN 'PP90'.  "No Confirmation
      CLEAR: l_bf_usage.
      CLEAR: l_prvbe, l_prvbe[].
      l_prvbe-low = 'SOFF'.
      l_prvbe-sign = 'I'.
      l_prvbe-option = 'NE'.
      APPEND l_prvbe.
    WHEN 'PP97'.  "Confirmation
      l_bf_usage = 'X'.
      CLEAR: l_prvbe, l_prvbe[].
      l_prvbe-low = 'SOFF'.
      l_prvbe-sign = 'I'.
      l_prvbe-option = 'NE'.
      APPEND l_prvbe.
    WHEN 'PP99'.  "Confirmation with GR
      l_bf_usage = 'X'.
      CLEAR: l_prvbe, l_prvbe[].
      l_prvbe-low = 'SOFF'.
      l_prvbe-sign = 'I'.
      l_prvbe-option = 'EQ'.
      APPEND l_prvbe.
  ENDCASE.

  SELECT SINGLE *
    INTO l_temp
    FROM ztpp_status
    WHERE id = p_id AND
          bf_usage = l_bf_usage AND
          prvbe IN l_prvbe .

  IF sy-subrc <> 0.
    p_flg = 'I'.
  ELSE.
    p_flg = 'U'.
    MOVE-CORRESPONDING l_temp TO it_upd.
    APPEND it_upd.
  ENDIF.
*  SELECT SINGLE arbpl
*    INTO l_temp
*    FROM ( ( crhd AS ch
*         INNER JOIN plpo AS pp ON ch~objid = pp~arbid  )
*         INNER JOIN plas AS pa ON pp~plnty = pa~plnty AND
*                                  pp~plnnr = pa~plnnr AND
*                                  pp~plnkn = pa~plnkn AND
*                                  pp~zaehl = pa~zaehl )
*    WHERE ch~arbpl = p_arbpl AND
*          pp~steus = p_steus AND
*          pp~plnty = 'M'     AND  "Reference Rate Routing
*          pp~plnnr = 'RP'    AND  "Group Key
*          ch~objty = 'A'     AND  "Work Center
*          pa~loekz <> 'X' .       "Delete Flag

ENDFORM.                    " check_data_for_ins
*&---------------------------------------------------------------------*
*&      Form  create_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_app802_ID  text
*      -->P_L_STEUS  text
*      <--P_P_FLG  text
*----------------------------------------------------------------------*
FORM create_data USING    p_arbpl  "Work Center
                          p_steus  "Control Key
                          p_rp_point  "Sort String
                          p_prvbe  "Supply Area
                          p_usr01  "B/F Point
                          p_usr02  "Not Defined.
                 CHANGING p_flg.
  DATA: l_plnnr TYPE plko-plnnr.
  SELECT SINGLE plnnr
    INTO l_plnnr
    FROM plpo
    WHERE plnty = 'M' AND
          plnnr = 'RP'   .
* Insert New Line .
  IF sy-subrc = 0 .
    PERFORM create_new_line USING p_arbpl
                                  p_steus
                                  p_rp_point
                                  p_prvbe
                                  p_usr01
                                  p_usr02
                           CHANGING p_flg.
* Create Reference Rate Routing And Insert New Line .
  ELSE.
    PERFORM create_routing CHANGING p_flg.
    PERFORM create_new_line USING p_arbpl
                                  p_steus
                                  p_rp_point
                                  p_prvbe
                                  p_usr01
                                  p_usr02
                           CHANGING p_flg.
  ENDIF.

ENDFORM.                    " create_data
*&---------------------------------------------------------------------*
*&      Form  update_bac
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_app802_ID  Work Center
*      -->P_L_STEUS  Control Key
*      -->P_IT_app802_RP_POINT  Sort String
*      -->P_IT_app802_PRVBE  Supply Area
*      -->P_IT_app802_USR01  B/F Point
*      -->P_IT_app802_USR02  Not Define
*      <--P_P_FLG  Check Flag
*----------------------------------------------------------------------*
FORM update_bdc USING    p_id
                         p_steus
                         p_rp_point
                         p_prvbe
                         p_usr01
                         p_usr02
                CHANGING p_flg.
  DATA: l_werks TYPE crhd-werks ,
        l_text(100),
        l_text_2(100).
*  PERFORM bdc_open_group.
  CLEAR: it_fbdcdata, it_fbdcdata[].
**  Set BDC Data.
  CLEAR: it_msg_tab,  it_msg_tab[],
         it_fbdcdata, it_fbdcdata[].
  PERFORM make_fbdcdata_for_upd USING p_id         "Work Center
                                      p_steus      "Control Key
*                                     p_rp_point   "Sort String
                                      p_prvbe      "Supply Area
                                      p_usr01      "B/F Point
                                      p_usr02    " Not Defined
                                CHANGING l_werks .
**  Run BDC For The Operation's Data Update.
  CALL TRANSACTION 'CA32'  USING it_fbdcdata
                           UPDATE 'S'
                           MODE   'N'
                           MESSAGES INTO it_msg_tab.
  IF sy-subrc <> 0.
    p_flg = 'X' .
    CONCATENATE 'Update Process Failed!! -'
                'Work Center :'
                p_id
      INTO l_text.
    CONCATENATE ',' ' Control Key :'
                p_steus
                ' - There is a Problem in SAP.'
      INTO l_text_2.
    MESSAGE s001 WITH l_text l_text_2.
  ENDIF.
**  Set BDC Data.
  CLEAR: it_msg_tab, it_msg_tab[],
         it_fbdcdata, it_fbdcdata[].
  PERFORM make_fbdcdata_for_upd_wc USING
                                     l_werks      "Plant
                                     p_id         "Work Center
                                     p_rp_point.  "Sort Str.
**  Run BDC For The Work Center's Data Update.
  CALL TRANSACTION 'CR02'  USING it_fbdcdata
                           UPDATE 'S'
                           MODE   'N'
                           MESSAGES INTO it_msg_tab.
  IF sy-subrc <> 0.
    p_flg = 'X' .
    CONCATENATE 'Udate Process Failed!! -'
                'Work Center :'
                p_id
      INTO l_text.
    CONCATENATE ',' ' Control Key :'
                p_steus
                ' - There is a Problem in SAP.'
      INTO l_text_2.
    MESSAGE s001 WITH l_text l_text_2.
  ENDIF.
*  PERFORM bdc_close_group.

ENDFORM.                    " update_bac
*&---------------------------------------------------------------------*
*&      Form  create_new_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ARBPL  Work Center
*      -->P_P_STEUS  Control
*      -->P_P_RP_POINT  Sort String
*      -->P_P_PRVBE  Supply Area
*      -->P_P_USR01  B/F Point
*      -->P_P_USR02  Not Defined.
*      <--P_P_FLG  Check Flag
*----------------------------------------------------------------------*
FORM create_new_line USING    p_arbpl
                              p_steus
                              p_sortb
                              p_prvbe
                              p_usr01
                              p_usr02
                     CHANGING p_flg.
  DATA: l_werks TYPE crhd-werks ,
        l_text(100),
        l_text_2(100).
  PERFORM check_arbpl USING p_arbpl
                      CHANGING p_flg .
  IF p_flg <> space.
    EXIT.
  ENDIF.
*  PERFORM bdc_open_group.
  CLEAR: it_fbdcdata, it_fbdcdata[].
**  Set BDC Data.
  CLEAR: it_msg_tab,  it_msg_tab[],
         it_fbdcdata, it_fbdcdata[].
  PERFORM make_fbdcdata_for_ins USING p_arbpl
                                      p_steus
                                      p_sortb
                                      p_prvbe
                                      p_usr01
                                      p_usr02
                                CHANGING l_werks .
**  Run BDC For The Operation's Data Update.
  CALL TRANSACTION 'CA32'  USING it_fbdcdata
                           UPDATE 'S'
                           MODE   'N'
                           MESSAGES INTO it_msg_tab.
  IF sy-subrc <> 0.
    p_flg = 'X' .
    CONCATENATE 'Insert Process Failed!! -'
                'Work Center :'
                p_arbpl
      INTO l_text.
    CONCATENATE ',' ' Control Key :'
                p_steus
                ' - There is a Problem in SAP.'
      INTO l_text_2.
    MESSAGE s001 WITH l_text l_text_2.
  ENDIF.

**  Set BDC Data.
  CLEAR: it_msg_tab, it_msg_tab[],
         it_fbdcdata, it_fbdcdata[].
  PERFORM make_fbdcdata_for_upd_wc USING l_werks    "Plant
                                         p_arbpl  "Work Center
                                         p_sortb.  "Sort Str.
**  Run BDC For The Work Center's Data Update.
  CALL TRANSACTION 'CR02'  USING it_fbdcdata
                           UPDATE 'S'
                           MODE   'N'
                           MESSAGES INTO it_msg_tab.
  IF sy-subrc <> 0.
    p_flg = 'X' .
    CONCATENATE 'Insert Process Failed!! -'
                'Work Center :'
                p_arbpl
      INTO l_text.
    CONCATENATE ',' ' Control Key :'
                p_steus
                ' - There is a Problem in SAP.'
      INTO l_text_2.
    MESSAGE s001 WITH l_text l_text_2.
  ENDIF.
*  PERFORM bdc_close_group.

ENDFORM.                    " create_new_line
*&---------------------------------------------------------------------*
*&      Form  check_arbpl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ARBPL  text
*      <--P_P_FLG  text
*----------------------------------------------------------------------*
FORM check_arbpl USING    p_arbpl
                 CHANGING p_flg.
  DATA: l_arbpl TYPE crhd-arbpl.
  SELECT SINGLE arbpl
    INTO l_arbpl
    FROM crhd
    WHERE objty = 'A' AND  "Work Center
          arbpl = p_arbpl .
  IF sy-subrc <> 0.
    p_flg = 'X'.
  ENDIF.

ENDFORM.                    " check_arbpl
*&---------------------------------------------------------------------*
*&      Form  make_fbdcdata_for_ins
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ARBPL  text
*      -->P_P_STEUS  text
*      -->P_P_SORTB  text
*      -->P_P_PRVBE  text
*      -->P_P_USR01  text
*      -->P_P_USR02  text
*      <--P_L_WERKS  text
*----------------------------------------------------------------------*
FORM make_fbdcdata_for_ins USING    p_arbpl
                                    p_steus
                                    p_sortb
                                    p_prvbe
                                    p_usr01
                                    p_usr02
                           CHANGING p_werks.
  DATA: l_vornr TYPE plpo-vornr,
        l_date_c(10).
  CLEAR l_vornr.

  SELECT SINGLE werks vornr
    INTO (p_werks, l_vornr)
    FROM  plpo
    WHERE vornr =  ( SELECT MAX( pp~vornr )
                       FROM ( plpo AS pp
                            INNER JOIN plas AS pa
                              ON pp~plnty = pa~plnty AND
                                 pp~plnnr = pa~plnnr AND
                                 pp~plnkn = pa~plnkn AND
                                 pp~zaehl = pa~zaehl    )
                       WHERE pp~plnty = 'M'  AND
                             pp~plnnr = 'RP' AND
                             pa~loekz <> 'X'    ) .
  l_vornr = l_vornr + 10 .

  WRITE: sy-datum TO l_date_c.

********************************************************
*** Setting For Data Insert.
********************************************************
  PERFORM bdc_dynpro      USING 'SAPLCPDI' '1001'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RC271-STTAG'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VOUE'.
  PERFORM bdc_field       USING 'RC271-PLNNR'
                                'RP'.
  PERFORM bdc_field       USING 'RC271-STTAG'
                                l_date_c.
  PERFORM bdc_field       USING 'RC271-WERKS'
                                p_werks.
*perform bdc_field       using 'RC271-PLNAL'
*                              record-PLNAL_004.

  PERFORM bdc_dynpro      USING 'SAPLCPDI' '5400'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'PLPOD-VORNR(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=EINF'.
  PERFORM bdc_field       USING 'RC27X-ENTRY_ACT'
                                '1'.
  PERFORM bdc_field       USING 'RC27X-FLG_SEL(01)'
                                'X'.

  PERFORM bdc_dynpro      USING 'SAPLCPDI' '5400'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'PLPOD-VORNR(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VOD1'.
  PERFORM bdc_field       USING 'RC27X-ENTRY_ACT'
                                '1'.
  PERFORM bdc_field       USING 'RC27X-FLG_SEL(01)'
                                'X'.
  PERFORM bdc_field       USING 'RC27X-FLG_SEL(02)'
                                ' '.
  PERFORM bdc_field       USING 'PLPOD-VORNR(01)'
                                l_vornr.
  PERFORM bdc_field       USING 'PLPOD-ARBPL(01)'
                                p_arbpl.
  PERFORM bdc_field       USING 'PLPOD-STEUS(01)'
                                p_steus.
  PERFORM bdc_field       USING 'PLPOD-LTXA1(01)'
                                'Made By Z-Program'.
  PERFORM bdc_field       USING 'PLPOD-BMSCH(01)'
                                '1'.
  PERFORM bdc_field       USING 'PLPOD-MEINH(01)'
                                'EA'.

  PERFORM bdc_dynpro      USING 'SAPLCPDO' '1200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'PLPOD-SLWID'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'PLPOD-SLWID'
                                'SA-BF'.

  PERFORM bdc_dynpro      USING 'SAPLCPDO' '1200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'PLPOD-USR00'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.
  PERFORM bdc_field       USING 'PLPOD-USR00'
                                p_prvbe.
  PERFORM bdc_field       USING 'PLPOD-USR01'
                                p_usr01.

ENDFORM.                    " make_fbdcdata_for_ins
*&---------------------------------------------------------------------*
*&      Form  create_routing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_routing CHANGING p_flg.
  DATA: l_text(100),
        l_text_2(100).
*  PERFORM bdc_open_group.
  CLEAR: it_fbdcdata, it_fbdcdata[].
**  Set BDC Data.
  CLEAR: it_msg_tab,  it_msg_tab[],
         it_fbdcdata, it_fbdcdata[].
  PERFORM make_bdcdata_for_new CHANGING p_flg .
**  Run BDC For The Operation's Creation.
  CALL TRANSACTION 'CA31'  USING it_fbdcdata
                           UPDATE 'S'
                           MODE   'N'
                           MESSAGES INTO it_msg_tab.
*  PERFORM bdc_close_group.
  IF sy-subrc <> 0.
    p_flg = 'X' .
    CONCATENATE 'Insert Process Failed!! -'
                'T Code :'
                ' CA31.'
      INTO l_text.
    MESSAGE s000 WITH l_text.
  ENDIF.
ENDFORM.                    " create_routing
*&---------------------------------------------------------------------*
*&      Form  make_bdcdata_for_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_FLG  text
*----------------------------------------------------------------------*
FORM make_bdcdata_for_new CHANGING p_flg.
  DATA: l_date_c(10).
  WRITE sy-datum TO l_date_c.
********************************************************
*** Setting For Routing Creation.
********************************************************
  PERFORM bdc_dynpro      USING 'SAPLCPDI' '1001'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RC271-PLNNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RC271-PLNNR'
                                'RP'.
  PERFORM bdc_field       USING 'RC271-STTAG'
                                l_date_c.
  PERFORM bdc_field       USING 'RC271-WERKS'
                                'P001'.

  PERFORM bdc_dynpro      USING 'SAPLCPDA' '1200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'PLKOD-PLNAL'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.
  PERFORM bdc_field       USING 'PLKOD-PLNAL'
                                '1'.
  PERFORM bdc_field       USING 'PLKOD-WERKS'
                                'P001'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'PLKOD-PLNME'.
  PERFORM bdc_field       USING 'PLKOD-VERWE'
                                '1'.
  PERFORM bdc_field       USING 'PLKOD-STATU'
                                '4'.
  PERFORM bdc_field       USING 'PLKOD-LOSBS'
                                '99,999,999.000'.
  PERFORM bdc_field       USING 'PLKOD-PLNME'
                                'EA'.

ENDFORM.                    " make_bdcdata_for_new
*&---------------------------------------------------------------------*
*&      Form  sort_ascending
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_ascending.
  DATA: field_name01(40),
        offset01 TYPE i.
*
  GET CURSOR FIELD field_name01.
*
  IF field_name01(06) = 'IT_APP'.
    SEARCH field_name01 FOR '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    SORT IT_app802 ASCENDING BY (field_name01).
  ENDIF.

ENDFORM.                    " sort_ascending
*&---------------------------------------------------------------------*
*&      Form  sort_descending
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_descending.
  DATA: field_name01(40),
        offset01 TYPE i.
*
  GET CURSOR FIELD field_name01.
*
  IF field_name01(06) = 'IT_APP'.
    SEARCH field_name01 FOR '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    SORT IT_app802 DESCENDING BY (field_name01).
  ENDIF.

ENDFORM.                    " sort_descending
*&---------------------------------------------------------------------*
*&      Form  set_field_ID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_id.
  SELECT DISTINCT id
    INTO id_value-key
    FROM ztpp_status.
    APPEND id_value TO id_list.
  ENDSELECT.

ENDFORM.                    " set_field_ID
