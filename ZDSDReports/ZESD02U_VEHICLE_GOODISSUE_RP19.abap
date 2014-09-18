************************************************************************
* Program Name      : ZESD02U_VEHICLE_GOODISSUE
* Author            : jun ho choi
* Creation Date     : 2003.09.01.
* Specifications By : jun ho choi
* Pattern           : 3-1
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : Vehicle Good Issue Interface
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 06/15/2005  chris       UD1K916506  prevent to create good issue if no
*                                     good reveipt quantity.
*
*
************************************************************************
REPORT zesd02u_vehicle_goodissue NO STANDARD PAGE HEADING
                                 MESSAGE-ID zmsd
                                 LINE-SIZE 168.


*
TABLES : ztpp_bfst,
         vbfa,
         cabn,
         ausp,
         usr01,
         ztsd_veh_log.

**--- insert by stlim (2004/05/24)
TABLES : vbak.
**--- end of stlim (2004/05/24)

*
DATA : BEGIN OF it_bfst OCCURS 0.
        INCLUDE STRUCTURE ztpp_bfst.
DATA : END OF it_bfst.

DATA : BEGIN OF bdc_tab OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF bdc_tab.

DATA : BEGIN OF mess_tab OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF mess_tab.

DATA : BEGIN OF bdc_list OCCURS 0,
       gubun_d(1),
       gubun_b(1),
       message(75),

       vin_num LIKE ztpp_bfst-vin_num,
       END OF bdc_list.

DATA : w_cnt TYPE i,
       w_objek LIKE ausp-objek,
       w_vbeln LIKE vbak-vbeln,
       w_n_8(8) TYPE n,
       w_c_8(8),
       w_c_8_gi(8),
       w_dealer(5),
       w_cnt_s TYPE i,
       w_cnt_e TYPE i,
       w_index LIKE sy-tabix.


*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS : p_mode(1) DEFAULT 'N'.
SELECTION-SCREEN END OF BLOCK b1.


*
TOP-OF-PAGE.
  PERFORM top_of_page.


*
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM top_of_page.


*
START-OF-SELECTION.
  SET PF-STATUS 'ESD02U'.
  PERFORM get_data.
  PERFORM bdc_process.
  PERFORM display_result.


*
END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM get_data.
  REFRESH : it_bfst.
  CLEAR   : it_bfst.

  SELECT *
         INTO TABLE it_bfst
         FROM ztpp_bfst
        WHERE bfp18_flg   EQ 'Y'
        AND   sd_deli_flg EQ ''.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM bdc_process.
 data: l_wemng like plaf-wemng.

  DESCRIBE TABLE it_bfst LINES w_cnt.
  IF w_cnt = 0.
    SKIP 5.
    WRITE:/ 'No Entry'.
    STOP.
  ENDIF.

  LOOP AT it_bfst.
**    PERFORM SAPGUI_PROGRESS_INDICATOR USING 1.

* Requested by Lance Younce changed by chris
* if no good recieve, no good issue should be created.
   select single wemng into l_wemng
     from plaf
     where plnum = it_bfst-PLAN_ORD.
   if l_wemng = 0.
     continue.
   endif.

* end of change on 06/15/2005

    PERFORM get_delivery_no.
    IF w_vbeln = ''. "DEL. NO
      CONTINUE.
    ELSE.
      SELECT SINGLE * FROM vbfa
             WHERE vbelv = w_vbeln
             AND   vbtyp_n = 'R'
             AND   vbtyp_v = 'J'.
      IF sy-subrc = 0. "G/I ALREADY
        CONTINUE.
      ENDIF.
    ENDIF.

    PERFORM get_rp19_a_vm.
    IF w_c_8 = ''.
      CONTINUE.
    ENDIF.

    PERFORM get_dealer_vm.

***--- insert by stlim (2004/05/25)
*    IF w_dealer+0(3) = 'B06'.
*      CLEAR : vbak.
*      SELECT SINGLE auart INTO vbak-auart
*                          FROM vbak
*                         WHERE vbeln EQ it_bfst-vin_num.
*      IF vbak-auart EQ 'ZVTO'.
*      ELSE.
*        PERFORM update_bfst.
*        CONTINUE.
*      ENDIF.
*    ENDIF.
***--- end of insert

*--- blocked by stlim (2004/05/25)
** Changed by Furong on 04/21/10
    IF w_dealer+0(3) <> 'B28' OR w_dealer+3(1) = 'X'.
*    IF w_dealer+0(3) = 'B06' OR w_dealer+3(1) = 'X'.
** End of change on 04/21/10
      PERFORM update_bfst.
      CONTINUE.
    ENDIF.

*-- Test vehicle can be used name as 'T' for the future ,
*-- Please discuss with SD Team.
*--- end of block

    REFRESH : bdc_tab, mess_tab.
    CLEAR   : bdc_tab, mess_tab.

    PERFORM move_it_bfst_2_bdc_list.

    PERFORM get_rp19_s_vm.
    PERFORM bdc_fill USING :
            'X' 'SAPMV50A'             '4004',
            ' ' 'LIKP-VBELN'           w_vbeln,
            ' ' 'BDC_OKCODE'           '/00',
            'X' 'SAPMV50A'             '1000',
            ' ' 'LIKP-WADAT_IST'       w_c_8_gi,
            ' ' 'BDC_OKCODE'           '=WABU_T'.

    CALL TRANSACTION 'VL02N' USING bdc_tab MODE p_mode
                                   UPDATE 'S'
                                   MESSAGES INTO mess_tab.
    READ TABLE mess_tab WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      bdc_list-gubun_d = 'E'.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                msgid               = mess_tab-msgid
                msgnr               = mess_tab-msgnr
                msgv1               = mess_tab-msgv1
                msgv2               = mess_tab-msgv2
                msgv3               = mess_tab-msgv3
                msgv4               = mess_tab-msgv4
           IMPORTING
                message_text_output = bdc_list-message.
      ztsd_veh_log-zresult = 'E'.
      ztsd_veh_log-zresult2 = ''.
      ztsd_veh_log-zmessage = bdc_list-message.
      MODIFY ztsd_veh_log.
    ELSE.
      READ TABLE mess_tab WITH KEY msgtyp = 'S'
                                   msgid  = 'VL'
                                   msgnr  = '311'.
      IF sy-subrc = 0.
        bdc_list-gubun_d = 'S'.
        bdc_list-message = ''.
        ztsd_veh_log-zresult = 'S'.
        ztsd_veh_log-zmessage = ''.
        PERFORM update_bfst.
        IF sy-subrc = 0.
          bdc_list-gubun_b = 'S'.
          ztsd_veh_log-zresult2 = 'S'.
          ztsd_veh_log-zmessage = ''.
        ELSE.
          bdc_list-gubun_b = 'E'.
          ztsd_veh_log-zresult2 = 'E'.
          ztsd_veh_log-zmessage = 'UPDATE ERROR'.
        ENDIF.
      ELSE.
        bdc_list-gubun_d = 'E'.
        ztsd_veh_log-zresult = 'E'.
        READ TABLE mess_tab INDEX 1.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
             EXPORTING
                  msgid               = mess_tab-msgid
                  msgnr               = mess_tab-msgnr
                  msgv1               = mess_tab-msgv1
                  msgv2               = mess_tab-msgv2
                  msgv3               = mess_tab-msgv3
                  msgv4               = mess_tab-msgv4
             IMPORTING
                  message_text_output = bdc_list-message.
        ztsd_veh_log-zmessage = bdc_list-message.
      ENDIF.
      MODIFY ztsd_veh_log. CLEAR ztsd_veh_log.
    ENDIF.

    APPEND bdc_list. CLEAR bdc_list.
  ENDLOOP.
ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SAPGUI_PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
FORM sapgui_progress_indicator USING gubun.
**DATA : W_PERC TYPE P DECIMALS 2,
**       W_TEXT(50).
**
**  W_PERC = SY-TABIX / W_CNT * 100.
**  WRITE W_PERC TO W_TEXT+0(7).
**  CASE GUBUN.
**  WHEN '1'.
**    CONCATENATE W_TEXT 'Posting good issue'
**                INTO W_TEXT SEPARATED BY SPACE.
**  ENDCASE.
**
**  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
**    EXPORTING
**      PERCENTAGE       = W_PERC
**      TEXT             = W_TEXT.
ENDFORM.                    " SAPGUI_PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  GET_DELIVERY_NO
*&---------------------------------------------------------------------*
FORM get_delivery_no.
*  SELECT SINGLE *
*         FROM CABN
*        WHERE ATNAM = 'P_VIN'.
*  SELECT SINGLE *
*         FROM AUSP
*        WHERE ATINN EQ CABN-ATINN
*        AND   ATWRT EQ IT_BFLOG-VIN.
*  IF SY-SUBRC = 0.
*    W_OBJEK = AUSP-OBJEK.
*  ENDIF.
  w_objek = it_bfst-vin_num. "BODY NO

**  SELECT SINGLE *
**         FROM CABN
**        WHERE ATNAM = 'P_DELIVERY_NO'.
**  SELECT SINGLE *
**         FROM AUSP
**        WHERE OBJEK EQ W_OBJEK
**        AND   ATINN EQ CABN-ATINN.
**  IF SY-SUBRC = 0.
**    W_VBELN = AUSP-ATWRT.
**  ELSE.
**    CLEAR W_VBELN.
**  ENDIF.

  w_vbeln = it_bfst-vin_num.
ENDFORM.                    " GET_DELIVERY_NO
*&---------------------------------------------------------------------*
*&      Form  GET_RP19_A_VM
*&---------------------------------------------------------------------*
FORM get_rp19_a_vm.
  SELECT SINGLE *
         FROM cabn
        WHERE atnam = 'P_RP19_ACTUAL_DATE'.
  SELECT SINGLE *
         FROM ausp
        WHERE objek EQ w_objek
        AND   atinn EQ cabn-atinn.
  IF sy-subrc = 0.
    w_c_8 = ausp-atwrt+0(8).
  ELSE.
    CLEAR w_c_8.
  ENDIF.
ENDFORM.                    " GET_RP19_A_VM
*&---------------------------------------------------------------------*
*&      Form  GET_DEALER_VM
*&---------------------------------------------------------------------*
FORM get_dealer_vm.
  w_objek = it_bfst-vin_num. "BODY NO

  SELECT SINGLE *
         FROM cabn
        WHERE atnam = 'P_DESTINATION_CODE'.
  SELECT SINGLE *
         FROM ausp
        WHERE objek EQ w_objek
        AND   atinn EQ cabn-atinn.
  IF sy-subrc = 0.
    w_dealer = ausp-atwrt.
  ELSE.
    CLEAR w_dealer.
  ENDIF.
ENDFORM.                    " GET_DEALER_VM
*&---------------------------------------------------------------------*
*&      Form  MOVE_IT_BFST_2_BDC_LIST
*&---------------------------------------------------------------------*
FORM move_it_bfst_2_bdc_list.
  ztsd_veh_log-mandt = sy-mandt.
  ztsd_veh_log-zdate = sy-datum.
  ztsd_veh_log-ztime = sy-uzeit.
  ztsd_veh_log-zgubun = 'B'.
  ztsd_veh_log-zkey = it_bfst-vin_num.

  bdc_list-vin_num = it_bfst-vin_num.
ENDFORM.                    " MOVE_IT_BFVIN_2_BDC_LIST
*&---------------------------------------------------------------------*
*&      Form  GET_RP19_S_VM
*&---------------------------------------------------------------------*
FORM get_rp19_s_vm.
  SELECT SINGLE *
         FROM cabn
        WHERE atnam = 'P_RP19_SHOP_DATE'.
  SELECT SINGLE *
         FROM ausp
        WHERE objek EQ w_objek
        AND   atinn EQ cabn-atinn.
  w_n_8 = ausp-atflv.
  IF w_n_8 NE '00000000'.
    SELECT SINGLE *
           FROM usr01
          WHERE bname = sy-uname.
    CASE usr01-datfm.
      WHEN '1'. "DD.MM.YYYY
        w_c_8_gi+4(4) = w_n_8+0(4).
        w_c_8_gi+2(2) = w_n_8+4(2).
        w_c_8_gi+0(2) = w_n_8+6(2).
      WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
        w_c_8_gi+4(4) = w_n_8+0(4).
        w_c_8_gi+0(2) = w_n_8+4(2).
        w_c_8_gi+2(2) = w_n_8+6(2).
    ENDCASE.
  ELSE.
    SELECT SINGLE *
           FROM usr01
          WHERE bname = sy-uname.
    CASE usr01-datfm.
      WHEN '1'. "DD.MM.YYYY
        w_c_8_gi+4(4) = sy-datum+0(4).
        w_c_8_gi+2(2) = sy-datum+4(2).
        w_c_8_gi+0(2) = sy-datum+6(2).
      WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
        w_c_8_gi+4(4) = sy-datum+0(4).
        w_c_8_gi+0(2) = sy-datum+4(2).
        w_c_8_gi+2(2) = sy-datum+6(2).
    ENDCASE.
  ENDIF.
ENDFORM.                    " GET_RP19_S_VM
*&---------------------------------------------------------------------*
*&      Form  BDC_FILL
*&---------------------------------------------------------------------*
FORM bdc_fill USING    p1 p2 p3.
  CLEAR bdc_tab.
  IF p1 = 'X'.
    bdc_tab-dynbegin = p1.
    bdc_tab-program  = p2.
    bdc_tab-dynpro   = p3.
  ELSE.
    bdc_tab-dynbegin = p1.
    bdc_tab-fnam     = p2.
    bdc_tab-fval     = p3.
  ENDIF.
  APPEND bdc_tab.
ENDFORM.                    " BDC_FILL
*&---------------------------------------------------------------------*
*&      Form  UPDATE_BFST
*&---------------------------------------------------------------------*
FORM update_bfst.
  UPDATE ztpp_bfst SET : sd_deli_flg = 'Y'
                         sd_deli_dat = sy-datum
                         sd_deli_tim = sy-uzeit
                   WHERE vin_num = it_bfst-vin_num.
ENDFORM.                    " UPDATE_BFST
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULT
*&---------------------------------------------------------------------*
FORM display_result.
  WRITE:/ ''.
  LOOP AT bdc_list.
    WRITE:/ sy-vline.
    CASE bdc_list-gubun_d.
      WHEN 'S'.
        FORMAT COLOR COL_POSITIVE.
        WRITE: (02) bdc_list-gubun_d.
        FORMAT COLOR COL_POSITIVE OFF.
      WHEN 'E'.
        FORMAT COLOR COL_NEGATIVE.
        WRITE: (02) bdc_list-gubun_d.
        FORMAT COLOR COL_NEGATIVE OFF.
      WHEN OTHERS.
        WRITE: (02) ''.
    ENDCASE.

    WRITE:  sy-vline.
    CASE bdc_list-gubun_b.
      WHEN 'S'.
        FORMAT COLOR COL_POSITIVE.
        WRITE: (02) bdc_list-gubun_b.
        FORMAT COLOR COL_POSITIVE OFF.
      WHEN 'E'.
        FORMAT COLOR COL_NEGATIVE.
        WRITE: (02) bdc_list-gubun_b.
        FORMAT COLOR COL_NEGATIVE OFF.
      WHEN OTHERS.
        WRITE: (02) ''.
    ENDCASE.

    WRITE:  sy-vline, (18) bdc_list-vin_num,
            sy-vline, (75) bdc_list-message,
            sy-vline.

    w_index = sy-tabix.
    HIDE : w_index.

    WRITE:/(110) sy-uline.
  ENDLOOP.
ENDFORM.                    " DISPLAY_RESULT
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  DESCRIBE TABLE bdc_list LINES w_cnt.
  WRITE:/ 'Total   records :', w_cnt.

  w_cnt_s = 0. w_cnt_e = 0.
  LOOP AT bdc_list.
    IF bdc_list-gubun_d = 'S'.
      w_cnt_s = w_cnt_s + 1.
    ENDIF.
    IF bdc_list-gubun_d <> 'S'.
      w_cnt_e = w_cnt_e + 1.
    ENDIF.
  ENDLOOP.
  WRITE:/ 'Success records :', w_cnt_s.
  WRITE:/ 'Error   records :', w_cnt_e.

  FORMAT COLOR COL_HEADING.
  WRITE:/(110) sy-uline.
  WRITE:/ sy-vline, (02) 'GI',
          sy-vline, (02) 'BF',
          sy-vline, (18) 'Equipment No(VIN)',
          sy-vline, (75) 'Message',
          sy-vline.
  WRITE:/(110) sy-uline.
  FORMAT COLOR COL_HEADING OFF.
ENDFORM.                    " TOP_OF_PAGE
