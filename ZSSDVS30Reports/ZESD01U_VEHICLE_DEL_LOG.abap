************************************************************************
* Program Name      : ZESD01U_VEHICLE_DEL_LOG
* Author            : jun ho choi
* Creation Date     : 2004.03.18.
* Specifications By : jun ho choi
* Pattern           : 3-1
* Development Request No : UD1K908261
* Addl Documentation:
* Description       : Vehicle Delivery Log
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 01/11/2005 wskim                     issue#20050107-003
* delivery document creation error when ALC Program running
* 06/16/2005 chris        UP1K916467   Change for RP18 reporting point
*                                      check.
*
************************************************************************
REPORT zesd01u_vehicle_del_log NO STANDARD PAGE HEADING
                               MESSAGE-ID zmsd
                               LINE-SIZE 95.
*
TABLES : ztsd_veh_log, ausp.

*
DATA : BEGIN OF it_log OCCURS 0.
        INCLUDE STRUCTURE ztsd_veh_log.
DATA : END OF it_log.

DATA : w_cnt TYPE i,
       w_objek LIKE ausp-objek,
       w_vbeln LIKE vbak-vbeln,
       w_n_8(8) TYPE n,
       w_c_8(8),
       w_c_8_gi(8),
       w_dealer(5),
       w_cnt_s TYPE i,
       w_cnt_e TYPE i,
       w_index LIKE sy-tabix,
       www(1) VALUE 'N'.
*
TABLES : vepvg, usr01.

DATA : BEGIN OF bdc_tab OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF bdc_tab.

DATA : BEGIN OF mess_tab OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF mess_tab.

DATA : w_date(8) TYPE n.

***************************************************************
*Selection-screen
***************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_zdate FOR ztsd_veh_log-zdate,
                 s_zkey  FOR ztsd_veh_log-zkey.
SELECTION-SCREEN SKIP 1.
PARAMETERS : p_all RADIOBUTTON GROUP radi,
             p_s   RADIOBUTTON GROUP radi,
             p_e   RADIOBUTTON GROUP radi.
*Issue # 20050107-003 requested by YKKO
*Changed by wskim,on 01112005
*-----Start
PARAMETERS : p_b   RADIOBUTTON GROUP radi,
             m_mode TYPE c DEFAULT 'N'.
*-----End
SELECTION-SCREEN END OF BLOCK b1.
***************************************************************
*
***************************************************************
TOP-OF-PAGE.
  PERFORM top_of_page.
*
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM top_of_page.
***************************************************************
*START-OF-SELECTION
***************************************************************
START-OF-SELECTION.
*Issue # 20050107-003 requested by YKKO
*Changed by wskim,on 01112005
*-----Start
  IF p_b <> 'X'.
    SET PF-STATUS 'ESD01U'.

*-----End
    PERFORM get_data.
    PERFORM display_message.
    PERFORM display_result.
*-----Start
  ELSE.
    SET PF-STATUS 'ESD03U'.
    PERFORM batch_process.
  ENDIF.
*-----End
*
END-OF-SELECTION.


*
AT USER-COMMAND.
  PERFORM user_command.


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM get_data.
  REFRESH : it_log.
  CLEAR   : it_log.

  CASE 'X'.
    WHEN p_all.
      SELECT *
             INTO TABLE it_log
             FROM ztsd_veh_log
            WHERE zgubun EQ 'D'
            AND   zdate IN s_zdate
            AND   zkey  IN s_zkey.
    WHEN p_s.
      SELECT *
             INTO TABLE it_log
             FROM ztsd_veh_log
            WHERE zgubun EQ 'D'
            AND   zdate IN s_zdate
            AND   zkey  IN s_zkey
            AND   zresult EQ 'S'.
    WHEN p_e.
      SELECT *
             INTO TABLE it_log
             FROM ztsd_veh_log
            WHERE zgubun EQ 'D'
            AND   zdate IN s_zdate
            AND   zkey  IN s_zkey
            AND   zresult EQ 'E'.
  ENDCASE.


* requested by Mr.Ko, changed by chris
* to create delivery, all vehicle must pass RP18.
  PERFORM check_vehicle_status.

* end of change on 06/16/2005
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULT
*&---------------------------------------------------------------------*
FORM display_result.

  WRITE:/ ''.
  LOOP AT it_log.
    WRITE:/ sy-vline, (10) it_log-zdate,
            sy-vline, (08) it_log-ztime,
            sy-vline, (10) it_log-zkey,
            sy-vline.

    CASE it_log-zresult.
      WHEN 'S'.
        FORMAT COLOR COL_POSITIVE.
        WRITE: (01) it_log-zresult, sy-vline.
        FORMAT COLOR COL_POSITIVE OFF.
      WHEN 'E'.
        FORMAT COLOR COL_NEGATIVE.
        WRITE: (01) it_log-zresult, sy-vline.
        FORMAT COLOR COL_NEGATIVE OFF.
    ENDCASE.

    WRITE:            (50) it_log-zmessage,
            sy-vline.

    w_index = sy-tabix.
    HIDE : w_index.

    WRITE:/(95) sy-uline.
  ENDLOOP.
ENDFORM.                    " DISPLAY_RESULT
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  DESCRIBE TABLE it_log LINES w_cnt.
  WRITE:/ 'Total   records :', w_cnt.

  w_cnt_s = 0. w_cnt_e = 0.
  LOOP AT it_log.
    IF it_log-zresult = 'S'.
      w_cnt_s = w_cnt_s + 1.
    ENDIF.
    IF it_log-zresult = 'E'.
      w_cnt_e = w_cnt_e + 1.
    ENDIF.
  ENDLOOP.
  WRITE:/ 'Success records :', w_cnt_s.
  WRITE:/ 'Error   records :', w_cnt_e.

  FORMAT COLOR COL_HEADING.
  WRITE:/(95) sy-uline.
  WRITE:/ sy-vline, (10) '   Date   ',
          sy-vline, (08) '  Time  ',
          sy-vline, (10) '   Key    ',
          sy-vline, (01) 'D',
          sy-vline, (50) 'Message',
          sy-vline.
  WRITE:/(95) sy-uline.
  FORMAT COLOR COL_HEADING OFF.
ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command.
  DATA : ok_code(4).
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.

  CASE ok_code.
    WHEN 'BACK'.
      SET SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'REST'.
      PERFORM restarting.
      sy-lsind = sy-lsind - 1.
      PERFORM display_result.
  ENDCASE.
ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  RESTARTING
*&---------------------------------------------------------------------*
FORM restarting.
  IF sy-lisel+26(1) EQ ' '.
    MESSAGE i000 WITH text-m03.
    EXIT.
  ENDIF.

  IF sy-lisel+15(1) NE ' '.
    READ TABLE it_log INDEX w_index.
    IF sy-subrc = 0.
      IF it_log-zresult EQ 'E'.
        PERFORM call_func.
        PERFORM get_data.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE i000 WITH text-m02.
  ENDIF.
ENDFORM.                    " RESTARTING
*&---------------------------------------------------------------------*
*&      Form  CALL_FUNC
*&---------------------------------------------------------------------*
FORM call_func.
**  CALL FUNCTION 'Z_FSD_VEHICLE_DELIVERY'
**    EXPORTING
**      EQUNO                     = IT_LOG-ZKEY
***   IMPORTING
***     DELIVERY_NO               =
***   EXCEPTIONS
***     NOT_FOUND_VIN             = 1
***     NOT_FOUND_SALES           = 2
***     NOT_FOUND_VSTEL           = 3
***     NOT_CREATE_DELIVERY       = 4
***     NOT_UPDATE_DELIVERY       = 5
***     OTHERS                    = 6
**          .
  PERFORM get_sales_no.
  CHECK sy-subrc = 0.
  PERFORM bdc_vl01n.
ENDFORM.                    " CALL_FUNC
*&---------------------------------------------------------------------*
*&      Form  GET_SALES_NO
*&---------------------------------------------------------------------*
FORM get_sales_no.
  TABLES : cabn.
*Issue # 20050107-003 requested by YKKO
*Changed by wskim,on 01112005
*-----Start
  IF p_b <> 'X'.
    SELECT SINGLE *
           FROM cabn
          WHERE atnam = 'P_SALES_ORDER'.
    SELECT SINGLE *
           FROM ausp
          WHERE objek EQ it_log-zkey
          AND   atinn EQ cabn-atinn.
    IF sy-subrc <> 0.
      it_log-zresult = 'E'.
      it_log-zmessage = 'NOT_FOUND_SALES'.
      MODIFY it_log INDEX w_index.
      MESSAGE s000 WITH it_log-zmessage DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
*-----Start
  ELSE.
    SELECT SINGLE *
          FROM cabn
         WHERE atnam = 'P_SALES_ORDER'.
    SELECT SINGLE *
           FROM ausp
          WHERE objek EQ it_log-zkey
          AND   atinn EQ cabn-atinn.
    IF sy-subrc <> 0.
      it_log-zresult = 'E'.
      it_log-zmessage = 'NOT_FOUND_SALES'.
      MODIFY it_log FROM it_log.
      MESSAGE s000 WITH it_log-zmessage DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.
*-----End
ENDFORM.                    " GET_SALES_NO
*&---------------------------------------------------------------------*
*&      Form  BDC_VL01N
*&---------------------------------------------------------------------*
FORM bdc_vl01n.

*Issue # 20050107-003 requested by YKKO
*Changed by wskim,on 01112005
*-----Start

  IF p_b <> 'X'.
    SELECT SINGLE *
            FROM vepvg
           WHERE vbeln EQ ausp-atwrt.
    IF sy-subrc <> 0.
      it_log-zresult = 'E'.
      it_log-zmessage = 'Cannot found Delivery Due List'.

* by ig.moon 4/5/2012 {
      CONCATENATE sy-uname ':' it_log-zmessage INTO it_log-zmessage.

      it_log-erdat = sy-datum.
      it_log-erzet = sy-uzeit.
      it_log-ernam = sy-uname.
* }

      MODIFY it_log INDEX w_index.
      EXIT.
    ENDIF.

    SELECT SINGLE *
           FROM usr01
          WHERE bname = sy-uname.
    CASE usr01-datfm.
      WHEN '1'. "DD.MM.YYYY
        w_date+4(4) = vepvg-ledat+0(4).
        w_date+2(2) = vepvg-ledat+4(2).
        w_date+0(2) = vepvg-ledat+6(2).
      WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
        w_date+4(4) = vepvg-ledat+0(4).
        w_date+0(2) = vepvg-ledat+4(2).
        w_date+2(2) = vepvg-ledat+6(2).
    ENDCASE.

    REFRESH : bdc_tab, mess_tab.
    CLEAR   : bdc_tab, mess_tab.

    PERFORM bdc_fill USING :
            'X' 'SAPMV50A'             '4001',
            ' ' 'LIKP-VSTEL'           vepvg-vstel,
            ' ' 'LV50C-DATBI'          w_date,
            ' ' 'LV50C-VBELN'          ausp-atwrt,
            ' ' 'BDC_OKCODE'           '/00',
            'X' 'SAPMV50A'             '1000',
            ' ' 'LIKP-VBELN'           it_log-zkey,
            ' ' 'LIPSD-G_LFIMG(01)'    '1',
            ' ' 'BDC_OKCODE'           '/00',
            'X' 'SAPMV50A'             '1000',
            ' ' 'BDC_OKCODE'           '=SICH_T'.

    CALL TRANSACTION 'VL01N' USING bdc_tab MODE m_mode
                                   UPDATE 'S'
                                   MESSAGES INTO mess_tab.
    READ TABLE mess_tab WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      it_log-zresult = 'E'.
      it_log-zmessage = 'NOT_CREATE_DELIVERY'.

* by ig.moon 4/5/2012 {
      CONCATENATE sy-uname ':' it_log-zmessage INTO it_log-zmessage.

      it_log-erdat = sy-datum.
      it_log-erzet = sy-uzeit.
      it_log-ernam = sy-uname.
* }
      MODIFY it_log INDEX w_index.
      EXIT.
    ELSE.
      READ TABLE mess_tab WITH KEY msgtyp = 'S'
                                   msgid  = 'VL'
                                   msgnr  = '311'.
      IF sy-subrc = 0.
        it_log-zresult = 'S'.
        it_log-zmessage = ''.

* by ig.moon 4/5/2012 {
        it_log-erdat = sy-datum.
        it_log-erzet = sy-uzeit.
        it_log-ernam = sy-uname.
* }

        MODIFY it_log INDEX w_index.
      ELSE.
        it_log-zresult = 'E'.
        it_log-zmessage = 'NOT_CREATE_DELIVERY'.

* by ig.moon 4/5/2012 {
        CONCATENATE sy-uname ':' it_log-zmessage INTO it_log-zmessage.

        it_log-erdat = sy-datum.
        it_log-erzet = sy-uzeit.
        it_log-ernam = sy-uname.
* }

        MODIFY it_log INDEX w_index.
        EXIT.
      ENDIF.
    ENDIF.
*-----Start
  ELSE.
    SELECT SINGLE *
          FROM vepvg
         WHERE vbeln EQ ausp-atwrt.
    IF sy-subrc <> 0.
      it_log-zresult = 'E'.
      it_log-zmessage = 'Cannot found Delivery Due List'.

* by ig.moon 4/5/2012 {
      CONCATENATE sy-uname ':' it_log-zmessage INTO it_log-zmessage.

      it_log-erdat = sy-datum.
      it_log-erzet = sy-uzeit.
      it_log-ernam = sy-uname.
* }

      MODIFY it_log FROM it_log.
      EXIT.
    ENDIF.

    SELECT SINGLE *
           FROM usr01
          WHERE bname = sy-uname.
    CASE usr01-datfm.
      WHEN '1'. "DD.MM.YYYY
        w_date+4(4) = vepvg-ledat+0(4).
        w_date+2(2) = vepvg-ledat+4(2).
        w_date+0(2) = vepvg-ledat+6(2).
      WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
        w_date+4(4) = vepvg-ledat+0(4).
        w_date+0(2) = vepvg-ledat+4(2).
        w_date+2(2) = vepvg-ledat+6(2).
    ENDCASE.


    REFRESH : bdc_tab, mess_tab.
    CLEAR   : bdc_tab, mess_tab.

    PERFORM bdc_fill USING :
            'X' 'SAPMV50A'             '4001',
            ' ' 'LIKP-VSTEL'           vepvg-vstel,
            ' ' 'LV50C-DATBI'          w_date,
            ' ' 'LV50C-VBELN'          ausp-atwrt,
            ' ' 'BDC_OKCODE'           '/00',
            'X' 'SAPMV50A'             '1000',
            ' ' 'LIKP-VBELN'           it_log-zkey,
            ' ' 'LIPSD-G_LFIMG(01)'    '1',
            ' ' 'BDC_OKCODE'           '/00',
            'X' 'SAPMV50A'             '1000',
            ' ' 'BDC_OKCODE'           '=SICH_T'.

    CALL TRANSACTION 'VL01N' USING bdc_tab MODE m_mode
                                   UPDATE 'S'
                                   MESSAGES INTO mess_tab.
    READ TABLE mess_tab WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      it_log-zresult = 'E'.
      it_log-zmessage = 'NOT_CREATE_DELIVERY'.

* by ig.moon 4/5/2012 {
      CONCATENATE sy-uname ':' it_log-zmessage INTO it_log-zmessage.

      it_log-erdat = sy-datum.
      it_log-erzet = sy-uzeit.
      it_log-ernam = sy-uname.
* }

      MODIFY it_log FROM it_log.
      EXIT.
    ELSE.
      READ TABLE mess_tab WITH KEY msgtyp = 'S'
                                   msgid  = 'VL'
                                   msgnr  = '311'.
      IF sy-subrc = 0.
        it_log-zresult = 'S'.
        it_log-zmessage = ''.

* by ig.moon 4/5/2012 {
        it_log-erdat = sy-datum.
        it_log-erzet = sy-uzeit.
        it_log-ernam = sy-uname.
* }

        MODIFY it_log FROM it_log.
      ELSE.
        it_log-zresult = 'E'.
        it_log-zmessage = 'NOT_CREATE_DELIVERY'.

* by ig.moon 4/5/2012 {
        CONCATENATE sy-uname ':' it_log-zmessage INTO it_log-zmessage.
        it_log-erdat = sy-datum.
        it_log-erzet = sy-uzeit.
        it_log-ernam = sy-uname.
* }

        MODIFY it_log FROM it_log.
        EXIT.
      ENDIF.
    ENDIF.

  ENDIF.
*-----End
*///// Issue : 20041019-001, Changed by BSBAE
  UPDATE ztsd_veh_log FROM it_log.
  IF sy-subrc EQ 0.
    MESSAGE s000 WITH text-m04.
  ELSE.
    MESSAGE e000 WITH text-m05.
  ENDIF.
*///// Issue : 20041019-001, Changed by BSBAE

ENDFORM.                                                    " BDC_VL01N
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
*&      Form  display_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_message.
  DESCRIBE TABLE it_log LINES w_cnt.
  IF w_cnt = 0.
    MESSAGE i000 WITH text-m01.
    EXIT.
  ENDIF.
ENDFORM.                    " display_message
*&---------------------------------------------------------------------*
*&      Form  batch_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM batch_process.
  REFRESH it_log.
  SELECT *
         INTO TABLE it_log
         FROM ztsd_veh_log
        WHERE zgubun EQ 'D'
          AND zresult EQ 'E'
** for SAP tuning
%_HINTS ORACLE 'INDEX (ZTSD_VEH_LOG "ZTSD_VEH_LOG~Z02")'.
** End

  DESCRIBE TABLE it_log LINES w_cnt.
  IF w_cnt = 0.
    MESSAGE i000 WITH text-m01.
    EXIT.
  ELSE.
*   requested by Mr.Ko, changed by chris
*   to create delivery, all vehicle must pass RP18.
    PERFORM check_vehicle_status.
*   end of change on 06/16/2005

    PERFORM restarting_batch.
    PERFORM display_result.
  ENDIF.
ENDFORM.                    " batch_process
*&---------------------------------------------------------------------*
*&      Form  restarting_batch
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM restarting_batch.
  CLEAR it_log.
  LOOP AT it_log.
    PERFORM call_func.
  ENDLOOP.

  SELECT *
         INTO TABLE it_log
         FROM ztsd_veh_log
        WHERE zgubun EQ 'D'
          AND zresult EQ 'E'.
ENDFORM.                    " restarting_batch
*&---------------------------------------------------------------------*
*&      Form  CHECK_VEHICLE_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_vehicle_status.
  DATA: lt_ausp LIKE ausp OCCURS 0 WITH HEADER LINE.
  DATA: l_atinn LIKE cabn-atinn.

  SELECT SINGLE atinn INTO l_atinn
    FROM cabn
    WHERE atnam  = 'P_RP_STATUS'.

  LOOP AT it_log.

    SELECT SINGLE atwrt INTO lt_ausp-atwrt
      FROM ausp
      WHERE klart = '002'
        AND objek = it_log-zkey
        AND atinn = l_atinn.

    IF sy-subrc NE 0 OR
       lt_ausp-atwrt LT '18'.
      DELETE it_log.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_VEHICLE_STATUS
