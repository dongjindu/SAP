*&------------------------------------------------------------------
*& Program ID     : ZMMR90500T
*& Profram Name   : Batch Job Program
*& Created by     : Yang
*& Created on     : 04.29.2008
*& Development ID :
*& Reference Pgm. :
*& Description    : Batch Job Program
*&
*& Modification Log
*&====================================================================
*& Date        Developer      Request ID      Description
*& 04.29.2009   Yang                       First development
*& 04.28.2010   Victor   MM20100329_001 vendor Code: Z_MM_IF_OB_02_001
*&--------------------------------------------------------------------
REPORT zmmr90500t    LINE-SIZE 154 LINE-COUNT 58 MESSAGE-ID zmmm
                      NO STANDARD PAGE HEADING.


TABLES : zmmt0036.
DATA : e_return LIKE zmms0053,
*       it_m091  LIKE zmmt0091 OCCURS 0 WITH HEADER LINE,
*       it_m083  LIKE zmmt0083 OCCURS 0 WITH HEADER LINE,
*       it_m084  LIKE zmmt0084 OCCURS 0 WITH HEADER LINE,
*       it_m073  LIKE zmmt0073 OCCURS 0 WITH HEADER LINE,
       it_m036  LIKE zmmt0036 OCCURS 0 WITH HEADER LINE,"
       it_m048  LIKE zmmt0048 OCCURS 0 WITH HEADER LINE,
       it_m038  LIKE zmmt0038 OCCURS 0 WITH HEADER LINE,
*       it_m067  LIKE zmmt0067 OCCURS 0 WITH HEADER LINE,"
       it_body  LIKE zmmt0051 OCCURS 0 WITH HEADER LINE.

DATA: gv_cnt TYPE i.
*
DATA: BEGIN OF contents OCCURS 0.
        INCLUDE STRUCTURE solisti1.
DATA: END OF contents.
*
DATA: alv_t_cont LIKE solisti1 OCCURS 0 WITH HEADER LINE.
*
DATA: BEGIN OF alv_receivers OCCURS 0.
        INCLUDE STRUCTURE somlreci1.
DATA: END   OF alv_receivers.
*
DATA: BEGIN OF docdata.
        INCLUDE STRUCTURE sodocchgi1.
DATA: END OF docdata.
*
DATA: sent_to_all(1) TYPE c.
*
DATA: object_id LIKE  sofolenti1-object_id.
*
DATA: BEGIN OF obj_header OCCURS 0.
        INCLUDE STRUCTURE solisti1.
DATA: END OF obj_header.
*
TYPE-POOLS: esp1.

SELECTION-SCREEN BEGIN OF BLOCK bk1 WITH FRAME TITLE text-sel.
PARAMETERS: p_r01  RADIOBUTTON GROUP r1 DEFAULT 'X' USER-COMMAND radi,
            p_r02  RADIOBUTTON GROUP r1,
            p_r03  RADIOBUTTON GROUP r1,
            p_r04  RADIOBUTTON GROUP r1,
            p_r05  RADIOBUTTON GROUP r1 MODIF ID glv,
            p_r06  RADIOBUTTON GROUP r1,
            p_r07  RADIOBUTTON GROUP r1.
SELECT-OPTIONS:
            s_bwart FOR zmmt0036-bwart.

PARAMETERS: p_r08  RADIOBUTTON GROUP r1,
            p_r09  RADIOBUTTON GROUP r1,
            p_r10  RADIOBUTTON GROUP r1,
            p_r11  RADIOBUTTON GROUP r1,
            p_r12  RADIOBUTTON GROUP r1,
            p_r13  RADIOBUTTON GROUP r1,
            p_r14  RADIOBUTTON GROUP r1,
            p_r15  RADIOBUTTON GROUP r1,
            p_r16  RADIOBUTTON GROUP r1,
            p_r17  RADIOBUTTON GROUP r1,
            p_r18  RADIOBUTTON GROUP r1,
            p_r19  RADIOBUTTON GROUP r1,
            p_r20  RADIOBUTTON GROUP r1.


SELECTION-SCREEN SKIP.

PARAMETERS:     p_shop  LIKE zmmt0051-ptype DEFAULT 'T'      MODIF ID
noi,
                p_mtart LIKE mara-mtart     DEFAULT 'PART'   MODIF ID
noi,
                p_date  LIKE sy-datum       DEFAULT sy-datum MODIF ID
noi,
                p_tdate LIKE sy-datum,
                p_flag  TYPE updkz.

SELECTION-SCREEN END OF BLOCK bk1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_selection_screen.

*--------------------------------------------------------------------*
*   INITIALIZATION                                                   *
*--------------------------------------------------------------------*
INITIALIZATION.
  PERFORM initialization.


***********************************************************************
* AT SELECTION-SCREEN Event                                            *
************************************************************************

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON RADIOBUTTON                                   *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON RADIOBUTTON GROUP r1.
  CASE sy-ucomm.
    WHEN 'RADI'.
      PERFORM modify_screen.
  ENDCASE.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  CLEAR gv_cnt.

  CASE 'X'.
    WHEN p_r01.
      PERFORM send_parts_plan.
    WHEN p_r02.
      PERFORM send_stock_level_to_hysco.
    WHEN p_r03.
*      PERFORM send_coil_no_to_mes.
    WHEN p_r04.
      PERFORM reprocess_container_tracking.
    WHEN p_r05.
      PERFORM send_stock_level_to_gcs.
    WHEN p_r06.
*      PERFORM LOCK_OBJECT USING 'E' 'R06'.
      PERFORM send_lpgr_to_gcs.
      PERFORM send_ckdgr_to_gcs.
*      PERFORM LOCK_OBJECT USING 'D' 'R06'.
    WHEN p_r07.
      PERFORM send_abnormal_to_gcs .
    WHEN p_r08.
      PERFORM save_zmmt0082_data.
    WHEN p_r09.
      PERFORM send_material_to_hkmc .
    WHEN p_r10.
      PERFORM send_source_list_to_hkmc .
    WHEN p_r11.
      PERFORM send_feeding_order_to_gcs.
    WHEN p_r12.
      PERFORM send_coil_return_to_hysco.
    WHEN p_r13.
      PERFORM send_engid_to_mobis.
    WHEN p_r14.
      PERFORM send_press_stock_to_mes.
    WHEN p_r15.
      PERFORM get_engid_status_from_mobis.      "Victor 11.09.2010
    WHEN p_r16.
      PERFORM send_engid_status_to_mobis.       "Victor 11.09.2010
    WHEN p_r17.
      PERFORM send_engid_asn_to_glovis.         "Victor 02.14.2011
    WHEN p_r18.
      PERFORM part_shortage_demand.             "Paul   06.01.2011
    WHEN p_r19.
      PERFORM send_engine_kanban.               "Paul   06.08.2011
    WHEN p_r20.
      PERFORM send_material_master_to_gcs.      "Paul   07.05.2011
  ENDCASE.

*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*

  PERFORM write_list_log.

*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM modify_screen .

  LOOP AT SCREEN .
    IF screen-group1 EQ 'NOI' .
      screen-invisible = 1.
      screen-active    = 0.
      screen-input     = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  send_parts_plan
*&---------------------------------------------------------------------*
FORM send_parts_plan .

*  CALL FUNCTION 'Z_MM_IF_OB_02_001_DB'
*       EXPORTING
*            I_PLANT  = 'KVA1'
*            I_SHOP   = P_SHOP
*            I_MTART  = P_MTART
*            I_PDATE  = P_DATE
*       IMPORTING
*            E_RETURN = E_RETURN
*       TABLES
*            IT_BODY  = IT_BODY.

ENDFORM.                    " send_parts_plan
*&---------------------------------------------------------------------*
*&      Form  SEND_TO_STOCK_LEVEL
*&---------------------------------------------------------------------*
FORM send_stock_level_to_hysco .

  CALL FUNCTION 'Z_MM_IF_OB_04_003_DB'
    EXPORTING
      i_lgort  = 'R901'
    IMPORTING
      e_return = e_return.

ENDFORM.                    " SEND_TO_STOCK_LEVEL
*&---------------------------------------------------------------------*
*&      Form  SEND_TO_STOCK_LEVEL
*&---------------------------------------------------------------------*
FORM send_stock_level_to_gcs.

  CALL FUNCTION 'Z_MM_IF_OB_02_004_DB'
    EXPORTING
      i_date   = p_date
    IMPORTING
      e_return = e_return.

ENDFORM.                    " SEND_TO_STOCK_LEVEL
*&---------------------------------------------------------------------*
*&      Form  send_coil_no_to_mes
*&---------------------------------------------------------------------*
FORM send_coil_no_to_mes .

*  CALL FUNCTION 'Z_MM_IF_OB_06_001_DB'
*    EXPORTING
*      i_atdat  = p_date
*      i_flag   = p_flag
*    IMPORTING
*      e_return = e_return
*    TABLES
*      it_m073  = it_m073.

ENDFORM.                    " send_coil_no_to_mes
*&---------------------------------------------------------------------*
*&      Form  WRITE_LIST_LOG
*&---------------------------------------------------------------------*
FORM write_list_log .

  CASE 'X'.
    WHEN p_r01.
      LOOP AT it_body.
        WRITE :/ it_body-plnt,
        it_body-line,
        it_body-ptype,
        it_body-pdate,
        it_body-model_code,
        it_body-idnrk,
        it_body-rpid,
        it_body-beskz,
        it_body-beikz.
      ENDLOOP.

    WHEN p_r02.
      WRITE :/ e_return-type,
               e_return-message.
    WHEN p_r03.
*      LOOP AT it_m073.
*        WRITE :/
*        it_m073-charg,
*        it_m073-matnr,
*        it_m073-maktx,
*        it_m073-menge.
*      ENDLOOP.
  ENDCASE.

ENDFORM.                    " WRITE_LIST_LOG
*&---------------------------------------------------------------------*
*&      Form  REPROCESS_CONTAINER_TRACKING
*&---------------------------------------------------------------------*
FORM reprocess_container_tracking .

  DATA : it_m031 LIKE zmmt0031 OCCURS 0 WITH HEADER LINE.

  CLEAR : it_m031, it_m031[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_m031
           FROM zmmt0031
           WHERE type = 'E'.

  CALL FUNCTION 'Z_MM_IF_IB_02_002'
*    IMPORTING
*      E_RETURN       = e_return
   TABLES
     it_body        = it_m031.


ENDFORM.                    " REPROCESS_CONTAINER_TRACKING
*&---------------------------------------------------------------------*
*&      Form  send_lpgr_to_gcs
*&---------------------------------------------------------------------*
FORM send_lpgr_to_gcs .

  CLEAR : it_m048, it_m048[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_m048
         FROM zmmt0048
         WHERE type  IN (' ', 'E')
*         WHERE type  = ' '
           AND xegld = 'X'
** Furong on 05/22/12 for tuning
%_HINTS ORACLE 'INDEX (ZMMT0048 "ZMMT0048~Z01")'.
** End on 05/22/12

**S__Paul 06/01/11
  IF sy-subrc = 0.
    gv_cnt = sy-dbcnt.
    it_m048-type     = 'S'.
    it_m048-message  = 'Batch job Release'.
    MODIFY it_m048 TRANSPORTING type message
                          WHERE type = ' '.

    MODIFY zmmt0048 FROM TABLE it_m048.
    COMMIT WORK.

    CHECK NOT it_m048[] IS INITIAL.

    CALL FUNCTION 'Z_MM_IF_OB_02_005_RE'
      IMPORTING
        e_return = e_return
      TABLES
        it_body  = it_m048.

  ENDIF.


ENDFORM.                    " send_lpgr_to_gcs

*&---------------------------------------------------------------------*
*&      Form  send_ckdgr_to_gcs
*&---------------------------------------------------------------------*
FORM send_ckdgr_to_gcs .

  CLEAR : it_m048, it_m048[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_m048
         FROM zmmt0048
         WHERE type  IN (' ', 'E')
*         WHERE type  = ' '
           AND xegld = ' '
** Furong on 05/22/12 for tuning
%_HINTS ORACLE 'INDEX (ZMMT0048 "ZMMT0048~Z01")'.
** End on 05/22/12

  IF sy-subrc = 0.
    gv_cnt = gv_cnt + sy-dbcnt.
    it_m048-type     = 'S'.
    it_m048-message  = 'Batch job Release'.
    MODIFY it_m048 TRANSPORTING type message
                          WHERE type = ' '.

    MODIFY zmmt0048 FROM TABLE it_m048.
    COMMIT WORK.

    CHECK NOT it_m048[] IS INITIAL.

    CALL FUNCTION 'Z_MM_IF_OB_02_006_RE'
      IMPORTING
        e_return = e_return
      TABLES
        it_body  = it_m048.

  ENDIF.

  IF gv_cnt > 0.
    IF e_return-type <> 'E'.
      MESSAGE s999 WITH gv_cnt 'Data(s) was sent..'.
    ELSE.
      MESSAGE s999 WITH 'Error' e_return-message.
    ENDIF.
  ELSE.
    MESSAGE i999 WITH 'No Data found.'.
  ENDIF.

ENDFORM.                    " send_ckdgr_to_gcs
*&---------------------------------------------------------------------*
*&      Form  send_lpgr_to_gcs
*&---------------------------------------------------------------------*
FORM send_abnormal_to_gcs .

  CLEAR : it_m036, it_m036[].

** Furong on 07/28/11
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_M036
*         FROM ZMMT0036
**         WHERE type  IN (' ', 'E').
*         WHERE TYPE  EQ ' '.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_m036
         FROM zmmt0036
*         WHERE type  IN (' ', 'E').
         WHERE type  EQ ' '
          AND bwart IN s_bwart
** furong on 05/22/12 for tuning
%_HINTS ORACLE 'INDEX (ZMMT0036 "ZMMT0036~Z01")'.

** End on 05/22/12
** End

  it_m036-type     = 'S'.
  it_m036-message  = 'Batch job Release'.
  MODIFY it_m036 TRANSPORTING type message
                        WHERE type = ' '.

  MODIFY zmmt0036 FROM TABLE it_m036.
  COMMIT WORK.

  CHECK NOT it_m036[] IS INITIAL.

  CALL FUNCTION 'Z_MM_IF_OB_02_002_RE'
    IMPORTING
      e_return = e_return
    TABLES
      it_body  = it_m036.


ENDFORM.                    " send_lpgr_to_gcs
*&---------------------------------------------------------------------*
*&      Form  SAVE_ZMMT0082_DATA
*&---------------------------------------------------------------------*
FORM save_zmmt0082_data .

*  DATA : it_m002 LIKE zmmt0002 OCCURS 0 WITH HEADER LINE.
*  DATA : it_m082 LIKE zmmt0082 OCCURS 0 WITH HEADER LINE.
*  DATA : l_prvbe LIKE marc-vspvb.
*
*  CLEAR : it_m002, it_m002[].
*
*  SELECT *
*         INTO CORRESPONDING FIELDS OF TABLE it_m002
*         FROM zmmt0002.
*
*  SORT it_m002 BY matnr rpoint.
*  DELETE ADJACENT DUPLICATES FROM it_m002
*            COMPARING matnr rpoint.
*
*  LOOP AT it_m002.
*    MOVE-CORRESPONDING it_m002 TO it_m082.
*
*    CLEAR : l_prvbe.
*    SELECT SINGLE vspvb INTO l_prvbe
*        FROM marc
*       WHERE matnr EQ it_m002-matnr
*         AND werks EQ 'KVA1'.
*
*    IF l_prvbe EQ 'BI'.
*      it_m082-rpoint = 'RP01'.
*    ENDIF.
*
*    APPEND it_m082. CLEAR it_m082.
*  ENDLOOP.
*
*  DELETE FROM zmmt0082 WHERE werks = 'P001'.
*
*  MODIFY zmmt0082 FROM TABLE it_m082.
*
*  COMMIT WORK.


ENDFORM.                    " SAVE_ZMMT0082_DATA
*&---------------------------------------------------------------------*
*&      Form  send_material_to_hkmc
*&---------------------------------------------------------------------*
FORM send_material_to_hkmc .


*  CALL FUNCTION 'Z_MM_IF_OB_07_001_DB'
*    EXPORTING
*      i_werks  = 'KVA1'
*      i_fdate  = p_date
*      i_tdate  = p_tdate
*    IMPORTING
*      e_return = e_return
*    TABLES
*      it_m083  = it_m083.


ENDFORM.                    " send_material_to_hkmc
*&---------------------------------------------------------------------*
*&      Form  send_source_list_to_hkmc
*&---------------------------------------------------------------------*
FORM send_source_list_to_hkmc .

**  CALL FUNCTION 'Z_MM_IF_OB_07_002_DB'
**    EXPORTING
**      i_werks  = 'KVA1'
**      i_fdate  = p_date
**      i_tdate  = p_tdate
**    IMPORTING
**      e_return = e_return
**    TABLES
**      it_m084  = it_m084.

ENDFORM.                    " send_source_list_to_hkmc
*&---------------------------------------------------------------------*
*&      Form  send_feeding_order_to_gcs
*&---------------------------------------------------------------------*
FORM send_feeding_order_to_gcs .

*  PERFORM LOCK_OBJECT USING 'E' 'R11'.

  CLEAR : it_m038, it_m038[].

** Changed by Furong on 07/18/11
*  SELECT  *
*         INTO CORRESPONDING FIELDS OF TABLE IT_M038
*         FROM ZMMT0038
*         WHERE TYPE  IN (' ', 'E').

** Changed by Furong on 10/06/11
*   SELECT  *
*         INTO CORRESPONDING FIELDS OF TABLE IT_M038
*         FROM ZMMT0038
*         WHERE ( TYPE  IN (' ', 'E') AND ZFEEDER <> 'HOT' )
*            OR ( TYPE  EQ 'E' AND ZFEEDER EQ 'HOT' ).

  SELECT  *
        INTO CORRESPONDING FIELDS OF TABLE it_m038
        FROM zmmt0038
        WHERE ( ( type  IN (' ', 'E') AND zfeeder <> 'HOT' )
           OR ( type  EQ 'E' AND zfeeder EQ 'HOT' ) )
*           AND REVERSED = ' '.
** Furong on 05/22/12 tuning
%_HINTS ORACLE 'INDEX (ZMMT0038 "ZMMT0038~Z03")'.
** End on 05/22/12

** End on 10/06/11

** End of change on 07/18/11

*C__Paul 01/06/11
*         WHERE type  EQ ' '.

** Changed by Furong on 10/12/11
*  SORT IT_M038 BY REVERSED.
** End on 10/12/11

  it_m038-type     = 'R'.
  it_m038-message  = 'Batch job Release'.
  MODIFY it_m038 TRANSPORTING type message
                        WHERE type = ' '
                           OR type = 'E'.

  MODIFY zmmt0038 FROM TABLE it_m038.
  COMMIT WORK.

  IF NOT it_m038[] IS INITIAL.

    CALL FUNCTION 'Z_MM_IF_OB_02_003_RE'
      IMPORTING
        e_return = e_return
      TABLES
        it_body  = it_m038.

    IF e_return-type <> 'E'.
      DESCRIBE TABLE it_m038 LINES gv_cnt.
      MESSAGE s999 WITH gv_cnt 'Data(s) was sent..'.
    ELSE.
      MESSAGE s999 WITH 'Error:' e_return-message.
    ENDIF.
  ELSE.
    MESSAGE i999 WITH 'No Data found.'.
  ENDIF.

*  PERFORM LOCK_OBJECT USING 'D' 'R11'.
ENDFORM.                    " send_feeding_order_to_gcs

*---------------------------------------------------------------------*
*       FORM SEND_EXPRESS_MESSAGE                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM alv_send_express_mail    TABLES p_content LIKE contents[]
                              USING  value(send_title).
*
  DESCRIBE TABLE alv_receivers LINES sy-tfill.
  IF sy-tfill = 0.
    MOVE sy-uname TO alv_receivers-receiver.
    MOVE 'X'      TO alv_receivers-express.
    APPEND alv_receivers.
  ENDIF.
*
  DESCRIBE TABLE alv_receivers LINES sy-tfill.
  IF sy-tfill = 0.
    EXIT.
  ENDIF.
* TITLE
  MOVE send_title TO docdata-obj_descr.
* CONTENT
  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = docdata
      document_type              = 'RAW'
    IMPORTING
      sent_to_all                = sent_to_all
      new_object_id              = object_id
    TABLES
      object_header              = obj_header
      object_content             = p_content
      receivers                  = alv_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

ENDFORM.                               "send_express_message.
*
*&---------------------------------------------------------------------*
*&      Form  SEND_TO_SAPOFFICE_MAIL
*&---------------------------------------------------------------------*
FORM send_to_sapoffice_mail .

  DATA : l_bklas TYPE bklas,
         t_bklas TYPE bklas,
         l_sobsl TYPE sobsl.

  DATA : BEGIN OF it_mara OCCURS 0,
         matnr LIKE mara-matnr,
         mtart LIKE mara-mtart,
         bklas LIKE mbew-bklas,
         END OF it_mara.

  CLEAR : it_mara,  it_mara[],
          contents, contents[].
  SELECT a~matnr
         a~mtart
         b~bklas
      INTO CORRESPONDING FIELDS OF TABLE it_mara
        FROM mara AS a INNER JOIN mbew AS b
             ON a~matnr = b~matnr
        WHERE  a~mtart IN ('PART', 'SEMI')
           AND b~bwkey = 'KVA1'.

  LOOP AT it_mara.
    CLEAR :  t_bklas, l_sobsl.

    SELECT SINGLE a~bklas INTO t_bklas
            FROM t025 AS a INNER JOIN t134 AS b
                     ON a~kkref = b~kkref
            WHERE a~bklas = it_mara-bklas
              AND b~mtart = it_mara-mtart.

    IF t_bklas IS INITIAL.    "Configuration check with master
      contents-line = it_mara-matnr.
      APPEND contents.
*      UPDATE mara SET : mstae = '10'
*                  WHERE matnr = it_mara-matnr.
    ENDIF.

    SELECT SINGLE sobsl INTO l_sobsl
           FROM marc
          WHERE matnr = it_mara-matnr
           AND  werks = 'KVA1'.

    CASE it_mara-mtart.
      WHEN 'SEMI'.
        IF it_mara-bklas NE  '7900'.
          IF  l_sobsl    NE  '50'.
            contents-line = it_mara-matnr.
            APPEND contents.
*            UPDATE mara SET : mstae = '10'
*                        WHERE matnr = it_mara-matnr.
          ENDIF.
        ENDIF.

      WHEN 'PART'.
        IF it_mara-bklas  = '7900'.
          IF it_mara-matnr+3(2) = 'FC'   AND
             ( l_sobsl          = '30' OR
               l_sobsl          = '31'  ).
          ELSE.
            contents-line = it_mara-matnr.
            APPEND contents.
*            UPDATE mara SET : mstae = '10'
*                        WHERE matnr = it_mara-matnr.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  PERFORM alv_send_express_mail    TABLES contents[]
                  USING 'Check material Type and VALUATION CLASS'.

ENDFORM.                    " SEND_TO_SAPOFFICE_MAIL
*&---------------------------------------------------------------------*
*&      Form  send_coil_return_to_hysco
*&---------------------------------------------------------------------*
FORM send_coil_return_to_hysco .


*  CLEAR : it_m067, it_m067[].
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_m067
*         FROM zmmt0067
*         WHERE type  = ' '.
*
*  it_m067-type     = 'S'.
*  it_m067-message  = 'Batch job Release'.
*  MODIFY it_m067 TRANSPORTING type message
*                        WHERE type = ' '.
*
*  MODIFY zmmt0067 FROM TABLE it_m067.
*  COMMIT WORK.
*
*  CHECK NOT it_m067[] IS INITIAL.
*
*  CALL FUNCTION 'Z_MM_IF_OB_04_008_DB'
*    IMPORTING
*      e_return = e_return
*    TABLES
*      it_body  = it_m067.
*

ENDFORM.                    " SEND_TO_STOCK_TRANSFER_HYSCO
*&---------------------------------------------------------------------*
*&      Form  CORRECT_JIT_GR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM correct_jit_gr .


  CALL FUNCTION 'ZITOUT09_CORRECT_JIT_GR'.


ENDFORM.                    " CORRECT_JIT_GR
*&---------------------------------------------------------------------*
*&      Form  SEND_ENGID_TO_MOBIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_engid_to_mobis .

*  DATA : it_m052 LIKE zmms0152 OCCURS 0 WITH HEADER LINE.
*
*  CLEAR : it_m091, it_m091[].
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_m091
*         FROM zmmt0091
*         WHERE trflg  = ' '
*           AND  type   <> 'A'   " 'A':KD engine Victor 01.21.2011
*           AND  type   <> 'B'.  " 'B':Stockpile engine
*
*  it_m091-trflg  = 'X'.
*  MODIFY it_m091 TRANSPORTING trflg
*                        WHERE trflg = ' '.
*
*  MODIFY zmmt0091 FROM TABLE it_m091.
*  COMMIT WORK.
*
*  CHECK NOT it_m091[] IS INITIAL.
*
*  LOOP AT it_m091.
*    it_m052-vbeln     = it_m091-vbeln.
*    it_m052-posnr     = it_m091-posnr.
*    it_m052-flag      = it_m091-cstatu.
*    it_m052-engine_id = it_m091-engine_id.
*    it_m052-matnr     = it_m091-matnr.
*    it_m052-lfdat     = it_m091-bl_date.
*    it_m052-erzet     = it_m091-bl_time.
*    it_m052-lifex     = it_m091-lifex.
*    it_m052-idnrk     = it_m091-matnr_fc.
*    APPEND it_m052. CLEAR : it_m052.
*  ENDLOOP.
*
*  CALL FUNCTION 'Z_MM_IF_OB_08_001_RE'
*    TABLES
*      it_body = it_m052.
*

ENDFORM.                    " SEND_ENGID_TO_MOBIS
*&---------------------------------------------------------------------*
*&      Form  SEND_PRESS_STOCK_TO_MES
*&---------------------------------------------------------------------*
FORM send_press_stock_to_mes .

  CALL FUNCTION 'Z_MM_IF_OB_06_003_DB'
    IMPORTING
      e_return = e_return.

ENDFORM.                    " SEND_PRESS_STOCK_TO_MES
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_selection_screen .
  CHECK sy-uname(4) = 'GLVS'.

  LOOP AT SCREEN.
    IF screen-group1 NE 'GLV'.
      screen-input      = 0.
      screen-active     = 0.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " MODIFY_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization .
  IF sy-uname(4) = 'GLVS'.
    CLEAR: p_r01, p_r02, p_r03, p_r04, p_r06, p_r07, p_r08,
           p_r09, p_r10, p_r11, p_r12, p_r13, p_r14.
    MOVE: 'X' TO p_r05.
  ENDIF.
ENDFORM.                    " INITIALIZATION

*&---------------------------------------------------------------------*
*&      Form  GET_ENGID_FROM_MOBIS
*&---------------------------------------------------------------------*
FORM get_engid_status_from_mobis .

*  DATA : it_m091 LIKE zmmt0091 OCCURS 0 WITH HEADER LINE,
*         it_m052 LIKE zmms0152 OCCURS 0 WITH HEADER LINE.
*
*  CLEAR : it_m091, it_m091[].
*
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_m091
*         FROM zmmt0091
*      WHERE cstatu = 'GR'
*        AND type   = 'E'.
*
*  CHECK NOT it_m091[] IS INITIAL.
*
*  LOOP AT it_m091.
*    MOVE-CORRESPONDING it_m091 TO it_m052.
*    it_m052-flag  = 'GR'.
*    it_m052-lfdat = it_m091-gr_date.
*    it_m052-erzet = it_m091-gr_time.
*
*    APPEND it_m052.
*  ENDLOOP.
*
*  CALL FUNCTION 'Z_MM_IF_IB_06_002'
*    TABLES
*      it_body = it_m052.

ENDFORM.                    " GET_ENGID_FROM_MOBIS
*&---------------------------------------------------------------------*
*&      Form  SEND_ENGID_STATUS_TO_MOBIS
*&---------------------------------------------------------------------*
FORM send_engid_status_to_mobis .

**  DATA : it_m091 LIKE zmmt0091 OCCURS 0 WITH HEADER LINE.
**  DATA : it_m052 LIKE zmms0152 OCCURS 0 WITH HEADER LINE.
**
**  CLEAR : it_m091, it_m091[].
**
**  SELECT *
**         INTO CORRESPONDING FIELDS OF TABLE it_m091
**         FROM zmmt0091
**     WHERE cstatu = 'BL'
**       AND type   = 'E'.
**
**  CHECK NOT it_m091[] IS INITIAL.
**
***  it_m091-trflg = ' '.
***  MODIFY it_m091 TRANSPORTING trflg
***                        WHERE trflg <> ' '.
**  it_m091-trflg  = 'X'.
**  MODIFY it_m091 TRANSPORTING trflg
**                        WHERE trflg = ' '.
**
**  MODIFY zmmt0091 FROM TABLE it_m091.
**  COMMIT WORK.
**
**  LOOP AT it_m091.
**    it_m052-vbeln     = it_m091-vbeln.
**    it_m052-posnr     = it_m091-posnr.
**    it_m052-flag      = it_m091-cstatu.
**    it_m052-engine_id = it_m091-engine_id.
**    it_m052-matnr     = it_m091-matnr.
**    it_m052-lfdat     = it_m091-bl_date.
**    it_m052-erzet     = it_m091-bl_time.
**    it_m052-lifex     = it_m091-lifex.
**    it_m052-idnrk     = it_m091-matnr_fc.
**    APPEND it_m052. CLEAR : it_m052.
**  ENDLOOP.
**
**  CALL FUNCTION 'Z_MM_IF_OB_08_001_RE'
**    TABLES
**      it_body = it_m052.
**
ENDFORM.                    " SEND_ENGID_STATUS_TO_MOBIS
*&---------------------------------------------------------------------*
*&      Form  SEND_ENGID_ASN_TO_GLOVIS
*&---------------------------------------------------------------------*
*       Send engine(Status : A(KD), B(stockpile) info  to Glovis
*----------------------------------------------------------------------*
FORM send_engid_asn_to_glovis .

**  DATA : it_m052 LIKE zmms0152 OCCURS 0 WITH HEADER LINE.
**  DATA : it_m216 LIKE zmms0216 OCCURS 0 WITH HEADER LINE.
**  DATA : it_m217 LIKE zmms0217 OCCURS 0 WITH HEADER LINE.
**  DATA : it_likp LIKE likp     OCCURS 0 WITH HEADER LINE.
**  DATA : it_tvknt LIKE tvknt   OCCURS 0 WITH HEADER LINE.
**  DATA : wa_m091  TYPE zmmt0091.
**  DATA : l_etd(15).  "ETD
**
**  CLEAR : it_m091, it_m091[], it_m216[], it_m217[].
**
**  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_m091
**         FROM zmmt0091
**         WHERE gcsflg  <> 'S'
**           AND ( type   = 'A' OR type = 'B' ).
**
**  CHECK NOT it_m091[] IS INITIAL.
**  SORT it_m091 BY traid vbeln engine_id.
**
**  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_likp
**  FROM likp
**    FOR ALL ENTRIES IN it_m091
**  WHERE vbeln =  it_m091-vbeln.
**
**  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_tvknt
**    FROM tvknt
**    WHERE spras = sy-langu.
**
**  CHECK NOT it_likp[] IS INITIAL.
**
**  SORT it_m091 BY traid vbeln.
**  LOOP AT it_m091.
**    MOVE-CORRESPONDING it_m091 TO wa_m091.
**
**
***2011.03.14 MODIFIED BY JEONGSU.YOUN
**    CLEAR it_likp.
**    READ TABLE it_likp WITH KEY vbeln = wa_m091-vbeln.
**
**    IF it_likp-lifnr <> 'SAFE'.
**      CONTINUE.
**    ENDIF.
***
**
**    AT NEW traid.
**      AT NEW vbeln.
***-  header
**        CLEAR : it_likp, it_tvknt.
**        READ TABLE it_likp WITH KEY vbeln = wa_m091-vbeln.
**
***        IF wa_m091-ebeln+0(2) = '42'.       "KD engine
**        IF wa_m091-type = 'A'.       "KD engine
**          READ TABLE it_tvknt WITH KEY knote  = it_likp-idt_cur_estloc
*.
**          it_m216-cont         =  wa_m091-traid.
**          it_m216-invo         =  wa_m091-vbeln.
**          it_m216-ship         =  'S'.
**          it_m216-desc_c       =  it_likp-idt_cur_estloc.
**          it_m216-dest_name    =  it_tvknt-bezei.
**          it_m216-trms_stat    =  wa_m091-type.
**        ELSEIF wa_m091-type = 'B'.       "stockpile
**          it_m216-cont         =  wa_m091-vbeln.
**          it_m216-invo         =  wa_m091-lifex.
**          it_m216-ship         =  'T'.
**          it_m216-desc_c       =  'USEXW'.
**          READ TABLE it_tvknt WITH KEY knote  = it_m216-desc_c.
**          it_m216-dest_name    =  it_tvknt-bezei.
**          it_m216-trms_stat    =  wa_m091-type.
**        ENDIF.
**
**        it_m216-corp         = 'K201'.
**        it_m216-eta          =  it_likp-lfdat.         "delivery date
**        l_etd                =  it_likp-idt_cur_evttst.
**        it_m216-etd          =  l_etd+0(8).
**        it_m216-cret_d       =  it_likp-erdat.
**        it_m216-cret_t       =  it_likp-erzet.
**        it_m216-trms_d       =  sy-datum.
**        it_m216-trms_t       =  sy-uzeit+0(4).
***       it_m216-TRMS_TEXT    =  it_m091-
**        APPEND it_m216. CLEAR : it_m216.
**      ENDAT.
**
**    ENDAT.
**
**
**
***-  item
**    IF wa_m091-type = 'A'.       "KD engine
**      it_m217-cont         =  wa_m091-traid.
**      it_m217-invo         =  wa_m091-vbeln.
**    ELSEIF wa_m091-type = 'B'.   "stockpile
**      it_m217-cont         =  wa_m091-vbeln.
**      it_m217-invo         =  wa_m091-lifex.
**    ENDIF.
**
**    it_m217-corp = 'K201'.
**    it_m217-glpo  =  wa_m091-engine_id+0(10).
**    it_m217-case  =  wa_m091-engine_id+10(2).
**    it_m217-part  =  wa_m091-matnr.
**    it_m217-fmpo  =  wa_m091-ebeln.
**    it_m217-line  =  wa_m091-ebelp.
**    it_m217-ship_q  =  1.
**    APPEND it_m217. CLEAR : it_m217, wa_m091.
**  ENDLOOP.
**
**  CHECK it_m216[] IS NOT INITIAL.
**  CHECK it_m217[] IS NOT INITIAL.
**
**  CALL FUNCTION 'Z_MM_IF_OB_02_008_DB'
**    TABLES
**      it_head   = it_m216
**      it_detail = it_m217.
**
ENDFORM.                    " SEND_ENGID_ASN_TO_GLOVIS
*&---------------------------------------------------------------------*
*&      Form  Part_shortage_demand
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM part_shortage_demand.

  DATA : g_dest(50),
         lt_zmmt0052 LIKE zmmt0052 OCCURS 0 WITH HEADER LINE,
         o_flag TYPE zresult,
         o_msg TYPE zmsg.
  .

  CLEAR : lt_zmmt0052,lt_zmmt0052[].

  CALL FUNCTION 'ZMM_PART_SHORTAGE_DEMAND_HR'
*   IMPORTING
*     O_FLAG             =
*     O_MSG              =
    TABLES
      gcs_zmmt0052       = lt_zmmt0052
            .

  g_dest = 'WMPM01'.

*  SELECT *
*    FROM ZMMT0052
*    INTO TABLE LT_ZMMT0052.

  IF NOT lt_zmmt0052[] IS INITIAL.

    CALL FUNCTION 'ZMM_PART_SHORT_DEMAND_HR_GCS'
      DESTINATION g_dest
      IMPORTING
        e_return              = e_return
      TABLES
        gcs_zmmt0052          = lt_zmmt0052
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        resource_failure      = 3
        OTHERS                = 4.

    IF sy-subrc EQ 0 AND e_return-type EQ 'S'.
      DATA : lv_cnt(6) TYPE c.
      DESCRIBE TABLE lt_zmmt0052 LINES gv_cnt.
      MESSAGE s999 WITH gv_cnt 'Data(s) was sent..'.
    ELSE.
      CASE sy-subrc.
        WHEN '1'.
          e_return-message = 'communication_failure'.
          e_return-type    = 'E'.
        WHEN '2'.
          e_return-message = 'system_failure'.
          e_return-type    = 'E'.
        WHEN '3'.
          e_return-message = 'resource_failure'.
          e_return-type    = 'E'.
        WHEN '4'.
          e_return-message = 'Others'.
          e_return-type    = 'E'.
      ENDCASE.
      MESSAGE s999 WITH 'Error:' e_return-message.
    ENDIF.


*    CASE sy-subrc.
*      WHEN '0'.
*        e_return-type     = 'S'.
*        lv_cnt = gv_cnt.
*        CONCATENATE lv_cnt 'Batch job Release' INTO e_return-message
*        SEPARATED BY space.
*      WHEN '1'.
*        e_return-message = 'communication_failure'.
*        e_return-type    = 'E'.
*      WHEN '2'.
*        e_return-message = 'system_failure'.
*        e_return-type    = 'E'.
*      WHEN '3'.
*        e_return-message = 'resource_failure'.
*        e_return-type    = 'E'.
*      WHEN '4'.
*        e_return-message = 'Others'.
*        e_return-type    = 'E'.
*    ENDCASE.

**S__PAUL ADD 06/15/11
    lt_zmmt0052-type     = e_return-type.
    lt_zmmt0052-message  = e_return-message.
    lt_zmmt0052-zedat    = sy-datum.
    lt_zmmt0052-zetim    = sy-uzeit.
    MODIFY lt_zmmt0052 TRANSPORTING zedat zetim type message
                              WHERE zbdat <> space.

    MODIFY zmmt0052 FROM TABLE lt_zmmt0052.
**E
*    IF sy-subrc EQ 0.
*      MESSAGE s999 WITH gv_cnt 'Data(s) was sent..'.
*    ELSE.
*      MESSAGE s999 WITH 'Error:' e_return-message..
*    ENDIF.
  ELSE.
    MESSAGE i999 WITH 'No Data found.'.
  ENDIF.

ENDFORM.                    " Part_shortage_demand
*&---------------------------------------------------------------------*
*&      Form  Send_Engine_Kanban
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_engine_kanban.
  DATA : it_m041 LIKE zmmt0041 OCCURS 0 WITH HEADER LINE,
         e_return LIKE  zmms0053.

*  PERFORM LOCK_OBJECT USING 'E' 'R19'.

  CLEAR : it_m041, it_m041[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_m041
         FROM zmmt0041
         WHERE type IN (' ','E')
** Furong on 05/22/12
%_HINTS ORACLE 'INDEX (ZMMT0041 "ZMMT0041~Z04")'.
** End on 05/22/12

  IF sy-subrc = 0.
    it_m041-type     = 'S'.
    it_m041-message  = 'Batch job Release'.
    MODIFY it_m041 TRANSPORTING type message
                          WHERE type = ' '
                             OR type = 'E'.

    MODIFY zmmt0041 FROM TABLE it_m041.
    COMMIT WORK.

    CALL FUNCTION 'Z_MM_IF_OB_02_009_RE'
      IMPORTING
        e_return = e_return
      TABLES
        it_body  = it_m041.

*  DATA : G_DEST(50),
*         LT_ZMMT0041 LIKE ZMMT0041 OCCURS 0 WITH HEADER LINE,
*         E_RETURN  LIKE  ZMMS0053.
*
*  G_DEST = 'WMPM01'.
*
*  CLEAR : LT_ZMMT0041,LT_ZMMT0041[].
*
*  SELECT *
*    FROM ZMMT0041
*    INTO TABLE LT_ZMMT0041.
*
*  IF SY-SUBRC = 0.
*
*    CALL FUNCTION 'Z_MM_IF_OB_02_009'
*         DESTINATION G_DEST
*         IMPORTING
*              E_RETURN              = E_RETURN
*         TABLES
*              IT_BODY               = LT_ZMMT0041
*         EXCEPTIONS
*              COMMUNICATION_FAILURE = 1
*              SYSTEM_FAILURE        = 2
*              RESOURCE_FAILURE      = 3
*              OTHERS                = 4.
*
    DESCRIBE TABLE it_m041 LINES gv_cnt.
    IF e_return-type <> 'E'.
      MESSAGE s999 WITH gv_cnt 'Data(s) was sent..'.
    ELSE.
      MESSAGE s999 WITH 'Error:' e_return-message.
    ENDIF.
  ELSE.
    MESSAGE i999 WITH 'No Data found.'.
  ENDIF.

*  PERFORM LOCK_OBJECT USING 'D' 'R19'.

ENDFORM.                    " Send_Engine_Kanban
*&---------------------------------------------------------------------*
*&      Form  SEND_MATERIAL_MASTER_TO_GCS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_material_master_to_gcs.
  DATA : e_return LIKE  zmms0053,
         iw_werks LIKE  zsmm_plant.

  CALL FUNCTION 'Z_MM_IF_OB_02_007_DB'
    EXPORTING
*    IW_WERKS       = IW_WERKS
*    I_FDATE        = SY-DATUM
*   I_TDATE        =
     i_mtart        = 'ROH'
   IMPORTING
     e_return       = e_return
* TABLES
*   IT_M086        =
            .
  IF e_return-type = 'E'.
    MESSAGE i009 WITH e_return-message.
  ELSE.
    MESSAGE s009 WITH e_return-message.
  ENDIF.


ENDFORM.                    " SEND_MATERIAL_MASTER_TO_GCS
*&---------------------------------------------------------------------*
*&      Form  LOCK_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0355   text
*      -->P_0356   text
*----------------------------------------------------------------------*
FORM lock_object USING    p_chk
                          p_r.

*  IF P_CHK = 'E'.
*    CALL FUNCTION 'ENQUEUE_EZ_ZMMT0038'
*         EXPORTING
*              RADIO          = P_R
*         EXCEPTIONS
*              FOREIGN_LOCK   = 1
*              SYSTEM_FAILURE = 2
*              OTHERS         = 3.
*
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      LEAVE PROGRAM.
*    ENDIF.
*
*  ELSE.
*    CALL FUNCTION 'DEQUEUE_EZ_ZMMT0038'
*         EXPORTING
*              RADIO = P_R.
*  ENDIF.
ENDFORM.                    " LOCK_OBJECT
