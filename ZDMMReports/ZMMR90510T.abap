*&------------------------------------------------------------------
*& Program ID     : ZMMR90510T
*& Profram Name   : Batch Job Program
*& Created by     :
*& Created on     :
*& Reference Pgm. :
*& Description    : Batch Job Program
*&
*& Modification Log
*&====================================================================
*& Date        Developer      Request ID      Description
*&
*&--------------------------------------------------------------------
REPORT ZMMR90510T    LINE-SIZE 154 LINE-COUNT 58 MESSAGE-ID ZMMM
                      NO STANDARD PAGE HEADING.


DATA : E_RETURN LIKE ZMMS0053,
       IT_M032  LIKE ZMMT0032 OCCURS 0 WITH HEADER LINE.

DATA: GV_CNT TYPE I.
*
DATA: BEGIN OF CONTENTS OCCURS 0.
        INCLUDE STRUCTURE SOLISTI1.
DATA: END OF CONTENTS.
*
DATA: ALV_T_CONT LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE.
*
DATA: BEGIN OF ALV_RECEIVERS OCCURS 0.
        INCLUDE STRUCTURE SOMLRECI1.
DATA: END   OF ALV_RECEIVERS.
*
DATA: BEGIN OF DOCDATA.
        INCLUDE STRUCTURE SODOCCHGI1.
DATA: END OF DOCDATA.
*
DATA: SENT_TO_ALL(1) TYPE C.
*
DATA: OBJECT_ID LIKE  SOFOLENTI1-OBJECT_ID.
*
DATA: BEGIN OF OBJ_HEADER OCCURS 0.
        INCLUDE STRUCTURE SOLISTI1.
DATA: END OF OBJ_HEADER.
*
TYPE-POOLS: ESP1.

SELECTION-SCREEN BEGIN OF BLOCK BK1 WITH FRAME TITLE TEXT-SEL.
PARAMETERS: P_R01  RADIOBUTTON GROUP R1 DEFAULT 'X',  " MODIF ID LIN,
* USER-COMMAND RADI,
            P_R02  RADIOBUTTON  GROUP R1 .
*            P_R03  RADIOBUTTON GROUP R1,
*            P_R04  RADIOBUTTON GROUP R1,
*            P_R05  RADIOBUTTON GROUP R1 MODIF ID GLV,
*            P_R06  RADIOBUTTON GROUP R1,
*            P_R07  RADIOBUTTON GROUP R1.
*PARAMETERS:     P_SHOP  LIKE ZMMT0051-PTYPE DEFAULT 'T'   MODIF ID
*NOI.
SELECT-OPTIONS: S_DATE FOR SY-DATUM DEFAULT SY-DATUM MODIF ID LIN.
SELECTION-SCREEN END OF BLOCK BK1.

AT SELECTION-SCREEN OUTPUT.

  PERFORM MODIFY_SELECTION_SCREEN.

*--------------------------------------------------------------------*
*   INITIALIZATION                                                   *
*--------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON RADIOBUTTON                                   *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON RADIOBUTTON GROUP R1.
  CASE SY-UCOMM.
    WHEN 'RADI'.
      PERFORM MODIFY_SCREEN.
  ENDCASE.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  CLEAR GV_CNT.

  CASE 'X'.
    WHEN P_R01.
      PERFORM PROCESS_LINE_FEEDING.
*    WHEN P_R06.
*    WHEN P_R07.
  ENDCASE.

*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*

  PERFORM WRITE_LIST_LOG.

*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM MODIFY_SCREEN .

*  LOOP AT SCREEN .
*    IF SCREEN-GROUP1 EQ 'LIN' .
*      SCREEN-INVISIBLE = 0.
*      SCREEN-ACTIVE    = 1.
*      SCREEN-INPUT     = 1.
*      MODIFY SCREEN.
*    ELSE.
*      SCREEN-INVISIBLE = 1.
*      SCREEN-ACTIVE    = 0.
*      SCREEN-INPUT     = 0.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  SEND_TO_STOCK_LEVEL
*&---------------------------------------------------------------------*
FORM SEND_STOCK_LEVEL_TO_HYSCO .

  CALL FUNCTION 'Z_MM_IF_OB_04_003_DB'
       EXPORTING
            I_LGORT  = 'R901'
       IMPORTING
            E_RETURN = E_RETURN.

ENDFORM.                    " SEND_TO_STOCK_LEVEL
*&---------------------------------------------------------------------*
*&      Form  SEND_TO_STOCK_LEVEL
*&---------------------------------------------------------------------*
FORM PROCESS_LINE_FEEDING.

  CLEAR : IT_M032[].

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_M032
    FROM ZMMT0032
     WHERE BUDAT IN S_DATE
       AND TYPE = 'E'
    %_HINTS ORACLE 'RULE'.  "Addition

  CALL FUNCTION 'Z_MM_IF_IB_02_003'
       TABLES
            IT_BODY = IT_M032.

ENDFORM.                    " SEND_TO_STOCK_LEVEL
*&---------------------------------------------------------------------*
*&      Form  send_coil_no_to_mes
*&---------------------------------------------------------------------*
FORM SEND_COIL_NO_TO_MES .

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
FORM WRITE_LIST_LOG .

  CASE 'X'.
    WHEN P_R01.
      LOOP AT IT_M032.
        WRITE :/ IT_M032-RSNUM,
IT_M032-RSPOS,
IT_M032-BUDAT,
IT_M032-LGORT,
IT_M032-UMLGO,
IT_M032-MATNR,
IT_M032-MENGE,
IT_M032-PKKEY,
IT_M032-TYPE,
IT_M032-MBLNR,
IT_M032-MESSAGE.
      ENDLOOP.

*    WHEN P_R02.
*      WRITE :/ E_RETURN-TYPE,
*               E_RETURN-MESSAGE.
  ENDCASE.

ENDFORM.                    " WRITE_LIST_LOG

*---------------------------------------------------------------------*
*       FORM SEND_EXPRESS_MESSAGE                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM ALV_SEND_EXPRESS_MAIL    TABLES P_CONTENT LIKE CONTENTS[]
                              USING  VALUE(SEND_TITLE).
*
  DESCRIBE TABLE ALV_RECEIVERS LINES SY-TFILL.
  IF SY-TFILL = 0.
    MOVE SY-UNAME TO ALV_RECEIVERS-RECEIVER.
    MOVE 'X'      TO ALV_RECEIVERS-EXPRESS.
    APPEND ALV_RECEIVERS.
  ENDIF.
*
  DESCRIBE TABLE ALV_RECEIVERS LINES SY-TFILL.
  IF SY-TFILL = 0.
    EXIT.
  ENDIF.
* TITLE
  MOVE SEND_TITLE TO DOCDATA-OBJ_DESCR.
* CONTENT
  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
       EXPORTING
            DOCUMENT_DATA              = DOCDATA
            DOCUMENT_TYPE              = 'RAW'
       IMPORTING
            SENT_TO_ALL                = SENT_TO_ALL
            NEW_OBJECT_ID              = OBJECT_ID
       TABLES
            OBJECT_HEADER              = OBJ_HEADER
            OBJECT_CONTENT             = P_CONTENT
            RECEIVERS                  = ALV_RECEIVERS
       EXCEPTIONS
            TOO_MANY_RECEIVERS         = 1
            DOCUMENT_NOT_SENT          = 2
            DOCUMENT_TYPE_NOT_EXIST    = 3
            OPERATION_NO_AUTHORIZATION = 4
            PARAMETER_ERROR            = 5
            X_ERROR                    = 6
            ENQUEUE_ERROR              = 7
            OTHERS                     = 8.

ENDFORM.                               "send_express_message.
*
*&---------------------------------------------------------------------*
*&      Form  SEND_TO_SAPOFFICE_MAIL
*&---------------------------------------------------------------------*
FORM SEND_TO_SAPOFFICE_MAIL .

  DATA : L_BKLAS TYPE BKLAS,
         T_BKLAS TYPE BKLAS,
         L_SOBSL TYPE SOBSL.

  DATA : BEGIN OF IT_MARA OCCURS 0,
         MATNR LIKE MARA-MATNR,
         MTART LIKE MARA-MTART,
         BKLAS LIKE MBEW-BKLAS,
         END OF IT_MARA.

  CLEAR : IT_MARA,  IT_MARA[],
          CONTENTS, CONTENTS[].
  SELECT A~MATNR
         A~MTART
         B~BKLAS
      INTO CORRESPONDING FIELDS OF TABLE IT_MARA
        FROM MARA AS A INNER JOIN MBEW AS B
             ON A~MATNR = B~MATNR
        WHERE  A~MTART IN ('PART', 'SEMI')
           AND B~BWKEY = 'KVA1'.

  LOOP AT IT_MARA.
    CLEAR :  T_BKLAS, L_SOBSL.

    SELECT SINGLE A~BKLAS INTO T_BKLAS
            FROM T025 AS A INNER JOIN T134 AS B
                     ON A~KKREF = B~KKREF
            WHERE A~BKLAS = IT_MARA-BKLAS
              AND B~MTART = IT_MARA-MTART.

    IF T_BKLAS IS INITIAL.    "Configuration check with master
      CONTENTS-LINE = IT_MARA-MATNR.
      APPEND CONTENTS.
*      UPDATE mara SET : mstae = '10'
*                  WHERE matnr = it_mara-matnr.
    ENDIF.

    SELECT SINGLE SOBSL INTO L_SOBSL
           FROM MARC
          WHERE MATNR = IT_MARA-MATNR
           AND  WERKS = 'KVA1'.

    CASE IT_MARA-MTART.
      WHEN 'SEMI'.
        IF IT_MARA-BKLAS NE  '7900'.
          IF  L_SOBSL    NE  '50'.
            CONTENTS-LINE = IT_MARA-MATNR.
            APPEND CONTENTS.
*            UPDATE mara SET : mstae = '10'
*                        WHERE matnr = it_mara-matnr.
          ENDIF.
        ENDIF.

      WHEN 'PART'.
        IF IT_MARA-BKLAS  = '7900'.
          IF IT_MARA-MATNR+3(2) = 'FC'   AND
             ( L_SOBSL          = '30' OR
               L_SOBSL          = '31'  ).
          ELSE.
            CONTENTS-LINE = IT_MARA-MATNR.
            APPEND CONTENTS.
*            UPDATE mara SET : mstae = '10'
*                        WHERE matnr = it_mara-matnr.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  PERFORM ALV_SEND_EXPRESS_MAIL    TABLES CONTENTS[]
          USING 'Check material Type and VALUATION CLASS'.

ENDFORM.                    " SEND_TO_SAPOFFICE_MAIL
*&---------------------------------------------------------------------*
*&      Form  send_coil_return_to_hysco
*&---------------------------------------------------------------------*
FORM SEND_COIL_RETURN_TO_HYSCO .


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
FORM CORRECT_JIT_GR .


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
FORM SEND_ENGID_TO_MOBIS .

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
FORM SEND_PRESS_STOCK_TO_MES .

  CALL FUNCTION 'Z_MM_IF_OB_06_003_DB'
       IMPORTING
            E_RETURN = E_RETURN.

ENDFORM.                    " SEND_PRESS_STOCK_TO_MES
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SELECTION_SCREEN .
*  CHECK SY-UNAME(4) = 'GLVS'.
*
  LOOP AT SCREEN .
    IF SCREEN-GROUP1 EQ 'LIN' .
      IF P_R01 = ' '.
        SCREEN-INVISIBLE = 1.
        SCREEN-ACTIVE    = 0.
        SCREEN-INPUT     = 0.
        MODIFY SCREEN.
      ENDIF.
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
FORM INITIALIZATION .
*  IF SY-UNAME(4) = 'GLVS'.
*    CLEAR: P_R01, P_R02, P_R03, P_R04, P_R06, P_R07, P_R08,
*           P_R09, P_R10, P_R11, P_R12, P_R13, P_R14.
*    MOVE: 'X' TO P_R05.
*  ENDIF.
ENDFORM.                    " INITIALIZATION

*&---------------------------------------------------------------------*
*&      Form  GET_ENGID_FROM_MOBIS
*&---------------------------------------------------------------------*
FORM GET_ENGID_STATUS_FROM_MOBIS .

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
FORM SEND_ENGID_STATUS_TO_MOBIS .

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
FORM SEND_ENGID_ASN_TO_GLOVIS .

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
FORM PART_SHORTAGE_DEMAND.

  DATA : G_DEST(50),
         LT_ZMMT0052 LIKE ZMMT0052 OCCURS 0 WITH HEADER LINE,
         O_FLAG TYPE ZRESULT,
         O_MSG TYPE ZMSG.
  .

  CLEAR : LT_ZMMT0052,LT_ZMMT0052[].

  CALL FUNCTION 'ZMM_PART_SHORTAGE_DEMAND_HR'
*   IMPORTING
*     O_FLAG             =
*     O_MSG              =
    TABLES
      GCS_ZMMT0052       = LT_ZMMT0052
            .

  G_DEST = 'WMPM01'.

*  SELECT *
*    FROM ZMMT0052
*    INTO TABLE LT_ZMMT0052.

  IF NOT LT_ZMMT0052[] IS INITIAL.

    CALL FUNCTION 'ZMM_PART_SHORT_DEMAND_HR_GCS'
     DESTINATION G_DEST
     IMPORTING
        E_RETURN          = E_RETURN
     TABLES
       GCS_ZMMT0052       = LT_ZMMT0052
     EXCEPTIONS
       COMMUNICATION_FAILURE       = 1
       SYSTEM_FAILURE              = 2
       RESOURCE_FAILURE            = 3
       OTHERS                      = 4
              .

    DATA : LV_CNT(6) TYPE C.
    DESCRIBE TABLE LT_ZMMT0052 LINES GV_CNT.

    CASE SY-SUBRC.
      WHEN '0'.
        E_RETURN-TYPE     = 'S'.
        LV_CNT = GV_CNT.
        CONCATENATE LV_CNT 'Batch job Release' INTO E_RETURN-MESSAGE
        SEPARATED BY SPACE.
      WHEN '1'.
        E_RETURN-MESSAGE = 'communication_failure'.
        E_RETURN-TYPE    = 'E'.
      WHEN '2'.
        E_RETURN-MESSAGE = 'system_failure'.
        E_RETURN-TYPE    = 'E'.
      WHEN '3'.
        E_RETURN-MESSAGE = 'resource_failure'.
        E_RETURN-TYPE    = 'E'.
      WHEN '4'.
        E_RETURN-MESSAGE = 'Others'.
        E_RETURN-TYPE    = 'E'.
    ENDCASE.

**S__PAUL ADD 06/15/11
    LT_ZMMT0052-TYPE     = E_RETURN-TYPE.
    LT_ZMMT0052-MESSAGE  = E_RETURN-MESSAGE.
    LT_ZMMT0052-ZEDAT    = SY-DATUM.
    LT_ZMMT0052-ZETIM    = SY-UZEIT.
    MODIFY LT_ZMMT0052 TRANSPORTING ZEDAT ZETIM TYPE MESSAGE
                              WHERE ZBDAT <> SPACE.

    MODIFY ZMMT0052 FROM TABLE LT_ZMMT0052.
**E
    MESSAGE S999 WITH GV_CNT 'Data(s) was sent..'.
  ELSE.
    MESSAGE I999 WITH 'No Data found.'.
  ENDIF.

ENDFORM.                    " Part_shortage_demand
