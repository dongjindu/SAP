************************************************************************
* Author                 : jun ho choi
* Creation Date          : 2003-09-01
* Specifications By      :
* Development Request No : UD1K901594
* Pattern                : 3-1
* Addl documentation     :
* Description            : Vehicle Good Issue Interface
*
*
*
* Modification Log
* Date       Developer    Request ID Description
*
************************************************************************
REPORT ZISD03U_VEHICLE_GOODISSUE NO STANDARD PAGE HEADING
                                 MESSAGE-ID ZMSD
                                 LINE-SIZE 168.
*
*
**
*TABLES : YTPP_BFLOG,
*         VBAK,
*         CABN,
*         AUSP.
*
*
**
*DATA : BEGIN OF IT_BFLOG OCCURS 0.
*       INCLUDE STRUCTURE YTPP_BFLOG.
*DATA : END OF IT_BFLOG.
*
*DATA : BEGIN OF BDC_TAB OCCURS 0.
*       INCLUDE STRUCTURE BDCDATA.
*DATA : END OF BDC_TAB.
*
*DATA : BEGIN OF MESS_TAB OCCURS 0.
*       INCLUDE STRUCTURE BDCMSGCOLL.
*DATA : END OF MESS_TAB.
*
*DATA : BEGIN OF BDC_LIST OCCURS 0,
*       GUBUN_D(1),
*       GUBUN_B(1),
*       MESSAGE(75),
*
*       EQUNO LIKE YTPP_BFLOG-EQUNO,
*       END OF BDC_LIST.
*
*DATA : W_CNT TYPE I,
*       W_OBJEK LIKE AUSP-OBJEK,
*       W_VBELN LIKE VBAK-VBELN,
*       W_CNT_S TYPE I,
*       W_CNT_E TYPE I,
*       W_INDEX LIKE SY-TABIX.
*
*
**
*SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*PARAMETERS : P_MODE(1) DEFAULT 'N'.
*SELECTION-SCREEN END OF BLOCK B1.
*
*
**
*TOP-OF-PAGE.
*  PERFORM TOP_OF_PAGE.
*
*
**
*TOP-OF-PAGE DURING LINE-SELECTION.
*  PERFORM TOP_OF_PAGE.
*
*
**
*START-OF-SELECTION.
*  SET PF-STATUS 'ESD02U'.
*  PERFORM GET_DATA.
*  PERFORM BDC_PROCESS.
*  PERFORM DISPLAY_RESULT.
*
*
**
*END-OF-SELECTION.
*
*
**
*AT USER-COMMAND.
*  PERFORM USER_COMMAND.
*
*
**&---------------------------------------------------------------------
*
**&      Form  GET_DATA
**&---------------------------------------------------------------------
*
*FORM GET_DATA.
*  REFRESH : IT_BFLOG.
*  CLEAR   : IT_BFLOG.
*
*  SELECT *
*         INTO TABLE IT_BFLOG
*         FROM YTPP_BFLOG
*        WHERE GIRES NE 'Y'.
*ENDFORM.                    " GET_DATA
**&---------------------------------------------------------------------
*
**&      Form  BDC_PROCESS
**&---------------------------------------------------------------------
*
*FORM BDC_PROCESS.
*  DESCRIBE TABLE IT_BFLOG LINES W_CNT.
*  IF W_CNT = 0.
*    SKIP 5.
*    WRITE:/ 'No Entry'.
*    STOP.
*  ENDIF.
*
*  LOOP AT IT_BFLOG.
*    PERFORM SAPGUI_PROGRESS_INDICATOR USING 1.
*
*    PERFORM GET_DELIVERY_NO.
*
*    REFRESH BDC_TAB. CLEAR BDC_TAB.
*
*    PERFORM MOVE_IT_BFLOG_2_BDC_LIST.
*
*    PERFORM BDC_FILL USING :
*            'X' 'SAPMV50A'             '4004',
*            ' ' 'LIKP-VBELN'           W_VBELN,
*            ' ' 'BDC_OKCODE'           '=WABU_T'.
*
*    CALL TRANSACTION 'VL02N' USING BDC_TAB MODE P_MODE
*                                   UPDATE 'S'
*                                   MESSAGES INTO MESS_TAB.
*    READ TABLE MESS_TAB WITH KEY MSGTYP = 'E'.
*    IF SY-SUBRC = 0.
*      BDC_LIST-GUBUN_D = 'E'.
*      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*           EXPORTING
*             MSGID               = MESS_TAB-MSGID
*             MSGNR               = MESS_TAB-MSGNR
*             MSGV1               = MESS_TAB-MSGV1
*             MSGV2               = MESS_TAB-MSGV2
*             MSGV3               = MESS_TAB-MSGV3
*             MSGV4               = MESS_TAB-MSGV4
*           IMPORTING
*             MESSAGE_TEXT_OUTPUT = BDC_LIST-MESSAGE.
*    ELSE.
*      READ TABLE MESS_TAB WITH KEY MSGTYP = 'S'
*                                   MSGID  = 'VL'
*                                   MSGNR  = '311'.
*      IF SY-SUBRC = 0.
*        BDC_LIST-GUBUN_D = 'S'.
*        PERFORM UPDATE_BFLOG.
*        IF SY-SUBRC = 0.
*          BDC_LIST-GUBUN_B = 'S'.
*        ELSE.
*          BDC_LIST-GUBUN_B = 'E'.
*        ENDIF.
*      ELSE.
*        BDC_LIST-GUBUN_D = 'E'.
*        READ TABLE MESS_TAB INDEX 1.
*        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*             EXPORTING
*               MSGID               = MESS_TAB-MSGID
*               MSGNR               = MESS_TAB-MSGNR
*               MSGV1               = MESS_TAB-MSGV1
*               MSGV2               = MESS_TAB-MSGV2
*               MSGV3               = MESS_TAB-MSGV3
*               MSGV4               = MESS_TAB-MSGV4
*             IMPORTING
*               MESSAGE_TEXT_OUTPUT = BDC_LIST-MESSAGE.
*      ENDIF.
*    ENDIF.
*
*    APPEND BDC_LIST. CLEAR BDC_LIST.
*  ENDLOOP.
*ENDFORM.                    " BDC_PROCESS
**&---------------------------------------------------------------------
*
**&      Form  SAPGUI_PROGRESS_INDICATOR
**&---------------------------------------------------------------------
*
*FORM SAPGUI_PROGRESS_INDICATOR USING GUBUN.
*DATA : W_PERC TYPE P DECIMALS 2,
*       W_TEXT(50).
*
*  W_PERC = SY-TABIX / W_CNT * 100.
*  WRITE W_PERC TO W_TEXT+0(7).
*  CASE GUBUN.
*  WHEN '1'.
*    CONCATENATE W_TEXT 'Posting good issue'
*                INTO W_TEXT SEPARATED BY SPACE.
*  ENDCASE.
*
*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*    EXPORTING
*      PERCENTAGE       = W_PERC
*      TEXT             = W_TEXT.
*ENDFORM.                    " SAPGUI_PROGRESS_INDICATOR
**&---------------------------------------------------------------------
*
**&      Form  GET_DELIVERY_NO
**&---------------------------------------------------------------------
*
*FORM GET_DELIVERY_NO.
**  SELECT SINGLE *
**         FROM CABN
**        WHERE ATNAM = 'P_VIN'.
**  SELECT SINGLE *
**         FROM AUSP
**        WHERE ATINN EQ CABN-ATINN
**        AND   ATWRT EQ IT_BFLOG-VIN.
**  IF SY-SUBRC = 0.
**    W_OBJEK = AUSP-OBJEK.
**  ENDIF.
*  W_OBJEK = IT_BFLOG-EQUNO.
*
*  SELECT SINGLE *
*         FROM CABN
*        WHERE ATNAM = 'P_DELIVERY_NO'.
*  SELECT SINGLE *
*         FROM AUSP
*        WHERE OBJEK EQ W_OBJEK
*        AND   ATINN EQ CABN-ATINN.
*  IF SY-SUBRC = 0.
*    W_VBELN = AUSP-ATWRT.
*  ENDIF.
*ENDFORM.                    " GET_DELIVERY_NO
**&---------------------------------------------------------------------
*
**&      Form  MOVE_IT_BFLOG_2_BDC_LIST
**&---------------------------------------------------------------------
*
*FORM MOVE_IT_BFLOG_2_BDC_LIST.
*  BDC_LIST-EQUNO = IT_BFLOG-EQUNO.
*ENDFORM.                    " MOVE_IT_BFLOG_2_BDC_LIST
**&---------------------------------------------------------------------
*
**&      Form  BDC_FILL
**&---------------------------------------------------------------------
*
*FORM BDC_FILL USING    P1 P2 P3.
*  CLEAR BDC_TAB.
*  IF P1 = 'X'.
*     BDC_TAB-DYNBEGIN = P1.
*     BDC_TAB-PROGRAM  = P2.
*     BDC_TAB-DYNPRO   = P3.
*  ELSE.
*     BDC_TAB-DYNBEGIN = P1.
*     BDC_TAB-FNAM     = P2.
*     BDC_TAB-FVAL     = P3.
*  ENDIF.
*  APPEND BDC_TAB.
*ENDFORM.                    " BDC_FILL
**&---------------------------------------------------------------------
*
**&      Form  UPDATE_BFLOG
**&---------------------------------------------------------------------
*
*FORM UPDATE_BFLOG.
*  UPDATE YTPP_BFLOG SET : GIRES = 'Y'
*                          GDATE = SY-DATUM
*                          GTIME = SY-UZEIT
*                    WHERE EQUNO = IT_BFLOG-EQUNO.
*ENDFORM.                    " UPDATE_BFLOG
**&---------------------------------------------------------------------
*
**&      Form  DISPLAY_RESULT
**&---------------------------------------------------------------------
*
*FORM DISPLAY_RESULT.
*  WRITE:/ ''.
*  LOOP AT BDC_LIST.
*    WRITE:/ SY-VLINE.
*    CASE BDC_LIST-GUBUN_D.
*      WHEN 'S'.
*        FORMAT COLOR COL_POSITIVE.
*        WRITE: (02) BDC_LIST-GUBUN_D.
*        FORMAT COLOR COL_POSITIVE OFF.
*      WHEN 'E'.
*        FORMAT COLOR COL_NEGATIVE.
*        WRITE: (02) BDC_LIST-GUBUN_D.
*        FORMAT COLOR COL_NEGATIVE OFF.
*      WHEN OTHERS.
*        WRITE: (02) ''.
*    ENDCASE.
*
*    WRITE:  SY-VLINE.
*    CASE BDC_LIST-GUBUN_B.
*      WHEN 'S'.
*        FORMAT COLOR COL_POSITIVE.
*        WRITE: (02) BDC_LIST-GUBUN_B.
*        FORMAT COLOR COL_POSITIVE OFF.
*      WHEN 'E'.
*        FORMAT COLOR COL_NEGATIVE.
*        WRITE: (02) BDC_LIST-GUBUN_B.
*        FORMAT COLOR COL_NEGATIVE OFF.
*      WHEN OTHERS.
*        WRITE: (02) ''.
*    ENDCASE.
*
*    WRITE:  SY-VLINE, (18) BDC_LIST-EQUNO,
*            SY-VLINE, (75) BDC_LIST-MESSAGE,
*            SY-VLINE.
*
*    W_INDEX = SY-TABIX.
*    HIDE : W_INDEX.
*
*    WRITE:/(110) SY-ULINE.
*  ENDLOOP.
*ENDFORM.                    " DISPLAY_RESULT
**&---------------------------------------------------------------------
*
**&      Form  TOP_OF_PAGE
**&---------------------------------------------------------------------
*
*FORM TOP_OF_PAGE.
*  DESCRIBE TABLE BDC_LIST LINES W_CNT.
*  WRITE:/ 'Total   records :', W_CNT.
*
*  W_CNT_S = 0. W_CNT_E = 0.
*  LOOP AT BDC_LIST.
*    IF BDC_LIST-GUBUN_D = 'S'.
*      W_CNT_S = W_CNT_S + 1.
*    ENDIF.
*    IF BDC_LIST-GUBUN_D <> 'S'.
*      W_CNT_E = W_CNT_E + 1.
*    ENDIF.
*  ENDLOOP.
*  WRITE:/ 'Success records :', W_CNT_S.
*  WRITE:/ 'Error   records :', W_CNT_E.
*
*  FORMAT COLOR COL_HEADING.
*  WRITE:/(110) SY-ULINE.
*  WRITE:/ SY-VLINE, (02) 'GI',
*          SY-VLINE, (02) 'BF',
*          SY-VLINE, (18) 'Equipment No',
*          SY-VLINE, (75) 'Message',
*          SY-VLINE.
*  WRITE:/(110) SY-ULINE.
*  FORMAT COLOR COL_HEADING OFF.
*ENDFORM.                    " TOP_OF_PAGE
**&---------------------------------------------------------------------
*
**&      Form  USER_COMMAND
**&---------------------------------------------------------------------
*
*FORM USER_COMMAND.
*  DATA : OK_CODE(4).
*  OK_CODE = SY-UCOMM.
*  CLEAR SY-UCOMM.
*
*  CASE OK_CODE.
*    WHEN 'BACK'.
*      SET SCREEN 0.
*    WHEN 'EXIT'.
*      LEAVE PROGRAM.
*    WHEN 'REST'.
*      PERFORM RESTARTING.
*      SY-LSIND = SY-LSIND - 1.
*      PERFORM DISPLAY_RESULT.
*  ENDCASE.
*ENDFORM.                    " USER_COMMAND
**&---------------------------------------------------------------------
*
**&      Form  RESTARTING
**&---------------------------------------------------------------------
*
*FORM RESTARTING.
*  IF SY-LISEL+3(1) EQ ' '.
*    READ TABLE BDC_LIST INDEX W_INDEX.
*    IF SY-SUBRC = 0.
*      IF BDC_LIST-GUBUN_D EQ 'S' AND
*         BDC_LIST-GUBUN_B NE 'S'.
*        PERFORM UPDATE_BFLOG.
*        IF SY-SUBRC = 0.
*          BDC_LIST-GUBUN_B = 'S'.
*        ELSE.
*          BDC_LIST-GUBUN_B = 'E'.
*        ENDIF.
*
*        MODIFY BDC_LIST INDEX W_INDEX.
*      ENDIF.
*    ENDIF.
*  ELSE.
*    MESSAGE I000 WITH TEXT-M02.
*  ENDIF.
*ENDFORM.                    " RESTARTING
