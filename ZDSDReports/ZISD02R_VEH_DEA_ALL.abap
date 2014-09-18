************************************************************************
* Author                 : jun ho choi
* Creation Date          : 2003-11-11
* Specifications By      :
* Development Request No : UD1K901594
* Pattern                : 3-1
* Addl documentation     :
* Description            : Inbound interface from HMA/HAC
*                          (Dealer allocation)
*
*
* Modification Log
* Date       Developer    Request ID Description
*
************************************************************************
REPORT ZISD02R_VEH_DEA_ALL NO STANDARD PAGE HEADING
                           MESSAGE-ID ZMSD
                           LINE-SIZE 135.


*
TABLES : ZTSD_VEH_DEA_ALL.

TABLES : AUSP, CABN.


*
DATA : BEGIN OF IT_IN2 OCCURS 0.
       INCLUDE STRUCTURE ZTSD_VEH_DEA_ALL.
DATA : END OF IT_IN2.

DATA : BEGIN OF BDC_TAB OCCURS 0.
       INCLUDE STRUCTURE BDCDATA.
DATA : END OF BDC_TAB.

DATA : BEGIN OF MESS_TAB OCCURS 0.
       INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF MESS_TAB.

DATA : BEGIN OF BDC_LIST OCCURS 0,
       MESSAGE(75).

       INCLUDE STRUCTURE ZTSD_VEH_DEA_ALL.
DATA : END OF BDC_LIST.

DATA : W_CNT TYPE I,
       W_CNT_S TYPE I,
       W_CNT_E TYPE I,
       W_INDEX LIKE SY-TABIX,
       W_OBJEK LIKE AUSP-OBJEK,
       WWW(1).


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_MODE(1) DEFAULT 'N'.
SELECTION-SCREEN END OF BLOCK B1.


*
TOP-OF-PAGE.
  PERFORM TOP_OF_PAGE.


*
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM TOP_OF_PAGE.


*
START-OF-SELECTION.
  SET PF-STATUS 'ISD02R'.
  PERFORM GET_DATA.
  PERFORM BDC_PROCESS.
  PERFORM DISPLAY_RESULT.


*
END-OF-SELECTION.


*
AT USER-COMMAND.
  PERFORM USER_COMMAND.


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
  REFRESH : IT_IN2.
  CLEAR   : IT_IN2.

  SELECT *
         INTO TABLE IT_IN2
         FROM ZTSD_VEH_DEA_ALL
        WHERE UPRSLT EQ ''.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.
  DESCRIBE TABLE IT_IN2 LINES W_CNT.
  IF W_CNT = 0.
    SKIP 5.
    WRITE:/ 'No Entry'.
    STOP.
  ENDIF.

  LOOP AT IT_IN2.
    MOVE-CORRESPONDING IT_IN2 TO BDC_LIST.

    PERFORM SAPGUI_PROGRESS_INDICATOR USING 1.

    SELECT SINGLE *
           FROM CABN
          WHERE ATNAM = 'P_VIN'.
    SELECT SINGLE *
           FROM AUSP
          WHERE ATINN EQ CABN-ATINN
          AND   ATWRT EQ IT_IN2-FVIN.
    IF SY-SUBRC = 0.
      W_OBJEK = AUSP-OBJEK.
    ELSE.
      BDC_LIST-MESSAGE = 'NOT_FOUND_OBJECT'.
      APPEND BDC_LIST. CLEAR BDC_LIST.
      CONTINUE.
    ENDIF.

    REFRESH : BDC_TAB, MESS_TAB.
    CLEAR   : BDC_TAB, MESS_TAB.

    PERFORM BDC_IE02_DEA_ALL USING IT_IN2-DECOMM IT_IN2-DEALER.

    WWW = 'N'.
    CALL TRANSACTION 'IE02' USING BDC_TAB MODE WWW "'N'
                                  UPDATE 'S'
                                  MESSAGES INTO MESS_TAB.
    READ TABLE MESS_TAB WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
             MSGID               = MESS_TAB-MSGID
             MSGNR               = MESS_TAB-MSGNR
             MSGV1               = MESS_TAB-MSGV1
             MSGV2               = MESS_TAB-MSGV2
             MSGV3               = MESS_TAB-MSGV3
             MSGV4               = MESS_TAB-MSGV4
           IMPORTING
             MESSAGE_TEXT_OUTPUT = BDC_LIST-MESSAGE.
      APPEND BDC_LIST. CLEAR BDC_LIST.
      CONTINUE.
    ELSE.
      READ TABLE MESS_TAB WITH KEY MSGTYP = 'S'
                                   MSGID  = 'IS'
                                   MSGNR  = '817'.
      IF SY-SUBRC <> 0.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
             EXPORTING
               MSGID               = MESS_TAB-MSGID
               MSGNR               = MESS_TAB-MSGNR
               MSGV1               = MESS_TAB-MSGV1
               MSGV2               = MESS_TAB-MSGV2
               MSGV3               = MESS_TAB-MSGV3
               MSGV4               = MESS_TAB-MSGV4
             IMPORTING
               MESSAGE_TEXT_OUTPUT = BDC_LIST-MESSAGE.
        APPEND BDC_LIST. CLEAR BDC_LIST.
        CONTINUE.
      ELSE.
        UPDATE ZTSD_VEH_DEA_ALL SET : UPRSLT = 'Y'
                                WHERE FVIN = IT_IN2-FVIN.
        BDC_LIST-UPRSLT = 'Y'.
        BDC_LIST-MESSAGE = ''.
        APPEND BDC_LIST. CLEAR BDC_LIST.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SAPGUI_PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
FORM SAPGUI_PROGRESS_INDICATOR USING GUBUN.
DATA : W_PERC TYPE P DECIMALS 2,
       W_TEXT(50).

  W_PERC = SY-TABIX / W_CNT * 100.
  WRITE W_PERC TO W_TEXT+0(7).
  CASE GUBUN.
  WHEN '1'.
    CONCATENATE W_TEXT 'Update dealer in vehicle master'
                INTO W_TEXT SEPARATED BY SPACE.
  ENDCASE.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      PERCENTAGE       = W_PERC
      TEXT             = W_TEXT.
ENDFORM.                    " SAPGUI_PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS_REST
*&---------------------------------------------------------------------*
FORM BDC_PROCESS_REST.
* BDC PROCESS

  SELECT SINGLE *
         FROM CABN
        WHERE ATNAM = 'P_VIN'.
  SELECT SINGLE *
         FROM AUSP
        WHERE ATINN EQ CABN-ATINN
        AND   ATWRT EQ BDC_LIST-FVIN.
  IF SY-SUBRC = 0.
    W_OBJEK = AUSP-OBJEK.
  ELSE.
    BDC_LIST-MESSAGE = 'NOT_FOUND_OBJECT'.
    MODIFY BDC_LIST INDEX W_INDEX.
    EXIT.
  ENDIF.

  REFRESH : BDC_TAB, MESS_TAB.
  CLEAR   : BDC_TAB, MESS_TAB.

  PERFORM BDC_IE02_DEA_ALL USING BDC_LIST-DECOMM BDC_LIST-DEALER.

  WWW = 'N'.
  CALL TRANSACTION 'IE02' USING BDC_TAB MODE WWW "'N'
                                UPDATE 'S'
                                MESSAGES INTO MESS_TAB.
  READ TABLE MESS_TAB WITH KEY MSGTYP = 'E'.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
           MSGID               = MESS_TAB-MSGID
           MSGNR               = MESS_TAB-MSGNR
           MSGV1               = MESS_TAB-MSGV1
           MSGV2               = MESS_TAB-MSGV2
           MSGV3               = MESS_TAB-MSGV3
           MSGV4               = MESS_TAB-MSGV4
         IMPORTING
           MESSAGE_TEXT_OUTPUT = BDC_LIST-MESSAGE.
    MODIFY BDC_LIST INDEX W_INDEX.
  ELSE.
    READ TABLE MESS_TAB WITH KEY MSGTYP = 'S'
                                 MSGID  = 'IS'
                                 MSGNR  = '817'.
    IF SY-SUBRC <> 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
             MSGID               = MESS_TAB-MSGID
             MSGNR               = MESS_TAB-MSGNR
             MSGV1               = MESS_TAB-MSGV1
             MSGV2               = MESS_TAB-MSGV2
             MSGV3               = MESS_TAB-MSGV3
             MSGV4               = MESS_TAB-MSGV4
           IMPORTING
             MESSAGE_TEXT_OUTPUT = BDC_LIST-MESSAGE.
      MODIFY BDC_LIST INDEX W_INDEX.
    ELSE.
      UPDATE ZTSD_VEH_DEA_ALL SET : UPRSLT = 'Y'
                              WHERE FVIN = BDC_LIST-FVIN.
      BDC_LIST-UPRSLT = 'Y'.
      BDC_LIST-MESSAGE = ''.
      MODIFY BDC_LIST INDEX W_INDEX.
    ENDIF.
  ENDIF.
ENDFORM.                    " BDC_PROCESS_REST
*&---------------------------------------------------------------------*
*&      Form  BDC_IE02_DEA_ALL
*&---------------------------------------------------------------------*
FORM BDC_IE02_DEA_ALL USING DCOM DEA.
  PERFORM BDC_FILL USING :
          'X' 'SAPMIEQ0'             '0100',
          ' ' 'RM63E-EQUNR'          W_OBJEK,
          ' ' 'BDC_OKCODE'           '/00',
          'X' 'SAPMIEQ0'             '0101',
          ' ' 'BDC_OKCODE'           '=KL',
          'X' 'SAPLCLCA'             '0602',
          ' ' 'RMCLF-KLART'          '002',
          ' ' 'BDC_OKCODE'           '=ENTE',
          'X' 'SAPLCLFM'             '0500',
          ' ' 'RMCLF-KREUZ(01)'      'X',
          ' ' 'BDC_OKCODE'           '=AUSW'.

  IF DCOM = 'H'.
  PERFORM BDC_FILL USING :
          'X' 'SAPLCTMS'             '0109',
          ' ' 'RCTMS-MNAME(01)'      'P_TBD',
          ' ' 'RCTMS-MWERT(01)'      DCOM,
          ' ' 'BDC_OKCODE'           '=BACK'.
  ELSE.
  PERFORM BDC_FILL USING :
          'X' 'SAPLCTMS'             '0109',
          ' ' 'RCTMS-MNAME(01)'      'P_DEALER_CODE',
          ' ' 'RCTMS-MWERT(01)'      DEA,
          ' ' 'BDC_OKCODE'           '=BACK'.
  ENDIF.

  PERFORM BDC_FILL USING :
          'X' 'SAPLCLFM'             '0500',
          ' ' 'BDC_OKCODE'           '=ENDE',
          'X' 'SAPMIEQ0'             '0101',
          ' ' 'BDC_OKCODE'           '=BU'.
ENDFORM.                    " BDC_IE02_DEA_ALL
*&---------------------------------------------------------------------*
*&      Form  BDC_FILL
*&---------------------------------------------------------------------*
FORM BDC_FILL USING    P1 P2 P3.
  CLEAR BDC_TAB.
  IF P1 = 'X'.
    BDC_TAB-DYNBEGIN = P1.
    BDC_TAB-PROGRAM  = P2.
    BDC_TAB-DYNPRO   = P3.
  ELSE.
    BDC_TAB-DYNBEGIN = P1.
    BDC_TAB-FNAM     = P2.
    BDC_TAB-FVAL     = P3.
  ENDIF.
  APPEND BDC_TAB.
ENDFORM.                    " BDC_FILL
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULT
*&---------------------------------------------------------------------*
FORM DISPLAY_RESULT.
  WRITE:/ ''.
  LOOP AT BDC_LIST.
    WRITE:/ SY-VLINE.
    CASE BDC_LIST-UPRSLT.
      WHEN 'Y'.
        FORMAT COLOR COL_POSITIVE.
        WRITE: (02) BDC_LIST-UPRSLT.
        FORMAT COLOR COL_POSITIVE OFF.
      WHEN ''.
        FORMAT COLOR COL_NEGATIVE.
        WRITE: (02) BDC_LIST-UPRSLT.
        FORMAT COLOR COL_NEGATIVE OFF.
      WHEN OTHERS.
        WRITE: (02) ''.
    ENDCASE.

    WRITE:  SY-VLINE, (03) BDC_LIST-RECTY,
            SY-VLINE, (17) BDC_LIST-FVIN,
            SY-VLINE, (01) BDC_LIST-DECOMM,
            SY-VLINE, (05) BDC_LIST-DEALER,
            SY-VLINE, (08) BDC_LIST-PDATE,
            SY-VLINE, (75) BDC_LIST-MESSAGE,
            SY-VLINE.

    W_INDEX = SY-TABIX.
    HIDE : W_INDEX.

    WRITE:/(134) SY-ULINE.
  ENDLOOP.
ENDFORM.                    " DISPLAY_RESULT
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  DESCRIBE TABLE BDC_LIST LINES W_CNT.
  WRITE:/ 'Total   records :', W_CNT.

  W_CNT_S = 0. W_CNT_E = 0.
  LOOP AT BDC_LIST.
    IF BDC_LIST-UPRSLT = 'S'.
      W_CNT_S = W_CNT_S + 1.
    ENDIF.
    IF BDC_LIST-UPRSLT <> 'S'.
      W_CNT_E = W_CNT_E + 1.
    ENDIF.
  ENDLOOP.
  WRITE:/ 'Success records :', W_CNT_S.
  WRITE:/ 'Error   records :', W_CNT_E.

  FORMAT COLOR COL_HEADING.
  WRITE:/(134) SY-ULINE.
  WRITE:/ SY-VLINE, (02) 'UP',
          SY-VLINE, (03) 'Rec',
          SY-VLINE, (17) 'Full VIN',
          SY-VLINE, (01) 'D',
          SY-VLINE, (06) 'Dealer',
          SY-VLINE, (08) 'ProcDate',
          SY-VLINE, (75) 'Message',
          SY-VLINE.
  WRITE:/(134) SY-ULINE.
  FORMAT COLOR COL_HEADING OFF.
ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM USER_COMMAND.
  DATA : OK_CODE(4).
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'BACK'.
      SET SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'REST'.
      PERFORM RESTARTING.
      SY-LSIND = SY-LSIND - 1.
      PERFORM DISPLAY_RESULT.
  ENDCASE.
ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  RESTARTING
*&---------------------------------------------------------------------*
FORM RESTARTING.
  IF SY-LISEL+3(1) EQ ' '.
    READ TABLE BDC_LIST INDEX W_INDEX.
    IF SY-SUBRC = 0.
      IF BDC_LIST-UPRSLT NE 'Y'.
        PERFORM BDC_PROCESS_REST.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE I000 WITH TEXT-M02.
  ENDIF.
ENDFORM.                    " RESTARTING
