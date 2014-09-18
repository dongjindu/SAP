************************************************************************
* Program Name      : ZIPP205I_ZTPPVHH
* Author            : DongYeop, Han
* Creation Date     : 2003.11.12.
* Specifications By : DongYeop, Han
* Pattern           : 1.1
* Development Request No : UD1K901977
* Addl Documentation:
* Description       : Transfer of Production Spec from PP to ALC
*                     (ztppvhh)
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  ZIPP205I_ZTPPVHH  NO STANDARD PAGE HEADING
                          LINE-SIZE 120
                          MESSAGE-ID ZDPP.

************************************************************************
*              D A T A     A R E A                                     *
************************************************************************
TABLES:ZTPP_SPEC.

DATA : BEGIN OF IT_SPEC OCCURS 0,
         WORDER   LIKE  ZTPP_SPEC-WORDER,    "W/O HEADER
         EXTC     LIKE  ZTPP_SPEC-EXTC,      "EXT COLOR
         INTC     LIKE  ZTPP_SPEC-INTC,      "INT COLOR
         MARK     LIKE  ZTPP_SPEC-MARK.      "IR/RP/DL
DATA : END OF IT_SPEC.

DATA : IT_WOHD      LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE,
       IT_WOCL      LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.
DATA : IT_ZSPPVHH   LIKE TABLE OF ZSPPVHH        WITH HEADER LINE,
       it_specvs    LIKE TABLE OF ztppvs         WITH HEADER LINE,
       IT_ZTPPVHH   LIKE TABLE OF ZTPPVHH        WITH HEADER LINE,
       IT_LIDT LIKE ZTBM_ABXPLIDT OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF IT_MESSAGE_H OCCURS 0,
       FUNCNAME(20),
       MSGTXT(100).
DATA : END OF IT_MESSAGE_H.

DATA: BEGIN OF IT_STATUS OCCURS 0,
        FCODE LIKE RSMPE-FUNC,
      END OF IT_STATUS.

* Field Symbol

*RANGES
RANGES R_MARK FOR ZTPP_SPEC-MARK.

*WORK AREA
DATA : wa_flag,              " No data check flag.
       WA_MATNR_CL           LIKE  MARA-MATNR,
       WA_MATNR_HD           LIKE  MARA-MATNR,
       WA_MODEL              LIKE  CONF_OUT-ATWRT.

DATA : WA_IR_VHH              LIKE  SY-TABIX,
       WA_RP_VHH              LIKE  SY-TABIX,
       WA_DL_VHH              LIKE  SY-TABIX.


*CONSTANTS
CONSTANTS: C_MODE    VALUE  'A',
           C_DEST(10) VALUE 'WMPP01'.   "Outbound Interface Destination

************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE T1.
POSITION 8.
PARAMETERS: C_TRANS AS CHECKBOX .
SELECT-OPTIONS: S_DATE FOR SY-DATUM NO-EXTENSION.
SELECTION-SCREEN: BEGIN OF LINE,
                  POSITION 8.
PARAMETERS : C_IR AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN: COMMENT 12(10)  TEXT-001  FOR FIELD C_IR,
                  POSITION 28.
PARAMETERS : C_RP AS CHECKBOX.
SELECTION-SCREEN: COMMENT 34(10)  TEXT-002  FOR FIELD C_RP,
                  POSITION 50.
PARAMETERS : C_DL AS CHECKBOX.
SELECTION-SCREEN: COMMENT 56(10)  TEXT-003  FOR FIELD C_DL,
                  END OF LINE,
                  END OF BLOCK B1.

************************************************************************
*              INITIALIZATION                                          *
************************************************************************
INITIALIZATION.

************************************************************************
*              AT SELECTION SCREEN                                     *
************************************************************************
AT SELECTION-SCREEN.

************************************************************************
*              START-OF-SELECTION PROCESSING                           *
************************************************************************
START-OF-SELECTION.
  PERFORM DATA_SELECT.
  PERFORM DATA_JOIN.
  PERFORM WRITE_OR_TRANSFER.
  SET PF-STATUS 'SUB_LIST'.

END-OF-SELECTION.

************************************************************************
*              TOP-OF-PAGE                                             *
************************************************************************
TOP-OF-PAGE.
  PERFORM HEAD_LINE.

************************************************************************
*             AT USER-COMMAND                                          *
************************************************************************
AT USER-COMMAND.
  PERFORM USER_COMMAND.

*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT
*&---------------------------------------------------------------------*
FORM DATA_SELECT.
  clear: wa_flag.
  PERFORM PROGRESS_INDICATOR USING 'CREATING SPEC DATA ....'.
  PERFORM RANGES_MARK.
*  SELECT WORDER EXTC INTC MARK
*     INTO TABLE IT_SPEC
*     FROM ZTPP_SPEC
*     WHERE MARK IN R_MARK.
  SORT IT_SPEC.
  IF IT_SPEC[] IS INITIAL.
    MESSAGE E001 WITH TEXT-004.
  ENDIF.
ENDFORM.                    " DATA_SELECT
*&---------------------------------------------------------------------*
*&      Form  RANGES_MARK
*&---------------------------------------------------------------------*
FORM RANGES_MARK.
  DATA: l_data                LIKE sy-datum.
    R_MARK-SIGN   = 'I'.
  CLEAR: l_data, IT_SPEC, IT_SPEC[].
  IF c_ir = 'X'.
    DELETE FROM ZTPPVHH CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
    r_mark-sign   = 'I'.      r_mark-option = 'EQ'.
    r_mark-low    = 'I'.      APPEND r_mark.
    SELECT worder extc intc mark
       INTO TABLE it_spec
       FROM ztpp_spec
       WHERE mark IN r_mark
         AND opdate = l_data.
  ENDIF.

  IF c_rp = 'X'.
    " Read Data in the Table ZTPPVS for the IR Data betwwen Date range..
    r_mark-sign   = 'I'.      r_mark-option = 'EQ'.
    r_mark-low    = 'R'.      APPEND r_mark.
    SELECT *  INTO TABLE it_specvs
       FROM ztppvs
       WHERE zsdat in s_date.

    loop at it_specvs  where flg = 'IR' .
       it_spec-worder  = it_specvs-matnr(14).
       it_spec-extc    = it_specvs-matnr+14(02).
       it_spec-intc    = it_specvs-matnr+16(02).
       it_spec-mark    = 'I'                   .
       append it_spec.
    endloop.
  ENDIF.

  IF c_dl = 'X'.
    r_mark-sign   = 'I'.      r_mark-option = 'EQ'.
    r_mark-low    = 'D'.      APPEND r_mark.
    SELECT worder extc intc mark
       INTO TABLE it_spec
       FROM ztpp_spec
       WHERE mark IN r_mark
         AND opdate = l_data.
  ENDIF.
ENDFORM.                    " RANGES_MARK
*&---------------------------------------------------------------------*
*&      Form  DATA_JOIN
*&---------------------------------------------------------------------*
FORM DATA_JOIN.
  SELECT * FROM ZTBM_ABXPLIDT
           INTO TABLE IT_LIDT.
  LOOP AT IT_LIDT.
    IT_ZSPPVHH-FLG  = 'IR'.            "DEFAULT 'IR'
    IT_ZSPPVHH-PLNT = IT_LIDT-PLNT.
    IT_ZSPPVHH-SHOP = IT_LIDT-SHOP.
    IT_ZSPPVHH-LINE = IT_LIDT-LINE.
    IT_ZSPPVHH-CARX = IT_LIDT-CARX.
    IT_ZSPPVHH-GUBN = IT_LIDT-GUBN.
    IT_ZSPPVHH-HPCC = IT_LIDT-HPCC.
    IF IT_LIDT-DGON IS INITIAL.
      IT_ZSPPVHH-DGON = IT_LIDT-GON1.
    ELSE.
      IT_ZSPPVHH-DGON = IT_LIDT-DGON.
    ENDIF.
    IT_ZSPPVHH-P_WO_CREATE_DATE = SY-DATUM.
    IT_ZSPPVHH-P_WO_CREATE_TIME = SY-UZEIT.
    SELECT SINGLE HEAD FROM ZTBM_ABXPCLDT
           INTO IT_ZSPPVHH-HEAD
           WHERE CARX = IT_LIDT-CARX
             AND GUBN = IT_LIDT-GUBN
             AND HPCC = IT_LIDT-HPCC.
    APPEND IT_ZSPPVHH. CLEAR IT_ZSPPVHH.
  ENDLOOP.
  SORT IT_ZSPPVHH.
  IF IT_ZSPPVHH[] IS INITIAL.
    MESSAGE w001 WITH TEXT-005.
    wa_flag = 'X'.
  ENDIF.
  DELETE ADJACENT DUPLICATES FROM IT_ZSPPVHH.
  DESCRIBE TABLE IT_ZSPPVHH LINES WA_IR_VHH.
ENDFORM.                    " DATA_JOIN

*&---------------------------------------------------------------------*
*&      Form  FTP_HANDLING_MASTER
*&---------------------------------------------------------------------*
FORM FTP_HANDLING_MASTER TABLES   ITAB
                         USING    P_MATNR.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = P_MATNR
            MODE         = 'R'
            CTYPE        = '001'
            DISPLAY      = 'X'
       TABLES
            VAL_TABLE    = ITAB
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            OTHERS       = 4.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  READ_WOHD_WOCL
*&---------------------------------------------------------------------*
FORM READ_WOHD_WOCL USING    P_IND P_ATNAM
                    CHANGING P_ATWRT.

  CASE P_IND.
    WHEN 'WOHD'.
      READ TABLE IT_WOHD WITH KEY ATNAM = P_ATNAM.
      IF SY-SUBRC EQ 0.
        P_ATWRT = IT_WOHD-ATWRT.
      ENDIF.
    WHEN 'WOCL'.
      READ TABLE IT_WOCL WITH KEY ATNAM = P_ATNAM.
      IF SY-SUBRC EQ 0.
        P_ATWRT = IT_WOCL-ATWRT.
      ENDIF.
  ENDCASE.
ENDFORM.                    " READ_WOHD_WOCL
*&---------------------------------------------------------------------*
*&      Form  WRITE_OR_TRANSFER
*&---------------------------------------------------------------------*
FORM WRITE_OR_TRANSFER.
  check wa_flag = space.
  IF C_TRANS = 'X'.
    PERFORM TRANSFER_TO_ALC.
  ELSE.
    IF NOT IT_ZSPPVHH[] IS INITIAL.
      PERFORM DATA_WRITE.
    ENDIF.
  ENDIF.
ENDFORM.                    " WRITE_OR_TRANSFER
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_TO_ALC
*&---------------------------------------------------------------------*
FORM TRANSFER_TO_ALC.
  DATA: L_CNT(6)       TYPE N,
        L_MSGTXT(100)        .

  PERFORM PROGRESS_INDICATOR USING 'TRANSFER FROM HPCC(H) TO ALC'.
*----> EAI TRANSFER
  DESCRIBE TABLE IT_ZSPPVHH LINES L_CNT.

  CALL FUNCTION 'Z_FPP_SET_ZTPPVHH'
    DESTINATION C_DEST
    EXPORTING
      IR             = WA_IR_VHH
      RP             = WA_RP_VHH
      DL             = WA_DL_VHH
    TABLES
      I_ZSPPVHH      = IT_ZSPPVHH
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
      SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT.

  IF SY-SUBRC <> 0.
    IT_MESSAGE_H-FUNCNAME = 'Z_FPP_SET_ZTPPVHH'.
    IT_MESSAGE_H-MSGTXT   = L_MSGTXT.
    APPEND IT_MESSAGE_H. CLEAR IT_MESSAGE_H.
  ELSE.
    LOOP AT IT_ZSPPVHH.
      MOVE-CORRESPONDING IT_ZSPPVHH TO IT_ZTPPVHH.
      MOVE SY-UNAME    TO  IT_ZTPPVHH-ZUSER.
      MOVE SY-DATUM    TO  IT_ZTPPVHH-ZSDAT.
      MOVE SY-UZEIT    TO  IT_ZTPPVHH-ZSTIM.
      APPEND IT_ZTPPVHH. CLEAR IT_ZTPPVHH.
    ENDLOOP.
    IF NOT IT_ZTPPVHH[] IS INITIAL.
      MODIFY ZTPPVHH FROM TABLE IT_ZTPPVHH.
      IF SY-SUBRC = 0.
        COMMIT WORK.
        IT_MESSAGE_H-FUNCNAME = 'ZTPPVHH'.
        CONCATENATE L_CNT TEXT-101  INTO IT_MESSAGE_H-MSGTXT
                                    SEPARATED BY SPACE       .
        APPEND IT_MESSAGE_H. CLEAR IT_MESSAGE_H.
      ELSE.
        ROLLBACK WORK.
        IT_MESSAGE_H-FUNCNAME = 'ZTPPVHH'.
        IT_MESSAGE_H-MSGTXT   =  TEXT-102.
        APPEND IT_MESSAGE_H. CLEAR IT_MESSAGE_H.
      ENDIF.
    ENDIF.
  ENDIF.
  IF SY-UCOMM = 'TRANS'.
    MOVE 'TRANS' TO IT_STATUS-FCODE.
    APPEND IT_STATUS. CLEAR IT_STATUS.
    SET PF-STATUS 'SUB_LIST' EXCLUDING IT_STATUS.
    LOOP AT IT_MESSAGE_H.
      WRITE:/ IT_MESSAGE_H.
    ENDLOOP.
  ELSE.
    EXPORT IT_MESSAGE_H  TO MEMORY ID 'MEMO'.
  ENDIF.
ENDFORM.                    " TRANSFER_TO_ALC
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
FORM PROGRESS_INDICATOR USING   P_TEXT.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT = P_TEXT.
ENDFORM.                    " PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  DATA_WRITE
*&---------------------------------------------------------------------*
FORM DATA_WRITE.
  DATA:ZEBRA.

  LOOP AT IT_ZSPPVHH.
    ZEBRA = SY-TABIX MOD 2.
    IF ZEBRA = 0.
      FORMAT COLOR 2 INTENSIFIED OFF.
    ELSE.
      FORMAT COLOR 2 INTENSIFIED ON.
    ENDIF.

    WRITE AT:/1(4)   IT_ZSPPVHH-FLG,
              6(4)   IT_ZSPPVHH-PLNT,
              11(4)  IT_ZSPPVHH-SHOP,
              16(4)  IT_ZSPPVHH-LINE,
              21(4)  IT_ZSPPVHH-CARX,
              26(4)  IT_ZSPPVHH-GUBN,
              31(4)  IT_ZSPPVHH-HPCC,
              36(8)  IT_ZSPPVHH-DGON,
              45(4)  IT_ZSPPVHH-LOCA,
              50(22) IT_ZSPPVHH-HEAD,
              73(11)  IT_ZSPPVHH-P_WO_CREATE_DATE,
              85(11)  IT_ZSPPVHH-P_WO_CREATE_TIME.

  ENDLOOP.
*  SET LEFT SCROLL-BOUNDARY COLUMN 35.
ENDFORM.                    " DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  HEAD_LINE
*&---------------------------------------------------------------------*
FORM HEAD_LINE.
  FORMAT COLOR 5 INTENSIFIED OFF.
  WRITE AT:/1(4)    TEXT-006,
            6(4)    TEXT-007,
            11(4)   TEXT-008,
            16(4)   TEXT-009,
            21(4)   TEXT-010,
            26(4)   TEXT-011,
            31(4)   TEXT-012,
            36(8)   TEXT-013,
            45(4)   TEXT-014,
            50(22)  TEXT-015,
            73(11)  TEXT-016,
            85(11)  TEXT-017.


ENDFORM.                    " HEAD_LINE
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM USER_COMMAND.
  CASE SY-UCOMM.
    WHEN 'TRANS'.
      PERFORM TRANSFER_TO_ALC.
  ENDCASE.
ENDFORM.                    " USER_COMMAND
