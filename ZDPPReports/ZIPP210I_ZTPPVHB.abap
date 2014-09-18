************************************************************************
* Program Name      : ZIPP210I_ZTPPVHB
* Author            : DongYeop, Han
* Creation Date     : 2003.11.12.
* Specifications By : DongYeop, Han
* Pattern           : 1.1
* Development Request No : UD1K901977
* Addl Documentation:
* Description       : Transfer of Production Spec from PP to ALC
*                     (ztppvhb)
* Modification Logs
* Date       Developer    RequestNo    Description
* 12/09/2004  Shiva       UD1K913430   1. Corrected the filed name of
*                                         the internal table IT_ZSPPVHB.
*                                      2. In the perform READ_WOHD_WOCL
*                        used variable instead of filed symbol parameter
*                       and then assigned value to field symbol.
************************************************************************
REPORT  ZIPP210I_ZTPPVHB NO STANDARD PAGE HEADING
                         LINE-SIZE 150
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
*      IT_WOCL      LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.
       it_specvs    LIKE TABLE OF ztppvs         WITH HEADER LINE,
       IT_ZSPPVHB   LIKE TABLE OF ZSPPVHB        WITH HEADER LINE,
       IT_ZTPPVHB   LIKE TABLE OF ZTPPVHB        WITH HEADER LINE,
       IT_LIDT      LIKE TABLE OF ZTBM_ABXPLIDT  WITH HEADER LINE.

DATA : BEGIN OF IT_MESSAGE_B OCCURS 0,
       FUNCNAME(20),
       MSGTXT(100).
DATA : END OF IT_MESSAGE_B.

DATA: BEGIN OF IT_STATUS OCCURS 0,
        FCODE LIKE RSMPE-FUNC,
      END OF IT_STATUS.

* Field Symbol
FIELD-SYMBOLS : <HPCB>.

*RANGES
RANGES R_MARK FOR ZTPP_SPEC-MARK.

*WORK AREA
DATA : wa_flag,              " No data check flag.
       WA_MATNR_CL           LIKE  MARA-MATNR,
       WA_MATNR_HD           LIKE  MARA-MATNR,
       WA_MODEL              LIKE  CONF_OUT-ATWRT.

DATA : WA_IR_VHB              LIKE  SY-TABIX,
       WA_RP_VHB              LIKE  SY-TABIX,
       WA_DL_VHB              LIKE  SY-TABIX.


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
  SORT IT_SPEC BY WORDER .
  DELETE ADJACENT DUPLICATES FROM IT_SPEC COMPARING WORDER .
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
    DELETE FROM ZTPPVHB CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
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
  LOOP AT IT_SPEC.
    CLEAR : WA_MATNR_HD.   " WA_MATNR_HD.
    CONCATENATE IT_SPEC-WORDER IT_SPEC-EXTC IT_SPEC-INTC
                                       INTO WA_MATNR_CL.
*   AT NEW WORDER.
    WA_MATNR_HD = IT_SPEC-WORDER.
    CLEAR : IT_WOHD, IT_WOHD[].
    PERFORM FTP_HANDLING_MASTER TABLES IT_WOHD
                                USING WA_MATNR_HD.

*     CLEAR : IT_WOCL, IT_WOCL[].
*     PERFORM FTP_HANDLING_MASTER TABLES IT_WOCL
*                                 USING WA_MATNR_CL.
*   ENDAT.
    IF IT_SPEC-MARK = 'I'.
      IT_ZSPPVHB-FLG = 'IR'.
    ELSEIF IT_SPEC-MARK = 'R'.
      IT_ZSPPVHB-FLG = 'RP'.
    ELSEIF IT_SPEC-MARK = 'D'.
      IT_ZSPPVHB-FLG = 'DL'.
    ENDIF.
    PERFORM MOVE_TO_IT_ZSPPVHB.
   IF NOT IT_ZSPPVHB-FLG IS INITIAL AND NOT IT_ZSPPVHB-MATNR IS INITIAL.
      CASE IT_ZSPPVHB-FLG.
        WHEN 'IR'.
          WA_IR_VHB = WA_IR_VHB + 1.
        WHEN 'RP'.
          WA_RP_VHB = WA_RP_VHB + 1.
        WHEN 'DL'.
          WA_DL_VHB = WA_DL_VHB + 1.
      ENDCASE.
      APPEND IT_ZSPPVHB. CLEAR IT_ZSPPVHB.
    ENDIF.
  ENDLOOP.
  SORT IT_ZSPPVHB.
  IF IT_ZSPPVHB[] IS INITIAL.
    MESSAGE w001 WITH TEXT-005.
    wa_flag = 'X'.
  ENDIF.
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
*&-------------------------------------------------------------------*
*&      Form  MOVE_TO_IT_ZSPPVHB
*&-------------------------------------------------------------------*
FORM MOVE_TO_IT_ZSPPVHB.
  DATA: L_DATIM(14).

  MOVE : WA_MATNR_HD   TO IT_ZSPPVHB-MATNR.
* PERFORM READ_WOHD_WOCL USING 'WOCL' 'P_WO_CREATE_DATE1'
*                      CHANGING L_DATIM.
* IT_ZSPPVHB-P_WO_CREATE_DATE = L_DATIM(8).
* IT_ZSPPVHB-P_WO_CREATE_TIME = L_DATIM+8(6).
  DATA : L_HPCB_FNAME(40).
  DATA : L_HPCB_SEARCH(40).
  DATA : L_NO1_NB(1)    TYPE  N,
         L_NO2_NB(2)    TYPE  N,
         L_NO3_NB(3)    TYPE  N.
  DATA : L_NUM          TYPE  I,
         L_LINES        LIKE  SY-INDEX.
*&--------------------------------------------------------------------&*
*&  Change by shiva. - 12/09/2004
  data: w_wo_hpc_b like zsppvhb-p_wo_hpc_b001.
*&--------------------------------------------------------------------&*
  DESCRIBE TABLE IT_WOHD LINES  L_LINES.
  IF L_LINES NE 0.
    DO 20 TIMES.
      L_NO3_NB = SY-INDEX.
      CLEAR : L_HPCB_SEARCH, L_HPCB_FNAME.
*      CONCATENATE 'IT_ZSPPVHB-B'  L_NO3_NB INTO L_HPCB_FNAME.
      CONCATENATE 'IT_ZSPPVHB-P_WO_HPC_B'
                                  L_NO3_NB INTO L_HPCB_FNAME.
      ASSIGN (L_HPCB_FNAME)   TO <HPCB>.
      CONCATENATE 'P_WO_HPC_B' L_NO3_NB  INTO L_HPCB_SEARCH.
      clear w_wo_hpc_b.
      PERFORM READ_WOHD_WOCL USING 'WOHD' L_HPCB_SEARCH
                             CHANGING w_wo_hpc_b.
      <HPCB> = w_wo_hpc_b.
    ENDDO.
  ENDIF.
ENDFORM.                    " MOVE_TO_IT_ZSPPVHB

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
*    WHEN 'WOCL'.
*      READ TABLE IT_WOCL WITH KEY ATNAM = P_ATNAM.
*      IF SY-SUBRC EQ 0.
*        P_ATWRT = IT_WOCL-ATWRT.
*      ENDIF.
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
    IF NOT IT_ZSPPVHB[] IS INITIAL.
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

  PERFORM PROGRESS_INDICATOR USING 'TRANSFER FROM HPCC(B) TO ALC'.
*----> EAI TRANSFER
  DESCRIBE TABLE IT_ZSPPVHB LINES L_CNT.

  CALL FUNCTION 'Z_FPP_SET_ZTPPVHB'
    DESTINATION C_DEST
    EXPORTING
      IR             = WA_IR_VHB
      RP             = WA_RP_VHB
      DL             = WA_DL_VHB
    TABLES
      I_ZSPPVHB      = IT_ZSPPVHB
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
      SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT.

  IF SY-SUBRC <> 0.
    IT_MESSAGE_B-FUNCNAME = 'Z_FPP_SET_ZTPPVHB'.
    IT_MESSAGE_B-MSGTXT   = L_MSGTXT.
    APPEND IT_MESSAGE_B. CLEAR IT_MESSAGE_B.
  ELSE.
    LOOP AT IT_ZSPPVHB.
      MOVE-CORRESPONDING IT_ZSPPVHB TO IT_ZTPPVHB.
      MOVE SY-UCOMM    TO  IT_ZTPPVHB-ZUSER.
      MOVE SY-DATUM    TO  IT_ZTPPVHB-ZSDAT.
      MOVE SY-UZEIT    TO  IT_ZTPPVHB-ZSTIM.
      APPEND IT_ZTPPVHB.
    ENDLOOP.
    IF NOT IT_ZTPPVHB[] IS INITIAL.
      MODIFY ZTPPVHB FROM TABLE IT_ZTPPVHB.
      IF SY-SUBRC = 0.
        COMMIT WORK.
        IT_MESSAGE_B-FUNCNAME = 'ZTPPVHB'.
        CONCATENATE L_CNT TEXT-101  INTO IT_MESSAGE_B-MSGTXT
                                    SEPARATED BY SPACE       .
        APPEND IT_MESSAGE_B. CLEAR IT_MESSAGE_B.
      ELSE.
        ROLLBACK WORK.
        IT_MESSAGE_B-FUNCNAME = 'ZTPPVHB'.
        IT_MESSAGE_B-MSGTXT   =  TEXT-102.
        APPEND IT_MESSAGE_B. CLEAR IT_MESSAGE_B.
      ENDIF.
    ENDIF.
  ENDIF.
  IF SY-UCOMM = 'TRANS'.
    MOVE 'TRANS' TO IT_STATUS-FCODE.
    APPEND IT_STATUS. CLEAR IT_STATUS.
    SET PF-STATUS 'SUB_LIST' EXCLUDING IT_STATUS.
    LOOP AT IT_MESSAGE_B.
      WRITE:/ IT_MESSAGE_B.
    ENDLOOP.
  ELSE.
    EXPORT IT_MESSAGE_B  TO MEMORY ID 'MEMO'.
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

  LOOP AT IT_ZSPPVHB.
    ZEBRA = SY-TABIX MOD 2.
    IF ZEBRA = 0.
      FORMAT COLOR 2 INTENSIFIED OFF.
    ELSE.
      FORMAT COLOR 2 INTENSIFIED ON.
    ENDIF.

    WRITE AT:/1(4)   IT_ZSPPVHB-FLG,
              6(18)  IT_ZSPPVHB-MATNR,
              15(4)  IT_ZSPPVHB-P_WO_HPC_B001,
              20(4)  IT_ZSPPVHB-P_WO_HPC_B002,
              25(4)  IT_ZSPPVHB-P_WO_HPC_B003,
              30(4)  IT_ZSPPVHB-P_WO_HPC_B004,
              35(4)  IT_ZSPPVHB-P_WO_HPC_B005,
              40(4)  IT_ZSPPVHB-P_WO_HPC_B006,
              45(4)  IT_ZSPPVHB-P_WO_HPC_B007,
              50(4)  IT_ZSPPVHB-P_WO_HPC_B008,
              55(4)  IT_ZSPPVHB-P_WO_HPC_B009,
              60(4)  IT_ZSPPVHB-P_WO_HPC_B010,
              65(4)  IT_ZSPPVHB-P_WO_HPC_B011,
              70(4)  IT_ZSPPVHB-P_WO_HPC_B012,
              75(4)  IT_ZSPPVHB-P_WO_HPC_B013,
              80(4)  IT_ZSPPVHB-P_WO_HPC_B014,
              85(4)  IT_ZSPPVHB-P_WO_HPC_B015,
              90(4)  IT_ZSPPVHB-P_WO_HPC_B016,
              95(4)  IT_ZSPPVHB-P_WO_HPC_B017,
             100(4)  IT_ZSPPVHB-P_WO_HPC_B018,
             105(4)  IT_ZSPPVHB-P_WO_HPC_B019,
             110(4)  IT_ZSPPVHB-P_WO_HPC_B020,
             115(11) IT_ZSPPVHB-P_WO_CREATE_DATE,
             137(11) IT_ZSPPVHB-P_WO_CREATE_TIME.
  ENDLOOP.
  SET LEFT SCROLL-BOUNDARY COLUMN 24.
ENDFORM.                    " DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  HEAD_LINE
*&---------------------------------------------------------------------*
FORM HEAD_LINE.
  FORMAT COLOR 5 INTENSIFIED OFF.
  WRITE AT:/1(4) TEXT-006,
             6(18) TEXT-007,
             25(4)   TEXT-008,
             30(4)   TEXT-009,
             35(4)   TEXT-010,
             40(4)   TEXT-011,
             45(4)   TEXT-012,
             50(4)   TEXT-013,
             55(4)   TEXT-014,
             60(4)   TEXT-015,
             65(4)   TEXT-016,
             70(4)   TEXT-017,
             75(4)   TEXT-018,
             80(4)   TEXT-019,
             85(4)   TEXT-020,
             90(4)   TEXT-021,
             95(4)   TEXT-022,
            100(4)   TEXT-023,
            105(4)   TEXT-024,
            110(4)   TEXT-025,
            115(4)   TEXT-026,
            120(4)   TEXT-027,
            125(11)  TEXT-028,
            137(11)  TEXT-029.
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
