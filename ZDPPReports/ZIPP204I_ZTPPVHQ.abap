************************************************************************
* Program Name      : ZIPP203I_ZTPPVHQ
* Author            : DongYeop, Han
* Creation Date     : 2003.11.12.
* Specifications By : DongYeop, Han
* Pattern           : 1.1
* Development Request No : UD1K901977
* Addl Documentation:
* Description       : Transfer of Production Spec from PP to ALC
*                     (ztppvhq)
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  zipp204_ztppvhq   NO STANDARD PAGE HEADING
                          LINE-SIZE 550
                          MESSAGE-ID zdpp.

************************************************************************
*              D A T A     A R E A                                     *
************************************************************************
TABLES:ztpp_spec.

DATA : BEGIN OF it_spec OCCURS 0,
         worder   LIKE  ztpp_spec-worder,    "W/O HEADER
         extc     LIKE  ztpp_spec-extc,      "EXT COLOR
         intc     LIKE  ztpp_spec-intc,      "INT COLOR
         mark     LIKE  ztpp_spec-mark.      "IR/RP/DL
DATA : END OF it_spec.

DATA : it_specvs    LIKE TABLE OF ztppvs         WITH HEADER LINE,
       it_wohd      LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
       it_wocl      LIKE TABLE OF zspp_vin_value WITH HEADER LINE.
DATA : it_zsppvhq  LIKE TABLE OF zsppvhq         WITH HEADER LINE,
       it_ztppvhq  LIKE TABLE OF ztppvhq         WITH HEADER LINE.
DATA : BEGIN OF it_message_q OCCURS 0,
       funcname(20),
       msgtxt(100).
DATA : END OF it_message_q.

DATA: BEGIN OF it_status OCCURS 0,
        fcode LIKE rsmpe-func,
      END OF it_status.

* Field Symbol
FIELD-SYMBOLS : <hpcq>.      " for HPCC Color Part

*RANGES
RANGES r_mark FOR ztpp_spec-mark.

*WORK AREA
DATA : wa_flag,              " No data Check flag.
       wa_matnr_cl           LIKE  mara-matnr,
       wa_matnr_hd           LIKE  mara-matnr,
       wa_model              LIKE  conf_out-atwrt.

DATA : wa_ir_vhq              LIKE  sy-tabix,
       wa_rp_vhq              LIKE  sy-tabix,
       wa_dl_vhq              LIKE  sy-tabix.


*CONSTANTS
CONSTANTS: c_mode    VALUE  'A',
           c_dest(10) VALUE 'WMPP01'.   "Outbound Interface Destination

************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
POSITION 8.
PARAMETERS: c_trans AS CHECKBOX .
SELECT-OPTIONS: s_date FOR sy-datum NO-EXTENSION.
SELECTION-SCREEN: BEGIN OF LINE,
                  POSITION 8.
PARAMETERS : c_ir AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN: COMMENT 12(10)  text-001  FOR FIELD c_ir,
                  POSITION 28.
PARAMETERS : c_rp AS CHECKBOX.
SELECTION-SCREEN: COMMENT 34(10)  text-002  FOR FIELD c_rp,
                  POSITION 50.
PARAMETERS : c_dl AS CHECKBOX.
SELECTION-SCREEN: COMMENT 56(10)  text-003  FOR FIELD c_dl,
                  END OF LINE,
                  END OF BLOCK b1.

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
  PERFORM data_select.
  PERFORM data_join.
  PERFORM write_or_transfer.
  SET PF-STATUS 'SUB_LIST'.

END-OF-SELECTION.

************************************************************************
*              TOP-OF-PAGE                                             *
************************************************************************
TOP-OF-PAGE.


************************************************************************
*             AT USER-COMMAND                                          *
************************************************************************
AT USER-COMMAND.
  PERFORM user_command.

*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT
*&---------------------------------------------------------------------*
FORM data_select.
  clear: wa_flag.
  PERFORM progress_indicator USING 'CREATING SPEC DATA ....'.
  PERFORM ranges_mark.
*  SELECT WORDER EXTC INTC MARK
*     INTO TABLE IT_SPEC
*     FROM ZTPP_SPEC
*     WHERE MARK IN R_MARK.

  SORT it_spec.
  IF it_spec[] IS INITIAL.
    MESSAGE w001 WITH text-004.
  ENDIF.
ENDFORM.                    " DATA_SELECT
*&---------------------------------------------------------------------*
*&      Form  RANGES_MARK
*&---------------------------------------------------------------------*
FORM ranges_mark.
  DATA: l_data                LIKE sy-datum.

  CLEAR: l_data, it_spec, it_spec[].
  IF c_ir = 'X'.
    DELETE FROM ZTPPVHQ CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
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
       WHERE zsdat IN s_date.
    r_mark-sign   = 'I'.
    LOOP AT it_specvs  WHERE flg = 'IR' .
      it_spec-worder  = it_specvs-matnr(14).
      it_spec-extc    = it_specvs-matnr+14(02).
      it_spec-intc    = it_specvs-matnr+16(02).
      it_spec-mark    = 'I'                   .
      APPEND it_spec.
    ENDLOOP.
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
FORM data_join.
  LOOP AT it_spec.
    CLEAR : wa_matnr_cl, wa_matnr_hd.
    CONCATENATE it_spec-worder it_spec-extc it_spec-intc
                                         INTO wa_matnr_cl.
    AT NEW worder.
      wa_matnr_hd = it_spec-worder.
      CLEAR : it_wohd, it_wohd[].
      PERFORM ftp_handling_master TABLES it_wohd
                                  USING wa_matnr_hd.

      CLEAR : it_wocl, it_wocl[].
      PERFORM ftp_handling_master TABLES it_wocl
                                  USING wa_matnr_cl.
    ENDAT.
    IF it_spec-mark = 'I'.
      it_zsppvhq-flg = 'IR'.
    ELSEIF it_spec-mark = 'R'.
      it_zsppvhq-flg = 'RP'.
    ELSEIF it_spec-mark = 'D'.
      it_zsppvhq-flg = 'DL'.
    ENDIF.
    PERFORM move_to_it_zsppvhq.
   IF NOT it_zsppvhq-flg IS INITIAL AND NOT it_zsppvhq-matnr IS INITIAL.
      CASE it_zsppvhq-flg.
        WHEN 'IR'.
          wa_ir_vhq = wa_ir_vhq + 1.
        WHEN 'RP'.
          wa_rp_vhq = wa_rp_vhq + 1.
        WHEN 'DL'.
          wa_dl_vhq = wa_dl_vhq + 1.
      ENDCASE.
      APPEND it_zsppvhq. CLEAR it_zsppvhq.
    ENDIF.
  ENDLOOP.
  SORT it_zsppvhq.
  IF it_zsppvhq[] IS INITIAL.
    MESSAGE w001 WITH text-005.
    wa_flag = 'X'.
  ENDIF.

ENDFORM.                    " DATA_JOIN
*&---------------------------------------------------------------------*
*&      Form  FTP_HANDLING_MASTER
*&---------------------------------------------------------------------*
FORM ftp_handling_master TABLES   itab
                         USING    p_matnr.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object       = p_matnr
            mode         = 'R'
            ctype        = '001'
            display      = 'X'
       TABLES
            val_table    = itab
       EXCEPTIONS
            no_data      = 1
            error_mode   = 2
            error_object = 3
            OTHERS       = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MOVE_TO_IT_ZSPPVHQ
*&---------------------------------------------------------------------*
FORM move_to_it_zsppvhq.
  DATA : l_hpcq_fname(40).
  DATA : l_hpcq_search(40).
  DATA : l_no1_nb(1)    TYPE  n,
         l_no2_nb(2)    TYPE  n,
         l_no3_nb(3)    TYPE  n.
  DATA : l_num          TYPE  i,
         l_lines        LIKE  sy-index,
         l_date   LIKE  conf_out-atwrt.

  MOVE : wa_matnr_cl   TO it_zsppvhq-matnr.
  PERFORM read_wohd_wocl USING 'WOCL' 'P_WO_CREATE_DATE1'
                         CHANGING l_date.
  it_zsppvhq-p_wo_create_date = l_date(8).
  it_zsppvhq-p_wo_create_time = l_date+8(6).
  DESCRIBE TABLE it_wocl LINES  l_lines.
  IF l_lines NE 0.
    DO 100 TIMES.
      l_no3_nb = sy-index.
      CLEAR : l_hpcq_search, l_hpcq_fname.
      CONCATENATE 'IT_ZSPPVHQ-Q'  l_no3_nb INTO l_hpcq_fname.
      ASSIGN (l_hpcq_fname)   TO <hpcq>.
      CONCATENATE 'P_WO_HPC_Q' l_no3_nb  INTO l_hpcq_search.
      PERFORM read_wohd_wocl USING 'WOCL' l_hpcq_search
                             CHANGING <hpcq>.
*      READ TABLE IT_WOCL001 WITH KEY ATNAM = L_HPCQ_SEARCH.
*      IF SY-SUBRC EQ 0.
*        <HPCQ> = IT_WOCL001-ATWRT.
*      ENDIF.
    ENDDO.
  ENDIF.
*  ENDIF.

ENDFORM.                    " MOVE_TO_IT_ZSPPVHQ
*&---------------------------------------------------------------------*
*&      Form  READ_WOHD_WOCL
*&---------------------------------------------------------------------*
FORM read_wohd_wocl USING    p_ind p_atnam
                    CHANGING p_atwrt.

  CASE p_ind.
    WHEN 'WOHD'.
      READ TABLE it_wohd WITH KEY atnam = p_atnam.
      IF sy-subrc EQ 0.
        p_atwrt = it_wohd-atwrt.
      ENDIF.
    WHEN 'WOCL'.
      READ TABLE it_wocl WITH KEY atnam = p_atnam.
      IF sy-subrc EQ 0.
        p_atwrt = it_wocl-atwrt.
      ENDIF.
  ENDCASE.
ENDFORM.                    " READ_WOHD_WOCL
*&---------------------------------------------------------------------*
*&      Form  WRITE_OR_TRANSFER
*&---------------------------------------------------------------------*
FORM write_or_transfer.
  check wa_flag = space.
  IF c_trans = 'X'.
    PERFORM transfer_to_alc.
  ELSE.
    IF NOT it_zsppvhq[] IS INITIAL.
      PERFORM data_write.
    ENDIF.
  ENDIF.
ENDFORM.                    " WRITE_OR_TRANSFER
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_TO_ALC
*&---------------------------------------------------------------------*
FORM transfer_to_alc.
  DATA: l_cnt(6)       TYPE n,
        l_msgtxt(100)        .

  PERFORM progress_indicator USING 'TRANSFER FROM SPEC TO ALC'.
*----> EAI TRANSFER
  DESCRIBE TABLE it_zsppvhq LINES l_cnt.

  CALL FUNCTION 'Z_FPP_SET_ZTPPVHQ'
    DESTINATION c_dest
    EXPORTING
      ir             = wa_ir_vhq
      rp             = wa_rp_vhq
      dl             = wa_dl_vhq
    TABLES
      i_zsppvhq      = it_zsppvhq
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF sy-subrc <> 0.
    it_message_q-funcname = 'Z_FPP_SET_ZTPPVHQ'.
    it_message_q-msgtxt   = l_msgtxt.
    APPEND it_message_q. CLEAR it_message_q.
  ELSE.
    LOOP AT it_zsppvhq.
      MOVE-CORRESPONDING it_zsppvhq TO it_ztppvhq.
      MOVE sy-uname    TO  it_ztppvhq-zuser.
      MOVE sy-datum    TO  it_ztppvhq-zsdat.
      MOVE sy-uzeit    TO  it_ztppvhq-zstim.
      APPEND it_ztppvhq.
    ENDLOOP.
    IF NOT it_ztppvhq[] IS INITIAL.
      MODIFY ztppvhq FROM TABLE it_ztppvhq.
      IF sy-subrc = 0.
        COMMIT WORK.
        it_message_q-funcname = 'ZTPPVHQ'.
        CONCATENATE l_cnt text-110  INTO it_message_q-msgtxt
                                    SEPARATED BY space       .
        APPEND it_message_q. CLEAR it_message_q.
      ELSE.
        ROLLBACK WORK.
        it_message_q-funcname = 'ZTPPVHQ'.
        it_message_q-msgtxt   = text-111 .
        APPEND it_message_q. CLEAR it_message_q.
      ENDIF.
    ENDIF.
  ENDIF.
  IF sy-ucomm = 'TRANS'.
    MOVE 'TRANS' TO it_status-fcode.
    APPEND it_status. CLEAR it_status.
    SET PF-STATUS 'SUB_LIST' EXCLUDING it_status.
    LOOP AT it_message_q.
      WRITE:/ it_message_q.
    ENDLOOP.
  ELSE.
    EXPORT it_message_q  TO MEMORY ID 'MEMO'.
  ENDIF.
ENDFORM.                    " TRANSFER_TO_ALC
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
FORM progress_indicator USING   p_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            text = p_text.
ENDFORM.                    " PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  DATA_WRITE
*&---------------------------------------------------------------------*
FORM data_write.
  DATA:zebra.
  PERFORM head_line.
  LOOP AT it_zsppvhq.
    zebra = sy-tabix MOD 2.
    IF zebra = 0.
      FORMAT COLOR 2 INTENSIFIED OFF.
    ELSE.
      FORMAT COLOR 2 INTENSIFIED ON.
    ENDIF.
    WRITE AT:/1(4)   it_zsppvhq-flg,
              6(18)  it_zsppvhq-matnr,
              25(4)  it_zsppvhq-q001,
              30(4)  it_zsppvhq-q002,
              35(4)  it_zsppvhq-q003,
              40(4)  it_zsppvhq-q004,
              45(4)  it_zsppvhq-q005,
              50(4)  it_zsppvhq-q006,
              55(4)  it_zsppvhq-q007,
              60(4)  it_zsppvhq-q008,
              65(4)  it_zsppvhq-q009,
              70(4)  it_zsppvhq-q010,
              75(4)  it_zsppvhq-q011,
              80(4)  it_zsppvhq-q012,
              85(4)  it_zsppvhq-q013,
              90(4)  it_zsppvhq-q014,
              95(4)  it_zsppvhq-q015,
             100(4)  it_zsppvhq-q016,
             105(4)  it_zsppvhq-q017,
             110(4)  it_zsppvhq-q018,
             115(4)  it_zsppvhq-q019,
             120(4)  it_zsppvhq-q020,
             125(4)  it_zsppvhq-q021,
             130(4)  it_zsppvhq-q022,
             135(4)  it_zsppvhq-q023,
             140(4)  it_zsppvhq-q024,
             145(4)  it_zsppvhq-q025,
             150(4)  it_zsppvhq-q026,
             155(4)  it_zsppvhq-q027,
             160(4)  it_zsppvhq-q028,
             165(4)  it_zsppvhq-q029,
             170(4)  it_zsppvhq-q030,
             175(4)  it_zsppvhq-q031,
             180(4)  it_zsppvhq-q032,
             185(4)  it_zsppvhq-q033,
             190(4)  it_zsppvhq-q034,
             195(4)  it_zsppvhq-q035,
             200(4)  it_zsppvhq-q036,
             205(4)  it_zsppvhq-q037,
             210(4)  it_zsppvhq-q038,
             215(4)  it_zsppvhq-q039,
             220(4)  it_zsppvhq-q040,
             225(4)  it_zsppvhq-q041,
             230(4)  it_zsppvhq-q042,
             235(4)  it_zsppvhq-q043,
             240(4)  it_zsppvhq-q044,
             245(4)  it_zsppvhq-q045,
             250(4)  it_zsppvhq-q046,
             255(4)  it_zsppvhq-q047,
             260(4)  it_zsppvhq-q048,
             265(4)  it_zsppvhq-q049,
             270(4)  it_zsppvhq-q050,
             275(4)  it_zsppvhq-q051,
             280(4)  it_zsppvhq-q052,
             285(4)  it_zsppvhq-q053,
             290(4)  it_zsppvhq-q054,
             295(4)  it_zsppvhq-q055,
             300(4)  it_zsppvhq-q056,
             305(4)  it_zsppvhq-q057,
             310(4)  it_zsppvhq-q058,
             315(4)  it_zsppvhq-q059,
             320(4)  it_zsppvhq-q060,
             325(4)  it_zsppvhq-q061,
             330(4)  it_zsppvhq-q062,
             335(4)  it_zsppvhq-q063,
             340(4)  it_zsppvhq-q064,
             345(4)  it_zsppvhq-q065,
             350(4)  it_zsppvhq-q066,
             355(4)  it_zsppvhq-q067,
             360(4)  it_zsppvhq-q068,
             365(4)  it_zsppvhq-q069,
             370(4)  it_zsppvhq-q070,
             375(4)  it_zsppvhq-q071,
             380(4)  it_zsppvhq-q072,
             385(4)  it_zsppvhq-q073,
             390(4)  it_zsppvhq-q074,
             395(4)  it_zsppvhq-q075,
             400(4)  it_zsppvhq-q076,
             405(4)  it_zsppvhq-q077,
             410(4)  it_zsppvhq-q078,
             415(4)  it_zsppvhq-q079,
             420(4)  it_zsppvhq-q080,
             425(4)  it_zsppvhq-q081,
             430(4)  it_zsppvhq-q082,
             435(4)  it_zsppvhq-q083,
             440(4)  it_zsppvhq-q084,
             445(4)  it_zsppvhq-q085,
             450(4)  it_zsppvhq-q086,
             455(4)  it_zsppvhq-q087,
             460(4)  it_zsppvhq-q088,
             465(4)  it_zsppvhq-q089,
             470(4)  it_zsppvhq-q090,
             475(4)  it_zsppvhq-q091,
             480(4)  it_zsppvhq-q092,
             485(4)  it_zsppvhq-q093,
             490(4)  it_zsppvhq-q094,
             495(4)  it_zsppvhq-q095,
             500(4)  it_zsppvhq-q096,
             505(4)  it_zsppvhq-q097,
             510(4)  it_zsppvhq-q098,
             515(4)  it_zsppvhq-q099,
             520(4)  it_zsppvhq-q100,
             525(11) it_zsppvhq-p_wo_create_date,
             537(11) it_zsppvhq-p_wo_create_date.

  ENDLOOP.
  SET LEFT SCROLL-BOUNDARY COLUMN 24.
ENDFORM.                    " DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  HEAD_LINE
*&---------------------------------------------------------------------*
FORM head_line.
  FORMAT COLOR 5 INTENSIFIED OFF.
  WRITE AT:/1(4) text-006,
              6(18) text-007,
              25(4)   text-008,
              30(4)   text-009,
              35(4)   text-010,
              40(4)   text-011,
              45(4)   text-012,
              50(4)   text-013,
              55(4)   text-014,
              60(4)   text-015,
              65(4)   text-016,
              70(4)   text-017,
              75(4)   text-018,
              80(4)   text-019,
              85(4)   text-020,
              90(4)   text-021,
              95(4)   text-022,
             100(4)   text-023,
             105(4)   text-024,
             110(4)   text-025,
             115(4)   text-026,
             120(4)   text-027,
             125(4)   text-028,
             130(4)   text-029,
             135(4)   text-030,
             140(4)   text-031,
             145(4)   text-032,
             150(4)   text-033,
             155(4)   text-034,
             160(4)   text-035,
             165(4)   text-036,
             170(4)   text-037,
             175(4)   text-038,
             180(4)   text-039,
             185(4)   text-040,
             190(4)   text-041,
             195(4)   text-042,
             200(4)   text-043,
             205(4)   text-044,
             210(4)   text-045,
             215(4)   text-046,
             220(4)   text-047,
             225(4)   text-048,
             230(4)   text-049,
             235(4)   text-050,
             240(4)   text-051,
             245(4)   text-052,
             250(4)   text-053,
             255(4)   text-054,
             260(4)   text-055,
             265(4)   text-056,
             270(4)   text-057,
             275(4)   text-058,
             280(4)   text-059,
             285(4)   text-060,
             290(4)   text-061,
             295(4)   text-062,
             300(4)   text-063,
             305(4)   text-064,
             310(4)   text-065,
             315(4)   text-066,
             320(4)   text-067,
             325(4)   text-068,
             330(4)   text-069,
             335(4)   text-070,
             340(4)   text-071,
             345(4)   text-072,
             350(4)   text-073,
             355(4)   text-074,
             360(4)   text-075,
             365(4)   text-076,
             370(4)   text-077,
             375(4)   text-078,
             380(4)   text-079,
             385(4)   text-080,
             390(4)   text-081,
             395(4)   text-082,
             400(4)   text-083,
             405(4)   text-084,
             410(4)   text-085,
             415(4)   text-086,
             420(4)   text-087,
             425(4)   text-088,
             430(4)   text-089,
             435(4)   text-090,
             440(4)   text-091,
             445(4)   text-092,
             450(4)   text-093,
             455(4)   text-094,
             460(4)   text-095,
             465(4)   text-096,
             470(4)   text-097,
             475(4)   text-098,
             480(4)   text-099,
             485(4)   text-100,
             490(4)   text-101,
             495(4)   text-102,
             500(4)   text-103,
             505(4)   text-104,
             510(4)   text-105,
             515(4)   text-106,
             520(4)   text-107,
             525(11)  text-108,
             537(11)  text-109.



ENDFORM.                    " HEAD_LINE
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command.
  CASE sy-ucomm.
    WHEN 'TRANS'.
      PERFORM transfer_to_alc.

  ENDCASE.
ENDFORM.                    " USER_COMMAND
